{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Andre Adami                                     }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBALWeightechWT3000IR_ABS_Fm1Fm2Fm9;

interface

uses
  Classes;

type

  TStatusPesagem = (spNaoDetectado, spEstavel, spInstavel, spSobrecarga, spSobrecargaNegativa);
  TInformacaoPeso = (ipNaoDetectado, ipPesoBruto, ipPesoLiquido, ipTara);
  TInformacaoSinalPeso = (sipNaoDetectado, sipPositivo, sipNegativo);

  (*
    Classe com lógica para interpretar formatos de mensagens Fm1, Fm2 e Fm9
    da balança Weightech WT3000IR_ABS.
  *)
  TFormatoFm1Fm2Fm9Util = class
  private
    class function TratarResposta(const aResposta: AnsiString): AnsiString;

    class function ExtrairSinalPeso(const aResposta: AnsiString): TInformacaoSinalPeso;
    class function ExtrairInformacaoPeso(const aResposta: AnsiString): TInformacaoPeso;
    class function ExtrairStatusPesagem(const aResposta: AnsiString): TStatusPesagem;

    class function DetectarFormatoTransmissao(const aResposta: AnsiString): Boolean;
  public
    class function InterpretarResposta(const aResposta: AnsiString; out aPesoLido: Double): Boolean;
  end;

implementation

Uses
  SysUtils, Math,
  ACBrConsts, ACBrUtil.Strings,
  {$IFDEF COMPILER6_UP}
   DateUtils, StrUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};

const
  (*
    Ref.: https://www.weightech.com.br/indicador-de-pesagem-wt3000-ir-abs
    Constantes para os dados (manual de integração pág.: 21)

    Formato de mensagem:
    S T , G S , + SP SP 3 0 0 . 0 0 SP SP k g CR LF
  *)

  INFORMACAO_PESO_BRUTO = 'GS';
  INFORMACAO_PESO_LIQUIDO = 'NT';
  INFORMACAO_TARA = 'TR';

  STATUS_PESAGEM_ESTAVEL = 'ST';
  STATUS_PESAGEM_INSTAVEL = 'US';
  STATUS_PESAGEM_EM_OVER_LOAD = 'OL';

  INDEX_INICIO_INFORMACAO_PESO = 4;
  TAMANHO_INFORMACAO_PESO = 2;

  INDEX_INICIO_STATUS_PESAGEM = 1;
  TAMANHO_STATUS_PESAGEM = 2;

  INDEX_INICIO_SINAL_PESO = 7;
  TAMANHO_SINAL_PESO = 1;

  INDEX_INICIO_PESO = 8;
  TAMANHO_PESO = 8;

  INDEX_INICIO_PACOTE_DADOS_PESAGEM = 1;
  TAMANHO_PACOTE_DADOS_PESAGEM = 19;

{ ACBrBALWeightechWT3000IR_ABS_Fm1Fm2Fm9 }

class function TFormatoFm1Fm2Fm9Util.DetectarFormatoTransmissao(const aResposta: AnsiString): Boolean;
begin
  Result := (Length(aResposta) = TAMANHO_PACOTE_DADOS_PESAGEM) and
  (ExtrairStatusPesagem(aResposta) <> spNaoDetectado) and
  (ExtrairInformacaoPeso(aResposta) <> ipNaoDetectado) and
  (ExtrairSinalPeso(aResposta) <> sipNaoDetectado);
end;

class function TFormatoFm1Fm2Fm9Util.ExtrairInformacaoPeso(
  const aResposta: AnsiString): TInformacaoPeso;
var
  vInformacaoPeso: AnsiString;
begin
  Result := ipNaoDetectado;

  vInformacaoPeso := Copy(aResposta, INDEX_INICIO_INFORMACAO_PESO, TAMANHO_INFORMACAO_PESO);
  case AnsiIndexText(vInformacaoPeso, [INFORMACAO_PESO_BRUTO, INFORMACAO_PESO_LIQUIDO, INFORMACAO_TARA]) of
    0: Result := ipPesoBruto;
    1: Result := ipPesoLiquido;
    2: Result := ipTara;
  end;
end;

class function TFormatoFm1Fm2Fm9Util.ExtrairSinalPeso(const aResposta: AnsiString): TInformacaoSinalPeso;
begin
  Result := sipNaoDetectado;

  case AnsiIndexText(Copy(aResposta, INDEX_INICIO_SINAL_PESO, TAMANHO_SINAL_PESO), ['+', '-']) of
    0: Result := sipPositivo;
    1: Result := sipNegativo;
  end;
end;

class function TFormatoFm1Fm2Fm9Util.ExtrairStatusPesagem(const aResposta: AnsiString): TStatusPesagem;
var
  vStatusPesagem: AnsiString;
begin
  Result := spNaoDetectado;

  vStatusPesagem := Copy(aResposta, INDEX_INICIO_STATUS_PESAGEM, TAMANHO_STATUS_PESAGEM);
  case AnsiIndexText(vStatusPesagem, [STATUS_PESAGEM_ESTAVEL, STATUS_PESAGEM_INSTAVEL, STATUS_PESAGEM_EM_OVER_LOAD]) of
    0: Result := spEstavel;
    1: Result := spInstavel;
    2:
    begin
      Result := spSobrecarga;

      if ExtrairSinalPeso(aResposta) = sipNegativo then
        Result := spSobrecargaNegativa;
    end;
  end;
end;

class function TFormatoFm1Fm2Fm9Util.InterpretarResposta(const aResposta: AnsiString; out aPesoLido: Double): Boolean;
var
  vTempResposta: AnsiString;
begin
  Result := False;
  aPesoLido := -9; // Por padrão, timeout

  vTempResposta := TratarResposta(aResposta);

  if not DetectarFormatoTransmissao(vTempResposta) then
    Exit;

  case ExtrairStatusPesagem(vTempResposta) of
    spEstavel:
    begin
      { Ajustando o separador de Decimal corretamente }
      vTempResposta := trim(StringReplace(vTempResposta, '.', DecimalSeparator, [rfReplaceAll]));
      vTempResposta := trim(StringReplace(vTempResposta, ',', DecimalSeparator, [rfReplaceAll]));

      aPesoLido := StrToFloatDef(copy(vTempResposta, INDEX_INICIO_PESO, TAMANHO_PESO), 0); // { ST - Estável }
    end;
    spInstavel: aPesoLido := -1; { US - Instável }
    spSobrecarga: aPesoLido := -10; { OL - Sobrecarga }
    spSobrecargaNegativa: aPesoLido := -2; { OL - Sobrecarga negativa/Peso negativo }
  end;

  Result := True;
end;

class function TFormatoFm1Fm2Fm9Util.TratarResposta(const aResposta: AnsiString): AnsiString;
var
  vPacotesDadosPesagens: TSplitResult; // ACBrUtil.Strings
  vLengthPacotesDadosPesagens: Integer;
begin
  Result := Trim(aResposta);
  vLengthPacotesDadosPesagens := 0;

  if Length(aResposta) < TAMANHO_PACOTE_DADOS_PESAGEM then
    Exit;

  {
    Se resposta em modo de transmissão contínuo:
    - Separar mensagens conforme o delimitador LF (Line Feed).
    - Pegar a quantidade de mensagens e ler a última do grupo.

    Exemplo:
    ST,GS,+    3000  kgCRLF
    ST,GS,+    3000  kgCRLF
    ST,GS,+    3000  kgCRLF
    ST,GS,+    3000  kgCRLF
  }
  if Pos(LF, Result) > 0 then // ACBrConsts
  begin
    vPacotesDadosPesagens := Split(LF, Result); // ACBrUtil.Strings
    vLengthPacotesDadosPesagens := Length(vPacotesDadosPesagens);

    // Pegar última resposta do grupo
    Result := vPacotesDadosPesagens[vLengthPacotesDadosPesagens - 1];
  end;

  Result := Copy(Result, INDEX_INICIO_PACOTE_DADOS_PESAGEM, TAMANHO_PACOTE_DADOS_PESAGEM);
end;

end.
