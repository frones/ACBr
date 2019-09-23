{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 04/04/2018: Fabio Junior Borba
|*  - Primeira Versao ACBrBALPrecision
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALPrecision;

interface
uses ACBrBALClass, Classes;

type
  TACBrBALPrecision = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    procedure SolicitarPeso; override;

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;

  end ;

implementation
Uses ACBrUtil, ACBrConsts, SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, synaser, Windows{$ENDIF},
     Math, StrUtils;

{ TACBrBALPrecision }

constructor TACBrBALPrecision.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Precision' ;
end;

function TACBrBALPrecision.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  Resposta : AnsiString ;
  Decimais : Integer ;
  wPosIni, wPosFim: Integer;
begin
  Decimais := 1000 ;

  wPosIni := Pos(STX, aResposta);
  wPosFim := PosEx(CR, aResposta, wPosIni + 1);

  if (wPosFim <= 0) then
    wPosFim := Length(aResposta) + 1;  // Não achou? ...Usa a String inteira

  // Contem a String inteira
  wResposta := Copy(aResposta, wPosIni, wPosFim - wPosIni);

  // Contem somente o Peso
  if (wResposta[1] in ['H', 'T', 'M', 'L', 'I']) then
    Resposta := '-' + Trim(Copy(wResposta, wPosIni + 2, 6))
  else
    Resposta := Trim(Copy(wResposta, wPosIni + 2, 6));

  if Length(Resposta) > 0 then
  begin
    { Ajustando o separador de Decimal corretamente }
    {Resposta := StringReplace(Resposta, '.', DecimalSeparator, [rfReplaceAll]);
    Resposta := StringReplace(Resposta, ',', DecimalSeparator, [rfReplaceAll]);}

    try
      if pos(DecimalSeparator, Resposta) > 0 then  { Já existe ponto decimal ? }
        Result := StrToFloat(Resposta)
      else
        Result := StrToInt(Resposta) / Decimais ;

      case AnsiIndexText(Copy(wResposta, 1, 1), ['G','F','A','@','H', 'T','M','L','I']) of
        0 : Result := 0 ;  { Aguardando Peso }
        1 : Result := 0 ;  { Aguardando Peso }
        2 : Result := -1  ;  { Instavel }
        3 : Result := -1  ;  { Instavel }
        { Peso Negativo }
        4 : Result := -2;
        5 : Result := -2;
        6 : Result := -2;
        7 : Result := -2;
        8 : Result := -2;
      end;
      if (wResposta[1] in ['H', 'T', 'M', 'L', 'I']) then
        wResposta := '-' + wResposta;
    except
      Result := 0 ;
    end;
  end
  else
    Result := 0;
end;

procedure TACBrBALPrecision.SolicitarPeso;
begin
    { Protocolo Precision
      A balança Precision manda leituras continuamente para a COM,
      não necessita de um comando para ler o peso.
      Configuração Padrao de Leitura:
      Baud Rate = 1200, Data Bits = 8, Parity = None, Stop Bits = 1,
      Handshaking = None
      ----------------------}

//  inherited;

end;

end.
