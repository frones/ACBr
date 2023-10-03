{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Pandaa                      }
{                              Daniel Moraes - Infocotidiano                   }
{                                                                              }
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

unit ACBrBancoC6;

interface

uses
  Classes, Contnrs, SysUtils, ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrBancoC6 }

  TACBrBancoC6 = class(TACBrBancoClass)
  private
    function ConverterMultaPercentual(const ACBrTitulo: TACBrTitulo): Double;
    function ConverterTipoPagamento(const ATipoPagamento: TTipo_Pagamento): String;

  protected
    function ConverterDigitoModuloFinal(): String; override;
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;

  public
    Constructor create(AOwner: TACBrBanco);
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string; override;

    procedure GerarRegistroHeader400(NumeroRemessa: Integer;  ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    function MontaInstrucoesCNAB400(const ACBrTitulo :TACBrTitulo; const nRegistro: Integer ): String; override;

    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;


    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    procedure LerRetorno400(ARetorno: TStringList); override;
    procedure LerRetorno240(ARetorno: TStringList); override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;

  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime ;

{ TACBrBancoC6 }
function TACBrBancoC6.ConverterDigitoModuloFinal(): String;
begin
  if Modulo.ModuloFinal = 1 then
      Result:= 'P'
   else
      Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoC6.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;

function TACBrBancoC6.DefineCampoLivreCodigoBarras(
  const ACBrTitulo: TACBrTitulo): String;
var LEmissao : char;
    LBoleto : TACBrBoleto;
begin
  LBoleto := ACBrTitulo.ACBrBoleto;

  case LBoleto.Cedente.ResponEmissao of
    tbBancoEmite,
    tbBancoPreEmite,
    tbBancoNaoReemite,
    tbBancoReemite      : LEmissao := '3'; //3 para cobrança registrada com emissão pelo banco
    else
      LEmissao := '4'; //4 para cobrança direta com emissão pelo cedente
  end;

  Result := PadLeft(OnlyNumber(LBoleto.Cedente.CodigoCedente), 12, '0') +
            ACBrTitulo.NossoNumero +
            ACBrTitulo.Carteira +
            LEmissao;
end;

function TACBrBancoC6.ConverterMultaPercentual(
  const ACBrTitulo: TACBrTitulo): Double;
begin
  if ACBrTitulo.MultaValorFixo then
      if (ACBrTitulo.ValorDocumento > 0) then
        Result := (ACBrTitulo.PercentualMulta / ACBrTitulo.ValorDocumento) * 100
      else
        Result := 0
    else
      Result := ACBrTitulo.PercentualMulta;
end;

function TACBrBancoC6.ConverterTipoPagamento(
  const ATipoPagamento: TTipo_Pagamento): String;
begin
    //1 - Aceita Qualquer valor
    //2 - Aceita entre um mínimo e máximo
    //3 - Não aceita pagamento com valor divergente
    //4 - Aceita a partir de um mínimo
  case ATipoPagamento of
    tpAceita_Qualquer_Valor:
      Result := '1';
    tpAceita_Valores_entre_Minimo_Maximo:
      Result := '2';
    tpSomente_Valor_Minimo:
      Result := '4';
    else
      Result := '3';
  end;

end;

constructor TACBrBancoC6.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                     := 0;
   fpNome                       := 'C6BANK';
   fpNumero                     := 336;
   fpTamanhoMaximoNossoNum      := 10;
   fpTamanhoAgencia             := 4;
   fpTamanhoConta               := 7;
   fpTamanhoCarteira            := 2;
   fpLayoutVersaoArquivo        := 22;
   fpLayoutVersaoLote           := 0;
   fpDensidadeGravacao          := '';
   fpModuloMultiplicadorInicial := 2;
   fpModuloMultiplicadorFinal   := 7;
   fpCodParametroMovimento      := '';
end;

function TACBrBancoC6.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:= '0' + ACBrTitulo.Carteira + ACBrTitulo.NossoNumero + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoC6.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia
            + '/'
            + ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

procedure TACBrBancoC6.GerarRegistroHeader400(NumeroRemessa: Integer;
  ARemessa: TStringList);
var
  LLinha, LConta : String;
  LBeneficiario: TACBrCedente;
begin
  LBeneficiario := ACBrBanco.ACBrBoleto.Cedente;
  LConta := PadLeft(LBeneficiario.Conta, 12, '0') ;

  If LBeneficiario.Modalidade = '2' then // Modalidade 1 = Conta Cobrança Direta Modalidade 2 = Cobrança Registrada
     LConta :=  PadLeft('0', 12, '0') ;

  LLinha:= '0'                                                +  // 001 a 001 - Tipo de Registro
           '1'                                                +  // 002 a 002 - Código Remessa
           'REMESSA'                                          +  // 003 a 009 - Literal Remessa
           '01'                                               +  // 010 a 011 - Código Serviço
           PadRight('COBRANCA', 8)                            +  // 012 a 019 - Literal Cobranca
           Space(7)                                           +  // 020 a 026 - Uso do Banco
           PadLeft(LBeneficiario.CodigoCedente, 12, '0')      +  // 027 a 038 - Código do Cedente
           Space(8)                                           +  // 039 a 046 - Uso do Banco
           PadRight(Nome, 30)                                 +  // 047 a 076 - Nome da Empresa
           IntToStrZero(fpNumero, 3)                          +  // 077 a 079 - Código do Banco
           Space(15)                                          +  // 080 a 094 - Uso do Banco
           FormatDateTime('ddmmyy',Now)                       +  // 095 a 100 - Data de Gravação
           Space(8)                                           +  // 101 a 108 - Uso do Banco
           LConta                                             +  // 109 a 120 - Conta Cobrança Direta
           Space(266)                                         +  // 121 a 386 - Uso do Banco
           IntToStrZero(NumeroRemessa, 8)                     +  // 387 a 394 - Sequecial de Remessa
           IntToStrZero(1, 6);                                   // 395 a 400 - Sequencial

    ARemessa.Add(UpperCase(LLinha));

end;

procedure TACBrBancoC6.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  LPercMulta: Double;
  LOcorrencia, LEspecie,
  LMensagemTodos, LMensagemCedente, LProtesto, LTipoSacado, LConta,
  LLinha,LDigitoNossoNumero : String;
  LBeneficiario: TACBrCedente;
  LTitulo     : TACBrTitulo;
  LPagador     : TACBrSacado;
  Index			  : Integer;
begin
  LTitulo      := ACBrTitulo;
  LBeneficiario := LTitulo.ACBrBoleto.Cedente;
  LPagador      := LTitulo.Sacado;

  LDigitoNossoNumero := CalcularDigitoVerificador(LTitulo);

  {Código da Ocorrencia}
  LOcorrencia:= TipoOcorrenciaToCodRemessa(LTitulo.OcorrenciaOriginal.Tipo);

  {Especie}
  LEspecie:= DefineEspecieDoc(LTitulo);

  {Intruções}
  LProtesto:= InstrucoesProtesto(LTitulo);

  {Tipo de Sacado}
  LTipoSacado := DefineTipoSacado(LTitulo);

  { Converte valor em moeda para percentual, pois o arquivo só permite % }
  LPercMulta := ConverterMultaPercentual(LTitulo);

  {Código da Empresa}
  LConta   := PadRight(LBeneficiario.Conta, 12) ;

  if LBeneficiario.Modalidade = '2' then
    LConta   :=   PadLeft(LBeneficiario.CodigoCedente, 12, '0') ;

  LMensagemCedente:= '';
  LMensagemTodos  := '';

  if LTitulo.Mensagem.Count > 0 then
  begin
    for Index := 0 to LTitulo.Mensagem.Count - 1 do
    begin
      if LMensagemTodos<> '' then
        LMensagemTodos:= LMensagemTodos + ' ';
      LMensagemTodos:= LMensagemTodos + Trim(LTitulo.Mensagem[Index]);
    end;

    // Primeira parte, 30 caracteres
    LMensagemCedente := Copy(LMensagemTodos, 1, 30);
    Delete(LMensagemTodos, 1, 30);

    // voltar com 4 blocos de 80 caracteres, vai como id 2, se nao tem mais nada, coloca vazio
    LTitulo.Mensagem.Text := LMensagemTodos;


  LLinha:= '1'                                                                                                           +  // 001 a 001 - ID Registro
           '0' + DefineTipoInscricao                                                                                     +  // 002 a 003 - Tipo Inscrição Empresa
           PadLeft(OnlyNumber(LBeneficiario.CNPJCPF), 14, '0')                                                           +  // 004 a 017 - CNPJ Empresa
           LConta                                                                                                        +  // 018 a 029 - Código da Empresa
           Space(8)                                                                                                      +  // 030 a 037 - Uso do Banco
           PadRight(LTitulo.NumeroDocumento, 25)                                                                         +  // 038 a 062 - Uso da Empresa
           '0' + LTitulo.NossoNumero + LDigitoNossoNumero                                                                +  // 063 a 074 - Nosso Número Completo
           Space(8)                                                                                                      +  // 075 a 082 - Uso do Banco
           IntToStrZero(fpNumero, 3)                                                                                     +  // 083 a 085 - Código do Banco
           Space(21)                                                                                                     +  // 086 a 106 - Uso do Banco
           PadLeft(LTitulo.Carteira, 2, '0')                                                                             +  // 107 a 108 - Código da Carteira
           LOcorrencia                                                                                                   +  // 109 a 110 - Código Ocorrência Remessa
           PadRight(LTitulo.SeuNumero, 10)                                                                               +  // 111 a 120 - Seu Número
           FormatDateTime('ddmmyy', LTitulo.Vencimento)                                                                  +  // 121 a 126 - Data Vencimento
           IntToStrZero(Round(LTitulo.ValorDocumento * 100 ), 13)                                                        +  // 127 a 139 - Valo Titulo
           Space(8)                                                                                                      +  // 140 a 147 - Uso do Banco
           LEspecie                                                                                                      +  // 148 a 149 - Espécie do Título
           'N'                                                                                                           +  // 150 a 150 - Aceite
           FormatDateTime('ddmmyy', LTitulo.DataDocumento)                                                               +  // 151 a 156 - Data Emissão Título
           LProtesto                                                                                                     +  // 157 a 160 - Intrução 1 e 2
           IntToStrZero(Round(LTitulo.ValorMoraJuros * 100 ), 13)                                                        +  // 161 a 173 - Juros ao Dia
           IfThen(LTitulo.DataDesconto < EncodeDate(2000,01,01),
                  '000000',
                  FormatDateTime('ddmmyy', LTitulo.DataDesconto))                                                        +  // 174 a 179 - Data Desconto
           IntToStrZero(Round(LTitulo.ValorDesconto * 100 ), 13)                                                         +  // 180 a 192 - Valor Desconto
           IfThen(LTitulo.DataMulta < EncodeDate(2000,01,01),
                  '000000',
                  FormatDateTime('ddmmyy', LTitulo.DataMulta))                                                           +  // 193 a 198 - Data Multa
           Space(7)                                                                                                      +  // 199 a 205 - Uso do Banco
           IntToStrZero(Round(LTitulo.ValorAbatimento * 100 ), 13)                                                       +  // 206 a 218 - Valor Abatimento
           LTipoSacado                                                                                                   +  // 219 a 220 - Tipo Sacado
           PadLeft(OnlyNumber(LPagador.CNPJCPF), 14, '0')                                                                +  // 221 a 234 - CNPJ/CPF Sacado
           PadRight(LPagador.NomeSacado, 40)                                                                             +  // 235 a 274 - Nome do Sacado
           PadRight(LPagador.Logradouro +
                    ' ' +
                    LPagador.Numero +
                    ' ' +
                    LPagador.Complemento, 40)                                                                            +  // 275 a 314 - Endereço Sacado
           PadRight(LPagador.Bairro, 12)                                                                                 +  // 315 a 326 - Bairro Sacado
           PadRight(LPagador.CEP, 8)                                                                                     +  // 327 a 334 - CEP Sacado
           PadRight(LPagador.Cidade, 15)                                                                                 +  // 335 a 349 - Cidade Sacado
           PadRight(LPagador.UF, 2)                                                                                      +  // 350 a 351 - UF Sacado
           PadRight(LMensagemCedente, 30)                                                                                +  // 352 a 381 - Sacador / Mensagem / Código CMC7
           IfThen(LTitulo.PercentualMulta > 0,
                  '2',
                  '0')                                                                                                   +  // 382 a 382 - Tipo de Multa
           IntToStrZero(Round(LPercMulta), 2)                                                                            +  // 383 a 384 - Percentual de Multa
           Space(1)                                                                                                      +  // 385 a 385 - Uso do Banco
           IfThen(LTitulo.DataMoraJuros < EncodeDate(2000,01,01),
                  '000000',
                  FormatDateTime('ddmmyy', LTitulo.DataMoraJuros))                                                       +  // 386 a 391 - Data Juros Mora
           PadLeft(IntToStr(LTitulo.DiasDeProtesto), 2, '0')                                                             +  // 392 a 393 - Prazo dias para Cartório
           Space(1)                                                                                                      +  // 394 a 394 - Uso do Banco
           IntToStrZero(aRemessa.Count + 1, 6)                                                                           + // 395 a 400 - Seqüencial

           PadLeft(DefineTipoInscricao, 2, '0')                                                                          + // 401 a 402 - Tipo Emitente
           PadLeft(OnlyNumber(LBeneficiario.CNPJCPF), 14, '0')                                                           + // 403 a 416 - CNPJ Emitente
           PadRight(LBeneficiario.Nome, 40)                                                                              + // 417 a 456 - Nome Emitente
           PadRight(LBeneficiario.Logradouro +
                    ' ' +
                    LBeneficiario.NumeroRes +
                    ' ' +
                    LBeneficiario.Complemento, 40)                                                                       + // 457 a 496 - Endereço Emitente
                    PadRight(LBeneficiario.Cidade, 15)                                                                   + // 497 a 511 - Cidade Emitente
           PadRight(LBeneficiario.UF, 2)                                                                                 + // 512 a 513 - UF Emitente
           PadRight(OnlyNumber(LBeneficiario.CEP), 8)                                                                    + // 514 a 521 - CEP Emitente
           PadRight(LPagador.Email, 120)                                                                                 + // 522 a 641 - Email
           IfThen(LTitulo.DataDesconto2 < EncodeDate(2000, 01, 01),
                  '      ',
                  FormatDateTime('ddmmyy',LTitulo.DataDesconto2))                                                        + // 642 a 647 - Data Desconto2
           PadRight(IfThen(LTitulo.ValorDesconto2 > 0 ,
           IntToStrZero(Round(LTitulo.ValorDesconto2 * 100), 13), ' '),13)                                               + // 648 a 660 - Valor Desconto2
           IfThen(LTitulo.DataDesconto3 < EncodeDate(2000, 01, 01),
                  '      ',
                  FormatDateTime('ddmmyy', LTitulo.DataDesconto3))                                                       + // 661 a 666 - Data Desconto3
           PadRight(IfThen(LTitulo.ValorDesconto3 > 0 ,
                               IntToStrZero(Round(LTitulo.ValorDesconto3 * 100), 13),
                               ' '),13)                                                                                  + // 667 a 679 - Valor Desconto3
           ConverterTipoPagamento(LTitulo.TipoPagamento)                                                                 + // 680 a 680 - Indicativo de Autorização para Recebimento de Valor Divergente
           IfThen((LTitulo.ValorMinPagamento = 0)
               or (LTitulo.ValorMaxPagamento = 0),
                     ' ',
                     IfThen(LTitulo.PercentualMinPagamento > 0,
                        'P',
                        'V')
                  )                                                                                                     + // 681 a 681 - Indicativo de valor ou percentual para o range mínimo e máximo de aceitação do pagamento
           IfThen(LTitulo.ValorMinPagamento = 0,
                  PadLeft(' ',13),
                  IfThen(LTitulo.ValorMinPagamento > 0,
                         IntToStrZero(Round(LTitulo.ValorMinPagamento * 100), 13),
                                            IntToStrZero(Round(LTitulo.PercentualMinPagamento * 100), 13)
                           )
                  )                                                                                                    + // 682 a 694 - Valor ou Percentual Mínimo para aceitação do pagamento
           IfThen(LTitulo.ValorMaxPagamento = 0,
                  PadLeft(' ',13),
                  IfThen(LTitulo.ValorMaxPagamento > 0,
                         IntToStrZero(Round(LTitulo.ValorMaxPagamento * 100), 13),
                         IntToStrZero(Round(LTitulo.PercentualMaxPagamento * 100), 13)
                         )
                  )                                                                                                    + // 695 a 707 - Valor ou Percentual Maximo para aceitação do pagamento
           Space(1)                                                                                                    + // 708 a 708 - Uso do Banco Brancos
           ifThen(LTitulo.QtdePagamentoParcial=0,
           PadLeft(' ',2),
           IntToStrZero(LTitulo.QtdePagamentoParcial, 2)
           );                                                                                                            // 709 a 710 - Quantidade de pagamentos parciais

    aRemessa.Add(UpperCase(LLinha));
    LLinha := MontaInstrucoesCNAB400(ACBrTitulo, aRemessa.Count);

    if not(LLinha = EmptyStr) then
      aRemessa.Add(UpperCase(LLinha));
  end;

end;

function TACBrBancoC6.MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo;
  const nRegistro: Integer): String;
var
  I: Integer;
  MensagemTodos: string;
begin
   Result := '';

  {parte da instrução vai no registro 1, 30 primeiros caracteres}
   MensagemTodos := '';
   for I := 0 to ACBrTitulo.Mensagem.Count - 1 do
   begin
     if MensagemTodos<> '' then
        MensagemTodos:= MensagemTodos + ' ';
     MensagemTodos:= MensagemTodos + ACBrTitulo.Mensagem[I];
   end;

   if MensagemTodos = '' then
     Exit;

   Result := '2';                                                             // 001 a 001 Tipo Registro '2'

   for I := 1 to 4 do
   begin                                                                      // Montagem das mensagens 320 posições, 30 primeiras posições vai no campo 54
                                                                              // do registro Tipo '1'
                                                                              // 002 a 081 Mensagem 1
                                                                              // 082 a 161 Mensagem 2
                                                                              // 162 a 241 Mensagem 3
                                                                              // 242 a 321 Mensagem 4
     Result := Result + PadRight(Copy(MensagemTodos, 1, 80), 80);
     if MensagemTodos <> '' then
       Delete(MensagemTodos, 1, 80);
   end;

   Result := Result +
               Space(44)                                                   +  // 322 a 365 - Uso do Banco
               PadRight(ACBrTitulo.SeuNumero, 10)                          +  // 366 a 375 - Seu Número
               FormatDateTime('ddmmyy', ACBrTitulo.Vencimento)             +  // 376 a 381 - Data Vencimento
               IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100 ), 13)   +  // 382 a 394 - Valo Titulo
               IntToStrZero(nRegistro + 1, 6);                                // 395 a 400 - Seqüencial

end;

function TACBrBancoC6.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);


    case CodOcorrencia of
      04: Result := '04-Alteração de Dados (Entrada)';
      05: Result := '05-Alteração de Dados (Baixa)';
      07: Result := '07-Liquidação após Baixa';
      08: Result := '08-Liquidação em Cartório';
      10: Result := '10-Baixa comandada do cliente arquivo';
      15: Result := '15-Baixa rejeitada';
      16: Result := '16-Instrução rejeitada';
      21: Result := '21-Confirma instrução de não protestar';
      32: Result := '32-Baixa por ter sido protestado';
      36: Result := '36-Custas de Edital';
      37: Result := '37-Custas de sustação judicial';
      38: Result := '38-Título sustado judicialmente';
      65: Result := '65-Pagamento com Cheque - Aguardando compensação';
      69: Result := '69-Cancelamento de Liquidação por Cheque Devolvido';
      71: Result := '71-Protesto cancelado pelo Cartório';
      75: Result := '75-Pagamento Parcial';
      90: Result := '90-Instrução de Protesto Rejeitada';
      95: Result := '95-Troca Uso Empresa';
      96: Result := '96-Emissão Extrato Mov. Carteira';
      97: Result := '97-Tarifa de sustação de protesto';
      98: Result := '98-Tarifa de protesto';
      99: Result := '99-Custas de protesto';
    end;

  if (Result <> '') then
    Exit;

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação Normal';
    09: Result := '09-Baixa Simples';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    17: Result := '17-Alterações de dados rejeitados';
    19: Result := '19-Confirma instrução de protesto';
    20: Result := '20-Confirma instruão de sustação de protesto';
    23: Result := '23-Protesto enviado a cartório';
    35: Result := '35-Alegações do sacado';
  end;
end;

function TACBrBancoC6.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

    case CodOcorrencia of
      04: Result := toRetornoAlteracaoDadosNovaEntrada;
      05: Result := toRetornoAlteracaoDadosBaixa;
      07: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      08: Result := toRetornoLiquidadoEmCartorio;
      10: Result := toRetornoBaixadoViaArquivo; //toRetornoBaixadoInstAgencia;
      15: Result := toRetornoBaixaRejeitada; //toRetornoLiquidadoEmCartorio;
      16: Result := toRetornoInstrucaoRejeitada; //toRetornoTituloPagoEmCheque;
      21: Result := toRetornoRecebimentoInstrucaoNaoProtestar; //toRetornoAcertoControleParticipante;
      32: Result := toRetornoBaixaPorProtesto; //toRetornoComandoRecusado;
      36: Result := toRetornoCustasEdital;
      37: Result := toRetornoCustasSustacaoJudicial;
      38: Result := toRetornoTituloSustadoJudicialmente;
      65: Result := toRetornoChequePendenteCompensacao;
      69: Result := toRetornoChequeDevolvido; //toRetornoCancelamentoDadosRateio;
      71: Result := toRetornoDevolvidoPeloCartorio;
      75: Result := toRetornoLiquidadoParcialmente;
      90: Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
      95: Result := toRetornoAlteracaoUsoCedente;
      96: Result := toRetornoTarifaExtratoPosicao;
      97: Result := toRetornoDespesasSustacaoProtesto;
      98: Result := toRetornoDespesasProtesto;
      99: Result := toRetornoCustasProtesto;
    end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixaSimples;//toRetornoBaixadoViaArquivo;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    17: Result := toRetornoAlteracaoDadosRejeitados;//toRetornoLiquidadoAposBaixaouNaoRegistro;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    23: Result := toRetornoEncaminhadoACartorio;
    35: Result := toRetornoAlegacaoDoSacado;//toRetornoDesagendamentoDebitoAutomatico;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoC6.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';


    case TipoOcorrencia of
      toRetornoAlteracaoDadosNovaEntrada                    : Result := '04';
      toRetornoAlteracaoDadosBaixa                          : Result := '05';
      toRetornoLiquidadoAposBaixaOuNaoRegistro              : Result := '07';
      toRetornoLiquidadoEmCartorio                          : Result := '08';
      toRetornoBaixadoViaArquivo                            : Result := '10';
      toRetornoBaixaRejeitada                               : Result := '15';
      toRetornoInstrucaoRejeitada                           : Result := '16';
      toRetornoRecebimentoInstrucaoNaoProtestar             : Result := '21';
      toRetornoBaixaPorProtesto                             : Result := '32';
      toRetornoCustasEdital                                 : Result := '36';
      toRetornoCustasSustacaoJudicial                       : Result := '37';
      toRetornoTituloSustadoJudicialmente                   : Result := '38';
      toRetornoChequePendenteCompensacao                    : Result := '65';
      toRetornoChequeDevolvido                              : Result := '69';
      toRetornoDevolvidoPeloCartorio                        : Result := '71';
      toRetornoLiquidadoParcialmente                        : Result := '75';
      toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente  : Result := '90';
      toRetornoAlteracaoUsoCedente                          : Result := '95';
      toRetornoTarifaExtratoPosicao                         : Result := '96';
      toRetornoDespesasSustacaoProtesto                     : Result := '97';
      toRetornoDespesasProtesto                             : Result := '98';
      toRetornoCustasProtesto                               : Result := '99';
    end;

  if (Result <> '') then
    Exit;

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                             : Result := '02';
    toRetornoRegistroRecusado                               : Result := '03';
    toRetornoLiquidado                                      : Result := '06';
    toRetornoBaixaSimples                                   : Result := '09';
    toRetornoAbatimentoConcedido                            : Result := '12';
    toRetornoAbatimentoCancelado                            : Result := '13';
    toRetornoVencimentoAlterado                             : Result := '14';
    toRetornoAlteracaoDadosRejeitados                       : Result := '17';
    toRetornoRecebimentoInstrucaoProtestar                  : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto             : Result := '20';
    toRetornoEncaminhadoACartorio                           : Result := '23';
    toRetornoAlegacaoDoSacado                               : Result := '35';
  else
    Result := '02';
  end;
end;

function TACBrBancoC6.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
   case TipoOcorrencia of
      toRetornoRegistroRecusado: //03
      case CodMotivo  of
         9000: Result := '9000-Data vencimento menor que o prazo de aceitação do título';
         9001: Result := '9001-Sacado bloqueado por atraso';
         9002: Result := '9002-Registro opcional inválido';
         9003: Result := '9003-Cep sem praça de cobrança';
         9004: Result := '9004-Prazo insuficiente para cobrança do título';
         9005: Result := '9005-Campo numérico inválido';
         9006: Result := '9006-Campo texto inválido';
         9007: Result := '9007-Campo tipo data inválido';
         9008: Result := '9008-Caractere inválido';
         9009: Result := '9009-Cpf/Cnpj do sacado e emitente devem ser diferentes';
         9010: Result := '9010-Data vencimento menor que a data de emissão';
         9011: Result := '9011-Data emissão maior que a data atual';
         9012: Result := '9012-Uf sacado inválido';
         9013: Result := '9013-Uf emitente inválido';
         9014: Result := '9014-Campo obrigatório não preenchido';
         9015: Result := '9015-Cpf do sacado inválido';
         9016: Result := '9016-Cnpj do sacado inválido';
         9017: Result := '9017-O nome do sacado enviado não confere com o nome do sacado cadastrado no sistema para este Cnpj/Cpf';
         9018: Result := '9018-Tipo do sacado inválido';
         9019: Result := '9019-O sacado está bloqueado';
         9020: Result := '9020-O endereço do sacado esta com o tamanho esta maior que o permitido';
         9021: Result := '9021-Digito do nosso numero inválido';
         9022: Result := '9022-Não existe faixa cadastrada para o banco e a conta';
         9023: Result := '9023-O nosso numero esta fora da faixa cadastrada para o cedente';
         9081: Result := '9081-Prazo insuficiente para cobrança do título neste Cep';
         9084: Result := '9084-Seu número do registro opcional diferente da linha do registro do título';
         9085: Result := '9085-Data de vencimento do registro opcional diferente da linha do registro do título';
         9086: Result := '9086-Valor do título no vencimento do registro opcional diferente da linha do registro do título';
         9087: Result := '9087-Os títulos de carteira de cobrança direta só podem ser enviados para contas de cobrnaça direta. acao: confira a carteira e a conta cobrança que está sendo enviada/atribuida ao título';
         9089: Result := '9089-Código cmc7 invalido';
         9090: Result := '9090-Entrada - nosso número já está sendo utilizado para mesmo banco/conta';
         9091: Result := '9091-Cep do sacado não pertence ao estado da federação (Uf) informado';
         9092: Result := '9092-Tipo de multa inválido';
         9093: Result := '9093-Registro opcional de emitente inválido';
         9097: Result := '9097-O campo Nosso Número não foi informado ou não foi possivel identificar o titulo';
         9098: Result := '9098-Foi encontrado mais de um título para esse nosso número';
         9099: Result := '9099-Preencha o campo de "conta de cobrança" no cadastro de carteira por cedente';
         9100: Result := '9100-Título possui registro opcional de emitente e a sua configuração não permite envio de títulos de terceiros';
         9101: Result := '9101-Título possui emitente, porém seus dados não foram informados';
         9103: Result := '9103-Ja existe titulo em aberto cadastrado para este cedente/seu numero/data vencimento/valor e emitente';
         9104: Result := '9104-Impedido pela legislação vigente';
         9106: Result := '9106-Nosso numero nao informado';
         9232: Result := '9232-Sacado pertence a empresa do grupo (coligada)';
         9233: Result := '9233-Por solicitação da diretoria de crédito/comercial';
         9234: Result := '9234-Inexistência de relação com o cedente';
         9235: Result := '9235-Outros';
         9236: Result := '9236-Recusado - Outros Motivos';
         9240: Result := '9240-Data multa menor que data de vencimento do título';
         9250: Result := '9250-Tipo de autorização para recebimento de valor divergente inválido';
         9251: Result := '9251-Indicativo Tipo de valor ou percentual inválido';
         9252: Result := '9252-Quantidade de pagamento parcial inválido';
         9254: Result := '9254-Mínimo não aceito para o título';
         9255: Result := '9255-Máximo não aceito para o título';
         9052: Result := '9052-Data de desconto 2 inválida';
         9230: Result := '9230-Valor desconto 2 inválido';
         9258: Result := '9258-Data de desconto 3 inválida';
         9259: Result := '9259-Valor desconto 3 inválido';
         9260: Result := '9260-Mínimo é obrigatório quando informado o tipo valor ou percentual';
         9261: Result := '9261-Tipo de autorização de recebimento de valor divergente não permitio para tipo de título 31';
         9262: Result := '9262-Para especie de título diferente de fatura de cartão de crédito não é possível informar o tipo aceita qualquer valor com range mínimo e máximo  preenchido';
         9263: Result := '9263-Mínimo e Máximo tem que ser informado para o tipo de autorização de valor divergente igual a 2';
         9264: Result := '9264-Mínimo e Máximo não devem ser informados para o tipo de autorização de valor divergente igual a 3';
         9265: Result := '9265-Mínimo deve ser informado e Máximo não pode ser informado para o tipo de autorização de valor divergente igual a 4';
         9266: Result := '9266-Valor não permitido para tipo de título fatura de cartão de crédito';
         9267: Result := '9267-Não é permitido ter juros, multa, abatimento, desconto ou protesto tipo de título fatura de cartão de crédito';
         9999: Result := '9999-Cep do sacado inválido';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoLiquidado: //06
      case CodMotivo of
         9105: Result := '9105-Crédito retido';
         9210: Result := '9210-Liquidação em cheque';
         9216: Result := '9216-Liquidação no guichê de caixa em dinheiro';
         9217: Result := '9217-Liquidação em banco correspondente';
         9218: Result := '9218-Liquidação por compensação eletrônica';
         9219: Result := '9219-Liquidação por conta';
         9223: Result := '9223-Liquidação por STR';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoLiquidadoEmCartorio: //08
      case CodMotivo of
         9201: Result:= '9201-Liquidação em cartório';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoBaixaSimples: //09
      case CodMotivo of
         9202: Result:= '9202-Baixa decurso prazo - banco';
         9237: Result:= '9237-Baixa por outros motivos';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoBaixadoViaArquivo: //10
      case CodMotivo of
         0000: Result:= '0000-Baixa comandada cliente arquivo';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoAlteracaoDadosRejeitados: //17
      case CodMotivo of
         9104: Result:= '9104-Impedido pela legislação vigente';
         9113: Result:= '9113-Não permitimos troca de carteira no evento de Alteração de Outros Dados';
         9114: Result:= '9114-Não permitimos troca de tipo titulo no evento de alteração de outros dados';
         9250: Result:= '9250-Tipo de autorização para recebimento de valor divergente inválido';
         9251: Result:= '9251-Indicativo Tipo de valor ou percentual inválido';
         9252: Result:= '9252-Quantidade de pagamento parcial inválido';
         9253: Result:= '9253-Quantidade de pagamento parcial inválido, somente é permitido um valor maior ou igual a quantidade de pagamentos já recebido';
         9254: Result:= '9254-Mínimo não aceito para o título';
         9255: Result:= '9255-Máximo não aceito para o título';
         9052: Result:= '9052-Data de desconto 2 inválida';
         9230: Result:= '9230-Valor desconto 2 inválido';
         9258: Result:= '9258-Data de desconto 3 inválida';
         9259: Result:= '9259-Valor desconto 3 inválido';
         9260: Result:= '9260-Mínimo é obrigatório quando informado o tipo valor ou percentual';
         9261: Result:= '9261-Tipo de autorização de recebimento de valor divergente não permitio para tipo de título 31';
         9262: Result:= '9262-Para especie de título diferente de fatura de cartão de crédito não é possível informar o tipo aceita qualquer valor com range mínimo e máximo  preenchido';
         9263: Result:= '9263-Mínimo e Máximo tem que ser informado para o tipo de autorização de valor divergente igual a 2';
         9264: Result:= '9264-Mínimo e Máximo não devem ser informados para o tipo de autorização de valor divergente igual a 3';
         9265: Result:= '9265-Mínimo deve ser informado e Máximo não pode ser informado para o tipo de autorização de valor divergente igual a 4';
         9266: Result:= '9266-Valor não permitido para tipo de título fatura de cartão de crédito';
         9267: Result:= '9267-Não é permitido ter juros, multa, abatimento, desconto ou protesto tipo de título fatura de cartão de crédito';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoBaixaPorProtesto: //32
      case CodMotivo of
         9203: Result:= '9203-Baixa protestado';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoAlegacaoDoSacado: //35
      case CodMotivo of
         9238: Result:= '9238-Pagador Rejeita Boleto';
         9239: Result:= '9239-Pagador Aceita Boleto';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoCustasEdital: //36
      case CodMotivo of
         9207: Result:= '9207-Custas de edital';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoCustasSustacaoJudicial: //37
      case CodMotivo of
         9208 : Result:= '9208-Custas de sustação de protesto';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoTituloSustadoJudicialmente: //38
      case CodMotivo of
         0000 : Result:= '0000-Título sustado judicialmente';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoLiquidadoParcialmente: //75
      case CodMotivo of
         9105: Result:= '9105-Crédito retido';
         9216: Result:= '9216-Liquidação no guichê de caixa em dinheiro';
         9217: Result:= '9217-Liquidação em banco correspondente';
         9218: Result:= '9218-Liquidação por compensação eletrônica';
         9219: Result:= '9219-Liquidação por conta';
         9223: Result:= '9223-Liquidação por STR';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente: //90
      case CodMotivo of
         9108: Result:= '9108-Título pertence a uma espécie que não pode ser protestada';
         9109: Result:= '9109-Protesto não permitido para título com moeda diferente de real';
         9110: Result:= '9110-Cep do sacado não atendido pelos cartórios cadastrados';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoTarifaExtratoPosicao: //96
      case CodMotivo of
         9213: Result:= '9213-Tarifa de manutenção de título vencido';
         9222: Result:= '9222-Emissão extrato mov. carteira';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoDespesasSustacaoProtesto: //97
      case CodMotivo of
         9204: Result:= '9204-Tarifa de sustacao de protesto';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoDespesasProtesto: //98
      case CodMotivo of
         9205: Result:= '9205-Tarifa de protesto';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
      toRetornoCustasProtesto: //99
      case CodMotivo of
         9206: Result:= '9206-Custas de protesto';
      else
         Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
   else
      Result:= '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
   end;

   if (Result = '0000') then begin
      case CodMotivo of
         9024: Result:= '9024-Identificação do título inválida';
         9025: Result:= '9025-Ocorrência não permitida pois o título esta baixado';
         9026: Result:= '9026-Ocorrência não permitida pois o título esta liquidado';
         9027: Result:= '9027-Ocorrência não permitida pois o título esta em protesto';
         9028: Result:= '9028-Não é permitida alteração de vencimento para carteira de desconto';
         9029: Result:= '9029-Situação do título inválida';
         9030: Result:= '9030-Não foi possível conceder o abatimento';
         9031: Result:= '9031-Não existe abatimento a ser cancelado';
         9032: Result:= '9032-Não foi possível prorrogar a data de vencimento do título';
         9033: Result:= '9033-Evento não permitido para situação do título';
         9034: Result:= '9034-Evento não permitido para cheques';
         9035: Result:= '9035-O código do registro esta diferente de 1';
         9036: Result:= '9036-Agência inválida';
         9037: Result:= '9037-Número da Conta Corrente para depósito Inválido';
         9038: Result:= '9038-O Cnpj do cedente passado para o arquivo não confere com o Cnpj do cedente cadastrado para o arquivo';
         9040: Result:= '9040-Cnpj do cedente não encontrado no cadastro';
         9041: Result:= '9041-Tipo do emitente inválido';
         9042: Result:= '9042-Cnpj do emitente inválido';
         9045: Result:= '9045-Campo nosso numero deve ter um valor de, no máximo , 10 digitos quando a carteira de cobrança não é direta';
         9046: Result:= '9046-No campo nosso número a identificação do título esta inválida';
         9047: Result:= '9047-Banco e conta de cobrança direta não informados';
         9049: Result:= '9049-Campo aceite enviado com valor nulo ou inválido';
         9050: Result:= '9050-Data de emisão inválida';
         9051: Result:= '9051-Data de vencimento inválida';
         9052: Result:= '9052-Data de desconto inválida';
         9053: Result:= '9053-Especie de titulo invalida';
         9054: Result:= '9054-Especie de titulo não encontrada';
         9055: Result:= '9055-Valor de título inválido';
         9056: Result:= '9056-Prazo de cartorio invalido';
         9057: Result:= '9057-Valor de abatimento inválido';
         9058: Result:= '9058-Valor de desconto inválido';
         9059: Result:= '9059-Código de ocorrência inválida ou inexistente';
         9060: Result:= '9060-Tipo de mora inválido';
         9062: Result:= '9062-Valor de juros ao dia inválido';
         9063: Result:= '9063-A data de juros mora é anterior à data de vencimento. Favor verificar estes campos';
         9064: Result:= '9064-A data de juros mora inválida';
         9065: Result:= '9065-Número da sequência diferente do esperado';
         9066: Result:= '9066-Número de sequencia inválido';
         9067: Result:= '9067-Registro inválido';
         9068: Result:= '9068-Cpf do emitente inválido';
         9070: Result:= '9070-Nome do emitente inválido';
         9071: Result:= '9071-Endereço do emitente inválido';
         9072: Result:= '9072-Cidade do emitente inválida';
         9073: Result:= '9073-Cep do emitente inválido';
         9074: Result:= '9074-Este contrato não está cadastrado para o cedente';
         9075: Result:= '9075-Não é permitida a entrada de títulos vencidos';
         9078: Result:= '9078-Não existe endereço, uf e cidade para o título';
         9079: Result:= '9079-Nosso número inválido';
         9083: Result:= '9083-O cedente não pode enviar esse tipo de título com esta carteira';
         9107: Result:= '9107-Tamanho máximo do nosso número para cobrança direta é 10 posições + digito(layout padrao matera/bradesco)';
         9224: Result:= '9224-Carteira do Tipo G não pode inserir titulos';
      else
         Result:= IntToStrZero(CodMotivo, 4) +' - Outros Motivos';
      end;
   end;
end;

function TACBrBancoC6.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Prorrogação}
    07 : Result:= toRemessaAlterarUsoEmpresa;               {Troca Uso da Empresa}
    09 : Result:= toRemessaProtestar;                       {Protestar}
    10 : Result:= toRemessaNaoProtestar;                    {Não Protestar}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar Protesto e Baixar Título}
    19 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar o Protesto e Manter em Carteira}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
    34 : Result:= toRemessaBaixaporPagtoDiretoCedente;      {Baixa por ter sido pago Diretamente ao Cedente}
    90 : Result:= toRemessaOutrasAlteracoes;                {Troca de emitente}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoC6.TipoOcorrenciaToCodRemessa(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin

    case TipoOcorrencia of
        toRemessaBaixar                         : Result := '02'; {Pedido de Baixa}
        toRemessaConcederAbatimento             : Result := '04'; {Concessão de Abatimento}
        toRemessaCancelarAbatimento             : Result := '05'; {Cancelamento de Abatimento}
        toRemessaAlterarVencimento              : Result := '06'; {Prorrogação}
        toRemessaAlterarUsoEmpresa              : Result := '07'; {Troca Uso da Empresa}
        toRemessaProtestar                      : Result := '09'; {Protestar}
        toRemessaNaoProtestar                   : Result := '10'; {Não Protestar}
        toRemessaCancelarInstrucaoProtestoBaixa : Result := '18'; {Sustar Protesto e Baixar o Tíulo}
        toRemessaCancelarInstrucaoProtesto      : Result := '19'; {Sustar o Protesto e Manter em Carteira}
        toRemessaOutrasOcorrencias              : Result := '31'; {Alteração de Outros Dados}
        toRemessaBaixaporPagtoDiretoCedente     : Result := '34'; {Baixa por ter sido pago Diretamente ao Cedente}
        toRemessaOutrasAlteracoes               : Result := '90'; {Troca de emitente}
      else
        Result := '01';                                           {Remessa}
    end;
end;

procedure TACBrBancoC6.LerRetorno240(ARetorno: TStringList);
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;

procedure TACBrBancoC6.LerRetorno400(ARetorno: TStringList);
var
  Index: Integer;
  LLinha: string;
  LTitulo: TACBrTitulo;
  LBoleto: TACBrBoleto;
  LColunaMotivoRejeicao : integer;
  LQtdeMotivosRejeicao : integer;
begin
  LBoleto := ACBrBanco.ACBrBoleto;

  if (StrToIntDef(copy(ARetorno.Strings[0], 77, 3), -1) <> Numero) then
    raise Exception.create(ACBrStr(LBoleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));

  LBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2)
    + '/'
    + Copy(ARetorno[0], 97, 2)
    + '/'
    + Copy(ARetorno[0], 99, 2), 0, 'DD/MM/YY');

  if LBoleto.LeCedenteRetorno then
  begin
    LBoleto.Cedente.Nome          := Trim(Copy(ARetorno[0], 47, 30));
    LBoleto.Cedente.CNPJCPF       := Trim(Copy(ARetorno[1], 4, 14));
    LBoleto.Cedente.CodigoCedente := Trim(Copy(ARetorno[1], 18, 20));
  end;

  if LBoleto.Cedente.CodigoCedente <> Copy(ARetorno[1], 18, 20) then
    raise Exception.create(ACBrStr(format('O Código de cedente do arquivo %s não é o mesmo do componente %s.',[Copy(ARetorno[1], 18, 20),LBoleto.Cedente.CodigoCedente])));

  case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
    01: LBoleto.Cedente.TipoInscricao := pFisica;
  else
    LBoleto.Cedente.TipoInscricao := pJuridica;
  end;

  LBoleto.ListadeBoletos.Clear;

  for Index := 1 to ARetorno.Count - 2 do
  begin
    LLinha := ARetorno[Index];

    if (Copy(LLinha, 1, 1) <> '1') then
      Continue;

    LTitulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;
    LTitulo.SeuNumero       := copy(LLinha, 38, 25);
    LTitulo.NossoNumero     := Copy(LLinha, 63, TamanhoMaximoNossoNum);
    LTitulo.Carteira        := copy(LLinha, 107, 2);
    LTitulo.NumeroDocumento := copy(LLinha, 117, 10);

    LTitulo.OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(LLinha, 109, 2), 0));
    if LTitulo.OcorrenciaOriginal.Tipo in [toRetornoRegistroRecusado,
      toRetornoBaixaRejeitada, toRetornoInstrucaoRejeitada] then
    begin
      LColunaMotivoRejeicao := 378; // posição da primeira rejeicao
      for LQtdeMotivosRejeicao := 1 to 3 do
      begin
        if Copy(LLinha, LColunaMotivoRejeicao, 2)<>'00' then
        begin
          LTitulo.MotivoRejeicaoComando.Add(Copy(LLinha, LColunaMotivoRejeicao, 2));
          LTitulo.DescricaoMotivoRejeicaoComando.add(CodMotivoRejeicaoToDescricao(LTitulo.OcorrenciaOriginal.Tipo,copy(LLinha, LColunaMotivoRejeicao, 2)));
        end;
        LColunaMotivoRejeicao := LColunaMotivoRejeicao + 2; // incrementa 2 posicoes para próxima rejeicao
      end;
    end;

    LTitulo.DataOcorrencia := StringToDateTimeDef(Copy(LLinha, 111, 2)
       + '/'
       + Copy(LLinha, 113, 2)
       + '/'
       + Copy(LLinha, 115, 2), 0, 'DD/MM/YY');

    case StrToIntDef(Copy(LLinha, 174, 2), 0) of
      01: LTitulo.EspecieDoc := 'DM';
      02: LTitulo.EspecieDoc := 'NP';
      03: LTitulo.EspecieDoc := 'CH';
      04: LTitulo.EspecieDoc := 'LC';
      05: LTitulo.EspecieDoc := 'RC';
      08: LTitulo.EspecieDoc := 'AS';
      12: LTitulo.EspecieDoc := 'DS';
      31: LTitulo.EspecieDoc := 'CC';
      99: LTitulo.EspecieDoc := 'OUT';
    end;

    case StrToInt(copy(LLinha, 108, 1)) of
      1: LTitulo.CaracTitulo := tcSimples;
      2: LTitulo.CaracTitulo := tcVinculada;
      3: LTitulo.CaracTitulo := tcCaucionada;
      4: LTitulo.CaracTitulo := tcDescontada;
    end;

    if (StrToIntDef(Copy(LLinha, 147, 6), 0) <> 0) then
      LTitulo.Vencimento := StringToDateTimeDef(Copy(LLinha, 147, 2)
        + '/'
        + Copy(LLinha, 149, 2)
        + '/'
        + Copy(LLinha, 151, 2), 0, 'DD/MM/YY');

    LTitulo.ValorDocumento       := StrToFloatDef(Copy(LLinha, 153, 13), 0) / 100;
    LTitulo.ValorDespesaCobranca := StrToFloatDef(Copy(LLinha, 176, 13), 0) / 100;
    LTitulo.ValorAbatimento      := StrToFloatDef(Copy(LLinha, 228, 13), 0) / 100;
    LTitulo.ValorDesconto        := StrToFloatDef(Copy(LLinha, 241, 13), 0) / 100;
    LTitulo.ValorRecebido        := StrToFloatDef(Copy(LLinha, 254, 13), 0) / 100;
    LTitulo.ValorMoraJuros       := StrToFloatDef(Copy(LLinha, 267, 13), 0) / 100;

    if (StrToIntDef(Copy(LLinha, 386, 6), 0) <> 0) then
      LTitulo.DataCredito := StringToDateTimeDef(Copy(LLinha, 386, 2)
        + '/'
        + Copy(LLinha, 388, 2)
        + '/'
        + Copy(LLinha, 390, 2), 0, 'DD/MM/YY');
  end;
end;

end.


