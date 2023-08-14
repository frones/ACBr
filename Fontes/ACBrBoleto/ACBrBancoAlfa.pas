{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: José M S Junior                                 }
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

unit ACBrBancoAlfa;

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBoletoConversao;

const
  CInstrucaoPagamento = 'Pagável em qualquer Banco, mesmo após o vencimento.';

type

  { TACBrBancoAlfa }

  TACBrBancoAlfa = class(TACBrBancoClass)
  private

  protected
    function GetLocalPagamento: String; override;
    function ConverterDigitoModuloFinal(): String; override;
    function DefineNumeroDocumentoModulo(const ACBrTitulo: TACBrTitulo): String; override;
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String; override;
    function MontaInstrucoesCNAB400(const ACBrTitulo :TACBrTitulo; const nRegistro: Integer ): String; override;
    procedure ValidaNossoNumeroResponsavel(out ANossoNumero: String; out ADigVerificador: String;
              const ACBrTitulo: TACBrTitulo); override;
    function DefineTamanhoAgenciaRemessa: Integer; override;

  public
    Constructor create(AOwner: TACBrBanco);

    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    Procedure LerRetorno400(ARetorno:TStringList); override;

    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;

  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil.Base, ACBrUtil.Strings;

{ TACBrBancoAlfa }
constructor TACBrBancoAlfa.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                 := 2;
   fpNome                   := 'BANCO ALFA S.A.';
   fpNumero                 := 025;
   fpTamanhoMaximoNossoNum  := 11;
   fpTamanhoAgencia         := 5;
   fpTamanhoConta           := 7;
   fpTamanhoCarteira        := 2;
   fpCodParametroMovimento  := '32';
   fpModuloMultiplicadorInicial:= 0;
   fpModuloMultiplicadorFinal:= 7;

end;

function TACBrBancoAlfa.DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if trim(EspecieDoc) = 'DM' then
      Result:= '01'
    else if trim(EspecieDoc) = 'NP' then
      Result:= '02'
    else if trim(EspecieDoc) = 'RC' then
      Result:= '03'
    else if trim(EspecieDoc) = 'NS' then
      Result:= '04'
    else if trim(EspecieDoc) = 'CH' then
      Result:= '05'
    else
      Result:= '99';
  end;

end;

function TACBrBancoAlfa.MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo;
  const nRegistro: Integer): String;
var
  lNossoNumero, lDigNossoNumero: String;
begin
  Result := '';
  ValidaNossoNumeroResponsavel(lNossoNumero, lDigNossoNumero, ACBrTitulo);

  with ACBrTitulo, ACBrBoleto do
  begin

     {Primeira instrução vai no registro 1}
     if Mensagem.Count <= 1 then
     begin
        Result := '';
        Exit;
     end;
     Result := '2' +                                                      // IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO
                 Copy(PadRight(TiraAcentos(Mensagem[1]), 80, ' '), 1, 80);// CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO

     if Mensagem.Count >= 3 then
        Result := Result +
                  Copy(PadRight(TiraAcentos(Mensagem[2]), 80, ' '), 1, 80) // CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
     else
        Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

     if Mensagem.Count >= 4 then
        Result := Result +
                  Copy(PadRight(TiraAcentos(Mensagem[3]), 80, ' '), 1, 80) // CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
     else
        Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

     if Mensagem.Count >= 5 then
        Result := Result +
                  Copy(PadRight(TiraAcentos(Mensagem[4]), 80, ' '), 1, 80) // CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
     else
        Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS


     Result := Result +
               space(45)                                                                     +  // 322 a 366 - Filler
               IntToStrZero(StrToIntDef(trim(Carteira), 0), 3)                               +  // 367 a 369 - Carteira
               IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia), 0), 5)       +  // 370 a 374 - Agencia
               IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta)  , 0), 7)       +
               IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.ContaDigito)  , 0), 1) +  // 375 a 382 - Conta e digito
               lNossoNumero + lDigNossoNumero                                                +  // 383 a 394 - Nosso Numero
               IntToStrZero( nRegistro + 1, 6);                                                 // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
  end;

end;

function TACBrBancoAlfa.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamento);
end;

function TACBrBancoAlfa.ConverterDigitoModuloFinal(): String;
begin
  if Modulo.ModuloFinal = 1 then
      Result:= '0'
   else
      Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoAlfa.DefineNumeroDocumentoModulo(const ACBrTitulo: TACBrTitulo): String;
var
  Docto: String;
begin
  Result := '0';
  Docto := '';

  with ACBrTitulo do
  begin
    if MatchText( Carteira , ['11','21','31','41']) then
      Docto := StringOfChar('0', 2) + PadLeft(NossoNumero,TamanhoMaximoNossoNum,'0')
    else
      Docto := Carteira + PadLeft(ACBrTitulo.NossoNumero,TamanhoMaximoNossoNum,'0');
  end;
  Result := Docto;

end;

function TACBrBancoAlfa.DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var
  lNossoNum, lDigNossoNum: String;
begin
  ValidaNossoNumeroResponsavel(lNossoNum, lDigNossoNum, ACBrTitulo);
  with ACBrTitulo.ACBrBoleto do
  begin
    Result := IntToStrZero(StrToIntDef(Cedente.Agencia,0),4) +
              IntToStrZero(StrToIntDef(ACBrTitulo.Carteira,0),2) +
              IntToStrZero(StrToIntDef(lNossoNum,0),10) + lDigNossoNum +
              PadLeft(RightStr(Cedente.Conta + Cedente.ContaDigito,7),7,'0') +
              '0';
  end;
end;

procedure TACBrBancoAlfa.ValidaNossoNumeroResponsavel(out ANossoNumero: String; out ADigVerificador: String;
  const ACBrTitulo: TACBrTitulo);
begin
  ANossoNumero:= '0';
  ADigVerificador:= '0';

  if (ACBrTitulo.ACBrBoleto.Cedente.ResponEmissao = tbBancoEmite) then
  begin
    ANossoNumero := StringOfChar('0', CalcularTamMaximoNossoNumero(ACBrTitulo.Carteira, ACBrTitulo.NossoNumero) );
    ADigVerificador := '0';

  end
  else
  begin
    ANossoNumero := PadLeft(ACBrTitulo.NossoNumero,TamanhoMaximoNossoNum, '0');
    ADigVerificador := CalcularDigitoVerificador(ACBrTitulo);
    if (ANossoNumero = EmptyStr) then
      ADigVerificador := '0';
  end;

end;

function TACBrBancoAlfa.DefineTamanhoAgenciaRemessa: Integer;
begin
  fpTamanhoAgencia:= 5;
  Result:= fpTamanhoAgencia;
end;

function TACBrBancoAlfa.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
  Result:= IntToStrZero(StrToIntDef(OnlyNumber(ACBrTitulo.NossoNumero),0),10) +'-'+CalcularDigitoVerificador(ACBrTitulo);
end;

procedure TACBrBancoAlfa.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  sOcorrencia, sEspecie, sAceite, aAgencia, aDigitoConta, aCarteira: String;
  sProtesto, sTipoSacado, aConta, sDataDesconto: String;
  wLinha, sNossoNumero, sDigitoNossoNumero, sTipoBoleto: String;

begin
   with ACBrTitulo do
   begin
     ValidaNossoNumeroResponsavel(sNossoNumero, sDigitoNossoNumero, ACBrTitulo);

     aAgencia := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia),0),5);
     aConta := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta),0),7);
     aCarteira := IntToStrZero(StrToIntDef(trim(Carteira),0), 3);
     aDigitoConta := PadLeft(trim(ACBrBoleto.Cedente.ContaDigito),1,'0');

     {Código da Ocorrencia}
     sOcorrencia:= TipoOcorrenciaToCodRemessa(OcorrenciaOriginal.Tipo);

     {Tipo de Boleto}
     sTipoBoleto:= DefineTipoBoleto(ACBrTitulo);

     {Especie}
     sEspecie:= DefineEspecieDoc(ACBrTitulo);

     {Aceite}
     sAceite:= DefineAceite(ACBrTitulo);

     {Data Desconto}
     sDataDesconto := DefineDataDesconto(ACBrTitulo, 'ddmmyy');

     {Intruções}
     sProtesto:= InstrucoesProtesto(ACBrTitulo);

     {Tipo de Sacado}
     sTipoSacado := DefineTipoSacado(ACBrTitulo);

     with ACBrBoleto do
     begin

       wLinha:= '1'                                            +  // 001 a 001 - ID Registro
       Space(19)                                               +  // 002 a 020 - Dados p/ Débito Automático
       '0'                                                     +  // 021 a 021 - Zero
       aCarteira                                               +  // 022 a 024 - código da carteira
       aAgencia                                                +  // 025 a 029 - código da Agência Cedente, sem o dígito
       aConta                                                  +  // 030 a 036 - Conta Corrente
       aDigitoConta                                            +  // 037 a 037 - dígito da Conta
       PadRight( SeuNumero,25,' ')                             +  // 038 a 062 - Numero de Controle do Participante
       IntToStrZero(fpNumero, 3)                               +  // 063 a 065 - Código do Banco
       StringOfChar('0',5)                                     +  // 066 a 070 - Zeros
       sNossoNumero + sDigitoNossoNumero                       +  // 071 a 082 - Identificação do Titulo + Digito de auto conferencia de número bancário
       IntToStrZero( round( ValorDescontoAntDia * 100), 10)    +  // 083 a 092 - Desconto Bonificação por dia
       sTipoBoleto                                             +  // 093 a 093 - Tipo de Boleto - Quem emite
       Space(1)                                                +  // 094 a 094 - Branco
       PadRight( Cedente.Operacao, 10)                         +  // 095 a 104 - Fornecido pelo Banco
       Space(4)                                                +  // 105 a 108 - Branco
       sOcorrencia                                             +  // 109 a 110 - Identificação da Ocorrência
       PadRight( NumeroDocumento, 10)                          +  // 111 a 120 - Numero Documento Cedente
       FormatDateTime( 'ddmmyy', Vencimento)                   +  // 121 a 126 - Data Vencimento
       IntToStrZero( Round( ValorDocumento * 100 ), 13)        +  // 127 a 139 - Valo Titulo
       StringOfChar('0',3)                                     +  // 140 a 143 - Banco Encarregado da Cobrança
       StringOfChar('0',5)                                     +  // 144 a 147 - Agência Depositária
       PadRight(sEspecie,2)                                    +  // 148 a 149 - Especie Documento
       sAceite                                                 +  // 150 a 150 - Define Aceite
       FormatDateTime( 'ddmmyy', DataDocumento )               +  // 151 a 156 - Data de Emissão
       sProtesto                                               +  // 157 a 158 - 1ª INSTRUÇÃO  // 159 a 160 - 2ª INSTRUÇÃO
       IntToStrZero( round(ACBrTitulo.ValorMoraJuros * 100 ), 13)+// 161 a 173 - Valor a ser cobrado por dia de atraso}
       SDataDesconto                                           +  // 174 a 179 - Data limite para concessão desconto
       IntToStrZero( round( ValorDesconto * 100 ), 13)         +  // 180 a 192 - Valor Desconto
       IntToStrZero( round( ValorIOF * 100 ), 13)              +  // 193 a 205 - Valor IOF
       IntToStrZero( round( ValorAbatimento * 100 ), 13)       +  // 206 a 218 - Valor Abatimento
       sTipoSacado                                             +  // 219 a 220 - Tipo Inscricao Sacado
       PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')              +  // 221 a 234 - Número de Inscrição do Pagador
       PadRight( Sacado.NomeSacado, 40, ' ')                   +  // 235 a 274 - Nome do Pagador
       PadRight( Sacado.Logradouro + ' '                       +
                 Sacado.Numero + ' '                           +
                 Sacado.Complemento, 40)                       +  // 275 a 314 - Endereço Completo Pagador
       StringOfChar(' ',12)                                    +  // 315 a 326 - Uso Banco
       PadRight( Sacado.CEP, 8 )                               +  // 327 a 334 - CEP
       PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF),15,'0')+ // 235 a 394 - Sacado Avalista
       StringOfChar(' ', 2)                                    +  // Branco
       PadRight(Sacado.SacadoAvalista.NomeAvalista, 43, ' ')   +  // Nome do sacador/avalista
       IntToStrZero(aRemessa.Count + 1, 6)                     ;  // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO

       aRemessa.Add(UpperCase(wLinha));
       wLinha := MontaInstrucoesCNAB400(ACBrTitulo, aRemessa.Count );

       if not(wLinha = EmptyStr) then
         aRemessa.Add(UpperCase(wLinha));

      end;
   end;

end;

procedure TACBrBancoAlfa.LerRetorno400(ARetorno: TStringList);
begin
  fpTamanhoAgencia := 4;
  inherited LerRetorno400(ARetorno);
  DefineTamanhoAgenciaRemessa;
end;

function TACBrBancoAlfa.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
  Result :=  IntToStrZero(StrToIntDef(ACBrTitulo.ACBrBoleto.Cedente.Agencia,0),4)  +'/'+
             IntToStrZero(StrToIntDef(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Conta),0),7) +
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoAlfa.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação normal';
    07: Result := '07-Liquidação por conta';
    09: Result := '09-Baixado Automaticamente via Arquivo';
    10: Result := '10-Baixado conforme instruções da Agência';
    11: Result := '11-Em Ser – Arquivo de títulos pendentes';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    15: Result := '15-Liquidação em Cartório';
    16: Result := '16-Título Pago em Cheque – Vinculado';
    17: Result := '17-Liquidação após baixa ou Título não registrado';
    18: Result := '18-Acerto de Depositária';
    19: Result := '19-Confirmação Recebimento Instrução de Protesto';
    20: Result := '20-Confirmação Recebimento Instrução Sustação de Protesto';
    21: Result := '21-Acerto do Controle do Participante';
    22: Result := '22-Título Com Pagamento Cancelado';
    23: Result := '23-Entrada do Título em Cartório';
    24: Result := '24-Entrada rejeitada por CEP Irregular';
    27: Result := '27-Baixa Rejeitada';
    28: Result := '28-Débito de tarifas/custas';
    30: Result := '30-Alteração de Outros Dados Rejeitados';
    32: Result := '32-Instrução Rejeitada';
    33: Result := '33-Confirmação Pedido Alteração Outros Dados';
    34: Result := '34-Retirado de Cartório e Manutenção Carteira';
    35: Result := '35-Desagendamento do débito automático';
    68: Result := '68-Acerto dos dados do rateio de Crédito';
    69: Result := '69-Cancelamento dos dados do rateio';
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoAlfa.CodOcorrenciaToTipo(const CodOcorrencia: Integer ) : TACBrTipoOcorrencia;
begin

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoLiquidadoPorConta;
    09: Result := toRetornoBaixadoViaArquivo;
    10: Result := toRetornoBaixadoInstAgencia;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoLiquidadoEmCartorio;
    16: Result := toRetornoTituloPagoEmCheque;
    17: Result := toRetornoLiquidadoAposBaixaouNaoRegistro;
    18: Result := toRetornoAcertoDepositaria;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoAcertoControleParticipante;
    22: Result := toRetornoTituloPagamentoCancelado;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoEntradaRejeitaCEPIrregular;
    27: Result := toRetornoBaixaRejeitada;
    28: Result := toRetornoDebitoTarifas;
    30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
    32: Result := toRetornoInstrucaoRejeitada;
    33: Result := toRetornoRecebimentoInstrucaoAlterarDados;
    34: Result := toRetornoRetiradoDeCartorio;
    35: Result := toRetornoDesagendamentoDebitoAutomatico;
    68: Result := toRetornoAcertoDadosRateioCredito;
    69: Result := toRetornoCancelamentoDadosRateio;

  else
    Result := toRetornoOutrasOcorrencias;
  end;

end;

function TACBrBancoAlfa.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                         : Result := '02';
    toRetornoRegistroRecusado                           : Result := '03';
    toRetornoLiquidado                                  : Result := '06';
    toRetornoLiquidadoPorConta                          : Result := '07';
    toRetornoBaixadoViaArquivo                          : Result := '09';
    toRetornoBaixadoInstAgencia                         : Result := '10';
    toRetornoTituloEmSer                                : Result := '11';
    toRetornoAbatimentoConcedido                        : Result := '12';
    toRetornoAbatimentoCancelado                        : Result := '13';
    toRetornoVencimentoAlterado                         : Result := '14';
    toRetornoLiquidadoEmCartorio                        : Result := '15';
    toRetornoTituloPagoEmCheque                         : Result := '16';
    toRetornoLiquidadoAposBaixaouNaoRegistro            : Result := '17';
    toRetornoAcertoDepositaria                          : Result := '18';
    toRetornoRecebimentoInstrucaoProtestar              : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto         : Result := '20';
    toRetornoAcertoControleParticipante                 : Result := '21';
    toRetornoTituloPagamentoCancelado                   : Result := '22';
    toRetornoEncaminhadoACartorio                       : Result := '23';
    toRetornoEntradaRejeitaCEPIrregular                 : Result := '24';
    toRetornoBaixaRejeitada                             : Result := '27';
    toRetornoDebitoTarifas                              : Result := '28';
    toRetornoAlteracaoOutrosDadosRejeitada              : Result := '30';
    toRetornoInstrucaoRejeitada                         : Result := '32';
    toRetornoRecebimentoInstrucaoAlterarDados           : Result := '33';
    toRetornoRetiradoDeCartorio                         : Result := '34';
    toRetornoDesagendamentoDebitoAutomatico             : Result := '35';
    toRetornoAcertoDadosRateioCredito                   : Result := '68';
    toRetornoCancelamentoDadosRateio                    : Result := '69';
  else
    Result := '02';
  end;

end;

function TACBrBancoAlfa.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
   case TipoOcorrencia of
      toRetornoRegistroConfirmado:
      case CodMotivo  of
         00: Result := '00-Ocorrencia aceita';
         01: Result := '01-Codigo de banco inválido';
         04: Result := '04-Cod. movimentacao nao permitido p/ a carteira';
         15: Result := '15-Caracteristicas de Cobranca Imcompativeis';
         17: Result := '17-Data de vencimento anterior a data de emissão';
         21: Result := '21-Espécie do Título inválido';
         24: Result := '24-Data da emissão inválida';
         38: Result := '38-Prazo para protesto inválido';
         39: Result := '39-Pedido para protesto não permitido para título';
         43: Result := '43-Prazo para baixa e devolução inválido';
         45: Result := '45-Nome do Sacado inválido';
         46: Result := '46-Tipo/num. de inscrição do Sacado inválidos';
         47: Result := '47-Endereço do Sacado não informado';
         48: Result := '48-CEP invalido';
         50: Result := '50-CEP referente a Banco correspondente';
         53: Result := '53-Nº de inscrição do Sacador/avalista inválidos (CPF/CNPJ)';
         54: Result := '54-Sacador/avalista não informado';
         67: Result := '67-Débito automático agendado';
         68: Result := '68-Débito não agendado - erro nos dados de remessa';
         69: Result := '69-Débito não agendado - Sacado não consta no cadastro de autorizante';
         70: Result := '70-Débito não agendado - Cedente não autorizado pelo Sacado';
         71: Result := '71-Débito não agendado - Cedente não participa da modalidade de débito automático';
         72: Result := '72-Débito não agendado - Código de moeda diferente de R$';
         73: Result := '73-Débito não agendado - Data de vencimento inválida';
         75: Result := '75-Débito não agendado - Tipo do número de inscrição do sacado debitado inválido';
         76: Result := '76-Pagador Eletrônico DDA (NOVO)- Esse motivo somente será disponibilizado no arquivo retorno para as empresas cadastradas nessa condição';
         86: Result := '86-Seu número do documento inválido';
         89: Result := '89-Email sacado nao enviado - Titulo com debito automatico';
         90: Result := '90-Email sacado nao enviado - Titulo com cobranca sem registro';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoRegistroRecusado:
      case CodMotivo of
        02: Result := '02– Código do registro detalhe inválido';
        03: Result := '03– Código da ocorrência inválida';
        04: Result := '04– Código de ocorrência não permitida para a carteira';
        05: Result := '05– Código de ocorrência não numérico';
        07: Result := '07– Agência/conta/Digito - |Inválido';
        08: Result := '08– Nosso número inválido';
        09: Result := '09– Nosso número duplicado';
        10: Result := '10– Carteira inválida';
        16: Result := '16– Data de vencimento inválida';
        18: Result := '18– Vencimento fora do prazo de operação';
        20: Result := '20– Valor do Título inválido';
        21: Result := '21– Espécie do Título inválida';
        22: Result := '22– Espécie não permitida para a carteira';
        24: Result := '24– Data de emissão inválida';
        38: Result := '38– Prazo para protesto inválido';
        44: Result := '44– Agência Cedente não prevista';
        50: Result := '50– CEP irregular - Banco Correspondente';
        63: Result := '63– Entrada para Título já cadastrado';
        65: Result := '65– Título rejeitado na operação - Cevar.';
        68: Result := '68– Débito não agendado – erro nos dados de remessa';
        69: Result := '69– Débito não agendado – Sacado não consta no cadastro de autorizante';
        70: Result := '70– Débito não agendado – Cedente não autorizado pelo Sacado';
        71: Result := '71– Débito não agendado – Cedente não participa do débito Automático';
        72: Result := '72– Débito não agendado – Código de moeda diferente de R$';
        73: Result := '73– Débito não agendado – Data de vencimento inválida';
        74: Result := '74– Débito não agendado – Conforme seu pedido, Título não registrado';
        75: Result := '75– Débito não agendado – Tipo de número de inscrição do debitado inválido';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoLiquidado:
      case CodMotivo of
         00: Result:= '00- Titulo pago com dinheiro';
         15: Result:= '15- Titulo pago com cheque';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoBaixadoViaArquivo:
      case CodMotivo of
         10: Result:= '10- Baixa comandada pelo cliente';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoBaixadoInstAgencia:
      case CodMotivo of
         00: Result:= '00- Baixado conforme instrucoes na agencia';
         14: Result:= '14- Titulo protestado';
         15: Result:= '15- Titulo excluido';
         16: Result:= '16- Titulo baixado pelo banco por decurso de prazo';
         20: Result:= '20- Titulo baixado e transferido para desconto';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoLiquidadoEmCartorio:
      case CodMotivo of
         00: Result:= '00- Pago com dinheiro';
         15: Result:= '15- Pago com cheque';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoLiquidadoAposBaixaouNaoRegistro:
      case CodMotivo of
         00: Result:= '00- Pago com dinheiro';
         15: Result:= '15- Pago com cheque';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoEntradaRejeitaCEPIrregular:
      case CodMotivo of
         48: Result:= '48- CEP invalido';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoBaixaRejeitada:
      case CodMotivo of
         04: Result:= '04- Codigo de ocorrencia nao permitido para a carteira';
         07: Result:= '07- Agencia\Conta\Digito invalidos';
         08: Result:= '08- Nosso numero invalido';
         10: Result:= '10- Carteira invalida';
         15: Result:= '15- Carteira\Agencia\Conta\NossoNumero invalidos';
         40: Result:= '40- Titulo com ordem de protesto emitido';
         42: Result:= '42- Codigo para baixa/devolucao via Telebradesco invalido';
         60: Result:= '60- Movimento para titulo nao cadastrado';
         77: Result:= '77- Transferencia para desconto nao permitido para a carteira';
         85: Result:= '85- Titulo com pagamento vinculado';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoDebitoTarifas:
      case CodMotivo of
         03: Result:= '03- Tarifa de sustação';
         04: Result:= '04- Tarifa de protesto';
         08: Result:= '08- Custas de protesto';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoALteracaoOutrosDadosRejeitada:
      case CodMotivo of
         01: Result:= '01- Código do Banco inválido';
         04: Result:= '04- Código de ocorrência não permitido para a carteira';
         05: Result:= '05- Código da ocorrência não numérico';
         08: Result:= '08- Nosso número inválido';
         15: Result:= '15- Característica da cobrança incompatível';
         16: Result:= '16- Data de vencimento inválido';
         17: Result:= '17- Data de vencimento anterior a data de emissão';
         18: Result:= '18- Vencimento fora do prazo de operação';
         24: Result:= '24- Data de emissão Inválida';
         29: Result:= '29- Valor do desconto maior/igual ao valor do Título';
         30: Result:= '30- Desconto a conceder não confere';
         31: Result:= '31- Concessão de desconto já existente ( Desconto anterior )';
         33: Result:= '33- Valor do abatimento inválido';
         34: Result:= '34- Valor do abatimento maior/igual ao valor do Título';
         38: Result:= '38- Prazo para protesto inválido';
         39: Result:= '39- Pedido de protesto não permitido para o Título';
         40: Result:= '40- Título com ordem de protesto emitido';
         42: Result:= '42- Código para baixa/devolução inválido';
         60: Result:= '60- Movimento para Título não cadastrado';
         85: Result:= '85- Título com Pagamento Vinculado.';

      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoInstrucaoRejeitada:
      case CodMotivo of
         01 : Result:= '01- Código do Banco inválido';
         02 : Result:= '02- Código do registro detalhe inválido';
         04 : Result:= '04- Código de ocorrência não permitido para a carteira';
         05 : Result:= '05- Código de ocorrência não numérico';
         07 : Result:= '07- Agência/Conta/dígito inválidos';
         08 : Result:= '08- Nosso número inválido';
         10 : Result:= '10- Carteira inválida';
         15 : Result:= '15- Características da cobrança incompatíveis';
         16 : Result:= '16- Data de vencimento inválida';
         17 : Result:= '17- Data de vencimento anterior a data de emissão';
         18 : Result:= '18- Vencimento fora do prazo de operação';
         20 : Result:= '20- Valor do título inválido';
         21 : Result:= '21- Espécie do Título inválida';
         22 : Result:= '22- Espécie não permitida para a carteira';
         24 : Result:= '24- Data de emissão inválida';
         28 : Result:= '28- Código de desconto via Telebradesco inválido';
         29 : Result:= '29- Valor do desconto maior/igual ao valor do Título';
         30 : Result:= '30- Desconto a conceder não confere';
         31 : Result:= '31- Concessão de desconto - Já existe desconto anterior';
         33 : Result:= '33- Valor do abatimento inválido';
         34 : Result:= '34- Valor do abatimento maior/igual ao valor do Título';
         36 : Result:= '36- Concessão abatimento - Já existe abatimento anterior';
         38 : Result:= '38- Prazo para protesto inválido';
         39 : Result:= '39- Pedido de protesto não permitido para o Título';
         40 : Result:= '40- Título com ordem de protesto emitido';
         41 : Result:= '41- Pedido cancelamento/sustação para Título sem instrução de protesto';
         42 : Result:= '42- Código para baixa/devolução inválido';
         45 : Result:= '45- Nome do Sacado não informado';
         46 : Result:= '46- Tipo/número de inscrição do Sacado inválidos';
         47 : Result:= '47- Endereço do Sacado não informado';
         48 : Result:= '48- CEP Inválido';
         50 : Result:= '50- CEP referente a um Banco correspondente';
         53 : Result:= '53- Tipo de inscrição do sacador avalista inválidos';
         60 : Result:= '60- Movimento para Título não cadastrado';
         77 : Result:= '77- Transferência para desconto não permitido para a carteira';
         85 : Result:= '85- Título com pagamento vinculado';
         86 : Result:= '86- Seu número inválido';

      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

      toRetornoDesagendamentoDebitoAutomatico:
      case CodMotivo of
         81 : Result:= '81- Tentativas esgotadas, baixado';
         82 : Result:= '82- Tentativas esgotadas, pendente';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
   else
      Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
   end;

   Result := ACBrSTr(Result);
end;

function TACBrBancoAlfa.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result := toRemessaBaixar;                             {Pedido de Baixa}
    04 : Result := toRemessaConcederAbatimento;                 {Concessão de Abatimento}
    05 : Result := toRemessaCancelarAbatimento;                 {Cancelamento de Abatimento concedido}
    06 : Result := toRemessaAlterarVencimento;                  {Alteração de vencimento}
    07 : Result := toRemessaAlterarExclusivoCliente;            {Alteração de Campo Especial}
    08 : Result := toRemessaAlterarNumeroTituloBeneficiario;    {Alteração de seu número do Cedente}
    09 : Result := toRemessaProtestar;                          {Pedido de protesto}
    10 : Result := toRemessaNaoProtestar;                       {Pedido para não Protestar protesto}
    18 : Result := toRemessaSustarProtestoBaixarTitulo;         {Sustar protesto e baixar titulo}
    19 : Result := toRemessaSustarProtestoManterCarteira;       {Sustar protesto e manter na carteira}
    31 : Result := toRemessaAlterarOutrosDados;                 {Alteração de Outros Dados}

  else
     Result:= toRemessaRegistrar;                               {Remessa}
  end;

end;

function TACBrBancoAlfa.TipoOcorrenciaToCodRemessa(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
      toRemessaBaixar                         : Result := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento             : Result := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento             : Result := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento              : Result := '06'; {Alteração de vencimento}
      toRemessaAlterarExclusivoCliente        : Result := '07'; {Alteração de Campo Especial}
      toRemessaAlterarNumeroTituloBeneficiario: Result := '08'; {Alteração de seu número do Cedente}
      toRemessaProtestar                      : Result := '09'; {Pedido de protesto}
      toRemessaNaoProtestar                   : Result := '10'; {Pedido para não Protestar protesto}
      toRemessaSustarProtestoBaixarTitulo     : Result := '18'; {Sustar protesto e baixar titulo}
      toRemessaSustarProtestoManterCarteira   : Result := '19'; {Sustar protesto e manter na carteira}
      toRemessaAlterarOutrosDados             : Result := '31'; {Alteração de Outros Dados}

  else
      Result := '01';                                           {Remessa}
  end;

end;

end.
