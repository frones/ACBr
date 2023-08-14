  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
  {                                                                              }
  { Colaboradores nesse arquivo: Victor H Gonzales                               }
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

  //Incluido 22/03/2023

{$I ACBr.inc}
unit ACBrBancoPefisa;

interface

uses
  Classes,
  Contnrs,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoConversao;

type

    { TACBrBancoPefisa }

  TACBrBancoPefisa = class(TACBrBancoClass)
  private
    function ConverterMultaPercentual(const ACBrTitulo: TACBrTitulo): Double;
    function ConverterTipoPagamento(const ATipoPagamento: TTipo_Pagamento): String;
  protected
    function ConverterDigitoModuloFinal(): String; override;
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function GetLocalPagamento: String; override;
  public
    Constructor create(AOwner: TACBrBanco);
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList); override;
    function MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo; const nRegistro: Integer): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;
    function DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    procedure EhObrigatorioAgenciaDV; override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    procedure LerRetorno400(ARetorno: TStringList); override;
  end;

implementation

uses
  StrUtils,
  ACBrBase,
  ACBrUtil.Strings,
  ACBrUtil.Base, ACBrUtil;

  { TACBrBancoPefisa }
function TACBrBancoPefisa.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

//  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
//  begin
//    case CodOcorrencia of
//      04: Result := '04-Transferência de Carteira/Entrada';
//      05: Result := '05-Transferência de Carteira/Baixa';
//      07: Result := '07-Confirmação do Recebimento da Instrução de Desconto';
//      08: Result := '08-Confirmação do Recebimento do Cancelamento do Desconto';
//      15: Result := '15-Franco de Pagamento';
//      24: Result := '24-Retirada de Cartório e Manutenção em Carteira';
//      25: Result := '25-Protestado e Baixado';
//      26: Result := '26-Instrução Rejeitada';
//      27: Result := '27-Confirmação do Pedido de Alteração de Outros Dados';
//      33: Result := '33-Confirmação da Alteração dos Dados do Rateio de Crédito';
//      34: Result := '34-Confirmação do Cancelamento dos Dados do Rateio de Crédito';
//      36: Result := '36-Confirmação de Envio de E-mail/SMS';
//      37: Result := '37-Envio de E-mail/SMS Rejeitado';
//      38: Result := '38-Confirmação de Alteração do Prazo Limite de Recebimento';
//      39: Result := '39-Confirmação de Dispensa de Prazo Limite de Recebimento';
//      40: Result := '40-Confirmação da Alteração do Número do Título Dado pelo Beneficiario';
//      41: Result := '41-Confirmação da Alteração do Número Controle do Participante';
//      42: Result := '42-Confirmação da Alteração dos Dados do Pagador';
//      43: Result := '43-Confirmação da Alteração dos Dados do Sacador/Avalista';
//      44: Result := '44-Título Pago com Cheque Devolvido';
//      45: Result := '45-Título Pago com Cheque Compensado';
//      46: Result := '46-Instrução para Cancelar Protesto Confirmada';
//      47: Result := '47-Instrução para Protesto para Fins Falimentares Confirmada';
//      48: Result := '48-Confirmação de Instrução de Transferência de Carteira/Modalidade de Cobrança';
//      49: Result := '49-Alteração de Contrato de Cobrança';
//      50: Result := '50-Título Pago com Cheque Pendente de Liquidação';
//      51: Result := '51-Título DDA Reconhecido pelo Pagador';
//      52: Result := '52-Título DDA não Reconhecido pelo Pagador';
//      53: Result := '53-Título DDA recusado pela CIP';
//      54: Result := '54-Confirmação da Instrução de Baixa de Título Negativado sem Protesto';
//    end;
//  end
//  else
//  begin
    case CodOcorrencia of
      04: Result := '04-Alteração de Dados (Entrada)';
      05: Result := '05-Alteração de Dados (Baixa)';
      07: Result := '07-Liquidação após Baixa';
      08: Result := '08-Liquidação em Cartório';
      10: Result := '10-Baixa comandada do cliente arquivo';
      15: Result := '15-Baixa rejeitada';
      16: Result := '16-Instrução rejeitada';
      //18: Result := '18-Acerto de Depositária';
      21: Result := '21-Confirma instrução de não protestar';
      //22: Result := '22-Titulo com Pagamento Cancelado';
      //24: Result := '24-Entrada Rejeitada por CEP Irregular';
      //25: Result := '25-Confirmação Recebimento Instrução de Protesto Falimentar';
      //27: Result := '27-Baixa Rejeitada';
      32: Result := '32-Baixa por ter sido protestado';
      //33: Result := '33-Confirmação Pedido Alteração Outros Dados';
      //34: Result := '34-Retirado de Cartório e Manutenção Carteira';
      36: Result := '36-Custas de Edital';
      37: Result := '37-Custas de sustação judicial';
      38: Result := '38-Título sustado judicialmente';
      //40: Result := '40-Estorno de Pagamento';
      //55: Result := '55-Sustado Judicial';
      65: Result := '65-Pagamento com Cheque - Aguardando compensação';
      //68: Result := '68-Acerto dos Dados do Rateio de Crédito';
      69: Result := '69-Cancelamento de Liquidação por Cheque Devolvido';
      71: Result := '71-Protesto cancelado pelo Cartório';
//p      72: Result := '72-Baixa Operacional';
//p      74: Result := '74-Cancelamento da Baixa Operacional';
      75: Result := '75-Pagamento Parcial';
      90: Result := '90-Instrução de Protesto Rejeitada';
      95: Result := '95-Troca Uso Empresa';
      96: Result := '96-Emissão Extrato Mov. Carteira';
      97: Result := '97-Tarifa de sustação de protesto';
      98: Result := '98-Tarifa de protesto';
      99: Result := '99-Custas de protesto';
    end;
//  end;

  if (Result <> '') then
    Exit;

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação Normal';
    09: Result := '09-Baixa Simples';
    //11: Result := '11-Em Ser - Arquivo de Títulos Pendentes';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    17: Result := '17-Alterações de dados rejeitados';
    19: Result := '19-Confirma instrução de protesto';
    20: Result := '20-Confirma instruão de sustação de protesto';
    23: Result := '23-Protesto enviado a cartório';
    //28: Result := '28-Débito de tarifas/custas';
//p    29: Result := '29-Sacado não retirou boleto eletronicamente';
    //30: Result := '30-Alteração de Outros Dados Rejeitados';
    35: Result := '35-Alegações do sacado';
    //73: Result := '73-Confirmação Recebimento Pedido de Negativação';
  end;
end;

function TACBrBancoPefisa.ConverterDigitoModuloFinal(): String;
begin
  if Modulo.ModuloFinal = 1 then
    Result := 'P'
  else
    Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoPefisa.DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var
  LEmissao    : char;
  Beneficiario: TACBrCedente;
begin
  Beneficiario := ACBrTitulo.ACBrBoleto.Cedente;

  case Beneficiario.ResponEmissao of
    tbBancoEmite, tbBancoPreEmite, tbBancoNaoReemite, tbBancoReemite:
      LEmissao := '3'; //3 para cobrança registrada com emissão pelo banco
    else
      LEmissao := '4'; //4 para cobrança direta com emissão pelo cedente
  end;

  Result :=

    PadLeft(OnlyNumber(Beneficiario.CodigoCedente), 12, '0') +
    PadLeft(ACBrTitulo.NossoNumero, 10, '0') +
    PadLeft(ACBrTitulo.Carteira, 2, '0') +
    LEmissao;

end;

function TACBrBancoPefisa.DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String;
begin
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'DM') then
    Result := '01'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'DS') then
    Result := '02'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'NP') then
    Result := '03'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'NS') then
    Result := '04'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'REC') then
    Result := '05'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'LC') then
    Result := '06'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'FC') then
    Result := '07'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CN') then
    Result := '08'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CT') then
    Result := '09'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CQ') then
    Result := '10'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CS') then
    Result := '11'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'MS') then
    Result := '12'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'ND') then
    Result := '13'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'DD') then
    Result := '15'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'CPS') then
    Result := '17'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'FT') then
    Result := '31'
  else
  if AnsiSameText(ACBrTitulo.EspecieDoc, 'OUT') then
    Result := '99'
  else
    Result := ACBrTitulo.EspecieDoc;
end;

procedure TACBrBancoPefisa.EhObrigatorioAgenciaDV;
begin
//
end;

function TACBrBancoPefisa.ConverterMultaPercentual(const ACBrTitulo: TACBrTitulo): Double;
begin
  if ACBrTitulo.MultaValorFixo then
    if (ACBrTitulo.ValorDocumento > 0) then
      Result := (ACBrTitulo.PercentualMulta / ACBrTitulo.ValorDocumento) * 100
    else
      Result := 0
  else
    Result := ACBrTitulo.PercentualMulta;
end;

function TACBrBancoPefisa.ConverterTipoPagamento(const ATipoPagamento: TTipo_Pagamento): String;
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

constructor TACBrBancoPefisa.create(AOwner: TACBrBanco);
begin
  inherited create(AOwner);
  fpDigito                     := 0;
  fpNome                       := 'PEFISA';
  fpNumero                     := 174;
  fpTamanhoMaximoNossoNum      := 10;
  fpTamanhoAgencia             := 4;
  fpTamanhoConta               := 12;
  fpTamanhoCarteira            := 2;
  fpLayoutVersaoArquivo        := 22;
  fpLayoutVersaoLote           := 0;
  fpDensidadeGravacao          := '';
  fpModuloMultiplicadorInicial := 2;
  fpModuloMultiplicadorFinal   := 7;
  fpCodParametroMovimento      := '';
end;

function TACBrBancoPefisa.MontarCampoCodigoCedente(
  const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+
   '/'+
   ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

function TACBrBancoPefisa.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := '0' + 
    ACBrTitulo.Carteira +
    ACBrTitulo.NossoNumero +
    ' ' +
    CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoPefisa.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.create(ACBrStr('Não implementado CNAB240'));
end;

procedure TACBrBancoPefisa.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
var
  LLinha      : String;
  Beneficiario: TACBrCedente;
begin
  Beneficiario := ACBrBanco.ACBrBoleto.Cedente;

  LLinha := '0' +                                                               // 001 a 001 - Tipo de Registro
    '1' +                                                                       // 002 a 002 - Código Remessa
    'REMESSA' +                                                                 // 003 a 009 - Literal Remessa
    '01' +                                                                      // 010 a 011 - Código Serviço
    PadRight('COBRANCA', 8) +                                                   // 012 a 019 - Literal Cobranca
    Space(7) +                                                                  // 020 a 026 - Uso do Banco Brancos
    PadLeft(Beneficiario.CodigoCedente, 12, '0') +                              // 027 a 038 - Código do Cedente
    Space(8) +                                                                  // 039 a 046 - Uso do Banco Brancos
    PadRight(Beneficiario.Nome, 30) +                                           // 047 a 076 - Nome da Empresa
    IntToStrZero(fpNumero, 3) +                                                 // 077 a 079 - Código do Banco
    Space(15) +                                                                 // 080 a 094 - Uso do Banco Brancos
    FormatDateTime('ddmmyy', Now) +                                             // 095 a 100 - Data de Gravação
    Space(8) +                                                                  // 101 a 108 - Uso do Banco Brancos
    PadLeft(' ', 12) +                                                          // 109 a 120 - Conta Cobrança Direta
    Space(266) +                                                                // 121 a 386 - Uso do Banco Brancos
    IntToStrZero(NumeroRemessa, 8) +                                            // 387 a 394 - Sequecial de Remessa
    IntToStrZero(1, 6);                                                         // 395 a 400 - Sequencial

  ARemessa.Add(TiraAcentos(UpperCase(LLinha)));
end;

procedure TACBrBancoPefisa.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; ARemessa: TStringList);
var
  LTitulo     : TACBrTitulo;
  Beneficiario: TACBrCedente;
  Pagador     : TACBrSacado;
  LLinha      : String;
begin

  LTitulo      := ACBrTitulo;
  Beneficiario := LTitulo.ACBrBoleto.Cedente;
  Pagador      := LTitulo.Sacado;

  LLinha := '1' +                                                               // 001 a 001 - Tipo de Registro
    PadLeft(DefineTipoInscricao, 2, '0') +                                      // 002 a 003 - Tipo de Inscrição Empresa
    PadLeft(OnlyNumber(Beneficiario.CNPJCPF), 14, '0') +                        // 004 a 017 - CNPJ Empresa
    PadRight(Beneficiario.CodigoTransmissao, 12) +                              // 018 a 029 - Código da Empresa
    Space(8) +                                                                  // 030 a 037 - Uso do Banco Brancos
    PadRight(LTitulo.NumeroDocumento, 25) +                                     // 038 a 062 - Uso da Empresa
    PadRight( '0'+
              LTitulo.NossoNumero +
              CalcularDigitoVerificador(LTitulo), 12) +                         // 063 a 074 - Nosso Número
    Space(8) +                                                                  // 075 a 082 - Uso do Banco Brancos
    Space(3) +                                                                  // 083 a 085 - Código do Banco
    Space(21) +                                                                 // 086 a 106 - Uso do Banco Brancos
    PadLeft(LTitulo.Carteira, 2, '0') +                                         // 107 a 108 - Código da Carteira
    TipoOcorrenciaToCodRemessa(LTitulo.OcorrenciaOriginal.Tipo) +               // 109 a 110 - Código Ocorrência Remessa
    PadRight(LTitulo.SeuNumero, 10) +                                           // 111 a 120 - Seu Número
    FormatDateTime('ddmmyy', LTitulo.Vencimento) +                              // 121 a 126 - Data Vencimento
    IntToStrZero(Round(LTitulo.ValorDocumento * 100), 13) +                     // 127 a 139 - Valo Titulo
    Space(8) +                                                                  // 140 a 147 - Uso do Banco Brancos
    DefineEspecieDoc(ACBrTitulo) +                                              // 148 a 149 - Espécie do Título
    DefineAceite(LTitulo) +                                                     // 150 a 150 - Aceite
    FormatDateTime('ddmmyy', LTitulo.DataDocumento) +                           // 151 a 156 - Data Emissão Título
    InstrucoesProtesto(LTitulo) +                                               // 157 a 158 - Instrucao 1 159 a 160 - Instrucao 2
    IntToStrZero(Round(LTitulo.ValorMoraJuros * 100), 13) +                     // 161 a 173 - Juros ao Dia
    IfThen(LTitulo.DataDesconto < EncodeDate(2000, 01, 01),
        '000000', 
        FormatDateTime('ddmmyy', LTitulo.DataDesconto)) +                       // 174 a 179 - Data Desconto
    IntToStrZero(Round(LTitulo.ValorDesconto * 100), 13) +                      // 180 a 192 - Valor Desconto
    IfThen(LTitulo.DataMulta < EncodeDate(2000, 01, 01), 
        '000000', 
        FormatDateTime('ddmmyy', LTitulo.DataMulta)) +                          // 193 a 198 - Data Multa
    Space(7) +                                                                  // 199 a 205 - Uso do Banco Brancos
    IntToStrZero(Round(LTitulo.ValorAbatimento * 100), 13) +                    // 206 a 218 - Valor Abatimento
    DefineTipoSacado(LTitulo) +                                                 // 219 a 220 - Tipo Sacado
    PadLeft(OnlyNumber(Pagador.CNPJCPF), 14, '0') +                             // 221 a 234 - CNPJ/CPF Sacado
    PadRight(Pagador.NomeSacado, 40) +                                          // 235 a 274 - Nome do Sacado
    PadRight(Pagador.Logradouro + 
        ' ' + 
        Pagador.Numero + 
        ' ' + 
        Pagador.Complemento, 40) +                                              // 275 a 314 - Endereço Sacado
    PadRight(Pagador.Bairro, 12) +                                              // 315 a 326 - Bairro Sacado
    PadRight(OnlyNumber(Pagador.CEP), 8) +                                      // 327 a 334 - CEP Sacado
    PadRight(Pagador.Cidade, 15) +                                              // 335 a 349 - Cidade Sacado
    PadRight(Pagador.UF, 2) +                                                   // 350 a 351 - UF Sacado
    Space(30) +                                                                 // 352 a 381 - Sacador / Mensagem / Código CMC7
    IfThen(LTitulo.PercentualMulta > 0, '2', '0') +                             // 382 a 382 - Tipo de Multa
    IntToStrZero(Round(ConverterMultaPercentual(ACBrTitulo)), 2) +              // 383 a 384 - Percentual de Multa
    Space(1) +                                                                  // 385 a 385 - Uso do Banco Brancos
    IfThen(LTitulo.DataMoraJuros < EncodeDate(2000, 01, 01), 
        '000000', 
        FormatDateTime('ddmmyy', LTitulo.DataMoraJuros)) +                      // 386 a 391 - Data Juros Mora
    PadLeft(IntToStr(LTitulo.DiasDeProtesto), 2, '0') +                         // 392 a 393 - Prazo dias para Cartório
    Space(1) +                                                                  // 394 a 394 - Uso do Banco Brancos
    IntToStrZero(ARemessa.Count + 1, 6) +                                       // 395 a 400 - Seqüencial

    // [401 a 710] - Campos opcionais
    PadLeft(DefineTipoInscricao, 2, '0') +                                      // 401 a 402 - Tipo Emitente
    PadLeft(OnlyNumber(Beneficiario.CNPJCPF), 14, '0') +                        // 403 a 416 - CNPJ Emitente
    PadRight(Beneficiario.Nome, 40) +                                           // 417 a 456 - Nome Emitente
    PadRight(Beneficiario.Logradouro + 
         ' ' + 
         Beneficiario.NumeroRes + 
         ' ' + 
         Beneficiario.Complemento, 40) +                                        // 457 a 496 - Endereço Emitente
    PadRight(Beneficiario.Cidade, 15) +                                         // 497 a 511 - Cidade Emitente
    PadRight(Beneficiario.UF, 2) +                                              // 512 a 513 - UF Emitente
    PadRight(OnlyNumber(Beneficiario.CEP), 8) +                                 // 514 a 521 - CEP Emitente
    PadRight(Pagador.Email, 120) +                                              // 522 a 641 - Email
    IfThen(LTitulo.DataDesconto2 < EncodeDate(2000, 01, 01), 
         '      ',
         FormatDateTime('ddmmyy', LTitulo.DataDesconto2)) +                     // 642 a 647 - Data Desconto2
    PadRight(IfThen(LTitulo.ValorDesconto2 > 0 ,
      IntToStrZero(Round(LTitulo.ValorDesconto2 * 100), 13), ' '),13) +         // 648 a 660 - Valor Desconto2

    IfThen(LTitulo.DataDesconto3 < EncodeDate(2000, 01, 01),
         '      ',
         FormatDateTime('ddmmyy', LTitulo.DataDesconto3)) +                     // 661 a 666 - Data Desconto3
    PadRight(IfThen(LTitulo.ValorDesconto3 > 0 ,
    IntToStrZero(Round(LTitulo.ValorDesconto3 * 100), 13), ' '),13) +           // 667 a 679 - Valor Desconto3
    ConverterTipoPagamento(LTitulo.TipoPagamento) +                             // 680 a 680 - Indicativo de Autorização para Recebimento de Valor Divergente
    IfThen((LTitulo.ValorMinPagamento = 0)
           or (LTitulo.ValorMaxPagamento = 0),
           ' ',
      IfThen(LTitulo.PercentualMinPagamento > 0,
             'P',
             'V')
    ) +                                                                         // 681 a 681 - Indicativo de valor ou percentual para o range mínimo e máximo de aceitação do pagamento
    IfThen(LTitulo.ValorMinPagamento = 0,
      PadLeft(' ',13),
        IfThen(LTitulo.ValorMinPagamento > 0,
           IntToStrZero(Round(LTitulo.ValorMinPagamento * 100), 13),
           IntToStrZero(Round(LTitulo.PercentualMinPagamento * 100), 13))
    ) +                                                                         // 682 a 694 - Valor ou Percentual Mínimo para aceitação do pagamento
    IfThen(LTitulo.ValorMaxPagamento = 0,
      PadLeft(' ',13),
        IfThen(LTitulo.ValorMaxPagamento > 0,
         IntToStrZero(Round(LTitulo.ValorMaxPagamento * 100), 13),
         IntToStrZero(Round(LTitulo.PercentualMaxPagamento * 100), 13))
    ) +                                                                         // 695 a 707 - Valor ou Percentual Maximo para aceitação do pagamento
    Space(1) +                                                                  // 708 a 708 - Uso do Banco Brancos
    ifThen(LTitulo.QtdePagamentoParcial=0,
      PadLeft(' ',2),
      IntToStrZero(LTitulo.QtdePagamentoParcial, 2)
    );                                                                          // 709 a 710 - Quantidade de pagamentos parciais

  ARemessa.Add(TiraAcentos(UpperCase(LLinha)));

  LLinha := MontaInstrucoesCNAB400(ACBrTitulo, ARemessa.Count);

  if not (LLinha = EmptyStr) then
    ARemessa.Add(TiraAcentos(UpperCase(LLinha)));
end;

function TACBrBancoPefisa.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamentoTodaRede);
end;

procedure TACBrBancoPefisa.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia  :Integer;
  CodMotivo, i, MotivoLinha :Integer;
  CodMotivo_19    :String;
  rConta, rDigitoConta      :String;
  Linha, rCedente, rCNPJCPF :String;
  rCodEmpresa               :String;
begin
  //ErroAbstract('LerRetorno400');

  //Utiliza o layout padrão para leitura de retorno CNAB400
  if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));


  rCedente   := trim(Copy(ARetorno[0],39,12));

  // A leitura deverá ser feita a partir da posição 26 devido ao fato de não existirem agências bancárias com mais de 4 (quatro) algarismos.
  //rAgencia := trim(Copy(ARetorno[1], 26, ACBrBanco.TamanhoAgencia));
  rConta   := trim(Copy(ARetorno[0], 27, 11));

  rDigitoConta := Copy(ARetorno[0], 38 ,1);

  rCodEmpresa:= rConta + rDigitoConta;
  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],109,5),0);

  if (StrToIntDef( Copy(ARetorno[0], 95, 6 ), 0) > 0) then
    ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                            Copy(ARetorno[0],97,2)+'/'+
                                                            Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

  if (StrToIntDef( Copy(ARetorno[0], 380, 6 ), 0) > 0) then
    ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(Copy(ARetorno[0],380,2)+'/'+
                                                                Copy(ARetorno[0],382,2)+'/'+
                                                                Copy(ARetorno[0],384,2),0, 'DD/MM/YY' );

  case StrToIntDef(Copy(ARetorno[1],2,2),0) of
     11: rCNPJCPF := Copy(ARetorno[1],7,11);
     14: rCNPJCPF := Copy(ARetorno[1],4,14);
  else
    rCNPJCPF := Copy(ARetorno[1],4,14);
  end;

  //ValidarDadosRetorno(rAgencia, rConta);
  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and ((rCodEmpresa <> Cedente.CodigoTransmissao) or (rCodEmpresa <> Cedente.Conta + Cedente.ContaDigito)) then
       raise Exception.Create(ACBrStr('Código da Empresa do arquivo inválido'));

    case StrToIntDef(Copy(ARetorno[1],2,2),0) of
       11: Cedente.TipoInscricao:= pFisica;
       14: Cedente.TipoInscricao:= pJuridica;
    else
       Cedente.TipoInscricao := pJuridica;
    end;

    if LeCedenteRetorno then
    begin
       try
         Cedente.CNPJCPF := rCNPJCPF;
       except
         // Retorno quando é CPF está vindo errado por isso ignora erro na atribuição
       end;

       Cedente.CodigoCedente:= rCedente;
       Cedente.Nome         := '';
       Cedente.Agencia      := '';
       Cedente.AgenciaDigito:= '';
       Cedente.Conta        := rConta;
       Cedente.ContaDigito  := rDigitoConta;
       Cedente.CodigoTransmissao  := rCodEmpresa;
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
     Linha := ARetorno[ContLinha] ;

     if Copy(Linha,1,1)<> '1' then
        Continue;

     Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

     with Titulo do
     begin
        SeuNumero                   := copy(Linha,38,25);
        NumeroDocumento             := copy(Linha,117,10);
        OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(
                                       copy(Linha,109,2),0));

        CodOcorrencia := StrToIntDef(IfThen(copy(Linha,109,2) = '  ','00',copy(Linha,109,2)),0);

        //-|Se a ocorrencia for igual a 19 - Confirmação de Receb. de Protesto
        //-|Verifica o motivo na posição 295 - A = Aceite , D = Desprezado
        if(CodOcorrencia = 19)then
         begin
           CodMotivo_19:= copy(Linha,295,1);
           if(CodMotivo_19 = 'A')then
            begin
              MotivoRejeicaoComando.Add(copy(Linha,295,1));
              DescricaoMotivoRejeicaoComando.Add('A - Aceito');
            end
           else
            begin
              MotivoRejeicaoComando.Add(copy(Linha,295,1));
              DescricaoMotivoRejeicaoComando.Add('D - Desprezado');
            end;
         end
        else
         begin
           MotivoLinha := 319;
           for i := 0 to 4 do
           begin
              CodMotivo := StrToInt(IfThen(copy(Linha,MotivoLinha,2) = '  ','00',copy(Linha,MotivoLinha,2)));

              {Se for o primeiro motivo}
              if (i = 0) then
               begin
                 {Somente estas ocorrencias possuem motivos 00}
                 if(CodOcorrencia in [02, 06, 09, 10, 15, 17])then
                  begin
                    MotivoRejeicaoComando.Add(IfThen(copy(Linha,MotivoLinha,2) = '  ','00',copy(Linha,MotivoLinha,2)));
                    DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
                  end
                 else
                  begin
                    if(CodMotivo = 0)then
                     begin
                       MotivoRejeicaoComando.Add('00');
                       DescricaoMotivoRejeicaoComando.Add('Sem Motivo');
                     end
                    else
                     begin
                       MotivoRejeicaoComando.Add(IfThen(copy(Linha,MotivoLinha,2) = '  ','00',copy(Linha,MotivoLinha,2)));
                       DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
                     end;
                  end;
               end
              else
               begin
                 //Apos o 1º motivo os 00 significam que não existe mais motivo
                 if CodMotivo <> 0 then
                 begin
                    MotivoRejeicaoComando.Add(IfThen(copy(Linha,MotivoLinha,2) = '  ','00',copy(Linha,MotivoLinha,2)));
                    DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
                 end;
               end;

              MotivoLinha := MotivoLinha + 2; //Incrementa a coluna dos motivos
           end;
         end;

        if (StrToIntDef(Copy(Linha,111,6),0) > 0) then
          DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                               Copy(Linha,113,2)+'/'+
                                               Copy(Linha,115,2),0, 'DD/MM/YY' );
        if (StrToIntDef(Copy(Linha,147,6),0) > 0) then
           Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                              Copy(Linha,149,2)+'/'+
                                              Copy(Linha,151,2),0, 'DD/MM/YY' );

        ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;
        ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
        ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
        ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
        ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
        ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
        ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
        ValorPago            := StrToFloatDef(Copy(Linha,254,13),0)/100;
        NossoNumero          := DefineNossoNumeroRetorno(Linha);
        Carteira             := Copy(Linha,DefinePosicaoCarteiraRetorno,3);
        ValorDespesaCobranca := StrToFloatDef(Copy(Linha,176,13),0)/100;
        ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

        // informações do local de pagamento
        Liquidacao.Banco      := StrToIntDef(Copy(Linha,166,3), -1);
        Liquidacao.Agencia    := Copy(Linha,169,4);
        Liquidacao.Origem     := '';

        if (StrToIntDef(Copy(Linha,296,6),0) > 0) then
           DataCredito:= StringToDateTimeDef( Copy(Linha,296,2)+'/'+
                                              Copy(Linha,298,2)+'/'+
                                              Copy(Linha,300,2),0, 'DD/MM/YY' );
     end;
  end;
end;

function TACBrBancoPefisa.MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo; const nRegistro: Integer): String;
begin
  Result := '';

  with ACBrTitulo, ACBrBoleto do
  begin

      {Primeira instrução vai no registro 1}
    if Mensagem.Count <= 1 then
    begin
      Result := '';
      Exit;
    end;

    Result := '2' +                                                             // 001 a 001 IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO
      Copy(PadRight(Mensagem[ 1 ], 80, ' '), 1, 80);                            // 002 a 081 CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO

    if Mensagem.Count >= 3 then
      Result := Result + Copy(PadRight(Mensagem[ 2 ], 80, ' '), 1, 80)          // 082 a 161 CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    else
      Result := Result + PadRight('', 80, ' ');                                 // CONTEÚDO DO RESTANTE DAS LINHAS

    if Mensagem.Count >= 4 then
      Result := Result + Copy(PadRight(Mensagem[ 3 ], 80, ' '), 1, 80)          // 162 a 241 CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    else
      Result := Result + PadRight('', 80, ' ');                                 // CONTEÚDO DO RESTANTE DAS LINHAS

    if Mensagem.Count >= 5 then
      Result := Result + Copy(PadRight(Mensagem[ 4 ], 80, ' '), 1, 80)          // 242 a 321 CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
    else
      Result := Result + PadRight('', 80, ' ');                                 // CONTEÚDO DO RESTANTE DAS LINHAS

    Result := Result + Space(44) +                                              // 322 a 365 - Uso do Banco
      PadRight(SeuNumero, 10) +                                                 // 366 a 375 - Seu Número
      FormatDateTime('ddmmyy', Vencimento) +                                    // 376 a 381 - Data Vencimento
      IntToStrZero(Round(ValorDocumento * 100), 13) +                           // 382 a 394 - Valo Titulo
      IntToStrZero(nRegistro + 1, 6);                                           // 395 a 400 - Sequencial
  end;
end;

function TACBrBancoPefisa.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result := toRetornoRegistroConfirmado;
    03 : Result := toRetornoRegistroRecusado;
    04 : Result := toRetornoAlteracaoDadosNovaEntrada;
    05 : Result := toRetornoAlteracaoDadosBaixa;
    06 : Result := toRetornoLiquidado;
    07 : Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    08 : Result := toRetornoLiquidadoEmCartorio;
    09 : Result := toRetornoBaixaSimples;
    10 : Result := toRetornoBaixadoViaArquivo;
    12 : Result := toRetornoAbatimentoConcedido;
    13 : Result := toRetornoAbatimentoCancelado;
    14 : Result := toRetornoVencimentoAlterado;
    15 : Result := toRetornoBaixaRejeitada;
    16 : Result := toRetornoInstrucaoRejeitada;
    17 : Result := toRetornoAlteracaoDadosRejeitados;
    19 : Result := toRetornoRecebimentoInstrucaoProtestar;
    20 : Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21 : Result := toRetornoConfInstrucaoNaoProtestar;
    23 : Result := toRetornoEncaminhadoACartorio;
    32 : Result := toRetornoBaixaPorProtesto;
    35 : Result := toRetornoAlegacaoDoSacado;
    36 : Result := toRetornoCustasEdital;
    37 : Result := toRetornoCustasSustacaoJudicial;
    38 : Result := toRetornoSustadoJudicial;
    65 : Result := toRetornoChequePendenteCompensacao;
    69 : Result := toRetornoChequeDevolvido;
    71 : Result := toRetornoEstornoProtesto;
    72 : Result := toRetornoBaixaAutomatica;
    74 : Result := toRetornoConfirmacaoCancelamentoBaixaAutomatica;
    75 : Result := toRetornoLiquidadoParcialmente;
    90 : Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    95 : Result := toRetornoAlteracaoUsoCedente;
    96 : Result := toRetornoTarifaExtratoPosicao;
    97 : Result := toRetornoCustasSustacao;
    98 : Result := toRetornoTarifaInstrucao;
    99 : Result := toRetornoCustasProtesto;
    else
      Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoPefisa.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado:
      Result := '02';
    toRetornoRegistroRecusado:
      Result := '03';
    toRetornoAlteracaoDadosNovaEntrada:  
      Result := '04';
    toRetornoAlteracaoDadosBaixa:  
      Result := '05';
    toRetornoLiquidado:
      Result := '06';
    toRetornoLiquidadoAposBaixaOuNaoRegistro:
      Result := '07';
    toRetornoLiquidadoEmCartorio:
      Result := '08';
    toRetornoBaixaSimples:
      Result := '09';
    toRetornoBaixadoViaArquivo:
      Result := '10';
    toRetornoAbatimentoConcedido:
      Result := '12';
    toRetornoAbatimentoCancelado:
      Result := '13';
    toRetornoVencimentoAlterado:
      Result := '14';
    toRetornoBaixaRejeitada:  
      Result := '15';
    toRetornoInstrucaoRejeitada:  
      Result := '16';
    toRetornoAlteracaoDadosRejeitados:
      Result := '17';
    toRetornoRecebimentoInstrucaoProtestar:
      Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto:
      Result := '20';
    toRetornoConfInstrucaoNaoProtestar:  
      Result := '21';   
    toRetornoEncaminhadoACartorio:
      Result := '23';
    toRetornoBaixaPorProtesto:  
      Result := '32';
    toRetornoAlegacaoDoSacado:
      Result := '35';
    toRetornoCustasEdital:  
      Result := '36';
    toRetornoCustasSustacaoJudicial:  
      Result := '37';
    toRetornoSustadoJudicial:  
      Result := '38';
    toRetornoChequePendenteCompensacao:  
      Result := '65';
    toRetornoChequeDevolvido:  
      Result := '69';
    toRetornoEstornoProtesto:  
      Result := '71';
    toRetornoBaixaAutomatica:  
      Result := '72';
    toRetornoConfirmacaoCancelamentoBaixaAutomatica:  
      Result := '74';
    toRetornoLiquidadoParcialmente:  
      Result := '75';
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente:  
      Result := '90';
    toRetornoAlteracaoUsoCedente:  
      Result := '95';
    toRetornoTarifaExtratoPosicao:  
      Result := '96';
    toRetornoCustasSustacao:  
      Result := '97';
    toRetornoTarifaInstrucao:  
      Result := '98';
    toRetornoCustasProtesto:  
      Result := '99';
    else
      Result := '02';
  end;
end;

function TACBrBancoPefisa.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroRecusado: //03
      case CodMotivo of
        9000:
          Result := '9000-Data vencimento menor que o prazo de aceitação do título';
        9001:
          Result := '9001-Sacado bloqueado por atraso';
        9002:
          Result := '9002-Registro opcional inválido';
        9003:
          Result := '9003-Cep sem praça de cobrança';
        9004:
          Result := '9004-Prazo insuficiente para cobrança do título';
        9005:
          Result := '9005-Campo numérico inválido';
        9006:
          Result := '9006-Campo texto inválido';
        9007:
          Result := '9007-Campo tipo data inválido';
        9008:
          Result := '9008-Caractere inválido';
        9009:
          Result := '9009-Cpf/Cnpj do sacado e emitente devem ser diferentes';
        9010:
          Result := '9010-Data vencimento menor que a data de emissão';
        9011:
          Result := '9011-Data emissão maior que a data atual';
        9012:
          Result := '9012-Uf sacado inválido';
        9013:
          Result := '9013-Uf emitente inválido';
        9014:
          Result := '9014-Campo obrigatório não preenchido';
        9015:
          Result := '9015-Cpf do sacado inválido';
        9016:
          Result := '9016-Cnpj do sacado inválido';
        9017:
          Result := '9017-O nome do sacado enviado não confere com o nome do sacado cadastrado no sistema para este Cnpj/Cpf';
        9018:
          Result := '9018-Tipo do sacado inválido';
        9019:
          Result := '9019-O sacado está bloqueado';
        9020:
          Result := '9020-O endereço do sacado esta com o tamanho esta maior que o permitido';
        9021:
          Result := '9021-Digito do nosso numero inválido';
        9022:
          Result := '9022-Não existe faixa cadastrada para o banco e a conta';
        9023:
          Result := '9023-O nosso numero esta fora da faixa cadastrada para o cedente';
        9081:
          Result := '9081-Prazo insuficiente para cobrança do título neste Cep';
        9084:
          Result := '9084-Seu número do registro opcional diferente da linha do registro do título';
        9085:
          Result := '9085-Data de vencimento do registro opcional diferente da linha do registro do título';
        9086:
          Result := '9086-Valor do título no vencimento do registro opcional diferente da linha do registro do título';
        9087:
          Result := '9087-Os títulos de carteira de cobrança direta só podem ser enviados para contas de cobrnaça direta. acao: confira a carteira e a conta cobrança que está sendo enviada/atribuida ao título';
        9089:
          Result := '9089-Código cmc7 invalido';
        9090:
          Result := '9090-Entrada - nosso número já está sendo utilizado para mesmo banco/conta';
        9091:
          Result := '9091-Cep do sacado não pertence ao estado da federação (Uf) informado';
        9092:
          Result := '9092-Tipo de multa inválido';
        9093:
          Result := '9093-Registro opcional de emitente inválido';
        9097:
          Result := '9097-O campo Nosso Número não foi informado ou não foi possivel identificar o titulo';
        9098:
          Result := '9098-Foi encontrado mais de um título para esse nosso número';
        9099:
          Result := '9099-Preencha o campo de "conta de cobrança" no cadastro de carteira por cedente';
        9100:
          Result := '9100-Título possui registro opcional de emitente e a sua configuração não permite envio de títulos de terceiros';
        9101:
          Result := '9101-Título possui emitente, porém seus dados não foram informados';
        9103:
          Result := '9103-Ja existe titulo em aberto cadastrado para este cedente/seu numero/data vencimento/valor e emitente';
        9104:
          Result := '9104-Impedido pela legislação vigente';
        9106:
          Result := '9106-Nosso numero nao informado';
        9232:
          Result := '9232-Sacado pertence a empresa do grupo (coligada)';
        9233:
          Result := '9233-Por solicitação da diretoria de crédito/comercial';
        9234:
          Result := '9234-Inexistência de relação com o cedente';
        9235:
          Result := '9235-Outros';
        9236:
          Result := '9236-Recusado - Outros Motivos';
        9240:
          Result := '9240-Data multa menor que data de vencimento do título';
        9250:
          Result := '9250-Tipo de autorização para recebimento de valor divergente inválido';
        9251:
          Result := '9251-Indicativo Tipo de valor ou percentual inválido';
        9252:
          Result := '9252-Quantidade de pagamento parcial inválido';
        9254:
          Result := '9254-Mínimo não aceito para o título';
        9255:
          Result := '9255-Máximo não aceito para o título';
        9052:
          Result := '9052-Data de desconto 2 inválida';
        9230:
          Result := '9230-Valor desconto 2 inválido';
        9258:
          Result := '9258-Data de desconto 3 inválida';
        9259:
          Result := '9259-Valor desconto 3 inválido';
        9260:
          Result := '9260-Mínimo é obrigatório quando informado o tipo valor ou percentual';
        9261:
          Result := '9261-Tipo de autorização de recebimento de valor divergente não permitio para tipo de título 31';
        9262:
          Result := '9262-Para especie de título diferente de fatura de cartão de crédito não é possível informar o tipo aceita qualquer valor com range mínimo e máximo  preenchido';
        9263:
          Result := '9263-Mínimo e Máximo tem que ser informado para o tipo de autorização de valor divergente igual a 2';
        9264:
          Result := '9264-Mínimo e Máximo não devem ser informados para o tipo de autorização de valor divergente igual a 3';
        9265:
          Result := '9265-Mínimo deve ser informado e Máximo não pode ser informado para o tipo de autorização de valor divergente igual a 4';
        9266:
          Result := '9266-Valor não permitido para tipo de título fatura de cartão de crédito';
        9267:
          Result := '9267-Não é permitido ter juros, multa, abatimento, desconto ou protesto tipo de título fatura de cartão de crédito';
        9999:
          Result := '9999-Cep do sacado inválido';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoLiquidado: //06
      case CodMotivo of
        9105:
          Result := '9105-Crédito retido';
        9210:
          Result := '9210-Liquidação em cheque';
        9216:
          Result := '9216-Liquidação no guichê de caixa em dinheiro';
        9217:
          Result := '9217-Liquidação em banco correspondente';
        9218:
          Result := '9218-Liquidação por compensação eletrônica';
        9219:
          Result := '9219-Liquidação por conta';
        9223:
          Result := '9223-Liquidação por STR';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoLiquidadoEmCartorio: //08
      case CodMotivo of
        9201:
          Result := '9201-Liquidação em cartório';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixaSimples: //09
      case CodMotivo of
        9202:
          Result := '9202-Baixa decurso prazo - banco';
        9237:
          Result := '9237-Baixa por outros motivos';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixadoViaArquivo: //10
      case CodMotivo of
        0000:
          Result := '0000-Baixa comandada cliente arquivo';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoAlteracaoDadosRejeitados: //17
      case CodMotivo of
        9104:
          Result := '9104-Impedido pela legislação vigente';
        9113:
          Result := '9113-Não permitimos troca de carteira no evento de Alteração de Outros Dados';
        9114:
          Result := '9114-Não permitimos troca de tipo titulo no evento de alteração de outros dados';
        9250:
          Result := '9250-Tipo de autorização para recebimento de valor divergente inválido';
        9251:
          Result := '9251-Indicativo Tipo de valor ou percentual inválido';
        9252:
          Result := '9252-Quantidade de pagamento parcial inválido';
        9253:
          Result := '9253-Quantidade de pagamento parcial inválido, somente é permitido um valor maior ou igual a quantidade de pagamentos já recebido';
        9254:
          Result := '9254-Mínimo não aceito para o título';
        9255:
          Result := '9255-Máximo não aceito para o título';
        9052:
          Result := '9052-Data de desconto 2 inválida';
        9230:
          Result := '9230-Valor desconto 2 inválido';
        9258:
          Result := '9258-Data de desconto 3 inválida';
        9259:
          Result := '9259-Valor desconto 3 inválido';
        9260:
          Result := '9260-Mínimo é obrigatório quando informado o tipo valor ou percentual';
        9261:
          Result := '9261-Tipo de autorização de recebimento de valor divergente não permitio para tipo de título 31';
        9262:
          Result := '9262-Para especie de título diferente de fatura de cartão de crédito não é possível informar o tipo aceita qualquer valor com range mínimo e máximo  preenchido';
        9263:
          Result := '9263-Mínimo e Máximo tem que ser informado para o tipo de autorização de valor divergente igual a 2';
        9264:
          Result := '9264-Mínimo e Máximo não devem ser informados para o tipo de autorização de valor divergente igual a 3';
        9265:
          Result := '9265-Mínimo deve ser informado e Máximo não pode ser informado para o tipo de autorização de valor divergente igual a 4';
        9266:
          Result := '9266-Valor não permitido para tipo de título fatura de cartão de crédito';
        9267:
          Result := '9267-Não é permitido ter juros, multa, abatimento, desconto ou protesto tipo de título fatura de cartão de crédito';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoBaixaPorProtesto: //32
      case CodMotivo of
        9203:
          Result := '9203-Baixa protestado';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoAlegacaoDoSacado: //35
      case CodMotivo of
        9238:
          Result := '9238-Pagador Rejeita Boleto';
        9239:
          Result := '9239-Pagador Aceita Boleto';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoCustasEdital: //36
      case CodMotivo of
        9207:
          Result := '9207-Custas de edital';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoCustasSustacaoJudicial: //37
      case CodMotivo of
        9208:
          Result := '9208-Custas de sustação de protesto';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoTituloSustadoJudicialmente: //38
      case CodMotivo of
        0000:
          Result := '0000-Título sustado judicialmente';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoLiquidadoParcialmente: //75
      case CodMotivo of
        9105:
          Result := '9105-Crédito retido';
        9216:
          Result := '9216-Liquidação no guichê de caixa em dinheiro';
        9217:
          Result := '9217-Liquidação em banco correspondente';
        9218:
          Result := '9218-Liquidação por compensação eletrônica';
        9219:
          Result := '9219-Liquidação por conta';
        9223:
          Result := '9223-Liquidação por STR';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente: //90
      case CodMotivo of
        9108:
          Result := '9108-Título pertence a uma espécie que não pode ser protestada';
        9109:
          Result := '9109-Protesto não permitido para título com moeda diferente de real';
        9110:
          Result := '9110-Cep do sacado não atendido pelos cartórios cadastrados';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoTarifaExtratoPosicao: //96
      case CodMotivo of
        9213:
          Result := '9213-Tarifa de manutenção de título vencido';
        9222:
          Result := '9222-Emissão extrato mov. carteira';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoDespesasSustacaoProtesto: //97
      case CodMotivo of
        9204:
          Result := '9204-Tarifa de sustacao de protesto';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoDespesasProtesto: //98
      case CodMotivo of
        9205:
          Result := '9205-Tarifa de protesto';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoCustasProtesto: //99
      case CodMotivo of
        9206:
          Result := '9206-Custas de protesto';
        else
          Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    else
      Result := '0000'; //IntToStrZero(CodMotivo,2) +' - Outros Motivos';
  end;

  if (Result = '0000') then
  begin
    case CodMotivo of
      9024:
        Result := '9024-Identificação do título inválida';
      9025:
        Result := '9025-Ocorrência não permitida pois o título esta baixado';
      9026:
        Result := '9026-Ocorrência não permitida pois o título esta liquidado';
      9027:
        Result := '9027-Ocorrência não permitida pois o título esta em protesto';
      9028:
        Result := '9028-Não é permitida alteração de vencimento para carteira de desconto';
      9029:
        Result := '9029-Situação do título inválida';
      9030:
        Result := '9030-Não foi possível conceder o abatimento';
      9031:
        Result := '9031-Não existe abatimento a ser cancelado';
      9032:
        Result := '9032-Não foi possível prorrogar a data de vencimento do título';
      9033:
        Result := '9033-Evento não permitido para situação do título';
      9034:
        Result := '9034-Evento não permitido para cheques';
      9035:
        Result := '9035-O código do registro esta diferente de 1';
      9036:
        Result := '9036-Agência inválida';
      9037:
        Result := '9037-Número da Conta Corrente para depósito Inválido';
      9038:
        Result := '9038-O Cnpj do cedente passado para o arquivo não confere com o Cnpj do cedente cadastrado para o arquivo';
      9040:
        Result := '9040-Cnpj do cedente não encontrado no cadastro';
      9041:
        Result := '9041-Tipo do emitente inválido';
      9042:
        Result := '9042-Cnpj do emitente inválido';
      9045:
        Result := '9045-Campo nosso numero deve ter um valor de, no máximo , 10 digitos quando a carteira de cobrança não é direta';
      9046:
        Result := '9046-No campo nosso número a identificação do título esta inválida';
      9047:
        Result := '9047-Banco e conta de cobrança direta não informados';
      9049:
        Result := '9049-Campo aceite enviado com valor nulo ou inválido';
      9050:
        Result := '9050-Data de emisão inválida';
      9051:
        Result := '9051-Data de vencimento inválida';
      9052:
        Result := '9052-Data de desconto inválida';
      9053:
        Result := '9053-Especie de titulo invalida';
      9054:
        Result := '9054-Especie de titulo não encontrada';
      9055:
        Result := '9055-Valor de título inválido';
      9056:
        Result := '9056-Prazo de cartorio invalido';
      9057:
        Result := '9057-Valor de abatimento inválido';
      9058:
        Result := '9058-Valor de desconto inválido';
      9059:
        Result := '9059-Código de ocorrência inválida ou inexistente';
      9060:
        Result := '9060-Tipo de mora inválido';
      9062:
        Result := '9062-Valor de juros ao dia inválido';
      9063:
        Result := '9063-A data de juros mora é anterior à data de vencimento. Favor verificar estes campos';
      9064:
        Result := '9064-A data de juros mora inválida';
      9065:
        Result := '9065-Número da sequência diferente do esperado';
      9066:
        Result := '9066-Número de sequencia inválido';
      9067:
        Result := '9067-Registro inválido';
      9068:
        Result := '9068-Cpf do emitente inválido';
      9070:
        Result := '9070-Nome do emitente inválido';
      9071:
        Result := '9071-Endereço do emitente inválido';
      9072:
        Result := '9072-Cidade do emitente inválida';
      9073:
        Result := '9073-Cep do emitente inválido';
      9074:
        Result := '9074-Este contrato não está cadastrado para o cedente';
      9075:
        Result := '9075-Não é permitida a entrada de títulos vencidos';
      9078:
        Result := '9078-Não existe endereço, uf e cidade para o título';
      9079:
        Result := '9079-Nosso número inválido';
      9083:
        Result := '9083-O cedente não pode enviar esse tipo de título com esta carteira';
      9107:
        Result := '9107-Tamanho máximo do nosso número para cobrança direta é 10 posições + digito(layout padrao matera/bradesco)';
      9224:
        Result := '9224-Carteira do Tipo G não pode inserir titulos';
    end;
  end;
  if Result = '0000' then
    Result := '';
end;

function TACBrBancoPefisa.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRemessaBaixar;                                              //Pedido de Baixa
                                                                
    04 : Result := toRemessaConcederAbatimento;                                 //Concessão de Abatimento
                                                                 
    05 : Result := toRemessaCancelarAbatimento;                                 //Cancelamento de Abatimento Concedido
                                                                  
    06 : Result := toRemessaAlterarVencimento;                                  //Alteração Vencimento
                                                                
    07 : Result := toRemessaAlterarUsoEmpresa;                                  //Troca Uso Empresa
                                                                  
    09 : Result := toRemessaProtestar;                                          //Pedido de Protesto
                                                                  
    18 : Result := toRemessaCancelarInstrucaoProtestoBaixa;                     //Sustar Protesto e Baixar o Tíulo
                                                                  
    19 : Result := toRemessaCancelarInstrucaoProtesto;                          //Sustar o Protesto e Manter em Carteira
                                                                  
    31 : Result := toRemessaOutrasAlteracoes;                                   //Alteração de Outros Dados
                                           
    else
      Result := toRemessaRegistrar;                                             //Remessa
  end;
end;

function TACBrBancoPefisa.TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
    toRemessaBaixar:
      Result := '02';                                                           //Pedido de Baixa
    toRemessaConcederAbatimento:
      Result := '04';                                                           //Concessão de Abatimento
    toRemessaCancelarAbatimento:
      Result := '05';                                                           //Cancelamento de Abatimento Concedido
    toRemessaAlterarVencimento:
      Result := '06';                                                           //Alteração Vencimento
    toRemessaAlterarUsoEmpresa:
      Result := '07';                                                           //Troca Uso Empresa
    toRemessaProtestar:
      Result := '09';                                                           //Pedido de Protesto
    toRemessaCancelarInstrucaoProtestoBaixa:
      Result := '18';                                                           //Sustar Protesto e Baixar o Tíulo
    toRemessaCancelarInstrucaoProtesto:
      Result := '19';                                                           //Sustar o Protesto e Manter em Carteira
    toRemessaOutrasAlteracoes:
      Result := '31';                                                           //Alteração de Outros Dados
    else
      Result := '01';                                                           //Remessa
  end;
end;

end.
