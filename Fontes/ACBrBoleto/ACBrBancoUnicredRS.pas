{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, José M S Junior                }
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

unit ACBrBancoUnicredRS;

interface

uses
  Classes, SysUtils, ACBrBoleto;

type

  { TACBrBancoUnicredRS }

  TACBrBancoUnicredRS = class(TACBrBancoClass)
  private
  protected

    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function DefineNumeroDocumentoModulo(const ACBrTitulo: TACBrTitulo): String; override;
    function ConverterDigitoModuloFinal(): String; override;
    function DefineTamanhoContaRemessa: Integer; override;

  public
    Constructor create(AOwner: TACBrBanco);

    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;

    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;
    function TipoOcorrenciaToCodRemessa(const ATipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses {$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils, ACBrUtil ;

{ TACBrBancoUnicredRS }

constructor TACBrBancoUnicredRS.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                 := 4;
   fpNome                   := 'UNICRED';
   fpNumero                 := 091;
   fpTamanhoMaximoNossoNum  := 10;
   fpTamanhoAgencia         := 4;
   fpTamanhoConta           := 7;
   fpTamanhoCarteira        := 3;
   fpCodParametroMovimento  := '113';
   fpModuloMultiplicadorInicial:= 0;
   fpModuloMultiplicadorFinal := 7;

end;

function TACBrBancoUnicredRS.DefineCampoLivreCodigoBarras(
  const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo.ACBrBoleto do
  begin
    Result := PadLeft(OnlyNumber(Cedente.Agencia), fpTamanhoAgencia, '0') +          { Campo Livre 1 - Agência }
              PadLeft(RightStr(Cedente.Conta + Cedente.ContaDigito, 10), 10, '0') +  { Campo Livre 2 - Conta }
              ACBrTitulo.NossoNumero +                                               { Campo Livre 3 - Nosso Núm }
              CalcularDigitoVerificador(ACBrTitulo);                                 { C/ Dígito }
  end;
end;

function TACBrBancoUnicredRS.DefineNumeroDocumentoModulo(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := copy(PadLeft( trim(ACBrTitulo.Carteira), 3, '0'), 2, 2)  + ACBrTitulo.NossoNumero;

end;

function TACBrBancoUnicredRS.ConverterDigitoModuloFinal(): String;
begin

  if Modulo.ModuloFinal = 1 then
     Result:= '0'
  else
     Result:= IntToStr(Modulo.DigitoFinal);

end;

function TACBrBancoUnicredRS.DefineTamanhoContaRemessa: Integer;
begin
  Result:= 12;
end;

function TACBrBancoUnicredRS.MontarCampoNossoNumero (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result := copy(PadLeft( trim(ACBrTitulo.Carteira), 3, '0'), 2, 2) + '/'+ ACBrTitulo.NossoNumero + '-'+ CalcularDigitoVerificador(ACBrTitulo);
end;

procedure TACBrBancoUnicredRS.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  sDigitoNossoNumero, sOcorrencia, sAgencia           : String;
  sProtesto, sTipoSacado, sConta                      : String;
  sCarteira, sLinha, sNossoNumero, sNumContrato       : String;
begin

  with ACBrTitulo do
  begin
   ValidaNossoNumeroResponsavel(sNossoNumero, sDigitoNossoNumero, ACBrTitulo);

    sAgencia      := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia), 0), 5);
    sConta        := IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta)  , 0), 7);
    sCarteira     := IntToStrZero(StrToIntDef(trim(Carteira), 0), 3);
    sNumContrato  := sAgencia + sConta + ACBrBoleto.Cedente.ContaDigito;

    {Pegando Código da Ocorrencia}
    sOcorrencia:= TipoOcorrenciaToCodRemessa(OcorrenciaOriginal.Tipo);

    {Pegando campo Intruções}
    sProtesto:= InstrucoesProtesto(ACBrTitulo);

    {Pegando Tipo de Sacado}
    sTipoSacado := DefineTipoSacado(ACBrTitulo);

    with ACBrBoleto do
    begin
       sLinha:= '1'                                                     +  { 001: ID Registro }
                sAgencia                                                +  { 002: Agência }
                Cedente.AgenciaDigito                                   +  { 007: Agência Dígito }
                PadLeft(sConta, DefineTamanhoContaRemessa, '0')         +  { 008: Conta Corrente }
                Cedente.ContaDigito                                     +  { 020: Conta Corrente Dígito }
                '0'                                                     +  { 021: Zero }
                sCarteira                                               +  { 022: Carteira }
                sNumContrato                                            +  { 025: Número Contrato }
                PadRight(SeuNumero, 25, ' ')                            +  { 038: Numero Controle do Participante }
                IntToStrZero(fpNumero, 3) + '00'                        +  { 063: Cód. do Banco = 091 }
                PadLeft(sNossoNumero + sDigitoNossoNumero, 15, '0')     +  { 068: Nosso Número }
                IntToStrZero(Round(ValorDescontoAntDia * 100), 10)      +  { 083: Desconto Bonificação por dia }
                '0' + Space(12) + '0' + Space(2)                        +  { 093: Zero | 094: Branco |
                                                                             095: 10 Brancos | 105: Branco |
                                                                             106: Zeros | 107: 2 Brancos }
                sOcorrencia                                             +  { 109: Ocorrência }
                PadRight(NumeroDocumento, 10)                           +  { 111: Número DOcumento }
                FormatDateTime( 'ddmmyy', Vencimento)                   +  { 121: Vencimento }
                IntToStrZero(Round(ValorDocumento * 100 ), 13)          +  { 127: Valor do Título }
                StringOfChar('0', 11)                                   +  { 140: Zeros }
                FormatDateTime('ddmmyy', DataDocumento)                 +  { 151: Data de Emissão }
                sProtesto                                               +  { 157: Protesto }
                IntToStrZero(Round(ValorMoraJuros * 100), 13)           +  { 161: Valor por dia de Atraso }
                IfThen(DataDesconto < 0, '000000',
                       FormatDateTime('ddmmyy', DataDesconto))         +  { 174: Data Limite Desconto }
                IntToStrZero(Round(ValorDesconto * 100), 13)           +  { 180: Valor Desconto }
                sNossoNumero + sDigitoNossoNumero + '00'               +  { 193: Nosso Número na Unicred }
                IntToStrZero(Round(ValorAbatimento * 100), 13)         +  { 206: Valor Abatimento }
                sTipoSacado                                            +  { 219: Tipo Inscrição Sacado }
                PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0')           +  { 221: Núm. Incrição Sacado }
                PadRight(Sacado.NomeSacado, 40, ' ')                   +  { 235: Nome do Sacado }
                PadRight(Sacado.Logradouro + ' ' + Sacado.Numero, 40)  +  { 275: Endereço do Sacado }
                PadRight(Sacado.Bairro, 12, ' ')                       +  { 315: Bairro do Sacado }
                PadRight(Sacado.CEP, 8, ' ')                           +  { 327: CEP do Sacado }
                PadRight(Sacado.Cidade, 20, ' ')                       +  { 335: Cidade do Sacado }
                PadRight(Sacado.UF, 2, ' ')                            +  { 355: UF Cidade do Sacado }
                Space(38)                                              +  { 357: Sacador/Avalista }
                IntToStrZero(aRemessa.Count + 1, 6);                      { 395: Núm Sequencial arquivo }

       aRemessa.Add(UpperCase(sLinha));

       sLinha := MontaInstrucoesCNAB400(ACBrTitulo, aRemessa.Count );
       if not(sLinha = EmptyStr) then
         aRemessa.Add(UpperCase(sLinha));

    end;
  end;
end;

function TACBrBancoUnicredRS.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  Result        := EmptyStr;
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  case CodOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação Normal';
    09: Result := '09-Baixado Automaticamente via Arquivo';
    10: Result := '10-Baixado conforme instruções da Agência';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    15: Result := '15-Liquidação em Cartório';
    19: Result := '19-Confirmação Recebimento Instrução de Protesto';
    20: Result := '20-Confirmação Recebimento Instrução Sustação de Protesto';
    21: Result := '21-Confirma Recebimento de Instrução de Não Protestar';
    24: Result := '24-Entrada rejeitada por CEP Irregular';
    27: Result := '27-Baixa Rejeitada';
    30: Result := '30-Alteração de Outros Dados Rejeitados';
    32: Result := '32-Instrução Rejeitada';
    33: Result := '33-Confirmação Pedido Alteração Outros Dados';
  end;
end;

function TACBrBancoUnicredRS.CodOcorrenciaToTipo(const CodOcorrencia:
   Integer ) : TACBrTipoOcorrencia;
begin

  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixadoViaArquivo;
    10: Result := toRetornoBaixadoInstAgencia;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoLiquidadoEmCartorio;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoAcertoControleParticipante;
    24: Result := toRetornoEntradaRejeitaCEPIrregular;
    27: Result := toRetornoBaixaRejeitada;
    30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
    32: Result := toRetornoInstrucaoRejeitada;
    33: Result := toRetornoRecebimentoInstrucaoAlterarDados;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoUnicredRS.TipoOCorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                             : Result := '02';
    toRetornoRegistroRecusado                               : Result := '03';
    toRetornoLiquidado                                      : Result := '06';
    toRetornoBaixadoViaArquivo                              : Result := '09';
    toRetornoBaixadoInstAgencia                             : Result := '10';
    toRetornoAbatimentoConcedido                            : Result := '12';
    toRetornoAbatimentoCancelado                            : Result := '13';
    toRetornoVencimentoAlterado                             : Result := '14';
    toRetornoLiquidadoEmCartorio                            : Result := '15';
    toRetornoRecebimentoInstrucaoProtestar                  : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto             : Result := '20';
    toRetornoAcertoControleParticipante                     : Result := '21';
    toRetornoEntradaRejeitaCEPIrregular                     : Result := '24';
    toRetornoBaixaRejeitada                                 : Result := '27';
    toRetornoAlteracaoOutrosDadosRejeitada                  : Result := '30';
    toRetornoInstrucaoRejeitada                             : Result := '32';
    toRetornoRecebimentoInstrucaoAlterarDados               : Result := '33';
  else
    Result := '02';
  end;
end;

function TACBrBancoUnicredRS.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
   case TipoOcorrencia of
     toRetornoRegistroConfirmado:
       case CodMotivo  of
         00: Result := '00-Ocorrencia aceita';
       else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
       end;
    toRetornoRegistroRecusado:
      case CodMotivo of
         02: Result := '02-Codigo do registro detalhe invalido';
         03: Result := '03-Codigo da Ocorrencia Invalida';
         04: Result := '04-Codigo da Ocorrencia nao permitida para a carteira';
         05: Result := '05-Codigo de Ocorrencia nao numerico';
         07: Result := '07-Agencia\Conta\Digito invalido';
         08: Result := '08-Nosso numero invalido';
         09: Result := '09-Nosso numero duplicado';
         10: Result := '10-Carteira invalida';
         16: Result := '16-Data de vencimento invalida';
         18: Result := '18-Vencimento fora do prazo de operacao';
         20: Result := '20-Valor do titulo invalido';
         21: Result := '21-Especie do titulo invalida';
         22: Result := '22-Especie nao permitida para a carteira';
         24: Result := '24-Data de emissao invalida';
         38: Result := '38-Prazo para protesto invalido';
         44: Result := '44-Agencia cedente nao prevista';
         45: Result := '45-Nome Sacado nao informado';
         46: Result := '46-Tipo/numero inscricao sacado invalido';
         47: Result := '47-Endereco sacado nao informado';
         48: Result := '48-CEP invalido';
         63: Result := '63-Entrada para titulo ja cadastrado';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoLiquidado:
      case CodMotivo of
         00: Result := '00-Titulo pago com dinheiro';
         15: Result := '15-Titulo pago com cheque';
         42: Result := '42-Rateio nao efetuado';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixadoViaArquivo:
      case CodMotivo of
         00: Result := '00-Ocorrencia aceita';
         10: Result := '10=Baixa comandada pelo cliente';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixadoInstAgencia:
         case CodMotivo of
            00: Result := '00-Baixado comandada';
         else
            Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
         end;
    toRetornoLiquidadoEmCartorio:
      case CodMotivo of
         00: Result := '00-Pago com dinheiro';
         15: Result := '15-Pago com cheque';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoEntradaRejeitaCEPIrregular:
      case CodMotivo of
         00: Result := '00-CEP invalido';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoBaixaRejeitada:
      case CodMotivo of
         04: Result := '04-Codigo de ocorrencia nao permitido para a carteira';
         07: Result := '07-Agencia\Conta\Digito invalidos';
         08: Result := '08-Nosso numero invalido';
         10: Result := '10-Carteira invalida';
         15: Result := '15-Carteira\Agencia\Conta\NossoNumero invalidos';
         40: Result := '40-Titulo com ordem de protesto emitido';
         60: Result := '60-Movimento para titulo nao cadastrado';
         77: Result := '70-Transferencia para desconto nao permitido para a carteira';
         85: Result := '85-Titulo com pagamento vinculado';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoALteracaoOutrosDadosRejeitada:
      case CodMotivo of
         01: Result := '01-Código do Banco inválido';
         04: Result := '04-Código de ocorrência não permitido para a carteira';
         05: Result := '05-Código da ocorrência não numérico';
         08: Result := '08-Nosso número inválido';
         15: Result := '15-Característica da cobrança incompatível';
         16: Result := '16-Data de vencimento inválido';
         17: Result := '17-Data de vencimento anterior a data de emissão';
         18: Result := '18-Vencimento fora do prazo de operação';
         24: Result := '24-Data de emissão Inválida';
         29: Result := '29-Valor do desconto maior/igual ao valor do Título';
         30: Result := '30-Desconto a conceder não confere';
         31: Result := '31-Concessão de desconto já existente ( Desconto anterior )';
         33: Result := '33-Valor do abatimento inválido';
         34: Result := '34-Valor do abatimento maior/igual ao valor do Título';
         38: Result := '38-Prazo para protesto inválido';
         39: Result := '39-Pedido de protesto não permitido para o Título';
         40: Result := '40-Título com ordem de protesto emitido';
         42: Result := '42-Código para baixa/devolução inválido';
         60: Result := '60-Movimento para Título não cadastrado';
         85: Result := '85-Título com Pagamento Vinculado.';
      else
         Result := IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoInstrucaoRejeitada:
      case CodMotivo of
         01 : Result := '01-Código do Banco inválido';
         02 : Result := '02-Código do registro detalhe inválido';
         04 : Result := '04-Código de ocorrência não permitido para a carteira';
         05 : Result := '05-Código de ocorrência não numérico';
         07 : Result := '07-Agência/Conta/dígito inválidos';
         08 : Result := '08-Nosso número inválido';
         10 : Result := '10-Carteira inválida';
         15 : Result := '15-Características da cobrança incompatíveis';
         16 : Result := '16-Data de vencimento inválida';
         17 : Result := '17-Data de vencimento anterior a data de emissão';
         18 : Result := '18-Vencimento fora do prazo de operação';
         20 : Result := '20-Valor do título inválido';
         21 : Result := '21-Espécie do Título inválida';
         22 : Result := '22-Espécie não permitida para a carteira';
         24 : Result := '24-Data de emissão inválida';
         28 : Result := '28-Código de desconto inválido';
         29 : Result := '29-Valor do desconto maior/igual ao valor do Título';
         30 : Result := '30-Desconto a conceder não confere';
         31 : Result := '31-Concessão de desconto - Já existe desconto anterior';
         33 : Result := '33-Valor do abatimento inválido';
         34 : Result := '34-Valor do abatimento maior/igual ao valor do Título';
         36 : Result := '36-Concessão abatimento - Já existe abatimento anterior';
         38 : Result := '38-Prazo para protesto inválido';
         39 : Result := '39-Pedido de protesto não permitido para o Título';
         40 : Result := '40-Título com ordem de protesto emitido';
         41 : Result := '41-Pedido cancelamento/sustação para Título sem instrução de protesto';
         42 : Result := '42-Código para baixa/devolução inválido';
         45 : Result := '45-Nome do Sacado não informado';
         46 : Result := '46-Tipo/número de inscrição do Sacado inválidos';
         47 : Result := '47-Endereço do Sacado não informado';
         48 : Result := '48-CEP Inválido';
         60 : Result := '60-Movimento para Título não cadastrado';
         85 : Result := '85-Título com pagamento vinculado';
         86 : Result := '86-Seu número inválido';
      else
         Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
    toRetornoDesagendamentoDebitoAutomatico:
         Result := IntToStrZero(CodMotivo,2) + ' - Outros Motivos';
   else
      Result := IntToStrZero(CodMotivo,2) + ' - Outros Motivos';
   end;
end;

function TACBrBancoUnicredRS.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    03 : Result:= toRemessaProtestoFinsFalimentares;        {Pedido de Protesto Falimentar}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração do controle do participante}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    19 : Result:= toRemessaCancelarInstrucaoProtesto;       {Sustar protesto e manter na carteira}
    22 : Result:= toRemessaTransfCessaoCreditoIDProd10;     {Transferência Cessão crédito ID. Prod.10}
    23 : Result:= toRemessaTransferenciaCarteira;           {Transferência entre Carteiras}
    24 : Result:= toRemessaDevTransferenciaCarteira;        {Dev. Transferência entre Carteiras}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
    68 : Result:= toRemessaAcertarRateioCredito;            {Acerto nos dados do rateio de Crédito}
    69 : Result:= toRemessaCancelarRateioCredito;           {Cancelamento do rateio de crédito.}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrBancoUnicredRS.TipoOcorrenciaToCodRemessa(
  const ATipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case ATipoOcorrencia of
         toRemessaBaixar                         : Result := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : Result := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : Result := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : Result := '06'; {Alteração de vencimento}
         toRemessaAlterarNumeroControle          : Result := '08'; {Alteração de seu número}
         toRemessaProtestar                      : Result := '09'; {Pedido de protesto}
         toRemessaNaoProtestar                   : Result := '10'; {Não Protestar}
         toRemessaCancelarInstrucaoProtestoBaixa : Result := '18'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : Result := '19'; {Sustar protesto e manter na carteira}
         toRemessaOutrasOcorrencias              : Result := '31'; {Alteração de Outros Dados}
         toRemessaPedidoNegativacao                  : Result := '45'; {Pedido de negativação}
         toRemessaExcluirNegativacaoBaixar           : Result := '46'; {Excluir negativação e baixar}
         toRemessaExcluirNegativacaoManterEmCarteira : Result := '47'; {Excluir negativação e manter em carteira.}
       else
         Result := '01';                                           {Remessa}
  end;

end;


end.


