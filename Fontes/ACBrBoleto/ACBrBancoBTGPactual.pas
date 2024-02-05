{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Daniel de Moraes, Victor H Gonzales - Panda,    }
{   Luiz Carlos Rodrigues                                                      }
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

unit ACBrBancoBTGPactual;

interface

uses
  Classes, SysUtils, Contnrs, ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrBancoBTGPactual }
  TACBrBancoBTGPactual = class(TACBrBancoClass)
  private
    fValorTotalDocs: Double;
  protected
    procedure EhObrigatorioContaDV; override;
    procedure EhObrigatorioAgenciaDV; override;
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function DefinePosicaoNossoNumeroRetorno: Integer; override;                     //Define posição para leitura de Retorno campo: NossoNumero
    function MontaInstrucoesCNAB240(ATitulo : TACBrTitulo; AIndex : Integer) : string;
    function ConverterDigitoModuloFinal(): String; override;
    function DefineDataOcorrencia(const ALinha: String): String; override;
  public
    constructor Create(AOwner: TACBrBanco);
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero( const ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa: TStringList): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
  end;

implementation

uses
  StrUtils,
  Variants,
  {$IFDEF COMPILER6_UP}
    DateUtils
  {$ELSE}
    ACBrD5,
    FileCtrl
  {$ENDIF},
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  Math;


function TACBrBancoBTGPactual.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado,  //02
    toRetornoRegistroRecusado,    //03
    toRetornoInstrucaoRejeitada,  //26
    toRetornoAlteracaoDadosRejeitados: //30
      case CodMotivo of
        01: Result := '01-Código do Banco Inválido';
        02: Result := '02-Código do Registro Detalhe Inválido';
        03: Result := '03-Código do Segmento Inválido';
        04: Result := '04-Código de Movimento Não Permitido para Carteira';
        05: Result := '05-Código de Movimento Inválido';
        06: Result := '06-Tipo/Número de Inscrição do Beneficiário Inválidos';
        07: Result := '07-Agência/Conta/DV Inválido';
        08: Result := '08-Nosso Número Inválido';
        09: Result := '09-Nosso Número Duplicado';
        10: Result := '10-Carteira Inválida';
        11: Result := '11-Forma de Cadastramento do Título Inválido';
        12: Result := '12-Tipo de Documento Inválido';
        13: Result := '13-Identificação da Emissão do Boleto de Pagamento Inválida';
        14: Result := '14-Identificação da Distribuição do Boleto de Pagamento Inválida';
        15: Result := '15-Características da Cobrança Incompatíveis';
        16: Result := '16-Data de Vencimento Inválida';
        17: Result := '17-Data de Vencimento Anterior a Data de Emissão';
        18: Result := '18-Vencimento Fora do Prazo de Operação';
        19: Result := '19-Título a Cargo de Bancos Correspondentes com Vencimento Inferior a XX Dias';
        20: Result := '20-Valor do Título Inválido';
        21: Result := '21-Espécie do Título Inválida';
        22: Result := '22-Espécie do Título Não Permitida para a Carteira';
        23: Result := '23-Aceite Inválido';
        24: Result := '24-Data da Emissão Inválida';
        25: Result := '25-Data da Emissão Posterior a Data de Entrada';
        26: Result := '26-Código de Juros de Mora Inválido';
        27: Result := '27-Valor/Taxa de Juros de Mora Inválido';
        28: Result := '28-Código do Desconto Inválido';
        29: Result := '29-Valor do Desconto Maior ou Igual ao Valor do Título';
        30: Result := '30-Desconto a Conceder Não Confere';
        31: Result := '31-Concessão de Desconto - Já Existe Desconto Anterior';
        32: Result := '32-Valor do IOF Inválido';
        33: Result := '33-Valor do Abatimento Inválido';
        34: Result := '34-Valor do Abatimento Maior ou Igual ao Valor do Título';
        35: Result := '35-Valor a Conceder Não Confere';
        36: Result := '36-Concessão de Abatimento - Já Existe Abatimento Anterior';
        37: Result := '37-Código para Protesto Inválido';
        38: Result := '38-Prazo para Protesto Inválido';
        39: Result := '39-Pedido de Protesto Não Permitido para o Título';
        40: Result := '40-Título com Ordem de Protesto Emitida';
        41: Result := '41-Pedido de Cancelamento/Sustação para Títulos sem Instrução de Protesto';
        42: Result := '42-Código para Baixa/Devolução Inválido';
        43: Result := '43-Prazo para Baixa/Devolução Inválido';
        44: Result := '44-Código da Moeda Inválido';
        45: Result := '45-Nome do Pagador Não Informado';
        46: Result := '46-Tipo/Número de Inscrição do Pagador Inválidos';
        47: Result := '47-Endereço do Pagador Não Informado';
        48: Result := '48-CEP Inválido';
        49: Result := '49-CEP Sem Praça de Cobrança (Não Localizado)';
        50: Result := '50-CEP Referente a um Banco Correspondente';
        51: Result := '51-CEP incompatível com a Unidade da Federação';
        52: Result := '52-Unidade da Federação Inválida';
        53: Result := '53-Tipo/Número de Inscrição do Sacador/Avalista Inválidos';
        54: Result := '54-Sacador/Avalista Não Informado';
        55: Result := '55-Nosso número no Banco Correspondente Não Informado';
        56: Result := '56-Código do Banco Correspondente Não Informado';
        57: Result := '57-Código da Multa Inválido';
        58: Result := '58-Data da Multa Inválida';
        59: Result := '59-Valor/Percentual da Multa Inválido';
        60: Result := '60-Movimento para Título Não Cadastrado';
        61: Result := '61-Alteração da Agência Cobradora/DV Inválida';
        62: Result := '62-Tipo de Impressão Inválido';
        63: Result := '63-Entrada para Título já Cadastrado';
        64: Result := '64-Número da Linha Inválido';
        65: Result := '65-Código do Banco para Débito Inválido';
        66: Result := '66-Agência/Conta/DV para Débito Inválido';
        67: Result := '67-Dados para Débito incompatível com a Identificação da Emissão do Boleto de Pagamento';
        68: Result := '68-Débito Automático Agendado';
        69: Result := '69-Débito Não Agendado - Erro nos Dados da Remessa';
        70: Result := '70-Débito Não Agendado - Pagador Não Consta do Cadastro de Autorizante';
        71: Result := '71-Débito Não Agendado - Beneficiário Não Autorizado pelo Pagador';
        72: Result := '72-Débito Não Agendado - Beneficiário Não Participa da Modalidade Débito';
        73: Result := '73-Débito Não Agendado - Código de Moeda Diferente de Real (R$)';
        74: Result := '74-Débito Não Agendado - Data Vencimento Inválida';
        75: Result := '75-Débito Não Agendado, Conforme seu Pedido, Título Não Registrado';
        76: Result := '76-Débito Não Agendado, Tipo/Num. Inscrição do Debitado, Inválido';
        77: Result := '77-Transferência para Desconto Não Permitida para a Carteira do Título';
        78: Result := '78-Data Inferior ou Igual ao Vencimento para Débito Automático';
        79: Result := '79-Data Juros de Mora Inválido';
        80: Result := '80-Data do Desconto Inválida';
        81: Result := '81-Tentativas de Débito Esgotadas - Baixado';
        82: Result := '82-Tentativas de Débito Esgotadas - Pendente';
        83: Result := '83-Limite Excedido';
        84: Result := '84-Número Autorização Inexistente';
        85: Result := '85-Título com Pagamento Vinculado';
        86: Result := '86-Seu Número Inválido';
        87: Result := '87--mail/SMS enviado';
        88: Result := '88--mail Lido';
        89: Result := '89--mail/SMS devolvido - endereço de e-mail ou número do celular incorreto ‘90’= e-mail devolvido - caixa postal cheia';
        91: Result := '91--mail/número do celular do Pagador não informado';
        92: Result := '92-agador optante por Boleto de Pagamento Eletrônico - e-mail não enviado';
        93: Result := '93-ódigo para emissão de Boleto de Pagamento não permite envio de e-mail';
        94: Result := '94-ódigo da Carteira inválido para envio e-mail.';
        95: Result := '95-ntrato não permite o envio de e-mail';
        96: Result := '96-úmero de contrato inválido';
        97: Result := '97-Rejeição da alteração do prazo limite de recebimento (a data deve ser informada no campo 28.3.p)';
        98: Result := '98-Rejeição de dispensa de prazo limite de recebimento';
        99: Result := '99-Rejeição da alteração do número do título dado pelo Beneficiário';
      else  Result := 'XX-Outros motivos';
      end;
    toRetornoDebitoTarifas: // 28 - Débito de Tarifas/Custas (Febraban 240 posições, v08.9 de 15/04/2014)
      case CodMotivo of
        01: Result := '01-Tarifa de Extrato de Posição';
        02: Result := '02-Tarifa de Manutenção de Título Vencido';
        03: Result := '03-Tarifa de Sustação';
        04: Result := '04-Tarifa de Protesto';
        05: Result := '05-Tarifa de Outras Instruções';
        06: Result := '06-Tarifa de Outras Ocorrências';
        07: Result := '07-Tarifa de Envio de Duplicata ao Sacado';
        08: Result := '08-Custas de Protesto';
        09: Result := '09-Custas de Sustação de Protesto';
        10: Result := '10-Custas de Cartório Distribuidor';
        11: Result := '11-Custas de Edital';
        12: Result := '12-Tarifa Sobre Devolução de Título Vencido';
        13: Result := '13-Tarifa Sobre Registro Cobrada na Baixa/Liquidação';
        14: Result := '14-Tarifa Sobre Reapresentação Automática';
        15: Result := '15-Tarifa Sobre Rateio de Crédito';
        16: Result := '16-Tarifa Sobre Informações Via Fax';
        17: Result := '17-Tarifa Sobre Prorrogação de Vencimento';
        18: Result := '18-Tarifa Sobre Alteração de Abatimento/Desconto';
        19: Result := '19-Tarifa Sobre Arquivo mensal (Em Ser)';
        20: Result := '20-Tarifa Sobre Emissão de Bloqueto Pré-Emitido pelo Banco';
      end;
    toRetornoLiquidado, // 06-Liquidação Normal
    toRetornoBaixaAutomatica, // 09-Baixa Automatica
    toRetornoLiquidadoSemRegistro: // 17-Liquidação Após Baixa ou Liquidação de Título Não Registrado
      case CodMotivo of
        01: Result := '01-Por Saldo';
        02: Result := '02-Por Conta';
        03: Result := '03-Liquidação no Guichê de Caixa em Dinheiro';
        04: Result := '04-Compensação Eletrônica';
        05: Result := '05-Compensação Convencional';
        06: Result := '06-Por Meio Eletrônico';
        07: Result := '07-Após Feriado Local';
        08: Result := '08-Em Cartório';
        09: Result := '09-Comandada Banco';
        10: Result := '10-Comandada Cliente Arquivo';
        11: Result := '11-Comandada Cliente On-line';
        12: Result := '12-Decurso Prazo - Cliente';
        13: Result := '13-Decurso Prazo - Banco';
        14: Result := '14-Protestado';
        15: Result := '15-Título Excluído';
        30: Result := '30-Liquidação no Guichê de Caixa em Cheque';
        31: Result := '31-Liquidação em banco correspondente';
        32: Result := '32-Liquidação Terminal de Auto-Atendimento';
        33: Result := '33-Liquidação na Internet (Home banking)';
        34: Result := '34-Liquidado Office Banking';
        35: Result := '35-Liquidado Correspondente em Dinheiro';
        36: Result := '36-Liquidado Correspondente em Cheque';
        37: Result := '37-Liquidado por meio de Central de Atendimento (Telefone)';
      end;
  end;
  Result := ACBrSTr(Result);
end;

function TACBrBancoBTGPactual.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;
  if CodOcorrencia>0 then
  begin
    case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      04: Result := toRetornoTransferenciaCarteiraEntrada;
      05: Result := toRetornoTransferenciaCarteiraBaixa;
      06: Result := toRetornoLiquidado;
      07: Result := toRetornoRecebimentoInstrucaoConcederDesconto;
      08: Result := toRetornoRecebimentoInstrucaoCancelarDesconto;
      09: Result := toRetornoBaixaAutomatica;
      11: Result := toRetornoTituloEmSer;
      12: Result := toRetornoAbatimentoConcedido;
      13: Result := toRetornoAbatimentoCancelado;
      14: Result := toRetornoVencimentoAlterado;
      15: Result := toRetornoBaixadoFrancoPagamento;
      17: Result := toRetornoLiquidadoSemRegistro;
      19: Result := toRetornoRecebimentoInstrucaoProtestar;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      23: Result := toRetornoEntradaEmCartorio;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoAlteracaoUsoCedente;
      28: Result := toRetornoDebitoTarifas;
      29: Result := toRetornoOcorrenciasDoSacado;
      30: Result := toRetornoAlteracaoDadosRejeitados;
      33: Result := toRetornoAcertoDadosRateioCredito;
      34: Result := toRetornoCancelamentoDadosRateio;
      35: Result := toRetornoDesagendamentoDebitoAutomatico;
      36: Result := toRetornoConfirmacaoEmailSMS;
      37: Result := toRetornoEmailSMSRejeitado;
      38: Result := toRetornoAlterarPrazoLimiteRecebimento;
      39: Result := toRetornoDispensarPrazoLimiteRecebimento;
      40: Result := toRetornoAlteracaoSeuNumero;
      41: Result := toRetornoAcertoControleParticipante;
      42: Result := toRetornoDadosAlterados;
      43: Result := toRetornoAlterarSacadorAvalista;
      44: Result := toRetornoChequeDevolvido;
      45: Result := toRetornoChequeCompensado;
      46: Result := toRetornoProtestoSustado;
      47: Result := toRetornoProtestoImediatoFalencia;
      48: Result := toRetornoConfInstrucaoTransferenciaCarteiraModalidadeCobranca;
      49: Result := toRetornoTipoCobrancaAlterado;
      50: Result := toRetornoChequePendenteCompensacao;
      51: Result := toRetornoTituloDDAReconhecidoPagador;
      52: Result := toRetornoTituloDDANaoReconhecidoPagador;
      53: Result := toRetornoTituloDDARecusadoCIP;
      54: Result := toRetornoBaixaTituloNegativadoSemProtesto;
      55: Result := toRetornoConfirmacaoPedidoDispensaMulta;
      56: Result := toRetornoConfirmacaoPedidoCobrancaMulta;
      57: Result := toRetornoRecebimentoInstrucaoAlterarJuros;
      58: Result := toRetornoAlterarDataDesconto;
      59: Result := toRetornoConfirmacaoPedidoAlteracaoBeneficiarioTitulo;
      60: Result := toRetornoRecebimentoInstrucaoDispensarJuros;
      61: Result := toRetornoConfirmacaoAlteracaoValorNominal;
      62: Result := toRetornoTituloSustadoJudicialmente;
      63: Result := toRetornoTituloSustadoJudicialmente;
      64: Result := toRetornoConfirmacaoAlteracaoValorMinimoOuPercentual;
      65: Result := toRetornoConfirmacaoAlteracaoValorMaximoOuPercentual;
    else
      Result := toRetornoOutrasOcorrencias;
    end;
  end;
end;

function TACBrBancoBTGPactual.ConverterDigitoModuloFinal: String;
begin
  if Modulo.ModuloFinal = 1 then
      Result:= 'P'
   else
      Result:= IntToStr(Modulo.DigitoFinal);
end;

constructor TACBrBancoBTGPactual.Create(AOwner: TACBrBanco);
begin
  inherited Create(AOwner);
  fpDigito                   := 1;
  fpNome                     := 'BTG Pactual';
  fpNumero                   := 208;
  fpTamanhoMaximoNossoNum    := 11;
  fpTamanhoNumeroDocumento   := 20;
  fpTamanhoAgencia           := 4;
  fpTamanhoConta             := 7;
  fpTamanhoCarteira          := 2;
  fValorTotalDocs            := 0;
  fpDensidadeGravacao        := '01600';
  fpModuloMultiplicadorFinal := 7;
  fpLayoutVersaoLote         := 060;
  fpLayoutVersaoArquivo      := 103;
end;

function TACBrBancoBTGPactual.DefineCampoLivreCodigoBarras(
  const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia                        // agencia sem digito
            + PadLeft(IntToStr(StrToInt(ACBrTitulo.Carteira)), 2,'0')    // codigo da carteira
            + ACBrTitulo.NossoNumero                                     // nosso número sem o digito
            + RightStr(ACBrTitulo.ACBrBoleto.Cedente.Conta,7)            // conta beneficiário sem digito
            + '0';                                                       // zero fixo
end;

function TACBrBancoBTGPactual.DefineDataOcorrencia(
  const ALinha: String): String;
begin
  Result := copy(ALinha, 158, 2)+'/'+copy(ALinha, 160, 2)+'/'+copy(ALinha, 162, 4);
end;

function TACBrBancoBTGPactual.DefinePosicaoNossoNumeroRetorno: Integer;
begin
  Result := 47
end;

procedure TACBrBancoBTGPactual.EhObrigatorioAgenciaDV;
begin
  //sem validação
end;

procedure TACBrBancoBTGPactual.EhObrigatorioContaDV;
begin
  //sem validação
end;

function TACBrBancoBTGPactual.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result:='';
  case TipoOcorrencia of   {C044 - CODIGO MOVTO RETORNO}
    toRetornoRegistroConfirmado:                                    Result := '02';
    toRetornoRegistroRecusado:                                      Result := '03';
    toRetornoTransferenciaCarteiraEntrada:                          Result := '04';
    toRetornoTransferenciaCarteiraBaixa:                            Result := '05';
    toRetornoLiquidado:                                             Result := '06';
    toRetornoRecebimentoInstrucaoConcederDesconto:                  Result := '07';
    toRetornoRecebimentoInstrucaoCancelarDesconto:                  Result := '08';
    toRetornoBaixaAutomatica:                                       Result := '09';
    toRetornoTituloEmSer:                                           Result := '11';
    toRetornoAbatimentoConcedido:                                   Result := '12';
    toRetornoAbatimentoCancelado:                                   Result := '13';
    toRetornoVencimentoAlterado:                                    Result := '14';
    toRetornoBaixadoFrancoPagamento:                                Result := '15';
    toRetornoLiquidadoSemRegistro:                                  Result := '17';
    toRetornoRecebimentoInstrucaoProtestar:                         Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto:                    Result := '20';
    toRetornoEntradaEmCartorio:                                     Result := '23';
    toRetornoRetiradoDeCartorio:                                    Result := '24';
    toRetornoBaixaPorProtesto:                                      Result := '25';
    toRetornoInstrucaoRejeitada:                                    Result := '26';
    toRetornoAlteracaoUsoCedente:                                   Result := '27';
    toRetornoDebitoTarifas:                                         Result := '28';
    toRetornoOcorrenciasDoSacado:                                   Result := '29';
    toRetornoAlteracaoDadosRejeitados:                              Result := '30';
    toRetornoAcertoDadosRateioCredito:                              Result := '33';
    toRetornoCancelamentoDadosRateio:                               Result := '34';
    toRetornoDesagendamentoDebitoAutomatico:                        Result := '35';
    toRetornoConfirmacaoEmailSMS:                                   Result := '36';
    toRetornoEmailSMSRejeitado:                                     Result := '37';
    toRetornoAlterarPrazoLimiteRecebimento:                         Result := '38';
    toRetornoDispensarPrazoLimiteRecebimento:                       Result := '39';
    toRetornoAlteracaoSeuNumero:                                    Result := '40';
    toRetornoAcertoControleParticipante:                            Result := '41';
    toRetornoDadosAlterados:                                        Result := '42';
    toRetornoAlterarSacadorAvalista:                                Result := '43';
    toRetornoChequeDevolvido:                                       Result := '44';
    toRetornoChequeCompensado:                                      Result := '45';
    toRetornoProtestoSustado:                                       Result := '46';
    toRetornoProtestoImediatoFalencia:                              Result := '47';
    toRetornoConfInstrucaoTransferenciaCarteiraModalidadeCobranca:  Result := '48';
    toRetornoTipoCobrancaAlterado:                                  Result := '49';
    toRetornoChequePendenteCompensacao:                             Result := '50';
    toRetornoTituloDDAReconhecidoPagador:                           Result := '51';
    toRetornoTituloDDANaoReconhecidoPagador:                        Result := '52';
    toRetornoTituloDDARecusadoCIP:                                  Result := '53';
    toRetornoBaixaTituloNegativadoSemProtesto:                      Result := '54';
    toRetornoConfirmacaoPedidoDispensaMulta:                        Result := '55';
    toRetornoConfirmacaoPedidoCobrancaMulta:                        Result := '56';
    toRetornoRecebimentoInstrucaoAlterarJuros:                      Result := '57';
    toRetornoAlterarDataDesconto:                                   Result := '58';
    toRetornoConfirmacaoPedidoAlteracaoBeneficiarioTitulo:          Result := '59';
    toRetornoRecebimentoInstrucaoDispensarJuros:                    Result := '60';
    toRetornoConfirmacaoAlteracaoValorNominal:                      Result := '61';
    toRetornoTituloSustadoJudicialmente:                            Result := '63';
    toRetornoConfirmacaoAlteracaoValorMinimoOuPercentual:           Result := '64';
    toRetornoConfirmacaoAlteracaoValorMaximoOuPercentual:           Result := '65';
  end;
end;

function TACBrBancoBTGPactual.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);
  case CodOcorrencia of
    02: Result:= '02 – Entrada confirmada';
    03: Result:= '03 – Entrada Rejeitada';
    04: Result:= '04 – Transferência de Carteira/Entrada';
    05: Result:= '05 – Transferência de Carteira/Baixa';
    06: Result:= '06 – Liquidação';
    09: Result:= '09 – Baixa';
    11: Result:= '11 – Títulos em Carteira (em ser)';
    12: Result:= '12 – Confirmação Recebimento Instrução de Abatimento';
    13: Result:= '13 – Confirmação Recebimento Instrução de Cancelamento Abatimento';
    14: Result:= '14 – Confirmação Recebimento Instrução Alteração de Vencimento';
    15: Result:= '15 – Franco de Pagamento';
    17: Result:= '17 – Liquidação Após Baixa ou Liquidação Título Não Registrado';
    19: Result:= '19 – Confirmação Recebimento Instrução de Protesto';
    20: Result:= '20 – Confirmação Recebimento Instrução de Sustação/Cancelamento de Protesto';
    23: Result:= '23 – Remessa a Cartório';
    24: Result:= '24 – Retirada de Cartório e Manutenção em Carteira';
    25: Result:= '25 – Protestado e Baixado';
    26: Result:= '26 – Instrução Rejeitada';
    27: Result:= '27 – Confirmação do Pedido de Alteração de Outros Dados';
    28: Result:= '28 – Débito de Tarifas/Custas';
    29: Result:= '29 – Ocorrências do Sacado';
    30: Result:= '30 – Alteração de Dados Rejeitada';
    44: Result:= '44 – Título pago com cheque devolvido';
    50: Result:= '50 – Título pago com cheque pendente de compensação';
  end;
  Result := ACBrSTr(Result);
end;

function TACBrBancoBTGPactual.MontaInstrucoesCNAB240(ATitulo: TACBrTitulo;
  AIndex: Integer): string;
begin

  if ATitulo.Mensagem.Count = 0 then
    Result := PadRight('', 80, ' ')
  else
  if (AIndex = 1) then
  begin
    if ATitulo.Mensagem.Count >= 1 then
         Result := Result + Copy(PadRight(ATitulo.Mensagem[0], 40, ' '), 1, 40) //Registro R mensagem 3
      else
         Result := Result + PadRight('', 40, ' ');

    if ATitulo.Mensagem.Count >= 2 then
         Result := Result + Copy(PadRight(ATitulo.Mensagem[1], 40, ' '), 1, 40) //Registro R mensagem 4
      else
         Result := Result + PadRight('', 40, ' ');
  end else
  begin
    if ATitulo.Mensagem.Count >= 3 then
         Result := Result + Copy(PadRight(ATitulo.Mensagem[2], 40, ' '), 1, 40) //Registro S mensagem 5
      else
         Result := Result + PadRight('', 40, ' ');

    if ATitulo.Mensagem.Count >= 4 then
         Result := Result + Copy(PadRight(ATitulo.Mensagem[3], 40, ' '), 1, 40) //Registro S mensagem 6
      else
         Result := Result + PadRight('', 40, ' ');

    if ATitulo.Mensagem.Count >= 5 then
         Result := Result + Copy(PadRight(ATitulo.Mensagem[4], 40, ' '), 1, 40) //Registro S mensagem 7
      else
         Result := Result + PadRight('', 40, ' ');

    if ATitulo.Mensagem.Count >= 6 then
         Result := Result + Copy(PadRight(ATitulo.Mensagem[5], 40, ' '), 1, 40) //Registro S mensagem 8
      else
         Result := Result + PadRight('', 40, ' ');

    if ATitulo.Mensagem.Count >= 7 then
         Result := Result + Copy(PadRight(ATitulo.Mensagem[6], 40, ' '), 1, 40) //Registro S mensagem 9
      else
         Result := Result + PadRight('', 40, ' ');
  end;

end;

function TACBrBancoBTGPactual.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin
  Result := RightStr(ACBrTitulo.ACBrBoleto.Cedente.Agencia,4)+'/'+
            ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

function TACBrBancoBTGPactual.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := PadLeft(IntToStr(StrToInt(ACBrTitulo.Carteira)), 3,'0')+'/' +
            ACBrTitulo.NossoNumero + '-' +
            CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoBTGPactual.GerarRegistroHeader240(NumeroRemessa: Integer): String;
var
  ATipoInscricao: string;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    case TipoInscricao of
      pFisica  : ATipoInscricao := '1';
      pJuridica: ATipoInscricao := '2';
    end;

    { GERAR REGISTRO-HEADER DO ARQUIVO }

    Result:= IntToStrZero(ACBrBanco.Numero, 3)           + // 001 a 003 Código do banco
             '0000'                                      + // 004 a 007 Lote de serviço
             '0'                                         + // 008 a 008 Tipo de registro - Registro header de arquivo
             Space(9)                                    + // 009 a 017 Uso exclusivo FEBRABAN/CNAB
             ATipoInscricao                              + // 018 a 018 Tipo de inscrição do cedente // fisica / jur
             PadLeft(OnlyNumber(CNPJCPF), 14, '0')       + // 019 a 032 Número de inscrição do cedente
             PadRight(Convenio, 20, ' ')                 + // 033 a 052 Código do convênio no banco
             PadLeft(Agencia, 5, '0')                    + // 053 a 057 Código da agência mantenedora da conta
             PadRight(AgenciaDigito,1,' ')               + // 058 a 058 Dígito da agência (Branco)
             PadLeft(OnlyNumber(Conta), 12, '0')         + // 059 a 070 Número da conta cosmos
             PadRight(ContaDigito,1,' ')                 + // 071 a 071 DV Conta
             space(1)                                    + // 072 a 072 Dígito verificador da ag/conta
             PadRight(Nome, 30)                          + // 073 a 102 Nome da empresa
             PadRight('BANCO BTG PACTUAL S.A.', 30, ' ') + // 103 a 132 Nome do banco
             Space(10)                                   + // 133 a 142 Uso exclusivo FEBRABAN/CNAB
             '1'                                         + // 143 a 143 Código de Remessa (1) / Retorno (2)
             FormatDateTime('ddmmyyyy', Now)             + // 144 a 151 Data do de geração do arquivo
             FormatDateTime('hhmmss', Now)               + // 152 a 157 Hora de geração do arquivo
             PadLeft(IntToStr(NumeroRemessa), 6, '0')    + // 158 a 163 Número seqüencial do arquivo
             PadLeft(IntToStr(LayoutVersaoArquivo), 3, '0') + // 164 a 166 Número da versão do layout do arquivo
             PadLeft(DensidadeGravacao,5,'0')            + // 167 a 171 Densidade de gravação do arquivo = "01600"
             Space(20)                                   + // 172 a 191 Para uso reservado do banco
             Space(20)                                   + // 192 a 211 Para uso reservado da empresa
             Space(29);                                    // 212 a 240 Uso exclusivo FEBRABAN/CNAB

    { GERAR REGISTRO HEADER DO LOTE }   {titulos em cobranca}

    Result:= Result                                      +
             #13#10                                      +
             IntToStrZero(ACBrBanco.Numero, 3)           + // 001 a 003 Código do banco
             '0001'                                      + // 004 a 007 Lote de serviço
             '1'                                         + // 008 a 008 Tipo de registro = "1" HEADER LOTE
             'R'                                         + // 009 a 009 Tipo de operação: R (Remessa) ou T (Retorno)
             '01'                                        + // 010 a 011 Tipo de serviço: 01 (Cobrança)
             Space(2)                                    + // 012 a 013 Uso exclusivo FEBRABAN/CNAB
             PadLeft(IntToStr(LayoutVersaoLote),3,'0')   + // 014 a 016 Número da versão do layout do lote
             Space(1)                                    + // 017 a 017 Uso exclusivo FEBRABAN/CNAB
             ATipoInscricao                              + // 018 a 018 Tipo de inscrição do cedente
             PadLeft(OnlyNumber(CNPJCPF), 15, '0')       + // 019 a 033 Número de inscrição do cedente
             PadRight(Convenio, 20, ' ')                 + // 034 a 053 Código do convênio no banco
             PadLeft(Agencia, 5, '0')                    + // 054 a 058 Código da agência mantenedora da conta (Zeros)
             PadLeft(AgenciaDigito, 1, ' ')              + // 059 a 059 Dígito da agência
             PadLeft(OnlyNumber(Conta), 12, '0')         + // 060 a 071 Número da conta cosmos
             PadLeft(ContaDigito, 1, ' ')                + // 072 a 072 DV Conta
             PadLeft(DigitoVerificadorAgenciaConta, 1, ' ') + // 073 a 073 Dígito verificador da agencia conta
             PadRight(Nome, 30, ' ')                     + // 074 a 103 Nome do cedente
             Space(40)                                   + // 104 a 143 Mensagem 1
             Space(40)                                   + // 144 a 183 Mensagem 2
             '00000000'                                  + // 184 a 191 Número remessa/retorno {pq zero}
             FormatDateTime('ddmmyyyy', Now)             + // 192 a 199 Data de gravação rem./ret.
             PadRight('', 8, '0')                        + // 200 a 207 Data do crédito - Só para arquivo retorno
             PadRight('', 33, ' ');                        // 208 a 240 Uso exclusivo FEBRABAN/CNAB
  end;
end;

procedure TACBrBancoBTGPactual.GerarRegistroHeader400(NumeroRemessa: Integer;
  aRemessa: TStringList);
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout CNAB400 deste banco.') );
end;

function TACBrBancoBTGPactual.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
  ATipoOcorrencia, ATipoBoleto, ATipoDistribuicao, ADataMoraJuros, CodProtesto, DiasProtesto: String;
  ACodigoDesconto, ADataDesconto, DigitoNossoNumero, ATipoAceite, AEspecieDoc, TipoSacado, EndSacado: String;
  ADiasBaixa, ACodigoCarteira, ATipoCarteira, ATipoDocumento: String;
  TipoAvalista: Char;
begin
  with ACBrTitulo do
  begin
    {Nosso Número}
    DigitoNossoNumero := CalcularDigitoVerificador(ACBrTitulo);

    {Aceite}
    case Aceite of
      atSim: ATipoAceite := 'A';
      atNao: ATipoAceite := 'N';
    end;

    {EspécieDoc}
    case AnsiIndexStr(UpperCase(EspecieDoc),['CH' ,'DM' ,'DMI','DS' ,'DSI','DR','LC','NCC','NCE',
                                             'NCI','NCR','NP' ,'NPR','TM' ,'TS','NS','RC' ,'FAT',
                                             'ND' ,'AP' ,'ME' ,'PC' ,'NF' ,'DD',
                                             'CPR','WAA','DAE','DAM','DAU','EC','CCC','DPD', {add casa}
                                             'BDA']) of
      0  : AEspecieDoc := '01';
      1  : AEspecieDoc := '02';
      2  : AEspecieDoc := '03';
      3  : AEspecieDoc := '04';
      4  : AEspecieDoc := '05';
      5  : AEspecieDoc := '06';
      6  : AEspecieDoc := '07';
      7  : AEspecieDoc := '08';
      8  : AEspecieDoc := '09';
      9  : AEspecieDoc := '10';
      10 : AEspecieDoc := '11';
      11 : AEspecieDoc := '12';
      12 : AEspecieDoc := '13';
      13 : AEspecieDoc := '14';
      14 : AEspecieDoc := '15';
      15 : AEspecieDoc := '16';
      16 : AEspecieDoc := '17';
      17 : AEspecieDoc := '18';
      18 : AEspecieDoc := '19';
      19 : AEspecieDoc := '20';
      20 : AEspecieDoc := '21';
      21 : AEspecieDoc := '22';
      22 : AEspecieDoc := '23';
      23 : AEspecieDoc := '24';
      24 : AEspecieDoc := '25'; { Cédula de Produto Rural}
      25 : AEspecieDoc := '26'; { Warrant}
      26 : AEspecieDoc := '27'; { Dívida Ativa de Estado}
      27 : AEspecieDoc := '28'; { Dívida Ativa de Município}
      28 : AEspecieDoc := '29'; { Dívida Ativa da União}
      29 : AEspecieDoc := '30'; { Encargos condominiais}
      30 : AEspecieDoc := '31'; { CC Cartão de Crédito}
      31 : AEspecieDoc := '32'; { BDP – Boleto de Proposta}
      32 : AEspecieDoc := '33'; { Boleto de Depósito e Aporte}
    else
      AEspecieDoc := '99';      { Outros}
    end;

    {Pegando o Tipo de Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar             : ATipoOcorrencia := '02';
      toRemessaConcederAbatimento : ATipoOcorrencia := '04';
      toRemessaAlterarVencimento  : ATipoOcorrencia := '06';
      toRemessaConcederDesconto   : ATipoOcorrencia := '07';
      toRemessaProtestar          : ATipoOcorrencia := '09';
      toRemessaSustarProtesto     : ATipoOcorrencia := '11';
    else
      ATipoOcorrencia := '01';
    end;

    {Mora Juros}
    if (ValorMoraJuros > 0) and (DataMoraJuros > 0) then
      ADataMoraJuros := FormatDateTime('ddmmyyyy', DataMoraJuros)
    else
      ADataMoraJuros := PadRight('', 8, '0');

    {Descontos}
    if (ValorDesconto > 0) and (DataDesconto > 0) then
    begin
      if TipoDesconto = tdPercentualAteDataInformada then
        ACodigoDesconto := '2'
      else
        ACodigoDesconto := '1';
      ADataDesconto := FormatDateTime('ddmmyyyy', DataDesconto);
    end
    else
    begin
      if ValorDesconto > 0 then
        ACodigoDesconto := '3'
      else
        ACodigoDesconto := '0';
      ADataDesconto := PadRight('', 8, '0');
    end;

    {Protesto}
    CodProtesto  := '3';
    DiasProtesto := '00';
    if (DataProtesto > 0) and (DataProtesto > Vencimento) then
    begin
      CodProtesto := '1';
      DiasProtesto := PadLeft(IntToStr(DaysBetween(DataProtesto, Vencimento)), 2, '0');
    end;

    // Nº Dias para Baixa/Devolucao
    ADiasBaixa  := '   ';
    if ((ATipoOcorrencia = '01') or
       (ATipoOcorrencia = '39')) and
       (Max(DataBaixa, DataLimitePagto) > Vencimento) then
       ADiasBaixa  := IntToStrZero(DaysBetween(Vencimento, Max(DataBaixa, DataLimitePagto)), 3);

    {Pegando Tipo de Boleto} //Quem emite e quem distribui o boleto?
    case ACBrBoleto.Cedente.ResponEmissao of
       tbBancoEmite : ATipoBoleto := '1';
       tbCliEmite   : ATipoBoleto := '2';
    else
      ATipoBoleto := '2';						 
    end;

    case ACBrBoleto.Cedente.IdentDistribuicao of
      tbBancoDistribui   : ATipoDistribuicao := '1';
      tbClienteDistribui : ATipoDistribuicao := '2';
    else
      ATipoDistribuicao := '2';							   
    end;

    {Sacado}
    case Sacado.Pessoa of
      pFisica:   TipoSacado := '1';
      pJuridica: TipoSacado := '2';
    else
      TipoSacado := '9';
    end;

    EndSacado := Sacado.Logradouro;
    if (Sacado.Numero <> '') then
      EndSacado := EndSacado + ', ' + Sacado.Numero;

    EndSacado := PadRight(trim(EndSacado), 40);

    {Avalista}
    if PadRight(Sacado.SacadoAvalista.CNPJCPF, 15, '0') = PadRight('0', 15, '0') then
      TipoAvalista := '0'
    else
      case Sacado.SacadoAvalista.Pessoa of
        pFisica:   TipoAvalista := '1';
        pJuridica: TipoAvalista := '2';
      else
        TipoAvalista := '9';
      end;

    {Codigo Carteira}
    case CaracTitulo of
      tcSimples            : ACodigoCarteira := '1'; {Cobrança Simples (Sem Registro e Eletrônica com Registro)}
      tcVinculada          : ACodigoCarteira := '2'; {Cobranca Vinculada}
      tcCaucionada         : ACodigoCarteira := '3'; {Cobrança Caucionada (Eletrônica com Registro e Convencional com Registro)}
      tcDescontada         : ACodigoCarteira := '4'; {Cobrança Descontada (Eletrônica com Registro)}
      tcVendor             : ACodigoCarteira := '5'; {Cobrança Vendor}
    else
      ACodigoCarteira := '1';
    end;

    {Tipo carteira}
    case ACBrBoleto.Cedente.TipoCarteira of
      tctRegistrada: ATipoCarteira := '1';
      tctSimples   : ATipoCarteira := '2';
    else
      ATipoCarteira := '1';
    end;

    {Tipo documento}
    case ACBrBoleto.Cedente.TipoDocumento of
      Tradicional: ATipoDocumento := '1';
      Escritural : ATipoDocumento := '2';
    else
      ATipoDocumento := '1';
    end;
	
    {SEGMENTO P}
    fValorTotalDocs := fValorTotalDocs  + ValorDocumento;
    Inc(fpQtdRegsLote);					   
    Result:= IntToStrZero(ACBrBanco.Numero, 3)                                      + // 001 a 003 Código do banco  {Ok}
             '0001'                                                                 + // 004 a 007 Lote de serviço {ok}
             '3'                                                                    + // 008 a 008 Tipo do registro: Registro detalhe
             IntToStrZero(fpQtdRegsLote, 5)                                          + // 009 a 013 Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)' {ok}
             'P'                                                                    + // 014 a 014 Código do segmento do registro detalhe {ok}
             Space(1)                                                               + // 015 a 015 Uso exclusivo FEBRABAN/CNAB: Branco ok
             ATipoOcorrencia                                                        + // 016 a 017 Código de movimento {2 digitos 01- entrada de titulos}
             PadLeft(ACBrBoleto.Cedente.Agencia, 5, '0')                            + // 018 a 022 Agência mantenedora da conta (Zeros)
             Space(1)                                                               + // 023 a 023 Dígito verificador da agência   {pq branco}
             PadLeft(OnlyNumber(ACBrBoleto.Cedente.Conta), 12, '0')                 + // 024 a 035 Número da conta cosmos {ok}
             PadRight(ACBrBoleto.Cedente.ContaDigito, 1, ' ')                       + // 036 a 036 Dígito verificador da conta {ok}
             Space(1)                                                               + // 037 a 037 Dígito verificador da ag/conta {?? pq branco}
             PadRight(NossoNumero, 20, ' ')                                         + // 038 a 057 Identificação do título no banco{OK}
             PadLeft(ACodigoCarteira, 1, ' ')                                        + // 058 a 058 Código da Carteira (característica dos títulos dentro das modalidades de cobrança: '1' = Cobrança Simples '3' = Cobrança Caucionada) {ok}
             PadLeft(ATipoCarteira, 1, ' ')                                          + // 059 a 059 Forma de cadastramento do título no banco (1=Cobrança Registrada, 2=Cobrança sem Registro) {ok}            Space(1)                                                               + // 060 a 060 Tipo de documento: Brancos   {pq fica em branco ? tradicional/escritual}
             PadLeft(ATipoDocumento, 1, ' ')                                         + // 060 a 060 Tipo de documento: Brancos   {tradicional/escritual}
             PadLeft(ATipoBoleto, 1, ' ')                                            + // 061 a 061 Quem emite / ident da emissao boleto  pgto
             PadLeft(ATipoDistribuicao, 1, ' ')                                      + // 062 a 062 Quem distribui  {ok}
             PadRight(NumeroDocumento, 15)                                          + // 063 a 077 Nº do documento de cobrança {ok}
             FormatDateTime('ddmmyyyy', Vencimento)                                 + // 078 a 085 Data de vencimento do título  {ok}
             IntToStrZero( Round( ValorDocumento * 100), 15)                        + // 086 a 100 Valor nominal do título {ok}
             '00000'                                                                + // 101 a 105 Agência cobradora. Se ficar em branco, o BTGPACTUAL determina automaticamente pelo CEP do sacado {ok}
             Space(1)                                                               + // 106 a 106 Dígito da agência cobradora {nao existe digitos??}
             PadRight(AEspecieDoc, 2)                                               + // 107 a 108 Espécie do documento {ok}
             ATipoAceite                                                            + // 109 a 109 Identificação de título Aceito / Não aceito {ok}
             FormatDateTime('ddmmyyyy', DataDocumento)                              + // 110 a 117 Data da emissão do documento {ok}
             PadLeft(trim(CodigoMora), 1)                                           + // 118 a 118 Código de mora (1=Valor diário; 2=Taxa Mensal; 3=Isento) {ok}
             ADataMoraJuros                                                         + // 119 a 126 Data a partir da qual serão cobrados juros {ok}
             IntToStrZero(round(ValorMoraJuros * 100), 15)                          + // 127 a 141 Valor de juros de mora por dia {ok}
             ACodigoDesconto                                                        + // 142 a 142 Código de desconto: 1 - Valor fixo até a data informada, 2 - Percentual desconto 4-Desconto por dia de antecipacao 0 - Sem desconto
             ADataDesconto                                                          + // 143 a 150 Data do desconto {ok}
             IntToStrZero(round(ValorDesconto * 100), 15)                           + // 151 a 165 Valor do desconto por dia {ok}
             IntToStrZero(Round(ValorIOF * 100), 15)                                + // 166 a 180 Valor do IOF a ser recolhido {ok}
             IntToStrZero(Round(ValorAbatimento * 100), 15)                         + // 181 a 195 Valor do abatimento {ok}
             PadRight(NumeroDocumento, 25)                                          + // 196 a 220 Identificação do título na empresa  {ok}
             CodProtesto                                                            + // 221 a 221 Código para protesto   {ok}
             DiasProtesto                                                           + // 222 a 223 Número de dias para protesto {ok}
             IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento), '1', '2')        + // 224 a 224 Código para baixa/devolução: Não baixar/não devolver
             ADiasBaixa                                                             + // 225 a 227 Brancos
             '09'                                                                   + // 228 a 229 Código da moeda: Real  {ok}
             PadRight('', 10 , '0')                                                 + // 230 a 239 Uso Exclusivo BTGPACTUAL  contrato {ok}
             Space(1);                                                                // 240 a 240 Uso exclusivo FEBRABAN/CNAB { ok}

    {SEGMENTO Q}
	Inc(fpQtdRegsLote);					   
    Result:= Result +  #13#10 +
             IntToStrZero(ACBrBanco.Numero, 3)                                          + // 001 a 003 Código do banco     {ok}
             '0001'                                                                     + // 004 a 007 Número do lote     {ok}
             '3'                                                                        + // 008 a 008 Tipo do registro: Registro detalhe  {ok}
             IntToStrZero(fpQtdRegsLote ,5)                                             + // 009 a 013 Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)  {ok}
             'Q'                                                                        + // 014 a 013 Código do segmento do registro detalhe  {ok}
             ' '                                                                        + // 015 a 015 Uso exclusivo FEBRABAN/CNAB: Branco  {ok}
             ATipoOcorrencia                                                            + // 016 a 017 Código de movimento  {ok}
             TipoSacado                                                                 + // 018 a 018 Tipo de inscrição  {ok}
             PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                               + // 019 a 033 Número de Inscrição  {ok}
             PadRight(Sacado.NomeSacado, 40, ' ')                                       + // 034 a 073 Nome sacado  {ok}
             PadRight(EndSacado, 40, ' ')                                               + // 074 a 113 Endereço  {ok}
             PadRight(Sacado.Bairro, 15, ' ')                                           + // 114 a 128 bairro sacado  {ok}
             Copy(PadLeft(OnlyNumber(Sacado.CEP),8,'0'),1,5)                            + // 129 a 133 CEP  {ok}
             Copy(PadLeft(OnlyNumber(Sacado.CEP),8,'0'),6,3)                            + // 134 a 136 Sufixo do CEP  {ok}
             PadRight(Sacado.Cidade, 15, ' ')                                           + // 137 a 151 cidade sacado  {ok}
             PadRight(Sacado.UF, 2, ' ')                                                + // 152 a 153 UF sacado  {ok}
             TipoAvalista                                                               + // 154 a 154 Tipo de inscrição sacador/avalista  {ok}
             PadRight(Sacado.SacadoAvalista.CNPJCPF, 15, '0')                           + // 155 a 169 Número de inscrição  {ok}
             PadRight(Sacado.SacadoAvalista.NomeAvalista,40,' ')                        + // 170 a 209 Nome do sacador/avalista  {ok}
             PadRight('', 3, '0')                                                       + // 210 a 212 Banco correspondente  {ok}
             PadRight('', 20, '0')                                                      + // 213 a 232 Nosso Número no banco correspondente  {ok}
             Space(8);                                                                    // 233 a 240 Uso exclusivo FEBRABAN/CNAB  {ok}
    if (PercentualMulta>0) or (Mensagem.Count>0) then
    begin
      {SEGMENTO R}
      Inc(fpQtdRegsLote);
      Result:= Result+ #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                          + // 001 - 003 / Código do Banco na compensação
               '0001'                                                     + // 004 - 007 / Numero do lote remessa
               '3'                                                        + // 008 - 008 / Tipo de registro
               IntToStrZero(fpQtdRegsLote, 5)                             + // 009 - 013 / Número seqüencial do registro no lote
               'R'                                                        + // 014 - 014 / Cód. Segmento do registro detalhe
               Space(1)                                                   + // 015 - 015 / Reservado (uso Banco)
               ATipoOcorrencia                                            + // 016 - 017 / Código de movimento remessa
               '0'                                                        + // 018 - 018 / Código do desconto 2
               PadLeft('', 8, '0')                                        + // 019 - 026 / Data do desconto 2
               IntToStrZero(0, 15)                                        + // 027 - 041 / Valor/Percentual a ser concedido 2
               '0'                                                        + // 042 - 042 / Código do desconto 3
               PadLeft('', 8, '0')                                        + // 043 - 050 / Data do desconto 3
               IntToStrZero(0, 15)                                        + // 051 - 065 / Valor/Percentual a ser concedido 3
               IfThen(PercentualMulta>0,
                 IfThen(MultaValorFixo,'1','2'),
                 '0')                                                     + // 066 - 066 1-Cobrar Multa Valor Fixo / 2-Percentual / 0-Não cobrar multa
               IfThen(PercentualMulta>0,
                 FormatDateTime('ddmmyyyy', DataMulta),
                 PadLeft('', 8, '0'))                                     + // 067 - 074 Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
               IntToStrZero(Round(PercentualMulta * 100), 15)             + // 075 - 089 / Valor/Percentual a ser aplicado
               Space(10)                                                  + // 090 - 099 / Reservado (uso Banco)
               MontaInstrucoesCNAB240(ACBrTitulo,1)                       + // 100 - 139 / Mensagem 3
                                                                            // 140 - 179 / Mensagem 4
               Space(61);                                                   // 180 - 240 / Reservado (uso Banco)
      {SEGMENTO R - FIM}
    end;

    if (Mensagem.Count > 2) then
    begin
      {SEGMENTO S}
      Inc(fpQtdRegsLote);
      Result:= Result+ #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)     + // 001 - 003 / Código do Banco na compensação
               '0001'                                           + // 004 - 007 / Numero do lote remessa
               '3'                                              + // 008 - 008 / Tipo de registro
               IntToStrZero(fpQtdRegsLote, 5)                   + // 009 - 013 / Número seqüencial do registro no lote
               'S'                                              + // 014 - 014 / Cód. Segmento do registro detalhe
               Space(1)                                         + // 015 - 015 / Reservado (uso Banco)
               ATipoOcorrencia                                  + // 016 - 017 / Código de movimento remessa
               '2'                                              + // 018 - 018 / Identificação da impressão
               MontaInstrucoesCNAB240(ACBrTitulo,2)             + // 019 - 058 / Mensagem 5
                                                                  // 059 - 098 / Mensagem 6
                                                                  // 099 - 138 / Mensagem 7
                                                                  // 139 - 178 / Mensagem 8
                                                                  // 179 - 218 / Mensagem 9
               Space(22);                                         // 219 - 240 / Reservado (uso Banco)
      {SEGMENTO S - FIM}
    end;
  end;
end;

procedure TACBrBancoBTGPactual.LerRetorno400(ARetorno: TStringList);
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout CNAB400 deste banco.') );
end;

function TACBrBancoBTGPactual.GerarRegistroTrailler240( ARemessa : TStringList ): String;
begin

  { REGISTRO TRAILER DO LOTE Pagina 68 - Unico Trailler com layout compativel }
  Result := IntToStrZero(ACBrBanco.Numero, 3)                         + // 001 - 003 Código do banco
           '0001'                                                     + // 004 - 007 Lote de Serviço
           '5'                                                        + // 008 - 008 Tipo do registro: Registro trailer do lote
           Space(9)                                                   + // 009 - 017 Uso exclusivo FEBRABAN/CNAB
           IntToStrZero((fpQtdRegsLote + 2 ), 6)                      + // 018 - 023 Quantidade de Registro no Lote (Registros P,Q header e trailer do lote)
           IntToStrZero((0), 6)                                       + // 024 - 029 Quantidade títulos em cobrança (CHEQUES)
           IntToStrZero(Round(fValorTotalDocs * 100), 17)             + // 030 - 046 Valor dos títulos em carteiras}
           PadRight('',06, '0')                                       + // 047 - 052 Quantidade títulos em cobrança
           PadRight('',17, '0')                                       + // 053 - 069 Valor dos títulos em carteiras}
           PadRight('',06, '0')                                       + // 070 - 075 Quantidade títulos em cobrança
           PadRight('',17, '0')                                       + // 076 - 092 Quantidade de Títulos em Carteiras
           PadRight('',06, '0')                                       + // 093 - 098 Quantidade títulos em cobrança
           PadRight('',17, '0')                                       + // 099 - 115 Quantidade de Títulos em Carteiras
           Space(8)                                                   + // 116 - 123 Número do aviso de lançamento
           Space(117);                                                  // 124 - 240 Uso exclusivo FEBRABAN/CNAB

  {GERAR REGISTRO TRAILER DO ARQUIVO - Pagina 18 }
  Result:= Result + #13#10 +
           IntToStrZero(ACBrBanco.Numero, 3)                          + // 001 - 003 Código do banco
           '9999'                                                     + // 004 - 007 Lote de serviço
           '9'                                                        + // 008 - 008 Tipo do registro: Registro trailer do arquivo
           PadRight('',9,' ')                                         + // 009 - 017 Uso exclusivo FEBRABAN/CNAB}
           '000001'                                                   + // 018 - 023 Quantidade de lotes do arquivo (Registros P,Q header e trailer do lote e do arquivo)
           IntToStrZero((fpQtdRegsLote + 4), 6)                       + // 024 - 029 Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
           PadRight('',006,'0')                                       + // 030 - 035 Uso exclusivo FEBRABAN/CNAB}
           PadRight('',205,' ');                                        // 240 - 205 Uso exclusivo FEBRABAN/CNAB}


  fValorTotalDocs := 0;
  fpQtdRegsLote   := 0;
end;
end.
