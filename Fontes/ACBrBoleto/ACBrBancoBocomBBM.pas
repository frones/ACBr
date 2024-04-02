{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Pandaaa                     }
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
//incluido em 20/03/2024

unit ACBrBancoBocomBBM;

interface

uses
  ACBrBoleto,
  ACBrBancoBradesco,
  ACBrBoletoConversao,
  Classes;

type

  { TACBrBancoBBM }

  TACBrBancoBocomBBM = class(TACBrBancoBradesco)
  private
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function ConverterMultaPercentual(const ACBrTitulo: TACBrTitulo): Integer;
  protected
    function MontaInstrucoesCNAB400(const ACBrTitulo :TACBrTitulo; const nRegistro: Integer ): String; override;
    function GerarLinhaRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList): String;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String; override;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList);override;
    Procedure LerRetorno240(ARetorno:TStringList);override;
  public
    constructor create(AOwner: TACBrBanco);
  end;

implementation

uses
  ACBrUtil.Base,
  SysUtils,
  StrUtils,
  ACBrUtil.Strings;

{ TACBrBancoBocomBBM }

constructor TACBrBancoBocomBBM.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpNumeroCorrespondente   := 107;
   fpNumero                 := 237;
   fpDensidadeGravacao      := '01600000';
   fpOrientacoesBanco.Clear;
   fpOrientacoesBanco.Add(ACBrStr('Crédito cedido ao BANCO BOCOM BBM S/A. Pagamento somente por este boleto.'));
end;

function TACBrBancoBocomBBM.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  LCodigoOcorrencia: Integer;
begin
  Result := '';
  LCodigoOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  case LCodigoOcorrencia of
    10: Result := '10-Baixado Conforme Instruções da Agência';
    15: Result := '15-Liquidação em Cartório';
    16: Result := '16-Titulo Pago em Cheque - Vinculado';
    18: Result := '18-Acerto de Depositária';
    21: Result := '21-Acerto do Controle do Participante';
    22: Result := '22-Titulo com Pagamento Cancelado';
    24: Result := '24-Entrada Rejeitada por CEP Irregular';
    25: Result := '25-Confirmação Recebimento Instrução de Protesto Falimentar';
    27: Result := '27-Baixa Rejeitada';
    32: Result := '32-Instrução Rejeitada';
    33: Result := '33-Confirmação Pedido Alteração Outros Dados';
    34: Result := '34-Retirado de Cartório e Manutenção Carteira';
    40: Result := '40-Estorno de Pagamento';
    55: Result := '55-Sustado Judicial';
    68: Result := '68-Acerto dos Dados do Rateio de Crédito';
    69: Result := '69-Cancelamento dos Dados do Rateio';
    74: Result := '74-Confirmação Pedido de Exclusão de Negatativação';
  end;

  if (Result <> '') then
  begin
    Result := ACBrSTr(Result);
    Exit;
  end;

  case LCodigoOcorrencia of
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    06: Result := '06-Liquidação Normal';
    09: Result := '09-Baixado Automaticamente via Arquivo';
    11: Result := '11-Em Ser - Arquivo de Títulos Pendentes';
    12: Result := '12-Abatimento Concedido';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    17: Result := '17-Liquidação após baixa ou Título não registrado';
    19: Result := '19-Confirmação Recebimento Instrução de Protesto';
    20: Result := '20-Confirmação Recebimento Instrução Sustação de Protesto';
    23: Result := '23-Entrada do Título em Cartório';
    28: Result := '28-Débito de tarifas/custas';
    29: Result := '29-Ocorrências do Pagador';
    30: Result := '30-Alteração de Outros Dados Rejeitados';
    35: Result := '35-Desagendamento do débito automático';
    73: Result := '73-Confirmação Recebimento Pedido de Negativação';
  end;

  Result := ACBrSTr(Result);
end;

function TACBrBancoBocomBBM.CodOcorrenciaToTipo(const CodOcorrencia: Integer ) : TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixadoViaArquivo;
    10: Result := toRetornoBaixadoInstAgencia;
    15: Result := toRetornoLiquidadoEmCartorio;
    17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    24: Result := toRetornoEntradaRejeitaCEPIrregular;
    27: Result := toRetornoBaixaRejeitada;
    28: Result := toRetornoDebitoTarifas;
    29: Result := toRetornoOcorrenciasdoSacado;
    30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
    32: Result := toRetornoInstrucaoRejeitada;
    35: Result := toRetornoDesagendamentoDebitoAutomatico;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoBocomBBM.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  case TipoOcorrencia of
    toRetornoRegistroConfirmado                  : Result := '02';
    toRetornoRegistroRecusado                    : Result := '03';
    toRetornoLiquidado                           : Result := '06';
    toRetornoBaixadoViaArquivo                   : Result := '09';
    toRetornoBaixadoInstAgencia                  : Result := '10';
    toRetornoLiquidadoEmCartorio                 : Result := '15';
    toRetornoLiquidadoAposBaixaOuNaoRegistro     : Result := '17';
    toRetornoEntradaRejeitaCEPIrregular          : Result := '24';
    toRetornoBaixaRejeitada                      : Result := '27';
    toRetornoDebitoTarifas                       : Result := '28';
    toRetornoOcorrenciasdoSacado                 : Result := '29';
    toRetornoAlteracaoOutrosDadosRejeitada       : Result := '30';
    toRetornoInstrucaoRejeitada                  : Result := '32';
    toRetornoDesagendamentoDebitoAutomatico      : Result := '35';
  end;
end;

function TACBrBancoBocomBBM.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String;
begin
   case TipoOcorrencia of
     toRetornoRegistroConfirmado:
        case StrToIntDef(CodMotivo,999) of
          00: Result := '00-Ocorrencia aceita';
          01: Result := '01-Codigo de banco inválido';
          04: Result := '04-Código do movimentacao nao permitido para a carteira';
          15: Result := '15-Caracteristicas de Cobranca Imcompativeis';
          17: Result := '17-Data de vencimento anterior a data de emissão';
          21: Result := '21-Espécie do Título inválido';
          24: Result := '24-Data da emissão inválida';
          38: Result := '38-Prazo para protesto inválido';
          39: Result := '39-Pedido para protesto não permitido para título';
          43: Result := '43-Prazo para baixa e devolução inválido';
          45: Result := '45-Nome do Pagador inválido';
          46: Result := '46-Tipo/num. de inscrição do Pagador inválidos';
          47: Result := '47-Endereço do Pagador não informado';
          48: Result := '48-CEP invalido';
          50: Result := '50-CEP referente a Banco correspondente';
          53: Result := '53-Nº de inscrição do Beneficiário Final inválidos (CPF/CNPJ)';
          54: Result := '54-Beneficiário Final não informado';
          67: Result := '67-Débito automático agendado';
          68: Result := '68-Débito não agendado - erro nos dados de remessa';
          69: Result := '69-Débito não agendado - Pagador não consta no cadastro de autorizante';
          70: Result := '70-Débito não agendado - Cedente não autorizado pelo Pagador';
          71: Result := '71-Débito não agendado - Cedente não participa da modalidade de débito automático';
          72: Result := '72-Débito não agendado - Código de moeda diferente de R$';
          73: Result := '73-Débito não agendado - Data de vencimento inválida';
          75: Result := '75-Débito não agendado - Tipo do número de inscrição do pagador debitado inválido';
          76: Result := '76-Pagador Eletrônico DDA (NOVO)- Esse motivo somente será disponibilizado no arquivo retorno para as empresas cadastradas nessa condição';
          86: Result := '86-Seu número do documento inválido';
          89: Result := '89-Email pagador nao enviado - Titulo com debito automatico';
          90: Result := '90-Email pagador nao enviado - Titulo com cobranca sem registro';
          else
            Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoRegistroRecusado:
        case StrToIntDef(CodMotivo,999) of
          02: Result:= '02-Codigo do registro detalhe invalido';
          03: Result:= '03-Codigo da Ocorrencia Invalida';
          04: Result:= '04-Codigo da Ocorrencia nao permitida para a carteira';
          05: Result:= '05-Codigo de Ocorrencia nao numerico';
          07: Result:= 'Agencia\Conta\Digito invalido';
          08: Result:= 'Nosso numero invalido';
          09: Result:= 'Nosso numero duplicado';
          10: Result:= 'Carteira invalida';
          13: Result:= 'Idetificacao da emissao do boleto invalida';
          16: Result:= 'Data de vencimento invalida';
          18: Result:= 'Vencimento fora do prazo de operacao';
          20: Result:= 'Valor do titulo invalido';
          21: Result:= 'Especie do titulo invalida';
          22: Result:= 'Especie nao permitida para a carteira';
          24: Result:= 'Data de emissao invalida';
          28: Result:= 'Codigo de desconto invalido';
          38: Result:= 'Prazo para protesto invalido';
          44: Result:= 'Agencia cedente nao prevista';
          45: Result:= 'Nome cedente nao informado';
          46: Result:= 'Tipo/numero inscricao pagador invalido';
          47: Result:= 'Endereco pagador nao informado';
          48: Result:= 'CEP invalido';
          50: Result:= 'CEP irregular - Banco correspondente';
          63: Result:= 'Entrada para titulo ja cadastrado';
          65: Result:= 'Limite excedido';
          66: Result:= 'Numero autorizacao inexistente';
          68: Result:= 'Debito nao agendado - Erro nos dados da remessa';
          69: Result:= 'Debito nao agendado - Pagador nao consta no cadastro de autorizante';
          70: Result:= 'Debito nao agendado - Cedente nao autorizado pelo pagador';
          71: Result:= 'Debito nao agendado - Cedente nao participa de debito automatico';
          72: Result:= 'Debito nao agendado - Codigo de moeda diferente de R$';
          73: Result:= 'Debito nao agendado - Data de vencimento invalida';
          74: Result:= 'Debito nao agendado - Conforme seu pedido titulo nao registrado';
          75: Result:= 'Debito nao agendado - Tipo de numero de inscricao de debitado invalido';
        else
           Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoLiquidado:
        case StrToIntDef(CodMotivo,999) of
           00: Result:= '00-Titulo pago com dinheiro';
           15: Result:= '15-Titulo pago com cheque';
           42: Result:= '42-Rateio não efetuado';
        else
           Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoBaixadoViaArquivo:
        case StrToIntDef(CodMotivo,999) of
           00: Result:= '00-Ocorrencia aceita';
           10: Result:= '10-Baixa comandada pelo cliente';
        else
           Result:= CodMotivo +' - Outros Motivos';
        end;
      toRetornoBaixadoInstAgencia:
        case StrToIntDef(CodMotivo,999) of
          00: Result:= '00-Baixado conforme instrucoes na agencia';
          14: Result:= '14-Titulo protestado';
          15: Result:= '15-Titulo excluido';
          16: Result:= '16-Titulo baixado pelo banco por decurso de prazo';
          20: Result:= '20-Titulo baixado e transferido para desconto';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;
     toRetornoLiquidadoEmCartorio:
        case StrToIntDef(CodMotivo,999) of
          00: Result:= '00-Pago com dinheiro';
          15: Result:= '15-Pago com cheque';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoLiquidadoAposBaixaouNaoRegistro:
        case StrToIntDef(CodMotivo,999) of
          00: Result:= '00-Pago com dinheiro';
          15: Result:= '15-Pago com cheque';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoEntradaRejeitaCEPIrregular:
        case StrToIntDef(CodMotivo,999) of
          48: Result:= '48-CEP invalido';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoBaixaRejeitada:
        case StrToIntDef(CodMotivo,999) of
          04: Result:= '04-Codigo de ocorrencia nao permitido para a carteira';
          07: Result:= '07-Agencia/Conta/Digito invalidos';
          08: Result:= '08-Nosso numero invalido';
          10: Result:= '10-Carteira invalida';
          15: Result:= '15-Carteira/Agencia/Conta/Nosso Numero invalidos';
          40: Result:= '40-Titulo com ordem de protesto emitido';
          42: Result:= '42-Codigo para baixa/devolucao via Telebradesco invalido';
          60: Result:= '60-Movimento para titulo nao cadastrado';
          77: Result:= '70-Transferencia para desconto nao permitido para a carteira';
          85: Result:= '85-Titulo com pagamento vinculado';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoDebitoTarifas:
        case StrToIntDef(CodMotivo,999) of
          02: Result:= '02-Tarifa de permanência título cadastrado';
          03: Result:= '03-Tarifa de sustação/Excl Negativação';
          04: Result:= '04-Tarifa de protesto/Incl Negativação';
          05: Result:= '05-Tarifa de outras instrucoes';
          06: Result:= '06-Tarifa de outras ocorrências';
          08: Result:= '08-Custas de protesto';
          12: Result:= '12-Tarifa de registro';
          13: Result:= '13-Tarifa titulo pago no Bradesco';
          14: Result:= '14-Tarifa titulo pago compensacao';
          15: Result:= '15-Tarifa título baixado não pago';
          16: Result:= '16-Tarifa alteracao de vencimento';
          17: Result:= '17-Tarifa concessão abatimento';
          18: Result:= '18-Tarifa cancelamento de abatimento';
          19: Result:= '19-Tarifa concessão desconto';
          20: Result:= '20-Tarifa cancelamento desconto';
          21: Result:= '21-Tarifa título pago cics';
          22: Result:= '22-Tarifa título pago Internet';
          23: Result:= '23-Tarifa título pago term. gerencial serviços';
          24: Result:= '24-Tarifa título pago Pág-Contas';
          25: Result:= '25-Tarifa título pago Fone Fácil';
          26: Result:= '26-Tarifa título Déb. Postagem';
          27: Result:= '27-Tarifa impressão de títulos pendentes';
          28: Result:= '28-Tarifa título pago BDN';
          29: Result:= '29-Tarifa título pago Term. Multi Funcao';
          30: Result:= '30-Impressão de títulos baixados';
          31: Result:= '31-Impressão de títulos pagos';
          32: Result:= '32-Tarifa título pago Pagfor';
          33: Result:= '33-Tarifa reg/pgto – guichê caixa';
          34: Result:= '34-Tarifa título pago retaguarda';
          35: Result:= '35-Tarifa título pago Subcentro';
          36: Result:= '36-Tarifa título pago Cartao de Credito';
          37: Result:= '37-Tarifa título pago Comp Eletrônica';
          38: Result:= '38-Tarifa título Baix. Pg. Cartorio';
          39: Result:='39-Tarifa título baixado acerto BCO';
          40: Result:='40-Baixa registro em duplicidade';
          41: Result:='41-Tarifa título baixado decurso prazo';
          42: Result:='42-Tarifa título baixado Judicialmente';
          43: Result:='43-Tarifa título baixado via remessa';
          44: Result:='44-Tarifa título baixado rastreamento';
          45: Result:='45-Tarifa título baixado conf. Pedido';
          46: Result:='46-Tarifa título baixado protestado';
          47: Result:='47-Tarifa título baixado p/ devolucao';
          48: Result:='48-Tarifa título baixado franco pagto';
          49: Result:='49-Tarifa título baixado SUST/RET/CARTÓRIO';
          50: Result:='50-Tarifa título baixado SUS/SEM/REM/CARTÓRIO';
          51: Result:='51-Tarifa título transferido desconto';
          52: Result:='52-Cobrado baixa manual';
          53: Result:='53-Baixa por acerto cliente';
          54: Result:='54-Tarifa baixa por contabilidade';
          55: Result:='55-Tarifa tentativa cons deb aut';
          56: Result:='56-Consulta informações via internet';
          57: Result:='57-Arquivo retorno via internet';
          58: Result:='58-Tarifa emissão Papeleta';
          59: Result:='59-Tarifa fornec papeleta semi preenchida';
          60: Result:='60-Acondicionador de papeletas (RPB)S';
          61: Result:='61-Acond. De papelatas (RPB)s PERSONAL';
          62: Result:='62-Papeleta formulário branco';
          63: Result:='63-Formulário A4 serrilhado';
          64: Result:='64-Fornecimento de softwares transmiss';
          65: Result:='65-Fornecimento de softwares consulta';
          66: Result:='66-Fornecimento Micro Completo';
          67: Result:='67-Fornecimento MODEN';
          68: Result:='68-Fornecimento de máquina FAX';
          69: Result:='69-Fornecimento de maquinas oticas';
          70: Result:='70-Fornecimento de Impressoras';
          71: Result:='71-Reativação de título';
          72: Result:='72-Alteração de produto negociado';
          73: Result:='73-Tarifa emissao de contra recibo';
          74: Result:='74-Tarifa emissao 2ª via papeleta';
          75: Result:='75-Tarifa regravação arquivo retorno';
          76: Result:='76-Arq. Títulos a vencer mensal';
          77: Result:='77-Listagem auxiliar de crédito';
          78: Result:='78-Tarifa cadastro cartela instrução permanente';
          79: Result:='79-Canalização de Crédito';
          80: Result:='80-Cadastro de Mensagem Fixa';
          81: Result:='81-Tarifa reapresentação automática título';
          82: Result:='82-Tarifa registro título déb. Automático';
          83: Result:='83-Tarifa Rateio de Crédito';
          84: Result:='84-Emissão papeleta sem valor';
          85: Result:='85-Sem uso';
          86: Result:='86-Cadastro de reembolso de diferença';
          87: Result:='87-Relatório fluxo de pagto';
          88: Result:='88-Emissão Extrato mov. Carteira';
          89: Result:='89-Mensagem campo local de pagto';
          90: Result:='90-Cadastro Concessionária serv. Publ.';
          91: Result:='91-Classif. Extrato Conta Corrente';
          92: Result:='92-Contabilidade especial';
          93: Result:='93-Realimentação pagto';
          94: Result:='94-Repasse de Créditos';
          95: Result:='95-Tarifa reg. pagto Banco Postal';
          96: Result:='96-Tarifa reg. Pagto outras mídias';
          97: Result:='97-Tarifa Reg/Pagto – Net Empresa';
          98: Result:='98-Tarifa título pago vencido';
          99: Result:='99-TR Tít. Baixado por decurso prazo';
          100: Result:='100-Arquivo Retorno Antecipado';
          101: Result:='101-Arq retorno Hora/Hora';
          102: Result:='102-TR. Agendamento Déb Aut';
          103: Result:='103-TR. Tentativa cons Déb Aut';
          104: Result:='104-TR Crédito on-line';
          105: Result:='105-TR. Agendamento rat. Crédito';
          106: Result:='106-TR Emissão aviso rateio';
          107: Result:='107-Extrato de protesto';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoOcorrenciasdoSacado:
        case StrToIntDef(CodMotivo,999) of
          78 : Result:= '78-Pagador alega que faturamento e indevido';
          116: Result:= '116-Pagador aceita/reconhece o faturamento';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoALteracaoOutrosDadosRejeitada:
        case StrToIntDef(CodMotivo,999) of
          01: Result:= '01-Código do Banco inválido';
          04: Result:= '04-Código de ocorrência não permitido para a carteira';
          05: Result:= '05-Código da ocorrência não numérico';
          08: Result:= '08-Nosso número inválido';
          15: Result:= '15-Característica da cobrança incompatível';
          16: Result:= '16-Data de vencimento inválido';
          17: Result:= '17-Data de vencimento anterior a data de emissão';
          18: Result:= '18-Vencimento fora do prazo de operação';
          24: Result:= '24-Data de emissão Inválida';
          26: Result:= '26-Código de juros de mora inválido';
          27: Result:= '27-Valor/taxa de juros de mora inválido';
          28: Result:= '28-Código de desconto inválido';
          29: Result:= '29-Valor do desconto maior/igual ao valor do Título';
          30: Result:= '30-Desconto a conceder não confere';
          31: Result:= '31-Concessão de desconto já existente ( Desconto anterior )';
          32: Result:= '32-Valor do IOF inválido';
          33: Result:= '33-Valor do abatimento inválido';
          34: Result:= '34-Valor do abatimento maior/igual ao valor do Título';
          38: Result:= '38-Prazo para protesto inválido';
          39: Result:= '39-Pedido de protesto não permitido para o Título';
          40: Result:= '40-Título com ordem de protesto emitido';
          42: Result:= '42-Código para baixa/devolução inválido';
          46: Result:= '46-Tipo/número de inscrição do pagador inválidos';
          48: Result:= '48-Cep Inválido';
          53: Result:= '53-Tipo/Número de inscrição do beneficiário final inválidos';
          54: Result:= '54-Beneficiário Final não informado';
          57: Result:= '57-Código da multa inválido';
          58: Result:= '58-Data da multa inválida';
          60: Result:= '60-Movimento para Título não cadastrado';
          79: Result:= '79-Data de Juros de mora Inválida';
          80: Result:= '80-Data do desconto inválida';
          85: Result:= '85-Título com Pagamento Vinculado.';
          88: Result:= '88-E-mail Pagador não lido no prazo 5 dias';
          91: Result:= '91-E-mail Pagador não recebido';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      (*Dados chave do boleto – não passíveis de alteração:
       Tipo de pessoa do beneficiário original, CPF ou CNPJ do beneficiário original,
       Nome ou razão social do beneficiário original, Tipo de pessoa do cliente pagador,
       CPF ou CNPJ do cliente pagador, Código da moeda, Identificação do nosso número,
       Data de emissão, Indicador de pagamento parcial.*)

      toRetornoComandoRecusado:
        case StrToIntDef(CodMotivo,999) of
          01 : Result:= '01-Código do Banco inválido';
          02 : Result:= '02-Código do registro detalhe inválido';
          04 : Result:= '04-Código de ocorrência não permitido para a carteira';
          05 : Result:= '05-Código de ocorrência não numérico';
          07 : Result:= '07-Agência/Conta/dígito inválidos';
          08 : Result:= '08-Nosso número inválido';
          10 : Result:= '10-Carteira inválida';
          15 : Result:= '15-Características da cobrança incompatíveis';
          16 : Result:= '16-Data de vencimento inválida';
          17 : Result:= '17-Data de vencimento anterior a data de emissão';
          18 : Result:= '18-Vencimento fora do prazo de operação';
          20 : Result:= '20-Valor do título inválido';
          21 : Result:= '21-Espécie do Título inválida';
          22 : Result:= '22-Espécie não permitida para a carteira';
          24 : Result:= '24-Data de emissão inválida';
          28 : Result:= '28-Código de desconto via Telebradesco inválido';
          29 : Result:= '29-Valor do desconto maior/igual ao valor do Título';
          30 : Result:= '30-Desconto a conceder não confere';
          31 : Result:= '31-Concessão de desconto - Já existe desconto anterior';
          33 : Result:= '33-Valor do abatimento inválido';
          34 : Result:= '34-Valor do abatimento maior/igual ao valor do Título';
          36 : Result:= '36-Concessão abatimento - Já existe abatimento anterior';
          38 : Result:= '38-Prazo para protesto inválido';
          39 : Result:= '39-Pedido de protesto não permitido para o Título';
          40 : Result:= '40-Título com ordem de protesto emitido';
          41 : Result:= '41-Pedido cancelamento/sustação para Título sem instrução de protesto';
          42 : Result:= '42-Código para baixa/devolução inválido';
          45 : Result:= '45-Nome do Pagador não informado';
          46 : Result:= '46-Tipo/número de inscrição do Pagador inválidos';
          47 : Result:= '47-Endereço do Pagador não informado';
          48 : Result:= '48-CEP Inválido';
          50 : Result:= '50-CEP referente a um Banco correspondente';
          53 : Result:= '53-Tipo de inscrição do Beneficiário Final inválidos';
          60 : Result:= '60-Movimento para Título não cadastrado';
          85 : Result:= '85-Título com pagamento vinculado';
          86 : Result:= '86-Seu número inválido';
          94 : Result:= '94-Título Penhorado – Instrução Não Liberada pela Agência';
          97 : Result:= '97-Instrução não permitida título negativado';
          98 : Result:= '98-Inclusão Bloqueada face a determinação Judicial';
          99 : Result:= '99-Telefone beneficiário não informado / inconsistente';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;

      toRetornoDesagendamentoDebitoAutomatico:
        case StrToIntDef(CodMotivo,999) of
          81 : Result:= '81-Tentativas esgotadas, baixado';
          82 : Result:= '82-Tentativas esgotadas, pendente';
          83 : Result:= '83-Cancelado pelo Pagador e Mantido Pendente, conforme negociação';
          84 : Result:= '84-Cancelado pelo Pagador e baixado, conforme negociação';
        else
          Result:= CodMotivo +' - Outros Motivos';
        end;
   else
     Result:= CodMotivo +' - Outros Motivos';
   end;

   Result := ACBrSTr(Result);
end;

function TACBrBancoBocomBBM.CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
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

function TACBrBancoBocomBBM.TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
    case TipoOcorrencia of
      toRemessaBaixar                         : Result := '02'; {Pedido de Baixa}
      toRemessaProtestoFinsFalimentares       : Result := '03'; {Pedido de Protesto Falimentar}
      toRemessaConcederAbatimento             : Result := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento             : Result := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento              : Result := '06'; {Alteração de vencimento}
      toRemessaAlterarControleParticipante    : Result := '07'; {Alteração do controle do participante}
      toRemessaAlterarNumeroControle          : Result := '08'; {Alteração de seu número}
      toRemessaProtestar                      : Result := '09'; {Pedido de protesto}
      toRemessaCancelarInstrucaoProtestoBaixa : Result := '18'; {Sustar protesto e baixar}
      toRemessaCancelarInstrucaoProtesto      : Result := '19'; {Sustar protesto e manter na carteira}
      toRemessaAlterarValorTitulo             : Result := '20'; {Alteração de valor}
      toRemessaTransferenciaCarteira          : Result := '23'; {Transferência entre carteiras}
      toRemessaDevTransferenciaCarteira       : Result := '24'; {Dev. Transferência entre carteiras}
      toRemessaOutrasOcorrencias              : Result := '31'; {Alteração de Outros Dados}
      else
        Result := '01';                                           {Remessa}
    end;
end;

function TACBrBancoBocomBBM.ConverterMultaPercentual(const ACBrTitulo: TACBrTitulo): Integer;
var LValor : Double;
begin
  if ACBrTitulo.MultaValorFixo then
  begin
    if (ACBrTitulo.ValorDocumento > 0) then
      LValor := (ACBrTitulo.PercentualMulta / ACBrTitulo.ValorDocumento) * 100
    else
      LValor := 0;
  end else
    LValor := ACBrTitulo.PercentualMulta;
  Result := Round(LValor * 100);
end;

function TACBrBancoBocomBBM.MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo; const nRegistro: Integer): String;
var LNossoNumero,LDigitoNossoNumero : String;
begin
  Result := '';

  ValidaNossoNumeroResponsavel(LNossoNumero, LDigitoNossoNumero, ACBrTitulo);
  {Primeira instrução vai no registro tipo 1}
  if ACBrTitulo.Mensagem.Count <= 1 then
  begin
    Result := '';
    Exit;
  end;

  Result := '2'               +                                                                                                // 001-001 IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO
            Copy(PadRight(ACBrTitulo.Mensagem[1], 80, ' '), 1, 80);                                                            // 002-081 CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO

  if ACBrTitulo.Mensagem.Count >= 3 then
    Result := Result +
              Copy(PadRight(ACBrTitulo.Mensagem[2], 80, ' '), 1, 80)                                                           // 082-161 CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
  else
    Result := Result + PadRight('', 80, ' ');                                                                                  // 082-161 CONTEÚDO DO RESTANTE DAS LINHAS

  if ACBrTitulo.Mensagem.Count >= 4 then
    Result := Result +
              Copy(PadRight(ACBrTitulo.Mensagem[3], 80, ' '), 1, 80)                                                           // 162-241 CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
  else
    Result := Result + PadRight('', 80, ' ');                                                                                 // 162-241 CONTEÚDO DO RESTANTE DAS LINHAS

  if ACBrTitulo.Mensagem.Count >= 5 then
    Result := Result +
              Copy(PadRight(ACBrTitulo.Mensagem[4], 80, ' '), 1, 80)                                                          // 242-321 CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
  else
    Result := Result + PadRight('', 80, ' ');                                                                                 // 242-321 CONTEÚDO DO RESTANTE DAS LINHAS


  Result := Result
            + IfThen(ACBrTitulo.DataDesconto2 > 0,FormatDateTime( 'ddmmyy', ACBrTitulo.DataDesconto2),PadLeft('', 6, '0'))   // 322-327 Data limite para concessão de Desconto 2
            + IntToStrZero( round( ACBrTitulo.ValorDesconto2 * 100 ), 13)                                                    // 328-340 Valor do Desconto 2
            + IfThen(ACBrTitulo.DataDesconto3 > 0, FormatDateTime( 'ddmmyy', ACBrTitulo.DataDesconto3) ,PadLeft('', 6, '0')) // 341-346 Data limite para concessão de Desconto 3
            + IntToStrZero( round( ACBrTitulo.ValorDesconto3 * 100 ), 13)                                                    // 347-359 Valor do Desconto 3
            + space(7)                                                                                                       // 360-366 Filler
            + IntToStrZero(StrToIntDef(trim(ACBrTitulo.Carteira), 0), 3)                                                     // 367-369 Num. da Carteira
            + IntToStrZero(StrToIntDef(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia), 0), 5)                             // 370-374 Código da Agência Beneficiário
            + IntToStrZero(StrToIntDef(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Conta)  , 0), 7)                             // 375-381 Num. da Conta-Corrente
            + ACBrTitulo.ACBrBoleto.Cedente.ContaDigito                                                                      // 382-382 DAC C/C
            + LNossoNumero                                                                                                   // 383-393 Nosso Número
            + LDigitoNossoNumero                                                                                             // 394-394 DAC Nosso Número
            + IntToStrZero( nRegistro + 1, 6);                                                                               // 395-400 Num. Sequencial do Registro
end;

function TACBrBancoBocomBBM.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
begin
   Result:= ACBrTitulo.Carteira+'/'+ACBrTitulo.NossoNumero+'-'+CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoBocomBBM.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;

procedure TACBrBancoBocomBBM.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
var
  LLinha, LEmpresa: String;
  LDataArquivo: TDateTime;
begin

  LDataArquivo := ACBrBanco.ACBrBoleto.DataArquivo;

  if LDataArquivo = 0 then
    LDataArquivo := Now;

  if (ACBrBanco.ACBrBoleto.ListadeBoletos.Count > 0) and
     (ACBrBanco.ACBrBoleto.ListadeBoletos[0].Sacado.SacadoAvalista.NomeAvalista <> '') then
    LEmpresa := ACBrBanco.ACBrBoleto.ListadeBoletos[0].Sacado.SacadoAvalista.NomeAvalista
  else
    LEmpresa := ACBrBanco.ACBrBoleto.Cedente.Nome;

  LLinha:= '0'                                                                  +  //001 a 001  ID do Registro
           '1'                                                                  +  //002 a 002  1 - Remessa
           'REMESSA'                                                            +  //003 a 009  Literal de Remessa
           '01'                                                                 +  //010 a 011  Código do Tipo de Serviço
           PadRight('COBRANCA', 15)                                             +  //012 a 026  Literal do tipo de serviço
           PadLeft(ACBrBanco.ACBrBoleto.Cedente.CodigoCedente, 20, '0')         +  //027 a 046  Codigo da Empresa no Banco
           PadRight(LEmpresa, 30)                                               +  //047 a 076  Nome da Empresa - Como é FIDC precisa passar como Avalista, caso contrário irá sair o nome padrão do cedente
           IntToStrZero(fpNumero, 3)                                            +  //077 a 079  Código do Banco na CIP 237
           PadRight(fpNome, 15)                                                 +  //080 a 094  Nome do Banco Bradesco
           FormatDateTime('ddmmyy', LDataArquivo)                               +  //095 a 100  Data de geração do arquivo
           Space(08)                                                            +  //101 a 108  brancos
           PadLeft(fpCodParametroMovimento, 2 )                                 +  //109 a 110  Cód. Parâm. Movimento MX
           IntToStrZero(NumeroRemessa, 7)                                       +  //111 a 117  Nr. Sequencial de Remessa
           Space(277)                                                           +  //118 a 394  brancos
           IntToStrZero(1, 6);                                                     //395 a 400  Nr. Sequencial de Remessa

  ARemessa.Add(UpperCase(LLinha));
end;

function TACBrBancoBocomBBM.GerarLinhaRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList): String;
var
  LOcorrencia, LEspecie, LAgencia,
  LProtesto, LTipoInscricao, LConta, LDigitoConta,
  LCarteira, LLinha, LNossoNumero, LDigitoNossoNumero, LCondicaoEmissao, LChaveNFE,
  LSeuNumero, LNumeroDocumento  : String;
  LValorPercentualMulta: Integer;
  LBoleto : TACBrBoleto;
begin
   Result := '';
   ValidaNossoNumeroResponsavel(LNossoNumero, LDigitoNossoNumero, ACBrTitulo);

   LBoleto := ACBrTitulo.ACBrBoleto;

   LAgencia := IntToStrZero(StrToIntDef(OnlyNumber(LBoleto.Cedente.Agencia),0),5);
   LConta   := IntToStrZero(StrToIntDef(OnlyNumber(LBoleto.Cedente.Conta),0),7);
   LCarteira:= IntToStrZero(StrToIntDef(trim(ACBrTitulo.Carteira),0), 3);
   LDigitoConta := PadLeft(trim(LBoleto.Cedente.ContaDigito),1,'0');

   {Código da Ocorrencia Original do Titulo}
   LOcorrencia:= TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo);

   {Tipo de Boleto}
   LCondicaoEmissao := DefineTipoBoleto(ACBrTitulo);

   {Especie}
   LEspecie:= DefineEspecieDoc(ACBrTitulo);

   {Intruções}
   LProtesto:= InstrucoesProtesto(ACBrTitulo);

   {Tipo de Pagador}
   LTipoInscricao := DefineTipoSacado(ACBrTitulo);

   { Converte valor em moeda para percentual, pois o arquivo só permite % }
   LValorPercentualMulta := ConverterMultaPercentual(ACBrTitulo);

   {Chave da NFe Vinculada}
   if ACBrTitulo.ListaDadosNFe.Count > 0 then
     LChaveNFe := ACBrTitulo.ListaDadosNFe[0].ChaveNFe
   else
     LChaveNFe := '';

   LSeuNumero := IfThen(ACBrTitulo.SeuNumero = '',ACBrTitulo.NumeroDocumento,ACBrTitulo.SeuNumero);
   LNumeroDocumento := IfThen(ACBrTitulo.NumeroDocumento = '',ACBrTitulo.SeuNumero,ACBrTitulo.NumeroDocumento);

   LLinha:= '1'                                                                 +  // 001 a 001 - 1 Fixo
   Poem_Zeros('0', 19)                                                          +  // 002 a 020 - Dados Débito Automático (Opcional)
   '0'                                                                          +  // 021 a 021 - 0 Fixo
   LCarteira                                                                    +  // 022 a 024 - Carteira 009
   LAgencia                                                                     +  // 025 a 029 - Código da Agencia 02373 Sem DV
   LConta                                                                       +  // 030 a 036 - Código da Conta Sem DV
   LDigitoConta                                                                 +  // 037 a 037 - DV da Conta
   PadRight( LSeuNumero ,25,' ')                                                +  // 038 a 062 - Numero de Controle do Participante
   '237'                                                                        +  // 063 a 065 - Código do Banco
   IfThen( ACBrTitulo.PercentualMulta > 0, '2', '0')                            +  // 066 a 066 - Indica se exite Multa ou não
   IntToStrZero( LValorPercentualMulta , 4)                                     +  // 067 a 070 - Percentual de Multa formatado com 2 casas decimais
   LNossoNumero                                                                 +  // 071 a 081 - Número bancário da cobrança
   LDigitoNossoNumero                                                           +  // 072 a 082 - DV Número bancário da cobrança
   IntToStrZero( round( ACBrTitulo.ValorDescontoAntDia * 100), 10)              +  // 083 a 092 - Desconto Bonificação por dia
   LCondicaoEmissao                                                             +  // 093 a 093 - Tipo Boleto(Quem emite)
   'N'                                                                          +  // 094 a 094 - Fixo N
   Space(10)                                                                    +  // 095 a 104 - Brancos
   Space(1)                                                                     +  // 105 a 105 - Indicador de Rateio (Brancos)
   Space(1)                                                                     +  // 106 a 106 - Endereçamento de Débito Automatico (Brancos)
   Space(2)                                                                     +  // 107 a 108 - Quantidade de Pagamentos (Brancos)
   LOcorrencia                                                                  +  // 109 a 110 - Código da Ocorrência
   PadRight( LNumeroDocumento ,  10)                                            +  // 111 a 120 - Numero Documento
   FormatDateTime( 'ddmmyy', ACBrTitulo.Vencimento)                             +  // 121 a 126 - Data Vencimento
   IntToStrZero( Round( ACBrTitulo.ValorDocumento * 100 ), 13)                  +  // 127 a 139 - Valor do Titulo
   StringOfChar('0',3)                                                          +  // 140 a 142 - Banco encarregado pela cobrança (Zeros)
   StringOfChar('0',5)                                                          +  // 143 a 147 - Agencia depositária (Zeros)
   PadRight(LEspecie,2)                                                         +  // 148 a 149 - Especie do Titulo
   'N'                                                                          +  // 150 a 150 - Identificação (Fixo N)
   FormatDateTime( 'ddmmyy', ACBrTitulo.DataDocumento )                         +  // 151 a 156 - Data de Emissão
   LProtesto                                                                    +  // 157 a 160 - Intruções de Protesto
   IntToStrZero( round(ACBrTitulo.ValorMoraJuros * 100 ), 13)                   +  // 161 a 173 - Valor a ser cobrado por dia de atraso
   IfThen(ACBrTitulo.DataDesconto < EncodeDate(2000,01,01),'000000',
          FormatDateTime( 'ddmmyy', ACBrTitulo.DataDesconto))                   +  // 174 a 179 - Data limite para concessão desconto
   IntToStrZero( round( ACBrTitulo.ValorDesconto * 100 ), 13)                   +  // 180 a 192 - Valor Desconto
   IntToStrZero( round( ACBrTitulo.ValorIOF * 100 ), 13)                        +  // 193 a 205 - Valor IOF
   IntToStrZero( round( ACBrTitulo.ValorAbatimento * 100 ), 13)                 +  // 206 a 218 - Valor Abatimento
   LTipoInscricao                                                               +  // 219 a 220 - Tipo de Inscrição do Pagador
   PadLeft(OnlyNumber(ACBrTitulo.Sacado.CNPJCPF),14,'0')                        +  // 221 a 234 - CPF / CNPJ do Pagador (Antigo Sacado)
   PadRight( ACBrTitulo.Sacado.NomeSacado, 40, ' ')                             +  // 235 a 274 - Nome do Pagador
   PadRight(ACBrTitulo.Sacado.Logradouro
           + Space(1)
           + ACBrTitulo.Sacado.Numero
           + Space(1)
           + ACBrTitulo.Sacado.Complemento
           + Space(1)
           + ACBrTitulo.Sacado.Bairro, 40)                                      +  // 275 a 314 - Endereço completo do pagador
   PadRight( ACBrTitulo.Sacado.Mensagem, 12, ' ')                               +  // 315 a 326 - 1ª Mensagem
   PadRight( ACBrTitulo.Sacado.CEP, 8 )                                         +  // 327 a 334 - CEP
   PadLeft(OnlyNumber(ACBrTitulo.Sacado.SacadoAvalista.CNPJCPF), 15, '0')       +  // 335 a 349 - CNPJ do beneficiário final
   Space(2)                                                                     +  // 350 a 351 - Brancos
   PadRight(ACBrTitulo.Sacado.SacadoAvalista.NomeAvalista, 43)                  +  // 352 a 394 - Nome do beneficiário final
   IntToStrZero(aRemessa.Count + 1, 6)                                          +  // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
   LChaveNFe;                                                                     // 401 a 444 Chave NFe

   Result := UpperCase(LLinha);
end;

procedure TACBrBancoBocomBBM.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  LLinha, LNossoNumero, LDigitoNossoNumero : String;
begin
  aRemessa.Add(UpperCase(GerarLinhaRegistroTransacao400(ACBrTitulo, aRemessa)));
  LLinha := MontaInstrucoesCNAB400(ACBrTitulo, aRemessa.Count );

  if not(LLinha = EmptyStr) then
    aRemessa.Add(UpperCase(LLinha));

  if (ACBrTitulo.Sacado.SacadoAvalista.NomeAvalista <> '') then
  begin
    ValidaNossoNumeroResponsavel(LNossoNumero, LDigitoNossoNumero, ACBrTitulo);
    LLinha := '7'                                                                + // 001 a 001 - Identificação do registro detalhe (7)
    PadRight(Trim(ACBrTitulo.Sacado.SacadoAvalista.Logradouro + ' ' +
                  ACBrTitulo.Sacado.SacadoAvalista.Numero     + ' ' +
                  ACBrTitulo.Sacado.SacadoAvalista.Bairro)  , 45, ' ')           + // 002 a 046 - Endereço Beneficiario Final
    PadRight(OnlyNumber(ACBrTitulo.Sacado.CEP), 8, '0' )                         + // 047 a 054 - CEP + Sufixo do CEP
    PadRight(ACBrTitulo.Sacado.SacadoAvalista.Cidade, 20, ' ')                   + // 055 a 074 - Cidade
    PadRight(ACBrTitulo.Sacado.SacadoAvalista.UF, 2, ' ')                        + // 075 a 076 - UF
    PadRight('', 290, ' ')                                                       + // 077 a 366 - Reserva Filer
    PadLeft(ACBrTitulo.Carteira, 3, '0')                                         + // 367 a 369 - Carteira
    PadLeft(OnlyNumber(ACBrTitulo.ACBrBoleto.Cedente.Agencia), 5, '0')           + // 370 a 374 - Agência mantenedora da conta
    PadLeft(ACBrTitulo.ACBrBoleto.Cedente.Conta, 7, '0')                         + // 375 a 381 - Número da Conta Corrente
    Padleft(ACBrTitulo.ACBrBoleto.Cedente.ContaDigito, 1 , ' ')                  + // 382 a 382 - Dígito Verificador da Conta DAC
    PadLeft(LNossoNumero, 11, '0')                                               + // 383 a 393 - Nosso Número
    PadLeft(LDigitoNossoNumero ,1,' ')                                           + // 394 a 394 - Digito Nosso Número
    IntToStrZero( ARemessa.Count + 1, 6);                                          // 395 a 400 - Número sequencial do registro

    ARemessa.Add(UpperCase(LLinha));
  end;
end;
procedure TACBrBancoBocomBBM.LerRetorno240(ARetorno: TStringList);
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;

procedure TACBrBancoBocomBBM.LerRetorno400(ARetorno: TStringList);
var LNome : String;
begin
  LNome      := fpNome;
  fpNome     := 'BoComBBM';
  fpNumero   := 107;
  inherited;
  fpNome     := LNome;
  fpNumero   := 237;
end;

end.
