{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, André Ferreira de Moraes,      }
{ José M S Junior, Victor Hugo Gonzales - Panda                                }
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

unit ACBrBoleto;

interface
uses Classes, Graphics, Contnrs, IniFiles,
     {$IFDEF FPC}
       LResources,
     {$ENDIF}
     {$IfNDef MSWINDOWS}
       ACBrConsts,
     {$ENDIF}
     SysUtils, typinfo, Variants,
     ACBrBase, ACBrMail, ACBrValidador,
     ACBrDFeSSL, pcnConversao, ACBrBoletoConversao, ACBrBoletoRetorno,
     ACBrPIXBase, ACBrPIXBRCode;

const
  CInstrucaoPagamento = 'Pagar preferencialmente nas agencias do %s';
  CInstrucaoPagamentoLoterica = 'Preferencialmente nas Casas Lotéricas até o valor limite';
  CInstrucaoPagamentoRegistro = 'Pagavel em qualquer banco até o vencimento.';
  CInstrucaoPagamentoTodaRede = 'Pagavel em toda rede bancaria';
  CCedente    = 'CEDENTE';
  CBanco      = 'BANCO';
  CConta      = 'CONTA';
  CTitulo     = 'TITULO';
  CWebService = 'WEBSERVICE';
  cACBrTipoOcorrenciaDecricao: array[0..344] of String = (
    {Ocorrências para arquivo remessa}
    'Remessa Registrar',
    'Remessa Baixar',
    'Remessa Debitar Em Conta',
    'Remessa Conceder Abatimento',
    'Remessa Cancelar Abatimento',
    'Remessa Conceder Desconto',
    'Remessa Cancelar Desconto',
    'Remessa Alterar Vencimento',
    'Remessa Alterar Vencimento Sustar Protesto',
    'Remessa Protestar',
    'Remessa Sustar Protesto',
    'Remessa Cancelar Instrucao Protesto Baixa',
    'Remessa Cancelar Instrucao Protesto',
    'Remessa Dispensar Juros',
    'Remessa Alterar Nome Endereco Sacado',
    'Remessa Alterar Numero Controle',
    'Remessa Outras Ocorrencias',
    'Remessa Alterar Controle Participante',
    'Remessa Alterar SeuNumero',
    'Remessa Transf Cessao Credito ID Prod10',
    'Remessa Transferencia Carteira',
    'Remessa Dev Transferencia Carteira',
    'Remessa Desagendar Debito Automatico',
    'Remessa Acertar Rateio Credito',
    'Remessa Cancelar Rateio Credito',
    'Remessa Alterar Uso Empresa',
    'Remessa Nao Protestar',
    'Remessa Protesto Fins Falimentares',
    'Remessa Baixa Por Pagto Direto Cedente',
    'Remessa Cancelar Instrucao',
    'Remessa Alterar Venc Sustar Protesto',
    'Remessa Cedente Discorda Sacado',
    'Remessa Cedente Solicita Dispensa Juros',
    'Remessa Outras Alteracoes',
    'Remessa Alterar Modalidade',
    'Remessa Alterar Exclusivo Cliente',
    'Remessa Nao Cobrar Juros Mora',
    'Remessa Cobrar Juros Mora',
    'Remessa Alterar Valor Titulo',
    'Remessa Excluir Sacador Avalista',
    'Remessa Alterar Numero Dias Protesto',
    'Remessa Alterar Prazo Protesto',
    'Remessa Alterar Prazo Devolucao',
    'Remessa Alterar Outros Dados',
    'Remessa Alterar Dados Emissao Bloqueto',
    'Remessa Alterar Protesto Devolucao',
    'Remessa Alterar Devolucao Protesto',
    'Remessa Negativacao Serasa',
    'Remessa Excluir Negativacao Serasa',
    'Remessa Alterar Juros e Mora',
    'Remessa Alterar Valor/Percentual Multa',
    'Remessa Dispensar Cobrança de Multa',
    'Remessa Alterar Valor/Data de Desconto',
    'Remessa Não Conceder Desconto',
    'Remessa Alterar Valor de Abatimento',
    'Remessa Alterar Prazo Limite de Recebimento',
    'Remessa Dispensar Prazo Limite de Recebimento',
    'Remessa Alterar número do título do Beneficiário',
    'Remessa Alterar Dados Pagador',
    'Remessa Alterar dados Sacador/Avalista',
    'Remessa Recusa Alegação do Pagador',
    'Remessa Alterar Dados Rateio de Crédito',
    'Remessa Pedido de Cancelamento Dados do Rateio de Crédito',
    'Remessa Pedido de Desagendamento do Débito Automático',
    'Remessa Alterar Espécie de Título',
    'Remessa Contrato de Cobrança',
    'Remessa Negativação Sem Protesto',
    'Remessa Baixa Título Negativado Sem Protesto',
    'Remessa Alterar Valor Mínimo',
    'Remessa Alterar Valor Máximo',
    'Remessa Excluir Negativacao Serasa e Baixar',
    'Remessa Pedido de negativação',
    'Remessa Excluir negativação e baixar',
    'Remessa Excluir negativação e manter em carteira',
    'Remessa Sustar Protesto e baixar',
    'Remessa Sustar Protesto e manter em carteira',
    'Remessa Resusa Alegação do Sacado',
    'Remessa Protestar Automaticamente',
    'Remessa Alteração de Status Desconto',
    'Remessa Protestar Urgente',
    'Remessa Registrar Direta',
    'Remessa Alterar Numero Dias Baixa',
    'Remessa Alterar Valor Mínimo/Máximo',
    'Remessa Alteração Quantidade Parcelas',
    'Remessa Alteração Valor Nominal',
    'Remessa Não Baixar Automaticamente',
    'Remessa Alteração Percentual Para Mínimo',
    'Remessa Alteração Percentual Para Máximo',
    'Remessa Alteração Percentual Para Mínimo/Máximo',
    'Remessa Prorrogar Vencimento',
    'Remessa Boleto Hibrido',

    {Ocorrências para arquivo retorno}
    'Retorno Abatimento Cancelado',
    'Retorno Abatimento Concedido',
    'Retorno Acerto Controle Participante',
    'Retorno Acerto Dados Rateio Credito',
    'Retorno Acerto Depositaria',
    'Retorno Aguardando Autorizacao Protesto Edital',
    'Retorno Alegacao DoSacado',
    'Retorno Alteracao Dados Baixa',
    'Retorno Alteracao Dados Nova Entrada',
    'Retorno Alteracao Dados Rejeitados',
    'Retorno Alteracao Data Emissao',
    'Retorno Alteracao Especie',
    'Retorno Alteracao Instrucao',
    'Retorno Alteracao Opcao Devolucao Para Protesto Confirmada',
    'Retorno Alteracao Opcao Protesto Para Devolucao Confirmada',
    'Retorno Alteracao Outros Dados Rejeitada',
    'Retorno Alteracao Reemissao Bloqueto Confirmada',
    'Retorno Alteracao Seu Numero',
    'Retorno Alteracao Uso Cedente',
    'Retorno Alterar Data Desconto',
    'Retorno Alterar Prazo Limite Recebimento',
    'Retorno Alterar Sacador Avalista',
    'Retorno Baixa Automatica',
    'Retorno Baixa Credito CC Atraves Sispag',
    'Retorno Baixa Credito CC Atraves Sispag Sem Titulo Corresp',
    'Retorno Baixado',
    'Retorno Baixado FrancoPagamento',
    'Retorno Baixado InstAgencia',
    'Retorno Baixado Por Devolucao',
    'Retorno Baixado Via Arquivo',
    'Retorno Baixa Liquidado Edital',
    'Retorno Baixa Manual Confirmada',
    'Retorno Baixa Ou Liquidacao Estornada',
    'Retorno Baixa Por Protesto',
    'Retorno Baixa Por Ter Sido Liquidado',
    'Retorno Baixa Rejeitada',
    'Retorno Baixa Simples',
    'Retorno Baixa Solicitada',
    'Retorno Baixa Titulo Negativado Sem Protesto',
    'Retorno Baixa Transferencia Para Desconto',
    'Retorno Cancelamento Dados Rateio',
    'Retorno Cheque Compensado',
    'Retorno Cheque Devolvido',
    'Retorno Cheque Pendente Compensacao',
    'Retorno Cobranca Contratual',
    'Retorno Cobranca Creditar',
    'Retorno Comando Recusado',
    'Retorno Conf Cancelamento Negativacao Expressa Tarifa',
    'Retorno Conf Entrada Negativacao Expressa Tarifa',
    'Retorno Conf Exclusao Entrada Negativacao Expressa Por Liquidacao Tarifa',
    'Retorno Conf Instrucao Transferencia Carteira Modalidade Cobranca',
    'Retorno Confirmacao Alteracao Banco Sacado',
    'Retorno Confirmacao Alteracao Juros Mora',
    'Retorno Confirmacao Email SMS',
    'Retorno Confirmacao Entrada Cobranca Simples',
    'Retorno Confirmacao Exclusao Banco Sacado',
    'Retorno Confirmacao Inclusao Banco Sacado',
    'Retorno Confirmacao Pedido Excl Negativacao',
    'Retorno Confirmacao Receb Pedido Negativacao',
    'Retorno Confirma Recebimento Instrucao NaoNegativar',
    'Retorno Conf Recebimento Inst Cancelamento Negativacao Expressa',
    'Retorno Conf Recebimento Inst Entrada Negativacao Expressa',
    'Retorno Conf Recebimento Inst Exclusao Entrada Negativacao Expressa',
    'Retorno Custas Cartorio',
    'Retorno Custas Cartorio Distribuidor',
    'Retorno Custas Edital',
    'Retorno Custas Irregularidade',
    'Retorno Custas Protesto',
    'Retorno Custas Sustacao',
    'Retorno Custas Sustacao Judicial',
    'Retorno Dados Alterados',
    'Retorno Debito Custas Antecipadas',
    'Retorno Debito Direto Autorizado',
    'Retorno Debito Direto NaoAutorizado',
    'Retorno Debito Em Conta',
    'Retorno Debito Mensal Tarifa Aviso Movimentacao Titulos',
    'Retorno Debito Mensal Tarifas Extrado Posicao',
    'Retorno Debito Mensal Tarifas Manutencao Titulos Vencidos',
    'Retorno Debito Mensal Tarifas Outras Instrucoes',
    'Retorno Debito Mensal Tarifas Outras Ocorrencias',
    'Retorno Debito Mensal Tarifas Protestos',
    'Retorno Debito Mensal Tarifas SustacaoProtestos',
    'Retorno Debito Tarifas',
    'Retorno Desagendamento Debito Automatico',
    'Retorno Desconto Cancelado',
    'Retorno Desconto Concedido',
    'Retorno Desconto Retificado',
    'Retorno Despesa Cartorio',
    'Retorno Despesas Protesto',
    'Retorno Despesas Sustacao Protesto',
    'Retorno Devolvido Pelo Cartorio',
    'Retorno Dispensar Indexador',
    'Retorno Dispensar Prazo Limite Recebimento',
    'Retorno Email SMS Rejeitado',
    'Retorno Emissao Bloqueto Banco Sacado',
    'Retorno Encaminhado A Cartorio',
    'Retorno Endereco Sacado Alterado',
    'Retorno Entrada Bordero Manual',
    'Retorno Entrada Confirmada Rateio Credito',
    'Retorno Entrada Em Cartorio',
    'Retorno Entrada Registrada Aguardando Avaliacao',
    'Retorno Entrada Rejeita CEP Irregular',
    'Retorno Entrada Rejeitada Carne',
    'Retorno Entrada Titulo Banco Sacado Rejeitada',
    'Retorno Equalizacao Vendor',
    'Retorno Estorno Baixa Liquidacao',
    'Retorno Estorno Pagamento',
    'Retorno Estorno Protesto',
    'Retorno Instrucao Cancelada',
    'Retorno Instrucao Negativacao Expressa Rejeitada',
    'Retorno Instrucao Protesto Rejeitada Sustada Ou Pendente',
    'Retorno Instrucao Rejeitada',
    'Retorno IOF Invalido',
    'Retorno Juros Dispensados',
    'Retorno Liquidado',
    'Retorno Liquidado Apos Baixa Ou Nao Registro',
    'Retorno Liquidado Em Cartorio',
    'Retorno Liquidado Parcialmente',
    'Retorno Liquidado PorConta',
    'Retorno Liquidado Saldo Restante',
    'Retorno Liquidado Sem Registro',
    'Retorno Manutencao Banco Sacado Rejeitada',
    'Retorno Manutencao Sacado Rejeitada',
    'Retorno Manutencao Titulo Vencido',
    'Retorno Negativacao Expressa Informacional',
    'Retorno Nome Sacado Alterado',
    'Retorno Ocorrencias Do Sacado',
    'Retorno Outras Ocorrencias',
    'Retorno Outras Tarifas Alteracao',
    'Retorno Pagador DDA',
    'Retorno Prazo Devolucao Alterado',
    'Retorno Prazo Protesto Alterado',
    'Retorno Protestado',
    'Retorno Protesto Imediato Falencia',
    'Retorno Protesto Ou Sustacao Estornado',
    'Retorno Protesto Sustado',
    'Retorno Recebimento Instrucao Alterar Dados',
    'Retorno Recebimento Instrucao Alterar EnderecoSacado',
    'Retorno Recebimento Instrucao Alterar Juros',
    'Retorno Recebimento Instrucao Alterar NomeSacado',
    'Retorno Recebimento Instrucao Alterar Tipo Cobranca',
    'Retorno Recebimento Instrucao Alterar Valor Titulo',
    'Retorno Recebimento Instrucao Alterar Vencimento',
    'Retorno Recebimento Instrucao Baixar',
    'Retorno Recebimento Instrucao Cancelar Abatimento',
    'Retorno Recebimento Instrucao Cancelar Desconto',
    'Retorno Recebimento Instrucao Conceder Abatimento',
    'Retorno Recebimento Instrucao Conceder Desconto',
    'Retorno Recebimento Instrucao Dispensar Juros',
    'Retorno Recebimento Instrucao Nao Protestar',
    'Retorno Recebimento Instrucao Protestar',
    'Retorno Recebimento Instrucao Sustar Protesto',
    'Retorno Reembolso Devolucao Desconto Vendor',
    'Retorno Reembolso Nao Efetuado',
    'Retorno Reembolso Transferencia Desconto Vendor',
    'Retorno Registro Confirmado',
    'Retorno Registro Recusado',
    'Retorno Relacao De Titulos',
    'Retorno Remessa Rejeitada',
    'Retorno Reservado',
    'Retorno Retirado De Cartorio',
    'Retorno Segunda Via Instrumento Protesto',
    'Retorno Segunda Via Instrumento Protesto Cartorio',
    'Retorno Solicitacao Impressao Titulo Confirmada',
    'Retorno Sustacao Envio Cartorio',
    'Retorno Sustado Judicial',
    'Retorno Tarifa Aviso Cobranca',
    'Retorno Tarifa De Manutencao De Titulos Vencidos',
    'Retorno Tarifa De Relacao Das Liquidacoes',
    'Retorno Tarifa Email Cobranca Ativa Eletronica',
    'Retorno Tarifa Emissao Aviso Movimentacao Titulos',
    'Retorno Tarifa Emissao Boleto Envio Duplicata',
    'Retorno Tarifa Extrato Posicao',
    'Retorno Tarifa Instrucao',
    'Retorno Tarifa Mensal Baixas Bancos Corresp Carteira',
    'Retorno Tarifa Mensal Baixas Carteira',
    'Retorno Tarifa Mensal Cancelamento Negativacao Expressa',
    'Retorno Tarifa Mensal Email Cobranca AtivaEletronica',
    'Retorno Tarifa Mensal Emissao Boleto Envio Duplicata',
    'Retorno Tarifa Mensal Exclusao Entrada Negativacao Expressa',
    'Retorno Tarifa Mensal Exclusao Negativacao Expressa Por Liquidacao',
    'Retorno Tarifa Mensal Liquidacoes Bancos Corresp Carteira',
    'Retorno Tarifa Mensal Liquidacoes Carteira',
    'Retorno Tarifa Mensal Por Boleto Ate 03 Envio Cobranca Ativa Eletronica',
    'Retorno Tarifa Mensal Ref Entradas Bancos Corresp Carteira',
    'Retorno Tarifa Mensal SMS Cobranca Ativa Eletronica',
    'Retorno Tarifa Ocorrencias',
    'Retorno Tarifa Por Boleto Ate 03 Envio Cobranca Ativa Eletronica',
    'Retorno Tarifa SMS Cobranca Ativa Eletronica',
    'Retorno Tipo Cobranca Alterado',
    'Retorno Titulo DDA Nao Reconhecido Pagador',
    'Retorno Titulo DDA Reconhecido Pagador',
    'Retorno Titulo DDA Recusado CIP',
    'Retorno Titulo Em Ser',
    'Retorno Titulo Ja Baixado',
    'Retorno Titulo Nao Existe',
    'Retorno Titulo Pagamento Cancelado',
    'Retorno Titulo Pago Em Cheque',
    'Retorno Titulo Sustado Judicialmente',
    'Retorno Transferencia Carteira',
    'Retorno Transferencia Carteira Baixa',
    'Retorno Transferencia Carteira Entrada',
    'Retorno Transferencia Cedente',
    'Retorno Transito Pago Cartorio',
    'Retorno Vencimento Alterado',
    'Retorno Rejeicao Sacado',
    'Retorno Aceite Sacado',
    'Retorno Liquidado On Line',
    'Retorno Estorno Liquidacao OnLine',
    'Retorno Confirmacao Alteracao Valor Nominal',
    'Retorno Confirmacao Alteracao Valor Percentual Minimo Maximo',
    'Tipo Ocorrencia Nenhum',
    'Retorno Confirmação de Recebimento de Pedido de Negativação',
    'Retorno Confirmação de Recebimento de Pedido de Exclusão de Negativação',
    'Retorno Confirmação de Entrada de Negativação',
    'Retorno Entrada de Negativação Rejeitada',
    'Retorno Confirmação de Exclusão de Negativação',
    'Retorno Exlusão de Negativação Rejeitada',
    'Retorno Exclusão e Negativação por Outros Motivos',
    'Retorno Ocorrência Informacional por Outros Motivos',
    'Retorno Inclusão de Negativação',
    'Retorno Exclusão de Negativação',
    'Retorno Em Transito',
    'Retorno Liquidação em Condicional em Cartório Com Cheque do Próprio Devedor',
    'Retorno Título Protestado Sustado Judicialmente em definitivo',
    'Retorno Liquidação de Título Descontado',
    'Retorno Protesto Em Cartório',
    'Retorno Sustação Solicitada',
    'Retorno Título Utilizado Como Garantia em Operação de Desconto',
    'Retorno Título Descontável Com Desistência de Garantia em Operação de Desconto',
    'Retorno Intenção de Pagamento',
    'Retorno Entrada Confirmada na CIP',
    'Retorno Confirmação de alteração do valor mínimo/percentual',
    'Retorno Confirmação de alteração do valor máximo/percentual',
    'Retorno Confirmação de Pedido de Dispensa de Multa',
    'Retorno Confirmação do Pedido de Cobrança de Multa',
    'Retorno Confirmação do Pedido de Alteração do Beneficiário do Título',
    'Retorno Excluir Protesto Carta Anuencia',
    'Retorno Confirmação Cancelamento Baixa Automatica',
    'Retorno Confirmação Alteracao Dias Baixa Automatica',
    'Retorno Confirmação Instrucao Protesto',
    'Retorno Confirmação Instrucao Sustacao Protesto',
    'Retorno Confirmação Instrucao Nao Protestar',
    'Retorno Confirmação Instrucao Nao Baixar Automaticamente',
    'Retorno Alteracao Percentual Minimo',
    'Retorno Alteracao Percentual Maximo',
    'Retorno Alteracao Percentual Minimo Maximo',
    'Retorno Recebimento Instrucao Nao Baixar',
    'Retorno Confirmacao Protesto',
    'Retorno Confirmacao Sustacao',
    'Retorno Protesto Sustado Judicialmente',
    'Retorno Confirmação Instrucao Sustar Protesto',
    'Retorno Confirmação Instrucao Alteracao Dias Baixa Automatica',
    'Retorno Alteracao Quantidade Parcela'
);

type
  TACBrTipoCobranca =
   (cobNenhum,
    cobBancoDoBrasil,
    cobSantander,
    cobCaixaEconomica,
    cobCaixaSicob,
    cobBradesco,
    cobItau,
    cobBancoMercantil,
    cobSicred,
    cobBancoob,
    cobBanrisul,
    cobBanestes,
    cobHSBC,
    cobBancoDoNordeste,
    cobBRB,
    cobBicBanco,
    cobBradescoSICOOB,
    cobBancoSafra,
    cobSafraBradesco,
    cobBancoCECRED,
    cobBancoDaAmazonia,
    cobBancoDoBrasilSICOOB,
    cobUniprime,
    cobUnicredRS,
    cobBanese,
    cobCrediSIS,
    cobUnicredES,
    cobBancoCresolSCRS,
    cobCitiBank,
    cobBancoABCBrasil,
    cobDaycoval,
    cobUniprimeNortePR,
    cobBancoPine,
    cobBancoPineBradesco,
    cobUnicredSC,
    cobBancoAlfa,
    cobBancoDoBrasilAPI,
    cobBancoDoBrasilWS,
    cobBancoCresol,
    cobMoneyPlus,
    cobBancoC6,
    cobBancoRendimento,
    cobBancoInter,
    cobBancoSofisaSantander,
    cobBS2,
    cobPenseBankAPI,
    cobBTGPactual,
    cobBancoOriginal,
    cobBancoVotorantim,
    cobBancoPefisa,
    cobBancoFibra,
    cobBancoSofisaItau,
    cobBancoIndustrialBrasil,
    cobBancoAthenaBradesco
    );

  TACBrTitulo = class;
  TACBrBoletoFCClass = class;
  TACBrCedente = class;
  TACBrBanco  = class;
  TACBrBoleto = class;
  TConfiguracoes = class;

  //quando alterar, verificar a mensagem de impressão do boleto.
  TACBrTipoDesconto = (
    tdNaoConcederDesconto,
    tdValorFixoAteDataInformada,
    tdPercentualAteDataInformada,
    tdValorAntecipacaoDiaCorrido,
    tdValorAntecipacaoDiaUtil,
    tdPercentualSobreValorNominalDiaCorrido,
    tdPercentualSobreValorNominalDiaUtil,
    tdCancelamentoDesconto);

  TACBrLayoutRemessa = (c400, c240);

  {Identificação da Distribuicao}
  TACBrIdentDistribuicao = (tbBancoDistribui, tbClienteDistribui);

  {Tipos de ocorrências permitidas no arquivos remessa / retorno}
  TACBrTipoOcorrencia =
  (
    {Ocorrências para arquivo remessa}
    toRemessaRegistrar,
    toRemessaBaixar,
    toRemessaDebitarEmConta,
    toRemessaConcederAbatimento,
    toRemessaCancelarAbatimento,
    toRemessaConcederDesconto,
    toRemessaCancelarDesconto,
    toRemessaAlterarVencimento,
    toRemessaAlterarVencimentoSustarProtesto,
    toRemessaProtestar,
    toRemessaSustarProtesto,
    toRemessaCancelarInstrucaoProtestoBaixa,
    toRemessaCancelarInstrucaoProtesto,
    toRemessaDispensarJuros,
    toRemessaAlterarNomeEnderecoSacado,
    toRemessaAlterarNumeroControle,
    toRemessaOutrasOcorrencias,
    toRemessaAlterarControleParticipante,
    toRemessaAlterarSeuNumero,
    toRemessaTransfCessaoCreditoIDProd10,
    toRemessaTransferenciaCarteira,
    toRemessaDevTransferenciaCarteira,
    toRemessaDesagendarDebitoAutomatico,
    toRemessaAcertarRateioCredito,
    toRemessaCancelarRateioCredito,
    toRemessaAlterarUsoEmpresa,
    toRemessaNaoProtestar,
    toRemessaProtestoFinsFalimentares,
    toRemessaBaixaporPagtoDiretoCedente,
    toRemessaCancelarInstrucao,
    toRemessaAlterarVencSustarProtesto,
    toRemessaCedenteDiscordaSacado,
    toRemessaCedenteSolicitaDispensaJuros,
    toRemessaOutrasAlteracoes,
    toRemessaAlterarModalidade,
    toRemessaAlterarExclusivoCliente,
    toRemessaNaoCobrarJurosMora,
    toRemessaCobrarJurosMora,
    toRemessaAlterarValorTitulo,
    toRemessaExcluirSacadorAvalista,
    toRemessaAlterarNumeroDiasProtesto,
    toRemessaAlterarPrazoProtesto,
    toRemessaAlterarPrazoDevolucao,
    toRemessaAlterarOutrosDados,
    toRemessaAlterarDadosEmissaoBloqueto,
    toRemessaAlterarProtestoDevolucao,
    toRemessaAlterarDevolucaoProtesto,
    toRemessaNegativacaoSerasa,
    toRemessaExcluirNegativacaoSerasa,
    toRemessaAlterarJurosMora,
    toRemessaAlterarMulta,
    toRemessaDispensarMulta,
    toRemessaAlterarDesconto,
    toRemessaNaoConcederDesconto,
    toRemessaAlterarValorAbatimento,
    toRemessaAlterarPrazoLimiteRecebimento,
    toRemessaDispensarPrazoLimiteRecebimento,
    toRemessaAlterarNumeroTituloBeneficiario,
    toRemessaAlterarDadosPagador,
    toRemessaAlterarDadosSacadorAvalista,
    toRemessaRecusaAlegacaoPagador,
    toRemessaAlterarDadosRateioCredito,
    toRemessaPedidoCancelamentoDadosRateioCredito,
    toRemessaPedidoDesagendamentoDebietoAutom,
    toRemessaAlterarEspecieTitulo,
    toRemessaAlterarContratoCobran,
    toRemessaNegativacaoSemProtesto,
    toRemessaBaixaTituloNegativadoSemProtesto,
    toRemessaAlterarValorMinimo,
    toRemessaAlterarValorMaximo,
    toRemessaExcluirNegativacaoSerasaBaixar,
    toRemessaPedidoNegativacao,
    toRemessaExcluirNegativacaoBaixar,
    toRemessaExcluirNegativacaoManterEmCarteira,
    toRemessaSustarProtestoBaixarTitulo,
    toRemessaSustarProtestoManterCarteira,
    toRemessaRecusaAlegacaoSacado,
    toRemessaProtestoAutomatico,
    toRemessaAlterarStatusDesconto,
    toRemessaProtestarUrgente,
    toRemessaRegistrarDireta,
    toRemessaAlterarNumeroDiasBaixa,
    toRemessaAlterarValorMinimoMaximo,
    toRemessaAlteracaoQuantidadeParcela,
    toRemessaAlteracaoValorNominal,
    toRemessaNaoBaixarAutomaticamente,
    toRemessaAlteracaoPercentualParaMinimo,
    toRemessaAlteracaoPercentualParaMaximo,
    toRemessaAlteracaoPercentualParaMinimoMaximo,
    toRemessaProrrogarVencimento,
    toRemessaHibrido,

    {Ocorrências para arquivo retorno}
    toRetornoAbatimentoCancelado,
    toRetornoAbatimentoConcedido,
    toRetornoAcertoControleParticipante,
    toRetornoAcertoDadosRateioCredito,
    toRetornoAcertoDepositaria,
    toRetornoAguardandoAutorizacaoProtestoEdital,
    toRetornoAlegacaoDoSacado,
    toRetornoAlteracaoDadosBaixa,
    toRetornoAlteracaoDadosNovaEntrada,
    toRetornoAlteracaoDadosRejeitados,
    toRetornoAlteracaoDataEmissao,
    toRetornoAlteracaoEspecie,
    toRetornoAlteracaoInstrucao,
    toRetornoAlteracaoOpcaoDevolucaoParaProtestoConfirmada,
    toRetornoAlteracaoOpcaoProtestoParaDevolucaoConfirmada,
    toRetornoAlteracaoOutrosDadosRejeitada,
    toRetornoAlteracaoReemissaoBloquetoConfirmada,
    toRetornoAlteracaoSeuNumero,
    toRetornoAlteracaoUsoCedente,
    toRetornoAlterarDataDesconto,
    toRetornoAlterarPrazoLimiteRecebimento,
    toRetornoAlterarSacadorAvalista,
    toRetornoBaixaAutomatica,
    toRetornoBaixaCreditoCCAtravesSispag,
    toRetornoBaixaCreditoCCAtravesSispagSemTituloCorresp,
    toRetornoBaixado,
    toRetornoBaixadoFrancoPagamento,
    toRetornoBaixadoInstAgencia,
    toRetornoBaixadoPorDevolucao,
    toRetornoBaixadoViaArquivo,
    toRetornoBaixaLiquidadoEdital,
    toRetornoBaixaManualConfirmada,
    toRetornoBaixaOuLiquidacaoEstornada,
    toRetornoBaixaPorProtesto,
    toRetornoBaixaPorTerSidoLiquidado,
    toRetornoBaixaRejeitada,
    toRetornoBaixaSimples,
    toRetornoBaixaSolicitada,
    toRetornoBaixaTituloNegativadoSemProtesto,
    toRetornoBaixaTransferenciaParaDesconto,
    toRetornoCancelamentoDadosRateio,
    toRetornoChequeCompensado,
    toRetornoChequeDevolvido,
    toRetornoChequePendenteCompensacao,
    toRetornoCobrancaContratual,
    toRetornoCobrancaCreditar,
    toRetornoComandoRecusado,
    toRetornoConfCancelamentoNegativacaoExpressaTarifa,
    toRetornoConfEntradaNegativacaoExpressaTarifa,
    toRetornoConfExclusaoEntradaNegativacaoExpressaPorLiquidacaoTarifa,
    toRetornoConfInstrucaoTransferenciaCarteiraModalidadeCobranca,
    toRetornoConfirmacaoAlteracaoBancoSacado,
    toRetornoConfirmacaoAlteracaoJurosMora,
    toRetornoConfirmacaoEmailSMS,
    toRetornoConfirmacaoEntradaCobrancaSimples,
    toRetornoConfirmacaoExclusaoBancoSacado,
    toRetornoConfirmacaoInclusaoBancoSacado,
    toRetornoConfirmacaoPedidoExclNegativacao,
    toRetornoConfirmacaoRecebPedidoNegativacao,
    toRetornoConfirmaRecebimentoInstrucaoNaoNegativar,
    toRetornoConfRecebimentoInstCancelamentoNegativacaoExpressa,
    toRetornoConfRecebimentoInstEntradaNegativacaoExpressa,
    toRetornoConfRecebimentoInstExclusaoEntradaNegativacaoExpressa,
    toRetornoCustasCartorio,
    toRetornoCustasCartorioDistribuidor,
    toRetornoCustasEdital,
    toRetornoCustasIrregularidade,
    toRetornoCustasProtesto,
    toRetornoCustasSustacao,
    toRetornoCustasSustacaoJudicial,
    toRetornoDadosAlterados,
    toRetornoDebitoCustasAntecipadas,
    toRetornoDebitoDiretoAutorizado,
    toRetornoDebitoDiretoNaoAutorizado,
    toRetornoDebitoEmConta,
    toRetornoDebitoMensalTarifaAvisoMovimentacaoTitulos,
    toRetornoDebitoMensalTarifasExtradoPosicao,
    toRetornoDebitoMensalTarifasManutencaoTitulosVencidos,
    toRetornoDebitoMensalTarifasOutrasInstrucoes,
    toRetornoDebitoMensalTarifasOutrasOcorrencias,
    toRetornoDebitoMensalTarifasProtestos,
    toRetornoDebitoMensalTarifasSustacaoProtestos,
    toRetornoDebitoTarifas,
    toRetornoDesagendamentoDebitoAutomatico,
    toRetornoDescontoCancelado,
    toRetornoDescontoConcedido,
    toRetornoDescontoRetificado,
    toRetornoDespesaCartorio,
    toRetornoDespesasProtesto,
    toRetornoDespesasSustacaoProtesto,
    toRetornoDevolvidoPeloCartorio,
    toRetornoDispensarIndexador,
    toRetornoDispensarPrazoLimiteRecebimento,
    toRetornoEmailSMSRejeitado,
    toRetornoEmissaoBloquetoBancoSacado,
    toRetornoEncaminhadoACartorio,
    toRetornoEnderecoSacadoAlterado,
    toRetornoEntradaBorderoManual,
    toRetornoEntradaConfirmadaRateioCredito,
    toRetornoEntradaEmCartorio,
    toRetornoEntradaRegistradaAguardandoAvaliacao,
    toRetornoEntradaRejeitaCEPIrregular,
    toRetornoEntradaRejeitadaCarne,
    toRetornoEntradaTituloBancoSacadoRejeitada,
    toRetornoEqualizacaoVendor,
    toRetornoEstornoBaixaLiquidacao,
    toRetornoEstornoPagamento,
    toRetornoEstornoProtesto,
    toRetornoInstrucaoCancelada,
    toRetornoInstrucaoNegativacaoExpressaRejeitada,
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente,
    toRetornoInstrucaoRejeitada,
    toRetornoIOFInvalido,
    toRetornoJurosDispensados,
    toRetornoLiquidado,
    toRetornoLiquidadoAposBaixaOuNaoRegistro,
    toRetornoLiquidadoEmCartorio,
    toRetornoLiquidadoParcialmente,
    toRetornoLiquidadoPorConta,
    toRetornoLiquidadoSaldoRestante,
    toRetornoLiquidadoSemRegistro,
    toRetornoManutencaoBancoSacadoRejeitada,
    toRetornoManutencaoSacadoRejeitada,
    toRetornoManutencaoTituloVencido,
    toRetornoNegativacaoExpressaInformacional,
    toRetornoNomeSacadoAlterado,
    toRetornoOcorrenciasDoSacado,
    toRetornoOutrasOcorrencias,
    toRetornoOutrasTarifasAlteracao,
    toRetornoPagadorDDA,
    toRetornoPrazoDevolucaoAlterado,
    toRetornoPrazoProtestoAlterado,
    toRetornoProtestado,
    toRetornoProtestoImediatoFalencia,
    toRetornoProtestoOuSustacaoEstornado,
    toRetornoProtestoSustado,
    toRetornoRecebimentoInstrucaoAlterarDados,
    toRetornoRecebimentoInstrucaoAlterarEnderecoSacado,
    toRetornoRecebimentoInstrucaoAlterarJuros,
    toRetornoRecebimentoInstrucaoAlterarNomeSacado,
    toRetornoRecebimentoInstrucaoAlterarTipoCobranca,
    toRetornoRecebimentoInstrucaoAlterarValorTitulo,
    toRetornoRecebimentoInstrucaoAlterarVencimento,
    toRetornoRecebimentoInstrucaoBaixar,
    toRetornoRecebimentoInstrucaoCancelarAbatimento,
    toRetornoRecebimentoInstrucaoCancelarDesconto,
    toRetornoRecebimentoInstrucaoConcederAbatimento,
    toRetornoRecebimentoInstrucaoConcederDesconto,
    toRetornoRecebimentoInstrucaoDispensarJuros,
    toRetornoRecebimentoInstrucaoNaoProtestar,
    toRetornoRecebimentoInstrucaoProtestar,
    toRetornoRecebimentoInstrucaoSustarProtesto,
    toRetornoReembolsoDevolucaoDescontoVendor,
    toRetornoReembolsoNaoEfetuado,
    toRetornoReembolsoTransferenciaDescontoVendor,
    toRetornoRegistroConfirmado,
    toRetornoRegistroRecusado,
    toRetornoRelacaoDeTitulos,
    toRetornoRemessaRejeitada,
    toRetornoReservado,
    toRetornoRetiradoDeCartorio,
    toRetornoSegundaViaInstrumentoProtesto,
    toRetornoSegundaViaInstrumentoProtestoCartorio,
    toRetornoSolicitacaoImpressaoTituloConfirmada,
    toRetornoSustacaoEnvioCartorio,
    toRetornoSustadoJudicial,
    toRetornoTarifaAvisoCobranca,
    toRetornoTarifaDeManutencaoDeTitulosVencidos,
    toRetornoTarifaDeRelacaoDasLiquidacoes,
    toRetornoTarifaEmailCobrancaAtivaEletronica,
    toRetornoTarifaEmissaoAvisoMovimentacaoTitulos,
    toRetornoTarifaEmissaoBoletoEnvioDuplicata,
    toRetornoTarifaExtratoPosicao,
    toRetornoTarifaInstrucao,
    toRetornoTarifaMensalBaixasBancosCorrespCarteira,
    toRetornoTarifaMensalBaixasCarteira,
    toRetornoTarifaMensalCancelamentoNegativacaoExpressa,
    toRetornoTarifaMensalEmailCobrancaAtivaEletronica,
    toRetornoTarifaMensalEmissaoBoletoEnvioDuplicata,
    toRetornoTarifaMensalExclusaoEntradaNegativacaoExpressa,
    toRetornoTarifaMensalExclusaoNegativacaoExpressaPorLiquidacao,
    toRetornoTarifaMensalLiquidacoesBancosCorrespCarteira,
    toRetornoTarifaMensalLiquidacoesCarteira,
    toRetornoTarifaMensalPorBoletoAte03EnvioCobrancaAtivaEletronica,
    toRetornoTarifaMensalRefEntradasBancosCorrespCarteira,
    toRetornoTarifaMensalSMSCobrancaAtivaEletronica,
    toRetornoTarifaOcorrencias,
    toRetornoTarifaPorBoletoAte03EnvioCobrancaAtivaEletronica,
    toRetornoTarifaSMSCobrancaAtivaEletronica,
    toRetornoTipoCobrancaAlterado,
    toRetornoTituloDDANaoReconhecidoPagador,
    toRetornoTituloDDAReconhecidoPagador,
    toRetornoTituloDDARecusadoCIP,
    toRetornoTituloEmSer,
    toRetornoTituloJaBaixado,
    toRetornoTituloNaoExiste,
    toRetornoTituloPagamentoCancelado,
    toRetornoTituloPagoEmCheque,
    toRetornoTituloSustadoJudicialmente,
    toRetornoTransferenciaCarteira,
    toRetornoTransferenciaCarteiraBaixa,
    toRetornoTransferenciaCarteiraEntrada,
    toRetornoTransferenciaCedente,
    toRetornoTransitoPagoCartorio,
    toRetornoVencimentoAlterado,
    toRetornoRejeicaoSacado,
    toRetornoAceiteSacado,
    toRetornoLiquidadoOnLine,
    toRetornoEstornoLiquidacaoOnLine,
    toRetornoConfirmacaoAlteracaoValorNominal,
    toRetornoConfirmacaoAlteracaoValorpercentualMinimoMaximo,
    toTipoOcorrenciaNenhum,
    toRetornoConfRecPedidoNegativacao,
    toRetornoConfRecPedidoExclusaoNegativacao,
    toRetornoConfEntradaNegativacao,
    toRetornoEntradaNegativacaoRejeitada,
    toRetornoConfExclusaoNegativacao,
    toRetornoExclusaoNegativacaoRejeitada,
    toRetornoExcusaoNegativacaoOutrosMotivos,
    toRetornoOcorrenciaInfOutrosMotivos,
    toRetornoInclusaoNegativacao,
    toRetornoExclusaoNegativacao,
    toRetornoEmTransito,
    toRetornoLiquidadoEmCartorioEmCondicionalComChequeDoDevedor,
    toRetornoProtestoSustadoDefinitivo,
    toRetornoLiquidadoTituloDescontado,
    toRetornoProtestadoEmCartorio,
    toRetornoSustacaoSolicitada,
    toRetornoTituloDescontado,
    toRetornoTituloDescontavel,
    toRetornoIntensaoPagamento ,
    toRetornoEntradaConfirmadaNaCip,
    toRetornoConfirmacaoAlteracaoValorMinimoOuPercentual,
    toRetornoConfirmacaoAlteracaoValorMaximoOuPercentual,
    toRetornoConfirmacaoPedidoDispensaMulta,
    toRetornoConfirmacaoPedidoCobrancaMulta,
    toRetornoConfirmacaoPedidoAlteracaoBeneficiarioTitulo,
    toRetornoExcluirProtestoCartaAnuencia,
    toRetornoConfirmacaoCancelamentoBaixaAutomatica,
    toRetornoConfAlteracaoDiasBaixaAutomatica,
    toRetornoConfInstrucaoProtesto,
    toRetornoConfInstrucaoSustacaoProtesto,
    toRetornoConfInstrucaoNaoProtestar,
    toRetornoConfInstrucaoNaoBaixarAutomaticamente,
    toRetornoAlteracaoPercentualMinimo,
    toRetornoAlteracaoPercentualMaximo,
    toRetornoAlteracaoPercentualMinimoMaximo,
    toRetornoRecebimentoInstrucaoNaoBaixar,
    toRetornoConfirmacaoProtesto,
    toRetornoConfirmacaoSustacao,
    toRetornoProtestoSustadoJudicialmente,
    toRetornoConfInstrucaoSustarProtesto,
    toRetornoConfInstrucaoAlteracaoDiasBaixaAutomatica,
    toRetornoAlteracaoQuantidadeParcela
  );

  //Complemento de instrução para alterar outros dados
  TACBrComplementoOcorrenciaOutrosDados =
  (
    TCompDesconto,
    TCompJurosDia,
    TCompDescontoDiasAntecipacao,
    TCompDataLimiteDesconto,
    TCompCancelaProtestoAutomatico,
    TCompCarteiraCobranca, //Segundo Manual ainda não disponivel
    TCompCancelaNegativacaoAutomatica
  );

  {TACBrOcorrencia}
  TACBrOcorrencia = class
  private
     fTipo: TACBrTipoOcorrencia;
     fpAOwner: TACBrTitulo;
     fComplementoOutrosDados: TACBrComplementoOcorrenciaOutrosDados;
     function GetCodigoBanco: String;
     function GetDescricao: String;
  public
     constructor Create(AOwner: TACBrTitulo);
     property Tipo: TACBrTipoOcorrencia read fTipo write fTipo;
     property ComplementoOutrosDados: TACBrComplementoOcorrenciaOutrosDados
      read fComplementoOutrosDados write fComplementoOutrosDados;
     property Descricao  : String  read GetDescricao;
     property CodigoBanco: String  read GetCodigoBanco;
  end;

  { TACBrBoletoChavePIX }
  TACBrBoletoChavePIX = class(TComponent)
  private
    fTipoChave: TACBrPIXTipoChave;
    fChave: String;
  published
    property TipoChavePIX: TACBrPIXTipoChave read fTipoChave write fTipoChave;
    property Chave: String   read fChave write fChave;
  end;

  { TACBrBancoClass }
  TACBrBancoClass = class
  protected
    fpDigito: Integer;
    fpNome:   String;
    fpNumero: Integer;
    fpModulo: TACBrCalcDigito;
    fpTamanhoAgencia: Integer;
    fpTamanhoCarteira: Integer;
    fpTamanhoConta: Integer;
    fpAOwner: TACBrBanco;
    fpTamanhoMaximoNossoNum: Integer;
    fpTamanhoNumeroDocumento: Integer;
    fpOrientacoesBanco: TStringList;
    fpCodigosMoraAceitos: String;
    fpCodigosGeracaoAceitos: String;
    fpNumeroCorrespondente: Integer;
    fpLayoutVersaoArquivo : Integer; // Versão do Hearder do arquivo
    fpLayoutVersaoLote : Integer; // Versão do Hearder do Lote
    fpCasasDecimaisMoraJuros: Integer;
    fpModuloMultiplicadorInicial: Integer;
    fpModuloMultiplicadorFinal: Integer;
    fpModuloMultiplicadorAtual: Integer;
    fpCodParametroMovimento: String;
    fpDensidadeGravacao: String;
    fpQtdRegsLote: Integer;
    fpQtdRegsCobranca: Integer;
    fpVlrRegsCobranca: Double;

    function GetLocalPagamento: String; virtual;
    function CalcularFatorVencimento(const DataVencimento: TDateTime): String; virtual;
    function CalcularDigitoCodigoBarras(const CodigoBarras: String): String; virtual;
    function FormatarMoraJurosRemessa(const APosicoes: Integer
       ;const ACBrTitulo: TACBrTitulo):String; Virtual;

    function DefineNumeroDocumentoModulo(const ACBrTitulo: TACBrTitulo): String; virtual;  //Utilizado para método CalculaDigitoVerificador
    function ConverterDigitoModuloFinal(): String; virtual;                                //Utilizado para método CalculaDigitoVerificador
    function DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String; virtual; //Utilizado para o método MontarCodigoBarras
    procedure ValidaNossoNumeroResponsavel(out ANossoNumero: String;
      out ADigVerificador: String; const ACBrTitulo :TACBrTitulo); virtual;                //Utilizado para validar Nosso número na geraçao Remessa e Retorno CNAB400
    function MontaInstrucoesCNAB400(const ACBrTitulo :TACBrTitulo; const nRegistro: Integer ): String; virtual;   //Utilizado para Montar Instruções CNAB 400
    function DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String; virtual;       //Utilizado para definir Especie na geração Remessa
    function DefineTipoSacado(const ACBrTitulo: TACBrTitulo): String; virtual;       //Utilizado para definir Tipo do Sacado na geração Remessa
    function DefineTipoSacadoAvalista(const ACBrTitulo: TACBrTitulo): String; virtual;//Utilizado para definir Tipo do Sacado avalista na geração Remessa
    function DefineTipoBoleto(const ACBrTitulo: TACBrTitulo): String; virtual;       //Utilizado para definir Tipo do Boleto na Remessa
    function InstrucoesProtesto(const ACBrTitulo: TACBrTitulo): String; virtual;     //Utilizado para definir Instruções de Protesto para Remessa
    function DefineCaracTitulo(const ACBrTitulo: TACBrTitulo): String; virtual;      //Utilizado para definir caracteristica do título na remessa
    function DefineCodigoProtesto(const ACBrTitulo: TACBrTitulo): String; virtual;   //Utilizado para definir Código Protesto do título na remessa
    function DefineTipoDiasProtesto(const ACBrTitulo: TACBrTitulo): String; virtual; //Utilizado para definir Tipo de Dias Protesto na remessa
    function DefineAceite(const ACBrTitulo: TACBrTitulo): String; virtual;           //Utilizado para definir o tipo de aceite na geração da remessa
    function DefineCodigoMoraJuros(const ACBrTitulo: TACBrTitulo): String; virtual; //Utilizado para definir o Código de Mora na Remessa.
    function DefineDataMoraJuros(const ACBrTitulo: TACBrTitulo; AFormat: String = 'ddmmyyyy'): String; virtual;   //Utilizado para definir a Data de Mora Juros na Remessa.
    function DefineCodigoDesconto(const ACBrTitulo: TACBrTitulo): String; virtual;  //Utilizando para definir Código de Desconto na Remessa
    function DefineDataDesconto(const ACBrTitulo: TACBrTitulo; AFormat: String = 'ddmmyyyy'): String; virtual;    //Utilizado para definir a Data de Desconto na Remessa
    function DefineCodigoMulta(const ACBrTitulo: TACBrTitulo): String; virtual;     //Utilizado para definir o Codigo Multa na Remessa
    function DefineDataMulta(const ACBrTitulo: TACBrTitulo; AFormat: String = 'ddmmyyyy'): String; virtual;       //Utilizado para definir data multa na Remessa
    function DefineNossoNumeroRetorno(const Retorno: String): String; virtual;      //Define a leitura do Nosso Número no Retorno
    function DefineTamanhoContaRemessa: Integer; virtual;                           //Define o tamnhano da Conta para Remessa e Retorno  (pode ser diferente do tamanho padrão no Boleto)
    function DefineTamanhoAgenciaRemessa: Integer; virtual;                         //Define o tamnhano da Agencia para Remessa e Retorno  (pode ser diferente do tamanho padrão no Boleto)
    function DefinePosicaoNossoNumeroRetorno: Integer; virtual;                     //Define posição para leitura de Retorno campo: NossoNumero
    function DefineTamanhoNossoNumeroRetorno: Integer; virtual;                     //Define posição para leitura de Retorno campo: NossoNumero
    function DefinePosicaoCarteiraRetorno:Integer; virtual;                         //Define posição para leitura de Retorno campo: NumeroDocumento
    function DefineDataOcorrencia(const ALinha: String): String; virtual;           //Define a data da ocorrencia
    function DefineSeuNumeroRetorno(const ALinha: String): String; virtual;         //Define o Seu Numero
    function DefinerCnpjCPFRetorno240(const ALinha: String): String; virtual;       //Define retorno rCnpjCPF
    function DefineNumeroDocumentoRetorno(const ALinha: String): String; virtual;   //Define o Numero Documento do Retorno
    procedure DefineRejeicaoComplementoRetorno(const ALinha: String; out ATitulo : TACBrTitulo); virtual;   //Define o Motivo da Rejeição ou Complemento no Retorno

    function DefineTipoInscricao: String; virtual;                            //Utilizado para definir Tipo de Inscrição na Remessa
    function DefineResponsEmissao: String; virtual;                           //Utilizado para definir Responsável Emissão na Remessa
    function DefineCampoConvenio(const ATam: Integer = 20): String; virtual;  //Utilizado para definir campo convenio na Remessa
    function DefineCampoDigitoAgencia: String; virtual;                       //Utilizado para definir Digito da Agencia na Remessa
    function DefineCampoDigitoConta: String; virtual;                         //Utilizado para definir Digito da Conta na Remessa
    function DefineCampoDigitoAgenciaConta: String; virtual;                  //Utilizado para definir Digito da Agencia / Conta na Remessa
    function DefinePosicaoUsoExclusivo: String; virtual;                      //Utilizado para definir Posições de uso exclusivo FEBRABAN na Remessa
    function DefineCodBeneficiarioHeader: String; virtual;                    //Utilizado para definir CodBeneficiario no Header da Remessa
    function DefineTipoDocumento: String; virtual;                            //Define o Tipo de Documento na remessa
    function DefineAceiteImpressao(const ACBrTitulo: TACBrTitulo): String; virtual;  //Utilizado para definir o tipo de aceite na impressao
    procedure EhObrigatorioAgencia; virtual;
    procedure EhObrigatorioAgenciaDV; virtual;
    procedure EhObrigatorioConta; virtual;
    procedure EhObrigatorioContaDV; virtual;
    procedure EhObrigatorioNomeBeneficiario; virtual;
  public
    Constructor create(AOwner: TACBrBanco);
    Destructor Destroy; override ;

    property ACBrBanco : TACBrBanco      read fpAOwner;
    property Numero    : Integer         read fpNumero;
    property Digito    : Integer         read fpDigito;
    property Nome      : String          read fpNome;
    Property Modulo    : TACBrCalcDigito read fpModulo;
    property TamanhoMaximoNossoNum: Integer    read fpTamanhoMaximoNossoNum;
    property TamanhoNumeroDocumento: Integer   read fpTamanhoNumeroDocumento;
    property TamanhoAgencia  :Integer read fpTamanhoAgencia;
    property TamanhoConta    :Integer read fpTamanhoConta;
    property TamanhoCarteira :Integer read fpTamanhoCarteira;
    property OrientacoesBanco: TStringList read fpOrientacoesBanco;
    property CodigosMoraAceitos: String read fpCodigosMoraAceitos;
    property CodigosGeracaoAceitos: String read fpCodigosGeracaoAceitos;
    property LocalPagamento  : String read GetLocalPagamento;
    property NumeroCorrespondente : Integer read fpNumeroCorrespondente;
    Property LayoutVersaoArquivo  : Integer read fpLayoutVersaoArquivo;
    Property LayoutVersaoLote     : Integer read fpLayoutVersaoLote;
    property CasasDecimaisMoraJuros: Integer read fpCasasDecimaisMoraJuros;
    property ModuloMultiplicadorInicial: Integer read fpModuloMultiplicadorInicial;
    property ModuloMultiplicadorFinal: Integer read fpModuloMultiplicadorFinal;
    property ModuloMultiplicadorAtual: Integer read fpModuloMultiplicadorAtual;
    property CodParametroMovimento: String read fpCodParametroMovimento;
    property DensidadeGravacao: String read fpDensidadeGravacao;
    property QtdRegsLote: Integer read fpQtdRegsLote write fpQtdRegsLote;
    property QtdRegsCobranca: Integer read fpQtdRegsCobranca write fpQtdRegsCobranca;
    property VlrRegsCobranca: Double read fpVlrRegsCobranca write fpVlrRegsCobranca;

    function CalcularDigitoVerificador(const ACBrTitulo : TACBrTitulo): String; virtual;
    function CalcularTamMaximoNossoNumero(const Carteira : String; const NossoNumero : String = ''; const Convenio: String = ''): Integer; virtual;

    function TipoDescontoToString(const AValue: TACBrTipoDesconto):string; virtual;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; virtual;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; virtual;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; virtual;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia;CodMotivo:Integer): String; overload; virtual;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String; overload; virtual;

    function CompOcorrenciaOutrosDadosToDescricao(const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String; virtual;
    function CompOcorrenciaOutrosDadosToCodigo(const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String; virtual;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; virtual;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; virtual;

    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; virtual;
    function MontarCampoNossoNumero(const ACBrTitulo : TACBrTitulo): String; virtual;
    function MontarLinhaDigitavel(const CodigoBarras: String; ACBrTitulo : TACBrTitulo): String; virtual;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; virtual;
    function MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String; virtual;

    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);  Virtual;
    function GerarRegistroHeader240(NumeroRemessa : Integer): String;    Virtual;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); Virtual;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; Virtual;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);  Virtual;
    function GerarRegistroTrailler240(ARemessa:TStringList): String;  Virtual;
    Procedure LerRetorno400(ARetorno:TStringList); Virtual;
    Procedure LerRetorno240(ARetorno:TStringList); Virtual;

    Procedure LerRetorno400Transacao4(ACBrTitulo :TACBrTitulo; ALinha:String); Virtual;


    function CalcularNomeArquivoRemessa : String; Virtual;
    function ValidarDadosRetorno(const AAgencia, AContaCedente: String; const ACNPJCPF: String= '';
       const AValidaCodCedente: Boolean= False ): Boolean; Virtual;
  end;

  { TACBrBanco }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBanco = class(TComponent)
  protected
    procedure Loaded; override;
  private
    fACBrBoleto        : TACBrBoleto;
    fNumeroBanco       : Integer;
    fTipoCobranca      : TACBrTipoCobranca;
    fBancoClass        : TACBrBancoClass;
    fLocalPagamento    : String;
    FCIP               : string;

    function GetNome   : String;
    function GetDigito : Integer;
    function GetNumero : Integer;
    function GetOrientacoesBanco: TStringList;
    function GetTamanhoAgencia: Integer;
    function GetTamanhoCarteira: Integer;
    function GetTamanhoConta: Integer;
    function GetTamanhoMaximoNossoNum : Integer;
    function GetTamanhoNumeroDocumento : Integer;
    function GetCodigosMoraAceitos: String;
    function GetCodigosGeracaoAceitos: string;
    function GetLocalPagamento: String;
    function GetNumeroCorrespondente: Integer;
    function GetLayoutVersaoArquivo    :Integer;
    function GetLayoutVersaoLote       :Integer;
    function GetCasasDecimaisMoraJuros :Integer;
    function GetDensidadeGravacao : String;

    procedure SetDigito(const AValue: Integer);
    procedure SetNome(const AValue: String);
    procedure SetTipoCobranca(const AValue: TACBrTipoCobranca);
    procedure SetNumero(const AValue: Integer);
    procedure SetTamanhoMaximoNossoNum(Const Avalue:Integer);
    procedure SetOrientacoesBanco(Const Avalue: TStringList);
    procedure SetLocalPagamento(const AValue: String);
    procedure SetNumeroCorrespondente(const AValue: Integer);
    procedure SetLayoutVersaoArquivo(const AValue: Integer);
    procedure SetLayoutVersaoLote(const AValue: Integer);
    procedure SetCasasDecimaisMoraJuros(const AValue: Integer);
    procedure SetDensidadeGravacao(const AValue: String);
    procedure SetCIP(const Value: string);
  public
    constructor Create( AOwner : TComponent); override;
    destructor Destroy ; override ;

    property ACBrBoleto : TACBrBoleto     read fACBrBoleto;
    property BancoClass : TACBrBancoClass read fBancoClass ;
    property TamanhoAgencia        :Integer read GetTamanhoAgencia;
    property TamanhoConta          :Integer read GetTamanhoConta;
    property TamanhoCarteira       :Integer read GetTamanhoCarteira;
    property CodigosMoraAceitos    :String  read GetCodigosMoraAceitos;
    property CodigosGeracaoAceitos :String  read GetCodigosGeracaoAceitos;
    property TamanhoNumeroDocumento:Integer  read GetTamanhoNumeroDocumento;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia;CodMotivo:Integer): String;

    function CompOcorrenciaOutrosDadosToDescricao(const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String; virtual;
    function CompOcorrenciaOutrosDadosToCodigo(const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String; virtual;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia ): String;
    function CalcularDigitoVerificador(const ACBrTitulo : TACBrTitulo): String;
    function CalcularTamMaximoNossoNumero(const Carteira : String; const NossoNumero : String = ''; const Convenio: String = ''): Integer;

    function MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
    function MontarLinhaDigitavel(const CodigoBarras: String; ACBrTitulo : TACBrTitulo): String;

    procedure GerarRegistroHeader400(NumeroRemessa : Integer; ARemessa:TStringList);
    function GerarRegistroHeader240(NumeroRemessa : Integer): String;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo;aRemessa: TStringList);
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
    procedure GerarRegistroTrailler400(ARemessa:TStringList);
    function GerarRegistroTrailler240(ARemessa:TStringList): String;

    procedure LerRetorno400(ARetorno:TStringList);
    procedure LerRetorno240(ARetorno:TStringList);

    function CalcularNomeArquivoRemessa : String;
    function ValidarDadosRetorno(const AAgencia, AContaCedente: String; const ACNPJCPF: String= '';
       const AValidaCodCedente: Boolean= False ): Boolean;
    function ConverterCodigoBarrasITF25ParaLinhaDigitavel(const ACodigoBarras:String):String;
  published
    property Numero    : Integer        read GetNumero  write SetNumero default 0;
    property Digito    : Integer        read GetDigito  write SetDigito stored false;
    property Nome      : String         read GetNome    write SetNome   stored false;
    property TamanhoMaximoNossoNum :Integer read GetTamanhoMaximoNossoNum  write SetTamanhoMaximoNossoNum;
    property TipoCobranca : TACBrTipoCobranca read fTipoCobranca   write SetTipoCobranca;
    property OrientacoesBanco : TStringList read GetOrientacoesBanco write SetOrientacoesBanco;
    property LocalPagamento : String read GetLocalPagamento write SetLocalPagamento;
    property NumeroCorrespondente : Integer read GetNumeroCorrespondente write SetNumeroCorrespondente default 0;
    property LayoutVersaoArquivo  : Integer read GetLayoutVersaoArquivo write SetLayoutVersaoArquivo;
    property LayoutVersaoLote     : Integer read GetLayoutVersaoLote write SetLayoutVersaoLote;
    property CasasDecimaisMoraJuros: Integer read GetCasasDecimaisMoraJuros write SetCasasDecimaisMoraJuros;
    property DensidadeGravacao : string read GetDensidadeGravacao write SetDensidadeGravacao;
    property CIP: string read FCIP write SetCIP;

  end;

  { TACBrCedenteWS }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCedenteWS = class(TComponent)
  private
    FClientID: string;
    FClientSecret: string;
    FKeyUser: string;
    FScope: string;
    FIndicadorPix: boolean;
    FIndicadorSMS: Boolean;
    FIndicadorEmail: Boolean;

    procedure SetClientID(const Value: string);
    procedure SetClientSecret(const Value: string);
    procedure SetKeyUser(const Value: string);
    procedure SetScope(const Value: string);
    procedure SetIndicadorPix(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;

  published
    property ClientID: string read fClientID write setClientID;
    property ClientSecret: string read fClientSecret write setClientSecret;
    property KeyUser: string read fKeyUser write setKeyUser;
    property Scope: string read fScope write setScope;
    property IndicadorPix: boolean read FIndicadorPix write SetIndicadorPix default False;
    property IndicadorEmail: Boolean read FIndicadorEmail write FIndicadorEmail default False;
    property IndicadorSMS: Boolean read FIndicadorSMS write FIndicadorSMS default False;

  end;

  { TACBrCedente }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCedente = class(TComponent)
  private
    fCodigoTransmissao: String;
    fLogradouro: String;
    fBairro: String;
    fNumeroRes: String;
    fCEP: String;
    fCidade: String;
    fCodigoCedente: String;
    fComplemento: String;
    fTelefone: String;
    fNomeCedente   : String;
    fFantasiaCedente : String;
    fAgencia       : String;
    fAgenciaDigito : String;
    fConta         : String;
    fContaDigito   : String;
    fModalidade    : String;
    fConvenio      : String;
    fTipoDocumento : TACBrTipoDocumento;
    fResponEmissao : TACBrResponEmissao;
    fCaracTitulo:TACBrCaracTitulo;
    fCNPJCPF       : String;
    fTipoInscricao : TACBrPessoaCedente;
    fUF            : String;
    fAcbrBoleto    : TACBrBoleto;
    fTipoCarteira: TACBrTipoCarteira;
    fDigitoVerificadorAgenciaConta: String;
    fCedenteWS: TACBrCedenteWS;
    fIdentDistribuicao: TACBrIdentDistribuicao;
    fOperacao: string;
    FPIX               : TACBrBoletoChavePIX;
    procedure SetAgencia(const AValue: String);
    procedure SetCNPJCPF ( const AValue: String ) ;
    procedure SetConta(const AValue: String);
    procedure SetTipoInscricao ( const AValue: TACBrPessoaCedente ) ;
  public
    constructor Create(AOwner : TComponent); override;

    property ACBrBoleto  : TACBrBoleto read fACBrBoleto;
  published
    property Nome         : String read fNomeCedente   write fNomeCedente;
    property FantasiaCedente: String read fFantasiaCedente   write fFantasiaCedente;
    property CodigoCedente: String read fCodigoCedente write fCodigoCedente;
    property CodigoTransmissao : String read fCodigoTransmissao write fCodigoTransmissao;
    property Agencia      : String read fAgencia       write SetAgencia;
    property AgenciaDigito: String read fAgenciaDigito write fAgenciaDigito;
    property Conta        : String read fConta         write SetConta;
    property ContaDigito  : String read fContaDigito   write fContaDigito;
    property Modalidade   : String read fModalidade    write fModalidade;
    property Convenio     : String read fConvenio      write fConvenio;
    property TipoDocumento : TACBrTipoDocumento read fTipoDocumento write fTipoDocumento default Tradicional;
    property TipoCarteira : TACBrTipoCarteira read fTipoCarteira write fTipoCarteira default tctSimples;
    property ResponEmissao: TACBrResponEmissao read fResponEmissao  write fResponEmissao default tbCliEmite ;
    property CaracTitulo : TACBrCaracTitulo read fCaracTitulo  write fCaracTitulo default tcSimples;
    property CNPJCPF     : String  read fCNPJCPF  write SetCNPJCPF;
    property TipoInscricao: TACBrPessoaCedente  read fTipoInscricao write  SetTipoInscricao;
    property Logradouro  : String  read fLogradouro  write fLogradouro;
    property NumeroRes   : String  read fNumeroRes   write fNumeroRes;
    property Complemento : String  read fComplemento write fComplemento;
    property Bairro      : String  read fBairro      write fBairro;
    property Cidade      : String  read fCidade      write fCidade;
    property UF          : String  read fUF          write fUF;
    property CEP         : String  read fCEP         write fCEP;
    property Telefone    : String  read fTelefone    write fTelefone;
    property DigitoVerificadorAgenciaConta  : String read fDigitoVerificadorAgenciaConta write fDigitoVerificadorAgenciaConta;
    property CedenteWS: TACBrCedenteWS read fCedenteWS;
    property IdentDistribuicao: TACBrIdentDistribuicao read fIdentDistribuicao  write fIdentDistribuicao default tbClienteDistribui;
    property Operacao: string read fOperacao write fOperacao;
    property PIX: TACBrBoletoChavePIX read FPIX write FPIX;
  end;

  { TACBrDataPeriodo }
  TACBrDataPeriodo = class
  private
    FDataInicio: TDateTime;
    FDataFinal : TDateTime;
    procedure SetDataInicio(const Value: TDateTime);
    procedure SetDataFinal(const Value: TDateTime);
  public
    constructor Create();
    property DataInicio: TDateTime read FDataInicio write SetDataInicio;
    property DataFinal: TDateTime  read FDataFinal  write SetDataFinal;
  end;

  { TACBrBoletoWSFiltroConsulta }
  TACBrBoletoWSFiltroConsulta = class
  private
    FContaCaucao                : Integer;
    FCnpjCpfPagador             : String;
    FDataVencimento             : TACBrDataPeriodo;
    FDataRegistro               : TACBrDataPeriodo;
    FDataMovimento              : TACBrDataPeriodo;
    FIndicadorSituacaoBoleto    : TACBrIndicadorSituacaoBoleto;
    FCodigoEstadoTituloCobranca : Integer;
    FBoletoVencido              : TACBrIndicadorBoletoVencido;
    FIndiceContinuidade         : Extended;
    FModalidadeCobrancao        : Integer;
    FCarteira                   : Integer;
    FCarteiraVariacao           : Integer;
    procedure SetContaCaucao(const Value: Integer);
    procedure SetCnpjCpfPagador(const Value: string);
    procedure SetDataVencimento(const Value: TACBrDataPeriodo);
    procedure SetDataRegistro(const Value: TACBrDataPeriodo);
    procedure SetDataMovimento(const Value: TACBrDataPeriodo);
    procedure SetIndicadorSituacaoBoleto(const Value: TACBrIndicadorSituacaoBoleto);
    procedure SetCodigoEstadoTituloCobranca(const Value: Integer);
    procedure SetBoletoVencido(const Value: TACBrIndicadorBoletoVencido);
    procedure SetIndiceContinuidade(const Value: Extended);
    procedure SetModalidadeCobranca(const Value: integer);
    procedure SetCarteira(const Value: Integer);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear;
    property indicadorSituacao          : TACBrIndicadorSituacaoBoleto read FIndicadorSituacaoBoleto    write SetIndicadorSituacaoBoleto;
    property contaCaucao                : Integer                      read FContaCaucao                write SetContaCaucao;
    property cnpjCpfPagador             : String                       read FCnpjCpfPagador             write SetCnpjCpfPagador;
    property dataVencimento             : TACBrDataPeriodo             read FDataVencimento             write SetDataVencimento;
    property dataRegistro               : TACBrDataPeriodo             read FDataRegistro               write SetDataRegistro;
    property dataMovimento              : TACBrDataPeriodo             read FDataMovimento              write SetDataMovimento;
    property codigoEstadoTituloCobranca : Integer                      read FCodigoEstadoTituloCobranca write SetCodigoEstadoTituloCobranca;
    property boletoVencido              : TACBrIndicadorBoletoVencido  read FBoletoVencido              write SetBoletoVencido;
    property indiceContinuidade         : Extended                     read FIndiceContinuidade         write SetIndiceContinuidade;
    property modalidadeCobranca         : integer                      read FModalidadeCobrancao        write SetModalidadeCobranca;
    property carteira                   : Integer                      read FCarteira                   write SetCarteira;
    property carteiraVariacao           : Integer                      read FCarteiraVariacao           write FCarteiraVariacao;
  end;

  { TACBrWebService }
  TACBrWebServiceOnAntesAutenticar  = procedure(var aToken: String; var aValidadeToken: TDateTime) of object;
  TACBrWebServiceOnDepoisAutenticar = procedure(const aToken: String; const aValidadeToken: TDateTime) of object;
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrWebService = class(TDFeSSL)
  private
    fAmbiente: TpcnTipoAmbiente;
    fOperacao: TOperacao;
    fVersaoDF: String;
    FBoletoWSConsulta: TACBrBoletoWSFiltroConsulta;
    FArquivoCRT: string;
    FArquivoKEY: string;
    FChavePrivada: AnsiString;
    FCertificado: AnsiString;
    procedure SetWSBoletoConsulta(const Value: TACBrBoletoWSFiltroConsulta);
    procedure SetCertificado(const Value: AnsiString);
    procedure SetChavePrivada(const Value: AnsiString);
  public
    constructor Create(AOwner: TComponent); reintroduce; virtual;
    destructor Destroy; override;
    function Enviar: Boolean; virtual;
    property Filtro: TACBrBoletoWSFiltroConsulta read FBoletoWSConsulta write SetWSBoletoConsulta;
  published
    property Ambiente: TpcnTipoAmbiente read fAmbiente write fAmbiente;
    property Operacao: TOperacao read fOperacao write fOperacao;
    property VersaoDF: string read fVersaoDF write fVersaoDF;
    property ArquivoCRT: string read FArquivoCRT write FArquivoCRT;
    property ArquivoKEY: string read FArquivoKEY write FArquivoKEY;
    property Certificado: AnsiString  read FCertificado write SetCertificado;
    property ChavePrivada: AnsiString  read FChavePrivada write SetChavePrivada;
  end;

  { TACBrArquivos }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrArquivos = class(TPersistent)
  private
    fLogRegistro: Boolean;
    fPathGravarRegistro: String;
    fOnGravarLog : TACBrGravarLog ;
  public
    Constructor Create;
    procedure Assign(Source: TPersistent); override;

  published
    property LogRegistro: Boolean read fLogRegistro write fLogRegistro;
    property PathGravarRegistro: string read fPathGravarRegistro write fPathGravarRegistro;
    property OnGravarLog : TACBrGravarLog read fOnGravarLog write fOnGravarLog;

  end;

  { TConfiguracoes }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TConfiguracoes = class(TComponent)
  private
    fArquivos: TACBrArquivos;
    fWebService: TACBrWebService;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Arquivos: TACBrArquivos read fArquivos write fArquivos;
    property WebService: TACBrWebService read fWebService write fWebService;

  end;

  { TACBrTituloLiquidacao }
  TACBrTituloLiquidacao = class
  private
    fBanco: Integer;
    fAgencia: String;
    fOrigem: String;
    fFormaPagto: String;
  public
    property Banco     : Integer read fBanco      write fBanco;
    property Agencia   : String  read fAgencia    write fAgencia;
    property Origem    : String  read fOrigem     write fOrigem;
    property FormaPagto: String  read fFormaPagto write fFormaPagto;
  end;

  { TACBrSacadoAvalista }
  TACBrSacadoAvalista = class
  private
    fTipoPessoa  : TACBrPessoa;

    fNomeAvalista: String;
    fCNPJCPF     : String;
    fLogradouro  : String;
    fNumero      : String;
    fComplemento : String;
    fBairro      : String;
    fCidade      : String;
    fUF          : String;
    fCEP         : String;
    fEmail       : String;
    fFone        : String;
    fInscricaoNr : String;
  public
    property Pessoa      : TACBrPessoa read fTipoPessoa  write fTipoPessoa;
    property NomeAvalista: String  read fNomeAvalista    write fNomeAvalista;
    property CNPJCPF     : String  read fCNPJCPF     write fCNPJCPF;
    property Logradouro  : String  read fLogradouro  write fLogradouro;
    property Numero      : String  read fNumero      write fNumero;
    property Complemento : String  read fComplemento write fComplemento;
    property Bairro      : String  read fBairro      write fBairro;
    property Cidade      : String  read fCidade      write fCidade;
    property UF          : String  read fUF          write fUF;
    property CEP         : String  read fCEP         write fCEP;
    property Email       : String  read fEmail       write fEmail;
    property Fone        : String  read fFone        write fFone;
    property InscricaoNr : String  read fInscricaoNr write fInscricaoNr;
  end;

  { TACBrSacado }
  TACBrSacado = class
  private
    fSacadoAvalista : TACBrSacadoAvalista;
    fTipoPessoa     : TACBrPessoa;
    fNomeSacado  : String;
    fCNPJCPF     : String;
    fLogradouro  : String;
    fNumero      : String;
    fComplemento : String;
    fBairro      : String;
    fCidade      : String;
    fUF          : String;
    fCEP         : String;
    fEmail       : String;
    fFone        : String;
    FMensagem    : String;
    function GetAvalista: String;
    procedure SetAvalista(const AValue: String);
    procedure SetMensagem(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    property Pessoa         : TACBrPessoa         read fTipoPessoa     write fTipoPessoa;
    property SacadoAvalista : TACBrSacadoAvalista read fSacadoAvalista write fSacadoAvalista;

    property Avalista    : String  read GetAvalista write SetAvalista;
    property NomeSacado  : String  read fNomeSacado  write fNomeSacado;
    property CNPJCPF     : String  read fCNPJCPF     write fCNPJCPF;
    property Logradouro  : String  read fLogradouro  write fLogradouro;
    property Numero      : String  read fNumero      write fNumero;
    property Complemento : String  read fComplemento write fComplemento;
    property Bairro      : String  read fBairro      write fBairro;
    property Cidade      : String  read fCidade      write fCidade;
    property UF          : String  read fUF          write fUF;
    property CEP         : String  read fCEP         write fCEP;
    property Email       : String  read fEmail       write fEmail;
    property Fone        : String  read fFone        write fFone;
    property Mensagem    : String  read FMensagem    write SetMensagem;
  end;

  { TACBrDadosNFe }
  TACBrDadosNFe = class
  private
    fChaveNFe: String;
    fEmissaoNFe: TDateTime;
    fNumNFe: String;
    fValorNFe: Currency;
  public

   property NumNFe     : String read fNumNFe write fNumNFe;
   property ValorNFe   : Currency read fValorNFe write fValorNFe;
   property EmissaoNFe : TDateTime read fEmissaoNFe write fEmissaoNFe;
   property ChaveNFe   : String read fChaveNFe write fChaveNFe;
  end;

  { TListadeNFes }
  TACBrListadeNFes = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TACBrDadosNFe);
    function  GetObject (Index: Integer): TACBrDadosNFe;
    procedure Insert (Index: Integer; Obj: TACBrDadosNFe);
  public
    function Add (Obj: TACBrDadosNFe): Integer;
    property Objects [Index: Integer]: TACBrDadosNFe
      read GetObject write SetObject; default;
  end;

  {TACBrBoletoPIXQRCode}
  TACBrBoletoPIXQRCode = class
  private
    Femv: String;
    Furl: String;
    FtxId: String;

    procedure Setemv(const Value: String);
    procedure SettxId(const Value: String);
    procedure Seturl(const Value: String);

  public
    constructor Create();
    destructor Destroy; override;
    property url : String read Furl write Seturl;
    property txId  : String read FtxId write SettxId;
    property emv   : String read Femv write Setemv;
    procedure PIXQRCodeDinamico(const AURL, ATXID: String; ATitulo : TACBrTitulo);
  end;

  { TACBrTitulo }
  TACBrTitulo = class
  private
    fInstrucao1        : String;
    fInstrucao2        : String;
    fInstrucao3        : String;
    fLocalPagamento    : String;
    fOcorrenciaOriginal: TACBrOcorrencia;
    fTipoDesconto      : TACBrTipoDesconto;
    fTipoDesconto2     : TACBrTipoDesconto;
    fTipoDesconto3     : TACBrTipoDesconto;
    fParcela           : Integer;
    fPercentualMulta   : Double;
    fMultaValorFixo    : Boolean;
    fSeuNumero         : String;
    fTipoDiasProtesto  : TACBrTipoDiasIntrucao;
    fTipoDiasNegativacao: TACBrTipoDiasIntrucao;
    fTipoImpressao     : TACBrTipoImpressao;
    fTotalParcelas     : Integer;
    fValorDescontoAntDia: Currency;
    fVencimento        : TDateTime;
    fDataDocumento     : TDateTime;
    fNumeroDocumento   : String;
    fEspecieDoc        : String;
    fAceite            : TACBrAceiteTitulo;
    fDataProcessamento : TDateTime;
    fNossoNumero       : String;
    fNossoNumeroCorrespondente: String;
    fUsoBanco          : String;
    fCarteira          : String;
    fEspecieMod        : String;
    fValorDocumento    : Currency;
    fMensagem          : TStrings;
    fInformativo       : TStrings;
    fInstrucoes        : TStrings;
    fSacado            : TACBrSacado;
    fLiquidacao        : TACBrTituloLiquidacao;
    fDetalhamento      : TStrings;
    fVerso             : Boolean;
    fArquivoLogoEmp    : String;
    fCompetencia       : string;

    fMotivoRejeicaoComando          : TStrings;
    fDescricaoMotivoRejeicaoComando : TStrings;

    fDataOcorrencia       : TDateTime;
    fDataCredito          : TDateTime;
    fDataAbatimento       : TDateTime;
    fDataDesconto         : TDateTime;
    fDataDesconto2        : TDateTime;
    fDataDesconto3        : TDateTime;
    fDataMoraJuros        : TDateTime;
    fDataMulta            : TDateTime;
    fDataProtesto         : TDateTime;
    fDiasDeProtesto       : Integer;
    fDataNegativacao      : TDateTime;
    fDiasDeNegativacao    : Integer;
    fDataBaixa            : TDateTime;
    fDataLimitePagto      : TDateTime;
    fValorDespesaCobranca : Currency;
    fValorAbatimento      : Currency;
    fValorDesconto        : Currency;
    fValorDesconto2       : Currency;
    fValorDesconto3       : Currency;
    fValorMoraJuros       : Currency;
    fValorIOF             : Currency;
    fValorOutrasDespesas  : Currency;
    fValorOutrosCreditos  : Currency;
    fValorRecebido        : Currency;
    fReferencia           : String;
    fVersao               : String;
    fACBrBoleto           : TACBrBoleto;
    fTextoLivre           : String;
    fCodigoMora           : String;
    fpLinhaDigitada       : String;
    fCodigoLiquidacao     : String;
    fCodigoLiquidacaoDescricao: String;
    fCarteiraEnvio        : TACBrCarteiraEnvio;
    fCodigoNegativacao    : TACBrCodigoNegativacao;
    fCodigoDesconto       : TACBrCodigoDesconto;
    fCodigoMoraJuros      : TACBrCodigoJuros;
    fCodigoMulta          : TACBrCodigoMulta;

    fCodigoGeracao        : String;
    fValorPago            : Currency;
    fCaracTitulo          : TACBrCaracTitulo;
    fListaDadosNFe        : TACBrListadeNFes;
    fTipoPagamento: TTipo_Pagamento;
    fQtdePagamentoParcial: Integer;
    fQtdeParcelas: Integer;
    fValorMinPagamento: Currency;
    fValorMaxPagamento: Currency;
    fPercentualMinPagamento: Currency;
    fPercentualMaxPagamento: Currency;
    fQrCode: TACBrBoletoPIXQRCode;
    fRetornoWeb: TACBrBoletoRetornoWS;
    fOrgaoNegativador:String;

    function GetQrCode: TACBrBoletoPIXQRCode;
    procedure SetCarteira(const AValue: String);
    procedure SetCodigoMora(const AValue: String);
    procedure SetDiasDeProtesto(AValue: Integer);
    procedure SetDiasDeNegativacao(AValue: Integer);
    procedure SetNossoNumero ( const AValue: String ) ;
    procedure SetParcela ( const AValue: Integer ) ;
    procedure SetTipoDiasProtesto(AValue: TACBrTipoDiasIntrucao);
    procedure SetTipoDiasNegativacao(AValue: TACBrTipoDiasIntrucao);
    procedure SetTotalParcelas ( const AValue: Integer );
    procedure SetCodigoGeracao (const AValue: String);
    procedure SetDataProtesto(AValue: TDateTime);
    procedure SetDataNegativacao(AValue: TDateTime);
    procedure SetVencimento(AValue: TDateTime);
    procedure setValorDocumento(const AValue: Currency);
    procedure AtualizaDadosProtesto();
    procedure AtualizaDadosNegativacao();
    procedure SetQrCode(const Value: TACBrBoletoPIXQRCode);
    procedure SetOrgaoNegativador(const Value: String);

   public
     constructor Create(ACBrBoleto:TACBrBoleto);
     destructor Destroy; override;
     procedure CarregaLogoEmp( const PictureLogo : TPicture );

     procedure Imprimir; overload;
     procedure Imprimir(AStream: TStream); overload;
     procedure GerarPDF; overload;
     procedure GerarPDF(AStream: TStream); overload;
     procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
      EnviaPDF: Boolean; sCC: TStrings = Nil; Anexos: TStrings = Nil);

     property ACBrBoleto        : TACBrBoleto read fACBrBoleto;
     property LocalPagamento    : String      read fLocalPagamento    write fLocalPagamento;
     property Vencimento        : TDateTime   read fVencimento        write SetVencimento;
     property DataDocumento     : TDateTime   read fDataDocumento     write fDataDocumento;
     property NumeroDocumento   : String      read fNumeroDocumento   write fNumeroDocumento ;
     property EspecieDoc        : String      read fEspecieDoc        write fEspecieDoc;
     property Aceite            : TACBrAceiteTitulo   read fAceite           write fAceite      default atNao;
     property DataProcessamento : TDateTime   read fDataProcessamento write fDataProcessamento;
     property NossoNumero       : String      read fNossoNumero       write SetNossoNumero;
     property NossoNumeroCorrespondente : String  read fNossoNumeroCorrespondente write fNossoNumeroCorrespondente;
     property UsoBanco          : String      read fUsoBanco          write fUsoBanco;
     property Carteira          : String      read fCarteira          write SetCarteira;
     property CarteiraEnvio     : TACBrCarteiraEnvio read fCarteiraEnvio write fCarteiraEnvio default tceCedente;
     property CodigoDesconto    : TACBrCodigoDesconto    read fCodigoDesconto    write fCodigoDesconto;
     property CodigoMoraJuros   : TACBrCodigoJuros       read fCodigoMoraJuros   write fCodigoMoraJuros default cjIsento;
     property CodigoMulta       : TACBrCodigoMulta       read fCodigoMulta       write fCodigoMulta;
     property CodigoNegativacao : TACBrCodigoNegativacao read fCodigoNegativacao write fCodigoNegativacao default cnNaoProtestar;

     property EspecieMod        : String      read fEspecieMod        write fEspecieMod;
     property ValorDocumento    : Currency    read fValorDocumento    write setValorDocumento;
     property Mensagem          : TStrings    read fMensagem          write fMensagem;
     property Informativo       : TStrings    read fInformativo       write fInformativo;
     property Instrucao1        : String      read fInstrucao1        write fInstrucao1;
     property Instrucao2        : String      read fInstrucao2        write fInstrucao2;
     property Instrucao3        : String      read fInstrucao3        write fInstrucao3;
     property Sacado            : TACBrSacado read fSacado            write fSacado;
     property Parcela           :Integer      read fParcela           write SetParcela default 1;
     property TotalParcelas     :Integer      read fTotalParcelas     write SetTotalParcelas default 1;
     property CodigoLiquidacao  : String      read fCodigoLiquidacao  write fCodigoLiquidacao;
     property CodigoLiquidacaoDescricao : String read fCodigoLiquidacaoDescricao write fCodigoLiquidacaoDescricao;
     property Detalhamento      : TStrings    read fDetalhamento      write fDetalhamento;
     property Verso             : Boolean     read fVerso             write fVerso default False;
     property ArquivoLogoEmp    : String      read fArquivoLogoEmp    write fArquivoLogoEmp;
     property Competencia       : string      read fCompetencia       write fCompetencia;

     property OcorrenciaOriginal : TACBrOcorrencia read  fOcorrenciaOriginal write fOcorrenciaOriginal;
     property TipoDesconto       : TACBrTipoDesconto read fTipoDesconto write fTipoDesconto;
     property TipoDesconto2      : TACBrTipoDesconto read fTipoDesconto2 write fTipoDesconto2;
     property TipoDesconto3      : TACBrTipoDesconto read fTipoDesconto3 write fTipoDesconto3;

     property MotivoRejeicaoComando          : TStrings    read fMotivoRejeicaoComando  write fMotivoRejeicaoComando;
     property DescricaoMotivoRejeicaoComando : TStrings    read fDescricaoMotivoRejeicaoComando  write fDescricaoMotivoRejeicaoComando;

     property DataOcorrencia                 : TDateTime read fDataOcorrencia    write fDataOcorrencia;
     property DataCredito                    : TDateTime read fDataCredito       write fDataCredito;
     property DataAbatimento                 : TDateTime read fDataAbatimento    write fDataAbatimento;
     property DataDesconto                   : TDateTime read fDataDesconto      write fDataDesconto;
     property DataDesconto2                  : TDateTime read fDataDesconto2     write fDataDesconto2;
     property DataDesconto3                  : TDateTime read fDataDesconto3     write fDataDesconto3;
     property DataMoraJuros                  : TDateTime read fDataMoraJuros     write fDataMoraJuros;
     property DataMulta                      : TDateTime read fDataMulta         write fDataMulta;
     property DataProtesto                   : TDateTime read fDataProtesto      write SetDataProtesto;
     property DiasDeProtesto                 : Integer   read fDiasDeProtesto    write SetDiasDeProtesto;
     property DataNegativacao                : TDateTime read fDataNegativacao   write SetDataNegativacao;
     property DiasDeNegativacao              : Integer   read fDiasDeNegativacao write SetDiasDeNegativacao;
     property OrgaoNegativador               : String    read fOrgaoNegativador  write SetOrgaoNegativador;
     property DataBaixa                      : TDateTime read fDataBaixa         write fDataBaixa;
     property DataLimitePagto                : TDateTime read fDataLimitePagto   write fDataLimitePagto;

     property ValorDespesaCobranca : Currency read fValorDespesaCobranca  write fValorDespesaCobranca;
     property ValorAbatimento      : Currency read fValorAbatimento       write fValorAbatimento;
     property ValorDesconto        : Currency read fValorDesconto         write fValorDesconto;
     property ValorDesconto2       : Currency read fValorDesconto2        write fValorDesconto2;
     property ValorDesconto3       : Currency read fValorDesconto3        write fValorDesconto3;
     property ValorMoraJuros       : Currency read fValorMoraJuros        write fValorMoraJuros;
     property ValorIOF             : Currency read fValorIOF              write fValorIOF;
     property ValorOutrasDespesas  : Currency read fValorOutrasDespesas   write fValorOutrasDespesas;
     property ValorOutrosCreditos  : Currency read fValorOutrosCreditos   write fValorOutrosCreditos;
     property ValorPago            : Currency read fValorPago             write fValorPago;
     property ValorRecebido        : Currency read fValorRecebido         write fValorRecebido;
     property Referencia           : String   read fReferencia            write fReferencia;
     property Versao               : String   read fVersao                write fVersao;
     property SeuNumero            : String   read fSeuNumero             write fSeuNumero;
     property PercentualMulta      : Double   read fPercentualMulta       write fPercentualMulta;
     property MultaValorFixo       : Boolean   read fMultaValorFixo       write fMultaValorFixo;
     property ValorDescontoAntDia  : Currency read fValorDescontoAntDia  write  fValorDescontoAntDia;
     property TextoLivre : String read fTextoLivre write fTextoLivre;
     property CodigoMora : String read fCodigoMora write SetCodigoMora;
     property TipoDiasProtesto     : TACBrTipoDiasIntrucao read fTipoDiasProtesto write SetTipoDiasProtesto;
     property TipoDiasNegativacao  : TACBrTipoDiasIntrucao read fTipoDiasNegativacao write SetTipoDiasNegativacao;
     property TipoImpressao        : TACBrTipoImpressao read fTipoImpressao write fTipoImpressao;
     property LinhaDigitada : String read fpLinhaDigitada;
     property CodigoGeracao: String read fCodigoGeracao write SetCodigoGeracao;
     property Liquidacao: TACBrTituloLiquidacao read fLiquidacao write fLiquidacao;
     property CaracTitulo: TACBrCaracTitulo read fCaracTitulo  write fCaracTitulo default tcSimples;

     property TipoPagamento: TTipo_Pagamento read fTipoPagamento write fTipoPagamento default tpNao_Aceita_Valor_Divergente;
     property QtdePagamentoParcial: integer read fQtdePagamentoParcial write fQtdePagamentoParcial;
     property QtdeParcelas: integer read fQtdeParcelas write fQtdeParcelas;
     property ValorMinPagamento: currency read fValorMinPagamento write fValorMinPagamento;
     property ValorMaxPagamento: currency read fValorMaxPagamento write fValorMaxPagamento;
     property PercentualMinPagamento: currency read fPercentualMinPagamento write fPercentualMinPagamento;
     property PercentualMaxPagamento: currency read fPercentualMaxPagamento write fPercentualMaxPagamento;
     property ListaDadosNFe : TACBrListadeNFes read fListaDadosNFe;
     property QrCode: TACBrBoletoPIXQRCode read GetQrCode write SetQrCode; // Utilizado somente em alguns bancos e com comunicação de API (uso manual por conta e risco da SwHouse)
     property RetornoWeb: TACBrBoletoRetornoWS read fRetornoWeb;

     function CriarNFeNaLista: TACBrDadosNFe;
   end;

  { TListadeBoletos }
  TListadeBoletos = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TACBrTitulo);
    function  GetObject (Index: Integer): TACBrTitulo;
    procedure Insert (Index: Integer; Obj: TACBrTitulo);
  public
    function Add (Obj: TACBrTitulo): Integer;
    property Objects [Index: Integer]: TACBrTitulo
      read GetObject write SetObject; default;
  end;

  {TACBrTipoOcorrenciaRemessa}
  TACBrOcorrenciaRemessa = Record
    Tipo     : TACBrTipoOcorrencia;
    Descricao: String;
  end;

  TACBrOcorrenciasRemessa =  Array of TACBrOcorrenciaRemessa;

  { TACBrBoleto }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBoleto = class( TACBrComponent )
  private
    fBanco: TACBrBanco;
    fACBrBoletoFC: TACBrBoletoFCClass;
    fDirArqRemessa: String;
    fDirArqRetorno: String;
    fLayoutRemessa: TACBrLayoutRemessa;
    fImprimirMensagemPadrao: boolean;
    fListadeBoletos : TListadeBoletos;
    fCedente        : TACBrCedente;
    FMAIL: TACBrMail;
    fNomeArqRemessa: String;
    fNomeArqRetorno: String;
    fNumeroArquivo : integer;     {Número seqüencial do arquivo remessa ou retorno}
    fDataArquivo : TDateTime;     {Data da geração do arquivo remessa ou retorno}
    fDataCreditoLanc : TDateTime; {Data de crédito dos lançamentos do arquivo retorno}
    fLeCedenteRetorno: boolean;
    fHomologacao: Boolean;
    fRemoveAcentosArqRemessa: Boolean;
    fLerNossoNumeroCompleto: Boolean;
    fConfiguracoes: TConfiguracoes;
    fListaConsultaRetornoWeb: TListaACBrBoletoRetornoWS;
    fPrefixArqRemessa : string;
    fOnAntesAutenticar:  TACBrWebServiceOnAntesAutenticar;
    fOnDepoisAutenticar: TACBrWebServiceOnDepoisAutenticar;

    procedure SetACBrBoletoFC(const Value: TACBrBoletoFCClass);
    procedure SetMAIL(AValue: TACBrMail);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CalcularValorDesconto(AValorDocumento, AValorDesconto : Double; ATipoDesconto : TACBrTipoDesconto):Double;
    function CalcularPercentualValor(AValorPercentual, AValorDocumento : Double) : Double;
    function GerarMensagemPadraoDesconto(const ATipoDesconto : TACBrTipoDesconto; AValorDesconto : Double; ATitulo : TACBrTitulo; ADataDesconto : TDateTime = 0): String;
    function GerarMensagemPadraoMulta(ATitulo: TACBrTitulo):String;
    function GerarMensagemPadraoJuros(ATitulo: TACBrTitulo):String;
    function GerarMensagemPadraoNegativacao(ATitulo: TACBrTitulo):String;
    function GerarMensagemPadraoProtesto(ATitulo: TACBrTitulo):String;
    function GerarMensagemPadraoAbatimento(ATitulo: TACBrTitulo):String;
    function GerarMensagemPadraoDataLimitePagamento(ATitulo: TACBrTitulo):String;

    function GetListaRetornoWeb(const Indice: Integer): TACBrBoletoRetornoWS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ListadeBoletos : TListadeBoletos read fListadeBoletos;
    property ListaRetornoWeb[const Indice: Integer]: TACBrBoletoRetornoWS read GetListaRetornoWeb;
    property ListaConsultaRetornoWeb: TListaACBrBoletoRetornoWS read fListaConsultaRetornoWeb;

    function CriarTituloNaLista: TACBrTitulo;
    function CriarRetornoWebNaLista: TACBrBoletoRetornoWS;
    function TotalListaRetornoWeb: Integer;

    procedure Imprimir; overload;
    procedure Imprimir(AIndice: Integer); overload;
    procedure Imprimir(AStream: TStream); overload;
    procedure Imprimir(AIndice: Integer; AStream: TStream); overload;
    procedure GerarPDF; overload;
    procedure GerarPDF(AIndex: Integer); overload;
    procedure GerarPDF(AStream: TStream); overload;
    procedure GerarPDF(AIndex: Integer; AStream: TStream); overload;
    procedure GerarHTML;
    procedure GerarJPG;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
      EnviaPDF: Boolean; sCC: TStrings = Nil; Anexos: TStrings = Nil);

    procedure AdicionarMensagensPadroes(Titulo : TACBrTitulo; AStringList: TStrings);

    function GerarRemessa(NumeroRemessa : Integer) : String;
    function GerarRemessaStream(NumeroRemessa : Integer; Stream:TStream) : String;
    procedure LerRetorno(AStream : TStream = Nil);


    procedure ChecarDadosObrigatorios;
    procedure EhObrigatorioAgencia;
    procedure EhObrigatorioAgenciaDV;
    procedure EhObrigatorioConta;
    procedure EhObrigatorioContaDV;
    procedure EhObrigatorioNomeBeneficiario;

    function EnviarBoleto: Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método Enviar' {$ENDIF};
    function Enviar: Boolean;

    function GetOcorrenciasRemessa() : TACBrOcorrenciasRemessa;
    function GetTipoCobranca(NumeroBanco: Integer; Carteira: String = ''): TACBrTipoCobranca;
    function LerArqIni(const AIniBoletos: String): Boolean;
    function LerConfiguracao(const AIniBoletos: String): Boolean;
    function GravarArqIni(DirIniRetorno: string; const NomeArquivo: String; const SomenteConfig:Boolean = false): String;
    function GravarConfiguracao(DirIniRetorno: string; const NomeArquivo: String): Boolean;

  published
    property MAIL  : TACBrMail read FMAIL write SetMAIL;

    property Homologacao    : Boolean            read fHomologacao            write fHomologacao default False;
    property Banco          : TACBrBanco         read fBanco                  write fBanco;
    property Cedente        : TACBrCedente       read fCedente                write fCedente ;
    property PrefixArqRemessa : String           read fPrefixArqRemessa       write fPrefixArqRemessa;
    property NomeArqRemessa : String             read fNomeArqRemessa         write fNomeArqRemessa;
    property DirArqRemessa  : String             read fDirArqRemessa          write fDirArqRemessa;
    property NomeArqRetorno : String             read fNomeArqRetorno         write fNomeArqRetorno;
    property DirArqRetorno  : String             read fDirArqRetorno          write fDirArqRetorno;
    property NumeroArquivo  : integer            read fNumeroArquivo          write fNumeroArquivo;
    property DataArquivo    : TDateTime          read fDataArquivo            write fDataArquivo;
    property DataCreditoLanc: TDateTime          read fDataCreditoLanc        write fDataCreditoLanc;
    property LeCedenteRetorno :boolean           read fLeCedenteRetorno       write fLeCedenteRetorno default false;
    property LayoutRemessa  : TACBrLayoutRemessa read fLayoutRemessa          write fLayoutRemessa default c400;
    property ImprimirMensagemPadrao : Boolean    read fImprimirMensagemPadrao write fImprimirMensagemPadrao default True;
    property ACBrBoletoFC : TACBrBoletoFCClass   read fACBrBoletoFC           write SetACBrBoletoFC;
    property RemoveAcentosArqRemessa: Boolean    read fRemoveAcentosArqRemessa write fRemoveAcentosArqRemessa default False;
    property LerNossoNumeroCompleto : Boolean    read fLerNossoNumeroCompleto write fLerNossoNumeroCompleto default False;
    property Configuracoes: TConfiguracoes       read fConfiguracoes          write fConfiguracoes;
    property OnAntesAutenticar : TACBrWebServiceOnAntesAutenticar  read fOnAntesAutenticar  write fOnAntesAutenticar;
    property OnDepoisAutenticar: TACBrWebServiceOnDepoisAutenticar read fOnDepoisAutenticar write fOnDepoisAutenticar;
  end;

  {TACBrBoletoFCClass}
  TACBrBoletoFCFiltro = (fiNenhum, fiPDF, fiHTML, fiJPG) ;

  TACBrBoletoFCOnObterLogo = procedure( const PictureLogo : TPicture; const NumeroBanco: Integer ) of object ;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBoletoFCClass = class(TACBrComponent)
  private
    fDirLogo         : String;
    fFiltro          : TACBrBoletoFCFiltro;
    fLayOut          : TACBrBolLayOut;
    fMostrarPreview  : Boolean;
    fMostrarProgresso: Boolean;
    fMostrarSetup    : Boolean;
    fNomeArquivo     : String;
    fPathNomeArquivo : String;
    fNumCopias       : Integer;
    fPrinterName     : String;
    fOnObterLogo     : TACBrBoletoFCOnObterLogo ;
    fSoftwareHouse   : String;
    FPdfSenha        : string;
    FTituloPreview   : string;
    FAlterarEscalaPadrao: Boolean;
    FNovaEscala: Integer;
    FIndiceImprimirIndividual: Integer;
    FCalcularNomeArquivoPDFIndividual: Boolean;
    function ComponentStateDesigning: Boolean;
    function GetArquivoLogo: String;
    function GetDirLogo: String;
    function GetNomeArquivo: String;
    function GetIndiceImprimirIndividual: Integer;
    function GetNomeArquivoPdfIndividual(const ANomeArquivo: String; const AIndex: Integer): String;
    procedure SetACBrBoleto(const Value: TACBrBoleto);
    procedure SetDirLogo(const AValue: String);
    procedure SetNomeArquivo(const AValue: String);
    procedure SetPdfSenha(const Value: string);
    procedure SetTituloPreview(const Value: string);
    procedure SetIndiceImprimirIndividual(const Value: Integer);

  protected
    fACBrBoleto : TACBrBoleto;
    procedure SetNumCopias(AValue: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function TituloRelatorio: String;
    function DefineAceiteImpressao(const ACBrTitulo: TACBrTitulo): String;
  public
    Constructor Create(AOwner: TComponent); override;

    procedure Imprimir; overload; virtual;
    procedure Imprimir(AStream: TStream); overload; virtual;
    procedure GerarPDF; overload; virtual;
    procedure GerarPDF(AIndex: Integer); overload; virtual;
    procedure GerarPDF(AStream: TStream); overload; virtual;
    procedure GerarPDF(AIndex: Integer; AStream: TStream); overload; virtual;
    procedure GerarHTML; virtual;
    procedure GerarJPG; virtual;

    procedure CarregaLogo( const PictureLogo : TPicture; const NumeroBanco: Integer ) ;

    property ArquivoLogo : String read GetArquivoLogo;
    property IndiceImprimirIndividual: Integer read GetIndiceImprimirIndividual  write SetIndiceImprimirIndividual   default -1;

  published
    property OnObterLogo     : TACBrBoletoFCOnObterLogo read fOnObterLogo write fOnObterLogo ;
    property ACBrBoleto      : TACBrBoleto     read fACBrBoleto       write SetACBrBoleto stored False;
    property LayOut          : TACBrBolLayOut  read fLayOut           write fLayOut           default lPadrao;
    property MostrarPreview  : Boolean         read fMostrarPreview   write fMostrarPreview   default True ;
    property MostrarSetup    : Boolean         read fMostrarSetup     write fMostrarSetup     default True ;
    property MostrarProgresso: Boolean         read fMostrarProgresso write fMostrarProgresso default True ;
    property NumCopias       : Integer         read fNumCopias        write SetNumCopias      default 1;
    property Filtro          : TACBrBoletoFCFiltro read fFiltro       write fFiltro           default fiNenhum ;
    property SoftwareHouse   : String          read fSoftwareHouse    write fSoftwareHouse;
    property PrinterName     : String          read fPrinterName      write fPrinterName;
    property DirLogo         : String          read GetDirLogo        write SetDirLogo;
    property NomeArquivo     : String          read GetNomeArquivo    write SetNomeArquivo ;
    property CalcularNomeArquivoPDFIndividual: Boolean        read FCalcularNomeArquivoPDFIndividual write FCalcularNomeArquivoPDFIndividual default True ;
    property PdfSenha        : string          read FPdfSenha         write SetPdfSenha;
    property AlterarEscalaPadrao: Boolean      read FAlterarEscalaPadrao write FAlterarEscalaPadrao default False;
    property NovaEscala      : Integer         read FNovaEscala       write FNovaEscala        default 96;
    property TituloPreview   : string          read FTituloPreview    write SetTituloPreview;
  end;

implementation

Uses {$IFNDEF NOGUI}Forms,{$ENDIF} Math, dateutils, strutils,  ACBrBoletoWS,
     ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math,ACBrUtil.XMLHTML,
     ACBrUtil.FilesIO,
     ACBrBancoBradesco,
     ACBrBancoBrasil,
     ACBrBancoAmazonia,
     ACBrBancoBanestes,
     ACBrBancoItau,
     ACBrBancoSicredi,
     ACBrBancoMercantil,
     ACBrBancoCaixa,
     ACBrBancoBanrisul,
     ACBrBancoSantander,
     ACBrBancoBancoob,
     ACBrBancoCaixaSICOB,
     ACBrBancoHSBC,
     ACBrBancoNordeste ,
     ACBrBancoBRB,
     ACBrBancoBic,
     ACBrBancoBradescoSICOOB,
     ACBrBancoSafra,
     ACBrBancoSafraBradesco,
     ACBrBancoCecred,
     ACBrBancoBrasilSicoob,
     ACBrUniprime,
     ACBrBancoUnicredRS,
     ACBrBancoBanese,
     ACBrBancoCredisis,
     ACBrBancoUnicredES,
     ACBrBancoCresolSCRS,
     ACBrBancoCitiBank,
     ACBrBancoABCBrasil,
     ACBrBancoDaycoval,
     ACBrUniprimeNortePR,
     ACBrBancoPine,
     ACBrBancoPineBradesco,
     ACBrBancoUnicredSC,
     ACBrBancoAlfa,
     ACBrBancoCresol,
     ACBrBancoBradescoMoneyPlus,
     ACBrBancoC6,
     ACBrBancoRendimento,
     ACBrBancoInter,
     ACBrBancoSofisaSantander,
     ACBrBancoBS2,
     ACBrBancoPenseBank,
     ACBrBancoBTGPactual,
     ACBrBancoOriginal,
     ACBrBancoVotorantim,
     ACBrBancoPefisa,
     ACBrBancoFibra,
     ACBrBancoSofisaItau,
     ACBrBancoIndustrialBrasil, 
     ACBrBancoAthenaBradesco;

{$IFNDEF FPC}
   {$R ACBrBoleto.dcr}
{$ENDIF}

{ TACBrBoletoWSFiltroConsulta }
procedure TACBrBoletoWSFiltroConsulta.SetContaCaucao(const Value: Integer);
begin
  FContaCaucao:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetCnpjCpfPagador(const Value: string);
begin
  FCnpjCpfPagador:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetDataVencimento(
  const Value: TACBrDataPeriodo);
begin
  FDataVencimento:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetDataRegistro(
  const Value: TACBrDataPeriodo);
begin
  FDataRegistro:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetDataMovimento(
  const Value: TACBrDataPeriodo);
begin
  FDataMovimento:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetIndicadorSituacaoBoleto(
  const Value: TACBrIndicadorSituacaoBoleto);
begin
  FIndicadorSituacaoBoleto:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetCodigoEstadoTituloCobranca(
  const Value: Integer);
begin
  FCodigoEstadoTituloCobranca:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetBoletoVencido(
  const Value: TACBrIndicadorBoletoVencido);
begin
  FBoletoVencido:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetIndiceContinuidade(
  const Value: Extended);
begin
  FIndiceContinuidade:= Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetModalidadeCobranca(const Value: integer);
begin
  FModalidadeCobrancao:= Value;
end;

procedure TACBrTitulo.SetOrgaoNegativador(const Value: String);
begin
  ForgaoNegativador := Value;
end;

procedure TACBrBoletoWSFiltroConsulta.SetCarteira(const Value: Integer);
begin
  FCarteira:= Value;
end;

constructor TACBrBoletoWSFiltroConsulta.Create();
begin
  FContaCaucao := 0;
  FCnpjCpfPagador := '';
  FDataVencimento := TACBrDataPeriodo.Create();
  FDataRegistro := TACBrDataPeriodo.Create();
  FDataMovimento := TACBrDataPeriodo.Create();
  FIndicadorSituacaoBoleto := isbNenhum;
  FCodigoEstadoTituloCobranca := 0;
  FBoletoVencido := ibvNenhum;
  FIndiceContinuidade := 0;
  FModalidadeCobrancao:= 0;
  FCarteira:= 0;
end;

destructor TACBrBoletoWSFiltroConsulta.Destroy;
begin
  FDataVencimento.Free;
  FDataRegistro.Free;
  FDataMovimento.Free;

  inherited Destroy;
end;

procedure TACBrBoletoWSFiltroConsulta.Clear;
begin
  FDataVencimento.FDataInicio := 0;
  FDataVencimento.FDataFinal  := 0;
  FDataRegistro.FDataInicio   := 0;
  FDataRegistro.FDataFinal    := 0;
  FDataMovimento.FDataInicio  := 0;
  FDataMovimento.FDataFinal   := 0;
  FContaCaucao                := 0;
  FCnpjCpfPagador             := '';
  FIndicadorSituacaoBoleto    := isbNenhum;
  FCodigoEstadoTituloCobranca := 0;
  FBoletoVencido              := ibvNenhum;
  FIndiceContinuidade         := 0;
  FModalidadeCobrancao        := 0;
  FCarteira                   := 0;
end;

{ TACBrDataPeriodo }
procedure TACBrDataPeriodo.SetDataInicio(const Value: TDateTime);
begin
  FDataInicio:= Value;
end;

procedure TACBrDataPeriodo.SetDataFinal(const Value: TDateTime);
begin
  FDataFinal:= Value;
end;

constructor TACBrDataPeriodo.Create();
begin
  FDataInicio:= 0;
  FDataFinal := 0;
end;

{ TACBrBoletoPIXQRCode }
procedure TACBrBoletoPIXQRCode.Setemv(const Value: String);
begin
  Femv:= Value;
end;

procedure TACBrBoletoPIXQRCode.SettxId(const Value: String);
begin
  FtxId:= Value;
end;

procedure TACBrBoletoPIXQRCode.Seturl(const Value: String);
begin
  Furl:= Value;
end;

constructor TACBrBoletoPIXQRCode.Create();
begin
  Femv:= '';
  Furl:= '';
  FtxId:= '';
end;

destructor TACBrBoletoPIXQRCode.Destroy;
begin
  inherited;
end;

procedure TACBrBoletoPIXQRCode.PIXQRCodeDinamico(const AURL, ATXID: String; ATitulo : TACBrTitulo);
var
  LEMV : TACBrPIXQRCodeDinamico;
begin
  if (EstaVazio(AURL) or EstaVazio(ATXID)) then
    raise Exception.Create(ACBrStr('URL e TXID é obrigatório!'));
  LEMV := TACBrPIXQRCodeDinamico.Create;
  try
    LEMV.IgnoreErrors := True;
    LEMV.MerchantName := ATitulo.ACBrBoleto.Cedente.Nome;
    LEMV.MerchantCity := ATitulo.ACBrBoleto.Cedente.Cidade;
    LEMV.PostalCode   := Poem_Zeros(ATitulo.ACBrBoleto.Cedente.CEP,8);
    LEMV.URL          := AURL;
    //LEMV.TxId         := ATXID;
    Seturl(AURL);
    SettxId(ATXID);
    Setemv(LEMV.AsString);
  finally
    LEMV.Free;
  end;
end;

{ TACBrArquivos }
constructor TACBrArquivos.Create;
begin
  fLogRegistro:= False;
  fPathGravarRegistro:= '';
  fOnGravarLog:= Nil;
end;

procedure TACBrArquivos.Assign(Source: TPersistent);
begin
  if Source is TACBrArquivos then
  begin
    fLogRegistro := TACBrArquivos(Source).LogRegistro;
    fPathGravarRegistro := TACBrArquivos(Source).PathGravarRegistro;
  end
  else
    inherited Assign(Source);
end;

{ TConfiguracoes }
constructor TConfiguracoes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fArquivos := TACBrArquivos.Create;
  fWebService := TACBrWebService.Create(Self);
end;

destructor TConfiguracoes.Destroy;
begin
  fArquivos.Free;
  fWebService.Free;
  inherited Destroy;
end;

{ TListadeNFes }
function TACBrListadeNFes.GetObject(Index: Integer): TACBrDadosNFe;
begin
   Result := inherited GetItem(Index) as TACBrDadosNFe ;
end;

procedure TACBrListadeNFes.SetObject(Index: Integer; Item: TACBrDadosNFe);
begin
   inherited SetItem (Index, Item) ;
end;

procedure TACBrListadeNFes.Insert(Index: Integer; Obj: TACBrDadosNFe);
begin
   inherited Insert(Index, Obj);
end;

function TACBrListadeNFes.Add(Obj: TACBrDadosNFe): Integer;
begin
   Result := inherited Add(Obj) ;
end;

function TACBrBancoClass.TipoDescontoToString(const AValue: TACBrTipoDesconto):string;
begin
  Result := '0';
  case AValue of
     tdNaoConcederDesconto                   : Result := '0';
     tdValorFixoAteDataInformada             : Result := '1';
     tdPercentualAteDataInformada            : Result := '2';
     tdValorAntecipacaoDiaCorrido            : Result := '3';
     tdValorAntecipacaoDiaUtil               : Result := '4';
     tdPercentualSobreValorNominalDiaCorrido : Result := '5';
     tdPercentualSobreValorNominalDiaUtil    : Result := '6';
     tdCancelamentoDesconto                  : Result := '7';
  end;
end;

{ TACBrWebService }
constructor TACBrWebService.Create(AOwner: TComponent);
begin
  inherited Create;
  fAmbiente := taHomologacao;
  fOperacao := tpInclui;
  fVersaoDF := '1.2';
  SSLHttpLib:= httpOpenSSL;
  FBoletoWSConsulta := TACBrBoletoWSFiltroConsulta.Create();

end;

destructor TACBrWebService.Destroy;
begin
  FBoletoWSConsulta.Free;
  inherited Destroy;
end;

function TACBrWebService.Enviar: Boolean;
begin
  Raise Exception.Create(ACBrStr('Método Enviar não ' +
            'implementado no ACBrBoleto!'));
end;

procedure TACBrWebService.SetCertificado(const Value: AnsiString);
begin
  FCertificado := Value;
  FArquivoCRT := EmptyStr;
end;

procedure TACBrWebService.SetChavePrivada(const Value: AnsiString);
begin
  FChavePrivada := Value;
  FArquivoKEY := EmptyStr;
end;

procedure TACBrWebService.SetWSBoletoConsulta(const Value: TACBrBoletoWSFiltroConsulta);
begin
  FBoletoWSConsulta := Value;
end;

{ TACBrCedenteWS }
constructor TACBrCedenteWS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FClientID := '';
  FClientSecret := '';
  FKeyUser := '';
end;

procedure TACBrCedenteWS.SetClientID(const Value: string);
begin
   if FClientID = Value then
     Exit;
   FClientID := Value;
end;

procedure TACBrCedenteWS.SetClientSecret(const Value: string);
begin
   if FClientSecret = Value then
     Exit;
   FClientSecret := Value;
end;

procedure TACBrCedenteWS.SetIndicadorPix(const Value: boolean);
begin
  FIndicadorPix := Value;
end;

procedure TACBrCedenteWS.SetKeyUser(const Value: string);
begin
   if FKeyUser = Value then
     Exit;
   FKeyUser := Value;
end;

procedure TACBrCedenteWS.SetScope(const Value: string);
begin
   if FScope = Value then
     Exit;
   FScope := Value;
end;

{ TACBrCedente }
constructor TACBrCedente.Create( AOwner : TComponent );
begin
  inherited Create(AOwner);

  fNomeCedente   := '';
  fAgencia       := '';
  fAgenciaDigito := '';
  fConta         := '';
  fContaDigito   := '';
  fModalidade    := '';
  fConvenio      := '';
  fCNPJCPF       := '';
  fDigitoVerificadorAgenciaConta:= '';
  fResponEmissao := tbCliEmite;
  fIdentDistribuicao := tbClienteDistribui;
  fCaracTitulo   := tcSimples;
  fTipoInscricao := pJuridica;
  fAcbrBoleto    := TACBrBoleto(AOwner);
  fTipoDocumento := Tradicional;

  fCedenteWS := TACBrCedenteWS.Create(self);
  fCedenteWS.Name := 'CedenteWS';
  
  fPIX            := TACBrBoletoChavePIX.Create(Self);
  fPIX.Name       := 'PIX';

  {$IFDEF COMPILER6_UP}
    fCedenteWS.SetSubComponent(True);
    fPIX.SetSubComponent(True);
  {$ENDIF}
end;

procedure TACBrCedente.SetCNPJCPF ( const AValue: String ) ;
var
  ACBrVal: TACBrValidador;
  ADocto: String;
begin
   if fCNPJCPF = AValue then
     Exit;

   ADocto := OnlyNumber(AValue);
   if EstaVazio(ADocto) then
   begin
      fCNPJCPF:= ADocto;
      Exit;
   end;

   ACBrVal := TACBrValidador.Create(Self);
   try
     with ACBrVal do
     begin
       if TipoInscricao = pFisica then
       begin
         TipoDocto := docCPF;
         Documento := RightStr(ADocto,11);
       end
       else
       begin
         TipoDocto := docCNPJ;
         Documento := RightStr(ADocto,14);
       end;

       IgnorarChar := './-';
       RaiseExcept := True;
       Validar;    // Dispara Exception se Documento estiver errado

       fCNPJCPF := Formatar;
     end;
   finally
     ACBrVal.Free;
   end;
end;

procedure TACBrCedente.SetConta(const AValue: String);
var
  aConta: Integer;
begin
   if fConta = AValue then
      exit;

   fConta:= AValue;
   aConta:= StrToIntDef(trim(AValue),0);

   if aConta = 0 then
      exit;

   fConta:= IntToStrZero(aConta, ACBrBoleto.Banco.TamanhoConta );
end;

procedure TACBrCedente.SetAgencia(const AValue: String);
var  aAgencia: Integer;
begin
   if fAgencia = AValue then
      exit;

   fAgencia:= AValue;

   aAgencia:= StrToIntDef(trim(AValue),0);

   if aAgencia = 0 then
      exit;

   fAgencia:= IntToStrZero(aAgencia, ACBrBoleto.Banco.TamanhoAgencia );
end;

procedure TACBrCedente.SetTipoInscricao ( const AValue: TACBrPessoaCedente ) ;
begin
   if fTipoInscricao = AValue then
      exit;

   fTipoInscricao := AValue;
end;

{ TACBrSacado }
constructor TACBrSacado.Create;
begin
   fSacadoAvalista := TACBrSacadoAvalista.Create;
end;

destructor TACBrSacado.Destroy;
begin
   fSacadoAvalista.Free;
  inherited;
end;

function TACBrSacado.GetAvalista: String;
begin
  Result:= Self.SacadoAvalista.NomeAvalista;
end;

procedure TACBrSacado.SetAvalista(const AValue: String);
begin
   if Self.SacadoAvalista.NomeAvalista = AValue then
     Exit;

   Self.SacadoAvalista.NomeAvalista:= AValue;
end;

procedure TACBrSacado.SetMensagem(const AValue: String);
begin
  FMensagem := AValue;
end;

{ TACBrTitulo }
constructor TACBrTitulo.Create(ACBrBoleto:TACBrBoleto);
begin
  inherited Create;

  fACBrBoleto        := ACBrBoleto;
  fLocalPagamento    := ACBrBoleto.Banco.LocalPagamento;
  fVencimento        := 0;
  fDataDocumento     := 0;
  fNumeroDocumento   := '';
  fEspecieDoc        := 'DM';
  fAceite            := atNao;
  fDataProcessamento := now;
  fNossoNumero       := '';
  fNossoNumeroCorrespondente:= '';
  fUsoBanco          := '';
  fCarteira          := '';
  fEspecieMod        := '';
  fValorDocumento    := 0;
  fInstrucao1        := '';
  fInstrucao2        := '';
  fInstrucao3        := '';
  fMensagem          := TStringList.Create;
  fDetalhamento      := TStringList.Create;
  fInformativo       := TStringList.Create;
  fInstrucoes        := TStringList.Create;
  fSacado            := TACBrSacado.Create;
  fLiquidacao        := TACBrTituloLiquidacao.Create;

  fOcorrenciaOriginal:= TACBrOcorrencia.Create(Self);
  fMotivoRejeicaoComando          := TStringList.Create;
  fDescricaoMotivoRejeicaoComando := TStringList.Create;

  fDataOcorrencia       := 0;
  fDataCredito          := 0;
  fDataAbatimento       := 0;
  fDataDesconto         := 0;
  fDataDesconto2        := 0;
  fDataDesconto3        := 0;
  fDataMoraJuros        := 0;
  fDataMulta            := 0;
  fDataProtesto         := 0;
  fDiasDeProtesto       := 0;
  fDataNegativacao      := 0;
  fDiasDeNegativacao    := 0;
  fDataBaixa            := 0;
  fDataLimitePagto      := 0;
  fValorDespesaCobranca := 0;
  fValorAbatimento      := 0;
  fValorDesconto        := 0;
  fValorDesconto2       := 0;
  fValorDesconto3       := 0;
  fValorMoraJuros       := 0;
  fValorIOF             := 0;
  fValorOutrasDespesas  := 0;
  fValorOutrosCreditos  := 0;
  fValorRecebido        := 0;
  fValorDescontoAntDia  := 0;
  fPercentualMulta      := 0;
  fMultaValorFixo       := false;
  fReferencia           := '';
  fVersao               := '';
  fTipoImpressao        := tipNormal;
  fTipoDesconto         := tdNaoConcederDesconto ;
  fTipoDesconto2        := tdNaoConcederDesconto ;
  fTipoDesconto3        := tdNaoConcederDesconto ;
  fCodigoMora    := '';
  fCodigoGeracao := '2';
  fCaracTitulo   := fACBrBoleto.Cedente.CaracTitulo;

   if ACBrBoleto.Cedente.ResponEmissao = tbCliEmite then
     fCarteiraEnvio := tceCedente
   else
     fCarteiraEnvio := tceBanco;

   fListaDadosNFe := TACBrListadeNFes.Create(true);
   fQrCode := TACBrBoletoPIXQRCode.Create();
   fRetornoWeb := TACBrBoletoRetornoWS.Create;
end;

destructor TACBrTitulo.Destroy;
begin
  fRetornoWeb.Free;
  fMensagem.Free;
  fDetalhamento.Free;
  fInformativo.Free;
  fSacado.Free;
  fLiquidacao.Free;
  fInstrucoes.Free;
  fOcorrenciaOriginal.Free;
  fMotivoRejeicaoComando.Free;
  fDescricaoMotivoRejeicaoComando.Free;
  fListaDadosNFe.Free;
  fQrCode.Free;
  inherited;
end;

procedure TACBrTitulo.SetNossoNumero ( const AValue: String ) ;
var
   wTamNossoNumero: Integer;
   wNossoNumero: String;
begin
   wNossoNumero:= OnlyNumber(AValue);
   with ACBrBoleto.Banco do
   begin
      wTamNossoNumero:= CalcularTamMaximoNossoNumero(Carteira, wNossoNumero,
                                                     ACBrBoleto.Cedente.Convenio);

      if Length(trim(wNossoNumero)) > wTamNossoNumero then
         raise Exception.Create( ACBrStr('Tamanho Máximo do Nosso Número é: '+ IntToStr(wTamNossoNumero) ));

      fNossoNumero := PadLeft(wNossoNumero,wTamNossoNumero,'0');
   end;
end;

function TACBrTitulo.GetQrCode: TACBrBoletoPIXQRCode;
begin
  if EstaVazio(fQrCode.emv) then
  begin
    fQrCode.emv:= fRetornoWeb.DadosRet.TituloRet.EMV;
    fQrCode.url:= fRetornoWeb.DadosRet.TituloRet.URL;
    fQrCode.txId:= fRetornoWeb.DadosRet.TituloRet.TxId;
    Result := fQrCode;
  end
  else
    Result := fQrCode;

end;

procedure TACBrTitulo.SetCarteira(const AValue: String);
var
  aCarteira: Integer;
begin
   if fCarteira = AValue then
      exit;

   with ACBrBoleto.Banco do
   begin
      aCarteira:= StrToIntDef(trim(AValue),0);

      fCarteira:=  AValue;

      if aCarteira < 1 then
         exit;

      fCarteira:= IntToStrZero(aCarteira,TamanhoCarteira);

   end;
end;

procedure TACBrTitulo.SetCodigoMora(const AValue: String);
begin
  if fCodigoMora = AValue then
      exit;

  if Pos(AValue,ACBrBoleto.Banco.CodigosMoraAceitos) = 0 then
     raise Exception.Create( ACBrStr('Código de Mora/Juros informado não é permitido ' +
                                     'para este banco!') );

  fCodigoMora := AValue;
end;

procedure TACBrTitulo.SetDiasDeProtesto(AValue: Integer);
begin
  if (fDiasDeProtesto = AValue) then
    Exit;

  fDiasDeProtesto := AValue;
  fDataProtesto := 0;
  AtualizaDadosProtesto();
end;

procedure TACBrTitulo.SetDiasDeNegativacao(AValue: Integer);
begin
  if (fDiasDeNegativacao = AValue) then
    Exit;

  fDiasDeNegativacao := AValue;
  fDataNegativacao := 0;
  AtualizaDadosNegativacao();
end;

procedure TACBrTitulo.SetCodigoGeracao(const AValue: String);
begin
  if fCodigoGeracao = AValue then
    Exit;

  if Pos(AValue,ACBrBoleto.Banco.CodigosGeracaoAceitos) = 0 then
     raise Exception.Create( ACBrStr('Código de Geração Inválido!') );

  fCodigoGeracao := AValue;
end;

procedure TACBrTitulo.SetDataProtesto(AValue: TDateTime);
begin
  if (fDataProtesto = AValue) then
    Exit;

   if (fTipoDiasProtesto = diUteis) then
     fDataProtesto:= IncWorkingDay(AValue,0)
   else
     fDataProtesto := Avalue;

  fDiasDeProtesto := 0;
  AtualizaDadosProtesto();
end;

procedure TACBrTitulo.SetDataNegativacao(AValue: TDateTime);
begin
  if (fDataNegativacao = AValue) then
    Exit;

   if (fTipoDiasNegativacao = diUteis) then
     fDataNegativacao:= IncWorkingDay(AValue,0)
   else
     fDataNegativacao := Avalue;

  fDiasDeNegativacao := 0;
  AtualizaDadosNegativacao();
end;

procedure TACBrTitulo.SetVencimento(AValue: TDateTime);
begin
  if (fVencimento = AValue) then
    Exit;

  fVencimento := AValue;
  AtualizaDadosProtesto();
  AtualizaDadosNegativacao();
end;

procedure TACBrTitulo.SetTipoDiasProtesto(AValue: TACBrTipoDiasIntrucao);
begin
  if fTipoDiasProtesto = AValue then
    Exit;

  fTipoDiasProtesto := AValue;
  if fDiasDeProtesto > 0 then
    fDataProtesto := 0;

  AtualizaDadosProtesto();
end;

procedure TACBrTitulo.SetTipoDiasNegativacao(AValue: TACBrTipoDiasIntrucao);
begin
  if fTipoDiasNegativacao = AValue then
    Exit;

  fTipoDiasNegativacao := AValue;
  if fDiasDeNegativacao > 0 then
    fDataNegativacao := 0;

  AtualizaDadosNegativacao();
end;

procedure TACBrTitulo.AtualizaDadosProtesto();
begin
  if fVencimento <= 0 then
    Exit;

  if (fDataProtesto > 0) then
  begin
    if (fTipoDiasProtesto = diUteis) then
      fDiasDeProtesto := WorkingDaysBetween(fVencimento, fDataProtesto)
    else
      fDiasDeProtesto := DaysBetween(fVencimento, fDataProtesto);
  end
  else if (fDiasDeProtesto > 0) then
  begin
    if (fTipoDiasProtesto = diUteis) then
      fDataProtesto := IncWorkingDay(fVencimento,fDiasDeProtesto)
    else
      fDataProtesto := IncDay(fVencimento,fDiasDeProtesto);
  end;
end;

procedure TACBrTitulo.AtualizaDadosNegativacao();
begin
  if fVencimento <= 0 then
    Exit;

  if (fDataNegativacao > 0) then
  begin
    if (fTipoDiasNegativacao = diUteis) then
      fDiasDeNegativacao := WorkingDaysBetween(fVencimento, fDataNegativacao)
    else
      fDiasDeNegativacao := DaysBetween(fVencimento, fDataNegativacao);
  end
  else if (fDiasDeNegativacao > 0) then
  begin
    if (fTipoDiasNegativacao = diUteis) then
      fDataNegativacao := IncWorkingDay(fVencimento,fDiasDeNegativacao)
    else
      fDataNegativacao := IncDay(fVencimento,fDiasDeNegativacao);
  end;
end;

procedure TACBrTitulo.SetParcela ( const AValue: Integer ) ;
begin
  if Assigned(ACBrBoleto.ACBrBoletoFC) then
    if (AValue > TotalParcelas) and (ACBrBoleto.ACBrBoletoFC.LayOut = lCarne) then
       raise Exception.Create( ACBrStr('Numero da Parcela Atual deve ser menor ' +
                                       'que o Total de Parcelas do Carnê') );
   fParcela := AValue;
end;

procedure TACBrTitulo.SetQrCode(const Value: TACBrBoletoPIXQRCode);
begin
  fQrCode := Value;
end;

procedure TACBrTitulo.SetTotalParcelas ( const AValue: Integer ) ;
begin
  if Assigned(ACBrBoleto.ACBrBoletoFC) then
    if (AValue < Parcela) and (ACBrBoleto.ACBrBoletoFC.LayOut = lCarne) then
      raise Exception.Create( ACBrStr('Numero da Parcela Atual deve ser menor ou igual ' +
                                      'o Total de Parcelas do Carnê') );
   fTotalParcelas := AValue;
end;

procedure TACBrTitulo.CarregaLogoEmp(const PictureLogo: TPicture);
begin
  if FileExists( ArquivoLogoEmp ) then
    PictureLogo.LoadFromFile( ArquivoLogoEmp );
end;

procedure TACBrTitulo.Imprimir;
begin
  if not Assigned(ACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) );

  if (fACBrBoleto.ListadeBoletos.Count <= 0)  then
    raise Exception.Create( ACBrStr('Nenhum Título encontrado na Lista de Boletos' ) ) ;

  ACBrBoleto.Imprimir(fACBrBoleto.ListadeBoletos.IndexOf(Self));
end;

procedure TACBrTitulo.Imprimir(AStream: TStream);
begin
  if not Assigned(ACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) );

  if (fACBrBoleto.ListadeBoletos.Count <= 0)  then
    raise Exception.Create( ACBrStr('Nenhum Título encontrado na Lista de Boletos' ) ) ;

  ACBrBoleto.Imprimir(fACBrBoleto.ListadeBoletos.IndexOf(Self), AStream);
end;

procedure TACBrTitulo.GerarPDF;
begin
  if not Assigned(ACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

  if (fACBrBoleto.ListadeBoletos.Count <= 0)  then
    raise Exception.Create( ACBrStr('Nenhum Título encontrado na Lista de Boletos' ) ) ;

  ACBrBoleto.GerarPDF(fACBrBoleto.ListadeBoletos.IndexOf(Self));
end;

procedure TACBrTitulo.GerarPDF(AStream: TStream);
begin
  if not Assigned(ACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

  if (fACBrBoleto.ListadeBoletos.Count <= 0)  then
    raise Exception.Create( ACBrStr('Nenhum Título encontrado na Lista de Boletos' ) ) ;

  ACBrBoleto.GerarPDF(fACBrBoleto.ListadeBoletos.IndexOf(Self), AStream);
end;

procedure TACBrTitulo.EnviarEmail(const sPara, sAssunto: String;
  sMensagem: TStrings; EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings);
begin
  if not Assigned(ACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) );

  if (fACBrBoleto.ListadeBoletos.Count <= 0)  then
    raise Exception.Create( ACBrStr('Nenhum Título encontrado na Lista de Boletos' ) ) ;

  if not Assigned(ACBrBoleto.MAIL) then
    raise Exception.Create( ACBrStr('Nenhum componente "ACBrMail" associado' ) );

  ACBrBoleto.ACBrBoletoFC.IndiceImprimirIndividual :=  fACBrBoleto.ListadeBoletos.IndexOf(Self);
  try
    ACBrBoleto.EnviarEmail(sPara, sAssunto, sMensagem, EnviaPDF, sCC, Anexos);
  finally
    ACBrBoleto.ACBrBoletoFC.IndiceImprimirIndividual:= -1;
  end;
end;

function TACBrTitulo.CriarNFeNaLista: TACBrDadosNFe;
var
  I: Integer;
begin
   I      := fListaDadosNFe.Add(TACBrDadosNFe.Create);
   Result := fListaDadosNFe[I];
end;

procedure TACBrTitulo.setValorDocumento(const AValue: Currency);
begin
  // O arredondamento é com objetivo de remover as diferenças em arredondamentos posteriores,
  //    que causa uma diferença entre o valor do documento e a linha digitável.
  // Veja: https://www.projetoacbr.com.br/forum/topic/48941-erro-ao-gerar-boleto-linha-digitavél-diferente-do-valor-do-boleto-sicoob
  fValorDocumento := RoundABNT(AValue, 2);
end;

{ TACBrBoleto }
constructor TACBrBoleto.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   fACBrBoletoFC           := nil;
   FMAIL                   := nil;
   fImprimirMensagemPadrao := True;

   fListadeBoletos := TListadeBoletos.Create(true);
   fListaConsultaRetornoWeb := TListaACBrBoletoRetornoWS.Create(True);

   fBanco := TACBrBanco.Create(self);
   fBanco.Name := 'Banco';
   {$IFDEF COMPILER6_UP}
    fBanco.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
   {$ENDIF}

   fCedente      := TACBrCedente.Create(self);
   fCedente.Name := 'Cedente';
   {$IFDEF COMPILER6_UP}
    fCedente.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
   {$ENDIF}

   fConfiguracoes      := TConfiguracoes.Create(self);
   fConfiguracoes.Name := 'Configuracoes';
   {$IFDEF COMPILER6_UP}
   fConfiguracoes.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
   {$ENDIF}
   FOnAntesAutenticar  := nil;
   FOnDepoisAutenticar := nil;
end;

destructor TACBrBoleto.Destroy;
begin
   fListadeBoletos.Free;
   fCedente.Free;
   fBanco.Free;
   fConfiguracoes.Free;
   fListaConsultaRetornoWeb.Free;

   inherited;
end;

procedure TACBrBoleto.SetACBrBoletoFC ( const Value: TACBrBoletoFCClass ) ;
Var OldValue: TACBrBoletoFCClass;
begin
   if Value <> fACBrBoletoFC then
   begin
      if Assigned(fACBrBoletoFC) then
         fACBrBoletoFC.RemoveFreeNotification(Self);

      OldValue      := fACBrBoletoFC ;   // Usa outra variavel para evitar Loop Infinito
      fACBrBoletoFC := Value;            // na remoção da associação dos componentes

      if Assigned(OldValue) then
         if Assigned(OldValue.ACBrBoleto) then
            OldValue.ACBrBoleto := nil ;

      if Value <> nil then
      begin
         Value.FreeNotification(self);
         Value.ACBrBoleto := self ;
      end ;
   end ;
end;

procedure TACBrBoleto.SetMAIL(AValue: TACBrMail);
begin
  if AValue <> FMAIL then
  begin
    if Assigned(FMAIL) then
      FMAIL.RemoveFreeNotification(Self);

    FMAIL := AValue;

    if AValue <> nil then
      AValue.FreeNotification(self);
  end;
end;

procedure TACBrBoleto.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited Notification ( AComponent, Operation ) ;

   if (Operation = opRemove) then
   begin
     if (fACBrBoletoFC <> nil) and (AComponent is TACBrBoletoFCClass) then
       fACBrBoletoFC := nil ;

     if (FMAIL <> nil) and (AComponent is TACBrMail) then
       FMAIL := nil;
   end;
end;

function TACBrBoleto.CriarTituloNaLista: TACBrTitulo;
var
   I : Integer;
begin
   I      := fListadeBoletos.Add(TACBrTitulo.Create(Self));
   Result := fListadeBoletos[I];
end;

function TACBrBoleto.CriarRetornoWebNaLista: TACBrBoletoRetornoWS;
var
  I: integer;
begin
  I := fListaConsultaRetornoWeb.Add(TACBrBoletoRetornoWS.Create);
  Result := fListaConsultaRetornoWeb[I];
end;

function TACBrBoleto.TotalListaRetornoWeb: Integer;
begin
  Result := ListadeBoletos.Count;
end;

procedure TACBrBoleto.Imprimir;
begin
   if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   if Banco.Numero = 0 then
      raise Exception.Create( ACBrStr('Banco não definido, impossivel listar boleto') );

   ChecarDadosObrigatorios;

   ACBrBoletoFC.Imprimir;
end;

procedure TACBrBoleto.Imprimir(AIndice: Integer);
Var
  AOldIndice: Integer;
begin
  if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   if Banco.Numero = 0 then
      raise Exception.Create( ACBrStr('Banco não definido, impossivel listar boleto') );

   ChecarDadosObrigatorios;

   AOldIndice := ACBrBoletoFC.IndiceImprimirIndividual;

   try
     ACBrBoletoFC.IndiceImprimirIndividual := AIndice;
     ACBrBoletoFC.Imprimir;
   finally
     ACBrBoletoFC.IndiceImprimirIndividual := AOldIndice;
   end;
end;

procedure TACBrBoleto.Imprimir(AStream: TStream);
begin
   if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   if Banco.Numero = 0 then
      raise Exception.Create( ACBrStr('Banco não definido, impossivel listar boleto') );

   ChecarDadosObrigatorios;

   ACBrBoletoFC.Imprimir(AStream);
end;

procedure TACBrBoleto.Imprimir(AIndice: Integer; AStream: TStream);
Var
  AOldIndice: Integer;
begin
  if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   if Banco.Numero = 0 then
      raise Exception.Create( ACBrStr('Banco não definido, impossivel listar boleto') );

   ChecarDadosObrigatorios;

   AOldIndice := ACBrBoletoFC.IndiceImprimirIndividual;

   try
     ACBrBoletoFC.IndiceImprimirIndividual := AIndice;
     ACBrBoletoFC.Imprimir(AStream);
   finally
     ACBrBoletoFC.IndiceImprimirIndividual := AOldIndice;
   end;
end;

procedure TACBrBoleto.GerarPDF;
begin
   if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   ChecarDadosObrigatorios;

   ACBrBoletoFC.GerarPDF;
end;

procedure TACBrBoleto.GerarPDF(AIndex: Integer);
begin
  if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   ChecarDadosObrigatorios;

   ACBrBoletoFC.GerarPDF(AIndex);
end;

procedure TACBrBoleto.GerarPDF(AStream: TStream);
begin
  if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   ChecarDadosObrigatorios;

   ACBrBoletoFC.GerarPDF(AStream);
end;

procedure TACBrBoleto.GerarPDF(AIndex: Integer; AStream: TStream);
begin
  if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   ChecarDadosObrigatorios;

   ACBrBoletoFC.GerarPDF(AIndex, AStream);
end;

procedure TACBrBoleto.GerarHTML;
begin
   if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) );

   ChecarDadosObrigatorios;

   ACBrBoletoFC.GerarHTML;
end;

procedure TACBrBoleto.GerarJPG;
begin
   if not Assigned(ACBrBoletoFC) then
      raise Exception.Create( ACBrStr('Nenhum componente "ACBrBoletoFC" associado' ) ) ;

   ChecarDadosObrigatorios;

   ACBrBoletoFC.GerarJPG;
end;

function TACBrBoleto.GerarMensagemPadraoAbatimento(
  ATitulo: TACBrTitulo): String;
begin
  if ATitulo.DataAbatimento <> 0 then
    Result := ACBrStr('Conceder abatimento de ' + FormatFloatBr(ATitulo.ValorAbatimento, 'R$ #,##0.00')+' para pagamento ate ' + FormatDateTime('dd/mm/yyyy',ATitulo.DataAbatimento))
  else
    Result := ACBrStr('Conceder abatimento de ' + FormatFloatBr(ATitulo.ValorAbatimento, 'R$ #,##0.00'));
end;

function TACBrBoleto.GerarMensagemPadraoDataLimitePagamento(
  ATitulo: TACBrTitulo): String;
begin
  if ATitulo.DataLimitePagto > ATitulo.Vencimento then
    Result:= ACBrStr('Não Receber após ' + IntToStr(DaysBetween(ATitulo.Vencimento, ATitulo.DataLimitePagto))+ ' dias')
  else
    Result := ACBrStr('Não Receber após o Vencimento');
end;

function TACBrBoleto.GetListaRetornoWeb(const Indice: Integer): TACBrBoletoRetornoWS;
begin
  Result := ListadeBoletos[indice].RetornoWeb;
end;

function TACBrBoleto.GerarMensagemPadraoDesconto(const ATipoDesconto : TACBrTipoDesconto; AValorDesconto : Double; ATitulo : TACBrTitulo; ADataDesconto : TDateTime = 0): String;
var ValorDesconto : Double;
begin

  case ATipoDesconto of
    tdPercentualAteDataInformada,
    tdPercentualSobreValorNominalDiaCorrido,
    tdPercentualSobreValorNominalDiaUtil :
      begin
        ValorDesconto := CalcularPercentualValor(AValorDesconto, ATitulo.ValorDocumento);
      end;
    else
      ValorDesconto := AValorDesconto;
  end;
  case ATipoDesconto of
    tdValorFixoAteDataInformada,
    tdPercentualAteDataInformada :
      begin
        if ADataDesconto > 0 then
          Result := ACBrStr('Conceder desconto de ' +
                            FormatFloatBr(ValorDesconto, 'R$ #,##0.00') +
                            ' para pagamento até ' +
                            FormatDateTime('dd/mm/yyyy',ADataDesconto)
                   );
      end;
    tdValorAntecipacaoDiaCorrido,
    tdPercentualSobreValorNominalDiaCorrido :
      begin
        Result := ACBrStr('Conceder desconto de ' +
                          FormatFloatBr(ValorDesconto, 'R$ #,##0.00') +
                          ' por dia de antecipaçao corrido.'
                  );
      end;
    tdValorAntecipacaoDiaUtil,
    tdPercentualSobreValorNominalDiaUtil :
      begin
        Result := ACBrStr('Conceder desconto de ' +
                          FormatFloatBr(ValorDesconto, 'R$ #,##0.00') +
                          ' por dia de antecipaçao útil.'
                  );
      end;
    tdNaoConcederDesconto :
      begin // depreciado... retrocompatibilidade com a implementação antiga
            // utilizar os enumeradores corretos quando houver descontos a exibir.
        if ADataDesconto > 0 then
          Result := ACBrStr('Conceder desconto de ' +
                            FormatFloatBr(ValorDesconto, 'R$ #,##0.00') +
                            ' para pagamento até ' +
                            FormatDateTime('dd/mm/yyyy',ADataDesconto)
                   )
        else
        if ValorDesconto > 0 then
          Result := ACBrStr('Conceder desconto de ' +
                          FormatFloatBr(ValorDesconto, 'R$ #,##0.00') +
                          ' por dia de antecipaçao corrido.'
                  );
      end;
  end;
end;

function TACBrBoleto.GerarMensagemPadraoJuros(ATitulo: TACBrTitulo): String;
var ATipoJuros,AJurosQuando : String;
begin
  if (ATitulo.CodigoMoraJuros in [cjTaxaMensal, cjValorMensal]) or (ATitulo.CodigoMora = '2') or (ATitulo.CodigoMora = 'B') then
    ATipoJuros := FloatToStr(ATitulo.ValorMoraJuros) + '% ao mês'
  else
    ATipoJuros := FormatFloatBr(ATitulo.ValorMoraJuros, 'R$ #,##0.00 por dia');

  if ATitulo.DataMoraJuros <> 0 then
  begin
    if ATitulo.Vencimento = ATitulo.DataMoraJuros then
      AJurosQuando := 'após o vencimento'
    else
      AJurosQuando := 'a partir de '+FormatDateTime('dd/mm/yyyy',ATitulo.DataMoraJuros);
  end else
    AJurosQuando := ' por dia de atraso';

  Result := ACBrStr(Format('Cobrar juros de %s de atraso para pagamento %s.',[ATipoJuros,AJurosQuando]));
end;

function TACBrBoleto.GerarMensagemPadraoMulta(ATitulo: TACBrTitulo): String;
var AValorMulta : Currency;
  ATipoMulta : String;
begin

  if ATitulo.MultaValorFixo then
    AValorMulta := ATitulo.PercentualMulta
  else
    AValorMulta := CalcularPercentualValor(ATitulo.PercentualMulta,ATitulo.ValorDocumento);

  if (ATitulo.DataMulta <> 0) and (ATitulo.DataMulta > ATitulo.Vencimento) then
    ATipoMulta := 'a partir de ' + FormatDateTime('dd/mm/yyyy',ATitulo.DataMulta)
  else
    ATipoMulta := 'após o vencimento';

  Result := ACBrStr(Format('Cobrar multa de R$%s para pagamento %s.',[FormatFloatBr(AValorMulta),ATipoMulta]));
end;

function TACBrBoleto.GerarMensagemPadraoNegativacao(ATitulo: TACBrTitulo): String;
begin
  if ATitulo.TipoDiasNegativacao = diCorridos then
    Result := ACBrStr('Negativar em ' + IntToStr(DaysBetween(ATitulo.Vencimento, ATitulo.DataNegativacao))+ ' dias corridos após o vencimento')
  else
    Result := ACBrStr('Negativar no '+IntToStr(max(ATitulo.DiasDeNegativacao,1)) + 'º dia útil após o vencimento');
end;

function TACBrBoleto.GerarMensagemPadraoProtesto(ATitulo: TACBrTitulo): String;
begin
  if ATitulo.TipoDiasProtesto = diCorridos then
    Result := ACBrStr('Protestar em ' + IntToStr(DaysBetween(ATitulo.Vencimento, ATitulo.DataProtesto))+ ' dias corridos após o vencimento')
  else
    Result := ACBrStr('Protestar no '+IntToStr(max(ATitulo.DiasDeProtesto,1)) + 'º dia útil após o vencimento');
end;

procedure TACBrBoleto.EnviarEmail(const sPara, sAssunto: String;
  sMensagem: TStrings; EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings);
var
  i: Integer;
  EMails: TStringList;
  sDelimiter: Char;
begin
  if not Assigned(FMAIL) then
    raise Exception.Create( ACBrStr('Componente ACBrMail não associado') );

  FMAIL.Clear;

  EMails := TStringList.Create;
  try
    if Pos( ';', sPara) > 0 then
       sDelimiter := ';'
    else
       sDelimiter := ',';
    QuebrarLinha( sPara, EMails, '"', sDelimiter);

    for i := 0 to EMails.Count -1 do
       FMAIL.AddAddress( EMails[i] );
  finally
    EMails.Free;
  end;

  FMAIL.Subject := sAssunto;

  if Assigned(sMensagem) then
  begin
    if FMAIL.IsHTML then
      FMAIL.Body.Assign(sMensagem);

    FMAIL.AltBody.Text := (StripHTML(sMensagem.Text));
  end;

  FMAIL.ClearAttachments;
  if (EnviaPDF) then
  begin
    GerarPDF;
    if ACBrBoletoFC.IndiceImprimirIndividual >= 0 then
        FMAIL.AddAttachment( ACBrBoletoFC.GetNomeArquivoPdfIndividual(ACBrBoletoFC.NomeArquivo, ACBrBoletoFC.IndiceImprimirIndividual)  ,
                        ExtractFileName( ACBrBoletoFC.GetNomeArquivoPdfIndividual(ACBrBoletoFC.NomeArquivo, ACBrBoletoFC.IndiceImprimirIndividual)) )

    else
      FMAIL.AddAttachment(ACBrBoletoFC.NomeArquivo,
                        ExtractFileName(ACBrBoletoFC.NomeArquivo) );
  end
  else
  begin
    GerarHTML;
    FMAIL.AddAttachment(ACBrBoletoFC.NomeArquivo,
                        ExtractFileName(ACBrBoletoFC.NomeArquivo));
  end;

  if Assigned(sCC) then
  begin
    for i := 0 to sCC.Count - 1 do
      FMAIL.AddCC(sCC[i]);
  end;

  if Assigned(Anexos) then
  begin
    for i := 0 to Anexos.Count - 1 do
      FMAIL.AddAttachment(Anexos[i],ExtractFileName(Anexos[i]));
  end;

  FMAIL.Send;
end;

procedure TACBrBoleto.AdicionarMensagensPadroes(Titulo: TACBrTitulo;
  AStringList: TStrings);
begin
  if not ImprimirMensagemPadrao  then
    exit;

  if Titulo.DataProtesto <> 0 then
   AStringList.Add(GerarMensagemPadraoProtesto(Titulo));

  if Titulo.DataNegativacao <> 0 then
   AStringList.Add(GerarMensagemPadraoNegativacao(Titulo));

  if Titulo.ValorAbatimento <> 0 then
   AStringList.Add(GerarMensagemPadraoAbatimento(Titulo));

  if Titulo.ValorDesconto <> 0 then
    AStringList.Add(GerarMensagemPadraoDesconto(Titulo.TipoDesconto,Titulo.ValorDesconto,Titulo,Titulo.DataDesconto));

  if Titulo.ValorDesconto2 <> 0 then
    AStringList.Add(GerarMensagemPadraoDesconto(Titulo.TipoDesconto2,Titulo.ValorDesconto2,Titulo,Titulo.DataDesconto2));

  if Titulo.ValorDesconto3 <> 0 then
    AStringList.Add(GerarMensagemPadraoDesconto(Titulo.TipoDesconto3,Titulo.ValorDesconto3,Titulo,Titulo.DataDesconto3));

  if Titulo.ValorMoraJuros <> 0 then
    AStringList.Add(GerarMensagemPadraoJuros(Titulo));

  if Titulo.PercentualMulta <> 0 then
    AStringList.Add(GerarMensagemPadraoMulta(Titulo));

  if Titulo.DataLimitePagto <> 0 then
    AStringList.Add(GerarMensagemPadraoDataLimitePagamento(Titulo));
end;

function TACBrBoleto.GerarRemessa(NumeroRemessa : Integer) : String;
var
   Stream:TMemoryStream;
begin
   Result:= '';
   if ListadeBoletos.Count < 1 then
      raise Exception.Create(ACBrStr('Lista de Boletos está vazia'));

   ChecarDadosObrigatorios;

   if not DirectoryExists( DirArqRemessa ) then
      ForceDirectories( DirArqRemessa );

   if not DirectoryExists( DirArqRemessa ) then
      raise Exception.Create( ACBrStr('Diretório inválido:' + sLineBreak + DirArqRemessa) );

   Stream:= TMemoryStream.Create;
  try
    Result:= GerarRemessaStream(NumeroRemessa, Stream);
    if Result <> '' then
      Stream.SaveToFile(Result);
  finally
    Stream.Free;
  end;
end;

function TACBrBoleto.GerarRemessaStream(NumeroRemessa: Integer;
  Stream: TStream): String;
var
   SLRemessa   : TStringList;
   ContTitulos : Integer;
   NomeArq     : String ;
begin
   Result:= '';
   if ListadeBoletos.Count < 1 then
      raise Exception.Create(ACBrStr('Lista de Boletos está vazia'));

   ChecarDadosObrigatorios;

   if Banco.Numero = 77 then
   begin
     NumeroArquivo  := NumeroRemessa;
     NomeArqRemessa := '';
   end;

   if ( NomeArqRemessa = '' ) then
      NomeArq := Banco.CalcularNomeArquivoRemessa
   else
      NomeArq := DirArqRemessa + PathDelim + NomeArqRemessa;

   SLRemessa := TStringList.Create;
   try
      {$IfNDef MSWINDOWS}
      SLRemessa.LineBreak := CRLF;
      {$EndIf}

      if LayoutRemessa = c400 then
      begin
         Banco.GerarRegistroHeader400( NumeroRemessa, SLRemessa );

         for ContTitulos:= 0 to ListadeBoletos.Count-1 do
            Banco.GerarRegistroTransacao400( ListadeBoletos[ContTitulos], SLRemessa);

         Banco.GerarRegistroTrailler400( SLRemessa );

      end
      else
      begin
        SLRemessa.Add( Banco.GerarRegistroHeader240( NumeroRemessa ) );

         for ContTitulos:= 0 to ListadeBoletos.Count-1 do
             SLRemessa.Add( Banco.GerarRegistroTransacao240( ListadeBoletos[ContTitulos] ) );

         SLRemessa.Add( Banco.GerarRegistroTrailler240( SLRemessa ) );
      end;

      if RemoveAcentosArqRemessa then
        SLRemessa.Text := TiraAcentos(SLRemessa.Text)
      else
        SLRemessa.Text := NativeStringToAnsi(SLRemessa.Text);

      SLRemessa.SaveToStream( Stream );
      Result:= NomeArq;
   finally
      SLRemessa.Free;
   end;
end;

procedure TACBrBoleto.LerRetorno(AStream: TStream);
var
  SlRetorno: TStringList;
  NomeArq  , BancoRetorno: String;
begin
   SlRetorno:= TStringList.Create;
   try
     Self.ListadeBoletos.Clear;

     if not Assigned(AStream) then
     begin
       if NomeArqRetorno = '' then
         raise Exception.Create(ACBrStr('NomeArqRetorno deve ser informado.'));

       if not FileExists(NomeArqRetorno) then
         NomeArq := IncludeTrailingPathDelimiter(fDirArqRetorno) + NomeArqRetorno
       else
         NomeArq := NomeArqRetorno;

       if not FilesExists( NomeArq ) then
         raise Exception.Create(ACBrStr('Arquivo não encontrado:'+sLineBreak+NomeArq));

       SlRetorno.LoadFromFile( NomeArq );
     end
     else
     begin
       AStream.Position := 0;
       SlRetorno.LoadFromStream(AStream);
     end;

     if SlRetorno.Count < 1 then
        raise exception.Create(ACBrStr('O Arquivo de Retorno:'+sLineBreak+
                                       NomeArq + sLineBreak+
                                       'está vazio.'+sLineBreak+
                                       ' Não há dados para processar'));

     case Length(SlRetorno.Strings[0]) of
        240 :
          begin
            if Copy(SlRetorno.Strings[0],143,1) <> '2' then
              Raise Exception.Create( ACBrStr( NomeArq + sLineBreak +
                'Não é um arquivo de Retorno de cobrança com layout CNAB240') );

            BancoRetorno  := Copy(SlRetorno.Strings[0],0,3);
            LayoutRemessa := c240 ;
          end;

        400 :
          begin
             if (Copy(SlRetorno.Strings[0],1,9) <> '02RETORNO')   then
               Raise Exception.Create( ACBrStr( NomeArq + sLineBreak +
                 'Não é um arquivo de Retorno de cobrança com layout CNAB400'));

             BancoRetorno  := Copy(SlRetorno.Strings[0],77,3);
             LayoutRemessa := c400 ;
          end;
        else
          raise Exception.Create( ACBrStr( NomeArq + sLineBreak+
            'Não é um arquivo de  Retorno de cobrança CNAB240 ou CNAB400'));
     end;

     if ( IntToStrZero(Banco.Numero, 3) <> BancoRetorno )
        and ( IntToStrZero(Banco.NumeroCorrespondente, 3) <> BancoRetorno )  then
       if LeCedenteRetorno then
         Banco.TipoCobranca := GetTipoCobranca( StrToIntDef(BancoRetorno, 0))
       else
         raise Exception.Create( ACBrStr( 'Arquivo de retorno de banco diferente do Cedente'));

     if LayoutRemessa = c240 then
        Banco.LerRetorno240(SlRetorno)
     else
        Banco.LerRetorno400(SlRetorno);

   finally
     SlRetorno.Free;
   end;
end;

function TACBrBoleto.CalcularPercentualValor(AValorPercentual, AValorDocumento: Double): Double;
begin
  Result := (AValorPercentual / 100) * AValorDocumento;
end;

function TACBrBoleto.CalcularValorDesconto(AValorDocumento, AValorDesconto : Double; ATipoDesconto : TACBrTipoDesconto): Double;
begin
  case ATipoDesconto of
    tdValorFixoAteDataInformada,
    tdValorAntecipacaoDiaCorrido,
    tdValorAntecipacaoDiaUtil :
      begin
        Result := AValorDesconto;
      end;
    tdPercentualAteDataInformada,
    tdPercentualSobreValorNominalDiaCorrido,
    tdPercentualSobreValorNominalDiaUtil :
      begin
        Result := CalcularPercentualValor(AValorDesconto,AValorDocumento);
      end;
    else
      result := 0;
  end;
end;

procedure TACBrBoleto.ChecarDadosObrigatorios;
begin
  EhObrigatorioNomeBeneficiario;
  EhObrigatorioConta;
  EhObrigatorioContaDV;
  EhObrigatorioAgencia;
  EhObrigatorioAgenciaDV;
end;

procedure TACBrBoleto.EhObrigatorioAgencia;
begin
  fBanco.BancoClass.EhObrigatorioAgencia;
end;

procedure TACBrBoleto.EhObrigatorioAgenciaDV;
begin
  fBanco.BancoClass.EhObrigatorioAgenciaDV;
end;

procedure TACBrBoleto.EhObrigatorioConta;
begin
  fBanco.BancoClass.EhObrigatorioConta;
end;

procedure TACBrBoleto.EhObrigatorioContaDV;
begin
  fBanco.BancoClass.EhObrigatorioContaDV;
end;

procedure TACBrBoleto.EhObrigatorioNomeBeneficiario;
begin
  fBanco.BancoClass.EhObrigatorioNomeBeneficiario;
end;

function TACBrBoleto.EnviarBoleto: Boolean;
begin
  Result:= Enviar;
end;

function TACBrBoleto.Enviar: Boolean;
var
  RemessaWS: TBoletoWS;
begin
  if not (Configuracoes.WebService.Operacao in [tpConsulta]) then
    if ListadeBoletos.Count < 1 then
      raise Exception.Create(ACBrStr('Lista de Boletos está vazia'));

  ChecarDadosObrigatorios;

  //Instancia classe para Registro Boleto WebService
  RemessaWS := TBoletoWS.Create(Self);
  try
    ListaConsultaRetornoWeb.Clear;
    try
      Result:= RemessaWS.Enviar;
    Except
      on E:Exception do
      begin
        if ( ( RemessaWS.RetornoBanco.CodRetorno = 0 ) and
             ( Trim( RemessaWS.RetornoBanco.Msg ) = '' ) ) then
          raise Exception.Create(ACBrStr('Erro: ' + E.Message))
        else
          raise Exception.Create(ACBrStr('Erro: ' + IntToStr(RemessaWS.RetornoBanco.CodRetorno) + sLineBreak +
                                 RemessaWS.RetornoBanco.Msg + sLineBreak));
      end;
    end;

  finally
    RemessaWS.Free;
  end;
end;

function TACBrBoleto.GetOcorrenciasRemessa(): TACBrOcorrenciasRemessa;
var I: Integer;
begin
  SetLength(Result, 77);

  for I:= 1 to 48 do
  begin
    Result[I-1].Tipo := TACBrTipoOcorrencia(I-1);
    Result[I-1].descricao := cACBrTipoOcorrenciaDecricao[I-1];
  end;
end;

function TACBrBoleto.GetTipoCobranca(NumeroBanco: Integer; Carteira: String = ''): TACBrTipoCobranca;
begin
  case NumeroBanco of
    001: Result := cobBancoDoBrasil;
    003: Result := cobBancoDaAmazonia;
    004: Result := cobBancoDoNordeste;
    008,033,353: Result := cobSantander;
    021: Result := cobBanestes;
    025: Result := cobBancoAlfa;
    041: Result := cobBanrisul;
    047: Result := cobBanese;
    070: Result := cobBRB;
    077: Result := cobBancoInter;
    084: Result := cobUniprimeNortePR;
    085: Result := cobBancoCECRED;
    091: Result := cobUnicredRS;
    097: Result := cobCrediSIS;
    099: Result := cobUniprime;
    104: Result := cobCaixaEconomica;
    133: Result := cobBancoCresol;
    136: Result := cobUnicredES;
    174: Result := cobBancoPefisa;
    208: Result := cobBTGPactual;
    212: Result := cobBancoOriginal;
    218: Result := cobBS2;
    224: Result := cobBancoFibra;
    237: Result := cobBradesco;
    246: Result := cobBancoABCBrasil;
    274: Result := cobMoneyPlus;
    336: Result := cobBancoC6;
    341: Result := cobItau;
    389: Result := cobBancoMercantil;
    399: Result := cobHSBC;
    422: Result := cobBancoSafra;
    604: Result := cobBancoIndustrialBrasil;
    633: Result := cobBancoRendimento;
    637: begin
           if StrToInt(Carteira) = 109 then
             Result := cobBancoSofisaItau
           else
             Result := cobBancoSofisaSantander;
         end;
    643: begin
           if StrToInt(Carteira) = 9 then
             Result := cobBancoPineBradesco
           else
             Result := cobBancoPine;
         end;
    655: Result := cobBancoVotorantim;
    707: Result := cobDaycoval;
    745: Result := cobCitiBank;
    748: Result := cobSicred;
    756: Result := cobBancoob;
  else
    raise Exception.Create('Erro ao configurar o tipo de cobrança.'+
      sLineBreak+'Número do Banco inválido: '+IntToStr(NumeroBanco));
  end;
end;

function TACBrBoleto.LerArqIni(const AIniBoletos: String): Boolean;
var
  IniBoletos : TMemIniFile ;
  Titulo : TACBrTitulo;
  NFe : TACBrDadosNFe;
  wTipoInscricao, wRespEmissao, wLayoutBoleto: Integer;
  wNumeroBanco, wIndiceACBr, wCNAB, wNumeroCorrespondente,
  wVersaoLote, wVersaoArquivo: Integer;
  wLocalPagto, MemFormatada, MemInformativo, MemDetalhamento: String;
  Sessao, sFim, LocalPagamento, OrientacoesBanco: String;
  I, N: Integer;
  DtMovimento, DtRegistro, DtVencimento: String;
begin
  Result   := False;

  IniBoletos := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniBoletos, IniBoletos);

    with Self.Cedente do
    begin
      //Cedente
      if IniBoletos.SectionExists('Cedente') then
      begin
        wTipoInscricao := IniBoletos.ReadInteger(CCedente,'TipoInscricao', IniBoletos.ReadInteger(CCedente,'TipoPessoa', 1 ) );
        try
           Cedente.TipoInscricao := TACBrPessoa( wTipoInscricao ) ;
        except
           Cedente.TipoInscricao := pJuridica ;
        end ;

        Nome          := IniBoletos.ReadString(CCedente,'Nome',Nome);
        FantasiaCedente:= IniBoletos.ReadString(CCedente,'FantasiaCedente',FantasiaCedente);
        CNPJCPF       := IniBoletos.ReadString(CCedente,'CNPJCPF',CNPJCPF);
        Logradouro    := IniBoletos.ReadString(CCedente,'Logradouro',Logradouro);
        NumeroRes     := IniBoletos.ReadString(CCedente,'Numero',NumeroRes);
        Bairro        := IniBoletos.ReadString(CCedente,'Bairro',Bairro);
        Cidade        := IniBoletos.ReadString(CCedente,'Cidade',Cidade);
        CEP           := IniBoletos.ReadString(CCedente,'CEP',CEP);
        Complemento   := IniBoletos.ReadString(CCedente,'Complemento',Complemento);
        UF            := IniBoletos.ReadString(CCedente,'UF',UF);
        Telefone      := IniBoletos.ReadString(CCedente,'Telefone',Telefone);
        CodigoCedente := IniBoletos.ReadString(CCedente,'CodigoCedente',CodigoCedente);
        Modalidade    := IniBoletos.ReadString(CCedente,'MODALIDADE',Modalidade);
        CodigoTransmissao:= IniBoletos.ReadString(CCedente,'CODTRANSMISSAO',CodigoTransmissao);
        Convenio      := IniBoletos.ReadString(CCedente,'CONVENIO',Convenio);
        CaracTitulo  := TACBrCaracTitulo(IniBoletos.ReadInteger(CCedente,'CaracTitulo',Integer(CaracTitulo) ));
        TipoCarteira := TACBrTipoCarteira(IniBoletos.ReadInteger(CCedente,'TipoCarteira', Integer(TipoCarteira) ));
        TipoDocumento:= TACBrTipoDocumento(IniBoletos.ReadInteger(CCedente,'TipoDocumento', Integer(TipoDocumento) ) + 1 );
        DigitoVerificadorAgenciaConta := IniBoletos.ReadString(CCedente,'DigitoVerificadorAgenciaConta', DigitoVerificadorAgenciaConta);
        IdentDistribuicao := TACBrIdentDistribuicao(IniBoletos.ReadInteger(CCedente,'IdentDistribuicao', Integer(IdentDistribuicao)));
        Operacao          := IniBoletos.ReadString(CCedente,'Operacao', Operacao);

        TipoInscricao :=  TACBrPessoaCedente(IniBoletos.ReadInteger(CCedente,'TipoInscricao',Integer(TipoInscricao) ));

        PIX.Chave        :=  IniBoletos.ReadString(CCedente,'PIX.Chave','');
        PIX.TipoChavePIX :=  TACBrPIXTipoChave(IniBoletos.ReadInteger(CCedente,'PIX.TipoChavePIX', 0 ));

        if Assigned(Self.ACBrBoletoFC) then
        begin
          wLayoutBoleto:= IniBoletos.ReadInteger(CCedente,'LAYOUTBOL', Integer(Self.ACBrBoletoFC.LayOut) );
          Self.ACBrBoletoFC.LayOut  := TACBrBolLayOut(wLayoutBoleto);
        end;
        wRespEmissao := IniBoletos.ReadInteger(CCedente,'RespEmis', Integer(ResponEmissao) );
        try
          ResponEmissao := TACBrResponEmissao( wRespEmissao );
        except
          ResponEmissao := tbCliEmite ;
        end ;

        Result   := True;
      end;

      if IniBoletos.SectionExists('BoletoConfig') then
      begin
        PrefixArqRemessa                  := IniBoletos.ReadString(CBanco,'PrefixArqRemessa',PrefixArqRemessa);
        Homologacao                       := IniBoletos.ReadBool(CBanco,'Homologacao', Homologacao );
        ImprimirMensagemPadrao            := IniBoletos.ReadBool(CBanco,'ImprimirMensagemPadrao', ImprimirMensagemPadrao );
        LeCedenteRetorno                  := IniBoletos.ReadBool(CBanco,'LeCedenteRetorno', LeCedenteRetorno );
        LerNossoNumeroCompleto            := IniBoletos.ReadBool(CBanco,'LerNossoNumeroCompleto', LerNossoNumeroCompleto );
        RemoveAcentosArqRemessa           := IniBoletos.ReadBool(CBanco,'RemoveAcentosArqRemessa', RemoveAcentosArqRemessa );
      end;


      //Banco
      if IniBoletos.SectionExists('Banco') then
      begin
        wNumeroBanco                      := IniBoletos.ReadInteger(CBanco,'Numero', 0 );
        wIndiceACBr                       := IniBoletos.ReadInteger(CBanco,'IndiceACBr', IniBoletos.ReadInteger(CBanco,'TipoCobranca', 0 ) );
        wCNAB                             := IniBoletos.ReadInteger(CBanco,'CNAB', Integer(LayoutRemessa) );
        wNumeroCorrespondente             := IniBoletos.ReadInteger(CBanco,'NumeroCorrespondente', 0 );
        wVersaoArquivo                    := IniBoletos.ReadInteger(CBanco,'VersaoArquivo', 0 );
        wVersaoLote                       := IniBoletos.ReadInteger(CBanco,'VersaoLote', 0 );

        LocalPagamento := IniBoletos.ReadString(CBanco,'LocalPagamento','');
        if NaoEstaVazio(LocalPagamento) then
          Banco.LocalPagamento              := LocalPagamento;

        OrientacoesBanco := StringReplace(IniBoletos.ReadString(CBanco,'OrientacoesBanco',''), '|', sLineBreak, [rfReplaceAll]);
        if NaoEstaVazio(OrientacoesBanco) then
          Banco.OrientacoesBanco.Text       := OrientacoesBanco;

        Banco.CasasDecimaisMoraJuros      := IniBoletos.ReadInteger(CBanco,'CasasDecimaisMoraJuros',Banco.CasasDecimaisMoraJuros);
        Banco.DensidadeGravacao           := IniBoletos.ReadString(CBanco,'DensidadeGravacao',Banco.DensidadeGravacao);
        Banco.CIP                         := IniBoletos.ReadString(CBanco,'CIP',Banco.CIP);

        {$IFDEF SUPPORTS_REGION}{$REGION 'deprecated enviados para sessão de [BoletoConfig] - previsão para remoção de retirada XX/XX/XXXX'}{$ENDIF}
          if IniBoletos.ValueExists('Banco','PrefixArqRemessa') then
            PrefixArqRemessa                := IniBoletos.ReadString(CBanco,'PrefixArqRemessa',PrefixArqRemessa);
          if IniBoletos.ValueExists('Banco','Homologacao') then
            Homologacao                     := IniBoletos.ReadBool(CBanco,'Homologacao', Homologacao );
          if IniBoletos.ValueExists('Banco','ImprimirMensagemPadrao') then
            ImprimirMensagemPadrao          := IniBoletos.ReadBool(CBanco,'ImprimirMensagemPadrao', ImprimirMensagemPadrao );
          if IniBoletos.ValueExists('Banco','LeCedenteRetorno') then
            LeCedenteRetorno                := IniBoletos.ReadBool(CBanco,'LeCedenteRetorno', LeCedenteRetorno );
          if IniBoletos.ValueExists('Banco','LerNossoNumeroCompleto') then
            LerNossoNumeroCompleto          := IniBoletos.ReadBool(CBanco,'LerNossoNumeroCompleto', LerNossoNumeroCompleto );
          if IniBoletos.ValueExists('Banco','RemoveAcentosArqRemessa') then
            RemoveAcentosArqRemessa         := IniBoletos.ReadBool(CBanco,'RemoveAcentosArqRemessa', RemoveAcentosArqRemessa );
        {$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


        if ( wCNAB = 0 ) then
           LayoutRemessa := c240
        else
           LayoutRemessa := c400;

        if ( wIndiceACBr > 0 ) then
          Banco.TipoCobranca:= TACBrTipoCobranca(wIndiceACBr)
        else if ( wNumeroBanco > 0 ) then
          Banco.TipoCobranca := GetTipoCobranca(wNumeroBanco);

        if (trim(Banco.Nome) = 'Não definido') then
           raise exception.Create('Banco não definido ou não '+
                                  'implementado no ACBrBoleto!');

        if ( wNumeroCorrespondente > 0 ) then
          Banco.NumeroCorrespondente:= wNumeroCorrespondente;

        if ( wVersaoArquivo > 0 ) then
          Banco.LayoutVersaoArquivo:= wVersaoArquivo;

        if ( wVersaoLote > 0 ) then
          Banco.LayoutVersaoLote:= wVersaoLote;

        Result := True;
      end;

      //Conta
      if IniBoletos.SectionExists('Conta') then
      begin
        Conta         := IniBoletos.ReadString(CConta,'Conta', Conta);
        ContaDigito   := IniBoletos.ReadString(CConta,'DigitoConta', ContaDigito);
        Agencia       := IniBoletos.ReadString(CConta,'Agencia', Agencia);
        AgenciaDigito := IniBoletos.ReadString(CConta,'DigitoAgencia', AgenciaDigito);
        DigitoVerificadorAgenciaConta := IniBoletos.ReadString(CConta,'DigitoVerificadorAgenciaConta',
                                      DigitoVerificadorAgenciaConta );

        Result := True;
      end;

      if IniBoletos.SectionExists('WEBSERVICE') then
      begin
        CedenteWS.ClientID                  := IniBoletos.ReadString(CWebService,'ClientID', CedenteWS.ClientID);
        CedenteWS.ClientSecret              := IniBoletos.ReadString(CWebService,'ClientSecret', CedenteWS.ClientSecret);
        CedenteWS.KeyUser                   := IniBoletos.ReadString(CWebService,'KeyUser', CedenteWS.KeyUser);
        CedenteWS.IndicadorPix              := IniBoletos.ReadBool(CWebService,'IndicadorPix', CedenteWS.IndicadorPix);
        CedenteWS.Scope                     := IniBoletos.ReadString(CWebService,'Scope', CedenteWS.Scope);
        Configuracoes.WebService.Ambiente   := TpcnTipoAmbiente(IniBoletos.ReadInteger(CWebService,'Ambiente', Integer(Configuracoes.WebService.Ambiente)));
        Configuracoes.WebService.SSLHttpLib := TSSLHttpLib(IniBoletos.ReadInteger(CWebService,'SSLHttpLib', Integer(Configuracoes.WebService.SSLHttpLib)));

        if IniBoletos.ValueExists(CWebService,'ArquivoCRT') then
          Configuracoes.WebService.ArquivoCRT := IniBoletos.ReadString(CWebService,'ArquivoCRT', Configuracoes.WebService.ArquivoCRT);

        if IniBoletos.ValueExists(CWebService,'ArquivoKEY') then
          Configuracoes.WebService.ArquivoKEY := IniBoletos.ReadString(CWebService,'ArquivoKEY', Configuracoes.WebService.ArquivoKEY);

        if IniBoletos.ValueExists(CWebService,'ArquivoPFX') then
          Configuracoes.WebService.ArquivoPFX := IniBoletos.ReadString(CWebService,'ArquivoPFX', Configuracoes.WebService.ArquivoPFX);

        Result := True;
      end;
    end;

    if (IniBoletos.SectionExists('Titulo')) or (IniBoletos.SectionExists('Titulo1')) then
    begin
      with Self do
      begin
        //Titulo
        if (trim(Banco.Nome) = 'Não definido') then
                raise exception.Create('Banco não definido ou não '+
                                       'implementado no ACBrBoleto!');

        I := 1 ;
        while true do
        begin
          Sessao := 'Titulo' + IntToStr(I);
          sFim   := IniBoletos.ReadString(Sessao,'NumeroDocumento','FIM');

          if (sFim = 'FIM') and (Sessao = 'Titulo1')  then
          begin
            Sessao := 'Titulo';
            sFim   := IniBoletos.ReadString(Sessao,'NumeroDocumento','FIM');
          end;

          if (sFim = 'FIM')  then
            break ;

          Titulo := CriarTituloNaLista;

          MemFormatada := IniBoletos.ReadString(Sessao,'Mensagem','') ;
          MemFormatada := StringReplace( MemFormatada,'|',sLineBreak, [rfReplaceAll] );

          MemDetalhamento := IniBoletos.ReadString(Sessao,'Detalhamento','') ;
          MemDetalhamento := StringReplace( MemDetalhamento,'|',sLineBreak, [rfReplaceAll] );

          MemInformativo := IniBoletos.ReadString(Sessao,'Informativo','') ;
          MemInformativo := StringReplace( MemInformativo,'|',sLineBreak, [rfReplaceAll] );

          with Titulo do
          begin
            Aceite        := TACBrAceiteTitulo(IniBoletos.ReadInteger(Sessao,'Aceite',1));
//            Sacado.Pessoa := TACBrPessoa( IniBoletos.ReadInteger(Sessao,'Sacado.Pessoa',2) );
            Sacado.Pessoa := TACBrPessoa( IniBoletos.ReadInteger(Sessao,'Sacado.Pessoa',2) );
            OcorrenciaOriginal.Tipo := TACBrTipoOcorrencia(IniBoletos.ReadInteger(Sessao,'OcorrenciaOriginal.TipoOcorrencia',0)) ;
            TipoDiasProtesto := TACBrTipoDiasIntrucao(IniBoletos.ReadInteger(Sessao,'TipoDiasProtesto',0));
            TipoDiasNegativacao := TACBrTipoDiasIntrucao(IniBoletos.ReadInteger(Sessao,'TipoDiasNegativacao',0));
            TipoImpressao := TACBrTipoImpressao(IniBoletos.ReadInteger(Sessao,'TipoImpressao',1));
            TipoDesconto := TACBrTipoDesconto(IniBoletos.ReadInteger(Sessao,'TipoDesconto',0));
            TipoDesconto2 := TACBrTipoDesconto(IniBoletos.ReadInteger(Sessao,'TipoDesconto2',0));
            CarteiraEnvio:= TACBrCarteiraEnvio(IniBoletos.ReadInteger(Sessao,'CarteiraEnvio', 0));
            MultaValorFixo := IniBoletos.ReadBool(Sessao,'MultaValorFixo',False);

            CodigoNegativacao := TACBrCodigoNegativacao(IniBoletos.ReadInteger(Sessao,'CodigoNegativacao', 3));

            wLocalPagto := IniBoletos.ReadString(Sessao,'LocalPagamento','');

            Vencimento          := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'Vencimento','')), now);
            DataDocumento       := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataDocumento','')),now);
            DataProcessamento   := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataProcessamento','')),now);
            DataAbatimento      := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataAbatimento','')),0);
            DataDesconto        := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataDesconto','')),0);
            DataMoraJuros       := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataMoraJuros','')),0);
  	        DataMulta           := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataMulta','')),0);
            DiasDeProtesto      := IniBoletos.ReadInteger(Sessao,'DiasDeProtesto',0);
            if (DiasDeProtesto = 0) then
              DataProtesto      := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataProtesto','')),0);
            DiasDeNegativacao   := IniBoletos.ReadInteger(Sessao,'DiasDeNegativacao',0);
            if (DiasDeNegativacao = 0) then
              DataNegativacao   := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataNegativacao','')),0);
            DataBaixa           := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataBaixa','')),0);
            DataLimitePagto     := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataLimitePagto','')),0);
            LocalPagamento      := IfThen(Trim(wLocalPagto) <> '',wLocalPagto,LocalPagamento);
            NumeroDocumento     := IniBoletos.ReadString(Sessao,'NumeroDocumento',NumeroDocumento);
            EspecieDoc          := IniBoletos.ReadString(Sessao,'Especie',EspecieDoc);
            Carteira            := trim(IniBoletos.ReadString(Sessao,'Carteira',''));
            NossoNumero         := IniBoletos.ReadString(Sessao,'NossoNumero','');
            ValorDocumento      := IniBoletos.ReadFloat(Sessao,'ValorDocumento',ValorDocumento);
            Sacado.NomeSacado   := IniBoletos.ReadString(Sessao,'Sacado.NomeSacado','');
            Sacado.CNPJCPF      := OnlyNumber(IniBoletos.ReadString(Sessao,'Sacado.CNPJCPF',''));
            Sacado.Logradouro   := IniBoletos.ReadString(Sessao,'Sacado.Logradouro','');
            Sacado.Numero       := IniBoletos.ReadString(Sessao,'Sacado.Numero','');
            Sacado.Bairro       := IniBoletos.ReadString(Sessao,'Sacado.Bairro','');
            Sacado.Complemento  := IniBoletos.ReadString(Sessao,'Sacado.Complemento','');
            Sacado.Cidade       := IniBoletos.ReadString(Sessao,'Sacado.Cidade','');
            Sacado.UF           := IniBoletos.ReadString(Sessao,'Sacado.UF','');
            Sacado.CEP          := OnlyNumber(IniBoletos.ReadString(Sessao,'Sacado.CEP',''));
            Sacado.Email        := IniBoletos.ReadString(Sessao,'Sacado.Email',Sacado.Email);
            EspecieMod          := IniBoletos.ReadString(Sessao,'EspecieMod',EspecieMod);
            Mensagem.Text       := MemFormatada;
            Informativo.Text    := MemInformativo;
            Detalhamento.Text   := MemDetalhamento;
            Instrucao1          := IniBoletos.ReadString(Sessao,'Instrucao1',Instrucao1);
            Instrucao2          := IniBoletos.ReadString(Sessao,'Instrucao2',Instrucao2);
            Instrucao3          := IniBoletos.ReadString(Sessao,'Instrucao3',Instrucao3);
            TotalParcelas       := IniBoletos.ReadInteger(Sessao,'TotalParcelas',TotalParcelas);
            Parcela             := IniBoletos.ReadInteger(Sessao,'Parcela',Parcela);
            ValorAbatimento     := IniBoletos.ReadFloat(Sessao,'ValorAbatimento',ValorAbatimento);
            ValorDesconto       := IniBoletos.ReadFloat(Sessao,'ValorDesconto',ValorDesconto);
            ValorMoraJuros      := IniBoletos.ReadFloat(Sessao,'ValorMoraJuros',ValorMoraJuros);
            ValorIOF            := IniBoletos.ReadFloat(Sessao,'ValorIOF',ValorIOF);
            ValorOutrasDespesas := IniBoletos.ReadFloat(Sessao,'ValorOutrasDespesas',ValorOutrasDespesas);
            SeuNumero           := IniBoletos.ReadString(Sessao,'SeuNumero',SeuNumero);
            PercentualMulta     := IniBoletos.ReadFloat(Sessao,'PercentualMulta',PercentualMulta);
            CodigoMora          := IniBoletos.ReadString(Sessao,'CodigoMora','1');
            CodigoMoraJuros     := TACBrCodigoJuros(IniBoletos.ReadInteger(Sessao,'CodigoMoraJuros', 2 ));
            CodigoGeracao       := IniBoletos.ReadString(Sessao,'CodigoGeracao','2');
            Competencia         := IniBoletos.ReadString(Sessao,'Competencia', Competencia);
            ArquivoLogoEmp      := IniBoletos.ReadString(Sessao,'ArquivoLogoEmp', ArquivoLogoEmp);
            Verso               := IniBoletos.ReadBool(Sessao,'Verso', False);
            Sacado.SacadoAvalista.Pessoa        := TACBrPessoa( IniBoletos.ReadInteger(Sessao,'Sacado.SacadoAvalista.Pessoa',2) );
            Sacado.SacadoAvalista.NomeAvalista  := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.NomeAvalista','');
            Sacado.SacadoAvalista.CNPJCPF       := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.CNPJCPF','');
            Sacado.SacadoAvalista.Logradouro    := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.Logradouro','');
            Sacado.SacadoAvalista.Numero        := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.Numero','');
            Sacado.SacadoAvalista.Complemento   := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.Complemento','');
            Sacado.SacadoAvalista.Bairro        := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.Bairro','');
            Sacado.SacadoAvalista.Cidade        := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.Cidade','');
            Sacado.SacadoAvalista.UF            := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.UF','');
            Sacado.SacadoAvalista.CEP           := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.CEP','');
            Sacado.SacadoAvalista.Email         := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.Email','');
            Sacado.SacadoAvalista.Fone          := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.Fone','');
            Sacado.SacadoAvalista.InscricaoNr   := IniBoletos.ReadString(Sessao,'Sacado.SacadoAvalista.InscricaoNr','');
            QrCode.emv                          := IniBoletos.ReadString(Sessao,'QrCode.emv','');
            QrCode.url                          := IniBoletos.ReadString(Sessao,'QrCode.url','');
            QrCode.txId                         := IniBoletos.ReadString(Sessao,'QrCode.txId','');
            TipoPagamento                       := TTipo_Pagamento( IniBoletos.ReadInteger(Sessao,'TipoPagamento',2));
            QtdePagamentoParcial                := IniBoletos.ReadInteger(Sessao,'QtdePagamentoParcial',0);
            QtdeParcelas                        := IniBoletos.ReadInteger(Sessao,'QtdeParcelas',0);
            ValorMinPagamento                   := IniBoletos.ReadFloat(Sessao,'ValorMinPagamento', ValorMinPagamento);
            ValorMaxPagamento                   := IniBoletos.ReadFloat(Sessao,'ValorMaxPagamento', ValorMaxPagamento);
            PercentualMinPagamento              := IniBoletos.ReadFloat(Sessao,'PercentualMinPagamento', PercentualMinPagamento);
            PercentualMaxPagamento              := IniBoletos.ReadFloat(Sessao,'PercentualMaxPagamento', PercentualMaxPagamento);

            //Apenas banco Pine
            if (IniBoletos.SectionExists('NFe'+IntToStr(I)) ) or (IniBoletos.SectionExists('NFe'+IntToStr(I)+'-1') ) then
            begin
              with Self do
              begin
                N := 1 ;
                while true do
                begin
                  Sessao := 'NFe'+IntToStr(I);
                  sFim   := IniBoletos.ReadString(Sessao,'ChaveNFe','FIM');

                  if (sFim = 'FIM') then
                  begin
                    Sessao := 'NFe'+IntToStr(I)+'-'+IntToStr(N);
                    sFim   := IniBoletos.ReadString(Sessao,'ChaveNFe','FIM');
                  end;

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break ;

                  NFe := CriarNFeNaLista;
                  NFe.NumNFe := IniBoletos.ReadString(Sessao,'NumNFe','');
                  NFe.ValorNFe := IniBoletos.ReadFloat(Sessao,'ValorNFe',0);
                  NFe.EmissaoNFe := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'EmissaoNFe','')), 0);
                  NFe.ChaveNFe := sFim;

                  Inc(N);
                end;
              end;
            end;

          end;
          inc(I);
          Result := True;
        end;
      end;
    end;

    //Filtro para Consulta por API
    if (IniBoletos.SectionExists('ConsultaAPI')) then
    begin
      Configuracoes.WebService.Filtro.Clear;

      Sessao := 'ConsultaAPI';
      sFim   := IniBoletos.ReadString(Sessao,'IndicadorSituacaoBoleto','0');
      if (sFim <> '0')  then
        Configuracoes.WebService.Filtro.indicadorSituacao := TACBrIndicadorSituacaoBoleto(StrToInt64Def(sFim,0))
      else
        raise exception.Create('Nenhum Indicador de Situacao definido para consulta!');

      DtMovimento  := Trim(IniBoletos.ReadString(Sessao,'DataInicioMovimento','0'));
      DtVencimento := Trim(IniBoletos.ReadString(Sessao,'DataInicioVencimento','0'));
      DtRegistro   := Trim(IniBoletos.ReadString(Sessao,'DataInicioRegistro','0'));

      if (DtMovimento = '0') and (DtVencimento = '0') and (DtRegistro = '0') then
        raise exception.Create('Nenhuma Data Definida para Consulta!');

      Configuracoes.WebService.Filtro.dataMovimento.FDataInicio := StrToDateDef(DtMovimento,0);
      Configuracoes.WebService.Filtro.dataMovimento.FDataFinal := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataFinalMovimento',DtMovimento)),0);

      Configuracoes.WebService.Filtro.dataVencimento.FDataInicio := StrToDateDef(DtVencimento,0);
      Configuracoes.WebService.Filtro.dataVencimento.FDataFinal := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataFinalVencimento',DtVencimento)),0);

      Configuracoes.WebService.Filtro.dataRegistro.FDataInicio := StrToDateDef(DtRegistro,0);
      Configuracoes.WebService.Filtro.dataRegistro.FDataFinal := StrToDateDef(Trim(IniBoletos.ReadString(Sessao,'DataFinalRegistro',DtRegistro)),0);

      Configuracoes.WebService.Filtro.cnpjCpfPagador := IniBoletos.ReadString(Sessao,'cnpjCpfPagador', '' );
      Configuracoes.WebService.Filtro.contaCaucao := IniBoletos.ReadInteger(Sessao,'ContaCaucao', 0 );
      Configuracoes.WebService.Filtro.codigoEstadoTituloCobranca := IniBoletos.ReadInteger(Sessao,'CodigoEstadoTituloCobranca', 0 );
      Configuracoes.WebService.Filtro.modalidadeCobranca := IniBoletos.ReadInteger(Sessao,'ModalidadeCobranca', 0 );
      Configuracoes.WebService.Filtro.carteira := IniBoletos.ReadInteger(Sessao,'Carteira', 0 );
      Configuracoes.WebService.Filtro.carteiraVariacao := IniBoletos.ReadInteger(Sessao,'CarteiraVariacao', 0 );
      Configuracoes.WebService.Filtro.indiceContinuidade := IniBoletos.ReadInteger(Sessao,'IndiceContinuidade', 0 );

      Result := True;
    end;

  finally
    IniBoletos.free;
  end;

end;

function TACBrBoleto.LerConfiguracao(const AIniBoletos: String): Boolean;
begin
  Result := LerArqIni(AIniBoletos);
end;

function TACBrBoleto.GravarArqIni(DirIniRetorno: string; const NomeArquivo: String; const SomenteConfig:Boolean = false): String;
var
  IniRetorno: TMemIniFile;
  SL: TStringList;
  wSessao: String;
  I: Integer;
  J: Integer;
begin
  Result:= '';
  if Pos(PathDelim,DirIniRetorno) <> Length(DirIniRetorno) then
     DirIniRetorno:= DirIniRetorno + PathDelim;

  IniRetorno:= TMemIniFile.Create(DirIniRetorno + IfThen( EstaVazio(NomeArquivo), 'Retorno.ini', NomeArquivo ) );
  try
    with Self do
    begin
       { BENEFICIARIO }
       IniRetorno.WriteString(CCedente,'CNPJCPF',Cedente.CNPJCPF);
       IniRetorno.WriteString(CCedente,'Nome',Cedente.Nome);
       IniRetorno.WriteString(CCedente,'FantasiaCedente',Cedente.FantasiaCedente);
       IniRetorno.WriteString(CCedente,'Logradouro',Cedente.Logradouro);
       IniRetorno.WriteString(CCedente,'Numero',Cedente.NumeroRes);
       IniRetorno.WriteString(CCedente,'Complemento',Cedente.Complemento);
       IniRetorno.WriteString(CCedente,'Bairro',Cedente.Bairro);
       IniRetorno.WriteString(CCedente,'Cidade',Cedente.Cidade);
       IniRetorno.WriteString(CCedente,'UF',Cedente.UF);
       IniRetorno.WriteString(CCedente,'CEP',Cedente.CEP);
       IniRetorno.WriteString(CCedente,'Telefone',Cedente.Telefone);

       IniRetorno.WriteString(CCedente,'CodigoCedente',Cedente. CodigoCedente);
       IniRetorno.WriteString(CCedente,'MODALIDADE',Cedente.Modalidade);
       IniRetorno.WriteString(CCedente,'CODTRANSMISSAO',Cedente.CodigoTransmissao);
       IniRetorno.WriteString(CCedente,'CONVENIO',Cedente.Convenio);

       IniRetorno.WriteString(CConta,'Conta',Cedente.Conta);
       IniRetorno.WriteString(CConta,'DigitoConta',Cedente.ContaDigito);
       IniRetorno.WriteString(CConta,'Agencia',Cedente.Agencia);
       IniRetorno.WriteString(CConta,'DigitoAgencia',Cedente.AgenciaDigito);
       IniRetorno.WriteString(CConta,'DigitoVerificadorAgenciaConta',Cedente.DigitoVerificadorAgenciaConta);

       IniRetorno.WriteInteger(CCedente,'CaracTitulo',Integer(Cedente.CaracTitulo));
       IniRetorno.WriteInteger(CCedente,'TipoDocumento',Integer(Cedente.TipoDocumento));
       IniRetorno.WriteInteger(CCedente,'TipoCarteira',Integer(Cedente.TipoCarteira));
       IniRetorno.WriteInteger(CCedente,'TipoInscricao',Integer(Cedente.TipoInscricao));
       IniRetorno.WriteInteger(CCedente,'IdentDistribuicao',Integer(Cedente.IdentDistribuicao));
       IniRetorno.WriteInteger(CCedente,'ResponEmissao',Integer(Cedente.ResponEmissao));
       IniRetorno.WriteString(CCedente,'Operacao',Cedente.Operacao);


       IniRetorno.WriteString(CCedente,'PIX.Chave',Cedente.PIX.Chave);
       IniRetorno.WriteInteger(CCedente,'PIX.TipoChavePIX',Integer(Cedente.PIX.TipoChavePIX));

       { BANCO }
       IniRetorno.WriteInteger(CBanco,'Numero',Banco.Numero);
       IniRetorno.WriteInteger(CBanco,'IndiceACBr',Integer(Banco.TipoCobranca));
       IniRetorno.WriteInteger(CBanco,'NumeroCorrespondente',Banco.NumeroCorrespondente);
       IniRetorno.WriteInteger(CBanco,'VersaoArquivo',Banco.LayoutVersaoArquivo);
       IniRetorno.WriteInteger(CBanco,'VersaoLote',Banco.LayoutVersaoLote);
       IniRetorno.WriteString(CBanco,'OrientacoesBanco',StringReplace( Banco.OrientacoesBanco.Text, sLineBreak, '|', [rfReplaceAll] ));

       IniRetorno.WriteString(CBanco,'LocalPagamento',Banco.LocalPagamento);
       IniRetorno.WriteInteger(CBanco,'CasasDecimaisMoraJuros',Banco.CasasDecimaisMoraJuros);
       IniRetorno.WriteString(CBanco,'DensidadeGravacao',Banco.DensidadeGravacao);
       IniRetorno.WriteString(CBanco,'CIP',Banco.CIP);

       { BOLETO }
       IniRetorno.WriteString(CBanco,'PrefixArqRemessa',PrefixArqRemessa);

       if LayoutRemessa = c240 then
         IniRetorno.WriteInteger(CBanco,'CNAB',0)
       else
         IniRetorno.WriteInteger(CBanco,'CNAB',1);

       IniRetorno.WriteBool(CBanco,'Homologacao',Homologacao);
       IniRetorno.WriteBool(CBanco,'ImprimirMensagemPadrao',ImprimirMensagemPadrao);
       IniRetorno.WriteBool(CBanco,'LeCedenteRetorno',LeCedenteRetorno);
       IniRetorno.WriteBool(CBanco,'LerNossoNumeroCompleto',LerNossoNumeroCompleto);
       IniRetorno.WriteBool(CBanco,'RemoveAcentosArqRemessa',RemoveAcentosArqRemessa);

       { WEBSERVICES }
       IniRetorno.WriteString(CWebService,'ClientID',Cedente.CedenteWS.ClientID);
       IniRetorno.WriteString(CWebService,'ClientSecret',Cedente.CedenteWS.ClientSecret);
       IniRetorno.WriteString(CWebService,'KeyUser',Cedente.CedenteWS.KeyUser);
       IniRetorno.WriteBool(CWebService,'IndicadorPix',Cedente.CedenteWS.IndicadorPix);
       IniRetorno.WriteString(CWebService,'Scope',Cedente.CedenteWS.Scope);
       IniRetorno.WriteInteger(CWebService,'Ambiente',Integer(Configuracoes.WebService.Ambiente));
       IniRetorno.WriteInteger(CWebService,'SSLHttpLib',Integer(Configuracoes.WebService.SSLHttpLib));

       if not SomenteConfig then
       begin
         for I:= 0 to Pred(ListadeBoletos.Count) do
         begin
           wSessao:= 'Titulo'+ IntToStr(I+1);
           IniRetorno.WriteString(wSessao,'Sacado.Nome', ListadeBoletos[I].Sacado.NomeSacado);
           IniRetorno.WriteString(wSessao,'Sacado.CNPJCPF', ListadeBoletos[I].Sacado.CNPJCPF);
           IniRetorno.WriteString(wSessao,'Vencimento',DateToStr(ListadeBoletos[I].Vencimento));
           IniRetorno.WriteString(wSessao,'DataDocumento',DateToStr(ListadeBoletos[I].DataDocumento));
           IniRetorno.WriteString(wSessao,'NumeroDocumento',ListadeBoletos[I].NumeroDocumento);
           IniRetorno.WriteString(wSessao,'DataProcessamento',DateToStr(ListadeBoletos[I].DataProcessamento));
           IniRetorno.WriteString(wSessao,'NossoNumero',ListadeBoletos[I].NossoNumero);
           IniRetorno.WriteString(wSessao,'Carteira',ListadeBoletos[I].Carteira);
           IniRetorno.WriteFloat(wSessao,'ValorDocumento',ListadeBoletos[I].ValorDocumento);
           IniRetorno.WriteString(wSessao,'DataOcorrencia',DateToStr(ListadeBoletos[I].DataOcorrencia));
           IniRetorno.WriteString(wSessao,'DataCredito',DateToStr(ListadeBoletos[I].DataCredito));
           IniRetorno.WriteString(wSessao,'DataBaixa',DateToStr(ListadeBoletos[I].DataBaixa));
           IniRetorno.WriteString(wSessao,'DataMoraJuros',DateToStr(ListadeBoletos[I].DataMoraJuros));
           IniRetorno.WriteFloat(wSessao,'ValorDespesaCobranca',ListadeBoletos[I].ValorDespesaCobranca);
           IniRetorno.WriteFloat(wSessao,'ValorAbatimento',ListadeBoletos[I].ValorAbatimento);
           IniRetorno.WriteFloat(wSessao,'ValorDesconto',ListadeBoletos[I].ValorDesconto);
           IniRetorno.WriteFloat(wSessao,'ValorMoraJuros',ListadeBoletos[I].ValorMoraJuros);
           IniRetorno.WriteFloat(wSessao,'ValorIOF',ListadeBoletos[I].ValorIOF);
           IniRetorno.WriteFloat(wSessao,'ValorOutrasDespesas',ListadeBoletos[I].ValorOutrasDespesas);
           IniRetorno.WriteFloat(wSessao,'ValorOutrosCreditos',ListadeBoletos[I].ValorOutrosCreditos);
           IniRetorno.WriteFloat(wSessao,'ValorRecebido',ListadeBoletos[I].ValorRecebido);
           IniRetorno.WriteString(wSessao,'SeuNumero',ListadeBoletos[I].SeuNumero);
           IniRetorno.WriteString(wSessao,'CodTipoOcorrencia',
                                  GetEnumName( TypeInfo(TACBrTipoOcorrencia),
                                               Integer(ListadeBoletos[I].OcorrenciaOriginal.Tipo)));
           IniRetorno.WriteString(wSessao,'DescricaoTipoOcorrencia',ListadeBoletos[I].OcorrenciaOriginal.Descricao);

           for J:= 0 to ListadeBoletos[I].DescricaoMotivoRejeicaoComando.Count-1 do
              IniRetorno.WriteString(wSessao,'MotivoRejeicao' + IntToStr(I+1),
                                     ListadeBoletos[I].DescricaoMotivoRejeicaoComando[J]);
         end;
       end;

    end;

    SL:= TStringList.Create;
    try
      IniRetorno.GetStrings(SL);
      Result:= SL.Text;
    finally
      SL.Free;
    end;

  finally
    IniRetorno.Free;
  end;

end;

function TACBrBoleto.GravarConfiguracao(DirIniRetorno: string;
  const NomeArquivo: String): Boolean;
var LArquivo : TStringList;
begin
  LArquivo := TStringList.Create;
  try
    LArquivo.Text := Self.GravarArqIni('', '',true);
    LArquivo.SaveToFile(DirIniRetorno + NomeArquivo);
    Result := true;
  finally
    LArquivo.Free;
  end;
end;

{ TListadeBoletos }
procedure TListadeBoletos.SetObject ( Index: Integer; Item: TACBrTitulo ) ;
begin
   inherited SetItem (Index, Item) ;
end;

function TListadeBoletos.GetObject ( Index: Integer ) : TACBrTitulo;
begin
   Result := inherited GetItem(Index) as TACBrTitulo ;
end;

procedure TListadeBoletos.Insert ( Index: Integer; Obj: TACBrTitulo ) ;
begin
   inherited Insert(Index, Obj);
end;

function TListadeBoletos.Add ( Obj: TACBrTitulo ) : Integer;
begin
   Result := inherited Add(Obj) ;
end;

{ TACBrBanco }
constructor TACBrBanco.Create ( AOwner: TComponent ) ;
begin
   inherited Create ( AOwner ) ;

   if not (AOwner is TACBrBoleto) then
      raise Exception.Create(ACBrStr('Aowner deve ser do tipo TACBrBoleto'));

   fACBrBoleto  := TACBrBoleto(AOwner);
   fNumeroBanco := 0;

   fBancoClass  := TACBrBancoClass.create(Self);

end;

destructor TACBrBanco.Destroy ;
begin
   fBancoClass.Free;
   inherited ;
end ;

function TACBrBanco.GetNome: String;
begin
   Result:= ACBrStr(fBancoClass.Nome);
end;

function TACBrBanco.GetDigito: Integer;
begin
   Result := fBancoClass.Digito;
end;

function TACBrBanco.GetNumero: Integer;
begin
  Result:=  BancoClass.Numero ;
end;

function TACBrBanco.GetOrientacoesBanco: TStringList;
begin
  Result:= BancoClass.OrientacoesBanco;
end;

function TACBrBanco.GetTamanhoAgencia: Integer;
begin
  Result:= BancoClass.TamanhoAgencia;
end;

function TACBrBanco.GetTamanhoCarteira: Integer;
begin
  Result:= BancoClass.TamanhoCarteira;
end;

function TACBrBanco.GetTamanhoConta: Integer;
begin
   Result:= BancoClass.TamanhoConta;
end;

function TACBrBanco.GetTamanhoMaximoNossoNum: Integer;
begin
   Result := BancoClass.TamanhoMaximoNossoNum;
end;

function TACBrBanco.GetTamanhoNumeroDocumento: Integer;
begin
  Result:= BancoClass.TamanhoNumeroDocumento;
end;

function TACBrBanco.GetCodigosMoraAceitos: String;
begin
  Result := BancoClass.CodigosMoraAceitos;
end;

function TACBrBanco.GetCodigosGeracaoAceitos: string;
begin
  Result := BancoClass.CodigosGeracaoAceitos;
end;

function TACBrBanco.GetLocalPagamento: String;
begin
   if fLocalPagamento = '' then
      if not (csDesigning in fACBrBoleto.ComponentState) then
         fLocalPagamento := fBancoClass.LocalPagamento;

   Result := fLocalPagamento;
end;

function TACBrBanco.GetNumeroCorrespondente: Integer;
begin
  Result:=  BancoClass.NumeroCorrespondente ;
end;

function TACBrBanco.GetLayoutVersaoArquivo: Integer;
begin
  Result:=  BancoClass.LayoutVersaoArquivo;
end;

function TACBrBanco.GetLayoutVersaoLote: Integer;
begin
  Result:=  BancoClass.LayoutVersaoLote;
end;

function TACBrBanco.GetCasasDecimaisMoraJuros: Integer;
begin
  Result:=  BancoClass.CasasDecimaisMoraJuros;
end;

function TACBrBanco.GetDensidadeGravacao: String;
begin
  Result:=  BancoClass.DensidadeGravacao;
end;

procedure TACBrBanco.SetDigito(const AValue: Integer);
begin
  {Apenas para aparecer no ObjectInspector do D7}
end;

procedure TACBrBanco.SetNome(const AValue: String);
begin
  {Apenas para aparecer no ObjectInspector do D7}
end;

procedure TACBrBanco.SetNumero(const AValue: Integer);
begin
  {Apenas para aparecer no ObjectInspector do D7}
end;

procedure TACBrBanco.SetLocalPagamento(const AValue: String);
begin
  fLocalPagamento := TrimRight(AValue);
end;

procedure TACBrBanco.SetNumeroCorrespondente(const AValue: Integer);
begin
  {Apenas para aparecer no ObjectInspector do D7}
end;

procedure TACBrBanco.SetLayoutVersaoArquivo(const AValue: Integer);
begin
  BancoClass.fpLayoutVersaoArquivo:= AValue;
end;

procedure TACBrBanco.SetLayoutVersaoLote(const AValue: Integer);
begin
  BancoClass.fpLayoutVersaoLote:= AValue;
end;

procedure TACBrBanco.SetCasasDecimaisMoraJuros(const AValue: Integer);
begin
  BancoClass.fpCasasDecimaisMoraJuros:= AValue;
end;

procedure TACBrBanco.SetCIP(const Value: string);
begin
  fCIP := Value;
end;

procedure TACBrBanco.SetDensidadeGravacao(const AValue: String);
begin
  BancoClass.fpDensidadeGravacao:= AValue;
end;

procedure TACBrBanco.SetTamanhoMaximoNossoNum(const Avalue: Integer);
begin
  {Altera o tamanho maximo do Nosso Numero}
  BancoClass.fpTamanhoMaximoNossoNum := AValue;
end;

procedure TACBrBanco.SetOrientacoesBanco(const Avalue: TStringList);
begin
   BancoClass.fpOrientacoesBanco.Text := AValue.Text;
end;

procedure TACBrBanco.SetTipoCobranca(const AValue: TACBrTipoCobranca);
begin
   if fTipoCobranca = AValue then
      exit;

   if fLocalPagamento = fBancoClass.LocalPagamento then   //Usando valor Default
     fLocalPagamento := '';

   fBancoClass.Free;

   case AValue of
     cobBancoDoBrasil        : fBancoClass := TACBrBancoBrasil.create(Self);         {001}
     cobBancoDoBrasilAPI     : fBancoClass := TACBrBancoBrasil.create(Self);         {001}
     cobBancoDoBrasilWS      : fBancoClass := TACBrBancoBrasil.create(Self);         {001}
     cobBancoDoBrasilSICOOB  : fBancoClass := TACBrBancoBrasilSICOOB.Create(Self);   {001}
     cobBancoDaAmazonia      : fBancoClass := TACBrBancoAmazonia.create(Self);       {003}
     cobBancoDoNordeste      : fBancoClass := TACBrBancoNordeste.create(Self);       {004}
     cobBanestes             : fBancoClass := TACBrBancoBanestes.create(Self);       {021}
     cobSantander            : fBancoClass := TACBrBancoSantander.create(Self);      {033,353,008}
     cobBanrisul             : fBancoClass := TACBrBanrisul.create(Self);            {041}
     cobBRB                  : fBancoClass := TACBrBancoBRB.create(Self);            {070}
     cobUnicredRS            : fBancoClass := TACbrBancoUnicredRS.Create(Self);      {091}
     cobBancoCECRED          : fBancoClass := TACBrBancoCecred.Create(Self);         {085}
     cobCrediSIS             : fBancoClass := TACBrBancoCrediSIS.Create(Self);       {097}
     cobUniprime             : fBancoClass := TACBrUniprime.create(Self);            {099}
     cobCaixaEconomica       : fBancoClass := TACBrCaixaEconomica.create(Self);      {104}
     cobCaixaSicob           : fBancoClass := TACBrCaixaEconomicaSICOB.create(Self); {104}
     cobUnicredES            : fBancoClass := TACBrBancoUnicredES.create(Self);      {136}
     cobBradesco             : fBancoClass := TACBrBancoBradesco.create(Self);       {237}
     cobItau                 : fBancoClass := TACBrBancoItau.Create(Self);           {341}
     cobBancoMercantil       : fBancoClass := TACBrBancoMercantil.create(Self);      {389}
     cobSicred               : fBancoClass := TACBrBancoSicredi.Create(Self);        {748}
     cobBancoob              : fBancoClass := TACBrBancoob.create(Self);             {756}
     cobHSBC                 : fBancoClass := TACBrBancoHSBC.create(Self);           {399}
     cobBicBanco             : fBancoClass := TACBrBancoBic.create(Self);            {237}
     cobBradescoSICOOB       : fBancoClass := TAcbrBancoBradescoSICOOB.create(Self); {237}
     cobBancoSafra           : fBancoClass := TACBrBancoSafra.create(Self);          {422}
     cobSafraBradesco        : fBancoClass := TACBrBancoSafraBradesco.Create(Self);  {422 + 237}
     cobBanese               : fBancoClass := TACBrBancoBanese.Create(Self);         {047}
     cobBancoCresolSCRS      : fBancoClass := TACBrBancoCresolSCRS.create(Self);     {133 + 237}
     cobCitiBank             : fBancoClass := TACBrBancoCitiBank.Create(Self);       {745}
     cobBancoABCBrasil       : fBancoClass := TACBrBancoABCBrasil.Create(Self);      {246}
     cobDaycoval             : fBancoClass := TACBrBancoDaycoval.Create(Self);       {745}
     cobUniprimeNortePR      : fBancoClass := TACBrUniprimeNortePR.Create(Self);     {084}
     cobBancoPine            : fBancoClass := TACBrBancoPine.create(Self);
     cobBancoPineBradesco    : fBancoClass := TACBrBancoPineBradesco.create(Self);   {643 + 237}
     cobUnicredSC            : fBancoClass := TACBrBancoUnicredSC.Create(Self);      {136 + 237}
     cobBancoAlfa            : fBancoClass := TACBrBancoAlfa.Create(Self);           {025}
     cobBancoCresol          : fBancoClass := TACBrBancoCresol.Create(Self);         {133}
     cobMoneyPlus            : fBancoClass := TACBrBancoBradescoMoneyPlus.create(Self); {274}
     cobBancoC6              : fBancoClass := TACBrBancoC6.Create(Self);             {336}
     cobBancoRendimento      : fBancoClass := TACBrBancoRendimento.Create(Self);     {633}
     cobBancoInter           : fBancoClass := TACBrBancoInter.Create(Self);          {077}
     cobBancoSofisaSantander : fBancoClass := TACBrBancoSofisaSantander.Create(Self); {637}
     cobBS2                  : fBancoClass := TACBrBancoBS2.Create(Self);             {218}
     cobPenseBankAPI         : fBancoClass := TACBrBancoPenseBank.Create(Self);
     cobBTGPactual           : fBancoClass := TACBrBancoBTGPactual.create(Self);     {208}
     cobBancoOriginal        : fBancoClass := TACBrBancoOriginal.Create(Self);        {212}
     cobBancoVotorantim      : fBancoClass := TACBrBancoVotorantim.create(Self);     {655}
     cobBancoPefisa          : fBancoClass := TACBrBancoPefisa.create(Self);         {174}
     cobBancoFibra           : fBancoClass := TACBrBancoFibra.create(Self);          {224}
     cobBancoSofisaItau      : fBancoClass := TACBrBancoSofisaItau.Create(Self);      {637}
     cobBancoIndustrialBrasil: fBancoClass := TACBrBancoIndustrialBrasil.Create(Self); {604}
     cobBancoAthenaBradesco  : fBancoClass := TACBrBancoAthenaBradesco.Create(Self);  {237}
   else
     fBancoClass := TACBrBancoClass.create(Self);
   end;

   fTipoCobranca := AValue;
end;

function TACBrBanco.TipoOcorrenciaToDescricao( const TipoOcorrencia: TACBrTipoOcorrencia) : String;
begin
   Result:= BancoClass.TipoOcorrenciaToDescricao(TipoOCorrencia);
end;

function TACBrBanco.CodOcorrenciaToTipo(const CodOcorrencia: Integer ) : TACBrTipoOcorrencia;
begin
   Result:= BancoClass.CodOcorrenciaToTipo(CodOcorrencia);
end;

function TACBrBanco.TipoOcorrenciaToCod (const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
   Result:= BancoClass.TipoOcorrenciaToCod(TipoOcorrencia);
end;

function TACBrBanco.CompOcorrenciaOutrosDadosToDescricao(
  const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String;
begin
  Result:= BancoClass.CompOcorrenciaOutrosDadosToDescricao(CompOcorrencia);
end;

function TACBrBanco.ConverterCodigoBarrasITF25ParaLinhaDigitavel(const ACodigoBarras: String): String;
begin
  result := OnlyNumber(BancoClass.MontarLinhaDigitavel(ACodigoBarras, nil));
end;

function TACBrBanco.CompOcorrenciaOutrosDadosToCodigo(
  const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String;
begin
  Result:= BancoClass.CompOcorrenciaOutrosDadosToCodigo(CompOcorrencia);
end;

function TACBrBanco.CodMotivoRejeicaoToDescricao( const TipoOcorrencia:
   TACBrTipoOcorrencia;CodMotivo: Integer) : String;
begin
  Result:= BancoClass.CodMotivoRejeicaoToDescricao(TipoOcorrencia,CodMotivo);
end;

function TACBrBanco.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer ) : TACBrTipoOcorrencia;
begin
   Result:= fBancoClass.CodOcorrenciaToTipoRemessa(CodOcorrencia);
end;

function TACBrBanco.TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia ) : String;
begin
   Result:= fBancoClass.TipoOcorrenciaToCodRemessa(TipoOcorrencia);
end;

function TACBrBanco.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo) : String;
begin
   Result:=  BancoClass.CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBanco.CalcularTamMaximoNossoNumero(const Carteira: String; const NossoNumero : String = '';
   const Convenio: String = ''): Integer;
begin
  Result:= BancoClass.CalcularTamMaximoNossoNumero(Carteira, NossoNumero, Convenio);
end;

function TACBrBanco.MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String;
begin
  Result:= BancoClass.MontarCampoCarteira(ACBrTitulo);
end;

function TACBrBanco.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo) : String;
begin
   Result:= BancoClass.MontarCampoNossoNumero(ACBrTitulo);
end;

function TACBrBanco.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo) : String;
begin
   Result:= BancoClass.MontarCodigoBarras(ACBrTitulo);
end;

function TACBrBanco.MontarLinhaDigitavel(const CodigoBarras:String; ACBrTitulo : TACBrTitulo) : String;
begin
   Result:= BancoClass.MontarLinhaDigitavel(CodigoBarras, ACBrTitulo);
end;

procedure TACBrBanco.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa:TStringList);
begin
  BancoClass.GerarRegistroHeader400( NumeroRemessa, ARemessa );
end;

function TACBrBanco.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  Result :=  BancoClass.GerarRegistroHeader240( NumeroRemessa );
end;

procedure TACBrBanco.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
begin
  BancoClass.GerarRegistroTransacao400( ACBrTitulo, aRemessa );
end;

function TACBrBanco.GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String;
begin
  Result := BancoClass.GerarRegistroTransacao240( ACBrTitulo );
end;

procedure TACBrBanco.GerarRegistroTrailler400(ARemessa: TStringList);
begin
  BancoClass.GerarRegistroTrailler400( ARemessa );
end;

function TACBrBanco.GerarRegistroTrailler240(ARemessa: TStringList): String;
begin
 Result :=  BancoClass.GerarRegistroTrailler240( ARemessa );
end;

procedure TACBrBanco.LerRetorno400(ARetorno: TStringList);
begin
   BancoClass.LerRetorno400(ARetorno);
end;

procedure TACBrBanco.Loaded;
begin
  inherited;

end;

procedure TACBrBanco.LerRetorno240(ARetorno: TStringList);
begin
   BancoClass.LerRetorno240(ARetorno);
end;

function TACBrBanco.CalcularNomeArquivoRemessa : String;
begin
  Result:= BancoClass.CalcularNomeArquivoRemessa ;
end;

function TACBrBanco.ValidarDadosRetorno(const AAgencia, AContaCedente: String; const ACNPJCPF: String= '';
       const AValidaCodCedente: Boolean= False ): Boolean;
begin
  Result:= BancoClass.ValidarDadosRetorno(AAgencia, AContaCedente, ACNPJCPF, AValidaCodCedente) ;
end;

function TACBrBanco.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
  Result:= BancoClass.MontarCampoCodigoCedente(ACBrTitulo);
end;

{ TACBrBancoClass }
constructor TACBrBancoClass.create(AOwner: TACBrBanco);
begin
   inherited create;

   fpAOwner                := AOwner;
   fpDigito                := 0;
   fpNome                  := 'Não definido';
   fpNumero                := 0;
   fpTamanhoMaximoNossoNum := 10;
   fpTamanhoAgencia        := 4;
   fpTamanhoConta          := 10;
   fpTamanhoNumeroDocumento:= 15;
   fpCodigosMoraAceitos    := '12';
   fpCodigosGeracaoAceitos := '0123456789';
   fpNumeroCorrespondente  := 0;
   fpLayoutVersaoArquivo   := 0;
   fpLayoutVersaoLote      := 0;
   fpCasasDecimaisMoraJuros:= 2;
   fpModuloMultiplicadorInicial:= 0;
   fpModuloMultiplicadorFinal:= 0;
   fpModuloMultiplicadorAtual:=0;
   fpCodParametroMovimento  := '';
   fpDensidadeGravacao      := '';
   fpQtdRegsLote            := 0;
   fpQtdRegsCobranca        := 0;
   fpVlrRegsCobranca        := 0;
   fpModulo                := TACBrCalcDigito.Create;
   fpOrientacoesBanco      := TStringList.Create;
end;

destructor TACBrBancoClass.Destroy;
begin
   fpModulo.Free;
   fpOrientacoesBanco.Free;
   Inherited Destroy;
end;

procedure TACBrBancoClass.EhObrigatorioAgencia;
begin
  if ACBrBanco.ACBrBoleto.Cedente.Agencia = '' then
    Raise Exception.Create(ACBrStr('Agência não informada'));
end;

procedure TACBrBancoClass.EhObrigatorioAgenciaDV;
begin
  if ACBrBanco.ACBrBoleto.Cedente.AgenciaDigito = '' then
    Raise Exception.Create(ACBrStr('Dígito da agência não informado'));
end;

procedure TACBrBancoClass.EhObrigatorioConta;
begin
  if ACBrBanco.ACBrBoleto.Cedente.Conta = '' then
    Raise Exception.Create(ACBrStr('Conta não informada'));
end;

procedure TACBrBancoClass.EhObrigatorioContaDV;
begin
  if ACBrBanco.ACBrBoleto.Cedente.ContaDigito = '' then
    Raise Exception.Create(ACBrStr('Dígito da conta não informado'));
end;

procedure TACBrBancoClass.EhObrigatorioNomeBeneficiario;
begin
  if ACBrBanco.ACBrBoleto.Cedente.Nome = '' then
    Raise Exception.Create(ACBrStr('Nome do cedente não informado'));
end;

procedure TACBrBancoClass.GerarRegistroHeader400(NumeroRemessa: Integer; ARemessa: TStringList);
var
  wLinha: String;
begin
  //ErroAbstract('GerarRemessa400');

  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    wLinha:= '0'                                                +  { ID do Registro }
             '1'                                                +  { ID do Arquivo( 1 - Remessa) }
             'REMESSA'                                          +  { Literal de Remessa }
             '01'                                               +  { Código do Tipo de Serviço }
             PadRight('COBRANCA', 15)                           +  { Descrição do tipo de serviço }
             PadLeft(CodigoCedente, 20, '0')                    +  { Codigo da Empresa no Banco }
             PadRight(Nome, 30)                                 +  { Nome da Empresa }
             IntToStrZero(fpNumero, 3)                          +  { Código do Banco 091 }
             PadRight(fpNome, 15)                               +  { Nome do Banco }
             FormatDateTime('ddmmyy',Now)                       +  { Data de geração do arquivo }
             Space(07)                                          +  { brancos }
             PadLeft(fpCodParametroMovimento, 3 )               +  { Cód. Parâm. Movto }
             IntToStrZero(NumeroRemessa, 7)                     +  { Nr. Sequencial de Remessa  }
             Space(277)                                         +  { brancos }
             IntToStrZero(1, 6);                                   { Nr. Sequencial de Remessa + brancos + Contador }

    ARemessa.Add(UpperCase(wLinha));
  end;

end;

function TACBrBancoClass.GerarRegistroHeader240(NumeroRemessa: Integer) : String;
var
  ListHeader: TStringList;
  ACodBeneficiario: String;
begin
  Result := '';
  //ErroAbstract('GerarRemessa240');

  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    if (Length( ContaDigito)  > 1) and (trim(DigitoVerificadorAgenciaConta) = '')  then
      DigitoVerificadorAgenciaConta:=  copy(ContaDigito,length(ContaDigito ),1) ;
  end;
  ACodBeneficiario:= trim(DefineCodBeneficiarioHeader);

  ListHeader:= TStringList.Create;
  try
    with ACBrBanco.ACBrBoleto.Cedente do
    begin

      { GERAR REGISTRO-HEADER DO ARQUIVO }
      ListHeader.Add(IntToStrZero(fpNumero, 3)         + //1 a 3 - Código do banco
      '0000'                                           + //4 a 7 - Lote de serviço
      '0'                                              + //8 - Tipo de registro - Registro header de arquivo
      PadRight('', 9, ' ')                             + //9 a 17 Uso exclusivo FEBRABAN/CNAB
      DefineTipoInscricao                              + //18 - Tipo de inscrição do cedente
      PadLeft(OnlyNumber(CNPJCPF), 14, '0')            + //19 a 32 -Número de inscrição do cedente
      DefineCampoConvenio(20)                          + //33 a 52 - Código do convênio no banco-Alfa
      PadLeft(OnlyNumber(Agencia), 5, '0')             + //53 a 57 - Código da agência do cedente-Numero
      DefineCampoDigitoAgencia                         + //58 - Dígito da agência do cedente -Alfa
      ifthen(ACodBeneficiario <> '',
             Padleft(ACodBeneficiario,14,'0') ,
             Padleft(Conta, 12 , '0')                  + //59-70 - Número da Conta Corrente -Numero
             DefineCampoDigitoConta                    + //71-71 -Dígito Verificador da Conta -Alfa
             DefineCampoDigitoAgenciaConta )           + //72 a 72 - Dígito Verificador da Conta
      PadRight(nome, 30, ' ')                          + //73 102 - Nome da Empresa-Alfa
      PadRight(fpNome, 30, ' ')                        + //103 a 132 -Nome do banco-Alfa
      PadRight('', 10, ' ')                            + //133 a 142 - Uso exclusivo FEBRABAN/CNAB  -Alfa
      '1'                                              + //143 - Código de Remessa (1) / Retorno (2)
      FormatDateTime('ddmmyyyy', Now)                  + //144 a 151 - Data do de geração do arquivo
      FormatDateTime('hhmmss', Now)                    + //152 a 157 - Hora de geração do arquivo
      PadLeft(IntToStr(NumeroRemessa), 6, '0')         + //158 a 163 - Número seqüencial do arquivo
      PadLeft(IntToStr(fpLayoutVersaoArquivo), 3, '0') + //164 a 166 - Número da versão do layout do arquivo
      PadLeft(fpDensidadeGravacao, 5, '0')             + //167 a 171 - Densidade de gravação do arquivo (BPI)  fixo 06250
      DefinePosicaoUsoExclusivo                        );// 172 a 240 - Uso Exclusivo FEBRABAN / CNAB

      { GERAR REGISTRO HEADER DO LOTE }

      ListHeader.Add(IntToStrZero(fpNumero, 3)   + //1 a 3 - Código do banco
      '0001'                                     + //4 a 7 - Lote de serviço
      '1'                                        + //8 - Tipo de registro - Registro header de arquivo
      'R'                                        + //9 - Tipo de operação 'R'
      '01'                                       + //10 a 11 - Tipo de serviço: 01 (Cobrança)
      '  '                                       + //12 a 13 - Uso Exclusivo FEBRABAN/CNAB /Alfa
      PadLeft(IntToStr(fpLayoutVersaoLote), 3, '0') + //14 a 16 - Número da versão do layout do lote
      ' '                                        + //17 - Uso exclusivo FEBRABAN/CNAB
      DefineTipoInscricao                        + //18 - Tipo de inscrição do cedente
      PadLeft(OnlyNumber(CNPJCPF), 15, '0')      + //19 a 33 -Número de inscrição do cedente
      DefineCampoConvenio(20)                    + //33 a 52 - Código do convênio no banco-Alfa
      Padleft(Agencia, 5, '0')                   + //54 a 58 - Agência Mantenedora da Conta
      DefineCampoDigitoAgencia                   + //59 - Dígito da agência do cedente
      ifthen(ACodBeneficiario <> '',
             Padleft(ACodBeneficiario,14,'0') ,
             Padleft(Conta, 12 , '0')            + //60 -71 Número da Conta Corrente
             DefineCampoDigitoConta              + //72 a 72 - Dígito Verificador da Conta
             DefineCampoDigitoAgenciaConta )     + //73 a 73 - Dígito Verificador da Ag/Conta
      PadRight(Nome, 30, ' ')                    + //74 a 103 - Nome do cedente
      PadRight('', 40, ' ')                      + //104 a 143 - Mensagem 1
      PadRight('', 40, ' ')                      + //144 a 183 - Mensagen 2
      PadLeft(IntToStr(NumeroRemessa), 8, '0')   + //184 a 191 - Número seqüencial do registro no lote
      FormatDateTime('ddmmyyyy', date)           +// 192 a 199 Data de Gravação Remessa/Retorno
      Padleft('0', 8 , '0')                      + //200 -207 Data do Crédito
      PadRight('', 33, ' '));                      //208 a 240 - Uso exclusivo FEBRABAN/CNAB

    end;

    Result := RemoverQuebraLinhaFinal(ListHeader.Text);
  finally
    ListHeader.Free;
  end;

end;

procedure TACBrBancoClass.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
  wLinha := '9' + Space(393)                     +  { ID Registro }
            IntToStrZero( ARemessa.Count + 1, 6);   { Contador de Registros }
  ARemessa.Add(UpperCase(wLinha));
end;

function TACBrBancoClass.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito+'/'+
             ACBrTitulo.ACBrBoleto.Cedente.Conta+'-'+
             ACBrTitulo.ACBrBoleto.Cedente.ContaDigito;
end;

function TACBrBancoClass.MontarCampoCarteira(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.Carteira;
end;

function TACBrBancoClass.GerarRegistroTrailler240(ARemessa: TStringList) : String;
var
  ListTrailler: TStringList;
begin
  Result:= '';

  if (fpQtdRegsLote = 0) then
    fpQtdRegsLote := 3 * ( ARemessa.Count - 1 ); //Corresponde a qtd linhas da remessa * qdt Segmentos do arquivo (3 é o padrão)

  {REGISTRO TRAILER DO LOTE}

  ListTrailler:= TStringList.Create;
  try
    ListTrailler.Add(IntToStrZero(fpNumero, 3)                          + //Código do Banco na Compensação 1 3 3 - Num G001
             '0001'                                                     + //Lote Lote de Serviço 4 7 4 - Num *G002
             '5'                                                        + //Tipo de Registro 8 8 1 - Num ‘5’ *G003
             Space(9)                                                   + //CNAB Uso Exclusivo FEBRABAN/CNAB 9 17 9 - Alfa Brancos G004
             IntToStrZero((fpQtdRegsLote + 2 ), 6)                      + //Qtde de Registros Quantidade de Registros no Lote 18 23 6 - Num *G057
             IntToStrZero((fpQtdRegsCobranca), 6)                       + //Quantidade de Títulos em Cobrança 24 29 6 - Num *C070  INFORMAR ZERO SEGUNDO O BANCO
             IntToStrZero( round( fpVlrRegsCobranca * 100), 17)         + //Valor Total dosTítulos em Carteiras 30 46 15 2 Num *C071     //ZERO SEGUNDO O BANCO
             PadRight('', 6, '0')                                       + //Quantidade de Títulos em Cobrança 47 52 6 - Num *C070
             PadRight('',17, '0')                                       + //Valor Total dosTítulos em Carteiras 53 69 15 2 Num *C071
             PadRight('',6,  '0')                                       + //Quantidade de Títulos em Cobrança 70 75 6 - Num *C070
             PadRight('',17, '0')                                       + //Quantidade de Títulos em Carteiras 76 92 15 2 Num *C071
             PadRight('',6,  '0')                                       + //Quantidade de Títulos em Cobrança 93 98 6 - Nim *C070
             PadRight('',17, '0')                                       + //Valor Total dosTítulos em Carteiras 99 115 15 2 Num *C071
             PadRight('',8, ' ')                                        + //Número do Aviso de Lançamento 116 123 8 - Alfa *C072
             PadRight('',117,' '))                                      ; //Uso Exclusivo FEBRABAN/CNAB 124 240 117 - Alfa Brancos G004

    {GERAR REGISTRO TRAILER DO ARQUIVO}

    ListTrailler.Add(IntToStrZero(fpNumero, 3)                          + //Código do banco
             '9999'                                                     + //Lote de serviço
             '9'                                                        + //Tipo do registro: Registro trailer do arquivo
             PadRight('',9,' ')                                         + //Uso exclusivo FEBRABAN/CNAB}
             '000001'                                                   + //Quantidade de lotes do arquivo (Registros P,Q,R, header e trailer do lote e do arquivo)
             IntToStrZero((fpQtdRegsLote + 4), 6)                       + //Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
             Padleft('0',6,'0')                                         + //Uso exclusivo FEBRABAN/CNAB}
             PadRight('',205,' '));                                       //Uso exclusivo FEBRABAN/CNAB}

    Result := RemoverQuebraLinhaFinal(ListTrailler.Text);

  finally
    fpQtdRegsLote      := 0;
    fpQtdRegsCobranca  := 0;
    fpVlrRegsCobranca  := 0;
    ListTrailler.Free;
  end;

end;

procedure TACBrBancoClass.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia  :Integer;
  CodMotivo : Variant;
  i, MotivoLinha :Integer;
  CodMotivo_19, rAgencia    :String;
  rConta, rDigitoConta      :String;
  Linha, rCedente, rCNPJCPF :String;
  rCodEmpresa               :String;
begin
  //ErroAbstract('LerRetorno400');

  //Utiliza o layout padrão para leitura de retorno CNAB400
  if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

  rCodEmpresa:= trim(Copy(ARetorno[0],27,20));
  rCedente   := trim(Copy(ARetorno[0],47,30));

  // A leitura deverá ser feita a partir da posição 26 devido ao fato de não existirem agências bancárias com mais de 4 (quatro) algarismos.
  rAgencia := trim(Copy(ARetorno[1], 26, ACBrBanco.TamanhoAgencia));
  rConta   := trim(Copy(ARetorno[1], 30, DefineTamanhoContaRemessa));

  rDigitoConta := Copy(ARetorno[1], 30 + DefineTamanhoContaRemessa ,1);

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

  ValidarDadosRetorno(rAgencia, rConta);
  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCodEmpresa <> PadLeft(Cedente.CodigoCedente, 20, '0')) then
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

       Cedente.CodigoCedente:= rCodEmpresa;
       Cedente.Nome         := rCedente;
       Cedente.Agencia      := rAgencia;
       Cedente.AgenciaDigito:= '0';
       Cedente.Conta        := rConta;
       Cedente.ContaDigito  := rDigitoConta;
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
              CodMotivo := IfThen(copy(Linha,MotivoLinha,2) = '  ','00',copy(Linha,MotivoLinha,2));

              {Se for o primeiro motivo}
              if (i = 0) then
               begin
                 {Somente estas ocorrencias possuem motivos 00}
                 if(CodOcorrencia in [02, 06, 09, 10, 15, 17])then
                  begin
                    MotivoRejeicaoComando.Add(IfThen(copy(Linha,MotivoLinha,2) = '  ','00',copy(Linha,MotivoLinha,2)));
                    if VarIsNumeric(CodMotivo) then
                      DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,Integer(CodMotivo)))
                    else
                      DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,VarToStr(CodMotivo)));
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
                       if VarIsNumeric(CodMotivo) then
                          DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,Integer(CodMotivo)))
                        else
                          DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,VarToStr(CodMotivo)));
                     end;
                  end;
               end
              else
               begin
                 //Apos o 1º motivo os 00 significam que não existe mais motivo
                 if CodMotivo <> 0 then
                 begin
                    MotivoRejeicaoComando.Add(IfThen(copy(Linha,MotivoLinha,2) = '  ','00',copy(Linha,MotivoLinha,2)));
                    if VarIsNumeric(CodMotivo) then
                      DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,Integer(CodMotivo)))
                    else
                      DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,VarToStr(CodMotivo)));
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
        if Linha[1] = '4' then
           LerRetorno400Transacao4(Titulo, Linha); // Utilizado no Bradesco Pix
     end;
  end;

end;

procedure TACBrBancoClass.LerRetorno400Transacao4(ACBrTitulo: TACBrTitulo;
  ALinha: String);
begin
  { Método implementado apenas para evitar Warnings de compilação (poderia ser abstrato)
    Você de fazer "override" desse método em todas as classes filhas de TACBrBancoClass }
end;

procedure TACBrBancoClass.LerRetorno240(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  TempData, Linha, rCedente, rCNPJCPF: String;
  ContLinha : Integer;
  rConvenioCedente: String;
  ACodBeneficiario: String;
begin
  //ErroAbstract('LerRetorno240');

  // Verifica se o arquivo pertence ao banco
  if StrToIntDef(copy(ARetorno.Strings[0], 1, 3),-1) <> Numero then
     raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                            'não' + 'é um arquivo de retorno do ' + Nome));

  ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                                          Copy(ARetorno[0],146,2)+'/'+
                                                          Copy(ARetorno[0],148,4),0, 'DD/MM/YYYY' );

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],158,6),0);

  rCedente         := trim(copy(ARetorno[0], 73, 30));

  rCNPJCPF         := DefinerCnpjCPFRetorno240(ARetorno[0]);

  rConvenioCedente := Trim(Copy(ARetorno[0], 33, 20));

  ValidarDadosRetorno('', '', rCNPJCPF);
  with ACBrBanco.ACBrBoleto do
  begin

     if LeCedenteRetorno then
     begin
       ACodBeneficiario:= trim(DefineCodBeneficiarioHeader);
       Cedente.Nome     := rCedente;
       Cedente.CNPJCPF  := rCNPJCPF;
       Cedente.Convenio := rConvenioCedente;
       Cedente.Agencia       := trim(copy(ARetorno[0], 53, 5));
       Cedente.AgenciaDigito := trim(copy(ARetorno[0], 58, 1));
       if (ACodBeneficiario <> '') then
         Cedente.CodigoCedente := trim(copy(ARetorno[0], 59, 14))
       else
       begin
         Cedente.Conta         := trim(copy(ARetorno[0], 59, 12));
         Cedente.ContaDigito   := trim(copy(ARetorno[0], 71, 1));
       end;

       if (StrToIntDef(copy(ARetorno[0], 18, 1), 0) = 1) then
         Cedente.TipoInscricao := pFisica
       else
         Cedente.TipoInscricao := pJuridica;

     end;

     ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  Linha := '';
  Titulo := nil;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
     Linha := ARetorno[ContLinha];

     if copy(Linha, 8, 1) <> '3' then // verifica se o registro (linha) é um registro detalhe (segmento J)
        Continue;

     if copy(Linha, 14, 1) = 'T' then // se for segmento T cria um novo titulo
        Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

     if Assigned(Titulo) then
     with Titulo do
     begin
        if copy(Linha, 14, 1) = 'T' then
        begin
          SeuNumero := DefineSeuNumeroRetorno(Linha);
          NumeroDocumento := DefineNumeroDocumentoRetorno(Linha);
          Carteira := copy(Linha, DefinePosicaoCarteiraRetorno, TamanhoCarteira);

          case strtoint(copy(Linha, 58, 1)) of
            1: CaracTitulo := tcSimples;
            2: CaracTitulo := tcVinculada;
            3: CaracTitulo := tcCaucionada;
            4: CaracTitulo := tcDescontada;
            5: CaracTitulo := tcVendor;
          end;

           TempData := copy(Linha, 74, 2) + '/'+copy(Linha, 76, 2)+'/'+copy(Linha, 78, 4);
           if TempData <> '00/00/0000' then
              Vencimento := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');

           ValorDocumento := StrToFloatDef(copy(Linha, 82, 15), 0) / 100;
           NossoNumero    := DefineNossoNumeroRetorno(Linha);
           ValorDespesaCobranca := StrToFloatDef(copy(Linha, 199, 15), 0) / 100;

           OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(Linha, 16, 2), 0));

           DefineRejeicaoComplementoRetorno(Linha, Titulo);

        end
        else // segmento U
        begin
           ValorIOF            := StrToFloatDef(copy(Linha, 63, 15), 0) / 100;
           ValorAbatimento     := StrToFloatDef(copy(Linha, 48, 15), 0) / 100;
           ValorDesconto       := StrToFloatDef(copy(Linha, 33, 15), 0) / 100;
           ValorMoraJuros      := StrToFloatDef(copy(Linha, 18, 15), 0) / 100;
           ValorOutrosCreditos := StrToFloatDef(copy(Linha, 123, 15), 0) / 100;
           ValorOutrasDespesas := StrToFloatDef(copy(Linha, 108, 15), 0) / 100;
           ValorRecebido       := StrToFloatDef(copy(Linha, 78, 15), 0) / 100;

           TempData            := DefineDataOcorrencia(Linha);
           if TempData <> '00/00/0000' then
               DataOcorrencia  := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');

           TempData := copy(Linha, 146, 2)+'/'+copy(Linha, 148, 2)+'/'+copy(Linha, 150, 4);
           if TempData <> '00/00/0000' then
               DataCredito     := StringToDateTimeDef(TempData, 0, 'DD/MM/YYYY');
        end;
     end;

  end;

end;

procedure TACBrBancoClass.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
begin
  { Método implementado apenas para evitar Warnings de compilação (poderia ser abstrato)
    Você de fazer "override" desse método em todas as classes filhas de TACBrBancoClass }
end;

function TACBrBancoClass.GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo) : String;
begin
   Result:= '';
end;

function TACBrBancoClass.CalcularDigitoVerificador(const ACBrTitulo :TACBrTitulo): String;
begin
  Result:= '';

  Modulo.CalculoPadrao;

  if fpModuloMultiplicadorInicial > 0 then
    Modulo.MultiplicadorInicial := fpModuloMultiplicadorInicial;
  if fpModuloMultiplicadorFinal > 0 then
    Modulo.MultiplicadorFinal := fpModuloMultiplicadorFinal;
  if fpModuloMultiplicadorAtual > 0 then
    Modulo.MultiplicadorAtual := fpModuloMultiplicadorAtual;

  Modulo.Documento := DefineNumeroDocumentoModulo(ACBrTitulo);

  Modulo.Calcular;

  Result := ConverterDigitoModuloFinal();

end;

function TACBrBancoClass.CalcularTamMaximoNossoNumero(const Carteira: String; const NossoNumero : String = '';
       const Convenio: String = ''): Integer;
begin
  Result := ACBrBanco.TamanhoMaximoNossoNum;
end;

function TACBrBancoClass.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String;
begin
  Result := '';
end ;

function TACBrBancoClass.CodOcorrenciaToTipo(const CodOcorrencia : Integer) : TACBrTipoOcorrencia;
begin
  Result := toRemessaRegistrar;
end ;

function TACBrBancoClass.TipoOcorrenciaToCod(const TipoOcorrencia : TACBrTipoOcorrencia) : String;
begin
  Result := '';
end ;

function TACBrBancoClass.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia;
       CodMotivo: Integer) : String;
begin
  Result := '';
end ;

function TACBrBancoClass.CompOcorrenciaOutrosDadosToDescricao(
  const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String;
begin
  Result := '';
end;

function TACBrBancoClass.CompOcorrenciaOutrosDadosToCodigo(
  const CompOcorrencia: TACBrComplementoOcorrenciaOutrosDados): String;
begin
  Result := '';
end;

function TACBrBancoClass.CodMotivoRejeicaoToDescricao(
const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String;
begin
  Result := '';
end;

function TACBrBancoClass.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia ;
begin
  Result := toRemessaRegistrar;
end ;

function TACBrBancoClass.TipoOcorrenciaToCodRemessa(const TipoOcorrencia : TACBrTipoOcorrencia): String ;
begin
  Result := '01';
end ;

function TACBrBancoClass.GetLocalPagamento: String;
begin
  Result := Format(ACBrStr(CInstrucaoPagamento), [fpNome] );
end;

function TACBrBancoClass.CalcularFatorVencimento(const DataVencimento: TDateTime): String;
begin
   {** Padrão para vencimentos até 21/02/2025 **}
   //Result := IntToStrZero( Max(Trunc(DataVencimento - EncodeDate(1997,10,07)),0),4 );

  {** Padrão com suporte a datas superiores a 21/02/2025
      http://www.abbc.org.br/images/content/manual%20operacional.pdf **}
   if DataVencimento = 0 then
      Result := '0000'
   else
      Result := IntToStrZero(Max((Trunc(DataVencimento) -
                                  Trunc(EncodeDate(2000,07,03))) mod 9000 + 1000, 0), 4);
end;

function TACBrBancoClass.CalcularDigitoCodigoBarras(const CodigoBarras: String) : String;
begin
   Modulo.CalculoPadrao;
   Modulo.Documento := CodigoBarras;
   Modulo.Calcular;

   if (Modulo.DigitoFinal = 0) or (Modulo.DigitoFinal > 9) then
      Result := '1'
   else
      Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoClass.CalcularNomeArquivoRemessa : String;
var
  Sequencia :Integer;
  NomeFixo, NomeArq: String;
begin
   Sequencia := 0;

   with ACBrBanco.ACBrBoleto do
   begin
      if NomeArqRemessa = '' then
       begin
         NomeFixo := DirArqRemessa + PathDelim + 'cb' + FormatDateTime( 'ddmm', Now );

         repeat
            Inc( Sequencia );
            NomeArq := NomeFixo + IntToStrZero( Sequencia, 2 ) + '.rem'
         until not FileExists( NomeArq ) ;

         Result := NomeArq;
       end
      else
         Result := DirArqRemessa + PathDelim + NomeArqRemessa ;
   end;
end;

function TACBrBancoClass.ValidarDadosRetorno(const AAgencia, AContaCedente: String; const ACNPJCPF: String= '';
   const AValidaCodCedente: Boolean= False ): Boolean;
begin
  try
    With ACBrBanco.ACBrBoleto do
    begin
      if NaoEstaVazio(ACNPJCPF) then
        if (not LeCedenteRetorno) and (ACNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
          raise Exception.CreateFmt(ACBrStr('CNPJ\CPF: %s do arquivo não corresponde aos dados do Cedente!'), [ACNPJCPF]);

      if NaoEstaVazio(AContaCedente) then
      begin
        if not AValidaCodCedente then
        begin
          if (not LeCedenteRetorno) and
             ((StrToIntDef(AAgencia,0) <> StrToIntDef(OnlyNumber(Cedente.Agencia),0)) or
              (AContaCedente <> RightStr(OnlyNumber( Cedente.Conta  ),Length(AContaCedente)))
             ) then
            raise Exception.CreateFmt(ACBrStr('Agencia: %s \ Conta: %s do arquivo não correspondem aos dados do Cedente!')
                  ,[AAgencia,AContaCedente]);
        end
        else
        begin
          if (not LeCedenteRetorno) and
             ((StrToIntDef(AAgencia,0) <> StrToIntDef(OnlyNumber(Cedente.Agencia),0)) or
              (StrToIntDef(AContaCedente,0) <> StrToIntDef(OnlyNumber( Cedente.CodigoCedente),0))
             ) then
            raise Exception.CreateFmt(ACBrStr('Agencia: %s \ Conta: %s do arquivo não correspondem aos dados do Cedente!')
                  ,[AAgencia,AContaCedente]);
        end;
      end;
    end;
  except
    {$IFNDEF COMPILER23_UP}
     Result := False;
    {$ENDIF}
    raise;
  end;

  Result := True;
end;

function TACBrBancoClass.FormatarMoraJurosRemessa(const APosicoes: Integer; const ACBrTitulo: TACBrTitulo): String;
var
  Multiplicador: integer;
begin
  Multiplicador:=  StrToIntDef('1' + IntToStrZero(0, fpCasasDecimaisMoraJuros), 100);
  Result :=  IntToStrZero(round(ACBrTitulo.ValorMoraJuros * Multiplicador ), APosicoes) ;
end;

function TACBrBancoClass.DefineNumeroDocumentoModulo(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.Carteira + ACBrTitulo.NossoNumero;
end;

function TACBrBancoClass.DefineNumeroDocumentoRetorno(
  const ALinha: String): String;
begin
  Result := copy(ALinha, 59, fpTamanhoNumeroDocumento);
end;

function TACBrBancoClass.ConverterDigitoModuloFinal: String;
begin
    Result:= IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoClass.DefineCampoLivreCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := '';
end;

procedure TACBrBancoClass.ValidaNossoNumeroResponsavel(out ANossoNumero: String; out ADigVerificador: String;
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
    ANossoNumero := ACBrTitulo.NossoNumero;
    ADigVerificador := CalcularDigitoVerificador(ACBrTitulo);
    if (ANossoNumero = EmptyStr) then
      ADigVerificador := '0';
  end;
end;

function TACBrBancoClass.MontaInstrucoesCNAB400(const ACBrTitulo: TACBrTitulo; const nRegistro: Integer): String;
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
     Result := '2'               +                                         // IDENTIFICAÇÃO DO LAYOUT PARA O REGISTRO
               Copy(PadRight(TiraAcentos(Mensagem[1]), 80, ' '), 1, 80);                // CONTEÚDO DA 1ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO

     if Mensagem.Count >= 3 then
        Result := Result +
                  Copy(PadRight(TiraAcentos(Mensagem[2]), 80, ' '), 1, 80)              // CONTEÚDO DA 2ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
     else
        Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

     if Mensagem.Count >= 4 then
        Result := Result +
                  Copy(PadRight(TiraAcentos(Mensagem[3]), 80, ' '), 1, 80)              // CONTEÚDO DA 3ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
     else
        Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS

     if Mensagem.Count >= 5 then
        Result := Result +
                  Copy(PadRight(TiraAcentos(Mensagem[4]), 80, ' '), 1, 80)              // CONTEÚDO DA 4ª LINHA DE IMPRESSÃO DA ÁREA "INSTRUÇÕES” DO BOLETO
     else
        Result := Result + PadRight('', 80, ' ');                          // CONTEÚDO DO RESTANTE DAS LINHAS


     Result := Result                                              +
               PadLeft('', 6,  '0')                                +       // 322 a 327 - Data limite para concessão de Desconto 2
               PadLeft('', 13, '0')                                +       // 328 a 340 - Valor do Desconto 2
               PadLeft('', 6,  '0')                                +       // 341 a 346 - Data limite para concessão de Desconto 3
               PadLeft('', 13, '0')                                +       // 347 a 359 - Valor do Desconto 3
               space(7)                                            +       // 360 a 366 - Reserva
               IntToStrZero(StrToIntDef(trim(Carteira), 0), 3)                         +
               IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Agencia), 0), 5) +
               IntToStrZero(StrToIntDef(OnlyNumber(ACBrBoleto.Cedente.Conta)  , 0), 7) +
               Cedente.ContaDigito                                                     +
               lNossoNumero + lDigNossoNumero                                          +
               IntToStrZero( nRegistro + 1, 6);                            // Nº SEQÜENCIAL DO REGISTRO NO ARQUIVO
  end;

end;

function TACBrBancoClass.DefineEspecieDoc(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if ACBrBanco.ACBrBoleto.LayoutRemessa = c240 then
    begin
      if AnsiSameText(EspecieDoc, 'CH') then
        Result := '01'
      else
      if AnsiSameText(EspecieDoc, 'DM') then
        Result := '02'
      else
      if AnsiSameText(EspecieDoc, 'DMI') then
        Result := '03'
      else
      if AnsiSameText(EspecieDoc, 'DS') then
        Result := '04'
      else
      if AnsiSameText(EspecieDoc, 'DSI') then
        Result := '05'
      else
      if AnsiSameText(EspecieDoc, 'DR') then
        Result := '06'
      else
      if AnsiSameText(EspecieDoc, 'LC') then
        Result := '07'
      else
      if AnsiSameText(EspecieDoc, 'NCC') then
        Result := '08'
      else
      if AnsiSameText(EspecieDoc, 'NCE') then
        Result := '09'
      else
      if AnsiSameText(EspecieDoc, 'NCI') then
        Result := '10'
      else
      if AnsiSameText(EspecieDoc, 'NCR') then
        Result := '11'
      else
      if AnsiSameText(EspecieDoc, 'NP') then
        Result := '12'
      else
      if AnsiSameText(EspecieDoc, 'NPR') then
        Result := '13'
      else
      if AnsiSameText(EspecieDoc, 'TM') then
        Result := '14'
      else
      if AnsiSameText(EspecieDoc, 'TS') then
        Result := '15'
      else
      if AnsiSameText(EspecieDoc, 'NS') then
        Result := '16'
      else
      if AnsiSameText(EspecieDoc, 'RC') then
        Result := '17'
      else
      if AnsiSameText(EspecieDoc, 'FAT') then
        Result := '18'
      else
      if AnsiSameText(EspecieDoc, 'ND') then
        Result := '19'
      else
      if AnsiSameText(EspecieDoc, 'AP') then
        Result := '20'
      else
      if AnsiSameText(EspecieDoc, 'ME') then
        Result := '21'
      else
      if AnsiSameText(EspecieDoc, 'PC') then
        Result := '22'
      else
      if AnsiSameText(EspecieDoc, 'NF') then
        Result := '23'
      else
      if AnsiSameText(EspecieDoc, 'DD') then
        Result := '24'
      else
      if AnsiSameText(EspecieDoc, 'CPR') then
        Result := '25'
      else
        Result := '99';
    end
    else
    begin
      if AnsiSameText(EspecieDoc,'DM') then
         Result:= '01'
      else if AnsiSameText(EspecieDoc, 'NP') then
         Result:= '02'
      else if AnsiSameText(EspecieDoc, 'NS') then
         Result:= '03'
      else if AnsiSameText(EspecieDoc, 'CS') then
         Result:= '04'
      else if AnsiSameText(EspecieDoc, 'REC') then
         Result:= '05'
      else if AnsiSameText(EspecieDoc, 'LC') then
         Result:= '10'
      else if AnsiSameText(EspecieDoc, 'ND') then
         Result:= '11'
      else if AnsiSameText(EspecieDoc, 'DS') then
         Result:= '12'
      else if AnsiSameText(EspecieDoc, 'BDP') then
         Result:= '32'
      else if AnsiSameText(EspecieDoc, 'OU') then
         Result:= '99'
      else
         Result := EspecieDoc;

    end;
  end;

end;

function TACBrBancoClass.DefineTipoSacado(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case Sacado.Pessoa of
        pFisica   : Result := '01';
        pJuridica : Result := '02';
     else
        Result := '99';
     end;

  end;
end;

function TACBrBancoClass.DefineTipoSacadoAvalista(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case Sacado.SacadoAvalista.Pessoa of
        pFisica   : Result := '1';
        pJuridica : Result := '2';
        pOutras   : Result := '9';
     else
        Result := '0';
     end;
  end;
end;

function TACBrBancoClass.DefineTipoBoleto(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if CarteiraEnvio = tceCedente then
      Result := '2'
    else
      Result := '1';
  end;

end;

function TACBrBancoClass.DefineTipoInscricao: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    case TipoInscricao of
        pFisica  : Result := '1';
      else
        Result := '2';
    end;
  end;

end;

procedure TACBrBancoClass.DefineRejeicaoComplementoRetorno(const ALinha: String; out ATitulo : TACBrTitulo);
var 
	LIdxMotivo : Integer;
begin
  LIdxMotivo := 214;

  while (LIdxMotivo < 223) do
  begin
    if (trim(Copy(ALinha, LIdxMotivo, 2)) <> '')  and (trim(Copy(ALinha, LIdxMotivo, 2)) <> '00') then
    begin
       ATitulo.MotivoRejeicaoComando.Add(Copy(ALinha, LIdxMotivo, 2));
       ATitulo.DescricaoMotivoRejeicaoComando.Add(ATitulo.ACBrBoleto.Banco.CodMotivoRejeicaoToDescricao(ATitulo.OcorrenciaOriginal.Tipo, StrToIntDef(Copy(ALinha, LIdxMotivo, 2), 0)));
    end;
    Inc(LIdxMotivo, 2);
  end;
end;

function TACBrBancoClass.DefineResponsEmissao: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    case ACBrBoleto.Cedente.ResponEmissao of
       tbBancoEmite      : Result := '1';
       tbBancoReemite    : Result := '4';
       tbBancoNaoReemite : Result := '5';
    else
         Result :=  '2';
    end;
  end;
end;

function TACBrBancoClass.DefineSeuNumeroRetorno(const ALinha: String): String;
begin
  Result := copy(ALinha, 106, 25);
end;

function TACBrBancoClass.DefineCaracTitulo(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case CaracTitulo of
      tcVinculada   : Result :='2';
      tcCaucionada  : Result :='3';
      tcDescontada  : Result :='4';
      tcVendor      : Result :='5';
    else
        Result := '1';
    end;
  end;
end;

function TACBrBancoClass.DefineCodigoProtesto(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case CodigoNegativacao of
        cnNenhum           :  Result := '0';
        cnProtestarCorrido :  Result := '1';
        cnProtestarUteis   :  Result := '2';
        cnNaoProtestar     :  Result := '3';
        cnNegativar        :  Result := '7';
        cnNaoNegativar     :  Result := '8';
       else
        Result := '0';
      end;
  end;
end;

function TACBrBancoClass.DefineTipoDiasProtesto(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case TipoDiasProtesto of
      diCorridos       : Result := '1';
      diUteis          : Result := '2';
    else
      Result := '3';
    end;
  end;
end;

function TACBrBancoClass.DefineAceite(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case Aceite of
         atSim : Result := 'A';
       else
         Result := 'N';
    end;
  end;
end;

function TACBrBancoClass.DefineAceiteImpressao(const ACBrTitulo: TACBrTitulo): String;
begin
  case ACBrTitulo.Aceite of
    atSim :
      Result := 'S';
  else
    Result := 'N';
  end;
end;

function TACBrBancoClass.DefineCodigoMoraJuros(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if (CodigoMora <> '') then
      Result := CodigoMora
    else
    begin
      if (ValorMoraJuros > 0) then
      begin
        case CodigoMoraJuros of
          cjTaxaMensal, cjValorMensal   :
            Result := '2';
        else
          Result := '1';
        end;
      end
      else
        Result := '3';
    end
  end;
end;

function TACBrBancoClass.DefineDataMoraJuros(const ACBrTitulo: TACBrTitulo; AFormat: String): String;
begin
  with ACBrTitulo do
  begin
    if (ValorMoraJuros > 0) then
    begin
      if DataMoraJuros > 0 then
        Result := FormatDateTime(AFormat, DataMoraJuros)
      else
        Result := PadRight('', Length(AFormat), '0');
    end
    else
      Result := PadRight('', Length(AFormat), '0');
  end;
end;

function TACBrBancoClass.DefineCodigoDesconto(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if (ValorDesconto > 0) then
    begin
      case TipoDesconto of
         tdValorFixoAteDataInformada : Result := '1';
         tdPercentualAteDataInformada: Result := '2';
        else
          Result := '1';
      end;
    end
    else
      Result := '0';
  end;
end;

function TACBrBancoClass.DefineDataDesconto(const ACBrTitulo: TACBrTitulo; AFormat: String): String;
begin
  with ACBrTitulo do
  begin
    if (ValorDesconto > 0) then
    begin
      if (DataDesconto > 0) then
        Result := FormatDateTime(AFormat, DataDesconto)
      else
        Result := PadRight('', Length(AFormat), '0');
    end
    else
      Result := PadRight('', Length(AFormat), '0');
  end;
end;

function TACBrBancoClass.DefineCodigoMulta(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    if (PercentualMulta > 0) then
      Result := '2'
    else
      Result := '0';
  end;
end;

function TACBrBancoClass.DefineDataMulta(const ACBrTitulo: TACBrTitulo; AFormat: String): String;
begin
  with ACBrTitulo do
  begin
    if (PercentualMulta > 0) then
      Result :=  IfThen(DataMulta > 0,FormatDateTime(AFormat, DataMulta),FormatDateTime(AFormat, Vencimento + 1))
    else
      Result := PadRight('', Length(AFormat), '0');
  end;
end;

function TACBrBancoClass.DefineDataOcorrencia(const ALinha: String): String;
begin
  Result := copy(ALinha, 138, 2)+'/'+copy(ALinha, 140, 2)+'/'+copy(ALinha, 142, 4);
end;

function TACBrBancoClass.DefineTipoDocumento: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    case TipoDocumento of
      Tradicional: Result := '1';
      Escritural: Result := '2';
    else
      Result := '1';
    end;
  end;
end;

function TACBrBancoClass.DefineCampoDigitoConta: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    Result :=  PadLeft(ContaDigito, 1, '0');
  end;
end;

function TACBrBancoClass.DefineCampoDigitoAgenciaConta: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    Result :=  PadLeft(DigitoVerificadorAgenciaConta, 1, ' ');
  end;
end;

function TACBrBancoClass.DefinePosicaoUsoExclusivo: String;
begin
  Result := Space(20)                                        + // 172 a 191 - Uso reservado do banco
            PadRight('REMESSA-PRODUCAO', 20, ' ')            + // 192 a 211 - Uso reservado da empresa
            PadRight('', 29, ' ');                            // 212 a 240 - Uso Exclusivo FEBRABAN / CNAB
end;

function TACBrBancoClass.DefinerCnpjCPFRetorno240(const ALinha: String): String;
begin
  Result := OnlyNumber( copy(ALinha, 19, 14) );
end;

function TACBrBancoClass.DefineCodBeneficiarioHeader: String;
begin
  //Por Default não informa beneficiario no Header
  Result:= '';
end;

function TACBrBancoClass.DefineCampoConvenio(const ATam: Integer): String;
begin
  Result := PadLeft(ACBrBanco.ACBrBoleto.Cedente.Convenio, ATam, '0');
end;

function TACBrBancoClass.DefineCampoDigitoAgencia: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    Result :=  PadLeft(AgenciaDigito, 1, '0');
  end;
end;

function TACBrBancoClass.DefineTamanhoContaRemessa: Integer;
begin
  Result:= ACBrBanco.TamanhoConta;
end;

function TACBrBancoClass.DefineTamanhoAgenciaRemessa: Integer;
begin
  Result:= ACBrBanco.TamanhoAgencia;
end;

function TACBrBancoClass.DefinePosicaoNossoNumeroRetorno: Integer;
begin
  if ACBrBanco.ACBrBoleto.LayoutRemessa = c240 then
    Result := 46
  else
    Result := 71;
end;

function TACBrBancoClass.DefineTamanhoNossoNumeroRetorno: Integer;
begin
  Result := TamanhoMaximoNossoNum;
end;

function TACBrBancoClass.DefinePosicaoCarteiraRetorno: Integer;
begin
  if ACBrBanco.ACBrBoleto.LayoutRemessa = c240 then
    Result := 39
  else
    Result := 22;
end;

function TACBrBancoClass.DefineNossoNumeroRetorno(const Retorno: String): String;
begin
  if ACBrBanco.ACBrBoleto.LerNossoNumeroCompleto then
    Result := Copy(Retorno,DefinePosicaoNossoNumeroRetorno,TamanhoMaximoNossoNum)
  else
    Result := Copy(Retorno,DefinePosicaoNossoNumeroRetorno,DefineTamanhoNossoNumeroRetorno);
end;

function TACBrBancoClass.InstrucoesProtesto(const ACBrTitulo: TACBrTitulo): String;
begin
  {Intruções}
  with ACBrTitulo do
  begin
    if ((DataProtesto > 0) and (DataProtesto > Vencimento)) then
      Result := IfThen(PadLeft(trim(Instrucao1),2,'0') = '00','06',PadLeft(trim(Instrucao1),2,'0'))
                 + IntToStrZero(DaysBetween(DataProtesto,Vencimento),2)
    else if TipoOcorrenciaToCodRemessa(OcorrenciaOriginal.Tipo) = '31' then
      Result := '9999'
    else if ((DataBaixa > 0) and (DataBaixa > Vencimento)) then
      Result := PadLeft(trim(Instrucao2),2,'0') + IntToStrZero(DaysBetween(DataBaixa,Vencimento),2)
    else
      Result := PadLeft(trim(Instrucao1),2,'0') + PadLeft(trim(Instrucao2),2,'0');
  end;
end;

function TACBrBancoClass.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo) : String;
var
  sCodigoBarras   : String;
  sFatorVencimento: String;
  sDigitoCodBarras: String;
begin
  with ACBrTitulo.ACBrBoleto do
  begin
    sFatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

    sCodigoBarras :=     IntToStrZero(Numero, 3) +                                              { Instituição Financ }
                         '9' +                                                                  { Cód. Moeda = Real }
                         sFatorVencimento +                                                     { Fator Vencimento }
                         IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +             { Valor Nominal }
                         DefineCampoLivreCodigoBarras(ACBrTitulo);                              { Campo Livre}

    sDigitoCodBarras := CalcularDigitoCodigoBarras(sCodigoBarras);
  end;

  { Insere o digito verificador calculado no código de barras }
  Result := copy( sCodigoBarras, 1, 4) + sDigitoCodBarras + Copy(sCodigoBarras, 5, 39);

end;

function TACBrBancoClass.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo) : String;
begin
   Result:= ACBrTitulo.NossoNumero;
end;

function TACBrBancoClass.MontarLinhaDigitavel (const CodigoBarras: String; ACBrTitulo : TACBrTitulo): String;
var
  Campo1, Campo2, Campo3, Campo4, Campo5: String;
begin
   fpModulo.FormulaDigito        := frModulo10;
   fpModulo.MultiplicadorInicial := 1;
   fpModulo.MultiplicadorFinal   := 2;
   fpModulo.MultiplicadorAtual   := 2;


  {Campo 1(Código Banco,Tipo de Moeda,5 primeiro digitos do Campo Livre) }
   fpModulo.Documento := Copy(CodigoBarras,1,3)+'9'+Copy(CodigoBarras,20,5);
   fpModulo.Calcular;

   Campo1 := copy( fpModulo.Documento, 1, 5) + '.' +
             copy( fpModulo.Documento, 6, 4) +
             IntToStr( fpModulo.DigitoFinal );

  {Campo 2(6ª a 15ª posições do campo Livre)}
   fpModulo.Documento:= copy( CodigoBarras, 25, 10);
   fpModulo.Calcular;

   Campo2 := Copy( fpModulo.Documento, 1, 5) + '.' +
             Copy( fpModulo.Documento, 6, 5) +
             IntToStr( fpModulo.DigitoFinal );

  {Campo 3 (16ª a 25ª posições do campo Livre)}
   fpModulo.Documento:= copy( CodigoBarras, 35, 10);
   fpModulo.Calcular;

   Campo3 := Copy( fpModulo.Documento, 1, 5) + '.' +
             Copy( fpModulo.Documento, 6, 5) +
             IntToStr( fpModulo.DigitoFinal );

  {Campo 4 (Digito Verificador Nosso Numero)}
   Campo4 := Copy( CodigoBarras, 5, 1);

  {Campo 5 (Fator de Vencimento e Valor do Documento)}
   Campo5 := Copy( CodigoBarras, 6, 14);

   Result := Campo1+' '+Campo2+' '+Campo3+' '+Campo4+' '+Campo5;
end;

{ TACBrBoletoFCClass }
constructor TACBrBoletoFCClass.Create ( AOwner: TComponent ) ;
begin
  inherited Create ( AOwner ) ;

  fACBrBoleto          := nil;
  fLayOut              := lPadrao;
  fNumCopias           := 1;
  fMostrarPreview      := True;
  fMostrarSetup        := True;
  fMostrarProgresso    := True;
  fFiltro              := fiNenhum;
  fNomeArquivo         := '' ;
  fPathNomeArquivo     := '' ;
  fPrinterName         := '' ;
  FAlterarEscalaPadrao := False;
  FNovaEscala          := 96;
  FIndiceImprimirIndividual := -1;
  FCalcularNomeArquivoPDFIndividual := True;

end;

function TACBrBoletoFCClass.DefineAceiteImpressao(const ACBrTitulo: TACBrTitulo): String;
begin
  Result:= ACBrBoleto.Banco.BancoClass.DefineAceiteImpressao(ACBrTitulo);
end;

procedure TACBrBoletoFCClass.Notification ( AComponent: TComponent;
   Operation: TOperation ) ;
begin
   inherited Notification ( AComponent, Operation ) ;

   if (Operation = opRemove) and (fACBrBoleto <> nil) and (AComponent is TACBrBoleto) then
      fACBrBoleto := nil ;
end;

function TACBrBoletoFCClass.TituloRelatorio: String;
begin
  Result := 'Boleto - '+ACBrBoleto.Banco.Nome+
            ' Ag:'+ACBrBoleto.Cedente.Agencia+'-'+ACBrBoleto.Cedente.AgenciaDigito+
            ' Conta:'+ACBrBoleto.Cedente.Conta+'-'+ACBrBoleto.Cedente.ContaDigito;
end;

procedure TACBrBoletoFCClass.CarregaLogo(const PictureLogo : TPicture; const NumeroBanco: Integer ) ;
begin
  if Assigned( fOnObterLogo ) then
     fOnObterLogo( PictureLogo, NumeroBanco)
  else
   begin
     if FileExists( ArquivoLogo ) then
        PictureLogo.LoadFromFile( ArquivoLogo );
   end ;
end ;

procedure TACBrBoletoFCClass.SetACBrBoleto ( const Value: TACBrBoleto ) ;
  Var OldValue : TACBrBoleto ;
begin
  if Value <> fACBrBoleto then
  begin
    if Assigned(fACBrBoleto) then
        fACBrBoleto.RemoveFreeNotification(Self);

     OldValue    := fACBrBoleto ;   // Usa outra variavel para evitar Loop Infinito
     fACBrBoleto := Value;          // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.ACBrBoletoFC) then
           OldValue.ACBrBoletoFC := nil ;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        Value.ACBrBoletoFC := self ;
     end ;
  end ;

end;

procedure TACBrBoletoFCClass.SetDirLogo(const AValue: String);
begin
  fDirLogo := PathWithoutDelim( Trim(AValue) );
end;

procedure TACBrBoletoFCClass.SetNomeArquivo(const AValue: String);
begin
  fNomeArquivo := Trim(AValue);
  fPathNomeArquivo := '';
end;

function TACBrBoletoFCClass.GetArquivoLogo: String;
var LArquivo : String;
begin
  LArquivo := PathWithDelim(DirLogo) + IntToStrZero( ACBrBoleto.Banco.Numero, 3);
  Result := LArquivo + '.bmp';
  {$IFDEF COMPILER7_UP}
    if FileExists(LArquivo + '.png') then
      Result := LArquivo + '.png';
  {$ENDIF}
end;

function TACBrBoletoFCClass.ComponentStateDesigning: Boolean;
begin
  Result := (csDesigning in Self.ComponentState);

  if not Result then
    if Assigned(fACBrBoleto) then
      Result := (csDesigning in fACBrBoleto.ComponentState);
end;

function TACBrBoletoFCClass.GetDirLogo: String;
begin
  if (fDirLogo = '') then
    if not ComponentStateDesigning then
      fDirLogo := ApplicationPath + 'Logos' ;

  Result := fDirLogo ;
end;

function TACBrBoletoFCClass.GetNomeArquivo: String;
var
  wPath, wFile: String;
begin
  if ComponentStateDesigning then
    Result := fNomeArquivo
  else
  begin
    if (fPathNomeArquivo = '') then
    begin
      wPath := ExtractFilePath(fNomeArquivo);
      if (wPath = '') then
        wPath := ApplicationPath;

      wFile := ExtractFileName(fNomeArquivo);
      if (wFile = '') then
        wFile := 'boleto';

      fPathNomeArquivo := trim(PathWithDelim(wPath) + wFile);
    end;

    Result := fPathNomeArquivo;
  end;
end;

function TACBrBoletoFCClass.GetIndiceImprimirIndividual: Integer;
begin
   Result := FIndiceImprimirIndividual;
end;

function TACBrBoletoFCClass.GetNomeArquivoPdfIndividual(const ANomeArquivo: String;
         const AIndex: Integer): String;
var
  lNumDocumento: String;
begin
  lNumDocumento := OnlyNumber(ACBrBoleto.ListadeBoletos[AIndex].NumeroDocumento);
  if ANomeArquivo = '' then
    Result := PathWithDelim( ApplicationPath ) + ChangeFileExt(lNumDocumento, '.pdf')
  else
    Result := ChangeFileExt( ChangeFileExt(ANomeArquivo,'') + '_' + lNumDocumento, '.pdf');
end;

procedure TACBrBoletoFCClass.SetNumCopias ( AValue: Integer ) ;
begin
  // O valor de cópias zero é utilizado por aplicações ISAPI no momento.
  // É utilizado por causa de problemas encontrados ao usar o Fortes Report.
  fNumCopias := max( 0, Avalue);
end;

procedure TACBrBoletoFCClass.SetPdfSenha(const Value: string);
begin
  FPdfSenha := Value;
end;

procedure TACBrBoletoFCClass.SetTituloPreview(const Value: string);
begin
  if Value <> FTituloPreview then
    FTituloPreview := Value;
end;

procedure TACBrBoletoFCClass.SetIndiceImprimirIndividual(const Value: Integer);
begin
  if Value <> FIndiceImprimirIndividual then
    FIndiceImprimirIndividual:= Value;
end;

procedure TACBrBoletoFCClass.Imprimir;
begin
   if not Assigned(fACBrBoleto) then
      raise Exception.Create(ACBrStr('Componente não está associado a ACBrBoleto'));

   if fACBrBoleto.ListadeBoletos.Count < 1 then
      raise Exception.Create(ACBrStr('Lista de Boletos está vazia'));
end;

procedure TACBrBoletoFCClass.Imprimir(AStream: TStream);
begin
   if not Assigned(fACBrBoleto) then
      raise Exception.Create(ACBrStr('Componente não está associado a ACBrBoleto'));

   if not Assigned(AStream) then
      raise Exception.Create(ACBrStr('Stream não pode ser nulo'));

   if fACBrBoleto.ListadeBoletos.Count < 1 then
      raise Exception.Create(ACBrStr('Lista de Boletos está vazia'));
end;

procedure TACBrBoletoFCClass.GerarPDF;
var
   FiltroAntigo         : TACBrBoletoFCFiltro;
   MostrarPreviewAntigo : Boolean;
   MostrarSetupAntigo   : Boolean;
   CalcularIndividual   : Boolean;
   PrinterNameAntigo    : String;
   NomeArquivoAntigo    : String;
begin
   if NomeArquivo = '' then
      raise Exception.Create( ACBrStr('NomeArquivo não especificado')) ;

   if not Assigned(fACBrBoleto) then
       raise Exception.Create(ACBrStr('Componente não está associado a ACBrBoleto'));

   FiltroAntigo         := Filtro;
   MostrarPreviewAntigo := MostrarPreview;
   MostrarSetupAntigo   := MostrarSetup;
   PrinterNameAntigo    := PrinterName;
   NomeArquivoAntigo    := NomeArquivo;

   Filtro         := fiPDF;
   MostrarPreview := false;
   MostrarSetup   := false;
   PrinterName    := '';
   CalcularIndividual := (FIndiceImprimirIndividual >= 0) and (FCalcularNomeArquivoPDFIndividual);

   try

     if fACBrBoleto.ListadeBoletos.Count < 1 then
       raise Exception.Create(ACBrStr('Lista de Boletos está vazia'));

     if CalcularIndividual then
     begin
       fPathNomeArquivo:= '';
       NomeArquivo := GetNomeArquivoPdfIndividual(NomeArquivoAntigo, FIndiceImprimirIndividual);
     end
     else
       NomeArquivo := ChangeFileExt(NomeArquivo, '.pdf');

     Imprimir;
   finally
     Filtro         := FiltroAntigo;
     MostrarPreview := MostrarPreviewAntigo;
     MostrarSetup   := MostrarSetupAntigo;
     PrinterName    := PrinterNameAntigo;
     if CalcularIndividual then
       NomeArquivo := NomeArquivoAntigo;
   end;
end;

procedure TACBrBoletoFCClass.GerarPDF(AIndex: Integer);
var
   AOldIndex: Integer;
begin
  AOldIndex := FIndiceImprimirIndividual;
  try
    FIndiceImprimirIndividual := AIndex;
    GerarPDF;
  finally
    FIndiceImprimirIndividual := AOldIndex;
  end;
end;

procedure TACBrBoletoFCClass.GerarPDF(AStream: TStream);
var
   FiltroAntigo         : TACBrBoletoFCFiltro;
   MostrarPreviewAntigo : Boolean;
   MostrarSetupAntigo   : Boolean;
begin
   if not Assigned(fACBrBoleto) then
       raise Exception.Create(ACBrStr('Componente não está associado a ACBrBoleto'));

   FiltroAntigo         := Filtro;
   MostrarPreviewAntigo := MostrarPreview;
   MostrarSetupAntigo   := MostrarSetup;

   try
     Filtro         := fiPDF;
     MostrarPreview := false;
     MostrarSetup   := false;

     Imprimir(AStream);
   finally
     Filtro         := FiltroAntigo;
     MostrarPreview := MostrarPreviewAntigo;
     MostrarSetup   := MostrarSetupAntigo;
   end;
end;

procedure TACBrBoletoFCClass.GerarPDF(AIndex: Integer; AStream: TStream);
var
   AOldIndex: Integer;
begin
  AOldIndex := FIndiceImprimirIndividual;
  try
    FIndiceImprimirIndividual := AIndex;
    GerarPDF(AStream);
  finally
    FIndiceImprimirIndividual := AOldIndex;
  end;
end;

procedure TACBrBoletoFCClass.GerarHTML;
var
   FiltroAntigo         : TACBrBoletoFCFiltro;
   MostrarPreviewAntigo : Boolean;
   MostrarSetupAntigo   : Boolean;
   PrinterNameAntigo    : String;
begin
   if NomeArquivo = '' then
      raise Exception.Create( ACBrStr('NomeArquivo não especificado')) ;

   NomeArquivo := ChangeFileExt(NomeArquivo, '.html');

   FiltroAntigo         := Filtro;
   MostrarPreviewAntigo := MostrarPreview;
   MostrarSetupAntigo   := MostrarSetup;
   PrinterNameAntigo    := PrinterName;
   try
     Filtro         := fiHTML;
     MostrarPreview := false;
     MostrarSetup   := false;
     PrinterName    := '';
     Imprimir;
   finally
     Filtro         := FiltroAntigo;
     MostrarPreview := MostrarPreviewAntigo;
     MostrarSetup   := MostrarSetupAntigo;
     PrinterName    := PrinterNameAntigo;
   end;
end;

procedure TACBrBoletoFCClass.GerarJPG;
var
   FiltroAntigo         : TACBrBoletoFCFiltro;
   MostrarPreviewAntigo : Boolean;
   MostrarSetupAntigo   : Boolean;
   PrinterNameAntigo    : String;
begin
   if NomeArquivo = '' then
      raise Exception.Create( ACBrStr('NomeArquivo não especificado')) ;

   NomeArquivo := ChangeFileExt(NomeArquivo, '.jpg');

   FiltroAntigo         := Filtro;
   MostrarPreviewAntigo := MostrarPreview;
   MostrarSetupAntigo   := MostrarSetup;
   PrinterNameAntigo    := PrinterName;
   try
     Filtro         := fiJPG;
     MostrarPreview := false;
     MostrarSetup   := false;
     PrinterName    := '';
     Imprimir;
   finally
     Filtro         := FiltroAntigo;
     MostrarPreview := MostrarPreviewAntigo;
     MostrarSetup   := MostrarSetupAntigo;
     PrinterName    := PrinterNameAntigo;
   end;
end;

{ TACBrOcorrencia }
function TACBrOcorrencia.GetCodigoBanco: String;
begin
   Result:= fpAowner.AcbrBoleto.Banco.TipoOcorrenciaToCod(Tipo);
end;

function TACBrOcorrencia.GetDescricao: String;
begin
   //if Tipo <> 0 then
      Result:= fpAowner.ACBrBoleto.Banco.TipoOcorrenciaToDescricao(Tipo)
   //else
   //   Result:= '';
end;

constructor TACBrOcorrencia.Create(AOwner: TACBrTitulo);
begin
   fTipo := toRemessaRegistrar;
   fpAOwner:= AOwner;
end;

{$ifdef FPC}
initialization
   {$I ACBrBoleto.lrs}
{$endif}

end.
