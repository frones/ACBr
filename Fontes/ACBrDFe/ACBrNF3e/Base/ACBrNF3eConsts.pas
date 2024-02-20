{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrNF3eConsts;

interface

uses
  SysUtils;

const
  NAME_SPACE_NF3e = 'xmlns="http://www.portalfiscal.inf.br/nf3e"';

  DSC_INFQRCODE = 'Texto com o QR-Code impresso no DANF3e NF3-e.';
  DSC_FINNF3e = 'Finalidade de emissão da NF3e';
  DSC_IDESTR = 'Documento de Identificação do Estrangeiro';
  DSC_INDIEDEST = 'Indicador da IE do Destinatário';
  DSC_IDACESSO = 'Código de Identificação da Unidade Consumidora';
  DSC_IDCODCLIENTE = 'Código de Identificação do Cliente';
  DSC_TPACESSO = 'Tipo de Acessante';
  DSC_XNOMEUC = 'Nome da Unidade Consumidora';
  DSC_TPCLASSE = 'Classe de Consumo da Unidade Consumidora';
  DSC_TPSUBCLASSE = 'Subclasse de Consumo da Unidade Consumidora';
  DSC_TPFASE = 'Tipo de Ligação';
  DSC_TPGRPTENSAO = 'Grupo e Subgrupo de Tensão';
  DSC_TPMODTAR = 'Modalidade Tarifária';
  DSC_LATGPS = 'Latitude da localização da captura';
  DSC_LONGGPS = 'Longitude da localização da captura';
  DSC_CHNF3E = 'Chave da NF3e Referenciada';
  DSC_COMPETEMIS = 'Ano e Mês da Emissão da NF';
  DSC_COMPETAPUR = 'Ano e Mês da Apuração';
  DSC_HASH115 = 'Hash do registro no arquivo do convênio 115';
  DSC_MOTSUB = 'Motivo da Substituição';
  DSC_NCONTRAT = 'Numero de Referência da quantidade contratada';
  DSC_TPGRCONTRAT = 'Tipo de Grandeza Contratada';
  DSC_TPPOSTAR = 'Tipo de Posto Tarifário Contratado';
  DSC_QUNIDCONTRAT = 'Quantidade Contratada em kW ou kWh';
  DSC_NMED = 'Numero de Referência da medição';
  DSC_IDMEDIDOR = 'Identificação do Medidor';
  DSC_DMEDANT = 'Data da leitura anterior';
  DSC_DMEDATU = 'Data da leitura atual';
  DSC_TPPARTCOMP = 'Tipo de Participação no Sistema de Compensação';
  DSC_VPOTINST = 'Potência Instalada em kW';
  DSC_IDACESSGER = 'Código Único de identificação da Unidade Geradora';
  DSC_ENERALOC = 'Energia Alocada';
  DSC_VSALDANT = 'Saldo Anterior';
  DSC_VCREDEXPIRADO = 'Créditos Expirados';
  DSC_VSALDATUAL = 'Saldo Atual';
  DSC_VCREDEXPIRAR = 'Créditos a Expirar';
  DSC_COMPETEXPIRAR = 'AAAA/MM que ocorrá a expiração';
  DSC_TPAJUSTE = 'Tipo de Ajuste';
  DSC_MOTAJUSTE = 'Motivo do Ajuste';
  DSC_QFATURADA = 'Quantidade Faturada';
  DSC_CCLASS = 'Código de Classificação';
  DSC_NITEMANT = 'Numero do Item Anterior';
  DSC_DINITARIF = 'Data de Inicio';
  DSC_DFIMTARIF = 'Data de Fim';
  DSC_TPATO = 'Tipo de Ato da ANEEL';
  DSC_NATO = 'Numero do Ato';
  DSC_ANOATO = 'Ano do Ato';
  DSC_TPTARIF = 'Tarifa de Aplicação';
  DSC_CPOSTARIF = 'Tipo de Posto Tarifário';
  DSC_UMED = 'Unidade de Medida';
  DSC_VTARIFHOM = 'Valor da Tarifa Homologada';
  DSC_VTARIFAPLIC = 'Valor da Trarifa Aplicada';
  DSC_MOTDIFTARIF = 'Motivo da Diferença de Tarifa';
  DSC_TPBAND = 'Tipo de Bandeira';
  DSC_VADBAND = 'Valor do Adicional';
  DSC_VADBANDAPLIC = 'Valor Adicional Aplicado';
  DSC_MOTDIFBAND = 'Motivo da Diferença do Adicional';
  DSC_XGRANDFAT = 'Nome da Grandeza Faturada';
  DSC_COMPETFAT = 'Ano e Mês do Faturamento';
  DSC_VFAT = 'Valor Faturado';
  DSC_DAPRESFAT = 'Data Apresentação da Fatura';
  DSC_DPROXLEITURA = 'Data Prevista Próxima Leitura';
  DSC_CODBARRAS = 'Código de Barras';
  DSC_CODDEBAUTO = 'Código de Autorização de Débito em Conta';
  DSC_CODBANCO = 'Numero do Banco para Débito em Conta';
  DSC_CODAGENCIA = 'Numero da Agência Bancária para Débito em Conta';
  DSC_VICMSDESON = 'Valor Total do ICMS Desonerado';
  DSC_VFCP = 'Valor Total do Fundo de Combate a Pobreza';
  DSC_VFCPST = 'Valor Total do FCP retido por Substituição Tributária';
  DSC_INDORIGEMQTD = 'Indicador da Origem da Quantidade Faturada';
  DSC_TPGRMED = 'Tipo de Grandeza Medida';
  DSC_VMEDANT = 'Valor da Medição Anterior';
  DSC_VMEDATU = 'Valor da Medição Atual';
  DSC_VCONST = 'Fator de Multiplicação do Medidor';
  DSC_VMED = 'Valor da Medição';
  DSC_PPERDATRAN = 'Percentual da Perda de Transformação';
  DSC_VMEDPERDATRAN = 'Valor Medido após perda de Transformação';
  DSC_VMEDPERDATEC = 'Valor Medido após perda Técnica';
  DSC_TPMOTNAOLEITURA = 'Tipo do Motivo da não Leitura';
  DSC_PFCP = 'Percentual do Fundo de Combate a Pobreza';
  DSC_PICMSST = 'Percentual do ICMS ST';
  DSC_VICMSST = 'Valor do ICMS ST';
  DSC_PFCPST = 'Percentual do Fundo de Combate a Pobreza ST';
  DSC_CBENEF = 'Código de Beneficio Fiscal';
  DSC_TPPROC = 'Tipo de Processo';
  DSC_CCONTAB = 'Numero da Conta Contábil';
  DSC_XCONTAB = 'Descrição da Conta Contábil';
  DSC_VCONTAB = 'Valor do Lançamento na Conta Contábil';
  DSC_TPLANC = 'Tipo de Lançamento Contábil';
  DSC_CODROTEIROLEITURA = 'Código de Roteiro de Leitura';
  DSC_CNIS = 'Numero da Identificação Social - NIS';
  DSC_NB = 'Numero do Beneficio';
  DSC_VRETPIS = 'Valor do PIS Retido';
  DSC_VRETCOFINS = 'Valor do COFINS Retido';
  DSC_VRETCSLL = 'Valor do CSLL Retido';
  DSC_VBCIRRF = 'Base de Cálculo do IRRF';
  DSC_VIRRF = 'Valor do IRRF Retido';
  DSC_VCOFINSEfet = 'Total do Valor Efetivo do COFINS';
  DSC_VPISEfet = 'Total do Valor Efetivo do PIS';
  DSC_URLQRCODEPIX = 'URL do QRCode do PIX';
  DSC_TPFONTEENERGIA = 'Tipo da fonte de energia utilizada';
  DSC_NSITEAUTORIZ = 'Numero do Site de Autorização';

  DSC_ULTNSU = 'Último NSU recebido pela Empresa';
  DSC_NSU = 'NSU específico';

  DSC_CMUNFG = 'Código do Município FG';
  DSC_VITEM = 'Valor líquido do Item';
  DSC_VPROD = 'Valor Total Bruto dos Produtos ou Serviços';
  DSC_VBC = 'Valor da BC do ICMS';
  DSC_PICMS = 'Alíquota do imposto';
  DSC_VICMS = 'Valor do ICMS';
  DSC_VBCST = 'Valor da BC do ICMS ST';
  DSC_VPIS = 'Valor do PIS';
  DSC_VCOFINS = 'Valor do COFINS';
  DSC_INFADPROD = 'Informações adicionais do Produto';
  DSC_CPROD = 'Código do produto ou serviço';
  DSC_XPROD = 'Descrição do Produto ou Serviço';
  DSC_PREDBC = 'Percentual da Redução de BC';
  DSC_PPIS = 'Alíquota do PIS (em percentual)';
  DSC_PCOFINS = 'Alíquota da COFINS (em percentual)';
  DSC_NPROCESSO = 'Número do Processo';
  DSC_VST = 'Valor TOTAL Icms substituição Tributária';
  DSC_DVENC = 'Data de vencimento';
  DSC_NFAT = 'Número da fatura';
  DSC_QTDE = 'Quantidade';
  DSC_VBCSTRET = 'Valor da BC do ICMS ST Retido';
  DSC_PICMSSTRET = 'Alíquota do ICMS Substituição Tributaria Retido';
  DSC_VICMSSTRET = 'Valor do ICMS Substituição Tributaria Retido';
  DSC_VBCFCPST = 'Valor da Base de Cálculo do FCP por Substituição Tributária';
  DSC_PFCPSTRET = 'Percentual do FCP retido anteriormente por Substituição Tributária';
  DSC_VFCPSTRET = 'Valor do FCP retido por Substituição Tributária';
  DSC_PREDBCEFET = 'Percentual de redução da base de cálculo efetiva';
  DSC_VBCEFET = 'Valor da base de cálculo efetiva';
  DSC_PICMSEFET = 'Alíquota do ICMS efetiva';
  DSC_VICMSEFET = 'Valor do ICMS efetivo';

implementation

end.

