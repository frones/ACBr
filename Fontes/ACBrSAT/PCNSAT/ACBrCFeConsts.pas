{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrCFeConsts;

interface

resourcestring
  DSC_INFADPROD = 'Informações adicionais do Produto';
  DSC_NITEM = 'Numero do item';
  DSC_CPROD = 'Código do produto ou serviço';
  DSC_CEAN = 'Código de Barra do Item';
  DSC_XPROD = 'Descrição do Produto ou Serviço';
  DSC_NCM = 'Código NCM';
  DSC_CEST = 'Código Identificador da Substitução Tributária';
  DSC_UCOM = 'Unidade Comercial';
  DSC_QCOM = 'Quantidade Comercial';
  DSC_VUNCOM = 'Valor Unitário de Comercialização';
  DSC_VPROD = 'Valor Total Bruto dos Produtos ou Serviços';
  DSC_NITEMPED = 'Item do Pedido de Compra da DI – adição';
  DSC_VDESC = 'Valor do desconto';
  DSC_VOUTRO = 'Outras Despesas Acessórias';
  DSC_ORIG = 'Origem da mercadoria';
  DSC_PICMS = 'Alíquota do imposto';
  DSC_VICMS = 'Valor do ICMS';
  DSC_CSOSN = 'Código de Situação da Operação – Simples Nacional';
  DSC_VBC = 'Valor da BC do ICMS';
  DSC_PPIS = 'Alíquota do PIS (em percentual)';
  DSC_VPIS = 'Valor do PIS';
  DSC_QBCPROD = 'BC da CIDE';
  DSC_VALIQPROD = 'Valor da alíquota (em reais)';
  DSC_PISOUTR = 'Grupo PIS outras operações';
  DSC_PCOFINS = 'Alíquota da COFINS (em percentual)';
  DSC_VCOFINS = 'Valor do COFINS';
  DSC_VBCISS = 'Valor da Base de Cálculo do ISSQN';
  DSC_VALIQ = 'Alíquota';
  DSC_VISSQN = 'Valor do Imposto sobre Serviço de Qualquer Natureza';
  DSC_CMUNFG = 'Código do Município FG';
  DSC_CLISTSERV = 'Lista Prestação de Serviços';
  DSC_VISS = 'Valor do Imposto sobre Serviço';
  DSC_CAUT = 'Número da Autorização';

  DSC_VDESCSUBTOT = 'Valor de Desconto sobre Subtotal';
  DSC_VACRESSUBTOT = 'Valor de Acréscimo sobre Subtotal';
  DSC_VPISST = 'Valor do PIS ST';
  DSC_VCOFINSST = 'Valor do COFINS ST';
  DSC_VCFE = 'Valor Total do CF-e';
  DSC_VCFELEI12741 = 'Valor aproximado dos tributos do CFe-SAT – Lei 12741/12.';
  DSC_VDEDUCISS = 'Valor das deduções para ISSQN';
  DSC_CSERVTRIBMUN = 'Codigo de tributação pelo ISSQN do municipio';
  DSC_CNATOP = 'Natureza da Operação de ISSQN';
  DSC_INDINCFISC = 'Indicador de Incentivo Fiscal do ISSQN';
  DSC_COFINSST = 'Grupo de COFINS Substituição Tributária';
  DSC_REGTRIB = 'Código de Regime Tributário';
  DSC_REGISSQN = 'Regime Especial de Tributação do ISSQN';
  DSC_RATISSQN = 'Indicador de rateio do Desconto sobre subtotal entre itens sujeitos à tributação pelo ISSQN.';
  DSC_NCFE = 'Número do Cupom Fiscal Eletronico';
  DSC_HEMI = 'Hora de emissão';
  DSC_SIGNAC = 'Assinatura do Aplicativo Comercial';
  DSC_MP = 'Grupo de informações sobre Pagamento do CFe';
  DSC_CMP = 'Código do Meio de Pagamento';
  DSC_VMP = 'Valor do Meio de Pagamento';
  DSC_CADMC = 'Credenciadora de cartão de débito ou crédito';
  DSC_VTROCO = 'Valor do troco';
  DSC_VITEM = 'Valor líquido do Item';
  DSC_VRATDESC = 'Rateio do desconto sobre subtotal';
  DSC_VRATACR = 'Rateio do acréscimo sobre subtotal';
  DSC_NUMEROCAIXA = 'Número do Caixa ao qual o SAT está conectado';
  DSC_VITEM12741 = 'Valor aproximado dos tributos do Produto ou serviço – Lei 12741/12';
  DSC_NSERIESAT = 'Número de série do equipamento SAT';
  DSC_DHINICIAL = 'Data e hora incial';
  DSC_DHFINAL = 'Data e Hora Final';

implementation

end.

