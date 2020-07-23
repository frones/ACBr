{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{*******************************************************************************
|* Historico
|*
|* 17/08/2016: Italo Jurisato Junior
|*  - Criado uma Unit especifica para as constantes usadas pelos componentes
|*    que geram DF-e
*******************************************************************************}

{$I ACBr.inc}

unit pcnConsts;

interface

uses
  SysUtils;

const
  CODIGO_BRASIL = 1058;

  ERR_MSG_MAIOR = 'Tamanho maior que o máximo permitido';
  ERR_MSG_MENOR = 'Tamanho menor que o mínimo permitido';
  ERR_MSG_VAZIO = 'Nenhum valor informado';
  ERR_MSG_INVALIDO = 'Conteúdo inválido';
  ERR_MSG_MAXIMO_DECIMAIS = 'Numero máximo de casas decimais permitidas';
  ERR_MSG_MAIOR_MAXIMO = 'Número de ocorrências maior que o máximo permitido - Máximo ';
  ERR_MSG_GERAR_CHAVE = 'Erro ao gerar a chave da DFe!';
  ERR_MSG_FINAL_MENOR_INICIAL = 'O numero final não pode ser menor que o inicial';
  ERR_MSG_ARQUIVO_NAO_ENCONTRADO = 'Arquivo não encontrado';
  ERR_MSG_SOMENTE_UM = 'Somente um campo deve ser preenchido';
  ERR_MSG_MENOR_MINIMO = 'Número de ocorrências menor que o mínimo permitido - Mínimo ';

  XML_V01           = '?xml version="1.0"?';
  ENCODING_UTF8     = '?xml version="1.0" encoding="UTF-8"?';
  ENCODING_UTF8_STD = '?xml version="1.0" encoding="UTF-8" standalone="no"?';

  NAME_SPACE = 'xmlns="http://www.portalfiscal.inf.br/nfe"';

  DSC_CHAVE = 'Chave do DFe';
  DSC_ULTNSU = 'Último NSU recebido pela Empresa';
  DSC_NSU = 'NSU específico';
  DSC_NNF = 'Número do Documento Fiscal';
  DSC_NATOP = 'Descrição da Natureza da Operação';
  DSC_INDPAG = 'Indicador da forma de pagamento';
  DSC_TPIMP = 'Formato de Impressão do Documento Auxiliar';
  DSC_TPEMIS = 'Forma de Emissão do DF-e';
  DSC_PROCEMI = 'Processo de emissão do DF-e';
  DSC_VERPROC = 'Versão do Processo de emissão do DF-e';
  DSC_DHCONT = 'Data e Hora de entrada em contingencia';
  DSC_XJUSTCONT = 'Justificativa de entrada em contingencia';
  DSC_IEST = 'Inscrição Estadual do Substituto tributário';
  DSC_XFANT = 'Nome de Fantasia';
  DSC_FONE = 'Telefone';
  DSC_EMAIL = 'Endereço de Email';
  DSC_CMUN = 'Código do Município';
  DSC_CPAIS = 'Código do País';
  DSC_XPAIS = 'Nome do País';
  DSC_OBSCONT = 'Observações de interesse do contribuite';
  DSC_ISUF = 'Inscrição na SUFRAMA';
  DSC_INFADFISCO = 'Informações adicionais de interesse do Fisco';
  DSC_PREDBC = 'Percentual da Redução de BC';
  DSC_VIR = 'Valor do IR';
  DSC_VINSS = 'Valor do INSS';
  DSC_VCSLL = 'Valor do CSLL';
  DSC_VNF = 'Valor Total do DF-e';
  DSC_VBCICMS = 'BC do ICMS';
  DSC_VBCST = 'Valor da BC do ICMS ST';
  DSC_VST = 'Valor TOTAL Icms substituição Tributária';
  DSC_NLACRE = 'Número dos Lacres';
  DSC_REFNFE = 'Chave de acesso das NF-e referenciadas';
  DSC_TPNF = 'Tipo do Documento Fiscal';
  DSC_RENAVAM = 'RENAVAM';
  DSC_PLACA = 'Placa do Veículo';
  DSC_TPVEIC = 'Tipo de Veículo';
  DSC_VAFRMM = 'Valor da AFRMM';
  DSC_VFRETE = 'Valor Total do Frete';
  DSC_VAGAO = 'Identificação do vagão';
  DSC_CHASSI = 'Número do chassi';
  DSC_CCOR = 'Cor do Veículo';
  DSC_XCOR = 'Descrição da Cor';
  DSC_CMOD = 'Modelo do Veículo';
  DSC_NFAT = 'Número da fatura';
  DSC_VORIG = 'Valor Original da Fatura';
  DSC_VLIQ = 'Valor Líquido da Fatura';
  DSC_NDUP = 'Número da duplicata';
  DSC_DVENC = 'Data de vencimento';
  DSC_VDUP = 'Valor da duplicata';
  DSC_XSERV = 'Descrição do serviço';
  DSC_ANO = 'Ano';
  DSC_NNFINI = 'Numero inicial';
  DSC_NNFFIN = 'Numero final';
  DSC_XJUST = 'Justificativa';
  DSC_NREC = 'Numero do recibo';
  DSC_NSERIE = 'Número de série';
  DSC_NPROT = 'Numero do protocolo';
  DSC_CNAE = 'Classificação Nacional de Atividades Econômicas';
  DSC_QTDE = 'Quantidade';
  DSC_QTDEITENS = 'Quantidade de Itens na lista';
  DSC_INDISS = 'Indicador da Exigibilidade do ISS';
  DSC_VDESCINCOND = 'Valor Desconto Incondicionado';
  DSC_VDESCCOND = 'Valor Desconto Condicionado';
  DSC_INDISSRET = 'Indicador de ISS Retido';
  DSC_VISSRET = 'Valor Retenção ISS';
  DSC_NPROCESSO = 'Número do Processo';
  DSC_TPAG = 'Forma de Pagamento';
  DSC_INDINCENTIVO = 'Indicador de Incentivo Fiscal';

  DSC_CNPJ = 'CNPJ(MF)';
  DSC_CPF = 'CPF';
  DSC_CUF = 'Código do UF (Unidade da Federação)';
  DSC_CNF = 'Número da Nota Fiscal Eletrônica';
  DSC_MOD = 'Modelo';
  DSC_SERIE = 'Série do Documento Fiscal';
  DSC_DEMI = 'Data de emissão';
  DSC_CDV = 'Digito Verificador';
  DSC_TPAMB = 'Identificação do Ambiente';
  DSC_XNOME = 'Razão Social ou Nome';
  DSC_IE = 'Inscrição Estadual';
  DSC_IM = 'Inscrição Municipal';
  DSC_XLGR = 'Logradouro';
  DSC_NRO = 'Número';
  DSC_XCPL = 'Complemento (Endereço)';
  DSC_XBAIRRO = 'Bairro';
  DSC_XMUN = 'Nome do Município';
  DSC_CEP = 'CEP';
  DSC_UF = 'Sigla da UF';
  DSC_INFADPROD = 'Informações adicionais do Produto';
  DSC_NITEM = 'Numero do item';
  DSC_CPROD = 'Código do produto ou serviço';
  DSC_CEAN = 'Código de Barra do Item';
  DSC_XPROD = 'Descrição do Produto ou Serviço';
  DSC_NCM = 'Código NCM';
  DSC_CEST = 'Código Identificador da Substitução Tributária';
  DSC_CFOP = 'CFOP';
  DSC_UCOM = 'Unidade Comercial';
  DSC_QCOM = 'Quantidade Comercial';
  DSC_VUNCOM = 'Valor Unitário de Comercialização';
  DSC_VPROD = 'Valor Total Bruto dos Produtos ou Serviços';
  DSC_NITEMPED = 'Item do Pedido de Compra da DI – adição';
  DSC_VDESC = 'Valor do desconto';
  DSC_VOUTRO = 'Outras Despesas Acessórias';
  DSC_XTEXTO = 'Conteúdo do Campo';
  DSC_ORIG = 'Origem da mercadoria';
  DSC_CST = 'Código da situação tributária ';
  DSC_PICMS = 'Alíquota do imposto';
  DSC_VICMS = 'Valor do ICMS';
  DSC_CSOSN = 'Código de Situação da Operação – Simples Nacional';
  DSC_VBC = 'Valor da BC do ICMS';
  DSC_PPIS = 'Alíquota do PIS (em percentual)';
  DSC_VPIS = 'Valor do PIS';
  DSC_QBCPROD = 'BC da CIDE';
  DSC_VALIQPROD = 'Valor da alíquota (em reais)';
  DSC_PCOFINS = 'Alíquota da COFINS (em percentual)';
  DSC_VCOFINS = 'Valor do COFINS';
  DSC_PISOUTR = 'Grupo PIS outras operações';
  DSC_VBCISS = 'Valor da Base de Cálculo do ISSQN';
  DSC_VALIQ = 'Alíquota';
  DSC_VISSQN = 'Valor do Imposto sobre Serviço de Qualquer Natureza';
  DSC_CMUNFG = 'Código do Município FG';
  DSC_CLISTSERV = 'Lista Prestação de Serviços';
  DSC_VISS = 'Valor do Imposto sobre Serviço';
  DSC_INFCPL = 'Informações complementares de interesse do contribuinte';
  DSC_OBSFISCO = 'Observações de interesse do fisco';
  DSC_XCAMPO = 'Identificação do Campo';

  DSC_MODAL  = 'Tipo de Modal';
  DSC_RNTRC  = 'Registro Nacional de Transportadores Rodoviários de Carga';
  DSC_TPPROP = 'Tipo de Proprietário';
  DSC_TPROD  = 'Tipo de Rodado';
  DSC_TPCAR  = 'Tipo de Carroceria';
  DSC_REFCTE = 'Chave de acesso do CT-e referenciado';
  DSC_QTDRAT = 'Quantidade Rateada';
  DSC_VDOC   = 'Valor do documento';
  DSC_CUNID  = 'Código da unidade de medida';
  DSC_LACR   = 'Grupo de lacres';
  DSC_CHCTE  = 'Chave do CTe';
  DSC_LOTA   = 'Indicador de lotação';
  DSC_VVALEPED  = 'Valor do Vale-Pedagio';

  DSC_RESPSEG  = 'Responsável pelo Seguro';
  DSC_XSEG     = 'Nome da Seguradora';
  DSC_NAPOL    = 'Número da Apólice';
  DSC_NAVER    = 'Número da Averbação';
  DSC_INFSEG   = 'Informações de seguro da carga';
  DSC_UFPER    = 'Sigla da UF do percurso do veículo';

  DSC_NONU      = 'Número ONU/UN';
  DSC_XNOMEAE   = 'Nome apropriado para embarque do produto';
  DSC_XCLARISCO = 'Classe e Risco secundário';
  DSC_GREMB     = 'Grupo de Embalagem';
  DSC_QTOTPROD  = 'Quantidade total por produto';
  DSC_QVOLTIPO  = 'Quantidade e tipo de volumes';

  DSC_TPUNIDTRANSP = 'Tipo de Unidade de Transporte';
  DSC_IDUNIDTRANSP = 'Identificação da Unidade de Transporte';
  DSC_TPUNIDCARGA  = 'Tipo de Unidade de Carga';
  DSC_IDUNIDCARGA  = 'Identificação da Unidade de Carga';

  DSC_TPNAV     = 'Tipo de Navegação';
  DSC_IRIN      = 'Irin do navio sempre deverá ser informado';

  DSC_AUTXML   = 'Autorizados para download do XML do DF-e';
  DSC_XCONTATO = 'Nome do Contato';

  DSC_DigestValue = 'Digest Value';
  DSC_SignatureValue = 'Signature Value';
  DSC_X509Certificate = 'X509 Certificate';

  DSC_VPAG = 'Valor do Pagamento';
  DSC_TPINTEGRA = 'Tipo de Integração para pagamento';
  DSC_TBAND = 'Bandeira da Operadora de Cartão';
  DSC_CAUT = 'Número da Autorização';

  DSC_IDCSRT = 'Identificador CSRT - Código de Segurança do Responsável Técnico';
  DSC_HASHCSRT = 'Hash do CSRT - Código de Segurança do Responsável Técnico';

  //CFe - Cupom Fiscal Eletrônico - SAT
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
  DSC_QRCODE = 'Assinatura Digital para uso em QRCODE';
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
  DSC_CHAVESEGURANCA = 'Chave de segurança';

implementation

end.

