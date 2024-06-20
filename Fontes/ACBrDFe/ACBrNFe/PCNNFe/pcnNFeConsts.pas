{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{ Colaboradores neste arquivo: Italo Jurisato Junior                           }
{																			   }
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
|*  - Criado uma Unit especifica para as constantes usadas pelo componente
|*    ACBrNFe
*******************************************************************************}

{$I ACBr.inc}

unit pcnNFeConsts;

interface

uses
  SysUtils;

const
  NAME_SPACE = 'xmlns="http://www.portalfiscal.inf.br/nfe"';

  DSC_AAMM = 'Ano e Mês';
  DSC_ANOFAB = 'Ano de Fabricação';
  DSC_ANOMOD = 'Ano do Modelo de Fabricação';
  DSC_CEANTRIB = 'Código de Barra do Item Tributação';
  DSC_CENQ = 'Código de Enquadramento Legal do IPI';
  DSC_CEXPORTADOR = 'Código do exportador';
  DSC_CFABRICANTE = 'Fabricante';
  DSC_CLENQ = 'Classe de enquadramento do IPI para Cigarros e Bebidas';
  DSC_CSITTRIB = 'Código de Tributação do ISSQN';
  DSC_CILIN = 'Cilindradas';
  DSC_CMT = 'Capacidade Máxima de Tração';
  DSC_CCORDEN = 'Código da Cor DENATRAN';
  DSC_TPREST = 'Restrição';
  DSC_CRT = 'Código de Regime Tributário';
  DSC_CODIF = 'Código de autorização / registro do CODIF';
  DSC_CONDVEIC = 'Condições do Veículo';
  DSC_CNPJPROD = 'CNPJ do produtor da mercadoria, quando diferente do emitente';
  DSC_CPRODANP = 'Código do produto ANP';
  DSC_CSELO = 'Código do selo';
  DSC_DDESEMB = 'Data do Desembaraço Aduaneiro';
  DSC_DDI = 'Data de registro da DI/DSI/DA';
  DSC_DESCR = 'Descrição completa';
  DSC_DFAB = 'Data de fabricação';
  DSC_DIST = 'Distância entre os eixos';
  DSC_DPAG = 'Data de pagamento do Documento de Arrecadação';
  DSC_DSAIENT = 'Data de saída ou entrada da mercadoria/produto';
  DSC_HSAIENT = 'Hora de saída ou entrada da mercadoria/produto';
  DSC_DVAL = 'Data de validade';
  DSC_ESP = 'Espécie dos volumes transportados';
  DSC_ESPVEIC = 'Espécie de veículo';
  DSC_EXTIPI = 'EX_TIPI';
  DSC_FINNFE = 'Finalidade de emissão da NFe';
  DSC_GENERO = 'Gênero do produto ou serviço';
  DSC_INDPROC = 'Indicador da origem do processo';
  DSC_IPITrib = 'IPI Tributável';
  DSC_MARCA = 'Marca dos volumes transportados';
  DSC_MATR = 'Matrícula do agente';
  DSC_NECF = 'Número de ordem seqüencial do ECF';
  DSC_NCOO = 'Número do Contador de Ordem de Operação - COO';
  DSC_MODBC = 'Modalidade de determinação da BC do ICMS';
  DSC_MODBCST = 'Modalidade de determinação da BC do ICMS ST';
  DSC_MOTDESICMS = 'Motivo da desoneração do ICMS';
  DSC_MODFRETE = 'Modalidade do Frete';
  DSC_NADICAO = 'Numero da Adição';
  DSC_NCANO = 'Numero de série do cano';
  DSC_NDI = 'Numero do Documento de Importação DI/DSI/DA';
  DSC_nDAR = 'Número do Documento Arrecadação de Receita';
  DSC_NLOTE = 'Número do Lote do medicamento';
  DSC_CPRODANVISA = 'Código de Produto da ANVISA';
  DSC_XMOTIVOISENCAO = 'Motivo da Isenção de registro da ANVISA';
  DSC_NMOTOR = 'Número de Motor';
  DSC_NPROC = 'Identificador do processo ou ato concessório';
  DSC_NSEQADIC = 'Numero seqüencial do item dentro da adição';
  DSC_NVOL = 'Numeração dos volumes transportados';
  DSC_PESOB = 'Peso Bruto (em kg)';
  DSC_PESOL = 'Peso Líquido (em kg)';
  DSC_PICMSRET = 'Alíquota da Retenção';
  DSC_PICMSST = 'Alíquota do imposto do ICMS ST';
  DSC_PIPI = 'Alíquota do IPI';
  DSC_PMVAST = 'Percentual da margem de valor Adicionado do ICMS ST';
  DSC_POT = 'Potência Motor';
  DSC_PCREDSN = 'Alíquota aplicável de cálculo do crédito (Simples Nacional).';
  DSC_VCREDICMSSN = 'Valor crédito do ICMS que pode ser aproveitado nos termos do art. 23 da LC 123 (Simples Nacional)';
  DSC_PREDBCST = 'Percentual da Redução de BC do ICMS ST';
  DSC_UFST = 'UF para qual é devido o ICMS ST';
  DSC_PBCOP = 'Percentual da BC operação própria';
  DSC_QLOTE = 'Quantidade de produto no Lote do medicamento';
  DSC_QSELO = 'Quantidade de selo de controle';
  DSC_QTEMP = 'Quantidade de combustível faturada à temperatura ambiente.';
  DSC_QTRIB = 'Quantidade Tributável';
  DSC_QUNID = 'Quantidade total na unidade padrão para tributação (somente para os produtos tributados por unidade)';
  DSC_QVOL = 'Quantidade de volumes transportados';
  DSC_REBOQUE = 'Reboque';
  DSC_REPEMI = 'Repartição Fiscal emitente';
  DSC_RNTC = 'Registro Nacional de Transportador de Carga (ANTT)';
  DSC_BALSA = 'Identificação da balsa';
  DSC_TPARMA = 'Indicador do tipo de arma de fogo';
  DSC_TPCOMB = 'Tipo de combustível';
  DSC_TPOP = 'Tipo da operação';
  DSC_TPPINT = 'Tipo de Pintura';
  DSC_UFCONS = 'Sigla da UF de consumo';
  DSC_UFDESEMB = 'Sigla da UF onde ocorreu o Desembaraço Aduaneiro';
  DSC_UFEMBARQ = 'Sigla da UF onde ocorrerá o embarque dos produtos';
  DSC_UTRIB = 'Unidade Tributável';
  DSC_VBCICMSST = 'BC do ICMS ST retido';
  DSC_VBCICMSSTCONS = 'Valor do ICMS ST da UF de consumo';
  DSC_VBCICMSSTDEST = 'BC do ICMS ST da UF de destino';
  DSC_VBCIRRF = 'Base de Cálculo do IRRF';
  DSC_VBCRET = 'BC da Retenção do ICMS';
  DSC_VBCRETPREV = 'Base de Cálculo da Retenção da Previdência Social';
  DSC_VBCSTRET = 'Valor da BC do ICMS ST Retido';
  DSC_VCIDE = 'Valor da CIDE';
  DSC_VDAR = 'Valor total constante no Documento de arrecadação de Receita';
  DSC_INDTOT = 'Indicador de soma no total da NFe';
  DSC_VDESCDI = 'Valor do desconto do item da DI – adição';
  DSC_VDESPADU = 'Valor das despesas aduaneiras';
  DSC_VICMSRET = 'Valor do ICMS Retido';
  DSC_VICMSST = 'Valor do ICMS Substituição Tributaria';
  DSC_VICMSSTRET = 'Valor do ICMS Substituição Tributaria Retido';
  DSC_VICMSSTCONS = 'Valor do ICMS Substituição Tributaria da UF de Consumo';
  DSC_VICMSSTDEST = 'Valor do ICMS Substituição Tributaria da UF de Destino';
  DSC_VII = 'Valor do Imposto de Importação';
  DSC_VIN = 'Condição do VIN';
  DSC_VIOF = 'Valor do Imposto sobre operações Financeiras';
  DSC_vIPI = 'Valor do Imposto sobre Produtos Industrializados';
  DSC_VIRRF = 'Valor do Imposto de Renda Retido na Fonte';
  DSC_VPMC = 'Preço Máximo ao Consumidor';
  DSC_VRETCOFINS = 'Valor Retido do COFINS';
  DSC_VRETCSLL = 'Valor Retido da CONTRIBUIÇÃO SOCIAL SOBRE O LUCRO LÍQUIDO';
  DSC_VRETPIS = 'Valor Retido do PIS';
  DSC_VRETPREV = 'Valor da Retenção da Previdência Social';
  DSC_VSEG = 'Valor Total do Seguro';
  DSC_VSERV = 'Valor total dos Serviços sob não incidência ou não Tributados pelo ICMS  / Valor do Serviço';
  DSC_VUNID = 'Valor por Unidade Tributável';
  DSC_VUNTRIB = 'Valor unitário de Tributação';
  DSC_XAGENTE = 'Nome do agente';
  DSC_XCONT = 'Contrato';
  DSC_XENDER = 'Endereço Completo';
  DSC_XLOCDESEMB = 'Local de Desembaraço';
  DSC_XLOCEMBARQ = 'Local onde ocorrerá o embarque dos produtos';
  DSC_XNEMP = 'Nota de Empenho';
  DSC_XPED = 'Pedido';
  DSC_XORGAO = 'Orgão emitente';
  DSC_CHNFE = 'Chave da NFe';
  DSC_IDLOTE = 'Numero do Lote';
  DSC_VERAPLIC = 'Versão do aplicativo';
  DSC_NREGDPEC = 'Número de registro do DPEC';
  DSC_DPEC_ID = 'Grupo de Identificação da TAG a ser assinada. DPEC + CNPJ do emissor.';
  DSC_SAFRA = 'Identificação da safra';
  DSC_REF = 'Mês e ano de referência';
  DSC_FORDIA = 'Grupo de Fornecimento diário de cana';
  DSC_DIA = 'Dia';
  DSC_QTOTMES = 'Quantidade Total do Mês';
  DSC_QTOTANT = 'Quantidade Total Anterior';
  DSC_TOTGER = 'Quantidade Total Geral';
  DSC_DEDUC = 'Grupo de Deduções – Taxas e Contribuições';
  DSC_XDED = 'Descrição da Dedução';
  DSC_VDED = 'Valor da Dedução';
  DSC_VFOR = 'Valor dos Fornecimentos';
  DSC_VTOTDED = 'Valor Total da Dedução';
  DSC_VLIQFOR = 'Valor Líquido dos Fornecimentos';
  DSC_INDNFE = 'Indicador de NF-e consultada';
  DSC_INDEMI = 'Indicador do Emissor da NF-e';
  DSC_QNF = 'Quantidade de Documento Fiscal';
  DSC_VTOTTRIB = 'Valor Aproximado Total de Tributos';
  DSC_IDDEST = 'Destino da Operação';
  DSC_INDFINAL = 'Indicador de Operação com Consumidor Final';
  DSC_INDPRES = 'Indicador de Presença do Consumidor Final';
  DSC_INDINTERMED = 'Indicador de Intermediador/marketplace';
  DSC_IDESTR = 'Documento de Identificação do Estrangeiro';
  DSC_INDIEDEST = 'Indicador da IE do Destinatário';
  DSC_NVE = 'Nomenclatura de Valor Aduaneiro e Estatística';
  DSC_NFCI = 'Número de Controle da FCI';
  DSC_NRECOPI = 'Número do RECOPI';
  DSC_TPVIATRANSP = 'Via de Transporte Internacional';
  DSC_TPINTERMEDIO = 'Forma de Importação';
  DSC_NDRAW = 'Número do Drawback';
  DSC_NRE = 'Número do Registro de Exportação';
  DSC_QEXPORT = 'Qtde Exportada';
  DSC_PMIXGN = 'Percentual de Gás Natural';
  DSC_VICMSDESON = 'Valor do ICMS Desoneração';
  DSC_PDEVOL = 'Percentual da Mercadoria Devolvida';
  DSC_VIPIDEVOL = 'Valor do IPI Devolvido';
  DSC_DCOMPET = 'Data da Prestação do Serviço';
  DSC_VDEDUCAO = 'Valor da Dedução';
  DSC_VOUTRODED = 'Valor Outras Deduções';
  DSC_CSERVICO = 'Código do Serviço';
  DSC_CREGTRIB = 'Código do Regime Especial de Tributação';
  DSC_XLOCDESP = 'Local de Despacho';
  DSC_INFQRCODE = 'Texto com o QR-Code impresso no DANFE NFC-e.';
  DSC_NBICO = 'Número de identificação do bico';
  DSC_NBOMBA = 'Número de identificação da bomba';
  DSC_NTANQUE = 'Número de identificação do tanque';
  DSC_VENCINI = 'Valor do Encerrante no início do abastecimento';
  DSC_VENCFIN = 'Valor do Encerrante no final do abastecimento';
  DSC_VBCUFDEST = 'Valor da BC do ICMS na UF do destinatário';
  DSC_PFCPUFDEST = 'Alíquota do ICMS realtivo ao Fundo de Combate à Pobreza';
  DSC_PICMSUFDEST = 'Alíquota interna da UF do destinatário';
  DSC_PICMSINTER = 'Alíquota interestadual das UF envolvidas';
  DSC_PICMSINTERPART = 'Percentual provisório de partilha entre os Estados';
  DSC_VFCPUFDEST = 'Valor do ICMS realtivo ao Fundo de Combate à Pobreza';
  DSC_VICMSUFDEST = 'Valor do ICMS de partilha para a UF do destinatário';
  DSC_VICMSUFREMET = 'Valor do ICMS de partilha para a UF do remetente';
  DSC_DESCANP = 'Descrição do produto conforme ANP';
  DSC_PGLP = 'Percentual do GLP derivado do petróleo no produto GLP';
  DSC_PGNN = 'Percentual de Gás Natural Nacional – GLGNn para o produto GLP';
  DSC_PGNI = 'Percentual de Gás Natural Importado – GLGNi para o produto GLP';
  DSC_VPART = 'Valor de partida';
  DSC_VBCFCP = 'Valor da Base de Cálculo do FCP';
  DSC_PFCP = 'Percentual do ICMS relativo ao Fundo de Combate à Pobreza (FCP)';
  DSC_VFCP = 'Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP)';
  DSC_VBCFCPST = 'Valor da Base de Cálculo do FCP por Substituição Tributária';
  DSC_PFCPST = 'Percentual do FCP retido por Substituição Tributária';
  DSC_VFCPST = 'Valor do FCP retido por Substituição Tributária';
  DSC_PFCPSTRET = 'Percentual do FCP retido anteriormente por Substituição Tributária';
  DSC_VFCPSTRET = 'Valor do FCP retido por Substituição Tributária';
  DSC_PST = 'Alíquota suportada pelo Consumidor Final';
  DSC_VBCFCPUFDEST = 'Valor da BC FCP na UF de destino';
  DSC_PREDBCEFET = 'Percentual de redução da base de cálculo efetiva';
  DSC_VBCEFET = 'Valor da base de cálculo efetiva';
  DSC_PICMSEFET = 'Alíquota do ICMS efetiva';
  DSC_VICMSEFET = 'Valor do ICMS efetivo';
  DSC_VICMSSUBSTITUTO = 'Valor do ICMS Substituto';
  //DSC_VPART = 'Valor';
  DSC_INDESCALA = 'Indicador de Escala de Produção';
  DSC_CNPJFAB = 'CNPJ do Fabricante da Mercadoria';
  DSC_CBENEF = 'Código de Benefício Fiscal na UF aplicado ao item';
  DSC_CAGREG = 'Código de Agregação';
  DSC_URLCHAVE = 'URL de consulta por chave de acesso a ser impressa no DANFE NFC-e';
  DSC_CNPJINTERM = 'CNPJ do Intermediador da Transação';
  DSC_IDCADINTERM = 'Nome do usuário ou identificação do perfil do vendedor no site do intermediador';
  DSC_CBARRA = 'Código de Barras próprio ou de terceiros';
  DSC_CBARRATRIB = 'Código de Barras próprio ou de terceiros (Tributação)';
  DSC_VICMSSTDESON = 'Valor do ICMS- ST desonerado';
  DSC_MOTDESICMSST = 'Motivo da desoneração do ICMS- ST';
  DSC_PFCPDIF = 'Percentual do diferimento do ICMS relativo ao Fundo de Combate à Pobreza (FCP)';
  DSC_VFCPDIF = 'Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) diferido';
  DSC_VFCPEFET = 'Valor efetivo do ICMS relativo ao Fundo de Combate à Pobreza (FCP)';
  DSC_INDSOMAPISST = 'Indica se o valor do PIS ST compõe o valor total da NF-e';
  DSC_INDSOMACOFINSST = 'Indica se o valor da COFINS ST compõe o valor total da NF-e';
  DSC_TPATO = 'Tipo do Ato Concessório';
  DSC_PBIO = 'Percentual do índice de mistura do Biodiesel';
  DSC_INDIMPORT = 'indicador de importação';
  DSC_PORIG = 'Percentual originário para a UF';
  DSC_ADREMICMS = 'Alíquota ad rem do imposto';
  DSC_VICMSMONO = 'Valor do ICMS próprio';
  DSC_ADREMICMSRETEN = 'Alíquota ad rem do imposto com retenção ';
  DSC_VICMSMONORETEN = 'Valor do ICMS com retenção';
  DSC_ADREMICMSDIF = 'Alíquota ad rem do imposto diferido';
  DSC_VICMSMONODIF = 'Valor do ICMS diferido';
  DSC_ADREMICMSRET = 'Alíquota ad rem do imposto retido anteriormente';
  DSC_VICMSMONORET = 'Valor do ICMS retido anteriormente';
  DSC_QBCMONO = 'Quantidade tributada';
  DSC_QBCMONORETEN = 'Quantidade tributada sujeita a retenção';
  DSC_PREDADREM = 'Percentual de redução do valor da alíquota adrem do ICMS';
  DSC_MOTREDADREM = 'Motivo da redução do adrem';
  DSC_VICMSMONOOP = 'Valor do ICMS da operação';
  DSC_QBCMONORET = 'Quantidade tributada retida anteriormente';
  DSC_PDIF = 'Percentual do diferimento';

  DSC_VICMS = 'Valor do ICMS';
  DSC_VST = 'Valor TOTAL Icms substituição Tributária';
  DSC_NATOP = 'Descrição da Natureza da Operação';
  DSC_INDPAG = 'Indicador da forma de pagamento';
  DSC_TPNF = 'Tipo do Documento Fiscal';
  DSC_CMUNFG = 'Código do Município FG';
  DSC_REFNFE = 'Chave de acesso das NF-e referenciadas';
  DSC_REFCTE = 'Chave de acesso do CT-e referenciado';
  DSC_IEST = 'Inscrição Estadual do Substituto tributário';
  DSC_CNAE = 'Classificação Nacional de Atividades Econômicas';
  DSC_ISUF = 'Inscrição na SUFRAMA';
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
  DSC_VFRETE = 'Valor Total do Frete';
  DSC_VDESC = 'Valor do desconto';
  DSC_VOUTRO = 'Outras Despesas Acessórias';
  DSC_NITEMPED = 'Item do Pedido de Compra da DI – adição';
  DSC_VAFRMM = 'Valor da AFRMM';
  DSC_CHASSI = 'Número do chassi';
  DSC_CCOR = 'Cor do Veículo';
  DSC_XCOR = 'Descrição da Cor';
  DSC_NSERIE = 'Número de série';
  DSC_TPVEIC = 'Tipo de Veículo';
  DSC_CMOD = 'Modelo do Veículo';
  DSC_LOTA   = 'Indicador de lotação';
  DSC_QBCPROD = 'BC da CIDE';
  DSC_VALIQPROD = 'Valor da alíquota (em reais)';
  DSC_VBCICMS = 'BC do ICMS';
  DSC_ORIG = 'Origem da mercadoria';
  DSC_VBC = 'Valor da BC do ICMS';
  DSC_PICMS = 'Alíquota do imposto';
  DSC_PREDBC = 'Percentual da Redução de BC';
  DSC_VBCST = 'Valor da BC do ICMS ST';
  DSC_CSOSN = 'Código de Situação da Operação – Simples Nacional';
  DSC_PPIS = 'Alíquota do PIS (em percentual)';
  DSC_VPIS = 'Valor do PIS';
  DSC_PISOUTR = 'Grupo PIS outras operações';
  DSC_PCOFINS = 'Alíquota da COFINS (em percentual)';
  DSC_VCOFINS = 'Valor do COFINS';
  DSC_COFINSST = 'Grupo de COFINS Substituição Tributária';
  DSC_VBCISS = 'Valor da Base de Cálculo do ISSQN';
  DSC_VALIQ = 'Alíquota';
  DSC_VISSQN = 'Valor do Imposto sobre Serviço de Qualquer Natureza';
  DSC_CLISTSERV = 'Lista Prestação de Serviços';
  DSC_VDESCINCOND = 'Valor Desconto Incondicionado';
  DSC_VDESCCOND = 'Valor Desconto Condicionado';
  DSC_VISSRET = 'Valor Retenção ISS';
  DSC_INDISS = 'Indicador da Exigibilidade do ISS';
  DSC_NPROCESSO = 'Número do Processo';
  DSC_INDINCENTIVO = 'Indicador de Incentivo Fiscal';
  DSC_VISS = 'Valor do Imposto sobre Serviço';
  DSC_NFAT = 'Número da fatura';
  DSC_VORIG = 'Valor Original da Fatura';
  DSC_VLIQ = 'Valor Líquido da Fatura';
  DSC_NDUP = 'Número da duplicata';
  DSC_DVENC = 'Data de vencimento';
  DSC_VDUP = 'Valor da duplicata';
  DSC_VAGAO = 'Identificação do vagão';
  DSC_PLACA = 'Placa do Veículo';
  DSC_NLACRE = 'Número dos Lacres';
  DSC_QTDE = 'Quantidade';
  DSC_TPAG = 'Forma de Pagamento';
  DSC_XPAG = 'Descrição da forma de Pagamento';
  DSC_VPAG = 'Valor do Pagamento';
  DSC_TPINTEGRA = 'Tipo de Integração para pagamento';
  DSC_TBAND = 'Bandeira da Operadora de Cartão';
  DSC_CAUT = 'Número da Autorização';
  DSC_CNPJPAG = 'CNPJ Transacional do Pagamento';
  DSC_UFPAG = 'UF do CNPJ do estabelecimento onde o pagamento foi processado/transacionado/recebido';
  DSC_CNPJRECEB = 'CNPJ do beneficiário do pagamento';
  DSC_IDTERMPAG = 'Identificador do terminal de pagamento';
  DSC_CCREDPRESUMIDO = 'Código de Benefício Fiscal de Crédito Presumido na UF aplicado ao item';
  DSC_PCREDPRESUMIDO = 'Percentual do Crédito Presumido';
  DSC_VCREDPRESUMIDO = 'Valor do Crédito Presumido';
  DSC_CBENEFRBC = 'Código de Benefício Fiscal na UF aplicado ao item quando houver RBC.';
  DSC_CNPJIF = 'CNPJ da instituição financeira, de pagamento, adquirente ou subadquirente.';
  DSC_UFRECEB = 'UF do CNPJ do estabelecimento beneficiário do pagamento.';

implementation

end.

