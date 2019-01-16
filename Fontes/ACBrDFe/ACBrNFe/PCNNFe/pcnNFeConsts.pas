{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
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
  //DSC_VPART = 'Valor';
  DSC_INDESCALA = 'Indicador de Escala de Produção';
  DSC_CNPJFAB = 'CNPJ do Fabricante da Mercadoria';
  DSC_CBENEF = 'Código de Benefício Fiscal na UF aplicado ao item';
  DSC_CAGREG = 'Código de Agregação';
  DSC_URLCHAVE = 'URL de consulta por chave de acesso a ser impressa no DANFE NFC-e';

implementation

end.

