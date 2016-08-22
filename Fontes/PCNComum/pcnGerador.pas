////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
// Desenvolvimento                                                            //
//         de Cte: Wiliam Zacarias da Silva Rosa                              //
//                                                                            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnGerador;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnConversao;

type

  TGeradorOpcoes = class;

  { TGerador }

  TGerador = class(TPersistent)
  private
    FArquivoFormatoXML: AnsiString;
    FArquivoFormatoTXT: AnsiString;
    FLayoutArquivoTXT: TstringList;
    FListaDeAlertas: TStringList;
    FTagNivel: string;
    FIDNivel: string;
    FOpcoes: TGeradorOpcoes;
    FPrefixo : string;
  public
    FIgnorarTagNivel: string;
    FIgnorarTagIdentacao: string;
    constructor Create;
    destructor Destroy; override;
    function SalvarArquivo(const CaminhoArquivo: string; const FormatoGravacao: TpcnFormatoGravacao = fgXML): Boolean;
    procedure wGrupo(const TAG: string; ID: string = ''; const Identar: Boolean = True);
    procedure wCampo(const Tipo: TpcnTipoCampo; ID, TAG: string; const min, max, ocorrencias: smallint; const valor: variant; const Descricao: string = ''; ParseTextoXML: Boolean = True; Atributo: String = '');
    procedure wGrupoNFSe(const TAG: string; ID: string = ''; const Identar: Boolean = True);
    procedure wCampoNFSe(const Tipo: TpcnTipoCampo; ID, TAG: string; const min, max, ocorrencias: smallint; const valor: variant; const Descricao: string = ''; ParseTextoXML: Boolean = True; Atributo: String = '');
    procedure wCampoCNPJCPF(const ID1, ID2: string; CNPJCPF: string; obrigatorio: Boolean = True; PreencheZeros: Boolean = True);
    procedure wCampoCNPJ(const ID: string; CNPJ: string; const cPais: Integer; obrigatorio: Boolean);
    procedure wCampoCPF(const ID: string; CPF: string; const cPais: Integer; obrigatorio: Boolean);
    procedure wAlerta(const ID, TAG, Descricao, Alerta: string);
    procedure wTexto(const Texto: string);
    procedure gtNivel(ID: string);
    procedure gtCampo(const Tag, ConteudoProcessado: string);
    procedure gtAjustarRegistros(const ID: string);
  published
    property ArquivoFormatoXML: AnsiString read FArquivoFormatoXML write FArquivoFormatoXML;
    property ArquivoFormatoTXT: AnsiString read FArquivoFormatoTXT write FArquivoFormatoTXT;
    property IDNivel: string read FIDNivel write FIDNivel;
    property ListaDeAlertas: TStringList read FListaDeAlertas write FListaDeAlertas;
    property LayoutArquivoTXT: TStringList read FLayoutArquivoTXT write FLayoutArquivoTXT;
    property Opcoes: TGeradorOpcoes read FOpcoes write FOpcoes;
    property Prefixo: string read FPrefixo write FPrefixo;
  end;

  { TGeradorOpcoes }

  TGeradorOpcoes = class(TPersistent)
  private
    FDecimalChar: Char;
    FSomenteValidar: boolean;
    FIdentarXML: boolean;
    FRetirarEspacos: boolean;
    FRetirarAcentos: boolean;
    FNivelIdentacao: integer;
    FTamanhoIdentacao: integer;
    FSuprimirDecimais: boolean;
    FTagVaziaNoFormatoResumido: boolean;
    FFormatoAlerta: string;
  public
    constructor Create;

  published
    property SomenteValidar: boolean read FSomenteValidar write FSomenteValidar default False;
    property RetirarEspacos: boolean read FRetirarEspacos write FRetirarEspacos default True;
    property RetirarAcentos: boolean read FRetirarAcentos write FRetirarAcentos default True;
    property IdentarXML: boolean read FIdentarXML write FIdentarXML default False;
    property TamanhoIdentacao: integer read FTamanhoIdentacao write FTamanhoIdentacao default 3;
    property SuprimirDecimais: boolean read FSuprimirDecimais write FSuprimirDecimais default False;
    property TagVaziaNoFormatoResumido: boolean read FTagVaziaNoFormatoResumido write FTagVaziaNoFormatoResumido default True;
    property FormatoAlerta: string read FFormatoAlerta write FFormatoAlerta;
    property DecimalChar: Char read FDecimalChar write FDecimalChar default '.';
  end;

const

  ERR_MSG_MAIOR = 'Tamanho maior que o máximo permitido';
  ERR_MSG_MENOR = 'Tamanho menor que o mínimo permitido';
  ERR_MSG_VAZIO = 'Nenhum valor informado';
  ERR_MSG_INVALIDO = 'Conteúdo inválido';
  ERR_MSG_MAXIMO_DECIMAIS = 'Numero máximo de casas decimais permitidas';
  ERR_MSG_MAIOR_MAXIMO = 'Número de ocorrências maior que o máximo permitido - Máximo ';
  ERR_MSG_GERAR_CHAVE = 'Erro ao gerar a chave da NFe!';
  ERR_MSG_FINAL_MENOR_INICIAL = 'O numero final não pode ser menor que o inicial';
  ERR_MSG_ARQUIVO_NAO_ENCONTRADO = 'Arquivo não encontrado';
  ERR_MSG_SOMENTE_UM = 'Somente um campo deve ser preenchido';
  ERR_MSG_MENOR_MINIMO = 'Número de ocorrências menor que o mínimo permitido - Mínimo ';

  CODIGO_BRASIL = 1058;

  XML_V01           = '?xml version="1.0"?';
  ENCODING_UTF8     = '?xml version="1.0" encoding="UTF-8"?';
  ENCODING_UTF8_STD = '?xml version="1.0" encoding="UTF-8" standalone="no"?';

  NAME_SPACE      = 'xmlns="http://www.portalfiscal.inf.br/nfe"';
  NAME_SPACE_CTE  = 'xmlns="http://www.portalfiscal.inf.br/cte"';
  NAME_SPACE_CFE  = 'xmlns="http://www.fazenda.sp.gov.br/sat"';
  NAME_SPACE_MDFE = 'xmlns="http://www.portalfiscal.inf.br/mdfe"';
  NAME_SPACE_GNRE = 'xmlns="http://www.gnre.pe.gov.br"';

  V0_02 = 'versao="0.02"';
  V1_00 = 'versao="1.00"';
  V1_01 = 'versao="1.01"';
  V1_02 = 'versao="1.02"';
  V1_03 = 'versao="1.03"';
  V1_04 = 'versao="1.04"';
  V1_07 = 'versao="1.07"';
  V1_10 = 'versao="1.10"';
  V2_00 = 'versao="2.00"';
  V2_01 = 'versao="2.01"';

  VM_Rodo_1_04  = 'versaoModal="1.04"';
  VM_Aereo_1_04 = 'versaoModal="1.04"';
  VM_Aqua_1_04  = 'versaoModal="1.04"';
  VM_Ferro_1_04 = 'versaoModal="1.04"';
  VM_Duto_1_04  = 'versaoModal="1.04"';

  // NFE //

  DSC_AAMM = 'Ano e Mês';
  DSC_ANOFAB = 'Ano de Fabricação';
  DSC_ANOMOD = 'Ano do Modelo de Fabricação';
  DSC_CCOR = 'Cor do Veículo';
  DSC_CDV = 'Digito Verificador';
  DSC_CEAN = 'Código de Barra do Item';
  DSC_CEANTRIB = 'Código de Barra do Item Tributação';
  DSC_CENQ = 'Código de Enquadramento Legal do IPI';
  DSC_CEP = 'CEP';
  DSC_CEXPORTADOR = 'Código do exportador';
  DSC_CFABRICANTE = 'Fabricante';
  DSC_CFOP = 'CFOP';
  DSC_CHASSI = 'Número do chassi';
  DSC_CHAVE = 'Chave da NFe';
  DSC_CLENQ = 'Classe de enquadramento do IPI para Cigarros e Bebidas';
  DSC_CLISTSERV = 'Lista Prestação de Serviços';
  DSC_CSITTRIB = 'Código de Tributação do ISSQN';
  DSC_CILIN = 'Cilindradas';
  DSC_CMT = 'Capacidade Máxima de Tração';
  DSC_CMOD = 'Modelo do Veículo';
  DSC_CCORDEN = 'Código da Cor DENATRAN';
  DSC_TPREST = 'Restrição';
  DSC_CMUN = 'Código do Município';
  DSC_CMUNFG = 'Código do Município FG';
  DSC_CNAE = 'Classificação Nacional de Atividades Econômicas';
  DSC_CRT = 'Código de Regime Tributário';
  DSC_CNF = 'Número da Nota Fiscal Eletrônica';
  DSC_CODIF = 'Código de autorização / registro do CODIF';
  DSC_CONDVEIC = 'Condições do Veículo';
  DSC_CPAIS = 'Código do País';
  DSC_CNPJPROD = 'CNPJ do produtor da mercadoria, quando diferente do emitente';
  DSC_CPROD = 'Código do produto ou serviço';
  DSC_CPRODANP = 'Código do produto ANP';
  DSC_CSELO = 'Código do selo';
  DSC_CST = 'Código da situação tributária ';
  DSC_CSOSN = 'Código de Situação da Operação – Simples Nacional';
  DSC_CUF = 'Código do UF (Unidade da Federação)';
  DSC_DDESEMB = 'Data do Desembaraço Aduaneiro';
  DSC_DDI = 'Data de registro da DI/DSI/DA';
  DSC_DEMI = 'Data de emissão';
  DSC_DESCR = 'Descrição completa';
  DSC_DFAB = 'Data de fabricação';
  DSC_DIST = 'Distância entre os eixos';
  DSC_DPAG = 'Data de pagamento do Documento de Arrecadação';
  DSC_DSAIENT = 'Data de saída ou entrada da mercadoria/produto';
  DSC_HSAIENT = 'Hora de saída ou entrada da mercadoria/produto';
  DSC_DVAL = 'Data de validade';
  DSC_DVENC = 'Data de vencimento';
  DSC_ESP = 'Espécie dos volumes transportados';
  DSC_ESPVEIC = 'Espécie de veículo';
  DSC_EXTIPI = 'EX_TIPI';
  DSC_FINNFE = 'Finalidade de emissão da NFe';
  DSC_FONE = 'Telefone';
  DSC_GENERO = 'Gênero do produto ou serviço';
  DSC_IE = 'Inscrição Estadual';
  DSC_IEST = 'Inscrição Estadual do Substituto tributário';
  DSC_IM = 'Inscrição Municipal';
  DSC_INDPAG = 'Indicador da forma de pagamento';
  DSC_INDPROC = 'Indicador da origem do processo';
  DSC_INFADFISCO = 'Informações adicionais de interesse do Fisco';
  DSC_INFADPROD = 'Informações adicionais do Produto';
  DSC_INFCPL = 'Informações complementares de interesse do contribuinte';
  DSC_ISUF = 'Inscrição na SUFRAMA';
  DSC_EMAIL = 'Endereço de Email';
  DSC_IPITrib = 'IPI Tributável';
  DSC_MARCA = 'Marca dos volumes transportados';
  DSC_MATR = 'Matrícula do agente';
  DSC_MOD = 'Modelo';
  DSC_NECF = 'Número de ordem seqüencial do ECF';
  DSC_NCOO = 'Número do Contador de Ordem de Operação - COO';
  DSC_MODBC = 'Modalidade de determinação da BC do ICMS';
  DSC_MODBCST = 'Modalidade de determinação da BC do ICMS ST';
  DSC_MOTDESICMS = 'Motivo da desoneração do ICMS';
  DSC_MODFRETE = 'Modalidade do Frete';
  DSC_NADICAO = 'Numero da Adição';
  DSC_NATOP = 'Descrição da Natureza da Operação';
  DSC_DHCONT = 'Data e Hora de entrada em contingencia';
  DSC_XJUSTCONT = 'Justificativa de entrada em contingencia';
  DSC_NCANO = 'Numero de série do cano';
  DSC_NCM = 'Código NCM';
  DSC_NDI = 'Numero do Documento de Importação DI/DSI/DA';
  DSC_nDAR = 'Número do Documento Arrecadação de Receita';
  DSC_NDUP = 'Número da duplicata';
  DSC_NFAT = 'Número da fatura';
  DSC_NITEM = 'Numero do item';
  DSC_NLACRE = 'Número dos Lacres';
  DSC_NLOTE = 'Número do Lote do medicamento';
  DSC_NMOTOR = 'Número de Motor';
  DSC_NNF = 'Número do Documento Fiscal';
  DSC_NPROC = 'Identificador do processo ou ato concessório';
  DSC_NRO = 'Número';
  DSC_NSEQADIC = 'Numero seqüencial do item dentro da adição';
  DSC_NSERIE = 'Número de série';
  DSC_NVOL = 'Numeração dos volumes transportados';
  DSC_ORIG = 'Origem da mercadoria';
  DSC_OBSCONT = 'Observações de interesse do contribuite';
  DSC_OBSFISCO = 'Observações de interesse do fisco';
  DSC_PCOFINS = 'Alíquota da COFINS (em percentual)';
  DSC_PESOB = 'Peso Bruto (em kg)';
  DSC_PESOL = 'Peso Líquido (em kg)';
  DSC_PICMS = 'Alíquota do imposto';
  DSC_PICMSRET = 'Alíquota da Retenção';
  DSC_PICMSST = 'Alíquota do imposto do ICMS ST';
  DSC_PIPI = 'Alíquota do IPI';
  DSC_PISOUTR = 'Grupo PIS outras operações';
  DSC_PLACA = 'Placa do Veículo';
  DSC_PMVAST = 'Percentual da margem de valor Adicionado do ICMS ST';
  DSC_POT = 'Potência Motor';
  DSC_PPIS = 'Alíquota do PIS (em percentual)';
  DSC_PREDBC = 'Percentual da Redução de BC';
  DSC_PCREDSN = 'Alíquota aplicável de cálculo do crédito (Simples Nacional).';
  DSC_VCREDICMSSN = 'Valor crédito do ICMS que pode ser aproveitado nos termos do art. 23 da LC 123 (Simples Nacional)';
  DSC_PREDBCST = 'Percentual da Redução de BC do ICMS ST';
  DSC_UFST = 'UF para qual é devido o ICMS ST';
  DSC_PBCOP = 'Percentual da BC operação própria';
  DSC_PROCEMI = 'Processo de emissão da NF-e';
  DSC_QBCPROD = 'BC da CIDE';
  DSC_QCOM = 'Quantidade Comercial';
  DSC_QLOTE = 'Quantidade de produto no Lote do medicamento';
  DSC_QSELO = 'Quantidade de selo de controle';
  DSC_QTEMP = 'Quantidade de combustível faturada à temperatura ambiente.';
  DSC_QTRIB = 'Quantidade Tributável';
  DSC_QUNID = 'Quantidade total na unidade padrão para tributação (somente para os produtos tributados por unidade)';
  DSC_QVOL = 'Quantidade de volumes transportados';
  DSC_REBOQUE = 'Reboque';
  DSC_REFNFE = 'Chave de acesso das NF-e referenciadas';
  DSC_RENAVAM = 'RENAVAM';
  DSC_REPEMI = 'Repartição Fiscal emitente';
  DSC_RNTC = 'Registro Nacional de Transportador de Carga (ANTT)';
  DSC_VAGAO = 'Identificação do vagão';
  DSC_BALSA = 'Identificação da balsa';
  DSC_SERIE = 'Série do Documento Fiscal';
  DSC_TPAMB = 'Identificação do Ambiente';
  DSC_TPARMA = 'Indicador do tipo de arma de fogo';
  DSC_TPCOMB = 'Tipo de combustível';
  DSC_TPEMIS = 'Forma de Emissão da NF-e';
  DSC_TPIMP = 'Formato de Impressão do DANFE';
  DSC_TPNF = 'Tipo do Documento Fiscal';
  DSC_TPOP = 'Tipo da operação';
  DSC_TPPINT = 'Tipo de Pintura';
  DSC_TPVEIC = 'Tipo de Veículo';
  DSC_UCOM = 'Unidade Comercial';
  DSC_UF = 'Sigla da UF';
  DSC_UFCONS = 'Sigla da UF de consumo';
  DSC_UFDESEMB = 'Sigla da UF onde ocorreu o Desembaraço Aduaneiro';
  DSC_UFEMBARQ = 'Sigla da UF onde ocorrerá o embarque dos produtos';
  DSC_UTRIB = 'Unidade Tributável';
  DSC_VALIQ = 'Alíquota';
  DSC_VALIQPROD = 'Valor da alíquota (em reais)';
  DSC_VBC = 'Valor da BC do ICMS';
  DSC_VBCICMS = 'BC do ICMS';
  DSC_VBCICMSST = 'BC do ICMS ST retido';
  DSC_VBCICMSSTCONS = 'Valor do ICMS ST da UF de consumo';
  DSC_VBCICMSSTDEST = 'BC do ICMS ST da UF de destino';
  DSC_VBCIRRF = 'Base de Cálculo do IRRF';
  DSC_VBCRET = 'BC da Retenção do ICMS';
  DSC_VBCRETPREV = 'Base de Cálculo da Retenção da Previdência Social';
  DSC_VBCST = 'Valor da BC do ICMS ST';
  DSC_VBCSTRET = 'Valor da BC do ICMS ST Retido';
  DSC_VCIDE = 'Valor da CIDE';
  DSC_VCOFINS = 'Valor do COFINS';
  DSC_VDAR = 'Valor total constante no Documento de arrecadação de Receita';
  DSC_VDESC = 'Valor do desconto';
  DSC_INDTOT = 'Indicador de soma no total da NFe';
  DSC_VDESCDI = 'Valor do desconto do item da DI – adição';
  DSC_NITEMPED = 'Item do Pedido de Compra da DI – adição';
  DSC_VDESPADU = 'Valor das despesas aduaneiras';
  DSC_VDUP = 'Valor da duplicata';
  DSC_VERPROC = 'Versão do Processo de emissão da NF-e';
  DSC_VFRETE = 'Valor Total do Frete';
  DSC_VICMS = 'Valor do ICMS';
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
  DSC_VBCISS = 'Valor da Base de Cálculo do ISSQN';
  DSC_VISS = 'Valor do Imposto sobre Serviço';
  DSC_VISSQN = 'Valor do Imposto sobre Serviço de Qualquer Natureza';
  DSC_VLIQ = 'Valor Líquido da Fatura';
  DSC_VNF = 'Valor Total da NF-e';
  DSC_VORIG = 'Valor Original da Fatura';
  DSC_VOUTRO = 'Outras Despesas Acessórias';
  DSC_VPIS = 'Valor do PIS';
  DSC_VPMC = 'Preço Máximo ao Consumidor';
  DSC_VPROD = 'Valor Total Bruto dos Produtos ou Serviços';
  DSC_VRETCOFINS = 'Valor Retido do COFINS';
  DSC_VRETCSLL = 'Valor Retido da CONTRIBUIÇÃO SOCIAL SOBRE O LUCRO LÍQUIDO';
  DSC_VRETPIS = 'Valor Retido do PIS';
  DSC_VRETPREV = 'Valor da Retenção da Previdência Social';
  DSC_VSEG = 'Valor Total do Seguro';
  DSC_VSERV = 'Valor total dos Serviços sob não incidência ou não Tributados pelo ICMS  / Valor do Serviço';
  DSC_VST = 'Valor TOTAL Icms substituição Tributária';
  DSC_VUNCOM = 'Valor Unitário de Comercialização';
  DSC_VUNID = 'Valor por Unidade Tributável';
  DSC_VUNTRIB = 'Valor unitário de Tributação';
  DSC_XAGENTE = 'Nome do agente';
  DSC_XBAIRRO = 'Bairro';
  DSC_XCAMPO = 'Identificação do Campo';
  DSC_XCONT = 'Contrato';
  DSC_XCOR = 'Descrição da Cor';
  DSC_XCPL = 'Complemento (Endereço)';
  DSC_XENDER = 'Endereço Completo';
  DSC_XFANT = 'Nome de Fantasia';
  DSC_XLGR = 'Logradouro';
  DSC_XLOCDESEMB = 'Local de Desembaraço';
  DSC_XLOCEMBARQ = 'Local onde ocorrerá o embarque dos produtos';
  DSC_XMUN = 'Nome do Município';
  DSC_XNEMP = 'Nota de Empenho';
  DSC_XNOME = 'Razão Social ou Nome';
  DSC_XPAIS = 'Nome do País';
  DSC_XPED = 'Pedido';
  DSC_XPROD = 'Descrição do Produto ou Serviço';
  DSC_XTEXTO = 'Conteúdo do Campo';
  DSC_XORGAO = 'Orgão emitente';
  DSC_DigestValue = 'Digest Value';
  DSC_SignatureValue = 'Signature Value';
  DSC_X509Certificate = 'X509 Certificate';
  DSC_XSERV = 'Descrição do serviço';
  DSC_ANO = 'Ano';
  DSC_CNPJ = 'CNPJ(MF)';
  DSC_CPF = 'CPF';
  DSC_NNFINI = 'Numero inicial';
  DSC_NNFFIN = 'Numero final';
  DSC_XJUST = 'Justificativa';
  DSC_CHNFE = 'Chave da NFe';
  DSC_NPROT = 'Numero do protocolo';
  DSC_NREC = 'Numero do recibo';
  DSC_IDLOTE = 'Numero do Lote';
  DSC_VERAPLIC = 'Versão do aplicativo';
  DSC_NREGDPEC = 'Número de registro do DPEC';
  DSC_DPEC_ID = 'Grupo de Identificação da TAG a ser assinada. DPEC + CNPJ do emissor.';
  DSC_SAFRA = 'Identificação da safra';
  DSC_REF = 'Mês e ano de referência';
  DSC_FORDIA = 'Grupo de Fornecimento diário de cana';
  DSC_DIA = 'Dia';
  DSC_QTDE = 'Quantidade';
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
  DSC_ULTNSU = 'Último NSU recebido pela Empresa';
  DSC_NSU = 'NSU específico';
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
  DSC_VAFRMM = 'Valor da AFRMM';
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
  DSC_VINSS = 'Valor do INSS';
  DSC_VIR = 'Valor do IR';
  DSC_VCSLL = 'Valor do CSLL';
  DSC_VOUTRODED = 'Valor Outras Deduções';
  DSC_VDESCINCOND = 'Valor Desconto Incondicionado';
  DSC_VDESCCOND = 'Valor Desconto Condicionado';
  DSC_INDISSRET = 'Indicador de ISS Retido';
  DSC_VISSRET = 'Valor Retenção ISS';
  DSC_INDISS = 'Indicador da Exigibilidade do ISS';
  DSC_CSERVICO = 'Código do Serviço';
  DSC_NPROCESSO = 'Número do Processo';
  DSC_CREGTRIB = 'Código do Regime Especial de Tributação';
  DSC_INDINCENTIVO = 'Indicador de Incentivo Fiscal';
  DSC_XLOCDESP = 'Local de Despacho';
  DSC_TPAG = 'Forma de Pagamento';
  DSC_VPAG = 'Valor do Pagamento';
  DSC_TBAND = 'Bandeira da Operadora de Cartão';
  DSC_CAUT = 'Número da Autorização';
  DSC_TPINTEGRA = 'Tipo de Integração para pagamento';
  DSC_CEST = 'Código Identificador da Substitução Tributária';
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

  // CTE //
  DSC_CHCTE    = 'Chave do CTe';
  DSC_TPCTe    = 'Tipo do Conhecimento';
  DSC_REFCTE   = 'Chave de acesso do CT-e referenciado';
  DSC_CMUNEMI  = 'Código do Município onde o CT-e está sendo emitido';
  DSC_ORIGCALC = 'Município origem para efeito de cálculo do frete';
  DSC_DESTCALC = 'Município destino para efeito de cálculo do frete';
  DSC_XOBS     = 'Observações Gerais';
  DSC_TOMA     = 'Tomador do Serviço';
  DSC_INFNF    = 'Informações da Nota Fiscal';
  DSC_INFNFE   = 'Informações da Nota Fiscal Eletrônica';
  DSC_NDOC     = 'Número da Nota Fiscal';
  DSC_PESO     = 'Peso';
  DSC_TPDOC    = 'Tipo de documento originário';
  DSC_OUTROS   = 'Descrição';
  DSC_VTPREST  = 'Valor total da prestação do serviço';
  DSC_VREC     = 'Valor a Receber';
  DSC_XNOMEC   = 'Nome do Componente';
  DSC_VCOMP    = 'Valor do Componente';
  DSC_VCRED    = 'Valor do Crédito outorgado/presumido';
  DSC_RESPSEG  = 'Responsável pelo Seguro';
  DSC_XSEG     = 'Nome da Seguradora';
  DSC_NAPOL    = 'Número da Apólice';
  DSC_NAVER    = 'Número da Averbação';
  DSC_VMERC    = 'Valor da mercadoria para efeito de averbação';

  DSC_INFSEG   = 'Informações de seguro da carga';
  DSC_RNTRC    = 'Registro Nacional de Transportadores Rodoviários de Carga';
  DSC_DPREV    = 'Data prevista de entrega';
  DSC_LOTA     = 'Indicador de lotação';
  DSC_CINT     = 'Código interno do emitente';
  DSC_LACR     = 'Grupo de lacres';
  DSC_TPPROP   = 'Tipo de Proprietário';
  DSC_INFOUTRO = 'informações dos demais documentos';
  DSC_VDOC     = 'Valor do documento';
  DSC_MODAL    = 'Tipo de Modal';
  DSC_TPSERV   = 'Tipo do Serviço';
  DSC_RETIRA   = 'Recebedor retira na Filial?';
  DSC_PRED     = 'Produto predominante';
  DSC_OUTCAT   = 'Outras características da carga';
  DSC_VTMERC   = 'Valor total da mercadoria';
  DSC_INFQ     = 'Informações de quantidades da Carga';
  DSC_CUNID    = 'Código da unidade de medida';
  DSC_TPMED    = 'Tipo da Medida';
  DSC_QTD      = 'Quantidade';
  DSC_DRET     = 'Detalhes da Retirada';

  DSC_XCARACAD  = 'Caracteristica adicional do transporte';
  DSC_XCARACSET = 'Caracteristica adicional do serviço';
  DSC_XEMI      = 'Funcionário emissor do CTe';
  DSC_XORIG     = 'Sigla ou código interno de Origem';
  DSC_XPASS     = 'Sigla ou código interno da Passagem';
  DSC_XDEST     = 'Sigla ou código interno do Destino';
  DSC_XROTA     = 'Código da Rota de Entrega';
  DSC_TPPER     = 'Tipo de data/período programado para entrega';
  DSC_DPROG     = 'Data programada';
  DSC_DINI      = 'Data inicial';
  DSC_DFIM      = 'Data final';
  DSC_TPHOR     = 'Tipo de hora para entrega';
  DSC_HPROG     = 'Hora programada';
  DSC_HINI      = 'Hora inicial';
  DSC_HFIM      = 'Hora final';
  DSC_NROMA     = 'Numero do Romaneio';
  DSC_NPED      = 'Numero do Pedido';
  DSC_QTDRAT    = 'Quantidade Rateada';
  DSC_NONU      = 'Número ONU/UN';
  DSC_XNOMEAE   = 'Nome apropriado para embarque do produto';
  DSC_XCLARISCO = 'Classe e Risco secundário';
  DSC_GREMB     = 'Grupo de Embalagem';
  DSC_QTOTPROD  = 'Quantidade total por produto';
  DSC_QVOLTIPO  = 'Quantidade e tipo de volumes';
  DSC_VUNITV    = 'Valor Unitário do Veículo';
  DSC_VFRETEV   = 'Frete Unitário';
  DSC_CIOT      = 'Código Identificador da Operação de Transporte';
  DSC_NOCC      = 'Número da Ordem de Coleta';
  DSC_NCOMPRA   = 'Número do Comprovante de Compra';
  DSC_VVALEPED  = 'Valor do Vale-Pedagio';
  DSC_CINTV     = 'Código interno do Veículo';
  DSC_TARA      = 'Tara em KG';
  DSC_CAPKG     = 'Capacidade em KG';
  DSC_CAPM3     = 'Capacidade em M3';
  DSC_TPROD     = 'Tipo de Rodado';
  DSC_TPCAR     = 'Tipo de Carroceria';
  DSC_INDSN     = 'Indicador de Simples Nacional';
  DSC_NMINU     = 'Número da Minuta';
  DSC_NOCA      = 'Número Operacional do Conhecimento Aéreo';
  DSC_XLAGEMI   = 'Identificação do Emissor';
  DSC_IDT       = 'Identificação Interna do Tomador';
  DSC_CL        = 'Classe';
  DSC_CTAR      = 'Código da Tarifa';
  DSC_VTAR      = 'Valor da Tarifa';
  DSC_XDIME     = 'Dimensão';
  DSC_CINFMANU  = 'Informações de Manuseio';
  DSC_CIMP      = 'Carga Especial';
  DSC_VPREST    = 'Valor da prestação BC do AFRMM';
  DSC_NBOOKING  = 'Número do Booking';
  DSC_NCTRL     = 'Número de Controle';
  DSC_XNAVIO    = 'Identificação do Navio';
  DSC_XBALSA    = 'Identificador da Balsa';
  DSC_NVIAG     = 'Número da Viagem';
  DSC_DIREC     = 'Direção';
  DSC_PRTEMB    = 'Porto de Embarque';
  DSC_PRTTRANS  = 'Porto de Transbordo';
  DSC_PRTDEST   = 'Porto de Destino';
  DSC_TPNAV     = 'Tipo de Navegação';
  DSC_IRIN      = 'IRIN do Navio';
  DSC_TPTRAF    = 'Tipo de Tráfego';
  DSC_RESPFAT   = 'Responsável pelo Faturamento';
  DSC_FERREMI   = 'Ferrovia Emitente do CTe';
  DSC_FLUXO     = 'Fluxo Ferroviário';
  DSC_IDTREM    = 'Identificação do Trem';
  DSC_CINTF     = 'Código interno da Ferrovia envolvida';
  DSC_CAPTO     = 'Capacidade em Toneladas';
  DSC_TPVAG     = 'Tipo de Vagão';
  DSC_PESOR     = 'Peso Real em Toneladas';
  DSC_PESOBC    = 'Peso Base de Calculo de Frete em Toneladas';
  DSC_COTM      = 'Número do Certificado do Operador de Transporte Multimodal';
  DSC_INDNEG    = 'Indicador Negociável';

  DSC_TPUNIDTRANSP = 'Tipo de Unidade de Transporte';
  DSC_IDUNIDTRANSP = 'Identificação da Unidade de Transporte';
  DSC_TPUNIDCARGA  = 'Tipo de Unidade de Carga';
  DSC_IDUNIDCARGA  = 'Identificação da Unidade de Carga';
  DSC_PONTOFULGOR  = 'Ponto de Fulgor';


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

implementation

uses
  DateUtils, ACBrConsts, ACBrUtil;

{ TGeradorOpcoes }

constructor TGeradorOpcoes.Create;
begin
  inherited;

  FIdentarXML := False;
  FTamanhoIdentacao := 3;
  FFormatoAlerta := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'; // Vide comentário em wAlerta
  FRetirarEspacos := True;
  FRetirarAcentos := True;
  FSuprimirDecimais := False;
  FSomenteValidar := False;
  FTagVaziaNoFormatoResumido := True;
  FDecimalChar := '.';
end;

{ TGerador }

constructor TGerador.Create;
begin
  inherited;

  FOpcoes := TGeradorOpcoes.Create;
  FListaDeAlertas := TStringList.Create;
  FLayoutArquivoTXT := TStringList.Create;
end;

destructor TGerador.Destroy;
begin
  FOpcoes.Free;
  FListaDeAlertas.Free;
  FLayoutArquivoTXT.Free;
  FIgnorarTagNivel := '!@#';
  FIgnorarTagIdentacao := '!@#';
  inherited Destroy;
end;

function TGerador.SalvarArquivo(const CaminhoArquivo: string; const FormatoGravacao: TpcnFormatoGravacao = fgXML): Boolean;
var
  ArquivoGerado: TStringList;
begin
  // Formato de gravação somente é válido para NFe
  ArquivoGerado := TStringList.Create;
  try
    try
      if FormatoGravacao = fgXML then
        ArquivoGerado.Add(FArquivoFormatoXML)
      else
        ArquivoGerado.Add(FArquivoFormatoTXT);
      ArquivoGerado.SaveToFile(CaminhoArquivo);
      Result := True;
    except
      Result := False;
      raise;
    end;
  finally
    ArquivoGerado.Free;
  end;
end;

procedure TGerador.wAlerta(const ID, TAG, Descricao, Alerta: string);
var
  s: string;
begin
  // O Formato da mensagem de erro pode ser alterado pelo usuario alterando-se a property FFormatoAlerta: onde;
  // %TAGNIVEL%  : Representa o Nivel da TAG; ex: <transp><vol><lacres>
  // %TAG%       : Representa a TAG; ex: <nLacre>
  // %ID%        : Representa a ID da TAG; ex X34
  // %MSG%       : Representa a mensagem de alerta
  // %DESCRICAO% : Representa a Descrição da TAG
  s := FOpcoes.FFormatoAlerta;
  s := stringReplace(s, '%TAGNIVEL%', FTagNivel, [rfReplaceAll]);
  s := stringReplace(s, '%TAG%', TAG, [rfReplaceAll]);
  s := stringReplace(s, '%ID%', ID, [rfReplaceAll]);
  s := stringReplace(s, '%MSG%', Alerta, [rfReplaceAll]);
  s := stringReplace(s, '%DESCRICAO%', Trim(Descricao), [rfReplaceAll]);
  if Trim(Alerta) <> '' then
    FListaDeAlertas.Add(s);
end;

procedure TGerador.wGrupo(const TAG: string; ID: string = ''; const Identar: Boolean = True);
begin
  // A propriedade FIgnorarTagNivel é utilizada para Ignorar TAG
  // na construção dos níveis para apresentação na mensagem de erro.
  gtNivel(ID);
  // Caso a tag seja um Grupo com Atributo
  if (pos('="', TAG) > 0) or (pos('= "', TAG) > 0) then
    gtCampo(RetornarConteudoEntre(TAG, ' ', '='), RetornarConteudoEntre(TAG, '"', '"'));
  //
  if not SubStrEmSubStr(TAG, FIgnorarTagNivel) then
  begin
    if TAG[1] <> '/' then
      FTagNivel := FTagNivel + '<' + TAG + '>';
    if (TAG[1] = '/') and (Copy(TAG, 2, 3) = 'det') then
      FTagNivel := copy(FTagNivel, 1, pos('<det', FTagNivel) - 1)
    else
      FTagNivel := StringReplace(FTagNivel, '<' + Copy(TAG, 2, MaxInt) + '>', '', []);
  end;
  //
  if (Identar) and (TAG[1] = '/') then
    Dec(FOpcoes.FNivelIdentacao);
  if SubStrEmSubStr(TAG, FIgnorarTagIdentacao) then
    Dec(FOpcoes.FNivelIdentacao);
  //
  if FOpcoes.IdentarXML then
    FArquivoFormatoXML := FArquivoFormatoXML + StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) + '<' + tag + '>' + #13#10
  else
    FArquivoFormatoXML := FArquivoFormatoXML + '<' + tag + '>';
  if (Identar) and (TAG[1] <> '/') then
    Inc(FOpcoes.FNivelIdentacao);
end;

procedure TGerador.wCampoCNPJCPF(const ID1, ID2: string; CNPJCPF: string;
  obrigatorio: Boolean; PreencheZeros: Boolean);
var
  Tamanho: integer;
  Ocorrencia: Integer;
begin
  CNPJCPF    := SomenteNumeros(trim(CNPJCPF));
  Tamanho    := length(CNPJCPF);
  Ocorrencia := Integer(obrigatorio);

  if (Tamanho <= 11) and (Tamanho > 0) then    // Se Vazio dá preferencia a CNPJ
  begin
    if PreencheZeros and (Tamanho <> 11) then
    begin
      CNPJCPF := PadLeft(CNPJCPF,11,'0');
      Tamanho := 11;
    end;

    wCampo(tcStr, ID2, 'CPF  ', 0, 11, Ocorrencia, CNPJCPF);
    if not ValidarCPF(CNPJCPF) then
      wAlerta(ID2, 'CPF', 'CPF', ERR_MSG_INVALIDO);
  end
  else
  begin
    if PreencheZeros and (obrigatorio or (Tamanho > 0))  and (Tamanho <> 14) then
    begin
      CNPJCPF := PadLeft(CNPJCPF,14,'0');
      Tamanho := 14;
    end;

    wCampo(tcStr, ID1, 'CNPJ', 0, 14, Ocorrencia, CNPJCPF);
    if (Tamanho > 0) and (not ValidarCNPJ(CNPJCPF)) then
      wAlerta(ID1, 'CNPJ', 'CNPJ', ERR_MSG_INVALIDO);
  end;

  if (not (Tamanho in[0,11,14])) then
    wAlerta(ID1 + '-' + ID2, 'CNPJ-CPF', 'CNPJ/CPF', ERR_MSG_INVALIDO);
end;

procedure TGerador.wCampoCNPJ(const ID: string; CNPJ: string; const cPais: Integer; obrigatorio: Boolean);
begin
  if cPais <> 1058 then
  begin
    wCampo(tcStr, ID, 'CNPJ', 00, 00, 1, '');
    exit;
  end;
  CNPJ := SomenteNumeros(trim(CNPJ));
  if obrigatorio then
    wCampo(tcEsp, ID, 'CNPJ', 14, 14, 1, CNPJ, DSC_CNPJ)
  else
    wCampo(tcEsp, ID, 'CNPJ', 14, 14, 0, CNPJ, DSC_CNPJ);
  if not ValidarCNPJ(CNPJ) then
    wAlerta(ID, 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
end;

procedure TGerador.wCampoCPF(const ID: string; CPF: string; const cPais: Integer; obrigatorio: Boolean);
begin
  if cPais <> 1058 then
  begin
    wCampo(tcStr, ID, 'CPF', 00, 00, 1, '');
    exit;
  end;
  CPF := SomenteNumeros(trim(CPF));
  if obrigatorio then
    wCampo(tcEsp, ID, 'CPF', 11, 11, 1, CPF, DSC_CPF)
  else
    wCampo(tcEsp, ID, 'CPF', 11, 11, 0, CPF, DSC_CPF);
  if not ValidarCPF(CPF) then
    wAlerta(ID, 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
end;

procedure TGerador.wCampo(const Tipo: TpcnTipoCampo; ID, TAG: string; const min, max, ocorrencias: smallint; const valor: variant; const Descricao: string = ''; ParseTextoXML : Boolean = True; Atributo: String = '');

  function IsEmptyDate( wAno, wMes, wDia: Word): Boolean;
  begin
    Result := ((wAno = 1899) and (wMes = 12) and (wDia = 30));
  end;

var
  NumeroDecimais: smallint;
  valorInt, TamMin, TamMax: Integer;
  valorDbl: Double;
  alerta, ConteudoProcessado, ATag: string;
  wAno, wMes, wDia, wHor, wMin, wSeg, wMse: Word;
  EstaVazio: boolean;
begin
  ID                  := Trim(ID);
  Tag                 := Trim(TAG);
  Atributo            := Trim(Atributo);
  EstaVazio           := False;
  NumeroDecimais      := 0;
  ConteudoProcessado  := '';
  TamMax              := max;
  TamMin              := min;

  case Tipo of
    tcStr:
      begin
        ConteudoProcessado := Trim( VarToStr(valor) );
        EstaVazio := ConteudoProcessado = '';
      end;

    tcDat, tcDatCFe:
      begin
        DecodeDate( VarToDateTime(valor), wAno, wMes, wDia);
        ConteudoProcessado := FormatFloat('0000', wAno) + '-' + FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia);
        if Tipo = tcDatCFe then
          ConteudoProcessado := SomenteNumeros(ConteudoProcessado);

        EstaVazio := IsEmptyDate( wAno, wMes, wDia );
      end;

    tcDatVcto:
      begin
        DecodeDate( VarToDateTime(valor), wAno, wMes, wDia);
        ConteudoProcessado := FormatFloat('00', wDia)+ '/' + FormatFloat('00', wMes)+ '/' +FormatFloat('0000', wAno);
        EstaVazio := IsEmptyDate( wAno, wMes, wDia );
      end;

    tcHor, tcHorCFe:
      begin
        DecodeTime( VarToDateTime(valor), wHor, wMin, wSeg, wMse);
        ConteudoProcessado := FormatFloat('00', wHor) + ':' + FormatFloat('00', wMin) + ':' + FormatFloat('00', wSeg);
        if Tipo = tcHorCFe then
          ConteudoProcessado := SomenteNumeros(ConteudoProcessado);

        EstaVazio := (wHor = 0) and (wMin = 0) and (wSeg = 0);
      end;

    tcDatHor:
      begin
        DecodeDateTime( VarToDateTime(valor), wAno, wMes, wDia, wHor, wMin, wSeg, wMse);
        ConteudoProcessado := FormatFloat('0000', wAno) + '-' +
                              FormatFloat('00', wMes) + '-' +
                              FormatFloat('00', wDia) + 'T' +
                              FormatFloat('00', wHor) + ':' +
                              FormatFloat('00', wMin) + ':' +
                              FormatFloat('00', wSeg);
        EstaVazio := ((wAno = 1899) and (wMes = 12) and (wDia = 30));
      end;

    tcDe2, tcDe3, tcDe4, tcDe6, tcDe10:
      begin
        // adicionar um para que o máximo e mínimo não considerem a virgula
        if not FOpcoes.FSuprimirDecimais then
        begin
          TamMax := TamMax + 1;
          TamMin := TamMin + 1;
        end;
        
        // Tipo numerico com decimais
        case Tipo of
          tcDe2 : NumeroDecimais :=  2;
          tcDe3 : NumeroDecimais :=  3;
          tcDe4 : NumeroDecimais :=  4;
          tcDe6 : NumeroDecimais :=  6;
          tcDe10: NumeroDecimais := 10;
        end;

        try
          valorDbl := valor; // Converte Variant para Double
          ConteudoProcessado := FloatToString( valorDbl, FOpcoes.DecimalChar, FloatMask(NumeroDecimais));
        except
          valorDbl := 0;
          ConteudoProcessado := '0.00';
        end;

        EstaVazio := (valorDbl = 0) and (ocorrencias = 0);

        if StrToIntDef(Copy(ConteudoProcessado, pos(FOpcoes.DecimalChar, ConteudoProcessado) + NumeroDecimais + 1, 10),0) > 0 then
          walerta(ID, Tag, Descricao, ERR_MSG_MAXIMO_DECIMAIS + ' ' + IntToStr(NumeroDecimais));

        // Caso não seja um valor fracionário; retira os decimais.
        if FOpcoes.FSuprimirDecimais then
          if int(valorDbl) = valorDbl then
            ConteudoProcessado := IntToStr(Round(valorDbl));

        if Length(ConteudoProcessado) < TamMin then
          ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
      end;

    tcEsp:
      begin
        // Tipo String - somente numeros
        ConteudoProcessado  := trim(valor);
        EstaVazio           := (valor = '');
        if not ValidarNumeros(ConteudoProcessado) then
          walerta(ID, Tag, Descricao, ERR_MSG_INVALIDO);
      end;

    tcInt:
      begin
        // Tipo Inteiro
        try
          valorInt := valor;
          ConteudoProcessado := IntToStr(valorInt);
        except
          valorInt := 0;
          ConteudoProcessado := '0';
        end;

        EstaVazio := (valorInt = 0) and (ocorrencias = 0);

        if Length(ConteudoProcessado) < TamMin then
          ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
      end;
  end;

  alerta := '';
  //(Existem tags obrigatórias que podem ser nulas ex. cEAN)  if (ocorrencias = 1) and (EstaVazio) then
  if (ocorrencias = 1) and (EstaVazio) and (TamMin > 0) then
    alerta := ERR_MSG_VAZIO;

  if (length(ConteudoProcessado) < TamMin) and (alerta = '') and (length(ConteudoProcessado) > 1) then
    alerta := ERR_MSG_MENOR;

  if length(ConteudoProcessado) > TamMax then
     alerta := ERR_MSG_MAIOR;

  // Grava alerta //
  if (alerta <> '') and (pos(ERR_MSG_VAZIO, alerta) = 0) and (not EstaVazio) then
    alerta := alerta + ' [' + VarToStr(valor) + ']';

  walerta(ID, TAG, Descricao, alerta);
  // Sai se for apenas para validar //
  if FOpcoes.FSomenteValidar then
    exit;

  // Grava no Formato Texto
  if not EstaVazio then
    gtCampo(tag, ConteudoProcessado)
  else
    gtCampo(tag, '');

  // adiciona o espaço ao inicio do atributo para não colar na tag se nao estiver vazio
  if Atributo <> '' then
    Atributo := ' ' + Atributo;

  // Grava a tag no arquivo - Quando não existir algum conteúdo
  if ((ocorrencias = 1) and (EstaVazio)) then
  begin
    if FOpcoes.FIdentarXML then
    begin
      if FOpcoes.FTagVaziaNoFormatoResumido then
        FArquivoFormatoXML := FArquivoFormatoXML + StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) + '<' + tag + '/>' + #13#10
      else
        FArquivoFormatoXML := FArquivoFormatoXML + StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) + '<' + tag + '></' + tag + '>' + #13#10
    end
    else
    begin
      if FOpcoes.FTagVaziaNoFormatoResumido then
        FArquivoFormatoXML := FArquivoFormatoXML + '<' + tag + Atributo + '/>'
      else
        FArquivoFormatoXML := FArquivoFormatoXML + '<' + tag + Atributo +  '></' + tag + '>';
    end;
    exit;
  end;

  // Grava a tag no arquivo - Quando existir algum conteúdo
  if ((ocorrencias = 1) or (not EstaVazio)) then
  begin
    if ParseTextoXML then
       ATag := '<' + tag + Atributo +  '>' +
               FiltrarTextoXML(FOpcoes.FRetirarEspacos, ConteudoProcessado, FOpcoes.FRetirarAcentos) +
               '</' + tag + '>'
    else
       ATag := '<' + tag + Atributo +  '>' +
               ConteudoProcessado +
               '</' + tag + '>';

    if FOpcoes.FIdentarXML then
      FArquivoFormatoXML := FArquivoFormatoXML +
         StringOfChar(' ', FOpcoes.FTamanhoIdentacao * FOpcoes.FNivelIdentacao) +
         ATag + sLineBreak
    else
      FArquivoFormatoXML := FArquivoFormatoXML + ATag;
  end;
end;

procedure TGerador.wTexto(const Texto: string);
begin
  FArquivoFormatoXML := FArquivoFormatoXML + Texto;
end;

// Gerador TXT

procedure TGerador.gtNivel(ID: string);
var
  i: integer;
begin
  ID := UpperCase(ID);
  FIDNivel := ID;
  if (FLayoutArquivoTXT.Count = 0) or (ID = '') then
    exit;
  for i := 0 to FLayoutArquivoTXT.Count - 1 do
    if pos('<' + ID + '>', UpperCase(FLayoutArquivoTXT.Strings[i])) > 0 then
      FArquivoFormatoTXT := FArquivoFormatoTXT + FLayoutArquivoTXT.Strings[i] + #13;
end;

procedure TGerador.gtCampo(const Tag, ConteudoProcessado: string);
var
  i: integer;
  List: TstringList;
begin
  if FLayoutArquivoTXT.Count = 0 then
    exit;
  List := TStringList.Create;
  List.Text := FArquivoFormatoTXT;
  //
  for i := 0 to List.count - 1 do
    if pos('<' + FIDNivel + '>', List.Strings[i]) > 0 then
      if pos('|' + UpperCase(Tag) + '¨', UpperCase(List.Strings[i])) > 0 then
        List[i] := StringReplace(List[i], '|' + UpperCase(Trim(TAG)) + '¨', '|' + conteudoProcessado, []);
  //
  FArquivoFormatoTXT := List.Text;
  List.Free;
end;

procedure TGerador.gtAjustarRegistros(const ID: string);
var
  i, j, k: integer;
  s, idLocal: string;
  ListArquivo: TstringList;
  ListCorrigido: TstringList;
  ListTAGs: TstringList;
begin
  if FLayoutArquivoTXT.Count = 0 then
    exit;
  ListTAGs := TStringList.Create;
  ListArquivo := TStringList.Create;
  ListCorrigido := TStringList.Create;
  // Elimina registros não utilizados
  ListArquivo.Text := FArquivoFormatoTXT;
  for i := 0 to ListArquivo.count - 1 do
  begin
    k := 0;
    for j := 0 to FLayoutArquivoTXT.count - 1 do
      if listArquivo[i] = FLayoutArquivoTXT[j] then
        if pos('¨', listArquivo[i]) > 0 then
          k := 1;
    if k = 0 then
      ListCorrigido.add(ListArquivo[i]);
  end;
  // Insere dados da chave da Nfe
  for i := 0 to ListCorrigido.count - 1 do
    if pos('^ID^', ListCorrigido[i]) > 1 then
      ListCorrigido[i] := StringReplace(ListCorrigido[i], '^ID^', ID, []);
  // Elimina Nome de TAG sem informação
  for j := 0 to FLayoutArquivoTXT.count - 1 do
  begin
    s := FLayoutArquivoTXT[j];
    while (pos('|', s) > 0) and (pos('¨', s) > 0) do
    begin
      s := copy(s, pos('|', s), maxInt);
      ListTAGs.add(copy(s, 1, pos('¨', s)));
      s := copy(s, pos('¨', s) + 1, maxInt);
    end;
  end;
  for i := 0 to ListCorrigido.count - 1 do
    for j := 0 to ListTAGs.count - 1 do
      ListCorrigido[i] := StringReplace(ListCorrigido[i], ListTAGs[j], '|', []);
  // Elimina Bloco <ID>
  for i := 0 to ListCorrigido.count - 1 do
    if pos('>', ListCorrigido[i]) > 0 then
     begin
      ListCorrigido[i] := Trim(copy(ListCorrigido[i], pos('>', ListCorrigido[i]) + 1, maxInt));
      idLocal := copy(ListCorrigido[i],1,pos('|',ListCorrigido[i])-1);

      if (length(idLocal) > 2) and (UpperCase(idLocal) <> 'NOTA FISCAL') and
         (copy(idLocal,length(idLocal),1) <> SomenteNumeros(copy(idLocal,length(idLocal),1))) then
       begin
         idLocal := copy(idLocal,1,length(idLocal)-1)+LowerCase(copy(idLocal,length(idLocal),1));
         ListCorrigido[i] := StringReplace(ListCorrigido[i],idLocal,idLocal,[rfIgnoreCase]);
       end;
     end;
  FArquivoFormatoTXT := ListCorrigido.Text;
  //
  ListTAGs.Free;
  ListArquivo.Free;
  ListCorrigido.Free;
end;

procedure TGerador.wCampoNFSe(const Tipo: TpcnTipoCampo; ID, TAG: string;
  const min, max, ocorrencias: smallint; const valor: variant;
  const Descricao: string; ParseTextoXML: Boolean; Atributo: String);
begin
  Self.wCampo(Tipo, ID, Self.Prefixo + TAG, min, max, ocorrencias, valor,
              Descricao, ParseTextoXML, Atributo);
end;

procedure TGerador.wGrupoNFSe(const TAG: string; ID: string;
  const Identar: Boolean);
begin
  if copy(TAG, 1, 1) = '/' then
     Self.wGrupo('/' + Self.Prefixo + copy(TAG, 2, length(TAG)), ID, Identar)
  else
     Self.wGrupo(Self.Prefixo + TAG, ID, Identar);
end;

end.

