{******************************************************************************}
{ Projeto: TACBrConvenio115                                                    }
{                                                                              }
{ Função: Gerar arquivo magnético para Notas Fiscais emitidas referentes aos   }
{         modelos abaixo relacionados, conforme estabelecido no Convênio       }
{         ICMS 115/03:                                                         }
{         - Nota Fiscal/Conta de Energia Elétrica, modelo 6;                   }
{         - Nota Fiscal de Serviço de Comunicação, modelo 21;                  }
{         - Nota Fiscal de Serviço de Telecomunicações, modelo 22;             }
{         - Qualquer outro documento fiscal relativo à prestação de serviço de }
{           comunicação ou ao fornecimento de energia elétrica.                }
{                                                                              }
{         As informações do referido convênio pode ser encontrada em:          }
{         https://www.confaz.fazenda.gov.br/legislacao/convenios/2003/cv115_03 }
{                                                                              }
{         Programa validador e demais informações podem ser acessadas em:      }
{         https://portal.fazenda.sp.gov.br/servicos/nf-comunicacao-energia/Paginas/Sobre.aspx                 }
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
{******************************************************************************}

{******************************************************************************}
{ Direitos Autorais Reservados © 2013 - Jéter Rabelo Ferreira                  }
{ Contato: jeter.rabelo@jerasoft.com.br                                        }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 19/02/2013: Jéter Rabelo Ferreira
|*  - Disponibilização do componente para o projeto ACBr
|*
|* 01/02/2017: Jéter Rabelo Ferreira
|*  - Modificação conforme mudança da legislação
|*  https://www.confaz.fazenda.gov.br/legislacao/convenios/2003/cv115_03an0-1.pdf
*******************************************************************************}
{$I ACBr.inc}

unit ACBrConvenio115;

interface

uses
  SysUtils, Classes, Contnrs, ACBrConsts, StrUtils, ACBrBase
  {$IFDEF FPC}
   ,LResources
  {$ENDIF} ;

{$R ACBrConvenio115.dcr}

type
  TTipoAssinanteConv115_Tab11_1 = (
    tac111None,
    tac111ComercialIndustrial,
    tac111PoderPublico,
    tac111ResidencialPessoaFisica,
    tac111Publico,
    tac111SemiPublico,
    tac111Outros);

  TTipoAssinanteConv115_Tab11_8_2 = (
    tac1182None,
    tac11182Comercial,
    tac11182Industrial,
    tac11182PessoaFisicaResidencial,
    tac11182ProdutorRural,
    tac11182AdmPublicaEstadualConvICMS107_95,
    tac11182PrestServicoTelecomConvIcms17_13,
    tac11182MissoesDiplomConvIcms158_94,
    tac11182Igrejas,
    tac11182Outros);

  TProdutoConv115_Tab11_2 = (
    pc112None,
    pc112Telefonia,
    pc112ComunicacaoDados,
    pc112TVAssinatura,
    pc112ProvimentoAcessoInternet,
    pc112Multimidia,
    pc112Outros);

  TSituacaoNFConv115 = (
    stcv115Normal,
    stcv115Cancelado,
    stcv115Substituido,
    stcv115Complementar);

  TTabelaIsencaoReducaoBC = (
    tirNone,
    tirGovernoEletronicoConvIcms141_07,
    tirInternetPopularConvIcms38_09,
    tirInternetEscolaPublicaConvIcms47_08,
    tirAICEConvIcms15_12,
    tirTvAssinaturaConvIcms57_99,
    tirMonitRastreamentoVeicCargaConvIcms139_06,
    tirAcessoInternetConvIcms78_01,
    tirOutras);

  TIndicadorPessoaM = (
    ipmCNPJ,
    ipmCpf,
    ipmJuridicaSemCnpj,
    ipmFisicaSemCpf);


  TStatusArquivoConv115 = (scv115Normal, scv115Substituto);

  TConvenio115AssinaturaMD5 = record
    Registro: string;
    Assinatura: string;
  end;

  TACBrConvenio115Item = class
  private
    /////////////////////////////////////////////////////////
    // Variáveis serão preenchidas pelo conteúdo do MESTRE //
    /////////////////////////////////////////////////////////
    FCnpjCpf: string;
    FUF: string;
    FDataEmissao: TDateTime;
    FModelo: SmallInt;
    FSerie: string;
    FNumeroNF: Integer;
    FReferenciaItemNF: Integer;
    FTipoAssinanteAte201612: TTipoAssinanteConv115_Tab11_1;
    FTipoAssinante: TTipoAssinanteConv115_Tab11_8_2;
    FTipoUtilizacao: TProdutoConv115_Tab11_2;
    /////////////////////////////////////////////////////////
    FCFOP: string;
    FItem: Integer;
    FCodigoServico: string;
    FDescricaoServico: string;
    FClassificacaoItem: string;
    FUnidade: string;
    FQtdeContratada: Currency;
    FQtdePrestada: Currency;
    FValorTotal: Currency;
    FDesconto: Currency;
    FAcrescimosDespAcessorias: Currency;
    FICMSBaseCalculo: Currency;
    FICMSValor: Currency;
    FIsentoNaoTributados: Currency;
    FOutrosValores: Currency;
    FICMSAliquota: Currency;
    FSituacao: TSituacaoNFConv115;
    FAnoMesApuracao: string;
    FNumeroDoContrato: string;
    FQtdeFaturada: Currency;
    FTarifaAplicadaPrecoMedio: Double;
    FAliquotaPis: Currency;
    FValorPis: Currency;
    FAliquotaCofins: Currency;
    FValorCofins: Currency;
    FDescontoJudicial: Boolean;
    FTipoIsencaoReducaoBC: TTabelaIsencaoReducaoBC;
    function GetGrupoTensao: string;
    procedure SetClassificacaoItem(const Value: string);
  public
    /////////////////////////////////////////////////////////////
    // Properties serão preenchidas pelo conteúdo do MESTRE    //
    // As mesmas foram incluídas aqui para que possam ser      //
    // utilizadas, por exemplo, na impressão de uma NF, pos    //
    // os cálculos das assinaturas estão contemplados aqui     //
    //                                                         //
    // OBS.:                                                   //
    // Na geração dos arquivos, o prrenchimento das properties //
    // serão totalmente ignorados                              //
    /////////////////////////////////////////////////////////////
    property CnpjCpf: string read FCnpjCpf write FCnpjCpf;
    property UF: string read FUF write FUF;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property Modelo: SmallInt read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property NumeroNF: Integer read FNumeroNF write FNumeroNF;
    property ReferenciaItemNF: Integer read FReferenciaItemNF write FReferenciaItemNF;
    property TipoAssinanteAte201612: TTipoAssinanteConv115_Tab11_1 read FTipoAssinanteAte201612 write FTipoAssinanteAte201612;
    property TipoAssinante: TTipoAssinanteConv115_Tab11_8_2 read FTipoAssinante write FTipoAssinante;
    property TipoUtilizacao: TProdutoConv115_Tab11_2 read FTipoUtilizacao write FTipoUtilizacao;
    /////////////////////////////////////////////////////////
    property GrupoTensao: string read GetGrupoTensao;
    property CFOP: string read FCFOP write FCFOP;
    property Item: Integer read FItem write FItem;
    property CodigoServico: string read FCodigoServico write FCodigoServico;
    property DescricaoServico: string read FDescricaoServico write FDescricaoServico;
    property ClassificacaoItem: string read FClassificacaoItem write SetClassificacaoItem;
    property Unidade: string read FUnidade write FUnidade;
    property QtdeContratada: Currency read FQtdeContratada write FQtdeContratada;
    property QtdePrestada: Currency read FQtdePrestada write FQtdePrestada;
    property ValorTotal: Currency read FValorTotal write FValorTotal;
    property Desconto: Currency read FDesconto write FDesconto;
    property AcrescimosDespAcessorias: Currency read FAcrescimosDespAcessorias write FAcrescimosDespAcessorias;
    property ICMSBaseCalculo: Currency read FICMSBaseCalculo write FICMSBaseCalculo;
    property ICMSValor: Currency read FICMSValor write FICMSValor;
    property ICMSAliquota: Currency read FICMSAliquota write FICMSAliquota;
    property IsentoNaoTributados: Currency read FIsentoNaoTributados write FIsentoNaoTributados;
    property OutrosValores: Currency read FOutrosValores write FOutrosValores;
    property Situacao: TSituacaoNFConv115 read FSituacao write FSituacao;
    property AnoMesApuracao: string read FAnoMesApuracao write FAnoMesApuracao;
    property NumeroDoContrato: string read FNumeroDoContrato write FNumeroDoContrato;
    property QtdeFaturada: Currency read FQtdeFaturada write FQtdeFaturada;
    property TarifaAplicadaPrecoMedio: Double read FTarifaAplicadaPrecoMedio write FTarifaAplicadaPrecoMedio;
    property AliquotaPis: Currency read FAliquotaPis write FAliquotaPis;
    property ValorPis: Currency read FValorPis write FValorPis;
    property AliquotaCofins: Currency read FAliquotaCofins write FAliquotaCofins;
    property ValorCofins: Currency read FValorCofins write FValorCofins;
    property DescontoJudicial: Boolean read FDescontoJudicial write FDescontoJudicial;
    property TipoIsencaoReducaoBC: TTabelaIsencaoReducaoBC read FTipoIsencaoReducaoBC write FTipoIsencaoReducaoBC;
    function RegistroEAssinatura(AVersaoAnterior: Boolean): TConvenio115AssinaturaMD5;
  end;

  { Lista de objetos do tipo TConvenio115Mestre }
  TACBrConvenio115Items = class(TObjectList)
  protected
    procedure SetObject(Index: Integer; Item: TACBrConvenio115Item);
    function GetObject(Index: Integer): TACBrConvenio115Item;
    procedure Insert(Index: Integer; Obj: TACBrConvenio115Item);
  public
    function Add(Obj: TACBrConvenio115Item): Integer;
    function New: TACBrConvenio115Item;
    property Objects[Index: Integer]: TACBrConvenio115Item read GetObject write SetObject; default;
  end;

  TACBrConvenio115Destinatario = class
  private
    /////////////////////////////////////////////////////////
    // Variáveis serão preenchidas pelo conteúdo do MESTRE //
    /////////////////////////////////////////////////////////
//    FDataEmissao: TDateTime;
//    FModelo: SmallInt;
//    FSerie: string;
//    FNumeroNF: Integer;
    /////////////////////////////////////////////////////////
    FCnpjCpf: string;
    FInscricaoEstadual: string;
    FRazaoSocial: string;
    FLogradouro: string;
    FNumero: string;
    FComplemento: string;
    FUF: string;
    FCep: string;
    FBairro: string;
    FMunicipio: string;
    FTelefone: string;
    FCodigoConsumidor: string;
    FCodigoDoMunicipio: string;
    procedure SetCnpjCpf(const Value: string);
  public
    property CnpjCpf: string read FCnpjCpf write SetCnpjCpf;
    property InscricaoEstadual: string read FInscricaoEstadual write FInscricaoEstadual;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property Logradouro: string read FLogradouro write FLogradouro;
    property Numero: string read FNumero write FNumero;
    property Complemento: string read FComplemento write FComplemento;
    property CEP: string read FCep write FCep;
    property Bairro: string read FBairro write FBairro;
    property Municipio: string read FMunicipio write FMunicipio;
    property UF: string read FUF write FUF;
    property Telefone: string read FTelefone write FTelefone;
    property CodigoConsumidor: string read FCodigoConsumidor write FCodigoConsumidor;
    property CodigoDoMunicipio: string read FCodigoDoMunicipio write FCodigoDoMunicipio;
  end;

  TACBrConvenio115Mestre = class
  private
    /////////////////////////////
    ///  Componente principal ///
    /////////////////////////////
    FModelo: SmallInt;
    FSerie: string;
    FCnpjEmitente: string;
    /////////////////////////////
    FTipoAssinanteAte201612: TTipoAssinanteConv115_Tab11_1;
    FTipoUtilizacao: TProdutoConv115_Tab11_2;
    FDataEmissao: TDateTime;
    FNumeroNF: Integer;
    FValorTotal: Currency;
    FICMS_BaseCalculo: Currency;
    FICMS_Valor: Currency;
    FIsentosNaoTributadas: Currency;
    FOutrosValores: Currency;
    FSituacaoDocumento: TSituacaoNFConv115;
    FAnoMesRefencia: string;
    FNumeroTerminalTelefonico: string;
    FUFTerminalTelefonico: string;
    FDestinatario: TACBrConvenio115Destinatario;
    FDetalhes: TACBrConvenio115Items;
    FReferenciaItemNF: Integer;
    FIndicadorPessoa: TIndicadorPessoaM;
    FTipoAssinante: TTipoAssinanteConv115_Tab11_8_2;
    FNumeroTelefonePrincipal: string;
    FNumeroFaturaComercial: string;
    FValorFaturaComecial: Currency;
    FObservacoes: string;
    function GetGrupoTensao: string;
    procedure SetTipoAssinanteAte201612(const Value: TTipoAssinanteConv115_Tab11_1);
    procedure SetTipoUtilizacao(const Value: TProdutoConv115_Tab11_2);
    procedure SetTipoAssinante(const Value: TTipoAssinanteConv115_Tab11_8_2);
    function GetDataLeituraAnterior: string;
    function GetDataLeituraAtual: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Destinatario: TACBrConvenio115Destinatario read FDestinatario write FDestinatario;
    property TipoAssinanteAte201612: TTipoAssinanteConv115_Tab11_1 read FTipoAssinanteAte201612 write SetTipoAssinanteAte201612;
    property TipoUtilizacao: TProdutoConv115_Tab11_2 read FTipoUtilizacao write SetTipoUtilizacao;
    property GrupoTensao: string read GetGrupoTensao;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;

    property Modelo: SmallInt read FModelo write FModelo;
    property Serie: String read FSerie write FSerie;

    property NumeroNF: Integer read FNumeroNF write FNumeroNF;
    property ValorTotal: Currency read FValorTotal write FValorTotal;
    property ICMS_BaseCalculo: Currency read FICMS_BaseCalculo write FICMS_BaseCalculo;
    property ICMS_Valor: Currency read FICMS_Valor write FICMS_Valor;
    property IsentosNaoTributadas: Currency read FIsentosNaoTributadas write FIsentosNaoTributadas;
    property OutrosValores: Currency read FOutrosValores write FOutrosValores;
    property SituacaoDocumento: TSituacaoNFConv115 read FSituacaoDocumento write FSituacaoDocumento;
    property AnoMesRefencia: string read FAnoMesRefencia write FAnoMesRefencia;
    property ReferenciaItemNF: Integer read FReferenciaItemNF;
    property NumeroTerminalTelefonico: string read FNumeroTerminalTelefonico write FNumeroTerminalTelefonico;
    property UFTerminalTelefonico: string read FUFTerminalTelefonico write FUFTerminalTelefonico;
    property IndicadorPessoa: TIndicadorPessoaM read FIndicadorPessoa write FIndicadorPessoa;
    property TipoAssinante: TTipoAssinanteConv115_Tab11_8_2 read FTipoAssinante write SetTipoAssinante;
    property NumeroTelefonePrincipal: string read FNumeroTelefonePrincipal write FNumeroTelefonePrincipal;
    property NumeroFaturaComercial: string read FNumeroFaturaComercial write FNumeroFaturaComercial;
    property ValorFaturaComecial: Currency read FValorFaturaComecial write FValorFaturaComecial;
    property DataLeituraAnterior: string read GetDataLeituraAnterior;
    property DataLeituraAtual: string read GetDataLeituraAtual;
    property Observacoes: string read FObservacoes write FObservacoes;
    property Detalhes: TACBrConvenio115Items read FDetalhes;
    function AutenticacaoDocumentoFiscal: string;
    function RegistroEAssinatura(AVersaoAnterior: Boolean): TConvenio115AssinaturaMD5;
    class function MontaAutenticacaoDocumentoFiscal(const ACnpjCpf: string;
                                                    ANumeroNF: Integer;
                                                    AValorTotal: Double;
                                                    AIcmsBaseCalculo: Double;
                                                    AIcmsValor: Double;
                                                    ADataEmissao: TDateTime;
                                                    const ACnpjEmitente: string): string;
  end;

  { Lista de objetos do tipo TConvenio115Mestre }
  TACBrConvenio115Mestres = class(TObjectList)
  protected
    procedure SetObject(Index: Integer; Item: TACBrConvenio115Mestre);
    function GetObject(Index: Integer): TACBrConvenio115Mestre;
    procedure Insert(Index: Integer; Obj: TACBrConvenio115Mestre);
  public
    function Add(Obj: TACBrConvenio115Mestre): Integer;
    function New: TACBrConvenio115Mestre;
    property Objects[Index: Integer]: TACBrConvenio115Mestre read GetObject write SetObject; default;
  end;

  TACBrConvenio115Responsavel = class(TPersistent)
  private
    FEMail: string;
    FCargo: string;
    FResponsavel: string;
  public
    constructor Create;
  published
    property Nome: string read FResponsavel write FResponsavel;
    property Cargo: string read FCargo write FCargo;
    property EMail: string read FEMail write FEMail;
  end;

  TConvenio115TipoArquivo = (c115taMestre, c115taitem, c115taDestinatario);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrConvenio115 = class(TACBrComponent)
  private
    FSalvarEm: string;
    FMestre: TACBrConvenio115Mestres;
    FUF: string;
    FSerie: string;
    FAno: SmallInt;
    FMes: SmallInt;
    FStatus: TStatusArquivoConv115;
    FResponsavel: TACBrConvenio115Responsavel;
    FOrdernar: Boolean;
    FCnpjEmitente: string;
    FModelo: SmallInt;
    FNumeroArquivoSubstituido: SmallInt;
    FNomeArquivoMestre: string;
    function GetVersao: string;
    procedure SetSalvarEm(const Value: string);
    procedure DoGerarMestre;
    procedure DoGerarItem;
    procedure DoGerarDestinatario;
    function DoNomeArquivo(TipoArquivo: TConvenio115TipoArquivo): string;
    function _VersaoAntiga: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Gerar;
    property Mestre: TACBrConvenio115Mestres read FMestre;
    property NomeArquivoMestre: string read FNomeArquivoMestre write FNomeArquivoMestre;
  published
    property Versao: string read GetVersao;
    property SalvarEm: string read FSalvarEm write SetSalvarEm;
    property UF: string read FUF write FUF;
    property Modelo: SmallInt read FModelo write FModelo;
    property Serie: string read FSerie write FSerie;
    property Ano: SmallInt read FAno write FAno;
    property Mes: SmallInt read FMes write FMes;
    property CnpjEmitente: string read FCnpjEmitente write FCnpjEmitente;
    property Status: TStatusArquivoConv115 read FStatus write FStatus;
    property Responsavel: TACBrConvenio115Responsavel read FResponsavel write FResponsavel;
    property NumeroArquivoSubstituido: SmallInt read FNumeroArquivoSubstituido write FNumeroArquivoSubstituido;
    property Ordernar: Boolean read FOrdernar write FOrdernar;
  end;

function SortMestre(Item1: Pointer; Item2: Pointer): Integer;

procedure Register;

const
  TTipoAssinanteConv115_Tab11_1DS: array [0 .. 6] of string =
    ('Sem Classificação',
    'Comercial / Industrial',
    'Poder Público',
    'Residencial / Pessoa Física',
    'Público',
    'Semi-Público',
    'Outros');

  TTipoAssinanteConv115_Tab11_8_2DS: array [0 .. 9] of string =
    ('Sem Classificação',
    'Comercial',
    'Industrial',
    'Residencial / Pessoa Física',
    'Produto Rural',
    'Orgão público estadual (Conv.ICMS 107/95)',
    'Prest.Serviço Telecom. responsábel recol. Imposto (Conv.ICMS 17/13)',
    'Missões diplomáticas, e afins (Conv.ICMS 158/94)',
    'Igrejas e Tempos de qualquer natureza',
    'Outros');

  TProdutoConv115_Tab11_2DS: array [0 .. 6] of string =
    ('Sem Classificação',
    'Telefonia',
    'Comunicação de dados',
    'TV por Assinatura',
    'Provimento de acesso à Internet',
    'Multimídia',
    'Outros');

  TSituacaoNFConv115DS: array [0 .. 2] of string =
    ('Normal',
    'Cancelado',
    'Substituído');

  TSituacaoNFConv115ID: array [0 .. 3] of string =
    ('N',
    'S',
    'R',
    'C');

implementation

uses
  ACBrUtil, ACBrEAD, Variants, ACBrValidador;

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrConvenio115]);
end;

function MD5String(const texto: string): string;
var
  MD5: TACBrEAD;
begin
  MD5 := TACBrEAD.Create(nil);
  try
    Result := UpperCase(MD5.MD5FromString(texto));
  finally
    FreeAndNil(MD5);
  end;
end;

{ TConvenio115Mestre }

constructor TACBrConvenio115Mestre.Create;
begin
  inherited;
  FDestinatario := TACBrConvenio115Destinatario.Create;
  FDetalhes := TACBrConvenio115Items.Create;
end;

destructor TACBrConvenio115Mestre.Destroy;
begin
  FreeAndNil(FDetalhes);
  FreeAndNil(FDestinatario);
  inherited;
end;

function TACBrConvenio115Mestre.AutenticacaoDocumentoFiscal: string;
begin
  Result := MontaAutenticacaoDocumentoFiscal(Destinatario.CnpjCpf,
                                             NumeroNF,
                                             ValorTotal,
                                             ICMS_BaseCalculo,
                                             ICMS_Valor,
                                             DataEmissao,
                                             FCnpjEmitente);
end;

function TACBrConvenio115Mestre.GetDataLeituraAnterior: string;
begin
  // Somente energia elétrica
  Result := '00000000';
end;

function TACBrConvenio115Mestre.GetDataLeituraAtual: string;
begin
  // Somente energia elétrica
  Result := '00000000';
end;

function TACBrConvenio115Mestre.GetGrupoTensao: string;
begin
  Result := '00';
end;

class function TACBrConvenio115Mestre.MontaAutenticacaoDocumentoFiscal(const ACnpjCpf: string;
                                                                       ANumeroNF: Integer;
                                                                       AValorTotal: Double;
                                                                       AIcmsBaseCalculo: Double;
                                                                       AIcmsValor: Double;
                                                                       ADataEmissao: TDateTime;
                                                                       const ACnpjEmitente: string): string;
var
  SRec: string;
begin
  SRec := PadLeft(OnlyNumber(ACnpjCpf), 14, '0') +                                  { 01 - CNPJ/CPF }
          PadLeft(IntToStr(aNumeroNF), 9, '0') +                                    { 12 - Numero NF }
          PadLeft(TiraPontos(FormatFloat('#,##0.00', AValorTotal)), 12, '0') +      { 14 - Valor }
          PadLeft(TiraPontos(FormatFloat('#,##0.00', AIcmsBaseCalculo)), 12, '0') + { 15 - Base ICMS }
          PadLeft(TiraPontos(FormatFloat('#,##0.00', AIcmsValor)), 12, '0');        { 16 - Valor ICMS }
  if ADataEmissao >= EncodeDate(2017,1,1) then
    SRec := SRec +
            DtOs(ADataEmissao)                                                      { 09 - Data da Emissão } +
            PadLeft(OnlyNumber(ACnpjEmitente), 14, '0');                            { 27 - CNPJ/CPF }
  Result := MD5String(SRec);
end;

function TACBrConvenio115Mestre.RegistroEAssinatura(AVersaoAnterior: Boolean): TConvenio115AssinaturaMD5;
  function _GetTab11_8_2: string;
  begin
    case TipoAssinante of
      tac11182Outros: Result := '99';
      else
        Result := PadLeft(IntToStr(Ord(TipoAssinante)), 2, '0');
    end;
  end;
var
  SRec: string;
begin
  SRec := {01} PadLeft(OnlyNumber(Destinatario.CnpjCpf), 14, '0') +
          {02} PadRight(IfThen(OnlyNumber(Destinatario.InscricaoEstadual) = '', 'ISENTO', OnlyNumber(Destinatario.InscricaoEstadual)), 14) +
          {03} PadRight(TiraAcentos(Destinatario.RazaoSocial), 35) +
          {04} PadRight(UpperCase(Destinatario.UF), 2) +
          {05} IfThen(AVersaoAnterior, IntToStr(Ord(TipoAssinanteAte201612)), '0') +
          {06} IntToStr(Ord(TipoUtilizacao)) +
          {07} GrupoTensao +
          {08} PadRight(Destinatario.CodigoConsumidor, 12) +
          {09} DtoS(DataEmissao) +
          {10} PadLeft(IntToStr(FModelo), 2, '0') +
          {11} PadRight(FSerie, 3) +
          {12} PadLeft(IntToStr(NumeroNF), 9, '0') +
          {13} AutenticacaoDocumentoFiscal;
          //{14} PadLeft(TiraPontos(FormatFloat('#,##0.00', ValorTotal)), 12, '0') +
          //{15} PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMS_BaseCalculo)), 12, '0') +
          //{16} PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMS_Valor)), 12, '0') +
          //{17} PadLeft(TiraPontos(FormatFloat('#,##0.00', IsentosNaoTributadas)), 12, '0') +
          //{18} PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 12, '0') +

  if ValorTotal < 0 then
    SRec := SRec + '-' +
           {14} PadLeft(TiraPontos(FormatFloat('#,##0.00', ValorTotal)), 11, '0')
  else
    SRec := SRec +
            {14} PadLeft(TiraPontos(FormatFloat('#,##0.00', ValorTotal)), 12, '0');

  if ICMS_BaseCalculo < 0 then
    SRec := SRec +
           {15}  '-' + PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMS_BaseCalculo)), 11, '0') +
           {16}  '-' + PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMS_Valor)), 11, '0')
  else
    SRec := SRec +
            {15} PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMS_BaseCalculo)), 12, '0') +
            {16} PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMS_Valor)), 12, '0');

  if IsentosNaoTributadas < 0 then
    SRec := SRec + '-' +
           {17} PadLeft(TiraPontos(FormatFloat('#,##0.00', IsentosNaoTributadas)), 11, '0')
  else
    SRec := SRec +
            {17} PadLeft(TiraPontos(FormatFloat('#,##0.00', IsentosNaoTributadas)), 12, '0');

  if OutrosValores < 0 then
    SRec := SRec + '-' +
           {18} PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 11, '0')
  else
    SRec := SRec +
            {18} PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 12, '0');

  SRec := SRec +
          {19} TSituacaoNFConv115ID[Ord(SituacaoDocumento)] +
          {20} AnoMesRefencia +
          {21} PadLeft(IntToStr(ReferenciaItemNF), 9, '0') +
          {22} PadRight(NumeroTerminalTelefonico, 12);
  if AVersaoAnterior then
  begin
    SRec := SRec +
          {23} PadRight('', 5);
  end
  else
  begin
    SRec := SRec +
          {23} IntToStr(Ord(IndicadorPessoa) + 1) +
          {24} _GetTab11_8_2 +
          {25} '00' + // Telecomunicação é 00
          {26} PadLeft(NumeroTelefonePrincipal, 12) +
          {27} PadLeft(OnlyNumber(FCnpjEmitente), 14, '0') +
          {28} PadRight(NumeroFaturaComercial, 20) +
          {29} PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 12, '0') +
          {30} DataLeituraAnterior +
          {31} DataLeituraAtual +
          {32} PadRight('', 50) +
          {33} PadRight('', 8, '0') +
          {34} PadRight(Observacoes, 30) +
          {33} PadRight('', 5);
  end;
  Result.Registro := SRec;
  Result.Assinatura := MD5String(Result.Registro);
end;

procedure TACBrConvenio115Mestre.SetTipoAssinante(
  const Value: TTipoAssinanteConv115_Tab11_8_2);
begin
  if Value = tac1182None then
    raise Exception.Create('Tipo de assinante inválido!');
  FTipoAssinante := Value;
end;

procedure TACBrConvenio115Mestre.SetTipoAssinanteAte201612(
  const Value: TTipoAssinanteConv115_Tab11_1);
begin
  if Value = tac111None then
    raise Exception.Create('Tipo de assinante inválido!');
  FTipoAssinanteAte201612 := Value;
end;

procedure TACBrConvenio115Mestre.SetTipoUtilizacao(
  const Value: TProdutoConv115_Tab11_2);
begin
  if Value = pc112None then
    raise Exception.Create('Tipo de utilização inválido!');
  FTipoUtilizacao := Value;
end;

{ TConvenio115Mestres }

function TACBrConvenio115Mestres.Add(Obj: TACBrConvenio115Mestre): Integer;
begin
  Result := inherited Add(Obj);
end;

function TACBrConvenio115Mestres.New: TACBrConvenio115Mestre;
begin
  Result := TACBrConvenio115Mestre.Create;
  Add(Result);
end;

function TACBrConvenio115Mestres.GetObject(Index: Integer): TACBrConvenio115Mestre;
begin
  Result := inherited GetItem(Index) as TACBrConvenio115Mestre;
end;

procedure TACBrConvenio115Mestres.Insert(Index: Integer; Obj: TACBrConvenio115Mestre);
begin
  inherited SetItem(Index, Obj);
end;

procedure TACBrConvenio115Mestres.SetObject(Index: Integer;
  Item: TACBrConvenio115Mestre);
begin
  inherited SetItem(Index, Item);
end;

function SortMestre(Item1, Item2: Pointer): Integer;
var
  OItem1, OItem2: TACBrConvenio115Mestre;
begin
  OItem1 := TACBrConvenio115Mestre(Item1);
  OItem2 := TACBrConvenio115Mestre(Item2);
  if (OItem1.NumeroNF > OItem2.NumeroNF) then
    Result := 1
  else if (OItem1.NumeroNF = OItem2.NumeroNF) then
    Result := 0
  else
    Result := -1;
end;

{ TConvenio115 }

procedure TACBrConvenio115.Clear;
begin
  FMestre.Clear;
end;

constructor TACBrConvenio115.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMestre := TACBrConvenio115Mestres.Create;
  FResponsavel := TACBrConvenio115Responsavel.Create();
  FNumeroArquivoSubstituido := 1;
  Ordernar := False;
end;

destructor TACBrConvenio115.Destroy;
begin
  Clear;
  FResponsavel.Free;
  FMestre.Free;
  inherited;
end;

procedure TACBrConvenio115.DoGerarDestinatario;
var
  OStr: TStringList;
  I: Integer;
  RRegistro: TConvenio115AssinaturaMD5;
  SRec: AnsiString;
begin
  OStr := TStringList.Create;
  try
    for I := 0 to FMestre.Count - 1 do
    begin
      SRec := {01} PadLeft(OnlyNumber(FMestre[I].Destinatario.CnpjCpf), 14, '0') +
              {02} PadRight(IfThen(OnlyNumber(FMestre[I].Destinatario.InscricaoEstadual) = '', 'ISENTO', OnlyNumber(FMestre[I].Destinatario.InscricaoEstadual)), 14) +
              {03} PadRight(TiraAcentos(FMestre[I].Destinatario.RazaoSocial), 35) +
              {04} PadRight(TiraAcentos(FMestre[I].Destinatario.Logradouro), 45) +
              {05} PadLeft(OnlyNumber(FMestre[I].Destinatario.Numero), 5, '0') +
              {06} PadRight(TiraAcentos(FMestre[I].Destinatario.Complemento), 15) +
              {07} PadLeft(OnlyNumber(FMestre[I].Destinatario.CEP), 8, '0') +
              {08} PadRight(TiraAcentos(FMestre[I].Destinatario.Bairro), 15) +
              {09} PadRight(FMestre[I].Destinatario.Municipio, 30) +
              {10} PadRight(UpperCase(FMestre[I].Destinatario.UF), 2);
      if _VersaoAntiga then
      begin
        SRec := SRec +
              {11} PadRight(OnlyNumber(FMestre[I].Destinatario.Telefone), 12, '0');
      end
      else
      begin
        SRec := SRec +
              {11} PadRight(OnlyNumber(FMestre[I].Destinatario.Telefone), 12, ' ');
      end;
      SRec := SRec +
              {12} PadRight(FMestre[I].Destinatario.CodigoConsumidor, 12) +
              {13} PadRight(FMestre[I].NumeroTerminalTelefonico, 12) +
              {14} PadRight(UpperCase(FMestre[I].UFTerminalTelefonico), 2);
      if _VersaoAntiga then
      begin
        SRec := SRec +
              {15} PadRight('', 5);
      end
      else
      begin
        SRec := SRec +
              {15} DtOS(FMestre[I].DataEmissao) +
              {16} PadLeft(IntToStr(Modelo), 2, '0') +
              {17} PadRight(Serie, 3) +
              {18} PadLeft(IntToStr(FMestre[I].NumeroNF), 9, '0') +
              {19} FMestre[I].Destinatario.CodigoDoMunicipio +
              {20} PadRight('', 5);
      end;
      RRegistro.Registro := String(SRec);
      RRegistro.Assinatura := MD5String(RRegistro.Registro);
      OStr.Add(RRegistro.Registro + RRegistro.Assinatura);
    end;
    OStr.SaveToFile(DoNomeArquivo(c115taDestinatario));
  finally
    FreeAndNil(OStr);
  end;
end;

procedure TACBrConvenio115.DoGerarItem;
var
  I: Integer;
  A: Integer;
  OStr: TStringList;
begin
  OStr := TStringList.Create;
  try
    for I := 0 to FMestre.Count - 1 do
    begin
      for A := 0 to FMestre[I].Detalhes.Count - 1 do
      begin
        OStr.Add(FMestre[I].Detalhes[A].RegistroEAssinatura(_VersaoAntiga).Registro +
                 FMestre[I].Detalhes[A].RegistroEAssinatura(_VersaoAntiga).Assinatura);
      end;
    end;
    OStr.SaveToFile(DoNomeArquivo(c115taitem));
  finally
    OStr.Free;
  end;
end;

procedure TACBrConvenio115.DoGerarMestre;
var
  I: Integer;
  OStr: TStringList;
  lNomeArquivo: string;
begin
  if Ordernar then
    Mestre.Sort(SortMestre);

  OStr := TStringList.Create;
  try
    lNomeArquivo := DoNomeArquivo(c115taMestre);
    for I := 0 to FMestre.Count - 1 do
    begin
      OStr.Add(FMestre[I].RegistroEAssinatura(_VersaoAntiga).Registro +
               FMestre[I].RegistroEAssinatura(_VersaoAntiga).Assinatura);
    end;
    OStr.SaveToFile(lNomeArquivo);
  finally
    OStr.Free;
  end;
end;

function TACBrConvenio115.DoNomeArquivo(TipoArquivo: TConvenio115TipoArquivo): string;
begin
  if _VersaoAntiga then
  begin
    { Composição do nome do arquivo:
      Nome do Arquivo                         |   |  Extensão
      U F | S S S | A A | M M |   ST   |  T   | . | V V V
      UF  | série | ano | mês | Status | tipo | . | volume
    }
    Result := FUF +
              PadLeft(Serie, 3, '0') +
              Copy(IntToStr(Ano), 3, 2) +
              PadLeft(IntToStr(Mes), 2, '0') +
              IfThen(Status = scv115Normal, 'N', 'S');
  end
  else
  begin
    { Composição do nome do arquivo:
      Nome do Arquivo                         |   |  Extensão
      U F | CCCCCCCCCCCCCC |   MM   | S S S | A A | M M |   ST   |  T   | . | V V V
      UF  |      CNPJ      | Modelo | série | ano | mês | Status | tipo | . | volume
    }
    Result := FUF +
              PadLeft(OnlyNumber(CnpjEmitente), 14, '0') +
              PadLeft(IntToStr(Modelo), 2, '0') +
              PadRight(Serie, 3) +
              Copy(IntToStr(Ano), 3, 2) +
              PadLeft(IntToStr(Mes), 2, '0') +
              IfThen(Status = scv115Normal, 'N', 'S') +
              PadLeft(IntToStr(NumeroArquivoSubstituido), 2, '0');
  end;

  case TipoArquivo of
    c115taMestre: Result := Result + 'M';
    c115taitem: Result := Result + 'I';
    c115taDestinatario:  Result := Result + 'D';
  end;

  if TipoArquivo =  c115taMestre then
    FNomeArquivoMestre := Result + '.001';

  Result := SalvarEm + Result + '.001'; // Pode ter 1 milhão de registros
end;

procedure TACBrConvenio115.Gerar;
var
  I: Integer;
  A: Integer;
  ICount: Integer;
begin
  if FSalvarEm = '' then
    raise Exception.Create('Pasta não informada para gravar os arquivos!');

  if FMestre.Count = 0 then
    raise Exception.Create('Nenhum registro Mestre informado!');

  for I := 0 to FMestre.Count - 1 do
  begin
    if FMestre[I].Detalhes.Count = 0 then
      raise Exception.Create('Detalhe não informado para a Nota Fiscal: ' + IntToStr(FMestre[I].NumeroNF) +
                             ' - Cliente: ' + FMestre[I].Destinatario.CodigoConsumidor + '/' + FMestre[I].Destinatario.RazaoSocial);

    if Ano < 2017 then
    begin
      if FMestre[I].TipoAssinanteAte201612 = tac111None then
        raise Exception.Create('Tipo de assinante inválido para a Nota Fiscal: ' + IntToStr(FMestre[I].NumeroNF) +
                               ' - Cliente: ' + FMestre[I].Destinatario.CodigoConsumidor + '/' + FMestre[I].Destinatario.RazaoSocial);
    end
    else
    begin
      if FMestre[I].TipoAssinante = tac1182None then
        raise Exception.Create('Tipo de assinante inválido para a Nota Fiscal: ' + IntToStr(FMestre[I].NumeroNF) +
                               ' - Cliente: ' + FMestre[I].Destinatario.CodigoConsumidor + '/' + FMestre[I].Destinatario.RazaoSocial);
    end;

    if FMestre[I].TipoUtilizacao = pc112None then
      raise Exception.Create('Tipo de utilização inválido para a Nota Fiscal: ' + IntToStr(FMestre[I].NumeroNF) +
                             ' - Cliente: ' + FMestre[I].Destinatario.CodigoConsumidor + '/' + FMestre[I].Destinatario.RazaoSocial);
  end;

  // Contador para fazer a referência do Ítem Mestre->Detalhe
  ICount := 1;

  // Atribuir os valores que são iguais na classe Detalhe
  for I := 0 to FMestre.Count - 1 do
  begin
    FMestre[I].FModelo := Modelo;
    FMestre[I].FSerie := Serie;
    FMestre[I].FCnpjEmitente := CnpjEmitente;
    FMestre[I].FReferenciaItemNF := ICount;
    for A := 0 to FMestre[I].Detalhes.Count -1 do
    begin
      FMestre[I].Detalhes[A].FTipoAssinanteAte201612 := FMestre[I].TipoAssinanteAte201612;
      FMestre[I].Detalhes[A].FTipoUtilizacao := FMestre[I].TipoUtilizacao;
      FMestre[I].Detalhes[A].FCnpjCpf := FMestre[I].Destinatario.CnpjCpf;
      FMestre[I].Detalhes[A].FUF := FMestre[I].Destinatario.UF;
      FMestre[I].Detalhes[A].FDataEmissao := FMestre[I].DataEmissao;
      FMestre[I].Detalhes[A].FModelo := FModelo;
      FMestre[I].Detalhes[A].FSerie := Serie;
      FMestre[I].Detalhes[A].FNumeroNF := FMestre[I].NumeroNF;
      Inc(ICount);
    end;
  end;

  DoGerarMestre;
  DoGerarItem;
  DoGerarDestinatario;
end;

function TACBrConvenio115.GetVersao: string;
begin
  Result := '0.1.0.0';
end;

procedure TACBrConvenio115.SetSalvarEm(const Value: string);
begin
  FSalvarEm := IncludeTrailingPathDelimiter(Value);
end;

function TACBrConvenio115._VersaoAntiga: Boolean;
begin
  Result := IntToStr(Ano) + PadLeft(IntToStr(Mes), 2, '0') < '201701';
end;

{ TConvenio115Responsavel }

constructor TACBrConvenio115Responsavel.Create;
begin
  inherited Create;
end;

{ TConvenio115Detalhe }

function TACBrConvenio115Item.GetGrupoTensao: string;
begin
  Result := '00';
end;

function TACBrConvenio115Item.RegistroEAssinatura(AVersaoAnterior: Boolean): TConvenio115AssinaturaMD5;
  function _GetTipoRedBC: string;
  begin
    case TipoIsencaoReducaoBC of
      tirOutras: Result := '99';
    else
      Result := PadLeft(IntToStr(Ord(TipoIsencaoReducaoBC)), 2, '0');
    end;
  end;
var
  SRec: string;
begin
  SRec := {01} PadLeft(OnlyNumber(FCnpjCpf), 14, '0') +
          {02} PadRight(UpperCase(FUF), 2) +
          {03} IfThen(AVersaoAnterior, IntToStr(Ord(FTipoAssinanteAte201612)), '0') +
          {04} IntToStr(Ord(FTipoUtilizacao)) +
          {05} GrupoTensao +
          {06} DtoS(FDataEmissao) +
          {07} PadLeft(IntToStr(FModelo), 2, '0') +
          {08} PadRight(FSerie, 3) +
          {09} PadLeft(IntToStr(FNumeroNF), 9, '0') +
          {10} PadLeft(CFOP, 4, '0') +
          {11} PadLeft(IntToStr(Item), 3, '0') +
          {12} PadRight(CodigoServico, 10) +
          {13} PadRight(TiraAcentos(DescricaoServico), 40) +
          {14} PadLeft(ClassificacaoItem, 4, '0') +
          {15} PadRight(Unidade, 6);
  if AVersaoAnterior then
    SRec := SRec +
          {16} PadLeft(TiraPontos(FormatFloat('#,##0.000', QtdeContratada)), 11, '0') +
          {17} PadLeft(TiraPontos(FormatFloat('#,##0.000', QtdePrestada)), 11, '0')
  else
    SRec := SRec +
          {16} PadLeft(TiraPontos(FormatFloat('#,##0.000', QtdeContratada)), 12, '0') +
          {17} PadLeft(TiraPontos(FormatFloat('#,##0.000', QtdePrestada)), 12, '0');

    if ValorTotal < 0 then
      SRec := SRec + '-' +
        {18} PadLeft(TiraPontos(FormatFloat('#,##0.00', ValorTotal)), 10, '0')
    else
      SRec := SRec +
            {18} PadLeft(TiraPontos(FormatFloat('#,##0.00', ValorTotal)), 11, '0');


    SRec := SRec +
          {19} PadLeft(TiraPontos(FormatFloat('#,##0.00', Desconto)), 11, '0') +
          {20} PadLeft(TiraPontos(FormatFloat('#,##0.00', AcrescimosDespAcessorias)), 11, '0');

    if ICMSBaseCalculo < 0 then
      begin
        SRec := SRec +
          {21} '-' + PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMSBaseCalculo)), 10, '0') +
          {22} '-' + PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMSValor)), 10, '0');
         // {23} '-' + PadLeft(TiraPontos(FormatFloat('#,##0.00', IsentoNaoTributados)), 10, '0') +
         // {24} '-' + PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 10, '0');
      end
    else
      begin
        SRec := SRec +
          {21} PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMSBaseCalculo)), 11, '0') +
          {22} PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMSValor)), 11, '0');
         // {23} PadLeft(TiraPontos(FormatFloat('#,##0.00', IsentoNaoTributados)), 11, '0') +
         // {24} PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 11, '0');
      end;

    if IsentoNaoTributados < 0 then
      SRec := SRec + '-' +
        {23} PadLeft(TiraPontos(FormatFloat('#,##0.00', IsentoNaoTributados)), 10, '0')
    else
      SRec := SRec +
            {23} PadLeft(TiraPontos(FormatFloat('#,##0.00', IsentoNaoTributados)), 11, '0');

    if OutrosValores < 0 then
      SRec := SRec + '-' +
        {24} PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 10, '0')
    else
      SRec := SRec +
            {24} PadLeft(TiraPontos(FormatFloat('#,##0.00', OutrosValores)), 11, '0');

    SRec := SRec +
          {25} PadLeft(TiraPontos(FormatFloat('#,##0.00', ICMSAliquota)), 4, '0') +
          {26} TSituacaoNFConv115ID[Ord(Situacao)] +
          {27} AnoMesApuracao;
  if AVersaoAnterior then
    SRec := SRec +
          {28} PadLeft('', 5)
  else
  begin
    SRec := SRec +
          {28} PadLeft(FNumeroDoContrato, 15) +
          {29} PadLeft(TiraPontos(FormatFloat('#,##0.000', QtdeFaturada)), 12, '0') +
          {30} PadLeft(TiraPontos(FormatFloat('#,##0.000000', TarifaAplicadaPrecoMedio)), 11, '0') +
          {31} PadLeft(TiraPontos(FormatFloat('#,##0.0000', AliquotaPis)), 6, '0') +
          {32} PadLeft(TiraPontos(FormatFloat('#,##0.0000', ValorPis)), 11, '0') +
          {33} PadLeft(TiraPontos(FormatFloat('#,##0.0000', AliquotaCofins)), 6, '0') +
          {34} PadLeft(TiraPontos(FormatFloat('#,##0.0000', ValorCofins)), 11, '0') +
          {35} IfThen(DescontoJudicial, 'J', ' ') +
          {36} _GetTipoRedBC +
          {37} PadLeft('', 5);
  end;
  Result.Registro := SRec;
  Result.Assinatura := MD5String(Result.Registro);
end;

procedure TACBrConvenio115Item.SetClassificacaoItem(const Value: string);
  function StrInVarArray(Str: string; lista: Variant): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if VarIsArray(lista) then
    begin
      for I := 0 to VarArrayHighBound(lista, 1) do
        if lista[I] = Str then
        begin
          Result := True;
          Break;
        end;
    end
    else if not VarIsNull(lista) then
    begin
      if VarIsStr(lista) then
        Result := lista = Str;
    end;
  end;

begin
  FClassificacaoItem := '';
  if not StrInVarArray(Value, VarArrayOf([
    // Assinatura
    '0101', '0102', '0103', '0104', '0105', '0199',
    // Habilitação
    '0201', '0202', '0203', '0204', '0205', '0299',
    // Serviço Medido
    '0301', '0302', '0303', '0304', '0305', '0306', '0307', '0308', '0309', '0310', '0311', '0312', '0313', '0314', '0315', '0399',
    // Serviço Pré-pago
    '0401', '0402', '0403', '0404', '0405', '0406', '0407', '0499',
    // Outros serviços
    '0501', '0502', '0599',
    // Energia Elétrica
    '0601', '0602', '0603', '0604', '0605', '0606', '0607', '0608', '0609', '0610', '0699',
    // Disponibilização de meios ou equipamento
    '0701', '0702', '0703', '0704', '0705', '0706', '0707', '0708', '0709', '0799',
    // Cobranças
    '0801', '0802', '0803', '0804', '0805', '0806', '0807', '0808', '0899',
    // Deduções
    '0901', '0902', '0903', '0904', '0905', '0906', '0907', '0999',
    // Serviço não medido
    '1001', '1002', '1003', '1004', '1005', '1099',
    // Cessão de Meios de Rede
    '1101', '1102', '1103', '1104', '1105', '1106', '1107', '1199'
    ])) then
    raise Exception.Create('Classificação do ítem inválida');
  FClassificacaoItem := Value;
end;

{ TConvenio115Detalhes }

function TACBrConvenio115Items.Add(Obj: TACBrConvenio115Item): Integer;
begin
  Result := inherited Add(Obj);
end;

function TACBrConvenio115Items.New: TACBrConvenio115Item;
begin
  Result := TACBrConvenio115Item.Create;
  Add(Result);
end;

function TACBrConvenio115Items.GetObject(Index: Integer): TACBrConvenio115Item;
begin
  Result := inherited GetItem(Index) as TACBrConvenio115Item;
end;

procedure TACBrConvenio115Items.Insert(Index: Integer; Obj: TACBrConvenio115Item);
begin
  inherited SetItem(Index, Obj);
end;

procedure TACBrConvenio115Items.SetObject(Index: Integer;
  Item: TACBrConvenio115Item);
begin
  inherited SetItem(Index, Item);
end;

{ TConvenio115Destinatario }

procedure TACBrConvenio115Destinatario.SetCnpjCpf(const Value: string);
var
  OValidador: TACBrValidador;
begin
  OValidador := TACBrValidador.Create(nil);
  try
    with OValidador do
    begin
      if Length(OnlyNumber(Value)) <= 11 then
        TipoDocto := docCPF
      else
        TipoDocto := docCNPJ;
      Documento := Value;
      if not Validar then
        raise Exception.Create('CNPJ/CPF: ' + Value + ' inválido');
      FCnpjCpf := Value;
    end;
  finally
    OValidador.Free;
  end;
end;

initialization
{$IFDEF FPC}
   {$I ACBrConvenio115.lrs}
{$ENDIF}
end.
