{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

unit ACBrIMendes;

interface

uses
  Classes, SysUtils, Contnrs,
  ACBrAPIBase, ACBrJSON, ACBrBase, ACBrSocket,
  ACBrUtil.Base;

const
  cIMendesURLProducao = '';
  cIMendesURLHomologacao = 'http://consultatributos.com.br:8080';
  cIMendesAPIV1 = 'api/v1/public';
  cIMendesAPIRegimeEspecial = 'regime_especial';
  cIMendesEndPointLogin = 'api/auth';
  cIMendesEndpointCadCliente = 'CadCliente';
  cIMendesEndpointEnviaRecebeDados = 'EnviaRecebeDados';
  cIMendesEndpointEnviaRegimeEspecial = 'SearchSpecialRegime';
  cIMendesDadosServico = 'dados';
  cIMendesNomeServico = 'nomeServico';
  cIMendesServicoDescricaoProdutos = 'DESCRPRODUTOS';

type    

  EACBrIMendesAuthError = class(Exception);
  EACBrIMendesDataSend = class(Exception);

  TACBrIMendesAmbiente = (
    imaNenhum,
    imaHomologacao,
    imaProducao);

  { TACBrIMendesCabecalho }
  TACBrIMendesCabecalho = class(TACBrAPISchema)
  private
    fUF: String;
    fCNPJ: String;
    fProdutosRetornados: Integer;
    fMensagem: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    procedure Assign(Source: TACBrIMendesCabecalho);
    function IsEmpty: Boolean; override;
    
    property UF: String read fUF write fUF;
    property CNPJ: String read fCNPJ write fCNPJ;
    property ProdutosRetornados: Integer read fProdutosRetornados write fProdutosRetornados;
    property Mensagem: String read fMensagem write fMensagem;
  end;

  { TACBrIMendesProduto }
  TACBrIMendesProduto = class(TACBrAPISchema)
  private
    fId: String;
    fDescricao: String;
    fEan: String;
    fNcm: String;
    fCest: String;
    fDescricaoGrupo: String;
    fCodigo: String;
    fCodInterno: String;
    fCodImendes: String;
    fImportado: String;
    fEncontrado: Boolean;
    fTipo: Integer;
    fChaveRetorno: String;
    fDtUltCons: TDateTime;
    fDtRev: TDateTime;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    procedure Assign(Source: TACBrIMendesProduto);
    function IsEmpty: Boolean; override;

    property Id: String read fId write fId;
    property Descricao: String read fDescricao write fDescricao;
    property Ean: String read fEan write fEan;
    property Ncm: String read fNcm write fNcm;
    property Cest: String read fCest write fCest;
    property DescricaoGrupo: String read fDescricaoGrupo write fDescricaoGrupo;
    property Codigo: String read fCodigo write fCodigo;
    property CodInterno: String read fCodInterno write fCodInterno;
    property CodImendes: String read fCodImendes write fCodImendes;
    property Importado: String read fImportado write fImportado;
    property Encontrado: Boolean read fEncontrado write fEncontrado;
    property Tipo: Integer read fTipo write fTipo;
    property ChaveRetorno: String read fChaveRetorno write fChaveRetorno;
    property DtUltCons: TDateTime read fDtUltCons write fDtUltCons;
    property DtRev: TDateTime read fDtRev write fDtRev;
  end;

  { TACBrIMendesProdutos }
  TACBrIMendesProdutos = class(TACBrAPISchemaArray)
  private
    function GetItem(AIndex: Integer): TACBrIMendesProduto;
    procedure SetItem(AIndex: Integer; AValue: TACBrIMendesProduto);
  public
    function New: TACBrIMendesProduto;
    function NewSchema: TACBrAPISchema; override;
    function Add(AProduto: TACBrIMendesProduto): Integer;
    procedure Insert(AIndex: Integer; AProduto: TACBrIMendesProduto);
    property Items[AIndex: Integer]: TACBrIMendesProduto read GetItem write SetItem; default;
  end;

  { TACBrIMendesConsultarResponse }
  TACBrIMendesConsultarResponse = class(TACBrAPISchema)
  private
    fCabecalho: TACBrIMendesCabecalho;
    fProdutos: TACBrIMendesProdutos;
    function GetCabecalho: TACBrIMendesCabecalho;
    function GetProdutos: TACBrIMendesProdutos;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrIMendesConsultarResponse);
    function IsEmpty: Boolean; override;

    property Cabecalho: TACBrIMendesCabecalho read GetCabecalho;
    property Produtos: TACBrIMendesProdutos read GetProdutos;
  end;

  { TACBrIMendesPortal }
  TACBrIMendesPortal = class(TACBrAPISchema)
  private
    fUserID: Integer;
    fMethod: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    procedure Assign(Source: TACBrIMendesPortal);
    function IsEmpty: Boolean; override;

    property UserID: Integer read fUserID write fUserID;
    property Method: String read fMethod write fMethod;
  end;

  { TACBrIMendesEmitente }
  TACBrIMendesEmitente = class(TACBrAPISchema)
  private
    famb: Integer;
    fcnpj: String;
    fcrt: Integer;
    fregimeTrib: String;
    fuf: String;
    fcnae: String;
    fsubstICMS: Boolean;
    finterdependente: Boolean;
    fcnaeSecundario: String;
    fdia: Integer;
    fmes: Integer;
    fano: Integer;
    fdataLimite: TDateTime;
    fportal: TACBrIMendesPortal;
    fregimeEspecial: String;
    function GetPortal: TACBrIMendesPortal;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrIMendesEmitente);

    property amb: Integer read famb write famb;
    property cnpj: String read fcnpj write fcnpj;
    property crt: Integer read fcrt write fcrt;
    property regimeTrib: String read fregimeTrib write fregimeTrib;
    property uf: String read fuf write fuf;
    property cnae: String read fcnae write fcnae;
    property substICMS: Boolean read fsubstICMS write fsubstICMS;
    property interdependente: Boolean read finterdependente write finterdependente;
    property cnaeSecundario: String read fcnaeSecundario write fcnaeSecundario;
    property dia: Integer read fdia write fdia;
    property mes: Integer read fmes write fmes;
    property ano: Integer read fano write fano;
    property dataLimite: TDateTime read fdataLimite write fdataLimite;
    property portal: TACBrIMendesPortal read GetPortal;
    property regimeEspecial: String read fregimeEspecial write fregimeEspecial;
  end;

  { TACBrIMendesPerfil }
  TACBrIMendesPerfil = class(TACBrAPISchema)
  private
    fuf: TStringList;
    //fcaracTrib: TACBrJsonArray;
    fcfop: String;
    ffinalidade: Integer;
    fsimplesN: String;
    forigem: Integer;
    fsubstICMS_Perfil: String;
    fregimeTrib: String;
    fprodZFM: String;
    fregimeEspecial: String;
    ffabricacaoPropria: Boolean;
    function GetCaracTrib: TACBrJsonArray;
    function GetUF: TStringList;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrIMendesPerfil);

    property uf: TStringList read GetUF;
    property cfop: String read fcfop write fcfop;
    property caracTrib: TACBrJsonArray read GetCaracTrib;
    property finalidade: Integer read ffinalidade write ffinalidade;
    property simplesN: String read fsimplesN write fsimplesN;
    property origem: Integer read forigem write forigem;
    property substICMS: String read fsubstICMS_Perfil write fsubstICMS_Perfil;
    property regimeTrib: String read fregimeTrib write fregimeTrib;
    property prodZFM: String read fprodZFM write fprodZFM;
    property regimeEspecial: String read fregimeEspecial write fregimeEspecial;
    property fabricacaoPropria: Boolean read ffabricacaoPropria write ffabricacaoPropria;
  end;

  { TACBrIMendesGradesRequest }
  TACBrIMendesGradesRequest = class(TACBrAPISchema)
  private
    femit: TACBrIMendesEmitente;
    fperfil: TACBrIMendesPerfil;
    fprodutos: TACBrIMendesProdutos;
    function GetEmit: TACBrIMendesEmitente;
    function GetPerfil: TACBrIMendesPerfil;
    function GetProdutos: TACBrIMendesProdutos;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrIMendesGradesRequest);
    function IsEmpty: Boolean; override;

    property emit: TACBrIMendesEmitente read GetEmit;
    property perfil: TACBrIMendesPerfil read GetPerfil;
    property produtos: TACBrIMendesProdutos read GetProdutos;
  end;

  { TACBrIMendesResumo }
  TACBrIMendesResumo = class(TACBrAPISchema)
  private
    fDataPrimeiroConsumo: TDateTime;
    fDataUltimoConsumo: TDateTime;
    fProdutosPendentes_Interno: Integer;
    fProdutosPendentes_EAN: Integer;
    fProdutosPendentes_Devolvidos: Integer;
    fProdutosPendentes_DataInicio: TDateTime;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrIMendesResumo);

    property DataPrimeiroConsumo: TDateTime read fDataPrimeiroConsumo write fDataPrimeiroConsumo;
    property DataUltimoConsumo: TDateTime read fDataUltimoConsumo write fDataUltimoConsumo;
    property ProdutosPendentes_Interno: Integer read fProdutosPendentes_Interno write fProdutosPendentes_Interno;
    property ProdutosPendentes_EAN: Integer read fProdutosPendentes_EAN write fProdutosPendentes_EAN;
    property ProdutosPendentes_Devolvidos: Integer read fProdutosPendentes_Devolvidos write fProdutosPendentes_Devolvidos;
    property ProdutosPendentes_DataInicio: TDateTime read fProdutosPendentes_DataInicio write fProdutosPendentes_DataInicio;
  end;

  { TACBrIMendesHistoricoResponse }
  TACBrIMendesHistoricoResponse = class(TACBrAPISchema)
  private
    fResumo: TACBrIMendesResumo;
    fProdDevolvidos: TACBrIMendesProdutos;
    function GetResumo: TACBrIMendesResumo;
    function GetProdDevolvidos: TACBrIMendesProdutos;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrIMendesHistoricoResponse);
    function IsEmpty: Boolean; override;

    property Resumo: TACBrIMendesResumo read GetResumo;
    property ProdDevolvidos: TACBrIMendesProdutos read GetProdDevolvidos;
  end;

  { TACBrIMendesRevenda }
  TACBrIMendesRevenda = class(TACBrAPISchema)
  private
    fCNPJCPF: String;
    fNome: String;
    fFone: String;
    fEmail: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrIMendesRevenda);

    property CNPJCPF: String read fCNPJCPF write fCNPJCPF;
    property Nome: String read fNome write fNome;
    property Fone: String read fFone write fFone;
    property Email: String read fEmail write fEmail;
  end;

  { TACBrIMendesCliente }
  TACBrIMendesCliente = class(TACBrAPISchema)
  private
    fCNPJCPF: String;
    fRazaoSocial: String;
    fEndereco: String;
    fNro: String;
    fBairro: String;
    fCidade: String;
    fUF: String;
    fCEP: String;
    fFone: String;
    fResponsavel: String;
    fEmail: String;
    fNroCNPJ: Integer;
    fValorImplantacao: Double;
    fValorMensalidade: Double;
    fStatus: String;
    fRegimeTrib: String;
    fTipoAtiv: String;
    fObservacao: String;
    fRevenda: TACBrIMendesRevenda;
    function GetRevenda: TACBrIMendesRevenda;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrIMendesCliente);

    property CNPJCPF: String read fCNPJCPF write fCNPJCPF;
    property RazaoSocial: String read fRazaoSocial write fRazaoSocial;
    property Endereco: String read fEndereco write fEndereco;
    property Nro: String read fNro write fNro;
    property Bairro: String read fBairro write fBairro;
    property Cidade: String read fCidade write fCidade;
    property UF: String read fUF write fUF;
    property CEP: String read fCEP write fCEP;
    property Fone: String read fFone write fFone;
    property Responsavel: String read fResponsavel write fResponsavel;
    property Email: String read fEmail write fEmail;
    property NroCNPJ: Integer read fNroCNPJ write fNroCNPJ;
    property ValorImplantacao: Double read fValorImplantacao write fValorImplantacao;
    property ValorMensalidade: Double read fValorMensalidade write fValorMensalidade;
    property Status: String read fStatus write fStatus;
    property RegimeTrib: String read fRegimeTrib write fRegimeTrib;
    property TipoAtiv: String read fTipoAtiv write fTipoAtiv;
    property Observacao: String read fObservacao write fObservacao;
    property Revenda: TACBrIMendesRevenda read GetRevenda;
  end;

  { TACBrIMendesErro }
  TACBrIMendesErro = class(TACBrAPISchema)
  private
    fSucesso: Boolean;
    fMensagem: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrIMendesErro);

    property Sucesso: Boolean read fSucesso write fSucesso;
    property Mensagem: String read fMensagem write fMensagem;
  end;

  { TACBrIMendesRegimeEspecial }
  TACBrIMendesRegimeEspecial = class(TACBrAPISchema)
  private
    fCode: String;
    fDescription: String;
    fLegalBasis: String;
    fInitialValidity: TDateTime;
    fFinalValidity: TDateTime;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrIMendesRegimeEspecial);

    property Code: String read fCode write fCode;
    property Description: String read fDescription write fDescription;
    property LegalBasis: String read fLegalBasis write fLegalBasis;
    property InitialValidity: TDateTime read fInitialValidity write fInitialValidity;
    property FinalValidity: TDateTime read fFinalValidity write fFinalValidity;
  end;

  { TACBrIMendesRegimeEspecialList }
  TACBrIMendesRegimeEspecialList = class(TACBrAPISchemaArray)
  private
    function GetItem(AIndex: Integer): TACBrIMendesRegimeEspecial;
    procedure SetItem(AIndex: Integer; AValue: TACBrIMendesRegimeEspecial);
  public
    function New: TACBrIMendesRegimeEspecial;
    function NewSchema: TACBrAPISchema; override;
    function Add(ARegimeEspecial: TACBrIMendesRegimeEspecial): Integer;
    procedure Insert(AIndex: Integer; ARegimeEspecial: TACBrIMendesRegimeEspecial);
    property Items[AIndex: Integer]: TACBrIMendesRegimeEspecial read GetItem write SetItem; default;
  end;

  { TACBrIMendes }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrIMendes = class(TACBrHTTP)
  private
    fKey: String;
    fCNPJ: String;
    fToken: String;
    fSenha: AnsiString;
    fAmbiente: TACBrIMendesAmbiente;
    fRespostaErro: TACBrIMendesErro;
    fConsultarDescricaoResponse: TACBrIMendesConsultarResponse;
    function GetConsultarDescricaoResponse: TACBrIMendesConsultarResponse;
    function GetRespostaErro: TACBrIMendesErro;
    function GetSenha: AnsiString;
    function CalcularURL: String;
    procedure SetSenha(AValue: AnsiString);
    procedure ValidarConfiguracao;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Autenticar;

    function ConsultarDescricao(const aDescricao: String; const aCNPJ: String = ''): Boolean;
    function ConsultarAlterados(const aUF: String; const aCNPJ: String = ''): Boolean;
    function ConsultarRegimesEspeciais(const aUF: String): Boolean;
    function HistoricoAcesso: Boolean;

    property ConsultarDescricaoResponse: TACBrIMendesConsultarResponse read GetConsultarDescricaoResponse;
    property RespostaErro: TACBrIMendesErro read GetRespostaErro;
  published
    property Ambiente: TACBrIMendesAmbiente read fAmbiente write fAmbiente;

    property CNPJ: String read fCNPJ write fCNPJ;
    property Senha: AnsiString read GetSenha write SetSenha;
  end;

implementation

uses
  StrUtils, synautil,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TACBrIMendesCabecalho }

constructor TACBrIMendesCabecalho.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrIMendesCabecalho.Clear;
begin
  fUF := EmptyStr;
  fCNPJ := EmptyStr;
  fProdutosRetornados := 0;
  fMensagem := EmptyStr;
end;

procedure TACBrIMendesCabecalho.Assign(Source: TACBrIMendesCabecalho);
begin
  fUF := Source.UF;
  fCNPJ := Source.CNPJ;
  fProdutosRetornados := Source.ProdutosRetornados;
  fMensagem := Source.Mensagem;
end;

function TACBrIMendesCabecalho.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fUF) and
    EstaVazio(fCNPJ) and
    EstaZerado(fProdutosRetornados) and
    EstaVazio(fMensagem);
end;

procedure TACBrIMendesCabecalho.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('UF', fUF)
    .Value('CNPJ', fCNPJ)
    .Value('produtosRetornados', fProdutosRetornados)
    .Value('mensagem', fMensagem);
end;

procedure TACBrIMendesCabecalho.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesCabecalho) then
    Assign(TACBrIMendesCabecalho(aSource));
end;

procedure TACBrIMendesCabecalho.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('UF', fUF, False)
    .AddPair('CNPJ', fCNPJ)
    .AddPair('produtosRetornados', fProdutosRetornados)
    .AddPair('mensagem', fMensagem);
end;

{ TACBrIMendesProduto }

constructor TACBrIMendesProduto.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrIMendesProduto.Clear;
begin
  fId := EmptyStr;
  fDescricao := EmptyStr;
  fEan := EmptyStr;
  fNcm := EmptyStr;
  fCest := EmptyStr;
  fDescricaoGrupo := EmptyStr;
  fCodigo := EmptyStr;
  fCodInterno := EmptyStr;
  fCodImendes := EmptyStr;
  fImportado := EmptyStr;
  fEncontrado := False;
  fTipo := 0;
  fChaveRetorno := EmptyStr;
  fDtUltCons := 0;
  fDtRev := 0;
end;

procedure TACBrIMendesProduto.Assign(Source: TACBrIMendesProduto);
begin
  fId := Source.Id;
  fDescricao := Source.Descricao;
  fEan := Source.Ean;
  fNcm := Source.Ncm;
  fCest := Source.Cest;
  fDescricaoGrupo := Source.DescricaoGrupo;
  fCodigo := Source.Codigo;
  fCodInterno := Source.CodInterno;
  fCodImendes := Source.CodImendes;
  fImportado := Source.Importado;
  fEncontrado := Source.Encontrado;
  fTipo := Source.Tipo;
  fChaveRetorno := Source.ChaveRetorno;
  fDtUltCons := Source.DtUltCons;
  fDtRev := Source.DtRev;
end;

function TACBrIMendesProduto.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fId) and
    EstaVazio(fDescricao) and
    EstaVazio(fEan) and
    EstaVazio(fNcm) and
    EstaVazio(fCest) and
    EstaVazio(fDescricaoGrupo) and
    EstaVazio(fCodigo) and
    EstaVazio(fCodInterno) and
    EstaVazio(fCodImendes) and
    EstaVazio(fImportado) and
    (not fEncontrado) and
    EstaZerado(fTipo) and
    EstaVazio(fChaveRetorno) and
    EstaZerado(fDtUltCons) and
    EstaZerado(fDtRev);
end;

procedure TACBrIMendesProduto.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2: String;
begin
  {$IfDef FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  {$EndIf}

  aJSon
    .Value('id', fId)
    .Value('descricao', fDescricao)
    .Value('ean', fEan)
    .Value('ncm', fNcm)
    .Value('cest', fCest)
    .Value('descricaoGrupo', fDescricaoGrupo)
    .Value('codigo', fCodigo)
    .Value('codInterno', fCodInterno)
    .Value('codImendes', fCodImendes)
    .Value('importado', fImportado)
    .Value('encontrado', fEncontrado)
    .Value('tipo', fTipo)
    .Value('chave_retorno', fChaveRetorno)
    .Value('dtultcons', s1)
    .Value('dtrev', s2);
  if NaoEstaVazio(s1) then
    fDtUltCons := StringToDateTimeDef(s1, 0, 'YYYY-MM-DD');
  if NaoEstaVazio(s2) then
    fDtRev := StringToDateTimeDef(s2, 0, 'YYYY-MM-DD');
end;

procedure TACBrIMendesProduto.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('id', fId, False)
    .AddPair('descricao', fDescricao)
    .AddPair('ean', fEan, False)
    .AddPair('ncm', fNcm)
    .AddPair('cest', fCest, False)
    .AddPair('descricaoGrupo', fDescricaoGrupo, False)
    .AddPair('codigo', fCodigo)
    .AddPair('codInterno', fCodInterno)
    .AddPair('codImendes', fCodImendes, False)
    .AddPair('importado', fImportado, False)
    .AddPair('encontrado', fEncontrado)
    .AddPair('tipo', fTipo, False)
    .AddPair('chave_retorno', fChaveRetorno, False)
    .AddPair('dtultcons', FormatDateBr(fDtUltCons, 'YYYY-MM-DD'))
    .AddPair('dtrev', FormatDateBr(fDtRev, 'YYYY-MM-DD'));
end;

procedure TACBrIMendesProduto.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesProduto) then
    Assign(TACBrIMendesProduto(aSource));
end;

{ TACBrIMendesProdutos }

function TACBrIMendesProdutos.New: TACBrIMendesProduto;
begin
  Result := TACBrIMendesProduto.Create;
  Add(Result);
end;

function TACBrIMendesProdutos.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrIMendesProdutos.Add(AProduto: TACBrIMendesProduto): Integer;
begin
  Result := inherited Add(AProduto);
end;

procedure TACBrIMendesProdutos.Insert(AIndex: Integer; AProduto: TACBrIMendesProduto);
begin
  inherited Insert(AIndex, AProduto);
end;

function TACBrIMendesProdutos.GetItem(AIndex: Integer): TACBrIMendesProduto;
begin
  Result := TACBrIMendesProduto(inherited Items[AIndex]);
end;

procedure TACBrIMendesProdutos.SetItem(AIndex: Integer; AValue: TACBrIMendesProduto);
begin
  inherited Items[AIndex] := AValue;
end;

{ TACBrIMendesConsultarResponse }

constructor TACBrIMendesConsultarResponse.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrIMendesConsultarResponse.Destroy;
begin
  if Assigned(fCabecalho) then
    fCabecalho.Free;
  if Assigned(fProdutos) then
    fProdutos.Free;
  inherited Destroy;
end;

procedure TACBrIMendesConsultarResponse.Clear;
begin
  if Assigned(fCabecalho) then
    fCabecalho.Clear;
  if Assigned(fProdutos) then
    fProdutos.Clear;
end;

function TACBrIMendesConsultarResponse.IsEmpty: Boolean;
begin
  Result :=
    (not Assigned(fCabecalho) or fCabecalho.IsEmpty) and
    (not Assigned(fProdutos) or fProdutos.IsEmpty);
end;

function TACBrIMendesConsultarResponse.GetCabecalho: TACBrIMendesCabecalho;
begin
  if not Assigned(fCabecalho) then
    fCabecalho := TACBrIMendesCabecalho.Create('cabecalho');
  Result := fCabecalho;
end;

function TACBrIMendesConsultarResponse.GetProdutos: TACBrIMendesProdutos;
begin
  if not Assigned(fProdutos) then
    fProdutos := TACBrIMendesProdutos.Create('produto');
  Result := fProdutos;
end;

procedure TACBrIMendesConsultarResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  Cabecalho.ReadFromJSon(aJSon);
  Produtos.ReadFromJSon(aJSon);
end;

procedure TACBrIMendesConsultarResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(fCabecalho) then
    Cabecalho.WriteToJSon(aJSon);
  if Assigned(fProdutos) then
    Produtos.WriteToJSon(aJSon);
end;

procedure TACBrIMendesConsultarResponse.Assign(Source: TACBrIMendesConsultarResponse);
begin
  if Assigned(Source.Cabecalho) then
    Cabecalho.Assign(Source.Cabecalho);
  if Assigned(Source.Produtos) then
    Produtos.Assign(Source.Produtos);
end;

procedure TACBrIMendesConsultarResponse.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesConsultarResponse) then
    Assign(TACBrIMendesConsultarResponse(aSource));
end;

{ TACBrIMendesPortal }

constructor TACBrIMendesPortal.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrIMendesPortal.Clear;
begin
  fUserID := 0;
  fMethod := EmptyStr;
end;

function TACBrIMendesPortal.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fUserID) and
    EstaVazio(fMethod);
end;

procedure TACBrIMendesPortal.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('userID', fUserID)
    .Value('method', fMethod);
end;

procedure TACBrIMendesPortal.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('userID', fUserID)
    .AddPair('method', fMethod);
end;

procedure TACBrIMendesPortal.Assign(Source: TACBrIMendesPortal);
begin
  fUserID := Source.UserID;
  fMethod := Source.Method;
end;

procedure TACBrIMendesPortal.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesPortal) then
    Assign(TACBrIMendesPortal(aSource));
end;

{ TACBrIMendesEmitente }

constructor TACBrIMendesEmitente.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrIMendesEmitente.Destroy;
begin
  if Assigned(fportal) then
    fportal.Free;
  inherited Destroy;
end;

procedure TACBrIMendesEmitente.Clear;
begin
  famb := 0;
  fcnpj := EmptyStr;
  fcrt := 0;
  fregimeTrib := EmptyStr;
  fuf := EmptyStr;
  fcnae := EmptyStr;
  fsubstICMS := False;
  finterdependente := False;
  fcnaeSecundario := EmptyStr;
  fdia := 0;
  fmes := 0;
  fano := 0;
  fdataLimite := 0;
  fregimeEspecial := EmptyStr;
  if Assigned(fportal) then
    fportal.Clear;
end;

function TACBrIMendesEmitente.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(famb) and
    EstaVazio(fcnpj) and
    EstaZerado(fcrt) and
    EstaVazio(fregimeTrib) and
    EstaVazio(fuf) and
    EstaVazio(fcnae) and
    (not fsubstICMS) and
    (not finterdependente) and
    EstaVazio(fcnaeSecundario) and
    EstaZerado(fdia) and
    EstaZerado(fmes) and
    EstaZerado(fano) and
    EstaZerado(fdataLimite) and
    EstaVazio(fregimeEspecial) and
    (not Assigned(fportal) or fportal.IsEmpty);
end;

function TACBrIMendesEmitente.GetPortal: TACBrIMendesPortal;
begin
  if not Assigned(fportal) then
    fportal := TACBrIMendesPortal.Create('portal');
  Result := fportal;
end;

procedure TACBrIMendesEmitente.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2, s3: String;
begin
  {$IfDef FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  s3 := EmptyStr;
  {$EndIf}
  aJSon
    .Value('amb', famb)
    .Value('cnpj', fcnpj)
    .Value('crt', fcrt)
    .Value('regimeTrib', fregimeTrib)
    .Value('uf', fuf)
    .Value('cnae', fcnae)
    .Value('substICMS', s1)
    .Value('interdependente', s2)
    .Value('cnaeSecundario', fcnaeSecundario)
    .Value('dia', fdia)
    .Value('mes', fmes)
    .Value('ano', fano)
    .Value('dataLimite', s3)
    .Value('regimeEspecial', fregimeEspecial);
  fsubstICMS := (s1 = 'S');
  finterdependente := (s2 = 'S');
  if NaoEstaVazio(s3) then
    fdataLimite := StringToDateTimeDef(s3, 0, 'YYYY-MM-DD');

  Portal.ReadFromJSon(aJSon);
end;

procedure TACBrIMendesEmitente.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('amb', famb)
    .AddPair('cnpj', fcnpj)
    .AddPair('crt', fcrt)
    .AddPair('regimeTrib', fregimeTrib)
    .AddPair('uf', fuf)
    .AddPair('cnae', fcnae)
    .AddPair('substICMS', IfThen(fsubstICMS, 'S', 'N'))
    .AddPair('interdependente', IfThen(finterdependente, 'S', 'N'))
    .AddPair('cnaeSecundario', fcnaeSecundario)
    .AddPair('dia', fdia)
    .AddPair('mes', fmes)
    .AddPair('ano', fano)
    .AddPair('dataLimite', FormatDateBr(fdataLimite, 'YYYY-MM-DD'))
    .AddPair('regimeEspecial', fregimeEspecial);

  if Assigned(fportal) and (not fportal.IsEmpty) then
    fportal.WriteToJSon(aJSon);
end;

procedure TACBrIMendesEmitente.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesEmitente) then
    Assign(TACBrIMendesEmitente(aSource));
end;

procedure TACBrIMendesEmitente.Assign(Source: TACBrIMendesEmitente);
begin
  famb := Source.amb;
  fcnpj := Source.cnpj;
  fcrt := Source.crt;
  fregimeTrib := Source.regimeTrib;
  fuf := Source.uf;
  fcnae := Source.cnae;
  fsubstICMS := Source.substICMS;
  finterdependente := Source.interdependente;
  fcnaeSecundario := Source.cnaeSecundario;
  fdia := Source.dia;
  fmes := Source.mes;
  fano := Source.ano;
  fdataLimite := Source.dataLimite;
  fregimeEspecial := Source.regimeEspecial;
  if Assigned(Source.portal) then
    Portal.Assign(Source.portal);
end;

{ TACBrIMendesPerfil }

constructor TACBrIMendesPerfil.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrIMendesPerfil.Destroy;
begin
  if Assigned(fuf) then
    fuf.Free;
  //if Assigned(fcaracTrib) then
  //  fcaracTrib.Free;
  inherited Destroy;
end;

procedure TACBrIMendesPerfil.Clear;
begin
  fuf.Clear;
  fcfop := EmptyStr;
  //fcaracTrib.Clear;
  ffinalidade := 0;
  fsimplesN := EmptyStr;
  forigem := 0;
  fsubstICMS_Perfil := EmptyStr;
  fregimeTrib := EmptyStr;
  fprodZFM := EmptyStr;
  fregimeEspecial := EmptyStr;
  ffabricacaoPropria := False;
end;

function TACBrIMendesPerfil.IsEmpty: Boolean;
begin
  Result :=
    //((not Assigned(fcaracTrib)) or EstaZerado(fcaracTrib.Count)) and
    ((not Assigned(fuf)) or EstaZerado(fuf.Count)) and
    EstaVazio(fcfop) and
    EstaZerado(ffinalidade) and
    EstaVazio(fsimplesN) and
    EstaZerado(forigem) and
    EstaVazio(fsubstICMS_Perfil) and
    EstaVazio(fregimeTrib) and
    EstaVazio(fprodZFM) and
    EstaVazio(fregimeEspecial) and
    (not ffabricacaoPropria);
end;

function TACBrIMendesPerfil.GetUF: TStringList;
begin
  if (not Assigned(fuf)) then
    fuf := TStringList.Create;
  Result := fuf;
end;

function TACBrIMendesPerfil.GetCaracTrib: TACBrJsonArray;
begin
  Result := nil
{  if (not Assigned(fcaracTrib)) then
    fcaracTrib := TACBrJsonArray.Create('caracTrib');
  Result := fcaracTrib;}
end;

procedure TACBrIMendesPerfil.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
  ja: TACBrJSONArray;
begin
  Clear;
  aJSon
    .Value('cfop', fcfop)
    .Value('finalidade', ffinalidade)
    .Value('simplesN', fsimplesN)
    .Value('origem', forigem)
    .Value('substICMS', fsubstICMS_Perfil)
    .Value('regimeTrib', fregimeTrib)
    .Value('prodZFM', fprodZFM)
    .Value('regimeEspecial', fregimeEspecial)
    .Value('fabricacaoPropria', ffabricacaoPropria);

  if aJSon.IsJSONArray('uf') then
  begin
    ja := aJSon.AsJSONArray['uf'];
    for i := 0 to ja.Count - 1 do
      fuf.Add(ja.Items[i]);
  end;
end;

procedure TACBrIMendesPerfil.DoWriteToJSon(aJSon: TACBrJSONObject);
var
  ja: TACBrJSONArray;
  i: Integer;
begin
  inherited DoWriteToJSon(aJSon);

  aJSon
    .AddPair('cfop', fcfop)
    .AddPair('finalidade', ffinalidade)
    .AddPair('simplesN', fsimplesN)
    .AddPair('origem', forigem)
    .AddPair('substICMS', fsubstICMS_Perfil)
    .AddPair('regimeTrib', fregimeTrib)
    .AddPair('prodZFM', fprodZFM)
    .AddPair('regimeEspecial', fregimeEspecial)
    .AddPair('fabricacaoPropria', ffabricacaoPropria);

  if NaoEstaZerado(fuf.Count) then
  begin
    ja := TACBrJSONArray.Create;
    try
      for i := 0 to fuf.Count - 1 do
        ja.AddElement(fuf[i]);
      aJSon.AddPair('uf', ja);
    finally
      ja.Free;
    end;
  end;
end;

procedure TACBrIMendesPerfil.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesPerfil) then
    Assign(TACBrIMendesPerfil(aSource));
end;

procedure TACBrIMendesPerfil.Assign(Source: TACBrIMendesPerfil);
begin
  fuf.Assign(Source.uf);
  fcfop := Source.cfop;
  ffinalidade := Source.finalidade;
  fsimplesN := Source.simplesN;
  forigem := Source.origem;
  fsubstICMS_Perfil := Source.substICMS;
  fregimeTrib := Source.regimeTrib;
  fprodZFM := Source.prodZFM;
  fregimeEspecial := Source.regimeEspecial;
  ffabricacaoPropria := Source.fabricacaoPropria;
end;

{ TACBrIMendesGradesRequest }

constructor TACBrIMendesGradesRequest.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrIMendesGradesRequest.Destroy;
begin
  if Assigned(femit) then
    femit.Free;
  if Assigned(fperfil) then
    fperfil.Free;
  if Assigned(fprodutos) then
    fprodutos.Free;
  inherited Destroy;
end;

procedure TACBrIMendesGradesRequest.Clear;
begin
  if Assigned(femit) then
    femit.Clear;
  if Assigned(fperfil) then
    fperfil.Clear;
  if Assigned(fprodutos) then
    fprodutos.Clear;
end;

function TACBrIMendesGradesRequest.IsEmpty: Boolean;
begin
  Result :=
    (not Assigned(femit) or femit.IsEmpty) and
    (not Assigned(fperfil) or fperfil.IsEmpty) and
    (not Assigned(fprodutos) or fprodutos.IsEmpty);
end;

function TACBrIMendesGradesRequest.GetEmit: TACBrIMendesEmitente;
begin
  if not Assigned(femit) then
    femit := TACBrIMendesEmitente.Create('emit');
  Result := femit;
end;

function TACBrIMendesGradesRequest.GetPerfil: TACBrIMendesPerfil;
begin
  if not Assigned(fperfil) then
    fperfil := TACBrIMendesPerfil.Create('perfil');
  Result := fperfil;
end;

function TACBrIMendesGradesRequest.GetProdutos: TACBrIMendesProdutos;
begin
  if not Assigned(fprodutos) then
    fprodutos := TACBrIMendesProdutos.Create('produtos');
  Result := fprodutos;
end;

procedure TACBrIMendesGradesRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  emit.ReadFromJSon(aJSon);
  perfil.ReadFromJSon(aJSon);
  produtos.ReadFromJSon(aJSon);
end;

procedure TACBrIMendesGradesRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(femit) and (not femit.IsEmpty) then
    femit.WriteToJSon(aJSon);
  if Assigned(fperfil) and (not fperfil.IsEmpty) then
    fperfil.WriteToJSon(aJSon);
  if Assigned(fprodutos) and (not fprodutos.IsEmpty) then
    fprodutos.WriteToJSon(aJSon);
end;

procedure TACBrIMendesGradesRequest.Assign(Source: TACBrIMendesGradesRequest);
begin
  if Assigned(Source.emit) then
    emit.Assign(Source.emit);
  if Assigned(Source.perfil) then
    perfil.Assign(Source.perfil);
  if Assigned(Source.produtos) then
    produtos.Assign(Source.produtos);
end;

procedure TACBrIMendesGradesRequest.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesGradesRequest) then
    Assign(TACBrIMendesGradesRequest(aSource));
end;

{ TACBrIMendesResumo }

constructor TACBrIMendesResumo.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrIMendesResumo.Clear;
begin
  fDataPrimeiroConsumo := 0;
  fDataUltimoConsumo := 0;
  fProdutosPendentes_Interno := 0;
  fProdutosPendentes_EAN := 0;
  fProdutosPendentes_Devolvidos := 0;
  fProdutosPendentes_DataInicio := 0;
end;

function TACBrIMendesResumo.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fDataPrimeiroConsumo) and
    EstaZerado(fDataUltimoConsumo) and
    EstaZerado(fProdutosPendentes_Interno) and
    EstaZerado(fProdutosPendentes_EAN) and
    EstaZerado(fProdutosPendentes_Devolvidos) and
    EstaZerado(fProdutosPendentes_DataInicio);
end;

procedure TACBrIMendesResumo.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2, s3: String;
begin
  {$IfDef FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  s3 := EmptyStr;
  {$EndIf}

  aJSon
    .Value('dataPrimeiroConsumo', s1)
    .Value('dataUltimoConsumo', s2)
    .Value('produtosPendentes_Interno', fProdutosPendentes_Interno)
    .Value('produtosPendentes_EAN', fProdutosPendentes_EAN)
    .Value('produtosPendentes_Devolvidos', fProdutosPendentes_Devolvidos)
    .Value('produtosPendentes_DataInicio', s3);
  if NaoEstaVazio(s1) then
    fDataPrimeiroConsumo := StringToDateTimeDef(s1, 0, 'YYYY-MM-DD');
  if NaoEstaVazio(s2) then
    fDataUltimoConsumo := StringToDateTimeDef(s2, 0, 'YYYY-MM-DD');
  if NaoEstaVazio(s3) then
    fProdutosPendentes_DataInicio := StringToDateTimeDef(s3, 0, 'YYYY-MM-DD');
end;

procedure TACBrIMendesResumo.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('dataPrimeiroConsumo', FormatDateBr(fDataPrimeiroConsumo, 'YYYY-MM-DD'), False)
    .AddPair('dataUltimoConsumo', FormatDateBr(fDataUltimoConsumo, 'YYYY-MM-DD'), False)
    .AddPair('produtosPendentes_Interno', fProdutosPendentes_Interno)
    .AddPair('produtosPendentes_EAN', fProdutosPendentes_EAN)
    .AddPair('produtosPendentes_Devolvidos', fProdutosPendentes_Devolvidos)
    .AddPair('produtosPendentes_DataInicio', FormatDateBr(fProdutosPendentes_DataInicio, 'YYYY-MM-DD'), False);
end;

procedure TACBrIMendesResumo.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesResumo) then
    Assign(TACBrIMendesResumo(aSource));
end;

procedure TACBrIMendesResumo.Assign(Source: TACBrIMendesResumo);
begin
  fDataPrimeiroConsumo := Source.DataPrimeiroConsumo;
  fDataUltimoConsumo := Source.DataUltimoConsumo;
  fProdutosPendentes_Interno := Source.ProdutosPendentes_Interno;
  fProdutosPendentes_EAN := Source.ProdutosPendentes_EAN;
  fProdutosPendentes_Devolvidos := Source.ProdutosPendentes_Devolvidos;
  fProdutosPendentes_DataInicio := Source.ProdutosPendentes_DataInicio;
end;

{ TACBrIMendesHistoricoResponse }

constructor TACBrIMendesHistoricoResponse.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrIMendesHistoricoResponse.Destroy;
begin
  if Assigned(fResumo) then
    fResumo.Free;
  if Assigned(fProdDevolvidos) then
    fProdDevolvidos.Free;
  inherited Destroy;
end;

procedure TACBrIMendesHistoricoResponse.Clear;
begin
  if Assigned(fResumo) then
    fResumo.Clear;
  if Assigned(fProdDevolvidos) then
    fProdDevolvidos.Clear;
end;

function TACBrIMendesHistoricoResponse.IsEmpty: Boolean;
begin
  Result :=
    (not Assigned(fResumo) or fResumo.IsEmpty) and
    (not Assigned(fProdDevolvidos) or fProdDevolvidos.IsEmpty);
end;

function TACBrIMendesHistoricoResponse.GetResumo: TACBrIMendesResumo;
begin
  if not Assigned(fResumo) then
    fResumo := TACBrIMendesResumo.Create('resumo');
  Result := fResumo;
end;

function TACBrIMendesHistoricoResponse.GetProdDevolvidos: TACBrIMendesProdutos;
begin
  if not Assigned(fProdDevolvidos) then
    fProdDevolvidos := TACBrIMendesProdutos.Create('prodDevolvidos');
  Result := fProdDevolvidos;
end;

procedure TACBrIMendesHistoricoResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  Resumo.ReadFromJSon(aJSon);
  ProdDevolvidos.ReadFromJSon(aJSon);
end;

procedure TACBrIMendesHistoricoResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(fResumo) and (not fResumo.IsEmpty) then
    fResumo.WriteToJSon(aJSon);
  if Assigned(fProdDevolvidos) and (not fProdDevolvidos.IsEmpty) then
    fProdDevolvidos.WriteToJSon(aJSon);
end;

procedure TACBrIMendesHistoricoResponse.Assign(Source: TACBrIMendesHistoricoResponse);
begin
  if Assigned(Source.Resumo) then
    Resumo.Assign(Source.Resumo);
  if Assigned(Source.ProdDevolvidos) then
    ProdDevolvidos.Assign(Source.ProdDevolvidos);
end;

procedure TACBrIMendesHistoricoResponse.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesHistoricoResponse) then
    Assign(TACBrIMendesHistoricoResponse(aSource));
end;

{ TACBrIMendesRevenda }

constructor TACBrIMendesRevenda.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrIMendesRevenda.Clear;
begin
  fCNPJCPF := EmptyStr;
  fNome := EmptyStr;
  fFone := EmptyStr;
  fEmail := EmptyStr;
end;

function TACBrIMendesRevenda.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fCNPJCPF) and
    EstaVazio(fNome) and
    EstaVazio(fFone) and
    EstaVazio(fEmail);
end;

procedure TACBrIMendesRevenda.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('cnpjcpf', fCNPJCPF)
    .Value('nome', fNome)
    .Value('fone', fFone)
    .Value('email', fEmail);
end;

procedure TACBrIMendesRevenda.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('cnpjcpf', fCNPJCPF)
    .AddPair('nome', fNome)
    .AddPair('fone', fFone)
    .AddPair('email', fEmail);
end;

procedure TACBrIMendesRevenda.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesRevenda) then
    Assign(TACBrIMendesRevenda(aSource));
end;

procedure TACBrIMendesRevenda.Assign(Source: TACBrIMendesRevenda);
begin
  fCNPJCPF := Source.CNPJCPF;
  fNome := Source.Nome;
  fFone := Source.Fone;
  fEmail := Source.Email;
end;

{ TACBrIMendesCliente }

constructor TACBrIMendesCliente.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrIMendesCliente.Destroy;
begin
  if Assigned(fRevenda) then
    fRevenda.Free;
  inherited Destroy;
end;

procedure TACBrIMendesCliente.Clear;
begin
  fCNPJCPF := EmptyStr;
  fRazaoSocial := EmptyStr;
  fEndereco := EmptyStr;
  fNro := EmptyStr;
  fBairro := EmptyStr;
  fCidade := EmptyStr;
  fUF := EmptyStr;
  fCEP := EmptyStr;
  fFone := EmptyStr;
  fResponsavel := EmptyStr;
  fEmail := EmptyStr;
  fNroCNPJ := 0;
  fValorImplantacao := 0;
  fValorMensalidade := 0;
  fStatus := EmptyStr;
  fRegimeTrib := EmptyStr;
  fTipoAtiv := EmptyStr;
  fObservacao := EmptyStr;
  if Assigned(fRevenda) then
    fRevenda.Clear;
end;

function TACBrIMendesCliente.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fCNPJCPF) and
    EstaVazio(fRazaoSocial) and
    EstaVazio(fEndereco) and
    EstaVazio(fNro) and
    EstaVazio(fBairro) and
    EstaVazio(fCidade) and
    EstaVazio(fUF) and
    EstaVazio(fCEP) and
    EstaVazio(fFone) and
    EstaVazio(fResponsavel) and
    EstaVazio(fEmail) and
    EstaZerado(fNroCNPJ) and
    EstaZerado(fValorImplantacao) and
    EstaZerado(fValorMensalidade) and
    EstaVazio(fStatus) and
    EstaVazio(fRegimeTrib) and
    EstaVazio(fTipoAtiv) and
    EstaVazio(fObservacao) and
    (not Assigned(fRevenda) or fRevenda.IsEmpty);
end;

function TACBrIMendesCliente.GetRevenda: TACBrIMendesRevenda;
begin
  if not Assigned(fRevenda) then
    fRevenda := TACBrIMendesRevenda.Create('revenda');
  Result := fRevenda;
end;

procedure TACBrIMendesCliente.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('cnpjcpf', fCNPJCPF)
    .Value('razaosocial', fRazaoSocial)
    .Value('endereco', fEndereco)
    .Value('nro', fNro)
    .Value('bairro', fBairro)
    .Value('cidade', fCidade)
    .Value('uf', fUF)
    .Value('cep', fCEP)
    .Value('fone', fFone)
    .Value('responsavel', fResponsavel)
    .Value('email', fEmail)
    .Value('nro_cnpj', fNroCNPJ)
    .Value('valorimplantacao', fValorImplantacao)
    .Value('valormensalidade', fValorMensalidade)
    .Value('status', fStatus)
    .Value('regimeTrib', fRegimeTrib)
    .Value('tipoAtiv', fTipoAtiv)
    .Value('observacao', fObservacao);
  Revenda.ReadFromJSon(aJSon);
end;

procedure TACBrIMendesCliente.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('cnpjcpf', fCNPJCPF)
    .AddPair('razaosocial', fRazaoSocial)
    .AddPair('endereco', fEndereco)
    .AddPair('nro', fNro)
    .AddPair('bairro', fBairro)
    .AddPair('cidade', fCidade)
    .AddPair('uf', fUF)
    .AddPair('cep', fCEP)
    .AddPair('fone', fFone)
    .AddPair('responsavel', fResponsavel)
    .AddPair('email', fEmail)
    .AddPair('nro_cnpj', fNroCNPJ)
    .AddPair('valorimplantacao', fValorImplantacao)
    .AddPair('valormensalidade', fValorMensalidade)
    .AddPair('status', fStatus)
    .AddPair('regimeTrib', fRegimeTrib)
    .AddPair('tipoAtiv', fTipoAtiv)
    .AddPair('observacao', fObservacao);
  if Assigned(fRevenda) and (not fRevenda.IsEmpty) then
    fRevenda.WriteToJSon(aJSon);
end;

procedure TACBrIMendesCliente.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesCliente) then
    Assign(TACBrIMendesCliente(aSource));
end;

procedure TACBrIMendesCliente.Assign(Source: TACBrIMendesCliente);
begin
  fCNPJCPF := Source.CNPJCPF;
  fRazaoSocial := Source.RazaoSocial;
  fEndereco := Source.Endereco;
  fNro := Source.Nro;
  fBairro := Source.Bairro;
  fCidade := Source.Cidade;
  fUF := Source.UF;
  fCEP := Source.CEP;
  fFone := Source.Fone;
  fResponsavel := Source.Responsavel;
  fEmail := Source.Email;
  fNroCNPJ := Source.NroCNPJ;
  fValorImplantacao := Source.ValorImplantacao;
  fValorMensalidade := Source.ValorMensalidade;
  fStatus := Source.Status;
  fRegimeTrib := Source.RegimeTrib;
  fTipoAtiv := Source.TipoAtiv;
  fObservacao := Source.Observacao;
  if Assigned(Source.Revenda) then
    Revenda.Assign(Source.Revenda);
end;

{ TACBrIMendesErro }

constructor TACBrIMendesErro.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrIMendesErro.Clear;
begin
  fSucesso := False;
  fMensagem := EmptyStr;
end;

function TACBrIMendesErro.IsEmpty: Boolean;
begin
  Result :=
    (not fSucesso) and
    EstaVazio(fMensagem);
end;

procedure TACBrIMendesErro.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('sucesso', fSucesso)
    .Value('mensagem', fMensagem);
end;

procedure TACBrIMendesErro.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('sucesso', fSucesso)
    .AddPair('mensagem', fMensagem);
end;

procedure TACBrIMendesErro.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesErro) then
    Assign(TACBrIMendesErro(aSource));
end;

procedure TACBrIMendesErro.Assign(Source: TACBrIMendesErro);
begin
  fSucesso := Source.Sucesso;
  fMensagem := Source.Mensagem;
end;

{ TACBrIMendesRegimeEspecial }

constructor TACBrIMendesRegimeEspecial.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrIMendesRegimeEspecial.Clear;
begin
  fCode := EmptyStr;
  fDescription := EmptyStr;
  fLegalBasis := EmptyStr;
  fInitialValidity := 0;
  fFinalValidity := 0;
end;

function TACBrIMendesRegimeEspecial.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fCode) and
    EstaVazio(fDescription) and
    EstaVazio(fLegalBasis) and
    EstaZerado(fInitialValidity) and
    EstaZerado(fFinalValidity);
end;

procedure TACBrIMendesRegimeEspecial.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2: String;
begin
  {$IfDef FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  {$EndIf}
  aJSon
    .Value('code', fCode)
    .Value('description', fDescription)
    .Value('legalBasis', fLegalBasis)
    .Value('initialValidity', s1)
    .Value('finalValidity', s2);
  if NaoEstaVazio(s1) then
    fInitialValidity := StringToDateTimeDef(s1, 0, 'YYYY-MM-DD');
  if NaoEstaVazio(s2) then
    fFinalValidity := StringToDateTimeDef(s2, 0, 'YYYY-MM-DD');
end;

procedure TACBrIMendesRegimeEspecial.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('code', fCode)
    .AddPair('description', fDescription)
    .AddPair('legalBasis', fLegalBasis)
    .AddPair('initialValidity', FormatDateBr(fInitialValidity, 'YYYY-MM-DD'), False)
    .AddPair('finalValidity', FormatDateBr(fFinalValidity, 'YYYY-MM-DD'), False);
end;

procedure TACBrIMendesRegimeEspecial.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrIMendesRegimeEspecial) then
    Assign(TACBrIMendesRegimeEspecial(aSource));
end;

procedure TACBrIMendesRegimeEspecial.Assign(Source: TACBrIMendesRegimeEspecial);
begin
  fCode := Source.Code;
  fDescription := Source.Description;
  fLegalBasis := Source.LegalBasis;
  fInitialValidity := Source.InitialValidity;
  fFinalValidity := Source.FinalValidity;
end;

{ TACBrIMendesRegimeEspecialList }

function TACBrIMendesRegimeEspecialList.New: TACBrIMendesRegimeEspecial;
begin
  Result := TACBrIMendesRegimeEspecial.Create;
end;

function TACBrIMendesRegimeEspecialList.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrIMendesRegimeEspecialList.Add(ARegimeEspecial: TACBrIMendesRegimeEspecial): Integer;
begin
  Result := inherited Add(ARegimeEspecial);
end;

procedure TACBrIMendesRegimeEspecialList.Insert(AIndex: Integer; ARegimeEspecial: TACBrIMendesRegimeEspecial);
begin
  inherited Insert(AIndex, ARegimeEspecial);
end;

function TACBrIMendesRegimeEspecialList.GetItem(AIndex: Integer): TACBrIMendesRegimeEspecial;
begin
  Result := TACBrIMendesRegimeEspecial(inherited Items[AIndex]);
end;

procedure TACBrIMendesRegimeEspecialList.SetItem(AIndex: Integer; AValue: TACBrIMendesRegimeEspecial);
begin
  inherited Items[AIndex] := AValue;
end;

function TACBrIMendes.GetConsultarDescricaoResponse: TACBrIMendesConsultarResponse;
begin
  if (not Assigned(fConsultarDescricaoResponse)) then
    fConsultarDescricaoResponse := TACBrIMendesConsultarResponse.Create;
  Result := fConsultarDescricaoResponse;
end;

function TACBrIMendes.GetRespostaErro: TACBrIMendesErro;
begin
  if (not Assigned(fRespostaErro)) then
    fRespostaErro := TACBrIMendesErro.Create;
  Result := fRespostaErro;
end;

function TACBrIMendes.GetSenha: AnsiString;
begin
  Result := StrCrypt(fSenha, fKey)  // Descritografa a Senha
end;

function TACBrIMendes.CalcularURL: String;
begin
  if (fAmbiente = imaProducao) then
    Result := cIMendesURLProducao
  else
    Result := cIMendesURLHomologacao;
end;

procedure TACBrIMendes.SetSenha(AValue: AnsiString);
begin
  if NaoEstaVazio(fKey) and (fSenha = StrCrypt(AValue, fKey)) then
    Exit;

  fKey := FormatDateTime('hhnnsszzz', Now);
  fSenha := StrCrypt(AValue, fKey);  // Salva Senha de forma Criptografada, para evitar "Inspect"
end;

procedure TACBrIMendes.ValidarConfiguracao;
var
  wErro: TStringList;
begin
  wErro := TStringList.Create;
  try
    if (fAmbiente = imaNenhum) then
      wErro.Add('- Ambiente');
    if EstaVazio(fCNPJ) then
      wErro.Add('- Email');
    if EstaVazio(fSenha) then
      wErro.Add('- Senha');
    if NaoEstaZerado(wErro.Count) then
      raise EACBrIMendesAuthError.Create('Configure as propriedades:' + sLineBreak + wErro.Text);
  finally
    wErro.Free;
  end;
end;

constructor TACBrIMendes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKey := EmptyStr;
  fCNPJ := EmptyStr;
  fToken := EmptyStr;
  fSenha := EmptyStr;
  fAmbiente := imaNenhum;
  fRespostaErro := Nil;
  fConsultarDescricaoResponse := Nil;
end;

destructor TACBrIMendes.Destroy;
begin
  if Assigned(fRespostaErro) then
    fRespostaErro.Free;
  if Assigned(fConsultarDescricaoResponse) then
    fConsultarDescricaoResponse.Free;
  inherited Destroy;
end;

procedure TACBrIMendes.Clear;
begin
  RespostaErro.Clear;
  ConsultarDescricaoResponse.Clear;
end;

procedure TACBrIMendes.Autenticar;
var
  wBody, wResp: TACBrJSONObject;
  wURL, wBodyStr: String;
begin
  if NaoEstaVazio(fToken) then
    Exit;

  RegistrarLog('  TACBrIMendes.Autenticar');
  LimparHTTP;
  ValidarConfiguracao;

  HttpSend.Protocol := '1.1';
  HttpSend.MimeType := cContentTypeApplicationJSon;
  wBody := TACBrJSONObject.Create;
  try
    wBody
      .AddPair('cnpj', fCNPJ)
      .AddPair('senha', Senha);

    wBodyStr := wBody.ToJSON;
    RegistrarLog('Req.Body: ' + wBodyStr);
    WriteStrToStream(HTTPSend.Document, wBodyStr);
  finally
    wBody.Free;
  end;

  try
    wURL := CalcularURL + cIMendesEndPointLogin;
    HTTPMethod(cHTTPMethodPOST, wURL);

    if (HTTPResultCode <> HTTP_OK) then
      raise EACBrIMendesAuthError.Create('Erro ao Autenticar:' + sLineBreak + HTTPResponse);

    wResp := TACBrJSONObject.Parse(HTTPResponse);
    try
      fToken := wResp.AsString['token'];
    finally
      wResp.Free;
    end;
  except
    on E: Exception do
      raise EACBrIMendesAuthError.Create('Erro ao Autenticar:' + sLineBreak + E.Message);
  end;
end;

function TACBrIMendes.ConsultarDescricao(const aDescricao: String;
  const aCNPJ: String): Boolean;
var
  jBody: TACBrJSONObject;
  wCNPJ, sBody: String;
begin
  Result := False;
  ValidarConfiguracao;

  LimparHTTP;
  HttpSend.Protocol := '1.1';
  HttpSend.MimeType := cContentTypeApplicationJSon;
  HTTPSend.Headers.Add('login: ' + fCNPJ);
  HTTPSend.Headers.Add('senha: ' + Senha);
     
  wCNPJ := fCNPJ;
  if NaoEstaVazio(aCNPJ) then
    wCNPJ := aCNPJ;

  jBody := TACBrJSONObject.Create;
  try
    jBody.AddPair(cIMendesNomeServico, cIMendesServicoDescricaoProdutos);
    jBody.AddPair(cIMendesDadosServico, wCNPJ + '|' + aDescricao);
    sBody := jBody.ToJSON;
    RegistrarLog('Req.wBody: ' + sBody);
    WriteStrToStream(HTTPSend.Document, sBody);
  finally
    jBody.Free;
  end; 

  try
    URLPathParams.Add(cIMendesAPIV1);
    URLPathParams.Add(cIMendesEndpointEnviaRecebeDados);
    HTTPMethod(cHTTPMethodPOST, CalcularURL);
    Result := (HTTPResultCode = HTTP_OK);
    if Result then
      ConsultarDescricaoResponse.AsJSON := HTTPResponse
    else
      RespostaErro.AsJSON := HTTPResponse;
  except
    on E: Exception do
      raise EACBrIMendesDataSend.Create('Erro ao Enviar Dados:' + sLineBreak + E.Message);
  end;
end;

function TACBrIMendes.ConsultarAlterados(const aUF: String; const aCNPJ: String): Boolean;
begin

end;

function TACBrIMendes.ConsultarRegimesEspeciais(const aUF: String): Boolean;
begin

end;

function TACBrIMendes.HistoricoAcesso: Boolean;
begin

end;

end. 
