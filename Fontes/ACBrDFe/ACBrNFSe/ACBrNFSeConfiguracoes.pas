{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, ACBrDFeSSL, pcnConversao, pnfsConversao;

type

 TEmitenteConfNFSe = class;
 TParamEnvelope = class;
 TConfigEnvelope = class;

 TConfigGeral = record
    VersaoSoap: String;
    Prefixo2: String;
    Prefixo3: String;
    Prefixo4: String;
    Identificador: String;
    QuebradeLinha: String;
    RetornoNFSe: String;
    ProLinkNFSe: String;
    HomLinkNFSe: String;
    DadosSenha: String;
    UseCertificateHTTP: Boolean;
 end;

 TConfigRemover = record
    QuebradeLinhaRetorno: Boolean;
    EComercial: Boolean;
    Tabulacao: Boolean;
    TagQuebradeLinhaUnica: Boolean;
 end;

 TConfigNameSpace = record
   Producao: String;
   Homologacao: String;
 end;

 TConfigAssinar = record
    RPS: Boolean;
    Lote: Boolean;
    URI: Boolean;
    ConsSit: Boolean;
    ConsLote: Boolean;
    ConsNFSeRps: Boolean;
    ConsNFSe: Boolean;
    Cancelar: Boolean;
    RpsGerar: Boolean;
    LoteGerar: Boolean;
    Substituir: Boolean;
    AbrirSessao: Boolean;
    FecharSessao: Boolean;
 end;

 TConfigXML = record
    Layout: String;
    VersaoDados: String;
    VersaoXML: String;
    NameSpace: String;
    CabecalhoStr: Boolean;
    DadosStr: Boolean;
    VersaoAtrib: String;
  end;

 TConfigSchemas = record
    Validar: Boolean;
    DefTipos: String;
    Cabecalho: String;
    ServicoTeste: String;
    ServicoEnviar: String;
    ServicoConSit: String;
    ServicoConLot: String;
    ServicoConRps: String;
    ServicoConSeqRps: String;
    ServicoConNfse: String;
    ServicoCancelar: String;
    ServicoGerar: String;
    ServicoEnviarSincrono: String;
    ServicoSubstituir: String;
    ServicoAbrirSessao: String;
    ServicoFecharSessao: String;
  end;

 TConfigSoapAction = record
    Teste: String;
    Recepcionar: String;
    ConsSit: String;
    ConsLote: String;
    ConsNFSeRps: String;
    ConsNFSe: String;
    Cancelar: String;
    Gerar: String;
    RecSincrono: String;
    Substituir: String;
    AbrirSessao: String;
    FecharSessao: String;
 end;

 TConfigURL = record
    HomRecepcaoLoteRPS: String;
    HomConsultaLoteRPS: String;
    HomConsultaNFSeRPS: String;
    HomConsultaSitLoteRPS: String;
    HomConsultaSeqRPS: String;
    HomConsultaNFSe: String;
    HomCancelaNFSe: String;
    HomGerarNFSe: String;
    HomRecepcaoSincrono: String;
    HomSubstituiNFSe: String;
    HomAbrirSessao: String;
    HomFecharSessao: String;

    ProRecepcaoLoteRPS: String;
    ProConsultaLoteRPS: String;
    ProConsultaSeqRPS: String;
    ProConsultaNFSeRPS: String;
    ProConsultaSitLoteRPS: String;
    ProConsultaNFSe: String;
    ProCancelaNFSe: String;
    ProGerarNFSe: String;
    ProRecepcaoSincrono: String;
    ProSubstituiNFSe: String;
    ProAbrirSessao: String;
    ProFecharSessao: String;
  end;

 TConfigGrupoMsgRet = record
    GrupoMsg: String;
    Recepcionar: String;
    ConsSit: String;
    ConsLote: String;
    ConsNFSeRps: String;
    ConsNFSe: String;
    Cancelar: String;
    Gerar: String;
    RecSincrono: String;
    Substituir: String;
    AbrirSessao: String;
    FecharSessao: String;
 end;

 { TParamEnvelope }

 TParamEnvelope = class
  private
    FEnvelope: String;
    FIncluiEncodingCab: Boolean;
    FIncluiEncodingDados: Boolean;
    FCabecalhoStr: Boolean;
    FDadosStr: Boolean;
    FTagGrupo: String;
    FTagElemento: String;
    FDocElemento: String;
    FInfElemento: String;
  public
//    Constructor Create;
//    destructor Destroy; override;
//    procedure Assign(Source: TPersistent); override;

    property Envelope: String             read FEnvelope            write FEnvelope;
    property IncluiEncodingCab: Boolean   read FIncluiEncodingCab   write FIncluiEncodingCab;
    property IncluiEncodingDados: Boolean read FIncluiEncodingDados write FIncluiEncodingDados;
    property CabecalhoStr: Boolean        read FCabecalhoStr        write FCabecalhoStr;
    property DadosStr: Boolean            read FDadosStr            write FDadosStr;
    property TagGrupo: String             read FTagGrupo            write FTagGrupo;
    property TagElemento: String          read FTagElemento         write FTagElemento;
    property DocElemento: String          read FDocElemento         write FDocElemento;
    property InfElemento: String          read FInfElemento         write FInfElemento;
  end;

 { TConfigEnvelope }

 TConfigEnvelope = class
  private
    FCabecalhoMsg: String;

    FRecepcionar: TParamEnvelope;
    FTeste: TParamEnvelope;
    FConsSit: TParamEnvelope;
    FConsLote: TParamEnvelope;
    FConsNFSeRps: TParamEnvelope;
    FConsNFSe: TParamEnvelope;
    FCancelar: TParamEnvelope;
    FGerar: TParamEnvelope;
    FRecSincrono: TParamEnvelope;
    FSubstituir: TParamEnvelope;
    FAbrirSessao: TParamEnvelope;
    FFecharSessao: TParamEnvelope;
  public
    Constructor Create;
    destructor Destroy; override;
//    procedure Assign(Source: TPersistent); override;

    property CabecalhoMsg: String read FCabecalhoMsg write FCabecalhoMsg;
    property Recepcionar: TParamEnvelope read FRecepcionar write FRecepcionar;
    property Teste: TParamEnvelope read FTeste write FTeste;
    property ConsSit: TParamEnvelope read FConsSit write FConsSit;
    property ConsLote: TParamEnvelope read FConsLote write FConsLote;
    property ConsNFSeRps: TParamEnvelope read FConsNFSeRps write FConsNFSeRps;
    property ConsNFSe: TParamEnvelope read FConsNFSe write FConsNFSe;
    property Cancelar: TParamEnvelope read FCancelar write FCancelar;
    property Gerar: TParamEnvelope read FGerar write FGerar;
    property RecSincrono: TParamEnvelope read FRecSincrono write FRecSincrono;
    property Substituir: TParamEnvelope read FSubstituir write FSubstituir;
    property AbrirSessao: TParamEnvelope read FAbrirSessao write FAbrirSessao;
    property FecharSessao: TParamEnvelope read FFecharSessao write FFecharSessao;
  end;

  { TDadosSenhaParamsCollectionItem }

  TDadosSenhaParamsCollectionItem = class(TCollectionItem)
  private
    FParam: String;
    FConteudo: String;
  published
    property Param: String read FParam write FParam;
    property Conteudo: String read FConteudo write FConteudo;
  end;

  { TDadosSenhaParamsCollection }

  TDadosSenhaParamsCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDadosSenhaParamsCollectionItem;
    procedure SetItem(Index: Integer; Const Value: TDadosSenhaParamsCollectionItem);
  public
    constructor Create(AOwner: TEmitenteConfNFSe);

    function Add: TDadosSenhaParamsCollectionItem;
    property Items[Index: Integer]: TDadosSenhaParamsCollectionItem read GetItem write SetItem; default;
  end;

 { TEmitenteConfNFSe }

 TEmitenteConfNFSe = class(TPersistent)
  private
    FCNPJ: String;
    FInscMun: String;
    FRazSocial: String;
    FWebUser: String;
    FWebSenha: String;
    FWebFraseSecr: String;
    FWebChaveAcesso: String;
    FDadosSenhaParams: TDadosSenhaParamsCollection;

    procedure SetDadosSenhaParams(const Value: TDadosSenhaParamsCollection);
  public
    Constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CNPJ: String         read FCNPJ         write FCNPJ;
    property InscMun: String      read FInscMun      write FInscMun;
    property RazSocial: String    read FRazSocial    write FRazSocial;
    property WebUser: String      read FWebUser      write FWebUser;
    property WebSenha: String     read FWebSenha     write FWebSenha;
    property WebFraseSecr: String read FWebFraseSecr write FWebFraseSecr;
    property WebChaveAcesso: String read FWebChaveAcesso write FWebChaveAcesso;
    property DadosSenhaParams: TDadosSenhaParamsCollection read FDadosSenhaParams write SetDadosSenhaParams;
  end;

  { TGeralConfNFSe }

  TGeralConfNFSe = class(TGeralConf)
  private
    FPIniParams: TMemIniFile;

    FConfigGeral: TConfigGeral;
    FConfigNameSpace: TConfigNameSpace;
    FConfigAssinar: TConfigAssinar;
    FConfigXML: TConfigXML;
    FConfigSchemas: TConfigSchemas;
    FConfigSoapAction: TConfigSoapAction;
    FConfigURL: TConfigURL;

    FConfigEnvelope: TConfigEnvelope;

    FConfigGrupoMsgRet: TConfigGrupoMsgRet;

    FCodigoMunicipio: Integer;
    FProvedor: TnfseProvedor;
    FxLinkURL_H: String;
    FxLinkURL_P: String;
    FxProvedor: String;
    FxMunicipio: String;
    FxUF: String;
    FxNomeURL_H: String;
    FxNomeURL_P: String;
    FSenhaWeb: String;
    FUserWeb: String;
    FCNPJPrefeitura: String;
    FConsultaLoteAposEnvio: Boolean;
    FPathIniCidades: String;
    FPathIniProvedor: String;
    FEmitente: TEmitenteConfNFSe;
    FConfigRemover: TConfigRemover;
    FBanco_P: String;
    FBanco_H: String;

    procedure SetCodigoMunicipio(const Value: Integer);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeGeralConfNFSe: TGeralConfNFSe); reintroduce;
    procedure SetConfigMunicipio;

    property ConfigGeral: TConfigGeral read FConfigGeral;
    property ConfigNameSpace: TConfigNameSpace read FConfigNameSpace;
    property ConfigAssinar: TConfigAssinar read FConfigAssinar;
    property ConfigXML: TConfigXML read FConfigXML;
    property ConfigSchemas: TConfigSchemas read FConfigSchemas;
    property ConfigSoapAction: TConfigSoapAction read FConfigSoapAction;
    property ConfigURL: TConfigURL read FConfigURL;

    property ConfigEnvelope: TConfigEnvelope read FConfigEnvelope;

    property ConfigGrupoMsgRet: TConfigGrupoMsgRet read FConfigGrupoMsgRet;
    property ConfigRemover: TConfigRemover read FConfigRemover;
  published
    property CodigoMunicipio: Integer read FCodigoMunicipio write SetCodigoMunicipio;
    property Provedor: TnfseProvedor read FProvedor;
    property xProvedor: String read FxProvedor;
    property xMunicipio: String read FxMunicipio;
    property xUF: String read FxUF;
    // Alguns provedores possui o nome da cidade na URL dos WebServer
    property xNomeURL_H: String read FxNomeURL_H;
    property xNomeURL_P: String read FxNomeURL_P;
    property xLinkURL_H: String read FxLinkURL_H;
    property xLinkURL_P: String read FxLinkURL_P;
    property SenhaWeb: String read FSenhaWeb write FSenhaWeb;
    property UserWeb: String read FUserWeb write FUserWeb;
    property CNPJPrefeitura: String read FCNPJPrefeitura write FCNPJPrefeitura;
    property ConsultaLoteAposEnvio: Boolean read FConsultaLoteAposEnvio write FConsultaLoteAposEnvio;
    property PathIniCidades: String read FPathIniCidades write FPathIniCidades;
    property PathIniProvedor: String read FPathIniProvedor write FPathIniProvedor;

    property Emitente: TEmitenteConfNFSe read FEmitente write FEmitente;
    property Banco_P: String read FBanco_P;
    property Banco_H: String read FBanco_H;
  end;

  { TArquivosConfNFSe }

  TArquivosConfNFSe = class(TArquivosConf)
  private
    FEmissaoPathNFSe: boolean;
    FSalvarApenasNFSeProcessadas: boolean;
    FPathGer: String;
    FPathRPS: String;
    FPathNFSe: String;
    FPathCan: String;
    FNomeLongoNFSe: Boolean;
    FTabServicosExt: Boolean;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeArquivosConfNFSe: TArquivosConfNFSe); reintroduce;

    function GetPathGer(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathRPS(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathNFSe(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathCan(Data: TDateTime = 0; CNPJ: String = ''): String;
  published
    property EmissaoPathNFSe: boolean read FEmissaoPathNFSe
      write FEmissaoPathNFSe default False;
    property SalvarApenasNFSeProcessadas: boolean
      read FSalvarApenasNFSeProcessadas write FSalvarApenasNFSeProcessadas default False;
    property PathGer: String read FPathGer write FPathGer;
    property PathRPS: String read FPathRPS  write FPathRPS;
    property PathNFSe: String read FPathNFSe write FPathNFSe;
    property PathCan: String read FPathCan write FPathCan;
    property NomeLongoNFSe: Boolean read FNomeLongoNFSe write FNomeLongoNFSe default False;
    property TabServicosExt: Boolean read FTabServicosExt write FTabServicosExt default False;
  end;

  { TConfiguracoesNFSe }

  TConfiguracoesNFSe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfNFSe;
    function GetGeral: TGeralConfNFSe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesNFSe: TConfiguracoesNFSe); reintroduce;

  published
    property Geral: TGeralConfNFSe read GetGeral;
    property Arquivos: TArquivosConfNFSe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil, ACBrNFSe,
  DateUtils;

{ TEmitenteConfNFSe }

constructor TEmitenteConfNFSe.Create;
begin
  FCNPJ := '';
  FInscMun := '';
  FRazSocial := '';
  FWebUser := '';
  FWebSenha := '';
  FWebFraseSecr := '';
  FWebChaveAcesso := '';
  FDadosSenhaParams := TDadosSenhaParamsCollection.Create(Self);
end;

procedure TEmitenteConfNFSe.Assign(Source: TPersistent);
begin
  if Source is TEmitenteConfNFSe then
  begin
    FCNPJ := TEmitenteConfNFSe(Source).CNPJ;
    FInscMun := TEmitenteConfNFSe(Source).InscMun;
    FRazSocial := TEmitenteConfNFSe(Source).RazSocial;
    FWebUser := TEmitenteConfNFSe(Source).WebUser;
    FWebSenha := TEmitenteConfNFSe(Source).WebSenha;
    FWebFraseSecr := TEmitenteConfNFSe(Source).WebFraseSecr;
    FWebChaveAcesso := TEmitenteConfNFSe(Source).WebChaveAcesso;
  end
  else
    inherited Assign(Source);
end;

destructor TEmitenteConfNFSe.Destroy;
begin
  FDadosSenhaParams.Free;

  inherited;
end;

procedure TEmitenteConfNFSe.SetDadosSenhaParams(
  const Value: TDadosSenhaParamsCollection);
begin
  FDadosSenhaParams := Value;
end;

{ TConfiguracoesNFSe }

constructor TConfiguracoesNFSe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  WebServices.ResourceName := 'ACBrNFSeServicos';
end;

procedure TConfiguracoesNFSe.Assign(DeConfiguracoesNFSe: TConfiguracoesNFSe);
begin
  Geral.Assign(DeConfiguracoesNFSe.Geral);
  WebServices.Assign(DeConfiguracoesNFSe.WebServices);
  Certificados.Assign(DeConfiguracoesNFSe.Certificados);
  Arquivos.Assign(DeConfiguracoesNFSe.Arquivos);
end;

function TConfiguracoesNFSe.GetArquivos: TArquivosConfNFSe;
begin
  Result := TArquivosConfNFSe(FPArquivos);
end;

function TConfiguracoesNFSe.GetGeral: TGeralConfNFSe;
begin
  Result := TGeralConfNFSe(FPGeral);
end;

procedure TConfiguracoesNFSe.CreateGeralConf;
begin
  FPGeral := TGeralConfNFSe.Create(Self);
end;

procedure TConfiguracoesNFSe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfNFSe.Create(self);
end;

{ TGeralConfNFSe }

constructor TGeralConfNFSe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmitente := TEmitenteConfNFSe.Create;

  FConfigEnvelope := TConfigEnvelope.Create;

  FProvedor := proNenhum;
  FPathIniCidades := '';
  FPathIniProvedor := '';
end;

destructor TGeralConfNFSe.Destroy;
begin
  FEmitente.Free;

  FConfigEnvelope.Free;
  inherited;
end;

procedure TGeralConfNFSe.Assign(DeGeralConfNFSe: TGeralConfNFSe);
begin
  inherited Assign(DeGeralConfNFSe);

  FProvedor := DeGeralConfNFSe.Provedor;

  FEmitente.Assign(DeGeralConfNFSe.Emitente);
end;

procedure TGeralConfNFSe.SetCodigoMunicipio(const Value: Integer);
begin
  FCodigoMunicipio := Value;
  if FCodigoMunicipio <> 0 then
    SetConfigMunicipio;
end;

procedure TGeralConfNFSe.SetConfigMunicipio;
var
  Ok: Boolean;
  NomeArqParams, Texto, sCampo, sFim, CodIBGE: String;
  I: Integer;
begin
  // ===========================================================================
  // Verifica se o código IBGE consta no arquivo de paramêtros: Cidades.ini
  // se encontrar retorna o nome do Provedor
  // ===========================================================================

  if PathIniCidades <> '' then
    NomeArqParams := PathWithDelim(PathIniCidades)
  else
  begin
    NomeArqParams  := ApplicationPath;
    PathIniCidades := NomeArqParams;
  end;

  NomeArqParams := NomeArqParams + 'Cidades.ini';

  if not FileExists(NomeArqParams) then
    raise Exception.Create('Arquivo de Parâmetro não encontrado: ' + NomeArqParams);

  CodIBGE := IntToStr(FCodigoMunicipio);

  FPIniParams := TMemIniFile.Create(NomeArqParams);

  FxProvedor := FPIniParams.ReadString(CodIBGE, 'Provedor', '');

  if FxProvedor = 'ISSFortaleza' then
    FProvedor := StrToProvedor(Ok, 'Ginfes')
  else
    FProvedor := StrToProvedor(Ok, FxProvedor);

  FxMunicipio := FPIniParams.ReadString(CodIBGE, 'Nome'     , '');
  FxUF        := FPIniParams.ReadString(CodIBGE, 'UF'       , '');
  FxNomeURL_H := FPIniParams.ReadString(CodIBGE, 'NomeURL_H', '');
  FxNomeURL_P := FPIniParams.ReadString(CodIBGE, 'NomeURL_P', '');
  FxLinkURL_H := FPIniParams.ReadString(CodIBGE, 'LinkURL_H', '');
  FxLinkURL_P := FPIniParams.ReadString(CodIBGE, 'LinkURL_P', '');
  FBanco_P    := FPIniParams.ReadString(CodIBGE, 'Banco_P', '');
  FBanco_H    := FPIniParams.ReadString(CodIBGE, 'Banco_H', 'BANCO_DEMONSTRACAO');

  // Configuração especifica da versão dos dados para cidades do mesmo provedor,
  // mas com versões diferentes.
  FConfigXML.VersaoDados := FPIniParams.ReadString(CodIBGE, 'VersaoDados', '');
  FConfigXML.VersaoAtrib := FPIniParams.ReadString(CodIBGE, 'VersaoAtrib', '');

  FPIniParams.Free;

  if FProvedor = proNenhum then
    raise Exception.Create('Código do Municipio [' + CodIBGE + '] não Encontrado.');

  // ===========================================================================
  // Le as configurações especificas do Provedor referente a cidade desejada
  // ===========================================================================

  if PathIniProvedor <> '' then
    NomeArqParams := PathWithDelim(PathIniProvedor)
  else
    NomeArqParams := ApplicationPath;

  NomeArqParams := NomeArqParams + FxProvedor + '.ini';;

  if not FileExists(NomeArqParams) then
    raise Exception.Create('Arquivo de Parâmetro não encontrado: ' + NomeArqParams);

  FPIniParams := TMemIniFile.Create(NomeArqParams);

  FConfigGeral.VersaoSoap    := trim(FPIniParams.ReadString('Geral', 'VersaoSoap', ''));
  FConfigGeral.Prefixo2      := trim(FPIniParams.ReadString('Geral', 'Prefixo2', ''));
  FConfigGeral.Prefixo3      := trim(FPIniParams.ReadString('Geral', 'Prefixo3', ''));
  FConfigGeral.Prefixo4      := trim(FPIniParams.ReadString('Geral', 'Prefixo4', ''));
  FConfigGeral.Identificador := trim(FPIniParams.ReadString('Geral', 'Identificador', ''));
  FConfigGeral.QuebradeLinha := trim(FPIniParams.ReadString('Geral', 'QuebradeLinha', ''));

  if FConfigGeral.QuebradeLinha = 'ENTER' then
    FConfigGeral.QuebradeLinha := #13#10;

  FConfigGeral.UseCertificateHTTP := FPIniParams.ReadBool('Geral', 'UseCertificado', False);

  FConfigRemover.QuebradeLinhaRetorno  := FPIniParams.ReadBool('Remover', 'QuebradeLinhaRetorno', False);
  FConfigRemover.EComercial            := FPIniParams.ReadBool('Remover', 'EComercial', False);
  FConfigRemover.Tabulacao             := FPIniParams.ReadBool('Remover', 'Tabulacao', False);
  FConfigRemover.TagQuebradeLinhaUnica := FPIniParams.ReadBool('Remover', 'TagQuebradeLinhaUnica', False);

  FConfigNameSpace.Producao    := StringReplace(FPIniParams.ReadString('NameSpace', 'Producao'   , ''), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
  FConfigNameSpace.Homologacao := StringReplace(FPIniParams.ReadString('NameSpace', 'Homologacao', ''), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);

  FConfigAssinar.RPS := FPIniParams.ReadBool('Assinar', 'RPS', False);
  FConfigAssinar.Lote := FPIniParams.ReadBool('Assinar', 'Lote', False);
  FConfigAssinar.URI := FPIniParams.ReadBool('Assinar', 'URI', False);

//  FConfigAssinar.URI := (SSLLib <> libCapicom) and (FConfigGeral.Identificador = 'id');

  FConfigAssinar.ConsSit := FPIniParams.ReadBool('Assinar', 'ConsSit', False);
  FConfigAssinar.ConsLote := FPIniParams.ReadBool('Assinar', 'ConsLote', False);
  FConfigAssinar.ConsNFSeRps := FPIniParams.ReadBool('Assinar', 'ConsNFSeRps', False);
  FConfigAssinar.ConsNFSe := FPIniParams.ReadBool('Assinar', 'ConsNFSe', False);
  FConfigAssinar.Cancelar := FPIniParams.ReadBool('Assinar', 'Cancelar', False);
  FConfigAssinar.RpsGerar := FPIniParams.ReadBool('Assinar', 'RpsGerar', False);
  FConfigAssinar.LoteGerar := FPIniParams.ReadBool('Assinar', 'LoteGerar', False);
  FConfigAssinar.Substituir := FPIniParams.ReadBool('Assinar', 'Substituir', False);
  FConfigAssinar.AbrirSessao := FPIniParams.ReadBool('Assinar', 'AbrirSessao', False);
  FConfigAssinar.FecharSessao := FPIniParams.ReadBool('Assinar', 'FecharSessao', False);

  FConfigXML.Layout := FPIniParams.ReadString('XML', 'Layout', 'ABRASF');

  // Configura com a versão que esta no arquivo <provedor>.ini caso a cidade não
  // tenha uma versão especifica.
  if FConfigXML.VersaoDados = '' then
    FConfigXML.VersaoDados := FPIniParams.ReadString('XML', 'VersaoDados', '');

  if FConfigXML.VersaoAtrib = '' then
    FConfigXML.VersaoAtrib := FPIniParams.ReadString('XML', 'VersaoAtrib', '');

  FConfigXML.VersaoXML := FPIniParams.ReadString('XML', 'VersaoXML', '');
  FConfigXML.NameSpace := Trim(FPIniParams.ReadString('XML', 'NameSpace', ''));
  FConfigXML.CabecalhoStr := FPIniParams.ReadBool('XML', 'Cabecalho', False);
  FConfigXML.DadosStr := FPIniParams.ReadBool('XML', 'Dados', False);

  if FConfigXML.Layout = 'ABRASF' then
  begin
    if FConfigXML.VersaoXML = '1.00' then
      FConfigXML.Layout := 'ABRASFv1'
    else
      FConfigXML.Layout := 'ABRASFv2';
  end;

  FConfigSchemas.Validar := FPIniParams.ReadBool('Schemas', 'Validar', True);
  FConfigSchemas.DefTipos := FPIniParams.ReadString('Schemas', 'DefTipos', '');
  FConfigSchemas.Cabecalho := FPIniParams.ReadString('Schemas', 'Cabecalho', '');
  FConfigSchemas.ServicoTeste  := FPIniParams.ReadString('Schemas', 'ServicoTeste', '');
  FConfigSchemas.ServicoEnviar := FPIniParams.ReadString('Schemas', 'ServicoEnviar', '');
  FConfigSchemas.ServicoConSit := FPIniParams.ReadString('Schemas', 'ServicoConSit', '');
  FConfigSchemas.ServicoConLot := FPIniParams.ReadString('Schemas', 'ServicoConLot', '');
  FConfigSchemas.ServicoConRps := FPIniParams.ReadString('Schemas', 'ServicoConRps', '');
  FConfigSchemas.ServicoConNfse := FPIniParams.ReadString('Schemas', 'ServicoConNfse', '');
  FConfigSchemas.ServicoCancelar := FPIniParams.ReadString('Schemas', 'ServicoCancelar', '');
  FConfigSchemas.ServicoGerar := FPIniParams.ReadString('Schemas', 'ServicoGerar', '');
  FConfigSchemas.ServicoEnviarSincrono := FPIniParams.ReadString('Schemas', 'ServicoEnviarSincrono', '');
  FConfigSchemas.ServicoSubstituir := FPIniParams.ReadString('Schemas', 'ServicoSubstituir', '');
  FConfigSchemas.ServicoAbrirSessao := FPIniParams.ReadString('Schemas', 'ServicoAbrirSessao', '');
  FConfigSchemas.ServicoFecharSessao := FPIniParams.ReadString('Schemas', 'ServicoFecharSessao', '');

  if FPIniParams.ReadString('SoapAction', 'Recepcionar', '') = '*******' then
  begin
    FConfigSoapAction.Recepcionar := FPIniParams.ReadString('SoapAction', 'Recepcionar_' + CodIBGE , '*');
    FConfigSoapAction.ConsSit     := FPIniParams.ReadString('SoapAction', 'ConsSit_' + CodIBGE     , '*');
    FConfigSoapAction.ConsLote    := FPIniParams.ReadString('SoapAction', 'ConsLote_' + CodIBGE    , '*');
    FConfigSoapAction.ConsNFSeRps := FPIniParams.ReadString('SoapAction', 'ConsNFSeRps_' + CodIBGE , '*');
    FConfigSoapAction.ConsNFSe    := FPIniParams.ReadString('SoapAction', 'ConsNFSe_' + CodIBGE    , '*');
    FConfigSoapAction.Cancelar    := FPIniParams.ReadString('SoapAction', 'Cancelar_' + CodIBGE    , '*');
    FConfigSoapAction.Gerar       := FPIniParams.ReadString('SoapAction', 'Gerar_' + CodIBGE       , '*');
    FConfigSoapAction.RecSincrono := FPIniParams.ReadString('SoapAction', 'RecSincrono_' + CodIBGE , '*');
    FConfigSoapAction.Substituir  := FPIniParams.ReadString('SoapAction', 'Substituir_' + CodIBGE  , '*');
    FConfigSoapAction.AbrirSessao := FPIniParams.ReadString('SoapAction', 'AbrirSessao_' + CodIBGE , '*');
    FConfigSoapAction.FecharSessao:= FPIniParams.ReadString('SoapAction', 'FecharSessao_' + CodIBGE, '*');
  end
  else begin
    FConfigSoapAction.Teste       := FPIniParams.ReadString('SoapAction', 'Teste', '*');
    FConfigSoapAction.Recepcionar := FPIniParams.ReadString('SoapAction', 'Recepcionar', '*');
    FConfigSoapAction.ConsSit     := FPIniParams.ReadString('SoapAction', 'ConsSit'    , '*');
    FConfigSoapAction.ConsLote    := FPIniParams.ReadString('SoapAction', 'ConsLote'   , '*');
    FConfigSoapAction.ConsNFSeRps := FPIniParams.ReadString('SoapAction', 'ConsNFSeRps', '*');
    FConfigSoapAction.ConsNFSe    := FPIniParams.ReadString('SoapAction', 'ConsNFSe'   , '*');
    FConfigSoapAction.Cancelar    := FPIniParams.ReadString('SoapAction', 'Cancelar'   , '*');
    FConfigSoapAction.Gerar       := FPIniParams.ReadString('SoapAction', 'Gerar'      , '*');
    FConfigSoapAction.RecSincrono := FPIniParams.ReadString('SoapAction', 'RecSincrono', '*');
    FConfigSoapAction.Substituir  := FPIniParams.ReadString('SoapAction', 'Substituir' , '*');
    FConfigSoapAction.AbrirSessao := FPIniParams.ReadString('SoapAction', 'AbrirSessao' , '*');
    FConfigSoapAction.FecharSessao:= FPIniParams.ReadString('SoapAction', 'FecharSessao', '*');
  end;
  
  if FPIniParams.ReadString('URL_H', 'RecepcaoLoteRPS', '') = '*******' then
  begin
    FConfigURL.HomRecepcaoLoteRPS    := StringReplace(FPIniParams.ReadString('URL_H', 'RecepcaoLoteRPS_' + CodIBGE   , ''                           ), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomConsultaSitLoteRPS := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaSitLoteRPS_' + CodIBGE, FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomConsultaLoteRPS    := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaLoteRPS_' + CodIBGE   , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomConsultaNFSeRPS    := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaNFSeRPS_' + CodIBGE   , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomConsultaNFSe       := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaNFSe_' + CodIBGE      , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomCancelaNFSe        := StringReplace(FPIniParams.ReadString('URL_H', 'CancelaNFSe_' + CodIBGE       , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomGerarNFSe          := StringReplace(FPIniParams.ReadString('URL_H', 'GerarNFSe_' + CodIBGE         , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomRecepcaoSincrono   := StringReplace(FPIniParams.ReadString('URL_H', 'RecepcaoSincrono_' + CodIBGE  , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomSubstituiNFSe      := StringReplace(FPIniParams.ReadString('URL_H', 'SubstituiNFSe_' + CodIBGE     , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomAbrirSessao        := StringReplace(FPIniParams.ReadString('URL_H', 'AbrirSessao_' + CodIBGE       , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    FConfigURL.HomFecharSessao       := StringReplace(FPIniParams.ReadString('URL_H', 'FecharSessao_' + CodIBGE      , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
  end
  else
  begin
    if FCodigoMunicipio = 5218805 then
    begin
      FConfigURL.ProRecepcaoLoteRPS    := FxNomeURL_H;
      FConfigURL.ProConsultaSitLoteRPS := FxNomeURL_H;
      FConfigURL.ProConsultaLoteRPS    := FxNomeURL_H;
      FConfigURL.ProConsultaNFSeRPS    := FxNomeURL_H;
      FConfigURL.ProConsultaNFSe       := FxNomeURL_H;
      FConfigURL.ProCancelaNFSe        := FxNomeURL_H;
      FConfigURL.ProGerarNFSe          := FxNomeURL_H;
      FConfigURL.ProRecepcaoSincrono   := FxNomeURL_H;
      FConfigURL.ProSubstituiNFSe      := FxNomeURL_H;
      FConfigURL.ProAbrirSessao        := FxNomeURL_H;
      FConfigURL.ProFecharSessao       := FxNomeURL_H;
    end
    else
    begin
      FConfigURL.HomRecepcaoLoteRPS    := StringReplace(FPIniParams.ReadString('URL_H', 'RecepcaoLoteRPS'   , ''                           ), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomConsultaSitLoteRPS := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaSitLoteRPS', FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomConsultaLoteRPS    := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaLoteRPS'   , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomConsultaNFSeRPS    := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaNFSeRPS'   , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomConsultaNFSe       := StringReplace(FPIniParams.ReadString('URL_H', 'ConsultaNFSe'      , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomCancelaNFSe        := StringReplace(FPIniParams.ReadString('URL_H', 'CancelaNFSe'       , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomGerarNFSe          := StringReplace(FPIniParams.ReadString('URL_H', 'GerarNFSe'         , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomRecepcaoSincrono   := StringReplace(FPIniParams.ReadString('URL_H', 'RecepcaoSincrono'  , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomSubstituiNFSe      := StringReplace(FPIniParams.ReadString('URL_H', 'SubstituiNFSe'     , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomAbrirSessao        := StringReplace(FPIniParams.ReadString('URL_H', 'AbrirSessao'       , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
      FConfigURL.HomFecharSessao       := StringReplace(FPIniParams.ReadString('URL_H', 'FecharSessao'      , FConfigURL.HomRecepcaoLoteRPS), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);
    end;
  end;

  if FPIniParams.ReadString('URL_P', 'RecepcaoLoteRPS', '') = '*******' then
  begin
    FConfigURL.ProRecepcaoLoteRPS    := StringReplace(FPIniParams.ReadString('URL_P', 'RecepcaoLoteRPS_' + CodIBGE   , ''                           ), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProConsultaSitLoteRPS := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaSitLoteRPS_' + CodIBGE, FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProConsultaLoteRPS    := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaLoteRPS_' + CodIBGE   , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProConsultaNFSeRPS    := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaNFSeRPS_' + CodIBGE   , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProConsultaNFSe       := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaNFSe_' + CodIBGE      , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProCancelaNFSe        := StringReplace(FPIniParams.ReadString('URL_P', 'CancelaNFSe_' + CodIBGE       , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProGerarNFSe          := StringReplace(FPIniParams.ReadString('URL_P', 'GerarNFSe_' + CodIBGE         , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProRecepcaoSincrono   := StringReplace(FPIniParams.ReadString('URL_P', 'RecepcaoSincrono_' + CodIBGE  , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProSubstituiNFSe      := StringReplace(FPIniParams.ReadString('URL_P', 'SubstituiNFSe_' + CodIBGE     , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProAbrirSessao        := StringReplace(FPIniParams.ReadString('URL_P', 'AbrirSessao_' + CodIBGE       , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    FConfigURL.ProFecharSessao       := StringReplace(FPIniParams.ReadString('URL_P', 'FecharSessao_' + CodIBGE      , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
  end
  else
  begin
    if FCodigoMunicipio = 5218805 then
    begin
      FConfigURL.ProRecepcaoLoteRPS    := FxNomeURL_P;
      FConfigURL.ProConsultaSitLoteRPS := FxNomeURL_P;
      FConfigURL.ProConsultaLoteRPS    := FxNomeURL_P;
      FConfigURL.ProConsultaNFSeRPS    := FxNomeURL_P;
      FConfigURL.ProConsultaNFSe       := FxNomeURL_P;
      FConfigURL.ProCancelaNFSe        := FxNomeURL_P;
      FConfigURL.ProGerarNFSe          := FxNomeURL_P;
      FConfigURL.ProRecepcaoSincrono   := FxNomeURL_P;
      FConfigURL.ProSubstituiNFSe      := FxNomeURL_P;
      FConfigURL.ProAbrirSessao        := FxNomeURL_P;
      FConfigURL.ProFecharSessao       := FxNomeURL_P;
    end
    else
    begin
      FConfigURL.ProRecepcaoLoteRPS    := StringReplace(FPIniParams.ReadString('URL_P', 'RecepcaoLoteRPS'   , ''                           ), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProConsultaSitLoteRPS := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaSitLoteRPS', FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProConsultaLoteRPS    := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaLoteRPS'   , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProConsultaNFSeRPS    := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaNFSeRPS'   , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProConsultaNFSe       := StringReplace(FPIniParams.ReadString('URL_P', 'ConsultaNFSe'      , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProCancelaNFSe        := StringReplace(FPIniParams.ReadString('URL_P', 'CancelaNFSe'       , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProGerarNFSe          := StringReplace(FPIniParams.ReadString('URL_P', 'GerarNFSe'         , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProRecepcaoSincrono   := StringReplace(FPIniParams.ReadString('URL_P', 'RecepcaoSincrono'  , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProSubstituiNFSe      := StringReplace(FPIniParams.ReadString('URL_P', 'SubstituiNFSe'     , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProAbrirSessao        := StringReplace(FPIniParams.ReadString('URL_P', 'AbrirSessao'       , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
      FConfigURL.ProFecharSessao       := StringReplace(FPIniParams.ReadString('URL_P', 'FecharSessao'      , FConfigURL.ProRecepcaoLoteRPS), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);
    end;
  end;

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('CabecalhoMsg', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.CabecalhoMsg := Texto;

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('Recepcionar', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.Recepcionar.Envelope := Texto;

  FConfigEnvelope.Recepcionar.IncluiEncodingCab := FPIniParams.ReadBool('Recepcionar', 'IncluiEncodingCab', False);
  FConfigEnvelope.Recepcionar.IncluiEncodingDados := FPIniParams.ReadBool('Recepcionar', 'IncluiEncodingDados', False);
  FConfigEnvelope.Recepcionar.CabecalhoStr := FPIniParams.ReadBool('Recepcionar', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.Recepcionar.DadosStr := FPIniParams.ReadBool('Recepcionar', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.Recepcionar.TagGrupo := FPIniParams.ReadString('Recepcionar', 'TagGrupo', 'EnviarLoteRpsEnvio');
  FConfigEnvelope.Recepcionar.TagElemento := FPIniParams.ReadString('Recepcionar', 'TagElemento', 'LoteRps');
  FConfigEnvelope.Recepcionar.DocElemento := FPIniParams.ReadString('Recepcionar', 'DocElemento', '');
  FConfigEnvelope.Recepcionar.InfElemento := FPIniParams.ReadString('Recepcionar', 'InfElemento', '');

  if (FProvedor = proNotaBlu) Then
  begin
    Texto := '';
    I := 1;
    while true do
    begin
      sCampo := 'Texto' + IntToStr(I);
      sFim   := FPIniParams.ReadString('Teste', sCampo, 'FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break;
      Texto := Texto + sFim;
      Inc(I);
    end;
    FConfigEnvelope.Teste.Envelope := Texto;

    FConfigEnvelope.Teste.IncluiEncodingCab := FPIniParams.ReadBool('Teste', 'IncluiEncodingCab', False);
    FConfigEnvelope.Teste.IncluiEncodingDados := FPIniParams.ReadBool('Teste', 'IncluiEncodingDados', False);
    FConfigEnvelope.Teste.CabecalhoStr := FPIniParams.ReadBool('Teste', 'CabecalhoStr', FConfigXML.CabecalhoStr);
    FConfigEnvelope.Teste.DadosStr := FPIniParams.ReadBool('Teste', 'DadosStr', FConfigXML.DadosStr);
    FConfigEnvelope.Teste.TagGrupo := FPIniParams.ReadString('Teste', 'TagGrupo', 'EnviarLoteRpsEnvio');
    FConfigEnvelope.Teste.TagElemento := FPIniParams.ReadString('Teste', 'TagElemento', 'LoteRps');
    FConfigEnvelope.Teste.DocElemento := FPIniParams.ReadString('Teste', 'DocElemento', '');
    FConfigEnvelope.Teste.InfElemento := FPIniParams.ReadString('Teste', 'InfElemento', '');
  end;

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('ConsSit', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.ConsSit.Envelope := Texto;

  FConfigEnvelope.ConsSit.IncluiEncodingCab := FPIniParams.ReadBool('ConsSit', 'IncluiEncodingCab', False);
  FConfigEnvelope.ConsSit.IncluiEncodingDados := FPIniParams.ReadBool('ConsSit', 'IncluiEncodingDados', False);
  FConfigEnvelope.ConsSit.CabecalhoStr := FPIniParams.ReadBool('ConsSit', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.ConsSit.DadosStr := FPIniParams.ReadBool('ConsSit', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.ConsSit.TagGrupo := FPIniParams.ReadString('ConsSit', 'TagGrupo', 'ConsultarSituacaoLoteRpsEnvio');
  FConfigEnvelope.ConsSit.TagElemento := FPIniParams.ReadString('ConsSit', 'TagElemento', '');
  FConfigEnvelope.ConsSit.DocElemento := FPIniParams.ReadString('ConsSit', 'DocElemento', 'ConsultarSituacaoLoteRpsEnvio');
  FConfigEnvelope.ConsSit.InfElemento := FPIniParams.ReadString('ConsSit', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('ConsLote', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.ConsLote.Envelope := Texto;

  FConfigEnvelope.ConsLote.IncluiEncodingCab := FPIniParams.ReadBool('ConsLote', 'IncluiEncodingCab', False);
  FConfigEnvelope.ConsLote.IncluiEncodingDados := FPIniParams.ReadBool('ConsLote', 'IncluiEncodingDados', False);
  FConfigEnvelope.ConsLote.CabecalhoStr := FPIniParams.ReadBool('ConsLote', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.ConsLote.DadosStr := FPIniParams.ReadBool('ConsLote', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.ConsLote.TagGrupo := FPIniParams.ReadString('ConsLote', 'TagGrupo', 'ConsultarLoteRpsEnvio');
  FConfigEnvelope.ConsLote.TagElemento := FPIniParams.ReadString('ConsLote', 'TagElemento', '');
  FConfigEnvelope.ConsLote.DocElemento := FPIniParams.ReadString('ConsLote', 'DocElemento', '');
  FConfigEnvelope.ConsLote.InfElemento := FPIniParams.ReadString('ConsLote', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('ConsNFSeRps', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.ConsNFSeRps.Envelope := Texto;

  FConfigEnvelope.ConsNFSeRps.IncluiEncodingCab := FPIniParams.ReadBool('ConsNFSeRps', 'IncluiEncodingCab', False);
  FConfigEnvelope.ConsNFSeRps.IncluiEncodingDados := FPIniParams.ReadBool('ConsNFSeRps', 'IncluiEncodingDados', False);
  FConfigEnvelope.ConsNFSeRps.CabecalhoStr := FPIniParams.ReadBool('ConsNFSeRps', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.ConsNFSeRps.DadosStr := FPIniParams.ReadBool('ConsNFSeRps', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.ConsNFSeRps.TagGrupo := FPIniParams.ReadString('ConsNFSeRps', 'TagGrupo', 'ConsultarNfseRpsEnvio');
  FConfigEnvelope.ConsNFSeRps.TagElemento := FPIniParams.ReadString('ConsNFSeRps', 'TagElemento', '');
  FConfigEnvelope.ConsNFSeRps.DocElemento := FPIniParams.ReadString('ConsNFSeRps', 'DocElemento', '');
  FConfigEnvelope.ConsNFSeRps.InfElemento := FPIniParams.ReadString('ConsNFSeRps', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('ConsNFSe', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.ConsNFSe.Envelope := Texto;

  FConfigEnvelope.ConsNFSe.IncluiEncodingCab := FPIniParams.ReadBool('ConsNFSe', 'IncluiEncodingCab', False);
  FConfigEnvelope.ConsNFSe.IncluiEncodingDados := FPIniParams.ReadBool('ConsNFSe', 'IncluiEncodingDados', False);
  FConfigEnvelope.ConsNFSe.CabecalhoStr := FPIniParams.ReadBool('ConsNFSe', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.ConsNFSe.DadosStr := FPIniParams.ReadBool('ConsNFSe', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.ConsNFSe.TagGrupo := FPIniParams.ReadString('ConsNFSe', 'TagGrupo', 'ConsultarNfseEnvio');
  FConfigEnvelope.ConsNFSe.TagElemento := FPIniParams.ReadString('ConsNFSe', 'TagElemento', '');
  FConfigEnvelope.ConsNFSe.DocElemento := FPIniParams.ReadString('ConsNFSe', 'DocElemento', '');
  FConfigEnvelope.ConsNFSe.InfElemento := FPIniParams.ReadString('ConsNFSe', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('Cancelar', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.Cancelar.Envelope := Texto;

  FConfigEnvelope.Cancelar.IncluiEncodingCab := FPIniParams.ReadBool('Cancelar', 'IncluiEncodingCab', False);
  FConfigEnvelope.Cancelar.IncluiEncodingDados := FPIniParams.ReadBool('Cancelar', 'IncluiEncodingDados', False);
  FConfigEnvelope.Cancelar.CabecalhoStr := FPIniParams.ReadBool('Cancelar', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.Cancelar.DadosStr := FPIniParams.ReadBool('Cancelar', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.Cancelar.TagGrupo := FPIniParams.ReadString('Cancelar', 'TagGrupo', 'CancelarNfseEnvio');
  FConfigEnvelope.Cancelar.TagElemento := FPIniParams.ReadString('Cancelar', 'TagElemento', '');
  FConfigEnvelope.Cancelar.DocElemento := FPIniParams.ReadString('Cancelar', 'DocElemento', 'Pedido');
  FConfigEnvelope.Cancelar.InfElemento := FPIniParams.ReadString('Cancelar', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('Gerar', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.Gerar.Envelope := Texto;

  FConfigEnvelope.Gerar.IncluiEncodingCab := FPIniParams.ReadBool('Gerar', 'IncluiEncodingCab', False);
  FConfigEnvelope.Gerar.IncluiEncodingDados := FPIniParams.ReadBool('Gerar', 'IncluiEncodingDados', False);
  FConfigEnvelope.Gerar.CabecalhoStr := FPIniParams.ReadBool('Gerar', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.Gerar.DadosStr := FPIniParams.ReadBool('Gerar', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.Gerar.TagGrupo := FPIniParams.ReadString('Gerar', 'TagGrupo', 'GerarNfseEnvio');
  FConfigEnvelope.Gerar.TagElemento := FPIniParams.ReadString('Gerar', 'TagElemento', 'Rps');
  FConfigEnvelope.Gerar.DocElemento := FPIniParams.ReadString('Gerar', 'DocElemento', '');
  FConfigEnvelope.Gerar.InfElemento := FPIniParams.ReadString('Gerar', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('RecSincrono', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.RecSincrono.Envelope := Texto;

  FConfigEnvelope.RecSincrono.IncluiEncodingCab := FPIniParams.ReadBool('RecSincrono', 'IncluiEncodingCab', False);
  FConfigEnvelope.RecSincrono.IncluiEncodingDados := FPIniParams.ReadBool('RecSincrono', 'IncluiEncodingDados', False);
  FConfigEnvelope.RecSincrono.CabecalhoStr := FPIniParams.ReadBool('RecSincrono', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.RecSincrono.DadosStr := FPIniParams.ReadBool('RecSincrono', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.RecSincrono.TagGrupo := FPIniParams.ReadString('RecSincrono', 'TagGrupo', 'EnviarLoteRpsSincronoEnvio');
  FConfigEnvelope.RecSincrono.TagElemento := FPIniParams.ReadString('RecSincrono', 'TagElemento', 'LoteRps');
  FConfigEnvelope.RecSincrono.DocElemento := FPIniParams.ReadString('RecSincrono', 'DocElemento', '');
  FConfigEnvelope.RecSincrono.InfElemento := FPIniParams.ReadString('RecSincrono', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('Substituir', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.Substituir.Envelope := Texto;

  FConfigEnvelope.Substituir.IncluiEncodingCab := FPIniParams.ReadBool('Substituir', 'IncluiEncodingCab', False);
  FConfigEnvelope.Substituir.IncluiEncodingDados := FPIniParams.ReadBool('Substituir', 'IncluiEncodingDados', False);
  FConfigEnvelope.Substituir.CabecalhoStr := FPIniParams.ReadBool('Substituir', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.Substituir.DadosStr := FPIniParams.ReadBool('Substituir', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.Substituir.TagGrupo := FPIniParams.ReadString('Substituir', 'TagGrupo', 'SubstituirNfseEnvio');
  FConfigEnvelope.Substituir.TagElemento := FPIniParams.ReadString('Substituir', 'TagElemento', '');
  FConfigEnvelope.Substituir.DocElemento := FPIniParams.ReadString('Substituir', 'DocElemento', 'Pedido');
  FConfigEnvelope.Substituir.InfElemento := FPIniParams.ReadString('Substituir', 'InfElemento', 'InfPedidoCancelamento');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('AbrirSessao', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.AbrirSessao.Envelope := Texto;

  FConfigEnvelope.AbrirSessao.IncluiEncodingCab := FPIniParams.ReadBool('AbrirSessao', 'IncluiEncodingCab', False);
  FConfigEnvelope.AbrirSessao.IncluiEncodingDados := FPIniParams.ReadBool('AbrirSessao', 'IncluiEncodingDados', False);
  FConfigEnvelope.AbrirSessao.CabecalhoStr := FPIniParams.ReadBool('AbrirSessao', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.AbrirSessao.DadosStr := FPIniParams.ReadBool('AbrirSessao', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.AbrirSessao.TagGrupo := FPIniParams.ReadString('AbrirSessao', 'TagGrupo', '');
  FConfigEnvelope.AbrirSessao.TagElemento := FPIniParams.ReadString('AbrirSessao', 'TagElemento', '');
  FConfigEnvelope.AbrirSessao.DocElemento := FPIniParams.ReadString('AbrirSessao', 'DocElemento', '');
  FConfigEnvelope.AbrirSessao.InfElemento := FPIniParams.ReadString('AbrirSessao', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('FecharSessao', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigEnvelope.FecharSessao.Envelope := Texto;

  FConfigEnvelope.FecharSessao.IncluiEncodingCab := FPIniParams.ReadBool('FecharSessao', 'IncluiEncodingCab', False);
  FConfigEnvelope.FecharSessao.IncluiEncodingDados := FPIniParams.ReadBool('FecharSessao', 'IncluiEncodingDados', False);
  FConfigEnvelope.FecharSessao.CabecalhoStr := FPIniParams.ReadBool('FecharSessao', 'CabecalhoStr', FConfigXML.CabecalhoStr);
  FConfigEnvelope.FecharSessao.DadosStr := FPIniParams.ReadBool('FecharSessao', 'DadosStr', FConfigXML.DadosStr);
  FConfigEnvelope.FecharSessao.TagGrupo := FPIniParams.ReadString('FecharSessao', 'TagGrupo', '');
  FConfigEnvelope.FecharSessao.TagElemento := FPIniParams.ReadString('FecharSessao', 'TagElemento', '');
  FConfigEnvelope.FecharSessao.DocElemento := FPIniParams.ReadString('FecharSessao', 'DocElemento', '');
  FConfigEnvelope.FecharSessao.InfElemento := FPIniParams.ReadString('FecharSessao', 'InfElemento', '');

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('RetornoNFSe', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigGeral.RetornoNFSe := Texto;

  if FPIniParams.ReadString('LinkNFSe', 'Producao', '') = '*******' then
    FConfigGeral.ProLinkNFSe := StringReplace(FPIniParams.ReadString('LinkNFSe', 'Producao_' + CodIBGE, ''), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll])
  else
    FConfigGeral.ProLinkNFSe := StringReplace(FPIniParams.ReadString('LinkNFSe', 'Producao', ''), '%NomeURL_P%', FxNomeURL_P, [rfReplaceAll]);

  if FPIniParams.ReadString('LinkNFSe', 'Homologacao', '') = '*******' then
    FConfigGeral.HomLinkNFSe := StringReplace(FPIniParams.ReadString('LinkNFSe', 'Homologacao_' + CodIBGE, ''), '%NomeURL_H%', FxNomeURL_P, [rfReplaceAll])
  else
    FConfigGeral.HomLinkNFSe := StringReplace(FPIniParams.ReadString('LinkNFSe', 'Homologacao', ''), '%NomeURL_H%', FxNomeURL_H, [rfReplaceAll]);

  Texto := '';
  I := 1;
  while true do
  begin
    sCampo := 'Texto' + IntToStr(I);
    sFim   := FPIniParams.ReadString('DadosSenha', sCampo, 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;
    Texto := Texto + sFim;
    Inc(I);
  end;
  FConfigGeral.DadosSenha := Texto;

  FConfigGrupoMsgRet.GrupoMsg    := FPIniParams.ReadString('GrupoMsgRet', 'GrupoMsg'   , '');
  FConfigGrupoMsgRet.Recepcionar := FPIniParams.ReadString('GrupoMsgRet', 'Recepcionar', '');
  FConfigGrupoMsgRet.ConsSit     := FPIniParams.ReadString('GrupoMsgRet', 'ConsSit'    , '');
  FConfigGrupoMsgRet.ConsLote    := FPIniParams.ReadString('GrupoMsgRet', 'ConsLote'   , '');
  FConfigGrupoMsgRet.ConsNFSeRPS := FPIniParams.ReadString('GrupoMsgRet', 'ConsNFSeRPS', '');
  FConfigGrupoMsgRet.ConsNFSe    := FPIniParams.ReadString('GrupoMsgRet', 'ConsNFSe'   , '');
  FConfigGrupoMsgRet.Cancelar    := FPIniParams.ReadString('GrupoMsgRet', 'Cancelar'   , '');
  FConfigGrupoMsgRet.Gerar       := FPIniParams.ReadString('GrupoMsgRet', 'Gerar'      , '');
  FConfigGrupoMsgRet.RecSincrono := FPIniParams.ReadString('GrupoMsgRet', 'RecSincrono', '');
  FConfigGrupoMsgRet.Substituir  := FPIniParams.ReadString('GrupoMsgRet', 'Substituir' , '');
  FConfigGrupoMsgRet.AbrirSessao  := FPIniParams.ReadString('GrupoMsgRet', 'AbrirSessao' , '');
  FConfigGrupoMsgRet.FecharSessao := FPIniParams.ReadString('GrupoMsgRet', 'FecharSessao', '');

  FPIniParams.Free;
end;

{ TArquivosConfNFSe }

constructor TArquivosConfNFSe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathNFSe := False;
  FSalvarApenasNFSeProcessadas := False;
  FPathGer := '';
  FPathRPS := '';
  FPathNFSe := '';
  FPathCan := '';
  FNomeLongoNFSe := False;
  FTabServicosExt := False;
end;

procedure TArquivosConfNFSe.Assign(DeArquivosConfNFSe: TArquivosConfNFSe);
begin
  inherited Assign(DeArquivosConfNFSe);

  EmissaoPathNFSe := DeArquivosConfNFSe.EmissaoPathNFSe;
  SalvarApenasNFSeProcessadas := DeArquivosConfNFSe.SalvarApenasNFSeProcessadas;
  PathGer := DeArquivosConfNFSe.PathGer;
  PathRPS := DeArquivosConfNFSe.PathRPS;
  PathNFSe := DeArquivosConfNFSe.PathNFSe;
  PathCan := DeArquivosConfNFSe.PathCan;
  NomeLongoNFSe := DeArquivosConfNFSe.NomeLongoNFSe;
  TabServicosExt := DeArquivosConfNFSe.TabServicosExt;
end;

function TArquivosConfNFSe.GetPathGer(Data: TDateTime;
  CNPJ: String): String;
begin
  Result := GetPath(FPathGer, 'NFSe', CNPJ, Data);
end;

function TArquivosConfNFSe.GetPathRPS(Data: TDateTime;
  CNPJ: String): String;
var
  Dir: String;
begin
  if FPathRPS <> '' then
    Result := GetPath(FPathRPS, 'Recibos', CNPJ, Data)
  else
  begin
    Dir := GetPath(FPathGer, 'NFSe', CNPJ, Data);

    Dir := PathWithDelim(Dir) + 'Recibos';

    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    Result := Dir;
  end;
end;

function TArquivosConfNFSe.GetPathNFSe(Data: TDateTime = 0;
  CNPJ: String = ''): String;
var
  Dir: String;
begin
  if FPathNFSe <> '' then
    Result := GetPath(FPathNFSe, 'Notas', CNPJ, Data)
  else
  begin
    Dir := GetPath(FPathGer, 'NFSe', CNPJ, Data);

    Dir := PathWithDelim(Dir) + 'Notas';

    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    Result := Dir;
  end;
end;

function TArquivosConfNFSe.GetPathCan(Data: TDateTime = 0;
  CNPJ: String = ''): String;
var
  Dir: String;
begin
  if FPathCan <> '' then
    Result := GetPath(FPathCan, 'Can', CNPJ, Data)
  else
  begin
    Dir := GetPath(FPathGer, 'NFSe', CNPJ, Data);

    Dir := PathWithDelim(Dir) + 'Can';

    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    Result := Dir;
  end;
end;

{ TDadosSenhaParamsCollection }

function TDadosSenhaParamsCollection.Add: TDadosSenhaParamsCollectionItem;
begin
  Result := TDadosSenhaParamsCollectionItem(inherited Add);
end;

constructor TDadosSenhaParamsCollection.Create(
  AOwner: TEmitenteConfNFSe);
begin
  inherited Create(TDadosSenhaParamsCollectionItem);
end;

function TDadosSenhaParamsCollection.GetItem(
  Index: Integer): TDadosSenhaParamsCollectionItem;
begin
  Result := TDadosSenhaParamsCollectionItem(inherited GetItem(Index));
end;

procedure TDadosSenhaParamsCollection.SetItem(Index: Integer;
  const Value: TDadosSenhaParamsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TConfigEnvelope }

constructor TConfigEnvelope.Create;
begin
  FRecepcionar  := TParamEnvelope.Create;
  FTeste        := TParamEnvelope.Create;
  FConsSit      := TParamEnvelope.Create;
  FConsLote     := TParamEnvelope.Create;
  FConsNFSeRps  := TParamEnvelope.Create;
  FConsNFSe     := TParamEnvelope.Create;
  FCancelar     := TParamEnvelope.Create;
  FGerar        := TParamEnvelope.Create;
  FRecSincrono  := TParamEnvelope.Create;
  FSubstituir   := TParamEnvelope.Create;
  FAbrirSessao  := TParamEnvelope.Create;
  FFecharSessao := TParamEnvelope.Create;
end;

destructor TConfigEnvelope.Destroy;
begin
  FRecepcionar.Free;
  FTeste.Free;
  FConsSit.Free;
  FConsLote.Free;
  FConsNFSeRps.Free;
  FConsNFSe.Free;
  FCancelar.Free;
  FGerar.Free;
  FRecSincrono.Free;
  FSubstituir.Free;
  FAbrirSessao.Free;
  FFecharSessao.Free;

  inherited;
end;

end.
