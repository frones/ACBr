{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFSeXConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, ACBrNFSeXConversao;

type

  { TDadosEmitente }

 TDadosEmitente = class(TPersistent)
 private
   FNomeFantasia: String;
   FInscricaoEstadual: String;
   FEndereco: String;
   FNumero: String;
   FCEP: String;
   FBairro: String;
   FComplemento: String;
   FMunicipio: String;
   FUF: String;
   FCodigoMunicipio: String;
   FTelefone: String;
   FEmail: String;

 public
   Constructor Create;
   procedure Assign(Source: TPersistent); override;

 published
   property NomeFantasia: String read FNomeFantasia write FNomeFantasia;
   property InscricaoEstadual: String read FInscricaoEstadual write FInscricaoEstadual;
   property Endereco: String read FEndereco write FEndereco;
   property Numero: String read FNumero write FNumero;
   property CEP: String read FCEP write FCEP;
   property Bairro: String read FBairro write FBairro;
   property Complemento: String read FComplemento write FComplemento;
   property Municipio: String read FMunicipio write FMunicipio;
   property UF: String read FUF write FUF;
   property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
   property Telefone: String read FTelefone write FTelefone;
   property Email: String read FEmail write FEmail;
 end;

 { TEmitenteConfNFSe }

 TEmitenteConfNFSe = class(TPersistent)
 private
   FCNPJ: String;
   FInscMun: String;
   FRazSocial: String;
   FWSUser: String;
   FWSSenha: String;
   FWSFraseSecr: String;
   FWSChaveAcesso: String;
   FWSChaveAutoriz: String;
   FDadosEmitente: TDadosEmitente;

 public
   Constructor Create;
   destructor Destroy; override;
   procedure Assign(Source: TPersistent); override;

 published
   property CNPJ: String           read FCNPJ           write FCNPJ;
   property InscMun: String        read FInscMun        write FInscMun;
   property RazSocial: String      read FRazSocial      write FRazSocial;
   property WSUser: String         read FWSUser         write FWSUser;
   property WSSenha: String        read FWSSenha        write FWSSenha;
   property WSFraseSecr: String    read FWSFraseSecr    write FWSFraseSecr;
   property WSChaveAcesso: String  read FWSChaveAcesso  write FWSChaveAcesso;
   property WSChaveAutoriz: String read FWSChaveAutoriz write FWSChaveAutoriz;

   property DadosEmitente: TDadosEmitente read FDadosEmitente write FDadosEmitente;
 end;

  { TAutenticacao }

  TAutenticacao = Class
  private
    FRequerCertificado: Boolean;
    FRequerLogin: Boolean;
    FRequerChaveAcesso: Boolean;
    FRequerChaveAutorizacao: Boolean;
    FRequerFraseSecreta: Boolean;
  public
    property RequerCertificado: Boolean read FRequerCertificado write FRequerCertificado;
    property RequerLogin: Boolean read FRequerLogin write FRequerLogin;
    property RequerChaveAcesso: Boolean read FRequerChaveAcesso write FRequerChaveAcesso;
    property RequerChaveAutorizacao: Boolean read FRequerChaveAutorizacao write FRequerChaveAutorizacao;
    property RequerFraseSecreta: Boolean read FRequerFraseSecreta write FRequerFraseSecreta;
  end;

  { TServicosDispobilizados }

  TServicosDispobilizados = Class
  private
    FEnviarLoteAssincrono: Boolean;
    FEnviarLoteSincrono: Boolean;
    FEnviarUnitario: Boolean;
    FConsultarSituacao: Boolean;
    FConsultarLote: Boolean;
    FConsultarRps: Boolean;
    FConsultarNfse: Boolean;
    FConsultarFaixaNfse: Boolean;
    FConsultarServicoPrestado: Boolean;
    FConsultarServicoTomado: Boolean;
    FCancelarNfse: Boolean;
    FSubstituirNfse: Boolean;
    FGerarToken: Boolean;
    FEnviarEvento: Boolean;
    FConsultarEvento: Boolean;
    FConsultarDFe: Boolean;
    FConsultarParam: Boolean;
    FConsultarSeqRps: Boolean;
    FConsultarLinkNfse: Boolean;
    FConsultarNfseChave: Boolean;
    FTestarEnvio: Boolean;
  public
    property EnviarLoteAssincrono: Boolean read FEnviarLoteAssincrono write FEnviarLoteAssincrono;
    property EnviarLoteSincrono: Boolean read FEnviarLoteSincrono write FEnviarLoteSincrono;
    property EnviarUnitario: Boolean read FEnviarUnitario write FEnviarUnitario;
    property ConsultarSituacao: Boolean read FConsultarSituacao write FConsultarSituacao;
    property ConsultarLote: Boolean read FConsultarLote write FConsultarLote;
    property ConsultarRps: Boolean read FConsultarRps write FConsultarRps;
    property ConsultarNfse: Boolean read FConsultarNfse write FConsultarNfse;
    property ConsultarFaixaNfse: Boolean read FConsultarFaixaNfse write FConsultarFaixaNfse;
    property ConsultarServicoPrestado: Boolean read FConsultarServicoPrestado write FConsultarServicoPrestado;
    property ConsultarServicoTomado: Boolean read FConsultarServicoTomado write FConsultarServicoTomado;
    property CancelarNfse: Boolean read FCancelarNfse write FCancelarNfse;
    property SubstituirNfse: Boolean read FSubstituirNfse write FSubstituirNfse;
    property GerarToken: Boolean read FGerarToken write FGerarToken;
    property EnviarEvento: Boolean read FEnviarEvento write FEnviarEvento;
    property ConsultarEvento: Boolean read FConsultarEvento write FConsultarEvento;
    property ConsultarDFe: Boolean read FConsultarDFe write FConsultarDFe;
    property ConsultarParam: Boolean read FConsultarParam write FConsultarParam;
    property ConsultarSeqRps: Boolean read FConsultarSeqRps write FConsultarSeqRps;
    property ConsultarLinkNfse: Boolean read FConsultarLinkNfse write FConsultarLinkNfse;
    property ConsultarNfseChave: Boolean read FConsultarNfseChave write FConsultarNfseChave;
    property TestarEnvio: Boolean read FTestarEnvio write FTestarEnvio;
  end;

  { TGeralConfNFSe }

  TGeralConfNFSe = class(TGeralConf)
  private
    FPIniParams: TMemIniFile;

    FCodigoMunicipio: Integer;
    FProvedor: TnfseProvedor;
    FVersao: TVersaoNFSe;
    FxProvedor: String;
    FxMunicipio: String;
    FxUF: String;
    FCNPJPrefeitura: String;
    FConsultaLoteAposEnvio: Boolean;
    FConsultaAposCancelar: Boolean;
    FEmitente: TEmitenteConfNFSe;
    FMontarPathSchema: Boolean;
    FLayout: TLayout;
    FLayoutNFSe: TLayoutNFSe;
    FAssinaturas: TAssinaturas;
    FAutenticacao: TAutenticacao;
    FServicosDisponibilizados: TServicosDispobilizados;
    FFormDiscriminacao: TFormatoDiscriminacao;

    procedure SetCodigoMunicipio(const Value: Integer);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;

    procedure Assign(DeGeralConfNFSe: TGeralConfNFSe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;
    procedure LerParamsMunicipio;

  published
    property CodigoMunicipio: Integer read FCodigoMunicipio write SetCodigoMunicipio;
    property Provedor: TnfseProvedor read FProvedor write FProvedor;
    property Versao: TVersaoNFSe read FVersao write FVersao;
    property xProvedor: String read FxProvedor;
    property xMunicipio: String read FxMunicipio;
    property xUF: String read FxUF;
    property CNPJPrefeitura: String read FCNPJPrefeitura write FCNPJPrefeitura;

    property ConsultaLoteAposEnvio: Boolean read FConsultaLoteAposEnvio write FConsultaLoteAposEnvio default True;
    property ConsultaAposCancelar: Boolean  read FConsultaAposCancelar  write FConsultaAposCancelar default True;

    property Emitente: TEmitenteConfNFSe read FEmitente write FEmitente;
    property MontarPathSchema: Boolean read FMontarPathSchema
      write FMontarPathSchema default True;
    property Layout: TLayout read FLayout;
    property LayoutNFSe: TLayoutNFSe read FLayoutNFSe write FLayoutNFSe default lnfsProvedor;
    property Assinaturas: TAssinaturas read FAssinaturas write FAssinaturas default taConfigProvedor;
    property PIniParams: TMemIniFile read FPIniParams;
    property Autenticacao: TAutenticacao read FAutenticacao;
    property ServicosDisponibilizados: TServicosDispobilizados read FServicosDisponibilizados;
    property FormatoDiscriminacao: TFormatoDiscriminacao read FFormDiscriminacao write FFormDiscriminacao default fdNenhum;
  end;

  { TArquivosConfNFSe }

  TArquivosConfNFSe = class(TArquivosConf)
  private
    FEmissaoPathNFSe: boolean;
    FPathGer: String;
    FPathRPS: String;
    FPathNFSe: String;
    FPathCan: String;
    FNomeLongoNFSe: Boolean;
    FTabServicosExt: Boolean;
    FIniTabServicos: String;

    procedure SetTabServicosExt(const Value: Boolean);
    function GetIniTabServicos: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeArquivosConfNFSe: TArquivosConfNFSe); reintroduce;

    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathGer(Data: TDateTime = 0; const CNPJ: String = '';
      const IE: String = ''): String;

    function GetPathRPS(Data: TDateTime = 0; const CNPJ: String = '';
      const IE: String = ''): String;

    function GetPathEvento(Data: TDateTime = 0; const CNPJ: String = '';
      const IE: String = ''): String;

    function GetPathNFSe(Data: TDateTime = 0; const CNPJ: String = '';
      const IE: String = ''): String;

    function GetPathCan(Data: TDateTime = 0; const CNPJ: String = '';
      const IE: String = ''): String;
  published
    property EmissaoPathNFSe: boolean read FEmissaoPathNFSe
      write FEmissaoPathNFSe default False;
    property PathGer: String read FPathGer write FPathGer;
    property PathRPS: String read FPathRPS  write FPathRPS;
    property PathNFSe: String read FPathNFSe write FPathNFSe;
    property PathCan: String read FPathCan write FPathCan;
    property NomeLongoNFSe: Boolean read FNomeLongoNFSe
      write FNomeLongoNFSe default False;
    property TabServicosExt: Boolean read FTabServicosExt
      write SetTabServicosExt default False;
    property IniTabServicos: String read GetIniTabServicos write FIniTabServicos;
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
  ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrNFSeX, ACBrDFeException;

{ TEmitenteConfNFSe }

constructor TEmitenteConfNFSe.Create;
begin
  inherited Create;

  FCNPJ           := '';
  FInscMun        := '';
  FRazSocial      := '';
  FWSUser         := '';
  FWSSenha        := '';
  FWSFraseSecr    := '';
  FWSChaveAcesso  := '';
  FWSChaveAutoriz := '';

  FDadosEmitente := TDadosEmitente.Create;
end;

procedure TEmitenteConfNFSe.Assign(Source: TPersistent);
begin
  if Source is TEmitenteConfNFSe then
  begin
    FCNPJ           := TEmitenteConfNFSe(Source).CNPJ;
    FInscMun        := TEmitenteConfNFSe(Source).InscMun;
    FRazSocial      := TEmitenteConfNFSe(Source).RazSocial;
    FWSUser         := TEmitenteConfNFSe(Source).WSUser;
    FWSSenha        := TEmitenteConfNFSe(Source).WSSenha;
    FWSFraseSecr    := TEmitenteConfNFSe(Source).WSFraseSecr;
    FWSChaveAcesso  := TEmitenteConfNFSe(Source).WSChaveAcesso;
    FWSChaveAutoriz := TEmitenteConfNFSe(Source).WSChaveAutoriz;

    FDadosEmitente.Assign(TEmitenteConfNFSe(Source).DadosEmitente);
  end
  else
    inherited Assign(Source);
end;

destructor TEmitenteConfNFSe.Destroy;
begin
  FDadosEmitente.Free;

  inherited;
end;

{ TConfiguracoesNFSe }

constructor TConfiguracoesNFSe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'NFSe';
  WebServices.ResourceName := 'ACBrNFSeXServicos';
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

  FPIniParams := TMemIniFile.Create('');
  FProvedor := proNenhum;
  FVersao := ve100;

  FConsultaLoteAposEnvio := True;
  FConsultaAposCancelar := True;
  FMontarPathSchema := True;
  FLayoutNFSe := lnfsProvedor;
  FAssinaturas := taConfigProvedor;
  FFormDiscriminacao := fdNenhum;

  FAutenticacao := TAutenticacao.Create;
  FServicosDisponibilizados := TServicosDispobilizados.Create;
end;

destructor TGeralConfNFSe.Destroy;
begin
  FEmitente.Free;
  FPIniParams.Free;
  FAutenticacao.Free;
  FServicosDisponibilizados.Free;

  inherited;
end;

procedure TGeralConfNFSe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'CodigoMunicipio', CodigoMunicipio);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'CNPJPrefeitura', CNPJPrefeitura);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'ConsultaLoteAposEnvio', ConsultaLoteAposEnvio);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'ConsultaAposCancelar', ConsultaAposCancelar);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'MontarPathSchema', MontarPathSchema);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'LayoutNFSe', Integer(LayoutNFSe));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'Assinaturas', Integer(Assinaturas));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'FormatoDiscriminacao', Integer(FormatoDiscriminacao));

  // Emitente
  with Emitente do
  begin
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.CNPJ', CNPJ);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.InscMun', InscMun);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.RazSocial', RazSocial);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.WSUser', WSUser);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.WSSenha', WSSenha);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.WSFraseSecr', WSFraseSecr);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.WSChaveAcesso', WSChaveAcesso);
    AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.WSChaveAutoriz', WSChaveAutoriz);
    // Dados do Emitente
    with DadosEmitente do
    begin
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.NomeFantasia', NomeFantasia);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.InscricaoEstadual', InscricaoEstadual);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Endereco', Endereco);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Numero', Numero);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.CEP', CEP);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Bairro', Bairro);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Complemento', Complemento);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Municipio', Municipio);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.UF', UF);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.CodigoMunicipio', CodigoMunicipio);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Telefone', Telefone);
      AIni.WriteString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Email', Email);
    end;
  end;
end;

procedure TGeralConfNFSe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  CodigoMunicipio := AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'CodigoMunicipio', CodigoMunicipio);
  CNPJPrefeitura := AIni.ReadString(fpConfiguracoes.SessaoIni, 'CNPJPrefeitura', CNPJPrefeitura);
  ConsultaLoteAposEnvio := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'ConsultaLoteAposEnvio', ConsultaLoteAposEnvio);
  ConsultaAposCancelar := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'ConsultaAposCancelar', ConsultaAposCancelar);
  MontarPathSchema := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'MontarPathSchema', MontarPathSchema);
  LayoutNFSe := TLayoutNFSe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'LayoutNFSe', Integer(LayoutNFSe)));
  Assinaturas := TAssinaturas(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'Assinaturas', Integer(Assinaturas)));
  FormatoDiscriminacao := TFormatoDiscriminacao(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'FormatoDiscriminacao', Integer(FormatoDiscriminacao)));

  // Emitente
  with Emitente do
  begin
    CNPJ := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.CNPJ', CNPJ);
    InscMun := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.InscMun', InscMun);
    RazSocial := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.RazSocial', RazSocial);
    WSUser := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.WSUser', WSUser);
    WSSenha := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.WSSenha', WSSenha);
    WSFraseSecr := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.WSFraseSecr', WSFraseSecr);
    WSChaveAcesso := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.WSChaveAcesso', WSChaveAcesso);
    WSChaveAutoriz := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.WSChaveAutoriz', WSChaveAutoriz);
    // Dados do Emitente
    with DadosEmitente do
    begin
      NomeFantasia := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.NomeFantasia', NomeFantasia);
      InscricaoEstadual := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.InscricaoEstadual', InscricaoEstadual);
      Endereco := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Endereco', Endereco);
      Numero := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Numero', Numero);
      CEP := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.CEP', CEP);
      Bairro := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Bairro', Bairro);
      Complemento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Complemento', Complemento);
      Municipio := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Municipio', Municipio);
      UF := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.UF', UF);
      CodigoMunicipio := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.CodigoMunicipio', CodigoMunicipio);
      Telefone := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Telefone', Telefone);
      Email := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Emitente.Dados.Email', Email);
    end;
  end;
end;

procedure TGeralConfNFSe.LerParamsMunicipio;
var
  Ok: Boolean;
  CodIBGE: string;
  ACBrNFSeXLocal: TACBrNFSeX;
begin
  if not Assigned(fpConfiguracoes.Owner) then
  begin
    Exit;
  end;

  // Carrega automaticamente o arquivo ACBrNFSeXServicos se necessário.
  ACBrNFSeXLocal := TACBrNFSeX(fpConfiguracoes.Owner);
  if not (csDesigning in fpConfiguracoes.Owner.ComponentState) then
    ACBrNFSeXLocal.LerCidades;

  // ===========================================================================
  // Verifica se o código IBGE consta no arquivo: ACBrNFSeXServicos
  // se encontrar carrega os parâmetros definidos.
  // ===========================================================================
  FPIniParams.SetStrings(fpConfiguracoes.WebServices.Params);

  CodIBGE := IntToStr(FCodigoMunicipio);

  FxMunicipio := FPIniParams.ReadString(CodIBGE, 'Nome', '');
  FxUF := FPIniParams.ReadString(CodIBGE, 'UF', '');
  FxProvedor := FPIniParams.ReadString(CodIBGE, 'Provedor', '');
  FVersao := StrToVersaoNFSe(Ok, FPIniParams.ReadString(CodIBGE, 'Versao', '1.00'));

  if (FxMunicipio <> '') and (FxProvedor = '') and (FLayoutNFSe = lnfsProvedor) then
    raise EACBrDFeException.Create('CodIBGE/Município: [' + CodIBGE +'/'+FxMunicipio +
            '] não está associado a nenhum Provedor.');

  FProvedor := StrToProvedor(FxProvedor);

  if FLayoutNFSe = lnfsPadraoNacionalv1 then
  begin
    FxProvedor := 'PadraoNacional';
    FVersao := ve100;
    FProvedor := proPadraoNacional;
  end;

  if FProvedor = proNenhum then
    raise EACBrDFeException.Create('Código do Município [' + CodIBGE +
            '] não Encontrado.');

  ACBrNFSeXLocal.SetProvider;

  if Assigned(ACBrNFSeXLocal.Provider) then
  begin
    if Assigned(ACBrNFSeXLocal.Provider.ConfigGeral) then
      FLayout := ACBrNFSeXLocal.Provider.ConfigGeral.Layout;
  end;
end;

procedure TGeralConfNFSe.Assign(DeGeralConfNFSe: TGeralConfNFSe);
begin
  inherited Assign(DeGeralConfNFSe);

  //FPIniParams.SetStrings(DeGeralConfNFSe.FPIniParams);
  FVersao     := DeGeralConfNFSe.Versao;
  FxProvedor  := DeGeralConfNFSe.xProvedor;
  FxMunicipio := DeGeralConfNFSe.xMunicipio;
  FxUF        := DeGeralConfNFSe.xUF;

  FCNPJPrefeitura        := DeGeralConfNFSe.CNPJPrefeitura;
  FConsultaLoteAposEnvio := DeGeralConfNFSe.ConsultaLoteAposEnvio;
  FConsultaAposCancelar  := DeGeralConfNFSe.ConsultaAposCancelar;
  FMontarPathSchema      := DeGeralConfNFSe.MontarPathSchema;
  FLayout                := DeGeralConfNFSe.Layout;
  FLayoutNFSe            := DeGeralConfNFSe.LayoutNFSe;
  FAssinaturas           := DeGeralConfNFSe.Assinaturas;
  FFormDiscriminacao     := DeGeralConfNFSe.FormatoDiscriminacao;

  FEmitente.Assign(DeGeralConfNFSe.Emitente);

  //Deve ser a última configuração para que não sobrescreva configurações importantes.
  //Daniel Morais, Panda, Antonio Carlos Junior, Italo Giurizzato Junior, Diego Folieni
  CodigoMunicipio := DeGeralConfNFSe.CodigoMunicipio;
end;

procedure TGeralConfNFSe.SetCodigoMunicipio(const Value: Integer);
begin
  FCodigoMunicipio := Value;

  if FCodigoMunicipio <> 0 then
    LerParamsMunicipio;
end;

{ TArquivosConfNFSe }

constructor TArquivosConfNFSe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathNFSe := False;
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
  PathGer := DeArquivosConfNFSe.PathGer;
  PathRPS := DeArquivosConfNFSe.PathRPS;
  PathNFSe := DeArquivosConfNFSe.PathNFSe;
  PathCan := DeArquivosConfNFSe.PathCan;
  NomeLongoNFSe := DeArquivosConfNFSe.NomeLongoNFSe;
  TabServicosExt := DeArquivosConfNFSe.TabServicosExt;
end;

function TArquivosConfNFSe.GetPathGer(Data: TDateTime = 0;
  const CNPJ: String = ''; const IE: String = ''): String;
begin
  Result := GetPath(FPathGer, 'NFSe', CNPJ, IE, Data);
end;

function TArquivosConfNFSe.GetPathRPS(Data: TDateTime = 0;
  const CNPJ: String = ''; const IE: String = ''): String;
var
  Dir: String;
begin
  if FPathRPS <> '' then
    Result := GetPath(FPathRPS, 'Recibos', CNPJ, IE, Data)
  else
  begin
    Dir := GetPath(FPathGer, 'NFSe', CNPJ, IE, Data);

    Dir := PathWithDelim(Dir) + 'Recibos';

    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    Result := Dir;
  end;
end;

procedure TArquivosConfNFSe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFSe', EmissaoPathNFSe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathGer', PathGer);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathRps', PathRPS);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathNFSe', PathNFSe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathCan', PathCan);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'NomeLongoNFSe', NomeLongoNFSe);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'TabServicosExt', TabServicosExt);
end;

procedure TArquivosConfNFSe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  EmissaoPathNFSe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFSe', EmissaoPathNFSe);
  PathGer := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathGer', PathGer);
  PathRPS := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathRps', PathRPS);
  PathNFSe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathNFSe', PathNFSe);
  PathCan := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathCan', PathCan);
  NomeLongoNFSe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'NomeLongoNFSe', NomeLongoNFSe);
  TabServicosExt := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'TabServicosExt', TabServicosExt);
end;

procedure TArquivosConfNFSe.SetTabServicosExt(const Value: Boolean);
begin
  FTabServicosExt := Value;

  if not Assigned(Owner) then Exit;
  if not Assigned(TConfiguracoesNFSe(Owner).Owner) then Exit;
  if not Assigned(TACBrNFSeX(TConfiguracoesNFSe(Owner).Owner).Provider) then Exit;

  with TACBrNFSeX(TConfiguracoesNFSe(Owner).Owner).Provider.ConfigGeral do
  begin
    TabServicosExt := FTabServicosExt;
  end;
end;

function TArquivosConfNFSe.GetPathNFSe(Data: TDateTime = 0;
  const CNPJ: String = ''; const IE: String = ''): String;
var
  Dir: String;
begin
  if FPathNFSe <> '' then
    Result := GetPath(FPathNFSe, 'Notas', CNPJ, IE, Data)
  else
  begin
    Dir := GetPath(FPathGer, 'NFSe', CNPJ, IE, Data);

    Dir := PathWithDelim(Dir) + 'Notas';

    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    Result := Dir;
  end;
end;

function TArquivosConfNFSe.GetIniTabServicos: String;
begin
  if FIniTabServicos = '' then
    if Assigned(fpConfiguracoes.Owner) then
      if not (csDesigning in fpConfiguracoes.Owner.ComponentState) then
        FIniTabServicos := ApplicationPath + 'TabServicos.ini';

  Result := FIniTabServicos;
end;

function TArquivosConfNFSe.GetPathCan(Data: TDateTime = 0;
  const CNPJ: String = ''; const IE: String = ''): String;
var
  Dir: String;
begin
  if FPathCan <> '' then
    Result := GetPath(FPathCan, 'Can', CNPJ, IE, Data)
  else
  begin
    Dir := GetPath(FPathGer, 'NFSe', CNPJ, IE, Data);

    Dir := PathWithDelim(Dir) + 'Can';

    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    Result := Dir;
  end;
end;

function TArquivosConfNFSe.GetPathEvento(Data: TDateTime; const CNPJ,
  IE: String): String;
var
  Dir: String;
begin
  if FPathNFSe <> '' then
    Result := GetPath(FPathNFSe, 'Eventos', CNPJ, IE, Data)
  else
  begin
    Dir := GetPath(FPathGer, 'NFSe', CNPJ, IE, Data);

    Dir := PathWithDelim(Dir) + 'Eventos';

    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    Result := Dir;
  end;
end;

{ TDadosEmitente }

procedure TDadosEmitente.Assign(Source: TPersistent);
begin
  if Source is TDadosEmitente then
  begin
    FNomeFantasia := TDadosEmitente(Source).NomeFantasia;
    FInscricaoEstadual := TDadosEmitente(Source).InscricaoEstadual;
    FEndereco := TDadosEmitente(Source).Endereco;
    FNumero := TDadosEmitente(Source).Numero;
    FCEP := TDadosEmitente(Source).CEP;
    FBairro := TDadosEmitente(Source).Bairro;
    FComplemento := TDadosEmitente(Source).Complemento;
    FMunicipio := TDadosEmitente(Source).Municipio;
    FUF := TDadosEmitente(Source).UF;
    FCodigoMunicipio := TDadosEmitente(Source).CodigoMunicipio;
    FTelefone := TDadosEmitente(Source).Telefone;
    FEmail := TDadosEmitente(Source).Email;
  end
  else
    inherited Assign(Source);
end;

constructor TDadosEmitente.Create;
begin
  inherited Create;

  FNomeFantasia := '';
  FInscricaoEstadual := '';
  FEndereco := '';
  FNumero := '';
  FCEP := '';
  FBairro := '';
  FComplemento := '';
  FMunicipio := '';
  FUF := '';
  FCodigoMunicipio := '';
  FTelefone := '';
  FEmail := '';
end;

end.
