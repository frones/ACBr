{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

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

unit ACBrNFeConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, pcnConversao, pcnConversaoNFe;

type

  { TGeralConfNFe }

  TGeralConfNFe = class(TGeralConf)
  private
    FModeloDF: TpcnModeloDF;
    FModeloDFCodigo: integer;
    FVersaoDF: TpcnVersaoDF;
    FAtualizarXMLCancelado: Boolean;
    FIdCSC: String;
    FCSC: String;
    FIncluirQRCodeXMLNFCe: Boolean;

    procedure SetCSC(AValue: String);
    procedure SetIdCSC(AValue: String);
    procedure SetModeloDF(AValue: TpcnModeloDF);
    procedure SetVersaoDF(const Value: TpcnVersaoDF);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfNFe: TGeralConfNFe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property ModeloDF: TpcnModeloDF read FModeloDF write SetModeloDF default moNFe;
    property ModeloDFCodigo: integer read FModeloDFCodigo;
    property VersaoDF: TpcnVersaoDF read FVersaoDF write SetVersaoDF default ve310;
    property AtualizarXMLCancelado: Boolean
      read FAtualizarXMLCancelado write FAtualizarXMLCancelado default False;
    property IdCSC: String read FIdCSC write SetIdCSC;
    property CSC: String read FCSC write SetCSC;
    property IncluirQRCodeXMLNFCe: Boolean read FIncluirQRCodeXMLNFCe write FIncluirQRCodeXMLNFCe default True;
  end;

  { TDownloadConfNFe }

  TDownloadConfNFe = class(TPersistent)
  private
    FPathDownload: String;
    FSepararPorNome: Boolean;
  public
    Constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property PathDownload: String read FPathDownload write FPathDownload;
    property SepararPorNome: Boolean read FSepararPorNome write FSepararPorNome default False;
  end;

  { TArquivosConfNFe }

  TArquivosConfNFe = class(TArquivosConf)
  private
    FEmissaoPathNFe: boolean;
    FSalvarEvento: boolean;
    FSalvarApenasNFeProcessadas: boolean;
    FPathNFe: String;
    FPathInu: String;
    FPathEvento: String;
    FDownloadNFe: TDownloadConfNFe;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfNFe: TArquivosConfNFe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathInu(CNPJ: String = ''): String;
    function GetPathNFe(Data: TDateTime = 0; CNPJ: String = ''; Modelo: Integer = 0): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; CNPJ: String = ''; Data: TDateTime = 0): String;
    function GetPathDownload(xNome: String = ''; CNPJ: String = ''; Data: TDateTime = 0): String;
  published
    property EmissaoPathNFe: boolean read FEmissaoPathNFe
      write FEmissaoPathNFe default False;
    property SalvarEvento: boolean read FSalvarEvento
      write FSalvarEvento default False;
    property SalvarApenasNFeProcessadas: boolean
      read FSalvarApenasNFeProcessadas write FSalvarApenasNFeProcessadas default False;
    property PathNFe: String read FPathNFe write FPathNFe;
    property PathInu: String read FPathInu write FPathInu;
    property PathEvento: String read FPathEvento write FPathEvento;
    property DownloadNFe: TDownloadConfNFe read FDownloadNFe write FDownloadNFe;
  end;

  { TConfiguracoesNFe }

  TConfiguracoesNFe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfNFe;
    function GetGeral: TGeralConfNFe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesNFe: TConfiguracoesNFe); reintroduce;

  published
    property Geral: TGeralConfNFe read GetGeral;
    property Arquivos: TArquivosConfNFe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil, ACBrNFe,
  DateUtils;

{ TDownloadConfNFe }

constructor TDownloadConfNFe.Create;
begin
  FPathDownload := '';
  FSepararPorNome := False;
end;

procedure TDownloadConfNFe.Assign(Source: TPersistent);
begin
  if Source is TDownloadConfNFe then
  begin
    FPathDownload := TDownloadConfNFe(Source).PathDownload;
    FSepararPorNome := TDownloadConfNFe(Source).SepararPorNome;
  end
  else
    inherited Assign(Source);
end;

{ TConfiguracoesNFe }

constructor TConfiguracoesNFe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'NFe';
  WebServices.ResourceName := 'ACBrNFeServicos';
end;

procedure TConfiguracoesNFe.Assign(DeConfiguracoesNFe: TConfiguracoesNFe);
begin
  Geral.Assign(DeConfiguracoesNFe.Geral);
  WebServices.Assign(DeConfiguracoesNFe.WebServices);
  Certificados.Assign(DeConfiguracoesNFe.Certificados);
  Arquivos.Assign(DeConfiguracoesNFe.Arquivos);
end;

function TConfiguracoesNFe.GetArquivos: TArquivosConfNFe;
begin
  Result := TArquivosConfNFe(FPArquivos);
end;

function TConfiguracoesNFe.GetGeral: TGeralConfNFe;
begin
  Result := TGeralConfNFe(FPGeral);
end;

procedure TConfiguracoesNFe.CreateGeralConf;
begin
  FPGeral := TGeralConfNFe.Create(Self);
end;

procedure TConfiguracoesNFe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfNFe.Create(self);
end;

{ TGeralConfNFe }

constructor TGeralConfNFe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FModeloDF := moNFe;
  FModeloDFCodigo := StrToInt(ModeloDFToStr(FModeloDF));
  FVersaoDF := ve310;
  FAtualizarXMLCancelado := False;
  FIdCSC := '';
  FCSC := '';
  FIncluirQRCodeXMLNFCe := True;
end;

procedure TGeralConfNFe.Assign(DeGeralConfNFe: TGeralConfNFe);
begin
  inherited Assign(DeGeralConfNFe);

  ModeloDF := DeGeralConfNFe.ModeloDF;
  VersaoDF := DeGeralConfNFe.VersaoDF;
  IdCSC    := DeGeralConfNFe.IdCSC;
  CSC      := DeGeralConfNFe.CSC;
  IncluirQRCodeXMLNFCe := DeGeralConfNFe.IncluirQRCodeXMLNFCe;
end;

procedure TGeralConfNFe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'IncluirQRCodeXMLNFCe', IncluirQRCodeXMLNFCe);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'AtualizarXMLCancelado', AtualizarXMLCancelado);
end;

procedure TGeralConfNFe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  IdCSC := AIni.ReadString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  CSC := AIni.ReadString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  ModeloDF := TpcnModeloDF(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF)));
  VersaoDF := TpcnVersaoDF(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
  AtualizarXMLCancelado := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'AtualizarXMLCancelado', AtualizarXMLCancelado);
  IncluirQRCodeXMLNFCe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'IncluirQRCodeXMLNFCe', IncluirQRCodeXMLNFCe);
end;

procedure TGeralConfNFe.SetModeloDF(AValue: TpcnModeloDF);
begin
  FModeloDF := AValue;
  FModeloDFCodigo := StrToInt(ModeloDFToStr(FModeloDF));
end;

procedure TGeralConfNFe.SetCSC(AValue: String);
begin
  if FCSC=AValue then
    Exit;

  FCSC:=Trim(AValue);
end;

procedure TGeralConfNFe.SetIdCSC(AValue: String);
begin
  if FIdCSC=AValue then
    Exit;

  FIdCSC:=IntToStrZero(StrToIntDef(AValue,0),6);
end;

procedure TGeralConfNFe.SetVersaoDF(const Value: TpcnVersaoDF);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfNFe }

constructor TArquivosConfNFe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FDownloadNFe := TDownloadConfNFe.Create;
  FEmissaoPathNFe := False;
  FSalvarEvento := False;
  FSalvarApenasNFeProcessadas := False;
  FPathNFe := '';
  FPathInu := '';
  FPathEvento := '';
end;

destructor TArquivosConfNFe.Destroy;
begin
  FDownloadNFe.Free;
  inherited;
end;

procedure TArquivosConfNFe.Assign(DeArquivosConfNFe: TArquivosConfNFe);
begin
  inherited Assign(DeArquivosConfNFe);

  EmissaoPathNFe             := DeArquivosConfNFe.EmissaoPathNFe;
  SalvarEvento               := DeArquivosConfNFe.SalvarEvento;
  SalvarApenasNFeProcessadas := DeArquivosConfNFe.SalvarApenasNFeProcessadas;
  PathNFe                    := DeArquivosConfNFe.PathNFe;
  PathInu                    := DeArquivosConfNFe.PathInu;
  PathEvento                 := DeArquivosConfNFe.PathEvento;
  FDownloadNFe.Assign(DeArquivosConfNFe.DownloadNFe);
end;

procedure TArquivosConfNFe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarApenasNFeProcessadas', SalvarApenasNFeProcessadas);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFe', EmissaoPathNFe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathNFe', PathNFe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathInu', PathInu);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'Download.PathDownload', DownloadNFe.PathDownload);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'Download.SepararPorNome', DownloadNFe.SepararPorNome);
end;

procedure TArquivosConfNFe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarEvento := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  SalvarApenasNFeProcessadas := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarApenasNFeProcessadas', SalvarApenasNFeProcessadas);
  EmissaoPathNFe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFe', EmissaoPathNFe);
  PathNFe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathNFe', PathNFe);
  PathInu := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathInu', PathInu);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  DownloadNFe.PathDownload := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Download.PathDownload', DownloadNFe.PathDownload);
  DownloadNFe.SepararPorNome := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'Download.SepararPorNome', DownloadNFe.SepararPorNome);
end;

function TArquivosConfNFe.GetPathDownload(xNome: String = ''; CNPJ: String = ''; Data: TDateTime = 0): String;
var
  rPathDown: String;
begin
  rPathDown := '';
  if EstaVazio(FDownloadNFe.PathDownload) then
     FDownloadNFe.PathDownload := PathSalvar;

  if (FDownloadNFe.SepararPorNome) and (NaoEstaVazio(xNome)) then
     rPathDown := rPathDown + PathWithDelim(FDownloadNFe.PathDownload) + OnlyAlphaNum(xNome)
  else
     rPathDown := FDownloadNFe.PathDownload;

  Result := GetPath(rPathDown, 'Down', CNPJ, Data);
end;

function TArquivosConfNFe.GetPathEvento(tipoEvento: TpcnTpEvento; CNPJ: String;
  Data: TDateTime): String;
var
  Dir: String;
begin
  Dir := GetPath(FPathEvento, 'Evento', CNPJ, Data);

  if AdicionarLiteral then
    Dir := PathWithDelim(Dir) + TpEventoToDescStr(tipoEvento);

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Result := Dir;
end;

function TArquivosConfNFe.GetPathInu(CNPJ: String = ''): String;
begin
  Result := GetPath(FPathInu, 'Inu', CNPJ);
end;

function TArquivosConfNFe.GetPathNFe(Data: TDateTime = 0; CNPJ: String = ''; Modelo: Integer = 0): String;
var
  DescricaoModelo: String;
begin
  case Modelo of
     0: DescricaoModelo := TACBrNFe(fpConfiguracoes.Owner).GetNomeModeloDFe;
    55: DescricaoModelo := 'NFe';
    65: DescricaoModelo := 'NFCe';
  end;

  Result := GetPath(FPathNFe, DescricaoModelo, CNPJ, Data, DescricaoModelo);
end;

end.

