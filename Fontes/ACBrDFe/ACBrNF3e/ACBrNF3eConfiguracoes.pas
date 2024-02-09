{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3eConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, pcnConversao, ACBrNF3eConversao;

type

  { TGeralConfNF3e }

  TGeralConfNF3e = class(TGeralConf)
  private
    FVersaoDF: TVersaoNF3e;
    FIdCSC: String;
    FCSC: String;
    FVersaoQRCode: TVersaoQrCode;

    procedure SetVersaoDF(const Value: TVersaoNF3e);
    procedure SetIdCSC(const AValue: String);
    procedure SetCSC(const AValue: String);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfNF3e: TGeralConfNF3e); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property VersaoDF: TVersaoNF3e read FVersaoDF write SetVersaoDF default ve100;
    property IdCSC: String read FIdCSC write SetIdCSC;
    property CSC: String read FCSC write SetCSC;
    property VersaoQRCode: TVersaoQrCode read FVersaoQRCode write FVersaoQRCode default veqr100;
  end;

  { TArquivosConfNF3e }

  TArquivosConfNF3e = class(TArquivosConf)
  private
    FEmissaoPathNF3e: boolean;
    FSalvarEvento: boolean;
    FNormatizarMunicipios: Boolean;
    FPathNF3e: String;
    FPathEvento: String;
    FPathArquivoMunicipios: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfNF3e: TArquivosConfNF3e); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathNF3e(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String = ''; const IE: String = ''; Data: TDateTime = 0): String;
  published
    property EmissaoPathNF3e: boolean read FEmissaoPathNF3e
      write FEmissaoPathNF3e default False;
    property SalvarEvento: boolean read FSalvarEvento
      write FSalvarEvento default False;
    property NormatizarMunicipios: boolean
      read FNormatizarMunicipios write FNormatizarMunicipios default False;
    property PathNF3e: String read FPathNF3e write FPathNF3e;
    property PathEvento: String read FPathEvento write FPathEvento;
    property PathArquivoMunicipios: String read FPathArquivoMunicipios write FPathArquivoMunicipios;
  end;

  { TConfiguracoesNF3e }

  TConfiguracoesNF3e = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfNF3e;
    function GetGeral: TGeralConfNF3e;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesNF3e: TConfiguracoesNF3e); reintroduce;

  published
    property Geral: TGeralConfNF3e read GetGeral;
    property Arquivos: TArquivosConfNF3e read GetArquivos;
    property WebServices;
    property Certificados;
    property RespTec;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO,
  DateUtils;

{ TConfiguracoesNF3e }

constructor TConfiguracoesNF3e.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'NF3e';
  WebServices.ResourceName := 'ACBrNF3eServicos';
end;

procedure TConfiguracoesNF3e.Assign(DeConfiguracoesNF3e: TConfiguracoesNF3e);
begin
  Geral.Assign(DeConfiguracoesNF3e.Geral);
  WebServices.Assign(DeConfiguracoesNF3e.WebServices);
  Certificados.Assign(DeConfiguracoesNF3e.Certificados);
  Arquivos.Assign(DeConfiguracoesNF3e.Arquivos);
  RespTec.Assign(DeConfiguracoesNF3e.RespTec);
end;

function TConfiguracoesNF3e.GetArquivos: TArquivosConfNF3e;
begin
  Result := TArquivosConfNF3e(FPArquivos);
end;

function TConfiguracoesNF3e.GetGeral: TGeralConfNF3e;
begin
  Result := TGeralConfNF3e(FPGeral);
end;

procedure TConfiguracoesNF3e.CreateGeralConf;
begin
  FPGeral := TGeralConfNF3e.Create(Self);
end;

procedure TConfiguracoesNF3e.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfNF3e.Create(self);
end;

{ TGeralConfNF3e }

constructor TGeralConfNF3e.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF     := ve100;
  FIdCSC        := '';
  FCSC          := '';
  FVersaoQRCode := veqr000;
end;

procedure TGeralConfNF3e.Assign(DeGeralConfNF3e: TGeralConfNF3e);
begin
  inherited Assign(DeGeralConfNF3e);

  VersaoDF     := DeGeralConfNF3e.VersaoDF;
  IdCSC        := DeGeralConfNF3e.IdCSC;
  CSC          := DeGeralConfNF3e.CSC;
  VersaoQRCode := DeGeralConfNF3e.VersaoQRCode;
end;

procedure TGeralConfNF3e.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoQRCode', Integer(VersaoQRCode));
end;

procedure TGeralConfNF3e.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  IdCSC        := AIni.ReadString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  CSC          := AIni.ReadString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  VersaoDF     := TVersaoNF3e(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
  VersaoQRCode := TVersaoQrCode(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoQRCode', Integer(VersaoQRCode)));
end;

procedure TGeralConfNF3e.SetIdCSC(const AValue: String);
begin
  if FIdCSC = AValue then
    Exit;

  FIdCSC := IntToStrZero(StrToIntDef(AValue,0),6);
end;

procedure TGeralConfNF3e.SetCSC(const AValue: String);
begin
  if FCSC = AValue then
    Exit;

  FCSC := Trim(AValue);
end;

procedure TGeralConfNF3e.SetVersaoDF(const Value: TVersaoNF3e);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfNF3e }

constructor TArquivosConfNF3e.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathNF3e             := False;
  FSalvarEvento                := False;
  FNormatizarMunicipios        := False;
  FPathNF3e                    := '';
  FPathEvento                  := '';
  FPathArquivoMunicipios       := '';
end;

destructor TArquivosConfNF3e.Destroy;
begin

  inherited;
end;

procedure TArquivosConfNF3e.Assign(DeArquivosConfNF3e: TArquivosConfNF3e);
begin
  inherited Assign(DeArquivosConfNF3e);

  EmissaoPathNF3e             := DeArquivosConfNF3e.EmissaoPathNF3e;
  SalvarEvento                := DeArquivosConfNF3e.SalvarEvento;
  NormatizarMunicipios        := DeArquivosConfNF3e.NormatizarMunicipios;
  PathNF3e                    := DeArquivosConfNF3e.PathNF3e;
  PathEvento                  := DeArquivosConfNF3e.PathEvento;
  PathArquivoMunicipios       := DeArquivosConfNF3e.PathArquivoMunicipios;
end;

procedure TArquivosConfNF3e.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNF3e', EmissaoPathNF3e);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathNF3e', PathNF3e);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

procedure TArquivosConfNF3e.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarEvento := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  EmissaoPathNF3e := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNF3e', EmissaoPathNF3e);
  NormatizarMunicipios := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  PathNF3e := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathNF3e', PathNF3e);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  PathArquivoMunicipios := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

function TArquivosConfNF3e.GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String;
  const IE: String; Data: TDateTime): String;
var
  Dir: String;
begin
  Dir := GetPath(FPathEvento, 'Evento', CNPJ, IE, Data);

  if AdicionarLiteral then
    Dir := PathWithDelim(Dir) + TpEventoToDescStr(tipoEvento);

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Result := Dir;
end;

function TArquivosConfNF3e.GetPathNF3e(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
begin
  Result := GetPath(FPathNF3e, 'NF3e', CNPJ, IE, Data, 'NF3e');
end;

end.

