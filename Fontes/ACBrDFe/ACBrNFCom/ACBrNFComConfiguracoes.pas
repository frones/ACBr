{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrNFComConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes,
//  ACBrDFeConversao,
  pcnConversao,
  ACBrNFComConversao;

type

  { TGeralConfNFCom }

  TGeralConfNFCom = class(TGeralConf)
  private
    FVersaoDF: TVersaoNFCom;
    FIdCSC: string;
    FCSC: string;
    FVersaoQRCode: TVersaoQrCode;

    procedure SetVersaoDF(const Value: TVersaoNFCom);
    procedure SetIdCSC(const AValue: string);
    procedure SetCSC(const AValue: string);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfNFCom: TGeralConfNFCom); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property VersaoDF: TVersaoNFCom read FVersaoDF write SetVersaoDF default ve100;
    property IdCSC: string read FIdCSC write SetIdCSC;
    property CSC: string read FCSC write SetCSC;
    property VersaoQRCode: TVersaoQrCode read FVersaoQRCode write FVersaoQRCode default veqr100;
  end;

  { TArquivosConfNFCom }

  TArquivosConfNFCom = class(TArquivosConf)
  private
    FEmissaoPathNFCom: boolean;
    FSalvarEvento: boolean;
    FNormatizarMunicipios: Boolean;
    FPathNFCom: string;
    FPathEvento: string;
    FPathArquivoMunicipios: string;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfNFCom: TArquivosConfNFCom); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathNFCom(Data: TDateTime = 0; const CNPJ: string = ''; const IE: string = ''): string;
    function GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: string = ''; const IE: string = ''; Data: TDateTime = 0): string;
  published
    property EmissaoPathNFCom: boolean read FEmissaoPathNFCom
      write FEmissaoPathNFCom default False;
    property SalvarEvento: boolean read FSalvarEvento
      write FSalvarEvento default False;
    property NormatizarMunicipios: boolean
      read FNormatizarMunicipios write FNormatizarMunicipios default False;
    property PathNFCom: string read FPathNFCom write FPathNFCom;
    property PathEvento: string read FPathEvento write FPathEvento;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
  end;

  { TConfiguracoesNFCom }

  TConfiguracoesNFCom = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfNFCom;
    function GetGeral: TGeralConfNFCom;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesNFCom: TConfiguracoesNFCom); reintroduce;

  published
    property Geral: TGeralConfNFCom read GetGeral;
    property Arquivos: TArquivosConfNFCom read GetArquivos;
    property WebServices;
    property Certificados;
    property RespTec;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO,
  DateUtils;

{ TConfiguracoesNFCom }

constructor TConfiguracoesNFCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'NFCom';
  WebServices.ResourceName := 'ACBrNFComServicos';
end;

procedure TConfiguracoesNFCom.Assign(DeConfiguracoesNFCom: TConfiguracoesNFCom);
begin
  Geral.Assign(DeConfiguracoesNFCom.Geral);
  WebServices.Assign(DeConfiguracoesNFCom.WebServices);
  Certificados.Assign(DeConfiguracoesNFCom.Certificados);
  Arquivos.Assign(DeConfiguracoesNFCom.Arquivos);
  RespTec.Assign(DeConfiguracoesNFCom.RespTec);
end;

function TConfiguracoesNFCom.GetArquivos: TArquivosConfNFCom;
begin
  Result := TArquivosConfNFCom(FPArquivos);
end;

function TConfiguracoesNFCom.GetGeral: TGeralConfNFCom;
begin
  Result := TGeralConfNFCom(FPGeral);
end;

procedure TConfiguracoesNFCom.CreateGeralConf;
begin
  FPGeral := TGeralConfNFCom.Create(Self);
end;

procedure TConfiguracoesNFCom.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfNFCom.Create(self);
end;

{ TGeralConfNFCom }

constructor TGeralConfNFCom.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF     := ve100;
  FIdCSC        := '';
  FCSC          := '';
  FVersaoQRCode := veqr000;
end;

procedure TGeralConfNFCom.Assign(DeGeralConfNFCom: TGeralConfNFCom);
begin
  inherited Assign(DeGeralConfNFCom);

  VersaoDF     := DeGeralConfNFCom.VersaoDF;
  IdCSC        := DeGeralConfNFCom.IdCSC;
  CSC          := DeGeralConfNFCom.CSC;
  VersaoQRCode := DeGeralConfNFCom.VersaoQRCode;
end;

procedure TGeralConfNFCom.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoQRCode', Integer(VersaoQRCode));
end;

procedure TGeralConfNFCom.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  IdCSC        := AIni.ReadString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  CSC          := AIni.ReadString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  VersaoDF     := TVersaoNFCom(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
  VersaoQRCode := TVersaoQrCode(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoQRCode', Integer(VersaoQRCode)));
end;

procedure TGeralConfNFCom.SetIdCSC(const AValue: string);
begin
  if FIdCSC = AValue then
    Exit;

  FIdCSC := IntToStrZero(StrToIntDef(AValue,0),6);
end;

procedure TGeralConfNFCom.SetCSC(const AValue: string);
begin
  if FCSC = AValue then
    Exit;

  FCSC := Trim(AValue);
end;

procedure TGeralConfNFCom.SetVersaoDF(const Value: TVersaoNFCom);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfNFCom }

constructor TArquivosConfNFCom.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathNFCom             := False;
  FSalvarEvento                := False;
  FNormatizarMunicipios        := False;
  FPathNFCom                    := '';
  FPathEvento                  := '';
  FPathArquivoMunicipios       := '';
end;

destructor TArquivosConfNFCom.Destroy;
begin

  inherited;
end;

procedure TArquivosConfNFCom.Assign(DeArquivosConfNFCom: TArquivosConfNFCom);
begin
  inherited Assign(DeArquivosConfNFCom);

  EmissaoPathNFCom             := DeArquivosConfNFCom.EmissaoPathNFCom;
  SalvarEvento                := DeArquivosConfNFCom.SalvarEvento;
  NormatizarMunicipios        := DeArquivosConfNFCom.NormatizarMunicipios;
  PathNFCom                    := DeArquivosConfNFCom.PathNFCom;
  PathEvento                  := DeArquivosConfNFCom.PathEvento;
  PathArquivoMunicipios       := DeArquivosConfNFCom.PathArquivoMunicipios;
end;

procedure TArquivosConfNFCom.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFCom', EmissaoPathNFCom);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathNFCom', PathNFCom);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

procedure TArquivosConfNFCom.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarEvento := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  EmissaoPathNFCom := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFCom', EmissaoPathNFCom);
  NormatizarMunicipios := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  PathNFCom := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathNFCom', PathNFCom);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  PathArquivoMunicipios := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

function TArquivosConfNFCom.GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: string;
  const IE: string; Data: TDateTime): string;
var
  Dir: string;
begin
  Dir := GetPath(FPathEvento, 'Evento', CNPJ, IE, Data);

  if AdicionarLiteral then
    Dir := PathWithDelim(Dir) + TpEventoToDescStr(tipoEvento);

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Result := Dir;
end;

function TArquivosConfNFCom.GetPathNFCom(Data: TDateTime = 0; const CNPJ: string = ''; const IE: string = ''): string;
begin
  Result := GetPath(FPathNFCom, 'NFCom', CNPJ, IE, Data, 'NFCom');
end;

end.

