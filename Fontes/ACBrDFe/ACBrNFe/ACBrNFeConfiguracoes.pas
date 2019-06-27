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
    FVersaoQRCode: TpcnVersaoQrCode;
    FCamposFatObrigatorios: Boolean;
    FForcarGerarTagRejeicao938: TForcarGeracaoTag;

    procedure SetCSC(const AValue: String);
    procedure SetIdCSC(const AValue: String);
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
    property VersaoDF: TpcnVersaoDF read FVersaoDF write SetVersaoDF default ve400;
    property AtualizarXMLCancelado: Boolean
      read FAtualizarXMLCancelado write FAtualizarXMLCancelado default False;
    property IdCSC: String read FIdCSC write SetIdCSC;
    property CSC: String read FCSC write SetCSC;
    property VersaoQRCode: TpcnVersaoQrCode read FVersaoQRCode write FVersaoQRCode default veqr100;
    property CamposFatObrigatorios: Boolean
      read FCamposFatObrigatorios write FCamposFatObrigatorios default True;
    property ForcarGerarTagRejeicao938: TForcarGeracaoTag read FForcarGerarTagRejeicao938 write FForcarGerarTagRejeicao938 default fgtNunca;
  end;

  { TArquivosConfNFe }

  TArquivosConfNFe = class(TArquivosConf)
  private
    FEmissaoPathNFe: boolean;
    FSalvarEvento: boolean;
    FSalvarApenasNFeProcessadas: boolean;
    FNormatizarMunicipios: Boolean;
    FPathNFe: String;
    FPathInu: String;
    FPathEvento: String;
    FPathArquivoMunicipios: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfNFe: TArquivosConfNFe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathInu(const CNPJ: String = ''): String;
    function GetPathNFe(Data: TDateTime = 0; const CNPJ: String = ''; Modelo: Integer = 0): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String = ''; Data: TDateTime = 0): String;
  published
    property EmissaoPathNFe: boolean read FEmissaoPathNFe
      write FEmissaoPathNFe default False;
    property SalvarEvento: boolean read FSalvarEvento
      write FSalvarEvento default False;
    property SalvarApenasNFeProcessadas: boolean
      read FSalvarApenasNFeProcessadas write FSalvarApenasNFeProcessadas default False;
    property NormatizarMunicipios: boolean
      read FNormatizarMunicipios write FNormatizarMunicipios default False;
    property PathNFe: String read FPathNFe write FPathNFe;
    property PathInu: String read FPathInu write FPathInu;
    property PathEvento: String read FPathEvento write FPathEvento;
    property PathArquivoMunicipios: String read FPathArquivoMunicipios write FPathArquivoMunicipios;
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
    property RespTec;
  end;

implementation

uses
  ACBrUtil, ACBrNFe,
  DateUtils;

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
  RespTec.Assign(DeConfiguracoesNFe.RespTec);
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

  FModeloDF                  := moNFe;
  FModeloDFCodigo            := StrToInt(ModeloDFToStr(FModeloDF));
  FVersaoDF                  := ve400;
  FAtualizarXMLCancelado     := False;
  FIdCSC                     := '';
  FCSC                       := '';
  FVersaoQRCode              := veqr000;
  FCamposFatObrigatorios     := True;
  FForcarGerarTagRejeicao938 := fgtNunca;
end;

procedure TGeralConfNFe.Assign(DeGeralConfNFe: TGeralConfNFe);
begin
  inherited Assign(DeGeralConfNFe);

  ModeloDF := DeGeralConfNFe.ModeloDF;
  VersaoDF := DeGeralConfNFe.VersaoDF;
  AtualizarXMLCancelado := DeGeralConfNFe.AtualizarXMLCancelado;
  IdCSC    := DeGeralConfNFe.IdCSC;
  CSC      := DeGeralConfNFe.CSC;
  VersaoQRCode := DeGeralConfNFe.VersaoQRCode;
  CamposFatObrigatorios := DeGeralConfNFe.CamposFatObrigatorios;
  ForcarGerarTagRejeicao938 := DeGeralConfNFe.ForcarGerarTagRejeicao938;
end;

procedure TGeralConfNFe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF));
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'AtualizarXMLCancelado', AtualizarXMLCancelado);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoQRCode', Integer(VersaoQRCode));
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'CamposFatObrigatorios', CamposFatObrigatorios);
  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'TagNT2018005', Integer(ForcarGerarTagRejeicao938));
end;

procedure TGeralConfNFe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  IdCSC                     := AIni.ReadString(fpConfiguracoes.SessaoIni, 'IdCSC', IdCSC);
  CSC                       := AIni.ReadString(fpConfiguracoes.SessaoIni, 'CSC', CSC);
  ModeloDF                  := TpcnModeloDF(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'ModeloDF', Integer(ModeloDF)));
  VersaoDF                  := TpcnVersaoDF(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
  AtualizarXMLCancelado     := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'AtualizarXMLCancelado', AtualizarXMLCancelado);
  VersaoQRCode              := TpcnVersaoQrCode(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoQRCode', Integer(VersaoQRCode)));
  CamposFatObrigatorios     := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'CamposFatObrigatorios', CamposFatObrigatorios);
  ForcarGerarTagRejeicao938 := TForcarGeracaoTag(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'TagNT2018005', Integer(ForcarGerarTagRejeicao938)));
end;

procedure TGeralConfNFe.SetModeloDF(AValue: TpcnModeloDF);
begin
  FModeloDF := AValue;
  FModeloDFCodigo := StrToInt(ModeloDFToStr(FModeloDF));
end;

procedure TGeralConfNFe.SetCSC(const AValue: String);
begin
  if FCSC=AValue then
    Exit;

  FCSC:=Trim(AValue);
end;

procedure TGeralConfNFe.SetIdCSC(const AValue: String);
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

  FEmissaoPathNFe := False;
  FSalvarEvento := False;
  FSalvarApenasNFeProcessadas := False;
  FNormatizarMunicipios := False;
  FPathNFe := '';
  FPathInu := '';
  FPathEvento := '';
  FPathArquivoMunicipios := '';
end;

destructor TArquivosConfNFe.Destroy;
begin

  inherited;
end;

procedure TArquivosConfNFe.Assign(DeArquivosConfNFe: TArquivosConfNFe);
begin
  inherited Assign(DeArquivosConfNFe);

  EmissaoPathNFe             := DeArquivosConfNFe.EmissaoPathNFe;
  SalvarEvento               := DeArquivosConfNFe.SalvarEvento;
  SalvarApenasNFeProcessadas := DeArquivosConfNFe.SalvarApenasNFeProcessadas;
  NormatizarMunicipios       := DeArquivosConfNFe.NormatizarMunicipios;
  PathNFe                    := DeArquivosConfNFe.PathNFe;
  PathInu                    := DeArquivosConfNFe.PathInu;
  PathEvento                 := DeArquivosConfNFe.PathEvento;
  PathArquivoMunicipios      := DeArquivosConfNFe.PathArquivoMunicipios;
end;

procedure TArquivosConfNFe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarApenasNFeProcessadas', SalvarApenasNFeProcessadas);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFe', EmissaoPathNFe);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathNFe', PathNFe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathInu', PathInu);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

procedure TArquivosConfNFe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarEvento := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  SalvarApenasNFeProcessadas := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarApenasNFeProcessadas', SalvarApenasNFeProcessadas);
  EmissaoPathNFe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathNFe', EmissaoPathNFe);
  NormatizarMunicipios := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  PathNFe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathNFe', PathNFe);
  PathInu := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathInu', PathInu);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  PathArquivoMunicipios := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

function TArquivosConfNFe.GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String;
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

function TArquivosConfNFe.GetPathInu(const CNPJ: String = ''): String;
begin
  Result := GetPath(FPathInu, 'Inu', CNPJ);
end;

function TArquivosConfNFe.GetPathNFe(Data: TDateTime = 0; const CNPJ: String = ''; Modelo: Integer = 0): String;
var
  DescricaoModelo: String;
begin
  case Modelo of
     0:
       begin
         if Assigned(fpConfiguracoes.Owner) then
           DescricaoModelo := TACBrNFe(fpConfiguracoes.Owner).GetNomeModeloDFe
         else
           DescricaoModelo := 'NFe';
       end;

    55:
       DescricaoModelo := 'NFe';
    65:
       DescricaoModelo := 'NFCe';
  end;

  Result := GetPath(FPathNFe, DescricaoModelo, CNPJ, Data, DescricaoModelo);
end;

end.

