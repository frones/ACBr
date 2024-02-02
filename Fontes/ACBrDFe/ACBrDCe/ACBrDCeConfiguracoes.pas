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

unit ACBrDCeConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, pcnConversao, ACBrDCeConversao;

type

  { TGeralConfDCe }

  TGeralConfDCe = class(TGeralConf)
  private
    FVersaoDF: TVersaoDCe;

    procedure SetVersaoDF(const Value: TVersaoDCe);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfDCe: TGeralConfDCe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property VersaoDF: TVersaoDCe read FVersaoDF write SetVersaoDF default ve100;
  end;

  { TArquivosConfDCe }

  TArquivosConfDCe = class(TArquivosConf)
  private
    FEmissaoPathDCe: boolean;
    FSalvarApenasDCeProcessados: boolean;
    FNormatizarMunicipios: Boolean;
    FPathDCe: String;
    FPathEvento: String;
    FPathArquivoMunicipios: String                                           ;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfDCe: TArquivosConfDCe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathDCe(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; const CNPJ: String = ''; const IE: String = ''; Data: TDateTime = 0): String;
  published
    property EmissaoPathDCe: boolean read FEmissaoPathDCe
      write FEmissaoPathDCe default False;
    property SalvarApenasDCeProcessados: boolean
      read FSalvarApenasDCeProcessados write FSalvarApenasDCeProcessados default False;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios default False;
    property PathDCe: String read FPathDCe write FPathDCe;
    property PathEvento: String read FPathEvento write FPathEvento;
    property PathArquivoMunicipios: String read FPathArquivoMunicipios write FPathArquivoMunicipios;
  end;

  { TConfiguracoesDCe }

  TConfiguracoesDCe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfDCe;
    function GetGeral: TGeralConfDCe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesDCe: TConfiguracoesDCe); reintroduce;

  published
    property Geral: TGeralConfDCe read GetGeral;
    property Arquivos: TArquivosConfDCe read GetArquivos;
    property WebServices;
    property Certificados;
    property RespTec;
  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.FilesIO,
  DateUtils;

{ TConfiguracoesDCe }

constructor TConfiguracoesDCe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'DCe';
  WebServices.ResourceName := 'ACBrDCeServicos';
end;

function TConfiguracoesDCe.GetArquivos: TArquivosConfDCe;
begin
  Result := TArquivosConfDCe(FPArquivos);
end;

function TConfiguracoesDCe.GetGeral: TGeralConfDCe;
begin
  Result := TGeralConfDCe(FPGeral);
end;

procedure TConfiguracoesDCe.CreateGeralConf;
begin
  FPGeral := TGeralConfDCe.Create(Self);
end;

procedure TConfiguracoesDCe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfDCe.Create(self);
end;

procedure TConfiguracoesDCe.Assign(DeConfiguracoesDCe: TConfiguracoesDCe);
begin
  Geral.Assign(DeConfiguracoesDCe.Geral);
  WebServices.Assign(DeConfiguracoesDCe.WebServices);
  Certificados.Assign(DeConfiguracoesDCe.Certificados);
  Arquivos.Assign(DeConfiguracoesDCe.Arquivos);
  RespTec.Assign(DeConfiguracoesDCe.RespTec);
end;

{ TGeralConfDCe }

procedure TGeralConfDCe.Assign(DeGeralConfDCe: TGeralConfDCe);
begin
  inherited Assign(DeGeralConfDCe);

  FVersaoDF := DeGeralConfDCe.VersaoDF;
end;

constructor TGeralConfDCe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF := ve100;
end;

procedure TGeralConfDCe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
end;

procedure TGeralConfDCe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  VersaoDF := TVersaoDCe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
end;

procedure TGeralConfDCe.SetVersaoDF(const Value: TVersaoDCe);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfDCe }

procedure TArquivosConfDCe.Assign(DeArquivosConfDCe: TArquivosConfDCe);
begin
  inherited Assign(DeArquivosConfDCe);

  FEmissaoPathDCe := DeArquivosConfDCe.EmissaoPathDCe;
  FSalvarApenasDCeProcessados := DeArquivosConfDCe.SalvarApenasDCeProcessados;
  FNormatizarMunicipios := DeArquivosConfDCe.NormatizarMunicipios;
  FPathDCe := DeArquivosConfDCe.PathDCe;
  FPathEvento := DeArquivosConfDCe.PathEvento;
  FPathArquivoMunicipios := DeArquivosConfDCe.PathArquivoMunicipios;
end;

constructor TArquivosConfDCe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathDCe := False;
  FSalvarApenasDCeProcessados := False;
  FNormatizarMunicipios := False;
  FPathDCe := '';
  FPathEvento := '';
  FPathArquivoMunicipios := '';
end;

destructor TArquivosConfDCe.Destroy;
begin

  inherited;
end;

function TArquivosConfDCe.GetPathEvento(tipoEvento: TpcnTpEvento;
  const CNPJ: String = ''; const IE: String = ''; Data: TDateTime = 0): String;
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

function TArquivosConfDCe.GetPathDCe(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
begin
  Result := GetPath(FPathDCe, 'DCe', CNPJ, IE, Data, 'DCe');
end;

procedure TArquivosConfDCe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarApenasDCeProcessados', SalvarApenasDCeProcessados);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathDCe', EmissaoPathDCe);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathDCe', PathDCe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

procedure TArquivosConfDCe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarApenasDCeProcessados := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarApenasDCeProcessados', SalvarApenasDCeProcessados);
  EmissaoPathDCe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathDCe', EmissaoPathDCe);
  NormatizarMunicipios := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'NormatizarMunicipios', NormatizarMunicipios);
  PathDCe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathDCe', PathDCe);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  PathArquivoMunicipios := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathArquivoMunicipios', PathArquivoMunicipios);
end;

end.
