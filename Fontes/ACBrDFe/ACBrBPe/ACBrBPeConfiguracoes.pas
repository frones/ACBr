{******************************************************************************}
{ Projeto: Componente ACBrBPe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Bilhete de }
{ Passagem Eletrônica - BPe                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2017                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrBPeConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, pcnConversao, pcnConversaoBPe;

type

  { TGeralConfBPe }

  TGeralConfBPe = class(TGeralConf)
  private
    FVersaoDF: TVersaoBPe;

    procedure SetVersaoDF(const Value: TVersaoBPe);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfBPe: TGeralConfBPe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property VersaoDF: TVersaoBPe read FVersaoDF write SetVersaoDF default ve100;
  end;

  { TDownloadConfBPe }

  TDownloadConfBPe = class(TPersistent)
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

  { TArquivosConfBPe }

  TArquivosConfBPe = class(TArquivosConf)
  private
    FEmissaoPathBPe: Boolean;
    FSalvarEvento: Boolean;
    FSalvarApenasBPeProcessadas: Boolean;
    FPathBPe: String;
    FPathEvento: String;
    FDownloadBPe: TDownloadConfBPe;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfBPe: TArquivosConfBPe); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathBPe(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; CNPJ: String = ''; Data: TDateTime = 0): String;
    function GetPathDownload(xNome: String = ''; CNPJ: String = ''; Data: TDateTime = 0): String;
  published
    property EmissaoPathBPe: Boolean read FEmissaoPathBPe
      write FEmissaoPathBPe default False;
    property SalvarEvento: Boolean read FSalvarEvento
      write FSalvarEvento default False;
    property SalvarApenasBPeProcessadas: Boolean
      read FSalvarApenasBPeProcessadas write FSalvarApenasBPeProcessadas default False;
    property PathBPe: String read FPathBPe write FPathBPe;
    property PathEvento: String read FPathEvento write FPathEvento;
    property DownloadBPe: TDownloadConfBPe read FDownloadBPe write FDownloadBPe;
  end;

  { TConfiguracoesBPe }

  TConfiguracoesBPe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfBPe;
    function GetGeral: TGeralConfBPe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesBPe: TConfiguracoesBPe); reintroduce;

  published
    property Geral: TGeralConfBPe read GetGeral;
    property Arquivos: TArquivosConfBPe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil, ACBrBPe,
  DateUtils;

{ TConfiguracoesBPe }

constructor TConfiguracoesBPe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'BPe';
  WebServices.ResourceName := 'ACBrBPeServicos';
end;

procedure TConfiguracoesBPe.Assign(DeConfiguracoesBPe: TConfiguracoesBPe);
begin
  Geral.Assign(DeConfiguracoesBPe.Geral);
  WebServices.Assign(DeConfiguracoesBPe.WebServices);
  Certificados.Assign(DeConfiguracoesBPe.Certificados);
  Arquivos.Assign(DeConfiguracoesBPe.Arquivos);
end;

function TConfiguracoesBPe.GetArquivos: TArquivosConfBPe;
begin
  Result := TArquivosConfBPe(FPArquivos);
end;

function TConfiguracoesBPe.GetGeral: TGeralConfBPe;
begin
  Result := TGeralConfBPe(FPGeral);
end;

procedure TConfiguracoesBPe.CreateGeralConf;
begin
  FPGeral := TGeralConfBPe.Create(Self);
end;

procedure TConfiguracoesBPe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfBPe.Create(self);
end;

{ TGeralConfBPe }

constructor TGeralConfBPe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF := ve100;
end;

procedure TGeralConfBPe.Assign(DeGeralConfBPe: TGeralConfBPe);
begin
  inherited Assign(DeGeralConfBPe);

  VersaoDF := DeGeralConfBPe.VersaoDF;
end;

procedure TGeralConfBPe.SetVersaoDF(const Value: TVersaoBPe);
begin
  FVersaoDF := Value;
end;

procedure TGeralConfBPe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
end;

procedure TGeralConfBPe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  VersaoDF := TVersaoBPe(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
end;

{ TDownloadConfBPe }

procedure TDownloadConfBPe.Assign(Source: TPersistent);
begin
  if Source is TDownloadConfBPe then
  begin
    FPathDownload := TDownloadConfBPe(Source).PathDownload;
    FSepararPorNome := TDownloadConfBPe(Source).SepararPorNome;
  end
  else
    inherited Assign(Source);
end;

constructor TDownloadConfBPe.Create;
begin
  FPathDownload := '';
  FSepararPorNome := False;
end;

{ TArquivosConfBPe }

constructor TArquivosConfBPe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FDownloadBPe := TDownloadConfBPe.Create;
  FEmissaoPathBPe := False;
  FSalvarEvento := False;
  FSalvarApenasBPeProcessadas := False;
  FPathBPe := '';
  FPathEvento := '';
end;

destructor TArquivosConfBPe.Destroy;
begin
  FDownloadBPe.Free;

  inherited;
end;

procedure TArquivosConfBPe.Assign(DeArquivosConfBPe: TArquivosConfBPe);
begin
  inherited Assign(DeArquivosConfBPe);

  EmissaoPathBPe             := DeArquivosConfBPe.EmissaoPathBPe;
  SalvarEvento               := DeArquivosConfBPe.SalvarEvento;
  SalvarApenasBPeProcessadas := DeArquivosConfBPe.SalvarApenasBPeProcessadas;
  PathBPe                    := DeArquivosConfBPe.PathBPe;
  PathEvento                 := DeArquivosConfBPe.PathEvento;

  FDownloadBPe.Assign(DeArquivosConfBPe.DownloadBPe);
end;

function TArquivosConfBPe.GetPathEvento(tipoEvento: TpcnTpEvento; CNPJ: String;
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

function TArquivosConfBPe.GetPathBPe(Data: TDateTime = 0; CNPJ: String = ''): String;
begin
  Result := GetPath(FPathBPe, ModeloDF, CNPJ, Data, ModeloDF);
end;

function TArquivosConfBPe.GetPathDownload(xNome, CNPJ: String;
  Data: TDateTime): String;
var
  rPathDown: String;
begin
  rPathDown := '';
  if EstaVazio(FDownloadBPe.PathDownload) then
     FDownloadBPe.PathDownload := PathSalvar;

  if (FDownloadBPe.SepararPorNome) and (NaoEstaVazio(xNome)) then
     rPathDown := rPathDown + PathWithDelim(FDownloadBPe.PathDownload) + TiraAcentos(xNome)
  else
     rPathDown := FDownloadBPe.PathDownload;

  Result := GetPath(rPathDown, 'Down', CNPJ, Data);
end;

procedure TArquivosConfBPe.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'SalvarApenasBPeProcessadas', SalvarApenasBPeProcessadas);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathBPe', EmissaoPathBPe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathBPe', PathBPe);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'Download.PathDownload', DownloadBPe.PathDownload);
  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'Download.SepararPorNome', DownloadBPe.SepararPorNome);
end;

procedure TArquivosConfBPe.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  SalvarEvento := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarEvento', SalvarEvento);
  SalvarApenasBPeProcessadas := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'SalvarApenasBPeProcessadas', SalvarApenasBPeProcessadas);
  EmissaoPathBPe := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathBPe', EmissaoPathBPe);
  PathBPe := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathBPe', PathBPe);
  PathEvento := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathEvento', PathEvento);
  DownloadBPe.PathDownload := AIni.ReadString(fpConfiguracoes.SessaoIni, 'Download.PathDownload', DownloadBPe.PathDownload);
  DownloadBPe.SepararPorNome := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'Download.SepararPorNome', DownloadBPe.SepararPorNome);
end;

end.
