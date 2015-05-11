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
  Classes, SysUtils, ACBrDFeConfiguracoes, pcnConversao, pcnConversaoNFe;

type

  { TGeralConfNFe }

  TGeralConfNFe = class(TGeralConf)
  private
    FModeloDF: TpcnModeloDF;
    FVersaoDF: TpcnVersaoDF;
    FModeloDFCodigo: integer;

    procedure SetModeloDF(AValue: TpcnModeloDF);
    procedure SetVersaoDF(const Value: TpcnVersaoDF);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfNFe: TGeralConfNFe); reintroduce;

  published
    property ModeloDF: TpcnModeloDF read FModeloDF write SetModeloDF default moNFe;
    property VersaoDF: TpcnVersaoDF read FVersaoDF write SetVersaoDF default ve310;
    property ModeloDFCodigo: integer read FModeloDFCodigo;
  end;

  { TArquivosConfNFe }

  TArquivosConfNFe = class(TArquivosConf)
  private
    FEmissaoPathNFe: boolean;
    FSalvarEvento: boolean;
    FSalvarApenasNFeProcessadas: boolean;
    FPathNFe: String;
    FPathCan: String;
    FPathInu: String;
    FPathCCe: String;
    FPathEvento: String;
    FPathDownload: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeArquivosConfNFe: TArquivosConfNFe); reintroduce;

    function GetPathCan(CNPJ: String = ''): String;
    function GetPathInu(CNPJ: String = ''): String;
    function GetPathNFe(Data: TDateTime = 0; CNPJ: String = ''): String;
    function GetPathCCe(CNPJ: String = ''): String;
    function GetPathEvento(tipoEvento: TpcnTpEvento; CNPJ: String = ''): String;
    function GetPathDownload(CNPJ: String = ''): String;
  published
    property EmissaoPathNFe: boolean read FEmissaoPathNFe
      write FEmissaoPathNFe default False;
    property SalvarCCeCanEvento: boolean read FSalvarEvento
      write FSalvarEvento default False;
    property SalvarApenasNFeProcessadas: boolean
      read FSalvarApenasNFeProcessadas write FSalvarApenasNFeProcessadas default False;
    property PathNFe: String read FPathNFe write FPathNFe;
    property PathCan: String read FPathCan write FPathCan;
    property PathInu: String read FPathInu write FPathInu;
    property PathCCe: String read FPathCCe write FPathCCe;
    property PathEvento: String read FPathEvento write FPathEvento;
    property PathDownload: String read FPathDownload write FPathDownload;
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

  published
    property Geral: TGeralConfNFe read GetGeral;
    property Arquivos: TArquivosConfNFe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil,
  DateUtils;

{ TConfiguracoesNFe }


constructor TConfiguracoesNFe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  WebServices.ResourceName := 'ACBrNFeServicos';
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
  FVersaoDF := ve200;
end;

procedure TGeralConfNFe.Assign(DeGeralConfNFe: TGeralConfNFe);
begin
  inherited Assign(DeGeralConfNFe);

  FModeloDF       := DeGeralConfNFe.ModeloDF;
  FVersaoDF       := DeGeralConfNFe.VersaoDF;
end;

procedure TGeralConfNFe.SetModeloDF(AValue: TpcnModeloDF);
begin
  FModeloDF := AValue;
  FModeloDFCodigo := StrToInt(ModeloDFToStr(FModeloDF));
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
  FPathNFe := '';
  FPathCan := '';
  FPathInu := '';
  FPathCCe := '';
  FPathEvento := '';
  FPathDownload := '';
end;

procedure TArquivosConfNFe.Assign(DeArquivosConfNFe: TArquivosConfNFe);
begin
  inherited Assign(DeArquivosConfNFe);

  FEmissaoPathNFe             := DeArquivosConfNFe.EmissaoPathNFe;
  FSalvarEvento               := DeArquivosConfNFe.SalvarCCeCanEvento;
  FSalvarApenasNFeProcessadas := DeArquivosConfNFe.SalvarApenasNFeProcessadas;
  FPathNFe                    := DeArquivosConfNFe.PathNFe;
  FPathCan                    := DeArquivosConfNFe.PathCan;
  FPathInu                    := DeArquivosConfNFe.PathInu;
  FPathCCe                    := DeArquivosConfNFe.PathCCe;
  FPathEvento                 := DeArquivosConfNFe.PathEvento;
  FPathDownload               := DeArquivosConfNFe.PathDownload;
end;

function TArquivosConfNFe.GetPathCan(CNPJ: String = ''): String;
begin
  Result := GetPath(FPathCan, 'Can', CNPJ);
end;

function TArquivosConfNFe.GetPathCCe(CNPJ: String = ''): String;
begin
  Result := GetPath(FPathCCe, 'CCe', CNPJ);
end;

function TArquivosConfNFe.GetPathDownload(CNPJ: String): String;
begin
  Result := GetPath(FPathDownload, 'Down', CNPJ);
end;

function TArquivosConfNFe.GetPathEvento(tipoEvento: TpcnTpEvento;
  CNPJ: String = ''): String;
var
  Dir, Evento: String;
begin
  Dir := GetPath(FPathEvento, 'Evento', CNPJ);

  if AdicionarLiteral then
  begin
    case tipoEvento of
      teCCe: Evento := 'CCe';
      teCancelamento: Evento := 'Cancelamento';
      teEPECNFe: Evento := 'EPEC';
      teManifDestConfirmacao: Evento := 'Confirmacao';
      teManifDestCiencia: Evento := 'Ciencia';
      teManifDestDesconhecimento: Evento := 'Desconhecimento';
      teManifDestOperNaoRealizada: Evento := 'NaoRealizada';
    end;

    Dir := PathWithDelim(Dir) + Evento;
  end;

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Result := Dir;
end;


function TArquivosConfNFe.GetPathInu(CNPJ: String = ''): String;
begin
  Result := GetPath(FPathInu, 'Inu', CNPJ);
end;

function TArquivosConfNFe.GetPathNFe(Data: TDateTime = 0; CNPJ: String = ''): String;
begin
  Result := GetPath(FPathNFe, 'NFe', CNPJ, Data);
end;


end.
