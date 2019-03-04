{******************************************************************************}
{ Projeto: Componente ACBrANe                                                  }
{  Biblioteca multiplataforma de componentes Delphi                            }
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
|* 24/02/2016: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrANeConfiguracoes;

interface

uses
  Classes, SysUtils,
  ACBrDFeConfiguracoes, pcnConversao, pcaConversao;

type

  { TGeralConfANe }

  TGeralConfANe = class(TGeralConf)
  private
    FVersaoDF: TVersaoANe;
    FTipoDoc: TTipoDoc;
    FUsuario: String;
    FSenha: String;
    FCodATM: String;
    FCNPJEmitente: String;

    procedure SetVersaoDF(const Value: TVersaoANe);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfANe: TGeralConfANe); reintroduce;

  published
    property TipoDoc: TTipoDoc read FTipoDoc write FTipoDoc;
    property VersaoDF: TVersaoANe read FVersaoDF write SetVersaoDF default ve200;
    property Usuario: String read FUsuario write FUsuario;
    property Senha: String read FSenha write FSenha;
    property CodATM: String read FCodATM write FCodATM;
    property CNPJEmitente: String read FCNPJEmitente write FCNPJEmitente;
  end;

  { TArquivosConfANe }

  TArquivosConfANe = class(TArquivosConf)
  private
    FEmissaoPathANe: boolean;
    FPathANe: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfANe: TArquivosConfANe); reintroduce;

    function GetPathANe(Data: TDateTime = 0; const CNPJ: String = ''): String;
  published
    property EmissaoPathANe: boolean read FEmissaoPathANe
      write FEmissaoPathANe default False;
    property PathANe: String read FPathANe write FPathANe;
  end;

  { TConfiguracoesANe }

  TConfiguracoesANe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfANe;
    function GetGeral: TGeralConfANe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesANe: TConfiguracoesANe); reintroduce;

  published
    property Geral: TGeralConfANe read GetGeral;
    property Arquivos: TArquivosConfANe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil, DateUtils;

{ TConfiguracoesANe }

constructor TConfiguracoesANe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  WebServices.ResourceName := 'ACBrANeServicos';
end;

function TConfiguracoesANe.GetArquivos: TArquivosConfANe;
begin
  Result := TArquivosConfANe(FPArquivos);
end;

function TConfiguracoesANe.GetGeral: TGeralConfANe;
begin
  Result := TGeralConfANe(FPGeral);
end;

procedure TConfiguracoesANe.CreateGeralConf;
begin
  FPGeral := TGeralConfANe.Create(Self);
end;

procedure TConfiguracoesANe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfANe.Create(self);
end;

procedure TConfiguracoesANe.Assign(DeConfiguracoesANe: TConfiguracoesANe);
begin
  Geral.Assign(DeConfiguracoesANe.Geral);
  WebServices.Assign(DeConfiguracoesANe.WebServices);
  Certificados.Assign(DeConfiguracoesANe.Certificados);
  Arquivos.Assign(DeConfiguracoesANe.Arquivos);
end;

{ TGeralConfANe }

procedure TGeralConfANe.Assign(DeGeralConfANe: TGeralConfANe);
begin
  inherited Assign(DeGeralConfANe);

  FVersaoDF := DeGeralConfANe.VersaoDF;
end;

constructor TGeralConfANe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF := ve200;
  FTipoDoc := tdCTe;
  FUsuario := '';
  FSenha := '';
  FCodATM := '';
  FCNPJEmitente := '';
end;

procedure TGeralConfANe.SetVersaoDF(const Value: TVersaoANe);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfANe }

procedure TArquivosConfANe.Assign(DeArquivosConfANe: TArquivosConfANe);
begin
  inherited Assign(DeArquivosConfANe);

  FEmissaoPathANe := DeArquivosConfANe.EmissaoPathANe;
  FPathANe        := DeArquivosConfANe.PathANe;
end;

constructor TArquivosConfANe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathANe := False;
  FPathANe := '';
end;

destructor TArquivosConfANe.Destroy;
begin

  inherited;
end;

function TArquivosConfANe.GetPathANe(Data: TDateTime = 0; const CNPJ: String = ''): String;
begin
  Result := GetPath(FPathANe, 'ANe', CNPJ, Data);
end;

end.
