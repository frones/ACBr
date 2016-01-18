{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
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

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit ACBrGNREConfiguracoes;

interface

uses
  Classes, SysUtils, ACBrDFeConfiguracoes, pcnConversao, pgnreConversao;

type

  { TGeralConfGNRe }

  TGeralConfGNRe = class(TGeralConf)
  private
    FVersaoDF: TpcnVersaoDF;

    procedure SetVersaoDF(const Value: TpcnVersaoDF);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfGNRe: TGeralConfGNRe); overload;

  published
    property VersaoDF: TpcnVersaoDF read FVersaoDF write SetVersaoDF default ve100;
  end;

  { TArquivosConfGNRe }

  TArquivosConfGNRe = class(TArquivosConf)
  private
    FEmissaoPathGNRe: boolean;
    FSalvarApenasGNReProcessadas: boolean;
    FPathGNRe: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeArquivosConfGNRe: TArquivosConfGNRe); overload;

    function GetPathGNRe(Data: TDateTime = 0; CNPJ: String = ''): String;
  published
    property EmissaoPathGNRe: boolean read FEmissaoPathGNRe
      write FEmissaoPathGNRe default False;
    property SalvarApenasGNReProcessadas: boolean
      read FSalvarApenasGNReProcessadas write FSalvarApenasGNReProcessadas default False;
    property PathGNRe: String read FPathGNRe write FPathGNRe;
  end;

  { TConfiguracoesGNRe }

  TConfiguracoesGNRe = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfGNRe;
    function GetGeral: TGeralConfGNRe;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesGNRe: TConfiguracoesGNRe); overload;

  published
    property Geral: TGeralConfGNRe read GetGeral;
    property Arquivos: TArquivosConfGNRe read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
  ACBrUtil,
  DateUtils;

{ TGeralConfGNRe }

constructor TGeralConfGNRe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF := ve100;
end;

procedure TGeralConfGNRe.Assign(DeGeralConfGNRe: TGeralConfGNRe);
begin
  inherited Assign(DeGeralConfGNRe);

  VersaoDF := DeGeralConfGNRe.VersaoDF;
end;

procedure TGeralConfGNRe.SetVersaoDF(const Value: TpcnVersaoDF);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfGNRe }

constructor TArquivosConfGNRe.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathGNRe := False;
  FSalvarApenasGNReProcessadas := False;
  FPathGNRe := '';
end;

procedure TArquivosConfGNRe.Assign(DeArquivosConfGNRe: TArquivosConfGNRe);
begin
  inherited Assign(DeArquivosConfGNRe);

  EmissaoPathGNRe             := DeArquivosConfGNRe.EmissaoPathGNRe;
  SalvarApenasGNReProcessadas := DeArquivosConfGNRe.SalvarApenasGNReProcessadas;
  PathGNRe                    := DeArquivosConfGNRe.PathGNRe;
end;

function TArquivosConfGNRe.GetPathGNRe(Data: TDateTime;
  CNPJ: String): String;
begin
  Result := GetPath(FPathGNRe, 'GNRe', CNPJ, Data);
end;

{ TConfiguracoesGNRe }

constructor TConfiguracoesGNRe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  WebServices.ResourceName := 'ACBrGNReServicos';
end;

procedure TConfiguracoesGNRe.Assign(DeConfiguracoesGNRe: TConfiguracoesGNRe);
begin
  Geral.Assign(DeConfiguracoesGNRe.Geral);
  WebServices.Assign(DeConfiguracoesGNRe.WebServices);
  Certificados.Assign(DeConfiguracoesGNRe.Certificados);
  Arquivos.Assign(DeConfiguracoesGNRe.Arquivos);
end;

function TConfiguracoesGNRe.GetArquivos: TArquivosConfGNRe;
begin
  Result := TArquivosConfGNRe(FPArquivos);
end;

function TConfiguracoesGNRe.GetGeral: TGeralConfGNRe;
begin
  Result := TGeralConfGNRe(FPGeral);
end;

procedure TConfiguracoesGNRe.CreateGeralConf;
begin
  FPGeral := TGeralConfGNRe.Create(Self);
end;

procedure TConfiguracoesGNRe.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfGNRe.Create(Self);
end;

end.
