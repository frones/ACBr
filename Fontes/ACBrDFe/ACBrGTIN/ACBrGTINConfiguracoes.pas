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

unit ACBrGTINConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes,
  pcnConversao,
  ACBrGTINConversao;

type

  { TGeralConfGTIN }

  TGeralConfGTIN = class(TGeralConf)
  private
    FVersaoDF: TVersaoGTIN;

    procedure SetVersaoDF(const Value: TVersaoGTIN);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfGTIN: TGeralConfGTIN); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property VersaoDF: TVersaoGTIN read FVersaoDF write SetVersaoDF default ve100;
  end;

  { TArquivosConfGTIN }

  TArquivosConfGTIN = class(TArquivosConf)
  private
    FPathGTIN: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfGTIN: TArquivosConfGTIN); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathGTIN(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
  published
    property PathGTIN: String read FPathGTIN write FPathGTIN;
  end;

  { TConfiguracoesGTIN }

  TConfiguracoesGTIN = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfGTIN;
    function GetGeral: TGeralConfGTIN;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesGTIN: TConfiguracoesGTIN); reintroduce;

  published
    property Geral: TGeralConfGTIN read GetGeral;
    property Arquivos: TArquivosConfGTIN read GetArquivos;
    property WebServices;
    property Certificados;
  //property RespTec;
  end;

implementation

uses
  DateUtils, ACBrUtil.FilesIO;

{ TConfiguracoesGTIN }

constructor TConfiguracoesGTIN.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'GTIN';
  WebServices.ResourceName := 'ACBrGTINServicos';
end;

function TConfiguracoesGTIN.GetArquivos: TArquivosConfGTIN;
begin
  Result := TArquivosConfGTIN(FPArquivos);
end;

function TConfiguracoesGTIN.GetGeral: TGeralConfGTIN;
begin
  Result := TGeralConfGTIN(FPGeral);
end;

procedure TConfiguracoesGTIN.CreateGeralConf;
begin
  FPGeral := TGeralConfGTIN.Create(Self);
end;

procedure TConfiguracoesGTIN.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfGTIN.Create(self);
end;

procedure TConfiguracoesGTIN.Assign(DeConfiguracoesGTIN: TConfiguracoesGTIN);
begin
  Geral.Assign(DeConfiguracoesGTIN.Geral);
  WebServices.Assign(DeConfiguracoesGTIN.WebServices);
  Certificados.Assign(DeConfiguracoesGTIN.Certificados);
  Arquivos.Assign(DeConfiguracoesGTIN.Arquivos);
  //RespTec.Assign(DeConfiguracoesGTIN.RespTec);
end;

{ TGeralConfGTIN }

procedure TGeralConfGTIN.Assign(DeGeralConfGTIN: TGeralConfGTIN);
begin
  inherited Assign(DeGeralConfGTIN);

  FVersaoDF := DeGeralConfGTIN.VersaoDF;
end;

constructor TGeralConfGTIN.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF := ve100;
end;

procedure TGeralConfGTIN.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
end;

procedure TGeralConfGTIN.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  VersaoDF := TVersaoGTIN(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
end;

procedure TGeralConfGTIN.SetVersaoDF(const Value: TVersaoGTIN);
begin
  FVersaoDF := Value;
end;

{ TArquivosConfGTIN }

procedure TArquivosConfGTIN.Assign(DeArquivosConfGTIN: TArquivosConfGTIN);
begin
  inherited Assign(DeArquivosConfGTIN);

  FPathGTIN := DeArquivosConfGTIN.PathGTIN;
end;

constructor TArquivosConfGTIN.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FPathGTIN := '';
end;

destructor TArquivosConfGTIN.Destroy;
begin

  inherited;
end;

function TArquivosConfGTIN.GetPathGTIN(Data: TDateTime = 0; const CNPJ: String = ''; const IE: String = ''): String;
begin
  Result := GetPath(FPathGTIN, 'GTIN', CNPJ, IE, Data, 'GTIN');
end;

procedure TArquivosConfGTIN.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathGTIN', PathGTIN);
end;

procedure TArquivosConfGTIN.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  PathGTIN := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathGTIN', PathGTIN);
end;

end.
