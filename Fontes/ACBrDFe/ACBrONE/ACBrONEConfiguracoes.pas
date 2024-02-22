{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrONEConfiguracoes;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrDFeConfiguracoes, pcnConversao, ACBrONEConversao;

type

  { TGeralConfONE }

  TGeralConfONE = class(TGeralConf)
  private
    FVersaoDF: TVersaoONE;
    FverAplic: String;
    FCNPJOper: String;

    procedure SetVersaoDF(const Value: TVersaoONE);
  public
    constructor Create(AOwner: TConfiguracoes); override;
    procedure Assign(DeGeralConfONE: TGeralConfONE); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

  published
    property VersaoDF: TVersaoONE read FVersaoDF write SetVersaoDF default ve200;
    property verAplic: String     read FverAplic write FverAplic;
    property CNPJOper: String     read FCNPJOper write FCNPJOper;
  end;

  { TArquivosConfONE }

  TArquivosConfONE = class(TArquivosConf)
  private
    FEmissaoPathONE: Boolean;
    FPathONE: String;
  public
    constructor Create(AOwner: TConfiguracoes); override;
    destructor Destroy; override;
    procedure Assign(DeArquivosConfONE: TArquivosConfONE); reintroduce;
    procedure GravarIni(const AIni: TCustomIniFile); override;
    procedure LerIni(const AIni: TCustomIniFile); override;

    function GetPathONE(Data: TDateTime = 0; const CNPJ: String = ''): String;
  published
    property EmissaoPathONE: Boolean read FEmissaoPathONE
      write FEmissaoPathONE default False;
    property PathONE: String read FPathONE write FPathONE;
  end;

  { TConfiguracoesONE }

  TConfiguracoesONE = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConfONE;
    function GetGeral: TGeralConfONE;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoesONE: TConfiguracoesONE); reintroduce;

  published
    property Geral: TGeralConfONE read GetGeral;
    property Arquivos: TArquivosConfONE read GetArquivos;
    property WebServices;
    property Certificados;
    property RespTec;
  end;

implementation

uses
  ACBrUtil.Base, ACBrONE,
  DateUtils;

{ TConfiguracoesONE }

constructor TConfiguracoesONE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSessaoIni := 'ONE';
  WebServices.ResourceName := 'ACBrONEServicos';
end;

procedure TConfiguracoesONE.Assign(DeConfiguracoesONE: TConfiguracoesONE);
begin
  Geral.Assign(DeConfiguracoesONE.Geral);
  WebServices.Assign(DeConfiguracoesONE.WebServices);
  Certificados.Assign(DeConfiguracoesONE.Certificados);
  Arquivos.Assign(DeConfiguracoesONE.Arquivos);
  RespTec.Assign(DeConfiguracoesONE.RespTec);
end;

function TConfiguracoesONE.GetArquivos: TArquivosConfONE;
begin
  Result := TArquivosConfONE(FPArquivos);
end;

function TConfiguracoesONE.GetGeral: TGeralConfONE;
begin
  Result := TGeralConfONE(FPGeral);
end;

procedure TConfiguracoesONE.CreateGeralConf;
begin
  FPGeral := TGeralConfONE.Create(Self);
end;

procedure TConfiguracoesONE.CreateArquivosConf;
begin
  FPArquivos := TArquivosConfONE.Create(self);
end;

{ TGeralConfONE }

constructor TGeralConfONE.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FVersaoDF := ve200;
end;

procedure TGeralConfONE.Assign(DeGeralConfONE: TGeralConfONE);
begin
  inherited Assign(DeGeralConfONE);

  VersaoDF := DeGeralConfONE.VersaoDF;
end;

procedure TGeralConfONE.SetVersaoDF(const Value: TVersaoONE);
begin
  FVersaoDF := Value;
end;

procedure TGeralConfONE.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF));
end;

procedure TGeralConfONE.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  VersaoDF := TVersaoONE(AIni.ReadInteger(fpConfiguracoes.SessaoIni, 'VersaoDF', Integer(VersaoDF)));
end;

{ TArquivosConfONE }

constructor TArquivosConfONE.Create(AOwner: TConfiguracoes);
begin
  inherited Create(AOwner);

  FEmissaoPathONE := False;
  FPathONE := '';
end;

destructor TArquivosConfONE.Destroy;
begin

  inherited;
end;

procedure TArquivosConfONE.Assign(DeArquivosConfONE: TArquivosConfONE);
begin
  inherited Assign(DeArquivosConfONE);

  FEmissaoPathONE             := DeArquivosConfONE.EmissaoPathONE;
  FPathONE                    := DeArquivosConfONE.PathONE;
end;

function TArquivosConfONE.GetPathONE(Data: TDateTime = 0; const CNPJ: String = ''): String;
begin
  Result := GetPath(FPathONE, ModeloDF, CNPJ, '', Data, ModeloDF);
end;

procedure TArquivosConfONE.GravarIni(const AIni: TCustomIniFile);
begin
  inherited GravarIni(AIni);

  AIni.WriteBool(fpConfiguracoes.SessaoIni, 'EmissaoPathONE', EmissaoPathONE);
  AIni.WriteString(fpConfiguracoes.SessaoIni, 'PathONE', PathONE);
end;

procedure TArquivosConfONE.LerIni(const AIni: TCustomIniFile);
begin
  inherited LerIni(AIni);

  EmissaoPathONE := AIni.ReadBool(fpConfiguracoes.SessaoIni, 'EmissaoPathONE', EmissaoPathONE);
  PathONE := AIni.ReadString(fpConfiguracoes.SessaoIni, 'PathONE', PathONE);
end;

end.
