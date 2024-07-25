{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit ACBrDebitoAutomaticoConfiguracoes;

interface

uses
  Classes, Forms, SysUtils,
  ACBrDebitoAutomaticoConversao;

type
  TConfiguracoes = class;

  { TGeralConf }

  TGeralConf = class(TComponent)
  private
    FBanco: TBanco;
    FLayoutVersao: TDebitoLayoutVersao;
    FCodigoBanco: String;
    FCNPJEmpresa: string;

    procedure SetBanco(const Value: TBanco);
    procedure SetLayoutVersao(const Value: TDebitoLayoutVersao);
  protected
    fpConfiguracoes: TConfiguracoes;
  public
    constructor Create(AConfiguracoes: TConfiguracoes); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure Assign(DeGeralConf: TGeralConf); reintroduce; virtual;
  published
    property Banco: TBanco read FBanco write SetBanco default debNenhum;
    property LayoutVersao: TDebitoLayoutVersao read FLayoutVersao write SetLayoutVersao default lv4;
    property CodigoBanco: String read FCodigoBanco;
    property CNPJEmpresa: String read FCNPJEmpresa write FCNPJEmpresa;
  end;

  TArquivosConf = class(TComponent)
  private
    FSalvar: Boolean;
    FSepararPorCNPJ: Boolean;
    FSepararPorMes: Boolean;
    FAdicionarLiteral: Boolean;
    FPathSalvar: String;

    function GetPathSalvar: String;
  protected
    fpConfiguracoes: TConfiguracoes;
  public
    constructor Create(AConfiguracoes: TConfiguracoes); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure Assign(DeArquivosConf: TArquivosConf); reintroduce; virtual;

    function GetPath(const APath, ALiteral, CNPJ: String; Data: TDateTime): String; virtual;
  published
    property PathSalvar: String read GetPathSalvar write FPathSalvar;
    property Salvar: Boolean read FSalvar write FSalvar default False;
    property AdicionarLiteral: Boolean read FAdicionarLiteral write FAdicionarLiteral default False;
    property SepararPorCNPJ: Boolean read FSepararPorCNPJ write FSepararPorCNPJ default False;
    property SepararPorMes: Boolean read FSepararPorMes write FSepararPorMes default False;
  end;

  TConfiguracoes = class(TComponent)
  protected
    FPGeral: TGeralConf;
    FPArquivos: TArquivosConf;

    procedure CreateGeralConf; virtual;
    procedure CreateArquivosConf; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(DeConfiguracoes: TConfiguracoes); reintroduce; virtual;

    procedure Limpar;
  published
    property Geral: TGeralConf read FPGeral;
    property Arquivos: TArquivosConf read FPArquivos;
  end;

implementation

uses
  Math, StrUtils, DateUtils,
  ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrDebitoAutomatico;

{ TConfiguracoes }

procedure TConfiguracoes.Assign(DeConfiguracoes: TConfiguracoes);
begin
  Geral.Assign(DeConfiguracoes.Geral);
  Arquivos.Assign(DeConfiguracoes.Arquivos);
end;

constructor TConfiguracoes.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  CreateGeralConf;
  FPGeral.Name  := 'GeralConf';
  {$IFDEF COMPILER6_UP}
   FPGeral.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}

  CreateArquivosConf;
  FPArquivos.Name  := 'ArquivosConf';
  {$IFDEF COMPILER6_UP}
   FPArquivos.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
end;

procedure TConfiguracoes.CreateArquivosConf;
begin
  FPArquivos := TArquivosConf.Create(self);
end;

procedure TConfiguracoes.CreateGeralConf;
begin
  FPGeral := TGeralConf.Create(Self);
end;

destructor TConfiguracoes.Destroy;
begin
  FPGeral.Free;
  FPArquivos.Free;

  inherited;
end;

procedure TConfiguracoes.Limpar;
begin
  FPGeral.FBanco := debNenhum;

  FPArquivos.FSalvar := False;
  FPArquivos.FSepararPorCNPJ := False;
  FPArquivos.FSepararPorMes := False;
  FPArquivos.FAdicionarLiteral := False;
  FPArquivos.FPathSalvar := '';
end;

{ TGeralConf }

procedure TGeralConf.Assign(DeGeralConf: TGeralConf);
begin
  Banco := DeGeralConf.Banco;
  FCodigoBanco := DeGeralConf.CodigoBanco;
  CNPJEmpresa := DeGeralConf.CNPJEmpresa;
end;

constructor TGeralConf.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  fpConfiguracoes := AConfiguracoes;

  FBanco := debNenhum;
  FLayoutVersao := lv4;
  FCodigoBanco := '000';
  FCNPJEmpresa := '';
end;

destructor TGeralConf.Destroy;
begin

  inherited;
end;

procedure TGeralConf.SetBanco(const Value: TBanco);
begin
  FBanco := Value;
  FCodigoBanco := BancoToStr(FBanco);

  if Assigned(fpConfiguracoes.Owner) then
    TACBrDebitoAutomatico(fpConfiguracoes.Owner).SetProvider;

  if FBanco = debNenhum then
    raise Exception.Create('Banco não Encontrado.');
end;

procedure TGeralConf.SetLayoutVersao(const Value: TDebitoLayoutVersao);
begin
  FLayoutVersao := Value;
end;

{ TArquivosConf }

procedure TArquivosConf.Assign(DeArquivosConf: TArquivosConf);
begin
  PathSalvar := DeArquivosConf.PathSalvar;
  Salvar := DeArquivosConf.Salvar;
  AdicionarLiteral := DeArquivosConf.AdicionarLiteral;
  SepararPorCNPJ := DeArquivosConf.SepararPorCNPJ;
  SepararPorMes := DeArquivosConf.SepararPorMes;
end;

constructor TArquivosConf.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  fpConfiguracoes := AConfiguracoes;
  FSalvar := True;
  FPathSalvar := '';
  FSepararPorMes := False;
  FAdicionarLiteral := False;
  FSepararPorCNPJ := False;
end;

destructor TArquivosConf.Destroy;
begin

  inherited;
end;

function TArquivosConf.GetPathSalvar: String;
begin
  if FPathSalvar = '' then
    FPathSalvar := ExtractFilePath(Application.ExeName);

  FPathSalvar := PathWithDelim(Trim(FPathSalvar));
  Result := FPathSalvar;
end;

function TArquivosConf.GetPath(const APath, ALiteral, CNPJ: String;
  Data: TDateTime): String;
var
  wDia, wMes, wAno: word;
  Dir, AnoMes, UmCNPJ: String;
  LenLiteral: integer;
begin
  if APath = '' then
    Dir := PathSalvar
  else
    Dir := APath;

  if SepararPorCNPJ then
  begin
    UmCNPJ := CNPJ;
    if UmCNPJ = '' then
      UmCNPJ := TConfiguracoes(Self.Owner).Geral.CNPJEmpresa;

    if UmCNPJ <> '' then
      Dir := PathWithDelim(Dir) + UmCNPJ;
  end;

  if SepararPorMes then
  begin
    if Data = 0 then
      Data := Now;

    DecodeDate(Data, wAno, wMes, wDia);
    AnoMes := IntToStr(wAno) + IntToStrZero(wMes, 2);

    if Pos(AnoMes, Dir) <= 0 then
      Dir := PathWithDelim(Dir) + AnoMes;
  end;

  LenLiteral := Length(ALiteral);
  if AdicionarLiteral and (LenLiteral > 0) then
  begin
    if RightStr(Dir, LenLiteral) <> ALiteral then
      Dir := PathWithDelim(Dir) + ALiteral;
  end;

  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Result := Dir;
end;

end.
