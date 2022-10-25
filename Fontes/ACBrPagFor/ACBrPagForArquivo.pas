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

unit ACBrPagForArquivo;

interface

uses
  Classes, 
  Sysutils, 
  Contnrs,
  ACBrBase,
  ACBrPagForClass, 
  ACBrPagForConversao, 
  ACBrPagForConfiguracoes;

type
  TRegistro = class(TObject)
  private
    FACBrPagFor: TComponent;
    FPagFor: TPagFor;
    FPagForTXT: String;
    FNomeArq: String;
    FAlertas: String;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function GerarTxt: String;
    function Gravar(const CaminhoArquivo: String = ''): boolean;
    function Ler(const aArquivoTXT: String): boolean;

    property PagFor: TPagFor read FPagFor;
    property PagForTXT: String read FPagForTXT write FPagForTXT;
    property NomeArq: String read FNomeArq write FNomeArq;
    property Alertas: String read FAlertas;
  end;

  TArquivos = class(TACBrObjectList)
  private
    FConfiguracoes: TConfiguracoes;
    FACBrPagFor: TComponent;

    function GetItem(Index: Integer): TRegistro;
    procedure SetItem(Index: Integer; const Value: TRegistro);
  public
    constructor Create(AOwner: TPersistent);

    function New: TRegistro;
    function Add(ARegistro: TRegistro): Integer; reintroduce;
    Procedure Insert(Index: Integer; ARegistro: TRegistro); reintroduce;

    function Last: TRegistro;

    function GerarPagFor(const ANomeArquivo: String = ''): Boolean;
    function GetNamePath: string;
    function Gravar(const PathArquivo: string = ''): boolean;
    procedure Ler(const AArquivoTXT: String);

    function LoadFromFile(const aCaminhoArquivo: String): Boolean;
    function LoadFromStream(aStream: TStringStream): Boolean;
    function LoadFromString(aTxtString: String): Boolean;

    property Items[Index: Integer]: TRegistro read GetItem  write SetItem;
    property Configuracoes: TConfiguracoes read FConfiguracoes  write FConfiguracoes;
    property ACBrPagFor: TComponent read FACBrPagFor;
  end;

implementation

uses
  synautil,
  ACBrUtil.Strings,
  ACBrPagFor, ACBrPagForInterface;

{ TRegistro }

constructor TRegistro.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrPagFor) then
    raise EACBrPagForException.Create('AOwner deve ser do tipo TACBrPagFor');

  FACBrPagFor := TACBrPagFor(AOwner);
  FPagFor := TPagFor.Create;
end;

destructor TRegistro.Destroy;
begin
  FPagFor.Free;

  inherited;
end;

function TRegistro.GerarTxt: String;
var
  FProvider: IACBrPagForProvider;
begin
  FProvider := TACBrPagFor(FACBrPagFor).Provider;

  if not Assigned(FProvider) then
    raise EACBrPagForException.Create(ERR_SEM_BANCO);

  FProvider.GerarTxt(PagFor, FPagForTXT, FAlertas);
  Result := PagForTXT;
end;

function TRegistro.Gravar(const CaminhoArquivo: String): boolean;
var
  ArquivoGerado: TStringList;
  LocPagForTxt: String;
begin
  LocPagForTxt := PagForTXT;
  ArquivoGerado := TStringList.Create;
  try
    ArquivoGerado.Add(LocPagForTxt);

    // remove todas as linhas em branco do arquivo
    RemoveEmptyLines(ArquivoGerado);

    ArquivoGerado.SaveToFile(CaminhoArquivo);
    Result := True;
  finally
    ArquivoGerado.Free;
  end;
end;

function TRegistro.Ler(const aArquivoTXT: String): boolean;
var
  FProvider: IACBrPagForProvider;
  Ok: Boolean;
begin
  TACBrPagFor(FACBrPagFor).Configuracoes.Geral.Banco := StrToBanco(Ok, Copy(aArquivoTXT, 1, 3));

  FProvider := TACBrPagFor(FACBrPagFor).Provider;

  if not Assigned(FProvider) then
    raise EACBrPagForException.Create(ERR_SEM_BANCO);

  Result := FProvider.LerTxt(aArquivoTXT, FPagFor);
end;

{ TArquivos }

function TArquivos.New: TRegistro;
begin
  Result := TRegistro.Create(FACBrPagFor);
  Add(Result);
end;

function TArquivos.Add(ARegistro: TRegistro): Integer;
begin
  Result := inherited Add(ARegistro);
end;

constructor TArquivos.Create(AOwner: TPersistent);
begin
  if not (AOwner is TACBrPagFor ) then
    raise EACBrPagForException.Create( 'AOwner deve ser do tipo TACBrPagFor');

  inherited Create;

  FACBrPagFor := TACBrPagFor( AOwner );
  FConfiguracoes := TACBrPagFor(FACBrPagFor).Configuracoes;
end;

function TArquivos.GerarPagFor(const ANomeArquivo: String): Boolean;
var
  i: Integer;
begin
  for i:= 0 to Self.Count-1 do
    TACBrPagFor( FACBrPagFor ).Arquivos.Items[i].Gravar(ANomeArquivo);

  Result := True;
end;

function TArquivos.GetItem(Index: Integer): TRegistro;
begin
  Result := TRegistro(inherited Items[Index]);
end;

function TArquivos.Last: TRegistro;
begin
  Result := TRegistro(inherited Last);
end;

function TArquivos.GetNamePath: string;
begin
  Result := 'TRegistro';
end;

function TArquivos.Gravar(const PathArquivo: string): boolean;
var
  i: Integer;
  CaminhoArquivo: String;
begin
  Result := True;

  try
    for i:= 0 to TACBrPagFor( FACBrPagFor ).Arquivos.Count-1 do
    begin
      CaminhoArquivo := PathArquivo;
      TACBrPagFor( FACBrPagFor ).Arquivos.Items[i].Gravar(CaminhoArquivo);
    end;
  except
    Result := False;
  end;
end;

procedure TArquivos.Insert(Index: Integer; ARegistro: TRegistro);
begin
  inherited Insert(Index, ARegistro);
end;

procedure TArquivos.Ler(const AArquivoTXT: String);
begin
  TACBrPagFor(FACBrPagFor).Arquivos.New;
  TACBrPagFor(FACBrPagFor).Arquivos.Last.Ler(AArquivoTXT);
end;

procedure TArquivos.SetItem(Index: Integer; const Value: TRegistro);
begin
  inherited Items[Index] := Value;
end;

function TArquivos.LoadFromFile(const aCaminhoArquivo: String): Boolean;
var
  aTxt: string;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(aCaminhoArquivo);
    aTxt := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  Result := LoadFromString(aTxt);
end;

function TArquivos.LoadFromStream(aStream: TStringStream): Boolean;
var
  aTxt: string;
begin
  AStream.Position := 0;
  aTxt := string(ReadStrFromStream(aStream, aStream.Size));

  Result := Self.LoadFromString(aTxt);
end;

function TArquivos.LoadFromString(aTxtString: String): Boolean;
begin
  with Self.New do
  begin
    Ler(aTxtString);
  end;

  Result := Self.Count > 0;
end;

end.
