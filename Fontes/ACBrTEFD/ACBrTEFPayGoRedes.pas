{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrTEFPayGoRedes;

interface

uses
  Classes, SysUtils
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   ,System.Generics.Collections, System.Generics.Defaults
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   ,System.Contnrs
  {$Else}
   ,Contnrs
  {$IfEnd}
  ;

const
  CACBrTEFPayGoRedesTxt = 'RedesPayGo.txt';
  CACBrTEFPayGoRedesRes = 'RedesPayGo';

type
  { TACBrTEFPayGoRede }

  TACBrTEFPayGoRede = class
  private
    FCNPJ: String;
    FCodRede: Integer;
    FCodSATCFe: Integer;
    FNomeTrad: String;
    FNomePGWeb: String;
  public
    constructor Create;
    procedure Clear;

    property Codigo: Integer read FCodRede write FCodRede;
    property NomeTrad: String read FNomeTrad write FNomeTrad;
    property NomePGWeb: String read FNomePGWeb write FNomePGWeb;
    property CodSATCFe: Integer read FCodSATCFe write FCodSATCFe;
    property CNPJ: String read FCNPJ write FCNPJ;
  end;

  { TACBrTEFPayGoTabelaRedes }

  TACBrTEFPayGoTabelaRedes = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrTEFPayGoRede>{$EndIf})
  private
    procedure LoadFromStringList(AStringList: TStrings);

  protected
    FFileName: String;
    FResourceName: String;
    FLoaded: Boolean;
    function LoadFromFile: Boolean;
    function LoadFromResource: Boolean;
    procedure SetFileName(AValue: String);

    procedure SetObject (Index: Integer; Item: TACBrTEFPayGoRede);
    function GetObject (Index: Integer): TACBrTEFPayGoRede;
    procedure Insert (Index: Integer; Obj: TACBrTEFPayGoRede);
  public
    constructor Create(AFileName: String = ''; AResourceName: String = '');

    procedure Load;
    function Find(ACodigo: Integer): TACBrTEFPayGoRede;
    function FindPGWeb(ANomePGWeb: String): TACBrTEFPayGoRede;

    property FileName: String read FFileName write SetFileName;
    property ResourceName: String read FResourceName;

    function Add(Obj: TACBrTEFPayGoRede): Integer;
    property Objects [Index: Integer]: TACBrTEFPayGoRede read GetObject write SetObject; default;
  end;

var
  TabelaRedes: TACBrTEFPayGoTabelaRedes;

implementation

{$IFDEF FPC}
 {$R ACBrTEFPayGo.rc}
{$ELSE}
 {$R ACBrTEFPayGo.res}
{$ENDIF}

uses
  Types,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrConsts;

{ TACBrTEFPayGoRede }

constructor TACBrTEFPayGoRede.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFPayGoRede.Clear;
begin
  FCNPJ := '';
  FNomeTrad := '';
  FNomePGWeb := '';
  FCodRede := 0;
  FCodSATCFe := 0;
end;

{ TACBrTEFPayGoTabelaRedes }

constructor TACBrTEFPayGoTabelaRedes.Create(AFileName: String; AResourceName: String);
begin
  inherited Create(True);

  if AFileName = '' then
    FFileName := ApplicationPath + CACBrTEFPayGoRedesTxt
  else
    FFileName := AFileName;

  if AResourceName = '' then
    FResourceName := CACBrTEFPayGoRedesRes
  else
    FResourceName := AResourceName;

  Clear;
end;

procedure TACBrTEFPayGoTabelaRedes.Load;
begin
  Clear;
  FLoaded := LoadFromFile or LoadFromResource;
end;

procedure TACBrTEFPayGoTabelaRedes.LoadFromStringList(AStringList: TStrings);
var
  Lin: String;
  i: Integer;
  Colunas: TSplitResult;
  ARede: TACBrTEFPayGoRede;
begin
  i := 0;
  while i < AStringList.Count do
  begin
    Lin := AStringList[i];
    if copy(Lin,1,1) <> '#' then
    begin
      Colunas := Split('|', Lin);
      if Length(Colunas) > 3 then
      begin
        ARede := TACBrTEFPayGoRede.Create;
        try
          ARede.Codigo := StrToInt(Colunas[0]);
          ARede.NomeTrad := Trim(Colunas[1]);
          ARede.NomePGWeb := Trim(Colunas[2]);
          ARede.CodSATCFe := StrToIntDef(Colunas[3], 999);  // 999 = Outros
          ARede.CNPJ := Colunas[4];
          Add(ARede);
        except
          ARede.Free;
        end;
      end;
    end;

    Inc(i);
  end;
end;

function TACBrTEFPayGoTabelaRedes.LoadFromFile: Boolean;
var
  SL: TStringList;
begin
  Result := False;
  if (FFileName <> '') and FileExists(FFileName) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FFileName);
      LoadFromStringList(SL);
      Result := True;
    finally
      SL.Free;
    end;
  end;
end;

function TACBrTEFPayGoTabelaRedes.LoadFromResource: Boolean;
var
  RS: TResourceStream;
  SL: TStringList;
begin
  {$IfDef FPC}Result := False;{$EndIf}

  RS := TResourceStream.Create(HInstance, FResourceName, RT_RCDATA);
  SL := TStringList.Create;
  try
    // Leitura do Resource pode falhar
    RS.Position := 0;
    SL.LoadFromStream(RS);
    LoadFromStringList(SL);
    Result := True;
  finally
    RS.Free;
    SL.Free;
  end;
end;

procedure TACBrTEFPayGoTabelaRedes.SetFileName(AValue: String);
begin
  if (FFileName = AValue) then
    Exit;

  if (AValue <> '') and (not FileExists(AValue)) then
    raise Exception.CreateFmt(ACBrStr(cACBrArquivoNaoEncontrado), [AValue]);

  FFileName := AValue;
  FLoaded := False;
end;

function TACBrTEFPayGoTabelaRedes.Find(ACodigo: Integer): TACBrTEFPayGoRede;
var
  i: Integer;
begin
  if not FLoaded then
    Load;

  Result := Nil;
  for i := 0 to Count-1 do
  begin
    if (Objects[i].Codigo = ACodigo) then
    begin
      Result := Objects[i];
      Break;
    end;
  end;
end;

function TACBrTEFPayGoTabelaRedes.FindPGWeb(ANomePGWeb: String
  ): TACBrTEFPayGoRede;
var
  i: Integer;
  uNomePGWeb: String;
begin
  if not FLoaded then
    Load;

  uNomePGWeb :=  UpperCase(Trim(ANomePGWeb));
  Result := Nil;
  for i := 0 to Count-1 do
  begin
    if (UpperCase(Objects[i].NomePGWeb) = uNomePGWeb) then
    begin
      Result := Objects[i];
      Break;
    end;
  end;
end;

procedure TACBrTEFPayGoTabelaRedes.SetObject(Index: Integer; Item: TACBrTEFPayGoRede);
begin
  inherited Items[Index] := Item;
end;

function TACBrTEFPayGoTabelaRedes.GetObject(Index: Integer): TACBrTEFPayGoRede;
begin
  Result := TACBrTEFPayGoRede(inherited Items[Index]);
end;

procedure TACBrTEFPayGoTabelaRedes.Insert(Index: Integer; Obj: TACBrTEFPayGoRede);
begin
  inherited Insert(Index, Obj);
end;

function TACBrTEFPayGoTabelaRedes.Add(Obj: TACBrTEFPayGoRede): Integer;
begin
  Result := inherited Add(Obj) ;
end;

initialization
  TabelaRedes := TACBrTEFPayGoTabelaRedes.Create();

finalization
  if Assigned(TabelaRedes) then
    FreeAndNil(TabelaRedes);

end.

