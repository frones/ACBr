{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Elias Cesar Vieira                             }
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
unit ACBrNCMs;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrSocket;

const
  CNCM_ARQUIVO_CACHE = 'ACBrNCM.json';
  CNCM_URL = 'https://portalunico.siscomex.gov.br/classif/api/publico/nomenclatura/download/json';

type
  EACBrNcmException = class(Exception);

  TACBrNCMGetJson = procedure(var aJson: String) of object;

  TACBrNCMTipoFiltro = (ntfIniciaCom, ntfContem, ntfFinalizaCom);

  { TACBrNCM }

  TACBrNCM = class
  private
    fAnoAto: Integer;
    fCodigoNcm: String;
    fDataFim: TDateTime;
    fDataInicio: TDateTime;
    fDescricaoNcm: String;
    fDescrCategoriaNcm: String;
    fCodigoCategoriaNcm: String;
    fNumeroAto: String;
    fTipoAto: String;
  public
    property CodigoNcm: String read fCodigoNcm write fCodigoNcm;
    property CodigoCategoriaNcm: String read fCodigoCategoriaNcm write fCodigoCategoriaNcm;
    property DescrCategoriaNcm: String read fDescrCategoriaNcm write fDescrCategoriaNcm;
    property DescricaoNcm: String read fDescricaoNcm write fDescricaoNcm;
    property DataInicio: TDateTime read fDataInicio write fDataInicio;
    property DataFim: TDateTime read fDataFim write fDataFim;
    property TipoAto: String read fTipoAto write fTipoAto;
    property NumeroAto: String read fNumeroAto write fNumeroAto;
    property AnoAto: Integer read fAnoAto write fAnoAto;

    procedure Assign(aNCMOrigem: TACBrNCM);
  end;

  { TACBrNCMsList }

  TACBrNCMsList = class(TACBrObjectList)
  protected
    fSortOrder: Integer;  // 0-Nenhum, 1-Codigo, 2-Descrição
    procedure SetObject(Index: integer; Item: TACBrNCM);
    function GetObject(Index: integer): TACBrNCM;
  public
    constructor Create(FreeObjects: boolean = True);
    procedure Clear; override;

    function Add(Obj: TACBrNCM): integer;
    function New: TACBrNCM;
    function Copy(Obj: TACBrNCM): Integer;
    function Find(const aCodigoNCM: String; Exact: Boolean = False): Integer;
    function FindDesc(const aDescricaoNCM: String; Exact: Boolean = False): Integer;

    procedure SortByCodigo;
    procedure SortByDescricao;

    procedure SaveToFile(const AFileName: String; aSeparator: Char = ';');
    procedure SaveToStringList(aStrings: TStrings; aSeparator: Char = ';');

    property Objects[Index: Integer]: TACBrNCM read GetObject write SetObject; default;
  end;

  { TACBrNCMs }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNCMs = class(TACBrHTTP)
  private
    fCacheArquivo: String;
    fCacheDiasValidade: Integer;
    fNCMs: TACBrNCMsList;
    fNCMsFiltrados: TACBrNCMsList;
    fOnBuscaEfetuada: TNotifyEvent;
    fOnGetJson: TACBrNCMGetJson;
    fUltimaAtualizacao: TDateTime;
    fUrlConsulta: string;

    procedure HTTPGetCompressed(const AURL: String);
    function UnZipHttpDoc: String;

    function GetCacheArquivo: String;
    function LimparDescricao(const aStr: String): String;
    function DownloadArquivo: String;
    function CarregarCache: String;
    function RenomearCacheErro: Boolean;

    procedure GravarCache(const aJsonStr: String);
    procedure CarregarJson(const aJsonStr: String);
    procedure CarregarListaNCMs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;

    procedure ListarNcms(const aCodigoCapitulo: String = '');  // Procedure mantida para retrocompatibilidade
    procedure ObterNCMs;

    function Validar(const aCodigoNcm: String): Boolean;
    function DescricaoNcm(const aCodigoNcm: String): String;

    function BuscarPorCodigo(const aCodigoNCM: String; Exato: Boolean = True): Integer;
    function BuscarPorDescricao(const aDescricao: String; aTipoFiltro: TACBrNCMTipoFiltro = ntfIniciaCom): Integer;

    property NCMs: TACBrNCMsList read fNCMs;
    property NCMsFiltrados: TACBrNCMsList read fNCMsFiltrados;
    property UltimaAtualizacao: TDateTime read fUltimaAtualizacao;

  published
    property UrlConsulta: string read fUrlConsulta write fUrlConsulta;
    property CacheArquivo: String read GetCacheArquivo write fCacheArquivo;
    property CacheDiasValidade: Integer read fCacheDiasValidade
      write fCacheDiasValidade default 0;  // 0 - Não Expira
    property OnBuscaEfetuada: TNotifyEvent read fOnBuscaEfetuada write fOnBuscaEfetuada;
    property OnGetJson: TACBrNCMGetJson read fOnGetJson write fOnGetJson;
  end;

  function CompNCMCodAsc(const pNCM1, pNCM2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
  function CompNCMDescAsc(const pNCM1, pNCM2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;

implementation

uses
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonDataObjects_ACBr,
  {$Else}
    {$IfDef FPC}fpjson{$Else}Jsons{$EndIf},
  {$EndIf}
  DateUtils,
  synautil,
  ACBrCompress,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrUtil.FilesIO, ACBrUtil.XMLHTML;

{ TACBrNCM }

procedure TACBrNCM.Assign(aNCMOrigem: TACBrNCM);
begin
  fAnoAto := aNCMOrigem.AnoAto;
  fCodigoNcm := aNCMOrigem.CodigoNcm;
  fDataFim := aNCMOrigem.DataFim;
  fDataInicio := aNCMOrigem.DataInicio;
  fDescricaoNcm := aNCMOrigem.DescricaoNcm;
  fDescrCategoriaNcm := aNCMOrigem.DescrCategoriaNcm;
  fCodigoCategoriaNcm := aNCMOrigem.CodigoCategoriaNcm;
  fNumeroAto := aNCMOrigem.NumeroAto;
  fTipoAto := aNCMOrigem.TipoAto;
end;

{ TACBrNCMsList }

constructor TACBrNCMsList.Create(FreeObjects: boolean);
begin
  inherited Create(FreeObjects);
  Clear;
end;

procedure TACBrNCMsList.Clear;
begin
  inherited Clear;
  fSortOrder := 0;
end;

procedure TACBrNCMsList.SetObject(Index: integer; Item: TACBrNCM);
begin
  inherited Items[Index] := Item;
end;

function TACBrNCMsList.GetObject(Index: integer): TACBrNCM;
begin
  Result := TACBrNCM(inherited Items[Index]);
end;

function TACBrNCMsList.Add(Obj: TACBrNCM): integer;
begin
  Result := inherited Add(Obj);
end;

function TACBrNCMsList.New: TACBrNCM;
begin
  Result := TACBrNCM.Create;
  Add(Result);
end;

function TACBrNCMsList.Copy(Obj: TACBrNCM): Integer;
var
  aNCM: TACBrNCM;
begin
  aNCM := TACBrNCM.Create;
  aNCM.Assign(Obj);
  Result := Add(aNCM);
end;

function TACBrNCMsList.Find(const aCodigoNCM: String; Exact: Boolean): Integer;
var
  I: Integer;
  wNCM: TACBrNCM;
begin
  Result := -1;
  if (Count < 1) or EstaVazio(aCodigoNCM) then
    Exit;

  SortByCodigo;

  wNCM := TACBrNCM.Create;
  try
    wNCM.CodigoNcm := aCodigoNCM;
    {$IfDef HAS_SYSTEM_GENERICS}
    I := FindObject(wNCM, TComparer<TObject>.Construct(CompNCMCodAsc), (not Exact));
    {$Else}
    I := FindObject(Pointer(wNCM), @CompNCMCodAsc, (not Exact));
    {$EndIf}

    if (I >= 0) then
      Result := I;
  finally
    wNCM.Free;
  end;
end;

function TACBrNCMsList.FindDesc(const aDescricaoNCM: String; Exact: Boolean): Integer;
var
  I: Integer;
  wNCM: TACBrNCM;
begin
  Result := -1;
  if (Count < 1) or EstaVazio(aDescricaoNCM) then
    Exit;

  SortByDescricao;

  wNCM := TACBrNCM.Create;
  try
    wNCM.DescricaoNcm := aDescricaoNCM;
    {$IfDef HAS_SYSTEM_GENERICS}
    I := FindObject(wNCM, TComparer<TObject>.Construct(CompNCMDescAsc), (not Exact));
    {$Else}
    I := FindObject(Pointer(wNCM), @CompNCMDescAsc, (not Exact));
    {$EndIf}

    if (I >= 0) then
      Result := I;
  finally
    wNCM.Free;
  end;
end;

procedure TACBrNCMsList.SortByCodigo;
begin
  if (fSortOrder = 1) then
    Exit;

  {$IfDef HAS_SYSTEM_GENERICS}
  Self.Sort(TComparer<TObject>.Construct(CompNCMCodAsc));
  {$Else}
  Self.Sort(@CompNCMCodAsc);
  {$EndIf}

  fSortOrder := 1;
end;

procedure TACBrNCMsList.SortByDescricao;
begin
  if (fSortOrder = 2) then
    Exit;

  {$IfDef HAS_SYSTEM_GENERICS}
  Self.Sort(TComparer<TObject>.Construct(CompNCMDescAsc));
  {$Else}
  Self.Sort(@CompNCMDescAsc);
  {$EndIf}

  fSortOrder := 2;
end;

procedure TACBrNCMsList.SaveToStringList(aStrings: TStrings; aSeparator: Char);
var
  I: Integer;
begin
  if (not Assigned(aStrings)) then
    Exit;

  aStrings.Clear;
  for I := 0 to Count - 1 do
    aStrings.Add(
      Objects[I].CodigoNcm + aSeparator +
      Objects[I].DescricaoNcm + aSeparator +
      FormatDateBr(Objects[I].DataInicio) + aSeparator +
      FormatDateBr(Objects[I].DataFim) + aSeparator +
      Objects[I].TipoAto + aSeparator +
      Objects[I].NumeroAto + aSeparator +
      IntToStr(Objects[I].AnoAto));
end;

procedure TACBrNCMsList.SaveToFile(const AFileName: String; aSeparator: Char);
var
  SL: TStringList;
begin
  if EstaVazio(AFileName) then
    raise EACBrNcmException.Create(ACBrStr('Nome do arquivo nao informado'));

  SL := TStringList.Create;
  try
    SaveToStringList(SL);
    SL.SaveToFile(AFileName);
  finally
    SL.Free;
  end;
end;

{ TACBrNCMs }

constructor TACBrNCMs.Create(AOwner: TComponent);
begin
  inherited;

  fNCMs := TACBrNCMsList.Create;
  fNCMsFiltrados := TACBrNCMsList.Create;

  fCacheArquivo := CNCM_ARQUIVO_CACHE;
  fCacheDiasValidade := 0;
  fUrlConsulta := CNCM_URL;

  fOnBuscaEfetuada := Nil;
  fOnGetJson := Nil;
  Clear;
end;

destructor TACBrNCMs.Destroy;
begin
  fNCMs.Free;
  fNCMsFiltrados.Free;
  inherited Destroy;
end;

procedure TACBrNCMs.Clear;
begin
  fNCMs.Clear;
  fNCMsFiltrados.Clear;
  fUltimaAtualizacao := 0;
end;

function TACBrNCMs.GetCacheArquivo: String;
var
  aPath: String;
begin
  if (fCacheArquivo <> '') and (not (csDesigning in Self.ComponentState)) and
     (Pos(PathDelim, fCacheArquivo) = 0) then
  begin
    aPath := ExtractFilePath(fCacheArquivo);
    if (aPath = '') then
      fCacheArquivo := ApplicationPath + fCacheArquivo;
  end;

  Result := fCacheArquivo;
end;

function TACBrNCMs.LimparDescricao(const aStr: String): String;
begin
  if EstaVazio(aStr) then
    Exit;

  Result := StripHTML(aStr);
  while (Length(Result) > 0) and CharInSet(Result[1], ['-',' ','.','"']) do
    Delete(Result, 1, 1);
end;

function TACBrNCMs.DownloadArquivo: String;
begin
  Result := '';
  if (fUrlConsulta = EmptyStr) then
    Exit;

  HTTPGetCompressed(fUrlConsulta);
  Result := UnZipHttpDoc;
end;

procedure TACBrNCMs.HTTPGetCompressed(const AURL: String);
begin
  HTTPSend.Headers.Add('Accept-Encoding: deflate, gzip');
  HTTPMethod( 'GET', AURL );
end;

function TACBrNCMs.UnZipHttpDoc: String;
var
  //CT: String;
  Resp: AnsiString;
  RespIsUTF8: Boolean;
  zt: TCompressType;
begin
  zt := DetectCompressType(HTTPSend.Document);
  if zt = ctUnknown then
  begin
    HTTPSend.Document.Position := 0;
    Resp := ReadStrFromStream(HTTPSend.Document, HTTPSend.Document.Size);
  end
  else
    Resp := ACBrUtil.FilesIO.UnZip(HTTPSend.Document);

  //CT := LowerCase( GetHeaderValue('Content-Type:') );
  RespIsUTF8 := True; //(pos('utf-8', CT) > 0);
  if RespIsUTF8 then
    Result := UTF8ToNativeString(Resp)
  else
    Result := String(Resp);
end;

procedure TACBrNCMs.CarregarJson(const aJsonStr: String);
var
  I: Integer;
  wJSonArr: TJsonArray;
  wJSon, wJsonNCM: TJsonObject;

  function CriarEValidarJson: TJSONObject;
  begin
    Result := Nil;
    try
      {$IfDef USE_JSONDATAOBJECTS_UNIT}
      Result := TJsonObject.Parse(aJSonStr) as TJsonObject;;
      {$Else}
      {$IfDef FPC}
      Result := GetJSON(aJsonStr) as TJSONObject;
      {$Else}
      Result := TJsonObject.Create;
      Result.Parse(aJsonStr);
      {$EndIf}
      {$EndIf}
    except
      On E: Exception do
      begin
        if Assigned(Result) then
          Result.Free;

        if RenomearCacheErro then
          raise EACBrNcmException.Create('Cache local invalido. Renomeado para: ' +
            QuotedStr(ChangeFileExt(fCacheArquivo, '.err')))
        else
          raise EACBrNcmException.Create('Arquivo Json invalido');
      end;
    end;
  end;

begin
  if (aJsonStr = EmptyStr) then
    Exit;

  NCMs.Clear;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
  wJSon := CriarEValidarJson;
  try
    fUltimaAtualizacao := StringToDateTime(wJSon.S['Data_Ultima_Atualizacao_NCM']);
    wJSonArr := wJSon.A['Nomenclaturas'];

    for I := 0 to wJSonArr.Count - 1 do
    begin
      wJSonNCM := wJSonArr.Items[I].ObjectValue;

      with NCMs.New do
      begin
        CodigoNcm := OnlyNumber(wJsonNCM.S['Codigo']);
        DescricaoNcm := LimparDescricao(wJsonNCM.S['Descricao']);
        DataInicio := StringToDateTimeDef(wJsonNCM.S['Data_Inicio'], 0, 'dd/mm/yyyy');
        DataFim := StringToDateTimeDef(wJsonNCM.S['Data_Fim'], 0, 'dd/mm/yyyy');
        TipoAto := wJsonNCM.S['Tipo_Ato'];
        NumeroAto := wJsonNCM.S['Numero_Ato'];
        AnoAto := wJsonNCM.I['Ano_Ato'];
      end;
    end;
  finally
    wJSon.Free;
  end;
  {$Else}
  {$IfDef FPC}
  wJSon := CriarEValidarJson;
  try
    fUltimaAtualizacao := StringToDateTimeDef(wJson.Strings['Data_Ultima_Atualizacao_NCM'], 0, 'dd/mm/yyyy');
    wJSonArr := wJSon.Arrays['Nomenclaturas'];

    for I := 0 to wJSonArr.Count - 1 do
    begin
      wJsonNCM := wJSonArr.Objects[I];
      with NCMs.New do
      begin
        CodigoNcm := OnlyNumber(wJsonNCM.Strings['Codigo']);
        DescricaoNcm := LimparDescricao(wJsonNCM.Strings['Descricao']);
        DataInicio := StringToDateTimeDef(wJsonNCM.Strings['Data_Inicio'], 0, 'dd/mm/yyyy');
        DataFim := StringToDateTimeDef(wJsonNCM.Strings['Data_Fim'], 0, 'dd/mm/yyyy');
        TipoAto := wJsonNCM.Strings['Tipo_Ato'];
        NumeroAto := wJsonNCM.Strings['Numero_Ato'];
        AnoAto := wJsonNCM.Integers['Ano_Ato'];
      end;
    end;
  finally
    wJSon.Free;
  end;
  {$Else}
  wJSon := CriarEValidarJson;
  try
    fUltimaAtualizacao := StringToDateTimeDef(wJson.Values['Data_Ultima_Atualizacao_NCM'].AsString, 0, 'dd/mm/yyyy');
    wJSonArr := wJSon.Values['Nomenclaturas'].AsArray;

    for I := 0 to wJSonArr.Count - 1 do
    begin
      wJsonNCM := wJSonArr.Items[I].AsObject;
      with NCMs.New do
      begin
        CodigoNcm := OnlyNumber(wJsonNCM.Values['Codigo'].AsString);
        DescricaoNcm := LimparDescricao(wJsonNCM.Values['Descricao'].AsString);
        DataInicio := StringToDateTimeDef(wJsonNCM.Values['Data_Inicio'].AsString, 0, 'dd/mm/yyyy');
        DataFim := StringToDateTimeDef(wJsonNCM.Values['Data_Fim'].AsString, 0, 'dd/mm/yyyy');
        TipoAto := wJsonNCM.Values['Tipo_Ato'].AsString;
        NumeroAto := wJsonNCM.Values['Numero_Ato'].AsString;
        AnoAto := wJsonNCM.Values['Ano_Ato'].AsInteger;
      end;
    end;
  finally
    wJSon.Free
  end;
  {$EndIf}
  {$EndIf}
end;

procedure TACBrNCMs.CarregarListaNCMs;
var
  aJsonStr: String;
begin
  aJsonStr := EmptyStr;

  if (NCMs.Count > 0) then  // Já obteve os NCMs ?  ...Sai
    Exit;

  if Assigned(OnGetJson) then
    OnGetJson(aJsonStr);

  if EstaVazio(aJsonStr) then  // Carregou no Evento ?
    aJsonStr := CarregarCache;

  if EstaVazio(aJsonStr) then  // Carregou do Cache ?
  begin
    aJsonStr := DownloadArquivo;
    GravarCache(aJsonStr);
  end;

  CarregarJson(aJsonStr);
end;

procedure TACBrNCMs.ListarNcms(const aCodigoCapitulo: String);
begin
  if EstaVazio(aCodigoCapitulo) then
    ObterNCMs
  else
    BuscarPorCodigo(aCodigoCapitulo);
end;

function TACBrNCMs.CarregarCache: String;
var
  wArq: String;
  wSL: TStringList;
  wDataCache: TDateTime;
begin
  Clear;
  wDataCache := 0;
  Result := EmptyStr;
  wArq := CacheArquivo;

  if (wArq = EmptyStr) or (not FileExists(wArq)) then
    Exit;

  if (CacheDiasValidade > 0) then
  begin
    {$IFDEF FPC}
    FileAge(wArq, wDataCache);
    {$ELSE}
      {$IFDEF DELPHI2007_UP}
      FileAge(wArq, wDataCache);
      {$ELSE}
      wDataCache := FileDateToDateTime(FileAge(wArq));
      {$ENDIF}
    {$ENDIF}

    if (DaysBetween(Now, wDataCache) > CacheDiasValidade) then
    begin
      DeleteFile(wArq);
      Exit;
    end;
  end;

  wSL := TStringList.Create;
  try
    wSL.LoadFromFile(wArq);
    Result := wSL.Text;
  finally
    wSL.Free;
  end;
end;

function TACBrNCMs.RenomearCacheErro: Boolean;
var
  wArq: String;
begin
  wArq := CacheArquivo;
  Result := FileExists(wArq);

  if Result then
    RenameFile(wArq, ChangeFileExt(wArq, '.err'));
end;

procedure TACBrNCMs.GravarCache(const aJsonStr: String);
var
  wArq: String;
begin
  wArq := CacheArquivo;
  if (Length(wArq) = 0) or (Length(aJsonStr) = 0) then
    Exit;

  WriteToFile(wArq, aJsonStr, True);
end;

procedure TACBrNCMs.ObterNCMs;
begin
  Clear;
  CarregarListaNCMs;
end;

function TACBrNCMs.Validar(const aCodigoNcm: String): Boolean;
var
  wIndex: Integer;
begin
  Result := False;

  if EstaVazio(aCodigoNCM) then
    raise EACBrNcmException.Create(ACBrStr('Codigo do NCM deve ser informado'));

  CarregarListaNCMs;
  wIndex := NCMs.Find(aCodigoNCM, True);

  if (wIndex > 0) then
    Result := (aCodigoNcm = NCMs[wIndex].CodigoNcm);
end;

function TACBrNCMs.DescricaoNcm(const aCodigoNcm: String): String;
var
  wIndex: Integer;
begin
  Result := EmptyStr;

  if EstaVazio(aCodigoNCM) then
    raise EACBrNcmException.Create(ACBrStr('Codigo do NCM deve ser informado'));

  CarregarListaNCMs;
  wIndex := NCMs.Find(aCodigoNCM, True);

  if (wIndex < 0) then
    raise EACBrNcmException.Create(ACBrStr('Codigo NCM nao encontrado'));

  if (aCodigoNcm = NCMs[wIndex].CodigoNcm) then
    Result := NCMs[wIndex].DescricaoNcm;
end;

function TACBrNCMs.BuscarPorCodigo(const aCodigoNCM: String; Exato: Boolean): Integer;
var
  I, wTam: Integer;
begin
  NCMsFiltrados.Clear;
  if EstaVazio(aCodigoNCM) then
    raise EACBrNcmException.Create(ACBrStr('Codigo do NCM deve ser informado'));

  CarregarListaNCMs;
  I := NCMs.Find(aCodigoNCM, Exato);
  wTam := Length(aCodigoNCM);
  if (I >= 0) then
  begin
    while (I < NCMs.Count) do
    begin
      if Exato and ((aCodigoNCM = NCMs[I].CodigoNcm) or
         (OnlyNumber(aCodigoNCM) = OnlyNumber(NCMs[I].CodigoNcm))) then
        NCMsFiltrados.Copy(NCMs[I])
      else if (aCodigoNCM = Copy(NCMs[I].CodigoNcm, 1, wTam)) then
        NCMsFiltrados.Copy(NCMs[I])
      else
        Break;

      Inc(I);
    end;
  end;

  Result := NCMsFiltrados.Count;

  if Assigned(fOnBuscaEfetuada) then
    OnBuscaEfetuada(Self);
end;

function TACBrNCMs.BuscarPorDescricao(const aDescricao: String;
  aTipoFiltro: TACBrNCMTipoFiltro): Integer;

  procedure FiltrarIniciaCom;
  var
    I, wTam: Integer;
  begin
    I := NCMs.FindDesc(aDescricao, False);
    wTam := Length(aDescricao);
    if (I >= 0) then
    begin
      while (I < NCMs.Count) do
      begin
        if (UpperCase(aDescricao) = UpperCase(LeftStrNativeString(NCMs[I].DescricaoNcm, wTam))) then
          NCMsFiltrados.Copy(NCMs[I])
        else
          Break;
           
        Inc(I);
      end;
    end;
  end;

  procedure FiltrarContem;
  var
    I: Integer;
  begin
    for I := 0 to Pred(NCMs.Count) do
      if (Pos(UpperCase(aDescricao), UpperCase(NCMs[I].DescricaoNcm)) > 0) then
        NCMsFiltrados.Copy(NCMs[I]);
  end;

  procedure FiltrarFinalizaCom;
  var
    I: Integer;
    wTam: Integer;
  begin
    wTam := Length(aDescricao);
    for I := 0 to Pred(NCMs.Count) do
      if (UpperCase(aDescricao) = UpperCase(RightStrNativeString(NCMs[I].DescricaoNcm, wTam))) then
        NCMsFiltrados.Copy(NCMs[I]);
  end;

begin
  NCMsFiltrados.Clear;
  if EstaVazio(aDescricao) then
    raise EACBrNcmException.Create(ACBrStr('Descriçao do NCM deve ser informada'));

  CarregarListaNCMs;
  if (aTipoFiltro = ntfIniciaCom) then
    FiltrarIniciaCom
  else if (aTipoFiltro = ntfFinalizaCom) then
    FiltrarFinalizaCom
  else
    FiltrarContem;

  Result := NCMsFiltrados.Count;

  if Assigned(OnBuscaEfetuada) then
     OnBuscaEfetuada(Self);
end;

function CompNCMCodAsc(const pNCM1, pNCM2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
var
  aNCM1, aNCM2: TACBrNCM;
begin
  aNCM1 := TACBrNCM(pNCM1);
  aNCM2 := TACBrNCM(pNCM2);

  if aNCM1.CodigoNcm > aNCM2.CodigoNcm then
    Result := 1
  else if aNCM1.CodigoNcm < aNCM2.CodigoNcm then
    Result := -1
  else
    Result := 0;
end;

function CompNCMDescAsc(const pNCM1, pNCM2: {$IfDef HAS_SYSTEM_GENERICS}TObject{$Else}Pointer{$EndIf}): Integer;
var
  aNCM1, aNCM2: TACBrNCM;
begin
  aNCM1 := TACBrNCM(pNCM1);
  aNCM2 := TACBrNCM(pNCM2);

  if aNCM1.DescricaoNcm > aNCM2.DescricaoNcm then
    Result := 1
  else if aNCM1.DescricaoNcm < aNCM2.DescricaoNcm then
    Result := -1
  else
    Result := 0;
end;

end.
