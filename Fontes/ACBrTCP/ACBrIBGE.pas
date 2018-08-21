{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
|* 12/08/2010: Primeira Versao
|*    Daniel Simoes de Almeida e André Moraes
******************************************************************************}

unit ACBrIBGE ;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, contnrs,
  ACBrSocket, ACBrBase;

const
  CIBGE_UF_COUNT    = 27;
  CIBGE_URL_UF      = 'https://servicodados.ibge.gov.br/api/v1/localidades/estados/';
  CIBGE_URL_MUN_UF  = 'https://servicodados.ibge.gov.br/api/v1/localidades/estados/{idUF}/municipios';
  CIBGE_URL_MUN     = 'https://servicodados.ibge.gov.br/api/v1/localidades/municipios';
  CIBGE_URL_EST_UF  = 'https://servicodados.ibge.gov.br/api/v1/pesquisas/indicadores/{indicadores}/resultados/{idUF}';
  CIBGE_IND_UF_AREA      = 48980;
  CIBGE_IND_UF_POPULACAO = 48985;
  CIBGE_URL_EST_MUN = 'https://servicodados.ibge.gov.br/api/v1/pesquisas/indicadores/{indicadores}/resultados/{idMunicipio}';
  CIBGE_IND_MUN_AREA      = 29167;
  CIBGE_IND_MUN_POPULACAO = 29171;

type

  EACBrIBGEException = class ( Exception );

  TACBrIBGE = class;
  TACBrIBGEUFs = class;
  TACBrIBGECidades = class;

  { TACBrIBGEUF }

  TACBrIBGEUF = class
  private
    fCidadesCarregadas: Boolean;
    fArea: Double;
    fCodRegiao: String;
    fCodUF: Integer;
    fNome: String;
    fPopulacao: Integer;
    fRegiao: String;
    fUF: String;
    function GetAsString: String;
    procedure SetAsString(AValue: String);
  public
    constructor Create;
    procedure Clear;

    property CodUF      : Integer read fCodUF       write fCodUF ;
    property UF         : String  read fUF          write fUF ;
    property Nome       : String  read fNome        write fNome ;
    property CodRegiao  : String  read fCodRegiao   write fCodRegiao ;
    property Regiao     : String  read fRegiao      write fRegiao ;
    property Area       : Double  read fArea        write fArea ;
    property Populacao  : Integer read fPopulacao   write fPopulacao ;

    property CidadesCarregadas: Boolean read fCidadesCarregadas write fCidadesCarregadas;

    property AsString: String read GetAsString write SetAsString;
  end ;

  { TACBrIBGEUFs }

  TACBrIBGEUFs = class(TACBrObjectList)
  protected
    FIBGE: TACBrIBGE;
    FSortOrder: Integer;  // 0-Nenhum, 1-CodUF, 2-Nome
    procedure SetObject (Index: Integer; Item: TACBrIBGEUF);
    function GetObject (Index: Integer): TACBrIBGEUF;
    procedure Insert (Index: Integer; Obj: TACBrIBGEUF);
  public
    constructor Create(AOwner: TACBrIBGE); reintroduce;
    procedure Clear; override;

    property IBGE: TACBrIBGE read FIBGE;

    function Add (Obj: TACBrIBGEUF): Integer;
    property Objects [Index: Integer]: TACBrIBGEUF
      read GetObject write SetObject; default;

    function Find(ACodUF: Integer): Integer; overload;
    function Find(AUF: String): Integer; overload;
    function UFToCodUF(AUF: String): Integer;

    procedure SortCodUF;
    procedure SortNome;

    procedure AddFromJSonStr(AJSonStr: String);
    procedure ParseJSonStat(AJSonStr: String);
  end;

  { TACBrIBGECidade }

  TACBrIBGECidade = class
  private
    fCodMunicipio : Integer ;
    fMunicipio  : String ;
    fCodUF      : Integer ;
    fMunicipioIdx: String;
    fUF         : String;
    fArea       : Double ;
    fPopulacao: Integer;
    function GetAsString: String;
    procedure SetAsString(AValue: String);

  public
    constructor Create;
    procedure Clear;
    procedure Assign(CidadeOrigem: TACBrIBGECidade);

    property CodMunicipio : Integer read fCodMunicipio  write fCodMunicipio;
    property Municipio    : String  read fMunicipio     write fMunicipio;
    property MunicipioIdx : String  read fMunicipioIdx  write fMunicipioIdx;
    property CodUF        : Integer read fCodUF         write fCodUF;
    property UF           : String  read fUF            write fUF;
    property Area         : Double  read fArea          write fArea;
    property Populacao    : Integer read fPopulacao     write fPopulacao;

    property AsString: String read GetAsString write SetAsString;
  end;

  { TACBrIBGECidades }

  TACBrIBGECidades = class(TACBrObjectList)
  protected
    FIBGE: TACBrIBGE;
    FSortOrder: Integer;  // 0-Nenhum, 1-CodMunicipio, 2-CodUF+Municipio
    procedure SetObject (Index: Integer; Item: TACBrIBGECidade);
    function GetObject (Index: Integer): TACBrIBGECidade;
    procedure Insert (Index: Integer; Obj: TACBrIBGECidade);
  public
    constructor Create(AOwner: TACBrIBGE); reintroduce;
    procedure Clear; override;

    property IBGE: TACBrIBGE read FIBGE;

    function Add (Obj: TACBrIBGECidade): Integer;
    property Objects [Index: Integer]: TACBrIBGECidade
      read GetObject write SetObject; default;

    function Copy(Obj: TACBrIBGECidade): Integer;

    function Find(AMunicipio: String; Exact: Boolean = False): Integer; overload;
    function Find(ACodMunicio: Integer; Exact: Boolean = True): Integer; overload;
    procedure SortByCodMunicipio;
    procedure SortByMunicipio;

    procedure AddFromJSonStr(AJSonStr: String);
    procedure ParseJSonStat(AJSonStr: String);
  end;

  TACBrIBGELerGravarCache = procedure(ConteudoCache: TStrings; var Tratado: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}		
  TACBrIBGE = class( TACBrHTTP )
  private
    fCacheArquivo: String;
    fCacheDiasValidade: Integer;
    fCacheLido: Boolean;
    fIgnorarCaixaEAcentos: Boolean;
    fListaCidades: TACBrIBGECidades ;
    fCidadesEncontradas: TACBrIBGECidades ;
    fListaUFs: TACBrIBGEUFs;
    fOnBuscaEfetuada : TNotifyEvent ;
    fOnGravarCache: TACBrIBGELerGravarCache;
    fOnLerCache: TACBrIBGELerGravarCache;
    function GetCacheArquivo: String;
    procedure SetIgnorarCaixaEAcentos(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy ; override ;

    property ListaUFs: TACBrIBGEUFs  read fListaUFs ;
    property ListaCidades: TACBrIBGECidades read fListaCidades ;

    property Cidades: TACBrIBGECidades read fCidadesEncontradas ;

    function BuscarPorCodigo( const ACodMun : Integer ) : Integer ;
    function BuscarPorNome( const ACidade : String; const AUF: String = '';
      const Exata: Boolean = False) : Integer ;

    procedure ObterUFs;
    procedure ObterEstatisticasUF;
    function UFToCodUF(AUF: String): Integer;

    procedure ObterCidades; overload;
    procedure ObterCidades(const ACodUF: Integer); overload;
    procedure ObterCidades(const AUF: String); overload;

    procedure ObterEstatisticasCidade(const ACodMunicipio: Integer); overload;
    procedure ObterEstatisticasCidadesUF(const ACodUF: Integer); overload;
    procedure ObterEstatisticasCidadesUF(const AUF: String); overload;
    procedure ObterEstatisticasCidades(const ListaMunicipios: String); overload;

    procedure CarregarCache;
    procedure SalvarCache;
    procedure SalvarCidades(AStream: TStream); overload;
    procedure SalvarCidades(AStringList: TStrings); overload;
    procedure SalvarCidades(AFile: String; Overwrite: Boolean = False); overload;
  published
    property CacheArquivo: String read GetCacheArquivo write fCacheArquivo;
    property CacheDiasValidade: Integer read fCacheDiasValidade
      write fCacheDiasValidade default 0;  // 0-não expira

    property IgnorarCaixaEAcentos: Boolean read fIgnorarCaixaEAcentos
      write SetIgnorarCaixaEAcentos default False;

    property OnBuscaEfetuada : TNotifyEvent read fOnBuscaEfetuada
       write fOnBuscaEfetuada ;
    property OnGravarCache : TACBrIBGELerGravarCache read fOnGravarCache
      write fOnGravarCache;
    property OnLerCache : TACBrIBGELerGravarCache read fOnLerCache
      write fOnLerCache;
  end ;

  function CompCidadeCodMunicipioAsc(pCidade1,pCidade2: Pointer): Integer;
  function CompCidadeMunicipioAsc(pCidade1,pCidade2: Pointer): Integer;

  function CompUFCodUFAsc(pUF1,pUF2: Pointer): Integer;
  function CompUFNomeAsc(pUF1,pUF2: Pointer): Integer;

implementation

uses
  strutils, dateutils,
  blcksock, Jsons, ACBrUtil;

{ TACBrIBGEUF }

function TACBrIBGEUF.GetAsString: String;
begin
  Result := IntToStr(fCodUF)    +'|'+
            fUF                 +'|'+
            fNome               +'|'+
            fCodRegiao          +'|'+
            fRegiao             +'|'+
            FloatToString(fArea)+'|'+
            IntToStr(fPopulacao);
end;

procedure TACBrIBGEUF.SetAsString(AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if (SL.Count <> 7) then
      raise EACBrIBGEException.CreateFmt( ACBrStr('Linha de UF inválida: %s'), [AValue] );

    CodUF     := StrToInt( SL[0] );
    UF        := SL[1];
    Nome      := SL[2];
    CodRegiao := SL[3];
    Regiao    := SL[4];
    Area      := StringToFloat( SL[5] );
    Populacao := StrToInt( SL[6] );
  finally
    SL.Free;
  end;
end;

constructor TACBrIBGEUF.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrIBGEUF.Clear;
begin
  fArea := 0;
  fCodRegiao := '';
  fCodUF := 0;
  fNome := '';
  fPopulacao := 0;
  fRegiao := '';
  fUF := '';
  fCidadesCarregadas := False;
end;

{ TACBrIBGEUFs }

constructor TACBrIBGEUFs.Create(AOwner: TACBrIBGE);
begin
  inherited Create(True);
  FIBGE := AOwner;
  FSortOrder := 0;
end;

procedure TACBrIBGEUFs.Clear;
begin
  inherited Clear;
  FSortOrder := 0;
end;

procedure TACBrIBGEUFs.SetObject(Index: Integer; Item: TACBrIBGEUF);
begin
  inherited SetItem (Index, Item) ;
end;

function TACBrIBGEUFs.GetObject(Index: Integer): TACBrIBGEUF;
begin
  Result := inherited GetItem(Index) as TACBrIBGEUF ;
end;

procedure TACBrIBGEUFs.Insert(Index: Integer; Obj: TACBrIBGEUF);
begin
  FSortOrder := 0;
  inherited Insert(Index, Obj);
end;

function TACBrIBGEUFs.Add(Obj: TACBrIBGEUF): Integer;
begin
  FSortOrder := 0;
  Result := inherited Add(Obj) ;
end;

function TACBrIBGEUFs.Find(ACodUF: Integer): Integer;
var
  oUF: TACBrIBGEUF;
begin
{$IFNDEF COMPILER23_UP}
  Result := -1;
{$ENDIF}
  SortCodUF;

  oUF := TACBrIBGEUF.Create;
  try
    oUF.CodUF := ACodUF;
    Result := FindObject(Pointer(oUF), CompUFCodUFAsc);
  finally
    oUF.Free;
  end;
end;

function TACBrIBGEUFs.Find(AUF: String): Integer;
var
  I: Integer;
begin
  Result := -1;

  AUF := UpperCase(AUF);
  I := 0;
  while (I < Count) and (Result < 0) do
  begin
    if (Objects[I].UF = AUF) then
      Result := I
    else
      Inc(I);
  end;
end;

function TACBrIBGEUFs.UFToCodUF(AUF: String): Integer;
var
  I: Integer;
begin
  I := Find(AUF);
  if (I >= 0) then
    Result := Objects[I].CodUF
  else
    Result := -1;
end;

procedure TACBrIBGEUFs.SortCodUF;
begin
  if FSortOrder = 1 then
    Exit;

  Sort(CompUFCodUFAsc);
  FSortOrder := 1;
end;

procedure TACBrIBGEUFs.SortNome;
begin
  if FSortOrder = 2 then
    Exit;

  Sort(CompUFNomeAsc);
  FSortOrder := 2;
end;

procedure TACBrIBGEUFs.AddFromJSonStr(AJSonStr: String);
var
  AJSon: TJson;
  I: Integer;
  AUF: TACBrIBGEUF;
  JSonUF: TJsonObject;
begin
  Clear;
  AJSon := TJson.Create;
  try
    AJSon.Parse(AJSonStr);

    For I := 0 to AJSon.Count-1 do
    begin
      JSonUF := AJSon.Get(I).AsObject;

      AUF := TACBrIBGEUF.Create;
      AUF.CodUF     := JSonUF.Values['id'].AsInteger;
      AUF.UF        := JSonUF.Values['sigla'].AsString;
      AUF.Nome      := JSonUF.Values['nome'].AsString;
      AUF.CodRegiao := JSonUF.Values['regiao'].AsObject.Values['sigla'].AsString;
      AUF.Regiao    := JSonUF.Values['regiao'].AsObject.Values['nome'].AsString;

      Add(AUF);
    end;
  finally
    AJSon.Free;
  end;
end;

procedure TACBrIBGEUFs.ParseJSonStat(AJSonStr: String);
var
  AJSon: TJson;
  I, J, idPesq, CodUF, iUF: Integer;
  JSonPesq, JSonRespUF, JSonRespUFAnos: TJsonObject;
  JSonResp: TJsonArray;
  Valor: Extended;
  oUF: TACBrIBGEUF;
begin
  AJSon := TJson.Create;
  try
    AJSon.Parse(AJSonStr);

    For I := 0 to AJSon.Count-1 do
    begin
      JSonPesq := AJSon.Get(I).AsObject;
      idPesq := JSonPesq.Values['id'].AsInteger;
      JSonResp := JSonPesq.Values['res'].AsArray;

      For J := 0 to JSonResp.Count-1 do
      begin
        JSonRespUF := JSonResp[J].AsObject;
        CodUF := JSonRespUF.Values['localidade'].AsInteger;
        iUF := Find(CodUF);
        if (iUF >= 0) then
        begin
          oUF := Objects[iUF];
          JSonRespUFAnos := JSonRespUF.Values['res'].AsObject;
          if JSonRespUFAnos.Count > 0 then
            Valor := JSonRespUFAnos.Items[JSonRespUFAnos.Count-1].Value.AsNumber
          else
            Valor := 0;

          case idPesq of
            CIBGE_IND_UF_AREA: oUF.Area := Valor;
            CIBGE_IND_UF_POPULACAO: oUF.Populacao := trunc( Valor );
          end;
        end;
      end;
    end;
  finally
    AJSon.Free;
  end;
end;

function CompUFCodUFAsc(pUF1, pUF2: Pointer): Integer;
var
  UF1, UF2: TACBrIBGEUF;
begin
  UF1 := TACBrIBGEUF(pUF1);
  UF2 := TACBrIBGEUF(pUF2);

  if UF1.CodUF > UF2.CodUF then
    Result := 1
  else if UF1.CodUF < UF2.CodUF then
    Result := -1
  else
    Result := 0;
end;

function CompUFNomeAsc(pUF1, pUF2: Pointer): Integer;
var
  UF1, UF2: TACBrIBGEUF;
begin
  UF1 := TACBrIBGEUF(pUF1);
  UF2 := TACBrIBGEUF(pUF2);

  if UF1.Nome > UF2.Nome then
    Result := 1
  else if UF1.Nome < UF2.Nome then
    Result := -1
  else
    Result := 0;
end;

{ TACBrIBGECidade }

constructor TACBrIBGECidade.Create;
begin
  inherited Create;
  Clear;
end ;

procedure TACBrIBGECidade.Clear;
begin
  fArea := 0 ;
  fMunicipio := '';
  fCodMunicipio := 0 ;
  fCodUF := 0 ;
  fUF := '';
end;

function TACBrIBGECidade.GetAsString: String;
begin
  Result := IntToStr(fCodMunicipio) +'|'+
            fMunicipio              +'|'+
            IntToStr(fCodUF)        +'|'+
            fUF                     +'|'+
            FloatToString(fArea)    +'|'+
            IntToStr(fPopulacao);
end;

procedure TACBrIBGECidade.SetAsString(AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if (SL.Count <> 6) then
      raise EACBrIBGEException.CreateFmt( ACBrStr('Linha de Cidade inválida: %s'), [AValue] );

    CodMunicipio := StrToInt( SL[0] );
    Municipio    := SL[1];
    CodUF        := StrToInt( SL[2] );
    UF           := SL[3];
    Area         := StringToFloat( SL[4] );
    Populacao    := StrToInt( SL[5] );
  finally
    SL.Free;
  end;
end;

procedure TACBrIBGECidade.Assign(CidadeOrigem: TACBrIBGECidade);
begin
  fCodMunicipio := CidadeOrigem.CodMunicipio;
  fMunicipio := CidadeOrigem.Municipio;
  fCodUF := CidadeOrigem.CodUF;
  fUF := CidadeOrigem.UF;
  fArea := CidadeOrigem.Area;
  fPopulacao := CidadeOrigem.Populacao;
end;

{ TACBrIBGECidades }

constructor TACBrIBGECidades.Create(AOwner: TACBrIBGE);
begin
  inherited Create(True);
  FIBGE := AOwner;
  FSortOrder := 0;
end;

procedure TACBrIBGECidades.Clear;
begin
  inherited Clear;
  FSortOrder := 0;
end;

procedure TACBrIBGECidades.SetObject(Index : Integer ; Item : TACBrIBGECidade) ;
begin
  FSortOrder := 0;
  inherited SetItem (Index, Item) ;
end ;

function TACBrIBGECidades.GetObject(Index : Integer) : TACBrIBGECidade ;
begin
  Result := inherited GetItem(Index) as TACBrIBGECidade ;
end ;

procedure TACBrIBGECidades.Insert(Index : Integer ; Obj : TACBrIBGECidade) ;
begin
  FSortOrder := 0;
  inherited Insert(Index, Obj);
end ;

function TACBrIBGECidades.Add(Obj : TACBrIBGECidade) : Integer ;
begin
  FSortOrder := 0;
  Result := inherited Add(Obj) ;
end ;

function TACBrIBGECidades.Copy(Obj: TACBrIBGECidade): Integer;
var
  oCidade: TACBrIBGECidade;
begin
  oCidade := TACBrIBGECidade.Create;
  oCidade.Assign(Obj);
  Result := Add(oCidade);
end;

function TACBrIBGECidades.Find(AMunicipio: String; Exact: Boolean): Integer;
var
  I: Integer;
  oCidadeFind: TACBrIBGECidade;
begin
  Result := -1;
  if (Count < 1) or (AMunicipio = '') then
    Exit;

  SortByMunicipio;

  oCidadeFind := TACBrIBGECidade.Create;
  try
    oCidadeFind.MunicipioIdx := AMunicipio;
    I := FindObject(Pointer(oCidadeFind), CompCidadeMunicipioAsc, (not Exact));
    if I >= 0 then
      Result := I;
  finally
    oCidadeFind.Free;
  end;
end;

function TACBrIBGECidades.Find(ACodMunicio: Integer; Exact: Boolean): Integer;
var
  oCidade: TACBrIBGECidade;
begin
  Result := -1;
  if (Count < 1) then
    Exit;

  SortByCodMunicipio;

  oCidade := TACBrIBGECidade.Create;
  try
    oCidade.CodMunicipio := ACodMunicio;
    Result := FindObject(Pointer(oCidade), CompCidadeCodMunicipioAsc, (not Exact));
  finally
    oCidade.Free;
  end;
end;

procedure TACBrIBGECidades.SortByCodMunicipio;
begin
  if FSortOrder = 1 then
    Exit;

  Sort(CompCidadeCodMunicipioAsc);
  FSortOrder := 1;
end;

procedure TACBrIBGECidades.SortByMunicipio;
begin
  if FSortOrder = 2 then
    Exit;

  Sort(CompCidadeMunicipioAsc);
  FSortOrder := 2;
end;

procedure TACBrIBGECidades.AddFromJSonStr(AJSonStr: String);
var
  AJSon: TJson;
  I, CodMunicipio: Integer;
  oCidade: TACBrIBGECidade;
  JSonCidade: TJsonObject;
begin
  AJSon := TJson.Create;
  try
    AJSon.Parse(AJSonStr);

    For I := 0 to AJSon.Count-1 do
    begin
      JSonCidade := AJSon.Get(I).AsObject;

      CodMunicipio := JSonCidade.Values['id'].AsInteger;
      oCidade := TACBrIBGECidade.Create;
      oCidade.CodMunicipio := CodMunicipio;
      oCidade.Municipio := JSonCidade.Values['nome'].AsString;
      if FIBGE.IgnorarCaixaEAcentos then
        oCidade.MunicipioIdx := LowerCase(TiraAcentos(oCidade.Municipio))
      else
        oCidade.MunicipioIdx := oCidade.Municipio;

      with JSonCidade.Values['microrregiao'].AsObject.Values['mesorregiao'].AsObject.Values['UF'].AsObject do
      begin
        oCidade.CodUF := Values['id'].AsInteger;
        oCidade.UF    := Values['sigla'].AsString;
      end;

      Add(oCidade);
    end;
  finally
    AJSon.Free;
  end;
end;

procedure TACBrIBGECidades.ParseJSonStat(AJSonStr: String);
var
  AJSon: TJson;
  I, J, idPesq, CodMun, iCidade: Integer;
  JSonPesq, JSonRespMun, JSonRespMunAnos: TJsonObject;
  JSonResp: TJsonArray;
  Valor: Extended;
  oCidade: TACBrIBGECidade;
begin
  AJSon := TJson.Create;
  try
    AJSon.Parse(AJSonStr);

    For I := 0 to AJSon.Count-1 do
    begin
      JSonPesq := AJSon.Get(I).AsObject;
      idPesq := JSonPesq.Values['id'].AsInteger;
      JSonResp := JSonPesq.Values['res'].AsArray;

      For J := 0 to JSonResp.Count-1 do
      begin
        JSonRespMun := JSonResp[J].AsObject;
        CodMun := JSonRespMun.Values['localidade'].AsInteger;
        iCidade := Find(CodMun * 10, False);   // Municipo vem sem o dígito verificador
        if (iCidade >= 0) then
        begin
          oCidade := Objects[iCidade];
          if trunc(oCidade.CodMunicipio/10) = CodMun then
          begin
            JSonRespMunAnos := JSonRespMun.Values['res'].AsObject;
            if JSonRespMunAnos.Count > 0 then
              Valor := JSonRespMunAnos.Items[JSonRespMunAnos.Count-1].Value.AsNumber
            else
              Valor := 0;

            case idPesq of
              CIBGE_IND_MUN_AREA: oCidade.Area := Valor;
              CIBGE_IND_MUN_POPULACAO: oCidade.Populacao := trunc( Valor );
            end;
          end;
        end;
      end;
    end;
  finally
    AJSon.Free;
  end;
end;

function CompCidadeCodMunicipioAsc(pCidade1, pCidade2: Pointer): Integer;
var
  oCidade1, oCidade2: TACBrIBGECidade;
begin
  oCidade1 := TACBrIBGECidade(pCidade1);
  oCidade2 := TACBrIBGECidade(pCidade2);

  if oCidade1.CodMunicipio > oCidade2.CodMunicipio then
    Result := 1
  else if oCidade1.CodMunicipio < oCidade2.CodMunicipio then
    Result := -1
  else
    Result := 0;
end;

function CompCidadeMunicipioAsc(pCidade1, pCidade2: Pointer): Integer;
var
  oCidade1, oCidade2: TACBrIBGECidade;
begin
  oCidade1 := TACBrIBGECidade(pCidade1);
  oCidade2 := TACBrIBGECidade(pCidade2);

  if oCidade1.MunicipioIdx > oCidade2.MunicipioIdx then
    Result := 1
  else if oCidade1.MunicipioIdx < oCidade2.MunicipioIdx then
    Result := -1
  else
    Result := 0;
end;

{ TACBrIBGE }

function TACBrIBGE.GetCacheArquivo: String;
var
  APath: String;
begin
  if (fCacheArquivo <> '') and (not (csDesigning in Self.ComponentState)) and
     (pos(PathDelim, fCacheArquivo) = 0) then
  begin
    APath := ExtractFilePath(fCacheArquivo);
    if (APath = '') then
      fCacheArquivo := ExtractFilePath( ParamStr(0) ) + fCacheArquivo ;
  end;

  Result := fCacheArquivo;
end;

procedure TACBrIBGE.SetIgnorarCaixaEAcentos(AValue: Boolean);
begin
  if fIgnorarCaixaEAcentos = AValue then
    Exit;

  fIgnorarCaixaEAcentos := AValue;
  fCacheLido := False;  // Força recarga do Cache, no formato correto
end;

constructor TACBrIBGE.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fOnBuscaEfetuada := nil ;
  fIgnorarCaixaEAcentos := False;

  fCidadesEncontradas := TACBrIBGECidades.create(Self);
  fListaCidades := TACBrIBGECidades.create(Self);
  fListaUFs := TACBrIBGEUFs.Create(Self);

  fCacheArquivo := 'ACBrIBGE.txt';
  fCacheDiasValidade := 0;
  fCacheLido := False;
  fOnGravarCache := Nil;
  fOnLerCache := Nil;

  HTTPSend.Sock.SSL.SSLType := LT_TLSv1_2;
end ;

destructor TACBrIBGE.Destroy ;
begin
  fCidadesEncontradas.Free;
  fListaCidades.Free ;
  fListaUFs.Free;

  inherited Destroy ;
end ;

function TACBrIBGE.BuscarPorCodigo(const ACodMun : Integer) : Integer ;
var
  ACodUF, iCidade: Integer;
begin
  RespHTTP.Clear;
  fCidadesEncontradas.Clear;
  if ACodMun = 0 then
     raise EACBrIBGEException.Create( ACBrStr('Código do Município deve ser informado') );

  ACodUF := StrToInt(LeftStr(IntToStr(ACodMun), 2));
  ObterCidades(ACodUF);

  iCidade := fListaCidades.Find(ACodMun);
  if (iCidade >= 0) then
    fCidadesEncontradas.Copy(fListaCidades[iCidade]);

  if Assigned( OnBuscaEfetuada ) then
     OnBuscaEfetuada( Self );

  Result := fCidadesEncontradas.Count;
end ;

function TACBrIBGE.BuscarPorNome(const ACidade: String; const AUF: String;
  const Exata: Boolean): Integer;
var
  I , CodUF: Integer ;
  CidadeEncontrar, CidadeAtual: String;
begin
  RespHTTP.Clear;
  fCidadesEncontradas.Clear;

  if (Trim(ACidade) = '') then
    raise EACBrIBGEException.Create( ACBrStr('Nome do Município deve ser informado') );

  if (Trim(AUF) <> '') then
  begin
    CodUF := UFToCodUF(Trim(AUF));
    if (CodUF < 0) then
      raise EACBrIBGEException.CreateFmt( ACBrStr('UF %s não encontrada'), [AUF] );

    ObterCidades(CodUF);
  end
  else
  begin
    CodUF := 0;
    ObterCidades;
  end;

  if IgnorarCaixaEAcentos then
    CidadeEncontrar := LowerCase(TiraAcentos(ACidade))
  else
    CidadeEncontrar := ACidade;

  I := fListaCidades.Find(CidadeEncontrar, Exata);
  if (I >= 0) then
  begin
    repeat
      if (CodUF = 0) or (fListaCidades[I].CodUF = CodUF) then
        fCidadesEncontradas.Copy(fListaCidades[I]);

      if (Exata and (fCidadesEncontradas.Count > 0)) then
        Break;

      Inc(I);
      if (I >= fListaCidades.Count) then
        Break;

      CidadeAtual := fListaCidades[I].MunicipioIdx;
      if not Exata then
        CidadeAtual := LeftStr(CidadeAtual, Length(CidadeEncontrar));

    until (CidadeAtual > CidadeEncontrar);
  end;

  Result := fCidadesEncontradas.Count;

  if Assigned( OnBuscaEfetuada ) then
     OnBuscaEfetuada( Self );
end ;

procedure TACBrIBGE.ObterCidades;
var
  Carregar: Boolean;
  I: Integer;
begin
  ObterUFs;

  Carregar := False;
  I := 0;
  while (I < fListaUFs.Count) and (not Carregar) do
  begin
    Carregar := (not fListaUFs[I].CidadesCarregadas);
    Inc(I);
  end;

  if not Carregar then                                    // Já carregou todas ?
    Exit;

  fListaCidades.Clear;
  HTTPGet(CIBGE_URL_MUN);
  fListaCidades.AddFromJSonStr(RespHTTP.Text);

  for I := 0 to fListaUFs.Count-1 do
    fListaUFs[I].CidadesCarregadas := True;

  SalvarCache;
end;

procedure TACBrIBGE.ObterCidades(const ACodUF: Integer);
var
  AURL: String;
  oUF: TACBrIBGEUF;
  iUF: Integer;
begin
  ObterUFs;

  iUF := fListaUFs.Find(ACodUF);
  if (iUF < 0) then
    raise EACBrIBGEException.CreateFmt( ACBrStr('Código da UF: %d inválido'), [ACodUF] );

  oUF := fListaUFs[iUF];
  if oUF.CidadesCarregadas then   // Já Carregou essa UF ?
    Exit;

  AURL := StringReplace(CIBGE_URL_MUN_UF, '{idUF}', IntToStrZero(ACodUF,2), []);
  HTTPGet(AURL);
  fListaCidades.AddFromJSonStr(RespHTTP.Text);

  oUF.CidadesCarregadas := True;
  SalvarCache;
end;

procedure TACBrIBGE.ObterCidades(const AUF: String);
var
  CodUF: Integer;
begin
  if (fListaUFs.Count = 0) then
    ObterUFs;

  CodUF := UFToCodUF(AUF);
  if CodUF > 0 then
    ObterCidades(CodUF)
  else
    raise EACBrIBGEException.CreateFmt( ACBrStr('UF: %s inválida'), [AUF] );
end;

procedure TACBrIBGE.SalvarCidades(AStream: TStream);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SalvarCidades(SL);
    SL.SaveToStream(AStream);
  finally
    SL.Free;
  end;
end;

procedure TACBrIBGE.SalvarCidades(AStringList: TStrings);
var
  CodUF, I, J: Integer;
begin
  CodUF := 0;
  fListaCidades.SortByCodMunicipio;

  AStringList.Clear;
  For I := 0 to fListaCidades.Count-1 do
  begin
    if CodUF <> fListaCidades[I].CodUF then
    begin
      CodUF := fListaCidades[I].CodUF;
      J := fListaUFs.Find(CodUF);
      AStringList.Add(fListaUFs[J].AsString);
    end;

    AStringList.Add(fListaCidades[I].AsString);
  end;
end;

procedure TACBrIBGE.SalvarCidades(AFile: String; Overwrite: Boolean);
var
  SL: TStringList;
begin
  AFile := Trim(AFile);

  if (AFile = '') then
    raise EACBrIBGEException.Create( ACBrStr('Nome do Arquivo não informado') );

  if (not Overwrite) and FileExists(AFile) then
    raise EACBrIBGEException.CreateFmt( ACBrStr('Arquivo %s já existente'), [AFile] );

  SL := TStringList.Create;
  try
    SalvarCidades(SL);
    SL.SaveToFile(AFile);
  finally
    SL.Free;
  end;
end;

procedure TACBrIBGE.ObterEstatisticasCidadesUF(const AUF: String);
var
  CodUF: Integer;
begin
  CodUF := UFToCodUF(AUF);
  if (CodUF > 0) then
    ObterEstatisticasCidadesUF(CodUF);
end;

procedure TACBrIBGE.ObterEstatisticasCidade(const ACodMunicipio: Integer);
begin
  ObterEstatisticasCidades(IntToStr(ACodMunicipio));
end;

procedure TACBrIBGE.ObterEstatisticasCidadesUF(const ACodUF: Integer);
var
  ListaMunicipios, OldCacheArquivo: String;
  I, Itens: Integer;
begin
  ObterCidades(ACodUF);

  OldCacheArquivo := CacheArquivo;
  try
    CacheArquivo := '';   // Desabilita o Cache
    ListaMunicipios := '';
    Itens := 0;
    I := fListaCidades.Find(ACodUF * 100000, False);  // Acha a primeira cidade dessa UF
    while (I > 0) and (I < fListaCidades.Count) and (fListaCidades[I].CodUF = ACodUF) do
    begin
      ListaMunicipios := ListaMunicipios + IntToStr(fListaCidades[I].CodMunicipio)+'|';
      Inc(Itens);

      if (Itens > 24) then
      begin
        ObterEstatisticasCidades(ListaMunicipios);
        ListaMunicipios := '';
        Itens := 0;
      end;

      Inc(I);
    end;

    if (Itens > 0) then
      ObterEstatisticasCidades(ListaMunicipios);
  finally
    CacheArquivo := OldCacheArquivo;
  end;

  SalvarCache;
end;

procedure TACBrIBGE.ObterEstatisticasCidades(const ListaMunicipios: String);
var
  AURL, Pesquisas: String;
begin
  Pesquisas := IntToStr(CIBGE_IND_MUN_AREA)+'|'+IntToStr(CIBGE_IND_MUN_POPULACAO);
  AURL := StringReplace(CIBGE_URL_EST_MUN, '{indicadores}', Pesquisas, []);
  AURL := StringReplace(AURL, '{idMunicipio}', ListaMunicipios, []);

  HTTPGet(AURL);
  fListaCidades.ParseJSonStat(RespHTTP.Text);

  SalvarCache;
end;

procedure TACBrIBGE.SalvarCache;
var
  Tratado: Boolean;
  Arq: String;
  SL: TStringList;
begin
  Arq := CacheArquivo;
  if (Arq = '') and (not Assigned(fOnGravarCache)) then
    Exit;

  if (fListaUFs.Count < 1) then
    raise EACBrIBGEException.Create( ACBrStr('Tabelas de UF não está na memória') );

  if (fListaCidades.Count < 1) then
    raise EACBrIBGEException.Create( ACBrStr('Tabelas de Cidades não está na memória') );

  SL := TStringList.Create;
  try
    SalvarCidades(SL);

    Tratado := False;
    if Assigned(fOnGravarCache) then
      fOnGravarCache(SL, Tratado);

    if (not Tratado) and (Arq <> '') then
      SL.SaveToFile(Arq);
  finally
    SL.Free
  end;
end;

procedure TACBrIBGE.CarregarCache;
var
  Arq, Aline: String;
  SL: TStringList;
  Tratado: Boolean;
  I: Integer;
  oUF: TACBrIBGEUF;
  oCidade: TACBrIBGECidade;
  DataCache: TDateTime;
begin
  fCacheLido := True;
  Arq := CacheArquivo;
  if (Arq = '') and (not Assigned(fOnLerCache)) then
    Exit;

  SL := TStringList.Create;
  try
    Tratado := False;
    if Assigned(fOnLerCache) then
      fOnLerCache(SL, Tratado);

    if (not Tratado) and (Arq <> '') and FileExists(Arq) then
    begin
      if CacheDiasValidade > 0 then
      begin
         DataCache := FileDateToDateTime(FileAge(Arq));
         if (DaysBetween(Now, DataCache) > CacheDiasValidade) then
         begin
           DeleteFile(Arq);
           Exit;
         end;
      end;

      SL.LoadFromFile(Arq);
    end;

    if SL.Count > 0 then
    begin
      fListaUFs.Clear;
      fListaCidades.Clear;

      For I := 0 to SL.Count-1 do
      begin
        Aline := SL[I];

        if (pos('|', Aline) = 3) then
        begin
          oUF := TACBrIBGEUF.Create;
          try
            oUF.AsString := Aline;
            oUF.CidadesCarregadas := True;
            fListaUFs.Add(oUF);
          except
            oUF.Free;
          end;
        end
        else
        begin
          oCidade := TACBrIBGECidade.Create;
          try
            oCidade.AsString := Aline;
            if IgnorarCaixaEAcentos then
              oCidade.MunicipioIdx := LowerCase(TiraAcentos(oCidade.Municipio))
            else
              oCidade.MunicipioIdx := oCidade.Municipio;

            fListaCidades.Add(oCidade);
          except
            oCidade.Free;
          end;
        end;
      end;
    end;
  finally
    SL.Free
  end;
end;

procedure TACBrIBGE.ObterUFs;
begin
  if not fCacheLido then
    CarregarCache;

  if (fListaUFs.Count >= CIBGE_UF_COUNT) then  // Já fez a carga ?
    Exit;

  fListaUFs.Clear;
  HTTPGet(CIBGE_URL_UF);
  fListaUFs.AddFromJSonStr(RespHTTP.Text);
end;

procedure TACBrIBGE.ObterEstatisticasUF;
var
  AURL, Pesquisas, UFs: String;
  I: Integer;
begin
  Pesquisas := IntToStr(CIBGE_IND_UF_AREA)+'|'+IntToStr(CIBGE_IND_UF_POPULACAO);
  UFs := '';
  for I := 0 to fListaUFs.Count-1 do
    UFs := UFs + IntToStrZero(fListaUFs[I].CodUF,2) + '|';

  AURL := StringReplace(CIBGE_URL_EST_UF, '{idPesquisas}', Pesquisas, []);
  AURL := StringReplace(AURL, '{idUF}', UFs, []);

  HTTPGet(AURL);
  fListaUFs.ParseJSonStat(RespHTTP.Text);
end;

function TACBrIBGE.UFToCodUF(AUF: String): Integer;
begin
  if (fListaUFs.Count = 0) then
    ObterUFs;

  Result := fListaUFs.UFToCodUF(AUF);
end;

end.

