{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXBase;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrBase;

resourcestring
  sErroMetodoNaoImplementado = 'Método %s não implementado para Classe %s';

const
  cBRCountryCode = 'BR';
  cMPMValueNotInformed = '***';

type
  TACBrPIXAPIVersion = ( verNenhuma, verPropria,
                         ver200,
                         ver210, ver211, ver212,
                         ver220, ver221, ver222,
                         ver230,
                         ver250,
                         ver260, ver261, ver262 );

  TACBrPIXTipoChave = ( tchNenhuma,
                        tchEmail,
                        tchCPF,
                        tchCNPJ,
                        tchCelular,
                        tchAleatoria );

  TACBrPIXStatusCobranca = ( stcNENHUM,
                             stcATIVA,
                             stcCONCLUIDA,
                             stcREMOVIDA_PELO_USUARIO_RECEBEDOR,
                             stcREMOVIDA_PELO_PSP );

  TACBrPIXStatusDevolucao = ( stdNENHUM,
                              stdEM_PROCESSAMENTO,
                              stdDEVOLVIDO,
                              stdNAO_REALIZADO );

  TACBrPIXStatusLoteCobranca = ( stlNENHUM,
                                 stlEM_PROCESSAMENTO,
                                 stlCRIADA,
                                 stlNEGADA );

  TACBrPIXNaturezaDevolucao = ( ndNENHUMA, ndORIGINAL, ndRETIRADA ) ;

  TACBrPIXModalidadeAgente = ( maNENHUM,
                               maAGTEC,
                               maAGTOT,
                               maAGPSS );

  TACBrPIXTipoCobranca = ( tcoNenhuma, tcoCob, tcoCobV );

  EACBrPixException = class(EACBrException);

  { TACBrPIXSchema }

  TACBrPIXSchema = class
  private
    function GetAsJSON: String; virtual;
    procedure SetAsJSON(AValue: String); virtual;

    function GetJSONContext(AJSon: TACBrJSONObject): TACBrJSONObject;
  protected
    fIsBacen: Boolean;
    fpObjectName: String;

    procedure AssignSchema(ASource: TACBrPIXSchema); virtual;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); virtual;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); virtual;

  public
    constructor Create(const ObjectName: String = ''); virtual;
    procedure Clear; virtual;
    function IsEmpty: Boolean; virtual;
    procedure WriteToJSon(AJSon: TACBrJSONObject);
    procedure ReadFromJSon(AJSon: TACBrJSONObject);

    property AsJSON: String read GetAsJSON write SetAsJSON;
    property IsBacen: Boolean read fIsBacen write fIsBacen default True;
  end;

  { TACBrPIXSchemaArray }

  TACBrPIXSchemaArray = class(TACBrObjectList)
  private
    function GetAsJSON: String;
    procedure SetAsJSON(AValue: String);
  protected
    fpArrayName: String;
    function NewSchema: TACBrPIXSchema; virtual;
  public
    constructor Create(const ArrayName: String);
    procedure Clear; override;
    function IsEmpty: Boolean; virtual;
    procedure Assign(Source: TACBrObjectList); virtual;

    procedure WriteToJSon(AJSon: TACBrJSONObject); virtual;
    procedure ReadFromJSon(AJSon: TACBrJSONObject); virtual;

    property AsJSON: String read GetAsJSON write SetAsJSON;
  end;


  function PIXStatusCobrancaToString(AStatus: TACBrPIXStatusCobranca): String;
  function StringToPIXStatusCobranca(const AString: String): TACBrPIXStatusCobranca;

  function PIXModalidadeAgenteToString(AModalidadeAgente: TACBrPIXModalidadeAgente): String;
  function StringToPIXModalidadeAgente(const AString: String): TACBrPIXModalidadeAgente;

  function PIXStatusDevolucaoToString(AStatus: TACBrPIXStatusDevolucao): String;
  function StringToPIXStatusDevolucao(const AString: String): TACBrPIXStatusDevolucao;

  function PIXNaturezaDevolucaoToString(AStatus: TACBrPIXNaturezaDevolucao): String;
  function StringToPIXNaturezaDevolucao(const AString: String): TACBrPIXNaturezaDevolucao;

  function PIXTipoCobrancaToString(ACob: TACBrPIXTipoCobranca): String;
  function StringToPIXTipoCobranca(const AString: String): TACBrPIXTipoCobranca;

  function PIXStatusLoteCobrancaToString(AStatus: TACBrPIXStatusLoteCobranca): String;
  function StringToPIXStatusLoteCobranca(const AString: String): TACBrPIXStatusLoteCobranca;

implementation

uses
  ACBrUtil.Strings;

function PIXStatusCobrancaToString(AStatus: TACBrPIXStatusCobranca): String;
begin
  case AStatus of
    stcATIVA: Result := 'ATIVA';
    stcCONCLUIDA: Result := 'CONCLUIDA';
    stcREMOVIDA_PELO_USUARIO_RECEBEDOR: Result := 'REMOVIDA_PELO_USUARIO_RECEBEDOR';
    stcREMOVIDA_PELO_PSP: Result := 'REMOVIDA_PELO_PSP';
  else
    Result := '';
  end;
end;

function StringToPIXStatusCobranca(const AString: String): TACBrPIXStatusCobranca;
var
  s: String;
begin
  s := UpperCase(Trim(AString));
  if (s = 'ATIVA') then
    Result := stcATIVA
  else if (s = 'CONCLUIDA') then
    Result := stcCONCLUIDA
  else if (s = 'REMOVIDA_PELO_USUARIO_RECEBEDOR') then
    Result := stcREMOVIDA_PELO_USUARIO_RECEBEDOR
  else if (s = 'REMOVIDA_PELO_PSP') then
    Result := stcREMOVIDA_PELO_PSP
  else
    Result := stcNENHUM;
end;

function PIXModalidadeAgenteToString(AModalidadeAgente: TACBrPIXModalidadeAgente): String;
begin
  case AModalidadeAgente of
    maAGTEC: Result := 'AGTEC';
    maAGTOT: Result := 'AGTOT';
    maAGPSS: Result := 'AGPSS';
  else
    Result := '';
  end;
end;

function StringToPIXModalidadeAgente(const AString: String): TACBrPIXModalidadeAgente;
var
  s: String;
begin
  s := UpperCase(Trim(AString));
  if (s = 'AGTEC') then
    Result := maAGTEC
  else if (s = 'AGTOT') then
    Result := maAGTOT
  else if (s = 'AGPSS') then
    Result := maAGPSS
  else
    Result := maNENHUM;
end;

function PIXStatusDevolucaoToString(AStatus: TACBrPIXStatusDevolucao): String;
begin
  case AStatus of
    stdEM_PROCESSAMENTO: Result := 'EM_PROCESSAMENTO';
    stdDEVOLVIDO: Result := 'DEVOLVIDO';
    stdNAO_REALIZADO: Result := 'NAO_REALIZADO';
  else
    Result := '';
  end;
end;

function StringToPIXStatusDevolucao(const AString: String): TACBrPIXStatusDevolucao;
var
  s: String;
begin
  s := UpperCase(Trim(AString));
  if (s = 'EM_PROCESSAMENTO') then
    Result := stdEM_PROCESSAMENTO
  else if (s = 'DEVOLVIDO') then
    Result := stdDEVOLVIDO
  else if (s = 'NAO_REALIZADO') then
    Result := stdNAO_REALIZADO
  else
    Result := stdNENHUM;
end;

function PIXNaturezaDevolucaoToString(AStatus: TACBrPIXNaturezaDevolucao): String;
begin
  case AStatus of
    ndORIGINAL: Result := 'ORIGINAL';
    ndRETIRADA: Result := 'RETIRADA';
  else
    Result := '';
  end;
end;

function StringToPIXNaturezaDevolucao(const AString: String): TACBrPIXNaturezaDevolucao;
var
  s: String;
begin
  s := UpperCase(Trim(AString));
  if (s = 'ORIGINAL') then
    Result := ndORIGINAL
  else if (s = 'RETIRADA') then
    Result := ndRETIRADA
  else
    Result := ndNENHUMA;
end;

function PIXTipoCobrancaToString(ACob: TACBrPIXTipoCobranca): String;
begin
  case ACob of
    tcoCobV: Result := 'cobv';
    tcoCob: Result := 'cob';
  else
    Result := '';
  end;
end;

function StringToPIXTipoCobranca(const AString: String): TACBrPIXTipoCobranca;
var
  s: String;
begin
  s := UpperCase(Trim(AString));
  if (s = 'COBV') then
    Result := tcoCobV
  else if (s = 'COB') then
    Result := tcoCob
  else
    Result := tcoNenhuma;
end;

function PIXStatusLoteCobrancaToString(AStatus: TACBrPIXStatusLoteCobranca): String;
begin
  case AStatus of
    stlEM_PROCESSAMENTO: Result := 'EM_PROCESSAMENTO';
    stlCRIADA: Result := 'CRIADA';
    stlNEGADA: Result := 'NEGADA';
  else
    Result := '';
  end;
end;

function StringToPIXStatusLoteCobranca(const AString: String): TACBrPIXStatusLoteCobranca;
var
  s: String;
begin
  s := UpperCase(Trim(AString));
  if (s = 'EM_PROCESSAMENTO') then
    Result := stlEM_PROCESSAMENTO
  else if (s = 'CRIADA') then
    Result := stlCRIADA
  else if (s = 'NEGADA') then
    Result := stlNEGADA
  else
    Result := stlNENHUM;
end;

{ TACBrPIXSchema }

constructor TACBrPIXSchema.Create(const ObjectName: String);
begin
  inherited Create;
  fpObjectName := ObjectName;
end;

procedure TACBrPIXSchema.Clear;
begin
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['Clear', ClassName]);
end;

function TACBrPIXSchema.IsEmpty: Boolean;
begin
  Result := False;
end;

procedure TACBrPIXSchema.AssignSchema(ASource: TACBrPIXSchema);
begin
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['AssignSchema', ClassName]);
end;

function TACBrPIXSchema.GetAsJSON: String;
var
  js: TACBrJSONObject;
begin
  js := TACBrJSONObject.Create;
  try
    WriteToJSon(js);
    Result := js.ToJSON;
  finally
    js.Free;
  end;
end;

procedure TACBrPIXSchema.SetAsJSON(AValue: String);
var
  js: TACBrJSONObject;
begin
  Clear;
  js := TACBrJSONObject.Parse(AValue) as TACBrJSONObject;
  try
    ReadFromJSon(js);
  finally
    js.Free;
  end;
end;

function TACBrPIXSchema.GetJSONContext(AJSon: TACBrJSONObject): TACBrJSONObject;
begin
  if (fpObjectName <> '') then
  begin
    Result := AJSon.AsJSONObject[fpObjectName];
    if (not Assigned(Result)) then
    begin
      AJSon.AddPairJSONObject(fpObjectName, EmptyStr);
      Result := AJSon.AsJSONObject[fpObjectName];
    end;
  end
  else
    Result := AJSon;
end;

procedure TACBrPIXSchema.WriteToJSon(AJSon: TACBrJSONObject);
begin
  if IsEmpty then
    Exit;

  if Assigned(GetJSONContext(AJSon)) then
    DoWriteToJSon(GetJSONContext(AJSon));
end;

procedure TACBrPIXSchema.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['DoWriteToJSon', ClassName]);
end;

procedure TACBrPIXSchema.ReadFromJSon(AJSon: TACBrJSONObject);
begin
  Clear;
  DoReadFromJSon(GetJSONContext(AJSon));
end;

procedure TACBrPIXSchema.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['DoReadFromJSon', ClassName]);
end;

{ TACBrPIXSchemaArray }

constructor TACBrPIXSchemaArray.Create(const ArrayName: String);
begin
  inherited Create(True);
  fpArrayName := ArrayName;
end;

procedure TACBrPIXSchemaArray.Clear;
begin
  inherited Clear;
end;

function TACBrPIXSchemaArray.IsEmpty: Boolean;
begin
  Result := (Count < 1);
end;

function TACBrPIXSchemaArray.NewSchema: TACBrPIXSchema;
begin
  {$IfDef FPC}Result := Nil;{$EndIf}
  raise EACBrPixException.CreateFmt(ACBrStr(sErroMetodoNaoImplementado), ['NewSchema', ClassName]);
end;

procedure TACBrPIXSchemaArray.Assign(Source: TACBrObjectList);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
    NewSchema.AssignSchema(TACBrPIXSchema(Source[i]));
end;

procedure TACBrPIXSchemaArray.WriteToJSon(AJSon: TACBrJSONObject);
var
  i: Integer;
  ja: TACBrJsonArray;
begin
  if IsEmpty then
    Exit;

  ja := TACBrJSONArray.Create;
  try
    for i := 0 to Count - 1 do
      ja.AddElementJSONString(TACBrPIXSchema(Items[i]).AsJSON);

    AJSon.AddPair(fpArrayName, ja);
  except
    ja.Free;
    raise;
  end;
end;

procedure TACBrPIXSchemaArray.ReadFromJSon(AJSon: TACBrJSONObject);
var
  i: Integer;
  ja: TACBrJsonArray;
begin
  Clear;

  ja := AJSon.AsJSONArray[fpArrayName];
  for i := 0 to ja.Count - 1 do
    NewSchema.ReadFromJSon(ja.ItemAsJSONObject[i]);
end;

function TACBrPIXSchemaArray.GetAsJSON: String;
var
  js: TACBrJSONObject;
begin
  js := TACBrJSONObject.Create;
  try
    WriteToJSon(js);
    Result := js.ToJSON;
  finally
    js.Free;
  end;
end;

procedure TACBrPIXSchemaArray.SetAsJSON(AValue: String);
var
  js: TACBrJSONObject;
begin
  Clear;
  js := TACBrJSONObject.Parse(AValue) as TACBrJSONObject;
  try
    ReadFromJSon(js);
  finally
    js.Free;
  end;
end;

end.

