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

unit ACBrPIXSchemasPix;

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
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr,
  {$Else}
   Jsons,
  {$EndIf}
  ACBrBase, ACBrPIXBase, ACBrPIXSchemasDevolucao;

type

  { TACBrPIXValor }

  TACBrPIXValor = class(TACBrPIXSchema)
  private
    fObjectName: String;
    fvalor: Currency;
  public
    constructor Create(const ObjectName: String);
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXValor);

    property valor: Currency read fvalor write fvalor;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXSaqueTroco }

  TACBrPIXSaqueTroco = class(TACBrPIXSchema)
  private
    fObjectName: String;
    fmodalidadeAgente: TACBrPIXModalidadeAgente;
    fprestadorDeServicoDeSaque: Integer;
    fvalor: Currency;
    procedure SetPrestadorDeServicoDeSaque(AValue: Integer);
  public
    constructor Create(const ObjectName: String);
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXSaqueTroco);

    property valor: Currency read fvalor write fvalor;
    property modalidadeAgente: TACBrPIXModalidadeAgente read fmodalidadeAgente write fmodalidadeAgente;
    property prestadorDeServicoDeSaque: Integer read fprestadorDeServicoDeSaque write SetPrestadorDeServicoDeSaque;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXComponentesValor }

  TACBrPIXComponentesValor = class(TACBrPIXSchema)
  private
    fabatimento: TACBrPIXValor;
    fdesconto: TACBrPIXValor;
    fjuros: TACBrPIXValor;
    fmulta: TACBrPIXValor;
    foriginal: TACBrPIXValor;
    fsaque: TACBrPIXSaqueTroco;
    ftroco: TACBrPIXSaqueTroco;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXComponentesValor);

    property original: TACBrPIXValor read foriginal;
    property saque: TACBrPIXSaqueTroco read fsaque;
    property troco: TACBrPIXSaqueTroco read ftroco;
    property juros: TACBrPIXValor read fjuros;
    property multa: TACBrPIXValor read fmulta;
    property abatimento: TACBrPIXValor read fabatimento;
    property desconto: TACBrPIXValor read fdesconto;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIX }

  TACBrPIX = class(TACBrPIXSchema)
  private
    fchave: String;
    fcomponentesValor: TACBrPIXComponentesValor;
    fdevolucoes: TACBrPIXDevolucoes;
    fendToEndId: String;
    fhorario: TDateTime;
    finfoPagador: String;
    ftxid: String;
    fvalor: Currency;
    procedure SetChave(AValue: String);
    procedure SetendToEndId(const AValue: String);
    procedure SetinfoPagador(AValue: String);
    procedure SetTxid(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIX);

    property endToEndId: String read fendToEndId write SetendToEndId;   //"Id fim a fim da transação"
    property txid: String read ftxid write SetTxid;
    property valor: Currency read fvalor write fvalor;
    property componentesValor: TACBrPIXComponentesValor read fcomponentesValor;
    property chave: String read fchave write SetChave;
    property horario: TDateTime read fhorario write fhorario;
    property infoPagador: String read finfoPagador write SetinfoPagador;
    property devolucoes: TACBrPIXDevolucoes read fdevolucoes;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXArray }

  TACBrPIXArray = class(TACBrObjectList)
  private
    fObjectName: String;
    function GetItem(Index: Integer): TACBrPIX;
    procedure SetItem(Index: Integer; Value: TACBrPIX);
  public
    constructor Create(const ObjectName: String);
    procedure Assign(Source: TACBrPIXArray);
    Function Add(ADevolucao: TACBrPIX): Integer;
    Procedure Insert(Index: Integer; ADevolucao: TACBrPIX);
    function New: TACBrPIX;
    property Items[Index: Integer]: TACBrPIX read GetItem write SetItem; default;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;


implementation

uses
  ACBrPIXUtil,
  ACBrUtil;

{ TACBrPIXValor }

constructor TACBrPIXValor.Create(const ObjectName: String);
begin
  inherited Create;
  fObjectName := ObjectName;
  Clear;
end;

procedure TACBrPIXValor.Clear;
begin
  fvalor := 0;
end;

procedure TACBrPIXValor.Assign(Source: TACBrPIXValor);
begin
  fvalor := Source.valor;
end;

procedure TACBrPIXValor.WriteToJSon(AJSon: TJsonObject);
var
  jso: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jso := AJSon.O[fObjectName];
   jso.S['valor'] := FormatarValorPIX(valor);
  {$Else}
   jso := AJSon[fObjectName].AsObject;
   jso['valor'].AsString := FormatarValorPIX(valor);
  {$EndIf}
end;

procedure TACBrPIXValor.ReadFromJSon(AJSon: TJsonObject);
var
  jso: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jso := AJSon.O[fObjectName];
   valor := StringToFloatDef(jso.S['valor'], 0);
  {$Else}
   jso := AJSon[fObjectName].AsObject;
   valor := StringToFloatDef(jso['valor'].AsString, 0);
  {$EndIf}
end;

{ TACBrPIXSaqueTroco }

constructor TACBrPIXSaqueTroco.Create(const ObjectName: String);
begin
  inherited Create;
  fObjectName := ObjectName;
  Clear;
end;

procedure TACBrPIXSaqueTroco.Clear;
begin
  fvalor := 0;
  fmodalidadeAgente := maNENHUM;
  fprestadorDeServicoDeSaque := -1;
end;

procedure TACBrPIXSaqueTroco.Assign(Source: TACBrPIXSaqueTroco);
begin
  fvalor := Source.valor;
  fmodalidadeAgente := Source.modalidadeAgente;
  prestadorDeServicoDeSaque := Source.prestadorDeServicoDeSaque;
end;

procedure TACBrPIXSaqueTroco.WriteToJSon(AJSon: TJsonObject);
var
  jso: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jso := AJSon.O[fObjectName];
   jso.S['valor'] := FormatarValorPIX(valor);
   if (fmodalidadeAgente <> maNENHUM) then
     jso.S['modalidadeAgente'] := PIXModalidadeAgenteToString(modalidadeAgente);
   if (prestadorDeServicoDeSaque >= 0) then
     jso.S['prestadorDeServicoDeSaque'] := IntToStrZero(prestadorDeServicoDeSaque, 8);
  {$Else}
   jso := AJSon[fObjectName].AsObject;
   jso['valor'].AsString := FormatarValorPIX(valor);
   if (fmodalidadeAgente <> maNENHUM) then
     jso['modalidadeAgente'].AsString := PIXModalidadeAgenteToString(modalidadeAgente);
   if (prestadorDeServicoDeSaque >= 0) then
     jso['prestadorDeServicoDeSaque'].AsString := IntToStrZero(prestadorDeServicoDeSaque, 8);
  {$EndIf}
end;

procedure TACBrPIXSaqueTroco.ReadFromJSon(AJSon: TJsonObject);
var
  jso: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jso := AJSon.O[fObjectName];
   valor := StringToFloatDef( jso.S['valor'], 0);
   modalidadeAgente := StringToPIXModalidadeAgente( jso.S['modalidadeAgente'] );
   prestadorDeServicoDeSaque := StrToIntDef(jso.S['prestadorDeServicoDeSaque'], -1);
  {$Else}
   jso := AJSon[fObjectName].AsObject;
   valor := StringToFloatDef( jso['valor'].AsString, 0);
   modalidadeAgente := StringToPIXModalidadeAgente( jso['modalidadeAgente'].AsString );
   prestadorDeServicoDeSaque := StrToIntDef(jso['prestadorDeServicoDeSaque'].AsString, -1);
  {$EndIf}
end;

procedure TACBrPIXSaqueTroco.SetPrestadorDeServicoDeSaque(AValue: Integer);
var
  e: String;
begin
  if (fprestadorDeServicoDeSaque = AValue) then
    Exit;

  e := ValidarPSS(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fprestadorDeServicoDeSaque := AValue;
end;

{ TACBrPIXComponentesValor }

constructor TACBrPIXComponentesValor.Create;
begin
  inherited Create;
  fabatimento := TACBrPIXValor.Create('abatimento');
  fdesconto := TACBrPIXValor.Create('desconto');
  fjuros := TACBrPIXValor.Create('juros');
  fmulta := TACBrPIXValor.Create('multa');
  foriginal := TACBrPIXValor.Create('original');
  fsaque := TACBrPIXSaqueTroco.Create('saque');
  ftroco := TACBrPIXSaqueTroco.Create('troco');
end;

destructor TACBrPIXComponentesValor.Destroy;
begin
  fabatimento.Free;
  fdesconto.Free;
  fjuros.Free;
  fmulta.Free;
  foriginal.Free;
  fsaque.Free;
  ftroco.Free;
  inherited Destroy;
end;

procedure TACBrPIXComponentesValor.Clear;
begin
  fabatimento.Clear;
  fdesconto.Clear;
  fjuros.Clear;
  fmulta.Clear;
  foriginal.Clear;
  fsaque.Clear;
  ftroco.Clear;
end;

procedure TACBrPIXComponentesValor.Assign(Source: TACBrPIXComponentesValor);
begin
  fabatimento.Assign(Source.abatimento);
  fdesconto.Assign(Source.desconto);
  fjuros.Assign(Source.juros);
  fmulta.Assign(Source.multa);
  foriginal.Assign(Source.original);
  fsaque.Assign(Source.saque);
  ftroco.Assign(Source.troco);
end;

procedure TACBrPIXComponentesValor.WriteToJSon(AJSon: TJsonObject);
var
  jso: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jso := AJSon.O['componentesValor'];
  {$Else}
   jso := AJSon['componentesValor'].AsObject;
  {$EndIf}

  foriginal.WriteToJSon(jso);
  fsaque.WriteToJSon(jso);
  ftroco.WriteToJSon(jso);
  fjuros.WriteToJSon(jso);
  fmulta.WriteToJSon(jso);
  fabatimento.WriteToJSon(jso);
  fdesconto.WriteToJSon(jso);
end;

procedure TACBrPIXComponentesValor.ReadFromJSon(AJSon: TJsonObject);
var
  jso: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jso := AJSon.O['componentesValor'];
  {$Else}
   jso := AJSon['componentesValor'].AsObject;
  {$EndIf}

  foriginal.ReadFromJSon(jso);
  fsaque.ReadFromJSon(jso);
  ftroco.ReadFromJSon(jso);
  fjuros.ReadFromJSon(jso);
  fmulta.ReadFromJSon(jso);
  fabatimento.ReadFromJSon(jso);
  fdesconto.ReadFromJSon(jso);
end;

{ TACBrPIX }

constructor TACBrPIX.Create;
begin
  inherited;
  fcomponentesValor := TACBrPIXComponentesValor.Create;
  fdevolucoes := TACBrPIXDevolucoes.Create();
  Clear;
end;

destructor TACBrPIX.Destroy;
begin
  fcomponentesValor.Free;
  fdevolucoes.Free;
  inherited Destroy;
end;

procedure TACBrPIX.Clear;
begin
  fendToEndId := '';
  ftxid := '';
  fvalor := 0;
  fchave := '';
  fhorario := 0;
  finfoPagador := '';
  ftxid := '';
  fvalor := 0;

  fcomponentesValor.Clear;
  fdevolucoes.Clear
end;

procedure TACBrPIX.Assign(Source: TACBrPIX);
begin
  fendToEndId := Source.endToEndId;
  ftxid := Source.txid;
  fvalor := Source.valor;
  fchave := Source.chave;
  fhorario := Source.horario;
  finfoPagador := Source.infoPagador;
  ftxid := Source.txid;
  fvalor := Source.valor;

  fcomponentesValor.Assign(Source.componentesValor);
  fdevolucoes.Assign(Source.devolucoes);
end;

procedure TACBrPIX.SetendToEndId(const AValue: String);
var
  e: String;
begin
  if (fendToEndId = AValue) then
    Exit;

  e := ValidarEndToEndId(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fendToEndId := AValue;
end;

procedure TACBrPIX.SetinfoPagador(AValue: String);
begin
  if finfoPagador = AValue then
    Exit;
  finfoPagador := copy(AValue, 1, 140);
end;

procedure TACBrPIX.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['endToEndId'] := fendToEndId;
   if (ftxid <> '') then
     AJSon.S['txid'] := ftxid;
   AJSon.S['valor'] := FormatarValorPIX(fvalor);
   fcomponentesValor.WriteToJSon(AJSon);
   if (fchave <> '') then
     AJSon.S['chave'] := fchave;
   AJSon.S['horario'] := DateTimeToIso8601(fhorario);
   AJSon.S['infoPagador'] := finfoPagador;
   fdevolucoes.WriteToJSon(AJSon);
  {$Else}
   AJSon['endToEndId'].AsString := fendToEndId;
   if (ftxid <> '') then
     AJSon['txid'].AsString := ftxid;
   AJSon['valor'].AsString := FormatarValorPIX(fvalor);
   fcomponentesValor.WriteToJSon(AJSon);
   if (fchave <> '') then
     AJSon['chave'].AsString := fchave;
   AJSon['horario'].AsString := DateTimeToIso8601(fhorario);
   AJSon['infoPagador'].AsString := finfoPagador;
   fdevolucoes.WriteToJSon(AJSon);
  {$EndIf}
end;

procedure TACBrPIX.ReadFromJSon(AJSon: TJsonObject);
var
  s: String;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fendToEndId := AJSon.S['endToEndId'];
   ftxid := AJSon.S['txid'];
   fvalor := StringToFloatDef(AJSon.S['valor'], 0);
   fcomponentesValor.ReadFromJSon(AJSon);
   fchave := AJSon.S['chave'];
   s := AJSon.S['horario'];
   if (s <> '') then
     fhorario := Iso8601ToDateTime(s);
   finfoPagador := AJSon.S['infoPagador'];
   fdevolucoes.ReadFromJSon(AJSon);
  {$Else}
   fendToEndId := AJSon['endToEndId'].AsString;
   ftxid := AJSon['txid'].AsString;
   fvalor := StringToFloatDef(AJSon['valor'].AsString, 0);
   fcomponentesValor.ReadFromJSon(AJSon);
   fchave := AJSon['chave'].AsString;
   s := AJSon['horario'].AsString;
   if (s <> '') then
     fhorario := Iso8601ToDateTime(s);
   finfoPagador := AJSon['infoPagador'].AsString;
   fdevolucoes.ReadFromJSon(AJSon);
  {$EndIf}
end;

procedure TACBrPIX.SetChave(AValue: String);
var
  e: String;
begin
  if fchave = AValue then
    Exit;

  e := ValidarChave(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fchave := AValue;
end;

procedure TACBrPIX.SetTxid(const AValue: String);
var
  e: String;
begin
  if ftxid = AValue then
    Exit;

  if (AValue = cMPMValueNotInformed) then
  begin
    fTxId := '';
    Exit;
  end;

  e := ValidarTxId(AValue, 35, 26);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fTxId := AValue;
end;

{ TACBrPIXArray }

function TACBrPIXArray.GetItem(Index: Integer): TACBrPIX;
begin
  Result := TACBrPIX(inherited Items[Index]);
end;

procedure TACBrPIXArray.SetItem(Index: Integer; Value: TACBrPIX);
begin
  inherited Items[Index] := Value;
end;

constructor TACBrPIXArray.Create(const ObjectName: String);
begin
  fObjectName := ObjectName;
  inherited Create(True);
end;

procedure TACBrPIXArray.Assign(Source: TACBrPIXArray);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
    New.Assign(Source[i]);
end;

function TACBrPIXArray.Add(ADevolucao: TACBrPIX): Integer;
begin
  Result := inherited Add(ADevolucao);
end;

procedure TACBrPIXArray.Insert(Index: Integer; ADevolucao: TACBrPIX);
begin
  inherited Insert(Index, ADevolucao);
end;

function TACBrPIXArray.New: TACBrPIX;
begin
  Result := TACBrPIX.Create;
  Self.Add(Result);
end;

procedure TACBrPIXArray.WriteToJSon(AJSon: TJsonObject);
var
  i: Integer;
  ja: TJsonArray;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   ja := AJSon.A[fObjectName];
   ja.Clear;
   for i := 0 to Count-1 do
     Items[i].WriteToJSon(ja.AddObject);
  {$Else}
   ja := AJSon[fObjectName].AsArray;
   ja.Clear;
   for i := 0 to Count-1 do
     Items[i].WriteToJSon(ja.Add.AsObject);
  {$EndIf}
end;

procedure TACBrPIXArray.ReadFromJSon(AJSon: TJsonObject);
var
  i: Integer;
  ja: TJsonArray;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   ja := AJSon.A[fObjectName];
   for i := 0 to ja.Count-1 do
     New.ReadFromJSon(ja.O[i]);
  {$Else}
   ja := AJSon[fObjectName].AsArray;
   for i := 0 to ja.Count-1 do
     New.ReadFromJSon(ja[i].AsObject);
  {$EndIf}
end;


end.

