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

unit ACBrPIXSchemasCobranca;

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
  ACBrBase, ACBrPIXBase,
  ACBrPIXSchemasCalendario, ACBrPIXSchemasDevedor, ACBrPIXSchemasLocation;

resourcestring
  sErroInfoAdicLimit = 'Limite de infoAdicionais atingido (50)';

type

  { TACBrPIXInfoAdicional }

  TACBrPIXInfoAdicional = class(TACBrPIXSchema)
  private
    fnome: String;
    fvalor: String;
    procedure SetNome(AValue: String);
    procedure SetValor(AValue: String);
  public
    constructor Create;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXInfoAdicional);

    property nome: String read fnome write SetNome;
    property valor: String read fvalor write SetValor;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXInfoAdicionalArray }

  TACBrPIXInfoAdicionalArray = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TACBrPIXInfoAdicional;
    procedure SetItem(Index: Integer; Value: TACBrPIXInfoAdicional);
  public
    procedure Assign(Source: TACBrPIXInfoAdicionalArray);
    Function Add(AInfoAdicional: TACBrPIXInfoAdicional): Integer;
    Procedure Insert(Index: Integer; AInfoAdicional: TACBrPIXInfoAdicional);
    function New: TACBrPIXInfoAdicional;
    property Items[Index: Integer]: TACBrPIXInfoAdicional read GetItem write SetItem; default;

    procedure WriteToJSon(AJSon: TJsonObject);
    procedure ReadFromJSon(AJSon: TJsonObject);
  end;

  { TACBrPIXSaqueTroco }

  TACBrPIXSaqueTroco = class(TACBrPIXSchema)
  private
    fmodalidadeAgente: TACBrPIXModalidadeAgente;
    fmodalidadeAlteracao: Boolean;
    fprestadorDoServicoDeSaque: Integer;
    fvalor: Currency;
    procedure SetprestadorDoServicoDeSaque(AValue: Integer);
  public
    constructor Create;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXSaqueTroco);

    property valor: Currency read fvalor write fvalor;
    property modalidadeAlteracao: Boolean read fmodalidadeAlteracao write fmodalidadeAlteracao;
    property modalidadeAgente: TACBrPIXModalidadeAgente read fmodalidadeAgente write fmodalidadeAgente;
    property prestadorDoServicoDeSaque: Integer read fprestadorDoServicoDeSaque write SetprestadorDoServicoDeSaque;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXRetirada }

  TACBrPIXRetirada = class(TACBrPIXSchema)
  private
    fsaque: TACBrPIXSaqueTroco;
    ftroco: TACBrPIXSaqueTroco;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXRetirada);

    property saque: TACBrPIXSaqueTroco read fsaque;
    property troco: TACBrPIXSaqueTroco read ftroco;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXCobValor }

  TACBrPIXCobValor = class(TACBrPIXSchema)
  private
    fmodalidadeAlteracao: Boolean;
    foriginal: Currency;
    fretirada: TACBrPIXRetirada;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXCobValor);

    property original: Currency read foriginal write foriginal;
    property modalidadeAlteracao: Boolean read fmodalidadeAlteracao write fmodalidadeAlteracao;
    property retirada: TACBrPIXRetirada read fretirada;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXCobBase }

  TACBrPIXCobBase = class(TACBrPIXSchema)
  private
    fchave: String;
    finfoAdicionais: TACBrPIXInfoAdicionalArray;
    fsolicitacaoPagador: String;
    procedure SetChave(AValue: String);
    procedure SetSolicitacaoPagador(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXCobBase);

    property chave: String read fchave write SetChave;
    property solicitacaoPagador: String read fsolicitacaoPagador write SetSolicitacaoPagador;
    property infoAdicionais: TACBrPIXInfoAdicionalArray read finfoAdicionais;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

  { TACBrPIXCobSolicitada }

  TACBrPIXCobSolicitada = class(TACBrPIXCobBase)
  private
    fcalendario: TACBrPIXCalendarioCobSolicitada;
    fdevedor: TACBrPIXDevedor;
    floc: TACBrPIXLocationCobSolicitada;
    fvalor: TACBrPIXCobValor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    procedure Assign(Source: TACBrPIXCobSolicitada);

    property calendario: TACBrPIXCalendarioCobSolicitada read fcalendario;
    property devedor: TACBrPIXDevedor read fdevedor;
    property loc: TACBrPIXLocationCobSolicitada read floc;
    property valor: TACBrPIXCobValor read fvalor;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

implementation

uses
  DateUtils, Math,
  ACBrUtil, ACBrConsts, ACBrPIXUtil;

{ TACBrPIXInfoAdicional }

constructor TACBrPIXInfoAdicional.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXInfoAdicional.Clear;
begin
  fnome := '';
  fvalor := '';
end;

procedure TACBrPIXInfoAdicional.Assign(Source: TACBrPIXInfoAdicional);
begin
  fnome := Source.nome;
  fvalor := Source.valor;
end;

procedure TACBrPIXInfoAdicional.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['nome'] := fnome;
   AJSon.S['valor'] := fvalor;
  {$Else}
   AJSon['nome'].AsString := fnome;
   AJSon['valor'].AsString := fvalor;
  {$EndIf}
end;

procedure TACBrPIXInfoAdicional.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   nome := AJSon.S['nome'];
   valor := AJSon.S['valor'];
  {$Else}
   nome := AJSon['nome'].AsString;
   valor := AJSon['valor'].AsString;
  {$EndIf}
end;

procedure TACBrPIXInfoAdicional.SetNome(AValue: String);
begin
  if fnome = AValue then
    Exit;
  fnome := copy(AValue, 1, 50);
end;

procedure TACBrPIXInfoAdicional.SetValor(AValue: String);
begin
  if fvalor = AValue then
    Exit;
  fvalor := copy(AValue, 1, 200);
end;

{ TACBrPIXInfoAdicionalArray }

function TACBrPIXInfoAdicionalArray.GetItem(Index: Integer): TACBrPIXInfoAdicional;
begin
  Result := TACBrPIXInfoAdicional(inherited Items[Index]);
end;

procedure TACBrPIXInfoAdicionalArray.SetItem(Index: Integer; Value: TACBrPIXInfoAdicional);
begin
  inherited Items[Index] := Value;
end;

procedure TACBrPIXInfoAdicionalArray.Assign(Source: TACBrPIXInfoAdicionalArray);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
    New.Assign(Source[i]);
end;

function TACBrPIXInfoAdicionalArray.Add(AInfoAdicional: TACBrPIXInfoAdicional): Integer;
begin
  if Count >= 50 then
    raise EACBrPixException.Create(ACBrStr(sErroInfoAdicLimit));

  Result := inherited Add(AInfoAdicional);
end;

procedure TACBrPIXInfoAdicionalArray.Insert(Index: Integer; AInfoAdicional: TACBrPIXInfoAdicional);
begin
  inherited Insert(Index, AInfoAdicional);
end;

function TACBrPIXInfoAdicionalArray.New: TACBrPIXInfoAdicional;
begin
  Result := TACBrPIXInfoAdicional.Create;
  Self.Add(Result);
end;

procedure TACBrPIXInfoAdicionalArray.WriteToJSon(AJSon: TJsonObject);
var
  i: Integer;
  ja: TJsonArray;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   ja := AJSon.A['infoAdicionais'];
   ja.Clear;
   for i := 0 to Count-1 do
     Items[i].WriteToJSon(ja.AddObject);
  {$Else}
   ja := AJSon['infoAdicionais'].AsArray;
   ja.Clear;
   for i := 0 to Count-1 do
     Items[i].WriteToJSon(ja.Add.AsObject);
  {$EndIf}
end;

procedure TACBrPIXInfoAdicionalArray.ReadFromJSon(AJSon: TJsonObject);
var
  i: Integer;
  ja: TJsonArray;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   ja := AJSon.A['infoAdicionais'];
   for i := 0 to ja.Count-1 do
     New.ReadFromJSon(ja.O[i]);
  {$Else}
   ja := AJSon['infoAdicionais'].AsArray;
   for i := 0 to ja.Count-1 do
     New.ReadFromJSon(ja[i].AsObject);
  {$EndIf}
end;

{ TACBrPIXSaqueTroco }

constructor TACBrPIXSaqueTroco.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXSaqueTroco.Clear;
begin
  fvalor := 0;
  fmodalidadeAlteracao := False;
  fmodalidadeAgente := maNENHUM;
  fprestadorDoServicoDeSaque := -1;
end;

procedure TACBrPIXSaqueTroco.Assign(Source: TACBrPIXSaqueTroco);
begin
  fvalor := Source.valor;
  fmodalidadeAlteracao := Source.modalidadeAlteracao;
  fmodalidadeAgente := Source.modalidadeAgente;
  fprestadorDoServicoDeSaque := Source.prestadorDoServicoDeSaque;
end;

procedure TACBrPIXSaqueTroco.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['valor'] := FormatarValorPIX(valor);
   AJSon.I['modalidadeAlteracao'] := IfThen(modalidadeAlteracao, 1, 0);
   if (fmodalidadeAgente <> maNENHUM) then
     AJSon.S['modalidadeAgente'] := PIXModalidadeAgenteToString(modalidadeAgente);
   if (prestadorDoServicoDeSaque >= 0) then
     AJSon.S['prestadorDoServicoDeSaque'] := IntToStrZero(prestadorDoServicoDeSaque, 8);
  {$Else}
   AJSon['valor'].AsString := FormatarValorPIX(valor);
   AJSon['modalidadeAlteracao'].AsInteger := IfThen(modalidadeAlteracao, 1, 0);
   if (fmodalidadeAgente <> maNENHUM) then
     AJSon['modalidadeAgente'].AsString := PIXModalidadeAgenteToString(modalidadeAgente);
   if (prestadorDoServicoDeSaque >= 0) then
     AJSon['prestadorDoServicoDeSaque'].AsString := IntToStrZero(prestadorDoServicoDeSaque, 8);
  {$EndIf}
end;

procedure TACBrPIXSaqueTroco.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   valor := StringToFloatDef( AJSon.S['valor'], 0 );
   modalidadeAlteracao := (AJSon.I['modalidadeAlteracao'] = 1);
   modalidadeAgente := StringToPIXModalidadeAgente( AJSon.S['modalidadeAgente'] );
   prestadorDoServicoDeSaque := StrToIntDef(AJSon.S['prestadorDoServicoDeSaque'], -1);
  {$Else}
   valor := StringToFloatDef( AJSon['valor'].AsString, 0 );
   modalidadeAlteracao := (AJSon['modalidadeAlteracao'].AsInteger = 1);
   modalidadeAgente := StringToPIXModalidadeAgente( AJSon['modalidadeAgente'].AsString );
   prestadorDoServicoDeSaque := StrToIntDef(AJSon['prestadorDoServicoDeSaque'].AsString, -1);
  {$EndIf}
end;

procedure TACBrPIXSaqueTroco.SetprestadorDoServicoDeSaque(AValue: Integer);
var
  e: String;
begin
  if (fprestadorDoServicoDeSaque = AValue) then
    Exit;

  e := ValidarPSS(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fprestadorDoServicoDeSaque := AValue;
end;

{ TACBrPIXRetirada }

constructor TACBrPIXRetirada.Create;
begin
  inherited;
  fsaque := TACBrPIXSaqueTroco.Create;
  ftroco := TACBrPIXSaqueTroco.Create;
end;

destructor TACBrPIXRetirada.Destroy;
begin
  fsaque.Free;
  ftroco.Free;
  inherited Destroy;
end;

procedure TACBrPIXRetirada.Clear;
begin
  fsaque.Clear;
  ftroco.Clear;
end;

procedure TACBrPIXRetirada.Assign(Source: TACBrPIXRetirada);
begin
  fsaque.Assign(Source.saque);
  ftroco.Assign(Source.troco);
end;

procedure TACBrPIXRetirada.WriteToJSon(AJSon: TJsonObject);
var
  s: String;
  st: TACBrPIXSaqueTroco;
  jsr, jsst: TJsonObject;
begin
  s := '';
  st := Nil;
  if (saque.modalidadeAgente <> maNENHUM) then
  begin
    st := saque;
    s := 'saque';
  end
  else if (troco.modalidadeAgente <> maNENHUM) then
  begin
    st := troco;
    s := 'troco';
  end;

  if (st <> nil) then
  begin
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     jsr := AJSon.O['retirada'];
     jsst := jsr.O[s];
    {$Else}
     jsr := AJSon['retirada'].AsObject;
     jsst := jsr[s].AsObject;
    {$EndIf}
    st.WriteToJSon(jsst);
  end;
end;

procedure TACBrPIXRetirada.ReadFromJSon(AJSon: TJsonObject);
var
  jsr, jss, jst: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsr := AJSon.O['retirada'];
   jss := jsr.O['saque'];
   jst := jsr.O['troco'];
   saque.ReadFromJSon(jss);
   troco.ReadFromJSon(jst);
  {$Else}
   jsr := AJSon['retirada'].AsObject;
   jss := jsr['saque'].AsObject;
   jst := jsr['troco'].AsObject;
   saque.ReadFromJSon(jss);
   troco.ReadFromJSon(jst);
  {$EndIf}
end;

{ TACBrPIXCobValor }

constructor TACBrPIXCobValor.Create;
begin
  inherited;
  fretirada := TACBrPIXRetirada.Create;
  Clear;
end;

destructor TACBrPIXCobValor.Destroy;
begin
  fretirada.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobValor.Clear;
begin
  foriginal := 0;
  fmodalidadeAlteracao := False;
  fretirada.Clear;
end;

procedure TACBrPIXCobValor.Assign(Source: TACBrPIXCobValor);
begin
  foriginal := Source.original;
  fmodalidadeAlteracao := Source.modalidadeAlteracao;
  fretirada.Assign(Source.retirada);
end;

procedure TACBrPIXCobValor.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['original'] := FormatarValorPIX(original);
   AJSon.I['modalidadeAlteracao'] := IfThen(modalidadeAlteracao, 1, 0);
  {$Else}
   AJSon['original'].AsString := FormatarValorPIX(original);
   AJSon['modalidadeAlteracao'].AsInteger := IfThen(modalidadeAlteracao, 1, 0);
  {$EndIf}
  retirada.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobValor.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   original := StringToFloatDef( AJSon.S['original'], 0 );
   modalidadeAlteracao := (AJSon.I['modalidadeAlteracao'] = 1);
  {$Else}
   original := StringToFloatDef( AJSon['original'].AsString, 0 );
   modalidadeAlteracao := (AJSon['modalidadeAlteracao'].AsInteger = 1);
  {$EndIf}
  retirada.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobBase }

constructor TACBrPIXCobBase.Create;
begin
  inherited;
  finfoAdicionais := TACBrPIXInfoAdicionalArray.Create();
  Clear;
end;

destructor TACBrPIXCobBase.Destroy;
begin
  finfoAdicionais.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobBase.Clear;
begin
  fchave := '';
  fsolicitacaoPagador := '';
  finfoAdicionais.Clear;
end;

procedure TACBrPIXCobBase.Assign(Source: TACBrPIXCobBase);
begin
  fchave := Source.chave;
  fsolicitacaoPagador := Source.solicitacaoPagador;
  finfoAdicionais.Assign(Source.infoAdicionais);
end;

procedure TACBrPIXCobBase.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (fchave <> '') then
     AJSon.S['chave'] := fchave;
   if (fsolicitacaoPagador <> '') then
     AJSon.S['solicitacaoPagador'] := fsolicitacaoPagador;
   finfoAdicionais.WriteToJSon(AJSon);
  {$Else}
   if (fchave <> '') then
     AJSon['chave'].AsString := fchave;
   if (fsolicitacaoPagador <> '') then
     AJSon['solicitacaoPagador'].AsString := fsolicitacaoPagador;
   finfoAdicionais.WriteToJSon(AJSon);
  {$EndIf}
end;

procedure TACBrPIXCobBase.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fchave := AJSon.S['chave'];
   fsolicitacaoPagador := AJSon.S['solicitacaoPagador'];
   finfoAdicionais.ReadFromJSon(AJSon);
  {$Else}
   fchave := AJSon['chave'].AsString;
   fsolicitacaoPagador := AJSon['solicitacaoPagador'].AsString;
   finfoAdicionais.ReadFromJSon(AJSon);
  {$EndIf}
end;

procedure TACBrPIXCobBase.SetChave(AValue: String);
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

procedure TACBrPIXCobBase.SetSolicitacaoPagador(AValue: String);
begin
  if fsolicitacaoPagador = AValue then
    Exit;
  fsolicitacaoPagador := copy(AValue, 1, 140);
end;

{ TACBrPIXCobSolicitada }

constructor TACBrPIXCobSolicitada.Create;
begin
  inherited;
  fcalendario := TACBrPIXCalendarioCobSolicitada.Create;
  fdevedor := TACBrPIXDevedor.Create;
  floc := TACBrPIXLocationCobSolicitada.Create;
  fvalor := TACBrPIXCobValor.Create;
  Clear;
end;

destructor TACBrPIXCobSolicitada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  floc.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobSolicitada.Clear;
begin
  inherited Clear;
  fcalendario.Clear;
  fdevedor.Clear;
  floc.Clear;
  fvalor.Clear;
end;

procedure TACBrPIXCobSolicitada.Assign(Source: TACBrPIXCobSolicitada);
begin
  inherited Assign(Source);
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  floc.Assign(Source.loc);
  fvalor.Assign(Source.valor);
end;

procedure TACBrPIXCobSolicitada.WriteToJSon(AJSon: TJsonObject);
begin
  inherited WriteToJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fcalendario.WriteToJSon(AJSon.O['calendario']);
   fdevedor.WriteToJSon(AJSon.O['devedor']);
   floc.WriteToJSon(AJSon.O['loc']);
   fvalor.WriteToJSon(AJSon.O['valor']);
  {$Else}
   fcalendario.WriteToJSon(AJSon['calendario'].AsObject);
   fdevedor.WriteToJSon(AJSon['devedor'].AsObject);
   floc.WriteToJSon(AJSon['loc'].AsObject);
   fvalor.WriteToJSon(AJSon['valor'].AsObject);
  {$EndIf}
end;

procedure TACBrPIXCobSolicitada.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  inherited ReadFromJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fcalendario.ReadFromJSon(AJSon.O['calendario']);
   fdevedor.ReadFromJSon(AJSon.O['devedor']);
   floc.ReadFromJSon(AJSon.O['loc']);
   fvalor.ReadFromJSon(AJSon.O['valor']);
  {$Else}
   fcalendario.ReadFromJSon(AJSon['calendario'].AsObject);
   fdevedor.ReadFromJSon(AJSon['devedor'].AsObject);
   floc.ReadFromJSon(AJSon['loc'].AsObject);
   fvalor.ReadFromJSon(AJSon['valor'].AsObject);
  {$EndIf}
end;

end.

