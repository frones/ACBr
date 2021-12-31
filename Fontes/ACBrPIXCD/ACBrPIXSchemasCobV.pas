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

unit ACBrPIXSchemasCobV;

interface

uses
  Classes, SysUtils,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr,
  {$Else}
   Jsons,
  {$EndIf}
  ACBrPIXBase, ACBrPIXSchemasCob, ACBrPIXSchemasDevedor, ACBrPIXSchemasLocation,
  ACBrPIXSchemasCalendario, ACBrPIXSchemasPix;

resourcestring
  sErroDescontoDataFixaLimit = 'Limite de descontoDataFixa atingido (3)';

type

  { TACBrPIXCalendarioCobVBase }

  TACBrPIXCalendarioCobVBase = class(TACBrPIXSchema)
  private
    fcriacao: TDateTime;
    fdataDeVencimento: TDateTime;
    fvalidadeAposVencimento: Integer;
    procedure SetCriacao(AValue: TDateTime);
    procedure SetDataDeVencimento(AValue: TDateTime);
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;

    property criacao: TDateTime read fcriacao write SetCriacao;
    property dataDeVencimento: TDateTime read fdataDeVencimento write SetDataDeVencimento;
    property validadeAposVencimento: Integer read fvalidadeAposVencimento write fvalidadeAposVencimento;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCalendarioCobVBase);
  end;

  { TACBrPIXCalendarioCobVSolicitada }

  TACBrPIXCalendarioCobVSolicitada = class(TACBrPIXCalendarioCobVBase)
  public
    property dataDeVencimento;
    property validadeAposVencimento;
  end;


  { TACBrPIXCalendarioCobVGerada }

  TACBrPIXCalendarioCobVGerada = class(TACBrPIXCalendarioCobVSolicitada)
  public
    property criacao;
  end;

  { TACBrPIXModalidadeValor }

  TACBrPIXModalidadeValor = class(TACBrPIXSchema)
  private
    fMaxModalidade: Integer;
    fmodalidade: Integer;
    fvalorPerc: Currency;
    procedure SetModalidade(AValue: Integer);
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String; MaxModalidade: Integer); reintroduce;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXModalidadeValor);
    function IsEmpty: Boolean; override;

    property modalidade: Integer read fmodalidade write SetModalidade;
    property valorPerc: Currency read fvalorPerc write fvalorPerc;
  end;

  { TACBrPIXDescontoDataFixa }

  TACBrPIXDescontoDataFixa = class(TACBrPIXSchema)
  private
    fdata: TDateTime;
    fvalorPerc: Currency;
    procedure SetData(AValue: TDateTime);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXDescontoDataFixa);
    function IsEmpty: Boolean; override;

    property data: TDateTime read fdata write SetData;
    property valorPerc: Currency read fvalorPerc write fvalorPerc;
  end;

  { TACBrPIXDescontosDataFixa }

  TACBrPIXDescontosDataFixa = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXDescontoDataFixa;
    procedure SetItem(Index: Integer; Value: TACBrPIXDescontoDataFixa);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ADescontoDataFixa: TACBrPIXDescontoDataFixa): Integer;
    Procedure Insert(Index: Integer; ADescontoDataFixa: TACBrPIXDescontoDataFixa);
    function New: TACBrPIXDescontoDataFixa;
    property Items[Index: Integer]: TACBrPIXDescontoDataFixa read GetItem write SetItem; default;
  end;


  { TACBrPIXDesconto }

  TACBrPIXDesconto = class(TACBrPIXModalidadeValor)
  private
    fdescontoDataFixa: TACBrPIXDescontosDataFixa;
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String; MaxModalidade: Integer); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXDesconto);
    function IsEmpty: Boolean; override;

    property descontoDataFixa: TACBrPIXDescontosDataFixa read fdescontoDataFixa;
  end;

  { TACBrPIXCobVValor }

  TACBrPIXCobVValor = class(TACBrPIXSchema)
  private
    fabatimento: TACBrPIXModalidadeValor;
    fdesconto: TACBrPIXDesconto;
    fjuros: TACBrPIXModalidadeValor;
    fmulta: TACBrPIXModalidadeValor;
    foriginal: Currency;
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVValor);

    property original: Currency read foriginal write foriginal;
    property multa: TACBrPIXModalidadeValor read fmulta;
    property juros: TACBrPIXModalidadeValor read fjuros;
    property abatimento: TACBrPIXModalidadeValor read fabatimento;
    property desconto: TACBrPIXDesconto read fdesconto;
  end;

  { TACBrPIXCobVSolicitada }

  TACBrPIXCobVSolicitada = class(TACBrPIXCobBase)
  private
    fcalendario: TACBrPIXCalendarioCobVSolicitada;
    fdevedor: TACBrPIXDadosDevedor;
    floc: TACBrPIXLocationCobSolicitada;
    fvalor: TACBrPIXCobVValor;
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVSolicitada);

    property calendario: TACBrPIXCalendarioCobVSolicitada read fcalendario;
    property devedor: TACBrPIXDadosDevedor read fdevedor;
    property loc: TACBrPIXLocationCobSolicitada read floc;
    property valor: TACBrPIXCobVValor read fvalor;
  end;

  { TACBrPIXCobVRevisada }

  TACBrPIXCobVRevisada = class(TACBrPIXCobVSolicitada)
  private
    fstatus: TACBrPIXStatusCobranca;
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVRevisada);

    property status: TACBrPIXStatusCobranca read fstatus write fstatus;
  end;

  { TACBrPIXCobVGerada }

  TACBrPIXCobVGerada = class(TACBrPIXCobBaseCopiaCola)
  private
    fcalendario: TACBrPIXCalendarioCobVGerada;
    fdevedor: TACBrPIXDadosDevedor;
    floc: TACBrPIXLocationCompleta;
    frecebedor: TACBrPIXDadosRecebedor;
    frevisao: Integer;
    fstatus: TACBrPIXStatusCobranca;
    ftxId: String;
    fvalor: TACBrPIXCobVValor;
    procedure SetRevisao(AValue: Integer);
    procedure SetTxId(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVGerada);

    property calendario: TACBrPIXCalendarioCobVGerada read fcalendario;
    property txId: String read ftxId write SetTxId;
    property revisao: Integer read frevisao write SetRevisao;
    property devedor: TACBrPIXDadosDevedor read fdevedor;
    property recebedor: TACBrPIXDadosRecebedor read frecebedor;
    property loc: TACBrPIXLocationCompleta read floc;
    property status: TACBrPIXStatusCobranca read fstatus write fstatus;
    property valor: TACBrPIXCobVValor read fvalor;
  end;

  { TACBrPIXCobVCompleta }

  TACBrPIXCobVCompleta = class(TACBrPIXCobVGerada)
  private
    fpix: TACBrPIXArray;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    destructor Destroy; override;
    procedure Assign(Source: TACBrPIXCobVCompleta);

    property pix: TACBrPIXArray read fpix;
  end;


  { TACBrPIXCobVCompletaArray }

  TACBrPIXCobVCompletaArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobVCompleta;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobVCompleta);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ACobV: TACBrPIXCobVCompleta): Integer;
    Procedure Insert(Index: Integer; ACobV: TACBrPIXCobVCompleta);
    function New: TACBrPIXCobVCompleta;
    property Items[Index: Integer]: TACBrPIXCobVCompleta read GetItem write SetItem; default;
  end;

implementation

uses
  DateUtils, Math,
  ACBrPIXUtil,
  ACBrUtil;

{ TACBrPIXCalendarioCobVBase }

constructor TACBrPIXCalendarioCobVBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXCalendarioCobVBase.Clear;
begin
  fcriacao := 0;
  fdataDeVencimento := 0;
  fvalidadeAposVencimento := 0;
end;

function TACBrPIXCalendarioCobVBase.IsEmpty: Boolean;
begin
  Result := (fcriacao = 0) and
            (fdataDeVencimento = 0) and
            (fvalidadeAposVencimento = 0);
end;

procedure TACBrPIXCalendarioCobVBase.Assign(Source: TACBrPIXCalendarioCobVBase);
begin
  fcriacao := Source.criacao;
  fdataDeVencimento := Source.dataDeVencimento;
  fvalidadeAposVencimento := Source.validadeAposVencimento;
end;

procedure TACBrPIXCalendarioCobVBase.SetDataDeVencimento(AValue: TDateTime);
begin
  if fdataDeVencimento = AValue then
    Exit;
  fdataDeVencimento := DateOf(AValue);
end;

procedure TACBrPIXCalendarioCobVBase.SetCriacao(AValue: TDateTime);
begin
  if fcriacao = AValue then
    Exit;
  fcriacao := DateOf(AValue);
end;

procedure TACBrPIXCalendarioCobVBase.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (fcriacao <> 0) then
     AJSon.S['criacao'] := DateTimeToIso8601(fcriacao);
   if (fdataDeVencimento <> 0) then
     AJSon.S['dataDeVencimento'] := FormatDateTime('yyyy-mm-dd', fdataDeVencimento);
   if (fvalidadeAposVencimento > 0) then
     AJSon.I['validadeAposVencimento'] := fvalidadeAposVencimento;
  {$Else}
   if (fcriacao <> 0) then
     AJSon['criacao'].AsString := DateTimeToIso8601(fcriacao);
   if (fdataDeVencimento <> 0) then
     AJSon['dataDeVencimento'].AsString := FormatDateTime('yyyy-mm-dd', fdataDeVencimento);
   if (fvalidadeAposVencimento > 0) then
     AJSon['validadeAposVencimento'].AsInteger := fvalidadeAposVencimento;
  {$EndIf}
end;

procedure TACBrPIXCalendarioCobVBase.DoReadFromJSon(AJSon: TJsonObject);
var
  s: String;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   s := AJSon.S['criacao'];
   if (s <> '') then
     fcriacao := Iso8601ToDateTime(s);
   fdataDeVencimento := StringToDateTimeDef(AJSon.S['dataDeVencimento'], 0, 'yyyy-mm-dd');
   fvalidadeAposVencimento := AJSon.I['validadeAposVencimento'];
  {$Else}
   s := AJSon['criacao'].AsString;
   if (s <> '') then
     fcriacao := Iso8601ToDateTime(s);
   fdataDeVencimento := StringToDateTimeDef(AJSon['dataDeVencimento'].AsString, 0, 'yyyy-mm-dd');
   fvalidadeAposVencimento := AJSon['validadeAposVencimento'].AsInteger;
  {$EndIf}
end;

{ TACBrPIXModalidadeValor }

constructor TACBrPIXModalidadeValor.Create(const ObjectName: String;
  MaxModalidade: Integer);
begin
  inherited Create(ObjectName);
  fMaxModalidade := MaxModalidade;
  Clear;
end;

procedure TACBrPIXModalidadeValor.Clear;
begin
  fmodalidade := 1;
  fvalorPerc := 0;
end;

procedure TACBrPIXModalidadeValor.Assign(Source: TACBrPIXModalidadeValor);
begin
  fmodalidade := Source.modalidade;
  fvalorPerc := Source.valorPerc;
end;

function TACBrPIXModalidadeValor.IsEmpty: Boolean;
begin
  Result := (fvalorPerc = 0);
end;

procedure TACBrPIXModalidadeValor.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.I['modalidade'] := fmodalidade;
   if (fvalorPerc > 0) then
     AJSon.S['valorPerc'] := FormatarValorPIX(fvalorPerc);
  {$Else}
   AJSon['modalidade'].AsInteger := fmodalidade;
   if (fvalorPerc > 0) then
     AJSon['valorPerc'].AsString := FormatarValorPIX(fvalorPerc);
  {$EndIf}
end;

procedure TACBrPIXModalidadeValor.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fmodalidade := AJSon.I['modalidade'];
   fvalorPerc := StringToFloatDef(AJSon.S['valorPerc'], 0);
  {$Else}
   fmodalidade := AJSon['modalidade'].AsInteger;
   fvalorPerc := StringToFloatDef(AJSon['valorPerc'].AsString, 0);
  {$EndIf}
end;

procedure TACBrPIXModalidadeValor.SetModalidade(AValue: Integer);
begin
  if fmodalidade = AValue then
    Exit;
  fmodalidade := max(min(AValue,fMaxModalidade),1);
end;

{ TACBrPIXDescontoDataFixa }

constructor TACBrPIXDescontoDataFixa.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXDescontoDataFixa.Clear;
begin
  fdata := 0;
  fvalorPerc := 0;
end;

procedure TACBrPIXDescontoDataFixa.Assign(Source: TACBrPIXDescontoDataFixa);
begin
  fdata := Source.data;
  fvalorPerc := Source.valorPerc;
end;

function TACBrPIXDescontoDataFixa.IsEmpty: Boolean;
begin
  Result := (fdata = 0) or (fvalorPerc = 0);
end;

procedure TACBrPIXDescontoDataFixa.SetData(AValue: TDateTime);
begin
  if fdata = AValue then
    Exit;
  fdata := DateOf(AValue);
end;

procedure TACBrPIXDescontoDataFixa.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXDescontoDataFixa) then
    Assign(TACBrPIXDescontoDataFixa(ASource));
end;

procedure TACBrPIXDescontoDataFixa.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['data'] := FormatDateTime('yyyy-mm-dd', fdata);
   if (fvalorPerc > 0) then
     AJSon.S['valorPerc'] := FormatarValorPIX(fvalorPerc);
  {$Else}
   AJSon['data'].AsString := FormatDateTime('yyyy-mm-dd', fdata);
   if (fvalorPerc > 0) then
     AJSon['valorPerc'].AsString := FormatarValorPIX(fvalorPerc);
  {$EndIf}
end;

procedure TACBrPIXDescontoDataFixa.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fdata :=  StringToDateTimeDef(AJSon.S['data'], 0, 'yyyy-mm-dd');
   fvalorPerc := StringToFloatDef(AJSon.S['valorPerc'], 0);
  {$Else}
   fdata :=  StringToDateTimeDef(AJSon['data'].AsString, 0, 'yyyy-mm-dd');
   fvalorPerc := StringToFloatDef(AJSon['valorPerc'].AsString, 0);
  {$EndIf}
end;

{ TACBrPIXDescontosDataFixa }

function TACBrPIXDescontosDataFixa.GetItem(Index: Integer
  ): TACBrPIXDescontoDataFixa;
begin
  Result := TACBrPIXDescontoDataFixa(inherited Items[Index]);
end;

procedure TACBrPIXDescontosDataFixa.SetItem(Index: Integer;
  Value: TACBrPIXDescontoDataFixa);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXDescontosDataFixa.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXDescontosDataFixa.Add(ADescontoDataFixa: TACBrPIXDescontoDataFixa
  ): Integer;
begin
   if Count >= 3 then
     raise EACBrPixException.Create(ACBrStr(sErroDescontoDataFixaLimit));

  Result := inherited Add(ADescontoDataFixa);
end;

procedure TACBrPIXDescontosDataFixa.Insert(Index: Integer;
  ADescontoDataFixa: TACBrPIXDescontoDataFixa);
begin
  inherited Insert(Index, ADescontoDataFixa);
end;

function TACBrPIXDescontosDataFixa.New: TACBrPIXDescontoDataFixa;
begin
  Result := TACBrPIXDescontoDataFixa.Create('');
  Self.Add(Result);
end;


{ TACBrPIXDesconto }

constructor TACBrPIXDesconto.Create(const ObjectName: String;
  MaxModalidade: Integer);
begin
  fdescontoDataFixa := TACBrPIXDescontosDataFixa.Create('descontoDataFixa');
  inherited Create(ObjectName, MaxModalidade);
end;

destructor TACBrPIXDesconto.Destroy;
begin
  fdescontoDataFixa.Free;
  inherited Destroy;
end;

procedure TACBrPIXDesconto.Clear;
begin
  inherited Clear;
  fdescontoDataFixa.Clear;
end;

procedure TACBrPIXDesconto.Assign(Source: TACBrPIXDesconto);
begin
  inherited Assign(Source);
  fdescontoDataFixa.Assign(Source.descontoDataFixa);
end;

function TACBrPIXDesconto.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and fdescontoDataFixa.IsEmpty;
end;

procedure TACBrPIXDesconto.DoWriteToJSon(AJSon: TJsonObject);
begin
  inherited DoWriteToJSon(AJSon);
  fdescontoDataFixa.WriteToJSon(AJSon);
end;

procedure TACBrPIXDesconto.DoReadFromJSon(AJSon: TJsonObject);
begin
  inherited DoReadFromJSon(AJSon);
  fdescontoDataFixa.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVValor }

constructor TACBrPIXCobVValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fabatimento := TACBrPIXModalidadeValor.Create('abatimento',2);
  fdesconto := TACBrPIXDesconto.Create('desconto',6);
  fjuros := TACBrPIXModalidadeValor.Create('juros', 8);
  fmulta := TACBrPIXModalidadeValor.Create('multa', 2);
  Clear;
end;

destructor TACBrPIXCobVValor.Destroy;
begin
  fabatimento.Free;
  fdesconto.Free;
  fjuros.Free;
  fmulta.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVValor.Clear;
begin
  foriginal := 0;
  fabatimento.Clear;
  fdesconto.Clear;
  fjuros.Clear;
  fmulta.Clear;
end;

function TACBrPIXCobVValor.IsEmpty: Boolean;
begin
  Result := (foriginal = 0) and
            fabatimento.IsEmpty and
            fdesconto.IsEmpty and
            fjuros.IsEmpty and
            fmulta.IsEmpty;
end;

procedure TACBrPIXCobVValor.Assign(Source: TACBrPIXCobVValor);
begin
  foriginal := Source.original;
  fabatimento.Assign(Source.abatimento);
  fdesconto.Assign(Source.desconto);
  fjuros.Assign(Source.juros);
  fmulta.Assign(Source.multa);
end;

procedure TACBrPIXCobVValor.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['original'] := FormatarValorPIX(foriginal);
  {$Else}
   AJSon['original'].AsString := FormatarValorPIX(foriginal);
  {$EndIf}
  fabatimento.WriteToJSon(AJSon);
  fdesconto.WriteToJSon(AJSon);
  fjuros.WriteToJSon(AJSon);
  fmulta.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobVValor.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   foriginal := StringToFloatDef(AJSon.S['original'], 0);
  {$Else}
   foriginal := StringToFloatDef(AJSon['original'].AsString, 0);
  {$EndIf}
  fabatimento.ReadFromJSon(AJSon);
  fdesconto.ReadFromJSon(AJSon);
  fjuros.ReadFromJSon(AJSon);
  fmulta.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVSolicitada }

constructor TACBrPIXCobVSolicitada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcalendario := TACBrPIXCalendarioCobVSolicitada.Create('calendario');
  fdevedor := TACBrPIXDadosDevedor.Create('devedor');
  floc := TACBrPIXLocationCobSolicitada.Create('loc');
  fvalor := TACBrPIXCobVValor.Create('valor');
  Clear;
end;

destructor TACBrPIXCobVSolicitada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  floc.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVSolicitada.Clear;
begin
  inherited Clear;
  fcalendario.Clear;
  fdevedor.Clear;
  floc.Clear;
  fvalor.Clear;
end;

function TACBrPIXCobVSolicitada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fcalendario.IsEmpty and
            fdevedor.IsEmpty and
            floc.IsEmpty and
            fvalor.IsEmpty;
end;

procedure TACBrPIXCobVSolicitada.Assign(Source: TACBrPIXCobVSolicitada);
begin
  inherited Assign(Source);
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  floc.Assign(Source.loc);
  fvalor.Assign(Source.valor);
end;

procedure TACBrPIXCobVSolicitada.DoWriteToJSon(AJSon: TJsonObject);
begin
  inherited DoWriteToJSon(AJSon);
  fcalendario.WriteToJSon(AJSon);
  fdevedor.WriteToJSon(AJSon);
  floc.WriteToJSon(AJSon);
  fvalor.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobVSolicitada.DoReadFromJSon(AJSon: TJsonObject);
begin
  inherited DoReadFromJSon(AJSon);
  fcalendario.ReadFromJSon(AJSon);
  fdevedor.ReadFromJSon(AJSon);
  floc.ReadFromJSon(AJSon);
  fvalor.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVRevisada }

procedure TACBrPIXCobVRevisada.Clear;
begin
  fstatus := stcNENHUM;
  inherited Clear;
end;

function TACBrPIXCobVRevisada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (fstatus = stcNENHUM);
end;

procedure TACBrPIXCobVRevisada.Assign(Source: TACBrPIXCobVRevisada);
begin
  inherited Assign(Source);
  fstatus := Source.status;
end;

procedure TACBrPIXCobVRevisada.DoWriteToJSon(AJSon: TJsonObject);
begin
  inherited DoWriteToJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (fstatus <> stcNENHUM) then
     AJSon.S['status'] :=  PIXStatusCobrancaToString(fstatus);
  {$Else}
   if (fstatus <> stcNENHUM) then
     AJSon['status'].AsString :=  PIXStatusCobrancaToString(fstatus);
  {$EndIf}
end;

procedure TACBrPIXCobVRevisada.DoReadFromJSon(AJSon: TJsonObject);
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fstatus := StringToPIXStatusCobranca(AJSon.S['status']);
  {$Else}
   fstatus := StringToPIXStatusCobranca(AJSon['status'].AsString);
  {$EndIf}
end;

{ TACBrPIXCobVGerada }

constructor TACBrPIXCobVGerada.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcalendario := TACBrPIXCalendarioCobVGerada.Create('calendario');
  fdevedor := TACBrPIXDadosDevedor.Create('devedor');
  frecebedor := TACBrPIXDadosRecebedor.Create('recebedor');
  floc := TACBrPIXLocationCompleta.Create('loc');
  fvalor := TACBrPIXCobVValor.Create('valor');
  Clear;
end;

destructor TACBrPIXCobVGerada.Destroy;
begin
  fcalendario.Free;
  fdevedor.Free;
  frecebedor.Free;
  floc.Free;
  fvalor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVGerada.Clear;
begin
  inherited Clear;
  frevisao := 0;
  fstatus := stcNENHUM;
  ftxId := '';
  fcalendario.Clear;
  fdevedor.Clear;
  frecebedor.Clear;
  floc.Clear;
  fvalor.Clear;
end;

function TACBrPIXCobVGerada.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (frevisao = 0) and
            (fstatus = stcNENHUM) and
            (ftxId = '') and
            fcalendario.IsEmpty and
            fdevedor.IsEmpty and
            frecebedor.IsEmpty and
            floc.IsEmpty and
            fvalor.IsEmpty;
end;

procedure TACBrPIXCobVGerada.Assign(Source: TACBrPIXCobVGerada);
begin
  inherited Assign(Source);
  frevisao := Source.revisao;
  fstatus := Source.status;
  ftxId := Source.txId;
  fcalendario.Assign(Source.calendario);
  fdevedor.Assign(Source.devedor);
  frecebedor.Assign(Source.recebedor);
  floc.Assign(Source.loc);
  fvalor.Assign(Source.valor);
end;

procedure TACBrPIXCobVGerada.SetRevisao(AValue: Integer);
begin
  if frevisao = AValue then
    Exit;
  frevisao := max(AValue,0);
end;

procedure TACBrPIXCobVGerada.SetTxId(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') then
  begin
    e := ValidarTxId(s, 35, 26);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

procedure TACBrPIXCobVGerada.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fcalendario.WriteToJSon(AJSon);
   AJSon.S['txid'] := ftxId;
   AJSon.I['revisao'] := frevisao;
   fdevedor.WriteToJSon(AJSon);
   frecebedor.WriteToJSon(AJSon);
   floc.WriteToJSon(AJSon);
   AJSon.S['status'] := PIXStatusCobrancaToString(fstatus);
   fvalor.WriteToJSon(AJSon);
  {$Else}
   fcalendario.WriteToJSon(AJSon);
   AJSon['txid'].AsString := ftxId;
   AJSon['revisao'].AsInteger := frevisao;
   fdevedor.WriteToJSon(AJSon);
   frecebedor.WriteToJSon(AJSon);
   floc.WriteToJSon(AJSon);
   AJSon['status'].AsString := PIXStatusCobrancaToString(fstatus);
   fvalor.WriteToJSon(AJSon);
  {$EndIf}
  inherited DoWriteToJSon(AJSon);
end;

procedure TACBrPIXCobVGerada.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fcalendario.ReadFromJSon(AJSon);
   txId := AJSon.S['txid'];
   revisao := AJSon.I['revisao'];
   fdevedor.ReadFromJSon(AJSon);
   frecebedor.ReadFromJSon(AJSon);
   floc.ReadFromJSon(AJSon);
   fstatus := StringToPIXStatusCobranca(AJSon.S['status']);
   fvalor.ReadFromJSon(AJSon);
  {$Else}
   fcalendario.ReadFromJSon(AJSon);
   txId := AJSon['txid'].AsString;
   revisao := AJSon['revisao'].AsInteger;
   fdevedor.ReadFromJSon(AJSon);
   frecebedor.ReadFromJSon(AJSon);
   floc.ReadFromJSon(AJSon);
   fstatus := StringToPIXStatusCobranca(AJSon['status'].AsString);
   fvalor.ReadFromJSon(AJSon);
  {$EndIf}
  inherited DoReadFromJSon(AJSon);
end;


{ TACBrPIXCobVCompleta }

constructor TACBrPIXCobVCompleta.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fpix := TACBrPIXArray.Create('pix');
  Clear;
end;

destructor TACBrPIXCobVCompleta.Destroy;
begin
  fpix.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobVCompleta.Clear;
begin
  fpix.Clear;
  inherited Clear;
end;

function TACBrPIXCobVCompleta.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fpix.IsEmpty;
end;

procedure TACBrPIXCobVCompleta.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXCobVCompleta) then
    Assign(TACBrPIXCobVCompleta(ASource));
end;

procedure TACBrPIXCobVCompleta.Assign(Source: TACBrPIXCobVCompleta);
begin
  inherited Assign(Source);
  fpix.Assign(Source.pix);
end;

procedure TACBrPIXCobVCompleta.DoWriteToJSon(AJSon: TJsonObject);
begin
  inherited DoWriteToJSon(AJSon);
  fpix.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobVCompleta.DoReadFromJSon(AJSon: TJsonObject);
begin
  inherited DoReadFromJSon(AJSon);
  fpix.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVCompletaArray }

function TACBrPIXCobVCompletaArray.GetItem(Index: Integer
  ): TACBrPIXCobVCompleta;
begin
  Result := TACBrPIXCobVCompleta(inherited Items[Index]);
end;

procedure TACBrPIXCobVCompletaArray.SetItem(Index: Integer; Value: TACBrPIXCobVCompleta);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobVCompletaArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobVCompletaArray.Add(ACobV: TACBrPIXCobVCompleta): Integer;
begin
  Result := inherited Add(ACobV);
end;

procedure TACBrPIXCobVCompletaArray.Insert(Index: Integer; ACobV: TACBrPIXCobVCompleta);
begin
  inherited Insert(Index, ACobV);
end;

function TACBrPIXCobVCompletaArray.New: TACBrPIXCobVCompleta;
begin
  Result := TACBrPIXCobVCompleta.Create('');
  Self.Add(Result);
end;

end.

