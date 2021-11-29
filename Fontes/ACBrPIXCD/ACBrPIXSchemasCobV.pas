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
  ACBrPIXBase, ACBrPIXSchemasCob, ACBrPIXSchemasDevedor, ACBrPIXSchemasLocation;

resourcestring
  sErroDescontoDataFixaLimit = 'Limite de descontoDataFixa atingido (3)';

type

  { TACBrPIXCobDataDeVencimento }

  TACBrPIXCobDataDeVencimento = class(TACBrPIXSchema)
  private
    fdataDeVencimento: TDateTime;
    fvalidadeAposVencimento: Integer;
    procedure SetDataDeVencimento(AValue: TDateTime);
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobDataDeVencimento);

    property dataDeVencimento: TDateTime read fdataDeVencimento write SetDataDeVencimento;
    property validadeAposVencimento: Integer read fvalidadeAposVencimento write fvalidadeAposVencimento;
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
    fMaxModalidade: Integer;
    fmodalidade: Integer;
    fvalorPerc: Currency;
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
    fcalendario: TACBrPIXCobDataDeVencimento;
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

    property calendario: TACBrPIXCobDataDeVencimento read fcalendario;
    property devedor: TACBrPIXDadosDevedor read fdevedor;
    property loc: TACBrPIXLocationCobSolicitada read floc;
    property valor: TACBrPIXCobVValor read fvalor;
  end;

implementation

uses
  DateUtils, Math,
  ACBrPIXUtil,
  ACBrUtil;

{ TACBrPIXCobDataDeVencimento }

constructor TACBrPIXCobDataDeVencimento.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXCobDataDeVencimento.Clear;
begin
  fdataDeVencimento := 0;
  fvalidadeAposVencimento := 0;
end;

function TACBrPIXCobDataDeVencimento.IsEmpty: Boolean;
begin
  Result := (fdataDeVencimento = 0) and
            (fvalidadeAposVencimento = 0);
end;

procedure TACBrPIXCobDataDeVencimento.Assign(Source: TACBrPIXCobDataDeVencimento
  );
begin
  fdataDeVencimento := Source.dataDeVencimento;
  fvalidadeAposVencimento := Source.validadeAposVencimento;
end;

procedure TACBrPIXCobDataDeVencimento.SetDataDeVencimento(AValue: TDateTime);
begin
  if fdataDeVencimento = AValue then
    Exit;
  fdataDeVencimento := DateOf(AValue);
end;

procedure TACBrPIXCobDataDeVencimento.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (dataDeVencimento <> 0) then
     AJSon.S['dataDeVencimento'] := FormatDateTime('yyyy-mm-dd', fdataDeVencimento)
   if (fvalidadeAposVencimento > 0) then
     AJSon.I['validadeAposVencimento'] := fvalidadeAposVencimento;
  {$Else}
   if (dataDeVencimento <> 0) then
     AJSon['dataDeVencimento'].AsString := FormatDateTime('yyyy-mm-dd', fdataDeVencimento);
   if (fvalidadeAposVencimento > 0) then
     AJSon['validadeAposVencimento'].AsInteger := fvalidadeAposVencimento;
  {$EndIf}
end;

procedure TACBrPIXCobDataDeVencimento.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fdataDeVencimento := StringToDateTimeDef(AJSon.S['dataDeVencimento'], 0, 'yyyy-mm-dd');
   fvalidadeAposVencimento := AJSon.I['validadeAposVencimento'];
  {$Else}
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

procedure TACBrPIXDescontoDataFixa.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['data'] := FormatDateTime('yyyy-mm-dd', fdata)
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
   fdata :=  StringToDateTimeDef(AJSon['data'].AsString, 0, 'yyyy-mm-dd');
   fvalorPerc := StringToFloatDef(AJSon['valorPerc'].AsString, 0);
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
  fabatimento.Clear;
  fdesconto.Clear;
  fjuros.Clear;
  fmulta.Clear;
end;

function TACBrPIXCobVValor.IsEmpty: Boolean;
begin
  Result := fabatimento.IsEmpty and
            fdesconto.IsEmpty and
            fjuros.IsEmpty and
            fmulta.IsEmpty;
end;

procedure TACBrPIXCobVValor.Assign(Source: TACBrPIXCobVValor);
begin
  fabatimento.Assign(Source.abatimento);
  fdesconto.Assign(Source.desconto);
  fjuros.Assign(Source.juros);
  fmulta.Assign(Source.multa);
end;

procedure TACBrPIXCobVValor.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   StringToFloatDef(AJSon.S['original'] := FormatarValorPIX(foriginal);
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
  fcalendario := TACBrPIXCobDataDeVencimento.Create('calendario');
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

end.

