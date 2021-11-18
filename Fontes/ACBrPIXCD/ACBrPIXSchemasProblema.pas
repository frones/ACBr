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

unit ACBrPIXSchemasProblema;

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
  ACBrBase, ACBrPIXBase;

type

  { TACBrPIXViolacao }

  TACBrPIXViolacao = class
  private
    fpropriedade: String;
    frazao: String;
    fvalor: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXViolacao);

    property razao: String read frazao write frazao;
    property propriedade: String read fpropriedade write fpropriedade;
    property valor: String read fvalor write fvalor;
  end;

  { TACBrPIXViolacoes }

  TACBrPIXViolacoes = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TACBrPIXViolacao;
    procedure SetItem(Index: Integer; Value: TACBrPIXViolacao);
  public
    procedure Assign(Source: TACBrPIXViolacoes);
    Function Add(AViolacao: TACBrPIXViolacao): Integer;
    Procedure Insert(Index: Integer; AViolacao: TACBrPIXViolacao);
    function New: TACBrPIXViolacao;
    property Items[Index: Integer]: TACBrPIXViolacao read GetItem write SetItem; default;
  end;

  { TACBrPIXProblema }

  TACBrPIXProblema = class
  private
    fcorrelationId: String;
    fdetail: String;
    fstatus: Integer;
    ftitle: String;
    ftype_uri: String;
    fviolacoes: TACBrPIXViolacoes;
    function GetAsJSON: String;
    procedure SetAsJSON(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Assign(Source: TACBrPIXProblema);

    property type_uri: String read ftype_uri write ftype_uri;
    property title: String read ftitle write ftitle;
    property status: Integer read fstatus write fstatus;
    property detail: String read fdetail write fdetail;
    property correlationId: String read fcorrelationId write fcorrelationId;
    property violacoes: TACBrPIXViolacoes read fviolacoes;

    property AsJSON: String read GetAsJSON write SetAsJSON;
  end;

implementation

uses
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr,
  {$Else}
   Jsons,
  {$EndIf}
  ACBrUtil;

{ TACBrPIXViolacao }

constructor TACBrPIXViolacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXViolacao.Clear;
begin
  fpropriedade := '';
  frazao := '';
  fvalor := '';
end;

procedure TACBrPIXViolacao.Assign(Source: TACBrPIXViolacao);
begin
  fpropriedade := Source.propriedade;
  frazao := Source.razao;
  fvalor := Source.valor;
end;

{ TACBrPIXViolacoes }

function TACBrPIXViolacoes.GetItem(Index: Integer): TACBrPIXViolacao;
begin
  Result := TACBrPIXViolacao(inherited Items[Index]);
end;

procedure TACBrPIXViolacoes.SetItem(Index: Integer; Value: TACBrPIXViolacao);
begin
  inherited Items[Index] := Value;
end;

procedure TACBrPIXViolacoes.Assign(Source: TACBrPIXViolacoes);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
    New.Assign(Source[i]);
end;

function TACBrPIXViolacoes.Add(AViolacao: TACBrPIXViolacao): Integer;
begin
  Result := inherited Add(AViolacao);
end;

procedure TACBrPIXViolacoes.Insert(Index: Integer; AViolacao: TACBrPIXViolacao);
begin
  inherited Insert(Index, AViolacao);
end;

function TACBrPIXViolacoes.New: TACBrPIXViolacao;
begin
  Result := TACBrPIXViolacao.Create;
  Self.Add(Result);
end;

{ TACBrPIXProblema }

constructor TACBrPIXProblema.Create;
begin
  inherited;
  fviolacoes := TACBrPIXViolacoes.Create();
  Clear;
end;

destructor TACBrPIXProblema.Destroy;
begin
  fviolacoes.Free;
  inherited Destroy;
end;

procedure TACBrPIXProblema.Clear;
begin
  fcorrelationId := '';
  fdetail := '';
  fstatus := 0;
  ftitle := '';
  ftype_uri := '';
  fviolacoes.Clear;
end;

procedure TACBrPIXProblema.Assign(Source: TACBrPIXProblema);
begin
  fcorrelationId := Source.correlationId;
  fdetail := Source.detail;
  fstatus := Source.status;
  ftitle := Source.title;
  ftype_uri := Source.type_uri;
  fviolacoes.Assign(Source.violacoes);
end;

function TACBrPIXProblema.GetAsJSON: String;
var
  jo: TJsonObject;
  vi: TACBrPIXViolacao;
  i: Integer;
begin
  jo := TJsonObject.Create();
  try
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     jo.S['type'] := ftype_uri;
     jo.S['title'] := ftitle;
     jo.I['status'] := fstatus;
     if (fdetail <> '') then
       jo.S['detail'] := fdetail;
     if (fcorrelationId <> '') then
       jo.S['correlationId'] := fcorrelationId;

     for i := 0 to fviolacoes.Count-1 do
     begin
       vi := fviolacoes[i];
       with jo.A['violacoes'].AddObject do
       begin
         if (vi.razao <> '') then
           S['razao'] := vi.razao;
         if (vi.propriedade <> '') then
           S['propriedade'] := vi.propriedade;
         if (vi.valor <> '') then
           S['valor'] := vi.valor;
       end;
     end;

     Result := jo.ToJSON();
    {$Else}
     jo['type'].AsString := ftype_uri;
     jo['title'].AsString := ftitle;
     jo['status'].AsInteger := fstatus;
     if (fdetail <> '') then
       jo['detail'].AsString := fdetail;
     if (fcorrelationId <> '') then
       jo['correlationId'].AsString := fcorrelationId;

     for i := 0 to fviolacoes.Count-1 do
     begin
       vi := fviolacoes[i];
       with jo['violacoes'].AsArray.Add.AsObject do
       begin
         if (vi.razao <> '') then
           Values['razao'].AsString := vi.razao;
         if (vi.propriedade <> '') then
           Values['propriedade'].AsString := vi.propriedade;
         if (vi.valor <> '') then
           Values['valor'].AsString := vi.valor;
       end;
     end;

     Result := jo.Stringify;
    {$EndIf}
  finally
    jo.Free;
  end;
end;

procedure TACBrPIXProblema.SetAsJSON(AValue: String);
var
  jo, jai: TJsonObject;
  ja: TJsonArray;
  i: Integer;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jo := TJsonObject.Parse(AValue) as TJsonObject;
   try
     ftype_uri := jo.S['type'];
     ftitle := jo.S['title'];
     fstatus := jo.I['status'];
     fdetail := jo.S['detail'];
     fcorrelationId := jo.S['correlationId'];
     ja := jo.A['violacoes'];
     for i := 0 to ja.Count-1 do
     begin
       jai := ja.O[i];
       with fviolacoes.New do
       begin
         razao := jai.S['razao'];
         propriedade := jai.S['propriedade'];
         valor := jai.S['valor'];
       end;
     end;
   finally
     jo.Free;
   end;
  {$Else}
   jo := TJsonObject.Create();
   try
     jo.Parse(AValue);

     ftype_uri := jo['type'].AsString;
     ftitle := jo['title'].AsString;
     fstatus := jo['status'].AsInteger;
     fdetail := jo['detail'].AsString;
     fcorrelationId := jo['correlationId'].AsString;
     ja := jo['violacoes'].AsArray;
     for i := 0 to ja.Count-1 do
     begin
       jai := ja[i].AsObject;
       with fviolacoes.New do
       begin
         razao := jai['razao'].AsString;
         propriedade := jai['propriedade'].AsString;
         valor := jai['valor'].AsString;
       end;
     end;
   finally
     jo.Free;
   end;
  {$EndIf}
end;

end.

