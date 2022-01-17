unit ACBrTXTClassTest;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrTXTClass, ACBrTests.Util ;

type

  { TTACBrTXTClass_MetodosFill_CasosVazios }

  TTACBrTXTClass_MetodosFill_CasosVazios= class(TTestCase)
  private
    fACBrTXTClass: TACBrTXTClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure VDFill_VariantNula;
    procedure VLFill_VariantNula;
    procedure VDFill_VariantUndefined;
    procedure VLFill_VariantUndefined;
    procedure DFill_ExtendedZero_ResultadoVazio;
    procedure DFill_ExtendedZero_ResultadoNaoVazio;
    procedure LFill_TDateTimeZero_ResultadoVazio;
    procedure LFill_TDateTimeZero_ResultadoNaoVazio;
    procedure DFill_InteiroZero_ResultadoNaoVazio;
    procedure DFill_InteiroZero_ResultadoVazio;
    procedure LFill_StringZero_ResultadoVazio;
    procedure LFill_StringZero_ResultadoNaoVazio;
    procedure LFill_TrimString;
    procedure LFill_TrimString_TamanhoMaiorQueZero;
    procedure RFill_TrimString;
    procedure RFill_TrimString_TamanhoMaiorQueZero;
  end;

  { TTACBrTXTClass_MetodosVLFill_Numericos }

  TTACBrTXTClass_MetodosVLFill_Numericos = class(TTestCase)
  private
    fACBrTXTClass: TACBrTXTClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure VLFill_Inteiro;
    procedure VLFill_InteiroZerosEsquerdaSemDecimais;
  end;

implementation

uses variants;

{ TTACBrTXTClass_MetodosVLFill_Numericos }

procedure TTACBrTXTClass_MetodosVLFill_Numericos.SetUp;
begin
  fACBrTXTClass := TACBrTXTClass.Create;
  fACBrTXTClass.Delimitador := '|';
end;

procedure TTACBrTXTClass_MetodosVLFill_Numericos.TearDown;
begin
  FreeAndNil(fACBrTXTClass);
end;

procedure TTACBrTXTClass_MetodosVLFill_Numericos.VLFill_Inteiro;
var
  vValue: Variant;
begin
  vValue := 123;
  CheckEquals('|012300', fACBrTXTClass.VLFill(vValue, 6), 'Não tratou Variant Inteiro corretamente.');
end;

procedure TTACBrTXTClass_MetodosVLFill_Numericos.VLFill_InteiroZerosEsquerdaSemDecimais;
var
  vValue: Variant;
begin
  vValue := 0200;
  CheckEquals('|00200', fACBrTXTClass.VLFill(vValue, 5, 0), 'Não tratou Variant Inteiro com Zeros a esquerda corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.VDFill_VariantNula;
var
  vValue: Variant;
begin
  vValue := Null;
  CheckEquals('|', fACBrTXTClass.VDFill(vValue), 'Não tratou Variant Nula corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.VLFill_VariantNula;
var
  vValue: Variant;
begin
  vValue := Null;
  CheckEquals('|', fACBrTXTClass.VLFill(vValue, 4), 'Não tratou Variant Nula corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.VDFill_VariantUndefined;
var
  vValue: Variant;
begin
  //vValue := Undefined;
  CheckEquals('|', fACBrTXTClass.VDFill(vValue), 'Não tratou Variant "não definida" corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.VLFill_VariantUndefined;
var
  vValue: Variant;
begin
  //vValue := Undefined;
  CheckEquals('|', fACBrTXTClass.VLFill(vValue, 4), 'Não tratou Variant "não definida" corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.DFill_ExtendedZero_ResultadoVazio;
var
  vValue: Extended;
begin
  vValue := 0.00;
  CheckEquals('|', fACBrTXTClass.DFill(vValue, 4, True), 'Não tratou Double como Nulo corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.DFill_InteiroZero_ResultadoNaoVazio;
var
  vValue: Integer;
begin
  vValue := 0;
  CheckEquals('|'+FormatFloat('0.0000',0), fACBrTXTClass.DFill(vValue, 4, False), 'Tratou o Inteiro como Nula!!');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.DFill_InteiroZero_ResultadoVazio;
var
  vValue: Integer;
begin
  vValue := 0;
  CheckEquals('|', fACBrTXTClass.DFill(vValue, 4, True), 'Não tratou Inteiro como Nulo corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.DFill_ExtendedZero_ResultadoNaoVazio;
var
  vValue: Extended;
begin
  vValue := 0.00;
  CheckEquals('|'+FormatFloat('0.0000',0), fACBrTXTClass.DFill(vValue, 4, False), 'Tratou o Double como Nula!!');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.LFill_TDateTimeZero_ResultadoVazio;
var
  vValue: {$IFDEF FPC}TDate{$ELSE}TDateTime{$ENDIF};
  vValue2: TDateTime;
begin
  vValue  := 0;
  vValue2 := 0;
  CheckEquals('|', fACBrTXTClass.LFill(vValue,  'ddmmyyyy', True), 'Não tratou Date como Nulo corretamente.');
  CheckEquals('|', fACBrTXTClass.LFill(vValue2, 'ddmmyyyy', True), 'Não tratou DateTime como Nulo corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.LFill_TrimString;
var
  vValue: string;
begin
  vValue := ' Projeto ACBr ';
  fACBrTXTClass.TrimString := True;
  CheckEquals('|Projeto ACBr', fACBrTXTClass.LFill(vValue, 0, False), 'Não removeu espaços antes e depois da string');
  fACBrTXTClass.TrimString := False;
  CheckEquals('| Projeto ACBr ', fACBrTXTClass.LFill(vValue, 0, False), 'Removeu espaços antes e depois da string indevidamente');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.LFill_TrimString_TamanhoMaiorQueZero;
var
  vValue: string;
begin
  vValue := ' 123456 ';
  fACBrTXTClass.TrimString := True;
  CheckEquals('|0000123456', fACBrTXTClass.LFill(vValue, 10, False), 'Não removeu espaços antes e depois da string');
  fACBrTXTClass.TrimString := False;
  CheckEquals('|00 123456 ', fACBrTXTClass.LFill(vValue, 10, False), 'Removeu espaços antes e depois da string indevidamente');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.RFill_TrimString;
var
  vValue: string;
begin
  vValue := ' Projeto ACBr ';
  fACBrTXTClass.TrimString := True;
  CheckEquals('|Projeto ACBr', fACBrTXTClass.RFill(vValue, 0), 'Não removeu espaços antes e depois da string');
  fACBrTXTClass.TrimString := False;
  CheckEquals('| Projeto ACBr ', fACBrTXTClass.RFill(vValue, 0), 'Removeu espaços antes e depois da string indevidamente');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.RFill_TrimString_TamanhoMaiorQueZero;
var
  vValue: string;
begin
  vValue := ' 123456 ';
  fACBrTXTClass.TrimString := True;
  CheckEquals('|123456XXXX', fACBrTXTClass.RFill(vValue, 10, 'X'), 'Não removeu espaços antes e depois da string');
  fACBrTXTClass.TrimString := False;
  CheckEquals('| 123456 XX', fACBrTXTClass.RFill(vValue, 10, 'X'), 'Removeu espaços antes e depois da string indevidamente');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.LFill_TDateTimeZero_ResultadoNaoVazio;
var
  vValue: {$IFDEF FPC}TDate{$ELSE}TDateTime{$ENDIF};
  vValue2: TDateTime;
begin
  vValue  := 0;
  vValue2 := 0;
  CheckEquals('|30121899', fACBrTXTClass.LFill(vValue,  'ddmmyyyy', False), 'Tratou Date como nulo!!!');
  CheckEquals('|30121899', fACBrTXTClass.LFill(vValue2, 'ddmmyyyy', False), 'Tratou DateTime como nulo!!!');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.LFill_StringZero_ResultadoVazio;
var
  vValue: string;
begin
  vValue  := '';
  CheckEquals('|', fACBrTXTClass.LFill(vValue,  0, True), 'Não tratou string vazio como Nulo corretamente.');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.LFill_StringZero_ResultadoNaoVazio;
var
  vValue: string;
  vValue2: string;
begin
  vValue  := '';
  vValue2 := ' ';
  CheckEquals('|', fACBrTXTClass.LFill(vValue,  0, False), 'Tratou string vazio como nulo!!!');
  CheckEquals('| ', fACBrTXTClass.LFill(vValue2, 0, False), 'Tratou string com espaços como nulo!!!');
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.SetUp;
begin
  fACBrTXTClass := TACBrTXTClass.Create;
  fACBrTXTClass.Delimitador := '|';
end;

procedure TTACBrTXTClass_MetodosFill_CasosVazios.TearDown;
begin
  FreeAndNil(fACBrTXTClass);
end;

initialization
  _RegisterTest('ACBrComum.ACBrTXTClass', TTACBrTXTClass_MetodosFill_CasosVazios);
  _RegisterTest('ACBrComum.ACBrTXTClass', TTACBrTXTClass_MetodosVLFill_Numericos);

end.

