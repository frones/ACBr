unit acbrtxtclasstest2;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface

uses
  Classes,
  {$ifdef FPC}
  fpcunit, testutils, testregistry,
  {$else}
  TestFramework,
  {$endif}
  SysUtils, ACBrTXTClass;

type

  { TTACBrTXTClass_MetodosFill_Arredondamentos }

  TTACBrTXTClass_MetodosFill_Arredondamentos= class(TTestCase)
  private
    fACBrTXTClass: TACBrTXTClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LFillExtendedSeisCasas_UmExtended_RetornaUm;
    procedure LFillExtendedSeisCasas_UmCurrency_RetornaUm;
    procedure LFillExtendedSeisCasas_UmDouble_RetornaUm;
    procedure LFillExtendedSeisCasas_UmInteger_RetornaUm;

    procedure LFillExtendedSeisCasas_UmMilesimoExtended_Retorno;
    procedure LFillExtendedSeisCasas_UmMilesimoCurrency_Retorno;
    procedure LFillExtendedSeisCasas_UmMilesimoDouble_Retorno;

    procedure LFillExtendedSeisCasas_UmCentesimodeMilesimoDouble_Retorno;
    procedure LFillExtendedSeisCasas_UmCentesimodeMilesimoExtended_Retorno;

    procedure LFillExtendedSeisCasas_UmMilionesimoDouble_Retorno;
    procedure LFillExtendedSeisCasas_UmMilionesimoExtended_Retorno;

    procedure LFillExtendedSeisCasas_UmBilionesimoDouble_Retorno;
    procedure LFillExtendedSeisCasas_UmBilionesimoExtended_Retorno;

    procedure LFillExtendedSeisCasas_0Ponto123456Extended_Retorno;
    procedure LFillExtendedSeisCasas_0Ponto123456Double_Retorno;

    procedure LFillExtendedSeisCasas_0Ponto48Currency_Retorno;
    procedure LFillExtendedSeisCasas_0Ponto48Double_Retorno;
    procedure LFillExtendedSeisCasas_0Ponto48Extended_Retorno;

    procedure LFillExtendedDuasCasas_2222Ponto88Extended_Retorno;
    procedure LFillExtendedDuasCasas_2222Ponto88Currency_Retorno;
    procedure LFillExtendedDuasCasas_2222Ponto88Double_Retorno;

    procedure LFillExtendedDuasCasas_4Ponto20Extended_Retorno;
    procedure LFillExtendedDuasCasas_4Ponto20Double_Retorno;
    procedure LFillExtendedDuasCasas_4Ponto20Currency_Retorno;

  end;

    { TTACBrTXTClass_MetodosFill_ValoresAltos }

    TTACBrTXTClass_MetodosFill_ValoresAltos = class(TTestCase)
    private
      fACBrTXTClass: TACBrTXTClass;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure LFillExtendedValorAltoSeisCasas_DozeCasas_2PontoDouble_Retorno;
      procedure LFillExtendedValorAltoSeteCasas_DozeCasas_2PontoDouble_Retorno;
      procedure LFillExtendedValorAltoOitoCasas_DozeCasas_2PontoDouble_Retorno;
      procedure LFillExtendedValorAltoNoveCasas_DozeCasas_2PontoDouble_Retorno;
      procedure LFillExtendedValorAltoDezCasas_DozeCasas_2PontoDouble_Retorno;
      procedure LFillExtendedValorAltoOnzeCasas_TrezeCasas_2PontoDouble_Retorno;
      procedure LFillExtendedValorAltoDozeCasas_QuatorzeCasas_2PontoDouble_Retorno;
    end;

implementation

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmExtended_RetornaUm;
var
  vValue: Extended;
begin
  vValue  := 1.00;
  CheckEquals('|0001000000', fACBrTXTClass.LFill(vValue,  10, 6, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmDouble_RetornaUm;
var
  vValue2: Double;
begin
  vValue2 := 1.00;
  CheckEquals('|0001000000', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmCurrency_RetornaUm;
var
  vValue3: Currency;
begin
  vValue3 := 1.00;
  CheckEquals('|0001000000', fACBrTXTClass.LFill(vValue3, 10, 6, True), 'Erro no Currency');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmInteger_RetornaUm;
var
  vValue4: Integer;
begin
  vValue4 := 1;
  CheckEquals('|0001000000', fACBrTXTClass.LFill(vValue4, 10, 6, True), 'Erro no Integer');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmMilesimoExtended_Retorno;
var
  vValue: Extended;
  vValue2: Double;
  vValue3: Currency;
begin
  //10^-3
  vValue  := 0.001;
  vValue2 := 0.001;
  vValue3 := 0.001;
  CheckEquals('|0000001000', fACBrTXTClass.LFill(vValue,  10, 6, True), 'Erro no Extended');
  CheckEquals('|0000001000', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double');
  CheckEquals('|0000001000', fACBrTXTClass.LFill(vValue3, 10, 6, True), 'Erro no Currency');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmMilesimoDouble_Retorno;
var
  vValue2: Double;
begin
  //10^-3
  vValue2 := 0.001;
  CheckEquals('|0000001000', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmMilesimoCurrency_Retorno;
var
  vValue3: Currency;
begin
  //10^-3
  vValue3 := 0.001;
  CheckEquals('|0000001000', fACBrTXTClass.LFill(vValue3, 10, 6, True), 'Erro no Currency');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmCentesimodeMilesimoDouble_Retorno;
var
  vValue2: Double;
begin
  //10^-5
  vValue2 := 0.00001;
  CheckEquals('|0000000010', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmCentesimodeMilesimoExtended_Retorno;
var
  vValue: Extended;
begin
  //10^-5
  vValue  := 0.00001;
  CheckEquals('|0000000010', fACBrTXTClass.LFill(vValue,  10, 6, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmMilionesimoExtended_Retorno;
var
  vValue: Extended;
begin
  //10^-6
  vValue  := 0.000001;
  CheckEquals('|0000000001', fACBrTXTClass.LFill(vValue,  10, 6, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmMilionesimoDouble_Retorno;
var
  vValue2: Double;
begin
  //10^-6
  vValue2 := 0.000001;
  CheckEquals('|0000000001', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmBilionesimoDouble_Retorno;
var
  vValue2: Double;
begin
  //10^-9
  vValue2 := 0.000000001;
  CheckEquals('|0000000000', fACBrTXTClass.LFill(vValue2, 10, 6, False), 'Erro no Double, not Null');
  CheckEquals('|', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double, Null');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_UmBilionesimoExtended_Retorno;
var
  vValue: Extended;
begin
  //10^-9
  vValue  := 0.000000001;
  CheckEquals('|0000000000', fACBrTXTClass.LFill(vValue,  10, 6, False), 'Erro no Extended, not Null');
  CheckEquals('|', fACBrTXTClass.LFill(vValue, 10, 6, True), 'Erro no Extended, Null');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_0Ponto48Extended_Retorno;
var
  vValue: Extended;
begin
  vValue  := 0.48;
  CheckEquals('|0000480000', fACBrTXTClass.LFill(vValue,  10, 6, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_0Ponto48Double_Retorno;
var
  vValue2: Double;
begin
  vValue2 := 0.48;
  CheckEquals('|0000480000', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_0Ponto48Currency_Retorno;
var
  vValue3: Currency;
begin
  vValue3 := 0.48;
  CheckEquals('|0000480000', fACBrTXTClass.LFill(vValue3, 10, 6, True), 'Erro no Currency');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_0Ponto123456Extended_Retorno;
var
  vValue: Extended;
  //vValue3: Currency;
begin
  vValue  := 0.123456;
  CheckEquals('|0000123456', fACBrTXTClass.LFill(vValue, 10, 6, True), 'Erro no Double');
  //vValue3 := 0.123456; //Não é possível pois Currency só tem precisão de 4 casas decimais
  //CheckEquals('|0000123456', fACBrTXTClass.LFill(vValue3, 10, 6, True), 'Erro no Currency');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedSeisCasas_0Ponto123456Double_Retorno;
var
  vValue2: Double;
begin
  vValue2 := 0.123456;
  CheckEquals('|0000123456', fACBrTXTClass.LFill(vValue2, 10, 6, True), 'Erro no Double');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedDuasCasas_2222Ponto88Extended_Retorno;
var
  vValue: Extended;
begin
  vValue  := 2222.88;
  CheckEquals('|0000222288', fACBrTXTClass.LFill(vValue,  10, 2, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedDuasCasas_2222Ponto88Double_Retorno;
var
  vValue: Double;
begin
  vValue  := 2222.88;
  CheckEquals('|0000222288', fACBrTXTClass.LFill(vValue,  10, 2, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedDuasCasas_2222Ponto88Currency_Retorno;
var
  vValue: Currency;
begin
  vValue := 2222.88;
  CheckEquals('|0000222288', fACBrTXTClass.LFill(vValue, 10, 2, True), 'Erro no Currency');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedDuasCasas_4Ponto20Extended_Retorno;
var
  vValue: Extended;
begin
  vValue  := 4.20;
  CheckEquals('|0000000420', fACBrTXTClass.LFill(vValue,  10, 2, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedDuasCasas_4Ponto20Double_Retorno;
var
  vValue: Double;
begin
  vValue  := 4.20;
  CheckEquals('|0000000420', fACBrTXTClass.LFill(vValue,  10, 2, True), 'Erro no Extended');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.LFillExtendedDuasCasas_4Ponto20Currency_Retorno;
var
  vValue: Currency;
begin
  vValue := 4.20;
  CheckEquals('|0000000420', fACBrTXTClass.LFill(vValue, 10, 2, True), 'Erro no Currency');
end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.SetUp;
begin
  fACBrTXTClass := TACBrTXTClass.Create;
  fACBrTXTClass.Delimitador := '|';

end;

procedure TTACBrTXTClass_MetodosFill_Arredondamentos.TearDown;
begin
  FreeAndNil(fACBrTXTClass);
end;


{ TTACBrTXTClass_MetodosFill_ValoresAltos }

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.SetUp;
begin
  fACBrTXTClass := TACBrTXTClass.Create;
  fACBrTXTClass.Delimitador := '|';
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.TearDown;
begin
  FreeAndNil(fACBrTXTClass);
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.LFillExtendedValorAltoSeisCasas_DozeCasas_2PontoDouble_Retorno;
var
  vValue: Double;
begin
  vValue := 999999.99;
  CheckEquals('|000099999999', fACBrTXTClass.LFill(vValue, 12, 2, True), 'Erro com: '+FormatFloat('0.00',vValue));
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.LFillExtendedValorAltoSeteCasas_DozeCasas_2PontoDouble_Retorno;
var
  vValue: Double;
begin
  vValue := 9999999.99;
  CheckEquals('|000999999999', fACBrTXTClass.LFill(vValue, 12, 2, True), 'Erro com: '+FormatFloat('0.00',vValue));
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.LFillExtendedValorAltoOitoCasas_DozeCasas_2PontoDouble_Retorno;
var
  vValue: Double;
begin
  vValue := 99999999.99;
  CheckEquals('|009999999999', fACBrTXTClass.LFill(vValue, 12, 2, True), 'Erro com: '+FormatFloat('0.00',vValue));
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.LFillExtendedValorAltoNoveCasas_DozeCasas_2PontoDouble_Retorno;
var
  vValue: Double;
begin
  vValue := 999999999.99;
  CheckEquals('|099999999999', fACBrTXTClass.LFill(vValue, 12, 2, True), 'Erro com: '+FormatFloat('0.00',vValue));
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.LFillExtendedValorAltoDezCasas_DozeCasas_2PontoDouble_Retorno;
var
  vValue: Double;
begin
  vValue := 9999999999.99;
  CheckEquals('|999999999999', fACBrTXTClass.LFill(vValue, 12, 2, True), 'Erro com: '+FormatFloat('0.00',vValue));
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.LFillExtendedValorAltoOnzeCasas_TrezeCasas_2PontoDouble_Retorno;
var
  vValue: Double;
begin
  vValue := 99999999999.99;
  CheckEquals('|9999999999999', fACBrTXTClass.LFill(vValue, 13, 2, True), 'Erro com: '+FormatFloat('0.00',vValue));
end;

procedure TTACBrTXTClass_MetodosFill_ValoresAltos.LFillExtendedValorAltoDozeCasas_QuatorzeCasas_2PontoDouble_Retorno;
var
  vValue: Double;
begin
  vValue := 999999999999.99;
  CheckEquals('|99999999999999', fACBrTXTClass.LFill(vValue, 14, 2, True), 'Erro com: '+FormatFloat('0.00',vValue));
end;



initialization

  RegisterTest('ACBrComum.ACBrTXTClass', TTACBrTXTClass_MetodosFill_Arredondamentos{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrTXTClass', TTACBrTXTClass_MetodosFill_ValoresAltos{$ifndef FPC}.Suite{$endif});
end.

