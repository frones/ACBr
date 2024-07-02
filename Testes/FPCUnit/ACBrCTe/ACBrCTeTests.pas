unit ACBrCTeTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrCTe;

type

  { ACBrCTeBaseTest }

  ACBrCTeBaseTest = class(TTestCase)
  private
    FACBrCTe1: TACBrCTe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
  end;

  


implementation

uses
  ACBrConsts, ACBrUtil.Strings;

{ ACBrCTeBaseTest }

procedure ACBrCTeBaseTest.SetUp;
begin
  inherited SetUp;

  FACBrCTe1 := TACBrCTe.Create(nil);
end;

procedure ACBrCTeBaseTest.TearDown;
begin
  FACBrCTe1.Free;

  inherited TearDown;
end;

procedure ACBrCTeBaseTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrCTe1.Conhecimentos.Count, 'Contador de CTe Não é zero');
end;


initialization

  _RegisterTest('ACBrCTeTests', ACBrCTeBaseTest);

end.
