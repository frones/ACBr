unit ACBrMDFeTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrMDFe;

type

  { ACBrMDFeBaseTest }

  ACBrMDFeBaseTest = class(TTestCase)
  private
    FACBrMDFe1: TACBrMDFe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
  end;

implementation

uses
  ACBrConsts, ACBrUtil.Strings;

{ ACBrMDFeBaseTest }

procedure ACBrMDFeBaseTest.SetUp;
begin
  inherited SetUp;

  FACBrMDFe1 := TACBrMDFe.Create(nil);
end;

procedure ACBrMDFeBaseTest.TearDown;
begin
  FACBrMDFe1.Free;

  inherited TearDown;
end;

procedure ACBrMDFeBaseTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrMDFe1.Manifestos.Count, 'Contador de MDFe Não é zero');
end;

initialization

  _RegisterTest('ACBrMDFeTests', ACBrMDFeBaseTest);

end.
