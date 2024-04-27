unit ACBrNFeTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFe;

type

  { ACBrNFeBaseTest }

  ACBrNFeBaseTest = class(TTestCase)
  private
    FACBrNFe1: TACBrNFe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
  end;

  


implementation

uses
  ACBrConsts, ACBrUtil.Strings;

{ ACBrNFeBaseTest }

procedure ACBrNFeBaseTest.SetUp;
begin
  inherited SetUp;

  FACBrNFe1 := TACBrNFe.Create(nil);
end;

procedure ACBrNFeBaseTest.TearDown;
begin
  FACBrNFe1.Free;

  inherited TearDown;
end;

procedure ACBrNFeBaseTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFe1.NotasFiscais.Count, 'Contador de NFeS Não é zero');
end;


initialization

  _RegisterTest('ACBrNFeTests', ACBrNFeBaseTest);

end.
