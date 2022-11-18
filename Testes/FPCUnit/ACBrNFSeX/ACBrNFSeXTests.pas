unit ACBrNFSeXTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXBaseTest }

  ACBrNFSeXBaseTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
  end;

  


implementation

uses
  ACBrConsts, ACBrUtil.Strings;

{ ACBrNFSeXBaseTest }

procedure ACBrNFSeXBaseTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
end;

procedure ACBrNFSeXBaseTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXBaseTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFeS Não é zero');
end;


initialization

  _RegisterTest('ACBrNFSeXTests', ACBrNFSeXBaseTest);
  

end.
