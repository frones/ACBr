unit ACBrLibNFeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestACBrNFeLib= class(TTestCase)
  published
    procedure TestInicializar;
  end;

implementation

uses
  ACBrLibNFeStaticImport;

procedure TTestACBrNFeLib.TestInicializar;
begin
  AssertEquals(ErrOk, LIB_Inicializar('ACBrNFe.ini', ''));
end;

initialization
  RegisterTest(TTestACBrNFeLib);
end.

