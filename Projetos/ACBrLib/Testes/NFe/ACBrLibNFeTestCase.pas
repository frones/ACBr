unit ACBrLibNFeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrNFeLib }

  TTestACBrNFeLib = class(TTestCase)
  published
    procedure Test_LIB_Inicializar;
    procedure Test_LIB_Finalizar;
  end;

implementation

uses
  ACBrLibNFeStaticImport;

procedure TTestACBrNFeLib.Test_LIB_Inicializar;
begin
  AssertEquals(ErrOk, LIB_Inicializar('',''));
end;

procedure TTestACBrNFeLib.Test_LIB_Finalizar;
begin
  AssertEquals(ErrOk, LIB_Finalizar());
end;

initialization
  RegisterTest(TTestACBrNFeLib);
end.

