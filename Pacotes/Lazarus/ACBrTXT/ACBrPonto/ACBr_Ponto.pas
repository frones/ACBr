{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Ponto;

interface

uses
  ACBrPonto, ACBrPonto_ACJEF, ACBrPonto_ACJEF_Class, ACBrPonto_AFD, 
  ACBrPonto_AFD_Class, ACBrPonto_AFDT, ACBrPonto_AFDT_Class, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPonto', @ACBrPonto.Register);
end;

initialization
  RegisterPackage('ACBr_Ponto', @Register);
end.
