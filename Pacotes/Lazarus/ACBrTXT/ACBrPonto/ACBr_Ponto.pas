{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Ponto;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrPonto, ACBrPonto_ACJEF, ACBrPonto_ACJEF_Class, ACBrPonto_AFD, 
  ACBrPonto_AFD_Class, ACBrPonto_AFDT, ACBrPonto_AFDT_Class, ACBrPontoReg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPontoReg', @ACBrPontoReg.Register);
end;

initialization
  RegisterPackage('ACBr_Ponto', @Register);
end.
