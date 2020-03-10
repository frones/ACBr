{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BoletoFC_Fortes;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBoletoFCFortesFr, ACBrBoletoFCFortesFrReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBoletoFCFortesFrReg', @ACBrBoletoFCFortesFrReg.Register);
end;

initialization
  RegisterPackage('ACBr_BoletoFC_Fortes', @Register);
end.
