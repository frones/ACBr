{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_OFX;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrOFXReg, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ACBr_OFX', @Register);
end.
