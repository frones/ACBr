{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSeX;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFSeXReg, ACBrNFSeX, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeXReg', @ACBrNFSeXReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSeX', @Register);
end.
