{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SAT_ECFVirtual;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrECFVirtualSAT, ACBrECFVirtualSATReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrECFVirtualSATReg', @ACBrECFVirtualSATReg.Register);
end;

initialization
  RegisterPackage('ACBr_SAT_ECFVirtual', @Register);
end.
