{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFe_DanfeESCPOS;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFeDANFeESCPOS, ACBrNFeDANFeESCPOSReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFeDANFeESCPOSReg', @ACBrNFeDANFeESCPOSReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFe_DanfeESCPOS', @Register);
end.
