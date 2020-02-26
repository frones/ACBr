{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit acbr_nfce_ecfvirtual;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrECFVirtualNFCe, ACBrECFVirtualNFCeReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrECFVirtualNFCeReg', @ACBrECFVirtualNFCeReg.Register);
end;

initialization
  RegisterPackage('acbr_nfce_ecfvirtual', @Register);
end.
