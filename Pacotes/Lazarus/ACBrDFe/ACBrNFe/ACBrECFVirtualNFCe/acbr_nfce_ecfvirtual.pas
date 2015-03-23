{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit acbr_nfce_ecfvirtual;

interface

uses
  ACBrECFVirtualNFCe, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrECFVirtualNFCe', @ACBrECFVirtualNFCe.Register);
end;

initialization
  RegisterPackage('acbr_nfce_ecfvirtual', @Register);
end.
