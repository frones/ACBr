{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SAT_ECFVirtual;

interface

uses
  ACBrECFVirtualSAT, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrECFVirtualSAT', @ACBrECFVirtualSAT.Register);
end;

initialization
  RegisterPackage('ACBr_SAT_ECFVirtual', @Register);
end.
