{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFe_DanfeESCPOS;

interface

uses
  ACBrNFeDANFeESCPOS, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFeDANFeESCPOS', @ACBrNFeDANFeESCPOS.Register);
end;

initialization
  RegisterPackage('ACBr_NFe_DanfeESCPOS', @Register);
end.
