{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrOpenSSL;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrEAD, OpenSSLExt, ACBrOpenSSLReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrOpenSSLReg', @ACBrOpenSSLReg.Register);
end;

initialization
  RegisterPackage('ACBrOpenSSL', @Register);
end.
