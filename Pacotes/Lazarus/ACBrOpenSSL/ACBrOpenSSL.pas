{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrOpenSSL;

interface

uses
  ACBrEAD, OpenSSLExt, ACBrOpenSSLReg, libxml2, libxmlsec, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrOpenSSLReg', @ACBrOpenSSLReg.Register);
end;

initialization
  RegisterPackage('ACBrOpenSSL', @Register);
end.
