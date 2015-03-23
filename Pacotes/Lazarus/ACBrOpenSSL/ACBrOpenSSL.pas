{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrOpenSSL;

interface

uses
  libxml2, libxmlsec, libxslt, ACBrEAD, OpenSSLExt, ACBrOpenSSLReg, libexslt, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrOpenSSLReg', @ACBrOpenSSLReg.Register);
end;

initialization
  RegisterPackage('ACBrOpenSSL', @Register);
end.
