{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSe_DanfseRL;

interface

uses
  ACBrNFSeDANFSeRL, ACBrNFSeDANFSeRLClass, ACBrNFSeDANFSeRLReg, 
  ACBrNFSeDANFSeRLRetrato, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeDANFSeRLReg', @ACBrNFSeDANFSeRLReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSe_DanfseRL', @Register);
end.
