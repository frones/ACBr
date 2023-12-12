{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSeXDanfseRL;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFSeXDANFSeRLReg, ACBrNFSeXDANFSeRL, ACBrNFSeXDANFSeRLClass, ACBrNFSeXDANFSeRLISSNet, 
  ACBrNFSeXDANFSeRLRetrato, ACBrNFSeXDANFSeRLSimplISS, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeXDANFSeRLReg', @ACBrNFSeXDANFSeRLReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSeXDanfseRL', @Register);
end.
