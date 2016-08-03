{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_GNREGuiaRL;

interface

uses
  ACBrGNReGuiaRLClass, ACBrGNReGuiaRLReg, ACBrGNReGuiaRL, 
  ACBrGNReGuiaRLRetrato, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrGNReGuiaRLReg', @ACBrGNReGuiaRLReg.Register);
end;

initialization
  RegisterPackage('ACBr_GNREGuiaRL', @Register);
end.
