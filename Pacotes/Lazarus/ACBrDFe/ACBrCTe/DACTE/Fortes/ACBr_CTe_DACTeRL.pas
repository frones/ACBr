{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_CTe_DACTeRL;

interface

uses
  ACBrCTeDACTeRLReg, ACBrCTeDACTeRLClass, ACBrCTeDACTeRL, 
  ACBrCTeDACTeRLRetrato, ACBrCTeDACTeRLRetratoA5, ACBrCTeDAEventoRL, 
  ACBrCTeDAEventoRLRetrato, ACBrCTeDAInutRL, ACBrCTeDAInutRLRetrato, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrCTeDACTeRLReg', @ACBrCTeDACTeRLReg.Register);
end;

initialization
  RegisterPackage('ACBr_CTe_DACTeRL', @Register);
end.
