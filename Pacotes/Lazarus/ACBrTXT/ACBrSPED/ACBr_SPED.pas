{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SPED;

interface

uses
  ACBrSped, ACBrSpedFiscal, ACBrSpedContabil, ACBrSpedPisCofins, 
  ACBrEPCImportar, ACBrSpedECF, ACBrEFDImportar, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSpedFiscal', @ACBrSpedFiscal.Register);
  RegisterUnit('ACBrSpedContabil', @ACBrSpedContabil.Register);
  RegisterUnit('ACBrSpedPisCofins', @ACBrSpedPisCofins.Register);
  RegisterUnit('ACBrEPCImportar', @ACBrEPCImportar.Register);
  RegisterUnit('ACBrSpedECF', @ACBrSpedECF.Register);
  RegisterUnit('ACBrEFDImportar', @ACBrEFDImportar.Register);
end;

initialization
  RegisterPackage('ACBr_SPED', @Register);
end.
