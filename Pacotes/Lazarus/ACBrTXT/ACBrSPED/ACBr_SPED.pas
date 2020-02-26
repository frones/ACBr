{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SPED;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrSped, ACBrSpedFiscal, ACBrSpedContabil, ACBrSpedPisCofins, 
  ACBrEPCImportar, ACBrSpedECF, ACBrEFDImportar, ACBrSpedReg, 
  ACBrSpedImportarReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSpedReg', @ACBrSpedReg.Register);
  RegisterUnit('ACBrSpedImportarReg', @ACBrSpedImportarReg.Register);
end;

initialization
  RegisterPackage('ACBr_SPED', @Register);
end.
