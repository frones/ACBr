{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SAT_Extrato_FPDF;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrSATExtratoFPDF, ACBrSATExtratoFPDFReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSATExtratoFPDFReg', @ACBrSATExtratoFPDFReg.Register);
end;

initialization
  RegisterPackage('ACBr_SAT_Extrato_FPDF', @Register);
end.
