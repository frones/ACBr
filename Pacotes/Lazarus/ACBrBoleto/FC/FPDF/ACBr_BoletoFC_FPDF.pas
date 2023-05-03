{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BoletoFC_FPDF;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBoletoFPDFReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBoletoFPDFReg', @ACBrBoletoFPDFReg.Register);
end;

initialization
  RegisterPackage('ACBr_BoletoFC_FPDF', @Register);
end.
