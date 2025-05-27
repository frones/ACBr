{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_MDFe_DAMDFeFPDF;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrMDFeDAMDFeFPDFReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrMDFeDAMDFeFPDFReg', @ACBrMDFeDAMDFeFPDFReg.Register);
end;

initialization
  RegisterPackage('ACBr_MDFe_DAMDFeFPDF', @Register);
end.
