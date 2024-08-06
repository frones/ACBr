{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFe_DanfeFPDF;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFeDANFeFPDF, ACBrNFeDANFeFPDFReg, ACBrNFeUtilsFPDF, ACBrNFCeDANFeFPDF, 
  ACBrNFCeDANFeFPDFReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFeDANFeFPDFReg', @ACBrNFeDANFeFPDFReg.Register);
  RegisterUnit('ACBrNFCeDANFeFPDFReg', @ACBrNFCeDANFeFPDFReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFe_DanfeFPDF', @Register);
end.
