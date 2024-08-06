{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSeXDanfseFPDF;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBr.DANFSeX.FPDFA4Retrato, ACBrNFSeXDANFSeFPDFClass, 
  ACBrNFSeXDANFSeFPDFReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeXDANFSeFPDFReg', @ACBrNFSeXDANFSeFPDFReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSeXDanfseFPDF', @Register);
end.
