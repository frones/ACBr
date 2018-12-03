{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFe_Danfe_LazReport;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFeDANFeLazReport, ACBrNFeDANFeLazReportReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFeDANFeLazReportReg', @ACBrNFeDANFeLazReportReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFe_Danfe_LazReport', @Register);
end.
