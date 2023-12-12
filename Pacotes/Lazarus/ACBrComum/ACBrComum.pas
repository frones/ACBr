{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrComum;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrReg, ACBrBase, ACBrConsts, ACBrUtil, ACBrUtil.Compatibilidade, ACBrUtil.Base, ACBrUtil.DateTime, 
  ACBrUtil.FilesIO, ACBrUtil.Math, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrAAC, ACBrCompress, ACBrImage, 
  ACBrPAFClass, StrUtilsEx, ACBrDelphiZXingQRCode, ACBr_fpdf, ACBr_fpdf_ext, ACBr_fpdf_report, Jsons, 
  ACBrJSON, ACBrUtil.FPDF, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrReg', @ACBrReg.Register);
end;

initialization
  RegisterPackage('ACBrComum', @Register);
end.
