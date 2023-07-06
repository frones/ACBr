{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrComum;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBase, ACBrConsts, ACBrUtil, ACBrReg, ACBrAAC, ACBrPAFClass, 
  ACBrCompress, ACBrImage, StrUtilsEx, ACBr_fpdf, ACBr_fpdf_ext, 
  ACBrDelphiZXingQRCode, ACBrUtil.FilesIO, ACBrUtil.Math, ACBrUtil.Strings, 
  ACBrUtil.XMLHTML, ACBrUtil.DateTime, ACBrUtil.Compatibilidade, 
  ACBrUtil.Base, Jsons, ACBrJSON, ACBrUtil.FPDF, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrReg', @ACBrReg.Register);
end;

initialization
  RegisterPackage('ACBrComum', @Register);
end.
