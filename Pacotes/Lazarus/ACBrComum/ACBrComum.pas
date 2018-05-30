{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrComum;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBase, ACBrConsts, ACBrUtil, ACBrReg, ACBrAAC, ACBrPAFClass, 
  ACBrCompress, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrReg', @ACBrReg.Register);
end;

initialization
  RegisterPackage('ACBrComum', @Register);
end.
