{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_MTER;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrMTer, ACBrMTERReg, ACBrMTerClass, ACBrMTerPMTG, ACBrMTerStxEtx, 
  ACBrMTerVT100, ACBrMTerSB100, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrMTERReg', @ACBrMTERReg.Register);
end;

initialization
  RegisterPackage('ACBr_MTER', @Register);
end.
