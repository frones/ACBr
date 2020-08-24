{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_ADRCST;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrADRCST, ACBrADRCST_Bloco0, ACBrADRCST_Bloco0_Class, ACBrADRCST_Bloco1, 
  ACBrADRCST_Bloco1_Class, ACBrADRCST_Bloco9, ACBrADRCST_Bloco9_Class, 
  ACBrADRCST_Blocos, ACBrADRCSTReg, ACBrADRCSTConversao, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrADRCSTReg', @ACBrADRCSTReg.Register);
end;

initialization
  RegisterPackage('ACBr_ADRCST', @Register);
end.
