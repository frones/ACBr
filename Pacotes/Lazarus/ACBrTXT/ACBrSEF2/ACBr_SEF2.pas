{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SEF2;

interface

uses
  ACBrSEF2, ACBrSEF2Conversao, ACBrSEF2_Bloco0, ACBrSEF2_Bloco0_1, 
  ACBrSEF2_Bloco9, ACBrSEF2_eDoc_BlocoC, ACBrSEF2_eDoc_BlocoC_Class, 
  ACBrSEF2_BlocoE_1, ACBrSEF2_BlocoE, ACBrSEF2_BlocoH, ACBrSEF2_BlocoH_1, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSEF2', @ACBrSEF2.Register);
end;

initialization
  RegisterPackage('ACBr_SEF2', @Register);
end.
