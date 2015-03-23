{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PAF;

interface

uses
  ACBrPAFRegistros, ACBrPAF, ACBrPAF_A, ACBrPAF_A_Class, ACBrPAF_B, 
  ACBrPAF_B_Class, ACBrPAF_C, ACBrPAF_C_Class, ACBrPAF_D, ACBrPAF_D_Class, 
  ACBrPAF_E, ACBrPAF_E_Class, ACBrPAF_N, ACBrPAF_N_Class, ACBrPAF_P, 
  ACBrPAF_P_Class, ACBrPAF_R, ACBrPAF_R_Class, ACBrPAF_T, ACBrPAF_T_Class, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPAF', @ACBrPAF.Register);
end;

initialization
  RegisterPackage('ACBr_PAF', @Register);
end.
