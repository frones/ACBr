{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PAF;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrPAFRegistros, ACBrPAF, ACBrPAF_A, ACBrPAF_A_Class, ACBrPAF_B, 
  ACBrPAF_B_Class, ACBrPAF_C, ACBrPAF_C_Class, ACBrPAF_D, ACBrPAF_D_Class, 
  ACBrPAF_E, ACBrPAF_E_Class, ACBrPAF_N, ACBrPAF_N_Class, ACBrPAF_P, 
  ACBrPAF_P_Class, ACBrPAF_R, ACBrPAF_R_Class, ACBrPAF_T, ACBrPAF_T_Class, 
  ACBrPAF_F, ACBrPAF_F_Class, ACBrPAF_G, ACBrPAF_G_Class, ACBrPAF_H, 
  ACBrPAF_H_Class, ACBrPAF_L, ACBrPAF_L_Class, ACBrPAF_M, ACBrPAF_M_Class, 
  ACBrPAF_S, ACBrPAF_S_Class, ACBrPAF_TITP, ACBrPAF_TITP_Class, ACBrPAF_U, 
  ACBrPAF_U_Class, ACBrPAF_Z, ACBrPAF_Z_Class, ACBrPAF_V, ACBrPAF_V_Class, 
  ACBrPAF_J, ACBrPAF_J_Class, ACBrPAFReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPAFReg', @ACBrPAFReg.Register);
end;

initialization
  RegisterPackage('ACBr_PAF', @Register);
end.
