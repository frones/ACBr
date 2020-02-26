{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_LFD;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrLFD3505, ACBrLFD, ACBrLFDBlocos, ACBrLFDBloco_0, ACBrLFDBloco_0_Class, 
  ACBrLFDBloco_8, ACBrLFDBloco_8_Class, ACBrLFDBloco_9, ACBrLFDBloco_9_Class, 
  ACBrLFDBloco_A, ACBrLFDBloco_A_Class, ACBrLFDBloco_B, ACBrLFDBloco_B_Class, 
  ACBrLFDBloco_C, ACBrLFDBloco_C_Class, ACBrLFDBloco_D, ACBrLFDBloco_D_Class, 
  ACBrLFDBloco_E, ACBrLFDBloco_E_Class, ACBrLFDBloco_H, ACBrLFDBloco_H_Class, 
  ACBrLFDBloco_I, ACBrLFDBloco_I_Class, ACBrLFDBloco_J, ACBrLFDBloco_J_Class, 
  ACBrLFDBloco_K, ACBrLFDBloco_K_Class, ACBrLFDBloco_L, ACBrLFDBloco_L_Class, 
  ACBrLFDBloco_Z, ACBrLFDBloco_Z_Class, ACBrLFDReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrLFDReg', @ACBrLFDReg.Register);
end;

initialization
  RegisterPackage('ACBr_LFD', @Register);
end.
