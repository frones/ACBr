{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Integrador;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrIntegradorReg, ACBrIntegrador, ACBrIntegradorResposta, pcnVFPe, 
  pcnVFPeR, pcnVFPeW, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrIntegradorReg', @ACBrIntegradorReg.Register);
end;

initialization
  RegisterPackage('ACBr_Integrador', @Register);
end.
