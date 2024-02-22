{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_CIOT;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrCIOT, ACBrCIOTConfiguracoes, ACBrCIOTContratos, ACBrCIOTReg, 
  pcnRetEnvCIOT, pcnCIOT, pcnCIOTR, pcnCIOTW, pcnCIOTW_eFrete, pcnCIOTW_REPOM, 
  ACBrCIOTConversao, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrCIOTReg', @ACBrCIOTReg.Register);
end;

initialization
  RegisterPackage('ACBr_CIOT', @Register);
end.
