{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_CIOT;

interface

uses
  ACBrCIOT, ACBrCIOTConfiguracoes, ACBrCIOTContratos, ACBrCIOTReg, pcaRetEnvANe, 
  pcnCIOT, pcnCIOTR, pcnCIOTW, pcnCIOTW_eFrete, pcnCIOTW_REPOM, pcnConversaoCIOT, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrCIOTReg', @ACBrCIOTReg.Register);
end;

initialization
  RegisterPackage('ACBr_CIOT', @Register);
end.
