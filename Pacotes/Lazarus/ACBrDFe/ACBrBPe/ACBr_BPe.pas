{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BPe;

interface

uses
  pcnBPe, pcnBPeR, pcnBPeW, pcnConsSitBPe, pcnConversaoBPe, pcnEnvEventoBPe, pcnEventoBPe, 
  pcnProcBPe, pcnRetConsSitBPe, pcnRetEnvBPe, pcnRetEnvEventoBPe, ACBrBPe, ACBrBPeBilhetes, 
  ACBrBPeConfiguracoes, ACBrBPeReg, ACBrBPeWebServices, ACBrBPeDABPEClass, 
  pcnBPeConsts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBPeReg', @ACBrBPeReg.Register);
end;

initialization
  RegisterPackage('ACBr_BPe', @Register);
end.
