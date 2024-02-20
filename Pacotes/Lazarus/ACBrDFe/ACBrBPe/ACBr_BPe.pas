{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BPe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBPeClass, ACBrBPeXmlReader, ACBrBPeXmlWriter, ACBrBPeConsSit, 
  ACBrBPeConversao, pcnEnvEventoBPe, ACBrBPeEnvEvento, ACBrBPeRetConsSit, 
  ACBrBPeRetEnvEvento, ACBrBPe, ACBrBPeBilhetes, ACBrBPeConfiguracoes, 
  ACBrBPeReg, ACBrBPeWebServices, ACBrBPeDABPEClass, ACBrBPeConsts, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ACBr_BPe', @Register);
end.
