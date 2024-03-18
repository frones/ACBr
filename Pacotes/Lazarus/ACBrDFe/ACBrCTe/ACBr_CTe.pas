{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_CTe;

interface

uses
  ACBrCTeConhecimentos, ACBrCTe, pcteConversaoCTe, ACBrCTeConfiguracoes, 
  ACBrCTeReg, ACBrCTeWebServices, pcteConsSitCTe, pcteCTe, pcteCTeR, pcteCTeW, 
  pcteEnvEventoCTe, pcteEventoCTe, pcteInutCTe, pcteLayoutTXT, pcteModeloCTe, 
  pcteProcCTe, ACBrCTeDACTEClass, pcteConsts, ACBrCTeXmlHandler, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrCTeReg', @ACBrCTeReg.Register);
end;

initialization
  RegisterPackage('ACBr_CTe', @Register);
end.
