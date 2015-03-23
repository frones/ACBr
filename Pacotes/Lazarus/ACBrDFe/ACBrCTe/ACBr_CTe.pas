{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_CTe;

interface

uses
  ACBrCTeConhecimentos, pcteRetInutCTe, ACBrCTe, ACBrCTeConfiguracoes, 
  ACBrCTeReg, ACBrCTeUtil, ACBrCTeWebServices, pcteCancCTe, pcteConsCad, 
  pcteConsSitCTe, pcteConsStatServ, pcteCTe, pcteCTeR, pcteCTeW, pcteInutCTe, 
  pcteRetCancCTe, pcteRetConsCad, pcteRetConsSitCTe, pcteRetConsStatServ, 
  pcteRetEnvCTe, pcteRetConsReciCTe, pcteProcCTe, pcteConsReciCTe, 
  ACBrCTeDACTEClass, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrCTeReg', @ACBrCTeReg.Register);
end;

initialization
  RegisterPackage('ACBr_CTe', @Register);
end.
