{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_MDFe;

interface

uses
  ACBrMDFe, ACBrMDFeConfiguracoes, ACBrMDFeManifestos, ACBrMDFeUtil, 
  ACBrMDFeWebServices, ACBrMDFeDAMDFEClass, ACBrMDFeReg, pmdfeSignature, 
  pmdfeConsReciMDFe, pmdfeConsSitMDFe, pmdfeConsStatServ, pmdfeConversao, 
  pmdfeEnvEventoMDFe, pmdfeMDFe, pmdfeMDFeR, pmdfeMDFeW, pmdfeProcMDFe, 
  pmdfeRetConsReciMDFe, pmdfeRetConsSitMDFe, pmdfeRetConsStatServ, 
  pmdfeRetEnvEventoMDFe, pmdfeRetEnvMDFe, pmdfeEventoMDFe, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrMDFeReg', @ACBrMDFeReg.Register);
end;

initialization
  RegisterPackage('ACBr_MDFe', @Register);
end.
