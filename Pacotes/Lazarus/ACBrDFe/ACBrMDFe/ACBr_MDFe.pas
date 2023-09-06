{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_MDFe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrMDFe, ACBrMDFeConfiguracoes, ACBrMDFeManifestos, ACBrMDFeReg, 
  ACBrMDFeWebServices, pmdfeConsMDFeNaoEnc, pmdfeConsSitMDFe, 
  pmdfeConversaoMDFe, pmdfeEnvEventoMDFe, pmdfeEventoMDFe, pmdfeMDFe, 
  pmdfeMDFeR, pmdfeMDFeW, pmdfeProcMDFe, pmdfeRetConsMDFeNaoEnc, 
  pmdfeRetConsSitMDFe, pmdfeRetEnvEventoMDFe, pmdfeRetEnvMDFe, 
  ACBrMDFeDAMDFeClass, pmdfeConsts, pmdfeProcInfraSA, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrMDFeReg', @ACBrMDFeReg.Register);
end;

initialization
  RegisterPackage('ACBr_MDFe', @Register);
end.
