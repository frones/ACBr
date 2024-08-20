{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_CTe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrCTeConhecimentos, ACBrCTe, pcteConversaoCTe, ACBrCTeConfiguracoes, 
  ACBrCTeReg, ACBrCTeWebServices, pcteCTe, pcteCTeR, pcteCTeW, pcteInutCTe, 
  pcteProcCTe, ACBrCTeDACTEClass, pcteConsts, ACBrCTe.XmlHandler, 
  ACBrCTe.ConsSit, ACBrCTe.RetConsSit, ACBrCTe.EventoClass, ACBrCTe.EnvEvento, 
  ACBrCTe.RetEnvEvento, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrCTeReg', @ACBrCTeReg.Register);
end;

initialization
  RegisterPackage('ACBr_CTe', @Register);
end.
