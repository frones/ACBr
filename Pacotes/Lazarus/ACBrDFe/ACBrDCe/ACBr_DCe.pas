{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_DCe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrDCe, ACBrDCeConfiguracoes, ACBrDCeDeclaracoes, ACBrDCeReg, 
  ACBrDCeWebServices, ACBrDCe.DACEClass, ACBrDCe.Classes, ACBrDCe.Consts, 
  ACBrDCe.Conversao, ACBrDCe.XmlReader, ACBrDCe.XmlWriter, ACBrDCe.ConsSit, 
  ACBrDCe.EnvEvento, ACBrDCe.EventoClass, ACBrDCe.RetConsSit, 
  ACBrDCe.RetEnvEvento, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrDCeReg', @ACBrDCeReg.Register);
end;

initialization
  RegisterPackage('ACBr_DCe', @Register);
end.
