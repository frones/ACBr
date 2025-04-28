{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_MDFe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrMDFe, ACBrMDFeConfiguracoes, ACBrMDFeManifestos, ACBrMDFeReg, 
  ACBrMDFeWebServices, pmdfeConversaoMDFe, pmdfeMDFeR, pmdfeMDFeW, 
  ACBrMDFeDAMDFeClass, ACBrMDFe.Classes, ACBrMDFe.ProcInfraSA, 
  ACBrMDFe.XmlReader, ACBrMDFe.XmlWriter, ACBrMDFe.ConsNaoEnc, 
  ACBrMDFe.RetConsNaoEnc, ACBrMDFe.ConsSit, ACBrMDFe.RetConsSit, 
  ACBrMDFe.EventoClass, ACBrMDFe.EnvEvento, ACBrMDFe.RetEnvEvento, 
  ACBrMDFe.Consts, ACBrMDFe.IniReader, ACBrMDFe.IniWriter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrMDFeReg', @ACBrMDFeReg.Register);
end;

initialization
  RegisterPackage('ACBr_MDFe', @Register);
end.
