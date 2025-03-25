{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFe, ACBrNFeReg, ACBrNFeNotasFiscais, ACBrNFeConfiguracoes, 
  ACBrNFeWebServices, ACBrNFeDANFEClass, pcnLayoutTXT, pcnNFeR, pcnNFeRTXT, 
  pcnNFeW, pcnProcNFe, pcnConversaoNFe, ACBrNFe.Classes, ACBrDFeDANFeReport, 
  ACBrNFe.AdmCSC, ACBrNFe.ConsSit, ACBrNFe.EnvEvento, ACBrNFe.EventoClass, 
  ACBrNFe.Inut, ACBrNFe.RetAdmCSC, ACBrNFe.RetConsSit, ACBrNFe.RetEnvEvento, 
  ACBrNFe.RetInut, ACBrNFe.XmlReader, ACBrNFe.XmlWriter, ACBrNFe.Consts, 
  ACBrNFe.Conversao, ACBrNFe.IniReader, ACBrNFe.IniWriter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFeReg', @ACBrNFeReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFe', @Register);
end.
