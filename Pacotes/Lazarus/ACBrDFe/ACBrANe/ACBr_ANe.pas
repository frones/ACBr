{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_ANe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrANe, ACBrANeConfiguracoes, ACBrANeDocumentos, ACBrANeReg, pcaRetEnvANe, 
  pcaANe, pcaANeR, pcaANeW, pcaConversao, ACBrANeInterface, ACBrANeParametros, 
  ACBrANe.Classes, ACBrANe.Consts, ACBrANe.Conversao, ACBrANe.ProviderBase, 
  ACBrANe.ProviderManager, ACBrANe.ProviderProprio, ACBrANe.WebServicesBase, 
  ACBrANe.WebServicesResponse, ATM.Provider, ELT.Provider, 
  PortoSeguro.Provider, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrANeReg', @ACBrANeReg.Register);
end;

initialization
  RegisterPackage('ACBr_ANe', @Register);
end.
