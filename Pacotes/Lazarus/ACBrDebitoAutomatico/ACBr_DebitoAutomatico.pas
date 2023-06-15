{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_DebitoAutomatico;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrDebitoAutomatico, 
	ACBrDebitoAutomaticoArquivo, 
  ACBrDebitoAutomaticoArquivoClass, 
	ACBrDebitoAutomaticoConfiguracoes, 
  ACBrDebitoAutomaticoReg, 
	DebitoAutomatico.Santander.GravarTxtRemessa, 
  DebitoAutomatico.Santander.LerTxtRetorno, 
  DebitoAutomatico.Santander.Provider, 
	ACBrDebitoAutomaticoClass, 
  ACBrDebitoAutomaticoConversao, 
	ACBrDebitoAutomaticoParametros, 
  ACBrDebitoAutomaticoGravarTxt, 
	ACBrDebitoAutomaticoInterface, 
  ACBrDebitoAutomaticoLerTxt, 
	ACBrDebitoAutomaticoProviderBase, 
  ACBrDebitoAutomaticoProviderManager, 
	Febraban150.GravarTxtRemessa, 
  Febraban150.LerTxtRetorno, 
	Febraban150.Provider, 
	LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrDebitoAutomaticoReg', @ACBrDebitoAutomaticoReg.Register);
end;

initialization
  RegisterPackage('ACBr_DebitoAutomatico', @Register);
	
end.
