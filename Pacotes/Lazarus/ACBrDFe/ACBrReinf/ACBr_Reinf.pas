{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Reinf;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrReinfEventos, pcnReinfR1000, pcnReinfR1070, pcnReinfR2010, pcnReinfR2020, pcnReinfR2060, 
  pcnReinfR2098, pcnReinfR2099, pcnReinfR9000, ACBrReinfReg, pcnReinfRetEventos, pcnConversaoReinf, 
  pcnCommonReinf, pcnEventosReinf, pcnGeradorReinf, pcnReinfR2030, pcnReinfR2040, pcnReinfR2050, 
  pcnReinfR2070, pcnReinfR3010, pcnReinfR5001, pcnReinfR5011, pcnReinfRetConsulta, ACBrReinfLoteEventos, 
  pcnReinfConsulta, pcnReinfR1050, pcnReinfR4010, pcnReinfR4020, pcnReinfR4040, pcnReinfR4080, 
  pcnReinfR4099, pcnReinfR9001, pcnReinfR9005, pcnReinfRetConsulta_R9011, pcnReinfRetConsulta_R9015, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrReinfReg', @ACBrReinfReg.Register);
end;

initialization
  RegisterPackage('ACBr_Reinf', @Register);
end.
