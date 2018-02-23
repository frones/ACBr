{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Reinf;

interface

uses
  pcnReinfClasses, ACBrReinfEventos, ACBrReinfEventosBase, pcnReinfR1000, 
  pcnReinfR1070, pcnReinfR2010, pcnReinfR2020, pcnReinfR2060, 
  pcnReinfR2098, pcnReinfR2099, pcnReinfR9000, ACBrReinfReg, 
  pcnReinfRetEventos, pcnConversaoReinf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrReinfReg', @ACBrReinfReg.Register);
end;

initialization
  RegisterPackage('ACBr_Reinf', @Register);
end.
