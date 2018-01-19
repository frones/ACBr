{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Reinf;

interface

uses
  ACBrReinfClasses, ACBrReinfEventos, ACBrReinfEventosBase, ACBrReinfR1000, 
  ACBrReinfR1070, ACBrReinfR2010, ACBrReinfR2020, ACBrReinfR2060, 
  ACBrReinfR2098, ACBrReinfR2099, ACBrReinfR9000, ACBrReinfReg, 
  ACBrReinfRetEventos, ACBrReinfUtils, pcnConversaoReinf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrReinfReg', @ACBrReinfReg.Register);
end;

initialization
  RegisterPackage('ACBr_Reinf', @Register);
end.
