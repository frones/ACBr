{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_ONE;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrONE, ACBrONEConfiguracoes, ACBrONEWebServices, ACBrONE.Conversao, 
  ACBrONE.DistLeitura, ACBrONE.EnvManutencaoEQP, ACBrONE.EnvRecepcaoLeitura, 
  ACBrONE.Consts, ACBrONE.RetDistLeitura, ACBrONE.RetManutencaoEQP, 
  ACBrONE.RetRecepcaoLeitura, ACBrONEReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrONEReg', @ACBrONEReg.Register);
end;

initialization
  RegisterPackage('ACBr_ONE', @Register);
end.
