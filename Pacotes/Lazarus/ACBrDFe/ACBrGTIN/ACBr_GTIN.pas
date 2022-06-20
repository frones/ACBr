{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_GTIN;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrGTINReg, ACBrGTIN, ACBrGTINConfiguracoes, ACBrGTINWebServices, 
  ACBrGTINConsts, ACBrGTINConversao, ACBrGTINConsultar, ACBrGTINRetConsultar, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrGTINReg', @ACBrGTINReg.Register);
end;

initialization
  RegisterPackage('ACBr_GTIN', @Register);
end.
