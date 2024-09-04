{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrBaaS;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBaaSReg, ACBrExtratoAPI, ACBrExtratoAPIBB, ACBrExtratoAPIInter, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBaaSReg', @ACBrBaaSReg.Register);
end;

initialization
  RegisterPackage('ACBrBaaS', @Register);
end.
