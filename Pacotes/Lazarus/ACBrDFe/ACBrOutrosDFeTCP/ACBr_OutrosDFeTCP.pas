{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_OutrosDFeTCP;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrSedex, ACBrOutrosDFeTCPReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrOutrosDFeTCPReg', @ACBrOutrosDFeTCPReg.Register);
end;

initialization
  RegisterPackage('ACBr_OutrosDFeTCP', @Register);
end.
