{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_LCDPR;

{$warn 5023 off : no warning about unused units}
interface

uses
  UACBrLCDPR, ACBrLCDPRReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrLCDPRReg', @ACBrLCDPRReg.Register);
end;

initialization
  RegisterPackage('ACBr_LCDPR', @Register);
end.
