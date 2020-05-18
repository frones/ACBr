{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_TEFD;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrTEFD, ACBrTEFDClass, ACBrTEFDDial, ACBrTEFDDisc, ACBrTEFDHiper, 
  ACBrTEFDCliSiTef, ACBrTEFDVeSPague, ACBrTEFDBanese, ACBrTEFDCappta, 
  ACBrTEFDReg, ACBrPicpay, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrTEFDReg', @ACBrTEFDReg.Register);
end;

initialization
  RegisterPackage('ACBr_TEFD', @Register);
end.
