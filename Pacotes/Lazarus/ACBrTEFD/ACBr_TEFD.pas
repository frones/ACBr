{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_TEFD;

interface

uses
  ACBrTEFD, ACBrTEFDClass, ACBrTEFDDial, ACBrTEFDDisc, ACBrTEFDHiper, 
  ACBrTEFDCliSiTef, ACBrTEFDVeSPague, ACBrTEFDBanese, ACBrTEFDCappta, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrTEFD', @ACBrTEFD.Register);
end;

initialization
  RegisterPackage('ACBr_TEFD', @Register);
end.
