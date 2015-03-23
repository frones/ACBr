{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_GNREGuiaRL;

interface

uses
  ACBrGNREGuiaFRFortes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrGNREGuiaFRFortes', @ACBrGNREGuiaFRFortes.Register);
end;

initialization
  RegisterPackage('ACBr_GNREGuiaRL', @Register);
end.
