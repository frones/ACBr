{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFCe_DanfeRL;

interface

uses
  ACBrDANFCeFortesFr, ACBrDANFCeFortesFrA4, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrDANFCeFortesFr', @ACBrDANFCeFortesFr.Register);
  RegisterUnit('ACBrDANFCeFortesFrA4', @ACBrDANFCeFortesFrA4.Register);
end;

initialization
  RegisterPackage('ACBr_NFCe_DanfeRL', @Register);
end.
