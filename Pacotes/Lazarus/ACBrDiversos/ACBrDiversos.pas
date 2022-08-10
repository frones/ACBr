{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrDiversos;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBarCode, ACBrCalculadora, ACBrCMC7, ACBrDiversosReg, ACBrEnterTab, 
  ACBrExtenso, ACBrFala, ACBrGIF, ACBrTroco, ACBrValidador, AJBarcode, Gif1, 
  Gif3, LCalculadora, ACBrInStore, ACBrCargaBal, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrDiversosReg', @ACBrDiversosReg.Register);
end;

initialization
  RegisterPackage('ACBrDiversos', @Register);
end.
