{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BoletoFC_Fortes; 

interface

uses
   ACBrBoletoFCFortesFr, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ACBrBoletoFCFortesFr', @ACBrBoletoFCFortesFr.Register); 
end; 

initialization
  RegisterPackage('ACBr_BoletoFC_Fortes', @Register); 
end.
