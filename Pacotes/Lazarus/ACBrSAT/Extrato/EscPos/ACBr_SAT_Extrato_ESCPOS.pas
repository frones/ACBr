{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SAT_Extrato_ESCPOS; 

interface

uses
   ACBrSATExtratoESCPOS, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ACBrSATExtratoESCPOS', @ACBrSATExtratoESCPOS.Register); 
end; 

initialization
  RegisterPackage('ACBr_SAT_Extrato_ESCPOS', @Register); 
end.
