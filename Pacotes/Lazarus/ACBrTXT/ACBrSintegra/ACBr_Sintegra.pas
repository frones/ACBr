{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Sintegra ; 

interface

uses
  ACBrSintegra, ACBrSintegraReg, LazarusPackageIntf;

implementation

procedure Register ; 
begin
  RegisterUnit('ACBrSintegraReg', @ACBrSintegraReg.Register) ; 
end ; 

initialization
  RegisterPackage('ACBr_Sintegra', @Register) ; 
end.
