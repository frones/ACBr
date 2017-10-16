{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_ANe;

interface

uses
  ACBrANe, ACBrANeConfiguracoes, ACBrANeDocumentos, ACBrANeReg, pcaRetEnvANe, 
  pcaANe, pcaANeR, pcaANeW, pcaConversao, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrANeReg', @ACBrANeReg.Register);
end;

initialization
  RegisterPackage('ACBr_ANe', @Register);
end.
