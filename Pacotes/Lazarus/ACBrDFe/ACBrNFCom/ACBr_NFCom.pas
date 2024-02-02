{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFCom;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFCom, ACBrNFComReg, ACBrNFComNotasFiscais, ACBrNFComConfiguracoes, 
  ACBrNFComWebServices, ACBrNFComDANFComClass, ACBrNFComClass, 
  ACBrNFComConsts, ACBrNFComConversao, ACBrNFComProc, ACBrNFComXmlReader, 
  ACBrNFComXmlWriter, ACBrNFComConsSit, ACBrNFComEnvEvento, 
  ACBrNFComEventoClass, ACBrNFComRetConsSit, ACBrNFComRetEnvEvento, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFComReg', @ACBrNFComReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFCom', @Register);
end.
