{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSe;

interface

uses
  ACBrNFSe, ACBrNFSeConfiguracoes, ACBrNFSeNotasFiscais, ACBrNFSeReg, 
  ACBrNFSeWebServices, pnfsCancNfseResposta, pnfsConsLoteRpsResposta, 
  pnfsConsNfseporRpsResposta, pnfsConsNfseResposta, 
  pnfsConsSitLoteRpsResposta, pnfsConversao, pnfsEnvLoteRpsResposta, 
  pnfsGerarNfseResposta, pnfsNFSe, pnfsNFSeG, pnfsNFSeR, pnfsNFSeW, 
  pnfsSignature, pnfsSubsNfseResposta, ACBrNFSeDANFSeClass, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeReg', @ACBrNFSeReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSe', @Register);
end.
