{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSe;

interface

uses
  ACBrNFSe, ACBrNFSeConfiguracoes, ACBrNFSeDANFSeClass, ACBrNFSeNotasFiscais, 
  ACBrNFSeReg, ACBrNFSeWebServices, pnfsConversao, pnfsNFSe, pnfsNFSeW, 
  pnfsEnvLoteRpsResposta, pnfsNFSeR, pnfsConsSitLoteRpsResposta, 
  pnfsConsLoteRpsResposta, pnfsConsNfseporRpsResposta, pnfsConsNfseResposta, 
  pnfsSignature, pnfsCancNfseResposta, ACBrProvedorBetha, 
  ACBrProvedorDigifred, ACBrProvedorEquiplano, ACBrProvedorfintelISS, 
  ACBrProvedorGinfesV3, ACBrProvedorGovBR, ACBrProvedorISSIntel, 
  ACBrProvedorISSNet, ACBrProvedorProdemge, ACBrProvedorPublica, 
  ACBrProvedorRecife, ACBrProvedorRJ, ACBrProvedorSimplISS, ACBrProvedorThema, 
  ACBrProvedorTiplan, ACBrProvedorWebISS, ACBrProvedorBetim, 
  ACBrProvedorfISSLex, ACBrProvedorSaatri, pnfsGerarNfseResposta, 
  ACBrProvedorAbaco, ACBrProvedorGoiania, ACBrProvedorISSCuritiba, 
  ACBrProvedorBHISS, ACBrProvedorNatal, ACBrProvedorISSe, 
  ACBrProvedorISSDigital, ACBrProvedorSalvador, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeReg', @ACBrNFSeReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSe', @Register);
end.
