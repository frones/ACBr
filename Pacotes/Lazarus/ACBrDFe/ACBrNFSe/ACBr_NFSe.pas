{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFSe, ACBrNFSeConfiguracoes, ACBrNFSeNotasFiscais, ACBrNFSeReg, ACBrNFSeWebServices, 
  pnfsCancNfseResposta, pnfsConsSitLoteRpsResposta, pnfsConversao, pnfsEnvLoteRpsResposta, pnfsNFSe, 
  pnfsNFSeG, pnfsNFSeR, pnfsNFSeW, pnfsSubsNfseResposta, ACBrNFSeDANFSeClass, pnfsNFSeW_ABRASFv1, 
  pnfsNFSeW_ABRASFv2, pnfsNFSeW_EGoverneISS, pnfsNFSeW_EL, pnfsNFSeW_Elotech, pnfsNFSeW_Equiplano, 
  pnfsNFSeW_Infisc, pnfsNFSeW_ISSDSF, pnfsNFSeW_Giap, pnfsConsts, pnfsConsURLResposta, pnfsNFSeW_GeisWeb, 
  pnfsNFSeW_Siat, pnfsNFSeW_SigISS, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeReg', @ACBrNFSeReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSe', @Register);
end.
