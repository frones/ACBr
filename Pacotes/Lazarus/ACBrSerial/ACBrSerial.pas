{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrSerial;

interface

uses
  ACBrCHQ, ACBrTERWilbor, ACBrBAL, ACBrBALClass, ACBrBALFilizola, 
  ACBrBALToledo, ACBrBALUrano, ACBrCHQBematech, ACBrCHQChronos, ACBrCHQClass, 
  ACBrCHQImpressoraComum, ACBrCHQImpressoraECF, ACBrCHQPerto, ACBrCHQSchalter, 
  ACBrCHQSotomaq, ACBrCHQUrano, ACBrDevice, ACBrDIS, ACBrDISClass, 
  ACBrDISGertecSerial, ACBrDISGertecTeclado, ACBrDISKeytecTeclado, ACBrECF, 
  ACBrECFBematech, ACBrECFClass, ACBrECFDaruma, ACBrECFDataRegis, 
  ACBrECFEpson, ACBrECFFiscNET, ACBrECFICash, ACBrECFMecaf, ACBrECFNaoFiscal, 
  ACBrECFNCR, ACBrECFQuattro, ACBrECFSchalter, ACBrECFSweda, ACBrECFSwedaSTX, 
  ACBrECFUrano, ACBrECFYanco, ACBrETQ, ACBrETQClass, ACBrETQPpla, ACBrGAV, 
  ACBrGAVClass, ACBrGAVImpressoraComum, ACBrGAVImpressoraECF, 
  ACBrGAVSerialGerbo, ACBrGAVSerialMenno, ACBrLCB, ACBrRFD, ACBrTER, 
  ACBrTERClass, ACBrSerialReg, ACBrETQZplII, ACBrDISSmakTeclado, 
  ACBrECFEscECF, ACBrECFVirtual, ACBrECFVirtualPrinter, 
  ACBrECFVirtualNaoFiscal, ACBrDISGertecTEC65lib, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSerialReg', @ACBrSerialReg.Register);
end;

initialization
  RegisterPackage('ACBrSerial', @Register);
end.
