{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrSerial;

interface

uses
  ACBrSerialReg, ACBrDevice, ACBrBAL, ACBrBALClass, ACBrBALFilizola, 
  ACBrBALToledo, ACBrBALUrano, ACBrBALSaturno, ACBrCHQ, ACBrCHQBematech, 
  ACBrCHQChronos, ACBrCHQClass, ACBrCHQImpressoraComum, ACBrCHQImpressoraECF, 
  ACBrCHQPerto, ACBrCHQSchalter, ACBrCHQSotomaq, ACBrCHQUrano, ACBrDIS, 
  ACBrDISClass, ACBrDISGertecSerial, ACBrDISGertecTeclado, 
  ACBrDISKeytecTeclado, ACBrECF, ACBrECFClass, ACBrECFBematech, ACBrECFDaruma, 
  ACBrECFDataRegis, ACBrECFEpson, ACBrECFFiscNET, ACBrECFICash, ACBrECFMecaf, 
  ACBrECFNaoFiscal, ACBrECFNCR, ACBrECFQuattro, ACBrECFSchalter, ACBrECFSweda, 
  ACBrECFSwedaSTX, ACBrECFUrano, ACBrECFYanco, ACBrECFEscECF, ACBrETQ, 
  ACBrETQClass, ACBrETQPpla, ACBrETQZplII, ACBrGAV, ACBrGAVClass, 
  ACBrGAVImpressoraComum, ACBrGAVImpressoraECF, ACBrGAVSerialGerbo, 
  ACBrGAVSerialMenno, ACBrLCB, ACBrRFD, ACBrTER, ACBrTERClass, ACBrTERWilbor, 
  ACBrDISSmakTeclado, ACBrDISGertecTEC65lib, ACBrECFVirtual, 
  ACBrECFVirtualBuffer, ACBrECFVirtualPrinter, ACBrECFVirtualNaoFiscal, 
  ACBrPosPrinter, ACBrEscBematech, ACBrEscPosEpson, ACBrEscDaruma, 
  ACBrEscDiebold, ACBrEscElgin, ACBrEscEpsonP2, ACBrSMS, ACBrSMSClass, 
  ACBrSMSDaruma, ACBrSMSZTE, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSerialReg', @ACBrSerialReg.Register);
end;

initialization
  RegisterPackage('ACBrSerial', @Register);
end.
