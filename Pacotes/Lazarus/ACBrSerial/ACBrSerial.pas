{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBrSerial;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrSerialReg, ACBrDevice, ACBrDeviceClass, ACBrDeviceSerial, ACBrDeviceTCP, ACBrDeviceLPT, ACBrDeviceRaw, 
  ACBrBAL, ACBrBALCapital, ACBrBALClass, ACBrBALFilizola, ACBrBALLenkeLK2500, ACBrBALSaturno, 
  ACBrBALSelfCheckout, ACBrBALToledo, ACBrBALUrano, ACBrBALSiciliano, ACBrCHQ, ACBrCHQBematech, 
  ACBrCHQChronos, ACBrCHQClass, ACBrCHQImpressoraComum, ACBrCHQImpressoraECF, ACBrCHQPerto, ACBrCHQSchalter, 
  ACBrCHQSotomaq, ACBrCHQUrano, ACBrDIS, ACBrDISClass, ACBrDISGertecSerial, ACBrDISGertecTeclado, 
  ACBrDISGertecTEC65lib, ACBrDISKeytecTeclado, ACBrDISSmakTecladoLib, ACBrDISSmakTeclado, ACBrDISSmakSerial, 
  ACBrECF, ACBrECFClass, ACBrECFBematech, ACBrECFDaruma, ACBrECFDataRegis, ACBrECFEpson, ACBrECFEscECF, 
  ACBrECFFiscNET, ACBrECFICash, ACBrECFMecaf, ACBrECFNaoFiscal, ACBrECFNCR, ACBrECFQuattro, ACBrECFSchalter, 
  ACBrECFSweda, ACBrECFSwedaSTX, ACBrECFUrano, ACBrECFVirtual, ACBrECFVirtualBuffer, 
  ACBrECFVirtualNaoFiscal, ACBrECFVirtualPrinter, ACBrECFYanco, ACBrEscBematech, ACBrEscCustomPos, 
  ACBrEscDaruma, ACBrEscDatecs, ACBrEscDiebold, ACBrEscElgin, ACBrEscEpsonP2, ACBrEscGPrinter, 
  ACBrEscPosEpson, ACBrEscPosStar, ACBrEscSunmi, ACBrEscZJiang, ACBrETQ, ACBrETQClass, ACBrETQEpl2, 
  ACBrETQEscLabel, ACBrETQPpla, ACBrETQZplII, ACBrGAV, ACBrGAVClass, ACBrGAVImpressoraComum, 
  ACBrGAVImpressoraECF, ACBrGAVSerialGerbo, ACBrGAVSerialMenno, ACBrLCB, ACBrPosPrinter, ACBrRFD, ACBrSMS, 
  ACBrSMSClass, ACBrSMSDaruma, ACBrSMSZTE, ACBrTER, ACBrTERClass, ACBrTERWilbor, ACBrSIN, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSerialReg', @ACBrSerialReg.Register);
end;

initialization
  RegisterPackage('ACBrSerial', @Register);
end.
