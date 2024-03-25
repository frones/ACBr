{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_TEFD;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrTEFDReg, ACBrTEFD, ACBrTEFDClass, ACBrTEFComum, ACBrTEFDDial, ACBrTEFDPayGo, ACBrTEFDPayGoWeb, 
  ACBrTEFDDisc, ACBrTEFDHiper, ACBrTEFDCliSiTef, ACBrTEFDVeSPague, ACBrTEFDBanese, ACBrTEFDCappta, 
  ACBrTEFCliSiTefComum, ACBrTEFPayGoComum, ACBrTEFPayGoWebComum, ACBrPOS, ACBrPOSPGWebAPI, ACBrPicpay, 
  ACBrTEFPayGoRedes, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFAPIPayGoWeb, ACBrTEFAPICliSiTef, 
  ACBrTEFDCliSiTefModular, ACBrTEFDDirecao, ACBrTEFDDialScopeGetcard, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrTEFDReg', @ACBrTEFDReg.Register);
end;

initialization
  RegisterPackage('ACBr_TEFD', @Register);
end.
