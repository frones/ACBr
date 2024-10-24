{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFCom_DANFComRL;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFCom.DANFComRL, ACBrNFCom.DANFComRLClass, ACBrNFCom.DANFComRLReg, 
  ACBrNFCom.DANFComRLRetrato, ACBrNFCom.DAEventoRL, 
  ACBrNFCom.DAEventoRLRetrato, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFCom.DANFComRLReg', @ACBrNFCom.DANFComRLReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFCom_DANFComRL', @Register);
end.
