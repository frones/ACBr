{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BPeDabpeESCPOS;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBPeDABPeESCPOS, ACBrBPeDABPeESCPOSReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBPeDABPeESCPOSReg', @ACBrBPeDABPeESCPOSReg.Register);
end;

initialization
  RegisterPackage('ACBr_BPeDabpeESCPOS', @Register);
end.
