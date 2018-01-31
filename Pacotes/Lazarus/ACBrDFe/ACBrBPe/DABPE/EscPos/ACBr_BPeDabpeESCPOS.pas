{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BPeDabpeESCPOS;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrBPeDABPeESCPOS, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBPeDABPeESCPOS', @ACBrBPeDABPeESCPOS.Register);
end;

initialization
  RegisterPackage('ACBr_BPeDabpeESCPOS', @Register);
end.
