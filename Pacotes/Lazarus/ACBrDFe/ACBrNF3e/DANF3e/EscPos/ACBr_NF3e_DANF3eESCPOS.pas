{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NF3e_DANF3eESCPOS;

interface

uses
  ACBrNF3eDANF3eESCPOS, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNF3eDANF3eESCPOS', @ACBrNF3eDANF3eESCPOS.Register);
end;

initialization
  RegisterPackage('ACBr_NF3e_DANF3eESCPOS', @Register);
end.
