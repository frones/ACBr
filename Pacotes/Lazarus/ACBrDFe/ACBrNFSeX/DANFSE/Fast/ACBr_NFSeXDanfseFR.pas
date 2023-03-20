{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NFSeXDANFSeFR;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNFSeXDANFSeFRReg, ACBrUtil.FR, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNFSeXDANFSeFRReg', @ACBrNFSeXDANFSeFRReg.Register);
end;

initialization
  RegisterPackage('ACBr_NFSeXDANFSeFR', @Register);
end.
