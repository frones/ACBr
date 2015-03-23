{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Convenio115;

interface

uses
  ACBrConvenio115, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrConvenio115', @ACBrConvenio115.Register);
end;

initialization
  RegisterPackage('ACBr_Convenio115', @Register);
end.
