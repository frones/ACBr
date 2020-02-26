{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Convenio115;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrConvenio115, ACBrConvenio115Reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrConvenio115Reg', @ACBrConvenio115Reg.Register);
end;

initialization
  RegisterPackage('ACBr_Convenio115', @Register);
end.
