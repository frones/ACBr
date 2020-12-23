{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SAT_Extrato_Fortes;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrSATExtratoFortesFr, ACBrSATExtratoFortesFrReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSATExtratoFortesFrReg', @ACBrSATExtratoFortesFrReg.Register
    );
end;

initialization
  RegisterPackage('ACBr_SAT_Extrato_Fortes', @Register);
end.
