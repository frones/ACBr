{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SAT_Extrato_Fortes;

interface

uses
  ACBrSATExtratoFortesFr, ACBrSATExtratoReportClass, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSATExtratoFortesFr', @ACBrSATExtratoFortesFr.Register);
end;

initialization
  RegisterPackage('ACBr_SAT_Extrato_Fortes', @Register);
end.
