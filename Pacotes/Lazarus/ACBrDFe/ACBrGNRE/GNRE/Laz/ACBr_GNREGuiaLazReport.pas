{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_GNREGuiaLazReport;

interface

uses
  ACBrGNREGuiaLazReportDm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrGNREGuiaLazReportDm', @ACBrGNREGuiaLazReportDm.Register);
end;

initialization
  RegisterPackage('ACBr_GNREGuiaLazReport', @Register);
end.
