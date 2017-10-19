{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_SATWS;

interface

uses
  ACBrSATWS, pcnSATConsulta, pcnSATConsultaRet, ACBrSATWS_Reg, 
  ACBrSATWS_WebServices, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrSATWS_Reg', @ACBrSATWS_Reg.Register);
end;

initialization
  RegisterPackage('ACBr_SATWS', @Register);
end.
