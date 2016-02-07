{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_GNRE;

interface

uses
  ACBrGNRE2, ACBrGNREConfiguracoes, ACBrGNREGuias, ACBrGNREReg, 
  pgnreConsResLoteGNRE, ACBrGNREWebServices, pgnreConfigUF, pgnreConsConfigUF, 
  pgnreConversao, pgnreGNRE, pgnreGNRER, pgnreGNRERetorno, pgnreGNREW, 
  pgnreRetCampoAdicional, pgnreRetConsConfigUF, pgnreRetConsResLoteGNRE, 
  pgnreRetDetalhamentoReceita, pgnreRetEnvLoteGNRE, pgnreRetPeriodoApuracao, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrGNREReg', @ACBrGNREReg.Register);
end;

initialization
  RegisterPackage('ACBr_GNRE', @Register);
end.
