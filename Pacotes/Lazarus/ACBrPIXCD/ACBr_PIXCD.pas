{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PIXCD;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrPIXBase, ACBrPIXUtil, ACBrPIXQRCodeEstatico, ACBrPIXSchemasCob, 
  ACBrPIXSchemasProblema, ACBrPIXSchemasPix, ACBrPIXSchemasDevolucao, 
  ACBrPIXSchemasPaginacao, ACBrPIXSchemasParametrosConsultaPix, 
  ACBrPIXSchemasPixConsultados, ACBrPIXSchemasParametrosConsultaCob, 
  ACBrPIXSchemasCalendario, ACBrPIXSchemasDevedor, ACBrPIXSchemasLocation, 
  ACBrPIXSchemasCobsConsultadas, ACBrPIXSchemasCobV, 
  ACBrPIXSchemasCobsVConsultadas, ACBrPIXSchemasLoteCobV, 
  ACBrPIXSchemasParametrosConsultaLote, ACBrPIXSchemasLotesCobVConsultadas, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ACBr_PIXCD', @Register);
end.
