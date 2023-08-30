{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PIXCD;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrPIXCDReg, ACBrPIXBase, ACBrPIXUtil, ACBrPIXCD, ACBrPIXBRCode, 
  ACBrPIXPSPItau, ACBrPIXPSPBancoDoBrasil, ACBrPIXPSPSantander, 
  ACBrPIXPSPShipay, ACBrPIXPSPSicredi, ACBrPIXPSPSicoob, 
  ACBrPIXSchemasProblema, ACBrPIXSchemasCalendario, ACBrPIXSchemasDevedor, 
  ACBrPIXSchemasLocation, ACBrPIXSchemasPaginacao, ACBrPIXSchemasDevolucao, 
  ACBrPIXSchemasPix, ACBrPIXSchemasParametrosConsultaPix, 
  ACBrPIXSchemasPixConsultados, ACBrPIXSchemasCob, 
  ACBrPIXSchemasParametrosConsultaCob, ACBrPIXSchemasCobsConsultadas, 
  ACBrPIXSchemasCobV, ACBrPIXSchemasCobsVConsultadas, ACBrPIXSchemasLoteCobV, 
  ACBrPIXSchemasParametrosConsultaLote, ACBrPIXSchemasLotesCobVConsultadas, 
  ACBrShipaySchemas, ACBrPIXPSPInter, ACBrPIXPSPPagSeguro, 
  ACBrPIXPSPGerenciaNet, ACBrPIXPSPBradesco, ACBrPIXPSPPixPDV, 
  ACBrPIXSchemasPixPDV, ACBrPIXPSPAilos, ACBrPIXPSPMatera, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPIXCDReg', @ACBrPIXCDReg.Register);
end;

initialization
  RegisterPackage('ACBr_PIXCD', @Register);
end.
