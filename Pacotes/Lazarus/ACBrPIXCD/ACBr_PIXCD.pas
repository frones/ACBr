{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PIXCD;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrPIXCDReg, ACBrPIXBase, ACBrPIXUtil, ACBrPIXCD, ACBrPIXBRCode, 
  ACBrPIXPSPAilos, ACBrPIXPSPBancoDoBrasil, ACBrPIXPSPBanrisul, 
  ACBrPIXPSPBradesco, ACBrPIXPSPCielo, ACBrPIXPSPGate2All, 
  ACBrPIXPSPGerenciaNet, ACBrPIXPSPInter, ACBrPIXPSPItau, ACBrPIXPSPMatera, 
  ACBrPIXPSPMercadoPago, ACBrPIXPSPPagSeguro, ACBrPIXPSPPixPDV, 
  ACBrPIXPSPSantander, ACBrPIXPSPShipay, ACBrPIXPSPSicoob, ACBrPIXPSPSicredi, 
  ACBrPIXSchemasProblema, ACBrPIXSchemasCalendario, ACBrPIXSchemasDevedor, 
  ACBrPIXSchemasLocation, ACBrPIXSchemasPaginacao, ACBrPIXSchemasPix, 
  ACBrPIXSchemasPixPDV, ACBrPIXSchemasDevolucao, 
  ACBrPIXSchemasParametrosConsultaPix, ACBrPIXSchemasPixConsultados, 
  ACBrPIXSchemasCob, ACBrPIXSchemasParametrosConsultaCob, 
  ACBrPIXSchemasCobsConsultadas, ACBrPIXSchemasCobV, 
  ACBrPIXSchemasCobsVConsultadas, ACBrPIXSchemasLoteCobV, 
  ACBrPIXSchemasParametrosConsultaLote, ACBrPIXSchemasLotesCobVConsultadas, 
  ACBrShipaySchemas, ACBrSchemasMatera, ACBrSchemasGate2All, ACBrPIXPSPC6Bank, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPIXCDReg', @ACBrPIXCDReg.Register);
end;

initialization
  RegisterPackage('ACBr_PIXCD', @Register);
end.
