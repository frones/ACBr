{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_OpenDelivery;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrOpenDeliveryReg, ACBrOpenDelivery, ACBrOpenDeliveryEvents, 
  ACBrOpenDeliveryException, ACBrOpenDeliveryHTTP, 
  ACBrOpenDeliveryHTTPSynapse, ACBrOpenDeliveryMarketPlace, 
  ACBrOpenDeliverySchema, ACBrOpenDeliverySchemaClasses, 
  ACBrOpenDeliveryWebService, pcnConversaoOD, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrOpenDeliveryReg', @ACBrOpenDeliveryReg.Register);
end;

initialization
  RegisterPackage('ACBr_OpenDelivery', @Register);
end.
