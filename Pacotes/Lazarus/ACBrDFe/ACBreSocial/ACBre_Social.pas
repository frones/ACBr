{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBre_Social;

{$warn 5023 off : no warning about unused units}
interface

uses
 ACBreSocialConfiguracoes, ACBreSocialEventos, ACBreSocialLoteEventos, 
 ACBreSocialReg, ACBreSocialWebServices, pcesConversaoeSocial, pcesGerador, 
 pcesIniciais, pcesNaoPeriodicos, pcesPeriodicos, pcesS1000, pcesS1005, 
 pcesS1010, pcesS1020, pcesS1030, pcesS1035, pcesS1040, pcesS1050, pcesS1060, 
 pcesS1070, pcesS1080, pcesS1200, pcesS1202, pcesS1207, pcesS1210, pcesS1250, 
 pcesS1260, pcesS1270, pcesS1280, pcesS1295, pcesS1298, pcesS1299, pcesS1300, 
 pcesS2190, pcesS2200, pcesS2205, pcesS2206, pcesS2210, pcesS2220, pcesS2230, 
 pcesS2240, pcesS2250, pcesS2298, pcesS2299, pcesS2300, pcesS2306, pcesS2399, 
 pcesS2400, pcesS3000, pcesS5001, pcesTabelas, pcesRetConsultaIdentEvt, 
 pcesRetConsultaLote, pcesRetDownloadEvt, pcesRetEnvioLote, pcesRetornoClass, 
 pcesS2260, pcesS5002, pcesS5003, pcesS5011, pcesS5012, pcesS5013, 
 pcesConsultaIdentEvt, pcesS2500, pcesS1220, pcesS2501, pcesS3500, pcesS5501, 
 pcesS5503, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBreSocialReg', @ACBreSocialReg.Register);
end;

initialization
  RegisterPackage('ACBre_Social', @Register);
end.
