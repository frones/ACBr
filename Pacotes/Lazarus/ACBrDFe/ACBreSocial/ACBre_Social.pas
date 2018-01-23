{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBre_Social;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBreSocialConfiguracoes, ACBreSocialEventos, ACBreSocialLoteEventos, 
  ACBreSocialReg, ACBreSocialWebServices, eSocial_Consts, eSocial_Conversao, 
  eSocial_Gerador, eSocial_Iniciais, eSocial_NaoPeriodicos, 
  eSocial_Periodicos, eSocial_S1000, eSocial_S1005, eSocial_S1010, 
  eSocial_S1020, eSocial_S1030, eSocial_S1035, eSocial_S1040, eSocial_S1050, 
  eSocial_S1060, eSocial_S1070, eSocial_S1080, eSocial_S1200, eSocial_S1202, 
  eSocial_S1207, eSocial_S1210, eSocial_S1220, eSocial_S1250, eSocial_S1260, 
  eSocial_S1270, eSocial_S1280, eSocial_S1295, eSocial_S1298, eSocial_S1299, 
  eSocial_S1300, eSocial_S2100, eSocial_S2190, eSocial_S2200, eSocial_S2205, 
  eSocial_S2206, eSocial_S2210, eSocial_S2220, eSocial_S2230, eSocial_S2240, 
  esocial_S2241, eSocial_S2250, eSocial_S2298, eSocial_S2299, eSocial_S2300, 
  eSocial_S2305, eSocial_S2306, eSocial_S2399, eSocial_S2400, eSocial_S3000, 
  eSocial_S4000, eSocial_S4999, eSocial_S5001, eSocial_Tabelas, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBreSocialReg', @ACBreSocialReg.Register);
end;

initialization
  RegisterPackage('ACBre_Social', @Register);
end.
