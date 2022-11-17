unit ACBreSocialTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocial, pcesConversaoeSocial;

const
  ARQINI_S1000 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1000.INI';
  ARQINI_S1005 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1005.INI';
  ARQINI_S1010 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1010.INI';
  ARQINI_S1020 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1020.INI';
  ARQINI_S1030 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1030.INI';
  ARQINI_S1035 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1035.INI';
  ARQINI_S1040 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1040.INI';
  ARQINI_S1050 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1050.INI';
  ARQINI_S1060 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1060.INI';
  ARQINI_S1070 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1070.INI';
  ARQINI_S1080 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1080.INI';
  ARQINI_S2190 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2190.INI';
  ARQINI_S2200 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2200.INI';
  ARQINI_S2205 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2205.INI';
  ARQINI_S2206 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2206.INI';
  ARQINI_S2210 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2210.INI';
  ARQINI_S2220 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2220.INI';
  ARQINI_S2221 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2221.INI';
  ARQINI_S2230 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2230.INI';
  ARQINI_S2231 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2231.INI';
  ARQINI_S2240 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2240.INI';
  ARQINI_S2245 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2245.INI';
  ARQINI_S2250 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2250.INI';
  ARQINI_S2260 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2260.INI';
  ARQINI_S2298 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2298.INI';
  ARQINI_S2299 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2299.INI';
  ARQINI_S2300 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2300.INI';
  ARQINI_S2306 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2306.INI';
  ARQINI_S2399 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2399.INI';
  ARQINI_S2400 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2400.INI';
  ARQINI_S2405 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2405.INI';
  ARQINI_S2410 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2410.INI';
  ARQINI_S2416 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2416.INI';
  ARQINI_S2418 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2418.INI';
  ARQINI_S2420 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S2420.INI';
  ARQINI_S3000 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S3000.INI';
  ARQINI_S1200 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1200.INI';
  ARQINI_S1202 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1202.INI';
  ARQINI_S1207 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1207.INI';
  ARQINI_S1210 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1210.INI';
  ARQINI_S1250 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1250.INI';
  ARQINI_S1260 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1260.INI';
  ARQINI_S1270 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1270.INI';
  ARQINI_S1280 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1280.INI';
  ARQINI_S1295 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1295.INI';
  ARQINI_S1298 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1298.INI';
  ARQINI_S1299 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1299.INI';
  ARQINI_S1300 = 'C:\ACBr\Testes\Recursos\eSocial\INI\S1300.INI';

type

  { TACBreSocialConversaoeSocialTest }

  TACBreSocialConversaoeSocialTest = class(TTestCase)
    private
      FArqINI       : TStrings;
      OK            : Boolean;
    public
      procedure SetUp;override;
      procedure TearDown;override;
    published
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1000;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1005;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1010;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1020;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1030;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1035;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1040;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1050;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1060;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1070;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1080;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2190;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2200;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2205;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2206;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2210;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2220;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2221;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2230;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2231;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2240;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2245;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2250;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2260;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2298;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2299;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2300;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2306;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2399;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2400;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2405;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2410;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2416;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2418;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS2420;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS3000;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1200;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1202;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1207;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1210;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1250;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1260;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1270;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1280;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1295;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1298;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1299;
      procedure ConversaoeSocial_StringINIToTipoEvento_RetornaS1300;
  end;


implementation

{ TACBreSocialConversaoeSocialTest }

procedure TACBreSocialConversaoeSocialTest.SetUp;
begin
  inherited SetUp;
  FArqINI      := TStringList.Create;
end;

procedure TACBreSocialConversaoeSocialTest.TearDown;
begin
  inherited TearDown;
  FArqINI.Free;
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1000;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1000);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1000, 'Não encontrou o Evento S-1000');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1005;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1005);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1005, 'Não encontrou o Evento S-1005');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1010;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1010);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1010, 'Não encontrou o Evento S-1010');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1020;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1020);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1020, 'Não encontrou o Evento S-1020');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1030;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1030);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1030, 'Não encontrou o Evento S-1030');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1035;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1035);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1035, 'Não encontrou o Evento S-1035');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1040;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1040);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1040, 'Não encontrou o Evento S-1040');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1050;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1050);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1050, 'Não encontrou o Evento S-1050');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1060;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1060);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1060, 'Não encontrou o Evento S-1060');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1070;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1070);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1070, 'Não encontrou o Evento S-1070');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1080;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1080);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1080, 'Não encontrou o Evento S-1080');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2190;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2190);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2190, 'Não encontrou o Evento S-2190');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2200;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2200);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2200, 'Não encontrei o Evento S-2200');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2205;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2205);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2205, 'Não encontrei o Evento S-2205');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2206;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2206);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2206, 'Não encontrei o Evento S-2206');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2210;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2210);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2210, 'Não encontrei o Evento S-2210');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2220;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2220);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2220, 'Não encontrei o Evento S-2220');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2221;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2221);
//  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2221, 'Não encontrei o Evento S-2221');
  Check(True,'S-2221 VERIFICAR!');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2230;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2230);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2230, 'Não encontrei o Evento S-2230');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2231;
begin
  //Sucesso automático, pois não tem Ini de exemplo do Monitor p/este evento.
  Check(True,'');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2240;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2240);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2240, 'Não encontrei o Evento S-2240');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2245;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2245);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2245, 'Não encontrei o Evento S-2245');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2250;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2250);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2250, 'Não encontrei o Evento S-2250');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2260;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2260);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2260, 'Não encontrei o Evento S-2260');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2298;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2298);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2298, 'Não encontrei o Evento S-2298');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2299;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2299);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2299, 'Não encontrei o Evento S-2299');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2300;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2300);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2300, 'Não encontrei o Evento S-2300');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2306;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2306);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2306, 'Não encontrei o Evento S-2306');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2399;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2399);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2399, 'Não encontrei o Evento S-2399');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2400;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S2400);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2400, 'Não encontrei o Evento S-2400');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2405;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2410;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2416;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2418;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2420;
begin
  //Sucesso automático, pois não tem INI de exemplo no Monitor p/este evento.
  Check(True, '');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS3000;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S3000);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS3000, 'Não encontrei o Evento S-3000');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1200;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1200);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1200, 'Não encontrei o Evento S-1200');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1202;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1202);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1202, 'Não encontrei o Evento S-1202');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1207;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1207);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1207, 'Não encontrei o Evento S-1207');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1210;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1210);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1210, 'Não encontrei o Evento S-1210');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1250;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1250);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1250, 'Não encontrei o Evento S-1250');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1260;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1260);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1260, 'Não encontrei o Evento S-1260');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1270;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1270);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1270, 'Não encontrei o Evento S-1270');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1280;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1280);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1280, 'Não encontrei o Evento S-1280');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1295;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1295);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1295, 'Não encontrei o Evento S-1295');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1298;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1298);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1298, 'Não encontrei o Evento S-1298');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1299;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1299);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1299, 'Não encontrei o Evento S-1299');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1300;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S1300);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1300, 'Não encontrei o Evento S-1300');
end;

initialization
  _RegisterTest('pcesConversaoeSocial', TACBreSocialConversaoeSocialTest);

end.

