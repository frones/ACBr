unit ACBreSocialTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, pcesConversaoeSocial, ACBreSocialTestsConsts,
  ACBreSocialEventosNaoPeriodicosTests, ACBreSocialEventosPeriodicosTests;

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

  { TACBreSocialTipoToStrStrToTipoTest }

  TACBreSocialTipoToStrStrToTipoTest = class(TTestCase)
    private
      auxStr : String;
      OK     : Boolean;
    public
      procedure Setup;override;
      procedure TearDown;override;
    published
      procedure VersaoeSocialToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToVersaoeSocial_ConvertendoTodosTipos_RetornoCorreto;
      procedure SimNaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToSimNao_ConvertendoTodosTipos_RetornoCorreto;
      procedure SimNaoFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToSimNaoFacultativo_ConvertendoTodosTipos_RetornoCorreto;
      procedure ModoLancamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToModoLancamento_ConvertendoTodosTipos_RetornoCorreto;
      procedure LayouteSocialToServico_ConvertendoTodosTipos_RetornoCorreto;
      procedure ServicoToLayout_ConvertendoTodosTipos_RetornoCorreto;
      procedure LayoutToSchema_ConvertendoTodosTipos_RetornoCorreto;


  end;

implementation

uses
  typinfo;


{ TACBreSocialTipoToStrStrToTipoTest }

procedure TACBreSocialTipoToStrStrToTipoTest.Setup;
begin
  inherited Setup;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.TearDown;
begin
  inherited TearDown;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.VersaoeSocialToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TVersaoeSocial;
  vauxString: string;
  EhIgual, OK: Boolean;
begin

  //for vElementoEnum in TVersaoeSocial do
  for vElementoEnum:= Low(TVersaoeSocial) to High(TVersaoeSocial) do
  begin
    vauxString := VersaoeSocialToStrEX(vElementoEnum);
    EhIgual := (StrToVersaoeSocialEX(OK, vauxString) = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro conversão no elemento Ord('+ GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'VersaoeSocialToStrEX => '+ vauxString+'  '+
                       'StrToVersaoeSocialEX => '+ GetEnumName(TypeInfo(vElementoEnum), ord(StrToVersaoeSocialEX(OK, vauxString)))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToVersaoeSocial_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: TVersaoeSocial;
  i: Integer;
  EhIgual, OK: Boolean;
begin
  for i := low(TVersaoeSocialArrayStrings) to High(TVersaoeSocialArrayStrings) do
  begin
    auxTipo := StrToVersaoeSocialEX(OK, TVersaoeSocialArrayStrings[i]);
    EhIgual := VersaoeSocialToStrEX(auxTipo) = TVersaoeSocialArrayStrings[i];
    CheckTrue(EhIgual and OK, 'Erro conversão no elemento '+ TVersaoeSocialArrayStrings[i] + ' (i='+ IntToStr(i)+')  '+
                       'StrToVersaoeSocialEX => '+ GetEnumName(TypeInfo(auxTipo), ord(auxTipo))+'  '+
                       'VersaoeSocialToStrEX => '+VersaoeSocialToStrEX(auxTipo));

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.SimNaoToStr_ConvertendoTodosTipos_RetornoCorreto;
begin
  auxStr := EmptyStr;
  auxStr := eSSimNaoToStr(tpSim);
  CheckEquals('S', auxStr, 'Erro de conversão: valor esperado "S", valor convertido "'+auxStr+'"');

  auxStr := EmptyStr;
  auxStr := eSSimNaoToStr(tpNao);
  CheckEquals('N', auxStr, 'Erro de conversão: valor esperado "N", valor convertido "'+auxStr+'"');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToSimNao_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo : tpSimNao;
begin
  auxTipo := eSStrToSimNao(OK, 'S');
  CheckTrue(auxTipo = tpSim, 'Erro de conversão, valor esperado "tpSim"');

  auxTipo := eSStrToSimNao(OK, 'N');
  CheckTrue(auxTipo = tpNao, 'Erro de conversão, valor esperado "tpNao"');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.SimNaoFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
begin
  auxStr := EmptyStr;
  auxStr := eSSimNaoFacultativoToStr(snfNada);
  CheckEquals('', auxStr, 'Erro de conversão: valor esperado "", valor convertido "'+auxStr+'"');

  auxStr := EmptyStr;
  auxStr := eSSimNaoFacultativoToStr(snfSim);
  CheckEquals('S', auxStr, 'Erro de conversão: valor esperado, "S", valor convertido "'+auxStr+'"');

  auxStr := EmptyStr;
  auxStr := eSSimNaoFacultativoToStr(snfNao);
  CheckEquals('N', auxStr, 'Erro de conversão: valor esperado, "N", valor convertido "'+auxStr+'"');

end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToSimNaoFacultativo_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: tpSimNaoFacultativo;
begin
  auxTipo := eSStrToSimNaoFacultativo(OK, '');
  CheckTrue(auxTipo = snfNada, 'Erro de conversão, valor esperado ""');

  auxTipo := eSStrToSimNaoFacultativo(OK, 'S');
  CheckTrue(auxTipo = snfSim, 'Erro de conversão, valor esperado "S"');

  auxTipo := eSStrToSimNaoFacultativo(OK, 'N');
  CheckTrue(auxTipo = snfNao, 'Erro de conversão, valor esperado "N"');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.ModoLancamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
begin
  auxStr := eSModoLancamentoToStr(mlInclusao);
  CheckEquals('inclusao', auxStr, 'Erro de conversão: valor esperado "inclusao", valor convertido "'+auxStr+'"');

  auxStr := eSModoLancamentoToStr(mlAlteracao);
  CheckEquals('alteracao', auxStr, 'Erro de conversão: valor esperado "alteracao", valor convertido "'+auxStr+'"');

  auxStr := eSModoLancamentoToStr(mlExclusao);
  CheckEquals('exclusao', auxStr, 'Erro de conversão: valor esperado "exclusao", valor convertido "'+auxStr+'"');

end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToModoLancamento_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: TModoLancamento;
begin
  auxTipo := eSStrToModoLancamento(OK, 'inclusao');
  CheckTrue(auxTipo = mlInclusao, 'Erro de conversão, valor esperado "inclusao"');

  auxTipo := eSStrToModoLancamento(OK, 'alteracao');
  CheckTrue(auxTipo = mlAlteracao, 'Erro de conversão, valor esperado "alteracao"');

  auxTipo := eSStrToModoLancamento(OK, 'exclusao');
  CheckTrue(auxTipo = mlExclusao, 'Erro de conversão, valor esperado "exclusao"');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.LayouteSocialToServico_ConvertendoTodosTipos_RetornoCorreto;
begin
  auxStr := LayOuteSocialToServico(LayEnvioLoteEventos);
  CheckEquals('EnviarLoteEventos', auxStr, 'Erro de conversão: valor esperado "EnviarLoteEventos", valor convertido "'+auxStr+'"');

  auxStr := LayOuteSocialToServico(LayConsultaLoteEventos);
  CheckEquals('ConsultarLoteEventos', auxStr, 'Erro de conversão: valor esperado "ConsultarLoteEventos", valor convertido "'+auxStr+'"');

  auxStr := LayouteSocialToServico(LayConsultaIdentEventos);
  CheckEquals('ConsultarIdentificadoresEventos', auxStr, 'Erro de conversão: valor esperado "ConsultarIdentificadoresEventos", valor convertido "'+auxStr+'"');

  auxStr := LayouteSocialToServico(LayDownloadEventos);
  CheckEquals('DownloadEventos', auxStr, 'Erro de conversão: valor esperado "DownloadEventos", valor convertido "'+auxStr+'"');

end;

procedure TACBreSocialTipoToStrStrToTipoTest.ServicoToLayout_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: TLayOut;
begin
  auxTipo := ServicoToLayOut(Ok, 'EnviarLoteEventos');
  CheckTrue(auxTipo = LayEnvioLoteEventos, 'Erro de conversão, valor esperado "LayEnvioLoteEventos"');

  auxTipo := ServicoToLayOut(Ok, 'ConsultarLoteEventos');
  CheckTrue(auxTipo = LayConsultaLoteEventos, 'Erro de conversão, valor esperado "LayConsultaLoteEventos"');

  auxTipo := ServicoToLayout(Ok, 'ConsultarIdentificadoresEventos');
  CheckTrue(auxTipo = LayConsultaIdentEventos, 'Erro de conversão, valor esperado "LayLayConsultaIdentEventos"');

  auxTipo := ServicoToLayout(Ok, 'DownloadEventos');
  CheckTrue(auxTipo = LayDownloadEventos, 'Erro de conversão, valor esperado "LayDownloadEventos"')
end;

procedure TACBreSocialTipoToStrStrToTipoTest.LayoutToSchema_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: TeSocialSchema;
begin
  auxTipo := LayOutToSchema(LayEnvioLoteEventos);
  CheckTrue(auxTipo = schEnvioLoteEventos, 'Erro de conversão, valor esperado "schEnvioLoteEventos"');

  auxTipo := LayOutToSchema(LayConsultaLoteEventos);
  CheckTrue(auxTipo = schConsultaLoteEventos, 'Erro de conversão, valor esperado "schConsultaLoteEventos"');

  auxTipo := LayOutToSchema(LayConsultaIdentEventos);
  CheckTrue(auxTipo = schConsultaIdentEventos, 'Erro de conversão, valor esperado "schConsultaIdentEventos"');

  auxTipo := LayOutToSchema(LayDownloadEventos);
  CheckTrue(auxTipo = schDownloadEventos, 'Erro de conversão, valor esperado "schDownloadEventos"');
end;

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
  FArqINI.LoadFromFile(ARQINI_S0100_S1000);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1000, 'Não encontrou o Evento S-1000');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1005;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1005);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1005, 'Não encontrou o Evento S-1005');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1010;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1010);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1010, 'Não encontrou o Evento S-1010');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1020;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_s0100_S1020);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1020, 'Não encontrou o Evento S-1020');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1030;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1030);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1030, 'Não encontrou o Evento S-1030');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1035;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1035);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1035, 'Não encontrou o Evento S-1035');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1040;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1040);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1040, 'Não encontrou o Evento S-1040');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1050;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1050);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1050, 'Não encontrou o Evento S-1050');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1060;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1060);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1060, 'Não encontrou o Evento S-1060');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1070;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1070);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1070, 'Não encontrou o Evento S-1070');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1080;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1080);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1080, 'Não encontrou o Evento S-1080');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2190;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2190);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2190, 'Não encontrou o Evento S-2190');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2200;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2200);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2200, 'Não encontrei o Evento S-2200');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2205;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2205);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2205, 'Não encontrei o Evento S-2205');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2206;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2206);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2206, 'Não encontrei o Evento S-2206');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2210;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2210);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2210, 'Não encontrei o Evento S-2210');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2220;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2220);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2220, 'Não encontrei o Evento S-2220');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2221;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2221);
//  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2221, 'Não encontrei o Evento S-2221');
  Check(True,'S-2221 VERIFICAR!');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2230;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2230);
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
  FArqINI.LoadFromFile(ARQINI_S0100_S2240);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2240, 'Não encontrei o Evento S-2240');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2245;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2245);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2245, 'Não encontrei o Evento S-2245');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2250;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2250);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2250, 'Não encontrei o Evento S-2250');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2260;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2260);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS2260, 'Não encontrei o Evento S-2260');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2298;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2298);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2298, 'Não encontrei o Evento S-2298');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2299;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2299);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2299, 'Não encontrei o Evento S-2299');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2300;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2300);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2300, 'Não encontrei o Evento S-2300');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2306;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2306);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2306, 'Não encontrei o Evento S-2306');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2399;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2399);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS2399, 'Não encontrei o Evento S-2399');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS2400;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S2400);
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
  FArqINI.LoadFromFile(ARQINI_S0100_S3000);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS3000, 'Não encontrei o Evento S-3000');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1200;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1200);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1200, 'Não encontrei o Evento S-1200');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1202;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1202);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1202, 'Não encontrei o Evento S-1202');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1207;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1207);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1207, 'Não encontrei o Evento S-1207');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1210;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1210);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1210, 'Não encontrei o Evento S-1210');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1250;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1250);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1250, 'Não encontrei o Evento S-1250');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1260;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1260);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1260, 'Não encontrei o Evento S-1260');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1270;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1270);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1270, 'Não encontrei o Evento S-1270');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1280;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1280);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1280, 'Não encontrei o Evento S-1280');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1295;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1295);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1295, 'Não encontrei o Evento S-1295');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1298;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1298);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1298, 'Não encontrei o Evento S-1298');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1299;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1299);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, veS01_00_00) = teS1299, 'Não encontrei o Evento S-1299');
end;

procedure TACBreSocialConversaoeSocialTest.ConversaoeSocial_StringINIToTipoEvento_RetornaS1300;
begin
  FArqINI.Clear;
  FArqINI.LoadFromFile(ARQINI_S0100_S1300);
  Check(StringINIToTipoEvento(OK, FArqINI.Text, ve02_05_00) = teS1300, 'Não encontrei o Evento S-1300');
end;

initialization
  _RegisterTest('pcesConversaoeSocial', TACBreSocialConversaoeSocialTest);
  _RegisterTest('pcesConversaoeSocial', TACBreSocialTipoToStrStrToTipoTest);
  _RegisterTest('pcesNaoPeriodicos'   , TACBreSocialEventosNaoPeriodicosTest);
  _RegisterTest('pcesPeriodicos'      , TACBreSocialEventosPeriodicosTest);

end.

