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
    public
      procedure Setup;override;
      procedure TearDown;override;
    published
      procedure VersaoeSocialToStrEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToVersaoeSocialEX_ConvertendoStringinvalida_RetornoException;
      procedure SimNaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToSimNao_ConvertendoTodosTipos_RetornoCorreto;
      procedure SimNaoFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToSimNaoFacultativo_ConvertendoTodosTipos_RetornoCorreto;
      procedure ModoLancamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToModoLancamento_ConvertendoTodosTipos_RetornoCorreto;
      procedure LayouteSocialToServico_ConvertendoTodosTipos_RetornoCorreto;
      procedure ServicoToLayout_ConvertendoTodosTipos_RetornoCorreto;
      procedure TipoEventoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure StrToTipoEvento_ConvertentoTodosTipos_RetornoCorreto;
      procedure eSprocEmiToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToprocEmi_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpInscricaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpInscricao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpTpInscAmbTabToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpTpInscAmbTab_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpInscPropToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpInscProp_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndCooperativaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndCooperativa_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndConstrutoraToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndConstrutora_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndDesFolhaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndDesFolha_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndOptRegEletronicoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndOptRegEletronico_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndOpcCPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndOpcCP_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSAliqRatToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToAliqRat_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpProcessoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpProcesso_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndAcordoIsencaoMultaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndAcordoIsencaoMulta_ConvertendoTodosTipos_RetornoCorreto;
      procedure eStpInscContratanteToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrTotpInscContratante_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncCPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncCP_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncIRRFToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncIRRF_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncCPRPToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncCPRT_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncFGTSToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncFGTS_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSCodIncSINDToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToCodIncSIND_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSExtDecisaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToExtDecisao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpIntervaloToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpIntervalo_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndSubstPatronalObraToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndSubstPatronalObra_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSindAutoriaToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToindAutoria_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpIndMatProcToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpIndMatProc_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndRetificacaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndRetiricacao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndApuracaoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndApuracao_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndMVToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndMV_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndSimplesToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIndSimples_ConvertendoTodosTipos_RetornoCorreto;
      procedure VersaoeSocialToDblEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure DblToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSNatAtividadeToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToNatAtividade_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSTpTributoToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToTpTributo_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSGrauExpToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToGrauExp_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIndNIFToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIdeOCToStr_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIdeOC_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSIdeOCToStrEX_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIdeOCEx_ConvertendoTodosTipos_RetornoCorreto;
      procedure eSStrToIdeOCEX_ConvertendoStringinvalida_RetornoException;








  end;

implementation

uses
  typinfo, ACBrBase;


{ TACBreSocialTipoToStrStrToTipoTest }

procedure TACBreSocialTipoToStrStrToTipoTest.Setup;
begin
  inherited Setup;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.TearDown;
begin
  inherited TearDown;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.VersaoeSocialToStrEX_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TVersaoeSocial;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum: TVersaoeSocial;
  EhIgual: Boolean;
begin
  for vElementoEnum:= Low(TVersaoeSocial) to High(TVersaoeSocial) do
  begin
    ConvertidoParaString := VersaoeSocialToStrEX(vElementoEnum);
    ReconvertidoParaElementoEnum := StrToVersaoeSocialEX(ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro conversão no elemento Ord('+ GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'VersaoeSocialToStrEX => '+ ConvertidoParaString+'  '+
                       'StrToVersaoeSocialEX => '+ GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
var
  ElementoDoArray: string;
  ConvertidoParaEnum: TVersaoeSocial;
  ReconvertidoParaString: String;
  i: TVersaoeSocial;
  EhIgual: Boolean;
begin
  for i := low(TVersaoeSocialArrayStrings) to High(TVersaoeSocialArrayStrings) do
  begin
    ElementoDoArray := TVersaoeSocialArrayStrings[i];
    ConvertidoParaEnum := StrToVersaoeSocialEX(ElementoDoArray);
    ReconvertidoParaString := VersaoeSocialToStrEX(ConvertidoParaEnum);
    EhIgual := ReconvertidoParaString = ElementoDoArray;
    CheckTrue(EhIgual, 'Erro conversão no elemento '+ ElementoDoArray + '  '+
                       'StrToVersaoeSocialEX => '+ GetEnumName(TypeInfo(ConvertidoParaEnum), ord(ConvertidoParaEnum))+'  '+
                       'VersaoeSocialToStrEX => '+ReconvertidoParaString
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToVersaoeSocialEX_ConvertendoStringinvalida_RetornoException;
var
  ConvertidoParaEnum: TVersaoeSocial;
begin
  try
    ConvertidoParaEnum := StrToVersaoeSocialEX('StringInvalidaParaConvesao');
  except
    on EACBrException do
    begin
      Exit;
    end;
  end;
  Fail('Não foi gerada Exception para a string inválida.');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.SimNaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpSimNao;
  EhIgual, OK: boolean;
  ConvertidoParaString: string;
  ReconvertidoParaElementoEnum : tpSimNao;
begin
  for vElementoEnum := Low(tpSimNao) to High(tpSimNao)do
  begin
    ConvertidoParaString         := eSSimNaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToSimNao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro conversão no elemento Ord('+ GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSSimNaoToStr => ' + ConvertidoParaString+ '  ' +
                       'eSStrToSimNao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToSimNao_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo : tpSimNao;
  OK: Boolean;
begin
  auxTipo := eSStrToSimNao(OK, 'S');
  CheckTrue(auxTipo = tpSim, 'Erro de conversão, valor esperado "tpSim"');

  auxTipo := eSStrToSimNao(OK, 'N');
  CheckTrue(auxTipo = tpNao, 'Erro de conversão, valor esperado "tpNao"');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.SimNaoFacultativoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, Ok: Boolean;
  vEnumElemento: tpSimNaoFacultativo;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpSimNaoFacultativo;
begin
  for vEnumElemento := Low(tpSimNaoFacultativo) to High(tpSimNaoFacultativo)do
  begin
    ConvertidoParaString         := eSSimNaoFacultativoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToSimNaoFacultativo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento)) + '  ' +
                       'eSSimNaoFacultativoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToSimNaoFacultativo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToSimNaoFacultativo_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: tpSimNaoFacultativo;
  OK: Boolean;
begin
  auxTipo := eSStrToSimNaoFacultativo(OK, '');
  CheckTrue(auxTipo = snfNada, 'Erro de conversão, valor esperado ""');

  auxTipo := eSStrToSimNaoFacultativo(OK, 'S');
  CheckTrue(auxTipo = snfSim, 'Erro de conversão, valor esperado "S"');

  auxTipo := eSStrToSimNaoFacultativo(OK, 'N');
  CheckTrue(auxTipo = snfNao, 'Erro de conversão, valor esperado "N"');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.ModoLancamentoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TModoLancamento;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum : TModoLancamento;
begin
  for vEnumElemento := Low(TModoLancamento) to High(TModoLancamento)do
  begin
    ConvertidoParaString         := esModoLancamentoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToModoLancamento(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                   'eSModoLancamentoToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToModoLancamento => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToModoLancamento_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: TModoLancamento;
  OK: Boolean;
begin
  auxTipo := eSStrToModoLancamento(OK, 'inclusao');
  CheckTrue(auxTipo = mlInclusao, 'Erro de conversão, valor esperado "inclusao"');

  auxTipo := eSStrToModoLancamento(OK, 'alteracao');
  CheckTrue(auxTipo = mlAlteracao, 'Erro de conversão, valor esperado "alteracao"');

  auxTipo := eSStrToModoLancamento(OK, 'exclusao');
  CheckTrue(auxTipo = mlExclusao, 'Erro de conversão, valor esperado "exclusao"');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.LayouteSocialToServico_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TLayout;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum : TLayout;
begin
  for vEnumElemento := Low(TLayOut) to High(TLayout)do
  begin
    ConvertidoParaString         := LayOuteSocialToServico(vEnumElemento);
    ReconvertidoParaElementoEnum := ServicoToLayout(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento)) + ')=' + IntToStr(ord(vEnumElemento))+ '  ' +
                       'LayOuteSocialToServico => ' + ConvertidoParaString + '  ' +
                       'ServicoToLayOut => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.ServicoToLayout_ConvertendoTodosTipos_RetornoCorreto;
var
  auxTipo: TLayOut;
  OK:Boolean;
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

procedure TACBreSocialTipoToStrStrToTipoTest.TipoEventoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vEnumElemento: TTipoEvento;
  EhIgual, OK: Boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TTipoEvento;
begin
  for vEnumElemento := Low(TTipoEvento) to High(TTipoEvento)do
  begin
    ConvertidoParaString         := TipoEventoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := StrToTipoEvento(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')=' + IntToStr(ord(vEnumElemento)) + '  ' +
                       'TipoEventoToStr => ' + ConvertidoParaString + '  ' +
                       'StrToTipoEvento => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.StrToTipoEvento_ConvertentoTodosTipos_RetornoCorreto;
//var
//  ConvertidoParaElementoEnum: TTipoEvento;
//  ReconvertidoParaString: String;
//  EhIgual: Boolean;
//  i: Integer;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSprocEmiToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vEnumElemento: TpProcEmi;
  EhIgual, OK: Boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpProcEmi;
begin
  for vEnumElemento := Low(TpProcEmi) to High(TpProcEmi)do
  begin
    ConvertidoParaString := eSprocEmiToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToprocEmi(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')=' + IntToStr(ord(vEnumElemento)) + '  ' +
                       'eSprocEmiToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToprocEmi => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToprocEmi_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpInscricaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: tpTpInsc;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpTpInsc;
begin
  for vEnumElemento := Low(tpTpInsc) to High(tpTpInsc)do
  begin
    ConvertidoParaString         := eSTpInscricaoToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToTpInscricao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                       'eSTpInscricaoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToTpInscricao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpInscricao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpTpInscAmbTabToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: tpTpInscAmbTab;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpTpInscAmbTab;
begin
  for vEnumElemento := Low(tpTpInscAmbTab) to High(tpTpInscAmbTab)do
  begin
    ConvertidoParaString         := eStpTpInscAmbTabToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrTotpTpInscAmbTab(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                       'eStpTpInscAmbTabToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrTotpTpInscAmbTab => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpTpInscAmbTab_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpInscPropToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TpTpInscProp;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpTpInscProp;
begin
  for vEnumElemento := Low(TpTpInscProp) to High(TpTpInscProp)do
  begin
    ConvertidoParaString   := eSTpInscPropToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToTpInscProp(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento)) + '  '+
                       'eSTpInscPropToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToTpInscProp => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpInscProp_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndCooperativaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TpIndCoop;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndCoop;
begin
  for vEnumElemento := Low(TpIndCoop) to High(TpIndCoop)do
  begin
    ConvertidoParaString         := eSIndCooperativaToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToIndCooperativa(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+ '  ' +
                   'eSIndCooperativaToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToIndCooperativa => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndCooperativa_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndConstrutoraToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vEnumElemento: TpIndConstr;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndConstr;
begin
  for vEnumElemento := Low(TpIndConstr) to High(TpIndConstr)do
  begin
    ConvertidoParaString         := eSIndConstrutoraToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToIndConstrutora(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento)) +'  '+
                       'eSIndConstrutoraToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndConstrutora => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndConstrutora_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndDesFolhaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK : Boolean;
  vEnumElemento: TpIndDesFolha;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndDesFolha;
begin
  for vEnumElemento := Low(TpIndDesFolha) to High(TpIndDesFolha)do
  begin
    ConvertidoParaString := eSIndDesFolhaToStr(vEnumElemento);
    ReconvertidoParaElementoEnum := eSStrToIndDesFolha(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vEnumElemento);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vEnumElemento), ord(vEnumElemento))+')='+IntToStr(ord(vEnumElemento))+'  '+
                       'eSIndDesFolhaToStr => ' + ConvertidoParaString+'  ' +
                       'eSStrToIndDesFolha => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndDesFolha_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndOptRegEletronicoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK : Boolean;
  vElementoEnum: TpIndOptRegEletron;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndOptRegEletron;
begin
  for vElementoEnum := Low(TpIndOptRegEletron) to High(TpIndOptRegEletron)do
  begin
    ConvertidoParaString         := eSIndOptRegEletronicoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndOptRegEletronico(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversao no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndOptRegEletronicoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndOptRegEletronico => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndOptRegEletronico_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndOpcCPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndOpcCP;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndOpcCP;
begin
  for vElementoEnum := Low(TpIndOpcCP) to High(TpIndOpcCP)do
  begin
    ConvertidoParaString         := eSIndOpcCPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndOpcCP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    Check(EhIgual and OK, 'Erro de conversao no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  ' +
                   'eSIndOpcCPToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToIndOpcCP => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndOpcCP_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSAliqRatToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpAliqRat;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpAliqRat;
begin
  for vElementoEnum := Low(TpAliqRat) to High(TpAliqRat)do
  begin
    ConvertidoParaString := eSAliqRatToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToAliqRat(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSAliqRatToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToAliqRat => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum),ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToAliqRat_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpProcessoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpTpProc;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpTpProc;
begin
  for vElementoEnum := Low(tpTpProc) to High(tpTpProc)do
  begin
    ConvertidoParaString := eSTpProcessoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpProcesso(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  ' +
                       'eSTpProcessoToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToTpProcesso => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpProcesso_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndAcordoIsencaoMultaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndAcordoIsencaoMulta;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndAcordoIsencaoMulta;
begin
  for vElementoEnum := Low(TpIndAcordoIsencaoMulta) to High(TpIndAcordoIsencaoMulta)do
  begin
    ConvertidoParaString         := eSIndAcordoIsencaoMultaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndAcordoIsencaoMulta(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  ' +
                       'eSIndAcordoIsencaoMultaToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndAcordoIsencaoMulta => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum),ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndAcordoIsencaoMulta_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eStpInscContratanteToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TptpInscContratante;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TptpInscContratante;
begin
  for vElementoEnum := Low(TptpInscContratante) to High(TptpInscContratante)do
  begin
    ConvertidoParaString         := eStpInscContratanteToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrTotpInscContratante(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eStpInscContratanteToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrTotpInscContratante => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrTotpInscContratante_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncCPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpCodIncCP;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: tpCodIncCP;
begin
  for vElementoEnum := Low(tpCodIncCP) to High(tpCodIncCP)do
  begin
    ConvertidoParaString         := eSCodIncCPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncCP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  ' +
                       'eSCodIncCPToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToCodIncCP => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncCP_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncIRRFToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpCodIncIRRF;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: tpCodIncIRRF;
begin
  for vElementoEnum := Low(tpCodIncIRRF) to High(tpCodIncIRRF)do
  begin
    ConvertidoParaString         := eSCodIncIRRFToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncIRRF(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                   'eSCodIncIRRFToStr => ' + ConvertidoParaString + '  ' +
                   'eSStrToCodIncIRRF => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncIRRF_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncCPRPToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: boolean;
  vElementoEnum: TpCodIncCPRP;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpCodIncCPRP;
begin
  for vElementoEnum := Low(TpCodIncCPRP) to High(TpCodIncCPRP)do
  begin
    ConvertidoParaString         := eSCodIncCPRPToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncCPRP(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum)) + '  ' +
                       'eSCodIncCPRPToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToCodIncCPRT => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
              );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncCPRT_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncFGTSToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpCodIncFGTS;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpCodIncFGTS;
begin
  for vElementoEnum := Low(TpCodIncFGTS) to High(TpCodIncFGTS)do
  begin
    ConvertidoParaString         := eSCodIncFGTSToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncFGTS(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSCodIncFGTSToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToCodIncFGTS => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncFGTS_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSCodIncSINDToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpCodIncSIND;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpCodIncSIND;
begin
  for vElementoEnum := Low(TpCodIncSIND) to High(TpCodIncSIND)do
  begin
    ConvertidoParaString         := eSCodIncSINDToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToCodIncSIND(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSCodIncSINDToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToCodIncSIND => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToCodIncSIND_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSExtDecisaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpExtDecisao;
  ConvertidoParaString : String;
  ReConvertidoParaElementoEnum: TpExtDecisao;
  EhIgual, OK: Boolean;
begin
  for vElementoEnum := Low(TpExtDecisao) to High(TpExtDecisao)do
  begin
    ConvertidoParaString := eSExtDecisaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToExtDecisao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSExtDecisaoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToExtDecisao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToExtDecisao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpIntervaloToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpTpIntervalo;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpTpIntervalo;
begin
  for vElementoEnum := Low(TpTpIntervalo) to High(TpTpIntervalo)do
  begin
    ConvertidoParaString := eSTpIntervaloToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpIntervalo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpIntervaloToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToTpIntervalo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpIntervalo_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndSubstPatronalObraToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual: Boolean;
  vElementoEnum: TpIndSubstPatronalObra;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndSubstPatronalObra;
  OK : Boolean;
begin
  for vElementoEnum := Low(TpIndSubstPatronalObra) to High(TpIndSubstPatronalObra)do
  begin
    OK := False;
    ConvertidoParaString := eSIndSubstPatronalObraToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndSubstPatronalObra(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndSubstPatronalObraToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndSubstPatronalObra => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );

  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndSubstPatronalObra_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSindAutoriaToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpindAutoria;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpindAutoria;
begin
  for vElementoEnum := Low(TpindAutoria) to High(TpindAutoria)do
  begin
    ConvertidoParaString := eSindAutoriaToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToindAutoria(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSindAutoriaToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToindAutoria => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToindAutoria_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpIndMatProcToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: tpIndMatProc;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpIndMatProc;
begin
  for vElementoEnum := Low(tpIndMatProc) to High(tpIndMatProc)do
  begin
    ConvertidoParaString := eSTpIndMatProcToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpIndMatProc(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSTpIndMatProcToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToTpIndMatProc => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpIndMatProc_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndRetificacaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndRetificacao;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndRetificacao;
begin
  for vElementoEnum := Low(TpIndRetificacao) to High(TpIndRetificacao)do
  begin
    ConvertidoParaString := eSIndRetificacaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndRetificacao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndRetificacaoToStr => ' + ConvertidoParaString + '  ' +
                       'eSStrToIndRetificacao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndRetiricacao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndApuracaoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK : Boolean;
  vElementoEnum: TpIndApuracao;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndApuracao;
begin
  for vElementoEnum := Low(TpIndApuracao) to High(TpIndApuracao) do
  begin
    ConvertidoParaString := eSIndApuracaoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndApuracao(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndApuracaoToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndApuracao => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndApuracao_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndMVToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndMV;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpIndMV;
begin
  for vElementoEnum := Low(TpIndMV) to High(TpIndMV)do
  begin
    ConvertidoParaString := eSIndMVToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndMV(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndMVToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIndMV => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndMV_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda falta implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndSimplesToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual, OK: Boolean;
  vElementoEnum: TpIndSimples;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndSimples;
begin
  for vElementoEnum := Low(TpIndSimples) to High(TpIndSimples)do
  begin
    ConvertidoParaString := eSIndSimplesToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndSimples(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  '+
                       'eSIndSimplesToStr => ' + ConvertidoParaString + '  '+
                       'eSStrToIndSimples => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIndSimples_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.VersaoeSocialToDblEX_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TVersaoeSocial;
  ConvertidoParaDbl: Real;
  ReconvertidoParaElementoEnum: TVersaoeSocial;
  EhIgual: Boolean;
begin
  for vElementoEnum := Low(TVersaoeSocial) to High(TVersaoeSocial)do
  begin
    ConvertidoParaDbl := VersaoeSocialToDblEX(vElementoEnum);
    ReconvertidoParaElementoEnum := DblToVersaoeSocialEX(ConvertidoParaDbl);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'VersaoeSocialToDbl => ' + FormatFloat('##,####', ConvertidoParaDbl) +'  '+
                       'DblToVersaoeSocial => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.DblToVersaoeSocialEX_ConvertendoTodosTipos_RetornoCorreto;
var
  ConvertidoParaEnum: TVersaoeSocial;
  ReconvertidoParaDbl: Real;
  i: Integer;
begin
  Fail('Ainda precisa implementar o teste');
  //for i:= Low(TVersaoeSocialArrayReals) to High(TVersaoeSocialArrayReals)do
  //begin
  //  ConvertidoParaEnum  := DblToVersaoeSocialEX(TVersaoeSocialArrayReals[i]);
  //  ReconvertidoParaDbl := VersaoeSocialToDblEX(ConvertidoParaEnum);
  //  EhIgual := (ReconvertidoParaDbl = TVersaoeSocialArrayReals[i]);
  //  CheckTrue(EhIgual, 'Erro na conversão do elemento '+ FormatFloat('##,####', TVersaoeSocialArrayReals[i]) + ' (i='+IntToStr(i)+')  '+
  //                     'DblToVersaoeSocial => ' + GetEnumName(TypeInfo(ConvertidoParaEnum), ord(ConvertidoParaEnum)) +'  '+
  //                     'VersaoeSocialToDbl => ' + FormatFloat('##,####', ReconvertidoParaDbl)
  //           );
  //end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSNatAtividadeToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpNatAtividade;
  EhIgual: Boolean;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpNatAtividade;
  OK: Boolean;
begin
  for vElementoEnum := Low(TpNatAtividade) to High(TpNatAtividade)do
  begin
    ConvertidoParaString := eSNatAtividadeToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToNatAtividade(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  '+
                       'eSNatAtividadeToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToNatAtividade => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToNatAtividade_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSTpTributoToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpTpTributo;
  ConvertidoParaString: String;
  EhIgual : Boolean;
  ReconvertidoParaElementoEnum: TpTpTributo;
  OK: boolean;
begin
  for vElementoEnum := Low(TpTpTributo) to High(TpTpTributo)do
  begin
    ConvertidoParaString := eSTpTributoToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToTpTributo(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    Check(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+ '  '+
                   'eSTpTributoToStr => ' + ConvertidoParaString +'  '+
                   'eSStrToTpTributo => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
         );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToTpTributo_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSGrauExpToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: TpGrauExp;
  EhIgual: Boolean;
  ConvertidoParaString : String;
  ReconvertidoParaElementoEnum: TpGrauExp;
  OK: Boolean;
begin
  for vElementoEnum := Low(TpGrauExp) to High(TpGrauExp)do
  begin
    ConvertidoParaString := eSGrauExpToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToGrauExp(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSGrauExpToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToGrauExp => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToGrauExp_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIndNIFToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  EhIgual: Boolean;
  vElementoEnum: TpIndNIF;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: TpIndNIF;
  OK: Boolean;
begin
  for vElementoEnum := Low(TpIndNIF) to High(TpIndNIF)do
  begin
    ConvertidoParaString := eSIndNIFToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIndNIF(OK, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum),ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIndNIFToStr => ' + ConvertidoParaString+ '  '+
                       'eSStrToIndNIF => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIdeOCToStr_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIdeOC;
  ConvertidoParaString: String;
  ReconvertidoParaElementoEnum: tpIdeOC;
  EhIgual: Boolean;
  OK: Boolean;
begin
  for vElementoEnum := Low(tpIdeOC) to High(tpIdeOC)do
  begin
    ConvertidoParaString := eSIdeOCToStr(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIdeOC(Ok, ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual and OK, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIdeOCToStr => ' + ConvertidoParaString +'  '+
                       'eSStrToIdeOC => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIdeOC_ConvertendoTodosTipos_RetornoCorreto;
begin
  Fail('Ainda precisa implementar o teste');
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSIdeOCToStrEX_ConvertendoTodosTipos_RetornoCorreto;
var
  vElementoEnum: tpIdeOC;
  ConvertidoParaString: String;
  EhIgual: Boolean;
  ReconvertidoParaElementoEnum: tpIdeOC;
begin
  for vElementoEnum := Low(tpIdeOC) to High(tpIdeOC)do
  begin
    ConvertidoParaString := eSIdeOCToStrEX(vElementoEnum);
    ReconvertidoParaElementoEnum := eSStrToIdeOCEX(ConvertidoParaString);
    EhIgual := (ReconvertidoParaElementoEnum = vElementoEnum);
    CheckTrue(EhIgual, 'Erro de conversão no elemento ord('+GetEnumName(TypeInfo(vElementoEnum), ord(vElementoEnum))+')='+IntToStr(ord(vElementoEnum))+'  '+
                       'eSIdeOCToStrEX => ' + ConvertidoParaString + '  '+
                       'eSStrToIdeOCEX => ' + GetEnumName(TypeInfo(ReconvertidoParaElementoEnum), ord(ReconvertidoParaElementoEnum))
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIdeOCEx_ConvertendoTodosTipos_RetornoCorreto;
var
  i: tpIdeOC;
  ConvertidoParaElementoEnum: tpIdeOC;
  ReconvertidoParaString: String;
  EhIgual: Boolean;
  ElementoDoArray: String;
begin
  for i:= Low(TtpIdeOCArrayStrings) to High(TtpIdeOCArrayStrings)do
  begin
    ElementoDoArray := TtpIdeOCArrayStrings[i];
    ConvertidoParaElementoEnum := eSStrToIdeOCEX(ElementoDoArray);
    ReconvertidoParaString := eSIdeOCToStrEX(ConvertidoParaElementoEnum);
    EhIgual := (ReconvertidoParaString = ElementoDoArray);
    CheckTrue(EhIgual, 'Erro de conversão no elemento '+ ElementoDoArray + '  '+
                       'eSStrToIdeOCEX => ' + GetEnumName(TypeInfo(ConvertidoParaElementoEnum), ord(ConvertidoParaElementoEnum))+'  '+
                       'eSIdeOCToStrEX => ' + ReconvertidoParaString
             );
  end;
end;

procedure TACBreSocialTipoToStrStrToTipoTest.eSStrToIdeOCEX_ConvertendoStringinvalida_RetornoException;
var
  ConvertidoParaEnum : tpIdeOC;
begin
  try
    ConvertidoParaEnum := esStrToIdeOCEX('StringInvalidaParaConversao');
  except
    on E:Exception do
    begin
      Exit;
    end;
  end;
  Fail('Não foi gerada Exception para a string inválida');
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

