unit ACBreSocialEventosIniciaisTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocialTestsConsts, ACBreSocial, pcesConversaoeSocial,
  ACBrDFeException, pcesS1000, pcesS1005, pcesCommon, ACBrUtil.DateTime;
type

  { TACBreSocialEventosIniciaisTest }

  TACBreSocialEventosIniciaisTest = class(TTestCase)
  private
    FACBreSocial: TACBreSocial;
    procedure ES1000Tests;
    procedure ES1005Tests;
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure ACBreSocialEventosIniciaisS1000_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1000_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1000_vS0101;
    procedure ACBreSocialEventosIniciaisS1005_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1005_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1005_vS0101;
  end;

implementation

{ TACBreSocialEventosIniciaisTest }

procedure TACBreSocialEventosIniciaisTest.ES1000Tests;
var
  eS1000: TevtInfoEmpregador;
  idePeriodo: TIdePeriodo;
  infoCadastro: TInfoCadastro;
  dadosIsencao: TDadosIsencao;
begin
  Check(FACBreSocial.Eventos.Iniciais.S1000.Count > 0, 'Não instanciou o S-1000 na lista');

  eS1000 := FACBreSocial.Eventos.Iniciais.S1000[0].evtInfoEmpregador;

  Check(eS1000.ModoLancamento = mlInclusao, 'ModoLancamento | Valor esperado=inclusao | Valor recebido:'+eSModoLancamentoToStr(eS1000.ModoLancamento));

  Check(eS1000.ideEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1000.ideEvento.ProcEmi));
  Check(eS1000.ideEvento.VerProc = '1.00', 'ideEvento.verProc | Valor esperado:1.00 | Valor recebido: ' + eS1000.ideEvento.VerProc);

  Check(eS1000.ideEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1000.ideEmpregador.TpInsc));
  Check(eS1000.ideEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1000.ideEmpregador.NrInsc);

  idePeriodo := eS1000.infoEmpregador.idePeriodo;
  Check(idePeriodo.IniValid = '2018-01', 'infoEmpregador.idePeriodo.iniValid | Valor esperado:2018-01 | Valor recebido:'+idePeriodo.IniValid);
  Check(idePeriodo.FimValid = '2018-12', 'infoEmpregador.idePeriodo.fimValid | Valor esperado:2018-12 | Valor recebido:'+idePeriodo.FimValid);

  infoCadastro := eS1000.infoEmpregador.infoCadastro;
  Check(infoCadastro.ClassTrib = ct00, 'infoEmpregador.InfoCadastro.classTrib | Valor esperado:00 | Valor recebido:'+tpClassTribToStr(infoCadastro.ClassTrib));
  Check(infoCadastro.IndCoop = icNaoecooperativa, 'infoEmpregador.infoCadastro.indCoop | Valor esperado:0 | Valor recebido:'+eSIndCooperativaToStr(infoCadastro.IndCoop));
  Check(infoCadastro.IndConstr = iconNaoeConstrutora, 'infoEmpregador.infoCadastro.indConstr | Valor esperado:0 | Valor recebido:'+eSIndConstrutoraToStr(infoCadastro.IndConstr));
  Check(infoCadastro.indDesFolha = idfNaoAplicavel, 'infoEmpregador.infoCadastro.indDesFolha | Valor esperado:0 | Valor recebido:'+eSIndDesFolhaToStr(infoCadastro.IndDesFolha));
  Check(infoCadastro.IndOpcCP = icpComercializacao, 'infoEmpregador.infoCadastro.indOpcCP | Valor esperado:1 | Valor recebido:'+eSIndOpcCPToStr(infoCadastro.IndOpcCP));
  Check(infoCadastro.IndPorte = tpSim, 'infoEmpregador.infoCadastro.indPorte | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(InfoCadastro.IndPorte));
  Check(infoCadastro.IndOptRegEletron = iorNaooptou, 'infoEmpregador.infoCadastro.indOptRegEletron | Valor esperado:0 | Valor recebido:'+eSIndOptRegEletronicoToStr(infoCadastro.IndOptRegEletron));
  Check(infoCadastro.cnpjEFR = '12345678901', 'infoEmpregador.infoCadastro.cnpjEFR | Valor esperado:12345678901 | Valor recebido:'+infoCadastro.cnpjEFR);
  Check(infoCadastro.dtTrans11096 = 0, 'infoEmpregador.infoCadastro.dtTrans11096 | Valor esperado: | Valor recebido:' + DateToStr(infoCadastro.dtTrans11096));
  if(FACBreSocial.Configuracoes.Geral.VersaoDF = veS01_01_00)then
    Check(infoCadastro.indTribFolhaPisCofins = snfSim, 'infoEmpregador.infoCadastro.indTribFolhaPisCofins | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(infoCadastro.indTribFolhaPisCofins));

  dadosIsencao := infoCadastro.DadosIsencao;
  Check(dadosIsencao.IdeMinLei = 'Sigla Min', 'infoEmpregador.infoCadastro.dadosIsencao.ideMinLei | Valor esperado:Sigla Min | Valor recebido:'+dadosIsencao.IdeMinLei);
  Check(dadosIsencao.NrCertif = '1111', 'infoEmpregador.infoCadastro.dadosIsencao.nrCertif | Valor esperado:1111 | Valor recebido:' + dadosIsencao.NrCertif);
  Check(dadosIsencao.DtEmisCertif = StoD('20180101000000'), 'infoEmpregador.infoCadastro.dadosIsencao.dtEmisCertif | Valor esperado:01/01/2018 | Valor recebido:'+DateToStr(dadosIsencao.DtEmisCertif));
  Check(dadosIsencao.DtVencCertif = StoD('20220101000000'), 'infoEmpregador.infoCadastro.dadosIsencao.dtVencCertif | Valor esperado:01/01/2022 | Valor recebido:'+DateToStr(dadosIsencao.DtVencCertif));
  Check(dadosIsencao.NrProtRenov='10', 'infoEmpregador.infoCadastro.dadosIsencao.nrProtRenov | Valor esperado:10 | Valor recebido:' + dadosIsencao.NrProtRenov);
  Check(dadosIsencao.DtProtRenov=StoD('20180430000000'), 'infoEmpregador.infoCadastro.dadosIsencao.dtProtRenov | Valor esperado:30/04/2018 | Valor recebido:'+DateToStr(dadosIsencao.DtProtRenov));
  Check(dadosIsencao.DtDou = StoD('20180430000000'), 'infoEmpregador.infoCadastro.dadosIsencao.dtDou | Valor esperado:30/04/2018 | Valor recebido:'+DateToStr(dadosIsencao.DtDou));
  Check(dadosIsencao.PagDou = '111', 'infoEmpregador.infoCadastro.dadosIsencao.pagDou | Valor esperado:111 | Valor recebido:'+dadosIsencao.PagDou);

  Check(infoCadastro.InfoOrgInternacional.IndAcordoIsenMulta = iaiSemacordo, 'infoEmpregador.infoCadastro.infoOrgInternacional.indAcordoIsenMulta | Valor esperado:0 | Valor recebido:'+eSIndAcordoIsencaoMultaToStr(infoCadastro.InfoOrgInternacional.IndAcordoIsenMulta));

  if(eS1000.ModoLancamento = mlAlteracao)then
  begin
    Check(eS1000.infoEmpregador.novaValidade.IniValid='2018-01', 'infoEmpregador.novaValidade.iniValid | Valor esperado:2018-01 | Valor recebido:'+eS1000.infoEmpregador.novaValidade.IniValid);
    Check(eS1000.infoEmpregador.novaValidade.FimValid='2018-12', 'infoEmpregador.novaValidade.fimValid | Valor esperado:2018-12 | Valor recebido:'+eS1000.infoEmpregador.novaValidade.FimValid);
  end;

end;

procedure TACBreSocialEventosIniciaisTest.ES1005Tests;
var
  eS1005: TEvtTabEstab;
  ideEstab: TIdeEstab;
  dadosEstab: TDadosEstab;
  aliqGilRat: TAliqGilRat;
  infoTrab: TInfoTrab;
begin
  Check(FACBreSocial.Eventos.Iniciais.S1005.Count > 0, 'Não instanciou o S-1005 na lista!');

  eS1005 := FACBreSocial.Eventos.Iniciais.S1005[0].evtTabEstab;
  Check(eS1005.Sequencial = 0, 'Sequencial | Valor esperado:0 | Valor recebido:'+IntToStr(eS1005.Sequencial));
  Check(eS1005.ModoLancamento = mlInclusao, 'ModoLancamento | Valor esperado:inclusao | Valor recebido:'+eSModoLancamentoToStr(eS1005.ModoLancamento));

  Check(eS1005.ideEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1005.ideEvento.ProcEmi));
  Check(eS1005.ideEvento.VerProc = '1.00', 'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1005.ideEvento.VerProc);

  Check(eS1005.ideEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1005.ideEmpregador.tpInsc));
  Check(eS1005.ideEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1005.ideEmpregador.NrInsc);

  ideEstab := eS1005.infoEstab.IdeEstab;
  Check(ideEstab.tpInsc = tiCNPJ, 'infoEstab.IdeEstab.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(ideEstab.tpInsc));
  Check(ideEstab.nrInsc = '12345678', 'infoEstab.ideEstab.nrInsc | Valor esperado:12345678 | Valor recebido:'+ideEstab.nrInsc);
  Check(ideEstab.iniValid='2018-01', 'infoEstab.ideEstab.iniValid | Valor esperado:2018-01 | Valor recebido:'+ideEstab.iniValid);
  Check(ideEstab.fimValid='2018-12', 'infoEstab.ideEstab.fimValid | Valor esperado:2018-12 | Valor recebido:'+ideEstab.fimValid);

  dadosEstab := eS1005.infoEstab.DadosEstab;
  Check(dadosEstab.cnaePrep = '2015', 'infoEstab.dadosEstab.cnaePrep | Valor esperado:2015 | Valor recebido:'+dadosEstab.cnaePrep);
  Check(dadosEstab.cnpjResp = '11111111111111', 'infoEstab.dadosEstab.cnpjResp | Valor esperado:11111111111111 | Valor recebido:'+dadosEstab.cnpjResp);

  aliqGilRat := dadosEstab.aliqGilrat;
  Check(aliqGilrat.AliqRat = arat1, 'infoEstab.dadosEstab.aliqGilRat.AliqRat | Valor esperado:1 | Valor recebido:'+eSAliqRatToStr(aliqGilrat.AliqRat));
  Check(aliqGilrat.Fap = 0, 'infoEstab.dadosEstab.aliqGilRat.fap | Valor esperado:0 | Valor recebido:'+FloatToStr(aliqGilrat.Fap));

  Check(aliqGilRat.ProcAdmJudRat.tpProc = tpAdministrativo,
        'infoEstab.dadosEstab.aliqGilRat.procAdmJudRat.tpProc | Valor esperado:1 | Valor recebido:'+eSTpProcessoToStr(aliqGilRat.ProcAdmJudRat.tpProc));

  Check(aliqGilRat.ProcAdmJudRat.nrProc = '1234',
        'infoEstab.dadosEstab.aliqGilRat.procAdmJudRat.nrProc | Valor esperado:1234 | Valor recebido:'+aliqGilRat.ProcAdmJudRat.nrProc);

  Check(aliqGilRat.ProcAdmJudRat.codSusp='5678',
        'infoEstab.dadosEstab.aliqGilRat.procAdmJudRat.codSusp | Valor esperado:5678 | Valor recebido:'+aliqGilRat.ProcAdmJudRat.codSusp);

  Check(aliqGilRat.ProcAdmJudFap.tpProc = tpAdministrativo,
        'infoEstab.dadosEstab.aliqGilRat.procAdmJudFap.tpProc | Valor esperado:1 | Valor recebido:'+eSTpProcessoToStr(aliqGilRat.ProcAdmJudFap.tpProc));

  Check(aliqGilRat.ProcAdmJudFap.nrProc = '1234',
        'infoEstab.dadosEstab.aliqGilRat.procAdmJudFap.nrProc | Valor esperado:1234 | Valor recebido:'+aliqGilRat.ProcAdmJudFap.nrProc);

  Check(aliqGilRat.ProcAdmJudFap.codSusp='5678',
        'infoEstab.dadosEstab.aliqGilRat.procAdmJudFap.codSusp | Valor esperado:5678 | Valor recebido:'+aliqGilRat.ProcAdmJudFap.codSusp);

  Check(dadosEstab.infoCaepf.tpCaepf = tcContrIndividual,
        'infoEstab.dadosEstab.infoCaepf.tpCaepf | Valor esperado:1 | Valor recebido:'+eSTpCaepfToStr(dadosEstab.infoCaepf.tpCaepf));

  Check(dadosEstab.infoObra.indSubstPatrObra = ispPatronalSubstituida,
        'infoEstab.dadosEstab.infoObra.indSubstPatrObr | Valor esperado:1 | Valor recebido:'+eSIndSubstPatronalObraToStr(dadosEstab.infoObra.indSubstPatrObra));

  infoTrab := dadosEstab.infoTrab;
  Check(infoTrab.infoApr.nrProcJud = '123',
        'infoEstab.dadosEstab.infoTrab.infoApr.nrProcJud | Valor esperado:123 | Valor recebido:'+infoTrab.infoApr.nrProcJud);

  Check(infoTrab.infoApr.infoEntEduc[0].nrInsc = '12345678',
        'infoEstab.dadosEstab.infoTrab.infoApr.infoEntEduc.nrInsc | Valor esperado:12345678 | Valor recebido:'+infoTrab.infoApr.infoEntEduc[0].nrInsc);

  Check(infoTrab.infoPCD.nrProcJud = '123',
        'infoEstab.dadosEstab.infoTrab.infoPCD.nrProcJud | Valor esperado:123 | Valor recebido:'+infoTrab.infoPCD.nrProcJud);
end;

procedure TACBreSocialEventosIniciaisTest.Setup;
begin
  inherited Setup;
  FACBreSocial := TACBreSocial.Create(nil);
end;

procedure TACBreSocialEventosIniciaisTest.TearDown;
begin
  inherited TearDown;
  FACBreSocial.Free;
end;

procedure TACBreSocialEventosIniciaisTest.ACBreSocialEventosIniciaisS1000_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Iniciais.S1000.Count = 0, 'Lista de eventos S-1000 não está vazia');;
end;

procedure TACBreSocialEventosIniciaisTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1000_vS0100;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S1000);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1000Tests;
    end;
  end;
end;

procedure TACBreSocialEventosIniciaisTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1000_vS0101;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1000);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1000Tests;
    end;
  end;
end;

procedure TACBreSocialEventosIniciaisTest.ACBreSocialEventosIniciaisS1005_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Iniciais.S1005.Count = 0, 'Lista de evento S-1005 não está vazia');
end;

procedure TACBreSocialEventosIniciaisTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1005_vS0100;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1005);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1005Tests;
    end;
  end;
end;

procedure TACBreSocialEventosIniciaisTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1005_vS0101;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1005);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1005Tests;
    end;
  end;
end;

end.

