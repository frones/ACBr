unit ACBreSocialEventosTabelasTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocialTestsConsts, ACBreSocial, pcesConversaoeSocial,
  ACBrDFeException, pcesS1010 , pcesS1020, pcesS1070, pcesCommon, ACBrUtil.DateTime;

type

  { TACBreSocialEventosTabelasTest }

  TACBreSocialEventosTabelasTest = class(TTestCase)
  private
    FACBreSocial: TACBreSocial;
    procedure ES1010Tests;
    procedure ES1020Tests;
    procedure ES1070Tests;
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure ACBreSocialEventosTabelasS1010_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1010_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1010_vS0101;
    procedure ACBreSocialEventosTabelasSS1020_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1020_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1020_vS0101;
    procedure ACBreSocialEventosTabelasS1070_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1070_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1070_vS0101;
  end;

implementation

{ TACBreSocialEventosTabelasTest }

procedure TACBreSocialEventosTabelasTest.ES1010Tests;
var
  eS1010: TEvtTabRubrica;
  ideRubrica: TideRubrica;
  dadosRubrica: TDadosRubrica;
  ideProcessoCP: TIdeProcessoCPCollectionItem;
  ideProcessoIRRF: TProcesso;
begin
  Check(FACBreSocial.Eventos.Tabelas.S1010.Count > 0, 'Não instanciou o S-1010 na lista!');

  eS1010 := FACBreSocial.Eventos.Tabelas.S1010[0].EvtTabRubrica;
  Check(eS1010.Sequencial = 0, 'Sequencial | Valor esperado:0 | Valor recebido:'+IntToStr(eS1010.Sequencial));
  Check(eS1010.ModoLancamento = mlInclusao, 'ModoLancamento | Valor esperado:inclusao | Valor recebido:'+eSModoLancamentoToStr(eS1010.ModoLancamento));

  Check(eS1010.IdeEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1010.IdeEvento.ProcEmi));
  Check(eS1010.ideEvento.VerProc = '1.00', 'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1010.ideEvento.VerProc);

  Check(eS1010.IdeEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1010.ideEmpregador.tpInsc));
  Check(eS1010.IdeEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1010.ideEmpregador.nrInsc);

  ideRubrica := eS1010.InfoRubrica.ideRubrica;
  Check(ideRubrica.CodRubr='123', 'infoRubrica.ideRubrica.codRubr | Valor esperado:123 | Valor recebido:'+ideRubrica.CodRubr);
  Check(ideRubrica.ideTabRubr='456', 'infoRubrica.ideRubrica.ideTabRubr | Valor esperado:456 | Valor recebido:'+ideRubrica.ideTabRubr);
  Check(ideRubrica.iniValid='2018-01', 'infoRubrica.ideRubrica.iniValid | Valor esperado:2018-01 | Valor recebido:'+ideRubrica.iniValid);
  Check(ideRubrica.fimValid='2018-12', 'infoRubrica.ideRubrica.fimValid | Valor esperado:2018-12 | Valor recebido:'+ideRubrica.fimValid);

  dadosRubrica := eS1010.infoRubrica.DadosRubrica;
  Check(dadosRubrica.dscRubr='Descricao', 'infoRubrica.dadosRubrica.dscRubr | Valor esperado:Descricao | Valor recebido:'+dadosRubrica.dscRubr);
  Check(dadosRubrica.natRubr=1022, 'infoRubrica.dadosRubrica.natRubr | Valor esperado:1022 | Valor recebido:'+IntToStr(dadosRubrica.natRubr));
  Check(dadosRubrica.tpRubr = tpVencimento, 'infoRubrica.dadosRubrica.tpRubr | Valor esperado:1 | Valor recebido:'+eSTpRubrToStr(dadosRubrica.tpRubr));
  Check(dadosRubrica.codIncCP = cicNaoeBasedeCalculo, 'infoRubrica.dadosRubrica.codIncCP | Valor esperado:00 | Valor recebido:'+eSCodIncCPToStr(dadosRubrica.codIncCP));
  Check(dadosRubrica.codIncIRRF = ciiNaoeBasedeCalculo, 'infoRubrica.dadosRubrica.codIncIRRF | Valor esperado:00 | Valor recebido:'+eSCodIncIRRFToStr(dadosRubrica.codIncIRRF));
  Check(dadosRubrica.codIncFGTS = cdfNaoeBasedeCalculo, 'infoRubrica.dadosRubrica.codIncFGTS | Valor esperado:00 | Valor recebido:'+eSCodIncFGTSToStr(dadosRubrica.codIncFGTS));
  Check(dadosRubrica.codIncCPRP = cicpNaoeBasedeCalculodeContribuicoesDevidasaoRPPSRegimeMilitar,
        'infoRubrica.dadosRubrica.codIncCPRP | Valor esperado:00 | Valor recebido:'+eSCodIncCPRPToStr(dadosRubrica.codIncCPRP));

  Check(dadosRubrica.tetoRemun = snfNao, 'infoRubrica.dadosRubrica.tetoRemun | Valor esperado:N | Valor recebido:'+eSSimNaoFacultativoToStr(dadosRubrica.tetoRemun));
  Check(dadosRubrica.observacao = EmptyStr, 'infoRubrica.dadosRubrica.observacao | Valor esperado: | Valor recebido:'+dadosRubrica.observacao);

  ideProcessoCP := dadosRubrica.IdeProcessoCP.Items[0];
  Check(ideProcessoCP.tpProc = tpAdministrativo,
        'infoRubrica.dadosRubrica.ideProcessoCP.tpProc | Valor esperado:1 | Valor recebido:'+eSTpProcessoToStr(ideProcessoCP.tpProc));

  Check(ideProcessoCP.nrProc = '123',
        'infoRubrica.dadosRubrica.ideProcessoCP.nrPro | Valor  esperado:123 | Valor recebido:'+ideProcessoCP.nrProc);

  ideProcessoIRRF := dadosRubrica.IdeProcessoIRRF.Items[0];
  Check(ideProcessoIRRF.nrProc ='123',
        'infoRubrica.dadosRubrica.ideProcessoIRRF.nrProc | Valor esperado:123 | Valor recebido:'+ideProcessoIRRF.nrProc);

  Check(ideProcessoIRRF.codSusp='456',
        'infoRubrica.dadosRubrica.ideProcessoIRRF.codSusp | Valor esperado:456 | Valor recebido:'+ideProcessoIRRF.codSusp);

  Check(dadosRubrica.IdeProcessoFGTS[0].nrProc = '123',
        'infoRubrica.dadosRubrica.ideProcessoFGTS.nrProc | Valor esperado:123 | Valor recebido:'+dadosRubrica.IdeProcessoFGTS[0].nrProc);

end;

procedure TACBreSocialEventosTabelasTest.ES1020Tests;
var
  eS1020: TevtTabLotacao;
begin
  Check(FACBreSocial.Eventos.Tabelas.S1020.Count > 0, 'Não instanciou o S-1020 na lista!');

  eS1020 := FACBreSocial.Eventos.Tabelas.S1020[0].EvtTabLotacao;

  Check(eS1020.ModoLancamento = mlInclusao, 'modoLancamento | Valor esperado:inclusao | Valor recebido:'+eSModoLancamentoToStr(mlInclusao));

  Check(eS1020.IdeEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(peAplicEmpregador));
  Check(eS1020.IdeEvento.VerProc = '1.00', 'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1020.IdeEvento.verProc);

  Check(eS1020.ideEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1020.ideEmpregador.tpInsc));
  Check(eS1020.ideEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1020.ideEmpregador.NrInsc);

  Check(eS1020.infoLotacao.ideLotacao.codLotacao = '123',
        'infoLotacao.ideLotacao.codLotacao | Valor esperado:123 | Valor recebido:'+eS1020.infoLotacao.ideLotacao.codLotacao);

  Check(eS1020.infoLotacao.ideLotacao.iniValid = '2018-01',
        'infoLotacao.ideLotacao.iniValid | Valor esperado:2018-01 | Valor recebido:'+eS1020.infoLotacao.ideLotacao.iniValid);

  Check(eS1020.infoLotacao.ideLotacao.fimValid = '2018-12',
        'infoLotacao.ideLotacao.fimValid | Valor esperado:2018-12 | Valor recebido:'+eS1020.infoLotacao.ideLotacao.fimValid);

  Check(eS1020.infoLotacao.dadosLotacao.tpLotacao = '01',
        'infoLotacao.dadosLotacao.tpLotacao | Valor esperado:01 | Valor recebido:'+eS1020.infoLotacao.dadosLotacao.tpLotacao);

  Check(eS1020.infoLotacao.dadosLotacao.tpInsc = tiCNPJ,
        'infoLotacao.dadosLotacao.tpInsc | Valor esperado:1 | Valor recebido:'+eStpInscricaoToStr(eS1020.infoLotacao.dadosLotacao.tpInsc));

  Check(eS1020.infoLotacao.dadosLotacao.nrInsc ='12345678000123',
        'infoLotacao.dadosLotacao.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1020.infoLotacao.dadosLotacao.nrInsc);

  Check(eS1020.infoLotacao.dadosLotacao.fPasLotacao.Fpas = '515',
        'infoLotacao.dadosLotacao.fPasLotacao.Fpas | Valor esperado:515 | Valor recebido:'+eS1020.infoLotacao.dadosLotacao.fPasLotacao.Fpas);

  Check(eS1020.infoLotacao.dadosLotacao.fPasLotacao.codTercs = '0015',
        'infoLotacao.dadosLotacao.fPasLotacao.codTerc | Valor esperado:0015 | Valor recebido:'+eS1020.infoLotacao.dadosLotacao.fPasLotacao.codTercs);

  Check(eS1020.infoLotacao.dadosLotacao.fPasLotacao.codTercsSusp ='0506',
        'infoLotacao.dadosLotacao.fPasLotacao.codTercsSups | Valor esperado:0506 | Valor recebido:'+eS1020.infoLotacao.dadosLotacao.fPasLotacao.codTercsSusp);

  Check(eS1020.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[0].codTerc = '1111',
        'infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.codTerc | Valor esperado:1111 | '+
        'Valor recebido:'+ eS1020.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[0].codTerc);

  Check(eS1020.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[0].nrProcJud = '456',
        'infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.nrProcJud | Valor esperado:456 | '+
        'Valor recebido:'+eS1020.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[0].nrProcJud);

  Check(eS1020.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[0].codSusp = '789',
        'infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.codSusp | Valor esperado:789 | ' +
        'Valor recebido:'+eS1020.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[0].codSusp);

  Check(eS1020.infoLotacao.dadosLotacao.infoEmprParcial.tpInscContrat = icCNPJ,
        'infoLotacao.dadosLotacao.infoEmprParcial.tpInscContrat | Valor esperado:1 | Valor recebido:'+eStpInscContratanteToStr(eS1020.infoLotacao.dadosLotacao.infoEmprParcial.tpInscContrat));

  Check(eS1020.infoLotacao.dadosLotacao.infoEmprParcial.NrInscContrat = '12345678000123',
        'infoLotacao.dadosLotacao.infoEmprParcial.NrInscContrat | Valor esperado:12345678000123 | Valor recebido:'+eS1020.infoLotacao.dadosLotacao.infoEmprParcial.NrInscContrat);

  Check(eS1020.infoLotacao.dadosLotacao.infoEmprParcial.tpInscProp = tpCNPJ,
        'infoLotacao.dadosLotacao.infoEmprParcial.tpInscProp | Valor esperado:1 | Valor recebido:'+eSTpInscPropToStr(eS1020.infoLotacao.dadosLotacao.infoEmprParcial.tpInscProp));

  Check(eS1020.infoLotacao.dadosLotacao.infoEmprParcial.nrInscProp='12345678000123',
        'infoLotacao.dadosLotacao.infoEmpreParcial.nrInscProp | Valor esperado:12345678000123 | Valor recebido:'+eS1020.infoLotacao.dadosLotacao.infoEmprParcial.nrInscProp);

  Check(eS1020.infoLotacao.dadosLotacao.dadosOpPort.aliqRat= arat1,
        'infoLotacao.dadosLotacao.dadosOpPort.aliqRat | Valor esperado:1 | Valor recebido:'+eSAliqRatToStr(eS1020.infoLotacao.dadosLotacao.dadosOpPort.aliqRat));

  Check(eS1020.infoLotacao.dadosLotacao.dadosOpPort.fap = 0.5000,
        'infoLotacao.dadosLotacao.dadosOpPort.fap | Valor esperado:0,5000 | Valor recebido:'+FormatFloat('0.000', eS1020.infoLotacao.dadosLotacao.dadosOpPort.fap));




end;

procedure TACBreSocialEventosTabelasTest.ES1070Tests;
var
  eS1070: TEvtTabProcesso;
begin
  Check(FACBreSocial.Eventos.Tabelas.S1070.Count > 0, 'Não instanciou o evento S-1070 na Lista!');
  eS1070 := FACBreSocial.Eventos.Tabelas.S1070[0].EvtTabProcesso;

  Check(eS1070.ModoLancamento = mlInclusao, 'ModoLancamento | Valor esperado:inclusao | Valor recebido:'+eSModoLancamentoToStr(eS1070.ModoLancamento));

  Check(eS1070.ideEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1070.IdeEvento.ProcEmi));

  Check(eS1070.ideEvento.VerProc='1.00', 'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1070.ideEvento.verProc);

  Check(eS1070.ideEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1070.IdeEmpregador.TpInsc));

  Check(eS1070.ideEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1070.ideEmpregador.NrInsc);

  Check(eS1070.InfoProcesso.ideProcesso.tpProc = tpAdministrativo,
        'infoProcesso.ideProcesso.tpProc | Valor esperado:1 | Valor recebido:'+eSTpProcessoToStr(eS1070.InfoProcesso.ideProcesso.tpProc));

  Check(eS1070.infoProcesso.ideProcesso.nrProc = '123',
        'infoProcesso.ideProcesso.nrProc | Valor esperado:123 | Valor recebido:'+eS1070.infoProcesso.ideProcesso.nrProc);

  Check(eS1070.infoProcesso.ideProcesso.iniValid = '2018-01',
        'infoProcesso.ideProcesso.iniValid | Valor esperado:2018-01 | Valor recebido:'+eS1070.infoProcesso.ideProcesso.iniValid);

  Check(eS1070.infoProcesso.ideProcesso.fimValid = '2018-12',
        'infoProcesso.ideProcesso.fimValid | Valor esperado:2018-12 | Valor recebido:'+eS1070.infoProcesso.ideProcesso.fimValid);

  Check(eS1070.infoProcesso.dadosProc.indAutoria = iaProprioContribuinte,
        'infoProcesso.dadosProc.indAutoria | Valor esperado:1 | Valor recebido:'+eSindAutoriaToStr(eS1070.infoProcesso.dadosProc.indAutoria));

  Check(eS1070.infoProcesso.dadosProc.indMatProc = impTributaria,
        'infoProcesso.dadosProc.indMatProc | Valor esperado:1 | Valor recebido:'+eSTpIndMatProcToStr(eS1070.infoProcesso.dadosProc.indMatProc));

  Check(eS1070.infoProcesso.dadosProc.observacao = EmptyStr,
        'infoProcesso.dadosProc.observacao | Valor esperado: | Valor recebido:'+eS1070.infoProcesso.dadosProc.observacao);

  Check(eS1070.infoProcesso.dadosProc.DadosProcJud.UfVara='SP',
        'infoProcesso.dadosProc.DadosProcJud.UfVara | Valor esperado:SP | Valor recebido:'+eS1070.infoProcesso.dadosProc.DadosProcJud.UfVara);

  Check(eS1070.infoProcesso.dadosProc.DadosProcJud.codMunic=3512345,
        'infoProcesso.dadosProc.DadosProcJud.codMunic | Valor esperado:3512345 | Valor recebido:'+IntToStr(eS1070.InfoProcesso.dadosProc.DadosProcJud.codMunic));

  Check(eS1070.infoProcesso.dadosProc.DadosProcJud.idVara='20',
        'infoProcesso.dadosProc.DadosProcJud.idVara | Valor esperado:20 | Valor recebido:'+eS1070.infoProcesso.dadosProc.DadosProcJud.idVara);

  Check(eS1070.InfoProcesso.dadosProc.infoSusp[0].codSusp='123',
        'infoProcesso.dadosProc.infoSusp.codSusp | Valor esperado:123 | Valor recebido:'+eS1070.InfoProcesso.dadosProc.infoSusp[0].codSusp);

  Check(eS1070.infoProcesso.dadosProc.infoSusp[0].indSusp = siLiminarMandadoSeguranca,
        'infoProcesso.dadosProc.infoSusp.indSusp | Valor esperado:01 | Valor recebido:'+eSIndSuspToStr(eS1070.infoProcesso.dadosProc.infoSusp[0].indSusp));

  Check(eS1070.InfoProcesso.dadosProc.infoSusp[0].dtDecisao = StoD('20180502000000'),
        'infoProcesso.dadosProc.infoSusp.dtDecisao | Valor esperado:02/05/2018 | Valor recebido:'+FormatDateBr(eS1070.InfoProcesso.dadosProc.infoSusp[0].dtDecisao));

  Check(eS1070.InfoProcesso.dadosProc.infoSusp[0].indDeposito = tpSim,
        'infoProcesso.dadosProc.infoSusp.indDeposito | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS1070.InfoProcesso.dadosProc.infoSusp[0].indDeposito));
end;

procedure TACBreSocialEventosTabelasTest.Setup;
begin
  inherited Setup;
  FACBreSocial := TACBreSocial.Create(nil);
end;

procedure TACBreSocialEventosTabelasTest.TearDown;
begin
  FACBreSocial.Free;
  inherited TearDown;
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventosTabelasS1010_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Tabelas.S1010.Count = 0, 'Lista de eventos S-1010 não está vazia');
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1010_vS0100;
begin
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S1010);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1010Tests;
    end;
  end;
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1010_vS0101;
begin
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1010);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1010Tests;
    end;
  end;
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventosTabelasSS1020_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Tabelas.S1020.Count = 0, 'Lista de eventos S-1020 não está vazia');
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1020_vS0100;
begin
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S1020);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName+ '  | Erro:'+E.Message);
      ES1020Tests;
    end;
  end;
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1020_vS0101;
begin
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1020);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1020Tests;
    end;
  end;
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventosTabelasS1070_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Tabelas.S1070.Count = 0, 'Lista de eventos S-1070 não está vazia!');
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1070_vS0100;
begin
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0100_S1070);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1070Tests;
    end;
  end;
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1070_vS0101;
begin
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S1070);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1070Tests;
    end;
  end;
end;

end.

