unit ACBreSocialEventosPeriodicosTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocialTestsConsts, ACBreSocial,
  pcesS1202, pcesS1207, pcesCommon, pcesConversaoeSocial;

type

  { TACBreSocialEventosPeriodicosTest }

  TACBreSocialEventosPeriodicosTest = class(TTestCase)
    private
      FACBreSocial : TACBreSocial;
    public
      procedure Setup;override;
      procedure TearDown;override;
    published
      procedure ACBreSocialEventosPeriodicosS1202_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1202;
      procedure ACBreSocialEventosPeriodicosS1207_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1207;
  end;

implementation

{ TACBreSocialEventosPeriodicosTest }

procedure TACBreSocialEventosPeriodicosTest.Setup;
begin
  inherited Setup;
  FACBreSocial := TACBreSocial.Create(nil);
end;

procedure TACBreSocialEventosPeriodicosTest.TearDown;
begin
  inherited TearDown;
  FACBreSocial.Free;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1202_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1202.Count = 0, 'Lista de eventos S-1202 não está vazia');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1202;
var
  eSS1202 : TEvtRmnRPPS;
  InfoComp : TInfoComplem;
  sucessVinc: TSucessaoVinc3;
  dmDevI: pcesS1202.TDMDevCollectionItem;
  ideEstabI: pcesS1202.TIdeEstabCollectionItem;
  ideADVI : TIdeAdvCollectionItem;
  itemRemun : TRubricaCollectionItem;
begin
  try
     FACBreSocial.Eventos.Clear;
     FACBreSocial.Eventos.LoadFromIni(ARQINI_S0100_S1202);
  except
    Check(FACBreSocial.Eventos.Periodicos.S1202.Count > 0, 'Não instânciou o S-1202 na lista');

    eSS1202 := FACBreSocial.Eventos.Periodicos.S1202[0].evtRmnRPPS;
    Check(eSS1202.ideEvento.indRetif = ireOriginal, 'ideEvento.indRetif diferente do valor esperado');
    Check(eSS1202.ideEvento.nrRecibo = '123' , 'ideEvento.nrRecibo diferente do valor esperado');
    Check(eSS1202.ideEvento.indApuracao = iapuMensal, 'ideEvento.indApuracao diferente do valor esperado');
    Check(eSS1202.ideEvento.perApur = '2018-05', 'ideEvento.perApur diferente do valor esperado');
    Check(eSS1202.ideEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi diferente do valor esperado');

    Check(eSS1202.ideEmpregador.TpInsc = tiCNPJ, 'ideEvento.tpInsc diferente do valor esperado');
    Check(eSS1202.ideEmpregador.NrInsc = '12345678000123', 'ideEvento.nrInsc diferente do valor esperado');

    Check(eSS1202.ideTrabalhador.cpfTrab = '12345678901', 'ideTrabalhador.cpfTrab diferente do valor esperado');

    infoComp := eSS1202.ideTrabalhador.InfoComplem;
    Check(InfoComp.nmTrab = 'Nome do Trabalhador', 'ideTrabalhador.infoComplem.nmTrab diferente do valor esperado');

    sucessVinc := infoComp.sucessaoVinc;;
    Check(sucessVinc.cnpjOrgaoAnt = '11111111111111', 'ideTrabalhador.infoComplem.sucessaoVinc.cnpjOrgaoAnt diferente do valor esperado');
    Check(sucessVinc.matricAnt = '123456', 'ideTrabalhador.infoComplem.sucessaoVinc.matricAnt diferente do valor esperado');

    dmDevI := eSS1202.dmDev[0];
    Check(dmdevI.ideDmDev = '1234', 'dmDev.ideDmDev diferente do valor esperado');
    Check(dmDevI.codCateg = 101, 'dmDev.codCategoria diferente do valor esperado');
    Check(dmDevI.indRRA = snfSim, 'dmDev.indRRA diferente do valor esperado');
    Check(dmDevI.infoRRA.qtdMesesRRA = 11, 'dmDev.infoRRA.qtdMesesRRA diferente do valor esperado');
    Check(dmDevI.infoRRA.DespProcJud.vlrDespCustas = 2200.50, 'dmDev.infoRRA.vlrDespCustas diferente do valor esperado');

    ideAdvI := dmDevI.infoRRA.ideAdv[0];
    Check(ideADVI.NrInsc = '11111111111111', 'dmDev.infoRRA.ideADV.nrInsc diferente do valor esperado');
    Check(ideADVI.vlrAdv = 350.00, 'dmDev.infoRRA.ideADV.vlrAdv diferente do valor esperado');

    ideEstabI := dmDevI.infoPerApur.ideEstab.Items[0];
    Check(ideEstabI.tpInsc = tiCNPJ, 'dmDev.infoPerApur.ideEstab.tpInsc diferente do valor esperado');
    Check(ideEstabI.nrInsc = '11111111111111', 'dmDev.infoPerApur.ideEstab.nrInsc diferente do valor esperado');

    Check(ideEstabI.remunPerApur.Items[0].matricula='1234567','dmDev.infoPerApur.ideEstab.remunPerApur.matricula diferente do valor esperado');

    itemRemun := dmDevI.infoPerApur.ideEstab.Items[0].remunPerApur.Items[0].itensRemun[0];
    Check(itemRemun.codRubr = '1000', 'dmDev.infoPerApur.IdeEstab.RemunPerApur.ItensRemun.codRubr diferente do valor esperado');
    Check(itemRemun.ideTabRubr = '1000', 'dmDev.infoPerApur.ideEstab.RemunPerApur.ItensRemun.ideTabRubr diferente do valor esperado');
    Check(itemRemun.qtdRubr = 500, 'dmDev.infoPerApur.ideEstab.RemunPerApur.ItensRemun.qtdRubr diferente do valor esperado');
    Check(itemRemun.vrRubr = 800, 'dmDev.infoPerApur.ideEstab.RemunPerApur.itensRemun.vrRubr diferente do valor esperado');

    Check(dmDevI.infoPerAnt.remunOrgSuc = tpNao, 'dmDev.infoPerAnt diferente de N');

    Check(dmDevI.infoPerAnt.idePeriodo.Items[0].perRef = '2020-05', 'dmDev.infoPerAnt.idePeriodo.perRef diferente do valor esperado');

    ideEstabI := dmDevI.infoPerAnt.idePeriodo.Items[0].ideEstab.Items[0];
    Check(ideEstabI.tpInsc = tiCNPJ, 'dmDev.infoPerAnt.idePeriodo.ideEstab.tpInsc diferente do valor esperado');
    Check(ideEstabI.nrInsc = '22222222222222', 'dmDev.infoPerAnt.idePeriodo.ideEstab.nrInsc diferente do valor esperado');

    Check(ideEstabI.remunPerAnt.Items[0].matricula = '123456', 'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.matricula diferente do valor esperado');

    itemRemun := ideEstabI.remunPerAnt.Items[0].itensRemun[0];
    Check(itemRemun.qtdRubr = 450, 'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.itensRemun.qtdRubr diferente do valor esperado');
    Check(itemRemun.fatorRubr = 11,'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.itensRemun.fatorRubr diferente do valor esperado');
    Check(itemRemun.vrRubr = 900, 'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.itensRemun.vrRubr diferente do valor esperado');
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1207_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1207.Count = 0, 'Lista de eventos S-1207 não está vazia');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1207;
var
  eSS1207: TEvtBenPrRP;
  dmDevI : pcesS1207.TDMDevCollectionItem;
  ideADVI: TIdeAdvCollectionItem;
  ideEstabI: pcesS1207.TIdeEstabCollectionItem;
  itemRemun: TRubricaCollectionItem;
  idePeriodoI: pcesS1207.TIdePeriodoCollectionItem;
begin
  try
    FACBreSocial.Eventos.LoadFromIni(ARQINI_S0100_S1207);
  except
    Check(FACBreSocial.Eventos.Periodicos.S1207.Count > 0, 'Não instanciou o S-1207 na lista');

    eSS1207 := FACBreSocial.Eventos.Periodicos.S1207[0].evtBenPrRP;

    Check(eSS1207.ideEvento.indRetif = ireOriginal, 'ideEvento.indRetif diferente do valor esperado');
    Check(eSS1207.ideEvento.NrRecibo = '123', 'ideEvento.nrRecibo diferente do valor esperado');
    Check(eSS1207.ideEvento.IndApuracao = iapuMensal, 'ideEvento.indApuracao do valor esperado');
    Check(eSS1207.ideEvento.perApur='2018-05', 'ideEvento.perApur diferente do valor esperado');

    Check(eSS1207.ideEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc diferente do valor esperado');
    Check(eSS1207.ideEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc diferente do valor esperado');

    Check(eSS1207.ideBenef.cpfBenef = '12345678901', 'ideBenef.cpfBenef diferente do valor esperado');

    dmDevI := eSS1207.dmDev[0];
    Check(dmDevI.ideDMDev = '1A', 'dmDev.ideDMDev diferente do valor esperado');
    Check(dmDevI.nrBeneficio = '123456', 'dmDev.nrBeneficio diferente do valor esperado');
    Check(dmDevI.indRRA = snfSim, 'dmDev.indRRA diferente do valor esperado');
    Check(dmDevI.infoRRA.tpProcRRA = tppAdministrativo, 'dmDev.infoRRA.tpProcRRA diferente do valor esperado');
    Check(dmDevI.infoRRA.nrProcRRA = '12345678901234567', 'dmDev.infoRRA.nrProcRRA diferente do valor esperado');
    Check(dmDevI.infoRRA.qtdMesesRRA = 5, 'dmDev.infoRRA.qtdMesesRRA diferente do valor esperado');
    Check(dmDevI.infoRRA.despProcJud.vlrDespCustas = 1000.25, 'dmDev.infoRRA.descProcJud.vlrDespCustas diferente do valor esperado');
    Check(dmDevI.infoRRA.despProcJud.vlrDespAdvogados = 250.50, 'dmDev.infoRRA.despProcJud.vlrDespAdvogados diferente do valor esperado');

    ideADVI := dmDevI.infoRRA.ideAdv[0];
    Check(ideADVI.tpInsc = tiCNPJ, 'dmDev.infoRRA.ideADV.tpInsc diferente do valor esperado');
    Check(ideADVI.nrInsc = '11111111111111', 'dmDev.infoRRA.ideADV.nrInsc diferente do valor esperado');
    Check(ideADVI.vlrAdv = 250, 'dmDev.infoRRA.ideADV.vlrADV diferente do esperado');

    ideEstabI := dmDevI.infoPerApur.ideEstab.Items[0];
    Check(ideEstabI.tpInsc = tiCNPJ, 'dmDev.infoPerAPur.ideEstab.tpInsc diferente do valor esperado');
    Check(ideEstabI.NrInsc = '22222222222222', 'dmDev.infoPerApur.ideEstab.nrInsc diferente do valor esperado');

    itemRemun := ideEstabI.itensRemun[0];
    Check(itemRemun.codRubr = '1', 'dmDev.infoPerApur.ideEstab.ItensRemun.codRubr diferente do valor esperado');
    Check(itemRemun.ideTabRubr = '100', 'dmDev.infoPerApur.ideEstab.itensRemun.ideTabRubr diferente do valor esperado');
    Check(itemRemun.qtdRubr = 50, 'dmDev.infoPerApur.ideEstab.itensRemun.qtdRubr diferente do valor esperado');
    Check(itemRemun.fatorRubr = 15, 'dmDev.infoPerApur.ideEstab.fatorRubr diferente do valor esperado');
    Check(itemRemun.vrRubr = 500, 'dmDev.infoPerApur.ideEstab.vrRubr diferente do valor esperado');

    idePeriodoI := dmDevI.infoPerAnt.idePeriodo.Items[0];
    Check(idePeriodoI.perRef = '2022-05', 'dmDev.infoPerAnt.idePeriodo.perRef diferente do valor esperado');

    ideEstabI := idePeriodoI.ideEstab.Items[0];
    Check(ideEstabI.tpInsc = tiCNPJ, 'dmDev.infoPerAnt.idePeriodo.ideEstab.tpInsc diferente do valor esperado');
    Check(ideEstabI.nrInsc = '33333333333333', 'dmDev.infoPerAnt.idePeriodo.ideEstab.nrInsc diferente do valor esperado');

    itemRemun := ideEstabI.itensRemun[0];
    Check(itemRemun.codRubr = '2', 'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.codRubr diferente do valor esperado');
    Check(itemRemun.ideTabRubr = '200', 'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.ideTabRubr diferente do valor esperado');
    Check(itemRemun.qtdRubr = 60, 'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.qtdRubr diferente do valor esperado');
    Check(itemRemun.fatorRubr = 10, 'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.fatorRubr diferente do valor esperado');
    Check(itemRemun.vrRubr = 300, 'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.vrRubr diferente do valor esperado');


  end;
end;

end.

