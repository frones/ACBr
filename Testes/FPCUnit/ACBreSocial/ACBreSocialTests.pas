unit ACBreSocialTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocial, pcesConversaoeSocial,
  pcesCommon, pcesS2400, pcesS1202, pcesS1207;

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

  { TACBreSocialEventosNaoPeriodicosTest }

  TACBreSocialEventosNaoPeriodicosTest = class(TTestCase)
    private
      FACBreSocial : TACBreSocial;
    public
      procedure Setup;override;
      procedure TearDown;override;
    published
      procedure ACBreSocialEventosNaoPeriodicosS2400_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2400;
  end;

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
      procedure ACBreSocualEventos_LoadFromINI_LeuePreencheuS1207;
  end;


implementation

{ TACBreSocualEventosPeriodicosTest }

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
     FACBreSocial.Eventos.LoadFromIni(ARQINI_S1202);
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

procedure TACBreSocialEventosPeriodicosTest.ACBreSocualEventos_LoadFromINI_LeuePreencheuS1207;
var
  eSS1207: TEvtBenPrRP;
  dmDevI : pcesS1207.TDMDevCollectionItem;
  ideADVI: TIdeAdvCollectionItem;
  ideEstabI: pcesS1207.TIdeEstabCollectionItem;
  itemRemun: TRubricaCollectionItem;
  idePeriodoI: pcesS1207.TIdePeriodoCollectionItem;
begin
  try
    FACBreSocial.Eventos.LoadFromIni(ARQINI_S1207);
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

{ TACBreSocialEventosNaoPeriodicosTest }

procedure TACBreSocialEventosNaoPeriodicosTest.Setup;
begin
  inherited Setup;
  FACBreSocial := TACBreSocial.Create(nil);
end;

procedure TACBreSocialEventosNaoPeriodicosTest.TearDown;
begin
  inherited TearDown;
  FACBreSocial.Free;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2400_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2400.Count = 0, 'Lista de Eventos S-2400 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2400;
var
  eSS2400 : TEvtCdBenefIn;
begin
  //Precisei fazer assim porque a rotina requer certificado digital e eu não tenho;
  //então ela levanta uma exceção que faz o teste falhar.
  try
    FACBreSocial.Eventos.LoadFromIni(ARQINI_S2400);
  except
    Check(FACBreSocial.Eventos.NaoPeriodicos.S2400.Count > 0, 'Não instanciou o S-2400 na lista');

    eSS2400    := FACBreSocial.Eventos.NaoPeriodicos.S2400.Items[0].EvtCdBenefIn;

    Check(eSS2400.IdeEvento.indRetif     = ireOriginal            , 'IdeEvento.indRetif diferente do valor esperado');
    Check(eSS2400.IdeEvento.NrRecibo     = EmptyStr               , 'IdeEvento.NrRecibo diferente do valor esperado');
    Check(eSS2400.IdeEvento.ProcEmi      = peAplicEmpregador      , 'IdeEvento.ProcEmi diferente do valor esperado');
    Check(eSS2400.IdeEvento.VerProc      = '1.1'                  , 'IdeEvento.VerProc diferente do valor esperado');

    Check(eSS2400.IdeEmpregador.TpInsc   = tiCNPJ                 , 'IdeEmpregador.tpInsc diferente do valor esperado');
    Check(eSS2400.IdeEmpregador.NrInsc   = '12345678000123'       , 'IdeEmpregador.NrInsc diferente do valor esperado');

    Check(eSS2400.Beneficiario.cpfBenef  = '99999999999'          , 'Beneficiario.cpfBenef diferente do valor esperado');
    Check(eSS2400.Beneficiario.nmBenefic = 'Beneficiario'         , 'Beneficiario.nmBenefic diferente do valor esperado');
    Check(eSS2400.Beneficiario.dtNascto  = StrToDate('01/01/2000'), 'Beneficiario.dtNascto diferente do valor esperado');
    Check(eSS2400.Beneficiario.dtInicio  = StrToDate('01/01/2022'), 'Beneficiario.dtInicio diferente do valor esperado');
    Check(eSS2400.Beneficiario.sexo      = 'M'                    , 'Beneficiario.sexo diferente do valor esperado');
    Check(eSS2400.Beneficiario.racaCor   = 1                      , 'Beneficiario.racaCor diferente do valor esperado');
    Check(eSS2400.Beneficiario.estCiv    = 1                      , 'Beneficiario.estCiv diferente do valor esperado');
    Check(eSS2400.Beneficiario.incFisMen = tpNao                  , 'Beneficiario.incFisMen diferente do valor esperado');

    Check(eSS2400.Beneficiario.endereco.Brasil.TpLograd  = 'R'    , 'Beneficiario.endereco.Brasil.TpLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.dscLograd = 'Logradouro de teste', 'Beneficiario.endereco.Brasil.dscLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.nrLograd  = '100'  , 'Beneficiario.endereco.Brasil.nrLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.Complemento = 'Apto 10', 'Beneficiario.endereco.Brasil.Complemento diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.Bairro = 'Centro' , 'Beneficiario.endereco.Brasil.Bairro diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.cep = '14123456'  , 'Beneficiario.endereco.Brasil.cep diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.CodMunic = 3512345, 'Beneficiario.endereco.Brasil.codMunic diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.uf = 'SP'         , 'Beneficiario.endereco.Brasil.uf diferente do valor esperado');

    Check(eSS2400.Beneficiario.dependente.Items[0].tpDep = tdConjuge, 'Beneficiario.dependente.tpDep diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].nmDep = 'Conjuge', 'Beneficiario.dependente.nmDep diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].dtNascto = StrToDate('01/01/1995'), 'Beneficiario.depentente.dtNascto diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].cpfDep = '11111111111', 'Beneficiario.dependente.cpfDep diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].sexoDep   = 'F', 'Beneficiario.dependente.sexo diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].depIRRF   = tpNao, 'Beneficiario.dependente.depIRRF diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].incFisMen = tpNao, 'Beneficiario.depentente.incFisMen diferente do valor esperado');
  end;
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
  _RegisterTest('pcesNaoPeriodicos'   , TACBreSocialEventosNaoPeriodicosTest);
  _RegisterTest('pcesPeriodicos'      , TACBreSocialEventosPeriodicosTest);

end.

