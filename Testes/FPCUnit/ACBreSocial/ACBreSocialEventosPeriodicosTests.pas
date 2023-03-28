unit ACBreSocialEventosPeriodicosTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocialTestsConsts, ACBreSocial,
  pcesS1202, pcesS1207, pcesS1210, pcesS1260, pcesCommon, pcesConversaoeSocial, ACBrDFeException,
  ACBrUtil.DateTime;

type

  { TACBreSocialEventosPeriodicosTest }

  TACBreSocialEventosPeriodicosTest = class(TTestCase)
    private
      FACBreSocial : TACBreSocial;
      procedure ES1202Tests;
      procedure ES1207Tests;
      procedure ES1210Tests;
      procedure ES1260Tests;
    public
      procedure Setup;override;
      procedure TearDown;override;
    published
      procedure ACBreSocialEventosPeriodicosS1200_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1200_vS0100;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1200_vS0101;
      procedure ACBreSocialEventosPeriodicosS1202_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1202_vS0100;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1202_vS0101;
      procedure ACBreSocialEventosPeriodicosS1207_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1207_vS0100;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1207_vS0101;
      procedure ACBreSocialEventosPeriodicosS1210_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1210_vS0100;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1210_vS0101;
      procedure ACBreSocialEventosPeriodicosS1260_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1260_vS0100;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1260_vS0101;
  end;

implementation

{ TACBreSocialEventosPeriodicosTest }

procedure TACBreSocialEventosPeriodicosTest.ES1202Tests;
var
  eSS1202 : TEvtRmnRPPS;
  InfoComp : TInfoComplem;
  sucessVinc: TSucessaoVinc3;
  dmDevI: pcesS1202.TDMDevCollectionItem;
  ideEstabI: pcesS1202.TIdeEstabCollectionItem;
  ideADVI : TIdeAdvCollectionItem;
  itemRemun : TRubricaCollectionItem;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1202.Count > 0, 'Não instânciou o S-1202 na lista');

  eSS1202 := FACBreSocial.Eventos.Periodicos.S1202[0].evtRmnRPPS;
  Check(eSS1202.ideEvento.indRetif = ireOriginal,
        'ideEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eSS1202.ideEvento.indRetif));

  Check(eSS1202.ideEvento.nrRecibo = '123' ,
        'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eSS1202.ideEvento.nrRecibo);

  Check(eSS1202.ideEvento.indApuracao = iapuMensal,
        'ideEvento.indApuracao | Valor esperado:1 | Valor recebido:'+eSIndApuracaoToStr(eSS1202.ideEvento.indApuracao));

  Check(eSS1202.ideEvento.perApur = '2018-05',
        'ideEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eSS1202.ideEvento.perApur);

  Check(eSS1202.ideEvento.ProcEmi = peAplicEmpregador,
        'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eSS1202.ideEvento.ProcEmi));

  Check(eSS1202.ideEmpregador.TpInsc = tiCNPJ,
        'ideEvento.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eSS1202.ideEmpregador.TpInsc));

  Check(eSS1202.ideEmpregador.NrInsc = '12345678000123',
        'ideEvento.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eSS1202.ideEmpregador.NrInsc);

  Check(eSS1202.ideTrabalhador.cpfTrab = '12345678901', 'ideTrabalhador.cpfTrab diferente do valor esperado');

  infoComp := eSS1202.ideTrabalhador.InfoComplem;
  Check(InfoComp.nmTrab = 'Nome do traballhador',
        'ideTrabalhador.infoComplem.nmTrab | Valor esperado:Nome do traballhador | Valor recebido:'+InfoComp.nmTrab);

  sucessVinc := infoComp.sucessaoVinc;
  Check(sucessVinc.cnpjOrgaoAnt = '11111111111111',
        'ideTrabalhador.infoComplem.sucessaoVinc.cnpjOrgaoAnt | Valor esperado:11111111111111 | Valor recebido:'+sucessVinc.cnpjOrgaoAnt);

  Check(sucessVinc.matricAnt = '123456',
        'ideTrabalhador.infoComplem.sucessaoVinc.matricAnt | Valor esperado:123456 | Valor recebido:'+sucessVinc.matricAnt);

  dmDevI := eSS1202.dmDev[0];
  Check(dmdevI.ideDmDev = '1234', 'dmDev.ideDmDev | Valor esperado:1234 | Valor recebido:'+dmDevI.ideDmDev);
  Check(dmDevI.codCateg = 101, 'dmDev.codCategoria | Valor esperado:101 | Valor recebido:'+IntToStr(dmDevI.codCateg));

  if(FACBreSocial.Configuracoes.Geral.VersaoDF = veS01_01_00)then
  begin
    Check(dmDevI.indRRA = snfSim, 'dmDev.indRRA | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(dmDevI.indRRA));
    Check(dmDevI.infoRRA.qtdMesesRRA = 11, 'dmDev.infoRRA.qtdMesesRRA | Valor esperado:11 | Valor recebido:'+FormatFloat('00', dmDevI.infoRRA.qtdMesesRRA));
    Check(dmDevI.infoRRA.DespProcJud.vlrDespCustas = 2200.50, 'dmDev.infoRRA.vlrDespCustas | Valor esperado:2200,50 | Valor recebido:'+FormatFloat('0,000.00', dmDevI.infoRRA.DespProcJud.vlrDespCustas));

    ideAdvI := dmDevI.infoRRA.ideAdv[0];
    Check(ideADVI.NrInsc = '11111111111111', 'dmDev.infoRRA.ideADV.nrInsc | Valor esperado:11111111111111 | Valor recebido:'+ideADVI.NrInsc);
    Check(ideADVI.vlrAdv = 350.00, 'dmDev.infoRRA.ideADV.vlrAdv | Valor esperado:350,00 | Valor recebido:'+FormatFloat('0.000,00', ideADVI.vlrAdv));
  end;

  ideEstabI := dmDevI.infoPerApur.ideEstab.Items[0];
  Check(ideEstabI.tpInsc = tiCNPJ, 'dmDev.infoPerApur.ideEstab.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(ideEstabI.tpInsc));
  Check(ideEstabI.nrInsc = '11111111111111', 'dmDev.infoPerApur.ideEstab.nrInsc | Valor esperado:11111111111111 | Valor recebido:'+ideEstabI.nrInsc);

  Check(ideEstabI.remunPerApur.Items[0].matricula='1234567',
        'dmDev.infoPerApur.ideEstab.remunPerApur.matricula | Valor esperado:1234567 | Valor recebido:'+ideEstabI.remunPerApur.Items[0].matricula);

  itemRemun := dmDevI.infoPerApur.ideEstab.Items[0].remunPerApur.Items[0].itensRemun[0];
  Check(itemRemun.codRubr = '1000',
        'dmDev.infoPerApur.IdeEstab.RemunPerApur.ItensRemun.codRubr | Valor esperado:1000 | Valor recebido:'+itemRemun.codRubr);

  Check(itemRemun.ideTabRubr = '1000',
        'dmDev.infoPerApur.ideEstab.RemunPerApur.ItensRemun.ideTabRubr | Valor esperado:1000 | Valor recebido:'+itemRemun.ideTabRubr);
  Check(itemRemun.qtdRubr = 500,
        'dmDev.infoPerApur.ideEstab.RemunPerApur.ItensRemun.qtdRubr | Valor esperado:500 | Valor recebido:'+FormatFloat('000', itemRemun.qtdRubr));

  Check(itemRemun.vrRubr = 800,
        'dmDev.infoPerApur.ideEstab.RemunPerApur.itensRemun.vrRubr | Valor esperado:800 | Valor recebido:'+FormatFloat('000',itemRemun.vrRubr));

  Check(dmDevI.infoPerAnt.remunOrgSuc = tpNao, 'dmDev.infoPerAnt | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(dmDevI.infoPerAnt.remunOrgSuc));

  Check(dmDevI.infoPerAnt.idePeriodo.Items[0].perRef = '2020-05',
        'dmDev.infoPerAnt.idePeriodo.perRef | Valor esperado:2020-05 | Valor recebido:'+dmDevI.infoPerAnt.idePeriodo.Items[0].perRef);

  ideEstabI := dmDevI.infoPerAnt.idePeriodo.Items[0].ideEstab.Items[0];
  Check(ideEstabI.tpInsc = tiCNPJ,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(ideEstabI.tpInsc));

  Check(ideEstabI.nrInsc = '22222222222222',
        'dmDev.infoPerAnt.idePeriodo.ideEstab.nrInsc | Valor esperado:22222222222222 | Valor recebido:'+ideEstabI.nrInsc);

  Check(ideEstabI.remunPerAnt.Items[0].matricula = '123456',
        'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.matricula | Valor esperado:123456 | Valor recebido:'+ideEstabI.remunPerAnt.Items[0].matricula);

  itemRemun := ideEstabI.remunPerAnt.Items[0].itensRemun[0];
  Check(itemRemun.qtdRubr = 450,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.itensRemun.qtdRubr | Valor esperado:450 | Valor recebido:'+FormatFloat('000', itemRemun.qtdRubr));

  Check(itemRemun.fatorRubr = 11,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.itensRemun.fatorRubr | Valor esperado:11 | Valor recebido:'+FormatFloat('000',itemRemun.fatorRubr));

  Check(itemRemun.vrRubr = 900,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.remunPerAnt.itensRemun.vrRubr | Valor esperado:900 | Valor recebido:'+FormatFloat('000', itemRemun.vrRubr));
end;

procedure TACBreSocialEventosPeriodicosTest.ES1207Tests;
var
  eSS1207: TEvtBenPrRP;
  dmDevI : pcesS1207.TDMDevCollectionItem;
  ideADVI: TIdeAdvCollectionItem;
  ideEstabI: pcesS1207.TIdeEstabCollectionItem;
  itemRemun: TRubricaCollectionItem;
  idePeriodoI: pcesS1207.TIdePeriodoCollectionItem;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1207.Count > 0, 'Não instanciou o S-1207 na lista');

  eSS1207 := FACBreSocial.Eventos.Periodicos.S1207[0].evtBenPrRP;

  Check(eSS1207.ideEvento.indRetif = ireOriginal,
        'ideEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eSS1207.ideEvento.indRetif));

  Check(eSS1207.ideEvento.NrRecibo = '123',
        'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eSS1207.ideEvento.NrRecibo);

  Check(eSS1207.ideEvento.IndApuracao = iapuMensal,
        'ideEvento.indApuracao | Valor esperado:1 | Valor recebido:'+eSIndApuracaoToStr(eSS1207.ideEvento.IndApuracao));

  Check(eSS1207.ideEvento.perApur='2018-05',
        'ideEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eSS1207.ideEvento.perApur);

  Check(eSS1207.ideEmpregador.TpInsc = tiCNPJ,
        'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eSS1207.ideEmpregador.tpInsc));

  Check(eSS1207.ideEmpregador.NrInsc = '12345678000123',
        'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eSS1207.ideEmpregador.NrInsc);

  Check(eSS1207.ideBenef.cpfBenef = '12345678901',
        'ideBenef.cpfBenef | Valor esperado:12345678901 | Valor recebido:'+eSS1207.ideBenef.cpfBenef);

  dmDevI := eSS1207.dmDev[0];
  Check(dmDevI.ideDMDev = '1A', 'dmDev.ideDMDev | Valor esperado:1A | Valor recebido:'+dmDevI.ideDMDev);
  Check(dmDevI.nrBeneficio = '123456', 'dmDev.nrBeneficio | Valor esperado:123456 | Valor recebido:'+dmDevI.nrBeneficio);

  if(FACBreSocial.Configuracoes.Geral.VersaoDF = veS01_01_00)then
  begin
    Check(dmDevI.indRRA = snfSim, 'dmDev.indRRA | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(dmDevI.indRRA));
    Check(dmDevI.infoRRA.tpProcRRA = tppAdministrativo,
          'dmDev.infoRRA.tpProcRRA | Valor esperado:1 | Valor recebido:'+eSTpProcRRAToStr(dmDevI.infoRRA.tpProcRRA));
    Check(dmDevI.infoRRA.nrProcRRA = '12345678901234567',
          'dmDev.infoRRA.nrProcRRA | Valor esperado:12345678901234567 | Valor recebido:'+dmDevI.infoRRA.nrProcRRA);
    Check(dmDevI.infoRRA.qtdMesesRRA = 5,
          'dmDev.infoRRA.qtdMesesRRA | Vaolor esperado:5 | Valor recebido:'+FormatFloat('00', dmDevI.infoRRA.qtdMesesRRA));
    Check(dmDevI.infoRRA.despProcJud.vlrDespCustas = 1000.25,
          'dmDev.infoRRA.descProcJud.vlrDespCustas | Valor esperado:1000,25 | Valor recebido:'+FormatFloat('0.000,00', dmDevI.infoRRA.despProcJud.vlrDespCustas));
    Check(dmDevI.infoRRA.despProcJud.vlrDespAdvogados = 250.50,
          'dmDev.infoRRA.despProcJud.vlrDespAdvogados | Valor esperado:250,50 | Valor recebido:'+FormatFloat('0,000.00', dmDevI.infoRRA.despProcJud.vlrDespAdvogados));

    ideADVI := dmDevI.infoRRA.ideAdv[0];
    Check(ideADVI.tpInsc = tiCNPJ,
          'dmDev.infoRRA.ideADV.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(ideADVI.tpInsc));
    Check(ideADVI.nrInsc = '11111111111111',
          'dmDev.infoRRA.ideADV.nrInsc | Valor esperado:11111111111111 | Valor recebido:'+ideADVI.nrInsc);
    Check(ideADVI.vlrAdv = 250,
          'dmDev.infoRRA.ideADV.vlrADV | Valor esperado:250 | Valor recebido:'+FormatFloat('000', ideADVI.vlrAdv));
  end;

  ideEstabI := dmDevI.infoPerApur.ideEstab.Items[0];
  Check(ideEstabI.tpInsc = tiCNPJ,
        'dmDev.infoPerAPur.ideEstab.tpInsc | Valor esperado:1 | Valor recebido:'+eStpInscricaoToStr(ideEstabI.tpInsc));
  Check(ideEstabI.NrInsc = '22222222222222',
        'dmDev.infoPerApur.ideEstab.nrInsc | Valor esperado:22222222222222 | Valor recebido:'+ideEstabI.NrInsc);

  itemRemun := ideEstabI.itensRemun[0];
  Check(itemRemun.codRubr = '1',
        'dmDev.infoPerApur.ideEstab.ItensRemun.codRubr | Valor esperado:1 | Valor recebido:'+itemRemun.codRubr);
  Check(itemRemun.ideTabRubr = '100',
        'dmDev.infoPerApur.ideEstab.itensRemun.ideTabRubr | Valor esperado:100 | Valor recebido:'+itemRemun.ideTabRubr);
  Check(itemRemun.qtdRubr = 50,
        'dmDev.infoPerApur.ideEstab.itensRemun.qtdRubr | Valor esperado:50 | Valor recebido:'+FormatFloat('00', itemRemun.qtdRubr));
  Check(itemRemun.fatorRubr = 15,
        'dmDev.infoPerApur.ideEstab.fatorRubr | Valor esperado:15 | Valor recebido:'+FormatFloat('00', itemRemun.fatorRubr));
  Check(itemRemun.vrRubr = 500,
        'dmDev.infoPerApur.ideEstab.vrRubr | Valor esperado:500 | Valor recebido:'+FormatFloat('0,000.00', itemRemun.vrRubr));

  idePeriodoI := dmDevI.infoPerAnt.idePeriodo.Items[0];
  Check(idePeriodoI.perRef = '2022-05',
        'dmDev.infoPerAnt.idePeriodo.perRef | Valor esperado:2022-05 | Valor recebido:'+idePeriodoI.perRef);

  ideEstabI := idePeriodoI.ideEstab.Items[0];
  Check(ideEstabI.tpInsc = tiCNPJ,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.tpInsc | Valor esperado:1 | Valor recebido:'+eStpInscricaoToStr(ideEstabI.tpInsc));
  Check(ideEstabI.nrInsc = '33333333333333',
        'dmDev.infoPerAnt.idePeriodo.ideEstab.nrInsc | Valor esperado:33333333333333 | Valor recebido:'+ideEstabI.nrInsc);

  itemRemun := ideEstabI.itensRemun[0];
  Check(itemRemun.codRubr = '2',
        'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.codRubr | Valor esperado:2 | Valor recebido:'+itemRemun.codRubr);
  Check(itemRemun.ideTabRubr = '200',
        'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.ideTabRubr | Valor esperado:200 | Valor recebido:'+itemRemun.ideTabRubr);

  Check(itemRemun.qtdRubr = 60,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.qtdRubr | Valor esperado:60 | Valor recebido:'+FormatFloat('00', itemRemun.qtdRubr));
  Check(itemRemun.fatorRubr = 10,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.fatorRubr | Valor esperado:10 | Valor recebido:'+FormatFloat('00', itemRemun.fatorRubr));
  Check(itemRemun.vrRubr = 300,
        'dmDev.infoPerAnt.idePeriodo.ideEstab.itensRemun.vrRubr | Valor esperado:300 | Valor recebido:'+FormatFloat('000', itemRemun.vrRubr));

end;

procedure TACBreSocialEventosPeriodicosTest.ES1210Tests;
var
  eS1210: TEvtPgtos;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1210.Count > 0, 'Não instanciou o S-1210 na lista!');

  eS1210 := FACBreSocial.Eventos.Periodicos.S1210[0].evtPgtos;

  Check(eS1210.IdeEvento.indRetif = ireOriginal ,
        'IdeEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eS1210.ideEvento.indRetif));

  Check(eS1210.ideEvento.NrRecibo = '123',
        'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eS1210.ideEvento.nrRecibo);

  Check(eS1210.ideEvento.IndApuracao = iapuMensal,
        'ideEvento.indApuracao | Valor esperado:1 | Valor recebido:'+eSIndApuracaoToStr(eS1210.ideEvento.IndApuracao));

  Check(eS1210.ideEvento.perApur = '2018-05',
        'ideEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eS1210.ideEvento.perApur);

  Check(eS1210.ideEvento.indGuia='1',
        'ideEvento.indGuia | Valor esperado:1 | Valor recebido:'+eS1210.ideEvento.indGuia);

  Check(eS1210.IdeEvento.ProcEmi = peAplicEmpregador,
        'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1210.ideEvento.procEmi));

  Check(eS1210.ideEvento.VerProc='1.00',
        'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1210.ideEvento.verProc);

  Check(eS1210.IdeEmpregador.TpInsc = tiCNPJ,
        'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1210.ideEmpregador.tpInsc));

  Check(eS1210.ideEmpregador.NrInsc = '12345678000123',
        'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1210.ideEmpregador.nrInsc);

  Check(eS1210.IdeBenef.CpfBenef = '12345678901',
        'ideBenef.cpfBenef | Valor esperado:12345678901 | Valor recebido:'+eS1210.ideBenef.CpfBenef);

  Check(eS1210.IdeBenef.InfoPgto.Items[0].DtPgto = StoD('20180503000000'),
        'ideBenef.infoPgto.dtPgto | Valor esperado:03/05/2018 | Valor recebido:'+FormatDateBr(eS1210.ideBenef.InfoPgto.Items[0].DtPgto));

  Check(eS1210.IdeBenef.infoPgto.Items[0].TpPgto = tpPgtoRemun1200,
        'ideBenef.infoPgto.tpPgto | Valor esperado:1 | Valor recebido:'+eSTpTpPgtoToStr(eS1210.ideBenef.infoPgto.Items[0].TpPgto));

  Check(eS1210.ideBenef.InfoPgto.Items[0].perRef = '2021-05',
        'ideBenef.infoPgto.perRef | Valor esperado:2021-05 | Valor recebido:'+eS1210.ideBenef.InfoPgto.Items[0].perRef);

  Check(eS1210.IdeBenef.InfoPgto.Items[0].ideDmDev='1',
        'ideBenef.infoPgto.ideDmDev | Valor esperado:1 | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].ideDmDev);

  Check(eS1210.ideBenef.infoPgto.Items[0].vrLiq=0,
        'ideBenef.infoPgto.VrLiq | Valor esperado:0 | Valor recebido:'+FormatFloat('#,###.##', eS1210.ideBenef.InfoPgto.Items[0].vrLiq));

  Check(eS1210.ideBenef.InfoPgto.Items[0].paisResidExt='105',
        'ideBenef.infoPgto.paisResidExt | Valor esperado:105 | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].paisResidExt);

  Check(eS1210.ideBenef.InfoPgto.Items[0].infoPgtoExt.indNIF = infPaisnaoNIF,
        'ideBenef.infoPgto.infoPgtoExt.indNIF | Valor esperado:3 | Valor recebido:'+eSIndNIFToStr(eS1210.ideBenef.InfoPgto.Items[0].infoPgtoExt.indNIF));

  Check(eS1210.ideBenef.InfoPgto.Items[0].infoPgtoExt.nifBenef = '01',
        'ideBenef.infoPgto.infoPgtoExt.nifBenf | Valor esperado:01 | Valor recebiddo:'+eS1210.ideBenef.InfoPgto.Items[0].infoPgtoExt.nifBenef);

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.frmTribut = 10,
        'ideBenef.infoPgto.infoPgtoExt.frmTribut | Valor esperado:10 | Valor recebido:'+IntToStr(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.frmTribut));

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endDscLograd = 'descricao do logradouro',
        'ideBenef.infoPgto.infoPgtoExt.endExt.endDscLograd | Valor esperado:descricao do logradouro | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endDscLograd);

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endNrLograd = '1',
        'ideBenef.infoPgto.infoPgtoExt.endExt.endNrLograd | Valor esperado:1 | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endNrLograd);

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endComplem='complemento do logradouro',
        'ideBenef.infoPgto.infoPgtoExt.endExt.endComplem | Valor esperado:complemento do logradouro | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endComplem);

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endBairro='Bairro/distrito',
        'ideBenef.infoPgto.infoPgtoExt.endExt.endBairro | Valor esperado:Bairro/distrito | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endBairro);

  Check(eS1210.ideBenef.InfoPgto.Items[0].infoPgtoExt.endExt.endCidade='Nome Cidade',
        'ideBenef.infoPgto.infoPgtoExt.endExt.endCidade | Valor esperado:Nome Cidade | Valor recebido:'+eS1210.ideBenef.InfoPgto.Items[0].infoPgtoExt.endExt.endCidade);

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endEstado='Provincia/Estado',
        'ideBenef.infoPgto.infoPgtoExt.endExt.endEstado | Valor esperado:Provincia/Estado | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endEstado);

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endCodPostal='12345678',
        'ideBenef.infoPgto.infoPgtoExt.endExt.endCodPostal | Valor esperado:12345678 | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.endCodPostal);

  Check(eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.telef='12345678',
        'ideBenef.infoPgto.infoPgtoExt.endExt.telef | Valor esperado:12345678 | Valor recebido:'+eS1210.ideBenef.infoPgto.Items[0].infoPgtoExt.endExt.telef);


end;

procedure TACBreSocialEventosPeriodicosTest.ES1260Tests;
var
  eS1260: TEvtComProd;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1260.Count > 0, 'Não instanciou o S-1260 na lista!');

  AssertEquals('Não instanciou o S-1260 na lista!', 1, FACBreSocial.Eventos.Periodicos.S1260.Count);

  eS1260 := FACBreSocial.Eventos.Periodicos.S1260[0].EvtComProd;


end;

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

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1200_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1200.Count = 0, 'Lista de eventos S-1200 não está vazia!');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1200_vS0100;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0100_S1200);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
//      ES1200Tests;
      Fail('Precisa implementar os testes do S-1200');
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1200_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S1200);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
//      ES1200Tests;
      Fail('Precisa implementar os testes do S-1200');
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1202_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1202.Count = 0, 'Lista de eventos S-1202 não está vazia');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1202_vS0100;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0100_S1202);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1202Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1202_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S1202);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1202Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1207_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1207.Count = 0, 'Lista de eventos S-1207 não está vazia');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1207_vS0100;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S1207);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1207Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1207_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1207);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1207Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1210_Create_ListaVazia;
begin
  CheckTrue(FACBreSocial.Eventos.Periodicos.S1210.Count = 0, 'Lista de eventos S-1210 não está vazia');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1210_vS0100;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF :=  veS01_00_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S1210);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1210Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1210_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF :=  veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1210);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES1210Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1260_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1260.Count = 0, 'Lista de eventos S-1260 não está vazia!');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1260_vS0100;
begin
  try
    FACBreSocial.Eventos.Periodicos.S1260.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S1260);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1260Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1260_vS0101;
begin
  try
    FACBreSocial.Eventos.Periodicos.S1260.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1260);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1260Tests;
    end;
  end;
end;

end.

