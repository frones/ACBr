unit ACBreSocialEventosPeriodicosTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocialTestsConsts, ACBreSocial,
  pcesS1200, pcesS1202, pcesS1207, pcesS1210, pcesS1260, pcesS1270, pcesS1280,
  pcesS1298, pcesS1299,
  pcesCommon,
  pcesConversaoeSocial, ACBrDFeException,
  ACBreSocialEventos,
  ACBrUtil.DateTime;

type

  { TACBreSocialEventosPeriodicosTest }

  TACBreSocialEventosPeriodicosTest = class(TTestCase)
    private
      FACBreSocial : TACBreSocial;
      procedure ES1200Tests;
      procedure ES1202Tests;
      procedure ES1207Tests;
      procedure ES1210Tests;
      procedure ES1260Tests;
      procedure ES1270Tests;
      procedure ES1280Tests;
      procedure ES1298Tests;
      procedure ES1299Tests;
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
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1260_vS0101;
      procedure ACBreSocialEventosPeriodicosS1270_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1270_vS0101;
      procedure ACBreSocialEventosPeriodicosS1280_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1280_vS0101;
      procedure ACBreSocialEventosPeriodicosS1298_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1298_vS0101;
      procedure ACBreSocialEventosPeriodicosS1299_Create_ListaVazia;
      procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1299_vS0101;
  end;

implementation

{ TACBreSocialEventosPeriodicosTest }

procedure TACBreSocialEventosPeriodicosTest.ES1200Tests;
var
  eSS1200 : TevtRemun;
  infoMV : TInfoMV;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1200.Count > 0, 'Não instânciou o S-1200 na lista');

  eSS1200 := FACBreSocial.Eventos.Periodicos.S1200[0].evtRemun;
  Check(eSS1200.ideEvento.indRetif = ireOriginal,
        'ideEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eSS1200.ideEvento.indRetif));

  Check(eSS1200.ideEvento.nrRecibo = '123' ,
        'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eSS1200.ideEvento.nrRecibo);

  Check(eSS1200.ideEvento.indApuracao = iapuMensal,
        'ideEvento.indApuracao | Valor esperado:1 | Valor recebido:'+eSIndApuracaoToStr(eSS1200.ideEvento.indApuracao));

  Check(eSS1200.ideEvento.perApur = '2018-05',
        'ideEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eSS1200.ideEvento.perApur);

  Check(eSS1200.ideEvento.indGuia = '1',
        'ideEvento.indGuia | Valor esperado:1 | Valor recebido:'+ eSS1200.ideEvento.indGuia);

  Check(eSS1200.ideEvento.ProcEmi = peAplicEmpregador,
        'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eSS1200.ideEvento.ProcEmi));

  Check(eSS1200.ideEvento.VerProc = '1.00',
        'ideEvento.VerProc | Valor esperado:1.00 | Valor recebido:'+eSS1200.ideEvento.VerProc);

  Check(eSS1200.ideEmpregador.TpInsc = tiCNPJ,
        'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eSS1200.ideEmpregador.TpInsc));

  Check(eSS1200.ideEmpregador.NrInsc = '12345678000123',
        'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eSS1200.ideEmpregador.NrInsc);

  Check(eSS1200.ideTrabalhador.cpfTrab = '12345678901',
        'ideTrabalhador.cpfTrab | Valor esperado: 12345678901 | Valor recebido: '+eSS1200.ideTrabalhador.cpfTrab);

  infoMV := eSS1200.ideTrabalhador.infoMV;

  Check(infoMV.indMV = imvDescontadaempregador,
        'ideTrabalhador.infoMV | Valor esperado: 1 | Valor recebido: '+eSIndMVToStr(infoMV.indMV));

  Check(infoMV.remunOutrEmpr.Items[0].tpInsc = tiCNPJ,
        'ideTrabalhador.infoMV.remunOutrEmpr.tpInsc | Valor esperado: 1 | Valor recebido: '+eSTpInscricaoToStr(infoMV.remunOutrEmpr.Items[0].tpInsc));

  Check(infoMV.remunOutrEmpr.Items[0].nrInsc = '12345678000123',
        'ideTrabalhador.infoMV.remunOutrEmpr.nrInsc | Valor esperado: 12345678000123 | Valor recebido: '+infoMV.remunOutrEmpr.Items[0].nrInsc);

  Check(infoMV.remunOutrEmpr.Items[0].codCateg = 222,
        'ideTrabalhador.infoMV.remunOutrEmpr.codCateg | Valor esperado: 222 | Valor recebido: '+IntToStr(infoMV.remunOutrEmpr.Items[0].codCateg));

  Check(infoMV.remunOutrEmpr.Items[0].vlrRemunOE = 100,
        'ideTrabalhador.infoMV.remunOutrEmpr.vlrRemunOE | Valor esperado: 100 | Valor recebido: '+FloatToStr(infoMV.remunOutrEmpr.Items[0].vlrRemunOE));

  Check(eSS1200.ideTrabalhador.infoComplem.nmTrab = 'Nome',
        'ideTrabalhador.infoComplem.nmTrab | Valor esperado: ''Nome'' | Valor recebido: '+eSS1200.ideTrabalhador.infoComplem.nmTrab);

  Check(eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.tpInsc = tiCNPJ,
        'ideTrabalhador.infoComplem.sucessaoVinc.tpInsc | Valor esperado: 1 | Valor recebido: '+eSTpInscricaoToStr(eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.tpInsc));

  Check(eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.nrInsc = '12345678000123',
        'ideTrabalhador.infoComplem.sucessaoVinc.nrInsc | Valor esperado: 12345678000123 | Valor recebido: '+eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.nrInsc);

  Check(eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.matricAnt = '123',
        'ideTrabalhador.infoComplem.sucessaoVinc.nrInsc | Valor esperado: 123 | Valor recebido: '+eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.matricAnt);

  Check(eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.dtAdm = StrToDateTime('10/04/2018'),
        'ideTrabalhador.infoComplem.sucessaoVinc.nrInsc | Valor esperado: 10/04/2018 | Valor recebido: '+DateTimeToStr(eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.dtAdm));

  Check(eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.observacao = 'observacao do emprego anterior',
        'ideTrabalhador.infoComplem.sucessaoVinc.observacao | Valor esperado: observacao do emprego anterior | Valor recebido: '+eSS1200.ideTrabalhador.infoComplem.sucessaoVinc.observacao);

  Check(eSS1200.ideTrabalhador.procJudTrab.Items[0].tpTrib = tptIRRF,
        'ideTrabalhador.procJudTrab.tpTrib | Valor esperado: 1 | Valor recebido: '+eSTpTributoToStr(eSS1200.ideTrabalhador.procJudTrab.Items[0].tpTrib));

  Check(eSS1200.ideTrabalhador.procJudTrab.Items[0].nrProcJud = '123',
        'ideTrabalhador.procJudTrab.nrProcJud | Valor esperado: 123 | Valor recebido: '+eSS1200.ideTrabalhador.procJudTrab.Items[0].nrProcJud);

  Check(eSS1200.ideTrabalhador.procJudTrab.Items[0].codSusp = '456',
        'ideTrabalhador.procJudTrab.codSusp | Valor esperado: 456 | Valor recebido: '+eSS1200.ideTrabalhador.procJudTrab.Items[0].codSusp);

  Check(eSS1200.ideTrabalhador.infoInterm.Items[0].dia = 28,
        'ideTrabalhador.infoInterm.dia | Valor esperado: 28 | Valor recebido: '+IntToStr(eSS1200.ideTrabalhador.infoInterm.Items[0].dia));

  Check(eSS1200.dmDev.Items[0].ideDmDev = '123',
        'dmDev.ideDmDev | Valor esperado: 123 | Valor recebido: '+eSS1200.dmDev.Items[0].ideDmDev);

  Check(eSS1200.dmDev.Items[0].codCateg = 456,
        'dmDev.codCateg | Valor esperado: 456 | Valor recebido: '+IntToStr(eSS1200.dmDev.Items[0].codCateg));

  if(FACBreSocial.Configuracoes.Geral.VersaoDF = veS01_01_00)then
  begin
    Check(eSS1200.dmDev.Items[0].indRRA = snfSim,
          'dmDev.indRRA | Valor esperado: S | Valor recebido: '+eSSimNaoFacultativoToStr(eSS1200.dmDev.Items[0].indRRA));

    Check(eSS1200.dmDev.Items[0].infoRRA.tpProcRRA = tppAdministrativo,
          'dmDev.infoRRA.tpProcRRA | Valor esperado: 1 | Valor recebido: '+eSTpProcRRAToStr(eSS1200.dmDev.Items[0].infoRRA.tpProcRRA));

    Check(eSS1200.dmDev.Items[0].infoRRA.nrProcRRA = '123',
          'dmDev.infoRRA.nrProcRRA | Valor esperado: 123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoRRA.nrProcRRA);

    Check(eSS1200.dmDev.Items[0].infoRRA.descRRA = 'Rendimentos Receb Acum.',
      'dmDev.infoRRA.descRRA | Valor esperado: Rendimentos Receb Acum. | Valor recebido: '+eSS1200.dmDev.Items[0].infoRRA.descRRA);

    Check(eSS1200.dmDev.Items[0].infoRRA.qtdMesesRRA = 5,
      'dmDev.infoRRA.qtdMesesRRA | Valor esperado: 5 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoRRA.qtdMesesRRA));

    Check(eSS1200.dmDev.Items[0].infoRRA.despProcJud.vlrDespCustas = 0,
      'dmDev.infoRRA.despProcJud.vlrDespCustas | Valor esperado: 0 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoRRA.despProcJud.vlrDespCustas));

    Check(eSS1200.dmDev.Items[0].infoRRA.despProcJud.vlrDespAdvogados = 0,
      'dmDev.infoRRA.despProcJud.vlrDespAdvogados | Valor esperado: 0 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoRRA.despProcJud.vlrDespAdvogados));

    Check(eSS1200.dmDev.Items[0].infoRRA.ideAdv.Items[0].tpInsc = tiCNPJ,
          'dmDev.infoRRA.ideAdv.tpInsc | Valor esperado: 1 | Valor recebido: '+eSTpInscricaoToStr(eSS1200.dmDev.Items[0].infoRRA.ideAdv.Items[0].tpInsc));

    Check(eSS1200.dmDev.Items[0].infoRRA.ideAdv.Items[0].nrInsc = '11111111111111',
          'dmDev.infoRRA.ideAdv.nrInsc | Valor esperado: 11111111111111 | Valor recebido: '+eSS1200.dmDev.Items[0].infoRRA.ideAdv.Items[0].nrInsc);

    Check(eSS1200.dmDev.Items[0].infoRRA.ideAdv.Items[0].vlrAdv = 1,
      'dmDev.infoRRA.ideAdv.vlrAdv | Valor esperado: 1 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoRRA.ideAdv.Items[0].vlrAdv));
  end;

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].tpInsc = tiCNPJ,
        'dmDev.infoPerApur.ideEstabLot.tpInsc | Valor esperado: 1 | Valor recebido: '+eSTpInscricaoToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].tpInsc));

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].nrInsc = '12345678000123',
        'dmDev.infoPerApur.ideEstabLot.nrInsc | Valor esperado: 12345678000123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].nrInsc);

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].codLotacao = '123',
        'dmDev.infoPerApur.ideEstabLot.codLotacao | Valor esperado: 123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].codLotacao);

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].qtdDiasAv = 20,
        'dmDev.infoPerApur.ideEstabLot.qtdDiasAv | Valor esperado: 20 | Valor recebido: '+IntToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].qtdDiasAv));

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].matricula = '123',
        'dmDev.infoPerApur.ideEstabLot.remunPerApur.matricula | Valor esperado: 123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].matricula);

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].indSimples = idsIntegralmente,
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.indSimples | Valor esperado: 1 | Valor recebido: '+eSIndSimplesToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].indSimples));

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun.Items[0].codRubr = '123',
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.itensRemun.codRubr | Valor esperado: 123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun[0].codRubr);

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun.Items[0].ideTabRubr = '456',
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.itensRemun.ideTabRubr | Valor esperado: 456 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun[0].ideTabRubr);

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun.Items[0].qtdRubr = 10,
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.itensRemun.qtdRubr | Valor esperado: 10 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun[0].qtdRubr));

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun.Items[0].fatorRubr = 2,
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.itensRemun.fatorRubr | Valor esperado: 2 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun[0].fatorRubr));

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun.Items[0].vrRubr = 30,
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.itensRemun.vrRubr | Valor esperado: 30 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun[0].vrRubr));

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun.Items[0].indApurIR = tiaiNormal,
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.itensRemun.indApurIR | Valor esperado: 0 | Valor recebido: '+eSTpIndApurIRToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].itensRemun[0].indApurIR));

  Check(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].infoAgNocivo.grauExp = ge1,
      'dmDev.infoPerApur.ideEstabLot.remunPerApur.infoAgNocivo.grauExp | Valor esperado: 1 | Valor recebido: '+eSGrauExpToStr(eSS1200.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].remunPerApur.Items[0].infoAgNocivo.grauExp));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dtAcConv = StrToDate('03/05/2018'),
      'dmDev.infoPerAnt.ideADC.dtAcConv | Valor esperado: 03/05/2018 | Valor recebido: '+DateToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dtAcConv));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].tpAcConv = tacAcordoColTrab,
      'dmDev.infoPerAnt.ideADC.tpAcConv | Valor esperado: A | Valor recebido: '+eSTpAcConvToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].tpAcConv));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dsc = 'Descricao',
      'dmDev.infoPerAnt.ideADC.dsc | Valor esperado: Descricao | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dsc);

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].remunSuc = tpSim,
      'dmDev.infoPerAnt.ideADC.remunSuc | Valor esperado: S | Valor recebido: '+eSSimNaoToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].remunSuc));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].perRef = '201805',
      'dmDev.infoPerAnt.ideADC.idePeriodo.perRef | Valor esperado: 201805 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].perRef);

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].tpInsc = tiCNPJ,
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.tpInsc | Valor esperado: 1 | Valor recebido: '+eSTpInscricaoToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].tpInsc));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].nrInsc = '12345678000123',
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.nrInsc | Valor esperado: 12345678000123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].nrInsc);

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].codLotacao = '123',
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.codLotacao | Valor esperado: 123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].codLotacao);

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].matricula = '1234',
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.matricula | Valor esperado: 1234 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].matricula);

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].indSimples = idsIntegralmente,
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.indSimples | Valor esperado: 1 | Valor recebido: '+eSIndSimplesToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].indSimples));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].codRubr = '123',
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.itensRemun.codRubr | Valor esperado: 123 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].codRubr);

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].ideTabRubr = '456',
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.itensRemun.ideTabRubr | Valor esperado: 456 | Valor recebido: '+eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].ideTabRubr);

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].qtdRubr = 10,
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.itensRemun.qtdRubr | Valor esperado: 10 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].qtdRubr));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].fatorRubr = 1,
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.itensRemun.fatorRubr | Valor esperado: 1 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].fatorRubr));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].vrRubr = 80,
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.itensRemun.vrRubr | Valor esperado: 80 | Valor recebido: '+FloatToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].vrRubr));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].indApurIR = tiaiNormal,
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.itensRemun.indApurIR | Valor esperado: 0 | Valor recebido: '+eSTpIndApurIRToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].itensRemun.Items[0].indApurIR));

  Check(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].infoAgNocivo.grauExp = ge1,
      'dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.remunPerAnt.infoAgNocivo.grauExp | Valor esperado: 1 | Valor recebido: '+eSGrauExpToStr(eSS1200.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].remunPerAnt.Items[0].infoAgNocivo.grauExp));

  Check(eSS1200.dmDev.Items[0].infoComplCont.codCBO = '000001',
      'dmDev.infoComplCont.codCBO | Valor esperado: 000001 | Valor recebido: '+eSS1200.dmDev.Items[0].infoComplCont.codCBO);

  Check(eSS1200.dmDev.Items[0].infoComplCont.natAtividade = navUrbano,
      'dmDev.infoComplCont.natAtividade | Valor esperado: 1 | Valor recebido: '+eSNatAtividadeToStr(eSS1200.dmDev.Items[0].infoComplCont.natAtividade));

  Check(eSS1200.dmDev.Items[0].infoComplCont.qtdDiasTrab = 20,
      'dmDev.infoComplCont.qtdDiasTrab | Valor esperado: 20 | Valor recebido: '+IntToStr(eSS1200.dmDev.Items[0].infoComplCont.qtdDiasTrab));


end;


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
  Check(InfoComp.nmTrab = 'Nome do trabalhador',
        'ideTrabalhador.infoComplem.nmTrab | Valor esperado:Nome do trabalhador | Valor recebido:'+InfoComp.nmTrab);

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

    ideAdvI := dmDevI.infoRRA.ideAdv.Items[0];
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

  if(FACBreSocial.Configuracoes.Geral.VersaoDF = veS01_01_00)then
    begin
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


end;

procedure TACBreSocialEventosPeriodicosTest.ES1260Tests;
var
  eS1260: TEvtComProd;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1260.Count > 0, 'Não instanciou o S-1260 na lista!');

  AssertEquals('Não instanciou o S-1260 na lista!', 1, FACBreSocial.Eventos.Periodicos.S1260.Count);

  eS1260 := FACBreSocial.Eventos.Periodicos.S1260[0].EvtComProd;

  Check(eS1260.IdeEvento.indRetif = ireOriginal,
        'ideEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eS1260.IdeEvento.indRetif));

  Check(eS1260.IdeEvento.nrRecibo = '123',
        'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eS1260.IdeEvento.nrRecibo);

  Check(eS1260.IdeEvento.perApur = '2018-05',
        'ideEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eS1260.IdeEvento.perApur);

  Check(eS1260.IdeEvento.indGuia = '1',
        'ideEvento.indGuia | Valor esperado:1 | Valor recebido:'+eS1260.IdeEvento.indGuia);

  Check(eS1260.IdeEvento.ProcEmi = peAplicEmpregador,
        'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1260.IdeEvento.procEmi));

  Check(eS1260.IdeEvento.verProc = '1.00',
        'ideEvento.indRetif | Valor esperado:1.00 | Valor recebido:'+eS1260.IdeEvento.verProc);

  Check(eS1260.IdeEmpregador.TpInsc = tiCNPJ,
        'IdeEmpregador.TpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1260.IdeEmpregador.TpInsc));

  Check(eS1260.IdeEmpregador.NrInsc = '12345678000123',
        'IdeEmpregador.NrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1260.IdeEmpregador.NrInsc);

  Check(eS1260.InfoComProd.IdeEstabel.nrInscEstabRural = '12345678000123',
        'InfoComProd.IdeEstabel.nrInscEstabRural | Valor esperado:12345678000123 | Valor recebido:'+eS1260.InfoComProd.IdeEstabel.nrInscEstabRural);

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].indComerc = icComProdPorProdRuralPFInclusiveSegEspEfetuadaDirVarejoConsFinal,
        'InfoComProd.IdeEstabel.TpComerc.indComerc | Valor esperado:1 | Valor recebido:'+eSIndComercStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].indComerc));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].vrTotCom = 1000,
        'InfoComProd.IdeEstabel.TpComerc.vrTotCom | Valor esperado:1000 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].vrTotCom));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].tpInsc = tiCNPJ,
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].tpInsc));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nrInsc = '12345678000123',
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nrInsc);

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].vrComerc = 2000,
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.vrComerc | Valor esperado:2000 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].vrComerc));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].serie = '1',
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nfs.serie | Valor esperado:1 | Valor recebido:'+eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].serie);

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].nrDocto = '50',
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nfs.serie | Valor esperado:50 | Valor recebido:'+eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].nrDocto);

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].dtEmisNF = StrToDateTime('04/05/2018'),
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nfs.dtEmisNF | Valor esperado:04/05/2018 | Valor recebido:'+DateTimeToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].dtEmisNF));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vlrBruto = 5000,
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nfs.vlrBruto | Valor esperado:5000 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vlrBruto));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vrCPDescPR = 0,
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nfs.vrCPDescPR | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vrCPDescPR));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vrRatDescPR = 0,
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nfs.vrRatDescPR | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vrRatDescPR));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vrSenarDesc = 0,
        'InfoComProd.IdeEstabel.TpComerc.IdeAdquir.nfs.vrSenarDesc | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].IdeAdquir.Items[0].nfs.Items[0].vrSenarDesc));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].tpProc = tpAdministrativo,
        'InfoComProd.IdeEstabel.TpComerc.InfoProcJud.tpProc | Valor esperado:1 | Valor recebido:'+eSTpProcessoToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].tpProc));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].nrProcJud = '123',
        'InfoComProd.IdeEstabel.TpComerc.InfoProcJud.nrProc | Valor esperado:123 | Valor recebido:'+eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].nrProcJud);

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].codSusp = 456,
        'InfoComProd.IdeEstabel.TpComerc.InfoProcJud.codSusp | Valor esperado:456 | Valor recebido:'+IntToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].codSusp));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].vrCPSusp = 0,
        'InfoComProd.IdeEstabel.TpComerc.InfoProcJud.vrCPSusp | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].vrCPSusp));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].vrRatSusp = 0,
      'InfoComProd.IdeEstabel.TpComerc.InfoProcJud.vrRatSusp | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].vrRatSusp));

  Check(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].vrSenarSusp = 0,
        'InfoComProd.IdeEstabel.TpComerc.InfoProcJud.vrSenarSusp | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1260.InfoComProd.IdeEstabel.TpComerc.Items[0].InfoProcJud.Items[0].vrSenarSusp));



end;

procedure TACBreSocialEventosPeriodicosTest.ES1270Tests;
var
  eS1270: TEvtContratAvNP;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1270.Count > 0, 'Não instanciou o S-1270 na lista!');

  eS1270 := FACBreSocial.Eventos.Periodicos.S1270[0].EvtContratAvNP;

  Check(eS1270.IdeEvento.indRetif = ireOriginal,
        'IdeEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eS1270.ideEvento.indRetif));

  Check(eS1270.IdeEvento.nrRecibo = '123',
        'IdeEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eS1270.ideEvento.nrRecibo);

  Check(eS1270.IdeEvento.perApur = '2018-05' ,
        'IdeEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eS1270.ideEvento.perApur);

  Check(eS1270.IdeEvento.indGuia = '1',
        'IdeEvento.indGuia | Valor esperado:1 | Valor recebido:'+eS1270.ideEvento.indGuia);

  Check(eS1270.IdeEvento.procEmi = peAplicEmpregador ,
        'IdeEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1270.ideEvento.procEmi));

  Check(eS1270.IdeEvento.verProc = '1.00' ,
        'IdeEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1270.ideEvento.verProc);

  Check(eS1270.IdeEmpregador.TpInsc = tiCNPJ,
        'IdeEmpregador.TpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1270.IdeEmpregador.TpInsc));

  Check(eS1270.IdeEmpregador.nrInsc = '12345678000123' ,
        'IdeEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1270.IdeEmpregador.nrInsc);

  Check(eS1270.remunAvNp.Items[0].tpInsc = tiCNPJ,
        'remunAvNp.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1270.remunAvNp.Items[0].tpInsc));

  Check(eS1270.remunAvNp.Items[0].nrInsc = '12345678000123',
        'remunAvNp.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1270.remunAvNp.Items[0].nrInsc);

  Check(eS1270.remunAvNp.Items[0].codLotacao = '123',
        'remunAvNp.codLotacao | Valor esperado:123 | Valor recebido:'+eS1270.remunAvNp.Items[0].codLotacao);

  Check(eS1270.remunAvNp.Items[0].vrBcCp00 = 0,
        'remunAvNp.vrBcCp00 | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1270.remunAvNp.Items[0].vrBcCp00));

  Check(eS1270.remunAvNp.Items[0].vrBcCp15 = 0,
        'remunAvNp.vrBcCp15 | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1270.remunAvNp.Items[0].vrBcCp15));

  Check(eS1270.remunAvNp.Items[0].vrBcCp20 = 0,
        'remunAvNp.vrBcCp20 | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1270.remunAvNp.Items[0].vrBcCp20));

  Check(eS1270.remunAvNp.Items[0].vrBcCp25 = 0,
        'remunAvNp.vrBcCp25 | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1270.remunAvNp.Items[0].vrBcCp25));

  Check(eS1270.remunAvNp.Items[0].vrBcCp13 = 0,
        'remunAvNp.vrBcCp13 | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1270.remunAvNp.Items[0].vrBcCp13));

  Check(eS1270.remunAvNp.Items[0].vrBcFgts = 0,
        'remunAvNp.vrBcFgts | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1270.remunAvNp.Items[0].vrBcFgts));

  Check(eS1270.remunAvNp.Items[0].vrDescCP = 0,
        'remunAvNp.vrDescCP | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1270.remunAvNp.Items[0].vrDescCP));


end;

procedure TACBreSocialEventosPeriodicosTest.ES1280Tests;
var
  eS1280: TEvtInfoComplPer;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1280.Count > 0, 'Não instanciou o S-1280 na lista!');

  eS1280 := FACBreSocial.Eventos.Periodicos.S1280[0].EvtInfoComplPer;

  Check(eS1280.IdeEvento.indRetif = ireOriginal,
        'IdeEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eS1280.ideEvento.indRetif));

  Check(eS1280.IdeEvento.nrRecibo = '123',
        'IdeEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eS1280.ideEvento.nrRecibo);

  Check(eS1280.IdeEvento.perApur = '2018-05' ,
        'IdeEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eS1280.ideEvento.perApur);

  Check(eS1280.IdeEvento.indGuia = '1',
        'IdeEvento.indGuia | Valor esperado:1 | Valor recebido:'+eS1280.ideEvento.indGuia);

  Check(eS1280.IdeEvento.procEmi = peAplicEmpregador ,
        'IdeEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1280.ideEvento.procEmi));

  Check(eS1280.IdeEvento.verProc = '1.00' ,
        'IdeEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1280.ideEvento.verProc);

  Check(eS1280.IdeEmpregador.TpInsc = tiCNPJ,
        'IdeEmpregador.TpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1280.IdeEmpregador.TpInsc));

  Check(eS1280.IdeEmpregador.nrInsc = '12345678000123' ,
        'IdeEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1280.IdeEmpregador.nrInsc);

  Check(eS1280.InfoSubstPatr.indSubstPatr = spIntegralmenteSubstituida,
        'InfoSubstPatr.indSubstPatr | Valor esperado:1 | Valor recebido:'+eSIndSubstPatrStr(eS1280.InfoSubstPatr.indSubstPatr));

  Check(eS1280.InfoSubstPatr.percRedContrib = 0,
        'InfoSubstPatr.percRedContrib | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1280.InfoSubstPatr.percRedContrib));

  Check(eS1280.InfoSubstPatrOpPort.Items[0].codLotacao = '05',
        'InfoSubstPatrOpPort.codLotacao | Valor esperado:05 | Valor recebido:'+eS1280.InfoSubstPatrOpPort.Items[0].codLotacao);

  Check(eS1280.InfoAtivConcom.fatorMes = 0,
        'InfoAtivConcom.fatorMes | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1280.InfoAtivConcom.fatorMes));

  Check(eS1280.InfoAtivConcom.fator13 = 0,
        'InfoAtivConcom.fator13 | Valor esperado:0 | Valor recebido:'+FloatToStr(eS1280.InfoAtivConcom.fator13));

  Check(eS1280.infoPercTransf11096.percTransf = 1,
        'infoPercTransf11096.percTransf | Valor esperado:1 | Valor recebido:'+IntToStr(eS1280.infoPercTransf11096.percTransf));

end;

procedure TACBreSocialEventosPeriodicosTest.ES1298Tests;
var
  eS1298: TEvtReabreEvPer;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1298.Count > 0, 'Não instanciou o S-1298 na lista!');

  eS1298 := FACBreSocial.Eventos.Periodicos.S1298[0].EvtReabreEvPer;

  Check(eS1298.IdeEvento.IndApuracao = iapuMensal,
        'IdeEvento.IndApuracao | Valor esperado:1 | Valor recebido:'+eSIndApuracaoToStr(eS1298.ideEvento.IndApuracao));

  Check(eS1298.IdeEvento.perApur = '2018-05' ,
        'IdeEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eS1298.ideEvento.perApur);

  Check(eS1298.IdeEvento.indGuia = '1',
        'IdeEvento.indGuia | Valor esperado:1 | Valor recebido:'+eS1298.ideEvento.indGuia);

  Check(eS1298.IdeEvento.procEmi = peAplicEmpregador ,
        'IdeEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1298.ideEvento.procEmi));

  Check(eS1298.IdeEvento.verProc = '1.00' ,
        'IdeEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1298.ideEvento.verProc);

  Check(eS1298.IdeEmpregador.TpInsc = tiCNPJ,
        'IdeEmpregador.TpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1298.IdeEmpregador.TpInsc));

  Check(eS1298.IdeEmpregador.nrInsc = '12345678000123' ,
        'IdeEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1298.IdeEmpregador.nrInsc);

end;

procedure TACBreSocialEventosPeriodicosTest.ES1299Tests;
var
  eS1299: TEvtFechaEvPer;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1299.Count > 0, 'Não instanciou o S-1299 na lista!');

  eS1299 := FACBreSocial.Eventos.Periodicos.S1299[0].EvtFechaEvPer;

  Check(eS1299.IdeEvento.IndApuracao = iapuMensal,
        'IdeEvento.IndApuracao | Valor esperado:1 | Valor recebido:'+eSIndApuracaoToStr(eS1299.ideEvento.IndApuracao));

  Check(eS1299.IdeEvento.perApur = '2018-05' ,
        'IdeEvento.perApur | Valor esperado:2018-05 | Valor recebido:'+eS1299.ideEvento.perApur);

  Check(eS1299.IdeEvento.indGuia = '1',
        'IdeEvento.indGuia | Valor esperado:1 | Valor recebido:'+eS1299.ideEvento.indGuia);

  Check(eS1299.IdeEvento.procEmi = peAplicEmpregador ,
        'IdeEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS1299.ideEvento.procEmi));

  Check(eS1299.IdeEvento.verProc = '1.00' ,
        'IdeEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS1299.ideEvento.verProc);

  Check(eS1299.IdeEmpregador.TpInsc = tiCNPJ,
        'IdeEmpregador.TpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS1299.IdeEmpregador.TpInsc));

  Check(eS1299.IdeEmpregador.nrInsc = '12345678000123' ,
        'IdeEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS1299.IdeEmpregador.nrInsc);

  Check(eS1299.InfoFech.evtRemun = tpSim ,
        'InfoFech.evtRemun | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS1299.InfoFech.evtRemun));

  Check(eS1299.InfoFech.evtPgtos = tpSim ,
        'InfoFech.evtPgtos | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS1299.InfoFech.evtPgtos));

  Check(eS1299.InfoFech.evtComProd = tpSim ,
        'InfoFech.evtComProd | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS1299.InfoFech.evtComProd));

  Check(eS1299.InfoFech.evtContratAvNP = tpSim ,
        'InfoFech.evtContratAvNP | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS1299.InfoFech.evtContratAvNP));

  Check(eS1299.InfoFech.evtInfoComplPer = tpSim ,
        'InfoFech.evtInfoComplPer | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS1299.InfoFech.evtInfoComplPer));

  Check(eS1299.InfoFech.indExcApur1250 = snfSim ,
        'InfoFech.indExcApur1250 | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(eS1299.InfoFech.indExcApur1250));

  Check(eS1299.InfoFech.transDCTFWeb = snfSim ,
        'InfoFech.transDCTFWeb | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(eS1299.InfoFech.transDCTFWeb));

  Check(eS1299.InfoFech.naoValid = snfSim ,
        'InfoFech.naoValid | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(eS1299.InfoFech.naoValid));
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
      ES1200Tests;
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
      ES1200Tests;
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

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1270_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1270.Count = 0, 'Lista de eventos S-1270 não está vazia!');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1270_vS0101;
begin
  try
    FACBreSocial.Eventos.Periodicos.S1270.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1270);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1270Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1280_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1280.Count = 0, 'Lista de eventos S-1280 não está vazia!');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1280_vS0101;
begin
  try
    FACBreSocial.Eventos.Periodicos.S1280.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1280);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1280Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1298_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1298.Count = 0, 'Lista de eventos S-1298 não está vazia!');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1298_vS0101;
begin
  try
    FACBreSocial.Eventos.Periodicos.S1298.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1298);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1298Tests;
    end;
  end;
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventosPeriodicosS1299_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Periodicos.S1299.Count = 0, 'Lista de eventos S-1299 não está vazia!');
end;

procedure TACBreSocialEventosPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1299_vS0101;
begin
  try
    FACBreSocial.Eventos.Periodicos.S1299.Clear;
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1299);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1299Tests;
    end;
  end;
end;

end.

