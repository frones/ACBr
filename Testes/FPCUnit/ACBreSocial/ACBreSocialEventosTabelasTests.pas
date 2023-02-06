unit ACBreSocialEventosTabelasTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocialTestsConsts, ACBreSocial, pcesConversaoeSocial,
  ACBrDFeException, pcesS1010 , pcesCommon, ACBrUtil.DateTime;

type

  { TACBreSocialEventosTabelasTest }

  TACBreSocialEventosTabelasTest = class(TTestCase)
  private
    FACBreSocial: TACBreSocial;
    procedure ES1010Tests;
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure ACBreSocialEventosTabelaSS1010_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1010_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS1010_vS0101;
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

  Check(dadosRubrica.IdeProcessoSIND[0].nrProc ='123',
        'infoRubrica.dadosRubrica.ideProcessoSIND.nrProc | Valor esperado:123 | Valor recebudi:'+dadosRubrica.ideProcessoSIND[0].nrProc);



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

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventosTabelaSS1010_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.Tabelas.S1010.Count = 0, 'Lista de eventos S-1010 não está vazia');
end;

procedure TACBreSocialEventosTabelasTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS1010_vS0100;
begin
  try
    FACBreSocial.Eventos.Clear;
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
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S1010);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi '+ E.ClassName);
      ES1010Tests;
    end;
  end;
end;

end.

