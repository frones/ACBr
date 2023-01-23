unit ACBreSocialEventosNaoPeriodicosTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBrUtil.DateTime, ACBreSocialTestsConsts,
  ACBrDFeException, ACBrDFeSSL, ACBreSocial, pcesS2400, pcesCommon,
  pcesConversaoeSocial, pcesS2240;

type

{ TACBreSocialEventosNaoPeriodicosTest }

TACBreSocialEventosNaoPeriodicosTest = class(TTestCase)
  private
    FACBreSocial : TACBreSocial;
    procedure ES2240Tests;
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure ACBreSocialEventosNaoPeriodicosS2400_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2400;
    procedure ACBreSocialEventosNaoPeriodicosS2240_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_S0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_S0101;
end;

implementation

{ TACBreSocialEventosNaoPeriodicosTest }

procedure TACBreSocialEventosNaoPeriodicosTest.Setup;
begin
  inherited Setup;
  FACBreSocial := TACBreSocial.Create(nil);
  FACBreSocial.Configuracoes.Geral.SSLLib := libOpenSSL;
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
    FACBreSocial.Eventos.LoadFromIni(ARQINI_S0100_S2400);
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

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2240_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2240.Count = 0, 'Lista de Eventos S-2240 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2240Tests;
var
  eS2240: TEvtExpRisco;
  infoAmb: TInfoAmbCollectionItem;
  agNoc: TAgNocCollectionItem;
  epi: TEpiCollectionItem;
  epiCompl: TEpiCompl;
  respReg: TRespRegCollectionItem;
begin

  eS2240 := FACBreSocial.Eventos.NaoPeriodicos.S2240[0].EvtExpRisco;

  Check(FACBreSocial.Eventos.NaoPeriodicos.S2240.Count > 0, 'Não instanciou o S-2240 na lista');

  Check(eS2240.Sequencial = 0, 'Sequencial | Valor esperado:0 | Valor recebido:'+IntToStr(eS2240.Sequencial));
  Check(eS2240.IdeEvento.indRetif = ireOriginal,
        'ideEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eS2240.IdeEvento.indRetif));
  Check(eS2240.IdeEvento.NrRecibo = '123',
        'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eS2240.IdeEvento.nrRecibo);
  Check(eS2240.IdeEvento.ProcEmi  = peAplicEmpregador,
        'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS2240.IdeEvento.ProcEmi));
  Check(eS2240.IdeEvento.VerProc  = '1.00',
        'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS2240.IdeEvento.VerProc);

  Check(eS2240.IdeEmpregador.TpInsc =  tiCNPJ,
        'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+ eSTpInscricaoToStr(eS2240.IdeEmpregador.TpInsc));
  Check(eS2240.IdeEmpregador.NrInsc = '12345678000123',
        'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS2240.IdeEmpregador.NrInsc);

  Check(eS2240.IdeVinculo.cpfTrab = '12345678901',
        'ideVinculo.cpfTrab | Valor esperado:12345678901 | Valor recebido:'+eS2240.IdeVinculo.cpfTrab);
  Check(eS2240.ideVinculo.matricula = '123',
        'ideVinculo.matricula | Valor esperado:123 | Valor recebido:'+eS2240.ideVinculo.matricula);
  Check(eS2240.ideVinculo.codCateg = 101,
        'ideVinculo.codCateg | Valor esperado:101 | Valor recebido:'+IntToStr(eS2240.ideVinculo.codCateg));

  Check(eS2240.infoExpRisco.dtIniCondicao = StoD('20180508000000'),
        'infoExpRisco.dtIniCondicao | Valor esperado:"08/05/2018" | Valor recebido:'+DateToStr(eS2240.infoExpRisco.dtIniCondicao));
  Check(eS2240.infoExpRisco.dtFimCondicao = StoD('20180509000000'),
        'infoExpRisco.dtFimCondicao | Valor esperado:"09/05/2018" | Valor recebido:'+DateToStr(eS2240.infoExpRisco.dtFimCondicao));

  infoAmb := eS2240.infoExpRisco.InfoAmb.Items[0];
  Check(infoAmb.localAmb = laEstabProprioEmpregador,
        'infoExpRisco.infoAmb.localAmb | Valor esperado:1 | Valor recebido:'+eSLocalAmbToStr(infoAmb.localAmb));
  Check(infoAmb.dscSetor = 'descicao',
        'infoExpRisco.infoAmb.dscSetor | Valor esperado:descicao | Valor recebido:'+infoAmb.dscSetor);
  Check(infoAmb.tpInsc = tiCNPJ,
        'infoExpRisco.infoAmb.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(infoAmb.tpInsc));
  Check(infoAmb.nrInsc = '999999999999',
        'infoExpRisco.infoAmb.nrInsc | Valor esperado:999999999999 | Valor recebido:'+infoAmb.nrInsc);

  Check(eS2240.infoExpRisco.infoAtiv.dscAtivDes = 'Descricao da atividade',
        'infoExpRisco.infoAtiv.dscAtivDes | Valor esperado:Descricao da atividade | Valor recebido:"'+eS2240.infoExpRisco.infoAtiv.dscAtivDes);

  agNoc := eS2240.infoExpRisco.agNoc.Items[0];
  Check(agNoc.codAgNoc = '02.01.001',
        'infoExpRisco.agNoc.codAgNoc | Valor esperado:02.01.001 | Valor recebido:'+agNoc.codAgNoc);
  Check(agNoc.dscAgNoc = 'descicao',
        'infoExpRisco.agNoc.dscAgNoc | Valor esperado:descicao | Valor recebido:'+agNoc.dscAgNoc);
  Check(agNoc.tpAval   = tpaQuantitativo,
        'infoExpRisco.agNoc.tpAval | Valor esperado:1 | Valor recebido:'+tpAvalToStr(agNoc.tpAval));
  Check(agNoc.intConc  = 1,
        'infoExpRisco.agNoc.intConc | Valor esperado:1 | Valor recebido:'+FormatFloat('#0.00', agNoc.intConc));
  Check(agNoc.limTol   = 10,
        'infoExpRisco.agNoc.limTol | Valor esperado:10 | Valor recebido:'+FormatFloat('#0.00', agNoc.limTol));
  Check(agNoc.unMed    = 1 ,
        'infoExpRisco.agNoc.unMed | Valor esperado:1 | Valor recebido:'+FormatFloat('#0.00', agNoc.unMed));
  Check(agNoc.tecMedicao = 'descricao',
        'infoExpRisco.agNoc.tecMedicao | Valor esperado:descricao | Valor recebido:'+agNoc.tecMedicao);

  Check(agNoc.epcEpi.utilizEPC = uEPCImplementa,
        'infoExpRisco.agNoc.epcEpi.utilizEPC | Valor esperado:2 | Valor recebido:'+eStpUtilizEPCToStr(agNoc.epcEpi.utilizEPC));
  Check(agNoc.epcEpi.eficEpc = tpSim,
        'infoExpRisco.agNoc.epcEpi.eficEpc | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(agNoc.epcEpi.eficEpc));
  Check(agNoc.epcEpi.utilizEPI = uEPIUtilizado,
        'infoExpRisco.agNoc.epcEpi.utilizEpi | Valor esperado:2 | Valor recebido:'+eStpUtilizEPIToStr(agNoc.epcEpi.utilizEPI));
  Check(agNoc.epcEpi.eficEpi = snfSim,
        'infoExpRisco.agNoc.epcEpi.eficEpi | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(agNoc.epcEpi.eficEpi));

  epi := agNoc.epcEpi.epi.Items[0];
  Check(epi.docAval = 'TesteDocAval',
        'infoExpRisco.agNoc.epcEpi.epi.docAval | Valor esperado:TesteDocAval | Valor recebido:'+epi.docAval);
  if(FACBreSocial.Configuracoes.Geral.VersaoDF = veS01_00_00)then
  begin
    Check(epi.dscEPI = 'TesteDscEPI',
          'infoExpRisco.agNoc.epcEpi.epi.dscEPI | Valor esperado:TesteDscEPI | Valor recebido:'+epi.dscEPI);
  end;
  epiCompl := agNoc.epcEpi.epiCompl;
  Check(epiCompl.medProtecao = snfNao,
        'infoExpRisco.agNoc.epcEpi.epiCompl.medProtecao | Valor esperado:N | Valor recebido:'+eSSimNaoFacultativoToStr(epiCompl.medProtecao));
  Check(epiCompl.condFuncto  = snfNao,
        'infoExpRisco.agNoc.epcEpi.epiCompl.condFuncto | Valor esperado:N | Valor recebido:'+eSSimNaoFacultativoToStr(epiCompl.condFuncto));
  Check(epiCompl.usoInint    = snfNao,
        'infoExpRisco.agNoc.epcEpi.epiCompl.usoInint | Valor esperado:N | Valor recebido:'+eSSimNaoFacultativoToStr(epiCompl.usoInint));
  Check(epiCompl.przValid    = snfNao, 'infoExpRisco.agNoc.epcEpi.epiCompl.przValid diferente do valor esperado');
  Check(epiCompl.periodicTroca = snfNao,
        'infoExpRisco.agNoc.epcEpi.epiCompl.periodicTroca | Valor esperado:N | Valor recebido:'+eSSimNaoFacultativoToStr(epiCompl.periodicTroca));
  Check(epiCompl.higienizacao  = snfNao,
        'infoExpRisco.agNoc.epcEpi.epiCompl.higienizacao | Valor esperado:N | Valor recebido:'+eSSimNaoFacultativoToStr(epiCompl.higienizacao));

  respReg := eS2240.infoExpRisco.respReg.Items[0];
  Check(respReg.cpfResp = '00000000000',
        'infoExpRisco.respReg.cpfResp | Valor esperado:00000000000 | Valor recebido:'+respReg.cpfResp);
  Check(respReg.ideOC = idCRM,
        'infoExpRisco.respReg.ideOC | Valor esperado:1 | Valor recebido:'+eSIdeOCToStr(respReg.ideOC));
  Check(respReg.dscOC = 'CRM',
        'infoExpRisco.respReg.dscOC | Valor esperado:CRM | Valor recebido:'+respReg.dscOC);
  Check(respReg.nrOC  = '123',
        'infoExpRisco.respReg.nrOc | Valor esperado:123 | Valor recedibo:'+respReg.nrOC);
  Check(respReg.ufOC  = 'SP' ,
        'infoExpRisco.respReg.ufOC | Valor esperado:SP | Valor recebido:'+respReg.ufOC);

  Check(eS2240.infoExpRisco.obs.obsCompl = 'obsCompl',
        'infoExpRisco.obs.obsCompl | Valor esperado:obsCompl | Valor recebido:'+eS2240.infoExpRisco.obs.obsCompl);


end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_S0100;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
  try
    FACBreSocial.Eventos.LoadFromIni(ARQINI_S0100_S2240);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2240Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_S0101;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
  try
    FACBreSocial.Eventos.LoadFromIni(ARQINI_S0101_S2240);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2240Tests;
    end;
  end;
end;

end.

