unit ACBreSocialEventosNaoPeriodicosTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBrUtil.DateTime, ACBreSocialTestsConsts,
  ACBrDFeException, ACBrDFeSSL, ACBreSocial, pcesS2400, pcesCommon,
  pcesConversaoeSocial, pcesS2240, pcesS2200;

type

{ TACBreSocialEventosNaoPeriodicosTest }

TACBreSocialEventosNaoPeriodicosTest = class(TTestCase)
  private
    FACBreSocial : TACBreSocial;
    procedure ES2240Tests;
    procedure ES2200Tests;
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure ACBreSocialEventosNaoPeriodicosS2400_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2400;
    procedure ACBreSocialEventosNaoPeriodicosS2240_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2200_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2200_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2200_vS0101;
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
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S2400);
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
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2240.Count > 0, 'Não instanciou o S-2240 na lista');

  eS2240 := FACBreSocial.Eventos.NaoPeriodicos.S2240[0].EvtExpRisco;

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
        'infoExpRisco.dtIniCondicao | Valor esperado:"08/05/2018" | Valor recebido:'+FormatDateBr(eS2240.infoExpRisco.dtIniCondicao));
  Check(eS2240.infoExpRisco.dtFimCondicao = StoD('20180509000000'),
        'infoExpRisco.dtFimCondicao | Valor esperado:"09/05/2018" | Valor recebido:'+FormatDateBr(eS2240.infoExpRisco.dtFimCondicao));

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
        'infoExpRisco.respReg.ideOC | Valor esperado:1 | Valor recebido:'+eSIdeOCToStrEX(respReg.ideOC));
  Check(respReg.dscOC = 'CRM',
        'infoExpRisco.respReg.dscOC | Valor esperado:CRM | Valor recebido:'+respReg.dscOC);
  Check(respReg.nrOC  = '123',
        'infoExpRisco.respReg.nrOc | Valor esperado:123 | Valor recedibo:'+respReg.nrOC);
  Check(respReg.ufOC  = 'SP' ,
        'infoExpRisco.respReg.ufOC | Valor esperado:SP | Valor recebido:'+respReg.ufOC);

  Check(eS2240.infoExpRisco.obs.obsCompl = 'obsCompl',
        'infoExpRisco.obs.obsCompl | Valor esperado:obsCompl | Valor recebido:'+eS2240.infoExpRisco.obs.obsCompl);


end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2200Tests;
var
  eS2200: TEvtAdmissao;
  trabalhador: TTrabalhador;
  enderecoBrasil: TBrasil;
  dependente: TDependenteCollectionItem;
  infoCeletista: TInfoCeletista;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2200.Count > 0, 'Não instanciou o S-2200 na lista');
  eS2200 := FACBreSocial.Eventos.NaoPeriodicos.S2200[0].EvtAdmissao;
  Check(eS2200.Sequencial = 0, 'Sequencial | Valor esperado:0 | Valor recebido:'+IntToStr(eS2200.Sequencial));

  Check(eS2200.ideEvento.indRetif = ireOriginal, 'ideEvento.indRetif | Valor esperado:1 | Valor recebido:'+eSIndRetificacaoToStr(eS2200.ideEvento.indRetif));
  Check(eS2200.ideEvento.NrRecibo = '123', 'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eS2200.ideEvento.nrRecibo);
  Check(eS2200.ideEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS2200.ideEvento.ProcEmi));
  Check(eS2200.ideEvento.VerProc = '1.00', 'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS2200.IdeEvento.verProc);

  Check(eS2200.IdeEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS2200.ideEmpregador.tpInsc));
  Check(eS2200.IdeEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS2200.ideEmpregador.NrInsc);

  trabalhador := eS2200.Trabalhador;
  Check(trabalhador.CpfTrab = '12345678901', 'trabalhador.cpfTrab | Valor esperado:12345678901 | Valor recebido:'+trabalhador.CpfTrab);
  Check(trabalhador.NmTrab = 'Nome do Trabalhador', 'trabalhador.NmTrab | Valor esperado:Nome do Trabalhador | Valor recebido:'+trabalhador.NmTrab);
  Check(trabalhador.Sexo='M', 'trabalhador.sexo | Valor esperado:M | Valor recebido:'+trabalhador.sexo);
  Check(trabalhador.RacaCor = 1, 'trabalhador.racaCor | Valor esperado:1 | Valor recebido:'+IntToStr(trabalhador.racaCor));
  Check(trabalhador.EstCiv = 1, 'trabalhador.estCiv | Valor esperado:1 | Valor recebido:'+IntToStr(trabalhador.EstCiv));
  Check(trabalhador.GrauInstr='01', 'trabalhador.grauInstr | Valor esperado:01 | Valor recebido:'+trabalhador.GrauInstr);
  Check(trabalhador.nmSoc='Nome Social', 'trabalhador.nmSoc | Valor esperado:Nome Social | Valor recebido:'+trabalhador.nmSoc);

  Check(trabalhador.Nascimento.dtNascto = StoD('19801010000000'), 'trabalhador.nascimento.dtNascto | Valor esperado:10/10/1980 | Valor recebido:'+FormatDateBr(trabalhador.Nascimento.dtNascto));
  Check(trabalhador.Nascimento.PaisNascto = '565', 'trabalhador.nascimento.paisNascto | Valor esperado:565 | Valor recebido:'+trabalhador.Nascimento.PaisNascto);
  Check(trabalhador.Nascimento.PaisNac = '565', 'trabalhador.nascimento.paisNac | Valor esperado:565 | Valor recebido:'+trabalhador.Nascimento.PaisNac);

  enderecoBrasil := trabalhador.Endereco.Brasil;
  Check(enderecoBrasil.TpLograd='Rua', 'trabalhador.endereco.brasil.tpLograd | Valor esperado:Rua | Valor recebido:'+enderecoBrasil.TpLograd);
  Check(enderecoBrasil.DscLograd='Nove de Novembro', 'trabalhador.endereco.brasil.dscLograd | Valor esperado:Nove de Novembro | Valor recebido:'+enderecoBrasil.DscLograd);
  Check(enderecoBrasil.NrLograd='100', 'trabalhador.endereco.brasil.nrLograd | Valor esperado:100 | Valor recebido:'+enderecoBrasil.NrLograd);
  Check(enderecoBrasil.Complemento='Apto 10', 'trabalhador.endereco.brasil.complemento | Valor esperado:Apto 10 | Valor recebido:'+enderecoBrasil.Complemento);
  Check(enderecoBrasil.Bairro='Centro', 'trabalhador.endereco.brasil.bairro | Valor esperado:Centro | Valor recebido:'+enderecoBrasil.Bairro);
  Check(enderecoBrasil.Cep='14123456', 'trabalhador.endereco.brasil.cep | Valor esperado:14123456 | Valor recebido:'+enderecoBrasil.Cep);
  Check(enderecoBrasil.CodMunic=3512345, 'trabalhador.endereco.brasil.codMunic | Valor esperado:3512345 | Valor recebido:'+IntToStr(enderecoBrasil.CodMunic));
  Check(enderecoBrasil.UF='SP', 'trabalhador.endereco.brasil.uf | Valor esperado:SP | Valor recebido:'+enderecoBrasil.uf);

  Check(trabalhador.trabImig.tmpResid = ttrPrazoIndeterminado, 'trabalhador.trabImig.tmpResid | Valor esperado:1 | Valor recebido:'+tpTmpResidToStr(trabalhador.trabImig.tmpResid));
  Check(trabalhador.trabImig.condIng = tciPermanenciaNoBrasilReuniaoFamiliar, 'trabalhador.trabImig.condIng | Valor esperado:3 | Valor recebido:'+tpCondIngToStr(Trabalhador.trabImig.condIng));

  Check(trabalhador.InfoDeficiencia.DefFisica      = tpNao ,
        'trabalhador.InfoDeficiencia.DefFisica | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(trabalhador.InfoDeficiencia.DefFisica));
  Check(trabalhador.InfoDeficiencia.DefVisual      = tpNao ,
        'trabalhador.InfoDeficiencia.DefVisual | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(trabalhador.InfoDeficiencia.DefVisual));
  Check(trabalhador.InfoDeficiencia.DefAuditiva    = tpNao ,
        'trabalhador.InfoDeficiencia.DefAuditiva | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(trabalhador.InfoDeficiencia.DefAuditiva));
  Check(trabalhador.InfoDeficiencia.DefMental      = tpNao ,
        'trabalhador.InfoDeficiencia.DefMental | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(trabalhador.InfoDeficiencia.DefMental));
  Check(trabalhador.InfoDeficiencia.DefIntelectual = tpNao ,
        'trabalhador.InfoDeficiencia.DefIntelectual | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(trabalhador.InfoDeficiencia.DefIntelectual));
  Check(trabalhador.InfoDeficiencia.ReabReadap     = tpNao ,
        'trabalhador.InfoDeficiencia.ReabReadap | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(trabalhador.InfoDeficiencia.ReabReadap));
  Check(trabalhador.InfoDeficiencia.infoCota       = tpNao ,
        'trabalhador.InfoDeficiencia.infoCota | Valor esperado:N | Valor recebido:'+eSSimNaoToStr(trabalhador.InfoDeficiencia.infoCota));
  Check(trabalhador.InfoDeficiencia.Observacao     = ''    ,
        'trabalhador.InfoDeficiencia.Observacao | Valor esperado: | Valor recebido:'+trabalhador.InfoDeficiencia.Observacao);

  dependente := trabalhador.Dependente.Items[0];
  Check(dependente.tpDep = tdConjuge, 'trabalhador.dependente.tpDep | Valor esperado:1 | Valor recebido:'+eStpDepToStr(dependente.tpDep));
  Check(dependente.nmDep = 'Nome do Dependente', 'trabalhador.dependente.nmDep | Valor esperado:Nome do Dependente | Valor recebido:'+dependente.nmDep);
  Check(dependente.dtNascto=StoD('20180305000000'), 'trabalhador.dependente.dtNascto | Valor esperado:05/03/2018 | Valor recebido:'+FormatDateBr(dependente.dtNascto));
  Check(dependente.cpfDep = EmptyStr, 'trabalhador.dependente.cpfDep | Valor esperado: | Valor recebido:'+dependente.cpfDep);
  Check(dependente.sexoDep = EmptyStr, 'trabalhador.dependente.sexoDep | Valor esperado: | Valor recebido:'+dependente.sexoDep);
  Check(dependente.depIRRF = tpSim, 'trabalhador.dependente.depIRRF | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(dependente.depIRRF));
  Check(dependente.depSF = tpSim, 'trabalhador.dependente.depSF | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(dependente.depSF));
  Check(dependente.incTrab = tpSim, 'trabalhador.dependente.incTrab | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(dependente.incTrab));

  Check(trabalhador.Contato.FonePrinc='1622334455',
        'trabalhador.contato.fonePrinc | Valor esperado:1622334455 | Valor recebido:'+trabalhador.contato.FonePrinc);

  Check(trabalhador.Contato.EmailPrinc= EmptyStr,
        'trabalhador.contato.emailPrinc | Valor esperado: | Valor recebido:'+trabalhador.contato.EmailPrinc);

  Check(eS2200.Vinculo.matricula = '123' ,
        'vinculo.matricula | Valor esperado:123 | Valor recebido:'+eS2200.vinculo.matricula);

  Check(eS2200.vinculo.tpRegTrab = trCLT,
        'vinculo.tpRegTrab | Valor esperado:1 | Valor recebido:'+eSTpRegTrabToStr(eS2200.vinculo.tpRegTrab));

  Check(eS2200.vinculo.tpRegPrev = rpRGPS,
        'vinculo.tpRegPrev | Valor esperado:1 | Valor recebido:'+eSTpRegPrevToStr(eS2200.vinculo.tpRegPrev));

  Check(eS2200.vinculo.cadIni = tpSim,
        'vinculo.cadIni | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS2200.vinculo.cadIni));

  infoCeletista := eS2200.vinculo.infoRegimeTrab.InfoCeletista;
  Check(infoCeletista.DtAdm = StoD('20180507000000'), 'vinculo.infoRegimeTrab.infoCeletista.dtAdm | Valor esperado:07/05/2018 | Valor recebido:'+FormatDateBr(infoCeletista.DtAdm));
  Check(infoCeletista.TpAdmissao = taAdmissao, 'vinculo.infoRegimeTrab.infoCeletista.tpAdmissao | Valor esperado:1 | Valor recebido:'+eSTpAdmissaoToStr(infoCeletista.TpAdmissao));
  Check(infoCeletista.IndAdmissao = iaNormal, 'vinculo.infoRegimeTrab.infoCeletista.indAdmissao | Valor esperado:1 | Valor recebido:'+eSTpIndAdmissaoToStr(infoCeletista.IndAdmissao));
  Check(infoCeletista.nrProcTrab = EmptyStr, 'vinculo.infoRegimeTrab.infoCeletista.nrProcTrab | Valor esperado: | Valor recebido:'+infoCeletista.nrProcTrab);
  Check(infoCeletista.NatAtividade = navUrbano, 'vinculo.infoRegimeTrab.infoCeletista.natAtividade | Valor esperado:1 | Valor recebido:'+eSNatAtividadeToStr(infoCeletista.NatAtividade));
  Check(infoCeletista.dtBase=5, 'vinculo.infoRegimeTrab.infoCeletista.dtBase | Valor esperado:5 | Valor recebido:'+IntToStr(infoCeletista.dtBase));
  Check(infoCeletista.cnpjSindCategProf='12345678000123', 'vinculo.infoRegimeTrab.infoCeletista.cnpjSindCategProf | Valor esperado:12345678000123 | Valor recebido:'+infoCeletista.cnpjSindCategProf);

  Check(infoCeletista.FGTS.DtOpcFGTS = StoD('20180507000000'),
        'vinculo.infoRegimeTrab.infoCeletista.FGTS.dtOpcFGTS | Valor esperado:07/05/2018 | Valor recebido:'+FormatDateBr(infoCeletista.FGTS.DtOpcFGTS ));

  Check(infoCeletista.TrabTemporario.hipLeg = 1,
        'vinculo.infoRegimeTrab.infoCeletista.trabTemporario.hipLeg | Valor esperado:1 | Valor recebido:'+IntToStr(infoCeletista.TrabTemporario.hipLeg));

  Check(infoCeletista.TrabTemporario.justContr='Justificativa',
        'vinculo.infoRegimeTrab.infoCeletista.trabTemporario.justContr | valor esperado:Justificativa | Valor recebido:'+infoCeletista.TrabTemporario.justContr);

  Check(infoCeletista.TrabTemporario.IdeEstabVinc.TpInsc = tiCNPJ,
        'vinculo.infoRegimeTrab.infoCeletista.trabTemporario.ideEstabVinc.tpInsc | ' +
        'Valor esperado:1 | ' +
        'Valor recebido:'+eSTpInscricaoToStr(infoCeletista.TrabTemporario.IdeEstabVinc.TpInsc));

  Check(infoCeletista.trabTemporario.IdeEstabVinc.NrInsc = '12345678000123',
        'vinculo.infoRegimeTrab.infoCeletista.trabTemporario.ideEstabVinc.nrInsc | '+
        'Valor esperado:12345678000123 | '+
        'Valor recebido:'+infoCeletista.trabTemporario.IdeEstabVinc.NrInsc);

  Check(infoCeletista.trabTemporario.IdeTrabSubstituido[0].CpfTrabSubst = '12345678901',
        'infoCeletista.trabTemporario.ideTrabSubstituido.cpfTrabSubst | '+
        'Valor esperado:12345678901 | '+
        'Valor recebido:'+infoCeletista.trabTemporario.IdeTrabSubstituido[0].CpfTrabSubst);

  Check(infoCeletista.aprend.TpInsc = tiCNPJ,
        'infoCeletista.aprend.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(infoCeletista.aprend.TpInsc));

  Check(infoCeletista.aprend.NrInsc = '12345678000123',
        'infoCeletista.aprend.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+infoCeletista.aprend.NrInsc);

  Check(eS2200.Vinculo.infoContrato.nmCargo='2',
        'vinculo.infoContrato.nmCargo | Valor esperado:2 | Valor recebido:'+eS2200.Vinculo.infoContrato.nmCargo);

  Check(eS2200.vinculo.infoContrato.CBOCargo = '999999',
        'vinculo.infoContrato.CBOCargo | Valor esperado:999999 | Valor recebido:'+eS2200.vinculo.infoContrato.CBOCargo);

  Check(eS2200.Vinculo.infoContrato.dtIngrCargo = StoD('20180507000000'),
        'vinculo.infoContrato.dtIngrCargo | Valor esperado:07/05/2018 | Valor recebido:'+FormatDateBr(eS2200.Vinculo.infoContrato.dtIngrCargo));

  Check(eS2200.vinculo.infoContrato.nmFuncao = 'funcao cargo',
        'vinculo.infoContrato.nmFuncao | Valor esperado:funcao cargo | Valor recebido:'+eS2200.vinculo.infoContrato.nmFuncao);

  Check(eS2200.vinculo.infoContrato.CBOFuncao='999999',
        'vinculo.infoContrato.CBOFuncao | Valor esperado:999999 | Valor recebido:'+eS2200.vinculo.infoContrato.CBOFuncao);

  Check(eS2200.vinculo.infoContrato.acumCargo = snfSim,
        'vinculo.infoContrato.acumCargo | Valor esperado:S | Valor recebido:'+eSSimNaoFacultativoToStr(eS2200.vinculo.infoContrato.acumCargo));

  Check(eS2200.vinculo.infoContrato.CodCateg = 111,
        'vinculo.infoContrato.codCateg | Valor esperado:111 | Valor recebido:'+IntToStr(eS2200.vinculo.infoContrato.CodCateg));

  Check(eS2200.vinculo.infoContrato.Remuneracao.VrSalFx=3000,
        'vinculo.infoContrato.remuneracao.vrSalFx | Valor esperado:3000 | Valor recebido:'+FloatToStr(eS2200.vinculo.infoContrato.Remuneracao.VrSalFx));

  Check(eS2200.vinculo.infoContrato.Remuneracao.UndSalFixo = sfPorHora,
        'vinculo.infoContrato.remuneracao.undSalFixo | Valor esperado:1 | Valor recebido:'+eSUndSalFixoToStr(eS2200.vinculo.infoContrato.Remuneracao.UndSalFixo));

  Check(eS2200.vinculo.infoContrato.Remuneracao.DscSalVar='Descricao',
        'vinculo.infoContrato.remuneracao.dscSalvar | Valor esperado:Descricao | Valor recebido:'+eS2200.vinculo.infoContrato.Remuneracao.DscSalVar);

  Check(eS2200.vinculo.infoContrato.Duracao.TpContr = PrazoIndeterminado,
        'vinculo.infoContrato.duracao.tpContr | Valor esperado:1 | Valor recebido:'+eSTpContrToStr(eS2200.vinculo.infoContrato.Duracao.TpContr));

  Check(eS2200.vinculo.infoContrato.Duracao.clauAssec = tpSim,
        'vinculo.infoContrato.duracao.clauAssec | Valor esperado:S | Valor recebido:'+eSSimNaoToStr(eS2200.vinculo.infoContrato.Duracao.clauAssec));

  Check(eS2200.vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc = tiCNPJ,
        'vinculo.infoContrato.localTrabalho.LocalTrabGeral.tpInsc | ' +
        'Valor esperado:1 | ' +
        'Valor recebido:'+eSTpInscricaoToStr(eS2200.vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc));

  Check(eS2200.vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.NrInsc = '12345678000123',
        'vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.nrInsc | '+
        'Valor esperado:12345678000123 | '+
        'Valor recebido:'+eS2200.vinculo.infoContrato.LocalTrabalho.LocalTrabGeral.NrInsc);

  Check(eS2200.Vinculo.infoContrato.HorContratual.qtdHrsSem=24,
        'vinculo.infoContrato.horContratual.qtdHrsSem | '+
        'Valor esperado:24 | '+
        'Valor recebido:'+FloatToStr(eS2200.Vinculo.infoContrato.HorContratual.qtdHrsSem));

  Check(eS2200.vinculo.infoContrato.HorContratual.tpJornada = tjJornadaComHorarioDiarioFixoEFolhaFixaOutroDiaDaSemana,
        'vinculo.infoContrato.horContratual.tpJornada | '+
        'Valor esperado:6 | ' +
        'Valor recebido:'+eSTpJornadaToStr(eS2200.vinculo.infoContrato.HorContratual.tpJornada));

  Check(eS2200.vinculo.infoContrato.HorContratual.tmpParc = tpNaoeTempoParcial,
        'vinculo.infoContrato.horContratual.tmpParc | '+
        'Valor esperado:0 | '+
        'Valor recebido:'+tpTmpParcToStr(eS2200.vinculo.infoContrato.HorContratual.tmpParc));

  Check(eS2200.Vinculo.infoContrato.HorContratual.horNoturno = tpNao,
        'vinculo.infoContrato.horContratutal.horNoturno | '+
        'Valor esperado:N | '+
        'Valor recebido:'+eSSimNaoToStr(eS2200.Vinculo.infoContrato.HorContratual.horNoturno));

  Check(eS2200.Vinculo.infoContrato.AlvaraJudicial.NrProcJud='12345678901234567890',
        'vinculo.infoContrato.horContratual.alvaraJudicial.nrProcJud | '+
        'Valor esperado:12345678901234567890 | ' +
        'Valor recebido:'+eS2200.Vinculo.infoContrato.AlvaraJudicial.NrProcJud);

  Check(eS2200.Vinculo.infoContrato.observacoes[0].observacao='Observacao 1',
        'vinculo.infoContrato.observacoes.observacao | '+
        'Valor esperado:Observacao 1 | '+
        'Valor recebido:'+eS2200.Vinculo.infoContrato.observacoes[0].observacao);

  Check(eS2200.vinculo.sucessaoVinc.tpInsc = tiCNPJ,
       'vinculo.sucessaoVinc.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+ eSTpInscricaoToStr(eS2200.vinculo.sucessaoVinc.tpInsc));

  Check(eS2200.vinculo.sucessaoVinc.nrInsc = '99999999999999',
        'vinculo.sucessaoVinc.nrInsc | '+
        'Valor esperado:99999999999999 | '+
        'Valor recebido:'+eS2200.vinculo.sucessaoVinc.nrInsc);

  Check(eS2200.Vinculo.sucessaoVinc.matricAnt='123',
       'vinculo.sucessaoVinc.matricAnt | '+
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2200.vinculo.sucessaoVinc.matricAnt);

  Check(eS2200.vinculo.sucessaoVinc.dtTransf = StoD('20180410000000'),
       'vinculo.sucessaoVinc.dtTransf | ' +
       'Valor esperado:10/04/2018 | '+
       'Valor recebido:'+FormatDateBr(eS2200.vinculo.sucessaoVinc.dtTransf));

  Check(eS2200.vinculo.sucessaoVinc.observacao = 'Observacao',
       'vinculo.sucessaoVinc.observacao | ' +
       'Valor esperado:Observacao | ' +
       'Valor recebido:'+eS2200.vinculo.sucessaoVinc.observacao);


end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_vS0100;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0100_S2240);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2240Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_vS0101;
begin
  FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
  try
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromIni(ARQINI_vS0101_S2240);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2240Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2200_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2200.Count = 0, 'Lista de eventos S-2200 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2200_vS0100;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_00_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0100_S2200);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2200Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2200_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0100_S2200);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2200Tests;
    end;
  end;
end;

end.

