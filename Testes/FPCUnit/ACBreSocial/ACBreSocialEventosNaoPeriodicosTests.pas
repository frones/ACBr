unit ACBreSocialEventosNaoPeriodicosTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBrUtil.DateTime, ACBreSocialTestsConsts,
  ACBrDFeException, ACBrDFeSSL, ACBreSocial,  pcesCommon,
  pcesConversaoeSocial,
  pcesS2190,
  pcesS2200,
  pcesS2205,
  pcesS2206,
  pcesS2210,
  pcesS2220,
  pcesS2230,
  pcesS2231,
  pcesS2240,
  pcesS2298,
  pcesS2299,
  pcesS2300,
  pcesS2306,
  pcesS2399,
  pcesS2400,
  pcesS2416,
  pcesS2418,
  pcesS2420,
  pcesS2500,
  pcesS2501,
  pcesS3000,
  pcesS3500;

type

{ TACBreSocialEventosNaoPeriodicosTest }

TACBreSocialEventosNaoPeriodicosTest = class(TTestCase)
  private
    FACBreSocial : TACBreSocial;
    procedure ES2190Tests;
    procedure ES2200Tests;
    procedure ES2205Tests;
    procedure ES2206Tests;
    procedure ES2210Tests;
    procedure ES2220Tests;
    procedure ES2230Tests;
    procedure ES2231Tests;
    procedure ES2240Tests;
    procedure ES2298Tests;
    procedure ES2299Tests;
    procedure ES2300Tests;
    procedure ES2306Tests;
    procedure ES2399Tests;
    procedure ES2416Tests;
    procedure ES2418Tests;
    procedure ES2420Tests;
    procedure ES2500Tests;
    procedure ES2501Tests;
    procedure ES3000Tests;
    procedure ES3500Tests;
  public
    procedure Setup;override;
    procedure TearDown;override;
  published
    procedure ACBreSocialEventosNaoPeriodicosS2190_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2190_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2200_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2200_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2200_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2205_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2205_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2206_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2206_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2210_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2210_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2220_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2220_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2230_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2230_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2231_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2231_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2240_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_vS0100;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2240_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2298_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2298_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2299_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2299_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2300_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2300_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2306_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2306_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2399_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2399_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2400_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2400;
    procedure ACBreSocialEventosNaoPeriodicosS2416_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2416_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2418_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2418_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2420_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2420_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2500_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2500_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS2501_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS2501_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS3000_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS3000_vS0101;
    procedure ACBreSocialEventosNaoPeriodicosS3500_Create_ListaVazia;
    procedure ACBreSocialEventos_LoadFromINI_LeuePreencheuS3500_vS0101;
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

    Check(eSS2400.Beneficiario.cpfBenef    = '99999999999'          , 'Beneficiario.cpfBenef diferente do valor esperado');
    Check(eSS2400.Beneficiario.nmBenefic   = 'Beneficiario'         , 'Beneficiario.nmBenefic diferente do valor esperado');
    Check(eSS2400.Beneficiario.dtNascto    = StrToDate('01/01/2000'), 'Beneficiario.dtNascto diferente do valor esperado');
    Check(eSS2400.Beneficiario.dtInicio    = StrToDate('01/01/2022'), 'Beneficiario.dtInicio diferente do valor esperado');
    Check(eSS2400.Beneficiario.sexo        = 'M'                    , 'Beneficiario.sexo diferente do valor esperado');
    Check(eSS2400.Beneficiario.racaCor     = 1                      , 'Beneficiario.racaCor diferente do valor esperado');
    Check(eSS2400.Beneficiario.estCiv      = 1                      , 'Beneficiario.estCiv diferente do valor esperado');
    Check(eSS2400.Beneficiario.incFisMen   = tpNao                  , 'Beneficiario.incFisMen diferente do valor esperado');
    Check(eSS2400.Beneficiario.dtIncFisMen = StrToDate('01/01/2022'), 'Beneficiario.dtIncFisMen diferente do valor esperado');

    Check(eSS2400.Beneficiario.endereco.Brasil.TpLograd  = 'R'    , 'Beneficiario.endereco.Brasil.TpLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.dscLograd = 'Logradouro de teste', 'Beneficiario.endereco.Brasil.dscLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.nrLograd  = '100'  , 'Beneficiario.endereco.Brasil.nrLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.Complemento = 'Apto 10', 'Beneficiario.endereco.Brasil.Complemento diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.Bairro = 'Centro' , 'Beneficiario.endereco.Brasil.Bairro diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.cep = '14123456'  , 'Beneficiario.endereco.Brasil.cep diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.CodMunic = 3512345, 'Beneficiario.endereco.Brasil.codMunic diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Brasil.uf = 'SP'         , 'Beneficiario.endereco.Brasil.uf diferente do valor esperado');

    Check(eSS2400.Beneficiario.endereco.Exterior.PaisResid = '023' , 'Beneficiario.endereco.Exterior.PaisResid diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Exterior.dscLograd = 'descricao', 'Beneficiario.endereco.Exterior.dscLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Exterior.nrLograd  = '100' , 'Beneficiario.endereco.Exterior.nrLograd diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Exterior.Complemento = 'Apto 10', 'Beneficiario.endereco.Exterior.Complemento diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Exterior.Bairro = 'Centro' , 'Beneficiario.endereco.Exterior.Bairro diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Exterior.NmCid = 'Cidade'  , 'Beneficiario.endereco.Exterior.NmCid diferente do valor esperado');
    Check(eSS2400.Beneficiario.endereco.Exterior.CodPostal = '123456', 'Beneficiario.endereco.Exterior.CodPostal diferente do valor esperado');

    Check(eSS2400.Beneficiario.dependente.Items[0].tpDep = tdConjuge, 'Beneficiario.dependente.tpDep diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].nmDep = 'Conjuge', 'Beneficiario.dependente.nmDep diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].dtNascto = StrToDate('01/01/1995'), 'Beneficiario.depentente.dtNascto diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].cpfDep = '11111111111', 'Beneficiario.dependente.cpfDep diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].sexoDep   = 'F', 'Beneficiario.dependente.sexo diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].depIRRF   = tpNao, 'Beneficiario.dependente.depIRRF diferente do valor esperado');
    Check(eSS2400.Beneficiario.dependente.Items[0].incFisMen = tpNao, 'Beneficiario.depentente.incFisMen diferente do valor esperado');
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2416_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2416.Count = 0, 'Lista de Eventos S-2416 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2416_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2416);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2416Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2418_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2418.Count = 0, 'Lista de Eventos S-2418 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2418_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2418);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2418Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2420_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2420.Count = 0, 'Lista de Eventos S-2420 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2420_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2420);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2420Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2500_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2500.Count = 0, 'Lista de Eventos S-2500 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2500_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2500);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2500Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2501_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2501.Count = 0, 'Lista de Eventos S-2501 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2501_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2501);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2501Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS3000_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S3000.Count = 0, 'Lista de Eventos S-3000 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS3000_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S3000);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES3000Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS3500_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S3500.Count = 0, 'Lista de Eventos S-3500 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS3500_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S3500);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES3500Tests;
    end;
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

procedure TACBreSocialEventosNaoPeriodicosTest.ES2298Tests;
var
  eS2298: TEvtReintegr;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2298.Count > 0, 'Não instanciou o S-2298 na lista');

  eS2298 := FACBreSocial.Eventos.NaoPeriodicos.S2298[0].EvtReintegr;

  Check(eS2298.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2298.Sequencial));

  Check(eS2298.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2298.IdeEvento.indRetif));

  Check(eS2298.IdeEvento.nrRecibo = '1000',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:1000 | ' +
       'Valor recebido:'+eS2298.IdeEvento.nrRecibo);

  Check(eS2298.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2298.IdeEvento.procEmi));

  Check(eS2298.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2298.IdeEvento.verProc);

  Check(eS2298.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2298.IdeEmpregador.tpInsc));

  Check(eS2298.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2298.IdeEmpregador.nrInsc);

  Check(eS2298.IdeVinculo.cpfTrab = '99999999999',
       'IdeVinculo.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2298.IdeVinculo.cpfTrab);

  Check(eS2298.IdeVinculo.matricula = '9999',
       'IdeVinculo.matricula | ' +
       'Valor esperado:9999 | ' +
       'Valor recebido:'+eS2298.IdeVinculo.matricula);

  Check(eS2298.InfoReintegr.tpReint = trReintegracaoDecisaoJudicial,
       'InfoReintegr.tpReint | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpReintToStr(eS2298.InfoReintegr.tpReint));

  Check(eS2298.InfoReintegr.nrProcJud = '99999',
       'InfoReintegr.nrProcJud | ' +
       'Valor esperado:99999 | ' +
       'Valor recebido:'+eS2298.InfoReintegr.nrProcJud);

  Check(eS2298.InfoReintegr.nrLeiAnistia = 'LEI6683_1979',
       'InfoReintegr.nrLeiAnistia | ' +
       'Valor esperado:LEI6683_1979 | ' +
       'Valor recebido:'+eS2298.InfoReintegr.nrLeiAnistia);

  Check(eS2298.InfoReintegr.dtEfetRetorno = StrToDateTime('30/05/2018'),
       'InfoReintegr.dtEfetRetorno | ' +
       'Valor esperado:30/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2298.InfoReintegr.dtEfetRetorno));

  Check(eS2298.InfoReintegr.dtEfeito = StrToDateTime('10/05/2018'),
       'InfoReintegr.dtEfeito | ' +
       'Valor esperado:10/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2298.InfoReintegr.dtEfeito));

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2299Tests;
var
  eS2299: TEvtDeslig;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2299.Count > 0, 'Não instanciou o S-2299 na lista');

  eS2299 := FACBreSocial.Eventos.NaoPeriodicos.S2299[0].EvtDeslig;

  Check(eS2299.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2299.Sequencial));

  Check(eS2299.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2299.IdeEvento.indRetif));

  Check(eS2299.IdeEvento.nrRecibo = '1',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eS2299.IdeEvento.nrRecibo);

  Check(eS2299.IdeEvento.indGuia = '1',
       'IdeEvento.indGuia | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eS2299.IdeEvento.indGuia);

  Check(eS2299.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2299.IdeEvento.procEmi));

  Check(eS2299.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2299.IdeEvento.verProc);

  Check(eS2299.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2299.IdeEmpregador.tpInsc));

  Check(eS2299.IdeEmpregador.nrInsc = '11111111111',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:11111111111 | ' +
       'Valor recebido:'+eS2299.IdeEmpregador.nrInsc);

  Check(eS2299.IdeVinculo.cpfTrab = '99999999999',
       'IdeVinculo.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2299.IdeVinculo.cpfTrab);

  Check(eS2299.IdeVinculo.matricula = '9999999',
       'IdeVinculo.matricula | ' +
       'Valor esperado:9999999 | ' +
       'Valor recebido:'+eS2299.IdeVinculo.matricula);

  Check(eS2299.InfoDeslig.mtvDeslig = '02',
       'InfoDeslig.mtvDeslig | ' +
       'Valor esperado:02 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.mtvDeslig);

  Check(eS2299.InfoDeslig.dtDeslig = StrToDateTime('01/01/2018'),
       'InfoDeslig.dtDeslig | ' +
       'Valor esperado:01/01/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2299.InfoDeslig.dtDeslig));

  Check(eS2299.InfoDeslig.dtAvPrv = StrToDateTime('01/01/2018'),
       'InfoDeslig.dtAvPrv | ' +
       'Valor esperado:01/01/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2299.InfoDeslig.dtAvPrv));

  Check(eS2299.InfoDeslig.indPagtoAPI = tpSim,
       'InfoDeslig.indPagtoAPI | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2299.InfoDeslig.indPagtoAPI));

  Check(eS2299.InfoDeslig.dtProjFimAPI = StrToDateTime('01/02/2018'),
       'InfoDeslig.dtProjFimAPI | ' +
       'Valor esperado:01/02/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2299.InfoDeslig.dtProjFimAPI));

  Check(eS2299.InfoDeslig.pensAlim = paNaoExistePensaoAlimenticia,
       'InfoDeslig.pensAlim | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eSTpPensaoAlimToStr(eS2299.InfoDeslig.pensAlim));

  Check(eS2299.InfoDeslig.percAliment = 0,
       'InfoDeslig.percAliment | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.percAliment));

  Check(eS2299.InfoDeslig.vrAlim = 0,
       'InfoDeslig.vrAlim | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.vrAlim));

  Check(eS2299.InfoDeslig.nrProcTrab = '99999999999999999999',
       'InfoDeslig.nrProcTrab | ' +
       'Valor esperado:99999999999999999999 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.nrProcTrab);

  Check(eS2299.InfoDeslig.infoInterm.Items[0].dia = 30,
       'InfoDeslig.infoInterm.dia | ' +
       'Valor esperado:30 | ' +
       'Valor recebido:'+IntToStr(eS2299.InfoDeslig.infoInterm.Items[0].dia));

  Check(eS2299.InfoDeslig.observacoes.Items[0].observacao = 'observacao',
       'InfoDeslig.observacoes.observacao | ' +
       'Valor esperado:observacao | ' +
       'Valor recebido:'+eS2299.InfoDeslig.observacoes.Items[0].observacao);

  Check(eS2299.InfoDeslig.SucessaoVinc.tpInsc = tiCNPJ,
       'InfoDeslig.SucessaoVinc.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2299.InfoDeslig.SucessaoVinc.tpInsc));

  Check(eS2299.InfoDeslig.SucessaoVinc.nrInsc = '99999999999',
       'InfoDeslig.SucessaoVinc.nrInsc | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.SucessaoVinc.nrInsc);

  Check(eS2299.InfoDeslig.transfTit.cpfSubstituto = '99999999999',
       'InfoDeslig.transfTit.cpfSubstituto | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.transfTit.cpfSubstituto);

  Check(eS2299.InfoDeslig.transfTit.dtNascto = StrToDateTime('01/01/2000'),
       'InfoDeslig.transfTit.dtNascto | ' +
       'Valor esperado:01/01/2000 | ' +
       'Valor recebido:'+DateTimeToStr(eS2299.InfoDeslig.transfTit.dtNascto));

  Check(eS2299.InfoDeslig.mudancaCPF.novoCPF = '99999999999',
       'InfoDeslig.mudancaCPF.novoCPF | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.mudancaCPF.novoCPF);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].ideDmDev = 'identifica',
       'InfoDeslig.VerbasResc.dmDev.ideDmDev | ' +
       'Valor esperado:identifica | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].ideDmDev);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].indRRA = snfSim,
       'InfoDeslig.VerbasResc.dmDev.indRRA | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].indRRA));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.tpProcRRA = tppAdministrativo,
       'InfoDeslig.VerbasResc.dmDev.infoRRA.tpProcRRA | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpProcRRAToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.tpProcRRA));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.nrProcRRA = '12345678901234567890',
       'InfoDeslig.VerbasResc.dmDev.infoRRA.nrProcRRA | ' +
       'Valor esperado:12345678901234567890 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.nrProcRRA);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.descRRA = '123',
       'InfoDeslig.VerbasResc.dmDev.infoRRA.descRRA | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.descRRA);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.qtdMesesRRA = 9,
       'InfoDeslig.VerbasResc.dmDev.infoRRA.qtdMesesRRA | ' +
       'Valor esperado:9 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.qtdMesesRRA));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespCustas = 10,
       'InfoDeslig.VerbasResc.dmDev.infoRRA.despProcJud.vlrDespCustas | ' +
       'Valor esperado:10 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespCustas));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespAdvogados = 20,
       'InfoDeslig.VerbasResc.dmDev.infoRRA.despProcJud.vlrDespAdvogados | ' +
       'Valor esperado:20 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespAdvogados));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].tpInsc = tiCNPJ,
       'InfoDeslig.VerbasResc.dmDev.infoRRA.ideAdv.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].tpInsc));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].nrInsc = '22222222222',
       'InfoDeslig.VerbasResc.dmDev.infoRRA.ideAdv.nrInsc | ' +
       'Valor esperado:22222222222 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].nrInsc);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].vlrAdv = 1000,
       'InfoDeslig.VerbasResc.dmDev.infoRRA.ideAdv.vlrAdv | ' +
       'Valor esperado:1000 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].vlrAdv));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].tpInsc = tiCNPJ,
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].tpInsc));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].nrInsc = '333333333333',
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.nrInsc | ' +
       'Valor esperado:333333333333 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].nrInsc);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].codLotacao = '01',
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.codLotacao | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].codLotacao);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].codRubr = '001',
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.detVerbas.codRubr | ' +
       'Valor esperado:001 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].codRubr);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].ideTabRubr = 'A01',
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.detVerbas.ideTabRubr | ' +
       'Valor esperado:A01 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].ideTabRubr);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].qtdRubr = 1,
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.detVerbas.qtdRubr | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].qtdRubr));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].fatorRubr = 1,
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.detVerbas.fatorRubr | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].fatorRubr));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].vrRubr = 10,
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.detVerbas.vrRubr | ' +
       'Valor esperado:10 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].vrRubr));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].indApurIR = tiaiNormal,
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.detVerbas.indApurIR | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eSTpIndApurIRToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].detVerbas.Items[0].indApurIR));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].infoAgNocivo.grauExp = ge1,
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.infoAgNocivo.grauExp | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSGrauExpToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].infoAgNocivo.grauExp));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].infoSimples.indSimples = idsIntegralmente,
       'InfoDeslig.VerbasResc.dmDev.infoPerApur.ideEstabLot.infoSimples.indSimples | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndSimplesToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerApur.ideEstabLot.Items[0].infoSimples.indSimples));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dtAcConv = StrToDateTime('01/01/2018'),
       'InfoDeslig.VerbasResc.dmDev.infoPerAnt.ideADC.dtAcConv | ' +
       'Valor esperado:01/01/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dtAcConv));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].tpAcConv = tacAcordoColTrab,
       'InfoDeslig.VerbasResc.dmDev.infoPerAnt.ideADC.tpAcConv | ' +
       'Valor esperado:A | ' +
       'Valor recebido:'+eSTpAcConvToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].tpAcConv));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dsc = 'descricao',
       'InfoDeslig.VerbasResc.dmDev.infoPerAnt.ideADC.dsc | ' +
       'Valor esperado:descricao | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].dsc);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].perRef = '2018-01',
       'InfoDeslig.VerbasResc.dmDev.infoPerAnt.ideADC.idePeriodo.perRef | ' +
       'Valor esperado:2018-01 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].perRef);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].tpInsc = tiCNPJ,
       'InfoDeslig.VerbasResc.dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].tpInsc));

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].nrInsc = '444444444444',
       'InfoDeslig.VerbasResc.dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.nrInsc | ' +
       'Valor esperado:444444444444 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].nrInsc);

  Check(eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].codLotacao = '001',
       'InfoDeslig.VerbasResc.dmDev.infoPerAnt.ideADC.idePeriodo.ideEstabLot.codLotacao | ' +
       'Valor esperado:001 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.dmDev.Items[0].infoPerAnt.ideADC.Items[0].idePeriodo.Items[0].ideEstabLot.Items[0].codLotacao);

  Check(eS2299.InfoDeslig.VerbasResc.procJudTrab.Items[0].tpTrib = tptIRRF,
       'InfoDeslig.VerbasResc.procJudTrab.tpTrib | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eSTpTributoToStr(eS2299.InfoDeslig.VerbasResc.procJudTrab.Items[0].tpTrib));

  Check(eS2299.InfoDeslig.VerbasResc.procJudTrab.Items[0].nrProcJud = '10000000001000000000',
       'InfoDeslig.VerbasResc.procJudTrab.nrProcJud | ' +
       'Valor esperado:10000000001000000000 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.procJudTrab.Items[0].nrProcJud);

  Check(eS2299.InfoDeslig.VerbasResc.procJudTrab.Items[0].codSusp = '0',
       'InfoDeslig.VerbasResc.procJudTrab.codSusp | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.procJudTrab.Items[0].codSusp);

  Check(eS2299.InfoDeslig.VerbasResc.infoMV.indMV = imvDescontadaempregador,
       'InfoDeslig.VerbasResc.infoMV.indMV | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndMVToStr(eS2299.InfoDeslig.VerbasResc.infoMV.indMV));

  Check(eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].tpInsc = tiCNPJ,
       'InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].tpInsc));

  Check(eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].nrInsc = '55555555555',
       'InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.nrInsc | ' +
       'Valor esperado:55555555555 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].nrInsc);

  Check(eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].codCateg = 123,
       'InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.codCateg | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+IntToStr(eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].codCateg));

  Check(eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].vlrRemunOE = 100,
       'InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.vlrRemunOE | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2299.InfoDeslig.VerbasResc.infoMV.remunOutrEmpr.Items[0].vlrRemunOE));

  Check(eS2299.InfoDeslig.VerbasResc.ProcCS.nrProcJud = '12345678901234567890',
       'InfoDeslig.VerbasResc.ProcCS.nrProcJud | ' +
       'Valor esperado:12345678901234567890 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.VerbasResc.ProcCS.nrProcJud);

  Check(eS2299.InfoDeslig.remunAposDeslig.indRemun = ireQuarentena,
       'InfoDeslig.remunAposDeslig.indRemun | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+TpIndRemunToStr(eS2299.InfoDeslig.remunAposDeslig.indRemun));

  Check(eS2299.InfoDeslig.remunAposDeslig.dtFimRemun = StrToDateTime('01/02/2018'),
       'InfoDeslig.remunAposDeslig.dtFimRemun | ' +
       'Valor esperado:01/02/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2299.InfoDeslig.remunAposDeslig.dtFimRemun));

  Check(eS2299.InfoDeslig.consigFGTS.Items[0].insConsig = '12345',
       'InfoDeslig.consigFGTS.Items[0].insConsig | ' +
       'Valor esperado:12345 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.consigFGTS.Items[0].insConsig);

  Check(eS2299.InfoDeslig.consigFGTS.Items[0].nrContr = '99999',
       'InfoDeslig.consigFGTS.Items[0].nrContr | ' +
       'Valor esperado:99999 | ' +
       'Valor recebido:'+eS2299.InfoDeslig.consigFGTS.Items[0].nrContr);

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2300Tests;
var
  eS2300: TEvtTSVInicio;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2300.Count > 0, 'Não instanciou o S-2300 na lista');

  eS2300 := FACBreSocial.Eventos.NaoPeriodicos.S2300[0].EvtTSVInicio;

  Check(eS2300.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2300.Sequencial));

  Check(eS2300.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2300.IdeEvento.indRetif));

  Check(eS2300.IdeEvento.nrRecibo = '9999',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:9999 | ' +
       'Valor recebido:'+eS2300.IdeEvento.nrRecibo);

  Check(eS2300.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2300.IdeEvento.procEmi));

  Check(eS2300.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2300.IdeEvento.verProc);

  Check(eS2300.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2300.IdeEmpregador.tpInsc));

  Check(eS2300.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2300.IdeEmpregador.nrInsc);

  Check(eS2300.Trabalhador.cpfTrab = '99999999999',
       'Trabalhador.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2300.Trabalhador.cpfTrab);

  Check(eS2300.Trabalhador.NmTrab = 'nome',
       'Trabalhador.NmTrab | ' +
       'Valor esperado:nome | ' +
       'Valor recebido:'+eS2300.Trabalhador.NmTrab);

  Check(eS2300.Trabalhador.Sexo = 'M',
       'Trabalhador.Sexo | ' +
       'Valor esperado:M | ' +
       'Valor recebido:'+eS2300.Trabalhador.Sexo);

  Check(eS2300.Trabalhador.RacaCor = 1,
       'Trabalhador.RacaCor | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+IntToStr(eS2300.Trabalhador.RacaCor));

  Check(eS2300.Trabalhador.EstCiv = 1,
       'Trabalhador.EstCiv | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+IntToStr(eS2300.Trabalhador.EstCiv));

  Check(eS2300.Trabalhador.GrauInstr = '01',
       'Trabalhador.GrauInstr | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eS2300.Trabalhador.GrauInstr);

  Check(eS2300.Trabalhador.nmSoc = 'nome',
       'Trabalhador.nmSoc | ' +
       'Valor esperado:nome | ' +
       'Valor recebido:'+eS2300.Trabalhador.nmSoc);

  Check(eS2300.Trabalhador.Nascimento.dtNascto = StrToDateTime('01/01/2000'),
       'Trabalhador.Nascimento.dtNascto | ' +
       'Valor esperado:01/01/2000 | ' +
       'Valor recebido:'+DateTimeToStr(eS2300.Trabalhador.Nascimento.dtNascto));

  Check(eS2300.Trabalhador.Nascimento.paisNascto = '063',
       'Trabalhador.Nascimento.paisNascto | ' +
       'Valor esperado:063 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Nascimento.paisNascto);

  Check(eS2300.Trabalhador.Nascimento.PaisNac = '105',
       'Trabalhador.Nascimento.PaisNac | ' +
       'Valor esperado:105 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Nascimento.PaisNac);

  Check(eS2300.Trabalhador.Endereco.Brasil.TpLograd = 'Rua',
       'Trabalhador.Endereco.Brasil.TpLograd | ' +
       'Valor esperado:Rua | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Brasil.TpLograd);

  Check(eS2300.Trabalhador.Endereco.Brasil.DscLograd = 'Central',
       'Trabalhador.Endereco.Brasil.DscLograd | ' +
       'Valor esperado:Central | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Brasil.DscLograd);

  Check(eS2300.Trabalhador.Endereco.Brasil.NrLograd = '100',
       'Trabalhador.Endereco.Brasil.NrLograd | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Brasil.NrLograd);

  Check(eS2300.Trabalhador.Endereco.Brasil.Complemento = 'Apto 10',
       'Trabalhador.Endereco.Brasil.Complemento | ' +
       'Valor esperado:Apto 10 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Brasil.Complemento);

  Check(eS2300.Trabalhador.Endereco.Brasil.Bairro = 'Centro',
       'Trabalhador.Endereco.Brasil.Bairro | ' +
       'Valor esperado:Centro | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Brasil.Bairro);

  Check(eS2300.Trabalhador.Endereco.Brasil.Cep = '14123456',
       'Trabalhador.Endereco.Brasil.Cep | ' +
       'Valor esperado:14123456 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Brasil.Cep);

  Check(eS2300.Trabalhador.Endereco.Brasil.CodMunic = 3512345,
       'Trabalhador.Endereco.Brasil.CodMunic | ' +
       'Valor esperado:3512345 | ' +
       'Valor recebido:'+IntToStr(eS2300.Trabalhador.Endereco.Brasil.CodMunic));

  Check(eS2300.Trabalhador.Endereco.Brasil.UF = 'SP',
       'Trabalhador.Endereco.Brasil.UF | ' +
       'Valor esperado:SP | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Brasil.UF);

  Check(eS2300.Trabalhador.Endereco.Exterior.PaisResid = 'Chile',
       'Trabalhador.Endereco.Exterior.PaisResid | ' +
       'Valor esperado:Chile | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Exterior.PaisResid);

  Check(eS2300.Trabalhador.Endereco.Exterior.DscLograd = 'Rua Santiago',
       'Trabalhador.Endereco.Exterior.DscLograd | ' +
       'Valor esperado:Rua Santiago | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Exterior.DscLograd);

  Check(eS2300.Trabalhador.Endereco.Exterior.NrLograd = '1000',
       'Trabalhador.Endereco.Exterior.NrLograd | ' +
       'Valor esperado:1000 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Exterior.NrLograd);

  Check(eS2300.Trabalhador.Endereco.Exterior.Complemento = 'Apto 123',
       'Trabalhador.Endereco.Exterior.Complemento | ' +
       'Valor esperado:Apto 123 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Exterior.Complemento);

  Check(eS2300.Trabalhador.Endereco.Exterior.Bairro = 'Centro',
       'Trabalhador.Endereco.Exterior.Bairro | ' +
       'Valor esperado:Centro | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Exterior.Bairro);

  Check(eS2300.Trabalhador.Endereco.Exterior.NmCid = 'Santiago',
       'Trabalhador.Endereco.Exterior.NmCid | ' +
       'Valor esperado:Santiago | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Exterior.NmCid);

  Check(eS2300.Trabalhador.Endereco.Exterior.CodPostal = '54321',
       'Trabalhador.Endereco.Exterior.CodPostal | ' +
       'Valor esperado:54321 | ' +
       'Valor recebido:'+eS2300.Trabalhador.Endereco.Exterior.CodPostal);

  Check(eS2300.Trabalhador.trabImig.tmpResid = ttrPrazoIndeterminado,
       'Trabalhador.trabImig.tmpResid | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+tpTmpResidToStr(eS2300.Trabalhador.trabImig.tmpResid));

  Check(eS2300.Trabalhador.trabImig.condIng = tciRefugiado,
       'Trabalhador.trabImig.condIng | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+tpCondIngToStr(eS2300.Trabalhador.trabImig.condIng));

  Check(eS2300.Trabalhador.InfoDeficiencia.DefFisica = tpNao,
       'Trabalhador.InfoDeficiencia.DefFisica | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.InfoDeficiencia.DefFisica));

  Check(eS2300.Trabalhador.InfoDeficiencia.DefVisual = tpNao,
       'Trabalhador.InfoDeficiencia.DefVisual | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.InfoDeficiencia.DefVisual));

  Check(eS2300.Trabalhador.InfoDeficiencia.DefAuditiva = tpNao,
       'Trabalhador.InfoDeficiencia.DefAuditiva | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.InfoDeficiencia.DefAuditiva));

  Check(eS2300.Trabalhador.InfoDeficiencia.DefMental = tpNao,
       'Trabalhador.InfoDeficiencia.DefMental | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.InfoDeficiencia.DefMental));

  Check(eS2300.Trabalhador.InfoDeficiencia.DefIntelectual = tpNao,
       'Trabalhador.InfoDeficiencia.DefIntelectual | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.InfoDeficiencia.DefIntelectual));

  Check(eS2300.Trabalhador.InfoDeficiencia.ReabReadap = tpNao,
       'Trabalhador.InfoDeficiencia.ReabReadap | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.InfoDeficiencia.ReabReadap));

  Check(eS2300.Trabalhador.InfoDeficiencia.Observacao = 'observacao',
       'Trabalhador.InfoDeficiencia.Observacao | ' +
       'Valor esperado:observacao | ' +
       'Valor recebido:'+eS2300.Trabalhador.InfoDeficiencia.Observacao);

  Check(eS2300.Trabalhador.Dependente.Items[0].tpDep = tdConjuge,
       'Trabalhador.Dependente.Items[0].tpDep | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eStpDepToStr(eS2300.Trabalhador.Dependente.Items[0].tpDep));

  Check(eS2300.Trabalhador.Dependente.Items[0].nmDep = 'nome',
     'Trabalhador.Dependente.Items[0].nmDep | ' +
     'Valor esperado:nome | ' +
     'Valor recebido:'+eS2300.Trabalhador.Dependente.Items[0].nmDep);

  Check(eS2300.Trabalhador.Dependente.Items[0].dtNascto = StrToDateTime('01/01/2000'),
     'Trabalhador.Dependente.Items[0].dtNascto | ' +
     'Valor esperado:01/01/2000 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.Trabalhador.Dependente.Items[0].dtNascto));

  Check(eS2300.Trabalhador.Dependente.Items[0].cpfDep = '99999999999',
     'Trabalhador.Dependente.Items[0].cpfDep | ' +
     'Valor esperado:99999999999 | ' +
     'Valor recebido:'+eS2300.Trabalhador.Dependente.Items[0].cpfDep);

  Check(eS2300.Trabalhador.Dependente.Items[0].depIRRF = tpSim,
     'Trabalhador.Dependente.Items[0].depIRRF | ' +
     'Valor esperado:S | ' +
     'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.Dependente.Items[0].depIRRF));

  Check(eS2300.Trabalhador.Dependente.Items[0].depSF = tpSim,
     'Trabalhador.Dependente.Items[0].depSF | ' +
     'Valor esperado:S | ' +
     'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.Dependente.Items[0].depSF));

  Check(eS2300.Trabalhador.Dependente.Items[0].incTrab = tpSim,
     'Trabalhador.Dependente.Items[0].incTrab | ' +
     'Valor esperado:S | ' +
     'Valor recebido:'+eSSimNaoToStr(eS2300.Trabalhador.Dependente.Items[0].incTrab));

  Check(eS2300.Trabalhador.Contato.FonePrinc = '33445566',
     'Trabalhador.Contato.FonePrinc | ' +
     'Valor esperado:33445566 | ' +
     'Valor recebido:'+eS2300.Trabalhador.Contato.FonePrinc);

  Check(eS2300.Trabalhador.Contato.emailPrinc = 'a@a.com.br',
     'Trabalhador.Contato.emailPrinc | ' +
     'Valor esperado:a@a.com.br | ' +
     'Valor recebido:'+eS2300.Trabalhador.Contato.emailPrinc);

  Check(eS2300.infoTSVInicio.cadIni = tpSim,
     'infoTSVInicio.cadIni | ' +
     'Valor esperado:S | ' +
     'Valor recebido:'+eSSimNaoToStr(eS2300.infoTSVInicio.cadIni));

  Check(eS2300.infoTSVInicio.matricula = '123',
     'infoTSVInicio.matricula | ' +
     'Valor esperado:123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.matricula);

  Check(eS2300.infoTSVInicio.codCateg = 101,
     'infoTSVInicio.codCateg | ' +
     'Valor esperado:101 | ' +
     'Valor recebido:'+IntToStr(eS2300.infoTSVInicio.codCateg));

  Check(eS2300.infoTSVInicio.dtInicio = StrToDateTime('01/01/2018'),
     'infoTSVInicio.dtInicio | ' +
     'Valor esperado:01/01/2018 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.dtInicio));

  Check(eS2300.infoTSVInicio.nrProcTrab = '123456',
     'infoTSVInicio.nrProcTrab | ' +
     'Valor esperado:123456 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.nrProcTrab);

  Check(eS2300.infoTSVInicio.natAtividade = navUrbano,
     'infoTSVInicio.natAtividade | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSNatAtividadeToStr(eS2300.infoTSVInicio.natAtividade));

  Check(eS2300.infoTSVInicio.infoComplementares.cargoFuncao.nmCargo = 'cargo',
     'infoTSVInicio.infoComplementares.cargoFuncao.nmCargo | ' +
     'Valor esperado:cargo | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.cargoFuncao.nmCargo);

  Check(eS2300.infoTSVInicio.infoComplementares.cargoFuncao.CBOCargo = '123456',
     'infoTSVInicio.infoComplementares.cargoFuncao.CBOCargo | ' +
     'Valor esperado:123456 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.cargoFuncao.CBOCargo);

  Check(eS2300.infoTSVInicio.infoComplementares.cargoFuncao.nmFuncao = 'funcao',
     'infoTSVInicio.infoComplementares.cargoFuncao.nmFuncao | ' +
     'Valor esperado:funcao | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.cargoFuncao.nmFuncao);

  Check(eS2300.infoTSVInicio.infoComplementares.cargoFuncao.CBOFuncao = '123456',
     'infoTSVInicio.infoComplementares.cargoFuncao.CBOFuncao | ' +
     'Valor esperado:123456 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.cargoFuncao.CBOFuncao);

  Check(eS2300.infoTSVInicio.infoComplementares.Remuneracao.VrSalFx = 1000,
     'infoTSVInicio.infoComplementares.Remuneracao.VrSalFx | ' +
     'Valor esperado:1000 | ' +
     'Valor recebido:'+FloatToStr(eS2300.infoTSVInicio.infoComplementares.Remuneracao.VrSalFx));

  Check(eS2300.infoTSVInicio.infoComplementares.Remuneracao.UndSalFixo = sfPorHora,
     'infoTSVInicio.infoComplementares.Remuneracao.UndSalFixo | ' +
     'Valor esperado:sfPorHora | ' +
     'Valor recebido:'+eSUndSalFixoToStr(eS2300.infoTSVInicio.infoComplementares.Remuneracao.UndSalFixo));

  Check(eS2300.infoTSVInicio.infoComplementares.Remuneracao.DscSalVar = 'descricao',
     'infoTSVInicio.infoComplementares.Remuneracao.DscSalVar | ' +
     'Valor esperado:descricao | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.Remuneracao.DscSalVar);

  Check(eS2300.infoTSVInicio.infoComplementares.Fgts.DtOpcFGTS = StrToDateTime('01/01/2023'),
     'infoTSVInicio.infoComplementares.Fgts.DtOpcFGTS | ' +
     'Valor esperado:01/01/2023 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.infoComplementares.Fgts.DtOpcFGTS));

  Check(eS2300.infoTSVInicio.infoComplementares.infoDirSind.categOrig = 1,
     'infoTSVInicio.infoComplementares.infoDirSind.categOrig | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+IntToStr(eS2300.infoTSVInicio.infoComplementares.infoDirSind.categOrig));

  Check(eS2300.infoTSVInicio.infoComplementares.infoDirSind.tpInsc = tiCNPJ,
     'infoTSVInicio.infoComplementares.infoDirSind.tpInsc | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpInscricaoToStr(eS2300.infoTSVInicio.infoComplementares.infoDirSind.tpInsc));

  Check(eS2300.infoTSVInicio.infoComplementares.infoDirSind.nrInsc = '9999999999',
     'infoTSVInicio.infoComplementares.infoDirSind.nrInsc | ' +
     'Valor esperado:9999999999 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoDirSind.nrInsc);

  Check(eS2300.infoTSVInicio.infoComplementares.infoDirSind.dtAdmOrig = StrToDateTime('01/01/2023'),
     'infoTSVInicio.infoComplementares.infoDirSind.dtAdmOrig | ' +
     'Valor esperado:01/01/2023 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.infoComplementares.infoDirSind.dtAdmOrig));

  Check(eS2300.infoTSVInicio.infoComplementares.infoDirSind.matricOrig = '123456',
     'infoTSVInicio.infoComplementares.infoDirSind.matricOrig | ' +
     'Valor esperado:123456 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoDirSind.matricOrig);

  Check(eS2300.infoTSVInicio.infoComplementares.infoDirSind.tpRegTrab = trCLT,
     'infoTSVInicio.infoComplementares.infoDirSind.tpRegTrab | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegTrabToStr(eS2300.infoTSVInicio.infoComplementares.infoDirSind.tpRegTrab));

  Check(eS2300.infoTSVInicio.infoComplementares.infoDirSind.tpRegPrev = rpRGPS,
     'infoTSVInicio.infoComplementares.infoDirSind.tpRegPrev | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegPrevToStr(eS2300.infoTSVInicio.infoComplementares.infoDirSind.tpRegPrev));

  Check(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.categOrig = 1,
     'infoTSVInicio.infoComplementares.infoTrabCedido.categOrig | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+IntToStr(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.categOrig));

  Check(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.cnpjCednt = '12345678000123',
     'infoTSVInicio.infoComplementares.infoTrabCedido.cnpjCednt | ' +
     'Valor esperado:12345678000123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.cnpjCednt);

  Check(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.matricCed = '456',
     'infoTSVInicio.infoComplementares.infoTrabCedido.matricCed | ' +
     'Valor esperado:456 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.matricCed);

  Check(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.dtAdmCed = StrToDateTime('01/01/2023'),
     'infoTSVInicio.infoComplementares.infoTrabCedido.dtAdmCed | ' +
     'Valor esperado:01/01/2023 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.dtAdmCed));

  Check(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.tpRegTrab = trCLT,
     'infoTSVInicio.infoComplementares.infoTrabCedido.tpRegTrab | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegTrabToStr(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.tpRegTrab));

  Check(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.tpRegPrev = rpRGPS,
     'infoTSVInicio.infoComplementares.infoTrabCedido.tpRegPrev | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegPrevToStr(eS2300.infoTSVInicio.infoComplementares.infoTrabCedido.tpRegPrev));

  Check(eS2300.infoTSVInicio.infoComplementares.infoMandElet.categOrig = 1,
     'infoTSVInicio.infoComplementares.infoMandElet.categOrig | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+IntToStr(eS2300.infoTSVInicio.infoComplementares.infoMandElet.categOrig));

  Check(eS2300.infoTSVInicio.infoComplementares.infoMandElet.cnpjOrig = '12345678000123',
     'infoTSVInicio.infoComplementares.infoMandElet.cnpjOrig | ' +
     'Valor esperado:12345678000123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoMandElet.cnpjOrig);

  Check(eS2300.infoTSVInicio.infoComplementares.infoMandElet.matricOrig = '456',
     'infoTSVInicio.infoComplementares.infoMandElet.matricOrig | ' +
     'Valor esperado:456 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoMandElet.matricOrig);

  Check(eS2300.infoTSVInicio.infoComplementares.infoMandElet.dtExercOrig = StrToDateTime('01/01/2023'),
     'infoTSVInicio.infoComplementares.infoMandElet.dtExercOrig | ' +
     'Valor esperado:01/01/2023 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.infoComplementares.infoMandElet.dtExercOrig));

  Check(eS2300.infoTSVInicio.infoComplementares.infoMandElet.indRemunCargo = snfSim,
     'infoTSVInicio.infoComplementares.infoMandElet.indRemunCargo | ' +
     'Valor esperado:S | ' +
     'Valor recebido:'+eSSimNaoFacultativoToStr(eS2300.infoTSVInicio.infoComplementares.infoMandElet.indRemunCargo));

  Check(eS2300.infoTSVInicio.infoComplementares.infoMandElet.tpRegTrab = trCLT,
     'infoTSVInicio.infoComplementares.infoMandElet.tpRegTrab | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegTrabToStr(eS2300.infoTSVInicio.infoComplementares.infoMandElet.tpRegTrab));

  Check(eS2300.infoTSVInicio.infoComplementares.infoMandElet.tpRegPrev = rpRGPS,
     'infoTSVInicio.infoComplementares.infoMandElet.tpRegPrev | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegPrevToStr(eS2300.infoTSVInicio.infoComplementares.infoMandElet.tpRegPrev));

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.natEstagio = neObrigatiorio,
     'infoTSVInicio.infoComplementares.infoEstagiario.natEstagio | ' +
     'Valor esperado:O | ' +
     'Valor recebido:'+eSTpNatEstagioToStr(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.natEstagio));

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.nivEstagio = nvFundamental,
     'infoTSVInicio.infoComplementares.infoEstagiario.nivEstagio | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpNivelEstagioToStr(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.nivEstagio));

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.areaAtuacao = 'informatica',
     'infoTSVInicio.infoComplementares.infoEstagiario.areaAtuacao | ' +
     'Valor esperado:informatica | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.areaAtuacao);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.nrApol = '123',
     'infoTSVInicio.infoComplementares.infoEstagiario.nrApol | ' +
     'Valor esperado:123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.nrApol);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.dtPrevTerm = StrToDateTime('01/01/2018'),
     'infoTSVInicio.infoComplementares.infoEstagiario.dtPrevTerm | ' +
     'Valor esperado:01/01/2018 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.dtPrevTerm));

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino = '12345678000123',
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino | ' +
     'Valor esperado:12345678000123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nmRazao = 'Razao Social',
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nmRazao | ' +
     'Valor esperado:Razao Social | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nmRazao);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.dscLograd = 'Rua Central',
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.dscLograd | ' +
     'Valor esperado:Rua Central | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.dscLograd);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nrLograd = '123',
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nrLograd | ' +
     'Valor esperado:123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.nrLograd);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.bairro = 'Centro',
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.bairro | ' +
     'Valor esperado:Centro | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.bairro);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Cep = '14123456',
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Cep | ' +
     'Valor esperado:14123456 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Cep);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.codMunic = 3512345,
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.codMunic | ' +
     'Valor esperado:3512345 | ' +
     'Valor recebido:'+IntToStr(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.codMunic));

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Uf = 'SP',
     'infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Uf | ' +
     'Valor esperado:SP | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.instEnsino.Uf);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg = '12345678000123',
     'infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg | ' +
     'Valor esperado:12345678000123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg);

  Check(eS2300.infoTSVInicio.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor = '99999999999',
     'infoTSVInicio.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor | ' +
     'Valor esperado:99999999999 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor);

  Check(eS2300.infoTSVInicio.mudancaCPF.cpfAnt = '99999999999',
     'infoTSVInicio.mudancaCPF.cpfAnt | ' +
     'Valor esperado:99999999999 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.mudancaCPF.cpfAnt);

  Check(eS2300.infoTSVInicio.mudancaCPF.matricAnt = '123',
     'infoTSVInicio.mudancaCPF.matricAnt | ' +
     'Valor esperado:123 | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.mudancaCPF.matricAnt);

  Check(eS2300.infoTSVInicio.mudancaCPF.dtAltCPF = StrToDateTime('01/01/2023'),
     'infoTSVInicio.mudancaCPF.dtAltCPF | ' +
     'Valor esperado:01/01/2023 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.mudancaCPF.dtAltCPF));

  Check(eS2300.infoTSVInicio.mudancaCPF.observacao = 'observacao',
     'infoTSVInicio.mudancaCPF.observacao | ' +
     'Valor esperado:observacao | ' +
     'Valor recebido:'+eS2300.infoTSVInicio.mudancaCPF.observacao);

  Check(eS2300.infoTSVInicio.afastamento.DtIniAfast = StrToDateTime('01/01/2023'),
     'infoTSVInicio.afastamento.DtIniAfast | ' +
     'Valor esperado:01/01/2023 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.afastamento.DtIniAfast));

  Check(eS2300.infoTSVInicio.afastamento.codMotAfast = mtvAcidenteDoencaTrabalho,
     'infoTSVInicio.afastamento.codMotAfast | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eStpMotivosAfastamentoToStr(eS2300.infoTSVInicio.afastamento.codMotAfast));

  Check(eS2300.infoTSVInicio.termino.dtTerm = StrToDateTime('01/01/2023'),
     'infoTSVInicio.termino.dtTerm | ' +
     'Valor esperado:01/01/2023 | ' +
     'Valor recebido:'+DateTimeToStr(eS2300.infoTSVInicio.termino.dtTerm));

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2306Tests;
var
  eS2306: TEvtTSVAltContr;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2306.Count > 0, 'Não instanciou o S-2306 na lista');

  eS2306 := FACBreSocial.Eventos.NaoPeriodicos.S2306[0].EvtTSVAltContr;

  Check(eS2306.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2306.Sequencial));

  Check(eS2306.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2306.IdeEvento.indRetif));

  Check(eS2306.IdeEvento.nrRecibo = '9999',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:9999 | ' +
       'Valor recebido:'+eS2306.IdeEvento.nrRecibo);

  Check(eS2306.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2306.IdeEvento.procEmi));

  Check(eS2306.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2306.IdeEvento.verProc);

  Check(eS2306.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2306.IdeEmpregador.tpInsc));

  Check(eS2306.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2306.IdeEmpregador.nrInsc);

  Check(eS2306.IdeTrabSemVinc.cpfTrab = '99999999999',
       'IdeTrabSemVinc.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2306.IdeTrabSemVinc.cpfTrab);

  Check(eS2306.IdeTrabSemVinc.matricula = '9999999',
       'IdeTrabSemVinc.matricula | ' +
       'Valor esperado:9999999 | ' +
       'Valor recebido:'+eS2306.IdeTrabSemVinc.matricula);

  Check(eS2306.IdeTrabSemVinc.codCateg = 555,
       'IdeTrabSemVinc.codCateg | ' +
       'Valor esperado:555 | ' +
       'Valor recebido:'+IntToStr(eS2306.IdeTrabSemVinc.codCateg));

  Check(eS2306.infoTSVAlteracao.dtAlteracao = StrToDateTime('01/01/2018'),
       'infoTSVAlteracao.dtAlteracao | ' +
       'Valor esperado:01/01/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2306.infoTSVAlteracao.dtAlteracao));

  Check(eS2306.infoTSVAlteracao.natAtividade = navUrbano,
       'infoTSVAlteracao.natAtividade | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSNatAtividadeToStr(eS2306.infoTSVAlteracao.natAtividade));

  Check(eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.nmCargo = 'nome',
       'infoTSVAlteracao.infoComplementares.cargoFuncao.nmCargo | ' +
       'Valor esperado:nome | ' +
       'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.nmCargo);

  Check(eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.CBOCargo = '123456',
       'infoTSVAlteracao.infoComplementares.cargoFuncao.CBOCargo | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.CBOCargo);

  Check(eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.nmFuncao = 'nome',
       'infoTSVAlteracao.infoComplementares.cargoFuncao.nmFuncao | ' +
       'Valor esperado:nome | ' +
       'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.nmFuncao);

  Check(eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.CBOFuncao = '654321',
       'infoTSVAlteracao.infoComplementares.cargoFuncao.CBOFuncao | ' +
       'Valor esperado:654321 | ' +
       'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.cargoFuncao.CBOFuncao);

  Check(eS2306.infoTSVAlteracao.infoComplementares.Remuneracao.VrSalFx = 100,
       'infoTSVAlteracao.infoComplementares.Remuneracao.VrSalFx | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2306.infoTSVAlteracao.infoComplementares.Remuneracao.VrSalFx));

  Check(eS2306.infoTSVAlteracao.infoComplementares.Remuneracao.UndSalFixo = sfPorHora,
       'infoTSVAlteracao.infoComplementares.Remuneracao.UndSalFixo | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSUndSalFixoToStr(eS2306.infoTSVAlteracao.infoComplementares.Remuneracao.UndSalFixo));

  Check(eS2306.infoTSVAlteracao.infoComplementares.Remuneracao.DscSalVar = 'descricao',
       'infoTSVAlteracao.infoComplementares.Remuneracao.DscSalVar | ' +
       'Valor esperado:descricao | ' +
       'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.Remuneracao.DscSalVar);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoDirigenteSindical.tpRegPrev = rpfRGPS,
     'infoTSVAlteracao.infoComplementares.infoDirigenteSindical.tpRegPrev | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegPrevFacultativoToStr(eS2306.infoTSVAlteracao.infoComplementares.infoDirigenteSindical.tpRegPrev));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoTrabCedido.tpRegPrev = rpfRGPS,
     'infoTSVAlteracao.infoComplementares.infoTrabCedido.tpRegPrev | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegPrevFacultativoToStr(eS2306.infoTSVAlteracao.infoComplementares.infoTrabCedido.tpRegPrev));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoMandElet.indRemunCargo = snfSim,
     'infoTSVAlteracao.infoComplementares.infoMandElet.indRemunCargo | ' +
     'Valor esperado:S | ' +
     'Valor recebido:'+eSSimNaoFacultativoToStr(eS2306.infoTSVAlteracao.infoComplementares.infoMandElet.indRemunCargo));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoMandElet.tpRegPrev = rpRGPS,
     'infoTSVAlteracao.infoComplementares.infoMandElet.tpRegPrev | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpRegPrevToStr(eS2306.infoTSVAlteracao.infoComplementares.infoMandElet.tpRegPrev));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.natEstagio = neObrigatiorio,
     'infoTSVAlteracao.infoComplementares.infoEstagiario.natEstagio | ' +
     'Valor esperado:O | ' +
     'Valor recebido:'+eSTpNatEstagioToStr(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.natEstagio));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.nivEstagio = nvFundamental,
     'infoTSVAlteracao.infoComplementares.infoEstagiario.nivEstagio | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eSTpNivelEstagioToStr(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.nivEstagio));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.areaAtuacao = 'atuacao',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.areaAtuacao | ' +
     'Valor esperado:atuacao | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.areaAtuacao);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.nrApol = '9999',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.nrApol | ' +
     'Valor esperado:9999 | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.nrApol);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.dtPrevTerm = StrToDateTime('01/01/2018'),
     'infoTSVAlteracao.infoComplementares.infoEstagiario.dtPrevTerm | ' +
     'Valor esperado:01/01/2018 | ' +
     'Valor recebido:'+DateTimeToStr(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.dtPrevTerm));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino = '12345678000123',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino | ' +
     'Valor esperado:12345678000123 | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.cnpjInstEnsino);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nmRazao = 'rz',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nmRazao | ' +
     'Valor esperado:rz | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nmRazao);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.dscLograd = 'desc',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.dscLograd | ' +
     'Valor esperado:desc | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.dscLograd);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nrLograd = '1',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nrLograd | ' +
     'Valor esperado:1 | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.nrLograd);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.bairro = 'centro',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.bairro | ' +
     'Valor esperado:centro | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.bairro);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Cep = '99999999',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Cep | ' +
     'Valor esperado:99999999 | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Cep);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.codMunic = 99999999,
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.codMunic | ' +
     'Valor esperado:99999999 | ' +
     'Valor recebido:'+IntToStr(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.codMunic));

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Uf = 'SP',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Uf | ' +
     'Valor esperado:SP | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.instEnsino.Uf);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg = '12345678000123',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg | ' +
     'Valor esperado:12345678000123 | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.ageIntegracao.cnpjAgntInteg);

  Check(eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor = '99999999999',
     'infoTSVAlteracao.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor | ' +
     'Valor esperado:99999999999 | ' +
     'Valor recebido:'+eS2306.infoTSVAlteracao.infoComplementares.infoEstagiario.supervisorEstagio.cpfSupervisor);

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2399Tests;
var
  eS2399: TEvtTSVTermino;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2399.Count > 0, 'Não instanciou o S-2399 na lista');

  eS2399 := FACBreSocial.Eventos.NaoPeriodicos.S2399[0].EvtTSVTermino;

  Check(eS2399.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2399.Sequencial));

  Check(eS2399.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2399.IdeEvento.indRetif));

  Check(eS2399.IdeEvento.nrRecibo = '123456',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2399.IdeEvento.nrRecibo);

  Check(eS2399.IdeEvento.indGuia = '1',
       'IdeEvento.indGuia | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eS2399.IdeEvento.indGuia);

  Check(eS2399.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2399.IdeEvento.procEmi));

  Check(eS2399.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2399.IdeEvento.verProc);

  Check(eS2399.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2399.IdeEmpregador.tpInsc));

  Check(eS2399.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2399.IdeEmpregador.nrInsc);

  Check(eS2399.IdeTrabSemVinc.cpfTrab = '99999999999',
       'IdeTrabSemVinc.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2399.IdeTrabSemVinc.cpfTrab);

  Check(eS2399.IdeTrabSemVinc.matricula = '9999999',
       'IdeTrabSemVinc.matricula | ' +
       'Valor esperado:9999999 | ' +
       'Valor recebido:'+eS2399.IdeTrabSemVinc.matricula);

  Check(eS2399.IdeTrabSemVinc.codCateg = 201,
       'IdeTrabSemVinc.codCateg | ' +
       'Valor esperado:201 | ' +
       'Valor recebido:'+IntToStr(eS2399.IdeTrabSemVinc.codCateg));

  Check(eS2399.InfoTSVTermino.dtTerm = StrToDateTime('01/01/2018'),
       'InfoTSVTermino.dtTerm | ' +
       'Valor esperado:01/01/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2399.InfoTSVTermino.dtTerm));

  Check(eS2399.InfoTSVTermino.mtvDesligTSV = '01',
       'InfoTSVTermino.mtvDesligTSV | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.mtvDesligTSV);

  Check(eS2399.InfoTSVTermino.pensAlim = paPercentualDePensaoAlimenticia,
       'InfoTSVTermino.pensAlim | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpPensaoAlimToStr(eS2399.InfoTSVTermino.pensAlim));

  Check(eS2399.InfoTSVTermino.percAliment = 1,
       'InfoTSVTermino.percAliment | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.percAliment));

  Check(eS2399.InfoTSVTermino.vrAlim = 1,
       'InfoTSVTermino.vrAlim | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.vrAlim));

  Check(eS2399.InfoTSVTermino.nrProcTrab = '01234567890123456789',
       'InfoTSVTermino.nrProcTrab | ' +
       'Valor esperado:01234567890123456789 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.nrProcTrab);

  Check(eS2399.InfoTSVTermino.mudancaCPF.novoCPF = '99999999999',
       'InfoTSVTermino.mudancaCPF.novoCPF | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.mudancaCPF.novoCPF);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideDmDev = '00001',
       'InfoTSVTermino.verbasResc.dmDev.ideDmDev | ' +
       'Valor esperado:00001 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideDmDev);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].indRRA = snfSim,
       'InfoTSVTermino.verbasResc.dmDev.indRRA | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].indRRA));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.tpProcRRA = tppAdministrativo,
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.tpProcRRA | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpProcRRAToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.tpProcRRA));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.nrProcRRA = '12345678901234567890',
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.nrProcRRA | ' +
       'Valor esperado:12345678901234567890 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.nrProcRRA);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.descRRA = 'descricao',
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.descRRA | ' +
       'Valor esperado:descricao | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.descRRA);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.qtdMesesRRA = 3,
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.qtdMesesRRA | ' +
       'Valor esperado:3 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.qtdMesesRRA));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespCustas = 1,
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.despProcJud.vlrDespCustas | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespCustas));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespAdvogados = 1,
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.despProcJud.vlrDespAdvogados | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.despProcJud.vlrDespAdvogados));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].tpInsc = tiCNPJ,
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.ideAdv.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].tpInsc));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].nrInsc = '12345678901234',
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.ideAdv.nrInsc | ' +
       'Valor esperado:12345678901234 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].nrInsc);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].vlrAdv = 1,
       'InfoTSVTermino.verbasResc.dmDev.infoRRA.ideAdv.vlrAdv | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].infoRRA.ideAdv.Items[0].vlrAdv));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].tpInsc = tiCNPJ,
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].tpInsc));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].nrInsc = '12345678000123',
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].nrInsc);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].codLotacao = '001',
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.codLotacao | ' +
       'Valor esperado:001 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].codLotacao);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].codRubr = '0001',
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.detVerbas.codRubr | ' +
       'Valor esperado:0001 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].codRubr);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].ideTabRubr = '00000001',
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.detVerbas.ideTabRubr | ' +
       'Valor esperado:00000001 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].ideTabRubr);

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].qtdRubr = 1,
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.detVerbas.qtdRubr | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].qtdRubr));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].fatorRubr = 50,
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.detVerbas.fatorRubr | ' +
       'Valor esperado:50 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].fatorRubr));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].vrRubr = 100,
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.detVerbas.vrRubr | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].vrRubr));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].indApurIR = tiaiNormal,
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.detVerbas.indApurIR | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eSTpIndApurIRToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].detVerbas.Items[0].indApurIR));

  Check(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].infoSimples.indSimples = idsIntegralmente,
       'InfoTSVTermino.verbasResc.dmDev.ideEstabLot.infoSimples.indSimples | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndSimplesToStr(eS2399.InfoTSVTermino.verbasResc.dmDev.Items[0].ideEstabLot.Items[0].infoSimples.indSimples));

  Check(eS2399.InfoTSVTermino.verbasResc.procJudTrab.Items[0].tpTrib = tptIRRF,
       'InfoTSVTermino.verbasResc.procJudTrab.tpTrib | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpTributoToStr(eS2399.InfoTSVTermino.verbasResc.procJudTrab.Items[0].tpTrib));

  Check(eS2399.InfoTSVTermino.verbasResc.procJudTrab.Items[0].nrProcJud = '99999999999999999999',
       'InfoTSVTermino.verbasResc.procJudTrab.nrProcJud | ' +
       'Valor esperado:99999999999999999999 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.procJudTrab.Items[0].nrProcJud);

  Check(eS2399.InfoTSVTermino.verbasResc.procJudTrab.Items[0].codSusp = '999',
       'InfoTSVTermino.verbasResc.procJudTrab.codSusp | ' +
       'Valor esperado:999 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.procJudTrab.Items[0].codSusp);

  Check(eS2399.InfoTSVTermino.verbasResc.infoMV.indMV = imvDescontadaempregador,
       'InfoTSVTermino.verbasResc.infoMV.indMV | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndMVToStr(eS2399.InfoTSVTermino.verbasResc.infoMV.indMV));

  Check(eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].tpInsc = tiCNPJ,
       'InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].tpInsc));

  Check(eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].nrInsc = '12345678000123',
       'InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].nrInsc);

  Check(eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].codCateg = 111,
       'InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.codCateg | ' +
       'Valor esperado:111 | ' +
       'Valor recebido:'+IntToStr(eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].codCateg));

  Check(eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].vlrRemunOE = 100,
       'InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.vlrRemunOE | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2399.InfoTSVTermino.verbasResc.infoMV.remunOutrEmpr.Items[0].vlrRemunOE));

  Check(eS2399.InfoTSVTermino.RemunAposTerm.indRemun = ireQuarentena,
       'InfoTSVTermino.RemunAposTerm.indRemun | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+TpIndRemunToStr(eS2399.InfoTSVTermino.RemunAposTerm.indRemun));

  Check(eS2399.InfoTSVTermino.RemunAposTerm.dtFimRemun = StrToDateTime('01/01/2023'),
       'InfoTSVTermino.RemunAposTerm.dtFimRemun | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2399.InfoTSVTermino.RemunAposTerm.dtFimRemun));

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2416Tests;
var
  eS2416: TEvtCdBenAlt;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2416.Count > 0, 'Não instanciou o S-2416 na lista');

  eS2416 := FACBreSocial.Eventos.NaoPeriodicos.S2416[0].EvtCdBenAlt;

  Check(eS2416.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2416.Sequencial));

  Check(eS2416.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2416.IdeEvento.indRetif));

  Check(eS2416.IdeEvento.nrRecibo = '123456',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2416.IdeEvento.nrRecibo);

  Check(eS2416.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2416.IdeEvento.procEmi));

  Check(eS2416.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2416.IdeEvento.verProc);

  Check(eS2416.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2416.IdeEmpregador.tpInsc));

  Check(eS2416.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2416.IdeEmpregador.nrInsc);

  Check(eS2416.ideBeneficio.cpfBenef = '99999999999',
       'ideBeneficio.cpfBenef | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2416.ideBeneficio.cpfBenef);

  Check(eS2416.ideBeneficio.nrBeneficio = '123',
       'ideBeneficio.nrBeneficio | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2416.ideBeneficio.nrBeneficio);

  Check(eS2416.InfoBenAlteracao.dtAltBeneficio = StrToDateTime('01/01/2023'),
       'InfoBenAlteracao.dtAltBeneficio | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2416.InfoBenAlteracao.dtAltBeneficio));

  Check(eS2416.InfoBenAlteracao.dadosBeneficio.tpBeneficio = 0101,
       'InfoBenAlteracao.dadosBeneficio.tpBeneficio | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+IntToStr(eS2416.InfoBenAlteracao.dadosBeneficio.tpBeneficio));

  Check(eS2416.InfoBenAlteracao.dadosBeneficio.tpPlanRP = prpSemSegregacaoDaMassa,
       'InfoBenAlteracao.dadosBeneficio.tpPlanRP | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eSTpPlanRPToStr(eS2416.InfoBenAlteracao.dadosBeneficio.tpPlanRP));

  Check(eS2416.InfoBenAlteracao.dadosBeneficio.dsc = 'descricao',
       'InfoBenAlteracao.dadosBeneficio.dsc | ' +
       'Valor esperado:descricao | ' +
       'Valor recebido:'+eS2416.InfoBenAlteracao.dadosBeneficio.dsc);

  Check(eS2416.InfoBenAlteracao.dadosBeneficio.indSuspensao = tpSim,
       'InfoBenAlteracao.dadosBeneficio.indSuspensao | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2416.InfoBenAlteracao.dadosBeneficio.indSuspensao));

  Check(eS2416.InfoBenAlteracao.dadosBeneficio.infoPenMorte.tpPenMorte = pmVitalicia,
       'InfoBenAlteracao.dadosBeneficio.infoPenMorte.tpPenMorte | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eStpTpPenMorteToStrEX(eS2416.InfoBenAlteracao.dadosBeneficio.infoPenMorte.tpPenMorte));

  Check(eS2416.InfoBenAlteracao.dadosBeneficio.suspensao.mtvSuspensao = mtvSuspensaoPorNaoRecadastramento,
       'InfoBenAlteracao.dadosBeneficio.suspensao.mtvSuspensao | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eStpTpMtvSuspensaoToStr(eS2416.InfoBenAlteracao.dadosBeneficio.suspensao.mtvSuspensao));

  Check(eS2416.InfoBenAlteracao.dadosBeneficio.suspensao.dscSuspensao = 'descricao',
       'InfoBenAlteracao.dadosBeneficio.suspensao.dscSuspensao | ' +
       'Valor esperado:descricao | ' +
       'Valor recebido:'+eS2416.InfoBenAlteracao.dadosBeneficio.suspensao.dscSuspensao);

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2418Tests;
var
  eS2418: TEvtReativBen;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2418.Count > 0, 'Não instanciou o S-2418 na lista');

  eS2418 := FACBreSocial.Eventos.NaoPeriodicos.S2418[0].EvtReativBen;

  Check(eS2418.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2418.Sequencial));

  Check(eS2418.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2418.IdeEvento.indRetif));

  Check(eS2418.IdeEvento.nrRecibo = '123456',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2418.IdeEvento.nrRecibo);

  Check(eS2418.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2418.IdeEvento.procEmi));

  Check(eS2418.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2418.IdeEvento.verProc);

  Check(eS2418.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2418.IdeEmpregador.tpInsc));

  Check(eS2418.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2418.IdeEmpregador.nrInsc);

  Check(eS2418.ideBeneficio.cpfBenef = '99999999999',
       'ideBeneficio.cpfBenef | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2418.ideBeneficio.cpfBenef);

  Check(eS2418.ideBeneficio.nrBeneficio = '123',
       'ideBeneficio.nrBeneficio | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2418.ideBeneficio.nrBeneficio);

  Check(eS2418.infoReativ.dtEfetReativ = StrToDateTime('01/01/2023'),
       'infoReativ.dtEfetReativ | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2418.infoReativ.dtEfetReativ));

  Check(eS2418.infoReativ.dtEfeito = StrToDateTime('01/02/2023'),
       'infoReativ.dtEfeito | ' +
       'Valor esperado:01/02/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2418.infoReativ.dtEfeito));

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2420Tests;
var
  eS2420: TEvtCdBenTerm;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2420.Count > 0, 'Não instanciou o S-2420 na lista');

  eS2420 := FACBreSocial.Eventos.NaoPeriodicos.S2420[0].EvtCdBenTerm;

  Check(eS2420.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2420.Sequencial));

  Check(eS2420.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2420.IdeEvento.indRetif));

  Check(eS2420.IdeEvento.nrRecibo = '123456',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2420.IdeEvento.nrRecibo);

  Check(eS2420.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2420.IdeEvento.procEmi));

  Check(eS2420.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2420.IdeEvento.verProc);

  Check(eS2420.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2420.IdeEmpregador.tpInsc));

  Check(eS2420.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2420.IdeEmpregador.nrInsc);

  Check(eS2420.ideBeneficio.cpfBenef = '99999999999',
       'ideBeneficio.cpfBenef | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2420.ideBeneficio.cpfBenef);

  Check(eS2420.ideBeneficio.nrBeneficio = '123',
       'ideBeneficio.nrBeneficio | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2420.ideBeneficio.nrBeneficio);

  Check(eS2420.infoBenTermino.dtTermBeneficio = StrToDateTime('01/01/2023'),
       'infoBenTermino.dtTermBeneficio | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2420.infoBenTermino.dtTermBeneficio));

  Check(eS2420.infoBenTermino.mtvTermino = tmcbObito,
       'infoBenTermino.mtvTermino | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eStpTpMotCessBenefToStrEX(eS2420.infoBenTermino.mtvTermino));

  Check(eS2420.infoBenTermino.cnpjOrgaoSuc = '12345678000123',
       'infoBenTermino.cnpjOrgaoSuc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2420.infoBenTermino.cnpjOrgaoSuc);

  Check(eS2420.infoBenTermino.novoCPF = '99999999999',
       'infoBenTermino.novoCPF | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2420.infoBenTermino.novoCPF);
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2500Tests;
var
  eS2500: TEvtProcTrab;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2500.Count > 0, 'Não instanciou o S-2500 na lista');

  eS2500 := FACBreSocial.Eventos.NaoPeriodicos.S2500[0].EvtProcTrab;

  Check(eS2500.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2500.Sequencial));

  Check(eS2500.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2500.IdeEvento.indRetif));

  Check(eS2500.IdeEvento.nrRecibo = '123456',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2500.IdeEvento.nrRecibo);

  Check(eS2500.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2500.IdeEvento.procEmi));

  Check(eS2500.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2500.IdeEvento.verProc);

  Check(eS2500.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2500.IdeEmpregador.tpInsc));

  Check(eS2500.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2500.IdeEmpregador.nrInsc);

  Check(eS2500.ideEmpregador.ideResp.tpInsc = tiCNPJ,
       'ideEmpregador.ideResp.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2500.ideEmpregador.ideResp.tpInsc));

  Check(eS2500.IdeEmpregador.ideResp.nrInsc = '12345678000123',
       'IdeEmpregador.ideResp.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2500.IdeEmpregador.ideResp.nrInsc);

  Check(eS2500.infoProcesso.origem = oprProcessoJudicial,
       'infoProcesso.origem | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpTpOrigemProcToStr(eS2500.infoProcesso.origem));

  Check(eS2500.infoProcesso.nrProcTrab = '01234567890123456789',
       'infoProcesso.nrProcTrab | ' +
       'Valor esperado:01234567890123456789 | ' +
       'Valor recebido:'+eS2500.infoProcesso.nrProcTrab);

  Check(eS2500.infoProcesso.obsProcTrab = 'observacao',
       'infoProcesso.obsProcTrab | ' +
       'Valor esperado:observacao | ' +
       'Valor recebido:'+eS2500.infoProcesso.obsProcTrab);

  Check(eS2500.infoProcesso.dadosCompl.infoProcJud.dtSent = StrToDateTime('01/01/2023'),
       'infoProcesso.dadosCompl.infoProcJud.dtSent | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.infoProcesso.dadosCompl.infoProcJud.dtSent));

  Check(eS2500.infoProcesso.dadosCompl.infoProcJud.ufVara = 'SP',
       'infoProcesso.dadosCompl.infoProcJud.ufVara | ' +
       'Valor esperado:SP | ' +
       'Valor recebido:'+eS2500.infoProcesso.dadosCompl.infoProcJud.ufVara);

  Check(eS2500.infoProcesso.dadosCompl.infoProcJud.codMunic = 3554003,
       'infoProcesso.dadosCompl.infoProcJud.codMunic | ' +
       'Valor esperado:3554003 | ' +
       'Valor recebido:'+IntToStr(eS2500.infoProcesso.dadosCompl.infoProcJud.codMunic));

  Check(eS2500.infoProcesso.dadosCompl.infoProcJud.idVara = 123,
       'infoProcesso.dadosCompl.infoProcJud.idVara | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+IntToStr(eS2500.infoProcesso.dadosCompl.infoProcJud.idVara));

  Check(eS2500.infoProcesso.dadosCompl.infoCCP.dtCCP = StrToDateTime('01/01/2023'),
       'infoProcesso.dadosCompl.infoCCP.dtCCP | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.infoProcesso.dadosCompl.infoCCP.dtCCP));

  Check(eS2500.infoProcesso.dadosCompl.infoCCP.tpCCP = CCPAmbitoEmpresa,
       'infoProcesso.dadosCompl.infoCCP.tpCCP | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpTpCCPToStr(eS2500.infoProcesso.dadosCompl.infoCCP.tpCCP));

  Check(eS2500.infoProcesso.dadosCompl.infoCCP.cnpjCCP = '12345678000123',
       'infoProcesso.dadosCompl.infoCCP.cnpjCCP | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2500.infoProcesso.dadosCompl.infoCCP.cnpjCCP);

  Check(eS2500.ideTrab.cpfTrab = '99999999999',
       'ideTrab.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2500.ideTrab.cpfTrab);

  Check(eS2500.ideTrab.nmTrab = 'nome',
       'ideTrab.nmTrab | ' +
       'Valor esperado:nome | ' +
       'Valor recebido:'+eS2500.ideTrab.nmTrab);

  Check(eS2500.ideTrab.dtNascto = StrToDateTime('01/01/1977'),
       'ideTrab.dtNascto | ' +
       'Valor esperado:01/01/1977 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.dtNascto));

  Check(eS2500.ideTrab.dependente.Items[0].cpfDep = '99999999999',
       'ideTrab.dependente.cpfDep | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2500.ideTrab.dependente.Items[0].cpfDep);

  Check(eS2500.ideTrab.dependente.Items[0].tpDep = tdAgregadoOutros,
       'ideTrab.dependente.tpDep | ' +
       'Valor esperado:99 | ' +
       'Valor recebido:'+eStpDepToStr(eS2500.ideTrab.dependente.Items[0].tpDep));

  Check(eS2500.ideTrab.dependente.Items[0].descDep = 'descricao',
       'ideTrab.dependente.descDep | ' +
       'Valor esperado:descricao | ' +
       'Valor recebido:'+eS2500.ideTrab.dependente.Items[0].descDep);

  Check(eS2500.ideTrab.infoContr.Items[0].tpContr = TrabalhadorComVinculoFormalizadoSemAlteracaoNasDatasDeAdmissaoEDeDesligamento,
       'ideTrab.infoContr.tpContr | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpContrS2500ToStr(eS2500.ideTrab.infoContr.Items[0].tpContr));

  Check(eS2500.ideTrab.infoContr.Items[0].indContr = tpSim,
       'ideTrab.infoContr.indContr | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2500.ideTrab.infoContr.Items[0].indContr));

  Check(eS2500.ideTrab.infoContr.Items[0].dtAdmOrig = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.dtAdmOrig | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].dtAdmOrig));

  Check(eS2500.ideTrab.infoContr.Items[0].indReint = snfSim,
       'ideTrab.infoContr.indReint | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2500.ideTrab.infoContr.Items[0].indReint));

  Check(eS2500.ideTrab.infoContr.Items[0].indCateg = tpSim,
       'ideTrab.infoContr.indCateg | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2500.ideTrab.infoContr.Items[0].indCateg));

  Check(eS2500.ideTrab.infoContr.Items[0].indNatAtiv = tpSim,
       'ideTrab.infoContr.indNatAtiv | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2500.ideTrab.infoContr.Items[0].indNatAtiv));

  Check(eS2500.ideTrab.infoContr.Items[0].indMotDeslig = tpSim,
       'ideTrab.infoContr.indMotDeslig | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2500.ideTrab.infoContr.Items[0].indMotDeslig));

  Check(eS2500.ideTrab.infoContr.Items[0].indUnic = snfSim,
       'ideTrab.infoContr.indUnic | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2500.ideTrab.infoContr.Items[0].indUnic));

  Check(eS2500.ideTrab.infoContr.Items[0].matricula = '123456',
       'ideTrab.infoContr.matricula | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].matricula);

  Check(eS2500.ideTrab.infoContr.Items[0].codCateg = 101,
       'ideTrab.infoContr.codCateg | ' +
       'Valor esperado:101 | ' +
       'Valor recebido:'+IntToStr(eS2500.ideTrab.infoContr.Items[0].codCateg));

  Check(eS2500.ideTrab.infoContr.Items[0].dtInicio = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.dtInicio | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].dtInicio));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.codCBO = '123456',
       'ideTrab.infoContr.infoCompl.codCBO | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.codCBO);

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.natAtividade = navUrbano,
       'ideTrab.infoContr.infoCompl.natAtividade | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSNatAtividadeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.natAtividade));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].dtRemun = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.infoCompl.remuneracao.dtRemun | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].dtRemun));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].VrSalFx = 100,
       'ideTrab.infoContr.infoCompl.remuneracao.VrSalFx | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].VrSalFx));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].UndSalFixo = sfPorTarefa,
       'ideTrab.infoContr.infoCompl.remuneracao.UndSalFixo | ' +
       'Valor esperado:6 | ' +
       'Valor recebido:'+eSUndSalFixoToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].UndSalFixo));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].DscSalVar = 'descricao',
       'ideTrab.infoContr.infoCompl.remuneracao.DscSalVar | ' +
       'Valor esperado:descricao | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.remuneracao.Items[0].DscSalVar);

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.tpRegTrab = trCLT,
       'ideTrab.infoContr.infoCompl.infoVinc.tpRegTrab | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpRegTrabToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.tpRegTrab));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.tpRegPrev = rpRGPS,
       'ideTrab.infoContr.infoCompl.infoVinc.tpRegPrev | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpRegPrevToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.tpRegPrev));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.dtAdm = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.infoCompl.infoVinc.dtAdm | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.dtAdm));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.tmpParc = tpNaoeTempoParcial,
       'ideTrab.infoContr.infoCompl.infoVinc.tmpParc | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+tpTmpParcToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.tmpParc));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.TpContr = PrazoDeterminadoVincOcDeUmFato,
       'ideTrab.infoContr.infoCompl.infoVinc.duracao.TpContr | ' +
       'Valor esperado:3 | ' +
       'Valor recebido:'+eSTpContrToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.TpContr));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.dtTerm = StrToDateTime('01/02/2023'),
       'ideTrab.infoContr.infoCompl.infoVinc.duracao.dtTerm | ' +
       'Valor esperado:01/02/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.dtTerm));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.clauAssec = tpSim,
       'ideTrab.infoContr.infoCompl.infoVinc.duracao.clauAssec | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.clauAssec));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.objDet = 'objeto',
       'ideTrab.infoContr.infoCompl.infoVinc.duracao.objDet | ' +
       'Valor esperado:objeto | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.duracao.objDet);

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.observacoes.Items[0].observacao = 'observacao',
       'ideTrab.infoContr.infoCompl.infoVinc.observacoes.observacao | ' +
       'Valor esperado:observacao | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.observacoes.Items[0].observacao);

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.tpInsc = tiCNPJ,
       'ideTrab.infoContr.infoCompl.infoVinc.sucessaoVinc.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.tpInsc));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.nrInsc = '12345678000123',
       'ideTrab.infoContr.infoCompl.infoVinc.sucessaoVinc.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.nrInsc);

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.matricAnt = '123456',
       'ideTrab.infoContr.infoCompl.infoVinc.sucessaoVinc.matricAnt | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.matricAnt);

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.dtTransf = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.infoCompl.infoVinc.sucessaoVinc.dtTransf | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.sucessaoVinc.dtTransf));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.infoDeslig.dtDeslig = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.infoCompl.infoVinc.infoDeslig.dtDeslig | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.infoDeslig.dtDeslig));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.infoDeslig.mtvDeslig = '01',
       'ideTrab.infoContr.infoCompl.infoVinc.infoDeslig.mtvDeslig | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.infoDeslig.mtvDeslig);

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.infoDeslig.dtProjFimAPI = StrToDateTime('01/02/2023'),
       'ideTrab.infoContr.infoCompl.infoVinc.infoDeslig.dtProjFimAPI | ' +
       'Valor esperado:01/02/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoVinc.infoDeslig.dtProjFimAPI));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoTerm.dtTerm = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.infoCompl.infoTerm.dtTerm | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoTerm.dtTerm));

  Check(eS2500.ideTrab.infoContr.Items[0].infoCompl.infoTerm.mtvDesligTSV = '01',
       'ideTrab.infoContr.infoCompl.infoTerm.mtvDesligTSV | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].infoCompl.infoTerm.mtvDesligTSV);

  Check(eS2500.ideTrab.infoContr.Items[0].mudCategAtiv.Items[0].codCateg = 101,
       'ideTrab.infoContr.mudCategAtiv.codCateg | ' +
       'Valor esperado:101 | ' +
       'Valor recebido:'+IntToStr(eS2500.ideTrab.infoContr.Items[0].mudCategAtiv.Items[0].codCateg));

  Check(eS2500.ideTrab.infoContr.Items[0].mudCategAtiv.Items[0].natAtividade = navUrbano,
       'ideTrab.infoContr.mudCategAtiv.natAtividade | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSNatAtividadeToStr(eS2500.ideTrab.infoContr.Items[0].mudCategAtiv.Items[0].natAtividade));

  Check(eS2500.ideTrab.infoContr.Items[0].mudCategAtiv.Items[0].dtMudCategAtiv = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.mudCategAtiv.dtMudCategAtiv | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].mudCategAtiv.Items[0].dtMudCategAtiv));

  Check(eS2500.ideTrab.infoContr.Items[0].unicContr.Items[0].matUnic = '123456',
       'ideTrab.infoContr.unicContr.matUnic | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].unicContr.Items[0].matUnic);

  Check(eS2500.ideTrab.infoContr.Items[0].unicContr.Items[0].codCateg = 101,
       'ideTrab.infoContr.unicContr.codCateg | ' +
       'Valor esperado:101 | ' +
       'Valor recebido:'+IntToStr(eS2500.ideTrab.infoContr.Items[0].unicContr.Items[0].codCateg));

  Check(eS2500.ideTrab.infoContr.Items[0].unicContr.Items[0].dtInicio = StrToDateTime('01/01/2023'),
       'ideTrab.infoContr.unicContr.dtInicio | ' +
       'Valor esperado:01/01/2023 | ' +
       'Valor recebido:'+DateTimeToStr(eS2500.ideTrab.infoContr.Items[0].unicContr.Items[0].dtInicio));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.tpInsc = tiCNPJ,
       'ideTrab.infoContr.ideEstab.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.tpInsc));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.nrInsc = '12345678000123',
       'ideTrab.infoContr.ideEstab.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].ideEstab.nrInsc);

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.compIni = '2023-01',
       'ideTrab.infoContr.ideEstab.infoVlr.compIni | ' +
       'Valor esperado:2023-01 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.compIni);

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.compFim = '2023-02',
       'ideTrab.infoContr.ideEstab.infoVlr.compFim | ' +
       'Valor esperado:2023-02 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.compFim);

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.repercProc = repDecisaoComPagamentoDeVerbasDeNaturezaRemuneratoria,
       'ideTrab.infoContr.ideEstab.infoVlr.repercProc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpTpRepercProcToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.repercProc));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrRemun = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.vrRemun | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrRemun));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrAPI = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.vrAPI | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrAPI));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vr13API = 113,
       'ideTrab.infoContr.ideEstab.infoVlr.vr13API | ' +
       'Valor esperado:113 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vr13API));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrInden = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.vrInden | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrInden));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrBaseIndenFGTS = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.vrBaseIndenFGTS | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.vrBaseIndenFGTS));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.pagDiretoResc = snfSim,
       'ideTrab.infoContr.ideEstab.infoVlr.pagDiretoResc | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.pagDiretoResc));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].perRef = '2023-01',
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.perRef | ' +
       'Valor esperado:2023-01 | ' +
       'Valor recebido:'+eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].perRef);

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcCpMensal = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.baseCalculo.vrBcCpMensal | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcCpMensal));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcCp13 = 113,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.baseCalculo.vrBcCp13 | ' +
       'Valor esperado:113 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcCp13));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcFgts = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.baseCalculo.vrBcFgts | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcFgts));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcFgts13 = 113,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.baseCalculo.vrBcFgts13 | ' +
       'Valor esperado:113 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.vrBcFgts13));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.infoAgNocivo.grauExp = ge1,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.baseCalculo.infoAgNocivo.grauExp | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSGrauExpToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseCalculo.infoAgNocivo.grauExp));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].infoFGTS.vrBcFgtsGuia = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.infoFGTS.vrBcFgtsGuia | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].infoFGTS.vrBcFgtsGuia));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].infoFGTS.vrBcFgts13Guia = 113,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.infoFGTS.vrBcFgts13Guia | ' +
       'Valor esperado:113 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].infoFGTS.vrBcFgts13Guia));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].infoFGTS.pagDireto = tpSim,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.infoFGTS.pagDireto | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].infoFGTS.pagDireto));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseMudCateg.codCateg = 101,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.baseMudCateg.codCateg | ' +
       'Valor esperado:101 | ' +
       'Valor recebido:'+IntToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseMudCateg.codCateg));

  Check(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseMudCateg.vrBcCPrev = 100,
       'ideTrab.infoContr.ideEstab.infoVlr.idePeriodo.baseMudCateg.vrBcCPrev | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2500.ideTrab.infoContr.Items[0].ideEstab.infoVlr.idePeriodo.Items[0].baseMudCateg.vrBcCPrev));

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2501Tests;
var
  eS2501: TEvtContProc;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2501.Count > 0, 'Não instanciou o S-2501 na lista');

  eS2501 := FACBreSocial.Eventos.NaoPeriodicos.S2501[0].EvtContProc;

  Check(eS2501.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2501.Sequencial));

  Check(eS2501.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2501.IdeEvento.indRetif));

  Check(eS2501.IdeEvento.nrRecibo = '123456',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2501.IdeEvento.nrRecibo);

  Check(eS2501.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2501.IdeEvento.procEmi));

  Check(eS2501.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS2501.IdeEvento.verProc);

  Check(eS2501.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2501.IdeEmpregador.tpInsc));

  Check(eS2501.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2501.IdeEmpregador.nrInsc);

  Check(eS2501.ideProc.nrProcTrab = '123456789012345',
       'ideProc.nrProcTrab | ' +
       'Valor esperado:123456789012345 | ' +
       'Valor recebido:'+eS2501.ideProc.nrProcTrab);

  Check(eS2501.ideProc.perApurPgto = '2023-01',
       'ideProc.perApurPgto | ' +
       'Valor esperado:2023-01 | ' +
       'Valor recebido:'+eS2501.ideProc.perApurPgto);

  Check(eS2501.ideProc.obs = 'observacao',
       'ideProc.obs | ' +
       'Valor esperado:observacao | ' +
       'Valor recebido:'+eS2501.ideProc.obs);

  Check(eS2501.ideTrab.Items[0].cpfTrab = '99999999999',
       'ideTrab.Items[0].cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS2501.ideTrab.Items[0].cpfTrab);

  Check(eS2501.ideTrab.Items[0].calcTrib.Items[0].perRef = '2023-01',
       'ideTrab.Items[0].calcTrib.perRef | ' +
       'Valor esperado:2023-01 | ' +
       'Valor recebido:'+eS2501.ideTrab.Items[0].calcTrib.Items[0].perRef);

  Check(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrBcCpMensal = 100,
       'ideTrab.Items[0].calcTrib.vrBcCpMensal | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrBcCpMensal));

  Check(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrBcCp13 = 113,
       'ideTrab.Items[0].calcTrib.vrBcCp13 | ' +
       'Valor esperado:113 | ' +
       'Valor recebido:'+FloatToStr(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrBcCp13));

  Check(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrRendIRRF = 100,
       'ideTrab.Items[0].calcTrib.vrRendIRRF | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrRendIRRF));

  Check(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrRendIRRF13 = 113,
       'ideTrab.Items[0].calcTrib.vrRendIRRF13 | ' +
       'Valor esperado:113 | ' +
       'Valor recebido:'+FloatToStr(eS2501.ideTrab.Items[0].calcTrib.Items[0].vrRendIRRF13));

  Check(eS2501.ideTrab.Items[0].calcTrib.Items[0].infoCRContrib.Items[0].tpCR = '113851',
       'ideTrab.Items[0].calcTrib.infoCRContrib.tpCR | ' +
       'Valor esperado:113851 | ' +
       'Valor recebido:'+eS2501.ideTrab.Items[0].calcTrib.Items[0].infoCRContrib.Items[0].tpCR);

  Check(eS2501.ideTrab.Items[0].calcTrib.Items[0].infoCRContrib.Items[0].vrCR = 100,
       'ideTrab.Items[0].calcTrib.infoCRContrib.vrCR | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2501.ideTrab.Items[0].calcTrib.Items[0].infoCRContrib.Items[0].vrCR));

  Check(eS2501.ideTrab.Items[0].infoCRIRRF.Items[0].tpCR = '593656',
       'ideTrab.Items[0].infoCRIRRF.tpCR | ' +
       'Valor esperado:593656 | ' +
       'Valor recebido:'+eS2501.ideTrab.Items[0].infoCRIRRF.Items[0].tpCR);

  Check(eS2501.ideTrab.Items[0].infoCRIRRF.Items[0].vrCR = 100,
       'ideTrab.Items[0].infoCRIRRF.vrCR | ' +
       'Valor esperado:100 | ' +
       'Valor recebido:'+FloatToStr(eS2501.ideTrab.Items[0].infoCRIRRF.Items[0].vrCR));

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES3000Tests;
var
  eS3000: TEvtExclusao;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S3000.Count > 0, 'Não instanciou o S-3000 na lista');

  eS3000 := FACBreSocial.Eventos.NaoPeriodicos.S3000[0].EvtExclusao;

  Check(eS3000.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS3000.Sequencial));

  Check(eS3000.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS3000.IdeEvento.procEmi));

  Check(eS3000.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS3000.IdeEvento.verProc);

  Check(eS3000.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS3000.IdeEmpregador.tpInsc));

  Check(eS3000.IdeEmpregador.nrInsc = '012345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:012345678000123 | ' +
       'Valor recebido:'+eS3000.IdeEmpregador.nrInsc);

  Check(eS3000.InfoExclusao.tpEvento = teS1000,
       'InfoExclusao.tpEvento | ' +
       'Valor esperado:1000 | ' +
       'Valor recebido:'+TipoEventoToStr(eS3000.InfoExclusao.tpEvento));

  Check(eS3000.InfoExclusao.nrRecEvt = '123456',
       'InfoExclusao.nrRecEvt | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS3000.InfoExclusao.nrRecEvt);

  Check(eS3000.InfoExclusao.IdeTrabalhador.cpfTrab = '99999999999',
       'InfoExclusao.IdeTrabalhador.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS3000.InfoExclusao.IdeTrabalhador.cpfTrab);

  Check(eS3000.InfoExclusao.IdeFolhaPagto.indApuracao = iapuMensal,
       'InfoExclusao.IdeFolhaPagto.indApuracao | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndApuracaoToStr(eS3000.InfoExclusao.IdeFolhaPagto.indApuracao));

  Check(eS3000.InfoExclusao.IdeFolhaPagto.perApur = '2018-04',
       'InfoExclusao.IdeFolhaPagto.perApur | ' +
       'Valor esperado:2018-04 | ' +
       'Valor recebido:'+eS3000.InfoExclusao.IdeFolhaPagto.perApur);

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES3500Tests;
var
  eS3500: TEvtExcProcTrab;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S3500.Count > 0, 'Não instanciou o S-3500 na lista');

  eS3500 := FACBreSocial.Eventos.NaoPeriodicos.S3500[0].EvtExcProcTrab;

  Check(eS3500.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS3500.Sequencial));

  Check(eS3500.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS3500.IdeEvento.procEmi));

  Check(eS3500.IdeEvento.verProc = '1.1',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.1 | ' +
       'Valor recebido:'+eS3500.IdeEvento.verProc);

  Check(eS3500.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS3500.IdeEmpregador.tpInsc));

  Check(eS3500.IdeEmpregador.nrInsc = '012345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:012345678000123 | ' +
       'Valor recebido:'+eS3500.IdeEmpregador.nrInsc);

  Check(eS3500.InfoExclusao.tpEvento = teS1000,
       'InfoExclusao.tpEvento | ' +
       'Valor esperado:1000 | ' +
       'Valor recebido:'+TipoEventoToStr(eS3500.InfoExclusao.tpEvento));

  Check(eS3500.InfoExclusao.nrRecEvt = '123456',
       'InfoExclusao.nrRecEvt | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS3500.InfoExclusao.nrRecEvt);

  Check(eS3500.InfoExclusao.ideProcTrab.nrProcTrab = '123456789012345',
       'InfoExclusao.ideProcTrab.nrProcTrab | ' +
       'Valor esperado:123456789012345 | ' +
       'Valor recebido:'+eS3500.InfoExclusao.ideProcTrab.nrProcTrab);

  Check(eS3500.InfoExclusao.ideProcTrab.cpfTrab = '99999999999',
       'InfoExclusao.ideProcTrab.cpfTrab | ' +
       'Valor esperado:99999999999 | ' +
       'Valor recebido:'+eS3500.InfoExclusao.ideProcTrab.cpfTrab);

  Check(eS3500.InfoExclusao.ideProcTrab.perApurPgto = '01/2023',
       'InfoExclusao.ideProcTrab.perApurPgto | ' +
       'Valor esperado:01/2023 | ' +
       'Valor recebido:'+eS3500.InfoExclusao.ideProcTrab.perApurPgto);
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
  Check(trabalhador.InfoDeficiencia.infoCota       = snfNao ,
        'trabalhador.InfoDeficiencia.infoCota | Valor esperado:N | Valor recebido:'+eSSimNaoFacultativoToStr(trabalhador.InfoDeficiencia.infoCota));
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

  if(FACBreSocial.Configuracoes.Geral.VersaoDF = veS01_01_00)then
  begin
    Check(eS2200.vinculo.transfDom.cpfSubstituido = '12345678901',
         'vinculo.transfDom.cpfSubstituido | ' +
         'Valor esperado:12345678901 | ' +
         'Valor recebido:'+eS2200.vinculo.transfDom.cpfSubstituido);

    Check(eS2200.vinculo.transfDom.matricAnt = '123',
         'vinculo.transfDom.matricAnt | ' +
         'Valor esperado:123 | ' +
         'Valor recebido:'+eS2200.vinculo.transfDom.matricAnt);

    Check(eS2200.vinculo.transfDom.dtTransf = StrToDate('10/04/2018'),
         'vinculo.transfDom.dtTransf | ' +
         'Valor esperado:10/04/2018 | ' +
         'Valor recebido:'+DateToStr(eS2200.vinculo.transfDom.dtTransf));

    Check(eS2200.vinculo.mudancaCPF.cpfAnt = '12345678901',
         'vinculo.mudancaCPF.cpfAnt | ' +
         'Valor esperado:12345678901 | ' +
         'Valor recebido:'+eS2200.vinculo.mudancaCPF.cpfAnt);

    Check(eS2200.vinculo.mudancaCPF.matricAnt = '123',
         'vinculo.mudancaCPF.matricAnt | ' +
         'Valor esperado:123 | ' +
         'Valor recebido:'+eS2200.vinculo.mudancaCPF.matricAnt);

    Check(eS2200.vinculo.mudancaCPF.dtAltCPF = StrToDate('10/04/2018'),
         'vinculo.mudancaCPF.dtAltCPF | ' +
         'Valor esperado:10/04/2018 | ' +
         'Valor recebido:'+DateToStr(eS2200.vinculo.mudancaCPF.dtAltCPF));

    Check(eS2200.vinculo.mudancaCPF.observacao = 'Observacao',
         'vinculo.mudancaCPF.observacao | ' +
         'Valor esperado:Observacao | ' +
         'Valor recebido:'+eS2200.vinculo.mudancaCPF.observacao);

    Check(eS2200.Vinculo.afastamento.DtIniAfast = StrToDate('10/04/2018'),
         'vinculo.afastamento.DtIniAfast | ' +
         'Valor esperado:10/04/2018 | ' +
         'Valor recebido:'+DateToStr(eS2200.vinculo.afastamento.DtIniAfast));

    Check(eS2200.vinculo.afastamento.codMotAfast = mtvAcidenteDoencaTrabalho,
         'vinculo.afastamento.codMotAfast | ' +
         'Valor esperado:01 | ' +
         'Valor recebido:'+eStpMotivosAfastamentoToStr(eS2200.vinculo.afastamento.codMotAfast));

    Check(eS2200.Vinculo.desligamento.DtDeslig = StrToDate('10/04/2018'),
         'vinculo.desligamento.DtDeslig | ' +
         'Valor esperado:10/04/2018 | ' +
         'Valor recebido:'+DateToStr(eS2200.vinculo.desligamento.DtDeslig));

    Check(eS2200.Vinculo.cessao.dtIniCessao = StrToDate('10/04/2018'),
         'vinculo.cessao.dtIniCessao | ' +
         'Valor esperado:10/04/2018 | ' +
         'Valor recebido:'+DateToStr(eS2200.vinculo.cessao.dtIniCessao));

  end;



end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2205Tests;
var
  eS2205: TEvtAltCadastral;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2205.Count > 0, 'Não instanciou o S-2205 na lista');

  eS2205 := FACBreSocial.Eventos.NaoPeriodicos.S2205[0].EvtAltCadastral;

  Check(eS2205.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2205.Sequencial));

  Check(eS2205.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2205.IdeEvento.indRetif));

  Check(eS2205.ideEvento.NrRecibo = '123', 'ideEvento.nrRecibo | Valor esperado:123 | Valor recebido:'+eS2205.ideEvento.nrRecibo);
  Check(eS2205.ideEvento.ProcEmi = peAplicEmpregador, 'ideEvento.procEmi | Valor esperado:1 | Valor recebido:'+eSprocEmiToStr(eS2205.ideEvento.ProcEmi));
  Check(eS2205.ideEvento.VerProc = '1.00', 'ideEvento.verProc | Valor esperado:1.00 | Valor recebido:'+eS2205.IdeEvento.verProc);

  Check(eS2205.IdeEmpregador.TpInsc = tiCNPJ, 'ideEmpregador.tpInsc | Valor esperado:1 | Valor recebido:'+eSTpInscricaoToStr(eS2205.ideEmpregador.tpInsc));
  Check(eS2205.IdeEmpregador.NrInsc = '12345678000123', 'ideEmpregador.nrInsc | Valor esperado:12345678000123 | Valor recebido:'+eS2205.ideEmpregador.NrInsc);

  Check(eS2205.ideTrabalhador.cpfTrab = '12345678901',
       'ideEmpregador.nrInsc | '+
       'Valor esperado:12345678901 | '+
       'Valor recebido:'+eS2205.ideTrabalhador.cpfTrab);

  Check(eS2205.dtAlteracao = StrToDateTime('07/05/2018'),
       'dtAlteracao | '+
       'Valor esperado:07/05/2018 | '+
       'Valor recebido:'+DateTimeToStr(eS2205.dtAlteracao));

  Check(eS2205.trabalhador.NmTrab = 'Nome do Trabalhador',
       'trabalhador.NmTrab | '+
       'Valor esperado:Nome do Trabalhador | '+
       'Valor recebido:'+eS2205.trabalhador.NmTrab);

  Check(eS2205.trabalhador.Sexo = 'M',
       'trabalhador.Sexo | '+
       'Valor esperado:M | '+
       'Valor recebido:'+eS2205.trabalhador.Sexo);

  Check(eS2205.trabalhador.RacaCor = 1,
       'trabalhador.RacaCor | '+
       'Valor esperado:1 | '+
       'Valor recebido:'+IntToStr(eS2205.trabalhador.RacaCor));

  Check(eS2205.trabalhador.EstCiv = 1,
       'trabalhador.EstCiv | '+
       'Valor esperado:1 | '+
       'Valor recebido:'+IntToStr(eS2205.trabalhador.EstCiv));

  Check(eS2205.trabalhador.GrauInstr = '01',
       'trabalhador.GrauInstr | '+
       'Valor esperado:01 | '+
       'Valor recebido:'+eS2205.trabalhador.GrauInstr);

  Check(eS2205.trabalhador.nmSoc = 'Nome Social',
       'trabalhador.nmSoc | '+
       'Valor esperado:Nome Social | '+
       'Valor recebido:'+eS2205.trabalhador.nmSoc);

  Check(eS2205.trabalhador.PaisNac = '1',
       'trabalhador.PaisNac | '+
       'Valor esperado:1 | '+
       'Valor recebido:'+eS2205.trabalhador.PaisNac);

  Check(eS2205.trabalhador.Endereco.Brasil.TpLograd = 'Rua',
       'trabalhador.Endereco.Brasil.TpLograd | '+
       'Valor esperado:Rua | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Brasil.TpLograd);

  Check(eS2205.trabalhador.Endereco.Brasil.DscLograd = 'Nove de Novembro',
       'trabalhador.Endereco.Brasil.DscLograd | '+
       'Valor esperado:Nove de Novembro | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Brasil.DscLograd);

  Check(eS2205.trabalhador.Endereco.Brasil.nrLograd = '100',
       'trabalhador.Endereco.Brasil.nrLograd | '+
       'Valor esperado:100 | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Brasil.nrLograd);

  Check(eS2205.trabalhador.Endereco.Brasil.Complemento = 'Apto 10',
       'trabalhador.Endereco.Brasil.Complemento | '+
       'Valor esperado:Apto 10 | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Brasil.Complemento);

  Check(eS2205.trabalhador.Endereco.Brasil.bairro = 'Centro',
       'trabalhador.Endereco.Brasil.bairro | '+
       'Valor esperado:Centro | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Brasil.bairro);

  Check(eS2205.trabalhador.Endereco.Brasil.cep = '14123456',
       'trabalhador.Endereco.Brasil.cep | '+
       'Valor esperado:14123456 | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Brasil.cep);

  Check(eS2205.trabalhador.Endereco.Brasil.codMunic = 3512345,
       'trabalhador.Endereco.Brasil.codMunic | '+
       'Valor esperado:3512345 | '+
       'Valor recebido:'+IntToStr(eS2205.trabalhador.Endereco.Brasil.codMunic));

  Check(eS2205.trabalhador.Endereco.Brasil.uf = 'SP',
       'trabalhador.Endereco.Brasil.uf | '+
       'Valor esperado:SP | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Brasil.uf);

  Check(eS2205.trabalhador.Endereco.Exterior.PaisResid = 'Argentina',
       'trabalhador.Endereco.Exterior.PaisResid | '+
       'Valor esperado:Argentina | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.PaisResid);

  Check(eS2205.trabalhador.Endereco.Exterior.PaisResid = 'Argentina',
       'trabalhador.Endereco.Exterior.PaisResid | '+
       'Valor esperado:Argentina | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.PaisResid);

  Check(eS2205.trabalhador.Endereco.Exterior.DscLograd = 'Rua La Paz',
       'trabalhador.Endereco.Exterior.DscLograd | '+
       'Valor esperado:Rua La Paz | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.DscLograd);

  Check(eS2205.trabalhador.Endereco.Exterior.nrLograd = '100',
       'trabalhador.Endereco.Exterior.nrLograd | '+
       'Valor esperado:100 | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.nrLograd);

  Check(eS2205.trabalhador.Endereco.Exterior.complemento = 'apto 123',
       'trabalhador.Endereco.Exterior.complemento | '+
       'Valor esperado:apto 123 | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.complemento);

  Check(eS2205.trabalhador.Endereco.Exterior.bairro = 'centro',
       'trabalhador.Endereco.Exterior.bairro | '+
       'Valor esperado:centro | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.bairro);

  Check(eS2205.trabalhador.Endereco.Exterior.nmCid = '123',
       'trabalhador.Endereco.Exterior.nmCid | '+
       'Valor esperado:123 | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.nmCid);

  Check(eS2205.trabalhador.Endereco.Exterior.codPostal = '123123',
       'trabalhador.Endereco.Exterior.codPostal | '+
       'Valor esperado:123123 | '+
       'Valor recebido:'+eS2205.trabalhador.Endereco.Exterior.codPostal);

  Check(eS2205.trabalhador.trabImig.tmpResid = ttrPrazoIndeterminado,
       'trabalhador.trabImig.tmpResid | '+
       'Valor esperado:1 | '+
       'Valor recebido:'+tpTmpResidToStr(eS2205.trabalhador.trabImig.tmpResid));

  Check(eS2205.trabalhador.trabImig.condIng = tciPermanenciaNoBrasilReuniaoFamiliar,
       'trabalhador.trabImig.condIng | '+
       'Valor esperado:3 | '+
       'Valor recebido:'+tpCondIngToStr(eS2205.trabalhador.trabImig.condIng));

  Check(eS2205.trabalhador.InfoDeficiencia.DefFisica = tpNao,
       'trabalhador.InfoDeficiencia.DefFisica | '+
       'Valor esperado:N | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.InfoDeficiencia.DefFisica));

  Check(eS2205.trabalhador.InfoDeficiencia.defVisual = tpNao,
       'trabalhador.InfoDeficiencia.defVisual | '+
       'Valor esperado:N | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.InfoDeficiencia.defVisual));

  Check(eS2205.trabalhador.InfoDeficiencia.defAuditiva = tpNao,
       'trabalhador.InfoDeficiencia.defAuditiva | '+
       'Valor esperado:N | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.InfoDeficiencia.defAuditiva));

  Check(eS2205.trabalhador.InfoDeficiencia.defMental = tpNao,
       'trabalhador.InfoDeficiencia.defMental | '+
       'Valor esperado:N | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.InfoDeficiencia.defMental));

  Check(eS2205.trabalhador.InfoDeficiencia.defIntelectual = tpNao,
       'trabalhador.InfoDeficiencia.defIntelectual | '+
       'Valor esperado:N | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.InfoDeficiencia.defIntelectual));

  Check(eS2205.trabalhador.InfoDeficiencia.reabReadap = tpNao,
       'trabalhador.InfoDeficiencia.reabReadap | '+
       'Valor esperado:N | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.InfoDeficiencia.reabReadap));

  Check(eS2205.trabalhador.InfoDeficiencia.infoCota = snfNao,
       'trabalhador.InfoDeficiencia.infoCota | '+
       'Valor esperado:N | '+
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2205.trabalhador.InfoDeficiencia.infoCota));

  Check(eS2205.trabalhador.InfoDeficiencia.observacao = 'Observacao',
       'trabalhador.InfoDeficiencia.observacao | '+
       'Valor esperado:Observacao | '+
       'Valor recebido:'+eS2205.trabalhador.InfoDeficiencia.observacao);

  Check(eS2205.trabalhador.Dependente.Items[0].tpDep = tdConjuge,
       'trabalhador.Dependente.tpDep | '+
       'Valor esperado:01 | '+
       'Valor recebido:'+eStpDepToStr(eS2205.trabalhador.Dependente.Items[0].tpDep));

  Check(eS2205.trabalhador.Dependente.Items[0].nmDep = 'Nome do Dependente',
       'trabalhador.Dependente.nmDep | '+
       'Valor esperado:Nome do Dependente | '+
       'Valor recebido:'+eS2205.trabalhador.Dependente.Items[0].nmDep);

  Check(eS2205.trabalhador.Dependente.Items[0].dtNascto = StrToDateTime('05/03/2018'),
       'trabalhador.Dependente.dtNascto | '+
       'Valor esperado:05/03/2018 | '+
       'Valor recebido:'+DateTimeToStr(eS2205.trabalhador.Dependente.Items[0].dtNascto));

  Check(eS2205.trabalhador.Dependente.Items[0].cpfDep = '12345678901',
       'trabalhador.Dependente.cpfDep | '+
       'Valor esperado:12345678901 | '+
       'Valor recebido:'+eS2205.trabalhador.Dependente.Items[0].cpfDep);

  Check(eS2205.trabalhador.Dependente.Items[0].sexoDep = 'F',
       'trabalhador.Dependente.sexoDep | '+
       'Valor esperado:F | '+
       'Valor recebido:'+eS2205.trabalhador.Dependente.Items[0].sexoDep);

  Check(eS2205.trabalhador.Dependente.Items[0].depIRRF = tpSim,
       'trabalhador.Dependente.depIRRF | '+
       'Valor esperado:S | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.Dependente.Items[0].depIRRF));

  Check(eS2205.trabalhador.Dependente.Items[0].depSF = tpSim,
       'trabalhador.Dependente.depSF | '+
       'Valor esperado:S | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.Dependente.Items[0].depSF));

  Check(eS2205.trabalhador.Dependente.Items[0].incTrab = tpSim,
       'trabalhador.Dependente.incTrab | '+
       'Valor esperado:S | '+
       'Valor recebido:'+eSSimNaoToStr(eS2205.trabalhador.Dependente.Items[0].incTrab));

  Check(eS2205.trabalhador.Contato.FonePrinc = '1622334455',
       'trabalhador.Contato.FonePrinc | '+
       'Valor esperado:1622334455 | '+
       'Valor recebido:'+eS2205.trabalhador.Contato.FonePrinc);

  Check(eS2205.trabalhador.Contato.emailPrinc = 'a@a.com.br',
       'trabalhador.Contato.emailPrinc | '+
       'Valor esperado:a@a.com.br | '+
       'Valor recebido:'+eS2205.trabalhador.Contato.emailPrinc);

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2206Tests;
var
  eS2206: TEvtAltContratual;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2206.Count > 0, 'Não instanciou o S-2206 na lista');

  eS2206 := FACBreSocial.Eventos.NaoPeriodicos.S2206[0].EvtAltContratual;

  Check(eS2206.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2206.Sequencial));

  Check(eS2206.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2206.IdeEvento.indRetif));

  Check(eS2206.IdeEvento.nrRecibo = '123',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2206.IdeEvento.nrRecibo);

  Check(eS2206.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2206.IdeEvento.procEmi));

  Check(eS2206.IdeEvento.verProc = '1.00',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.00 | ' +
       'Valor recebido:'+eS2206.IdeEvento.verProc);

  Check(eS2206.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2206.IdeEmpregador.tpInsc));

  Check(eS2206.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2206.IdeEmpregador.nrInsc);

  Check(eS2206.IdeVinculo.cpfTrab = '12345678901',
       'IdeVinculo.cpfTrab | ' +
       'Valor esperado:12345678901 | ' +
       'Valor recebido:'+eS2206.IdeVinculo.cpfTrab);

  Check(eS2206.IdeVinculo.matricula = '123',
       'IdeVinculo.matricula | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2206.IdeVinculo.matricula);

  Check(eS2206.AltContratual.dtAlteracao = StrToDateTime('07/05/2018'),
       'AltContratual.dtAlteracao | ' +
       'Valor esperado:07/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2206.AltContratual.dtAlteracao));

  Check(eS2206.AltContratual.dtEf = StrToDateTime('08/05/2018'),
       'AltContratual.dtEf | ' +
       'Valor esperado:08/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2206.AltContratual.dtEf));

  Check(eS2206.AltContratual.dscAlt = 'Descricao da Alteracao',
       'AltContratual.dscAlt | ' +
       'Valor esperado:Descricao da Alteracao | ' +
       'Valor recebido:'+eS2206.AltContratual.dscAlt);

  Check(eS2206.AltContratual.Vinculo.tpRegPrev = rpRGPS,
       'AltContratual.Vinculo.tpRegPrev | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpRegPrevToStr(eS2206.AltContratual.Vinculo.tpRegPrev));

  Check(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.TpRegJor = rjTeletrabalhoPrevistoIncisoIIIArt62CLT,
       'AltContratual.infoRegimeTrab.InfoCeletista.TpRegJor | ' +
       'Valor esperado:4 | ' +
       'Valor recebido:'+eSTpRegJorToStr(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.TpRegJor));

  Check(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.natAtividade = navUrbano,
       'AltContratual.infoRegimeTrab.InfoCeletista.natAtividade | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSNatAtividadeToStr(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.natAtividade));

  Check(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.dtBase = 10,
       'AltContratual.infoRegimeTrab.InfoCeletista.dtBase | ' +
       'Valor esperado:10 | ' +
       'Valor recebido:'+IntToStr(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.dtBase));

  Check(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.cnpjSindCategProf = '12345678000123',
       'AltContratual.infoRegimeTrab.InfoCeletista.cnpjSindCategProf | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2206.AltContratual.infoRegimeTrab.InfoCeletista.cnpjSindCategProf);

  Check(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.TrabTemporario.justProrr = 'Justificativa',
       'AltContratual.infoRegimeTrab.InfoCeletista.TrabTemporario.justProrr | ' +
       'Valor esperado:Justificativa | ' +
       'Valor recebido:'+eS2206.AltContratual.infoRegimeTrab.InfoCeletista.TrabTemporario.justProrr);

  Check(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.aprend.TpInsc = tiCNPJ,
       'AltContratual.infoRegimeTrab.InfoCeletista.aprend.TpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.aprend.TpInsc));

  Check(eS2206.AltContratual.infoRegimeTrab.InfoCeletista.aprend.NrInsc = '12345678000123',
       'AltContratual.infoRegimeTrab.InfoCeletista.aprend.NrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2206.AltContratual.infoRegimeTrab.InfoCeletista.aprend.NrInsc);

  Check(eS2206.AltContratual.infoRegimeTrab.InfoEstatutario.tpPlanRP = prpSemSegregacaoDaMassa,
       'AltContratual.infoRegimeTrab.InfoEstatutario.tpPlanRP | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eSTpPlanRPToStr(eS2206.AltContratual.infoRegimeTrab.InfoEstatutario.tpPlanRP));

  Check(eS2206.AltContratual.infoRegimeTrab.InfoEstatutario.indTetoRGPS = snfNao,
       'AltContratual.infoRegimeTrab.InfoEstatutario.indTetoRGPS | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2206.AltContratual.infoRegimeTrab.InfoEstatutario.indTetoRGPS));

  Check(eS2206.AltContratual.infoRegimeTrab.InfoEstatutario.indAbonoPerm = snfNao,
       'AltContratual.infoRegimeTrab.InfoEstatutario.indAbonoPerm | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2206.AltContratual.infoRegimeTrab.InfoEstatutario.indAbonoPerm));

  Check(eS2206.AltContratual.infoContrato.nmCargo = 'nome cargo',
       'AltContratual.infoContrato.nmCargo | ' +
       'Valor esperado:nome cargo | ' +
       'Valor recebido:'+eS2206.AltContratual.infoContrato.nmCargo);

  Check(eS2206.AltContratual.infoContrato.CBOCargo = '999999',
       'AltContratual.infoContrato.CBOCargo | ' +
       'Valor esperado:999999 | ' +
       'Valor recebido:'+eS2206.AltContratual.infoContrato.CBOCargo);

  Check(eS2206.AltContratual.infoContrato.nmFuncao = 'nome funcao',
       'AltContratual.infoContrato.nmFuncao | ' +
       'Valor esperado:nome funcao | ' +
       'Valor recebido:'+eS2206.AltContratual.infoContrato.nmFuncao);

  Check(eS2206.AltContratual.infoContrato.CBOFuncao = '999999',
       'AltContratual.infoContrato.CBOFuncao | ' +
       'Valor esperado:n999999 | ' +
       'Valor recebido:'+eS2206.AltContratual.infoContrato.CBOFuncao);

  Check(eS2206.AltContratual.infoContrato.acumCargo = snfNao,
       'AltContratual.infoContrato.acumCargo | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoFacultativoToStr(eS2206.AltContratual.infoContrato.acumCargo));

  Check(eS2206.AltContratual.infoContrato.codCateg = 111,
       'AltContratual.infoContrato.codCateg | ' +
       'Valor esperado:111 | ' +
       'Valor recebido:'+IntToStr(eS2206.AltContratual.infoContrato.codCateg));

  Check(eS2206.AltContratual.infoContrato.Remuneracao.VrSalFx = 3000,
       'AltContratual.infoContrato.Remuneracao.VrSalFx | ' +
       'Valor esperado:3000 | ' +
       'Valor recebido:'+FloatToStr(eS2206.AltContratual.infoContrato.Remuneracao.VrSalFx));

  Check(eS2206.AltContratual.infoContrato.Remuneracao.UndSalFixo = sfPorHora,
       'AltContratual.infoContrato.Remuneracao.UndSalFixo | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSUndSalFixoToStr(eS2206.AltContratual.infoContrato.Remuneracao.UndSalFixo));

  Check(eS2206.AltContratual.infoContrato.Remuneracao.dscSalVar = 'Descricao',
         'AltContratual.infoContrato.Remuneracao.dscSalVar | ' +
         'Valor esperado:Descricao | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.Remuneracao.dscSalVar);

  Check(eS2206.AltContratual.infoContrato.Duracao.TpContr = PrazoDeterminadoVincOcDeUmFato,
         'AltContratual.infoContrato.Duracao.TpContr | ' +
         'Valor esperado:3 | ' +
         'Valor recebido:'+eSTpContrToStr(eS2206.AltContratual.infoContrato.Duracao.TpContr));

  Check(eS2206.AltContratual.infoContrato.Duracao.dtTerm = StrToDateTime('01/01/2020'),
         'AltContratual.infoContrato.Duracao.dtTerm | ' +
         'Valor esperado:01/01/2020 | ' +
         'Valor recebido:'+DateTimeToStr(eS2206.AltContratual.infoContrato.Duracao.dtTerm));

  Check(eS2206.AltContratual.infoContrato.Duracao.objDet = 'Safra 2020',
         'AltContratual.infoContrato.Duracao.objDet | ' +
         'Valor esperado:Safra 2020 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.Duracao.objDet);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc = tiCNPJ,
         'AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc | ' +
         'Valor esperado:1 | ' +
         'Valor recebido:'+eSTpInscricaoToStr(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.TpInsc));

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.nrInsc = '12345678000123',
         'AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.nrInsc | ' +
         'Valor esperado:12345678000123 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.nrInsc);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.DescComp = 'Descricao complementar',
         'AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.descComp | ' +
         'Valor esperado:Descricao complementar | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTrabGeral.descComp);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.TpLograd = 'R',
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.TpLograd | ' +
         'Valor esperado:R | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.TpLograd);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.dscLograd = 'rua do logradouro',
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.dscLograd | ' +
         'Valor esperado:rua do logradouro | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.dscLograd);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.nrLograd = '123',
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.nrLograd | ' +
         'Valor esperado:123 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.nrLograd);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.Complemento = 'apto 123',
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.Complemento | ' +
         'Valor esperado:apto 123 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.Complemento);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.bairro = 'Centro',
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.bairro | ' +
         'Valor esperado:Centro | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.bairro);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.cep = '13400000',
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.cep | ' +
         'Valor esperado:13400000 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.cep);

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.codMunic = 3538709,
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.codMunic | ' +
         'Valor esperado:3538709 | ' +
         'Valor recebido:'+IntToStr(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.codMunic));

  Check(eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.uf = 'SP',
         'AltContratual.infoContrato.LocalTrabalho.LocalTempDom.uf | ' +
         'Valor esperado:SP | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.LocalTrabalho.LocalTempDom.uf);

  Check(eS2206.AltContratual.infoContrato.HorContratual.qtdHrsSem = 240,
         'AltContratual.infoContrato.HorContratual.qtdHrsSem | ' +
         'Valor esperado:240 | ' +
         'Valor recebido:'+FloatToStr(eS2206.AltContratual.infoContrato.HorContratual.qtdHrsSem));

  Check(eS2206.AltContratual.infoContrato.HorContratual.tpJornada = tjJornada12x36,
         'AltContratual.infoContrato.HorContratual.tpJornada | ' +
         'Valor esperado:2 | ' +
         'Valor recebido:'+eSTpJornadaToStr(eS2206.AltContratual.infoContrato.HorContratual.tpJornada));

  Check(eS2206.AltContratual.infoContrato.HorContratual.tmpParc = tpNaoeTempoParcial,
         'AltContratual.infoContrato.HorContratual.tmpParc | ' +
         'Valor esperado:0 | ' +
         'Valor recebido:'+tpTmpParcToStr(eS2206.AltContratual.infoContrato.HorContratual.tmpParc));

  Check(eS2206.AltContratual.infoContrato.HorContratual.horNoturno = tpNao,
         'AltContratual.infoContrato.HorContratual.horNoturno | ' +
         'Valor esperado:N | ' +
         'Valor recebido:'+eSSimNaoToStr(eS2206.AltContratual.infoContrato.HorContratual.horNoturno));

  Check(eS2206.AltContratual.infoContrato.HorContratual.dscJorn = 'descricao jornada trabalho',
         'AltContratual.infoContrato.HorContratual.dscJorn | ' +
         'Valor esperado:descricao jornada trabalho | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.HorContratual.dscJorn);

  Check(eS2206.AltContratual.infoContrato.AlvaraJudicial.NrProcJud = '12345678901234567890',
         'AltContratual.infoContrato.AlvaraJudicial.NrProcJud | ' +
         'Valor esperado:12345678901234567890 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.AlvaraJudicial.NrProcJud);

  Check(eS2206.AltContratual.infoContrato.observacoes.Items[0].observacao = 'Observacao 1',
         'AltContratual.infoContrato.observacoes.observacao | ' +
         'Valor esperado:Observacao 1 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.observacoes.Items[0].observacao);

  Check(eS2206.AltContratual.infoContrato.observacoes.Items[1].observacao = 'Observacao 2',
         'AltContratual.infoContrato.observacoes.observacao | ' +
         'Valor esperado:Observacao 2 | ' +
         'Valor recebido:'+eS2206.AltContratual.infoContrato.observacoes.Items[1].observacao);

  Check(eS2206.AltContratual.infoContrato.treiCap.Items[0].codTreiCap = 1006,
         'AltContratual.infoContrato.treiCap.codTreiCap | ' +
         'Valor esperado:1006 | ' +
         'Valor recebido:'+IntToStr(eS2206.AltContratual.infoContrato.treiCap.Items[0].codTreiCap));

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2210Tests;
var
  eS2210: TEvtCAT;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2210.Count > 0, 'Não instanciou o S-2210 na lista');

  eS2210 := FACBreSocial.Eventos.NaoPeriodicos.S2210[0].EvtCAT;

  Check(eS2210.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2210.Sequencial));

  Check(eS2210.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2210.IdeEvento.indRetif));

  Check(eS2210.IdeEvento.nrRecibo = '123',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2210.IdeEvento.nrRecibo);

  Check(eS2210.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2210.IdeEvento.procEmi));

  Check(eS2210.IdeEvento.verProc = '1.00',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.00 | ' +
       'Valor recebido:'+eS2210.IdeEvento.verProc);

  Check(eS2210.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2210.IdeEmpregador.tpInsc));

  Check(eS2210.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2210.IdeEmpregador.nrInsc);

  Check(eS2210.IdeVinculo.cpfTrab = '11111111111',
       'IdeVinculo.cpfTrab | ' +
       'Valor esperado:11111111111 | ' +
       'Valor recebido:'+eS2210.IdeVinculo.cpfTrab);

  Check(eS2210.IdeVinculo.matricula = '123',
       'IdeVinculo.matricula | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2210.IdeVinculo.matricula);

  Check(eS2210.IdeVinculo.codCateg = 1,
       'IdeVinculo.codCateg | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+IntToStr(eS2210.IdeVinculo.codCateg));

  Check(eS2210.Cat.dtAcid = StrToDateTime('08/05/2018'),
       'Cat.dtAcid | ' +
       'Valor esperado:08/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2210.Cat.dtAcid));

  Check(eS2210.Cat.tpAcid = '1.0.01',
       'Cat.tpAcid | ' +
       'Valor esperado:1.0.01 | ' +
       'Valor recebido:'+eS2210.Cat.tpAcid);

  Check(eS2210.Cat.hrAcid = '0800',
       'Cat.hrAcid | ' +
       'Valor esperado:0800 | ' +
       'Valor recebido:'+eS2210.Cat.hrAcid);

  Check(eS2210.Cat.hrsTrabAntesAcid = '0100',
       'Cat.hrsTrabAntesAcid | ' +
       'Valor esperado:0100 | ' +
       'Valor recebido:'+eS2210.Cat.hrsTrabAntesAcid);

  Check(eS2210.Cat.tpCat = tcInicial,
       'Cat.tpCat | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpCatToStr(eS2210.Cat.tpCat));

  Check(eS2210.Cat.indCatObito = tpNao,
       'Cat.indCatObito | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2210.Cat.indCatObito));

  Check(eS2210.Cat.dtOBito = 0,
       'Cat.dtOBito | ' +
       'Valor esperado: | ' +
       'Valor recebido:'+DateTimeToStr(eS2210.Cat.dtOBito));

  Check(eS2210.Cat.indComunPolicia = tpNao,
       'Cat.indComunPolicia | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2210.Cat.indComunPolicia));

  Check(eS2210.Cat.codSitGeradora = 200004300,
       'Cat.codSitGeradora | ' +
       'Valor esperado:200004300 | ' +
       'Valor recebido:'+IntToStr(eS2210.Cat.codSitGeradora));

  Check(eS2210.Cat.iniciatCAT = icIniciativaEmpregador,
       'Cat.iniciatCAT | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIniciatCATToStr(eS2210.Cat.iniciatCAT));

  Check(eS2210.Cat.obsCAT = 'observacao',
       'Cat.obsCAT | ' +
       'Valor esperado:observacao | ' +
       'Valor recebido:'+eS2210.Cat.obsCAT);

  Check(eS2210.Cat.ultDiaTrab = StrToDateTime('01/01/2022'),
       'Cat.ultDiaTrab | ' +
       'Valor esperado:01/01/2022 | ' +
       'Valor recebido:'+DateTimeToStr(eS2210.Cat.ultDiaTrab));

  Check(eS2210.Cat.houveAfast = tpSim,
       'Cat.houveAfast | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2210.Cat.houveAfast));

  Check(eS2210.Cat.localAcidente.tpLocal = tlEstabEmpregadorBrasil,
       'Cat.localAcidente.tpLocal | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpLocalToStr(eS2210.Cat.localAcidente.tpLocal));

  Check(eS2210.Cat.localAcidente.dscLocal = 'Oficina',
       'Cat.localAcidente.dscLocal | ' +
       'Valor esperado:Oficina | ' +
       'Valor recebido:'+eS2210.Cat.localAcidente.dscLocal);

  Check(eS2210.Cat.localAcidente.dscLocal = 'Oficina',
       'Cat.localAcidente.dscLocal | ' +
       'Valor esperado:Oficina | ' +
       'Valor recebido:'+eS2210.Cat.localAcidente.dscLocal);

  Check(eS2210.Cat.localAcidente.TpLograd = 'R',
       'Cat.localAcidente.TpLograd | '+
       'Valor esperado:R | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.TpLograd);

  Check(eS2210.Cat.localAcidente.DscLograd = 'Rua Central',
       'Cat.localAcidente.DscLograd | '+
       'Valor esperado:Rua Central | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.DscLograd);

  Check(eS2210.Cat.localAcidente.nrLograd = '100',
       'Cat.localAcidente.nrLograd | '+
       'Valor esperado:100 | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.nrLograd);

  Check(eS2210.Cat.localAcidente.Complemento = '',
       'Cat.localAcidente.Complemento | '+
       'Valor esperado: | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.Complemento);

  Check(eS2210.Cat.localAcidente.bairro = 'Centro',
       'Cat.localAcidente.bairro | '+
       'Valor esperado:Centro | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.bairro);

  Check(eS2210.Cat.localAcidente.cep = '14123456',
       'Cat.localAcidente.cep | '+
       'Valor esperado:14123456 | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.cep);

  Check(eS2210.Cat.localAcidente.codMunic = 3512345,
       'Cat.localAcidente.codMunic | '+
       'Valor esperado:3512345 | '+
       'Valor recebido:'+IntToStr(eS2210.Cat.localAcidente.codMunic));

  Check(eS2210.Cat.localAcidente.uf = 'SP',
       'Cat.localAcidente.uf | '+
       'Valor esperado:SP | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.uf);

  Check(eS2210.Cat.localAcidente.pais = '008',
       'Cat.localAcidente.pais | '+
       'Valor esperado:008 | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.pais);

  Check(eS2210.Cat.localAcidente.codPostal = '14234012',
       'Cat.localAcidente.codPostal | '+
       'Valor esperado:14234012 | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.codPostal);

  Check(eS2210.Cat.localAcidente.ideLocalAcid.tpInsc = tiCNPJ,
       'Cat.localAcidente.ideLocalAcid.tpInsc | '+
       'Valor esperado:1 | '+
       'Valor recebido:'+eSTpInscricaoToStr(eS2210.Cat.localAcidente.ideLocalAcid.tpInsc));

  Check(eS2210.Cat.localAcidente.ideLocalAcid.nrInsc = '22222222222222',
       'Cat.localAcidente.ideLocalAcid.nrInsc | '+
       'Valor esperado:22222222222222 | '+
       'Valor recebido:'+eS2210.Cat.localAcidente.ideLocalAcid.nrInsc);

  //Check(eS2210.Cat.parteAtingida.Items[0].codParteAting = 753030000,
  //     'Cat.parteAtingida.codParteAting | '+
  //     'Valor esperado:753030000 | '+
  //     'Valor recebido:'+IntToStr(eS2210.Cat.parteAtingida.Items[0].codParteAting));
  //
  //Check(eS2210.Cat.parteAtingida.Items[0].lateralidade = laEsquerda,
  //     'Cat.parteAtingida.lateralidade | '+
  //     'Valor esperado:1 | '+
  //     'Valor recebido:'+eSLateralidadeToStr(eS2210.Cat.parteAtingida.Items[0].lateralidade));
  //
  //Check(eS2210.Cat.agenteCausador.Items[0].codAgntCausador = 302010300,
  //     'Cat.agenteCausador.codAgntCausador | '+
  //     'Valor esperado:302010300 | '+
  //     'Valor recebido:'+eSLateralidadeToStr(eS2210.Cat.agenteCausador.Items[0].codAgntCausador));

  Check(eS2210.Cat.atestado.dtAtendimento = StrToDateTime('08/05/2018'),
       'Cat.atestado.dtAtendimento | '+
       'Valor esperado:08/05/2018 | '+
       'Valor recebido:'+DateTimeToStr(eS2210.Cat.atestado.dtAtendimento));

  Check(eS2210.Cat.atestado.hrAtendimento = '0830',
       'Cat.atestado.hrAtendimento | '+
       'Valor esperado:0830 | '+
       'Valor recebido:'+eS2210.Cat.atestado.hrAtendimento);

  Check(eS2210.Cat.atestado.indInternacao = tpSim,
       'Cat.atestado.indInternacao | '+
       'Valor esperado:S | '+
       'Valor recebido:'+eSSimNaoToStr(eS2210.Cat.atestado.indInternacao));

  Check(eS2210.Cat.atestado.durTrat = 5,
       'Cat.atestado.durTrat | '+
       'Valor esperado:5 | '+
       'Valor recebido:'+IntToStr(eS2210.Cat.atestado.durTrat));

  Check(eS2210.Cat.atestado.indAfast = tpSim,
       'Cat.atestado.indAfast | '+
       'Valor esperado:S | '+
       'Valor recebido:'+eSSimNaoToStr(eS2210.Cat.atestado.indAfast));

  Check(eS2210.Cat.atestado.dscLesao = 1,
       'Cat.atestado.dscLesao | '+
       'Valor esperado:1 | '+
       'Valor recebido:'+IntToStr(eS2210.Cat.atestado.dscLesao));

  Check(eS2210.Cat.atestado.dscCompLesao = 'descricao',
       'Cat.atestado.dscCompLesao | '+
       'Valor esperado:descricao | '+
       'Valor recebido:'+eS2210.Cat.atestado.dscCompLesao);

  Check(eS2210.Cat.atestado.diagProvavel = 'Diagnostivo',
       'Cat.atestado.diagProvavel | '+
       'Valor esperado:Diagnostivo | '+
       'Valor recebido:'+eS2210.Cat.atestado.diagProvavel);

  Check(eS2210.Cat.atestado.codCID = '1234',
       'Cat.atestado.codCID | '+
       'Valor esperado:1234 | '+
       'Valor recebido:'+eS2210.Cat.atestado.codCID);

  Check(eS2210.Cat.atestado.observacao = 'observacao',
       'Cat.atestado.observacao | '+
       'Valor esperado:observacao | '+
       'Valor recebido:'+eS2210.Cat.atestado.observacao);

  Check(eS2210.Cat.atestado.emitente.nmEmit = 'Nome',
       'Cat.atestado.emitente.nmEmit | '+
       'Valor esperado:Nome | '+
       'Valor recebido:'+eS2210.Cat.atestado.emitente.nmEmit);

  Check(eS2210.Cat.atestado.emitente.ideOC = idCRM,
       'Cat.atestado.emitente.ideOC | '+
       'Valor esperado:1 | '+
       'Valor recebido:'+eSIdeOCToStrEX(eS2210.Cat.atestado.emitente.ideOC));

  Check(eS2210.Cat.atestado.emitente.nrOc = '123',
       'Cat.atestado.emitente.nrOc | '+
       'Valor esperado:123 | '+
       'Valor recebido:'+eS2210.Cat.atestado.emitente.nrOc);

  Check(eS2210.Cat.atestado.emitente.ufOC = 'SP',
       'Cat.atestado.emitente.ufOC | '+
       'Valor esperado:SP | '+
       'Valor recebido:'+eS2210.Cat.atestado.emitente.ufOC);

  Check(eS2210.Cat.catOrigem.nrRecCatOrig = '123',
       'Cat.catOrigem.nrRecCatOrig | '+
       'Valor esperado:123 | '+
       'Valor recebido:'+eS2210.Cat.catOrigem.nrRecCatOrig);

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2220Tests;
var
  eS2220: TevtMonit;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2220.Count > 0, 'Não instanciou o S-2220 na lista');

  eS2220 := FACBreSocial.Eventos.NaoPeriodicos.S2220[0].evtMonit;

  Check(eS2220.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2220.Sequencial));

  Check(eS2220.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2220.IdeEvento.indRetif));

  Check(eS2220.IdeEvento.nrRecibo = '123',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2220.IdeEvento.nrRecibo);

  Check(eS2220.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2220.IdeEvento.procEmi));

  Check(eS2220.IdeEvento.verProc = '1.00',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.00 | ' +
       'Valor recebido:'+eS2220.IdeEvento.verProc);

  Check(eS2220.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2220.IdeEmpregador.tpInsc));

  Check(eS2220.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2220.IdeEmpregador.nrInsc);

  Check(eS2220.IdeVinculo.cpfTrab = '12345678901',
       'IdeVinculo.cpfTrab | ' +
       'Valor esperado:12345678901 | ' +
       'Valor recebido:'+eS2220.IdeVinculo.cpfTrab);

  Check(eS2220.IdeVinculo.matricula = '123',
       'IdeVinculo.matricula | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2220.IdeVinculo.matricula);

  Check(eS2220.IdeVinculo.codCateg = 101,
       'IdeVinculo.codCateg | ' +
       'Valor esperado:101 | ' +
       'Valor recebido:'+IntToStr(eS2220.IdeVinculo.codCateg));

  Check(eS2220.exMedOcup.tpExameOcup = taAdmissional,
       'exMedOcup.tpExameOcup | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eSTpExameOcupToStr(eS2220.exMedOcup.tpExameOcup));

  Check(eS2220.exMedOcup.Aso.DtAso = StrToDateTime('08/05/2018'),
       'exMedOcup.Aso.DtAso | ' +
       'Valor esperado:08/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2220.exMedOcup.Aso.DtAso));

  Check(eS2220.exMedOcup.Aso.ResAso = raApto,
       'exMedOcup.Aso.ResAso | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSResAsoToStr(eS2220.exMedOcup.Aso.ResAso));

  Check(eS2220.exMedOcup.Aso.Exame.Items[0].DtExm = StrToDateTime('08/05/2018'),
       'exMedOcup.Aso.Exame.DtExm | ' +
       'Valor esperado:08/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2220.exMedOcup.Aso.Exame.Items[0].DtExm));

  Check(eS2220.exMedOcup.Aso.Exame.Items[0].ProcRealizado = '0',
       'exMedOcup.Aso.Exame.ProcRealizado | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+eS2220.exMedOcup.Aso.Exame.Items[0].ProcRealizado);

  Check(eS2220.exMedOcup.Aso.Exame.Items[0].obsProc = 'Observacao',
       'exMedOcup.Aso.Exame.obsProc | ' +
       'Valor esperado:Observacao | ' +
       'Valor recebido:'+eS2220.exMedOcup.Aso.Exame.Items[0].obsProc);

  Check(eS2220.exMedOcup.Aso.Exame.Items[0].ordExame = oeSequencial,
       'exMedOcup.Aso.Exame.ordExame | ' +
       'Valor esperado:2 | ' +
       'Valor recebido:'+eSOrdExameToStr(eS2220.exMedOcup.Aso.Exame.Items[0].ordExame));

  Check(eS2220.exMedOcup.Aso.Exame.Items[0].indResult = irNormal,
       'exMedOcup.Aso.Exame.indResult | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndResultToStr(eS2220.exMedOcup.Aso.Exame.Items[0].indResult));

  Check(eS2220.exMedOcup.Aso.Medico.NmMed = 'Nome do Medico',
       'exMedOcup.Aso.Medico.NmMed | ' +
       'Valor esperado:Nome do Medico | ' +
       'Valor recebido:'+eS2220.exMedOcup.Aso.Medico.NmMed);

  Check(eS2220.exMedOcup.Aso.Medico.nrCRM = '123',
       'exMedOcup.Aso.Medico.nrCRM | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2220.exMedOcup.Aso.Medico.nrCRM);

  Check(eS2220.exMedOcup.Aso.Medico.ufCRM = 'UF',
       'exMedOcup.Aso.Medico.ufCRM | ' +
       'Valor esperado:UF | ' +
       'Valor recebido:'+eS2220.exMedOcup.Aso.Medico.ufCRM);

  Check(eS2220.exMedOcup.RespMonit.cpfResp = '11111111111',
       'exMedOcup.RespMonit.cpfResp | ' +
       'Valor esperado:11111111111 | ' +
       'Valor recebido:'+eS2220.exMedOcup.RespMonit.cpfResp);

  Check(eS2220.exMedOcup.RespMonit.nmResp = 'Nome do médico',
       'exMedOcup.RespMonit.nmResp | ' +
       'Valor esperado:Nome do médico | ' +
       'Valor recebido:'+eS2220.exMedOcup.RespMonit.nmResp);

  Check(eS2220.exMedOcup.RespMonit.nrCRM = '123456',
       'exMedOcup.RespMonit.nrCRM | ' +
       'Valor esperado:123456 | ' +
       'Valor recebido:'+eS2220.exMedOcup.RespMonit.nrCRM);

  Check(eS2220.exMedOcup.RespMonit.ufCRM = 'SP',
       'exMedOcup.RespMonit.ufCRM | ' +
       'Valor esperado:SP | ' +
       'Valor recebido:'+eS2220.exMedOcup.RespMonit.ufCRM);

end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2230Tests;
var
  eS2230: TEvtAfastTemp;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2230.Count > 0, 'Não instanciou o S-2230 na lista');

  eS2230 := FACBreSocial.Eventos.NaoPeriodicos.S2230[0].EvtAfastTemp;

  Check(eS2230.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2230.Sequencial));

  Check(eS2230.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2230.IdeEvento.indRetif));

  Check(eS2230.IdeEvento.nrRecibo = '123',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2230.IdeEvento.nrRecibo);

  Check(eS2230.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2230.IdeEvento.procEmi));

  Check(eS2230.IdeEvento.verProc = '1.00',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.00 | ' +
       'Valor recebido:'+eS2230.IdeEvento.verProc);

  Check(eS2230.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2230.IdeEmpregador.tpInsc));

  Check(eS2230.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2230.IdeEmpregador.nrInsc);

  Check(eS2230.IdeVinculo.cpfTrab = '12345678901',
       'IdeVinculo.cpfTrab | ' +
       'Valor esperado:12345678901 | ' +
       'Valor recebido:'+eS2230.IdeVinculo.cpfTrab);

  Check(eS2230.IdeVinculo.matricula = '123',
       'IdeVinculo.matricula | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2230.IdeVinculo.matricula);

  Check(eS2230.IdeVinculo.codCateg = 456,
       'IdeVinculo.codCateg | ' +
       'Valor esperado:456 | ' +
       'Valor recebido:'+IntToStr(eS2230.IdeVinculo.codCateg));

  Check(eS2230.infoAfastamento.iniAfastamento.DtIniAfast = StrToDate('08/05/2018'),
       'infoAfastamento.iniAfastamento.DtIniAfast | ' +
       'Valor esperado:08/05/2018 | ' +
       'Valor recebido:'+DateToStr(eS2230.infoAfastamento.iniAfastamento.DtIniAfast));

  Check(eS2230.infoAfastamento.iniAfastamento.codMotAfast = mtvAcidenteDoencaTrabalho,
       'infoAfastamento.iniAfastamento.codMotAfast | ' +
       'Valor esperado:01 | ' +
       'Valor recebido:'+eStpMotivosAfastamentoToStr(eS2230.infoAfastamento.iniAfastamento.codMotAfast));

  Check(eS2230.infoAfastamento.iniAfastamento.infoMesmoMtv = tpSim,
       'infoAfastamento.iniAfastamento.infoMesmoMtv | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2230.infoAfastamento.iniAfastamento.infoMesmoMtv));

  Check(eS2230.infoAfastamento.iniAfastamento.tpAcidTransito = tpatAtropelamento,
       'infoAfastamento.iniAfastamento.tpAcidTransito | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eStpTpAcidTransitoToStr(eS2230.infoAfastamento.iniAfastamento.tpAcidTransito));

  Check(eS2230.infoAfastamento.iniAfastamento.Observacao = 'Observacao',
       'infoAfastamento.iniAfastamento.Observacao | ' +
       'Valor esperado:Observacao | ' +
       'Valor recebido:'+eS2230.infoAfastamento.iniAfastamento.Observacao);

  Check(eS2230.infoAfastamento.iniAfastamento.perAquis.dtInicio = StrToDateTime('08/05/2018'),
       'infoAfastamento.iniAfastamento.perAquis.dtInicio | ' +
       'Valor esperado:08/05/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2230.infoAfastamento.iniAfastamento.perAquis.dtInicio));

  Check(eS2230.infoAfastamento.iniAfastamento.perAquis.dtFim = StrToDateTime('08/06/2018'),
       'infoAfastamento.iniAfastamento.perAquis.dtFim | ' +
       'Valor esperado:08/06/2018 | ' +
       'Valor recebido:'+DateTimeToStr(eS2230.infoAfastamento.iniAfastamento.perAquis.dtFim));

  Check(eS2230.infoAfastamento.iniAfastamento.infoCessao.cnpjCess = '12345678000123',
       'infoAfastamento.iniAfastamento.infoCessao.cnpjCess | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2230.infoAfastamento.iniAfastamento.infoCessao.cnpjCess);

  Check(eS2230.infoAfastamento.iniAfastamento.infoCessao.infOnus = ocCedente,
       'infoAfastamento.iniAfastamento.infoCessao.infOnus | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+tpInfOnusToStr(eS2230.infoAfastamento.iniAfastamento.infoCessao.infOnus));

  Check(eS2230.infoAfastamento.iniAfastamento.infoMandSind.cnpjSind = '12345678000123',
       'infoAfastamento.iniAfastamento.infoMandSind.cnpjSind | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2230.infoAfastamento.iniAfastamento.infoMandSind.cnpjSind);

  Check(eS2230.infoAfastamento.iniAfastamento.infoMandSind.infOnusRemun = orEmpregador,
       'infoAfastamento.iniAfastamento.infoMandSind.infOnusRemun | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+tpOnusRemunToStr(eS2230.infoAfastamento.iniAfastamento.infoMandSind.infOnusRemun));

  Check(eS2230.infoAfastamento.iniAfastamento.infoMandElet.cnpjMandElet = '12345678901234',
       'infoAfastamento.iniAfastamento.infoMandElet.cnpjMandElet | ' +
       'Valor esperado:12345678901234 | ' +
       'Valor recebido:'+eS2230.infoAfastamento.iniAfastamento.infoMandElet.cnpjMandElet);

  Check(eS2230.infoAfastamento.iniAfastamento.infoMandElet.indRemunCargo = tpNao,
       'infoAfastamento.iniAfastamento.infoMandElet.indRemunCargo | ' +
       'Valor esperado:N | ' +
       'Valor recebido:'+eSSimNaoToStr(eS2230.infoAfastamento.iniAfastamento.infoMandElet.indRemunCargo));

  Check(eS2230.infoAfastamento.infoRetif.origRetif = 1,
       'infoAfastamento.infoRetif.origRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+IntToStr(eS2230.infoAfastamento.infoRetif.origRetif));

  Check(eS2230.infoAfastamento.infoRetif.tpProc = tpAdministrativo,
       'infoAfastamento.infoRetif.tpProc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpProcessoToStr(eS2230.infoAfastamento.infoRetif.tpProc));

  Check(eS2230.infoAfastamento.infoRetif.nrProc = '123',
       'infoAfastamento.infoRetif.nrProc | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2230.infoAfastamento.infoRetif.nrProc);

  Check(eS2230.infoAfastamento.fimAfastamento.dtTermAfast = StrToDateTime('11/05/2018'),
       'infoAfastamento.fimAfastamento.dtTermAfast | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2230.infoAfastamento.infoRetif.nrProc);


end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2231Tests;
var
  eS2231: TEvtCessao;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2231.Count > 0, 'Não instanciou o S-2231 na lista');

  eS2231 := FACBreSocial.Eventos.NaoPeriodicos.S2231[0].EvtCessao;

  Check(eS2231.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2231.Sequencial));

  Check(eS2231.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2231.IdeEvento.indRetif));

  Check(eS2231.IdeEvento.nrRecibo = '123',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2231.IdeEvento.nrRecibo);

  Check(eS2231.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2231.IdeEvento.procEmi));

  Check(eS2231.IdeEvento.verProc = '1.00',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.00 | ' +
       'Valor recebido:'+eS2231.IdeEvento.verProc);

  Check(eS2231.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2231.IdeEmpregador.tpInsc));

  Check(eS2231.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2231.IdeEmpregador.nrInsc);

  Check(eS2231.IdeVinculo.cpfTrab = '12345678901',
       'IdeVinculo.cpfTrab | ' +
       'Valor esperado:12345678901 | ' +
       'Valor recebido:'+eS2231.IdeVinculo.cpfTrab);

  Check(eS2231.IdeVinculo.matricula = '123',
       'IdeVinculo.matricula | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2231.IdeVinculo.matricula);

  Check(eS2231.InfoCessao.iniCessao.dtIniCessao = StrToDate('08/05/2018'),
       'InfoCessao.iniCessao.dtIniCessao | ' +
       'Valor esperado:08/05/2018 | ' +
       'Valor recebido:'+DateToStr(eS2231.InfoCessao.iniCessao.dtIniCessao));

  Check(eS2231.InfoCessao.iniCessao.cnpjCess = '12345678000123',
       'InfoCessao.iniCessao.cnpjCess | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2231.InfoCessao.iniCessao.cnpjCess);

  Check(eS2231.InfoCessao.iniCessao.respRemun = tpSim,
       'InfoCessao.iniCessao.respRemun | ' +
       'Valor esperado:S | ' +
       'Valor recebido:'+ eSSimNaoToStr(eS2231.InfoCessao.iniCessao.respRemun));

  Check(eS2231.InfoCessao.fimCessao.dtTermCessao = StrToDate('10/05/2018'),
       'InfoCessao.fimCessao.dtTermCessao | ' +
       'Valor esperado:10/05/2018 | ' +
       'Valor recebido:'+DateToStr(eS2231.InfoCessao.fimCessao.dtTermCessao));


end;

procedure TACBreSocialEventosNaoPeriodicosTest.ES2190Tests;
var
  eS2190: TEvtAdmPrelim;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2190.Count > 0, 'Não instanciou o S-2190 na lista');

  eS2190 := FACBreSocial.Eventos.NaoPeriodicos.S2190[0].EvtAdmPrelim;

  Check(eS2190.Sequencial = 0,
       'Sequencial | ' +
       'Valor esperado:0 | ' +
       'Valor recebido:'+IntToStr(eS2190.Sequencial));

  Check(eS2190.IdeEvento.indRetif = ireOriginal,
       'IdeEvento.indRetif | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSIndRetificacaoToStr(eS2190.IdeEvento.indRetif));

  Check(eS2190.IdeEvento.nrRecibo = '123',
       'IdeEvento.nrRecibo | ' +
       'Valor esperado:123 | ' +
       'Valor recebido:'+eS2190.IdeEvento.nrRecibo);

  Check(eS2190.IdeEvento.procEmi = peAplicEmpregador,
       'IdeEvento.procEmi | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSprocEmiToStr(eS2190.IdeEvento.procEmi));

  Check(eS2190.IdeEvento.verProc = '1.00',
       'IdeEvento.verProc | ' +
       'Valor esperado:1.00 | ' +
       'Valor recebido:'+eS2190.IdeEvento.verProc);

  Check(eS2190.IdeEmpregador.tpInsc = tiCNPJ,
       'IdeEmpregador.tpInsc | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpInscricaoToStr(eS2190.IdeEmpregador.tpInsc));

  Check(eS2190.IdeEmpregador.nrInsc = '12345678000123',
       'IdeEmpregador.nrInsc | ' +
       'Valor esperado:12345678000123 | ' +
       'Valor recebido:'+eS2190.IdeEmpregador.nrInsc);

  Check(eS2190.InfoRegPrelim.cpfTrab = '12345678901',
       'InfoRegPrelim.cpfTrab | ' +
       'Valor esperado:12345678901 | ' +
       'Valor recebido:'+eS2190.InfoRegPrelim.cpfTrab);

  Check(eS2190.InfoRegPrelim.dtNascto = StrToDateTime('10/10/1980'),
       'InfoRegPrelim.dtNascto | ' +
       'Valor esperado:10/10/1980 | ' +
       'Valor recebido:'+DateTimeToStr(eS2190.InfoRegPrelim.dtNascto));

  Check(eS2190.InfoRegPrelim.dtAdm = StrToDateTime('15/05/2000'),
       'InfoRegPrelim.dtAdm | ' +
       'Valor esperado:15/05/2000 | ' +
       'Valor recebido:'+DateTimeToStr(eS2190.InfoRegPrelim.dtAdm));

  Check(eS2190.InfoRegPrelim.matricula = 'dados matricula',
       'InfoRegPrelim.matricula | ' +
       'Valor esperado:dados matricula | ' +
       'Valor recebido:'+eS2190.InfoRegPrelim.matricula);

  Check(eS2190.InfoRegPrelim.codCateg = 301,
       'InfoRegPrelim.codCateg | ' +
       'Valor esperado:301 | ' +
       'Valor recebido:'+IntToStr(eS2190.InfoRegPrelim.codCateg));

  Check(eS2190.InfoRegPrelim.natAtividade = navUrbano,
       'InfoRegPrelim.natAtividade | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSNatAtividadeToStr(eS2190.InfoRegPrelim.natAtividade));

  Check(eS2190.InfoRegPrelim.infoRegCTPS.CBOCargo = '999999',
       'InfoRegPrelim.infoRegCTPS.CBOCargo | ' +
       'Valor esperado:999999 | ' +
       'Valor recebido:'+eS2190.InfoRegPrelim.infoRegCTPS.CBOCargo);

  Check(eS2190.InfoRegPrelim.infoRegCTPS.vrSalFx = 2000,
       'InfoRegPrelim.infoRegCTPS.vrSalFx | ' +
       'Valor esperado:2000 | ' +
       'Valor recebido:'+FloatToStr(eS2190.InfoRegPrelim.infoRegCTPS.vrSalFx));

  Check(eS2190.InfoRegPrelim.infoRegCTPS.undSalFixo = sfPorMes,
       'InfoRegPrelim.infoRegCTPS.undSalFixo | ' +
       'Valor esperado:5 | ' +
       'Valor recebido:'+eSUndSalFixoToStr(eS2190.InfoRegPrelim.infoRegCTPS.undSalFixo));

  Check(eS2190.InfoRegPrelim.infoRegCTPS.tpContr = PrazoIndeterminado,
       'InfoRegPrelim.infoRegCTPS.tpContr | ' +
       'Valor esperado:1 | ' +
       'Valor recebido:'+eSTpContrToStr(eS2190.InfoRegPrelim.infoRegCTPS.tpContr));

  Check(eS2190.InfoRegPrelim.infoRegCTPS.dtTerm = StrToDateTime('15/05/2001'),
       'InfoRegPrelim.infoRegCTPS.dtTerm | ' +
       'Valor esperado:15/05/2001 | ' +
       'Valor recebido:'+DateTimeToStr(eS2190.InfoRegPrelim.infoRegCTPS.dtTerm));

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

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2298_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2298.Count = 0, 'Lista de eventos S-2298 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2298_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0100_S2298);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2298Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2299_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2299.Count = 0, 'Lista de eventos S-2299 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2299_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2299);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2299Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2300_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2300.Count = 0, 'Lista de eventos S-2300 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2300_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2300);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2300Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2306_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2306.Count = 0, 'Lista de eventos S-2306 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2306_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2306);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2306Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2399_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2399.Count = 0, 'Lista de eventos S-2399 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2399_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2399);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2399Tests;
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
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2200);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2200Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2205_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2205.Count = 0, 'Lista de eventos S-2205 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2205_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2205);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2205Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2206_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2206.Count = 0, 'Lista de eventos S-2206 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2206_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2206);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2206Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2210_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2210.Count = 0, 'Lista de eventos S-2210 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2210_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2210);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2210Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2220_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2220.Count = 0, 'Lista de eventos S-2220 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2220_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2220);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2220Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2230_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2230.Count = 0, 'Lista de eventos S-2230 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2230_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2230);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2230Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2231_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2231.Count = 0, 'Lista de eventos S-2231 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2231_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2231);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2231Tests;
    end;
  end;
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventosNaoPeriodicosS2190_Create_ListaVazia;
begin
  Check(FACBreSocial.Eventos.NaoPeriodicos.S2190.Count = 0, 'Lista de eventos S-2190 não está vazia');
end;

procedure TACBreSocialEventosNaoPeriodicosTest.ACBreSocialEventos_LoadFromINI_LeuePreencheuS2190_vS0101;
begin
  try
    FACBreSocial.Configuracoes.Geral.VersaoDF := veS01_01_00;
    FACBreSocial.Eventos.Clear;
    FACBreSocial.Eventos.LoadFromINI(ARQINI_vS0101_S2190);
  except
    on E:Exception do
    begin
      CheckIs(E, EACBrDFeException, 'Era esperado EACBrDFeException, mas o erro foi ' + E.ClassName);
      ES2190Tests;
    end;
  end;
end;

end.

