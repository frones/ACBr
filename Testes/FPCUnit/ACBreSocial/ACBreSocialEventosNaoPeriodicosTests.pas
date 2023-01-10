unit ACBreSocialEventosNaoPeriodicosTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util, ACBreSocialTestsConsts, ACBreSocial,
  pcesS2400, pcesCommon, pcesConversaoeSocial;

type

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

implementation

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

end.

