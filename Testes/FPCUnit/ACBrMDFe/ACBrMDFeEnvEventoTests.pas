unit ACBrMDFeEnvEventoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrMDFe.EnvEvento, // Unit nova
  pcnConversao;

type

  { ACBrMDFeEnvEventoTest }

  ACBrMDFeEnvEventoTest = class(TTestCase)
  private
    FEnvEvento_New: TEventoMDFe;
    Item_new: TInfEventoCollectionItem;

    sxml_old: string;
    sxml_new: string;

    procedure Gerar_InfEvento(ATipoEvento: TpcnTpEvento; codOrgao: Integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarXml_Evento_Cancelamento;
    procedure GerarXml_Evento_Encerramento;
    procedure GerarXml_Evento_InclusaoCondutor;
    procedure GerarXml_Evento_InclusaoDFe;
    procedure GerarXml_Evento_PagamentoOperacao;
    procedure GerarXml_Evento_AlteracaoPagtoServMDFe;
    procedure GerarXml_Evento_ConfirmaServMDFe;

    procedure LerXml_Evento;
    procedure LerArquivoINI_Evento;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrMDFeConstantesTests,
  ACBrMDFe.EventoClass,
  pmdfeConversaoMDFe,
  pmdfeMDFe;

{ ACBrMDFeEnvEventoTests }

procedure ACBrMDFeEnvEventoTest.SetUp;
begin
  inherited SetUp;

  FEnvEvento_New := TEventoMDFe.Create;
end;

procedure ACBrMDFeEnvEventoTest.TearDown;
begin
  FEnvEvento_New.Free;

  inherited TearDown;
end;

procedure ACBrMDFeEnvEventoTest.Gerar_InfEvento(ATipoEvento: TpcnTpEvento; codOrgao: Integer);
begin
  // Gerar o XML usando a unit nova
  FEnvEvento_New.Versao := '4.00';
  FEnvEvento_New.idLote := 1;

  Item_new := FEnvEvento_New.Evento.New;

  Item_new.InfEvento.TpAmb := taHomologacao;
  Item_new.InfEvento.CNPJCPF := '12345678000123';
  Item_new.InfEvento.cOrgao := codOrgao;
  Item_new.InfEvento.chMDFe := '12345678901234567890123456789012345678901234';
  Item_new.InfEvento.dhEvento := StrToDateTime('09/04/2024 18:14:00');
  Item_new.InfEvento.tpEvento := ATipoEvento;
  Item_new.InfEvento.nSeqEvento := 1;
  Item_new.InfEvento.versaoEvento := '4.00';
end;

procedure ACBrMDFeEnvEventoTest.GerarXml_Evento_Cancelamento;
begin
  sxml_old := sxml_EventoCancelamento;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCancelamento, 35);

  Item_new.InfEvento.detEvento.nProt := '123456';
  Item_new.InfEvento.detEvento.xJust := 'Dados Errados Informados na Nota';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrMDFeEnvEventoTest.GerarXml_Evento_Encerramento;
begin
  sxml_old := sxml_EventoEncerramento;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teEncerramento, 35);

  Item_new.InfEvento.detEvento.nProt := '123456';
  Item_new.InfEvento.detEvento.dtEnc := StrToDate('09/04/2024');
  Item_new.InfEvento.detEvento.cUF := 35;
  Item_new.InfEvento.detEvento.cMun := 3503208;
  Item_new.InfEvento.detEvento.indEncPorTerceiro := tiSim;

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrMDFeEnvEventoTest.GerarXml_Evento_InclusaoCondutor;
begin
  sxml_old := sxml_EventoInclusaoCondutor;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teInclusaoCondutor, 35);

  Item_new.InfEvento.detEvento.xNome := 'Pedro';
  Item_new.InfEvento.detEvento.CPF := '12345678901';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrMDFeEnvEventoTest.GerarXml_Evento_InclusaoDFe;
var
  Item: TInfDocCollectionItem;
begin
  sxml_old := sxml_EventoInclusaoDFe;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teInclusaoDFe, 35);

  Item_new.InfEvento.detEvento.nProt := '123456';
  Item_new.InfEvento.detEvento.cMunCarrega := 3503208;
  Item_new.InfEvento.detEvento.xMunCarrega := 'Araraquara';

  Item := Item_new.InfEvento.detEvento.infDoc.New;

  Item.cMunDescarga := 3503208;
  Item.xMunDescarga := 'Araraquara';
  Item.chNFe := '12345678901234567890123456789012345678901234';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrMDFeEnvEventoTest.GerarXml_Evento_PagamentoOperacao;
var
  Item: TinfPagCollectionItem;
  Item2: pmdfeMDFe.TCompcollectionItem;
begin
  sxml_old := sxml_EventoPagamentoOperacao;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(tePagamentoOperacao, 35);

  Item_new.InfEvento.detEvento.nProt := '123456';
  Item_new.InfEvento.detEvento.infViagens.qtdViagens := 1;
  Item_new.InfEvento.detEvento.infViagens.nroViagem := 1;

  Item := Item_new.InfEvento.detEvento.infPag.New;

  Item.xNome := 'Pedro';
  Item.CNPJCPF := '12345678901234';
  Item.vContrato := 100;
  Item.indPag := ipVista;
  Item.vAdiant := 0;
  Item.tpAntecip := taNenhum;
  Item.infBanc.PIX := '12345678901234';

  Item2 := Item.Comp.New;

  Item2.tpComp := tcValePedagio;
  Item2.vComp := 100;
  Item2.xComp := 'Comp1';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrMDFeEnvEventoTest.GerarXml_Evento_AlteracaoPagtoServMDFe;
var
  Item: TinfPagCollectionItem;
  Item2: pmdfeMDFe.TCompcollectionItem;
begin
  sxml_old := sxml_EventoAlteracaoPagtoServMDFe;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teAlteracaoPagtoServMDFe, 35);

  Item_new.InfEvento.detEvento.nProt := '123456';

  Item := Item_new.InfEvento.detEvento.infPag.New;

  Item.xNome := 'Pedro';
  Item.CNPJCPF := '12345678901234';
  Item.vContrato := 100;
  Item.indPag := ipVista;
  Item.vAdiant := 0;
  Item.tpAntecip := taNenhum;
  Item.infBanc.PIX := '12345678901234';

  Item2 := Item.Comp.New;

  Item2.tpComp := tcValePedagio;
  Item2.vComp := 100;
  Item2.xComp := 'Comp1';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrMDFeEnvEventoTest.GerarXml_Evento_ConfirmaServMDFe;
begin
  sxml_old := sxml_EventoConfirmaServMDFe;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teConfirmaServMDFe, 35);

  Item_new.InfEvento.detEvento.nProt := '123456';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrMDFeEnvEventoTest.LerXml_Evento;
begin
  FEnvEvento_New.LerXMLFromString(sxml_EventoCancelamento);

  CheckEquals('ID1101111234567890123456789012345678901234567890123401', FEnvEvento_New.Evento[0].InfEvento.id, 'Id valor incorreto');
  CheckEquals(35, FEnvEvento_New.Evento[0].InfEvento.cOrgao, 'cOrgao valor incorreto');
  CheckEquals('2', TpAmbToStr(FEnvEvento_New.Evento[0].InfEvento.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('12345678000123', FEnvEvento_New.Evento[0].InfEvento.CNPJCPF, 'CNPJCPF valor incorreto');
  CheckEquals('12345678901234567890123456789012345678901234', FEnvEvento_New.Evento[0].InfEvento.chMDFe, 'chMDFe valor incorreto');
  CheckEquals(EncodeDataHora('2024-04-09T18:14:00-03:00'), FEnvEvento_New.Evento[0].InfEvento.dhEvento, 'dhEvento valor incorreto');
  CheckEquals('110111', TpEventoToStr(FEnvEvento_New.Evento[0].InfEvento.tpEvento), 'tpEvento valor incorreto');
  CheckEquals(1, FEnvEvento_New.Evento[0].InfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
  // Leitura do grupo detEveto
  CheckEquals('Cancelamento', FEnvEvento_New.Evento[0].InfEvento.detEvento.descEvento, 'descEvento valor incorreto');
  CheckEquals('123456', FEnvEvento_New.Evento[0].InfEvento.detEvento.nProt, 'nProt valor incorreto');
  CheckEquals('Dados Errados Informados na Nota', FEnvEvento_New.Evento[0].InfEvento.detEvento.xJust, 'xJust valor incorreto');
end;

procedure ACBrMDFeEnvEventoTest.LerArquivoINI_Evento;
const
  SArquivo = 'C:\ACBr\trunk2\Testes\Recursos\MDFe\EventoCancelamento.txt';
begin
  FEnvEvento_New.LerFromIni(SArquivo);

  CheckEquals(35, FEnvEvento_New.Evento[0].InfEvento.cOrgao, 'cOrgao valor incorreto');
  CheckEquals('1', TpAmbToStr(FEnvEvento_New.Evento[0].InfEvento.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('12345678000123', FEnvEvento_New.Evento[0].InfEvento.CNPJCPF, 'CNPJCPF valor incorreto');
  CheckEquals('12345678901234567890123456789012345678901234', FEnvEvento_New.Evento[0].InfEvento.chMDFe, 'chMDFe valor incorreto');
  CheckEquals(EncodeDataHora('2024-04-09T18:14:00-03:00'), FEnvEvento_New.Evento[0].InfEvento.dhEvento, 'dhEvento valor incorreto');
  CheckEquals('110111', TpEventoToStr(FEnvEvento_New.Evento[0].InfEvento.tpEvento), 'tpEvento valor incorreto');
  CheckEquals(1, FEnvEvento_New.Evento[0].InfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
  // Leitura do grupo detEveto
  CheckEquals('123456', FEnvEvento_New.Evento[0].InfEvento.detEvento.nProt, 'nProt valor incorreto');
  CheckEquals('Dados Errados Informados na Nota', FEnvEvento_New.Evento[0].InfEvento.detEvento.xJust, 'xJust valor incorreto');
end;

initialization

  _RegisterTest('ACBrMDFeEnvEventoTests', ACBrMDFeEnvEventoTest);

end.
