unit ACBrCTeEnvEventoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrCTe.EnvEvento, // Unit nova
  pcnConversao;

type

  { ACBrCTeEnvEventoTest }

  ACBrCTeEnvEventoTest = class(TTestCase)
  private
    FEnvEvento_New: ACBrCTe.EnvEvento.TEventoCTe;
    Item_new: ACBrCTe.EnvEvento.TInfEventoCollectionItem;
    sxml_old: string;
    sxml_new: string;

    procedure Gerar_InfEvento(ATipoEvento: TpcnTpEvento; codOrgao: Integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarXml_Evento_CCe;
    procedure GerarXml_Evento_Cancelamento;
    procedure GerarXml_Evento_EPEC;
    procedure GerarXml_Evento_MultModal;
    procedure GerarXml_Evento_PrestacaoDesacordo;
    procedure GerarXml_Evento_CancPrestacaoDesacordo;
    procedure GerarXml_Evento_GTV;
    procedure GerarXml_Evento_ComprEntrega;
    procedure GerarXml_Evento_CancComprEntrega;
    procedure GerarXml_Evento_InsucessoEntrega;
    procedure GerarXml_Evento_CancInsucessoEntrega;

    procedure LerXml_Evento;
    procedure LerArquivoINI_Evento;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrCTeConstantesTests,
  pcteEventoCTe,
  ACBrCTe.EventoClass,
  pcteConversaoCTe;

{ ACBrCTeEnvEventoTests }

procedure ACBrCTeEnvEventoTest.SetUp;
begin
  inherited SetUp;

  FEnvEvento_New := ACBrCTe.EnvEvento.TEventoCTe.Create;
end;

procedure ACBrCTeEnvEventoTest.TearDown;
begin
  FEnvEvento_New.Free;

  inherited TearDown;
end;

procedure ACBrCTeEnvEventoTest.Gerar_InfEvento(ATipoEvento: TpcnTpEvento; codOrgao: Integer);
begin
  // Gerar o XML usando a unit nova
  FEnvEvento_New.Versao := '4.00';
  FEnvEvento_New.idLote := 1;

  Item_new := FEnvEvento_New.Evento.New;

  Item_new.InfEvento.TpAmb := taHomologacao;
  Item_new.InfEvento.CNPJ := '12345678000123';
  Item_new.InfEvento.cOrgao := codOrgao;
  Item_new.InfEvento.chCTe := '12345678901234567890123456789012345678901234';
  Item_new.InfEvento.dhEvento := StrToDateTime('09/04/2024 18:14:00');
  Item_new.InfEvento.tpEvento := ATipoEvento;
  Item_new.InfEvento.nSeqEvento := 1;
  Item_new.InfEvento.versaoEvento := '4.00';
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_CCe;
var
  Item: TInfCorrecaoCollectionItem;
begin
  sxml_old := sxml_EventoCCe;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCCe, 35);

  Item := Item_new.InfEvento.detEvento.infCorrecao.New;

  Item.grupoAlterado := 'grupo';
  Item.campoAlterado := 'campo';
  Item.valorAlterado := 'valor';
  Item.nroItemAlterado := 1;

  Item_new.InfEvento.detEvento.xCondUso := '';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_Cancelamento;
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

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_EPEC;
begin
  sxml_old := sxml_EventoEPEC;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teEPEC, 35);
  Item_new.InfEvento.detEvento.xJust := 'Produto diferente do pedido';
  Item_new.InfEvento.detEvento.vICMS := 10;
  Item_new.InfEvento.detEvento.vICMSST := 10;
  Item_new.InfEvento.detEvento.vTPrest := 10;
  Item_new.InfEvento.detEvento.vCarga := 10;
  Item_new.InfEvento.detEvento.toma := tmRemetente;
  Item_new.InfEvento.detEvento.UF := 'SP';
  Item_new.InfEvento.detEvento.CNPJCPF := '12345678000123';
  Item_new.InfEvento.detEvento.IE := '12345';
  Item_new.InfEvento.detEvento.modal := mdRodoviario;
  Item_new.InfEvento.detEvento.UFIni := 'SP';
  Item_new.InfEvento.detEvento.UFFim := 'SP';
  Item_new.InfEvento.detEvento.dhEmi := StrToDateTime('09/04/2024 18:14:00');

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_MultModal;
begin
  sxml_old := sxml_EventoMultModal;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teMultiModal, 35);
  Item_new.InfEvento.detEvento.xRegistro := 'Registro';
  Item_new.InfEvento.detEvento.nDoc := '10';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_PrestacaoDesacordo;
begin
  sxml_old := sxml_EventoPrestacaoDesacordo;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(tePrestDesacordo, 35);
  Item_new.InfEvento.detEvento.xOBS := 'motivo do desacordo';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_CancPrestacaoDesacordo;
begin
  sxml_old := sxml_EventoCancPrestacaoDesacordo;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCancPrestDesacordo, 35);
  Item_new.InfEvento.detEvento.nProt := '12345';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_GTV;
var
  Item: TInfGTVCollectionItem;
  ItemEspecie: TInfEspecieCollectionItem;
begin
  sxml_old := sxml_EventoGTV;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teGTV, 35);

  Item := Item_new.InfEvento.detEvento.infGTV.New;

  Item.nDoc := '123';
  Item.id := '12345678901234567890';
  Item.serie := '1';
  Item.subserie := '0';
  Item.dEmi := StrToDateTime('09/04/2024');
  Item.nDV := 0;
  Item.qCarga := 10;

  ItemEspecie := Item.infEspecie.New;

  ItemEspecie.tpEspecie := teNumerario;
  ItemEspecie.vEspecie := 100;

  Item.rem.CNPJCPF := '12345678000123';
  Item.rem.IE := '12345678';
  Item.rem.UF := 'SP';
  Item.rem.xNome := 'Remetente';

  Item.dest.CNPJCPF := '12345678000123';
  Item.dest.IE := '12345678';
  Item.dest.UF := 'SP';
  Item.dest.xNome := 'Destinatario';

  Item.placa := 'ABC1234';
  Item.UF := 'SP';
  Item.RNTRC := '123456';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_ComprEntrega;
var
  Item: TInfEntregaCollectionItem;
begin
  sxml_old := sxml_EventoComprEntrega;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teComprEntrega, 35);
  Item_new.InfEvento.detEvento.nProt := '12345';
  Item_new.InfEvento.detEvento.dhEntrega := StrToDateTime('09/04/2024 18:14:00');
  Item_new.InfEvento.detEvento.nDoc := '123';
  Item_new.InfEvento.detEvento.xNome := 'Nome do Cliente';
  Item_new.InfEvento.detEvento.latitude := 5;
  Item_new.InfEvento.detEvento.longitude := 5;
  Item_new.InfEvento.detEvento.hashEntrega := '123456';
  Item_new.InfEvento.detEvento.dhHashEntrega := StrToDateTime('09/04/2024 18:14:00');

  Item := Item_new.InfEvento.detEvento.infEntrega.New;

  Item.chNFe := '12345678901234567890123456789012345678901234';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_CancComprEntrega;
begin
  sxml_old := sxml_EventoCancComprEntrega;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCancComprEntrega, 35);
  Item_new.InfEvento.detEvento.nProt := '123456';
  Item_new.InfEvento.detEvento.nProtCE := '123456';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_InsucessoEntrega;
var
  Item: TInfEntregaCollectionItem;
begin
  sxml_old := sxml_EventoInsucessoEntrega;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teInsucessoEntregaCTe, 92);
  Item_new.InfEvento.detEvento.nProt := '12345';
  Item_new.InfEvento.detEvento.dhTentativaEntrega := StrToDateTime('09/04/2024 18:14:00');
  Item_new.InfEvento.detEvento.nTentativa := 1;
  Item_new.InfEvento.detEvento.tpMotivo := tmOutro;
  Item_new.InfEvento.detEvento.xJustMotivo := 'Destinatario mudou';
  Item_new.InfEvento.detEvento.latitude := 5;
  Item_new.InfEvento.detEvento.longitude := 5;
  Item_new.InfEvento.detEvento.hashTentativaEntrega := '123456';
  Item_new.InfEvento.detEvento.dhHashTentativaEntrega := StrToDateTime('09/04/2024 18:14:00');

  Item := Item_new.InfEvento.detEvento.infEntrega.New;

  Item.chNFe := '12345678901234567890123456789012345678901234';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.GerarXml_Evento_CancInsucessoEntrega;
begin
  sxml_old := sxml_EventoCancInsucessoEntrega;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCancInsucessoEntregaCTe, 92);
  Item_new.infEvento.detEvento.nProt := '123456789012345';
  Item_new.infEvento.detEvento.nProtIE := '123456789012345';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_old, sxml_new, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrCTeEnvEventoTest.LerXml_Evento;
var
  i: Integer;
begin
  FEnvEvento_New.LerXMLFromString(sxml_EventoCCe);

  CheckEquals('ID11011012345678901234567890123456789012345678901234001', FEnvEvento_New.Evento[0].InfEvento.id, 'Id valor incorreto');
  CheckEquals(35, FEnvEvento_New.Evento[0].InfEvento.cOrgao, 'cOrgao valor incorreto');
  CheckEquals('2', TpAmbToStr(FEnvEvento_New.Evento[0].InfEvento.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('12345678000123', FEnvEvento_New.Evento[0].InfEvento.CNPJ, 'CNPJ valor incorreto');
  CheckEquals('12345678901234567890123456789012345678901234', FEnvEvento_New.Evento[0].InfEvento.chCTe, 'chCTe valor incorreto');
  CheckEquals(EncodeDataHora('2024-04-09T18:14:00-03:00'), FEnvEvento_New.Evento[0].InfEvento.dhEvento, 'dhEvento valor incorreto');
  CheckEquals('110110', TpEventoToStr(FEnvEvento_New.Evento[0].InfEvento.tpEvento), 'tpEvento valor incorreto');
  CheckEquals(1, FEnvEvento_New.Evento[0].InfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
  CheckEquals('4.00', FEnvEvento_New.Evento[0].InfEvento.versaoEvento, 'verEvento valor incorreto');
  // Leitura do grupo detEveto
  CheckEquals('Carta de Correcao', FEnvEvento_New.Evento[0].InfEvento.detEvento.descEvento, 'descEvento valor incorreto');

  for I := 0 to FEnvEvento_New.Evento[0].InfEvento.detEvento.infCorrecao.Count -1 do
  begin
    CheckEquals('grupo', FEnvEvento_New.Evento[0].InfEvento.detEvento.infCorrecao[i].grupoAlterado, 'grupoAlterado valor incorreto');
    CheckEquals('campo', FEnvEvento_New.Evento[0].InfEvento.detEvento.infCorrecao[i].campoAlterado, 'campoAlterado valor incorreto');
    CheckEquals('valor', FEnvEvento_New.Evento[0].InfEvento.detEvento.infCorrecao[i].valorAlterado, 'valorAlterado valor incorreto');
    CheckEquals(1, FEnvEvento_New.Evento[0].InfEvento.detEvento.infCorrecao[i].nroItemAlterado, 'nroItemAlterado valor incorreto');
  end;
end;

procedure ACBrCTeEnvEventoTest.LerArquivoINI_Evento;
const
//  SArquivo = '..\..\..\Recursos\CTe\EventoComprovanteEntrega.txt';
  SArquivo = 'C:\ACBr\trunk2\Testes\Recursos\CTe\EventoComprovanteEntrega.txt';
var
  i: Integer;
begin
  FEnvEvento_New.LerFromIni(SArquivo, False);

  CheckEquals(35, FEnvEvento_New.Evento[0].InfEvento.cOrgao, 'cOrgao valor incorreto');
  CheckEquals('12345678000123', FEnvEvento_New.Evento[0].InfEvento.CNPJ, 'CNPJ valor incorreto');
  CheckEquals('12345678901234567890123456789012345678901234', FEnvEvento_New.Evento[0].InfEvento.chCTe, 'chCTe valor incorreto');
  CheckEquals(EncodeDataHora('2024-04-09T18:14:00-03:00'), FEnvEvento_New.Evento[0].InfEvento.dhEvento, 'dhEvento valor incorreto');
  CheckEquals('110180', TpEventoToStr(FEnvEvento_New.Evento[0].InfEvento.tpEvento), 'tpEvento valor incorreto');
  CheckEquals(1, FEnvEvento_New.Evento[0].InfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
  CheckEquals('123456789', FEnvEvento_New.Evento[0].InfEvento.detEvento.nProt, 'nProt valor incorreto');
  CheckEquals('SP', FEnvEvento_New.Evento[0].InfEvento.detEvento.UF, 'UF valor incorreto');
  CheckEquals(EncodeDataHora('2024-04-09T18:14:00'), FEnvEvento_New.Evento[0].InfEvento.detEvento.dhEntrega, 'dhEntrega valor incorreto');
  CheckEquals('12345678', FEnvEvento_New.Evento[0].InfEvento.detEvento.nDoc, 'nDoc valor incorreto');
  CheckEquals('Pedro', FEnvEvento_New.Evento[0].InfEvento.detEvento.xNome, 'xNome valor incorreto');
  CheckEquals(5, FEnvEvento_New.Evento[0].InfEvento.detEvento.latitude, 'latitude valor incorreto');
  CheckEquals(5, FEnvEvento_New.Evento[0].InfEvento.detEvento.longitude, 'longitude valor incorreto');
  CheckEquals('0uJObQk29JacPTFTlMtooavXdpM=', FEnvEvento_New.Evento[0].InfEvento.detEvento.hashEntrega, 'hashEntrega valor incorreto');
  CheckEquals(EncodeDataHora('2024-04-09T18:14:00'), FEnvEvento_New.Evento[0].InfEvento.detEvento.dhHashEntrega, 'dhHashEntrega valor incorreto');

  // Leitura do grupo infEntrega
  for i := 0 to FEnvEvento_New.Evento[0].InfEvento.detEvento.infEntrega.Count -1 do
    CheckEquals('12345678901234567890123456789012345678901234',
      FEnvEvento_New.Evento[0].InfEvento.detEvento.infEntrega[i].chNFe, 'chNFe valor incorreto');
end;

initialization

  _RegisterTest('ACBrCTeEnvEventoTests', ACBrCTeEnvEventoTest);

end.
