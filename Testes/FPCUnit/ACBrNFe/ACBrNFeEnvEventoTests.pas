unit ACBrNFeEnvEventoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFe.EnvEvento, // Unit nova
  pcnConversao;

type

  { ACBrNFeEnvEventoTest }

  ACBrNFeEnvEventoTest = class(TTestCase)
  private
    FEnvEvento_New: TEventoNFe;
    Item_new: TInfEventoCollectionItem;
    sxml_old: string;
    sxml_new: string;

    procedure Gerar_InfEvento(ATipoEvento: TpcnTpEvento);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarXml_Evento_CCe;
    procedure GerarXml_Evento_Cancelamento;
    procedure GerarXml_Evento_CancSubstituicao;
    procedure GerarXml_Evento_ManifDestConfirmacao;
    procedure GerarXml_Evento_ManifDestCiencia;
    procedure GerarXml_Evento_ManifDesconhecimento;
    procedure GerarXml_Evento_ManifNaoRealizada;
    procedure GerarXml_Evento_EPEC;
    procedure GerarXml_Evento_PedProrrogacao;
    procedure GerarXml_Evento_CancPedProrrogacao;
    procedure GerarXml_Evento_ComprEntrega;
    procedure GerarXml_Evento_CancComprEntrega;
    procedure GerarXml_Evento_AtorInteressado;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrNFeConstantesTests,
  pcnEventoNFe,
  ACBrNFe.EventoClass,
  pcnConversaoNFe;

{ ACBrNFeEnvEventoTests }

procedure ACBrNFeEnvEventoTest.SetUp;
begin
  inherited SetUp;

  FEnvEvento_New := TEventoNFe.Create;
end;

procedure ACBrNFeEnvEventoTest.TearDown;
begin
  FEnvEvento_New.Free;

  inherited TearDown;
end;

procedure ACBrNFeEnvEventoTest.Gerar_InfEvento(ATipoEvento: TpcnTpEvento);
begin
  // Gerar o XML usando a unit nova
  FEnvEvento_New.Versao := '4.00';
  FEnvEvento_New.idLote := 1;

  Item_new := FEnvEvento_New.Evento.New;

  Item_new.InfEvento.TpAmb := taHomologacao;
  Item_new.InfEvento.CNPJ := '12345678000123';
  Item_new.InfEvento.cOrgao := 35;
  Item_new.InfEvento.chNFe := '12345678901234567890123456789012345678901234';
  Item_new.InfEvento.dhEvento := StrToDateTime('09/04/2024 18:14:00');
  Item_new.InfEvento.tpEvento := ATipoEvento;
  Item_new.InfEvento.nSeqEvento := 1;
  Item_new.InfEvento.versaoEvento := '4.00';
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_CCe;
begin
  sxml_old := sxml_EventoCCe;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCCe);
  Item_new.InfEvento.detEvento.xCorrecao := 'Descricao do produto errada';
  Item_new.InfEvento.detEvento.xCondUso := '';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_Cancelamento;
begin
  sxml_old := sxml_EventoCancelamento;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCancelamento);
  Item_new.InfEvento.detEvento.nProt := '123456';
  Item_new.InfEvento.detEvento.xJust := 'Dados Errados Informados na Nota';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_CancSubstituicao;
begin
  sxml_old := sxml_EventoCancSubst;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCancSubst);
  Item_new.InfEvento.detEvento.cOrgaoAutor := 35;
  Item_new.InfEvento.detEvento.tpAutor := taEmpresaEmitente;
  Item_new.InfEvento.detEvento.verAplic := '1.00';
  Item_new.InfEvento.detEvento.nProt := '123456';
  Item_new.InfEvento.detEvento.xJust := 'Dados Errados Informados na Nota';
  Item_new.InfEvento.detEvento.chNFeRef := '12345678901234567890123456789012345678901234';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_ManifDestConfirmacao;
begin
  sxml_old := sxml_EventoManifDestConf;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teManifDestConfirmacao);

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_ManifDestCiencia;
begin
  sxml_old := sxml_EventoManifDestCiencia;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teManifDestCiencia);

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_ManifDesconhecimento;
begin
  sxml_old := sxml_EventoManifDesconhecimento;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teManifDestDesconhecimento);

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_ManifNaoRealizada;
begin
  sxml_old := sxml_EventoManiNaoRealizada;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teManifDestOperNaoRealizada);
  Item_new.InfEvento.detEvento.xJust := 'Produto diferente do pedido';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_EPEC;
begin
  sxml_old := sxml_EventoEPEC;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teEPECNFe);
  Item_new.InfEvento.detEvento.cOrgaoAutor := 35;
  Item_new.InfEvento.detEvento.tpAutor := taEmpresaEmitente;
  Item_new.InfEvento.detEvento.verAplic := '1.00';
  Item_new.InfEvento.detEvento.xJust := 'Produto diferente do pedido';
  Item_new.InfEvento.detEvento.dhEmi := StrToDateTime('09/04/2024 18:14:00');
  Item_new.InfEvento.detEvento.tpNF := tnSaida;
  Item_new.InfEvento.detEvento.IE := '12345';

  Item_new.InfEvento.detEvento.vNF := 10;
  Item_new.InfEvento.detEvento.vICMS := 10;
  Item_new.InfEvento.detEvento.vST := 10;

  Item_new.InfEvento.detEvento.dest.UF := 'SP';
  Item_new.InfEvento.detEvento.dest.CNPJCPF := '12345678000123';
  Item_new.InfEvento.detEvento.dest.idEstrangeiro := '';
  Item_new.InfEvento.detEvento.dest.IE := '12345';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_PedProrrogacao;
var
  Item: TitemPedidoCollectionItem;
begin
  sxml_old := sxml_EventoPedProrrog;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(tePedProrrog1);
  Item_new.InfEvento.detEvento.nProt := '123456';

  Item := Item_new.InfEvento.detEvento.itemPedido.New;

  Item.numItem := 1;
  Item.qtdeItem := 10;

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_CancPedProrrogacao;
begin
  sxml_old := sxml_EventoCanPedProrrog1;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCanPedProrrog1);
  Item_new.InfEvento.detEvento.idPedidoCancelado := '123456';
  Item_new.InfEvento.detEvento.nProt := '123456';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_ComprEntrega;
begin
  sxml_old := sxml_EventoComprEntrega;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teComprEntregaNFe);
  Item_new.InfEvento.detEvento.cOrgaoAutor := 35;
  Item_new.InfEvento.detEvento.tpAutor := taEmpresaEmitente;
  Item_new.InfEvento.detEvento.verAplic := '1.00';
  Item_new.InfEvento.detEvento.dhEntrega := StrToDateTime('09/04/2024 18:14:00');
  Item_new.InfEvento.detEvento.nDoc := '123';
  Item_new.InfEvento.detEvento.xNome := 'Nome do Cliente';
  Item_new.InfEvento.detEvento.latGPS := 10;
  Item_new.InfEvento.detEvento.longGPS := 20;
  Item_new.InfEvento.detEvento.hashComprovante := '123456';
  Item_new.InfEvento.detEvento.dhHashComprovante := StrToDateTime('09/04/2024 18:14:00');

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_CancComprEntrega;
begin
  sxml_old := sxml_EventoCancComprEntrega;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teCancComprEntregaNFe);
  Item_new.InfEvento.detEvento.cOrgaoAutor := 35;
  Item_new.InfEvento.detEvento.tpAutor := taEmpresaEmitente;
  Item_new.InfEvento.detEvento.verAplic := '1.00';
  Item_new.InfEvento.detEvento.nProtEvento := '123456';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

procedure ACBrNFeEnvEventoTest.GerarXml_Evento_AtorInteressado;
var
  Item: TautXMLCollectionItem;
begin
  sxml_old := sxml_EventoAtorInteressado;

  // Gerar o XML usando a unit nova
  Gerar_InfEvento(teAtorInteressadoNFe);
  Item_new.InfEvento.detEvento.cOrgaoAutor := 35;
  Item_new.InfEvento.detEvento.tpAutor := taEmpresaEmitente;
  Item_new.InfEvento.detEvento.verAplic := '1.00';

  Item := Item_new.InfEvento.detEvento.autXML.New;

  Item.CNPJCPF := '12345678000123';

  Item_new.InfEvento.detEvento.tpAutorizacao := taPermite;
  Item_new.InfEvento.detEvento.xCondUso := 'Autorizado o acesso';

  FEnvEvento_New.GerarXML;
  sxml_new := FEnvEvento_New.XmlEnvio;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de EnvEvento diferente do antigo');
end;

initialization

  _RegisterTest('ACBrNFeEnvEventoTests', ACBrNFeEnvEventoTest);

end.
