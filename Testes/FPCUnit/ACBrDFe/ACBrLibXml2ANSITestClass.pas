unit ACBrLibXml2ANSITestClass;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrXmlDocument, ACBrXmlReader, ACBrXmlBase, ACBrLibXml2,
  synautil, ACBrUtil.Strings, ACBrLibXml2TestAnsiConsts;

type

  { ACBrLibXml2Test }

  { ACBrLibXml2ANSITest }

  ACBrLibXml2ANSITest= class(TTestCase)
  private
    FXmlReader: TACBrXmlReader;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ConverteCaracteresEscape;
    procedure LoadFromXMLTagsComAcento;
    procedure LoadFromFileTagsComAcento;
    procedure LoadFromXMLTagsCaracteresEscapados;
    procedure LoadFromFileTagsCaracteresEscapados;
    procedure LoadFromXMLConteudoComAcento;
    procedure LoadFromXMLConteudoComAcentoUsandoACBrStr;
    procedure LoadFromFileConteudoComAcento;
    procedure LoadFromXMLConteudoCaracteresEscapados;
    procedure LoadFromXMLCDATA;
    procedure LoadFromFileCDATA;
  end;

implementation

const
  cArquivoXmlValidoAnsi_CaracteresEscapados = '..\..\Recursos\Xml\12_XmlAnsi_ComCaracteresEscapados.xml';
  cArquivoXmlAnsi_TagAcentuada = '..\..\Recursos\Xml\14_XmlAnsi_TagAcentuada.xml';
  cArquivoXmlAnsi_ConteudoAcentuado = '..\..\Recursos\Xml\16_XmlAnsi_ConteudoAcentuado.xml';
  cArquivoXmlAnsi_TagCaracteresEscapados = '..\..\Recursos\Xml\18_XmlAnsi_TagCaracteresEscapados.xml';
  cArquivoXmlAnsi_CDATA = '..\..\Recursos\Xml\20_XmlAnsi_CDATA.xml';

procedure ACBrLibXml2ANSITest.SetUp;
begin
  inherited SetUp;
  FXmlReader := TACBrXmlReader.Create;
end;

procedure ACBrLibXml2ANSITest.TearDown;
begin
  inherited TearDown;
  FXmlReader.Free;
end;

procedure ACBrLibXml2ANSITest.ConverteCaracteresEscape;
var
  FDocument: TACBrXmlDocument;
  XmlNode: TACBrXmlNode;
  strXml: AnsiString;
begin
  FDocument := TACBrXmlDocument.Create;
  try
    try
      FDocument.LoadFromFile(cArquivoXmlValidoAnsi_CaracteresEscapados);
      XmlNode := FDocument.Root;
      CheckTrue(Assigned(XmlNode), 'Não encontrou a tag no XML');
      strXml := FXmlReader.ObterConteudo(XmlNode, tcStr);
      Check(strXml = ANSI_STRCOMP, 'Não converteu corretamente a informação da tag em ANSI' + sLineBreak
                              + 'Esperado: ' + ANSI_STRCOMP + sLineBreak
                              + 'Convertido: ' + strXml);

    except
      on E:Exception do
         Fail(E.Message);
    end;
  finally
    FDocument.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromXMLTagsComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(ANSI_TAGS_ACENTUADAS);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML ANSI:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromFileTagsComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlAnsi_TagAcentuada);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo: '+ cArquivoXmlAnsi_TagAcentuada + ' |Erro: ' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromXMLTagsCaracteresEscapados;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(ANSI_TAGS_ESCAPADOS);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML ANSI:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromFileTagsCaracteresEscapados;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlAnsi_TagCaracteresEscapados);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo: '+ cArquivoXmlAnsi_TagCaracteresEscapados + ' |Erro: ' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromXMLConteudoComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(ANSI_CONTEUDO_ACENTUADO);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'seção', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML ANSI:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromXMLConteudoComAcentoUsandoACBrStr;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(ACBrStr(ANSI_CONTEUDO_ACENTUADO));

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'seção', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML ANSI:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromFileConteudoComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlAnsi_ConteudoAcentuado);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'seção', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo: '+ cArquivoXmlAnsi_ConteudoAcentuado + ' |Erro: ' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromXMLConteudoCaracteresEscapados;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(ANSI_CONTEUDO_ESCAPADO);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'seção', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML ANSI: '+ E.Message);
    end;

  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromXMLCDATA;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(ANSI_CDATA);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');
      Check(XmlDoc.Root.Childrens.Count <> 0, 'TACBrXmlDocument.Root.Childrens permaneceu vazio');
      Check(Assigned(XmlDoc.Root.Childrens.FindAnyNs('secao')), 'Não encontrei o conteúdo dentro de CDATA');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML ANSI:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2ANSITest.LoadFromFileCDATA;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlAnsi_CDATA);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');
      Check(XmlDoc.Root.Childrens.Count <> 0, 'TACBrXmlDocument.Root.Childrens permaneceu vazio');
      Check(Assigned(XmlDoc.Root.Childrens.FindAnyNs('seção')), 'Não encontrou a tag de dentro do CDATA');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo:'+ cArquivoXmlAnsi_CDATA + '|Erro:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;



initialization

  _RegisterTest('ACBrDFe.ACBrLibXml2', ACBrLibXml2ANSITest);
end.

