unit ACBrLibXml2UTF8TestClass;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrXmlDocument, ACBrXmlReader, ACBrXmlBase, ACBrLibXml2,
  synautil, ACBrUtil.Strings, ACBrLibXml2TestUTF8Consts;

type

  { ACBrLibXml2Test }

  ACBrLibXml2UTF8Test= class(TTestCase)
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
    procedure LoadFromFileConteudoComAcento;
    procedure LoadFromXMLConteudoCaracteresEscapados;
    procedure LoadFromXMLCDATA;
    procedure LoadFromFileCDATA;
  end;

implementation

const
  cArquivoXmlValidoUtf8_CaracteresEscapados = '..\..\Recursos\Xml\11_XmlUTF8_ComCaracteresEscapados.xml';
  cArquivoXmlUTF8_TagAcentuada = '..\..\Recursos\Xml\13_XmlUTF8_TagAcentuada.xml';
  cArquivoXmlUTF8_ConteudoAcentuado = '..\..\Recursos\Xml\15_XmlUTF8_ConteudoAcentuado.xml';
  cArquivoXmlUTF8_TagCaracteresEscapados = '..\..\Recursos\Xml\17_XmlUTF8_TagCaracteresEscapados.xml';
  cArquivoXmlUTF8_CDATA = '..\..\Recursos\Xml\19_XmlUTF8_CDATA.xml';

procedure ACBrLibXml2UTF8Test.SetUp;
begin
  inherited SetUp;
  FXmlReader := TACBrXmlReader.Create;
end;

procedure ACBrLibXml2UTF8Test.TearDown;
begin
  inherited TearDown;
  FXmlReader.Free;
end;

procedure ACBrLibXml2UTF8Test.ConverteCaracteresEscape;
var
  FDocument: TACBrXmlDocument;
  XmlNode: TACBrXmlNode;
  strXml: AnsiString;
begin
  FDocument := TACBrXmlDocument.Create;
  try
    try
      FDocument.LoadFromFile(cArquivoXmlValidoUtf8_CaracteresEscapados);
      XmlNode := FDocument.Root;
      CheckTrue(Assigned(XmlNode), 'Não encontrou a tag no XML');
      strXml := FXmlReader.ObterConteudo(XmlNode, tcStr);
      Check(strXml = UTF8_STRCOMP, 'Não converteu corretamente a informação da tag em UTF8' + sLineBreak
                              + 'Esperado: ' + UTF8_STRCOMP + sLineBreak
                              + 'Convertido: ' + strXml);

    except
      on E:Exception do
         Fail(E.Message);
    end;

  finally
    FDocument.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromXMLTagsComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(UTF8_TAGS_ACENTUADAS);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML UTF8:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromFileTagsComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlUTF8_TagAcentuada);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo: '+ cArquivoXmlUTF8_TagAcentuada + ' |Erro: ' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromXMLTagsCaracteresEscapados;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(UTF8_TAGS_ESCAPADOS);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML UTF8:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromFileTagsCaracteresEscapados;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlUTF8_TagCaracteresEscapados);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'teste', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo: '+ cArquivoXmlUTF8_TagCaracteresEscapados + ' |Erro: ' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromXMLConteudoComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(UTF8_CONTEUDO_ACENTUADO);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'seção', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML UTF8:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromFileConteudoComAcento;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlUTF8_ConteudoAcentuado);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'seção', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo: '+ cArquivoXmlUTF8_ConteudoAcentuado + ' |Erro: ' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromXMLConteudoCaracteresEscapados;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(UTF8_CONTEUDO_ESCAPADO);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Root.Content = 'seção', 'TACBrXMLDocument.Root.Content diferente do esperado');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML UTF8:'+ E.Message);
    end;

  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromXMLCDATA;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.Clear;
      XmlDoc.LoadFromXml(UTF8_CDATA);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');
      Check(XmlDoc.Root.Childrens.Count <> 0, 'TACBrXmlDocument.Root.Childrens permaneceu vazio');
      Check(Assigned(XmlDoc.Root.Childrens.FindAnyNs('secao')), 'Não encontrei o conteúdo dentro de CDATA');

    except
      on E:Exception do
         Fail('Falhei ao ler o XML UTF8:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure ACBrLibXml2UTF8Test.LoadFromFileCDATA;
var
  XmlDoc: TACBrXmlDocument;
begin
  XmlDoc := TACBrXmlDocument.Create;
  try
    try
      XmlDoc.LoadFromFile(cArquivoXmlUTF8_CDATA);

      Check(Assigned(XmlDoc.Root), 'TACBrXMLDocument.Root permaneceu nil');
      Check(XmlDoc.Xml <> '', 'TACBrXMLDocument.Xml está vazio');
      Check(XmlDoc.Root.Childrens.Count <> 0, 'TACBrXmlDocument.Root.Childrens permaneceu vazio');
      Check(Assigned(XmlDoc.Root.Childrens.FindAnyNs('seção')), 'Não encontrou a tag de dentro do CDATA');

    except
      on E:Exception do
         Fail('Falhei ao ler o arquivo:'+ cArquivoXmlUTF8_CDATA + '|Erro:' + E.Message);
    end;
  finally
    XmlDoc.Free;
  end;
end;



initialization

  _RegisterTest('ACBrDFe.ACBrLibXml2', ACBrLibXml2UTF8Test);
end.

