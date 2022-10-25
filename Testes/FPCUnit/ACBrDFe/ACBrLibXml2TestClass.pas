unit ACBrLibXml2TestClass;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrXmlDocument, ACBrLibXml2, synautil, ACBrUtil.Strings;

type

  { ACBrLibXml2Test }

  ACBrLibXml2Test= class(TTestCase)
  private
    function XMLFoiDetectadoViaArquivo(const NomeArquivo: string): Boolean;
    function XMLFoiDetectadoViaString(const NomeArquivo: string): Boolean;
  published
    procedure CarregaXmlsViaString;
    procedure CarregaXmlsViaNomeArquivo;
    procedure DetectaTagAcentuada;
  end;

implementation

const
  cArquivoXmlNfsex_TagAcentuada = '..\..\Recursos\NFSe\01_XmlNfsex_TagAcentuada-NFSe.xml';
  cArquivoXmlNfsexSoap_TagAcentuada = '..\..\Recursos\NFSe\02_XmlNfsexSoap_TagAcentuada-NFSe.xml';
  cArquivoXmlUtf8_SomenteCaracteresAnsi ='..\..\Recursos\Xml\01_XmlUtf8_ValidoSomenteCaracteresANSI.xml';
  cArquivoXmlUtf8Wikipedia_ValidoCaracteresChinesesArmeniosLatinosEtc = '..\..\Recursos\Xml\02_XmlUtf8_ValidoWikipediaCaracteresChinesesArmeniosLatinosEtc.xml';
  cArquivoXmlValidoUtf8_TagsComCaracteresChinesesArmeniosLatinosEtc = '..\..\Recursos\Xml\03_XmlUtf8_ValidoTagsComCaracteresChinesesArmeniosLatinosEtc.xml';
  cSubRaizXmlNFse = 'Nfse';
  cTagInfNFSe = 'InfNfse';
  cTagAcentuada = 'DescricaoCodigoTributacaoMunicípio';



function ACBrLibXml2Test.XMLFoiDetectadoViaString(const NomeArquivo: string): Boolean;
var
  FDocument: TACBrXmlDocument;
  XmlNode: TACBrXmlNode;
  XMLStr: String;
  XMLUTF8: AnsiString;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(NomeArquivo);
    XMLUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  // Converte de UTF8 para a String nativa da IDE //
  XMLStr := DecodeToString(XMLUTF8, True);
  FDocument := TACBrXmlDocument.Create();
  try
    FDocument.Clear();
    FDocument.LoadFromXml(XMLStr);
    //Possível Correção no Delphi
//    FDocument.LoadFromXml(NativeStringToUTF8(XMLStr));

    XmlNode := FDocument.Root;
    Result := Assigned(XmlNode);
  finally
    FDocument.Free;
  end;

end;

function ACBrLibXml2Test.XMLFoiDetectadoViaArquivo(const NomeArquivo: string): Boolean;
var
  FDocument: TACBrXmlDocument;
  XmlNode: TACBrXmlNode;
begin
  // Converte de UTF8 para a String nativa da IDE //
  FDocument := TACBrXmlDocument.Create();
  try
    FDocument.Clear();
    FDocument.LoadFromFile(NomeArquivo);

    XmlNode := FDocument.Root;
    Result := Assigned(XmlNode);
  finally
    FDocument.Free;
  end;
end;


procedure ACBrLibXml2Test.CarregaXmlsViaString;
var
  NomeArquivo: string;
begin
  NomeArquivo :=   cArquivoXmlNfsex_TagAcentuada;
  CheckTrue(XMLFoiDetectadoViaString(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlNfsexSoap_TagAcentuada;
  CheckTrue(XMLFoiDetectadoViaString(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlUtf8_SomenteCaracteresAnsi;
  CheckTrue(XMLFoiDetectadoViaString(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlUtf8Wikipedia_ValidoCaracteresChinesesArmeniosLatinosEtc;
  CheckTrue(XMLFoiDetectadoViaString(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlValidoUtf8_TagsComCaracteresChinesesArmeniosLatinosEtc;
  CheckTrue(XMLFoiDetectadoViaString(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));
end;

procedure ACBrLibXml2Test.CarregaXmlsViaNomeArquivo;
var
  NomeArquivo: string;
begin
  NomeArquivo :=   cArquivoXmlNfsex_TagAcentuada;
  CheckTrue(XMLFoiDetectadoViaArquivo(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlNfsexSoap_TagAcentuada;
  CheckTrue(XMLFoiDetectadoViaArquivo(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlUtf8_SomenteCaracteresAnsi;
  CheckTrue(XMLFoiDetectadoViaArquivo(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlUtf8Wikipedia_ValidoCaracteresChinesesArmeniosLatinosEtc;
  CheckTrue(XMLFoiDetectadoViaArquivo(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));

  NomeArquivo := cArquivoXmlValidoUtf8_TagsComCaracteresChinesesArmeniosLatinosEtc;
  CheckTrue(XMLFoiDetectadoViaArquivo(NomeArquivo) , 'Arquivo xml incorretamente detectado como vazio ou não carregado: '+ ExtractFileName(NomeArquivo));
end;

procedure ACBrLibXml2Test.DetectaTagAcentuada;
var
  FDocument: TACBrXmlDocument;
  XmlNode: TACBrXmlNode;
begin
  // Converte de UTF8 para a String nativa da IDE //
  FDocument := TACBrXmlDocument.Create();
  try
    FDocument.Clear();
    FDocument.LoadFromFile(cArquivoXmlNfsex_TagAcentuada);

    XmlNode := FDocument.Root.Childrens.Find(cSubRaizXmlNFse);
    If not Assigned(XmlNode) then Fail('Possível erro no teste na detecção do XML: ' + cSubRaizXmlNFse);
    XmlNode := XmlNode.Childrens.Find(cTagInfNFSe);
    If not Assigned(XmlNode) then Fail('Possível erro no teste na detecção do XML: ' + cTagInfNFSe);

    //Possível Correção no Delphi
//    XmlNode := XmlNode.Childrens.Find(NativeStringToUTF8(cTagAcentuada));
    XmlNode := XmlNode.Childrens.Find(cTagAcentuada);

    CheckTrue(Assigned(XmlNode));
  finally
    FDocument.Free;
  end;


end;



initialization

  _RegisterTest('ACBrDFe.ACBrLibXml2', ACBrLibXml2Test);
end.

