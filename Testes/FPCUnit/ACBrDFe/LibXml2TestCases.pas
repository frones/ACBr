unit LibXml2TestCases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  ACBrUtil, ACBrDFeSSL;

type

  LibXml2Test = class(TTestCase)
  private
    FpDFeSSL: TDFeSSL;
    SchemaPath: string;
    XmlPath: string;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Schema_Xml_Valido;
    procedure Schema_Xml_Nao_Valido;
    procedure Sign_Xml_Valido;
    procedure Sign_Xml_Nao_Valido;
  end;

implementation

procedure LibXml2Test.Schema_Xml_Valido;
var
  Xml: TStringlist;
  Erro, AXML, DeclaracaoXML: String;
begin
  CheckTrue(FileExists(XmlPath + 'xml-valido.xml'), 'Arquivo Xml não não encontrado.');
  CheckTrue(FileExists(SchemaPath + 'nfe_v3.10.xsd'), 'Arquivo Schema não encontrado.');

  Erro := '';
  Xml := TStringlist.Create;
  Xml.LoadFromFile(XmlPath + 'xml-valido.xml');
  AXML := Xml.Text;

  // Extraindo apenas os dados da NFe (sem nfeProc)
  DeclaracaoXML := ObtemDeclaracaoXML(AXML);
  AXML := DeclaracaoXML + '<NFe xmlns' +
          RetornarConteudoEntre(AXML, '<NFe xmlns', '</NFe>') +'</NFe>';

  CheckTrue(FpDFeSSL.Validar(AXML, SchemaPath + 'nfe_v3.10.xsd', Erro), 'Erro ao validar xml com o Schema/Xml invalido.');
end;

procedure LibXml2Test.Schema_Xml_Nao_Valido;
var
  Xml: TStringlist;
  Erro, AXML, DeclaracaoXML: String;
begin
  CheckTrue(FileExists(XmlPath + 'xml-schema-invalido.xml'), 'Arquivo Xml não não encontrado.');
  CheckTrue(FileExists(SchemaPath + 'nfe_v3.10.xsd'), 'Arquivo Schema não encontrado.');

  Erro := '';
  Xml := TStringlist.Create;
  Xml.LoadFromFile(XmlPath + 'xml-schema-invalido.xml');
  AXML := Xml.Text;

  // Extraindo apenas os dados da NFe (sem nfeProc)
  DeclaracaoXML := ObtemDeclaracaoXML(AXML);
  AXML := DeclaracaoXML + '<NFe xmlns' +
          RetornarConteudoEntre(AXML, '<NFe xmlns', '</NFe>') +'</NFe>';

  CheckFalse(FpDFeSSL.Validar(AXML, SchemaPath + 'nfe_v3.10.xsd', Erro), 'Xml Valido.');
end;

procedure LibXml2Test.Sign_Xml_Valido;
var
  Xml: TStringlist;
  Erro, AXML, DeclaracaoXML: String;
begin
  CheckTrue(FileExists(XmlPath + 'xml-valido.xml'), 'Arquivo Xml não não encontrado.');

  Erro := '';

  Xml := TStringlist.Create;
  Xml.LoadFromFile(XmlPath + 'xml-valido.xml');
  AXML := Xml.Text;

  // Extraindo apenas os dados da NFe (sem nfeProc)
  DeclaracaoXML := ObtemDeclaracaoXML(AXML);
  AXML := DeclaracaoXML + '<NFe xmlns' +
          RetornarConteudoEntre(AXML, '<NFe xmlns', '</NFe>') +'</NFe>';

  CheckTrue(FpDFeSSL.VerificarAssinatura(AXML, Erro, 'infNFe'), 'Assinatura invalida.');
end;

procedure LibXml2Test.Sign_Xml_Nao_Valido;
var
  Xml: TStringlist;
  Erro, AXML, DeclaracaoXML: String;
begin
  CheckTrue(FileExists(XmlPath + 'xml-sign-invalido.xml'), 'Arquivo Xml não não encontrado.');

  Erro := '';

  Xml := TStringlist.Create;
  Xml.LoadFromFile(XmlPath + 'xml-sign-invalido.xml');
  AXML := Xml.Text;

  // Extraindo apenas os dados da NFe (sem nfeProc)
  DeclaracaoXML := ObtemDeclaracaoXML(AXML);
  AXML := DeclaracaoXML + '<NFe xmlns' +
          RetornarConteudoEntre(AXML, '<NFe xmlns', '</NFe>') +'</NFe>';

  CheckFalse(FpDFeSSL.VerificarAssinatura(AXML, Erro, 'infNFe'), 'Assinatura Valida.');
end;

procedure LibXml2Test.SetUp;
begin
  FpDFeSSL := TDFeSSL.Create;
  FpDFeSSL.SSLCryptLib := cryWinCrypt;
  FpDFeSSL.SSLHttpLib := httpWinHttp;
  FpDFeSSL.SSLXmlSignLib := xsLibXml2;
  FpDFeSSL.NumeroSerie := '372F0EDED2C8C284180C10A83BF603B9';
  SchemaPath := '..\..\..\Exemplos\ACBrDFe\Schemas\NFe\';
  XmlPath := ApplicationPath + 'Xml\'
end;

procedure LibXml2Test.TearDown;
begin
  FreeAndNil(FpDFeSSL);
end;

initialization

  RegisterTest('ACBrDFe.xsLibXml2', LibXml2Test{$ifndef FPC}.suite{$endif});
end.

