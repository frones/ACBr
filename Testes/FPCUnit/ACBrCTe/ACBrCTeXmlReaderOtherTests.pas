unit ACBrCTeXmlReaderOtherTests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ACBrCTeXmlHandler, ACBrTests.Util, pcteCTe;

type

  { TACBrCTeXmlReaderOtherTests }

  TACBrCTeXmlReaderOtherTests = class(TTestCase)
  published
    procedure LerXML_SemACBrCTeAtribuido_RetornaException;
    procedure LerXML_SemArquivoCarregado_RetornaException;
    procedure LerXML_ArquivoXMLIncorreto_RetornaException;
    procedure LerXML_XMLSemAtributoId_RetornaException;
    procedure LerXML_XMLSemAtributoVersao_RetornaException;
    procedure LerXML_XMLValido_NaoRetornaException;
  end;

implementation

uses
  ACBrCTeTestConsts, ACBrBase;

{ TACBrCTeXmlReaderOtherTests }

procedure TACBrCTeXmlReaderOtherTests.LerXML_SemACBrCTeAtribuido_RetornaException;
var
  FCTeReader: TCTeXmlReader;
begin
  FCTeReader := TCTeXmlReader.Create(nil);
  try
    try
    FCTeReader.CarregarArquivo(XML_CTE_NORMAL_VERSAO400);
    FCTeReader.LerXml;
    except
      on E:Exception do
        exit;
    end;
    Fail('Não foi gerada uma exception para CTe não atribuído');
  finally
    FCTeReader.Free;
  end;
end;

procedure TACBrCTeXmlReaderOtherTests.LerXML_SemArquivoCarregado_RetornaException;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    try
      FCTeReader.LerXml;
    except
      on E:Exception do
        exit;
    end;
    Fail('Não foi gerada uma Exception para Arquivo não carregado');
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderOtherTests.LerXML_ArquivoXMLIncorreto_RetornaException;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTE.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    try
      FCTeReader.CarregarArquivo(XML_INVALIDO);
      FCTeReader.LerXml;
    except
      on E:Exception do
        exit;
    end;
    Fail('Não foi gerada uma Exception para Arquivo Xml incorreto');
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderOtherTests.LerXML_XMLSemAtributoId_RetornaException;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    try
      FCTeReader.CarregarArquivo(XML_SEMATRIBUTOID);
      FCTeReader.LerXml;
    except
      on E:Exception do
        exit;
    end;
    Fail('Não foi gerada uma Exception para Atribuido Id não encontrado');
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderOtherTests.LerXML_XMLSemAtributoVersao_RetornaException;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    try
      FCTeReader.CarregarArquivo(XML_SEMATRIBUTOVERSAO);
      FCTeReader.LerXml;
    except
      on E:Exception do
        exit;
    end;
    Fail('Não foi gerada uma Exception para Atributo Versão não encontrado');
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

procedure TACBrCTeXmlReaderOtherTests.LerXML_XMLValido_NaoRetornaException;
var
  FCTe: TCTe;
  FCTeReader: TCTeXmlReader;
  Success: Boolean;
begin
  FCTe := TCTe.Create;
  FCTeReader := TCTeXmlReader.Create(FCTe);
  try
    try
    FCTeReader.CarregarArquivo(XML_CTE_NORMAL_VERSAO400);
    Success := FCTeReader.LerXml;

    CheckEquals(True, Success, 'LerXML retornou com XML válido retornou False');
    except
      on E:Exception do
        Fail('Erro na leitura do XML: ' + E.Message);
    end;
  finally
    FCTeReader.Free;
    FCTe.Free;
  end;
end;

{ TACBrCTeXmlReaderOtherTests }



initialization
  _RegisterTest('ACBrCTeXmlReader.OutrosTestes', TACBrCTeXmlReaderOtherTests);

end.

