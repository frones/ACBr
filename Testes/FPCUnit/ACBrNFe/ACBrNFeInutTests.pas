unit ACBrNFeInutTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFe.Inut; // Unit nova

type

  { ACBrNFeInutTest }

  ACBrNFeInutTest = class(TTestCase)
  private
    FInut_New: TInutNFe;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarXml_Inut;
    procedure LerXml_Inut;

  end;

implementation

uses
  ACBrNFeConstantesTests,
  pcnConversao;

{ ACBrNFeInutTests }

procedure ACBrNFeInutTest.SetUp;
begin
  inherited SetUp;

  FInut_New := TInutNFe.Create;
end;

procedure ACBrNFeInutTest.TearDown;
begin
  FInut_New.Free;

  inherited TearDown;
end;

procedure ACBrNFeInutTest.GerarXml_Inut;
var
  sxml_old, sxml_new: string;
begin
  sxml_old := sXml_Inut;

  // Gerar o XML usando a unit nova
  FInut_New.Versao := '4.00';
  FInut_New.TpAmb := taHomologacao;
  FInut_New.cUF := 35;
  FInut_New.ano := 2024;
  FInut_New.CNPJ := '12345678000123';
  FInut_New.modelo := 55;
  FInut_New.serie := 1;
  FInut_New.nNFIni := 10;
  FInut_New.nNFFin := 20;
  FInut_New.xJust := 'Erro no Sistema de Emissao de Notas';

  sxml_new := FInut_New.GerarXML;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de Inut diferente do antigo');
end;

procedure ACBrNFeInutTest.LerXml_Inut;
begin
  FInut_New.LerXMLFromString(sXml_Inut);

  CheckEquals('4.00', FInut_New.versao, 'Versao valor incorreto');
  CheckEquals('ID35241234567800012355001000000010000000020', FInut_New.Id, 'Id valor incorreto');
  CheckEquals('2', tpAmbToStr(FInut_New.tpAmb), 'tpAmb valor incorreto');
  CheckEquals(35, FInut_New.cUF, 'cUF valor incorreto');
  CheckEquals(24, FInut_New.ano, 'ano valor incorreto');
  CheckEquals('12345678000123', FInut_New.CNPJ, 'CNPJ valor incorreto');
  CheckEquals(55, FInut_New.Modelo, 'mod valor incorreto');
  CheckEquals(1, FInut_New.serie, 'serie valor incorreto');
  CheckEquals(10, FInut_New.nNFIni, 'nNFIni valor incorreto');
  CheckEquals(20, FInut_New.nNFFin, 'nNFFin valor incorreto');
  CheckEquals('Erro no Sistema de Emissao de Notas', FInut_New.xJust, 'xJust valor incorreto');
end;

initialization

  _RegisterTest('ACBrNFeInutTests', ACBrNFeInutTest);

end.
