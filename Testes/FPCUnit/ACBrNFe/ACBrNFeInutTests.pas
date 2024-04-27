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

initialization

  _RegisterTest('ACBrNFeInutTests', ACBrNFeInutTest);

end.
