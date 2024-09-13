unit ACBrMDFeConsSitTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrMDFe.ConsSit; // Unit nova

type

  { ACBrMDFeConsSitTest }

  ACBrMDFeConsSitTest = class(TTestCase)
  private
    FConsSit_New: TConsSitMDFe;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarXml_ConsSit;

  end;

implementation

uses
  ACBrMDFeConstantesTests,
  pcnConversao;

{ ACBrMDFeConsSitTests }

procedure ACBrMDFeConsSitTest.SetUp;
begin
  inherited SetUp;

  FConsSit_New := TConsSitMDFe.Create;
end;

procedure ACBrMDFeConsSitTest.TearDown;
begin
  FConsSit_New.Free;

  inherited TearDown;
end;

procedure ACBrMDFeConsSitTest.GerarXml_ConsSit;
var
  sxml_old, sxml_new: string;
begin
  sxml_old := sXml_ConsSit;

  // Gerar o XML usando a unit nova
  FConsSit_New.TpAmb := taHomologacao;
  FConsSit_New.chMDFe := '12345678901234567890123456789012345678901234';
  FConsSit_New.Versao := '4.00';

  sxml_new := FConsSit_New.GerarXML;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de ConsSit diferente do antigo');
end;

initialization

  _RegisterTest('ACBrMDFeConsSitTests', ACBrMDFeConsSitTest);

end.
