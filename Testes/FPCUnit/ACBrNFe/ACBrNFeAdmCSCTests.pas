unit ACBrNFeAdmCSCTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFe.AdmCSC; // Unit nova

type

  { ACBrNFeAdmCSCTest }

  ACBrNFeAdmCSCTest = class(TTestCase)
  private
    FAdmCSC_New: TAdmCSCNFCe;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarXml_AdmCSC_ConsultaCSC;
    procedure GerarXml_AdmCSC_NovoCSC;
    procedure GerarXml_AdmCSC_RevogaCSC;

  end;

implementation

uses
  ACBrNFeConstantesTests,
  pcnConversao;

{ ACBrNFeAdmCSCTests }

procedure ACBrNFeAdmCSCTest.SetUp;
begin
  inherited SetUp;

  FAdmCSC_New := TAdmCSCNFCe.Create;
end;

procedure ACBrNFeAdmCSCTest.TearDown;
begin
  FAdmCSC_New.Free;

  inherited TearDown;
end;

procedure ACBrNFeAdmCSCTest.GerarXml_AdmCSC_ConsultaCSC;
var
  sxml_old, sxml_new: string;
begin
  sxml_old := sXml_admCSCConsulta;

  // Gerar o XML usando a unit nova
  FAdmCSC_New.TpAmb := taHomologacao;
  FAdmCSC_New.RaizCNPJ := '12345678';
  FAdmCSC_New.indOP := ioConsultaCSC;
  FAdmCSC_New.idCsc := 0;
  FAdmCSC_New.codigoCsc := '';
  FAdmCSC_New.Versao := '4.00';

  sxml_new := FAdmCSC_New.GerarXML;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de ConsultaCSC diferente do antigo');
end;

procedure ACBrNFeAdmCSCTest.GerarXml_AdmCSC_NovoCSC;
var
  sxml_old, sxml_new: string;
begin
  sxml_old := sXml_admCSCNovo;

  // Gerar o XML usando a unit nova
  FAdmCSC_New.TpAmb := taHomologacao;
  FAdmCSC_New.RaizCNPJ := '12345678';
  FAdmCSC_New.indOP := ioNovoCSC;
  FAdmCSC_New.idCsc := 0;
  FAdmCSC_New.codigoCsc := '';
  FAdmCSC_New.Versao := '4.00';

  sxml_new := FAdmCSC_New.GerarXML;;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de NovoCSC diferente do antigo');
end;

procedure ACBrNFeAdmCSCTest.GerarXml_AdmCSC_RevogaCSC;
var
  sxml_old, sxml_new: string;
begin
  sxml_old := sXml_admCSCRevoga;

  // Gerar o XML usando a unit nova
  FAdmCSC_New.TpAmb := taHomologacao;
  FAdmCSC_New.RaizCNPJ := '12345678';
  FAdmCSC_New.indOP := ioRevogaCSC;
  FAdmCSC_New.idCsc := 1;
  FAdmCSC_New.codigoCsc := 'abc';
  FAdmCSC_New.Versao := '4.00';

  sxml_new := FAdmCSC_New.GerarXML;;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de RevogaCSC diferente do antigo');
end;

initialization

  _RegisterTest('ACBrNFeAdmCSCTests', ACBrNFeAdmCSCTest);

end.
