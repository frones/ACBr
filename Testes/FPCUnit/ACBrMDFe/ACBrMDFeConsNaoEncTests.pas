unit ACBrMDFeConsNaoEncTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrMDFe.ConsNaoEnc; // Unit nova

type

  { ACBrMDFeConsNaoEncTest }

  ACBrMDFeConsNaoEncTest = class(TTestCase)
  private
    FConsNaoEnc_New: TConsMDFeNaoEnc;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GerarXml_ConsNaoEnc;

  end;

implementation

uses
  ACBrMDFeConstantesTests,
  pcnConversao;

{ ACBrMDFeConsNaoEncTest }

procedure ACBrMDFeConsNaoEncTest.SetUp;
begin
  inherited SetUp;

  FConsNaoEnc_New := TConsMDFeNaoEnc.Create;
end;

procedure ACBrMDFeConsNaoEncTest.TearDown;
begin
  FConsNaoEnc_New.Free;

  inherited TearDown;
end;

procedure ACBrMDFeConsNaoEncTest.GerarXml_ConsNaoEnc;
var
  sxml_old, sxml_new: string;
begin
  sxml_old := sXml_ConsNaoEnc;

  // Gerar o XML usando a unit nova
  FConsNaoEnc_New.TpAmb := taHomologacao;
  FConsNaoEnc_New.CNPJCPF := '12345678000123';
  FConsNaoEnc_New.Versao := '4.00';

  sxml_new := FConsNaoEnc_New.GerarXML;

  CheckEquals(sxml_new, sxml_old, 'Xml novo de ConsNaoEnc diferente do antigo');
end;

initialization

  _RegisterTest('ACBrMDFeConsNaoEncTests', ACBrMDFeConsNaoEncTest);

end.
