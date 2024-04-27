unit ACBrNFeRetAdmCSCTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrTests.Util,
  ACBrNFe.RetAdmCSC;

type

  { ACBrNFeRetAdmCSCTest }

  ACBrNFeRetAdmCSCTest = class(TTestCase)
  private
    FRetAdmCSC: TRetAdmCSCNFCe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadFromFile_RetAdmCSC;
    procedure LoadFromFile_Ret_Vazio;

  end;

implementation

uses
  pcnConversao,
  ACBrUtil.XMLHTML,
  ACBrNFeConstantesTests;

{ ACBrNFeRetAdmCSCTest }

procedure ACBrNFeRetAdmCSCTest.SetUp;
begin
  inherited SetUp;

  FRetAdmCSC := TRetAdmCSCNFCe.Create;
end;

procedure ACBrNFeRetAdmCSCTest.TearDown;
begin
  FRetAdmCSC.Free;

  inherited TearDown;
end;

procedure ACBrNFeRetAdmCSCTest.LoadFromFile_RetAdmCSC;
var
  sxml: string;
  i: Integer;
begin
  sxml := sxml_RetAdmCSC;

  FRetAdmCSC.XmlRetorno := ParseText(sxml);
  FRetAdmCSC.LerXML;

  CheckEquals('1.00', FRetAdmCSC.versao, 'Versao valor incorreto');
  CheckEquals('2', tpAmbToStr(FRetAdmCSC.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('1', IndOperacaoToStr(FRetAdmCSC.indOp), 'indOp valor incorreto');
  CheckEquals(150, FRetAdmCSC.cStat, 'cStat valor incorreto');
  CheckEquals('Consulta de CSC realizada', FRetAdmCSC.xMotivo, 'xMotivo valor incorreto');

  for i := 0 to FRetAdmCSC.dadosCsc.Count -1 do
  begin
    CheckEquals(1, FRetAdmCSC.dadosCsc[i].idCsc, 'idCsc valor incorreto');
    CheckEquals('abc', FRetAdmCSC.dadosCsc[i].codigoCsc, 'codigoCsc valor incorreto');
  end;
end;

procedure ACBrNFeRetAdmCSCTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
  i: Integer;
begin
  sxml := sxml_Ret_Vazio;

  FRetAdmCSC.XmlRetorno := ParseText(sxml);
  FRetAdmCSC.LerXML;

  CheckEquals('', FRetAdmCSC.versao, 'Versao valor incorreto');
  CheckEquals('1', tpAmbToStr(FRetAdmCSC.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('1', IndOperacaoToStr(FRetAdmCSC.indOp), 'indOp valor incorreto');
  CheckEquals(0, FRetAdmCSC.cStat, 'cStat valor incorreto');
  CheckEquals('', FRetAdmCSC.xMotivo, 'xMotivo valor incorreto');

  for i := 0 to FRetAdmCSC.dadosCsc.Count -1 do
  begin
    CheckEquals(0, FRetAdmCSC.dadosCsc[i].idCsc, 'idCsc valor incorreto');
    CheckEquals('', FRetAdmCSC.dadosCsc[i].codigoCsc, 'codigoCsc valor incorreto');
  end;
end;

initialization

  _RegisterTest('ACBrNFeRetAdmCSCTests', ACBrNFeRetAdmCSCTest);

end.
