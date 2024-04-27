unit ACBrNFeRetInutTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrTests.Util,
  ACBrNFe.RetInut;

type

  { ACBrNFeRetInutTest }

  ACBrNFeRetInutTest = class(TTestCase)
  private
    FRetInut: TRetInutNFe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadFromFile_RetInut;
    procedure LoadFromFile_Ret_Vazio;

  end;

implementation

uses
  pcnConversao,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrNFeConstantesTests;

{ ACBrNFeRetInutTest }

procedure ACBrNFeRetInutTest.SetUp;
begin
  inherited SetUp;

  FRetInut := TRetInutNFe.Create;
end;

procedure ACBrNFeRetInutTest.TearDown;
begin
  FRetInut.Free;

  inherited TearDown;
end;

procedure ACBrNFeRetInutTest.LoadFromFile_RetInut;
var
  sxml: string;
begin
  sxml := sxml_RetInut;

  FRetInut.XmlRetorno := ParseText(sxml);
  FRetInut.LerXML;

  CheckEquals('3.00', FRetInut.versao, 'Versao valor incorreto');
  CheckEquals('ID113130001934975', FRetInut.Id, 'Id valor incorreto');
  CheckEquals('2', tpAmbToStr(FRetInut.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('AM3.00', FRetInut.verAplic, 'verAplic valor incorreto');
  CheckEquals(102, FRetInut.cStat, 'cStat valor incorreto');
  CheckEquals('Inutilizacao de numero homologado', FRetInut.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(13, FRetInut.cUF, 'cUF valor incorreto');
  CheckEquals(EncodeDataHora('2013-01-24T19:46:32-04:00'), FRetInut.dhRecbto, 'dhRecbto valor incorreto');
  CheckEquals(13, FRetInut.ano, 'ano valor incorreto');
  CheckEquals('12345678000123', FRetInut.CNPJ, 'CNPJ valor incorreto');
  CheckEquals(65, FRetInut.Modelo, 'mod valor incorreto');
  CheckEquals(1, FRetInut.serie, 'serie valor incorreto');
  CheckEquals(16, FRetInut.nNFIni, 'nNFIni valor incorreto');
  CheckEquals(18, FRetInut.nNFFin, 'nNFFin valor incorreto');
  CheckEquals('113130001934975', FRetInut.nProt, 'nProt valor incorreto');
end;

procedure ACBrNFeRetInutTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
begin
  sxml := sxml_Ret_Vazio;

  FRetInut.XmlRetorno := ParseText(sxml);
  FRetInut.LerXML;

  CheckEquals('', FRetInut.versao, 'Versao valor incorreto');
  CheckEquals('', FRetInut.Id, 'Id valor incorreto');
  CheckEquals('1', tpAmbToStr(FRetInut.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('', FRetInut.verAplic, 'verAplic valor incorreto');
  CheckEquals(0, FRetInut.cStat, 'cStat valor incorreto');
  CheckEquals('', FRetInut.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(0, FRetInut.cUF, 'cUF valor incorreto');
  CheckEquals(EncodeDataHora('1899-12-30T00:00:00'), FRetInut.dhRecbto, 'dhRecbto valor incorreto');
  CheckEquals(0, FRetInut.ano, 'ano valor incorreto');
  CheckEquals('', FRetInut.CNPJ, 'CNPJ valor incorreto');
  CheckEquals(0, FRetInut.Modelo, 'mod valor incorreto');
  CheckEquals(0, FRetInut.serie, 'serie valor incorreto');
  CheckEquals(0, FRetInut.nNFIni, 'nNFIni valor incorreto');
  CheckEquals(0, FRetInut.nNFFin, 'nNFFin valor incorreto');
  CheckEquals('', FRetInut.nProt, 'nProt valor incorreto');
end;

initialization

  _RegisterTest('ACBrNFeRetInutTests', ACBrNFeRetInutTest);

end.
