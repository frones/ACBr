unit ACBrNFeRetEnvEventoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrTests.Util,
  ACBrNFe.RetEnvEvento;

type

  { ACBrNFeRetEnvEventoTest }

  ACBrNFeRetEnvEventoTest = class(TTestCase)
  private
    FRetEnvEvento: TRetEventoNFe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadFromFile_RetEnvEvento;
    procedure LoadFromFile_Ret_Vazio;

  end;

implementation

uses
  pcnConversao,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrNFeConstantesTests;

{ ACBrNFeRetEnvEventoTest }

procedure ACBrNFeRetEnvEventoTest.SetUp;
begin
  inherited SetUp;

  FRetEnvEvento := TRetEventoNFe.Create;
end;

procedure ACBrNFeRetEnvEventoTest.TearDown;
begin
  FRetEnvEvento.Free;

  inherited TearDown;
end;

procedure ACBrNFeRetEnvEventoTest.LoadFromFile_RetEnvEvento;
var
  sxml: string;
begin
  sxml := sxml_RetEnvEvento;

  FRetEnvEvento.XmlRetorno := ParseText(sxml);
  FRetEnvEvento.LerXML;
{
  CheckEquals('3.00', FRetEnvEvento.versao, 'Versao valor incorreto');
  CheckEquals('ID113130001934975', FRetEnvEvento.Id, 'Id valor incorreto');
  CheckEquals('2', tpAmbToStr(FRetEnvEvento.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('AM3.00', FRetEnvEvento.verAplic, 'verAplic valor incorreto');
  CheckEquals(102, FRetEnvEvento.cStat, 'cStat valor incorreto');
  CheckEquals('Inutilizacao de numero homologado', FRetEnvEvento.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(13, FRetEnvEvento.cUF, 'cUF valor incorreto');
  CheckEquals(EncodeDataHora('2013-01-24T19:46:32-04:00'), FRetEnvEvento.dhRecbto, 'dhRecbto valor incorreto');
  CheckEquals(13, FRetEnvEvento.ano, 'ano valor incorreto');
  CheckEquals('12345678000123', FRetEnvEvento.CNPJ, 'CNPJ valor incorreto');
  CheckEquals(65, FRetEnvEvento.Modelo, 'mod valor incorreto');
  CheckEquals(1, FRetEnvEvento.serie, 'serie valor incorreto');
  CheckEquals(16, FRetEnvEvento.nNFIni, 'nNFIni valor incorreto');
  CheckEquals(18, FRetEnvEvento.nNFFin, 'nNFFin valor incorreto');
  CheckEquals('113130001934975', FRetEnvEvento.nProt, 'nProt valor incorreto');
  }
end;

procedure ACBrNFeRetEnvEventoTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
begin
  sxml := sxml_Ret_Vazio;

  FRetEnvEvento.XmlRetorno := ParseText(sxml);
  FRetEnvEvento.LerXML;
  {
  CheckEquals('', FRetEnvEvento.versao, 'Versao valor incorreto');
  CheckEquals('', FRetEnvEvento.Id, 'Id valor incorreto');
  CheckEquals('1', tpAmbToStr(FRetEnvEvento.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('', FRetEnvEvento.verAplic, 'verAplic valor incorreto');
  CheckEquals(0, FRetEnvEvento.cStat, 'cStat valor incorreto');
  CheckEquals('', FRetEnvEvento.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(0, FRetEnvEvento.cUF, 'cUF valor incorreto');
  CheckEquals(EncodeDataHora('1899-12-30T00:00:00'), FRetEnvEvento.dhRecbto, 'dhRecbto valor incorreto');
  CheckEquals(0, FRetEnvEvento.ano, 'ano valor incorreto');
  CheckEquals('', FRetEnvEvento.CNPJ, 'CNPJ valor incorreto');
  CheckEquals(0, FRetEnvEvento.Modelo, 'mod valor incorreto');
  CheckEquals(0, FRetEnvEvento.serie, 'serie valor incorreto');
  CheckEquals(0, FRetEnvEvento.nNFIni, 'nNFIni valor incorreto');
  CheckEquals(0, FRetEnvEvento.nNFFin, 'nNFFin valor incorreto');
  CheckEquals('', FRetEnvEvento.nProt, 'nProt valor incorreto');
  }
end;

initialization

  _RegisterTest('ACBrNFeRetEnvEventoTests', ACBrNFeRetEnvEventoTest);

end.
