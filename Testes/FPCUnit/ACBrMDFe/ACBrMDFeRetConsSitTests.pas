unit ACBrMDFeRetConsSitTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrMDFe.RetConsSit;

type

  { ACBrMDFeRetConsSitTest }

  ACBrMDFeRetConsSitTest = class(TTestCase)
  private
    FRetConsSitMDFe: TRetConsSitMDFe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadFromFile_RetConsSitMDFe;
    procedure LoadFromFile_Ret_Vazio;

  end;

implementation

uses
  ACBrXmlBase,
  pcnConversao,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrMDFeConstantesTests;

{ ACBrMDFeRetConsSitTest }

procedure ACBrMDFeRetConsSitTest.SetUp;
begin
  inherited SetUp;

  FRetConsSitMDFe := TRetConsSitMDFe.Create('4.00');
end;

procedure ACBrMDFeRetConsSitTest.TearDown;
begin
  FRetConsSitMDFe.Free;

  inherited TearDown;
end;

procedure ACBrMDFeRetConsSitTest.LoadFromFile_RetConsSitMDFe;
var
  sxml: string;
  i, j: Integer;
begin
  sxml := sxml_RetConsSit;

  FRetConsSitMDFe.XmlRetorno := ParseText(sxml);
  FRetConsSitMDFe.LerXML;

  // Leitura do grupo <retConsSitMDFe>
  CheckEquals('4.00', FRetConsSitMDFe.versao, 'Versao valor incorreto');
  CheckEquals('2', tpAmbToStr(FRetConsSitMDFe.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('SP_PL_MDFe_400', FRetConsSitMDFe.verAplic, 'verAplic valor incorreto');
  CheckEquals(100, FRetConsSitMDFe.cStat, 'cStat valor incorreto');
  CheckEquals('Autorizado o uso do MDF-e', FRetConsSitMDFe.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(35, FRetConsSitMDFe.cUF, 'cUF valor incorreto');

  // Leitura do grupo <infProt> que esta dentro do grupo <protMDFe>
  CheckEquals('2', TipoAmbienteToStr(FRetConsSitMDFe.protMDFe.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('SP_PL_MDFe_400', FRetConsSitMDFe.protMDFe.verAplic, 'verAplic valor incorreto');
  CheckEquals('12345678901234567890123456789012345678901234', FRetConsSitMDFe.protMDFe.chDFe, 'chMDFe valor incorreto');
  CheckEquals(EncodeDataHora('2010-08-31T19:20:22'), FRetConsSitMDFe.protMDFe.dhRecbto, 'dhRecbto valor incorreto');
  CheckEquals('123456789012345', FRetConsSitMDFe.protMDFe.nProt, 'nProt valor incorreto');
  CheckEquals('PNG7OJ2WYLQyhkL2kWBykEGSVQA=', FRetConsSitMDFe.protMDFe.digVal, 'digVal valor incorreto');
  CheckEquals(100, FRetConsSitMDFe.protMDFe.cStat, 'cStat valor incorreto');
  CheckEquals('Autorizado o uso do MDF-e', FRetConsSitMDFe.protMDFe.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(1, FRetConsSitMDFe.protMDFe.cMsg, 'cMsg valor incorreto');
  CheckEquals('Autorizado', FRetConsSitMDFe.protMDFe.xMsg, 'xMsg valor incorreto');

  // Leitura do grupo <procEventoMDFe>
  for i := 0 to FRetConsSitMDFe.procEventoMDFe.Count -1 do
  begin
    CheckEquals('4.00', FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.versao, 'Versao valor incorreto');

    // Leitura do grupo <infEvento> que esta dentro do grupo <evento>
    for j := 0 to FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento.Count -1 do
    begin
      CheckEquals(35, FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento[j].RetInfEvento.cOrgao, 'cOrgao valor incorreto');
      CheckEquals('1', tpAmbToStr(FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento[j].RetInfEvento.tpAmb), 'tpAmb valor incorreto');
      CheckEquals('12345678901234567890123456789012345678901234', FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento[j].RetInfEvento.chMDFe, 'chMDFe valor incorreto');
      CheckEquals('110111', TpEventoToStr(FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento[j].RetInfEvento.tpEvento), 'tpEvento valor incorreto');
      CheckEquals(1, FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento[j].RetInfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
      CheckEquals(EncodeDataHora('2018-09-18T16:07:56-03:00'), FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento[j].RetInfEvento.dhRegEvento, 'dhRegEvento valor incorreto');
      CheckEquals('123456789012345', FRetConsSitMDFe.procEventoMDFe[i].RetEventoMDFe.retEvento[j].RetInfEvento.nProt, 'nProt valor incorreto');
    end;
  end;
end;

procedure ACBrMDFeRetConsSitTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
begin
  sxml := sxml_Ret_Vazio;

  FRetConsSitMDFe.XmlRetorno := ParseText(sxml);
  FRetConsSitMDFe.LerXML;

  // Leitura do grupo <retConsSitNFe>
  CheckEquals('', FRetConsSitMDFe.versao, 'Versao valor incorreto');
end;

initialization

  _RegisterTest('ACBrMDFeRetConsSitTests', ACBrMDFeRetConsSitTest);

end.
