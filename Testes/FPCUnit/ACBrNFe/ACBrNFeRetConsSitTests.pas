unit ACBrNFeRetConsSitTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFe.RetConsSit;

type

  { ACBrNFeRetConsSitTest }

  ACBrNFeRetConsSitTest = class(TTestCase)
  private
    FRetConsSitNFe: TRetConsSitNFe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadFromFile_RetConsSitNFe;
    procedure LoadFromFile_Ret_Vazio;

  end;

implementation

uses
  ACBrConsts, ACBrXmlBase,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrNFeConstantesTests,
  pcnConversao;

{ ACBrNFeRetConsSitTest }

procedure ACBrNFeRetConsSitTest.SetUp;
begin
  inherited SetUp;

  FRetConsSitNFe := TRetConsSitNFe.Create('4.00');
end;

procedure ACBrNFeRetConsSitTest.TearDown;
begin
  FRetConsSitNFe.Free;

  inherited TearDown;
end;

procedure ACBrNFeRetConsSitTest.LoadFromFile_RetConsSitNFe;
var
  sxml: string;
  i: Integer;
begin
  sxml := sxml_RetConsSit;

  FRetConsSitNFe.XmlRetorno := ParseText(sxml);
  FRetConsSitNFe.LerXML;

  // Leitura do grupo <retConsSitNFe>
  CheckEquals('4.00', FRetConsSitNFe.versao, 'Versao valor incorreto');
  CheckEquals('2', tpAmbToStr(FRetConsSitNFe.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('SP_NFE_PL_006e', FRetConsSitNFe.verAplic, 'verAplic valor incorreto');
  CheckEquals(100, FRetConsSitNFe.cStat, 'cStat valor incorreto');
  CheckEquals('Autorizado o uso da NF-e', FRetConsSitNFe.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(35, FRetConsSitNFe.cUF, 'cUF valor incorreto');
  CheckEquals('35100804550110000188550010000000101204117493', FRetConsSitNFe.chNFe, 'chNFe valor incorreto');

  // Leitura do grupo <infProt> que esta dentro do grupo <protNFe>
  CheckEquals('2', TipoAmbienteToStr(FRetConsSitNFe.protNFe.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('SP_NFE_PL_006e', FRetConsSitNFe.protNFe.verAplic, 'verAplic valor incorreto');
  CheckEquals('35100804550110000188550010000000101204117493', FRetConsSitNFe.protNFe.chDFe, 'chNFe valor incorreto');
  CheckEquals(EncodeDataHora('2010-08-31T19:20:22'), FRetConsSitNFe.protNFe.dhRecbto, 'dhRecbto valor incorreto');
  CheckEquals('135100025493261', FRetConsSitNFe.protNFe.nProt, 'nProt valor incorreto');
  CheckEquals('PNG7OJ2WYLQyhkL2kWBykEGSVQA=', FRetConsSitNFe.protNFe.digVal, 'digVal valor incorreto');
  CheckEquals(100, FRetConsSitNFe.protNFe.cStat, 'cStat valor incorreto');
  CheckEquals('Autorizado o uso da NF-e', FRetConsSitNFe.protNFe.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(1, FRetConsSitNFe.protNFe.cMsg, 'cMsg valor incorreto');
  CheckEquals('Autorizado', FRetConsSitNFe.protNFe.xMsg, 'xMsg valor incorreto');

  // Leitura do grupo <procEventoNFe>
  for i := 0 to FRetConsSitNFe.procEventoNFe.Count -1 do
  begin
    CheckEquals('1.00', FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.versao, 'Versao valor incorreto');

    // Leitura do grupo <infEvento> que esta dentro do grupo <evento>
    CheckEquals(35, FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.cOrgao, 'cOrgao valor incorreto');
    CheckEquals('1', tpAmbToStr(FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.tpAmb), 'tpAmb valor incorreto');
    CheckEquals('05694537000112', FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.CNPJ, 'CNPJ valor incorreto');
    CheckEquals('35180905694537000112550010001449721001449725', FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.chNFe, 'chNFe valor incorreto');
    CheckEquals(EncodeDataHora('2018-09-18T16:07:56-03:00'), FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.dhEvento, 'dhEvento valor incorreto');
    CheckEquals('110111', TpEventoToStr(FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.tpEvento), 'tpEvento valor incorreto');
    CheckEquals(1, FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
    CheckEquals('1.00', FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.versaoEvento, 'Versao valor incorreto');

    // Leitura do grupo <detEvento> que esta dentro do grupo <infEvento>
    CheckEquals('Cancelamento', FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.detEvento.descEvento, 'descEvento valor incorreto');
    CheckEquals('135180636988450', FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.detEvento.nProt, 'nProt valor incorreto');
    CheckEquals('erro de faturamento', FRetConsSitNFe.procEventoNFe[i].RetEventoNFe.InfEvento.detEvento.xJust, 'xJust valor incorreto');
  {
  infEvento.DetEvento.xCorrecao := ObterConteudoTag(ANode.Childrens.FindAnyNs('xCorrecao'), tcStr);
  infEvento.DetEvento.xCondUso:= ObterConteudoTag(ANode.Childrens.FindAnyNs('xCondUso'), tcStr);

  InfEvento.detEvento.cOrgaoAutor := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgaoAutor'), tcInt);
  infEvento.detEvento.tpAutor := StrToTipoAutor(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAutor'), tcStr));
  infEvento.detEvento.verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
  infEvento.detEvento.dhEmi := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
  infEvento.detEvento.tpNF := StrToTpNF(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr));
  infEvento.detEvento.IE := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);

  // Comprovante de Entrega da NF-e e o Cancelamento do Comprovante
  infEvento.detEvento.dhEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEntrega'), tcDatHor);
  infEvento.detEvento.nDoc := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDoc'), tcStr);
  infEvento.detEvento.xNome := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  infEvento.detEvento.latGPS := ObterConteudoTag(ANode.Childrens.FindAnyNs('latGPS'), tcDe6);
  infEvento.detEvento.longGPS := ObterConteudoTag(ANode.Childrens.FindAnyNs('longGPS'), tcDe6);

  infEvento.detEvento.hashComprovante := ObterConteudoTag(ANode.Childrens.FindAnyNs('hashComprovante'), tcStr);
  infEvento.detEvento.hashComprovante := ObterConteudoTag(ANode.Childrens.FindAnyNs('hashComprovante'), tcDatHor);
  infEvento.detEvento.nProtEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProtEvento'), tcStr);
  infEvento.detEvento.tpAutorizacao := StrToAutorizacao(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAutorizacao'), tcStr));

  Ler_Dest(ANode.Childrens.FindAnyNs('dest'));
  Ler_autXML(ANode.Childrens.FindAnyNs('autXML'))
  }
  end;
end;

procedure ACBrNFeRetConsSitTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
begin
  sxml := sxml_Ret_Vazio;

  FRetConsSitNFe.XmlRetorno := ParseText(sxml);
  FRetConsSitNFe.LerXML;

  // Leitura do grupo <retConsSitNFe>
  CheckEquals('', FRetConsSitNFe.versao, 'Versao valor incorreto');
end;

initialization

  _RegisterTest('ACBrNFeRetConsSitTests', ACBrNFeRetConsSitTest);

end.
