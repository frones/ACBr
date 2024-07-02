unit ACBrCTeRetConsSitTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrCTe.RetConsSit;

type

  { ACBrCTeRetConsSitTest }

  ACBrCTeRetConsSitTest = class(TTestCase)
  private
    FRetConsSitCTe: TRetConsSitCTe;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadFromFile_RetConsSitCTe;
    procedure LoadFromFile_Ret_Vazio;

  end;

implementation

uses
  ACBrConsts, ACBrXmlBase,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrCTeConstantesTests,
  pcnConversao;

{ ACBrCTeRetConsSitTest }

procedure ACBrCTeRetConsSitTest.SetUp;
begin
  inherited SetUp;

  FRetConsSitCTe := TRetConsSitCTe.Create('4.00');
end;

procedure ACBrCTeRetConsSitTest.TearDown;
begin
  FRetConsSitCTe.Free;

  inherited TearDown;
end;

procedure ACBrCTeRetConsSitTest.LoadFromFile_RetConsSitCTe;
var
  sxml: string;
  i, j: Integer;
begin
  sxml := sxml_RetConsSit;

  FRetConsSitCTe.XmlRetorno := ParseText(sxml);
  FRetConsSitCTe.LerXML;

  // Leitura do grupo <retConsSitCTe>
  CheckEquals('4.00', FRetConsSitCTe.versao, 'Versao valor incorreto');
  CheckEquals('2', tpAmbToStr(FRetConsSitCTe.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('SP_PL_CTe_400', FRetConsSitCTe.verAplic, 'verAplic valor incorreto');
  CheckEquals(100, FRetConsSitCTe.cStat, 'cStat valor incorreto');
  CheckEquals('Autorizado o uso do CT-e', FRetConsSitCTe.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(35, FRetConsSitCTe.cUF, 'cUF valor incorreto');
//  CheckEquals('12345678901234567890123456789012345678901234', FRetConsSitCTe.chCTe, 'chCTe valor incorreto');

  // Leitura do grupo <infProt> que esta dentro do grupo <protCTe>
  CheckEquals('2', TipoAmbienteToStr(FRetConsSitCTe.protCTe.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('SP_PL_CTe_400', FRetConsSitCTe.protCTe.verAplic, 'verAplic valor incorreto');
  CheckEquals('12345678901234567890123456789012345678901234', FRetConsSitCTe.protCTe.chDFe, 'chCTe valor incorreto');
  CheckEquals(EncodeDataHora('2010-08-31T19:20:22'), FRetConsSitCTe.protCTe.dhRecbto, 'dhRecbto valor incorreto');
  CheckEquals('123456789012345', FRetConsSitCTe.protCTe.nProt, 'nProt valor incorreto');
  CheckEquals('PNG7OJ2WYLQyhkL2kWBykEGSVQA=', FRetConsSitCTe.protCTe.digVal, 'digVal valor incorreto');
  CheckEquals(100, FRetConsSitCTe.protCTe.cStat, 'cStat valor incorreto');
  CheckEquals('Autorizado o uso do CT-e', FRetConsSitCTe.protCTe.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(1, FRetConsSitCTe.protCTe.cMsg, 'cMsg valor incorreto');
  CheckEquals('Autorizado', FRetConsSitCTe.protCTe.xMsg, 'xMsg valor incorreto');

  // Leitura do grupo <procEventoNFe>
  for i := 0 to FRetConsSitCTe.procEventoCTe.Count -1 do
  begin
    CheckEquals('1.00', FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.versao, 'Versao valor incorreto');

    // Leitura do grupo <infEvento> que esta dentro do grupo <evento>
    for j := 0 to FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento.Count -1 do
    begin
      CheckEquals(35, FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.cOrgao, 'cOrgao valor incorreto');
      CheckEquals('1', tpAmbToStr(FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.tpAmb), 'tpAmb valor incorreto');
      CheckEquals('12345678901234567890123456789012345678901234', FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.chCTe, 'chCTe valor incorreto');
      CheckEquals('110111', TpEventoToStr(FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.tpEvento), 'tpEvento valor incorreto');
      CheckEquals(1, FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
      CheckEquals('12345678000123', FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.CNPJDest, 'CNPJDest valor incorreto');
      CheckEquals(EncodeDataHora('2018-09-18T16:07:56-03:00'), FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.dhRegEvento, 'dhRegEvento valor incorreto');
      CheckEquals('123456789012345', FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.retEvento[j].RetInfEvento.nProt, 'nProt valor incorreto');
    end;

{
    // Leitura do grupo <detEvento> que esta dentro do grupo <infEvento>
    CheckEquals('Cancelamento', FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.InfEvento.detEvento.descEvento, 'descEvento valor incorreto');
    CheckEquals('123456789012345', FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.InfEvento.detEvento.nProt, 'nProt valor incorreto');
    CheckEquals('erro de faturamento', FRetConsSitCTe.procEventoCTe[i].RetEventoCTe.InfEvento.detEvento.xJust, 'xJust valor incorreto');

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

procedure ACBrCTeRetConsSitTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
begin
  sxml := sxml_Ret_Vazio;

  FRetConsSitCTe.XmlRetorno := ParseText(sxml);
  FRetConsSitCTe.LerXML;

  // Leitura do grupo <retConsSitNFe>
  CheckEquals('', FRetConsSitCTe.versao, 'Versao valor incorreto');
end;

initialization

  _RegisterTest('ACBrCTeRetConsSitTests', ACBrCTeRetConsSitTest);

end.
