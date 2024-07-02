unit ACBrCTeRetEnvEventoTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrTests.Util,
  ACBrCTe.RetEnvEvento;

type

  { ACBrCTeRetEnvEventoTest }

  ACBrCTeRetEnvEventoTest = class(TTestCase)
  private
    FRetEnvEvento: TRetEventoCTe;
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
  ACBrCTeConstantesTests;

{ ACBrCTeRetEnvEventoTest }

procedure ACBrCTeRetEnvEventoTest.SetUp;
begin
  inherited SetUp;

  FRetEnvEvento := TRetEventoCTe.Create;
end;

procedure ACBrCTeRetEnvEventoTest.TearDown;
begin
  FRetEnvEvento.Free;

  inherited TearDown;
end;

procedure ACBrCTeRetEnvEventoTest.LoadFromFile_RetEnvEvento;
var
  sxml: string;
begin
  sxml := sxml_RetEnvEvento;

  FRetEnvEvento.XmlRetorno := ParseText(sxml);
  FRetEnvEvento.LerXML;

  CheckEquals('1.00', FRetEnvEvento.versao, 'Versao valor incorreto');
  CheckEquals(8, FRetEnvEvento.idLote, 'idLote valor incorreto');
  CheckEquals('1', tpAmbToStr(FRetEnvEvento.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('AN_1.0.0', FRetEnvEvento.verAplic, 'verAplic valor incorreto');
  CheckEquals(91, FRetEnvEvento.cOrgao, 'cOrgao valor incorreto');
  CheckEquals(128, FRetEnvEvento.cStat, 'cStat valor incorreto');
  CheckEquals('Lote de evento processado', FRetEnvEvento.xMotivo, 'xMotivo valor incorreto');

  // Leitura do grupo infEvento que esta dentro de retEvento (que é uma lista)
  // primeira ocorrência
  if FRetEnvEvento.retEvento.Count > 0 then
  begin
    CheckEquals('ID891120000074187', FRetEnvEvento.retEvento[0].RetInfEvento.Id, 'Id valor incorreto');
    CheckEquals('1', tpAmbToStr(FRetEnvEvento.retEvento[0].RetInfEvento.tpAmb), 'tpAmb valor incorreto');
    CheckEquals('AN_1.0.0', FRetEnvEvento.retEvento[0].RetInfEvento.verAplic, 'verAplic valor incorreto');
    CheckEquals(91, FRetEnvEvento.retEvento[0].RetInfEvento.cOrgao, 'cOrgao valor incorreto');
    CheckEquals(135, FRetEnvEvento.retEvento[0].RetInfEvento.cStat, 'cStat valor incorreto');
    CheckEquals('Evento registrado e vinculado a NF-e', FRetEnvEvento.retEvento[0].RetInfEvento.xMotivo, 'xMotivo valor incorreto');
    CheckEquals('35121012345678000123550010000003911000003915', FRetEnvEvento.retEvento[0].RetInfEvento.chCTe, 'chCTe valor incorreto');
    CheckEquals('210210', TpEventoToStr(FRetEnvEvento.retEvento[0].RetInfEvento.tpEvento), 'tpEvento valor incorreto');
    CheckEquals(1, FRetEnvEvento.retEvento[0].RetInfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
    CheckEquals(EncodeDataHora('2012-11-21T23:51:55-02:00'), FRetEnvEvento.retEvento[0].RetInfEvento.dhRegEvento, 'dhRegEvento valor incorreto');
    CheckEquals('891120000074187', FRetEnvEvento.retEvento[0].RetInfEvento.nProt, 'nProt valor incorreto');
  end;

  // Leitura do grupo infEvento que esta dentro de retEvento (que é uma lista)
  // segunda ocorrência
  if FRetEnvEvento.retEvento.Count > 1 then
  begin
    CheckEquals('ID891120000074188', FRetEnvEvento.retEvento[1].RetInfEvento.Id, 'Id valor incorreto');
    CheckEquals('1', tpAmbToStr(FRetEnvEvento.retEvento[1].RetInfEvento.tpAmb), 'tpAmb valor incorreto');
    CheckEquals('AN_1.0.0', FRetEnvEvento.retEvento[1].RetInfEvento.verAplic, 'verAplic valor incorreto');
    CheckEquals(91, FRetEnvEvento.retEvento[1].RetInfEvento.cOrgao, 'cOrgao valor incorreto');
    CheckEquals(135, FRetEnvEvento.retEvento[1].RetInfEvento.cStat, 'cStat valor incorreto');
    CheckEquals('Evento registrado e vinculado a NF-e', FRetEnvEvento.retEvento[1].RetInfEvento.xMotivo, 'xMotivo valor incorreto');
    CheckEquals('35121012345678000123550010000068961010068962', FRetEnvEvento.retEvento[1].RetInfEvento.chCTe, 'chCTe valor incorreto');
    CheckEquals('210210', TpEventoToStr(FRetEnvEvento.retEvento[1].RetInfEvento.tpEvento), 'tpEvento valor incorreto');
    CheckEquals(1, FRetEnvEvento.retEvento[1].RetInfEvento.nSeqEvento, 'nSeqEvento valor incorreto');
    CheckEquals(EncodeDataHora('2012-11-21T23:51:55-02:00'), FRetEnvEvento.retEvento[1].RetInfEvento.dhRegEvento, 'dhRegEvento valor incorreto');
    CheckEquals('891120000074188', FRetEnvEvento.retEvento[1].RetInfEvento.nProt, 'nProt valor incorreto');
  end;
end;

procedure ACBrCTeRetEnvEventoTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
begin
  sxml := sxml_Ret_Vazio;

  FRetEnvEvento.XmlRetorno := ParseText(sxml);
  FRetEnvEvento.LerXML;

  CheckEquals('', FRetEnvEvento.versao, 'Versao valor incorreto');
  CheckEquals(0, FRetEnvEvento.idLote, 'idLote valor incorreto');
  CheckEquals('1', tpAmbToStr(FRetEnvEvento.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('', FRetEnvEvento.verAplic, 'verAplic valor incorreto');
  CheckEquals(0, FRetEnvEvento.cOrgao, 'cOrgao valor incorreto');
  CheckEquals(0, FRetEnvEvento.cStat, 'cStat valor incorreto');
  CheckEquals('', FRetEnvEvento.xMotivo, 'xMotivo valor incorreto');

end;

initialization

  _RegisterTest('ACBrCTeRetEnvEventoTests', ACBrCTeRetEnvEventoTest);

end.
