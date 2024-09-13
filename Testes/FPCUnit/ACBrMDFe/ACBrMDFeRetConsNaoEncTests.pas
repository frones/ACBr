unit ACBrMDFeRetConsNaoEncTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrMDFe.RetConsNaoEnc; // Unit nova

type

  { ACBrMDFeRetConsNaoEncTest }

  ACBrMDFeRetConsNaoEncTest = class(TTestCase)
  private
    FRetConsNaoEnc: TRetConsMDFeNaoEnc;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadFromFile_RetConsNaoEnc;
    procedure LoadFromFile_Ret_Vazio;
  end;

implementation

uses
  pcnConversao,
  ACBrUtil.XMLHTML,
  ACBrMDFeConstantesTests;

{ ACBrMDFeConsSitTests }

procedure ACBrMDFeRetConsNaoEncTest.SetUp;
begin
  inherited SetUp;

  FRetConsNaoEnc := TRetConsMDFeNaoEnc.Create;
end;

procedure ACBrMDFeRetConsNaoEncTest.TearDown;
begin
  FRetConsNaoEnc.Free;

  inherited TearDown;
end;

procedure ACBrMDFeRetConsNaoEncTest.LoadFromFile_RetConsNaoEnc;
var
  sxml: string;
  i: Integer;
begin
  sxml := sxml_RetConsNaoEnc;

  FRetConsNaoEnc.XmlRetorno := ParseText(sxml);
  FRetConsNaoEnc.LerXML;

  // Leitura do grupo <retConsMDFeNaoEnc>
  CheckEquals('4.00', FRetConsNaoEnc.versao, 'Versao valor incorreto');
  CheckEquals('2', tpAmbToStr(FRetConsNaoEnc.tpAmb), 'tpAmb valor incorreto');
  CheckEquals('SP_PL_MDFe_400', FRetConsNaoEnc.verAplic, 'verAplic valor incorreto');
  CheckEquals(100, FRetConsNaoEnc.cStat, 'cStat valor incorreto');
  CheckEquals('MDFe nao encerrado', FRetConsNaoEnc.xMotivo, 'xMotivo valor incorreto');
  CheckEquals(35, FRetConsNaoEnc.cUF, 'cUF valor incorreto');

  for i := 0 to FRetConsNaoEnc.InfMDFe.Count -1 do
  begin
    CheckEquals('12345678901234567890123456789012345678901234', FRetConsNaoEnc.InfMDFe[i].chMDFe, 'chMDFe valor incorreto');
    CheckEquals('123456789012345', FRetConsNaoEnc.InfMDFe[i].nProt, 'nProt valor incorreto');
  end;
end;

procedure ACBrMDFeRetConsNaoEncTest.LoadFromFile_Ret_Vazio;
var
  sxml: string;
begin
  sxml := sxml_Ret_Vazio;

  FRetConsNaoEnc.XmlRetorno := ParseText(sxml);
  FRetConsNaoEnc.LerXML;

  // Leitura do grupo <retConsMDFeNaoEnc>
  CheckEquals('', FRetConsNaoEnc.versao, 'Versao valor incorreto');
end;

initialization

  _RegisterTest('ACBrMDFeRetConsNaoEncTests', ACBrMDFeRetConsNaoEncTest);

end.
