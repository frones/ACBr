unit ACBrNFSeXRetornoSoapTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrTests.Util,
  ACBrNFSeX;

type

  { ACBrNFSeXRetornoSoapTest }

  ACBrNFSeXRetornoSoapTest = class(TTestCase)
  private
    FACBrNFSeX1: TACBrNFSeX;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AoCriarComponente_ContadorDeNotas_DeveSerZero;
    procedure LoadFromFile_TratarXmlRetornado;
    procedure Fiorilli200_LoadFromFile_TratarXMLRetornado;

  end;

implementation

uses
  ACBrConsts, ACBrXmlBase,
  ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrNFSeXConversao;

const
  SArquivoRetornoSoap  = '..\..\..\..\Recursos\NFSe\RetornoSoap.xml';
  SArquivoRetorno  = '..\..\..\..\Recursos\NFSe\Retorno.xml';
  UmMunicipioWebFisco = 3527603;

{ ACBrNFSeXRetornoSoapTest }

procedure ACBrNFSeXRetornoSoapTest.SetUp;
begin
  inherited SetUp;
  FACBrNFSeX1 := TACBrNFSeX.Create(nil);
  FACBrNFSeX1.Configuracoes.Geral.CodigoMunicipio := UmMunicipioWebFisco;
end;

procedure ACBrNFSeXRetornoSoapTest.TearDown;
begin
  FACBrNFSeX1.Free;
  inherited TearDown;
end;

procedure ACBrNFSeXRetornoSoapTest.AoCriarComponente_ContadorDeNotas_DeveSerZero;
begin
  CheckEquals(0, FACBrNFSeX1.NotasFiscais.Count, 'Contador de NFSe Não é zero');
end;

procedure ACBrNFSeXRetornoSoapTest.LoadFromFile_TratarXmlRetornado;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoRetornoSoap);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  sxml := SeparaDados(sxml, 'Body');

  sxml := ParseText(AnsiString(sxml), True, {$IfDef FPC}True{$Else}False{$EndIf});

  sxml := StringReplace(sxml, '&', '&amp;', [rfReplaceAll]);
  sxml := RemoverIdentacao(sxml);
  sxml := RemoverCaracteresDesnecessarios(sxml);
  sxml := InserirDeclaracaoXMLSeNecessario(sxml);

  WriteToTXT(SArquivoRetorno, sxml, False, False);
end;

procedure ACBrNFSeXRetornoSoapTest.Fiorilli200_LoadFromFile_TratarXMLRetornado;
var
  lStrList: TStringList;
  sxml: string;
begin
  lStrList := TStringList.Create;;
  try
    //lStrList.TrailingLineBreak := False; //Não funciona no Delphi 7
    lStrList.LoadFromFile(SArquivoRetornoSoap);
    sxml := lStrList.Text;
  finally
    lStrList.Free;
  end;

  //remove o linebreak que fica no final da string por ter vindo do "TStringList.Text"
  if Length(sxml) >= Length(sLineBreak) then
  begin
    sxml := Copy(sxml, 0, Length(sxml) - Length(sLineBreak));
  end;

  sxml := SeparaDados(sxml, 'Body');

  sxml := NativeStringToUTF8(sxml);
  sxml := StringReplace(sxml, '&#xd;', '\s\n', [rfReplaceAll]);
  sxml := ParseText(AnsiString(sxml), True, {$IfDef FPC}True{$Else}False{$EndIf});
  sxml := RemoverPrefixosDesnecessarios(sxml);
  sxml := RemoverCaracteresDesnecessarios(sxml);

  WriteToTXT(SArquivoRetorno, sxml, False, False);
end;

initialization

  _RegisterTest('ACBrNFSeXRetornoSoapTests', ACBrNFSeXRetornoSoapTest);

end.
