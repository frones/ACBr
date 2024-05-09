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
//    procedure Fiorilli200_LoadFromFile_TratarXMLRetornado;

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
  UmMunicipioWebFisco = 3169356;

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

  if Pos('Body', sxml) > 0 then
    sxml := SeparaDados(sxml, 'Body');

(*
  // ISSIntel - 1508407
  sxml := ParseText(AnsiString(sxml), True, {$IfDef FPC}True{$Else}False{$EndIf});
  sxml := RemoverIdentacao(sxml);
  sxml := RemoverCaracteresDesnecessarios(sxml);
  sxml := RemoverPrefixosDesnecessarios(sxml);
  sxml := StringReplace(sxml, '&', '&amp;', [rfReplaceAll]);
*)

(*
  // SigISSWeb - 3545803
  sxml := NativeStringToUTF8(sxml);
  sxml := ParseText(AnsiString(sxml), True, {$IfDef FPC}True{$Else}False{$EndIf});
  sxml := RemoverDeclaracaoXML(sxml);
  sxml := RemoverIdentacao(sxml);
  sxml := RemoverCaracteresDesnecessarios(sxml);
  sxml := RemoverPrefixosDesnecessarios(sxml);
  sxml := StringReplace(sxml, '&', '&amp;', [rfReplaceAll]);
*)

(*
  // Fiorilli - 3544004
//  sxml := SeparaDados(sxml, 'ns2:ListaNfse');

  if UTF8Decode(sxml) = '' then
    sxml := NativeStringToUTF8(sxml);

  sxml := string(NativeStringToUTF8(sxml));
  sxml := StringReplace(sxml, '&#xd;', '\s\n', [rfReplaceAll]);
  sxml := StringReplace(sxml, ''#$A'', '\s\n', [rfReplaceAll]);
  sxml := ParseText(AnsiString(sxml), True, {$IfDef FPC}True{$Else}False{$EndIf});
  sxml := RemoverPrefixosDesnecessarios(sxml);
  sxml := RemoverCaracteresDesnecessarios(sxml);
  sxml := StringReplace(sxml, '&', '&amp;', [rfReplaceAll]);
*)
(*
  sxml := ParseText(AnsiString(sxml), True, {$IfDef FPC}True{$Else}False{$EndIf});

  sxml := StringReplace(sxml, '&', '&amp;', [rfReplaceAll]);
  sxml := RemoverIdentacao(sxml);
  sxml := RemoverCaracteresDesnecessarios(sxml);
  sxml := InserirDeclaracaoXMLSeNecessario(sxml);
*)


//  NFSeBrasil - 3169356
  sxml := SeparaDados(sxml, 'return');

  if Pos('ISO-8859-1', sxml) > 0 then
    sxml := AnsiToNativeString(sXML);

  sxml := StringReplace(sxml, '&amp;amp;', 'e',[rfReplaceAll]);
  sxml := ParseText(AnsiString(sxml), True, {$IfDef FPC}True{$Else}False{$EndIf});
  sxml := RemoverDeclaracaoXML(sxml, True);
  sxml := RemoverCDATA(sxml);
  sxml := RemoverIdentacao(sxml);
  sxml := RemoverPrefixosDesnecessarios(sxml);
  sxml := RemoverCaracteresDesnecessarios(sxml);
  sxml := StringReplace(sxml, 'R$', '', [rfReplaceAll]);
  sxml := StringReplace(sxml, '&', '&amp;', [rfReplaceAll]);
  sxml := NativeStringToUTF8(sxml);

  sxml := InserirDeclaracaoXMLSeNecessario(sxml);

  WriteToTXT(SArquivoRetorno, sxml, False, False);
end;

(*
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
*)
initialization

  _RegisterTest('ACBrNFSeXRetornoSoapTests', ACBrNFSeXRetornoSoapTest);

end.
