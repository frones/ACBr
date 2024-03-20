unit ACBrUtilTest;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, Math,
  {$ifdef FPC}
  LConvEncoding,
  {$endif}
  ACBrTests.Util;

type

  { ParseTextTest }

  ParseTextTest = class(TTestCase)
  private
  published
    procedure ParseDecode;
    procedure ParseEncode;
    procedure VerificarConversaoTextoLongo;
    procedure ParseStringSimples_SemParametros;
    procedure ParseStringAcentuada_SemParametros;
    procedure ParseStringAcentuada_DecodeFalse_IsUTF8False;
    procedure ParseStringAcentuada_DecodeTrue_IsUTF8False;
    procedure ParseStringAcentuada_DecodeFalse_IsUTF8True;
    procedure ParseStringAcentuadaConvertidaParaAnsi_SemParametros;
    procedure ParseStringAcentuadaConvertidaParaAnsi_DecodeFalse_IsUTF8False;
    procedure ParseStringAcentuadaConvertidaParaAnsi_DecodeTrue_IsUTF8False;
    procedure ParseStringAcentuadaConvertidaParaAnsi_DecodeFalse_IsUTF8True;
    procedure ParseStringAcentuadaConvertidaParaUTF8_SemParametros;
    procedure ParseStringAcentuadaConvertidaParaUTF8_DecodeFalse_IsUTF8False;
    procedure ParseStringAcentuadaConvertidaParaUTF8_DecodeTrue_IsUTF8False;
    procedure ParseStringAcentuadaConvertidaParaUTF8_DecodeFalse_IsUTF8True;

  end;

  { LerTagXMLTest }

  LerTagXMLTest = class(TTestCase)
  published
    procedure Simples;
    procedure SemIgnorarCase;
    procedure ComVariasTags;
  end;

  { TestXmlEhUTF8 }

  TestXmlEhUTF8 = class(TTestCase)
  published
    procedure PadraoUTF8AspasDuplasUpperCase;
    procedure PadraoUTF8AspasSimplesLowerCase;
    procedure NaoUTF8;
    procedure ApenasXML10;
  end;

  { SepararDadosTest }

  SepararDadosTest = class(TTestCase)
  published
    procedure Simples;
    procedure TextoLongo;
    procedure MostrarChave;
    procedure ComVariasChaves;
    procedure SemFecharChave;
    procedure SemAbrirChave;
    procedure ComPrefixo;
    procedure MostrarChaveComPrefixo;
    procedure QuandoImitacaoChaveEstaNoMeio;
  end;

  { TruncFixTest }

  TruncFixTest = class(TTestCase)
  published
    procedure AsExpression;
    procedure AsDouble;
    procedure AsExtended;
    procedure AsCurrency;
    procedure AsLargeExtended;
    procedure AsLargeDouble;
  end;

  { TruncToTest }

  TruncToTest = class(TTestCase)
  private
  published
    procedure As199Currency;
    procedure As4386Currency;
    procedure As1526Currency;
    procedure As113Currency;
    procedure As199Extended;
    procedure As4386Extended;
    procedure As1526Extended;
    procedure As113Extended;
    procedure As199Double;
    procedure As9999899Double;
    procedure As9999899DoubleTruncToZero;
    procedure As4386Double;
    procedure As1526Double;
    procedure As113Double;
    procedure As199Single;
    procedure As4386Single;
    procedure As1526Single;
    procedure As113Single;
    procedure As11133Single;
    procedure As11133Double;
    procedure As11133Extended;
    procedure As11133Currency;
  end;

  { RoundABNTTest }

  {Veja coment·rio na parte "initialization"}
  RoundABNTTest = class(TTestCase)
  published
    procedure AsIntegerImpar;
    procedure AsIntegerPar;
    procedure TresParaDuasCasasDecimais;
    procedure QuatroParaDuasCasasDecimais;
    procedure ExpressaoCurrDuasCasasDecimais;
    procedure ExpressaoDblDuasCasasDecimais;
    procedure TestesEstouro;
    procedure ValoresNegativos;
    procedure DoctoABNTRegra2_1;
    procedure DoctoABNTRegra2_2;
    procedure DoctoABNTRegra2_3;
    procedure DoctoABNTRegra2_4;
  end;

  { RoundABNTMudancasDeArredondamentoPara_rmUpTest }

  {Veja coment·rio na parte "initialization"}
  RoundABNTMudancasDeArredondamentoPara_rmUpTest = class(RoundABNTTest)
  private
    OldRM: TFPURoundingMode;
  protected
     procedure SetUp; override;
     procedure TearDown; override;
  end;

  { RoundABNTMudancasDeArredondamentoPara_rmDownTest }

  {Veja coment·rio na parte "initialization"}
  RoundABNTMudancasDeArredondamentoPara_rmDownTest = class(RoundABNTTest)
  private
    OldRM: TFPURoundingMode;
  protected
     procedure SetUp; override;
     procedure TearDown; override;
  end;

  { RoundABNTMudancasDeArredondamentoPara_rmTruncateTest }

  {Veja coment·rio na parte "initialization"}
  RoundABNTMudancasDeArredondamentoPara_rmTruncateTest = class(RoundABNTTest)
  private
    OldRM: TFPURoundingMode;
  protected
     procedure SetUp; override;
     procedure TearDown; override;
  end;

  { StripHTMLTest }

  StripHTMLTest = class(TTestCase)
  published
   procedure TesteSimples;
   procedure TesteCompleto;
   procedure TesteTagsInvalidas;
   procedure TesteTagsInvertidas;
  end;

  { CompareVersionsTest }

  CompareVersionsTest = class(TTestCase)
  published
   procedure VersaoIgual;
   procedure VersaoMaior;
   procedure VersaoMenor;
   procedure TrocarDelimitador;
   procedure NomeclaturaDiferente;
  end;

  { TestBitTest }

  TestBitTest = class(TTestCase)
  published
    procedure TestarTodosBits;
  end;

  { TesteSetBit }

  TesteSetBit = class(TTestCase)
  private
    AByte: Integer;
  protected
    procedure SetUp; override;
  published
    procedure LigaBitsDeZeroASete;
    procedure LigaBitsPares;
    procedure LigaBitsImpares;
    procedure LigaTodosOsBitsDeUmByte;
  end;

  { TesteClearBit }

  TesteClearBit = class(TTestCase)
  private
    AByte: Integer;
  protected
    procedure SetUp; override;
  published
    procedure DesligaBitsDeZeroASete;
    procedure DesligaBitsPares;
    procedure DesligaBitsImpares;
    procedure DesligaTodosOsBitsDeUmByte;
  end;

  { TestePutBit }

  TestePutBit = class(TTestCase)
  published
    procedure LigaEDesligaBitsDeZeroASete;
  end;

  { IntToBinTest }

  IntToBinTest = class(TTestCase)
  published
    procedure TestarTodosBits;
  end;

  { BinToIntTest }

  BinToIntTest = class(TTestCase)
  published
    procedure TestarTodosBits;
  end;

  { BcdToAscTest }

  BcdToAscTest = class(TTestCase)
  published
    procedure Normal;
    procedure ComZerosAEsquerda;
  end;

  { AscToBcdTest }

  AscToBcdTest = class(TTestCase)
  published
    procedure TamanhoTres;
    procedure TruncandoComTamanhoDois;
    procedure AumentandoComtamanhoQuatro;
  end;

  { IntToLEStrTest }

  IntToLEStrTest = class(TTestCase)
  published
    procedure TamanhoUm;
    procedure TamanhoDois;
    procedure TamanhoQuatro;
  end;

  { LEStrToIntTest }

  LEStrToIntTest = class(TTestCase)
  published
    procedure TamanhoUm;
    procedure TamanhoDois;
    procedure TamanhoQuatro;
  end;

  { HexToAsciiTest }

  HexToAsciiTest = class(TTestCase)
  published
    procedure Simples;
    procedure Comlexo;
  end;

  { AsciiToHexTest }

  AsciiToHexTest = class(TTestCase)
  published
    procedure Simples;
    procedure Comlexo;
  end;

  { BinaryStringToStringTest }

  BinaryStringToStringTest = class(TTestCase)
  published
    procedure Simples;
    procedure ComNulos;
    procedure Reverso;
  end;

  { StringToBinaryStringTest }

  StringToBinaryStringTest = class(TTestCase)
  published
    procedure Simples;
    procedure ComNulos;
    procedure NaoHexa;
    procedure Reverso;
  end;

  { IntToStrZeroTest }

  IntToStrZeroTest = class(TTestCase)
  published
   procedure AdicionarZerosAoNumero;
   procedure TamanhoMenorQueLimite;
  end;

  { FloatToIntStrTest }

  FloatToIntStrTest = class(TTestCase)
  published
   procedure Normal;
   procedure ValorDecimaisDefault;
   procedure MudandoPadraoDeDecimais;
   procedure SemDecimais;
   procedure ValorComMaisDecimais;
  end;

  { FloatToStringTest }

  FloatToStringTest = class(TTestCase)
  published
   procedure Normal;
   procedure ComDecimaisZerados;
   procedure MudandoPontoDecimal;
   procedure ComFormatacao;
   procedure RemovendoSerparadorDeMilhar;
  end;

  { FormatFloatBrTest }

  FormatFloatBrTest = class(TTestCase)
  published
   procedure Normal;
   procedure ComDecimaisZerados;
   procedure ComFormatacao;
   procedure ComSerparadorDeMilhar;
   procedure ComMascaraDisplayFormat027x2;
   procedure ComMascaraDisplayFormat026x2;
   procedure ComMascaraDisplayFormat02666x3;
   procedure ComMascaraDisplayFormat026660x4;
   procedure ComMascaraDisplayFormat026601x5;
   procedure ComMascaramsk15x2;
  end;

  { FloatMaskTest }

  FloatMaskTest = class(TTestCase)
  published
   procedure Inteiro;
   procedure DuasCasas_Com_Separador_de_Milhar;
   procedure DuasCasas_Sem_Separador_de_Milhar;
   procedure QuatroCasas_Com_Separador_de_Milhar;
   procedure QuatroCasas_Sem_Separador_de_Milhar;
  end;

  { StringToFloatTest }

  StringToFloatTest = class(TTestCase)
  published
   procedure ComVirgula;
   procedure ComPonto;
   procedure ApenasInteiro;
  end;

  { StringToFloatDefTest }

  StringToFloatDefTest = class(TTestCase)
  published
   procedure ComVirgula;
   procedure ComPonto;
   procedure ApenasInteiro;
   procedure ValorDefault;
  end;

  { StrIsIPTest }

  StrIsIPTest = class(TTestCase)
  published
    procedure Normal;
    procedure NormalComZerosAEsquerda;
    procedure ComNome;
    procedure Errados;
  end;

  { EstaVazio_NaoEstaVazioTest }

  EstaVazio_NaoEstaVazioTest = class(TTestCase)
  published
    procedure Vazio;
    procedure NaoVazio;
    procedure ComEspacos;
  end;

  { EstaZerado_NaoEstaZeradoTest }

  EstaZerado_NaoEstaZeradoTest = class(TTestCase)
  published
    procedure Zerado;
    procedure NaoZerado;
    procedure Negativo;
  end;

  { TamanhoIgualTest }

  TamanhoIgualTest = class(TTestCase)
  published
    procedure Menor;
    procedure Maior;
    procedure Igual;
  end;

  { TamanhoMenorTest }

  TamanhoMenorTest = class(TTestCase)
  published
    procedure Menor;
    procedure Maior;
    procedure Igual;
  end;

  { TraduzComandoTest }

  TraduzComandoTest = class(TTestCase)
  published
    procedure ComPipeEComentario;
    procedure ComPipeApenas;
    procedure SemPipe;
  end;

  { StringToAscTest }

  StringToAscTest = class(TTestCase)
  published
    procedure Normal;
  end;

  { AscToStringTest }

  AscToStringTest = class(TTestCase)
  published
    procedure Normal;
    procedure ComLetras;
    procedure ComValoresInvalidos;
  end;

  { StrCryptTest }

  StrCryptTest = class(TTestCase)
  published
    procedure CryptDecrypt;
    procedure CryptStringVazia;
    procedure CryptChaveVazia;
    procedure CryptStringEChaveVazia;
  end;

  { SomaAscIITest }

  SomaAscIITest = class(TTestCase)
  published
    procedure Computar;
  end;

  { StringCrc16Test }

  StringCrc16Test = class(TTestCase)
  published
    procedure Computar;
  end;

  { PathWithDelimTest }

  PathWithDelimTest = class(TTestCase)
  published
    procedure SemDelimiter;
    procedure ComDelimiter;
  end;

  { PathWithoutDelimTest }

  PathWithoutDelimTest = class(TTestCase)
  published
    procedure SemDelimiter;
    procedure ComDelimiter;
  end;

  { TranslateUnprintableTest }

  TranslateUnprintableTest = class(TTestCase)
  published
    procedure CaracteresNormais;
    procedure CaracteresDeControle;
    procedure Ambos;
  end;

  { EAN13Test }

  EAN13Test = class(TTestCase)
  published
    procedure Valido;
    procedure TamanhoMaior;
    procedure TamanhoMenor;
    procedure DigitoInvalido;
    procedure ComLetras;
    procedure EAN13Valido_StringInvalida_RetornaFalso;
    procedure EAN13Valido_StringValida_RetornaTrue;
    procedure EAN13Valido_String0000000000000_RetornaTrue;
    procedure EAN13Valido_String7896232517828_RetornaTrue;
    procedure EAN13Valido_String7896645900026_RetornaTrue;
    procedure EAN13Valido_String7898908141016_RetornaTrue;
    procedure EAN13Valido_String7893946087173_RetornaTrue;
    procedure EAN13Valido_String7897186015095_RetornaTrue;
    procedure EAN13Valido_String7891060886139_RetornaTrue;
    procedure EAN13Valido_String7898132132019_RetornaTrue;
    procedure EAN13Valido_String7506195185568_RetornaTrue;
    procedure EAN13Valido_String2000100002629_RetornaTrue;
    procedure EAN13_DV_StringAlphanumerica_RetornaVazio;
    procedure EAN13_DV_Codigo000000000000_Retorna0;
    procedure EAN13_DV_Codigo789623251782_Retorna8;
    procedure EAN13_DV_Codigo789664590002_Retorna6;
    procedure EAN13_DV_Codigo789890814101_Retorna6;
    procedure EAN13_DV_Codigo789394608717_Retorna3;
    procedure EAN13_DV_Codigo789718601509_Retorna5;
    procedure EAN13_DV_Codigo789106088613_Retorna9;
    procedure EAN13_DV_Codigo789813213201_Retorna9;
    procedure EAN13_DV_Codigo750619518556_Retorna8;
    procedure EAN13_DV_Codigo200010000262_Retorna9;
  end;

  { ComparaValorTest }

  ComparaValorTest = class(TTestCase)
  published
    procedure ValorUmMenorTolerancia001Igual;
    procedure ValorUmMenorTolerancia0001Menor;
    procedure ValorUmMaiorTolerancia0001Maior;
    procedure ValorUmMenorTolerancia00001Igual;
    procedure ValorUmMenorTolerancia000001Menor;
    procedure ValorUmMaiorTolerancia000001Maior;
  end;

  { ZipUnzip }

  ZipUnzip = class(TTestCase)
  private
    fStr: String;
    fTituloACBr: String;
    fTituloCompressDeflateAndEncoded64: String;
    fTituloCompressGZipAndEncoded64: String;
  protected
    procedure SetUp; override;
  published
    procedure DeflateCompressDecompressAString;
    procedure DeflateCompressDecompressAStream;
    procedure DeflateCompressAndEncode64;
    procedure GZipCompressDecompressAsString;
    procedure GZipCompressDecompressAsStream;
    procedure GZipCompressAndEncode64;
    procedure ZipFileCompressDecompressAString;
    procedure Decode64AndDeCompressDeflate;
    procedure Decode64AndDeCompressGZip;
  end;



implementation

uses
  synacode,
  ACBrCompress, ACBrConsts, ACBrUtil.Compatibilidade, ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrUtil.Math, ACBrUtil.XMLHTML, ACBrUtil.Strings;

{ TestePutBit }

procedure TestePutBit.LigaEDesligaBitsDeZeroASete;
var
  AByte: Integer;
begin
  AByte := 0;
  PutBit(AByte, 0, True);
  CheckEquals(AByte, 1);     // 0000 0001
  PutBit(AByte, 1, True);
  CheckEquals(AByte, 3);     // 0000 0011
  PutBit(AByte, 2, True);
  CheckEquals(AByte, 7);     // 0000 0111
  PutBit(AByte, 3, True);
  CheckEquals(AByte, 15);    // 0000 1111
  PutBit(AByte, 4, True);
  CheckEquals(AByte, 31);    // 0001 1111
  PutBit(AByte, 5, True);
  CheckEquals(AByte, 63);    // 0011 1111
  PutBit(AByte, 6, True);
  CheckEquals(AByte, 127);   // 0111 1111
  PutBit(AByte, 7, True);
  CheckEquals(AByte, 255);   // 1111 1111
  PutBit(AByte, 0, False);
  CheckEquals(AByte, 254);   // 1111 1110
  PutBit(AByte, 1, False);
  CheckEquals(AByte, 252);   // 1111 1100
  PutBit(AByte, 2, False);
  CheckEquals(AByte, 248);   // 1111 1000
  PutBit(AByte, 3, False);
  CheckEquals(AByte, 240);   // 1111 0000
  PutBit(AByte, 4, False);
  CheckEquals(AByte, 224);   // 1110 0000
  PutBit(AByte, 5, False);
  CheckEquals(AByte, 192);   // 1100 0000
  PutBit(AByte, 6, False);
  CheckEquals(AByte, 128);   // 1000 0000
  PutBit(AByte, 7, False);
  CheckEquals(AByte, 0);     // 0000 0000
end;

{ TesteClearBit }

procedure TesteClearBit.SetUp;
begin
  inherited SetUp;
  AByte := 255;
end;

procedure TesteClearBit.DesligaBitsDeZeroASete;
begin
  ClearBit(AByte, 0);
  CheckEquals(AByte, 254);   // 1111 1110
  ClearBit(AByte, 1);
  CheckEquals(AByte, 252);   // 1111 1100
  ClearBit(AByte, 2);
  CheckEquals(AByte, 248);   // 1111 1000
  ClearBit(AByte, 3);
  CheckEquals(AByte, 240);   // 1111 0000
  ClearBit(AByte, 4);
  CheckEquals(AByte, 224);   // 1110 0000
  ClearBit(AByte, 5);
  CheckEquals(AByte, 192);   // 1100 0000
  ClearBit(AByte, 6);
  CheckEquals(AByte, 128);   // 1000 0000
  ClearBit(AByte, 7);
  CheckEquals(AByte, 0);     // 0000 0000
end;

procedure TesteClearBit.DesligaBitsPares;
begin
  // 1010 1010
  ClearBit(AByte, 0);
  ClearBit(AByte, 2);
  ClearBit(AByte, 4);
  ClearBit(AByte, 6);
  CheckEquals(AByte, 170);
end;

procedure TesteClearBit.DesligaBitsImpares;
begin
  // 0101 0101
  ClearBit(AByte, 1);
  ClearBit(AByte, 3);
  ClearBit(AByte, 5);
  ClearBit(AByte, 7);
  CheckEquals(AByte, 85);
end;

procedure TesteClearBit.DesligaTodosOsBitsDeUmByte;
begin
  // 0000 0000
  ClearBit(AByte, 0);
  ClearBit(AByte, 1);
  ClearBit(AByte, 2);
  ClearBit(AByte, 3);
  ClearBit(AByte, 4);
  ClearBit(AByte, 5);
  ClearBit(AByte, 6);
  ClearBit(AByte, 7);
  CheckEquals(AByte, 0);
end;

{ TesteSetBit }

procedure TesteSetBit.SetUp;
begin
  inherited SetUp;
  AByte := 0;
end;

procedure TesteSetBit.LigaBitsDeZeroASete;
begin
  SetBit(AByte, 0);
  CheckEquals(AByte, 1);    // 0000 0001
  SetBit(AByte, 1);
  CheckEquals(AByte, 3);    // 0000 0011
  SetBit(AByte, 2);
  CheckEquals(AByte, 7);    // 0000 0111
  SetBit(AByte, 3);
  CheckEquals(AByte, 15);   // 0000 1111
  SetBit(AByte, 4);
  CheckEquals(AByte, 31);   // 0001 1111
  SetBit(AByte, 5);
  CheckEquals(AByte, 63);   // 0011 1111
  SetBit(AByte, 6);
  CheckEquals(AByte, 127);  // 0111 1111
  SetBit(AByte, 7);
  CheckEquals(AByte, 255);  // 1111 1111
end;

procedure TesteSetBit.LigaBitsPares;
begin
  // 0101 0101
  SetBit(AByte, 0);
  SetBit(AByte, 2);
  SetBit(AByte, 4);
  SetBit(AByte, 6);
  CheckEquals(AByte, 85);
end;

procedure TesteSetBit.LigaBitsImpares;
begin
  // 1010 1010
  SetBit(AByte, 1);
  SetBit(AByte, 3);
  SetBit(AByte, 5);
  SetBit(AByte, 7);
  CheckEquals(AByte, 170);
end;

procedure TesteSetBit.LigaTodosOsBitsDeUmByte;
begin
  // 1111 1111
  SetBit(AByte, 0);
  SetBit(AByte, 1);
  SetBit(AByte, 2);
  SetBit(AByte, 3);
  SetBit(AByte, 4);
  SetBit(AByte, 5);
  SetBit(AByte, 6);
  SetBit(AByte, 7);
  CheckEquals(AByte, 255);
end;

{ ZipUnzip }

procedure ZipUnzip.SetUp;
var
  I: Integer;
  Linha: AnsiString;
begin
  Linha := '';
  fTituloACBr := 'Projeto ACBr - A MAIOR COMUNIDADE DE AUTOMACAO COMERCIAL DO BRASIL - http://www.projetoacbr.com.br/';

  // Valores obtidos em: http://www.txtwizard.net/compression
  fTituloCompressDeflateAndEncoded64 := 'eJwljEEKgCAUBa/yL5Du2z21hZD9sDxARRBBKCJ4/YxgVgMzc473WSJBq0wdgRwse9LswmQNzEANhJUdNPjzg9cWIxkm5bHYsVVXKamXstYq0v/bjj2LIz5iz/IFhyIdrg==';
  {$IfNDef USE_ZLibExGZ}
  fTituloCompressGZipAndEncoded64 := 'H4sIAAAAAAAA/yWMQQqAIBQFr/IvkO7bPbWFkP2wPEBFEEEoInj9jGBWAzNzjvdZIkGrTB2BHCx70uzCZA3MQA2ElR00+POD1xYjGSblsdixVVcpqZey1irS/9uOPYsjPmLP8gVzteS/YwAAAA==';
  {$Else}
  fTituloCompressGZipAndEncoded64 := 'H4sIAAAAAAAAACWMQQqAIBQFr/IvkO7bPbWFkP2wPEBFEEEoInj9jGBWAzNzjvdZIkGrTB2BHCx70uzCZA3MQA2ElR00+POD1xYjGSblsdixVVcpqZey1irS/9uOPYsjPmLP8gVzteS/YwAAAA==';
  {$EndIf}

  For I := 0 to 255 do
    Linha := Linha + AnsiChr(i);

  fStr := '';
  For I := 1 to 1000 do
    fStr := fStr + Linha + sLineBreak;
end;

procedure ZipUnzip.DeflateCompressDecompressAString;
var
  AZipStr, AUnZipStr: AnsiString;
  L1, L2: Integer;
begin
  AZipStr   := Zip(fStr);
  AUnZipStr := UnZip(AZipStr);
  L1 := length(fStr);
  L2 := length(AUnZipStr);
  CheckEquals(L1, L2);
  CheckEquals(AUnZipStr, fStr);
end;

procedure ZipUnzip.GZipCompressDecompressAsString;
var
  AZipStr, AUnZipStr: AnsiString;
  L1, L2: Integer;
begin
  AZipStr   := GZipCompress(fStr);
  AUnZipStr := UnZip(AZipStr);
  L1 := length(fStr);
  L2 := length(AUnZipStr);
  CheckEquals(L1, L2);
  CheckEquals(AUnZipStr, fStr);
end;

procedure ZipUnzip.GZipCompressDecompressAsStream;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(fStr);
  try
    CheckEquals(UnZip(GZipCompress(SS)), fStr);
  finally
    SS.Free;
  end;
end;

procedure ZipUnzip.DeflateCompressDecompressAStream;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(fStr);
  try
    CheckEquals(UnZip(Zip(SS)), fStr);
  finally
    SS.Free;
  end;
end;

procedure ZipUnzip.DeflateCompressAndEncode64;
var
  ResultEncoded: String;
begin
  // http://www.txtwizard.net/compression   , Deflate
  ResultEncoded := EncodeBase64(Zip(fTituloACBr));

  CheckEquals(ResultEncoded, fTituloCompressDeflateAndEncoded64);
end;

procedure ZipUnzip.Decode64AndDeCompressDeflate;
var
  ResultDeCompress: AnsiString;
begin
  // http://www.txtwizard.net/compression   , Deflate
  ResultDeCompress := UnZip( DecodeBase64(fTituloCompressDeflateAndEncoded64) );

  CheckEquals(ResultDeCompress, fTituloACBr);
end;

procedure ZipUnzip.GZipCompressAndEncode64;
var
  ResultEncoded: String;
begin
  // http://www.txtwizard.net/compression   , Deflate
  ResultEncoded := EncodeBase64(GZipCompress(fTituloACBr));

  CheckEquals(ResultEncoded, fTituloCompressGZipAndEncoded64);
end;

procedure ZipUnzip.ZipFileCompressDecompressAString;
var
  AZipStr, AUnZipStr: AnsiString;
  L1, L2: Integer;
begin
  AZipStr   := ZipFileCompress(fStr);
  AUnZipStr := UnZip(AZipStr);
  L1 := length(fStr);
  L2 := length(AUnZipStr);
  CheckEquals(L1, L2);
  CheckEquals(AUnZipStr, fStr);
end;

procedure ZipUnzip.Decode64AndDeCompressGZip;
var
  ResultDeCompress: AnsiString;
begin
  // http://www.txtwizard.net/compression   , Deflate
  ResultDeCompress := UnZip( DecodeBase64(fTituloCompressGZipAndEncoded64) );

  CheckEquals(ResultDeCompress, fTituloACBr);
end;

{ ComparaValorTest }

procedure ComparaValorTest.ValorUmMenorTolerancia001Igual;
begin
  CheckEquals( 0, ComparaValor(100, 100.01, 0.01));
end;

procedure ComparaValorTest.ValorUmMenorTolerancia0001Menor;
begin
  CheckEquals( -1, ComparaValor(100, 100.02, 0.01));
end;

procedure ComparaValorTest.ValorUmMaiorTolerancia0001Maior;
begin
  CheckEquals( 1, ComparaValor(100.02, 100, 0.01));
end;

procedure ComparaValorTest.ValorUmMenorTolerancia00001Igual;
begin
  CheckEquals( 0, ComparaValor(100, 100.0001, 0.0001));
end;

procedure ComparaValorTest.ValorUmMenorTolerancia000001Menor;
begin
  CheckEquals( -1, ComparaValor(100, 100.0002, 0.0001));
end;

procedure ComparaValorTest.ValorUmMaiorTolerancia000001Maior;
begin
  CheckEquals( 1, ComparaValor(100.0002, 100, 0.0001));
end;

{ EAN13Test }

procedure EAN13Test.Valido;
begin
  CheckEquals( '8', EAN13_DV('123456789012'));
  CheckEquals( '5', EAN13_DV('789835741001'));

  CheckTrue(EAN13Valido('2004700001341') );
  CheckTrue(EAN13Valido('1234567890128') );
  CheckTrue(EAN13Valido('7898357410015') );
end;

procedure EAN13Test.TamanhoMaior;
begin
   CheckFalse(EAN13Valido('78983574100156'));
end;

procedure EAN13Test.TamanhoMenor;
begin
  CheckFalse(EAN13Valido('789835741001'));
end;

procedure EAN13Test.DigitoInvalido;
begin
  CheckFalse(EAN13Valido('7898357410010'));
  CheckFalse(EAN13Valido('1234567890129'));
end;

procedure EAN13Test.ComLetras;
begin
  CheckFalse(EAN13Valido('A89835741001D'));
end;

procedure EAN13Test.EAN13Valido_String0000000000000_RetornaTrue;
begin
  CheckTrue(EAN13Valido('0000000000000'));
end;

procedure EAN13Test.EAN13Valido_String2000100002629_RetornaTrue;
begin
  CheckTrue(EAN13Valido('2000100002629'));
end;

procedure EAN13Test.EAN13Valido_String7506195185568_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7506195185568'));
end;

procedure EAN13Test.EAN13Valido_String7891060886139_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7891060886139'));
end;

procedure EAN13Test.EAN13Valido_String7893946087173_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7893946087173'));
end;

procedure EAN13Test.EAN13Valido_String7896232517828_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7896232517828'));
end;

procedure EAN13Test.EAN13Valido_String7896645900026_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7896645900026'));
end;

procedure EAN13Test.EAN13Valido_String7897186015095_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7897186015095'));
end;

procedure EAN13Test.EAN13Valido_String7898132132019_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7898132132019'));

end;

procedure EAN13Test.EAN13Valido_String7898908141016_RetornaTrue;
begin
  CheckTrue(EAN13Valido('7898908141016'));
end;

procedure EAN13Test.EAN13Valido_StringInvalida_RetornaFalso;
begin
  CheckFalse(EAN13Valido('abcdefghijklm'));
end;

procedure EAN13Test.EAN13Valido_StringValida_RetornaTrue;
begin
  CheckTrue(EAN13Valido('1234567890128'));
end;

procedure EAN13Test.EAN13_DV_Codigo000000000000_Retorna0;
begin
  CheckEquals('0', EAN13_DV('000000000000'));
end;

procedure EAN13Test.EAN13_DV_Codigo200010000262_Retorna9;
begin
  CheckEquals('9', EAN13_DV('200010000262'));
end;

procedure EAN13Test.EAN13_DV_Codigo750619518556_Retorna8;
begin
  CheckEquals('8', EAN13_DV('750619518556'));
end;

procedure EAN13Test.EAN13_DV_Codigo789106088613_Retorna9;
begin
  CheckEquals('9', EAN13_DV('789106088613'));
end;

procedure EAN13Test.EAN13_DV_Codigo789394608717_Retorna3;
begin
  CheckEquals('3', EAN13_DV('789394608717'));
end;

procedure EAN13Test.EAN13_DV_Codigo789623251782_Retorna8;
begin
  CheckEquals('8', EAN13_DV('789623251782'));
end;

procedure EAN13Test.EAN13_DV_Codigo789664590002_Retorna6;
begin
  CheckEquals('6', EAN13_DV('789664590002'));
end;

procedure EAN13Test.EAN13_DV_Codigo789718601509_Retorna5;
begin
  CheckEquals('5', EAN13_DV('789718601509'));
end;

procedure EAN13Test.EAN13_DV_Codigo789813213201_Retorna9;
begin
  CheckEquals('9', EAN13_DV('789813213201'));
end;

procedure EAN13Test.EAN13_DV_Codigo789890814101_Retorna6;
begin
  CheckEquals('6', EAN13_DV('789890814101'));
end;

procedure EAN13Test.EAN13_DV_StringAlphanumerica_RetornaVazio;
begin
  CheckEquals('', EAN13_DV('1234567890ab'));
end;

{ TranslateUnprintableTest }

procedure TranslateUnprintableTest.CaracteresNormais;
var
  AStr: String;
  I: Integer;
begin
  AStr := 'Projeto ACBr. - ';
  For I := 32 to 126 do
    AStr := AStr + chr(I);

  CheckEquals(AStr, TranslateUnprintable(AStr));
end;

procedure TranslateUnprintableTest.CaracteresDeControle;
var
  Resp : String;
begin
  Resp := TranslateUnprintable(NUL+SOH+STX+ETX+ENQ+ACK+TAB+BS+LF+FF+CR+WAK+NAK+ESC+FS+GS);
  CheckEquals('[NUL][SOH][STX][ETX][ENQ][ACK][TAB][BS][LF][FF][CR][WAK][NAK][ESC][FS][GS]', Resp);
end;

procedure TranslateUnprintableTest.Ambos;
var
  Resp: String;
begin
  Resp := TranslateUnprintable(STX+'Projeto ACBr.'+ETX+CR+LF);
  CheckEquals('[STX]Projeto ACBr.[ETX][CR][LF]', Resp);
end;

{ PathWithoutDelimTest }

procedure PathWithoutDelimTest.SemDelimiter;
begin
  CheckEquals('c:\temp', PathWithoutDelim('c:\temp'));
end;

procedure PathWithoutDelimTest.ComDelimiter;
begin
  CheckEquals('c:\temp', PathWithoutDelim('c:\temp\'));
end;

{ PathWithDelimTest }

procedure PathWithDelimTest.SemDelimiter;
begin
  CheckEquals('c:\temp\', PathWithDelim('c:\temp'));
end;

procedure PathWithDelimTest.ComDelimiter;
begin
  CheckEquals('c:\temp\', PathWithDelim('c:\temp\'));
end;

{ StringCrc16Test }

procedure StringCrc16Test.Computar;
begin
  //O cast para Cardinal È por conta do Delphi 7 (usando DUnit)
  CheckEquals(Cardinal(47933),StringCrc16('123456789'));
  CheckEquals(Cardinal(14809),StringCrc16('987654321'));
  CheckEquals(Cardinal(28843),StringCrc16('Projeto ACBr'));
  CheckEquals(Cardinal(59551),StringCrc16('ACBr Projeto'));
end;

{ SomaAscIITest }

procedure SomaAscIITest.Computar;
var
  Resp: Integer;
begin
  Resp := SomaAscII('Projeto ACBr. 123456 -=- ()[]');
  CheckEquals(1950,Resp);

  CheckEquals(294, SomaAscII('abc'));
  CheckEquals(294, SomaAscII('cba'));
end;

{ StrCryptTest }

procedure StrCryptTest.CryptDecrypt;
var
  AStr, Chave, Resp: String;
begin
  Chave := 'aRFD353+-k.as';
  AStr := 'Projeto ACBr. Teste Unit·rio';
  Resp := StrCrypt(AStr,Chave);

  CheckNotEquals(AStr, Resp);
  CheckEquals( AStr, StrCrypt(Resp, Chave) );
end;

procedure StrCryptTest.CryptStringVazia;
begin
  CheckEquals( '', StrCrypt('', '123') );
end;

procedure StrCryptTest.CryptChaveVazia;
begin
  CheckEquals( '123', StrCrypt('123', '') );
end;

procedure StrCryptTest.CryptStringEChaveVazia;
begin
  CheckEquals( '', StrCrypt('', '') );
end;

{ AscToStringTest }

procedure AscToStringTest.Normal;
var
  Resp: AnsiString;
begin
  Resp := AscToString('#13,#10,#255,#65,#150');
  CheckEquals( #13+#10+#255+#65+#150, Resp );
end;

procedure AscToStringTest.ComLetras;
var
  Resp: String;
begin
  Resp := AscToString('#13,A,#10,1,#255,B,#65,9,A,C,B,r,#150');
  CheckEquals( #13+'A'+#10+'1'+#255+'BA'+'9ACBr'+#150,  Resp );
end;

procedure AscToStringTest.ComValoresInvalidos;
var
  Resp: String;
begin
  Resp := AscToString('#13,A,#10,1,#255,B,#65,9,A,C,B,r,#150,#DN');
  CheckEquals( #13+'A'+#10+'1'+#255+'BA'+'9ACBr'+#150+'#DN',  Resp );
end;

{ StringToAscTest }

procedure StringToAscTest.Normal;
var
  Resp: String;
  AnsiStr: AnsiString;
begin
  AnsiStr := #13+#10+#255+#65+#150;
  Resp := StringToAsc(AnsiStr);
  CheckEquals('#13,#10,#255,#65,#150', Resp );
end;

{ TraduzComandoTest }

procedure TraduzComandoTest.ComPipeEComentario;
var
  Resp: String;
begin
  Resp := TraduzComando('#13,v,#10 | Ignorar');
  CheckEquals(chr(13)+'v'+chr(10), Resp );
end;

procedure TraduzComandoTest.ComPipeApenas;
var
  Resp: String;
begin
  Resp := TraduzComando('#13,v,#10 | ');
  CheckEquals(chr(13)+'v'+chr(10), Resp );
end;

procedure TraduzComandoTest.SemPipe;
var
  Resp: String;
begin
  Resp := TraduzComando('#13,v,#10');
  CheckEquals(chr(13)+'v'+chr(10), Resp );
end;


{ TamanhoMenorTest }

procedure TamanhoMenorTest.Menor;
begin
  CheckTrue( TamanhoMenor('ABC',5) );
end;

procedure TamanhoMenorTest.Maior;
begin
  CheckFalse( TamanhoMenor('ABCDEF',5) );
end;

procedure TamanhoMenorTest.Igual;
begin
  CheckFalse( TamanhoMenor('ABCDE',5) );
end;

{ TamanhoIgualTest }

procedure TamanhoIgualTest.Menor;
begin
  CheckFalse( TamanhoIgual('ABC',5) );
  CheckFalse( TamanhoIgual(123,5) );
end;

procedure TamanhoIgualTest.Maior;
begin
  CheckFalse( TamanhoIgual('ABCDEF',5) );
  CheckFalse( TamanhoIgual(123456,5) );
end;

procedure TamanhoIgualTest.Igual;
begin
  CheckTrue( TamanhoIgual('ABCDE',5) );
  CheckTrue( TamanhoIgual(12345,5) );
end;

{ EstaZerado_NaoEstaZeradoTest }

procedure EstaZerado_NaoEstaZeradoTest.Zerado;
var
  AInt : Integer;
  ADbl : Double;
begin
  AInt := 0;
  ADbl := 0;

  CheckTrue( EstaZerado(AInt) );
  CheckTrue( EstaZerado(ADbl) );

  CheckFalse( NaoEstaZerado(AInt) );
  CheckFalse( NaoEstaZerado(ADbl) );
end;

procedure EstaZerado_NaoEstaZeradoTest.NaoZerado;
var
  AInt : Integer;
  ADbl : Double;
begin
  AInt := 1;
  ADbl := 1.1;

  CheckFalse( EstaZerado(AInt) );
  CheckFalse( EstaZerado(ADbl) );

  CheckTrue( NaoEstaZerado(AInt) );
  CheckTrue( NaoEstaZerado(ADbl) );
end;

procedure EstaZerado_NaoEstaZeradoTest.Negativo;
var
  AInt : Integer;
  ADbl : Double;
begin
  AInt := -1;
  ADbl := -1.1;

  CheckFalse( EstaZerado(AInt) );
  CheckFalse( EstaZerado(ADbl) );

  CheckTrue( NaoEstaZerado(AInt) );
  CheckTrue( NaoEstaZerado(ADbl) );
end;

{ EstaVazio_NaoEstaVazioTest }

procedure EstaVazio_NaoEstaVazioTest.Vazio;
var
  AStr: String;
begin
  AStr := '';
  CheckTrue( EstaVazio(AStr) );
  CheckFalse( NaoEstaVazio(AStr) );
end;

procedure EstaVazio_NaoEstaVazioTest.NaoVazio;
var
  AStr: String;
begin
  AStr := 'ACBr';
  CheckFalse( EstaVazio(AStr) );
  CheckTrue( NaoEstaVazio(AStr) );
end;

procedure EstaVazio_NaoEstaVazioTest.ComEspacos;
var
  AStr: Char;
begin
  AStr := ' ';
  CheckFalse( EstaVazio(AStr) );
  CheckTrue( NaoEstaVazio(AStr) );
end;

{ FloatMaskTest }

procedure FloatMaskTest.Inteiro;
begin
  CheckEquals('0',FloatMask(0));
end;

procedure FloatMaskTest.DuasCasas_Com_Separador_de_Milhar;
begin
  CheckEquals(',0.00',FloatMask(2));
end;

procedure FloatMaskTest.QuatroCasas_Com_Separador_de_Milhar;
begin
  CheckEquals(',0.0000',FloatMask(4));
end;

procedure FloatMaskTest.DuasCasas_Sem_Separador_de_Milhar;
begin
  CheckEquals('0.00',FloatMask(2, False));
end;

procedure FloatMaskTest.QuatroCasas_Sem_Separador_de_Milhar;
begin
  CheckEquals('0.0000',FloatMask(4, False));
end;

{ FormatFloatBrTest }

procedure FormatFloatBrTest.Normal;
begin
  CheckEquals('115,89', FormatFloatBr(115.89));
end;

procedure FormatFloatBrTest.ComDecimaisZerados;
begin
  CheckEquals('115,00', FormatFloatBr(115.00));
end;

procedure FormatFloatBrTest.ComFormatacao;
begin
  CheckEquals('115,8900', FormatFloatBr(115.89, '0.0000'));
end;

procedure FormatFloatBrTest.ComSerparadorDeMilhar;
begin
  CheckEquals('123.456,789', FormatFloatBr(123456.789, '###,000.000'));
end;

function Mascara(ADec : Integer) : string;
begin
  result :=  format(sDisplayFormat,[ADec,0]);
end;

procedure FormatFloatBrTest.ComMascaraDisplayFormat026601x5;
begin
  CheckEquals('0,26601', FormatFloatBr(0.26601, Mascara(5)));
end;

procedure FormatFloatBrTest.ComMascaramsk15x2;
begin
  CheckEquals('35.015.010,12', FormatFloatBr(msk15x2, 35015010.12));
end;

procedure FormatFloatBrTest.ComMascaraDisplayFormat026660x4;
begin
  CheckEquals('0,2660', FormatFloatBr(0.266, Mascara(4)));
end;

procedure FormatFloatBrTest.ComMascaraDisplayFormat02666x3;
begin
  CheckEquals('0,266', FormatFloatBr(0.266, Mascara(3)));
end;

procedure FormatFloatBrTest.ComMascaraDisplayFormat026x2;
begin
  CheckNotEquals('0,26', FormatFloatBr(0.266, Mascara(2)));
end;

procedure FormatFloatBrTest.ComMascaraDisplayFormat027x2;
begin
  CheckEquals('0,27', FormatFloatBr(0.266, Mascara(2)));
end;

{ StringToBinaryStringTest }

procedure StringToBinaryStringTest.Simples;
begin
  CheckEquals(chr(1)+'ABC'+chr(255)+'123', StringToBinaryString('\x01ABC\xFF123'));
end;

procedure StringToBinaryStringTest.Reverso;
Var
  AllChars: AnsiString;
  I: Integer;
  Resp: String;
begin
  AllChars := '';
  Resp := '';
  For I := 0 to 31 do
  begin
    AllChars := AllChars + chr(I) ;
    Resp := Resp + '\x'+Trim(IntToHex(I,2)) ;
  end;

  CheckTrue(AllChars = StringToBinaryString(Resp) ) ;
end;

procedure StringToBinaryStringTest.NaoHexa;
begin
  CheckEquals('\xml\xA\\A\ab', StringToBinaryString('\xml\xA\\\x41\ab'));
end;

procedure StringToBinaryStringTest.ComNulos;
begin
  CheckEquals(chr(0)+chr(13)+chr(10)+chr(0)+'ABC'+chr(0)+chr(13), StringToBinaryString('\x00\x0D\x0A\x00ABC\x00\x0D'));
end;

{ BinaryStringToStringTest }

procedure BinaryStringToStringTest.Simples;
begin
  CheckEquals('\x01ABC\xFF123', BinaryStringToString(chr(1)+'ABC'+chr(255)+'123'));
end;

procedure BinaryStringToStringTest.Reverso;
Var
  AllChars: AnsiString;
  I: Integer;
  Resp: String;
begin
  AllChars := '';
  Resp := '';
  For I := 0 to 31 do
  begin
    AllChars := AllChars + chr(I) ;
    Resp := Resp + '\x'+Trim(IntToHex(I,2)) ;
  end;

  CheckEquals(Resp, BinaryStringToString(AllChars) ) ;
end;

procedure BinaryStringToStringTest.ComNulos;
begin
  CheckEquals('\x00\x0D\x0A\x00ABC\x00\x0D', BinaryStringToString(chr(0)+chr(13)+chr(10)+chr(0)+'ABC'+chr(0)+chr(13)));
end;

{ AsciiToHexTest }

procedure AsciiToHexTest.Simples;
begin
  CheckEquals('41424344', AsciiToHex('ABCD') );
end;

procedure AsciiToHexTest.Comlexo;
begin
  CheckEquals('312C4D6FFF', AsciiToHex('1,Mo'+chr(255)) );
end;

{ HexToAsciiTest }

procedure HexToAsciiTest.Simples;
begin
  CheckEquals('ABCD', HexToAscii('41424344') );
end;

procedure HexToAsciiTest.Comlexo;
begin
  CheckEquals('1,Mo'+chr(255), HexToAscii('312C4D6FFF') );
end;

{ LEStrToIntTest }

procedure LEStrToIntTest.TamanhoUm;
begin
  CheckEquals( 255, LEStrToInt(chr(255)) );
end;

procedure LEStrToIntTest.TamanhoDois;
begin
  CheckEquals( 106 , LEStrToInt(chr(106) + chr(00)) );
end;

procedure LEStrToIntTest.TamanhoQuatro;
var
  LEStr: AnsiString;
begin
  LEStr := IntToLEStr(1056,4);

  CheckEquals( 1056, LEStrToInt(LEStr) );
  CheckEquals( 1056, LEStrToInt(chr(32) + chr(04) + chr(00) + chr(00)) );
  CheckEquals( AsciiToHex(LEStr), AsciiToHex(chr(32) + chr(04) + chr(00) + chr(00)));
end;

{ IntToLEStrTest }

procedure IntToLEStrTest.TamanhoUm;
begin
  CheckEquals( chr(255), IntToLEStr(255,1) );
end;

procedure IntToLEStrTest.TamanhoDois;
begin
  CheckEquals( chr(106) + chr(00), IntToLEStr(106,2) );
end;

procedure IntToLEStrTest.TamanhoQuatro;
var
  LEStr: AnsiString;
begin
  LEStr := IntToLEStr(1056,4);
  // CheckEquals simples falharia pois a String inicia com '0'
  CheckTrue( LEStr = chr(32) + chr(04) + chr(00) + chr(00));
end;

{ AscToBcdTest }

procedure AscToBcdTest.TamanhoTres;
var
  BCD: String;
begin
  BCD := AscToBcd('123456',3);
  CheckEquals(chr(18) + chr(52) + chr(86), BCD );
  CheckEquals('123456',BcdToAsc(BCD));
end;

procedure AscToBcdTest.TruncandoComTamanhoDois;
begin
  CheckEquals(chr(18) + chr(52), AscToBcd('123456',2));
end;

procedure AscToBcdTest.AumentandoComtamanhoQuatro;
begin
  CheckEquals(chr(00) + chr(18) + chr(52) + chr(86), AscToBcd('123456',4));
end;

{ BcdToAscTest }

procedure BcdToAscTest.Normal;
begin
  CheckEquals('123456', BcdToAsc(chr(18) + chr(52) + chr(86)));
end;

procedure BcdToAscTest.ComZerosAEsquerda;
begin
  CheckEquals('00000000123456', BcdToAsc(chr(00)+chr(00)+chr(00)+chr(00)+chr(18)+chr(52)+chr(86)));
end;

{ BinToIntTest }

procedure BinToIntTest.TestarTodosBits;
begin
  CheckEquals(0, BinToInt('00000000') );
  CheckEquals(0, BinToInt('0000') );
  CheckEquals(0, BinToInt('0') );
  CheckEquals(1, BinToInt('0001') );
  CheckEquals(1, BinToInt('00000001') );
  CheckEquals(1, BinToInt('1') );
  CheckEquals(4, BinToInt('0100') );
  CheckEquals(15, BinToInt('1111') );
  CheckEquals(100, BinToInt('01100100') );
  CheckEquals(255, BinToInt('11111111') );
end;

{ IntToBinTest }

procedure IntToBinTest.TestarTodosBits;
begin
  CheckEquals('0001', IntToBin(1,4) );
  CheckEquals('0100', IntToBin(4,4) );
  CheckEquals('1111', IntToBin(15,4) );
  CheckEquals('00000000', IntToBin(0,8) );
  CheckEquals('01100100', IntToBin(100,8) );
  CheckEquals('11111111', IntToBin(255,8) );
end;

{ TestBitTest }

procedure TestBitTest.TestarTodosBits;
begin
  CheckTrue( TestBit(1,0));
  CheckTrue( TestBit(2,1));
  CheckTrue( TestBit(3,0));
  CheckTrue( TestBit(3,1));
  CheckTrue( TestBit(4,2));
  CheckTrue( TestBit(5,0));
  CheckTrue( TestBit(5,2));
  CheckTrue( TestBit(6,2));
  CheckTrue( TestBit(6,1));
  CheckTrue( TestBit(7,2));
  CheckTrue( TestBit(7,1));
  CheckTrue( TestBit(7,0));
  CheckTrue( TestBit(8,3));
  CheckTrue( TestBit(9,3));
  CheckTrue( TestBit(9,0));
  CheckTrue( TestBit(10,3));
  CheckTrue( TestBit(10,1));
  CheckTrue( TestBit(11,3));
  CheckTrue( TestBit(11,1));
  CheckTrue( TestBit(11,0));
  CheckTrue( TestBit(12,3));
  CheckTrue( TestBit(12,2));
  CheckTrue( TestBit(13,3));
  CheckTrue( TestBit(13,2));
  CheckTrue( TestBit(13,0));
  CheckTrue( TestBit(14,3));
  CheckTrue( TestBit(14,2));
  CheckTrue( TestBit(14,1));
  CheckTrue( TestBit(15,3));
  CheckTrue( TestBit(15,2));
  CheckTrue( TestBit(15,1));
  CheckTrue( TestBit(15,0));
  CheckTrue( TestBit(255,0));
  CheckTrue( TestBit(255,1));
  CheckTrue( TestBit(255,2));
  CheckTrue( TestBit(255,3));
  CheckTrue( TestBit(255,4));
  CheckTrue( TestBit(255,5));
  CheckTrue( TestBit(255,6));
  CheckTrue( TestBit(255,7));
end;

{ RoundABNTTest }

procedure RoundABNTTest.AsIntegerImpar;
var
  AInt: Integer;
begin
  AInt := 5;
  CheckEquals( AInt, RoundABNT(5.1, 0));
  CheckEquals( AInt, RoundABNT(5.2, 0));
  CheckEquals( AInt, RoundABNT(5.3, 0));
  CheckEquals( AInt, RoundABNT(5.4, 0));
  AInt := 6;
  CheckEquals( AInt, RoundABNT(5.5, 0));
  CheckEquals( AInt, RoundABNT(5.6, 0));
  CheckEquals( AInt, RoundABNT(5.7, 0));
  CheckEquals( AInt, RoundABNT(5.8, 0));
  CheckEquals( AInt, RoundABNT(5.9, 0));
end;

procedure RoundABNTTest.AsIntegerPar;
var
  AInt: Integer;
begin
  AInt := 4;
  CheckEquals( AInt, RoundABNT(4.1, 0));
  CheckEquals( AInt, RoundABNT(4.2, 0));
  CheckEquals( AInt, RoundABNT(4.3, 0));
  CheckEquals( AInt, RoundABNT(4.4, 0));
  CheckEquals( AInt, RoundABNT(4.5, 0));
  AInt := 5;
  CheckEquals( AInt, RoundABNT(4.501, 0));
  CheckEquals( AInt, RoundABNT(4.6, 0));
  CheckEquals( AInt, RoundABNT(4.7, 0));
  CheckEquals( AInt, RoundABNT(4.8, 0));
  CheckEquals( AInt, RoundABNT(4.9, 0));
end;

procedure RoundABNTTest.TresParaDuasCasasDecimais;
var
  AVal: Double;
begin
  AVal := 4.86;
  CheckEquals( AVal, RoundABNT(4.855, 2));
  AVal := 4.56;
  CheckEquals( AVal, RoundABNT(4.555, 2));
  AVal := 5.10;
  CheckEquals( AVal, RoundABNT(5.101, 2));
  CheckEquals( AVal, RoundABNT(5.102, 2));
  CheckEquals( AVal, RoundABNT(5.103, 2));
  CheckEquals( AVal, RoundABNT(5.104, 2));
  CheckEquals( AVal, RoundABNT(5.105, 2));
  AVal := 5.11;
  CheckEquals( AVal, RoundABNT(5.1050123, 2));
  CheckEquals( AVal, RoundABNT(5.106, 2));
  CheckEquals( AVal, RoundABNT(5.107, 2));
  CheckEquals( AVal, RoundABNT(5.108, 2));
  CheckEquals( AVal, RoundABNT(5.109, 2));
end;

procedure RoundABNTTest.QuatroParaDuasCasasDecimais;
var
  AVal: Double;
begin
  AVal := 5.10;
  CheckEquals( AVal, RoundABNT(5.1010, 2));
  CheckEquals( AVal, RoundABNT(5.1011, 2));
  CheckEquals( AVal, RoundABNT(5.1012, 2));
  CheckEquals( AVal, RoundABNT(5.1013, 2));
  CheckEquals( AVal, RoundABNT(5.1014, 2));
  CheckEquals( AVal, RoundABNT(5.1015, 2));
  CheckEquals( AVal, RoundABNT(5.1016, 2));
  CheckEquals( AVal, RoundABNT(5.1017, 2));
  CheckEquals( AVal, RoundABNT(5.1018, 2));
  CheckEquals( AVal, RoundABNT(5.1019, 2));

  CheckEquals( AVal, RoundABNT(5.1020, 2));
  CheckEquals( AVal, RoundABNT(5.1021, 2));
  CheckEquals( AVal, RoundABNT(5.1022, 2));
  CheckEquals( AVal, RoundABNT(5.1023, 2));
  CheckEquals( AVal, RoundABNT(5.1024, 2));
  CheckEquals( AVal, RoundABNT(5.1025, 2));
  CheckEquals( AVal, RoundABNT(5.1026, 2));
  CheckEquals( AVal, RoundABNT(5.1027, 2));
  CheckEquals( AVal, RoundABNT(5.1028, 2));
  CheckEquals( AVal, RoundABNT(5.1029, 2));

  CheckEquals( AVal, RoundABNT(5.1030, 2));
  CheckEquals( AVal, RoundABNT(5.1031, 2));
  CheckEquals( AVal, RoundABNT(5.1032, 2));
  CheckEquals( AVal, RoundABNT(5.1033, 2));
  CheckEquals( AVal, RoundABNT(5.1034, 2));
  CheckEquals( AVal, RoundABNT(5.1035, 2));
  CheckEquals( AVal, RoundABNT(5.1036, 2));
  CheckEquals( AVal, RoundABNT(5.1037, 2));
  CheckEquals( AVal, RoundABNT(5.1038, 2));
  CheckEquals( AVal, RoundABNT(5.1039, 2));

  CheckEquals( AVal, RoundABNT(5.1040, 2));
  CheckEquals( AVal, RoundABNT(5.1041, 2));
  CheckEquals( AVal, RoundABNT(5.1042, 2));
  CheckEquals( AVal, RoundABNT(5.1043, 2));
  CheckEquals( AVal, RoundABNT(5.1044, 2));
  CheckEquals( AVal, RoundABNT(5.1045, 2));
  CheckEquals( AVal, RoundABNT(5.1046, 2));
  CheckEquals( AVal, RoundABNT(5.1047, 2));
  CheckEquals( AVal, RoundABNT(5.1048, 2));
  CheckEquals( AVal, RoundABNT(5.1049, 2));
  CheckEquals( AVal, RoundABNT(5.1050, 2));

  AVal := 5.11;
  CheckEquals( AVal, RoundABNT(5.1051, 2));
  CheckEquals( AVal, RoundABNT(5.1052, 2));
  CheckEquals( AVal, RoundABNT(5.1053, 2));
  CheckEquals( AVal, RoundABNT(5.1054, 2));
  CheckEquals( AVal, RoundABNT(5.1055, 2));
  CheckEquals( AVal, RoundABNT(5.1056, 2));
  CheckEquals( AVal, RoundABNT(5.1057, 2));
  CheckEquals( AVal, RoundABNT(5.1058, 2));
  CheckEquals( AVal, RoundABNT(5.1059, 2));

  CheckEquals( AVal, RoundABNT(5.1060, 2));
  CheckEquals( AVal, RoundABNT(5.1061, 2));
  CheckEquals( AVal, RoundABNT(5.1062, 2));
  CheckEquals( AVal, RoundABNT(5.1063, 2));
  CheckEquals( AVal, RoundABNT(5.1064, 2));
  CheckEquals( AVal, RoundABNT(5.1065, 2));
  CheckEquals( AVal, RoundABNT(5.1066, 2));
  CheckEquals( AVal, RoundABNT(5.1067, 2));
  CheckEquals( AVal, RoundABNT(5.1068, 2));
  CheckEquals( AVal, RoundABNT(5.1069, 2));

  CheckEquals( AVal, RoundABNT(5.1070, 2));
  CheckEquals( AVal, RoundABNT(5.1071, 2));
  CheckEquals( AVal, RoundABNT(5.1072, 2));
  CheckEquals( AVal, RoundABNT(5.1073, 2));
  CheckEquals( AVal, RoundABNT(5.1074, 2));
  CheckEquals( AVal, RoundABNT(5.1075, 2));
  CheckEquals( AVal, RoundABNT(5.1076, 2));
  CheckEquals( AVal, RoundABNT(5.1077, 2));
  CheckEquals( AVal, RoundABNT(5.1078, 2));
  CheckEquals( AVal, RoundABNT(5.1079, 2));

  CheckEquals( AVal, RoundABNT(5.1080, 2));
  CheckEquals( AVal, RoundABNT(5.1081, 2));
  CheckEquals( AVal, RoundABNT(5.1082, 2));
  CheckEquals( AVal, RoundABNT(5.1083, 2));
  CheckEquals( AVal, RoundABNT(5.1084, 2));
  CheckEquals( AVal, RoundABNT(5.1085, 2));
  CheckEquals( AVal, RoundABNT(5.1086, 2));
  CheckEquals( AVal, RoundABNT(5.1087, 2));
  CheckEquals( AVal, RoundABNT(5.1088, 2));
  CheckEquals( AVal, RoundABNT(5.1089, 2));

  CheckEquals( AVal, RoundABNT(5.1090, 2));
  CheckEquals( AVal, RoundABNT(5.1091, 2));
  CheckEquals( AVal, RoundABNT(5.1092, 2));
  CheckEquals( AVal, RoundABNT(5.1093, 2));
  CheckEquals( AVal, RoundABNT(5.1094, 2));
  CheckEquals( AVal, RoundABNT(5.1095, 2));
  CheckEquals( AVal, RoundABNT(5.1096, 2));
  CheckEquals( AVal, RoundABNT(5.1097, 2));
  CheckEquals( AVal, RoundABNT(5.1098, 2));
  CheckEquals( AVal, RoundABNT(5.1099, 2));
end;

procedure RoundABNTTest.ExpressaoCurrDuasCasasDecimais;
  Function ToDouble(c:Currency):Double;
  begin
    Result := c;
  end;
var
  currVal, currValorUnit, currQtde, currTotal: Currency;
begin
  currValorUnit :=0.99;
  currQtde :=0.995;
  currTotal := currValorUnit * currQtde;

  // 0.99 x 0.995 = 0,98505, porÈm "currTotal" È um currency, que somente usa 4 casas decimais, portanto ser·: 0,9850
  // PorÈm, se GetRoundMode for "rmUp", ocorrer· arredondamento, quando o valor È atribuido ao Currency, ficando 0,9851

  if (GetRoundMode = rmUp) then
    currVal := 0.99
  else
    currVal := 0.98;
  CheckEquals(currVal , RoundABNT(currTotal, 2), 0.00001);

  //----------- casts implÌcitos...
  currVal := 0.99;
  //   RoundABNT tem um par‚metro do tipo "Double" isso pode fazer com que expressıes
  // mudem o valor passado para a RoundABNT dependendo do compilador.
  //   Isso È causado pela mudanÁa da ordem dos casts implÌcitos. Sendo assim,
  // n„o È um erro na RoundABNT.
  //   Por exemplo, O teste comentado abaixo falha quando o compilador È Win 64 bits mas passa no Win 32:
  //CheckEquals( currVal, RoundABNT(currValorUnit * currQtde, 2), 0.00001);
  //   O que acontece È que no Win 32, as vari·veis s„o convertidas para Double antes da multiplicaÁ„o e,
  // assim, todas as casas decimais ser„o utilizadas. J· no Win 64,
  // as vari·veis s„o multiplicadas como Currency e sÛ depois convertidas para Double.
  //   No Win 64 o teste fica como abaixo:
  //CheckEquals( currVal, RoundABNT((Double(currValorUnit) * Double(currQtde)), 2), 0.00001);
  //   Como o teste acima gera erro no FPC/Lazarus, usamos a rotina abaixo:
  CheckEquals( currVal, RoundABNT((ToDouble(currValorUnit) * ToDouble(currQtde)), 2), 0.00001);
end;

procedure RoundABNTTest.ExpressaoDblDuasCasasDecimais;
var
  dblVal, dblValorUnit, dblQtde, dblTotal: Double;
begin
  dblValorUnit :=0.99;
  dblQtde :=0.995;
  dblTotal := dblValorUnit * dblQtde;
  dblVal := 0.98;
  CheckEquals( dblVal, RoundABNT(0.9849, 2));
  CheckEquals( dblVal, RoundABNT(0.9850, 2));
  dblVal := 0.99;
  CheckEquals( dblVal, RoundABNT(0.98505, 2));
  CheckEquals( dblVal, RoundABNT(dblTotal, 2));
  CheckEquals( dblVal, RoundABNT(dblValorUnit * dblQtde, 2));
  dblValorUnit := 0.69;
  dblQtde := 28.50;
  dblTotal := dblValorUnit * dblQtde;
  CheckEquals( 19.66, RoundABNT(dblTotal, 2), 0.00001);
  dblValorUnit := 4.885;
  dblQtde := 1;
  dblTotal := dblValorUnit * dblQtde;
  CheckEquals( 4.88, RoundABNT(dblTotal, 2), 0.00001);

  dblTotal := 14.90 * 1.550;
  CheckEquals( 23.10, RoundABNT(dblTotal, 2), 0.00001);

  dblTotal := 1.5 * 40.13;
  CheckEquals( 60.20, RoundABNT(dblTotal, 2), 0.00001);

end;

procedure RoundABNTTest.TestesEstouro;
var
  extVal: Extended;
begin
  extVal := 12334234.46;
  CheckEquals( extVal, RoundABNT(12334234.4567567567567567567,-2), 0.00001 );
  extVal := 12334234.4568;
  CheckEquals( extVal, RoundABNT(12334234.4567567567567567567,-4), 0.00001 );
  extVal := 5233.456757;
  CheckEquals( extVal, RoundABNT(5233.4567567567567567567,-6), 0.00001 );
  extVal := 9999999999.46;
  CheckEquals( extVal, RoundABNT(9999999999.4567567567567567567,-2), 0.00001 );
  extVal := 9.4121295902;
  CheckEquals( extVal, RoundABNT(9.41212959024529,-10), 0.00001 );
end;

procedure RoundABNTTest.ValoresNegativos;
var
  AVal: Double;
begin
  AVal := -2;
  CheckEquals( AVal, RoundABNT(AVal, 0) );
  CheckEquals( AVal, RoundABNT(AVal, -1) );
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -0.94;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -0.95;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -0.96;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -0.97;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -0.98;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -0.99;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -1.94;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -1.95;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -1.96;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
  AVal := -1.97;
  CheckEquals( AVal, RoundABNT(AVal, -2) );
end;

procedure RoundABNTTest.DoctoABNTRegra2_1;
begin
  CheckEquals( 1.3, RoundABNT(1.3333, 1), 0.00001 );
end;

procedure RoundABNTTest.DoctoABNTRegra2_2;
begin
  CheckEquals( 1.7, RoundABNT(1.6666, 1), 0.00001 );
  CheckEquals( 4.9, RoundABNT(4.8505, 1), 0.00001 );
end;

procedure RoundABNTTest.DoctoABNTRegra2_3;
begin
  CheckEquals( 4.6, RoundABNT(4.5500, 1), 0.00001 );
end;

procedure RoundABNTTest.DoctoABNTRegra2_4;
begin
  CheckEquals( 4.8, RoundABNT(4.8500, 1), 0.00001 );
end;

{ TruncFixTest }

procedure TruncFixTest.AsExpression;
begin
  CheckEquals( 156, TruncFix( 1.602 * 0.98 * 100) );
  CheckEquals( 64, TruncFix( 5 * 12.991) );
  CheckEquals( 49, TruncFix( 2.09 * 23.5) );
end;

procedure TruncFixTest.AsDouble;
var
  ADouble: Double;
begin
  ADouble := 1.602 * 0.98 * 100;
  CheckEquals( 156, TruncFix( ADouble ) );
  ADouble := 5 * 12.991;
  CheckEquals( 64, TruncFix( ADouble ) );
  ADouble := 2.09 * 23.5;
  CheckEquals( 49, TruncFix( ADouble ) );
end;

procedure TruncFixTest.AsExtended;
var
  AExtended: Extended;
begin
  AExtended := 1.602 * 0.98 * 100;
  CheckEquals( 156, TruncFix( AExtended ) );
  AExtended := 5 * 12.991;
  CheckEquals( 64, TruncFix( AExtended ) );
  AExtended := 2.09 * 23.5;
  CheckEquals( 49, TruncFix( AExtended ) );
end;

procedure TruncFixTest.AsCurrency;
var
  ACurr: Currency;
begin
  ACurr := 1.602 * 0.98 * 100;
  CheckEquals( 156, TruncFix( ACurr ) );
  ACurr := 5 * 12.991;
  CheckEquals( 64, TruncFix( ACurr ) );
  ACurr := 2.09 * 23.5;
  CheckEquals( 49, TruncFix( ACurr ) );
end;

procedure TruncFixTest.AsLargeExtended;
var
  AExtended, AExtendedT: Extended;
begin
  AExtended  := 116529560.3123 * 100;
  AExtendedT := 11652956031;

  CheckEquals( AExtendedT, TruncFix( AExtended ) );
end;

procedure TruncFixTest.AsLargeDouble;
var
  ADouble: Double;
begin
  ADouble := 999999.99 * power(10, 2);

  CheckEquals( 99999999, TruncFix( ADouble ) );
end;

{ StrIsIPTest }

procedure StrIsIPTest.Normal;
begin
  CheckTrue(StrIsIP('192.168.0.1'));
  CheckTrue(StrIsIP('127.0.0.1'));
end;

procedure StrIsIPTest.NormalComZerosAEsquerda;
begin
  CheckTrue(StrIsIP('192.168.000.001'));
end;

procedure StrIsIPTest.ComNome;
begin
  CheckFalse(StrIsIP('hostname'));
end;

procedure StrIsIPTest.Errados;
begin
  CheckFalse(StrIsIP('19216801'));
  CheckFalse(StrIsIP('192168.0.1'));
  CheckFalse(StrIsIP('192.168'));
end;


{ StringToFloatDefTest }

procedure StringToFloatDefTest.ComVirgula;
begin
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 1));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.1));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.01));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.0001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.00001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.000001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.0000001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123,45', 0), 0.00000000001));
end;

procedure StringToFloatDefTest.ComPonto;
begin
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 1));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.1));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.01));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.0001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.00001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.000001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.0000001));
  CheckTrue(SameValue(123.45, StringToFloatDef('123.45', 0), 0.00000000001));
end;

procedure StringToFloatDefTest.ApenasInteiro;
begin
  CheckEquals(123, StringToFloatDef('123', 0));
end;

procedure StringToFloatDefTest.ValorDefault;
begin
  CheckEquals(1212.10, StringToFloatDef('12,12,1', 0), 0.001);
  CheckEquals(10, StringToFloatDef('ewerwt', 10));
end;

{ StringToFloatTest }

procedure StringToFloatTest.ComVirgula;
begin
  CheckEquals(123.45, StringToFloat('123,45'), 1);
  CheckEquals(123.45, StringToFloat('123,45'), 0.1);
  CheckEquals(123.45, StringToFloat('123,45'), 0.01);
  CheckEquals(123.45, StringToFloat('123,45'), 0.001);
  CheckEquals(123.45, StringToFloat('123,45'), 0.0001);
  CheckEquals(123.45, StringToFloat('123,45'), 0.00001);
  CheckEquals(123.45, StringToFloat('123,45'), 0.000001);
  CheckEquals(123.45, StringToFloat('123,45'), 0.0000001);
  CheckEquals(123.45, StringToFloat('123,45'), 0.0000000001);
end;

procedure StringToFloatTest.ComPonto;
begin
  CheckEquals(123.45, StringToFloat('123.45'), 1);
  CheckEquals(123.45, StringToFloat('123.45'), 0.1);
  CheckEquals(123.45, StringToFloat('123.45'), 0.01);
  CheckEquals(123.45, StringToFloat('123.45'), 0.001);
  CheckEquals(123.45, StringToFloat('123.45'), 0.0001);
  CheckEquals(123.45, StringToFloat('123.45'), 0.00001);
  CheckEquals(123.45, StringToFloat('123.45'), 0.000001);
  CheckEquals(123.45, StringToFloat('123.45'), 0.000000001);
end;

procedure StringToFloatTest.ApenasInteiro;
begin
  CheckEquals(123.00, StringToFloat('123'));
end;


{ FloatToStringTest }

procedure FloatToStringTest.Normal;
begin
  CheckEquals('115.89', FloatToString(115.89));
end;

procedure FloatToStringTest.ComDecimaisZerados;
begin
  CheckEquals('115', FloatToString(115.00));
end;

procedure FloatToStringTest.MudandoPontoDecimal;
begin
  CheckEquals('115,89', FloatToString(115.89, ','));
end;

procedure FloatToStringTest.ComFormatacao;
begin
  CheckEquals('115,890', FloatToString(115.89, ',', '0.000'));
end;

procedure FloatToStringTest.RemovendoSerparadorDeMilhar;
begin
  CheckEquals('123456.789', FloatToString(123456.789, '.', '###,000.000'));
end;

{ FloatToIntStrTest }

procedure FloatToIntStrTest.Normal;
begin
  CheckEquals('12345', FloatToIntStr(123.45));
end;

procedure FloatToIntStrTest.ValorDecimaisDefault;
begin
  CheckEquals('1234500', FloatToIntStr(12345));
end;

procedure FloatToIntStrTest.MudandoPadraoDeDecimais;
begin
  CheckEquals('12345000', FloatToIntStr(123.45, 5));
end;

procedure FloatToIntStrTest.SemDecimais;
begin
  CheckEquals('123', FloatToIntStr(123.453,0));
end;

procedure FloatToIntStrTest.ValorComMaisDecimais;
begin
  CheckEquals('12345', FloatToIntStr(123.453678,2));
end;

{ IntToStrZeroTest }

procedure IntToStrZeroTest.AdicionarZerosAoNumero;
begin
  CheckEquals('0000000123', IntToStrZero(123, 10));
end;

procedure IntToStrZeroTest.TamanhoMenorQueLimite;
begin
  CheckEquals('98', IntToStrZero(987, 2));
end;

{ CompareVersionsTest }

procedure CompareVersionsTest.VersaoIgual;
begin
   CheckEquals(0, CompareVersions('1.3.1' , '1.3.1'));
end;

procedure CompareVersionsTest.VersaoMaior;
begin
   CheckEquals(11, CompareVersions('1.3.4' , '1.2.1'));
end;

procedure CompareVersionsTest.VersaoMenor;
begin
   CheckEquals(-11, CompareVersions('1.2.1' , '1.3.4'));
end;

procedure CompareVersionsTest.TrocarDelimitador;
begin
   CheckEquals(-109, CompareVersions('1-4-9', '3-8-7', '-'));
end;

procedure CompareVersionsTest.NomeclaturaDiferente;
begin
  CheckEquals(-1, CompareVersions('1.0.3', '01.00.04'));
  CheckEquals(11, CompareVersions('1.2.5', '01.01.04'));
  CheckEquals(-11, CompareVersions('1.2.3', '01.10.5555'));
  CheckEquals(1, CompareVersions('8', '1'));
  CheckEquals(91, CompareVersions('2.3.7', '1.9'));
  CheckEquals(89110889, CompareVersions('2.0.0.9.8.6', '0.9.9.7.6.5.34.3.2'));
  CheckEquals(-1, CompareVersions('1.2a', '1.2d'));
end;

{ StripHTMLTest }

procedure StripHTMLTest.TesteSimples;
begin
  CheckEquals('Teste string em html', StripHTML('<br><b>Teste string em html</b><br>'));
end;

procedure StripHTMLTest.TesteCompleto;
begin
  CheckEquals('FPCUnit de TestesACBrUtil, Testes Unit·rios', StripHTML('<!DOCTYPE html>'
                           +'<html>'
                               +'<head>'
                                   +'FPCUnit de Testes'
                               +'</head>'
                               +'<body>'
                                   +'ACBrUtil, Testes Unit·rios'
                               +'</body>'
                           +'</html>'));
end;

procedure StripHTMLTest.TesteTagsInvalidas;
var
  SL: TStringList;
  Striped: String;
begin
  SL := TStringList.Create;
  try
    SL.Add('</zera>');
    SL.Add('<CE>*** TESTE DE TAGS INV¡LIDAS ***</CE>');
    SL.Add('<ce> <>tags inv·lidas no texto">">><<</CE>');
    SL.Add('<AD><da><ec></</A Direita</ad>');
    SL.Add('</corte_total>');

    Striped := StripHTML(SL.Text);

    SL.Clear;
    SL.Add('');
    SL.Add('*** TESTE DE TAGS INV¡LIDAS ***');
    SL.Add(' tags inv·lidas no texto">">><<');
    SL.Add('</</A Direita');
    SL.Add('');

    CheckEquals(SL.Text, Striped);
  finally
    SL.Free;
  end;
end;

procedure StripHTMLTest.TesteTagsInvertidas;
var
  AStr: String;
begin
  AStr := '>>> 1234 <<<';
  CheckEquals(AStr, StripHTML(AStr));
end;

{ SepararDadosTest }

procedure SepararDadosTest.Simples;
begin
  CheckEquals('Teste Simples', SeparaDados('<ACBr>Teste Simples</ACBr>', 'ACBr'));
  CheckEquals('Teste     Simples', SeparaDados('<ACBr>Teste     Simples</ACBr>', 'ACBr'));
  CheckEquals('TesteSimples', SeparaDados('<ACBr>TesteSimples</ACBr>', 'ACBr'));
end;

procedure SepararDadosTest.TextoLongo;
begin
  CheckEquals('ACBr Util', SeparaDados('<ACBrUtil>Teste com texto longo <b>ACBr Util</b> feito por DJSystem</ACBrUtil>', 'b'));
  CheckEquals('#ACBrUtil', SeparaDados('<ACBrUtil>Teste com texto longo <b>#ACBrUtil</b> feito por DJSystem</ACBrUtil>', 'b'));
end;

procedure SepararDadosTest.MostrarChave;
begin
  CheckEquals('<ACBr>Teste Simples</ACBr>', SeparaDados('<ACBr>Teste Simples</ACBr>', 'ACBr',  true));
  CheckEquals('<ACBrTeste>Teste     Simples</ACBrTeste>', SeparaDados('<ACBrTeste>Teste     Simples</ACBrTeste>', 'ACBrTeste', true));
  CheckEquals('<ACBr>TesteSimples</ACBr>', SeparaDados('<ACBr>TesteSimples</ACBr>', 'ACBr', true));
  CheckEquals('<b>ACBr Util</b>', SeparaDados('<ACBrUtil>Teste com texto longo <b>ACBr Util</b> feito por DJSystem', 'b', true));
  CheckEquals('<u>#ACBrUtil</u>', SeparaDados('<ACBrUtil>Teste com texto longo <u>#ACBrUtil</u> feito por DJSystem', 'u', true));
end;

procedure SepararDadosTest.MostrarChaveComPrefixo;
const
  TEXTO = '<ns0:ACBr>Teste<ns0:ACBrTeste>Projeto ACBr</ns0:ACBrTeste></ns0:ACBr>';
begin
  CheckEquals('<ns0:ACBrTeste>Projeto ACBr</ns0:ACBrTeste>', SeparaDados(TEXTO, 'ACBrTeste', True));
  CheckEquals('', SeparaDados(TEXTO, 'ACBrTeste', False, False));
end;

procedure SepararDadosTest.QuandoImitacaoChaveEstaNoMeio;
const
  TEXTO = '<ACBr>Teste/ACBr</ACBr>';
begin
  CheckEquals('Teste/ACBr', SeparaDados(TEXTO, 'ACBr'));
end;

procedure SepararDadosTest.ComPrefixo;
const
  TEXTO = '<ns0:ACBr>Teste<ns0:ACBrTeste>Projeto ACBr</ns0:ACBrTeste></ns0:ACBr>';
begin
  CheckEquals('Projeto ACBr', SeparaDados(TEXTO, 'ACBrTeste'));
  CheckEquals('', SeparaDados(TEXTO, 'ACBrTeste', False, False));
end;

procedure SepararDadosTest.ComVariasChaves;
begin
  CheckEquals('ACBrUtil', SeparaDados('<ACBr>Teste <ACBrTeste>ACBrUtil</ACBrTeste> com <ACBrTeste>FPCUnit</ACBrTeste></ACBr>', 'ACBrTeste'));
end;

procedure SepararDadosTest.SemFecharChave;
begin
  CheckEquals('', SeparaDados('<ACBrUtil>Teste com texto longo <b>ACBr Util</b> realizado por FPCUnit', 'ACBrUtil'));
end;

procedure SepararDadosTest.SemAbrirChave;
begin
  CheckEquals('', SeparaDados('Teste com texto longo <b>ACBr Util</b> realizado por FPCUnit</ACBrUtil>', 'ACBrUtil'));
end;

{ LerTagXMLTest }

procedure LerTagXMLTest.Simples;
begin
  {$warnings off}
    CheckEquals('Teste Simples', LerTagXML('<ACBr>Teste Simples</ACBr>', 'acbr'));
  {$warnings on}
end;

procedure LerTagXMLTest.SemIgnorarCase;
begin
  {$warnings off}
    CheckEquals('Teste sem ignorar case', LerTagXML('<ACBr>Teste sem ignorar case</ACBr>', 'ACBr', false));
    CheckEquals('', LerTagXML('<ACBr>Teste sem ignorar case</ACBr>', 'acbr', false));
    CheckEquals('Ler Aqui', LerTagXML('<ACBr>Teste sem <acbr>Ler Aqui</acbr> ignorar case</ACBr>', 'acbr', false));
  {$warnings on}
end;

procedure LerTagXMLTest.ComVariasTags;
begin
  {$warnings off}
    CheckEquals('mais um teste', LerTagXML('<ACBr> teste <br> outro teste </br> <b>mais um teste</b> </ACBr>', 'b'));
  {$warnings on}
end;

{ TestXmlEhUTF8 }

procedure TestXmlEhUTF8.PadraoUTF8AspasDuplasUpperCase;
begin
  CheckTrue( XmlEhUTF8( '<?xml version="1.0" encoding="UTF-8"?>' ) );
end;

procedure TestXmlEhUTF8.PadraoUTF8AspasSimplesLowerCase;
begin
  CheckTrue( XmlEhUTF8( '<?xml version=''1.0'' encoding=''UTF-8''?>' ) );
end;

procedure TestXmlEhUTF8.NaoUTF8;
begin
  CheckFalse( XmlEhUTF8( '<?xml version="1.0" encoding="iso-8859-15"?>' ) );
end;

procedure TestXmlEhUTF8.ApenasXML10;
begin
  CheckFalse( XmlEhUTF8( '<?xml version="1.0"?>' ) );
end;

{ ParseTextTest }

procedure ParseTextTest.ParseDecode;
begin
  CheckEquals('&', ParseText('&amp;'));
  CheckEquals('<', ParseText('&lt;'));
  CheckEquals('>', ParseText('&gt;'));
  CheckEquals('"', ParseText('&quot;'));
  CheckEquals(#39, ParseText('&#39;'));
  CheckEquals(ACBrStr('·'), ParseText('&aacute;', True, False));
  CheckEquals(ACBrStr('¡'), ParseText('&Aacute;', True, False));
  CheckEquals(ACBrStr('‚'), ParseText('&acirc;',  True, False));
  CheckEquals(ACBrStr('¬'), ParseText('&Acirc;',  True, False));
  CheckEquals(ACBrStr('„'), ParseText('&atilde;', True, False));
  CheckEquals(ACBrStr('√'), ParseText('&Atilde;', True, False));
  CheckEquals(ACBrStr('‡'), ParseText('&agrave;', True, False));
  CheckEquals(ACBrStr('¿'), ParseText('&Agrave;', True, False));
  CheckEquals(ACBrStr('È'), ParseText('&eacute;', True, False));
  CheckEquals(ACBrStr('…'), ParseText('&Eacute;', True, False));
  CheckEquals(ACBrStr('Í'), ParseText('&ecirc;',  True, False));
  CheckEquals(ACBrStr(' '), ParseText('&Ecirc;',  True, False));
  CheckEquals(ACBrStr('Ì'), ParseText('&iacute;', True, False));
  CheckEquals(ACBrStr('Õ'), ParseText('&Iacute;', True, False));
  CheckEquals(ACBrStr('Û'), ParseText('&oacute;', True, False));
  CheckEquals(ACBrStr('”'), ParseText('&Oacute;', True, False));
  CheckEquals(ACBrStr('ı'), ParseText('&otilde;', True, False));
  CheckEquals(ACBrStr('’'), ParseText('&Otilde;', True, False));
  CheckEquals(ACBrStr('Ù'), ParseText('&ocirc;',  True, False));
  CheckEquals(ACBrStr('‘'), ParseText('&Ocirc;',  True, False));
  CheckEquals(ACBrStr('˙'), ParseText('&uacute;', True, False));
  CheckEquals(ACBrStr('⁄'), ParseText('&Uacute;', True, False));
  CheckEquals(ACBrStr('¸'), ParseText('&uuml;',   True, False));
  CheckEquals(ACBrStr('‹'), ParseText('&Uuml;',   True, False));
  CheckEquals(ACBrStr('Á'), ParseText('&ccedil;', True, False));
  CheckEquals(ACBrStr('«'), ParseText('&Ccedil;', True, False));
  CheckEquals('''', ParseText('&apos;',  True, False));
end;

procedure ParseTextTest.ParseEncode;
begin
  CheckEquals('&amp;', ParseText('&', False));
  CheckEquals('&lt;', ParseText('<', False));
  CheckEquals('&gt;', ParseText('>', False));
  CheckEquals('&quot;', ParseText('"', False));
  CheckEquals('&#39;', ParseText(#39, False));
end;

procedure ParseTextTest.VerificarConversaoTextoLongo;
begin
  CheckEquals('&<>"', ParseText('&amp;&lt;&gt;&quot;'));
  CheckEquals('&"<>', ParseText('&amp;&quot;&lt;&gt;'));
  CheckEquals('<&">', ParseText('&lt;&amp;&quot;&gt;'));
  CheckEquals( ACBrStr(#39'·√«‹… ’'''), ParseText('&#39;&aacute;&Atilde;&Ccedil;&Uuml;'
              + '&Eacute;&Ecirc;&Otilde;&apos;', True, False));
end;

procedure ParseTextTest.ParseStringSimples_SemParametros;
begin
  CheckEquals('abcdef', ParseText('abcdef'));
end;

procedure ParseTextTest.ParseStringAcentuada_SemParametros;
begin
  CheckEquals('·√«‹… ’', ParseText('·√«‹… ’'));
end;

procedure ParseTextTest.ParseStringAcentuada_DecodeFalse_IsUTF8False;
begin
  CheckEquals('·√«‹… ’', ParseText('·√«‹… ’', False, False));
end;

procedure ParseTextTest.ParseStringAcentuada_DecodeTrue_IsUTF8False;
begin
  CheckEquals('·√«‹… ’', ParseText('·√«‹… ’', True, False));
end;

procedure ParseTextTest.ParseStringAcentuada_DecodeFalse_IsUTF8True;
begin
  CheckEquals('·√«‹… ’', ParseText('·√«‹… ’', False, True));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaAnsi_SemParametros;
var
  LStr: String;
begin
  LStr := NativeStringToAnsi('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaAnsi_DecodeFalse_IsUTF8False;
var
  LStr: String;
begin
  LStr := NativeStringToAnsi('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr, False, False));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaAnsi_DecodeTrue_IsUTF8False;
var
  LStr: String;
begin
  LStr := NativeStringToAnsi('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr, True, False));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaAnsi_DecodeFalse_IsUTF8True;
var
  LStr: String;
begin
  LStr := NativeStringToAnsi('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr, False, True));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaUTF8_SemParametros;
var
  LStr: String;
begin
  LStr := NativeStringToUTF8('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaUTF8_DecodeFalse_IsUTF8False;
var
  LStr: String;
begin
  LStr := NativeStringToUTF8('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr, False, False));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaUTF8_DecodeTrue_IsUTF8False;
var
  LStr: String;
begin
  LStr := NativeStringToUTF8('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr, True, False));
end;

procedure ParseTextTest.ParseStringAcentuadaConvertidaParaUTF8_DecodeFalse_IsUTF8True;
var
  LStr: String;
begin
  LStr := NativeStringToUTF8('·√«‹… ’');
  CheckEquals(LStr, ParseText(LStr, False, true));
end;

{ TruncToTest }

procedure TruncToTest.As11133Currency;
var
  VValor: Currency;
begin
  VValor := 1 * 11.1335;
  CheckEquals(11.1335, TruncTo( VValor, 4 ), 0.0001);
end;

procedure TruncToTest.As11133Double;
var
  VValor: Double;
begin
  VValor := 1 * 11.133;
  CheckEquals(11.133, TruncTo( VValor, 3 ), 0.0001);
end;

procedure TruncToTest.As11133Extended;
var
  VValor: Extended;
begin
  VValor := 1 * 1189.13390;
  CheckEquals(1189.1339, TruncTo( VValor, 4 ), 0.0001);
end;

procedure TruncToTest.As11133Single;
begin
  CheckEquals(11.133, TruncTo( 11.133, 3 ), 0.0001);
end;

procedure TruncToTest.As113Currency;
var
  VValor: Currency;
begin
  VValor := 1 * 1.139;
  ChecknotEquals(1.13, TruncTo( VValor, 3 ), 0.0001);
end;

procedure TruncToTest.As113Double;
var
  VValor: Double;
begin
  VValor := 1 * 1.135;
  CheckEquals(1.13, TruncTo( VValor, 2 ), 0.0001);
end;

procedure TruncToTest.As113Extended;
var
  VValor: Extended;
begin
  VValor := 1 * 1.1373;
  CheckEquals(1.137, TruncTo( VValor, 3 ), 0.0001);
end;

procedure TruncToTest.As113Single;
begin
  CheckEquals(1.13, TruncTo( 1.13, 2 ), 0.0001);
end;

procedure TruncToTest.As1526Currency;
var
  VValor: Currency;
begin
  VValor := 1 * 15.26001;
  CheckEquals(15.26, TruncTo( VValor, 2 ), 0.0001);
end;

procedure TruncToTest.As1526Double;
var
  VValor: Double;
begin
  VValor := 1 * 15.2623;
  CheckEquals(15.262, TruncTo( VValor, 3 ), 0.0001);
end;

procedure TruncToTest.As1526Extended;
var
  VValor: Extended;
begin
  VValor := 1 * 155.2611;
  CheckEquals(155.261, TruncTo( VValor, 3 ), 0.0001);
end;

procedure TruncToTest.As1526Single;
var
  VValor: Single;
begin
  VValor := 1 * 155.2611;
  CheckEquals(155.261, TruncTo( VValor, 3 ), 0.0001);
end;

procedure TruncToTest.As199Currency;
var
  VValor: Currency;
begin
  VValor := 1 * 1.997;
  CheckEquals(1.997, TruncTo( VValor, 3 ), 0.0001);
end;

procedure TruncToTest.As199Double;
var
  VValor: Double;
begin
  VValor := 1 * 1.9985;
  CheckEquals(1.998, TruncTo( VValor, 3 ), 0.0001);
  VValor := 1 * 78.22;
  CheckEquals(78.22, TruncTo( VValor, 2 ), 0.001);
end;

procedure TruncToTest.As9999899Double;
var
  VValor: Double;
begin
  VValor := 1 * 99998.999658800007;
  CheckEquals(99998.99, TruncTo( VValor, 2 ), 0.0001);
end;

procedure TruncToTest.As9999899DoubleTruncToZero;
var
  VValor: Double;
begin
  VValor := 1 * 99998.999658800007;
  CheckEquals(99998, TruncTo( VValor, 0 ), 0.0001);
end;

procedure TruncToTest.As199Extended;
var
  VValor: Extended;
begin
  VValor := 1 * 1.999;
  CheckEquals(1.99, TruncTo( VValor, 2 ), 0.0001);
end;

procedure TruncToTest.As199Single;
var
  VValor: Single;
begin
  VValor := 1 * 1.99;
  CheckEquals(1.9, TruncTo( VValor, 1 ), 0.0001);
end;

procedure TruncToTest.As4386Currency;
var
  VValor: Currency;
begin
  VValor := 1 * 43.86;
  CheckEquals(43.86, TruncTo( VValor, 2 ), 0.0001);
end;

procedure TruncToTest.As4386Double;
var
  VValor: Double;
begin
  VValor := 1 * 43.86;
  CheckEquals(43.86, TruncTo( VValor, 2 ), 0.0001);
end;

procedure TruncToTest.As4386Extended;
var
  VValor: Extended;
begin
  VValor := 1 * 43.86;
  CheckEquals(43.86, TruncTo( VValor, 2 ), 0.0001);
end;

procedure TruncToTest.As4386Single;
//var
//  VValor: Single;
begin
 // VValor := -430000.80016;
  CheckEquals(-430000.8001, TruncTo(-430000.80016 , 4 ), 0.0001);
end;


{ RoundABNTMudancasDeArredondamentoTest }

procedure RoundABNTMudancasDeArredondamentoPara_rmUpTest.SetUp;
begin
  inherited;
  //Isso simula que a aplicaÁ„o esteja num modo de arredondamento diferente do esperado
  // pelo mÈtodo ACBrUtil.RoundABNT
  OldRM := GetRoundMode;
  SetRoundMode(rmUP);
end;

procedure RoundABNTMudancasDeArredondamentoPara_rmUpTest.TearDown;
begin
  SetRoundMode(OldRM);
  inherited;
end;

{ RoundABNTMudancasDeArredondamentoPara_rmDownTest }

procedure RoundABNTMudancasDeArredondamentoPara_rmDownTest.SetUp;
begin
  inherited;
  //Isso simula que a aplicaÁ„o esteja num modo de arredondamento diferente do esperado
  // pelo mÈtodo ACBrUtil.RoundABNT
  OldRM := GetRoundMode;
  SetRoundMode(rmDown);
end;

procedure RoundABNTMudancasDeArredondamentoPara_rmDownTest.TearDown;
begin
  SetRoundMode(OldRM);
  inherited;
end;

{ RoundABNTMudancasDeArredondamentoPara_rmTruncateTest }

procedure RoundABNTMudancasDeArredondamentoPara_rmTruncateTest.SetUp;
begin
  inherited;
  //Isso simula que a aplicaÁ„o esteja num modo de arredondamento diferente do esperado
  // pelo mÈtodo ACBrUtil.RoundABNT
  OldRM := GetRoundMode;
  SetRoundMode(rmTruncate);
end;

procedure RoundABNTMudancasDeArredondamentoPara_rmTruncateTest.TearDown;
begin
  inherited;
  SetRoundMode(OldRM);
end;

initialization
  _RegisterTest('ACBrComum.ACBrUtil', ParseTextTest);
  _RegisterTest('ACBrComum.ACBrUtil', LerTagXMLTest);
  _RegisterTest('ACBrComum.ACBrUtil', TestXmlEhUTF8);
  _RegisterTest('ACBrComum.ACBrUtil', SepararDadosTest);
  _RegisterTest('ACBrComum.ACBrUtil', TruncFixTest);
  _RegisterTest('ACBrComum.ACBrUtil', TruncToTest);

  {Temos 4 classes de testes da RoundABNT. No entanto, apenas uma delas pode ser executada por vez.
   Quando mais de uma È adicionada, podem causar efeitos colaterais ao modificar o RoundingMode.
   Mesmo assim, as outras podem ser adicionadas bastando descomentar as linhas abaixo}
  _RegisterTest('ACBrComum.ACBrUtil', RoundABNTTest);
  _RegisterTest('ACBrComum.ACBrUtil', RoundABNTMudancasDeArredondamentoPara_rmUpTest);
  _RegisterTest('ACBrComum.ACBrUtil', RoundABNTMudancasDeArredondamentoPara_rmDownTest);
  _RegisterTest('ACBrComum.ACBrUtil', RoundABNTMudancasDeArredondamentoPara_rmTruncateTest);
  _RegisterTest('ACBrComum.ACBrUtil', CompareVersionsTest);
  _RegisterTest('ACBrComum.ACBrUtil', TestBitTest);
  _RegisterTest('ACBrComum.ACBrUtil', TesteSetBit);
  _RegisterTest('ACBrComum.ACBrUtil', TesteClearBit);
  _RegisterTest('ACBrComum.ACBrUtil', TestePutBit);
  _RegisterTest('ACBrComum.ACBrUtil', IntToBinTest);
  _RegisterTest('ACBrComum.ACBrUtil', BinToIntTest);
  _RegisterTest('ACBrComum.ACBrUtil', BcdToAscTest);
  _RegisterTest('ACBrComum.ACBrUtil', AscToBcdTest);
  _RegisterTest('ACBrComum.ACBrUtil', IntToLEStrTest);
  _RegisterTest('ACBrComum.ACBrUtil', LEStrToIntTest);
  _RegisterTest('ACBrComum.ACBrUtil', HexToAsciiTest);
  _RegisterTest('ACBrComum.ACBrUtil', AsciiToHexTest);
  _RegisterTest('ACBrComum.ACBrUtil', BinaryStringToStringTest);
  _RegisterTest('ACBrComum.ACBrUtil', StringToBinaryStringTest);
  _RegisterTest('ACBrComum.ACBrUtil', StripHTMLTest);
  _RegisterTest('ACBrComum.ACBrUtil', IntToStrZeroTest);
  _RegisterTest('ACBrComum.ACBrUtil', FloatToIntStrTest);
  _RegisterTest('ACBrComum.ACBrUtil', FloatToStringTest);
  _RegisterTest('ACBrComum.ACBrUtil', FormatFloatBrTest);
  _RegisterTest('ACBrComum.ACBrUtil', FloatMaskTest);
  _RegisterTest('ACBrComum.ACBrUtil', StringToFloatTest);
  _RegisterTest('ACBrComum.ACBrUtil', StringToFloatDefTest);
  _RegisterTest('ACBrComum.ACBrUtil', StrIsIPTest);
  _RegisterTest('ACBrComum.ACBrUtil', EstaVazio_NaoEstaVazioTest);
  _RegisterTest('ACBrComum.ACBrUtil', EstaZerado_NaoEstaZeradoTest);
  _RegisterTest('ACBrComum.ACBrUtil', TamanhoIgualTest);
  _RegisterTest('ACBrComum.ACBrUtil', TamanhoMenorTest);
  _RegisterTest('ACBrComum.ACBrUtil', TraduzComandoTest);
  _RegisterTest('ACBrComum.ACBrUtil', StringToAscTest);
  _RegisterTest('ACBrComum.ACBrUtil', AscToStringTest);
  _RegisterTest('ACBrComum.ACBrUtil', StrCryptTest);
  _RegisterTest('ACBrComum.ACBrUtil', SomaAscIITest);
  _RegisterTest('ACBrComum.ACBrUtil', StringCrc16Test);
  _RegisterTest('ACBrComum.ACBrUtil', PathWithDelimTest);
  _RegisterTest('ACBrComum.ACBrUtil', PathWithoutDelimTest);
  _RegisterTest('ACBrComum.ACBrUtil', TranslateUnprintableTest);
  _RegisterTest('ACBrComum.ACBrUtil', EAN13Test);
  _RegisterTest('ACBrComum.ACBrUtil', ComparaValorTest);
  _RegisterTest('ACBrComum.ACBrUtil', ZipUnzip);
  //TODO: WriteToTXT, WriteLog,
end.

