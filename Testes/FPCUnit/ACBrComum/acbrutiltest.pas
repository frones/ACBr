unit ACBrUtilTest;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  fpcunit, testutils, testregistry, LConvEncoding
  {$else}
    TestFramework
  {$endif};

type

  { WorkingDaysBetweenTest }

  WorkingDaysBetweenTest = class(TTestCase)
  published
    procedure WorkingDaysBetween_DataMesmaSemana;
    procedure WorkingDaysBetween_DataPosSemana;
    procedure WorkingDaysBetween_DataInicioSabado;
    procedure WorkingDaysBetween_DataInicioDomingo;
    procedure WorkingDaysBetween_DataFinalSabado;
    procedure WorkingDaysBetween_DataFinalDomingo;
    procedure WorkingDaysBetween_DataFinalMenor;
    procedure WorkingDaysBetween_DataZero;
    procedure WorkingDaysBetween_DataInicialZero;
    procedure WorkingDaysBetween_DataFinalZero;
  end;

   { IncWorkingDayTest }

  IncWorkingDayTest = class(TTestCase)
  private
  published
    procedure IncWorkingDayTest_DataInicioSabado;
    procedure IncWorkingDayTest_DataInicioDomingo;
    procedure IncWorkingDayTest_PosSemana;
    procedure IncWorkingDayTest_DiaFinalSabado;
    procedure IncWorkingDayTest_DiaFinalDomingo;
    procedure IncWorkingDayTest_ZeroDiaSabado;
    procedure IncWorkingDayTest_ZeroDiaDomingo;
    procedure IncWorkingDayTest_ZeroDiaSemana;
    procedure IncWorkingDayTest_DiaNegativo;
    procedure IncWorkingDayTest_DiaNegativoInicioSabado;
    procedure IncWorkingDayTest_DiaNegativoInicioDomingo;
  end;

  { FindDelimiterInTextTest }

  FindDelimiterInTextTest = class(TTestCase)
  private
  published
    procedure FindDelimiterInTextTest_SemDelimitador;
    procedure FindDelimiterInTextTest_DelimitadorPipe;
    procedure FindDelimiterInTextTest_DelimitadorVirgula;
    procedure FindDelimiterInTextTest_DelimitadorPontoEVirgula;
    procedure FindDelimiterInTextTest_DelimitadorCustomizado;
  end;

  { FindDelimiterInTextTest }

  { ChangeLineBreakTest }

  ChangeLineBreakTest = class(TTestCase)
  private
    FCRLFText: String;
    FLFText: String;
    FCRText: String;
    FPipeText: String;
  protected
    procedure SetUp; override;
  published
    procedure CRLFParaPipe;
    procedure LFParaPipe;
    procedure CRParaPipe;
    procedure CRLFParaLF;
    procedure LFParaLF;
    procedure CRParaLF;
    procedure CRLFParaCR;
    procedure LFParaCR;
    procedure CRParaCR;
    procedure CRLFParaCRLF;
    procedure LFParaCRLF;
    procedure CRParaCRLF;
  end;

  { AddDelimitedTextToListTeste }

  AddDelimitedTextToListTeste = class(TTestCase)
  private
    FSL: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AddDelimitedTextToListTeste_StringVazia;
    procedure AddDelimitedTextToListTeste_DoisItens;
    procedure AddDelimitedTextToListTeste_SemDelimitador;
    procedure AddDelimitedTextToListTeste_DelimitadorEspaco;
    procedure AddDelimitedTextToListTeste_ComUmaAspasSimples;
    procedure AddDelimitedTextToListTeste_ComDuasAspasSimples;
    procedure AddDelimitedTextToListTeste_ComUmaAspasDuplasComQuote;
    procedure AddDelimitedTextToListTeste_ComUmaAspasDuplasSemQuote;
    procedure AddDelimitedTextToListTeste_ComDuasAspasDuplaComQuote;
    procedure AddDelimitedTextToListTeste_ComDuasAspasDuplaSemQuote;
    procedure AddDelimitedTextToListTeste_ComDelimitadorEntreAspasSimples;
    procedure AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasComQuote;
    procedure AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasSemQuote;
  end;

  { Split }

  SplitTeste = class(TTestCase)
  published
    procedure Split_StringVazia;
    procedure Split_DoisItens;
    procedure Split_SemDelimitador;
    procedure Split_DelimitadorEspaco;
  end;

  { TiraPontosTest }

  TiraPontosTest = class(TTestCase)
  published
    procedure TiraPontos_StringInvalida_RetornaVazio;
    procedure TiraPontos_StringInicioInvalido_RetornaFinal;
    procedure TiraPontos_StringFimInvalido_RetornaInicio;
    procedure TiraPontos_StringMeioInvalido_RetornaExtremos;
    procedure TiraPontos_StringExtremosInvalidos_RetornaMeio;
    procedure TiraPontos_StringComEspacos_RetornaMeioSemEspacos;
  end;

  { ParseTextTest }

  ParseTextTest = class(TTestCase)
  private
  published
    procedure ParseDecode;
    procedure ParseEncode;
    procedure VerificarConversaoTextoLongo;
  end;

  { LerTagXMLTest }

  LerTagXMLTest = class(TTestCase)
  published
    procedure Simples;
    procedure SemIgnorarCase;
    procedure ComVariasTags;
  end;

  { DecodeToStringTest }

  DecodeToStringTest = class(TTestCase)
  published
    procedure DecodeToString_TesteUTF8;
    procedure DecodeToString_TesteAnsi;
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
  end;

  { QuebrarLinhaTest }

  QuebrarLinhaTest = class(TTestCase)
  published
    procedure TresCampos;
    procedure PipeDelimiter;
  end;

  { ACBrStrToAnsiTest }

  ACBrStrToAnsiTest = class(TTestCase)
  published
    procedure ACBrStrToAnsi_TesteUTF8;
    procedure ACBrStrToAnsi_TesteReverso;
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

  { padRightTest }

  padRightTest = class(TTestCase)
  published
    procedure CompletarString;
    procedure ManterString;
    procedure TruncarString;
  end;

  { padLeftTest }

  padLeftTest = class(TTestCase)
  published
   procedure CompletarString;
   procedure CompletarStringAcentosAnsi;
   procedure CompletarStringAcentosUTF8;
   procedure ManterString;
   procedure TruncarString;
  end;

  { padCenterTest }

  padCenterTest = class(TTestCase)
  published
   procedure PreencherString;
   procedure TruncarString;
  end;

  { padSpaceTest }

  padSpaceTest = class(TTestCase)
  published
   procedure CompletarString;
   procedure TruncarString;
   procedure SubstituirSeparadorPorEspacos;
   procedure SubstituirSeparadorPorCaracter;
  end;

  { RemoverEspacosDuplosTest }

  RemoverEspacosDuplosTest = class(TTestCase)
  published
   procedure RemoverApenasEspacosDuplos;
   procedure RemoverMaisQueDoisEspacos;
  end;

  { RemoveStringTest }

  RemoveStringTest = class(TTestCase)
  published
   procedure Remover;
  end;

  { RemoveStringsTest }

  RemoveStringsTest = class(TTestCase)
  private
    StringsToRemove: array [1..5] of AnsiString;
  protected
    procedure SetUp; override;
  published
   procedure TextoSimples;
   procedure TextoLongo;
  end;

  { StripHTMLTest }

  StripHTMLTest = class(TTestCase)
  published
   procedure TesteSimples;
   procedure TesteCompleto;
   procedure TesteTagsInvalidas;
   procedure TesteTagsInvertidas;
  end;

  { RemoveEmptyLinesTest }

  RemoveEmptyLinesTest = class(TTestCase)
  private
    SL: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
   procedure VerificarLinhas;
   procedure VerificarConteudo;
  end;

  { RandomNameTest }

  RandomNameTest = class(TTestCase)
  published
   procedure TamanhoDois;
   procedure TamanhoQuatro;
   procedure TamanhoOito;
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

  { IfEmptyThenTest }

  IfEmptyThenTest = class(TTestCase)
  published
   procedure RetornarValorNormal;
   procedure SeVazioRetornaValorPadrao;
   procedure RealizarDoTrim;
   procedure NaoRealizarDoTrim;
  end;

  { PosAtTest }

  PosAtTest = class(TTestCase)
  private
    AStr: String;
  protected
    procedure SetUp; override;
  published
   procedure AchaPrimeiraOcorrencia;
   procedure AchaSegundaOcorrencia;
   procedure AchaTerceiraOcorrencia;
   procedure NaoAchaOcorrencia;
  end;

  { PosLastTest }

  PosLastTest = class(TTestCase)
  private
    AStr: String;
  protected
    procedure SetUp; override;
  published
   procedure AchaOcorrencia;
   procedure NaoAchaOcorrencia;
  end;

  { PosLastTest }

  { CountStrTest }

  CountStrTest = class(TTestCase)
  private
    AStr: String;
  protected
    procedure SetUp; override;
  published
   procedure AchaOcorrencia;
   procedure NaoAchaOcorrencia;
  end;

  { Poem_ZerosTest }

  Poem_ZerosTest = class(TTestCase)
  published
    procedure ParamString;
    procedure Truncando;
    procedure ParamInt64;
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

  { FormatDateBrTest }

  FormatDateBrTest = class(TTestCase)
  published
   procedure Normal;
   procedure Bissesto;
   procedure ComMascara;
  end;

  { FormatDateTimeBrTest }

  FormatDateTimeBrTest = class(TTestCase)
  published
   procedure Normal;
   procedure BissestoMeiaNoite;
   procedure ComMascara;
  end;

  { StringToDateTimeTest }

  StringToDateTimeTest = class(TTestCase)
  published
   procedure DataAno4Digitos;
   procedure DataAno2Digitos;
   procedure Hora;
   procedure DataEHora;
   procedure ComFormatSettingsDiferente;
  end;

  { StringToDateTimeDefTest }

  StringToDateTimeDefTest = class(TTestCase)
  published
   procedure Data;
   procedure Hora;
   procedure DataEHora;
   procedure ValorDefault;
  end;

  { StoDTest }

  StoDTest = class(TTestCase)
  published
   procedure Normal;
   procedure DataSemHora;
   procedure DataInvalida;
  end;

  { DtoSTest }

  DtoSTest = class(TTestCase)
  published
   procedure Data;
  end;

  { DTtoSTest }

  DTtoSTest = class(TTestCase)
  published
   procedure DataEHora;
   procedure DataSemHora;
  end;

  { StrIsAlphaTest }

  StrIsAlphaTest = class(TTestCase)
  published
   procedure Texto;
   procedure TextoComNumeros;
   procedure TextoComCaractersEspeciais;
   procedure TextoComCaractersAcentuados;
  end;

  { StrIsAlphaNumTest }

  StrIsAlphaNumTest = class(TTestCase)
  published
    procedure Texto;
    procedure TextoComNumeros;
    procedure TextoComCaractersEspeciais;
    procedure TextoComCaractersAcentuados;
  end;

  { StrIsNumberTest }

  StrIsNumberTest = class(TTestCase)
  published
    procedure Texto;
    procedure Numeros;
    procedure TextoComNumeros;
    procedure TextoComSeparadores;
    procedure TextoComCaractersEspeciais;
  end;

  { CharIsAlphaTest }

  CharIsAlphaTest = class(TTestCase)
  published
    procedure Caracter;
    procedure Numero;
    procedure CaracterEspecial;
  end;

  { CharIsAlphaNumTest }

  CharIsAlphaNumTest = class(TTestCase)
  published
    procedure Caracter;
    procedure Numero;
    procedure CaracterEspecial;
  end;

  { CharIsNumTest }

  CharIsNumTest = class(TTestCase)
  published
    procedure Caracter;
    procedure Numero;
    procedure CaracterEspecial;
  end;

  { OnlyNumberTest }

  OnlyNumberTest = class(TTestCase)
  private
  published
    procedure Texto;
    procedure Numeros;
    procedure TextoComNumeros;
    procedure TextoComSeparadores;
    procedure TextoComCaractersEspeciais;
  end;

  { OnlyAlphaTest }

  OnlyAlphaTest = class(TTestCase)
  published
    procedure Texto;
    procedure Numeros;
    procedure TextoComNumeros;
    procedure TextoComCaractersEspeciais;
  end;

  { OnlyAlphaNumTest }

  OnlyAlphaNumTest = class(TTestCase)
  published
    procedure Texto;
    procedure Numeros;
    procedure TextoComNumeros;
    procedure TextoComCaractersEspeciais;
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

  { TiraAcentosTest }

  TiraAcentosTest = class(TTestCase)
  published
    procedure Normal;
    procedure ComQuebrasDeLinha;
  end;

  { TiraAcentoTest }

  TiraAcentoTest = class(TTestCase)
  published
    procedure Normal;
  end;

  { AjustaLinhasTest }

  AjustaLinhasTest = class(TTestCase)
  private
    AStr1, AStr2: String;
  protected
    procedure SetUp; override;
  published
    procedure QuebraEmQuarentaColunas;
    procedure QuebraEmTrintaColunasSemPad;
    procedure QuebraEmTrintaColunasComPad;
    procedure QuebraEmSeteColunas;
    procedure ComLimiteDeLinhasSemPad;
    procedure ComLimiteDeLinhasComPad;
  end;

  { QuebraLinhasTest }

  QuebraLinhasTest = class(TTestCase)
  private
    AStr: String;
  protected
    procedure SetUp; override;
  published
    procedure QuebraEmNoventaColunas;
    procedure QuebraEmQuarentaColunas;
    procedure QuebraEmCinquentaComSeparadorE;
    procedure QuebraStrComLineBreakEm32cols;
    procedure QuebraDuasLinhasNoLimiteColuna;
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
  Math, dateutils,
  synacode,
  ACBrUtil, ACBrCompress, ACBrConsts;

{ ChangeLineBreakTest }

procedure ChangeLineBreakTest.SetUp;
begin
  inherited SetUp;
  FLFText := 'LINHA1'+LF+LF+'LINHA3'+LF+'LINHA4'+LF+LF+'LINHA6'+LF;
  FCRText := 'LINHA1'+CR+CR+'LINHA3'+CR+'LINHA4'+CR+CR+'LINHA6'+CR;
  FCRLFText := 'LINHA1'+CR+LF+CR+LF+'LINHA3'+CR+LF+'LINHA4'+CR+LF+CR+LF+'LINHA6'+CR+LF;
  FPipeText := 'LINHA1||LINHA3|LINHA4||LINHA6|';
end;

procedure ChangeLineBreakTest.CRLFParaPipe;
begin
  CheckEquals(ChangeLineBreak(FCRLFText,'|'), FPipeText);
end;

procedure ChangeLineBreakTest.LFParaPipe;
begin
  CheckEquals(ChangeLineBreak(FLFText,'|'), FPipeText);
end;

procedure ChangeLineBreakTest.CRParaPipe;
begin
  CheckEquals(ChangeLineBreak(FCRText,'|'), FPipeText);
end;

procedure ChangeLineBreakTest.CRLFParaLF;
begin
  CheckEquals(ChangeLineBreak(FCRLFText,LF), FLFText);
end;

procedure ChangeLineBreakTest.LFParaLF;
begin
  CheckEquals(ChangeLineBreak(FLFText,LF), FLFText);
end;

procedure ChangeLineBreakTest.CRParaLF;
begin
  CheckEquals(ChangeLineBreak(FCRText,LF), FLFText);
end;

procedure ChangeLineBreakTest.CRLFParaCR;
begin
  CheckEquals(ChangeLineBreak(FCRLFText,CR), FCRText);
end;

procedure ChangeLineBreakTest.LFParaCR;
begin
  CheckEquals(ChangeLineBreak(FLFText,CR), FCRText);
end;

procedure ChangeLineBreakTest.CRParaCR;
begin
  CheckEquals(ChangeLineBreak(FCRText,CR), FCRText);
end;

procedure ChangeLineBreakTest.CRLFParaCRLF;
begin
  CheckEquals(ChangeLineBreak(FCRLFText,CRLF), FCRLFText);
end;

procedure ChangeLineBreakTest.LFParaCRLF;
begin
  CheckEquals(ChangeLineBreak(FLFText,CRLF), FCRLFText);
end;

procedure ChangeLineBreakTest.CRParaCRLF;
begin
  CheckEquals(ChangeLineBreak(FCRText,CRLF), FCRLFText);
end;

{ AddDelimitedTextToListTeste }

procedure AddDelimitedTextToListTeste.SetUp;
begin
  inherited SetUp;
  FSL := TStringList.Create;
end;

procedure AddDelimitedTextToListTeste.TearDown;
begin
  FSL.Free;
  inherited TearDown;
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_StringVazia;
begin
  CheckEquals(0, AddDelimitedTextToList('',';',FSL));
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_DoisItens;
begin
  CheckEquals(2, AddDelimitedTextToList('comercial@djpdv.com.br;financeiro@djpdv.com.br',';',FSL));
  CheckEquals('comercial@djpdv.com.br', FSL[0]);
  CheckEquals('financeiro@djpdv.com.br', FSL[1]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_SemDelimitador;
begin
  CheckEquals(1, AddDelimitedTextToList('comercial@djpdv.com.br',';',FSL));
  CheckEquals('comercial@djpdv.com.br', FSL[0]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_DelimitadorEspaco;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO ACBR www.projetoacbr.com.br',' ',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('ACBR', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComUmaAspasSimples;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|''ACBR|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('''ACBR', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDuasAspasSimples;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|''ACBR''|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('''ACBR''', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComUmaAspasDuplasComQuote;
begin
  CheckEquals(2, AddDelimitedTextToList('PROJETO|"ACBR|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('ACBR|www.projetoacbr.com.br', FSL[1]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComUmaAspasDuplasSemQuote;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|"ACBR|www.projetoacbr.com.br','|',FSL, #0 ));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('"ACBR', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDuasAspasDuplaComQuote;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|"ACBR"|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('ACBR', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDuasAspasDuplaSemQuote;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|"ACBR"|www.projetoacbr.com.br','|',FSL, #0 ));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('"ACBR"', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDelimitadorEntreAspasSimples;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO ''A C B R'' www.projetoacbr.com.br',' ',FSL, ''''));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('A C B R', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasComQuote;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO "A C B R" www.projetoacbr.com.br',' ',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('A C B R', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasSemQuote;
begin
  CheckEquals(6, AddDelimitedTextToList('PROJETO "A C B R" www.projetoacbr.com.br',' ',FSL, #0));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('"A', FSL[1]);
  CheckEquals('C', FSL[2]);
  CheckEquals('B', FSL[3]);
  CheckEquals('R"', FSL[4]);
  CheckEquals('www.projetoacbr.com.br', FSL[5]);
end;

{ FindDelimiterInTextTest }

procedure FindDelimiterInTextTest.FindDelimiterInTextTest_SemDelimitador;
begin
  CheckEquals(' ',FindDelimiterInText('comercial@djpdv.com.br'));
end;

procedure FindDelimiterInTextTest.FindDelimiterInTextTest_DelimitadorPipe;
begin
  CheckEquals('|',FindDelimiterInText('comercial@djpdv.com.br|financeiro@djpdv.com.br'));
end;

procedure FindDelimiterInTextTest.FindDelimiterInTextTest_DelimitadorVirgula;
begin
  CheckEquals(',',FindDelimiterInText('comercial@djpdv.com.br,financeiro@djpdv.com.br'));
end;

procedure FindDelimiterInTextTest.FindDelimiterInTextTest_DelimitadorPontoEVirgula;
begin
  CheckEquals(';',FindDelimiterInText('comercial@djpdv.com.br;financeiro@djpdv.com.br'));
end;

procedure FindDelimiterInTextTest.FindDelimiterInTextTest_DelimitadorCustomizado;
begin
  CheckEquals('&',FindDelimiterInText('comercial@djpdv.com.br&financeiro@djpdv.com.br','&'));
end;

{ IncWorkingDayTest }

procedure IncWorkingDayTest.IncWorkingDayTest_DataInicioSabado;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,17);
  WorkingDays  := 11;
  ADateResult  := EncodeDate(2017,07,03);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_DataInicioDomingo;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,18);
  WorkingDays  := 11;
  ADateResult  := EncodeDate(2017,07,03);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_PosSemana;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,23);
  WorkingDays  := 10;
  ADateResult  := EncodeDate(2017,07,07);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_DiaFinalSabado;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,22);
  WorkingDays  := 2;
  ADateResult  := EncodeDate(2017,06,26);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_DiaFinalDomingo;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,23);
  WorkingDays  := 1;
  ADateResult  := EncodeDate(2017,06,26);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_ZeroDiaSabado;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,24);
  WorkingDays  := 0;
  ADateResult  := EncodeDate(2017,06,26);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_ZeroDiaDomingo;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,25);
  WorkingDays  := 0;
  ADateResult  := EncodeDate(2017,06,26);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_ZeroDiaSemana;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,26);
  WorkingDays  := 0;
  ADateResult  := EncodeDate(2017,06,26);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_DiaNegativo;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,19);
  WorkingDays  := -3;
  ADateResult  := EncodeDate(2017,06,14);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_DiaNegativoInicioSabado;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,24);
  WorkingDays  := -6;
  ADateResult  := EncodeDate(2017,06,16);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

procedure IncWorkingDayTest.IncWorkingDayTest_DiaNegativoInicioDomingo;
var
  ADateIni, ADateResult: TDateTime;
  WorkingDays: Integer;
begin
  ADateIni     := EncodeDate(2017,06,25);
  WorkingDays  := -6;
  ADateResult  := EncodeDate(2017,06,16);

  CheckEquals(ADateResult,ACBrUtil.IncWorkingDay(ADateIni,WorkingDays));
end;

{ WorkingDaysBetweenTest }

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataMesmaSemana;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,06,26);
  ADateEnd := EncodeDate(2017,06,30);
  CheckEquals(4,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataPosSemana;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,06,26);
  ADateEnd := EncodeDate(2017,07,07);
  CheckEquals(9,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataInicioSabado;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,06,24);
  ADateEnd := EncodeDate(2017,07,03);
  CheckEquals(6,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataInicioDomingo;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,06,25);
  ADateEnd := EncodeDate(2017,07,03);
  CheckEquals(6,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataFinalSabado;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,06,25);
  ADateEnd := EncodeDate(2017,07,08);
  CheckEquals(10,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataFinalDomingo;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,06,25);
  ADateEnd := EncodeDate(2017,07,09);
  CheckEquals(10,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataFinalMenor;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,07,10);
  ADateEnd := EncodeDate(2017,07,09);
  CheckEquals(0,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataZero;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := 0;
  ADateEnd := 0;
  CheckEquals(0,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataInicialZero;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := 0;
  ADateEnd := EncodeDate(2017,07,09);
  CheckEquals(0,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

procedure WorkingDaysBetweenTest.WorkingDaysBetween_DataFinalZero;
var
  ADateIni, ADateEnd: TDateTime;
begin
  ADateIni := EncodeDate(2017,07,09);
  ADateEnd := 0;
  CheckEquals(0,ACBrUtil.WorkingDaysBetween(ADateIni,ADateEnd));
end;

{ ZipUnzip }

procedure ZipUnzip.SetUp;
var
  I: Integer;
  Linha: AnsiString;
begin
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

{ TiraPontosTest }

procedure TiraPontosTest.TiraPontos_StringComEspacos_RetornaMeioSemEspacos;
begin
  CheckEquals('AaBbCcDdç123', ACBrUtil.TiraPontos('/-.) (,/-. )AaBbCc Ddç 1 23 (,,( ).-/'));

end;

procedure TiraPontosTest.TiraPontos_StringExtremosInvalidos_RetornaMeio;
begin
  CheckEquals('AaBbCcDdç123', ACBrUtil.TiraPontos('/-.)(,/-.)AaBbCcDdç123(,,().-/'));

end;

procedure TiraPontosTest.TiraPontos_StringFimInvalido_RetornaInicio;
begin
  CheckEquals('AaBbCcDdç123', ACBrUtil.TiraPontos('AaBbCcDdç123/-.)(,/-.)(,,().-/'));

end;

procedure TiraPontosTest.TiraPontos_StringInicioInvalido_RetornaFinal;
begin
  CheckEquals('AaBbCcDdç123', ACBrUtil.TiraPontos('/-.)(,/-.)(,,().-/AaBbCcDdç123'));
end;

procedure TiraPontosTest.TiraPontos_StringInvalida_RetornaVazio;
begin
  CheckEquals('', ACBrUtil.TiraPontos('/-.)(,/-.)(,,().-/'));
end;

procedure TiraPontosTest.TiraPontos_StringMeioInvalido_RetornaExtremos;
begin
  CheckEquals('AaBbCcDdç123', ACBrUtil.TiraPontos('AaBbCc/-.)(,/-.)(,,().-/Ddç123'));
end;

{ EAN13Test }

procedure EAN13Test.Valido;
begin
  CheckEquals( '8', EAN13_DV('123456789012'));
  CheckEquals( '5', EAN13_DV('789835741001'));

  CheckTrue( EAN13Valido('2004700001341') );
  CheckTrue( EAN13Valido('1234567890128') );
  CheckTrue( EAN13Valido('7898357410015') );
end;

procedure EAN13Test.TamanhoMaior;
begin
   CheckFalse( EAN13Valido('78983574100156'));
end;

procedure EAN13Test.TamanhoMenor;
begin
  CheckFalse( EAN13Valido('789835741001'));
end;

procedure EAN13Test.DigitoInvalido;
begin
  CheckFalse( EAN13Valido('7898357410010'));
  CheckFalse( EAN13Valido('1234567890129'));
end;

procedure EAN13Test.ComLetras;
begin
  CheckFalse( EAN13Valido('A89835741001D'));
end;

procedure EAN13Test.EAN13Valido_String0000000000000_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('0000000000000'));
end;

procedure EAN13Test.EAN13Valido_String2000100002629_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('2000100002629'));
end;

procedure EAN13Test.EAN13Valido_String7506195185568_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7506195185568'));
end;

procedure EAN13Test.EAN13Valido_String7891060886139_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7891060886139'));
end;

procedure EAN13Test.EAN13Valido_String7893946087173_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7893946087173'));
end;

procedure EAN13Test.EAN13Valido_String7896232517828_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7896232517828'));
end;

procedure EAN13Test.EAN13Valido_String7896645900026_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7896645900026'));
end;

procedure EAN13Test.EAN13Valido_String7897186015095_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7897186015095'));
end;

procedure EAN13Test.EAN13Valido_String7898132132019_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7898132132019'));

end;

procedure EAN13Test.EAN13Valido_String7898908141016_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('7898908141016'));
end;

procedure EAN13Test.EAN13Valido_StringInvalida_RetornaFalso;
begin
  CheckFalse(ACBrUtil.EAN13Valido('abcdefghijklm'));
end;

procedure EAN13Test.EAN13Valido_StringValida_RetornaTrue;
begin
  CheckTrue(ACBrUtil.EAN13Valido('1234567890128'));
end;

procedure EAN13Test.EAN13_DV_Codigo000000000000_Retorna0;
begin
  CheckEquals('0', ACBrUtil.EAN13_DV('000000000000'));
end;

procedure EAN13Test.EAN13_DV_Codigo200010000262_Retorna9;
begin
  CheckEquals('9', ACBrUtil.EAN13_DV('200010000262'));
end;

procedure EAN13Test.EAN13_DV_Codigo750619518556_Retorna8;
begin
  CheckEquals('8', ACBrUtil.EAN13_DV('750619518556'));
end;

procedure EAN13Test.EAN13_DV_Codigo789106088613_Retorna9;
begin
  CheckEquals('9', ACBrUtil.EAN13_DV('789106088613'));
end;

procedure EAN13Test.EAN13_DV_Codigo789394608717_Retorna3;
begin
  CheckEquals('3', ACBrUtil.EAN13_DV('789394608717'));
end;

procedure EAN13Test.EAN13_DV_Codigo789623251782_Retorna8;
begin
  CheckEquals('8', ACBrUtil.EAN13_DV('789623251782'));
end;

procedure EAN13Test.EAN13_DV_Codigo789664590002_Retorna6;
begin
  CheckEquals('6', ACBrUtil.EAN13_DV('789664590002'));
end;

procedure EAN13Test.EAN13_DV_Codigo789718601509_Retorna5;
begin
  CheckEquals('5', ACBrUtil.EAN13_DV('789718601509'));
end;

procedure EAN13Test.EAN13_DV_Codigo789813213201_Retorna9;
begin
  CheckEquals('9', ACBrUtil.EAN13_DV('789813213201'));
end;

procedure EAN13Test.EAN13_DV_Codigo789890814101_Retorna6;
begin
  CheckEquals('6', ACBrUtil.EAN13_DV('789890814101'));
end;

procedure EAN13Test.EAN13_DV_StringAlphanumerica_RetornaVazio;
begin
  CheckEquals('', ACBrUtil.EAN13_DV('1234567890ab'));
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
  CheckEquals(47933,StringCrc16('123456789'));
  CheckEquals(14809,StringCrc16('987654321'));
  CheckEquals(28843,StringCrc16('Projeto ACBr'));
  CheckEquals(59551,StringCrc16('ACBr Projeto'));
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
  AStr := 'Projeto ACBr. Teste Unitário';
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

{ QuebraLinhasTest }

procedure QuebraLinhasTest.SetUp;
begin
  // Nota Essa Unit está em CP1252
  //              0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....
  AStr := ACBrStr('Dez Milhões e Duzentos e Cinquenta e Cinco Mil e Quatrocentos e Trinta e Cinco Reais');
end;

procedure QuebraLinhasTest.QuebraEmNoventaColunas;
begin
  CheckEquals(AStr, QuebraLinhas(AStr,90));
end;

procedure QuebraLinhasTest.QuebraEmQuarentaColunas;
var
  Resp: String;
begin
  Resp := QuebraLinhas(AStr,40);
                 //   0....+....1....+....2....+....3....+....4
  CheckEquals(ACBrStr('Dez Milhões e Duzentos e Cinquenta e ')+sLineBreak+
              ACBrStr('Cinco Mil e Quatrocentos e Trinta e ')+sLineBreak+
              ACBrStr('Cinco Reais') ,Resp );
end;

procedure QuebraLinhasTest.QuebraEmCinquentaComSeparadorE;
var
  Resp: String;
begin
  Resp := QuebraLinhas(AStr,50,'e');
                //   0....+....1....+....2....+....3....+....4....+....5
  CheckEquals(ACBrStr('Dez Milhões e Duzentos e Cinquenta e Cinco Mil e')+sLineBreak+
              ACBrStr(' Quatrocentos e Trinta e Cinco Reais'), Resp );
end;

procedure QuebraLinhasTest.QuebraStrComLineBreakEm32cols;
var
  Resp: String;
  AMsg1, AMsg2: String;
begin
  AMsg1 := 'Erro ao enviar Dados da Venda:'+sLineBreak+
           '539-Rejeição: Duplicidade de NF-e com diferença na Chave de Acesso'+sLineBreak+
           '[chNFe:35150705481336000137650220000000031000000626]'+sLineBreak+
           '[nRec:351000006395525]'+sLineBreak+
           'Tentar novamente ?';

      //   0....+....1....+....2....+....3....+....4....+....5
  AMsg2 := 'Erro ao enviar Dados da Venda:'+sLineBreak+
           '539-Rejeição: Duplicidade de '+sLineBreak+
           'NF-e com diferença na Chave de '+sLineBreak+
           'Acesso'+sLineBreak+
           '[chNFe:3515070548133600013765022'+sLineBreak+
           '0000000031000000626]'+sLineBreak+
           '[nRec:351000006395525]'+sLineBreak+
           'Tentar novamente ?';

  Resp := QuebraLinhas( ACBrStr(AMsg1), 32);

  CheckEquals( ACBrStr(AMsg2), Resp );
end;

procedure QuebraLinhasTest.QuebraDuasLinhasNoLimiteColuna;
var
  Resp: String;
  AMsg1: String;
begin
  AMsg1 := 'Endereço de Entrega sem Cod.IBGE'+sLineBreak+
           'Tentar novamente ?';

      //   0....+....1....+....2....+....3....+....4....+....5

  Resp := QuebraLinhas( ACBrStr(AMsg1), 32);

  CheckEquals( ACBrStr(AMsg1), Resp );
end;

{ AjustaLinhasTest }

procedure AjustaLinhasTest.SetUp;
begin
  AStr1 := '....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8';
  AStr2 := 'Linha1'+sLineBreak+'   Linha2'+sLineBreak+sLineBreak+'Linha 4'+sLineBreak;
end;

procedure AjustaLinhasTest.QuebraEmQuarentaColunas;
begin
  CheckEquals( copy(AStr1,1,40) + #10 + copy(AStr1,41,80) + #10,
               AjustaLinhas(AStr1,40));
  CheckEquals( StringReplace(AStr2, sLineBreak, #10, [rfReplaceAll]),
               AjustaLinhas(AStr2,40));
end;

procedure AjustaLinhasTest.QuebraEmTrintaColunasSemPad;
begin
  CheckEquals( copy(AStr1,1,30) + #10 + copy(AStr1,31,30) + #10 + copy(AStr1,61,80) + #10,
               AjustaLinhas(AStr1,30));
  CheckEquals( StringReplace(AStr2, sLineBreak, #10, [rfReplaceAll]),
               AjustaLinhas(AStr2,40));
end;

procedure AjustaLinhasTest.QuebraEmTrintaColunasComPad;
var
  Resp: String;
begin
  Resp := AjustaLinhas(AStr1,30,0,True);
  CheckEquals( copy(AStr1,1,30) + #10 + copy(AStr1,31,30) + #10 + PadRight(copy(AStr1,61,80),30) + #10,
               Resp );

  Resp := AjustaLinhas(AStr2,30,0,True);
  CheckEquals('Linha1                        '#10+
              '   Linha2                     '#10+
              '                              '#10+
              'Linha 4                       '#10, Resp );
end;

procedure AjustaLinhasTest.QuebraEmSeteColunas;
var
  Resp: String;
begin
  Resp := AjustaLinhas(AStr2,7);
  CheckEquals('Linha1'#10'   Linh'#10'a2'#10#10'Linha 4'#10, Resp);
end;

procedure AjustaLinhasTest.ComLimiteDeLinhasSemPad;
var
  Resp: String;
begin
  Resp := AjustaLinhas(AStr1,30,2);
  CheckEquals( copy(AStr1,1,30) + #10 + copy(AStr1,31,30) + #10, Resp );

  Resp := AjustaLinhas(AStr2,30,2);
  CheckEquals( 'Linha1'#10'   Linha2'#10, Resp );
end;

procedure AjustaLinhasTest.ComLimiteDeLinhasComPad;
var
  Resp: String;
begin
  Resp := AjustaLinhas(AStr1,30,3,True);
  CheckEquals( copy(AStr1,1,30) + #10 + copy(AStr1,31,30) + #10 + PadRight(copy(AStr1,61,80),30) + #10,
               Resp );

  Resp := AjustaLinhas(AStr2,30,3,True);
  CheckEquals( 'Linha1                        '#10+
               '   Linha2                     '#10+
               '                              '#10, Resp);
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

{ FormatDateTimeBrTest }

procedure FormatDateTimeBrTest.Normal;
Var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(1971,08,14,12,13,14,0);
  CheckEquals('14/08/1971 12:13:14', FormatDateTimeBr(ADateTime));
end;

procedure FormatDateTimeBrTest.BissestoMeiaNoite;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(2012,02,29,23,59,59,0);
  CheckEquals('29/02/2012 23:59:59', FormatDateTimeBr(ADateTime));
end;

procedure FormatDateTimeBrTest.ComMascara;
Var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(1971,08,14,12,3,4,0);
  CheckEquals('08/14/1971 12:03', FormatDateTimeBr(ADateTime, 'MM/DD/YYYY hh:nn'));
  CheckEquals('14/08/71 12:3:4', FormatDateTimeBr(ADateTime, 'DD/MM/YY h:n:s'));
end;

{ FormatDateBrTest }

procedure FormatDateBrTest.Normal;
Var
  ADate: TDateTime;
begin
  ADate := EncodeDate(1971,08,14);
  CheckEquals('14/08/1971', FormatDateBr(ADate));
end;

procedure FormatDateBrTest.Bissesto;
var
  ADate: TDateTime;
begin
  ADate := EncodeDate(2012,02,29);
  CheckEquals('29/02/2012', FormatDateBr(ADate));
end;

procedure FormatDateBrTest.ComMascara;
Var
  ADate: TDateTime;
begin
  ADate := EncodeDate(1971,08,14);
  CheckEquals('08/14/1971', FormatDateBr(ADate, 'MM/DD/YYYY'));
  CheckEquals('14/08/71', FormatDateBr(ADate, 'DD/MM/YY'));
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

{ CountStrTest }

procedure CountStrTest.SetUp;
begin
  // 0....+....1....+....2....+....3....+....
  AStr := 'Projeto ACBr, Teste Unitário ACBr. ACBr' ;
end;

procedure CountStrTest.AchaOcorrencia;
begin
  CheckEquals(3, CountStr(AStr, 'e'));
  CheckEquals(3, CountStr(AStr, 'ACBr'));
  CheckEquals(1, CountStr(AStr, '.'));
end;

procedure CountStrTest.NaoAchaOcorrencia;
begin
  CheckEquals(0, CountStr('z', AStr));
  CheckEquals(0, CountStr('ACBR', AStr));
end;

{ PosLastTest }

procedure PosLastTest.SetUp;
begin
  // 0....+....1....+....2....+....3....+....
  AStr := 'Projeto ACBr, Teste Unitário ACBr. ACBr' ;
end;

procedure PosLastTest.AchaOcorrencia;
begin
  CheckEquals(19, PosLast('e', AStr));
  CheckEquals(36, PosLast('ACBr', AStr));
  CheckEquals(1, PosLast('Projeto', AStr));
end;

procedure PosLastTest.NaoAchaOcorrencia;
begin
  CheckEquals(0, PosLast('z', AStr));
  CheckEquals(0, PosLast('ACBR', AStr));
end;

{ PosAtTest }

procedure PosAtTest.SetUp;
begin
       // 0....+....1....+....2....+....3....+....
  AStr := 'Projeto ACBr, Teste Unitário ACBr. ACBr' ;
end;

procedure PosAtTest.AchaPrimeiraOcorrencia;
begin
  CheckEquals(5, PosAt('e', AStr));
  CheckEquals(9, PosAt('ACBr', AStr));
end;

procedure PosAtTest.AchaSegundaOcorrencia;
begin
  CheckEquals(16, PosAt('e', AStr, 2));
  CheckEquals(30, PosAt('ACBr', AStr, 2));
end;

procedure PosAtTest.AchaTerceiraOcorrencia;
begin
  CheckEquals(19, PosAt('e', AStr, 3));
  CheckEquals(36, PosAt('ACBr', AStr, 3));
end;

procedure PosAtTest.NaoAchaOcorrencia;
begin
  CheckEquals(0, PosAt('z', AStr));
  CheckEquals(0, PosAt('ACBR', AStr));
  CheckEquals(0, PosAt('e', AStr, 4));
  CheckEquals(0, PosAt('ACBr', AStr, 4));
end;

{ RandomNameTest }

procedure RandomNameTest.TamanhoDois;
var
  AName: String;
begin
  AName := RandomName(2);
  CheckEquals(2, Length(AName));
  CheckTrue( StrIsAlpha(AName) );
end;

procedure RandomNameTest.TamanhoQuatro;
var
  AName: String;
begin
  AName := RandomName(4);
  CheckEquals(4, Length(AName));
  CheckTrue( StrIsAlpha(AName) );
end;

procedure RandomNameTest.TamanhoOito;
var
  AName: String;
begin
  AName := RandomName(8);
  CheckEquals(8, Length(AName));
  CheckTrue( StrIsAlpha(AName) );
end;

{ RemoveEmptyLinesTest }

procedure RemoveEmptyLinesTest.SetUp;
begin
  SL := TStringList.Create;
  SL.Add('');
  SL.Add('');
  SL.Add('Linha1');
  SL.Add('');
  SL.Add('Linha2');
  SL.Add('');
  SL.Add('');
  SL.Add('Linha3');
  SL.Add('');
  SL.Add('');

  RemoveEmptyLines( SL );
end;

procedure RemoveEmptyLinesTest.TearDown;
begin
  SL.Free;
end;

procedure RemoveEmptyLinesTest.VerificarLinhas;
begin
  CheckEquals( 3, SL.Count);
end;

procedure RemoveEmptyLinesTest.VerificarConteudo;
var
  Texto: String;
begin
  Texto := SL.Text;
  CheckEquals('Linha1'+sLineBreak+'Linha2'+sLineBreak+'Linha3'+sLineBreak, Texto );
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
var
  currVal, currValorUnit, currQtde, currTotal: Currency;
begin
  currValorUnit :=0.99;
  currQtde :=0.995;
  currTotal := currValorUnit * currQtde;

  // 0.99 x 0.995 = 0,98505, porém "currTotal" é um currency, que somente usa 4 casas decimais, portanto será: 0,9850
  currVal := 0.98;
  CheckEquals(currVal , RoundABNT(currTotal, 2), 0.00001);
  // RoundABNT tem um parâmetro do tipo "Double", portanto aqui todas as casas decimais serão utilizadas
  currVal := 0.99;
  CheckEquals( currVal, RoundABNT(currValorUnit * currQtde, 2), 0.00001);
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

{ ACBrStrToAnsiTest }

procedure ACBrStrToAnsiTest.ACBrStrToAnsi_TesteUTF8;
Var
  UTF8Str : AnsiString;
begin
  {$IfDef FPC}
  UTF8Str := CP1252ToUTF8('ÁÉÍÓÚ');  // Nota: essa Unit usa CP1252
  {$Else}
   {$ifdef UNICODE}
    UTF8Str := UTF8Encode('ÁÉÍÓÚ');
   {$Else}
    UTF8Str := 'ÁÉÍÓÚ';
   {$endif}
  {$EndIf}

  CheckEquals( 'ÁÉÍÓÚ', ACBrStrToAnsi(UTF8Str) );
end;

procedure ACBrStrToAnsiTest.ACBrStrToAnsi_TesteReverso;
begin
  CheckEquals( 'ÁÉÍÓÚ', ACBrStrToAnsi(ACBrStr('ÁÉÍÓÚ')) );
end;

{ DecodeToStringTest }

procedure DecodeToStringTest.DecodeToString_TesteUTF8;
Var
  UTF8Str : AnsiString;
begin
  {$IfDef FPC}
  UTF8Str := CP1252ToUTF8('ÁÉÍÓÚ');  // Nota: essa Unit usa CP1252
  {$Else}
  UTF8Str := UTF8Encode('ÁÉÍÓÚ');
  {$EndIf}

  CheckEquals(ACBrStr('ÁÉÍÓÚ'), DecodeToString(UTF8Str, True));
end;

procedure DecodeToStringTest.DecodeToString_TesteAnsi;
Var
  AnsiStr : AnsiString;
begin
  AnsiStr := 'ÁÉÍÓÚ';  // Nota: essa Unit usa CP1252
  CheckEquals(ACBrStr('ÁÉÍÓÚ'), DecodeToString(AnsiStr, False));
end;

{ TiraAcentoTest }

procedure TiraAcentoTest.Normal;
begin
   // Nota: essa Unit usa CP1252
   CheckEquals('a', TiraAcento('á'));
   CheckEquals('a', TiraAcento('à'));
   CheckEquals('a', TiraAcento('ã'));
   CheckEquals('a', TiraAcento('ä'));
   CheckEquals('a', TiraAcento('â'));
   CheckEquals('A', TiraAcento('À'));
   CheckEquals('A', TiraAcento('Á'));
   CheckEquals('A', TiraAcento('Ã'));
   CheckEquals('A', TiraAcento('Ä'));
   CheckEquals('A', TiraAcento('Â'));
   CheckEquals('e', TiraAcento('è'));
   CheckEquals('e', TiraAcento('é'));
   CheckEquals('e', TiraAcento('ë'));
   CheckEquals('e', TiraAcento('ê'));
   CheckEquals('E', TiraAcento('È'));
   CheckEquals('E', TiraAcento('É'));
   CheckEquals('E', TiraAcento('Ë'));
   CheckEquals('E', TiraAcento('Ê'));
   CheckEquals('i', TiraAcento('ì'));
   CheckEquals('i', TiraAcento('í'));
   CheckEquals('i', TiraAcento('ï'));
   CheckEquals('i', TiraAcento('î'));
   CheckEquals('I', TiraAcento('Ì'));
   CheckEquals('I', TiraAcento('Í'));
   CheckEquals('I', TiraAcento('Ï'));
   CheckEquals('I', TiraAcento('Î'));
   CheckEquals('o', TiraAcento('ò'));
   CheckEquals('o', TiraAcento('ó'));
   CheckEquals('o', TiraAcento('õ'));
   CheckEquals('o', TiraAcento('ö'));
   CheckEquals('o', TiraAcento('ô'));
   CheckEquals('O', TiraAcento('Ò'));
   CheckEquals('O', TiraAcento('Ó'));
   CheckEquals('O', TiraAcento('Õ'));
   CheckEquals('O', TiraAcento('Ö'));
   CheckEquals('O', TiraAcento('Ô'));
   CheckEquals('u', TiraAcento('ù'));
   CheckEquals('u', TiraAcento('ú'));
   CheckEquals('u', TiraAcento('ü'));
   CheckEquals('u', TiraAcento('û'));
   CheckEquals('U', TiraAcento('Ù'));
   CheckEquals('U', TiraAcento('Ú'));
   CheckEquals('U', TiraAcento('Ü'));
   CheckEquals('U', TiraAcento('Û'));
   CheckEquals('c', TiraAcento('ç'));
   CheckEquals('C', TiraAcento('Ç'));
   CheckEquals('n', TiraAcento('ñ'));
   CheckEquals('N', TiraAcento('Ñ'));
end;

{ TiraAcentosTest }

procedure TiraAcentosTest.Normal;
begin
  CheckEquals('TesteACBrUtil', TiraAcentos( ACBrStr('TêstéÃCBrÜtìl')) );
end;

procedure TiraAcentosTest.ComQuebrasDeLinha;
var
  AStr: String;
begin
  AStr := 'idLote=1'#13#10'[EVENTO001]'#13#10'tpAmb=2'#13#10+
          'chNFe=35170205481336000137550040000001361002146742'#13#10+
          'CNPJ=05481336000137'#13#10'dhEvento=15/02/2017 17:42:11'#13#10+
          'tpEvento=110110'#13#10'nSeqEvento=1'#13#10'versaoEvento=1.00'#13#10+
          'xCorrecao=012345678901234567890123456789012345678901234567890123456789abcde'#13#10;
  CheckEquals(Astr, TiraAcentos(AStr) );
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

{ OnlyAlphaNumTest }

procedure OnlyAlphaNumTest.Texto;
begin
  CheckEquals('TesteACBr', OnlyAlphaNum('TesteACBr'));
end;

procedure OnlyAlphaNumTest.Numeros;
begin
  CheckEquals('12345', OnlyAlphaNum('12345'));
end;

procedure OnlyAlphaNumTest.TextoComNumeros;
begin
  CheckEquals('TesteACBr12345', OnlyAlphaNum('TesteACBr12345'));
end;

procedure OnlyAlphaNumTest.TextoComCaractersEspeciais;
begin
  CheckEquals('TesteACBr12345', OnlyAlphaNum('T!e@s#t$e%A&C*B(r)1_2-3=4+5"'));
end;

{ OnlyAlphaTest }

procedure OnlyAlphaTest.Texto;
begin
  CheckEquals('TesteACBr', OnlyAlpha('TesteACBr'));
end;

procedure OnlyAlphaTest.Numeros;
begin
  CheckEquals('', OnlyAlpha('12345'));
end;

procedure OnlyAlphaTest.TextoComNumeros;
begin
   CheckEquals('TesteACBr', OnlyAlpha('TesteACBr12345'));
end;

procedure OnlyAlphaTest.TextoComCaractersEspeciais;
begin
   CheckEquals('TesteACBr', OnlyAlpha('T!e@s#t$e%A&C*B(r)'));
end;

{ OnlyNumberTest }

procedure OnlyNumberTest.Texto;
begin
   CheckEquals('', OnlyNumber('TesteACBr'));
end;

procedure OnlyNumberTest.Numeros;
begin
   CheckEquals('12345', OnlyNumber('12345'));
end;

procedure OnlyNumberTest.TextoComNumeros;
begin
   CheckEquals('12345', OnlyNumber('TesteACBr12345'));
end;

procedure OnlyNumberTest.TextoComSeparadores;
begin
   CheckEquals('1234500', OnlyNumber('1.2345,00'));
end;

procedure OnlyNumberTest.TextoComCaractersEspeciais;
begin
  CheckEquals('12345', OnlyNumber('!1@2#34$5%'));
end;

{ CharIsNumTest }

procedure CharIsNumTest.Caracter;
begin
  CheckFalse(CharIsNum('A'));
end;

procedure CharIsNumTest.Numero;
begin
  CheckTrue(CharIsNum('1'));
end;

procedure CharIsNumTest.CaracterEspecial;
begin
  CheckFalse(CharIsNum('#'));
end;

{ CharIsAlphaNumTest }

procedure CharIsAlphaNumTest.Caracter;
begin
  CheckTrue(CharIsAlphaNum('A'));
end;

procedure CharIsAlphaNumTest.Numero;
begin
  CheckTrue(CharIsAlphaNum('1'));
end;

procedure CharIsAlphaNumTest.CaracterEspecial;
begin
  CheckFalse(CharIsAlphaNum('#'));
end;

{ CharIsAlphaTest }

procedure CharIsAlphaTest.Caracter;
begin
  CheckTrue(CharIsAlpha('A'));
end;

procedure CharIsAlphaTest.Numero;
begin
  CheckFalse(CharIsAlpha('1'));
end;

procedure CharIsAlphaTest.CaracterEspecial;
begin
  CheckFalse(CharIsAlpha('#'));
end;

{ StrIsNumberTest }

procedure StrIsNumberTest.Texto;
begin
  CheckFalse(StrIsNumber('TesteACBrUtil'));
end;

procedure StrIsNumberTest.Numeros;
begin
  CheckTrue(StrIsNumber('0123456789'));
end;

procedure StrIsNumberTest.TextoComSeparadores;
begin
   CheckFalse(StrIsNumber('1.2345,00'));
end;

procedure StrIsNumberTest.TextoComNumeros;
begin
   CheckFalse(StrIsNumber('TesteACBrUtil1234'));
end;

procedure StrIsNumberTest.TextoComCaractersEspeciais;
begin
   CheckFalse(StrIsNumber('_%#$@$*&!""'));
end;

{ StrIsAlphaNumTest }

procedure StrIsAlphaNumTest.Texto;
begin
  CheckTrue(StrIsAlphaNum('TesteACBrUtil'));
end;

procedure StrIsAlphaNumTest.TextoComNumeros;
begin
  CheckTrue(StrIsAlphaNum('TesteACBrUtil1234'));
end;

procedure StrIsAlphaNumTest.TextoComCaractersEspeciais;
begin
  CheckFalse(StrIsAlphaNum('_%#$@$*&!""'));
end;

procedure StrIsAlphaNumTest.TextoComCaractersAcentuados;
begin
  CheckFalse(StrIsAlphaNum('TesteACBrÃštil'));
end;

{ StrIsAlphaTest }

procedure StrIsAlphaTest.Texto;
begin
  CheckTrue(StrIsAlpha('TesteACBrUtil'));
end;

procedure StrIsAlphaTest.TextoComNumeros;
begin
  CheckFalse(StrIsAlpha('TesteACBrUtil1234'));
end;

procedure StrIsAlphaTest.TextoComCaractersEspeciais;
begin
  CheckFalse(StrIsAlpha('_%#$@$*&!""'));
end;

procedure StrIsAlphaTest.TextoComCaractersAcentuados;
begin
  CheckFalse(StrIsAlpha('TesteACBrÃštil'));
end;

{ DTtoSTest }

procedure DTtoSTest.DataEHora;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(2015,01,14,12,51,49,0);
  CheckEquals('20150114125149', DTtoS(ADateTime));;
end;

procedure DTtoSTest.DataSemHora;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDate(2015,01,14);
  CheckEquals('20150114000000', DTtoS(ADateTime));
end;

{ DtoSTest }

procedure DtoSTest.Data;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDate(2015,01,14);
  CheckEquals('20150114', DtoS(ADateTime));
end;

{ StoDTest }

procedure StoDTest.Normal;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(2015,01,14,16,28,12,0);
  CheckEquals(ADateTime, StoD('20150114162812'));
end;

procedure StoDTest.DataSemHora;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDate(2015,01,14);
  CheckEquals(ADateTime, StoD('20150114'));
end;

procedure StoDTest.DataInvalida;
begin
  CheckEquals(0, StoD('DataInvalida'));
end;

{ StringToDateTimeDefTest }

procedure StringToDateTimeDefTest.Data;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDate(2015,01,02);
  CheckEquals(ADateTime, StringToDateTimeDef('02/01/2015', Now, 'd/m/yyyy'));
end;

procedure StringToDateTimeDefTest.Hora;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeTime(12,45,12,0);
  CheckEquals(ADateTime, StringToDateTimeDef('12:45:12', Now, 'h:n:s'));
end;

procedure StringToDateTimeDefTest.DataEHora;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(2015,01,14,12,45,12,0);
  CheckEquals(ADateTime, StringToDateTimeDef('14/01/2015 12:45:12', Now, 'd/m/yyyy h:n:s'));
end;

procedure StringToDateTimeDefTest.ValorDefault;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(2015,01,14,12,45,12,0);
  // Data Errada
  CheckEquals(ADateTime, StringToDateTimeDef('30/02/2001 00:01:12', ADateTime));
  // Hora Errada
  CheckEquals(ADateTime, StringToDateTimeDef('03/02/2001 10:61:12', ADateTime));
  // Tudo Errado
  CheckEquals(ADateTime, StringToDateTimeDef('Erro', ADateTime));
end;

{ StringToDateTimeTest }

procedure StringToDateTimeTest.DataAno4Digitos;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDate(2015,02,03);
  CheckEquals(ADateTime, StringToDateTime('03/02/2015', 'd/m/yyyy'));
  CheckEquals(ADateTime, StringToDateTime('03/02/2015', 'dd/mm/yyyy'));
  CheckEquals(ADateTime, StringToDateTime('03/02/2015', 'dd-mm-yyyy'));
end;

procedure StringToDateTimeTest.DataAno2Digitos;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDate(2015,02,28);
  CheckEquals(ADateTime, StringToDateTime('28/02/15', 'd/m/yyyy'));
  CheckEquals(ADateTime, StringToDateTime('28/02/15', 'dd/mm/yy'));
  CheckEquals(ADateTime, StringToDateTime('28/02/15', 'dd-mm-yy'));
end;

procedure StringToDateTimeTest.Hora;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeTime(12,45,12,0);
  CheckEquals(ADateTime, StringToDateTime('12:45:12'));
end;

procedure StringToDateTimeTest.DataEHora;
var
  ADateTime: TDateTime;
begin
  ADateTime := EncodeDateTime(2015,01,14,12,45,12,0);
  CheckEquals(ADateTime, StringToDateTime('14/01/2015 12:45:12', 'd/m/yyyy h:n:s'));
end;

procedure StringToDateTimeTest.ComFormatSettingsDiferente;
var
  OldDateSeprator, OldTimeSeparator: Char ;
  ADateTime: TDateTime;
begin
  OldDateSeprator := DateSeparator ;
  OldTimeSeparator := TimeSeparator;
  try
    DateSeparator := '-';
    TimeSeparator := ';';

    ADateTime := EncodeDateTime(2015,01,14,12,45,12,0);
    CheckEquals(ADateTime, StringToDateTime('14-01-2015 12;45;12', 'd/m/yyyy h:n:s'));
    CheckEquals(ADateTime, StringToDateTime('14/01/2015 12:45:12', 'd/m/yyyy h:n:s'));
  finally
    DateSeparator := OldDateSeprator;
    TimeSeparator := OldTimeSeparator;
  end ;
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
  CheckEquals(0, StringToFloatDef('12,12,1', 0));
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

{ Poem_ZerosTest }

procedure Poem_ZerosTest.ParamString;
begin
  CheckEquals('001', Poem_Zeros('1', 3));
  CheckEquals('000000TesteACBr', Poem_Zeros('TesteACBr', 15));
  CheckEquals('000000000000000', Poem_Zeros('', 15));
end;

procedure Poem_ZerosTest.Truncando;
begin
  CheckEquals('123', Poem_Zeros('12345', 3));
end;

procedure Poem_ZerosTest.ParamInt64;
begin
  CheckEquals('001', Poem_Zeros(1, 3));
  CheckEquals('123', Poem_Zeros(12345, 3));
  CheckEquals('000000000000000', Poem_Zeros(0, 15));
end;

{ IfEmptyThenTest }

procedure IfEmptyThenTest.RetornarValorNormal;
begin
  CheckEquals('ACBrTeste', IfEmptyThen('ACBrTeste', 'ValorPadrao'));
end;

procedure IfEmptyThenTest.SeVazioRetornaValorPadrao;
begin
  CheckEquals('ValorPadrao', IfEmptyThen('', 'ValorPadrao'));
end;

procedure IfEmptyThenTest.RealizarDoTrim;
begin
  CheckEquals('ValorPadrao', IfEmptyThen('      ', 'ValorPadrao', true));
  CheckEquals('ValorPadrao', IfEmptyThen('      ', 'ValorPadrao'));
end;

procedure IfEmptyThenTest.NaoRealizarDoTrim;
begin
  CheckEquals('ACBrTeste  ', IfEmptyThen('ACBrTeste  ', 'ValorPadrao', false));
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
  CheckEquals('FPCUnit de TestesACBrUtil, Testes Unitários', StripHTML('<!DOCTYPE html>'
                           +'<html>'
                               +'<head>'
                                   +'FPCUnit de Testes'
                               +'</head>'
                               +'<body>'
                                   +'ACBrUtil, Testes Unitários'
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
    SL.Add('<CE>*** TESTE DE TAGS INVÁLIDAS ***</CE>');
    SL.Add('<ce> <>tags inválidas no texto">">><<</CE>');
    SL.Add('<AD><da><ec></</A Direita</ad>');
    SL.Add('</corte_total>');

    Striped := StripHTML(SL.Text);

    SL.Clear;
    SL.Add('');
    SL.Add('*** TESTE DE TAGS INVÁLIDAS ***');
    SL.Add(' tags inválidas no texto">">><<');
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

{ RemoveStringsTest }

procedure RemoveStringsTest.SetUp;
begin
  StringsToRemove[1] := 'a';
  StringsToRemove[2] := 'b';
  StringsToRemove[3] := 'c';
  StringsToRemove[4] := 'te';
  StringsToRemove[5] := 'AC';
end;

procedure RemoveStringsTest.TextoSimples;
begin
  CheckEquals('s', RemoveStrings('testeabc', StringsToRemove));
end;

procedure RemoveStringsTest.TextoLongo;
begin
  CheckEquals('Tes Unitrio BrUtil ', RemoveStrings('Teste Unitario ACBrUtil ', StringsToRemove));
end;

{ RemoveStringTest }

procedure RemoveStringTest.Remover;
begin
  CheckEquals('TstACBr', RemoveString('e', 'TesteACBr'));
  CheckEquals('#####', RemoveString('ACBr', '#ACBr#ACBr#ACBr#ACBr#'));
end;

{ RemoverEspacosDuplosTest }

procedure RemoverEspacosDuplosTest.RemoverApenasEspacosDuplos;
begin
  CheckEquals('Teste ACBr', RemoverEspacosDuplos('  Teste  ACBr  '));
end;

procedure RemoverEspacosDuplosTest.RemoverMaisQueDoisEspacos;
begin
  CheckEquals('Teste ACBr Com FPCUnit', RemoverEspacosDuplos('Teste    ACBr Com  FPCUnit     '));
end;

{ padSpaceTest }

procedure padSpaceTest.CompletarString;
begin
  CheckEquals('TesteACBrZZZZZZ', PadSpace('TesteACBr', 15, '|', 'Z'));
  CheckEquals('TesteACBr      ', PadSpace('TesteACBr', 15, '|'));
end;

procedure padSpaceTest.TruncarString;
begin
  CheckEquals('TesteACBr', PadSpace('TesteACBrZZZZZZ', 9, '|'));
end;

procedure padSpaceTest.SubstituirSeparadorPorEspacos;
begin
  CheckEquals(' Teste Unitario ACBr ', PadSpace('|Teste|Unitario|ACBr|', 21, '|'));
  CheckEquals('   Teste   Unitario   ACBr    ', PadSpace('|Teste|Unitario|ACBr|', 30, '|'));
end;

procedure padSpaceTest.SubstituirSeparadorPorCaracter;
begin
  CheckEquals('ZTesteZUnitarioZACBrZ', PadSpace('|Teste|Unitario|ACBr|', 21, '|', 'Z'));
  CheckEquals('ZZZTesteZZZUnitarioZZZACBrZZZZ', PadSpace('|Teste|Unitario|ACBr|', 30, '|', 'Z'));
end;

{ padCenterTest }

procedure padCenterTest.PreencherString;
begin
  CheckEquals('ZZZTESTEZZZZ', PadCenter('TESTE', 12, 'Z'));
  CheckEquals('ZZZZTESTEZZZZ', PadCenter('TESTE', 13, 'Z'));
  CheckEquals('    TESTE    ', PadCenter('TESTE', 13));
end;

procedure padCenterTest.TruncarString;
begin
  CheckEquals('TesteACBr', PadCenter('TesteACBrUtil', 9));
end;

{ padLeftTest }

procedure padLeftTest.CompletarString;
begin
  CheckEquals('ZZZACBrCompletaString', PadLeft('ACBrCompletaString', 21, 'Z'));
  CheckEquals('   ACBrCompletaString', PadLeft('ACBrCompletaString', 21));
end;

procedure padLeftTest.CompletarStringAcentosAnsi;
var
  AcentosStr: String;
begin
  AcentosStr := ACBrStr('ÁÉÍÓÚ');

  CheckEquals('     '+AcentosStr, PadLeft(AcentosStr, 10));
end;

procedure padLeftTest.CompletarStringAcentosUTF8;
Var
  UTF8Str : AnsiString;
begin
  {$IfDef FPC}
  UTF8Str := CP1252ToUTF8('ÁÉÍÓÚ');  // Nota: essa Unit usa CP1252
  {$Else}
   {$IFDEF UNICODE}
    UTF8Str := 'ÁÉÍÓÚ';
   {$ELSE}
    UTF8Str := UTF8Encode('ÁÉÍÓÚ');
   {$ENDIF}
  {$EndIf}

  CheckEquals('     '+UTF8Str, PadLeft(UTF8Str, 10));
end;

procedure padLeftTest.ManterString;
begin
  CheckEquals('ACBrMantemString', PadLeft('ACBrMantemString', 16, 'Z'));
end;

procedure padLeftTest.TruncarString;
begin
  CheckEquals('ACBrTruncaSt', PadLeft('ACBrTruncaString', 12, 'Z'));
// CheckEquals('TruncaString', PadLeft('ACBrTruncaString', 12, 'Z'));
end;

{ padRightTest }

procedure padRightTest.CompletarString;
begin
  CheckEquals('ACBrCompletaStringZZZ', PadRight('ACBrCompletaString', 21, 'Z'));
  CheckEquals('ACBrCompletaString   ', PadRight('ACBrCompletaString', 21));
end;

procedure padRightTest.ManterString;
begin
  CheckEquals('ACBrMantemString', PadRight('ACBrMantemString', 16, 'Z'));
end;

procedure padRightTest.TruncarString;
begin
  CheckEquals('ACBrTrunca', PadRight('ACBrTruncaString', 10, 'Z'));
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

procedure QuebrarLinhaTest.TresCampos;
Var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    QuebrarLinha('"CAMPO1";"CAMPO2";"CAMPO3"',SL);
    CheckEquals( 'CAMPO1', SL[0]);
    CheckEquals( 'CAMPO2', SL[1]);
    CheckEquals( 'CAMPO3', SL[2]);
  finally
    SL.Free;
  end;
end;

procedure QuebrarLinhaTest.PipeDelimiter;
Var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    QuebrarLinha('CAMPO1|CAMPO2|CAMPO3',SL, ' ', '|');
    CheckEquals( 'CAMPO1', SL[0] );
    CheckEquals( 'CAMPO2', SL[1] );
    CheckEquals( 'CAMPO3', SL[2] );
  finally
    SL.Free;
  end;
end;

{ LerTagXMLTest }

procedure LerTagXMLTest.Simples;
begin
  CheckEquals('Teste Simples', LerTagXML('<ACBr>Teste Simples</ACBr>', 'acbr'));
end;

procedure LerTagXMLTest.SemIgnorarCase;
begin
  CheckEquals('Teste sem ignorar case', LerTagXML('<ACBr>Teste sem ignorar case</ACBr>', 'ACBr', false));
  CheckEquals('', LerTagXML('<ACBr>Teste sem ignorar case</ACBr>', 'acbr', false));
  CheckEquals('Ler Aqui', LerTagXML('<ACBr>Teste sem <acbr>Ler Aqui</acbr> ignorar case</ACBr>', 'acbr', false));
end;

procedure LerTagXMLTest.ComVariasTags;
begin
  CheckEquals('mais um teste', LerTagXML('<ACBr> teste <br> outro teste </br> <b>mais um teste</b> </ACBr>', 'b'));
end;

{ ParseTextTest }

procedure ParseTextTest.ParseDecode;
begin
  CheckEquals('&', ParseText('&amp;'));
  CheckEquals('<', ParseText('&lt;'));
  CheckEquals('>', ParseText('&gt;'));
  CheckEquals('"', ParseText('&quot;'));
  CheckEquals(#39, ParseText('&#39;'));
  CheckEquals(ACBrStr('á'), ParseText('&aacute;', True, False));
  CheckEquals(ACBrStr('Á'), ParseText('&Aacute;', True, False));
  CheckEquals(ACBrStr('â'), ParseText('&acirc;',  True, False));
  CheckEquals(ACBrStr('Â'), ParseText('&Acirc;',  True, False));
  CheckEquals(ACBrStr('ã'), ParseText('&atilde;', True, False));
  CheckEquals(ACBrStr('Ã'), ParseText('&Atilde;', True, False));
  CheckEquals(ACBrStr('à'), ParseText('&agrave;', True, False));
  CheckEquals(ACBrStr('À'), ParseText('&Agrave;', True, False));
  CheckEquals(ACBrStr('é'), ParseText('&eacute;', True, False));
  CheckEquals(ACBrStr('É'), ParseText('&Eacute;', True, False));
  CheckEquals(ACBrStr('ê'), ParseText('&ecirc;',  True, False));
  CheckEquals(ACBrStr('Ê'), ParseText('&Ecirc;',  True, False));
  CheckEquals(ACBrStr('í'), ParseText('&iacute;', True, False));
  CheckEquals(ACBrStr('Í'), ParseText('&Iacute;', True, False));
  CheckEquals(ACBrStr('ó'), ParseText('&oacute;', True, False));
  CheckEquals(ACBrStr('Ó'), ParseText('&Oacute;', True, False));
  CheckEquals(ACBrStr('õ'), ParseText('&otilde;', True, False));
  CheckEquals(ACBrStr('Õ'), ParseText('&Otilde;', True, False));
  CheckEquals(ACBrStr('ô'), ParseText('&ocirc;',  True, False));
  CheckEquals(ACBrStr('Ô'), ParseText('&Ocirc;',  True, False));
  CheckEquals(ACBrStr('ú'), ParseText('&uacute;', True, False));
  CheckEquals(ACBrStr('Ú'), ParseText('&Uacute;', True, False));
  CheckEquals(ACBrStr('ü'), ParseText('&uuml;',   True, False));
  CheckEquals(ACBrStr('Ü'), ParseText('&Uuml;',   True, False));
  CheckEquals(ACBrStr('ç'), ParseText('&ccedil;', True, False));
  CheckEquals(ACBrStr('Ç'), ParseText('&Ccedil;', True, False));
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
  CheckEquals( ACBrStr(#39'áÃÇÜÉÊÕ'''), ParseText('&#39;&aacute;&Atilde;&Ccedil;&Uuml;'
              + '&Eacute;&Ecirc;&Otilde;&apos;', True, False));
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

{ SplitTeste }

procedure SplitTeste.Split_DelimitadorEspaco;
var
  SR: TSplitResult;
begin
  SR := Split(' ', 'PROJETO ACBR www.projetoacbr.com.br');
  CheckEquals(3, Length(SR));
  CheckEquals('PROJETO', SR[0]);
  CheckEquals('ACBR', SR[1]);
  CheckEquals('www.projetoacbr.com.br', SR[2]);
end;

procedure SplitTeste.Split_DoisItens;
var
  SR: TSplitResult;
begin
  SR := Split(';', 'comercial@djpdv.com.br;financeiro@djpdv.com.br');
  CheckEquals(2, Length(SR));
  CheckEquals('comercial@djpdv.com.br', SR[0]);
  CheckEquals('financeiro@djpdv.com.br', SR[1]);
end;

procedure SplitTeste.Split_SemDelimitador;
var
  SR: TSplitResult;
begin
  SR := Split(';', 'comercial@djpdv.com.br');
  CheckEquals(1, Length(SR));
  CheckEquals('comercial@djpdv.com.br', SR[0]);
end;

procedure SplitTeste.Split_StringVazia;
begin
  CheckEquals(0, Length(Split(';','')));
end;

initialization

  RegisterTest('ACBrComum.ACBrUtil', AddDelimitedTextToListTeste{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', SplitTeste{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', FindDelimiterInTextTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', ChangeLineBreakTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', WorkingDaysBetweenTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', IncWorkingDayTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TiraPontosTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', ParseTextTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', LerTagXMLTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', DecodeToStringTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', SepararDadosTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', QuebrarLinhaTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', ACBrStrToAnsiTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TruncFixTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TruncToTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', RoundABNTTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', CompareVersionsTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TestBitTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', IntToBinTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', BinToIntTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', BcdToAscTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', AscToBcdTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', IntToLEStrTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', LEStrToIntTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', HexToAsciiTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', AsciiToHexTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', BinaryStringToStringTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StringToBinaryStringTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', padRightTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', padLeftTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', padCenterTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', padSpaceTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', RemoveStringTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', RemoveStringsTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', RemoverEspacosDuplosTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StripHTMLTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', RemoveEmptyLinesTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', RandomNameTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', IfEmptyThenTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', PosAtTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', PosLastTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', CountStrTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', Poem_ZerosTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', IntToStrZeroTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', FloatToIntStrTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', FloatToStringTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', FormatFloatBrTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', FloatMaskTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StringToFloatTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StringToFloatDefTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', FormatDateBrTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', FormatDateTimeBrTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StringToDateTimeTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StringToDateTimeDefTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StoDTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', DtoSTest{$ifndef FPC}.Suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', DTtoSTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StrIsAlphaTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StrIsAlphaNumTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StrIsNumberTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', CharIsAlphaTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', CharIsAlphaNumTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', CharIsNumTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', OnlyNumberTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', OnlyAlphaTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', OnlyAlphaNumTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StrIsIPTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', EstaVazio_NaoEstaVazioTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', EstaZerado_NaoEstaZeradoTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TamanhoIgualTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TamanhoMenorTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TiraAcentosTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TiraAcentoTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', AjustaLinhasTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', QuebraLinhasTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TraduzComandoTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StringToAscTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', AscToStringTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StrCryptTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', SomaAscIITest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', StringCrc16Test{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', PathWithDelimTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', PathWithoutDelimTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', TranslateUnprintableTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', EAN13Test{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', ComparaValorTest{$ifndef FPC}.suite{$endif});
  RegisterTest('ACBrComum.ACBrUtil', ZipUnzip{$ifndef FPC}.suite{$endif});
  //TODO: WriteToTXT, WriteLog,
end.

