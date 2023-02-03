unit ACBrUtil.StringsTests;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  LConvEncoding,
  {$endif}
  ACBrTests.Util;

type

  { StrIsBase64Test }

  StrIsBase64Test = class(TTestCase)
  published
    procedure TextoVazio;
    procedure TextoEmBase64SemPad;
    procedure TextoEmBase64ComUmPad;
    procedure TextoEmBase64ComDoisPads;
    procedure TextoEmBase64TamanhoErrado;
    procedure TextoEmBase64ComExcessoDePad;
    procedure TextoEmBase64ComEspacos;
    procedure TextoNaoBase64;
    procedure TextoComCaractersEspeciais;
  end;

  { StrIsBinaryTest }

  StrIsBinaryTest = class(TTestCase)
  published
    procedure TextoVazio;
    procedure TextoEmBinario;
    procedure TextoEmBinarioComEspacos;
    procedure TextoNaoBinario;
    procedure TextoComCaractersEspeciais;
  end;

  { StrIsHexaTest }

  StrIsHexaTest = class(TTestCase)
  published
    procedure TextoVazio;
    procedure TextoEmHexaMaiusculo;
    procedure TextoEmHexaMinusculo;
    procedure TextoEmHexaComEspacos;
    procedure TextoNaoHexa;
    procedure TextoComCaractersEspeciais;
  end;

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
    procedure AddDelimitedTextToListTeste_ComDuasAspasSimplesQuoteAspasDuplas;
    procedure AddDelimitedTextToListTeste_ComUmaAspasDuplasComQuote;
    procedure AddDelimitedTextToListTeste_ComUmaAspasDuplasSemQuote;
    procedure AddDelimitedTextToListTeste_ComDuasAspasDuplaComQuote;
    procedure AddDelimitedTextToListTeste_ComDuasAspasDuplaSemQuote;
    procedure AddDelimitedTextToListTeste_ComDelimitadorEntreAspasSimplesQuoteAspasSimples;
    procedure AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasComQuoteInvalido;
    procedure AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasSemQuote;
    procedure AddDelimitedTextToListTeste_ComDelimitadoresVazios;
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

  { QuebraLinhasTest }

  QuebraLinhasTest = class(TTestCase)
  private
    AStr: String;
    FTexto: String;
  protected
    procedure SetUp; override;
  published
    procedure QuebraEmNoventaColunas;
    procedure QuebraEmQuarentaColunas;
    procedure QuebraEmCinquentaComSeparadorE;
    procedure QuebraStrComLineBreakEm32cols;
    procedure QuebraDuasLinhasNoLimiteColuna;
    procedure QuebraLinhaComVariosCRLFEm12;
    procedure QuebraLinhaComVariosCRLFEm24;
  end;

  { QuebrarLinhaTest }

  QuebrarLinhaTest = class(TTestCase)
  published
    procedure TresCampos;
    procedure PipeDelimiter;
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

  { RandomNameTest }

  RandomNameTest = class(TTestCase)
  published
   procedure TamanhoDois;
   procedure TamanhoQuatro;
   procedure TamanhoOito;
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

  { ACBrStrToAnsiTest }

  ACBrStrToAnsiTest = class(TTestCase)
  published
    procedure ACBrStrToAnsi_TesteUTF8;
    procedure ACBrStrToAnsi_TesteReverso;
  end;

  { DecodeToStringTest }

  DecodeToStringTest = class(TTestCase)
  published
    procedure DecodeToString_TesteUTF8;
    procedure DecodeToString_TesteAnsi;
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

  { OnlyAlphaNumTest }

  OnlyAlphaNumTest = class(TTestCase)
  published
    procedure Texto;
    procedure Numeros;
    procedure TextoComNumeros;
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

  { CharIsNumTest }

  CharIsNumTest = class(TTestCase)
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

  { CharIsAlphaTest }

  CharIsAlphaTest = class(TTestCase)
  published
    procedure Caracter;
    procedure Numero;
    procedure CaracterEspecial;
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

  { StrIsAlphaNumTest }

  StrIsAlphaNumTest = class(TTestCase)
  published
    procedure Texto;
    procedure TextoComNumeros;
    procedure TextoComCaractersEspeciais;
    procedure TextoComCaractersAcentuados;
  end;

  { StrIsAlphaTest }

  StrIsAlphaTest = class(TTestCase)
  published
   procedure Texto;
   procedure TextoComNumeros;
   procedure TextoComCaractersEspeciais;
   procedure TextoComCaractersAcentuados;
  end;

  { Poem_ZerosTest }

  Poem_ZerosTest = class(TTestCase)
  published
    procedure ParamString;
    procedure Truncando;
    procedure ParamInt64;
  end;


  { IfEmptyThenTest }

  IfEmptyThenTest = class(TTestCase)
  published
   procedure RetornarValorNormal;
   procedure SeVazioRetornaValorPadrao;
   procedure RealizarDoTrim;
   procedure NaoRealizarDoTrim;
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

  { RemoveStringTest }

  RemoveStringTest = class(TTestCase)
  published
   procedure Remover;
  end;

  { RemoverEspacosDuplosTest }

  RemoverEspacosDuplosTest = class(TTestCase)
  published
   procedure RemoverApenasEspacosDuplos;
   procedure RemoverMaisQueDoisEspacos;
  end;

  { padSpaceTest }

  padSpaceTest = class(TTestCase)
  published
   procedure CompletarString;
   procedure TruncarString;
   procedure SubstituirSeparadorPorEspacos;
   procedure SubstituirSeparadorPorCaracter;
   procedure NaoRemoverEspacos;
  end;

  { padCenterTest }

  padCenterTest = class(TTestCase)
  published
   procedure PreencherString;
   procedure TruncarString;
  end;

  { padLeftTest }

  padLeftTest = class(TTestCase)
  published
   procedure CompletarString;
   procedure CompletarStringAcentosAnsi;
   procedure CortarStringAcentosAnsi;
   procedure CompletarStringAcentosUTF8;
   procedure CortarStringAcentosUTF8;
   procedure ManterString;
   procedure TruncarString;
  end;

  { padRightTest }

  padRightTest = class(TTestCase)
  published
    procedure CompletarStringComAcentos;
    procedure CortarStringComAcentos;
    procedure CompletarString;
    procedure ManterString;
    procedure TruncarString;
  end;

  { Split }

  SplitTeste = class(TTestCase)
  published
    procedure Split_StringVazia;
    procedure Split_DoisItens;
    procedure Split_SemDelimitador;
    procedure Split_DelimitadorEspaco;
  end;

  { RemoverQuebraLinhaFinal }
  RemoverQuebraLinhaFinalTest = class(TTestCase)
  private
  published
    procedure UmaLinhaComQuebraPadrao;
    procedure UmaLinhaSemQuebraPadrao;
    procedure UmaLinhaComQuebraDiferenciada;
    procedure UmaLinhaSemQuebraDiferenciada;
    procedure DuasLinhasComQuebraPadrao;
    procedure DuasLinhasSemQuebraPadrao;
    procedure DuasLinhasComQuebraDiferenciada;
    procedure DuasLinhasSemQuebraDiferenciada;
  end;



implementation

uses
  ACBrConsts, ACBrUtil.Strings;

{ StrIsBase64Test }

procedure StrIsBase64Test.TextoVazio;
begin
  CheckFalse(StrIsBase64(''));
end;

procedure StrIsBase64Test.TextoEmBase64SemPad;
begin
  CheckTrue(StrIsBase64('UHJvamV0byBBQ0Jy'));
end;

procedure StrIsBase64Test.TextoEmBase64ComUmPad;
begin
  CheckTrue(StrIsBase64('UHJvamV0b0FDQnI='));
end;

procedure StrIsBase64Test.TextoEmBase64ComDoisPads;
begin
  CheckTrue(StrIsBase64('UHJvamV0b0FDQg=='));
end;

procedure StrIsBase64Test.TextoEmBase64TamanhoErrado;
begin
  CheckFalse(StrIsBase64('UHJvamV0byBBQ0J'));
  CheckFalse(StrIsBase64('UHJvamV0byBBQ0Jy='));
end;

procedure StrIsBase64Test.TextoEmBase64ComExcessoDePad;
begin
  CheckFalse(StrIsBase64('UHJvamV0b0FDQ==='));
end;

procedure StrIsBase64Test.TextoEmBase64ComEspacos;
begin
  CheckFalse(StrIsBase64('UHJv amV0 byBB Q0Jy'));
end;

procedure StrIsBase64Test.TextoNaoBase64;
begin
  CheckFalse(StrIsBase64('Projeto ACBr'));
end;

procedure StrIsBase64Test.TextoComCaractersEspeciais;
begin
  CheckFalse(StrIsBase64('Projeto@ACBR#123.90'));
end;

{ StrIsBinaryTest }

procedure StrIsBinaryTest.TextoVazio;
begin
  CheckTrue(StrIsBinary(''));
end;

procedure StrIsBinaryTest.TextoEmBinario;
begin
  CheckTrue(StrIsBinary('0001110111110000'));
end;

procedure StrIsBinaryTest.TextoEmBinarioComEspacos;
begin
  CheckFalse(StrIsBinary('00011 1011 1110 000'));
end;

procedure StrIsBinaryTest.TextoNaoBinario;
begin
  CheckFalse(StrIsBinary('ProjetoACBR'));
end;

procedure StrIsBinaryTest.TextoComCaractersEspeciais;
begin
  CheckFalse(StrIsBinary('Projeto@ACBR#123.90'));
end;

{ StrIsHexaTest }

procedure StrIsHexaTest.TextoVazio;
begin
  CheckTrue(StrIsHexa(''));
end;

procedure StrIsHexaTest.TextoEmHexaMaiusculo;
begin
  CheckTrue(StrIsHexa('1234567890ABCDEF'));
end;

procedure StrIsHexaTest.TextoEmHexaMinusculo;
begin
  CheckTrue(StrIsHexa('1234567890abcdef'));
end;

procedure StrIsHexaTest.TextoEmHexaComEspacos;
begin
  CheckFalse(StrIsHexa('0A 12 13 A6 DF FF'));
end;

procedure StrIsHexaTest.TextoNaoHexa;
begin
  CheckFalse(StrIsHexa('ProjetoACBR'));
end;

procedure StrIsHexaTest.TextoComCaractersEspeciais;
begin
  CheckFalse(StrIsHexa('Projeto@ACBR#123.90'));
end;

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
  CheckEquals(3, AddDelimitedTextToList('PROJETO|AC''BR|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('AC''BR', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDuasAspasSimplesQuoteAspasDuplas;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|''ACBR''|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('''ACBR''', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComUmaAspasDuplasComQuote;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|AC"BR|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('AC"BR', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComUmaAspasDuplasSemQuote;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|AC"BR|www.projetoacbr.com.br','|',FSL, #0 ));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('AC"BR', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDuasAspasDuplaComQuote;
begin
  CheckEquals(4, AddDelimitedTextToList('PROJETO|"A|C"|"B|R"|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('A|C', FSL[1]);
  CheckEquals('B|R', FSL[2]);
  CheckEquals('www.projetoacbr.com.br', FSL[3]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDuasAspasDuplaSemQuote;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO|"ACBR"|www.projetoacbr.com.br','|',FSL, #0 ));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('"ACBR"', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDelimitadorEntreAspasSimplesQuoteAspasSimples;
begin
  CheckEquals(3, AddDelimitedTextToList('PROJETO ''A C B R'' www.projetoacbr.com.br',' ',FSL, ''''));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('A C B R', FSL[1]);
  CheckEquals('www.projetoacbr.com.br', FSL[2]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasComQuoteInvalido;
begin
  CheckEquals(6, AddDelimitedTextToList('PROJETO|A"A|C|B|R"|www.projetoacbr.com.br','|',FSL));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('A"A', FSL[1]);
  CheckEquals('C', FSL[2]);
  CheckEquals('B', FSL[3]);
  CheckEquals('R"', FSL[4]);
  CheckEquals('www.projetoacbr.com.br', FSL[5]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDelimitadorEntreAspasDuplasSemQuote;
begin
  CheckEquals(6, AddDelimitedTextToList('PROJETO A"A C B R" www.projetoacbr.com.br',' ',FSL, #0));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('A"A', FSL[1]);
  CheckEquals('C', FSL[2]);
  CheckEquals('B', FSL[3]);
  CheckEquals('R"', FSL[4]);
  CheckEquals('www.projetoacbr.com.br', FSL[5]);
end;

procedure AddDelimitedTextToListTeste.AddDelimitedTextToListTeste_ComDelimitadoresVazios;
begin
  CheckEquals(7, AddDelimitedTextToList('PROJETO||||ACBR||www.projetoacbr.com.br','|',FSL, #0));
  CheckEquals('PROJETO', FSL[0]);
  CheckEquals('', FSL[1]);
  CheckEquals('', FSL[2]);
  CheckEquals('', FSL[3]);
  CheckEquals('ACBR', FSL[4]);
  CheckEquals('', FSL[5]);
  CheckEquals('www.projetoacbr.com.br', FSL[6]);
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

{ TiraPontosTest }

procedure TiraPontosTest.TiraPontos_StringComEspacos_RetornaMeioSemEspacos;
begin
  CheckEquals('AaBbCcDdÁ123', TiraPontos('/-.) (,/-. )AaBbCc DdÁ 1 23 (,,( ).-/'));

end;

procedure TiraPontosTest.TiraPontos_StringExtremosInvalidos_RetornaMeio;
begin
  CheckEquals('AaBbCcDdÁ123', TiraPontos('/-.)(,/-.)AaBbCcDdÁ123(,,().-/'));

end;

procedure TiraPontosTest.TiraPontos_StringFimInvalido_RetornaInicio;
begin
  CheckEquals('AaBbCcDdÁ123', TiraPontos('AaBbCcDdÁ123/-.)(,/-.)(,,().-/'));

end;

procedure TiraPontosTest.TiraPontos_StringInicioInvalido_RetornaFinal;
begin
  CheckEquals('AaBbCcDdÁ123', TiraPontos('/-.)(,/-.)(,,().-/AaBbCcDdÁ123'));
end;

procedure TiraPontosTest.TiraPontos_StringInvalida_RetornaVazio;
begin
  CheckEquals('', TiraPontos('/-.)(,/-.)(,,().-/'));
end;

procedure TiraPontosTest.TiraPontos_StringMeioInvalido_RetornaExtremos;
begin
  CheckEquals('AaBbCcDdÁ123', TiraPontos('AaBbCc/-.)(,/-.)(,,().-/DdÁ123'));
end;

{ QuebraLinhasTest }

procedure QuebraLinhasTest.SetUp;
begin
  // Nota Essa Unit est· em CP1252
  //              0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....
  AStr := ACBrStr('Dez Milhıes e Duzentos e Cinquenta e Cinco Mil e Quatrocentos e Trinta e Cinco Reais');
  FTexto := 'Projeto ACBr'+sLineBreak+sLineBreak+
            'www.projetoacbr.com.br'+sLineBreak+sLineBreak+sLineBreak+sLineBreak+
            '123456 123456789 1234567890';
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
  CheckEquals(ACBrStr('Dez Milhıes e Duzentos e Cinquenta e ')+sLineBreak+
              ACBrStr('Cinco Mil e Quatrocentos e Trinta e ')+sLineBreak+
              ACBrStr('Cinco Reais') ,Resp );
end;

procedure QuebraLinhasTest.QuebraEmCinquentaComSeparadorE;
var
  Resp: String;
begin
  Resp := QuebraLinhas(AStr,50,'e');
                //   0....+....1....+....2....+....3....+....4....+....5
  CheckEquals(ACBrStr('Dez Milhıes e Duzentos e Cinquenta e Cinco Mil e')+sLineBreak+
              ACBrStr(' Quatrocentos e Trinta e Cinco Reais'), Resp );
end;

procedure QuebraLinhasTest.QuebraStrComLineBreakEm32cols;
var
  Resp: String;
  AMsg1, AMsg2: String;
begin
  AMsg1 := 'Erro ao enviar Dados da Venda:'+sLineBreak+
           '539-RejeiÁ„o: Duplicidade de NF-e com diferenÁa na Chave de Acesso'+sLineBreak+
           '[chNFe:35150705481336000137650220000000031000000626]'+sLineBreak+
           '[nRec:351000006395525]'+sLineBreak+
           'Tentar novamente ?';

      //   0....+....1....+....2....+....3....+....4....+....5
  AMsg2 := 'Erro ao enviar Dados da Venda:'+sLineBreak+
           '539-RejeiÁ„o: Duplicidade de '+sLineBreak+
           'NF-e com diferenÁa na Chave de '+sLineBreak+
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
  AMsg1 := 'EndereÁo de Entrega sem Cod.IBGE'+sLineBreak+
           'Tentar novamente ?';

      //   0....+....1....+....2....+....3....+....4....+....5

  Resp := QuebraLinhas( ACBrStr(AMsg1), 32);

  CheckEquals( ACBrStr(AMsg1), Resp );
end;

procedure QuebraLinhasTest.QuebraLinhaComVariosCRLFEm12;
var
  Texto12: String;
begin
           // 123456789012
  Texto12 := 'Projeto ACBr'+sLineBreak+sLineBreak+
             'www.projetoa'+sLineBreak+
             'cbr.com.br'+sLineBreak+sLineBreak+sLineBreak+sLineBreak+
             '123456 '+sLineBreak+
             '123456789 '+sLineBreak+
             '1234567890';

  CheckEquals(Texto12, QuebraLinhas(FTexto,12));
end;

procedure QuebraLinhasTest.QuebraLinhaComVariosCRLFEm24;
var
  Texto24: String;
begin
           // 123456789012345678901234
  Texto24 := 'Projeto ACBr'+sLineBreak+sLineBreak+
             'www.projetoacbr.com.br'+sLineBreak+sLineBreak+sLineBreak+sLineBreak+
             '123456 123456789 '+sLineBreak+
             '1234567890';

  CheckEquals(Texto24, QuebraLinhas(FTexto,24));
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

{ CountStrTest }

procedure CountStrTest.SetUp;
begin
  // 0....+....1....+....2....+....3....+....
  AStr := 'Projeto ACBr, Teste Unit·rio ACBr. ACBr' ;
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

{ PosAtTest }

procedure PosAtTest.SetUp;
begin
       // 0....+....1....+....2....+....3....+....
  AStr := 'Projeto ACBr, Teste Unit·rio ACBr. ACBr' ;
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

{ PosLastTest }

procedure PosLastTest.SetUp;
begin
  // 0....+....1....+....2....+....3....+....
  AStr := 'Projeto ACBr, Teste Unit·rio ACBr. ACBr' ;
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

{ ACBrStrToAnsiTest }

procedure ACBrStrToAnsiTest.ACBrStrToAnsi_TesteUTF8;
Var
  UTF8Str : AnsiString;
begin
  {$IfDef FPC}
  UTF8Str := CP1252ToUTF8('¡…Õ”⁄');  // Nota: essa Unit usa CP1252
  {$Else}
   {$ifdef UNICODE}
    UTF8Str := UTF8Encode('¡…Õ”⁄');
   {$Else}
    UTF8Str := '¡…Õ”⁄';
   {$endif}
  {$EndIf}

  CheckEquals( '¡…Õ”⁄', ACBrStrToAnsi(UTF8Str) );
end;

procedure ACBrStrToAnsiTest.ACBrStrToAnsi_TesteReverso;
begin
  CheckEquals( '¡…Õ”⁄', ACBrStrToAnsi(ACBrStr('¡…Õ”⁄')) );
end;

{ DecodeToStringTest }

procedure DecodeToStringTest.DecodeToString_TesteUTF8;
Var
  UTF8Str : AnsiString;
begin
  {$IfDef FPC}
  UTF8Str := CP1252ToUTF8('¡…Õ”⁄');  // Nota: essa Unit usa CP1252
  {$Else}
  UTF8Str := UTF8Encode('¡…Õ”⁄');
  {$EndIf}

  CheckEquals(ACBrStr('¡…Õ”⁄'), DecodeToString(UTF8Str, True));
end;

procedure DecodeToStringTest.DecodeToString_TesteAnsi;
Var
  AnsiStr : AnsiString;
begin
  AnsiStr := '¡…Õ”⁄';  // Nota: essa Unit usa CP1252
  CheckEquals(ACBrStr('¡…Õ”⁄'), DecodeToString(AnsiStr, False));
end;

{ TiraAcentosTest }

procedure TiraAcentosTest.Normal;
begin
  CheckEquals('TesteACBrUtil', TiraAcentos( ACBrStr('TÍstÈ√CBr‹tÏl')) );
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

{ TiraAcentoTest }

procedure TiraAcentoTest.Normal;
begin
   // Nota: essa Unit usa CP1252
   CheckEquals('a', TiraAcento('·'));
   CheckEquals('a', TiraAcento('‡'));
   CheckEquals('a', TiraAcento('„'));
   CheckEquals('a', TiraAcento('‰'));
   CheckEquals('a', TiraAcento('‚'));
   CheckEquals('A', TiraAcento('¿'));
   CheckEquals('A', TiraAcento('¡'));
   CheckEquals('A', TiraAcento('√'));
   CheckEquals('A', TiraAcento('ƒ'));
   CheckEquals('A', TiraAcento('¬'));
   CheckEquals('e', TiraAcento('Ë'));
   CheckEquals('e', TiraAcento('È'));
   CheckEquals('e', TiraAcento('Î'));
   CheckEquals('e', TiraAcento('Í'));
   CheckEquals('E', TiraAcento('»'));
   CheckEquals('E', TiraAcento('…'));
   CheckEquals('E', TiraAcento('À'));
   CheckEquals('E', TiraAcento(' '));
   CheckEquals('i', TiraAcento('Ï'));
   CheckEquals('i', TiraAcento('Ì'));
   CheckEquals('i', TiraAcento('Ô'));
   CheckEquals('i', TiraAcento('Ó'));
   CheckEquals('I', TiraAcento('Ã'));
   CheckEquals('I', TiraAcento('Õ'));
   CheckEquals('I', TiraAcento('œ'));
   CheckEquals('I', TiraAcento('Œ'));
   CheckEquals('o', TiraAcento('Ú'));
   CheckEquals('o', TiraAcento('Û'));
   CheckEquals('o', TiraAcento('ı'));
   CheckEquals('o', TiraAcento('ˆ'));
   CheckEquals('o', TiraAcento('Ù'));
   CheckEquals('O', TiraAcento('“'));
   CheckEquals('O', TiraAcento('”'));
   CheckEquals('O', TiraAcento('’'));
   CheckEquals('O', TiraAcento('÷'));
   CheckEquals('O', TiraAcento('‘'));
   CheckEquals('u', TiraAcento('˘'));
   CheckEquals('u', TiraAcento('˙'));
   CheckEquals('u', TiraAcento('¸'));
   CheckEquals('u', TiraAcento('˚'));
   CheckEquals('U', TiraAcento('Ÿ'));
   CheckEquals('U', TiraAcento('⁄'));
   CheckEquals('U', TiraAcento('‹'));
   CheckEquals('U', TiraAcento('€'));
   CheckEquals('c', TiraAcento('Á'));
   CheckEquals('C', TiraAcento('«'));
   CheckEquals('n', TiraAcento('Ò'));
   CheckEquals('N', TiraAcento('—'));
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
  CheckFalse(StrIsAlphaNum('TesteACBr√ötil'));
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
  CheckFalse(StrIsAlpha('TesteACBr√ötil'));
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

procedure padSpaceTest.NaoRemoverEspacos;
begin
  CheckEquals('      190,25      KG', PadSpace('      190,25|KG', 20, '|', ' ', False));
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
  AcentosStr := ACBrStr('¡…Õ”⁄');

  CheckEquals('     '+AcentosStr, PadLeft(AcentosStr, 10));
end;

procedure padLeftTest.CortarStringAcentosAnsi;
var
  Str1, Str2: String;
begin
  Str1 := ACBrStr('¡…Õ”⁄«');
  Str2 := ACBrStr('¡…Õ');

  CheckEquals(Str2, PadLeft(Str1, 3));
end;

procedure padLeftTest.CompletarStringAcentosUTF8;
Var
  UTF8Str: String;
begin
  {$IfDef FPC}
  UTF8Str := CP1252ToUTF8('¡…Õ”⁄');  // Nota: essa Unit usa CP1252
  {$Else}
   UTF8Str := UTF8Encode('¡…Õ”⁄');
  {$EndIf}

  //D7 ir· falhar
  CheckEquals('     '+UTF8Str, PadLeft(UTF8Str, 10));
end;

procedure padLeftTest.CortarStringAcentosUTF8;
Var
  UTF8Str1, UTF8Str2: String;
begin
  {$IfDef FPC}
  UTF8Str1 := CP1252ToUTF8('¡…Õ”⁄');  // Nota: essa Unit usa CP1252
  UTF8Str2 := CP1252ToUTF8('¡…Õ');  // Nota: essa Unit usa CP1252
  {$Else}
   UTF8Str1 := UTF8Encode('¡…Õ”⁄');
   UTF8Str2 := UTF8Encode('¡…Õ');
  {$EndIf}

  //D7 ir· falhar
  CheckEquals(UTF8Str2, PadLeft(UTF8Str1, 3));
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

procedure padRightTest.CompletarStringComAcentos;
var
  StrAcentos: String;
begin
  StrAcentos := ACBrStr('ACBr¡…Õ”⁄«');

  CheckEquals(StrAcentos+'ZZZZZ', PadRight(StrAcentos, 15, 'Z'));
  CheckEquals(StrAcentos+'     ', PadRight(StrAcentos, 15));
end;

procedure padRightTest.CortarStringComAcentos;
var
  Str1, Str2: String;
begin
  Str1 := ACBrStr('¡…Õ”⁄«');
  Str2 := ACBrStr('¡…Õ');

  CheckEquals(Str2, PadRight(Str1, 3));
end;

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

{ RemoverQuebraLinhaFinalTest }

procedure RemoverQuebraLinhaFinalTest.DuasLinhasComQuebraDiferenciada;
const
  S = 'Projeto ACBr;www.projetoacbr.com.br;';
begin
  CheckEquals('Projeto ACBr;www.projetoacbr.com.br', RemoverQuebraLinhaFinal(S, ';'));
end;

procedure RemoverQuebraLinhaFinalTest.DuasLinhasComQuebraPadrao;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('Projeto ACBr');
    SL.Add('www.projetoacbr.com.br');
    CheckEquals('Projeto ACBr' + sLineBreak + 'www.projetoacbr.com.br', RemoverQuebraLinhaFinal(SL.Text));
  finally
    SL.Free;
  end;
end;

procedure RemoverQuebraLinhaFinalTest.DuasLinhasSemQuebraDiferenciada;
const
  S = 'Projeto ACBr;www.projetoacbr.com.br';
begin
  CheckEquals('Projeto ACBr;www.projetoacbr.com.br', RemoverQuebraLinhaFinal(S, ';'));
end;

procedure RemoverQuebraLinhaFinalTest.DuasLinhasSemQuebraPadrao;
const
  S = 'Projeto ACBr' + sLineBreak + 'www.projetoacbr.com.br';
begin
  CheckEquals('Projeto ACBr' + sLineBreak + 'www.projetoacbr.com.br', RemoverQuebraLinhaFinal(S));
end;

procedure RemoverQuebraLinhaFinalTest.UmaLinhaComQuebraDiferenciada;
const
  S = 'Projeto ACBr;';
begin
  CheckEquals('Projeto ACBr', RemoverQuebraLinhaFinal(S, ';'));
end;

procedure RemoverQuebraLinhaFinalTest.UmaLinhaComQuebraPadrao;
const
  S = 'Projeto ACBr' + sLineBreak;
begin
  CheckEquals('Projeto ACBr', RemoverQuebraLinhaFinal(S));
end;

procedure RemoverQuebraLinhaFinalTest.UmaLinhaSemQuebraDiferenciada;
const
  S = 'Projeto ACBr';
begin
  CheckEquals('Projeto ACBr', RemoverQuebraLinhaFinal(S, ';'));
end;

procedure RemoverQuebraLinhaFinalTest.UmaLinhaSemQuebraPadrao;
const
  S = 'Projeto ACBr';
begin
  CheckEquals('Projeto ACBr', RemoverQuebraLinhaFinal(S));
end;

initialization

  _RegisterTest('ACBrComum.ACBrUtil.Strings', AddDelimitedTextToListTeste);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', SplitTeste);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', FindDelimiterInTextTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', ChangeLineBreakTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', TiraPontosTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', DecodeToStringTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', QuebrarLinhaTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', ACBrStrToAnsiTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', padRightTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', padLeftTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', padCenterTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', padSpaceTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', RemoveStringTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', RemoveStringsTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', RemoverEspacosDuplosTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', RemoveEmptyLinesTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', RandomNameTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', IfEmptyThenTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', PosAtTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', PosLastTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', CountStrTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', Poem_ZerosTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', StrIsAlphaTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', StrIsAlphaNumTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', StrIsNumberTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', StrIsHexaTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', StrIsBinaryTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', StrIsBase64Test);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', CharIsAlphaTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', CharIsAlphaNumTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', CharIsNumTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', OnlyNumberTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', OnlyAlphaTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', OnlyAlphaNumTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', TiraAcentosTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', TiraAcentoTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', AjustaLinhasTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', QuebraLinhasTest);
  _RegisterTest('ACBrComum.ACBrUtil.Strings', RemoverQuebraLinhaFinalTest);


end.
