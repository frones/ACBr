{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}
{                                                                              }
{  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
{ cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

{$IFDEF FPC}
 {$IFNDEF NOGUI}
  {$DEFINE USE_LCLIntf}
 {$ENDIF}
{$ENDIF}

unit ACBrUtil.Strings;

interface

Uses
  SysUtils, Math, Classes,
  ACBrBase, ACBrConsts, IniFiles
//  {$IfDef HAS_UNIT_ANSISTRINGS} , AnsiStrings {$EndIf}
  {$IfDef COMPILER6_UP}, StrUtils, DateUtils {$Else}, ACBrD5, FileCtrl {$EndIf}
  {$IfDef FPC}
    ,dynlibs, LazUTF8, LConvEncoding, LCLType
    {$IfDef USE_LCLIntf} ,LCLIntf {$EndIf}
  {$EndIf}
  {$IfDef MSWINDOWS}
    ,Windows, ShellAPI
  {$Else}
    {$IfNDef FPC}
      {$IfDef ANDROID}
      ,System.IOUtils
      {$EndIf}
      {$IfDef  POSIX}
      ,Posix.Stdlib
      ,Posix.Unistd
      ,Posix.Fcntl
      {$Else}
      ,Libc
      {$EndIf}
    {$Else}
      ,unix, BaseUnix
    {$EndIf}
    {$IfNDef NOGUI}
      {$IfDef FMX}
        ,FMX.Forms
      {$Else}
        ,Forms
      {$EndIf}
    {$EndIf}
  {$EndIf} ;

type
  TSetOfChars = set of AnsiChar;
  TSplitResult = array of string;

function Split(const ADelimiter: Char; const AString: string): TSplitResult;
function DecodeToString( const ABinaryString : AnsiString; const StrIsUTF8: Boolean ) : String ;
function RetornarConteudoEntre(const Frase, Inicio, Fim: String; IncluiInicioFim: Boolean = False): string;

procedure QuebrarLinha(const Alinha: string; const ALista: TStringList;
  const QuoteChar: char = '"'; Delimiter: char = ';');

function ACBrStr( const AString : String ) : String ;
function ACBrStrToAnsi( const AString : String ) : String ;

function NativeStringToUTF8(const AString : String ) : AnsiString;
function UTF8ToNativeString(const AUTF8String : AnsiString ) : String;

function NativeStringToAnsi(const AString : String ) : AnsiString;
function AnsiToNativeString(const AAnsiString : AnsiString ) : String;

{$IfDef FPC}
var ACBrANSIEncoding: String;
function GetSysANSIencoding: String;
{$EndIf}

function ACBrUTF8ToAnsi( const AUTF8String : AnsiString ) : AnsiString;
function ACBrAnsiToUTF8( const AAnsiString : AnsiString ) : AnsiString;

function TranslateString(const S: AnsiString; CP_Destino: Word; CP_Atual: Word = 0): AnsiString;

function AnsiChr( b: Byte) : AnsiChar;

function LengthNativeString(const AString: String): Integer;
function LeftStrNativeString(const AString: String; const ALen: Integer): String;
function RightStrNativeString(const AString: String; const ALen: Integer): String;
function PadRight(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String;
function PadRightA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString;
function PadLeft(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String;
function PadLeftA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString;
function PadCenter(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String;
function PadCenterA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString;
function PadSpace(const AString : String; const nLen : Integer; Separador : String;
   const Caracter : Char = ' '; const RemoverEspacos: Boolean = True) : String;

function RemoveString(const sSubStr, sString: String): String;
function RemoveStrings(const AText: AnsiString; StringsToRemove: array of AnsiString): AnsiString;
function RemoverEspacosDuplos(const AString: String): String;
procedure RemoveEmptyLines( AStringList: TStringList);

function RandomName(const LenName : Integer = 8) : String;

function IfEmptyThen( const AValue, DefaultValue: String; DoTrim: Boolean = True) : String;
function PosAt(const SubStr, S: AnsiString; Ocorrencia : Cardinal = 1): Integer;
function RPos(const aSubStr, aString : AnsiString; const aStartPos: Integer): Integer; overload;
function RPos(const aSubStr, aString : AnsiString): Integer; overload;
function PosLast(const SubStr, S: String): Integer;
function CountStr(const AString, SubStr : String ) : Integer ;
Function Poem_Zeros(const Texto : String; const Tamanho : Integer) : String; overload;
function Poem_Zeros(const NumInteiro : Int64 ; Tamanho : Integer) : String ; overload;
function RemoveZerosEsquerda(const ANumStr: String): String;

function StrIsAlpha(const S: String): Boolean;
function StrIsAlphaNum(const S: String): Boolean;
function StrIsNumber(const S: String): Boolean;
function StrIsHexa(const S: String): Boolean;
function StrIsBinary(const S: String): Boolean;
function StrIsBase64(const S: String): Boolean;
function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;
function CharIsNum(const C: Char): Boolean;
function CharIsHexa(const C: Char): Boolean;
function CharIsBinary(const C: Char): Boolean;
function CharIsBase64(const C: Char): Boolean;
function OnlyNumber(const AValue: String): String;
function OnlyAlpha(const AValue: String): String;
function OnlyAlphaNum(const AValue: String): String;
function OnlyCharsInSet(const AValue: String; SetOfChars: TSetOfChars): String;

function TiraAcentos( const AString : String ) : String ;
function TiraAcento( const AChar : AnsiChar ) : AnsiChar ;

function AjustaLinhas(const Texto: AnsiString; Colunas: Integer ;
   NumMaxLinhas: Integer = 0; PadLinhas: Boolean = False): AnsiString;
function QuebraLinhas(const Texto: String; const Colunas: Integer;
   const CaracterQuebrar : AnsiChar = ' '): String;
function RemoverQuebraLinhaFinal(const ATexto: String; const AQuebraLinha: String = ''): String;

function TiraPontos(const Str: string): string;
function TBStrZero(const Texto: string; const Casas: byte): string;
function Space(Tamanho: Integer): string;
function LinhaSimples(Tamanho: Integer): string;
function LinhaDupla(Tamanho: Integer): string;

function MatchText(const AText: String; const AValues: array of String): Boolean;

function FindDelimiterInText( const AText: String; ADelimiters: String = ''): Char;
function AddDelimitedTextToList( const AText: String; const ADelimiter: Char;
   AStringList: TStrings; const AQuoteChar: Char = '"'): Integer;

function ChangeLineBreak(const AText: String; const NewLineBreak: String = ';'): String;

//function UTF8Decode(const S: String): String;

implementation

uses
  ACBrUtil.Compatibilidade;

var
  Randomized : Boolean ;

function Split(const ADelimiter: Char; const AString: string): TSplitResult;
var
  i, ACount: Integer;
  vRows: TStrings;
begin
  vRows := TStringList.Create;
  try
    ACount := AddDelimitedTextToList(AString, ADelimiter, vRows);
    SetLength(Result, ACount);
    for i := 0 to ACount - 1 do
      Result[i] := vRows.Strings[i];
  finally
    FreeAndNil(vRows);
  end;
end;

{------------------------------------------------------------------------------
   Realiza o tratamento de uma String recebida de um Serviço Web

   - Se a String recebida for UTF8:
     - Delphi 7, converte a String para Ansi
     - Delphi XE, converte para UnicodeString (UTF16)
     - Lazarus converte de AnsiString para String
   - Se a String recebida for Ansi
     - No Delphi 7, converte de AnsiString para String
     - No Delphi XE, converte de Ansi para UnicodeString
     - No Lazarus, converte de Ansi para UTF8
 ------------------------------------------------------------------------------}
function DecodeToString(const ABinaryString: AnsiString; const StrIsUTF8: Boolean
  ): String;
begin
  if StrIsUTF8 then
    Result := UTF8ToNativeString(ABinaryString)
  else
    Result := AnsiToNativeString(ABinaryString);
end;

function RetornarConteudoEntre(const Frase, Inicio, Fim: String;
  IncluiInicioFim: Boolean): string;
var
  i: integer;
  s: string;
begin
  result := '';
  i := pos(Inicio, Frase);
  if i = 0 then
    Exit;

  if IncluiInicioFim then
  begin
    s := Copy(Frase, i, maxInt);
    result := Copy(s, 1, pos(Fim, s) + Length(Fim) - 1);
  end
  else
  begin
    s := Copy(Frase, i + length(Inicio), maxInt);
    result := Copy(s, 1, pos(Fim, s) - 1);
  end;
end;

procedure QuebrarLinha(const Alinha: string; const ALista: TStringList;
  const QuoteChar: char; Delimiter: char);
var
  P, P1: PChar;
  S: string;
begin
  ALista.BeginUpdate;
  try
    ALista.Clear;
    P := PChar(Alinha);

    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while (P^ <> #0) and (P^ <> Delimiter) do
        {$IFDEF MSWINDOWS}
          P := CharNext(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}

        SetString(S, P1, P - P1);
      end;
      ALista.Add(S);

      if P^ = Delimiter then
      begin
        P1 := P;

        {$IFDEF MSWINDOWS}
        if CharNext(P1)^ = #0 then
        {$ELSE}
        Inc(P1);
        if P1^ = #0 then
        {$ENDIF}
          ALista.Add('');

        repeat
          {$IFDEF MSWINDOWS}
          P := CharNext(P);
          {$ELSE}
          Inc(P);
          {$ENDIF}
        until not (CharInSet(P^, [#1..' ']));
      end;
    end;
  finally
    ALista.EndUpdate;
  end;
end;

{-----------------------------------------------------------------------------
  Todos os Fontes do ACBr usam Encoding CP1252, para manter compatibilidade com
  D5 a D2007, Porém D2009 e superiores usam Unicode, e Lazarus 0.9.27 ou superior,
  usam UTF-8. A função abaixo converte a "AString" de ANSI CP1252, para UNICODE
  ou UTF8, de acordo com as diretivas do Compilador
 -----------------------------------------------------------------------------}
function ACBrStr( const AString : String ) : String ;
begin
{$IFDEF UNICODE}
  {$IFDEF FPC}
    Result := CP1252ToUTF8( AString ) ;
  {$ELSE}
    Result := String(AString) ;
  {$ENDIF}
{$ELSE}
  Result := AString
{$ENDIF}
end ;

{-----------------------------------------------------------------------------
   Todos os Fontes do ACBr usam Encoding CP1252, para manter compatibilidade com
  D5 a D2007, Porém D2009 e superiores usam Unicode, e Lazarus 0.9.27 ou superior,
  usam UTF-8. A função abaixo, Converte a AString de UTF8 ou Unicode para a página
  de código nativa do Sistema Operacional, (apenas se o Compilador usar UNICODE)
 -----------------------------------------------------------------------------}
function ACBrStrToAnsi( const AString: String): String;
begin
{$IFDEF UNICODE}
  {$IFDEF FPC}
    Result := UTF8ToCP1252( AString ) ;
  {$ELSE}
    Result := string(AnsiString( AString )) ;
  {$ENDIF}
{$ELSE}
  Result := AString
{$ENDIF}
end;

{-----------------------------------------------------------------------------
  Converte a AString nativa do Compilador, para UTF8, de acordo o suporte a
  UNICODE/UTF8 do Compilador
 -----------------------------------------------------------------------------}
function NativeStringToUTF8(const AString : String ) : AnsiString;
{$IFNDEF FPC}
 {$IFDEF UNICODE}
  var
    RBS: RawByteString;
 {$ENDIF}
{$ENDIF}
begin
  {$IFDEF USE_UTF8}
    Result := AString;  // FPC, DELPHI LINUX e NEXTGEN usam UTF8 de forma nativa
  {$ELSE}
    {$IFDEF UNICODE}
      RBS := UTF8Encode(AString);
      SetCodePage(RBS, 0, False);
      Result := AnsiString(RBS);
    {$ELSE}
      Result := UTF8Encode(AString);
    {$ENDIF}
  {$ENDIF}
end;

function UTF8ToNativeString(const AUTF8String: AnsiString): String;
begin
  {$IfDef USE_UTF8}
   Result := AUTF8String;  // FPC, DELPHI LINUX e NEXTGEN usam UTF8 de forma nativa
  {$Else}
   {$IfDef UNICODE}
    {$IfDef DELPHI12_UP}  // delphi 2009 em diante
     Result := UTF8ToString(AUTF8String);
    {$Else}
     Result := UTF8Decode(AUTF8String);
    {$EndIf}
   {$Else}
    Result := Utf8ToAnsi(AUTF8String) ;
   {$EndIf}

   if Result = '' then
     Result := String(AUTF8String);
  {$EndIf}
end;

function NativeStringToAnsi(const AString: String): AnsiString;
begin
  {$IfDef USE_UTF8}
   {$IfDef ANDROID}
    Result := TranslateString(AString, 1252)
   {$Else}
    Result := ACBrUTF8ToAnsi(AString);
   {$EndIf}
  {$Else}
    Result := AnsiString(AString);
  {$EndIf}
end;

function AnsiToNativeString(const AAnsiString: AnsiString): String;
begin
  {$IfDef USE_UTF8}
    Result := ACBrAnsiToUTF8(AAnsiString);
  {$Else}
    Result := String(AAnsiString);
  {$EndIf}
end;

{-----------------------------------------------------------------------------
  Converte uma String que está em UTF8 para ANSI, considerando as diferetes IDEs
  suportadas pelo ACBr
 -----------------------------------------------------------------------------}
function ACBrUTF8ToAnsi(const AUTF8String : AnsiString ) : AnsiString;
begin
  {$IfNDef FPC}
    Result := AnsiString( UTF8ToNativeString(AUTF8String));
  {$Else}
    Result := ConvertEncoding( AUTF8String, EncodingUTF8, ACBrANSIEncoding);
  {$EndIf}
end;

{-----------------------------------------------------------------------------
  Converte uma String que está em ANSI para UTF8, considerando as diferetes IDEs
  suportadas pelo ACBr
 -----------------------------------------------------------------------------}
function ACBrAnsiToUTF8(const AAnsiString: AnsiString): AnsiString;
begin
  {$IfNDef FPC}
    Result := NativeStringToUTF8(String(AAnsiString));
  {$Else}
    Result := ConvertEncoding( AAnsiString, ACBrANSIEncoding, EncodingUTF8 );
  {$EndIf}
end;

{$IfDef FPC}
function GetSysANSIencoding: String;
begin
  Result := {$IfDef NOGUI}GetConsoleTextEncoding{$Else}GetDefaultTextEncoding{$EndIf};
  if (Result = EncodingUTF8) or (Result = EncodingAnsi) then
    Result := 'cp1252';  // Usando página de código ANSI padrão para o Brasil
end;
{$EndIf}

{------------------------------------------------------------------------------
  Traduz uma String de uma página de código para outra
http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_10147769.html
 ------------------------------------------------------------------------------}
function TranslateString(const S: AnsiString; CP_Destino: Word; CP_Atual: Word = 0): AnsiString;
{$IFDEF POSIX}
var
  R: RawByteString;
begin
  R := String(S);
  if CP_Atual = 0 then
    SetCodePage(R, CP_Destino, True)
  else
    SetCodePage(R, CP_ACP, True);

  Result := AnsiString(R);
end;
{$ELSE}
{$IfNDef MSWINDOWS}
 Var
   AnsiStr : AnsiString ;
   UTF8Str : String ;
 begin
   if CP_Atual = 0 then
   begin
     UTF8Str := AnsiToUtf8( S );

     case CP_Destino of
       437   : Result := UTF8ToCP437( UTF8Str ) ;
       850   : Result := UTF8ToCP850( UTF8Str ) ;
       852   : Result := UTF8ToCP852( UTF8Str ) ;
       866   : Result := UTF8ToCP866( UTF8Str ) ;
       874   : Result := UTF8ToCP874( UTF8Str ) ;
       1250  : Result := UTF8ToCP1250( UTF8Str ) ;
       1251  : Result := UTF8ToCP1251( UTF8Str ) ;
       1252  : Result := UTF8ToCP1252( UTF8Str ) ;
       1253  : Result := UTF8ToCP1253( UTF8Str ) ;
       1254  : Result := UTF8ToCP1254( UTF8Str ) ;
       1255  : Result := UTF8ToCP1255( UTF8Str ) ;
       1256  : Result := UTF8ToCP1256( UTF8Str ) ;
       1257  : Result := UTF8ToCP1257( UTF8Str ) ;
       1258  : Result := UTF8ToCP1258( UTF8Str ) ;
       28591 : Result := UTF8ToISO_8859_1( UTF8Str ) ;
       28592 : Result := UTF8ToISO_8859_2( UTF8Str ) ;
     else
       Result := S;
     end ;
   end
   else
   begin
     case CP_Atual of
       437   : UTF8Str := CP437ToUTF8( S ) ;
       850   : UTF8Str := CP850ToUTF8( S ) ;
       852   : UTF8Str := CP852ToUTF8( S ) ;
       866   : UTF8Str := CP866ToUTF8( S ) ;
       874   : UTF8Str := CP874ToUTF8( S ) ;
       1250  : UTF8Str := CP1250ToUTF8( S ) ;
       1251  : UTF8Str := CP1251ToUTF8( S ) ;
       1252  : UTF8Str := CP1252ToUTF8( S ) ;
       1253  : UTF8Str := CP1253ToUTF8( S ) ;
       1254  : UTF8Str := CP1254ToUTF8( S ) ;
       1255  : UTF8Str := CP1255ToUTF8( S ) ;
       1256  : UTF8Str := CP1256ToUTF8( S ) ;
       1257  : UTF8Str := CP1257ToUTF8( S ) ;
       1258  : UTF8Str := CP1258ToUTF8( S ) ;
       28591 : UTF8Str := ISO_8859_1ToUTF8( S ) ;
       28592 : UTF8Str := ISO_8859_2ToUTF8( S ) ;
     else
        UTF8Str := AnsiToUtf8( S );
     end ;

     Result := ACBrStrToAnsi( UTF8Str ) ;
   end ;

 end ;
{$ELSE}
   function WideStringToStringEx(const WS: WideString; CodePage: Word): AnsiString;
   var
     L: Integer;
   begin
     L := WideCharToMultiByte(CodePage, 0, PWideChar(WS), -1, nil, 0, nil, nil);
     SetLength(Result, L - 1);
     WideCharToMultiByte(CodePage, 0, PWideChar(WS), -1, PAnsiChar(Result), L - 1, nil, nil);
   end;

   function StringToWideStringEx(const S: AnsiString; CodePage: Word): WideString;
   var
     L: Integer;
   begin
     L:= MultiByteToWideChar(CodePage, 0, PAnsiChar(S), -1, nil, 0);
     SetLength(Result, L - 1);
     MultiByteToWideChar(CodePage, 0, PAnsiChar(S), -1, PWideChar(Result), L - 1);
   end;
 begin
   Result := WideStringToStringEx( StringToWideStringEx(S, CP_Atual), CP_Destino);
 end;
{$ENDIF}
{$ENDIF}

{-----------------------------------------------------------------------------
 Faz o mesmo que o comando chr(), porém retorna um AnsiChar ao invés de Char
 Util quando for usada para compor valores em AnsiString,
 veja exemplos nesse mesmo fonte...
 -----------------------------------------------------------------------------}
function AnsiChr(b: Byte): AnsiChar;
begin
  Result := AnsiChar(chr(b));
end;


{-----------------------------------------------------------------------------
  Retorna o numero de caracteres dentro de uma String, semelhante a Length()
  Porém Lenght() não funciona corretamente em FPC com UTF8 e acentos
 ---------------------------------------------------------------------------- }
function LengthNativeString(const AString: String): Integer;
begin
  {$IfDef FPC}
   Result := UTF8Length(AString);
  {$Else}
   Result := Length(AString);
  {$EndIf}
end;

{-----------------------------------------------------------------------------
  Semelhante a LeftStr(), mas trata corretanmente Strings em UTF8 no FPC
 ---------------------------------------------------------------------------- }
function LeftStrNativeString(const AString: String; const ALen: Integer): String;
begin
  {$IfDef FPC}
   Result := UTF8LeftStr(AString, ALen);
  {$Else}
//    {$IfDef HAS_UNIT_ANSISTRINGS}
//     Result := Ansistrings.LeftStr(AString, ALen);
//    {$Else}
     Result := LeftStr(AString, ALen);
//    {$EndIf}
  {$EndIf}
end;

{-----------------------------------------------------------------------------
  Semelhante a RightStr(), mas trata corretanmente Strings em UTF8 no FPC
 ---------------------------------------------------------------------------- }
function RightStrNativeString(const AString: String; const ALen: Integer): String;
begin
  {$IfDef FPC}
   Result := UTF8RightStr(AString, ALen);
  {$Else}
//    {$IfDef HAS_UNIT_ANSISTRINGS}
//     Result := Ansistrings.RightStr(AString, ALen);
//    {$Else}
     Result := RightStr(AString, ALen);
//    {$EndIf}
  {$EndIf}
end;

{-----------------------------------------------------------------------------
  Completa <AString> com <Caracter> a direita, até o tamanho <nLen>, Alinhando
  a <AString> a Esquerda. Se <AString> for maior que <nLen>, ela será truncada
 ---------------------------------------------------------------------------- }
function PadRight(const AString : String; const nLen : Integer;
   const Caracter : Char) : String ;
var
  Tam: Integer;
begin
  Tam := LengthNativeString( AString );
  if Tam < nLen then
    Result := AString + StringOfChar(Caracter, (nLen - Tam))
  else
    Result := LeftStrNativeString(AString, nLen);
end;

function PadRightA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString;
var
  Tam: Integer;
begin
  Tam := Length( AAnsiString );
  if Tam < nLen then
    Result := AAnsiString + StringOfChar(Caracter, (nLen - Tam))
  else
    Result := LeftStr(AAnsiString, nLen);
end;

{-----------------------------------------------------------------------------
  Completa <AString> com <Caracter> a esquerda, até o tamanho <nLen>, Alinhando
  a <AString> a Direita. Se <AString> for maior que <nLen>, ela será truncada
 ---------------------------------------------------------------------------- }
function PadLeft(const AString : String; const nLen : Integer;
   const Caracter : Char) : String ;
var
  Tam: Integer;
begin
  Tam := LengthNativeString( AString );
  if Tam < nLen then
    Result := StringOfChar(Caracter, (nLen - Tam)) + AString
  else
    Result := LeftStrNativeString(AString, nLen);  //RightStr(AString,nLen) ;
end;

function PadLeftA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString;
var
  Tam: Integer;
begin
  Tam := Length( AAnsiString );
  if Tam < nLen then
    Result := StringOfChar(Caracter, (nLen - Tam)) + AAnsiString
  else
    Result := LeftStr(AAnsiString, nLen);  //RightStr(AString,nLen) ;
end;

{-----------------------------------------------------------------------------
 Completa <AString> Centralizando, preenchendo com <Caracter> a esquerda e direita
 ---------------------------------------------------------------------------- }
function PadCenter(const AString : String; const nLen : Integer;
   const Caracter : Char) : String ;
var
  nCharLeft: Integer;
  Tam: integer;
begin
  Tam := LengthNativeString( AString );
  if Tam < nLen then
  begin
    nCharLeft := Trunc( (nLen - Tam) / 2 ) ;
    Result    := PadRight( StringOfChar(Caracter, nCharLeft) + AString, nLen, Caracter) ;
  end
  else
    Result := LeftStrNativeString(AString, nLen);
end;

function PadCenterA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString;
var
  nCharLeft: Integer;
  Tam: integer;
begin
  Tam := Length( AAnsiString );
  if (Tam < nLen) then
  begin
    nCharLeft := Trunc( (nLen - Tam) / 2 ) ;
    Result    := PadRightA( StringOfChar(Caracter, nCharLeft) + AAnsiString, nLen, Caracter) ;
  end
  else
    Result := LeftStr(AAnsiString, nLen);
end;

{-----------------------------------------------------------------------------
  Ajusta a <AString> com o tamanho de <nLen> inserindo espaços no meio,
  substituindo <Separador> por n X <Caracter>  (Justificado)
 ---------------------------------------------------------------------------- }
function PadSpace(const AString : String; const nLen : Integer;
   Separador : String; const Caracter : Char = ' '; const RemoverEspacos : Boolean = True) : String ;
var StuffStr : String ;
    nSep, nCharSep, nResto, nFeito, Ini : Integer ;
    D : Double ;
begin
  Result := copy(AString,1,nLen) ;
  if Separador = String(Caracter) then  { Troca Separador, senao fica em loop infinito }
  begin
     Result    := StringReplace( Result, Separador, #255,[rfReplaceAll]);
     Separador := #255 ;
  end ;

  nSep := CountStr( Result, Separador ) ;

  if nSep < 1 then
  begin
     Result := PadRight( Result, nLen, Caracter ) ;
     exit ;
  end ;

  if RemoverEspacos then
    Result := Trim( Result ) ;

  D        := (nLen - (LengthNativeString(Result)-nSep)) / nSep ;
  nCharSep := Trunc( D ) ;
  nResto   := nLen - ( (LengthNativeString(Result)-nSep) + (nCharSep*nSep) ) ;
  nFeito   := nSep ;
  StuffStr := String( StringOfChar( Caracter, nCharSep ) ) ;

  Ini := Pos( Separador, Result ) ;
  while Ini > 0 do
  begin
    Result := StuffString( Result,
        Ini,
        length(Separador),
        StuffStr + ifthen(nFeito <= nResto, String(Caracter), '' )
      );

    nFeito := nFeito - 1 ;
    Ini    := Pos( Separador, Result ) ;
  end ;
end ;

{-----------------------------------------------------------------------------
   Remove todas ocorrencias <sSubStr> de <sString>, retornando a String alterada
 ---------------------------------------------------------------------------- }
function RemoveString(const sSubStr, sString : String) : String ;
begin
   Result := StringReplace( sString, sSubStr, '', [rfReplaceAll]);
end;

{-----------------------------------------------------------------------------
   Remove todas ocorrencias do array <StringsToRemove> da String <AText>
   retornando a String alterada
 ---------------------------------------------------------------------------- }
function RemoveStrings(const AText: AnsiString;
  StringsToRemove: array of AnsiString): AnsiString;
Var
  I, J : Integer ;
  StrToFind : AnsiString ;
begin
  Result := AText ;
  { Verificando parâmetros de Entrada }
  if (AText = '') or (Length(StringsToRemove) = 0) then
     exit ;

  { Efetua um Sort no Array de acordo com o Tamanho das Substr a remover,
    para Pesquisar da Mais Larga a Mais Curta (Pois as Substr Mais Curtas podem
    estar contidas nas mais Largas) }
  For I := High( StringsToRemove ) downto Low( StringsToRemove )+1 do
     for j := Low( StringsToRemove ) to I-1 do
        if Length(StringsToRemove[J]) > Length(StringsToRemove[J+1]) then
        begin
           StrToFind := StringsToRemove[J];
           StringsToRemove[J] := StringsToRemove[J+1];
           StringsToRemove[J+1] := StrToFind;
        end;

  For I := High(StringsToRemove) downto Low(StringsToRemove) do
  begin
     StrToFind := StringsToRemove[I] ;
     J := Pos( StrToFind, Result ) ;
     while J > 0 do
     begin
        Delete( Result, J, Length( StrToFind ) ) ;
        J := PosEx( String(StrToFind), String(Result), J) ;
     end ;
  end ;
end ;

{-----------------------------------------------------------------------------
   Remove todos os espacos duplos do texto
 ---------------------------------------------------------------------------- }
function RemoverEspacosDuplos(const AString: String): String;
begin
  Result := Trim(AString);
  while Pos('  ', Result) > 0 do
    Result := StringReplace( Result, '  ', ' ', [rfReplaceAll]);
end;


{-----------------------------------------------------------------------------
   Remove todas as linhas vazias de um TStringList
 ---------------------------------------------------------------------------- }
procedure RemoveEmptyLines(AStringList : TStringList) ;
var
  I : Integer ;
begin
  I := 0;
  while I < AStringList.Count do
  begin
    if trim(AStringList[I]) = '' then
      AStringList.Delete(I)
    else
      Inc(I);
  end;
end;

{-----------------------------------------------------------------------------
   Cria um Nome Aleatório (usado por exemplo, em arquivos temporários)
 ---------------------------------------------------------------------------- }
function RandomName(const LenName : Integer ) : String ;
 Var I, N : Integer ;
     C : Char ;
begin
   if not Randomized then
   begin
      Randomize ;
      Randomized := True ;
   end ;

   Result := '' ;

   For I := 1 to LenName do
   begin
      N := Random( 25 ) ;
      C := Char( 65 + N ) ;

      Result := Result + C ;
   end ;
end ;

{-----------------------------------------------------------------------------
  Verifica se "AValue" é vazio, se for retorna "DefaultValue". "DoTrim", se
  verdadeiro (default) faz Trim em "AValue" antes da comparação
 ---------------------------------------------------------------------------- }
function IfEmptyThen(const AValue, DefaultValue: String; DoTrim: Boolean
  ): String;
Var
  AStr : String;
begin
  if DoTrim then
     AStr := Trim(AValue)
  else
     AStr := AValue;

  if AStr = EmptyStr then
     Result := DefaultValue
  else
     Result := AValue;
end;

{-----------------------------------------------------------------------------
  Acha a e-nesima "Ocorrencia" de "SubStr" em "S"
 ---------------------------------------------------------------------------- }
function PosAt(const SubStr, S: AnsiString; Ocorrencia : Cardinal = 1): Integer;
Var Count : Cardinal ;
begin
  Result := Pos( SubStr, S) ;
  Count  := 1 ;
  while (Count < Ocorrencia) and (Result > 0) do
  begin
     Result := PosEx( String(SubStr), String(S), Result+1) ;
     Count  := Count + 1 ;
  end ;
end ;

function RPos(const aSubStr, aString : AnsiString; const aStartPos: Integer): Integer; overload;
var
  i: Integer;
  pStr: PChar;
  pSub: PChar;
begin
  pSub := Pointer(aSubStr);

  for i := aStartPos downto 1 do
  begin
    pStr := @(aString[i]);
    if (pStr^ = pSub^) then
    begin
      if CompareMem(pSub, pStr, Length(aSubStr)) then
      begin
        result := i;
        EXIT;
      end;
    end;
  end;

  result := 0;
end;

function RPos(const aSubStr, aString : AnsiString): Integer; overload;
begin
  result := RPos(aSubStr, aString, Length(aString) - Length(aSubStr) + 1);
end;

{-----------------------------------------------------------------------------
  Acha a Ultima "Ocorrencia" de "SubStr" em "S"
 ---------------------------------------------------------------------------- }
function PosLast(const SubStr, S: String ): Integer;
Var P : Integer ;
begin
  Result := 0 ;
  P := Pos( SubStr, S) ;
  while P <> 0 do
  begin
     Result := P ;
     P := PosEx( SubStr, S, P+1) ;
  end ;
end ;


{-----------------------------------------------------------------------------
  Retorna quantas ocorrencias de <SubStr> existem em <AString>
 ---------------------------------------------------------------------------- }
function CountStr(const AString, SubStr : String ) : Integer ;
Var ini : Integer ;
begin
  result := 0 ;
  if SubStr = '' then exit ;

  ini := Pos( SubStr, AString ) ;
  while ini > 0 do
  begin
     Result := Result + 1 ;
     ini    := PosEx( SubStr, AString, ini + 1 ) ;
  end ;
end ;

{-----------------------------------------------------------------------------
  Insere ZEROS (0) a esquerda de <Texto> até completar <Tamanho>
  Se Length(<Texto>) for maior que <Tamanho>, a string <Texto> será truncada
  Se você precisa que o texto não seja truncado use a função TBStrZero.
 ---------------------------------------------------------------------------- }
function Poem_Zeros(const Texto : String ; const Tamanho : Integer) : String ;
begin
  Result := PadLeft(Trim(Texto),Tamanho,'0') ;
end ;

function Poem_Zeros(const NumInteiro : Int64 ; Tamanho : Integer) : String ;
begin
  Result := Poem_Zeros( IntToStr( NumInteiro ), Tamanho);
end ;

function RemoveZerosEsquerda(const ANumStr: String): String;
var
  I, L: Integer;
begin
  L := Length(ANumStr);
  I := 1;
  while (I < L) and (ANumStr[I] = '0') do
    Inc(I);

  Result := Copy(ANumStr, I, L);
end;

{-----------------------------------------------------------------------------
 *** Extraido de JclStrings.pas  - Project JEDI Code Library (JCL) ***
  Retorna <True> se <S> contem apenas caracteres Alpha maiusculo/minuscula
 ---------------------------------------------------------------------------- }
function StrIsAlpha(const S: String): Boolean;
Var A : Integer ;
begin
  Result := true ;
  A      := 1 ;
  while Result and ( A <= Length( S ) )  do
  begin
     Result := CharIsAlpha( S[A] ) ;
     Inc(A) ;
  end;
end ;

{-----------------------------------------------------------------------------
 *** Extraido de JclStrings.pas  - Project JEDI Code Library (JCL) ***
  Retorna <True> se <S> contem apenas caracteres Alpha maiusculo/minuscula
  ou Numericos
 ---------------------------------------------------------------------------- }
function StrIsAlphaNum(const S: String): Boolean;
Var
  A : Integer ;
begin
  Result := true ;
  A      := 1 ;
  while Result and ( A <= Length( S ) )  do
  begin
     Result := CharIsAlphaNum( S[A] ) ;
     Inc(A) ;
  end;
end ;

{-----------------------------------------------------------------------------
 *** Extraido de JclStrings.pas  - Project JEDI Code Library (JCL) ***
  Retorna <True> se <S> contem apenas caracteres Numericos.
  Retorna <False> se <S> for vazio
 ---------------------------------------------------------------------------- }
function StrIsNumber(const S: String): Boolean;
Var
  A, LenStr : Integer ;
begin
  LenStr := Length( S ) ;
  Result := (LenStr > 0) ;
  A      := 1 ;
  while Result and ( A <= LenStr )  do
  begin
     Result := CharIsNum( S[A] ) ;
     Inc(A) ;
  end;
end ;

{-----------------------------------------------------------------------------
  Retorna <True> se <S> contem apenas caracteres em Hexa decimal
 ---------------------------------------------------------------------------- }
function StrIsHexa(const S: String): Boolean;
Var
  A : Integer ;
begin
  Result := True ;
  A      := 1 ;
  while Result and ( A <= Length( S ) )  do
  begin
     Result := CharIsHexa( S[A] ) ;
     Inc(A) ;
  end;
end;

{-----------------------------------------------------------------------------
  Retorna <True> se <S> contem apenas caracteres em Binário (0 e 1)
 ---------------------------------------------------------------------------- }
function StrIsBinary(const S: String): Boolean;
Var
  A : Integer ;
begin
  Result := True ;
  A      := 1 ;
  while Result and ( A <= Length( S ) )  do
  begin
     Result := CharIsBinary( S[A] ) ;
     Inc(A) ;
  end;
end;

{-----------------------------------------------------------------------------
  Retorna <True> se <S> é uma String válidos para decodificação em Base64
  https://stackoverflow.com/questions/12943971/validating-base64-input-with-free-pascal-and-decodestringbase64
 ---------------------------------------------------------------------------- }
function StrIsBase64(const S: String): Boolean;
var
  ValLen, A: Integer;
begin
  ValLen := Length(S);
  // Tamanho de Strings em Base64 devem ser multiplos de 4
  Result := (ValLen > 0) and (ValLen mod 4 = 0);

  // Deve ter no máximo 2 sinais de '=' no final (padding)
  while Result and (S[ValLen] = '=') do
  begin
    Dec(ValLen);
    Result := (ValLen >= (Length(S) - 2)) ;
  end;

  A := 1 ;
  while Result and ( A <= ValLen )  do
  begin
     Result := CharIsBase64( S[A] ) ;
     Inc(A) ;
  end;
end;

{-----------------------------------------------------------------------------
 *** Extraido de JclStrings.pas  - Project JEDI Code Library (JCL) ***
  Retorna <True> se <C> é Alpha maiusculo/minusculo
 ---------------------------------------------------------------------------- }
function CharIsAlpha(const C: Char): Boolean;
begin
  Result := CharInSet( C, ['A'..'Z','a'..'z'] ) ;
end ;

{-----------------------------------------------------------------------------
 *** Extraido de JclStrings.pas  - Project JEDI Code Library (JCL) ***
  Retorna <True> se <C> é Alpha maiusculo/minusculo ou Numerico
 ---------------------------------------------------------------------------- }
function CharIsAlphaNum(const C: Char): Boolean;
begin
  Result := ( CharIsAlpha( C ) or CharIsNum( C ) );
end ;

{-----------------------------------------------------------------------------
 *** Extraido de JclStrings.pas  - Project JEDI Code Library (JCL) ***
  Retorna <True> se <C> é Númerico
 ---------------------------------------------------------------------------- }
function CharIsNum(const C: Char): Boolean;
begin
  Result := CharInSet( C, ['0'..'9'] ) ;
end ;

{-----------------------------------------------------------------------------
  Retorna <True> se <C> é um char hexa válido
 ---------------------------------------------------------------------------- }
function CharIsHexa(const C: Char): Boolean;
begin
  Result := CharInSet( C, ['0'..'9','A'..'F','a'..'f'] ) ;
end;

{-----------------------------------------------------------------------------
  Retorna <True> se <C> é um char Binário válido (0 ou 1)
 ---------------------------------------------------------------------------- }
function CharIsBinary(const C: Char): Boolean;
begin
 Result := CharInSet( C, ['0','1'] ) ;
end;

{-----------------------------------------------------------------------------
  Retorna <True> se <C> é um char válido em Strings codificadas em Base64
 ---------------------------------------------------------------------------- }
function CharIsBase64(const C: Char): Boolean;
begin
 Result := CharInSet( C, ['A'..'Z', 'a'..'z', '0'..'9', '+', '/'] ) ;
end;

{-----------------------------------------------------------------------------
  Retorna uma String apenas com os char Numericos contidos em <Value>
 ---------------------------------------------------------------------------- }
function OnlyNumber(const AValue: String): String;
Var
  I : Integer ;
  LenValue : Integer;
begin
  Result   := '' ;
  LenValue := Length( AValue ) ;
  For I := 1 to LenValue  do
  begin
     if CharIsNum( AValue[I] ) then
        Result := Result + AValue[I];
  end;
end ;

{-----------------------------------------------------------------------------
  Retorna uma String apenas com os char Alpha contidos em <Value>
 ---------------------------------------------------------------------------- }
function OnlyAlpha(const AValue: String): String;
Var
  I : Integer ;
  LenValue : Integer;
begin
  Result := '' ;
  LenValue := Length( AValue ) ;
  For I := 1 to LenValue do
  begin
     if CharIsAlpha( AValue[I] ) then
        Result := Result + AValue[I];
  end;
end ;
{-----------------------------------------------------------------------------
  Retorna uma String apenas com os char Alpha-Numericos contidos em <Value>
 ---------------------------------------------------------------------------- }
function OnlyAlphaNum(const AValue: String): String;
Var
  I : Integer ;
  LenValue : Integer;
begin
  Result := '' ;
  LenValue := Length( AValue ) ;
  For I := 1 to LenValue do
  begin
     if CharIsAlphaNum( AValue[I] ) then
        Result := Result + AValue[I];
  end;
end ;

function OnlyCharsInSet(const AValue: String; SetOfChars: TSetOfChars): String;
Var
  I : Integer ;
  LenValue : Integer;
begin
  Result := '' ;
  LenValue := Length( AValue ) ;
  For I := 1 to LenValue do
  begin
     if CharInSet( AValue[I], SetOfChars) then
        Result := Result + AValue[I];
  end;
end;

{-----------------------------------------------------------------------------
  Substitui caracter acentuado por compativel
 ---------------------------------------------------------------------------- }
function TiraAcento( const AChar : AnsiChar ) : AnsiChar ;
begin
  case Byte(AChar) of
    192..198 : Result := 'A' ;
    199      : Result := 'C' ;
    200..203 : Result := 'E' ;
    204..207 : Result := 'I' ;
    208      : Result := 'D' ;
    209      : Result := 'N' ;
    210..214 : Result := 'O' ;
    215      : Result := 'x' ;
    216,248  : Result := '0' ;
    217..220 : Result := 'U' ;
    221      : Result := 'Y' ;
    222,254  : Result := 'b' ;
    223      : Result := 'B' ;
    224..230 : Result := 'a' ;
    231      : Result := 'c' ;
    232..235 : Result := 'e' ;
    236..239 : Result := 'i' ;
    240,242..246 : Result := 'o' ;
    247      : Result := '/';
    241      : Result := 'n' ;
    249..252 : Result := 'u' ;
    253,255  : Result := 'y' ;
  else
    Result := AChar ;
  end;
end ;

{-----------------------------------------------------------------------------
  Substitui todos os caracteres acentuados por compativeis.
 ---------------------------------------------------------------------------- }
function TiraAcentos( const AString : String ) : String ;
Var A : Integer ;
    Letra : AnsiChar ;
    AnsiStr, Ret : AnsiString ;
begin
  Result  := '' ;
  Ret     := '' ;
  AnsiStr := NativeStringToAnsi( AString );
  For A := 1 to Length( AnsiStr ) do
  begin
     Letra := TiraAcento( AnsiStr[A] ) ;
     if not (Byte(Letra) in [32..126,13,10,8]) then    {Letras / numeros / pontos / sinais}
        Letra := ' ' ;
     Ret := Ret + Letra ;
  end ;

  Result := ACBrStr(string(Ret))
end ;

{-----------------------------------------------------------------------------
  Quebra Linhas grandes no máximo de Colunas especificado, ou caso encontre
  uma quebra de Linha (CR ou CR+LF)
  Retorna uma String usando o #10 como separador de Linha
  Se <NumMaxLinhas> for especificado, para ao chegar no Limite de Linhas
  Se <PadLinhas> for True, Todas as linhas terão o mesmo tamanho de Colunas
    com espaços a esquerda se necessário.
 ---------------------------------------------------------------------------- }
function AjustaLinhas(const Texto: AnsiString; Colunas: Integer;
  NumMaxLinhas: Integer; PadLinhas: Boolean): AnsiString;
Var
  Count,P,I : Integer ;
  Linha, CurrLineBreak, VTexto : String;
begin
  VTexto := String(Texto);
  { Trocando todos os #13+#10 por #10 }
  CurrLineBreak := sLineBreak ;
  if (CurrLineBreak <> CRLF) then
     VTexto := StringReplace(VTexto, CRLF, LF, [rfReplaceAll]) ;

  if (CurrLineBreak <> LF) then
     VTexto := StringReplace(VTexto, CurrLineBreak, LF, [rfReplaceAll]) ;

  { Ajustando a largura das Linhas para o máximo permitido em  "Colunas"
    e limitando em "NumMaxLinhas" o total de Linhas}
  Count  := 0 ;
  Result := '' ;
  while ((Count < NumMaxLinhas) or (NumMaxLinhas = 0)) and
        (Length(VTexto) > 0) do
  begin
     P := pos(#10, VTexto ) ;
     if P > (Colunas + 1) then
        P := Colunas + 1 ;

     if P = 0 then
        P := min( Length( VTexto ), Colunas ) + 1 ;

     // somar 2 quando encontrar uma tag para não quebrar ela
     if (Copy(VTexto, P-1, 1) = '<') or (Copy(VTexto, P-2, 2) = '</') then
        inc(P, 2);

     I := 0 ;
     if copy(VTexto,P,1) = #10 then   // Pula #10 ?
        I := 1 ;

     Linha := copy(VTexto,1,P-1) ;    // Remove #10 (se hover)

     if PadLinhas then
        Result := Result + AnsiString(PadRight( Linha, Colunas)) + #10
     else
        Result := Result + AnsiString(Linha) + #10 ;

     Inc(Count) ;
     VTexto := copy(VTexto, P+I, Length(VTexto) ) ;
  end ;

  { Permitir impressão de uma linha em branco }
  if Result = '' then
  begin
    if PadLinhas then
      Result := AnsiString(Space(Colunas)) + #10
    else
      Result := #10;
  end;
end;

{-----------------------------------------------------------------------------
  Quebra amigável de Linhas de um <Texto>, em um determinado numero de <Colunas>,
  respeitando o espaço existente entre as palavras. Permite especificar um
  separador diferente de espaço em <CaracterQuebrar>
 ---------------------------------------------------------------------------- }
function QuebraLinhas(const Texto: String; const Colunas: Integer;
   const CaracterQuebrar : AnsiChar = ' '): String;
Var
  PosIni, PosFim, PosLF, Tamanho : Integer ;
  AnsiStr, Resp: String;
begin
  Resp := '';
  // Converte para Ansi, para não se perder com caracteres UTF8
  AnsiStr := ACBrStrToAnsi(Texto);
  if sLineBreak <> LF then
    AnsiStr := StringReplace(AnsiStr, sLineBreak, LF, [rfReplaceAll]);

  Tamanho := Length(AnsiStr) ;
  PosIni  := 1 ;
  if Colunas > 0 then
  begin
    repeat
       if (PosIni > 1) and (AnsiStr[PosIni-1] <> LF) then
          Resp := Resp + LF;

       PosFim := PosIni + Colunas - 1 ;

       if Tamanho > PosFim then  // Ainda tem proxima linha ?
       begin
          if CharInSet(AnsiStr[PosFim+1], [CaracterQuebrar, LF]) then  // Proximo já é uma Quebra ?
             Inc(PosFim)
          else
          begin
            while (not CharInSet(AnsiStr[PosFim], [CaracterQuebrar, LF])) and (PosFim > PosIni) do  // Ache uma Quebra
              Dec(PosFim) ;
          end;
       end;

       if PosFim = PosIni then  // Não foi capaz de encontrar uma quebra, divida a palavra em "Coluna"
          PosFim := PosIni + Colunas - 1 ;

       PosLF := PosEx(LF, AnsiStr, PosIni+1);
       if (PosLF > 0) and (PosLF < PosFim) then
         PosFim := PosLF;

       Resp := Resp + Copy( AnsiStr, PosIni, (PosFim-PosIni)+1 );
       PosIni := PosFim + 1 ;

       // Pula CaracterQuebrar no Inicio da String
       if (PosIni <= Tamanho) then
       begin
          while CharInSet(AnsiStr[PosIni], [CaracterQuebrar, LF]) and (PosIni <= Tamanho) do
          begin
             if AnsiStr[PosIni] = LF then
               Resp := Resp + LF;

             Inc(PosIni) ;
          end;
       end;

    until (PosIni > Tamanho);
  end
  else
    Resp := AnsiStr;

  if sLineBreak <> LF then
    Resp := StringReplace(Resp, LF, sLineBreak, [rfReplaceAll]);

  Result := ACBrStr(Resp);
end;

{-----------------------------------------------------------------------------
  Remove a última quebra de linha caso seja a informada no parâmetro AQuebraLinha
  ou a quebra de linha padrão do sistema
 -----------------------------------------------------------------------------}
function RemoverQuebraLinhaFinal(const ATexto: String; const AQuebraLinha: String = ''): String;
var
  StrQ: String;
  LT, LQ: Integer;
begin
  Result := ATexto;
  StrQ := AQuebraLinha;
  if StrQ = '' then
    StrQ := sLineBreak;
  LT := Length(ATexto);
  LQ := Length(StrQ);
  if LT < LQ then
    Exit;
  if  Copy(ATexto, LT - LQ + 1, LQ) = StrQ then
    Result := Copy(ATexto, 1, LT - LQ);
end;

//funcoes para uso com o modulo ACBrSintegra ***********************************************

function TiraPontos(const Str: string): string;
var
  i, Count: Integer;
begin
  SetLength(Result, Length(str));
  Count := 0;
  for i := 1 to Length(str) do
  begin
    if not CharInSet(str[i], [ '/', ',', '-', '.', ')', '(', ' ']) then
    begin
      inc(Count);
      Result[Count] := str[i];
    end;
  end;
  SetLength(Result, Count);
end;

{-----------------------------------------------------------------------------
  Insere ZEROS (0) a esquerda de <Texto> até completar <Tamanho>
  Se Length(<Texto>) for maior que <Tamanho>, a string <Texto> NÃO SERÁ truncada
  Se você precisa que o texto SEJA truncado use a função Poem_Zeros.
 ---------------------------------------------------------------------------- }
function TBStrZero(const Texto: string; const Casas: byte): string;
var
  Ch: Char;
begin
  Result := Texto;

  if length(Texto)>Casas then
    Exit
  else
    Ch := '0';

  while Length(Result) < Casas do
    Result := Ch + Result;
end;

function Space(Tamanho: Integer): string;
begin
  Result := StringOfChar(' ', Tamanho);
end;

function LinhaSimples(Tamanho: Integer): string;
begin
  Result := StringOfChar('-', Tamanho);
end;

function LinhaDupla(Tamanho: Integer): string;
begin
  Result := StringOfChar('=', Tamanho);
end;

//FIM funcoes para uso com o modulo ACBrSintegra ******

function MatchText(const AText: String; const AValues: array of String
  ): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(AValues) to High(AValues) do
    if AText = AValues[I] then
    begin
      Result := True;
      Break;
    end;
end;

{------------------------------------------------------------------------------
  Encontra qual é o primeiro Delimitador usado, em "AText", em uma lista de
  delimitadores, informada em "ADelimiters".
  Se "ADelimiters" for vazio, usa como padrão ";,|"
 ------------------------------------------------------------------------------}
function FindDelimiterInText(const AText: String; ADelimiters: String): Char;
var
  I: Integer;
begin
  if (ADelimiters = '') then
    ADelimiters := ';,|';

  Result := ' ';
  I := 1;
  while (Result = ' ') and (I <= Length(ADelimiters)) do
  begin
    if (pos( ADelimiters[I], AText) > 0) then
      Result := ADelimiters[I];

    Inc(I);
  end;
end;

{------------------------------------------------------------------------------
  Quebra a String "AText", em várias linhas, separando-a de acordo com a ocorrência
  de "ADelimiter", e adiciona os Itens encontrados em "AStringList".
  Retorna o número de Itens Inseridos.
  Informe #0 em "AQuoteChar", para que as Aspas Duplas sejam ignoradas na divisão
  Se AQuoteChar for diferente de #0, ele será considerado, para evitar os delimitadores
  que estão dentro de um contexto do QuoteChar...
  Veja exemplos de uso e retorno em: "ACBrUtilTeste"
 ------------------------------------------------------------------------------}
function AddDelimitedTextToList(const AText: String; const ADelimiter: Char;
  AStringList: TStrings; const AQuoteChar: Char): Integer;
var
  SL: TStringList;
  {$IfNDef HAS_STRICTDELIMITER}
   L, Pi, Pf, Pq: Integer;
  {$EndIf}
begin
  Result := 0;
  if (AText = '') then
    Exit;

  SL := TStringList.Create;
  try
    {$IfDef HAS_STRICTDELIMITER}
     SL.Delimiter := ADelimiter;
     SL.QuoteChar := AQuoteChar;
     SL.StrictDelimiter := True;
     SL.DelimitedText := AText;
    {$Else}
     L  := Length(AText);
     Pi := 1;
     if (ADelimiter = AQuoteChar) then
       Pq := L+1
     else
     begin
       Pq := Pos(AQuoteChar, AText);
       if Pq = 0 then
         Pq := L+1;
     end;

     while Pi <= L do
     begin
       if (Pq = Pi) then
       begin
         Inc(Pi);  // Pula o Quote
         Pf := PosEx(AQuoteChar, AText, Pi);
         Pq := Pf;
       end
       else
         Pf := PosEx(ADelimiter, AText, Pi);

       if Pf = 0 then
         Pf := L+1;

       SL.Add(Copy(AText, Pi, Pf-Pi));

       if (Pq = Pf) then
       begin
         Pq := PosEx(AQuoteChar, AText, Pq+1);
         Inc(Pf);
       end;

       Pi := Pf + 1;
     end;
    {$EndIf}
    Result := SL.Count;

    AStringList.AddStrings(SL);
  finally
    SL.Free;
  end;
end;

{-------------------------------------------------------------------------------
Procedure para trocar a quebra de linha por um caracter separador
-------------------------------------------------------------------------------}
function ChangeLineBreak(const AText: String; const NewLineBreak: String = ';'): String;
begin
  Result := AText;
  if Trim(Result) <> '' then
  begin
    // Troca todos CR+LF para apenas LF
    Result := StringReplace(Result, CRLF, LF, [rfReplaceAll]);

    // Se existe apenas CR, também troca os mesmos para LF
    Result := StringReplace(Result, CR, LF, [rfReplaceAll]);

    { Agora temos todas quebras como LF... Se a Quebra de linha final for
      diferente de LF, aplique a substituição }
    if NewLineBreak <> LF then
      Result := StringReplace(Result, LF, NewLineBreak, [rfReplaceAll]);
  end
end;

//function UTF8Decode(const S: String): String;
//begin
//  {$IfDef COMPILER6_UP}
//    Result := System.UTF8ToString(S);
//  {$Else}
//   Result := System.UTF8Decode(S);
//  {$EndIf}
//end;

initialization
{$IfDef FPC}
  ACBrANSIEncoding := GetSysANSIencoding;
{$EndIf}
  Randomized := False;

end.
