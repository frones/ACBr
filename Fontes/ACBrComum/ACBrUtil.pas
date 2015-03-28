{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
{ cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

{$IFDEF FPC}
 {$IFNDEF NOGUI}
  {$DEFINE USE_LCLIntf}
 {$ENDIF}
 {$IFNDEF MSWINDOWS}
  {$DEFINE USE_LConvEncoding}
 {$ENDIF}
{$ENDIF}

unit ACBrUtil;

interface

Uses SysUtils, Math, Classes, ACBrConsts
    {$IFDEF COMPILER6_UP} ,StrUtils, DateUtils {$ELSE} ,ACBrD5, FileCtrl {$ENDIF}
    {$IFDEF FPC}
      ,dynlibs, LazUTF8, zstream
      {$IFDEF USE_LConvEncoding} ,LConvEncoding {$ENDIF}
      {$IFDEF USE_LCLIntf} ,LCLIntf {$ENDIF}
    {$ELSE}
      ,ACBrZLibExGZ
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      ,Windows, ShellAPI
    {$else}
      {$IFNDEF FPC}
        ,Libc
      {$else}
        ,unix, BaseUnix
      {$endif}
    {$endif} ;

const
{$IFDEF CPU64}
  CINPOUTDLL = 'inpout64.dll';
{$ELSE}
  CINPOUTDLL = 'inpout32.dll';
{$ENDIF}

function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
   const IsUTF8: Boolean = True) : String;
function LerTagXML( const AXML, ATag: String; IgnoreCase: Boolean = True) : String;
function DecodeToString( const ABinaryString : AnsiString; const StrIsUTF8: Boolean ) : String ;
function SeparaDados( const AString : String; const Chave : String; const MantemChave : Boolean = False ) : String;

procedure QuebrarLinha(const Alinha: string; const ALista: TStringList;
  const QuoteChar: char = '"'; Delimiter: char = ';');

function ACBrStr( AString : AnsiString ) : String ;
function ACBrStrToAnsi( AString : String ) : AnsiString ;
function TruncFix( X : Double ) : Integer ;
function RoundABNT(const AValue: Double; const Digits: SmallInt): Double;
function CompareVersions( const VersionStr1, VersionStr2 : String;
  Delimiter: char = '.' ) : Extended;

function TestBit(const Value: Integer; const Bit: Byte): Boolean;
function IntToBin (value: LongInt; digits: integer ): string;
function BinToInt(Value: String): LongInt;

Function BcdToAsc( const StrBCD : AnsiString) : String ;
Function AscToBcd( const ANumStr: String ; const TamanhoBCD : Byte) : AnsiString ;

function IntToLEStr(AInteger: Integer; BytesStr: Integer = 2): AnsiString;
function LEStrToInt(ALEStr: AnsiString): Integer;

Function HexToAscii(const HexStr : String) : AnsiString ;
Function AsciiToHex(const ABinaryString: AnsiString): String;

function BinaryStringToString(const AString: AnsiString): AnsiString;
function StringToBinaryString(const AString: AnsiString): AnsiString;

function PadRight(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String;
function PadLeft(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String;
function PadCenter(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String;
function PadSpace(const AString : String; const nLen : Integer; Separador : String;
   const Caracter : Char = ' ') : String ;

function RemoveString(const sSubStr, sString: String): String;
function RemoveStrings(const AText: String; StringsToRemove: array of AnsiString): AnsiString;
function RemoverEspacosDuplos(const AString: String): String;
function StripHTML(const AHTMLString : AnsiString) : AnsiString;
procedure RemoveEmptyLines( AStringList: TStringList) ;
function RandomName(const LenName : Integer = 8) : String ;

{ PosEx, retirada de StrUtils.pas do D7, para compatibilizar com o Delphi 6
  (que nao possui essa funçao) }
{$IFNDEF COMPILER7_UP}
function PosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;
{$ENDIF}

{$IFNDEF COMPILER6_UP}
  type TRoundToRange = -37..37;
  function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double;
  function SimpleRoundTo(const AValue: Double; const ADigit: TRoundToRange = -2): Double;

  { IfThens retirada de Math.pas do D7, para compatibilizar com o Delphi 5
  (que nao possue essas funçao) }
  function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload;
  function IfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64; overload;
  function IfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0.0): Double; overload;

  { IfThens retirada de StrUtils.pas do D7, para compatibilizar com o Delphi 5
  (que nao possue essas funçao) }
  function IfThen(AValue: Boolean; const ATrue: string;
    AFalse: string = ''): string; overload;
{$endif}

function IfEmptyThen( const AValue, DefaultValue: String; DoTrim: Boolean = True) : String;
function PosAt(const SubStr, S: AnsiString; Ocorrencia : Cardinal = 1): Integer;
function PosLast(const SubStr, S: AnsiString): Integer;
function CountStr(const AString, SubStr : AnsiString ) : Integer ;
Function Poem_Zeros(const Texto : String; const Tamanho : Integer) : String; overload;
function Poem_Zeros(const NumInteiro : Int64 ; Tamanho : Integer) : String ; overload;

Function CreateFormatSettings: TFormatSettings;

Function IntToStrZero(const NumInteiro : Int64; Tamanho : Integer) : String;
function FloatToIntStr(const AValue: Double; const DecimalDigits: SmallInt = 2): String;
function FloatToString(const AValue: Double; SeparadorDecimal: Char = '.';
  AFormat: String = ''): String;
function FormatFloatBr(const AValue: Extended; AFormat: String = ''): String;
function FloatMask(const DecimalDigits: SmallInt = 2): String;
Function StringToFloat( NumString : String ) : Double ;
Function StringToFloatDef( const NumString : String ;
   const DefaultValue : Double ) : Double ;

function FormatDateBr(const ADateTime: TDateTime; AFormat: String = ''): String;
function FormatDateTimeBr(const ADate: TDateTime; AFormat: String = ''): String;
Function StringToDateTime( const DateTimeString : String;
   const Format : String = '') : TDateTime ;
Function StringToDateTimeDef( const DateTimeString : String ;
   const DefaultValue : TDateTime; const Format : String = '') : TDateTime ;
function StoD( YYYYMMDDhhnnss: String) : TDateTime;
function DtoS( ADate : TDateTime) : String;
function DTtoS( ADateTime : TDateTime) : String;

function StrIsAlpha(const S: String): Boolean;
function StrIsAlphaNum(const S: String): Boolean;
function StrIsNumber(const S: String): Boolean;
function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;
function CharIsNum(const C: Char): Boolean;
function OnlyNumber(const AValue: String): String;
function OnlyAlpha(const AValue: String): String;
function OnlyAlphaNum(const AValue: String): String;
function StrIsIP(const AValue: String): Boolean;

function EstaVazio(const AValue: String): Boolean;overload;
procedure EstaVazio(const AValue, AMensagem: String);overload;
function NaoEstaVazio(AValue: String): Boolean;
function EstaZerado(AValue: Double): Boolean;overload;
function EstaZerado(AValue: Integer): Boolean;overload;
procedure EstaZerado(AValue: Integer; AMensagem: String);overload;
function NaoEstaZerado(AValue: Double): Boolean;overload;
function NaoEstaZerado(AValue: Integer): Boolean;overload;
function TamanhoIgual(const AValue: String; const ATamanho: Integer): Boolean;overload;
procedure TamanhoIgual(const AValue: String; const ATamanho: Integer; AMensagem: String);overload;
function TamanhoIgual(const AValue: Integer; const ATamanho: Integer): Boolean;overload;
procedure TamanhoIgual(const AValue: Integer; const ATamanho: Integer; AMensagem: String);overload;
function TamanhoMenor(const AValue: String; const ATamanho: Integer): Boolean;

function TiraAcentos( const AString : String ) : String ;
function TiraAcento( const AChar : AnsiChar ) : AnsiChar ;

function AjustaLinhas(Texto: AnsiString; Colunas: Integer ;
   NumMaxLinhas: Integer = 0; PadLinhas: Boolean = False): AnsiString;
function QuebraLinhas(const Texto: String; const Colunas: Integer;
   const CaracterQuebrar : Char = ' '): String;

function TraduzComando( AString : AnsiString ) : AnsiString ;
Function StringToAsc( AString : AnsiString ) : AnsiString ;
Function AscToString( AString : AnsiString ) : AnsiString ;

function InPort(const PortAddr:word): byte;
procedure OutPort(const PortAddr: word; const Databyte: byte); overload ;

function StrCrypt(const AString, StrChave: AnsiString): AnsiString;
function SomaAscII(const AString : AnsiString): Integer;
function StringCrc16(AString : AnsiString ) : word;

function ApplicationPath: String;
Procedure FindFiles( const FileMask : String; AStringList : TStrings;
  IncludePath : Boolean = True ) ;
Function FilesExists(const FileMask: String) : Boolean ;
Procedure DeleteFiles(const FileMask: String; RaiseExceptionOnFail : Boolean = True)  ;
Procedure TryDeleteFile(const AFile: String; WaitTime: Integer = 100)  ;
function CopyFileTo(const AFromFileName, AToFileName : String;
   const AFailIfExists : Boolean = false) : Boolean;
Function PathWithDelim( const APath : String ) : String ;
Function PathWithoutDelim( const APath : String ) : String ;
Procedure CopyFilesToDir( FileMask : String ; ToDirName : String;
   const ForceDirectory : Boolean = False)  ;
procedure RunCommand(const Command: String; const Params: String = '';
   Wait : Boolean = false; WindowState : Word = 5);
procedure OpenURL( const URL : String ) ;

function FunctionDetect (LibName, FuncName: String; var LibPointer: Pointer)
 : boolean; overload ;
function FunctionDetect (LibName, FuncName: String; var LibPointer: Pointer;
   var LibHandle: THandle ): boolean; overload ;
function UnLoadLibrary(LibName: String ): Boolean ;

function FlushToDisk( sFile: string): boolean;
function FlushFileToDisk( sFile: string): boolean;

Procedure DesligarMaquina(Reboot: Boolean = False; Forcar: Boolean = False;
   LogOff: Boolean = False) ;
Procedure WriteToTXT( const ArqTXT : String; ABinaryString : AnsiString;
   const AppendIfExists : Boolean = True; const AddLineBreak : Boolean = True );
procedure WriteLog( const ArqTXT, ABinaryString : AnsiString ;
   const Traduz : Boolean = False) ;
function TranslateUnprintable( const ABinaryString: AnsiString ): AnsiString;

//funcoes para uso com o modulo ACBrSintegra
function TiraPontos(Str: string): string;
function TBStrZero(const i: string; const Casas: byte): string;
function Space(Tamanho: Integer): string;
function LinhaSimples(Tamanho: Integer): string;
function LinhaDupla(Tamanho: Integer): string;

function EAN13Valido( CodEAN13 : String ) : Boolean ;
function EAN13_DV( CodEAN13 : String ) : String ;

function TranslateString(const S: AnsiString; CP_Destino: Word; CP_Atual: Word = 0): AnsiString;
function MatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean;

function UnZip(S: TStream): AnsiString; overload;
function UnZip(S: AnsiString): AnsiString; overload;

{$IFDEF MSWINDOWS}
var xInp32 : function (wAddr: word): byte; stdcall;
var xOut32 : function (wAddr: word; bOut: byte): byte; stdcall;
var xBlockInput : function (Block: BOOL): BOOL; stdcall;
{$ENDIF}

implementation

var
  Randomized : Boolean ;

{-----------------------------------------------------------------------------
  Todos os Fontes do ACBr usam Encoding CP1252, para manter compatibilidade com
  D5 a D2007, Porém D2009 e superiores e Lazarus 0.9.27 e acima usam UTF8.
  A função abaixo converte a "AString" de ANSI, para UTF8, porém apenas se o
  Compilador usar UNICODE
 -----------------------------------------------------------------------------}
function ACBrStr( AString : AnsiString ) : String ;
begin
{$IFDEF UNICODE}
 {$IFDEF USE_LConvEncoding}
   Result := CP1252ToUTF8( AString ) ;
 {$ELSE}
   {$IFDEF FPC}
     Result := SysToUTF8( AString ) ;
   {$ELSE}
     Result := String(AnsiToUtf8( String(AString) )) ;
   {$ENDIF}
 {$ENDIF}
{$ELSE}
  Result := AString
{$ENDIF}
end ;

{-----------------------------------------------------------------------------
  Converte a AString de UTF8 para ANSI nativo, apenas se o Compilador usar UNICODE
 -----------------------------------------------------------------------------}
function ACBrStrToAnsi(AString: String): AnsiString;
begin
{$IFDEF UNICODE}
 {$IFDEF USE_LConvEncoding}
   Result := UTF8ToCP1252( AString ) ;
 {$ELSE}
   {$IFDEF FPC}
    Result := Utf8ToAnsi( AString ) ;
   {$ELSE}
    Result := AnsiString( AString ) ;
   {$ENDIF}
 {$ENDIF}
{$ELSE}
  Result := AString
{$ENDIF}
end;

{-----------------------------------------------------------------------------
 Corrige, bug da função Trunc.
 Deve calcular Trunc somente com variaveis e nunca com Expressoes, caso contrá-
 rio o resultado pode não ser o esperado.
 // Valores de Teste: Trunc(1,602 x 0,98) | 5 * 12,991 | 2,09 * 23,5
 -----------------------------------------------------------------------------}
function TruncFix( X : Double ) : Integer ;
begin
  Result := Trunc( SimpleRoundTo( X, -9) ) ;
end ;

{-----------------------------------------------------------------------------
 Arredondamento segundo as normas da ABNT NBR 5891/77  (por: DSA)
 Fontes:
 http://www.sofazquemsabe.com/2011/01/como-fazer-arredondamento-da-numeracao.html
 http://partners.bematech.com.br/2011/12/edicao-98-entendendo-o-truncamento-e-arredondamento-no-ecf/
 -----------------------------------------------------------------------------}
function RoundABNT(const AValue: Double; const Digits: SmallInt):Double;
var
   Pow, PowValue, RestPart : Extended;
   IntPart, FracPart, LastNumber : Integer;
Begin
   Pow      := intpower(10, abs(Digits) );
   PowValue := SimpleRoundTo( AValue * Pow, -9) ; // SimpleRoundTo elimina dizimas ;
   IntPart  := trunc( PowValue );
   FracPart := trunc( frac( PowValue ) * 100);

   if (FracPart > 50) then
      Inc( IntPart )

   else if (FracPart = 50) then
    begin
      LastNumber := round( frac( IntPart / 10) * 10);

      if odd(LastNumber) then
         Inc( IntPart )
      else
       begin
         RestPart := frac( PowValue * 10 ) ;

         if RestPart > 0 then
            Inc( IntPart );
       end ;
    end ;

   Result := (IntPart / Pow);
end;

{-----------------------------------------------------------------------------
 Compara 2 Strings de controle de versão. Delimiter padrão = '.'
 Retorna 0 se VersionStr1 = VersionStr2
 Retorna Valor Negativo se VersionStr1 < VersionStr2
 Retorna Valor Positivo se VersionStr1 > VersionStr2
 Retorna valor indicando as diferenças encontras de acordo com os niveis. Ex:
 1.0.3; 1=Major=100, 0=Minor=10, 3=Build=1
 VersionStr1 VersionStr2       Result
    1.0.3      1.0.4        0 + 0 -1 = -1
    1.2.5      1.1.4        0 +10 +1 = 11
    2.0.3      1.2.9       100-10 -1 = 89
 ---------------------------------------------------------------------------- }
function CompareVersions(const VersionStr1, VersionStr2: String; Delimiter: char
  ): Extended;
var
  Niveis, I, P1I, P1F, P2I, P2F: Integer;
  SubVer1, SubVer2: String;
  Pow: Extended;

  Function FormataSubVer( ASubVer: String): String;
  const
    cDIGITOS_COMPARAR = 9;
  begin
     Result := Trim(ASubVer);
     if ASubVer = '' then
       Result := StringOfChar('0',cDIGITOS_COMPARAR)
     else if StrIsNumber(Result) then  // Se for numerico, remove zeros a esquerda
       Result := PadLeft(Result,cDIGITOS_COMPARAR,'0') ;
  end;
begin
  Result := 0;
  if Trim(VersionStr1) = Trim(VersionStr2) then
    exit ;

  Niveis := max( CountStr(VersionStr1, Delimiter), CountStr(VersionStr2, Delimiter) ) ;
  P1I := 1; P1F := 0 ;
  P2I := 1; P2F := 0 ;

  I := Niveis;
  while I >= 0 do
  begin
    P1F := PosEx(Delimiter, VersionStr1, P1I);
    P2F := PosEx(Delimiter, VersionStr2, P2I);

    if P1F = 0 then
      P1F := Length(VersionStr1)+1;
    if P2F = 0 then
      P2F := Length(VersionStr2)+1;

    SubVer1 := FormataSubVer( Copy(VersionStr1, P1I, P1F-P1I) );
    SubVer2 := FormataSubVer( Copy(VersionStr2, P2I, P2F-P2I) );

    if SubVer1 <> SubVer2 then
    begin
      Pow := intpower(10, I );

      if (SubVer1 > SubVer2) then
        Result := Result + Pow
      else
        Result := Result - Pow ;
    end;

    P1I := P1F+1;
    P2I := P2F+1;
    Dec( I );
  end;
end;

{-----------------------------------------------------------------------------
 *** Adaptado de JclLogic.pas  - Project JEDI Code Library (JCL) ***
 Retorna True se o nBit está ativo (ligado) dentro do valor Value. Inicia em 0
 ---------------------------------------------------------------------------- }
function TestBit(const Value: Integer; const Bit: Byte): Boolean;
Var Base : Byte ;
begin
  Base := (Trunc(Bit/8)+1) * 8 ;
  Result := (Value and (1 shl (Bit mod Base))) <> 0;
end;

{-----------------------------------------------------------------------------
 Extraido de  http://delphi.about.com/od/mathematics/a/baseconvert.htm (Zago)
 Converte um Inteiro para uma string com a representação em Binário
 4,4 = '0100'; 15,4 = '1111'; 100,8 = '01100100'; 255,8 = '11111111'
 -----------------------------------------------------------------------------}
function IntToBin ( value: LongInt; digits: integer ): string;
begin
  Result := StringOfChar( '0', digits ) ;
  while value > 0 do
  begin
    if ( value and 1 ) = 1 then
      result [ digits ] := '1';

    dec ( digits ) ;
    value := value shr 1;
  end;
end;

{-----------------------------------------------------------------------------
 converte uma String com a representação de Binário para um Inteiro
 '0100' = 4; '1111' = 15; '01100100' = 100; '11111111' = 255
 -----------------------------------------------------------------------------}
function BinToInt(Value: String): LongInt;
var
  L, I, B: Integer;
begin
  Result := 0;
  
  // remove zeros a esquerda
  while Copy(Value,1,1) = '0' do
    Value := Copy(Value,2,Length(Value)-1) ;

  L := Length(Value);
  for I := L downto 1 do
  begin
    if Value[I] = '1' then
    begin
      B := (1 shl (L-I));
      Result := Result + B ;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Converte uma String no Formato BCD para uma String que pode ser convertida em
  Integer ou Double.  // Adaptada do manual da Bematech //   Exemplo:
  - Se uma variável retornada for de 9 bytes BCD, e seu valor for R$ 1478401.7 os
    7 bytes retornados em caracter (14 dígitos BCD) serão:  0 0 0 0 1 71 132 1 112.
    ou chr(00)+chr(00)+chr(00)+chr(00)+chr(01)+chr(71)+chr(132)+chr(01)+chr(112).
    O retorno deve ser convertido para Hexa: 71dec = 47hex; 132dec = 84hex; 112dec = 70hex
    Nesse caso essa função irá retornar:  "00 00 00 00 01 47 84 01 70"
 ---------------------------------------------------------------------------- }
function BcdToAsc(const StrBCD: AnsiString): String;
Var
  A,BCD_CHAR : Integer ;
  BH,BL,ASC_CHAR : String ;
begin
  result := '' ;

  for A := 1 to Length( StrBCD ) do
  begin
     BCD_CHAR := ord( StrBCD[A] ) ;
     BH := IntToStr( Trunc(BCD_CHAR / 16) ) ;
     If ( BCD_CHAR mod 16 ) > 9 Then
        BL := chr( 48 + BCD_CHAR mod 16 )
     Else
        BL := IntToStr( BCD_CHAR mod 16 ) ;

     ASC_CHAR := BH + BL ;
     Result := Result + ASC_CHAR
  end ;
end;

{-----------------------------------------------------------------------------
  Converte uma String com Numeros para uma String no Formato BCD
  - TamanhoBCD define quantos bytes a String Resultante deve ter
  - Para transformar o valor for  "123456" em 7 bytes BCD, teriamos:
    00 00 00 00 12 34 56    ou
    chr(00) + chr(00) + chr(00) + chr(00) + chr(18) + chr(52) + chr(86).
 ---------------------------------------------------------------------------- }
function AscToBcd(const ANumStr: String; const TamanhoBCD: Byte): AnsiString;
Var
  StrBCD, BCDChar : String ;
  I, L, DecVal: Integer;
begin
  Result := '' ;

  if not StrIsNumber( ANumStr ) then
     raise Exception.Create('Parâmetro "ANumStr" deve conter apenas números') ;

  L := TamanhoBCD*2;
  StrBCD := PadLeft( LeftStr(ANumStr,L), L , '0' );
  For I := 1 to TamanhoBCD do
  begin
     BCDChar := copy(StrBCD, (I*2)-1, 2);
     DecVal := StrToInt( '$'+BCDChar );
     Result := Result + AnsiChar(chr( DecVal ))  ;
  end;
end ;


{-----------------------------------------------------------------------------
  Converte um "AInteger" em uma String binária codificada como Little Endian,
  no tamanho máximo de "BytesStr"
  Exemplos: IntToLEStr( 106 ) = chr(106) + chr(0)
 ---------------------------------------------------------------------------- }
function IntToLEStr(AInteger: Integer; BytesStr: Integer): AnsiString;
var
   AHexStr: String;
   LenHex, P, DecVal: Integer;
begin
  LenHex  := BytesStr * 2 ;
  AHexStr := IntToHex(AInteger,LenHex);
  Result  := '' ;

  P := 1;
  while P < LenHex do
  begin
    DecVal := StrToInt('$'+copy(AHexStr,P,2)) ;
    Result := chr( DecVal ) + Result;
    P := P + 2 ;
  end ;
end;

{-----------------------------------------------------------------------------
  converte uma String binária codificada como Little Endian em Inteiro
  Veja exemplos na function acima
 ---------------------------------------------------------------------------- }
function LEStrToInt(ALEStr: AnsiString): Integer;
var
   AHexStr: String;
   LenLE, P : Integer ;
begin
  LenLE   := Length(ALEStr);
  AHexStr := '';

  P := 1;
  while P <= LenLE do
  begin
    AHexStr := IntToHex(ord(ALEStr[P]),2) + AHexStr;
    Inc( P ) ;
  end ;

  if AHexStr <> '' then
    Result := StrToInt( '$'+AHexStr )
  else
    Result := 0;
end;


{-----------------------------------------------------------------------------
  Converte uma String em HexaDecimal <HexStr> pela sua representação em ASCII
  Ex: "C080" em Hexadecimal é igual a "+Ç" em ASCII que é igual a 49280 que é
      igual a "1100000010000000" em binário
      Portanto se HexStr = "CO80", Result = "+Ç"
 ---------------------------------------------------------------------------- }
function HexToAscii(const HexStr : String) : AnsiString ;
Var
  B   : Byte ;
  Cmd : AnsiString ;
  I, L: Integer ;
begin
  Result := '' ;
  Cmd    := Trim(HexStr);
  I      := 1 ;
  L      := Length( HexStr ) ;

  while I < L do
  begin
     B := StrToIntDef('$' + copy(Cmd, I, 2), 32) ;
     Result := Result + AnsiChar( chr(B) ) ;
     Inc( I, 2) ;
  end ;
end ;

{-----------------------------------------------------------------------------
  Converte uma String pela sua representação em HexaDecimal
  Ex: "C080" em Hexadecimal é igual a "+Ç" em ASCII que é igual a 49280 que é
      igual a "1100000010000000" em binário
      Portanto se AString = "+Ç", Result = "C080"
 ---------------------------------------------------------------------------- }
function AsciiToHex(const ABinaryString: AnsiString): String;
 Var I, L: Integer;
begin
  Result := '' ;
  L := Length(ABinaryString) ;
  for I := 1 to L do
     Result := Result + IntToHex(Ord(ABinaryString[I]), 2);
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
  Tam := Length(AString);
  if Tam < nLen then
    Result := AString + StringOfChar(Caracter, (nLen - Tam))
  else
    Result := LeftStr(AString,nLen) ;
end ;

{-----------------------------------------------------------------------------
  Completa <AString> com <Caracter> a esquerda, até o tamanho <nLen>, Alinhando
  a <AString> a Direita. Se <AString> for maior que <nLen>, ela será truncada
 ---------------------------------------------------------------------------- }
function PadLeft(const AString : String; const nLen : Integer;
   const Caracter : Char) : String ;
var
  Tam: Integer;
begin
  Tam := Length(AString);
  if Tam < nLen then
    Result := StringOfChar(Caracter, (nLen - Tam)) + AString
  else
    Result := RightStr(AString,nLen) ;
end ;

{-----------------------------------------------------------------------------
 Completa <AString> Centralizando, preenchendo com <Caracter> a esquerda e direita
 ---------------------------------------------------------------------------- }
function PadCenter(const AString : String; const nLen : Integer;
   const Caracter : Char) : String ;
var
  nCharLeft: Integer;
  Tam: integer;
begin
  Tam := Length( AString );
  if Tam < nLen then
  begin
    nCharLeft := Trunc( (nLen - Tam) / 2 ) ;
    Result    := PadRight( StringOfChar(Caracter, nCharLeft) + AString, nLen, Caracter) ;
  end
  else
    Result := copy(AString, 1, nLen) ;
end ;

{-----------------------------------------------------------------------------
  Ajusta a <AString> com o tamanho de <nLen> inserindo espaços no meio,
  substituindo <Separador> por n X <Caracter>  (Justificado)
 ---------------------------------------------------------------------------- }
function PadSpace(const AString : String; const nLen : Integer;
   Separador : String; const Caracter : Char = ' ') : String ;
var StuffStr : AnsiString ;
    nSep, nCharSep, nResto, nFeito, Ini : Integer ;
    D : Double ;
begin
  Result := copy(AString,1,nLen) ;
  if Separador = String(Caracter) then  { Troca Separador, senao fica em loop infinito }
  begin
     Result    := AnsiString(StringReplace( String(Result), Separador, #255,[rfReplaceAll]));
     Separador := #255 ;
  end ;

  nSep   := CountStr( Result, AnsiString(Separador) ) ;

  if nSep < 1 then
  begin
     Result := PadRight(Result, nLen, Caracter ) ;
     exit ;
  end ;

  Result   := AnsiString( Trim( String( Result ) ) ) ;
  D        := (nLen - (Length(Result)-nSep)) / nSep ;
  nCharSep := Trunc( D ) ;
  nResto   := nLen - ( (Length(Result)-nSep) + (nCharSep*nSep) ) ;
  nFeito   := nSep ;
  StuffStr := StringOfChar( Caracter, nCharSep ) ;

  Ini := Pos( Separador, String( Result ) ) ;
  while Ini > 0 do
  begin
    Result := AnsiString(
      StuffString(
        String(Result),
        Ini,
        length(Separador),
        String(StuffStr) + ifthen(nFeito <= nResto, String(Caracter), '' )
      )
    );

    nFeito := nFeito - 1 ;
    Ini    := Pos( String(Separador), String(Result) ) ;
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
   Remove todas ocorrencias do array <StringsToRemove> da String <AText>
   retornando a String alterada
 ---------------------------------------------------------------------------- }
function RemoveStrings(const AText: String;
  StringsToRemove: array of AnsiString): AnsiString;
Var
  I, J : Integer ;
  StrToFind : String ;
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
   Remove todas as TAGS de HTML de uma String, retornando a String alterada
 ---------------------------------------------------------------------------- }
function StripHTML(const AHTMLString : AnsiString) : AnsiString;
var
   PosIniTag, PosFimTag ,HTMLSize : Integer;
begin
   Result   := '' ;
   HTMLSize := Length( AHTMLString ) ;

   PosFimTag := 0 ;
   PosIniTag := Pos('<', String(AHTMLString)) ;
   while PosIniTag > 0 do
   begin
      Result := Result + copy(AHTMLString, PosFimTag+1, (PosIniTag - PosFimTag-1 ) ) ;

      PosFimTag := PosEx( '>', String(AHTMLString), PosIniTag ) ;
      if PosFimTag = 0 then
         PosFimTag := PosIniTag-1
      else
         PosIniTag := PosEx( '<', String(AHTMLString), PosFimTag )
   end ;
   Result := Result + copy(AHTMLString, PosFimTag+1, HTMLSize ) ;

end;
{-----------------------------------------------------------------------------
   Remove todas ocorrencias <sSubStr> de <sString>, retornando a String alterada
 ---------------------------------------------------------------------------- }
function RemoveString(const sSubStr, sString : String) : String ;
begin
   Result := StringReplace( sString, sSubStr, '', [rfReplaceAll]);
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
  Retorna quantas ocorrencias de <SubStr> existem em <AString>
 ---------------------------------------------------------------------------- }
function CountStr(const AString, SubStr : AnsiString ) : Integer ;
Var ini : Integer ;
begin
  result := 0 ;
  if SubStr = '' then exit ;

  ini := Pos( SubStr, AString ) ;
  while ini > 0 do
  begin
     Result := Result + 1 ;
     ini    := PosEx( String(SubStr), String(AString), ini + 1 ) ;
  end ;
end ;

{$IFNDEF COMPILER6_UP}
function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double;
var
  LFactor: Double;
begin
  LFactor := IntPower(10, ADigit);
  Result := Round(AValue / LFactor) * LFactor;
end;

function SimpleRoundTo(const AValue: Double; const ADigit: TRoundToRange = -2): Double;
var
  LFactor: Double;
begin
  LFactor := IntPower(10, ADigit);
  Result := Trunc((AValue / LFactor) + 0.5) * LFactor;
end;

function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64): Int64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double): Double;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThen(AValue: Boolean; const ATrue: string;
  AFalse: string = ''): string;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{$endif}

{$IFNDEF COMPILER7_UP}
{-----------------------------------------------------------------------------
 *** PosEx, retirada de StrUtils.pas do Borland Delphi ***
  para compatibilizar com o Delphi 6  (que nao possui essa funçao)
 ---------------------------------------------------------------------------- }
function PosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;
{$endif}

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

{-----------------------------------------------------------------------------
  Acha a Ultima "Ocorrencia" de "SubStr" em "S"
 ---------------------------------------------------------------------------- }
function PosLast(const SubStr, S: AnsiString ): Integer;
Var P : Integer ;
begin
  Result := 0 ;
  P := Pos( SubStr, S) ;
  while P <> 0 do
  begin
     Result := P ;
     P := PosEx( String(SubStr), String(S), P+1) ;
  end ;
end ;

{-----------------------------------------------------------------------------
  Insere ZEROS (0) a esquerda de <Texto> até completar <Tamanho> 
 ---------------------------------------------------------------------------- }
function Poem_Zeros(const Texto : String ; const Tamanho : Integer) : String ;
begin
  Result := PadLeft(LeftStr(Trim(Texto),Tamanho),Tamanho,'0') ;
end ;

function Poem_Zeros(const NumInteiro : Int64 ; Tamanho : Integer) : String ;
begin
  Result := IntToStrZero( NumInteiro, Tamanho) ;
end ;

function CreateFormatSettings: TFormatSettings;
begin
  {$IFDEF FPC}
   Result := DefaultFormatSettings;
  {$ELSE}
   Result := TFormatSettings.Create('');
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  Transforma <NumInteiro> em String, preenchendo com Zeros a Esquerda até
  atingiros digitos de <Tamnho>
 ---------------------------------------------------------------------------- }
function IntToStrZero(const NumInteiro : Int64 ; Tamanho : Integer) : String ;
begin
  Result := Poem_Zeros( IntToStr( NumInteiro ), Tamanho) ;
end ;

{-----------------------------------------------------------------------------
  Converte uma <NumString> para Double, semelhante ao StrToFloatDef, mas
  verifica se a virgula é '.' ou ',' efetuando a conversão se necessário
  Se não for possivel converter, retorna <DefaultValue>
 ---------------------------------------------------------------------------- }
function StringToFloatDef(const NumString : String ; const DefaultValue : Double
   ) : Double ;
begin
  try
     Result := StringToFloat( NumString ) ;
  except
     Result := DefaultValue ;
  end ;
end ;

{-----------------------------------------------------------------------------
  Faz o mesmo que FormatFloat, porém garante que o resultado final terá
  o separador de decimal = ',' e o separador de milhar como Ponto
 ---------------------------------------------------------------------------- }
function FormatFloatBr(const AValue: Extended; AFormat: String): String;
Var
  {$IFDEF HAS_FORMATSETTINGS}
  FS: TFormatSettings;
  {$ELSE}
  OldDecimalSeparator, OldThousandSeparator : Char ;
  {$ENDIF}
begin
  if AFormat = '' then
     AFormat := '0.00';

  {$IFDEF HAS_FORMATSETTINGS}
  FS := CreateFormatSettings;
  FS.DecimalSeparator := ',';
  FS.ThousandSeparator := '.';
  Result := FormatFloat(AFormat, AValue, FS);
  {$ELSE}
  OldDecimalSeparator := DecimalSeparator;
  OldThousandSeparator := ThousandSeparator;
  try
    DecimalSeparator := ',';
    ThousandSeparator := '.';
    Result := FormatFloat(AFormat, AValue);
  finally
    DecimalSeparator := OldDecimalSeparator;
    ThousandSeparator := OldThousandSeparator;
  end;
  {$ENDIF}
end;

function FloatMask(const DecimalDigits: SmallInt): String;
begin
  Result := '0';

  if DecimalDigits > 0 then
     Result := Result + '.' + StringOfChar('0',DecimalDigits)
end;

{-----------------------------------------------------------------------------
  Converte uma <NumString> para Double, semelhante ao StrToFloat, mas
  verifica se a virgula é '.' ou ',' efetuando a conversão se necessário
  Se não for possivel converter, dispara Exception
 ---------------------------------------------------------------------------- }
function StringToFloat(NumString : String) : Double ;
var
  DS: Char;
begin
  NumString := Trim( NumString ) ;

  DS := {$IFDEF HAS_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;

  if DS <> '.' then
     NumString := StringReplace(NumString,'.',DS,[rfReplaceAll]) ;

  if DS <> ',' then
     NumString := StringReplace(NumString,',',DS,[rfReplaceAll]) ;

  Result := StrToFloat(NumString)
end ;

{-----------------------------------------------------------------------------
  Converte um Double para string, SEM o separator decimal, considerando as
  decimais como parte final da String. Ex: 100,00 = "10000"; 1,23 = "123"
 ---------------------------------------------------------------------------- }
function FloatToIntStr(const AValue : Double ; const DecimalDigits : SmallInt
   ) : String ;
var
   Pow : Extended ;
begin
  Pow    := intpower(10, abs(DecimalDigits) );
  Result := IntToStr( Trunc( SimpleRoundTo( AValue * Pow ,0) ) ) ;
end;

{-----------------------------------------------------------------------------
  Converte um Double para string, semelhante a FloatToStr(), porém
  garante que não haverá separador de Milhar e o Separador Decimal será igual a
  "SeparadorDecimal" ( o default é .(ponto))
 ---------------------------------------------------------------------------- }
function FloatToString(const AValue: Double; SeparadorDecimal: Char;
  AFormat: String): String;
var
  DS, TS: Char;
begin
  if EstaVazio(AFormat) then
    Result := FloatToStr(AValue)
  else
    Result := FormatFloat(AFormat, AValue);

  DS := {$IFDEF HAS_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  TS := {$IFDEF HAS_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator;

  // Removendo Separador de milhar //
  Result := StringReplace(Result, TS, '', [rfReplaceAll]);

  // Verificando se precisa mudar Separador decimal //
  if DS <> SeparadorDecimal then
    Result := StringReplace(Result, DS, SeparadorDecimal, [rfReplaceAll]);
end;

{-----------------------------------------------------------------------------
  Converte uma <DateTimeString> para TDateTime, semelhante ao StrToDateTimeDef,
  mas verifica se o seprador da Data é compativo com o S.O., efetuando a
  conversão se necessário. Se não for possivel converter, retorna <DefaultValue>
 ---------------------------------------------------------------------------- }
function StringToDateTimeDef(const DateTimeString : String ;
   const DefaultValue : TDateTime ; const Format : String) : TDateTime ;
begin
  try
     Result := StringToDateTime( DateTimeString, Format ) ;
  except
     Result := DefaultValue ;
  end ;
end ;

function FormatDateBr(const ADateTime: TDateTime; AFormat: String): String;
begin
  if AFormat = '' then
     AFormat := 'DD/MM/YYYY';

  Result := FormatDateTimeBr( DateOf(ADateTime), AFormat);
end;

function FormatDateTimeBr(const ADate: TDateTime; AFormat: String): String;
Var
  {$IFDEF HAS_FORMATSETTINGS}
  FS: TFormatSettings;
  {$ELSE}
  OldDateSeparator: Char ;
  OldTimeSeparator: Char ;
  {$ENDIF}
begin
  if AFormat = '' then
     AFormat := 'DD/MM/YYYY hh:nn:ss';

  {$IFDEF HAS_FORMATSETTINGS}
  FS.DateSeparator := '/';
  FS.TimeSeparator := ':';
  Result := FormatDateTime(AFormat, ADate, FS);
  {$ELSE}
  OldDateSeparator := DateSeparator;
  OldTimeSeparator := TimeSeparator;
  try
    DateSeparator := '/';
    TimeSeparator := ':';
    Result := FormatDateTime(AFormat, ADate);
  finally
    DateSeparator := OldDateSeparator;
    TimeSeparator := OldTimeSeparator;
  end;
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  Converte uma <DateTimeString> para TDateTime, semelhante ao StrToDateTimeDef,
  mas verifica se o seprador da Data é compativo com o S.O., efetuando a
  conversão se necessário. Se não for possivel converter, retorna <DefaultValue>
 ---------------------------------------------------------------------------- }
function StringToDateTime(const DateTimeString : String ; const Format : String
   ) : TDateTime ;
Var
  AStr : String;
  {$IFDEF HAS_FORMATSETTINGS}
  FS: TFormatSettings;
  {$ELSE}
  OldShortDateFormat: String ;
  {$ENDIF}
begin
  {$IFDEF HAS_FORMATSETTINGS}

   if Format <> '' then
     FS.ShortDateFormat := Format;

   AStr := Trim( StringReplace(DateTimeString,'/',FormatSettings.DateSeparator, [rfReplaceAll])) ;
   AStr := StringReplace(AStr,':',FormatSettings.TimeSeparator, [rfReplaceAll]) ;

   Result := StrToDateTime(AStr, FS);
  {$ELSE}
   OldShortDateFormat := ShortDateFormat ;
   try
     if Format <> '' then
       ShortDateFormat := Format ;

     AStr := Trim( StringReplace(DateTimeString,'/',DateSeparator, [rfReplaceAll])) ;
     AStr := StringReplace(AStr,':',TimeSeparator, [rfReplaceAll]) ;

     Result := StrToDateTime( AStr ) ;
   finally
     ShortDateFormat := OldShortDateFormat ;
   end ;
  {$ENDIF}
end ;

{-----------------------------------------------------------------------------
  Converte uma String no formato YYYYMMDDhhnnss  para TDateTime
 ---------------------------------------------------------------------------- }
function StoD( YYYYMMDDhhnnss: String) : TDateTime;
begin
  YYYYMMDDhhnnss := trim( YYYYMMDDhhnnss ) ;

  try
    Result := EncodeDateTime( StrToIntDef(copy(YYYYMMDDhhnnss, 1,4),0),  // YYYY
                              StrToIntDef(copy(YYYYMMDDhhnnss, 5,2),0),  // MM
                              StrToIntDef(copy(YYYYMMDDhhnnss, 7,2),0),  // DD
                              StrToIntDef(copy(YYYYMMDDhhnnss, 9,2),0),  // hh
                              StrToIntDef(copy(YYYYMMDDhhnnss,11,2),0),  // nn
                              StrToIntDef(copy(YYYYMMDDhhnnss,13,2),0),  // ss
                              0 );
  except
    Result := 0;
  end;
end;

{-----------------------------------------------------------------------------
  Converte um TDateTime para uma String no formato YYYYMMDD
 ---------------------------------------------------------------------------- }
function DtoS( ADate : TDateTime) : String;
begin
  Result := FormatDateTime('yyyymmdd', ADate ) ;
end ;

{-----------------------------------------------------------------------------
  Converte um TDateTime para uma String no formato YYYYMMDDhhnnss
 ---------------------------------------------------------------------------- }
function DTtoS( ADateTime : TDateTime) : String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', ADateTime ) ;
end ;

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
  Retorna <True> se <C> é Alpha maiusculo/minusculo 
 ---------------------------------------------------------------------------- }
function CharIsAlpha(const C: Char): Boolean;
begin
  Result := ( C in ['A'..'Z','a'..'z'] ) ;
end ;

{-----------------------------------------------------------------------------
 *** Extraido de JclStrings.pas  - Project JEDI Code Library (JCL) ***
  Retorna <True> se <C> é Númerico 
 ---------------------------------------------------------------------------- }
function CharIsNum(const C: Char): Boolean;
begin
  Result := ( C in ['0'..'9'] ) ;
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

{-----------------------------------------------------------------------------
 ** Baseada em "IsIp" de synautil.pas - Synapse http://www.ararat.cz/synapse/ **
  Retorna <True> se <Value> é um IP Valido
 ---------------------------------------------------------------------------- }
function StrIsIP(const AValue: String): Boolean;
var
  TempIP : string;
  function ByteIsOk(const AValue: string): Boolean;
  var
    x: integer;
  begin
    x := StrToIntDef(AValue, -1);
    Result := (x >= 0) and (x < 256);
    // X may be in correct range, but value still may not be correct value!
    // i.e. "$80"
    if Result then
       Result := StrIsNumber( AValue ) ;
  end;

  function Fetch(var AValue: string; const Delimiter: string): string;
  var
    p : Integer ;
  begin
    p := pos(Delimiter,AValue) ;
    Result := copy(AValue, 1, p-1);
    AValue := copy(AValue, p+1, Length(AValue));
  end;
begin
  TempIP := AValue;
  Result := False;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if ByteIsOk(TempIP) then
    Result := True;
end;

function EstaVazio(const AValue: String): Boolean;
begin
  Result := (AValue = '');
end;

procedure EstaVazio(const AValue, AMensagem: String);
begin
  if EstaVazio(AValue) then
    raise Exception.Create(AMensagem);
end;

function NaoEstaVazio(AValue: String): Boolean;
begin
  Result := not EstaVazio(AValue);
end;

function EstaZerado(AValue: Double): Boolean;
begin
  Result := (AValue = 0);
end;

function EstaZerado(AValue: Integer): Boolean;
begin
  Result := (AValue = 0);
end;

procedure EstaZerado(AValue: Integer; AMensagem: String);
begin
  if EstaZerado(AValue) then
    raise Exception.Create(AMensagem);
end;

function NaoEstaZerado(AValue: Double): Boolean;
begin
  Result := not EstaZerado(AValue);
end;

function NaoEstaZerado(AValue: Integer): Boolean;
begin
  Result := not EstaZerado(AValue);
end;

function TamanhoIgual(const AValue: String; const ATamanho: Integer): Boolean;
begin
 Result := (Length(AValue) = ATamanho);
end;

procedure TamanhoIgual(const AValue: String; const ATamanho: Integer;
  AMensagem: String);
begin
  if not TamanhoIgual(AValue, ATamanho) then
    raise Exception.Create(AMensagem);
end;

function TamanhoIgual(const AValue: Integer; const ATamanho: Integer): Boolean;
begin
  Result := (Length(IntToStr(AValue)) = ATamanho);
end;

procedure TamanhoIgual(const AValue: Integer; const ATamanho: Integer;
  AMensagem: String);
begin
  if not TamanhoIgual(AValue, ATamanho) then
    raise Exception.Create(AMensagem);
end;

function TamanhoMenor(const AValue: String; const ATamanho: Integer): Boolean;
begin
  Result := (Length(AValue) < ATamanho);
end;

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
  AnsiStr := ACBrStrToAnsi( AString );
  For A := 1 to Length( AnsiStr ) do
  begin
     Letra := TiraAcento( AnsiStr[A] ) ;
     if not (Letra in [#32..#126,#13,#10,#8]) then    {Letras / numeros / pontos / sinais}
        Letra := ' ' ;
     Ret := Ret + Letra ;
  end ;

  Result := ACBrStr(Ret)
end ;

{-----------------------------------------------------------------------------
  Substitui caracter acentuado por compativel
 ---------------------------------------------------------------------------- }
function TiraAcento( const AChar : AnsiChar ) : AnsiChar ;
begin
  case AChar of
    'à','á','ã','ä','â' : Result := 'a' ;
    'À','Á','Ã','Ä','Â' : Result := 'A' ;
    'è','é',    'ë','ê' : Result := 'e' ;
    'È','É',    'Ë','Ê' : Result := 'E' ;
    'ì','í',    'ï','î' : Result := 'i' ;
    'Ì','Í',    'Ï','Î' : Result := 'I' ;
    'ò','ó','õ','ö','ô' : Result := 'o' ;
    'Ò','Ó','Õ','Ö','Ô' : Result := 'O' ;
    'ù','ú',    'ü','û' : Result := 'u' ;
    'Ù','Ú',    'Ü','Û' : Result := 'U' ;
    'ç'                 : Result := 'c' ;
    'Ç'                 : Result := 'C' ;
    'ñ'                 : Result := 'n' ;
    'Ñ'                 : Result := 'N' ;
  else
    Result := AChar ;
  end;
end ;

{-----------------------------------------------------------------------------
  Quebra Linhas grandes no máximo de Colunas especificado, ou caso encontre 
  uma quebra de Linha (CR ou CR+LF)
 ---------------------------------------------------------------------------- }
function AjustaLinhas(Texto: AnsiString; Colunas: Integer ;
   NumMaxLinhas: Integer = 0; PadLinhas: Boolean = False): AnsiString;
Var
  Count,P,I : Integer ;
  Linha, CRLF : AnsiString ;
begin
  { Trocando todos os #13+#10 por #10 }
  CRLF := sLineBreak ;
  if (CRLF <> #13+#10) then
     Texto := AnsiString(StringReplace(String(Texto), #13+#10, #10, [rfReplaceAll])) ;

  if (CRLF <> #10) then
     Texto := AnsiString(StringReplace(String(Texto), String(CRLF), #10, [rfReplaceAll])) ;

  { Ajustando a largura das Linhas para o máximo permitido em  "Colunas"
    e limitando em "NumMaxLinhas" o total de Linhas}
  Count  := 0 ;
  Result := '' ;
  while ((Count < NumMaxLinhas) or (NumMaxLinhas = 0)) and
        (Length(Texto) > 0) do
  begin
     P := pos(#10, String( Texto ) ) ;
     if P > (Colunas + 1) then
        P := Colunas + 1 ;

     if P = 0 then
        P := min( Length( Texto ), Colunas ) + 1 ;

     // somar 2 quando encontrar uma tag para não quebrar ela
     if (Copy(Texto, P-1, 1) = '<') or (Copy(Texto, P-2, 2) = '</') then
        inc(P, 2);

     I := 0 ;
     if copy(Texto,P,1) = #10 then   // Pula #10 ?
        I := 1 ;

     Linha := copy(Texto,1,P-1) ;    // Remove #10 (se hover)

     if PadLinhas then
        Result := Result + PadRight( Linha, Colunas) + #10
     else
        Result := Result + Linha + #10 ;

     Inc(Count) ;
     Texto := copy(Texto, P+I, Length(Texto) ) ;
  end ;

  { Permitir impressão de uma linha em branco --Acrescentado por Marciano Lizzoni }
  if Result = '' then
    Result := Result + #10;
end;

function QuebraLinhas(const Texto: String; const Colunas: Integer;
   const CaracterQuebrar : Char = ' '): String;
Var
  PosIni, PosFim, Tamanho : Integer ;
begin
  Result  := '';
  Tamanho := Length(Texto) ;
  PosIni  := 1 ;
  repeat
     if PosIni > 1 then
        Result := Result + sLineBreak;

     PosFim := PosIni + Colunas - 1 ;

     if Tamanho > PosFim then                  // Ainda tem proxima linha ?
        if Texto[PosFim+1] <> CaracterQuebrar then   // Proximo já é uma Quebra ?
           while (Texto[PosFim] <> CaracterQuebrar) and (PosFim > PosIni) do // Ache uma Quebra
              Dec(PosFim) ;

     if PosFim = PosIni then  // Não foi capaz de encontrar uma quebra
        PosFim := PosIni + Colunas - 1 ;

     Result := Result + Copy( Texto, PosIni, (PosFim-PosIni)+1 );
     PosIni := PosFim + 1 ;

     // Pula CaracterQuebrar no Inicio da String
     while (Texto[PosIni] = CaracterQuebrar) and (PosIni <= Tamanho) do
        Inc(PosIni) ;

  until (PosIni > Tamanho);
end;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo '#13,v,#10', substituindo #nn por chr(nn). Ignora todo
   texto apos a String ' | '
 ---------------------------------------------------------------------------- }
function TraduzComando( AString : AnsiString ) : AnsiString ;
Var A : Integer ;
begin
  A := pos(' | ', String( AString ) ) ;
  if A > 0 then
     AString := copy(AString,1,A-1) ;   { removendo texto apos ' | ' }

  Result := AscToString( AString ) ;
end ;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo chr(13)+chr(10) para uma representação que possa ser
   lida por AscToString Ex: '#13,#10'
 ---------------------------------------------------------------------------- }
function StringToAsc( AString: AnsiString): AnsiString;
Var A : Integer ;
begin
  Result := '' ;
  For A := 1 to Length( AString ) do
     Result := Result + AnsiString('#'+IntToStr( Ord( AString[A] ) )+',') ;

  Result := copy(Result,1, Length( Result )-1 ) ;
end;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo '#13,v,#10', substituindo #nn por chr(nn).
  Usar , para separar um campo do outro... No exemplo acima o resultado seria
  chr(13)+'v'+chr(10) 
 ---------------------------------------------------------------------------- }
function AscToString(AString: AnsiString): AnsiString;
Var A : Integer ;
    Token : AnsiString ;
    C : AnsiChar ;
begin
  AString := AnsiString( Trim( String( AString ) ) );
  Result  := '' ;
  A       := 1  ;
  Token   := '' ;

  while A <= length( AString ) + 1 do
  begin
     if A > length( AString ) then
        C := ','
     else
        C := AString[A] ;

     if (C = ',') and (Length( Token ) >= 1) then
      begin
        if Token[1] = '#' then
        try
           Token := AnsiChar( chr( StrToInt( copy(String(Token),2,length(String(Token))) ) ) );
        except
        end ;

        Result := Result + Token ;
        Token := '' ;
      end
     else
        Token := Token + C ;

     A := A + 1 ;
  end ;
end;

{-----------------------------------------------------------------------------
 Substitui todos os caracteres de Controle ( menor que ASCII 32 ou maior que
 ASCII 127), de <AString> por sua representação em HEXA. (\xNN)
 Use StringToBinaryString para Converter para o valor original.
 ---------------------------------------------------------------------------- }
function BinaryStringToString(const AString: AnsiString): AnsiString;
var
   ASC : Integer;
   I, N : Integer;
begin
  Result  := '' ;
  N := Length(AString) ;
  For I := 1 to N do
  begin
     ASC := Ord(AString[I]) ;
     if (ASC < 32) or (ASC > 127) then
        Result := Result + AnsiString('\x'+Trim(IntToHex(ASC,2)))
     else
        Result := Result + AString[I] ;
  end ;
end ;

{-----------------------------------------------------------------------------
 Substitui toda representação em HEXA de <AString> (Iniciada por \xNN, (onde NN,
 é o valor em Hexa)).
 Retornana o Estado original, AString de BinaryStringToString.
 ---------------------------------------------------------------------------- }
function StringToBinaryString(const AString: AnsiString): AnsiString;
var
   P : LongInt;
   Hex : String;
   CharHex : AnsiChar;
begin
  Result := AString ;

  P := pos('\x',String(Result)) ;
  while P > 0 do
  begin
     Hex := copy(String(Result),P+2,2) ;

     try
        CharHex := AnsiChar( Chr(StrToInt('$'+Hex)) ) ;
     except
        CharHex := ' ' ;
     end ;

     Result := AnsiString( StringReplace(String(Result),'\x'+Hex,String(CharHex),[rfReplaceAll]) );
     P      := pos('\x',String(Result)) ;
  end ;
end ;

{-----------------------------------------------------------------------------
 Retorna a String <AString> encriptada por <StrChave>.
 Use a mesma chave para Encriptar e Desencriptar
 ---------------------------------------------------------------------------- }
function StrCrypt(const AString, StrChave: AnsiString): AnsiString;
var
  i, TamanhoString, pos, PosLetra, TamanhoChave: Integer;
  C : AnsiChar ;
begin
  Result        := AString;
  TamanhoString := Length(AString);
  TamanhoChave  := Length(StrChave);

  for i := 1 to TamanhoString do
  begin
     pos := (i mod TamanhoChave);
     if pos = 0 then
        pos := TamanhoChave;

     posLetra := ord(Result[i]) xor ord(StrChave[pos]);
     if posLetra = 0 then
        posLetra := ord(Result[i]);

     C := AnsiChar( chr(posLetra) );
     Result[i] := C ;
  end;
end ;

{-----------------------------------------------------------------------------
 Retorna a soma dos Valores ASCII de todos os char de <AString>
 -----------------------------------------------------------------------------}
function SomaAscII(const AString : AnsiString): Integer;
Var A , TamanhoString : Integer ;
begin
  Result        := 0 ;
  TamanhoString := Length(AString);

  For A := 1 to TamanhoString do
     Result := Result + ord( AString[A] ) ;
end ;

{-----------------------------------------------------------------------------
 Retorna valor de CRC16 de <AString>    http://www.ibrtses.com/delphi/dcrc.html
 -----------------------------------------------------------------------------}
function StringCrc16(AString : AnsiString ):word;

  procedure ByteCrc(data:byte;var crc:word);
   Var i : Byte;
  begin
    For i := 0 to 7 do
    begin
       if ((data and $01) xor (crc and $0001)<>0) then
        begin
          crc := crc shr 1;
          crc := crc xor $A001;
        end
       else
          crc := crc shr 1;

       data := data shr 1; // this line is not ELSE and executed anyway.
    end;
  end;

  var len,i : integer;
begin
 len    := length(AString);
 Result := 0;

 for i := 1 to len do
    bytecrc( ord( AString[i] ), Result);
end;


{-----------------------------------------------------------------------------
 Lê 1 byte de uma porta de Hardware
 Nota: - Essa funçao funciona normalmente em Win9x,
        - XP /NT /2000, deve-se usar um device driver que permita acesso direto
          a porta do Hardware a ser acessado (consulte o fabricante do Hardware)
        - Linux: é necessário ser ROOT para acessar man man
          (use: su  ou  chmod u+s SeuPrograma )
 ---------------------------------------------------------------------------- }
{$WARNINGS OFF}
function InPort(const PortAddr:word): byte;
{$IFNDEF MSWINDOWS}
var Buffer : Pointer ;
    FDevice : String ;
    N : Integer ;
    FHandle : Integer ;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Assigned( xInp32 ) then
     Result := xInp32(PortAddr)
{$ELSE}
  FDevice := '/dev/port' ;
  Buffer  := @Result ;

  FHandle := FileOpen(FDevice, fmOpenRead);
  if FHandle <= 0 then
     raise Exception.Create('Erro abrindo:  '+FDevice+#10+#10+
                            'Você deve ter direito de Leitura nesse diretório.');
  try
     N := FileSeek( FHandle, PortAddr, 0 )  ;
     if N <= 0 then
        raise Exception.Create('Erro ao acessar a porta: '+IntToStr(PortAddr));


     N := FileRead(FHandle, Buffer^, 1) ;
     if N <= 0 then
        raise Exception.Create('Erro ao ler a porta: '+IntToStr(PortAddr));
  finally
     FileClose( FHandle );
  end ;
{$ENDIF}
end ;
{$WARNINGS ON}

{-----------------------------------------------------------------------------
 Envia 1 byte para uma porta de Hardware 
 Nota: - Essa funçao funciona normalmente em Win9x,
        - XP /NT /2000, deve-se usar um device driver que permita acesso direto
          a porta do Hardware a ser acessado (consulte o fabricante do Hardware)
        - Linux: é necessário ser ROOT para acessar /dev/port
          (use: su  ou  chmod u+s SeuPrograma ) 
 ---------------------------------------------------------------------------- }
procedure OutPort(const PortAddr: word; const Databyte: byte);
{$IFNDEF MSWINDOWS}
var Buffer : Pointer ;
    FDevice : String ;
    N : Integer ;
    FHandle : Integer ;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Assigned( xOut32 ) then
     xOut32(PortAddr, Databyte)
{$ELSE}
  Buffer := @Databyte ;
  FDevice := '/dev/port' ;

  FHandle := FileOpen(FDevice, fmOpenWrite);
  if FHandle <= 0 then
     raise Exception.Create('Erro abrindo:  '+FDevice+#10+#10+
                            'Você deve ter direito de Escrita nesse diretório.');
  try
     N := FileSeek( FHandle, PortAddr, 0 )  ;
     if N <= 0 then
        raise Exception.Create('Erro ao acessar a porta: '+IntToStr(PortAddr));

     N := FileWrite(Fhandle, Buffer^, 1) ;
     if N <= 0 then
        raise Exception.Create('Erro ao escrever na porta: '+IntToStr(PortAddr));
  finally
     FileClose( FHandle );
  end ;
//sleep(2)
{$ENDIF}
end ;

{-----------------------------------------------------------------------------
 Retorna String contendo o Path da Aplicação
-----------------------------------------------------------------------------}
function ApplicationPath: String;
begin
  Result := PathWithDelim(ExtractFilePath(ParamStr(0)));
end;

{-----------------------------------------------------------------------------
 Encontra arquivos que correspondam a "FileMask" e cria lista com o Path e nome
 dos mesmos em "AstringList"
-----------------------------------------------------------------------------}
procedure FindFiles(const FileMask : String ; AStringList : TStrings ;
   IncludePath : Boolean) ;
var SearchRec : TSearchRec ;
    RetFind   : Integer ;
    LastFile  : string ;
    Path      : String ;
begin
  LastFile := '' ;
  Path     := ExtractFilePath(FileMask) ;
  RetFind  := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec);
  AStringList.Clear;

  try
     while (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;

        if pos(LastFile, '..') = 0 then    { ignora . e .. }
           AStringList.Add( IfThen(IncludePath, Path, '') + LastFile) ;

        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end;


{-----------------------------------------------------------------------------
  Semelhante a FileExists, mas permite uso de mascaras Ex:(*.BAK, TEST*.PX, etc)
 ---------------------------------------------------------------------------- }
function FilesExists(const FileMask : String) : Boolean ;
var SearchRec : TSearchRec ;
    RetFind   : Integer ;
    LastFile  : string ;
begin
  LastFile := '' ;
  Result   := false ;
  RetFind  := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec) ;
  try
     while (not Result) and (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;
        Result   := (pos(LastFile, '..') = 0) ;   { ignora . e .. }
        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end ;


{-----------------------------------------------------------------------------
  Semelhante a DeleteFile, porem tenta deletar o Arquivo por
  <WaitTime> milisegundos. Gera Exceção se não conseguir apagar o arquivo.
 ---------------------------------------------------------------------------- }
procedure TryDeleteFile(const AFile : String ; WaitTime : Integer) ;
  Var TFim : TDateTime ;
begin
  TFim := IncMilliSecond(now,WaitTime) ;
  repeat
     SysUtils.DeleteFile( AFile ) ;
  until (not FileExists( AFile )) or (now > TFim) ;

  if FileExists( AFile ) then
     raise Exception.Create('Erro ao apagar: ' + AFile);
end ;
{-----------------------------------------------------------------------------
  Semelhante a DeleteFile, mas permite uso de mascaras Ex:(*.BAK, TEST*.PX, etc)
  Gera Exceção se não conseguir apagar algum dos arquivos.
 ---------------------------------------------------------------------------- }
procedure DeleteFiles(const FileMask : String ; RaiseExceptionOnFail : Boolean
   ) ;
var SearchRec : TSearchRec ;
    RetFind   : Integer ;
    LastFile  : string ;
    Path      : String ;
begin
  LastFile := '' ;
  Path     := ExtractFilePath(FileMask) ;
  RetFind  := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec);
  try
     while (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;

        if pos(LastFile, '..') = 0 then    { ignora . e .. }
        begin
           if not SysUtils.DeleteFile(Path + LastFile) then
             if RaiseExceptionOnFail then
               raise Exception.Create('Erro ao apagar: ' + Path + LastFile);
        end ;

        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end ;

{-----------------------------------------------------------------------------
 *** CopyFileTo Extraida de idGlobals.pas - INDY ***
 Copia arquivo "AFromFilename" para "AToFilename".  Retorna true se OK
 Nao copia, e retorna false se o destino "AToFilename" já existir e
   "AFailIfExists"  for true
 ---------------------------------------------------------------------------- }
function CopyFileTo(const AFromFileName, AToFileName : String;
   const AFailIfExists : Boolean) : Boolean;
{$IFNDEF MSWINDOWS}
var LStream : TStream;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := CopyFile(PChar(AFromFileName), PChar(AToFileName), AFailIfExists);
  {$ELSE}
    if FileExists(AToFileName) and AFailIfExists then
       Result := False
    else
     begin
       LStream := TFileStream.Create(AFromFileName, fmOpenRead or fmShareDenyWrite);
       try
          with TFileStream.Create(AToFileName, fmCreate) do
             try
                CopyFrom(LStream, 0);
             finally
                Free;
             end;
       finally
          FreeAndNil(LStream);
       end;
       Result := True;
     end;
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  Verifica se <APath> possui "PathDelim" no final. Retorna String com o Path
  já ajustado
 ---------------------------------------------------------------------------- }
function PathWithDelim(const APath : String) : String ;
begin
  Result := Trim(APath) ;
  if Result <> '' then
     if RightStr(Result,1) <> PathDelim then   { Tem delimitador no final ? }
        Result := Result + PathDelim ;
end ;

{-----------------------------------------------------------------------------
  Verifica se <APath> possui "PathDelim" no final. Retorna String SEM o
  DELIMITADOR de Path no final
 ---------------------------------------------------------------------------- }
function PathWithoutDelim(const APath : String) : String ;
Var
  Delimiters : AnsiString ;
begin
  Result := Trim(APath) ;

  Delimiters := PathDelim+'/\' ;
  while (Result <> '') and (pos(String(RightStr(Result,1)), String(Delimiters) ) > 0) do   { Tem delimitador no final ? }
     Result := copy(Result,1,Length(Result)-1)
end;

{-----------------------------------------------------------------------------
  Copia todos os arquivos especificados na mascara <FileMask> para o diretório
  <ToDirName>   Gera Exceção se não conseguir copiar algum dos arquivos.
 ---------------------------------------------------------------------------- }
procedure CopyFilesToDir(FileMask : String ; ToDirName : String ;
   const ForceDirectory : Boolean) ;
var SearchRec : TSearchRec ;
    RetFind   : Integer ;
    LastFile  : string ;
    Path      : String ;
begin
  ToDirName := PathWithDelim(ToDirName) ;
  FileMask  := Trim(FileMask) ;

  if ToDirName = '' then
     raise Exception.Create('Diretório destino não especificado') ;

  if not DirectoryExists(ToDirName) then
  begin
     if not ForceDirectory then
        raise Exception.Create('Diretório ('+ToDirName+') não existente.')
     else
      begin
        ForceDirectories( ToDirName ) ;  { Tenta criar o diretório }
        if not DirectoryExists( ToDirName ) then
           raise Exception.Create( 'Não foi possivel criar o diretório' + sLineBreak +
                                   ToDirName);
      end ;
  end ;

  LastFile := '' ;
  Path     := ExtractFilePath(FileMask) ;
  RetFind  := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec);
  try
     while (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;

        if pos(LastFile, '..') = 0 then    { ignora . e .. }
        begin
           if not CopyFileTo(Path + LastFile, ToDirName + LastFile) then
             raise Exception.Create('Erro ao Copiar o arquivo ('+
                  Path + LastFile + ') para o diretório ('+ToDirName+')') ;
        end ;

        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end ;

{-----------------------------------------------------------------------------
 - Executa programa Externo descrito em "Command", adcionando os Parametros
   "Params" na linha de comando
 - Se "Wait" for true para a execução da aplicação para esperar a conclusao do
   programa externo executado por "Command"
 - WindowState apenas é utilizado na plataforma Windows
 ---------------------------------------------------------------------------- }
procedure RunCommand(const Command: String; const Params: String;
   Wait : Boolean; WindowState : Word);
var
  {$ifdef MSWINDOWS}
   SUInfo: TStartupInfo;
   ProcInfo: TProcessInformation;
   Executed : Boolean ;
   PCharStr : PChar ;
  {$endif}
  ConnectCommand : PChar;
  {$ifdef LINUX}
   FullCommand : AnsiString;
  {$endif}
begin
  {$ifdef LINUX}
     FullCommand := Trim(Command + ' ' + Params) ;
     if not Wait then
        FullCommand := FullCommand + ' &' ;  { & = Rodar em BackGround }

     {$IFNDEF FPC}
       ConnectCommand := PChar(FullCommand);
       Libc.system(ConnectCommand);
     {$ELSE}
       fpSystem(FullCommand)
     {$ENDIF}
  {$endif}
  {$ifdef MSWINDOWS}
     PCharStr := PChar(Trim(Params)) ;
     if Length(PCharStr) = 0 then
        PCharStr := nil ;

     if not Wait then
        ShellExecute(0,'open',PChar(Trim(Command)),PCharStr, nil, WindowState )
//        winexec(ConnectCommand, WindowState)
     else
      begin
        ConnectCommand := PChar(Trim(Command) + ' ' + Trim(Params));
        PCharStr := PChar(ExtractFilePath(Command)) ;
        if Length(PCharStr) = 0 then
           PCharStr := nil ;
        FillChar(SUInfo, SizeOf(SUInfo), #0);
        with SUInfo do
        begin
           cb          := SizeOf(SUInfo);
           dwFlags     := STARTF_USESHOWWINDOW;
           wShowWindow := WindowState;
        end;

        Executed := CreateProcess(nil, ConnectCommand, nil, nil, false,
                    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil,
                    PCharStr, SUInfo, ProcInfo);

        try
           { Aguarda até ser finalizado }
           if Executed then
              WaitForSingleObject(ProcInfo.hProcess, INFINITE);
        finally
           { Libera os Handles }
           CloseHandle(ProcInfo.hProcess);
           CloseHandle(ProcInfo.hThread);
        end;
      end;
  {$endif}
end;

procedure OpenURL( const URL : String ) ;
{$IFDEF LINUX}
  Var BrowserName : String ;
{$ENDIF}
begin
 {$IFDEF USE_LCLIntf}
   LCLIntf.OpenURL( URL ) ;
 {$ELSE}
   {$IFDEF MSWINDOWS}
     RunCommand(URL);
   {$ENDIF}
   {$IFDEF LINUX}
     BrowserName := GetEnvironmentVariable('BROWSER') ;
     if BrowserName = '' then
        BrowserName := 'konqueror' ;

     RunCommand(BrowserName, URL);
   {$ENDIF}
 {$ENDIF}
end ;

{$IFDEF MSWINDOWS}
 { Fonte: http://stackoverflow.com/questions/1635947/how-to-make-sure-that-a-file-was-permanently-saved-on-usb-when-user-doesnt-use }
 function FlushToDisk( sFile: string): boolean;
 var
   hDrive: THandle;
   S:      string;
   OSFlushed: boolean;
   bResult: boolean;
 begin
   bResult := False;
   S := '\\.\' + ExtractFileDrive( sFile )[1] + ':';

   //NOTE: this may only work for the SYSTEM user
   hDrive    := CreateFile(PChar(S), GENERIC_READ or
     GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
     OPEN_EXISTING, 0, 0);
   OSFlushed := FlushFileBuffers(hDrive);

   CloseHandle(hDrive);

   if OSFlushed then
   begin
     bResult := True;
   end;

   Result := bResult;
 end;
{$ELSE}
 function FlushToDisk(sFile: string): boolean;
 var
   hDrive: THandle;
 begin
   hDrive := fpOpen(sFile, O_Creat or O_RDWR {$IFDEF LINUX}or O_SYNC{$ENDIF});
   Result := (fpfsync(hDrive) = 0);
   fpClose(hDrive);
 end ;
{$ENDIF}

{$IFDEF MSWINDOWS}
 { Discussão em: http://www.djsystem.com.br/acbr/forum/viewtopic.php?f=5&t=5811 }
 function FlushFileToDisk( sFile: string): boolean;
 var
   hFile: THandle;
   //bResult: boolean;
   //lastErr: Cardinal;
   filename: WideString;
 begin
   //Result := False;

   filename := '\\.\' + sFile; //Para usar a versão Wide da função CreateFile e aceitar o caminho completo do arquivo

   hFile := Windows.CreateFileW( PWideChar(filename),
               GENERIC_READ or GENERIC_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
               FILE_ATTRIBUTE_NORMAL  or FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING, 0);

//   GetLasError Verifica se houve algum erro na execução de CreateFile
//   lastErr := GetLastError();
//
//   if (lastErr <> ERROR_SUCCESS) then
//   begin
//     Beep( 750, 100);
////     try
//       RaiseLastOSError(lastErr);
////     except
////       on Ex : EOSError do
////       begin
////          MessageDlg('Caught an OS error with code: ' +
////             IntToStr(Ex.ErrorCode), mtError, [mbOK], 0);
////          SetLastError(ERROR_SUCCESS);
////       end
////     end;
//   end;

    Result := FlushFileBuffers(hFile);

//   GetLasError Verifica se houve algum erro na execução de FlushFileBuffers
//    lastErr := GetLastError();
//
//    if (lastErr <> ERROR_SUCCESS) then
//    begin
//   if (lastErr <> ERROR_SUCCESS) then
//   begin
//     Beep( 750, 100);
////     try
//       RaiseLastOSError(lastErr);
////     except
////       on Ex : EOSError do
////       begin
////          MessageDlg('Caught an OS error with code: ' +
////             IntToStr(Ex.ErrorCode), mtError, [mbOK], 0);
////          SetLastError(ERROR_SUCCESS);
////       end
////     end;
//   end;

    CloseHandle(hFile);
 end;
{$ELSE}
 function FlushFileToDisk(sFile: string): boolean;
 var
   hDrive: THandle;
 begin
   hDrive := fpOpen(sFile, O_Creat or O_RDWR {$IFDEF LINUX}or O_SYNC{$ENDIF});
   Result := (fpfsync(hDrive) = 0);
   fpClose(hDrive);
 end ;
{$ENDIF}

{-----------------------------------------------------------------------------
 - Tenta desligar a Maquina.
 - Se "Reboot" for true Reinicializa
 *** Versão Windows extraida do www.forumweb.com.br/forum  por: Rafael Luiz ***
 ---------------------------------------------------------------------------- }
procedure DesligarMaquina(Reboot : Boolean ; Forcar : Boolean ; LogOff : Boolean
   ) ;

{$IFDEF MSWINDOWS}
   function WindowsNT: Boolean;
   var
     osVersao : TOSVersionInfo;
   begin
     osVersao.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
     GetVersionEx(osVersao);
     Result := osVersao.dwPlatformID = VER_PLATFORM_WIN32_NT;
   end;

   procedure ObtemPrivilegios;
   var
     tmpLUID : TLargeInteger;
     hdlProc, hdlToken : THandle;
     tkpNovo, tkpIgnore : TTokenPrivileges;
     dwBuffer, dwIgnoreBuffer : DWord;
   begin
     // Abrir token do processo para ajustar privilégios
     hdlProc := GetCurrentProcess;
     OpenProcessToken(hdlProc, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
         hdlToken);

     // Obter o identificador único do privilégio de shutdown
     LookupPrivilegeValue('', 'SeShutdownPrivilege', tmpLUID);

     // Habilita o privilégio de shutdown em novo token
     tkpNovo.PrivilegeCount := 1;
     tkpNovo.Privileges[0].Luid := tmpLUID;
     tkpNovo.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
     dwBuffer := SizeOf(TTokenPrivileges);

     // Ajusta o privilégio com o novo token
     AdjustTokenPrivileges(hdlToken, False, tkpNovo,
         dwbuffer, tkpIgnore, dwIgnoreBuffer);
   end;


    Var RebootParam : Longword ;
   begin
      if WindowsNT then
         ObtemPrivilegios;

      if Reboot then
         RebootParam := EWX_REBOOT
      else if LogOff then
         RebootParam := EWX_LOGOFF
      else
         RebootParam := EWX_SHUTDOWN  ;

      if Forcar then
         RebootParam := RebootParam or EWX_FORCE ;

      ExitWindowsEx(RebootParam, 0);
   end;

{$ELSE}
   begin
      // Precisa ser o ROOT ou a
      // aplicação ter provilegios de ROOT  (use: su  ,  chmod u+s SeuPrograma )
      if Reboot then
         RunCommand('reboot')
      else
         RunCommand('halt') ;
   end ;
{$ENDIF}

{-----------------------------------------------------------------------------
 - Grava conteudo de "AString" no arquivo "ArqTXT".
 - Se arquivo "ArqTXT" não existir, será criado.  Se "ArqTXT" já existir e
   "Append" for verdadeiro adiciona "AString" no final do arquivo
 ---------------------------------------------------------------------------- }
procedure WriteToTXT(const ArqTXT : String; ABinaryString: AnsiString;
  const AppendIfExists: Boolean; const AddLineBreak: Boolean);
var
  FS : TFileStream ;
  LineBreak : AnsiString ;
begin
  FS := TFileStream.Create( ArqTXT,
               IfThen( AppendIfExists and FileExists(ArqTXT),
                       Integer(fmOpenReadWrite), Integer(fmCreate)) or fmShareDenyWrite );
  try
     FS.Seek(0, soFromEnd);  // vai para EOF
     FS.Write(Pointer(ABinaryString)^,Length(ABinaryString));

     if AddLineBreak then
     begin
        LineBreak := sLineBreak;
        FS.Write(Pointer(LineBreak)^,Length(LineBreak));
     end ;
  finally
     FS.Free ;
  end;
end;

procedure WriteLog(const ArqTXT, ABinaryString: AnsiString;
  const Traduz: Boolean);
var
  Buf: AnsiString;
begin
  if ArqTXT = '' then
     exit ;

  if Traduz then
     Buf := TranslateUnprintable(ABinaryString)
  else
     Buf := ABinaryString;

  try
     WriteToTXT(ArqTXT, Buf, True, True);
  except
  end ;
end;

function TranslateUnprintable(const ABinaryString: AnsiString): AnsiString;
Var
  Buf, Ch : AnsiString ;
  I   : Integer ;
  ASC : Byte ;
begin
  Buf := '' ;
  For I := 1 to Length(ABinaryString) do
  begin
     ASC := Ord(ABinaryString[I]) ;

     case ABinaryString[I] of
        NUL   : Ch := '[NUL]' ;
        SOH   : Ch := '[SOH]' ;
        STX   : Ch := '[STX]' ;
        ETX   : Ch := '[ETX]' ;
        ENQ   : Ch := '[ENQ]' ;
        ACK   : Ch := '[ACK]' ;
        TAB   : Ch := '[TAB]' ;
        BS    : Ch := '[BS]' ;
        LF    : Ch := '[LF]' ;
        FF    : Ch := '[FF]' ;
        CR    : Ch := '[CR]' ;
        WAK   : Ch := '[WAK]' ;
        NAK   : Ch := '[NAK]' ;
        ESC   : Ch := '[ESC]' ;
        FS    : Ch := '[FS]' ;
        GS    : Ch := '[GS]' ;
        #32..#126 : Ch := ABinaryString[I] ;
     else ;
       Ch := '['+IntToStr(ASC)+']'
     end;

     Buf := Buf + Ch ;
  end ;

  Result := Buf;
end;

{-----------------------------------------------------------------------------
  Tenta carregar a Biblioteca (DLL) <LibName> e veirica se a função <FuncName>
  existe na DLL. Se existir, retorna ponteiro para a DLL em <LibPointer>
  Veja Exempo de uso em InPort e OutPort (logo acima)
  ( Função encontrada na Internet - Autor desconhecido )
 -----------------------------------------------------------------------------}
function FunctionDetect (LibName, FuncName: String; var LibPointer: Pointer): boolean;
Var
  LibHandle: tHandle;
begin
 Result := FunctionDetect(LibName, FuncName, LibPointer, LibHandle);
end;

function FunctionDetect (LibName, FuncName: String; var LibPointer: Pointer;
   var LibHandle: THandle ): boolean;
begin
 Result := false;
 LibPointer := NIL;
 {$IFDEF FPC}
  LibHandle := dynlibs.LoadLibrary(LibName) ;
 {$ELSE}
  if LoadLibrary(PChar(LibName)) = 0 then
     exit;                                 { não consegiu ler a DLL }

  LibHandle := GetModuleHandle(PChar(LibName));  { Pega o handle da DLL }
 {$ENDIF}

 if LibHandle <> 0 then                    { Se 0 não pegou o Handle, falhou }
  begin
     LibPointer := GetProcAddress(LibHandle, PChar(FuncName));{Procura a função}
     if LibPointer <> NIL then
        Result := true;
  end;
end;

function UnLoadLibrary(LibName: String ): Boolean ;
var
  LibHandle: THandle ;
begin
 Result := True ;

{$IFDEF FPC}
 LibHandle := dynlibs.LoadLibrary( LibName ) ;
 if LibHandle <> 0 then
    Result := dynlibs.FreeLibrary(LibHandle) ;
{$ELSE}
{$IFDEF DELPHI12_UP}
 LibHandle := GetModuleHandle( PWideChar( String( LibName ) ) );
 {$ELSE}
 LibHandle := GetModuleHandle( PChar( LibName ) );
 {$ENDIF}
 if LibHandle <> 0 then
    Result := FreeLibrary( LibHandle )
{$ENDIF}
end ;


//funcoes para uso com o modulo ACBrSintegra ***********************************************

function TBStrZero(const i: string; const Casas: byte): string;
var
  Ch: Char;
begin
  Result := I;

  if length(i)>Casas then
    Exit
  else
    Ch := '0';

  while Length(Result) < Casas do
    Result := Ch + Result;
end;

function TiraPontos(Str: string): string;
var
  i: Integer;
  xStr : String;
begin
 xStr := '';
 for i:=1 to Length(Trim(str)) do
   if (Pos(Copy(str,i,1),'/-.)(,')=0) then xStr := xStr + str[i];

 xStr := StringReplace(xStr,' ','',[rfReplaceAll]);

 Result:=xStr;
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

{------------------------------------------------------------------------------
 Calcula e Retorna o Digito verificador do EAN-13 de acordo com 12 primeiros
  caracteres de <CodEAN13>
 ------------------------------------------------------------------------------}
function EAN13_DV(CodEAN13: String): String;
Var A,DV : Integer ;
begin
   Result   := '' ;
   CodEAN13 := String( PadLeft(AnsiString(Trim(String(CodEAN13))),12,'0') ) ;
   if not StrIsNumber( CodEAN13 ) then
      exit ;

   DV := 0;
   For A := 12 downto 1 do
      DV := DV + (StrToInt( CodEAN13[A] ) * IfThen(odd(A),1,3));

   DV := (Ceil( DV / 10 ) * 10) - DV ;

   Result := IntToStr( DV );
end;

{------------------------------------------------------------------------------
 Retorna True se o <CodEAN13> informado for válido
 ------------------------------------------------------------------------------}
function EAN13Valido(CodEAN13: String): Boolean;
begin
  Result := false ;
  if Length(CodEAN13) = 13 then
     Result := ( CodEAN13[13] =  EAN13_DV(CodEAN13) ) ;
end;

{------------------------------------------------------------------------------
  Traduz uma String de uma página de código para outra
http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_10147769.html
 ------------------------------------------------------------------------------}
function TranslateString(const S: AnsiString; CP_Destino: Word; CP_Atual: Word = 0): AnsiString;
{$IFDEF USE_LConvEncoding}
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

function MatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean;
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

{$IFDEF FPC}
function UnZip(S: TStream): AnsiString;
{ Descompacta um arquivo padrão GZIP de Stream... Fontes:
  http://wiki.freepascal.org/paszlib
  http://www.gocher.me/GZIP
}
var
  DS: TDecompressionStream;
  MS: TMemoryStream;
  readCount: integer;
  Buf: array[0..1023] of byte;
  hdr: longword;
begin
  S.Position := 0; // goto start of input stream
  hdr := S.ReadDWord;
  if (hdr and $00088B1F) = $00088B1F then // gzip header (deflate method)
    S.Position := 10     // Pula cabeçalho gzip
  else if (hdr and $00009C78) = $00009C78 then // zlib header
    S.Position := 2      // Pula cabeçalho zlib
  else
    S.Position := 0;

  MS := TMemoryStream.Create;
  DS := Tdecompressionstream.Create(S, (S.Position > 0) );
  Buf[0] := 0;
  try
    repeat
      readCount := DS.Read(Buf, SizeOf(Buf));
      if readCount <> 0 then
        MS.Write(Buf, readCount);
    until readCount < SizeOf(Buf);

    MS.Position := 0;
    Result := '';
    SetLength(Result, MS.Size);
    MS.ReadBuffer(Result[1], MS.Size);
  finally
    DS.Free;
    MS.Free;
  end;
end;

function UnZip(S: AnsiString): AnsiString; overload;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(S);
  try
    Result := UnZip(SS);
  finally
    SS.Free;
  end;
end;

{$ELSE}

function UnZip(S: TStream): AnsiString;
Var
  DataStr: AnsiString;
begin
  DataStr := '';
  S.Position := 0;
  SetLength(DataStr, S.Size);
  S.ReadBuffer(DataStr[1], S.Size);
  
  Result := UnZip(S);
end;

function UnZip(S: AnsiString): AnsiString; overload;
begin
  Result := GZDecompressStr(S);
end ;
{$ENDIF}

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
  Result := '';

  {$IFDEF UNICODE}
   if not StrIsUTF8 then
     Result := ACBrStr( ABinaryString )
   else
     {$IFNDEF FPC}
      {$IFDEF DELPHI12_UP}  // delphi 2009 em diante
       Result := UTF8ToString(ABinaryString);
      {$ELSE}
       Result := UTF8Decode(ABinaryString);
      {$ENDIF}
     {$ENDIF} ;

  {$ELSE}
   if StrIsUTF8 then
     Result := Utf8ToAnsi( ABinaryString ) ;
  {$ENDIF}

  if Result = '' then
    Result := String(ABinaryString);
end;

function SeparaDados(const AString: String; const Chave: String;
  const MantemChave: Boolean): String;
var
  PosIni, PosFim : Integer;
  UTexto, UChave :String;
begin
  UTexto := AnsiUpperCase(AString);
  UChave := AnsiUpperCase(Chave);
  PosIni := 0;
  PosFim := 0;

  if MantemChave then
   begin
     PosIni := Pos('<' + UChave, UTexto);
     if PosIni > 0 then
       PosFim := Pos('/' + UChave, UTexto) + length(UChave) + 3;

     if (PosFim = 0) then
      begin
        PosIni := Pos('NS2:' + UChave, UTexto) - 1;
        if PosIni > 0 then
          PosFim := Pos('/NS2:' + UChave, UTexto) + length(UChave) + 3;
      end;
   end
  else
   begin
     PosIni := Pos('<' + UChave, UTexto) ;
     if PosIni > 0 then
     begin
       PosIni := PosIni + Pos('>', copy(UTexto, PosIni, length(UTexto)));
       PosFim := Pos('/' + UChave, UTexto);
     end;

     if (PosFim = 0) then
      begin
        PosIni := Pos('NS2:' + UChave, UTexto) ;
        if PosIni > 0 then
        begin
          PosIni := PosIni + Pos('>', copy(UTexto, PosIni, length(UTexto)));
          PosFim := Pos('/NS2:' + UChave, UTexto);
        end ;
      end;
   end;

  Result := copy(AString, PosIni, PosFim - (PosIni + 1));
end;

{------------------------------------------------------------------------------
   Realiza o tratamento de uma String recebida de um Serviço Web
   Transforma caracteres HTML Entity em ASCII ou vice versa
 ------------------------------------------------------------------------------}

function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
   const IsUTF8: Boolean = True ) : String;
var
  AStr: String;

  function InternalStringReplace(const S, OldPatern: String; NewPattern: AnsiString ): String;
  begin
    if pos(OldPatern, S) > 0 then
      Result := StringReplace(S, OldPatern, ACBrStr(NewPattern), [rfReplaceAll])
    else
      Result := S;
  end;

begin
  Astr := DecodeToString( Texto, IsUTF8 ) ;

  if Decode then
  begin
    Astr := InternalStringReplace(AStr, '&amp;'   , '&');
    AStr := InternalStringReplace(AStr, '&lt;'    , '<');
    AStr := InternalStringReplace(AStr, '&gt;'    , '>');
    AStr := InternalStringReplace(AStr, '&quot;'  , '"');
    AStr := InternalStringReplace(AStr, '&#39;'   , #39);
    AStr := InternalStringReplace(AStr, '&aacute;', 'á');
    AStr := InternalStringReplace(AStr, '&Aacute;', 'Á');
    AStr := InternalStringReplace(AStr, '&acirc;' , 'â');
    AStr := InternalStringReplace(AStr, '&Acirc;' , 'Â');
    AStr := InternalStringReplace(AStr, '&atilde;', 'ã');
    AStr := InternalStringReplace(AStr, '&Atilde;', 'Ã');
    AStr := InternalStringReplace(AStr, '&agrave;', 'à');
    AStr := InternalStringReplace(AStr, '&Agrave;', 'À');
    AStr := InternalStringReplace(AStr, '&eacute;', 'é');
    AStr := InternalStringReplace(AStr, '&Eacute;', 'É');
    AStr := InternalStringReplace(AStr, '&ecirc;' , 'ê');
    AStr := InternalStringReplace(AStr, '&Ecirc;' , 'Ê');
    AStr := InternalStringReplace(AStr, '&iacute;', 'í');
    AStr := InternalStringReplace(AStr, '&Iacute;', 'Í');
    AStr := InternalStringReplace(AStr, '&oacute;', 'ó');
    AStr := InternalStringReplace(AStr, '&Oacute;', 'Ó');
    AStr := InternalStringReplace(AStr, '&otilde;', 'õ');
    AStr := InternalStringReplace(AStr, '&Otilde;', 'Õ');
    AStr := InternalStringReplace(AStr, '&ocirc;' , 'ô');
    AStr := InternalStringReplace(AStr, '&Ocirc;' , 'Ô');
    AStr := InternalStringReplace(AStr, '&uacute;', 'ú');
    AStr := InternalStringReplace(AStr, '&Uacute;', 'Ú');
    AStr := InternalStringReplace(AStr, '&uuml;'  , 'ü');
    AStr := InternalStringReplace(AStr, '&Uuml;'  , 'Ü');
    AStr := InternalStringReplace(AStr, '&ccedil;', 'ç');
    AStr := InternalStringReplace(AStr, '&Ccedil;', 'Ç');
    AStr := InternalStringReplace(AStr, '&apos;'  , '''');
  end
  else
  begin
    AStr := StringReplace(AStr, '&', '&amp;' , [rfReplaceAll]);
    AStr := StringReplace(AStr, '<', '&lt;'  , [rfReplaceAll]);
    AStr := StringReplace(AStr, '>', '&gt;'  , [rfReplaceAll]);
    AStr := StringReplace(AStr, '"', '&quot;', [rfReplaceAll]);
    AStr := StringReplace(AStr, #39, '&#39;' , [rfReplaceAll]);
  end;

  Result := AStr;
end;

function LerTagXML(const AXML, ATag: String; IgnoreCase: Boolean): String;
Var
  PI, PF : Integer ;
  UXML, UTAG: String;
begin
  Result := '';
  if IgnoreCase then
  begin
    UXML := UpperCase(AXML) ;
    UTAG := UpperCase(ATag) ;
  end
  else
  begin
    UXML := AXML ;
    UTAG := ATag ;
  end;

  PI := pos('<'+UTAG+'>', UXML ) ;
  if PI = 0 then exit ;

  PI := PI + Length(UTAG) + 2;
  PF := PosEx('</'+UTAG+'>', UXML, PI) ;
  if PF = 0 then
     PF := Length(AXML);

  Result := copy(AXML, PI, PF-PI)
end ;

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
        until not ((P^ in [#1..' ']));
      end;
    end;
  finally
    ALista.EndUpdate;
  end;
end;

//*****************************************************************************************


initialization
{$IFDEF MSWINDOWS}
  if not FunctionDetect(CINPOUTDLL,'Inp32',@xInp32) then
    xInp32 := NIL ;

  if not FunctionDetect(CINPOUTDLL,'Out32',@xOut32) then
    xOut32 := NIL ;

  if not FunctionDetect('USER32.DLL', 'BlockInput', @xBlockInput) then
  	 xBlockInput := NIL ;
{$ENDIF}

  Randomized := False ;
end.

