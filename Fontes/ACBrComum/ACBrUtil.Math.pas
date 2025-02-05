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

unit ACBrUtil.Math;

interface

Uses
  SysUtils, Math, Classes,
  ACBrBase, ACBrConsts, IniFiles,
  {$IfDef COMPILER6_UP} StrUtils, DateUtils {$Else} ACBrD5, FileCtrl {$EndIf}
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

function SimpleRoundToEX(const AValue: Extended; const ADigit: TRoundToRange = -2): Extended;
function TruncFix( X : Extended ) : Int64 ;
function RoundABNT(const AValue: Double; const Digits: TRoundToRange;
  const Delta: Double = 0.00001 ): Double;
function TruncTo(const AValue: Double; const Digits: TRoundToRange): Double;
function ComparaValor(const ValorUm, ValorDois : Double; const Tolerancia : Double = 0 ): Integer;

function TestBit(const AValue: Integer; const AIndex: Byte): Boolean;
procedure ClearBit(var AValue: Integer; const AIndex: Byte);
procedure SetBit(var AValue: Integer; const AIndex: Byte);
procedure PutBit(var AValue: Integer; const AIndex: Byte; State: Boolean);

function IntToBin (value: LongInt; digits: integer ): string;
function BinToInt(Value: String): LongInt;

Function BcdToAsc( const StrBCD : AnsiString) : String ;
Function AscToBcd( const ANumStr: String ; const TamanhoBCD : Byte) : AnsiString ;

function IntToLEStr(AInteger: Integer; BytesStr: Integer = 2): AnsiString;
function LEStrToInt(const ALEStr: AnsiString): Integer;
function IntToBEStr(AInteger: Integer; BytesStr: Integer = 2): AnsiString;
function BEStrToInt(const ABEStr: AnsiString): Integer;

Function HexToAscii(const HexStr : String) : AnsiString ;
Function AsciiToHex(const ABinaryString: AnsiString): String;
function TryHexToAscii(const HextStr: String; out Value: AnsiString): Boolean;
function HexToAsciiDef(const HexStr: String; const Default: AnsiString): AnsiString;

function BinaryStringToString(const AString: AnsiString): String;
function StringToBinaryString(const AString: String): AnsiString;

implementation

uses
  synautil, ACBrUtil.Strings;

{-----------------------------------------------------------------------------
 Faz o mesmo que "SimpleRoundTo", porém divide pelo Fator, ao invés de Multiplicar.
 Isso evita Erro A.V. de estouro de Inteiro.
 Nota: Funcao copiada de SimpleRoundTo do Delphi Seatle
 -----------------------------------------------------------------------------}
function SimpleRoundToEX(const AValue: Extended; const ADigit: TRoundToRange = -2): Extended;
var
  LFactor: Extended;
begin
  LFactor := IntPower(10.0, ADigit);
  if AValue < 0 then
    Result := Int((AValue / LFactor) - 0.5) * LFactor
  else
    Result := Int((AValue / LFactor) + 0.5) * LFactor;
end;

{-----------------------------------------------------------------------------
 Corrige, bug da função Trunc.
 Deve calcular Trunc somente com variaveis e nunca com Expressoes, caso contrá-
 rio o resultado pode não ser o esperado.
 // Valores de Teste: Trunc(1,602 x 0,98) | 5 * 12,991 | 2,09 * 23,5
 -----------------------------------------------------------------------------}
function TruncFix( X : Extended ) : Int64 ;
begin
  Result := Trunc( SimpleRoundToEX( X, -9) ) ;
end ;

{-----------------------------------------------------------------------------
 Arredondamento segundo as normas da ABNT NBR 5891/77  (por: DSA)
 Fontes:
 http://www.sofazquemsabe.com/2011/01/como-fazer-arredondamento-da-numeracao.html
 http://partners.bematech.com.br/2011/12/edicao-98-entendendo-o-truncamento-e-arredondamento-no-ecf/
 -----------------------------------------------------------------------------}
function RoundABNT(const AValue: Double; const Digits: TRoundToRange;
  const Delta: Double): Double;
var
   Pow, FracValue, PowValue : Extended;
   RestPart: Double;
   IntCalc, FracCalc, LastNumber, IntValue : Int64;
   Negativo: Boolean;
{$IFNDEF EXTERNALLINKER}
   OldRM: TFPURoundingMode;
{$ELSE}
   OldRM: TRoundingMode;
{$ENDIF}
begin
  OldRM := GetRoundMode;
  try
    if (OldRM <> rmNearest) then
      SetRoundMode(rmNearest);

    Negativo  := (AValue < 0);
    Pow       := intpower(10, abs(Digits) );
    PowValue  := abs(AValue) / 10 ;
    IntValue  := trunc(PowValue);
    FracValue := frac(PowValue);

    PowValue := SimpleRoundToEX( FracValue * 10 * Pow, -9) ; // SimpleRoundTo elimina dizimas ;
    IntCalc  := trunc( PowValue );
    FracCalc := trunc( frac( PowValue ) * 100 );

    if (FracCalc > 50) then
     Inc( IntCalc )

    else if (FracCalc = 50) then
    begin
     LastNumber := round( frac( IntCalc / 10) * 10);

     if odd(LastNumber) then
       Inc( IntCalc )
     else
     begin
       RestPart := frac( PowValue * 10 ) ;

       if RestPart > Delta then
         Inc( IntCalc );
     end ;
    end ;

    Result := ((IntValue*10) + (IntCalc / Pow));
    if Negativo then
     Result := -Result;
  finally
    SetRoundMode(OldRM);
  end;
end;

function TruncTo(const AValue: Double; const Digits: TRoundToRange): Double;
var
 VFrac : Double;
 Pow: Extended;
begin
  Result := AValue;
  VFrac  := Frac(Result);

  if VFrac <> 0 then
  begin
    Pow    := intpower(10, abs(Digits) );
    VFrac  := TruncFix(VFrac * Pow);
    VFrac  := VFrac / Pow;
    Result := Int(Result) + VFrac  ;
  end;
end;

{-----------------------------------------------------------------------------
Compara valores levando em conta uma Tolerancia que pode ser aplicada
tanto para positivo quando negativo.
Retorna -1 se ValorUm for menor; 1 Se ValorUm for maior; 0 - Se forem iguais
Inspirada em "CompareValue" do FPC, math
------------------------------------------------------------------------------}
function ComparaValor(const ValorUm, ValorDois: Double;
  const Tolerancia: Double): Integer;
var
  diff: Extended;
begin
 Result := 1;

 diff := SimpleRoundTo( abs(ValorUm - ValorDois), -9);
 if diff <= Tolerancia then
   Result := 0
  else
    if ValorUm < ValorDois then
      Result := -1;
end;

{-----------------------------------------------------------------------------
 http://wiki.freepascal.org/Bit_manipulation
 Retorna True se o Bit em Index está ativo (ligado) dentro do valor Value. Inicia em 0
 ---------------------------------------------------------------------------- }
function TestBit(const AValue: Integer; const AIndex: Byte): Boolean;
begin
  Result := ((AValue shr AIndex) and 1) = 1;
end;

{-----------------------------------------------------------------------------
 http://wiki.freepascal.org/Bit_manipulation
 Desliga um Bit especificado em  "Index", em "Value" (passado por referencia)
 ---------------------------------------------------------------------------- }
procedure ClearBit(var AValue: Integer; const AIndex: Byte);
begin
  AValue := AValue and ((Integer(1) shl AIndex) xor High(Integer));
end;

{-----------------------------------------------------------------------------
 http://wiki.freepascal.org/Bit_manipulation
 Liga um Bit especificado em  "Index", em "Value" (passado por referencia)
 ---------------------------------------------------------------------------- }
procedure SetBit(var AValue: Integer; const AIndex: Byte);
begin
  AValue:=  AValue or (Integer(1) shl AIndex);
end;

{-----------------------------------------------------------------------------
 http://wiki.freepascal.org/Bit_manipulation
 Liga ou Desliga um Bit especificado em  "Index", em "Value", (passado por
 referencia), e de acordo com o Boleano "State".
 ---------------------------------------------------------------------------- }
procedure PutBit(var AValue: Integer; const AIndex: Byte; State: Boolean);
begin
  AValue := (AValue and ((Integer(1) shl AIndex) xor High(Integer))) or (Integer(State) shl AIndex);
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
  StrBCD := PadLeft( ANumStr, L , '0' );
  For I := 1 to TamanhoBCD do
  begin
     BCDChar := copy(StrBCD, (I*2)-1, 2);
     DecVal := StrToInt( '$'+BCDChar );
     Result := Result + AnsiChr( DecVal )  ;
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
    Result := AnsiChr( DecVal ) + Result;
    P := P + 2 ;
  end ;
end;

{-----------------------------------------------------------------------------
  converte uma String binária codificada como Little Endian em Inteiro
  Veja exemplos na function acima
 ---------------------------------------------------------------------------- }
function LEStrToInt(const ALEStr: AnsiString): Integer;
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
  Converte um "AInteger" em uma String binária codificada como Big Endian,
  no tamanho máximo de "BytesStr"
  Exemplos: IntToBEStr( 106, 2 ) = chr(0) + chr(106)
 ---------------------------------------------------------------------------- }
function IntToBEStr(AInteger: Integer; BytesStr: Integer): AnsiString;
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
    Result := Result + AnsiChar( DecVal );
    P := P + 2 ;
  end ;
end;

{-----------------------------------------------------------------------------
  converte uma String binária codificada como Big Endian em Inteiro
  Veja exemplos na function acima
 ---------------------------------------------------------------------------- }
function BEStrToInt(const ABEStr: AnsiString): Integer;
var
   AHexStr: String;
   LenBE, P : Integer ;
begin
  LenBE   := Length(ABEStr);
  AHexStr := '';

  P := 1;
  while P <= LenBE do
  begin
    AHexStr := AHexStr + IntToHex(ord(ABEStr[P]),2);
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
  Cmd : String ;
  I, L: Integer ;
begin
  Result := '' ;
  Cmd    := Trim(HexStr);
  I      := 1 ;
  L      := Length(Cmd) ;

  while I < L do
  begin
     B := StrToInt('$' + copy(Cmd, I, 2)) ;
     Result := Result + AnsiChr( B ) ;
     Inc( I, 2) ;
  end ;
end ;

function TryHexToAscii(const HextStr: String; out Value: AnsiString): Boolean;
begin
  try
    Value := HexToAscii(HextStr);
    Result := True;
  except
    Result := False;
  end;
end;

function HexToAsciiDef(const HexStr: String; const Default: AnsiString): AnsiString;
begin
  if not TryHexToAscii(HexStr, Result) then
    Result := Default;
end;

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
 Substitui todos os caracteres de Controle ( menor que ASCII 32 ou maior que
 ASCII 127), de <AString> por sua representação em HEXA. (\xNN)
 Use StringToBinaryString para Converter para o valor original.
 ---------------------------------------------------------------------------- }
function BinaryStringToString(const AString: AnsiString): String;
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
        Result := Result + '\x'+Trim(IntToHex(ASC,2))
     else
        Result := Result + Char(AString[I]) ;
  end ;
end ;

{-----------------------------------------------------------------------------
 Substitui toda representação em HEXA de <AString> (Iniciada por \xNN, (onde NN,
 é o valor em Hexa)).
 Retornana o Estado original, AString de BinaryStringToString.
 ---------------------------------------------------------------------------- }
function StringToBinaryString(const AString: String): AnsiString;
var
   P, I : LongInt;
   Hex : String;
   CharHex : AnsiString;
begin
  Result := AnsiString(AString);

  P := pos('\x',String(Result)) ;
  while P > 0 do
  begin
     Hex := copy(String(Result),P+2,2) ;

     if (Length(Hex) = 2) and StrIsHexa(Hex) then
     begin
       try
          CharHex := AnsiChr(StrToInt('$'+Hex));
       except
          CharHex := ' ' ;
       end ;

       Result := ReplaceString(Result, AnsiString('\x'+Hex), CharHex );
       I := 1;
     end
     else
       I := 4;

     P := PosEx('\x', String(Result), P + I) ;
  end ;
end ;


end.
