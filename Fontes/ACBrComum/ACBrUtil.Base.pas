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

unit ACBrUtil.Base;

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
  {$EndIf}
;
//  , ACBrUtil.Compatibilidade, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.Math, ACBrUtil.DateTime, ACBrUtil.FilesIO;

type
  TFormatMask = (msk4x2, msk7x2, msk9x2, msk10x2, msk13x2, msk15x2, msk6x3, msk6x4, mskAliq);


function PosExA(const SubStr, S: AnsiString; Offset: Integer = 1): Integer;

Function IntToStrZero(const NumInteiro : Int64; Tamanho : Integer) : String;
function FloatToIntStr(const AValue: Double; const DecimalDigits: SmallInt = 2): String;
function FloatToString(const AValue: Double; SeparadorDecimal: Char = '.';
  const AFormat: String = ''): String;
function FormatFloatBr(const AValue: Extended; AFormat: String = ''): String; overload;
function FormatFloatBr(const AFormat: TFormatMask; const AValue: Extended): String; overload;
function FloatMask(const DecimalDigits: SmallInt = 2; UseThousandSeparator: Boolean = True): String;
function StringDecimalToFloat(const AValue: String; const DecimalDigits: SmallInt = 2): Double;
Function StringToFloat(NumString : String): Double;
Function StringToFloatDef( const NumString : String ;
   const DefaultValue : Double ) : Double ;

function EstaVazio(const AValue: String): Boolean;overload;
procedure EstaVazio(const AValue, AMensagem: String);overload;
function NaoEstaVazio(const AValue: String): Boolean;
function EstaZerado(const AValue: Double): Boolean;overload;
function EstaZerado(const AValue: Integer): Boolean;overload;
procedure EstaZerado(const AValue: Integer; const AMensagem: String);overload;
function NaoEstaZerado(const AValue: Double): Boolean;overload;
function NaoEstaZerado(const AValue: Integer): Boolean;overload;
function TamanhoIgual(const AValue: String; const ATamanho: Integer): Boolean;overload;
procedure TamanhoIgual(const AValue: String; const ATamanho: Integer; const AMensagem: String);overload;
function TamanhoIgual(const AValue: Integer; const ATamanho: Integer): Boolean;overload;
procedure TamanhoIgual(const AValue: Integer; const ATamanho: Integer; const AMensagem: String);overload;
function TamanhoMenor(const AValue: String; const ATamanho: Integer): Boolean;

function TraduzComando( const AString : String ) : AnsiString ;
Function StringToAsc( const AString : AnsiString ) : String ;
Function AscToString( const AString : String ) : AnsiString ;

function EAN13Valido( const CodEAN13 : String ) : Boolean ;
function EAN13_DV( CodEAN13 : String ) : String ;

procedure RttiSetProp(AObject: TObject; AProp: String; AValue: String);

function CodigoUFparaUF(const codigo: integer): string;
function UFparaCodigoUF(const UF: string): integer;

implementation

uses
  synautil, StrUtilsEx, typinfo,
  ACBrCompress, ACBrUtil.Compatibilidade, ACBrUtil.Strings;

function PosExA(const SubStr, S: AnsiString; Offset: Integer): Integer;
begin
  {$IFDEF DELPHIXE3_UP}
   Result := Pos(SubStr, S, Offset);
  {$Else}
   Result := PosEx(SubStr, S, Offset);
  {$EndIf}
end;

{-----------------------------------------------------------------------------
  Transforma <NumInteiro> em String, preenchendo com Zeros a Esquerda até
  atingiros digitos de <Tamnho>
 ---------------------------------------------------------------------------- }
function IntToStrZero(const NumInteiro : Int64 ; Tamanho : Integer) : String ;
begin
  Result := ACBrUtil.Strings.Poem_Zeros( NumInteiro, Tamanho) ;
end ;

{-----------------------------------------------------------------------------
  Converte uma <NumString> para Double, semelhante ao StrToFloatDef, mas
  verifica se a virgula é '.' ou ',' efetuando a conversão se necessário
  Se não for possivel converter, retorna <DefaultValue>
 ---------------------------------------------------------------------------- }
function StringToFloatDef(const NumString : String ; const DefaultValue : Double
   ) : Double ;
begin
  if EstaVazio(NumString) then
     Result := DefaultValue
  else
   begin
     try
        Result := StringToFloat( NumString ) ;
     except
        Result := DefaultValue ;
     end ;
   end;
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
     AFormat := FloatMask();

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

function FloatMask(const DecimalDigits: SmallInt; UseThousandSeparator: Boolean
  ): String;
begin
  if DecimalDigits > 0 then
  begin
    if UseThousandSeparator then
      Result := ','
    else
      Result := '';

    Result := Result + '0.' + StringOfChar('0',DecimalDigits)
  end
  else
    Result := '0';
end;

{-----------------------------------------------------------------------------
  Converte uma <NumString> para Double, semelhante ao StrToFloat, mas
  verifica se a virgula é '.' ou ',' efetuando a conversão se necessário
  Se não for possivel converter, dispara Exception
 ---------------------------------------------------------------------------- }
function StringToFloat(NumString: String): Double;
var
  DS: Char;
begin
  NumString := Trim(NumString);

  DS := {$IFDEF HAS_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;

  if DS <> '.' then
    NumString := StringReplace(NumString, '.', DS, [rfReplaceAll]);

  if DS <> ',' then
    NumString := StringReplace(NumString, ',', DS, [rfReplaceAll]);

  while ACBrUtil.Strings.CountStr(NumString, DS) > 1 do
    NumString := StringReplace(NumString, DS, '', []);

  Result := StrToFloat(NumString);
end;

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
  Converte um String, SEM separador decimal, para Double, considerando a
  parte final da String como as decimais. Ex: 10000 = "100,00"; 123 = "1,23"
 ---------------------------------------------------------------------------- }
function StringDecimalToFloat(const AValue: String; const DecimalDigits: SmallInt): Double;
var
  iTam: Integer;
  sValue: String;
begin
  sValue := AValue;
  iTam   := LengthNativeString(sValue);
  if (iTam < DecimalDigits) then
    sValue := StringOfChar('0', (DecimalDigits - iTam)) + sValue;

  sValue := ReverseString(sValue);
  Insert({$IFDEF HAS_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, sValue, DecimalDigits+1);
  Result := StrToFloat(ReverseString(sValue));
end;

{-----------------------------------------------------------------------------
  Converte um Double para string, semelhante a FloatToStr(), porém
  garante que não haverá separador de Milhar e o Separador Decimal será igual a
  "SeparadorDecimal" ( o default é .(ponto))
 ---------------------------------------------------------------------------- }
function FloatToString(const AValue: Double; SeparadorDecimal: Char;
  const AFormat: String): String;
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
  if ( DS <> TS ) then
    Result := StringReplace(Result, TS, '', [rfReplaceAll]);

  // Verificando se precisa mudar Separador decimal //
  if DS <> SeparadorDecimal then
    Result := StringReplace(Result, DS, SeparadorDecimal, [rfReplaceAll]);
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

function NaoEstaVazio(const AValue: String): Boolean;
begin
  Result := not EstaVazio(AValue);
end;

function EstaZerado(const AValue: Double): Boolean;
begin
  Result := (AValue = 0);
end;

function EstaZerado(const AValue: Integer): Boolean;
begin
  Result := (AValue = 0);
end;

procedure EstaZerado(const AValue: Integer; const AMensagem: String);
begin
  if EstaZerado(AValue) then
    raise Exception.Create(AMensagem);
end;

function NaoEstaZerado(const AValue: Double): Boolean;
begin
  Result := not EstaZerado(AValue);
end;

function NaoEstaZerado(const AValue: Integer): Boolean;
begin
  Result := not EstaZerado(AValue);
end;

function TamanhoIgual(const AValue: String; const ATamanho: Integer): Boolean;
begin
 Result := (Length(AValue) = ATamanho);
end;

procedure TamanhoIgual(const AValue: String; const ATamanho: Integer;
  const AMensagem: String);
begin
  if not TamanhoIgual(AValue, ATamanho) then
    raise Exception.Create(AMensagem);
end;

function TamanhoIgual(const AValue: Integer; const ATamanho: Integer): Boolean;
begin
  Result := (Length(IntToStr(AValue)) = ATamanho);
end;

procedure TamanhoIgual(const AValue: Integer; const ATamanho: Integer;
  const AMensagem: String);
begin
  if not TamanhoIgual(AValue, ATamanho) then
    raise Exception.Create(AMensagem);
end;

function TamanhoMenor(const AValue: String; const ATamanho: Integer): Boolean;
begin
  Result := (Length(AValue) < ATamanho);
end;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo '#13,v,#10', substituindo #nn por chr(nn). Ignora todo
   texto apos a String ' | '
 ---------------------------------------------------------------------------- }
function TraduzComando(const AString: String): AnsiString;
Var
  A : Integer ;
  VString : String;
begin
  VString := AString;
  A := pos(' | ', VString ) ;
  if A > 0 then
     VString := copy(VString,1,A-1) ;   { removendo texto apos ' | ' }

  Result := AscToString( VString ) ;
end ;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo chr(13)+chr(10) para uma representação que possa ser
   lida por AscToString Ex: '#13,#10'
 ---------------------------------------------------------------------------- }
function StringToAsc(const AString: AnsiString): String;
Var A : Integer ;
begin
  Result := '' ;
  For A := 1 to Length( AString ) do
     Result := Result + '#'+IntToStr( Ord( AString[A] ) )+',' ;

  Result := copy(Result,1, Length( Result )-1 ) ;
end;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo '#13,v,#10', substituindo #nn por chr(nn).
  Usar , para separar um campo do outro... No exemplo acima o resultado seria
  chr(13)+'v'+chr(10)
 ---------------------------------------------------------------------------- }
function AscToString(const AString: String): AnsiString;

   procedure ConverteToken(var AToken: AnsiString);
   var
     n: Integer;
   begin
     if AToken[1] = '#' then
     begin
       if TryStrToInt(copy(string(AToken), 2, Length(string(AToken))), n) then
         AToken := ACBrUtil.Strings.AnsiChr(n);
     end;
   end;

Var
  posicao, tamanhoString : Integer ;
  Token : AnsiString ;
  VString : string;
  C : Char ;
begin
  VString       := Trim( AString  );
  Result        := '' ;
  posicao       := 1  ;
  Token         := '' ;
  tamanhoString := Length( VString );

  while posicao <= tamanhoString + 1 do
  begin
    if posicao > tamanhoString then
      C := ','
    else
      C := VString[posicao] ;

    if (C = ',') and (Length( Token ) >= 1) then
    begin
      ConverteToken(Token);
      Result := Result + Token;
      Token  := '' ;
    end
    else
      Token := Token + AnsiString(C) ;

    posicao := posicao + 1 ;
  end ;
end;

{------------------------------------------------------------------------------
 Calcula e Retorna o Digito verificador do EAN-13 de acordo com 12 primeiros
  caracteres de <CodEAN13>
 ------------------------------------------------------------------------------}
function EAN13_DV(CodEAN13: String): String;
Var A,DV : Integer ;
begin
   Result   := '' ;
   CodEAN13 := ACBrUtil.Strings.PadLeft(Trim(CodEAN13),12,'0');
   if not ACBrUtil.Strings.StrIsNumber( CodEAN13 ) then
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
function EAN13Valido(const CodEAN13: String): Boolean;
begin
  Result := false ;
  if Length(CodEAN13) = 13 then
     Result := ( CodEAN13[13] =  EAN13_DV(CodEAN13) ) ;
end;

function FormatFloatBr(const AFormat: TFormatMask; const AValue: Extended): String; overload;
var
  Mask: String;
begin
  case AFormat of
    msk4x2  : Mask := '#,##0.00';
    msk7x2  : Mask := '#,###,##0.00';
    msk9x2  : Mask := '###,###,##0.00';
    msk10x2 : Mask := '#,###,###,##0.00';
    msk13x2 : Mask := '#,###,###,###,##0.00';
    msk15x2 : Mask := '###,###,###,###,##0.00';
    msk6x3  : Mask := ',0.000';
    msk6x4  : Mask := ',0.0000';
    mskAliq : Mask := '#00%';
  end;

  Result := FormatFloatBr(AValue, Mask);
  {$IfDef FPC}
  // Workround para BUG em FPC
  if (AValue > 999) and (pos(',', Mask) > 0) and (pos('.', Result) = 0) then
    Result := FormatFloatBr(AValue);
  {$EndIf}
end;

{------------------------------------------------------------------------------
   Inserir um valor a propriedade por RTTI
 ------------------------------------------------------------------------------}
procedure RttiSetProp(AObject: TObject; AProp, AValue: String);
var
  Propinfo: PPropInfo;
begin
  PropInfo := GetPropInfo(AObject.ClassInfo, AProp);
  if (PropInfo = nil) then
    Exit;
  SetPropValue(AObject, AProp, AValue);
end;

function CodigoUFparaUF(const codigo: integer): string;
const
  (**)UFS = '.AC.AL.AP.AM.BA.CE.DF.ES.GO.MA.MT.MS.MG.PA.PB.PR.PE.PI.RJ.RN.RS.RO.RR.SC.SP.SE.TO.DF.DF.';
  CODIGOS = '.12.27.16.13.29.23.53.32.52.21.51.50.31.15.25.41.26.22.33.24.43.11.14.42.35.28.17.90.91.';
begin
  try
    result := copy(UFS, pos('.' + IntToStr(Codigo) + '.', CODIGOS) + 1, 2);
  except
    result := '';
  end;
end;

function UFparaCodigoUF(const UF: string): integer;
const
  (**)UFS = '.AC.AL.AP.AM.BA.CE.DF.ES.GO.MA.MT.MS.MG.PA.PB.PR.PE.PI.RJ.RN.RS.RO.RR.SC.SP.SE.TO.';
  CODIGOS = '.12.27.16.13.29.23.53.32.52.21.51.50.31.15.25.41.26.22.33.24.43.11.14.42.35.28.17.';
begin
  try
    result := StrToInt(copy(CODIGOS, pos('.' + UF + '.', UFS) + 1, 2));
  except
    result := 0;
  end;
end;

initialization

end.

