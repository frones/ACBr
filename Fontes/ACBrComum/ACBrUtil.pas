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

unit ACBrUtil deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Utilize uma das units ACBrUtil no lugar dessa' {$ENDIF};

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
  , ACBrUtil.Compatibilidade, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.Math, ACBrUtil.DateTime,
    ACBrUtil.FilesIO;

type
  TFormatMask = ACBrUtil.Base.TFormatMask;
  TFindFileSortType = ACBrUtil.FilesIO.TFindFileSortType;
  TFindFileSortDirection = ACBrUtil.FilesIO.TFindFileSortDirection;

{$IFDEF FPC}
  {$DEFINE SUPPORTS_SCOPEDENUMS}
{$ENDIF}
{$IFDEF DELPHI2009_UP}
  {$DEFINE SUPPORTS_SCOPEDENUMS}
{$ENDIF}

const
  msk4x2  = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk4x2  deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  msk7x2  = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk7x2  deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  msk9x2  = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk9x2  deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  msk10x2 = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk10x2 deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  msk13x2 = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk13x2 deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  msk15x2 = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk15x2 deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  msk6x3  = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk6x3  deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  msk6x4  = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}msk6x4  deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};
  mskAliq = {$IFDEF SUPPORTS_SCOPEDENUMS}TFormatMask.{$ENDIF}mskAliq deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFormatMask da Unit ACBrUtil.Base.pas' {$ENDIF};

//deprecated const... Definidas na unit ACBrUtil.FilesIO
const
  fstNone     = {$IFDEF SUPPORTS_SCOPEDENUMS}TFindFileSortType.{$ENDIF}fstNone deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFindFileSortType da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
  fstDateTime = {$IFDEF SUPPORTS_SCOPEDENUMS}TFindFileSortType.{$ENDIF}fstDateTime deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFindFileSortType da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
  fstFileName = {$IFDEF SUPPORTS_SCOPEDENUMS}TFindFileSortType.{$ENDIF}fstFileName deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFindFileSortType da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
  fsdNone       = {$IFDEF SUPPORTS_SCOPEDENUMS}TFindFileSortDirection.{$ENDIF}fsdNone deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFindFileSortDirection da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
  fsdAscending  = {$IFDEF SUPPORTS_SCOPEDENUMS}TFindFileSortDirection.{$ENDIF}fsdAscending deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFindFileSortDirection da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
  fsdDescending = {$IFDEF SUPPORTS_SCOPEDENUMS}TFindFileSortDirection.{$ENDIF}fsdDescending deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o tipo TFindFileSortDirection da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
  //...deprecated



{/////////  ACBrUtil.Compatibilidade (especialmente D6/D5)}
{$IFNDEF COMPILER6_UP}
  type TRoundToRange = -37..37;
  function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
  function SimpleRoundTo(const AValue: Double; const ADigit: TRoundToRange = -2): Double; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};

  { IfThens retirada de Math.pas do D7, para compatibilizar com o Delphi 5
  (que nao possue essas funçao) }
  function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
  function IfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
  function IfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0.0): Double; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
  function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
{$ENDIF}

{$IFNDEF COMPILER7_UP}
{ PosEx, retirada de StrUtils.pas do D7, para compatibilizar com o Delphi 6
  (que nao possui essa funçao) }
function PosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
{$ENDIF}

{$IfNDef HAS_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
{$EndIf}

{$IFDEF HAS_FORMATSETTINGS}
function CreateFormatSettings: TFormatSettings; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Compatibilidade.pas' {$ENDIF};
{$ENDIF}

{/////////  ACBrUtil.Base}
function PosExA(const SubStr, S: AnsiString; Offset: Integer = 1): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};

Function IntToStrZero(const NumInteiro : Int64; Tamanho : Integer) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function FloatToIntStr(const AValue: Double; const DecimalDigits: SmallInt = 2): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function FloatToString(const AValue: Double; SeparadorDecimal: Char = '.';
  const AFormat: String = ''): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function FormatFloatBr(const AValue: Extended; AFormat: String = ''): String; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function FormatFloatBr(const AFormat: TFormatMask; const AValue: Extended): String; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function FloatMask(const DecimalDigits: SmallInt = 2; UseThousandSeparator: Boolean = True): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function StringDecimalToFloat(const AValue: String; const DecimalDigits: SmallInt = 2): Double; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
Function StringToFloat(NumString : String): Double; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
Function StringToFloatDef(const NumString : String; const DefaultValue : Double ) : Double; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};

function EstaVazio(const AValue: String): Boolean; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
procedure EstaVazio(const AValue, AMensagem: String); overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function NaoEstaVazio(const AValue: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function EstaZerado(const AValue: Double): Boolean;overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function EstaZerado(const AValue: Integer): Boolean;overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
procedure EstaZerado(const AValue: Integer; const AMensagem: String);overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function NaoEstaZerado(const AValue: Double): Boolean;overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function NaoEstaZerado(const AValue: Integer): Boolean;overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function TamanhoIgual(const AValue: String; const ATamanho: Integer): Boolean;overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
procedure TamanhoIgual(const AValue: String; const ATamanho: Integer; const AMensagem: String);overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function TamanhoIgual(const AValue: Integer; const ATamanho: Integer): Boolean;overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
procedure TamanhoIgual(const AValue: Integer; const ATamanho: Integer; const AMensagem: String);overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function TamanhoMenor(const AValue: String; const ATamanho: Integer): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};

function TraduzComando( const AString : String ) : AnsiString ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
Function StringToAsc( const AString : AnsiString ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
Function AscToString( const AString : String ) : AnsiString ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};

function EAN13Valido( const CodEAN13 : String ) : Boolean ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};
function EAN13_DV( CodEAN13 : String ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};

procedure RttiSetProp(AObject: TObject; AProp: String; AValue: String); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Base.pas' {$ENDIF};

{/////////  ACBrUtil.XMLHTML}
function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
   const IsUTF8: Boolean = True) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ParseText da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};

function LerTagXML( const AXML, ATag: String; IgnoreCase: Boolean = True) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método SeparaDados() da unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function XmlEhUTF8(const AXML: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método XmlEhUTF8 da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function ConverteXMLtoUTF8(const AXML: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ConverteXMLtoUTF8 da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function ConverteXMLtoNativeString(const AXML: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ConverteXMLtoNativeString da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function ObtemDeclaracaoXML(const AXML: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ObtemDeclaracaoXML da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function RemoverDeclaracaoXML(const AXML: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RemoverDeclaracaoXML da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function InserirDeclaracaoXMLSeNecessario(const AXML: String;
   const ADeclaracao: String = CUTF8DeclaracaoXML): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método InserirDeclaracaoXMLSeNecessario da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function SeparaDados(const AString: String; const Chave: String; const MantemChave : Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método SeparaDados da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function SeparaDadosArray(const AArray: Array of String; const AString: String; const MantemChave: Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método SeparaDadosArray da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
procedure EncontrarInicioFinalTag(const aText, ATag: String; var PosIni, PosFim: integer;const PosOffset: integer = 0); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método EncontrarInicioFinalTag da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
function StripHTML(const AHTMLString : String) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StripHTML da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};
procedure AcharProximaTag(const ABinaryString: AnsiString; const PosIni: Integer; var ATag: AnsiString; var PosTag: Integer); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método AcharProximaTag da Unit ACBrUtil.XMLHTML.pas' {$ENDIF};

{/////////  ACBrUtil.Strings}
type
  TSetOfChars = ACBrUtil.Strings.TSetOfChars;  //deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use a classe TSetOfChars da Unit ACBrUtil.Strings.pas' {$ENDIF};
  TSplitResult = ACBrUtil.Strings.TSplitResult; //deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use a classe TSplitResult da Unit ACBrUtil.Strings.pas' {$ENDIF};

function Split(const ADelimiter: Char; const AString: string): TSplitResult;  deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método Split da Unit ACBrUtil.Strings.pas' {$ENDIF};
function DecodeToString( const ABinaryString : AnsiString; const StrIsUTF8: Boolean ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método DecodeToString da Unit ACBrUtil.Strings.pas' {$ENDIF};
function RetornarConteudoEntre(const Frase, Inicio, Fim: String; IncluiInicioFim: Boolean = False): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RetornarConteudoEntre da Unit ACBrUtil.Strings.pas' {$ENDIF};

procedure QuebrarLinha(const Alinha: string; const ALista: TStringList;
  const QuoteChar: char = '"'; Delimiter: char = ';'); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método QuebrarLinha da Unit ACBrUtil.Strings.pas' {$ENDIF};

function ACBrStr( const AString : String ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ACBrStr da Unit ACBrUtil.Strings.pas' {$ENDIF};
function ACBrStrToAnsi( const AString : String ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ACBrStrToAnsi da Unit ACBrUtil.Strings.pas' {$ENDIF};

function NativeStringToUTF8(const AString : String ) : AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método NativeStringToUTF8 da Unit ACBrUtil.Strings.pas' {$ENDIF};
function UTF8ToNativeString(const AUTF8String : AnsiString ) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método UTF8ToNativeString da Unit ACBrUtil.Strings.pas' {$ENDIF};

function NativeStringToAnsi(const AString : String ) : AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método NativeStringToAnsi da Unit ACBrUtil.Strings.pas' {$ENDIF};
function AnsiToNativeString(const AAnsiString : AnsiString ) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método AnsiToNativeString da Unit ACBrUtil.Strings.pas' {$ENDIF};

{$IfDef FPC}
function GetSysANSIencoding: String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método GetSysANSIencoding da Unit ACBrUtil.Strings.pas' {$ENDIF};
{$EndIf}
function ACBrUTF8ToAnsi( const AUTF8String : AnsiString ) : AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ACBrUTF8ToAnsi da Unit ACBrUtil.Strings.pas' {$ENDIF};
function ACBrAnsiToUTF8( const AAnsiString : AnsiString ) : AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ACBrAnsiToUTF8 da Unit ACBrUtil.Strings.pas' {$ENDIF};

function TranslateString(const S: AnsiString; CP_Destino: Word; CP_Atual: Word = 0): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Strings.pas' {$ENDIF};

function AnsiChr( b: Byte) : AnsiChar; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método AnsiChr da Unit ACBrUtil.Strings.pas' {$ENDIF};

function PadRight(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PadRight da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PadRightA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PadRightA da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PadLeft(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PadLeft da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PadLeftA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PadLeftA da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PadCenter(const AString : String; const nLen : Integer;
   const Caracter : Char = ' ') : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PadCenter da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PadCenterA(const AAnsiString : AnsiString; const nLen : Integer;
   const Caracter : AnsiChar = ' ') : AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PadCenterA da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PadSpace(const AString : String; const nLen : Integer; Separador : String;
   const Caracter : Char = ' '; const RemoverEspacos: Boolean = True) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PadSpace da Unit ACBrUtil.Strings.pas' {$ENDIF};

function RemoveString(const sSubStr, sString: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RemoveString da Unit ACBrUtil.Strings.pas' {$ENDIF};
function RemoveStrings(const AText: AnsiString; StringsToRemove: array of AnsiString): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RemoveStrings da Unit ACBrUtil.Strings.pas' {$ENDIF};
function RemoverEspacosDuplos(const AString: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RemoverEspacosDuplos da Unit ACBrUtil.Strings.pas' {$ENDIF};
procedure RemoveEmptyLines( AStringList: TStringList); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RemoveEmptyLines da Unit ACBrUtil.Strings.pas' {$ENDIF};

function RandomName(const LenName : Integer = 8) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RandomName da Unit ACBrUtil.Strings.pas' {$ENDIF};

function IfEmptyThen( const AValue, DefaultValue: String; DoTrim: Boolean = True) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método IfEmptyThen da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PosAt(const SubStr, S: AnsiString; Ocorrencia : Cardinal = 1): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PosAt da Unit ACBrUtil.Strings.pas' {$ENDIF};
function RPos(const aSubStr, aString : AnsiString; const aStartPos: Integer): Integer; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RPos da Unit ACBrUtil.Strings.pas' {$ENDIF};
function RPos(const aSubStr, aString : AnsiString): Integer; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RPos da Unit ACBrUtil.Strings.pas' {$ENDIF};
function PosLast(const SubStr, S: String): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método PosLast da Unit ACBrUtil.Strings.pas' {$ENDIF};
function CountStr(const AString, SubStr : String ) : Integer ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método CountStr da Unit ACBrUtil.Strings.pas' {$ENDIF};
Function Poem_Zeros(const Texto : String; const Tamanho : Integer) : String; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método Poem_Zeros da Unit ACBrUtil.Strings.pas' {$ENDIF};
function Poem_Zeros(const NumInteiro : Int64 ; Tamanho : Integer) : String ; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método Poem_Zeros da Unit ACBrUtil.Strings.pas' {$ENDIF};
function RemoveZerosEsquerda(const ANumStr: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RemoveZerosEsquerda da Unit ACBrUtil.Strings.pas' {$ENDIF};

function StrIsAlpha(const S: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StrIsAlpha da Unit ACBrUtil.Strings.pas' {$ENDIF};
function StrIsAlphaNum(const S: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StrIsAlphaNum da Unit ACBrUtil.Strings.pas' {$ENDIF};
function StrIsNumber(const S: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StrIsNumber da Unit ACBrUtil.Strings.pas' {$ENDIF};
function StrIsHexa(const S: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StrIsHexa da Unit ACBrUtil.Strings.pas' {$ENDIF};
function StrIsBinary(const S: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StrIsBinary da Unit ACBrUtil.Strings.pas' {$ENDIF};
function StrIsBase64(const S: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StrIsBase64 da Unit ACBrUtil.Strings.pas' {$ENDIF};
function CharIsAlpha(const C: Char): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método CharIsAlpha da Unit ACBrUtil.Strings.pas' {$ENDIF};
function CharIsAlphaNum(const C: Char): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método CharIsAlphaNum da Unit ACBrUtil.Strings.pas' {$ENDIF};
function CharIsNum(const C: Char): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método CharIsNum da Unit ACBrUtil.Strings.pas' {$ENDIF};
function CharIsHexa(const C: Char): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método CharIsHexa da Unit ACBrUtil.Strings.pas' {$ENDIF};
function CharIsBinary(const C: Char): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método CharIsBinary da Unit ACBrUtil.Strings.pas' {$ENDIF};
function CharIsBase64(const C: Char): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método CharIsBase64 da Unit ACBrUtil.Strings.pas' {$ENDIF};
function OnlyNumber(const AValue: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método OnlyNumber da Unit ACBrUtil.Strings.pas' {$ENDIF};
function OnlyAlpha(const AValue: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método OnlyAlpha da Unit ACBrUtil.Strings.pas' {$ENDIF};
function OnlyAlphaNum(const AValue: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método OnlyAlphaNum da Unit ACBrUtil.Strings.pas' {$ENDIF};
function OnlyCharsInSet(const AValue: String; SetOfChars: TSetOfChars): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método OnlyCharsInSet da Unit ACBrUtil.Strings.pas' {$ENDIF};

function TiraAcentos( const AString : String ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método TiraAcentos da Unit ACBrUtil.Strings.pas' {$ENDIF};
function TiraAcento( const AChar : AnsiChar ) : AnsiChar ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método TiraAcento da Unit ACBrUtil.Strings.pas' {$ENDIF};

function AjustaLinhas(const Texto: AnsiString; Colunas: Integer ;
   NumMaxLinhas: Integer = 0; PadLinhas: Boolean = False): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método AjustaLinhas da Unit ACBrUtil.Strings.pas' {$ENDIF};
function QuebraLinhas(const Texto: String; const Colunas: Integer;
   const CaracterQuebrar : AnsiChar = ' '): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método QuebraLinhas da Unit ACBrUtil.Strings.pas' {$ENDIF};
function RemoverQuebraLinhaFinal(const ATexto: String; const AQuebraLinha: String = ''): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método RemoverQuebraLinhaFinal da Unit ACBrUtil.Strings.pas' {$ENDIF};

function TiraPontos(const Str: string): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método TiraPontos da Unit ACBrUtil.Strings.pas' {$ENDIF};
function TBStrZero(const i: string; const Casas: byte): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método TBStrZero da Unit ACBrUtil.Strings.pas' {$ENDIF};
function Space(Tamanho: Integer): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método Space da Unit ACBrUtil.Strings.pas' {$ENDIF};
function LinhaSimples(Tamanho: Integer): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método LinhaSimples da Unit ACBrUtil.Strings.pas' {$ENDIF};
function LinhaDupla(Tamanho: Integer): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método LinhaDupla da Unit ACBrUtil.Strings.pas' {$ENDIF};

function MatchText(const AText: String; const AValues: array of String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método MatchText da Unit ACBrUtil.Strings.pas' {$ENDIF};

function FindDelimiterInText( const AText: String; ADelimiters: String = ''): Char; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método FindDelimiterInText da Unit ACBrUtil.Strings.pas' {$ENDIF};
function AddDelimitedTextToList( const AText: String; const ADelimiter: Char;
   AStringList: TStrings; const AQuoteChar: Char = '"'): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método AddDelimitedTextToList da Unit ACBrUtil.Strings.pas' {$ENDIF};

function ChangeLineBreak(const AText: String; const NewLineBreak: String = ';'): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método ChangeLineBreak da Unit ACBrUtil.Strings.pas' {$ENDIF};


{/////////  ACBrUtil.Math}
function SimpleRoundToEX(const AValue: Extended; const ADigit: TRoundToRange = -2): Extended; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function TruncFix( X : Extended ) : Int64 ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function RoundABNT(const AValue: Double; const Digits: TRoundToRange;
  const Delta: Double = 0.00001 ): Double; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function TruncTo(const AValue: Double; const Digits: TRoundToRange): Double; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function ComparaValor(const ValorUm, ValorDois : Double; const Tolerancia : Double = 0 ): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};

function TestBit(const AValue: Integer; const AIndex: Byte): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
procedure ClearBit(var AValue: Integer; const AIndex: Byte); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
procedure SetBit(var AValue: Integer; const AIndex: Byte); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
procedure PutBit(var AValue: Integer; const AIndex: Byte; State: Boolean); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};

function IntToBin (value: LongInt; digits: integer ): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function BinToInt(Value: String): LongInt; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};

Function BcdToAsc( const StrBCD : AnsiString) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
Function AscToBcd( const ANumStr: String ; const TamanhoBCD : Byte) : AnsiString ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};

function IntToLEStr(AInteger: Integer; BytesStr: Integer = 2): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function LEStrToInt(const ALEStr: AnsiString): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function IntToBEStr(AInteger: Integer; BytesStr: Integer = 2): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function BEStrToInt(const ABEStr: AnsiString): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};

function HexToAscii(const HexStr : String) : AnsiString ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function AsciiToHex(const ABinaryString: AnsiString): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function TryHexToAscii(const HextStr: String; out Value: AnsiString): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function HexToAsciiDef(const HexStr: String; const Default: AnsiString): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};

function BinaryStringToString(const AString: AnsiString): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};
function StringToBinaryString(const AString: String): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.Math.pas' {$ENDIF};

{/////////  ACBrUtil.DateTime}
function FormatDateBr(const ADateTime: TDateTime; AFormat: String = ''): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
function FormatDateTimeBr(const ADate: TDateTime; AFormat: String = ''): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
Function StringToDateTime( const DateTimeString : String; const Format : String = ''): TDateTime ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
Function StringToDateTimeDef( const DateTimeString : String ;
   const DefaultValue : TDateTime; const Format : String = '') : TDateTime ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
function StoD( YYYYMMDDhhnnss: String) : TDateTime; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
function DtoS( ADate : TDateTime) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
function DTtoS( ADateTime : TDateTime) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};

function Iso8601ToDateTime(const AISODate: string): TDateTime; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
function DateTimeToIso8601(ADate: TDateTime; const ATimeZone: string = ''): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};

function IsWorkingDay(ADate: TDateTime): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
function WorkingDaysBetween(StartDate, EndDate: TDateTime): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};
function IncWorkingDay(ADate: TDateTime; WorkingDays: Integer): TDatetime; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.DateTime.pas' {$ENDIF};

{//// ACBrUtil.FilesIO}
function CompareVersions( const VersionStr1, VersionStr2 : String;
  Delimiter: char = '.' ) : Extended; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

function InPort(const PortAddr:word): byte; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
procedure OutPort(const PortAddr: word; const Databyte: byte); overload ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

function StrCrypt(const AString, StrChave: AnsiString): AnsiString; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function SomaAscII(const AString : AnsiString): Integer; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function StringCrc16(const AString : AnsiString ) : word; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function StringCrcCCITT(const s: AnsiString; initial:Word=$1D0F; polynomial:Word=$1021): Word; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

function ApplicationPath: String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Procedure FindFiles( const FileMask : String; AStringList : TStrings;
  IncludePath : Boolean = True;
  SortType: TFindFileSortType = fstNone;
  SortDirection: TFindFileSortDirection = fsdNone ) ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Procedure FindSubDirectories( const APath: String; AStringList : TStrings;
  IncludePath : Boolean = True ) ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Function FilesExists(const FileMask: String) : Boolean ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Procedure DeleteFiles(const FileMask: String; RaiseExceptionOnFail : Boolean = True)  ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Procedure TryDeleteFile(const AFile: String; WaitTime: Integer = 1000)  ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function CopyFileTo(const AFromFileName, AToFileName : String;
   const AFailIfExists : Boolean = false) : Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Function PathWithDelim( const APath : String ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Function PathWithoutDelim( const APath : String ) : String ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Procedure CopyFilesToDir( FileMask : String ; ToDirName : String;
   const ForceDirectory : Boolean = False)  ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
procedure RunCommand(const Command: String; const Params: String = '';
   Wait : Boolean = false; WindowState : Word = 5); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
procedure OpenURL( const URL : String ) ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

function FunctionDetect (const LibName, FuncName: String; var LibPointer: Pointer): boolean; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function FunctionDetect (const LibName, FuncName: String; var LibPointer: Pointer;
   var LibHandle: TLibHandle ): boolean; overload ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function UnLoadLibrary(const LibName: String ): Boolean ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

function FlushToDisk(const sFile: string): boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function FlushFileToDisk(const sFile: string): boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

Procedure DesligarMaquina(Reboot: Boolean = False; Forcar: Boolean = False;
   LogOff: Boolean = False) ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
{$IfNDef NOGUI}
function ForceForeground(AppHandle:{$IfDef FPC}LCLType.HWND{$Else}THandle{$EndIf}): boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
{$EndIf}

Procedure WriteToFile( const Arq: String; const ABinaryString : AnsiString;
   const ForceDirectory : Boolean = False); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
Procedure WriteToTXT( const ArqTXT : String; const ABinaryString : AnsiString;
   const AppendIfExists : Boolean = True; const AddLineBreak : Boolean = True;
   const ForceDirectory : Boolean = False); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
procedure WriteLog(const ArqTXT : String; const ABinaryString: AnsiString;
   const Traduz : Boolean = False) ; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function TranslateUnprintable( const ABinaryString: AnsiString ): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

function UnZip(S: TStream): AnsiString; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function UnZip(const ABinaryString: AnsiString): AnsiString; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function Zip(AStream: TStream): AnsiString; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function Zip(const ABinaryString: AnsiString): AnsiString; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

procedure LerIniArquivoOuString(const IniArquivoOuString: String; AMemIni: TMemIniFile); deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function StringIsINI(const AString: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function StringIsAFile(const AString: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function StringIsXML(const AString: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function StrIsIP(const AValue: String): Boolean; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método StrIsIP da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

{$IFDEF MSWINDOWS}
var xInp32 : function (wAddr: word): byte; stdcall;
var xOut32 : function (wAddr: word; bOut: byte): byte; stdcall;
var xBlockInput : function (Block: BOOL): BOOL; stdcall;

procedure LoadInpOut; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
procedure LoadBlockInput; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

function GetLastErrorAsHexaStr(WinErro: DWORD = 0): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};
function GetFileVersion(const AFile: String): String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método da Unit ACBrUtil.FilesIO.pas' {$ENDIF};

{$ENDIF}

implementation

Uses
  synautil,
  ACBrCompress, StrUtilsEx, typinfo;


function PosExA(const SubStr, S: AnsiString; Offset: Integer): Integer;
begin
  Result := ACBrUtil.Base.PosExA(SubStr, S, Offset);
end;

{-----------------------------------------------------------------------------
  Transforma <NumInteiro> em String, preenchendo com Zeros a Esquerda até
  atingiros digitos de <Tamnho>
 ---------------------------------------------------------------------------- }
function IntToStrZero(const NumInteiro : Int64 ; Tamanho : Integer) : String ;
begin
  Result := ACBrUtil.Base.IntToStrZero( NumInteiro, Tamanho) ;
end ;

{-----------------------------------------------------------------------------
  Converte uma <NumString> para Double, semelhante ao StrToFloatDef, mas
  verifica se a virgula é '.' ou ',' efetuando a conversão se necessário
  Se não for possivel converter, retorna <DefaultValue>
 ---------------------------------------------------------------------------- }
function StringToFloatDef(const NumString : String ; const DefaultValue : Double) : Double ;
begin
  Result := ACBrUtil.Base.StringToFloatDef(NumString, DefaultValue);
end ;

{-----------------------------------------------------------------------------
  Faz o mesmo que FormatFloat, porém garante que o resultado final terá
  o separador de decimal = ',' e o separador de milhar como Ponto
 ---------------------------------------------------------------------------- }
function FormatFloatBr(const AValue: Extended; AFormat: String): String;
begin
  Result := ACBrUtil.Base.FormatFloatBr(AValue, AFormat);
end;

function FloatMask(const DecimalDigits: SmallInt; UseThousandSeparator: Boolean): String;
begin
  Result := ACBrUtil.Base.FloatMask(DecimalDigits, UseThousandSeparator);
end;

{-----------------------------------------------------------------------------
  Converte uma <NumString> para Double, semelhante ao StrToFloat, mas
  verifica se a virgula é '.' ou ',' efetuando a conversão se necessário
  Se não for possivel converter, dispara Exception
 ---------------------------------------------------------------------------- }
function StringToFloat(NumString: String): Double;
begin
  Result := ACBrUtil.Base.StringToFloat(NumString);
end;

{-----------------------------------------------------------------------------
  Converte um Double para string, SEM o separator decimal, considerando as
  decimais como parte final da String. Ex: 100,00 = "10000"; 1,23 = "123"
 ---------------------------------------------------------------------------- }
function FloatToIntStr(const AValue : Double ; const DecimalDigits : SmallInt) : String ;
begin
  Result := ACBrUtil.Base.FloatToIntStr(AValue, DecimalDigits);
end;

{-----------------------------------------------------------------------------
  Converte um String, SEM separador decimal, para Double, considerando a
  parte final da String como as decimais. Ex: 10000 = "100,00"; 123 = "1,23"
 ---------------------------------------------------------------------------- }
function StringDecimalToFloat(const AValue: String; const DecimalDigits: SmallInt): Double;
begin
  Result := ACBrUtil.Base.StringDecimalToFloat(AValue, DecimalDigits);
end;

{-----------------------------------------------------------------------------
  Converte um Double para string, semelhante a FloatToStr(), porém
  garante que não haverá separador de Milhar e o Separador Decimal será igual a
  "SeparadorDecimal" ( o default é .(ponto))
 ---------------------------------------------------------------------------- }
function FloatToString(const AValue: Double; SeparadorDecimal: Char; const AFormat: String): String;
begin
  Result := ACBrUtil.Base.FloatToString(AValue, SeparadorDecimal, AFormat);
end;

function EstaVazio(const AValue: String): Boolean;
begin
  Result := ACBrUtil.Base.EstaVazio(AValue);
end;

procedure EstaVazio(const AValue, AMensagem: String);
begin
  ACBrUtil.Base.EstaVazio(AValue, AMensagem);
end;

function NaoEstaVazio(const AValue: String): Boolean;
begin
  Result := ACBrUtil.Base.NaoEstaVazio(AValue);
end;

function EstaZerado(const AValue: Double): Boolean;
begin
  Result := ACBrUtil.Base.EstaZerado(AValue);
end;

function EstaZerado(const AValue: Integer): Boolean;
begin
  Result := ACBrUtil.Base.EstaZerado(AValue);
end;

procedure EstaZerado(const AValue: Integer; const AMensagem: String);
begin
  ACBrUtil.Base.EstaZerado(AValue, AMensagem);
end;

function NaoEstaZerado(const AValue: Double): Boolean;
begin
  Result := ACBrUtil.Base.NaoEstaZerado(AValue);
end;

function NaoEstaZerado(const AValue: Integer): Boolean;
begin
  Result := ACBrUtil.Base.NaoEstaZerado(AValue);
end;

function TamanhoIgual(const AValue: String; const ATamanho: Integer): Boolean;
begin
  Result := ACBrUtil.Base.TamanhoIgual(AValue, ATamanho);
end;

procedure TamanhoIgual(const AValue: String; const ATamanho: Integer; const AMensagem: String);
begin
  ACBrUtil.Base.TamanhoIgual(AValue, ATamanho, AMensagem);
end;

function TamanhoIgual(const AValue: Integer; const ATamanho: Integer): Boolean;
begin
  Result := ACBrUtil.Base.TamanhoIgual(AValue, ATamanho);
end;

procedure TamanhoIgual(const AValue: Integer; const ATamanho: Integer; const AMensagem: String);
begin
  ACBrUtil.Base.TamanhoIgual(AValue, ATamanho, AMensagem);
end;

function TamanhoMenor(const AValue: String; const ATamanho: Integer): Boolean;
begin
  Result := ACBrUtil.Base.TamanhoMenor(AValue, ATamanho);
end;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo '#13,v,#10', substituindo #nn por chr(nn). Ignora todo
   texto apos a String ' | '
 ---------------------------------------------------------------------------- }
function TraduzComando(const AString: String): AnsiString;
begin
  Result := ACBrUtil.Base.TraduzComando(AString);
end;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo chr(13)+chr(10) para uma representação que possa ser
   lida por AscToString Ex: '#13,#10'
 ---------------------------------------------------------------------------- }
function StringToAsc(const AString: AnsiString): String;
begin
  Result := ACBrUtil.Base.StringToAsc(AString);
end;

{-----------------------------------------------------------------------------
  Traduz Strings do Tipo '#13,v,#10', substituindo #nn por chr(nn).
  Usar , para separar um campo do outro... No exemplo acima o resultado seria
  chr(13)+'v'+chr(10)
 ---------------------------------------------------------------------------- }
function AscToString(const AString: String): AnsiString;
begin
  Result := ACBrUtil.Base.AscToString(AString);
end;

{------------------------------------------------------------------------------
 Calcula e Retorna o Digito verificador do EAN-13 de acordo com 12 primeiros
  caracteres de <CodEAN13>
 ------------------------------------------------------------------------------}
function EAN13_DV(CodEAN13: String): String;
begin
  Result := ACBrUtil.Base.EAN13_DV(CodEAN13);
end;

{------------------------------------------------------------------------------
 Retorna True se o <CodEAN13> informado for válido
 ------------------------------------------------------------------------------}
function EAN13Valido(const CodEAN13: String): Boolean;
begin
  Result := ACBrUtil.Base.EAN13Valido(CodEAN13);
end;

function FormatFloatBr(const AFormat: TFormatMask; const AValue: Extended): String; overload;
begin
  Result := ACBrUtil.Base.FormatFloatBr(AFormat, AValue);
end;

{------------------------------------------------------------------------------
   Inserir um valor a propriedade por RTTI
 ------------------------------------------------------------------------------}
procedure RttiSetProp(AObject: TObject; AProp, AValue: String);
begin
  ACBrUtil.Base.RttiSetProp(AObject, AProp, AValue);
end;
{///FIM//////  ACBrUtil.Base}


{/////////  ACBrUtil.Compatibilidade (especialmente D6/D5)}
{$IFNDEF COMPILER6_UP}
function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double;
begin
  Result := ACBrUtil.Compatibilidade.RoundTo(AValue, ADigit);
end;

function SimpleRoundTo(const AValue: Double; const ADigit: TRoundToRange = -2): Double;
begin
  Result := ACBrUtil.Compatibilidade.SimpleRoundTo(AValue, ADigit);
end;

{ IfThens retirada de Math.pas do D7, para compatibilizar com o Delphi 5
(que nao possue essas funçao) }
function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload;
begin
  Result := ACBrUtil.Compatibilidade.IfThen(AValue, ATrue, AFalse);
end;

function IfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64; overload;
begin
  Result := ACBrUtil.Compatibilidade.IfThen(AValue, ATrue, AFalse);
end;

function IfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0.0): Double; overload;
begin
  Result := ACBrUtil.Compatibilidade.IfThen(AValue, ATrue, AFalse);
end;

function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload;
begin
  Result := ACBrUtil.Compatibilidade.IfThen(AValue, ATrue, AFalse);
end;
{$endif}

{$IFNDEF COMPILER7_UP}
{ PosEx, retirada de StrUtils.pas do D7, para compatibilizar com o Delphi 6
  (que nao possui essa funçao) }
function PosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;
begin
  Result := ACBrUtil.Compatibilidade.PosEx(SubStr, S, Offset);
end;
{$ENDIF}

{$IfNDef HAS_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
begin
  Result := ACBrUtil.Compatibilidade.CharInSet(C, CharSet);
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
begin
  Result := ACBrUtil.Compatibilidade.CharInSet(C, CharSet);
end;
{$EndIf}

{$IFDEF HAS_FORMATSETTINGS}
function CreateFormatSettings: TFormatSettings;
begin
  Result := ACBrUtil.Compatibilidade.CreateFormatSettings;
end;
{$ENDIF}
{///FIM//////  ACBrUtil.Compatibilidade (especialmente D6/D5)}



{/////////  ACBrUtil.XMLHTML}
function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
   const IsUTF8: Boolean = True) : String;
begin
  Result := ACBrUtil.XMLHTML.ParseText(Texto, Decode, IsUTF8);
end;

function LerTagXML( const AXML, ATag: String; IgnoreCase: Boolean = True) : String;
begin
  Result := ACBrUtil.XMLHTML.LerTagXML( AXML, ATag, IgnoreCase);
end;

function XmlEhUTF8(const AXML: String): Boolean;
begin
  Result := ACBrUtil.XMLHTML.XmlEhUTF8(AXML);
end;

function ConverteXMLtoUTF8(const AXML: String): String;
begin
  Result := ACBrUtil.XMLHTML.ConverteXMLtoUTF8(AXML);
end;

function ConverteXMLtoNativeString(const AXML: String): String;
begin
  Result := ACBrUtil.XMLHTML.ConverteXMLtoNativeString(AXML);
end;

function ObtemDeclaracaoXML(const AXML: String): String;
begin
  Result := ACBrUtil.XMLHTML.ObtemDeclaracaoXML(AXML);
end;

function RemoverDeclaracaoXML(const AXML: String): String;
begin
  Result := ACBrUtil.XMLHTML.RemoverDeclaracaoXML(AXML);
end;

function InserirDeclaracaoXMLSeNecessario(const AXML: String;
   const ADeclaracao: String = CUTF8DeclaracaoXML): String;
begin
  Result := ACBrUtil.XMLHTML.InserirDeclaracaoXMLSeNecessario(AXML, ADeclaracao);
end;

function SeparaDados(const AString: String; const Chave: String; const MantemChave : Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True) : String;
begin
  Result := ACBrUtil.XMLHTML.SeparaDados(AString, Chave, MantemChave, PermitePrefixo, AIgnoreCase);
end;

function SeparaDadosArray(const AArray: Array of String; const AString: String; const MantemChave: Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True) : String;
begin
  Result := ACBrUtil.XMLHTML.SeparaDadosArray(AArray, AString, MantemChave, PermitePrefixo, AIgnoreCase);
end;

procedure EncontrarInicioFinalTag(const aText, ATag: String; var PosIni, PosFim: integer;const PosOffset: integer = 0);
begin
  ACBrUtil.XMLHTML.EncontrarInicioFinalTag(aText, ATag, PosIni, PosFim, PosOffset);
end;

function StripHTML(const AHTMLString : String) : String;
begin
  Result := ACBrUtil.XMLHTML.StripHTML(AHTMLString);
end;

procedure AcharProximaTag(const ABinaryString: AnsiString; const PosIni: Integer; var ATag: AnsiString; var PosTag: Integer);
begin
  ACBrUtil.XMLHTML.AcharProximaTag(ABinaryString, PosIni, ATag, PosTag);
end;
{////FIM/////  ACBrUtil.XMLHTML}

{/////////  ACBrUtil.Strings}
function Split(const ADelimiter: Char; const AString: string): TSplitResult;
begin
  Result := ACBrUtil.Strings.Split(ADelimiter, AString);
end;

function DecodeToString( const ABinaryString : AnsiString; const StrIsUTF8: Boolean ) : String ;
begin
  Result := ACBrUtil.Strings.DecodeToString(ABinaryString, StrIsUTF8);
end;

function RetornarConteudoEntre(const Frase, Inicio, Fim: String; IncluiInicioFim: Boolean = False): string;
begin
  Result := ACBrUtil.Strings.RetornarConteudoEntre(Frase, Inicio, Fim, IncluiInicioFim);
end;

procedure QuebrarLinha(const Alinha: string; const ALista: TStringList;
  const QuoteChar: char = '"'; Delimiter: char = ';');
begin
  ACBrUtil.Strings.QuebrarLinha(Alinha, ALista, QuoteChar, Delimiter);
end;

function ACBrStr( const AString : String ) : String ;
begin
  Result := ACBrUtil.Strings.ACBrStr(AString);
end;

function ACBrStrToAnsi( const AString : String ) : String ;
begin
  Result := ACBrUtil.Strings.ACBrStrToAnsi(AString);
end;

function NativeStringToUTF8(const AString : String ) : AnsiString;
begin
  Result := ACBrUtil.Strings.NativeStringToUTF8(AString);
end;

function UTF8ToNativeString(const AUTF8String : AnsiString ) : String;
begin
  Result := ACBrUtil.Strings.UTF8ToNativeString(AUTF8String);
end;

function NativeStringToAnsi(const AString : String ) : AnsiString;
begin
  Result := ACBrUtil.Strings.NativeStringToAnsi(AString);
end;

function AnsiToNativeString(const AAnsiString : AnsiString ) : String;
begin
  Result := ACBrUtil.Strings.AnsiToNativeString(AAnsiString);
end;

{$IfDef FPC}
function GetSysANSIencoding: String;
begin
  Result := ACBrUtil.Strings.GetSysANSIencoding;
end;

{$EndIf}
function ACBrUTF8ToAnsi( const AUTF8String : AnsiString ) : AnsiString;
begin
  Result := ACBrUtil.Strings.ACBrUTF8ToAnsi(AUTF8String);
end;

function ACBrAnsiToUTF8( const AAnsiString : AnsiString ) : AnsiString;
begin
  Result := ACBrUtil.Strings.ACBrAnsiToUTF8(AAnsiString);
end;

function AnsiChr( b: Byte) : AnsiChar;
begin
  Result := ACBrUtil.Strings.AnsiChr(b);
end;

function TranslateString(const S: AnsiString; CP_Destino: Word; CP_Atual: Word = 0): AnsiString;
begin
  Result := ACBrUtil.Strings.TranslateString(S, CP_Destino, CP_Atual);
end;


function PadRight(const AString : String; const nLen : Integer; const Caracter : Char = ' ') : String;
begin
  Result := ACBrUtil.Strings.PadRight(AString, nLen, Caracter);
end;

function PadRightA(const AAnsiString : AnsiString; const nLen : Integer; const Caracter : AnsiChar = ' ') : AnsiString;
begin
  Result := ACBrUtil.Strings.PadRightA(AAnsiString, nLen, Caracter);
end;

function PadLeft(const AString : String; const nLen : Integer; const Caracter : Char = ' ') : String;
begin
  Result := ACBrUtil.Strings.PadLeft(AString, nLen, Caracter);
end;

function PadLeftA(const AAnsiString : AnsiString; const nLen : Integer; const Caracter : AnsiChar = ' ') : AnsiString;
begin
  Result := ACBrUtil.Strings.PadLeftA(AAnsiString, nLen, Caracter);
end;

function PadCenter(const AString : String; const nLen : Integer; const Caracter : Char = ' ') : String;
begin
  Result := ACBrUtil.Strings.PadCenter(AString, nLen, Caracter);
end;

function PadCenterA(const AAnsiString : AnsiString; const nLen : Integer; const Caracter : AnsiChar = ' ') : AnsiString;
begin
  Result := ACBrUtil.Strings.PadCenterA(AAnsiString, nLen, Caracter);
end;

function PadSpace(const AString : String; const nLen : Integer; Separador : String;
   const Caracter : Char = ' '; const RemoverEspacos: Boolean = True) : String;
begin
  Result := ACBrUtil.Strings.PadSpace(AString, nLen, Separador, Caracter, RemoverEspacos);
end;

function RemoveString(const sSubStr, sString: String): String;
begin
  Result := ACBrUtil.Strings.RemoveString(sSubStr, sString);
end;

function RemoveStrings(const AText: AnsiString; StringsToRemove: array of AnsiString): AnsiString;
begin
  Result := ACBrUtil.Strings.RemoveStrings(AText, StringsToRemove);
end;

function RemoverEspacosDuplos(const AString: String): String;
begin
  Result := ACBrUtil.Strings.RemoverEspacosDuplos(AString);
end;

procedure RemoveEmptyLines( AStringList: TStringList);
begin
  ACBrUtil.Strings.RemoveEmptyLines(AStringList);
end;

function RandomName(const LenName : Integer = 8) : String;
begin
  Result := ACBrUtil.Strings.RandomName(LenName);
end;

function IfEmptyThen( const AValue, DefaultValue: String; DoTrim: Boolean = True) : String;
begin
  Result := ACBrUtil.Strings.IfEmptyThen(AValue, DefaultValue, DoTrim);
end;

function PosAt(const SubStr, S: AnsiString; Ocorrencia : Cardinal = 1): Integer;
begin
  Result := ACBrUtil.Strings.PosAt(SubStr, S, Ocorrencia);
end;

function RPos(const aSubStr, aString : AnsiString; const aStartPos: Integer): Integer; overload;
begin
  Result := ACBrUtil.Strings.RPos(aSubStr, aString, aStartPos);
end;

function RPos(const aSubStr, aString : AnsiString): Integer; overload;
begin
  Result := ACBrUtil.Strings.RPos(aSubStr, aString);
end;

function PosLast(const SubStr, S: String): Integer;
begin
  Result := ACBrUtil.Strings.PosLast(SubStr, S);
end;

function CountStr(const AString, SubStr : String ) : Integer ;
begin
  Result := ACBrUtil.Strings.CountStr(AString, SubStr);
end;

Function Poem_Zeros(const Texto : String; const Tamanho : Integer) : String; overload;
begin
  Result := ACBrUtil.Strings.Poem_Zeros(Texto, Tamanho);
end;

function Poem_Zeros(const NumInteiro : Int64 ; Tamanho : Integer) : String ; overload;
begin
  Result := ACBrUtil.Strings.Poem_Zeros(NumInteiro, Tamanho);
end;

function RemoveZerosEsquerda(const ANumStr: String): String;
begin
  Result := ACBrUtil.Strings.RemoveZerosEsquerda(ANumStr);
end;


function StrIsAlpha(const S: String): Boolean;
begin
  Result := ACBrUtil.Strings.StrIsAlpha(S);
end;

function StrIsAlphaNum(const S: String): Boolean;
begin
  Result := ACBrUtil.Strings.StrIsAlphaNum(S);
end;

function StrIsNumber(const S: String): Boolean;
begin
  Result := ACBrUtil.Strings.StrIsNumber(S);
end;

function StrIsHexa(const S: String): Boolean;
begin
  Result := ACBrUtil.Strings.StrIsHexa(S);
end;

function StrIsBinary(const S: String): Boolean;
begin
  Result := ACBrUtil.Strings.StrIsBinary(S);
end;

function StrIsBase64(const S: String): Boolean;
begin
  Result := ACBrUtil.Strings.StrIsBase64(S);
end;

function CharIsAlpha(const C: Char): Boolean;
begin
  Result := ACBrUtil.Strings.CharIsAlpha(C);
end;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  Result := ACBrUtil.Strings.CharIsAlphaNum(C);
end;

function CharIsNum(const C: Char): Boolean;
begin
  Result := ACBrUtil.Strings.CharIsNum(C);
end;

function CharIsHexa(const C: Char): Boolean;
begin
  Result := ACBrUtil.Strings.CharIsHexa(C);
end;

function CharIsBinary(const C: Char): Boolean;
begin
  Result := ACBrUtil.Strings.CharIsBinary(C);
end;

function CharIsBase64(const C: Char): Boolean;
begin
  Result := ACBrUtil.Strings.CharIsBase64(C);
end;

function OnlyNumber(const AValue: String): String;
begin
  Result := ACBrUtil.Strings.OnlyNumber(AValue);
end;

function OnlyAlpha(const AValue: String): String;
begin
  Result := ACBrUtil.Strings.OnlyAlpha(AValue);
end;

function OnlyAlphaNum(const AValue: String): String;
begin
  Result := ACBrUtil.Strings.OnlyAlphaNum(AValue);
end;

function OnlyCharsInSet(const AValue: String; SetOfChars: TSetOfChars): String;
begin
  Result := ACBrUtil.Strings.OnlyCharsInSet(AValue, SetOfChars);
end;

function TiraAcentos( const AString : String ) : String ;
begin
  Result := ACBrUtil.Strings.TiraAcentos(AString);
end;

function TiraAcento( const AChar : AnsiChar ) : AnsiChar ;
begin
  Result := ACBrUtil.Strings.TiraAcento(AChar);
end;

function AjustaLinhas(const Texto: AnsiString; Colunas: Integer ;
   NumMaxLinhas: Integer = 0; PadLinhas: Boolean = False): AnsiString;
begin
  Result := ACBrUtil.Strings.AjustaLinhas(Texto, Colunas, NumMaxLinhas, PadLinhas);
end;

function QuebraLinhas(const Texto: String; const Colunas: Integer;
   const CaracterQuebrar : AnsiChar = ' '): String;
begin
  Result := ACBrUtil.Strings.QuebraLinhas(Texto, Colunas, CaracterQuebrar);
end;

function RemoverQuebraLinhaFinal(const ATexto: String; const AQuebraLinha: String = ''): String;
begin
  Result := ACBrUtil.Strings.RemoverQuebraLinhaFinal(ATexto, AQuebraLinha);
end;

function TiraPontos(const Str: string): string;
begin
  Result := ACBrUtil.Strings.TiraPontos(Str);
end;

function TBStrZero(const i: string; const Casas: byte): string;
begin
  Result := ACBrUtil.Strings.TBStrZero(i, Casas);
end;

function Space(Tamanho: Integer): string;
begin
  Result := ACBrUtil.Strings.Space(Tamanho);
end;

function LinhaSimples(Tamanho: Integer): string;
begin
  Result := ACBrUtil.Strings.LinhaSimples(Tamanho);
end;

function LinhaDupla(Tamanho: Integer): string;
begin
  Result := ACBrUtil.Strings.LinhaDupla(Tamanho);
end;

function MatchText(const AText: String; const AValues: array of String): Boolean;
begin
  Result := ACBrUtil.Strings.MatchText(AText, AValues);
end;

function FindDelimiterInText( const AText: String; ADelimiters: String = ''): Char;
begin
  Result := ACBrUtil.Strings.FindDelimiterInText(AText, ADelimiters);
end;

function AddDelimitedTextToList( const AText: String; const ADelimiter: Char;
   AStringList: TStrings; const AQuoteChar: Char = '"'): Integer;
begin
  Result := ACBrUtil.Strings.AddDelimitedTextToList(AText, ADelimiter, AStringList, AQuoteChar);
end;

function ChangeLineBreak(const AText: String; const NewLineBreak: String = ';'): String;
begin
  Result := ACBrUtil.Strings.ChangeLineBreak(AText, NewLineBreak);
end;


{///FIM//////  ACBrUtil.Strings}

{/////////  ACBrUtil.Math}

function SimpleRoundToEX(const AValue: Extended; const ADigit: TRoundToRange = -2): Extended;
begin
  Result := ACBrUtil.Math.SimpleRoundToEX(AValue, ADigit);
end;

function TruncFix( X : Extended ) : Int64 ;
begin
  Result := ACBrUtil.Math.TruncFix(X);
end;

function RoundABNT(const AValue: Double; const Digits: TRoundToRange; const Delta: Double = 0.00001 ): Double;
begin
  Result := ACBrUtil.Math.RoundABNT(AValue, Digits, Delta);
end;

function TruncTo(const AValue: Double; const Digits: TRoundToRange): Double;
begin
  Result := ACBrUtil.Math.TruncTo(AValue, Digits);
end;

function ComparaValor(const ValorUm, ValorDois : Double; const Tolerancia : Double = 0 ): Integer;
begin
  Result := ACBrUtil.Math.ComparaValor(ValorUm, ValorDois, Tolerancia);
end;

function TestBit(const AValue: Integer; const AIndex: Byte): Boolean;
begin
  Result := ACBrUtil.Math.TestBit(AValue, AIndex);
end;

procedure ClearBit(var AValue: Integer; const AIndex: Byte);
begin
  ACBrUtil.Math.ClearBit(AValue, AIndex);
end;

procedure SetBit(var AValue: Integer; const AIndex: Byte);
begin
  ACBrUtil.Math.SetBit(AValue, AIndex);
end;

procedure PutBit(var AValue: Integer; const AIndex: Byte; State: Boolean);
begin
  ACBrUtil.Math.PutBit(AValue, AIndex, State);
end;

function IntToBin(value: LongInt; digits: integer ): string;
begin
  Result := ACBrUtil.Math.IntToBin(value, digits);
end;

function BinToInt(Value: String): LongInt;
begin
  Result := ACBrUtil.Math.BinToInt(Value);
end;

Function BcdToAsc( const StrBCD : AnsiString) : String ;
begin
  Result := ACBrUtil.Math.BcdToAsc(StrBCD);
end;

Function AscToBcd( const ANumStr: String ; const TamanhoBCD : Byte) : AnsiString ;
begin
  Result := ACBrUtil.Math.AscToBcd(ANumStr, TamanhoBCD);
end;

function IntToLEStr(AInteger: Integer; BytesStr: Integer = 2): AnsiString;
begin
  Result := ACBrUtil.Math.IntToLEStr(AInteger, BytesStr);
end;

function LEStrToInt(const ALEStr: AnsiString): Integer;
begin
  Result := ACBrUtil.Math.LEStrToInt(ALEStr);
end;

function IntToBEStr(AInteger: Integer; BytesStr: Integer = 2): AnsiString;
begin
  Result := ACBrUtil.Math.IntToBEStr(AInteger, BytesStr);
end;

function BEStrToInt(const ABEStr: AnsiString): Integer;
begin
  Result := ACBrUtil.Math.BEStrToInt(ABEStr);
end;

function HexToAscii(const HexStr : String) : AnsiString ;
begin
  Result := ACBrUtil.Math.HexToAscii(HexStr);
end;

function AsciiToHex(const ABinaryString: AnsiString): String;
begin
  Result := ACBrUtil.Math.AsciiToHex(ABinaryString);
end;

function TryHexToAscii(const HextStr: String; out Value: AnsiString): Boolean;
begin
  Result := ACBrUtil.Math.TryHexToAscii(HextStr, Value);
end;

function HexToAsciiDef(const HexStr: String; const Default: AnsiString): AnsiString;
begin
  Result := ACBrUtil.Math.HexToAsciiDef(HexStr, Default);
end;

function BinaryStringToString(const AString: AnsiString): String;
begin
  Result := ACBrUtil.Math.BinaryStringToString(AString);
end;

function StringToBinaryString(const AString: String): AnsiString;
begin
  Result := ACBrUtil.Math.StringToBinaryString(AString);
end;
{///FIM//////  ACBrUtil.Math}

{/////////  ACBrUtil.DateTime}
function FormatDateBr(const ADateTime: TDateTime; AFormat: String = ''): String;
begin
  Result := ACBrUtil.DateTime.FormatDateBr(ADateTime, AFormat);
end;

function FormatDateTimeBr(const ADate: TDateTime; AFormat: String = ''): String;
begin
  Result := ACBrUtil.DateTime.FormatDateTimeBr(ADate, AFormat);
end;

function StringToDateTime( const DateTimeString : String; const Format : String = '') : TDateTime ;
begin
  Result := ACBrUtil.DateTime.StringToDateTime(DateTimeString, Format);
end;

function StringToDateTimeDef( const DateTimeString : String ;
   const DefaultValue : TDateTime; const Format : String = '') : TDateTime ;
begin
  Result := ACBrUtil.DateTime.StringToDateTimeDef(DateTimeString, DefaultValue, Format);
end;

function StoD( YYYYMMDDhhnnss: String) : TDateTime;
begin
  Result := ACBrUtil.DateTime.StoD(YYYYMMDDhhnnss);
end;

function DtoS( ADate : TDateTime) : String;
begin
  Result := ACBrUtil.DateTime.DtoS(ADate);
end;

function DTtoS( ADateTime : TDateTime) : String;
begin
  Result := ACBrUtil.DateTime.DTtoS(ADateTime);
end;

function Iso8601ToDateTime(const AISODate: string): TDateTime;
begin
  Result := ACBrUtil.DateTime.Iso8601ToDateTime(AISODate);
end;

function DateTimeToIso8601(ADate: TDateTime; const ATimeZone: string = ''): string;
begin
  Result := ACBrUtil.DateTime.DateTimeToIso8601(ADate, ATimeZone);
end;

function IsWorkingDay(ADate: TDateTime): Boolean;
begin
  Result := ACBrUtil.DateTime.IsWorkingDay(ADate);
end;

function WorkingDaysBetween(StartDate, EndDate: TDateTime): Integer;
begin
  Result := ACBrUtil.DateTime.WorkingDaysBetween(StartDate, EndDate);
end;

function IncWorkingDay(ADate: TDateTime; WorkingDays: Integer): TDatetime;
begin
  Result := ACBrUtil.DateTime.IncWorkingDay(ADate, WorkingDays);
end;

{///FIM//////  ACBrUtil.DateTime}

{//// ACBrUtil.FilesIO}
function CompareVersions( const VersionStr1, VersionStr2 : String; Delimiter: char = '.' ) : Extended;
begin
  Result := ACBrUtil.FilesIO.CompareVersions(VersionStr1, VersionStr2, Delimiter);
end;

function InPort(const PortAddr:word): byte;
begin
  Result := ACBrUtil.FilesIO.InPort(PortAddr);
end;

procedure OutPort(const PortAddr: word; const Databyte: byte); overload ;
begin
  ACBrUtil.FilesIO.OutPort(PortAddr, Databyte);
end;

function StrCrypt(const AString, StrChave: AnsiString): AnsiString;
begin
  Result := ACBrUtil.FilesIO.StrCrypt(AString, StrChave);
end;

function SomaAscII(const AString : AnsiString): Integer;
begin
  Result := ACBrUtil.FilesIO.SomaAscII(AString);
end;

function StringCrc16(const AString : AnsiString ) : word;
begin
  Result := ACBrUtil.FilesIO.StringCrc16(AString);
end;

function StringCrcCCITT(const s: AnsiString; initial:Word=$1D0F; polynomial:Word=$1021): Word;
begin
  Result := ACBrUtil.FilesIO.StringCrcCCITT(s, initial, polynomial);
end;

function ApplicationPath: string;
begin
  Result := ACBrUtil.FilesIO.ApplicationPath;
end;

procedure FindFiles( const FileMask : String; AStringList : TStrings;
  IncludePath : Boolean = True;
  SortType: TFindFileSortType = fstNone;
  SortDirection: TFindFileSortDirection = fsdNone ) ;
begin
  ACBrUtil.FilesIO.FindFiles(FileMask, AStringList, IncludePath, SortType, SortDirection);
end;

Procedure FindSubDirectories( const APath: String; AStringList : TStrings; IncludePath : Boolean = True ) ;
begin
  ACBrUtil.FilesIO.FindSubDirectories(APath, AStringList, IncludePath);
end;

function FilesExists(const FileMask: String) : Boolean ;
begin
  Result := ACBrUtil.FilesIO.FilesExists(FileMask);
end;

procedure DeleteFiles(const FileMask: String; RaiseExceptionOnFail : Boolean = True)  ;
begin
  ACBrUtil.FilesIO.DeleteFiles(FileMask, RaiseExceptionOnFail);
end;

procedure TryDeleteFile(const AFile: String; WaitTime: Integer = 1000)  ;
begin
  ACBrUtil.FilesIO.TryDeleteFile(AFile, WaitTime);
end;

function CopyFileTo(const AFromFileName, AToFileName : String; const AFailIfExists : Boolean = false) : Boolean;
begin
  Result := ACBrUtil.FilesIO.CopyFileTo(AFromFileName, AToFileName, AFailIfExists);
end;

function PathWithDelim( const APath : String ) : String ;
begin
  Result := ACBrUtil.FilesIO.PathWithDelim(APath);
end;

function PathWithoutDelim( const APath : String ) : String ;
begin
  Result := ACBrUtil.FilesIO.PathWithoutDelim(APath);
end;

procedure CopyFilesToDir( FileMask : String ; ToDirName : String; const ForceDirectory : Boolean = False)  ;
begin
  ACBrUtil.FilesIO.CopyFilesToDir(FileMask, ToDirName, ForceDirectory);
end;

procedure RunCommand(const Command: String; const Params: String = '';
   Wait : Boolean = false; WindowState : Word = 5);
begin
  ACBrUtil.FilesIO.RunCommand(Command, Params, Wait, WindowState);
end;

procedure OpenURL( const URL : String ) ;
begin
  ACBrUtil.FilesIO.OpenURL(URL);
end;

function FunctionDetect (const LibName, FuncName: String; var LibPointer: Pointer): boolean; overload ;
begin
  Result := ACBrUtil.FilesIO.FunctionDetect(LibName, FuncName, LibPointer);
end;

function FunctionDetect (const LibName, FuncName: String; var LibPointer: Pointer;
   var LibHandle: TLibHandle ): boolean; overload ;
begin
  Result := ACBrUtil.FilesIO.FunctionDetect(LibName, FuncName, LibPointer, LibHandle);
end;

function UnLoadLibrary(const LibName: String ): Boolean ;
begin
  Result := ACBrUtil.FilesIO.UnLoadLibrary(LibName);
end;


function FlushToDisk(const sFile: string): boolean;
begin
  Result := ACBrUtil.FilesIO.FlushToDisk(sFile);
end;

function FlushFileToDisk(const sFile: string): boolean;
begin
  Result := ACBrUtil.FilesIO.FlushFileToDisk(sFile);
end;

Procedure DesligarMaquina(Reboot: Boolean = False; Forcar: Boolean = False;
   LogOff: Boolean = False) ;
begin
  ACBrUtil.FilesIO.DesligarMaquina(Reboot, Forcar, LogOff);
end;

{$IfNDef NOGUI}
function ForceForeground(AppHandle:{$IfDef FPC}LCLType.HWND{$Else}THandle{$EndIf}): boolean;
begin
  Result := ACBrUtil.FilesIO.ForceForeground(AppHandle);
end;
{$EndIf}

procedure WriteToFile( const Arq: String; const ABinaryString : AnsiString;
   const ForceDirectory : Boolean = False);
begin
  ACBrUtil.FilesIO.WriteToFile(Arq, ABinaryString, ForceDirectory);
end;

procedure WriteToTXT( const ArqTXT : String; const ABinaryString : AnsiString;
   const AppendIfExists : Boolean = True; const AddLineBreak : Boolean = True;
   const ForceDirectory : Boolean = False);
begin
  ACBrUtil.FilesIO.WriteToTXT(ArqTXT, ABinaryString, AppendIfExists, AddLineBreak, ForceDirectory);
end;

procedure WriteLog(const ArqTXT : String; const ABinaryString: AnsiString;
   const Traduz : Boolean = False) ;
begin
  ACBrUtil.FilesIO.WriteLog(ArqTXT, ABinaryString, Traduz);
end;

function TranslateUnprintable( const ABinaryString: AnsiString ): String;
begin
  Result := ACBrUtil.FilesIO.TranslateUnprintable(ABinaryString);
end;

function UnZip(S: TStream): AnsiString; overload;
begin
  Result := ACBrUtil.FilesIO.UnZip(S);
end;

function UnZip(const ABinaryString: AnsiString): AnsiString; overload;
begin
  Result := ACBrUtil.FilesIO.UnZip(ABinaryString);
end;

function Zip(AStream: TStream): AnsiString; overload;
begin
  Result := ACBrUtil.FilesIO.Zip(AStream);
end;

function Zip(const ABinaryString: AnsiString): AnsiString; overload;
begin
  Result := ACBrUtil.FilesIO.Zip(ABinaryString);
end;

procedure LerIniArquivoOuString(const IniArquivoOuString: String; AMemIni: TMemIniFile);
begin
  ACBrUtil.FilesIO.LerIniArquivoOuString(IniArquivoOuString, AMemIni);
end;

function StringIsINI(const AString: String): Boolean;
begin
  Result := ACBrUtil.FilesIO.StringIsINI(AString);
end;

function StringIsAFile(const AString: String): Boolean;
begin
  Result := ACBrUtil.FilesIO.StringIsAFile(AString);
end;

function StringIsXML(const AString: String): Boolean;
begin
  Result := ACBrUtil.FilesIO.StringIsXML(AString);
end;

function StrIsIP(const AValue: String): Boolean;
begin
  Result := ACBrUtil.FilesIO.StrIsIP(AValue);
end;

{$IFDEF MSWINDOWS}
procedure LoadInpOut;
begin
  ACBrUtil.FilesIO.LoadInpOut;
end;
procedure LoadBlockInput;
begin
  ACBrUtil.FilesIO.LoadBlockInput;
end;

function GetLastErrorAsHexaStr(WinErro: DWORD = 0): String;
begin
  Result := ACBrUtil.FilesIO.GetLastErrorAsHexaStr(WinErro);
end;
function GetFileVersion(const AFile: String): String;
begin
  Result := ACBrUtil.FilesIO.GetFileVersion(AFile);
end;
{$ENDIF}

{//FIM// ACBrUtil.FilesIO}

initialization

end.

