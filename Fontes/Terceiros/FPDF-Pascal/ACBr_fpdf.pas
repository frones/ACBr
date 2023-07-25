{

 FPDF Pascal
 https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal

 Copyright (C) 2023 Projeto ACBr - Daniel Simões de Almeida

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

 Except as contained in this notice, the name of <Projeto ACBr> shall not be
 used in advertising or otherwise to promote the sale, use or other dealings in
 this Software without prior written authorization from <Projeto ACBr>.

 Based on:
 - The library FPDF written in PHP by Olivier PLATHEY
   http://www.fpdf.org/
 - Free JPDF Pascal from Jean Patrick e Gilson Nunes
   https://github.com/jepafi/Free-JPDF-Pascal
}

unit ACBr_fpdf;
// If you don't want the AnsiString vs String warnings to bother you
{$DEFINE REMOVE_CAST_WARN}

{$IfNDef FPC}
  {$IFDEF REMOVE_CAST_WARN}
   {$IF CompilerVersion >= 20}
    {$WARN IMPLICIT_STRING_CAST OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
   {$IfEnd}
  {$EndIf}
{$EndIf}

{$IfDef FPC}
  {$Mode objfpc}{$H+}
  {$Define USE_UTF8}
{$EndIf}

{$IfDef POSIX}
  {$IfDef LINUX}
    {$Define USE_UTF8}
  {$EndIf}
  {$Define FMX}
  {$Define HAS_SYSTEM_GENERICS}
{$EndIf}

{$IfDef NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
  {$Define HAS_SYSTEM_GENERICS}
  {$Define USE_UTF8}
{$EndIf}

interface

uses
  Classes,Contnrs,
  {$IfDef FPC}
   zstream
  {$Else}
   {$IfDef HAS_SYSTEM_GENERICS}
    System.Generics.Collections, System.Generics.Defaults,
   {$EndIf}
   ZLib
  {$EndIf},
  SysUtils;

const
  FPDF_VERSION = '1.85';
  LF = #10;
  CR = #13;

{$IfDef NEXTGEN}
type
  AnsiString = RawByteString;
  AnsiChar = UTF8Char;
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^MarshaledAString;
  WideString = String;
{$EndIf}

type
  EFPDFError = Exception;

  TFPDFColor = (cBlack, cSilver, cGray, cWhite, cMaroon, cRed, cPurple, cFuchsia,
    cGreen, cLime, cOlive, cYellow, cNavy, cBlue, cTeal, cAqua, cLightGrey);
  TFPDFOrientation = (poPortrait, poLandscape, poDefault);
  TFPDFRotation = (ro0, ro90, ro180, ro270);
  TFPDFUnit = (puPT, puMM, puCM, puIN, puPX);
  TFPDFPageFormat = (pfA3, pfA4, pfA5, pfLetter, pfLegal);
  TFPDFFontType = (ftCore, ftTrueType, ftType1);
  TFPDFFontEncode = (encNone, encCP1252);

  TFPDFFontInfo = array [0..255] of Integer;
  TFPDFZoomMode = (zmDefault, zmFullPage, zmFullWidth, zmReal, zmCustom);
  TFPDFLayoutMode = (lmDefault, lmSingle, lmContinuous, lmTwo);
  TFPDFContentStream = (csToViewBrowser, csToDownload);

  TByteArray = array of byte;
  TStringArray = array of string;

  TFPDFImageInfo = record
    n: Integer;
    i: Integer;
    ImageName: String;
    data: AnsiString;
    w: Integer;
    h: Integer;
    cs: String;
    bpc: Integer;
    f: String;
    dp: String;
    pal: AnsiString;
    trns: array of Integer;
    smask: AnsiString;
  end;

  TFPDFPageSize = record
    w: Double;
    h: Double;
  end;

  { TFPDFFont }

  TFPDFFont = class
  private
    fcw: TFPDFFontInfo;
    fenc: TFPDFFontEncode;
    fFontName: String;
    fFontType: TFPDFFontType;
    fName: String;
    fuv1: TFPDFFontInfo;
    fuv2: TFPDFFontInfo;
    fup: Double;
    fut: Double;
    fsubsetted: Boolean;
    fdiff: String;
    procedure SetFontName(AValue: String);
    procedure SetName(AValue: String);
  public
    constructor Create;
    procedure fill(var AFontInfo: TFPDFFontInfo; AValue: Integer);

    property FontType: TFPDFFontType read fFontType write fFontType;
    property FontName: String read fFontName write SetFontName;
    property Name: String read fName write SetName;
    property up: Double read fup write fup;
    property ut: Double read fut write fut;
    property cw: TFPDFFontInfo read fcw write fcw ;
    property enc: TFPDFFontEncode read fenc write fenc;
    property uv1: TFPDFFontInfo read fuv1 write fuv1;
    property uv2: TFPDFFontInfo read fuv2 write fuv2;
    property subsetted: Boolean read fsubsetted write fsubsetted;
    property diff: String read fdiff write fdiff;
  end;

  { TFPDFFonts }

  TFPDFFonts = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TFPDFFont>{$EndIf})
  private
    function GetFont(const AFontName: String): TFPDFFont;
    procedure SetObject(Index: Integer; Item: TFPDFFont);
    function GetObject(Index: Integer): TFPDFFont;
  public
    function New: TFPDFFont;
    function Add(Obj: TFPDFFont): Integer;
    procedure Insert(Index: Integer; Obj: TFPDFFont);
    property Item[Index: Integer]: TFPDFFont read GetObject write SetObject; default;
    property Font[const AFontName: String]: TFPDFFont read GetFont;
  end;

  TFPDFUsedFont = record
    FontName: String;
    n: Integer;
  end;

  { TStringListList }

  TStringListList = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TStringList>{$EndIf})
  private
    procedure SetObject(Index: Integer; Item: TStringList);
    function GetObject(Index: Integer): TStringList;
  public
    function New: TStringList;
    function Add(Obj: TStringList): Integer;
    procedure Insert(Index: Integer; Obj: TStringList);
    property Page[Index: Integer]: TStringList read GetObject write SetObject; default;
  end;

  { Links }

  TFPDFLink = record
    Page: Integer;
    y: Double;
  end;

  TFPDFPageLink = record
    X: Double;
    Y: Double;
    Width: Double;
    Height: Double;
    Link: String;
    n: Integer;
  end;

  TFPDFPageLinks = array of TFPDFPageLink;

  { TFPDF }

  TFPDF = class
  private
    procedure DefineDefaultPageSizes;
    function FindUsedFontIndex(const AFontName: String): Integer;

  protected
    page: Integer;                        // current page number
    n: Integer;                           // current object number
    offsets: array of Int64;              // array of object offsets
    buffer: TMemoryStream;                // buffer holding in-memory PDF
    pages: array of AnsiString;           // array containing pages
    state: Integer;                       // current document state
    compress: Boolean;                    // compression flag
    k: Double;                            // scale factor (number of points in user unit)
    DefOrientation: TFPDFOrientation;     // default orientation
    CurOrientation: TFPDFOrientation;     // current orientation
    StdPageSizes: array[TFPDFPageFormat] of TFPDFPageSize; // standard page sizes
    DefPageSize: TFPDFPageSize;           // default page size
    CurPageSize: TFPDFPageSize;           // current page size
    CurRotation: TFPDFRotation;           // current page rotation
    PageInfo: TStringListList;            // page-related data
    wPt, hPt: Double;                     // dimensions of current page in points
    w, h: Double;                         // dimensions of current page in user unit
    lMargin: Double;                      // left margin
    tMargin: Double;                      // top margin
    rMargin: Double;                      // right margin
    bMargin: Double;                      // page break margin
    cMargin: Double;                      // cell margin
    x, y: Double;                         // current position in user unit for cell positionning
    lasth: Double;                        // height of last cell printed
    LineWidth: Double;                    // line width in user unit
    fontpath: String;                     // path containing fonts
    Fonts: TFPDFFonts;                    // TObjectList of core fonts
    UsedFonts: array of TFPDFUsedFont;    // array of used fonts
    FontFiles: array of String;           // array of font files
    encodings: TStringList;               // array of encodings
    cmaps: TStringList;                   // array of ToUnicode CMaps
    FontFamily: String;                   // current font family
    FontStyle: String;                    // current font style
    underline: Boolean;                   // underlining flag
    CurrentFont: TFPDFFont;               // current font info
    FontSizePt: Double;                   // current font size in points
    FontSize: Double;                     // current font size in user unit
    DrawColor: String;                    // commands for drawing color
    FillColor: String;                    // commands for filling color
    TextColor: String;                    // commands for text color
    ColorFlag: Boolean;                   // indicates whether fill and text colors are different
    WithAlpha: Boolean;                   // indicates whether alpha channel is used
    ws: Double;                           // word spacing
    images: array of TFPDFImageInfo;      // array of used images
    PageLinks: array of TFPDFPageLinks;   // array of links in pages
    links: array of TFPDFLink;            // array of internal links
    AutoPageBreak: Boolean;               // automatic page breaking
    PageBreakTrigger: Double;             // threshold used to trigger page breaks
    InHeader: Boolean;                    // flag set when processing header
    InFooter: Boolean;                    // flag set when processing footer
    AliasNbPages: String;                 // alias for total number of pages
    ZoomMode: TFPDFZoomMode;              // zoom display mode
    ZoomFactor: Integer;
    LayoutMode: TFPDFLayoutMode;          // layout display mode
    metadata: TStringList;                // document properties
    CreationDate: TDateTime;              // document creation date
    PDFVersion: Double;                   // PDF version number

    UseUTF8: boolean;                     // Set True if your compiler uses UTF8 as default (no convertions needed)
    TimeZone: String;                     // TimeZone to be used on Date values

    function FloatToStr(Value: Double): String;
    function ConvertTextToAnsi(const AText: String): String;
    procedure PopulateCoreFonts; virtual;
    function FindUsedImage(const ImageName:String): TFPDFImageInfo;
    procedure Image(img: TFPDFImageInfo; vX: Double = -9999; vY: Double = -9999;
      vWidth: Double = 0; vHeight: Double = 0; const vLink: String = ''); overload;

    function _getpagesize(APageSize: TFPDFPageSize): TFPDFPageSize; overload;
    function _getpagesize(APageFormat: TFPDFPageFormat): TFPDFPageSize; overload;
    procedure _beginpage(AOrientation: TFPDFOrientation; APageSize: TFPDFPageSize; ARotation: TFPDFRotation); virtual;
    procedure _endpage; virtual;

    function _isascii(const AString: String): Boolean;
    function _EncodeText(const AString: String): String;
    function _UTF8encode(const AString: String): String;
    function _UTF8toUTF16(const AString: String): WideString;

    function _escape(const sText: AnsiString): AnsiString;
    function _textstring(const AString: String): String; virtual;
    function _dounderline(vX, vY: Double; const vText: String): String;
    function _parseimage(vImageStream: TStream; const vImageExt: String): TFPDFImageInfo; overload;
    function _parsejpg(vImageStream: TStream): TFPDFImageInfo;
    function _parsepng(vImageStream: TStream): TFPDFImageInfo;
    procedure _out(const AData: AnsiString);
    procedure _put(const AData: AnsiString);
    function _getoffset: Int64;
    procedure _newobj(vn: Integer = - 1);
    procedure _putstream(const Adata: AnsiString); virtual;
    procedure _putstreamobject(const Adata: AnsiString);
    procedure _putlinks(const APage: Integer);
    procedure _putpage(const APage: Integer);
    procedure _putpages;
    procedure _putfonts;
    function _tounicodecmap(uv1, uv2: TFPDFFontInfo): String;
    procedure _putimages;
    procedure _putimage(var img: TFPDFImageInfo);
    procedure _putxobjectdict;
    procedure _putresourcedict(); virtual;
    procedure _putresources; virtual;
    procedure _putinfo;
    procedure _putcatalog; virtual;
    procedure _putheader;
    procedure _puttrailer; virtual;
    procedure _enddoc; virtual;

    function GzCompress(const StrIn: AnsiString; CompLevel: TCompressionLevel = clMax): AnsiString;
    function GzDecompress(const StrIn: AnsiString): AnsiString;
    procedure InternalCreate; virtual;

  public
    constructor Create(AOrientation: TFPDFOrientation = poPortrait;
      APageUnit: TFPDFUnit = puMM; APageFormat: TFPDFPageFormat = pfA4); overload;
    constructor Create(AOrientation: TFPDFOrientation; APageUnit: TFPDFUnit;
      APageSize: TFPDFPageSize); overload;
    destructor Destroy; override;

    procedure SetMargins(marginLeft: Double; marginTop: Double; marginRight: Double = -1);
    procedure SetLeftMargin(marginLeft: Double);
    procedure SetTopMargin(marginTop: Double);
    procedure SetRightMargin(marginRight: Double);
    procedure SetAutoPageBreak(AAuto: Boolean; AMargin: Double = 0.0);
    procedure SetDisplayMode(AZoomMode: TFPDFZoomMode; ALayoutMode: TFPDFLayoutMode = lmDefault; AZoomFactor: smallint = 0);
    procedure SetCompression(ACompress: Boolean);
    procedure SetTitle(const ATitle: String);
    procedure SetAuthor(const AAuthor: String);
    procedure SetSubject(const ASubject: String);
    procedure SetKeywords(const AKeywords: String);
    procedure SetCreator(const ACreator: String);
    procedure SetAliasNbPages(const AAlias: String = '{nb}');

    procedure SetTimeZone(const ATimeZone: String = 'Z');
    procedure SetUTF8(mode: Boolean = True);

    procedure Error(const ATextMsg: String; E: Exception = nil);
    procedure Close;

    procedure AddPage; overload;
    procedure AddPage(vOrientation: TFPDFOrientation); overload;
    procedure AddPage(AOrientation: TFPDFOrientation; ASize: TFPDFPageSize; ARotation: TFPDFRotation); overload;
    procedure Header; virtual;
    procedure Footer; virtual;
    function PageNo: Integer;

    procedure SetDrawColor(color: TFPDFColor); overload;
    procedure SetDrawColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetFillColor(color: TFPDFColor); overload;
    procedure SetFillColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetTextColor(color: TFPDFColor); overload;
    procedure SetTextColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetUnderline(fUnderline: Boolean = False);

    function GetStringWidth(const vText: String): Double;
    procedure SetLineWidth(vWidth: Double);

    procedure Line(vX1, vY1, vX2, vY2: Double);
    procedure Rect(vX, vY, vWidht, vHeight: Double; const vStyle: String = '');

    procedure AddFont(AFamily: String; AStyle: String = ''; AFile: String = '');
    procedure SetFont(const AFamily: String; const AStyle: String = ''; ASize: Double = 0.0);
    procedure SetFontSize(ASize: Double; fUnderline: Boolean = False);

    function AddLink: Integer;
    procedure SetLink(nLink: Integer; vY: Double = 0; vPage: Integer = -1);
    procedure Link(vX, vY: Double; vWidth, vHeight: Double; vLink: String);

    procedure Text(vX, vY: Double; const vText: String);
    function AcceptPageBreak: Boolean; virtual;
    procedure Cell(vWidth: Double; vHeight: Double = 0; const vText: String = '';
      const vBorder: String = '0'; vLineBreak: Integer = 0; const vAlign: String = '';
      vFill: Boolean = False; vLink: String = ''); virtual;
    procedure MultiCell(vWidth, vHeight: Double; const vText: String;
      const vBorder: String = '0'; const vAlign: String = 'J'; vFill: Boolean = False;
      AIndent: double = 0);
    procedure Write(vHeight: Double; const vText: String; const vLink: String = '');
    procedure Ln(vHeight: Double = 0);

    procedure Image(const vFile: String; vX: Double = -9999; vY: Double = -9999;
      vWidth: Double = 0; vHeight: Double = 0; const vLink: String = ''); overload; virtual;
    procedure Image(vImageStream: TStream; const vImageExt: String;
      vX: Double = -9999; vY: Double = -9999; vWidth: Double = 0;
      vHeight: Double = 0; const vLink: String = ''); overload;

    function GetPageWidth: Double;
    function GetPageHeight: Double;
    function GetX: Double;
    procedure SetX(vX: Double);
    function GetY: Double;
    procedure SetY(vY: Double; ResetX: Boolean = True);
    procedure SetXY(vX, vY: Double);

    procedure SaveToFile(const vFile: String);
    function SaveToString: AnsiString;
    procedure SaveToStream(AStream: TStream);
    procedure CreateContentStream(AStream: TStream; cs: TFPDFContentStream = csToViewBrowser; const AFileName: String = '');
  end;

const
  CFontEncodeStr: array[TFPDFFontEncode] of shortstring = ('', 'cp1252');
  CFontType: array[TFPDFFontType] of shortstring =('Core', 'TrueType', 'Type1');
  CPDFRotation: array[TFPDFRotation] of Integer = (0, 90, 180, 270);
  cUNIT: array[TFPDFUnit] of Double = (1, (72 / 25.4), (72 / 2.54), 72, 0.75);
  cCOLOR: array[TFPDFColor] of array [0..2] of smallint = (
   (000, 000, 000), (192, 192, 192), (128, 128, 128), (255, 255, 255),
   (128, 000, 000), (255, 000, 000), (128, 000, 128), (255, 000, 255),
   (000, 128, 000), (000, 255, 000), (128, 128, 000), (255, 255, 000),
   (000, 000, 128), (000, 000, 255), (000, 128, 128), (000, 255, 255),
   (220, 220, 220) );

function SwapBytes(Value: Cardinal): Cardinal; overload;
function SwapBytes(Value: Word): Word; overload;
function Split(const AString: string; const ADelimiter: string = ' '): TStringArray;
function CountStr(const AString, SubStr : String ) : Integer ;

var
  FPDFFormatSetings: TFormatSettings;

implementation

uses
  StrUtils, Math;

{ TFPDFFont }

procedure TFPDFFont.SetFontName(AValue: String);
begin
  if (fFontName = AValue) then
    Exit;

  fFontName := AValue;
  if (fName = '') then
    fName := AValue;
end;

procedure TFPDFFont.SetName(AValue: String);
begin
  if (fName = AValue) then
    Exit;

  fName := AValue;
  if (fFontName = '') then
    fFontName := AValue;
end;

constructor TFPDFFont.Create;
begin
  inherited;
  fName := '';
  fFontName := '';
  fFontType := ftCore;
  fup := -100;
  fut := 50;
  fenc := encCP1252;
  fsubsetted := False;
  fdiff := '';
  fill(fcw, 600);
  fill(fuv1, -1);
  fill(fuv2, -1);

  fuv1[000] := 0;      fuv2[000] := 128;
  fuv1[128] := 8364;
  fuv1[130] := 8218;
  fuv1[131] := 402;
  fuv1[132] := 8222;
  fuv1[133] := 8230;
  fuv1[134] := 8224;   fuv2[134] := 2;
  fuv1[136] := 710;
  fuv1[137] := 8240;
  fuv1[138] := 352;
  fuv1[139] := 8249;
  fuv1[140] := 338;
  fuv1[142] := 381;
  fuv1[145] := 8216;   fuv2[145] := 2;
  fuv1[147] := 8220;   fuv2[147] := 2;
  fuv1[149] := 8226;
  fuv1[150] := 8211;   fuv2[150] := 2;
  fuv1[152] := 732;
  fuv1[153] := 8482;
  fuv1[154] := 353;
  fuv1[155] := 8250;
  fuv1[156] := 339;
  fuv1[158] := 382;
  fuv1[159] := 376;
  fuv1[160] := 160;    fuv2[160] := 96;
end;

procedure TFPDFFont.fill(var AFontInfo: TFPDFFontInfo; AValue: Integer);
var
  j: Integer;
begin
  for j := Low(TFPDFFontInfo) to High(TFPDFFontInfo) do
    AFontInfo[j] := AValue;
end;

{ TFPDFFonts }

function TFPDFFonts.GetFont(const AFontName: String): TFPDFFont;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if LowerCase(Item[i].FontName) = LowerCase(AFontName) then
    begin
      Result := Item[i];
      Break;
    end;
  end;
end;

procedure TFPDFFonts.SetObject(Index: Integer; Item: TFPDFFont);
begin
  inherited Items[Index] := Item;
end;

function TFPDFFonts.GetObject(Index: Integer): TFPDFFont;
begin
  Result := TFPDFFont(inherited Items[Index]);
end;

function TFPDFFonts.New: TFPDFFont;
begin
  Result := TFPDFFont.Create;
  Add(Result);
end;

function TFPDFFonts.Add(Obj: TFPDFFont): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TFPDFFonts.Insert(Index: Integer; Obj: TFPDFFont);
begin
  inherited Insert(Index, Obj);
end;

{ TStringListList }

procedure TStringListList.SetObject(Index: Integer; Item: TStringList);
begin
  inherited Items[Index] := Item;
end;

function TStringListList.GetObject(Index: Integer): TStringList;
begin
  Result := TStringList(inherited Items[Index]);
end;

function TStringListList.New: TStringList;
begin
  Result := TStringList.Create;
  Add(Result);
end;

function TStringListList.Add(Obj: TStringList): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TStringListList.Insert(Index: Integer; Obj: TStringList);
begin
  inherited Insert(Index, Obj);
end;

{ TFPDF }

{%region Constructor/Destructor}

constructor TFPDF.Create(AOrientation: TFPDFOrientation; APageUnit: TFPDFUnit;
  APageFormat: TFPDFPageFormat);
var
  APageSize: TFPDFPageSize;
begin
  //Scale factor
  Self.k := cUNIT[APageUnit];
  // Page sizes
  DefineDefaultPageSizes;

  APageSize := _getpagesize(APageFormat);
  Create(AOrientation, APageUnit, APageSize);
end;

constructor TFPDF.Create(AOrientation: TFPDFOrientation; APageUnit: TFPDFUnit;
  APageSize: TFPDFPageSize);
var
  margin: Double;
  s: String;
begin
  InternalCreate;

  // Initialization of properties
  Self.state := 0;
  Self.page := 0;
  Self.n := 2;
  Self.buffer := TMemoryStream.Create;
  Self.buffer.Position := 0;
  SetLength(Self.pages, 0);
  Self.PageInfo := TStringListList.Create(True);
  SetLength(Self.UsedFonts,0);
  SetLength(Self.FontFiles,0);
  Self.encodings := TStringList.Create;
  Self.cmaps := TStringList.Create;
  SetLength(Self.images,0);
  SetLength(Self.links,0);
  Self.InHeader := False;
  Self.InFooter := False;
  Self.lasth := 0;
  Self.FontFamily := '';
  Self.FontStyle := '';
  Self.FontSizePt := 12;
  Self.underline := false;
  Self.DrawColor := '0 G';
  Self.FillColor := '0 g';
  Self.TextColor := '0 g';
  Self.ColorFlag := False;
  Self.WithAlpha := False;
  Self.ws := 0;

  // Font path
  Self.fontpath := '';
  s := GetEnvironmentVariable('FPDF_FONTPATH');
  if (s <> '') then
    Self.fontpath := IncludeTrailingPathDelimiter(s)
  else
  begin
    s := ExtractFilePath(ParamStr(0))+PathDelim+'font';
    if DirectoryExists(s) then
      Self.fontpath := s
  end;

  // Core Fonts
  Self.CurrentFont := Nil;
  Self.Fonts := TFPDFFonts.Create(True);
  PopulateCoreFonts;

  //Scale factor
  Self.k := cUNIT[APageUnit];
  // Page sizes
  DefineDefaultPageSizes;

  Self.DefPageSize.w := APageSize.w;
  Self.DefPageSize.h := APageSize.h;
  Self.CurPageSize.w := APageSize.w;
  Self.CurPageSize.h := APageSize.h;

  //Page AOrientation
  if (AOrientation = poDefault) then
    AOrientation := poPortrait;

  if (AOrientation = poLandscape) then
  begin
    Self.w := APageSize.h;
    Self.h := APageSize.w;
  end
  else
  begin
    Self.w := APageSize.w;
    Self.h := APageSize.h;
  end;

  Self.DefOrientation := AOrientation;
  Self.CurOrientation := Self.DefOrientation;
  Self.wPt := Self.w*Self.k;
  Self.hPt := Self.h*Self.k;

  // Page rotation
  Self.CurRotation := ro0;
  // Page margins (1 cm)
  margin := 28.35/Self.k;
  SetMargins(margin,margin);
  // Interior cell margin (1 mm)
  Self.cMargin := margin/10;
  // Line width (0.2 mm)
  Self.LineWidth := 0.567/Self.k;
  // Automatic page break
  SetAutoPageBreak(true, 2*margin);
  // Default display mode
  SetDisplayMode(zmDefault);
  // Enable compression
  SetCompression(True);
  // Metadata
  Self.metadata := TStringList.Create;
  Self.metadata.Values['Producer'] := 'FPDF Pascal '+FPDF_VERSION;
  // Set default PDF version number
  Self.PDFVersion := 1.3;

  Self.FontSize := Self.FontSizePt/Self.k;
  Self.UseUTF8 := {$IfDef USE_UTF8}True{$Else}False{$EndIf};
  Self.TimeZone := 'Z';
  Self.ZoomFactor := 0;
end;

procedure TFPDF.InternalCreate;
begin
  {}
end;

destructor TFPDF.Destroy;
begin
  Self.buffer.Free;
  Self.PageInfo.Free;
  Self.encodings.Free;
  Self.cmaps.Free;
  Self.metadata.Free;
  Self.Fonts.Free;
  inherited Destroy;
end;

{%endregion}

procedure TFPDF.SetMargins(marginLeft: Double; marginTop: Double; marginRight: Double);
begin
  //Set left and top margins
  Self.lMargin := marginLeft;
  Self.tMargin := marginTop;
  if (marginRight = -1) then
    Self.rMargin := Self.lMargin
  else
    Self.rMargin := marginRight;
end;

procedure TFPDF.SetLeftMargin(marginLeft: Double);
begin
  //Set left margin
  Self.lMargin := marginLeft;
  if ((Self.page > 0) and (Self.x < marginLeft)) then
    Self.x := marginLeft;
end;

procedure TFPDF.SetTopMargin(marginTop: Double);
begin
  //Set top margin
  Self.tMargin := marginTop;
end;

procedure TFPDF.SetRightMargin(marginRight: Double);
begin
  //Set right margin
  Self.rMargin := marginRight;
end;

procedure TFPDF.SetAutoPageBreak(AAuto: Boolean; AMargin: Double);
begin
  //Set auto page break mode and triggering margin
  Self.AutoPageBreak := AAuto;
  Self.bMargin := AMargin;
  Self.PageBreakTrigger := Self.h - AMargin;
end;

procedure TFPDF.SetDisplayMode(AZoomMode: TFPDFZoomMode; ALayoutMode: TFPDFLayoutMode; AZoomFactor: smallint);
begin
  Self.ZoomMode := AZoomMode;
  Self.LayoutMode := ALayoutMode;
  Self.ZoomFactor := AZoomFactor;
end;

procedure TFPDF.SetCompression(ACompress: Boolean);
begin
  //Set page compression
  Self.compress := ACompress;
end;

procedure TFPDF.SetTitle(const ATitle: String);
begin
  //Title of document
  Self.metadata.Values['Title'] := _EncodeText(ATitle);
end;

procedure TFPDF.SetAuthor(const AAuthor: String);
begin
  //Author of document
  Self.metadata.Values['Author'] := _EncodeText(AAuthor);
end;

procedure TFPDF.SetSubject(const ASubject: String);
begin
  //Subject of document
  Self.metadata.Values['Subject'] := _EncodeText(ASubject);
end;

procedure TFPDF.SetKeywords(const AKeywords: String);
begin
  //Keywords of document
  Self.metadata.Values['Keywords'] := _EncodeText(AKeywords);
end;

procedure TFPDF.SetCreator(const ACreator: String);
begin
  //Creator of document
  Self.metadata.Values['Creator'] := _EncodeText(ACreator);
end;

procedure TFPDF.SetAliasNbPages(const AAlias: String);
begin
  //Define an alias for total number of pages
  Self.AliasNbPages := AAlias;
end;

procedure TFPDF.SetTimeZone(const ATimeZone: String);
var
  hr, mi: Integer;
  Err: Boolean;
begin
  if UpperCase(ATimeZone) <> 'Z' then
  begin
    Err := (Length(ATimeZone) <> 6) or
           ((ATimeZone[1] = '-') or (ATimeZone[1] = '+')) or
           (not (ATimeZone[4] = ':'));

    if not Err then
    begin
      hr := StrToIntDef(copy(ATimeZone,1,3), -99);
      mi := StrToIntDef(copy(ATimeZone,5,2), -99);
      Err := ((hr < -11) or (hr > 14)) or ((mi < 0) or (mi > 60));
    end;

    if Err then
      Error('Invalid TimeZone: '+ATimeZone);
  end;

  Self.TimeZone := UpperCase(ATimeZone);
end;

procedure TFPDF.SetUTF8(mode: Boolean);
begin
  Self.UseUTF8 := mode;
end;

procedure TFPDF.Error(const ATextMsg: String; E: Exception);
var
  s: String;
begin
  //Fatal error
  s := ATextMsg;
  if Assigned(E) then
    s := s + sLineBreak + E.Message;

  raise EFPDFError.Create('FPDF error: ' + s);
end;

procedure TFPDF.Close;
begin
  // Terminate document
  if (Self.state = 3) then
    Exit;

  if (Self.page = 0) then
    AddPage(Self.DefOrientation, Self.DefPageSize, ro0);

  // Page footer
  Self.InFooter := True;
  try
    Footer();
  finally
    Self.InFooter := False;
  end;
  // Close page
  _endpage;
  // Close document
  _enddoc;
end;

procedure TFPDF.AddPage;
begin
  AddPage(Self.CurOrientation, Self.CurPageSize, Self.CurRotation);
end;

procedure TFPDF.AddPage(vOrientation: TFPDFOrientation);
begin
  AddPage(vOrientation, Self.CurPageSize, Self.CurRotation);
end;

procedure TFPDF.AddPage(AOrientation: TFPDFOrientation; ASize: TFPDFPageSize; ARotation: TFPDFRotation);
var
  vdc, vfc, vtc, vfamily, vstyle: String;
  vFontSize, vlw: Double;
  vcf, vunder: Boolean;
begin
  // Start a new page
  if (Self.state=3) then
    Error('The document is closed');

  vfamily := Self.FontFamily;
  vstyle := Self.FontStyle;
  vunder := Self.underline;
  vFontSize := Self.FontSizePt;
  vlw := Self.LineWidth;
  vdc := Self.DrawColor;
  vfc := Self.FillColor;
  vtc := Self.TextColor;
  vcf := Self.ColorFlag;

  if (Self.page > 0) then
  begin
    // Page footer
    Self.InFooter := True;
    try
      Footer;
    finally
      Self.InFooter := False;
    end;
    // Close page
    _endpage();
  end;

  //Start new page
  _beginpage(AOrientation, ASize, ARotation);
  //Set line cap style to square
  _out('2 J');
  //Set line width
  Self.LineWidth := vlw;
  _out(Format('%.2f', [vlw*Self.k], FPDFFormatSetings) + ' w');

  //Set font
  if (vfamily <> '') then
    SetFont(vfamily, vstyle, vFontSize);

  //Set colors
  Self.DrawColor := vdc;
  if (vdc <> '0 G') then
    _out(vdc);

  Self.FillColor := vfc;
  if (vfc <> '0 g') then
    _out(vfc);

  Self.TextColor := vtc;
  Self.ColorFlag := vcf;

  //Page header
  Self.InHeader := True;
  try
    Header;
  finally
    Self.InHeader := False;
  end;

  //Restore line width
  if (Self.LineWidth <> vlw) then
  begin
    Self.LineWidth := vlw;
    _out(Format('%.2f', [vlw*Self.k], FPDFFormatSetings) + ' w');
  end;

  //Restore font
  if (vfamily <> '') then
    SetFont(vfamily, vstyle, vFontSize);

  //Restore colors
  if (Self.DrawColor <> vdc) then
  begin
    Self.DrawColor := vdc;
    _out(vdc);
  end;

  if (Self.FillColor <> vfc) then
  begin
    Self.FillColor := vfc;
    _out(vfc);
  end;

  Self.TextColor := vtc;
  Self.ColorFlag := vcf;
  Self.underline := vunder;
end;

procedure TFPDF.Header;
begin
  // Implementing an inheritance, if necessary
end;

procedure TFPDF.Footer;
begin
  // Implementing an inheritance, if necessary
end;

function TFPDF.PageNo: Integer;
begin
  //Get current page number
  Result := Self.page;
end;

procedure TFPDF.SetDrawColor(color: TFPDFColor);
begin
  SetDrawColor(cCOLOR[color][0], cCOLOR[color][1], cCOLOR[color][2]);
end;


procedure TFPDF.SetDrawColor(ValR: Integer; ValG: Integer; ValB: Integer);
begin
  //Set color for all stroking operations
  if (((ValR = 0) and (ValG = 0) and (ValB = 0)) or (ValG < 0)) then
    Self.DrawColor := Format('%.3f', [ValR / 255], FPDFFormatSetings)+' G'
  else
    Self.DrawColor := Format('%.3f %.3f %.3f RG', [ValR / 255, ValG / 255, ValB / 255], FPDFFormatSetings);

  if (Self.page > 0) then
    _out(Self.DrawColor);
end;

procedure TFPDF.SetFillColor(color: TFPDFColor);
begin
  SetFillColor(cCOLOR[color][0], cCOLOR[color][1], cCOLOR[color][2]);
end;

procedure TFPDF.SetFillColor(ValR: Integer; ValG: Integer; ValB: Integer);
begin
  //Set color for all stroking operations
  if (((ValR = 0) and (ValG = 0) and (ValB = 0)) or (ValG < 0)) then
    Self.FillColor := Format('%.3f g', [ValR / 255], FPDFFormatSetings)
  else
    Self.FillColor := Format('%.3f %.3f %.3f rg', [ValR / 255, ValG / 255, ValB / 255], FPDFFormatSetings);

  Self.ColorFlag := (Self.FillColor <> Self.TextColor);

  if (Self.page > 0) then
    _out(Self.FillColor);
end;

procedure TFPDF.SetTextColor(color: TFPDFColor);
begin
  SetTextColor(cCOLOR[color][0], cCOLOR[color][1], cCOLOR[color][2]);
end;

procedure TFPDF.SetTextColor(ValR: Integer; ValG: Integer; ValB: Integer);
begin
  //Set color for text
  if (((ValR = 0) and (ValG = 0) and (ValB = 0)) or (ValG < 0)) then
    Self.TextColor := Format('%.3f g', [ValR / 255], FPDFFormatSetings)
  else
    Self.TextColor := Format('%.3f %.3f %.3f rg', [ValR / 255, ValG / 255, ValB / 255], FPDFFormatSetings);

  Self.ColorFlag := (Self.FillColor <> Self.TextColor);
end;

procedure TFPDF.SetUnderline(fUnderline: Boolean);
begin
  Self.underline := fUnderline;
end;

function TFPDF.GetStringWidth(const vText: String): Double;
var
  cw: TFPDFFontInfo;
  vw, l, i: Integer;
begin
  // Get width of a string in the current font
  Result := 0;
  if not Assigned(Self.CurrentFont) then
    Exit;

  cw := Self.CurrentFont.cw;
  vw := 0;
  l := Length(vText);
  for i := 1 to l do
    vw := vw + cw[ord(vText[i])];

  Result := vw*Self.FontSize/1000;
end;

procedure TFPDF.SetLineWidth(vWidth: Double);
begin
  //Set line width
  Self.LineWidth := vWidth;
  if (Self.page > 0) then
    _out(Format('%.2f', [vWidth*Self.k], FPDFFormatSetings) + ' w');
end;

procedure TFPDF.Line(vX1, vY1, vX2, vY2: Double);
begin
  //Draw a line
  _out(Format('%.2f %.2f m %.2f %.2f l S', [vX1*Self.k, (Self.h-vY1)*Self.k, vX2*Self.k, (Self.h-vY2)*Self.k], FPDFFormatSetings));
end;

procedure TFPDF.Rect(vX, vY, vWidht, vHeight: Double; const vStyle: String);
var
  vop, s: String;
begin
  //Draw a rectangle
  s := UpperCase(vStyle);
  if (s = 'F') then
    vop := 'f'
  else if ((s = 'FD') or (s = 'DF')) then
    vop := 'B'
  else
    vop := 'S';

  _out(Format('%.2f %.2f %.2f %.2f re %s', [vX*Self.k, (Self.h-vY)*Self.k, vWidht*Self.k, -vHeight*Self.k, vop], FPDFFormatSetings));
end;

procedure TFPDF.AddFont(AFamily: String; AStyle: String; AFile: String);
begin
(* //TODO
	// Add a TrueType, OpenType or Type1 font
	$family = strtolower($family);
	if($file=='')
		$file = str_replace(' ','',$family).strtolower($style).'.php';
	$style = strtoupper($style);
	if($style=='IB')
		$style = 'BI';
	$fontkey = $family.$style;
	if(isset($this->UsedFonts[$fontkey]))
		return;
	$info = $this->_loadfont($file);
	$info['i'] = count($this->UsedFonts)+1;
	if(!empty($info['file']))
	{
		// Embedded font
		if($info['type']=='TrueType')
			$this->FontFiles[$info['file']] = array('length1'=>$info['originalsize']);
		else
			$this->FontFiles[$info['file']] = array('length1'=>$info['size1'], 'length2'=>$info['size2']);
	}
	$this->UsedFonts[$fontkey] = $info;
*)
end;

procedure TFPDF.SetFont(const AFamily: String; const AStyle: String; ASize: Double);
var
  vFamily, vStyle, vFontName, vStyleName: String;
  oFont: TFPDFFont;
  FontIndex: Integer;
begin
  // Select a font; size given in points
  if (Trim(AFamily)='') then
    vFamily := Self.FontFamily
  else
    vFamily := LowerCase(AFamily);

  vStyle := UpperCase(AStyle);
  if (pos('U', vStyle) > 0) then
  begin
    Self.underline := True;
    vStyle := StringReplace(vStyle, 'U', '', [rfReplaceAll]);
  end
  else
    Self.underline := False;

  if (vStyle = 'IB') then
    vStyle := 'BI';

  if (ASize = 0) then
    ASize := Self.FontSizePt;

  if (vFamily = 'arial') then
    vFamily := 'helvetica';

  if ((vFamily='symbol') or (vFamily='zapfdingbats')) then
    vStyle := '';

  // Test if font is already selected
  if ((Self.FontFamily=vFamily) and (Self.FontStyle=vStyle) and (Self.FontSizePt=ASize)) then
    Exit;

  // Test if font is already loaded
  vStyleName := IfThen(pos('B',vStyle) > 0, 'Bold', '') + IfThen(pos('I',vStyle) > 0, 'Oblique', '');
  vFontName := vFamily + IfThen(vStyleName<>'','-','') + vStyleName;
  oFont := Fonts.Font[vFontName];

  if (oFont = Nil) then
    Error('Undefined font: '+vFontName);

  // Select it
  Self.FontFamily := vFamily;
  Self.FontStyle := vStyle;
  Self.FontSizePt := ASize;
  Self.FontSize := ASize/Self.k;
  Self.CurrentFont := oFont;

  FontIndex := FindUsedFontIndex(vFontName);
  if (FontIndex < 1) then
  begin
    FontIndex := Length(Self.UsedFonts)+1;
    SetLength(Self.UsedFonts, FontIndex);
    Self.UsedFonts[FontIndex-1].FontName := vFontName;
  end;

  if (Self.page > 0) then
    _out(Format('BT /F%d %.2f Tf ET', [FontIndex, Self.FontSizePt], FPDFFormatSetings));
end;

procedure TFPDF.SetFontSize(ASize: Double; fUnderline: Boolean);
begin
  // Set font size in points
  if (Self.FontSizePt=ASize) then
    Exit;

  Self.FontSizePt := ASize;
  Self.FontSize := (ASize/Self.k);
  if ((Self.page > 0) and Assigned(Self.CurrentFont)) then
    _out(Format('BT /F%d %.2f Tf ET', [FindUsedFontIndex(Self.CurrentFont.Name), Self.FontSizePt], FPDFFormatSetings));
end;

function TFPDF.AddLink: Integer;
begin
  // Create a new internal link
  Result := Length(Self.links);
  SetLength(Self.links, Result+1);
  Self.links[Result].Page := 0;
  Self.links[Result].y := 0;
end;

procedure TFPDF.SetLink(nLink: Integer; vY: Double; vPage: Integer);
begin
  if (nLink < 0) or (nLink >= Length(Self.links)) then
    Error('Invalid Link Index: '+IntToStr(nLink));

  // Set destination of internal link
  if (vY = -1) then
    vY := Self.y;

  if (vPage < 0) then
    vPage := Self.page;

  if (vPage > Length(Self.pages)) then
    Error('Invalid Link Page: '+IntToStr(vPage));

  Self.links[nLink].Page := vPage;
  Self.links[nLink].y := vY;
end;

procedure TFPDF.Link(vX, vY: Double; vWidth, vHeight: Double; vLink: String);
var
  i, IdLink, p: Integer;
begin
  if (vLink = '') then
    Error('Empty Link');

  IdLink := StrToIntDef(vLink, -1);
  if (IdLink >= 0) then
    if (IdLink >= Length(Self.links)) then
      Error('Invalid Link Index: '+IntToStr(IdLink));

  // Put a link on the page
  p := Self.page-1;
  i := Length(Self.PageLinks[p]);
  SetLength(Self.PageLinks[p], i+1);
  Self.PageLinks[p][i].X := vx * Self.k;
  Self.PageLinks[p][i].Y := Self.hPt - vY * Self.k;
  Self.PageLinks[p][i].Width := vWidth * Self.k;
  Self.PageLinks[p][i].Height := vHeight * Self.k;
  Self.PageLinks[p][i].Link := vLink;
end;

procedure TFPDF.Text(vX, vY: Double; const vText: String);
var
  s, t: String;
begin
  // Output a string
  if not Assigned(Self.CurrentFont) then
     Error('No font has been set');

  t := ConvertTextToAnsi(vText);
  s := Format('BT %.2f %.2f Td (%s) Tj ET', [vX*Self.k, (Self.h-vY)*Self.k, _escape(t)], FPDFFormatSetings);
  if ( Self.underline and (t <> '') ) then
    s := s + ' ' + Self._dounderline(vX, vY, t);

  if Self.ColorFlag then
    s := 'q ' + Self.TextColor + ' ' + s + ' Q';

  _out(s);
end;

function TFPDF.AcceptPageBreak: Boolean;
begin
  //Accept automatic page break or not
  Result := Self.AutoPageBreak;
end;

procedure TFPDF.Cell(vWidth: Double; vHeight: Double; const vText: String;
  const vBorder: String; vLineBreak: Integer; const vAlign: String;
  vFill: Boolean; vLink: String);
var
  vk, vx, vy, vws, vdx: Double;
  s, vop, t: String;
begin
  // Output a cell
  vk := Self.k;

  if ((Self.y+vHeight > Self.PageBreakTrigger) and (not Self.InHeader) and (not Self.InFooter) and Self.AcceptPageBreak) then
  begin
    // Automatic page break
    vx := Self.x;
    vws := Self.ws;
    if (vws>0) then
    begin
      Self.ws := 0;
      _out('0 Tw');
    end;

    AddPage;
    Self.x := vx;
    if (vws > 0) then
    begin
      Self.ws := vws;
      _out(Format('%.3f Tw', [vws*vk], FPDFFormatSetings));
    end;
  end;

  if (vWidth = 0) then
    vWidth := Self.w-Self.rMargin-Self.x;

  s := '';
  if (vFill or (vBorder='1')) then
  begin
    if vFill then
      vop := IfThen((vBorder='1'), 'B', 'f')
    else
      vop := 'S';

    s := Format('%.2f %.2f %.2f %.2f re %s ', [Self.x*vk, (Self.h-Self.y)*vk, vWidth*vk, -vHeight*vk, vop], FPDFFormatSetings);
  end;

  vx := Self.x;
  vy := Self.y;

  if (pos('L', vBorder) > 0) then
    s := s + Format('%.2f %.2f m %.2f %.2f l S ', [vx*vk, (Self.h-vy)*vk, vx*vk, (Self.h-(vy+vHeight))*vk], FPDFFormatSetings);

  if (pos('T', vBorder) > 0) then
    s := s + Format('%.2f %.2f m %.2f %.2f l S ', [vx*vk, (Self.h-vy)*vk, (vx+vWidth)*vk, (Self.h-vy)*vk], FPDFFormatSetings);

  if (pos('R', vBorder) > 0) then
    s := s + Format('%.2f %.2f m %.2f %.2f l S ', [(vx+vWidth)*vk, (Self.h-vy)*vk, (vx+vWidth)*vk, (Self.h-(vy+vHeight))*vk], FPDFFormatSetings);

  if (pos('B', vBorder) > 0) then
    s := s + Format('%.2f %.2f m %.2f %.2f l S ', [vx*vk, (Self.h-(vy+vHeight))*vk, (vx+vWidth)*vk, (Self.h-(vy+vHeight))*vk], FPDFFormatSetings);

  if (vText <> '') then
  begin
    t := ConvertTextToAnsi(vText);
    if not Assigned(Self.CurrentFont) then
      Error('No font has been set');

    if (vAlign ='R') then
      vdx := vWidth-Self.cMargin-GetStringWidth(t)
    else if (vAlign ='C') then
      vdx := (vWidth-GetStringWidth(t))/2
    else
      vdx := Self.cMargin;

    if Self.ColorFlag then
      s := s + 'q ' + Self.TextColor + ' ';

    s := s + Format('BT %.2f %.2f Td (%s) Tj ET', [(Self.x+vdx)*vk, (Self.h-(Self.y+0.5*vHeight+0.3*Self.FontSize))*vk, _escape(t)], FPDFFormatSetings);
    if Self.underline then
      s := s + ' '+Self._dounderline(Self.x+vdx, Self.y+0.5*vHeight+0.3*Self.FontSize, t);

    if Self.ColorFlag then
      s := s + ' Q';

    if (vLink <> '') then
      Link( Self.x+vdx, Self.y+0.5*vHeight-0.5*Self.FontSize, GetStringWidth(t), Self.FontSize, vLink);
  end;

  if (s <> '') then
    _out(s);

  Self.lasth := vHeight;
  if (vLineBreak > 0) then
  begin
    // Go to next line
    Self.y := Self.y + vHeight;
    if (vLineBreak = 1) then
      Self.x := Self.lMargin;
  end
  else
    Self.x := Self.x + vWidth;
end;


procedure TFPDF.MultiCell(vWidth, vHeight: Double; const vText: String;
  const vBorder: String; const vAlign: String; vFill: Boolean; AIndent: double);
{ http://www.fpdf.org/en/script/script39.php }
var
  cw: TFPDFFontInfo;
  w, wFirst, wOther, wMax, wMaxFirst, wMaxOther, SaveX: Double;
  s, b, vb, b2: String;
  nb, sep, i, j, l, ns, nl, ls: Integer;
  c: Char;
  vUTF8, First: Boolean;
begin
  // Output text with automatic or explicit line breaks
  if not Assigned(Self.CurrentFont) then
    Error('No font has been set');

  vUTF8 := Self.UseUTF8;
  try
    cw := Self.CurrentFont.cw;
    if (vWidth=0) then
      vWidth := Self.w-Self.rMargin-Self.x;

    wFirst := vWidth - AIndent;
    wOther := vWidth;

    wMaxFirst := (wFirst-2*Self.cMargin)*1000/Self.FontSize;
    wMaxOther := (wOther-2*Self.cMargin)*1000/Self.FontSize;

    s := StringReplace(ConvertTextToAnsi(vText), CR, '', [rfReplaceAll]);
    Self.UseUTF8 := False;
    nb := Length(s);
    if ((nb>0) and (s[nb-1] = LF)) then
      Dec(nb);

    b := '0';
    vb := vBorder;
    if (vb <> '') then
    begin
      if (vb = '1') then
      begin
        vb := 'LTRB';
        b := 'LRT';
        b2 := 'LR';
      end
      else
      begin
        b2 := '';
        if (pos('L', vb) > 0) then
          b2 := b2 + 'L';
        if (pos('R', vb) > 0) then
          b2 := b2 + 'R';

        b := ifthen(pos('T', vb) > 0, b2+'T', b2);
      end;
    end;

    sep := -1;
    i := 1;
    j := 1;
    l := 0;
    ns := 0;
    ls := 0;
    nl := 1;
    First := True;
    while (i <= nb) do
    begin
      // Get next character
      c := s[i];
      if (c = LF) then
      begin
        // Explicit line break
        if (Self.ws > 0) then
        begin
          Self.ws := 0;
  	      _out('0 Tw');
        end;

        Cell(vWidth, vHeight, copy(s, j, i-j), b, 2, vAlign, vFill);
        Inc(i);
        sep := -1;
        j := i;
        l := 0;
        ns := 0;
        Inc(nl);
        if ((vb <> '') and (nl=2)) then
          b := b2;
        continue;
      end;

      if (c=' ') then
      begin
        sep := i;
        ls := l;
        Inc(ns);
      end;

      l := l + cw[ord(c)];

      if First then
      begin
        wMax := wMaxFirst;
        w := wFirst;
      end
      else
      begin
        wMax := wMaxOther;
        w := wOther;
      end;

      if (l> wMax) then
      begin
        // Automatic line break
        if (sep=-1) then
        begin
          if (i=j) then
  	        Inc(i);
  	      if(Self.ws > 0) then
          begin
  	        Self.ws := 0;
            _out('0 Tw');
          end;
          SaveX := Self.x;
          if First and (AIndent >0 ) then
          begin
            Self.SetX(Self.x + AIndent);
            First := False;
          end;
          Cell(vWidth, vHeight, copy(s, j, i-j), b, 2, vAlign, vFill);
          Self.SetX(SaveX);
        end
        else
        begin
          if (vAlign='J') then
          begin
            if (ns>1) then
              Self.ws := (wmax-ls)/1000*Self.FontSize/(ns-1)
            else
              Self.ws := 0;

            _out(Format('%.3f Tw', [Self.ws*Self.k], FPDFFormatSetings));
          end;
          SaveX := Self.x;
          if First and (AIndent >0 ) then
          begin
            Self.SetX(Self.x + AIndent);
            First := False;
          end;
          Cell( vWidth, vHeight, Copy(s, j, sep-j), b, 2, vAlign, vFill);
          Self.SetX(SaveX);
          i := sep+1;
        end;

        sep := -1;
        j := i;
        l := 0;
        ns := 0;
        Inc(nl);

        if ((vb<>'') and (nl=2)) then
          b := b2;
      end
      else
        Inc(i);
    end;

    // Last chunk
    if (Self.ws > 0) then
    begin
      Self.ws := 0;
      _out('0 Tw');
    end;

    if ((vb <> '') and (pos('B', vb) > 0)) then
      b := b + 'B';

    Cell(vWidth, vHeight, copy(s, j, i-j), b, 2, vAlign, vFill);
    Self.x := Self.lMargin;
  finally
    Self.UseUTF8 := vUTF8;
  end;
end;


procedure TFPDF.Write(vHeight: Double; const vText: String; const vLink: String
  );
var
  cw: TFPDFFontInfo;
  vw, wmax: Double;
  s: String;
  nb, sep, i, j, l, nl: Integer;
  c: Char;
  vUTF8: Boolean;
begin
  // Output text in flowing mode
  if not Assigned(Self.CurrentFont) then
    Error('No font has been set');

  vUTF8 := Self.UseUTF8;
  try
    cw := Self.CurrentFont.cw;
    vw := Self.w-Self.rMargin-Self.x;
    wmax := (vw-2*Self.cMargin)*1000/Self.FontSize;
    s := StringReplace(ConvertTextToAnsi(vText), CR, '', [rfReplaceAll]);
    Self.UseUTF8 := False;
    nb := Length(s);
    sep := -1;
    i := 1;
    j := 0;
    l := 0;
    nl := 1;
    while (i <= nb) do
    begin
      // Get next character
      c := s[i];
      if (c = LF) then
      begin
        // Explicit line break
        Cell(vw, vHeight, Copy(s, j, i-j), '0', 2, '', false, vLink);
        Inc(i);
        sep := -1;
        j := i;
        l := 0;
        if (nl = 1) then
        begin
          Self.x := Self.lMargin;
          vw := Self.w-Self.rMargin-Self.x;
          wmax := (vw-2*Self.cMargin)*1000/Self.FontSize;
        end;

        Inc(nl);
        continue;
      end;

      if (c = ' ') then
        sep := i;

      l := l + cw[Ord(c)];
      if (l > wmax) then
      begin
        // Automatic line break
        if (sep=-1) then
        begin
          if(Self.x > Self.lMargin) then
          begin
            // Move to next line
            Self.x := Self.lMargin;
            Self.y := Self.y + vHeight;
            vw := Self.w-Self.rMargin-Self.x;
            wmax := (vw-2*Self.cMargin)*1000/Self.FontSize;
            Inc(i);
            Inc(nl);
            continue;
          end;

          if (i = j) then
            inc(i);

  	      Cell(vw, vHeight, Copy(s, j, i-j), '0', 2, '', False, vLink);
        end
        else
        begin
          Cell(vw, vHeight, copy(s, j, sep-j), '0', 2, '', False, vLink);
  	      i := sep+1;
        end;

        sep := -1;
        j := i;
        l := 0;
        if (nl=1) then
        begin
          Self.x := Self.lMargin;
          vw := Self.w-Self.rMargin-Self.x;
          wmax := (vw-2*Self.cMargin)*1000/Self.FontSize;
        end;

        Inc(nl);
      end
      else
        Inc(i);
    end;

    // Last chunk
    if (i <> j) then
      Cell(l/1000*Self.FontSize, vHeight, copy(s, j, Length(s)), '0', 0, '', False, vLink);
  finally
    Self.UseUTF8 := vUTF8;
  end;
end;

procedure TFPDF.Ln(vHeight: Double);
begin
  //Line feed; default value is last cell height
  Self.x := Self.lMargin;
  if (vHeight <= 0) then
    Self.y := Self.y + Self.lasth
  else
    Self.y := Self.y + vHeight;
end;

procedure TFPDF.Image(const vFile: String; vX: Double; vY: Double;
  vWidth: Double; vHeight: Double; const vLink: String);
var
  l: Integer;
  img: TFPDFImageInfo;
  ext: String;
  ms: TMemoryStream;
begin
  //Put an image file on the page
  ext := StringReplace(UpperCase(ExtractFileExt(vFile)), '.', '', [rfReplaceAll]);
  if (ext = '') then
    Error('Image File without an extension!');

  img := FindUsedImage(vFile);
  if (img.data = '') then
  begin
    ms := TMemoryStream.Create;
    try
      ms.LoadFromFile(vFile);
      //First use of image, get info
      l := Length(Self.images);
      SetLength(Self.images, l + 1);
      Self.images[l] := _parseimage(ms, ext);
      Self.images[l].i := l+1;
      Self.images[l].ImageName := vFile;
      img := Self.images[l];
    finally
      ms.Free;
    end;
  end;

  Image(img, vX, vY, vWidth, vHeight, vLink);
end;

procedure TFPDF.Image(vImageStream: TStream; const vImageExt: String;
  vX: Double; vY: Double; vWidth: Double; vHeight: Double; const vLink: String);
var
  l: Integer;
begin
  //Put an image Stream on the page
  l := Length(Self.images);
  SetLength(Self.images, l+1);
  Self.images[l] := _parseimage(vImageStream, vImageExt);
  Self.images[l].i := l+1;
  Self.images[l].ImageName := '';

  Image(Self.images[l], vX, vY, vWidth, vHeight);
end;

function TFPDF.GetPageWidth: Double;
begin
  // Get current page width
  Result := Self.w;
end;

function TFPDF.GetPageHeight: Double;
begin
  // Get current page height
  Result := Self.h;
end;

function TFPDF.GetX: Double;
begin
  //Get x position
  Result := Self.x;
end;

procedure TFPDF.SetX(vX: Double);
begin
  //Set x position
  if (vX >= 0) then
    Self.x := vX
  else
    Self.x := Self.w + vX;
end;

function TFPDF.GetY: Double;
begin
  //Get y position
  Result := Self.y;
end;

procedure TFPDF.SetY(vY: Double; ResetX: Boolean);
begin
  //Set y position and reset x
  if (vY >= 0) then
    Self.y := vY
  else
    Self.y := Self.h + vY;

  if ResetX then
    Self.x := Self.lMargin;
end;

procedure TFPDF.SetXY(vX, vY: Double);
begin
  //Set x and y positions
  SetX(vX);
  SetY(vY, False);
end;

procedure TFPDF.SaveToFile(const vFile: String);
begin
  //Save file locally
  Close;
  try
    Self.buffer.SaveToFile(vFile);
  except
    on E: Exception do
      Error('Unable to create output file: ' + vFile, E);
  end;
end;

function TFPDF.SaveToString: AnsiString;
begin
  //Save to String
  Close;
  Result := '';
  try
    Self.buffer.Position := 0;
    SetLength(Result, Self.buffer.Size);
    Self.buffer.Read(Pointer(Result)^, Self.buffer.Size);
  except
    On E: Exception do
      Error('Unable to save to String', E);
  end;
end;

procedure TFPDF.SaveToStream(AStream: TStream);
begin
  //Save to stream
  Close;
  try
    Self.buffer.Position := 0;
    AStream.Seek(0, soEnd);
    AStream.CopyFrom(Self.buffer, Self.buffer.Size);
  except
    On E: Exception do
      Error('Unable to save to stream', E);
  end;
end;

procedure TFPDF.CreateContentStream(AStream: TStream; cs: TFPDFContentStream;
  const AFileName: String);
var
  cth, f: String;
begin
  Close;

  if (AFileName = '') then
    f := 'doc.pdf'
  else
    f := ExtractFileName(AFileName);

  try
    case cs of
      csToViewBrowser:
        cth := 'Content-Type: application/pdf' + sLineBreak +
               'Content-Disposition: inline; filename="'+f+'"' + sLineBreak;

      else  // csToDownload:
        cth := 'Content-Type: application/x-download' + sLineBreak +
               'Content-Disposition: attachment; filename="'+f+'"' + sLineBreak;
    end;

    cth := cth + 'Cache-Control: private, max-age=0, must-revalidate' + sLineBreak;
    cth := cth + 'Pragma: public' + sLineBreak;

    // Write headers
    AStream.Size := 0;
    AStream.Write(Pointer(cth)^, Length(cth) * SizeOf(char));

    // Now Save PDF data...
    SaveToStream(AStream);
  except
    On E: Exception do
      Error('Unable to Create Content Stream', E);
  end;
end;

procedure TFPDF.PopulateCoreFonts;
const
    CW_HELVETICA: TFPDFFontInfo = (
      278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,
      278,278,278,278,278,278,278,278,278,278,278,278,355,556,556,889,667,191,333,333,389,584,
      278,333,278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,584,556,1015,667,
      667,722,722,667,611,778,722,278,500,667,556,833,722,778,667,778,722,667,611,722,667,944,
      667,667,611,278,278,278,469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
      556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,584,350,556,350,222,556,
      333,1000,556,556,333,1000,667,333,1000,350,611,350,350,222,222,333,333,350,556,1000,333,
      1000,500,333,944,350,500,667,278,333,556,556,556,556,260,556,333,737,370,556,584,333,737,
      333,400,584,333,333,333,556,537,278,333,333,365,556,834,834,834,611,667,667,667,667,667,
      667,1000,722,667,667,667,667,278,278,278,278,722,722,778,778,778,778,778,584,778,722,722,
      722,722,667,667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,278,278,556,
      556,556,556,556,556,556,584,611,556,556,556,556,500,556,500);

    CW_HELVETICA_BOLD: TFPDFFontInfo = (
      278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,
      278,278,278,278,278,278,278,278,278,278,278,333,474,556,556,889,722,238,333,333,389,584,
      278,333,278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,584,611,975,722,
      722,722,722,667,611,778,722,278,556,722,611,833,722,778,667,778,722,667,611,722,667,944,
      667,667,611,333,278,333,584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
      611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,584,350,556,350,278,556,
      500,1000,556,556,333,1000,667,333,1000,350,611,350,350,278,278,500,500,350,556,1000,333,1000,
      556,333,944,350,500,667,278,333,556,556,556,556,280,556,333,737,370,556,584,333,737,333,
      400,584,333,333,333,611,556,278,333,333,365,556,834,834,834,611,722,722,722,722,722,722,
      1000,722,667,667,667,667,278,278,278,278,722,722,778,778,778,778,778,584,778,722,722,722,
      722,667,667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,278,278,611,611,
      611,611,611,611,611,584,611,611,611,611,611,556,611,556);

    CW_HELVETICA_OBLIQUE: TFPDFFontInfo = (
      278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,
      278,278,278,278,278,278,278,278,278,278,278,278,355,556,556,889,667,191,333,333,389,584,
      278,333,278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,584,556,1015,667,
      667,722,722,667,611,778,722,278,500,667,556,833,722,778,667,778,722,667,611,722,667,944,
      667,667,611,278,278,278,469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
      556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,584,350,556,350,222,556,
      333,1000,556,556,333,1000,667,333,1000,350,611,350,350,222,222,333,333,350,556,1000,333,1000,
      500,333,944,350,500,667,278,333,556,556,556,556,260,556,333,737,370,556,584,333,737,333,
      400,584,333,333,333,556,537,278,333,333,365,556,834,834,834,611,667,667,667,667,667,667,
      1000,722,667,667,667,667,278,278,278,278,722,722,778,778,778,778,778,584,778,722,722,722,
      722,667,667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,278,278,556,556,
      556,556,556,556,556,584,611,556,556,556,556,500,556,500);

    CW_HELVETICA_BOLD_OBLIQUE: TFPDFFontInfo = (
      278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,278,
      278,278,278,278,278,278,278,278,278,278,278,333,474,556,556,889,722,238,333,333,389,584,
      278,333,278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,584,611,975,722,
      722,722,722,667,611,778,722,278,556,722,611,833,722,778,667,778,722,667,611,722,667,944,
      667,667,611,333,278,333,584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
      611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,584,350,556,350,278,556,
      500,1000,556,556,333,1000,667,333,1000,350,611,350,350,278,278,500,500,350,556,1000,333,1000,
      556,333,944,350,500,667,278,333,556,556,556,556,280,556,333,737,370,556,584,333,737,333,
      400,584,333,333,333,611,556,278,333,333,365,556,834,834,834,611,722,722,722,722,722,722,
      1000,722,667,667,667,667,278,278,278,278,722,722,778,778,778,778,778,584,778,722,722,722,
      722,667,667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,278,278,611,611,
      611,611,611,611,611,584,611,611,611,611,611,556,611,556);

    CW_TIMES_ROMAN: TFPDFFontInfo = (
      250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,
      250,250,250,250,250,250,250,250,250,250,250,333,408,500,500,833,778,180,333,333,500,564,
      250,333,250,278,500,500,500,500,500,500,500,500,500,500,278,278,564,564,564,444,921,722,
      667,667,722,611,556,722,722,333,389,722,611,889,722,722,556,722,667,556,611,722,722,944,
      722,722,611,333,278,333,469,500,333,444,500,444,500,444,333,500,500,278,278,500,278,778,
      500,500,500,500,333,389,278,500,500,722,500,500,444,480,200,480,541,350,500,350,333,500,
      444,1000,500,500,333,1000,556,333,889,350,611,350,350,333,333,444,444,350,500,1000,333,980,
      389,333,722,350,444,722,250,333,500,500,500,500,200,500,333,760,276,500,564,333,760,333,
      400,564,300,300,333,500,453,250,333,300,310,500,750,750,750,444,722,722,722,722,722,722,
      889,667,611,611,611,611,333,333,333,333,722,722,722,722,722,722,722,564,722,722,722,722,
      722,722,556,500,444,444,444,444,444,444,667,444,444,444,444,444,278,278,278,278,500,500,
      500,500,500,500,500,564,500,500,500,500,500,500,500,500);

    CW_TIMES_BOLD: TFPDFFontInfo = (
      250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,
      250,250,250,250,250,250,250,250,250,250,250,333,555,500,500,1000,833,278,333,333,500,570,
      250,333,250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,570,500,930,722,
      667,722,722,667,611,778,778,389,500,778,667,944,722,778,611,778,722,556,667,722,722,1000,
      722,722,667,333,278,333,581,500,333,500,556,444,556,444,333,500,556,278,333,556,278,833,
      556,500,556,556,444,389,333,556,500,722,500,500,444,394,220,394,520,350,500,350,333,500,
      500,1000,500,500,333,1000,556,333,1000,350,667,350,350,333,333,500,500,350,500,1000,333,1000,
      389,333,722,350,444,722,250,333,500,500,500,500,220,500,333,747,300,500,570,333,747,333,
      400,570,300,300,333,556,540,250,333,300,330,500,750,750,750,500,722,722,722,722,722,722,
      1000,722,667,667,667,667,389,389,389,389,722,722,778,778,778,778,778,570,778,722,722,722,
      722,722,611,556,500,500,500,500,500,500,722,444,444,444,444,444,278,278,278,278,500,556,
      500,500,500,500,500,570,500,556,556,556,556,500,556,500);

    CW_TIMES_ITALIC: TFPDFFontInfo = (
      250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,
      250,250,250,250,250,250,250,250,250,250,250,333,420,500,500,833,778,214,333,333,500,675,
      250,333,250,278,500,500,500,500,500,500,500,500,500,500,333,333,675,675,675,500,920,611,
      611,667,722,611,611,722,722,333,444,667,556,833,667,722,611,722,611,500,556,722,611,833,
      611,556,556,389,278,389,422,500,333,500,500,444,500,444,278,500,500,278,278,444,278,722,
      500,500,500,500,389,389,278,500,444,667,444,444,389,400,275,400,541,350,500,350,333,500,
      556,889,500,500,333,1000,500,333,944,350,556,350,350,333,333,556,556,350,500,889,333,980,
      389,333,667,350,389,556,250,389,500,500,500,500,275,500,333,760,276,500,675,333,760,333,
      400,675,300,300,333,500,523,250,333,300,310,500,750,750,750,500,611,611,611,611,611,611,
      889,667,611,611,611,611,333,333,333,333,722,667,722,722,722,722,722,675,722,722,722,722,
      722,556,611,500,500,500,500,500,500,500,667,444,444,444,444,444,278,278,278,278,500,500,
      500,500,500,500,500,675,500,500,500,500,500,444,500,444);

    CW_TIMES_BOLD_ITALIC: TFPDFFontInfo = (
      250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,
      250,250,250,250,250,250,250,250,250,250,250,389,555,500,500,833,778,278,333,333,500,570,
      250,333,250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,570,500,832,667,
      667,667,722,667,667,722,778,389,500,667,611,889,722,722,611,722,667,556,611,722,667,889,
      667,611,611,333,278,333,570,500,333,500,500,444,500,444,333,500,556,278,278,500,278,778,
      556,500,500,500,389,389,278,556,444,667,500,444,389,348,220,348,570,350,500,350,333,500,
      500,1000,500,500,333,1000,556,333,944,350,611,350,350,333,333,500,500,350,500,1000,333,1000,
      389,333,722,350,389,611,250,389,500,500,500,500,220,500,333,747,266,500,606,333,747,333,
      400,570,300,300,333,576,500,250,333,300,300,500,750,750,750,500,667,667,667,667,667,667,
      944,667,667,667,667,667,389,389,389,389,722,722,722,722,722,722,722,570,722,722,722,722,
      722,611,611,500,500,500,500,500,500,500,722,444,444,444,444,444,278,278,278,278,500,556,
      500,500,500,500,500,570,500,556,556,556,556,444,500,444);

    CW_SYMBOL: TFPDFFontInfo = (
      250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,250,
      250,250,250,250,250,250,250,250,250,250,250,333,713,500,549,833,778,439,333,333,500,549,
      250,549,250,278,500,500,500,500,500,500,500,500,500,500,278,278,549,549,549,444,549,722,
      667,722,612,611,763,603,722,333,631,722,686,889,722,722,768,741,556,592,611,690,439,768,
      645,795,611,333,863,333,658,500,500,631,549,549,494,439,521,411,603,329,603,549,549,576,
      521,549,549,521,549,603,439,576,713,686,493,686,494,480,200,480,549,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,750,620,247,549,167,713,500,753,753,753,753,1042,987,603,987,603,
      400,549,411,549,549,713,494,460,549,549,549,549,1000,603,1000,658,823,686,795,987,768,768,
      823,768,768,713,713,713,713,713,713,713,768,713,790,790,890,823,549,250,713,603,603,1042,
      987,603,987,603,494,329,790,790,786,713,384,384,384,384,384,384,494,494,494,494,0,329,
      274,686,686,686,384,384,384,384,384,384,494,494,494,0);

    CW_ZAPFDINGBATS: TFPDFFontInfo = (
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,278,974,961,974,980,719,789,790,791,690,960,939,
      549,855,911,933,911,945,974,755,846,762,761,571,677,763,760,759,754,494,552,537,577,692,
      786,788,788,790,793,794,816,823,789,841,823,833,816,831,923,744,723,749,790,792,695,776,
      768,792,759,707,708,682,701,826,815,789,789,707,687,696,689,786,787,713,791,785,791,873,
      761,762,762,759,759,892,892,788,784,438,138,277,415,392,392,668,668,0,390,390,317,317,
      276,276,509,509,410,410,234,234,334,334,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,732,544,544,910,667,760,760,776,595,694,626,788,788,788,788,
      788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,
      788,788,788,788,788,788,788,788,788,788,788,788,788,788,894,838,1016,458,748,924,748,918,
      927,928,928,834,873,828,924,924,917,930,931,463,883,836,836,867,867,696,696,874,0,874,
      760,946,771,865,771,888,967,888,831,873,927,970,918,0);
var
  f: TFPDFFont;

begin
  Self.Fonts.Clear;

  with Self.Fonts.New do
    Name := 'Courier';

  with Self.Fonts.New do
    Name := 'Courier-Bold';

  with Self.Fonts.New do
    Name := 'Courier-Oblique';

  with Self.Fonts.New do
    Name := 'Courier-BoldOblique';

  with Self.Fonts.New do
  begin
    Name := 'Helvetica';
    cw := CW_HELVETICA;
  end;

  with Self.Fonts.New do
  begin
    Name := 'Helvetica-Bold';
    cw := CW_HELVETICA_BOLD;
  end;

  with Self.Fonts.New do
  begin
    Name := 'Helvetica-Oblique';
    cw := CW_HELVETICA_OBLIQUE;
  end;

  with Self.Fonts.New do
  begin
    Name := 'Helvetica-BoldOblique';
    cw := CW_HELVETICA_BOLD_OBLIQUE;
  end;

  with Self.Fonts.New do
  begin
    Name := 'Times-Roman';
    FontName := 'Times';
    cw := CW_TIMES_ROMAN;
  end;

  with Self.Fonts.New do
  begin
    Name := 'Times-Bold';
    cw := CW_TIMES_BOLD;
  end;

  f := Self.Fonts.New;
  with f do
  begin
    Name := 'Times-Italic';
    FontName := 'Times-Oblique';
    cw := CW_TIMES_ITALIC;
  end;

  with Self.Fonts.New do
  begin
    Name := 'Times-BoldItalic';
    FontName := 'Times-BoldOblique';
    cw := CW_TIMES_BOLD_ITALIC;
  end;

  with Self.Fonts.New do
  begin
    Name := 'Symbol';
    cw := CW_SYMBOL;
    fill(fuv1, -1);
    fill(fuv2, -1);

    fuv1[032] := 160;
    fuv1[033] := 33;
    fuv1[034] := 8704;
    fuv1[035] := 35;
    fuv1[036] := 8707;
    fuv1[037] := 37;    fuv2[037] := 2;
    fuv1[039] := 8715;
    fuv1[040] := 40;    fuv2[040] := 2;
    fuv1[042] := 8727;
    fuv1[043] := 43;    fuv2[043] := 2;
    fuv1[045] := 8722;
    fuv1[046] := 46;    fuv2[046] := 18;
    fuv1[064] := 8773;
    fuv1[065] := 913;   fuv2[065] := 2;
    fuv1[067] := 935;
    fuv1[068] := 916;   fuv2[068] := 2;
    fuv1[070] := 934;
    fuv1[071] := 915;
    fuv1[072] := 919;
    fuv1[073] := 921;
    fuv1[074] := 977;
    fuv1[075] := 922;   fuv2[075] := 4;
    fuv1[079] := 927;   fuv2[079] := 2;
    fuv1[081] := 920;
    fuv1[082] := 929;
    fuv1[083] := 931;   fuv2[083] := 3;
    fuv1[086] := 962;
    fuv1[087] := 937;
    fuv1[088] := 926;
    fuv1[089] := 936;
    fuv1[090] := 918;
    fuv1[091] := 91;
    fuv1[092] := 8756;
    fuv1[093] := 93;
    fuv1[094] := 8869;
    fuv1[095] := 95;
    fuv1[096] := 63717;
    fuv1[097] := 945;   fuv2[097] := 2;
    fuv1[099] := 967;
    fuv1[100] := 948;   fuv2[100] := 2;
    fuv1[102] := 966;
    fuv1[103] := 947;
    fuv1[104] := 951;
    fuv1[105] := 953;
    fuv1[106] := 981;
    fuv1[107] := 954;   fuv2[107] := 4;
    fuv1[111] := 959;   fuv2[111] := 2;
    fuv1[113] := 952;
    fuv1[114] := 961;
    fuv1[115] := 963;   fuv2[115] := 3;
    fuv1[118] := 982;
    fuv1[119] := 969;
    fuv1[120] := 958;
    fuv1[121] := 968;
    fuv1[122] := 950;
    fuv1[123] := 123;   fuv2[123] := 3;
    fuv1[126] := 8764;
    fuv1[160] := 8364;
    fuv1[161] := 978;
    fuv1[162] := 8242;
    fuv1[163] := 8804;
    fuv1[164] := 8725;
    fuv1[165] := 8734;
    fuv1[166] := 402;
    fuv1[167] := 9827;
    fuv1[168] := 9830;
    fuv1[169] := 9829;
    fuv1[170] := 9824;
    fuv1[171] := 8596;
    fuv1[172] := 8592;  fuv2[172] := 4;
    fuv1[176] := 176;   fuv2[176] := 2;
    fuv1[178] := 8243;
    fuv1[179] := 8805;
    fuv1[180] := 215;
    fuv1[181] := 8733;
    fuv1[182] := 8706;
    fuv1[183] := 8226;
    fuv1[184] := 247;
    fuv1[185] := 8800;  fuv2[185] := 2;
    fuv1[187] := 8776;
    fuv1[188] := 8230;
    fuv1[189] := 63718; fuv2[189] := 2;
    fuv1[191] := 8629;
    fuv1[192] := 8501;
    fuv1[193] := 8465;
    fuv1[194] := 8476;
    fuv1[195] := 8472;
    fuv1[196] := 8855;
    fuv1[197] := 8853;
    fuv1[198] := 8709;
    fuv1[199] := 8745;  fuv2[199] := 2;
    fuv1[201] := 8835;
    fuv1[202] := 8839;
    fuv1[203] := 8836;
    fuv1[204] := 8834;
    fuv1[205] := 8838;
    fuv1[206] := 8712;  fuv2[206] := 2;
    fuv1[208] := 8736;
    fuv1[209] := 8711;
    fuv1[210] := 63194;
    fuv1[211] := 63193;
    fuv1[212] := 63195;
    fuv1[213] := 8719;
    fuv1[214] := 8730;
    fuv1[215] := 8901;
    fuv1[216] := 172;
    fuv1[217] := 8743;  fuv2[217] := 2;
    fuv1[219] := 8660;
    fuv1[220] := 8656;  fuv2[220] := 4;
    fuv1[224] := 9674;
    fuv1[225] := 9001;
    fuv1[226] := 63720; fuv2[226] := 3;
    fuv1[229] := 8721;
    fuv1[230] := 63723; fuv2[230] := 10;
    fuv1[241] := 9002;
    fuv1[242] := 8747;
    fuv1[243] := 8992;
    fuv1[244] := 63733;
    fuv1[245] := 8993;
    fuv1[246] := 63734; fuv2[246] := 9;
  end;

  with Self.Fonts.New do
  begin
    Name := 'ZapfDingbats';
    cw := CW_ZAPFDINGBATS;
    fill(fuv1, -1);
    fill(fuv2, -1);

    fuv1[032] := 32;
    fuv1[033] := 9985;  fuv2[033] := 4;
    fuv1[037] := 9742;
    fuv1[038] := 9990;  fuv2[038] := 4;
    fuv1[042] := 9755;
    fuv1[043] := 9758;
    fuv1[044] := 9996;  fuv2[044] := 28;
    fuv1[072] := 9733;
    fuv1[073] := 10025; fuv2[073] := 35;
    fuv1[108] := 9679;
    fuv1[109] := 10061;
    fuv1[110] := 9632;
    fuv1[111] := 10063; fuv2[111] := 4;
    fuv1[115] := 9650;
    fuv1[116] := 9660;
    fuv1[117] := 9670;
    fuv1[118] := 10070;
    fuv1[119] := 9687;
    fuv1[120] := 10072; fuv2[120] := 7;
    fuv1[128] := 10088; fuv2[128] := 14;
    fuv1[161] := 10081; fuv2[161] := 7;
    fuv1[168] := 9827;
    fuv1[169] := 9830;
    fuv1[170] := 9829;
    fuv1[171] := 9824;
    fuv1[172] := 9312;  fuv2[172] := 10;
    fuv1[182] := 10102; fuv2[182] := 31;
    fuv1[213] := 8594;
    fuv1[214] := 8596;  fuv2[214] := 2;
    fuv1[216] := 10136; fuv2[216] := 24;
    fuv1[241] := 10161; fuv2[241] := 14;
  end;
end;

function TFPDF.FindUsedImage(const ImageName: String): TFPDFImageInfo;
var
  i: Integer;
begin
  Result.data := '';

  if (Length(Self.images) < 1) then
   Exit;

  for i := 0 to Length(Self.images)-1 do
  begin
    if (Self.images[i].ImageName = ImageName) then
    begin
      Result := Self.images[i];
      break;
    end;
  end;
end;

function TFPDF._getpagesize(APageSize: TFPDFPageSize): TFPDFPageSize;
begin
  if (APageSize.w > APageSize.h) then
  begin
    Result.h := APageSize.w;
    Result.w := APageSize.h;
  end
  else
  begin
    Result.h := APageSize.h;
    Result.w := APageSize.w;
  end;
end;

function TFPDF._getpagesize(APageFormat: TFPDFPageFormat): TFPDFPageSize;
begin
  Result.w := StdPageSizes[APageFormat].w/Self.k;
  Result.h := StdPageSizes[APageFormat].h/Self.k;
end;

procedure TFPDF._beginpage(AOrientation: TFPDFOrientation; APageSize: TFPDFPageSize; ARotation: TFPDFRotation);
begin
  Self.page := Self.page + 1;
  SetLength(Self.pages, Self.page);
  Self.pages[Self.page-1] := '';
  SetLength(Self.PageLinks, Self.page);
  SetLength(Self.PageLinks[Self.page-1], 0);
  Self.PageInfo.New;

  Self.state := 2;
  Self.x := Self.lMargin;
  Self.y := Self.tMargin;
  Self.FontFamily := '';

  // Check page size and orientation
  if ((AOrientation <> Self.CurOrientation) or (APageSize.w <> Self.CurPageSize.w) or (APageSize.h <> Self.CurPageSize.h)) then
  begin
    // New size or orientation
    if (AOrientation = poLandscape) then
    begin
      Self.w := APageSize.h;
      Self.h := APageSize.w;
    end
    else
    begin
      Self.w := APageSize.w;
      Self.h := APageSize.h;
    end;

    Self.wPt := Self.w*Self.k;
    Self.hPt := Self.h*Self.k;
    Self.PageBreakTrigger := Self.h-Self.bMargin;
    Self.CurOrientation := AOrientation;
    Self.CurPageSize := APageSize;
  end;

  if ((AOrientation <> Self.DefOrientation) or (APageSize.w <> Self.DefPageSize.w) or (APageSize.h <> Self.DefPageSize.h)) then
    Self.PageInfo[Self.page-1].Values['size'] := FloatToStr(Self.wPt) + ' ' + FloatToStr(Self.hPt);

  if (ARotation <> ro0) then
    Self.PageInfo[Self.page-1].Values['rotation'] := IntToStr(CPDFRotation[ARotation]);

  Self.CurRotation := ARotation;
end;

procedure TFPDF._endpage;
begin
  Self.state := 1;
end;

(* //TODO
protected function _loadfont($font)
{
	// Load a font definition file from the font directory
	if(strpos($font,'/')!==false || strpos($font,"\\")!==false)
		$this->Error('Incorrect font definition file name: '.$font);
	include($this->fontpath.$font);
	if(!isset($name))
		$this->Error('Could not include font definition file');
	if(isset($enc))
		$enc = strtolower($enc);
	if(!isset($subsetted))
		$subsetted = false;
	return get_defined_vars();
}

*)

function TFPDF._isascii(const AString: String): Boolean;
var
  nb, i: Integer;
begin
  // Test if string is ASCII
  nb := Length(AString);
  i := 1;
  Result := True;
  while Result and (i <= nb) do
  begin
    Result := not (ord(AString[i]) > 127);
    Inc(i);
  end;
end;

function TFPDF._EncodeText(const AString: String): String;
begin
  if Self.UseUTF8 then
    Result := _UTF8encode(AString)
  else
    Result := AString;
end;


function TFPDF._UTF8encode(const AString: String): String;
{$IFNDEF FPC}
 {$IFDEF UNICODE}
  var
    RBS: RawByteString;
 {$ENDIF}
{$ENDIF}
begin
  if Self.UseUTF8 then
    Result := AString
  else
  begin
   {$IFNDEF FPC}
     {$IFDEF UNICODE}
       RBS := UTF8Encode(AString);
       SetCodePage(RBS, 0, False);
       Result := AnsiString(RBS);
     {$ELSE}
       Result := UTF8Encode(AString);
     {$ENDIF}
   {$ELSE}
     Result := UTF8Encode(AString);
   {$ENDIF}
  end;
end;

function TFPDF._UTF8toUTF16(const AString: String): WideString;
begin
  Result := WideString(AString);
end;

function TFPDF._escape(const sText: AnsiString): AnsiString;
begin
  //Add \ before \, ( and )
  Result := sText;
  if (pos('\', sText) > 0) then
    Result := StringReplace(Result,  '\', '\\', [rfReplaceAll]);

  if (pos(')', sText) > 0) then
    Result := StringReplace( Result, ')', '\)', [rfReplaceAll]);

  if (pos('(', sText) > 0) then
    Result := StringReplace( Result, '(', '\(', [rfReplaceAll]);

  if (pos(CR, sText) > 0) then
    Result := StringReplace( Result, CR, '\'+CR, [rfReplaceAll]);
end;

function TFPDF._textstring(const AString: String): String;
var
  s: AnsiString;
begin
  // Format a text string
  if (not _isascii(AString)) then
    s := AnsiString(_UTF8toUTF16(AString))
  else
    s := AString;

  Result := '('+_escape(s)+')';
end;

function TFPDF._dounderline(vX, vY: Double; const vText: String): String;
var
  vw, up, ut: Double;
begin
  // Underline text
  up := Self.CurrentFont.up;
  ut := Self.CurrentFont.ut;

  vw := GetStringWidth(vText) + Self.ws * CountStr(vText,' ');
  Result := Format('%.2f %.2f %.2f %.2f re f',
     [vX * Self.k, (Self.h-(vY-up/1000*Self.FontSize))*Self.k, vw*Self.k, -ut/1000*Self.FontSizePt],
     FPDFFormatSetings);
end;

function TFPDF._parsejpg(vImageStream: TStream): TFPDFImageInfo;
var
  Data: AnsiString;
  SegType, ColorComponents, ColorBits: Byte;
  Width, Height: Word;
  l: Integer;
  ColSpace: String;

  procedure ReadNextSegment(S: TStream; out SegType: Byte; out SegData: AnsiString);
  var
    Len: Word;
    SegStart: Byte;
  begin
    SegType := 0;
    SegData := '';
    while ((SegType = 0) or (SegType = 255)) and  (S.Position <= S.Size-1)  do
    begin
      SegStart := 0;
      while (SegStart <> 255) do    // Scan for the Start
        S.ReadBuffer(SegStart, 1);

      S.ReadBuffer(SegType, 1);
    end;

    Len := 0;
    if (SegType > 0) and (S.Position < S.Size-2) then
    begin
      S.ReadBuffer(Len, 2);
      Len := SwapBytes(Len);
      Dec(Len, 2);
    end;

    if (Len > 0) and ((S.Position + Len) < S.Size) then
    begin
      SetLength(SegData, Len);
      S.ReadBuffer(SegData[1], Len);
    end;
  end;


begin
  // Extract info from a JPEG file

  vImageStream.Position := 0;
  Data := '';
  Result.pal := '';
  SetLength(Result.trns,0);
  Result.data := '';

  // Check signature
  SetLength(Data, 2);
  vImageStream.ReadBuffer(Data[1], 2);
  if (Data <> #255 + #216) then     //  SOI, FF+D8, Start of Image
    Error('JPG - Invalid');

  Width := 0;
  Height := 0;
  ColorBits := 0;
  ColorComponents := 0;
  SegType := 0;
  while (SegType <> 217) do  // EOI, FF+D9, End Of Image
  begin
    ReadNextSegment(vImageStream, SegType, Data);
    if (SegType = 0) then
      Error('JPG - SOF Invalid');

    case SegType of
      192, 194: // SOF0, C0 or SOF2, C2
      begin
        if (Length(Data) > 4) then
        begin
          Move(Data[2], Height, 2);
          Height := SwapBytes(Height);
          Move(Data[4], Width, 2);
          Width := SwapBytes(Width);

           // Get the color components and precision
           Move(Data[6], ColorComponents, 1);
           Move(Data[7], ColorBits, 1);
           ColorBits := ColorBits * 8;
           Break;
        end;
      end;
    end;
  end;

  if (Width = 0) then
    Error('JPG - SOF Invalid');

  if (ColorComponents = 3) then
    ColSpace := 'DeviceRGB'
  else if (ColorComponents = 4) then
    ColSpace := 'DeviceCMYK'
  else
    ColSpace := 'DeviceGray';

  Result.w := Width;
  Result.h := Height;
  Result.bpc := ColorBits;
  Result.cs := ColSpace;
  Result.f := 'DCTDecode';
  vImageStream.Position := 0;
  Setlength(Result.data, vImageStream.Size);
  l := vImageStream.read(PAnsiChar(Result.data)^, vImageStream.Size);
  SetLength(Result.data, l);
end;

function TFPDF._parsepng(vImageStream: TStream): TFPDFImageInfo;
var
  s, color, alpha, LinData, LinColor, LinAlpha, ChunkType, ChunkData, DataDecompress: AnsiString;
  Width, Height, LenChunk, LenWidth, p, i, j, l, m,
    LenColor, LenLinColor, LenLinAlpha, ld: LongWord;
  BitDepth, ColorType, CompressionMethod, FilterMethod, InterlaceMethod: Byte;

  function ReadNextChunk(S: TStream; out ChunckType: AnsiString; out ChunkData: AnsiString): Integer;
  var
    Len: LongWord;
    crc: AnsiString;
  begin
    Len := 0;
    ChunckType := '';
    ChunkData := '';
    S.ReadBuffer(Len, 4);
    Len := SwapBytes(Len);
    SetLength(ChunckType, 4);
    S.ReadBuffer(ChunckType[1], 4);
    if (Len > 0) then
    begin
      SetLength(ChunkData, Len);
      S.ReadBuffer(ChunkData[1], Len);
    end;
    SetLength(crc, 4);
    S.ReadBuffer(crc[1], 4);

    Result := Len;
  end;

begin
  vImageStream.Position := 0;
  s := '';
  Result.pal := '';
  SetLength(Result.trns,0);
  Result.data := '';

  // Check signature
  SetLength(s, 8);
  vImageStream.ReadBuffer(s[1], 8);
  if (s <> #137 + 'PNG' + CR+LF+#26+LF) then
    Error('PNG - Invalid');

  {
     The IHDR chunk must appear FIRST. It contains:
     Width:              4 bytes
     Height:             4 bytes
     Bit depth:          1 byte
     Color type:         1 byte
     Compression method: 1 byte
     Filter method:      1 byte
     Interlace method:   1 byte
  }
  LenChunk := ReadNextChunk(vImageStream, ChunkType, ChunkData);
  if(ChunkType <> 'IHDR') or (LenChunk <> 13) then
    Error('PNG - Incorrect IHDR');

  Width := 0;
  Move(ChunkData[1], Width, 4);
  Width := SwapBytes(Width);
  Height := 0;
  Move(ChunkData[5], Height, 4);
  Height := SwapBytes(Height);
  BitDepth := Byte(ChunkData[9]);
  ColorType := Byte(ChunkData[10]);
  CompressionMethod := Byte(ChunkData[11]);
  FilterMethod := Byte(ChunkData[12]);
  InterlaceMethod := Byte(ChunkData[13]);

  // Check is a Supported PNG
  if (BitDepth > 8) then
    Error('PNG - 16-bit depth not supported');

  case ColorType of
    0, 4: Result.cs := 'DeviceGray';
    2, 6: Result.cs := 'DeviceRGB';
    3: Result.cs := 'Indexed';
  else
    Error('PNG - Unknown color type');
  end;

  if (CompressionMethod <> 0) then
    Error('PNG - Unknown compression method');
  if (FilterMethod <> 0) then
    Error('PNG - Unknown filter method');
  if(InterlaceMethod <> 0) then
    Error('PNG - Interlacing not supported');

  // Scan chunks looking for palette, transparency and image data
  while (ChunkType <> 'IEND') and (LenChunk > 0) do
  begin
    if (ChunkType = 'PLTE') then
    begin
      // Read palette
      Result.pal := ChunkData;
    end

    else if (ChunkType = 'tRNS') then
    begin
      // Read transparency info
      if (ColorType = 0) then
      begin
        SetLength(Result.trns, 1);
        Result.trns[0] := ord(ChunkData[2]);
      end
      else if (ColorType = 2) then
      begin
        SetLength(Result.trns, 3);
        Result.trns[0] := ord(ChunkData[2]);
        Result.trns[1] := ord(ChunkData[4]);
        Result.trns[2] := ord(ChunkData[6]);
      end
      else
      begin
        p := pos(#0, ChunkData);
        if (p > 0) then
        begin
          SetLength(Result.trns, 1);
          Result.trns[0] := p-1;
        end;
      end;
    end

    else if (ChunkType='IDAT') then
    begin
      // Read image data block
      ld := Length(Result.data);
      SetLength(Result.data, ld+LenChunk);
      Move(ChunkData[1], Result.data[ld+1], LenChunk);
    end;

    LenChunk := ReadNextChunk(vImageStream, ChunkType, ChunkData);
  end;

  if (Result.cs='Indexed') and (Length(Result.pal)=0) then
    Error('PNG - Missing palette');

  Result.w := Width;
  Result.h := Height;
  Result.bpc := BitDepth;
  Result.f := 'FlateDecode';
  Result.dp := '/Predictor 15'+
               ' /Colors ' +IfThen(Result.cs='DeviceRGB','3','1')+
               ' /BitsPerComponent '+IntToStr(BitDepth)+
               ' /Columns '+IntToStr(Width);


  if (ColorType >= 4) then
  begin
    // Extract alpha channel
    color := '';
    alpha := '';
    LinData := '';
    LinAlpha := '';
    LinColor := '';
    DataDecompress := GzDecompress(Result.data);

    if (ColorType = 4) then
      LenColor := 1        // Gray image
    else
      LenColor := 3;       // RGB image

    LenWidth := (LenColor+1)*Width;
    LenLinAlpha := Width+1;
    LenLinColor := Width*LenColor+1;
    SetLength(LinData, LenWidth);
    SetLength(LinAlpha, LenLinAlpha);
    SetLength(LinColor, LenLinColor);
    SetLength(alpha, LenLinAlpha*Height);
    SetLength(color, LenLinColor*Height);
    for i := 0 to Height-1 do
    begin
      p := ((1+LenWidth)*i)+1;
      LinColor[1] := DataDecompress[p];
      LinAlpha[1] := DataDecompress[p];
      Move(DataDecompress[p+1], LinData[1], LenWidth);
      j := 1; l := 2; m := 2;
      while j < LenWidth do
      begin
        Move(LinData[j], LinColor[m], LenColor);
        Move(LinData[j+LenColor], LinAlpha[l], 1);
        inc(j, LenColor+1);
        inc(m, LenColor);
        inc(l);
      end;
      Move(LinColor[1], color[LenLinColor*i+1], LenLinColor);
      Move(LinAlpha[1], alpha[LenLinAlpha*i+1], LenLinAlpha);
    end;

    Result.data := gzcompress(color);
    Result.smask := gzcompress(alpha);
    Self.WithAlpha := True;
    if (Self.PDFVersion < 1.4) then
      Self.PDFVersion := 1.4;
  end;
end;

procedure TFPDF._out(const AData: AnsiString);
begin
  // Add a line to the current page
  case Self.state of
    2: Self.pages[Self.page-1] := Self.pages[Self.page-1] + AData + LF;
    0: Error('No page has been added yet');
    1: Error('Invalid call');
    3: Error('The document is closed');
  end;
end;

procedure TFPDF._put(const AData: AnsiString);
begin
  // Add a line to the document
  Self.buffer.Write(Pointer(AData+LF)^, Length(AData)+1);
end;

function TFPDF._getoffset: Int64;
begin
  Result := Self.buffer.Size;
end;

procedure TFPDF._newobj(vn: Integer=-1);
begin
  // Begin a new object
  if (vn < 0) then
  begin
    Self.n := Self.n + 1;
    vn := Self.n;
  end;

  if (vn > Length(Self.offsets)) then
    SetLength(Self.offsets, vn);

  Self.offsets[vn-1] := _getoffset();
  _put(IntToStr(vn)+' 0 obj');
end;

procedure TFPDF._putstream(const Adata: AnsiString);
begin
  _put('stream');
  _put(Adata);
  _put('endstream');
end;

procedure TFPDF._putstreamobject(const Adata: AnsiString);
var
  d: AnsiString;
  entries: String;
begin
  if Self.compress then
  begin
    entries := '/Filter /FlateDecode ';
    d := gzcompress(Adata);
  end
  else
  begin
    entries := '';
    d := Adata;
  end;

  entries := entries + '/Length '+IntToStr(Length(d));
  _newobj();
  _put('<<'+entries+'>>');
  _putstream(d);
  _put('endobj');
end;

procedure TFPDF._putlinks(const APage: Integer);
var
  l, i, idLink: Integer;
  aRect, s, vs: String;
  vh: Double;
  pl: TFPDFPageLink;
  lk: TFPDFLink;
  PageSizeInfo: TStringArray;
begin
  l := Length(Self.PageLinks[APage-1]);
  for i := 0 to l-1 do
  begin
    pl := Self.PageLinks[APage-1][i];
    _newobj();
    aRect := Format('%.2f %.2f %.2f %.2f', [pl.X, pl.Y, pl.x+pl.Width, pl.y-pl.Height], FPDFFormatSetings);
    s := '<</Type /Annot /Subtype /Link /Rect ['+aRect+'] /Border [0 0 0] ';
    IdLink := StrToIntDef(pl.Link, -1);
    if (IdLink = -1) then   // No Integer value in 'Link'
      s := s + '/A <</S /URI /URI '+_textstring(pl.Link)+'>>>>'
    else if (idLink < Length(Self.links)) then
    begin
      lk := Self.links[idLink];
      if (lk.Page > 0) then
      begin
        vh := -1;
        vs := Self.PageInfo[lk.Page-1].Values['size'];
        if (vs <> '') then
        begin
          PageSizeInfo := Split(vs);
          if (Length(PageSizeInfo) > 1) then
            vh := StrToFloatDef(PageSizeInfo[1], -1);
        end;

        if (vh = -1) then
          vh := IfThen(Self.DefOrientation=poPortrait, Self.DefPageSize.h * Self.k,  Self.DefPageSize.w * Self.k);

        s := s + Format('/Dest [%s 0 R /XYZ 0 %.2f null]>>', [Self.PageInfo[lk.Page-1].Values['n'], vh-lk.y*Self.k], FPDFFormatSetings);
      end;
    end;

    _put(s);
    _put('endobj');
  end;
end;

procedure TFPDF._putpage(const APage: Integer);
var
  s: String;
  PageSizeInfo: TStringArray;
  i: Integer;
begin
  _newobj();
  _put('<</Type /Page');
  _put('/Parent 1 0 R');
  s := Self.PageInfo[APage-1].Values['size'];
  if (s <> '') then
  begin
    PageSizeInfo := Split(s);
    if (Length(PageSizeInfo) > 1) then
      _put(Format('/MediaBox [0 0 %s %s]', [PageSizeInfo[0],PageSizeInfo[1]], FPDFFormatSetings));
  end;

  s := Self.PageInfo[APage-1].Values['rotation'];
  if (s <> '') and (s <> '0') then
    _put('/Rotate '+s);

  _put('/Resources 2 0 R');

  if (Length(Self.PageLinks[APage-1]) > 0) then
  begin
    s := '/Annots [';
    for i := 0 to Length(Self.PageLinks[APage-1])-1 do
      s := s + IntToStr(Self.PageLinks[APage-1][i].n) + ' 0 R ';

    s := s + ']';
    _put(s);
  end;

  if (Self.WithAlpha) then
    _put('/Group <</Type /Group /S /Transparency /CS /DeviceRGB>>');

  _put('/Contents '+IntToStr(Self.n+1)+' 0 R>>');
  _put('endobj');

  // Page content
  if (Self.AliasNbPages <> '') then
    Self.pages[APage-1] := StringReplace(Self.pages[APage-1], Self.AliasNbPages, IntToStr(Self.page), [rfReplaceAll]);

  _putstreamobject(Self.pages[APage-1]);

  // Link annotations
  _putlinks(APage);
end;

procedure TFPDF._putpages;
var
  vnb, vn, i, j: Integer;
  kids: String;
  vw, vh: Double;
begin
  vnb := Self.page;
  vn := Self.n;

  for i := 0 to vnb-1 do
  begin
    Inc(vn);
    Self.PageInfo[i].Values['n'] := IntToStr(vn);
    Inc(vn);

    for j := 0 to Length(Self.PageLinks[i])-1 do
    begin
      Inc(vn);
      Self.PageLinks[i][j].n := vn;
    end;
  end;

  for i := 1 to vnb do
    _putpage(i);

  // Pages root
  _newobj(1);
  _put('<</Type /Pages');
  kids := '/Kids [';
  for i := 0 to vnb-1 do
    kids := kids + Self.PageInfo[i].Values['n']+' 0 R ';
  kids := kids + ']';
  _put(kids);
  _put('/Count '+IntToStr(vnb));

  if (Self.DefOrientation = poPortrait) then
  begin
    vw := Self.DefPageSize.w;
    vh := Self.DefPageSize.h;
  end
  else
  begin
    vw := Self.DefPageSize.h;
    vh := Self.DefPageSize.w;
  end;

  _put(Format('/MediaBox [0 0 %.2f %.2f]', [vw*Self.k, vh*Self.k], FPDFFormatSetings));
  _put('>>');
  _put('endobj');
end;

procedure TFPDF._putfonts;
var
  LenFonts, i, j: Integer;
  Font: TFPDFFont;
  cmap, cmapkey, FontName, s: String;
  FontType: TFPDFFontType;
  cw: TFPDFFontInfo;
begin
  (* //TODO
  foreach($this->FontFiles as $file=>$info)
  {
    // Font file embedding
    $this->_newobj();
    $this->FontFiles[$file]['n'] = $this->n;
    $font = file_get_contents($this->fontpath.$file,true);
    if(!$font)
	    $this->Error('Font file not found: '.$file);
    $compressed = (substr($file,-2)=='.z');
    if(!$compressed && isset($info['length2']))
	    $font = substr($font,6,$info['length1']).substr($font,6+$info['length1']+6,$info['length2']);
    $this->_put('<</Length '.strlen($font));
    if($compressed)
	    $this->_put('/Filter /FlateDecode');
    $this->_put('/Length1 '.$info['length1']);
    if(isset($info['length2']))
	    $this->_put('/Length2 '.$info['length2'].' /Length3 0');
    $this->_put('>>');
    $this->_putstream($font);
    $this->_put('endobj');
  }
  *)

  LenFonts := Length(Self.UsedFonts);
  for i := 0 to LenFonts-1 do
  begin
    Font := Self.Fonts.Font[Self.UsedFonts[i].FontName];
    // Encoding
    if (Font.diff <> '') then
    begin
      if (Self.encodings.Values[CFontEncodeStr[Font.enc]] = '') then
      begin
        _newobj();
        _put('<</Type /Encoding /BaseEncoding /WinAnsiEncoding /Differences ['+Font.diff+']>>');
        _put('endobj');
        Self.encodings.Values[CFontEncodeStr[Font.enc]] := IntToStr(Self.n);
      end;
    end;

    // ToUnicode CMap
    cmap := _tounicodecmap(Font.uv1, Font.uv2);
    cmapkey := '';
    if (cmap <> '') then
    begin
      if (Font.enc <> encNone) then
        cmapkey := CFontEncodeStr[Font.enc]
      else
        cmapkey := Font.Name;

      if (cmaps.Values[cmapkey] = '') then
      begin
        _putstreamobject(cmap);
        cmaps.Values[cmapkey] := IntToStr(Self.n);
      end;
    end;

    // Font object
    Self.UsedFonts[i].n := Self.n+1;
    FontType := Font.FontType;
    FontName := Font.Name;
    if Font.subsetted then
      FontName := 'AAAAAA+'+FontName;

    if (FontType = ftCore) then
    begin
      // Core font
      _newobj();
      _put('<</Type /Font');
      _put('/BaseFont /'+FontName);
      _put('/Subtype /Type1');
      if ((FontName <> 'Symbol') and (FontName <> 'ZapfDingbats')) then
        _put('/Encoding /WinAnsiEncoding');

      if (cmapkey <> '') then
        _put('/ToUnicode '+Self.cmaps.Values[cmapkey]+' 0 R');

      _put('>>');
      _put('endobj');
    end

    else if ((FontType = ftType1) or (FontType = ftTrueType)) then
    begin
      // Additional Type1 or TrueType/OpenType font
      _newobj();
      _put('<</Type /Font');
      _put('/BaseFont /'+FontName);
      _put('/Subtype /'+CFontType[FontType]);
      _put('/FirstChar 32 /LastChar 255');
      _put('/Widths '+IntToStr(Self.n+1)+' 0 R');
      _put('/FontDescriptor '+IntToStr(Self.n+2)+' 0 R');
      if (Font.diff <> '') then
        _put('/Encoding '+Self.encodings.Values[CFontEncodeStr[Font.enc]]+' 0 R')
      else
        _put('/Encoding /WinAnsiEncoding');

      if (cmapkey <> '') then
        _put('/ToUnicode '+Self.cmaps.Values[cmapkey]+' 0 R');

      _put('>>');
      _put('endobj');

      // Widths
      _newobj();
      cw := Font.cw;
      s := '[';
      for j := 32 to 255 do
        s := s + IntToStr(cw[j])+' ';

      _put(s+']');
      _put('endobj');

      // Descriptor
      _newobj();
      s := '<</Type /FontDescriptor /FontName /'+FontName;
      (* //TODO
      foreach($font['desc'] as $k=>$v)
	      $s .= ' /'.$k.' '.$v;
      if(!empty($font['file']))
	      $s .= ' /FontFile'.($type=='Type1' ? '' : '2').' '.$this->FontFiles[$font['file']]['n'].' 0 R';
      *)
      _put(s+'>>');
      _put('endobj');
    end;
    (* //TODO
    else
    {
	    // Allow for additional types
	    $mtd = '_put'.strtolower($type);
	    if(!method_exists($this,$mtd))
		    $this->Error('Unsupported font type: '.$type);
	    $this->$mtd($font);
    } *)
  end;
end;

function TFPDF._tounicodecmap(uv1, uv2: TFPDFFontInfo): String;
var
  ranges, chars, s: String;
  nbr, nbc, i: Integer;
begin
  ranges := '';
  nbr := 0;
  chars := '';
  nbc := 0;
  s := '';

  for i := Low(TFPDFFontInfo) to High(TFPDFFontInfo) do
  begin
    if (uv1[i] >= 0) then
    begin
      if (uv2[i] >= 0) then
      begin
        ranges := ranges + Format('<%.2x> <%.2x> <%.4x>'+LF, [i, i+uv2[i]-1, uv1[i]]);
        Inc(nbr);
      end
      else
      begin
        chars := chars + Format('<%.2x> <%.4x>'+LF, [i, uv1[i]]);
        inc(nbc);
      end;
    end;
  end;

  if (nbr > 0) or (nbc > 0) then
  begin
    s := '/CIDInit /ProcSet findresource begin'+LF;
    s := s + '12 dict begin'+LF;
    s := s + 'begincmap'+LF;
    s := s + '/CIDSystemInfo'+LF;
    s := s + '<</Registry (Adobe)'+LF;
    s := s + '/Ordering (UCS)'+LF;
    s := s + '/Supplement 0'+LF;
    s := s + '>> def'+LF;
    s := s + '/CMapName /Adobe-Identity-UCS def'+LF;
    s := s + '/CMapType 2 def'+LF;
    s := s + '1 begincodespacerange'+LF;
    s := s + '<00> <FF>'+LF;
    s := s + 'endcodespacerange'+LF;

    if (nbr > 0) then
    begin
      s := s + IntToStr(nbr)+' beginbfrange'+LF;
      s := s + ranges;
      s := s + 'endbfrange'+LF;
    end;

    if (nbc > 0) then
    begin
      s := s + IntToStr(nbc)+' beginbfchar'+LF;
      s := s + chars;
      s := s + 'endbfchar'+LF;
    end;

    s := s + 'endcmap'+LF;
    s := s + 'CMapName currentdict /CMap defineresource pop'+LF;
    s := s + 'end'+LF;
    s := s + 'end';
  end;

  Result := s;
end;

procedure TFPDF._putimages;
var
  l, i: Integer;
begin
  l := Length(Self.images)-1;
  for i := 0 to l do
  begin
    _putimage(Self.images[i]);
    Self.images[i].data := '';
    Self.images[i].smask := '';
  end;
end;

procedure TFPDF._putimage(var img: TFPDFImageInfo);
var
  s: AnsiString;
  i, l: Integer;
  smask: TFPDFImageInfo;
begin
  _newobj();
  img.n := Self.n;
  _put('<</Type /XObject');
  _put('/Subtype /Image');
  _put('/Width '+FloatToStr(img.w));
  _put('/Height '+FloatToStr(img.h));
  if (img.cs = 'Indexed') then
    _put('/ColorSpace [/Indexed /DeviceRGB '+FloatToStr(Length(img.pal)/3-1)+' '+IntToStr(Self.n+1)+' 0 R]')
  else
  begin
    _put('/ColorSpace /'+img.cs);
    if(img.cs = 'DeviceCMYK') then
      _put('/Decode [1 0 1 0 1 0 1 0]');
  end;

  _put('/BitsPerComponent '+IntToStr(img.bpc));
  if (img.f <> '') then
    _put('/Filter /'+img.f);

  if (img.dp <> '') then
    _put('/DecodeParms <<'+img.dp+'>>');

  if (Length(img.trns) > 0) then
  begin
    s := '';
    l := Length(img.trns)-1;
    for i := 0 to l do
      s := s + IntToStr(img.trns[i])+' '+IntToStr(img.trns[i])+' ';

    _put('/Mask ['+s+']');
  end;

  if (img.smask <> '') then
    _put('/SMask '+IntToStr(Self.n+1)+' 0 R');

  _put('/Length '+IntToStr(Length(img.data))+'>>');
  _putstream(img.data);
  _put('endobj');

  // Soft mask
  if (img.smask <> '') then
  begin
    smask.w := img.w;
    smask.h := img.h;
    smask.cs := 'DeviceGray';
    smask.bpc := 8;
    smask.f := img.f;
    smask.dp := '/Predictor 15 /Colors 1 /BitsPerComponent 8 /Columns '+IntToStr(img.w);
    smask.data := img.smask;
    smask.smask := '';
    _putimage(smask);
  end;

  // Palette
  if (img.cs = 'Indexed') then
    _putstreamobject(img.pal);
end;

procedure TFPDF._putxobjectdict;
var
  l, i: Integer;
begin
  l := Length(Self.images)-1;
  for i := 0 to l do
    _put('/I'+IntToStr(Self.images[i].i)+' '+IntToStr(Self.images[i].n)+' 0 R');
end;

procedure TFPDF._putresourcedict();
var
  l, i: Integer;
begin
  _put('/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]');
  _put('/Font <<');
  l := Length(Self.UsedFonts)-1;
  for i := 0 to l do
    _put('/F'+IntToStr(i+1)+' '+IntToStr(Self.UsedFonts[i].n)+' 0 R');

  _put('>>');
  _put('/XObject <<');
  _putxobjectdict();
  _put('>>');
end;

procedure TFPDF._putresources;
begin
  _putfonts();
  _putimages();
  // Resource dictionary
  _newobj(2);
  _put('<<');
  _putresourcedict();
  _put('>>');
  _put('endobj');
end;

procedure TFPDF._putinfo;
var
  dt, tz, key, val: String;
  i: Integer;
begin
  dt := FormatDateTime('yyyymmddhhnnss', Self.CreationDate);
  if (Length(Self.TimeZone) = 6)  then
    tz := copy(Self.TimeZone,1,3)+#39+copy(Self.TimeZone,5,2)+#39
  else
    tz := Self.TimeZone;

  Self.metadata.Values['CreationDate'] := 'D:'+dt+tz;
  for i := 0 to Self.metadata.Count-1 do
  begin
    key := Self.metadata.Names[i];
    val := Self.metadata.ValueFromIndex[i];
    _put('/'+key+' '+_textstring(val));
  end;
end;

procedure TFPDF._putcatalog;
var
  vn: String;
begin
  vn := Self.PageInfo[0].Values['n'];
  _put('/Type /Catalog');
  _put('/Pages 1 0 R');

  case Self.ZoomMode of
    zmFullPage: _put('/OpenAction ['+vn+' 0 R /Fit]');
    zmFullWidth: _put('/OpenAction ['+vn+' 0 R /FitH null]');
    zmReal: _put('/OpenAction ['+vn+' 0 R /XYZ null null 1]');
    zmCustom:
      if (Self.ZoomFactor > 0) then
        _put('/OpenAction ['+vn+' 0 R /XYZ null null '+Format('%.2f', [Self.ZoomFactor/100], FPDFFormatSetings)+']');
  end;

  case Self.LayoutMode of
    lmSingle: _put('/PageLayout /SinglePage');
    lmContinuous: _put('/PageLayout /OneColumn');
    lmTwo: _put('/PageLayout /TwoColumnLeft');
  end;
end;

procedure TFPDF._putheader;
begin
  _put('%PDF-'+FloatToStr(Self.PDFVersion));
end;

procedure TFPDF._puttrailer();
begin
  _put('/Size '+IntToStr(Self.n+1));
  _put('/Root '+IntToStr(Self.n)+' 0 R');
  _put('/Info '+IntToStr(Self.n-1)+' 0 R');
end;

procedure TFPDF._enddoc;
var
  offset: Int64;
  i: Integer;
begin
  _putheader();
  _putpages();
  _putresources();

  // Info
  Self.CreationDate := Now;
  _newobj();
  _put('<<');
  _putinfo();
  _put('>>');
  _put('endobj');

  // Catalog
  _newobj();
  _put('<<');
  _putcatalog();
  _put('>>');
  _put('endobj');

  // Cross-ref
  offset := _getoffset();
  _put('xref');
  _put('0 '+IntToStr(Self.n+1));
  _put('0000000000 65535 f ');
  for i := 0 to Self.n-1 do
    _put(Format('%.10d 00000 n ', [Self.offsets[i]], FPDFFormatSetings));

  // Trailer
  _put('trailer');
  _put('<<');
  _puttrailer();
  _put('>>');
  _put('startxref');
  _put(IntToStr(offset));
  _put('%%EOF');
  Self.state := 3;

  Self.PageInfo.Clear;
  Self.cmaps.Clear;
  Self.encodings.Clear;
  SetLength(Self.pages, 0);
  SetLength(Self.PageLinks, 0);
  SetLength(Self.links, 0);
end;

{%region Utility Functions}

function TFPDF.ConvertTextToAnsi(const AText: String): String;
begin
  if Self.UseUTF8 then
    Result := Utf8ToAnsi(AText)
  else
    Result := AText;
end;

function TFPDF.FloatToStr(Value: Double): String;
begin
  Result := SysUtils.FloatToStr(Value, FPDFFormatSetings);
end;

procedure TFPDF.Image(img: TFPDFImageInfo; vX: Double; vY: Double;
  vWidth: Double; vHeight: Double; const vLink: String);
var
  x2: Double;
begin
  // Automatic width and height calculation if needed
  if ((vWidth=0) and (vHeight = 0)) then
  begin
    // Put image at 96 dpi
    vWidth := -96;
    vHeight := -96;
  end;

  if (vWidth < 0) then
    vWidth := -img.w*72/vWidth/Self.k;

  if (vHeight < 0) then
    vHeight := -img.h*72/vHeight/Self.k;

  if (vWidth = 0) then
    vWidth := vHeight*img.w/img.h;

  if (vHeight = 0) then
    vHeight := vWidth*img.h/img.w;

  // Flowing mode
  if (vY = -9999) then
  begin
    if ((Self.y+vHeight > Self.PageBreakTrigger) and (not Self.InHeader) and (not Self.InFooter) and (Self.AcceptPageBreak)) then
    begin
      // Automatic page break
      x2 := Self.x;
      AddPage(Self.CurOrientation, Self.CurPageSize, Self.CurRotation);
      Self.x := x2;
    end;

    vY := Self.y;
    Self.y := Self.y + vHeight;
  end;

  if (vX = -9999) then
    vX := Self.x;

  _out(Format('q %.2f 0 0 %.2f %.2f %.2f cm /I%d Do Q', [vWidth*Self.k, vHeight*Self.k, vX*Self.k, (Self.h-(vY+vHeight))*Self.k, img.i], FPDFFormatSetings));
  if (vLink <> '') then
    Link(vX, vY, vWidth, vHeight, vLink);
end;

procedure TFPDF.DefineDefaultPageSizes;
begin
  Self.StdPageSizes[pfA3].w := 841.89;
  Self.StdPageSizes[pfA3].h := 1190.55;
  Self.StdPageSizes[pfA4].w := 595.28;
  Self.StdPageSizes[pfA4].h := 841.89;
  Self.StdPageSizes[pfA5].w := 420.94;
  Self.StdPageSizes[pfA5].h := 595.28;
  Self.StdPageSizes[pfLetter].w := 612;
  Self.StdPageSizes[pfLetter].h := 792;
  Self.StdPageSizes[pfLegal].w := 612;
  Self.StdPageSizes[pfLegal].h := 1008;
end;

function TFPDF.FindUsedFontIndex(const AFontName: String): Integer;
var
  i, l: Integer;
begin
  l := Length(Self.UsedFonts);
  Result := 0;
  for i := 0 to l-1 do
  begin
    if LowerCase(Self.UsedFonts[i].FontName) = LowerCase(AFontName) then
    begin
      Result := i+1;
      Break;
    end;
  end;
end;

{%region Image Handle and Conversion}

function TFPDF._parseimage(vImageStream: TStream; const vImageExt: String): TFPDFImageInfo;
var
  ex: String;
begin
  Result.data := '';
  ex := UpperCase(Trim(vImageExt));
  if (ex = '') then
    Error('Image Stream without an extension!');

  if (ex = 'PNG') then
    Result := _parsepng(vImageStream)
  else if ((ex = 'JPG') or (ex = 'JPEG')) then
    Result := _parsejpg(vImageStream)
  else
    Error('Unsupported image formatting: ' + ex);
end;

{%endregion}

{%region Compression Methods }

function TFPDF.GzCompress(const StrIn: AnsiString;
  CompLevel: TCompressionLevel): AnsiString;
var
  cs: TCompressionStream;
  ms: TMemoryStream;
  l: Integer;
begin
  Result := '';
  ms := TMemoryStream.Create;
  try
    cs := TCompressionStream.Create(CompLevel, ms);
    try
      l := length(StrIn);
      cs.Write(StrIn[1], l);
    finally
      cs.Free;
    end;

    Setlength(Result, ms.Size);
    ms.Position := 0;
    l := ms.read(PAnsiChar(Result)^, ms.Size);
    SetLength(Result, l);
  finally
    ms.Free;
  end;
end;

function TFPDF.GzDecompress(const StrIn: AnsiString): AnsiString;
const
  bufsize = 65536;
var
  dcs: TDecompressionStream;
  ms: TMemoryStream;
  lb, lr: Integer;
  buf: AnsiString;
begin
  Result := '';
  ms := TMemoryStream.Create();
  try
    ms.Write(PAnsiChar(StrIn)^, Length(StrIn));
    ms.Position := 0;
    dcs := TDecompressionStream.Create(ms);
    try
      try
        repeat
          SetLength(buf, bufsize);
          lb := dcs.Read(buf[1], bufsize);
          lr := Length(Result);
          SetLength(Result, lr+lb);
          Move(buf[1], Result[lr+1], lb);
        until lb < bufsize;
      except
        Result := '';
        raise;
      end;
    finally
      dcs.Free;
    end;
  finally
    ms.Free;
  end;
end;

function SwapBytes(Value: Cardinal): Cardinal;
type
  Bytes = packed array[0..3] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[3];
  Bytes(Result)[1]:= Bytes(Value)[2];
  Bytes(Result)[2]:= Bytes(Value)[1];
  Bytes(Result)[3]:= Bytes(Value)[0];
end;

function SwapBytes(Value: Word): Word;
type
  Bytes = packed array[0..1] of Byte;
begin
  Bytes(Result)[0]:= Bytes(Value)[1];
  Bytes(Result)[1]:= Bytes(Value)[0];
end;

function Split(const AString: string; const ADelimiter: string = ' '): TStringArray;
var
  p1, p2, i: Integer;
begin
  SetLength(Result,0);
  i := 0;
  p1 := 1;
  p2 := pos(ADelimiter, AString);
  if (p2 = 0) and (AString <> '') then
    p2 := Length(AString) + 1;
  while (p2 > 0) do
  begin
    Inc(i);
    SetLength(Result, i);
    Result[i-1] := TrimLeft(copy(AString, p1, (p2-p1)));
    p1 := p2+1;
    p2 := PosEx(ADelimiter, AString + ADelimiter, p1);
  end;
end;

function CountStr(const AString, SubStr : String ) : Integer ;
Var
  i: Integer ;
begin
  Result := 0;
  if (AString = '') or (SubStr = '') then
    Exit;

  i := Pos(SubStr, AString);
  while (i > 0) do
  begin
     Inc(Result);
     i := PosEx(SubStr, AString, i+1);
  end;
end;

initialization
  with FPDFFormatSetings do
  begin
    CurrencyString := '$';
    CurrencyFormat := 1;
    CurrencyDecimals := 2;
    DateSeparator := '-';
    TimeSeparator := ':';
    ListSeparator := ',';
    ShortDateFormat := 'd/m/y';
    LongDateFormat := 'dd" "mmmm" "yyyy';
    TimeAMString := 'AM';
    TimePMString := 'PM';
    ShortTimeFormat := 'hh:nn';
    LongTimeFormat := 'hh:nn:ss';
    ThousandSeparator := #0;
    DecimalSeparator := '.';
    TwoDigitYearCenturyWindow := 50;
    NegCurrFormat := 5;
  end;
end.

