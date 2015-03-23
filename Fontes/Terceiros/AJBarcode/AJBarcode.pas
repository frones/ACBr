unit AJBarcode;

{$I ACBr.inc}

{
Barcode Component
Version 1.27 (27.10.2004)
Copyright 1998-2004 Andreas Schmidt and friends

for use with Delphi 1 - 7
Delphi 1 not tested; better use Delphi 2 (or higher)

Freeware
Feel free to distribute the component as
long as all files are unmodified and kept together.

I'am not responsible for wrong barcodes.

bug-reports, enhancements:
mailto:shmia@bizerba.de or a_j_schmidt@rocketmail.com

please tell me wich version you are using, when mailing me.


get latest version from
http://members.tripod.de/AJSchmidt/index.html
http://mitglied.lycos.de/AJSchmidt/fbarcode.zip


many thanx and geetings to
Nikolay Simeonov, Wolfgang Koranda, Norbert Waas,
Richard Hugues, Olivier Guilbaud, Berend Tober, Jan Tungli,
Mauro Lemes, Norbert Kostka, Frank De Prins, Shane O'Dea,
Daniele Teti, Ignacio Trivino, Samuel J. Comstock, Roberto Parola,
Stefano Torricella and Mariusz Mialkon.

i use tabs:  1 tab = 3 spaces


History:
----------------------------------------------------------------------
Version 1.0:
- initial release
Version 1.1:
- more comments
- changed function Code_93Extended (now correct ?)
Version 1.2:
- Bugs (found by Nikolay Simeonov) removed
Version 1.3:
- EAN8/EAN13 added by Wolfgang Koranda (wkoranda@csi.com)
Version 1.4:
- Bug (found by Norbert Waas) removed
  Component must save the Canvas-properties Font,Pen and Brush
Version 1.5:
- Bug (found by Richard Hugues) removed
  Last line of barcode was 1 Pixel too wide
Version 1.6:
- new read-only property 'Width'
Version 1.7
- check for numeric barcode types
- compatible with Delphi 1 (i hope)
Version 1.8
- add Color and ColorBar properties
Version 1.9
- Code 128 C added by Jan Tungli
Version 1.10
- Bug in Code 39 Character I removed
Version 1.11 (06.07.1999)
- additional Code Types
  CodeUPC_A,
  CodeUPC_E0,
  CodeUPC_E1,
  CodeUPC_Supp2,
  CodeUPC_Supp5
  by Jan Tungli
Version 1.12 (13.07.1999)
- improved ShowText property by Mauro Lemes
  you must change your applications due changed interface of TBarcode.
Version 1.13 (23.07.1999)
- additional Code Types
  CodeEAN128A,
  CodeEAN128B,
  CodeEAN128C
  (support by Norbert Kostka)
- new property 'CheckSumMethod'
Version 1.14 (29.07.1999)
- checksum for EAN128 by Norbert Kostka
- bug fix for EAN128C
Version 1.15 (23.09.1999)
- bug fix for Code 39 with checksum by Frank De Prins
Version 1.16 (10.11.1999)
- width property is now writable (suggestion by Shane O'Dea)
Version 1.17 (27.06.2000)
- new OnChange property
- renamed TBarcode to TAsBarcode to avoid name conflicts
Version 1.18 (25.08.2000)
- some speed improvements (Code 93 and Code 128)
Version 1.19 (27.09.2000)
  (thanks to Samuel J. Comstock)
- origin of the barcode (left upper edge) is moved so that
  the barcode stays always on the canvas
- new (read only) properties 'CanvasWidth' and 'CanvasHeight' gives you
  the size of the resulting image.
- a wrapper class for Quick Reports is now available.
Version 1.20 (13.09.2000)
- Assign procedure added
- support for scaling barcode to Printer (see Demo)
Version 1.21 (19.07.2001)
  (thanks to Roberto Parola)
- new properties ShowTextFont and ShowTextPosition
Version 1.22 (26.10.2001)
- Code 128 Symbol #12 (=comma) fixed (thanks to Stefano Torricella)
Version 1.23 (13.11.2002)
- UPC_E0 and UPC_E1 stopcodes fixed (thanks to Duo Dreamer)
Version 1.24 (04.12.2002)
- Bugfix for Code93 Extended
Version 1.25 (15.05.2003)
- fixed a bug in procedure Assign (thanks to Mariusz Mialkon)
Version 1.26 (27.05.2004)
- fixed a bug for Code93 (wrong checksum calculation for barcode with more than 14 chars)
Version 1.27 (27.10.2004)
- added Code128 Charset A control codes from 0 to 31 


Todo (missing features)
-----------------------

- more CheckSum Methods
- user defined barcodes
- checksum event (fired when the checksum is calculated)
- rename the unit name (from 'barcode' to 'fbarcode') to avoid name conflicts
- I'am working on PDF417 barcode (has anybody some technical information about PDF417
  or a PDF417 reader ?)



Known Bugs
---------
- Top and Left properties must be set at runtime.
- comments not compatible with Delphi 1
}



interface

uses
  Classes,
  {$IFDEF COMPILER6_UP} Types, {$ELSE} Windows,{$ENDIF}
  {$IFDEF VisualCLX}
   QGraphics
  {$ELSE}
   Graphics
  {$ENDIF} ;

type
  TBarcodeType =
  (
  bcCode_2_5_interleaved,
  bcCode_2_5_industrial,
  bcCode_2_5_matrix,
  bcCode39,
  bcCode39Extended,
  bcCode128A,
  bcCode128B,
  bcCode128C,
  bcCode93,
  bcCode93Extended,
  bcCodeMSI,
  bcCodePostNet,
  bcCodeCodabar,
  bcCodeEAN8,
  bcCodeEAN13,
  bcCodeUPC_A,
  bcCodeUPC_E0,
  bcCodeUPC_E1,
  bcCodeUPC_Supp2,    { UPC 2 digit supplemental }
  bcCodeUPC_Supp5,    { UPC 5 digit supplemental }
  bcCodeEAN128A,
  bcCodeEAN128B,
  bcCodeEAN128C
  );


  TBarLineType = (white, black, black_half);  {for internal use only}
  { black_half means a black line with 2/5 height (used for PostNet) }


  TBarcodeOption = (bcoNone, bcoCode, bcoTyp, bcoBoth); { Type of text to show }

// Additions from Roberto Parola to improve the text output
  TShowTextPosition =
  (
    stpTopLeft,
    stpTopRight,
    stpTopCenter,
    stpBottomLeft,
    stpBottomRight,
    stpBottomCenter
  );
//


  TCheckSumMethod =
  (
  csmNone,
  csmModulo10
  );


  TAsBarcode = class(TComponent)
  private
    { Private-Deklarationen }
    FHeight : integer;
    FText  : string;
    FTop    : integer;
    FLeft   : integer;
    FModul  : integer;
    FRatio  : double;
    FTyp    : TBarcodeType;
    FCheckSum:boolean;
    FShowText:TBarcodeOption;
    FAngle  : double;
    FColor  : TColor;
    FColorBar:TColor;
    FCheckSumMethod : TCheckSumMethod;
      FOnChange : TNotifyEvent;


    modules:array[0..3] of shortint;
    FShowTextFont: TFont;
    FShowTextPosition: TShowTextPosition;


    procedure OneBarProps(code:char; var Width:integer; var lt:TBarLineType);

    procedure DoLines(data:string; Canvas:TCanvas);

    function SetLen(pI:byte):string;

    function Code_2_5_interleaved:string;
    function Code_2_5_industrial:string;
    function Code_2_5_matrix:string;
    function Code_39:string;
    function Code_39Extended:string;
    function Code_128:string;
    function Code_93:string;
    function Code_93Extended:string;
    function Code_MSI:string;
    function Code_PostNet:string;
    function Code_Codabar:string;
    function Code_EAN8:string;
    function Code_EAN13:string;
    function Code_UPC_A:string;
    function Code_UPC_E0:string;
    function Code_UPC_E1:string;
    function Code_Supp5:string;
    function Code_Supp2:string;

    function GetTypText:string;
    procedure MakeModules;

    procedure SetModul(v:integer);

    function GetWidth : integer;
    procedure SetWidth(Value :integer);

      procedure SetRatio(const Value: Double);
      procedure SetTyp(const Value: TBarcodeType);
      procedure SetAngle(const Value: Double);
      procedure SetText(const Value: string);
      procedure SetShowText(const Value: TBarcodeOption);
      procedure SetTop(const Value: Integer);
      procedure SetLeft(const Value: Integer);
      procedure SetCheckSum(const Value: Boolean);
    procedure SetHeight(const Value: integer);
    function GetCanvasHeight: Integer;
    function GetCanvasWidth: Integer;
// Additions from Roberto Parola to improve the text output
    procedure SetShowTextFont(const Value: TFont);
    procedure SetShowTextPosition(const Value: TShowTextPosition);

  protected
    { Protected-Deklarationen }
    function MakeData : string;
      procedure DoChange; virtual;

  public
    { Public-Deklarationen }
    constructor Create(Owner:TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;

    function DoCheckSumming(const data : string):string;
    procedure DrawBarcode(Canvas:TCanvas);
    procedure DrawText(Canvas:TCanvas);
    property CanvasHeight :Integer read GetCanvasHeight;
    property CanvasWidth :Integer read GetCanvasWidth;
  published
    { Published-Deklarationen }
   { Height of Barcode (Pixel)}
    property Height : integer read FHeight write SetHeight;
    property Text   : string read FText write SetText;
    property Top    : Integer read FTop write SetTop;
    property Left   : Integer read FLeft write SetLeft;
   { Width of the smallest line in a Barcode }
    property Modul  : integer read FModul  write SetModul;
    property Ratio  : Double read FRatio write SetRatio;
    property Typ    : TBarcodeType read FTyp write SetTyp default bcCode_2_5_interleaved;
   { build CheckSum ? }
    property Checksum:boolean read FCheckSum write SetCheckSum default FALSE;
    property CheckSumMethod:TCheckSumMethod read FCheckSumMethod write FCheckSumMethod default csmModulo10;

   { 0 - 360 degree }
    property Angle  :double read FAngle write SetAngle;

    property ShowText:TBarcodeOption read FShowText write SetShowText default bcoNone;
    property ShowTextFont: TFont read FShowTextFont write SetShowTextFont;
    property ShowTextPosition: TShowTextPosition read FShowTextPosition write SetShowTextPosition default stpTopLeft;
    property Width : integer read GetWidth write SetWidth stored False;
    property Color:TColor read FColor write FColor default clWhite;
    property ColorBar:TColor read FColorBar write FColorBar default clBlack;
      property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

{ used for EAN 8/13 }
function CheckSumModulo10(const data:string):string;

implementation

uses SysUtils, Math;


{$DEFINE ASSERT_SUPPORTED}

{$IFDEF VER80}
{$UNDEF ASSERT_SUPPORTED}
{$ENDIF}
{$IFDEF VER90}
{$UNDEF ASSERT_SUPPORTED}
{$ENDIF}
{$IFDEF VER100}
{$UNDEF ASSERT_SUPPORTED}
{$ENDIF}
{$IFDEF VER110}
{$UNDEF ASSERT_SUPPORTED}
{$ENDIF}


type
  TBCdata = record
   Name:string;        { Name of Barcode }
   num :Boolean;       { numeric data only }
  end;

const BCdata:array[bcCode_2_5_interleaved..bcCodeEAN128C] of TBCdata =
  (
    (Name:'2_5_interleaved'; num:True),
    (Name:'2_5_industrial';  num:True),
    (Name:'2_5_matrix';      num:True),
    (Name:'Code39';          num:False),
    (Name:'Code39 Extended'; num:False),
    (Name:'Code128A';        num:False),
    (Name:'Code128B';        num:False),
    (Name:'Code128C';        num:True),
    (Name:'Code93';          num:False),
    (Name:'Code93 Extended'; num:False),
    (Name:'MSI';             num:True),
    (Name:'PostNet';         num:True),
    (Name:'Codebar';         num:False),
    (Name:'EAN8';            num:True),
    (Name:'EAN13';           num:True),
    (Name:'UPC_A';           num:True),
    (Name:'UPC_E0';          num:True),
    (Name:'UPC_E1';          num:True),
    (Name:'UPC Supp2';       num:True),
    (Name:'UPC Supp5';       num:True),
    (Name:'EAN128A';         num:False),
    (Name:'EAN128B';         num:False),
    (Name:'EAN128C';         num:True)
  );

function CheckSumModulo10(const data:string):string;
	var i,fak,sum : Integer;
begin
	sum := 0;
	fak := Length(data);
	for i:=1 to Length(data) do
	begin
		if (fak mod 2) = 0 then
			sum := sum + (StrToInt(data[i])*1)
		else
			sum := sum + (StrToInt(data[i])*3);
		dec(fak);
	end;
	if (sum mod 10) = 0 then
		result := data+'0'
	else
		result := data+IntToStr(10-(sum mod 10));
end;


{
  converts a string from '321' to the internal representation '715'
  i need this function because some pattern tables have a different
  format :

  '00111'
  converts to '05161'
}
function Convert(const s:string):string;
var
  i, v : integer;
begin
  Result := s;  { same Length as Input - string }
  for i:=1 to Length(s) do
  begin
    v := ord(s[i]) - 1;

    if odd(i) then
      Inc(v, 5);
    Result[i] := Chr(v);
  end;
end;

(*
 * Berechne die Quersumme aus einer Zahl x
 * z.B.: Quersumme von 1234 ist 10
 *)
function quersumme(x:integer):integer;
var
  sum:integer;
begin
  sum := 0;

  while x > 0 do
  begin
    sum := sum + (x mod 10);
    x := x div 10;
  end;
  result := sum;
end;


{
  Rotate a Point by Angle 'alpha'
}
function Rotate2D(p:TPoint; alpha:double): TPoint;
var
  sinus, cosinus : Extended;
begin
(*
  sinus   := sin(alpha);
  cosinus := cos(alpha);
*)
  { twice as fast than calc sin() and cos() }
  SinCos(alpha, sinus, cosinus);

  result.x := Round(p.x*cosinus + p.y*sinus);
  result.y := Round(-p.x*sinus + p.y*cosinus);
end;

{
  Move Point "a" by Vector "b"
}
function Translate2D(a, b:TPoint): TPoint;
begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
end;

(*
  not used, but left in place for future use
procedure Rotate2Darray(p:array of TPoint; alpha:double);
var
   i : Integer;
begin
   for i:=Low(p) to High(p) do
      p[i] := Rotate2D(p[i], alpha);
end;

procedure Translate2Darray(p:array of TPoint; shift:TPoint);
var
   i : Integer;
begin
   for i:=Low(p) to High(p) do
      p[i] := Translate2D(p[i], shift);
end;
*)

{
  Move the orgin so that when point is rotated by alpha, the rect
  between point and orgin stays in the visible quadrant.
}
function TranslateQuad2D(const alpha :double; const orgin, point :TPoint): TPoint;
var
   alphacos: Extended;
   alphasin: Extended;
   moveby:   TPoint;
begin
   SinCos(alpha, alphasin, alphacos);
   {
   SinCos is twice as fast as:
   alphasin := sin(alpha);
   alphacos := cos(alpha);
   }

   if alphasin >= 0 then
   begin
      if alphacos >= 0 then
      begin
         { 1. Quadrant }
         moveby.x := 0;
         moveby.y := Round(alphasin*point.x);
      end
      else
      begin
         { 2. Quadrant }
         moveby.x := -Round(alphacos*point.x);
         moveby.y := Round(alphasin*point.x - alphacos*point.y);
      end;
   end
   else
   begin
      if alphacos >= 0 then
      begin
         { 4. quadrant }
         moveby.x := -Round(alphasin*point.y);
         moveby.y := 0;
      end
      else
      begin
         { 3. quadrant }
         moveby.x := -Round(alphacos*point.x) - Round(alphasin*point.y);
         moveby.y := -Round(alphacos*point.y);
      end;
   end;
   Result := Translate2D(orgin, moveby);
end;


constructor TAsBarcode.Create(Owner:TComponent);
begin
  inherited Create(owner);
  FAngle := 0.0;
  FRatio := 2.0;
  FModul := 1;
  FTyp   := bcCodeEAN13;
  FCheckSum := FALSE;
  FCheckSumMethod := csmModulo10;
  FShowText := bcoNone;
  FColor    := clWhite;
  FColorBar := clBlack;
  FShowTextFont := TFont.Create;
  FShowTextPosition := stpTopLeft;
end;

destructor TAsBarcode.Destroy;
begin
  FShowTextFont.Free;
  inherited;
end;

procedure TAsBarcode.Assign(Source: TPersistent);
var
   BSource : TAsBarcode;
begin
   if Source is TAsBarcode then
   begin
      BSource    := TAsBarcode(Source);
      FHeight    := BSource.FHeight;
      FText      := BSource.FText;
      FTop       := BSource.FTop;
      FLeft      := BSource.FLeft;
      FModul     := BSource.FModul;
      FRatio     := BSource.FRatio;
      FTyp       := BSource.FTyp;
      FCheckSum  := BSource.FCheckSum;
      FShowText  := BSource.FShowText;
      FShowTextPosition := BSource.FShowTextPosition;  // 15.05.2003
      FAngle     := BSource.FAngle;
      FColor     := BSource.FColor;
      FColorBar  := BSource.FColorBar;
      FCheckSumMethod := BSource.FCheckSumMethod;
      FOnChange  := BSource.FOnChange;
   end
   else
      inherited;  // 15.05.2003
end;


function TAsBarcode.GetTypText:string;
begin
  result := BCdata[FTyp].Name;
end;



{ set Modul Width  }
procedure TAsBarcode.SetModul(v:integer);
begin
  if (v >= 1) and (v < 50) then
   begin
    FModul := v;
      DoChange;
   end;
end;


{
calculate the width and the linetype of a sigle bar


  Code   Line-Color      Width               Height
------------------------------------------------------------------
  '0'   white           100%                full
  '1'   white           100%*Ratio          full
  '2'   white           150%*Ratio          full
  '3'   white           200%*Ratio          full
  '5'   black           100%                full
  '6'   black           100%*Ratio          full
  '7'   black           150%*Ratio          full
  '8'   black           200%*Ratio          full
  'A'   black           100%                2/5  (used for PostNet)
  'B'   black           100%*Ratio          2/5  (used for PostNet)
  'C'   black           150%*Ratio          2/5  (used for PostNet)
  'D'   black           200%*Ratio          2/5  (used for PostNet)
}
procedure TAsBarcode.OneBarProps(code:char; var Width:integer; var lt:TBarLineType);
begin
  case code of
    '0': begin width := modules[0]; lt := white; end;
    '1': begin width := modules[1]; lt := white; end;
    '2': begin width := modules[2]; lt := white; end;
    '3': begin width := modules[3]; lt := white; end;


    '5': begin width := modules[0]; lt := black; end;
    '6': begin width := modules[1]; lt := black; end;
    '7': begin width := modules[2]; lt := black; end;
    '8': begin width := modules[3]; lt := black; end;

    'A': begin width := modules[0]; lt := black_half; end;
    'B': begin width := modules[1]; lt := black_half; end;
    'C': begin width := modules[2]; lt := black_half; end;
    'D': begin width := modules[3]; lt := black_half; end;
  else
    begin
   {something went wrong  :-(  }
   {mistyped pattern table}
    raise Exception.CreateFmt('%s: internal Error', [self.ClassName]);
    end;
  end;
end;


function TAsBarcode.MakeData : string;
var
  i : integer;
begin
  {calculate the with of the different lines (modules)}
  MakeModules;


  {numeric barcode type ?}
  if BCdata[Typ].num then
  begin
   FText := Trim(FText); {remove blanks}
    for i := 1 to Length(Ftext) do
      if (FText[i] > '9') or (FText[i] < '0') then
        raise Exception.Create('Barcode must be numeric');
  end;


  {get the pattern of the barcode}
  case Typ of
    bcCode_2_5_interleaved: Result := Code_2_5_interleaved;
    bcCode_2_5_industrial:  Result := Code_2_5_industrial;
    bcCode_2_5_matrix:      Result := Code_2_5_matrix;
    bcCode39:               Result := Code_39;
    bcCode39Extended:       Result := Code_39Extended;
    bcCode128A,
    bcCode128B,
    bcCode128C,
    bcCodeEAN128A,
    bcCodeEAN128B,
    bcCodeEAN128C:          Result := Code_128;
    bcCode93:               Result := Code_93;
    bcCode93Extended:       Result := Code_93Extended;
    bcCodeMSI:              Result := Code_MSI;
    bcCodePostNet:          Result := Code_PostNet;
    bcCodeCodabar:          Result := Code_Codabar;
    bcCodeEAN8:             Result := Code_EAN8;
    bcCodeEAN13:            Result := Code_EAN13;
    bcCodeUPC_A:            Result := Code_UPC_A;
    bcCodeUPC_E0:           Result := Code_UPC_E0;
    bcCodeUPC_E1:           Result := Code_UPC_E1;
    bcCodeUPC_Supp2:        Result := Code_Supp2;
    bcCodeUPC_Supp5:        Result := Code_Supp5;
  else
    raise Exception.CreateFmt('%s: wrong BarcodeType', [self.ClassName]);
  end;

{
Showmessage(Format('Data <%s>', [Result]));
}
end;



function TAsBarcode.GetWidth:integer;
var
  data : string;
  i : integer;
  w : integer;
  lt : TBarLineType;
begin
  Result := 0;

  {get barcode pattern}
  data := MakeData;

  for i:=1 to Length(data) do  {examine the pattern string}
  begin
    OneBarProps(data[i], w, lt);
    Inc(Result, w);
  end;
end;

procedure TAsBarcode.SetWidth(Value :integer);
var
  data : string;
  i : integer;
  w, wtotal : integer;
  lt : TBarLineType;
begin
  wtotal := 0;

  {get barcode pattern}
  data := MakeData;

  for i:=1 to Length(data) do  {examine the pattern string}
  begin
    OneBarProps(data[i], w, lt);
    Inc(wtotal, w);
  end;


  {
  wtotal:  current width of barcode
  Value :  new width of barcode



  }

  if wtotal > 0 then  { don't divide by 0 ! }
    SetModul((FModul * Value) div wtotal);
end;



function TAsBarcode.DoCheckSumming(const data : string):string;
begin
  case FCheckSumMethod of

    csmNone:
      Result := data;
    csmModulo10:
      Result := CheckSumModulo10(data);

  end;
end;




{
////////////////////////////// EAN /////////////////////////////////////////
}


{
////////////////////////////// EAN8 /////////////////////////////////////////
}

{Pattern for Barcode EAN Charset A}
     {L1   S1   L2   S2}
const tabelle_EAN_A:array['0'..'9'] of string =
  (
  ('2605'),    { 0 }
  ('1615'),    { 1 }
  ('1516'),    { 2 }
  ('0805'),    { 3 }
  ('0526'),    { 4 }
  ('0625'),    { 5 }
  ('0508'),    { 6 }
  ('0706'),    { 7 }
  ('0607'),    { 8 }
  ('2506')     { 9 }
  );

{Pattern for Barcode EAN Charset C}
     {S1   L1   S2   L2}
const tabelle_EAN_C:array['0'..'9'] of string =
  (
  ('7150' ),    { 0 }
  ('6160' ),    { 1 }
  ('6061' ),    { 2 }
  ('5350' ),    { 3 }
  ('5071' ),    { 4 }
  ('5170' ),    { 5 }
  ('5053' ),    { 6 }
  ('5251' ),    { 7 }
  ('5152' ),    { 8 }
  ('7051' )     { 9 }
  );


function TAsBarcode.Code_EAN8:string;
var
  i : integer;
  tmp : String;
begin
  if FCheckSum then
  begin
    tmp := SetLen(7);
    tmp := DoCheckSumming(copy(tmp,length(tmp)-6,7));
  end
  else
    tmp := SetLen(8);

{$IFDEF ASSERT_SUPPORTED}
  Assert(Length(tmp)=8, 'Invalid Text len (EAN8)');
{$ENDIF}
  result := '505';   {Startcode}

  for i:=1 to 4 do
    result := result + tabelle_EAN_A[tmp[i]] ;

  result := result + '05050';   {Center Guard Pattern}

  for i:=5 to 8 do
    result := result + tabelle_EAN_C[tmp[i]] ;

  result := result + '505';   {Stopcode}
end;

{////////////////////////////// EAN13 ///////////////////////////////////////}

{Pattern for Barcode EAN Zeichensatz B}
     {L1   S1   L2   S2}
const tabelle_EAN_B:array['0'..'9'] of string =
  (
  ('0517'),    { 0 }
  ('0616'),    { 1 }
  ('1606'),    { 2 }
  ('0535'),    { 3 }
  ('1705'),    { 4 }
  ('0715'),    { 5 }
  ('3505'),    { 6 }
  ('1525'),    { 7 }
  ('2515'),    { 8 }
  ('1507')     { 9 }
  );

{Zuordung der Paraitaetsfolgen für EAN13}
const tabelle_ParityEAN13:array[0..9, 1..6] of char =
  (
  ('A', 'A', 'A', 'A', 'A', 'A'),    { 0 }
  ('A', 'A', 'B', 'A', 'B', 'B'),    { 1 }
  ('A', 'A', 'B', 'B', 'A', 'B'),    { 2 }
  ('A', 'A', 'B', 'B', 'B', 'A'),    { 3 }
  ('A', 'B', 'A', 'A', 'B', 'B'),    { 4 }
  ('A', 'B', 'B', 'A', 'A', 'B'),    { 5 }
  ('A', 'B', 'B', 'B', 'A', 'A'),    { 6 }
  ('A', 'B', 'A', 'B', 'A', 'B'),    { 7 }
  ('A', 'B', 'A', 'B', 'B', 'A'),    { 8 }
  ('A', 'B', 'B', 'A', 'B', 'A')     { 9 }
  );

function TAsBarcode.Code_EAN13:string;
var
  i, LK: integer;
  tmp : String;
begin
  if FCheckSum then
  begin
    tmp := SetLen(12);
    tmp := DoCheckSumming(tmp);
  end
  else
    tmp := SetLen(13);

{$IFDEF ASSERT_SUPPORTED}
  Assert(Length(tmp) = 13, 'Invalid Text len (EAN13)');
{$ENDIF}

  LK := StrToInt(tmp[1]);
  tmp := copy(tmp,2,12);

  result := '505';   {Startcode}

  for i:=1 to 6 do
  begin
    case tabelle_ParityEAN13[LK,i] of
      'A' : result := result + tabelle_EAN_A[tmp[i]];
      'B' : result := result + tabelle_EAN_B[tmp[i]] ;
      'C' : result := result + tabelle_EAN_C[tmp[i]] ;
  end;
  end;

  result := result + '05050';   {Center Guard Pattern}

  for i:=7 to 12 do
    result := result + tabelle_EAN_C[tmp[i]] ;

    result := result + '505';   {Stopcode}
end;

{Pattern for Barcode 2 of 5}
const tabelle_2_5:array['0'..'9', 1..5] of char =
  (
  ('0', '0', '1', '1', '0'),    {'0'}
  ('1', '0', '0', '0', '1'),    {'1'}
  ('0', '1', '0', '0', '1'),    {'2'}
  ('1', '1', '0', '0', '0'),    {'3'}
  ('0', '0', '1', '0', '1'),    {'4'}
  ('1', '0', '1', '0', '0'),    {'5'}
  ('0', '1', '1', '0', '0'),    {'6'}
  ('0', '0', '0', '1', '1'),    {'7'}
  ('1', '0', '0', '1', '0'),    {'8'}
  ('0', '1', '0', '1', '0')     {'9'}
  );

function TAsBarcode.Code_2_5_interleaved:string;
var
  i, j: integer;
  c : char;

begin
  result := '5050';   {Startcode}

  for i:=1 to Length(FText) div 2 do
  begin
    for j:= 1 to 5 do
    begin
      if tabelle_2_5[FText[i*2-1], j] = '1' then
        c := '6'
      else
        c := '5';
      result := result + c;
      if tabelle_2_5[FText[i*2], j] = '1' then
        c := '1'
      else
        c := '0';
      result := result + c;
    end;
  end;

  result := result + '605';    {Stopcode}
end;


function TAsBarcode.Code_2_5_industrial:string;
var
  i, j: integer;
begin
  result := '606050';   {Startcode}

  for i:=1 to Length(FText) do
  begin
    for j:= 1 to 5 do
    begin
    if tabelle_2_5[FText[i], j] = '1' then
      result := result + '60'
    else
      result := result + '50';
    end;
  end;

  result := result + '605060';   {Stopcode}
end;

function TAsBarcode.Code_2_5_matrix:string;
var
  i, j: integer;
  c :char;
begin
  result := '705050';   {Startcode}

  for i:=1 to Length(FText) do
  begin
    for j:= 1 to 5 do
    begin
      if tabelle_2_5[FText[i], j] = '1' then
        c := '1'
      else
        c := '0';

    {Falls i ungerade ist dann mache Lücke zu Strich}
      if odd(j) then
        c := chr(ord(c)+5);
      result := result + c;
    end;
   result := result + '0';   {Lücke zwischen den Zeichen}
  end;

  result := result + '70505';   {Stopcode}
end;


function TAsBarcode.Code_39:string;

type TCode39 =
  record
    c : char;
    data : array[0..9] of char;
    chk: shortint;
  end;

const tabelle_39: array[0..43] of TCode39 = (
  ( c:'0'; data:'505160605'; chk:0 ),
  ( c:'1'; data:'605150506'; chk:1 ),
  ( c:'2'; data:'506150506'; chk:2 ),
  ( c:'3'; data:'606150505'; chk:3 ),
  ( c:'4'; data:'505160506'; chk:4 ),
  ( c:'5'; data:'605160505'; chk:5 ),
  ( c:'6'; data:'506160505'; chk:6 ),
  ( c:'7'; data:'505150606'; chk:7 ),
  ( c:'8'; data:'605150605'; chk:8 ),
  ( c:'9'; data:'506150605'; chk:9 ),
  ( c:'A'; data:'605051506'; chk:10),
  ( c:'B'; data:'506051506'; chk:11),
  ( c:'C'; data:'606051505'; chk:12),
  ( c:'D'; data:'505061506'; chk:13),
  ( c:'E'; data:'605061505'; chk:14),
  ( c:'F'; data:'506061505'; chk:15),
  ( c:'G'; data:'505051606'; chk:16),
  ( c:'H'; data:'605051605'; chk:17),
  ( c:'I'; data:'506051605'; chk:18),
  ( c:'J'; data:'505061605'; chk:19),
  ( c:'K'; data:'605050516'; chk:20),
  ( c:'L'; data:'506050516'; chk:21),
  ( c:'M'; data:'606050515'; chk:22),
  ( c:'N'; data:'505060516'; chk:23),
  ( c:'O'; data:'605060515'; chk:24),
  ( c:'P'; data:'506060515'; chk:25),
  ( c:'Q'; data:'505050616'; chk:26),
  ( c:'R'; data:'605050615'; chk:27),
  ( c:'S'; data:'506050615'; chk:28),
  ( c:'T'; data:'505060615'; chk:29),
  ( c:'U'; data:'615050506'; chk:30),
  ( c:'V'; data:'516050506'; chk:31),
  ( c:'W'; data:'616050505'; chk:32),
  ( c:'X'; data:'515060506'; chk:33),
  ( c:'Y'; data:'615060505'; chk:34),
  ( c:'Z'; data:'516060505'; chk:35),
  ( c:'-'; data:'515050606'; chk:36),
  ( c:'.'; data:'615050605'; chk:37),
  ( c:' '; data:'516050605'; chk:38),
  ( c:'*'; data:'515060605'; chk:0 ),
  ( c:'$'; data:'515151505'; chk:39),
  ( c:'/'; data:'515150515'; chk:40),
  ( c:'+'; data:'515051515'; chk:41),
  ( c:'%'; data:'505151515'; chk:42)
  );


function FindIdx(z:char):integer;
var
  i:integer;
begin
  for i:=0 to High(tabelle_39) do
  begin
    if z = tabelle_39[i].c then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

var
  i, idx : integer;
  checksum:integer;

begin
  checksum := 0;
  {Startcode}
  Result := tabelle_39[FindIdx('*')].data ;
  Result := Result + '0';

  for i:=1 to Length(FText) do
  begin
    idx := FindIdx(FText[i]);
    if idx < 0 then
      continue;
    result := result + tabelle_39[idx].data + '0';
    Inc(checksum, tabelle_39[idx].chk);
  end;

  {Calculate Checksum Data}
  if FCheckSum then
    begin
    checksum := checksum mod 43;
    for i:=0 to High(tabelle_39) do
      if checksum = tabelle_39[i].chk then
      begin
        result := result + tabelle_39[i].data + '0';
        break;
      end;
    end;

  {Stopcode}
  result := result + tabelle_39[FindIdx('*')].data;
end;

function TAsBarcode.Code_39Extended:string;

const code39x : array[0..127] of string[2] =
  (
  ('%U'), ('$A'), ('$B'), ('$C'), ('$D'), ('$E'), ('$F'), ('$G'),
  ('$H'), ('$I'), ('$J'), ('$K'), ('$L'), ('$M'), ('$N'), ('$O'),
  ('$P'), ('$Q'), ('$R'), ('$S'), ('$T'), ('$U'), ('$V'), ('$W'),
  ('$X'), ('$Y'), ('$Z'), ('%A'), ('%B'), ('%C'), ('%D'), ('%E'),
   (' '), ('/A'), ('/B'), ('/C'), ('/D'), ('/E'), ('/F'), ('/G'),
  ('/H'), ('/I'), ('/J'), ('/K'), ('/L'), ('/M'), ('/N'), ('/O'),
  ( '0'),  ('1'),  ('2'),  ('3'),  ('4'),  ('5'),  ('6'),  ('7'),
   ('8'),  ('9'), ('/Z'), ('%F'), ('%G'), ('%H'), ('%I'), ('%J'),
  ('%V'),  ('A'),  ('B'),  ('C'),  ('D'),  ('E'),  ('F'),  ('G'),
   ('H'),  ('I'),  ('J'),  ('K'),  ('L'),  ('M'),  ('N'),  ('O'),
   ('P'),  ('Q'),  ('R'),  ('S'),  ('T'),  ('U'),  ('V'),  ('W'),
   ('X'),  ('Y'),  ('Z'), ('%K'), ('%L'), ('%M'), ('%N'), ('%O'),
  ('%W'), ('+A'), ('+B'), ('+C'), ('+D'), ('+E'), ('+F'), ('+G'),
  ('+H'), ('+I'), ('+J'), ('+K'), ('+L'), ('+M'), ('+N'), ('+O'),
  ('+P'), ('+Q'), ('+R'), ('+S'), ('+T'), ('+U'), ('+V'), ('+W'),
  ('+X'), ('+Y'), ('+Z'), ('%P'), ('%Q'), ('%R'), ('%S'), ('%T')
  );


var
  save:string;
  i : integer;
begin
  save := FText;
  FText := '';

  for i:=1 to Length(save) do
  begin
    if ord(save[i]) <= 127 then
      FText := FText + code39x[ord(save[i])];
  end;
  result := Code_39;
  FText := save;
end;



{
Code 128
}
function TAsBarcode.Code_128:string;
type TCode128 =
  record
    a, b : char;
    c : string[2];
    data : string[6];
  end;

const tabelle_128: array[0..102] of TCode128 = (
  ( a:' '; b:' '; c:'00'; data:'212222' ),
  ( a:'!'; b:'!'; c:'01'; data:'222122' ),
  ( a:'"'; b:'"'; c:'02'; data:'222221' ),
  ( a:'#'; b:'#'; c:'03'; data:'121223' ),
  ( a:'$'; b:'$'; c:'04'; data:'121322' ),
  ( a:'%'; b:'%'; c:'05'; data:'131222' ),
  ( a:'&'; b:'&'; c:'06'; data:'122213' ),
  ( a:''''; b:''''; c:'07'; data:'122312' ),
  ( a:'('; b:'('; c:'08'; data:'132212' ),
  ( a:')'; b:')'; c:'09'; data:'221213' ),
  ( a:'*'; b:'*'; c:'10'; data:'221312' ),
  ( a:'+'; b:'+'; c:'11'; data:'231212' ),
  ( a:','; b:','; c:'12'; data:'112232' ), {23.10.2001 Stefano Torricella}
  ( a:'-'; b:'-'; c:'13'; data:'122132' ),
  ( a:'.'; b:'.'; c:'14'; data:'122231' ),
  ( a:'/'; b:'/'; c:'15'; data:'113222' ),
  ( a:'0'; b:'0'; c:'16'; data:'123122' ),
  ( a:'1'; b:'1'; c:'17'; data:'123221' ),
  ( a:'2'; b:'2'; c:'18'; data:'223211' ),
  ( a:'3'; b:'3'; c:'19'; data:'221132' ),
  ( a:'4'; b:'4'; c:'20'; data:'221231' ),
  ( a:'5'; b:'5'; c:'21'; data:'213212' ),
  ( a:'6'; b:'6'; c:'22'; data:'223112' ),
  ( a:'7'; b:'7'; c:'23'; data:'312131' ),
  ( a:'8'; b:'8'; c:'24'; data:'311222' ),
  ( a:'9'; b:'9'; c:'25'; data:'321122' ),
  ( a:':'; b:':'; c:'26'; data:'321221' ),
  ( a:';'; b:';'; c:'27'; data:'312212' ),
  ( a:'<'; b:'<'; c:'28'; data:'322112' ),
  ( a:'='; b:'='; c:'29'; data:'322211' ),
  ( a:'>'; b:'>'; c:'30'; data:'212123' ),
  ( a:'?'; b:'?'; c:'31'; data:'212321' ),
  ( a:'@'; b:'@'; c:'32'; data:'232121' ),
  ( a:'A'; b:'A'; c:'33'; data:'111323' ),
  ( a:'B'; b:'B'; c:'34'; data:'131123' ),
  ( a:'C'; b:'C'; c:'35'; data:'131321' ),
  ( a:'D'; b:'D'; c:'36'; data:'112313' ),
  ( a:'E'; b:'E'; c:'37'; data:'132113' ),
  ( a:'F'; b:'F'; c:'38'; data:'132311' ),
  ( a:'G'; b:'G'; c:'39'; data:'211313' ),
  ( a:'H'; b:'H'; c:'40'; data:'231113' ),
  ( a:'I'; b:'I'; c:'41'; data:'231311' ),
  ( a:'J'; b:'J'; c:'42'; data:'112133' ),
  ( a:'K'; b:'K'; c:'43'; data:'112331' ),
  ( a:'L'; b:'L'; c:'44'; data:'132131' ),
  ( a:'M'; b:'M'; c:'45'; data:'113123' ),
  ( a:'N'; b:'N'; c:'46'; data:'113321' ),
  ( a:'O'; b:'O'; c:'47'; data:'133121' ),
  ( a:'P'; b:'P'; c:'48'; data:'313121' ),
  ( a:'Q'; b:'Q'; c:'49'; data:'211331' ),
  ( a:'R'; b:'R'; c:'50'; data:'231131' ),
  ( a:'S'; b:'S'; c:'51'; data:'213113' ),
  ( a:'T'; b:'T'; c:'52'; data:'213311' ),
  ( a:'U'; b:'U'; c:'53'; data:'213131' ),
  ( a:'V'; b:'V'; c:'54'; data:'311123' ),
  ( a:'W'; b:'W'; c:'55'; data:'311321' ),
  ( a:'X'; b:'X'; c:'56'; data:'331121' ),
  ( a:'Y'; b:'Y'; c:'57'; data:'312113' ),
  ( a:'Z'; b:'Z'; c:'58'; data:'312311' ),
  ( a:'['; b:'['; c:'59'; data:'332111' ),
  ( a:'\'; b:'\'; c:'60'; data:'314111' ),
  ( a:']'; b:']'; c:'61'; data:'221411' ),
  ( a:'^'; b:'^'; c:'62'; data:'431111' ),
  ( a:'_'; b:'_'; c:'63'; data:'111224' ),
  ( a:#0 ; b:'`'; c:'64'; data:'111422' ),
  ( a:#1 ; b:'a'; c:'65'; data:'121124' ),
  ( a:#2 ; b:'b'; c:'66'; data:'121421' ),
  ( a:#3 ; b:'c'; c:'67'; data:'141122' ),
  ( a:#4 ; b:'d'; c:'68'; data:'141221' ),
  ( a:#5 ; b:'e'; c:'69'; data:'112214' ),
  ( a:#6 ; b:'f'; c:'70'; data:'112412' ),
  ( a:#7 ; b:'g'; c:'71'; data:'122114' ),
  ( a:#8 ; b:'h'; c:'72'; data:'122411' ),
  ( a:#9 ; b:'i'; c:'73'; data:'142112' ),
  ( a:#10; b:'j'; c:'74'; data:'142211' ),
  ( a:#11; b:'k'; c:'75'; data:'241211' ),
  ( a:#12; b:'l'; c:'76'; data:'221114' ),
  ( a:#13; b:'m'; c:'77'; data:'413111' ),
  ( a:#14; b:'n'; c:'78'; data:'241112' ),
  ( a:#15; b:'o'; c:'79'; data:'134111' ),
  ( a:#16; b:'p'; c:'80'; data:'111242' ),
  ( a:#17; b:'q'; c:'81'; data:'121142' ),
  ( a:#18; b:'r'; c:'82'; data:'121241' ),
  ( a:#19; b:'s'; c:'83'; data:'114212' ),
  ( a:#20; b:'t'; c:'84'; data:'124112' ),
  ( a:#21; b:'u'; c:'85'; data:'124211' ),
  ( a:#22; b:'v'; c:'86'; data:'411212' ),
  ( a:#23; b:'w'; c:'87'; data:'421112' ),
  ( a:#24; b:'x'; c:'88'; data:'421211' ),
  ( a:#25; b:'y'; c:'89'; data:'212141' ),
  ( a:#26; b:'z'; c:'90'; data:'214121' ),
  ( a:#27; b:'{'; c:'91'; data:'412121' ),
  ( a:#28; b:'|'; c:'92'; data:'111143' ),
  ( a:#29; b:'}'; c:'93'; data:'111341' ),
  ( a:#30; b:'~'; c:'94'; data:'131141' ),
  ( a:#31; b:' '; c:'95'; data:'114113' ),
  ( a:' '; b:' '; c:'96'; data:'114311' ),
  ( a:' '; b:' '; c:'97'; data:'411113' ),
  ( a:' '; b:' '; c:'98'; data:'411311' ),
  ( a:' '; b:' '; c:'99'; data:'113141' ),
  ( a:' '; b:' '; c:'  '; data:'114131' ),
  ( a:' '; b:' '; c:'  '; data:'311141' ),
  ( a:' '; b:' '; c:'  '; data:'411131' )      { FNC1 }
  );

StartA = '211412';
StartB = '211214';
StartC = '211232';
Stop   = '2331112';




{find Code 128 Codeset A or B}
function Find_Code128AB(c:char):integer;
var
  i:integer;
  v:char;
begin
  for i:=0 to High(tabelle_128) do
  begin
    if FTyp = bcCode128A then
      v := tabelle_128[i].a
    else
      v := tabelle_128[i].b;

    if c = v then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

{ find Code 128 Codeset C }
function Find_Code128C(c:string):integer;
  var  i:integer;
  begin
    for i:=0 to High(tabelle_128) do begin
      if tabelle_128[i].c = c then begin
       result := i;
       exit;
      end;
    end;
    result := -1;
  end;



var i, j, idx: integer;
  startcode:string;
  checksum : integer;
  codeword_pos : integer;

begin
  case FTyp of
    bcCode128A, bcCodeEAN128A:
      begin checksum := 103; startcode:= StartA; end;
    bcCode128B, bcCodeEAN128B:
      begin checksum := 104; startcode:= StartB; end;
    bcCode128C, bcCodeEAN128C:
      begin checksum := 105; startcode:= StartC; end;
    else
      raise Exception.CreateFmt('%s: wrong BarcodeType in Code_128', [self.ClassName]);
  end;

  result := startcode;    {Startcode}
  codeword_pos := 1;

  case FTyp of
    bcCodeEAN128A,
    bcCodeEAN128B,
    bcCodeEAN128C:
      begin
      {
      special identifier
      FNC1 = function code 1
      for EAN 128 barcodes
      }
      result := result + tabelle_128[102].data;
      Inc(checksum, 102*codeword_pos);
      Inc(codeword_pos);
      {
      if there is no checksum at the end of the string
      the EAN128 needs one (modulo 10)
      }
      if FCheckSum then FText:=DoCheckSumming(FTEXT);
      end;
  end;

  if (FTyp = bcCode128C) or (FTyp = bccodeEAN128C) then
  begin
    if (Length(FText) mod 2<>0) then FText:='0'+FText;
    for i:=1 to (Length(FText) div 2) do
    begin
      j:=(i-1)*2+1;
      idx:=Find_Code128C(copy(Ftext,j,2));
      if idx < 0 then idx := Find_Code128C('00');
      result := result + tabelle_128[idx].data;
      Inc(checksum, idx*codeword_pos);
      Inc(codeword_pos);
    end;
  end
  else
    for i:=1 to Length(FText) do
    begin
      idx := Find_Code128AB(FText[i]);
      if idx < 0 then
        idx := Find_Code128AB(' ');
      result := result + tabelle_128[idx].data;
      Inc(checksum, idx*codeword_pos);
      Inc(codeword_pos);
    end;

  checksum := checksum mod 103;
  result := result + tabelle_128[checksum].data;

  result := result + Stop;      {Stopcode}
  Result := Convert(Result);
end;





function TAsBarcode.Code_93:string;
type TCode93 =
  record
    c : char;
    data : array[0..5] of char;
  end;

const tabelle_93: array[0..46] of TCode93 = (
  ( c:'0'; data:'131112'  ),
  ( c:'1'; data:'111213'  ),
  ( c:'2'; data:'111312'  ),
  ( c:'3'; data:'111411'  ),
  ( c:'4'; data:'121113'  ),
  ( c:'5'; data:'121212'  ),
  ( c:'6'; data:'121311'  ),
  ( c:'7'; data:'111114'  ),
  ( c:'8'; data:'131211'  ),
  ( c:'9'; data:'141111'  ),
  ( c:'A'; data:'211113'  ),
  ( c:'B'; data:'211212'  ),
  ( c:'C'; data:'211311'  ),
  ( c:'D'; data:'221112'  ),
  ( c:'E'; data:'221211'  ),
  ( c:'F'; data:'231111'  ),
  ( c:'G'; data:'112113'  ),
  ( c:'H'; data:'112212'  ),
  ( c:'I'; data:'112311'  ),
  ( c:'J'; data:'122112'  ),
  ( c:'K'; data:'132111'  ),
  ( c:'L'; data:'111123'  ),
  ( c:'M'; data:'111222'  ),
  ( c:'N'; data:'111321'  ),
  ( c:'O'; data:'121122'  ),
  ( c:'P'; data:'131121'  ),
  ( c:'Q'; data:'212112'  ),
  ( c:'R'; data:'212211'  ),
  ( c:'S'; data:'211122'  ),
  ( c:'T'; data:'211221'  ),
  ( c:'U'; data:'221121'  ),
  ( c:'V'; data:'222111'  ),
  ( c:'W'; data:'112122'  ),
  ( c:'X'; data:'112221'  ),
  ( c:'Y'; data:'122121'  ),
  ( c:'Z'; data:'123111'  ),
  ( c:'-'; data:'121131'  ),
  ( c:'.'; data:'311112'  ),
  ( c:' '; data:'311211'  ),
  ( c:'$'; data:'321111'  ),
  ( c:'/'; data:'112131'  ),
  ( c:'+'; data:'113121'  ),
  ( c:'%'; data:'211131'  ),
  ( c:'['; data:'121221'  ),   {only used for Extended Code 93}
  ( c:']'; data:'312111'  ),   {only used for Extended Code 93}
  ( c:'{'; data:'311121'  ),   {only used for Extended Code 93}
  ( c:'}'; data:'122211'  )    {only used for Extended Code 93}
  );


{find Code 93}
function Find_Code93(c:char):integer;
var
  i:integer;
begin
  for i:=0 to High(tabelle_93) do
  begin
    if c = tabelle_93[i].c then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;




var
  i, idx : integer;
  checkC, checkK,   {Checksums}
  weightC, weightK : integer;
begin

  result := '111141';   {Startcode}

  for i:=1 to Length(FText) do
  begin
    idx := Find_Code93(FText[i]);
    if idx < 0 then
      raise Exception.CreateFmt('%s:Code93 bad Data <%s>', [self.ClassName,FText]);
    result := result + tabelle_93[idx].data;
  end;

  checkC := 0;
  checkK := 0;

  weightC := 1;
  weightK := 2;

  for i:=Length(FText) downto 1 do
  begin
    idx := Find_Code93(FText[i]);

    Inc(checkC, idx*weightC);
    Inc(checkK, idx*weightK);

    Inc(weightC);
    if weightC > 20 then weightC := 1;
    Inc(weightK);
//  if weightK > 15 then weightC := 1;
    if weightK > 15 then weightK:= 1;
  end;

  Inc(checkK, checkC);

  checkC := checkC mod 47;
  checkK := checkK mod 47;

  result := result + tabelle_93[checkC].data +
    tabelle_93[checkK].data;

  result := result + '1111411';   {Stopcode}
  Result := Convert(Result);
end;





function TAsBarcode.Code_93Extended:string;
const code93x : array[0..127] of string[2] =
  (
  (']U'), ('[A'), ('[B'), ('[C'), ('[D'), ('[E'), ('[F'), ('[G'),
  ('[H'), ('[I'), ('[J'), ('[K'), ('[L'), ('[M'), ('[N'), ('[O'),
  ('[P'), ('[Q'), ('[R'), ('[S'), ('[T'), ('[U'), ('[V'), ('[W'),
  ('[X'), ('[Y'), ('[Z'), (']A'), (']B'), (']C'), (']D'), (']E'),
   (' '), ('{A'), ('{B'), ('{C'), ('{D'), ('{E'), ('{F'), ('{G'),
  ('{H'), ('{I'), ('{J'), ('{K'), ('{L'), ('{M'), ('{N'), ('{O'),
  ( '0'),  ('1'),  ('2'),  ('3'),  ('4'),  ('5'),  ('6'),  ('7'),
   ('8'),  ('9'), ('{Z'), (']F'), (']G'), (']H'), (']I'), (']J'),
  (']V'),  ('A'),  ('B'),  ('C'),  ('D'),  ('E'),  ('F'),  ('G'),
   ('H'),  ('I'),  ('J'),  ('K'),  ('L'),  ('M'),  ('N'),  ('O'),
   ('P'),  ('Q'),  ('R'),  ('S'),  ('T'),  ('U'),  ('V'),  ('W'),
   ('X'),  ('Y'),  ('Z'), (']K'), (']L'), (']M'), (']N'), (']O'),
  (']W'), ('}A'), ('}B'), ('}C'), ('}D'), ('}E'), ('}F'), ('}G'),
  ('}H'), ('}I'), ('}J'), ('}K'), ('}L'), ('}M'), ('}N'), ('}O'),
  ('}P'), ('}Q'), ('}R'), ('}S'), ('}T'), ('}U'), ('}V'), ('}W'),
  ('}X'), ('}Y'), ('}Z'), (']P'), (']Q'), (']R'), (']S'), (']T')
  );

var
  save : string;
  i : integer;
begin
 {CharToOem(PChar(FText), save);}

  save := FText;
  FText := '';


  for i:=1 to Length(save) do
  begin
    if ord(save[i]) <= 127 then
      FText := FText + code93x[ord(save[i])];
  end;

  {Showmessage(Format('Text: <%s>', [FText]));}

  result := Code_93;
  FText := save;
end;



function TAsBarcode.Code_MSI:string;
const tabelle_MSI:array['0'..'9'] of string[8] =
  (
  ( '51515151' ),    {'0'}
  ( '51515160' ),    {'1'}
  ( '51516051' ),    {'2'}
  ( '51516060' ),    {'3'}
  ( '51605151' ),    {'4'}
  ( '51605160' ),    {'5'}
  ( '51606051' ),    {'6'}
  ( '51606060' ),    {'7'}
  ( '60515151' ),    {'8'}
  ( '60515160' )     {'9'}
  );

var
  i:integer;
  check_even, check_odd, checksum:integer;
begin
  result := '60';    {Startcode}
  check_even := 0;
  check_odd  := 0;

  for i:=1 to Length(FText) do
  begin
    if odd(i-1) then
      check_odd := check_odd*10+ord(FText[i])
    else
      check_even := check_even+ord(FText[i]);

    result := result + tabelle_MSI[FText[i]];
  end;

  checksum := quersumme(check_odd*2) + check_even;

  checksum := checksum mod 10;
  if checksum > 0 then
    checksum := 10-checksum;

  result := result + tabelle_MSI[chr(ord('0')+checksum)];

  result := result + '515'; {Stopcode}
end;



function TAsBarcode.Code_PostNet:string;
const tabelle_PostNet:array['0'..'9'] of string[10] =
  (
  ( '5151A1A1A1' ),    {'0'}
  ( 'A1A1A15151' ),    {'1'}
  ( 'A1A151A151' ),    {'2'}
  ( 'A1A15151A1' ),    {'3'}
  ( 'A151A1A151' ),    {'4'}
  ( 'A151A151A1' ),    {'5'}
  ( 'A15151A1A1' ),    {'6'}
  ( '51A1A1A151' ),    {'7'}
  ( '51A1A151A1' ),    {'8'}
  ( '51A151A1A1' )     {'9'}
  );
var
  i:integer;
begin
  result := '51';

  for i:=1 to Length(FText) do
  begin
    result := result + tabelle_PostNet[FText[i]];
  end;
  result := result + '5';
end;


function TAsBarcode.Code_Codabar:string;
type TCodabar =
  record
    c : char;
    data : array[0..6] of char;
  end;

const tabelle_cb: array[0..19] of TCodabar = (
  ( c:'1'; data:'5050615'  ),
  ( c:'2'; data:'5051506'  ),
  ( c:'3'; data:'6150505'  ),
  ( c:'4'; data:'5060515'  ),
  ( c:'5'; data:'6050515'  ),
  ( c:'6'; data:'5150506'  ),
  ( c:'7'; data:'5150605'  ),
  ( c:'8'; data:'5160505'  ),
  ( c:'9'; data:'6051505'  ),
  ( c:'0'; data:'5050516'  ),
  ( c:'-'; data:'5051605'  ),
  ( c:'$'; data:'5061505'  ),
  ( c:':'; data:'6050606'  ),
  ( c:'/'; data:'6060506'  ),
  ( c:'.'; data:'6060605'  ),
  ( c:'+'; data:'5060606'  ),
  ( c:'A'; data:'5061515'  ),
  ( c:'B'; data:'5151506'  ),
  ( c:'C'; data:'5051516'  ),
  ( c:'D'; data:'5051615'  )
  );



{find Codabar}
function Find_Codabar(c:char):integer;
var
  i:integer;
begin
  for i:=0 to High(tabelle_cb) do
  begin
    if c = tabelle_cb[i].c then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

var
  i, idx : integer;
begin
  Result := tabelle_cb[Find_Codabar('A')].data ;
  Result := Result + '0';
  for i:=1 to Length(FText) do
  begin
    idx := Find_Codabar(FText[i]);
    result := result + tabelle_cb[idx].data + '0';
  end;
  result := result + tabelle_cb[Find_Codabar('B')].data;
end;



{---------------}

{Assist function}
function TAsBarcode.SetLen(pI:byte):string;
begin
   Result := StringOfChar('0', pI-Length(FText)) + FText;
{
   old implementation, if your Delphi version does not support
   StringOfChar()

  Result := FText;
  while Length(Result) < pI do
    Result:='0'+Result;
}
end;



function TAsBarcode.Code_UPC_A:string;
var
  i : integer;
  tmp : String;
begin
  FText := SetLen(12);
  if FCheckSum then tmp:=DoCheckSumming(copy(FText,1,11));
  if FCheckSum then FText:=tmp else tmp:=FText;
  result := '505';   {Startcode}
  for i:=1 to 6 do
    result := result + tabelle_EAN_A[tmp[i]];
  result := result + '05050';   {Trennzeichen}
  for i:=7 to 12 do
    result := result + tabelle_EAN_C[tmp[i]];
  result := result + '505';   {Stopcode}
end;


{UPC E Parity Pattern Table , Number System 0}
const tabelle_UPC_E0:array['0'..'9', 1..6] of char =
  (
  ('E', 'E', 'E', 'o', 'o', 'o' ),    { 0 }
  ('E', 'E', 'o', 'E', 'o', 'o' ),    { 1 }
  ('E', 'E', 'o', 'o', 'E', 'o' ),    { 2 }
  ('E', 'E', 'o', 'o', 'o', 'E' ),    { 3 }
  ('E', 'o', 'E', 'E', 'o', 'o' ),    { 4 }
  ('E', 'o', 'o', 'E', 'E', 'o' ),    { 5 }
  ('E', 'o', 'o', 'o', 'E', 'E' ),    { 6 }
  ('E', 'o', 'E', 'o', 'E', 'o' ),    { 7 }
  ('E', 'o', 'E', 'o', 'o', 'E' ),    { 8 }
  ('E', 'o', 'o', 'E', 'o', 'E' )     { 9 }
  );

function TAsBarcode.Code_UPC_E0:string;
var i,j : integer;
   tmp : String;
   c   : char;
begin
  FText := SetLen(7);
  tmp:=DoCheckSumming(copy(FText,1,6));
  c:=tmp[7];
  if FCheckSum then FText:=tmp else tmp := FText;
  result := '505';   {Startcode}
  for i:=1 to 6 do
  begin
    if tabelle_UPC_E0[c,i]='E' then
    begin
      for j:= 1 to 4 do result := result + tabelle_EAN_C[tmp[i],5-j];
    end
    else
    begin
      result := result + tabelle_EAN_A[tmp[i]];
    end;
  end;
  result := result + '050505';   {Stopcode}
end;

function TAsBarcode.Code_UPC_E1:string;
var i,j : integer;
   tmp : String;
   c   : char;
begin
  FText := SetLen(7);
  tmp:=DoCheckSumming(copy(FText,1,6));
  c:=tmp[7];
  if FCheckSum then FText:=tmp else tmp := FText;
  result := '505';   {Startcode}
  for i:=1 to 6 do
  begin
    if tabelle_UPC_E0[c,i]='E' then
    begin
      result := result + tabelle_EAN_A[tmp[i]];
    end
    else
    begin
      for j:= 1 to 4 do result := result + tabelle_EAN_C[tmp[i],5-j];
    end;
  end;
  result := result + '050505';   {Stopcode}
end;

{assist function}
function getSupp(Nr : String) : String;
var i,fak,sum : Integer;
      tmp   : String;
begin
  sum := 0;
  tmp := copy(nr,1,Length(Nr)-1);
  fak := Length(tmp);
  for i:=1 to length(tmp) do
  begin
    if (fak mod 2) = 0 then
      sum := sum + (StrToInt(tmp[i])*9)
    else
      sum := sum + (StrToInt(tmp[i])*3);
    dec(fak);
  end;
  sum:=((sum mod 10) mod 10) mod 10;
  result := tmp+IntToStr(sum);
end;

function TAsBarcode.Code_Supp5:string;
var i,j : integer;
   tmp : String;
   c   : char;
begin
  FText := SetLen(5);
  tmp:=getSupp(copy(FText,1,5)+'0');
  c:=tmp[6];
  if FCheckSum then FText:=tmp else tmp := FText;
  result := '506';   {Startcode}
  for i:=1 to 5 do
  begin
    if tabelle_UPC_E0[c,(6-5)+i]='E' then
    begin
      for j:= 1 to 4 do result := result + tabelle_EAN_C[tmp[i],5-j];
    end
    else
    begin
      result := result + tabelle_EAN_A[tmp[i]];
    end;
    if i<5 then result:=result+'05'; { character delineator }
  end;
end;

function TAsBarcode.Code_Supp2:string;
var i,j : integer;
   tmp,mS : String;
begin
  FText := SetLen(2);
  i:=StrToInt(Ftext);
  case i mod 4 of
    3: mS:='EE';
    2: mS:='Eo';
    1: mS:='oE';
    0: mS:='oo';
  end;
  tmp:=getSupp(copy(FText,1,5)+'0');

  if FCheckSum then FText:=tmp else tmp := FText;
  result := '506';   {Startcode}
  for i:=1 to 2 do
  begin
    if mS[i]='E' then
    begin
      for j:= 1 to 4 do result := result + tabelle_EAN_C[tmp[i],5-j];
    end
    else
    begin
      result := result + tabelle_EAN_A[tmp[i]];
    end;
    if i<2 then result:=result+'05'; { character delineator }
  end;
end;

{---------------}




procedure TAsBarcode.MakeModules;
begin
  case Typ of
    bcCode_2_5_interleaved,
    bcCode_2_5_industrial,
    bcCode39,
    bcCodeEAN8,
    bcCodeEAN13,
    bcCode39Extended,
    bcCodeCodabar,
    bcCodeUPC_A,
    bcCodeUPC_E0,
    bcCodeUPC_E1,
    bcCodeUPC_Supp2,
    bcCodeUPC_Supp5:

    begin
      if Ratio < 2.0 then Ratio := 2.0;
      if Ratio > 3.0 then Ratio := 3.0;
    end;

    bcCode_2_5_matrix:
    begin
      if Ratio < 2.25 then Ratio := 2.25;
      if Ratio > 3.0 then Ratio := 3.0;
    end;
    bcCode128A,
    bcCode128B,
    bcCode128C,
    bcCode93,
    bcCode93Extended,
    bcCodeMSI,
    bcCodePostNet:    ;
  end;


  modules[0] := FModul;
  modules[1] := Round(FModul*FRatio);
  modules[2] := modules[1] * 3 div 2;
  modules[3] := modules[1] * 2;
end;





{
Draw the Barcode

Parameter :
'data' holds the pattern for a Barcode.
A barcode begins always with a black line and
ends with a black line.

The white Lines builds the space between the black Lines.

A black line must always followed by a white Line and vica versa.

Examples:
  '50505'   // 3 thin black Lines with 2 thin white Lines
  '606'     // 2 fat black Lines with 1 thin white Line

  '5605015' // Error


data[] : see procedure OneBarProps

}
procedure TAsBarcode.DoLines(data:string; Canvas:TCanvas);

var i:integer;
  lt : TBarLineType;
  xadd:integer;
  wid, hei:integer;
  a,b,c,d,     {Edges of a line (we need 4 Point because the line}
          {is a recangle}
  orgin : TPoint;
  alpha:double;
begin
  xadd := 0;
  orgin.x := FLeft;
  orgin.y := FTop;

  alpha := FAngle/180.0*pi;

  { Move the orgin so the entire barcode ends up in the visible region. }
  orgin := TranslateQuad2D(alpha,orgin,Point(Self.Width ,Self.Height));

  with Canvas do begin
    Pen.Width := 1;

   for i:=1 to Length(data) do  {examine the pattern string}
    begin

      {
      input:  pattern code
      output: Width and Linetype
      }
      OneBarProps(data[i], wid, lt);

      if (lt = black) or (lt = black_half) then
      begin
        Pen.Color := FColorBar;
      end
      else
      begin
        Pen.Color := FColor;
      end;
      Brush.Color := Pen.Color;

      if lt = black_half then
        hei := FHeight * 2 div 5
      else
        hei := FHeight;





      a.x := xadd;
      a.y := 0;

      b.x := xadd;
      b.y := hei;

    {c.x := xadd+width;}
    c.x := xadd+Wid-1;  {23.04.1999 Line was 1 Pixel too wide}
      c.y := Hei;

    {d.x := xadd+width;}
    d.x := xadd+Wid-1;  {23.04.1999 Line was 1 Pixel too wide}
      d.y := 0;

    {a,b,c,d builds the rectangle we want to draw}


    {rotate the rectangle}
      a := Translate2D(Rotate2D(a, alpha), orgin);
      b := Translate2D(Rotate2D(b, alpha), orgin);
      c := Translate2D(Rotate2D(c, alpha), orgin);
      d := Translate2D(Rotate2D(d, alpha), orgin);

    {draw the rectangle}
      Polygon([a,b,c,d]);

      xadd := xadd + wid;
    end;
  end;
end;



procedure TAsBarcode.DrawBarcode(Canvas:TCanvas);
var
  data : string;
  SaveFont: TFont;
  SavePen: TPen;
  SaveBrush: TBrush;
begin
  Savefont  := TFont.Create;
  SavePen   := TPen.Create;
  SaveBrush := TBrush.Create;


  {get barcode pattern}
  data := MakeData;


  try
   {store Canvas properties}
    Savefont.Assign(Canvas.Font);
    SavePen.Assign(Canvas.Pen);
    SaveBrush.Assign(Canvas.Brush);

    DoLines(data, Canvas);    {draw the barcode}

    if FShowText <> bcoNone then
      DrawText(Canvas);   {show readable Text}


   {restore old Canvas properties}
    Canvas.Font.Assign(savefont);
    Canvas.Pen.Assign(SavePen);
    Canvas.Brush.Assign(SaveBrush);
  finally
    Savefont.Free;
    SavePen.Free;
    SaveBrush.Free;
  end;
end;


{
  draw contents and type/name of barcode
  as human readable text at the left
  upper edge of the barcode.

  main use for this procedure is testing.

  note: this procedure changes Pen and Brush
  of the current canvas.

  Modifications from Roberto Parola to improve the text output
  Its useful to print the Text (code) on the barcode, in case the pen
  doesnt read the barcode.
  I didnt implement the EAN8 and EAN13 way to print the code, because
  the first character is outside of the bound of the barcode, and this
  can cause some problems (expecially in a report)
}
procedure TAsBarcode.DrawText(Canvas:TCanvas);
var
  PosX, PosY: Integer;
  SaveFont: TFont;
begin
  with Canvas do
  begin
//    Font.Size := 5;
    {the fixed font size is a problem, if you
     use very large or small barcodes}
// then i thought well to modify it

// I know... you already did it in the DrawBarcode function, and this one is
// called only by there... but i want to be sure :)
    SaveFont := TFont.Create;
    try
      Font.Assign(ShowTextFont);
      try
        Pen.Color := Font.Color;
//      Brush.Color := clWhite;
        Brush.Color := Color;

// I only consider the Text (code) position.
// As stated by Andreas Schmidt
        PosX := FLeft;
        PosY := FTop;

        if ShowTextPosition in [stpTopLeft, stpBottomLeft] then
          PosX := FLeft
        else
          if ShowTextPosition in [stpTopRight, stpBottomRight] then
            PosX := FLeft + Width - TextWidth(Text)
          else
            // i know, the last IF is useless, but i like it this way
            if ShowTextPosition in [stpTopCenter, stpBottomCenter] then
              PosX := FLeft + Trunc((Width - TextWidth(Text))/2);

        if ShowTextPosition in [stpTopLeft, stpTopCenter, stpTopRight] then
          PosY := FTop
        else
          if ShowTextPosition in [stpBottomLeft, stpBottomCenter, stpBottomRight] then
            PosY := FTop + Height - TextHeight(Text);

        if FShowText in [bcoCode, bcoBoth] then
        begin
          Brush.Style := bsSolid;     // Apagando a area de impressão do Código //
          FillRect(Rect(PosX,PosY,PosX + TextWidth(FText), PosY + TextHeight(Text)) ) ;
          
          TextOut(PosX, PosY, FText);         {contents of Barcode}
        end ;

        if FShowText in [bcoTyp, bcoBoth] then
        begin
          PosX  := FLeft ;
          PosY  := FTop+Round(Font.Height*2.5) ;

          Brush.Style := bsSolid;     // Apagando a area  //
          FillRect(Rect(PosX,PosY,PosX + TextWidth(GetTypText), PosY + TextHeight(GetTypText)) ) ;

          TextOut(PosX, PosY, GetTypText); {type/name of barcode}
        end ;
      finally
        Font.Assign(SaveFont);
      end;
    finally
      SaveFont.Free;
    end;
  end;
end;


procedure TAsBarcode.DoChange;
begin
   if Assigned(FOnChange) then
      FOnChange(Self);
end;

procedure TAsBarcode.SetRatio(const Value: Double);
begin
   if Value <> FRatio then
   begin
      FRatio := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetTyp(const Value: TBarcodeType);
begin
   if Value <> FTyp then
   begin
      FTyp := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetAngle(const Value: Double);
begin
   if Value <> FAngle then
   begin
      FAngle := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetText(const Value: string);
begin
   if Value <> FText then
   begin
      FText := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetShowText(const Value: TBarcodeOption);
begin
   if Value <> FShowText then
   begin
      FShowText := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetTop(const Value: Integer);
begin
   if Value <> FTop then
   begin
      FTop := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetLeft(const Value: Integer);
begin
   if Value <> FLeft then
   begin
      FLeft := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetCheckSum(const Value: Boolean);
begin
   if Value <> FCheckSum then
   begin
      FCheckSum := Value;
      DoChange;
   end;
end;

procedure TAsBarcode.SetHeight(const Value: integer);
begin
   if Value <> FHeight then
   begin
      FHeight := Value;
      DoChange;
   end;
end;

(*
procedure Register;
begin
  {
  there is a function to determine the page name
  independent of your language
  but i forgot it.
  could you get me a hint to avoid this hard coded string 'Extras'
  }
  RegisterComponents('Extras', [TAsBarcode]);
end;
*)

function TAsBarcode.GetCanvasHeight: Integer;
var
  alpha :Extended;
begin
  alpha := FAngle/180.0*pi;
  Result := Round(abs(sin(alpha))*Self.Width + abs(cos(alpha))*Self.Height + 0.5); {.5 rounds up always}
end;

function TAsBarcode.GetCanvasWidth: Integer;
var
  alpha :Extended;
begin
  alpha := FAngle/180.0*pi;
  Result := Round(abs(cos(alpha))*Self.Width + abs(sin(alpha))*Self.Height + 0.5); { .5 rounds up always}
end;


procedure TAsBarcode.SetShowTextFont(const Value: TFont);
begin
  FShowTextFont.Assign(Value);
  DoChange;
end;

procedure TAsBarcode.SetShowTextPosition(const Value: TShowTextPosition);
begin
   if Value <> FShowTextPosition then
   begin
     FShowTextPosition := Value;
     DoChange;
   end;
end;

end.
