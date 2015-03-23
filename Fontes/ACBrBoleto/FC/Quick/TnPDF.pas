{******************************************************************************

Nishita's PDF Creation VCL (TNPDF)
Version 2.0

Filename:             tnpdf.pas
Author:               K. Nishita
Compiler:           Delphi 1.0, 2.0, 3.0, 4.0, 5.0

Description:       Create PDF Files

NOTE:
If you want to use FlateDecode/Zlib compression, copy paszlib files (paszlib.zip)
included with this distribution to your Delphi librally path.

Copyright (c) 2002, K. Nishita.  All Rights Reserved.
Last Revised Date:    3/3/2002

******************************************************************************

MOD by Toci <toth.akos@interware.hu>
for TQRMultiExport

Last modified: 25/1/2008

 @ line 706 - a part commented casue of a drawing problem:
              The output image was larger than the PDF page
              solve it and contact me!

 @ lines 98, 174, 245, 900 - Property for JPEG Quality added

******************************************************************************}

unit tnpdf;

interface

{$IFDEF VER130}
  {$DEFINE DFS_DELPHI_3_UP}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE DFS_DELPHI_3_UP}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE DFS_DELPHI_3_UP}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE DFS_DELPHI_3_UP}
{$ENDIF}

{$DEFINE DFS_DELPHI_3_UP}
{.$DEFINE USE_ZLIB}  {Remove this if you do not want to use ZLIB Compression }


uses
  SysUtils, WinProcs, WinTypes, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Dialogs{$IFDEF USE_ZLIB} ,dZLib{$ENDIF},JPEG;
{
NOTE:
If you want to use FlateDecode/Zlib compression, copy paszlib files (paszlib.zip) 
included with this distribution to your Delphi librally path.
}

Type TPDFOrientation = (poPortrait, poLandscape);
Type TPDFBrushStyle = (poSolid,  poDashed, poBeveled, poInset, poUnderline);
Type TPDFFontName = (poHelvetica,poHelveticaBold,poHelveticaOblique,
poHelveticaBoldOblique,poCourier,poCourierBold,poCourierOblique,
poCourierBoldOblique,poTimesRoman,poTimesBold,poTimesItalic,
poTimesBoldItalic,poSymbol,poZapfDingbats);

type TPDFFont = class
public
Name:TPDFFontName;
Size:Integer;
end;

type
  TPrintPDF = class(TComponent)
  private
    { Private declarations }
  PDF:TMemoryStream;
  FCanvasWidth:Integer;
  FCanvasHeight:Integer;
  FFileName:string;
  FTITLE:String;
  FPageNumber:Integer;
  FFont:TFont;
  FPDFFont:TPDFFont;
  FLineWidth:Integer;
  FOrientation:TPDFOrientation;
  FAuthor:String;
  FCreator:String;
  FKeywords:String;
  FSubject:String;
  FProducer:String;
  FFileCompress:boolean;
  FJPEGQuality : Integer;

  ParentNum,ContentNum,ResourceNum,FontNum,OutLinesNum,CatalogNum,
  FontNumber,CurrentSetPageObject,NumberofImages:Integer;
  CurrentObjectNum:Integer;
  ObjectOffset:LongInt;
  ObjectOffsetList: TStringList;
  PageNumberList: TStringList;
  FontNumberList: TStringList;
  CRCounter:LongInt;

  BitsPerComponent: Byte;
  ColorSpace: String;
  ColorCount: Byte;
  ImageStream:TMemoryStream;
  TempStream:TMemoryStream;
  pTempStream:TMemoryStream;
  sTempStream:TMemoryStream;
  cTempStream:TMemoryStream;

  StreamSize1,StreamSize2:LongInt;
  {$IFDEF USE_ZLIB}
  CompressionStream : TCompressionStream;
  {$ENDIF}
  procedure AddToOffset(offset:LongInt);
  procedure StreamWriteStr(var ms: TMemoryStream; s: string);
  procedure SetPDFHeader;
  procedure SetCatalog;
  procedure SetOutLine;
  procedure SetDocInfo;
  procedure SetPages;
  procedure SetPageObject;
  procedure StartStream;
  procedure EndStream;
  procedure SetArray;
  procedure SetFontType;
  procedure CreateFont(Subtype,BaseFont,Encoding:string);
  procedure SetXref;
  procedure SetBitmap(ABitmap:TBitmap);
  procedure SetJPEG(ABitmap:TBitmap);
  procedure WriteBitmap(a:Integer);
  function GetOffsetNumber(offset:string):string;

  protected
    { Protected declarations }
  public
    { Public declarations }
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure BeginDoc;
  procedure EndDoc;
  procedure NewPage;
  procedure DrawLine(x1,y1,x2,y2:Integer);
  procedure DrawRectangle(x1,y1,x2,y2:Integer);
  procedure TextOut(X, Y: Integer; const Text: string);
  procedure MemoOut(X, Y: Integer; Memo: TMemo);
  procedure ImageOut(X, Y: Integer; ABitmap:TImage);
  procedure DrawBitmap(X, Y: Integer; ABitmap:TBitmap);
  procedure DrawJPEG(X, Y: Integer; ABitmap:TBitmap);
  procedure Draw(X, Y: Integer; ABitmap:TImage);

  published
    { Published declarations }
  property FileName: string read FFileName write FFileName;
  property TITLE: string read FTITLE write FTITLE;
  property PageNumber: Integer read FPageNumber;
  property PageWidth: Integer read FCanvasWidth write FCanvasWidth;
  property PageHeight: Integer read FCanvasHeight write FCanvasHeight;
  property LineWidth: Integer read FLineWidth write FLineWidth;
  property Author: string read FAuthor write FAuthor;
  property Creator: string read FCreator write FCreator;
  property Keywords: string read FKeywords write FKeywords;
  property Subject: string read FSubject write FSubject;
  property Producer: string read FProducer write FProducer;
  property Font: TPDFFont read FPDFFont write FPDFFont;
  property Compress:boolean read FFileCompress write FFileCompress;
  property JPEGQuality:Integer read FJPEGQuality write FJPEGQuality;
  end;

// procedure Register;

implementation

{$IFDEF VER80}
 function LTrim(Const Str: String): String;
   var len: Byte absolute Str;
       i: Integer;
   begin
     i := 1;
     while (i <= len) and (Str[i] = ' ') do Inc(i);
     LTrim := Copy(Str,i,len)
   end ;

   function RTrim(Str: String): String;
   var len: Byte absolute Str;
   begin
     while (Str[len] = ' ') do Dec(len);
     RTrim := Str
   end ;
   function Trim(Str: String): String;
   begin
     Trim := LTrim(RTrim(Str))
   end ;
{$ENDIF}

{procedure Register;
begin
  RegisterComponents('Nishita', [TPrintPDF]);
end;}

function TPrintPDF.GetOffsetNumber(offset:string):string;
var x,y:LongInt;
begin
x:=Length(offset);
result:='';
for y:= 1 to 10-x do
result:=result+'0';
result:=result+offset;
end;

procedure TPrintPDF.StreamWriteStr(var ms: TMemoryStream; s: string);
begin
CRCounter:=CRCounter+2;
s:=s+#13#10;
ms.Write(s[1], Length(s));
end;

constructor TPrintPDF.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
ObjectOffsetList:=TStringList.Create;
PageNumberList:=TStringList.Create;
FontNumberList:=TStringList.Create;
PDF:=TMemoryStream.Create;
TempStream:=TMemoryStream.Create;
ImageStream:=TMemoryStream.Create;
pTempStream:=TMemoryStream.Create;
sTempStream:=TMemoryStream.Create;
cTempStream:=TMemoryStream.Create;

Font:=TPDFFont.Create;
Font.Name:=poCourier;
Font.Size:=12;
FLineWidth:=1;
PageWidth:= 612;
PageHeight:= 792;
Compress:=True;
FJPEGQuality:=50;

{$IFDEF USE_ZLIB}
Compress:=true;
{$ENDIF}

Producer:='';
Author:='';
Creator:='';
Keywords:='';
Subject:='';

end;

destructor TPrintPDF.Destroy;
var i:Integer;
begin
ObjectOffsetList.Free;
PageNumberList.Free;
FontNumberList.Free;
PDF.Free;
TempStream.Free;
ImageStream.Free;
Font.Free;
pTempStream.Free;
sTempStream.Free;
cTempStream.Free;
inherited Destroy;
end;

procedure TPrintPDF.AddToOffset(offset:LongInt);
begin
ObjectOffset:=ObjectOffset+offset;
ObjectOffsetList.Add(IntToStr(ObjectOffset));
CRCounter:=0;
end;

procedure TPrintPDF.BeginDoc;
var i:Integer;
begin
FPageNumber:=1;
NumberofImages:=0;
CurrentObjectNum:=0;
ObjectOffset:=0;
CurrentSetPageObject:=0;
CRCounter:=0;
FontNumber:=0;

ObjectOffsetList.Clear;
PageNumberList.Clear;
FontNumberList.Clear;
PDF.Clear;
TempStream.Clear;
ImageStream.Clear;

SetPDFHeader;
SetDocInfo;
StartStream;
end;

procedure TPrintPDF.EndDoc;
var i:Integer;
begin
EndStream;
SetOutLine;
SetFontType;
SetPages;
SetArray;
for i:= 1 to NumberofImages do
WriteBitmap(i);
for i:= 1 to PageNumber do
begin
SetPageObject;
end;
SetCatalog;
SetXref;
StreamWriteStr(PDF,'%%EOF');
PDF.SaveToFile(FileName);

if (NumberofImages > 0) then
begin
for i:=1 to NumberofImages do
begin
{$IFDEF WIN32}
DeleteFile(pchar('~tmpim'+IntToStr(i)));
{$ELSE}
DeleteFile(('~tmpim'+IntToStr(i)));
{$ENDIF}
end;
end;

end;

procedure TPrintPDF.SetPDFHeader;
begin
CurrentObjectNum:=0;
StreamWriteStr(PDF,'%PDF-1.4');
AddToOffset(PDF.Size);
end;

procedure TPrintPDF.SetDocInfo;
begin
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<<');
StreamWriteStr(TempStream,'/Producer ('+Producer+')');
StreamWriteStr(TempStream,'/Author ('+Author+')');
StreamWriteStr(TempStream,'/CreationDate (D:'+FormatDateTime('YYYYMMDDHHmmSS',now)+')');
StreamWriteStr(TempStream,'/Creator ('+Creator+')');
StreamWriteStr(TempStream,'/Keywords ('+Keywords+')');
StreamWriteStr(TempStream,'/Subject ('+Subject+')');
StreamWriteStr(TempStream,'/Title ('+Title+')');
StreamWriteStr(TempStream,'/ModDate ()');
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;

procedure TPrintPDF.SetArray;
var i:Integer;
begin
CurrentObjectNum:=CurrentObjectNum+1;
ResourceNum:=CurrentObjectNum;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /ProcSet [ /PDF /Text /ImageC]');
StreamWriteStr(TempStream,'/XObject << ');
for i:=1 to NumberofImages do
StreamWriteStr(TempStream,'/Im'+IntToStr(i)+' '+IntToStr(CurrentObjectNum+i)+' 0 R');
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'/Font << ');

for i:=1 to FontNumber do
StreamWriteStr(TempStream,'/F'+IntToStr(i)+' '+FontNumberList.Strings[i-1]+' 0 R ');

StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;

procedure TPrintPDF.SetFontType;
begin
CreateFont('Type1','Helvetica','WinAnsiEncoding');
CreateFont('Type1','Helvetica-Bold','WinAnsiEncoding');
CreateFont('Type1','Helvetica-Oblique','WinAnsiEncoding');
CreateFont('Type1','Helvetica-BoldOblique','WinAnsiEncoding');
CreateFont('Type1','Courier','WinAnsiEncoding');
CreateFont('Type1','Courier-Bold','WinAnsiEncoding');
CreateFont('Type1','Courier-Oblique','WinAnsiEncoding');
CreateFont('Type1','Courier-BoldOblique','WinAnsiEncoding');
CreateFont('Type1','Times-Roman','WinAnsiEncoding');
CreateFont('Type1','Times-Bold','WinAnsiEncoding');
CreateFont('Type1','Times-Italic','WinAnsiEncoding');
CreateFont('Type1','Times-BoldItalic','WinAnsiEncoding');
CreateFont('Type1','Symbol','WinAnsiEncoding');
CreateFont('Type1','ZapfDingbats','WinAnsiEncoding');
end;

{************************}
procedure TPrintPDF.SetOutLine;
begin
CurrentObjectNum:=CurrentObjectNum+1;
OutLinesNum:=CurrentObjectNum;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /Type /Outlines');
StreamWriteStr(TempStream,'/Count 0');
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;

procedure TPrintPDF.SetPages;
var i, PageObjNum : Integer;
begin

CurrentObjectNum:=CurrentObjectNum+1;
ParentNum:=CurrentObjectNum;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /Type /Pages');
StreamWriteStr(TempStream,'/Kids [');

PageObjNum:=2;
for i:= 1 to PageNumber do
begin
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum+i+1+NumberofImages)+' 0 R');
PageNumberList.Add(IntToStr(PageObjNum));
PageObjNum:=PageObjNum+2;
end;
StreamWriteStr(TempStream,']');
StreamWriteStr(TempStream,'/Count '+IntToStr(PageNumber));
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;

procedure TPrintPDF.SetPageObject;
var i:Integer;
begin
ContentNum:=ContentNum+1;
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /Type /Page');
StreamWriteStr(TempStream,'/Parent '+IntToStr(ParentNum)+' 0 R');
StreamWriteStr(TempStream,'/MediaBox [ 0 0 '+IntToStr(PageWidth)+' '+IntToStr(PageHEight)+']');
StreamWriteStr(TempStream,'/Contents '+PageNumberList.Strings[CurrentSetPageObject]+' 0 R');
StreamWriteStr(TempStream,'/Resources '+IntToStr(ResourceNum)+' 0 R');
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
CurrentSetPageObject:=CurrentSetPageObject+1;
end;

procedure TPrintPDF.SetCatalog;
begin
CurrentObjectNum:=CurrentObjectNum+1;
CatalogNum:=CurrentObjectNum;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /Type /Catalog');
StreamWriteStr(TempStream,'/Pages '+IntToStr(ParentNum)+' 0 R');
StreamWriteStr(TempStream,'/Outlines '+IntToStr(OutlinesNum)+' 0 R');
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;     

procedure TPrintPDF.NewPage;
var TempSize:LongInt;
begin
FPageNumber:=FPageNumber+1;

{$IFDEF USE_ZLIB}
if Compress then
begin
CompressionStream := TCompressionStream.Create(clDefault,TempStream);
CompressionStream.CopyFrom(sTempStream, 0);
CompressionStream.Free;
end
else
{$ENDIF}
sTempStream.SaveToStream(TempStream);

sTempStream.Clear;

StreamWriteStr(TempStream,'endstream');
StreamWriteStr(TempStream,'endobj');
StreamSize2:=6;
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);

TempSize:=TempStream.Size-StreamSize1-StreamSize2-Length('Stream')-Length('endstream')-6;
ContentNum:=CurrentObjectNum;
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,IntToStr(TempSize));
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);


ContentNum:=CurrentObjectNum;
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /Length '+IntToStr(CurrentObjectNum+1)+' 0 R');

{$IFDEF USE_ZLIB}
if Compress then StreamWriteStr(TempStream,'/Filter [/FlateDecode]');
{$ENDIF}

StreamWriteStr(TempStream,' >>');

StreamSize1:=TempStream.Size;
StreamWriteStr(TempStream,'stream');
end;

procedure TPrintPDF.StartStream;
begin
ContentNum:=CurrentObjectNum;
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /Length '+IntToStr(CurrentObjectNum+1)+' 0 R');

{$IFDEF USE_ZLIB}
if Compress then StreamWriteStr(TempStream,'/Filter [/FlateDecode]');
{$ENDIF}

StreamWriteStr(TempStream,' >>');
StreamSize1:=TempStream.Size;
StreamWriteStr(TempStream,'stream');
sTempStream.Clear;
end;

procedure TPrintPDF.EndStream;
var TempSize: LongInt;
begin

{$IFDEF USE_ZLIB}
if Compress then
begin
CompressionStream := TCompressionStream.Create(clDefault,TempStream);
CompressionStream.CopyFrom(sTempStream, 0);
CompressionStream.Free;
end
else
{$ENDIF}
sTempStream.SaveToStream(TempStream);

sTempStream.Clear;

StreamWriteStr(TempStream,'endstream');
StreamWriteStr(TempStream,'endobj');
StreamSize2:=6;
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);

TempSize:=TempStream.Size-StreamSize1-StreamSize2-Length('Stream')-Length('endstream')-6;
ContentNum:=CurrentObjectNum;
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,IntToStr(TempSize));
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;

procedure TPrintPDF.SetXref;
var i:Integer;
begin
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,'xref');
StreamWriteStr(TempStream,'0 '+IntToStr(CurrentObjectNum));
StreamWriteStr(TempStream,'0000000000 65535 f');

for i:=0 to CurrentObjectNum-2 do
StreamWriteStr(TempStream,GetOffsetNumber(trim(ObjectOffsetList.Strings[i]))+' 00000 n');

StreamWriteStr(TempStream,'trailer');
StreamWriteStr(TempStream,'<< /Size '+IntToStr(CurrentObjectNum));
StreamWriteStr(TempStream,'/Root '+IntToStr(CatalogNum)+' 0 R');
StreamWriteStr(TempStream,'/Info 1 0 R');
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'startxref');
StreamWriteStr(TempStream,trim(ObjectOffsetList.Strings[CurrentObjectNum-1]));
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;

procedure TPrintPDF.DrawLine(x1,y1,x2,y2:Integer);
begin
StreamWriteStr(sTempStream,IntToStr(x1)+' '+IntToStr((PageHeight-y1))+' m');
StreamWriteStr(sTempStream,IntToStr(x2)+' '+IntToStr((PageHeight-y2))+' l');
StreamWriteStr(sTempStream,IntToStr(LineWidth)+' w');
StreamWriteStr(sTempStream,'S');  {S-Solid,  D-Dashed, B-Beveled, I-Inset, U-Underline}
end;

procedure TPrintPDF.DrawRectangle(x1,y1,x2,y2:Integer);
begin
DrawLine(x1,y1,x1,y2);
DrawLine(x1,y2,x2,y2);
DrawLine(x2,y2,x2,y1);
DrawLine(x2,y1,x1,y1);
end;

procedure TPrintPDF.TextOut(X, Y: Integer; const Text: string);
begin
StreamWriteStr(sTempStream,'BT');
StreamWriteStr(sTempStream,'/F'+IntToStr((Integer(Font.Name)+1))+' '+IntToStr(Font.Size)+' Tf');
StreamWriteStr(sTempStream,IntToStr(X)+' '+IntToStr((PageHeight-Y))+' Td');
StreamWriteStr(sTempStream,'('+Text+') Tj');
StreamWriteStr(sTempStream,'ET');
end;

procedure TPrintPDF.MemoOut(X, Y: Integer; Memo: TMemo);
var i:Integer;
begin
StreamWriteStr(sTempStream,'BT');
StreamWriteStr(sTempStream,'/F'+IntToStr((Integer(Font.Name)+1))+' '+IntToStr(Font.Size)+' Tf');
StreamWriteStr(sTempStream,IntToStr(X)+' '+IntToStr((PageHeight-Y))+' Td');

for i:=0 to Memo.Lines.Count do
begin
StreamWriteStr(sTempStream,'('+Memo.Lines[i]+') Tj');
StreamWriteStr(sTempStream,'0 -12.5 TD');
end;

StreamWriteStr(sTempStream,'ET');
end;

procedure TPrintPDF.Draw(X, Y: Integer; ABitmap:TImage);
begin
ImageOut(X, Y, ABitmap);
end;


procedure TPrintPDF.ImageOut(X, Y: Integer; ABitmap:TImage);
var tempsx,tempsy:double;
begin

tempsx:=((PageWidth)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSX)*10));
tempsy:=((PageHeight)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSY)*11.900));

NumberofImages:=NumberofImages+1;
StreamWriteStr(sTempStream,'q');
StreamWriteStr(sTempStream,IntToStr(trunc(ABitmap.Picture.Bitmap.Width*tempsx))+
' 0 0 '+IntToStr(trunc(ABitmap.Picture.Bitmap.Height*tempsy))+
 ' '+IntToStr(X)+' '+IntToStr(PageHeight-Y-trunc(ABitmap.Picture.Bitmap.Height*tempsy))
 +' cm');
StreamWriteStr(sTempStream,'/Im'+IntToStr(NumberofImages)+' Do');
StreamWriteStr(sTempStream,'Q');
SetBitmap(ABitmap.Picture.Bitmap);
end;


procedure TPrintPDF.DrawBitmap(X, Y: Integer; ABitmap:TBitmap);
var tempsx,tempsy:double;
begin

if (PageHeight > PageWidth) then begin
  tempsx:=((PageWidth)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSX)*10));
  tempsy:=((PageHeight)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSY)*11.900));
end
else begin
  tempsx:=((PageWidth)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSX)* 13));
  tempsy:=((PageHeight)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSY)*8));
end;

NumberofImages:=NumberofImages+1;
StreamWriteStr(sTempStream,'q');
StreamWriteStr(sTempStream,IntToStr(trunc(ABitmap.Width*tempsx))+
' 0 0 '+IntToStr(trunc(ABitmap.Height*tempsy))+
 ' '+IntToStr(X)+' '+IntToStr(PageHeight-Y-trunc(ABitmap.Height*tempsy))
 +' cm');
StreamWriteStr(sTempStream,'/Im'+IntToStr(NumberofImages)+' Do');
StreamWriteStr(sTempStream,'Q');
SetBitmap(ABitmap);
end;

procedure TPrintPDF.DrawJPEG(X, Y: Integer; ABitmap:TBitmap);
var tempsx,tempsy:double;
begin
{
if (PageHeight > PageWidth) then begin
  tempsx:=((PageWidth)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSX)*10));
  tempsy:=((PageHeight)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSY)*11.900));
end
else begin
  tempsx:=((PageWidth)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSX)* 13));
  tempsy:=((PageHeight)/(WinProcs.GetDeviceCaps(GetDC(0), LOGPIXELSY)*8));
end;
}
// commented becaue of a drawing problem
// de image was larger than the paper in the Acrobat Reader...
// when somebody knows why just tell me

tempsx:=1;
tempsy:=1;

NumberofImages:=NumberofImages+1;
StreamWriteStr(sTempStream,'q');
StreamWriteStr(sTempStream,IntToStr(trunc(ABitmap.Width*tempsx))+
' 0 0 '+IntToStr(trunc(ABitmap.Height*tempsy))+
 ' '+IntToStr(X)+' '+IntToStr(PageHeight-Y-trunc(ABitmap.Height*tempsy))
 +' cm');
StreamWriteStr(sTempStream,'/Im'+IntToStr(NumberofImages)+' Do');
StreamWriteStr(sTempStream,'Q');
SetJPEG(ABitmap);
end;


procedure TPrintPDF.SetBitmap(ABitmap:TBitmap);
TYPE pRGBArray = ^TRGBArray;
     TRGBArray = ARRAY[0..0] OF TRGBTriple;
var
tmpBitmap:TBitmap;
InfoSize: {$IFNDEF WIN32}Integer{$ELSE}DWORD{$ENDIF};
ImageSize: {$IFNDEF WIN32}LongInt{$ELSE}DWORD{$ENDIF};
x, y: integer;
pb: PByteArray;
b: TRGBTriple;

bits :Pointer;
Info: PBitmapInfo;
BitmapInfo   :  TBitmapInfo;

DeviceContext:  hDC;
i            :  INTEGER;
j            :  INTEGER;
ScanLine     :  pRGBArray;
iSize: LongInt;

aa : integer;
PalEntries: array[0..255] of TPaletteEntry;
crgb : cardinal;
pbr   : pByteArray;
s : string;

begin
tmpBitmap:=nil;
tmpBitmap:=ABitmap;

GetDIBSizes(tmpBitmap.Handle, InfoSize, ImageSize);

ImageStream.Clear;

{$IFDEF DFS_DELPHI_3_UP}
//      tmpBitmap.PixelFormat := pf24Bit;
      tmpBitmap.PixelFormat := pf8Bit;
      for y := 0 to tmpBitmap.Height-1 do
      begin
        pb := tmpBitmap.ScanLine[y];
        ImageStream.Write(pb^, tmpBitmap.Width);
      end;
{$ELSE}

     GetMem(ScanLine, 3*tmpBitmap.Width);
      TRY
        WITH BitmapInfo DO
        BEGIN
          bmiHeader.biSize        := InfoSize;
          bmiHeader.biWidth       := tmpBitmap.Width;
          bmiHeader.biHeight      := tmpBitmap.Height;
          bmiHeader.biPlanes      := 1;
          bmiHeader.biBitCount    := 24;
          bmiHeader.biCompression := BI_RGB;
        END;
        DeviceContext := GetDC(0);
        TRY
          FOR j := tmpBitmap.Height-1 downTO 0 DO
          BEGIN
            GetDIBits (DeviceContext, tmpBitmap.Handle, j, 1, ScanLine, BitmapInfo, DIB_RGB_COLORS);
            ImageStream.Write(ScanLine^, 3*tmpBitmap.Width);
          END;

        FINALLY
          ReleaseDC(0, DeviceContext);
        END;

      FINALLY
        FreeMem(ScanLine, 3*tmpBitmap.Width);
      END;

{$ENDIF}

GetDIBSizes(tmpBitmap.Handle, InfoSize, ImageSize);

for aa:= 0 to 255 do begin
  PalEntries[aa].peRed := 0;
  PalEntries[aa].peGreen := 0;
  PalEntries[aa].peBlue := 0;
  PalEntries[aa].peFlags := 0;
end;
GetPaletteEntries(tmpBitmap.Palette, 0, 256 + 1, PalEntries);
  for aa := 0 to 255 do begin
    with PalEntries[aa] do
      S := S + IntToHex(peRed, 2) +
           IntToHex(peGreen, 2) +
           IntToHex(peBlue, 2) +
           ' ';
  end;

pTempStream.Clear;
StreamWriteStr(pTempStream,'<< /Type /XObject');
StreamWriteStr(pTempStream,'/Subtype /Image');
StreamWriteStr(pTempStream,'/Width '+IntToStr(tmpBitmap.Width));
StreamWriteStr(pTempStream,'/Height '+IntToStr(tmpBitmap.Height));
StreamWriteStr(pTempStream,'/ColorSpace [/Indexed /DeviceRGB 255 <'+S+'>]');
StreamWriteStr(pTempStream,'/BitsPerComponent 8');
StreamWriteStr(pTempStream,'/Length '+IntToStr(ImageSize));
StreamWriteStr(pTempStream,'/Name /Im'+IntToStr(NumberofImages));
{$IFDEF USE_ZLIB}
if Compress then StreamWriteStr(pTempStream,'/Filter [/FlateDecode]');
{$ENDIF}
StreamWriteStr(pTempStream,'>>');
StreamWriteStr(pTempStream,'stream');
pTempStream.Seek(0, soFromEnd);

{$IFDEF USE_ZLIB}
if Compress then
begin
CompressionStream := TCompressionStream.Create(clDefault,pTempStream);
CompressionStream.CopyFrom(ImageStream, 0);
CompressionStream.Free;
end
else
{$ENDIF}
ImageStream.SaveToStream(pTempStream);
pTempStream.SaveToFile('~tmpim'+IntToStr(NumberofImages));
end;

// JPEG
procedure TPrintPDF.SetJPEG(ABitmap:TBitmap);
TYPE pRGBArray = ^TRGBArray;
     TRGBArray = ARRAY[0..0] OF TRGBTriple;
var
tmpBitmap:TBitmap;
JPE : TJPEGImage;
InfoSize: {$IFNDEF WIN32}Integer{$ELSE}DWORD{$ENDIF};
ImageSize: {$IFNDEF WIN32}LongInt{$ELSE}DWORD{$ENDIF};
x, y: integer;
pb: PByteArray;
b: TRGBTriple;

bits :Pointer;
Info: PBitmapInfo;
BitmapInfo   :  TBitmapInfo;

DeviceContext:  hDC;
i            :  INTEGER;
j            :  INTEGER;
ScanLine     :  pRGBArray;
iSize: LongInt;

aa : integer;
PalEntries: array[0..255] of TPaletteEntry;
crgb : cardinal;
pbr   : pByteArray;
s : string;

begin
tmpBitmap:=nil;
tmpBitmap:=ABitmap;
JPE := TJPEGImage.Create;
JPE.Assign(tmpBitmap);

GetDIBSizes(tmpBitmap.Handle, InfoSize, ImageSize);

ImageStream.Clear;

if Compress then
  JPE.CompressionQuality:=FJPEGQuality;
JPE.SaveToStream(ImageStream);

pTempStream.Clear;
StreamWriteStr(pTempStream,'<< /Type /XObject');
StreamWriteStr(pTempStream,'/Subtype /Image');
StreamWriteStr(pTempStream,'/Width '+IntToStr(tmpBitmap.Width));
StreamWriteStr(pTempStream,'/Height '+IntToStr(tmpBitmap.Height));
StreamWriteStr(pTempStream,'/ColorSpace /DeviceRGB');
StreamWriteStr(pTempStream,'/BitsPerComponent 8');
StreamWriteStr(pTempStream,'/Length '+IntToStr(ImageStream.Size));
StreamWriteStr(pTempStream,'/Name /Im'+IntToStr(NumberofImages));
StreamWriteStr(pTempStream,'/Filter [/DCTDecode]');
StreamWriteStr(pTempStream,'>>');
StreamWriteStr(pTempStream,'stream');
pTempStream.Seek(0, soFromEnd);

ImageStream.SaveToStream(pTempStream);
JPE.Free;
pTempStream.SaveToFile('~tmpim'+IntToStr(NumberofImages));
end;


procedure TPrintPDF.WriteBitmap(a:Integer);
begin
CurrentObjectNum:=CurrentObjectNum+1;
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
ImageStream.Clear;
ImageStream.LoadFromFile('~tmpim'+IntToStr(a));
TempStream.Seek(0, soFromEnd);
ImageStream.SaveToStream(TempStream);
StreamWriteStr(TempStream,#13#10+'endstream');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;


procedure TPrintPDF.CreateFont(Subtype,BaseFont,Encoding:string);
begin
FontNumber:=FontNumber+1;
CurrentObjectNum:=CurrentObjectNum+1;
FontNumberList.Add(IntToStr(CurrentObjectNum));
TempStream.Clear;
StreamWriteStr(TempStream,IntToStr(CurrentObjectNum)+' 0 obj');
StreamWriteStr(TempStream,'<< /Type /Font');
StreamWriteStr(TempStream,'/Subtype /'+Subtype);
StreamWriteStr(TempStream,'/Name /F'+IntToStr(FontNumber));
StreamWriteStr(TempStream,'/BaseFont /'+BaseFont);
StreamWriteStr(TempStream,'/Encoding /'+Encoding);
StreamWriteStr(TempStream,'>>');
StreamWriteStr(TempStream,'endobj');
AddToOffset(TempStream.Size);
PDF.Seek(0, soFromEnd);
TempStream.SaveToStream(PDF);
end;


end.
