{

 FPDF Pascal Report
 https://github.com/arimateia/FPDF-Pascal
 (forked from https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal)

 Copyright (c) 2023 Arimateia Jr - https://nuvemfiscal.com.br
 - Colaboradores : Victor H Gonzales - Pandaaa - Compatibilização D7 / Lazarus

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

}

unit ACBr_fpdf_report;

{$ifdef fpc}
  {$mode delphi}
  {$H+}
{$endif}

// If you have DelphiZXingQRCode Unit on you LibPath
// https://github.com/foxitsoftware/DelphiZXingQRCode
{$DEFINE DelphiZXingQRCode}

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  Types,
  Math,

  ACBr_fpdf,
  ACBr_fpdf_ext
  {$IfDEF DelphiZXingQRCode}
   ,ACBrDelphiZXingQRCode
  {$ENDIF}
  ;

type
  IFPDF = interface;

  TFPDFExt2 = class;

  TFPDFPage = class;

  TFPDFBand = class;

  TFPDFBandType = (btLeftMargin, btTopMargin, btRightMargin, btBottomMargin,
    btData, btReportHeader, btPageHeader, btReportFooter, btPageFooter,
    btOverlay);

  TFPDFBandArray = array of TFPDFBand;

  TFPDFBandTypeArray = array of TFPDFBandType;

  TFPDFPageList = class;

  TFPDFBandList = class;

  TFPDFReportOptions = class;

  TFPDFEngineOptions = class;

  TFPDFEngineEventArgs = class;

  TFPDFReportEventArgs = class;

  TFPDFPageEventArgs = class;

  TFPDFBandEventArgs = class;

  TFPDFBandInitArgs = class;

  TFPDFBandDrawArgs = class;

  TFPDFEngineEvent = procedure(Args: TFPDFEngineEventArgs) of object;

  TFPDFBandEvent = procedure(Args: TFPDFBandEventArgs) of object;

  TFPDFBandInitEvent = procedure(Args: TFPDFBandInitArgs) of object;

  TFPDFBandDrawEvent = procedure(Args: TFPDFBandDrawArgs) of object;

  TFPDFMargins = record
    Left: double;
    Top: double;
    Right: double;
    Bottom: double;
  end;

  TFPDFReport = class
  private
    FPages: TFPDFPageList;
    FOptions: TFPDFReportOptions;
    FEngineOptions: TFPDFEngineOptions;
    FDefaultPageMargins: TFPDFMargins;
    FDefaultFontFamily: string;
    procedure CheckHasPage;

    function HasEndlessPage: boolean;
    property Pages: TFPDFPageList read FPages;
    property DefaultFontFamily: string read FDefaultFontFamily;

    procedure DoStartReport(Args: TFPDFReportEventArgs);
    procedure DoEndReport(Args: TFPDFReportEventArgs);
  protected
    procedure OnStartReport(Args: TFPDFReportEventArgs); virtual;
    procedure OnEndReport(Args: TFPDFReportEventArgs); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetMargins(ALeft, ATop: double; ARight: double = -1; ABottom: double = -1);
    procedure SetFont(const AFontFamily: string);
    function AddPage(AOrientation: TFPDFOrientation = poPortrait;
      APageUnit: TFPDFUnit = puMM; APageFormat: TFPDFPageFormat = pfA4): TFPDFPage; overload;
    function AddPage(AOrientation: TFPDFOrientation;
      APageUnit: TFPDFUnit; APageSize: TFPDFPageSize): TFPDFPage; overload;
    function AddPage(AOrientation: TFPDFOrientation;
      APageUnit: TFPDFUnit; APageWidth, APageHeight: double): TFPDFPage; overload;
    procedure AddPage(APage: TFPDFPage; ADefaultMargins: boolean = False); overload;
    procedure AddBand(ABand: TFPDFBand); overload;
    function AddBand(ABandType: TFPDFBandType; ABandHeight: double;
      ADrawEvent: TFPDFBandDrawEvent): TFPDFBand; overload;
    property Options: TFPDFReportOptions read FOptions;
    property EngineOptions: TFPDFEngineOptions read FEngineOptions;
  end;

  TFPDFReportOptions = class
  private
    FAuthor: string;
  public
    property Author: string read FAuthor write FAuthor;
  end;

  TFPDFEngineOptions = class
  private
    FDoublePass: boolean;
  public
    property DoublePass: boolean read FDoublePass write FDoublePass;
  end;

  TFPDFEngine = class
  private
    FReport: TFPDFReport;
    FOwnsReport: boolean;
    FPDF: TFPDFExt2;
    FCompressed: Boolean;
    FDoublePass: Boolean;
    FFinalPass: boolean;
    FFreeSpace: double;
    FActiveMiddleBands: TFPDFBandList;
    FTotalPages: integer;
    FEndlessHeight: double;
    FBreakPage: boolean;
    FInMargins: boolean;
    function IsAnyOfTypes(ABand: TFPDFBand; ABandTypes: array of TFPDFBandType): boolean;
    function GetBands(APage: TFPDFPage; ABandTypes: array of TFPDFBandType): TFPDFBandArray;
    function IsMiddleOrReportFooterBand(ABand: TFPDFBand): boolean;
    function IsMarginBand(ABand: TFPDFBand): boolean;
    function OffsetEnabled(ABand: TFPDFBand): boolean;
    function GetCurrentPage: integer;



    procedure RunFirstPass;
    procedure DrawReport;
    procedure StartNewPage(APage: TFPDFPage);
    procedure InitBands(APage: TFPDFPage); overload;
    procedure InitBands(APage: TFPDFPage; ABandTypes: array of TFPDFBandType); overload;
    procedure CalculatePageMargins(APage: TFPDFPage);
    procedure CalculateBandDimensions(APage: TFPDFPage);
    procedure DrawPage(APage: TFPDFPage);
    procedure DrawMarginBands(APage: TFPDFPage);
    procedure DrawFirstBands(APage: TFPDFPage);
    procedure DrawMiddleBands(APage: TFPDFPage);
    procedure DrawLastBands(APage: TFPDFPage);
    procedure DrawBands(APage: TFPDFPage; ABandType: TFPDFBandType); overload;
    procedure DrawBands(APage: TFPDFPage; ABandTypes: array of TFPDFBandType); overload;
    procedure DrawBand(APage: TFPDFPage; ABand: TFPDFBand);
  private
    procedure NotifyReportStart;
    procedure NotifyReportEnd;

    procedure SetDoublePass(const Value: boolean);
    property PDF: TFPDFExt2 read FPDF;
    property FinalPass: boolean read FFinalPass write FFinalPass;
    property EndlessHeight: double read FEndlessHeight;
  public
    constructor Create(AReport: TFPDFReport; AOwnsReport: boolean = True);
    destructor Destroy; override;
    procedure SaveToFile(const AFileName: String);
    procedure SaveToStream(AStream: TStream);
    property CurrentPage: integer read GetCurrentPage;
    property TotalPages: integer read FTotalPages;
    property Compressed: Boolean read FCompressed write FCompressed;
  end;

  TFPDFPage = class
  private
    FBands: TFPDFBandList;
    FOrientation: TFPDFOrientation;
    FPageUnit: TFPDFUnit;
    FPageHeight: double;
    FPageWidth: double;
    FEndlessHeight: boolean;
    FVisible: boolean;

    FDefTopMargin: double;
    FDefBottomMargin: double;
    FDefLeftMargin: double;
    FDefRightMargin: double;
    FCurTopMargin: double;
    FCurBottomMargin: double;
    FCurLeftMargin: double;
    FCurRightMargin: double;
  private
    property DefTopMargin: double read FDefTopMargin write FDefTopMargin;
    property DefBottomMargin: double read FDefBottomMargin write FDefBottomMargin;
    property DefLeftMargin: double read FDefLeftMargin write FDefLeftMargin;
    property DefRightMargin: double read FDefRightMargin write FDefRightMargin;

    property Bands: TFPDFBandList read FBands;
  public
    constructor Create(AOrientation: TFPDFOrientation = poPortrait;
      APageUnit: TFPDFUnit = puMM; APageFormat: TFPDFPageFormat = pfA4); overload;
    constructor Create(AOrientation: TFPDFOrientation;
      APageUnit: TFPDFUnit; APageSize: TFPDFPageSize); overload;
    constructor Create(AOrientation: TFPDFOrientation;
      APageUnit: TFPDFUnit; APageWidth, APageHeight: double); overload;
    destructor Destroy; override;
    procedure AddBand(ABand: TFPDFBand); overload;
    function AddBand(ABandType: TFPDFBandType; ABandHeight: double;
      ADrawEvent: TFPDFBandDrawEvent): TFPDFBand; overload;
    property Orientation: TFPDFOrientation read FOrientation write FOrientation;
    property PageUnit: TFPDFUnit read FPageUnit;
    property PageHeight: double read FPageHeight write FPageHeight;
    property PageWidth: double read FPageWidth write FPageWidth;
    property EndlessHeight: boolean read FEndlessHeight write FEndlessHeight;
    property TopMargin: double read FCurTopMargin write FCurTopMargin;
    property BottomMargin: double read FCurBottomMargin write FCurBottomMargin;
    property LeftMargin: double read FCurLeftMargin write FCurLeftMargin;
    property RightMargin: double read FCurRightMargin write FCurRightMargin;
    property Visible: boolean read FVisible write FVisible;
  end;

  TFPDFEngineEventArgs = class
  private
    FEngine: TFPDFEngine;

    function GetCurrentPage: integer;
    function GetFinalPass: boolean;
    function GetTotalPages: integer;
  public
    constructor Create(AEngine: TFPDFEngine);
    property CurrentPage: integer read GetCurrentPage;
    property FinalPass: boolean read GetFinalPass;
    property TotalPages: integer read GetTotalPages;
  end;

  TFPDFReportEventArgs = class(TFPDFEngineEventArgs);

  TFPDFPageEventArgs = class(TFPDFEngineEventArgs)
  private
    FPage: TFPDFPage;
    FRecalculateDimensions: boolean;
    FLeftMargin: double;
    FTopMargin: double;
    FRightMargin: double;
    FBottomMargin: double;

    function GetOrientation: TFPDFOrientation;
    function GetPageHeight: double;
    function GetPageWidth: double;
  public
    constructor Create(AEngine: TFPDFEngine; APage: TFPDFPage); reintroduce;
    property RecalculateDimensions: boolean read FRecalculateDimensions write FRecalculateDimensions;
    property Orientation: TFPDFOrientation read GetOrientation;
    property PageHeight: double read GetPageHeight;
    property PageWidth: double read GetPageWidth;
    property LeftMargin: double read FLeftMargin write FLeftMargin;
    property TopMargin: double read FTopMargin write FTopMargin;
    property RightMargin: double read FRightMargin write FRightMargin;
    property BottomMargin: double read FBottomMargin write FBottomMargin;
  end;

  TFPDFBandEventArgs = class(TFPDFPageEventArgs)
  private
    FBand: TFPDFBand;
    FPDF: IFPDF;

    procedure SetPDF(APDF: IFPDF);
  public
    constructor Create(AEngine: TFPDFEngine; APage: TFPDFPage; ABand: TFPDFBand);
    property Band: TFPDFBand read FBand;
    property PDF: IFPDF read FPDF;
  end;

  TFPDFBandInitArgs = class(TFPDFBandEventArgs)
  private
    function GetHeight: double;
    function GetWidth: double;
    procedure SetHeight(const Value: double);
    procedure SetWidth(const Value: double);
  public
    property Height: double read GetHeight write SetHeight;
    property Width: double read GetWidth write SetWidth;
  end;

  TFPDFBandDrawArgs = class(TFPDFBandEventArgs)
  private
    FDrawAgain: boolean;
    FFreeSpace: double;
    FReservedSpace: double;

    procedure SetFreeSpace(const Value: double);
    procedure SetReservedSpace(const Value: double);
  public
    property DrawAgain: boolean read FDrawAgain write FDrawAgain;
    property FreeSpace: double read FFreeSpace;
    property ReservedSpace: double read FReservedSpace;
  end;

  TFPDFBand = class
  private
    FBandType: TFPDFBandType;
    FName: string;
    FAutoHeight: boolean;
    FHeight: double;
    FWidth: double;
    FVisible: boolean;

    procedure DoInit(Args: TFPDFBandInitArgs);
    procedure DoDraw(Args: TFPDFBandDrawArgs);
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); virtual;
    procedure OnDraw(Args: TFPDFBandDrawArgs); virtual;
  public
    constructor Create(ABandType: TFPDFBandType); virtual;
    property BandType: TFPDFBandType read FBandType;
    property AutoHeight: boolean read FAutoHeight write FAutoHeight;
    property Height: double read FHeight write FHeight;
    property Width: double read FWidth write FWidth;
    property Visible: boolean read FVisible write FVisible;
  end;

  TFPDFPageList = class
  private
    FInternalList: TObjectList;
  protected
    function GetItem(Index: Integer): TFPDFPage;
    procedure SetItem(Index: Integer; Value: TFPDFPage);
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    function Add(Value: TFPDFPage): Integer;
    function Count: Integer;
    property Objects[Index: Integer]: TFPDFPage read GetItem write SetItem; default;
  end;

  TFPDFBandList = class
  private
    FInternalList: TObjectList;
  protected
    function GetItem(Index: Integer): TFPDFBand;
    procedure SetItem(Index: Integer; Value: TFPDFBand);
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    function Add(Value: TFPDFBand): Integer;
    procedure Clear;
    function Count: Integer;
    function Contains(const Value: TFPDFBand): Boolean;
    function IndexOf(const Value: TFPDFBand): Integer;
    function Remove(const Value: TFPDFBand): Integer;
    property Objects[Index: Integer]: TFPDFBand read GetItem write SetItem; default;
  end;

  TFPDFExt2 = class(TFPDFExt)
  private
    function GetCurrentFontFamily: string;
    function GetCurrentFontSize: double;
    function GetCurrentFontSizePt: double;
  public
    property CurrentFontFamily: string read GetCurrentFontFamily;
    property CurrentFontSize: double read GetCurrentFontSize;
    property CurrentFontSizePt: double read GetCurrentFontSizePt;
  end;

  IFPDF = interface
  ['{5AA2E972-7B70-4C25-B068-D2678B905EEB}']
    function GetOrientation: TFPDFOrientation;
    function GetCurrentPage: integer;
    function GetCurrentFontSize: double;
    function GetCurrentFontSizePt: double;
    function GetCurrentFontFamily: string;
    function GetTextColor: string;

    procedure SetFont(const AFamily: String; const AStyle: String = ''; ASize: Double = 0.0); overload;
    procedure SetFont(ASize: Double; const AStyle: String); overload;
    procedure SetDrawColor(color: TFPDFColor); overload;
    procedure SetDrawColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetFillColor(color: TFPDFColor); overload;
    procedure SetFillColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetTextColor(color: TFPDFColor); overload;
    procedure SetTextColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetTextColorAsString(const Value: string);
    procedure SetUnderline(fUnderline: Boolean = False);
    procedure SetDash(ABlack, AWhite: double); overload;
    procedure SetDash(AWidth: double); overload;
    procedure SetLineWidth(vWidth: Double);

    function WordWrap(var AText: string; AMaxWidth: Double; AIndent: double = 0): integer;
    function GetNumLines(const AText: string; AWidth: Double; AIndent: double = 0): integer;
    function GetStringWidth(const vText: String): Double;
    function GetStringHeight(const AText: string; AWidth: double;
      ALineSpacing: double = 0; AIndent: double = 0): double;
    procedure Text(vX, vY: Double; const vText: String);
    function TextBox(x, y, w, h: Double): Double; overload;
    function TextBox(x, y, w, h: Double; const AText: string;
      const vAlign: char = 'T'; const hAlign: char = 'L';
      border: Double = 1; const link: string = '';
      force: boolean = true; hmax: Double = 0;
      vOffSetX: Double = 0; vOffSetY: Double = 0;
      AIndent: double = 0; vLineSpacing: double = 0): Double; overload;
    function TextBox(x, y, w, h: double; const AText: string;
      const vAlign: char = 'T'; const hAlign: char = 'L';
      ABorder: boolean = True; AWordWrap: boolean = True;
      AScale: boolean = False; ALineSpacing: double = 0): double; overload;

    procedure Rotate(NewAngle: Double = 0; vX: Double = -1; vY: Double = -1);
    procedure Line(vX1, vY1, vX2, vY2: Double);
    procedure Rect(vX, vY, vWidht, vHeight: Double; const vStyle: String = '');
    procedure DashedLine(vX1, vY1, vX2, vY2: Double; ADashWidth: double = 1);
    procedure DashedRect(vX, vY, vWidht, vHeight: Double; const vStyle: String = ''; ADashWidth: double = 1);
    procedure Image(x, y, w, h: double; AStream: TStream;
      const vAlign: char = 'T'; const hAlign: char = 'L';
      AStretched: boolean = False);
    procedure CodeEAN13(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure CodeEAN8(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure CodeI25(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure Code39(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure Code128(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    {$IFDEF DelphiZXingQRCode}
    function QRCode(vX: double; vY: double; const QRCodeData: String;
      DotSize: Double = 0; AEncoding: TQRCodeEncoding = qrAuto): double; overload;
    procedure QRCode(vX: double; vY: double; vQRCodeSize: double;
      const QRCodeData: String; AEncoding: TQRCodeEncoding = qrAuto); overload;
    {$ENDIF}

    property Orientation: TFPDFOrientation read GetOrientation;
    property CurrentPage: integer read GetCurrentPage;
    property CurrentFontSize: double read GetCurrentFontSize;
    property CurrentFontSizePt: double read GetCurrentFontSizePt;
    property CurrentFontFamily: string read GetCurrentFontFamily;
    property TextColor: string read GetTextColor write SetTextColorAsString;
  end;

  EFPDFReportException = class(Exception)
  end;

  TImgType = (itUnknown, itPng, itJpg);

  TImageUtils = class
  private
    function ReadMWord(AStream: TStream): Word;
    function GetJPGSize(AStream: TStream; var wWidth, wHeight: Word): boolean;
    function GetPNGSize(AStream: TStream; var wWidth, wHeight: Word): boolean;
  public
    function GetImageSize(const sFile: string; var wWidth, wHeight: Word): boolean; overload;
    function GetImageSize(ABytes: array of Byte; var wWidth, wHeight: Word): boolean; overload;
    function GetImageSize(AStream: TStream; var wWidth, wHeight: Word): boolean; overload;
    function GetImageType(const sFile: string): TImgType; overload;
    function GetImageType(ABytes: array of Byte): TImgType; overload;
    function GetImageType(AStream: TStream): TImgType; overload;
  end;

implementation

const
  cMarginBands: array[0..3] of TFPDFBandType = (
    btLeftMargin,
    btTopMargin,
    btRightMargin,
    btBottomMargin
  );
  cFirstBands: array[0..1] of TFPDFBandType =(
    btReportHeader,
    btPageHeader
  );
  cMiddleBands: array[0..0] of TFPDFBandType = (
    btData//, btReportFooter
  );
  cLastBands: array[0..0] of TFPDFBandType = (
    btPageFooter
  );

{const
  cMarginBands: TFPDFBandTypeArray = [
    btLeftMargin,
    btTopMargin,
    btRightMargin,
    btBottomMargin
  ];
  cFirstBands: TFPDFBandTypeArray = [
    btReportHeader,
    btPageHeader
  ];
  cMiddleBands: TFPDFBandTypeArray = [
    btData//, btReportFooter
  ];
  cLastBands: TFPDFBandTypeArray = [
    btPageFooter
  ];}
type
  TFPDFAnonymousBand = class(TFPDFBand)
  private
    FHeight: double;
    FInitEvent: TFPDFBandInitEvent;
    FDrawEvent: TFPDFBandDrawEvent;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ABandType: TFPDFBandType; ABandHeight: double; ADrawEvent: TFPDFBandDrawEvent); reintroduce; overload;
    constructor Create(ABandType: TFPDFBandType; AInitEvent: TFPDFBandInitEvent; ADrawEvent: TFPDFBandDrawEvent); reintroduce; overload;
  end;

  TFPDFWrapper = class(TInterfacedObject, IFPDF)
  private
    FPDF: TFPDFExt2;
    FOffsetX: double;
    FOffsetY: double;
    FModified: boolean;
    FHighestX: double;
    FHighestY: double;

    function GetOrientation: TFPDFOrientation;
    function GetCurrentPage: integer;
    function GetCurrentFontSize: double;
    function GetCurrentFontSizePt: double;
    function GetCurrentFontFamily: string;
    function GetTextColor: string;
    procedure ApplyOffset(var x, y: double);
    procedure ApplyHighestX(x: double);
    procedure ApplyHighestY(y: double);
    procedure ApplyHighestXY(x, y: double);
  public
    constructor Create(APDF: TFPDFExt2; AOffsetXY: boolean);
    property Modified: boolean read FModified;
    property HighestX: double read FHighestX;
    property HighestY: double read FHighestY;
  public
    procedure SetFont(const AFamily: String; const AStyle: String = ''; ASize: Double = 0.0); overload;
    procedure SetFont(ASize: Double; const AStyle: String); overload;
    procedure SetDrawColor(color: TFPDFColor); overload;
    procedure SetDrawColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetFillColor(color: TFPDFColor); overload;
    procedure SetFillColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetTextColor(color: TFPDFColor); overload;
    procedure SetTextColor(ValR: Integer = 0; ValG: Integer = -1; ValB: Integer = -1); overload;
    procedure SetTextColorAsString(const Value: string);
    procedure SetUnderline(fUnderline: Boolean = False);
    procedure SetDash(ABlack, AWhite: double); overload;
    procedure SetDash(AWidth: double); overload;
    procedure SetLineWidth(vWidth: Double);

    function WordWrap(var AText: string; AMaxWidth: Double; AIndent: double = 0): integer;
    function GetNumLines(const AText: string; AWidth: Double; AIndent: double = 0): integer;
    function GetStringWidth(const vText: String): Double;
    function GetStringHeight(const AText: string; AWidth: double;
      ALineSpacing: double = 0; AIndent: double = 0): double;
    procedure Text(vX, vY: Double; const vText: String);
    function TextBox(x, y, w, h: Double): Double; overload;
    function TextBox(x, y, w, h: Double; const AText: string;
      const vAlign: char = 'T'; const hAlign: char = 'L';
      border: Double = 1; const link: string = '';
      force: boolean = true; hmax: Double = 0;
      vOffSetX: Double = 0; vOffSetY: Double = 0;
      AIndent: double = 0; vLineSpacing: double = 0): Double; overload;
    function TextBox(x, y, w, h: double; const AText: string;
      const vAlign: char = 'T'; const hAlign: char = 'L';
      ABorder: boolean = True; AWordWrap: boolean = True;
      AScale: boolean = False; ALineSpacing: double = 0): double; overload;

    procedure Rotate(NewAngle: Double = 0; vX: Double = -1; vY: Double = -1);
    procedure Line(vX1, vY1, vX2, vY2: Double);
    procedure Rect(vX, vY, vWidht, vHeight: Double; const vStyle: String = '');
    procedure DashedLine(vX1, vY1, vX2, vY2: Double; ADashWidth: double = 1);
    procedure DashedRect(vX, vY, vWidht, vHeight: Double; const vStyle: String = ''; ADashWidth: double = 1);
    procedure Image(x, y, w, h: double; AStream: TStream;
      const vAlign: char = 'T'; const hAlign: char = 'L';
      AStretched: boolean = False);
    procedure CodeEAN13(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure CodeEAN8(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure CodeI25(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure Code39(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure Code128(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    {$IFDEF DelphiZXingQRCode}
    function QRCode(vX: double; vY: double; const QRCodeData: String;
      DotSize: Double = 0; AEncoding: TQRCodeEncoding = qrAuto): double; overload;
    procedure QRCode(vX: double; vY: double; vQRCodeSize: double;
      const QRCodeData: String; AEncoding: TQRCodeEncoding = qrAuto); overload;
    {$ENDIF}
  end;

procedure Error(const AMsg: string; E: Exception = nil);
var
  s: String;
begin
  //Fatal error
  s := AMsg;
  if Assigned(E) then
    s := s + sLineBreak + E.Message;

  raise EFPDFReportException.Create('FPDFReport error: ' + s);
end;

{ TFPDFReport }

procedure TFPDFReport.AddBand(ABand: TFPDFBand);
begin
  CheckHasPage;
  FPages[FPages.Count - 1].AddBand(ABand);
end;

function TFPDFReport.AddBand(ABandType: TFPDFBandType; ABandHeight: double;
  ADrawEvent: TFPDFBandDrawEvent): TFPDFBand;
begin
  CheckHasPage;
  Result := FPages[FPages.Count - 1].AddBand(ABandType, ABandHeight, ADrawEvent);
end;

function TFPDFReport.AddPage(AOrientation: TFPDFOrientation;
  APageUnit: TFPDFUnit; APageWidth, APageHeight: double): TFPDFPage;
begin
  Result := TFPDFPage.Create(AOrientation, APageUnit, APageWidth, APageHeight);
  AddPage(Result, True);
end;

procedure TFPDFReport.AddPage(APage: TFPDFPage; ADefaultMargins: boolean);
begin
  FPages.Add(APage);
  if ADefaultMargins then
  begin
    APage.DefLeftMargin := FDefaultPageMargins.Left;
    APage.DefTopMargin := FDefaultPageMargins.Top;
    APage.DefRightMargin := FDefaultPageMargins.Right;
    APage.DefBottomMargin := FDefaultPageMargins.Bottom;

    APage.LeftMargin := FDefaultPageMargins.Left;
    APage.TopMargin := FDefaultPageMargins.Top;
    APage.RightMargin := FDefaultPageMargins.Right;
    APage.BottomMargin := FDefaultPageMargins.Bottom;
  end;
end;

function TFPDFReport.AddPage(AOrientation: TFPDFOrientation;
  APageUnit: TFPDFUnit; APageSize: TFPDFPageSize): TFPDFPage;
begin
  Result := TFPDFPage.Create(AOrientation, APageUnit, APageSize);
  AddPage(Result, True);
end;

function TFPDFReport.AddPage(AOrientation: TFPDFOrientation;
  APageUnit: TFPDFUnit; APageFormat: TFPDFPageFormat): TFPDFPage;
begin
  Result := TFPDFPage.Create(AOrientation, APageUnit, APageFormat);
  AddPage(Result, True);
end;

procedure TFPDFReport.CheckHasPage;
begin
  if FPages.Count = 0 then
    Error('No page has been added yet');
end;

constructor TFPDFReport.Create;
begin
  FPages := TFPDFPageList.Create;
  FOptions := TFPDFReportOptions.Create;
  FEngineOptions := TFPDFEngineOptions.Create;
end;

destructor TFPDFReport.Destroy;
begin
  FPages.Free;
  FOptions.Free;
  FEngineOptions.Free;
  inherited;
end;

procedure TFPDFReport.DoEndReport(Args: TFPDFReportEventArgs);
begin
  OnEndReport(Args);
end;

procedure TFPDFReport.DoStartReport(Args: TFPDFReportEventArgs);
begin
  OnStartReport(Args);
end;

function TFPDFReport.HasEndlessPage: boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pages.Count - 1 do
    if Pages[I].EndlessHeight then
    begin
      Result := True;
      Break;
    end;
end;

procedure TFPDFReport.OnEndReport(Args: TFPDFReportEventArgs);
begin
end;

procedure TFPDFReport.OnStartReport(Args: TFPDFReportEventArgs);
begin
end;

procedure TFPDFReport.SetFont(const AFontFamily: string);
begin
//  CheckHasPage;

  FDefaultFontFamily := AFontFamily;
end;

procedure TFPDFReport.SetMargins(ALeft, ATop, ARight, ABottom: double);
begin
//  CheckHasPage;

  FDefaultPageMargins.Left := ALeft;
  FDefaultPageMargins.Top := ATop;
  if ARight = -1 then
    FDefaultPageMargins.Right := ALeft
  else
    FDefaultPageMargins.Right := ARight;
  if ABottom = -1 then
    FDefaultPageMargins.Bottom := ATop
  else
    FDefaultPageMargins.Bottom := ABottom;
end;

{ TFPDFEngine }

procedure TFPDFEngine.CalculateBandDimensions(APage: TFPDFPage);
var
  Band: TFPDFBand;
  I: Integer;
begin
  for I := 0 to APage.Bands.Count - 1 do
  begin
    Band := APage.Bands[I];
    if IsMarginBand(Band) then
    begin
      case Band.BandType of
        btLeftMargin,
        btRightMargin:
          Band.Height := APage.PageHeight - APage.DefTopMargin - APage.DefBottomMargin;
        btTopMargin,
        btBottomMargin:
          Band.Width := APage.PageWidth - APage.DefLeftMargin - APage.DefRightMargin;
      else
        //
      end;
    end
    else
      Band.Width := APage.PageWidth - APage.LeftMargin - APage.RightMargin;

    if Band.BandType = btOverlay then
      Band.Height := APage.PageHeight - APage.TopMargin - APage.BottomMargin;
  end;
end;

procedure TFPDFEngine.CalculatePageMargins(APage: TFPDFPage);
var
  LeftMargin, TopMargin, RightMargin, BottomMargin: double;
  Bands: TFPDFBandArray;
  Band: TFPDFBand;
  I: Integer;
begin
  LeftMargin := APage.DefLeftMargin;
  TopMargin := APage.DefTopMargin;
  RightMargin := APage.DefRightMargin;
  BottomMargin := APage.DefBottomMargin;

  Bands := GetBands(APage, cMarginBands);
  for I := 0 to Length(Bands) - 1 do
  begin
    Band := Bands[I];
    if not Band.Visible then
      Continue;
    case Band.BandType of
      btLeftMargin:
        LeftMargin := LeftMargin + Band.Width;
      btTopMargin:
        TopMargin := TopMargin + Band.Height;
      btRightMargin:
        RightMargin := RightMargin + Band.Width;
      btBottomMargin:
        BottomMargin := BottomMargin + Band.Height;
    end;
  end;

  APage.LeftMargin := LeftMargin;
  APage.TopMargin := TopMargin;
  APage.RightMargin := RightMargin;
  APage.BottomMargin := BottomMargin;

  CalculateBandDimensions(APage);
end;

procedure TFPDFEngine.RunFirstPass;
var
  Engine: TFPDFEngine;
begin
  FTotalPages := 0;
  FEndlessHeight := 0;
  if FDoublePass then
  begin
    Engine := TFPDFEngine.Create(FReport, False);
    try
      Engine.SetDoublePass(False);
      Engine.DrawReport;

      // current page
      FTotalPages := Engine.PDF.page;
      FEndlessHeight := Engine.EndlessHeight;
    finally
      Engine.Free;
    end;
  end;
  FFinalPass := True;
end;

constructor TFPDFEngine.Create(AReport: TFPDFReport; AOwnsReport: boolean);
var
  DefPageSize: TFPDFPageSize;
begin
  inherited Create;
  FReport := AReport;
  FOwnsReport := AOwnsReport;
  FActiveMiddleBands := TFPDFBandList.Create(False);
  FCompressed := True;
  FDoublePass := AReport.EngineOptions.DoublePass or AReport.HasEndlessPage;
  FFinalPass := False;

  if AReport.Pages.Count > 0 then
  begin
    DefPageSize.w := AReport.Pages[0].PageWidth;
    DefPageSize.h := AReport.Pages[0].PageHeight;
    FPDF := TFPDFExt2.Create(AReport.Pages[0].Orientation,
      AReport.Pages[0].PageUnit, DefPageSize);
  end
  else
    FPDF := TFPDFExt2.Create;

  FPDF.SetFont(AReport.DefaultFontFamily, '', 12);
end;

destructor TFPDFEngine.Destroy;
begin
  FPDF.Free;
  FActiveMiddleBands.Free;
  if FOwnsReport then
    FReport.Free;
  inherited;
end;

procedure TFPDFEngine.DrawBand(APage: TFPDFPage; ABand: TFPDFBand);
var
  PreviousX, PreviousY, UsedSpace, ReservedSpace: double;
  PDFWrapper: TFPDFWrapper;
  Args: TFPDFBandDrawArgs;
  CurrentBandIndex: integer;
  I: integer;
begin
  if (ABand.BandType = btReportHeader) and (FPDF.page > 1) then
    Exit;

  if IsMiddleOrReportFooterBand(ABand) and (FFreeSpace < ABand.Height) and
    not FReport.HasEndlessPage then
  begin
    FBreakPage := True;
    Exit;
  end;

//  if IsMiddleOrReportFooterBand(ABand) and (not FActiveMiddleBands.Contains(ABand)
//    or (FFreeSpace <= 0)) then
//    Exit;

  if not ABand.Visible then
    Exit;

//  if IsMiddleOrReportFooterBand(ABand) and not FReport.HasEndlessPage and
//    (FFreeSpace < ABand.Height) then
//  begin
//    FBreakPage := True;
//    Exit;
//  end;

  PreviousX := FPDF.GetX;
  PreviousY := FPDF.GetY;

  case ABand.BandType of
    btTopMargin,
    btLeftMargin:
      FPDF.SetXY(APage.DefLeftMargin, APage.DefTopMargin);

    btRightMargin:
      FPDF.SetXY(APage.PageWidth - APage.RightMargin, APage.DefTopMargin);

    btBottomMargin:
      FPDF.SetXY(APage.DefLeftMargin, APage.PageHeight - APage.BottomMargin);

    btOverlay:
      FPDF.SetXY(APage.LeftMargin, APage.TopMargin);
  else
    //
  end;

  PDFWrapper := TFPDFWrapper.Create(FPDF, OffsetEnabled(ABand));
  Args := TFPDFBandDrawArgs.Create(Self, APage, ABand);
  try
    Args.SetPDF(PDFWrapper);
    Args.SetFreeSpace(FFreeSpace);
    ReservedSpace := 0;
    if IsMiddleOrReportFooterBand(ABand) and (FActiveMiddleBands.Count > 1) then
    begin
      CurrentBandIndex := FActiveMiddleBands.IndexOf(ABand);
      for I := CurrentBandIndex + 1 to FActiveMiddleBands.Count - 1 do
      begin
        if ABand.Height > (FFreeSpace - ReservedSpace - FActiveMiddleBands[I].Height) then
          Break;
        ReservedSpace := ReservedSpace + FActiveMiddleBands[I].Height;
      end;
    end;
    Args.SetReservedSpace(ReservedSpace);

    ABand.DoDraw(Args);

    if not FInMargins then
    begin
      if PDFWrapper.Modified then
      begin
        if ABand.AutoHeight then
          UsedSpace := PDFWrapper.HighestY - PreviousY
        else
          UsedSpace := ABand.Height;

        FFreeSpace := FFreeSpace - UsedSpace;
        if FReport.HasEndlessPage then
          FEndlessHeight := FEndlessHeight + UsedSpace;
        FPDF.SetXY(PreviousX, PreviousY + UsedSpace);
      end;

      if FActiveMiddleBands.Contains(ABand) and not Args.DrawAgain then
        FActiveMiddleBands.Remove(ABand);
    end;
  finally
    Args.Free;
  end;
end;

procedure TFPDFEngine.DrawBands(APage: TFPDFPage; ABandTypes: array of TFPDFBandType);
var
  Bands: TFPDFBandArray;
  Band: TFPDFBand;
  I: Integer;
begin
  Bands := GetBands(APage, ABandTypes);
  for I := 0 to Length(Bands) - 1  do
  begin
    Band := Bands[I];
    DrawBand(APage, Band);
    if FBreakPage then
    begin
      FBreakPage := False;
      Break;
    end;
  end;
end;

procedure TFPDFEngine.DrawFirstBands(APage: TFPDFPage);
var
  I: integer;
begin
  for I := 0 to Length(cFirstBands) - 1 do
  begin
    if (cFirstBands[I] = btReportHeader) and (FPDF.page > 1) then
      Continue;
    DrawBands(APage, cFirstBands[I]);
  end;
end;

procedure TFPDFEngine.DrawLastBands(APage: TFPDFPage);
var
  I: integer;
  TotalHeight: double;
  Bands: TFPDFBandArray;
  Band: TFPDFBand;
begin
  if not FReport.HasEndlessPage then
  begin
    TotalHeight := 0;
    Bands := GetBands(APage, cLastBands);
    for I := 0 to Length(Bands) - 1 do
    begin
      Band := Bands[I];
      if not Band.Visible then
        Continue;
      TotalHeight := TotalHeight + Band.Height;
    end;
    // Print on bottom of page
    if TotalHeight > 0 then
      FPDF.SetXY(APage.LeftMargin, APage.PageHeight - APage.BottomMargin - TotalHeight);
  end;

  for I := 0 to Length(cLastBands) - 1 do
  begin
//    if (cLastBands[I] = TFPDFReportFooterBand) and (FPDF.page > 1) then
//      Continue;
    DrawBands(APage, cLastBands[I]);
  end;
end;

procedure TFPDFEngine.DrawMarginBands(APage: TFPDFPage);
var
  I: integer;
begin
  for I := 0 to Length(cMarginBands) - 1 do
  begin
    DrawBands(APage, cMarginBands[I]);
  end;
end;

procedure TFPDFEngine.DrawMiddleBands(APage: TFPDFPage);
var
  Band: TFPDFBand;
  Bands : TFPDFBandArray;
  I: Integer;
begin
  // Calculate free space
  {for Band in GetBands(APage, cLastBands) do
  begin
    if Band.Visible then
      FFreeSpace := FFreeSpace - Band.Height;
  end;}

  Bands := GetBands(APage, cLastBands);

  for I := Low(Bands) to High(Bands) do
  begin
    if Bands[I].Visible then
      FFreeSpace := FFreeSpace - Bands[I].Height;
  end;

  while FActiveMiddleBands.Count > 0 do
  begin
    Band := FActiveMiddleBands[0];
    DrawBand(APage, Band);
    if FBreakPage then
    begin
      FBreakPage := False;
      Break;
    end;
  end;
end;

procedure TFPDFEngine.DrawBands(APage: TFPDFPage; ABandType: TFPDFBandType);
begin
  DrawBands(APage, [ABandType]);
end;

procedure TFPDFEngine.DrawPage(APage: TFPDFPage);
begin
  repeat
    StartNewPage(APage);
    // Draw bands
    DrawFirstBands(APage);
    DrawMiddleBands(APage);
    DrawLastBands(APage);
    DrawBands(APage, btOverlay);
  until FActiveMiddleBands.Count = 0;
end;

procedure TFPDFEngine.DrawReport;
var
  Page: TFPDFPage;
  I: Integer;
begin
  NotifyReportStart;
  for I := 0 to FReport.Pages.Count - 1 do
  begin
    Page := FReport.Pages[I];
    InitBands(Page);
    DrawPage(Page);
  end;
  NotifyReportEnd;
end;

function TFPDFEngine.GetBands(APage: TFPDFPage;
  ABandTypes: array of TFPDFBandType): TFPDFBandArray;
var
  Band: TFPDFBand;
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to APage.Bands.Count - 1 do
  begin
    Band := APage.Bands[I];
    if IsAnyOfTypes(Band, ABandTypes) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Band;
    end;
  end;
end;

function TFPDFEngine.GetCurrentPage: integer;
begin
  Result := FPDF.page;
end;

procedure TFPDFEngine.InitBands(APage: TFPDFPage);
//var MergedBands : array of TFPDFBandType;
//I, J : Integer;
begin
  InitBands(APage, cMarginBands);
  CalculatePageMargins(APage);
//  CalculateBandDimensions(APage, nil);
  FActiveMiddleBands.Clear;
  {SetLength(MergedBands, Length(cFirstBands) + Length(cMiddleBands) + Length(cLastBands));
  J :=0;
  for i := Low(cFirstBands) to High(cFirstBands) do
  begin
    MergedBands[j] := cFirstBands[i];
  end;
  for i := Low(cMiddleBands) to High(cMiddleBands) do
  begin
    MergedBands[j] := cMiddleBands[i];
  end;
  for i := Low(cLastBands) to High(cLastBands) do
  begin
    MergedBands[j] := cLastBands[i];
  end;

  InitBands(APage, MergedBands);}

  InitBands(APage, [btReportHeader,btPageHeader,btData,btPageFooter]);
  InitBands(APage, [btReportFooter]);

  if FReport.HasEndlessPage then
    FEndlessHeight := FEndlessHeight + APage.TopMargin;// + APage.BottomMargin;
end;

procedure TFPDFEngine.InitBands(APage: TFPDFPage;
  ABandTypes: array of TFPDFBandType);
var
  Bands: TFPDFBandArray;
  Band: TFPDFBand;
  I: Integer;
  Args: TFPDFBandInitArgs;
begin
  Bands := GetBands(APage, ABandTypes);
  for I := 0 to Length(Bands) - 1 do
  begin
    Band := Bands[I];
    Args := TFPDFBandInitArgs.Create(Self, APage, Band);
    try
      Band.DoInit(Args);
      if Band.Visible then
      begin
        if not IsMarginBand(Band) and (Band.Height <= 0) then
          raise EFPDFReportException.CreateFmt(
            'Field %s.Height must be greater than 0', [Band.ClassName]);

        if IsMiddleOrReportFooterBand(Band) then
          FActiveMiddleBands.Add(Band);
      end;
    finally
      Args.Free;
    end;
  end;
end;

function TFPDFEngine.IsAnyOfTypes(ABand: TFPDFBand;
  ABandTypes: array of TFPDFBandType): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to Length(ABandTypes) - 1 do
    if ABand.BandType = ABandTypes[I] then
    begin
      Result := True;
      Break;
    end;
end;

function TFPDFEngine.IsMarginBand(ABand: TFPDFBand): boolean;
begin
  Result := IsAnyOfTypes(ABand, cMarginBands);
end;

function TFPDFEngine.IsMiddleOrReportFooterBand(ABand: TFPDFBand): boolean;
begin
  Result := IsAnyOfTypes(ABand, cMiddleBands) or (ABand.BandType = btReportFooter);
end;

procedure TFPDFEngine.NotifyReportEnd;
var
  Args: TFPDFReportEventArgs;
begin
  Args := TFPDFReportEventArgs.Create(Self);
  try
    FReport.DoEndReport(Args);
  finally
    Args.Free;
  end;
end;

procedure TFPDFEngine.NotifyReportStart;
var
  Args: TFPDFReportEventArgs;
begin
  Args := TFPDFReportEventArgs.Create(Self);
  try
    FReport.DoStartReport(Args);
  finally
    Args.Free;
  end;
end;

function TFPDFEngine.OffsetEnabled(ABand: TFPDFBand): boolean;
begin
  Result := not (ABand.BandType = btOverlay);
end;

procedure TFPDFEngine.SaveToFile(const AFileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TFPDFEngine.SaveToStream(AStream: TStream);
begin
  RunFirstPass;

  FPDF.SetCompression(FCompressed);
  DrawReport;
  FPDF.SaveToStream(AStream);
end;

procedure TFPDFEngine.SetDoublePass(const Value: boolean);
begin
  FDoublePass := Value;
end;

procedure TFPDFEngine.StartNewPage(APage: TFPDFPage);
var
  LPageSize: TFPDFPageSize;
begin
  CalculatePageMargins(APage);

  FPDF.SetMargins(APage.LeftMargin, APage.TopMargin, APage.RightMargin);
  FPDF.SetAutoPageBreak(False, APage.BottomMargin);

  if FFinalPass and FReport.HasEndlessPage then
    APage.PageHeight := FEndlessHeight;

  if APage.Orientation = poLandscape then
  begin
    LPageSize.h := APage.PageWidth;
    LPageSize.w := APage.PageHeight;
  end
  else
  begin
    LPageSize.w := APage.PageWidth;
    LPageSize.h := APage.PageHeight;
  end;

  FPDF.AddPage(APage.Orientation, LPageSize, ro0);

  FInMargins := True;
  try
    DrawMarginBands(APage);
  finally
    FInMargins := False;
  end;

  FPDF.SetXY(APage.LeftMargin, APage.TopMargin);

  if FReport.HasEndlessPage then
    FFreeSpace := 1e+6
  else
    FFreeSpace := APage.PageHeight - APage.TopMargin - APage.BottomMargin;
end;

{ TFPDFPage }

procedure TFPDFPage.AddBand(ABand: TFPDFBand);
begin
  FBands.Add(ABand);
end;

function TFPDFPage.AddBand(ABandType: TFPDFBandType; ABandHeight: double;
  ADrawEvent: TFPDFBandDrawEvent): TFPDFBand;
begin
  Result := TFPDFAnonymousBand.Create(ABandType, ABandHeight, ADrawEvent);
  AddBand(Result);
end;

constructor TFPDFPage.Create(AOrientation: TFPDFOrientation;
  APageUnit: TFPDFUnit; APageWidth, APageHeight: double);
begin
  FOrientation := AOrientation;
  FPageUnit := APageUnit;
  FPageHeight := APageHeight;
  FPageWidth := APageWidth;
  FBands := TFPDFBandList.Create;
end;

constructor TFPDFPage.Create(AOrientation: TFPDFOrientation;
  APageUnit: TFPDFUnit; APageSize: TFPDFPageSize);
begin
  if (AOrientation = poLandscape) and (APageSize.h > APageSize.w) then
    Create(AOrientation, APageUnit, APageSize.h, APageSize.w)
  else
    Create(AOrientation, APageUnit, APageSize.w, APageSize.h);
end;

constructor TFPDFPage.Create(AOrientation: TFPDFOrientation;
  APageUnit: TFPDFUnit; APageFormat: TFPDFPageFormat);

  function _getpagesize(APageFormat: TFPDFPageFormat; AScaleFactor: double): TFPDFPageSize;
  begin
    Result.w := cPDFPageSizes[APageFormat].w / AScaleFactor;
    Result.h := cPDFPageSizes[APageFormat].h / AScaleFactor;
  end;

var
  ScaleFactor: double;
  PageSize: TFPDFPageSize;
begin
  ScaleFactor := cUNIT[APageUnit];
  PageSize := _getpagesize(APageFormat, ScaleFactor);

  Create(AOrientation, APageUnit, PageSize);
end;

destructor TFPDFPage.Destroy;
begin
  FBands.Free;
  inherited;
end;

{ TFPDFWrapper }

procedure TFPDFWrapper.ApplyHighestX(x: double);
begin
  if x > FHighestX then
  begin
    FHighestX := x;
    if not FModified then
      FModified := True;
  end;
end;

procedure TFPDFWrapper.ApplyHighestXY(x, y: double);
begin
  ApplyHighestX(x);
  ApplyHighestY(y);
end;

procedure TFPDFWrapper.ApplyHighestY(y: double);
begin
  if y > FHighestY then
  begin
    FHighestY := y;
    if not FModified then
      FModified := True;
  end;
end;

procedure TFPDFWrapper.ApplyOffset(var x, y: double);
begin
  x := FOffsetX + x;
  y := FOffsetY + y;

  ApplyHighestX(x);
  ApplyHighestY(y);
end;

procedure TFPDFWrapper.Code128(const ABarCode: string; vX, vY, BarHeight,
  BarWidth: double);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + BarWidth, vY + BarHeight);

  FPDF.Code128(ABarCode, vX, vY, BarHeight, BarWidth);
end;

procedure TFPDFWrapper.Code39(const ABarCode: string; vX, vY, BarHeight,
  BarWidth: double);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + BarWidth, vY + BarHeight);

  FPDF.Code39(ABarCode, vX, vY, BarHeight, BarWidth);
end;

procedure TFPDFWrapper.CodeEAN13(const ABarCode: string; vX, vY, BarHeight,
  BarWidth: double);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + BarWidth, vY + BarHeight);

  FPDF.CodeEAN13(ABarCode, vX, vY, BarHeight, BarWidth);
end;

procedure TFPDFWrapper.CodeEAN8(const ABarCode: string; vX, vY, BarHeight,
  BarWidth: double);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + BarWidth, vY + BarHeight);

  FPDF.CodeEAN8(ABarCode, vX, vY, BarHeight, BarWidth);
end;

procedure TFPDFWrapper.CodeI25(const ABarCode: string; vX, vY, BarHeight,
  BarWidth: double);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + BarWidth, vY + BarHeight);

  FPDF.CodeI25(ABarCode, vX, vY, BarHeight, BarWidth);
end;

constructor TFPDFWrapper.Create(APDF: TFPDFExt2; AOffsetXY: boolean);
begin
  inherited Create;
  FPDF := APDF;
  if AOffsetXY then
  begin
    FOffsetX := FPDF.GetX;
    FOffsetY := FPDF.GetY;
  end
  else
  begin
    FOffsetX := 0;
    FOffsetY := 0;
  end;
end;

procedure TFPDFWrapper.DashedLine(vX1, vY1, vX2, vY2, ADashWidth: double);
begin
  ApplyOffset(vX1, vY1);
  ApplyOffset(vX2, vY2);

  FPDF.DashedLine(vX1, vY1, vX2, vY2, ADashWidth);
end;

procedure TFPDFWrapper.DashedRect(vX, vY, vWidht, vHeight: Double;
  const vStyle: String; ADashWidth: double);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + vWidht, vY + vHeight);

  FPDF.DashedRect(vX, vY, vWidht, vHeight, vStyle, ADashWidth);
end;

function TFPDFWrapper.GetCurrentFontFamily(): string;
begin
  Result := FPDF.FontFamily;
end;

function TFPDFWrapper.GetCurrentFontSize(): double;
begin
  Result := FPDF.FontSize;
end;

function TFPDFWrapper.GetCurrentFontSizePt(): double;
begin
  Result := FPDF.FontSizePt;
end;

function TFPDFWrapper.GetCurrentPage(): integer;
begin
  Result := FPDF.PageNo;
end;

function TFPDFWrapper.GetNumLines(const AText: string; AWidth,
  AIndent: double): integer;
begin
  Result := FPDF.GetNumLines(AText, AWidth, AIndent);
end;

function TFPDFWrapper.GetOrientation: TFPDFOrientation;
begin
  Result := FPDF.CurOrientation;
end;

function TFPDFWrapper.GetStringHeight(const AText: string; AWidth, ALineSpacing,
  AIndent: double): double;
begin
  Result := FPDF.GetStringHeight(AText, AWidth, ALineSpacing, AIndent);
end;

function TFPDFWrapper.GetStringWidth(const vText: String): Double;
begin
  Result := FPDF.GetStringWidth(vText);
end;

function TFPDFWrapper.GetTextColor: string;
begin
  Result := FPDF.TextColor;
end;

procedure TFPDFWrapper.Image(x, y, w, h: double; AStream: TStream; const vAlign,
  hAlign: char; AStretched: boolean);
var
  ImageUtils: TImageUtils;
  nImgW, nImgH, xImg, yImg, logoWmm, logoHmm: double;
  logoW, logoH: word;
begin
  ApplyOffset(x, y);
  ApplyHighestXY(x + w, y + h);

  ImageUtils := TImageUtils.Create;
  try
    if (AStream.Size > 0) and ImageUtils.GetImageSize(AStream, logoW, logoH) then
    begin
      xImg := x;
      yImg := y;
      logoWmm := (logoW/72)*25.4;
      logoHmm := (logoH/72)*25.4;

      if AStretched then
      begin
        nImgW := w;
        nImgH := h;
      end
      else
      begin
        nImgW := w;
        nImgH := RoundTo(logoHmm * (nImgW/logoWmm), 0);
        if nImgH > w then
        begin
          nImgH := w;
          nImgW := RoundTo(logoWmm * (nImgH/logoHmm), 0);
        end;

        if nImgW > nImgH then
        begin
          // Vertical alignment
          if vAlign = 'C' then
          begin
            // Center
            yImg := yImg + ((h - nImgH) / 2);
          end
          else if vAlign = 'B' then
          begin
            // Bottom
            yImg := yImg + h - nImgH;
          end;
        end
        else if nImgH > nImgW then
        begin
          // Horizontal alignment
          if hAlign = 'C' then
          begin
            // Center
            xImg := xImg + ((w - nImgW) / 2);
          end
          else if hAlign = 'R' then
          begin
            // Right
            xImg := xImg + w - nImgW;
          end;
        end;
      end;

      if ImageUtils.GetImageType(AStream) = itPng then
        FPDF.Image(AStream, 'PNG', xImg, yImg, nImgW, nImgH)
      else if ImageUtils.GetImageType(AStream) = itJpg then
        FPDF.Image(AStream, 'JPG', xImg, yImg, nImgW, nImgH);
    end;
  finally
    ImageUtils.Free;
  end;
end;

procedure TFPDFWrapper.Line(vX1, vY1, vX2, vY2: Double);
begin
  ApplyOffset(vX1, vY1);
  ApplyOffset(vX2, vY2);

  FPDF.Line(vX1, vY1, vX2, vY2);
end;

{$IFDEF DelphiZXingQRCode}
function TFPDFWrapper.QRCode(vX, vY: double; const QRCodeData: String;
  DotSize: Double; AEncoding: TQRCodeEncoding): double;
begin
  ApplyOffset(vX, vY);

  Result := FPDF.QRCode(vX, vY, QRCodeData, DotSize, AEncoding);

  ApplyHighestXY(vX + Result, vY + Result);
end;

procedure TFPDFWrapper.QRCode(vX, vY, vQRCodeSize: double;
  const QRCodeData: String; AEncoding: TQRCodeEncoding);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + vQRCodeSize, vY + vQRCodeSize);

  FPDF.QRCode(vX, vY, vQRCodeSize, QRCodeData, AEncoding);
end;
{$ENDIF}

procedure TFPDFWrapper.Rect(vX, vY, vWidht, vHeight: Double;
  const vStyle: String);
begin
  ApplyOffset(vX, vY);
  ApplyHighestXY(vX + vWidht, vY + vHeight);

  FPDF.Rect(vX, vY, vWidht, vHeight, vStyle);
end;

procedure TFPDFWrapper.Rotate(NewAngle, vX, vY: Double);
begin
  ApplyOffset(vX, vY);

  FPDF.Rotate(NewAngle, vX, vY);
end;

procedure TFPDFWrapper.SetDash(ABlack, AWhite: double);
begin
  FPDF.SetDash(ABlack, AWhite);
end;

procedure TFPDFWrapper.SetDash(AWidth: double);
begin
  FPDF.SetDash(AWidth);
end;

procedure TFPDFWrapper.SetDrawColor(ValR, ValG, ValB: Integer);
begin
  FPDF.SetDrawColor(ValR, ValG, ValB);
end;

procedure TFPDFWrapper.SetDrawColor(color: TFPDFColor);
begin
  FPDF.SetDrawColor(color);
end;

procedure TFPDFWrapper.SetFillColor(color: TFPDFColor);
begin
  FPDF.SetFillColor(color);
end;

procedure TFPDFWrapper.SetFillColor(ValR, ValG, ValB: Integer);
begin
  FPDF.SetFillColor(ValR, ValG, ValB);
end;

procedure TFPDFWrapper.SetFont(ASize: Double; const AStyle: String);
begin
  FPDF.SetFont(FPDF.CurrentFontFamily, AStyle, ASize);
end;

procedure TFPDFWrapper.SetLineWidth(vWidth: Double);
begin
  FPDF.SetLineWidth(vWidth);
end;

procedure TFPDFWrapper.SetTextColorAsString(const Value: string);
begin
  FPDF.TextColor := Value;
end;

procedure TFPDFWrapper.SetTextColor(ValR, ValG, ValB: Integer);
begin
  FPDF.SetTextColor(ValR, ValG, ValB);
end;

procedure TFPDFWrapper.SetTextColor(color: TFPDFColor);
begin
  FPDF.SetTextColor(color);
end;

procedure TFPDFWrapper.SetUnderline(fUnderline: Boolean);
begin
  FPDF.SetUnderline(fUnderline);
end;

procedure TFPDFWrapper.SetFont(const AFamily, AStyle: String; ASize: Double);
begin
  FPDF.SetFont(AFamily, AStyle, ASize);
end;

procedure TFPDFWrapper.Text(vX, vY: Double; const vText: String);
begin
  ApplyOffset(vX, vY);
  ApplyHighestX(vX + FPDF.GetStringWidth(vText));

  FPDF.Text(vX, vY, vText);
end;

function TFPDFWrapper.TextBox(x, y, w, h: Double): Double;
begin
  ApplyOffset(x, y);
  Result := FPDF.TextBox(x, y, w, h, '', 'T', 'L');
  if Result > h then
    h := Result;
  ApplyHighestXY(x + w, y + h);
end;

function TFPDFWrapper.TextBox(x, y, w, h: Double; const AText: string;
  const vAlign, hAlign: char; border: Double; const link: string;
  force: boolean; hmax, vOffSetX, vOffSetY, AIndent,
  vLineSpacing: double): Double;
begin
  ApplyOffset(x, y);
  Result := FPDF.TextBox(x, y, w, h, AText, vAlign, hAlign, border > 0,
    not force, force, vLineSpacing);
  if Result > h then
    h := Result;
  ApplyHighestXY(x + w, y + h);
end;

function TFPDFWrapper.TextBox(x, y, w, h: double; const AText: string;
  const vAlign, hAlign: char; ABorder, AWordWrap, AScale: boolean;
  ALineSpacing: double): double;
begin
  ApplyOffset(x, y);
  Result := FPDF.TextBox(x, y, w, h, AText, vAlign, hAlign, ABorder, AWordWrap,
    AScale, ALineSpacing);
  if Result > h then
    h := Result;
  ApplyHighestXY(x + w, y + h);
end;

function TFPDFWrapper.WordWrap(var AText: string; AMaxWidth,
  AIndent: double): integer;
begin
  Result := FPDF.WordWrap(AText, AMaxWidth, AIndent);
end;

{ TFPDFBand }

procedure TFPDFBand.OnDraw(Args: TFPDFBandDrawArgs);
begin
end;

procedure TFPDFBand.OnInit(Args: TFPDFBandInitArgs);
begin
end;

constructor TFPDFBand.Create(ABandType: TFPDFBandType);
begin
  FBandType := ABandType;
  FName := Self.ClassName;
  FVisible := True;
end;

procedure TFPDFBand.DoDraw(Args: TFPDFBandDrawArgs);
begin
  OnDraw(Args);
end;

procedure TFPDFBand.DoInit(Args: TFPDFBandInitArgs);
begin
  OnInit(Args);
end;

{ TFPDFBandEventArgs }

constructor TFPDFBandEventArgs.Create(AEngine: TFPDFEngine; APage: TFPDFPage;
  ABand: TFPDFBand);
begin
  inherited Create(AEngine, APage);
  FBand := ABand;
end;

procedure TFPDFBandEventArgs.SetPDF(APDF: IFPDF);
begin
  FPDF := APDF;
end;

{ TFPDFExt2 }

function TFPDFExt2.GetCurrentFontFamily: string;
begin
  Result := Self.FontFamily;
end;

function TFPDFExt2.GetCurrentFontSize: double;
begin
  Result := Self.FontSize;
end;

function TFPDFExt2.GetCurrentFontSizePt: double;
begin
  Result := Self.FontSizePt;
end;

{ TImageUtils }

function TImageUtils.GetImageSize(const sFile: string; var wWidth,
  wHeight: Word): boolean;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(sFile, fmOpenRead);
  try
    Result := GetImageSize(FileStream, wWidth, wHeight);
  finally
    FileStream.Free;
  end;
end;

function TImageUtils.GetImageSize(AStream: TStream; var wWidth,
  wHeight: Word): boolean;
begin
  Result := GetPNGSize(AStream, wWidth, wHeight);
  if not Result then
    Result := GetJPGSize(AStream, wWidth, wHeight);
end;

function TImageUtils.GetImageType(AStream: TStream): TImgType;
var
  wWidth, wHeight: word;
begin
  if GetPNGSize(AStream, wWidth, wHeight) then
    Result := itPng;
  if GetJPGSize(AStream, wWidth, wHeight) then
    Result := itJpg;
  Result := itUnknown;
end;

function TImageUtils.GetImageType(const sFile: string): TImgType;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(sFile, fmOpenRead);
  try
    Result := GetImageType(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TImageUtils.GetJPGSize(AStream: TStream; var wWidth,
  wHeight: Word): boolean;
const
  ValidSignature: array[0..1] of Byte = ($FF, $D8);
  Parameterless = [$01, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7];
var
  Sig: array[0..1] of byte;
  x: integer;
  Seg: byte;
  Dummy: array[0..15] of byte;
  Len: word;
  ReadLen: LongInt;
begin
  AStream.Position := 0;

  ReadLen := AStream.read(Sig[0], SizeOf(Sig));

  for x := Low(Sig) to High(Sig) do
    if Sig[x] <> ValidSignature[x] then
      ReadLen := 0;

  if ReadLen > 0 then
  begin
    ReadLen := AStream.read(Seg, 1);
    while (Seg = $FF) and (ReadLen > 0) do
    begin
      ReadLen := AStream.read(Seg, 1);
      if Seg <> $FF then
      begin
        if (Seg = $C0) or (Seg = $C1) then
        begin
          ReadLen := AStream.read(Dummy[0], 3); { don't need these bytes }
          wHeight := ReadMWord(AStream);
          wWidth  := ReadMWord(AStream);
        end
        else
        begin
          if not (Seg in Parameterless) then
          begin
            Len := ReadMWord(AStream);
            AStream.Seek(Len - 2, 1);
            AStream.read(Seg, 1);
          end
          else
            Seg := $FF; { Fake it to keep looping. }
        end;
      end;
    end;
  end;
  Result := (wWidth > 0) and (wHeight > 0);
end;

function TImageUtils.ReadMWord(AStream: TStream): Word;
type
  TMotorolaWord = record
    case Byte of
      0: (Value: Word);
      1: (Byte1, Byte2: Byte);
  end;
var
  MW: TMotorolaWord;
begin
  { It would probably be better to just read these two bytes in normally }
  { and then do a small ASM routine to swap them.  But we aren't talking }
  { about reading entire files, so I doubt the performance gain would be }
  { worth the trouble. }
  AStream.read(MW.Byte2, SizeOf(Byte));
  AStream.read(MW.Byte1, SizeOf(Byte));
  Result := MW.Value;
end;

function TImageUtils.GetPNGSize(AStream: TStream; var wWidth,
  wHeight: Word): boolean;
const
  ValidSignature: array[0..7] of byte = (137,80,78,71,13,10,26,10);
var
  Sig: array of byte;
  x: integer;
begin
  AStream.Position := 0;
  SetLength(Sig, SizeOf(ValidSignature));

  AStream.read(Sig[0], SizeOf(ValidSignature));
  for x := Low(Sig) to High(Sig) do
    if Sig[x] <> ValidSignature[x] then
      Result := False;
  AStream.Seek(18, 0);
  wWidth := ReadMWord(AStream);
  AStream.Seek(22, 0);
  wHeight := ReadMWord(AStream);
  Result := True;
end;

function TImageUtils.GetImageSize(ABytes: array of Byte; var wWidth,
  wHeight: Word): boolean;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := GetImageSize(Stream, wWidth, wHeight);
  finally
    Stream.Free;
  end;
end;

function TImageUtils.GetImageType(ABytes: array of Byte): TImgType;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := GetImageType(Stream);
  finally
    Stream.Free;
  end;
end;

{ TFPDFBandDrawArgs }

procedure TFPDFBandDrawArgs.SetFreeSpace(const Value: double);
begin
  FFreeSpace := Value;
end;

procedure TFPDFBandDrawArgs.SetReservedSpace(const Value: double);
begin
  FReservedSpace := Value;
end;

{ TFPDFEngineEventArgs }

constructor TFPDFEngineEventArgs.Create(AEngine: TFPDFEngine);
begin
  FEngine := AEngine;
end;

function TFPDFEngineEventArgs.GetCurrentPage: integer;
begin
  Result := FEngine.CurrentPage;
end;

function TFPDFEngineEventArgs.GetFinalPass: boolean;
begin
  Result := FEngine.FinalPass;
end;

function TFPDFEngineEventArgs.GetTotalPages: integer;
begin
  Result := FEngine.TotalPages;
end;

{ TFPDFPageEventArgs }

constructor TFPDFPageEventArgs.Create(AEngine: TFPDFEngine; APage: TFPDFPage);
begin
  inherited Create(AEngine);
  FPage := APage;
  FLeftMargin := APage.LeftMargin;
  FTopMargin := APage.TopMargin;
  FRightMargin := APage.RightMargin;
  FBottomMargin := APage.BottomMargin;
end;

function TFPDFPageEventArgs.GetOrientation: TFPDFOrientation;
begin
  Result := FPage.Orientation;
end;

function TFPDFPageEventArgs.GetPageHeight: double;
begin
  Result := FPage.PageHeight;
end;

function TFPDFPageEventArgs.GetPageWidth: double;
begin
  Result := FPage.PageWidth;
end;

{ TFPDFAnonymousBand }

constructor TFPDFAnonymousBand.Create(ABandType: TFPDFBandType;
  AInitEvent: TFPDFBandInitEvent; ADrawEvent: TFPDFBandDrawEvent);
begin
  inherited Create(ABandType);
  FHeight := 0;
  FInitEvent := AInitEvent;
  FDrawEvent := ADrawEvent;
end;

constructor TFPDFAnonymousBand.Create(ABandType: TFPDFBandType; ABandHeight: double;
  ADrawEvent: TFPDFBandDrawEvent);
begin
  inherited Create(ABandType);
  FHeight := ABandHeight;
  FInitEvent := nil;
  FDrawEvent := ADrawEvent;
end;

procedure TFPDFAnonymousBand.OnDraw(Args: TFPDFBandDrawArgs);
begin
  if Assigned(FDrawEvent) then
    FDrawEvent(Args);
end;

procedure TFPDFAnonymousBand.OnInit(Args: TFPDFBandInitArgs);
begin
  Args.Band.Height := FHeight;
  if Assigned(FInitEvent) then
    FInitEvent(Args);
end;

{ TFPDFBandInitArgs }

function TFPDFBandInitArgs.GetHeight: double;
begin
  Result := Band.Height;
end;

function TFPDFBandInitArgs.GetWidth: double;
begin
  Result := Band.Width;
end;

procedure TFPDFBandInitArgs.SetHeight(const Value: double);
begin
  Band.Height := Value;
end;

procedure TFPDFBandInitArgs.SetWidth(const Value: double);
begin
  Band.Width := Value;
end;

{ TFPDFPageList }

function TFPDFPageList.Add(Value: TFPDFPage): Integer;
begin
  Result := FInternalList.Add(Value);
end;

constructor TFPDFPageList.Create;
begin
  Create(True);
end;

constructor TFPDFPageList.Create(AOwnsObjects: Boolean);
begin
  FInternalList := TObjectList.Create(AOwnsObjects);
end;

destructor TFPDFPageList.Destroy;
begin
  FInternalList.Free;
  inherited;
end;

function TFPDFPageList.GetItem(Index: Integer): TFPDFPage;
begin
  Result := TFPDFPage(FInternalList[Index]);
end;

procedure TFPDFPageList.SetItem(Index: Integer; Value: TFPDFPage);
begin
  FInternalList.Insert(Index, Value)
end;

function TFPDFPageList.Count: Integer;
begin
  Result := FInternalList.Count;
end;

{ TFPDFBandList }

constructor TFPDFBandList.Create;
begin
  Create(True);
end;

constructor TFPDFBandList.Create(AOwnsObjects: Boolean);
begin
  FInternalList := TObjectList.Create(AOwnsObjects);
end;

destructor TFPDFBandList.Destroy;
begin
  FInternalList.Free;
  inherited;
end;

function TFPDFBandList.Add(Value: TFPDFBand): Integer;
begin
  Result := FInternalList.Add(Value);
end;

procedure TFPDFBandList.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  for I := FInternalList.Count - 1 downto 0 do
  begin
    Obj := FInternalList[I];
    FInternalList.Delete(I);
    if FInternalList.OwnsObjects then
      Obj.Free;
  end;
end;

function TFPDFBandList.Contains(const Value: TFPDFBand): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

function TFPDFBandList.Count: Integer;
begin
  Result := FInternalList.Count;
end;

function TFPDFBandList.GetItem(Index: Integer): TFPDFBand;
begin
  Result := TFPDFBand(FInternalList[Index]);
end;

function TFPDFBandList.IndexOf(const Value: TFPDFBand): Integer;
begin
  Result := FInternalList.IndexOf(Value);
end;

function TFPDFBandList.Remove(const Value: TFPDFBand): Integer;
begin
  Result := FInternalList.Remove(Value);
end;

procedure TFPDFBandList.SetItem(Index: Integer; Value: TFPDFBand);
begin
  FInternalList.Insert(Index, Value)
end;

end.
