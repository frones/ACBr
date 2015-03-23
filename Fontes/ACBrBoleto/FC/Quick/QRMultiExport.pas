{-------------------------------------------------------------------------------+
|                                                                               |
|  TQRMultiExport - The QuickReport export component                            |
|                                                                               |
|  Description         Export your QuickReport outputs to image files or PDF    |
|                                                                               |
|  Author              Toci                                                     |
|  MailTo              toth.akos@interware.hu                                   |
|                                                                               |
|  Version             1.0                                                      |
|  Date                25/01/2008                                               |
|  Target              Delphi7, you may try other                               |
|                                                                               |
|  Supported formats   PDF, JPEG, GIF, BMP, WMF, EMF, (TIFF)                    |
|                                                                               |
|  License             Free under the terms of the GPL                          |
|                      (General Public License)                                 |
|                                                                               |
+-------------------------------------------------------------------------------}

unit QRMultiExport;
{.$DEFINE USEGIFIMAGE}  // Working correctly
{$DEFINE USEPDFDOC}    // Nishita's PDF Creation VCL (TNPDF) - it's a MOD!!
{.$DEFINE USETIFFIMAGE} // The component has some error

interface
uses WinProcs, Windows, Messages, SysUtils, Classes, Controls, Graphics, JPEG,
  Dialogs, StdCtrls, ExtCtrls, QuickRpt, QRPrntr, Registry, Printers
{$IFDEF USEPDFDOC}    , tnpdf    {$ENDIF}
{$IFDEF USEGIFIMAGE}  , GIFImage {$ENDIF}
{$IFDEF USETIFFIMAGE} , TIFFBMP  {$ENDIF} ;

type

  TQRExportFormat = ({$IFDEF USEPDFDOC}qrxPDF{$ENDIF} {$IFDEF USEGIFIMAGE}, qrxGIF{$ENDIF},
                      qrxJPEG, qrxBMP, qrxEMF, qrxWMF{$IFDEF USETIFFIMAGE}, qrxTIFF{$ENDIF});

  TNotifyPageEvent = procedure(Sender: TObject; Page : Word; FileName: String) of object;
  TNotifyPrepEvent = procedure(Sender: TObject; Pages : Word) of object;

  TAccessGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Paint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas write FCanvas;
  end;


  TQRMultiExport = class(TComponent)
  private
    { Private declarations }
    FReport       : TQuickRep;
    FWidth        : Integer;
    FHeight       : Integer;
    FCompression  : Integer;
    FExportFormat : TQRExportFormat;
    FPixelFormat  : TPixelFormat;
    FShowDialog   : Boolean;
    FFileName     : String;
    FDialogTitle  : String;
    Fdpi          : Real;


    FOnBeginExport  : TNotifyEvent;
    FOnFinishExport : TNotifyEvent;
    FOnPage         : TNotifyPageEvent;
    FOnPrepare      : TNotifyPrepEvent;

    FMetaFile       : TMetafile;

    procedure SetCompression(const Value : Integer);
    procedure SetDPI(const Value : Real);

  protected

  public
    constructor Create(AOwner : TComponent); override ;

    procedure DoExport;

  published
    property Report       : TQuickRep read FReport write FReport;
    property MinWidth     : Integer read FWidth write FWidth;
    property MinHeight    : Integer read FHeight write FHeight;
    property Compression  : Integer read FCompression write SetCompression;
    property PixelFormat  : TPixelFormat read FPixelFormat write FPixelFormat;
    property ExportFormat : TQRExportFormat read FExportFormat write FExportFormat;
    property DPI          : Real read Fdpi write SetDPI;
    property FileName     : String read FFileName write FFileName;
    property ShowDialog   : Boolean read FShowDialog write FShowDialog;
    property DialogTitle  : String read FDialogTitle write FDialogTitle;

    property OnBeginExport  : TNotifyEvent read FOnBeginExport write FOnBeginExport;
    property OnFinishExport : TNotifyEvent read FOnFinishExport write FOnFinishExport;
    property OnPrepare      : TNotifyPrepEvent read FOnPrepare write FOnPrepare;
    property OnPage         : TNotifyPageEvent read FOnPage write FOnPage;

  end;

const
  ExFilPDF  = {$IFDEF USEPDFDOC}1{$ELSE}0{$ENDIF};
  ExFilTIFF = {$IFDEF USETIFFIMAGE}1{$ELSE}0{$ENDIF};
  ExFilGIF  = {$IFDEF USEGIFIMAGE}1{$ELSE}0{$ENDIF};

  ExFilCtr = 3+ExFilPDF+ExFilTIFF+ExFilGIF;


  DefaultExportExt : array[0..ExFilCtr] of String[5] = ({$IFDEF USEPDFDOC}'.PDF',{$ENDIF}{$IFDEF USEGIFIMAGE}'.GIF',{$ENDIF}'.JPG','.BMP','.EMF','.WMF'{$IFDEF USETIFFIMAGE},'.TIF'{$ENDIF});
  DefaultExportName : array[0..ExFilCtr] of String = ({$IFDEF USEPDFDOC}'Adobe Acrobat Document',{$ENDIF}{$IFDEF USEGIFIMAGE}'GIF Image',{$ENDIF}'JPEG image','Bitmap','EMF Image','Windows Meta File'{$IFDEF USETIFFIMAGE},'TIFF image'{$ENDIF});



Procedure Register;

{$R QRMultiExport.dcr}

implementation
uses Math;

constructor TAccessGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TAccessGraphicControl.Destroy;
begin
  if GetCaptureControl = Self then SetCaptureControl(nil);
  FCanvas.Free;
  inherited Destroy;
end;

procedure TAccessGraphicControl.WMPaint(var Message: TWMPaint);
begin
  if Message.DC <> 0 then
  begin
    Canvas.Lock;
    try
      Canvas.Handle := Message.DC;
      try
        Paint;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

procedure TAccessGraphicControl.Paint;
begin //
end;

{--- TQRMultiExport ------------------------------------------------------------}
constructor TQRMultiExport.Create(AOwner : TComponent);
begin //
  inherited Create(AOwner);
  FPixelFormat:=pf24bit;
  FExportFormat:=qrxJPEG;
  Fdpi:=150.0;
  FFileName:='Export'+DefaultExportExt[Ord(qrxJPEG)];
  FCompression:=50;
end;

procedure TQRMultiExport.SetDPI(const Value : Real);
begin
  Fdpi:=Value;
  if Assigned(Report) then begin
    MinWidth:=Round(Report.Page.Width/25.4*Value);
    MinHeight:=Round(Report.Page.Length/25.4*Value);
  end;
end;

procedure TQRMultiExport.SetCompression(const Value : Integer);
begin
  FCompression := Max(1,Min(Value, 100));
end;


procedure TQRMultiExport.DoExport;
var
  JPG : TJPEGImage;
  BMP : TBitMap;
{$IFDEF USEGIFIMAGE}
  GIF : TGIFImage;
{$ENDIF}
{$IFDEF USETIFFIMAGE}
  TIF : TTIFFBitmap;
{$ENDIF}
{$IFDEF USEPDFDOC}
  PDF : TPrintPDF;
{$ENDIF}
  r1,r2 : Real;

  SD : TSaveDialog;
  Reg : TRegistry;
  i,j : Integer;
  s : String;
  Page,
  Filter : String;
  Found : Boolean;
  iFileName : String;

begin //
  if FReport = nil then exit;
  FReport.Prepare;

  if Assigned(FOnPrepare) then FOnPrepare(Self, FReport.QRPrinter.PageCount);

  Reg:=TRegistry.Create(KEY_READ);
  Reg.RootKey:=HKEY_CLASSES_ROOT;
  Filter:='';
  for i:=Ord(Low(TQRExportFormat)) to Ord(High(TQRExportFormat)) do begin
    Found:=False;
    s:=DefaultExportExt[i];
    Reg.OpenKey(s, False);
    s:=Reg.ReadString('');
    Reg.CloseKey;
    if s<>'' then begin
      Reg.OpenKey(s, False);
      s:=Reg.ReadString('');
      Reg.CloseKey;
      if s<>'' then Found:=True;
    end;
    if not Found then s:=DefaultExportName[i];
    Filter:=Filter+'|'+s+'|*'+DefaultExportExt[i];
  end;
  Reg.RootKey:=HKEY_CURRENT_USER;
  Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', False);
  s:=Reg.ReadString('Personal');
  Reg.CloseKey;
  Delete(Filter,1,1);
  Reg.Free;

  if FShowDialog then begin
    SD:=TSaveDialog.Create(Self);
    SD.Title := FDialogTitle;
    SD.FileName:=FFileName;
    SD.Filter := Filter;
    SD.FilterIndex := Ord(FExportFormat)+1;
    if SD.Execute then begin
      FExportFormat:=TQRExportFormat(SD.FilterIndex-1);
      FFileName:=SD.FileName;
      SD.Free;
    end else begin
      SD.Free;
      exit;
    end;
  end;

  if Assigned(FOnBeginExport) then FOnBeginExport(Self);

  for i:=1 to FReport.QRPrinter.PageCount do begin


    iFileName:=FFileName;
    if iFileName='' then iFileName:=IncludeTrailingPathDelimiter(s)+'Export';
    s:=IncludeTrailingPathDelimiter(ExtractFilePath(iFileName));
    if s='\' then s:='';

    if {$IFDEF USEPDFDOC}(FExportFormat<>qrxPDF) and {$ENDIF}
       (FReport.QRPrinter.PageCount>1) then begin
      Page:=IntToStr(i);
      while Length(Page)<5 do Page:='0'+Page;
    end else Page:='';

    iFileName:=s+ChangeFileExt(ExtractFileName(iFileName),'')+Page+DefaultExportExt[Ord(FExportFormat)];

    if Assigned(FOnPage) then FOnPage(Self, i, iFileName);

    FMetafile := FReport.QRPrinter.GetPage(i);

    if Fdpi > 0 then begin
      MinWidth:=Round(Report.Page.Width/25.4*Fdpi);
      MinHeight:=Round(Report.Page.Length/25.4*Fdpi);
    end;

    r1:= FWidth / FMetafile.Width;
    r2:= FHeight / FMetafile.Height;

    if r1>r2 then begin
      FMetafile.Width:=Round(FMetafile.Width*r1);
      FMetafile.Height:=Round(FMetafile.Height*r1);
    end else begin
      FMetafile.Width:=Round(FMetafile.Width*r2);
      FMetafile.Height:=Round(FMetafile.Height*r2);
    end;

{$IFDEF USEPDFDOC}
    If (FExportFormat=qrxPDF) and (i=1) then begin
      PDF:=TPrintPDF.Create(Self);
      PDF.FileName := iFileName;
      PDF.Compress:=True;
      PDF.JPEGQuality:=FCompression;
      if FReport.Page.Orientation = Printers.poLandscape then begin
        PDF.PageWidth:=FHeight;
        PDF.PageHeight:=FWidth;
      end else begin
        PDF.PageWidth:=FWidth;
        PDF.PageHeight:=FHeight;
      end;
      PDF.BeginDoc;
    end;
{$ENDIF}

    case FExportFormat of
      qrxEMF: begin
        FMetaFile.Enhanced:=True;
        FMetaFile.SaveToFile(iFileName);
      end;

      qrxWMF: begin
        FMetaFile.Enhanced:=False;
        FMetaFile.SaveToFile(iFileName);
      end;

      else begin
        BMP:=TBitMap.Create;
        BMP.Width:=FMetafile.Width;
        BMP.Height:=FMetafile.Height;
        BMP.Canvas.Draw(0,0,FMetafile);

        case FExportFormat of
          qrxBMP: begin
            BMP.PixelFormat:=FPixelFormat;
            BMP.SaveToFile(iFileName);
          end;
          qrxJPEG: begin
            JPG:=TJPEGImage.Create;
            JPG.Assign(BMP);
            JPG.CompressionQuality:=FCompression;
            JPG.SaveToFile(iFileName);
            JPG.Free;
          end;
{$IFDEF USEGIFIMAGE}
          qrxGIF: begin
            GIF:=TGIFImage.Create;
            GIF.ColorReduction:=rmQuantize;
            GIF.DitherMode:=dmBurkes;
            GIF.Assign(BMP);
            GIF.SaveToFile(iFileName);
            GIF.Free;
          end;
{$ENDIF}
{$IFDEF USETIFFIMAGE}
          qrxTIFF: begin
            TIF:=TTIFFBitmap.Create;
            TIF.Assign(BMP);
            TIF.PixelFormat:=FPixelFormat;
            TIF.SaveToTifFile(iFileName, TRUE);
            TIF.Free;
          end;
{$ENDIF}
{$IFDEF USEPDFDOC}
          qrxPDF: begin
            PDF.DrawJPEG(0,0,BMP);
            if i<FReport.QRPrinter.PageCount then
              PDF.NewPage;
          end;
{$ENDIF}
        end; {- case 2 -}

        BMP.Free;

      end; {- case other formats -}
    end; {- case 1 -}

  end;

{$IFDEF USEPDFDOC}
  If FExportFormat=qrxPDF then begin
    PDF.EndDoc;
    PDF.Free;
  end;
{$ENDIF}

  if Assigned(FOnFinishExport) then FOnFinishExport(Self);


end;



{--- Register component --------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('QReport', [TQRMultiExport]);
end;


end.
