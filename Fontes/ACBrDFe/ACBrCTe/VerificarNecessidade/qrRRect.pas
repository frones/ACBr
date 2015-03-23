unit qrRRect;

interface

uses
  Graphics, Classes, Windows, QuickRpt;

type
  TQRRoundRect = class(TQRPrintable)
  private
    FBrush: TBrush;
    FPen: TPen;
    FCurve: Integer;
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SetCurve(Value: Integer);
  protected
    procedure Paint; override;
    procedure Print(OfsX, OfsY: integer); override;
    procedure StyleChanged(sender: TObject);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadVisible(Reader: TReader); virtual;
    procedure WriteDummy(Writer: TWriter); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Height default 65;
    property Pen: TPen read FPen write Setpen;
    property Width default 65;
    property Curve: Integer read FCurve write SetCurve;
  end;

procedure Register;

implementation

constructor TQRRoundRect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 50;
  FCurve := 20;
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  FBrush.OnChange := StyleChanged;
  FPen.OnChange := StyleChanged;
end;

procedure TQRRoundRect.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Visible', ReadVisible, WriteDummy, false);              { <-- do not resource }
  inherited DefineProperties(Filer);
end;

procedure TQRRoundRect.ReadVisible(Reader: TReader);
begin
  Enabled := Reader.ReadBoolean;
end;

procedure TQRRoundRect.WriteDummy(Writer: TWriter);
begin
end;

procedure TQRRoundRect.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TQRRoundRect.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TQRRoundRect.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TQRRoundRect.SetCurve(Value: Integer);
begin
  if Value <> FCurve then
  begin
    FCurve := Value;
    Invalidate;
  end;
end;

procedure TQRRoundRect.Paint;
begin
  inherited paint;
  with Canvas do
  begin
    Pen := FPen;
    Brush := FBrush;
    RoundRect(0, 0, Width, Height, FCurve, FCurve);
  end
end;

procedure TQRRoundRect.Print(OfsX, OfsY: Integer);
begin
  if ParentReport.FinalPass and Enabled then
  begin
    QRPrinter.Canvas.Brush := Brush;
    QRPrinter.Canvas.Pen := Pen;
    with QRPrinter do
    begin
      with Canvas do
        RoundRect(XPos(OfsX + Size.Left), YPos(OfsY + Size.Top),
          XPos(OfsX + Size.Left + Size.Width), YPos(OfsY + Size.Top + Size.Height),
          Round(QRPrinter.XFactor * FCurve * 2.54),
          Round(QRPrinter.YFactor * FCurve * 2.54));
    end;
  end;
end;

destructor TQRRoundRect.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('QReport', [TQRRoundRect]);
{$IFDEF ver100}
  RegisterNonActiveX([TQRRoundRect], axrComponentOnly);
{$ENDIF}
end;

end.

