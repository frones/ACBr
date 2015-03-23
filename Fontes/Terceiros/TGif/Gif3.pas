{Version Beta1}
{*********************************************************}
{*                  LiteGif3CLX.PAS                      *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I ACBr.inc}

unit Gif3;

interface

uses
    SysUtils, Types, Classes,
    {$IFDEF VisualCLX}
      QGraphics, QControls, QForms, QDialogs, QStdCtrls
    {$ELSE}
      Graphics, Controls, Forms, Dialogs, StdCtrls
    {$ENDIF}
    , Gif1 ;

type
  TRGBColor = packed Record
    Red,
    Green,
    Blue: Byte;
  end;

  TDisposalType = (dtUndefined,   {Take no action}
                   dtDoNothing,   {Leave graphic, next frame goes on top of it}
                   dtToBackground,{restore original background for next frame}
                   dtToPrevious); {restore image as it existed before this frame}

type
  TGIFImage = class;

  TgfFrame = class
  private
    { private declarations }
    frLeft: Integer;
    frTop: Integer;
    frWidth: Integer;
    frHeight: Integer;

    frDelay: Integer;
    frDisposalMethod: TDisposalType;
    frImage: TBitmap;
    TheEnd: boolean;    {end of what gets copied}

    IsCopy: boolean;

    constructor Create;
    constructor CreateCopy(Item: TgfFrame);
  Public
    destructor Destroy; override;
  end;

  TgfFrameList = class(TList)
  private
    function GetFrame(I: integer): TgfFrame;
  public
    {note: Frames is 1 based, goes from [1..Count]}
    property Frames[I: integer]: TgfFrame read GetFrame; default;
    end;

  TGIFImage = class(TPersistent)
  private
    { Private declarations }
    FAnimated: Boolean;
    FCurrentFrame: Integer;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FNumFrames: Integer;
    FNumIterations: Integer;
    FTransparent: Boolean;
    FVisible: Boolean;

    TheEnd: boolean;   {copy to here}

    Accum: TBitmap;
    FAnimate: Boolean;
    FStretchedRect: TRect;
    WasDisposal: TDisposalType;

    Frames: TgfFrameList;

    LastTime: Int64;
    CurrentInterval: integer;

    fsStartFrame: Integer;
    fsStopFrame: Integer;
    fsCurrentIteration: Integer;

    procedure SetAnimate(AAnimate: Boolean);
    procedure SetCurrentFrame(AFrame: Integer);
    function GetBitMap: TBitmap;

    procedure NextFrame(OldFrame: Integer);
    procedure SetStartFrame(const Value: Integer);
    procedure SetStopFrame(const Value: Integer);
  public
    ShowIt: boolean;
    IsCopy: boolean;   {set if this is a copy of one in Cache}

    { Public declarations }
    constructor Create;
    constructor CreateCopy(Item: TGIFImage);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y, Wid, Ht, Left, Top: integer);
    property Bitmap: TBitmap read GetBitmap;
    property IsAnimated: Boolean read FAnimated;
    property IsTransparent: Boolean read FTransparent;
    property NumFrames: Integer read FNumFrames;
    property NumIterations: Integer read FNumIterations;
                                                         
    procedure CheckTime(AControl: TControl);

    property Width: integer read FImageWidth;
    property Height: integer read FImageHeight;
    property Animate: Boolean read FAnimate write SetAnimate;
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    property Visible: Boolean read FVisible write FVisible;

    property StartFrame : Integer read fsStartFrame write SetStartFrame ;
    property StopFrame  : Integer read fsStopFrame  write SetStopFrame  ;
    property CurrentIteration : Integer read fsCurrentIteration write fsCurrentIteration ;
  end;

function CreateAGifFromStream(var NonAnimated: boolean;
              Stream: TStream): TGifImage;
function CreateAGif(const Name: string; var NonAnimated: boolean): TGifImage;

implementation

uses
  {$IFNDEF MSWINDOWS}
    {$IFNDEF FPC}
      Libc
    {$else}
      unix
    {$endif},
  {$endif}
  {$ifdef MSWindows}
  mmSystem,
  {$endif}
  Math;

function CreateBitmap(Width, Height: integer): TBitmap;
begin
Result := TBitmap.Create;
Result.Width := Width;
Result.Height := Height;
end;

function CreateAGifFromStream(var NonAnimated: boolean;
              Stream: TStream): TGifImage;
var
  AGif: TGif;
  Frame: TgfFrame;
  I: integer;
begin
Try
  NonAnimated := True;
  AGif := TGif.Create;
  Try
    Stream.Position := 0;
    AGif.LoadFromStream(Stream);
    Result := TGifImage.Create;

    Result.FNumFrames := AGif.ImageCount;
    Result.FAnimated := Result.FNumFrames > 1;
    NonAnimated := not Result.FAnimated;
    Result.FImageWidth := AGif.Width;
    Result.FImageHeight := AGif.Height;
    Result.FNumIterations:= AGif.LoopCount;
    if Result.FNumIterations <= 0 then
      Result.FNumIterations := 0;  {loop forever}
    Result.FTransparent := AGif.Transparent;

    for I := 0 to Result.FNumFrames-1 do
      begin
      Frame := TgfFrame.Create;
      Frame.frImage := TBitmap.Create;
      Frame.frImage.Assign(AGif.Bitmap[I]);
      Frame.frDisposalMethod := TDisposalType(AGif.ImageDisposal[I]);
      Frame.frImage.Transparent := AGif.Bitmap[I].Transparent;
      if Frame.frImage.Transparent then
        Frame.frImage.TransparentColor := AGif.TransparentColor;
      Frame.frLeft := AGif.ImageLeft[I];
      Frame.frTop := AGif.ImageTop[I];
      Frame.frWidth := AGif.ImageWidth[I];
      Frame.frHeight := AGif.ImageHeight[I];
      Frame.frDelay := AGif.ImageDelay[I] * 10;  

      Result.Frames.Add(Frame);
      end;
    if Result.IsAnimated then
      Result.WasDisposal := dtToBackground;
  finally
    AGif.Free;
    end;
except
  Result := Nil;
  end;
end;

function CreateAGif(const Name: string; var NonAnimated: boolean): TGifImage;
var
  Stream: TFileStream;
begin
Result := Nil;
try
  Stream := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
  try
    Result := CreateAGifFromStream(NonAnimated, Stream);
  finally
    Stream.Free;
    end;
except
  end;
end;

{----------------TgfFrame.Create}
constructor TgfFrame.Create;
begin
inherited Create;
end;

constructor TgfFrame.CreateCopy(Item: TgfFrame);
begin
inherited Create;
System.Move(Item.frLeft, frLeft, LongWord(@TheEnd)-LongWord(@frLeft));
IsCopy := True;
end;

{----------------TgfFrame.Destroy}
destructor TgfFrame.Destroy;
begin
if not IsCopy then
  FreeAndNil(frImage);
inherited Destroy;
end;

{----------------TGIFImage.Create}
constructor TGIFImage.Create;
begin
  inherited Create;
  FVisible := True;
  FCurrentFrame := 1;
  fsStartFrame  := 1 ;
  fsStopFrame   := 0 ;

  Frames := TgfFrameList.Create;
end;

constructor TGIFImage.CreateCopy(Item: TGIFImage);
var
  I: integer;
begin
inherited Create;
FImageWidth := Item.Width;
FimageHeight := Item.Height;
System.Move(Item.FAnimated, FAnimated, LongWord(@TheEnd)-LongWord(@FAnimated));
IsCopy := True;

Frames := TgfFrameList.Create;
for I := 1 to FNumFrames do
  Frames.Add(TgfFrame.CreateCopy(Item.Frames[I]));
FCurrentFrame := 1;
fsCurrentIteration := 1;
if FAnimated then
  WasDisposal := dtToBackground;
end;

{----------------TGIFImage.Destroy}
destructor TGIFImage.Destroy;
var
  Lp: Integer;
begin
if (FNumFrames > 0) then
    for Lp := FNumFrames downto 1 do
      Frames[Lp].Free;
Frames.Free;
FreeAndNil(Accum);
inherited Destroy;
end;

{----------------TGIFImage.Draw}
procedure TGIFImage.Draw(Canvas: TCanvas; X, Y, Wid, Ht, Left, Top: integer);
var
  FRect: TRect;  {Frame image size}
  NBitmap: TBitmap;
  WMul, HMul: double;

  procedure LoadBackground;
  begin
  if not Assigned(Accum) then
    Accum := CreateBitmap(Wid, Ht);
  Accum.Canvas.CopyMode := cmSrcCopy;
  Accum.Canvas.CopyRect(Rect(0, 0, Wid, Ht), Canvas, Rect(X+Left, Y+Top, X+Left+Wid, Y+Top+Ht));
  end;

begin
FStretchedRect := Rect(X, Y, X+Wid, Y+Ht);

if (FVisible) and (FNumFrames > 0) then
  begin
  WMul := Wid/FImageWidth;   {amount of width stretch}
  HMul := Ht/FImageHeight;   {amount of height stretch}
  with Frames[FCurrentFrame] do
    begin
    NBitmap := frImage;
    FRect := Rect(frLeft, frTop, frLeft+frWidth, frTop+frHeight);
    end;

  if (FCurrentFrame = 1) or not Assigned(Accum) or (WasDisposal = dtToBackground) then
    LoadBackground;

  Canvas.CopyMode := cmSrcCopy;
  if WasDisposal <> dtToBackground then
    Canvas.Draw(X, Y, Accum);
  with FRect do
    begin
    Canvas.StretchDraw(Rect(X+Round(WMul*Left), Y+Round(HMul*Top),
            X+Round(WMul*Right), Y+Round(HMul*Bottom)), NBitmap);
    if Frames[FCurrentFrame].frDisposalmethod <> dtToBackground then
      Accum.Canvas.StretchDraw(Rect(Round(WMul*Left), Round(HMul*Top),
              Round(WMul*Right), Round(HMul*Bottom)), NBitmap);
    end;
  end;
end;

{$IFNDEF MSWINDOWS}
function timeGetTime: Int64;
var
  tv: timeval;
begin
{$ifdef FPC}
  fpgettimeofday(@tv, Nil);
{$else}
  GetTimeOfDay(tv, Nil);
{$endif}
Result := Int64(tv.tv_sec) * 1000 + Int64(tv.tv_usec) div 1000;
end;
{$endif}

{----------------TGifImage.CheckTime}
procedure TGifImage.CheckTime(AControl: TControl);
var
  ThisTime: Int64;
  FrameStop : Integer ;
begin
  if not FAnimate then Exit;

  ThisTime := timeGetTime;
  if ThisTime - LastTime < CurrentInterval then
     Exit;

  LastTime := ThisTime;

  FrameStop := fsStopFrame ;
  if FrameStop <= 0 then
     FrameStop := FNumFrames ;

  if (FCurrentFrame >= FrameStop) then
  begin
    if (FNumIterations > 0) and (CurrentIteration >= FNumIterations) then
    begin
      SetAnimate(False);
      Exit;
    end;
    
    Inc(fsCurrentIteration);
  end;

  NextFrame(FCurrentFrame);
  Inc(FCurrentFrame);
  if (FCurrentFrame > FrameStop) or (FCurrentFrame <= 0) then
    FCurrentFrame := fsStartFrame ;

  if AControl <> nil then
     AControl.Invalidate ;

  CurrentInterval := Max(Frames[FCurrentFrame].frDelay, 1);
end;

{----------------TGIFImage.SetAnimate}
procedure TGIFImage.SetAnimate(AAnimate: Boolean);
begin
  if AAnimate = FAnimate then
     Exit;

  FAnimate           := AAnimate;
  fsCurrentIteration := 1;

  if AAnimate and (FNumFrames > 1) then
  begin
    CurrentInterval := Max(Frames[FCurrentFrame].frDelay, 1);
    LastTime        := timeGetTime;
  end;
end;

{----------------TGIFImage.SetCurrentFrame}
procedure TGIFImage.SetCurrentFrame(AFrame: Integer);
begin
  if AFrame = FCurrentFrame then
     Exit;

   NextFrame(FCurrentFrame);
   if AFrame > FNumFrames then
      FCurrentFrame := fsStartFrame
   else
      if AFrame < 1 then
         FCurrentFrame := fsStopFrame
      else
         FCurrentFrame := AFrame;
         
  if FAnimated then
     WasDisposal := dtToBackground;
end;

{----------------TGIFImage.GetBitmap}
function TGIFImage.GetBitmap: TBitmap;
begin
Result := Frames[1].frImage;
end;

{----------------TGIFImage.NextFrame}
procedure TGIFImage.NextFrame(OldFrame: Integer);
begin
WasDisposal := Frames[OldFrame].frDisposalMethod;
end;

{----------------TgfFrameList.GetFrame}
function TgfFrameList.GetFrame(I: integer): TgfFrame;
begin
Assert((I <= Count) and (I >= 1   ), 'Frame index out of range');
Result := TgfFrame(Items[I-1]);
end;

procedure TGIFImage.SetStartFrame(const Value: Integer);
begin
  fsStartFrame := min( max(Value , 1), FNumFrames);
end;

procedure TGIFImage.SetStopFrame(const Value: Integer);
begin
  fsStopFrame := max( min(Value, FNumFrames), 0) ;
end;

end.

