{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:    L. David Baldwin  http://pbear.com           }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 17/03/2007: Primeira Versao
|*    Daniel Simoes de Almeida
******************************************************************************}

{$I ACBr.inc}

unit ACBrGIF;

interface

{$IFDEF VisualCLX}
  {$DEFINE FPC_CLX}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE FPC_CLX}
{$ENDIF}

uses
  Classes, SysUtils,
 {$IFDEF VisualCLX}
  QGraphics, QControls, QExtCtrls, QDialogs,
 {$ELSE}
  Graphics, Controls, ExtCtrls, Dialogs, 
 {$ENDIF}
  Gif3 ;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}
  TACBrGIF = class ( TGraphicControl )
  private
    fsGIF  : TGIFImage ;
    fsTimer  : TTimer ;
    fsActive : Boolean;
    fsOnStart: TNotifyEvent;
    fsOnStop : TNotifyEvent;
    fsFilename: TFilename;
    fsAutoSize: Boolean;
    fsTransparent: Boolean;
    fsRepetitions: Integer;
    fsOnLoop: TNotifyEvent;
    fsOnChangeFrame: TNotifyEvent;
    fsPaused: Boolean;

    procedure AtualizaImagem(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    function GetStartFrame: Integer;
    function GetStopFrame: Integer;
    procedure SetStartFrame(const Value: Integer);
    procedure SetStopFrame(const Value: Integer);
    function GetCurrentFrame: Integer;
    procedure SetFilename(const Value: TFilename);
    function GetIsAnimated: Boolean;
    procedure SetTransparent(const Value: Boolean);
    procedure SetCurrentFrame(const Value: Integer);
    procedure SetPaused(const Value: Boolean);

  protected
    procedure SetAutoSize(Value: Boolean); {$IFNDEF VisualCLX}override ;{$ENDIF}
    procedure Paint; override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent ); override ;
    destructor Destroy ; override ;

    property GIF : TGIFImage read fsGIF ;
    property CurrentFrame : Integer read GetCurrentFrame write SetCurrentFrame ;

    procedure Pause ;
    property Paused : Boolean read fsPaused write SetPaused ;
    procedure Resume ;
    procedure Play(const StartFrame, StopFrame, Repetitions: Integer) ;
    procedure Seek(const SkipFrames : Integer ) ;
    procedure Stop ;
    procedure Start ;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const AFileName: String);
  published
    property Align ;
    property Anchors;
    property AutoSize: Boolean read fsAutoSize write SetAutoSize default True;
    property Transparent: Boolean read fsTransparent write SetTransparent default False;
    property Filename: TFilename read fsFilename write SetFilename;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    {$IFDEF FPC_CLX}
     property OnMouseEnter;
     property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    property IsAnimated : Boolean read GetIsAnimated ;
    property OnStart : TNotifyEvent read fsOnStart write fsOnStart ;
    property OnStop  : TNotifyEvent read fsOnStop  write fsOnStop  ;
    property OnLoop  : TNotifyEvent read fsOnLoop  write fsOnLoop  ;
    property OnChangeFrame : TNotifyEvent read fsOnChangeFrame
       write fsOnChangeFrame  ;

    property Active : Boolean read fsActive write SetActive default False ;
    property StartFrame : Integer read GetStartFrame write SetStartFrame default 1 ;
    property StopFrame  : Integer read GetStopFrame  write SetStopFrame default 0 ;
    property Repetitions  : Integer read fsRepetitions write fsRepetitions default 0 ;
  end;


implementation

Uses ACBrUtil ;

{ TACBrGIF }
constructor TACBrGIF.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  fsActive     := False;
  fsPaused     := False;
  fsFilename   := '';
  fsAutoSize   := True;
  fsTransparent:= False;
  fsRepetitions:= 0;

  // Criando Definição de arquivo GIF //
  fsGIF := TGIFImage.Create ;

  // Criando Thread (Timer) para mudança de Frames //
  fsTimer := TTimer.Create( self ) ;
  fsTimer.Interval := 50 ;
  fsTimer.OnTimer  := AtualizaImagem ;
  fsTimer.Enabled  := False ;

  ControlStyle := ControlStyle - [csAcceptsControls] ;
  ControlStyle := ControlStyle + [csOpaque];
  Height:= 100 ;
  Width := 100 ;
end;


destructor TACBrGIF.Destroy;
begin
  fsTimer.Enabled := False ;
  fsTimer.Free ;

  fsGIF.Free ;

  inherited Destroy ;
end;

procedure TACBrGIF.AtualizaImagem(Sender: TObject);
Var
  OldFrame, OldIteration, STFrame : Integer ;
begin
  if not Assigned( fsGIF ) then
  begin
     fsTimer.Enabled := False ;
     Exit ;
  end ;

  if (not Assigned(Parent)) or (not Parent.Visible) then
     Exit ;

  {$IFDEF FPC}
  if Parent is TControl then
     if not TControl(Parent).IsVisible then
        Exit ;
  {$ENDIF}      

  OldFrame     := fsGIF.CurrentFrame ;
  OldIteration := fsGIF.CurrentIteration ;

  if (fsRepetitions > 0) then
  begin
     STFrame := StopFrame ;
     if STFrame = 0 then
        STFrame := fsGIF.NumFrames ;

     if (OldIteration >= fsRepetitions) and (OldFrame >= STFrame) then
     begin
        Stop ;
        Exit ;
     end ;
  end ;

  if fsGIF.ShowIt then
     fsGIF.CheckTime( self ) ;

  // Verifica se mudou de Frame //
  if Assigned(OnChangeFrame) and  (OldFrame <> fsGIF.CurrentFrame) then
     fsOnChangeFrame( self ) ;

  // Verifica se efetuou Loop //
  if Assigned(fsOnLoop) and  (OldIteration <> fsGIF.CurrentIteration) then
     fsOnLoop( self ) ;

  if not ( fsGIF.Animate and Visible )  then
     Stop ;

  fsTimer.Interval := 50;
end;


procedure TACBrGIF.SetActive(const Value: Boolean);
begin
  if Value = fsActive then exit ;

  fsActive := Value;

  if Value then
  begin
     if (fsGIF.NumFrames <= 0) then
        raise Exception.Create(ACBrStr('Arquivo GIF não informado'));

     fsGIF.CurrentFrame := StartFrame ;
  end ;

  fsGIF.Animate   := Value ;
  fsTimer.Enabled := Value ;
  fsPaused        := False ;

  if Value then
   begin
     AtualizaImagem(self);
     Invalidate ;

     if Assigned( fsOnStart ) then
        fsOnStart( self ) ;
   end
  else
     if Assigned( fsOnStop ) then
        fsOnStop( self ) ;
end;

procedure TACBrGIF.Start;
begin
  if Active then  // Se estava ligado, Desliga para re-inicar variaveis
     Stop ;

  Active := True ;
end;

procedure TACBrGIF.Stop;
begin
  Active := False ;
end;

procedure TACBrGIF.SetFilename(const Value: TFilename);
Var
  NonAnimated : Boolean ;
begin
  if (fsFilename <> '') and (Value = fsFilename) then exit ;

  Active := False ;

  if Assigned( fsGIF ) then
     FreeAndNil( fsGIF );

  fsFilename := Value;

  if fsFilename <> '' then
   begin
     try
        NonAnimated := False ;
        fsGIF := CreateAGif( fsFilename, NonAnimated) ;

        fsGIF.ShowIt := True ;
        SetAutoSize( AutoSize ) ;
     except
        if Assigned( fsGIF ) then
           FreeAndNil( fsGIF ) ;

        fsFilename := '' ;
        fsGIF := TGIFImage.Create ;
        
        MessageDlg('Erro ao carregar GIF: ' + sLineBreak + Value,mtError,[mbOK],0);
     end ;
   end
  else
     fsGIF := TGIFImage.Create ;

  Repaint ;
end;

procedure TACBrGIF.SetAutoSize(Value: Boolean);
begin
  fsAutoSize := Value;

  if Value and (fsGIF.Width > 0) and (fsGIF.Height > 0) then
  begin
     Height := fsGIF.Height ;
     Width  := fsGIF.Width ;

     Repaint ;
  end ;
end;

procedure TACBrGIF.Paint;
 Var TextHeight, TextWidth : Integer ;
begin
  if (csDesigning in ComponentState) and (Filename = '') then
   begin
    // Apaga o interior do Retangulo //
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);

    // Desenha linha pontilhada //
    Canvas.Pen.Color   := clBlack;
    Canvas.Pen.Style   := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);

    // Escrevendo o Nome do Componente
    TextHeight := Canvas.TextHeight(Name) ;
    TextWidth  := Canvas.TextWidth(Name) ;
    Canvas.TextOut( Trunc((Width  - TextWidth) /2),
                    Trunc((Height - TextHeight)/2), Name ) ;
   end
  else
     fsGIF.Draw(Canvas,0,0, Width, Height, Left, Top);
end;

procedure TACBrGIF.Resize;
begin
  inherited;
end;

procedure TACBrGIF.SetTransparent(const Value: Boolean);
begin
  if not Value then
     ControlStyle := ControlStyle + [csOpaque]
  else
     ControlStyle := ControlStyle - [csOpaque];

  fsTransparent := Value;
end;

function TACBrGIF.GetIsAnimated: Boolean;
begin
  Result := fsGIF.IsAnimated ;
end;

function TACBrGIF.GetStartFrame: Integer;
begin
  Result := fsGIF.StartFrame ;
end;

function TACBrGIF.GetStopFrame: Integer;
begin
  Result := fsGIF.StopFrame ;
end;

function TACBrGIF.GetCurrentFrame: Integer;
begin
  Result := fsGIF.CurrentFrame ;
end;

procedure TACBrGIF.SetStartFrame(const Value: Integer);
begin
  fsGIF.StartFrame := Value ;
end;

procedure TACBrGIF.SetStopFrame(const Value: Integer);
begin
  fsGIF.StopFrame := Value ;
end;

procedure TACBrGIF.Pause;
begin
  fsTimer.Enabled := False ;
  fsPaused := True ;
end;

procedure TACBrGIF.Resume;
begin
  if not fsActive then
     Active := True
  else
     fsTimer.Enabled := True ;

  fsPaused := not fsTimer.Enabled ;
end;

procedure TACBrGIF.Play(const StartFrame, StopFrame, Repetitions: Integer);
begin
  Stop;
  Self.StartFrame  := StartFrame ;
  Self.StopFrame   := StopFrame ;
  Self.Repetitions := Repetitions ;
  Start ;
end;

procedure TACBrGIF.Seek(const SkipFrames: Integer);
begin
  fsGIF.CurrentFrame := fsGIF.CurrentFrame + SkipFrames ;
end;

procedure TACBrGIF.SetCurrentFrame(const Value: Integer);
begin
  fsGIF.CurrentFrame := Value ;
  Invalidate ;
end;

procedure TACBrGIF.SetPaused(const Value: Boolean);
begin
  if Value then
     Pause
  else
     Resume ;
end;

procedure TACBrGIF.LoadFromFile(const AFileName: String);
begin
  Filename := AFileName ;
end;

procedure TACBrGIF.LoadFromStream(Stream: TStream);
 Var OldActive, NonAnimated : Boolean ;
begin
  OldActive := Active ;
  Active := False ;

  fsFilename := '' ;

  if Assigned( fsGIF ) then
     FreeAndNil( fsGIF ) ;

  try
     NonAnimated := False ;
     fsGIF := CreateAGifFromStream( NonAnimated, Stream) ;

     fsGIF.ShowIt := True ;
     SetAutoSize( AutoSize ) ;
     Active := OldActive ;
  except
     if Assigned( fsGIF ) then
        FreeAndNil( fsGIF ) ;

     fsGIF := TGIFImage.Create ;
     raise Exception.Create(ACBrStr('Erro ao carregar GIF from Stream'));
  end ;

  Repaint ;
end;

end.

