unit Sobre;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, LCLType;

type

  { TfrmSobre }

  TfrmSobre = class(TForm)
    Label3: TLabel;
    lDesenvolvedores1: TLabel;
    lVersao1: TLabel;
    Timer1: TTimer;
    lVersao: TLabel;
    lDesenvolvedores: TLabel;
    lACBr: TLabel;
    BitBtn1: TBitBtn;
    lNome: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lColaboradores: TLabel;
    bAjuda: TBitBtn;
    Image1: TImage;
    procedure Timer1Timer(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lACBrClick(Sender: TObject);
    procedure lDesenvolvedoresClick(Sender: TObject);
    procedure bAjudaClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;{%h-}
      Shift: TShiftState; X, Y: Integer);{%h-}
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;{%h-}
      Shift: TShiftState; X, Y: Integer);{%h-}
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);{%h-}
  private
    { Private declarations }
    fsFinal : Integer ;
  public
    { Public declarations }
  end;

var
  frmSobre: TfrmSobre;

implementation

{$R *.lfm}

Uses ACBrUtil, Math ;

procedure TfrmSobre.FormCreate(Sender: TObject);
 Var I : Integer ;
begin
  ClientHeight := 218 ;
  self.DoubleBuffered := True ;

  fsFinal := Height + 20 ;
  For I := 0 to ControlCount -1 do
  begin
     if Controls[I] is TLabel then
     begin
        with Controls[I] as TLabel do
        begin
           fsFinal := max(fsFinal,Top+Height + 20) ;
        end ;
     end ;
  end ;
end;

procedure TfrmSobre.Timer1Timer(Sender: TObject);
Var I : Integer ;
begin
  Update ;
  For I := 0 to ControlCount -1 do
  begin
     if Controls[I] is TLabel then
     begin
        with Controls[I] as TLabel do
        begin
           Top := Top - 1 ;

           if Top+Height < 0 then
              Top := fsFinal-Height
        end ;
     end ;
  end ;

  Application.ProcessMessages ;
end;

procedure TfrmSobre.BitBtn1Click(Sender: TObject);
begin
  Timer1.Enabled := false ;
  close ;
end;

procedure TfrmSobre.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Timer1.Enabled := false ;
  CanClose       := true ;
end;

procedure TfrmSobre.lACBrClick(Sender: TObject);
begin
  OpenURL( 'http://acbr.sf.net' );
end;

procedure TfrmSobre.lDesenvolvedoresClick(Sender: TObject);
begin
  OpenURL('www.djsystem.com.br');
end;

procedure TfrmSobre.bAjudaClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/wiki/index.php/ECF');
end;

procedure TfrmSobre.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := False ;
end;

procedure TfrmSobre.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := True ;
end;

procedure TfrmSobre.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) then
     bAjuda.Click ;
end;

end.

