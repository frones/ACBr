unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls;

type

  { TForm3 }

  TForm3 = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    tempo:TDateTime;
  end;

var
  Form3: TForm3;


implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.BitBtn1Click(Sender: TObject);
begin
  if not timer1.enabled then
    tempo:=time;
  timer1.enabled:=not timer1.enabled;
  if bitbtn1.caption='Parar' then
    bitbtn1.caption:='Iniciar'
  else
    bitbtn1.caption:='Parar';
end;

procedure TForm3.FormCreate(Sender: TObject);
begin

end;

procedure TForm3.Label1Click(Sender: TObject);
begin

end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  label1.caption:=FormatDateTime('HH:MM:SS:ZZZ',Time - tempo);
end;

end.

