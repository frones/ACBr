unit Unit3;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls ;

type

  { TForm3 }

  TForm3 = class(TForm)
     Button1 : TButton;
     cbxRede : TComboBox;
     edNSU : TEdit;
     edFinalizacao : TEdit;
     edValor : TEdit;
     Label1 : TLabel;
     Label2 : TLabel;
     Label3 : TLabel;
     Label4 : TLabel;
  private
     fIsNCN : Boolean;
     procedure SetIsNCN(const AValue : Boolean);
    { private declarations }
  public
    { public declarations }
    Property IsNCN : Boolean read fIsNCN write SetIsNCN ;
  end; 

var
  Form3 : TForm3; 

implementation

{$IFNDEF FPC}
 {$R *.dfm}
{$ELSE}
 {$R *.lfm}
{$ENDIF}

{ TForm3 }

procedure TForm3.SetIsNCN(const AValue : Boolean);
begin
   if fIsNCN = AValue then exit;
   fIsNCN := AValue;

   edValor.Enabled := AValue;
   if AValue then
      Caption := 'NCN'
   else
      Caption := 'CNF' ;

   Button1.Caption := Caption;
end;

end.

