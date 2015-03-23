unit Unit2;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    MaskEdit,
  {$ELSE}
    Mask,
  {$ENDIF}
  Forms, Controls, Graphics, ComCtrls, StdCtrls  ;

type

  { TForm2 }

  TForm2 = class(TForm)
     Button1 : TButton;
     cbxRede : TComboBox;
     edNSU : TEdit;
     edValor : TEdit;
     Label1 : TLabel;
     Label2 : TLabel;
     Label3 : TLabel;
     Label4 : TLabel;
     meHora : TMaskEdit;
     Label5: TLabel;
     edData : TMaskEdit;
     procedure FormCreate(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2 : TForm2; 

implementation

{$IFNDEF FPC}
 {$R *.dfm}
{$ELSE}
 {$R *.lfm}
{$ENDIF}


{ TForm2 }

procedure TForm2.FormCreate(Sender : TObject);
begin
   edData.Text := FormatDateTime('DD/MM/YYYY',now) ;
end;

end.

