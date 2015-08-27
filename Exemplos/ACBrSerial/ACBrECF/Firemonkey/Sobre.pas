unit Sobre;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.StdCtrls;

type
  TfrmSobre = class(TForm)
    Image1: TImage;
    lColaboradores: TMemo;
    lNome: TLabel;
    lVersao: TLabel;
    lACBr: TLabel;
    Label1: TLabel;
    lDesenvolvedores: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSobre: TfrmSobre;

implementation

{$R *.fmx}

end.
