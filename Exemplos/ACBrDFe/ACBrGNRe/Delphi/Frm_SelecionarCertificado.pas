unit Frm_SelecionarCertificado;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, Buttons;

type

  { TfrSelecionarCertificado }

  TfrmSelecionarCertificado = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmSelecionarCertificado: TfrmSelecionarCertificado;

implementation

{$R *.dfm}

end.

