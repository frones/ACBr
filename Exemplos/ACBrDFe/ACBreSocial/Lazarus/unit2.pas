unit Unit2;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, Buttons;

type

  { TfrSelecionarCertificado }

  TfrSelecionarCertificado = class(TForm)
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
  frSelecionarCertificado: TfrSelecionarCertificado;

implementation

{$R *.lfm}

end.

