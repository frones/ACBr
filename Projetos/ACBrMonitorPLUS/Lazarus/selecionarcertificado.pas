unit SelecionarCertificado;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
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

