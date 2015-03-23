unit uTrocarBandeja;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ACBrSMSClass;

type
  TfrmTrocarBandeja = class(TForm)
    rdgBandeja: TRadioGroup;
    btnTrocarChip: TButton;
    btnCancelar: TButton;
    procedure btnTrocarChipClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTrocarBandeja: TfrmTrocarBandeja;

implementation

uses
  uPrincipal{, ACBrSMSClass};

{$R *.lfm}

procedure TfrmTrocarBandeja.btnCancelarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmTrocarBandeja.btnTrocarChipClick(Sender: TObject);
begin
  if frmPrincipal.ACBrSMS1.BandejasSimCard < 2 then
    raise Exception.Create('Este modelo possui somente uma bandeja de SimCard.');

  if rdgBandeja.ItemIndex = 0 then
    frmPrincipal.ACBrSMS1.TrocarBandeja(simCard1)
  else
    frmPrincipal.ACBrSMS1.TrocarBandeja(simCard2);

  ShowMessage(
    'Pronto.' +
    sLineBreak +
    sLineBreak +
    'Última resposta:' + sLineBreak +
    String(frmPrincipal.ACBrSMS1.UltimaResposta)
  );
end;

end.
