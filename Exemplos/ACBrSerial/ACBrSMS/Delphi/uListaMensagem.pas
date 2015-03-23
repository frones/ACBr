unit uListaMensagem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type
  TfrmListaMensagem = class(TForm)
    GroupBox4: TGroupBox;
    btnListarMensagens: TButton;
    rgdFiltroMsg: TRadioGroup;
    lstMensagens: TListView;
    rdgBandeja: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure btnListarMensagensClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmListaMensagem: TfrmListaMensagem;

implementation

uses
  uPrincipal, ACBrSMSClass;

{$R *.dfm}

procedure TfrmListaMensagem.FormCreate(Sender: TObject);
begin
  lstMensagens.Clear;

  rdgBandeja.Enabled := frmPrincipal.ACBrSMS1.BandejasSimCard > 1;
  if rdgBandeja.Enabled then
    rdgBandeja.ItemIndex := Integer(frmPrincipal.ACBrSMS1.SimCard);
end;

procedure TfrmListaMensagem.btnListarMensagensClick(Sender: TObject);
var
  Filtro: TACBrSMSFiltro;
begin
  lstMensagens.Clear;

  if frmPrincipal.ACBrSMS1.BandejasSimCard > 1 then
  begin
    if rdgBandeja.ItemIndex = 0 then
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard1)
    else
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard2);
  end;

  case rgdFiltroMsg.ItemIndex of
    1: Filtro := fltLidas;
    2: Filtro := fltNaoLidas;
  else
    Filtro := fltTudo;
  end;

  frmPrincipal.ACBrSMS1.ListarMensagens(Filtro, 'mensagens.txt');
  ShowMessage('Pronto.');
end;
end.
