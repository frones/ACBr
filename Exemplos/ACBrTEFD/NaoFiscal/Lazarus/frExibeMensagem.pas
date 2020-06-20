unit frExibeMensagem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormExibeMensagem }

  TFormExibeMensagem = class(TForm)
    btOk: TButton;
    pMensagem: TPanel;
    tEspera: TTimer;
    procedure btOkKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pMensagemClick(Sender: TObject);
    procedure tEsperaTimer(Sender: TObject);
  private
    fTempoEspera: Integer;
    fFinalEspera: TDateTime;
    function GetMensagem: String;
    procedure SetMensagem(AValue: String);
    procedure AbortarTempoEspera;
  public
    property Mensagem: String read GetMensagem write SetMensagem;
    property TempoEspera: Integer read fTempoEspera write fTempoEspera;
  end;

implementation

uses
  dateutils,
  ACBrUtil, ACBrConsts;

{$R *.lfm}

{ TFormExibeMensagem }

procedure TFormExibeMensagem.FormShow(Sender: TObject);
begin
  if (TempoEspera > 0) then
  begin
    fFinalEspera := IncMilliSecond(Now, fTempoEspera);
    tEspera.Enabled := True;
  end
  else
  begin
    fFinalEspera := 0;
    tEspera.Enabled := False;
  end;
end;

procedure TFormExibeMensagem.pMensagemClick(Sender: TObject);
begin
  AbortarTempoEspera;
end;

procedure TFormExibeMensagem.btOkKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AbortarTempoEspera;
end;

procedure TFormExibeMensagem.FormClick(Sender: TObject);
begin
  AbortarTempoEspera;
end;

procedure TFormExibeMensagem.tEsperaTimer(Sender: TObject);
var
  SegundosRestantes: Integer;
begin
  SegundosRestantes := SecondsBetween(fFinalEspera, Now);
  if SegundosRestantes <= 0 then
    ModalResult := mrOK
  else
    btOk.Caption := Format('OK (%d)', [SegundosRestantes]);
end;

function TFormExibeMensagem.GetMensagem: String;
begin
  Result := pMensagem.Caption;
end;

procedure TFormExibeMensagem.SetMensagem(AValue: String);
var
  NumLin, AltLin: Integer;
begin
  pMensagem.Caption := AValue;

  // Se houver quebra de linhas na msg, aumente o formulário...
  NumLin := CountStr(AValue, CR);
  if (NumLin > 0) then
  begin
    AltLin := pMensagem.Canvas.GetTextHeight('H');
    Height := Height + (NumLin * AltLin);
  end;
end;

procedure TFormExibeMensagem.AbortarTempoEspera;
begin
  tEspera.Enabled := False;
  btOk.Caption := 'OK';
end;

end.

