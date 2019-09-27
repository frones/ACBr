unit UfmLiberaAcao;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfmLiberaAcao }

  TfmLiberaAcao = class(TForm)
    Button1: TButton;
    btnReinicia: TButton;
    lblMensagem: TLabel;
    tmrFechaForm: TTimer;
    tmrValida: TTimer;
    procedure btnReiniciaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrFechaFormTimer(Sender: TObject);
    procedure tmrValidaTimer(Sender: TObject);
  private
    procedure ReiniciaCaptura;
    function CarregaDigitalPadrao: string;
  public
    function PermitirAcao: Boolean;
  end;

var
  fmLiberaAcao: TfmLiberaAcao;

implementation

uses SCVBioControlID;


{$R *.lfm}

{ TfmLiberaAcao }

procedure TfmLiberaAcao.FormShow(Sender: TObject);
begin
  ReiniciaCaptura;
end;

procedure TfmLiberaAcao.tmrFechaFormTimer(Sender: TObject);
begin
  tmrFechaForm.Enabled := False;
  Self.ModalResult := mrOK;
end;

procedure TfmLiberaAcao.btnReiniciaClick(Sender: TObject);
begin
  ReiniciaCaptura;
end;

procedure TfmLiberaAcao.tmrValidaTimer(Sender: TObject);
var
   stemplate: string;
begin
  tmrValida.Enabled := False;
  try
    stemplate := RetornarDigital();
    if (stemplate <> '') then
    begin
      if (ValidarDigital(stemplate, CarregaDigitalPadrao) = True) then
      begin
        lblMensagem.Caption := 'Digital válida';
        tmrFechaForm.Enabled := True;
      end
      else
      begin
        lblMensagem.Caption := 'Digital inválida';
        btnReinicia.Enabled := True;
      end;
    end
    else
    begin
      ReiniciaCaptura;
    end;

  except
    on E: Exception do
    begin
      lblMensagem.Caption := 'Última leitura ocasionou um erro...'+sLineBreak+'Gostaria de reiniciar?';
      ShowMessage('Erro: '+E.Message);
      btnReinicia.Enabled := True;
    end;
  end;
end;

procedure TfmLiberaAcao.ReiniciaCaptura;
begin
  btnReinicia.Enabled := False;
  lblMensagem.Caption := 'Por favor, coloque sua digital...';
  tmrValida.Enabled := True;
end;

function TfmLiberaAcao.CarregaDigitalPadrao: string;
var
  FileTemplate: TStringList;
begin
  FileTemplate := TStringList.Create;
  try
    FileTemplate.LoadFromFile('Digital.txt');
    Result := FileTemplate.Text;
  finally
    FileTemplate.Free;
  end;
end;

function TfmLiberaAcao.PermitirAcao: Boolean;
begin
  Result := fmLiberaAcao.ShowModal = mrOK;
end;

end.

