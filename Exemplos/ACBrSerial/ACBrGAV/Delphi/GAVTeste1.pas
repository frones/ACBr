{$I ACBr.inc}

unit GAVTeste1;

interface

uses
  SysUtils,
 {$IFDEF Delphi6_UP} Types, Variants, {$ELSE} Windows,{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ACBrBase, ACBrGAV, ACBrECF;

type
  TForm1 = class(TForm)
    btAbrir: TBitBtn;
    gbEstado: TGroupBox;
    lEstado: TLabel;
    cbxMonitorar: TCheckBox;
    btEstado: TBitBtn;
    BitBtn1: TBitBtn;
    Timer1: TTimer;
    ACBrGAV1: TACBrGAV;
    cbxModelo: TComboBox;
    Label1: TLabel;
    cbxPorta: TComboBox;
    Label2: TLabel;
    ACBrECF1: TACBrECF;
    SpeedButton1: TSpeedButton;
    procedure btAbrirClick(Sender: TObject);
    procedure cbxMonitorarClick(Sender: TObject);
    procedure btEstadoClick(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbxModelo.ItemIndex := 2 ;
  cbxModeloChange( Sender );
  cbxPortaChange( Sender );
end;

procedure TForm1.btAbrirClick(Sender: TObject);
begin
  btAbrir.Enabled := false ;

  try
     ACBrGAV1.AbreGaveta ;
     btEstadoClick(Sender);
  finally
     btAbrir.Enabled := True ;
  end ;
end;

procedure TForm1.cbxMonitorarClick(Sender: TObject);
begin
  Timer1.Enabled := cbxMonitorar.Checked ;
end;

procedure TForm1.btEstadoClick(Sender: TObject);
begin
  if not ACBrGAV1.Ativo then
     ACBrGAV1.Ativar ;

  if ACBrGAV1.GavetaAberta then
   begin
     lEstado.Font.Color := clGreen ;
     lEstado.Caption := 'A B E R T A' ;
   end
  else
   begin
     lEstado.Font.Color := clRed ;
     lEstado.Caption := 'F E C H A D A' ;
   end ;

end;

procedure TForm1.cbxModeloChange(Sender: TObject);
Var OldModelo : TACBrGAVModelo ;
begin
  ACBrGAV1.Desativar ;
  OldModelo := ACBrGAV1.Modelo ;

  try
     ACBrGAV1.Modelo := TACBrGAVModelo( cbxModelo.ItemIndex ) ;

     if ACBrGAV1.Modelo = gavImpressoraECF then
     begin
        ACBrGAV1.ECF := ACBrECF1 ;
        ACBrECF1.Ativar ;
     end ;
  except
     ACBrGAV1.Modelo := OldModelo ;
  end ;

  cbxModelo.ItemIndex := Integer( ACBrGAV1.Modelo ) ;
  cbxPorta.Text  := ACBrGAV1.Porta ;
end;

procedure TForm1.cbxPortaChange(Sender: TObject);
begin
  if ACBrGAV1.Modelo <> gavImpressoraECF then
  begin
    ACBrGAV1.Desativar ;
    ACBrGAV1.Porta := cbxPorta.Text ;
  end ;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  close ;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  ACBrECF1.Ativar ;
  ACBrECF1.TestarDialog ;
end;

end.
