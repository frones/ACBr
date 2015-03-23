unit uPrincipal;

interface

uses
  ACBrBase, ACBrSMS, ACBrSMSClass,

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, jpeg, ExtCtrls, ACBrSMSDaruma, ComCtrls;

type
  TfrmPrincipal = class(TForm)
    GroupBox5: TGroupBox;
    btnAtivar: TButton;
    cbxPorta: TComboBox;
    cbxVelocidade: TComboBox;
    cbxModelo: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MainMenu1: TMainMenu;
    Informaes1: TMenuItem;
    menEmLinha: TMenuItem;
    menIMEI: TMenuItem;
    menOperadora: TMenuItem;
    menNivelSinal: TMenuItem;
    menModelo: TMenuItem;
    menFabricante: TMenuItem;
    menFirmware: TMenuItem;
    Mtodos1: TMenuItem;
    menMensagemEnviar: TMenuItem;
    menMensagemListar: TMenuItem;
    menTrocarBandeja: TMenuItem;
    pBotoes: TPanel;
    Image1: TImage;
    Sobre1: TMenuItem;
    menSincronismo: TMenuItem;
    menEnviarLote: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    ACBrSMS1: TACBrSMS;
    menIMSI: TMenuItem;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    procedure FormDestroy(Sender: TObject);
    procedure btnAtivarClick(Sender: TObject);
    procedure menEmLinhaClick(Sender: TObject);
    procedure menIMEIClick(Sender: TObject);
    procedure menNivelSinalClick(Sender: TObject);
    procedure menModeloClick(Sender: TObject);
    procedure menFabricanteClick(Sender: TObject);
    procedure menFirmwareClick(Sender: TObject);
    procedure Sobre1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure menOperadoraClick(Sender: TObject);
    procedure menMensagemEnviarClick(Sender: TObject);
    procedure menMensagemListarClick(Sender: TObject);
    procedure menTrocarBandejaClick(Sender: TObject);
    procedure menSincronismoClick(Sender: TObject);
    procedure menEnviarLoteClick(Sender: TObject);
    procedure menIMSIClick(Sender: TObject);
    procedure ACBrSMS1Progresso(const AAtual, ATotal: Integer);
  private
    procedure AtivarMenus(const AAtivar: Boolean);
    function PathIni: String;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  IniFiles, StrUtils, TypInfo,
  uListaMensagem, uTrocarBandeja, uEnviarMensagem, uEnvioLote;

{$R *.dfm}

function TfrmPrincipal.PathIni: String;
begin
  Result :=
    IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
end;

procedure TfrmPrincipal.ACBrSMS1Progresso(const AAtual, ATotal: Integer);
begin
  StatusBar1.Panels[0].Text := Format('%d de %d', [AAtual, ATotal]);

  ProgressBar1.Position := AAtual;
  ProgressBar1.Max      := ATotal;

  Application.ProcessMessages;
end;

procedure TfrmPrincipal.AtivarMenus(const AAtivar: Boolean);
var
  I: Integer;
begin
  for I := 0 to MainMenu1.Items.Count - 1 do
    MainMenu1.Items[I].Enabled := AAtivar;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  I: TACBrSMSModelo;
begin
  // Popular combobox dos modelos
  cbxModelo.Items.Clear ;
  for I := Low(TACBrSMSModelo) to High(TACBrSMSModelo) do
    cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrSMSModelo), integer(I) ) ) ;

  // listar portas seriais do computador
  ACBrSMS1.Device.AcharPortasSeriais(cbxPorta.Items);

  // Ler configurações
  Ini := TIniFile.Create(PathIni);
  try
    cbxModelo.ItemIndex := cbxModelo.Items.IndexOf(Ini.ReadString('CONFIG', 'Modelo', 'modNenhum'));
    cbxPorta.ItemIndex  := cbxPorta.Items.IndexOf(Ini.ReadString('CONFIG', 'Porta', 'COM1'));
    cbxVelocidade.Text  := Ini.ReadString('CONFIG', 'Velocidade', '115200');
  finally
    Ini.Free;
  end;

  // desativar todos os menus até a ativação do componente
  AtivarMenus(False);
end;

procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  ACBrSMS1.Desativar;
end;

procedure TfrmPrincipal.btnAtivarClick(Sender: TObject);
var
  Ini: TIniFile;
begin
  if not ACBrSMS1.Ativo then
  begin
    ACBrSMS1.Modelo       := TACBrSMSModelo(cbxModelo.ItemIndex);
    ACBrSMS1.Device.Porta := cbxPorta.Text;
    ACBrSMS1.Device.Baud  := StrToInt(cbxVelocidade.Text);
    ACBrSMS1.Ativar;

    btnAtivar.Caption := 'Desativar';
    AtivarMenus(True);

    // gravação das ultimas configurações válidas
    Ini := TIniFile.Create(PathIni);
    try
      Ini.WriteString('CONFIG', 'Modelo', cbxModelo.Text);
      Ini.WriteString('CONFIG', 'Porta', cbxPorta.Text);
      Ini.WriteString('CONFIG', 'Velocidade', cbxVelocidade.Text);
    finally
      Ini.Free;
    end;

    menTrocarBandeja.Visible := ACBrSMS1.BandejasSimCard > 1;
  end
  else
  begin
    ACBrSMS1.Desativar;
    btnAtivar.Caption := 'Ativar';
    AtivarMenus(False);
  end;
end;

procedure TfrmPrincipal.menEmLinhaClick(Sender: TObject);
var
  Msg: String;
begin
  Msg := IfThen(ACBrSMS1.EmLinha, 'SMS em linha', 'SMS não está em linha.');
  ShowMessage(Msg);
end;

procedure TfrmPrincipal.menEnviarLoteClick(Sender: TObject);
begin
  frmEnvioLote := TfrmEnvioLote.Create(Self);
  try
    frmEnvioLote.ShowModal;
  finally
    FreeAndNil(frmEnvioLote);
  end;
end;

procedure TfrmPrincipal.menFabricanteClick(Sender: TObject);
begin
  ShowMessage(
    'Fabricante: ' +
    sLineBreak +
    sLineBreak +
    ACBrSMS1.Fabricante
  );
end;

procedure TfrmPrincipal.menFirmwareClick(Sender: TObject);
begin
  ShowMessage(
    'Firmware: ' +
    sLineBreak +
    sLineBreak +
    ACBrSMS1.Firmware
  );
end;

procedure TfrmPrincipal.menIMEIClick(Sender: TObject);
begin
  ShowMessage(
    'IMEI: ' +
    sLineBreak +
    sLineBreak +
    ACBrSMS1.IMEI
  );
end;

procedure TfrmPrincipal.menIMSIClick(Sender: TObject);
begin
  ShowMessage(
    'IMSI: ' +
    sLineBreak +
    sLineBreak +
    ACBrSMS1.IMSI
  );
end;

procedure TfrmPrincipal.menMensagemEnviarClick(Sender: TObject);
begin
  frmEnviarMensagem := TfrmEnviarMensagem.Create(Self);
  try
    frmEnviarMensagem.ShowModal;
  finally
    FreeAndNil(frmEnviarMensagem);
  end;
end;

procedure TfrmPrincipal.menMensagemListarClick(Sender: TObject);
begin
  frmListaMensagem := TfrmListaMensagem.Create(Self);
  try
    frmListaMensagem.ShowModal;
  finally
    FreeAndNil(frmListaMensagem);
  end;
end;

procedure TfrmPrincipal.menModeloClick(Sender: TObject);
begin
  ShowMessage(
    'Modelo modem: ' +
    sLineBreak +
    sLineBreak +
    ACBrSMS1.ModeloModem
  );
end;

procedure TfrmPrincipal.menNivelSinalClick(Sender: TObject);
begin
  ShowMessage(
    'Nível de Sinal: ' +
    sLineBreak +
    sLineBreak +
    FloatToStr(ACBrSMS1.NivelSinal)
  );
end;

procedure TfrmPrincipal.menOperadoraClick(Sender: TObject);
begin
  ShowMessage(
    'Operadora: ' +
    sLineBreak +
    sLineBreak +
    ACBrSMS1.Operadora
  );
end;

procedure TfrmPrincipal.menSincronismoClick(Sender: TObject);
var
  Msg: String;
  Sinc: TACBrSMSSincronismo;
begin
  Sinc := ACBrSMS1.EstadoSincronismo;

  case Sinc of
    sinErro           : Msg := 'Erro.';
    sinSincronizado   : Msg := 'Sincronizado.';
    sinNaoSincronizado: Msg := 'Não sincronizado.';
    sinBucandoRede    : Msg := 'Buscando rede...';
  end;

  ShowMessage('Sincronismo Atual: ' + Msg);
end;

procedure TfrmPrincipal.menTrocarBandejaClick(Sender: TObject);
begin
  frmTrocarBandeja := TfrmTrocarBandeja.Create(Self);
  try
    frmTrocarBandeja.ShowModal;
  finally
    FreeAndNil(frmTrocarBandeja);
  end;
end;

procedure TfrmPrincipal.Sobre1Click(Sender: TObject);
begin
  ACBrAboutDialog;
end;

end.
