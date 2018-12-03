unit principal;

interface

uses
  IniFiles, TypInfo, pcnConversao,

  ACBrNFe, ACBrNFeDANFEClass, ACBrNFeDANFeESCPOS,

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Buttons, ACBrPosPrinter, ACBrBase, ACBrDFe,
  ACBrDFeReport, ACBrDFeDANFeReport;

type
  TfrmPrincipal = class(TForm)
    ACBrNFe: TACBrNFe;
    ACBrNFeDANFeESCPOS: TACBrNFeDANFeESCPOS;
    OpenDialog: TOpenDialog;
    cbxPorta: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbxModelo: TComboBox;
    Label3: TLabel;
    cbxVelocidade: TComboBox;
    Label4: TLabel;
    edtLinhasEntreCupom: TSpinEdit;
    edtLogomarca: TEdit;
    Label5: TLabel;
    btnProcurarLogomarca: TSpeedButton;
    btnNFCeImprimirDANFE: TButton;
    txtMemo: TMemo;
    btnImprimirRelatorio: TButton;
    chkImprimirItem1Linha: TCheckBox;
    chkDanfeResumido: TCheckBox;
    chkIgnorarTagsFormatacao: TCheckBox;
    chkImprimirDescAcresItem: TCheckBox;
    chkViaConsumidor: TCheckBox;
    btnNFCeImprimirEvento: TButton;
    edtCSCId: TEdit;
    Label6: TLabel;
    edtCSCNumero: TEdit;
    Label7: TLabel;
    chkAbrirGaveta: TCheckBox;
    ACBrPosPrinter1: TACBrPosPrinter;
    procedure FormCreate(Sender: TObject);
    procedure btnNFCeImprimirDANFEClick(Sender: TObject);
    procedure btnProcurarLogomarcaClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnImprimirRelatorioClick(Sender: TObject);
    procedure btnNFCeImprimirEventoClick(Sender: TObject);
  private
    FConfig: TIniFile;
    procedure LerConfiguracoes;
    function GetConfigPath: TFileName;
    procedure GravarConfiguracoes;
    procedure ConfigurarComponente;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.dfm}

function TfrmPrincipal.GetConfigPath: TFileName;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TfrmPrincipal.LerConfiguracoes;
begin
  FConfig := TIniFile.Create(GetConfigPath);
  try
    cbxModelo.ItemIndex              := FConfig.ReadInteger('CONFIG', 'Modelo', 0);
    cbxPorta.ItemIndex               := cbxPorta.Items.IndexOf(FConfig.ReadString('CONFIG', 'Porta', 'COM1'));
    cbxVelocidade.ItemIndex          := cbxVelocidade.Items.IndexOf(FConfig.ReadString('CONFIG', 'Baud', '9600'));
    edtLinhasEntreCupom.Value        := FConfig.ReadInteger('CONFIG', 'Linhas', 5);
    edtLogomarca.Text                := FConfig.ReadString('CONFIG', 'Logo', '');
    chkImprimirItem1Linha.Checked    := FConfig.ReadBool('CONFIG', 'ImprimirItem1Linha', False);
    chkDanfeResumido.Checked         := FConfig.ReadBool('CONFIG', 'DANFCeResumido', False);
    chkIgnorarTagsFormatacao.Checked := FConfig.ReadBool('CONFIG', 'IgnorarTagsFormatacao', False);
    chkImprimirDescAcresItem.Checked := FConfig.ReadBool('CONFIG', 'ImprimirDescAcresItem', True);
    edtCSCId.Text                    := FConfig.ReadString('CONFIG', 'CscId', '');
    edtCSCNumero.Text                := FConfig.ReadString('CONFIG', 'CscNumero', '');
  finally
    FConfig.Free;
  end;
end;

procedure TfrmPrincipal.GravarConfiguracoes;
begin
  FConfig := TIniFile.Create(GetConfigPath);
  try
    FConfig.WriteInteger('CONFIG', 'Modelo', cbxModelo.ItemIndex);
    FConfig.WriteString('CONFIG', 'Porta', cbxPorta.Text);
    FConfig.WriteString('CONFIG', 'Baud', cbxVelocidade.Text);
    FConfig.WriteInteger('CONFIG', 'Linhas', edtLinhasEntreCupom.Value);
    FConfig.WriteString('CONFIG', 'Logo', edtLogomarca.Text);
    FConfig.WriteBool('CONFIG', 'ImprimirItem1Linha', chkImprimirItem1Linha.Checked);
    FConfig.WriteBool('CONFIG', 'DANFCeResumido', chkDanfeResumido.Checked);
    FConfig.WriteBool('CONFIG', 'IgnorarTagsFormatacao', chkIgnorarTagsFormatacao.Checked);
    FConfig.WriteBool('CONFIG', 'ImprimirDescAcresItem', chkImprimirDescAcresItem.Checked);
    FConfig.WriteString('CONFIG', 'CscId', edtCSCId.Text);
    FConfig.WriteString('CONFIG', 'CscNumero', edtCSCNumero.Text);
  finally
    FConfig.Free;
  end;
end;


procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GravarConfiguracoes;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  ModeloImpressora: TACBrPosPrinterModelo;
begin
  edtLogomarca.Clear;

  // lista de impressoras suportadas
  cbxModelo.Items.Clear ;
  For ModeloImpressora := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
    cbxModelo.Items.Add(GetEnumName(TypeInfo(TACBrPosPrinterModelo), Integer(ModeloImpressora)));

  // portas COM disponíveis
  cbxPorta.Items.BeginUpdate;
  try
    cbxPorta.Items.Clear;
    ACBrPosPrinter1.Device.AcharPortasSeriais(cbxPorta.Items);
  finally
    cbxPorta.Items.EndUpdate;
  end;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.ConfigurarComponente;
begin
  ACBrNFe.Configuracoes.Geral.IdCSC := edtCSCId.Text;
  ACBrNFe.Configuracoes.Geral.CSC   := edtCSCNumero.Text;

  ACBrPosPrinter1.Modelo        := TACBrPosPrinterModelo(cbxModelo.ItemIndex);
  ACBrPosPrinter1.Device.Porta  := cbxPorta.Text;
  ACBrPosPrinter1.Device.Baud   := StrToInt(cbxVelocidade.Text);
  ACBrPosPrinter1.IgnorarTags   := chkIgnorarTagsFormatacao.Checked;
  ACBrPosPrinter1.ControlePorta := True; // True faz com que o componente abra e feche a porta conforme a necessidade automaticamente

  ACBrNFeDANFeESCPOS.ImprimeEmUmaLinha     := chkImprimirItem1Linha.Checked;
  ACBrNFeDANFeESCPOS.ImprimeDescAcrescItem := chkImprimirDescAcresItem.Checked;

end;

procedure TfrmPrincipal.btnImprimirRelatorioClick(Sender: TObject);
begin
  ConfigurarComponente;
  ACBrNFeDANFeESCPOS.ImprimirRelatorio(txtMemo.Lines);
end;

procedure TfrmPrincipal.btnProcurarLogomarcaClick(Sender: TObject);
begin
  OpenDialog.DefaultExt := '.bmp';
  OpenDialog.Filter     := 'Arquivos de imagem Bitmap|*.bmp';
  OpenDialog.Title      := 'Abrir arquivo de imagem';

  if OpenDialog.Execute then
    edtLogomarca.Text := OpenDialog.FileName;
end;

procedure TfrmPrincipal.btnNFCeImprimirDANFEClick(Sender: TObject);
begin
  OpenDialog.DefaultExt := '.xml';
  OpenDialog.Filter     := 'Arquivos XML|*.xml';
  OpenDialog.Title      := 'Abrir arquivo de NFC-e';

  if OpenDialog.Execute then
  begin
    ACBrNFe.NotasFiscais.Clear;
    ACBrNFe.NotasFiscais.LoadFromFile(OpenDialog.FileName);

    if ACBrNFe.NotasFiscais.Count <= 0 then
      raise Exception.Create('Nenhuma nota fiscal de consumidor foi selecionada!');

    if ACBrNFe.NotasFiscais[0].NFe.Ide.modelo <> 65 then
      raise Exception.Create('Nota Fiscal não é do tipo NFC-e!');


    // impressão da NFC-e
    ConfigurarComponente;
    ACBrNFeDANFeESCPOS.ViaConsumidor := chkViaConsumidor.Checked;
    ACBrNFeDANFeESCPOS.ImprimeItens  := not chkDanfeResumido.Checked;

    ACBrNFe.NotasFiscais[0].Imprimir;

    if chkAbrirGaveta.Checked then
      ACBrPosPrinter1.AbrirGaveta;
  end;
end;

procedure TfrmPrincipal.btnNFCeImprimirEventoClick(Sender: TObject);
begin
  OpenDialog.Title      := 'Selecione a NFC-e';
  OpenDialog.DefaultExt := '*.XML';
  OpenDialog.Filter     := 'Arquivos XML (*.XML)|*.XML';
  if OpenDialog.Execute then
  begin
    ACBrNFe.NotasFiscais.Clear;
    ACBrNFe.NotasFiscais.LoadFromFile(OpenDialog.FileName);
  end;

  OpenDialog.Title      := 'Selecione o Evento';
  OpenDialog.DefaultExt := '*.XML';
  OpenDialog.Filter     := 'Arquivos XML (*.XML)|*.XML';
  if OpenDialog.Execute then
  begin
    ACBrNFe.EventoNFe.Evento.Clear;
    ACBrNFe.EventoNFe.LerXML(OpenDialog.FileName) ;
  end;

  if ACBrNFe.NotasFiscais.Count <= 0 then
    raise Exception.Create('Nenhuma nota fiscal de consumidor foi selecionada!');

  if ACBrNFe.EventoNFe.Evento.Count <= 0 then
    raise Exception.Create('Nenhum evento foi selecionado!');

  if ACBrNFe.NotasFiscais[0].NFe.Ide.modelo <> 65 then
    raise Exception.Create('Nota Fiscal não é do tipo NFC-e!');


  // impressão do evento
  ConfigurarComponente;
  ACBrNFeDANFeESCPOS.ImprimirEVENTO;
end;

end.





