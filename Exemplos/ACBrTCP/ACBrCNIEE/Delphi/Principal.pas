unit Principal;

interface

uses
  httpsend,

  Windows, Forms, Classes, Controls, StdCtrls, ExtCtrls, Dialogs, Buttons,
  DB, Grids, DBGrids, DBCtrls, DBClient, ACBrCNIEE, ACBrBase, ACBrSocket;

type
  TfrPrincipal = class(TForm)
    btExportar: TBitBtn;
    btListar: TBitBtn;
    btProxy: TBitBtn;
    btSair: TBitBtn;
    dtsCadastro: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    GroupBox1: TGroupBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    sbArquivo: TSpeedButton;
    Label2: TLabel;
    edArquivo: TEdit;
    edURLDownload: TEdit;
    btAbrir: TBitBtn;
    btDownload: TBitBtn;
    tmpCadastro: TClientDataSet;
    tmpCadastroMarca: TStringField;
    tmpCadastroModelo: TStringField;
    tmpCadastroVersao: TStringField;
    tmpCadastroTipo: TStringField;
    tmpCadastroMarcaDescr: TStringField;
    tmpCadastroModeloDescr: TStringField;
    tmpCadastroVersaoSB: TStringField;
    tmpCadastroQtLacreSL: TIntegerField;
    tmpCadastroQTLacreFab: TIntegerField;
    tmpCadastroMFD: TStringField;
    tmpCadastroLacreMFD: TStringField;
    tmpCadastroAtoAprovacao: TStringField;
    tmpCadastroAtoRegistroMG: TStringField;
    tmpCadastroFormatoNumero: TStringField;
    ACBrCNIEE1: TACBrCNIEE;
    rgTipoExportacao: TRadioGroup;
    GroupBox2: TGroupBox;
    edMarca: TEdit;
    Label3: TLabel;
    edModelo: TEdit;
    Label4: TLabel;
    btnPesquisar: TBitBtn;
    edVersaoSB: TEdit;
    Label5: TLabel;
    lVersao: TLabel;
    procedure btAbrirClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btListarClick(Sender: TObject);
    procedure btProxyClick(Sender: TObject);
    procedure btSairClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbArquivoClick(Sender: TObject);
    procedure btExportarClick(Sender: TObject);
    procedure btnPesquisarClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
    frPrincipal: TfrPrincipal;

implementation

uses
  ProxyConfig, WinInet, SysUtils, ACBrUtil;

{$R *.dfm}

{ TfrPrincipal }

procedure TfrPrincipal.FormCreate(Sender: TObject);
begin
  edArquivo.Text := ExtractFilePath(Application.ExeName) + 'Tabela_CNIEE.bin';
  ACBrCNIEE1.Arquivo := edArquivo.Text;
  ACBrCNIEE1.LerConfiguracoesProxy ;

  edMarca.Clear;
  edModelo.Clear;
  edVersaoSB.Clear;
end;

procedure TfrPrincipal.FormShow(Sender: TObject);
begin
  if FileExists(edArquivo.Text) then
    btAbrir.Click;
end;

procedure TfrPrincipal.sbArquivoClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edArquivo.Text := OpenDialog1.FileName;
    btAbrir.Click;
  end;
end;

procedure TfrPrincipal.btDownloadClick(Sender: TObject);
begin
  tmpCadastro.Close;

  ACBrCNIEE1.URLDownload := edURLDownload.Text;
  if ACBrCNIEE1.DownloadTabela then
  begin
    MessageDlg('Download da tabela efetuado com sucesso.', mtInformation, [mbOK], 0);

    if MessageDlg('Deseja abrir a tabela e mostrar os dados?', mtInformation, [mbYes,mbNo], 0) = mrYes then
      btAbrir.Click;
  end
  else
    MessageDlg('Não foi possível efetuar o download da tabela.', mtError, [mbOK], 0);
end;

procedure TfrPrincipal.btExportarClick(Sender: TObject);
begin
  case rgTipoExportacao.ItemIndex of
    0:
      begin
        SaveDialog1.Title      := 'Exportar arquivo CSV';
        SaveDialog1.FileName   := 'TabelaCNIEE.csv';
        SaveDialog1.DefaultExt := '.csv';
        SaveDialog1.Filter     := 'Arquivos CSV|*.csv';

        if SaveDialog1.Execute then
          ACBrCNIEE1.Exportar(SaveDialog1.FileName, exCSV);
      end;

    1:
      begin
        SaveDialog1.Title      := 'Exportar arquivo DSV';
        SaveDialog1.FileName   := 'TabelaCNIEE.dsv';
        SaveDialog1.DefaultExt := '.dsv';
        SaveDialog1.Filter     := 'Arquivos DSV|*.dsv';

        if SaveDialog1.Execute then
          ACBrCNIEE1.Exportar(SaveDialog1.FileName, exDSV);
      end;

    2:
      begin
        SaveDialog1.Title      := 'Exportar arquivo XML';
        SaveDialog1.FileName   := 'TabelaCNIEE.xml';
        SaveDialog1.DefaultExt := '.xml';
        SaveDialog1.Filter     := 'Arquivos XML|*.xml';

        if SaveDialog1.Execute then
          ACBrCNIEE1.Exportar(SaveDialog1.FileName, exXML);
      end;

    3:
      begin
        SaveDialog1.Title      := 'Exportar arquivo HTML';
        SaveDialog1.FileName   := 'TabelaCNIEE.html';
        SaveDialog1.DefaultExt := '.html';
        SaveDialog1.Filter     := 'Arquivos HTML|*.html';

        if SaveDialog1.Execute then
          ACBrCNIEE1.Exportar(SaveDialog1.FileName, exHTML);
      end;

    4:
      begin
        SaveDialog1.Title      := 'Exportar arquivo TXT';
        SaveDialog1.FileName   := 'TabelaCNIEE.txt';
        SaveDialog1.DefaultExt := '.txt';
        SaveDialog1.Filter     := 'Arquivos TXT|*.txt';

        if SaveDialog1.Execute then
          ACBrCNIEE1.Exportar(SaveDialog1.FileName, exTXT);
      end;

    5:
      begin
        SaveDialog1.Title      := 'Exportar arquivo delimitado';
        SaveDialog1.FileName   := 'TabelaCNIEE.txt';
        SaveDialog1.DefaultExt := '.txt';
        SaveDialog1.Filter     := 'Arquivos TXT|*.txt';

        if SaveDialog1.Execute then
          ACBrCNIEE1.Exportar(SaveDialog1.FileName, '|');
      end;
  end;

  MessageDlg(
    Format('Tabela exportada com sucesso em "%s"'+ sLineBreak, [SaveDialog1.FileName]),
    mtInformation,
    [mbOK],
    0
  );
end;

procedure TfrPrincipal.btAbrirClick(Sender: TObject);
var
  I: Integer;
begin
  ACBrCNIEE1.Arquivo := edArquivo.Text;
  if ACBrCNIEE1.AbrirTabela then
  begin
    lVersao.Caption := 'Versão: '+ACBrCNIEE1.VersaoArquivo;

    tmpCadastro.Close;
    tmpCadastro.CreateDataSet;
    tmpCadastro.DisableControls;
    try
      for I := 0 to ACBrCNIEE1.Cadastros.Count - 1 do
      begin
        tmpCadastro.Append;
        tmpCadastroMarca.AsString         := ACBrCNIEE1.Cadastros[I].CodMarca;
        tmpCadastroModelo.AsString        := ACBrCNIEE1.Cadastros[I].CodModelo;
        tmpCadastroVersao.AsString        := ACBrCNIEE1.Cadastros[I].CodVersao;
        tmpCadastroTipo.AsString          := ACBrCNIEE1.Cadastros[I].TipoECF;
        tmpCadastroMarcaDescr.AsString    := ACBrCNIEE1.Cadastros[I].DescrMarca;
        tmpCadastroModeloDescr.AsString   := ACBrCNIEE1.Cadastros[I].DescrModelo;
        tmpCadastroVersaoSB.AsString      := ACBrCNIEE1.Cadastros[I].Versao;
        tmpCadastroQtLacreSL.AsInteger    := ACBrCNIEE1.Cadastros[I].QtLacresSL;
        tmpCadastroQtLacreFab.AsInteger   := ACBrCNIEE1.Cadastros[I].QtLacresFab;
        tmpCadastroMFD.AsString           := ACBrCNIEE1.Cadastros[I].TemMFD;
        tmpCadastroLacreMFD.AsString      := ACBrCNIEE1.Cadastros[I].TemLacreMFD;
        tmpCadastroAtoAprovacao.AsString  := ACBrCNIEE1.Cadastros[I].AtoAprovacao;
        tmpCadastroAtoRegistroMG.AsString := ACBrCNIEE1.Cadastros[I].AtoRegistro;
        tmpCadastroFormatoNumero.AsString := ACBrCNIEE1.Cadastros[I].FormatoNumFabricacao;
        tmpCadastro.Post;
      end;
    finally
      tmpCadastro.First;
      tmpCadastro.EnableControls;
    end;
  end;
end;

procedure TfrPrincipal.btProxyClick(Sender: TObject);
Var
  frProxyConfig: TfrProxyConfig;
begin
  frProxyConfig := TfrProxyConfig.Create(self);
  try
    frProxyConfig.edServidor.Text := ACBrCNIEE1.ProxyHost;
    frProxyConfig.edPorta.Text    := ACBrCNIEE1.ProxyPort;
    frProxyConfig.edUser.Text     := ACBrCNIEE1.ProxyUser;
    frProxyConfig.edSenha.Text    := ACBrCNIEE1.ProxyPass;

    if frProxyConfig.ShowModal = mrOK then
    begin
      ACBrCNIEE1.ProxyHost := frProxyConfig.edServidor.Text;
      ACBrCNIEE1.ProxyPort := frProxyConfig.edPorta.Text;
      ACBrCNIEE1.ProxyUser := frProxyConfig.edUser.Text;
      ACBrCNIEE1.ProxyPass := frProxyConfig.edSenha.Text;
    end;
  finally
    frProxyConfig.Free;
  end;
end;

procedure TfrPrincipal.btListarClick(Sender: TObject);
begin
  MessageDlg('Função ainda não Implementada', mtError, [mbOK], 0);
end;

procedure TfrPrincipal.btnPesquisarClick(Sender: TObject);
var
  ECF: TACBrCNIEERegistro;
begin
  ECF := ACBrCNIEE1.BuscarECF(edMarca.Text, edModelo.Text, edVersaoSB.Text);

  if ECF <> nil then
    ShowMessage(Format('Código do ECF: %s.%s.%s', [ECF.CodMarca, ECF.CodModelo, ECF.CodVersao]))
  else
    ShowMessage('Nenhum ECF encontrado para a marca e modelo informados.');
end;

procedure TfrPrincipal.btSairClick(Sender: TObject);
begin
  Close;
end;

end.
