unit Principal;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR



uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics,
  Data.Bind.Controls,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  Datasnap.Midas,
  ACBrSocket, ACBrCNIEE, ACBrBase;

//**   Original VCL Uses section : 


//**   httpsend,
//** 
//**   Windows, Forms, Classes, Controls, StdCtrls, ExtCtrls, Dialogs, Buttons,
//**   DB, Grids, DBGrids, DBCtrls, DBClient, ACBrCNIEE, ACBrBase, ACBrSocket;




type
  TfrPrincipal = class(TForm)
    BindingsList1: TBindingsList;
    BindScopeDB_dtsCadastro: TBindScopeDB;
    BindSourceDB_dtsCadastro: TBindSourceDB;
    LinkGridToDataSourceDBGrid1BindSourceDB_dtsCadastro: TLinkGridToDataSource;

    btExportar: TButton;
    btListar: TButton;
    btProxy: TButton;
    btSair: TButton;
    dtsCadastro: TDataSource;
    DBGrid1: TStringGrid;
    DBNavigator1: TBindNavigator;
    GroupBox1: TGroupBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    sbArquivo: TSpeedButton;
    Label2: TLabel;
    edArquivo: TEdit;
    edURLDownload: TEdit;
    btAbrir: TButton;
    btDownload: TButton;
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
    rgTipoExportacao: TPanel;
    GroupBox2: TGroupBox;
    edMarca: TEdit;
    Label3: TLabel;
    edModelo: TEdit;
    Label4: TLabel;
    btnPesquisar: TButton;
    edVersaoSB: TEdit;
    Label5: TLabel;
    lVersao: TLabel;
    rbCSV: TRadioButton;
    rbDSV: TRadioButton;
    rbXML: TRadioButton;
    rbHTML: TRadioButton;
    rbTXT: TRadioButton;
    rbDelimitado: TRadioButton;
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
  {$IfDef ANDROID}
  System.IOUtils,
  {$EndIf}
  ProxyConfig,
  ACBrUtil;

{$R *.FMX}

{ TfrPrincipal }

procedure TfrPrincipal.FormCreate(Sender: TObject);
begin
  {$Ifdef ANDROID}
  edArquivo.Text := TPath.Combine(TPath.GetHomePath, 'Tabela_CNIEE.bin');
  {$Else}
  edArquivo.Text := './Tabela_CNIEE.bin';
  {$EndIf}
  ACBrCNIEE1.Arquivo := edArquivo.Text;
  ACBrCNIEE1.LerConfiguracoesProxy ;

  edMarca.Text := '';
  edModelo.Text := '';
  edVersaoSB.Text := '';
end;

procedure TfrPrincipal.FormShow(Sender: TObject);
begin
  if FileExists(edArquivo.Text) then
    btAbrirClick(Sender);
end;

procedure TfrPrincipal.sbArquivoClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edArquivo.Text := OpenDialog1.FileName;
    btAbrirClick(Sender);
  end;
end;

procedure TfrPrincipal.btDownloadClick(Sender: TObject);
begin
  tmpCadastro.Close;

  ACBrCNIEE1.URLDownload := edURLDownload.Text;
  if ACBrCNIEE1.DownloadTabela then
  begin
    MessageDlg('Download da tabela efetuado com sucesso.', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);

    if MessageDlg('Deseja abrir a tabela e mostrar os dados?', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo], 0) = mrYes then
      btAbrirClick(Sender);
  end
  else
    MessageDlg('Não foi possível efetuar o download da tabela.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
end;

procedure TfrPrincipal.btExportarClick(Sender: TObject);
begin
  if rbCSV.IsChecked then
  begin
    SaveDialog1.Title      := 'Exportar arquivo CSV';
    SaveDialog1.FileName   := 'TabelaCNIEE.csv';
    SaveDialog1.DefaultExt := '.csv';
    SaveDialog1.Filter     := 'Arquivos CSV|*.csv';

    if SaveDialog1.Execute then
      ACBrCNIEE1.Exportar(SaveDialog1.FileName, exCSV);
  end

  else if rbDSV.IsChecked then
  begin
    SaveDialog1.Title      := 'Exportar arquivo DSV';
    SaveDialog1.FileName   := 'TabelaCNIEE.dsv';
    SaveDialog1.DefaultExt := '.dsv';
    SaveDialog1.Filter     := 'Arquivos DSV|*.dsv';

    if SaveDialog1.Execute then
      ACBrCNIEE1.Exportar(SaveDialog1.FileName, exDSV);
  end

  else if rbXML.IsChecked then
  begin
    SaveDialog1.Title      := 'Exportar arquivo XML';
    SaveDialog1.FileName   := 'TabelaCNIEE.xml';
    SaveDialog1.DefaultExt := '.xml';
    SaveDialog1.Filter     := 'Arquivos XML|*.xml';

    if SaveDialog1.Execute then
      ACBrCNIEE1.Exportar(SaveDialog1.FileName, exXML);
  end

  else if rbHTML.IsChecked then
  begin
    SaveDialog1.Title      := 'Exportar arquivo HTML';
    SaveDialog1.FileName   := 'TabelaCNIEE.html';
    SaveDialog1.DefaultExt := '.html';
    SaveDialog1.Filter     := 'Arquivos HTML|*.html';

    if SaveDialog1.Execute then
      ACBrCNIEE1.Exportar(SaveDialog1.FileName, exHTML);
  end

  else if rbTXT.IsChecked then
  begin
    SaveDialog1.Title      := 'Exportar arquivo TXT';
    SaveDialog1.FileName   := 'TabelaCNIEE.txt';
    SaveDialog1.DefaultExt := '.txt';
    SaveDialog1.Filter     := 'Arquivos TXT|*.txt';

    if SaveDialog1.Execute then
      ACBrCNIEE1.Exportar(SaveDialog1.FileName, exTXT);
  end

  else if rbDelimitado.IsChecked then
  begin
    SaveDialog1.Title      := 'Exportar arquivo delimitado';
    SaveDialog1.FileName   := 'TabelaCNIEE.txt';
    SaveDialog1.DefaultExt := '.txt';
    SaveDialog1.Filter     := 'Arquivos TXT|*.txt';

    if SaveDialog1.Execute then
      ACBrCNIEE1.Exportar(SaveDialog1.FileName, '|');
  end;

  MessageDlg(
    Format('Tabela exportada com sucesso em "%s"'+ sLineBreak, [SaveDialog1.FileName]),
    TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbOk],
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
    lVersao.Text  := 'Versão: '+ACBrCNIEE1.VersaoArquivo;

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
  MessageDlg('Função ainda não Implementada', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
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
