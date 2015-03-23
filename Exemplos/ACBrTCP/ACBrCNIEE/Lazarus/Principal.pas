unit Principal ; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, db, dbf, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls, DBGrids, DbCtrls,
  ACBrCNIEE ;

type

  { TfrPrincipal }

  TfrPrincipal = class(TForm)
     ACBrCNIEE1 : TACBrCNIEE ;
     btExportar : TBitBtn ;
     btListar : TBitBtn ;
     btProxy : TBitBtn ;
     btSair : TBitBtn ;
     btAbrir : TBitBtn ;
     btDownload : TBitBtn ;
     Datasource1 : TDatasource ;
     DBGrid1 : TDBGrid ;
     DBNavigator1 : TDBNavigator ;
     edArquivo : TEdit ;
     edURLDownload : TEdit ;
     GroupBox1 : TGroupBox ;
     Label1 : TLabel ;
     Label2 : TLabel ;
     Label3 : TLabel ;
     lVersao : TLabel ;
     rgTipoExportacao : TRadioGroup ;
     tmpCadastro : TMemDataset ;
     OpenDialog1 : TOpenDialog ;
     Panel1 : TPanel ;
     SaveDialog1 : TSaveDialog ;
     sbArquivo : TSpeedButton ;
     procedure btAbrirClick(Sender : TObject) ;
     procedure btDownloadClick(Sender : TObject) ;
     procedure btExportarClick(Sender : TObject) ;
     procedure btListarClick(Sender : TObject) ;
     procedure btProxyClick(Sender : TObject) ;
     procedure btSairClick(Sender : TObject) ;
     procedure FormCreate(Sender : TObject) ;
     procedure FormShow(Sender : TObject) ;
     procedure sbArquivoClick(Sender : TObject) ;
  private
    { private declarations }
  public
    { public declarations }
  end ; 

var
  frPrincipal : TfrPrincipal ;

implementation

Uses ProxyConfig ;

{$R *.lfm}

{ TfrPrincipal }

procedure TfrPrincipal.FormCreate(Sender : TObject) ;
begin
  edArquivo.Text := ExtractFilePath(Application.ExeName) + 'Tabela_CNIEE.bin';
  ACBrCNIEE1.Arquivo := edArquivo.Text;
  ACBrCNIEE1.LerConfiguracoesProxy ;
end;

procedure TfrPrincipal.FormShow(Sender : TObject) ;
begin
  if FileExists( edArquivo.Text ) then
     btAbrir.Click;
end;

procedure TfrPrincipal.sbArquivoClick(Sender : TObject) ;
begin
   if OpenDialog1.Execute then
   begin
      edArquivo.Text := OpenDialog1.FileName;
      btAbrir.Click;
   end ;
end;

procedure TfrPrincipal.btDownloadClick(Sender : TObject) ;
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

procedure TfrPrincipal.btExportarClick(Sender : TObject) ;
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
  end;

  MessageDlg(
    Format('Tabela exportada com sucesso em "%s"'+ sLineBreak, [SaveDialog1.FileName]),
    mtInformation,
    [mbOK],
    0
  );
end;

procedure TfrPrincipal.btListarClick(Sender : TObject) ;
begin
   MessageDlg('Função ainda não Implementada',mtError,[mbOK], 0);
end;

procedure TfrPrincipal.btAbrirClick(Sender : TObject) ;
var
  I: Integer;
begin
  ACBrCNIEE1.Arquivo := edArquivo.Text;
  if ACBrCNIEE1.AbrirTabela then
  begin
    lVersao.Caption := 'Versão: '+ACBrCNIEE1.VersaoArquivo;
    tmpCadastro.DisableControls;

    tmpCadastro.Open;
    tmpCadastro.First;
    while not tmpCadastro.EOF do
       tmpCadastro.Delete;

    try
      for I := 0 to ACBrCNIEE1.Cadastros.Count - 1 do
      begin
        tmpCadastro.Append;
        tmpCadastro.FieldByName('CodMarca').AsString    := ACBrCNIEE1.Cadastros[I].CodMarca;
        tmpCadastro.FieldByName('CodModelo').AsString   := ACBrCNIEE1.Cadastros[I].CodModelo;
        tmpCadastro.FieldByName('CodVersao').AsString   := ACBrCNIEE1.Cadastros[I].CodVersao;
        tmpCadastro.FieldByName('TipoECF').AsString     := ACBrCNIEE1.Cadastros[I].TipoECF;
        tmpCadastro.FieldByName('DescMarca').AsString   := ACBrCNIEE1.Cadastros[I].DescrMarca;
        tmpCadastro.FieldByName('DescModelo').AsString  := ACBrCNIEE1.Cadastros[I].DescrModelo;
        tmpCadastro.FieldByName('Versao').AsString      := ACBrCNIEE1.Cadastros[I].Versao;
        tmpCadastro.FieldByName('LacresSL').AsInteger   := ACBrCNIEE1.Cadastros[I].QtLacresSL;
        tmpCadastro.FieldByName('LacresFab').AsInteger  := ACBrCNIEE1.Cadastros[I].QtLacresFab;
        tmpCadastro.FieldByName('TemMFD').AsString      := ACBrCNIEE1.Cadastros[I].TemMFD;
        tmpCadastro.FieldByName('LacreMFD').AsString    := ACBrCNIEE1.Cadastros[I].TemLacreMFD;
        tmpCadastro.FieldByName('AtoAprovacao').AsString:= ACBrCNIEE1.Cadastros[I].AtoAprovacao;
        tmpCadastro.FieldByName('AtoRegistro').AsString := ACBrCNIEE1.Cadastros[I].AtoRegistro;
        tmpCadastro.FieldByName('FormatoNumFabricacao').AsString := ACBrCNIEE1.Cadastros[I].FormatoNumFabricacao;
        tmpCadastro.Post;
      end;
    finally
      tmpCadastro.First;
      tmpCadastro.EnableControls;
    end;
  end;
end;

procedure TfrPrincipal.btProxyClick(Sender : TObject) ;
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

procedure TfrPrincipal.btSairClick(Sender : TObject) ;
begin
   Close;
end;

end.

