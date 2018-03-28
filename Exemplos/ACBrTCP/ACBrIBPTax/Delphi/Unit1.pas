unit Unit1;

interface

uses
  ACBrIBPTax,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBClient, Buttons, DBCtrls, ExtCtrls, Grids, DBGrids,
  ACBrBase, ACBrSocket, ComCtrls;

type
  TForm1 = class(TForm)
    GroupBox2: TGroupBox;
    Label3: TLabel;
    edNCM: TEdit;
    btnPesquisar: TBitBtn;
    rgTipoExportacao: TRadioGroup;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    btExportar: TBitBtn;
    btSair: TBitBtn;
    btProxy: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lVersao: TLabel;
    edArquivo: TEdit;
    btAbrir: TBitBtn;
    OpenDialog1: TOpenDialog;
    tmpCadastro: TClientDataSet;
    dtsCadastro: TDataSource;
    SaveDialog1: TSaveDialog;
    tmpCadastroNCM: TStringField;
    tmpCadastroEx: TIntegerField;
    tmpCadastroTabela: TIntegerField;
    ACBrIBPTax1: TACBrIBPTax;
    ckbBuscaNCMParcial: TCheckBox;
    tmpCadastroDescricao: TStringField;
    Label4: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    tmpCadastroAliqFedNacional: TFloatField;
    tmpCadastroAliqFedImportado: TFloatField;
    tmpCadastroAliqEstadual: TFloatField;
    tmpCadastroAliqMunicipal: TFloatField;
    lbVigencia: TLabel;
    lblChave: TLabel;
    lblFonte: TLabel;
    TabSheet3: TTabSheet;
    btnAPIConsultarProduto: TButton;
    edtCNPJ: TEdit;
    edtToken: TEdit;
    Label2: TLabel;
    Label5: TLabel;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btExportarClick(Sender: TObject);
    procedure btSairClick(Sender: TObject);
    procedure btAbrirClick(Sender: TObject);
    procedure btProxyClick(Sender: TObject);
    procedure btnPesquisarClick(Sender: TObject);
    procedure ACBrIBPTax1ErroImportacao(const ALinha, AErro: string);
    procedure btnAPIConsultarProdutoClick(Sender: TObject);
  private
    StrNCM: string;
    StrUF: string;
    StrEX_TIPI: String;
    StrCodInterno: string;
    StrDescricao: string;
    StrUnidade: string;
    StrValor: string;
    StrGTIN: string;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  ProxyConfig,
  ACBrUtil;

{$R *.dfm}

procedure TForm1.ACBrIBPTax1ErroImportacao(const ALinha, AErro: string);
begin
  Memo1.Lines.Add(Alinha);
  Memo1.Lines.Add(AErro);
  Memo1.Lines.Add('');
end;

procedure TForm1.btAbrirClick(Sender: TObject);
var
  I: Integer;
begin
  Memo1.Clear;

  if OpenDialog1.Execute then
  begin
    edArquivo.Text := OpenDialog1.FileName;

    // se o path do arquivo não for passado, então o componente vai tentar baixar
    // a tabela no URL informado
    Memo1.Lines.BeginUpdate;
    try
      if ACBrIBPTax1.AbrirTabela(edArquivo.Text) then
      begin
        lVersao.Caption := 'Versão: ' + ACBrIBPTax1.VersaoArquivo;
        lbVigencia.Caption := 'Vigência: ' + Format('%s a %s', [DateToStr(ACBrIBPTax1.VigenciaInicio), DateToStr(ACBrIBPTax1.VigenciaFim)]);
        lblChave.Caption := 'Chave: ' + ACBrIBPTax1.ChaveArquivo;
        lblFonte.Caption := 'Fonte: ' + ACBrIBPTax1.Fonte;

        tmpCadastro.Close;
        tmpCadastro.CreateDataSet;
        tmpCadastro.DisableControls;
        try
          for I := 0 to ACBrIBPTax1.Itens.Count - 1 do
          begin
            tmpCadastro.Append;
            tmpCadastroNCM.AsString             := ACBrIBPTax1.Itens[I].NCM;
            tmpCadastroDescricao.AsString       := ACBrIBPTax1.Itens[I].Descricao;
            tmpCadastroEx.AsString              := ACBrIBPTax1.Itens[I].Excecao;
            tmpCadastroTabela.AsInteger         := Integer(ACBrIBPTax1.Itens[I].Tabela);
            tmpCadastroAliqFedNacional.AsFloat  := ACBrIBPTax1.Itens[I].FederalNacional;
            tmpCadastroAliqFedImportado.AsFloat := ACBrIBPTax1.Itens[I].FederalImportado;
            tmpCadastroAliqEstadual.AsFloat     := ACBrIBPTax1.Itens[I].Estadual;
            tmpCadastroAliqMunicipal.AsFloat    := ACBrIBPTax1.Itens[I].Municipal;
            tmpCadastro.Post;
          end;
        finally
          tmpCadastro.First;
          tmpCadastro.EnableControls;

          Label4.Caption := 'Quantidade de itens: ' + IntToStr(tmpCadastro.RecordCount);
        end;
      end;
    finally
      Memo1.Lines.EndUpdate;
    end;
  end;
end;

procedure TForm1.btExportarClick(Sender: TObject);
begin
  case rgTipoExportacao.ItemIndex of
    0:
      begin
        SaveDialog1.Title      := 'Exportar arquivo CSV';
        SaveDialog1.FileName   := 'IBPTax.csv';
        SaveDialog1.DefaultExt := '.csv';
        SaveDialog1.Filter     := 'Arquivos CSV|*.csv';

        if SaveDialog1.Execute then
          ACBrIBPTax1.Exportar(SaveDialog1.FileName, exCSV);
      end;

    1:
      begin
        SaveDialog1.Title      := 'Exportar arquivo DSV';
        SaveDialog1.FileName   := 'IBPTax.dsv';
        SaveDialog1.DefaultExt := '.dsv';
        SaveDialog1.Filter     := 'Arquivos DSV|*.dsv';

        if SaveDialog1.Execute then
          ACBrIBPTax1.Exportar(SaveDialog1.FileName, exDSV);
      end;

    2:
      begin
        SaveDialog1.Title      := 'Exportar arquivo XML';
        SaveDialog1.FileName   := 'IBPTax.xml';
        SaveDialog1.DefaultExt := '.xml';
        SaveDialog1.Filter     := 'Arquivos XML|*.xml';

        if SaveDialog1.Execute then
          ACBrIBPTax1.Exportar(SaveDialog1.FileName, exXML);
      end;

    3:
      begin
        SaveDialog1.Title      := 'Exportar arquivo HTML';
        SaveDialog1.FileName   := 'IBPTax.html';
        SaveDialog1.DefaultExt := '.html';
        SaveDialog1.Filter     := 'Arquivos HTML|*.html';

        if SaveDialog1.Execute then
          ACBrIBPTax1.Exportar(SaveDialog1.FileName, exHTML);
      end;

    4:
      begin
        SaveDialog1.Title      := 'Exportar arquivo TXT';
        SaveDialog1.FileName   := 'IBPTaxTexto.txt';
        SaveDialog1.DefaultExt := '.txt';
        SaveDialog1.Filter     := 'Arquivos TXT|*.txt';

        if SaveDialog1.Execute then
          ACBrIBPTax1.Exportar(SaveDialog1.FileName, exTXT);
      end;

    5:
      begin
        SaveDialog1.Title      := 'Exportar arquivo delimitado';
        SaveDialog1.FileName   := 'IBPTax.txt';
        SaveDialog1.DefaultExt := '.txt';
        SaveDialog1.Filter     := 'Arquivos TXT|*.txt';

        if SaveDialog1.Execute then
          ACBrIBPTax1.Exportar(SaveDialog1.FileName, '|', False);
      end;
  end;

  MessageDlg(
    Format('Tabela exportada com sucesso em "%s"'+ sLineBreak, [SaveDialog1.FileName]),
    mtInformation,
    [mbOK],
    0
  );
end;

procedure TForm1.btnAPIConsultarProdutoClick(Sender: TObject);
var
  Retorno: TACBrIBPTaxProdutoDTO;
begin
  ACBrIBPTax1.CNPJEmpresa := edtCNPJ.Text;
  ACBrIBPTax1.Token       := edtToken.Text;

  StrNCM := InputBox('NCM', 'Informe o NCM (8 dígitos):', StrNCM);
  StrUF := InputBox('UF', 'Informe a UF (Sigla):', StrUF);
  StrEX_TIPI := InputBox('Exceção', 'Informe a exceção da TIPI (0 para nenhuma)', StrEX_TIPI);
  StrCodInterno := InputBox('Código interno', 'Informe o código interno (opcional)', StrCodInterno);
  StrDescricao := InputBox('Descrição', 'Informe a descrição do item:', StrDescricao);
  StrUnidade := InputBox('Unidade de medida', 'Informe a unidade de medida', StrUnidade);
  StrValor := InputBox('Valor', 'Informe o valor', StrValor);
  StrGTIN := InputBox('GTIN', 'Informe o GTIN', 'SEM GTIN');

  Retorno := ACBrIBPTax1.API_ConsultarProduto(StrNCM
    , StrUF
    , StrToInt(StrEX_TIPI)
    , StrCodInterno
    , StrDescricao
    , StrUnidade
    , StringToFloatDef(StrValor, 0)
    , StrGTIN
    );

  Memo2.Clear;
  Memo2.Lines.Add('Codigo : ' + Retorno.Codigo);
  Memo2.Lines.Add('UF : ' + Retorno.UF);
  Memo2.Lines.Add('EX : ' + IntToStr(Retorno.EX));
  Memo2.Lines.Add('Descricao : ' + Retorno.Descricao);
  Memo2.Lines.Add('Aliq. Nacional : ' + FloatToStr(Retorno.Nacional));
  Memo2.Lines.Add('Aliq. Estadual : ' + FloatToStr(Retorno.Estadual));
  Memo2.Lines.Add('Aliq. Municipal: ' + FloatToStr(Retorno.Municipal));
  Memo2.Lines.Add('Aliq. Importado : ' + FloatToStr(Retorno.Importado));
  Memo2.Lines.Add('Início Vigência: ' + DateToStr(Retorno.VigenciaInicio));
  Memo2.Lines.Add('Fim Vigência: ' + DateToStr(Retorno.VigenciaFim));
  Memo2.Lines.Add('Versão: ' + Retorno.Versao);
  Memo2.Lines.Add('Chave: ' + Retorno.Chave);
  Memo2.Lines.Add('Fonte: ' + Retorno.Fonte);
  Memo2.Lines.Add('Valor : ' + FloatToStr(Retorno.Valor));
  Memo2.Lines.Add('Valor Tributo Nacional : ' + FloatToStr(Retorno.ValorTributoNacional));
  Memo2.Lines.Add('Valor Tributo Estadual : ' + FloatToStr(Retorno.ValorTributoEstadual));
  Memo2.Lines.Add('Valor Tributo Municipal: ' + FloatToStr(Retorno.ValorTributoMunicipal));
  Memo2.Lines.Add('Valor Tributo Importado : ' + FloatToStr(Retorno.ValorTributoImportado));
  Memo2.Lines.Add('JSON : ' + Retorno.JSON);
end;

procedure TForm1.btnPesquisarClick(Sender: TObject);
var
  ex, descricao: String;
  tabela: Integer;
  aliqFedNac, aliqFedImp, aliqEst, aliqMun: Double;
begin
  if ACBrIBPTax1.Procurar(edNCM.Text, ex, descricao, tabela, aliqFedNac, aliqFedImp, aliqEst, aliqMun, ckbBuscaNCMParcial.Checked) then
  begin
    ShowMessage(
      'Código: '    + edNCM.Text  + sLineBreak +
      'Exceção: '   + ex + sLineBreak +
      'Descrição: ' + descricao + sLineBreak +
      'Tabela: '    + IntToStr(tabela) + sLineBreak +
      'Aliq Federal Nacional: '  + FloatToStr(aliqFedNac) + sLineBreak +
      'Aliq Federal Importado: '  + FloatToStr(aliqFedImp) + sLineBreak +
      'Aliq Estadual: '  + FloatToStr(aliqEst) + sLineBreak +
      'Aliq Municipal: '  + FloatToStr(aliqMun)
    );
  end
  else
    showmessage('Código não encontrado!');
end;

procedure TForm1.btProxyClick(Sender: TObject);
var
  frProxyConfig: TfrProxyConfig;
begin
  frProxyConfig := TfrProxyConfig.Create(self);
  try
    frProxyConfig.edServidor.Text := ACBrIBPTax1.ProxyHost;
    frProxyConfig.edPorta.Text    := ACBrIBPTax1.ProxyPort;
    frProxyConfig.edUser.Text     := ACBrIBPTax1.ProxyUser;
    frProxyConfig.edSenha.Text    := ACBrIBPTax1.ProxyPass;

    if frProxyConfig.ShowModal = mrOK then
    begin
      ACBrIBPTax1.ProxyHost := frProxyConfig.edServidor.Text;
      ACBrIBPTax1.ProxyPort := frProxyConfig.edPorta.Text;
      ACBrIBPTax1.ProxyUser := frProxyConfig.edUser.Text;
      ACBrIBPTax1.ProxyPass := frProxyConfig.edSenha.Text;
    end;
  finally
    frProxyConfig.Free;
  end;
end;

procedure TForm1.btSairClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

end.
