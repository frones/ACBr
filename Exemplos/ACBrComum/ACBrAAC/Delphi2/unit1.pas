unit Unit1;

interface

uses
  ACBrPAFClass, ACBrECFClass, ACBrUtil,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACBrAAC, ACBrBase, ACBrECF, StdCtrls, ExtCtrls, jpeg, Buttons,
  ComCtrls, Spin;

type
  TForm1 = class(TForm)
    botao_pesquisa: TSpeedButton;
    Image1: TImage;
    Edit1: TEdit;
    ACBrECF: TACBrECF;
    ACBrAAC1: TACBrAAC;
    pnlBotton: TPanel;
    btnSairAplicativo: TSpeedButton;
    btnSalvarArquivo: TSpeedButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    btnAdicionarECF: TSpeedButton;
    btnExcluirECFs: TSpeedButton;
    edNumeroSerieECF: TEdit;
    edGTECF: TEdit;
    gbxECFCaptura: TGroupBox;
    btnECFCapturarDados: TSpeedButton;
    Label2: TLabel;
    Label9: TLabel;
    cbxECFPorta: TComboBox;
    cbxECFModelo: TComboBox;
    edCROECF: TSpinEdit;
    lstECFsAutorizados: TListView;
    cbxECFVelocidade: TComboBox;
    Label20: TLabel;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label15: TLabel;
    edRazaoSocial: TEdit;
    edCNPJ: TEdit;
    edInscEstadual: TEdit;
    edInscMunicipal: TEdit;
    GroupBox1: TGroupBox;
    Label14: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label5: TLabel;
    edNomePAF: TEdit;
    edVersaoPAF: TEdit;
    edMD5Paf: TEdit;
    edExecutavel: TEdit;
    GroupBox2: TGroupBox;
    edSistemaOperacional: TEdit;
    Label19: TLabel;
    edBancoDados: TEdit;
    Label18: TLabel;
    edLinguagem: TEdit;
    Label17: TLabel;
    GroupBox3: TGroupBox;
    cbxTipoFuncionamento: TComboBox;
    cbxTipoIntegracao: TComboBox;
    cbxTipoDesenvolvimento: TComboBox;
    Label16: TLabel;
    Label13: TLabel;
    Label10: TLabel;
    pgcConfigPafECF: TPageControl;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet9: TTabSheet;
    ckRealizaLancamentoMesa: TCheckBox;
    ckDAVConfAnexoII: TCheckBox;
    ckRealizaDAVOS: TCheckBox;
    ckRealizaDAVNaoFiscal: TCheckBox;
    ckRealizaDAVECF: TCheckBox;
    ckRealizaPreVenda: TCheckBox;
    ckDAVDiscrFormula: TCheckBox;
    ckUsaImpressoraNaoFiscal: TCheckBox;
    ckBarSimilarECFComum: TCheckBox;
    ckBarSimilarBalanca: TCheckBox;
    ckBarSimilarECFRestaurante: TCheckBox;
    ckIndiceTecnicoProd: TCheckBox;
    ckCriaAbastDivergEncerrante: TCheckBox;
    ckIntegradoComBombas: TCheckBox;
    ckArmazenaEncerranteIniFinal: TCheckBox;
    ckEmiteContrEncerrAposREDZLEIX: TCheckBox;
    ckAcumulaVolumeDiario: TCheckBox;
    ckImpedeVendaVlrZero: TCheckBox;
    ckCadastroPlacaBomba: TCheckBox;
    ckTransportePassageiro: TCheckBox;
    ckCupomMania: TCheckBox;
    ckEmitePED: TCheckBox;
    ckTransfDAV: TCheckBox;
    ckRecompoeGT: TCheckBox;
    ckTransfPreVenda: TCheckBox;
    ckTotalizaValoresLista: TCheckBox;
    ckMinasLegal: TCheckBox;
    edCNIEE: TEdit;
    Label21: TLabel;
    edtPerfilRequisitos: TEdit;
    lblPerfilRequisitos: TLabel;
    procedure btnSairAplicativoClick(Sender: TObject);
    procedure btnECFCapturarDadosClick(Sender: TObject);
    procedure btnAdicionarECFClick(Sender: TObject);
    procedure btnExcluirECFsClick(Sender: TObject);
    procedure btnSalvarArquivoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edGTECFKeyPress(Sender: TObject; var Key: Char);
    procedure ACBrAAC1GetChave(var Chave: AnsiString);
  private
    function GetPathArquivoAuxiliar: String;
    procedure buscaInformacoesArquivoAuxiliar;
    procedure recriarArquivoAuxiliar;
    procedure AdicionarItemGrid(const ANumeroSerie: String;
      const AValorGT: Double; const ACRO: Integer; const ACNIEE: Integer);
    function StrToNumero(const AString: String): Double;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  TypInfo;

{$R *.dfm}

procedure TForm1.ACBrAAC1GetChave(var Chave: AnsiString);
begin
  Chave := '1234';
end;

procedure TForm1.AdicionarItemGrid(const ANumeroSerie: String;
  const AValorGT: Double; const ACRO: Integer; const ACNIEE: Integer);
begin
  with lstECFsAutorizados.Items.Add do
  begin
    Caption := ANumeroSerie;
    SubItems.Add(FormatFloat(',#0.00', AValorGT));
    SubItems.Add(Format('%3.3d', [ACRO]));
    SubItems.Add(Format('%6.6d', [ACNIEE]));
  end;
end;

function TForm1.StrToNumero(const AString: String): Double;
var
  StrTmp: String;
begin
  StrTmp := StringReplace(AString, '.', '', [rfReplaceAll]);
  Result := StrToFloat(StrTmp);
end;

function TForm1.GetPathArquivoAuxiliar: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'AX.TXT';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  TipoFunc: TACBrPAFTipoFuncionamento;
  TipoDesenv: TACBrPAFTipoDesenvolvimento;
  TipoIntegr: TACBrPAFTipoIntegracao;
begin
  PageControl1.ActivePageIndex    := 0;
  pgcConfigPafECF.ActivePageIndex := 0;

  cbxTipoFuncionamento.Items.BeginUpdate;  
  try
    cbxTipoFuncionamento.Items.Clear;
    for TipoFunc := Low(TACBrPAFTipoFuncionamento) to High(TACBrPAFTipoFuncionamento) do
      cbxTipoFuncionamento.Items.Add(GetEnumName(TypeInfo(TACBrPAFTipoFuncionamento), Integer(TipoFunc)));
  finally
    cbxTipoFuncionamento.Items.EndUpdate;
    cbxTipoFuncionamento.ItemIndex := 0;
  end;

  cbxTipoDesenvolvimento.Items.BeginUpdate;  
  try
    cbxTipoDesenvolvimento.Items.Clear;
    for TipoDesenv := Low(TACBrPAFTipoDesenvolvimento) to High(TACBrPAFTipoDesenvolvimento) do
      cbxTipoDesenvolvimento.Items.Add(GetEnumName(TypeInfo(TACBrPAFTipoDesenvolvimento), Integer(TipoDesenv)));
  finally
    cbxTipoDesenvolvimento.Items.EndUpdate;
    cbxTipoDesenvolvimento.ItemIndex := 0;
  end;

  cbxTipoIntegracao.Items.BeginUpdate;  
  try
    cbxTipoIntegracao.Items.Clear;
    for TipoIntegr := Low(TACBrPAFTipoIntegracao) to High(TACBrPAFTipoIntegracao) do
      cbxTipoIntegracao.Items.Add(GetEnumName(TypeInfo(TACBrPAFTipoIntegracao), Integer(TipoIntegr)));
  finally
    cbxTipoIntegracao.Items.EndUpdate;
    cbxTipoIntegracao.ItemIndex := 0;
  end;    

  if not FileExists(GetPathArquivoAuxiliar) then
  begin
    MessageDlg('O Sistema não encontrou o arquivo auxiliar de configuração do PAF-ECF neste computador. O arquivo será recriado', mtWarning, [mbOK], 0);
    recriarArquivoAuxiliar;
  end
  else
    buscaInformacoesArquivoAuxiliar;
end;

procedure TForm1.edGTECFKeyPress(Sender: TObject; var Key: Char);
begin
  if not( Key in ['0'..'9', ',', #8] ) then
    Key := #0
end;

procedure Tform1.recriarArquivoAuxiliar;
var
  F: TextFile;
  ct: integer;
begin
  try
    if FileExists(GetPathArquivoAuxiliar) then
      DeleteFile(GetPathArquivoAuxiliar);

    AssignFile(F, GetPathArquivoAuxiliar);
    Rewrite(F);
    closefile(F);

    ACBrAAC1.NomeArquivoAux := GetPathArquivoAuxiliar;
//    ACBrAAC1.AbrirArquivo;
    ACBrAAC1.IdentPAF.Empresa.RazaoSocial    := edRazaoSocial.Text;
    ACBrAAC1.IdentPAF.Empresa.CNPJ           := edCNPJ.Text;
    ACBrAAC1.IdentPAF.Empresa.IE             := edInscEstadual.Text;
    ACBrAAC1.IdentPAF.Empresa.IM             := edInscMunicipal.Text;
    ACBrAAC1.IdentPAF.Paf.Nome               := edNomePAF.Text;
    ACBrAAC1.IdentPAF.Paf.Linguagem          := edLinguagem.Text;
    ACBrAAC1.IdentPAF.Paf.BancoDados         := edBancoDados.Text;
    ACBrAAC1.IdentPAF.Paf.SistemaOperacional := edSistemaOperacional.Text;
    ACBrAAC1.IdentPAF.Paf.Versao             := edVersaoPAF.Text;
    ACBrAAC1.IdentPAF.Paf.PrincipalExe.Nome  := edExecutavel.Text;
    ACBrAAC1.IdentPAF.Paf.PrincipalExe.MD5   := edMD5Paf.Text;
    ACBrAAC1.IdentPAF.Paf.PerfilRequisitos   := edtPerfilRequisitos.Text;

    //funcionalidades
    ACBrAAC1.IdentPAF.Paf.TipoFuncionamento   := TACBrPAFTipoFuncionamento(cbxTipoFuncionamento.ItemIndex);
    ACBrAAC1.IdentPAF.Paf.TipoDesenvolvimento := TACBrPAFTipoDesenvolvimento(cbxTipoDesenvolvimento.ItemIndex);
    ACBrAAC1.IdentPAF.Paf.IntegracaoPAFECF    := TACBrPAFTipoIntegracao(cbxTipoIntegracao.ItemIndex);

    //parametros de nao concomitancia
    ACBrAAC1.IdentPAF.Paf.RealizaPreVenda              := ckRealizaPreVenda.Checked;
    ACBrAAC1.IdentPAF.Paf.RealizaDAVECF                := ckRealizaDAVECF.Checked;
    ACBrAAC1.IdentPAF.Paf.RealizaDAVNaoFiscal          := ckRealizaDAVNaoFiscal.Checked;
    ACBrAAC1.IdentPAF.Paf.RealizaDAVOS                 := ckRealizaDAVOS.Checked;
    ACBrAAC1.IdentPAF.Paf.DAVConfAnexoII               := ckDAVConfAnexoII.Checked;
    ACBrAAC1.IdentPAF.Paf.RealizaLancamentoMesa        := ckRealizaLancamentoMesa.Checked;

   // aplicações especiais
    ACBrAAC1.IdentPAF.Paf.IndiceTecnicoProd            := ckIndiceTecnicoProd.Checked;
    ACBrAAC1.IdentPAF.Paf.BarSimilarECFRestaurante     := ckBarSimilarECFRestaurante.Checked;
    ACBrAAC1.IdentPAF.Paf.BarSimilarECFComum           := ckBarSimilarECFComum.Checked;
    ACBrAAC1.IdentPAF.Paf.BarSimilarBalanca            := ckBarSimilarBalanca.Checked;
    ACBrAAC1.IdentPAF.Paf.UsaImpressoraNaoFiscal       := ckUsaImpressoraNaoFiscal.Checked;
    ACBrAAC1.IdentPAF.Paf.DAVDiscrFormula              := ckDAVDiscrFormula.Checked;

    //posto
    ACBrAAC1.IdentPAF.Paf.ImpedeVendaVlrZero           := ckImpedeVendaVlrZero.Checked;
    ACBrAAC1.IdentPAF.Paf.AcumulaVolumeDiario          := ckAcumulaVolumeDiario.Checked;
    ACBrAAC1.IdentPAF.Paf.ArmazenaEncerranteIniFinal   := ckArmazenaEncerranteIniFinal.Checked;
    ACBrAAC1.IdentPAF.Paf.EmiteContrEncerrAposREDZLEIX := ckEmiteContrEncerrAposREDZLEIX.Checked;
    ACBrAAC1.IdentPAF.Paf.IntegradoComBombas           := ckIntegradoComBombas.Checked;
    ACBrAAC1.IdentPAF.Paf.CriaAbastDivergEncerrante    := ckCriaAbastDivergEncerrante.Checked;
    ACBrAAC1.IdentPAF.Paf.CadastroPlacaBomba           := ckCadastroPlacaBomba.Checked;

    // transporte de passageiros
    ACBrAAC1.IdentPAF.Paf.TransportePassageiro         := ckTransportePassageiro.Checked;

   //criterios por uf
    ACBrAAC1.IdentPAF.Paf.TotalizaValoresLista         := ckTotalizaValoresLista.Checked;
    ACBrAAC1.IdentPAF.Paf.TransfPreVenda               := ckTransfPreVenda.Checked;
    ACBrAAC1.IdentPAF.Paf.TransfDAV                    := ckTransfDAV.Checked;
    ACBrAAC1.IdentPAF.Paf.RecompoeGT                   := ckRecompoeGT.Checked;
    ACBrAAC1.IdentPAF.Paf.EmitePED                     := ckEmitePED.Checked;
    ACBrAAC1.IdentPAF.Paf.CupomMania                   := ckCupomMania.Checked;
    ACBrAAC1.IdentPAF.Paf.MinasLegal                   := ckMinasLegal.Checked;
                                                            
    // ECFS Autorizados para uso
    ACBrAAC1.IdentPAF.ECFsAutorizados.Clear;
    for ct := 0 to lstECFsAutorizados.Items.Count - 1 do
    begin
      with ACBrAAC1.IdentPAF.ECFsAutorizados.New do
      begin
        NumeroSerie := lstECFsAutorizados.Items[ct].Caption;
        ValorGT     := StrToNumero(lstECFsAutorizados.Items[ct].SubItems[0]);
        CRO         := StrToInt(lstECFsAutorizados.Items[ct].SubItems[1]);
        CNI         := StrToInt(lstECFsAutorizados.Items[ct].SubItems[2])
      end;
    end;

    ACBrAAC1.SalvarArquivo;
    MessageDlg('Arquivo auxiliar salvo com sucesso!', mtWarning, [mbOK], 0);

    buscaInformacoesArquivoAuxiliar;
  except
    on E: Exception do
    begin
      ShowMessage('Ocorreu o seguinte erro:' + sLineBreak + E.Message);
    end;
  end;
end;

procedure Tform1.buscaInformacoesArquivoAuxiliar;
var
  ct : integer;
begin
  ACBrAAC1.NomeArquivoAux := GetPathArquivoAuxiliar;
  ACBrAAC1.AbrirArquivo;
  edRazaoSocial.Text        := ACBrAAC1.IdentPAF.Empresa.RazaoSocial;
  edCNPJ.Text               := ACBrAAC1.IdentPAF.Empresa.CNPJ;
  edInscEstadual.Text       := ACBrAAC1.IdentPAF.Empresa.IE;
  edInscMunicipal.Text      := ACBrAAC1.IdentPAF.Empresa.IM;
  edNomePAF.Text            := ACBrAAC1.IdentPAF.Paf.Nome;
  edVersaoPAF.Text          := ACBrAAC1.IdentPAF.Paf.Versao;
  edLinguagem.Text          := ACBrAAC1.IdentPAF.Paf.Linguagem;
  edBancoDados.Text         := ACBrAAC1.IdentPAF.Paf.BancoDados;
  edSistemaOperacional.Text := ACBrAAC1.IdentPAF.Paf.SistemaOperacional;
  edExecutavel.Text         := ACBrAAC1.IdentPAF.Paf.PrincipalExe.Nome;
  edMD5Paf.Text             := ACBrAAC1.IdentPAF.Paf.PrincipalExe.MD5;
  edtPerfilRequisitos.Text  := ACBrAAC1.IdentPAF.Paf.PerfilRequisitos;

  cbxTipoFuncionamento.ItemIndex   := Integer(ACBrAAC1.IdentPAF.Paf.TipoFuncionamento);
  cbxTipoDesenvolvimento.ItemIndex := Integer(ACBrAAC1.IdentPAF.Paf.TipoDesenvolvimento);
  cbxTipoIntegracao.ItemIndex      := Integer(ACBrAAC1.IdentPAF.Paf.IntegracaoPAFECF);

  ckRealizaPreVenda.Checked              := ACBrAAC1.IdentPAF.Paf.RealizaPreVenda;
  ckRealizaDAVECF.Checked                := ACBrAAC1.IdentPAF.Paf.RealizaDAVECF;
  ckRealizaDAVNaoFiscal.Checked          := ACBrAAC1.IdentPAF.Paf.RealizaDAVNaoFiscal;
  ckRealizaDAVOS.Checked                 := ACBrAAC1.IdentPAF.Paf.RealizaDAVOS;
  ckDAVConfAnexoII.Checked               := ACBrAAC1.IdentPAF.Paf.DAVConfAnexoII;
  ckRealizaLancamentoMesa.Checked        := ACBrAAC1.IdentPAF.Paf.RealizaLancamentoMesa;
  ckIndiceTecnicoProd.Checked            := ACBrAAC1.IdentPAF.Paf.IndiceTecnicoProd;
  ckBarSimilarECFRestaurante.Checked     := ACBrAAC1.IdentPAF.Paf.BarSimilarECFRestaurante;
  ckBarSimilarECFComum.Checked           := ACBrAAC1.IdentPAF.Paf.BarSimilarECFComum;
  ckBarSimilarBalanca.Checked            := ACBrAAC1.IdentPAF.Paf.BarSimilarBalanca;
  ckUsaImpressoraNaoFiscal.Checked       := ACBrAAC1.IdentPAF.Paf.UsaImpressoraNaoFiscal;
  ckDAVDiscrFormula.Checked              := ACBrAAC1.IdentPAF.Paf.DAVDiscrFormula;
  ckImpedeVendaVlrZero.Checked           := ACBrAAC1.IdentPAF.Paf.ImpedeVendaVlrZero;
  ckAcumulaVolumeDiario.Checked          := ACBrAAC1.IdentPAF.Paf.AcumulaVolumeDiario;
  ckArmazenaEncerranteIniFinal.Checked   := ACBrAAC1.IdentPAF.Paf.ArmazenaEncerranteIniFinal;
  ckEmiteContrEncerrAposREDZLEIX.Checked := ACBrAAC1.IdentPAF.Paf.EmiteContrEncerrAposREDZLEIX;
  ckIntegradoComBombas.Checked           := ACBrAAC1.IdentPAF.Paf.IntegradoComBombas;
  ckCriaAbastDivergEncerrante.Checked    := ACBrAAC1.IdentPAF.Paf.CriaAbastDivergEncerrante;
  ckCadastroPlacaBomba.Checked           := ACBrAAC1.IdentPAF.Paf.CadastroPlacaBomba;
  ckTransportePassageiro.Checked         := ACBrAAC1.IdentPAF.Paf.TransportePassageiro;
  ckTotalizaValoresLista.Checked         := ACBrAAC1.IdentPAF.Paf.TotalizaValoresLista;
  ckTransfPreVenda.Checked               := ACBrAAC1.IdentPAF.Paf.TransfPreVenda;
  ckTransfDAV.Checked                    := ACBrAAC1.IdentPAF.Paf.TransfDAV;
  ckRecompoeGT.Checked                   := ACBrAAC1.IdentPAF.Paf.RecompoeGT;
  ckEmitePED.Checked                     := ACBrAAC1.IdentPAF.Paf.EmitePED;
  ckCupomMania.Checked                   := ACBrAAC1.IdentPAF.Paf.CupomMania;
  ckMinasLegal.Checked                   := ACBrAAC1.IdentPAF.Paf.MinasLegal;

  lstECFsAutorizados.Clear;
  lstECFsAutorizados.Items.BeginUpdate;
  try
    for ct:=0 to  ACBrAAC1.IdentPAF.ECFsAutorizados.Count-1 do
    begin
      with TACBrAACECF(ACBrAAC1.IdentPAF.ECFsAutorizados.Items[ct]) do
        AdicionarItemGrid(NumeroSerie, ValorGT, CRO, CNI);
    end;
  finally
    lstECFsAutorizados.Items.EndUpdate;
  end;
end;

procedure Tform1.btnECFCapturarDadosClick(Sender: TObject);
begin
  gbxECFCaptura.Enabled := False;
  try
    try
      ACBrECF.Porta       := cbxECFPorta.Text;
      ACBrECF.Modelo      := TACBrECFModelo(cbxECFModelo.ItemIndex + 2);
      ACBrECF.Device.Baud :=  StrToInt(cbxECFVelocidade.Text);
      ACBrECF.Ativar;
    except
      on E: Exception do
      begin
        ShowMessage(
          'Erro ao conectar à porta do ECF' + sLineBreak +
          E.Message
        );
        exit;
      end;
    end;

    edNumeroSerieECF.Text := ACBrECF.NumSerie;
    edCROECF.Text         := ACBrECF.NumCRO;
    edGTECF.Text          := FormatFloat(',#0.00', ACBrECF.GrandeTotal);

    MessageDlg('Dados capturados com sucesso! O ECF será desconectado...', mtInformation, [mbOK], 0);
  finally
    ACBrECF.Desativar;
    gbxECFCaptura.Enabled := True;
  end;
end;

procedure Tform1.btnAdicionarECFClick(Sender: TObject);
begin
  try
    if Trim(edNumeroSerieECF.Text) = '' then
    begin
      edNumeroSerieECF.SetFocus;
      raise Exception.Create('Número de série do ECF inválido!');
    end;

    if Trim(edGTECF.Text) = '' then
    begin
      edNumeroSerieECF.SetFocus;
      raise Exception.Create('Valor do GT não informado!');
    end;

    if Trim(edCNIEE.Text) = '' then
    begin
      edNumeroSerieECF.SetFocus;
      raise Exception.Create('Codigo CNIEE não informado!');
    end;

    AdicionarItemGrid(edNumeroSerieECF.Text, StrToNumero(edGTECF.Text), edCROECF.Value, StrToInt(edCNIEE.Text));
  except
    on E: Exception do
    begin
      edNumeroSerieECF.SetFocus;
      ShowMessage('Ocorreu o seguinte erro:' + sLineBreak + E.Message);
    end;
  end;
end;

procedure Tform1.btnExcluirECFsClick(Sender: TObject);
begin
  lstECFsAutorizados.Clear;
end;

procedure Tform1.btnSalvarArquivoClick(Sender: TObject);
begin
  recriarArquivoAuxiliar;
end;

procedure Tform1.btnSairAplicativoClick(Sender: TObject);
begin
  close;
end;

end.
