unit PixCDShipay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, Grids, ACBrPIXCD, ACBrPIXPSPShipay, ACBrShipaySchemas;

const
  CURL_ACBR = 'https://projetoacbr.com.br/tef/';

type

  TDemoShipayDados = record
    Order_ID: String;
    QRCode: String;
    Total: Double;
    Status: TShipayOrderStatus;
  end;

  { TfrPixCDShipay }

  TfrPixCDShipay = class(TForm)
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPShipay1: TACBrPSPShipay;
    btConsultarCobranca: TBitBtn;
    btEstornarCobranca: TBitBtn;
    btCriarCobrancaImediata1: TBitBtn;
    btCancelarCobranca: TBitBtn;
    btFluxoPagar: TBitBtn;
    btFluxoCancelarCobranca: TBitBtn;
    btFluxoReiniciar: TBitBtn;
    btFluxoEstornarPagto: TBitBtn;
    btLerParametros: TBitBtn;
    btQREAnalisar1: TBitBtn;
    btSalvarParametros: TBitBtn;
    btFluxoItemIncluir: TBitBtn;
    btFluxoItemExcluir: TBitBtn;
    cbAmbiente: TComboBox;
    edFluxoClienteDoc: TEdit;
    edFluxoClienteNome: TEdit;
    edConsultarOrder_ID: TEdit;
    edEstornarOrder_ID: TEdit;
    edFluxoItemDescricao: TEdit;
    edItemEAN: TEdit;
    edCancelarOrder_ID: TEdit;
    edFluxoItemEAN: TEdit;
    edFluxoItemPreco: TFloatSpinEdit;
    edProxyHost: TEdit;
    edProxySenha: TEdit;
    edProxyUsuario: TEdit;
    edItemSKU: TEdit;
    edCompradorNome: TEdit;
    edCompradorDoc: TEdit;
    edCompradorEmail: TEdit;
    edCompradorFone: TEdit;
    edItemDescricao: TEdit;
    edLogArquivo: TEdit;
    edShipayAccessKey: TEdit;
    edShipayClientID: TEdit;
    edShipaySecretKey: TEdit;
    edOrderRef: TEdit;
    edItemQtd: TFloatSpinEdit;
    edItemPreco: TFloatSpinEdit;
    edEstornarValor: TFloatSpinEdit;
    gbFluxoCliente: TGroupBox;
    gbItem: TGroupBox;
    gbDiversos: TGroupBox;
    gbFluxoItens: TGroupBox;
    gbProxy: TGroupBox;
    gbShipay: TGroupBox;
    gbComprador: TGroupBox;
    gdFluxoItens: TStringGrid;
    gbFluxoTotal: TGroupBox;
    gbFluxoStatus: TGroupBox;
    ImageList1: TImageList;
    imgQRCriarCobranca: TImage;
    imFluxoQRCode: TImage;
    lbFluxoMsgPagto: TLabel;
    lbAmbiente: TLabel;
    lbCancelarOrder_ID: TLabel;
    lbFluxoClienteDoc: TLabel;
    lbFluxoClienteNome: TLabel;
    lbConsultarOrder_ID: TLabel;
    lbEstornarOrder_ID: TLabel;
    lbFluxoItemDescricao: TLabel;
    lbFluxoItemEAN: TLabel;
    lbFluxoItemValor: TLabel;
    lbProxyHost: TLabel;
    lbProxyPorta: TLabel;
    lbProxySenha: TLabel;
    lbProxyUsuario: TLabel;
    lbTimeOut: TLabel;
    lbItemQtd: TLabel;
    lbItemEAN: TLabel;
    lbItemSKU: TLabel;
    lbItemPreco: TLabel;
    lbCompradorNome: TLabel;
    lbLog: TLabel;
    lbCompradorDoc: TLabel;
    lbCompradorEmail: TLabel;
    lbCompradorFone: TLabel;
    lbCobExpiracao: TLabel;
    lbItemDescricao: TLabel;
    lbLogArquivo: TLabel;
    lbShipayAccessKey: TLabel;
    lbShipayClientID: TLabel;
    lbShipaySecretKey: TLabel;
    lbOrderRef: TLabel;
    lbEstornarValor: TLabel;
    mmLog: TMemo;
    pnFluxoBotoes: TPanel;
    pnFluxoQRCode: TPanel;
    pnFluxoTotal: TPanel;
    pnFluxoStatus: TPanel;
    pnFluxoPagto: TPanel;
    Panel2: TPanel;
    pnFluxoDadosItem: TPanel;
    pnRodapeConfig: TPanel;
    pgPrincipal: TPageControl;
    pgTestes: TPageControl;
    pnLogs: TPanel;
    sbArqLog: TSpeedButton;
    btGerarTxID: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    seCobExpiracao: TSpinEdit;
    seProxyPorta: TSpinEdit;
    edTimeOut: TSpinEdit;
    Splitter1: TSplitter;
    tmConsultarPagto: TTimer;
    tsFluxoPagto: TTabSheet;
    tsEstornarPagto: TTabSheet;
    tsConsultarPagto: TTabSheet;
    tsCriarCob: TTabSheet;
    tsCancelarPagto: TTabSheet;
    tsConfiguracao: TTabSheet;
    tsTestes: TTabSheet;
    procedure btCancelarCobrancaClick(Sender: TObject);
    procedure btFluxoCancelarCobrancaClick(Sender: TObject);
    procedure btFluxoCancelarPagtoClick(Sender: TObject);
    procedure btConsultarCobrancaClick(Sender: TObject);
    procedure btCriarCobrancaImediata1Click(Sender: TObject);
    procedure btEstornarCobrancaClick(Sender: TObject);
    procedure btFluxoEstornarPagtoClick(Sender: TObject);
    procedure btFluxoReiniciar1Click(Sender: TObject);
    procedure btFluxoReiniciarClick(Sender: TObject);
    procedure btGerarTxIDClick(Sender: TObject);
    procedure btFluxoItemExcluirClick(Sender: TObject);
    procedure btFluxoItemIncluirClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btFluxoPagarClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbConsultaCEPClick(Sender: TObject);
    procedure sbVerSenhaProxyClick(Sender: TObject);
    procedure tmConsultarPagtoTimer(Sender: TObject);
  private
    fFluxoDados: TDemoShipayDados;

    function GetNomeArquivoConfiguracao: String;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;

    procedure AvaliarInterfaceFluxo;
    procedure AvaliarInterfaceFluxoItem;

    procedure ReiniciarFluxo;
    procedure ConsultarCobranca;

    procedure AtualizarTotal;
    procedure AtualizarStatus(aStatus: TShipayOrderStatus);

    function FormatarJson(const aJson: String): String;

  public
    property FluxoDados: TDemoShipayDados read fFluxoDados write fFluxoDados;
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;
  end;

var
  frPixCDShipay: TfrPixCDShipay;

implementation

uses
  IniFiles, jsonparser, jsonscanner, TypInfo, synacode, Jsons,
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Base, ACBrImage, ACBrPIXUtil,
  ACBrUtil.Math, ACBrDelphiZXingQRCode;

{$R *.lfm}

{ TfrPixCDShipay }

procedure TfrPixCDShipay.sbVerSenhaProxyClick(Sender: TObject);
begin
  if sbVerSenhaProxy.Down then
    edProxySenha.EchoMode := emNormal
  else
    edProxySenha.EchoMode := emPassword;
end;

procedure TfrPixCDShipay.tmConsultarPagtoTimer(Sender: TObject);
begin
  if EstaVazio(FluxoDados.Order_ID) then
  begin
    tmConsultarPagto.Enabled := False;
    ShowMessage('Nenhuma cobrança a ser consultada');
    Exit;
  end;

  tmConsultarPagto.Enabled := False;
  try
    ConsultarCobranca;
  finally
    if (FluxoDados.Status in [spsPending, spsPendingV]) then
      tmConsultarPagto.Enabled := True;
  end;
end;

function TfrPixCDShipay.GetNomeArquivoConfiguracao: String;
begin
  Result := 'PIXCDTeste.ini';
end;

procedure TfrPixCDShipay.LerConfiguracao;
var
  wIni: TIniFile;
begin
  if (not FilesExists(NomeArquivoConfiguracao)) then
  begin
    pgPrincipal.TabIndex := 2;
    Abort;
  end;

  wIni := TIniFile.Create(NomeArquivoConfiguracao);
  try
    cbAmbiente.ItemIndex := wIni.ReadInteger('PIX','Ambiente', 0);
    edTimeOut.Value := wIni.ReadInteger('PIX', 'TimeOut', ChttpTimeOutDef);

    seCobExpiracao.Value := wIni.ReadInteger('Cobranca', 'Expiracao', seCobExpiracao.Value);

    edProxyHost.Text := wIni.ReadString('Proxy', 'Host', '');
    seProxyPorta.Text := wIni.ReadString('Proxy', 'Porta', '');
    edProxyUsuario.Text := wIni.ReadString('Proxy', 'User', '');
    edProxySenha.Text := StrCrypt(DecodeBase64(wIni.ReadString('Proxy', 'Pass', '')), CURL_ACBR);

    edLogArquivo.Text := wIni.ReadString('Log', 'Arquivo', '');

    edShipayClientID.Text := wIni.ReadString('Shipay', 'ClientID', '');
    edShipaySecretKey.Text := wIni.ReadString('Shipay', 'SecretKey', '');
    edShipayAccessKey.Text := wIni.ReadString('Shipay', 'AccessKey', '');
  finally
    wIni.Free;
  end;

  AplicarConfiguracao;
end;

procedure TfrPixCDShipay.GravarConfiguracao;
var
  wIni: TIniFile;
begin
  wIni := TIniFile.Create(NomeArquivoConfiguracao);
  try
    wIni.WriteInteger('PIX','Ambiente', cbAmbiente.ItemIndex);
    wIni.WriteInteger('PIX', 'TimeOut', edTimeout.Value);

    wIni.WriteInteger('Cobranca', 'Expiracao', seCobExpiracao.Value);

    wIni.WriteString('Proxy', 'Host', edProxyHost.Text);
    wIni.WriteString('Proxy', 'Porta', seProxyPorta.Text);
    wIni.WriteString('Proxy', 'User', edProxyUsuario.Text);
    wIni.WriteString('Proxy', 'Pass', EncodeBase64(StrCrypt(edProxySenha.Text, CURL_ACBR)));

    wIni.WriteString('Log', 'Arquivo', edLogArquivo.Text);

    wIni.WriteString('Shipay', 'ClientID', edShipayClientID.Text);
    wIni.WriteString('Shipay', 'SecretKey', edShipaySecretKey.Text);
    wIni.WriteString('Shipay', 'AccessKey', edShipayAccessKey.Text);
  finally
    wIni.Free;
  end;

  //LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPixCDShipay.AplicarConfiguracao;
begin
  {ACBrPixCD1.Recebedor.Nome := edtRecebedorNome.Text;
  ACBrPixCD1.Recebedor.CEP := edtRecebedorCEP.Text;
  ACBrPixCD1.Recebedor.Cidade := edtRecebedorCidade.Text;
  ACBrPixCD1.Recebedor.UF := cbxRecebedorUF.Text;
  ACBrPixCD1.Recebedor.CodCategoriaComerciante := seRecebedorMCC.Value;}

  ACBrPixCD1.Ambiente := TACBrPixCDAmbiente(cbAmbiente.ItemIndex);
  ACBrPixCD1.TimeOut := edTimeOut.Value;

  ACBrPixCD1.Proxy.Host := edProxyHost.Text;
  ACBrPixCD1.Proxy.Port := seProxyPorta.Text;
  ACBrPixCD1.Proxy.User := edProxyUsuario.Text;
  ACBrPixCD1.Proxy.Pass := edProxySenha.Text;

  ACBrPixCD1.ArqLOG := edLogArquivo.Text;
  ACBrPixCD1.NivelLog := 3;
  ACBrPixCD1.PSP := ACBrPSPShipay1;

  ACBrPSPShipay1.ClientID := edShipayClientID.Text;
  ACBrPSPShipay1.SecretKey := edShipaySecretKey.Text;
  ACBrPSPShipay1.AccessKey := edShipayAccessKey.Text;
end;

procedure TfrPixCDShipay.AtualizarStatus(aStatus: TShipayOrderStatus);

  procedure AtualizarPanel(aTexto: String; aCor: TColor);
  begin
    pnFluxoStatus.Color := aCor;
    pnFluxoStatus.Caption := aTexto;
  end;

begin
  if (FluxoDados.Status = aStatus) then
    Exit;

  fFluxoDados.Status := aStatus;
  AvaliarInterfaceFluxo;

  case FluxoDados.Status of
    spsPending, spsPendingV: AtualizarPanel('AGUARDANDO PAGAMENTO', $001ADAE3);
    spsApproved: AtualizarPanel('PAGAMENTO FINALIZADO', $0009E31F);
    spsCancelled: AtualizarPanel('PAGAMENTO CANCELADO', $000600EA);
    spsRefunded: AtualizarPanel('PAGAMENTO ESTORNADO', $009A9A9A);
    spsExpired: AtualizarPanel('PAGAMENTO EXPIRADO', $000080FF);
    spsRefundPending: AtualizarPanel('ESTORNO PENDENTE', $00523C30);
  else
    AtualizarPanel('VENDENDO', $00979700);
  end;

  if (FluxoDados.Status in [spsPending, spsPendingV]) then
    tmConsultarPagto.Enabled := True;
end;

procedure TfrPixCDShipay.AvaliarInterfaceFluxo;
begin
  with FluxoDados do
  begin
    gbFluxoCliente.Enabled := (Status = spsNone);
    gbFluxoItens.Enabled := (Status = spsNone);

    btFluxoPagar.Visible := (Status = spsNone);
    btFluxoPagar.Enabled := (Total > 0) and btFluxoPagar.Visible;

    pnFluxoQRCode.Visible := (Status in [spsPending, spsPendingV]);
    btFluxoCancelarCobranca.Visible := (Status in [spsPending, spsPendingV]);
    btFluxoEstornarPagto.Visible := (Status = spsApproved);
    btFluxoReiniciar.Visible := (not (Status in [spsNone, spsPending, spsPendingV]));
    lbFluxoMsgPagto.Visible := (ACBrPixCD1.Ambiente = ambTeste) and (Status in [spsPending, spsPendingV]);
  end;

  if gbFluxoItens.Enabled then
    AvaliarInterfaceFluxoItem;
end;

procedure TfrPixCDShipay.AvaliarInterfaceFluxoItem;
begin
  btFluxoItemIncluir.Enabled := (FluxoDados.Status = spsNone);
  btFluxoItemExcluir.Enabled := (FluxoDados.Status = spsNone) and (gdFluxoItens.RowCount > 1);
  pnFluxoTotal.Caption := FormatFloatBr(FluxoDados.Total, 'R$ ,0.00');
end;

procedure TfrPixCDShipay.ReiniciarFluxo;
begin
  ACBrPSPShipay1.Clear;
  AtualizarTotal;
  AtualizarStatus(spsNone);
  fFluxoDados.Order_ID := EmptyStr;
  AvaliarInterfaceFluxo;
end;

procedure TfrPixCDShipay.ConsultarCobranca;
begin
  ACBrPSPShipay1.GetOrderInfo(FluxoDados.Order_ID);
  AtualizarStatus(ACBrPSPShipay1.OrderInfo.status);
  AvaliarInterfaceFluxo;
end;

procedure TfrPixCDShipay.AtualizarTotal;
var
  I: Integer;
begin
  fFluxoDados.Total := 0;
  for I := 1 to Pred(gdFluxoItens.RowCount) do
    fFluxoDados.Total += StrToCurrDef(gdFluxoItens.Cells[2, I], 0);
end;

function TfrPixCDShipay.FormatarJson(const aJson: String): String;
var
  jpar: TJSONParser;
  j: TJsonObject;
begin
  Result := aJson;

  try
    j := TJSONObject.Create();
    try
      Result := j.Decode(Result);
    finally
      j.Free;
    end;
    jpar :=TJSONParser.Create(Result, [joUTF8]);
    try
      Result := jpar.Parse.FormatJSON([], 2);
    finally
      jpar.Free;
    end;
  except
    Result := aJson;
  end;
end;

procedure TfrPixCDShipay.sbConsultaCEPClick(Sender: TObject);
//var
//  EndAchado: TACBrCEPEndereco;
begin
  {try
    ACBrCEP1.BuscarPorCEP(OnlyNumber(edRecebedorCEP.Text));
    if (ACBrCEP1.Enderecos.Count > 0) then
    begin
      EndAchado := ACBrCEP1.Enderecos[0];
      edtRecebedorCidade.Text := EndAchado.Municipio;
      cbxRecebedorUF.ItemIndex := cbxRecebedorUF.Items.IndexOf(EndAchado.UF);
    end;
  except
    MessageDlg('Erro ao executar Consulta do CEP', mtError, [mbOK], 0);
  end;}
end;

procedure TfrPixCDShipay.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
  AplicarConfiguracao;
end;

procedure TfrPixCDShipay.FormCreate(Sender: TObject);
var
  I: TACBrPixCDAmbiente;
begin
  cbAmbiente.Items.Clear;
  for I := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
    cbAmbiente.Items.Add(GetEnumName(TypeInfo(TACBrPixCDAmbiente), Integer(I)));

  gdFluxoItens.ColWidths[0] := 140;
  gdFluxoItens.ColWidths[1] := 205;
  gdFluxoItens.ColWidths[2] := 120;

  LerConfiguracao;
  ReiniciarFluxo;
end;

procedure TfrPixCDShipay.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrPixCDShipay.btFluxoPagarClick(Sender: TObject);
var
  I: Integer;
begin
  ACBrPSPShipay1.Order.Clear;

  //Verificando se precisa carregar as carteiras
  if (ACBrPSPShipay1.Wallets.Count <= 0) then
    ACBrPSPShipay1.GetWallets;

  // Preenchendo dados do Comprador
  with ACBrPSPShipay1.Order.buyer do
  begin
    name := edFluxoClienteNome.Text;
    cpf_cnpj := edFluxoClienteDoc.Text;
  end;

  // Preenchendo dados dos itens
  for I := 1 to Pred(gdFluxoItens.RowCount) do
    with ACBrPSPShipay1.Order.items.New do
    begin
      quantity := 1;
      ean := gdFluxoItens.Cells[0, I];
      sku := gdFluxoItens.Cells[0, I];
      item_title := gdFluxoItens.Cells[1, I];
      unit_price := StrToCurrDef(gdFluxoItens.Cells[2, I], 0);
    end;

  ACBrPSPShipay1.Order.order_ref := FormatDateTime('yymmddhhnnss', Now);
  ACBrPSPShipay1.Order.total := FluxoDados.Total;
  ACBrPSPShipay1.Order.wallet := ACBrPSPShipay1.Wallets[0].wallet;
  ACBrPSPShipay1.PostOrder;

  fFluxoDados.Order_ID := ACBrPSPShipay1.OrderCreated.order_id;
  fFluxoDados.QRCode := Trim(ACBrPSPShipay1.OrderCreated.qr_code_text);
  PintarQRCode(FluxoDados.QRCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
  tmConsultarPagto.Enabled := True;
end;

procedure TfrPixCDShipay.btCriarCobrancaImediata1Click(Sender: TObject);
var
  wQRCode: String;
begin
  ACBrPSPShipay1.Clear;

  //Verificando se precisa carregar as carteiras
  if (ACBrPSPShipay1.Wallets.Count <= 0) then
    ACBrPSPShipay1.GetWallets;

  // Preenchendo dados do Comprador
  with ACBrPSPShipay1.Order.buyer do
  begin
    name := edCompradorNome.Text;
    cpf_cnpj := edCompradorDoc.Text;
    email := edCompradorEmail.Text;
    phone := edCompradorFone.Text;
  end;

  // Preenchendo dados do iten
  with ACBrPSPShipay1.Order.items.New do
  begin
    ean := edItemEAN.Text;
    sku := edItemSKU.Text;
    quantity := edItemQtd.Value;
    unit_price := edItemPreco.Value;
    item_title := edItemDescricao.Text;
  end;

  ACBrPSPShipay1.Order.order_ref := IfEmptyThen(edOrderRef.Text, FormatDateTime('yymmddhhnnss', Now));
  ACBrPSPShipay1.Order.total := RoundABNT(edItemPreco.Value * edItemQtd.Value, -2);
  ACBrPSPShipay1.Order.wallet := ACBrPSPShipay1.Wallets[0].wallet;

  try
    ACBrPSPShipay1.PostOrder;
  except
    On E: Exception do
    begin
      mmLog.Lines.Text := E.Message;
      Abort;
    end;
  end;

  mmLog.Lines.Text := ACBrPSPShipay1.OrderCreated.AsJSON;
  wQRCode := Trim(ACBrPSPShipay1.OrderCreated.qr_code_text);
  PintarQRCode(wQRCode, imgQRCriarCobranca.Picture.Bitmap, qrUTF8BOM);
end;

procedure TfrPixCDShipay.btEstornarCobrancaClick(Sender: TObject);
begin 
  if EstaVazio(edEstornarOrder_ID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edEstornarOrder_ID.SetFocus;
    Exit;
  end;

  if (edEstornarValor.Value <= 0) then
  begin
    MessageDlg('Preencha o Valor', mtError, [mbOK], 0);
    edEstornarValor.SetFocus;
    Exit;
  end;

  ACBrPSPShipay1.RefundOrder(edEstornarOrder_ID.Text, edEstornarValor.Value);
  mmLog.Lines.Text := FormatarJson(ACBrPSPShipay1.OrderInfo.AsJSON);
end;

procedure TfrPixCDShipay.btFluxoEstornarPagtoClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente estornar o pagamento?', mtConfirmation, mbYesNo, 0) = mrNo) then
    Exit;

  if ACBrPSPShipay1.RefundOrder(FluxoDados.Order_ID, FluxoDados.Total) then
  begin
    ConsultarCobranca;
    ShowMessage('Pagamento Estornado com Sucesso');
  end
  else
  begin
    ShowMessage('Falha ao Estornar. Reiniciando o Fluxo de Pagamento');
    ReiniciarFluxo;
  end;
end;

procedure TfrPixCDShipay.btFluxoReiniciar1Click(Sender: TObject);
var
  qrcode: String;
begin                                                      
  qrcode := Trim(ACBrPSPShipay1.OrderCreated.qr_code_text);
  PintarQRCode(qrCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
end;

procedure TfrPixCDShipay.btFluxoReiniciarClick(Sender: TObject);
begin
  ReiniciarFluxo;
end;

procedure TfrPixCDShipay.btCancelarCobrancaClick(Sender: TObject);
begin
  if EstaVazio(edCancelarOrder_ID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edCancelarOrder_ID.SetFocus;
    Exit;
  end;

  ACBrPSPShipay1.DeleteOrder(edCancelarOrder_ID.Text);
  mmLog.Lines.Text := FormatarJson(ACBrPSPShipay1.OrderInfo.AsJSON);
end;

procedure TfrPixCDShipay.btFluxoCancelarCobrancaClick(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;

  if (MessageDlg('Deseja realmente Cancelar a Cobrança?', mtConfirmation, mbYesNo, 0) = mrNo) then
  begin
    tmConsultarPagto.Enabled := True;
    Exit;
  end;

  if ACBrPSPShipay1.DeleteOrder(FluxoDados.Order_ID) then
  begin
    ConsultarCobranca;
    ShowMessage('Cobrança cancelada com sucesso');
  end
  else
  begin
    ShowMessage('Falha ao Cancelar. Reiniciando Fluxo de Pagamento');
    ReiniciarFluxo;
  end;
end;

procedure TfrPixCDShipay.btFluxoCancelarPagtoClick(Sender: TObject);
begin
  if EstaVazio(FluxoDados.Order_ID) then
  begin
    ShowMessage('Nenhuma cobrança pendente');
    Exit;
  end;

  if ACBrPSPShipay1.DeleteOrder(FluxoDados.Order_ID) then
    ShowMessage('Cobrança cancelada com Sucesso')
  else
    ShowMessage('Falha ao Cancelar. Consulte ID para mais informações: ' +
      sLineBreak + '(Order_ID: ' + FluxoDados.Order_ID + ')');
end;

procedure TfrPixCDShipay.btConsultarCobrancaClick(Sender: TObject);
begin
  if EstaVazio(edConsultarOrder_ID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edConsultarOrder_ID.SetFocus;
    Exit;
  end;

  ACBrPSPShipay1.GetOrderInfo(edConsultarOrder_ID.Text);
  mmLog.Lines.Text := FormatarJson(ACBrPSPShipay1.OrderInfo.AsJSON);
end;

procedure TfrPixCDShipay.btGerarTxIDClick(Sender: TObject);
begin
  edOrderRef.Text := CriarTxId;
end;

procedure TfrPixCDShipay.btFluxoItemExcluirClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente excluir o Item?', mtConfirmation, mbYesNo, 0) = mrNo) then
    Exit;

  gdFluxoItens.DeleteRow(gdFluxoItens.Row);

  AtualizarTotal;
  AvaliarInterfaceFluxoItem;
end;

procedure TfrPixCDShipay.btFluxoItemIncluirClick(Sender: TObject);
begin
  if EstaVazio(edFluxoItemDescricao.Text) then
  begin
    ShowMessage('Informe a Descrição do Item');
    edFluxoItemDescricao.SetFocus;
  end
  else if EstaVazio(edFluxoItemEAN.Text) then
  begin
    ShowMessage('Informe o Código EAN do Item');
    edFluxoItemEAN.SetFocus;
  end
  else if (edFluxoItemPreco.Value <= 0) then
  begin
    ShowMessage('Informe o Preço do Item');
    edFluxoItemPreco.SetFocus;
  end
  else
  begin
    with gdFluxoItens do
    begin
      RowCount := RowCount + 1;
      Cells[0,RowCount-1] := Trim(edFluxoItemEAN.Text);
      Cells[1,RowCount-1] := Trim(edFluxoItemDescricao.Text);
      Cells[2,RowCount-1] := Trim(edFluxoItemPreco.Text);
    end;

    AtualizarTotal;
  end;

  AvaliarInterfaceFluxoItem;
end;

end.

