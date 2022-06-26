unit PixCDShipay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, Grids, ACBrPIXCD, ACBrPIXPSPShipay,
  ACBrShipaySchemas, ACBrPIXBase
  {$IfDef FPC}
  , DateTimePicker
  {$EndIf};

const
  cMaxConsultas = 36;
  CURL_ACBR = 'https://projetoacbr.com.br/tef/';

type

  TDemoShipayDados = record
    Order_ID: String;
    QRCode: String;
    Total: Double;
    Status: TShipayOrderStatus;
    EmErro: Boolean;
    Order_IDCancelar: String;
    QtdConsultas: Integer;
  end;

  { TfrPixCDShipay }

  TfrPixCDShipay = class(TForm)
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPShipay1: TACBrPSPShipay;
    btCancelarOrder: TBitBtn;
    btCancelarOrderDueDate: TBitBtn;
    btCobBacenCopiaECola: TSpeedButton;
    btConsultarOrderID: TBitBtn;
    btConsultarOrderv: TBitBtn;
    btConsultarPeriodo: TBitBtn;
    btCriarCobranca: TBitBtn;
    btCriarCobBacen: TBitBtn;
    btEstornarOrder: TBitBtn;
    btEstornarOrderDueDate: TBitBtn;
    btFluxoCancelarCobranca: TBitBtn;
    btFluxoCancelarConsulta: TBitBtn;
    btFluxoCopiaECola: TSpeedButton;
    btFluxoEstornarPagto: TBitBtn;
    btFluxoFecharVenda: TBitBtn;
    btFluxoItemExcluir: TBitBtn;
    btFluxoItemIncluir: TBitBtn;
    btFluxoNovaVenda: TBitBtn;
    btFluxoPagar: TBitBtn;
    btFluxoTentarNovamente: TBitBtn;
    btLerParametros: TBitBtn;
    btQREAnalisar1: TBitBtn;
    btSalvarParametros: TBitBtn;
    cbAmbiente: TComboBox;
    cbCobBacenAbatimentoModalidade: TComboBox;
    cbCobBacenDescModalidade: TComboBox;
    cbCobBacenJurosModalidade: TComboBox;
    cbCobBacenMultaModalidade: TComboBox;
    cbCobCarteiras: TComboBox;
    cbFluxoCarteiras: TComboBox;
    edCancelarOrderDueDate_ID: TEdit;
    edCancelarOrder_ID: TEdit;
    edCobBacenAbatimentoValor: TEdit;
    edCobBacenCompradorDoc: TEdit;
    edCobBacenCompradorNome: TEdit;
    edCobBacenDescValor: TEdit;
    edCobBacenJurosValor: TEdit;
    edCobBacenMultaValor: TEdit;
    edCobCopiaECola: TEdit;
    edCobBacenCopiaECola: TEdit;
    edCobBacenValor: TEdit;
    edCompradorDoc: TEdit;
    edCompradorEmail: TEdit;
    edCompradorFone: TEdit;
    edCompradorNome: TEdit;
    edConsultarOrderID: TEdit;
    edConsultarOrdervID: TEdit;
    edConsultarPeriodoInicio: TDateTimePicker;
    edConsultarPeriodoFim: TDateTimePicker;
    edCobBacenVencimento: TDateTimePicker;
    edEstornarOrderDueDate_ID: TEdit;
    edEstornarOrder_ID: TEdit;
    edEstornarValor: TEdit;
    edFluxoClienteDoc: TEdit;
    edFluxoClienteNome: TEdit;
    edFluxoCopiaECola: TEdit;
    edFluxoItemDescricao: TEdit;
    edFluxoItemEAN: TEdit;
    edFluxoItemValor: TEdit;
    edItemDescricao: TEdit;
    edItemEAN: TEdit;
    edItemPreco: TEdit;
    edItemQtd: TEdit;
    edItemSKU: TEdit;
    edCobOrderRef: TEdit;
    edLogArquivo: TEdit;
    edProxyHost: TEdit;
    edProxySenha: TEdit;
    edProxyUsuario: TEdit;
    edShipayAccessKey: TEdit;
    edShipayClientID: TEdit;
    edShipaySecretKey: TEdit;
    edTimeOut: TSpinEdit;
    gbCobBacenAbatimento: TGroupBox;
    gbCobBacenMulta: TGroupBox;
    gbCobBacenJuros: TGroupBox;
    gbCobComprador: TGroupBox;
    gbCobBacenComprador: TGroupBox;
    gbFluxoCarteira: TGroupBox;
    gbFluxoTotal: TGroupBox;
    gbOrdervConsultar: TGroupBox;
    gbConfigDiversos: TGroupBox;
    gbFluxoCliente: TGroupBox;
    gbFluxoItens: TGroupBox;
    gbFluxoStatus: TGroupBox;
    gbCobItem: TGroupBox;
    gbConfigProxy: TGroupBox;
    gbConfigShipay: TGroupBox;
    gdFluxoItens: TStringGrid;
    gdConsultarPeriodoList: TStringGrid;
    gbCobBacenDesconto: TGroupBox;
    gbOrderConsultar: TGroupBox;
    gbOrderCancelar: TGroupBox;
    gbOrderDueDateCancelar: TGroupBox;
    gbOrderEstornar: TGroupBox;
    gbOrderDueDateEstornar: TGroupBox;
    ImageList1: TImageList;
    imCobQRCode: TImage;
    imCobBacenQRCode: TImage;
    imFluxoQRCode: TImage;
    lbAmbiente: TLabel;
    lbCancelarOrderDueDate_ID: TLabel;
    lbCancelarOrder_ID: TLabel;
    lbCobBacenAbatimentoModalidade: TLabel;
    lbCobBacenAbatimentoValor: TLabel;
    lbCobBacenDescModalidade: TLabel;
    lbCobBacenDescValor: TLabel;
    lbCobBacenJurosModalidade: TLabel;
    lbCobBacenJurosValor: TLabel;
    lbCobBacenMultaModalidade: TLabel;
    lbCobBacenMultaValor: TLabel;
    lbCobCopiaECola: TLabel;
    lbCobBacenCopiaECola: TLabel;
    lbCobBacenCompradorDoc: TLabel;
    lbCobBacenCompradorNome: TLabel;
    lbCobBacenDiasPagar: TLabel;
    lbCobBacenValor: TLabel;
    lbCompradorDoc: TLabel;
    lbCompradorEmail: TLabel;
    lbCompradorFone: TLabel;
    lbCompradorNome: TLabel;
    lbConsultarOrderID: TLabel;
    lbConsultarOrdervID: TLabel;
    lbConsultarPeriodoInicio: TLabel;
    lbConsultarPeriodoFim: TLabel;
    lbCobBacenVencimento: TLabel;
    lbConsultarPeriodoOffset: TLabel;
    lbConsultarPeriodoLimite: TLabel;
    lbEstornarOrderDueDate_ID: TLabel;
    lbEstornarOrder_ID: TLabel;
    lbEstornarValor: TLabel;
    lbFluxoClienteDoc: TLabel;
    lbFluxoClienteNome: TLabel;
    lbFluxoCopiaECola: TLabel;
    lbFluxoItemDescricao: TLabel;
    lbFluxoItemEAN: TLabel;
    lbFluxoItemValor: TLabel;
    lbFluxoMsgPagto: TLabel;
    lbItemDescricao: TLabel;
    lbItemEAN: TLabel;
    lbItemPreco: TLabel;
    lbItemQtd: TLabel;
    lbItemSKU: TLabel;
    lbCobOrderRef: TLabel;
    lbCobExpiracao: TLabel;
    lbCobCarteiras: TLabel;
    lbLogArquivo: TLabel;
    lbProxyHost: TLabel;
    lbProxyPorta: TLabel;
    lbProxySenha: TLabel;
    lbProxyUsuario: TLabel;
    lbResposta: TLabel;
    lbShipayAccessKey: TLabel;
    lbShipayClientID: TLabel;
    lbShipaySecretKey: TLabel;
    lbTimeOut: TLabel;
    mmResposta: TMemo;
    PageControl1: TPageControl;
    pnFluxoCopiaECola: TPanel;
    pnFluxoDiv3: TPanel;
    pnFluxoRodapeInfo2: TPanel;
    pnFluxoRodapeInfo1: TPanel;
    pnConfigProxy: TPanel;
    pnConfigDiversos: TPanel;
    pnConfigShipay: TPanel;
    pnFluxoBotoes: TPanel;
    pnFluxoBotoesErroConsultar: TPanel;
    pnFluxoBotoesPrincipais: TPanel;
    pnFluxoBotoesRight: TPanel;
    pnFluxoCarteira: TPanel;
    pnFluxoDiv10: TPanel;
    pnFluxoDiv4: TPanel;
    pnFluxoDiv5: TPanel;
    pnFluxoDiv6: TPanel;
    pnFluxoDiv8: TPanel;
    pnFluxoQRCode: TPanel;
    pnFluxoTotal: TPanel;
    pnFluxoTotalStr: TPanel;
    pnOrderDueDateEstornar: TPanel;
    pnOrderEstornar: TPanel;
    pnOrdervConsultar: TPanel;
    pnOrderConsultar: TPanel;
    pnOrderDueDateCancelar: TPanel;
    pnOrderCancelar: TPanel;
    pnCobBacenMulta: TPanel;
    pnCobBacenJuros: TPanel;
    pnCobBacenDesconto: TPanel;
    pnCobBacenAbatimento: TPanel;
    pnCobBacenComprador: TPanel;
    pnCobItem: TPanel;
    pnCobComprador: TPanel;
    pnFluxoCliente: TPanel;
    pgCriarCob: TPageControl;
    pnCriarCob: TPanel;
    pnCriarCobBacen: TPanel;
    pnFluxoBackground: TPanel;
    pnConfiguracao: TPanel;
    pnRodapeRespostas: TPanel;
    pgPrincipal: TPageControl;
    pgTestes: TPageControl;
    pnFluxoDadosItem: TPanel;
    pnFluxoDiv1: TPanel;
    pnFluxoDiv2: TPanel;
    pnFluxoPagto: TPanel;
    pnFluxoRodape: TPanel;
    pnFluxoStatus: TPanel;
    pnRespostas: TPanel;
    pnRodapeConfig: TPanel;
    sbArqLog: TSpeedButton;
    edConsultarPeriodoOffset: TSpinEdit;
    edConsultarPeriosoLimite: TSpinEdit;
    edCobBacenDiasPagar: TSpinEdit;
    edCobExpiracao: TSpinEdit;
    btCobCopiaECola: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    seProxyPorta: TSpinEdit;
    Splitter1: TSplitter;
    tmCancelarCobPendente: TTimer;
    tsCriarOrderDueDate: TTabSheet;
    tsCriarCobImediata: TTabSheet;
    tbConsultarOrderID: TTabSheet;
    tbConsultarPorPeriodo: TTabSheet;
    tmConsultarPagto: TTimer;
    tsFluxoPagto: TTabSheet;
    tsEstornarPagto: TTabSheet;
    tsConsultarPagto: TTabSheet;
    tsCriarCob: TTabSheet;
    tsOrderCancelar: TTabSheet;
    tsConfiguracao: TTabSheet;
    tsTestes: TTabSheet;
    procedure btCancelarOrderClick(Sender: TObject);
    procedure btCancelarOrderDueDateClick(Sender: TObject);
    procedure btCobBacenCopiaEColaClick(Sender: TObject);
    procedure btCobCopiaEColaClick(Sender: TObject);
    procedure btConsultarOrdervClick(Sender: TObject);
    procedure btConsultarPeriodoClick(Sender: TObject);
    procedure btCriarCobBacenClick(Sender: TObject);
    procedure btEstornarOrderDueDateClick(Sender: TObject);
    procedure btFluxoCancelarCobrancaClick(Sender: TObject);
    procedure btFluxoCancelarConsultaClick(Sender: TObject);
    procedure btFluxoCancelarPagtoClick(Sender: TObject);
    procedure btConsultarOrderIDClick(Sender: TObject);
    procedure btCriarCobrancaClick(Sender: TObject);
    procedure btEstornarOrderClick(Sender: TObject);
    procedure btFluxoCopiaEColaClick(Sender: TObject);
    procedure btFluxoEstornarPagtoClick(Sender: TObject);
    procedure btFluxoFecharVendaClick(Sender: TObject);
    procedure btFluxoReiniciar1Click(Sender: TObject);
    procedure btFluxoNovaVendaClick(Sender: TObject);
    procedure btFluxoItemExcluirClick(Sender: TObject);
    procedure btFluxoItemIncluirClick(Sender: TObject);
    procedure btFluxoTentarNovamenteClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btFluxoPagarClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbVerSenhaProxyClick(Sender: TObject);
    procedure tmCancelarCobPendenteTimer(Sender: TObject);
    procedure tmConsultarPagtoTimer(Sender: TObject);
  private
    fFluxoDados: TDemoShipayDados;

    function GetNomeArquivoConfiguracao: String;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;

    procedure LimparInterfaceFluxo;
    procedure AvaliarInterfaceFluxo;
    procedure AvaliarInterfaceFluxoItem;
    procedure HabilitarInterface(aLiberada: Boolean);

    procedure ReiniciarFluxo;
    procedure ConsultarCobranca;
    procedure EstornarPagamento;

    procedure HabilitarFluxoErroConsulta(aEmErro: Boolean);

    procedure InicializarGridList;
    procedure InicializarGridFluxo;
    procedure ExcluirItemGrid(aGrid: TStringGrid; aIndex: Integer);
    procedure AdicionarItemGridList(aOrder_id, aStatus, aCaixa, aLoja: String; aValor: Double);
    procedure AdicionarItemGridFluxo(aEAN, aDescricao: String; aValor: Double);
    procedure InicializarComponentesDefault;
    procedure CarregarCarteiras;

    procedure AtualizarTotal;
    procedure AtualizarStatus(aStatus: TShipayOrderStatus = spsNone);

    procedure MostrarRespostaEndPoint(aEndPoint: String; aOrder: TACBrPIXSchema);
  public
    property FluxoDados: TDemoShipayDados read fFluxoDados write fFluxoDados;
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;
  end;

var
  frPixCDShipay: TfrPixCDShipay;

implementation

uses
  {$IfDef FPC}
  jsonparser, jsonscanner,
  {$EndIf}
  IniFiles, TypInfo, synacode, DateUtils, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrUtil.Base, ACBrImage, ACBrUtil.Math, ACBrDelphiZXingQRCode,
  ACBrUtil.DateTime, ACBrPIXSchemasCobV, Clipbrd;

{$R *.lfm}

{ TfrPixCDShipay }

procedure TfrPixCDShipay.sbVerSenhaProxyClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbVerSenhaProxy.Down then
    edProxySenha.EchoMode := emNormal
  else
    edProxySenha.EchoMode := emPassword;
  {$EndIf}
end;

procedure TfrPixCDShipay.tmCancelarCobPendenteTimer(Sender: TObject);
begin
  tmCancelarCobPendente.Enabled := False;

  if EstaVazio(fFluxoDados.Order_IDCancelar) then
    Exit;

  try
    if ACBrPSPShipay1.DeleteOrder(FluxoDados.Order_IDCancelar) then
      ShowMessage('Pagamento cancelado/estornado com sucesso');
  except
    tmCancelarCobPendente.Enabled := True;
  end;
end;

procedure TfrPixCDShipay.tmConsultarPagtoTimer(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;
  try      
    if EstaVazio(FluxoDados.Order_ID) then
    begin
      ShowMessage('Nenhuma cobrança a ser consultada');
      Exit;
    end;

    try
      ConsultarCobranca;
      fFluxoDados.QtdConsultas := fFluxoDados.QtdConsultas + 1;
    except
      HabilitarFluxoErroConsulta(True);
    end;
  finally
    if (FluxoDados.Status in [spsPending, spsPendingV, spsRefundPending]) and
       (not fFluxoDados.EmErro) and
       (fFluxoDados.QtdConsultas <= cMaxConsultas) then  // Consulta por 180 segundos
      tmConsultarPagto.Enabled := True;
  end;
end;

function TfrPixCDShipay.GetNomeArquivoConfiguracao: String;
begin
  Result :=  ApplicationPath + 'PIXCDTeste.ini';
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
end;

procedure TfrPixCDShipay.AplicarConfiguracao;
begin
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

procedure TfrPixCDShipay.LimparInterfaceFluxo;
begin
  edFluxoItemEAN.Clear;
  edFluxoItemValor.Clear;
  edFluxoItemDescricao.Clear;
  InicializarGridFluxo;
end;

procedure TfrPixCDShipay.AvaliarInterfaceFluxo;
begin
  with FluxoDados do
  begin
    gbFluxoCliente.Enabled := (Status = spsNone);
    gbFluxoItens.Enabled := (Status in [spsNone, spsCancelled]);

    btFluxoPagar.Visible := (Status in [spsNone, spsCancelled]);
    btFluxoPagar.Enabled := (Total > 0) and btFluxoPagar.Visible;

    cbFluxoCarteiras.Enabled := (Status in [spsNone, spsCancelled]);
    pnFluxoQRCode.Visible := (Status in [spsPending, spsPendingV]);
    btFluxoCancelarCobranca.Visible := (Status in [spsPending, spsPendingV]);
    btFluxoEstornarPagto.Visible := (Status = spsApproved);
    btFluxoNovaVenda.Visible := (not (Status in [spsNone, spsPending, spsPendingV]));
    lbFluxoMsgPagto.Visible := (ACBrPixCD1.Ambiente = ambTeste) and (Status in [spsPending, spsPendingV]);
    pnFluxoCopiaECola.Visible := (Status in [spsPending, spsPendingV]);
  end;

  if gbFluxoItens.Enabled then
    AvaliarInterfaceFluxoItem;
end;

procedure TfrPixCDShipay.AvaliarInterfaceFluxoItem;
begin
  btFluxoItemIncluir.Enabled := (FluxoDados.Status in [spsNone, spsCancelled]);
  btFluxoItemExcluir.Enabled := (FluxoDados.Status in [spsNone, spsCancelled]) and
    (gdFluxoItens.RowCount > 1) and (gdFluxoItens.Row > 0);
end;

procedure TfrPixCDShipay.HabilitarInterface(aLiberada: Boolean);
begin
  pnFluxoPagto.Enabled := aLiberada;
end;

procedure TfrPixCDShipay.ReiniciarFluxo;
begin
  ACBrPSPShipay1.Clear;
  LimparInterfaceFluxo;
  CarregarCarteiras;

  AtualizarTotal;
  AtualizarStatus(spsNone);

  fFluxoDados.QtdConsultas := 0;
  fFluxoDados.Order_ID := EmptyStr;
  AvaliarInterfaceFluxo;
end;

procedure TfrPixCDShipay.ConsultarCobranca;
begin
  HabilitarInterface(False);
  try
    ACBrPSPShipay1.GetOrderInfo(FluxoDados.Order_ID);

    if fFluxoDados.EmErro then
      HabilitarFluxoErroConsulta(False);

    AtualizarStatus(ACBrPSPShipay1.OrderInfo.status);
    AvaliarInterfaceFluxo;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrPixCDShipay.EstornarPagamento;
begin
  HabilitarInterface(False);
  try
    if ACBrPSPShipay1.RefundOrder(FluxoDados.Order_ID, FluxoDados.Total) then
    begin
      ConsultarCobranca;

      if (fFluxoDados.Status = spsRefunded) then
        ShowMessage('Pagamento Estornado com Sucesso')
      else if (fFluxoDados.Status = spsRefundPending) then
        tmConsultarPagto.Enabled := True;  // Estorno pendente? ...Consultar até alterar Status
    end
    else
    begin
      ShowMessage('Falha ao Estornar. Reiniciando o Fluxo de Pagamento');
      ReiniciarFluxo;
    end;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrPixCDShipay.HabilitarFluxoErroConsulta(aEmErro: Boolean);
begin
  fFluxoDados.EmErro := aEmErro;
  pnFluxoQRCode.Visible := (not aEmErro);
  lbFluxoMsgPagto.Visible := (not aEmErro);        
  pnFluxoBotoesErroConsultar.Visible := aEmErro;
  pnFluxoBotoesPrincipais.Visible := (not aEmErro);

  AtualizarStatus;
end;

procedure TfrPixCDShipay.InicializarGridList;
begin
  with gdConsultarPeriodoList do
  begin
    RowCount := 1;
    ColWidths[0] := 230;
    ColWidths[1] := 070;
    ColWidths[2] := 110;
    ColWidths[3] := 110;
    ColWidths[4] := 110;

    Cells[0,0] := 'Order_id';
    Cells[1,0] := 'Valor';
    Cells[2,0] := 'Status';
    Cells[3,0] := 'Caixa';
    Cells[4,0] := 'Loja';
  end;
end;

procedure TfrPixCDShipay.InicializarGridFluxo;
begin
  with gdFluxoItens do
  begin
    RowCount := 1;
    ColWidths[0] := 140;
    ColWidths[1] := 203;
    ColWidths[2] := 120;

    Cells[0,0] := 'EAN';
    Cells[1,0] := 'Descrição';
    Cells[2,0] := 'Valor';

    AdicionarItemGridFluxo('0123456789012', 'Batata Doce', 3.69)
  end;
end;

procedure TfrPixCDShipay.ExcluirItemGrid(aGrid: TStringGrid; aIndex: Integer);
var
  I, J: Integer;
begin
  with aGrid do
  begin
    for I := aIndex to RowCount - 2 do
      for J := 0 to ColCount - 1 do
        Cells[J, I] := Cells[J, I+1];

    RowCount := RowCount - 1
  end;
end;

procedure TfrPixCDShipay.AdicionarItemGridList(aOrder_id, aStatus, aCaixa,
  aLoja: String; aValor: Double);
begin
  with gdConsultarPeriodoList do
  begin
    RowCount := RowCount + 1;
    Cells[0, RowCount-1] := aOrder_id;
    Cells[1, RowCount-1] := FormatFloatBr(aValor);
    Cells[2, RowCount-1] := aStatus;
    Cells[3, RowCount-1] := aCaixa;
    Cells[4, RowCount-1] := aLoja;
  end;
end;

procedure TfrPixCDShipay.AdicionarItemGridFluxo(aEAN, aDescricao: String;
  aValor: Double);
begin
  with gdFluxoItens do
  begin
    RowCount := RowCount + 1;
    Cells[0, RowCount-1] := aEAN;
    Cells[1, RowCount-1] := aDescricao;
    Cells[2, RowCount-1] := FormatFloatBr(aValor);
  end;
end;

procedure TfrPixCDShipay.InicializarComponentesDefault;
var
  I: TACBrPixCDAmbiente;
  J: TACBrPIXDescontoModalidade;
  K: TACBrPIXValoresModalidade;
  L: TACBrPIXJurosModalidade;
begin
  cbAmbiente.Items.Clear;
  for I := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
    cbAmbiente.Items.Add(GetEnumName(TypeInfo(TACBrPixCDAmbiente), Integer(I)));

  cbCobBacenDescModalidade.Items.Clear;
  for J := Low(TACBrPIXDescontoModalidade) to High(TACBrPIXDescontoModalidade) do
    cbCobBacenDescModalidade.Items.Add(IntToStr(Ord(J)) + ' - ' + DescontoModalidadeToString(J));
  cbCobBacenDescModalidade.ItemIndex := 0;

  cbCobBacenMultaModalidade.Items.Clear;
  for K := Low(TACBrPIXValoresModalidade) to High(TACBrPIXValoresModalidade) do
    cbCobBacenMultaModalidade.Items.Add(IntToStr(Ord(K)) + ' - ' + ValoresModalidadeToString(K));
  cbCobBacenMultaModalidade.ItemIndex := 0;

  cbCobBacenJurosModalidade.Items.Clear;
  for L := Low(TACBrPIXJurosModalidade) to High(TACBrPIXJurosModalidade) do
    cbCobBacenJurosModalidade.Items.Add(IntToStr(Ord(L)) + ' - ' + JurosModalidadeToString(L));
  cbCobBacenJurosModalidade.ItemIndex := 0;

  cbCobBacenAbatimentoModalidade.Items.Clear;
  for K := Low(TACBrPIXValoresModalidade) to High(TACBrPIXValoresModalidade) do
    cbCobBacenAbatimentoModalidade.Items.Add(IntToStr(Ord(K)) + ' - ' + ValoresModalidadeToString(K));
  cbCobBacenAbatimentoModalidade.ItemIndex := 0;

  edCobBacenVencimento.DateTime := IncDay(Today, 7);

  edConsultarPeriodoInicio.DateTime := StartOfTheDay(IncDay(Today, -1));
  edConsultarPeriodoFim.DateTime := EndOfTheDay(IncDay(Today, -1));
end;

procedure TfrPixCDShipay.CarregarCarteiras;
var
  I: Integer;
begin
  if (ACBrPSPShipay1.Wallets.Count <= 0) then
  begin
    ACBrPSPShipay1.GetWallets;
    for I := 0 to Pred(ACBrPSPShipay1.Wallets.Count) do
    begin
      cbCobCarteiras.Items.Add(ACBrPSPShipay1.Wallets[I].wallet);
      cbFluxoCarteiras.Items.Add(ACBrPSPShipay1.Wallets[I].wallet);
    end;

    if (cbCobCarteiras.Items.Count > 0) then
      cbCobCarteiras.ItemIndex := (cbCobCarteiras.Items.Count - 1);

    if (cbFluxoCarteiras.Items.Count > 0) then
      cbFluxoCarteiras.ItemIndex := (cbFluxoCarteiras.Items.Count - 1);
  end;
end;

procedure TfrPixCDShipay.AtualizarTotal;
var
  I: Integer;
begin
  fFluxoDados.Total := 0;
  for I := 1 to Pred(gdFluxoItens.RowCount) do
    fFluxoDados.Total := fFluxoDados.Total +
      StrToCurrDef(StringReplace(gdFluxoItens.Cells[2, I], '.', '', []), 0);
  pnFluxoTotalStr.Caption := FormatFloatBr(FluxoDados.Total, 'R$ ,0.00');
end;

procedure TfrPixCDShipay.AtualizarStatus(aStatus: TShipayOrderStatus);

  procedure AtualizarPanelPrincipal(aTexto: String; aCor: TColor);
  begin
    pnFluxoStatus.Color := aCor;
    pnFluxoStatus.Caption := aTexto;
  end;

begin
  if fFluxoDados.EmErro then
  begin
    AtualizarPanelPrincipal('ERRO AO CONSULTAR', clRed);
    Exit;
  end;

  fFluxoDados.Status := aStatus;
  AvaliarInterfaceFluxo;

  case FluxoDados.Status of
    spsPending, spsPendingV: AtualizarPanelPrincipal('AGUARDANDO PAGAMENTO', $001ADAE3);
    spsApproved: AtualizarPanelPrincipal('PAGAMENTO FINALIZADO', $0009E31F);
    spsCancelled: AtualizarPanelPrincipal('PAGAMENTO CANCELADO', $000600EA);
    spsRefunded: AtualizarPanelPrincipal('PAGAMENTO ESTORNADO', $009A9A9A);
    spsExpired: AtualizarPanelPrincipal('PAGAMENTO EXPIRADO', $000080FF);
    spsRefundPending: AtualizarPanelPrincipal('ESTORNO PENDENTE', $00523C30);
  else
    AtualizarPanelPrincipal('VENDENDO', clMenuHighlight);
  end;
end;

procedure TfrPixCDShipay.MostrarRespostaEndPoint(aEndPoint: String;
  aOrder: TACBrPIXSchema);
var
  I: Integer;
begin
  with mmResposta.Lines do
  begin
    Add('Comando executado: ' + aEndPoint);

    if (aOrder is TShipayOrderCreated) then
    with TShipayOrderCreated(aOrder) do
    begin
      Add('order_id: ' + order_id);
      Add('status: ' + ShipayOrderStatusToString(status));
      Add('pix_dict_key: ' + IfEmptyThen(pix_dict_key, 'null'));
      Add('pix_psp: ' + IfEmptyThen(pix_psp, 'null'));
      Add('wallet: ' + wallet);
      Add('CopiaECola: ' + Trim(ACBrPSPShipay1.OrderCreated.qr_code_text));
    end
    else if (aOrder is TShipayOrderInfo) then
    with TShipayOrderInfo(aOrder) do
    begin
      Add('order_id: ' + order_id);
      Add('status: ' + ShipayOrderStatusToString(status));
      Add('balance: ' + FormatFloatBr(balance));
      Add('external_id: ' + external_id);
      Add('wallet: ' + wallet);
      Add('created_at: ' + FormatDateTimeBr(created_at));
      Add('paid_amount: ' + FormatFloatBr(paid_amount));
      Add('payment_date: ' + FormatDateTimeBr(payment_date));
      Add('pix_psp: ' + IfEmptyThen(pix_psp, 'null'));
      Add('total_order: ' + FormatFloatBr(total_order));
      Add('updated_at: ' + FormatDateTimeBr(updated_at));
    end
    else if (aOrder is TShipayOrdersList) then
    with TShipayOrdersList(aOrder) do
    begin
      Add('count: ' + IntToStr(count));
      Add('offset: ' + IntToStr(offset));
      Add('total: ' + IntToStr(total));


      for I := 0 to data.Count - 1 do
        AdicionarItemGridList(
          data[I].order_id,
          ShipayOrderStatusToString(data[I].status),
          data[I].store_pos_name,
          data[I].store_name,
          data[I].total_order);
    end
    else if (aOrder is TShipayOrderError) then
    with TShipayOrderError(aOrder) do
    begin
      Add('code: ' + IntToStr(code));
      Add('message: ' + message);
    end;

    Add(sLineBreak);
  end;
end;

procedure TfrPixCDShipay.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
  AplicarConfiguracao;
end;

procedure TfrPixCDShipay.FormCreate(Sender: TObject);
begin
  InicializarComponentesDefault;
  LerConfiguracao;
  ReiniciarFluxo;
  InicializarGridList;
end;

procedure TfrPixCDShipay.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrPixCDShipay.btFluxoPagarClick(Sender: TObject);
var
  I: Integer;
begin
  HabilitarInterface(False);
  try
    ACBrPSPShipay1.Order.Clear;

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
        unit_price := StrToCurrDef(StringReplace(gdFluxoItens.Cells[2, I], '.', '', []), 0);
      end;

    ACBrPSPShipay1.Order.order_ref := FormatDateTime('yymmddhhnnss', Now);
    ACBrPSPShipay1.Order.total := FluxoDados.Total;
    ACBrPSPShipay1.Order.wallet := cbFluxoCarteiras.Text;

    ACBrPSPShipay1.PostOrder;

    fFluxoDados.Order_ID := ACBrPSPShipay1.OrderCreated.order_id;
    fFluxoDados.QRCode := Trim(ACBrPSPShipay1.OrderCreated.qr_code_text);
    PintarQRCode(FluxoDados.QRCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
    edFluxoCopiaECola.Text := fFluxoDados.QRCode;

    ConsultarCobranca;
    tmConsultarPagto.Enabled := True;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrPixCDShipay.btCriarCobrancaClick(Sender: TObject);
var
  wQRCode, wRotina: String;
  wQtd, wPreco: Double;
begin
  wRotina := 'PostOrder';
  ACBrPSPShipay1.Clear;
  CarregarCarteiras;

  // Preenchendo dados do Comprador
  with ACBrPSPShipay1.Order.buyer do
  begin
    name := edCompradorNome.Text;
    cpf_cnpj := edCompradorDoc.Text;
    email := edCompradorEmail.Text;
    phone := edCompradorFone.Text;
  end;
                                                     
  wQtd   := StrToFloatDef(edItemQtd.Text, 1);
  wPreco := StrToFloatDef(edItemPreco.Text, 1);

  // Preenchendo dados do item
  with ACBrPSPShipay1.Order.items.New do
  begin
    quantity := wQtd;
    unit_price := wPreco;
    ean := edItemEAN.Text;
    sku := edItemSKU.Text;
    item_title := edItemDescricao.Text;
  end;

  ACBrPSPShipay1.Order.order_ref := IfEmptyThen(edCobOrderRef.Text, FormatDateTime('yymmddhhnnss', Now));
  ACBrPSPShipay1.Order.total := RoundABNT((wPreco * wQtd), -2);
  ACBrPSPShipay1.Order.wallet := cbCobCarteiras.Text;

  try
    if (edCobExpiracao.Value > 0) then
    begin
      wRotina := 'PostOrderV';
      ACBrPSPShipay1.Order.expiration := edCobExpiracao.Value;
      ACBrPSPShipay1.PostOrderV;
    end
    else
      ACBrPSPShipay1.PostOrder;
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint(wRotina, ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint(wRotina, ACBrPSPShipay1.OrderCreated);
  wQRCode := Trim(ACBrPSPShipay1.OrderCreated.qr_code_text);
  PintarQRCode(wQRCode, imCobQRCode.Picture.Bitmap, qrUTF8BOM);
  edCobCopiaECola.Text := wQRCode;
end;

procedure TfrPixCDShipay.btEstornarOrderClick(Sender: TObject);
var
  wValor: Double;
begin
  if EstaVazio(edEstornarOrder_ID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edEstornarOrder_ID.SetFocus;
    Exit;
  end;

  if EstaVazio(edEstornarValor.Text) then
  begin
    MessageDlg('Preencha o Valor', mtError, [mbOK], 0);
    edEstornarValor.SetFocus;
    Exit;
  end;

  wValor := StrToFloatDef(edEstornarValor.Text, 1);
  try
    ACBrPSPShipay1.RefundOrder(edEstornarOrder_ID.Text, wValor);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('RefundOrder', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('RefundOrder', ACBrPSPShipay1.OrderInfo);
end;

procedure TfrPixCDShipay.btFluxoCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edFluxoCopiaECola.Text);
end;

procedure TfrPixCDShipay.btFluxoEstornarPagtoClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente estornar o pagamento?', mtConfirmation, mbYesNo, 0) = mrNo) then
    Exit;

  EstornarPagamento;
end;

procedure TfrPixCDShipay.btFluxoFecharVendaClick(Sender: TObject);
begin
  HabilitarFluxoErroConsulta(False);
  ReiniciarFluxo;
end;

procedure TfrPixCDShipay.btFluxoReiniciar1Click(Sender: TObject);
var
  qrcode: String;
begin                                                      
  qrcode := Trim(ACBrPSPShipay1.OrderCreated.qr_code_text);
  PintarQRCode(qrCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
end;

procedure TfrPixCDShipay.btFluxoNovaVendaClick(Sender: TObject);
begin
  ReiniciarFluxo;
end;

procedure TfrPixCDShipay.btCancelarOrderClick(Sender: TObject);
begin
  if EstaVazio(edCancelarOrder_ID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edCancelarOrder_ID.SetFocus;
    Exit;
  end;

  try
    ACBrPSPShipay1.DeleteOrder(edCancelarOrder_ID.Text);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('DeleteOrder', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('DeleteOrder', ACBrPSPShipay1.OrderInfo);
end;

procedure TfrPixCDShipay.btCancelarOrderDueDateClick(Sender: TObject);
begin 
  if EstaVazio(edCancelarOrderDueDate_ID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edCancelarOrderDueDate_ID.SetFocus;
    Exit;
  end;

  try
    ACBrPSPShipay1.PatchOrderDueDate(edCancelarOrderDueDate_ID.Text);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('PatchOrderDueDate', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('PatchOrderDueDate', ACBrPSPShipay1.OrderInfo);
end;

procedure TfrPixCDShipay.btCobBacenCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edCobBacenCopiaECola.Text);
end;

procedure TfrPixCDShipay.btCobCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edCobCopiaECola.Text);
end;

procedure TfrPixCDShipay.btConsultarOrdervClick(Sender: TObject);
begin
  if EstaVazio(edConsultarOrdervID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edConsultarOrdervID.SetFocus;
    Exit;
  end;

  try
    ACBrPSPShipay1.GetOrderVInfo(edConsultarOrdervID.Text);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('GetOrderVInfo', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('GetOrderVInfo', ACBrPSPShipay1.OrderInfo);
end;

procedure TfrPixCDShipay.btConsultarPeriodoClick(Sender: TObject);
begin
  try
    InicializarGridList;
    ACBrPSPShipay1.GetOrdersList(
      edConsultarPeriodoInicio.DateTime,
      edConsultarPeriodoFim.DateTime,
      edConsultarPeriodoOffset.Value,
      edConsultarPeriosoLimite.Value);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('GetOrderInfo', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('GetOrderInfo', ACBrPSPShipay1.OrderList);
end;

procedure TfrPixCDShipay.btCriarCobBacenClick(Sender: TObject);
var
  wQRCode: String;
begin
  ACBrPSPShipay1.Clear;
  CarregarCarteiras;

  // Preenchendo dados do Comprador
  with ACBrPSPShipay1.OrderDueDate.buyer do
  begin
    name := edCobBacenCompradorNome.Text;
    cpf_cnpj := edCobBacenCompradorDoc.Text;
  end;

  // Preenchendo dados do item
  with ACBrPSPShipay1.OrderDueDate.items.New do
  begin
    quantity := 1;
    unit_price := StrToFloatDef(edCobBacenValor.Text, 1);
    item_title := 'Item Teste';
    ean := '0123456789012';
    sku := 'MTC-6110';
  end;

  with ACBrPSPShipay1.OrderDueDate do
  begin
    order_ref := FormatDateTime('yymmddhhnnss', Now);
    total := StrToFloatDef(edCobBacenValor.Text, 1);
    wallet := 'pix';

    with amount_details do
    begin
      discount.modalidade := TACBrPIXDescontoModalidade(StrToIntDef(Copy(cbCobBacenDescModalidade.Text, 1, 1), 0));
      if (Ord(discount.modalidade) >= 3) then
        discount.valorPerc := StrToFloatDef(edCobBacenDescValor.Text, 0)
      else
      with discount.descontosDataFixa.New do
      begin
        data := IncDay(edCobBacenVencimento.DateTime, -2);
        valorPerc := StrToFloatDef(edCobBacenDescValor.Text, 0);
      end;

      fine.modalidade := TACBrPIXValoresModalidade(StrToIntDef(Copy(cbCobBacenMultaModalidade.Text, 1, 1), 0));
      fine.valorPerc := StrToFloatDef(edCobBacenMultaValor.Text, 0);

      interest.modalidade := TACBrPIXJurosModalidade(StrToIntDef(Copy(cbCobBacenJurosModalidade.Text, 1, 1), 0));
      interest.valorPerc := StrToFloatDef(edCobBacenJurosValor.Text, 0);

      rebate.modalidade := TACBrPIXValoresModalidade(StrToIntDef(Copy(cbCobBacenAbatimentoModalidade.Text, 1, 1), 0));
      rebate.valorPerc := StrToFloatDef(edCobBacenAbatimentoValor.Text, 0);
    end;

    calendar.dataDeVencimento := edCobBacenVencimento.DateTime;
    calendar.validadeAposVencimento := edCobBacenDiasPagar.Value;
  end;

  try
    ACBrPSPShipay1.PostOrderDueDate;
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('PostOrderDueDate', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('PostOrderDueDate', ACBrPSPShipay1.OrderCreated);
  wQRCode := Trim(ACBrPSPShipay1.OrderCreated.qr_code_text);
  PintarQRCode(wQRCode, imCobBacenQRCode.Picture.Bitmap, qrUTF8BOM);
  edCobBacenCopiaECola.Text := wQRCode;
end;

procedure TfrPixCDShipay.btEstornarOrderDueDateClick(Sender: TObject);
begin
  if EstaVazio(edEstornarOrderDueDate_ID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edEstornarOrderDueDate_ID.SetFocus;
    Exit;
  end;

  try
    ACBrPSPShipay1.DeleteOrderDueDate(edEstornarOrderDueDate_ID.Text);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('DeleteOrderDueDate', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('DeleteOrderDueDate', ACBrPSPShipay1.OrderInfo);
end;

procedure TfrPixCDShipay.btFluxoCancelarCobrancaClick(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;
  HabilitarInterface(False);
  try
    if (MessageDlg('Deseja realmente Cancelar a Cobrança?', mtConfirmation, mbYesNo, 0) = mrNo) then
    begin
      tmConsultarPagto.Enabled := True;
      Exit;
    end;

    ConsultarCobranca;
    if (fFluxoDados.Status = spsApproved) then
    begin
      if (MessageDlg('Cobrança já foi PAGA. Deseja ESTORNAR pagamento?', mtConfirmation, mbYesNo, 0) = mrYes) then
        EstornarPagamento;
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
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrPixCDShipay.btFluxoCancelarConsultaClick(Sender: TObject);
begin
  ShowMessage('A Cobrança será cancelada/estornada assim que a conexão for reestabelecida');

  HabilitarFluxoErroConsulta(False);
  fFluxoDados.Order_IDCancelar := fFluxoDados.Order_ID;
  tmCancelarCobPendente.Enabled := True;
  ReiniciarFluxo;
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

procedure TfrPixCDShipay.btConsultarOrderIDClick(Sender: TObject);
begin
  if EstaVazio(edConsultarOrderID.Text) then
  begin
    MessageDlg('Preencha a Order_ID', mtError, [mbOK], 0);
    edConsultarOrderID.SetFocus;
    Exit;
  end;

  try
    ACBrPSPShipay1.GetOrderInfo(edConsultarOrderID.Text);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('GetOrderInfo', ACBrPSPShipay1.OrderError);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('GetOrderInfo', ACBrPSPShipay1.OrderInfo);
end;

procedure TfrPixCDShipay.btFluxoItemExcluirClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente excluir o Item?', mtConfirmation, mbYesNo, 0) = mrNo) then
    Exit;

  ExcluirItemGrid(gdFluxoItens, gdFluxoItens.Row);

  AtualizarTotal;
  AvaliarInterfaceFluxoItem;
end;

procedure TfrPixCDShipay.btFluxoItemIncluirClick(Sender: TObject);
var
  wValor: Double;
begin
  wValor := StrToFloatDef(edFluxoItemValor.Text, 1);

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
  else
  begin
    AdicionarItemGridFluxo(
      Trim(edFluxoItemEAN.Text),
      Trim(edFluxoItemDescricao.Text),
      wValor);

    AtualizarTotal;
  end;

  AvaliarInterfaceFluxo;
end;

procedure TfrPixCDShipay.btFluxoTentarNovamenteClick(Sender: TObject);
begin
  try
    ConsultarCobranca;
  except
    ShowMessage('Erro ao Consultar');
  end;
end;

end.

