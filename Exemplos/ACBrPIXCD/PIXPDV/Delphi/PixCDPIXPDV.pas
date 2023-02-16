unit PixCDPIXPDV;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, Grids, ACBrPIXCD, ACBrPIXBase, ACBrBase,
  ACBrPIXPSPPixPDV, ACBrPIXSchemasPixPDV, ImgList;

const
  cMaxConsultas = 36;
  CURL_ACBR = 'https://projetoacbr.com.br/tef/';

type

  TDemoPixPDVDados = record
    QrCode_ID: string;
    QRCode: string;
    Total: Double;
    Status: TPixPDVQrStatusTipo;
    EmErro: Boolean;
    Order_IDCancelar: string;
    QtdConsultas: Integer;
  end;

  { TfrPixCDPIXPDV }

  TfrPixCDPIXPDV = class(TForm)
    ACBrPixCD1: TACBrPixCD;
    btCobBacenCopiaECola: TSpeedButton;
    btConsultarQRCodeID: TBitBtn;
    btConsultarPeriodo: TBitBtn;
    btCriarCobBacen: TBitBtn;
    btEstornarQRCode_ID: TBitBtn;
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
    cbCobBacenDescModalidade: TComboBox;
    cbCobBacenJurosModalidade: TComboBox;
    cbCobBacenMultaModalidade: TComboBox;
    edCobBacenCompradorDoc: TEdit;
    edCobBacenCompradorNome: TEdit;
    edCobBacenDescValor: TEdit;
    edCobBacenJurosValor: TEdit;
    edCobBacenMultaValor: TEdit;
    edCobBacenCopiaECola: TEdit;
    edCobBacenValor: TEdit;
    edConsultarQRCodeID: TEdit;
    edConsultarPeriodoInicio: TDateTimePicker;
    edConsultarPeriodoFim: TDateTimePicker;
    edCobBacenVencimento: TDateTimePicker;
    edEstornarQRCode_ID: TEdit;
    edFluxoClienteDoc: TEdit;
    edFluxoClienteNome: TEdit;
    edFluxoQRCodeID: TEdit;
    edFluxoItemDescricao: TEdit;
    edFluxoItemEAN: TEdit;
    edFluxoItemValor: TEdit;
    edLogArquivo: TEdit;
    edProxyHost: TEdit;
    edProxySenha: TEdit;
    edProxyUsuario: TEdit;
    edPIXPDVSecretKey: TEdit;
    edPIXPDVCnpj: TEdit;
    edPIXPDVToken: TEdit;
    edTimeOut: TSpinEdit;
    gbCobBacenMulta: TGroupBox;
    gbCobBacenJuros: TGroupBox;
    gbCobBacenComprador: TGroupBox;
    gbFluxoTotal: TGroupBox;
    gbConfigDiversos: TGroupBox;
    gbFluxoCliente: TGroupBox;
    gbFluxoItens: TGroupBox;
    gbFluxoStatus: TGroupBox;
    gbConfigProxy: TGroupBox;
    gbConfigPIXPDV: TGroupBox;
    gdFluxoItens: TStringGrid;
    gdConsultarPeriodo: TStringGrid;
    gbCobBacenDesconto: TGroupBox;
    gbQRCodeConsultar: TGroupBox;
    gbQRCodeEstornar: TGroupBox;
    imCobBacenQRCode: TImage;
    imFluxoQRCode: TImage;
    lbAmbiente: TLabel;
    lbCobBacenDescModalidade: TLabel;
    lbCobBacenDescValor: TLabel;
    lbCobBacenJurosModalidade: TLabel;
    lbCobBacenJurosValor: TLabel;
    lbCobBacenMultaModalidade: TLabel;
    lbCobBacenMultaValor: TLabel;
    lbCobBacenCopiaECola: TLabel;
    lbCobBacenCompradorDoc: TLabel;
    lbCobBacenCompradorNome: TLabel;
    lbCobBacenDiasPagar: TLabel;
    lbCobBacenValor: TLabel;
    lbConsultarQRCodeID: TLabel;
    lbConsultarPeriodoInicio: TLabel;
    lbConsultarPeriodoFim: TLabel;
    lbCobBacenVencimento: TLabel;
    lbConsultarPeriodoTipoData: TLabel;
    lbEstornarQRCode_ID: TLabel;
    lbFluxoClienteDoc: TLabel;
    lbFluxoClienteNome: TLabel;
    lbFluxoQRCodeID: TLabel;
    lbFluxoItemDescricao: TLabel;
    lbFluxoItemEAN: TLabel;
    lbFluxoItemValor: TLabel;
    lbFluxoMsgPagto: TLabel;
    lbLogArquivo: TLabel;
    lbProxyHost: TLabel;
    lbProxyPorta: TLabel;
    lbProxySenha: TLabel;
    lbProxyUsuario: TLabel;
    lbResposta: TLabel;
    lbPIXPDVSecretKey: TLabel;
    lbPIXPDVCnpj: TLabel;
    lbPIXPDVToken: TLabel;
    lbTimeOut: TLabel;
    mmResposta: TMemo;
    pgConsultar: TPageControl;
    pnFluxoCopiaECola: TPanel;
    pnFluxoDiv3: TPanel;
    pnFluxoRodapeInfo2: TPanel;
    pnFluxoRodapeInfo1: TPanel;
    pnConfigProxy: TPanel;
    pnConfigDiversos: TPanel;
    pnConfigPIXPDV: TPanel;
    pnFluxoBotoes: TPanel;
    pnFluxoBotoesErroConsultar: TPanel;
    pnFluxoBotoesPrincipais: TPanel;
    pnFluxoBotoesRight: TPanel;
    pnFluxoDiv10: TPanel;
    pnFluxoDiv8: TPanel;
    pnFluxoQRCode: TPanel;
    pnFluxoTotal: TPanel;
    pnFluxoTotalStr: TPanel;
    pnQRCodeEstornar: TPanel;
    pnQRCodeConsultar: TPanel;
    pnCobBacenMulta: TPanel;
    pnCobBacenJuros: TPanel;
    pnCobBacenDesconto: TPanel;
    pnCobBacenComprador: TPanel;
    pnFluxoCliente: TPanel;
    pgCriarCob: TPageControl;
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
    edCobBacenDiasPagar: TSpinEdit;
    sbVerSenhaProxy: TSpeedButton;
    seProxyPorta: TSpinEdit;
    Splitter1: TSplitter;
    tmCancelarCobPendente: TTimer;
    tsCriarOrderDueDate: TTabSheet;
    tbConsultarQRCodeID: TTabSheet;
    tbConsultarPorPeriodo: TTabSheet;
    tmConsultarPagto: TTimer;
    tsFluxoPagto: TTabSheet;
    tsEstornarPagto: TTabSheet;
    tsConsultarPagto: TTabSheet;
    tsCriarCob: TTabSheet;
    tsConfiguracao: TTabSheet;
    tsTestes: TTabSheet;
    ACBrPSPPixPDV1: TACBrPSPPixPDV;
    tsConsultaStatusToken: TTabSheet;
    btnConsultaStatusToken: TButton;
    cbConsultaPeriodoTipo: TComboBox;
    tsConsultaSaldo: TTabSheet;
    btnConsultaSaldo: TButton;
    tbConsultarExtrato: TTabSheet;
    lbConsultarExtratoInicio: TLabel;
    lbConsultarExtratoFim: TLabel;
    edConsultarExtratoInicio: TDateTimePicker;
    edConsultarExtratoFim: TDateTimePicker;
    btConsultarExtrato: TBitBtn;
    gdConsultarExtrato: TStringGrid;
    TabSheet1: TTabSheet;
    tsQrDinamico: TTabSheet;
    edQrDinamicoMensagem: TEdit;
    lbCobBacenMensagem: TLabel;
    edQrDinamicoValor: TEdit;
    lbCobBacenValor1: TLabel;
    edQrDinamicoMinutos: TSpinEdit;
    lbCobBacenDiasPagar1: TLabel;
    btQrDinamicoCriar: TBitBtn;
    imQrDinamicoQRCode: TImage;
    lbCobBacenCopiaECola2: TLabel;
    edQrDinamicoCopiaECola: TEdit;
    pnSimularPagto: TPanel;
    lbPixPDVQRCodeID: TLabel;
    edPixPDVSimularPagtoQRCodeID: TEdit;
    btSimularPagto: TBitBtn;
    mmPixPDVSimularPagto: TMemo;
    Panel1: TPanel;
    btSimularPagtoLimpar: TBitBtn;
    procedure btCancelarOrderClick(Sender: TObject);
    procedure btCancelarOrderDueDateClick(Sender: TObject);
    procedure btCobBacenCopiaEColaClick(Sender: TObject);
    procedure btConsultarPeriodoClick(Sender: TObject);
    procedure btCriarCobBacenClick(Sender: TObject);
    procedure btEstornarQRCde_IDClick(Sender: TObject);
    procedure btFluxoCancelarCobrancaClick(Sender: TObject);
    procedure btFluxoCancelarConsultaClick(Sender: TObject);
    procedure btFluxoCancelarPagtoClick(Sender: TObject);
    procedure btConsultarQRCodeIDClick(Sender: TObject);
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
    procedure ACBrPixCD1QuandoGravarLog(const ALogLine: string;
      var Tratado: Boolean);
    procedure btnConsultaStatusTokenClick(Sender: TObject);
    procedure btnConsultaSaldoClick(Sender: TObject);
    procedure btConsultarExtratoClick(Sender: TObject);
    procedure btQrDinamicoCriarClick(Sender: TObject);
    procedure btSimularPagtoLimparClick(Sender: TObject);
    procedure btSimularPagtoClick(Sender: TObject);
  private
    fFluxoDados: TDemoPixPDVDados;

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
    procedure AdicionarItemGridList(aQrCode_id, aStatus, aTipo, aMensagem,
      aEndtoEndId, aIdentificadorId, aSenderNome, aSenderCpfCnpj: string;
      aSenderData, aDataExpirou: TDateTime; aValor: Double);
    procedure InicializarGridExtrato;
    procedure AdicionarItemGridExtrato(aQrCode_id, aTransacaoTipo, aDescricao,
      aTipo: string; aData: TDateTime; aValor: Double);

    procedure InicializarGridFluxo;
    procedure ExcluirItemGrid(aGrid: TStringGrid; aIndex: Integer);
    procedure AdicionarItemGridFluxo(aEAN, aDescricao: String; aValor: Double);
    procedure InicializarComponentesDefault;

    procedure AtualizarTotal;
    procedure AtualizarStatus(aStatus: TPixPDVQrStatusTipo = pqsNone);

    procedure MostrarRespostaEndPoint(aEndPoint: String; aSchema: TACBrPIXSchema);
  public
    property FluxoDados: TDemoPixPDVDados read fFluxoDados write fFluxoDados;
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;
  end;

var
  frPixCDPIXPDV: TfrPixCDPIXPDV;

implementation

uses
  {$IfDef FPC}
  jsonparser, jsonscanner,
  {$EndIf}
  IniFiles, TypInfo, synacode, DateUtils, Clipbrd,
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Base, ACBrImage,
  ACBrUtil.Math, ACBrDelphiZXingQRCode, ACBrUtil.DateTime, ACBrPIXSchemasCobV;

{$R *.dfm}

{ TfrPixCDPIXPDV }

procedure TfrPixCDPIXPDV.FormCreate(Sender: TObject);
begin
  InicializarComponentesDefault;
  LerConfiguracao;
  ReiniciarFluxo;
  InicializarGridList;
end;

{ TfrPixCDPIXPDV - Private }

function TfrPixCDPIXPDV.GetNomeArquivoConfiguracao: String;
begin
  Result :=  ApplicationPath + 'PIXCDTeste.ini';
end;

procedure TfrPixCDPIXPDV.LerConfiguracao;
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

    edPIXPDVCnpj.Text := wIni.ReadString('PIXPDV', 'CNPJ', '00641418000188');
    edPIXPDVToken.Text := wIni.ReadString('PIXPDV', 'Token', 'tk-ezI0OTgwMzRDLUE1MzctNDM3QS1CQTk0LUZFODlFMEE0MzIyNn0');
    edPIXPDVSecretKey.Text := wIni.ReadString('PIXPDV', 'SecretKey', 'sk-e0JBNTFGRTY0LTczMkYtNDYxNC1CQ0Q1LUI0OTVDODgxOTUwRX0');
  finally
    wIni.Free;
  end;

  AplicarConfiguracao;
end;

procedure TfrPixCDPIXPDV.GravarConfiguracao;
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

    wIni.WriteString('PIXPDV', 'CNPJ', edPIXPDVCnpj.Text);
    wIni.WriteString('PIXPDV', 'Token', edPIXPDVToken.Text);
    wIni.WriteString('PIXPDV', 'SecretKey', edPIXPDVSecretKey.Text);
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDPIXPDV.AplicarConfiguracao;
begin
  ACBrPixCD1.PSP := ACBrPSPPixPDV1;

  ACBrPixCD1.Ambiente := TACBrPixCDAmbiente(cbAmbiente.ItemIndex);
  ACBrPixCD1.TimeOut := edTimeOut.Value;

  ACBrPixCD1.Proxy.Host := edProxyHost.Text;
  ACBrPixCD1.Proxy.Port := seProxyPorta.Text;
  ACBrPixCD1.Proxy.User := edProxyUsuario.Text;
  ACBrPixCD1.Proxy.Pass := edProxySenha.Text;

  ACBrPixCD1.ArqLOG := edLogArquivo.Text;
  ACBrPixCD1.NivelLog := 3;

  ACBrPSPPixPDV1.CNPJ := edPIXPDVCnpj.Text;
  ACBrPSPPixPDV1.Token := edPIXPDVToken.Text;
  ACBrPSPPixPDV1.ClientSecret := edPIXPDVSecretKey.Text;
end;

procedure TfrPixCDPIXPDV.LimparInterfaceFluxo;
begin
  edFluxoItemEAN.Clear;
  edFluxoItemValor.Clear;
  edFluxoItemDescricao.Clear;
  InicializarGridFluxo;
end;

procedure TfrPixCDPIXPDV.AvaliarInterfaceFluxo;
begin
  with FluxoDados do
  begin
    gbFluxoCliente.Enabled := (Status = pqsNone);
    gbFluxoItens.Enabled := (Status in [pqsNone, pqsCanceled]);

    btFluxoPagar.Visible := (Status in [pqsNone, pqsCanceled]);
    btFluxoPagar.Enabled := (Total > 0) and btFluxoPagar.Visible;

    pnFluxoQRCode.Visible := (Status in [pqsCreated]);
    btFluxoCancelarCobranca.Visible := (Status in [pqsExpired]);
    btFluxoEstornarPagto.Visible := (Status = pqsApproved);
    btFluxoNovaVenda.Visible := (not (Status in [pqsNone, pqsExpired]));
    lbFluxoMsgPagto.Visible := (ACBrPixCD1.Ambiente = ambTeste) and (Status in [pqsCreated]);
    pnFluxoCopiaECola.Visible := (Status in [pqsCreated]);
  end;

  if gbFluxoItens.Enabled then
    AvaliarInterfaceFluxoItem;
end;

procedure TfrPixCDPIXPDV.AvaliarInterfaceFluxoItem;
begin
  btFluxoItemIncluir.Enabled := (FluxoDados.Status in [pqsNone, pqsCanceled]);
  btFluxoItemExcluir.Enabled := (FluxoDados.Status in [pqsNone, pqsCanceled]) and
    (gdFluxoItens.RowCount > 1) and (gdFluxoItens.Row > 0);
end;

procedure TfrPixCDPIXPDV.HabilitarInterface(aLiberada: Boolean);
begin
  pnFluxoPagto.Enabled := aLiberada;
end;

procedure TfrPixCDPIXPDV.ReiniciarFluxo;
begin
  ACBrPSPPixPDV1.Clear;
  LimparInterfaceFluxo;

  AtualizarTotal;
  AtualizarStatus(pqsNone);

  fFluxoDados.QtdConsultas := 0;
  fFluxoDados.QrCode_ID := EmptyStr;
  AvaliarInterfaceFluxo;
end;

procedure TfrPixCDPIXPDV.ConsultarCobranca;
begin
  HabilitarInterface(False);
  try
    ACBrPSPPixPDV1.GetQrStatus(FluxoDados.QrCode_ID);

    if fFluxoDados.EmErro then
      HabilitarFluxoErroConsulta(False);

    AtualizarStatus(ACBrPSPPixPDV1.QrStatus.Status);
    AvaliarInterfaceFluxo;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrPixCDPIXPDV.EstornarPagamento;
var
   s: string;
begin
  HabilitarInterface(False);
  try
    ACBrPSPPixPDV1.PostQrRefund(fFluxoDados.QrCode_ID);
    s := ACBrPSPPixPDV1.QrRefund.QrRefundId;
    if (s <> EmptyStr) then
    begin
      fFluxoDados.QrCode_ID := s;
      ConsultarCobranca;

      if (fFluxoDados.Status = pqsRefunded) then
        ShowMessage('Pagamento Estornado com Sucesso')
      else if (fFluxoDados.Status = pqsCreated) then
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

procedure TfrPixCDPIXPDV.HabilitarFluxoErroConsulta(aEmErro: Boolean);
begin
  fFluxoDados.EmErro := aEmErro;
  pnFluxoQRCode.Visible := (not aEmErro);
  lbFluxoMsgPagto.Visible := (not aEmErro);
  pnFluxoBotoesErroConsultar.Visible := aEmErro;
  pnFluxoBotoesPrincipais.Visible := (not aEmErro);

  AtualizarStatus;
end;

procedure TfrPixCDPIXPDV.InicializarGridList;
begin
  with gdConsultarPeriodo do
  begin
    RowCount := 1;
    ColWidths[0] := 230;
    ColWidths[1] := 110;
    ColWidths[2] := 070;
    ColWidths[3] := 110;
    ColWidths[4] := 250;
    ColWidths[5] := 150;
    ColWidths[6] := 220;
    ColWidths[7] := 200;
    ColWidths[8] := 110;
    ColWidths[9] := 80;
    ColWidths[10] := 80;

    Cells[0,0] := 'QrCode_id';
    Cells[1,0] := 'Status';
    Cells[2,0] := 'Valor';
    Cells[3,0] := 'Tipo';
    Cells[4,0] := 'Mensagem';
    Cells[5,0] := 'EndToEndId';
    Cells[6,0] := 'IdentificadorId';
    Cells[7,0] := 'Sender-Nome';
    Cells[8,0] := 'Sender-cpf/cnpj';
    Cells[9,0] := 'Sender-Data';
    Cells[10,0] := 'Data Expirou';
  end;
end;

procedure TfrPixCDPIXPDV.AdicionarItemGridList(aQrCode_id, aStatus, aTipo,
  aMensagem, aEndtoEndId, aIdentificadorId, aSenderNome, aSenderCpfCnpj: string;
  aSenderData, aDataExpirou: TDateTime; aValor: Double);
begin
  with gdConsultarPeriodo do
  begin
    RowCount := RowCount + 1;
    Cells[0, RowCount-1] := aQrCode_id;
    Cells[1, RowCount-1] := aStatus;
    Cells[2, RowCount-1] := FormatFloatBr(aValor);
    Cells[3, RowCount-1] := aTipo;
    Cells[4, RowCount-1] := aMensagem;
    Cells[5, RowCount-1] := aEndtoEndId;
    Cells[6, RowCount-1] := aIdentificadorId;
    Cells[7, RowCount-1] := aSenderNome;
    Cells[8, RowCount-1] := aSenderCpfCnpj;
    Cells[9, RowCount-1] := FormatDateBr(aSenderData);
    Cells[10, RowCount-1] := FormatDateBr(aDataExpirou);
  end;
end;

procedure TfrPixCDPIXPDV.InicializarGridExtrato;
begin
  with gdConsultarExtrato do
  begin
    RowCount := 1;
    ColWidths[0] := 230;
    ColWidths[1] := 200;
    ColWidths[2] := 250;
    ColWidths[3] := 80;
    ColWidths[4] := 070;
    ColWidths[5] := 30;

    Cells[0,0] := 'QrCode_id';
    Cells[1,0] := 'Tipo de Transação';
    Cells[2,0] := 'Descrição';
    Cells[3,0] := 'Data';
    Cells[4,0] := 'Valor';
    Cells[5,0] := 'Tipo';
  end;
end;

procedure TfrPixCDPIXPDV.AdicionarItemGridExtrato(aQrCode_id, aTransacaoTipo,
  aDescricao, aTipo: string; aData: TDateTime; aValor: Double);
begin
  with gdConsultarExtrato do
  begin
    RowCount := RowCount + 1;
    Cells[0, RowCount-1] := aQrCode_id;
    Cells[1, RowCount-1] := aTransacaoTipo;
    Cells[2, RowCount-1] := aDescricao;
    Cells[3, RowCount-1] := FormatDateBr(aData);
    Cells[4, RowCount-1] := FormatFloatBr(aValor);
    Cells[5, RowCount-1] := aTipo;
  end;
end;

procedure TfrPixCDPIXPDV.InicializarGridFluxo;
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

procedure TfrPixCDPIXPDV.ExcluirItemGrid(aGrid: TStringGrid; aIndex: Integer);
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

procedure TfrPixCDPIXPDV.AdicionarItemGridFluxo(aEAN, aDescricao: String;
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

procedure TfrPixCDPIXPDV.InicializarComponentesDefault;
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

  edCobBacenVencimento.DateTime := IncDay(Today, 7);

  edConsultarPeriodoInicio.DateTime := StartOfTheDay(IncDay(Today, -1));
  edConsultarPeriodoFim.DateTime := EndOfTheDay(IncDay(Today, -1));
  cbConsultaPeriodoTipo.ItemIndex := 0;

  edConsultarExtratoInicio.DateTime := StartOfTheDay(IncDay(Today, -1));
  edConsultarExtratoFim.DateTime := EndOfTheDay(IncDay(Today, -1));
end;

procedure TfrPixCDPIXPDV.AtualizarTotal;
var
  I: Integer;
begin
  fFluxoDados.Total := 0;
  for I := 1 to Pred(gdFluxoItens.RowCount) do
    fFluxoDados.Total := fFluxoDados.Total +
      StrToCurrDef(StringReplace(gdFluxoItens.Cells[2, I], '.', '', []), 0);
  pnFluxoTotalStr.Caption := FormatFloatBr(FluxoDados.Total, 'R$ ,0.00');
end;

procedure TfrPixCDPIXPDV.AtualizarStatus(aStatus: TPixPDVQrStatusTipo);

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
    pqsCreated: AtualizarPanelPrincipal('AGUARDANDO PAGAMENTO', $001ADAE3);
    pqsApproved: AtualizarPanelPrincipal('PAGAMENTO FINALIZADO', $0009E31F);
    pqsCanceled: AtualizarPanelPrincipal('PAGAMENTO CANCELADO', $000600EA);
    pqsRefunded: AtualizarPanelPrincipal('PAGAMENTO ESTORNADO', $009A9A9A);
    pqsExpired: AtualizarPanelPrincipal('PAGAMENTO EXPIRADO', $000080FF);
//    pqsRefundPending: AtualizarPanelPrincipal('ESTORNO PENDENTE', $00523C30);
  else
    AtualizarPanelPrincipal('VENDENDO', clMenuHighlight);
  end;
end;

procedure TfrPixCDPIXPDV.MostrarRespostaEndPoint(aEndPoint: String;
  aSchema: TACBrPIXSchema);
var
  I: Integer;
begin
  with mmResposta.Lines do
  begin
    Add('Comando executado: ' + aEndPoint);

    if (aSchema is TPixPDVQrGerado) then
    with TPixPDVQrGerado(aSchema) do
    begin
      Add('QrCode_id: ' + QrCodeId);
      Add('QrCode: ' + QrCode);
      Add('QrCode_Base64: ' + QrCodeBase64);
      Add('Url: ' + Url);
      Add(' ');
      Add('Json: ' + AsJSON);
    end
    else if (aSchema is TPixPDVStatusTokenDados) then
    with TPixPDVStatusTokenDados(aSchema) do
    begin
      Add('CNPJ: ' + Cnpj);
      Add('Nome: ' + Nome);
      Add('Fantasia: ' + Fantasia);
      Add(' ');
      Add('Json: ' + AsJSON);
    end
    else if (aSchema is TPixPDVQrStatus) then
    with TPixPDVQrStatus(aSchema) do
    begin
      Add('Status: ' + PixPDVQrStatusToString(Status));
      if Status = pqsApproved then begin
        Add('endToEndId: ' + EndToEndId);
        Add('identificadorId: ' + IdentificadorId);
        Add('Pagador Nome: ' + Sender.Nome);
        Add('Pagador Cpf/Cnpj: ' + Sender.Cpf_Cnpj);
        Add('Pagador Data: ' + DateTimeToStr(Sender.Data));
        Add('Pagador Valor: ' + FloatToStr(Sender.Valor));
        Add(' ');
        Add('Json: ' + AsJSON);
      end;
    end
    else if (aSchema is TPixPDVQrResumo) then
    with TPixPDVQrResumo(aSchema) do
    begin
      for I := 0 to itens.Count - 1 do
        AdicionarItemGridList(
          itens[I].TransacaoId,
          PixPDVQrStatusToString(itens[I].Status),
          itens[I].TransacaoTipo,
          itens[I].Mensagem,
          itens[I].EndToEndId,
          itens[I].IdentificadorId,
          itens[I].Sender.Nome,
          itens[I].Sender.Cpf_Cnpj,
          itens[I].Sender.Data,
          itens[I].Expirou,
          itens[I].Sender.Valor);
      Add(' ');
      Add('Json: ' + AsJSON);
    end
    else if (aSchema is TPixPDVSaldo) then
    with TPixPDVSaldo(aSchema) do
    begin
      Add('Total: ' + FormatFloatBr(Total));
      Add('Disponivel: ' + FormatFloatBr(Disponivel));
      Add('Bloqueado: ' + FormatFloatBr(Bloqueado));
      Add(' ');
      Add('Json: ' + AsJSON);
    end
    else if (aSchema is TPixPDVExtrato) then
    with TPixPDVExtrato(aSchema) do
    begin
      for I := 0 to itens.Count - 1 do
        AdicionarItemGridExtrato(
          itens[I].TransacaoId,
          itens[I].TransacaoTipo,
          itens[I].Descricao,
          itens[I].Tipo,
          itens[I].Data,
          itens[I].Valor);
      Add(' ');
      Add('Json: ' + AsJSON);
    end
    else if (aSchema is TPixPDVQrRefund) then
    with TPixPDVQrRefund(aSchema) do
    begin
      Add('refundId: ' + QrRefundId);
      Add(' ');
      Add('Json: ' + AsJSON);
    end
    else if (aSchema is TPixPDVError) then
    with TPixPDVError(aSchema) do
    begin
      Add('code: ' + IntToStr(code));
      Add('message: ' + description);
    end;

    Add(sLineBreak);
  end;
end;

{ TfrPixCDPIXPDV - Page Control - Fluxo de Pagamento }

procedure TfrPixCDPIXPDV.btFluxoItemIncluirClick(Sender: TObject);
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

procedure TfrPixCDPIXPDV.btFluxoItemExcluirClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente excluir o Item?', mtConfirmation, mbOKCancel, 0) = mrNo) then
    Exit;

  ExcluirItemGrid(gdFluxoItens, gdFluxoItens.Row);

  AtualizarTotal;
  AvaliarInterfaceFluxoItem;
end;

procedure TfrPixCDPIXPDV.btFluxoPagarClick(Sender: TObject);
begin
  HabilitarInterface(False);
  try
    ACBrPSPPixPDV1.QrDinamico.Clear;

    ACBrPSPPixPDV1.QrDinamico.Valor := FluxoDados.Total;
    ACBrPSPPixPDV1.QrDinamico.Minutos := 5;
    ACBrPSPPixPDV1.QrDinamico.Mensagem := 'Venda efetuada para o cliente: ' +
      edFluxoClienteDoc.Text + ' - ' + edFluxoClienteNome.Text;

    ACBrPSPPixPDV1.PostQrDinamico;

    fFluxoDados.QrCode_ID := ACBrPSPPixPDV1.QrGerado.QrCodeId;
    fFluxoDados.QRCode := ACBrPSPPixPDV1.QrGerado.QrCode;
    PintarQRCode(FluxoDados.QRCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
    edFluxoQRCodeID.Text := fFluxoDados.QRCode_ID;

    ConsultarCobranca;
    tmConsultarPagto.Enabled := True;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrPixCDPIXPDV.btFluxoCancelarCobrancaClick(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;
  HabilitarInterface(False);
  try
    if (MessageDlg('Deseja realmente Cancelar a Cobrança?', mtConfirmation, mbOKCancel, 0) = mrNo) then
    begin
      tmConsultarPagto.Enabled := True;
      Exit;
    end;

    ConsultarCobranca;
    if (fFluxoDados.Status = pqsApproved) then
    begin
      if (MessageDlg('Cobrança já foi PAGA. Deseja ESTORNAR pagamento?', mtConfirmation, mbOKCancel, 0) = mrYes) then
        EstornarPagamento;
      Exit;
    end;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrPixCDPIXPDV.btFluxoEstornarPagtoClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente estornar o pagamento?', mtConfirmation, mbOKCancel, 0) = mrNo) then
    Exit;

  EstornarPagamento;
end;

procedure TfrPixCDPIXPDV.btFluxoNovaVendaClick(Sender: TObject);
begin
  ReiniciarFluxo;
end;

procedure TfrPixCDPIXPDV.btFluxoCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edFluxoQRCodeID.Text);
end;

{ TfrPixCDPIXPDV - Page Control - EndPoints - Criar Cobrança }

procedure TfrPixCDPIXPDV.btCriarCobBacenClick(Sender: TObject);
var
  wQRCode: String;
begin
  ACBrPSPPixPDV1.Clear;

  with ACBrPSPPixPDV1.QrCobranca do
  begin
    Valor := StrToFloatDef(edCobBacenValor.Text, 1);
    Vencimento := edCobBacenVencimento.DateTime;
    Expira := edCobBacenDiasPagar.Value;
    Mensagem := 'Pix Cobranca (Boleto Pix) emitido por PIXPDV !';
    Documento := FormatDateTime('yymmddhhnnss', Now);

    // Preenchendo dados do Comprador
    Pagador.Nome := edCobBacenCompradorNome.Text;
    if Length(Trim(edCobBacenCompradorDoc.Text)) = 11 then
      Pagador.Cpf := edCobBacenCompradorDoc.Text
    else
      Pagador.Cnpj := edCobBacenCompradorDoc.Text;
    Pagador.Logradouro := 'Rua XYZ, 123';
    Pagador.Cidade := 'TATUI';
    Pagador.Uf := 'SP';
    Pagador.Cep := '18280460';

    Desconto.modalidade := TACBrPIXDescontoModalidade(StrToIntDef(Copy(cbCobBacenDescModalidade.Text, 1, 1), 0));
    if (Ord(Desconto.modalidade) >= 3) then
      Desconto.valorPerc := StrToFloatDef(edCobBacenDescValor.Text, 0)
    else
      with Desconto.descontosDataFixa.New do
      begin
        data := IncDay(edCobBacenVencimento.DateTime, -2);
        valorPerc := StrToFloatDef(edCobBacenDescValor.Text, 0);
      end;

    Juros.modalidade := TACBrPIXJurosModalidade(StrToIntDef(Copy(cbCobBacenJurosModalidade.Text, 1, 1), 0));
    Juros.valorPerc := StrToFloatDef(edCobBacenJurosValor.Text, 0);

    Multa.modalidade := TACBrPIXValoresModalidade(StrToIntDef(Copy(cbCobBacenMultaModalidade.Text, 1, 1), 0));
    Multa.valorPerc := StrToFloatDef(edCobBacenMultaValor.Text, 0);
  end;

  try
    ACBrPSPPixPDV1.PostQrCobranca;
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('PostQrCobranca', ACBrPSPPixPDV1.Error);
      Abort;
    end;
  end;

  MostrarRespostaEndPoint('PostQrCobranca', ACBrPSPPixPDV1.QrCobranca);
  wQRCode := Trim(ACBrPSPPixPDV1.QrGerado.QrCode);
  PintarQRCode(wQRCode, imCobBacenQRCode.Picture.Bitmap, qrUTF8BOM);
  edCobBacenCopiaECola.Text := wQRCode;
end;

procedure TfrPixCDPIXPDV.btCobBacenCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edCobBacenCopiaECola.Text);
end;     

{ TfrPixCDPIXPDV - Page Control - EndPoints - Cancelar }

procedure TfrPixCDPIXPDV.btCancelarOrderClick(Sender: TObject);
begin
  MessageDlg('Em desenvolvimento', mtInformation, [mbOK], 0);
end;

procedure TfrPixCDPIXPDV.btCancelarOrderDueDateClick(Sender: TObject);
begin
  MessageDlg('Em desenvolvimento', mtInformation, [mbOK], 0);
end;

{ TfrPixCDPIXPDV - Page Control - EndPoints - Consultar - Status token }

procedure TfrPixCDPIXPDV.btnConsultaStatusTokenClick(Sender: TObject);
begin
  try
    ACBrPSPPixPDV1.PostStatusToken;
    MostrarRespostaEndPoint('StatusToken', ACBrPSPPixPDV1.StatusTokenDados);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('StatusToken', ACBrPSPPixPDV1.Error);
    end;
  end;
end;

{ TfrPixCDPIXPDV - Page Control - EndPoints - Consultar - QrCode }

procedure TfrPixCDPIXPDV.btConsultarQRCodeIDClick(Sender: TObject);
begin
  if EstaVazio(edConsultarQRCodeID.Text) then
  begin
    MessageDlg('Preencha o QRCode_ID', mtError, [mbOK], 0);
    edConsultarQRCodeID.SetFocus;
    Exit;
  end;

  try
    ACBrPSPPixPDV1.GetQrStatus(edConsultarQRCodeID.Text);
    MostrarRespostaEndPoint('GetQrStatus', ACBrPSPPixPDV1.QrStatus);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('GetQrStatus', ACBrPSPPixPDV1.Error);
    end;
  end;
end;

{ TfrPixCDPIXPDV - Page Control - EndPoints - Consultar - Período }

procedure TfrPixCDPIXPDV.btConsultarPeriodoClick(Sender: TObject);
var
  wtipodata: TACBrPSPPIXPDVTipoData;
begin
  try
    InicializarGridList;
    if cbConsultaPeriodoTipo.ItemIndex = 1 then
      wtipodata := tpdtRecebimento
    else if cbConsultaPeriodoTipo.ItemIndex = 2 then
      wtipodata := tpdtVencimento
    else
      wtipodata := tpdtEmissao;

    ACBrPSPPixPDV1.GetQrResumo(
      edConsultarPeriodoInicio.DateTime,
      edConsultarPeriodoFim.DateTime,
      wtipodata);
    MostrarRespostaEndPoint('GetQrResumo', ACBrPSPPixPDV1.QrResumo);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('GetQrResumo', ACBrPSPPixPDV1.Error);
    end;
  end;
end;

{ TfrPixCDPIXPDV - Page Control - EndPoints - Consultar - Saldo }

procedure TfrPixCDPIXPDV.btnConsultaSaldoClick(Sender: TObject);
begin
  try
    ACBrPSPPixPDV1.GetSaldo;
    MostrarRespostaEndPoint('GetSaldo', ACBrPSPPixPDV1.Saldo);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('GetSaldo', ACBrPSPPixPDV1.Error);
    end;
  end;
end;

{ TfrPixCDPIXPDV - Page Control - EndPoints - Consultar - Extrato }

procedure TfrPixCDPIXPDV.btConsultarExtratoClick(Sender: TObject);
begin
  try
    InicializarGridExtrato;

    ACBrPSPPixPDV1.GetExtrato(
      edConsultarPeriodoInicio.DateTime,
      edConsultarPeriodoFim.DateTime);
    MostrarRespostaEndPoint('GetExtrato', ACBrPSPPixPDV1.Extrato);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('GetExtrato', ACBrPSPPixPDV1.Error);
    end;
  end;
end;

{ TfrPixCDPIXPDV - Page Control - EndPoints - Estornar }

procedure TfrPixCDPIXPDV.btEstornarQRCde_IDClick(Sender: TObject);
begin
  if EstaVazio(edEstornarQRCode_ID.Text) then
  begin
    MessageDlg('Preencha a QRCode_ID a ser estornado !', mtError, [mbOK], 0);
    edEstornarQRCode_ID.SetFocus;
    Exit;
  end;

  try
    ACBrPSPPixPDV1.PostQrRefund(edEstornarQRCode_ID.Text);
    MostrarRespostaEndPoint('QrRefund', ACBrPSPPixPDV1.QrRefund);
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      MostrarRespostaEndPoint('QrRefund', ACBrPSPPixPDV1.Error);
    end;
  end;
end;

{ TfrPixCDPIXPDV - Page Control - Configuração }

procedure TfrPixCDPIXPDV.sbVerSenhaProxyClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbVerSenhaProxy.Down then
    edProxySenha.EchoMode := emNormal
  else
    edProxySenha.EchoMode := emPassword;
  {$EndIf}
end;

procedure TfrPixCDPIXPDV.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
  AplicarConfiguracao;
end;

procedure TfrPixCDPIXPDV.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;







procedure TfrPixCDPIXPDV.tmCancelarCobPendenteTimer(Sender: TObject);
begin
  tmCancelarCobPendente.Enabled := False;

  if EstaVazio(fFluxoDados.Order_IDCancelar) then
    Exit;
end;

procedure TfrPixCDPIXPDV.tmConsultarPagtoTimer(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;
  try      
    if EstaVazio(FluxoDados.QrCode_ID) then
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
    if (FluxoDados.Status in [pqsCreated]) and
       (not fFluxoDados.EmErro) and
       (fFluxoDados.QtdConsultas <= cMaxConsultas) then  // Consulta por 180 segundos
      tmConsultarPagto.Enabled := True;
  end;
end;

procedure TfrPixCDPIXPDV.ACBrPixCD1QuandoGravarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  Tratado := False;
end;

procedure TfrPixCDPIXPDV.btFluxoFecharVendaClick(Sender: TObject);
begin
  HabilitarFluxoErroConsulta(False);
  ReiniciarFluxo;
end;

procedure TfrPixCDPIXPDV.btFluxoReiniciar1Click(Sender: TObject);
var
  qrcode: String;
begin
  qrcode := ACBrPSPPixPDV1.QrGerado.QrCode;
  PintarQRCode(qrCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
end;

procedure TfrPixCDPIXPDV.btFluxoCancelarConsultaClick(Sender: TObject);
begin
  ShowMessage('A Cobrança será cancelada/estornada assim que a conexão for reestabelecida');

  HabilitarFluxoErroConsulta(False);
  fFluxoDados.Order_IDCancelar := fFluxoDados.QrCode_ID;
  tmCancelarCobPendente.Enabled := True;
  ReiniciarFluxo;
end;

procedure TfrPixCDPIXPDV.btFluxoCancelarPagtoClick(Sender: TObject);
begin
  if EstaVazio(FluxoDados.QrCode_ID) then
  begin
    ShowMessage('Nenhuma cobrança pendente');
    Exit;
  end;
end;

procedure TfrPixCDPIXPDV.btFluxoTentarNovamenteClick(Sender: TObject);
begin
  try
    ConsultarCobranca;
  except
    ShowMessage('Erro ao Consultar');
  end;
end;

procedure TfrPixCDPIXPDV.btQrDinamicoCriarClick(Sender: TObject);
var
  wQRCode: String;
begin
  ACBrPSPPixPDV1.Clear;

  with ACBrPSPPixPDV1.QrDinamico do
  begin
    Valor := StrToFloatDef(edQrDinamicoValor.Text, 1);
    Minutos := edQrDinamicoMinutos.Value;
    Mensagem := edQrDinamicoMensagem.Text;
  end;

  try
    ACBrPSPPixPDV1.PostQrDinamico;

    MostrarRespostaEndPoint('PostQrDinamico', ACBrPSPPixPDV1.QrGerado);
    wQRCode := Trim(ACBrPSPPixPDV1.QrGerado.QrCode);
    PintarQRCode(wQRCode, imQrDinamicoQRCode.Picture.Bitmap, qrUTF8BOM);
    edQrDinamicoCopiaECola.Text := wQRCode;
  except
    On E: Exception do
    begin
      mmResposta.Lines.Add(E.Message);
      mmResposta.Lines.Add(ACBrPSPPixPDV1.QrDinamico.AsJSON);

      MostrarRespostaEndPoint('PostQrDinamico', ACBrPSPPixPDV1.Error);
    end;
  end;
end;

procedure TfrPixCDPIXPDV.btSimularPagtoLimparClick(Sender: TObject);
begin
  mmPixPDVSimularPagto.Lines.Clear;
end;

procedure TfrPixCDPIXPDV.btSimularPagtoClick(Sender: TObject);
var
  wTimerPagtoEnabled: Boolean;
begin
  mmPixPDVSimularPagto.Lines.Clear;

  if EstaVazio(edPixPDVSimularPagtoQRCodeID.Text) then
  begin
    MessageDlg('Preencha a QRCode_ID para Simular o Pagamento', mtError, [mbOK], 0);
    edPixPDVSimularPagtoQRCodeID.SetFocus;
    Exit;
  end;

  if (ACBrPixCD1.Ambiente <> ambTeste) then
    raise Exception.Create(ACBrStr('Função só disponível em ambiente de Testes'));

  wTimerPagtoEnabled := tmConsultarPagto.Enabled;
  tmConsultarPagto.Enabled := False;
  try
    try
      ACBrPSPPixPDV1.PostQrSimulaPagar(edPixPDVSimularPagtoQRCodeID.Text);
      mmPixPDVSimularPagto.Lines.Text := 'Result Code: ' +
        IntToStr(ACBrPSPPixPDV1.Http.ResultCode) + sLineBreak +
        ACBrPSPPixPDV1.Http.ResultString;
    except
      On E: Exception do
        mmPixPDVSimularPagto.Lines.Add(E.Message);
    end;
  finally
    tmConsultarPagto.Enabled := wTimerPagtoEnabled;
  end;
end;

end.

