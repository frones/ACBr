unit FrPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, ACBrPIXCD, ACBrPIXPSPItau, ACBrPIXPSPBancoDoBrasil,
  ACBrPIXPSPSantander, ACBrPIXBase, ACBrCEP, ACBrBase;

const
  CURL_ACBR = 'https://projetoacbr.com.br/tef/';
  CURL_MCC = 'https://classification.codes/classifications/industry/mcc/';

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrCEP1: TACBrCEP;
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPBancoDoBrasil1: TACBrPSPBancoDoBrasil;
    ACBrPSPItau1: TACBrPSPItau;
    ACBrPSPSantander1: TACBrPSPSantander;
    btQREGerar: TBitBtn;
    btQREColar: TBitBtn;
    btLerParametros: TBitBtn;
    btSalvarParametros: TBitBtn;
    cbxAmbiente: TComboBox;
    cbxItauTipoChave: TComboBox;
    cbxSantanderTipoChave: TComboBox;
    cbxNivelLog: TComboBox;
    cbxPSPAtual: TComboBox;
    cbxBBTipoChave: TComboBox;
    edtArqLog: TEdit;
    edtItauChavePIX: TEdit;
    edtBBClientID: TEdit;
    edtQREInfoAdicional: TEdit;
    edtQRETxId: TEdit;
    edtSantanderChavePIX: TEdit;
    edtItauClientID: TEdit;
    edtBBClientSecret: TEdit;
    edtSantanderConsumerKey: TEdit;
    edtItauClientSecret: TEdit;
    edtBBDevAppKey: TEdit;
    edtSantanderConsumerSecret: TEdit;
    edtItauXCorrelationId: TEdit;
    edtCEP: TEdit;
    edtCidade: TEdit;
    edtBBChavePIX: TEdit;
    fleQREValor: TFloatSpinEdit;
    imgErrNome: TImage;
    imgErrPSP: TImage;
    imgQRE: TImage;
    imgBBErroChavePIX: TImage;
    imgItauErroChavePIX: TImage;
    imgSantanderErroChavePIX: TImage;
    Label34: TLabel;
    Label35: TLabel;
    mQRE: TMemo;
    Panel1: TPanel;
    pQREMemo: TPanel;
    pQREGerado: TPanel;
    pQREDados: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    pConfPSPBB: TPanel;
    pBotoesConfiguracao: TPanel;
    pConfPSPBB1: TPanel;
    pConfPSPBB2: TPanel;
    seMCC: TSpinEdit;
    edtNome: TEdit;
    edtProxyHost: TEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    gbLog: TGroupBox;
    gbProxy: TGroupBox;
    gbPSP: TGroupBox;
    gbRecebedor: TGroupBox;
    ImageList1: TImageList;
    imgErrCEP: TImage;
    imgInfoMCC: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label36: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lURLTEF: TLabel;
    mLog: TMemo;
    pConfPIX: TPanel;
    pgConfPixPSP: TPageControl;
    pgPSPs: TPageControl;
    pgTestes: TPageControl;
    pgTesteEndPoints: TPageControl;
    pgPrincipal: TPageControl;
    pLogs: TPanel;
    sbArqLog: TSpeedButton;
    sbConsultaCEP: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    seProxyPorta: TSpinEdit;
    seTimeout: TSpinEdit;
    Splitter1: TSplitter;
    tsEndPointPix: TTabSheet;
    tsEndPointCob: TTabSheet;
    tsEndPointCobV: TTabSheet;
    tsEndPoints: TTabSheet;
    tsQRCodeEstatico: TTabSheet;
    tsShipay: TTabSheet;
    tsBB: TTabSheet;
    tsItau: TTabSheet;
    tsSantander: TTabSheet;
    tsPSP: TTabSheet;
    tsPIX: TTabSheet;
    tsTestes: TTabSheet;
    tsConfiguracao: TTabSheet;
    Valor: TLabel;
    procedure ACBrPixCD1QuandoGravarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure btQREGerarClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btQREColarClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure cbxPSPAtualChange(Sender: TObject);
    procedure edtBBChavePIXChange(Sender: TObject);
    procedure edtCEPChange(Sender: TObject);
    procedure edtCEPExit(Sender: TObject);
    procedure edOnlyNumbersKeyPress(Sender: TObject; var Key: char);
    procedure edtItauChavePIXChange(Sender: TObject);
    procedure edtNomeChange(Sender: TObject);
    procedure edtSantanderChavePIXChange(Sender: TObject);
    procedure pgPrincipalChange(Sender: TObject);
    procedure QuandoMudarDadosQRCode(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgInfoMCCClick(Sender: TObject);
    procedure lURLTEFClick(Sender: TObject);
    procedure sbArqLogClick(Sender: TObject);
    procedure sbConsultaCEPClick(Sender: TObject);
    procedure sbVerSenhaProxyClick(Sender: TObject);
  private
    function GetNomeArquivoConfiguracao: String;
    procedure AdicionarLinhaLog(AMensagem: String);
    procedure TratarException(Sender : TObject; E : Exception);

    procedure LigarAlertasdeErrosDeConfiguracao;
    procedure VerificarConfiguracaoPIXCD;

    procedure ConfigurarACBrPIXCD;
    procedure ConfigurarACBrPSPs;

    procedure LimparQRCodeEstatico;
    procedure PintarQRCodeEstatico;
    procedure PintarQRCode(const Dados: String; ABMP: TBitmap);

  public
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;

  end;

var
  Form1: TForm1;

implementation

uses
  TypInfo, IniFiles,
  synacode,
  ACBrDelphiZXingQRCode,
  ACBrUtil, ACBrValidador, ACBrPIXUtil;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  j: TACBrPixCDAmbiente;
  k: TACBrPIXTipoChave;
begin
  cbxPSPAtual.Items.Clear;
  For i := 0 to pgPSPs.PageCount-1 do
     cbxPSPAtual.Items.Add( pgPSPs.Pages[i].Caption );

  cbxAmbiente.Items.Clear;
  For j := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
     cbxAmbiente.Items.Add( GetEnumName(TypeInfo(TACBrPixCDAmbiente), integer(j) ));

  cbxBBTipoChave.Items.Clear;
  For k := Low(TACBrPIXTipoChave) to High(TACBrPIXTipoChave) do
     cbxBBTipoChave.Items.Add( GetEnumName(TypeInfo(TACBrPIXTipoChave), integer(k) ));
  cbxItauTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbxSantanderTipoChave.Items.Assign(cbxBBTipoChave.Items);

  Application.OnException := @TratarException;

  ImageList1.GetBitmap(5, imgInfoMCC.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrNome.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrCEP.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrPSP.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgBBErroChavePIX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroChavePIX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgSantanderErroChavePIX.Picture.Bitmap);

  pgPrincipal.ActivePageIndex := 0;
  pgConfPixPSP.ActivePageIndex := 0;
  pgPSPs.ActivePageIndex := 0;
  pgTestes.ActivePageIndex := 0;

  LerConfiguracao;
end;

procedure TForm1.imgInfoMCCClick(Sender: TObject);
begin
  OpenURL(CURL_MCC);
end;

procedure TForm1.lURLTEFClick(Sender: TObject);
begin
  OpenURL(CURL_ACBR);
end;

procedure TForm1.sbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if (Trim(edtArqLog.Text) = '') then
  begin
    MessageDlg('Arquivo de Log não informado', mtError, [mbOK], 0);
    Exit;
  end;

  if pos(PathDelim,edtArqLog.Text) = 0 then
    AFileLog := ApplicationPath + edtArqLog.Text
  else
    AFileLog := edtArqLog.Text;

  if not FileExists(AFileLog) then
    MessageDlg('Arquivo '+AFileLog+' não encontrado', mtError, [mbOK], 0)
  else
    OpenURL(AFileLog);
end;

procedure TForm1.sbConsultaCEPClick(Sender: TObject);
var
  EndAchado: TACBrCEPEndereco;
begin
  try
    ACBrCEP1.BuscarPorCEP(OnlyNumber(edtCEP.Text));
    if (ACBrCEP1.Enderecos.Count > 0) then
    begin
      EndAchado := ACBrCEP1.Enderecos[0];
      edtCidade.Text := EndAchado.Municipio;
    end;
  except
    MessageDlg('Erro ao executar Consulta do CEP', mtError, [mbOK], 0);
  end;
end;

procedure TForm1.sbVerSenhaProxyClick(Sender: TObject);
begin
  if sbVerSenhaProxy.Down then
    edtProxySenha.EchoMode := emNormal
  else
    edtProxySenha.EchoMode := emPassword;
end;

function TForm1.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName,'.ini');
end;

procedure TForm1.edtCEPChange(Sender: TObject);
begin
  if (Length(edtCEP.Text) > 5) then
  begin
    edtCEP.Text := FormatarMascaraDinamica(OnlyNumber(edtCEP.Text), '*****-***');
    edtCEP.SelStart := Length(edtCEP.Text);
  end;

  imgErrCEP.Visible := (Length(edtCEP.Text) < 9);
  sbConsultaCEP.Visible := not imgErrCEP.Visible;
end;

procedure TForm1.ACBrPixCD1QuandoGravarLog(const ALogLine: String; var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TForm1.btQREGerarClick(Sender: TObject);
begin
  VerificarConfiguracaoPIXCD;
  PintarQRCodeEstatico;
end;

procedure TForm1.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TForm1.btQREColarClick(Sender: TObject);
begin
  mQRE.CopyToClipboard;
end;

procedure TForm1.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TForm1.cbxPSPAtualChange(Sender: TObject);
begin
  imgErrPSP.Visible := (cbxPSPAtual.ItemIndex < 1);
end;

procedure TForm1.edtCEPExit(Sender: TObject);
begin
  if (not imgErrCEP.Visible) and (edtCidade.Text = '') then
    sbConsultaCEP.Click;
end;

procedure TForm1.edOnlyNumbersKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet( Key, [#8,#13,'0'..'9'] ) then
    Key := #0;
end;

procedure TForm1.edtBBChavePIXChange(Sender: TObject);
begin
  cbxBBTipoChave.ItemIndex := Integer(DetectarTipoChave(edtBBChavePIX.Text));
  imgBBErroChavePIX.Visible := (edtBBChavePIX.Text <> '') and (cbxBBTipoChave.ItemIndex = 0);
end;

procedure TForm1.edtItauChavePIXChange(Sender: TObject);
begin
  cbxItauTipoChave.ItemIndex := Integer(DetectarTipoChave(edtItauChavePIX.Text));
  imgItauErroChavePIX.Visible := (edtItauChavePIX.Text <> '') and (cbxItauTipoChave.ItemIndex = 0);
end;

procedure TForm1.edtNomeChange(Sender: TObject);
begin
  imgErrNome.Visible := (Length(Trim(edtNome.Text)) < 5);
end;

procedure TForm1.edtSantanderChavePIXChange(Sender: TObject);
begin
  cbxSantanderTipoChave.ItemIndex := Integer(DetectarTipoChave(edtSantanderChavePIX.Text));
  imgSantanderErroChavePIX.Visible := (edtSantanderChavePIX.Text <> '') and (cbxSantanderTipoChave.ItemIndex = 0);
end;

procedure TForm1.pgPrincipalChange(Sender: TObject);
begin
  if (pgPrincipal.ActivePageIndex = 0) and btSalvarParametros.Enabled then
  begin
    GravarConfiguracao;
    AplicarConfiguracao;
  end;

  btSalvarParametros.Enabled := (pgPrincipal.ActivePageIndex = 1);
end;

procedure TForm1.QuandoMudarDadosQRCode(Sender: TObject);
begin
  LimparQRCodeEstatico;
end;

procedure TForm1.AdicionarLinhaLog(AMensagem: String);
begin
  mLog.Lines.Add(AMensagem);
end;

procedure TForm1.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');

  if pgPrincipal.ActivePage = tsConfiguracao then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracao;
begin
  edtNomeChange(Nil);
  edtCEPChange(Nil);
  cbxPSPAtualChange(Nil);
  edtBBChavePIXChange(Nil);
  edtItauChavePIXChange(Nil);
  edtSantanderChavePIXChange(Nil);
end;

procedure TForm1.VerificarConfiguracaoPIXCD;
begin
  if imgErrNome.Visible or imgErrCEP.Visible or imgErrPSP.Visible then
  begin
    pgPrincipal.ActivePageIndex := 1;
    pgConfPixPSP.ActivePageIndex := 0;
    MessageDlg('Favor configurar os campos sinalizados', mtWarning, [mbOK], 0);
    Abort;
  end;
end;

procedure TForm1.LerConfiguracao;
Var
  Ini : TIniFile ;
begin
  AdicionarLinhaLog('- LerConfiguracao: '+NomeArquivoConfiguracao);
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    edtNome.Text := Ini.ReadString('Recebedor', 'Nome', '');
    edtCEP.Text := Ini.ReadString('Recebedor', 'CEP', '');
    edtCidade.Text := Ini.ReadString('Recebedor', 'Cidade', '');
    seMCC.Value := Ini.ReadInteger('Recebedor', 'MCC', 0);

    cbxPSPAtual.ItemIndex := Ini.ReadInteger('PIX','PSP', 0);
    cbxAmbiente.ItemIndex := Ini.ReadInteger('PIX','Ambiente', 0);
    seTimeout.Value := Ini.ReadInteger('PIX', 'TimeOut', ChttpTimeOutDef);

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    seProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := StrCrypt(DecodeBase64(Ini.ReadString('Proxy', 'Pass', '')), CURL_ACBR);

    edtArqLog.Text := Ini.ReadString('Log', 'Arquivo', '');
    cbxNivelLog.ItemIndex := Ini.ReadInteger('Log', 'Nivel', 1);

    edtBBChavePIX.Text := Ini.ReadString('BancoBrasil', 'ChavePIX', '');
    edtBBClientID.Text := Ini.ReadString('BancoBrasil', 'ClientID', '');
    edtBBClientSecret.Text := Ini.ReadString('BancoBrasil', 'ClientSecret', '');
    edtBBDevAppKey.Text := Ini.ReadString('BancoBrasil', 'DeveloperApplicationKey', '');

    edtItauChavePIX.Text := Ini.ReadString('Itau', 'ChavePIX', '');
    edtItauClientID.Text := Ini.ReadString('Itau', 'ClientID', '');
    edtItauClientSecret.Text := Ini.ReadString('Itau', 'ClientSecret', '');
    edtItauXCorrelationId.Text := Ini.ReadString('Itau', 'XCorrelationId', '');

    edtSantanderChavePIX.Text := Ini.ReadString('Santander', 'ChavePIX', '');
    edtSantanderConsumerKey.Text := Ini.ReadString('Santander', 'ConsumerKey', '');
    edtSantanderConsumerSecret.Text := Ini.ReadString('Santander', 'ConsumerSecret', '');

  finally
     Ini.Free ;
  end ;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TForm1.GravarConfiguracao;
Var
  Ini : TIniFile ;
begin
  AdicionarLinhaLog('- LerConfiguracao: '+NomeArquivoConfiguracao);
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    Ini.WriteString('Recebedor', 'Nome', edtNome.Text);
    Ini.WriteString('Recebedor', 'CEP', edtCEP.Text);
    Ini.WriteString('Recebedor', 'Cidade', edtCidade.Text);
    Ini.WriteInteger('Recebedor', 'MCC', seMCC.Value);

    Ini.WriteInteger('PIX','PSP', cbxPSPAtual.ItemIndex);
    Ini.WriteInteger('PIX','Ambiente', cbxAmbiente.ItemIndex);
    Ini.WriteInteger('PIX', 'TimeOut', seTimeout.Value);

    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', seProxyPorta.Text);
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', EncodeBase64(StrCrypt(edtProxySenha.Text, CURL_ACBR)) );

    Ini.WriteString('Log', 'Arquivo', edtArqLog.Text);
    Ini.WriteInteger('Log', 'Nivel', cbxNivelLog.ItemIndex);

    Ini.WriteString('BancoBrasil', 'ChavePIX', edtBBChavePIX.Text);
    Ini.WriteString('BancoBrasil', 'ClientID', edtBBClientID.Text);
    Ini.WriteString('BancoBrasil', 'ClientSecret', edtBBClientSecret.Text);
    Ini.WriteString('BancoBrasil', 'DeveloperApplicationKey', edtBBDevAppKey.Text);

    Ini.WriteString('Itau', 'ChavePIX', edtItauChavePIX.Text);
    Ini.WriteString('Itau', 'ClientID', edtItauClientID.Text);
    Ini.WriteString('Itau', 'ClientSecret', edtItauClientSecret.Text);
    Ini.WriteString('Itau', 'XCorrelationId', edtItauXCorrelationId.Text);

    Ini.WriteString('Santander', 'ChavePIX', edtSantanderChavePIX.Text);
    Ini.WriteString('Santander', 'ConsumerKey', edtSantanderConsumerKey.Text);
    Ini.WriteString('Santander', 'ConsumerSecret', edtSantanderConsumerSecret.Text);
  finally
     Ini.Free ;
  end ;

end;

procedure TForm1.AplicarConfiguracao;
begin
  AdicionarLinhaLog('- AplicarConfiguracao');
  ConfigurarACBrPIXCD;
  ConfigurarACBrPSPs;
end;

procedure TForm1.ConfigurarACBrPIXCD;
begin
  AdicionarLinhaLog('  - ConfigurarACBrPIXCD');
  ACBrPixCD1.Recebedor.Nome := edtNome.Text;
  ACBrPixCD1.Recebedor.CEP := edtCEP.Text;
  ACBrPixCD1.Recebedor.Cidade := edtCidade.Text;
  ACBrPixCD1.Recebedor.CodCategoriaComerciante := seMCC.Value;

  ACBrPixCD1.Ambiente := TACBrPixCDAmbiente(cbxAmbiente.ItemIndex);
  ACBrPixCD1.TimeOut := seTimeout.Value;

  ACBrPixCD1.Proxy.Host := edtProxyHost.Text;
  ACBrPixCD1.Proxy.Port := seProxyPorta.Text;
  ACBrPixCD1.Proxy.User := edtProxyUser.Text;
  ACBrPixCD1.Proxy.Pass := edtProxySenha.Text;

  ACBrPixCD1.ArqLOG := edtArqLog.Text;
  ACBrPixCD1.NivelLog := cbxNivelLog.ItemIndex;

  case cbxPSPAtual.ItemIndex of
    //0: ACBrPixCD1.PSP := ACBrPSPShiPay1;
    1: ACBrPixCD1.PSP := ACBrPSPBancoDoBrasil1;
    2: ACBrPixCD1.PSP := ACBrPSPItau1;
    3: ACBrPixCD1.PSP := ACBrPSPSantander1;
  else
    raise Exception.Create('PSP configurado é inválido');
  end;
end;

procedure TForm1.ConfigurarACBrPSPs;
begin
  AdicionarLinhaLog('  - ConfigurarACBrPSPs');
  ACBrPSPBancoDoBrasil1.ChavePIX := edtBBChavePIX.Text;
  ACBrPSPBancoDoBrasil1.ClientID := edtBBClientID.Text;
  ACBrPSPBancoDoBrasil1.ClientSecret := edtBBClientSecret.Text;
  ACBrPSPBancoDoBrasil1.DeveloperApplicationKey := edtBBDevAppKey.Text;

  ACBrPSPItau1.ChavePIX := edtItauChavePIX.Text;
  ACBrPSPItau1.ClientID := edtItauClientID.Text;
  ACBrPSPItau1.ClientSecret := edtItauClientSecret.Text;
  ACBrPSPItau1.xCorrelationID := edtItauXCorrelationId.Text;

  ACBrPSPSantander1.ChavePIX := edtItauChavePIX.Text;
  ACBrPSPSantander1.ConsumerKey := edtSantanderConsumerKey.Text;
  ACBrPSPSantander1.ConsumerSecret := edtSantanderConsumerSecret.Text;
end;

procedure TForm1.LimparQRCodeEstatico;
begin
  mQRE.Text := '';
  imgQRE.Picture.Clear;
end;

procedure TForm1.PintarQRCodeEstatico;
begin
  mQRE.Text := ACBrPixCD1.GerarQRCodeEstatico(fleQREValor.Value, edtQREInfoAdicional.Text, edtQRETxId.Text);
  PintarQRCode(mQRE.Text, imgQRE.Picture.Bitmap);
end;

procedure TForm1.PintarQRCode(const Dados: String; ABMP: TBitmap);
var
  QRCode: TDelphiZXingQRCode;
  QRCodeBitmap: TBitmap;
  Row, Column: Integer;
begin
  QRCode := TDelphiZXingQRCode.Create;
  QRCodeBitmap := TBitmap.Create;
  try
    QRCode.Encoding  := qrUTF8BOM;
    QRCode.QuietZone := 2;
    QRCode.Data      := widestring(Dados);

    QRCodeBitmap.Width  := QRCode.Columns;
    QRCodeBitmap.Height := QRCode.Rows;

    for Row := 0 to QRCode.Rows - 1 do
    begin
      for Column := 0 to QRCode.Columns - 1 do
      begin
        if (QRCode.IsBlack[Row, Column]) then
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clBlack
        else
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;
      end;
    end;

    ABMP.Assign(QRCodeBitmap);
  finally
    QRCode.Free;
    QRCodeBitmap.Free;
  end;
end;


end.

