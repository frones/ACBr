{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

//{$I ACBr.inc}

unit FrPrincipal;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, DateTimePicker,
  ACBrCEP,
  ACBrPIXCD, ACBrPIXPSPItau, ACBrPIXPSPBancoDoBrasil, ACBrPIXPSPSantander,
  ACBrPIXBase, ACBrPIXSchemasPix, ACBrPIXSchemasDevolucao;

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
    btGetPixe2eidCosultar: TBitBtn;
    btGetPixConsultar: TBitBtn;
    btQREAnalisar1: TBitBtn;
    btQREColar: TBitBtn;
    btLerParametros: TBitBtn;
    btQREAnalisar: TBitBtn;
    btQREGerar: TBitBtn;
    btSalvarParametros: TBitBtn;
    cbxAmbiente: TComboBox;
    cbxItauTipoChave: TComboBox;
    cbxSantanderTipoChave: TComboBox;
    cbxNivelLog: TComboBox;
    cbxPSPAtual: TComboBox;
    cbxBBTipoChave: TComboBox;
    dtGetPixInicio: TDateTimePicker;
    dtGetPixFim: TDateTimePicker;
    edtArqLog: TEdit;
    edtGetPixTxId: TEdit;
    edtGetPixCPFCNPJ: TEdit;
    lGetPixE2eid: TLabel;
    edtItauChavePIX: TEdit;
    edtBBClientID: TEdit;
    edtQREInfoAdicional: TEdit;
    edtGetPixE2eid: TEdit;
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
    lE2eid: TLabel;
    lInicio: TLabel;
    lFim: TLabel;
    lCPFCPNJ: TLabel;
    lPagina: TLabel;
    lPagina1: TLabel;
    mGetPixE2eid: TMemo;
    mGetPix: TMemo;
    mQRE: TMemo;
    pgTestesPix: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pGetPixE2eid: TPanel;
    pGetPixE2eid1: TPanel;
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
    seGetPixPagina: TSpinEdit;
    seGetPixItensPagina: TSpinEdit;
    Splitter1: TSplitter;
    tsGetPixe2eid: TTabSheet;
    tsGetPix: TTabSheet;
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
    procedure btGetPixConsultarClick(Sender: TObject);
    procedure btGetPixe2eidCosultarClick(Sender: TObject);
    procedure btQREAnalisar1Click(Sender: TObject);
    procedure btQREAnalisarClick(Sender: TObject);
    procedure btQREGerarClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btQREColarClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure cbxPSPAtualChange(Sender: TObject);
    procedure edtBBChavePIXChange(Sender: TObject);
    procedure edtCEPChange(Sender: TObject);
    procedure edtCEPExit(Sender: TObject);
    procedure edOnlyNumbersKeyPress(Sender: TObject; var Key: char);
    procedure edtGetPixCPFCNPJChange(Sender: TObject);
    procedure edtItauChavePIXChange(Sender: TObject);
    procedure edtNomeChange(Sender: TObject);
    procedure edtSantanderChavePIXChange(Sender: TObject);
    procedure mQREChange(Sender: TObject);
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
    procedure PropPixParaLinhas(const NomePix: String; APix: TACBrPIX; SL: TStrings);

    function FormatarJSON(const AJSON: String): String;
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
  {$IfDef FPC}
   fpjson, jsonparser, jsonscanner,
  {$EndIf}
  TypInfo, IniFiles, DateUtils,
  synacode,
  ACBrDelphiZXingQRCode,
  ACBrUtil, ACBrValidador,
  ACBrPIXUtil, ACBrPIXQRCodeEstatico;

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
  pgTestesPix.ActivePageIndex := 0;

  dtGetPixInicio.DateTime := EncodeDateTime(2020,04,01,0,0,0,0);
  dtGetPixFim.DateTime := EncodeDateTime(2020,04,02,10,0,0,0);

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

procedure TForm1.btGetPixConsultarClick(Sender: TObject);
var
  Ok: Boolean;
  i: Integer;
begin
  mGetPix.Lines.Clear;
  Ok := ACBrPixCD1.PSP.epPix.ConsultarPixRecebidos( dtGetPixInicio.DateTime,
                                                    dtGetPixFim.DateTime,
                                                    edtGetPixTxId.Text,
                                                    OnlyNumber(edtGetPixCPFCNPJ.Text),
                                                    seGetPixPagina.Value,
                                                    seGetPixItensPagina.Value);
  if Ok then
  begin
    mGetPix.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.PixConsultados.AsJSON);
    mGetPix.Lines.Add('');
    mGetPix.Lines.Add('Encontrado: '+IntToStr(ACBrPixCD1.PSP.epPix.PixConsultados.pix.Count)+', documentos PIX');
    for i := 0 to ACBrPixCD1.PSP.epPix.PixConsultados.pix.Count-1 do
    begin
      mGetPix.Lines.Add('');
      PropPixParaLinhas( '  Pix['+IntToStr(i)+']',
                         ACBrPixCD1.PSP.epPix.PixConsultados.pix[i],
                         mGetPix.Lines);
    end;
  end
  else
    mGetPix.Text := ACBrPixCD1.PSP.epPix.Problema.AsJSON;
end;

procedure TForm1.btGetPixe2eidCosultarClick(Sender: TObject);
begin
  mGetPixE2eid.Lines.Clear;
  if ACBrPixCD1.PSP.epPix.ConsultarPix(edtGetPixE2eid.Text) then
  BEGIN
    mGetPixE2eid.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Pix.AsJSON);
    PropPixParaLinhas('Pix', ACBrPixCD1.PSP.epPix.Pix, mGetPixE2eid.Lines);
  end
  else
    mGetPixE2eid.Text := ACBrPixCD1.PSP.epPix.Problema.AsJSON;
end;

procedure TForm1.btQREAnalisar1Click(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TForm1.btQREAnalisarClick(Sender: TObject);
var
  qre: TACBrPIXQRCodeEstatico;
begin
  qre := TACBrPIXQRCodeEstatico.Create;
  try
    AdicionarLinhaLog('----- Analise do QRCode Estático -----');
    AdicionarLinhaLog('QrCode: '+qre.QRCode);

    qre.IgnorarErrosQRCode := True;
    qre.QRCode := mQRE.Text;
    AdicionarLinhaLog('');
    AdicionarLinhaLog('NomeRecebedor: '+qre.NomeRecebedor);
    AdicionarLinhaLog('CidadeRecebedor: '+qre.CidadeRecebedor);
    AdicionarLinhaLog('CEPRecebedor: '+qre.CEPRecebedor);
    AdicionarLinhaLog('ChavePix: '+qre.ChavePix);
    AdicionarLinhaLog('TipoChavePix: '+GetEnumName(TypeInfo(TACBrPIXTipoChave), integer(qre.TipoChavePix)));
    AdicionarLinhaLog('Valor: '+FormatFloat('0.00', qre.Valor));
    AdicionarLinhaLog('infoAdicional: '+qre.infoAdicional);
    AdicionarLinhaLog('TxId: '+qre.TxId);
    AdicionarLinhaLog('pss: '+IntToStr(qre.pss));
    AdicionarLinhaLog('mcc: '+IntToStr(qre.mcc));
  finally
    qre.Free;
  end;
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

procedure TForm1.edtGetPixCPFCNPJChange(Sender: TObject);
var
  AStr, Mascara: String;
begin
  AStr := OnlyNumber(edtGetPixCPFCNPJ.Text);
  if (Length(AStr) > 11) then
    Mascara := '**.***.***/****-**'
  else
    Mascara := '***.***.***-**';

  edtGetPixCPFCNPJ.Text := ACBrValidador.FormatarMascaraDinamica(AStr, Mascara);
  edtGetPixCPFCNPJ.SelStart := Length(edtGetPixCPFCNPJ.Text);
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

procedure TForm1.mQREChange(Sender: TObject);
begin
  btQREAnalisar.Enabled := (Trim(mQRE.Text) <> '');
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
  mQREChange(Nil);
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

procedure TForm1.PropPixParaLinhas(const NomePix: String;
  APix: TACBrPIX; SL: TStrings);
var
  i: Integer;
  NomeDev: String;
begin
  SL.Add(NomePix+'.endToEndId: '+APix.endToEndId);
  SL.Add(NomePix+'.TxId: '+APix.txid);
  SL.Add(NomePix+'.valor: '+FormatFloatBr(APix.valor));
  SL.Add(NomePix+'.componentesValor.original.valor: '+FormatFloatBr(APix.componentesValor.original.valor));
  SL.Add(NomePix+'.componentesValor.saque.valor: '+FormatFloatBr(APix.componentesValor.saque.valor));
  SL.Add(NomePix+'.componentesValor.troco.valor: '+FormatFloatBr(APix.componentesValor.troco.valor));
  SL.Add(NomePix+'.componentesValor.juros.valor: '+FormatFloatBr(APix.componentesValor.juros.valor));
  SL.Add(NomePix+'.componentesValor.multa.valor: '+FormatFloatBr(APix.componentesValor.multa.valor));
  SL.Add(NomePix+'.componentesValor.abatimento.valor: '+FormatFloatBr(APix.componentesValor.abatimento.valor));
  SL.Add(NomePix+'.componentesValor.desconto.valor: '+FormatFloatBr(APix.componentesValor.desconto.valor));
  SL.Add(NomePix+'.chave: '+APix.chave);
  SL.Add(NomePix+'.horario: '+FormatDateTimeBr(APix.horario));
  SL.Add(NomePix+'.infoPagador: '+APix.infoPagador);
  SL.Add(NomePix+'.devolucoes: '+IntToStr(APix.devolucoes.Count) );

  for i := 0 to APix.devolucoes.Count-1 do
  begin
    NomeDev := NomePix+'devolucoes['+IntToStr(i)+']';
    SL.Add(NomeDev+'.valor: '+FormatFloatBr(APix.devolucoes[i].valor));
    SL.Add(NomeDev+'.natureza: '+PIXNaturezaDevolucaoToString(APix.devolucoes[i].natureza));
    SL.Add(NomeDev+'.descricao: '+APix.devolucoes[i].descricao);
    SL.Add(NomeDev+'.id: '+APix.devolucoes[i].id);
    SL.Add(NomeDev+'.rtrId: '+APix.devolucoes[i].rtrId);
    SL.Add(NomeDev+'.horario.solicitacao: '+FormatDateTimeBr(APix.devolucoes[i].horario.solicitacao));
    SL.Add(NomeDev+'.horario.liquidacao: '+FormatDateTimeBr(APix.devolucoes[i].horario.liquidacao));
    SL.Add(NomeDev+'.status: '+ PIXStatusDevolucaoToString(APix.devolucoes[i].status));
    SL.Add(NomeDev+'.motivo: '+APix.devolucoes[i].motivo);
  end;
end;

function TForm1.FormatarJSON(const AJSON: String): String;
{$IfDef FPC}
var
  jpar: TJSONParser;
{$EndIf}
begin
  Result := AJSON;
  {$IfDef FPC}
   jpar :=TJSONParser.Create(AJSON, [joUTF8]);
   try
     Result := jpar.Parse.FormatJSON([], 2);
   finally
     jpar.Free;
   end;
  {$EndIf}
end;

end.

