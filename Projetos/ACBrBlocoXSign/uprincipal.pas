unit uPrincipal;

{$mode objfpc}{$H+}
{$r BannerACBrSAC.rc}

interface

uses
  Windows, Classes, SysUtils, strutils, dateutils, IniFiles, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, MaskEdit, Menus,
  ComCtrls, Spin, EditBtn, UtilUnit, ACBrGIF, ACBrUtil, ACBrEnterTab, ACBrDFe,
  ACBrDFeSSL, ACBrDFeWebService, ACBrBlocoX, ACBrDFeUtil;

const
  _C = 'tYk*5W@';

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    ACBrBlocoX1: TACBrBlocoX;
    ACBrEnterTab1: TACBrEnterTab;
    ACBrGIF1: TACBrGIF;
    Bevel2: TBevel;
    btnBuscarCertificado: TSpeedButton;
    btnConsultar: TBitBtn;
    btnValidar: TBitBtn;
    btnBuscarArquivo: TSpeedButton;
    btnCriarAssinatura: TBitBtn;
    btnSalvarArquivo: TBitBtn;
    btnTransmitir: TBitBtn;
    btTransmitirArq: TButton;
    btCancelarArq: TButton;
    btConsultarHistArq: TButton;
    btConsultarPendContrib: TButton;
    btConsultarPendDesenvolvedor: TButton;
    btConsultarProcessArq: TButton;
    btDownloadArq: TButton;
    btListarArquivos: TButton;
    btReprocessarArq: TButton;
    edProxyHost: TEdit;
    edProxyPorta: TSpinEdit;
    edProxySenha: TEdit;
    edProxyUser: TEdit;
    edtArqBlocoX: TEdit;
    edtCertificado: TEdit;
    edtSenhaCertificado: TEdit;
    gbProxy: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    lblProxyPorta: TLabel;
    lblProxyUser: TLabel;
    lblProxySenha: TLabel;
    lblProxyHost: TLabel;
    memArqAssinado: TMemo;
    mmRetornoBlocoX: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnComandos: TPanel;
    rbtTipoCapicom: TRadioButton;
    rbtTipoOpenSSL: TRadioButton;
    rgTipo: TRadioGroup;
    SaveDialog1: TSaveDialog;
    tsWSRecepcao: TTabSheet;
    tsConfiguracao: TTabSheet;
    tsWSBlocoX: TTabSheet;
    procedure ACBrGIF1Click(Sender: TObject);
    procedure btCancelarArqClick(Sender: TObject);
    procedure btConsultarHistArqClick(Sender: TObject);
    procedure btConsultarPendContribClick(Sender: TObject);
    procedure btConsultarPendDesenvolvedorClick(Sender: TObject);
    procedure btConsultarProcessArqClick(Sender: TObject);
    procedure btDownloadArqClick(Sender: TObject);
    procedure btListarArquivosClick(Sender: TObject);
    procedure btnBuscarArquivoClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnCriarAssinaturaClick(Sender: TObject);
    procedure btnSalvarArquivoClick(Sender: TObject);
    procedure btnTransmitirClick(Sender: TObject);
    procedure btnValidarClick(Sender: TObject);
    procedure btReprocessarArqClick(Sender: TObject);
    procedure btTransmitirArqClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure LerConfiguracoes;
    procedure btnBuscarCertificadoClick(Sender: TObject);
  private
    procedure ConfigurarDFe;
    function GetPathConfig: String;
    function GetXML: AnsiString;
    procedure GravarConfiguracoes;
    procedure CarregarGifBannerACBrSAC;
    function ValidarArquivo : Boolean;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

Uses
  ACBrBlocoX_WebServices, synautil;

const
  TIPO_CAPICOM = 'CAPICOM';
  TIPO_OPENSSL = 'OPENSSL';

{$R *.lfm}

{ TfrmPrincipal }

procedure TfrmPrincipal.CarregarGifBannerACBrSAC;
var
  S: TResourceStream;
begin
  S := TResourceStream.Create(HInstance, 'BANNER_ACBrSAC', RT_RCDATA);
  try
    ACBrGIF1.LoadFromStream(S);
    ACBrGIF1.Active := True;
  finally
    S.Free;
  end;
end;

function TfrmPrincipal.ValidarArquivo: Boolean;
begin
  Result := True;

  if memArqAssinado.Text = '' then
  begin
    MessageDlg('Erro','Arquivo Vazio',mtError,[mbOK],0);
    Result := False;
  end;

  if not XmlEstaAssinado(memArqAssinado.Text) then
  begin
    MessageDlg('Erro','Arquivo sem assinatura',mtError,[mbOK],0);
    Result := False;
  end;
end;

function TfrmPrincipal.GetPathConfig: String;
begin
  Result := ExtractFilePath(ParamStr(0)) + ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
end;

function TfrmPrincipal.GetXML: AnsiString;
var
  wArqXml: String;
  wFS: TFileStream;
begin
  Result := EmptyStr;
  OpenDialog1.Title      := 'Selecione o XML';
  OpenDialog1.DefaultExt := '.xml';
  OpenDialog1.Filter     := 'Arquivos XML|*.xml';
  OpenDialog1.InitialDir := ApplicationPath;

  if OpenDialog1.Execute then
  begin
    wArqXml := OpenDialog1.FileName;
    if (wArqXml = EmptyStr) then
      Exit;

    if (not FileExists(wArqXml)) then
      raise Exception.Create('Arquivo não encontrado');

    wFS := TFileStream.Create(wArqXml, fmOpenRead);
    try
      wFS.Position := 0;
      Result := ReadStrFromStream(wFS, wFS.Size);
    finally
      wFS.Free;
    end;
  end;
end;

procedure TfrmPrincipal.ConfigurarDFe;
begin
  if rbtTipoCapicom.Checked then
  begin
    ACBrBlocoX1.Configuracoes.Geral.SSLLib := libCapicom;
    ACBrBlocoX1.Configuracoes.Certificados.NumeroSerie := edtCertificado.Text;
  end
  else
  begin
    ACBrBlocoX1.Configuracoes.Geral.SSLLib            := libOpenSSL;
    ACBrBlocoX1.Configuracoes.Certificados.ArquivoPFX := edtCertificado.Text;
  end;
  ACBrBlocoX1.Configuracoes.Certificados.Senha := edtSenhaCertificado.Text;
  ACBrBlocoX1.Configuracoes.WebServices.ProxyHost := edProxyHost.Text;
  ACBrBlocoX1.Configuracoes.WebServices.ProxyPort := edProxyPorta.Text;
  ACBrBlocoX1.Configuracoes.WebServices.ProxyUser := edProxyUser.Text;
  ACBrBlocoX1.Configuracoes.WebServices.ProxyPass := edProxySenha.Text;
end;

procedure TfrmPrincipal.GravarConfiguracoes;
var
  F: TIniFile;
begin
  F := TIniFile.Create(GetPathConfig);
  try
    F.WriteString('CONFIG', 'Certificado', edtCertificado.Text);
    F.WriteString('CONFIG', 'UltimoArquivo', edtArqBlocoX.Text);
    F.WriteString('CONFIG', 'Tipo', IfThen(rbtTipoCapicom.Checked, TIPO_CAPICOM, TIPO_OPENSSL));
    GravaINICrypt(F, 'Certificado', 'Senha', edtSenhaCertificado.Text, _C);
    F.WriteString('Proxy', 'Host', edProxyHost.Text);
    F.WriteString('Proxy', 'Porta', edProxyPorta.Text);
    F.WriteString('Proxy', 'User', edProxyUser.Text);
    GravaINICrypt(F, 'Proxy', 'Senha', edProxySenha.Text, _C);
  finally
    F.Free;
  end;
end;

procedure TfrmPrincipal.LerConfiguracoes;
var
  F: TIniFile;
begin
  F := TIniFile.Create(GetPathConfig);
  try
    edtCertificado.Text      := F.ReadString('CONFIG', 'Certificado', '');
    edtArqBlocoX.Text        := F.ReadString('CONFIG', 'UltimoArquivo', '');
    rbtTipoCapicom.Checked   := F.ReadString('CONFIG', 'Tipo', TIPO_CAPICOM) = TIPO_CAPICOM;
    rbtTipoOpenSSL.Checked   := F.ReadString('CONFIG', 'Tipo', TIPO_CAPICOM) = TIPO_OPENSSL;
    edtSenhaCertificado.Text := LeINICrypt(F, 'Certificado', 'Senha', _C);
    edProxyHost.Text         := F.ReadString('Proxy', 'Host', '');
    edProxyPorta.Value       := F.ReadInteger('Proxy', 'Porta', 0);
    edProxyUser.Text         := F.ReadString('Proxy', 'User', '');
    edProxySenha.Text        := LeINICrypt(F, 'Proxy', 'Senha', _C);
  finally
    F.Free;
  end;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  fArquivo: TStringList;
begin
  edtCertificado.Clear;
  edtArqBlocoX.Clear;
  memArqAssinado.Clear;

  LerConfiguracoes;

  ConfigurarDFe;

  if (UpperCase(ParamStr(2)) = '/E') or
     (UpperCase(ParamStr(2)) = '/V') then
  begin
    if FilesExists(ParamStr(1)) then
    begin
      try
        fArquivo := TStringList.Create;
        fArquivo.LoadFromFile(ParamStr(1));
        if Pos('</reducaoz>',LowerCase(fArquivo.Text)) > 0 then
          memArqAssinado.Text := ACBrBlocoX1.SSL.Assinar(fArquivo.Text, 'ReducaoZ', 'Mensagem')
        else if Pos('</estoque>',LowerCase(fArquivo.Text)) > 0 then
          memArqAssinado.Text := ACBrBlocoX1.SSL.Assinar(fArquivo.Text, 'Estoque', 'Mensagem')
        else
          ShowMessage('Arquivo não reconhecido');
      finally
        fArquivo.Free;
      end;

      if (UpperCase(ParamStr(2)) = '/E') then
        btnTransmitirClick(Self)
      else
        btnValidarClick(Self);

      WriteToTXT(ExtractFileNameWithoutExt(ParamStr(1))+'-resposta.'+ExtractFileExt(ParamStr(1)),memArqAssinado.Text);
      Application.Terminate;
    end
    else
    begin
      WriteToTXT(ExtractFileNameWithoutExt(ParamStr(1))+'-resposta.'+ExtractFileExt(ParamStr(1)),'Arquivo Inválido');
      Application.Terminate;
    end;
  end
  else if UpperCase(ParamStr(2)) = '/C' then
  begin
    ACBrBlocoX1.WebServices.ConsultarBlocoX.Recibo := ParamStr(1);
    ACBrBlocoX1.WebServices.ConsultarBlocoX.Executar;

    memArqAssinado.Text := ACBrBlocoX1.WebServices.ConsultarBlocoX.RetWS;
    WriteToTXT(ExtractFileNameWithoutExt(ParamStr(1))+'consultar-resposta.'+ExtractFileExt(ParamStr(1)),memArqAssinado.Text);
    Application.Terminate;
  end
  else
     CarregarGifBannerACBrSAC;;
end;

procedure TfrmPrincipal.Image2Click(Sender: TObject);
begin
  OpenURL('http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html');
end;

procedure TfrmPrincipal.btnBuscarCertificadoClick(Sender: TObject);
begin
  if rbtTipoCapicom.Checked then
    edtCertificado.Text := ACBrBlocoX1.SSL.SelecionarCertificado
  else
  begin
    OpenDialog1.DefaultExt :=  '*.pfx';
    OpenDialog1.Filter:= 'Arquivos de certificado|*.pfx';
    if OpenDialog1.Execute then
      edtCertificado.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmPrincipal.ACBrGIF1Click(Sender: TObject);
begin
  OpenURL('http://www.projetoacbr.com.br/forum/SAC/cadastro/');
end;

procedure TfrmPrincipal.btCancelarArqClick(Sender: TObject);
var
  wXML: AnsiString;
  wMotivo, wRecibo: String;
begin
  if not InputQuery('Consultar Processamento', 'Informe o número do Recibo', wRecibo) then
    Exit;

  if not InputQuery('Consultar Processamento', 'Informe o Motivo do cancelamento', wMotivo) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.CancelarArquivo do
  begin
    Recibo := wRecibo;
    Motivo := wMotivo;
    RemoverEncodingXMLAssinado := True;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.txt', wXML);
  end;

  with ACBrBlocoX1.WebServices.CancelarArquivoBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btConsultarHistArqClick(Sender: TObject);
var
  wRecibo: String;
  wXML: AnsiString;
begin
  if not InputQuery('Consultar Processamento', 'Informe o número do Recibo', wRecibo) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.ConsultarHistoricoArquivo do
  begin
    Recibo := wRecibo;
    RemoverEncodingXMLAssinado := False;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.xml', wXML);
  end;

  with ACBrBlocoX1.WebServices.ConsultarHistoricoArquivoBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btConsultarPendContribClick(Sender: TObject);
var
  wIE: String;
  wXML: AnsiString;
begin
  if not InputQuery('Consultar Pendencias Contribuinte', 'Informe a Inscrição Estadual', wIE) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.ConsultarPendenciasContribuinte do
  begin
    InscricaoEstadual := wIE;
    RemoverEncodingXMLAssinado := True;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.txt', wXML);
  end;

  with ACBrBlocoX1.WebServices.ConsultarPendenciasContribuinteBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btConsultarPendDesenvolvedorClick(Sender: TObject);
var
  wCNPJ: String;
  wXML: AnsiString;
begin
  if not InputQuery('Consultar Pend. Desenvolvedor PAF-ECF', 'Informe o CNPJ', wCNPJ) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.ConsultarPendenciasDesenvolvedorPafEcf do
  begin
    CNPJ := wCNPJ;
    RemoverEncodingXMLAssinado := True;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.xml', wXML);
  end;

  with ACBrBlocoX1.WebServices.ConsultarPendenciasDesenvolvedorPafEcfBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btConsultarProcessArqClick(Sender: TObject);
var
  wRecibo: String;
  wXML: AnsiString;
begin
  if not InputQuery('Consultar Processamento', 'Informe o número do Recibo', wRecibo) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.ConsultarProcessamentoArquivo do
  begin
    Recibo := wRecibo;
    RemoverEncodingXMLAssinado := True;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.txt', wXML);
  end;

  with ACBrBlocoX1.WebServices.ConsultarProcessamentoArquivoBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btDownloadArqClick(Sender: TObject);
var
  wRecibo: String;
  wXML: AnsiString;
begin
  if not InputQuery('Consultar Processamento', 'Informe o número do Recibo', wRecibo) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.DownloadArquivo do
  begin
    Recibo := wRecibo;
    RemoverEncodingXMLAssinado := True;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.txt', XMLAssinado);
  end;

  with ACBrBlocoX1.WebServices.DownloadArquivoBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btListarArquivosClick(Sender: TObject);
var
  wIE: String;
  wXML: AnsiString;
begin
  if not InputQuery('Listar Arquivos', 'Informe a Inscrição Estadual', wIE) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.ListarArquivos do
  begin
    InscricaoEstadual := wIE;
    RemoverEncodingXMLAssinado := True;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.txt', wXML);
  end;

  with ACBrBlocoX1.WebServices.ListarArquivosBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btnBuscarArquivoClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt :=  '*.xml';
  OpenDialog1.Filter:= 'Arquivos XML|*.xml|Arquivos TXT|*.txt|Todos os arquivos|*.*';
  if OpenDialog1.Execute then
    edtArqBlocoX.Text := OpenDialog1.FileName;
end;

procedure TfrmPrincipal.btnConsultarClick(Sender: TObject);
var
  Recibo: String;
begin
  if not InputQuery('Consultar', 'Entre com o número do recibo', Recibo) then
    Exit;

  ConfigurarDFe;

  ACBrBlocoX1.WebServices.ConsultarBlocoX.Recibo := Recibo;
  ACBrBlocoX1.WebServices.ConsultarBlocoX.Executar;

  memArqAssinado.Text := ACBrBlocoX1.WebServices.ConsultarBlocoX.RetWS;
end;

procedure TfrmPrincipal.btnCriarAssinaturaClick(Sender: TObject);
var
  FXMLOriginal: TStringList;
begin
  memArqAssinado.Lines.Clear;

  if (Trim(edtCertificado.Text) = '') then
  begin
    edtCertificado.SetFocus;
    raise Exception.Create('Certificado não foi informado!');
  end;

  if (Trim(edtArqBlocoX.Text) = '') or (not FileExists(Trim(edtArqBlocoX.Text)))then
  begin
    edtArqBlocoX.SetFocus;
    raise Exception.Create('Arquivo inválido!');
  end;

  ConfigurarDFe;

  try
    FXMLOriginal := TStringList.Create;
    FXMLOriginal.LoadFromFile(edtArqBlocoX.Text);
    if Pos('</reducaoz>',LowerCase(FXMLOriginal.Text)) > 0 then
      memArqAssinado.Text := ACBrBlocoX1.SSL.Assinar(FXMLOriginal.Text, 'ReducaoZ', 'Mensagem')
    else if Pos('</estoque>',LowerCase(FXMLOriginal.Text)) > 0 then
      memArqAssinado.Text := ACBrBlocoX1.SSL.Assinar(FXMLOriginal.Text, 'Estoque', 'Mensagem')
    else
      ShowMessage('Arquivo não reconhecido');
  finally
    FXMLOriginal.Free;
  end;

  GravarConfiguracoes;
end;

procedure TfrmPrincipal.btnSalvarArquivoClick(Sender: TObject);
begin
  SaveDialog1.FileName := ExtractFileNameWithoutExt(edtArqBlocoX.Text)+'-assinado.'+ExtractFileExt(edtArqBlocoX.Text);
  SaveDialog1.Execute;
end;

procedure TfrmPrincipal.btnTransmitirClick(Sender: TObject);
var
  wWebServiceBlocoX: TTransmitirArquivoBlocoX;
begin
  if not ValidarArquivo then
    Exit;

  ConfigurarDFe;

  wWebServiceBlocoX := ACBrBlocoX1.WebServices.TransmitirArquivoBlocoX;

  wWebServiceBlocoX.XML := memArqAssinado.Text;

  if wWebServiceBlocoX.Executar then
    memArqAssinado.Text := wWebServiceBlocoX.RetWS
  else
    memArqAssinado.Text := 'Erro ao enviar' + sLineBreak + wWebServiceBlocoX.Msg;
end;

procedure TfrmPrincipal.btnValidarClick(Sender: TObject);
var
  wWebServiceBlocoX: TValidarBlocoX;
begin
  if not ValidarArquivo then
    Exit;

  ConfigurarDFe;

  wWebServiceBlocoX := ACBrBlocoX1.WebServices.ValidarBlocoX;

  wWebServiceBlocoX.XML := memArqAssinado.Text;
  wWebServiceBlocoX.ValidarPafEcfEEcf := True;

  if wWebServiceBlocoX.Executar then
    memArqAssinado.Text := wWebServiceBlocoX.RetWS
  else
    memArqAssinado.Text := 'Erro ao validar' + sLineBreak + wWebServiceBlocoX.Msg;
end;

procedure TfrmPrincipal.btReprocessarArqClick(Sender: TObject);
var
  wRecibo: String;
  wXML: AnsiString;
begin
  if not InputQuery('Reprocessar Arquivo', 'Informe o número do Recibo', wRecibo) then
    Exit;

  ConfigurarDFe;

  with ACBrBlocoX1.ReprocessarArquivo do
  begin
    Recibo := wRecibo;
    RemoverEncodingXMLAssinado := True;
    GerarXML(True);
    wXML := XMLAssinado;

    // DEBUG:
    //WriteToTXT('_LogXML.txt', XMLAssinado);
  end;

  with ACBrBlocoX1.WebServices.ReprocessarArquivoBlocoX do
  begin
    UsarCData := True;
    XML := wXML;
    Executar;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);

    mmRetornoBlocoX.Text := RetWS;
  end;
end;

procedure TfrmPrincipal.btTransmitirArqClick(Sender: TObject);
var
  wXML: AnsiString;
  wWebServiceBlocoX: TTransmitirArquivoBlocoX;
begin
  wXML := GetXML;
  if (wXML = EmptyStr) then
    Exit;

  ConfigurarDFe;

  wWebServiceBlocoX := ACBrBlocoX1.WebServices.TransmitirArquivoBlocoX;
  with wWebServiceBlocoX do
  begin
    XML := wXML;

    // DEBUG:
    //WriteToTXT('_Log.txt', RetornoWS + sLineBreak + sLineBreak + RetWS);


    if Executar then
      mmRetornoBlocoX.Text := RetWS
    else
      mmRetornoBlocoX.Text := 'Erro ao enviar' + sLineBreak + Msg;
  end;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GravarConfiguracoes
end;

end.

