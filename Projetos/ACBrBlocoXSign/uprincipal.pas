unit uPrincipal;

{$mode objfpc}{$H+}
{$r BannerACBrSAC.rc}

interface

uses
  Windows, Classes, SysUtils, strutils, dateutils, IniFiles, base64, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, MaskEdit,
  Menus, ComCtrls, Spin, EditBtn, UtilUnit, ACBrGIF, ACBrUtil, ACBrValidador,
  ACBrEnterTab, ACBrDFe, ACBrDFeSSL, ACBrDFeWebService, ACBrBlocoX, ACBrDFeUtil;

const
  _C = 'tYk*5W@';

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    ACBrBlocoX1: TACBrBlocoX;
    ACBrEnterTab1: TACBrEnterTab;
    ACBrGIF1: TACBrGIF;
    Bevel2: TBevel;
    btnConsultar: TBitBtn;
    btnValidar: TBitBtn;
    btnBuscarArquivo: TSpeedButton;
    btnBuscarCertificado: TSpeedButton;
    btnCriarAssinatura: TBitBtn;
    btnSalvarArquivo: TBitBtn;
    btnTransmitir: TBitBtn;
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
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    rgTipo: TRadioGroup;
    rbtTipoCapicom: TRadioButton;
    rbtTipoOpenSSL: TRadioButton;
    SaveDialog1: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ACBrGIF1Click(Sender: TObject);
    procedure btnBuscarArquivoClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnCriarAssinaturaClick(Sender: TObject);
    procedure btnSalvarArquivoClick(Sender: TObject);
    procedure btnTransmitirClick(Sender: TObject);
    procedure btnValidarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure LerConfiguracoes;
    procedure btnBuscarCertificadoClick(Sender: TObject);
  private
    procedure ConfigurarDFe;
    function GetPathConfig: String;
    procedure GravarConfiguracoes;
    procedure CarregarGifBannerACBrSAC;
    function ValidarArquivo : Boolean;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

Uses
  ACBrBlocoX_WebServices;

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

procedure TfrmPrincipal.ConfigurarDFe;
begin
  if rbtTipoCapicom.Checked then
  begin
    ACBrBlocoX1.Configuracoes.Geral.SSLLib := libCapicom;
    ACBrBlocoX1.Configuracoes.Certificados.NumeroSerie := edtCertificado.Text;
  end
  else
  begin
    ACBrBlocoX1.Configuracoes.Geral.SSLLib             := libOpenSSL;
    ACBrBlocoX1.Configuracoes.Certificados.ArquivoPFX  := edtCertificado.Text;
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
    F.WriteString('CONFIG', 'UltimoArquivo',     edtArqBlocoX.Text);
    F.WriteString('CONFIG', 'Tipo',        IfThen(rbtTipoCapicom.Checked, TIPO_CAPICOM, TIPO_OPENSSL));
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

procedure TfrmPrincipal.btnBuscarArquivoClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt :=  '*.xml';
  OpenDialog1.Filter:= 'Arquivos XML|*.xml|Arquivos TXT|*.txt|Todos os arquivos|*.*';
  if OpenDialog1.Execute then
    edtArqBlocoX.Text := OpenDialog1.FileName;
end;

procedure TfrmPrincipal.btnConsultarClick(Sender: TObject);
var
  Recibo : String;
begin
  if not InputQuery('Consultar', 'Entre com o número do recibo', Recibo) then
    exit;

  ConfigurarDFe;

  ACBrBlocoX1.WebServices.ConsultarBlocoX.Recibo := Recibo;
  ACBrBlocoX1.WebServices.ConsultarBlocoX.Executar;

  memArqAssinado.Text := ACBrBlocoX1.WebServices.ConsultarBlocoX.RetWS;
end;

procedure TfrmPrincipal.btnCriarAssinaturaClick(Sender: TObject);
var
  StrCodigoVinculacao: String;
  MsgErroValidacao: String;
  FXMLOriginal: TStringList;
begin
  memArqAssinado.Lines.Clear;

  if Trim(edtCertificado.Text) = '' then
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
  wWebServiceBlocoX: TEnviarBlocoX;
begin
  if not ValidarArquivo then
    Exit;

  ConfigurarDFe;

  wWebServiceBlocoX := ACBrBlocoX1.WebServices.EnviarBlocoX;

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
  wWebServiceBlocoX.ValidarPafEcfEEcf    := True;

  if wWebServiceBlocoX.Executar then
    memArqAssinado.Text := wWebServiceBlocoX.RetWS
  else
    memArqAssinado.Text := 'Erro ao validar' + sLineBreak + wWebServiceBlocoX.Msg;
end;

end.

