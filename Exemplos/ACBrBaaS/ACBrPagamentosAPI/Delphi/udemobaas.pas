{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César                                                                }
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

{$I ACBr.inc}

unit uDemoBaaS;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin,
  ACBrPagamentosAPI,
  ACBrPagamentosAPIBB,
  ACBrOpenSSLUtils,
  ImgList, ACBrBase,
  ACBrSocket;

const
  CURL_ACBR = 'https://projetoacbr.com.br';

type
  { TfrPagamentosAPITeste }
  TfrPagamentosAPITeste = class(TForm)
    ACBrOpenSSLUtils1: TACBrOpenSSLUtils;
    ACBrPagamentosAPI1: TACBrPagamentosAPI;
    ACBrPagamentosAPIBB1: TACBrPagamentosAPIBB;
    btBBVerSenhaPFX: TSpeedButton;
    btLerParametros: TBitBtn;
    btSalvarParametros: TBitBtn;
    cbAmbiente: TComboBox;
    cbNivelLog: TComboBox;
    cbBancoAtual: TComboBox;
    edBBArqCertificado: TEdit;
    edBBArqChavePrivada: TEdit;
    edBBArqPFX: TEdit;
    edBBSenhaPFX: TEdit;
    edArqLog: TEdit;
    edBBClientID: TEdit;
    edBBClientSecret: TEdit;
    edBBDevAppKey: TEdit;
    edProxyHost: TEdit;
    edProxySenha: TEdit;
    edProxyUsuario: TEdit;
    gbLog: TGroupBox;
    gbProxy: TGroupBox;
    gbBanco: TGroupBox;
    ImageList1: TImageList;
    imBBErroCertificado: TImage;
    imBBErroChavePrivada: TImage;
    imBBErroPFX: TImage;
    imErroBanco: TImage;
    lbProxyUsuario: TLabel;
    lbLogArquivo: TLabel;
    lbLogNivel: TLabel;
    Label13: TLabel;
    lbAmbiente: TLabel;
    lbTimeout: TLabel;
    lbBBClientID: TLabel;
    lbBBClientSecret: TLabel;
    lbBBDevAppKey: TLabel;
    lbProxySenha: TLabel;
    lbProxyHost: TLabel;
    lbProxyPorta: TLabel;
    lbBBArqCertificado: TLabel;
    lbBBArqChavePrivada: TLabel;
    lbBBArqPFX: TLabel;
    lbBBErroCertificado: TLabel;
    lbBBErroChavePrivada: TLabel;
    lbBBErroPFX: TLabel;
    lbBBSenhaPFX: TLabel;
    OpenDialog1: TOpenDialog;
    pgTestes: TPageControl;
    pgBancos: TPageControl;
    pgBBCertificados: TPageControl;
    pConfPSPBB: TPanel;
    pgPrincipal: TPageControl;
    pgConfig: TPageControl;
    pnBBCertificados: TPanel;
    pnBBChaveECert: TPanel;
    pnBBPFX: TPanel;
    pnConfigRodape: TPanel;
    pnLog: TPanel;
    pnProxy: TPanel;
    pnBanco: TPanel;
    rgBBTipoCertificado: TRadioGroup;
    sbArqLog: TSpeedButton;
    sbBBAcharArqCertificado: TSpeedButton;
    sbBBAcharChavePrivada: TSpeedButton;
    sbBBAcharPFX: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    seProxyPorta: TSpinEdit;
    seTimeout: TSpinEdit;
    tsPagamentos: TTabSheet;
    tsTestes: TTabSheet;
    tsConfig: TTabSheet;
    tsBB: TTabSheet;
    tsBancos: TTabSheet;
    tsBBChaveECertificado: TTabSheet;
    tsBBPFX: TTabSheet;
    tsPagamentosAPI: TTabSheet;
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure cbBancoAtualChange(Sender: TObject);
    procedure edBBArqCertificadoExit(Sender: TObject);
    procedure edBBArqChavePrivadaExit(Sender: TObject);
    procedure edBBArqPFXExit(Sender: TObject);
    procedure rgBBTipoCertificadoSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbBBAcharArqCertificadoClick(Sender: TObject);
    procedure sbBBAcharChavePrivadaClick(Sender: TObject);
    procedure sbBBAcharPFXClick(Sender: TObject);
  private
    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;

    procedure VerificarConfiguracao;
    procedure VerificarConfiguracaoBB;
                                          
    procedure ConfigurarBancos;
    procedure ConfigurarACBrPagamentosAPI;

    procedure InicializarBitmaps;
    procedure InicializarActivePages;
    procedure InicializarComponentesDefault;

    procedure LimparAlertas;
    procedure LigarAlertasdeErrosDeConfiguracao;
    procedure LigarAlertasdeErrosDeConfiguracaoBB;
    procedure TratarException(Sender: TObject; E: Exception);

    function NomeArquivoConfiguracao: String;
    function RemoverPathAplicacao(const AFileName: String): String;
    function AdicionarPathAplicacao(const AFileName: String): String;

    function ValidarPFX(aArquivo: String; aImg: TControl = nil): String;
    function ValidarCertificado(aArquivo: String; aImg: TControl = nil): String;
    function ValidarChavePrivada(aArquivo: String; aImg: TControl = nil): String;
  public

  end;

var
  frPagamentosAPITeste: TfrPagamentosAPITeste;

implementation

uses
  IniFiles, TypInfo, synacode, uPagamentos,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

{$R *.dfm}

{ TfrPagamentosAPITeste }

procedure TfrPagamentosAPITeste.edBBArqChavePrivadaExit(Sender: TObject);
begin
  lbBBArqChavePrivada.Caption := ValidarChavePrivada(edBBArqChavePrivada.Text, imBBErroChavePrivada);
end;

procedure TfrPagamentosAPITeste.edBBArqPFXExit(Sender: TObject);
begin
  lbBBArqPFX.Caption := ValidarPFX(edBBArqPFX.Text, imBBErroPFX);
end;

procedure TfrPagamentosAPITeste.FormCreate(Sender: TObject);
begin
  LimparAlertas;
  InicializarBitmaps;
  InicializarActivePages;
  InicializarComponentesDefault;
  Application.OnException := TratarException;

  LerConfiguracao;
  VerificarConfiguracao;
end;

procedure TfrPagamentosAPITeste.sbBBAcharArqCertificadoClick(Sender: TObject);
begin 
  OpenDialog1.FileName := edBBArqCertificado.Text;
  if OpenDialog1.Execute then
    edBBArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  lbBBErroCertificado.Caption := ValidarCertificado(edBBArqCertificado.Text, imBBErroCertificado);
end;

procedure TfrPagamentosAPITeste.sbBBAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBBArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edBBArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  lbBBErroChavePrivada.Caption := ValidarChavePrivada(edBBArqChavePrivada.Text, imBBErroChavePrivada);
end;

procedure TfrPagamentosAPITeste.sbBBAcharPFXClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBBArqPFX.Text;
  if OpenDialog1.Execute then
    edBBArqPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  lbBBErroPFX.Caption := ValidarPFX(edBBArqPFX.Text, imBBErroPFX);
end;

procedure TfrPagamentosAPITeste.edBBArqCertificadoExit(Sender: TObject);
begin
  lbBBArqCertificado.Caption := ValidarCertificado(edBBArqCertificado.Text, imBBErroCertificado);
end;

procedure TfrPagamentosAPITeste.cbBancoAtualChange(Sender: TObject);
begin
  imErroBanco.Visible := (cbBancoAtual.ItemIndex < 0);
end;

procedure TfrPagamentosAPITeste.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
  AplicarConfiguracao;
end;

procedure TfrPagamentosAPITeste.rgBBTipoCertificadoSelectionChanged(Sender: TObject);
begin
  pnBBPFX.Visible := (rgBBTipoCertificado.ItemIndex = 0);
  pnBBChaveECert.Visible := (rgBBTipoCertificado.ItemIndex = 1);
end;

procedure TfrPagamentosAPITeste.LerConfiguracao;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    cbBancoAtual.ItemIndex := Ini.ReadInteger('PagamentosAPI','Banco', 0);
    cbAmbiente.ItemIndex := Ini.ReadInteger('PagamentosAPI','Ambiente', 0);
    seTimeout.Value := Ini.ReadInteger('PagamentosAPI', 'TimeOut', ChttpTimeOutDef);

    edProxyHost.Text := Ini.ReadString('Proxy', 'Host', EmptyStr);
    seProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', EmptyStr);
    edProxyUsuario.Text := Ini.ReadString('Proxy', 'Usuario', EmptyStr);
    edProxySenha.Text := StrCrypt(DecodeBase64(Ini.ReadString('Proxy', 'Senha', EmptyStr)), CURL_ACBR);

    edArqLog.Text := Ini.ReadString('Log', 'Arquivo', EmptyStr);
    cbNivelLog.ItemIndex := Ini.ReadInteger('Log', 'Nivel', 1);

    edBBClientID.Text := Ini.ReadString('BancoBrasil', 'ClientID', EmptyStr);
    edBBClientSecret.Text := Ini.ReadString('BancoBrasil', 'ClientSecret', EmptyStr);
    edBBDevAppKey.Text := Ini.ReadString('BancoBrasil', 'DeveloperApplicationKey', EmptyStr);
    edBBArqPFX.Text := Ini.ReadString('BancoBrasil', 'ArqPFX', edBBArqPFX.Text);
    edBBSenhaPFX.Text := Ini.ReadString('BancoBrasil', 'SenhaPFX', edBBSenhaPFX.Text);
    edBBArqChavePrivada.Text := Ini.ReadString('BancoBrasil', 'ArqChavePrivada', edBBArqChavePrivada.Text);
    edBBArqCertificado.Text := Ini.ReadString('BancoBrasil', 'ArqCertificado', edBBArqCertificado.Text);
    rgBBTipoCertificado.ItemIndex := Ini.ReadInteger('BancoBrasil', 'TipoCertificado', rgBBTipoCertificado.ItemIndex);
    rgBBTipoCertificadoSelectionChanged(Nil);
  finally
    Ini.Free;
  end;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPagamentosAPITeste.GravarConfiguracao;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try 
    Ini.WriteInteger('PagamentosAPI','Banco', cbBancoAtual.ItemIndex);
    Ini.WriteInteger('PagamentosAPI','Ambiente', cbAmbiente.ItemIndex);
    Ini.WriteInteger('PagamentosAPI', 'TimeOut', seTimeout.Value); 

    Ini.WriteString('Proxy', 'Host', edProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', seProxyPorta.Text);
    Ini.WriteString('Proxy', 'Usuario', edProxyUsuario.Text);
    Ini.WriteString('Proxy', 'Senha', EncodeBase64(StrCrypt(edProxySenha.Text, CURL_ACBR)));  

    Ini.WriteString('Log', 'Arquivo', edArqLog.Text);
    Ini.WriteInteger('Log', 'Nivel', cbNivelLog.ItemIndex); 

    Ini.WriteString('BancoBrasil', 'ClientID', edBBClientID.Text);
    Ini.WriteString('BancoBrasil', 'ClientSecret', edBBClientSecret.Text);
    Ini.WriteString('BancoBrasil', 'DeveloperApplicationKey', edBBDevAppKey.Text);
    Ini.WriteString('BancoBrasil', 'ArqChavePrivada', edBBArqChavePrivada.Text);
    Ini.WriteString('BancoBrasil', 'ArqCertificado', edBBArqCertificado.Text);
    Ini.WriteString('BancoBrasil', 'ArqPFX', edBBArqPFX.Text);
    Ini.WriteString('BancoBrasil', 'SenhaPFX', edBBSenhaPFX.Text);
    Ini.WriteInteger('BancoBrasil', 'TipoCertificado', rgBBTipoCertificado.ItemIndex);
  finally
    Ini.Free;
  end;

  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPagamentosAPITeste.AplicarConfiguracao;
begin
  ConfigurarACBrPagamentosAPI;
  ConfigurarBancos;
end;

procedure TfrPagamentosAPITeste.VerificarConfiguracao;
begin
  if imErroBanco.Visible then
  begin
    pgPrincipal.ActivePage := tsConfig;
    pgConfig.ActivePage := tsPagamentosAPI;
    MessageDlg('Configure o Banco utilizado', mtWarning, [mbOK], 0);
    Abort;
  end;

  case cbBancoAtual.ItemIndex of
    0: VerificarConfiguracaoBB;
    //...
  end;
end;

procedure TfrPagamentosAPITeste.VerificarConfiguracaoBB;
var
  wErro: String;
begin
  wErro := EmptyStr;

  if EstaVazio(edBBClientID.Text) then
    wErro := ' - Client ID' + sLineBreak;
  if EstaVazio(edBBClientID.Text) then
    wErro := wErro + ' - Client Secret' + sLineBreak;
  if EstaVazio(edBBDevAppKey.Text) then
    wErro := wErro + ' - Developer Application Key' + sLineBreak;

  if (cbAmbiente.ItemIndex > 0) then
  begin
    if (rgBBTipoCertificado.ItemIndex = 1) then
    begin
      if imBBErroCertificado.Visible then
        wErro := wErro + ' - Arquivo Certificado PEM/CSR' + sLineBreak;
      if imBBErroChavePrivada.Visible then
        wErro := wErro + ' - Arquivo Chave Privada';
    end
    else if imBBErroPFX.Visible then
      wErro := ' - Arquivo PFX';
  end;

  if NaoEstaVazio(wErro) then
  begin
    pgPrincipal.ActivePage := tsConfig;
    pgConfig.ActivePage := tsBancos;
    pgBancos.ActivePage := tsBB;
    MessageDlg('Configure o(s) campo(s):' + sLineBreak + wErro, mtWarning, [mbOK], 0);
    Abort;
  end;
end;

procedure TfrPagamentosAPITeste.ConfigurarBancos;
begin
  ACBrPagamentosAPIBB1.ClientID := edBBClientID.Text;
  ACBrPagamentosAPIBB1.ClientSecret := edBBClientSecret.Text;
  ACBrPagamentosAPIBB1.DeveloperApplicationKey := edBBDevAppKey.Text;
  if (rgBBTipoCertificado.ItemIndex = 0) then  // Se usa PFX
  begin
    ACBrPagamentosAPIBB1.ArquivoPFX := edBBArqPFX.Text;
    ACBrPagamentosAPIBB1.SenhaPFX := edBBSenhaPFX.Text;
  end
  else  // Se usa Certificado PEM + Chave Privada
  begin
    ACBrPagamentosAPIBB1.ArquivoChavePrivada := edBBArqChavePrivada.Text;
    ACBrPagamentosAPIBB1.ArquivoCertificado := edBBArqCertificado.Text;
  end;
end;

procedure TfrPagamentosAPITeste.ConfigurarACBrPagamentosAPI;
begin
  ACBrPagamentosAPI1.Ambiente := TACBrPagamentosAPIAmbiente(cbAmbiente.ItemIndex);  
  ACBrPagamentosAPI1.TimeOut := seTimeout.Value;  

  ACBrPagamentosAPI1.ArqLOG := edArqLog.Text;
  ACBrPagamentosAPI1.NivelLog := cbNivelLog.ItemIndex;    

  case cbBancoAtual.ItemIndex of
    0: ACBrPagamentosAPI1.Banco := ACBrPagamentosAPIBB1;
  else
    raise Exception.Create('Banco configurado é inválido');
  end;

  ACBrPagamentosAPI1.Banco.ProxyHost := edProxyHost.Text;
  ACBrPagamentosAPI1.Banco.ProxyPort := seProxyPorta.Text;
  ACBrPagamentosAPI1.Banco.ProxyUser := edProxyUsuario.Text;
  ACBrPagamentosAPI1.Banco.ProxyPass := edProxySenha.Text;
end;

procedure TfrPagamentosAPITeste.InicializarBitmaps;
begin
  ImageList1.GetBitmap(6, imErroBanco.Picture.Bitmap);
  ImageList1.GetBitmap(7, sbVerSenhaProxy.Glyph);
  ImageList1.GetBitmap(9, sbArqLog.Glyph);
  ImageList1.GetBitmap(10, btSalvarParametros.Glyph);
  ImageList1.GetBitmap(11, btLerParametros.Glyph); 

  ImageList1.GetBitmap(6, imBBErroPFX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imBBErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imBBErroChavePrivada.Picture.Bitmap);
  ImageList1.GetBitmap(9, sbBBAcharPFX.Glyph);
  ImageList1.GetBitmap(9, sbBBAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, sbBBAcharArqCertificado.Glyph);
  ImageList1.GetBitmap(7, btBBVerSenhaPFX.Glyph);
end;

procedure TfrPagamentosAPITeste.InicializarActivePages;
var
  frPagto: TfrmPagamentos;
begin
  pgPrincipal.ActivePageIndex := 0;
  pgConfig.ActivePageIndex := 0;
  pgBancos.ActivePageIndex := 0;

  frPagto := TfrmPagamentos.Create(Self);  // Destruído automaticamente ao fechar demo
  frPagto.pgPagamentos.Parent := tsPagamentos;
end;

procedure TfrPagamentosAPITeste.InicializarComponentesDefault;
var
  i: Integer;
  j: TACBrPagamentosAPIAmbiente;
begin
  cbBancoAtual.Items.Clear;
  for i := 0 to pgBancos.PageCount - 1 do
    cbBancoAtual.Items.Add(pgBancos.Pages[i].Caption);
    
  cbAmbiente.Items.Clear;
  for j := Low(TACBrPagamentosAPIAmbiente) to High(TACBrPagamentosAPIAmbiente) do
    cbAmbiente.Items.Add(GetEnumName(TypeInfo(TACBrPagamentosAPIAmbiente), Integer(j)));

  pnBBPFX.Parent := pnBBCertificados;
  pnBBChaveECert.Parent := pnBBCertificados;
end;

procedure TfrPagamentosAPITeste.LimparAlertas;
begin
  lbBBErroPFX.Caption := EmptyStr;
  lbBBErroCertificado.Caption := EmptyStr;
  lbBBErroChavePrivada.Caption := EmptyStr;
end;

procedure TfrPagamentosAPITeste.LigarAlertasdeErrosDeConfiguracao;
begin
  imErroBanco.Visible := (cbBancoAtual.ItemIndex < 0);
  case cbBancoAtual.ItemIndex of
    0: LigarAlertasdeErrosDeConfiguracaoBB;
    //...
  end;
end;

procedure TfrPagamentosAPITeste.LigarAlertasdeErrosDeConfiguracaoBB;
begin
  lbBBArqPFX.Caption := ValidarPFX(edBBArqPFX.Text, imBBErroPFX);
  lbBBArqCertificado.Caption := ValidarCertificado(edBBArqCertificado.Text, imBBErroCertificado);
  lbBBArqChavePrivada.Caption := ValidarChavePrivada(edBBArqChavePrivada.Text, imBBErroChavePrivada);
end;

procedure TfrPagamentosAPITeste.TratarException(Sender: TObject; E: Exception);
begin
  if (pgPrincipal.ActivePage = tsConfig) then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

function TfrPagamentosAPITeste.NomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

function TfrPagamentosAPITeste.RemoverPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (pos(ApplicationPath, s) = 1) then
    Result := ExtractFileName(s)
  else
    Result := s;
end;

function TfrPagamentosAPITeste.AdicionarPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (s = '') then
    Result := s
  else if (ExtractFilePath(AFileName) <> '') then
    Result := s
  else
    Result := ApplicationPath + s;
end;

function TfrPagamentosAPITeste.ValidarPFX(aArquivo: String; aImg: TControl): String;
var
  Arq: String;
begin
  Arq := AdicionarPathAplicacao(aArquivo);
  Result := 'OK';
  if EstaVazio(Arq) then
    Result := ACBrStr('Arquivo não especificado')
  else if (not FileExists(Arq)) then
    Result := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPFXFromFile(Arq, edBBSenhaPFX.Text);
    except
      On e: Exception do
        Result := e.Message;
    end;
  end;

  if Assigned(aImg) then
    aImg.Visible := (Result <> 'OK');
end;

function TfrPagamentosAPITeste.ValidarCertificado(aArquivo: String; aImg: TControl): String;
var
  Arq: String;
begin
  Arq := AdicionarPathAplicacao(aArquivo);
  Result := 'OK';
  if EstaVazio(Arq) then
    Result := ACBrStr('Arquivo não especificado')
  else if (not FileExists(Arq)) then
    Result := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(Arq);
    except
      On e: Exception do
        Result := e.Message;
    end;
  end;

  if Assigned(aImg) then
    aImg.Visible := (Result <> 'OK');
end;

function TfrPagamentosAPITeste.ValidarChavePrivada(aArquivo: String; aImg: TControl): String;
var
  Arq: String;
begin
  Arq := AdicionarPathAplicacao(aArquivo);
  Result := 'OK';
  if EstaVazio(Arq) then
    Result := ACBrStr('Arquivo não especificado')
  else if (not FileExists(Arq)) then
    Result := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(Arq);
    except
      On e: Exception do
        Result := e.Message;
    end;
  end;

  if Assigned(aImg) then
    aImg.Visible := (Result <> 'OK');
end;

end.

