unit Principal;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Grids, ExtCtrls, Spin, Buttons, ACBrExtratoAPI, ACBrOpenSSLUtils,
  ACBrBase;

const
  cACBrStr = 'ACBrExtratoAPI';

type

  { TfrPrincipal }

  TfrPrincipal = class(TForm)
    ACBrExtratoAPI1: TACBrExtratoAPI;
    ACBrOpenSSLUtils1: TACBrOpenSSLUtils;
    btInterAcharCertificado: TSpeedButton;
    btInterAcharChavePrivada: TSpeedButton;
    btConsultarExtrato: TButton;
    btGravarConfig: TButton;
    btBBAcharCertificado: TSpeedButton;
    btBBAcharChavePrivada: TSpeedButton;
    btLerConfig: TButton;
    cbConfigGeralBanco: TComboBox;
    cbConfigGeralAmbiente: TComboBox;
    cbConfigLogNivel: TComboBox;
    edInicio: TDateTimePicker;
    edFim: TDateTimePicker;
    gbConfigInter: TGroupBox;
    edAgencia: TEdit;
    edInterCertificado: TEdit;
    edInterChavePrivada: TEdit;
    edBBClientID: TEdit;
    edInterClientID: TEdit;
    edBBClientSecret: TEdit;
    edInterClientSecret: TEdit;
    edConta: TEdit;
    eBBDevAppKey: TEdit;
    edBBCertificado: TEdit;
    edBBChavePrivada: TEdit;
    edBBMCITeste: TEdit;
    edConfigLogArquivo: TEdit;
    edConfigProxyHost: TEdit;
    edConfigProxySenha: TEdit;
    edConfigProxyUsuario: TEdit;
    gbConfigGeral: TGroupBox;
    gbConfigLog: TGroupBox;
    gbConfigProxy: TGroupBox;
    gbConfigBB: TGroupBox;
    lbInicio: TLabel;
    lbFim: TLabel;
    lbConfigGeralBanco: TLabel;
    lbConfigProxyUsuario: TLabel;
    lbConfigLogArquivo: TLabel;
    lbConfigLogNivel: TLabel;
    lbConfigGeralAmbiente: TLabel;
    lbInterCertificado: TLabel;
    lbInterChavePrivada: TLabel;
    lbInterClientID: TLabel;
    lbInterClientSecret: TLabel;
    lbBBDevAppKey: TLabel;
    lbBBClientID: TLabel;
    lbConfigProxySenha: TLabel;
    lbBBClientSecret: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbInterErroCertificado: TLabel;
    lbInterErroChavePrivada: TLabel;
    lbBBMCITeste: TLabel;
    lbConfigProxyHost: TLabel;
    lbConfigProxyPorta: TLabel;
    lbBBCertificado: TLabel;
    lbBBChavePrivada: TLabel;
    lbBBErroCertificado: TLabel;
    lbBBErroChavePrivada: TLabel;
    OpenDialog1: TOpenDialog;
    pnConfigInter: TPanel;
    pnConfigRodape: TPanel;
    pnConfigBB: TPanel;
    pgConfigBancos: TPageControl;
    pnConfigBancos: TPanel;
    pnConfigGeral: TPanel;
    pnConfig: TPanel;
    pgPrincipal: TPageControl;
    gdLancamentos: TStringGrid;
    pnConfigLog: TPanel;
    pnConfigProxy: TPanel;
    btConfigLogArquivo: TSpeedButton;
    btConfigProxyVerSenha: TSpeedButton;
    edConfigProxyPorta: TSpinEdit;
    tsConfigInter: TTabSheet;
    tsConfigBB: TTabSheet;
    tsConsulta: TTabSheet;
    tsConfig: TTabSheet;
    procedure btBBAcharCertificadoClick(Sender: TObject);
    procedure btBBAcharChavePrivadaClick(Sender: TObject);
    procedure btConfigLogArquivoClick(Sender: TObject);
    procedure btConfigProxyVerSenhaClick(Sender: TObject);
    procedure btConsultarExtratoClick(Sender: TObject);
    procedure btGravarConfigClick(Sender: TObject);
    procedure btInterAcharCertificadoClick(Sender: TObject);
    procedure btInterAcharChavePrivadaClick(Sender: TObject);
    procedure btLerConfigClick(Sender: TObject);
    procedure cbConfigGeralAmbienteChange(Sender: TObject);
    procedure cbConfigGeralBancoChange(Sender: TObject);
    procedure edBBArqsChange(Sender: TObject);
    procedure edBBCertificadoExit(Sender: TObject);
    procedure edBBChavePrivadaExit(Sender: TObject);
    procedure edInterArqsChange(Sender: TObject);
    procedure edInterCertificadoExit(Sender: TObject);
    procedure edInterChavePrivadaExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao; 
    procedure PreencherLancamentos;
    procedure ConfigurarOwnerBancos;

    procedure AvaliarInterfaceConfig;

    function ValidarCertificado(aArq: String): String;
    function ValidarChavePrivada(aArq: String): String;

    function RemoverPathAplicacao(const aArquivo: String): String;
    function AdicionarPathAplicacao(const aArquivo: String): String;

    function NomeArquivoConfiguracao: String;
  public

  end;

var
  frPrincipal: TfrPrincipal;

implementation

uses
  TypInfo, IniFiles, synacode, DateUtils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrExtratoAPIBB;

{$R *.dfm}

{ TfrPrincipal }

procedure TfrPrincipal.btConsultarExtratoClick(Sender: TObject);
begin
  AplicarConfiguracao;
  ACBrExtratoAPI1.ConsultarExtrato(edAgencia.Text, edConta.Text{, edInicio.Date, edFim.Date});
  PreencherLancamentos;
end;

procedure TfrPrincipal.btBBAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBBChavePrivada.Text;
  if OpenDialog1.Execute then
    edBBChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  lbBBErroChavePrivada.Caption := ValidarChavePrivada(edBBChavePrivada.Text);
end;

procedure TfrPrincipal.btConfigLogArquivoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edConfigLogArquivo.Text;
  if OpenDialog1.Execute then
    edConfigLogArquivo.Text := RemoverPathAplicacao(OpenDialog1.FileName);
end;

procedure TfrPrincipal.btConfigProxyVerSenhaClick(Sender: TObject);
begin
  {$IfDef FPC}
  if btConfigProxyVerSenha.Down then
    edConfigProxySenha.EchoMode := emNormal
  else
    edConfigProxySenha.EchoMode := emPassword;
  {$Else}
  if btConfigProxyVerSenha.Down then
    edConfigProxySenha.PasswordChar := #0
  else
    edConfigProxySenha.PasswordChar := '*';
  {$EndIf}
end;

procedure TfrPrincipal.btBBAcharCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBBCertificado.Text;
  if OpenDialog1.Execute then
    edBBCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  lbBBErroCertificado.Caption := ValidarCertificado(edBBCertificado.Text);
end;

procedure TfrPrincipal.btGravarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrPrincipal.btInterAcharCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edInterCertificado.Text;
  if OpenDialog1.Execute then
    edInterCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  lbInterErroCertificado.Caption := ValidarCertificado(edInterCertificado.Text);
end;

procedure TfrPrincipal.btInterAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edInterChavePrivada.Text;
  if OpenDialog1.Execute then
    edInterChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  lbInterErroChavePrivada.Caption := ValidarChavePrivada(edInterChavePrivada.Text);
end;

procedure TfrPrincipal.btLerConfigClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrPrincipal.cbConfigGeralAmbienteChange(Sender: TObject);
begin
  AplicarConfiguracao;
  AvaliarInterfaceConfig;
end;

procedure TfrPrincipal.cbConfigGeralBancoChange(Sender: TObject);
begin
  AplicarConfiguracao;
  AvaliarInterfaceConfig;
end;

procedure TfrPrincipal.edBBArqsChange(Sender: TObject);
begin
  lbBBErroChavePrivada.Caption := EmptyStr;
  lbBBErroCertificado.Caption := EmptyStr;
end;

procedure TfrPrincipal.edBBCertificadoExit(Sender: TObject);
begin
  lbBBErroCertificado.Caption := ValidarCertificado(edBBCertificado.Text);
end;

procedure TfrPrincipal.edBBChavePrivadaExit(Sender: TObject);
begin
  lbBBErroChavePrivada.Caption := ValidarChavePrivada(edBBChavePrivada.Text);
end;

procedure TfrPrincipal.edInterArqsChange(Sender: TObject);
begin
  lbInterErroChavePrivada.Caption := EmptyStr;
  lbInterErroCertificado.Caption := EmptyStr;
end;

procedure TfrPrincipal.edInterCertificadoExit(Sender: TObject);
begin
  lbInterErroCertificado.Caption := ValidarCertificado(edInterCertificado.Text);
end;

procedure TfrPrincipal.edInterChavePrivadaExit(Sender: TObject);
begin
  lbInterErroChavePrivada.Caption := ValidarChavePrivada(edInterChavePrivada.Text);
end;

procedure TfrPrincipal.FormCreate(Sender: TObject);
var
  i: TACBrExtratoAPIBancoConsulta;
  j: TACBrExtratoAPIAmbiente;
begin
  cbConfigGeralBanco.Items.Clear;
  for i := Low(TACBrExtratoAPIBancoConsulta) to High(TACBrExtratoAPIBancoConsulta) do
    cbConfigGeralBanco.Items.Add(GetEnumName(TypeInfo(TACBrExtratoAPIBancoConsulta), Integer(i)));

  cbConfigGeralAmbiente.Items.Clear;
  for j := Low(TACBrExtratoAPIAmbiente) to High(TACBrExtratoAPIAmbiente) do
    cbConfigGeralAmbiente.Items.Add(GetEnumName(TypeInfo(TACBrExtratoAPIAmbiente), Integer(j)));

  LerConfiguracao;
  ConfigurarOwnerBancos;
  AvaliarInterfaceConfig;

  edInicio.Date := StartOfTheMonth(Today);
  edFim.Date := IncDay(Today, -1);
end;

procedure TfrPrincipal.LerConfiguracao;
var
  wIni: TIniFile;
begin
  wIni := TIniFile.Create(NomeArquivoConfiguracao);
  try 
    edConfigProxyHost.Text := wIni.ReadString('Proxy', 'Host', EmptyStr);
    edConfigProxyPorta.Text := wIni.ReadString('Proxy', 'Porta', EmptyStr);
    edConfigProxyUsuario.Text := wIni.ReadString('Proxy', 'Usuario', EmptyStr);
    edConfigProxySenha.Text := StrCrypt(DecodeBase64(wIni.ReadString('Proxy', 'Senha', EmptyStr)), cACBrStr);
              
    edConfigLogArquivo.Text := wIni.ReadString('Log', 'Arquivo', EmptyStr);
    cbConfigLogNivel.ItemIndex := wIni.ReadInteger('Log', 'Nivel', 1);

    cbConfigGeralBanco.ItemIndex := wIni.ReadInteger('Config', 'Banco', 0);
    cbConfigGeralAmbiente.ItemIndex := wIni.ReadInteger('Config', 'Ambiente', 0);

    eBBDevAppKey.Text := wIni.ReadString('BB', 'DevAppKey', EmptyStr);
    edBBMCITeste.Text := wIni.ReadString('BB', 'MCITeste', EmptyStr);
    edBBClientID.Text := wIni.ReadString('BB', 'ClientID', EmptyStr);
    edBBClientSecret.Text := wIni.ReadString('BB', 'ClientSecret', EmptyStr);
    edBBCertificado.Text := wIni.ReadString('BB', 'Certificado', EmptyStr);
    edBBChavePrivada.Text := wIni.ReadString('BB', 'ChavePrivada', EmptyStr);

    edInterClientID.Text := wIni.ReadString('Inter', 'ClientID', EmptyStr);
    edInterClientSecret.Text := wIni.ReadString('Inter', 'ClientSecret', EmptyStr);
    edInterCertificado.Text := wIni.ReadString('Inter', 'Certificado', EmptyStr);
    edInterChavePrivada.Text := wIni.ReadString('Inter', 'ChavePrivada', EmptyStr);
  finally
    wIni.Free;
  end;

  AplicarConfiguracao;
end;

procedure TfrPrincipal.GravarConfiguracao;
var
  wIni: TIniFile;
begin
  wIni := TIniFile.Create(NomeArquivoConfiguracao);
  try
    wIni.WriteString('Proxy', 'Host', edConfigProxyHost.Text);
    wIni.WriteString('Proxy', 'Porta', edConfigProxyPorta.Text);
    wIni.WriteString('Proxy', 'Usuario', edConfigProxyUsuario.Text);
    wIni.WriteString('Proxy', 'Senha', EncodeBase64(StrCrypt(edConfigProxySenha.Text, cACBrStr)));

    wIni.WriteString('Log', 'Arquivo', edConfigLogArquivo.Text);
    wIni.WriteInteger('Log', 'Nivel', cbConfigLogNivel.ItemIndex);

    wIni.WriteInteger('Config', 'Banco', cbConfigGeralBanco.ItemIndex);
    wIni.WriteInteger('Config', 'Ambiente', cbConfigGeralAmbiente.ItemIndex);

    wIni.WriteString('BB', 'DevAppKey', eBBDevAppKey.Text);
    wIni.WriteString('BB', 'MCITeste', edBBMCITeste.Text);
    wIni.WriteString('BB', 'ClientID', edBBClientID.Text);
    wIni.WriteString('BB', 'ClientSecret', edBBClientSecret.Text);
    wIni.WriteString('BB', 'Certificado', edBBCertificado.Text);
    wIni.WriteString('BB', 'ChavePrivada', edBBChavePrivada.Text);

    wIni.WriteString('Inter', 'ClientID', edInterClientID.Text);
    wIni.WriteString('Inter', 'ClientSecret', edInterClientSecret.Text);
    wIni.WriteString('Inter', 'Certificado', edInterCertificado.Text);
    wIni.WriteString('Inter', 'ChavePrivada', edInterChavePrivada.Text);
  finally
    wIni.Free;
  end;

  AplicarConfiguracao;
end;

procedure TfrPrincipal.AplicarConfiguracao;
begin
  if NaoEstaVazio(edConfigProxyHost.Text) then
  begin
    ACBrExtratoAPI1.Banco.ProxyHost := edConfigProxyHost.Text;
    ACBrExtratoAPI1.Banco.ProxyPort := edConfigProxyPorta.Text;
    ACBrExtratoAPI1.Banco.ProxyUser := edConfigProxyUsuario.Text;
    ACBrExtratoAPI1.Banco.ProxyPass := edConfigProxySenha.Text;
  end;

  if NaoEstaVazio(edConfigLogArquivo.Text) then
  begin
    ACBrExtratoAPI1.LogArquivo := edConfigLogArquivo.Text;
    ACBrExtratoAPI1.LogNivel := cbConfigLogNivel.ItemIndex;
  end;

  ACBrExtratoAPI1.Ambiente := TACBrExtratoAPIAmbiente(cbConfigGeralAmbiente.ItemIndex);
  ACBrExtratoAPI1.BancoConsulta := TACBrExtratoAPIBancoConsulta(cbConfigGeralBanco.ItemIndex);

  if (ACBrExtratoAPI1.BancoConsulta = bccBancoDoBrasil) then
  with TACBrExtratoAPIBB(ACBrExtratoAPI1.Banco) do
  begin
    ClientID := edBBClientID.Text;
    ClientSecret := edBBClientSecret.Text;
    ArquivoCertificado := edBBCertificado.Text;
    ArquivoChavePrivada := edBBChavePrivada.Text;
    DeveloperApplicationKey := eBBDevAppKey.Text;
    if (ACBrExtratoAPI1.Ambiente = eamHomologacao) then
      xMCITeste := edBBMCITeste.Text;
  end;

  if (ACBrExtratoAPI1.BancoConsulta = bccInter) then
  begin
    ACBrExtratoAPI1.Banco.ClientID := edInterClientID.Text;
    ACBrExtratoAPI1.Banco.ClientSecret := edInterClientSecret.Text;
    ACBrExtratoAPI1.Banco.ArquivoCertificado := edInterCertificado.Text;
    ACBrExtratoAPI1.Banco.ArquivoChavePrivada := edInterChavePrivada.Text;
  end;
end;

function TfrPrincipal.ValidarCertificado(aArq: String): String;
var
  wArquivo: String;
begin
  Result := '-- OK --';
  wArquivo := AdicionarPathAplicacao(aArq);
  if EstaVazio(wArquivo) then
    Result := ACBrStr('Arquivo não especificado')
  else if (not FileExists(wArquivo)) then
    Result := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(wArquivo);
    except
      On E: Exception do
        Result := E.Message;
    end;
  end;
end;

function TfrPrincipal.ValidarChavePrivada(aArq: String): String;
var
  wArquivo: String;
begin
  Result := '-- OK --';
  wArquivo := AdicionarPathAplicacao(aArq);
  if EstaVazio(wArquivo) then
    Result := ACBrStr('Arquivo não especificado')
  else if (not FileExists(wArquivo)) then
    Result := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(wArquivo);
    except
      On E: Exception do
        Result := E.Message;
    end;
  end;
end;

function TfrPrincipal.RemoverPathAplicacao(const aArquivo: String): String;
begin
  Result := Trim(aArquivo);
  if (Pos(ApplicationPath, Result) = 1) then
    Result := ExtractFileName(Result);
end;

function TfrPrincipal.AdicionarPathAplicacao(const aArquivo: String): String;
begin
  Result := Trim(aArquivo);
  if NaoEstaVazio(Result) and EstaVazio(ExtractFilePath(Result)) then
    Result := ApplicationPath + Result;
end;

procedure TfrPrincipal.PreencherLancamentos;
var
  i: Integer;
begin
  if EstaZerado(ACBrExtratoAPI1.ExtratoConsultado.Lancamentos.Count) then
    Exit;

  with ACBrExtratoAPI1.ExtratoConsultado do
  begin
    gdLancamentos.RowCount := Lancamentos.Count + 1;
    for i := 0 to Lancamentos.Count - 1 do
    begin
      gdLancamentos.Cells[0, i+1] := IntToStr(i+1);
      gdLancamentos.Cells[1, i+1] := FormatDateBr(Lancamentos[i].dataLancamento);
      gdLancamentos.Cells[3, i+1] := Lancamentos[i].Descricao;
      gdLancamentos.Cells[4, i+1] := FloatToString(Lancamentos[i].Valor);

      if (Lancamentos[i].dataMovimento > 0) then
        gdLancamentos.Cells[2, i+1] := FormatDateBr(Lancamentos[i].dataMovimento);
      end;
  end;
end;

procedure TfrPrincipal.ConfigurarOwnerBancos;
begin
  gbConfigBB.Parent := pnConfigBancos;
  gbConfigInter.Parent := pnConfigBancos;
end;

procedure TfrPrincipal.AvaliarInterfaceConfig;
var
  wProducao, wHomologacao: Boolean;
begin
  wProducao := (ACBrExtratoAPI1.Ambiente = eamProducao);
  wHomologacao := (ACBrExtratoAPI1.Ambiente = eamHomologacao);

  edBBCertificado.Enabled := wProducao;
  edBBChavePrivada.Enabled := wProducao;
  edBBMCITeste.Enabled := wHomologacao;

  gbConfigBB.Visible := (ACBrExtratoAPI1.BancoConsulta = bccBancoDoBrasil);
  gbConfigInter.Visible := (ACBrExtratoAPI1.BancoConsulta = bccInter);
end;

function TfrPrincipal.NomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName,'.ini');
end;

end.

