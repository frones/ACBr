unit uprincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, ACBrGIF, ACBrUtil,
  ACBrValidador, ACBrEnterTab, ACBrDFe, ACBrDFeSSL, Vcl.Mask, ACBrBase, IniFiles;

type
  TfrmPrincipal = class(TForm)
    ACBrEnterTab1: TACBrEnterTab;
    edtCertificado: TEdit;
    btnBuscarCertificado: TSpeedButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    rbtTipoCapicom: TRadioButton;
    rbtTipoOpenSSL: TRadioButton;
    edtCNPJSoftwareHouse: TMaskEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtCNPJCliente: TMaskEdit;
    memCodigoVinculacao: TMemo;
    Label4: TLabel;
    btnCopiar: TSpeedButton;
    btnCriarAssinatura: TSpeedButton;
    edtSenhaCertificado: TEdit;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnBuscarCertificadoClick(Sender: TObject);
    procedure btnCopiarClick(Sender: TObject);
    procedure btnCriarAssinaturaClick(Sender: TObject);
  private
    FACBrDFe: TACBrDFe;
    function GetPathConfig: String;
    procedure ConfigurarDFe;
    procedure GravarConfiguracoes;
    procedure LerConfiguracoes;
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

  const
    TIPO_CAPICOM = 'CAPICOM';
    TIPO_OPENSSL = 'OPENSSL';

implementation

{$R *.dfm}

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FACBrDFe.Free;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  FACBrDFe := TACBrDFe.Create(Self);

  edtCertificado.Clear;
  edtCNPJCliente.Clear;
  edtCNPJSoftwareHouse.Clear;
  memCodigoVinculacao.Clear;

  LerConfiguracoes;
end;

function TfrmPrincipal.GetPathConfig: String;
begin
  Result := ExtractFilePath(ParamStr(0)) + ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
end;

procedure TfrmPrincipal.btnBuscarCertificadoClick(Sender: TObject);
begin
  if rbtTipoCapicom.Checked then
    edtCertificado.Text := FACBrDFe.SSL.SelecionarCertificado
  else
  begin
    if OpenDialog1.Execute then
      edtCertificado.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmPrincipal.btnCopiarClick(Sender: TObject);
begin
  memCodigoVinculacao.CopyToClipboard;
  ShowMessage('Copiado para a área de transferência!');
end;

procedure TfrmPrincipal.btnCriarAssinaturaClick(Sender: TObject);
var
  StrCodigoVinculacao: String;
  MsgErroValidacao: String;
begin
  if Trim(edtCertificado.Text) = '' then
  begin
    edtCertificado.SetFocus;
    raise Exception.Create('Certificado não foi informado!');
  end;

  MsgErroValidacao := ValidarCNPJ(edtCNPJSoftwareHouse.Text);
  if Trim(MsgErroValidacao) <> '' then
  begin
    edtCNPJSoftwareHouse.SetFocus;
    raise Exception.Create('CNPJ da Software House inválido!');
  end;

  MsgErroValidacao := ValidarCNPJ(edtCNPJCliente.Text);
  if Trim(MsgErroValidacao) <> '' then
  begin
    edtCNPJCliente.SetFocus;
    raise Exception.Create('CNPJ do Cliente inválido!');
  end;

  ConfigurarDFe;

  StrCodigoVinculacao      := Onlynumber(edtCNPJSoftwareHouse.Text) + Onlynumber(edtCNPJCliente.Text);
  memCodigoVinculacao.Text := FACBrDFe.SSL.CalcHash(StrCodigoVinculacao, dgstSHA256, outBase64, True);

  ShowMessage('Código de vinculação gerado com sucesso');
  GravarConfiguracoes;
end;

procedure TfrmPrincipal.ConfigurarDFe;
begin
  if rbtTipoCapicom.Checked then
  begin
    FACBrDFe.Configuracoes.Geral.SSLLib             := libCapicom;
    FACBrDFe.Configuracoes.Certificados.NumeroSerie := edtCertificado.Text;
    FACBrDFe.Configuracoes.Certificados.Senha       := edtSenhaCertificado.Text
  end
  else
  begin
    FACBrDFe.Configuracoes.Geral.SSLLib             := libOpenSSL;
    FACBrDFe.Configuracoes.Certificados.ArquivoPFX  := edtCertificado.Text;
    FACBrDFe.Configuracoes.Certificados.Senha       := edtSenhaCertificado.Text;
  end;
end;

procedure TfrmPrincipal.GravarConfiguracoes;
var F: TIniFile;
begin
  F := TIniFile.Create(GetPathConfig);
  try
    F.WriteString('CONFIG', 'Certificado', edtCertificado.Text);
    F.WriteString('CONFIG', 'CNPJ_SH',     edtCNPJSoftwareHouse.Text);
    if rbtTipoCapicom.Checked then
      F.WriteString('CONFIG', 'Tipo', TIPO_CAPICOM)
    else
      F.WriteString('CONFIG', 'Tipo', TIPO_OPENSSL);
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
    edtCertificado.Text       := F.ReadString('CONFIG', 'Certificado', '');
    edtCNPJSoftwareHouse.Text := F.ReadString('CONFIG', 'CNPJ_SH', '');
    rbtTipoCapicom.Checked    := F.ReadString('CONFIG', 'Tipo', TIPO_CAPICOM) = TIPO_CAPICOM;
  finally
    F.Free;
  end;
end;

end.
