unit uPrincipal;

{$mode objfpc}{$H+}
{$r BannerACBrSAC.rc}

interface

uses
  Windows,
  Classes, SysUtils, strutils, IniFiles, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, MaskEdit, Menus, ACBrGIF, ACBrUtil,
  ACBrValidador, ACBrEnterTab, ACBrDFe, ACBrDFeSSL;

const
  _C = 'tYk*5W@';

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    ACBrEnterTab1: TACBrEnterTab;
    ACBrGIF1: TACBrGIF;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnCopiar: TSpeedButton;
    btnCriarAssinatura: TBitBtn;
    edtCertificado: TEdit;
    edtSenhaCertificado: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtCNPJSoftwareHouse: TMaskEdit;
    edtCNPJCliente: TMaskEdit;
    Label5: TLabel;
    memCodigoVinculacao: TMemo;
    OpenDialog1: TOpenDialog;
    rbtTipoCapicom: TRadioButton;
    rbtTipoOpenSSL: TRadioButton;
    btnBuscarCertificado: TSpeedButton;
    procedure ACBrGIF1Click(Sender: TObject);
    procedure btnCopiarClick(Sender: TObject);
    procedure btnCriarAssinaturaClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure LerConfiguracoes;
    procedure btnBuscarCertificadoClick(Sender: TObject);
    procedure rbtTipoCapicomChange(Sender: TObject);
    procedure rbtTipoOpenSSLChange(Sender: TObject);
  private
    FACBrDFe: TACBrDFe;
    procedure ConfigurarDFe;
    function GetPathConfig: String;
    procedure GravarConfiguracoes;
    procedure CarregarGifBannerACBrSAC;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  UtilUnit;

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

function TfrmPrincipal.GetPathConfig: String;
begin
  Result := ExtractFilePath(ParamStr(0)) + ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
end;

procedure TfrmPrincipal.ConfigurarDFe;
begin
  if rbtTipoCapicom.Checked then
  begin
    FACBrDFe.Configuracoes.Geral.SSLLib             := libCapicom;
    FACBrDFe.Configuracoes.Certificados.NumeroSerie := edtCertificado.Text
  end
  else
  begin
    FACBrDFe.Configuracoes.Geral.SSLLib             := libOpenSSL;
    FACBrDFe.Configuracoes.Certificados.ArquivoPFX  := edtCertificado.Text;
    FACBrDFe.Configuracoes.Certificados.Senha       := edtSenhaCertificado.Text;
  end;
end;

procedure TfrmPrincipal.GravarConfiguracoes;
var
  F: TIniFile;
begin
  F := TIniFile.Create(GetPathConfig);
  try
    F.WriteString('CONFIG', 'Certificado', edtCertificado.Text);
    F.WriteString('CONFIG', 'CNPJ_SH',     edtCNPJSoftwareHouse.Text);
    F.WriteString('CONFIG', 'Tipo',        IfThen(rbtTipoCapicom.Checked, TIPO_CAPICOM, TIPO_OPENSSL));
    GravaINICrypt(F, 'Certificado', 'Senha', edtSenhaCertificado.Text, _C);
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
    rbtTipoOpenSSL.Checked    := F.ReadString('CONFIG', 'Tipo', TIPO_CAPICOM) = TIPO_OPENSSL;
    edtSenhaCertificado.Text := LeINICrypt(F, 'Certificado', 'Senha', _C);
  finally
    F.Free;
  end;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  CarregarGifBannerACBrSAC;

  FACBrDFe := TACBrDFe.Create(Self);

  edtCertificado.Clear;
  edtCNPJCliente.Clear;
  edtCNPJSoftwareHouse.Clear;
  memCodigoVinculacao.Clear;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FACBrDFe.Free;
end;

procedure TfrmPrincipal.Image2Click(Sender: TObject);
begin
  OpenURL('http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html');
end;

procedure TfrmPrincipal.btnBuscarCertificadoClick(Sender: TObject);
begin
  if rbtTipoCapicom.Checked then
  begin
    FACBrDFe.Configuracoes.Geral.SSLLib := libCapicom;
    edtCertificado.Text := FACBrDFe.SSL.SelecionarCertificado
  end
  else
  begin
    FACBrDFe.Configuracoes.Geral.SSLLib := libOpenSSL;
    if OpenDialog1.Execute then
      edtCertificado.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmPrincipal.rbtTipoCapicomChange(Sender: TObject);
begin
  edtCertificado.ReadOnly := True;
end;

procedure TfrmPrincipal.rbtTipoOpenSSLChange(Sender: TObject);
begin
  edtCertificado.ReadOnly := False;
end;

procedure TfrmPrincipal.btnCopiarClick(Sender: TObject);
begin
  memCodigoVinculacao.CopyToClipboard;
  ShowMessage('Copiado para a área de transferência!');
end;

procedure TfrmPrincipal.ACBrGIF1Click(Sender: TObject);
begin
  OpenURL('http://www.projetoacbr.com.br/forum/SAC/cadastro/');
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

end.

