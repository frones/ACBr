unit uPrincipal;

{$mode objfpc}{$H+}
{$r BannerACBrSAC.rc}

interface

uses
  Windows,
  Classes, SysUtils, strutils, IniFiles, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, MaskEdit, Menus, ACBrGIF, ACBrUtil,
  ACBrValidador, ACBrEnterTab, ACBrDFeSSL;

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
    rbtTipoWinCrypt: TRadioButton;
    rbtTipoOpenSSL: TRadioButton;
    btnBuscarCertificado: TSpeedButton;
    procedure ACBrGIF1Click(Sender: TObject);
    procedure btnCopiarClick(Sender: TObject);
    procedure btnCriarAssinaturaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure LerConfiguracoes;
    procedure btnBuscarCertificadoClick(Sender: TObject);
    procedure rbtTipoWinCryptChange(Sender: TObject);
    procedure rbtTipoOpenSSLChange(Sender: TObject);
  private
    FDFeSSL: TDFeSSL;
    procedure ConfigurarDFe;
    function GetPathConfig: String;
    procedure GravarConfiguracoes;
    procedure CarregarGifBannerACBrSAC;
    function SelecionarCertificado: String;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  UtilUnit, Unit2;

const
  TIPO_WINCRYPT = 'WINCRYPT';
  TIPO_OPENSSL = 'OPENSSL';

{$R *.lfm}

{ TfrmPrincipal }

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  CarregarGifBannerACBrSAC;

  FDFeSSL := TDFeSSL.Create;

  edtCertificado.Clear;
  edtCNPJCliente.Clear;
  edtCNPJSoftwareHouse.Clear;
  memCodigoVinculacao.Clear;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  FDFeSSL.Free;
end;

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

function TfrmPrincipal.SelecionarCertificado: String;
var
  I: Integer;
begin
  Result := '';

  frSelecionarCertificado := TfrSelecionarCertificado.Create(Self);
  try
    FDFeSSL.LerCertificadosStore;

    For I := 0 to FDFeSSL.ListaCertificados.Count-1 do
    begin
      with FDFeSSL.ListaCertificados[I] do
      begin
        if (CNPJ <> '') then
        begin
          with frSelecionarCertificado.StringGrid1 do
          begin
            RowCount := RowCount + 1;
            Cells[ 0, RowCount-1] := NumeroSerie;
            Cells[ 1, RowCount-1] := RazaoSocial;
            Cells[ 2, RowCount-1] := CNPJ;
            Cells[ 3, RowCount-1] := FormatDateBr(DataVenc);
            Cells[ 4, RowCount-1] := Certificadora;
          end;
        end;
      end;
    end;

    frSelecionarCertificado.ShowModal;

    if frSelecionarCertificado.ModalResult = mrOK then
      Result := frSelecionarCertificado.StringGrid1.Cells[ 0,
                            frSelecionarCertificado.StringGrid1.Row];
  finally
     frSelecionarCertificado.Free;
  end;
end;

function TfrmPrincipal.GetPathConfig: String;
begin
  Result := ExtractFilePath(ParamStr(0)) + ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
end;

procedure TfrmPrincipal.ConfigurarDFe;
begin
  if rbtTipoWinCrypt.Checked then
  begin
    FDFeSSL.SSLCryptLib := cryWinCrypt;
    FDFeSSL.NumeroSerie := edtCertificado.Text
  end
  else
  begin
    FDFeSSL.SSLCryptLib := cryOpenSSL;
    FDFeSSL.ArquivoPFX  := edtCertificado.Text;
    FDFeSSL.Senha       := edtSenhaCertificado.Text;
  end;
end;

procedure TfrmPrincipal.GravarConfiguracoes;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetPathConfig);
  try
    Ini.WriteString('CONFIG', 'Certificado', edtCertificado.Text);
    Ini.WriteString('CONFIG', 'CNPJ_SH',     edtCNPJSoftwareHouse.Text);
    Ini.WriteString('CONFIG', 'Tipo',        IfThen(rbtTipoOpenSSL.Checked, TIPO_OPENSSL, TIPO_WINCRYPT));
    GravaINICrypt(Ini, 'Certificado', 'Senha', edtSenhaCertificado.Text, _C);
  finally
    Ini.Free;
  end;
end;

procedure TfrmPrincipal.LerConfiguracoes;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetPathConfig);
  try
    edtCertificado.Text       := Ini.ReadString('CONFIG', 'Certificado', '');
    edtCNPJSoftwareHouse.Text := Ini.ReadString('CONFIG', 'CNPJ_SH', '');
    rbtTipoOpenSSL.Checked    := Ini.ReadString('CONFIG', 'Tipo', TIPO_WINCRYPT) = TIPO_OPENSSL;
    rbtTipoWinCrypt.Checked   := not rbtTipoOpenSSL.Checked;
    edtSenhaCertificado.Text  := LeINICrypt(Ini, 'Certificado', 'Senha', _C);
  finally
    Ini.Free;
  end;
end;

procedure TfrmPrincipal.Image2Click(Sender: TObject);
begin
  OpenURL('http://www.projetoacbr.com.br/forum/sacv2/sobre/');
end;

procedure TfrmPrincipal.btnBuscarCertificadoClick(Sender: TObject);
begin
  if rbtTipoWinCrypt.Checked then
  begin
    FDFeSSL.SSLCryptLib := cryWinCrypt;
    edtCertificado.Text := SelecionarCertificado;
  end
  else
  begin
    FDFeSSL.SSLCryptLib := cryOpenSSL;
    if OpenDialog1.Execute then
      edtCertificado.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmPrincipal.rbtTipoWinCryptChange(Sender: TObject);
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
  memCodigoVinculacao.Text := FDFeSSL.CalcHash(StrCodigoVinculacao, dgstSHA256, outBase64, True);

  ShowMessage('Código de vinculação gerado com sucesso');
  GravarConfiguracoes;
end;

end.

