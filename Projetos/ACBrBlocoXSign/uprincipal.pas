{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit uPrincipal;

{$mode objfpc}{$H+}
{$r BannerACBrSAC.rc}

interface

uses
  Windows, Classes, SysUtils, strutils, dateutils, IniFiles, FileUtil, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, MaskEdit, Menus,
  ComCtrls, Spin, EditBtn, UtilUnit, ACBrGIF, ACBrEnterTab, ACBrDFe,
  ACBrDFeSSL, ACBrDFeWebService, ACBrBlocoX, ACBrDFeUtil;

const
  _C = 'tYk*5W@';

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    ACBrBlocoX1: TACBrBlocoX;
    ACBrEnterTab1: TACBrEnterTab;
    btnBuscarCertificado: TSpeedButton;
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
    edtCertificado: TEdit;
    edtSenhaCertificado: TEdit;
    gbProxy: TGroupBox;
    lbCertificado: TLabel;
    lbSenhaCertificado: TLabel;
    lblProxyPorta: TLabel;
    lblProxyUser: TLabel;
    lblProxySenha: TLabel;
    lblProxyHost: TLabel;
    mmRetornoBlocoX: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnComandos: TPanel;
    rbtTipoCapicom: TRadioButton;
    rbtTipoOpenSSL: TRadioButton;
    SaveDialog1: TSaveDialog;
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
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  synautil, LazFileUtils, ACBrUtil.FilesIO, ACBrBlocoX_WebServices;

const
  TIPO_CAPICOM = 'CAPICOM';
  TIPO_OPENSSL = 'OPENSSL';

{$R *.lfm}

{ TfrmPrincipal }

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
begin
  edtCertificado.Clear;

  LerConfiguracoes;

  ConfigurarDFe;

  if UpperCase(ParamStr(2)) = '/C' then
  begin
    ACBrBlocoX1.ConsultarProcessamentoArquivo.Recibo := ParamStr(1);
    ACBrBlocoX1.WebServices.ConsultarProcessamentoArquivoBlocoX.Executar;

    mmRetornoBlocoX.Text := ACBrBlocoX1.WebServices.ConsultarProcessamentoArquivoBlocoX.RetWS;
    WriteToTXT(ExtractFileNameWithoutExt(ParamStr(1))+'consultar-resposta.'+ExtractFileExt(ParamStr(1)),mmRetornoBlocoX.Text);
    Application.Terminate;
  end;
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
  wRecibo := EmptyStr;
  wMotivo := EmptyStr;

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
  wRecibo := EmptyStr;
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
  wIE := EmptyStr;
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
  wCNPJ := EmptyStr;
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
  wRecibo := EmptyStr;
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
  wRecibo := EmptyStr;
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
  wIE := EmptyStr;
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

procedure TfrmPrincipal.btReprocessarArqClick(Sender: TObject);
var
  wRecibo: String;
  wXML: AnsiString;
begin
  wRecibo := EmptyStr;
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

