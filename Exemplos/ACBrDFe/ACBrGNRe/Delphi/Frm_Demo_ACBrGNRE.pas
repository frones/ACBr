{$I ACBr.inc}

unit Frm_Demo_ACBrGNRE;

interface

uses IniFiles, ShellAPI,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, OleCtrls, SHDocVw, StdCtrls, Buttons, ExtCtrls,
  pcnConversao, pgnreConversao, ACBrGNRE2, ACBrGNREGuiaClass,
  ACBrDFeUtil, ACBrBase, ACBrDFe, ACBrGNReGuiaRLClass;

type
  TfrmDemo_ACBrGNRE = class(TForm)
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    GroupBox1: TGroupBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label25: TLabel;
    sbtnGetCert: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    TabSheet2: TTabSheet;
    GroupBox3: TGroupBox;
    sbtnPathSalvar: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    TabSheet3: TTabSheet;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    ckVisualizar: TCheckBox;
    cbUF: TComboBox;
    rgTipoAmb: TRadioGroup;
    gbProxy: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    TabSheet4: TTabSheet;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edtEmitCNPJ: TEdit;
    edtEmitIE: TEdit;
    edtEmitRazao: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitCEP: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitComp: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCodCidade: TEdit;
    edtEmitCidade: TEdit;
    edtEmitUF: TEdit;
    TabSheet7: TTabSheet;
    GroupBox5: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    edtSmtpHost: TEdit;
    edtSmtpPort: TEdit;
    edtSmtpUser: TEdit;
    edtSmtpPass: TEdit;
    edtEmailAssunto: TEdit;
    cbEmailSSL: TCheckBox;
    mmEmailMsg: TMemo;
    btnSalvarConfig: TBitBtn;
    Panel2: TPanel;
    Panel3: TPanel;
    btnImprimir: TButton;
    btnConsultaConfigUF: TButton;
    btnCriarEnviar: TButton;
    btnGerarMDFe: TButton;
    btnGerarPDF: TButton;
    btnConsultarRecibo: TButton;
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
    WBResposta: TWebBrowser;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet10: TTabSheet;
    memoRespWS: TMemo;
    Dados: TTabSheet;
    MemoDados: TMemo;
    OpenDialog1: TOpenDialog;
    ACBrGNRE1: TACBrGNRE;
    ACBrGNREGuiaRL1: TACBrGNREGuiaRL;
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure btnConsultaConfigUFClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnConsultarReciboClick(Sender: TObject);
    procedure btnGerarGNREClick(Sender: TObject);
    procedure btnGerarPDFClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure ACBrGNRE1StatusChange(Sender: TObject);
    procedure ACBrGNRE1GerarLog(const ALogLine: String;
      var Tratado: Boolean);
    {
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    }
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure LoadXML(MyMemo: TMemo; MyWebBrowser: TWebBrowser);
    procedure GerarGNRE;
  public
    { Public declarations }
  end;

var
  frmDemo_ACBrGNRE: TfrmDemo_ACBrGNRE;

implementation

uses
 FileCtrl, DateUtils,
 ufrmStatus,
 ACBrGNREGuias, ACBrGNREConfiguracoes, pcnAuxiliar;

const
  SELDIRHELP = 1000;

{$R *.dfm}

(*
procedure TForm1.lblMouseEnter(Sender: TObject);
begin
 TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TForm1.lblMouseLeave(Sender: TObject);
begin
 TLabel(Sender).Font.Style := [fsBold];
end;
*)
procedure TfrmDemo_ACBrGNRE.GravarConfiguracao;
var
 IniFile    : String;
 Ini        : TIniFile;
 StreamMemo : TMemoryStream;
begin
 IniFile := ChangeFileExt( Application.ExeName, '.ini');

 Ini := TIniFile.Create( IniFile );
 try
  Ini.WriteString( 'Certificado','Caminho' ,edtCaminho.Text);
  Ini.WriteString( 'Certificado','Senha'   ,edtSenha.Text);
  Ini.WriteString( 'Certificado','NumSerie',edtNumSerie.Text);

  Ini.WriteBool(   'Geral','Salvar'      ,ckSalvar.Checked);
  Ini.WriteString( 'Geral','PathSalvar'  ,edtPathLogs.Text);

  Ini.WriteString( 'WebService','UF'        ,cbUF.Text);
  Ini.WriteInteger( 'WebService','Ambiente'  ,rgTipoAmb.ItemIndex);
  Ini.WriteBool(   'WebService','Visualizar',ckVisualizar.Checked);

  Ini.WriteString( 'Proxy','Host'   ,edtProxyHost.Text);
  Ini.WriteString( 'Proxy','Porta'  ,edtProxyPorta.Text);
  Ini.WriteString( 'Proxy','User'   ,edtProxyUser.Text);
  Ini.WriteString( 'Proxy','Pass'   ,edtProxySenha.Text);

  Ini.WriteString( 'Emitente','CNPJ'       ,edtEmitCNPJ.Text);
  Ini.WriteString( 'Emitente','IE'         ,edtEmitIE.Text);
  Ini.WriteString( 'Emitente','RazaoSocial',edtEmitRazao.Text);
  Ini.WriteString( 'Emitente','Fantasia'   ,edtEmitFantasia.Text);
  Ini.WriteString( 'Emitente','Fone'       ,edtEmitFone.Text);
  Ini.WriteString( 'Emitente','CEP'        ,edtEmitCEP.Text);
  Ini.WriteString( 'Emitente','Logradouro' ,edtEmitLogradouro.Text);
  Ini.WriteString( 'Emitente','Numero'     ,edtEmitNumero.Text);
  Ini.WriteString( 'Emitente','Complemento',edtEmitComp.Text);
  Ini.WriteString( 'Emitente','Bairro'     ,edtEmitBairro.Text);
  Ini.WriteString( 'Emitente','CodCidade'  ,edtEmitCodCidade.Text);
  Ini.WriteString( 'Emitente','Cidade'     ,edtEmitCidade.Text);
  Ini.WriteString( 'Emitente','UF'         ,edtEmitUF.Text);

  Ini.WriteString( 'Email','Host'    ,edtSmtpHost.Text);
  Ini.WriteString( 'Email','Port'    ,edtSmtpPort.Text);
  Ini.WriteString( 'Email','User'    ,edtSmtpUser.Text);
  Ini.WriteString( 'Email','Pass'    ,edtSmtpPass.Text);
  Ini.WriteString( 'Email','Assunto' ,edtEmailAssunto.Text);
  Ini.WriteBool(   'Email','SSL'     ,cbEmailSSL.Checked );

  StreamMemo := TMemoryStream.Create;
  mmEmailMsg.Lines.SaveToStream(StreamMemo);
  StreamMemo.Seek(0,soFromBeginning);
  Ini.WriteBinaryStream( 'Email','Mensagem',StreamMemo);

  StreamMemo.Free;
 finally
  Ini.Free;
 end;
end;

procedure TfrmDemo_ACBrGNRE.LerConfiguracao;
var
 IniFile    : String;
 Ini        : TIniFile;
 Ok         : Boolean;
 StreamMemo : TMemoryStream;
 Caminho    : String;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );
  try
    {$IFDEF ACBrGNREOpenSSL}
    edtCaminho.Text  := Ini.ReadString( 'Certificado','Caminho' ,'');
    edtSenha.Text    := Ini.ReadString( 'Certificado','Senha'   ,'');
    ACBrGNRE1.Configuracoes.Certificados.Certificado  := edtCaminho.Text;
    ACBrGNRE1.Configuracoes.Certificados.Senha        := edtSenha.Text;
    edtNumSerie.Visible := False;
    Label25.Visible     := False;
    sbtnGetCert.Visible := False;
    {$ELSE}
    edtNumSerie.Text := Ini.ReadString( 'Certificado','NumSerie','');
    ACBrGNRE1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;
    //edtNumSerie.Text := ACBrGNRE1.Configuracoes.Certificados.NumeroSerie;
    Label1.Caption := 'Informe o número de série do certificado'#13+
                      'Disponível no Internet Explorer no menu'#13+
                      'Ferramentas - Opções da Internet - Conteúdo '#13+
                      'Certificados - Exibir - Detalhes - '#13+
                      'Número do certificado';
    Label2.Visible     := False;
    edtCaminho.Visible := False;
    edtSenha.Visible   := False;
    sbtnCaminhoCert.Visible := False;
    {$ENDIF}

    ckSalvar.Checked         := Ini.ReadBool(   'Geral','Salvar'      ,True);
    edtPathLogs.Text         := Ini.ReadString( 'Geral','PathSalvar'  ,'');

    ACBrGNRE1.Configuracoes.Geral.Salvar       := ckSalvar.Checked;

    cbUF.ItemIndex       := cbUF.Items.IndexOf(Ini.ReadString('WebService','UF','SP'));
    rgTipoAmb.ItemIndex  := Ini.ReadInteger('WebService','Ambiente'  ,0);
    ckVisualizar.Checked :=Ini.ReadBool(    'WebService','Visualizar',False);
    ACBrGNRE1.Configuracoes.WebServices.UF         := cbUF.Text;
    ACBrGNRE1.Configuracoes.WebServices.Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    ACBrGNRE1.Configuracoes.WebServices.Visualizar := ckVisualizar.Checked;

    ACBrGNRE1.Configuracoes.Arquivos.Salvar           := True;
    ACBrGNRE1.Configuracoes.Arquivos.SepararPorMes    := True;
    ACBrGNRE1.Configuracoes.Arquivos.AdicionarLiteral := True;
    ACBrGNRE1.Configuracoes.Arquivos.PathGNRE         := edtPathLogs.Text;

    Caminho := ACBrGNRE1.Configuracoes.Arquivos.GetPathGNRE(0);

    ACBrGNRE1.Configuracoes.Arquivos.PathSalvar := Caminho;

    edtProxyHost.Text  := Ini.ReadString( 'Proxy','Host'   ,'');
    edtProxyPorta.Text := Ini.ReadString( 'Proxy','Porta'  ,'');
    edtProxyUser.Text  := Ini.ReadString( 'Proxy','User'   ,'');
    edtProxySenha.Text := Ini.ReadString( 'Proxy','Pass'   ,'');
    ACBrGNRE1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
    ACBrGNRE1.Configuracoes.WebServices.ProxyPort := edtProxyPorta.Text;
    ACBrGNRE1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
    ACBrGNRE1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;

//    if ACBrGNRE1.GNREGuia <> nil then
//      ACBrGNRE1.GNREGuia.PathPDF    := Caminho;

    edtEmitCNPJ.Text       := Ini.ReadString( 'Emitente','CNPJ'       ,'');
    edtEmitIE.Text         := Ini.ReadString( 'Emitente','IE'         ,'');
    edtEmitRazao.Text      := Ini.ReadString( 'Emitente','RazaoSocial','');
    edtEmitFantasia.Text   := Ini.ReadString( 'Emitente','Fantasia'   ,'');
    edtEmitFone.Text       := Ini.ReadString( 'Emitente','Fone'       ,'');
    edtEmitCEP.Text        := Ini.ReadString( 'Emitente','CEP'        ,'');
    edtEmitLogradouro.Text := Ini.ReadString( 'Emitente','Logradouro' ,'');
    edtEmitNumero.Text     := Ini.ReadString( 'Emitente','Numero'     ,'');
    edtEmitComp.Text       := Ini.ReadString( 'Emitente','Complemento','');
    edtEmitBairro.Text     := Ini.ReadString( 'Emitente','Bairro'     ,'');
    edtEmitCodCidade.Text  := Ini.ReadString( 'Emitente','CodCidade'  ,'');
    edtEmitCidade.Text     := Ini.ReadString( 'Emitente','Cidade'     ,'');
    edtEmitUF.Text         := Ini.ReadString( 'Emitente','UF'         ,'');

    edtSmtpHost.Text      := Ini.ReadString( 'Email','Host'   ,'');
    edtSmtpPort.Text      := Ini.ReadString( 'Email','Port'   ,'');
    edtSmtpUser.Text      := Ini.ReadString( 'Email','User'   ,'');
    edtSmtpPass.Text      := Ini.ReadString( 'Email','Pass'   ,'');
    edtEmailAssunto.Text  := Ini.ReadString( 'Email','Assunto','');
    cbEmailSSL.Checked    := Ini.ReadBool(   'Email','SSL'    ,False);

    StreamMemo := TMemoryStream.Create;
    Ini.ReadBinaryStream( 'Email','Mensagem',StreamMemo);
    mmEmailMsg.Lines.LoadFromStream(StreamMemo);
    StreamMemo.Free;
   finally
    Ini.Free;
   end;
end;

procedure TfrmDemo_ACBrGNRE.LoadXML(MyMemo: TMemo;
  MyWebBrowser: TWebBrowser);
begin
  MyMemo.Lines.SaveToFile(ExtractFileDir(application.ExeName)+'temp.xml');
  MyWebBrowser.Navigate(ExtractFileDir(application.ExeName)+'temp.xml');
end;

procedure TfrmDemo_ACBrGNRE.GerarGNRE;
begin
   {
  with ACBrGNRE1.Guias.Add.GNRE do
  begin
    c01_UfFavorecida := 'PR';
    c02_receita := 100099;
    c28_tipoDocOrigem := 10;
    c04_docOrigem := '999999';
    c06_valorPrincipal := 100.50;
    c14_dataVencimento := Now;
    c15_convenio := '16461313';
    c17_inscricaoEstadualEmitente := '9023725557';
    c33_dataPagamento := Now;
  end;

  with ACBrGNRE1.Guias.Add.GNRE do
  begin
    c01_UfFavorecida := 'PR';
    c02_receita := 100099;
    c28_tipoDocOrigem := 10;
    c04_docOrigem := '888888';
    c06_valorPrincipal := 200.33;
    c14_dataVencimento := Now;
    c15_convenio := '16461313';
    c17_inscricaoEstadualEmitente := '9023725557';
    c33_dataPagamento := Now;
  end;
  }

  with ACBrGNRE1.Guias.Add.GNRE do
  begin
    c01_UfFavorecida := 'PR';
    c02_receita := 100099;
    c28_tipoDocOrigem := 10;
    c04_docOrigem := '777777';
    c06_valorPrincipal := 120.50;
    c14_dataVencimento := Now;
    c15_convenio := '16461313';
    c17_inscricaoEstadualEmitente := '9023725557';
    c33_dataPagamento := Now;
  end;
end;

procedure TfrmDemo_ACBrGNRE.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
  begin
    edtCaminho.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmDemo_ACBrGNRE.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrGNRE1.SSL.SelecionarCertificado;
end;

procedure TfrmDemo_ACBrGNRE.sbtnPathSalvarClick(Sender: TObject);
var
 Dir : string;
begin
  if Length(edtPathLogs.Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := edtPathLogs.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
    edtPathLogs.Text := Dir;
end;

procedure TfrmDemo_ACBrGNRE.FormCreate(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrmDemo_ACBrGNRE.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  LerConfiguracao;
end;

procedure TfrmDemo_ACBrGNRE.lblColaboradorClick(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/5', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrGNRE.lblPatrocinadorClick(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/35', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrGNRE.lblDoar1Click(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/14', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrGNRE.btnConsultaConfigUFClick(Sender: TObject);
var
  aux : String;
begin
  aux := '';
  if not(InputQuery('Consulta Configuração UF', 'Uf', aux))
   then exit;
  ACBrGNRE1.WebServices.ConsultaUF.Uf := aux;

  aux := '';
  if not(InputQuery('Consulta Configuração UF', 'Receita', aux))
   then exit;
  ACBrGNRE1.WebServices.ConsultaUF.receita := StrToIntDef(aux, 0);

  ACBrGNRE1.WebServices.ConsultaUF.Executar;

  MemoResp.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.ConsultaUF.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.ConsultaUF.RetWS);
  LoadXML(MemoResp, WBResposta);

  PageControl2.ActivePageIndex := 4;
  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Consulta Configuração UF');
  MemoDados.Lines.Add('ambiente: '    +TpAmbToStr(ACBrGNRE1.WebServices.ConsultaUF.ambiente));
  MemoDados.Lines.Add('codigo: '    +IntToStr(ACBrGNRE1.WebServices.ConsultaUF.codigo));
  MemoDados.Lines.Add('descricao: '  +ACBrGNRE1.WebServices.ConsultaUF.descricao);
  MemoDados.Lines.Add('Uf: '      +ACBrGNRE1.WebServices.ConsultaUF.Uf);
  MemoDados.Lines.Add('exigeUfFavorecida : ' + IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeUfFavorecida = 'S', 'SIM', 'NÃO'));
  MemoDados.Lines.Add('exigeReceita: '     + IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeReceita = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeContribuinteEmitente: '+ IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeContribuinteEmitente = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeDataVencimento: '     + IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeDataVencimento = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeConvenio: '+ IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeConvenio = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeDataPagamento: '+ IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeDataPagamento = 'S', 'SIM', 'NÃO'));
end;

procedure TfrmDemo_ACBrGNRE.btnCriarEnviarClick(Sender: TObject);
begin
  ACBrGNRE1.Guias.Clear;
  GerarGNRE;
  ACBrGNRE1.Enviar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  LoadXML(MemoResp, WBResposta);

  PageControl2.ActivePageIndex := 4;
  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Envio GNRE');
  MemoDados.Lines.Add('ambiente: '+ TpAmbToStr(ACBrGNRE1.WebServices.Retorno.ambiente));
  MemoDados.Lines.Add('codigo: '+ IntToStr(ACBrGNRE1.WebServices.Retorno.codigo));
  MemoDados.Lines.Add('descricao: '+ ACBrGNRE1.WebServices.Retorno.descricao);
  MemoDados.Lines.Add('Recibo: '+ ACBrGNRE1.WebServices.Retorno.numeroRecibo);
  MemoDados.Lines.Add('Protocolo: '+ ACBrGNRE1.WebServices.Retorno.protocolo);

  ACBrGNRE1.Guias.Clear;
end;

procedure TfrmDemo_ACBrGNRE.btnConsultarReciboClick(Sender: TObject);
var
 aux : String;
begin
  if not(InputQuery('Consultar Recibo Lote', 'Número do Recibo', aux))
   then exit;

  ACBrGNRE1.WebServices.Retorno.numeroRecibo := aux;
  ACBrGNRE1.WebServices.Retorno.Executar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrGNRE.btnGerarGNREClick(Sender: TObject);
begin
  ACBrGNRE1.Guias.Clear;
  GerarGNRE;
  ACBrGNRE1.Guias.Items[0].GravarXML;

  ShowMessage('Arquivo gerado em: '+ACBrGNRE1.Guias.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: '+ACBrGNRE1.Guias.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrGNRE1.Guias.Items[0].NomeArq);
  LoadXML(MemoResp, WBResposta);
  PageControl2.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrGNRE.btnGerarPDFClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o GNRE';
  OpenDialog1.DefaultExt := '*-gnre.txt';
  OpenDialog1.Filter := 'Arquivos GNRE (*-gnre.txt)|*-gnre.txt|Arquivos TXT (*.txt)|*.txt|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrGNRE1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrGNRE1.GuiasRetorno.Clear;
    ACBrGNRE1.GuiasRetorno.LoadFromFile(OpenDialog1.FileName);
//    TACBrGNREGuiaFR(ACBrGNRE1.GNREGuia).FastFile :=
//      IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Report\GNRE_GUIA.fr3' ;
    ACBrGNRE1.GuiasRetorno.ImprimirPDF;
  end;
end;

procedure TfrmDemo_ACBrGNRE.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a Guia';
  OpenDialog1.DefaultExt := '*-gnre.txt';
  OpenDialog1.Filter := 'Arquivos GNRE (*-gnre.txt)|*-gnre.txt|Arquivos TXT (*.txt)|*.txt|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrGNRE1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrGNRE1.GuiasRetorno.Clear;
    ACBrGNRE1.GuiasRetorno.LoadFromFile(OpenDialog1.FileName);
//    TACBrGNREGuiaFR(ACBrGNRE1.GNREGuia).FastFile :=
//      IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Report\GNRE_GUIA.fr3' ;
    ACBrGNRE1.GuiasRetorno.Imprimir;
  end;
end;

procedure TfrmDemo_ACBrGNRE.ACBrGNRE1StatusChange(Sender: TObject);

  procedure MostrarStatus(const AMsg: string);
  begin
    if AMsg = '' then
    begin
      if ( frmStatus <> nil ) then
        frmStatus.Hide
    end
    else
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := AMsg;
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
  end;

begin
  case ACBrGNRE1.Status of
    stGNREIdle: MostrarStatus('');
    stGNRERecepcao: MostrarStatus('Enviando dados do GNRE...');
    stGNRERetRecepcao: MostrarStatus('Recebendo dados do GNRE...');
    stGNREConsulta: MostrarStatus('Consultando Lote GNRE...');
  end;
  Application.ProcessMessages;
end;

procedure TfrmDemo_ACBrGNRE.ACBrGNRE1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

end.
