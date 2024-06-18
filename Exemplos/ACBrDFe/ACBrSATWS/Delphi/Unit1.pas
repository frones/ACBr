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

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrDFe, ACBrSATWS, ComCtrls, ACBrDFeSSL,
  Buttons, ExtCtrls, Spin, DateUtils;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ednserieSAT: TEdit;
    eddhInicial: TEdit;
    eddhFinal: TEdit;
    edchaveSeguranca: TEdit;
    Button1: TButton;
    trvwNFe: TTreeView;
    ACBrSATWS1: TACBrSATWS;
    PanelClient: TPanel;
    PageControl1: TPageControl;
    TabConfiguracoes: TTabSheet;
    Panel1: TPanel;
    Shape1: TShape;
    btnSalvarConfig: TBitBtn;
    Label18: TLabel;
    PageControl2: TPageControl;
    TabGeral: TTabSheet;
    gbGeral: TGroupBox;
    cbxExibirErrosSchema: TCheckBox;
    Label11: TLabel;
    edtFormatoAlerta: TEdit;
    Label12: TLabel;
    cbFormaEmissao: TComboBox;
    Label13: TLabel;
    cbxRetirarAcentos: TCheckBox;
    cbxRetirarEspacos: TCheckBox;
    cbVersaoDados: TComboBox;
    gbLib: TGroupBox;
    Label14: TLabel;
    cbSSLLib: TComboBox;
    Label15: TLabel;
    cbCryptLib: TComboBox;
    Label16: TLabel;
    cbHttpLib: TComboBox;
    cbXMLSignLib: TComboBox;
    Label17: TLabel;
    TabWebService: TTabSheet;
    gbProxy: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    gbWebService: TGroupBox;
    rgTipoAmbiente: TRadioGroup;
    cbxSalvarSOAP: TCheckBox;
    Label5: TLabel;
    seTimeOut: TSpinEdit;
    cbxVisualizar: TCheckBox;
    Label6: TLabel;
    cbSSLType: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure cbVersaoDadosChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXMLSignLibChange(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label18Click(Sender: TObject);
  private
    procedure GravarConfiguracao;
    procedure AtualizarSSLLibsCombo;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ACBrUtil, pcnConversao, blcksock, TypInfo, IniFiles;

{$R *.dfm}

procedure TForm1.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I, J : Integer;
  Lote, Node: TTreeNode;
begin
  trvwNFe.Items.Clear;

  ACBrSATWS1.WebServices.ConsultarSATWS.nserieSAT := StrToIntDef(ednserieSAT.Text,0);
  ACBrSATWS1.WebServices.ConsultarSATWS.dhInicial := StrToDateTime(eddhInicial.text);
  ACBrSATWS1.WebServices.ConsultarSATWS.dhFinal := StrToDateTime(eddhFinal.text);
  ACBrSATWS1.WebServices.ConsultarSATWS.chaveSeguranca := edchaveSeguranca.Text;
  if ACBrSATWS1.WebServices.ConsultarSATWS.Executar then
  begin
    trvwNFe.AutoExpand := True;

    if ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Lote.Count <= 0 then
    begin
      ShowMessage(ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Mensagem);
      exit;
    end;

    for I:=0 to ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Lote.Count-1 do
    begin
      with ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Lote[I] do
      begin
        Lote := trvwNFe.Items.Add(nil,NRec);
        trvwNFe.Items.AddChild(Lote,'NRec= ' +NRec);
        trvwNFe.Items.AddChild(Lote,'dhEnvioLote= ' +DateTimeToStr(dhEnvioLote));
        trvwNFe.Items.AddChild(Lote,'dhProcessamento= ' +DateTimeToStr(dhProcessamento));
        trvwNFe.Items.AddChild(Lote,'TipoLote= ' +TipoLote);
        trvwNFe.Items.AddChild(Lote,'Origem= ' +Origem);
        trvwNFe.Items.AddChild(Lote,'QtdeCupoms= ' +IntToStr(QtdeCupoms));
        trvwNFe.Items.AddChild(Lote,'SituacaoLote= ' +SituacaoLote);
        for J:=0 to InfCFe.Count - 1 do
        begin
          Node := trvwNFe.Items.AddChild(Lote,'CFe');
          trvwNFe.Items.AddChild(Node,'Chave= '  +InfCFe[J].Chave);
          trvwNFe.Items.AddChild(Node,'nCupom= '  +InfCFe[J].nCupom);
          trvwNFe.Items.AddChild(Node,'vCFe= '  + FormatFloatBr( InfCFe[J].vCFe ));
          trvwNFe.Items.AddChild(Node,'dEmi + hEmi= '  + FormatDateTimeBr( InfCFe[J].dEmi + InfCFe[J].hEmi ));
          trvwNFe.Items.AddChild(Node,'Situacao= '  +InfCFe[J].Situacao);
          trvwNFe.Items.AddChild(Node,'Erros= '  +InfCFe[J].Erros);
        end;
      end;
    end;
  end
  else
    ShowMessage(ACBrSATWS1.WebServices.ConsultarSATWS.ConsultaRet.Mensagem);

end;  

procedure TForm1.cbCryptLibChange(Sender: TObject);
begin
  try
    if(cbCryptLib.ItemIndex <> -1)then
      ACBrSATWS1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TForm1.cbHttpLibChange(Sender: TObject);
begin
  try
    if(cbHttpLib.ItemIndex <> -1)then
      ACBrSATWS1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TForm1.cbSSLLibChange(Sender: TObject);
begin
  try
    if(cbSSLLib.ItemIndex <> -1)then
      ACBrSATWS1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TForm1.cbSSLTypeChange(Sender: TObject);
begin
  if(cbSSLType.ItemIndex <> -1)then
    ACBrSATWS1.Configuracoes.WebServices.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TForm1.cbVersaoDadosChange(Sender: TObject);
begin
  ACBrSATWS1.WebServices.ConsultarSATWS.versaoDados := cbVersaoDados.Text;
end;

procedure TForm1.cbXMLSignLibChange(Sender: TObject);
begin
  try
    if(cbXMLSignLib.ItemIndex <> -1)then
      ACBrSATWS1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXMLSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  J: TSSLCryptLib;
  K: TSSLHttpLib;
  L: TSSLXmlSignLib;
  M: TSSLType;
begin
  eddhInicial.Text := FormatDateTime('DD/MM/YYYY HH:NN:SS', IncDay(Now,-10));
  eddhFinal.Text   := FormatDateTime('DD/MM/YYYY HH:NN:SS', Now);

  cbFormaEmissao.Clear;
  for I:=Low(TpcnTipoEmissao) to High(TpcnTipoEmissao)do
    cbFormaEmissao.Items.Add(GetEnumName(TypeInfo(TpcnTipoEmissao), Integer(I)));
  cbFormaEmissao.ItemIndex := 0;

  cbSSLLib.Clear;
  for T:= Low(TSSLLib) to High(TSSLLib)do
    cbSSLLib.Items.Add(GetEnumName(TypeInfo(TSSLLib), Integer(T)));
  cbSSLLib.ItemIndex := 0;

  cbCryptLib.Clear;
  for J:= Low(TSSLCryptLib) to High(TSSLCryptLib)do
    cbCryptLib.Items.Add(GetEnumName(TypeInfo(TSSLCryptLib), Integer(J)));
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Clear;
  for K:=Low(TSSLHttpLib) to High(TSSLHttpLib)do
    cbHttpLib.Items.Add(GetEnumName(TypeInfo(TSSLHttpLib), Integer(K)));
  cbHttpLib.ItemIndex := 0;

  cbXMLSignLib.Clear;
  for L:=Low(TSSLXmlSignLib) to High(TSSLXmlSignLib)do
    cbXMLSignLib.Items.Add(GetEnumName(TypeInfo(TSSLXmlSignLib), Integer(L)));
  cbXMLSignLib.ItemIndex := 0;

  cbSSLType.Clear;
  for M:=Low(TSSLType) to High(TSSLType)do
    cbSSLType.Items.Add(GetEnumName(TypeInfo(TSSLType), Integer(M)));
  cbSSLType.ItemIndex := 0;

  LerConfiguracao;
end;

procedure TForm1.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');
  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteBool('Geral'  , 'ExibirErroSchema', cbxExibirErrosSchema.Checked);
    Ini.WriteString('Geral', 'FormatoAlerta', edtFormatoAlerta.Text);
    Ini.WriteInteger('Geral', 'FormaEmissao', cbFormaEmissao.ItemIndex);
    Ini.WriteInteger('Geral', 'VersaoDados', cbVersaoDados.ItemIndex);
    Ini.WriteBool('Geral', 'RetirarAcentos', cbxRetirarAcentos.Checked);
    Ini.WriteBool('Geral', 'RetirarEspacos', cbxRetirarEspacos.Checked);

    Ini.WriteInteger('Geral', 'SSLLib', cbSSLLib.ItemIndex);
    Ini.WriteInteger('Geral', 'CryptLib', cbCryptLib.ItemIndex);
    Ini.WriteInteger('Geral', 'HttpLib', cbHttpLib.ItemIndex);
    Ini.WriteInteger('Geral', 'XMLSignLib', cbXMLSignLib.ItemIndex);

    Ini.WriteInteger('WebService', 'Ambiente', rgTipoAmbiente.ItemIndex);
    Ini.WriteBool('WebService', 'VisualizarMensagem', cbxVisualizar.Checked);
    Ini.WriteBool('WebService', 'SalvarSOAP', cbxSalvarSOAP.Checked);
    Ini.WriteInteger('WebService', 'TimeOut', seTimeOut.Value);
    Ini.WriteInteger('WebService', 'SSLType', cbSSLType.ItemIndex);

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TForm1.Label18Click(Sender: TObject);
begin
  OpenURL('https://projetoacbr.com.br/');
end;

procedure TForm1.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');
  Ini := TIniFile.Create(IniFile);
  try
    cbxExibirErrosSchema.Checked := Ini.ReadBool('Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text := Ini.ReadString('Geral', 'FormatoAlerta', 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex := Ini.ReadInteger('Geral', 'FormaEmissao', Integer(teNormal));
    cbVersaoDados.ItemIndex := Ini.ReadInteger('Geral', 'VersaoDados', 0);
    cbxRetirarAcentos.Checked := Ini.ReadBool('Geral', 'RetirarAcentos', True);
    cbxRetirarEspacos.Checked := Ini.ReadBool('Geral', 'RetirarEspacos', True);

    cbSSLLib.ItemIndex := Ini.ReadInteger('Geral', 'SSLLib', Integer(libWinCrypt));
    cbCryptLib.ItemIndex := Ini.ReadInteger('Geral', 'CryptLib', Integer(cryWinCrypt));
    cbHttpLib.ItemIndex := Ini.ReadInteger('Geral', 'HttpLib', Integer(httpWinHttp));
    cbXMLSignLib.ItemIndex := Ini.ReadInteger('Geral', 'XMLSignLib', Integer(xsLibXml2));

    rgTipoAmbiente.ItemIndex := Ini.ReadInteger('WebService', 'Ambiente', 0);
    cbxVisualizar.Checked := Ini.ReadBool('WebService', 'VisualizarMensagem', False);
    cbxSalvarSOAP.Checked := Ini.ReadBool('WebService', 'SalvarSOAP', False);
    seTimeOut.Value := Ini.ReadInteger('WebService', 'TimeOut', 5000);
    cbSSLType.ItemIndex := Ini.ReadInteger('WebService', 'SSLType', Integer(LT_TLSv1_2));

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TForm1.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrSATWS1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrSATWS1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrSATWS1.Configuracoes.Geral.SSLHttpLib);
  cbXMLSignLib.ItemIndex := Integer(ACBrSATWS1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrSATWS1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TForm1.ConfigurarComponente;
begin
  ACBrSATWS1.Configuracoes.Geral.ExibirErroSchema := cbxExibirErrosSchema.Checked;
  ACBrSATWS1.Configuracoes.Geral.FormatoAlerta := edtFormatoAlerta.Text;
  ACBrSATWS1.Configuracoes.Geral.FormaEmissao := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);
  ACBrSATWS1.Configuracoes.Geral.RetirarAcentos := cbxRetirarAcentos.Checked;
  ACBrSATWS1.Configuracoes.Geral.RetirarEspacos := cbxRetirarEspacos.Checked;
  ACBrSATWS1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  ACBrSATWS1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  ACBrSATWS1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  ACBrSATWS1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXMLSignLib.ItemIndex);

  ACBrSATWS1.WebServices.ConsultarSATWS.versaoDados := cbVersaoDados.Text;

  ACBrSATWS1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(rgTipoAmbiente.ItemIndex);
  ACBrSATWS1.Configuracoes.WebServices.Visualizar := cbxVisualizar.Checked;
  ACBrSATWS1.Configuracoes.WebServices.Salvar := cbxSalvarSOAP.Checked;
  ACBrSATWS1.Configuracoes.WebServices.TimeOut := seTimeOut.Value;
  ACBrSATWS1.Configuracoes.WebServices.SSLType := TSSLType(cbSSLType.ItemIndex);
  ACBrSATWS1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
  ACBrSATWS1.Configuracoes.WebServices.ProxyPort := edtProxyPorta.Text;
  ACBrSATWS1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
  ACBrSATWS1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;
  ACBrSATWS1.Configuracoes.WebServices.UF := 'SP';

end;

end.
