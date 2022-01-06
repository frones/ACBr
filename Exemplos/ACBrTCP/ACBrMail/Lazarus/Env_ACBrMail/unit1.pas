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

{$mode objfpc}{$H+}

interface

uses
  Classes, SynMemo, Forms, Controls, Graphics, ExtCtrls, Menus, SynEditTypes,
  ComCtrls, StdCtrls, DBGrids, Buttons, Grids, Spin, ActnList, ACBrMail, sysutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrMail1: TACBrMail;
    ApplicationProperties1: TApplicationProperties;
    btAbrirHtml: TToolButton;
    btAbrirTexto: TToolButton;
    btAvancarHtml: TToolButton;
    btCentroHtml: TToolButton;
    btColarHtml: TToolButton;
    btColarTexto: TToolButton;
    btCopiarHtml: TToolButton;
    btCopiarTexto: TToolButton;
    btDesfazerHtml: TToolButton;
    btDesfazerTexto: TToolButton;
    btDireitaHtml: TToolButton;
    btEnviar: TBitBtn;
    btEsquerdaHtml: TToolButton;
    btExcluirTexto: TToolButton;
    btHtml: TToolButton;
    btItalicoHtml: TToolButton;
    btItemListaHtml: TToolButton;
    btJustificadoHtml: TToolButton;
    btListaDesordHtml: TToolButton;
    btListaOrdHtml: TToolButton;
    btNegritoHtml: TToolButton;
    btOutrasTags: TToolButton;
    btQuebraHtml: TToolButton;
    btRecortarHtml: TToolButton;
    btRecortarTexto: TToolButton;
    btRecuarHtml: TToolButton;
    btRefazerHtml: TToolButton;
    btSalvarHtml: TToolButton;
    btSalvarTexto: TToolButton;
    btSelTodoTexto: TToolButton;
    btSublinhadoHtml: TToolButton;
    btTabelaHtml: TToolButton;
    btTitulosHtml: TToolButton;
    btVisualizar: TToolButton;
    CheckBox1: TCheckBox;
    cbUsarThread: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Edit1: TEdit;
    Image1: TImage;
    ImgPonte: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    mDesfazerTexto: TMenuItem;
    MenuItem17: TMenuItem;
    mRecortarTexto: TMenuItem;
    MenuItem27: TMenuItem;
    mCopiarTexto: TMenuItem;
    mColarTexto: TMenuItem;
    mExcluirTexto: TMenuItem;
    MenuItem37: TMenuItem;
    mSelTudoTexto: TMenuItem;
    mDesfazerHtml: TMenuItem;
    MenuItem40: TMenuItem;
    mRecortarHtml: TMenuItem;
    mCopiarHtml: TMenuItem;
    mColarHtml: TMenuItem;
    mExcluirHtml: TMenuItem;
    MenuItem45: TMenuItem;
    mSelTudoHtml: TMenuItem;
    mRefazerHtml: TMenuItem;
    MenuItem48: TMenuItem;
    mNivel1: TMenuItem;
    mNivel2: TMenuItem;
    mNivel3: TMenuItem;
    mNivel4: TMenuItem;
    mNivel5: TMenuItem;
    mNivel6: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    PopupAnexos: TPopupMenu;
    PopupMemoTexto: TPopupMenu;
    PopupSynMemo: TPopupMenu;
    PopupTabela: TPopupMenu;
    PopupOutros: TPopupMenu;
    PopupNiveis: TPopupMenu;
    ProgressBar1: TProgressBar;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    btIncluir: TSpeedButton;
    btCancelar: TSpeedButton;
    btExcluir: TSpeedButton;
    Shape5: TShape;
    SpinEdit2: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynMemo1: TSynMemo;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton34: TToolButton;
    ToolButton35: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton4: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure btQuebraHtmlClick(Sender: TObject);
    procedure btColarTextoClick(Sender: TObject);
    procedure btCopiarTextoClick(Sender: TObject);
    procedure btDesfazerTextoClick(Sender: TObject);
    procedure btEnviarClick(Sender: TObject);
    procedure btAbrirTextoClick(Sender: TObject);
    procedure btAbrirHtmlClick(Sender: TObject);
    procedure btAvancarHtmlClick(Sender: TObject);
    procedure btCentroHtmlClick(Sender: TObject);
    procedure btColarHtmlClick(Sender: TObject);
    procedure btCopiarHtmlClick(Sender: TObject);
    procedure btDireitaHtmlClick(Sender: TObject);
    procedure btEsquerdaHtmlClick(Sender: TObject);
    procedure btHtmlClick(Sender: TObject);
    procedure btItalicoHtmlClick(Sender: TObject);
    procedure btItemListaHtmlClick(Sender: TObject);
    procedure btJustificadoHtmlClick(Sender: TObject);
    procedure btListaDesordHtmlClick(Sender: TObject);
    procedure btListaOrdHtmlClick(Sender: TObject);
    procedure btRecortarHtmlClick(Sender: TObject);
    procedure btRecortarTextoClick(Sender: TObject);
    procedure btRecuarHtmlClick(Sender: TObject);
    procedure btSalvarTextoClick(Sender: TObject);
    procedure btSalvarHtmlClick(Sender: TObject);
    procedure btSublinhadoHtmlClick(Sender: TObject);
    procedure btTabelaHtmlClick(Sender: TObject);
    procedure btTitulosHtmlClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1SelectEditor(Sender: TObject; Column: TColumn;
      var Editor: TWinControl);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid2GetCellHint(Sender: TObject; Column: TColumn;
      var AText: String);
    procedure DBGrid2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure DBGrid2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DBGrid2TitleClick(Column: TColumn);
    procedure dmdsDestinosStateChange(Sender: TObject);
    procedure Enviar_ACBrMailMailProcess(const AMail: TACBrMail; const aStatus: TMailStatus);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1Enter(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure MenuItem32Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem48Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure mExcluirHtmlClick(Sender: TObject);
    procedure mNivel1Click(Sender: TObject);
    procedure mNivel2Click(Sender: TObject);
    procedure mNivel3Click(Sender: TObject);
    procedure mNivel4Click(Sender: TObject);
    procedure mNivel5Click(Sender: TObject);
    procedure mNivel6Click(Sender: TObject);
    procedure btIncluirClick(Sender: TObject);
    procedure btCancelarClick(Sender: TObject);
    procedure btExcluirClick(Sender: TObject);
    procedure btNegritoHtmlClick(Sender: TObject);
    procedure btOutrasTagsClick(Sender: TObject);
    procedure mSelTudoHtmlClick(Sender: TObject);
    procedure SynMemo1Change(Sender: TObject);
    procedure SynMemo1Enter(Sender: TObject);
    procedure SynMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SynMemo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynMemo1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure btVisualizarClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure btRefazerHtmlClick(Sender: TObject);
    procedure btDesfazerHtmlClick(Sender: TObject);
    procedure btExcluirTextoClick(Sender: TObject);
    procedure btSelTodoTextoClick(Sender: TObject);
  private
    const
      fWord = '.doc;.docx;';
      fExcel = '.xls;.xlsx;';
      fPowerpoint = '.ppt;.pps;.pptx;.ppsx;';
      fTexto = '.txt;.log;.ini;';
      fDocumento = '.odt;.fodt;.rtf;';
      fDesenho = '.cds;.svg;.odg;.fodg;';
      fPlanilha = '.ods;.fods;';
      fApresentacao = '.odp;.fodp;';
      fCompacto = '.rar;.zip;.tar;.tar.gz;.7z;.deb;.rpm;.iso;';
      fVideo = '.avi;.mp4;.3gp;.flv;.swf;.wmv;';
      fAudio = '.wav;.mp3;.wma;';
      fImagem = '.jpg;.png;.gif;';
      fBancos = '.db;.fdb;.gdb;.mdb;';
      fWeb = '.xhtml;.html;.htm;.mht;';
      fExec = '.exe;.bin;.msi;';
      fPdf = '.pdf;';

    var
      vTotalFileSize: Int64;
  public
    function CorHTML(aCor: TColor): string;
    function EstiloFonteCSS: string;
    procedure CarregaTabCaracteres;
    procedure AplicaTags(aInicial: string; aFinal: string);
    procedure AplicaTagsIndentadas(aInicial: string; aFinal: string);
    procedure ReposicionaCursor;
    procedure NovaMensagem;
    procedure MemoStatus;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uContasList, uContatosList, mimemess, db, typinfo, LCLType, LCLIntf,
  RegExpr, uDados, FileUtil, Dialogs;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  dm.dsDestinos.OnStateChange := @dmdsDestinosStateChange;
  DBGrid2.FocusRectVisible := False;
  dm.tbDestinos.CreateDataset;
  dm.tbDestinos.Open;
  dm.tbAnexos.CreateDataset;
  dm.tbAnexos.Open;
  if not(FileExists('contas.dat')) then
  begin
    dm.tbContas.CreateDataset;
    dm.tbContas.SaveToFile('contas.dat');
  end;
  if not(FileExists('contatos.dat')) then
  begin
    dm.tbContatos.CreateDataset;
    dm.tbContatos.SaveToFile('contatos.dat');
  end;
  dm.tbContas.LoadFromFile('contas.dat');
  dm.tbContatos.LoadFromFile('contatos.dat');
  CarregaTabCaracteres;
  dm.tbContas.FilterOptions := [foCaseInsensitive];
  dm.tbContatos.FilterOptions := [foCaseInsensitive];
  NovaMensagem;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SynMemo1.Highlighter := nil; // necessário - não remover
end;

procedure TForm1.FormResize(Sender: TObject);
var
  g, l: integer;
begin
  g := DBGrid1.Width - DBGrid1.Columns[0].Width;
  l := trunc(g/2) - 11;
  DBGrid1.Columns[1].Width := l;
  DBGrid1.Columns[2].Width := l;
  DBGrid2.Columns[0].Width := DBGrid2.Width - DBGrid2.Columns[1].Width - 18;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  btDesfazerTexto.Enabled := Memo1.CanUndo;
end;

procedure TForm1.Memo1Enter(Sender: TObject);
begin
  MemoStatus;
end;

procedure TForm1.Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  MemoStatus;
end;

procedure TForm1.Memo1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MemoStatus;
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  Application.MessageBox(PChar(MenuItem10.Hint),
    'Sobre Enviador de E-mails ACBrMail ...',MB_ICONASTERISK);
end;

procedure TForm1.MenuItem13Click(Sender: TObject);
begin
  SynMemo1.SelText := EstiloFonteCSS;
end;

procedure TForm1.MenuItem15Click(Sender: TObject);
var
  vValue: String;
begin
  if dm.tbAnexos.IsEmpty then Exit;
  vValue := dm.tbAnexos.Fields[2].AsString;
  if InputQuery('Renomear Anexo', 'Nome:', vValue) then
  begin
    dm.tbAnexos.Edit;
    dm.tbAnexos.Fields[2].AsString := vValue;
    dm.tbAnexos.Post;
  end;
end;

procedure TForm1.MenuItem16Click(Sender: TObject);
begin
  if dm.tbAnexos.IsEmpty then Exit;
  vTotalFileSize -= dm.tbAnexos.Fields[3].AsInteger;
  dm.tbAnexos.Delete;
  DBGrid2.Columns[0].Title.Caption := IntToStr(dm.tbAnexos.RecordCount) + ' anexo(s)';
  if vTotalFileSize >= 1048576 then
    DBGrid2.Columns[1].Title.Caption := FormatFloat('0.00',vTotalFileSize / 1048576) + ' MB'
  else
    DBGrid2.Columns[1].Title.Caption := FormatFloat('0.00',vTotalFileSize / 1024) + ' KB';
end;

procedure TForm1.MenuItem17Click(Sender: TObject);
begin
  Application.MessageBox(PChar(MenuItem17.Hint), PChar(MenuItem17.Caption),
    MB_ICONINFORMATION);
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
  AplicaTags('<a href="','"></a>');
end;

procedure TForm1.MenuItem20Click(Sender: TObject);
begin
  AplicaTags('<a id="','"></a>');
end;

procedure TForm1.MenuItem21Click(Sender: TObject);
begin
  AplicaTags('<p>','</p>');
end;

procedure TForm1.MenuItem22Click(Sender: TObject);
begin
  AplicaTags('<!-- ',' -->');
end;

procedure TForm1.MenuItem23Click(Sender: TObject);
begin
  AplicaTags('<img src="cid:','" alt="" height="200" width="300">');
end;

procedure TForm1.MenuItem24Click(Sender: TObject);
begin
  AplicaTagsIndentadas('<div>','</div>');
end;

procedure TForm1.MenuItem25Click(Sender: TObject);
begin
  if dm.ColorDialog1.Execute then SynMemo1.SelText := CorHTML(dm.ColorDialog1.Color);
end;

procedure TForm1.MenuItem26Click(Sender: TObject);
begin
  SynMemo1.SelText := '<hr>';
end;

procedure TForm1.MenuItem28Click(Sender: TObject);
begin
  SynMemo1.SelText := '&nbsp;';
end;

procedure TForm1.MenuItem29Click(Sender: TObject);
begin
  AplicaTagsIndentadas('<table border="1" cellpadding="1" cellspacing="1">','</table>');
  AplicaTagsIndentadas('<tbody>','</tbody>');
end;

procedure TForm1.MenuItem30Click(Sender: TObject);
begin
  AplicaTagsIndentadas('<tr>','</tr>');
end;

procedure TForm1.MenuItem31Click(Sender: TObject);
begin
  AplicaTags('<td>','</td>');
end;

procedure TForm1.MenuItem32Click(Sender: TObject);
begin
  AplicaTags('<sub>','</sub>');
end;

procedure TForm1.MenuItem33Click(Sender: TObject);
begin
  AplicaTags('<sup>','</sup>');
end;

procedure TForm1.MenuItem48Click(Sender: TObject);
begin
  Application.MessageBox(PChar(MenuItem48.Hint), PChar(MenuItem48.Caption),
    MB_ICONINFORMATION);
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.mExcluirHtmlClick(Sender: TObject);
begin
  SynMemo1.Lines.Clear;
  SynMemo1Change(nil);
end;

procedure TForm1.mNivel1Click(Sender: TObject);
begin
  AplicaTags('<h1>','</h1>');
end;

procedure TForm1.mNivel2Click(Sender: TObject);
begin
  AplicaTags('<h2>','</h2>');
end;

procedure TForm1.mNivel3Click(Sender: TObject);
begin
  AplicaTags('<h3>','</h3>');
end;

procedure TForm1.mNivel4Click(Sender: TObject);
begin
  AplicaTags('<h4>','</h4>');
end;

procedure TForm1.mNivel5Click(Sender: TObject);
begin
  AplicaTags('<h5>','</h5>');
end;

procedure TForm1.mNivel6Click(Sender: TObject);
begin
  AplicaTags('<h6>','</h6>');
end;

procedure TForm1.btIncluirClick(Sender: TObject);
begin
  with TfrmContatosList.Create(nil) do
  try
    Tag := 1;
    if (ShowModal <> mrOK) and not(dm.tbDestinos.State in [dsInsert,dsEdit]) then
    try
      dm.tbDestinos.Append;
    except
      // usado para contornar comportamento do evento OnEditButtonClick do
      // TDBGrid quando este método é chamado por ele
      dm.tbDestinos.Append;
    end;
  finally
    Release;
    Free;
    dm.tbContatos.Filter := '';
  end;
  DBGrid1.SelectedIndex := 1;
  DBGrid1.EditorMode := True;
  DBGrid1.SetFocus;
end;

procedure TForm1.btCancelarClick(Sender: TObject);
begin
  dm.tbDestinos.Cancel;
end;

procedure TForm1.btExcluirClick(Sender: TObject);
begin
  dm.tbDestinos.Delete;
  dmdsDestinosStateChange(nil);
end;

procedure TForm1.btNegritoHtmlClick(Sender: TObject);
begin
  AplicaTags('<b>','</b>');
end;

procedure TForm1.btOutrasTagsClick(Sender: TObject);
begin
  btOutrasTags.CheckMenuDropdown;
end;

procedure TForm1.mSelTudoHtmlClick(Sender: TObject);
begin
  SynMemo1.SelectAll;
end;

procedure TForm1.SynMemo1Change(Sender: TObject);
begin
  btRefazerHtml.Enabled := SynMemo1.CanRedo;
  btDesfazerHtml.Enabled := SynMemo1.CanUndo;
  mRefazerHtml.Enabled := SynMemo1.CanRedo;
  mDesfazerHtml.Enabled := SynMemo1.CanUndo;
end;

procedure TForm1.SynMemo1Enter(Sender: TObject);
begin
  ReposicionaCursor;
end;

procedure TForm1.SynMemo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_S) then btSalvarHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_O) then btAbrirHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_B) then btNegritoHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_I) then btItalicoHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_U) then btSublinhadoHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_1) then mNivel1.Click;
  if (Shift = [ssCtrl]) and (Key = VK_2) then mNivel2.Click;
  if (Shift = [ssCtrl]) and (Key = VK_3) then mNivel3.Click;
  if (Shift = [ssCtrl]) and (Key = VK_4) then mNivel4.Click;
  if (Shift = [ssCtrl]) and (Key = VK_5) then mNivel5.Click;
  if (Shift = [ssCtrl]) and (Key = VK_6) then mNivel6.Click;
  if (Shift = [ssCtrl]) and (Key = VK_G) then btCentroHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_J) then btJustificadoHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_D) then btDireitaHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_E) then btEsquerdaHtml.Click;

  // tabela,linha,coluna
  if (Shift = [ssCtrl,ssShift]) and (Key = VK_T) then MenuItem29.Click;
  if (Shift = [ssCtrl]) and (Key = VK_L) then MenuItem30.Click;
  if (Shift = [ssCtrl]) and (Key = VK_K) then MenuItem31.Click;

  if (Shift = [ssCtrl]) and (Key = VK_Q) then btQuebraHtml.Click;

  if (Shift = [ssCtrl,ssShift]) and (Key = VK_V) then btVisualizar.Click;

  // cor
  if (Shift = [ssCtrl,ssShift]) and (Key = VK_C) then MenuItem25.Click;

  // tag de estilo css
  if (Shift = [ssCtrl,ssShift]) and (Key = VK_S) then ToolButton11.Click;

  // fonte css
  if (Shift = [ssCtrl,ssShift]) and (Key = VK_F) then MenuItem13.Click;

  if (Shift = [ssCtrl,ssShift]) and (Key = VK_B) then btListaDesordHtml.Click;
  if (Shift = [ssCtrl,ssShift]) and (Key = VK_N) then btListaOrdHtml.Click;
  if (Shift = [ssCtrl]) and (Key = VK_W) then btItemListaHtml.Click;

  // ajuda atalhos
  if (Shift = [ssCtrl]) and (Key = VK_H) then MenuItem48.Click;

end;

procedure TForm1.SynMemo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReposicionaCursor;
end;

procedure TForm1.SynMemo1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  btColarHtml.Enabled := SynMemo1.CanPaste;
  mColarHtml.Enabled := SynMemo1.CanPaste;
  if SynMemo1.SelText <> '' then
  begin
    btCopiarHtml.Enabled := True;
    btRecortarHtml.Enabled := True;
    mCopiarHtml.Enabled := True;
    mRecortarHtml.Enabled := True;
  end
  else
  begin
    btCopiarHtml.Enabled := False;
    btRecortarHtml.Enabled := False;
    mCopiarHtml.Enabled := False;
    mRecortarHtml.Enabled :=False;
  end;
end;

procedure TForm1.ToolButton11Click(Sender: TObject);
begin
  AplicaTags(' style="','"');
end;

procedure TForm1.ToolButton12Click(Sender: TObject);
begin
  SynMemo1.SelText := WrapText(SynMemo1.SelText,LineEnding,[',',' '],70);
end;

procedure TForm1.btVisualizarClick(Sender: TObject);
var
  sTemp: TStringList;
begin
  try
    // procedimento para não perder as marcas de alteração do SynMemo
    sTemp := TStringList.Create;
    sTemp.Assign(SynMemo1.Lines);
    sTemp.SaveToFile('temp.html');
    OpenDocument('temp.html');
  finally
    sTemp.Free;
  end;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  if Application.MessageBox('Iniciar Nova Mensagem?','Confirmação',6) <> 6 then Exit;
  NovaMensagem;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  try
    frmContatosList := TfrmContatosList.Create(nil);
    frmContatosList.ShowModal;
  finally
    frmContatosList.Release;
    FreeAndNil(frmContatosList);
    dm.tbContatos.Filter := '';
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
var
  vFileSize: Int64;
begin
  with dm.OpenDialog1 do
    if Execute then
    begin
      if dm.tbAnexos.Locate('arquivo',FileName,[]) then Exit;
      dm.tbAnexos.Append;
      dm.tbAnexos.Fields[1].AsString := FileName;
      dm.tbAnexos.Fields[2].AsString := ExtractFileName(FileName);
      vFileSize := FileSize(FileName);
      dm.tbAnexos.Fields[3].AsInteger := vFileSize;
      if vFileSize >= 1048576 then
        dm.tbAnexos.Fields[5].AsString := FormatFloat('0.00',vFileSize / 1048576) + ' MB'
      else
        dm.tbAnexos.Fields[5].AsString := FormatFloat('0.00',vFileSize / 1024) + ' KB';
      dm.tbAnexos.Fields[4].AsString := ExtractFileExt(FileName) + ';';
      dm.tbAnexos.Post;
      vTotalFileSize += vFileSize;
      DBGrid2.Columns[0].Title.Caption := IntToStr(dm.tbAnexos.RecordCount) + ' anexo(s)';
      if vTotalFileSize >= 1048576 then
        DBGrid2.Columns[1].Title.Caption := FormatFloat('0.00',vTotalFileSize / 1048576) + ' MB'
      else
        DBGrid2.Columns[1].Title.Caption := FormatFloat('0.00',vTotalFileSize / 1024) + ' KB';
    end;
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  try
    frmContasList := TfrmContasList.Create(nil);
    frmContasList.ShowModal;
  finally
    frmContasList.Release;
    FreeAndNil(frmContasList);
    dm.tbContas.Filter := '';
  end;
  ComboBox4.Clear;
  if not(dm.tbContas.IsEmpty) then
  begin
    ComboBox4.Items.CommaText := dm.CarregaContas;
    ComboBox4.ItemIndex := 0;
  end;
end;

procedure TForm1.btRefazerHtmlClick(Sender: TObject);
begin
  SynMemo1.Redo;
end;

procedure TForm1.btDesfazerHtmlClick(Sender: TObject);
begin
  SynMemo1.Undo;
end;

procedure TForm1.btExcluirTextoClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  MemoStatus;
end;

procedure TForm1.btSelTodoTextoClick(Sender: TObject);
begin
  Memo1.SelectAll;
  MemoStatus;
end;

procedure TForm1.CarregaTabCaracteres;
var
   tmc : TMailCharset;
   tms : string;
 begin
   ComboBox1.Clear;
   for tmc in [Low(TMailCharset) .. High(TMailCharset)] do
   begin
     tms := GetEnumName(TypeInfo(TMailCharset), integer(tmc));
     ComboBox1.Items.Add(tms);
   end;
 end;

function TForm1.CorHTML(aCor: TColor): string;
var
  tmpRGB : TColorRef;
begin
  tmpRGB := ColorToRGB(aCor);
  Result := Format('#%.2x%.2x%.2x', [GetRValue(tmpRGB), GetGValue(tmpRGB),
                   GetBValue(tmpRGB)]);
end;

function TForm1.EstiloFonteCSS: string;
var
  vStyle: string;
begin
  Result := '';
  vStyle := ' text-decoration:';
  with dm.FontDialog1 do
    if Execute then
    begin
      Result := ' font:';
      if fsItalic in Font.Style then Result += ' italic';
      if fsBold in Font.Style then Result += ' bold';
      Result += ' '+IntToStr(Font.Size)+'px';
      Result += ' '''+Font.Name+''';';
      if fsUnderline in Font.Style then vStyle += ' underline';
      if fsStrikeOut in Font.Style then vStyle += ' line-through';
      if Length(vStyle) > 17 then Result += vStyle + ';';
      Result += ' color: '+CorHTML(Font.Color)+';';
    end;
end;

procedure TForm1.AplicaTags(aInicial: string; aFinal: string);
var
  vTamanho: Integer;
begin
  with SynMemo1 do
  begin
    vTamanho := Length(SelText);
    SelText :=  aInicial + SelText + aFinal;
    SelStart := SelStart - Length(aFinal) - vTamanho;
    SelEnd := SelStart + vTamanho;
    SetFocus;
  end;
end;

procedure TForm1.AplicaTagsIndentadas(aInicial: string; aFinal: string);
var
  rx: TRegExpr;
  i, z: integer;
begin
  try
    rx := TRegExpr.Create;
    rx.Expression := '^(\s*)?(.*)$';
    rx.Exec(SynMemo1.LineText);
    AplicaTags(aInicial + LineEnding, LineEnding + rx.Match[1] + aFinal);
    z := trunc(rx.MatchLen[1] / 2);
    for i := 0 to z do
      btAvancarHtml.Click;
  finally
    rx.Free;
  end;
end;

procedure TForm1.ReposicionaCursor;
var
  sLen, sLin: Integer;
begin
  if SynMemo1.SelText = '' then
  begin
    sLen := Length(SynMemo1.SelText);
    sLin := Length(SynMemo1.LineText);
    SynMemo1.SelStart := (SynMemo1.SelStart - sLin - sLen) + sLin;
  end;
end;

procedure TForm1.NovaMensagem;
begin
  dm.tbDestinos.Close;
  dm.tbAnexos.Close;
  dm.tbDestinos.Open;
  dm.tbAnexos.Open;
  SynMemo1.ClearAll;
  SynMemo1.ClearUndo;
  SynMemo1.Modified := False;
  SynMemo1.MarkTextAsSaved;
  btDesfazerHtml.Enabled := False;
  btRefazerHtml.Enabled := False;
  mDesfazerHtml.Enabled := False;
  mRefazerHtml.Enabled := False;
  Memo1.Clear;
  Memo2.Clear;
  Memo1.Modified := False;
  Memo2.Modified := False;
  btDesfazerTexto.Enabled := False;
  mDesfazerTexto.Enabled := False;
  Edit1.Text := '';
  DBGrid2.Columns[0].Title.Caption := '0 anexos';
  DBGrid2.Columns[1].Title.Caption := '0,0 KB';
  SpinEdit2.Value := 1;
  ComboBox1.Text := 'ISO_8859_1';
  ComboBox2.ItemIndex := 2;
  ComboBox3.ItemIndex := 2;
  ComboBox2Change(nil);
  CheckBox1.Checked := False;
  vTotalFileSize := 0;
  dm.tbDestinos.Append;
  if Showing then
  begin
    DBGrid1.SetFocus;
    DBGrid1.SelectedIndex := 1;
    DBGrid1.EditorMode := True;
  end;
end;

procedure TForm1.MemoStatus;
begin
  btDesfazerTexto.Enabled := Memo1.CanUndo;
  mDesfazerTexto.Enabled := Memo1.CanUndo;
  btColarTexto.Enabled := SynMemo1.CanPaste;
  mColarTexto.Enabled := SynMemo1.CanPaste;
  if Memo1.SelText <> '' then
  begin
    btCopiarTexto.Enabled := True;
    btRecortarTexto.Enabled := True;
    mCopiarTexto.Enabled := True;
    mRecortarTexto.Enabled := True;
  end
  else
  begin
    btCopiarTexto.Enabled := False;
    btRecortarTexto.Enabled := False;
    mCopiarTexto.Enabled := False;
    mRecortarTexto.Enabled := False;
  end;
end;

procedure TForm1.DBGrid1SelectEditor(Sender: TObject; Column: TColumn;
  var Editor: TWinControl);
begin
  if Column.FieldName = 'lista' then
    TCustomComboBox(Editor).Style := csDropDownList;
end;

procedure TForm1.DBGrid1TitleClick(Column: TColumn);
begin
  if not(DBGrid1.EditorMode) then
    dm.Ordenacao(dm.tbDestinos, Column.FieldName);
end;

procedure TForm1.DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  vRect: TRect;
  ImgId: Integer;
  vExt: String;
begin
  if gdSelected in State then
  begin
    DBGrid2.Canvas.Brush.Color := $00FFF9F2;
    DBGrid2.Canvas.Font.Color := DBGrid2.Font.Color;
    DBGrid2.Canvas.Font.Style := [fsBold];
    DBGrid2.Canvas.FillRect(Rect);
    DBGrid2.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
  if (Column.Index = 0) and not(dm.tbAnexos.IsEmpty) then begin
    vRect := Rect;
    vRect.Top := vRect.Top + 3;
    vRect.Left := vRect.Left + 5;
    vRect.Right := Rect.Left + 21;
    vRect.Bottom := Rect.Top + 19;
    DBGrid2.Canvas.FillRect(Rect);
    ImgId := 0;
    vExt := LowerCase(dm.tbAnexos.Fields[4].AsString);
    if pos(vExt,fWord) > 0 then ImgId := 7 else
    if pos(vExt,fExcel) > 0 then ImgId := 5 else
    if pos(vExt,fPowerpoint) > 0 then ImgId := 6 else
    if pos(vExt,fTexto) > 0 then ImgId := 11 else
    if pos(vExt,fDocumento) > 0 then ImgId := 12 else
    if pos(vExt,fDesenho) > 0 then ImgId := 14 else
    if pos(vExt,fPlanilha) > 0 then ImgId := 10 else
    if pos(vExt,fApresentacao) > 0 then ImgId := 15 else
    if pos(vExt,fCompacto) > 0 then ImgId := 0 else
    if pos(vExt,fVideo) > 0 then ImgId := 3 else
    if pos(vExt,fAudio) > 0 then ImgId := 1 else
    if pos(vExt,fImagem) > 0 then ImgId := 8 else
    if pos(vExt,fBancos) > 0 then ImgId := 2 else
    if pos(vExt,fWeb) > 0 then ImgId := 13 else
    if pos(vExt,fExec) > 0 then ImgId := 16 else
    if pos(vExt,fPdf) > 0 then ImgId := 9 else
      ImgId := 4;
    dm.ImagesFiles.GetBitmap(ImgId,ImgPonte.Picture.Bitmap);
    DBGrid2.Canvas.StretchDraw(vRect, ImgPonte.Picture.Graphic);
    vRect := Rect;
    vRect.Left := vRect.Left + 21;
    DBGrid2.DefaultDrawColumnCell(vRect, DataCol, Column, State);
  end;
end;

procedure TForm1.DBGrid2GetCellHint(Sender: TObject; Column: TColumn;
  var AText: String);
begin
  if not(dm.tbAnexos.IsEmpty) then
    AText := dm.tbAnexos.Fields[1].AsString;
end;

procedure TForm1.DBGrid2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    Key := 0;
    MenuItem16.Click;
  end;
end;

procedure TForm1.DBGrid2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  gc: TGridCoord;
begin
  with TDBGrid(Sender) do
  begin
    Options := Options + [dgIndicator];
    if not(dgMultiselect in Options) and (Button = mbRight) then
    begin
      gc:= MouseCoord(X, Y);
      if (gc.X > 0) AND (gc.Y > 0) then begin
        DataSource.DataSet.MoveBy(gc.Y - TStringGrid(Sender).Row);
        SetFocus;
      end;
    end;
    Options := Options - [dgIndicator];
  end;
end;

procedure TForm1.DBGrid2TitleClick(Column: TColumn);
begin
  dm.Ordenacao(dm.tbAnexos,Column.FieldName);
end;

procedure TForm1.dmdsDestinosStateChange(Sender: TObject);
begin
  if (dm.tbDestinos.State in [dsInsert,dsEdit]) then
  begin
    // btEnviar.Enabled := False;
    btIncluir.Enabled := False;
    btExcluir.Enabled := False;
    btCancelar.Enabled := True;
  end
  else
  begin
    // btEnviar.Enabled := True;
    btIncluir.Enabled := True;
    if dm.tbDestinos.IsEmpty then
      btExcluir.Enabled := False
    else
      btExcluir.Enabled := True;
    btCancelar.Enabled := False;
  end;
end;

procedure TForm1.Enviar_ACBrMailMailProcess(const AMail: TACBrMail; const aStatus: TMailStatus);
begin
  ProgressBar1.Position := Integer(aStatus);
  case aStatus of
    pmsStartProcess:
    begin
      Panel1.Caption := 'Status do envio: Iniciando processo de envio.';
    end;
    pmsConfigHeaders:
      Panel1.Caption := 'Status do envio: Configurando o cabeçalho do e-mail.';
    pmsLoginSMTP:
      Panel1.Caption := 'Status do envio: Logando no servidor de e-mail.';
    pmsStartSends:
      Panel1.Caption := 'Status do envio: Iniciando os envios.';
    pmsSendTo:
      Panel1.Caption := 'Status do envio: Processando lista de destinatários.';
    pmsSendCC:
      Panel1.Caption := 'Status do envio: Processando lista CC.';
    pmsSendBCC:
      Panel1.Caption := 'Status do envio: Processando lista BCC.';
    pmsSendReplyTo:
      Panel1.Caption := 'Status do envio: Processando lista ReplyTo.';
    pmsSendData:
      Panel1.Caption := 'Status do envio: Enviando dados.';
    pmsLogoutSMTP:
      Panel1.Caption := 'Status do envio: Fazendo Logout no servidor de e-mail.';
    pmsDone:
    begin
      Panel1.Caption := 'Status do envio: Terminando e limpando.';
      ProgressBar1.Position := ProgressBar1.Max;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  WindowState := wsMaximized;
  DBGrid1.SetFocus;
  DBGrid1.SelectedIndex := 1;
  DBGrid1.EditorMode := True;
  if dm.tbContas.IsEmpty then
  begin
    Application.MessageBox('Você precisa cadastrar uma conta para poder enviar e-mails!',
      'Informação', MB_ICONASTERISK);
    ToolButton4.Click;
  end
  else
  begin
    ComboBox4.Clear;
    ComboBox4.Items.CommaText := dm.CarregaContas;
    ComboBox4.ItemIndex := 0;
  end;
end;

procedure TForm1.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if gdSelected in State then
  begin
    DBGrid1.Canvas.Brush.Color := Column.Color;
    DBGrid1.Canvas.Font.Color := Column.Font.Color;
    DBGrid1.Canvas.FillRect(Rect);
    DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TForm1.btEnviarClick(Sender: TObject);
begin
  if dm.tbDestinos.State in [dsInsert,dsEdit] then
  begin
    if (dm.ValidaEmail(dm.tbDestinos.Fields[2].AsString)) then
      dm.tbDestinos.Post
    else
      dm.tbDestinos.Cancel;
  end;

  Screen.Cursor := crHourGlass;
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := True;
  Panel1.Caption := 'Status do envio:';
  Application.ProcessMessages;
  try
    with ACBrMail1 do
    begin
      OnMailProcess := @Enviar_ACBrMailMailProcess;
      ReadingConfirmation := CheckBox1.Checked;
      DefaultCharset := TMailCharset(ComboBox1.ItemIndex);
      IsHTML := (ComboBox2.ItemIndex <> 0);
      Priority := TMessPriority(ComboBox3.ItemIndex);
      Attempts := SpinEdit2.Value;
      if (IsHTML) then
        Body.Text := SynMemo1.Text
      else
        Body.Text := Memo1.Text;

      if ComboBox2.ItemIndex = 2 then
         AltBody.Text := Memo2.Text;

      Subject := Edit1.Text;
      dm.tbContas.Locate('NomeEmail',ComboBox4.Text,[]);
      From := dm.tbContas.Fields[3].AsString;
      FromName := dm.tbContas.Fields[2].AsString;
      Host := dm.tbContas.Fields[6].AsString;
      Username := dm.tbContas.Fields[4].AsString;
      Password := dm.tbContas.Fields[5].AsString;
      Port := dm.tbContas.Fields[7].AsString;
      SetSSL := dm.tbContas.Fields[8].AsBoolean;
      SetTLS := dm.tbContas.Fields[9].AsBoolean;
      dm.tbDestinos.DisableControls;
      dm.tbAnexos.DisableControls;

      dm.tbDestinos.First;
      while not(dm.tbDestinos.EOF) do
      begin
        case dm.tbDestinos.Fields[1].AsString of
          'Para:': AddAddress(dm.tbDestinos.Fields[2].AsString,dm.tbDestinos.Fields[3].AsString);
          'CC:': AddCC(dm.tbDestinos.Fields[2].AsString,dm.tbDestinos.Fields[3].AsString);
          'CCO:': AddBCC(dm.tbDestinos.Fields[2].AsString);
          'Responder a:': AddReplyTo(dm.tbDestinos.Fields[2].AsString,dm.tbDestinos.Fields[3].AsString);
        end;
        dm.tbDestinos.Next;
      end;

      dm.tbAnexos.First;
      while not(dm.tbAnexos.EOF) do
      begin
        AddAttachment(dm.tbAnexos.Fields[1].AsString,dm.tbAnexos.Fields[2].AsString);
        dm.tbAnexos.Next;
      end;

      Sleep(500);
      Send( cbUsarThread.Checked );
      Application.MessageBox('Mensagem enviada com sucesso!','Informação',MB_ICONASTERISK);
    end;
  finally
    Screen.Cursor := crDefault;
    dm.tbDestinos.EnableControls;
    dm.tbAnexos.EnableControls;
    Panel1.Caption := '';
    ProgressBar1.Visible := False;
  end;
  NovaMensagem;
end;

procedure TForm1.btAbrirTextoClick(Sender: TObject);
begin
  dm.OpenDialog1.Filter := 'Documento Texto (*.txt,*.tex)|*.txt;*.tex';
  if dm.OpenDialog1.Execute then Memo1.Lines.LoadFromFile(dm.OpenDialog1.FileName);
end;

procedure TForm1.btQuebraHtmlClick(Sender: TObject);
begin
  SynMemo1.SelText := '<br>';
end;

procedure TForm1.ApplicationProperties1Exception(Sender: TObject; E: Exception);
begin
  Screen.Cursor := crDefault;
  Application.MessageBox(PChar('Ocorreu um erro!' + LineEnding + LineEnding +
    'Veja a Mensagem do Sistema:' + LineEnding + E.Message),'Erro',
    MB_ICONERROR);
end;

procedure TForm1.btColarTextoClick(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
  MemoStatus;
end;

procedure TForm1.btCopiarTextoClick(Sender: TObject);
begin
  Memo1.CopyToClipboard;
  MemoStatus;
end;

procedure TForm1.btDesfazerTextoClick(Sender: TObject);
begin
  Memo1.Undo;
  MemoStatus;
end;

procedure TForm1.btAbrirHtmlClick(Sender: TObject);
begin
  dm.OpenDialog1.Filter := 'Documento HTML (*.htm,*.html)|*.htm;*.html';
  if dm.OpenDialog1.Execute then SynMemo1.Lines.LoadFromFile(dm.OpenDialog1.FileName);
end;

procedure TForm1.btAvancarHtmlClick(Sender: TObject);
begin
  if not SynMemo1.SelAvail then
  begin
    SynMemo1.LineText := '  ' + SynMemo1.LineText;
    SynMemo1.SelStart := SynMemo1.SelStart + 2;
  end
  else
    SynMemo1.CommandProcessor(612, '', nil);
end;

procedure TForm1.btCentroHtmlClick(Sender: TObject);
begin
  AplicaTagsIndentadas('<div style="text-align: center;">','</div>');
end;

procedure TForm1.btColarHtmlClick(Sender: TObject);
begin
  SynMemo1.PasteFromClipboard;
end;

procedure TForm1.btCopiarHtmlClick(Sender: TObject);
begin
  SynMemo1.CopyToClipboard;
end;

procedure TForm1.btDireitaHtmlClick(Sender: TObject);
begin
  AplicaTagsIndentadas('<div style="text-align: right;">','</div>');
end;

procedure TForm1.btEsquerdaHtmlClick(Sender: TObject);
begin
  AplicaTagsIndentadas('<div style="text-align: left;">','</div>');
end;

procedure TForm1.btHtmlClick(Sender: TObject);
begin
  AplicaTagsIndentadas(
    '<html>'+LineEnding+
    '  <head>'+LineEnding+
    '    <meta content="text/html; charset='+
    StringReplace(ComboBox1.Text,'_','-',[rfReplaceAll])+
    '" http-equiv="Content-Type">'+LineEnding+
    '  </head>'+LineEnding+
    '  <body text="#000000" bgcolor="#FFFFFF">',
    '  </body>'+LineEnding+
    '</html>'+LineEnding);
  btAvancarHtml.Click;
end;

procedure TForm1.btItalicoHtmlClick(Sender: TObject);
begin
  AplicaTags('<i>','</i>');
end;

procedure TForm1.btItemListaHtmlClick(Sender: TObject);
begin
  AplicaTags('<li>','</li>');
end;

procedure TForm1.btJustificadoHtmlClick(Sender: TObject);
begin
  AplicaTagsIndentadas('<div style="text-align: justify;">','</div>');
end;

procedure TForm1.btListaDesordHtmlClick(Sender: TObject);
begin
  AplicaTagsIndentadas('<ul>','</ul>');
end;

procedure TForm1.btListaOrdHtmlClick(Sender: TObject);
begin
  AplicaTagsIndentadas('<ol>','</ol>');
end;

procedure TForm1.btRecortarHtmlClick(Sender: TObject);
begin
  SynMemo1.CutToClipboard;
end;

procedure TForm1.btRecortarTextoClick(Sender: TObject);
begin
  Memo1.CutToClipboard;
  MemoStatus;
end;

procedure TForm1.btRecuarHtmlClick(Sender: TObject);
var
  rx: TRegExpr;
begin
  try
    rx := TRegExpr.Create;
    rx.Expression := '^(\s\s)?(.*)$';
    if not SynMemo1.SelAvail then
    begin
      rx.Exec(SynMemo1.LineText);
      if rx.MatchLen[1] > 0 then
      begin
        SynMemo1.LineText := rx.Match[2];
        SynMemo1.SelStart := SynMemo1.SelStart - 2;
      end;
    end
    else
      SynMemo1.CommandProcessor(613, '', nil);
  finally
    rx.Free;
  end;
end;

procedure TForm1.btSalvarTextoClick(Sender: TObject);
begin
  dm.SaveDialog1.Filter := 'Documento Texto (*.txt,*.tex)|*.txt;*.tex';
  if dm.SaveDialog1.Execute then Memo1.Lines.SaveToFile(dm.SaveDialog1.FileName);
end;

procedure TForm1.btSalvarHtmlClick(Sender: TObject);
begin
  dm.SaveDialog1.Filter := 'Documento HTML (*.htm,*.html)|*.htm;*.html';
  if dm.SaveDialog1.Execute then SynMemo1.Lines.SaveToFile(dm.SaveDialog1.FileName);
end;

procedure TForm1.btSublinhadoHtmlClick(Sender: TObject);
begin
  AplicaTags('<u>','</u>');
end;

procedure TForm1.btTabelaHtmlClick(Sender: TObject);
begin
  btTabelaHtml.CheckMenuDropdown;
end;

procedure TForm1.btTitulosHtmlClick(Sender: TObject);
begin
  btTitulosHtml.CheckMenuDropdown;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0:
    begin
      Notebook1.PageIndex := 1;
      Shape5.Visible := False;
      Panel7.Visible := False;
      Splitter2.Visible := False;
      MemoStatus;
    end;
    1:
    begin
      Notebook1.PageIndex := 0;
      Shape5.Visible := False;
      Panel7.Visible := False;
      Splitter2.Visible := False;
    end;
    2:
    begin
      Notebook1.PageIndex := 0;
      Shape5.Visible := True;
      Panel7.Visible := True;
      Splitter2.Visible := True;
    end;
  end;
end;

end.

