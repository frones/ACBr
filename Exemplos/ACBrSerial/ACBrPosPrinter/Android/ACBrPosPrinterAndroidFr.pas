{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interaÁ„o com equipa- }
{ mentos de AutomaÁ„o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  VocÍ pode obter a ˙ltima vers„o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca È software livre; vocÍ pode redistribuÌ-la e/ou modific·-la }
{ sob os termos da LicenÁa P˙blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers„o 2.1 da LicenÁa, ou (a seu critÈrio) }
{ qualquer vers„o posterior.                                                   }
{                                                                              }
{  Esta biblioteca È distribuÌda na expectativa de que seja ˙til, porÈm, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implÌcita de COMERCIABILIDADE OU      }
{ ADEQUA«√O A UMA FINALIDADE ESPECÕFICA. Consulte a LicenÁa P˙blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN«A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  VocÍ deve ter recebido uma cÛpia da LicenÁa P˙blica Geral Menor do GNU junto}
{ com esta biblioteca; se n„o, escreva para a Free Software Foundation, Inc.,  }
{ no endereÁo 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ VocÍ tambÈm pode obter uma copia da licenÁa em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simıes de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - TatuÌ - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrPosPrinterAndroidFr;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Gestures, System.Actions, FMX.ActnList,
  ACBrBase, ACBrPosPrinter,
  ACBrPosPrinterElginE1Service,
  {$IfDef ANDROID}
   {ACBrPosPrinterGEDI,} ACBrPosPrinterElginE1Lib, ACBrPosPrinterTecToySunmiLib,
  {$EndIf}
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.ListBox, FMX.Layouts, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.ScrollBox, FMX.Memo, System.ImageList, FMX.ImgList, FMX.VirtualKeyboard,
  FMX.Memo.Types;

type
  TPosPrinterAndroidTesteForm = class(TForm)
    GestureManager1: TGestureManager;
    tabsPrincipal: TTabControl;
    tabConfig: TTabItem;
    ToolBar1: TToolBar;
    lblTituloConfig: TLabel;
    tabTeste: TTabItem;
    ToolBar2: TToolBar;
    lblTituloTestes: TLabel;
    btnBack: TSpeedButton;
    ACBrPosPrinter1: TACBrPosPrinter;
    ListBox1: TListBox;
    lbImpressoras: TListBoxItem;
    lbModelos: TListBoxItem;
    cbxImpressorasBth: TComboBox;
    btnProcurarBth: TCornerButton;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    Layout1: TLayout;
    SpeedButton1: TSpeedButton;
    btnAtivar: TCornerButton;
    lbLarguraEspacejamento: TListBoxItem;
    GridPanelLayout1: TGridPanelLayout;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    seColunas: TSpinBox;
    seEspLinhas: TSpinBox;
    seLinhasPular: TSpinBox;
    lbBotoes: TListBoxItem;
    GridPanelLayout2: TGridPanelLayout;
    btLerConfig: TCornerButton;
    btSalvarConfig: TCornerButton;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    GridPanelLayout3: TGridPanelLayout;
    btnTiposLetra: TButton;
    btAlinhamento: TButton;
    btnBarras: TButton;
    btQRCode: TButton;
    mImp: TMemo;
    GridPanelLayout4: TGridPanelLayout;
    btnImprimir: TCornerButton;
    btnLimpar: TCornerButton;
    lbCodBarras: TListBoxItem;
    GridPanelLayout5: TGridPanelLayout;
    Label1: TLabel;
    Label4: TLabel;
    cbHRI: TCheckBox;
    seBarrasLargura: TSpinBox;
    seBarrasAltura: TSpinBox;
    ImageList1: TImageList;
    StyleBook1: TStyleBook;
    chbTodasBth: TCheckBox;
    cbxModelo: TComboBox;
    btAcentos: TButton;
    cbxPagCodigo: TComboBox;
    cbSuportaBMP: TCheckBox;
    lbiClasse: TListBoxItem;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    GridPanelLayout6: TGridPanelLayout;
    rbClasseInterna: TRadioButton;
    rbClasseExterna: TRadioButton;
    Layout2: TLayout;
    cbControlePorta: TCheckBox;
    cbSmartPOS: TCheckBox;
    btnLerStatus: TButton;
    btnLerInfo: TButton;
    btBeep: TButton;
    btnAbrirGaveta: TButton;
    btnCortarPapel: TButton;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure btnAtivarClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnProcurarBthClick(Sender: TObject);
    procedure btLerConfigClick(Sender: TObject);
    procedure btSalvarConfigClick(Sender: TObject);
    procedure btnTiposLetraClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btAlinhamentoClick(Sender: TObject);
    procedure btnBarrasClick(Sender: TObject);
    procedure btQRCodeClick(Sender: TObject);
    procedure btnLerStatusClick(Sender: TObject);
    procedure btnLerInfoClick(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure btAcentosClick(Sender: TObject);
    procedure btBeepClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure rbMudaClasseImpressora(Sender: TObject);
    procedure btnAbrirGavetaClick(Sender: TObject);
    procedure btnCortarPapelClick(Sender: TObject);
  private
    { Private declarations }
    fE1Printer: TACBrPosPrinterElginE1Service;
    {$IfDef ANDROID}
    //fGEDIPrinter: TACBrPosPrinterGEDI;
    fE1Lib: TACBrPosPrinterElginE1Lib;
    fSunmiPrinter: TACBrPosPrinterTecToySunmiLib;
    {$EndIf}
    FVKService: IFMXVirtualKeyboardService;

    function CalcularNomeArqINI: String;
    procedure CarregarModelosInternos;
    procedure CarregarModelosExternos;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure ConfigurarACBrPosPrinter;
    function PedirPermissoes: Boolean;

    procedure ExibirErroImpressaoE1(const MsgErro: string);
  public
    { Public declarations }
  end;

var
  PosPrinterAndroidTesteForm: TPosPrinterAndroidTesteForm;

implementation

uses
  System.typinfo, System.IniFiles, System.StrUtils, System.Permissions,
  {$IfDef ANDROID}
  Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  {$EndIf}
  FMX.DialogService, FMX.Platform,
  ACBrUtil, ACBrConsts, ACBrUtil.FilesIO;

{$R *.fmx}

procedure TPosPrinterAndroidTesteForm.FormCreate(Sender: TObject);
var
  p: TACBrPosPaginaCodigo;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FVKService));

  { This defines the default active tab at runtime }
  tabsPrincipal.First;
  tabsPrincipal.TabPosition := TTabPosition.None;

  btnProcurarBthClick(Sender);
  if cbxImpressorasBth.Items.Count > 0 then
    cbxImpressorasBth.ItemIndex := 0;

  cbxPagCodigo.Items.Clear ;
  For p := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
    cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(p) ) ) ;

  fE1Printer := TACBrPosPrinterElginE1Service.Create(ACBrPosPrinter1);
  {$IfDef ANDROID}
   fE1Printer.Modelo := TElginE1Printers.prnM8;
   fE1Printer.OnErroImpressao := ExibirErroImpressaoE1;
  {$Else}
   fE1Printer.Modelo := prnI9;
   // Usar por TXT
   fE1Printer.PastaEntradaE1 := 'c:\E1\pathIN';
   fE1Printer.PastaSaidaE1 := 'c:\E1\pathOUT';
   // Usar por TCP
   //fE1Printer.IPePortaE1 := '192.168.56.1:89';
  {$EndIf}

  {$IfDef ANDROID}
  //fGEDIPrinter := TACBrPosPrinterGEDI.Create(ACBrPosPrinter1);
  fE1Lib := TACBrPosPrinterElginE1Lib.Create(ACBrPosPrinter1);
  fE1Lib.Modelo := TElginE1LibPrinters.prnM8;
  fSunmiPrinter := TACBrPosPrinterTecToySunmiLib.Create(ACBrPosPrinter1);
  {$EndIf}

  LerConfiguracao;
end;

procedure TPosPrinterAndroidTesteForm.FormDestroy(Sender: TObject);
begin
  fE1Printer.Free;
  {$IfDef ANDROID}
  //fGEDIPrinter.Free;
  fE1Lib.Free;
  fSunmiPrinter.Free;
  {$EndIf}
end;

function TPosPrinterAndroidTesteForm.PedirPermissoes: Boolean;
begin
  Result := ACBrPosPrinter1.Device.PedirPermissoesBlueTooth;
end;

procedure TPosPrinterAndroidTesteForm.rbMudaClasseImpressora(Sender: TObject);
begin
  if rbClasseInterna.IsChecked then
    CarregarModelosInternos
  else
    CarregarModelosExternos;
end;

procedure TPosPrinterAndroidTesteForm.btAcentosClick(Sender: TObject);
begin
  if cbxPagCodigo.ItemIndex < 0 then Exit;

  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('Fonte tipo A com P·gina de CÛdigo ' + cbxPagCodigo.Selected.Text);
  mImp.Lines.Add('¿ noite, vovÙ Kowalsky vÍ o Ìm„ cair no pÈ do ping¸im queixoso e vovÛ pıe aÁ˙car no ch· de t‚maras do jabuti feliz.');
  mImp.Lines.Add('¡…Õ”⁄·ÈÌÛ˙Á«„ı√’ Í¬‚‘Ù¿‡');
  mImp.Lines.Add('');
  mImp.Lines.Add('</FB>Fonte tipo B com P·gina de CÛdigo ' + cbxPagCodigo.Selected.Text);
  mImp.Lines.Add('¿ noite, vovÙ Kowalsky vÍ o Ìm„ cair no pÈ do ping¸im queixoso e vovÛ pıe aÁ˙car no ch· de t‚maras do jabuti feliz.');
  mImp.Lines.Add('¡…Õ”⁄·ÈÌÛ˙Á«„ı√’ Í¬‚‘Ù¿‡');
  mImp.Lines.Add('</corte_total>');
end;

procedure TPosPrinterAndroidTesteForm.btAlinhamentoClick(Sender: TObject);
var
  BmpMono: String;
  HasBMP: Boolean;
begin
  BmpMono := ApplicationPath+'acbrmono.bmp';
  HasBMP := cbSuportaBMP.IsChecked and FileExists(BmpMono);

  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('TEXTO NORMAL');
  mImp.Lines.Add('</ae>ALINHADO A ESQUERDA');
  if HasBMP then
    mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA');
  mImp.Lines.Add('</in><S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ce>ALINHADO NO CENTRO');
  if HasBMP then
    mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA');
  mImp.Lines.Add('</in><S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ad>ALINHADO A DIREITA');
  if HasBMP then
    mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA');
  mImp.Lines.Add('</in><S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</ae></fn>TEXTO NORMAL');
  mImp.Lines.Add('</corte_total>');
end;

procedure TPosPrinterAndroidTesteForm.btBeepClick(Sender: TObject);
begin
  ACBrPosPrinter1.Imprimir('</beep>');
end;

procedure TPosPrinterAndroidTesteForm.btLerConfigClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TPosPrinterAndroidTesteForm.btnAbrirGavetaClick(Sender: TObject);
begin
  ACBrPosPrinter1.Imprimir('</abre_gaveta>');
end;

procedure TPosPrinterAndroidTesteForm.btnAtivarClick(Sender: TObject);
begin
  ConfigurarACBrPosPrinter;
  ACBrPosPrinter1.Ativar;

  lblTituloTestes.Text := 'Testes em: '+ACBrPosPrinter1.Porta;
  mImp.Lines.Clear;
  tabsPrincipal.Next;
end;

procedure TPosPrinterAndroidTesteForm.btnBackClick(Sender: TObject);
begin
  ACBrPosPrinter1.Desativar;
  tabsPrincipal.Previous;
end;

procedure TPosPrinterAndroidTesteForm.btnBarrasClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('<barra_mostrar>'+ifthen(cbHRI.IsChecked,'1','0')+'</barra_mostrar>');
  mImp.Lines.Add('<barra_largura>'+IntToStr(Trunc(seBarrasLargura.Value))+'</barra_largura>');
  mImp.Lines.Add('<barra_altura>'+IntToStr(Trunc(seBarrasAltura.Value))+'</barra_altura>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('EAN 8: 1234567');
  mImp.Lines.Add('<ean8>1234567</ean8>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('EAN13: 123456789012');
  mImp.Lines.Add('<ean13>123456789012</ean13>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('std25: 1234567890');
  mImp.Lines.Add('<std>1234567890</std>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('INT25: 1234567890');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE11: 1234567890');
  mImp.Lines.Add('<code11>1234567890</code11>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE39: ABCDE12345');
  mImp.Lines.Add('<code39>ABCDE12345</code39>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE93: ABC123abc');
  mImp.Lines.Add('<code93>ABC123abc</code93>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE128: $-=+ABC123abc');
  mImp.Lines.Add('<code128>$-=+ABC123abc</code128>');
  mImp.Lines.Add('CODE128C: 3515071111111111111159');
  mImp.Lines.Add('<code128c>3515071111111111111159</code128c>');
  if ACBrPosPrinter1.TagsNaoSuportadas.IndexOf(cTagBarraCode128c) >= 0 then
  begin
    mImp.Lines.Add('<c>CODE128C: 3515071111111111111159</c>');
    mImp.Lines.Add('<code128c>3515071111111111111159</code128c>');
    mImp.Lines.Add('<c>CODE128C: 1234567890001135408700</c>');
    mImp.Lines.Add('<code128c>1234567890001135408700</code128c>');
  end
  else
  begin
    mImp.Lines.Add('<c>CODE128C: 35150711111111111111591234567890001135408700</c>');
    mImp.Lines.Add('<code128c>35150711111111111111591234567890001135408700</code128c>');
  end;

  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('UPCA: 12345678901');
  mImp.Lines.Add('<upca>12345678901</upca>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODABAR: A123456789012345A');
  mImp.Lines.Add('<codabar>A123456789012345A</codabar>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('MSI: 1234567890');
  mImp.Lines.Add('<msi>1234567890</msi>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TPosPrinterAndroidTesteForm.btnCortarPapelClick(Sender: TObject);
begin
  ACBrPosPrinter1.Imprimir('</corte>');
end;

procedure TPosPrinterAndroidTesteForm.btnImprimirClick(Sender: TObject);
begin
  ACBrPosPrinter1.Buffer.Text := mImp.Lines.Text;
  ACBrPosPrinter1.Imprimir;
 // ACBrPosPrinter1.Imprimir(mImp.Lines.Text);
end;

procedure TPosPrinterAndroidTesteForm.btnLerInfoClick(Sender: TObject);
begin
  btnLimparClick(Sender);
  mImp.Lines.Add( ACBrPosPrinter1.LerInfoImpressora );
end;

procedure TPosPrinterAndroidTesteForm.btnLerStatusClick(Sender: TObject);
var
  Status: TACBrPosPrinterStatus;
  i: TACBrPosTipoStatus;
  AStr: String;
begin
  btnLimparClick(Sender);
  Status := ACBrPosPrinter1.LerStatusImpressora;

  if Status = [] then
    mImp.Lines.Add('Nennhum Erro encontrado')
  else
  begin
    AStr := '';
    For i := Low(TACBrPosTipoStatus) to High(TACBrPosTipoStatus) do
    begin
      if i in Status then
        AStr := AStr + GetEnumName(TypeInfo(TACBrPosTipoStatus), integer(i) )+ ', ';
    end;

    mImp.Lines.Add( AStr );
  end;
end;

procedure TPosPrinterAndroidTesteForm.btnLimparClick(Sender: TObject);
begin
  mImp.Lines.Clear;
end;

procedure TPosPrinterAndroidTesteForm.btnProcurarBthClick(Sender: TObject);
var
  sl: TStringList;
begin
  if not PedirPermissoes then
    exit;

  cbxImpressorasBth.Items.Clear;
  try
    ACBrPosPrinter1.Device.AcharPortasBlueTooth( cbxImpressorasBth.Items, chbTodasBth.IsChecked );
    cbxImpressorasBth.Items.Add('NULL');
  except
  end;

  sl := TStringList.Create;
  try
    ACBrUtil.FilesIO.FindFiles('/dev/tty*', sl, True );
    cbxImpressorasBth.Items.AddStrings(sl);
  finally
    sl.Free;
  end;
end;

procedure TPosPrinterAndroidTesteForm.btnTiposLetraClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('FONTE NORMAL: '+IntToStr(ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
  mImp.Lines.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteNormal));
  mImp.Lines.Add('<e>EXPANDIDO: '+IntToStr(ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
  mImp.Lines.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteExpandida));
  mImp.Lines.Add('</e><c>CONDENSADO: '+IntToStr(ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
  mImp.Lines.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteCondensada));
  mImp.Lines.Add('</c><n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<in>FONTE INVERTIDA');
  mImp.Lines.Add('</in><S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');
  mImp.Lines.Add('FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('<n>LIGA NEGRITO');
  mImp.Lines.Add('<i>LIGA ITALICO');
  mImp.Lines.Add('<S>LIGA SUBLINHADA');
  mImp.Lines.Add('<c>LIGA CONDENSADA');
  mImp.Lines.Add('<e>LIGA EXPANDIDA');
  mImp.Lines.Add('<a>LIGA ALTURA DUPLA');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('<e><n>NEGRITO E EXPANDIDA</n></e>');
  mImp.Lines.Add('<c><n>NEGRITO E CONDENSADA</n></c>');
  mImp.Lines.Add('<e><a>EXPANDIDA E ALT.DUPLA</a></e>');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('</FB>FONTE TIPO B');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<in>FONTE INVERTIDA');
  mImp.Lines.Add('</in><S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');
  mImp.Lines.Add('</FA>FONTE TIPO A');
  mImp.Lines.Add('</FN>FONTE NORMAL');
  mImp.Lines.Add('</corte_total>');
end;

procedure TPosPrinterAndroidTesteForm.btQRCodeClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('<qrcode_tipo>'+IntToStr(ACBrPosPrinter1.ConfigQRCode.Tipo)+'</qrcode_tipo>');
  mImp.Lines.Add('<qrcode_largura>'+IntToStr(ACBrPosPrinter1.ConfigQRCode.LarguraModulo)+'</qrcode_largura>');
  mImp.Lines.Add('<qrcode_error>'+IntToStr(ACBrPosPrinter1.ConfigQRCode.ErrorLevel)+'</qrcode_error>');
  mImp.Lines.Add('<qrcode>http://projetoacbr.com.br</qrcode>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('</ad>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/questoes_importantes.html</qrcode>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('Exemplo de QRCode para NFCe');
  mImp.Lines.Add('<qrcode_error>0</qrcode_error><qrcode>https://www.homologacao.nfce.fazenda.sp.gov.br/NFCeConsultaPublica/Paginas/ConsultaQRCode.aspx?'+
                 'chNFe=35150805481336000137650220000000711000001960&nVersao=100&tpAmb=2&dhEmi=323031352D30382D31395432323A33333A32352D30333A3030&vNF=3.00&'+
                 'vICMS=0.12&digVal=776967396F2B665861706673396878776E64594C396F61654C35493D&cIdToken=000001&cHashQRCode=9BD312D558823E1EC68CEDB338A39B6150B0480E</qrcode>');
  mImp.Lines.Add('Exemplo de QRCode para SAT');
  mImp.Lines.Add('<qrcode_error>0</qrcode_error><qrcode>35150811111111111111591234567890001672668828|20150820201736|118.72|05481336000137|'+
                 'TCbeD81ePUpMvso4VjFqRTvs4ovqmR1ZG3bwSCumzHtW8bbMedVJjVnww103v3LxKfgckAyuizcR/9pXaKay6M4Gu8kyDef+6VH5qONIZV1cB+mFfXiaCgeZ'+
                 'ALuRDCH1PRyb6hoBeRUkUk6lOdXSczRW9Y83GJMXdOFroEbzFmpf4+WOhe2BZ3mEdXKKGMfl1EB0JWnAThkGT+1Er9Jh/3En5YI4hgQP3NC2BiJVJ6oCEbKb'+
                 '85s5915DSZAw4qB/MlESWViDsDVYEnS/FQgA2kP2A9pR4+agdHmgWiz30MJYqX5Ng9XEYvvOMzl1Y6+7/frzsocOxfuQyFsnfJzogw==</qrcode>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TPosPrinterAndroidTesteForm.btSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

function TPosPrinterAndroidTesteForm.CalcularNomeArqINI: String;
begin
  Result := ApplicationPath + 'ACBrPosPrinter.ini';
end;

procedure TPosPrinterAndroidTesteForm.CarregarModelosExternos;
begin
  cbxModelo.Items.Clear;
  cbxModelo.Items.Add('Elgin E1 Service');
  cbxModelo.Items.Add('Elgin E1 Lib');
  //cbxModelo.Items.Add('Gertec GEDI');
  cbxModelo.Items.Add('TecToy Sunmi Service');
  lbImpressoras.Enabled := False;
end;

procedure TPosPrinterAndroidTesteForm.CarregarModelosInternos;
var
  m: TACBrPosPrinterModelo;
begin
  cbxModelo.Items.Clear;
  For m := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(m) ) );

  lbImpressoras.Enabled := True;
end;

procedure TPosPrinterAndroidTesteForm.cbxModeloChange(Sender: TObject);
begin
  if rbClasseInterna.IsChecked and (cbxModelo.ItemIndex = Integer(ppExterno)) then
    rbClasseExterna.IsChecked := True
  else if rbClasseExterna.IsChecked and (cbxModelo.ItemIndex = 1) then
    cbxPagCodigo.ItemIndex := Integer(pcUTF8);
end;

procedure TPosPrinterAndroidTesteForm.ConfigurarACBrPosPrinter;
begin
  if not PedirPermissoes then
    exit;

  if rbClasseExterna.IsChecked then
  begin
    if cbSmartPOS.IsChecked then
    begin
      fE1Printer.Modelo := TElginE1Printers.prnSmartPOS;
      fE1Lib.Modelo     := TElginE1LibPrinters.prnSmartPOS;
    end
    else
    begin
      fE1Printer.Modelo := TElginE1Printers.prnM8;
      fE1Lib.Modelo     := TElginE1LibPrinters.prnM8;
    end;

    case cbxModelo.ItemIndex of
      0: ACBrPosPrinter1.ModeloExterno := fE1Printer;
      1: ACBrPosPrinter1.ModeloExterno := fE1Lib;
      //2: ACBrPosPrinter1.ModeloExterno := fGEDIPrinter;
    else
      ACBrPosPrinter1.ModeloExterno := fSunmiPrinter;
    end;

    cbxImpressorasBth.ItemIndex := cbxImpressorasBth.Items.IndexOf('NULL');
  end
  else
  begin
    if Assigned(cbxModelo.Selected) then
      ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo(cbxModelo.ItemIndex)
    else
      ACBrPosPrinter1.Modelo := ppTexto;

    if Assigned(cbxImpressorasBth.Selected) then
      ACBrPosPrinter1.Porta := cbxImpressorasBth.Selected.Text
    else if cbxImpressorasBth.ItemIndex = cbxImpressorasBth.Items.IndexOf('NULL') then
      cbxImpressorasBth.ItemIndex := -1;
  end;

  if Assigned(cbxPagCodigo.Selected) then
    ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo(cbxPagCodigo.ItemIndex);

  ACBrPosPrinter1.ColunasFonteNormal := Trunc(seColunas.Value);
  ACBrPosPrinter1.EspacoEntreLinhas := Trunc(seEspLinhas.Value);
  ACBrPosPrinter1.LinhasEntreCupons := Trunc(seLinhasPular.Value);
  ACBrPosPrinter1.ConfigLogo.KeyCode1 := 1;
  ACBrPosPrinter1.ConfigLogo.KeyCode2 := 0;
  ACBrPosPrinter1.ControlePorta := cbControlePorta.IsChecked;
end;

procedure TPosPrinterAndroidTesteForm.ExibirErroImpressaoE1(
  const MsgErro: string);
begin
  TDialogService.MessageDialog( MsgErro,
                                TMsgDlgType.mtError, [TMsgDlgBtn.mbOK],
                                TMsgDlgBtn.mbOk, 0, nil, nil);
end;

procedure TPosPrinterAndroidTesteForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if (FVKService <> nil) then
    begin
      if TVirtualKeyboardState.Visible in FVKService.VirtualKeyboardState then
      begin
        FVKService.HideVirtualKeyboard;
        Key := 0;
        Exit
      end;
    end;

    if (tabsPrincipal.ActiveTab = tabTeste) then
    begin
      tabsPrincipal.Previous;
      Key := 0;
    end;
  end;
end;

procedure TPosPrinterAndroidTesteForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if tabsPrincipal.ActiveTab <> tabsPrincipal.Tabs[tabsPrincipal.TabCount - 1] then
          tabsPrincipal.Next;
        Handled := True;
      end;

    sgiRight:
      begin
        if tabsPrincipal.ActiveTab <> tabsPrincipal.Tabs[0] then
          tabsPrincipal.Previous;
        Handled := True;
      end;
  end;
end;

procedure TPosPrinterAndroidTesteForm.GravarConfiguracao;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := CalcularNomeArqINI;

  INI := TIniFile.Create(ArqINI);
  try
    INI.WriteBool('PosPrinter','ClasseInterna', rbClasseInterna.IsChecked);
    INI.WriteInteger('PosPrinter','Modelo', cbxModelo.ItemIndex);
    INI.WriteBool('PosPrinter','SmartPOS',cbSmartPOS.IsChecked);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo',cbxPagCodigo.ItemIndex);
    INI.WriteBool('Modelo','BMP',cbSuportaBMP.IsChecked);
    if Assigned(cbxImpressorasBth.Selected) then
      INI.WriteString('PosPrinter','Porta', cbxImpressorasBth.Selected.Text);

    INI.WriteInteger('PosPrinter','Colunas', Trunc(seColunas.Value) );
    INI.WriteInteger('PosPrinter','EspacoEntreLinhas', Trunc(seEspLinhas.Value) );
    INI.WriteInteger('PosPrinter','LinhasPular', Trunc(seLinhasPular.Value) );
    INI.WriteBool('PosPrinter','ControlePorta',cbControlePorta.IsChecked);
    INI.WriteInteger('Barras','Largura',Trunc(seBarrasLargura.Value));
    INI.WriteInteger('Barras','Altura',Trunc(seBarrasAltura.Value));
    INI.WriteBool('Barras','HRI',cbHRI.IsChecked);
  finally
    INI.Free ;
  end ;
end;

procedure TPosPrinterAndroidTesteForm.LerConfiguracao;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := CalcularNomeArqINI;

  INI := TIniFile.Create(ArqINI);
  try
    rbClasseInterna.IsChecked   := INI.ReadBool('PosPrinter','ClasseInterna', True);
    rbClasseExterna.IsChecked   := not rbClasseInterna.IsChecked;
    rbMudaClasseImpressora(nil);
    cbxModelo.ItemIndex         := INI.ReadInteger('PosPrinter','Modelo', -1);
    cbSmartPOS.IsChecked        := INI.ReadBool('PosPrinter','SmartPOS', False);
    cbSuportaBMP.IsChecked      := INI.ReadBool('Modelo','BMP', True);
    cbxPagCodigo.ItemIndex      := Ini.ReadInteger('PosPrinter','PaginaDeCodigo', Integer(ACBrPosPrinter1.PaginaDeCodigo));
    cbxImpressorasBth.ItemIndex := cbxImpressorasBth.Items.IndexOf(INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta));
    seColunas.Value             := INI.ReadInteger('PosPrinter','Colunas', 32);
    seEspLinhas.Value           := INI.ReadInteger('PosPrinter','EspacoEntreLinhas', 0);
    seLinhasPular.Value         := INI.ReadInteger('PosPrinter','LinhasPular', 5);
    cbControlePorta.IsChecked   := INI.ReadBool('PosPrinter','ControlePorta', True);
    seBarrasLargura.Value       := INI.ReadInteger('Barras','Largura', ACBrPosPrinter1.ConfigBarras.LarguraLinha);
    seBarrasAltura.Value        := INI.ReadInteger('Barras','Altura', ACBrPosPrinter1.ConfigBarras.Altura);
    cbHRI.IsChecked             := INI.ReadBool('Barras','HRI', ACBrPosPrinter1.ConfigBarras.MostrarCodigo);
  finally
    INI.Free ;
  end;
end;

end.

