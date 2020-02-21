unit ACBrPosPrinterAndroidFr;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, ACBrBase, ACBrPosPrinter,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.ListBox, FMX.Layouts, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.ScrollBox, FMX.Memo, System.ImageList, FMX.ImgList, FMX.VirtualKeyboard;

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
    btnLerStatus: TButton;
    btnLerInfo: TButton;
    mImp: TMemo;
    GridPanelLayout4: TGridPanelLayout;
    btnImprimir: TCornerButton;
    btnLimpar: TCornerButton;
    ImageList1: TImageList;
    ListBoxItem1: TListBoxItem;
    GridPanelLayout5: TGridPanelLayout;
    Label1: TLabel;
    Label4: TLabel;
    cbHRI: TCheckBox;
    seBarrasLargura: TSpinBox;
    seBarrasAltura: TSpinBox;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    ListBoxGroupHeader6: TListBoxGroupHeader;
    GridPanelLayout6: TGridPanelLayout;
    cbxModelo: TComboBox;
    cbSuportaBMP: TCheckBox;
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
  private
    { Private declarations }
    FVKService: IFMXVirtualKeyboardService;

    function CalcularNomeArqINI: String;
    procedure LerINI;
    procedure GravarINI;
    procedure ConfigurarACBrPosPrinter;
    function PedirPermissoes: Boolean;
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
  ACBrUtil, ACBrConsts;

{$R *.fmx}

procedure TPosPrinterAndroidTesteForm.FormCreate(Sender: TObject);
var
  I: TACBrPosPrinterModelo;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FVKService));

  { This defines the default active tab at runtime }
  tabsPrincipal.First;
  tabsPrincipal.TabPosition := TTabPosition.None;

  btnProcurarBthClick(Sender);
  if cbxImpressorasBth.Items.Count > 0 then
    cbxImpressorasBth.ItemIndex := 0;

  cbxModelo.Items.Clear ;
  For I := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(I) ) ) ;

  if cbxModelo.Items.Count > 0 then
    cbxModelo.ItemIndex := 0;

  LerINI;
end;

function TPosPrinterAndroidTesteForm.PedirPermissoes: Boolean;
Var
  Ok: Boolean;
begin
  Ok := True;
  {$IfDef ANDROID}
  PermissionsService.RequestPermissions( [JStringToString(TJManifest_permission.JavaClass.BLUETOOTH),
                                          JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_ADMIN),
                                          JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_PRIVILEGED)],
      procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
      var
        GR: TPermissionStatus;
      begin
        for GR in AGrantResults do
          if (GR <> TPermissionStatus.Granted) then
          begin
            Ok := False;
            Break;
          end;
      end );

  if not OK then
  begin
    TDialogService.MessageDialog( 'Sem permissões para acessar despositivo BlueTooth',
                                  TMsgDlgType.mtError, [TMsgDlgBtn.mbOK],
                                  TMsgDlgBtn.mbOk, 0, nil, nil);
  end;
  {$EndIf}

  Result := Ok;
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
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ce>ALINHADO NO CENTRO');
  if HasBMP then
    mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ad>ALINHADO A DIREITA');
  if HasBMP then
    mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</ae></fn>TEXTO NORMAL');
  mImp.Lines.Add('</corte_total>');
end;

procedure TPosPrinterAndroidTesteForm.btLerConfigClick(Sender: TObject);
begin
  LerINI;
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
begin
  if not PedirPermissoes then
    exit;

  cbxImpressorasBth.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasBlueTooth( cbxImpressorasBth.Items );
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
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
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
  mImp.Lines.Add('<in><e>INVERTIDA E EXPANDIDA</e></in>');
  mImp.Lines.Add('<in><c>INVERTIDA E CONDENSADA</c></in>');
  mImp.Lines.Add('<in><a>INVERTIDA E ALT.DUPLA</a></in>');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('</FB>FONTE TIPO B');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
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
  GravarINI;
end;

function TPosPrinterAndroidTesteForm.CalcularNomeArqINI: String;
begin
  Result := ApplicationPath + 'ACBrPosPrinter.ini';
end;

procedure TPosPrinterAndroidTesteForm.ConfigurarACBrPosPrinter;
begin
  if not PedirPermissoes then
    exit;

  if Assigned(cbxImpressorasBth.Selected) then
    ACBrPosPrinter1.Porta := cbxImpressorasBth.Selected.Text;

  if Assigned(cbxModelo.Selected) then
    ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo(cbxModelo.ItemIndex);

  ACBrPosPrinter1.ColunasFonteNormal := Trunc(seColunas.Value);
  ACBrPosPrinter1.EspacoEntreLinhas := Trunc(seEspLinhas.Value);
  ACBrPosPrinter1.LinhasEntreCupons := Trunc(seLinhasPular.Value);
  ACBrPosPrinter1.ConfigLogo.KeyCode1 := 1;
  ACBrPosPrinter1.ConfigLogo.KeyCode2 := 0;
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

procedure TPosPrinterAndroidTesteForm.GravarINI;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := CalcularNomeArqINI;

  INI := TIniFile.Create(ArqINI);
  try
    INI.WriteInteger('PosPrinter','Modelo', cbxModelo.ItemIndex);
    INI.WriteBool('Modelo','BMP',cbSuportaBMP.IsChecked);
    if Assigned(cbxImpressorasBth.Selected) then
      INI.WriteString('PosPrinter','Porta', cbxImpressorasBth.Selected.Text);

    INI.WriteInteger('PosPrinter','Colunas', Trunc(seColunas.Value) );
    INI.WriteInteger('PosPrinter','EspacoEntreLinhas', Trunc(seEspLinhas.Value) );
    INI.WriteInteger('PosPrinter','LinhasPular', Trunc(seLinhasPular.Value) );
    INI.WriteInteger('Barras','Largura',Trunc(seBarrasLargura.Value));
    INI.WriteInteger('Barras','Altura',Trunc(seBarrasAltura.Value));
    INI.WriteBool('Barras','HRI',cbHRI.IsChecked);
  finally
    INI.Free ;
  end ;
end;

procedure TPosPrinterAndroidTesteForm.LerINI;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := CalcularNomeArqINI;

  INI := TIniFile.Create(ArqINI);
  try
    cbxModelo.ItemIndex := INI.ReadInteger('PosPrinter','Modelo', Integer(ACBrPosPrinter1.Modelo));
    cbSuportaBMP.IsChecked := INI.ReadBool('Modelo','BMP',False);
    cbxImpressorasBth.ItemIndex := cbxImpressorasBth.Items.IndexOf(INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta));
    seColunas.Value := INI.ReadInteger('PosPrinter','Colunas',ACBrPosPrinter1.ColunasFonteNormal);
    seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoEntreLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasPular',ACBrPosPrinter1.LinhasEntreCupons);
    seBarrasLargura.Value := INI.ReadInteger('Barras','Largura',ACBrPosPrinter1.ConfigBarras.LarguraLinha);
    seBarrasAltura.Value := INI.ReadInteger('Barras','Altura',ACBrPosPrinter1.ConfigBarras.Altura);
    cbHRI.IsChecked  := INI.ReadBool('Barras','HRI',ACBrPosPrinter1.ConfigBarras.MostrarCodigo);
  finally
    INI.Free ;
  end ;
end;

end.

