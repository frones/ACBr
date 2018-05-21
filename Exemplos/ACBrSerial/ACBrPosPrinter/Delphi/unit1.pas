unit Unit1;

interface

uses
  Classes, SysUtils, ACBrPosPrinter, ACBrBase, Forms, StdCtrls, Buttons,
  Spin, Controls, ComCtrls, ExtCtrls;

type

  { TFrPosPrinterTeste }

  TFrPosPrinterTeste = class(TForm)
    ACBrPosPrinter1: TACBrPosPrinter;
    bAtivar: TBitBtn;
    bImprimir: TBitBtn;
    bImpTagsValidas: TButton;
    bLerInfo: TButton;
    bLimpar: TBitBtn;
    bTagFormtacaoCaracter: TButton;
    bTagQRCode: TButton;
    bLerStatus: TButton;
    bTagLogo: TButton;
    bTagsAlinhamento: TButton;
    bTagsCodBarras: TButton;
    bTagsTesteInvalidas: TButton;
    bTagsTestePagCodigo: TButton;
    bImpLinhaALinha: TButton;
    cbHRI: TCheckBox;
    cbxModelo: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    cbIgnorarTags: TCheckBox;
    cbTraduzirTags: TCheckBox;
    cbControlePorta: TCheckBox;
    edLog: TEdit;
    gbCodBarrasConfig1: TGroupBox;
    gbCodBarrasConfig2: TGroupBox;
    gbConfiguracao: TGroupBox;
    gbCodBarrasConfig: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    mImp: TMemo;
    mLog: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SbArqLog: TSpeedButton;
    btSerial: TSpeedButton;
    seLogoFatorX: TSpinEdit;
    seLogoFatorY: TSpinEdit;
    seQRCodeLarguraModulo: TSpinEdit;
    seQRCodeErrorLevel: TSpinEdit;
    seLogoKC2: TSpinEdit;
    seQRCodeTipo: TSpinEdit;
    seColunas: TSpinEdit;
    seBarrasLargura: TSpinEdit;
    seEspLinhas: TSpinEdit;
    seBarrasAltura: TSpinEdit;
    seLinhasBuffer: TSpinEdit;
    seLinhasPular: TSpinEdit;
    seLogoKC1: TSpinEdit;
    tsImprimir: TTabSheet;
    tsLog: TTabSheet;
    cbCortarPapel: TCheckBox;
    bTagsTestePageMode: TButton;
    procedure ACBrPosPrinter1GravarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure bAtivarClick(Sender: TObject);
    procedure bImprimirClick(Sender: TObject);
    procedure bImpTagsValidasClick(Sender: TObject);
    procedure bLerInfoClick(Sender: TObject);
    procedure bLerStatusClick(Sender: TObject);
    procedure bLimparClick(Sender: TObject);
    procedure bTagFormtacaoCaracterClick(Sender: TObject);
    procedure bTagLogoClick(Sender: TObject);
    procedure bTagQRCodeClick(Sender: TObject);
    procedure bTagsAlinhamentoClick(Sender: TObject);
    procedure bTagsTesteInvalidasClick(Sender: TObject);
    procedure bTagsCodBarrasClick(Sender: TObject);
    procedure bTagsTestePagCodigoClick(Sender: TObject);
    procedure bImpLinhaALinhaClick(Sender: TObject);
    procedure cbControlePortaChange(Sender: TObject);
    procedure cbHRIChange(Sender: TObject);
    procedure cbIgnorarTagsChange(Sender: TObject);
    procedure cbTraduzirTagsChange(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure cbxPagCodigoChange(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure SbArqLogClick(Sender: TObject);
    procedure seBarrasAlturaChange(Sender: TObject);
    procedure seBarrasLarguraChange(Sender: TObject);
    procedure seEspLinhasChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
    procedure seColunasChange(Sender: TObject);
    procedure seLinhasBufferChange(Sender: TObject);
    procedure seLinhasPularChange(Sender: TObject);
    procedure seLogoFatorXChange(Sender: TObject);
    procedure seLogoFatorYChange(Sender: TObject);
    procedure seLogoKC1Change(Sender: TObject);
    procedure seLogoKC2Change(Sender: TObject);
    procedure seQRCodeErrorLevelChange(Sender: TObject);
    procedure seQRCodeLarguraModuloChange(Sender: TObject);
    procedure seQRCodeTipoChange(Sender: TObject);
    procedure cbCortarPapelClick(Sender: TObject);
    procedure bTagsTestePageModeClick(Sender: TObject);
  private
    Procedure GravarINI ;
    Procedure LerINI ;
  public
    
  end;

var
  FrPosPrinterTeste: TFrPosPrinterTeste;

implementation

Uses 
  typinfo, IniFiles, Printers, ConfiguraSerial, ACBrUtil;

{$R *.dfm}

{ TFrPosPrinterTeste }

procedure TFrPosPrinterTeste.FormCreate(Sender: TObject);
var
  I: TACBrPosPrinterModelo;
  J: TACBrPosPaginaCodigo;
  K: Integer;
begin
  cbxModelo.Items.Clear ;
  For I := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(I) ) ) ;

  cbxPagCodigo.Items.Clear ;
  For J := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(J) ) ) ;

  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );
  cbxPorta.Items.Add('LPT1') ;
  cbxPorta.Items.Add('LPT2') ;
  cbxPorta.Items.Add('/dev/ttyS0') ;
  cbxPorta.Items.Add('/dev/ttyS1') ;
  cbxPorta.Items.Add('/dev/ttyUSB0') ;
  cbxPorta.Items.Add('/dev/ttyUSB1') ;
  cbxPorta.Items.Add('\\localhost\Epson') ;
  cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;

  For K := 0 to Printer.Printers.Count-1 do
    cbxPorta.Items.Add('RAW:'+Printer.Printers[K]);

  PageControl1.ActivePageIndex := 0;

  LerINI;
end;


procedure TFrPosPrinterTeste.bLimparClick(Sender: TObject);
begin
  mImp.Clear;
end;

procedure TFrPosPrinterTeste.bTagFormtacaoCaracterClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('FONTE NORMAL: '+IntToStr(ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
  mImp.Lines.Add(PadLeft('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteNormal));
  mImp.Lines.Add('<e>EXPANDIDO: '+IntToStr(ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
  mImp.Lines.Add(PadLeft('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteExpandida));
  mImp.Lines.Add('</e><c>CONDENSADO: '+IntToStr(ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
  mImp.Lines.Add(PadLeft('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteCondensada));
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
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('<e><n>NEGRITO E EXPANDIDA</n></e>');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('<in><c>INVERTIDA E CONDENSADA</c></in>');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('</FB>FONTE TIPO B');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');
  mImp.Lines.Add('</FA>FONTE TIPO A');
  mImp.Lines.Add('</FN>FONTE NORMAL');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagLogoClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('Logo: '+chr(ACBrPosPrinter1.ConfigLogo.KeyCode1)+
                          chr(ACBrPosPrinter1.ConfigLogo.KeyCode2) +
                          ' - FatorX: ' + IntToStr(ACBrPosPrinter1.ConfigLogo.FatorX)+
                          ' - FatorY: ' + IntToStr(ACBrPosPrinter1.ConfigLogo.FatorY));
  mImp.Lines.Add('</logo>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagQRCodeClick(Sender: TObject);
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
  mImp.Lines.Add('<qrcode_error>0</qrcode_error>'+
       '<qrcode>https://www.homologacao.nfce.fazenda.sp.gov.br/NFCeConsultaPublica/Paginas/ConsultaQRCode.aspx?chNFe=35150805481336000137650220000000711000001960'+
       '&nVersao=100&tpAmb=2&dhEmi=323031352D30382D31395432323A33333A32352D30333A3030&vNF=3.00&vICMS=0.12'+
       '&digVal=776967396F2B665861706673396878776E64594C396F61654C35493D&cIdToken=000001&cHashQRCode=9BD312D558823E1EC68CEDB338A39B6150B0480E</qrcode>');
  mImp.Lines.Add('Exemplo de QRCode para SAT');
  mImp.Lines.Add('<qrcode_error>0</qrcode_error>'+
       '<qrcode>35150811111111111111591234567890001672668828|20150820201736|118.72|05481336000137|'+
       'TCbeD81ePUpMvso4VjFqRTvs4ovqmR1ZG3bwSCumzHtW8bbMedVJjVnww103v3LxKfgckAyuizcR/9pXaKay6M4Gu8kyDef+6VH5qONIZV1cB+mFfXiaCgeZALuRDCH1PRyb6hoBeRUkUk6'+
       'lOdXSczRW9Y83GJMXdOFroEbzFmpf4+WOhe2BZ3mEdXKKGMfl1EB0JWnAThkGT+1Er9Jh/3En5YI4hgQP3NC2BiJVJ6oCEbKb85s5915DSZAw4qB/MlESWViDsDVYEnS/FQgA2kP2A9pR4+'+
       'agdHmgWiz30MJYqX5Ng9XEYvvOMzl1Y6+7/frzsocOxfuQyFsnfJzogw==</qrcode>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsAlinhamentoClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('TEXTO NORMAL');
  mImp.Lines.Add('</ae>ALINHADO A ESQUERDA');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ce>ALINHADO NO CENTRO');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ad>ALINHADO A DIREITA');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</ae></fn>TEXTO NORMAL');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsTesteInvalidasClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add(ACBrStr('<CE>*** TESTE DE TAGS INV¡LIDAS ***</CE>'));
  mImp.Lines.Add(ACBrStr('<ce> <>tags inv·lidas no texto">">><<</CE>'));
  mImp.Lines.Add('<AD><da><ec></</A Direita</ad>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsCodBarrasClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
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

procedure TFrPosPrinterTeste.bTagsTestePagCodigoClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add(ACBrStr('¡…Õ”⁄·ÈÌÛ˙Á«„ı√’ Í¿‡'));
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bImpLinhaALinhaClick(Sender: TObject);
begin
  ACBrPosPrinter1.ImprimirLinha('</zera>');
  ACBrPosPrinter1.ImprimirLinha('</linha_dupla>');
  ACBrPosPrinter1.ImprimirLinha('FONTE NORMAL: '+IntToStr(ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
  ACBrPosPrinter1.ImprimirLinha(PadLeft('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteNormal));
  ACBrPosPrinter1.ImprimirLinha('<e>EXPANDIDO: '+IntToStr(ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
  ACBrPosPrinter1.ImprimirLinha(PadLeft('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteExpandida));
  ACBrPosPrinter1.ImprimirLinha('</e><c>CONDENSADO: '+IntToStr(ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
  ACBrPosPrinter1.ImprimirLinha(PadLeft('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteCondensada));
  ACBrPosPrinter1.ImprimirLinha('</c><n>FONTE NEGRITO</N>');
  ACBrPosPrinter1.ImprimirLinha('<in>FONTE INVERTIDA</in>');
  ACBrPosPrinter1.ImprimirLinha('<S>FONTE SUBLINHADA</s>');
  ACBrPosPrinter1.ImprimirLinha('<i>FONTE ITALICO</i>');
  ACBrPosPrinter1.ImprimirLinha('FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</linha_simples>');
  ACBrPosPrinter1.ImprimirLinha('<n>LIGA NEGRITO');
  ACBrPosPrinter1.ImprimirLinha('<i>LIGA ITALICO');
  ACBrPosPrinter1.ImprimirLinha('<S>LIGA SUBLINHADA');
  ACBrPosPrinter1.ImprimirLinha('<c>LIGA CONDENSADA');
  ACBrPosPrinter1.ImprimirLinha('<e>LIGA EXPANDIDA');
  ACBrPosPrinter1.ImprimirLinha('</fn>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</linha_simples>');
  ACBrPosPrinter1.ImprimirLinha('<e><n>NEGRITO E EXPANDIDA</n></e>');
  ACBrPosPrinter1.ImprimirLinha('</fn>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('<in><c>INVERTIDA E CONDENSADA</c></in>');
  ACBrPosPrinter1.ImprimirLinha('</fn>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</linha_simples>');
  ACBrPosPrinter1.ImprimirLinha('</FB>FONTE TIPO B');
  ACBrPosPrinter1.ImprimirLinha('<n>FONTE NEGRITO</N>');
  ACBrPosPrinter1.ImprimirLinha('<e>FONTE EXPANDIDA</e>');
  ACBrPosPrinter1.ImprimirLinha('<in>FONTE INVERTIDA</in>');
  ACBrPosPrinter1.ImprimirLinha('<S>FONTE SUBLINHADA</s>');
  ACBrPosPrinter1.ImprimirLinha('<i>FONTE ITALICO</i>');
  ACBrPosPrinter1.ImprimirLinha('</FA>FONTE TIPO A');
  ACBrPosPrinter1.ImprimirLinha('</FN>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</corte_total>');
end;

procedure TFrPosPrinterTeste.cbControlePortaChange(Sender: TObject);
begin
  ACBrPosPrinter1.ControlePorta := cbControlePorta.Checked;
end;

procedure TFrPosPrinterTeste.cbHRIChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.MostrarCodigo := cbHRI.Checked;
end;

procedure TFrPosPrinterTeste.cbIgnorarTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.IgnorarTags := cbIgnorarTags.Checked;
end;

procedure TFrPosPrinterTeste.cbTraduzirTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.TraduzirTags := cbTraduzirTags.Checked;
end;

procedure TFrPosPrinterTeste.cbxModeloChange(Sender: TObject);
begin
  try
     ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModelo.ItemIndex ) ;
  except
     cbxModelo.ItemIndex := Integer( ACBrPosPrinter1.Modelo ) ;
     raise ;
  end ;
end;

procedure TFrPosPrinterTeste.cbxPagCodigoChange(Sender: TObject);
begin
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo(cbxPagCodigo.ItemIndex);
end;

procedure TFrPosPrinterTeste.cbxPortaChange(Sender: TObject);
begin
  try
    ACBrPosPrinter1.Porta := cbxPorta.Text ;
  finally
    cbxPorta.Text := ACBrPosPrinter1.Porta ;
  end ;

  btSerial.Visible := ACBrPosPrinter1.Device.IsSerialPort;
end;

procedure TFrPosPrinterTeste.SbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim,edLog.Text) = 0 then
    AFileLog := ExtractFilePath( Application.ExeName ) + edLog.Text
  else
    AFileLog := edLog.Text;

  OpenURL( AFileLog );
end;

procedure TFrPosPrinterTeste.seBarrasAlturaChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.Altura := seBarrasAltura.Value;
end;

procedure TFrPosPrinterTeste.seBarrasLarguraChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.LarguraLinha := seBarrasLargura.Value;
end;

procedure TFrPosPrinterTeste.seEspLinhasChange(Sender: TObject);
begin
  ACBrPosPrinter1.EspacoEntreLinhas := seEspLinhas.Value;
end;

procedure TFrPosPrinterTeste.btSerialClick(Sender: TObject);
Var
  frConfiguraSerial : TfrConfiguraSerial ;
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    frConfiguraSerial.Device.Porta        := ACBrPosPrinter1.Device.Porta ;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text ;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString ;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
       cbxPorta.Text                       := frConfiguraSerial.Device.Porta ;
       ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString ;
    end ;
  finally
     FreeAndNil( frConfiguraSerial ) ;
  end ;
end;

procedure TFrPosPrinterTeste.seColunasChange(Sender: TObject);
begin
  ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
end;

procedure TFrPosPrinterTeste.seLinhasBufferChange(Sender: TObject);
begin
  ACBrPosPrinter1.LinhasBuffer := seLinhasBuffer.Value;
end;

procedure TFrPosPrinterTeste.seLinhasPularChange(Sender: TObject);
begin
  ACBrPosPrinter1.LinhasEntreCupons := seLinhasPular.Value;
end;

procedure TFrPosPrinterTeste.seLogoFatorXChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.FatorX := seLogoFatorX.Value;
end;

procedure TFrPosPrinterTeste.seLogoFatorYChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.FatorY := seLogoFatorY.Value;
end;

procedure TFrPosPrinterTeste.seLogoKC1Change(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.KeyCode1 := seLogoKC1.Value;
end;

procedure TFrPosPrinterTeste.seLogoKC2Change(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.KeyCode2 := seLogoKC2.Value;
end;

procedure TFrPosPrinterTeste.seQRCodeErrorLevelChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigQRCode.ErrorLevel := seQRCodeErrorLevel.Value;
end;

procedure TFrPosPrinterTeste.seQRCodeLarguraModuloChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigQRCode.LarguraModulo := seQRCodeLarguraModulo.Value;
end;

procedure TFrPosPrinterTeste.seQRCodeTipoChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigQRCode.Tipo := seQRCodeTipo.Value;
end;

procedure TFrPosPrinterTeste.GravarINI;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := ChangeFileExt( Application.ExeName,'.ini' ) ;

  INI := TIniFile.Create(ArqINI);
  try
     INI.WriteInteger('PosPrinter','Modelo',cbxModelo.ItemIndex);
     INI.WriteString('PosPrinter','Porta',cbxPorta.Text);
     INI.WriteInteger('PosPrinter','Colunas',seColunas.Value);
     INI.WriteInteger('PosPrinter','EspacoEntreLinhas',seEspLinhas.Value);
     INI.WriteInteger('PosPrinter','LinhasBuffer',seLinhasBuffer.Value);
     INI.WriteInteger('PosPrinter','LinhasPular',seLinhasPular.Value);
     INI.WriteBool('PosPrinter','CortarPapel',cbCortarPapel.Checked);
     INI.WriteBool('PosPrinter','ControlePorta',cbControlePorta.Checked);
     INI.WriteBool('PosPrinter','TraduzirTags',cbTraduzirTags.Checked);
     INI.WriteBool('PosPrinter','IgnorarTags',cbIgnorarTags.Checked);
     INI.WriteString('PosPrinter','ArqLog',edLog.Text);
     INI.WriteInteger('PosPrinter','PaginaDeCodigo',cbxPagCodigo.ItemIndex);
     INI.WriteInteger('Barras','Largura',seBarrasLargura.Value);
     INI.WriteInteger('Barras','Altura',seBarrasAltura.Value);
     INI.WriteBool('Barras','HRI',cbHRI.Checked);
     INI.WriteInteger('QRCode','Tipo',seQRCodeTipo.Value);
     INI.WriteInteger('QRCode','LarguraModulo',seQRCodeLarguraModulo.Value);
     INI.WriteInteger('QRCode','ErrorLevel',seQRCodeErrorLevel.Value);
     INI.WriteInteger('Logo','KC1',seLogoKC1.Value);
     INI.WriteInteger('Logo','KC2',seLogoKC2.Value);
     INI.WriteInteger('Logo','FatorX',seLogoFatorX.Value);
     INI.WriteInteger('Logo','FatorY',seLogoFatorY.Value);
  finally
     INI.Free ;
  end ;
end;

procedure TFrPosPrinterTeste.LerINI;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := ChangeFileExt( Application.ExeName,'.ini' ) ;

  INI := TIniFile.Create(ArqINI);
  try
     cbxModelo.ItemIndex := INI.ReadInteger('PosPrinter','Modelo', Integer(ACBrPosPrinter1.Modelo));
     cbxPorta.Text := INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta);
     cbxPortaChange(nil);
     seColunas.Value := INI.ReadInteger('PosPrinter','Colunas',ACBrPosPrinter1.ColunasFonteNormal);
     seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoEntreLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
     seLinhasBuffer.Value := INI.ReadInteger('PosPrinter','LinhasBuffer',ACBrPosPrinter1.LinhasBuffer);
     seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasPular',ACBrPosPrinter1.LinhasEntreCupons);
     cbCortarPapel.Checked := INI.ReadBool('PosPrinter','CortarPapel',ACBrPosPrinter1.CortaPapel);
     cbControlePorta.Checked := INI.ReadBool('PosPrinter','ControlePorta',ACBrPosPrinter1.ControlePorta);
     cbTraduzirTags.Checked := INI.ReadBool('PosPrinter','TraduzirTags',ACBrPosPrinter1.TraduzirTags);
     cbIgnorarTags.Checked := INI.ReadBool('PosPrinter','IgnorarTags',ACBrPosPrinter1.IgnorarTags);
     edLog.Text := INI.ReadString('PosPrinter','ArqLog',ACBrPosPrinter1.ArqLOG);
     cbxPagCodigo.ItemIndex := INI.ReadInteger('PosPrinter','PaginaDeCodigo',Integer(ACBrPosPrinter1.PaginaDeCodigo));
     seBarrasLargura.Value := INI.ReadInteger('Barras','Largura',ACBrPosPrinter1.ConfigBarras.LarguraLinha);
     seBarrasAltura.Value := INI.ReadInteger('Barras','Altura',ACBrPosPrinter1.ConfigBarras.Altura);
     cbHRI.Checked := INI.ReadBool('Barras','HRI',ACBrPosPrinter1.ConfigBarras.MostrarCodigo);
     seQRCodeTipo.Value := INI.ReadInteger('QRCode','Tipo',ACBrPosPrinter1.ConfigQRCode.Tipo);
     seQRCodeLarguraModulo.Value := INI.ReadInteger('QRCode','LarguraModulo',ACBrPosPrinter1.ConfigQRCode.LarguraModulo);
     seQRCodeErrorLevel.Value := INI.ReadInteger('QRCode','ErrorLevel',ACBrPosPrinter1.ConfigQRCode.ErrorLevel);
     seLogoKC1.Value := INI.ReadInteger('Logo','KC1',ACBrPosPrinter1.ConfigLogo.KeyCode1);
     seLogoKC2.Value := INI.ReadInteger('Logo','KC2',ACBrPosPrinter1.ConfigLogo.KeyCode2);
     seLogoFatorX.Value := INI.ReadInteger('Logo','FatorX',ACBrPosPrinter1.ConfigLogo.FatorX);
     seLogoFatorY.Value := INI.ReadInteger('Logo','FatorY',ACBrPosPrinter1.ConfigLogo.FatorY);
  finally
     INI.Free ;
  end ;
end;

procedure TFrPosPrinterTeste.bImprimirClick(Sender: TObject);
begin
  if bAtivar.Caption = 'Ativar' then
    bAtivar.Click;

  ACBrPosPrinter1.Imprimir(mImp.Text);
end;

procedure TFrPosPrinterTeste.bImpTagsValidasClick(Sender: TObject);
begin
  ACBrPosPrinter1.RetornarTags(mImp.Lines);
  ACBrPosPrinter1.ImprimirTags;
end;

procedure TFrPosPrinterTeste.bLerInfoClick(Sender: TObject);
begin
  mImp.Lines.Add( ACBrPosPrinter1.LerInfoImpressora );
end;

procedure TFrPosPrinterTeste.bLerStatusClick(Sender: TObject);
var
  Status: TACBrPosPrinterStatus;
  i: TACBrPosTipoStatus;
  AStr: String;
begin
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

procedure TFrPosPrinterTeste.bAtivarClick(Sender: TObject);
begin
  if ACBrPosPrinter1.Ativo then
  begin
     ACBrPosPrinter1.Desativar ;
     bAtivar.Caption := 'Ativar' ;
     btSerial.Enabled := True ;
  end
  else
  begin
    try
       Self.Enabled := False;
       ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModelo.ItemIndex );
       ACBrPosPrinter1.Porta  := cbxPorta.Text;
       ACBrPosPrinter1.ArqLOG := edLog.Text;
       ACBrPosPrinter1.LinhasBuffer := seLinhasBuffer.Value;
       ACBrPosPrinter1.LinhasEntreCupons := seLinhasPular.Value;
       ACBrPosPrinter1.EspacoEntreLinhas := seEspLinhas.Value;
       ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
       ACBrPosPrinter1.CortaPapel := cbCortarPapel.Checked;
       ACBrPosPrinter1.ControlePorta := cbControlePorta.Checked;
       ACBrPosPrinter1.TraduzirTags := cbTraduzirTags.Checked;
       ACBrPosPrinter1.IgnorarTags := cbIgnorarTags.Checked;
       ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo( cbxPagCodigo.ItemIndex );
       ACBrPosPrinter1.ConfigBarras.MostrarCodigo := cbHRI.Checked;
       ACBrPosPrinter1.ConfigBarras.LarguraLinha := seBarrasLargura.Value;
       ACBrPosPrinter1.ConfigBarras.Altura := seBarrasAltura.Value;
       ACBrPosPrinter1.ConfigQRCode.Tipo := seQRCodeTipo.Value;
       ACBrPosPrinter1.ConfigQRCode.LarguraModulo := seQRCodeLarguraModulo.Value;
       ACBrPosPrinter1.ConfigQRCode.ErrorLevel := seQRCodeErrorLevel.Value;
       ACBrPosPrinter1.ConfigLogo.KeyCode1 := seLogoKC1.Value;
       ACBrPosPrinter1.ConfigLogo.KeyCode2 := seLogoKC2.Value;
       ACBrPosPrinter1.ConfigLogo.FatorX := seLogoFatorX.Value;
       ACBrPosPrinter1.ConfigLogo.FatorY := seLogoFatorY.Value;

       ACBrPosPrinter1.Ativar ;

       btSerial.Enabled := False ;
       bAtivar.Caption := 'Desativar' ;

       GravarINI ;
    finally
       Self.Enabled := True;
       cbxModelo.ItemIndex   := Integer(ACBrPosPrinter1.Modelo) ;
       cbxPorta.Text         := ACBrPosPrinter1.Porta ;
    end ;
  end;
end;

procedure TFrPosPrinterTeste.ACBrPosPrinter1GravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  mLog.Lines.Add(ALogLine);
  Tratado := False;
end;

procedure TFrPosPrinterTeste.cbCortarPapelClick(Sender: TObject);
begin
  ACBrPosPrinter1.CortaPapel := cbCortarPapel.Checked;
end;

procedure TFrPosPrinterTeste.bTagsTestePageModeClick(Sender: TObject);
begin
  mImp.Lines.Add('</zera><barra_mostrar>0</barra_mostrar><barra_largura>2</barra_largura><barra_altura>40</barra_altura>');
  mImp.Lines.Add('<mp><mp_direcao>0</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>0</mp_esquerda><mp_largura>257</mp_largura><mp_altura>740</mp_altura><mp_espaco>50</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('<mp_direcao>1</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>210</mp_esquerda><mp_largura>400</mp_largura><mp_altura>500</mp_altura><mp_espaco>25</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('</mp>');
  mImp.Lines.Add('MODO PAGINA DESLIGADO');
  mImp.Lines.Add('<mp><mp_direcao>3</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>0</mp_esquerda><mp_largura>400</mp_largura><mp_altura>500</mp_altura><mp_espaco>25</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('<mp_direcao>2</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>350</mp_esquerda><mp_largura>257</mp_largura><mp_altura>740</mp_altura><mp_espaco>50</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('</mp>');
  mImp.Lines.Add('');
  mImp.Lines.Add('</corte_total>');
end;

end.

