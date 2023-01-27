unit uDemoPrinter;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  ACBr.Android.Sunmi.Printer;

type
  TForm2 = class(TForm)
    Button1: TButton;
    ACBr: TImage;
    PrinterState: TText;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ACBrSunmiPrinter1: TACBrSunmiPrinter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    function ObterTextoStatusGaveta: String;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  ACBrSunmiPrinter1
    .setFontSize(10).printTextLF('Teste de impressao Font 10')
    .setFontSize(12).printTextLF('Teste de impressao Font 12')
    .setFontSize(14).printTextLF('Teste de impressao Font 14')
    .setFontSize(16).printTextLF('Teste de impressao Font 16')
    .setFontSize(18).printTextLF('Teste de impressao Font 18')
    .setFontSize(20).printTextLF('Teste de impressao Font 20')
    .setAlignment(1)
    .printBarCode('TESTE', 4)
    .lineWrap(2)
    .setAlignment(2)
    .printText('printText')
    .printTextLF('|printText + LF')
    .lineWrap(1)
    .setAlignment(1)
    .printQRCode('ACBr Automação Comercial Brasil')
    .lineWrap(1)
    .setAlignment(2)
    .printBitmap(ACBr.Bitmap)
    .lineWrap(5)
    .cutPaper;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  case ACBrSunmiPrinter1.PrinterState of
    TACBrSunmiPrinterState.spsOk:
      PrinterState.Text := 'Impressora Ok';
    TACBrSunmiPrinterState.spsInitializing:
      PrinterState.Text := 'Impressora inicializando';
    TACBrSunmiPrinterState.spsError:
      PrinterState.Text := 'Impressora em erro';
    TACBrSunmiPrinterState.spsOutOfPaper:
      PrinterState.Text := 'Impressora sem papel';
    TACBrSunmiPrinterState.spsOverheated:
      PrinterState.Text := 'Impressora Superaquecida';
    TACBrSunmiPrinterState.spsCoverIsOpen:
      PrinterState.Text := 'Impressora com tampa aberta';
    TACBrSunmiPrinterState.spsCutterAbnormal:
      PrinterState.Text := 'Impressora com erro no cortador';
    TACBrSunmiPrinterState.spsCutterNormal:
      PrinterState.Text := 'Impressora com recuo no cortador';
    TACBrSunmiPrinterState.spsPrinterNotDetected:
      PrinterState.Text := 'Impressora não encontrada';
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  ACBrSunmiPrinter1.cutPaper;
  PrinterState.Text := ACBrSunmiPrinter1.CutPaperTimes.ToString + ' Cortes executados';
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  ACBrSunmiPrinter1.openDrawer;
  PrinterState.Text := 'Gaveta aberta ' +
                       ACBrSunmiPrinter1.OpenDrawerTimes.ToString+ ' vezes' + sLineBreak +
                       ObterTextoStatusGaveta;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  PrinterState.Text := ObterTextoStatusGaveta;
end;

function TForm2.ObterTextoStatusGaveta: String;
begin
  if ACBrSunmiPrinter1.DrawerIsOpen then
    Result := 'Gaveta ABERTA'
  else
    Result := 'Gaveta FECHADA';
end;

end.
