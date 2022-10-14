unit Androidapi.JNI.SunmiPrinter;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes;

type

  JSunmiPrinter = Interface;

  JSunmiPrinterClass = interface(JObjectClass)
    ['{D10A3CF9-9D61-446E-BD75-F56B5100625F}']
    { class } function init(aContext: JContext): JSunmiPrinter; cdecl;
  end;

  [JavaSignature('com/acbr/sunmi_printer/Printer')]
  JSunmiPrinter = interface(JObject)
    ['{ECC4767A-37E2-4549-AC7A-018E9A8851FE}']
    Procedure PrintTeste; cdecl;
    Function getServiceVersion: JString; cdecl;
    Function getPrinterSerialNo: JString; cdecl;
    Function getPrinterVersion: JString; cdecl;
    Function getPrinterModal: JString; cdecl;
    Procedure printerInit; cdecl;
    Procedure printerSelfChecking; cdecl;
    Procedure lineWrap(n: Integer); cdecl;
    Procedure sendRAWData(data: TJavaArray<Byte>); cdecl;
    Procedure setAlignment(alignment: Integer); cdecl;
    Procedure setFontName(typeface: JString); cdecl;
    Procedure setFontSize(fontsize: Single); cdecl;
    Procedure printText(text: JString); cdecl;
    Procedure printTextLF(text: JString); cdecl;
    Procedure printTextWithFont(text: JString; typeface: JString;
      fontsize: Single); cdecl;
    Procedure printColumnsText(colsTextArr: Array of JString;
      colsWidthArr: Array Of Integer; colsAlign: Array Of Integer); cdecl;
    Procedure printBitmap(bitmap: JBitmap); cdecl;
    Procedure printBarCode(data: JString; symbology, height, width,
      textposition: Integer); cdecl;
    Procedure printQRCode(data: JString; modulesize: Integer;
      errorlevel: Integer); cdecl;
    Procedure printOriginalText(text: JString); cdecl;
    Procedure commitPrinterBuffer; cdecl;
    Procedure enterPrinterBuffer(clean: Boolean); cdecl;
    Procedure exitPrinterBuffer(commit: Boolean); cdecl;

    Procedure cutPaper; cdecl;
    Procedure openDrawer; cdecl;
    Function getCutPaperTimes: Integer; cdecl;
    Function getOpenDrawerTimes: Integer; cdecl;
    Function getPrinterMode: Integer; cdecl;
    Function getPrinterBBMDistance: Integer; cdecl;
    Function updatePrinterState: Integer; cdecl;
    Function getDrawerStatus: Boolean; cdecl;
  end;

  TJSunmiPrinter = class(TJavaGenericImport<JSunmiPrinterClass, JSunmiPrinter>)
  end;

implementation

end.
