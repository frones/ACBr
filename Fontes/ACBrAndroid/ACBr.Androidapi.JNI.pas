{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Jaques Nascimento                               }
{                                                                              }
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

unit ACBr.Androidapi.JNI;

{$I ACBr.inc}

interface

{$IFDEF ANDROID}

Uses
  FMX.Helpers.Android,
  FMX.Platform.Android,
  Androidapi.AppGlue,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI.Net,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Media,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util;

Type

  JSecondScreen = Interface;

  JSecondScreenClass = interface(JObjectClass)
    ['{288A6E4D-D98D-4DF0-B24E-40A65B8057FC}']
    { class } function init(aContext: JActivity): JSecondScreen; cdecl;
  end;

  [JavaSignature('com/tectoy/secondDisplay/SecondScreen')]
  JSecondScreen = interface(JObject)
    ['{43C7E3B7-1EB7-4296-B1A1-941A84A63AB9}']
    Function getView: JView; cdecl;
    Function getWidth: Integer; cdecl;
    Function getHeight: Integer; cdecl;
    Procedure setBitmap(aBitmap: JBitmap; aX, aY: Integer); cdecl;
  end;

  TJSecondScreen = class(TJavaGenericImport<JSecondScreenClass, JSecondScreen>)
  end;

  JStateLeds = Interface;

  JStateLedsClass = Interface(JObjectClass)
    ['{5BC90E4C-EB75-4292-8560-6F5D74B2A118}']
    { class } function init(Context: JContext): JStateLeds; cdecl;
  End;

  [JavaSignature('com/tectoy/leds/Leds')]
  JStateLeds = interface(JObject)
    ['{CDFB063F-3690-4DDA-8B22-3B402EE6F1DB}']
    Procedure controlLamp(status: Integer; lamp: JString); cdecl;
    Procedure controlLampForLoop(status: Integer;
      lightTime, putoutTime: LongInt; lamp: JString); cdecl;
    Procedure closeAllLamp; cdecl;
  end;

  TJStateLeds = class(TJavaGenericImport<JStateLedsClass, JStateLeds>)
  end;

  JVideoPlayer = Interface;

  JVideoPlayerClass = Interface(JObjectClass)
    ['{71C197E2-4980-4FEC-9C19-1B27D6E8F792}']
    { class } function init(aContext: JActivity): JVideoPlayer; cdecl;
  End;

  [JavaSignature('com/tectoy/play/VideoPlayer')]
  JVideoPlayer = interface(JObject)
    ['{54381F35-676E-462A-869C-2A4367BDD274}']
    Procedure setPath(path: JString); cdecl;
    Procedure show; cdecl;
    Procedure hide; cdecl;
    Procedure play; cdecl;
    Procedure pause; cdecl;
    Procedure resume; cdecl;
    Procedure setBounds(x, y, w, h: Integer); cdecl;
  end;

  TJVideoPlayer = class(TJavaGenericImport<JVideoPlayerClass, JVideoPlayer>)
  end;

  JSunmiPrinter = Interface;

  JSunmiPrinterClass = interface(JObjectClass)
    ['{52801898-2E0A-40D9-B86C-13E5F319EF8C}']
    { class } function init(aContext: JContext): JSunmiPrinter; cdecl;
  end;

  [JavaSignature('com/tectoy/sunmiprinter/SunmiPrinter')]
  JSunmiPrinter = interface(JObject)
    ['{45EA6154-3E39-4DCD-A5A2-0A578729D7D9}']
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

{$ENDIF}

implementation

end.
