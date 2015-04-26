{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAEventoQR;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DAEvento favor alterar
// a data e o nome da linha abaixo.
// Última liberação:
// 26/11/2013 por Italo Jurisato Junior

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,
  {$IFDEF QReport_PDF}
     QRPDFFilt, QRPrntr,
  {$ENDIF}
  ACBrDFeQRCodeBar, pmdfeMDFe, ACBrMDFe, Printers,
  pcnConversao, pmdfeEnvEventoMDFe;

type

  TfrmMDFeDAEventoQR = class(TForm)
    QRMDFeEvento: TQuickRep;

    procedure FormDestroy(Sender: TObject);
  private

  protected
    //BarCode: TBarCode128c;
    FACBrMDFe: TACBrMDFe;
    FMDFe: TMDFe;
    FEventoMDFe: TInfEventoCollectionItem;
    FLogo: String;
    FNumCopias: Integer;
    FSistema: String;
    FUsuario: String;
    FMostrarPreview: Boolean;
    FMargemSuperior: Double;
    FMargemInferior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FImpressora: String;

    procedure SetBarCodeImage(ACode: String; QRImage: TQRImage);
  public
    class procedure Imprimir(AEventoMDFe: TInfEventoCollectionItem;
                             ALogo: String = '';
                             ANumCopias: Integer = 1;
                             ASistema: String = '';
                             AUsuario: String = '';
                             AMostrarPreview: Boolean = True;
                             AMargemSuperior: Double = 0.7;
                             AMargemInferior: Double = 0.7;
                             AMargemEsquerda: Double = 0.7;
                             AMargemDireita: Double = 0.7;
                             AImpressora: String = '';
                             AMDFe: TMDFe = nil);

    class procedure SavePDF(AEventoMDFe: TInfEventoCollectionItem;
                            ALogo: String = '';
                            AFile: String = '';
                            ASistema: String = '';
                            AUsuario: String = '';
                            AMargemSuperior: Double = 0.7;
                            AMargemInferior: Double = 0.7;
                            AMargemEsquerda: Double = 0.7;
                            AMargemDireita: Double = 0.7;
                            AMDFe: TMDFe = nil);
  end;

implementation

uses
  MaskUtils;

var
  Printer: TPrinter;

{$R *.dfm}

class procedure TfrmMDFeDAEventoQR.Imprimir(AEventoMDFe: TInfEventoCollectionItem;
                                            ALogo: String = '';
                                            ANumCopias: Integer = 1;
                                            ASistema: String = '';
                                            AUsuario: String = '';
                                            AMostrarPreview: Boolean = True;
                                            AMargemSuperior: Double = 0.7;
                                            AMargemInferior: Double = 0.7;
                                            AMargemEsquerda: Double = 0.7;
                                            AMargemDireita: Double = 0.7;
                                            AImpressora: String = '';
                                            AMDFe: TMDFe = nil);
begin
  with Create(nil) do
     try
        FEventoMDFe     := AEventoMDFe;
        FLogo           := ALogo;
        FNumCopias      := ANumCopias;
        FSistema        := ASistema;
        FUsuario        := AUsuario;
        FMostrarPreview := AMostrarPreview;
        FMargemSuperior := AMargemSuperior;
        FMargemInferior := AMargemInferior;
        FMargemEsquerda := AMargemEsquerda;
        FMargemDireita  := AMargemDireita;
        FImpressora     := AImpressora;

        if AMDFe <> nil then
         FMDFe := AMDFe;

        Printer := TPrinter.Create;

        if FImpressora > '' then
          QRMDFeEvento.PrinterSettings.PrinterIndex := Printer.Printers.IndexOf(FImpressora);

        if AMostrarPreview then
         begin
           QRMDFeEvento.PrinterSettings.Copies := FNumCopias;

         {$IFDEF QReport_PDF}
           QRMDFeEvento.PrevShowSearch      := False;
           QRMDFeEvento.PrevShowThumbs      := False;
           QRMDFeEvento.PreviewInitialState := wsMaximized;
           QRMDFeEvento.PrevInitialZoom     := qrZoomToWidth;

           // Incluido por Italo em 14/02/2012
           // QRMDFeEvento.PreviewDefaultSaveType := stPDF;
           // Incluido por Italo em 16/02/2012
           QRExportFilterLibrary.AddFilter(TQRPDFDocumentFilter);
         {$ENDIF}

           QRMDFeEvento.Prepare;
//           FTotalPages := QREvento.QRPrinter.PageCount;
           QRMDFeEvento.Preview;
           // Incluido por Italo em 11/04/2013
           // Segundo o Rodrigo Chiva resolveu o problema de travamento
           // após o fechamento do Preview
           Application.ProcessMessages;
         end else
         begin
           FMostrarPreview := True;
           QRMDFeEvento.PrinterSettings.Copies := FNumCopias;
           QRMDFeEvento.Prepare;
//           FTotalPages := QREvento.QRPrinter.PageCount;
           QRMDFeEvento.Print;
         end;
     finally
        // Incluido por Rodrigo Fernandes em 17/06/2013
        // QRMDFeEvento.QRPrinter.Free;
        // QRMDFeEvento.QRPrinter:=nil;

        // Incluido por Italo em 21/08/2013
        QRMDFeEvento.Free;
        QRMDFeEvento := nil;

        // Incluido por Rodrigo Fernandes em 11/03/2013
        // Liberando o objeto Printer da memoria
        Printer.Free;
        Free;
     end;
end;

class procedure TfrmMDFeDAEventoQR.SavePDF(AEventoMDFe: TInfEventoCollectionItem;
                                           ALogo: String = '';
                                           AFile: String = '';
                                           ASistema: String = '';
                                           AUsuario: String = '';
                                           AMargemSuperior: Double = 0.7;
                                           AMargemInferior: Double = 0.7;
                                           AMargemEsquerda: Double = 0.7;
                                           AMargemDireita: Double = 0.7;
                                           AMDFe: TMDFe = nil);
{$IFDEF QReport_PDF}
 var
  qf: TQRPDFDocumentFilter;
  i: Integer;
{$ENDIF}
begin
{$IFDEF QReport_PDF}
  with Create(nil) do
     try
        FEventoMDFe     := AEventoMDFe;
        FLogo           := ALogo;
        FSistema        := ASistema;
        FUsuario        := AUsuario;
        FMargemSuperior := AMargemSuperior;
        FMargemInferior := AMargemInferior;
        FMargemEsquerda := AMargemEsquerda;
        FMargemDireita  := AMargemDireita;

        if AMDFe <> nil then
          FMDFe := AMDFe;

        for i := 0 to ComponentCount -1 do
          begin
            if (Components[i] is TQRShape) and (TQRShape(Components[i]).Shape = qrsRoundRect) then
              begin
                TQRShape(Components[i]).Shape     := qrsRectangle;
                TQRShape(Components[i]).Pen.Width := 1;
              end;
          end;

        FMostrarPreview := True;
        QRMDFeEvento.Prepare;
        // Incluido por Italo em 27/08/2013
//        FTotalPages := QREvento.QRPrinter.PageCount;

        qf := TQRPDFDocumentFilter.Create(AFile);
        try
          qf.CompressionOn := False;
          QRMDFeEvento.QRPrinter.ExportToFilter( qf );
        finally
          qf.Free;
        end;
     finally
        Free;
     end;
{$ENDIF}
end;

procedure TfrmMDFeDAEventoQR.SetBarCodeImage(ACode: String; QRImage: TQRImage);
var
  b: TBarCode128c;
begin
  b := TBarCode128c.Create;
  try
    // Width  := QRImage.Width;
    b.Code := ACode;
    b.PaintCodeToCanvas(ACode, QRImage.Canvas, QRImage.ClientRect);
  finally
    b.free;
  end;
end;

// Incluido por Rodrigo Fernandes em 11/03/2013
procedure TfrmMDFeDAEventoQR.FormDestroy(Sender: TObject);
begin
  QRMDFeEvento.QRPrinter.Free;
  QRMDFeEvento.Free;
end;

end.

