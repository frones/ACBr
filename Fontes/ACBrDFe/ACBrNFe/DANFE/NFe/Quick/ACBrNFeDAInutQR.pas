{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - NFe - http://www.nfe.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrNFeDAInutQR;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DAInut favor alterar
// a data e o nome da linha abaixo.
// Última liberação:

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,
  {$IFDEF QReport_PDF}
     QRPDFFilt, QRPrntr,
  {$ENDIF}
  ACBrDFeQRCodeBar, pcnNFe, ACBrNFe, Printers, pcnConversao;

type

  TfrmNFeDAInutQR = class(TForm)
    QRNFeInut: TQuickRep;
    procedure FormDestroy(Sender: TObject);
  private

  protected
    //BarCode : TBarCode128c;
    FACBrNFe        : TACBrNFe;
    FNFe            : TNFe;
    FLogo           : String;
    FNumCopias      : Integer;
    FSistema        : String;
    FUsuario        : String;
    FMostrarPreview : Boolean;
    FMargemSuperior : Double;
    FMargemInferior : Double;
    FMargemEsquerda : Double;
    FMargemDireita  : Double;
    FImpressora     : String;

    procedure SetBarCodeImage(ACode: String; QRImage: TQRImage);
  public
    class procedure Imprimir(AACBrNFe: TACBrNFe;
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
                             ANFe: TNFe = nil);

    class procedure SavePDF(AACBrNFe: TACBrNFe;
                            ALogo: String = '';
                            AFile: String = '';
                            ASistema: String = '';
                            AUsuario: String = '';
                            AMargemSuperior: Double = 0.7;
                            AMargemInferior: Double = 0.7;
                            AMargemEsquerda: Double = 0.7;
                            AMargemDireita: Double = 0.7;
                            ANFe: TNFe = nil);
  end;

implementation

uses
  MaskUtils;

var
  Printer: TPrinter;

{$R *.dfm}

class procedure TfrmNFeDAInutQR.Imprimir(AACBrNFe: TACBrNFe;
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
                                         ANFe: TNFe = nil);
begin
  with Create(nil) do
     try
        FACBrNFe        := AACBrNFe;
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

        if ANFe <> nil then
         FNFe := ANFe;

        Printer := TPrinter.Create;

        if FImpressora > '' then
          QRNFeInut.PrinterSettings.PrinterIndex := Printer.Printers.IndexOf(FImpressora);

        if AMostrarPreview then
         begin
           QRNFeInut.PrinterSettings.Copies := FNumCopias;

         {$IFDEF QReport_PDF}
           QRNFeInut.PrevShowSearch      := False;
           QRNFeInut.PrevShowThumbs      := False;
           QRNFeInut.PreviewInitialState := wsMaximized;
           QRNFeInut.PrevInitialZoom     := qrZoomToWidth;

           QRExportFilterLibrary.AddFilter(TQRPDFDocumentFilter);
         {$ENDIF}

           QRNFeInut.Prepare;
           QRNFeInut.Preview;
           Application.ProcessMessages;
         end else
         begin
           FMostrarPreview := True;
           QRNFeInut.PrinterSettings.Copies := FNumCopias;
           QRNFeInut.Prepare;
           QRNFeInut.Print;
         end;
     finally
        QRNFeInut.Free;
        QRNFeInut := nil;
        Printer.Free;
        Free;
     end;
end;

class procedure TfrmNFeDAInutQR.SavePDF(AACBrNFe: TACBrNFe;
                                        ALogo: String = '';
                                        AFile: String = '';
                                        ASistema: String = '';
                                        AUsuario: String = '';
                                        AMargemSuperior: Double = 0.7;
                                        AMargemInferior: Double = 0.7;
                                        AMargemEsquerda: Double = 0.7;
                                        AMargemDireita: Double = 0.7;
                                        ANFe: TNFe = nil);
{$IFDEF QReport_PDF}
var
  qf: TQRPDFDocumentFilter;
  i: Integer;
{$ENDIF}
begin
{$IFDEF QReport_PDF}
  with Create ( nil ) do
     try
        FACBrNFe        := AACBrNFe;
        FLogo           := ALogo;
        FSistema        := ASistema;
        FUsuario        := AUsuario;
        FMargemSuperior := AMargemSuperior;
        FMargemInferior := AMargemInferior;
        FMargemEsquerda := AMargemEsquerda;
        FMargemDireita  := AMargemDireita;

        if ANFe <> nil then
          FNFe := ANFe;

        for i := 0 to ComponentCount -1 do
          begin
            if (Components[i] is TQRShape) and (TQRShape(Components[i]).Shape = qrsRoundRect) then
              begin
                TQRShape(Components[i]).Shape := qrsRectangle;
                TQRShape(Components[i]).Pen.Width := 1;
              end;
          end;

        FMostrarPreview := True;
        QRNFeInut.Prepare;

        qf := TQRPDFDocumentFilter.Create(AFile);
        try
          qf.CompressionOn := False;
          QRNFeInut.QRPrinter.ExportToFilter(qf);
        finally
          qf.Free;
        end;
     finally
        Free;
     end;
{$ENDIF}
end;

procedure TfrmNFeDAInutQR.SetBarCodeImage(ACode: String; QRImage: TQRImage);
var
  b: TBarCode128c;
begin
  b := TBarCode128c.Create;
  try
    //      Width  := QRImage.Width;
    b.Code := ACode;
    b.PaintCodeToCanvas(ACode, QRImage.Canvas, QRImage.ClientRect);
  finally
    b.free;
  end;
end;

procedure TfrmNFeDAInutQR.FormDestroy(Sender: TObject);
begin
  QRNFeInut.QRPrinter.Free;
  QRNFeInut.Free;
end;

end.

