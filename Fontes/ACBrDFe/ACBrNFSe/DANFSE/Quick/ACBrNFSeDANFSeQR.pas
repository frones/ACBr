{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
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

{$I ACBr.inc}

unit ACBrNFSeDANFSeQR;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DANFSE favor alterar
// a data e o nome da linha abaixo.
// Última liberação:
// 05/09/2013 por Italo Jurisato Junior

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,
  {$IFDEF QReport_PDF}
     QRPDFFilt,
     QRPrntr,
  {$ENDIF}
  pnfsNFSe, ACBrNFSe, Printers;

type
  TfqrDANFSeQR = class(TForm)
    QRNFSe: TQuickRep;
  private
    { Private declarations }
  protected
    //BarCode : TBarCode128c ;
    FACBrNFSe       : TACBrNFSe;
    FNFSe           : TNFSe;
    FLogo           : String;
    FEmail          : String;
    FFax            : String;
    FNumCopias      : Integer;
    FSistema        : String;
    FSite           : String;
    FUsuario        : String;
    AfterPreview    : Boolean;
    ChangedPos      : Boolean;
    FSemValorFiscal : Boolean;
    FMargemSuperior : double;
    FMargemInferior : double;
    FMargemEsquerda : double;
    FMargemDireita  : double;
    FImpressora     : String;
    FPrestLogo      : String;
    FPrefeitura     : String;
    FNFSeCancelada  : Boolean;
    FImprimeCanhoto : Boolean;

    procedure qrlSemValorFiscalPrint(sender: TObject; var Value: String);
    procedure SetBarCodeImage (ACode: String; QRImage: TQRImage);
  public
    { Public declarations }
    class procedure Imprimir(ANFSe           : TNFSe;
                             ALogo           : String  = '';
                             AEmail          : String  = '';
                             AFax            : String  = '';
                             ANumCopias      : Integer = 1;
                             ASistema        : String  = '';
                             ASite           : String  = '';
                             AUsuario        : String  = '' ;
                             APreview        : Boolean = True;
                             AMargemSuperior : Double  = 0.8;
                             AMargemInferior : Double  = 0.8;
                             AMargemEsquerda : Double  = 0.6;
                             AMargemDireita  : Double  = 0.51;
                             AImpressora     : String  = '';
                             APrestLogo      : String  = '';
                             APrefeitura     : String  = '';
                             ANFSeCancelada  : Boolean = False;
                             AImprimeCanhoto : Boolean = False);

    class procedure SavePDF(AFile           : String;
                            ANFSe           : TNFSe;
                            ALogo           : String  = '';
                            AEmail          : String  = '';
                            AFax            : String  = '';
                            ANumCopias      : Integer = 1;
                            ASistema        : String  = '';
                            ASite           : String  = '';
                            AUsuario        : String  = '';
                            AMargemSuperior : Double  = 0.8;
                            AMargemInferior : Double  = 0.8;
                            AMargemEsquerda : Double  = 0.6;
                            AMargemDireita  : Double  = 0.51;
                            APrestLogo      : String  = '';
                            APrefeitura     : String  = '';
                            ANFSeCancelada  : Boolean = False;
                            AImprimeCanhoto : Boolean = False);

  end;

implementation

uses
 MaskUtils;

var
 Printer: TPrinter;

{$R *.dfm}

class procedure TfqrDANFSeQR.Imprimir(ANFSe           : TNFSe;
                                      ALogo           : String  = '';
                                      AEmail          : String  = '';
                                      AFax            : String  = '';
                                      ANumCopias      : Integer = 1;
                                      ASistema        : String  = '';
                                      ASite           : String  = '';
                                      AUsuario        : String  = '' ;
                                      APreview        : Boolean = True;
                                      AMargemSuperior : Double  = 0.8;
                                      AMargemInferior : Double  = 0.8;
                                      AMargemEsquerda : Double  = 0.6;
                                      AMargemDireita  : Double  = 0.51;
                                      AImpressora     : String  = '';
                                      APrestLogo      : String  = '';
                                      APrefeitura     : String  = '';
                                      ANFSeCancelada  : Boolean = False;
                                      AImprimeCanhoto : Boolean = False);
{$IFDEF QReport_PDF}
var
 qf : TQRPDFFilter;
{$ENDIF}
begin
 with Create ( nil ) do
  try
   FNFSe           := ANFSe;
   FLogo           := ALogo;
   FEmail          := AEmail;
   FFax            := AFax;
   FNumCopias      := ANumCopias;
   FSistema        := ASistema;
   FSite           := ASite;
   FUsuario        := AUsuario;
   FMargemSuperior := AMargemSuperior;
   FMargemInferior := AMargemInferior;
   FMargemEsquerda := AMargemEsquerda;
   FMargemDireita  := AMargemDireita;
   FImpressora     := AImpressora;
   FPrestLogo      := APrestLogo;
   FPrefeitura     := APrefeitura;
   FNFSeCancelada  := ANFSeCancelada;
   FImprimeCanhoto := AImprimeCanhoto;

   Printer := TPrinter.Create;

   if FImpressora > ''
    then QRNFSe.PrinterSettings.PrinterIndex := Printer.Printers.IndexOf(FImpressora);

   if APreview
    then begin
     QRNFSe.PrinterSettings.Copies := FNumCopias;

     {$IFDEF QReport_PDF}
       QRNFSe.PrevShowSearch      := False;
       QRNFSe.PrevShowThumbs      := False;
       QRNFSe.PreviewInitialState := wsMaximized;
       QRNFSe.PrevInitialZoom     := qrZoomToWidth;

       qf := TQRPDFFilter.Create(nil);
     {$ENDIF}

     QRNFSe.Prepare;
     QRNFSe.Preview;
     // Incluido por Italo em 11/04/2013
     // Segundo o Rodrigo Chiva resolveu o problema de travamento
     // após o fechamento do Preview
     Application.ProcessMessages;
    {$IFDEF QReport_PDF}
     qf.Free;
    {$ENDIF}
    end
    else begin
     AfterPreview := True;
     QRNFSe.PrinterSettings.Copies := FNumCopias;
     QRNFSe.Prepare;
     QRNFSe.Print;
    end;

  finally
   Free;
  end;
end;

class procedure TfqrDANFSeQR.SavePDF(AFile           : String;
                                     ANFSe           : TNFSe;
                                     ALogo           : String  = '';
                                     AEmail          : String  = '';
                                     AFax            : String  = '';
                                     ANumCopias      : Integer = 1;
                                     ASistema        : String  = '';
                                     ASite           : String  = '';
                                     AUsuario        : String  = '';
                                     AMargemSuperior : Double  = 0.8;
                                     AMargemInferior : Double  = 0.8;
                                     AMargemEsquerda : Double  = 0.6;
                                     AMargemDireita  : Double  = 0.51;
                                     APrestLogo      : String  = '';
                                     APrefeitura     : String  = '';
                                     ANFSeCancelada  : Boolean = False;
                                     AImprimeCanhoto : Boolean = False);

{$IFDEF QReport_PDF}
var
 qf : TQRPDFDocumentFilter;
 i  : integer;
{$ENDIF}
begin
{$IFDEF QReport_PDF}
  with Create ( nil ) do
   try
    FNFSe           := ANFSe;
    FLogo           := ALogo;
    FEmail          := AEmail;
    FFax            := AFax;
    FNumCopias      := ANumCopias;
    FSistema        := ASistema;
    FSite           := ASite;
    FUsuario        := AUsuario;
    FMargemSuperior := AMargemSuperior;
    FMargemInferior := AMargemInferior;
    FMargemEsquerda := AMargemEsquerda;
    FMargemDireita  := AMargemDireita;
    FPrestLogo      := APrestLogo;
    FPrefeitura     := APrefeitura;
    FNFSeCancelada  := ANFSeCancelada;
    FimprimeCanhoto := AImprimeCanhoto;

    for i := 0 to ComponentCount -1 do
     begin
      if (Components[i] is TQRShape) and (TQRShape(Components[i]).Shape = qrsRoundRect)
       then begin
        TQRShape(Components[i]).Shape := qrsRectangle;
        TQRShape(Components[i]).Pen.Width := 1;
       end;
      end;

    AfterPreview := True;
    QRNFSe.Prepare;
    qf := TQRPDFDocumentFilter.Create(AFile);
    qf.CompressionOn := False;
    QRNFSe.QRPrinter.ExportToFilter( qf );
    qf.Free;
   finally
    Free;
   end;
{$ENDIF}
end;

procedure TfqrDANFSeQR.qrlSemValorFiscalPrint(sender: TObject;
  var Value: String);
begin
  inherited;
 if FSemValorFiscal
  then Value := '';
end;

procedure TfqrDANFSeQR.SetBarCodeImage(ACode: String; QRImage: TQRImage);
//var
// b : TBarCode128c;
begin
//   b := TBarCode128c.Create;
//   Width  := QRImage.Width;
//   b.Code := ACode ;
//   b.PaintCodeToCanvas(ACode, QRImage.Canvas, QRImage.ClientRect);
//   b.free;
end;

end.
