{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
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
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 20/08/2009: Caique Rodrigues
|*  - Doação units para geração do Danfe via QuickReport
|* 18/03/2010: André R. Langner
|*  - Acréscimo dos parâmetros "FEmail", "FResumoCanhoto", "FFax", "FNumCopias",
|*    "FSistema", "FSite", "FUsuario", "FImprimeHoraSaida", "FHoraSaida",
|*    nas Class procedures "Imprimir" e "SavePDF"
|*  - Habilitada a funcionalidade da procedure "SavePDF";
|* 23/11/2010: Peterson de Cerqueira Matos
|*  - Acréscimo dos parâmetros "FCasasDecimaisqCom", "FCasasDecimaisvUnCom",
|*    "FImpressora" nas Class procedures "Imprimir" e "SavePDF"
|* 20/05/2011: Peterson de Cerqueira Matos
|*  - Acréscimo do parâmetro "FResumoCanhoto_Texto"
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFeQR;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,
  {$IFDEF QReport_PDF}
     QRPDFFilt,
     QRPrntr,
  {$ENDIF}
  ACBrDFeQRCodeBar, pcnNFe, ACBrNFe, pcnConversao, Printers;

type      
  TfqrDANFeQR = class(TForm)
    QRNFe: TQuickRep;
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    //BarCode : TBarCode128c;
    FACBrNFe            : TACBrNFe;
    FNFe                : TNFe;
    FLogo               : String;
    FEmail              : String;
    FResumoCanhoto      : Boolean;
    FFax                : String;
    FNumCopias          : Integer;
    FSistema            : String;
    FSite               : String;
    FUsuario            : String;
    AfterPreview        : Boolean;
    FExpandirLogoMarca  : Boolean;
    ChangedPos          : Boolean;
    FSemValorFiscal     : Boolean;
    FMargemSuperior     : double;
    FMargemInferior     : double;
    FMargemEsquerda     : double;
    FMargemDireita      : double;
    FCasasDecimaisqCom  : Integer;
    FCasasDecimaisvUnCom: Integer;
    FImpressora         : String;
    FResumoCanhoto_Texto: String;
    // Incluido por Luis Fernando em  22/01/2013
    FNFeCancelada       : Boolean;
    FPosCanhoto         : TPosRecibo;
    FImprimirDescPorc   : Boolean; //incluido por Fernando pasqueto 11/07/2014
    FImprimirDetalhamentoEspecifico : Boolean; //incluido por Fernando pasqueto 15/07/2014
    FImprimirTotalLiquido: Boolean; //incluido Fernando Pasqueto 29/10/2014
    // Incluido por Italo em 27/03/2014
    // Destinado exclusivamente ao DANFE da NFC-e
    FImprimirItens: Boolean;
    
    // Incluido por Edilson Alves de Oliveira em 10/10/2014
    // Destinado exclusivamente ao DANFE da NFC-e
    FViaConsumidor : Boolean;
    FvTroco: Currency;

    procedure qrlSemValorFiscalPrint(sender: TObject; var Value: String);
    procedure SetBarCodeImage ( ACode : String; QRImage : TQRImage );
  public
    { Public declarations }
    HrTotalPages : integer;

    class procedure Imprimir(ANFe                 : TNFe;
                             AACBrNFe             : TACBrNFe;
                             APosCanhoto          : TPosRecibo;
                             ALogo                : String   = '';
                             AEmail               : String   = '';
                             AResumoCanhoto       : Boolean  = False;
                             AFax                 : String   = '';
                             ANumCopias           : Integer  = 1;
                             ASistema             : String   = '';
                             ASite                : String   = '';
                             AUsuario             : String   = '';
                             APreview             : Boolean  = True;
                             AMargemSuperior      : Double   = 0.8;
                             AMargemInferior      : Double   = 0.8;
                             AMargemEsquerda      : Double   = 0.6;
                             AMargemDireita       : Double   = 0.51;
                             ACasasDecimaisqCom   : Integer  = 4;
                             ACasasDecimaisvUncCom: Integer  = 4;
                             AImpressora          : String   = '';
                             AResumoCanhoto_Texto : String   = '';
                             AExpandirLogoMarca   : Boolean  = False;
                             ANFeCancelada        : Boolean  = False;
//                             APosCanhoto          : TPosRecibo  = prCabecalho;
                             AImprimirItens       : Boolean  = True;
                             AViaConsumidor       : Boolean  = True;
                             AvTroco              : Currency = 0.0;
                             AImprimirDescPorc    : Boolean  = False;
                             AImprimirDetalhamentoEspecifico : Boolean  = False;
                             AImprimirTotalLiquido: Boolean = False);

    class procedure SavePDF(AFile                : String;
                            ANFe                 : TNFe;
                            AACBrNFe             : TACBrNFe;
                            APosCanhoto          : TPosRecibo;
                            ALogo                : String   = '';
                            AEmail               : String   = '';
                            AResumoCanhoto       : Boolean  = False;
                            AFax                 : String   = '';
                            ANumCopias           : Integer  = 1;
                            ASistema             : String   = '';
                            ASite                : String   = '';
                            AUsuario             : String   = '';
                            AMargemSuperior      : Double   = 0.8;
                            AMargemInferior      : Double   = 0.8;
                            AMargemEsquerda      : Double   = 0.6;
                            AMargemDireita       : Double   = 0.51;
                            ACasasDecimaisqCom   : Integer  = 4;
                            ACasasDecimaisvUncCom: Integer  = 4;
                            AResumoCanhoto_Texto : String   = '';
                            AExpandirLogoMarca   : Boolean  = False;
                            ANFeCancelada        : Boolean  = False;
//                            APosCanhoto          : TPosRecibo  = prCabecalho;
                            AImprimirItens       : Boolean  = True;
                            AViaConsumidor       : Boolean  = True;
                            AvTroco              : Currency = 0.0;
                            AImprimirDescPorc    : Boolean  = False;
                            AImprimirDetalhamentoEspecifico : Boolean  = False;
                            AImprimirTotalLiquido: Boolean = False);
  end;

implementation

uses
 MaskUtils;

var
 Printer: TPrinter;

{$R *.dfm}

class procedure TfqrDANFeQR.Imprimir(ANFe                 : TNFe;
                                     AACBrNFe             : TACBrNFe;
                                     APosCanhoto          : TPosRecibo;
                                     ALogo                : String   = '';
                                     AEmail               : String   = '';
                                     AResumoCanhoto       : Boolean  = False;
                                     AFax                 : String   = '';
                                     ANumCopias           : Integer  = 1;
                                     ASistema             : String   = '';
                                     ASite                : String   = '';
                                     AUsuario             : String   = '';
                                     APreview             : Boolean  = True;
                                     AMargemSuperior      : Double   = 0.8;
                                     AMargemInferior      : Double   = 0.8;
                                     AMargemEsquerda      : Double   = 0.6;
                                     AMargemDireita       : Double   = 0.51;
                                     ACasasDecimaisqCom   : Integer  = 4;
                                     ACasasDecimaisvUncCom: Integer  = 4;
                                     AImpressora          : String   = '';
                                     AResumoCanhoto_Texto : String   = '';
                                     AExpandirLogoMarca   : Boolean  = False;
                                     ANFeCancelada        : Boolean  = False;
//                                     APosCanhoto          : TPosRecibo  = prCabecalho;
                                     AImprimirItens       : Boolean  = True;
                                     AViaConsumidor       : Boolean  = True;
                                     AvTroco              : Currency = 0.0;
                                     AImprimirDescPorc    : Boolean  = False;
                                     AImprimirDetalhamentoEspecifico : Boolean  = False;
                                     AImprimirTotalLiquido: Boolean = False);
begin
  with Create ( nil ) do
     try
        FNFe                 := ANFe;
        FACBrNFe             := AACBrNFe;
        FLogo                := ALogo;
        FEmail               := AEmail;
        FResumoCanhoto       := AResumoCanhoto;
        FFax                 := AFax;
        FNumCopias           := ANumCopias;
        FSistema             := ASistema;
        FSite                := ASite;
        FUsuario             := AUsuario;
        FMargemSuperior      := AMargemSuperior;
        FMargemInferior      := AMargemInferior;
        FMargemEsquerda      := AMargemEsquerda;
        FMargemDireita       := AMargemDireita;
        FCasasDecimaisqCom   := ACasasDecimaisqCom;
        FCasasDecimaisvUnCom := ACasasDecimaisvUncCom;
        FImpressora          := AImpressora;
        FResumoCanhoto_Texto := AResumoCanhoto_Texto;
        FExpandirLogoMarca   := AExpandirLogoMarca;
        FNFeCancelada        := ANFeCancelada;
        FPosCanhoto          := APosCanhoto;
        FImprimirItens       := AImprimirItens;
        FViaConsumidor       := AViaConsumidor;
        FvTroco              := AvTroco;
        FImprimirDescPorc    := AImprimirDescPorc;
        FImprimirDetalhamentoEspecifico := AImprimirDetalhamentoEspecifico;
        FImprimirTotalLiquido:= AImprimirTotalLiquido;

        Printer := TPrinter.Create;

        if FImpressora > '' then
          QRNFe.PrinterSettings.PrinterIndex := Printer.Printers.IndexOf(FImpressora);

        if APreview
         then begin
           QRNFe.PrinterSettings.Copies := FNumCopias;

         {$IFDEF QReport_PDF}
           QRNFe.PrevShowSearch      := False;
           QRNFe.PrevShowThumbs      := False;
           QRNFe.PreviewInitialState := wsMaximized;
           QRNFe.PrevInitialZoom     := qrZoomToWidth;
           // Incluido por Italo em 16/07/2014
           QRExportFilterLibrary.AddFilter(TQRPDFDocumentFilter);
         {$ENDIF}

           QRNFe.Prepare;
           HrTotalPages := QRNFe.QRPrinter.PageCount;
           QRNFe.Preview;
           // Incluido por Italo em 11/04/2013
           // Segundo o Rodrigo Chiva resolveu o problema de travamento
           // após o fechamento do Preview
           Application.ProcessMessages;
         end
         else begin
           AfterPreview := True;
           QRNFe.PrinterSettings.Copies := FNumCopias;
           QRNFe.Prepare;
           HrTotalPages := QRNFe.QRPrinter.PageCount;
           QRNFe.Print;
         end;

     finally
        // Incluido por Rodrigo Fernandes em 11/03/2013
        // Liberando o objeto Printer da memoria
        Printer.Free;
        Free;
     end;
end;

class procedure TfqrDANFeQR.SavePDF(AFile                : String;
                                    ANFe                 : TNFe;
                                    AACBrNFe             : TACBrNFe;
                                    APosCanhoto          : TPosRecibo;
                                    ALogo                : String   = '';
                                    AEmail               : String   = '';
                                    AResumoCanhoto       : Boolean  = False;
                                    AFax                 : String   = '';
                                    ANumCopias           : Integer  = 1;
                                    ASistema             : String   = '';
                                    ASite                : String   = '';
                                    AUsuario             : String   = '';
                                    AMargemSuperior      : Double   = 0.8;
                                    AMargemInferior      : Double   = 0.8;
                                    AMargemEsquerda      : Double   = 0.6;
                                    AMargemDireita       : Double   = 0.51;
                                    ACasasDecimaisqCom   : Integer  = 4;
                                    ACasasDecimaisvUncCom: Integer  = 4;
                                    AResumoCanhoto_Texto : String   = '';
                                    AExpandirLogoMarca   : Boolean  = False;
                                    ANFeCancelada        : Boolean  = False;
//                                    APosCanhoto          : TPosRecibo  = prCabecalho;
                                    AImprimirItens       : Boolean  = True;
                                    AViaConsumidor       : Boolean  = True;
                                    AvTroco              : Currency = 0.0;
                                    AImprimirDescPorc    : Boolean  = False;
                                    AImprimirDetalhamentoEspecifico : Boolean  = False;
                                    AImprimirTotalLiquido: Boolean = False);
{$IFDEF QReport_PDF}
var
  qf : TQRPDFDocumentFilter;
  i  : integer;
{$ENDIF}
begin
{$IFDEF QReport_PDF}
  with Create ( nil ) do
     try
        FNFe                 := ANFe;
        FACBrNFe             := AACBrNFe;
        FLogo                := ALogo;
        FEmail               := AEmail;
        FResumoCanhoto       := AResumoCanhoto;
        FFax                 := AFax;
        FNumCopias           := ANumCopias;
        FSistema             := ASistema;
        FSite                := ASite;
        FUsuario             := AUsuario;
        FMargemSuperior      := AMargemSuperior;
        FMargemInferior      := AMargemInferior;
        FMargemEsquerda      := AMargemEsquerda;
        FMargemDireita       := AMargemDireita;
        FCasasDecimaisqCom   := ACasasDecimaisqCom;
        FCasasDecimaisvUnCom := ACasasDecimaisvUncCom;
        FResumoCanhoto_Texto := AResumoCanhoto_Texto;
        FExpandirLogoMarca   := AExpandirLogoMarca;
        FNFeCancelada        := ANFeCancelada;
        FPosCanhoto          := APosCanhoto;
        FImprimirItens       := AImprimirItens;
        FViaConsumidor       := AViaConsumidor;
        FvTroco              := AvTroco;
        FImprimirDescPorc    := AImprimirDescPorc;
        FImprimirDetalhamentoEspecifico := AImprimirDetalhamentoEspecifico;
        FImprimirTotalLiquido:= AImprimirTotalLiquido;
        
        for i := 0 to ComponentCount -1 do
          begin
            if (Components[i] is TQRShape) and (TQRShape(Components[i]).Shape = qrsRoundRect) then
              begin
                TQRShape(Components[i]).Shape := qrsRectangle;
                TQRShape(Components[i]).Pen.Width := 1;
              end;
          end;
        AfterPreview := True;
        QRNFe.Prepare;
        qf := TQRPDFDocumentFilter.Create(AFile);
        qf.CompressionOn := False;
        QRNFe.QRPrinter.ExportToFilter( qf );
        qf.Free;
     finally
        Free;
     end;
{$ENDIF}
end;

procedure TfqrDANFeQR.qrlSemValorFiscalPrint(sender: TObject;
  var Value: String);
begin
  inherited;
  if FSemValorFiscal then
     Value := '';
end;

procedure TfqrDANFeQR.SetBarCodeImage(ACode: String; QRImage: TQRImage);
var
 b : TBarCode128c;
begin
   b := TBarCode128c.Create;
//      Width  := QRImage.Width;
   b.Code := ACode;
   b.PaintCodeToCanvas( ACode, QRImage.Canvas, QRImage.ClientRect );
   b.free;
end;

// Incluido por Rodrigo Fernandes em 11/03/2013
procedure TfqrDANFeQR.FormDestroy(Sender: TObject);
begin
  QRNFe.QRPrinter.Free;
  QRNFe.Free;
end;

end.
