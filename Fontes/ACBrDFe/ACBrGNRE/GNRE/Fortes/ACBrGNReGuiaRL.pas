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

{******************************************************************************
|* Historico
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrGNReGuiaRL;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  pgnreGNRERetorno, ACBrGNRE2;

type

  { TfrlGuiaRL }

  TfrlGuiaRL = class(TForm)
  RLGNRe: TRLReport;
  RLPDFFilter1: TRLPDFFilter;
  private
    procedure AjustarMargens(AMargemSuperior: Double; AMargemInferior: Double; AMargemEsquerda: Double; AMargemDireita: Double);
  protected
    FACBrGNRe: TACBrGNRE;
    FGNRe: TGNRERetorno;
    FEmail: string;
    FFax: string;
    FNumCopias: integer;
    FSistema: string;
    FSite: string;
    FUsuario: string;
    AfterPreview: boolean;
    ChangedPos: boolean;
    FSemValorFiscal: boolean;
    FMargemSuperior: double;
    FMargemInferior: double;
    FMargemEsquerda: double;
    FMargemDireita: double;
    FImpressora: string;
  public
    { Public declarations }
    class procedure Imprimir(AOwner: TComponent;
      AGNRe: TGNRERetorno;
      AEmail: string = '';
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      APreview: boolean = True;
      AMargemSuperior: double = 8;
      AMargemInferior: double = 8;
      AMargemEsquerda: double = 6;
      AMargemDireita: double = 5.1;
      AImpressora: string = '';
      APrintDialog  : Boolean = True  );

    class procedure SavePDF(AOwner: TComponent;
      AFile: string;
      AGNRe: TGNRERetorno;
      AEmail: string = '';
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      AMargemSuperior: double = 8;
      AMargemInferior: double = 8;
      AMargemEsquerda: double = 6;
      AMargemDireita: double = 5.1);
  end;

implementation

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrlGuiaRL.Imprimir(AOwner: TComponent;
  AGNRe: TGNRERetorno;
  AEmail: string = '';
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  APreview: boolean = True;
  AMargemSuperior: double = 8;
  AMargemInferior: double = 8;
  AMargemEsquerda: double = 6;
  AMargemDireita: double = 5.1;
  AImpressora: string = '';
  APrintDialog: Boolean = True);
begin
  with Create(AOwner) do
    try
      FGNRe := AGNRe;
      FEmail := AEmail;
      FNumCopias := ANumCopias;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FImpressora := AImpressora;

      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if (FNumCopias > 0) and (RLPrinter.Copies <> FNumCopias) then
      begin
        RLPrinter.Copies := FNumCopias;
      end;


      AjustarMargens(AMargemSuperior, AMargemInferior, AMargemEsquerda, AMargemDireita);

      RLGNRe.PrintDialog := APrintDialog;
      if APreview = True then
        RLGNRe.PreviewModal
      else
        RLGNRe.Print;
    finally
      Free;
    end;
end;

class procedure TfrlGuiaRL.SavePDF(AOwner: TComponent;
  AFile: string;
  AGNRe: TGNRERetorno;
  AEmail: string = '';
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  AMargemSuperior: double = 8;
  AMargemInferior: double = 8;
  AMargemEsquerda: double = 6;
  AMargemDireita: double = 5.1);
begin
  with Create(AOwner) do
    try
      FGNRe := AGNRe;
      FEmail := AEmail;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;

      AjustarMargens(AMargemSuperior, AMargemInferior, AMargemEsquerda, AMargemDireita);

      with RLPDFFilter1.DocumentInfo do
      begin
//        Title := ACBrStr('Guia - GNRe nº ') +  FGNRe.
        //KeyWords := ACBrStr('Número:') + FGNRe.c42_identificadorGuia;
          //ACBrStr('; Data de emissão: ') + FormatDateTime('dd/mm/yyyy', FGNRe.Ide.dhEmi) +'; CNPJ: ' + FGNRe.emit.CNPJ;
      end;

      RLPDFFilter1.FileName := AFile;
      RLGNRe.Prepare;
      RLPDFFilter1.FilterPages(RLGNRe.Pages);

//      RLGNRe.SaveToFile(AFile);
    finally
      Free;
    end;
end;

procedure TfrlGuiaRL.AjustarMargens(AMargemSuperior: Double; AMargemInferior: Double; AMargemEsquerda: Double; AMargemDireita: Double);
begin
  RLGNRe.Margins.TopMargin := AMargemSuperior;
  RLGNRe.Margins.BottomMargin := AMargemInferior;
  RLGNRe.Margins.LeftMargin := AMargemEsquerda;
  RLGNRe.Margins.RightMargin := AMargemDireita;
end;

end.
