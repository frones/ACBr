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

unit ACBrMDFeDAMDFeRL;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, pmdfeMDFe, ACBrMDFe,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts;

type

  { TfrlDAMDFeRL }

  TfrlDAMDFeRL = class(TForm)
    RLMDFe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
  private
    { Private declarations }
  protected
    FACBrMDFe: TACBrMDFe;
    FMDFe: TMDFe;
    FLogo: string;
    FEmail: string;
    FFax: string;
    FNumCopias: integer;
    FSistema: string;
    FSite: string;
    FUsuario: string;
    AfterPreview: boolean;
    FExpandirLogoMarca: boolean;
    ChangedPos: boolean;
    FSemValorFiscal: boolean;
    FMargemSuperior: double;
    FMargemInferior: double;
    FMargemEsquerda: double;
    FMargemDireita: double;
    FImpressora: string;
    FMDFeCancelada: boolean;
    FMDFeEncerrado: boolean;
    procedure rllSemValorFiscalPrint(Sender: TObject; var Value: string);
  public
    { Public declarations }
    class procedure Imprimir(AOwner: TComponent; 
		  AMDFe: TMDFe; ALogo: string = '';
      AEmail: string = '';
      AExpandirLogoMarca: boolean = False;
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      APreview: boolean = True;
      AMargemSuperior: double = 0.8;
      AMargemInferior: double = 0.8;
      AMargemEsquerda: double = 0.6;
      AMargemDireita: double = 0.51;
      AImpressora: string = '';
      AMDFeCancelada: boolean = False;
      AMDFeEncerrado: boolean = False;
      APrintDialog  : Boolean = True  );

    class procedure SavePDF(AOwner: TComponent; 
		  AFile: string; AMDFe: TMDFe;
      ALogo: string = '';
      AEmail: string = '';
      AExpandirLogoMarca: boolean = False;
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      AMargemSuperior: double = 0.8;
      AMargemInferior: double = 0.8;
      AMargemEsquerda: double = 0.6;
      AMargemDireita: double = 0.51;
      AMDFeCancelada: boolean = False;
      AMDFeEncerrado: boolean = False);

  end;

implementation

uses
  MaskUtils, ACBrUtil, ACBrMDFeDAMDFeRLClass;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrlDAMDFeRL.Imprimir(AOwner: TComponent;
  AMDFe: TMDFe;
  ALogo: string = '';
  AEmail: string = '';
  AExpandirLogoMarca: boolean = False;
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  APreview: boolean = True;
  AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8;
  AMargemEsquerda: double = 0.6;
  AMargemDireita: double = 0.51;
  AImpressora: string = '';
  AMDFeCancelada: boolean = False;
  AMDFeEncerrado: boolean = False;
  APrintDialog: Boolean = True);
begin
  with Create(AOwner) do
    //with TfrlDAMDFeRL do
    try
      FMDFe := AMDFe;
      FLogo := ALogo;
      FEmail := AEmail;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FImpressora := AImpressora;
      FMDFeCancelada := AMDFeCancelada;
      FMDFeEncerrado := AMDFeEncerrado;

      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if FNumCopias > 0 then
        RLPrinter.Copies := FNumCopias
      else
        RLPrinter.Copies := 1;

      RLMDFe.PrintDialog := APrintDialog;
      if APreview = True then
        RLMDFe.PreviewModal
      else
        RLMDFe.Print;
    finally
      Free;
    end;
end;

class procedure TfrlDAMDFeRL.SavePDF(AOwner: TComponent;
  AFile: string;
  AMDFe: TMDFe;
  ALogo: string = '';
  AEmail: string = '';
  AExpandirLogoMarca: boolean = False;
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8;
  AMargemEsquerda: double = 0.6;
  AMargemDireita: double = 0.51;
  AMDFeCancelada: boolean = False;
  AMDFeEncerrado: boolean = False);
begin
  with Create(AOwner) do
    //with TfrlDAMDFeRL do
    try
      FMDFe := AMDFe;
      FLogo := ALogo;
      FEmail := AEmail;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FMDFeCancelada := AMDFeCancelada;
      FMDFeEncerrado := AMDFeEncerrado;

      RLMDFe.ShowProgress := TACBrMDFeDAMDFeRL(AOwner).MostrarStatus;;
      RLPDFFilter1.ShowProgress := TACBrMDFeDAMDFeRL(AOwner).MostrarStatus;;
      RLPDFFilter1.FileName := AFile;

      with RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('DAMDFe - MDFe nº ') +
          FormatFloat('000,000,000', FMDFe.Ide.nMDF);
        KeyWords := ACBrStr('Número:') + FormatFloat('000,000,000', FMDFe.Ide.nMDF) +
          ACBrStr('; Data de emissão: ') + FormatDateTime('dd/mm/yyyy', FMDFe.Ide.dhEmi) +
          '; CNPJ: ' + FMDFe.emit.CNPJCPF;
      end;

      RLMDFe.Prepare;
      RLPDFFilter1.FilterPages(RLMDFe.Pages);
//      RLMDFe.SaveToFile(AFile);
    finally
      Free;
    end;
end;

procedure TfrlDAMDFeRL.rllSemValorFiscalPrint(Sender: TObject; var Value: string);
begin
  inherited;
  if FSemValorFiscal then
    Value := '';
end;

end.
