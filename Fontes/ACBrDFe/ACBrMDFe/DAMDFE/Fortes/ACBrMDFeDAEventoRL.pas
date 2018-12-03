{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAEventoRL;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  {$IFDEF BORLAND}DB, DBClient, {$ELSE}BufDataset, DB, RLFilters, {$ENDIF}
  RLReport, RLPDFFilter, RLPrinters,
  ACBrMDFe, ACBrMDFeDAMDFeRLClass, ACBrDFeReportFortes,
  pmdfeMDFe, pcnConversao, pmdfeEnvEventoMDFe;

type

  { TfrmMDFeDAEventoRL }

  TfrmMDFeDAEventoRL = class(TForm)
    Datasource1: TDatasource;
    RLMDFeEvento: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormDestroy(Sender: TObject);
  protected
    fpACBrMDFe: TACBrMDFe;
    fpMDFe: TMDFe;
    fpEventoMDFe: TInfEventoCollectionItem;
    fpDAMDFe: TACBrMDFeDAMDFeRL;

  public
    class procedure Imprimir(ADAMDFe: TACBrMDFeDAMDFeRL;
      AEventoMDFe: TInfEventoCollectionItem; AMDFe: TMDFe = nil);
    class procedure SalvarPDF(ADAMDFe: TACBrMDFeDAMDFeRL;
      AEventoMDFe: TInfEventoCollectionItem; AFile: string; AMDFe: TMDFe = nil);
  end;

implementation

uses
  MaskUtils, ACBrUtil;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}

{$endif}

class procedure TfrmMDFeDAEventorl.Imprimir(ADAMDFe: TACBrMDFeDAMDFeRL;
      AEventoMDFe: TInfEventoCollectionItem; AMDFe: TMDFe = nil);
var
  DAMDFeReport: TfrmMDFeDAEventorl;
begin
  DAMDFeReport := Create(nil);
  try
    DAMDFeReport.fpDAMDFe := ADAMDFe;
    DAMDFeReport.fpEventoMDFe := AEventoMDFe;
    TDFeReportFortes.AjustarReport(DAMDFeReport.RLMDFeEvento, DAMDFeReport.fpDAMDFe);

    if (AMDFe <> nil) then
      DAMDFeReport.fpMDFe := AMDFe;

    if DAMDFeReport.fpDAMDFe.MostraPreview then
      DAMDFeReport.RLMDFeEvento.PreviewModal
    else
      DAMDFeReport.RLMDFeEvento.Print;
  finally
    DAMDFeReport.Free;
  end;
end;

class procedure TfrmMDFeDAEventorl.SalvarPDF(ADAMDFe: TACBrMDFeDAMDFeRL;
      AEventoMDFe: TInfEventoCollectionItem; AFile: string; AMDFe: TMDFe = nil);
var
  DAMDFeReport: TfrmMDFeDAEventorl;
begin
  DAMDFeReport := Create(nil);
  try;
    DAMDFeReport.fpDAMDFe := ADAMDFe;
    DAMDFeReport.fpEventoMDFe := AEventoMDFe;
    TDFeReportFortes.AjustarReport(DAMDFeReport.RLMDFeEvento, DAMDFeReport.fpDAMDFe);
    TDFeReportFortes.AjustarFiltroPDF(DAMDFeReport.RLPDFFilter1, DAMDFeReport.fpDAMDFe, AFile);

    if (AMDFe <> nil) then
      DAMDFeReport.fpMDFe := AMDFe;

    with DAMDFeReport.RLPDFFilter1.DocumentInfo do
    begin
        Title := ACBrStr(Format('DAMDFe EVENTO - MDFe %s', [DAMDFeReport.fpEventoMDFe.RetInfEvento.chMDFe]));
        KeyWords := ACBrStr(Format('Número: %s; Data de emissão: %s; CNPJ: %s',
          [FormatFloat('000,000,000', DAMDFeReport.fpEventoMDFe.RetInfEvento.nSeqEvento),
          FormatDateTime('dd/mm/yyyy', DAMDFeReport.fpEventoMDFe.RetInfEvento.dhRegEvento),
          DAMDFeReport.fpEventoMDFe.InfEvento.CNPJCPF]));
    end;

    DAMDFeReport.RLMDFeEvento.Prepare;
    DAMDFeReport.RLPDFFilter1.FilterPages(DAMDFeReport.RLMDFeEvento.Pages);
  finally
    DAMDFeReport.Free;
  end;
end;

procedure TfrmMDFeDAEventorl.FormDestroy(Sender: TObject);
begin
  RLMDFeEvento.Free;
end;

end.
