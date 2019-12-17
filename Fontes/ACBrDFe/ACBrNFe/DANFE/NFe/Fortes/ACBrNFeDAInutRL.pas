{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - NFe - http://www.nfe.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015 Juliomar Marchetti                     }
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

unit ACBrNFeDAInutRL;


interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt,
  {$ELSE}
      Graphics, Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF}
  pcnNFe, pcnInutNFe, ACBrUtil, ACBrNFeDANFeRLClass, ACBrDFeReportFortes,
  RLReport, RLFilters, RLPDFFilter;

type

  { TfrmNFeDAInutRL }

  TfrmNFeDAInutRL = class(TForm)
    RLNFeInut: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
  protected
    fpInutNFe: TInutNFe;
    fpNFe: TNFe;
    fpDANFe: TACBrNFeDANFeRL;
  public
    class procedure Imprimir(aDANFe: TACBrNFeDANFeRL; AInutNFe: TInutNFe; ANFe: TNFe = nil);
    class procedure SalvarPDF(aDANFe: TACBrNFeDANFeRL; AInutNFe: TInutNFe; const AFile: String = ''; ANFe: TNFe = nil);
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

class procedure TfrmNFeDAInutRL.Imprimir(aDANFe: TACBrNFeDANFeRL; AInutNFe: TInutNFe; ANFe: TNFe = nil);
Var
   DANFeReport: TfrmNFeDAInutRL;
begin
  DANFeReport := Create(nil);
  try
    DANFeReport.fpDANFe := aDANFe;
    DANFeReport.fpInutNFe := AInutNFe;
    TDFeReportFortes.AjustarReport(DANFeReport.RLNFeInut, DANFeReport.fpDANFe);

    if ANFe <> nil then
      DANFeReport.fpNFe := ANFe;

    if aDANFe.MostraPreview then
      DANFeReport.RLNFeInut.PreviewModal
    else
      DANFeReport.RLNFeInut.Print;
  finally
     DANFeReport.Free;
  end;
end;

class procedure TfrmNFeDAInutRL.SalvarPDF(aDANFe: TACBrNFeDANFeRL; AInutNFe: TInutNFe; const AFile: String = ''; ANFe: TNFe = nil);
var
   i :integer;
   DANFeReport: TfrmNFeDAInutRL;
begin
  DANFeReport := Create(nil);
  try
    DANFeReport.fpDANFe := aDANFe;
    DANFeReport.fpInutNFe := AInutNFe;
    TDFeReportFortes.AjustarReport(DANFeReport.RLNFeInut, DANFeReport.fpDANFe);
    TDFeReportFortes.AjustarFiltroPDF(DANFeReport.RLPDFFilter1, DANFeReport.fpDANFe, AFile);

    if ANFe <> nil then
    begin
      DANFeReport.fpNFe := ANFe;

      with DANFeReport.RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('Inutilização - Nota fiscal nº ' +
          FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF));
        KeyWords := ACBrStr('Número:' + FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF) +
          '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', DANFeReport.fpNFe.Ide.dEmi) +
          '; Destinatário: ' + DANFeReport.fpNFe.Dest.xNome +
          '; CNPJ: ' + DANFeReport.fpNFe.Dest.CNPJCPF);
      end;
    end;

    for i := 0 to DANFeReport.ComponentCount -1 do
    begin
      if (DANFeReport.Components[i] is TRLDraw) and (TRLDraw(DANFeReport.Components[i]).DrawKind = dkRectangle) then
      begin
       TRLDraw(DANFeReport.Components[i]).DrawKind := dkRectangle;
       TRLDraw(DANFeReport.Components[i]).Pen.Width := 1;
      end;
    end;

    DANFeReport.RLNFeInut.Prepare;
    DANFeReport.RLPDFFilter1.FilterPages(DANFeReport.RLNFeInut.Pages);
  finally
     DANFeReport.Free;
  end;
end;

end.
