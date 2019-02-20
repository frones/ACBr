{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

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
|* 14/03/2013: Peterson de Cerqueira Matos
|*  - Início da impressão dos eventos em Fortes Report
******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFeEventoRL;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QControls, QForms, QExtCtrls, Qt,
  {$ELSE}
  Controls, Forms, ExtCtrls,
  {$ENDIF}
  pcnEnvEventoNFe, pcnNFe,
  ACBrNFeDANFeRLClass, ACBrDFeReportFortes,
  RLReport, RLPDFFilter, RLFilters;

type

  { TfrlDANFeEventoRL }

  TfrlDANFeEventoRL = class(TForm)
    RLEvento: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
  protected
    fpNFe: TNFe;
    fpDANFe: TACBrNFeDANFeRL;
    fpEventoNFe: TInfEventoCollectionItem;
  public
    { Public declarations }
    class procedure Imprimir(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem; ANFe: TNFe = nil);
    class procedure SalvarPDF(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem; const AFile: String; ANFe: TNFe = nil);
  end;

implementation

uses
  Math,
  ACBrUtil;

{$IfNDef FPC}
  {$R *.dfm}
{$Else}
  {$R *.lfm}
{$EndIf}

class procedure TfrlDANFeEventoRL.Imprimir(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem; ANFe: TNFe = nil);
var
  DANFeReport: TfrlDANFeEventoRL;
begin
  DANFeReport := Create(nil);
  try
    DANFeReport.fpDANFe := ADANFe;
    DANFeReport.fpEventoNFe := FEventoNFe;
    TDFeReportFortes.AjustarReport(DANFeReport.RLEvento, DANFeReport.fpDANFe);

    if (ANFe <> nil) then
      DANFeReport.fpNFe := ANFe;

    if ADANFe.MostraPreview then
      DANFeReport.RLEvento.PreviewModal
    else
      DANFeReport.RLEvento.Print;
  finally
    DANFeReport.Free;
  end;
end;

class procedure TfrlDANFeEventoRL.SalvarPDF(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem; const AFile: String; ANFe: TNFe = nil);
var
  DANFeReport: TfrlDANFeEventoRL;
begin
  DANFeReport := Create(nil);
  try;
    DANFeReport.fpDANFe := ADANFe;
    DANFeReport.fpEventoNFe := FEventoNFe;
    TDFeReportFortes.AjustarReport(DANFeReport.RLEvento, DANFeReport.fpDANFe);
    TDFeReportFortes.AjustarFiltroPDF(DANFeReport.RLPDFFilter1, DANFeReport.fpDANFe, AFile);

    if (ANFe <> nil) then
    begin
      DANFeReport.fpNFe := ANFe;

      with DANFeReport.RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('Evento - Nota fiscal nº ') +
          FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF);
        KeyWords := ACBrStr(
          'Número:' + FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF) +
          '; Data de emissão: ' + FormatDateBr(DANFeReport.fpNFe.Ide.dEmi) +
          '; Destinatário: ' + DANFeReport.fpNFe.Dest.xNome +
          '; CNPJ: ' + DANFeReport.fpNFe.Dest.CNPJCPF +
          '; Valor total: ' + FormatFloatBr(DANFeReport.fpNFe.Total.ICMSTot.vNF));
      end;
    end;

    DANFeReport.RLEvento.Prepare;
    DANFeReport.RLPDFFilter1.FilterPages(DANFeReport.RLEvento.Pages);
  finally
    DANFeReport.Free;
  end;
end;

end.
