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

{$I ACBr.inc}
unit ACBrNFeDANFeRL;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, Qt,
  {$ELSE}
  Graphics, Controls, Forms,
  {$ENDIF}
  ACBrNFeDANFeRLClass, ACBrDFeReportFortes,
  pcnNFe, pcnConversao,
  RLReport, RLPDFFilter, RLFilters;

type

  { TfrlDANFeRL }

  TfrlDANFeRL = class(TForm)
    RLNFe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormCreate(Sender: TObject);

  protected
    fpNFe: TNFe;
    fpDANFe: TACBrNFeDANFeRL;
    fpCorDestaqueProdutos: TColor;

    fpLinhasUtilizadas: Integer;
    fpAuxDiferencaPDF: Integer;

    fpQuantItens: Integer;
    fpItemAtual: Integer;

  public
    class procedure Imprimir(ADANFe: TACBrNFeDANFeRL; ANotas: array of TNFe);
    class procedure SalvarPDF(ADANFe: TACBrNFeDANFeRL; ANFe: TNFe; const AFile: String);
  end;

implementation

uses
  ACBrValidador, ACBrUtil, ACBrDFeReport, ACBrDelphiZXingQRCode;

{$IfNDef FPC}
 {$R *.dfm}
{$Else}
 {$R *.lfm}
{$EndIf}

class procedure TfrlDANFeRL.Imprimir(ADANFe: TACBrNFeDANFeRL; ANotas: array of TNFe);
var
  Report: TRLReport;
  ReportNext: TRLCustomReport;
  i: Integer;
  DANFeReport: TfrlDANFeRL;
  ReportArray: array of TfrlDANFeRL;
begin
  if (Length(ANotas) < 1) then
    Exit;

  DANFeReport := nil;
  try
    SetLength(ReportArray, Length(ANotas));

    for i := 0 to High(ANotas) do
    begin
      DANFeReport := Create(nil);
      DANFeReport.fpNFe := ANotas[i];
      DANFeReport.fpDANFe := ADANFe;
      if ADANFe.AlterarEscalaPadrao then
      begin
        DANFeReport.Scaled := False;
        DANFeReport.ScaleBy(ADANFe.NovaEscala , Screen.PixelsPerInch);
      end;

      DANFeReport.RLNFe.CompositeOptions.ResetPageNumber := True;
      DANFeReport.fpAuxDiferencaPDF := 0;
      ReportArray[i] := DANFeReport;
    end;

    Report := ReportArray[0].RLNFe;
    //Associa cada Report com o próximo;
    for i := 1 to High(ReportArray) do
    begin
      ReportNext := Report;
      while (ReportNext.NextReport <> nil) do
      begin
        ReportNext := ReportNext.NextReport;
      end;
      ReportNext.NextReport := ReportArray[i].RLNFe;
    end;

    TDFeReportFortes.AjustarReport(Report, ADANFe);

    if ADANFe.MostraPreview then
    begin
      if Assigned(DANFeReport) then
        SelectedFilter := DANFeReport.RLPDFFilter1;

      Report.PreviewModal;
    end
    else
      Report.Print;
  finally
    if (ReportArray <> nil) then
    begin
      for i := 0 to High(ReportArray) do
        FreeAndNil(ReportArray[i]);

      SetLength(ReportArray, 0);
      Finalize(ReportArray);
      ReportArray := nil;
    end;
  end;
end;

class procedure TfrlDANFeRL.SalvarPDF(ADANFe: TACBrNFeDANFeRL; ANFe: TNFe; const AFile: String);
var
  DANFeReport: TfrlDANFeRL;
begin
  DANFeReport := Create(nil);
  try
    DANFeReport.fpNFe := ANFe;
    DANFeReport.fpDANFe := ADANFe;
    if ADANFe.AlterarEscalaPadrao then
    begin
      DANFeReport.Scaled := False;
      DANFeReport.ScaleBy(ADANFe.NovaEscala , Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANFeReport.RLNFe, DANFeReport.fpDANFe);
    TDFeReportFortes.AjustarFiltroPDF(DANFeReport.RLPDFFilter1, DANFeReport.fpDANFe, AFile);

    with DANFeReport.RLPDFFilter1.DocumentInfo do
    begin
      Title := ACBrStr('DANFE - Nota fiscal nº ') +
        FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF);
      KeyWords := ACBrStr(
        'Número:' + FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF) +
        '; Data de emissão: ' + FormatDateBr(DANFeReport.fpNFe.Ide.dEmi) +
        '; Destinatário: ' + DANFeReport.fpNFe.Dest.xNome +
        '; CNPJ: ' + DANFeReport.fpNFe.Dest.CNPJCPF +
        '; Valor total: ' + FormatFloatBr(DANFeReport.fpNFe.Total.ICMSTot.vNF));
    end;

    DANFeReport.fpAuxDiferencaPDF := 10;
    DANFeReport.RLNFe.Prepare;
    DANFeReport.RLPDFFilter1.FilterPages(DANFeReport.RLNFe.Pages);
  finally
    FreeAndNil(DANFeReport);
  end;
end;

procedure TfrlDANFeRL.FormCreate(Sender: TObject);
begin
  {$IfNDef FPC}
  Self.Scaled := False;
  {$EndIf}
  fpCorDestaqueProdutos := StringToColor('$00E5E5E5');

end;

end.
