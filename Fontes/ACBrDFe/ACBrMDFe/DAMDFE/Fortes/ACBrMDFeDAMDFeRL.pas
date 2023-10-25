{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAMDFeRL;

interface

uses
  SysUtils, 
  Variants, 
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  ExtCtrls,
  RLReport, 
  RLFilters, 
  RLPrinters, 
  RLPDFFilter, 
  RLConsts,
  pmdfeMDFe, 
  ACBrMDFe, 
  ACBrMDFeDAMDFeRLClass, 
  ACBrDFeReportFortes;

type

  { TfrlDAMDFeRL }

  TfrlDAMDFeRL = class(TForm)
    RLMDFe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
  private
    procedure AdicionaInformacaoPDF;
    procedure AjustarEscala;
  protected
    fpACBrMDFe: TACBrMDFe;
    fpMDFe: TMDFe;
    fpDAMDFe: TACBrMDFeDAMDFeRL;
    fpAfterPreview: boolean;
    fpChangedPos: boolean;
    fpSemValorFiscal: boolean;
    fpAuxDiferencaPDF: Integer;

    procedure rllSemValorFiscalPrint(Sender: TObject; var Value: string);
  public
    { Public declarations }
    class procedure Imprimir(ADAMDFe: TACBrMDFeDAMDFeRL; AMDFes: array of TMDFe);
    class procedure SalvarPDF(ADAMDFe: TACBrMDFeDAMDFeRL; AMDFe: TMDFe; AFile: String); overload;
    class procedure SalvarPDF(ADANFe: TACBrMDFeDAMDFeRL; AMDFe: TMDFe; AStream: TStream); overload;

  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.DateTime;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

procedure TfrlDAMDFeRL.AdicionaInformacaoPDF;
begin
  with RLPDFFilter1.DocumentInfo do
  begin
    Title := ACBrStr('DAMDFe - MDFe nº ') +
        FormatFloat('000,000,000', fpMDFe.Ide.nMDF);
    KeyWords := ACBrStr('Número:') + FormatFloat('000,000,000', fpMDFe.Ide.nMDF) +
        ACBrStr('; Data de emissão: ') + FormatDateTime('dd/mm/yyyy', fpMDFe.Ide.dhEmi) +
        '; CNPJ: ' + fpMDFe.emit.CNPJCPF;
  end;
end;

procedure TfrlDAMDFeRL.AjustarEscala;
begin
  if fpDAMDFe.AlterarEscalaPadrao then
  begin
    Scaled := False;
    ScaleBy(fpDAMDFe.NovaEscala, Screen.PixelsPerInch);
  end;
end;

class procedure TfrlDAMDFeRL.Imprimir(ADAMDFe: TACBrMDFeDAMDFeRL; AMDFes: array of TMDFe);
var
  Report: TRLReport;
  ReportNext: TRLCustomReport;
  i: Integer;
  DAMDFeReport: TfrlDAMDFeRL;
  ReportArray: array of TfrlDAMDFeRL;
begin
  if (Length(AMDFes) < 1) then
    Exit;

  try
    SetLength(ReportArray, Length(AMDFes));

    for i := 0 to High(AMDFes) do
    begin
      DAMDFeReport := Create(nil);
      DAMDFeReport.fpMDFe := AMDFes[i];
      DAMDFeReport.fpDAMDFe := ADAMDFe;
      DAMDFeReport.AjustarEscala;

      DAMDFeReport.RLMDFe.CompositeOptions.ResetPageNumber := True;
      DAMDFeReport.fpAuxDiferencaPDF := 0;
      ReportArray[i] := DAMDFeReport;
    end;

    if Length(ReportArray) = 0 then
      raise Exception.Create('Nenhum relatorio foi inicializado.');

    Report := ReportArray[0].RLMDFe;
    for i := 1 to High(ReportArray) do
    begin
      if (Report.NextReport = nil) then
        Report.NextReport := ReportArray[i].RLMDFe
      else
      begin
        ReportNext := Report.NextReport;

        repeat
          if (ReportNext.NextReport <> nil) then
            ReportNext := ReportNext.NextReport;
        until (ReportNext.NextReport = nil);

        ReportNext.NextReport := ReportArray[i].RLMDFe;
      end;
    end;

    TDFeReportFortes.AjustarReport(Report, ADAMDFe);
    TDFeReportFortes.AjustarMargem(Report, ADAMDFe);

    if ADAMDFe.MostraPreview then
    begin
      if Assigned(DAMDFeReport) then
        SelectedFilter := DAMDFeReport.RLPDFFilter1;
      Report.PreviewModal
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

class procedure TfrlDAMDFeRL.SalvarPDF(ADAMDFe: TACBrMDFeDAMDFeRL; AMDFe: TMDFe; AFile: String);
var
  DAMDFeReport: TfrlDAMDFeRL;
//  ADir: String;
begin
  DAMDFeReport := Create(nil);
  try
    DAMDFeReport.fpMDFe := AMDFe;
    DAMDFeReport.fpDAMDFe := ADAMDFe;

    DAMDFeReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DAMDFeReport.RLMDFe, DAMDFeReport.fpDAMDFe);
    TDFeReportFortes.AjustarFiltroPDF(DAMDFeReport.RLPDFFilter1, DAMDFeReport.fpDAMDFe, AFile);

    DAMDFeReport.AdicionaInformacaoPDF;

    DAMDFeReport.fpAuxDiferencaPDF := 10;
    DAMDFeReport.RLMDFe.Prepare;
    DAMDFeReport.RLPDFFilter1.FilterPages(DAMDFeReport.RLMDFe.Pages);
  finally
    FreeAndNil(DAMDFeReport);
  end;
end;

class procedure TfrlDAMDFeRL.SalvarPDF(ADANFe: TACBrMDFeDAMDFeRL; AMDFe: TMDFe;
  AStream: TStream);
var
  DAMDFeReport: TfrlDAMDFeRL;
begin
  DAMDFeReport := Create(nil);
  try
    DAMDFeReport.fpMDFe := AMDFe;
    DAMDFeReport.fpDAMDFe := ADANFe;
    DAMDFeReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DAMDFeReport.RLMDFe, DAMDFeReport.fpDAMDFe);
    DAMDFeReport.RLPDFFilter1.ShowProgress := DAMDFeReport.fpDAMDFe.MostraStatus;

    DAMDFeReport.AdicionaInformacaoPDF;

    DAMDFeReport.fpAuxDiferencaPDF := 10;
    DAMDFeReport.RLMDFe.Prepare;
    DAMDFeReport.RLPDFFilter1.FilterPages(DAMDFeReport.RLMDFe.Pages, AStream);
  finally
    FreeAndNil(DAMDFeReport);
  end;
end;

procedure TfrlDAMDFeRL.rllSemValorFiscalPrint(Sender: TObject; var Value: string);
begin
  inherited;
  if fpSemValorFiscal then
    Value := '';
end;

end.
