{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDCe.DACERL;

interface

{$H+}

uses
  SysUtils, Variants, Classes, StrUtils,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt,
  {$ELSE}
  {$IFDEF MSWINDOWS}Windows, Messages, {$ENDIF}
  Graphics, Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF}
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts, RLBarcode,
  ACBrDCe, ACBrDCe.DACERLClass,
  ACBrDCe.Classes, pcnConversao;

type

  { TfrmDADCeRL }

  TfrmDADCeRL = class(TForm)
    Datasource1: TDatasource;
    RLDCe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure AdicionaInformacaoPDF;
    procedure AjustarEscala;

  protected
    fpACBrDCe: TACBrDCe;
    fpDCe: TDCe;
    fpDADCe: TACBrDCeDACERL;
    fpSemValorFiscal: boolean;
    fpAuxDiferencaPDF: Integer;
    fpTotalPages: integer;

    FcdsDocumentos: {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};

    procedure ConfigDataSet;

    procedure rllSemValorFiscalPrint(Sender: TObject; var Value: string);
    function GetTextoResumoCanhoto: string;

  public
    class procedure Imprimir(aDADCe: TACBrDCeDACERL; ADCes: array of TDCe);
    class procedure SalvarPDF(aDADCe: TACBrDCeDACERL; ADCe: TDCe; const AFile: string);overload;
    class procedure SalvarPDF(aDADCe: TACBrDCeDACERL; ADCe: TDCe; AStream: TStream); overload;

  end;

implementation

uses
  MaskUtils, ACBrDCe.Conversao,
  ACBrDFeUtil, ACBrDFeReportFortes;


{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrmDADCeRL.Imprimir(aDADCe: TACBrDCeDACERL; ADCes: array of TDCe);
var
  Report: TRLReport;
  ReportNext: TRLCustomReport;
  i: integer;
  DADCeReport: TfrmDADCeRL;
  ReportArray: array of TfrmDADCeRL;
begin
  if (Length(ADCes) < 1) then
    exit;

  DADCeReport := nil;
  try
    SetLength(ReportArray, Length(ADCes));

    for i := 0 to High(ADCes) do
    begin
      DADCeReport := Create(nil);
      DADCeReport.fpDCe := ADCes[i];
      DADCeReport.fpDADCe := aDADCe;
      DADCeReport.AjustarEscala;

      DADCeReport.RLDCe.CompositeOptions.ResetPageNumber := True;
      DADCeReport.fpAuxDiferencaPDF := 0;
      ReportArray[i] := DADCeReport;
    end;

    if Length(ReportArray) = 0 then
      raise Exception.Create('Nenhum relatorio foi inicializado.');

    Report := ReportArray[0].RLDCe;
    for i := 1 to High(ReportArray) do
    begin
      ReportNext := Report;
      while (ReportNext.NextReport <> nil) do
      begin
        ReportNext := ReportNext.NextReport;
      end;
      ReportNext.NextReport := ReportArray[i].RLDCe;

    end;

    TDFeReportFortes.AjustarReport(Report, aDADCe);
    TDFeReportFortes.AjustarMargem(Report, aDADCe);

    if aDADCe.MostraPreview then
    begin
      if Assigned(DADCeReport) then
        SelectedFilter := DADCeReport.RLPDFFilter1;

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

class procedure TfrmDADCeRL.SalvarPDF(aDADCe: TACBrDCeDACERL; ADCe: TDCe;
  const AFile: string);
var
  DADCeReport: TfrmDADCeRL;
begin
  DADCeReport := Create(nil);
  try
    DADCeReport.fpDCe := ADCe;
    DADCeReport.fpDADCe := aDADCe;
    DADCeReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DADCeReport.RLDCe, DADCeReport.fpDADCe);
    TDFeReportFortes.AjustarMargem(DADCeReport.RLDCe, DADCeReport.fpDADCe);
    TDFeReportFortes.AjustarFiltroPDF(DADCeReport.RLPDFFilter1, DADCeReport.fpDADCe, AFile);

    DADCeReport.AdicionaInformacaoPDF;

    DADCeReport.fpAuxDiferencaPDF := 10;
    DADCeReport.RLDCe.Prepare;
    DADCeReport.RLPDFFilter1.FilterPages(DADCeReport.RLDCe.Pages);
  finally
    if DADCeReport <> nil then
        FreeAndNil(DADCeReport);
  end;
end;

class procedure TfrmDADCeRL.SalvarPDF(aDADCe: TACBrDCeDACERL; ADCe: TDCe;
  AStream: TStream);
var
  DADCeReport: TfrmDADCeRL;
begin
  DADCeReport := Create(nil);
  try
    DADCeReport.fpDCe := ADCe;
    DADCeReport.fpDADCe := aDADCe;
    DADCeReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DADCeReport.RLDCe, DADCeReport.fpDADCe);
    DADCeReport.RLPDFFilter1.ShowProgress := DADCeReport.fpDADCe.MostraStatus;

    DADCeReport.AdicionaInformacaoPDF;

    DADCeReport.fpAuxDiferencaPDF := 10;
    DADCeReport.RLDCe.Prepare;
    DADCeReport.RLPDFFilter1.FilterPages(DADCeReport.RLDCe.Pages, AStream);
  finally
    FreeAndNil(DADCeReport);
  end;

end;

procedure TfrmDADCeRL.rllSemValorFiscalPrint(Sender: TObject; var Value: string);
begin
  inherited;
  if fpSemValorFiscal then
    Value := '';
end;

procedure TfrmDADCeRL.FormDestroy(Sender: TObject);
begin
  RLDCe.Free;
  FreeAndNil(FcdsDocumentos);
end;

procedure TfrmDADCeRL.AdicionaInformacaoPDF;
begin
  with RLPDFFilter1.DocumentInfo do
  begin
    Title := 'DACE - Declaração nº ' +
    FormatFloat('000,000,000', fpDCe.Ide.nDC);
    KeyWords := 'Número:' + FormatFloat('000,000,000', fpDCe.Ide.nDC) +
      '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', fpDCe.Ide.dhEmi) +
      '; Destinatário: ' + fpDCe.Dest.xNome +
      '; CNPJ: ' + fpDCe.Dest.CNPJCPF;
  end;
end;

procedure TfrmDADCeRL.AjustarEscala;
begin
  if fpDADCe.AlterarEscalaPadrao then
  begin
    Scaled := False;
    ScaleBy(fpDADCe.NovaEscala, Screen.PixelsPerInch);
  end;
end;

procedure TfrmDADCeRL.FormCreate(Sender: TObject);
begin
  {$IfNDef FPC}
  Self.Scaled := False;
  {$EndIf}
  ConfigDataSet;
end;

procedure TfrmDADCeRL.ConfigDataSet;
begin
  if not Assigned(FcdsDocumentos) then
    FcdsDocumentos :=
{$IFDEF BORLAND}  TClientDataSet.create(nil)  {$ELSE}
      TBufDataset.Create(nil)
{$ENDIF}
  ;

  if FcdsDocumentos.Active then
  begin
   {$IFDEF BORLAND}
    if FcdsDocumentos is TClientDataSet then
    TClientDataSet(FcdsDocumentos).EmptyDataSet;
   {$ENDIF}
    FcdsDocumentos.Active := False;
  end;

   {$IFDEF BORLAND}
   if FcdsDocumentos is TClientDataSet then
    begin
    TClientDataSet(FcdsDocumentos).StoreDefs := False;
    TClientDataSet(FcdsDocumentos).IndexDefs.Clear;
    TClientDataSet(FcdsDocumentos).IndexFieldNames := '';
    TClientDataSet(FcdsDocumentos).IndexName := '';
    TClientDataSet(FcdsDocumentos).Aggregates.Clear;
    TClientDataSet(FcdsDocumentos).AggFields.Clear;
    end;
   {$ELSE}
  if FcdsDocumentos is TBufDataset then
  begin
    TBufDataset(FcdsDocumentos).IndexDefs.Clear;
    TBufDataset(FcdsDocumentos).IndexFieldNames := '';
    TBufDataset(FcdsDocumentos).IndexName := '';
  end;
   {$ENDIF}

  with FcdsDocumentos do
    if FieldCount = 0 then
    begin
      FieldDefs.Clear;
      Fields.Clear;
      FieldDefs.Add('TIPO_1', ftString, 15);
      FieldDefs.Add('CNPJCPF_1', ftString, 70);
      FieldDefs.Add('DOCUMENTO_1', ftString, 43);
      FieldDefs.Add('TIPO_2', ftString, 15);
      FieldDefs.Add('CNPJCPF_2', ftString, 70);
      FieldDefs.Add('DOCUMENTO_2', ftString, 43);

     {$IFDEF BORLAND}
      if FcdsDocumentos is TClientDataSet then
      TClientDataSet(FcdsDocumentos).CreateDataSet;
     {$ELSE}
      if FcdsDocumentos is TBufDataset then
        TBufDataset(FcdsDocumentos).CreateDataSet;
     {$ENDIF}
    end;

   {$IFDEF BORLAND}
    if FcdsDocumentos is TClientDataSet then
    TClientDataSet(FcdsDocumentos).StoreDefs := False;
   {$ENDIF}

  if not FcdsDocumentos.Active then
    FcdsDocumentos.Active := True;

    {$IFDEF BORLAND}
     if FcdsDocumentos is TClientDataSet then
     if FcdsDocumentos.Active then
     TClientDataSet(FcdsDocumentos).LogChanges := False;
   {$ENDIF}

  DataSource1.dataset := FcdsDocumentos;
end;

function TfrmDADCeRL.GetTextoResumoCanhoto: string;
begin
  Result := 'EMIT: ' + fpDCe.Emit.xNome + ' - ' +
    'EMISSÃO: ' + FormatDateTime('DD/MM/YYYY', fpDCe.Ide.dhEmi) + '  -  DEST.: ';
  Result := Result + fpDCe.Dest.xNome;
  Result := Result + ' - VALOR TOTAL: R$ ' + fpDADCe.FormatarValorUnitario(fpDCe.Total.vDC);
end;

end.
