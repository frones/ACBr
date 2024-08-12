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

unit ACBrNF3e.DANF3ERL;

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
  ACBrNF3e, ACBrNF3e.DANF3ERLClass,
  ACBrNF3eClass, pcnConversao;

type

  { TfrmDANF3eRL }

  TfrmDANF3eRL = class(TForm)
    Datasource1: TDatasource;
    RLNF3e: TRLReport;
    RLPDFFilter1: TRLPDFFilter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure AdicionaInformacaoPDF;
    procedure AjustarEscala;

  protected
    fpACBrNF3e: TACBrNF3e;
    fpNF3e: TNF3e;
    fpDANF3e: TACBrNF3eDANF3eRL;
    fpSemValorFiscal: boolean;
    fpAuxDiferencaPDF: Integer;
    fpTotalPages: integer;

    FcdsDocumentos: {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};

    procedure ConfigDataSet;

    procedure rllSemValorFiscalPrint(Sender: TObject; var Value: string);
    function GetTextoResumoCanhoto: string;

  public
    class procedure Imprimir(aDANF3e: TACBrNF3eDANF3eRL; ANF3es: array of TNF3e);
    class procedure SalvarPDF(aDANF3e: TACBrNF3eDANF3eRL; ANF3e: TNF3e; const AFile: string);overload;
    class procedure SalvarPDF(aDANF3e: TACBrNF3eDANF3eRL; ANF3e: TNF3e; AStream: TStream); overload;

  end;

implementation

uses
  MaskUtils, ACBrNF3eConversao,
  ACBrDFeUtil, ACBrDFeReportFortes;


{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrmDANF3eRL.Imprimir(aDANF3e: TACBrNF3eDANF3eRL; ANF3es: array of TNF3e);
var
  Report: TRLReport;
  ReportNext: TRLCustomReport;
  i: integer;
  DANF3eReport: TfrmDANF3eRL;
  ReportArray: array of TfrmDANF3eRL;
begin
  if (Length(ANF3es) < 1) then
    exit;

  DANF3eReport := nil;
  try
    SetLength(ReportArray, Length(ANF3es));

    for i := 0 to High(ANF3es) do
    begin
      DANF3eReport := Create(nil);
      DANF3eReport.fpNF3e := ANF3es[i];
      DANF3eReport.fpDANF3e := aDANF3e;
      DANF3eReport.AjustarEscala;

      DANF3eReport.RLNF3e.CompositeOptions.ResetPageNumber := True;
      DANF3eReport.fpAuxDiferencaPDF := 0;
      ReportArray[i] := DANF3eReport;
    end;

    if Length(ReportArray) = 0 then
      raise Exception.Create('Nenhum relatorio foi inicializado.');

    Report := ReportArray[0].RLNF3e;
    for i := 1 to High(ReportArray) do
    begin
      ReportNext := Report;
      while (ReportNext.NextReport <> nil) do
      begin
        ReportNext := ReportNext.NextReport;
      end;
      ReportNext.NextReport := ReportArray[i].RLNF3e;

    end;

    TDFeReportFortes.AjustarReport(Report, aDANF3e);
    TDFeReportFortes.AjustarMargem(Report, aDANF3e);

    if aDANF3e.MostraPreview then
    begin
      if Assigned(DANF3eReport) then
        SelectedFilter := DANF3eReport.RLPDFFilter1;

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

class procedure TfrmDANF3eRL.SalvarPDF(aDANF3e: TACBrNF3eDANF3eRL; ANF3e: TNF3e;
  const AFile: string);
var
  DANF3eReport: TfrmDANF3eRL;
begin
  DANF3eReport := Create(nil);
  try
    DANF3eReport.fpNF3e := ANF3e;
    DANF3eReport.fpDANF3e := aDANF3e;
    DANF3eReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DANF3eReport.RLNF3e, DANF3eReport.fpDANF3e);
    TDFeReportFortes.AjustarMargem(DANF3eReport.RLNF3e, DANF3eReport.fpDANF3e);
    TDFeReportFortes.AjustarFiltroPDF(DANF3eReport.RLPDFFilter1, DANF3eReport.fpDANF3e, AFile);

    DANF3eReport.AdicionaInformacaoPDF;

    DANF3eReport.fpAuxDiferencaPDF := 10;
    DANF3eReport.RLNF3e.Prepare;
    DANF3eReport.RLPDFFilter1.FilterPages(DANF3eReport.RLNF3e.Pages);
  finally
    if DANF3eReport <> nil then
      FreeAndNil(DANF3eReport);
  end;
end;

class procedure TfrmDANF3eRL.SalvarPDF(aDANF3e: TACBrNF3eDANF3eRL; ANF3e: TNF3e;
  AStream: TStream);
var
  DANF3eReport: TfrmDANF3eRL;
begin
  DANF3eReport := Create(nil);
  try
    DANF3eReport.fpNF3e := ANF3e;
    DANF3eReport.fpDANF3e := aDANF3e;
    DANF3eReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DANF3eReport.RLNF3e, DANF3eReport.fpDANF3e);
    DANF3eReport.RLPDFFilter1.ShowProgress := DANF3eReport.fpDANF3e.MostraStatus;

    DANF3eReport.AdicionaInformacaoPDF;

    DANF3eReport.fpAuxDiferencaPDF := 10;
    DANF3eReport.RLNF3e.Prepare;
    DANF3eReport.RLPDFFilter1.FilterPages(DANF3eReport.RLNF3e.Pages, AStream);
  finally
    FreeAndNil(DANF3eReport);
  end;

end;

procedure TfrmDANF3eRL.rllSemValorFiscalPrint(Sender: TObject; var Value: string);
begin
  inherited;
  if fpSemValorFiscal then
    Value := '';
end;

procedure TfrmDANF3eRL.FormDestroy(Sender: TObject);
begin
  RLNF3e.Free;
  FreeAndNil(FcdsDocumentos);
end;

procedure TfrmDANF3eRL.AdicionaInformacaoPDF;
begin
  with RLPDFFilter1.DocumentInfo do
  begin
    Title := 'DACE - Declaração nº ' +
    FormatFloat('000,000,000', fpNF3e.Ide.nNF);
    KeyWords := 'Número:' + FormatFloat('000,000,000', fpNF3e.Ide.nNF) +
      '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', fpNF3e.Ide.dhEmi) +
      '; Destinatário: ' + fpNF3e.Dest.xNome +
      '; CNPJ: ' + fpNF3e.Dest.CNPJCPF;
  end;
end;

procedure TfrmDANF3eRL.AjustarEscala;
begin
  if fpDANF3e.AlterarEscalaPadrao then
  begin
    Scaled := False;
    ScaleBy(fpDANF3e.NovaEscala, Screen.PixelsPerInch);
  end;
end;

procedure TfrmDANF3eRL.FormCreate(Sender: TObject);
begin
  {$IfNDef FPC}
  Self.Scaled := False;
  {$EndIf}
  ConfigDataSet;
end;

procedure TfrmDANF3eRL.ConfigDataSet;
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

function TfrmDANF3eRL.GetTextoResumoCanhoto: string;
begin
  Result := 'EMIT: ' + fpNF3e.Emit.xNome + ' - ' +
    'EMISSÃO: ' + FormatDateTime('DD/MM/YYYY', fpNF3e.Ide.dhEmi) + '  -  DEST.: ';
  Result := Result + fpNF3e.Dest.xNome;
  Result := Result + ' - VALOR TOTAL: R$ ' + fpDANF3e.FormatarValorUnitario(fpNF3e.Total.vNF);
end;

end.
