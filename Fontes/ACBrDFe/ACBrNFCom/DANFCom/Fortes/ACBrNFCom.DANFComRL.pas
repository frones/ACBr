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

unit ACBrNFCom.DANFComRL;

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
  ACBrNFCom, ACBrNFCom.DANFComRLClass,
  ACBrNFComClass, pcnConversao;

type

  { TfrmDANFcomRL }

  TfrmDANFComRL = class(TForm)
    Datasource1: TDatasource;
    RLNFCom: TRLReport;
    RLPDFFilter1: TRLPDFFilter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure AdicionaInformacaoPDF;
    procedure AjustarEscala;

  protected
    fpACBrNFCom: TACBrNFCom;
    fpNFCom: TNFCom;
    fpDANFCom: TACBrNFComDANFComRL;
    fpSemValorFiscal: boolean;
    fpAuxDiferencaPDF: Integer;
    fpTotalPages: integer;

    FcdsDocumentos: {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};
    procedure ConfigDataSet;

    procedure rllSemValorFiscalPrint(Sender: TObject; var Value: string);
    function GetTextoResumoCanhoto: string;

  public
    class procedure Imprimir(aDANFCom: TACBrNFComDANFComRL; ANFComs: array of TNFCom);
    class procedure SalvarPDF(aDANFCom: TACBrNFComDANFComRL; ANFCom: TNFCom;
      const AFile: string);overload;
    class procedure SalvarPDF(aDANFCom: TACBrNFComDANFComRL; ANFCom: TNFCom;
      AStream: TStream); overload;

  end;

implementation

uses
  MaskUtils, ACBrNFComConversao,
  ACBrDFeUtil, ACBrDFeReportFortes;


{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrmDANFComRL.Imprimir(aDANFCom: TACBrNFComDANFComRL; ANFComs: array of TNFCom);
var
  Report: TRLReport;
  ReportNext: TRLCustomReport;
  i: integer;
  DANFComReport: TfrmDANFComRL;
  ReportArray: array of TfrmDANFComRL;
begin
  if (Length(ANFComs) < 1) then
    exit;

  DANFComReport := nil;

  try
    SetLength(ReportArray, Length(ANFComs));

    for i := 0 to High(ANFComs) do
    begin
      DANFComReport := Create(nil);
      DANFComReport.fpNFCom := ANFComs[i];
      DANFComReport.fpDANFCom := aDANFCom;
      DANFComReport.AjustarEscala;

      DANFComReport.RLNFCom.CompositeOptions.ResetPageNumber := True;
      DANFComReport.fpAuxDiferencaPDF := 0;
      ReportArray[i] := DANFComReport;
    end;

    if Length(ReportArray) = 0 then
      raise Exception.Create('Nenhum relatorio foi inicializado.');

    Report := ReportArray[0].RLNFCom;

    for i := 1 to High(ReportArray) do
    begin
      ReportNext := Report;

      while (ReportNext.NextReport <> nil) do
      begin
        ReportNext := ReportNext.NextReport;
      end;

      ReportNext.NextReport := ReportArray[i].RLNFCom;
    end;

    TDFeReportFortes.AjustarReport(Report, aDANFCom);
    TDFeReportFortes.AjustarMargem(Report, aDANFCom);

    if aDANFCom.MostraPreview then
    begin
      if Assigned(DANFComReport) then
        SelectedFilter := DANFComReport.RLPDFFilter1;

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

class procedure TfrmDANFComRL.SalvarPDF(aDANFCom: TACBrNFComDANFComRL;
  ANFCom: TNFCom; const AFile: string);
var
  DANFComReport: TfrmDANFComRL;
begin
  DANFComReport := Create(nil);

  try
    DANFComReport.fpNFCom := ANFCom;
    DANFComReport.fpDANFCom := aDANFCom;
    DANFComReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DANFComReport.RLNFCom, DANFComReport.fpDANFCom);
    TDFeReportFortes.AjustarMargem(DANFComReport.RLNFCom, DANFComReport.fpDANFCom);
    TDFeReportFortes.AjustarFiltroPDF(DANFComReport.RLPDFFilter1, DANFComReport.fpDANFCom, AFile);

    DANFComReport.AdicionaInformacaoPDF;

    DANFComReport.fpAuxDiferencaPDF := 10;
    DANFComReport.RLNFCom.Prepare;
    DANFComReport.RLPDFFilter1.FilterPages(DANFComReport.RLNFCom.Pages);
  finally
    if DANFComReport <> nil then
      FreeAndNil(DANFComReport);
  end;
end;

class procedure TfrmDANFComRL.SalvarPDF(aDANFCom: TACBrNFComDANFComRL;
  ANFCom: TNFCom; AStream: TStream);
var
  DANFComReport: TfrmDANFComRL;
begin
  DANFComReport := Create(nil);

  try
    DANFComReport.fpNFCom := ANFCom;
    DANFComReport.fpDANFCom := aDANFCom;
    DANFComReport.AjustarEscala;

    TDFeReportFortes.AjustarReport(DANFComReport.RLNFCom, DANFComReport.fpDANFCom);
    DANFComReport.RLPDFFilter1.ShowProgress := DANFComReport.fpDANFCom.MostraStatus;

    DANFComReport.AdicionaInformacaoPDF;

    DANFComReport.fpAuxDiferencaPDF := 10;
    DANFComReport.RLNFCom.Prepare;
    DANFComReport.RLPDFFilter1.FilterPages(DANFComReport.RLNFCom.Pages, AStream);
  finally
    FreeAndNil(DANFComReport);
  end;
end;

procedure TfrmDANFComRL.rllSemValorFiscalPrint(Sender: TObject; var Value: string);
begin
  inherited;

  if fpSemValorFiscal then
    Value := '';
end;

procedure TfrmDANFComRL.FormDestroy(Sender: TObject);
begin
  RLNFCom.Free;
  FreeAndNil(FcdsDocumentos);
end;

procedure TfrmDANFComRL.AdicionaInformacaoPDF;
begin
  with RLPDFFilter1.DocumentInfo do
  begin
    Title := 'DANFCom - Nota Fiscal nº ' +
    FormatFloat('000,000,000', fpNFCom.Ide.nNF);
    KeyWords := 'Número:' + FormatFloat('000,000,000', fpNFCom.Ide.nNF) +
      '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', fpNFCom.Ide.dhEmi) +
      '; Destinatário: ' + fpNFCom.Dest.xNome +
      '; CNPJ: ' + fpNFCom.Dest.CNPJCPF;
  end;
end;

procedure TfrmDANFComRL.AjustarEscala;
begin
  if fpDANFCom.AlterarEscalaPadrao then
  begin
    Scaled := False;
    ScaleBy(fpDANFCom.NovaEscala, Screen.PixelsPerInch);
  end;
end;

procedure TfrmDANFComRL.FormCreate(Sender: TObject);
begin
  {$IfNDef FPC}
  Self.Scaled := False;
  {$EndIf}
  ConfigDataSet;
end;

procedure TfrmDANFComRL.ConfigDataSet;
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

function TfrmDANFComRL.GetTextoResumoCanhoto: string;
begin
  Result := 'EMIT: ' + fpNFCom.Emit.xNome + ' - ' +
    'EMISSÃO: ' + FormatDateTime('DD/MM/YYYY', fpNFCom.Ide.dhEmi) + '  -  DEST.: ';
  Result := Result + fpNFCom.Dest.xNome;
  Result := Result + ' - VALOR A RECEBER: R$ ' + fpDANFCom.FormatarValorUnitario(fpNFCom.Total.vNF);
end;

end.
