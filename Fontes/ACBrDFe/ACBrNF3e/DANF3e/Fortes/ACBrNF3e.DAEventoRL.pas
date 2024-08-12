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

unit ACBrNF3e.DAEventoRL;

interface

uses
  SysUtils, Variants, Classes, StrUtils,
  {$IFDEF CLX}QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt,
  {$ELSE}{$IFDEF MSWINDOWS}Windows, Messages, {$ENDIF}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, {$ENDIF}
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts, RLBarcode,
  ACBrNF3e, ACBrNF3e.DANF3ERLClass,
  ACBrNF3eClass, pcnConversao, ACBrNF3eEnvEvento;

type

  { TfrmNF3eDAEventoRL }

  TfrmNF3eDAEventoRL = class(TForm)
    Datasource1: TDatasource;
    RLNF3eEvento: TRLReport;
    RLPDFFilter1: TRLPDFFilter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  protected
    fpACBrNF3e: TACBrNF3e;
    fpDANF3e: TACBrNF3eDANF3eRL;
    fpNF3e: TNF3e;
    fpEventoNF3e: TInfEventoCollectionItem;

    FcdsDocumentos: {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};

    procedure ConfigDataSet;
  public
    class procedure Imprimir(aDANF3e: TACBrNF3eDANF3eRL;
                             AEventoNF3e: TInfEventoCollectionItem;
                             ANF3e: TNF3e = nil);
    class procedure SalvarPDF(aDANF3e: TACBrNF3eDANF3eRL;
                              AEventoNF3e: TInfEventoCollectionItem; const AFile: string;
                              ANF3e: TNF3e = nil); overload;
    class procedure SalvarPDF(ADANF3e: TACBrNF3eDANF3eRL;
                              FEventoNF3e: TInfEventoCollectionItem; AStream: TStream;
                              ANF3e: TNF3e = nil); overload;
  end;

implementation

uses
  MaskUtils,
  ACBrUtil.Strings, ACBrUtil.Base, ACBrUtil.DateTime,
  ACBrDFeReportFortes;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

class procedure TfrmNF3eDAEventoRL.Imprimir(aDANF3e: TACBrNF3eDANF3eRL; AEventoNF3e: TInfEventoCollectionItem;
  ANF3e: TNF3e= nil);
var
  DANF3eEvReport: TfrmNF3eDAEventoRL;
begin
  DANF3eEvReport := Create(nil);
  try
    DANF3eEvReport.fpDANF3e := aDANF3e;
    DANF3eEvReport.fpEventoNF3e := AEventoNF3e;

    if aDANF3e.AlterarEscalaPadrao then
    begin
      DANF3eEvReport.Scaled := False;
      DANF3eEvReport.ScaleBy(aDANF3e.NovaEscala, Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANF3eEvReport.RLNF3eEvento, DANF3eEvReport.fpDANF3e);
    TDFeReportFortes.AjustarMargem(DANF3eEvReport.RLNF3eEvento, DANF3eEvReport.fpDANF3e);

    if ANF3e <> nil then
      DANF3eEvReport.fpNF3e := ANF3e;

    if aDANF3e.MostraPreview then
      DANF3eEvReport.RLNF3eEvento.PreviewModal
    else
      DANF3eEvReport.RLNF3eEvento.Print;

  finally
    DANF3eEvReport.Free;
  end;
end;

class procedure TfrmNF3eDAEventoRL.SalvarPDF(aDANF3e: TACBrNF3eDANF3eRL; AEventoNF3e: TInfEventoCollectionItem;
  const AFile: string; ANF3e: TNF3e = nil);
var
  DANF3eEvReport: TfrmNF3eDAEventoRL;
begin
  DANF3eEvReport := Create(nil);
  try
    DANF3eEvReport.fpDANF3e := aDANF3e;
    DANF3eEvReport.fpEventoNF3e := AEventoNF3e;

    if aDANF3e.AlterarEscalaPadrao then
    begin
      DANF3eEvReport.Scaled := False;
      DANF3eEvReport.ScaleBy(aDANF3e.NovaEscala, Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANF3eEvReport.RLNF3eEvento, DANF3eEvReport.fpDANF3e);
    TDFeReportFortes.AjustarMargem(DANF3eEvReport.RLNF3eEvento, DANF3eEvReport.fpDANF3e);
    TDFeReportFortes.AjustarFiltroPDF(DANF3eEvReport.RLPDFFilter1, DANF3eEvReport.fpDANF3e, AFile);

    if ANF3e <> nil then
    begin
      DANF3eEvReport.fpNF3e := ANF3e;

      with DANF3eEvReport.RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('DAEvento - Declaração nº ') +
          FormatFloat('000,000,000', DANF3eEvReport.fpNF3e.Ide.nNF);
        KeyWords := ACBrStr('Número:') + FormatFloat('000,000,000', DANF3eEvReport.fpNF3e.Ide.nNF) +
          ACBrStr('; Data de emissão: ') + FormatDateTime('dd/mm/yyyy', DANF3eEvReport.fpNF3e.Ide.dhEmi) +
          ACBrStr('; Destinatário: ') + DANF3eEvReport.fpNF3e.Dest.xNome +
          '; CNPJ: ' + DANF3eEvReport.fpNF3e.Dest.CNPJCPF;
      end;
    end;

    DANF3eEvReport.RLNF3eEvento.Prepare;
    DANF3eEvReport.RLPDFFilter1.FilterPages(DANF3eEvReport.RLNF3eEvento.Pages);
  finally
    DANF3eEvReport.Free;
  end;
end;

class procedure TfrmNF3eDAEventoRL.SalvarPDF(ADANF3e: TACBrNF3eDANF3eRL;
  FEventoNF3e: TInfEventoCollectionItem; AStream: TStream; ANF3e: TNF3e);
var
  DANF3eReport: TfrmNF3eDAEventoRL;
begin
  DANF3eReport := Create(nil);
  try;
    DANF3eReport.fpDANF3e := ADANF3e;
    DANF3eReport.fpEventoNF3e := FEventoNF3e;

    if ADANF3e.AlterarEscalaPadrao then
    begin
      DANF3eReport.Scaled := False;
      DANF3eReport.ScaleBy(ADANF3e.NovaEscala, Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANF3eReport.RLNF3eEvento, DANF3eReport.fpDANF3e);
    DANF3eReport.RLPDFFilter1.ShowProgress := DANF3eReport.fpDANF3e.MostraStatus;

    if (ANF3e <> nil) then
    begin
      DANF3eReport.fpNF3e := ANF3e;

      with DANF3eReport.RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('DAEvento - Declaração nº ') +
          FormatFloat('000,000,000', DANF3eReport.fpNF3e.Ide.nNF);
        KeyWords := ACBrStr(
          'Número:' + FormatFloat('000,000,000', DANF3eReport.fpNF3e.Ide.nNF) +
          '; Data de emissão: ' + FormatDateBr(DANF3eReport.fpNF3e.Ide.dhEmi) +
          '; Destinatário: ' + DANF3eReport.fpNF3e.Dest.xNome +
          '; CNPJ: ' + DANF3eReport.fpNF3e.Dest.CNPJCPF);
      end;
    end;

    DANF3eReport.RLNF3eEvento.Prepare;
    DANF3eReport.RLPDFFilter1.FilterPages(DANF3eReport.RLNF3eEvento.Pages, AStream);
  finally
    DANF3eReport.Free;
  end;
end;

procedure TfrmNF3eDAEventoRL.FormDestroy(Sender: TObject);
begin
//  RLPrinter.Free;
  RLNF3eEvento.Free;
  FreeAndNil(FcdsDocumentos);
end;

procedure TfrmNF3eDAEventoRL.FormCreate(Sender: TObject);
begin
  ConfigDataSet;
end;

procedure TfrmNF3eDAEventoRL.ConfigDataSet;
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
      FieldDefs.Add('TIPO_1', ftString, 14);
      FieldDefs.Add('CNPJCPF_1', ftString, 70);
      FieldDefs.Add('DOCUMENTO_1', ftString, 33);
      FieldDefs.Add('TIPO_2', ftString, 14);
      FieldDefs.Add('CNPJCPF_2', ftString, 70);
      FieldDefs.Add('DOCUMENTO_2', ftString, 33);

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

end.
