{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Juliomar Marchetti                              }
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

unit ACBrCTeDAInutRL;


interface

uses
  SysUtils, Variants, Classes, StrUtils,
  {$IFDEF CLX}QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt,
  {$ELSE}Graphics, Controls, Forms, Dialogs, ExtCtrls,{$ENDIF}
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB,
  Printers, RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  pcteCTe, pcnConversao, pcteInutCTe,
  ACBrCTe, ACBrCTeDACTeRLClass, ACBrUtil;

type

  { TfrmCTeDAInutRL }

  TfrmCTeDAInutRL = class(TForm)
    DataSource1: TDataSource;
    RLCTeInut: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    cdsItens:  {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};
    procedure ConfigDataSet;
  protected
    fpInutCTe: TInutCTe;
    fpCTe: TCTe;
    fpDACTe: TACBrCTeDACTeRL;

  public
    class procedure Imprimir(ADACTe: TACBrCTeDACTeRL; AInutCTe: TInutCTe; ACTe: TCTe = nil);
    class procedure SalvarPDF(ADACTe: TACBrCTeDACTeRL; AInutCTe: TInutCTe; AFile: String; ACTe: TCTe = nil);
  end;

implementation

uses
  MaskUtils, ACBrDFeReportFortes;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

class procedure TfrmCTeDAInutRL.Imprimir(ADACTe: TACBrCTeDACTeRL; AInutCTe: TInutCTe; ACTe: TCTe = nil);
var
  DACTeInuReport: TfrmCTeDAInutRL;
begin
  DACTeInuReport := Create(nil);
  try
    DACTeInuReport.fpDACTe := aDACTe;
    DACTeInuReport.fpInutCTe := AInutCTe;
    if aDACTe.AlterarEscalaPadrao then
    begin
      DACTeInuReport.Scaled := False;
      DACTeInuReport.ScaleBy(aDACTe.NovaEscala , Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DACTeInuReport.RLCTeInut, DACTeInuReport.fpDACTe);
    TDFeReportFortes.AjustarMargem(DACTeInuReport.RLCTeInut, DACTeInuReport.fpDACTe);

    if ACTe <> nil then
      DACTeInuReport.fpCTe := ACTe;

    if aDACTe.MostraPreview = True then
      DACTeInuReport.RLCTeInut.PreviewModal
    else
      DACTeInuReport.RLCTeInut.Print;
  finally
     DACTeInuReport.Free;
  end;
end;

class procedure TfrmCTeDAInutRL.SalvarPDF(ADACTe: TACBrCTeDACTeRL; AInutCTe: TInutCTe; AFile: String; ACTe: TCTe = nil);
var
  DACTeInuReport: TfrmCTeDAInutRL;
  i :integer;
begin
  DACTeInuReport := Create(nil);
  try
    DACTeInuReport.fpDACTe := aDACTe;
    DACTeInuReport.fpInutCTe := AInutCTe;
    if aDACTe.AlterarEscalaPadrao then
    begin
      DACTeInuReport.Scaled := False;
      DACTeInuReport.ScaleBy(aDACTe.NovaEscala , Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DACTeInuReport.RLCTeInut, DACTeInuReport.fpDACTe);
    TDFeReportFortes.AjustarMargem(DACTeInuReport.RLCTeInut, DACTeInuReport.fpDACTe);
    TDFeReportFortes.AjustarFiltroPDF(DACTeInuReport.RLPDFFilter1, DACTeInuReport.fpDACTe, AFile);

    for i := 0 to DACTeInuReport.ComponentCount -1 do
    begin
      if (DACTeInuReport.Components[i] is TRLDraw) and (TRLDraw(DACTeInuReport.Components[i]).DrawKind = dkRectangle) then
      begin
        TRLDraw(DACTeInuReport.Components[i]).DrawKind := dkRectangle;
        TRLDraw(DACTeInuReport.Components[i]).Pen.Width := 1;
      end;
    end;

    if ACTe <> nil then
    begin
      DACTeInuReport.fpCTe := ACTe;
      with DACTeInuReport.RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('Inutilização - Conhecimento nº ' +
          FormatFloat('000,000,000', DACTeInuReport.fpCTe.Ide.nCT));
        KeyWords := ACBrStr('Número:' + FormatFloat('000,000,000', DACTeInuReport.fpCTe.Ide.nCT) +
          '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', DACTeInuReport.fpCTe.Ide.dhEmi) +
          '; Destinatário: ' + DACTeInuReport.fpCTe.Dest.xNome +
          '; CNPJ: ' + DACTeInuReport.fpCTe.Dest.CNPJCPF );
      end;
    end;

    DACTeInuReport.RLCTeInut.Prepare;
    DACTeInuReport.RLPDFFilter1.FilterPages(DACTeInuReport.RLCTeInut.Pages);
  finally
     DACTeInuReport.Free;
  end;
end;

procedure TfrmCTeDAInutRL.ConfigDataSet;
begin
 if not Assigned( cdsItens ) then
 cdsItens:=  {$IFDEF BORLAND}  TClientDataSet.create(nil)  {$ELSE}  TBufDataset.create(nil) {$ENDIF};

  if cdsItens.Active then
 begin
 {$IFDEF BORLAND}
  if cdsItens is TClientDataSet then
  TClientDataSet(cdsItens).EmptyDataSet;
 {$ENDIF}
  cdsItens.Active := False;
 end;

 {$IFDEF BORLAND}
 if cdsItens is TClientDataSet then
  begin
  TClientDataSet(cdsItens).StoreDefs := False;
  TClientDataSet(cdsItens).IndexDefs.Clear;
  TClientDataSet(cdsItens).IndexFieldNames := '';
  TClientDataSet(cdsItens).IndexName := '';
  TClientDataSet(cdsItens).Aggregates.Clear;
  TClientDataSet(cdsItens).AggFields.Clear;
  end;
 {$ELSE}
 if cdsItens is TBufDataset then
  begin
  TBufDataset(cdsItens).IndexDefs.Clear;
  TBufDataset(cdsItens).IndexFieldNames:='';
  TBufDataset(cdsItens).IndexName:='';
  end;
 {$ENDIF}

 with cdsItens do
  if FieldCount = 0 then
  begin
    FieldDefs.Clear;
    Fields.Clear;
    FieldDefs.Add('CODIGO',ftString,60);
   {$IFDEF BORLAND}
    if cdsItens is TClientDataSet then
    TClientDataSet(cdsItens).CreateDataSet;
   {$ELSE}
    if cdsItens is TBufDataset then
    TBufDataset(cdsItens).CreateDataSet;
   {$ENDIF}
   end;

 {$IFDEF BORLAND}
  if cdsItens is TClientDataSet then
  TClientDataSet(cdsItens).StoreDefs := False;
 {$ENDIF}

   if not cdsItens.Active then
   cdsItens.Active := True;

  {$IFDEF BORLAND}
   if cdsItens is TClientDataSet then
   if cdsItens.Active then
   TClientDataSet(cdsItens).LogChanges := False;
 {$ENDIF}

 cdsItens.Insert;
 cdsItens.FieldByName('CODIGO').AsString := '1';
 cdsItens.Post;

 DataSource1.dataset := cdsItens;
end;

procedure TfrmCTeDAInutRL.FormCreate(Sender: TObject);
begin
  ConfigDataSet;
end;

procedure TfrmCTeDAInutRL.FormDestroy(Sender: TObject);
begin
  RLCTeInut.Free;
  if Assigned(cdsItens) then
    cdsItens.Free;
end;

end.

