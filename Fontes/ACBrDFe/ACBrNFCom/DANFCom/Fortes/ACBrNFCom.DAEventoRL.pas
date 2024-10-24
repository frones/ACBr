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

unit ACBrNFCom.DAEventoRL;

interface

uses
  SysUtils, Variants, Classes, StrUtils,
  {$IFDEF CLX}QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt,
  {$ELSE}{$IFDEF MSWINDOWS}Windows, Messages, {$ENDIF}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, {$ENDIF}
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts, RLBarcode,
  ACBrNFCom, ACBrNFCom.DANFComRLClass,
  ACBrNFComClass, pcnConversao, ACBrNFComEnvEvento;

type

  { TfrmNFComDAEventoRL }

  TfrmNFComDAEventoRL = class(TForm)
    Datasource1: TDatasource;
    RLNFComEvento: TRLReport;
    RLPDFFilter1: TRLPDFFilter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  protected
    fpACBrNFCom: TACBrNFCom;
    fpDANFCom: TACBrNFComDANFComRL;
    fpNFCom: TNFCom;
    fpEventoNFCom: TInfEventoCollectionItem;

    FcdsDocumentos: {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};

    procedure ConfigDataSet;
  public
    class procedure Imprimir(aDANFCom: TACBrNFComDANFComRL;
                             AEventoNFCom: TInfEventoCollectionItem;
                             ANFCom: TNFCom = nil);
    class procedure SalvarPDF(aDANFCom: TACBrNFComDANFComRL;
                              AEventoNFCom: TInfEventoCollectionItem; const AFile: string;
                              ANFCom: TNFCom = nil); overload;
    class procedure SalvarPDF(ADANFCom: TACBrNFComDANFComRL;
                              FEventoNFCom: TInfEventoCollectionItem; AStream: TStream;
                              ANFCom: TNFCom = nil); overload;
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

class procedure TfrmNFComDAEventoRL.Imprimir(aDANFCom: TACBrNFComDANFComRL; AEventoNFCom: TInfEventoCollectionItem;
  ANFCom: TNFCom= nil);
var
  DANFComEvReport: TfrmNFComDAEventoRL;
begin
  DANFComEvReport := Create(nil);
  try
    DANFComEvReport.fpDANFCom := aDANFCom;
    DANFComEvReport.fpEventoNFCom := AEventoNFCom;

    if aDANFCom.AlterarEscalaPadrao then
    begin
      DANFComEvReport.Scaled := False;
      DANFComEvReport.ScaleBy(aDANFCom.NovaEscala, Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANFComEvReport.RLNFComEvento, DANFComEvReport.fpDANFCom);
    TDFeReportFortes.AjustarMargem(DANFComEvReport.RLNFComEvento, DANFComEvReport.fpDANFCom);

    if ANFCom <> nil then
      DANFComEvReport.fpNFCom := ANFCom;

    if aDANFCom.MostraPreview then
      DANFComEvReport.RLNFComEvento.PreviewModal
    else
      DANFComEvReport.RLNFComEvento.Print;

  finally
    DANFComEvReport.Free;
  end;
end;

class procedure TfrmNFComDAEventoRL.SalvarPDF(aDANFCom: TACBrNFComDANFComRL; AEventoNFCom: TInfEventoCollectionItem;
  const AFile: string; ANFCom: TNFCom = nil);
var
  DANFComEvReport: TfrmNFComDAEventoRL;
begin
  DANFComEvReport := Create(nil);
  try
    DANFComEvReport.fpDANFCom := aDANFCom;
    DANFComEvReport.fpEventoNFCom := AEventoNFCom;

    if aDANFCom.AlterarEscalaPadrao then
    begin
      DANFComEvReport.Scaled := False;
      DANFComEvReport.ScaleBy(aDANFCom.NovaEscala, Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANFComEvReport.RLNFComEvento, DANFComEvReport.fpDANFCom);
    TDFeReportFortes.AjustarMargem(DANFComEvReport.RLNFComEvento, DANFComEvReport.fpDANFCom);
    TDFeReportFortes.AjustarFiltroPDF(DANFComEvReport.RLPDFFilter1, DANFComEvReport.fpDANFCom, AFile);

    if ANFCom <> nil then
    begin
      DANFComEvReport.fpNFCom := ANFCom;

      with DANFComEvReport.RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('DANFCom - Conhecimento nº ') +
          FormatFloat('000,000,000', DANFComEvReport.fpNFCom.Ide.nNF);
        KeyWords := ACBrStr('Número:') + FormatFloat('000,000,000', DANFComEvReport.fpNFCom.Ide.nNF) +
          ACBrStr('; Data de emissão: ') + FormatDateTime('dd/mm/yyyy', DANFComEvReport.fpNFCom.Ide.dhEmi) +
          ACBrStr('; Destinatário: ') + DANFComEvReport.fpNFCom.Dest.xNome +
          '; CNPJ: ' + DANFComEvReport.fpNFCom.Dest.CNPJCPF;
      end;
    end;

    DANFComEvReport.RLNFComEvento.Prepare;
    DANFComEvReport.RLPDFFilter1.FilterPages(DANFComEvReport.RLNFComEvento.Pages);
  finally
    DANFComEvReport.Free;
  end;
end;

class procedure TfrmNFComDAEventoRL.SalvarPDF(ADANFCom: TACBrNFComDANFComRL;
  FEventoNFCom: TInfEventoCollectionItem; AStream: TStream; ANFCom: TNFCom);
var
  DANFComReport: TfrmNFComDAEventoRL;
begin
  DANFComReport := Create(nil);
  try;
    DANFComReport.fpDANFCom := ADANFCom;
    DANFComReport.fpEventoNFCom := FEventoNFCom;

    if ADANFCom.AlterarEscalaPadrao then
    begin
      DANFComReport.Scaled := False;
      DANFComReport.ScaleBy(ADANFCom.NovaEscala, Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANFComReport.RLNFComEvento, DANFComReport.fpDANFCom);
    DANFComReport.RLPDFFilter1.ShowProgress := DANFComReport.fpDANFCom.MostraStatus;

    if (ANFCom <> nil) then
    begin
      DANFComReport.fpNFCom := ANFCom;

      with DANFComReport.RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('Evento - Nota fiscal nº ') +
          FormatFloat('000,000,000', DANFComReport.fpNFCom.Ide.nNF);
        KeyWords := ACBrStr(
          'Número:' + FormatFloat('000,000,000', DANFComReport.fpNFCom.Ide.nNF) +
          '; Data de emissão: ' + FormatDateBr(DANFComReport.fpNFCom.Ide.dhEmi) +
          '; Destinatário: ' + DANFComReport.fpNFCom.Dest.xNome +
          '; CNPJ: ' + DANFComReport.fpNFCom.Dest.CNPJCPF);
      end;
    end;

    DANFComReport.RLNFComEvento.Prepare;
    DANFComReport.RLPDFFilter1.FilterPages(DANFComReport.RLNFComEvento.Pages, AStream);
  finally
    DANFComReport.Free;
  end;
end;

procedure TfrmNFComDAEventoRL.FormDestroy(Sender: TObject);
begin
//  RLPrinter.Free;
  RLNFComEvento.Free;
  FreeAndNil(FcdsDocumentos);
end;

procedure TfrmNFComDAEventoRL.FormCreate(Sender: TObject);
begin
  ConfigDataSet;
end;

procedure TfrmNFComDAEventoRL.ConfigDataSet;
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
