{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{******************************************************************************
|* Historico
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAMDFeRL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, pmdfeMDFe, ACBrMDFe, ACBrMDFeUtil,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB;

type

  { TfrlDAMDFeRL }

  TfrlDAMDFeRL = class(TForm)
    dsItens: TDatasource;
    RLMDFe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    FACBrMDFe: TACBrMDFe;
    FMDFe: TMDFe;
    FLogo: string;
    FEmail: string;
    FFax: string;
    FNumCopias: integer;
    FSistema: string;
    FSite: string;
    FUsuario: string;
    AfterPreview: boolean;
    FExpandirLogoMarca: boolean;
    ChangedPos: boolean;
    FSemValorFiscal: boolean;
    FMargemSuperior: double;
    FMargemInferior: double;
    FMargemEsquerda: double;
    FMargemDireita: double;
    FImpressora: string;
    FMDFeCancelada: boolean;
    FMDFeEncerrado: boolean;

    cdsItens:  {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};
    procedure ConfigDataSet;

    procedure rllSemValorFiscalPrint(Sender: TObject; var Value: string);
  public
    { Public declarations }
    class procedure Imprimir(AMDFe: TMDFe; ALogo: string = '';
      AEmail: string = '';
      AExpandirLogoMarca: boolean = False;
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      APreview: boolean = True;
      AMargemSuperior: double = 0.8;
      AMargemInferior: double = 0.8;
      AMargemEsquerda: double = 0.6;
      AMargemDireita: double = 0.51;
      AImpressora: string = '';
      AMDFeCancelada: boolean = False;
      AMDFeEncerrado: boolean = False);

    class procedure SavePDF(AFile: string; AMDFe: TMDFe;
      ALogo: string = '';
      AEmail: string = '';
      AExpandirLogoMarca: boolean = False;
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      AMargemSuperior: double = 0.8;
      AMargemInferior: double = 0.8;
      AMargemEsquerda: double = 0.6;
      AMargemDireita: double = 0.51;
      AMDFeCancelada: boolean = False;
      AMDFeEncerrado: boolean = False);

  end;

implementation

uses
  MaskUtils;

{$R *.dfm}

class procedure TfrlDAMDFeRL.Imprimir(AMDFe: TMDFe;
  ALogo: string = '';
  AEmail: string = '';
  AExpandirLogoMarca: boolean = False;
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  APreview: boolean = True;
  AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8;
  AMargemEsquerda: double = 0.6;
  AMargemDireita: double = 0.51;
  AImpressora: string = '';
  AMDFeCancelada: boolean = False;
  AMDFeEncerrado: boolean = False);
begin
  with Create(nil) do
    //with TfrlDAMDFeRL do
    try
      FMDFe := AMDFe;
      FLogo := ALogo;
      FEmail := AEmail;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FImpressora := AImpressora;
      FMDFeCancelada := AMDFeCancelada;
      FMDFeEncerrado := AMDFeEncerrado;

      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if FNumCopias > 0 then
        RLPrinter.Copies := FNumCopias
      else
        RLPrinter.Copies := 1;

      if APreview = True then
        RLMDFe.PreviewModal
      else
        RLMDFe.Print;
    finally
      Free;
    end;
end;

class procedure TfrlDAMDFeRL.SavePDF(AFile: string;
  AMDFe: TMDFe;
  ALogo: string = '';
  AEmail: string = '';
  AExpandirLogoMarca: boolean = False;
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8;
  AMargemEsquerda: double = 0.6;
  AMargemDireita: double = 0.51;
  AMDFeCancelada: boolean = False;
  AMDFeEncerrado: boolean = False);
var
  i: integer;
begin
  with Create(nil) do
    //with TfrlDAMDFeRL do
    try
      FMDFe := AMDFe;
      FLogo := ALogo;
      FEmail := AEmail;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FMDFeCancelada := AMDFeCancelada;
      FMDFeEncerrado := AMDFeEncerrado;

      with RLPDFFilter1.DocumentInfo do
      begin
        Title := 'DAMDFe - MDFe nº ' +
          FormatFloat('000,000,000', FMDFe.Ide.nMDF);
        KeyWords := 'Número:' + FormatFloat('000,000,000', FMDFe.Ide.nMDF) +
          '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', FMDFe.Ide.dhEmi) +
          '; CNPJ: ' + FMDFe.emit.CNPJ;
      end;

      RLMDFe.SaveToFile(AFile);
    finally
      Free;
    end;
end;

procedure TfrlDAMDFeRL.FormCreate(Sender: TObject);
begin
  ConfigDataSet;
end;

procedure TfrlDAMDFeRL.ConfigDataSet;
begin
  if not Assigned(cdsItens) then
    cdsItens :=
{$IFDEF BORLAND}  TClientDataSet.create(nil)  {$ELSE}
      TBufDataset.Create(nil)
{$ENDIF}
  ;

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
    TBufDataset(cdsItens).IndexFieldNames := '';
    TBufDataset(cdsItens).IndexName := '';
  end;
 {$ENDIF}

  with cdsItens do
    if FieldCount = 0 then
    begin
      FieldDefs.Clear;
      Fields.Clear;
      FieldDefs.Add('CHAVE1', ftString, 84);
      FieldDefs.Add('CHAVE2', ftString, 84);

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

  dsItens.dataset := cdsItens;
end;

procedure TfrlDAMDFeRL.rllSemValorFiscalPrint(Sender: TObject; var Value: string);
begin
  inherited;
  if FSemValorFiscal then
    Value := '';
end;

end.
