{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Mark dos Santos Gonçalves              }
{                                       Juliomar Marchetti                     }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrCTeDACTeRL;

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
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB,
  RLBarcode, pcteCTe, ACBrCTe, pcnConversao;

type

  { TfrmDACTeRL }

  TfrmDACTeRL = class(TForm)
    Datasource1: TDatasource;
    RLCTe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  protected

    FACBrCTe: TACBrCTe;
    FCTe: TCTe;
    FLogo: string;
    FEmail: string;
    FImprimeHoraSaida: boolean;
    FHoraSaida: string;
    FResumoCanhoto: boolean;
    FFax: string;
    FNumCopias: integer;
    FSistema: string;
    FUrl: string;
    FUsuario: string;
    FMostrarPreview: boolean;
    FMostrarStatus: Boolean;
    FExpandirLogoMarca: boolean;
    ChangedPos: boolean;
    FSemValorFiscal: boolean;
    FMargemSuperior: double;
    FMargemInferior: double;
    FMargemEsquerda: double;
    FMargemDireita: double;
    FImpressora: string;
    FPosRecibo: TPosRecibo;
    FCTeCancelada: boolean;
    FTotalPages: integer;
    FEPECEnviado: boolean;

    cdsDocumentos: {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};
    procedure ConfigDataSet;

    procedure rllSemValorFiscalPrint(Sender: TObject; var Value: string);
    function getTextoResumoCanhoto: string;
  public
    class procedure Imprimir(AOwner: TComponent; ACTe: TCTe; ALogo: string = ''; AEmail: string = '';
      AImprimeHoraSaida: boolean = False; AExpandirLogoMarca: boolean = False; AHoraSaida: string = '';
      AResumoCanhoto: boolean = False; AFax: string = ''; ANumCopias: integer = 1;
      ASistema: string = ''; AUrl: string = ''; AUsuario: string = ''; APreview: boolean = True; AMostrarStatus: Boolean = True;
      AMargemSuperior: double = 0.8; AMargemInferior: double = 0.8; AMargemEsquerda: double = 0.6;
      AMargemDireita: double = 0.51; AImpressora: string = ''; APosRecibo: TPosRecibo = prCabecalho;
      ACTeCancelada: boolean = False; AEPECEnviado: boolean = False; APrintDialog : Boolean = True);

    class procedure SavePDF(AOwner: TComponent; AFile: string; ACTe: TCTe; ALogo: string = ''; AEmail: string = '';
      AImprimeHoraSaida: boolean = False; AExpandirLogoMarca: boolean = False; AHoraSaida: string = '';
      AResumoCanhoto: boolean = False; AFax: string = ''; AMostrarStatus: Boolean = True; ANumCopias: integer = 1;
      ASistema: string = ''; AUrl: string = ''; AUsuario: string = ''; AMargemSuperior: double = 0.8;
      AMargemInferior: double = 0.8; AMargemEsquerda: double = 0.6; AMargemDireita: double = 0.51;
      APosRecibo: TPosRecibo = prCabecalho; ACTeCancelada: boolean = False; AEPECEnviado: boolean = False);

  end;

implementation

uses MaskUtils, ACBrDFeUtil, pcteConversaoCTe, ACBrUtil;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrmDACTeRL.Imprimir(AOwner: TComponent; ACTe: TCTe; ALogo: string = ''; AEmail: string = '';
  AImprimeHoraSaida: boolean = False; AExpandirLogoMarca: boolean = False; AHoraSaida: string = '';
  AResumoCanhoto: boolean = False; AFax: string = ''; ANumCopias: integer = 1; ASistema: string = '';
  AUrl: string = ''; AUsuario: string = ''; APreview: boolean = True; AMostrarStatus: Boolean = True; AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8; AMargemEsquerda: double = 0.6; AMargemDireita: double = 0.51;
  AImpressora: string = ''; APosRecibo: TPosRecibo = prCabecalho; ACTeCancelada: boolean = False;
  AEPECEnviado: boolean = False; APrintDialog: Boolean = True);
begin
  with Create(AOwner) do
    try
      FCTe := ACTe;
      FLogo := ALogo;
      FEmail := AEmail;
      FImprimeHoraSaida := AImprimeHoraSaida;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FHoraSaida := AHoraSaida;
      FResumoCanhoto := AResumoCanhoto;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FUrl := AUrl;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FImpressora := AImpressora;
      FPosRecibo := APosRecibo;
      FCTeCancelada := ACTeCancelada;
      FEPECEnviado := AEPECEnviado;
      FMostrarStatus := AMostrarStatus;

      RLCTe.ShowProgress := FMostrarStatus;
      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if FNumCopias > 0 then
        RLPrinter.Copies := FNumCopias
      else
        RLPrinter.Copies := 1;

      RLCTe.PrintDialog := APrintDialog;
      if APreview = True then
        RLCTe.PreviewModal
      else
        RLCTe.Print;
    finally
      RLCTe.Free;
      RLCTe := nil;
      Free;
    end;
end;

class procedure TfrmDACTeRL.SavePDF(AOwner: TComponent; AFile: string; ACTe: TCTe; ALogo: string = '';
  AEmail: string = ''; AImprimeHoraSaida: boolean = False; AExpandirLogoMarca: boolean = False;
  AHoraSaida: string = ''; AResumoCanhoto: boolean = False; AFax: string = ''; AMostrarStatus: Boolean = True; ANumCopias: integer = 1;
  ASistema: string = ''; AUrl: string = ''; AUsuario: string = ''; AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8; AMargemEsquerda: double = 0.6; AMargemDireita: double = 0.51;
  APosRecibo: TPosRecibo = prCabecalho; ACTeCancelada: boolean = False; AEPECEnviado: boolean = False);
begin
  with Create(AOwner) do
    try
      FCTe := ACTe;
      FLogo := ALogo;
      FEmail := AEmail;
      FImprimeHoraSaida := AImprimeHoraSaida;
      FHoraSaida := AHoraSaida;
      FResumoCanhoto := AResumoCanhoto;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FUrl := AUrl;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FPosRecibo := APosRecibo;
      FCTeCancelada := ACTeCancelada;
      FEPECEnviado := AEPECEnviado;
      FMostrarStatus := AMostrarStatus;

      RLCTe.ShowProgress := FMostrarStatus;

      with RLPDFFilter1.DocumentInfo do
      begin
        Title := 'DACTE - Conhecimento nº ' +
          FormatFloat('000,000,000', FCTe.Ide.nCT);
        KeyWords := 'Número:' + FormatFloat('000,000,000', FCTe.Ide.nCT) +
          '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', FCTe.Ide.dhEmi) +
          '; Destinatário: ' + FCTe.Dest.xNome +
          '; CNPJ: ' + FCTe.Dest.CNPJCPF;
      end;

      RLPDFFilter1.ShowProgress := FMostrarStatus;
      RLPDFFilter1.FileName     := AFile;
      RLCTe.ShowProgress        := FMostrarStatus;
      RLCTe.Prepare;
      RLPDFFilter1.FilterPages(RLCTe.Pages);
    finally
      Free;
    end;
end;

procedure TfrmDACTeRL.rllSemValorFiscalPrint(Sender: TObject; var Value: string);
begin
  inherited;
  if FSemValorFiscal then
    Value := '';
end;

procedure TfrmDACTeRL.FormDestroy(Sender: TObject);
begin
  RLCTe.Free;
  FreeAndNil(cdsDocumentos);
end;

procedure TfrmDACTeRL.FormCreate(Sender: TObject);
begin
  ConfigDataSet;
end;

procedure TfrmDACTeRL.ConfigDataSet;
begin
  if not Assigned(cdsDocumentos) then
    cdsDocumentos :=
{$IFDEF BORLAND}  TClientDataSet.create(nil)  {$ELSE}
      TBufDataset.Create(nil)
{$ENDIF}
  ;

  if cdsDocumentos.Active then
  begin
   {$IFDEF BORLAND}
    if cdsDocumentos is TClientDataSet then
    TClientDataSet(cdsDocumentos).EmptyDataSet;
   {$ENDIF}
    cdsDocumentos.Active := False;
  end;

   {$IFDEF BORLAND}
   if cdsDocumentos is TClientDataSet then
    begin
    TClientDataSet(cdsDocumentos).StoreDefs := False;
    TClientDataSet(cdsDocumentos).IndexDefs.Clear;
    TClientDataSet(cdsDocumentos).IndexFieldNames := '';
    TClientDataSet(cdsDocumentos).IndexName := '';
    TClientDataSet(cdsDocumentos).Aggregates.Clear;
    TClientDataSet(cdsDocumentos).AggFields.Clear;
    end;
   {$ELSE}
  if cdsDocumentos is TBufDataset then
  begin
    TBufDataset(cdsDocumentos).IndexDefs.Clear;
    TBufDataset(cdsDocumentos).IndexFieldNames := '';
    TBufDataset(cdsDocumentos).IndexName := '';
  end;
   {$ENDIF}

  with cdsDocumentos do
    if FieldCount = 0 then
    begin
      FieldDefs.Clear;
      Fields.Clear;
      FieldDefs.Add('TIPO_1', ftString, 15);
      FieldDefs.Add('CNPJCPF_1', ftString, 70);
      FieldDefs.Add('DOCUMENTO_1', ftString, 33);
      FieldDefs.Add('TIPO_2', ftString, 15);
      FieldDefs.Add('CNPJCPF_2', ftString, 70);
      FieldDefs.Add('DOCUMENTO_2', ftString, 33);

     {$IFDEF BORLAND}
      if cdsDocumentos is TClientDataSet then
      TClientDataSet(cdsDocumentos).CreateDataSet;
     {$ELSE}
      if cdsDocumentos is TBufDataset then
        TBufDataset(cdsDocumentos).CreateDataSet;
     {$ENDIF}
    end;

   {$IFDEF BORLAND}
    if cdsDocumentos is TClientDataSet then
    TClientDataSet(cdsDocumentos).StoreDefs := False;
   {$ENDIF}

  if not cdsDocumentos.Active then
    cdsDocumentos.Active := True;

    {$IFDEF BORLAND}
     if cdsDocumentos is TClientDataSet then
     if cdsDocumentos.Active then
     TClientDataSet(cdsDocumentos).LogChanges := False;
   {$ENDIF}

  DataSource1.dataset := cdsDocumentos;
end;

function TfrmDACTeRL.getTextoResumoCanhoto: string;
begin
  Result := 'EMIT: ' + FCTe.Emit.xNome + ' - ' +
    'EMISSÃO: ' + FormatDateTime('DD/MM/YYYY', FCTe.Ide.dhEmi) + '  -  TOMADOR: ';
  if FCTe.Ide.Toma4.xNome = '' then
  begin
    case FCTe.Ide.Toma03.Toma of
      tmRemetente: Result := Result + FCTe.Rem.xNome;
      tmExpedidor: Result := Result + FCTe.Exped.xNome;
      tmRecebedor: Result := Result + FCTe.Receb.xNome;
      tmDestinatario: Result := Result + FCTe.Dest.xNome;
    end;
  end
  else
    Result := Result + FCTe.Ide.Toma4.xNome;
  Result := Result + ' - VALOR A RECEBER: R$ ' + FormatFloatBr(FCTe.vPrest.vRec, '###,###,###,##0.00');
end;

end.
