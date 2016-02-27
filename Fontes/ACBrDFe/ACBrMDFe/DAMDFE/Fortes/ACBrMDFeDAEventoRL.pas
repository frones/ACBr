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

unit ACBrMDFeDAEventoRL;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, RLReport, RLPDFFilter, pmdfeMDFe, ACBrMDFe,
  RLPrinters, pcnConversao, pmdfeEnvEventoMDFe
  {$IFDEF BORLAND} , DB, DBClient {$ELSE}, BufDataset, DB, RLFilters {$ENDIF} ;

type

  { TfrmMDFeDAEventoRL }

  TfrmMDFeDAEventoRL = class(TForm)
    Datasource1: TDatasource;
    RLMDFeEvento: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormDestroy(Sender: TObject);
  protected
    FACBrMDFe: TACBrMDFe;
    FMDFe: TMDFe;
    FEventoMDFe: TInfEventoCollectionItem;
    FLogo: string;
    FNumCopias: integer;
    FSistema: string;
    FUsuario: string;
    FMostrarPreview: boolean;
    FMargemSuperior: double;
    FMargemInferior: double;
    FMargemEsquerda: double;
    FMargemDireita: double;
    FImpressora: string;

  public
    class procedure Imprimir(AOwner: TComponent;
		  AEventoMDFe: TInfEventoCollectionItem;
      ALogo: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      AUsuario: string = '';
      AMostrarPreview: boolean = True;
      AMargemSuperior: double = 0.7;
      AMargemInferior: double = 0.7;
      AMargemEsquerda: double = 0.7;
      AMargemDireita: double = 0.7;
      AImpressora: string = '';
      AMDFe: TMDFe = nil;
      APrintDialog  : Boolean = True  );

    class procedure SavePDF(AOwner: TComponent;
		  AEventoMDFe: TInfEventoCollectionItem;
      ALogo: string = '';
      AFile: string = '';
      ASistema: string = '';
      AUsuario: string = '';
      AMargemSuperior: double = 0.7;
      AMargemInferior: double = 0.7;
      AMargemEsquerda: double = 0.7;
      AMargemDireita: double = 0.7;
      AMDFe: TMDFe = nil);
  end;

implementation

uses
  MaskUtils, ACBrUtil;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrmMDFeDAEventorl.Imprimir(AOwner: TComponent;
  AEventoMDFe: TInfEventoCollectionItem;
  ALogo: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  AUsuario: string = '';
  AMostrarPreview: boolean = True;
  AMargemSuperior: double = 0.7;
  AMargemInferior: double = 0.7;
  AMargemEsquerda: double = 0.7;
  AMargemDireita: double = 0.7;
  AImpressora: string = '';
  AMDFe: TMDFe = nil;
  APrintDialog  : Boolean = True  );
begin
  with Create ( AOwner ) do
    try
      FEventoMDFe := AEventoMDFe;
      FLogo := ALogo;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FUsuario := AUsuario;
      FMostrarPreview := AMostrarPreview;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FImpressora := AImpressora;


      if AMDFe <> nil then
        FMDFe := AMDFe;

      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if FNumCopias > 0 then
        RLPrinter.Copies := FNumCopias
      else
        RLPrinter.Copies := 1;

      RLMDFeEvento.PrintDialog := APrintDialog;

      if AMostrarPreview then
        RLMDFeEvento.PreviewModal
      else
        RLMDFeEvento.Print;
    finally
      RLMDFeEvento.Free;
      RLMDFeEvento := nil;
      Free;
    end;
end;

class procedure TfrmMDFeDAEventorl.SavePDF(AOwner: TComponent;
  AEventoMDFe: TInfEventoCollectionItem;
  ALogo: string = ''; AFile: string = '';
  ASistema: string = '';
  AUsuario: string = '';
  AMargemSuperior: double = 0.7;
  AMargemInferior: double = 0.7;
  AMargemEsquerda: double = 0.7;
  AMargemDireita: double = 0.7;
  AMDFe: TMDFe = nil);
begin
  with Create(AOwner) do
    //with TfrmMDFeDAEventoRL do
    try
      FEventoMDFe := AEventoMDFe;
      FLogo := ALogo;
      FSistema := ASistema;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;

      if AMDFe <> nil then
        FMDFe := AMDFe;

      with RLPDFFilter1.DocumentInfo do
      begin
        Title :=
          ACBrStr(Format('DAMDFe EVENTO - MDFe %s',[AEventoMDFe.RetInfEvento.chMDFe]));
        KeyWords :=
          ACBrStr(Format('Número: %s; Data de emissão: %s; CNPJ: %s',
            [FormatFloat('000,000,000', AEventoMDFe.RetInfEvento.nSeqEvento),FormatDateTime('dd/mm/yyyy', AEventoMDFe.RetInfEvento.dhRegEvento),
              AEventoMDFe.InfEvento.CNPJ]));
      end;

      RLMDFeEvento.SaveToFile(AFile);
    finally
      Free;
    end;

end;

procedure TfrmMDFeDAEventorl.FormDestroy(Sender: TObject);
begin
  RLMDFeEvento.Free;
end;

end.

