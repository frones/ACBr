{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Peterson de Cerqueira Matos                     }
{                              Wemerson Souto                                  }
{                              Daniel Simoes de Almeida                        }
{                              André Ferreira de Moraes                        }
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

unit ACBrNFeDANFeEventoRL;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QControls, QForms, QExtCtrls, Qt,
  {$ELSE}
  Controls, Forms, ExtCtrls,
  {$ENDIF}
  pcnEnvEventoNFe, pcnNFe,
  ACBrNFeDANFeRLClass, ACBrDFeReportFortes,
  RLReport, RLPDFFilter, RLFilters;

type

  { TfrlDANFeEventoRL }

  TfrlDANFeEventoRL = class(TForm)
    RLEvento: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
  protected
    fpNFe: TNFe;
    fpDANFe: TACBrNFeDANFeRL;
    fpEventoNFe: TInfEventoCollectionItem;
  public
    { Public declarations }
    class procedure Imprimir(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem; ANFe: TNFe = nil);
    class procedure SalvarPDF(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem; const AFile: String;
                              ANFe: TNFe = nil); overload;
    class procedure SalvarPDF(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem; AStream: TStream;
                              ANFe: TNFe = nil); overload;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

{$IfNDef FPC}
  {$R *.dfm}
{$Else}
  {$R *.lfm}
{$EndIf}

class procedure TfrlDANFeEventoRL.Imprimir(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem;
                                           ANFe: TNFe = nil);
var
  DANFeReport: TfrlDANFeEventoRL;
begin
  DANFeReport := Create(nil);
  try
    DANFeReport.fpDANFe := ADANFe;
    DANFeReport.fpEventoNFe := FEventoNFe;
    if ADANFe.AlterarEscalaPadrao then
    begin
      DANFeReport.Scaled := False;
      DANFeReport.ScaleBy(ADANFe.NovaEscala , Screen.PixelsPerInch);
    end;
    TDFeReportFortes.AjustarReport(DANFeReport.RLEvento, DANFeReport.fpDANFe);

    if (ANFe <> nil) then
      DANFeReport.fpNFe := ANFe;

    if ADANFe.MostraPreview then 
    begin
      if Assigned(DANFeReport) then
        SelectedFilter := DANFeReport.RLPDFFilter1;
      if Assigned(DANFeReport.fpNFe) then
      begin
        DANFeReport.RLEvento.Title := OnlyNumber(DANFeReport.fpNFe.InfNFe.Id);
      end;
      DANFeReport.RLEvento.PreviewModal
    end
    else
      DANFeReport.RLEvento.Print;
  finally
    DANFeReport.Free;
  end;
end;

class procedure TfrlDANFeEventoRL.SalvarPDF(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem;
                                            const AFile: String; ANFe: TNFe = nil);
var
  DANFeReport: TfrlDANFeEventoRL;
  LNNF : string;
begin
  DANFeReport := Create(nil);
  try;
    DANFeReport.fpDANFe := ADANFe;
    DANFeReport.fpEventoNFe := FEventoNFe;
    if ADANFe.AlterarEscalaPadrao then
    begin
      DANFeReport.Scaled := False;
      DANFeReport.ScaleBy(ADANFe.NovaEscala , Screen.PixelsPerInch);
    end;
    TDFeReportFortes.AjustarReport(DANFeReport.RLEvento, DANFeReport.fpDANFe);
    TDFeReportFortes.AjustarFiltroPDF(DANFeReport.RLPDFFilter1, DANFeReport.fpDANFe, AFile);

    if (ANFe <> nil) then
    begin
      DANFeReport.fpNFe := ANFe;

      with DANFeReport.RLPDFFilter1.DocumentInfo do
      begin
        if DANFeReport.fpDANFe.FormatarNumeroDocumento then
          LNNF := FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF)
        else
          IntToStr(DANFeReport.fpNFe.Ide.nNF);

        Title := ACBrStr('Evento - Nota fiscal nº ') + LNNF;
        KeyWords := ACBrStr(
          'Número:' + LNNF +
          '; Data de emissão: ' + FormatDateBr(DANFeReport.fpNFe.Ide.dEmi) +
          '; Destinatário: ' + DANFeReport.fpNFe.Dest.xNome +
          '; CNPJ: ' + DANFeReport.fpNFe.Dest.CNPJCPF +
          '; Valor total: ' + FormatFloatBr(DANFeReport.fpNFe.Total.ICMSTot.vNF));
      end;
    end;

    DANFeReport.RLEvento.Prepare;
    DANFeReport.RLPDFFilter1.FilterPages(DANFeReport.RLEvento.Pages);
  finally
    DANFeReport.Free;
  end;
end;

class procedure TfrlDANFeEventoRL.SalvarPDF(ADANFe: TACBrNFeDANFeRL; FEventoNFe: TInfEventoCollectionItem;
                                            AStream: TStream; ANFe: TNFe = nil);
var
  DANFeReport: TfrlDANFeEventoRL;
  LNNF : string;
begin
  DANFeReport := Create(nil);
  try;
    DANFeReport.fpDANFe := ADANFe;
    DANFeReport.fpEventoNFe := FEventoNFe;

    if ADANFe.AlterarEscalaPadrao then
    begin
      DANFeReport.Scaled := False;
      DANFeReport.ScaleBy(ADANFe.NovaEscala , Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANFeReport.RLEvento, DANFeReport.fpDANFe);
    DANFeReport.RLPDFFilter1.ShowProgress := DANFeReport.fpDANFe.MostraStatus;

    if (ANFe <> nil) then
    begin
      DANFeReport.fpNFe := ANFe;

      with DANFeReport.RLPDFFilter1.DocumentInfo do
      begin
        if DANFeReport.fpDANFe.FormatarNumeroDocumento then
          LNNF := FormatFloat('000,000,000', DANFeReport.fpNFe.Ide.nNF)
        else
          LNNF := IntToStr(DANFeReport.fpNFe.Ide.nNF);

        Title := ACBrStr('Evento - Nota fiscal nº ') + LNNF;
        KeyWords := ACBrStr(
          'Número:' + LNNF +
          '; Data de emissão: ' + FormatDateBr(DANFeReport.fpNFe.Ide.dEmi) +
          '; Destinatário: ' + DANFeReport.fpNFe.Dest.xNome +
          '; CNPJ: ' + DANFeReport.fpNFe.Dest.CNPJCPF +
          '; Valor total: ' + FormatFloatBr(DANFeReport.fpNFe.Total.ICMSTot.vNF));
      end;
    end;

    DANFeReport.RLEvento.Prepare;
    DANFeReport.RLPDFFilter1.FilterPages(DANFeReport.RLEvento.Pages, AStream);
  finally
    DANFeReport.Free;
  end;
end;

end.
