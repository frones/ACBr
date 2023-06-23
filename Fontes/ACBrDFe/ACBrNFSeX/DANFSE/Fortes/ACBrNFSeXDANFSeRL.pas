{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Italo Giurizzato Junior                         }
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

unit ACBrNFSeXDANFSeRL;

interface

uses
  SysUtils, Variants, Classes,
  {$IFDEF CLX}
   QGraphics, QControls, QForms,
//   QDialogs,
   QExtCtrls, Qt,
  {$ELSE}
   Graphics, Controls, Forms,
//   Dialogs,
   ExtCtrls, Printers,
  {$ENDIF}
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF}
  ACBrNFSeXClass, ACBrNFSeX, ACBrNFSeXDANFSeRLClass, ACBrDFeReportFortes;

type

  { TfrlXDANFSeRL }

  TfrlXDANFSeRL = class(TForm)
    RLPDFFilter1: TRLPDFFilter;
    RLNFSe: TRLReport;

    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RLNFSeNeedData(Sender: TObject; var MoreData: Boolean);
  private
    { Private declarations }
    FMoreData: Boolean;

    function GetACBrNFSe: TACBrNFSeX;
  protected
    fpDANFSe: TACBrNFSeXDANFSeRL;
    fpNFSe: TNFSe;
    fpSemValorFiscal: Boolean;
    cdsItens: {$IFDEF BORLAND}TClientDataSet{$ELSE}TBufDataset{$ENDIF};

    procedure frlSemValorFiscalPrint(sender: TObject; var Value: String);
    property ACBrNFSe: TACBrNFSeX read GetACBrNFSe;
  public
    { Public declarations }
    class procedure Imprimir(ADANFSe: TACBrNFSeXDANFSeRL; ANotas: array of TNFSe);
    class procedure SalvarPDF(ADANFSe: TACBrNFSeXDANFSeRL; ANFSe: TNFSe; AFile: String); overload;
    class procedure SalvarPDF(ADANFSe: TACBrNFSeXDANFSeRL; ANFSe: TNFSe; AStream: TStream); overload;
    class procedure QuebradeLinha(const sQuebradeLinha: String); virtual;
  end;

var
  frlXDANFSeRL: TfrlXDANFSeRL;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TfrlXDANFSeRL.FormCreate(Sender: TObject);
begin
  FMoreData := True;
end;

procedure TfrlXDANFSeRL.FormDestroy(Sender: TObject);
begin
  FreeAndNil( cdsItens );
end;

procedure TfrlXDANFSeRL.frlSemValorFiscalPrint(sender: TObject;
  var Value: String);
begin
  if fpSemValorFiscal then
    Value := '';
end;

function TfrlXDANFSeRL.GetACBrNFSe: TACBrNFSeX;
begin
  Result := TACBrNFSeX(fpDANFSe.ACBrNFSe);
end;

procedure TfrlXDANFSeRL.RLNFSeNeedData(Sender: TObject; var MoreData: Boolean);
begin
  MoreData := FMoreData;
  FMoreData := False;
end;

class procedure TfrlXDANFSeRL.Imprimir(ADANFSe: TACBrNFSeXDANFSeRL; ANotas: array of TNFSe);
var
  Report: TRLReport;
  ReportNext: TRLCustomReport;
  i: Integer;
  DANFSeReport: TfrlXDANFSeRL;
  ReportArray: array of TfrlXDANFSeRL;
begin
  if (Length(ANotas) < 1) then
    Exit;

  DANFSeReport := nil;
  try
    SetLength(ReportArray, Length(ANotas));

    for i := 0 to High(ANotas) do
    begin
      DANFSeReport := Create(nil);
      DANFSeReport.fpNFSe := ANotas[i];
      DANFSeReport.fpDANFSe := ADANFSe;
      if ADANFSe.AlterarEscalaPadrao then
      begin
        DANFSeReport.Scaled := False;
        DANFSeReport.ScaleBy(ADANFSe.NovaEscala , Screen.PixelsPerInch);
      end;

      DANFSeReport.RLNFSe.CompositeOptions.ResetPageNumber := True;
      ReportArray[i] := DANFSeReport;
    end;

    Report := ReportArray[0].RLNFSe;
    for i := 1 to High(ReportArray) do
    begin
      if (Report.NextReport = nil) then
        Report.NextReport := ReportArray[i].RLNFSe
      else
      begin
        ReportNext := Report.NextReport;

        repeat
          if (ReportNext.NextReport <> nil) then
            ReportNext := ReportNext.NextReport;
        until (ReportNext.NextReport = nil);

        ReportNext.NextReport := ReportArray[i].RLNFSe;
      end;
    end;

    TDFeReportFortes.AjustarReport(Report, ADANFSe);

    if ADANFSe.MostraPreview then
    begin
      if Assigned(DANFSeReport) then
        SelectedFilter := DANFSeReport.RLPDFFilter1;

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

class procedure TfrlXDANFSeRL.QuebradeLinha(const sQuebradeLinha: String);
begin
  // Reescrever
end;

class procedure TfrlXDANFSeRL.SalvarPDF(ADANFSe: TACBrNFSeXDANFSeRL; ANFSe: TNFSe; AFile: String);
var
  DANFSeReport: TfrlXDANFSeRL;
begin
  DANFSeReport := Create(nil);
  try
    DANFSeReport.fpNFSe := ANFSe;
    DANFSeReport.fpDANFSe := ADANFSe;
    if ADANFSe.AlterarEscalaPadrao then
    begin
      DANFSeReport.Scaled := False;
      DANFSeReport.ScaleBy(ADANFSe.NovaEscala , Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANFSeReport.RLNFSe, DANFSeReport.fpDANFSe);
    TDFeReportFortes.AjustarFiltroPDF(DANFSeReport.RLPDFFilter1, DANFSeReport.fpDANFSe, AFile);

    with DANFSeReport.RLPDFFilter1.DocumentInfo do
    begin
      Title := 'NFSe - ' + DANFSeReport.fpNFSe.Numero;
      KeyWords := 'Número:' + DANFSeReport.fpNFSe.Numero +
        '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', DANFSeReport.fpNFSe.DataEmissao) +
        '; Tomador: ' + DANFSeReport.fpNFSe.Tomador.RazaoSocial +
        '; CNPJ: ' + DANFSeReport.fpNFSe.Tomador.IdentificacaoTomador.CpfCnpj +
        '; Valor total: ' + FormatFloat(',0.00', DANFSeReport.fpNFSe.Servico.Valores.ValorServicos);
    end;

    DANFSeReport.RLNFSe.Prepare;
    DANFSeReport.RLPDFFilter1.FilterPages(DANFSeReport.RLNFSe.Pages);
  finally
    FreeAndNil(DANFSeReport);
  end;
end;

class procedure TfrlXDANFSeRL.SalvarPDF(ADANFSe: TACBrNFSeXDANFSeRL; ANFSe: TNFSe; AStream: TStream);
var
  DANFSeReport: TfrlXDANFSeRL;
begin
  DANFSeReport := Create(nil);
  try
    DANFSeReport.fpNFSe := ANFSe;
    DANFSeReport.fpDANFSe := ADANFSe;
    if ADANFSe.AlterarEscalaPadrao then
    begin
      DANFSeReport.Scaled := False;
      DANFSeReport.ScaleBy(ADANFSe.NovaEscala , Screen.PixelsPerInch);
    end;

    TDFeReportFortes.AjustarReport(DANFSeReport.RLNFSe, DANFSeReport.fpDANFSe);
     DANFSeReport.RLPDFFilter1.ShowProgress := DANFSeReport.fpDANFSe.MostraStatus;

    with DANFSeReport.RLPDFFilter1.DocumentInfo do
    begin
      Title := 'NFSe - ' + DANFSeReport.fpNFSe.Numero;
      KeyWords := 'Número:' + DANFSeReport.fpNFSe.Numero +
        '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', DANFSeReport.fpNFSe.DataEmissao) +
        '; Tomador: ' + DANFSeReport.fpNFSe.Tomador.RazaoSocial +
        '; CNPJ: ' + DANFSeReport.fpNFSe.Tomador.IdentificacaoTomador.CpfCnpj +
        '; Valor total: ' + FormatFloat(',0.00', DANFSeReport.fpNFSe.Servico.Valores.ValorServicos);
    end;

    DANFSeReport.RLNFSe.Prepare;
    DANFSeReport.RLPDFFilter1.FilterPages(DANFSeReport.RLNFSe.Pages, AStream);
  finally
    FreeAndNil(DANFSeReport);
  end;
end;

end.
