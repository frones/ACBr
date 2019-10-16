{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
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
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBrGNREGuiaFR;

interface

uses
  Forms, SysUtils, Classes, Graphics, ACBrGNREGuiaClass, ACBrGNREGuiaFRDM,
  pcnConversao, frxClass, pgnreGNRERetorno;

type
  EACBrGNREGuiaFR = class(Exception);
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrGNREGuiaFR = class(TACBrGNREGuiaClass)
  private
    FdmGuia        : TdmACBrGNREFR;
    FFastFile      : String;
    FEspessuraBorda: Integer;
    FShowDialog    : boolean;
    function GetPreparedReport: TfrxReport;
    function PrepareReport(GNRE: TGNRERetorno = nil): boolean;
    procedure AjustaMargensReports;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirGuia(GNRE: TGNRERetorno = nil); override;
    procedure ImprimirGuiaPDF(GNRE: TGNRERetorno = nil); override;
  published
    property FastFile      : String read FFastFile write FFastFile;
    property dmGuia        : TdmACBrGNREFR read FdmGuia write FdmGuia;
    property EspessuraBorda: Integer read FEspessuraBorda write FEspessuraBorda;
    property PreparedReport: TfrxReport read GetPreparedReport;
    property ShowDialog    : boolean read FShowDialog write FShowDialog default true;
  end;

implementation

uses ACBrGNRE2, ACBrUtil, StrUtils, Dialogs, ACBrGNREGuiasRetorno;

constructor TACBrGNREGuiaFR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FdmGuia         := TdmACBrGNREFR.Create(Self);
  FFastFile       := '';
  FEspessuraBorda := 1;
end;

destructor TACBrGNREGuiaFR.Destroy;
begin
  dmGuia.Free;
  inherited Destroy;
end;

function TACBrGNREGuiaFR.GetPreparedReport: TfrxReport;
begin
  if Trim(FFastFile) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := dmGuia.frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrGNREGuiaFR.ImprimirGuia(GNRE: TGNRERetorno);
begin
  if PrepareReport(GNRE) then
  begin
    if MostrarPreview then
      dmGuia.frxReport.ShowPreparedReport
    else
      dmGuia.frxReport.Print;
  end;
end;

procedure TACBrGNREGuiaFR.ImprimirGuiaPDF(GNRE: TGNRERetorno);
const
  TITULO_PDF = 'Guia Nacional de Recolhimento de Tributos Estaduais - GNRE';
var
  i: Integer;
  OldShowDialog: Boolean;
begin
  if PrepareReport(GNRE) then
  begin
    dmGuia.frxPDFExport.Author     := Sistema;
    dmGuia.frxPDFExport.Creator    := Sistema;
    dmGuia.frxPDFExport.Producer   := Sistema;
    dmGuia.frxPDFExport.Title      := TITULO_PDF;
    dmGuia.frxPDFExport.Subject    := TITULO_PDF;
    dmGuia.frxPDFExport.Keywords   := TITULO_PDF;
    OldShowDialog := dmGuia.frxPDFExport.ShowDialog;
    try
      dmGuia.frxPDFExport.ShowDialog := False;
      for i := 0 to TACBrGNRE(ACBrGNRE).GuiasRetorno.Count - 1 do
      begin
        dmGuia.frxPDFExport.FileName := IncludeTrailingPathDelimiter(PathPDF) + 'GNRE_' + dmGuia.GNRE.RepresentacaoNumerica + '.pdf';
        dmGuia.frxReport.Export(dmGuia.frxPDFExport);
      end;
    finally
      dmGuia.frxPDFExport.ShowDialog := OldShowDialog;
    end;
  end;
end;

procedure TACBrGNREGuiaFR.AjustaMargensReports;
var
  Page: TfrxReportPage;
  I: Integer;
begin
  for I := 0 to (dmGuia.frxReport.PreviewPages.Count - 1) do
  begin
    Page := dmGuia.frxReport.PreviewPages.Page[I];
    if (MargemSuperior > 0) then
      Page.TopMargin := MargemSuperior;
    if (MargemInferior > 0) then
      Page.BottomMargin := MargemInferior;
    if (MargemEsquerda > 0) then
      Page.LeftMargin := MargemEsquerda;
    if (MargemDireita > 0) then
      Page.RightMargin := MargemDireita;
    dmGuia.frxReport.PreviewPages.ModifyPage(I, Page);
  end;
end;


function TACBrGNREGuiaFR.PrepareReport(GNRE: TGNRERetorno): boolean;
var
  i: Integer;
  Stream: TStringStream;
begin
  Result := False;

  if Trim(FastFile) <> '' then
  begin
    if not (UpperCase(Copy(FastFile, Length(FastFile)-3, 4)) = '.FR3') then
    begin
      Stream := TStringStream.Create(FastFile);
      dmGuia.frxReport.FileName := '';
      dmGuia.frxReport.LoadFromStream(Stream);
      Stream.Free;
    end
    else
    if FileExists(FastFile) then
      dmGuia.frxReport.LoadFromFile(FastFile)
    else
      raise EACBrGNREGuiaFR.CreateFmt('Caminho do arquivo de impressão da Guia "%s" inválido.', [FastFile]);
  end
  else
    raise EACBrGNREGuiaFR.Create('Caminho do arquivo de impressão do Guia não assinalado.');

  dmGuia.frxReport.PrintOptions.Copies      := NumCopias;
  dmGuia.frxReport.PrintOptions.ShowDialog  := ShowDialog;
  dmGuia.frxReport.ShowProgress             := MostrarStatus;
  dmGuia.frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(dmGuia.frxReport.PrintOptions.Printer) then
    dmGuia.frxReport.PrintOptions.Printer := Impressora;

  if Assigned(GNRE) then
  begin
    dmGuia.GNRE := GNRE;
    dmGuia.CarregaDados;

    Result := dmGuia.frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrGNRE) then
    begin
      for i := 0 to TACBrGNRE(ACBrGNRE).GuiasRetorno.Count - 1 do
      begin
        dmGuia.GNRE := TACBrGNRE(ACBrGNRE).GuiasRetorno.Items[i].GNRE;
        dmGuia.CarregaDados;

        if (i > 0) then
          Result := dmGuia.frxReport.PrepareReport(False)
        else
          Result := dmGuia.frxReport.PrepareReport;
      end;
    end
    else
      raise EACBrGNREGuiaFR.Create('Propriedade ACBrGNRE não assinalada.');
  end;

  AjustaMargensReports;
end;

end.
