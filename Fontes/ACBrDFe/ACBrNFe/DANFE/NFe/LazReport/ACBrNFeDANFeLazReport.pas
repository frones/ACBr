{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Wemerson Souto                                  }
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

unit ACBrNFeDANFeLazReport;

interface

uses
  Classes, 
  SysUtils, 
  FileUtil, 
  Forms, 
  Controls,  
  ExtCtrls,
  DateUtils, 
  PrintersDlgs, 
  Printers, 
  strutils, 
  BufDataset, 
  DB,
  Dialogs,
  LResources,
  pcnNFe, 
  pcnConversao, 
  pcnConversaoNFe, 
  ACBrNFe.EnvEvento,
  ACBrNFe.RetInut,
  ACBrNFe, 
  ACBrNFeDANFEClass, 
  ACBrUtil.Base, 
  ACBrUtil.Strings, 
  ACBrUtil.FilesIO, 
  ACBrUtil.DateTime,
  ACBrDFeUtil, 
  ACBrValidador, 
  ACBrDelphiZXingQRCode,
  LR_Class, 
  LR_View, 
  LR_BarC, 
  LR_Shape, 
  LR_DBSet, 
  LR_Desgn, 
  lr_e_fclpdf;

type

  TACBrNFeLazReportClass = class;

  EACBrNFeDANFELazReport = class(Exception);

  { TACBrNFeDANFeLazReport }
  TACBrNFeDANFeLazReport = class(TACBrNFeDANFEClass)
  private
    FdmDanfe: TACBrNFeLazReportClass;
    FLazFile: string;
    FLazFileEvento: string;
    FLazFileInutilizacao: string;
    FMarcaDaguaMSG: string;

    function GetReport: TfrReport;
    function GetPreparedReport: TfrReport;
    function GetPreparedReportEvento: TfrReport;
    function GetPreparedReportInutilizacao: TfrReport;
    function PrepareReport(NFE: TNFe = nil): boolean;
    function PrepareReportEvento: boolean;
    function PrepareReportInutilizacao: boolean;
    procedure AjustarMargensReportAtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE: TNFe = nil); override;
    procedure ImprimirDANFEResumido(NFE: TNFe = nil); override;
    procedure ImprimirDANFEPDF(NFE: TNFe = nil); override;
    procedure ImprimirEVENTO(NFE: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(NFE: TNFe = nil); override;
    procedure ImprimirINUTILIZACAO(NFE: TNFe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(NFE: TNFe = nil); override;
    procedure DesignReport(NFE: TNFe); overload;
    procedure DesignReport(EventoNFe: TEventoNFe); overload;
    procedure DesignReport(RetInutNFe: TRetInutNFe); overload;

    property Report: TfrReport read GetReport;
    property PreparedReport: TfrReport read GetPreparedReport;
    property PreparedReportEvento: TfrReport read GetPreparedReportEvento;
    property PreparedReportInutilizacao: TfrReport read GetPreparedReportInutilizacao;

  published
    property LazFile: string read FLazFile write FLazFile;
    property LazFileEvento: string read FLazFileEvento write FLazFileEvento;
    property LazFileInutilizacao: string read FLazFileInutilizacao write FLazFileInutilizacao;
    property MarcaDaguaMSG: string read FMarcaDaguaMSG write FMarcaDaguaMSG;

  end;

  { TACBrNFeLazReportClass }

  TACBrNFeLazReportClass = class
  private
    bdsInutilizacao: TBufDataset;
    bdsLocalEntrega: TBufDataset;
    bdsPagamento: TBufDataset;
    bdsISSQN: TBufDataset;
    bdsDestinatario: TBufDataset;
    bdsDadosProdutos: TBufDataset;
    bdsDuplicatas: TBufDataset;
    bdsCalculoImposto: TBufDataset;
    bdsFatura: TBufDataset;
    bdsLocalRetirada: TBufDataset;
    bdsInformacoesAdicionais: TBufDataset;
    bdsEventos: TBufDataset;
    bdsVeiculo: TBufDataset;
    bdsVolumes: TBufDataset;
    bdsTransportador: TBufDataset;
    bdsParametros: TBufDataset;
    bdsIdentificacao: TBufDataset;
    bdsEmitente: TBufDataset;

    frDBInutilizacao: TfrDBDataSet;
    frDBLocalEntrega: TfrDBDataSet;
    frDBPagamento: TfrDBDataSet;
    frDBISSQN: TfrDBDataSet;
    frDBDestinatario: TfrDBDataSet;
    frDBDadosProdutos: TfrDBDataSet;
    frDBDuplicatas: TfrDBDataSet;
    frDBCalculoImposto: TfrDBDataSet;
    frDBFatura: TfrDBDataSet;
    frDBLocalRetirada: TfrDBDataSet;
    frDBInformacoesAdicionais: TfrDBDataSet;
    frDBEventos: TfrDBDataSet;
    frDBVeiculo: TfrDBDataSet;
    frDBVolumes: TfrDBDataSet;
    frDBTransportador: TfrDBDataSet;
    frDBParametros: TfrDBDataSet;
    frDBIdentificacao: TfrDBDataSet;
    frDBEmitente: TfrDBDataSet;

    DummyDm: TDatamodule;
    FfrReport: TfrReport;
    FPrintDialog: TPrintDialog;

    FDANFE: TACBrNFeDANFeLazReport;
    FNFe: TNFe;
    FEvento: TEventoNFe;
    FInutilizacao: TRetInutNFe;

    procedure CarregaIdentificacao;
    procedure CarregaEmitente;
    procedure CarregaDestinatario;
    procedure CarregaDadosProdutos;
    procedure CarregaParametros;
    procedure CarregaCalculoImposto;
    procedure CarregaTransportador;
    procedure CarregaVeiculo;
    procedure CarregaVolumes;
    procedure CarregaDuplicatas;
    procedure CarregaISSQN;
    procedure CarregaLocalRetirada;
    procedure CarregaLocalEntrega;
    procedure CarregaFatura;
    procedure CarregaPagamento;
    procedure CarregaInformacoesAdicionais;

    procedure ReportOnDBImageRead(Sender: TObject; S: TStream; var GraphExt : string);

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property NFe: TNFe read FNFe write FNFe;
    property Evento: TEventoNFe read FEvento write FEvento;
    property Inutilizacao: TRetInutNFe read FInutilizacao write FInutilizacao;
    property DANFE: TACBrNFeDANFeLazReport read FDANFE;
    property Report: TfrReport read FfrReport;
    property PrintDialog: TPrintDialog read FPrintDialog;

    procedure CarregaDadosNFe;
    procedure CarregaDadosEventos;
    procedure CarregaDadosInutilizacao;

  end;

implementation

{ TACBrNFeDANFeLazReport }
constructor TACBrNFeDANFeLazReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FdmDanfe := TACBrNFeLazReportClass.Create(Self);
  FLazFile := '';
  FLazFileEvento := '';
  FLazFileInutilizacao := '';
  FMarcaDaguaMSG := '';

end;

destructor TACBrNFeDANFeLazReport.Destroy;
begin
  FdmDanfe.Free;

  inherited Destroy;
end;

procedure TACBrNFeDANFeLazReport.DesignReport(NFE: TNFe);
var
  wProjectStream: TStringStream;
begin
  if not (uppercase(copy(LazFile, length(LazFile) - 3, 4)) = '.LRF') then
    begin
      wProjectStream := TStringStream.Create(LazFile);
      FdmDanfe.Report.FileName := '';
      FdmDanfe.Report.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
  else
  begin
    if FileExists(LazFile) then
      FdmDanfe.Report.LoadFromFile(LazFile)
    else
      raise EACBrNFeDANFELazReport.CreateFmt('Caminho do arquivo de impressão do DANFE "%s" inválido.', [LazFile]);
  end;

  FdmDanfe.NFe := NFE;
  FdmDanfe.CarregaDadosNFe;
  FdmDanfe.Report.DesignReport;
end;

procedure TACBrNFeDANFeLazReport.DesignReport(EventoNFe: TEventoNFe);
var
  wProjectStream: TStringStream;
begin
  if not (uppercase(copy(LazFile, length(LazFileEvento) - 3, 4)) = '.LRF') then
    begin
      wProjectStream := TStringStream.Create(LazFileEvento);
      FdmDanfe.Report.FileName := '';
      FdmDanfe.Report.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(LazFileEvento) then
        FdmDanfe.Report.LoadFromFile(LazFileEvento)
      else
        raise EACBrNFeDANFELazReport.CreateFmt('Caminho do arquivo de impressão do DANFE "%s" inválido.', [LazFileEvento]);
    end;

  FdmDanfe.Evento := EventoNFe;
  FdmDanfe.CarregaDadosEventos;
  FdmDanfe.Report.DesignReport;
end;

procedure TACBrNFeDANFeLazReport.DesignReport(RetInutNFe: TRetInutNFe);
var
  wProjectStream: TStringStream;
begin
  if not (uppercase(copy(LazFile, length(LazFileInutilizacao) - 3, 4)) = '.LRF') then
    begin
      wProjectStream := TStringStream.Create(LazFileInutilizacao);
      FdmDanfe.Report.FileName := '';
      FdmDanfe.Report.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(LazFileInutilizacao) then
        FdmDanfe.Report.LoadFromFile(LazFileInutilizacao)
      else
        raise EACBrNFeDANFELazReport.CreateFmt('Caminho do arquivo de impressão do DANFE "%s" inválido.', [LazFileInutilizacao]);
    end;

  FdmDanfe.Inutilizacao := RetInutNFe;
  FdmDanfe.CarregaDadosInutilizacao;
  FdmDanfe.Report.DesignReport;
end;

function TACBrNFeDANFeLazReport.GetReport: TfrReport;
begin
  if Trim(FLazFile) = '' then
    Result := nil
  else
  begin
    Result := FdmDanfe.Report;
  end;
end;

function TACBrNFeDANFeLazReport.GetPreparedReport: TfrReport;
begin
  if Trim(FLazFile) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := FdmDanfe.Report
    else
      Result := nil;
  end;
end;

function TACBrNFeDANFeLazReport.GetPreparedReportEvento: TfrReport;
begin
  if Trim(FLazFileEvento) = '' then
    Result := nil
  else
  begin
    if PrepareReportEvento then
      Result := FdmDanfe.Report
    else
      Result := nil;
  end;
end;

function TACBrNFeDANFeLazReport.GetPreparedReportInutilizacao: TfrReport;
begin
  if Trim(FLazFileInutilizacao) = '' then
    Result := nil
  else
  begin
    if PrepareReportInutilizacao then
      Result := FdmDanfe.Report
    else
      Result := nil;
  end;
end;

function TACBrNFeDANFeLazReport.PrepareReport(NFE: TNFe): boolean;
var
  I: integer;
  wProjectStream: TStringStream;
  ReportStream: array of TMemoryStream;
begin
  Result := False;

  if Trim(LazFile) <> '' then
  begin
    if not (uppercase(copy(LazFile, length(LazFile) - 3, 4)) = '.LRF') then
    begin
      wProjectStream := TStringStream.Create(LazFile);
      FdmDanfe.Report.FileName := '';
      FdmDanfe.Report.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(LazFile) then
        FdmDanfe.Report.LoadFromFile(LazFile)
      else
        raise EACBrNFeDANFELazReport.CreateFmt('Caminho do arquivo de impressão do DANFE "%s" inválido.', [LazFile]);
    end;
  end
  else
    raise EACBrNFeDANFELazReport.Create('Caminho do arquivo de impressão do DANFE não assinalado.');

  FdmDanfe.Report.ShowProgress := MostraStatus;

  // preparar relatorio
  if Assigned(NFE) then
  begin
    FdmDanfe.NFe := NFE;
    FdmDanfe.CarregaDadosNFe;

    AjustarMargensReportAtual;
    Result := FdmDanfe.Report.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrNFe) then
    begin

      SetLength(ReportStream, TACBrNFe(ACBrNFe).NotasFiscais.Count);

      for i := 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count - 1 do
      begin
        FdmDanfe.NFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe;
        FdmDanfe.CarregaDadosNFe;

        AjustarMargensReportAtual;
        Result := FdmDanfe.Report.PrepareReport;

        if not FdmDanfe.Report.CanRebuild and not (roDontUpgradePreparedReport in FdmDanfe.Report.Options) then
          FdmDanfe.Report.EMFPages.UpgradeToCurrentVersion;

        ReportStream[i] := TMemoryStream.Create;
        FdmDanfe.Report.EMFPages.SaveToStream(ReportStream[i]);
      end;

      for i := Low(ReportStream) to High(ReportStream) do
      begin
        ReportStream[i].Seek(0, soFromBeginning);

        if i > 0 then
          FdmDanfe.Report.EMFPages.AddPagesFromStream(ReportStream[i])
        else
          FdmDanfe.Report.EMFPages.LoadFromStream(ReportStream[i]);

        ReportStream[i].Free;
      end;

      FdmDanfe.Report.CanRebuild := False;
    end
    else
      raise EACBrNFeDANFELazReport.Create('Propriedade ACBrNFe não assinalada.');
  end;
end;

function TACBrNFeDANFeLazReport.PrepareReportEvento: boolean;
var
  wProjectStream: TStringStream;
begin

  if Trim(LazFileEvento) <> '' then
  begin
    if not (uppercase(copy(LazFileEvento, length(LazFileEvento) - 3, 4)) = '.LRF') then
    begin
      wProjectStream := TStringStream.Create(LazFileEvento);
      FdmDanfe.Report.FileName := '';
      FdmDanfe.Report.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(LazFileEvento) then
        FdmDanfe.Report.LoadFromFile(LazFileEvento)
      else
        raise EACBrNFeDANFELazReport.CreateFmt(
          'Caminho do arquivo de impressão do EVENTO "%s" inválido.', [LazFileEvento]);
    end;
  end
  else
    raise EACBrNFeDANFELazReport.Create(
      'Caminho do arquivo de impressão do EVENTO não assinalado.');

  FdmDanfe.Report.ShowProgress := MostraStatus;

  // preparar relatorio
  if Assigned(ACBrNFe) then
  begin
    if assigned(TACBrNFe(ACBrNFe).EventoNFe) then
    begin
      FdmDanfe.Evento := TACBrNFe(ACBrNFe).EventoNFe;
      FdmDanfe.CarregaDadosEventos;
    end
    else
      raise EACBrNFeDANFELazReport.Create('Evento não foi assinalado.');

    if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then
    begin
      FdmDanfe.Report.Variables.Values['PossuiNFe'] := QuotedStr('S');
      FdmDanfe.NFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
      FdmDanfe.CarregaDadosNFe;
    end;

    AjustarMargensReportAtual;
    Result := FdmDanfe.Report.PrepareReport;
  end
  else
    raise EACBrNFeDANFELazReport.Create('Propriedade ACBrNFe não assinalada.');
end;

function TACBrNFeDANFeLazReport.PrepareReportInutilizacao: boolean;
var
  wProjectStream: TStringStream;
begin

  if Trim(LazFileInutilizacao) <> '' then
  begin
    if not (uppercase(copy(LazFileInutilizacao, length(LazFileInutilizacao) - 3, 4)) =
      '.LRF') then
    begin
      wProjectStream := TStringStream.Create(LazFileInutilizacao);
      FdmDanfe.Report.FileName := '';
      FdmDanfe.Report.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(LazFileInutilizacao) then
        FdmDanfe.Report.LoadFromFile(LazFileInutilizacao)
      else
        raise EACBrNFeDANFELazReport.CreateFmt('Caminho do arquivo de impressão de INUTILIZAÇÃO "%s" inválido.', [LazFileInutilizacao]);
    end;
  end
  else
    raise EACBrNFeDANFELazReport.Create(
      'Caminho do arquivo de impressão de INUTILIZAÇÃO não assinalado.');

  FdmDanfe.Report.DefaultCopies := NumCopias;
  FdmDanfe.Report.ShowProgress := MostraStatus;

  // preparar relatorio
  if Assigned(ACBrNFe) then
  begin
    if assigned(TACBrNFe(ACBrNFe).InutNFe) then
    begin
      FdmDanfe.Inutilizacao := TACBrNFe(ACBrNFe).InutNFe.RetInutNFe;
      FdmDanfe.CarregaDadosInutilizacao;
    end
    else
      raise EACBrNFeDANFELazReport.Create('INUTILIZAÇÃO não foi assinalada.');

    AjustarMargensReportAtual;
    Result := FdmDanfe.Report.PrepareReport;
  end
  else
    raise EACBrNFeDANFELazReport.Create('Propriedade ACBrNFe não assinalada.');

end;

procedure TACBrNFeDANFeLazReport.AjustarMargensReportAtual;
var
  i: Integer;
begin
  for i := 0 to (FdmDanfe.Report.Pages.Count -1) do
  begin
    if MargemSuperior > 0 then
       FdmDanfe.Report.Pages[i].Margins.Top    := Round(MargemSuperior);
    if MargemInferior > 0 then
       FdmDanfe.Report.Pages[i].Margins.Bottom := Round(MargemInferior);
    if MargemEsquerda > 0 then
       FdmDanfe.Report.Pages[i].Margins.Left   := Round(MargemEsquerda);
    if MargemDireita > 0 then
       FdmDanfe.Report.Pages[i].Margins.Right  := Round(MargemDireita);
  end;
end;

procedure TACBrNFeDANFeLazReport.ImprimirDANFE(NFE: TNFe);
var
  POldInd, PNewInd, PageIni, PageFim: integer;
begin
  // Define a impressora
  POldInd := Printer.PrinterIndex;
  PNewInd := 0;
  if Impressora <> '' then
  begin
    PNewInd := Printer.Printers.IndexOf(Impressora);
    if PNewInd < 0 then
      PNewInd := 0;
  end;

  if POldInd <> PNewInd then
    FdmDanfe.Report.ChangePrinter(POldInd, PNewInd);

  if PrepareReport(NFE) then
  begin
    if MostraPreview then
      FdmDanfe.Report.ShowPreparedReport
    else
    begin
      PageIni := 1;
      PageFim := FdmDanfe.Report.EMFPages.Count;

      if MostraSetup then
      begin

        with FdmDanfe.PrintDialog do
        begin
          Options := [poPageNums];
          Collate := True;
          Copies := NumCopias;
          FromPage := PageIni;
          ToPage := PageFim;
          MaxPage := PageFim;
        end;

        if FdmDanfe.PrintDialog.Execute then
        begin
          if (Printer.PrinterIndex <> PNewInd) or
            FdmDanfe.Report.CanRebuild or
            FdmDanfe.Report.ChangePrinter(PNewInd, Printer.PrinterIndex) then
          begin
            PrepareReport(NFE);
          end;

          NumCopias := FdmDanfe.PrintDialog.Copies;

          if FdmDanfe.PrintDialog.PrintRange = prPageNums then
          begin
            PageIni := FdmDanfe.PrintDialog.FromPage;
            PageFim := FdmDanfe.PrintDialog.ToPage;
          end;
        end;
      end;

      FdmDanfe.Report.PrintPreparedReport(IntToStr(PageIni) + '-' +
        IntToStr(PageFim), NumCopias);
    end;
  end;
end;

procedure TACBrNFeDANFeLazReport.ImprimirDANFEResumido(NFE: TNFe);
var
  POldInd, PNewInd, PageIni, PageFim: integer;
begin
  // Define a impressora
  POldInd := Printer.PrinterIndex;
  PNewInd := 0;
  if Impressora <> '' then
  begin
    PNewInd := Printer.Printers.IndexOf(Impressora);
    if PNewInd < 0 then
      PNewInd := 0;
  end;

  if POldInd <> PNewInd then
    FdmDanfe.Report.ChangePrinter(POldInd, PNewInd);

  if PrepareReport(NFE) then
  begin
    if MostraPreview then
      FdmDanfe.Report.ShowPreparedReport
    else
    begin
      PageIni := 1;
      PageFim := 0;

      if MostraSetup then
      begin

        with FdmDanfe.PrintDialog do
        begin
          Options := [poPageNums];
          Collate := True;
          Copies := NumCopias;
          FromPage := 1;
          ToPage := FdmDanfe.Report.EMFPages.Count;
          MaxPage := FdmDanfe.Report.EMFPages.Count;
        end;

        if FdmDanfe.PrintDialog.Execute then
        begin
          if (Printer.PrinterIndex <> PNewInd) or
            FdmDanfe.Report.CanRebuild or
            FdmDanfe.Report.ChangePrinter(PNewInd, Printer.PrinterIndex) then
          begin
            PrepareReport(NFE);
          end;

          NumCopias := FdmDanfe.PrintDialog.Copies;

          if FdmDanfe.PrintDialog.PrintRange = prPageNums then
          begin
            PageIni := FdmDanfe.PrintDialog.FromPage;
            PageFim := FdmDanfe.PrintDialog.ToPage;
          end;
        end;
      end;

      FdmDanfe.Report.PrintPreparedReport(IntToStr(PageIni) + '-' +
        IntToStr(PageFim), NumCopias);
    end;
  end;
end;

procedure TACBrNFeDANFeLazReport.ImprimirDANFEPDF(NFE: TNFe);
begin
  if PrepareReport(NFE) then
  begin
    with FdmDanfe do
    begin

      FPArquivoPDF := PathWithDelim(Self.PathPDF) + OnlyNumber(NFe.infNFe.ID) + '-nfe.pdf';

      if not DirectoryExists(ExtractFileDir(FPArquivoPDF)) then
        ForceDirectories(ExtractFileDir(FPArquivoPDF));

      Report.ExportTo(TlrPdfExportFilter, FPArquivoPDF);
    end;
  end;
end;

procedure TACBrNFeDANFeLazReport.ImprimirEVENTO(NFE: TNFe);
var
  POldInd, PNewInd, PageIni, PageFim: integer;
begin
  // Define a impressora
  POldInd := Printer.PrinterIndex;
  PNewInd := 0;
  if Impressora <> '' then
  begin
    PNewInd := Printer.Printers.IndexOf(Impressora);
    if PNewInd < 0 then
      PNewInd := 0;
  end;

  if POldInd <> PNewInd then
    FdmDanfe.Report.ChangePrinter(POldInd, PNewInd);

  if PrepareReportEvento then
  begin
    if MostraPreview then
      FdmDanfe.Report.ShowPreparedReport
    else
    begin
      PageIni := 1;
      PageFim := 0;

      if MostraSetup then
      begin

        with FdmDanfe.PrintDialog do
        begin
          Options := [poPageNums];
          Collate := True;
          Copies := NumCopias;
          FromPage := 1;
          ToPage := FdmDanfe.Report.EMFPages.Count;
          MaxPage := FdmDanfe.Report.EMFPages.Count;
        end;

        if FdmDanfe.PrintDialog.Execute then
        begin
          if (Printer.PrinterIndex <> PNewInd) or
            FdmDanfe.Report.CanRebuild or
            FdmDanfe.Report.ChangePrinter(PNewInd, Printer.PrinterIndex) then
          begin
            PrepareReportEvento;
          end;

          NumCopias := FdmDanfe.PrintDialog.Copies;

          if FdmDanfe.PrintDialog.PrintRange = prPageNums then
          begin
            PageIni := FdmDanfe.PrintDialog.FromPage;
            PageFim := FdmDanfe.PrintDialog.ToPage;
          end;
        end;
      end;

      FdmDanfe.Report.PrintPreparedReport(IntToStr(PageIni) + '-' +
        IntToStr(PageFim), NumCopias);
    end;
  end;
end;

procedure TACBrNFeDANFeLazReport.ImprimirEVENTOPDF(NFE: TNFe);
begin
  if PrepareReportEvento then
  begin
    FPArquivoPDF := StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[0].InfEvento.id,
      'ID', '', [rfIgnoreCase]);
    FPArquivoPDF := PathWithDelim(Self.PathPDF) + FPArquivoPDF + '-procEventoNFe.pdf';

    if not DirectoryExists(ExtractFileDir(FPArquivoPDF)) then
      ForceDirectories(ExtractFileDir(FPArquivoPDF));

    FdmDanfe.Report.ExportTo(TlrPdfExportFilter, FPArquivoPDF);
  end;
end;

procedure TACBrNFeDANFeLazReport.ImprimirINUTILIZACAO(NFE: TNFe);
var
  POldInd, PNewInd, PageIni, PageFim: integer;
begin
  // Define a impressora
  POldInd := Printer.PrinterIndex;
  PNewInd := 0;
  if Impressora <> '' then
  begin
    PNewInd := Printer.Printers.IndexOf(Impressora);
    if PNewInd < 0 then
      PNewInd := 0;
  end;

  if POldInd <> PNewInd then
    FdmDanfe.Report.ChangePrinter(POldInd, PNewInd);

  if PrepareReportInutilizacao then
  begin
    if MostraPreview then
      FdmDanfe.Report.ShowPreparedReport
    else
    begin
      PageIni := 1;
      PageFim := 0;

      if MostraSetup then
      begin

        with FdmDanfe.PrintDialog do
        begin
          Options := [poPageNums];
          Collate := True;
          Copies := NumCopias;
          FromPage := 1;
          ToPage := FdmDanfe.Report.EMFPages.Count;
          MaxPage := FdmDanfe.Report.EMFPages.Count;
        end;

        if FdmDanfe.PrintDialog.Execute then
        begin
          if (Printer.PrinterIndex <> PNewInd) or
            FdmDanfe.Report.CanRebuild or
            FdmDanfe.Report.ChangePrinter(PNewInd, Printer.PrinterIndex) then
          begin
            PrepareReportInutilizacao;
          end;

          NumCopias := FdmDanfe.PrintDialog.Copies;

          if FdmDanfe.PrintDialog.PrintRange = prPageNums then
          begin
            PageIni := FdmDanfe.PrintDialog.FromPage;
            PageFim := FdmDanfe.PrintDialog.ToPage;
          end;
        end;
      end;

      FdmDanfe.Report.PrintPreparedReport(IntToStr(PageIni) + '-' +
        IntToStr(PageFim), NumCopias);
    end;
  end;
end;

procedure TACBrNFeDANFeLazReport.ImprimirINUTILIZACAOPDF(NFE: TNFe);
begin
  if PrepareReportInutilizacao then
  begin
    FPArquivoPDF := OnlyNumber(TACBrNFe(ACBrNFe).InutNFe.RetInutNFe.Id);
    FPArquivoPDF := PathWithDelim(Self.PathPDF) + FPArquivoPDF + '-procInutNFe.pdf';

    if not DirectoryExists(ExtractFileDir(FPArquivoPDF)) then
      ForceDirectories(ExtractFileDir(FPArquivoPDF));

    FdmDanfe.Report.ExportTo(TlrPdfExportFilter, FPArquivoPDF);
  end;
end;

{ TACBrNFeLazReportClass }
constructor TACBrNFeLazReportClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrNFeDANFeLazReport) then
    raise EACBrNFeException.Create('AOwner deve ser do tipo TACBrNFeDANFeLazReport');

  FDANFE := TACBrNFeDANFeLazReport(AOwner);

  DummyDm := TDatamodule.Create(nil);
  FfrReport := TfrCompositeReport.Create(DummyDm);
  with FfrReport do
  begin
    DoublePass := true;
    PreviewButtons := [pbZoom, pbFind, pbSave, pbPrint, pbExit];
    InitialZoom := pzPageWidth;
    OnDBImageRead := ReportOnDBImageRead;
  end;

  FPrintDialog := TPrintDialog.Create(DummyDm);

  // bdsIdentificacao
  if not Assigned(bdsIdentificacao) then
  begin
    bdsIdentificacao := TBufDataset.Create(DummyDm);
    frDBIdentificacao := TfrDBDataSet.Create(DummyDm);
    with frDBIdentificacao do
    begin
      Name := 'frDBIdentificacao';
      DataSet := bdsIdentificacao;
      OpenDataSource := False;
    end;
    with bdsIdentificacao do
    begin
      Name := 'bdsIdentificacao';
      FieldDefs.Add('Id', ftString, 44);
      FieldDefs.Add('Chave', ftString, 60);
      FieldDefs.Add('cUF', ftString, 2);
      FieldDefs.Add('cNF', ftString, 9);
      FieldDefs.Add('NatOp', ftString, 60);
      FieldDefs.Add('IndPag', ftString, 1);
      FieldDefs.Add('Mod_', ftString, 2);
      FieldDefs.Add('Serie', ftString, 3);
      FieldDefs.Add('NNF', ftString, 11);
      FieldDefs.Add('DEmi', ftString, 19);
      FieldDefs.Add('DSaiEnt', ftString, 10);
      FieldDefs.Add('TpNF', ftString, 1);
      FieldDefs.Add('CMunFG', ftString, 7);
      FieldDefs.Add('TpImp', ftString, 1);
      FieldDefs.Add('TpEmis', ftString, 1);
      FieldDefs.Add('CDV', ftString, 1);
      FieldDefs.Add('TpAmb', ftString, 1);
      FieldDefs.Add('FinNFe', ftString, 1);
      FieldDefs.Add('ProcEmi', ftString, 1);
      FieldDefs.Add('VerProc', ftString, 6);
      FieldDefs.Add('HoraSaida', ftString, 10);
      FieldDefs.Add('MensagemFiscal', ftString, 200);
      FieldDefs.Add('URL', ftString, 1000);
      CreateDataSet;
    end;
  end;

  // bdsEmitente
  if not Assigned(bdsEmitente) then
  begin
    bdsEmitente := TBufDataset.Create(DummyDm);
    frDBEmitente := TfrDBDataset.Create(DummyDm);
    with frDBEmitente do
    begin
      Name := 'frDBEmitente';
      DataSet := bdsEmitente;
      OpenDataSource := False;
    end;
    with bdsEmitente do
    begin
      Name := 'bdsEmitente';
      FieldDefs.Add('CNPJ', ftString, 18);
      FieldDefs.Add('XNome', ftString, 60);
      FieldDefs.Add('XFant', ftString, 60);
      FieldDefs.Add('XLgr', ftString, 60);
      FieldDefs.Add('Nro', ftString, 60);
      FieldDefs.Add('XCpl', ftString, 60);
      FieldDefs.Add('XBairro', ftString, 60);
      FieldDefs.Add('CMun', ftString, 7);
      FieldDefs.Add('XMun', ftString, 60);
      FieldDefs.Add('UF', ftString, 2);
      FieldDefs.Add('CEP', ftString, 9);
      FieldDefs.Add('CPais', ftString, 4);
      FieldDefs.Add('XPais', ftString, 60);
      FieldDefs.Add('Fone', ftString, 15);
      FieldDefs.Add('IE', ftString, 15);
      FieldDefs.Add('IM', ftString, 15);
      FieldDefs.Add('IEST', ftString, 15);
      FieldDefs.Add('CRT', ftString, 1);
      FieldDefs.Add('DESCR_CST', ftString, 30);
      FieldDefs.Add('DADOS_ENDERECO', ftString, 1000);
      CreateDataSet;
    end;
  end;

  // bdsDestinatario
  if not Assigned(bdsDestinatario) then
  begin
    bdsDestinatario := TBufDataset.Create(DummyDm);
    frDBDestinatario := TfrDBDataset.Create(DummyDm);
    with frDBDestinatario do
    begin
      Name := 'frDBDestinatario';
      DataSet := bdsDestinatario;
      OpenDataSource := False;
    end;
    with bdsDestinatario do
    begin
      Name := 'bdsDestinatario';
      FieldDefs.Add('CNPJCPF', ftString, 18);
      FieldDefs.Add('XNome', ftString, 60);
      FieldDefs.Add('XLgr', ftString, 60);
      FieldDefs.Add('Nro', ftString, 60);
      FieldDefs.Add('XCpl', ftString, 60);
      FieldDefs.Add('XBairro', ftString, 60);
      FieldDefs.Add('CMun', ftString, 7);
      FieldDefs.Add('XMun', ftString, 60);
      FieldDefs.Add('UF', ftString, 2);
      FieldDefs.Add('CEP', ftString, 9);
      FieldDefs.Add('CPais', ftString, 4);
      FieldDefs.Add('XPais', ftString, 60);
      FieldDefs.Add('Fone', ftString, 15);
      FieldDefs.Add('IE', ftString, 18);
      FieldDefs.Add('Consumidor', ftString, 150);
      CreateDataSet;
    end;
  end;

  // bdsDadosProdutos
  if not Assigned(bdsDadosProdutos) then
  begin
    bdsDadosProdutos := TBufDataset.Create(DummyDm);
    frDBDadosProdutos := TfrDBDataset.Create(DummyDm);
    with frDBDadosProdutos do
    begin
      Name := 'frDBDadosProdutos';
      DataSet := bdsDadosProdutos;
      OpenDataSource := False;
    end;
    with bdsDadosProdutos do
    begin
      Name := 'bdsDadosProdutos';
      FieldDefs.Add('CProd', ftString, 60);
      FieldDefs.Add('cEAN', ftString, 60);
      FieldDefs.Add('XProd', ftString, 120);
      FieldDefs.Add('infAdProd', ftString, 1000);
      FieldDefs.Add('NCM', ftString, 9);
      FieldDefs.Add('EXTIPI', ftString, 8);
      FieldDefs.Add('genero', ftString, 8);
      FieldDefs.Add('CFOP', ftString, 4);
      FieldDefs.Add('UCom', ftString, 6);
      FieldDefs.Add('QCom', ftFloat);
      FieldDefs.Add('VUnCom', ftFloat);
      FieldDefs.Add('VProd', ftString, 18);
      FieldDefs.Add('cEANTrib', ftString, 60);
      FieldDefs.Add('UTrib', ftString, 6);
      FieldDefs.Add('QTrib', ftFloat);
      FieldDefs.Add('vUnTrib', ftFloat);
      FieldDefs.Add('vFrete', ftString, 18);
      FieldDefs.Add('vOutro', ftString, 18);
      FieldDefs.Add('vSeg', ftString, 18);
      FieldDefs.Add('vDesc', ftString, 18);
      FieldDefs.Add('ORIGEM', ftString, 1);
      FieldDefs.Add('CST', ftString, 3);
      FieldDefs.Add('vBC', ftString, 18);
      FieldDefs.Add('pICMS', ftString, 18);
      FieldDefs.Add('vICMS', ftString, 18);
      FieldDefs.Add('vIPI', ftString, 18);
      FieldDefs.Add('pIPI', ftString, 18);
      FieldDefs.Add('VTotTrib', ftString, 18);
      FieldDefs.Add('ChaveNFe', ftString, 50);
      FieldDefs.Add('vISSQN', ftString, 18);
      FieldDefs.Add('vBcISSQN', ftString, 18);
      FieldDefs.Add('vBcST', ftString, 18);
      FieldDefs.Add('vICMSST', ftString, 18);
      FieldDefs.Add('nLote', ftString, 20);
      FieldDefs.Add('qLote', ftFloat);
      FieldDefs.Add('dFab', ftDateTime);
      FieldDefs.Add('dVal', ftDateTime);
      FieldDefs.Add('DescricaoProduto', ftString, 2000);
      FieldDefs.Add('Unidade', ftString, 14);
      FieldDefs.Add('Quantidade', ftString, 18);
      FieldDefs.Add('ValorUnitario', ftString, 18);
      FieldDefs.Add('Valorliquido', ftString, 18);
      FieldDefs.Add('ValorAcrescimos', ftString, 18);
      CreateDataSet;
    end;
  end;

  // bdsParametros
  if not Assigned(bdsParametros) then
  begin
    bdsParametros := TBufDataset.Create(DummyDm);
    frDBParametros := TfrDBDataset.Create(DummyDm);
    with frDBParametros do
    begin
      Name := 'frDBParametros';
      DataSet := bdsParametros;
      OpenDataSource := False;
    end;
    with bdsParametros do
    begin
      Name := 'bdsParametros';
      FieldDefs.Add('poscanhoto', ftString, 1);
      FieldDefs.Add('ResumoCanhoto', ftString, 200);
      FieldDefs.Add('Mensagem0', ftString, 60);
      FieldDefs.Add('Imagem', ftString, 256);
      FieldDefs.Add('Sistema', ftString, 150);
      FieldDefs.Add('Usuario', ftString, 60);
      FieldDefs.Add('Site', ftString, 60);
      FieldDefs.Add('Email', ftString, 60);
      FieldDefs.Add('Desconto', ftString, 60);
      FieldDefs.Add('TotalLiquido', ftString, 60);
      FieldDefs.Add('ChaveAcesso_Descricao', ftString, 90);
      FieldDefs.Add('Contingencia_ID', ftString, 36);
      FieldDefs.Add('Contingencia_Descricao', ftString, 60);
      FieldDefs.Add('Contingencia_Valor', ftString, 60);
      FieldDefs.Add('LogoExpandido', ftString, 1);
      FieldDefs.Add('DESCR_CST', ftString, 30);
      FieldDefs.Add('ConsultaAutenticidade', ftString, 300);
      FieldDefs.Add('sDisplayFormat', ftString, 25);
      FieldDefs.Add('iFormato', ftInteger);
      FieldDefs.Add('Casas_qCom', ftInteger);
      FieldDefs.Add('Casas_vUnCom', ftInteger);
      FieldDefs.Add('Mask_qCom', ftString, 30);
      FieldDefs.Add('Mask_vUnCom', ftString, 30);
      FieldDefs.Add('LogoCarregado', ftBlob);
      FieldDefs.Add('QrCodeCarregado', ftGraphic, 1000);
      FieldDefs.Add('DescricaoViaEstabelec', ftString, 30);
      FieldDefs.Add('QtdeItens', ftInteger);
      FieldDefs.Add('ExpandirDadosAdicionaisAuto', ftString, 1);
      FieldDefs.Add('ImprimeDescAcrescItem', ftInteger);
      FieldDefs.Add('nProt', ftString, 30);
      FieldDefs.Add('dhRecbto', ftDateTime);
      CreateDataSet;
    end;
  end;

  // bdsDuplicatas
  if not Assigned(bdsDuplicatas) then
  begin
    bdsDuplicatas := TBufDataset.Create(DummyDm);
    frDBDuplicatas := TfrDBDataset.Create(DummyDm);
    with frDBDuplicatas do
    begin
      Name := 'frDBDuplicatas';
      DataSet := bdsDuplicatas;
      OpenDataSource := False;
    end;
    with bdsDuplicatas do
    begin
      Name := 'bdsDuplicatas';
      FieldDefs.Add('NDup', ftString, 60);
      FieldDefs.Add('DVenc', ftString, 10);
      FieldDefs.Add('VDup', ftFloat);
      FieldDefs.Add('ChaveNFe', ftString, 50);
      CreateDataSet;
    end;
  end;

  // bdsCalculoImposto
  if not Assigned(bdsCalculoImposto) then
  begin
    bdsCalculoImposto := TBufDataset.Create(DummyDm);
    frDBCalculoImposto := TfrDBDataset.Create(DummyDm);
    with frDBCalculoImposto do
    begin
      Name := 'frDBCalculoImposto';
      DataSet := bdsCalculoImposto;
      OpenDataSource := False;
    end;
    with bdsCalculoImposto do
    begin
      Name := 'bdsCalculoImposto';
      FieldDefs.Add('VBC', ftFloat);
      FieldDefs.Add('VICMS', ftFloat);
      FieldDefs.Add('VBCST', ftFloat);
      FieldDefs.Add('VST', ftFloat);
      FieldDefs.Add('VProd', ftFloat);
      FieldDefs.Add('VFrete', ftFloat);
      FieldDefs.Add('VSeg', ftFloat);
      FieldDefs.Add('VDesc', ftFloat);
      FieldDefs.Add('VII', ftFloat);
      FieldDefs.Add('VIPI', ftFloat);
      FieldDefs.Add('VPIS', ftFloat);
      FieldDefs.Add('VCOFINS', ftFloat);
      FieldDefs.Add('VOutro', ftFloat);
      FieldDefs.Add('VNF', ftFloat);
      FieldDefs.Add('VTotTrib', ftFloat);
      FieldDefs.Add('VTribPerc', ftFloat);
      FieldDefs.Add('VTribFonte', ftString, 100);
      FieldDefs.Add('vTotPago', ftFloat);
      FieldDefs.Add('vTroco', ftFloat);
      FieldDefs.Add('ValorApagar', ftFloat);
      CreateDataSet;
    end;
  end;

  // bdsTransportador
  if not Assigned(bdsTransportador) then
  begin
    bdsTransportador := TBufDataset.Create(DummyDm);
    frDBTransportador := TfrDBDataset.Create(DummyDm);
    with frDBTransportador do
    begin
      Name := 'frDBTransportador';
      DataSet := bdsTransportador;
      OpenDataSource := False;
    end;
    with bdsTransportador do
    begin
      Name := 'bdsTransportador';
      FieldDefs.Add('ModFrete', ftString, 14);
      FieldDefs.Add('CNPJCPF', ftString, 18);
      FieldDefs.Add('XNome', ftString, 60);
      FieldDefs.Add('IE', ftString, 15);
      FieldDefs.Add('XEnder', ftString, 60);
      FieldDefs.Add('XMun', ftString, 60);
      FieldDefs.Add('UF', ftString, 2);
      CreateDataSet;
    end;
  end;

  // bdsVeiculo
  if not Assigned(bdsVeiculo) then
  begin
    bdsVeiculo := TBufDataset.Create(DummyDm);
    frDBVeiculo := TfrDBDataset.Create(DummyDm);
    with frDBVeiculo do
    begin
      Name := 'frDBVeiculo';
      DataSet := bdsVeiculo;
      OpenDataSource := False;
    end;
    with bdsVeiculo do
    begin
      Name := 'bdsVeiculo';
      FieldDefs.Add('PLACA', ftString, 8);
      FieldDefs.Add('UF', ftString, 2);
      FieldDefs.Add('RNTC', ftString, 20);
      CreateDataSet;
    end;
  end;

  // bdsVolumes
  if not Assigned(bdsVolumes) then
  begin
    bdsVolumes := TBufDataset.Create(DummyDm);
    frDBVolumes := TfrDBDataset.Create(DummyDm);
    with frDBVolumes do
    begin
      Name := 'frDBVolumes';
      DataSet := bdsVolumes;
      OpenDataSource := False;
    end;
    with bdsVolumes do
    begin
      Name := 'bdsVolumes';
      FieldDefs.Add('QVol', ftFloat);
      FieldDefs.Add('Esp', ftString, 60);
      FieldDefs.Add('Marca', ftString, 60);
      FieldDefs.Add('NVol', ftString, 60);
      FieldDefs.Add('PesoL', ftFloat);
      FieldDefs.Add('PesoB', ftFloat);
      CreateDataSet;
    end;
  end;

  // csdEvento
  if not Assigned(bdsEventos) then
  begin
    bdsEventos := TBufDataset.Create(DummyDm);
    bdsEventos.Name := 'bdsEventos';
    frDBEventos := TfrDBDataset.Create(DummyDm);
    with frDBEventos do
    begin
      Name := 'frDBEventos';
      DataSet := bdsEventos;
      OpenDataSource := False;
    end;
  end;

  // bdsISSQN
  if not Assigned(bdsISSQN) then
  begin
    bdsISSQN := TBufDataset.Create(DummyDm);
    frDBISSQN := TfrDBDataset.Create(DummyDm);
    with frDBISSQN do
    begin
      Name := 'frDBISSQN';
      DataSet := bdsISSQN;
      OpenDataSource := False;
    end;
    with bdsISSQN do
    begin
      Name := 'bdsISSQN';
      FieldDefs.Add('vSERV', ftFloat);
      FieldDefs.Add('vBC', ftFloat);
      FieldDefs.Add('vISS', ftFloat);
      FieldDefs.Add('vDescIncond', ftFloat);
	  FieldDefs.Add('vISSRet', ftFloat);
      CreateDataSet;
    end;
  end;

  // bdsFatura
  if not Assigned(bdsFatura) then
  begin
    bdsFatura := TBufDataset.Create(DummyDm);
    frDBFatura := TfrDBDataset.Create(DummyDm);
    with frDBFatura do
    begin
      Name := 'frDBFatura';
      DataSet := bdsFatura;
      OpenDataSource := False;
    end;
    with bdsFatura do
    begin
      Name := 'bdsFatura';
      FieldDefs.Add('iForma', ftInteger);
      FieldDefs.Add('Pagamento', ftString, 20);
      FieldDefs.Add('nFat', ftString, 60);
      FieldDefs.Add('vOrig', ftFloat);
      FieldDefs.Add('vDesc', ftFloat);
      FieldDefs.Add('vLiq', ftFloat);
      CreateDataSet;
    end;
  end;

  // bdsLocalRetirada
  if not Assigned(bdsLocalRetirada) then
  begin
    bdsLocalRetirada := TBufDataset.Create(DummyDm);
    frDBLocalRetirada := TfrDBDataset.Create(DummyDm);
    with frDBLocalRetirada do
    begin
      Name := 'frDBLocalRetirada';
      DataSet := bdsLocalRetirada;
      OpenDataSource := False;
    end;
    with bdsLocalRetirada do
    begin
      Name := 'bdsLocalRetirada';
      FieldDefs.Add('CNPJ', ftString, 18);
      FieldDefs.Add('XLgr', ftString, 60);
      FieldDefs.Add('Nro', ftString, 60);
      FieldDefs.Add('XCpl', ftString, 60);
      FieldDefs.Add('XBairro', ftString, 60);
      FieldDefs.Add('CMun', ftString, 7);
      FieldDefs.Add('XMun', ftString, 60);
      FieldDefs.Add('UF', ftString, 2);
      CreateDataSet;
    end;
  end;

  // bdsLocalEntrega
  if not Assigned(bdsLocalEntrega) then
  begin
    bdsLocalEntrega := TBufDataset.Create(DummyDm);
    frDBLocalEntrega := TfrDBDataset.Create(DummyDm);
    with frDBLocalEntrega do
    begin
      Name := 'frDBLocalEntrega';
      DataSet := bdsLocalEntrega;
      OpenDataSource := False;
    end;
    with bdsLocalEntrega do
    begin
      Name := 'bdsLocalEntrega';
      FieldDefs.Add('CNPJ', ftString, 18);
      FieldDefs.Add('XLgr', ftString, 60);
      FieldDefs.Add('Nro', ftString, 60);
      FieldDefs.Add('XCpl', ftString, 60);
      FieldDefs.Add('XBairro', ftString, 60);
      FieldDefs.Add('CMun', ftString, 7);
      FieldDefs.Add('XMun', ftString, 60);
      FieldDefs.Add('UF', ftString, 2);
      CreateDataSet;
    end;
  end;

  // bdsInformacoesAdicionais
  if not Assigned(bdsInformacoesAdicionais) then
  begin
    bdsInformacoesAdicionais := TBufDataset.Create(DummyDm);
    frDBInformacoesAdicionais := TfrDBDataset.Create(DummyDm);
    with frDBInformacoesAdicionais do
    begin
      Name := 'frDBInformacoesAdicionais';
      DataSet := bdsInformacoesAdicionais;
      OpenDataSource := False;
    end;
    with bdsInformacoesAdicionais do
    begin
      Name := 'bdsInformacoesAdicionais';
      FieldDefs.Add('OBS', ftString, 6900);
      FieldDefs.Add('LinhasOBS', ftInteger);
      CreateDataSet;
    end;
  end;

  // bdsPagamento
  if not Assigned(bdsPagamento) then
  begin
    bdsPagamento := TBufDataset.Create(DummyDm);
    frDBPagamento := TfrDBDataset.Create(DummyDm);
    with frDBPagamento do
    begin
      Name := 'frDBPagamento';
      DataSet := bdsPagamento;
      OpenDataSource := False;
    end;
    with bdsPagamento do
    begin
      Name := 'bdsPagamento';
      FieldDefs.Add('tPag', ftString, 50);
      FieldDefs.Add('vPag', ftFloat);
      FieldDefs.Add('vTroco', ftFloat);
      FieldDefs.Add('CNPJ', ftString, 50);
      FieldDefs.Add('tBand', ftString, 50);
      FieldDefs.Add('cAut', ftString, 20);
      CreateDataSet;
    end;
  end;

  //bdsInutilização
  if not Assigned(bdsInutilizacao) then
  begin
    bdsInutilizacao := TBufDataset.Create(DummyDm);
    bdsInutilizacao.Name := 'bdsInutilizacao';
    frDBInutilizacao := TfrDBDataset.Create(DummyDm);
    with frDBInutilizacao do
    begin
      Name := 'frDBInutilizacao';
      DataSet := bdsInutilizacao;
      OpenDataSource := False;
    end;
  end;
end;

destructor TACBrNFeLazReportClass.Destroy;
begin
  bdsInutilizacao.Free;
  bdsLocalEntrega.Free;
  bdsPagamento.Free;
  bdsISSQN.Free;
  bdsDestinatario.Free;
  bdsDadosProdutos.Free;
  bdsDuplicatas.Free;
  bdsCalculoImposto.Free;
  bdsFatura.Free;
  bdsLocalRetirada.Free;
  bdsInformacoesAdicionais.Free;
  bdsEventos.Free;
  bdsVeiculo.Free;
  bdsVolumes.Free;
  bdsTransportador.Free;
  bdsParametros.Free;
  bdsIdentificacao.Free;
  bdsEmitente.Free;

  frDBInutilizacao.Free;
  frDBLocalEntrega.Free;
  frDBPagamento.Free;
  frDBISSQN.Free;
  frDBDestinatario.Free;
  frDBDadosProdutos.Free;
  frDBDuplicatas.Free;
  frDBCalculoImposto.Free;
  frDBFatura.Free;
  frDBLocalRetirada.Free;
  frDBInformacoesAdicionais.Free;
  frDBEventos.Free;
  frDBVeiculo.Free;
  frDBVolumes.Free;
  frDBTransportador.Free;
  frDBParametros.Free;
  frDBIdentificacao.Free;
  frDBEmitente.Free;

  FfrReport.Free;
  FPrintDialog.Free;
  DummyDm.Free;

  inherited Destroy;
end;

procedure TACBrNFeLazReportClass.ReportOnDBImageRead(Sender: TObject; S: TStream; var GraphExt : string);
Var
  val1, val2:  WORD;
begin
  val1 := 0;
  val2 := 0;

  S.Seek(0, soFromBeginning);
  S.Read(val1,2);
  S.Position := 2;
  S.Read(val2,2);
  if (val1 = $4D42) then  GraphExt := 'bmp';
  if (val1 = $4947) and (val2 = $3846) then  GraphExt := 'gif';
  if (val1 = $5089) and (val2 = $474E) then  GraphExt := 'png';
  if (val1 = $D8FF) and (val2 = $E0FF) then  GraphExt := 'jpg';
  S.Seek(0, soFromBeginning);
end;

procedure TACBrNFeLazReportClass.CarregaCalculoImposto;
begin
  with bdsCalculoImposto do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Total.ICMSTot do
    begin
      FieldByName('VBC').AsFloat := VBC;
      FieldByName('VICMS').AsFloat := VICMS;
      FieldByName('VBCST').AsFloat := VBCST;
      FieldByName('VST').AsFloat := VST;
      FieldByName('VProd').AsFloat := VProd;
      FieldByName('VFrete').AsFloat := VFrete;
      FieldByName('VSeg').AsFloat := VSeg;
      FieldByName('VDesc').AsFloat := VDesc;
      FieldByName('VII').AsFloat := VII;
      FieldByName('VIPI').AsFloat := VIPI;
      FieldByName('VPIS').AsFloat := VPIS;
      FieldByName('VCOFINS').AsFloat := VCOFINS;
      FieldByName('VOutro').AsFloat := VOutro;
      FieldByName('VNF').AsFloat := VNF;
      FieldByName('VTotTrib').AsFloat := VTotTrib;
      FieldByName('ValorApagar').AsFloat := VProd - VDesc + VOutro;
      FieldByName('VTribPerc').AsFloat :=
        FDANFE.ManterVTribPerc(VTotTrib, VProd, VNF);
      if NaoEstaVazio(FDANFE.FonteTributos) then
        FieldByName('VTribFonte').AsString :=
          '(Fonte: ' + FDANFE.FonteTributos + ')';
    end;
    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaDadosNFe;
begin
  CarregaParametros;
  CarregaIdentificacao;
  CarregaEmitente;
  CarregaDestinatario;
  CarregaDadosProdutos;
  CarregaCalculoImposto;
  CarregaTransportador;
  CarregaVeiculo;
  CarregaVolumes;
  CarregaDuplicatas;
  CarregaISSQN;
  CarregaLocalRetirada;
  CarregaLocalEntrega;
  CarregaFatura;
  CarregaPagamento;
  CarregaInformacoesAdicionais;
end;

procedure TACBrNFeLazReportClass.CarregaDadosProdutos;
var
  inItem: integer;
begin
  if not bdsParametros.Active then
    CarregaParametros;

  bdsParametros.First;

  // verificar se e DANFE detalhado
  // dados dos produtos
  with bdsDadosProdutos do
  begin
    Close;
    CreateDataSet;
    if (NFe.Ide.modelo <> 65) then
    begin
      for inItem := 0 to NFe.Det.Count - 1 do
      begin
        Append;
        with FNFe.Det.Items[inItem] do
        begin
          FieldByName('ChaveNFe').AsString := FNFe.infNFe.ID;
          FieldByName('cProd').AsString :=
            FDANFE.ManterCodigo(Prod.cEAN, Prod.cProd);
          FieldByName('cEAN').AsString := Prod.cEAN;
          FieldByName('XProd').AsString :=
            StringReplace(Prod.xProd, ';', sLineBreak , [rfReplaceAll]);
          FieldByName('VProd').AsString :=
            FDANFE.ManterVprod(Prod.VProd, Prod.vDesc);
          FieldByName('vTotTrib').AsString :=
            FDANFE.ManterdvTotTrib(Imposto.vTotTrib);
          FieldByName('infAdProd').AsString :=
            FDANFE.ManterinfAdProd(FNFe, inItem);
          FieldByName('DescricaoProduto').AsString :=
            FDANFE.ManterXProd(FNFe, inItem);
          FieldByName('NCM').AsString := Prod.NCM;
          FieldByName('EXTIPI').AsString := Prod.EXTIPI;
          FieldByName('genero').AsString := '';
          FieldByName('CFOP').AsString := Prod.CFOP;
          FieldByName('Ucom').AsString := Prod.UCom;
          FieldByName('QCom').AsFloat := Prod.QCom;
          FieldByName('VUnCom').AsFloat := Prod.VUnCom;
          FieldByName('cEANTrib').AsString := Prod.cEANTrib;
          FieldByName('UTrib').AsString := Prod.uTrib;
          FieldByName('QTrib').AsFloat := Prod.qTrib;
          FieldByName('VUnTrib').AsFloat := Prod.vUnTrib;
          FieldByName('vFrete').AsString := FormatFloatBr(Prod.vFrete, ',0.00');
          FieldByName('vSeg').AsString := FormatFloatBr(Prod.vSeg, ',0.00');
          FieldByName('vOutro').AsString := FormatFloatBr(Prod.vOutro, ',0.00');
          FieldByName('vDesc').AsString := FormatFloatBr(FDANFE.ManterVDesc(Prod.vDesc, Prod.VUnCom, Prod.QCom), ',0.00');
          FieldByName('ORIGEM').AsString := OrigToStr(Imposto.ICMS.orig);
          FieldByName('CST').AsString := FDANFE.ManterCst(FNFe.Emit.CRT, Imposto.ICMS.CSOSN, Imposto.ICMS.CST);
          FieldByName('VBC').AsString := FormatFloatBr(Imposto.ICMS.vBC, ',0.00');
          FieldByName('PICMS').AsString := FormatFloatBr(Imposto.ICMS.pICMS, ',0.00');
          FieldByName('VICMS').AsString := FormatFloatBr(Imposto.ICMS.vICMS, ',0.00');
          FieldByName('VBCST').AsString := FormatFloatBr(Imposto.ICMS.vBcST, ',0.00');
          FieldByName('VICMSST').AsString := FormatFloatBr(Imposto.ICMS.vICMSST, ',0.00');
          FieldByName('VIPI').AsString := FormatFloatBr(Imposto.IPI.VIPI, ',0.00');
          FieldByName('PIPI').AsString := FormatFloatBr(Imposto.IPI.PIPI, ',0.00');
          FieldByName('vISSQN').AsString := FormatFloatBr(Imposto.ISSQN.vISSQN, ',0.00');
          FieldByName('vBcISSQN').AsString := FormatFloatBr(Imposto.ISSQN.vBC, ',0.00');
          FieldByName('Valorliquido').AsString := FormatFloatBr(Prod.vProd - Prod.vDesc, ',0.00');
          FieldByName('ValorAcrescimos').AsString := FormatFloatBr(Prod.vProd + Prod.vOutro, ',0.00');

          case FDANFE.ImprimeValor of
            iuComercial:
            begin
              FieldByName('Unidade').AsString := FieldByName('Ucom').AsString;
              FieldByName('Quantidade').AsString :=
                FDANFE.FormatarQuantidade(FieldByName('QCom').AsFloat);
              FieldByName('ValorUnitario').AsString :=
                FDANFE.FormatarValorUnitario(FieldByName('VUnCom').AsFloat);
            end;
            iuTributavel:
            begin
              FieldByName('Unidade').AsString := FieldByName('UTrib').AsString;
              FieldByName('Quantidade').AsString :=
                FDANFE.FormatarQuantidade(FieldByName('QTrib').AsFloat);
              FieldByName('ValorUnitario').AsString :=
                FDANFE.FormatarValorUnitario(FieldByName('VUnTrib').AsFloat);
            end;
            iuComercialETributavel:
            begin
              if FieldByName('Ucom').AsString = FieldByName('UTrib').AsString then
              begin
                FieldByName('Unidade').AsString := FieldByName('Ucom').AsString;
                FieldByName('Quantidade').AsString :=
                  FDANFE.FormatarQuantidade(FieldByName('QCom').AsFloat);
                FieldByName('ValorUnitario').AsString :=
                  FDANFE.FormatarValorUnitario(FieldByName('VUnCom').AsFloat);
              end
              else
              begin
                FieldByName('Unidade').AsString :=
                  FDANFE.ManterUnidades(FieldByName('Ucom').AsString,
                  FieldByName('UTrib').AsString);
                FieldByName('Quantidade').AsString :=
                  FDANFE.ManterQuantidades(FieldByName('QCom').AsFloat,
                  FieldByName('QTrib').AsFloat);
                FieldByName('ValorUnitario').AsString :=
                  FDANFE.ManterValoresUnitarios(FieldByName('VUnCom').AsFloat,
                  FieldByName('VUnTrib').AsFloat);
              end;
            end;
          end;
          Post;
        end;
      end;
    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaDestinatario;
begin
  { destinatário }
  with bdsDestinatario do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Dest do
    begin
      if NaoEstaVazio(idEstrangeiro) then
        FieldByName('CNPJCPF').AsString := idEstrangeiro
      else
        FieldByName('CNPJCPF').AsString := FormatarCNPJouCPF(CNPJCPF);

      FieldByName('IE').AsString := IE;
      FieldByName('XNome').AsString := XNome;
      with EnderDest do
      begin
        FieldByName('XLgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := XMun;
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(CEP);
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;

      FieldByName('Consumidor').AsString := '';

      if (bdsIdentificacao.FieldByName('Mod_').AsString = '65') then
      begin
        if NaoEstaVazio(idEstrangeiro) then
          FieldByName('Consumidor').AsString :=
            'ESTRANGEIRO: ' + Trim(FieldByName('CNPJCPF').AsString) +
            ' ' + trim(FieldByName('XNome').AsString)
        else
        begin
          if (FieldByName('CNPJCPF').AsString = '') then
            FieldByName('Consumidor').AsString := 'CONSUMIDOR NÃO IDENTIFICADO'
          else
            FieldByName('Consumidor').AsString :=
              IfThen(Length(CNPJCPF) = 11, 'CPF: ', 'CNPJ: ') +
              Trim(FieldByName('CNPJCPF').AsString) + ' ' +
              trim(FieldByName('XNome').AsString);
        end;

        if Trim(FieldByName('XLgr').AsString) <> '' then
          FieldByName('Consumidor').AsString :=
            FieldByName('Consumidor').AsString + sLineBreak  +
            Trim(FieldByName('XLgr').AsString) + ', ' +
            Trim(FieldByName('Nro').AsString);
        if Trim(FieldByName('XCpl').AsString) <> '' then
          FieldByName('Consumidor').AsString :=
            FieldByName('Consumidor').AsString + sLineBreak  +
            Trim(FieldByName('XCpl').AsString);

        if Trim(FieldByName('XMun').AsString) <> '' then
          FieldByName('Consumidor').AsString :=
            FieldByName('Consumidor').AsString + sLineBreak  +
            Trim(FieldByName('XBairro').AsString) + ' - ' +
            Trim(FieldByName('XMun').AsString) + '/' +
            Trim(FieldByName('UF').AsString);
      end;
    end;
    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaDuplicatas;
var
  i: integer;
begin
  bdsDuplicatas.Close;
  bdsDuplicatas.CreateDataSet;

  if not (FDANFE.ExibeCampoFatura and (FNFe.Ide.indPag = ipVista) and (FNFe.infNFe.Versao <= 3.10)) then
  begin
    with bdsDuplicatas do
    begin
      for i := 0 to NFe.Cobr.Dup.Count - 1 do
      begin
        Append;
        with FNFe.Cobr.Dup[i] do
        begin
          FieldByName('ChaveNFe').AsString := FNFe.infNFe.ID;
          FieldByName('NDup').AsString := NDup;
          FieldByName('DVenc').AsString := FormatDateBr(DVenc);
          FieldByName('VDup').AsFloat := VDup;
        end;
        Post;
      end;
    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaEmitente;
begin
  { emitente }
  with bdsEmitente do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Emit do
    begin
      FieldByName('CNPJ').AsString := FormatarCNPJ(CNPJCPF);
      FieldByName('XNome').AsString :=
        DANFE.ManterNomeImpresso(XNome, XFant);
      FieldByName('XFant').AsString := XFant;
      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := XMun;
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(CEP);
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
      FieldByName('IM').AsString := IM;
      FieldByName('IEST').AsString := IEST;
      FieldByName('CRT').AsString := CRTToStr(CRT);

      if Trim(FieldByName('CRT').AsString) = '1' then
        FieldByName('DESCR_CST').AsString := 'CSOSN/CST'
      else
        FieldByName('DESCR_CST').AsString := 'CST';

      bdsEmitente.FieldByName('DADOS_ENDERECO').AsString :=
        Trim(FieldByName('XLgr').AsString) + ', ' + Trim(
        FieldByName('Nro').AsString);
      if (trim(FieldByName('XCpl').AsString) <> '') then
        bdsEmitente.FieldByName('DADOS_ENDERECO').AsString :=
          bdsEmitente.FieldByName('DADOS_ENDERECO').AsString + ', ' +
          Trim(FieldByName('XCpl').AsString);

      bdsEmitente.FieldByName('DADOS_ENDERECO').AsString :=
        bdsEmitente.FieldByName('DADOS_ENDERECO').AsString + ' - ' +
        Trim(FieldByName('XBairro').AsString) + ' - ' +
        Trim(FieldByName('XMun').AsString) + ' - ' + Trim(
        FieldByName('UF').AsString) + sLineBreak  + 'Fone: ' +
        Trim(FieldByName('Fone').AsString) +
        ' - CEP: ' + Trim(FieldByName('CEP').AsString);
      if trim(FDANFE.Site) <> '' then
        bdsEmitente.FieldByName('DADOS_ENDERECO').AsString :=
          bdsEmitente.FieldByName('DADOS_ENDERECO').AsString + sLineBreak  +
          trim(FDANFE.Site);
      if trim(FDANFE.Email) <> '' then
        bdsEmitente.FieldByName('DADOS_ENDERECO').AsString :=
          bdsEmitente.FieldByName('DADOS_ENDERECO').AsString + sLineBreak  +
          Trim(FDANFE.Email);
    end;

    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaFatura;
begin
  with bdsFatura do
  begin
    Close;
    CreateDataSet;

    if FDANFE.ExibeCampoFatura then
    begin
      Append;

      FieldByName('iForma').AsInteger := integer(FNFe.Ide.indPag);

      if FNFe.infNFe.Versao >= 4 then
        FieldByName('Pagamento').AsString := 'DADOS DA FATURA'
      else
      begin
        case FNFe.Ide.indPag of
          ipVista: FieldByName('Pagamento').AsString := 'PAGAMENTO À VISTA';
          ipPrazo: FieldByName('Pagamento').AsString := 'PAGAMENTO A PRAZO';
          ipOutras: FieldByName('Pagamento').AsString := 'OUTROS';
        end;
      end;

      if NaoEstaVazio(FNFe.Cobr.Fat.nFat) then
      begin
        with FNFe.Cobr.Fat do
        begin
          FieldByName('nfat').AsString := nFat;
          FieldByName('vOrig').AsFloat := vOrig;
          FieldByName('vDesc').AsFloat := vDesc;
          FieldByName('vLiq').AsFloat := vLiq;
        end;
      end;

      if ((FNFe.infNFe.Versao >= 4) or (FNFe.Ide.indPag = ipOutras)) and
        EstaVazio(FNFe.Cobr.Fat.nFat) then
        Cancel
      else
        Post;

    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaPagamento;
var
  i: integer;
begin
  with bdsPagamento do
  begin
    Close;
    CreateDataSet;
    for i := 0 to NFe.Pag.Count - 1 do
    begin
      Append;
      with FNFe.Pag[i] do
      begin
        FieldByName('tPag').AsString := FormaPagamentoToDescricao(tPag);
        FieldByName('vPag').AsFloat := vPag;
        // ver tpIntegra
        FieldByName('CNPJ').AsString := FormatarCNPJ(CNPJ);
        FieldByName('tBand').AsString := BandeiraCartaoToDescStr(tBand);
        FieldByName('cAut').AsString := cAut;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaIdentificacao;
begin
  with bdsIdentificacao do
  begin
    Close;
    CreateDataSet;
    Append;

    FieldByName('Id').AsString := OnlyNumber(FNFe.infNFe.Id);
    FieldByName('Chave').AsString := FormatarChaveAcesso(FNFe.infNFe.Id);
    FieldByName('CUF').AsString := IntToStr(FNFe.Ide.CUF);
    FieldByName('CNF').AsString := IntToStr(FNFe.Ide.CNF);
    FieldByName('NatOp').AsString := FNFe.Ide.NatOp;
    FieldByName('IndPag').AsString := IndpagToStr(FNFe.Ide.IndPag);
    FieldByName('Mod_').AsString := IntToStr(FNFe.Ide.Modelo);
    FieldByName('Serie').AsString := IntToStr(FNFe.Ide.Serie);
    FieldByName('NNF').AsString :=
      FormatarNumeroDocumentoFiscal(IntToStr(FNFe.Ide.NNF));
    FieldByName('DEmi').AsString := FormatDateBr(FNFe.Ide.DEmi);
    FieldByName('DSaiEnt').AsString :=
      IfThen(FNFe.Ide.DSaiEnt <> 0, FormatDateBr(FNFe.Ide.DSaiEnt));
    FieldByName('TpNF').AsString := tpNFToStr(FNFe.Ide.TpNF);
    FieldByName('CMunFG').AsString := IntToStr(FNFe.Ide.CMunFG);
    FieldByName('TpImp').AsString := TpImpToStr(FNFe.Ide.TpImp);
    FieldByName('TpEmis').AsString := TpEmisToStr(FNFe.Ide.TpEmis);
    FieldByName('CDV').AsString := IntToStr(FNFe.Ide.CDV);
    FieldByName('TpAmb').AsString := TpAmbToStr(FNFe.Ide.TpAmb);
    FieldByName('FinNFe').AsString := FinNFeToStr(FNFe.Ide.FinNFe);
    FieldByName('ProcEmi').AsString := procEmiToStr(FNFe.Ide.ProcEmi);
    FieldByName('VerProc').AsString := FNFe.Ide.VerProc;
    if FNFe.infNFe.versao = 2.00 then
      FieldByName('HoraSaida').AsString :=
        ifthen(FNFe.ide.hSaiEnt = 0, '', TimeToStr(FNFe.ide.hSaiEnt))
    else
      FieldByName('HoraSaida').AsString :=
        ifthen(TimeOf(FNFe.ide.dSaiEnt) = 0, '', TimeToStr(FNFe.ide.dSaiEnt));

    if (FNFe.Ide.Modelo = 65) then
    begin
      FieldByName('DEmi').AsString := FormatDateTimeBr(FNFe.Ide.DEmi);
      if FNFe.Ide.TpAmb = taHomologacao then
        FieldByName('MensagemFiscal').AsString := 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL'
      else
      begin
        if FNFe.Ide.tpEmis <> teNormal then
          FieldByName('MensagemFiscal').AsString :=
            'EMITIDA EM CONTINGÊNCIA' + LineBreak + 'Pendente de autorização';
        //else
        //  FieldByName('MensagemFiscal').AsString := 'ÁREA DE MENSAGEM FISCAL';
      end;

      FieldByName('URL').AsString :=
        TACBrNFe(DANFE.ACBrNFe).GetURLConsultaNFCe(FNFe.Ide.cUF,
        FNFe.Ide.tpAmb, FNFe.infNFe.Versao);
    end
    else
    begin
      FieldByName('MensagemFiscal').AsString := '';
      FieldByName('URL').AsString := '';
    end;
    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaInformacoesAdicionais;
var
  vTemp: TStringList;
  IndexCampo: integer;
  Campos: TSplitResult;
  BufferInfCpl: string;
  wObs: string;
  wLinhasObs: integer;
begin
  wLinhasObs := 0;
  BufferInfCpl := '';
  vTemp := TStringList.Create;

  try
    wObs := FDANFE.ManterDocreferenciados(FNFe) + FDANFE.ManterInfAdFisco(FNFe) +
      FDANFE.ManterObsFisco(FNFe) + FDANFE.ManterProcreferenciado(FNFe) +
      FDANFE.ManterInfContr(FNFe) + FDANFE.ManterInfCompl(FNFe) +
      FDANFE.ManterContingencia(FNFe);

    if Trim(wObs) <> '' then
    begin
      Campos := Split(';', wObs);
      for IndexCampo := 0 to Length(Campos) - 1 do
        vTemp.Add(Campos[IndexCampo]);

      wLinhasObs := 1; //TotalObS(vTemp.Text);
      BufferInfCpl := vTemp.Text;
    end;

    with bdsInformacoesAdicionais do
    begin
      Close;
      CreateDataSet;
      Append;
      FieldByName('OBS').AsString := BufferInfCpl;
      FieldByName('LinhasOBS').AsInteger := wLinhasObs;
      Post;
    end;

  finally
    vTemp.Free;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaISSQN;
begin
  with bdsISSQN do
  begin
    Close;
    CreateDataSet;
    Append;
    with FNFe.Total.ISSQNtot do
    begin
      FieldByName('vSERV').AsFloat := VServ;
      FieldByName('vBC').AsFloat := VBC;
      FieldByName('vISS').AsFloat := VISS;
      FieldByName('vDescIncond').AsFloat := vDescIncond;
	  FieldByName('vISSRet').AsFloat      := vISSRet;
    end;
    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaLocalEntrega;
begin
  { local de entrega }
  with bdsLocalEntrega do
  begin
    Close;
    CreateDataSet;

    if NaoEstaVazio(FNFe.Entrega.xLgr) then
    begin
      Append;

      with FNFe.Entrega do
      begin
        FieldByName('CNPJ').AsString := FormatarCNPJouCPF(CNPJCPF);
        ;
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := XMun;
        FieldByName('UF').AsString := UF;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaLocalRetirada;
begin
  { local de retirada }
  with bdsLocalRetirada do
  begin
    Close;
    CreateDataSet;

    if NaoEstaVazio(FNFe.Retirada.xLgr) then
    begin
      Append;

      with FNFe.Retirada do
      begin
        FieldByName('CNPJ').AsString := FormatarCNPJouCPF(CNPJCPF);
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := XMun;
        FieldByName('UF').AsString := UF;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaParametros;
var
  vChave_Contingencia: string;
  vStream: TMemoryStream;
  vStringStream: TStringStream;
  P: integer;
begin
  { parâmetros }
  with bdsParametros do
  begin
    Close;
    CreateDataSet;
    Append;

    FieldByName('poscanhoto').AsString := IntToStr(Ord(DANFE.PosCanhoto));
    FieldByName('ResumoCanhoto').AsString := '';
    FieldByName('Mensagem0').AsString := '';
    FieldByName('Contingencia_ID').AsString := '';
    FieldByName('ConsultaAutenticidade').AsString :=
      'Consulta de autenticidade no portal nacional da NF-e' + sLineBreak +
      'www.nfe.fazenda.gov.br/portal ou no site da Sefaz autorizadora';

    if Assigned(FNFe) then
    begin
      if DANFE.ExibeResumoCanhoto then
      begin
        if EstaVazio(DANFE.TextoResumoCanhoto) then
          FieldByName('ResumoCanhoto').AsString :=
            'Emissão: ' + FormatDateBr(FNFe.Ide.DEmi) +
            '  Dest/Reme: ' + FNFe.Dest.XNome + '  Valor Total: ' +
            FormatFloatBr(FNFe.Total.ICMSTot.VNF)
        else
          FieldByName('ResumoCanhoto').AsString := DANFE.TextoResumoCanhoto;
      end;

      if (FNFe.Ide.TpAmb = taHomologacao) then
      begin
        if (FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSCAN, teDPEC,
          teSVCAN, teSVCRS, teSVCSP]) then
        begin
          if (FNFe.procNFe.cStat in [101, 135, 151, 155]) then
            FieldByName('Mensagem0').AsString :=
              'NFe sem Valor Fiscal - HOMOLOGAÇÃO ' +
              sLineBreak + 'NFe em Contingência - CANCELADA'
          else
            FieldByName('Mensagem0').AsString :=
              'NFe sem Valor Fiscal - HOMOLOGAÇÃO' +
              sLineBreak + 'NFe em Contingência';
        end
        else
          FieldByName('Mensagem0').AsString :=
            'NFe sem Valor Fiscal - HOMOLOGAÇÃO';
      end
      else
      begin
        if not (FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSVCAN,
          teSVCRS, teSVCSP]) then
        begin
          //prioridade para opção NFeCancelada
          if (FDANFE.Cancelada) or
            ((NaoEstaVazio(FNFe.procNFe.nProt)) and
            (FNFe.procNFe.cStat in [101, 135, 151, 155])) then
            FieldByName('Mensagem0').AsString := 'NFe Cancelada'
          else if (FNFe.procNFe.cStat = 110) or (FNFe.procNFe.cStat = 301) or
            (FNFe.procNFe.cStat = 302) or (FNFe.procNFe.cStat = 303) then
            FieldByName('Mensagem0').AsString := 'NFe denegada pelo Fisco'
          else if ((EstaVazio(FDANFE.Protocolo)) and
            (EstaVazio(FNFe.procNFe.nProt))) then
            FieldByName('Mensagem0').AsString :=
              'NFe sem Autorização de Uso da SEFAZ'
          else if (FNFe.Ide.tpImp = tiSimplificado) then
            FieldByName('Mensagem0').AsString := 'EMISSÃO NORMAL';
        end;
      end;

      case FNFe.Ide.tpEmis of
        teNormal,
        teSVCAN,
        teSCAN,
        teSVCRS,
        teSVCSP:
        begin
          FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
          FieldByName('Contingencia_ID').AsString := '';

          if ((FDANFE.Cancelada) or (FNFe.procNFe.cStat in
            [101, 151, 155])) then
            FieldByName('Contingencia_Descricao').AsString :=
              'PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO'
          else if (FNFe.procNFe.cStat = 110) or (FNFe.procNFe.cStat = 301) or
            (FNFe.procNFe.cStat = 302) or (FNFe.procNFe.cStat = 303) then
            FieldByName('Contingencia_Descricao').AsString :=
              'PROTOCOLO DE DENEGAÇÃO DE USO'
          else
            FieldByName('Contingencia_Descricao').AsString :=
              'PROTOCOLO DE AUTORIZAÇÃO DE USO';

          if EstaVazio(FDANFE.Protocolo) then
          begin
            if EstaVazio(FNFe.procNFe.nProt) then
              FieldByName('Contingencia_Valor').AsString :=
                'NFe sem Autorização de Uso da SEFAZ'
            else
            begin
              FieldByName('Contingencia_Valor').AsString :=
                FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0,
                DateTimeToStr(FNFe.procNFe.dhRecbto), '');
              FieldByName('nProt').AsString := FNFe.procNfe.nProt;
              FieldByName('dhRecbto').AsDateTime := FNFe.procNFe.dhRecbto;
            end;
          end
          else
          begin
            FieldByName('Contingencia_Valor').AsString :=
              FDANFE.Protocolo;
            P := Pos('-', FDANFE.Protocolo);
            if P = 0 then
            begin
              FieldByName('nProt').AsString :=
                Trim(FDANFE.Protocolo);
              FieldByName('dhRecbto').AsDateTime := 0;
            end
            else
            begin
              FieldByName('nProt').AsString :=
                Trim(Copy(FDANFE.Protocolo, 1, P - 1));
              FieldByName('dhRecbto').AsDateTime :=
                StringToDateTimeDef(
                Trim(Copy(FDANFE.Protocolo, P + 1, Length(FDANFE.Protocolo) -
                P)), 0, 'dd/mm/yyyy hh:nn:ss');
            end;
          end;
        end;

        teContingencia,
        teFSDA:
        begin
          vChave_Contingencia :=
            TACBrNFe(DANFE.ACBrNFe).GerarChaveContingencia(FNFe);
          FieldByName('ChaveAcesso_Descricao').AsString :=
            'CHAVE DE ACESSO';
          FieldByName('Contingencia_ID').AsString :=
            vChave_Contingencia;
          FieldByName('Contingencia_Descricao').AsString := 'DADOS DA NF-E';
          FieldByName('Contingencia_Valor').AsString :=
            FormatarChaveAcesso(vChave_Contingencia);
          FieldByName('ConsultaAutenticidade').AsString := '';
        end;

        teDPEC:
        begin
          if NaoEstaVazio(FNFe.procNFe.nProt) then // DPEC TRANSMITIDO
          begin
            FieldByName('Contingencia_Descricao').AsString :=
              'PROTOCOLO DE AUTORIZAÇÃO DE USO';
            FieldByName('Contingencia_Valor').AsString :=
              FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0,
              DateTimeToStr(FNFe.procNFe.dhRecbto), '');
          end
          else
          begin
            FieldByName('Contingencia_Descricao').AsString :=
              'NÚMERO DE REGISTRO DPEC';
            if NaoEstaVazio(FDANFE.Protocolo) then
              FieldByName('Contingencia_Valor').AsString :=
                FDANFE.Protocolo;
          end;
        end;

        teOffLine:
        begin
          FieldByName('Contingencia_Valor').AsString :=
            FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <>
            0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');
          FieldByName('nProt').AsString := FNFe.procNfe.nProt;
          FieldByName('dhRecbto').AsDateTime := FNFe.procNFe.dhRecbto;
        end;
      end;

      FieldByName('QtdeItens').AsInteger := NFe.Det.Count;

    end;

    if NaoEstaVazio(FieldByName('Mensagem0').AsString) then
      FieldByName('Mensagem0').AsString := FieldByName('Mensagem0').AsString + sLineBreak;

    FieldByName('Mensagem0').AsString :=
      FieldByName('Mensagem0').AsString + TACBrNFeDANFeLazReport(FDANFE).MarcaDaguaMSG;
    FieldByName('LogoExpandido').AsString := IfThen(FDANFE.ExpandeLogoMarca, '1', '0');
    FieldByName('Sistema').AsString :=
      IfThen(FDANFE.Sistema <> '', FDANFE.Sistema, 'Projeto ACBr - http://acbr.sf.net');

    FieldByName('Usuario').AsString :=
      IfThen(FDANFE.Usuario <> '', ' - ' + FDANFE.Usuario, '');

    FieldByName('Site').AsString := FDANFE.Site;
    FieldByName('Email').AsString := FDANFE.Email;
    FieldByName('Desconto').AsString :=
      IfThen(FDANFE.ImprimeDescPorPercentual, '%', 'VALOR');
    FieldByName('TotalLiquido').AsString :=
      IfThen(FDANFE.ImprimeTotalLiquido, 'LÍQUIDO', 'TOTAL');
    FieldByName('sDisplayFormat').AsString := ',0.%.*d';
    FieldByName('iFormato').AsInteger :=
      integer(FDANFE.CasasDecimais.Formato);
    FieldByName('Mask_qCom').AsString :=
      FDANFE.CasasDecimais.MaskqCom;
    FieldByName('Mask_vUnCom').AsString :=
      FDANFE.CasasDecimais.MaskvUnCom;
    FieldByName('Casas_qCom').AsInteger :=
      FDANFE.CasasDecimais.qCom;
    FieldByName('Casas_vUnCom').AsInteger :=
      FDANFE.CasasDecimais.vUnCom;

    // Carregamento da imagem
    if NaoEstaVazio(DANFE.Logo) then
    begin
      FieldByName('Imagem').AsString := DANFE.Logo;
      vStream := TMemoryStream.Create;
      try
        if FileExists(DANFE.Logo) then
          vStream.LoadFromFile(DANFE.Logo)
        else
        begin
          vStringStream := TStringStream.Create(DANFE.Logo);
          try
            vStream.LoadFromStream(vStringStream);
          finally
            vStringStream.Free;
          end;
        end;
        vStream.Position := 0;
        TBlobField(bdsParametros.FieldByName('LogoCarregado')).LoadFromStream(vStream);
      finally
        vStream.Free;
      end;
    end;

    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaTransportador;
Var
  ok: Boolean;
begin
  with bdsTransportador do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Transp do
    begin
      FieldByName('ModFrete').AsString := modFreteToDesStr( modFrete, DblToVersaoDF(ok, FNFe.infNFe.Versao));
      with Transporta do
      begin
        FieldByName('CNPJCPF').AsString := FormatarCNPJouCPF(CNPJCPF);
        FieldByName('XNome').AsString := XNome;
        FieldByName('IE').AsString := IE;
        FieldByName('XEnder').AsString := XEnder;
        FieldByName('XMun').AsString := XMun;
        FieldByName('UF').AsString := UF;
      end;
    end;
    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaVeiculo;
begin
  with bdsVeiculo do
  begin
    Close;
    CreateDataSet;
    Append;
    with FNFe.Transp.VeicTransp do
    begin
      FieldByName('PLACA').AsString := Placa;
      FieldByName('UF').AsString := UF;
      FieldByName('RNTC').AsString := RNTC;
    end;

    Post;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaVolumes;
var
  i: integer;
begin
  with bdsVolumes do
  begin
    Close;
    CreateDataSet;
    for i := 0 to NFe.Transp.Vol.Count - 1 do
    begin
      Append;
      with FNFe.Transp.Vol[i] do
      begin
        FieldByName('QVol').AsFloat := QVol;
        FieldByName('Esp').AsString := Esp;
        FieldByName('Marca').AsString := Marca;
        FieldByName('NVol').AsString := NVol;
        FieldByName('PesoL').AsFloat := PesoL;
        FieldByName('PesoB').AsFloat := PesoB;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaDadosEventos;
var
  i: integer;
  CondicoesUso, Correcao: string;
begin
  with bdsEventos do
  begin
    Close;

    FieldDefs.Clear;
    FieldDefs.Add('DescricaoTipoEvento', ftString, 150);
    FieldDefs.Add('Modelo', ftString, 2);
    FieldDefs.Add('Serie', ftString, 3);
    FieldDefs.Add('Numero', ftString, 9);
    FieldDefs.Add('MesAno', ftString, 5);
    FieldDefs.Add('Barras', ftString, 44);
    FieldDefs.Add('ChaveAcesso', ftString, 60);
    FieldDefs.Add('cOrgao', ftInteger);
    FieldDefs.Add('tpAmb', ftString, 100);
    FieldDefs.Add('dhEvento', ftDateTime);
    FieldDefs.Add('TipoEvento', ftString, 6);
    FieldDefs.Add('DescEvento', ftString, 100);
    FieldDefs.Add('nSeqEvento', ftInteger);
    FieldDefs.Add('versaoEvento', ftString, 10);
    FieldDefs.Add('cStat', ftInteger);
    FieldDefs.Add('xMotivo', ftString, 100);
    FieldDefs.Add('nProt', ftString, 20);
    FieldDefs.Add('dhRegEvento', ftDateTime);
    FieldDefs.Add('xJust', ftBlob);
    FieldDefs.Add('xCondUso', ftBlob);
    FieldDefs.Add('xCorrecao', ftBlob);

    CreateDataSet;

    for i := 0 to FEvento.Evento.Count - 1 do
    begin
      Append;

      with Evento.Evento[i] do
      begin
        FieldByName('DescricaoTipoEvento').AsString :=
          InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);

        // nota fiscal eletronica
        FieldByName('Modelo').AsString := Copy(InfEvento.chNFe, 21, 2);
        FieldByName('Serie').AsString := Copy(InfEvento.chNFe, 23, 3);
        FieldByName('Numero').AsString := Copy(InfEvento.chNFe, 26, 9);
        FieldByName('MesAno').AsString :=
          Copy(InfEvento.chNFe, 05, 2) + '/' + copy(InfEvento.chNFe, 03, 2);
        FieldByName('Barras').AsString := InfEvento.chNFe;
        FieldByName('ChaveAcesso').AsString := FormatarChaveAcesso(InfEvento.chNFe);

        // Carta de correção eletrônica
        FieldByName('cOrgao').AsInteger := InfEvento.cOrgao;

        case InfEvento.tpAmb of
          taProducao: FieldByName('tpAmb').AsString := 'PRODUÇÃO';
          taHomologacao: FieldByName('tpAmb').AsString :=
              'HOMOLOGAÇÃO - SEM VALOR FISCAL';
        end;

        FieldByName('dhEvento').AsDateTime := InfEvento.dhEvento;
        FieldByName('TipoEvento').AsString := InfEvento.TipoEvento;
        FieldByName('DescEvento').AsString := InfEvento.DescEvento;
        FieldByName('nSeqEvento').AsInteger := InfEvento.nSeqEvento;
        FieldByName('versaoEvento').AsString := InfEvento.versaoEvento;
        FieldByName('cStat').AsInteger := RetInfEvento.cStat;
        FieldByName('xMotivo').AsString := RetInfEvento.xMotivo;
        FieldByName('nProt').AsString := RetInfEvento.nProt;
        FieldByName('dhRegEvento').AsDateTime := RetInfEvento.dhRegEvento;

        if InfEvento.tpEvento <> teCCe then
        begin
          FieldByName('xJust').AsString := InfEvento.detEvento.xJust;
        end
        else
        begin
          CondicoesUso := InfEvento.detEvento.xCondUso;
          CondicoesUso := StringReplace(CondicoesUso, 'com: I',
            'com:' + sLineBreak  + ' I', [rfReplaceAll]);
          CondicoesUso := StringReplace(CondicoesUso, ';', ';' + sLineBreak , [rfReplaceAll]);

          Correcao := InfEvento.detEvento.xCorrecao;
          Correcao := StringReplace(InfEvento.detEvento.xCorrecao,
            ';', sLineBreak , [rfReplaceAll]);

          FieldByName('xCondUso').AsString := CondicoesUso;
          FieldByName('xCorrecao').AsString := Correcao;
        end;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeLazReportClass.CarregaDadosInutilizacao;
begin
  CarregaParametros;

  with bdsInutilizacao do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('ID', ftString, 44);
    FieldDefs.Add('CNPJ', ftString, 20);
    FieldDefs.Add('nProt', ftString, 20);
    FieldDefs.Add('Modelo', ftInteger);
    FieldDefs.Add('Serie', ftInteger);
    FieldDefs.Add('Ano', ftInteger);
    FieldDefs.Add('nNFIni', ftInteger);
    FieldDefs.Add('nNFFin', ftInteger);
    FieldDefs.Add('xJust', ftString, 50);
    FieldDefs.Add('versao', ftString, 20);
    FieldDefs.Add('TpAmb', ftString, 32);
    FieldDefs.Add('verAplic', ftString, 20);
    FieldDefs.Add('cStat', ftInteger);
    FieldDefs.Add('xMotivo', ftString, 50);
    FieldDefs.Add('cUF', ftString, 2);
    FieldDefs.Add('dhRecbto', ftDateTime);
    CreateDataSet;

    Append;

    with FInutilizacao do
    begin
      FieldByName('ID').AsString := OnlyNumber(ID);
      FieldByName('CNPJ').AsString := FormatarCNPJ(CNPJ);
      FieldByName('nProt').AsString := nProt;
      FieldByName('Modelo').AsInteger := Modelo;
      FieldByName('Serie').AsInteger := Serie;
      FieldByName('Ano').AsInteger := Ano;
      FieldByName('nNFIni').AsInteger := nNFIni;
      FieldByName('nNFFin').AsInteger := nNFFin;
      FieldByName('xJust').AsString := xJust;
      FieldByName('versao').AsString := versao;
      FieldByName('verAplic').AsString := verAplic;
      FieldByName('cStat').AsInteger := cStat;
      FieldByName('xMotivo').AsString := xMotivo;
      FieldByName('dhRecbto').AsDateTime := dhRecbto;
      FieldByName('cUF').AsString := CUFtoUF(cUF);

      case tpAmb of
        taProducao: FieldByName('tpAmb').AsString := 'PRODUÇÃO';
        taHomologacao: FieldByName('tpAmb').AsString :=
            'HOMOLOGAÇÃO - SEM VALOR FISCAL';
      end;

      Post;
    end;
  end;
end;

end.
