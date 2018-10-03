{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
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
|* 11/08/2010: Itamar Luiz Bermond
|*  - Inicio do desenvolvimento
|* 24/08/2010: Régys Silveira
|*  - Acerto da exportação para PDF
|*  - Acerto para checar se o relatório foi realmente preparado
|     antes de continuar a imprir ou gerar o PDF
|*  - Acerto nas propriedades do arquivo PDF
|* 26/08/2010: Régys Silveira / Itamar Bermond
|*  - Acerto na propriedade "PreparedReport"
******************************************************************************}
{$I ACBr.inc}

unit ACBrNFeDANFEFR;

interface

uses
  SysUtils, Classes, Forms, ACBrNFeDANFEClass, ACBrNFeDANFEFRDM,
  pcnNFe, pcnConversao, frxClass;

type
  EACBrNFeDANFEFR = class(Exception);
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrNFeDANFEFR = class( TACBrNFeDANFEClass )
   private
    FdmDanfe: TACBrNFeFRClass;
    FFastFile: String;
    FEspessuraBorda: Integer;
    FFastFileEvento: String;
    FShowDialog: Boolean;
    FExibirTotalTributosItem: Boolean;
    FExibeCampoFatura: Boolean;
    FTributosFonte: string;
    FTributosPercentual: TpcnPercentualTributos;
    FTributosPercentualPersonalizado: double;
    FMarcaDaguaMSG: string;
    FDetalhado: Boolean;
    FURLConsultaPublica:String;
    FDescricaoViaEstabelec: string;
    FImprimirUnQtVlComercial: TImprimirUnidQtdeValor;
    FExpandirDadosAdicionaisAuto: boolean;
    FImprimirDadosArma: Boolean;
    fQuebraLinhaEmDetalhamentoEspecifico : Boolean;
    FIncorporarFontesPdf: Boolean;
    FIncorporarBackgroundPdf: Boolean;
    FFastFileInutilizacao: String;
    FImprimirDadosDocReferenciados: Boolean;
    FPrintMode: TfrxPrintMode;
    FPrintOnSheet: Integer;
    FBorderIcon : TBorderIcons;
    FExibeCaptionButton: Boolean;

    function GetPreparedReport: TfrxReport;
    function GetPreparedReportEvento: TfrxReport;
		function GetPreparedReportInutilizacao: TfrxReport;
    function PrepareReport(NFE: TNFe = nil): Boolean;
    function PrepareReportEvento: Boolean;
    function PrepareReportInutilizacao: Boolean;
    procedure setTributosPercentual(const Value: TpcnPercentualTributos);
    procedure setTributosPercentualPersonalizado(const Value: double);
    procedure frxReportPreview(Sender: TObject);
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
    property PreparedReport: TfrxReport read GetPreparedReport;
    property PreparedReportEvento: TfrxReport read GetPreparedReportEvento;
    property PreparedReportInutilizacao: TfrxReport read GetPreparedReportInutilizacao;
  published
    property FastFile: String read FFastFile write FFastFile;
    property FastFileEvento: String read FFastFileEvento write FFastFileEvento;
    property FastFileInutilizacao: String read FFastFileInutilizacao write FFastFileInutilizacao;
    property dmDanfe: TACBrNFeFRClass read FdmDanfe write FdmDanfe;
    property EspessuraBorda: Integer read FEspessuraBorda write FEspessuraBorda;
    property ShowDialog: Boolean read FShowDialog write FShowDialog default false;
    property ExibirTotalTributosItem: Boolean read FExibirTotalTributosItem write FExibirTotalTributosItem;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura;
    property TributosFonte: string read FTributosFonte write FTributosFonte;
    property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write setTributosPercentual;
    property TributosPercentualPersonalizado: double read FTributosPercentualPersonalizado write setTributosPercentualPersonalizado;
    property MarcaDaguaMSG: string read FMarcaDaguaMSG write FMarcaDaguaMSG;
    property ImprimirUnQtVlComercial: TImprimirUnidQtdeValor read FImprimirUnQtVlComercial write FImprimirUnQtVlComercial;
    property Detalhado: Boolean read FDetalhado write FDetalhado;
    property URLConsultaPublica:String read FURLConsultaPublica write FURLConsultaPublica;
    property DescricaoViaEstabelec: string read FDescricaoViaEstabelec write FDescricaoViaEstabelec;
    property ExpandirDadosAdicionaisAuto: boolean read FExpandirDadosAdicionaisAuto write FExpandirDadosAdicionaisAuto;
    property ImprimirDadosArma: Boolean read FImprimirDadosArma write FImprimirDadosArma;
    property QuebraLinhaEmDetalhamentoEspecifico : Boolean  read fQuebraLinhaEmDetalhamentoEspecifico Write fQuebraLinhaEmDetalhamentoEspecifico;
    property IncorporarBackgroundPdf: Boolean read FIncorporarBackgroundPdf write FIncorporarBackgroundPdf;
    property IncorporarFontesPdf: Boolean read FIncorporarFontesPdf write FIncorporarFontesPdf;
    property ImprimirDadosDocReferenciados: Boolean read FImprimirDadosDocReferenciados write FImprimirDadosDocReferenciados;
    property PrintMode: TfrxPrintMode read FPrintMode write FPrintMode default pmDefault;
    property PrintOnSheet: Integer read FPrintOnSheet write FPrintOnSheet default 0;
    property BorderIcon: TBorderIcons read FBorderIcon write FBorderIcon;
    property ExibeCaptionButton: Boolean read FExibeCaptionButton write FExibeCaptionButton default False;

  end;

implementation

uses ACBrNFe, ACBrUtil, StrUtils, pcnConversaoNFe;

constructor TACBrNFeDANFEFR.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FdmDanfe := TACBrNFeFRClass.Create(Self);
  FFastFile := '' ;
  FEspessuraBorda := 1;
  FExibirTotalTributosItem := False;
  FExibeCampoFatura := True;
  FTributosFonte := '';
  FTributosPercentual := ptValorProdutos;
  FTributosPercentualPersonalizado := 0;
  FMarcaDaguaMSG:='';
  FImprimirUnQtVlComercial:=iuComercial;
  ExpandirDadosAdicionaisAuto:=false;
  { NFC-e }
  FvTroco := 0;
  FDetalhado := False;
  FDescricaoViaEstabelec := 'Via do Consumidor';// utilizado para NFC-e
  FURLConsultaPublica:= ''; //NFCe
  FImprimirDadosArma := True;
  fQuebraLinhaEmDetalhamentoEspecifico  := True;
  FIncorporarFontesPdf := True;
  FIncorporarBackgroundPdf := True;
  FImprimirDadosDocReferenciados := True;
  FBorderIcon := [biSystemMenu,biMaximize,biMinimize];
  FExibeCaptionButton := False;
end;

destructor TACBrNFeDANFEFR.Destroy;
begin
  FdmDanfe.Free;
  inherited Destroy;
end;

function TACBrNFeDANFEFR.GetPreparedReport: TfrxReport;
begin
  if Trim(FFastFile) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := FdmDanfe.frxReport
    else
      Result := nil;
  end;
end;

function TACBrNFeDANFEFR.GetPreparedReportEvento: TfrxReport;
begin
  if Trim(FFastFileEvento) = '' then
    Result := nil
  else
  begin
    if PrepareReportEvento then
      Result := FdmDanfe.frxReport
    else
      Result := nil;
  end;
end;

function TACBrNFeDANFEFR.GetPreparedReportInutilizacao: TfrxReport;
begin
  if Trim(FFastFileInutilizacao) = '' then
    Result := nil
  else
  begin
    if PrepareReportInutilizacao then
      Result := FdmDanfe.frxReport
    else
      Result := nil;
  end;
end;

function TACBrNFeDANFEFR.PrepareReport(NFE: TNFe): Boolean;
var
  I: Integer;
  wProjectStream: TStringStream;
  Page: TfrxReportPage;
begin
  Result := False;

  if ViaConsumidor then
    FDescricaoViaEstabelec := 'Via Consumidor'
  else
    FDescricaoViaEstabelec := 'Via Estabelecimento';

  FdmDanfe.ExibirTotalTributosItem := FExibirTotalTributosItem;
  FdmDanfe.ExibeCampoFatura :=  FExibeCampoFatura;
  FdmDanfe.TributosFonte :=  FTributosFonte;
  FdmDanfe.TributosPercentual :=  FTributosPercentual;
  FdmDanfe.TributosPercentualPersonalizado :=  FTributosPercentualPersonalizado;
  FdmDanfe.MarcaDaguaMSG :=  FMarcaDaguaMSG;
  FdmDanfe.ImprimirUnQtVlComercial := FImprimirUnQtVlComercial;
  FdmDanfe.ExpandirDadosAdicionaisAuto := FExpandirDadosAdicionaisAuto;
  FdmDanfe.vTroco := FvTroco;
  FdmDanfe.Detalhado := FDetalhado;
  FdmDanfe.DescricaoViaEstabelec := FDescricaoViaEstabelec;
  FdmDanfe.URLConsultaPublica    := FURLConsultaPublica;
  FdmDanfe.ImprimirDadosArma := FImprimirDadosArma;
  FdmDanfe.QuebraLinhaEmDetalhamentoEspecifico := fQuebraLinhaEmDetalhamentoEspecifico;
  FdmDanfe.IncorporarBackgroundPdf := FIncorporarFontesPdf;
  FdmDanfe.IncorporarFontesPdf := FIncorporarBackgroundPdf;
  FdmDanfe.ImprimirDadosDocReferenciados := FImprimirDadosDocReferenciados;

  FdmDanfe.SetDataSetsToFrxReport;
  if Trim(FastFile) <> '' then
  begin
    if not (uppercase(copy(FastFile,length(FastFile)-3,4))='.FR3') then
    begin
      wProjectStream:=TStringStream.Create(FastFile);
      FdmDanfe.frxReport.FileName := '';
      FdmDanfe.frxReport.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(FastFile) then
        FdmDanfe.frxReport.LoadFromFile(FastFile)
      else
        raise EACBrNFeDANFEFR.CreateFmt('Caminho do arquivo de impressão do DANFE "%s" inválido.', [FastFile]);
    end;
  end
  else
    raise EACBrNFeDANFEFR.Create('Caminho do arquivo de impressão do DANFE não assinalado.');

  FdmDanfe.frxReport.PrintOptions.Copies := FNumCopias;
  FdmDanfe.frxReport.PrintOptions.ShowDialog := FShowDialog;
  FdmDanfe.frxReport.PrintOptions.PrintMode := FPrintMode; //Precisamos dessa propriedade porque impressoras não fiscais cortam o papel quando há muitos itens. O ajuste dela deve ser necessariamente após a carga do arquivo FR3 pois, antes da carga o componente é inicializado
  FdmDanfe.frxReport.PrintOptions.PrintOnSheet := FPrintOnSheet; //Essa propriedade pode trabalhar em conjunto com a printmode
  FdmDanfe.frxReport.ShowProgress := FMostrarStatus;
  FdmDanfe.frxReport.PreviewOptions.AllowEdit := False;
  FdmDanfe.frxReport.PreviewOptions.ShowCaptions := FExibeCaptionButton;
  FdmDanfe.frxReport.OnPreview := frxReportPreview;


  // Define a impressora
  if Length(Impressora) > 0 then
    FdmDanfe.frxReport.PrintOptions.Printer := FImpressora;

  // preparar relatorio
  if Assigned(NFE) then
  begin
    FdmDanfe.NFe := NFE;
    FdmDanfe.CarregaDadosNFe;

    Result := FdmDanfe.frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrNFe) then
    begin
      for i := 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count - 1 do
      begin
        FdmDanfe.NFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe;
        FdmDanfe.CarregaDadosNFe;

        if (i > 0) then
          Result := FdmDanfe.frxReport.PrepareReport(False)
        else
          Result := FdmDanfe.frxReport.PrepareReport;
      end;
    end
    else
      raise EACBrNFeDANFEFR.Create('Propriedade ACBrNFe não assinalada.');
  end;

  if Assigned(FdmDanfe.NFe) and
    (FdmDanfe.NFe.Ide.modelo = 55) then
//  if(TACBrNFe(ACBrNFe).Configuracoes.Geral.ModeloDF = moNFe)then
    for i := 0 to FdmDanfe.frxReport.PreviewPages.Count - 1 do
    begin
      Page := FdmDanfe.frxReport.PreviewPages.Page[i];
      if MargemSuperior > 0 then
        Page.TopMargin    := MargemSuperior * 10;
      if MargemInferior > 0 then
        Page.BottomMargin := MargemInferior * 10;
      if MargemEsquerda > 0 then
        Page.LeftMargin   := MargemEsquerda * 10;
      if MargemDireita > 0 then
        Page.RightMargin  := MargemDireita * 10;
      FdmDanfe.frxReport.PreviewPages.ModifyPage(i, Page);
    end;
end;

function TACBrNFeDANFEFR.PrepareReportEvento: Boolean;
var
 wProjectStream: TStringStream;
begin
  FdmDanfe.IncorporarBackgroundPdf := FIncorporarFontesPdf;
  FdmDanfe.IncorporarFontesPdf := FIncorporarBackgroundPdf;

  FdmDanfe.SetDataSetsToFrxReport;
  if Trim(FastFileEvento) <> '' then
  begin
    if not (uppercase(copy(FastFileEvento,length(FastFileEvento)-3,4))='.FR3') then
    begin
      wProjectStream:=TStringStream.Create(FastFileEvento);
      FdmDanfe.frxReport.FileName := '';
      FdmDanfe.frxReport.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(FastFileEvento) then
        FdmDanfe.frxReport.LoadFromFile(FastFileEvento)
      else
        raise EACBrNFeDANFEFR.CreateFmt('Caminho do arquivo de impressão do EVENTO "%s" inválido.', [FastFileEvento]);
    end
  end
  else
    raise EACBrNFeDANFEFR.Create('Caminho do arquivo de impressão do EVENTO não assinalado.');

  FdmDanfe.frxReport.PrintOptions.Copies := NumCopias;
  FdmDanfe.frxReport.PrintOptions.ShowDialog := ShowDialog;
  FdmDanfe.frxReport.ShowProgress := FMostrarStatus;
  FdmDanfe.frxReport.PreviewOptions.ShowCaptions := FExibeCaptionButton;
  FdmDanfe.frxReport.OnPreview := frxReportPreview;

  // Define a impressora
  if Length(Impressora) > 0 then
    FdmDanfe.frxReport.PrintOptions.Printer := FImpressora;
  // preparar relatorio
  if Assigned(ACBrNFe) then
  begin
    if assigned(TACBrNFe(ACBrNFe).EventoNFe) then
    begin
      FdmDanfe.Evento := TACBrNFe(ACBrNFe).EventoNFe;
      FdmDanfe.CarregaDadosEventos;
    end
    else
      raise EACBrNFeDANFEFR.Create('Evento não foi assinalado.');

    if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then
    begin
      FdmDanfe.frxReport.Variables['PossuiNFe'] := QuotedStr('S');
      FdmDanfe.NFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
      FdmDanfe.CarregaDadosNFe;
    end;

    Result := FdmDanfe.frxReport.PrepareReport;
  end
  else
    raise EACBrNFeDANFEFR.Create('Propriedade ACBrNFe não assinalada.');
end;

function TACBrNFeDANFEFR.PrepareReportInutilizacao: Boolean;
var
 wProjectStream: TStringStream;
begin
  FdmDanfe.SetDataSetsToFrxReport;
  if Trim(FastFileInutilizacao) <> '' then
  begin
    if not (uppercase(copy(FastFileInutilizacao,length(FastFileInutilizacao)-3,4))='.FR3') then
    begin
      wProjectStream:=TStringStream.Create(FastFileInutilizacao);
      FdmDanfe.frxReport.FileName := '';
      FdmDanfe.frxReport.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(FastFileInutilizacao) then
        FdmDanfe.frxReport.LoadFromFile(FastFileInutilizacao)
      else
        raise EACBrNFeDANFEFR.CreateFmt('Caminho do arquivo de impressão de INUTILIZAÇÃO "%s" inválido.', [FastFileInutilizacao]);
    end
  end
  else
    raise EACBrNFeDANFEFR.Create('Caminho do arquivo de impressão de INUTILIZAÇÃO não assinalado.');

  FdmDanfe.frxReport.PrintOptions.Copies := NumCopias;
  FdmDanfe.frxReport.PreviewOptions.ShowCaptions := FExibeCaptionButton;
  FdmDanfe.frxReport.OnPreview := frxReportPreview;

  // preparar relatorio
  if Assigned(ACBrNFe) then
  begin
    if assigned(TACBrNFe(ACBrNFe).InutNFe) then
    begin
      FdmDanfe.Inutilizacao := TACBrNFe(ACBrNFe).InutNFe.RetInutNFe;
      FdmDanfe.CarregaDadosInutilizacao;
    end
    else
      raise EACBrNFeDANFEFR.Create('INUTILIZAÇÃO não foi assinalada.');

    Result := FdmDanfe.frxReport.PrepareReport;
  end
  else
    raise EACBrNFeDANFEFR.Create('Propriedade ACBrNFe não assinalada.');

end;

procedure TACBrNFeDANFEFR.setTributosPercentual(
  const Value: TpcnPercentualTributos);
begin
  FTributosPercentual := Value;
  if Value <> ptPersonalizado then
    FTributosPercentualPersonalizado := 0;
end;

procedure TACBrNFeDANFEFR.setTributosPercentualPersonalizado(
  const Value: double);
begin
  if FTributosPercentual=ptPersonalizado then
    FTributosPercentualPersonalizado := Value
  else
    FTributosPercentualPersonalizado := 0;
end;

procedure TACBrNFeDANFEFR.frxReportPreview(Sender: TObject);
begin
 FdmDanfe.frxReport.PreviewForm.BorderIcons := FBorderIcon;
end;

procedure TACBrNFeDANFEFR.ImprimirDANFE(NFE: TNFe);
begin
  FDetalhado := True;

  if PrepareReport(NFE) then
  begin
    if MostrarPreview then
      FdmDanfe.frxReport.ShowPreparedReport
    else
      FdmDanfe.frxReport.Print;
  end;
end;

procedure TACBrNFeDANFEFR.ImprimirDANFEResumido(NFE: TNFe);
begin
  FDetalhado := False;

  if PrepareReport(NFE) then
  begin
    if MostrarPreview then
      FdmDanfe.frxReport.ShowPreparedReport
    else
      FdmDanfe.frxReport.Print;
  end;
end;

procedure TACBrNFeDANFEFR.ImprimirDANFEPDF(NFE: TNFe);
const
  TITULO_PDF = 'Nota Fiscal Eletrônica';
var
	fsShowDialog : Boolean;
begin
  if PrepareReport(NFE) then
  begin
		with FdmDanfe do
		begin
			fsShowDialog := frxPDFExport.ShowDialog;
			frxPDFExport.Author        := Sistema;
			frxPDFExport.Creator       := Sistema;
			frxPDFExport.Producer      := Sistema;
			frxPDFExport.Title         := TITULO_PDF;
			frxPDFExport.Subject       := TITULO_PDF;
			frxPDFExport.Keywords      := TITULO_PDF;
			frxPDFExport.ShowDialog    := False;
			frxPDFExport.EmbeddedFonts := False;
			frxPDFExport.Background    := False;

			frxPDFExport.FileName := PathWithDelim(Self.PathPDF) +	OnlyNumber(NFe.infNFe.ID) + '-nfe.pdf';
            Self.FPArquivoPDF := frxPDFExport.FileName;
	
			if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
				ForceDirectories(ExtractFileDir(frxPDFExport.FileName));
	
			frxReport.Export(frxPDFExport);
			frxPDFExport.ShowDialog := fsShowDialog;
    end;		
  end;
end;

procedure TACBrNFeDANFEFR.ImprimirEVENTO(NFE: TNFe);
begin
  if PrepareReportEvento then
  begin
    if MostrarPreview then
      FdmDanfe.frxReport.ShowPreparedReport
    else
      FdmDanfe.frxReport.Print;
  end;
end;

procedure TACBrNFeDANFEFR.ImprimirEVENTOPDF(NFE: TNFe);
const
  TITULO_PDF = 'Eventos Nota Fiscal Eletrônica';
var
  NomeArq: String;
begin
  if PrepareReportEvento then
  begin
    FdmDanfe.frxPDFExport.Author     := Sistema;
    FdmDanfe.frxPDFExport.Creator    := Sistema;
    FdmDanfe.frxPDFExport.Producer   := Sistema;
    FdmDanfe.frxPDFExport.Title      := TITULO_PDF;
    FdmDanfe.frxPDFExport.Subject    := TITULO_PDF;
    FdmDanfe.frxPDFExport.Keywords   := TITULO_PDF;
    FdmDanfe.frxPDFExport.ShowDialog := False;

    NomeArq := StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[0].InfEvento.id, 'ID', '', [rfIgnoreCase]);

    FdmDanfe.frxPDFExport.FileName := PathWithDelim(Self.PathPDF) + NomeArq + '-procEventoNFe.pdf';
    Self.FPArquivoPDF := FdmDanfe.frxPDFExport.FileName;

    if not DirectoryExists(ExtractFileDir(FdmDanfe.frxPDFExport.FileName)) then
      ForceDirectories(ExtractFileDir(FdmDanfe.frxPDFExport.FileName));

    FdmDanfe.frxReport.Export(FdmDanfe.frxPDFExport);
  end;
end;

procedure TACBrNFeDANFEFR.ImprimirINUTILIZACAO(NFE: TNFe);
begin
  if PrepareReportInutilizacao then
  begin
    if MostrarPreview then
      FdmDanfe.frxReport.ShowPreparedReport
    else
      FdmDanfe.frxReport.Print;
  end;
end;

procedure TACBrNFeDANFEFR.ImprimirINUTILIZACAOPDF(NFE: TNFe);
const
  TITULO_PDF = 'Inutilização de Numeração';
var
  NomeArq: String;
begin
  if PrepareReportInutilizacao then
  begin
    FdmDanfe.frxPDFExport.Author     := Sistema;
    FdmDanfe.frxPDFExport.Creator    := Sistema;
    FdmDanfe.frxPDFExport.Producer   := Sistema;
    FdmDanfe.frxPDFExport.Title      := TITULO_PDF;
    FdmDanfe.frxPDFExport.Subject    := TITULO_PDF;
    FdmDanfe.frxPDFExport.Keywords   := TITULO_PDF;
    FdmDanfe.frxPDFExport.ShowDialog := False;

    NomeArq := OnlyNumber(TACBrNFe(ACBrNFe).InutNFe.RetInutNFe.Id);

    FdmDanfe.frxPDFExport.FileName := PathWithDelim(Self.PathPDF) + NomeArq + '-procInutNFe.pdf';
    Self.FPArquivoPDF := FdmDanfe.frxPDFExport.FileName;

    if not DirectoryExists(ExtractFileDir(FdmDanfe.frxPDFExport.FileName)) then
      ForceDirectories(ExtractFileDir(FdmDanfe.frxPDFExport.FileName));

    FdmDanfe.frxReport.Export(FdmDanfe.frxPDFExport);
  end;
end;

end.
