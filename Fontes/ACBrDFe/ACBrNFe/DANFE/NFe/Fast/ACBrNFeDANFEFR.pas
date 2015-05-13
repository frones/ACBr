{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                          }
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
  Forms, SysUtils, Classes, Graphics, ACBrNFeDANFEClass, ACBrNFeDANFEFRDM,
  pcnNFe, pcnConversao, frxClass;

type
  EACBrNFeDANFEFR = class(Exception);

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
    FvTroco: Currency;
    FDetalhado: Boolean;
    FURLConsultaPublica:String;
    FDescricaoViaEstabelec: string;
    FImprimirUnQtVlComercial: boolean;
    FExpandirDadosAdicionaisAuto: boolean;
    FImprimirDadosArma: Boolean;
    function GetPreparedReport: TfrxReport;
    function GetPreparedReportEvento: TfrxReport;
    function PrepareReport(NFE: TNFe = nil): Boolean;
    function PrepareReportEvento: Boolean;
    procedure setTributosPercentual(const Value: TpcnPercentualTributos);
    procedure setTributosPercentualPersonalizado(const Value: double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFE(NFE: TNFe = nil); override;
    procedure ImprimirDANFEResumido(NFE: TNFe = nil); override;
    procedure ImprimirDANFEPDF(NFE: TNFe = nil); override;
    procedure ImprimirEVENTO(NFE: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(NFE: TNFe = nil); override;
  published
    property FastFile: String read FFastFile write FFastFile;
    property FastFileEvento: String read FFastFileEvento write FFastFileEvento;
    property dmDanfe: TACBrNFeFRClass read FdmDanfe write FdmDanfe;
    property EspessuraBorda: Integer read FEspessuraBorda write FEspessuraBorda;
    property PreparedReport: TfrxReport read GetPreparedReport;
    property PreparedReportEvento: TfrxReport read GetPreparedReportEvento;
    property ShowDialog: Boolean read FShowDialog write FShowDialog default false; // Isaque Pinheiro
    property ExibirTotalTributosItem: Boolean read FExibirTotalTributosItem write FExibirTotalTributosItem;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura;  //Incluido em 22/05/2013 - Fábio Gabriel
    property TributosFonte: string read FTributosFonte write FTributosFonte;
    property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write setTributosPercentual;
    property TributosPercentualPersonalizado: double read FTributosPercentualPersonalizado write setTributosPercentualPersonalizado;
    property MarcaDaguaMSG: string read FMarcaDaguaMSG write FMarcaDaguaMSG;
    property ImprimirUnQtVlComercial: boolean read FImprimirUnQtVlComercial write FImprimirUnQtVlComercial;
    property vTroco: Currency read FvTroco write FvTroco;
    property Detalhado: Boolean read FDetalhado write FDetalhado;
    property URLConsultaPublica:String read FURLConsultaPublica write FURLConsultaPublica;
    property DescricaoViaEstabelec: string read FDescricaoViaEstabelec write FDescricaoViaEstabelec;
    property ExpandirDadosAdicionaisAuto: boolean read FExpandirDadosAdicionaisAuto write FExpandirDadosAdicionaisAuto;
    property ImprimirDadosArma: Boolean read FImprimirDadosArma write FImprimirDadosArma;
  end;

implementation

uses ACBrNFe, ACBrNFeUtil, ACBrUtil, StrUtils, Dialogs;

constructor TACBrNFeDANFEFR.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FdmDanfe := TACBrNFeFRClass.Create(Self);
  FFastFile := '' ;
  FEspessuraBorda := 1;
  FExibirTotalTributosItem := False;
  FExibeCampoFatura := True;  //Incluido em 22/05/2013 - Fábio Gabriel - Setado por padrão True
  FTributosFonte := '';
  FTributosPercentual := ptValorProdutos;
  FTributosPercentualPersonalizado := 0;
  FMarcaDaguaMSG:='';
  FImprimirUnQtVlComercial:=false;
  ExpandirDadosAdicionaisAuto:=false;
  { NFC-e }
  FvTroco := 0;
  FDetalhado := False;
  FDescricaoViaEstabelec := 'Via do Consumidor';// utilizado para NFC-e
  FURLConsultaPublica:= ''; //NFCe
  FImprimirDadosArma := True;
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

function TACBrNFeDANFEFR.PrepareReport(NFE: TNFe): Boolean;
var
  I: Integer;
 wProjectStream: TStringStream;
begin
  Result := False;

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
  FdmDanfe.frxReport.ShowProgress := FMostrarStatus;

  if Assigned(ACBrNFe) then
   if(TACBrNFe(ACBrNFe).Configuracoes.Geral.ModeloDF = moNFCe)then
     FdmDanfe.frxReport.PrintOptions.PrintMode := pmSplit; 

  // Incluído por Luciano Enzweiler em 23/01/2013
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
end;

function TACBrNFeDANFEFR.PrepareReportEvento: Boolean;
begin
  if Trim(FastFileEvento) <> '' then
  begin
    if FileExists(FastFileEvento) then
      FdmDanfe.frxReport.LoadFromFile(FastFileEvento)
    else
      raise EACBrNFeDANFEFR.CreateFmt('Caminho do arquivo de impressão do EVENTO "%s" inválido.', [FastFileEvento]);
  end
  else
    raise EACBrNFeDANFEFR.Create('Caminho do arquivo de impressão do EVENTO não assinalado.');

  FdmDanfe.frxReport.PrintOptions.Copies := NumCopias;

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
  I: Integer;
	fsShowDialog : Boolean;
begin
  if PrepareReport(NFE) then
  begin
    fsShowDialog := FdmDanfe.frxPDFExport.ShowDialog;
    FdmDanfe.frxPDFExport.Author     := Sistema;
    FdmDanfe.frxPDFExport.Creator    := Sistema;
    FdmDanfe.frxPDFExport.Producer   := Sistema;
    FdmDanfe.frxPDFExport.Title      := TITULO_PDF;
    FdmDanfe.frxPDFExport.Subject    := TITULO_PDF;
    FdmDanfe.frxPDFExport.Keywords   := TITULO_PDF;
    FdmDanfe.frxPDFExport.ShowDialog := False;

    for I := 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count - 1 do
    begin
      FdmDanfe.frxPDFExport.FileName := PathPDF +
                                       StringReplace(UpperCase(FdmDanfe.NFe.infNFe.ID),'NFE','', [rfReplaceAll, rfIgnoreCase]) +
                                       '-nfe.pdf';
      FdmDanfe.frxReport.Export(FdmDanfe.frxPDFExport);
    end;
		FdmDanfe.frxPDFExport.ShowDialog := fsShowDialog;
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

    {
    NomeArq := TACBrNFe(ACBrNFe).EventoNFe.Evento[0].InfEvento.chNFe;
    NomeArq := NomeArq + '-' + TACBrNFe(ACBrNFe).EventoNFe.Evento[0].InfEvento.TipoEvento;
    NomeArq := NomeArq + '-' + IntToStr(TACBrNFe(ACBrNFe).EventoNFe.Evento[0].InfEvento.nSeqEvento);
    }
    {
    NomeArq := TACBrNFe(ACBrNFe).EventoNFe.Evento[0].InfEvento.TipoEvento;
    NomeArq := NomeArq + TACBrNFe(ACBrNFe).EventoNFe.Evento[0].InfEvento.chNFe;
    }

//    NomeArq := Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[0].InfEvento.id, 3, 52);

    NomeArq := StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[0].InfEvento.id, 'ID', '', [rfIgnoreCase]);

    FdmDanfe.frxPDFExport.FileName := PathWithDelim(Self.PathPDF) + NomeArq + '-procEventoNFe.pdf';
    FdmDanfe.frxReport.Export(FdmDanfe.frxPDFExport);
  end;
end;

end.
