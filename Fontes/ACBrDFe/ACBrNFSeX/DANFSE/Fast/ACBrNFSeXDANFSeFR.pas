  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
  {                                                                              }
  { Colaboradores nesse arquivo: Juliomar Marchetti                              }
  {                              Victor Hugo Gonzales - Pandaaa                  }
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
unit ACBrNFSeXDANFSeFR;

interface

uses
  SysUtils,
  Classes,
  ACBrBase,
  ACBrNFSeX,
  ACBrNFSeXDANFSeClass,
  ACBrNFSeXClass,
  frxClass,
  DB,
  frxDBSet,
  frxExportPDF,
  frxBarcode,
  ACBrValidador,
  ACBrUtil.FR;

type
  EACBrNFSeXDANFSeFR = class(Exception);
{$IFDEF RTL230_UP}
  [ ComponentPlatformsAttribute(piacbrAllPlatforms) ]
{$ENDIF RTL230_UP}

  TACBrNFSeXDANFSeFR = class(TACBrNFSeXDANFSeClass)
  private
    FFastFile         : String;
    FEspessuraBorda   : Integer;
    FDANFSeXClassOwner: TACBrNFSeXDANFSeClass;
    function GetPreparedReport: TfrxReport;
    function PrepareReport(ANFSe: TNFSe = nil): Boolean;
    procedure CriarDataSetsFrx;
    procedure CarregaDados(ANFSe: TNFSe);
    procedure CarregaIdentificacao(ANFSe: TNFSe);
    procedure CarregaItensServico(ANFSe: TNFSe);
    procedure CarregaParametros(ANFSe: TNFSe);
    procedure CarregaPrestador(ANFSe: TNFSe);
    procedure CarregaServicos(ANFSe: TNFSe);
    procedure CarregaTomador(ANFSe: TNFSe);
    procedure CarregaItermediario(ANFSe: TNFSe);
    procedure CarregaTransportadora(ANFSe: TNFSe);
    procedure CarregaCondicaoPagamento(ANFSe: TNFSe);
    procedure CarregaCondicaoPagamentoParcelas(ANFSe: TNFSe);
    procedure CarregaLogoPrefeitura;
    procedure CarregaImagemPrestadora;

    function ManterDocumento(const sCpfCnpj: String): string;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
    procedure SetDataSetsToFrxReport;
    procedure AjustaMargensReports;
    function GetACBrNFSe: TACBrNFSeX;

  protected
    property ACBrNFSe: TACBrNFSeX read GetACBrNFSe;
  public
    frxReport   : TfrxReport; // Está como public, pois quando declarado em datamodule, tem acesso externo, e pode ser que alguem esteja usando.
    frxPDFExport: TfrxPDFExport;
      // CDSs
    cdsIdentificacao            : TACBrFRDataSet;
    cdsPrestador                : TACBrFRDataSet;
    cdsServicos                 : TACBrFRDataSet;
    cdsParametros               : TACBrFRDataSet;
    cdsTomador                  : TACBrFRDataSet;
    cdsIntermediario            : TACBrFRDataSet;
    cdsTransportadora           : TACBrFRDataSet;
    cdsItensServico             : TACBrFRDataSet;
    cdsCondicaoPagamento        : TACBrFRDataSet;
    cdsCondicaoPagamentoParcelas: TACBrFRDataSet;

      // FrxDBs
    frxIdentificacao            : TfrxDBDataset;
    frxPrestador                : TfrxDBDataset;
    frxTomador                  : TfrxDBDataset;
    frxIntermediario            : TfrxDBDataset;
    frxTransportadora           : TfrxDBDataset;
    frxServicos                 : TfrxDBDataset;
    frxParametros               : TfrxDBDataset;
    frxItensServico             : TfrxDBDataset;
    frxCondicaoPagamento        : TfrxDBDataset;
    frxCondicaoPagamentoParcelas: TfrxDBDataset;

    FIncorporarFontesPdf    : Boolean;
    FIncorporarBackgroundPdf: Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFSe(NFSe: TNFSe = nil); override;
    procedure ImprimirDANFSePDF(NFSe: TNFSe = nil); overload; override;
    procedure ImprimirDANFSePDF(AStream: TStream; NFSe: TNFSe = nil); overload; override;
    property PreparedReport: TfrxReport read GetPreparedReport;
    property DANFSeXClassOwner: TACBrNFSeXDANFSeClass read FDANFSeXClassOwner;
  published
    property FastFile               : String read FFastFile write FFastFile;
    property EspessuraBorda         : Integer read FEspessuraBorda write FEspessuraBorda;
    property IncorporarFontesPdf    : Boolean read FIncorporarFontesPdf write FIncorporarFontesPdf;
    property IncorporarBackgroundPdf: Boolean read FIncorporarBackgroundPdf write FIncorporarBackgroundPdf;
  end;

implementation

uses
  StrUtils,
  DateUtils,
  Math,
  ACBrUtil.Strings,
  ACBrDFeUtil,
  ACBrUtil.Math,
  ACBrUtil.FilesIO,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.XMLHTML,
  ACBrNFSeXConversao,
  ACBrNFSeXInterface,
  ACBrImage,
  ACBrDelphiZXingQRCode,
  ACBrNFSeXConfiguracoes;

constructor TACBrNFSeXDANFSeFR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDANFSeXClassOwner := TACBrNFSeXDANFSeClass(Self);
  FFastFile          := '';
  FEspessuraBorda    := 1;
  CriarDataSetsFrx;
  FIncorporarFontesPdf     := false;
  FIncorporarBackgroundPdf := false;
end;

destructor TACBrNFSeXDANFSeFR.Destroy;
begin
  frxIdentificacao.Free;
  frxPrestador.Free;
  frxTomador.Free;
  frxIntermediario.Free;
  frxTransportadora.Free;
  frxServicos.Free;
  frxParametros.Free;
  frxItensServico.Free;
  frxCondicaoPagamento.Free;
  frxCondicaoPagamentoParcelas.Free;

  cdsIdentificacao.Free;
  cdsPrestador.Free;
  cdsServicos.Free;
  cdsParametros.Free;
  cdsTomador.Free;
  cdsIntermediario.Free;
  cdsTransportadora.Free;
  cdsItensServico.Free;
  cdsCondicaoPagamento.Free;
  cdsCondicaoPagamentoParcelas.Free;

  frxReport.Free;
  frxPDFExport.Free;

  inherited Destroy;
end;

function TACBrNFSeXDANFSeFR.GetACBrNFSe: TACBrNFSeX;
begin
  Result := TACBrNFSeX(FDANFSeXClassOwner.ACBrNFSe);
end;

function TACBrNFSeXDANFSeFR.GetPreparedReport: TfrxReport;
begin
  if Trim(FFastFile) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrNFSeXDANFSeFR.ImprimirDANFSe(NFSe: TNFSe);
begin
  Provedor := TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Provedor;

  DANFSeXClassOwner.FIndexImpressaoIndividual := -1;
  if PrepareReport(NFSe) then
  begin
    if MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrNFSeXDANFSeFR.ImprimirDANFSePDF(AStream: TStream; NFSe: TNFSe);
const
  TITULO_PDF = 'Nota Fiscal de Serviço Eletrônica';
var
  I             : Integer;
  LArquivoPDF   : string;
  LOldShowDialog: Boolean;
begin
  Provedor := TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Provedor;

  if PrepareReport(NFSe) then
  begin
    frxPDFExport.Author        := Sistema;
    frxPDFExport.Creator       := Sistema;
    frxPDFExport.Subject       := TITULO_PDF;
    frxPDFExport.EmbeddedFonts := false;
    frxPDFExport.Background    := IncorporarBackgroundPdf;
    frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;

    LOldShowDialog := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := false;

      frxPDFExport.Stream := AStream;
      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := LOldShowDialog;
    end;
  end;
end;

procedure TACBrNFSeXDANFSeFR.ImprimirDANFSePDF(NFSe: TNFSe);
const
  TITULO_PDF = 'Nota Fiscal de Serviço Eletrônica';
var
  I            : Integer;
  LArquivoPDF  : string;
  OldShowDialog: Boolean;
begin
  Provedor := TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Provedor;

  for I := 1 to TACBrNFSeX(ACBrNFSe).NotasFiscais.Count do
  begin
    DANFSeXClassOwner.FIndexImpressaoIndividual := I;
    if PrepareReport(NFSe) then
    begin
      frxPDFExport.Author        := Sistema;
      frxPDFExport.Creator       := Sistema;
      frxPDFExport.Subject       := TITULO_PDF;
      frxPDFExport.EmbeddedFonts := false;
      frxPDFExport.Background    := IncorporarBackgroundPdf;
      frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;

      OldShowDialog := frxPDFExport.ShowDialog;
      try
        frxPDFExport.ShowDialog := false;
          LArquivoPDF := Trim(DANFSeXClassOwner.NomeDocumento);

          if EstaVazio(LArquivoPDF) then
            if Assigned(NFSe) then
              LArquivoPDF         := TACBrNFSeX(ACBrNFSe).NumID[ NFSe ] + '-nfse.pdf'
            else
              LArquivoPDF         := TACBrNFSeX(ACBrNFSe).NumID[ TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[ I -1].NFSe ] + '-nfse.pdf';

          frxPDFExport.FileName := PathWithDelim(DANFSeXClassOwner.PathPDF) + LArquivoPDF;

          if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
            ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

          frxReport.Export(frxPDFExport);

          FPArquivoPDF := frxPDFExport.FileName;
          if Assigned(NFSe) then
            Break;
      finally
        frxPDFExport.ShowDialog := OldShowDialog;
      end;
    end;
  end;
end;

procedure TACBrNFSeXDANFSeFR.AjustaMargensReports;
var
  Page: TfrxReportPage;
  I   : Integer;
begin
    //Não tratar quando for margem 0 ou a margem padrão, usar a do FR3
  for I := 0 to (frxReport.PreviewPages.Count - 1) do
  begin
    Page := frxReport.PreviewPages.Page[ I ];
    if (MargemSuperior > 0) and (MargemSuperior <> 8) then
      Page.TopMargin := MargemSuperior;
    if (MargemInferior > 0) and (MargemInferior <> 8) then
      Page.BottomMargin := MargemInferior;
    if (MargemEsquerda > 0) and (MargemEsquerda <> 6) then
      Page.LeftMargin := MargemEsquerda;
    if (MargemDireita > 0) and (MargemDireita <> 5.1) then
      Page.RightMargin := MargemDireita;
    frxReport.PreviewPages.ModifyPage(I, Page);
  end;
end;

procedure TACBrNFSeXDANFSeFR.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(frxIdentificacao);
  frxReport.EnabledDataSets.Add(frxPrestador);
  frxReport.EnabledDataSets.Add(frxTomador);
  frxReport.EnabledDataSets.Add(frxIntermediario);
  frxReport.EnabledDataSets.Add(frxTransportadora);
  frxReport.EnabledDataSets.Add(frxServicos);
  frxReport.EnabledDataSets.Add(frxParametros);
  frxReport.EnabledDataSets.Add(frxItensServico);
  frxReport.EnabledDataSets.Add(frxCondicaoPagamento);
  frxReport.EnabledDataSets.Add(frxCondicaoPagamentoParcelas);
end;

function TACBrNFSeXDANFSeFR.PrepareReport(ANFSe: TNFSe): Boolean;
var
  I             : Integer;
  wProjectStream: TStringStream;
begin
  Result := false;

  SetDataSetsToFrxReport;
  if Trim(FastFile) <> '' then
  begin
    if not (uppercase(copy(FastFile, length(FastFile) - 3, 4)) = '.FR3') then
    begin
      wProjectStream          := TStringStream.Create(FastFile);
      frxReport.FileName      := '';
      wProjectStream.Position := 0;
      frxReport.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(FastFile) then
        frxReport.LoadFromFile(FastFile)
      else
        raise EACBrNFSeXDANFSeFR.CreateFmt('Caminho do arquivo de impressão do DANFSe "%s" inválido.', [ FastFile ]);
    end;
  end
  else
    raise EACBrNFSeXDANFSeFR.Create('Caminho do arquivo de impressão do DANFSe não assinalado.');

  frxReport.PrintOptions.Copies      := NumCopias;
  frxReport.PrintOptions.ShowDialog  := MostraSetup;
  frxReport.ShowProgress             := MostraStatus;
  frxReport.PreviewOptions.AllowEdit := false;

    // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  if Assigned(ANFSe) then
  begin
    CarregaDados(ANFSe);
    Result := frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrNFSe) then
    begin
      if DANFSeXClassOwner.FIndexImpressaoIndividual > 0  then
      begin
        CarregaDados(TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[ DANFSeXClassOwner.FIndexImpressaoIndividual -1 ].NFSe);
        Result := frxReport.PrepareReport( DANFSeXClassOwner.FIndexImpressaoIndividual > 0 );
      end else
      begin
        for I := 0 to TACBrNFSeX(ACBrNFSe).NotasFiscais.Count - 1 do
        begin
          CarregaDados(TACBrNFSeX(ACBrNFSe).NotasFiscais.Items[ I ].NFSe);
          Result := frxReport.PrepareReport( not (i > 0) );
        end;
      end;
    end
    else
      raise EACBrNFSeXDANFSeFR.Create('Propriedade ACBrNFSe não assinalada.');
  end;

  AjustaMargensReports;
end;

procedure TACBrNFSeXDANFSeFR.CriarDataSetsFrx;
begin
  frxReport                                    := TfrxReport.Create(nil);
  frxReport.PreviewOptions.Buttons             := [ pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick ];
  frxReport.EngineOptions.UseGlobalDataSetList := false;

  frxReport.Tag := 1;
    // Version := '5.2.3'
  frxReport.DotMatrixReport           := false;
  frxReport.IniFile                   := '\Software\Fast Reports';
  frxReport.PreviewOptions.Buttons    := [ pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick ];
  frxReport.PreviewOptions.Zoom       := 1.000000000000000000;
  frxReport.PrintOptions.Printer      := 'Padrão';
  frxReport.PrintOptions.PrintOnSheet := 0;
  frxReport.ScriptLanguage            := 'PascalScript';
  frxReport.StoreInDFM                := false;
  frxReport.OnBeforePrint             := frxReportBeforePrint;
  frxReport.OnReportPrint             := 'frxReportOnReportPrint';

  frxPDFExport := TfrxPDFExport.Create(nil);

  frxPDFExport.UseFileCache    := True;
  frxPDFExport.ShowProgress    := True;
  frxPDFExport.OverwritePrompt := false;
  frxPDFExport.PrintOptimized  := True;
  frxPDFExport.Outline         := false;
  frxPDFExport.Background      := True;
  frxPDFExport.HTMLTags        := True;
  frxPDFExport.Author          := 'FastReport';
  frxPDFExport.Subject         := 'Exportando o DANFSe para PDF';
  frxPDFExport.HideToolbar     := false;
  frxPDFExport.HideMenubar     := false;
  frxPDFExport.HideWindowUI    := false;
  frxPDFExport.FitWindow       := false;
  frxPDFExport.CenterWindow    := false;
  frxPDFExport.PrintScaling    := false;

  RttiSetProp(frxPDFExport, 'Transparency', 'False');

  cdsIdentificacao := TACBrFRDataSet.Create(nil);

  cdsIdentificacao.Close;

  cdsIdentificacao.FieldDefs.Clear;
  cdsIdentificacao.FieldDefs.Add('id', ftString, 10);
  cdsIdentificacao.FieldDefs.Add('Numero', ftString, 16);
  cdsIdentificacao.FieldDefs.Add('Serie', ftString, 5);
  cdsIdentificacao.FieldDefs.Add('Tipo', ftString, 1);
  cdsIdentificacao.FieldDefs.Add('Competencia', ftString, 20);
  cdsIdentificacao.FieldDefs.Add('NumeroNFSe', ftString, 16);
  cdsIdentificacao.FieldDefs.Add('NFSeSubstituida', ftString, 16);
  cdsIdentificacao.FieldDefs.Add('DataEmissao', ftString, 19);
  cdsIdentificacao.FieldDefs.Add('CodigoVerificacao', ftString, 50);
  cdsIdentificacao.FieldDefs.Add('LinkNFSe', ftString, 500);

  cdsIdentificacao.CreateDataSet;
{$IFNDEF FPC}
  cdsIdentificacao.LogChanges := false;
{$ELSE}
  cdsIdentificacao.Open;
{$ENDIF}
  cdsPrestador := TACBrFRDataSet.Create(nil);

  cdsPrestador.Close;

  cdsPrestador.FieldDefs.Clear;
  cdsPrestador.FieldDefs.Add('Cnpj', ftString, 18);
  cdsPrestador.FieldDefs.Add('InscricaoMunicipal', ftString, 15);
  cdsPrestador.FieldDefs.Add('InscricaoEstadual', ftString, 20);
  cdsPrestador.FieldDefs.Add('RazaoSocial', ftString, 60);
  cdsPrestador.FieldDefs.Add('NomeFantasia', ftString, 60);
  cdsPrestador.FieldDefs.Add('Endereco', ftString, 60);
  cdsPrestador.FieldDefs.Add('Numero', ftString, 60);
  cdsPrestador.FieldDefs.Add('Complemento', ftString, 60);
  cdsPrestador.FieldDefs.Add('Bairro', ftString, 60);
  cdsPrestador.FieldDefs.Add('CodigoMunicipio', ftString, 7);
  cdsPrestador.FieldDefs.Add('UF', ftString, 2);
  cdsPrestador.FieldDefs.Add('CEP', ftString, 9);
  cdsPrestador.FieldDefs.Add('xMunicipio', ftString, 60);
  cdsPrestador.FieldDefs.Add('CodigoPais', ftString, 4);
  cdsPrestador.FieldDefs.Add('Telefone', ftString, 15);
  cdsPrestador.FieldDefs.Add('Email', ftString, 60);

  cdsPrestador.CreateDataSet;

{$IFNDEF FPC}
  cdsPrestador.LogChanges := false;
{$ELSE}
  cdsPrestador.Open;
{$ENDIF}
  cdsServicos := TACBrFRDataSet.Create(nil);

  cdsServicos.Close;

  cdsServicos.FieldDefs.Clear;
  cdsServicos.FieldDefs.Add('ItemListaServico', ftString, 6);
  cdsServicos.FieldDefs.Add('CodigoCnae', ftString, 15);
  cdsServicos.FieldDefs.Add('CodigoNbs', ftString, 9);
  cdsServicos.FieldDefs.Add('CodigoTributacaoMunicipio', ftString, 20);
  cdsServicos.FieldDefs.Add('Discriminacao', ftString, 4000);
  cdsServicos.FieldDefs.Add('ExigibilidadeISS', ftString, 60);
  cdsServicos.FieldDefs.Add('CodigoMunicipio', ftString, 60);
  cdsServicos.FieldDefs.Add('MunicipioIncidencia', ftString, 60);
  cdsServicos.FieldDefs.Add('CodigoPais', ftString, 4);
  cdsServicos.FieldDefs.Add('NumeroProcesso', ftString, 10);
  cdsServicos.FieldDefs.Add('xItemListaServico', ftString, 300);
  cdsServicos.FieldDefs.Add('ResponsavelRetencao', ftString, 1);
  cdsServicos.FieldDefs.Add('Descricao', ftString, 80);
  cdsServicos.FieldDefs.Add('ValorServicos', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorDeducoes', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorPis', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorCofins', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorInss', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorIr', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorCsll', ftCurrency);
  cdsServicos.FieldDefs.Add('IssRetido', ftString, 1);
  cdsServicos.FieldDefs.Add('ValorIss', ftCurrency);
  cdsServicos.FieldDefs.Add('OutrasRetencoes', ftCurrency);
  cdsServicos.FieldDefs.Add('BaseCalculo', ftCurrency);
  cdsServicos.FieldDefs.Add('Aliquota', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorLiquidoNfse', ftCurrency);
  cdsServicos.FieldDefs.Add('ValorIssRetido', ftCurrency);
  cdsServicos.FieldDefs.Add('DescontoCondicionado', ftCurrency);
  cdsServicos.FieldDefs.Add('DescontoIncondicionado', ftCurrency);
  cdsServicos.FieldDefs.Add('TotalServicos', ftCurrency); // Nao usado - mantido por compatibilidade era calcfield
  cdsServicos.FieldDefs.Add('TotalNota', ftCurrency); // Nao usado - mantido por compatibilidade era calcfield
  cdsServicos.FieldDefs.Add('Tributacao', ftString, 1);
  cdsServicos.FieldDefs.Add('OutrosDescontos', ftCurrency);
  cdsServicos.FieldDefs.Add('DescricaoTotalRetDemo', ftString, 19);
  cdsServicos.FieldDefs.Add('DescriçãoTributosFederais', ftString, 35);
  cdsServicos.FieldDefs.Add('ValorTotalNotaFiscal', ftCurrency);

    // Provedor SP
  cdsServicos.FieldDefs.Add('ValorCargaTributaria', ftCurrency);
  cdsServicos.FieldDefs.Add('PercentualCargaTributaria', ftCurrency);
  cdsServicos.FieldDefs.Add('FonteCargaTributaria', ftString, 10);

  cdsServicos.CreateDataSet;
{$IFNDEF FPC}
  cdsServicos.LogChanges := false;
{$ELSE}
  cdsServicos.Open;
{$ENDIF}
  cdsParametros := TACBrFRDataSet.Create(nil);

  cdsParametros.Close;

  cdsParametros.FieldDefs.Clear;
  cdsParametros.FieldDefs.Add('ExigibilidadeISS', ftString, 60);
  cdsParametros.FieldDefs.Add('CodigoMunicipio', ftString, 60);
  cdsParametros.FieldDefs.Add('MunicipioIncidencia', ftString, 60);
  cdsParametros.FieldDefs.Add('MunicipioPrestacao', ftString, 60);
  cdsParametros.FieldDefs.Add('OutrasInformacoes', ftString, 1000);
  cdsParametros.FieldDefs.Add('InformacoesComplementares', ftString, 1000);
  cdsParametros.FieldDefs.Add('CodigoObra', ftString, 60);
  cdsParametros.FieldDefs.Add('Art', ftString, 60);
  cdsParametros.FieldDefs.Add('Imagem', ftString, 256);
  cdsParametros.FieldDefs.Add('LogoExpandido', ftString, 1);
  cdsParametros.FieldDefs.Add('LogoCarregado', ftBlob); // Carregar foto
  cdsParametros.FieldDefs.Add('imgPrefeitura', ftString, 256);
  cdsParametros.FieldDefs.Add('LogoPrefExpandido', ftString, 1);
  cdsParametros.FieldDefs.Add('LogoPrefCarregado', ftBlob);
  cdsParametros.FieldDefs.Add('Nome_Prefeitura', ftString, 256);
  cdsParametros.FieldDefs.Add('Mensagem0', ftString, 60);
  cdsParametros.FieldDefs.Add('Sistema', ftString, 150);
  cdsParametros.FieldDefs.Add('Usuario', ftString, 50);
  cdsParametros.FieldDefs.Add('Site', ftString, 50);
  cdsParametros.FieldDefs.Add('NaturezaOperacao', ftString, 50);
  cdsParametros.FieldDefs.Add('RegimeEspecialTributacao', ftString, 80);
  cdsParametros.FieldDefs.Add('OptanteSimplesNacional', ftString, 30);
  cdsParametros.FieldDefs.Add('IncentivadorCultural', ftString, 10);
  cdsParametros.FieldDefs.Add('TipoRecolhimento', ftString, 15);
  cdsParametros.FieldDefs.Add('id_sis_legado', ftInteger);
    //
  cdsParametros.FieldDefs.Add('ValorCredito', ftCurrency);

  cdsParametros.CreateDataSet;
{$IFNDEF FPC}
  cdsParametros.LogChanges := false;
{$ELSE}
  cdsParametros.Open;
{$ENDIF}
  cdsTomador := TACBrFRDataSet.Create(nil);

  cdsTomador.Close;

  cdsTomador.FieldDefs.Clear;
  cdsTomador.FieldDefs.Add('CpfCnpj', ftString, 18);
  cdsTomador.FieldDefs.Add('InscricaoMunicipal', ftString, 15);
  cdsTomador.FieldDefs.Add('InscricaoEstadual', ftString, 20);
  cdsTomador.FieldDefs.Add('RazaoSocial', ftString, 60);
  cdsTomador.FieldDefs.Add('NomeFantasia', ftString, 60);
  cdsTomador.FieldDefs.Add('Endereco', ftString, 60);
  cdsTomador.FieldDefs.Add('Numero', ftString, 60);
  cdsTomador.FieldDefs.Add('Complemento', ftString, 60);
  cdsTomador.FieldDefs.Add('Bairro', ftString, 60);
  cdsTomador.FieldDefs.Add('CodigoMunicipio', ftString, 7);
  cdsTomador.FieldDefs.Add('UF', ftString, 2);
  cdsTomador.FieldDefs.Add('CEP', ftString, 9);
  cdsTomador.FieldDefs.Add('xMunicipio', ftString, 60);
  cdsTomador.FieldDefs.Add('CodigoPais', ftString, 4);
  cdsTomador.FieldDefs.Add('Telefone', ftString, 15);
  cdsTomador.FieldDefs.Add('Email', ftString, 60);

  cdsTomador.CreateDataSet;
{$IFNDEF FPC}
  cdsTomador.LogChanges := false;
{$ELSE}
  cdsTomador.Open;
{$ENDIF}
  cdsIntermediario := TACBrFRDataSet.Create(nil);

  cdsIntermediario.Close;

  cdsIntermediario.FieldDefs.Clear;
  cdsIntermediario.FieldDefs.Add('RazaoSocial', ftString, 150);

  cdsIntermediario.CreateDataSet;
{$IFNDEF FPC}
  cdsIntermediario.LogChanges := false;
{$ELSE}
  cdsIntermediario.Open;
{$ENDIF}
  cdsTransportadora := TACBrFRDataSet.Create(nil);

  cdsTransportadora.Close;

  cdsTransportadora.FieldDefs.Clear;
  cdsTransportadora.FieldDefs.Add('Cnpj', ftString, 18);
  cdsTransportadora.FieldDefs.Add('InscicaoEstadual', ftString, 15);
  cdsTransportadora.FieldDefs.Add('RazaoSocial', ftString, 60);
  cdsTransportadora.FieldDefs.Add('Placa', ftString, 7);
  cdsTransportadora.FieldDefs.Add('Endereco', ftString, 60);
  cdsTransportadora.FieldDefs.Add('CodigoMunicipio', ftInteger);
  cdsTransportadora.FieldDefs.Add('NomeMunicipio', ftString, 60);
  cdsTransportadora.FieldDefs.Add('Sigla', ftString, 2);
  cdsTransportadora.FieldDefs.Add('BacenPais', ftInteger);
  cdsTransportadora.FieldDefs.Add('NomePais', ftString, 60);
  cdsTransportadora.FieldDefs.Add('TipoFrete', ftInteger);

  cdsTransportadora.CreateDataSet;
{$IFNDEF FPC}
  cdsTransportadora.LogChanges := false;
{$ELSE}
  cdsTransportadora.Open;
{$ENDIF}
  cdsItensServico := TACBrFRDataSet.Create(nil);
  cdsItensServico.Close;
  cdsItensServico.FieldDefs.Clear;
  cdsItensServico.FieldDefs.Add('DiscriminacaoServico', ftString, 4000);
  cdsItensServico.FieldDefs.Add('Quantidade', ftString, 10);
  cdsItensServico.FieldDefs.Add('ValorUnitario', ftString, 30);
  cdsItensServico.FieldDefs.Add('ValorTotal', ftString, 30);
  cdsItensServico.FieldDefs.Add('Tributavel', ftString, 1);
  cdsItensServico.FieldDefs.Add('Unidade', ftString, 3);
  cdsItensServico.FieldDefs.Add('Aliquota', ftString, 30);
  cdsItensServico.FieldDefs.Add('AliquotaISSST', ftString, 30);
  cdsItensServico.FieldDefs.Add('ValorISSST', ftString, 30);
  cdsItensServico.FieldDefs.Add('DescontoIncondicionado', ftString, 30);

  cdsItensServico.CreateDataSet;
{$IFNDEF FPC}
  cdsItensServico.LogChanges := false;
{$ELSE}
  cdsItensServico.Open;
{$ENDIF}
  cdsCondicaoPagamento := TACBrFRDataSet.Create(nil);

  cdsCondicaoPagamento.Close;

  cdsCondicaoPagamento.FieldDefs.Clear;

  cdsCondicaoPagamento.FieldDefs.Add('Condicao', ftString, 30);
  cdsCondicaoPagamento.FieldDefs.Add('Parcela', ftString, 10);

  cdsCondicaoPagamento.CreateDataSet;
{$IFNDEF FPC}
  cdsCondicaoPagamento.LogChanges := false;
{$ELSE}
  cdsCondicaoPagamento.Open;
{$ENDIF}
  cdsCondicaoPagamentoParcelas := TACBrFRDataSet.Create(nil);

  cdsCondicaoPagamentoParcelas.Close;

  cdsCondicaoPagamentoParcelas.FieldDefs.Clear;

  cdsCondicaoPagamentoParcelas.FieldDefs.Add('Condicao', ftString, 30);
  cdsCondicaoPagamentoParcelas.FieldDefs.Add('Parcela', ftString, 10);
  cdsCondicaoPagamentoParcelas.FieldDefs.Add('DataVencimento', ftString, 19);
  cdsCondicaoPagamentoParcelas.FieldDefs.Add('Valor', ftCurrency);

  cdsCondicaoPagamentoParcelas.CreateDataSet;
{$IFNDEF FPC}
  cdsCondicaoPagamentoParcelas.LogChanges := false;
{$ELSE}
  cdsCondicaoPagamentoParcelas.Open;
{$ENDIF}
  frxIdentificacao := TfrxDBDataset.Create(Self);

  frxIdentificacao.UserName        := 'Identificacao';
  frxIdentificacao.Enabled         := false;
  frxIdentificacao.CloseDataSource := false;
  frxIdentificacao.OpenDataSource  := false;

  frxIdentificacao.FieldAliases.Clear;
  frxIdentificacao.FieldAliases.Add('id=id');
  frxIdentificacao.FieldAliases.Add('Numero=Numero');
  frxIdentificacao.FieldAliases.Add('Serie=Serie');
  frxIdentificacao.FieldAliases.Add('Tipo=Tipo');
  frxIdentificacao.FieldAliases.Add('Competencia=Competencia');
  frxIdentificacao.FieldAliases.Add('NumeroNFSe=NumeroNFSe');
  frxIdentificacao.FieldAliases.Add('NFSeSubstituida=NFSeSubstituida');
  frxIdentificacao.FieldAliases.Add('DataEmissao=DataEmissao');
  frxIdentificacao.FieldAliases.Add('CodigoVerificacao=CodigoVerificacao');
  frxIdentificacao.FieldAliases.Add('LinkNFSe=LinkNFSe');

  frxIdentificacao.DataSet       := cdsIdentificacao;
  frxIdentificacao.BCDToCurrency := false;

  frxCondicaoPagamento := TfrxDBDataset.Create(Self);

  frxCondicaoPagamento.UserName        := 'CondicaoPagamento';
  frxCondicaoPagamento.Enabled         := false;
  frxCondicaoPagamento.CloseDataSource := false;
  frxCondicaoPagamento.OpenDataSource  := false;
  frxCondicaoPagamento.FieldAliases.Clear;
  frxCondicaoPagamento.FieldAliases.Add('Condicao=Condicao');
  frxCondicaoPagamento.FieldAliases.Add('Parcela=Parcela');

  frxCondicaoPagamento.DataSet       := cdsCondicaoPagamento;
  frxCondicaoPagamento.BCDToCurrency := false;

  frxCondicaoPagamentoParcelas                 := TfrxDBDataset.Create(Self);
  frxCondicaoPagamentoParcelas.UserName        := 'CondicaoPagamentoParcelas';
  frxCondicaoPagamentoParcelas.Enabled         := false;
  frxCondicaoPagamentoParcelas.CloseDataSource := false;
  frxCondicaoPagamentoParcelas.OpenDataSource  := false;

  frxCondicaoPagamentoParcelas.FieldAliases.Clear;
  frxCondicaoPagamentoParcelas.FieldAliases.Add('Condicao=Condicao');
  frxCondicaoPagamentoParcelas.FieldAliases.Add('Parcela=Parcela');
  frxCondicaoPagamentoParcelas.FieldAliases.Add('DataVencimento=DataVencimento');
  frxCondicaoPagamentoParcelas.FieldAliases.Add('Valor=Valor');

  frxCondicaoPagamentoParcelas.DataSet       := cdsCondicaoPagamentoParcelas;
  frxCondicaoPagamentoParcelas.BCDToCurrency := false;

  frxPrestador := TfrxDBDataset.Create(Self);

  frxPrestador.UserName        := 'Prestador';
  frxPrestador.Enabled         := false;
  frxPrestador.CloseDataSource := false;
  frxPrestador.OpenDataSource  := false;

  frxPrestador.FieldAliases.Clear;
  frxPrestador.FieldAliases.Add('Cnpj=Cnpj');
  frxPrestador.FieldAliases.Add('InscricaoMunicipal=InscricaoMunicipal');
  frxPrestador.FieldAliases.Add('InscricaoEstadual=InscricaoEstadual');
  frxPrestador.FieldAliases.Add('RazaoSocial=RazaoSocial');
  frxPrestador.FieldAliases.Add('NomeFantasia=NomeFantasia');
  frxPrestador.FieldAliases.Add('Endereco=Endereco');
  frxPrestador.FieldAliases.Add('Numero=Numero');
  frxPrestador.FieldAliases.Add('Complemento=Complemento');
  frxPrestador.FieldAliases.Add('Bairro=Bairro');
  frxPrestador.FieldAliases.Add('CodigoMunicipio=CodigoMunicipio');
  frxPrestador.FieldAliases.Add('UF=UF');
  frxPrestador.FieldAliases.Add('CEP=CEP');
  frxPrestador.FieldAliases.Add('xMunicipio=xMunicipio');
  frxPrestador.FieldAliases.Add('CodigoPais=CodigoPais');
  frxPrestador.FieldAliases.Add('Telefone=Telefone');
  frxPrestador.FieldAliases.Add('Email=Email');
  frxPrestador.DataSet       := cdsPrestador;
  frxPrestador.BCDToCurrency := false;

  frxTomador := TfrxDBDataset.Create(Self);

  frxTomador.UserName        := 'Tomador';
  frxTomador.Enabled         := false;
  frxTomador.CloseDataSource := false;
  frxTomador.OpenDataSource  := false;

  frxTomador.FieldAliases.Clear;
  frxTomador.FieldAliases.Add('CpfCnpj=CpfCnpj');
  frxTomador.FieldAliases.Add('InscricaoMunicipal=InscricaoMunicipal');
  frxTomador.FieldAliases.Add('InscricaoEstadual=InscricaoEstadual');
  frxTomador.FieldAliases.Add('RazaoSocial=RazaoSocial');
  frxTomador.FieldAliases.Add('NomeFantasia=NomeFantasia');
  frxTomador.FieldAliases.Add('Endereco=Endereco');
  frxTomador.FieldAliases.Add('Numero=Numero');
  frxTomador.FieldAliases.Add('Complemento=Complemento');
  frxTomador.FieldAliases.Add('Bairro=Bairro');
  frxTomador.FieldAliases.Add('CodigoMunicipio=CodigoMunicipio');
  frxTomador.FieldAliases.Add('UF=UF');
  frxTomador.FieldAliases.Add('CEP=CEP');
  frxTomador.FieldAliases.Add('xMunicipio=xMunicipio');
  frxTomador.FieldAliases.Add('CodigoPais=CodigoPais');
  frxTomador.FieldAliases.Add('Telefone=Telefone');
  frxTomador.FieldAliases.Add('Email=Email');

  frxTomador.DataSet       := cdsTomador;
  frxTomador.BCDToCurrency := false;

  frxIntermediario := TfrxDBDataset.Create(Self);

  frxIntermediario.UserName := 'Intermediario';
  frxIntermediario.CloseDataSource := false;
  frxIntermediario.OpenDataSource  := false;

  frxIntermediario.FieldAliases.Clear;
  frxIntermediario.FieldAliases.Add('RazaoSocial=RazaoSocial');

  frxIntermediario.DataSet       := cdsIntermediario;
  frxIntermediario.BCDToCurrency := false;

  frxTransportadora := TfrxDBDataset.Create(Self);

  frxTransportadora.UserName        := 'Transportadora';
  frxTransportadora.CloseDataSource := false;
  frxTransportadora.OpenDataSource  := false;

  frxTransportadora.FieldAliases.Clear;
  frxTransportadora.FieldAliases.Add('Cnpj=Cnpj');
  frxTransportadora.FieldAliases.Add('InscicaoEstadual=InscicaoEstadual');
  frxTransportadora.FieldAliases.Add('RazaoSocial=RazaoSocial');
  frxTransportadora.FieldAliases.Add('Placa=Placa');
  frxTransportadora.FieldAliases.Add('Endereco=Endereco');
  frxTransportadora.FieldAliases.Add('CodigoMunicipio=CodigoMunicipio');
  frxTransportadora.FieldAliases.Add('NomeMunicipio=NomeMunicipio');
  frxTransportadora.FieldAliases.Add('Sigla=Sigla');
  frxTransportadora.FieldAliases.Add('BacenPais=BacenPais');
  frxTransportadora.FieldAliases.Add('NomePais=NomePais');
  frxTransportadora.FieldAliases.Add('TipoFrete=TipoFrete');

  frxTransportadora.DataSet       := cdsTransportadora;
  frxTransportadora.BCDToCurrency := false;

  frxServicos := TfrxDBDataset.Create(Self);

  frxServicos.UserName        := 'Servicos';
  frxServicos.Enabled         := false;
  frxServicos.CloseDataSource := false;
  frxServicos.OpenDataSource  := false;

  frxServicos.FieldAliases.Clear;
  frxServicos.FieldAliases.Add('ItemListaServico=ItemListaServico');
  frxServicos.FieldAliases.Add('CodigoCnae=CodigoCnae');
  frxServicos.FieldAliases.Add('CodigoNbs=CodigoNbs');
  frxServicos.FieldAliases.Add('CodigoTributacaoMunicipio=CodigoTributacaoMunicipio');
  frxServicos.FieldAliases.Add('Discriminacao=Discriminacao');
  frxServicos.FieldAliases.Add('CodigoMunicipio=CodigoMunicipio');
  frxServicos.FieldAliases.Add('CodigoPais=CodigoPais');
  frxServicos.FieldAliases.Add('ExigibilidadeISS=ExigibilidadeISS');
  frxServicos.FieldAliases.Add('MunicipioIncidencia=MunicipioIncidencia');
  frxServicos.FieldAliases.Add('NumeroProcesso=NumeroProcesso');
  frxServicos.FieldAliases.Add('xItemListaServico=xItemListaServico');
  frxServicos.FieldAliases.Add('ResponsavelRetencao=ResponsavelRetencao');
  frxServicos.FieldAliases.Add('Descricao=Descricao');
  frxServicos.FieldAliases.Add('ValorServicos=ValorServicos');
  frxServicos.FieldAliases.Add('ValorDeducoes=ValorDeducoes');
  frxServicos.FieldAliases.Add('ValorPis=ValorPis');
  frxServicos.FieldAliases.Add('ValorCofins=ValorCofins');
  frxServicos.FieldAliases.Add('ValorInss=ValorInss');
  frxServicos.FieldAliases.Add('ValorIr=ValorIr');
  frxServicos.FieldAliases.Add('ValorCsll=ValorCsll');
  frxServicos.FieldAliases.Add('IssRetido=IssRetido');
  frxServicos.FieldAliases.Add('ValorIss=ValorIss');
  frxServicos.FieldAliases.Add('OutrasRetencoes=OutrasRetencoes');
  frxServicos.FieldAliases.Add('BaseCalculo=BaseCalculo');
  frxServicos.FieldAliases.Add('Aliquota=Aliquota');
  frxServicos.FieldAliases.Add('ValorLiquidoNfse=ValorLiquidoNfse');
  frxServicos.FieldAliases.Add('ValorIssRetido=ValorIssRetido');
  frxServicos.FieldAliases.Add('DescontoCondicionado=DescontoCondicionado');
  frxServicos.FieldAliases.Add('DescontoIncondicionado=DescontoIncondicionado');
  frxServicos.FieldAliases.Add('TotalNota=TotalNota');
  frxServicos.FieldAliases.Add('Tributacao=Tributacao');
  frxServicos.FieldAliases.Add('OutrosDescontos=OutrosDescontos');
  frxServicos.FieldAliases.Add('DescricaoTotalRetDemo=DescricaoTotalRetDemo');
  frxServicos.FieldAliases.Add('DescriçãoTributosFederais=DescriçãoTributosFederais');
  frxServicos.FieldAliases.Add('ValorTotalNotaFiscal=ValorTotalNotaFiscal');
    // Provedor SP
  frxServicos.FieldAliases.Add('ValorCargaTributaria=ValorCargaTributaria');
  frxServicos.FieldAliases.Add('PercentualCargaTributaria=PercentualCargaTributaria');
  frxServicos.FieldAliases.Add('FonteCargaTributaria=FonteCargaTributaria');

  frxServicos.DataSet       := cdsServicos;
  frxServicos.BCDToCurrency := false;

  frxParametros := TfrxDBDataset.Create(Self);

  frxParametros.UserName        := 'Parametros';
  frxParametros.Enabled         := false;
  frxParametros.CloseDataSource := false;
  frxParametros.OpenDataSource  := false;

  frxParametros.FieldAliases.Clear;
  frxParametros.FieldAliases.Add('ExigibilidadeISS=ExigibilidadeISS');
  frxParametros.FieldAliases.Add('CodigoMunicipio=CodigoMunicipio');
  frxParametros.FieldAliases.Add('MunicipioIncidencia=MunicipioIncidencia');
  frxParametros.FieldAliases.Add('MunicipioPrestacao=MunicipioPrestacao');
  frxParametros.FieldAliases.Add('OutrasInformacoes=OutrasInformacoes');
  frxParametros.FieldAliases.Add('InformacoesComplementares=InformacoesComplementares');
  frxParametros.FieldAliases.Add('CodigoObra=CodigoObra');
  frxParametros.FieldAliases.Add('Art=Art');
  frxParametros.FieldAliases.Add('Imagem=Imagem');
  frxParametros.FieldAliases.Add('LogoExpandido=LogoExpandido');
  frxParametros.FieldAliases.Add('LogoCarregado=LogoCarregado');
  frxParametros.FieldAliases.Add('imgPrefeitura=imgPrefeitura');
  frxParametros.FieldAliases.Add('LogoPrefExpandido=LogoPrefExpandido');
  frxParametros.FieldAliases.Add('LogoPrefCarregado=LogoPrefCarregado');
  frxParametros.FieldAliases.Add('Nome_Prefeitura=Nome_Prefeitura');
  frxParametros.FieldAliases.Add('Mensagem0=Mensagem0');
  frxParametros.FieldAliases.Add('Sistema=Sistema');
  frxParametros.FieldAliases.Add('Usuario=Usuario');
  frxParametros.FieldAliases.Add('Site=Site');
  frxParametros.FieldAliases.Add('IncentivadorCultural=IncentivadorCultural');
  frxParametros.FieldAliases.Add('OptanteSimplesNacional=OptanteSimplesNacional');
  frxParametros.FieldAliases.Add('RegimeEspecialTributacao=RegimeEspecialTributacao');
  frxParametros.FieldAliases.Add('NaturezaOperacao=NaturezaOperacao');
  frxParametros.FieldAliases.Add('TipoRecolhimento=TipoRecolhimento');
  frxParametros.FieldAliases.Add('ValorCredito=ValorCredito');
  frxParametros.FieldAliases.Add('id_sis_legado=id_sis_legado');

  frxParametros.DataSet       := cdsParametros;
  frxParametros.BCDToCurrency := false;

  frxItensServico := TfrxDBDataset.Create(Self);

  frxItensServico.UserName        := 'ItensServico';
  frxItensServico.Enabled         := false;
  frxItensServico.CloseDataSource := false;
  frxItensServico.OpenDataSource  := false;

  frxItensServico.FieldAliases.Clear;
  frxItensServico.FieldAliases.Add('DiscriminacaoServico=DiscriminacaoServico');
  frxItensServico.FieldAliases.Add('Quantidade=Quantidade');
  frxItensServico.FieldAliases.Add('ValorUnitario=ValorUnitario');
  frxItensServico.FieldAliases.Add('ValorTotal=ValorTotal');
  frxItensServico.FieldAliases.Add('Tributavel=Tributavel');
  frxItensServico.FieldAliases.Add('Unidade=Unidade');
  frxItensServico.FieldAliases.Add('Aliquota=Aliquota');
  frxItensServico.FieldAliases.Add('AliquotaISSST=AliquotaISSST');
  frxItensServico.FieldAliases.Add('ValorISSST=ValorISSST');
  frxItensServico.FieldAliases.Add('DescontoIncondicionado=DescontoIncondicionado');

  frxItensServico.DataSet       := cdsItensServico;
  frxItensServico.BCDToCurrency := false;
end;

procedure TACBrNFSeXDANFSeFR.CarregaCondicaoPagamento(ANFSe: TNFSe);
var
  LCDS: TACBrFRDataSet;
  FProvider    : IACBrNFSeXProvider;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;
  
  LCDS := cdsCondicaoPagamento;
  LCDS.EmptyDataSet;
  LCDS.Append;
  LCDS.FieldByName('Condicao').AsString := FProvider.CondicaoPagToStr(ANFSe.CondicaoPagamento.Condicao);
  LCDS.FieldByName('Parcela').AsString  := IntToStr(ANFSe.CondicaoPagamento.QtdParcela);
  LCDS.Post;
end;

procedure TACBrNFSeXDANFSeFR.CarregaCondicaoPagamentoParcelas(ANFSe: TNFSe);
var
  I   : Integer;
  LCDS: TACBrFRDataSet;
  FProvider    : IACBrNFSeXProvider;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  LCDS := cdsCondicaoPagamentoParcelas;
  LCDS.EmptyDataSet;

  for I := 0 to Pred(ANFSe.CondicaoPagamento.Parcelas.Count) do
  begin
    LCDS.Append;
    LCDS.FieldByName('Condicao').AsString       := FProvider.CondicaoPagToStr(ANFSe.CondicaoPagamento.Parcelas[ I ].Condicao);
    LCDS.FieldByName('Parcela').AsString        := ANFSe.CondicaoPagamento.Parcelas[ I ].Parcela;
    LCDS.FieldByName('DataVencimento').AsString := FormatDateBr(ANFSe.CondicaoPagamento.Parcelas[ I ].DataVencimento);
    LCDS.FieldByName('Valor').AsFloat           := ANFSe.CondicaoPagamento.Parcelas[ I ].Valor;
    LCDS.Post;
  end;

end;

procedure TACBrNFSeXDANFSeFR.CarregaDados(ANFSe: TNFSe);
begin
  CarregaIdentificacao(ANFSe);
  CarregaPrestador(ANFSe);
  CarregaTomador(ANFSe);
  CarregaItermediario(ANFSe);
  CarregaServicos(ANFSe);
  CarregaItensServico(ANFSe);
  CarregaParametros(ANFSe);
  CarregaTransportadora(ANFSe);
  CarregaCondicaoPagamento(ANFSe);
  CarregaCondicaoPagamentoParcelas(ANFSe);
end;

procedure TACBrNFSeXDANFSeFR.CarregaIdentificacao(ANFSe: TNFSe);
var
  LCDS: TACBrFRDataSet;
begin
  LCDS := cdsIdentificacao;

  LCDS.EmptyDataSet;
  LCDS.Append;

  LCDS.FieldByName('Id').AsString := ANFSe.IdentificacaoRps.Numero + ANFSe.IdentificacaoRps.Serie;

  if (FormatarNumeroDocumentoNFSe) then
    LCDS.FieldByName('Numero').AsString := FormatarNumeroDocumentoFiscalNFSe(ANFSe.IdentificacaoRps.Numero)
  else
    LCDS.FieldByName('Numero').AsString := ANFSe.IdentificacaoRps.Numero;

  LCDS.FieldByName('Serie').AsString := ANFSe.IdentificacaoRps.Serie;

  if Provedor = proISSNet then
    LCDS.FieldByName('Competencia').AsString := FormatDateTime('dd/mm/yyyy', ANFSe.Competencia)
  else
    LCDS.FieldByName('Competencia').AsString := FormatDateTime('mm/yyyy', ANFSe.Competencia);

  if (FormatarNumeroDocumentoNFSe) then
    LCDS.FieldByName('NFSeSubstituida').AsString := FormatarNumeroDocumentoFiscalNFSe(ANFSe.NfseSubstituida)
  else
    LCDS.FieldByName('NFSeSubstituida').AsString := ANFSe.NfseSubstituida;

  if (FormatarNumeroDocumentoNFSe) then
    LCDS.FieldByName('NumeroNFSe').AsString := FormatarNumeroDocumentoFiscalNFSe(ANFSe.Numero)
  else
    LCDS.FieldByName('NumeroNFSe').AsString := ANFSe.Numero;

  if HourOf(ANFSe.DataEmissao) <> 0 then
    LCDS.FieldByName('DataEmissao').AsString := FormatDateTimeBr(ANFSe.DataEmissao)
  else
    LCDS.FieldByName('DataEmissao').AsString := FormatDateBr(ANFSe.DataEmissao);

  if ANFSe.Numero = '' then
  begin
    if frxReport.findcomponent('Memo12') <> nil then
      TfrxMemoView(frxReport.findcomponent('Memo12')).Text := 'Data do RPS';

    LCDS.FieldByName('DataEmissao').AsString := FormatDateBr(ANFSe.DataEmissaoRps);
  end;

  LCDS.FieldByName('CodigoVerificacao').AsString := ANFSe.CodigoVerificacao;
  LCDS.FieldByName('LinkNFSe').AsString          := ANFSe.Link;

  LCDS.Post;
end;

procedure TACBrNFSeXDANFSeFR.CarregaItensServico(ANFSe: TNFSe);
var
  I            : Integer;
  FProvider    : IACBrNFSeXProvider;
  LCDS         : TACBrFRDataSet;
  LItemsServico: TItemServicoCollection;
begin
  FProvider     := TACBrNFSeX(FACBrNFSe).Provider;
  LItemsServico := ANFSe.Servico.ItemServico;

  LCDS := cdsItensServico;
  LCDS.EmptyDataSet;

  for I := 0 to LItemsServico.Count - 1 do
  begin
    LCDS.Append;
    LCDS.FieldByName('DiscriminacaoServico').AsString   := LItemsServico.Items[ I ].Descricao;
    LCDS.FieldByName('Quantidade').AsString             := FloatToStr(LItemsServico.Items[ I ].Quantidade);
    LCDS.FieldByName('ValorUnitario').AsString          := FormatFloatBr(LItemsServico.Items[ I ].ValorUnitario, ',0.00');
    LCDS.FieldByName('ValorTotal').AsString             := FormatFloatBr(LItemsServico.Items[ I ].ValorTotal, ',0.00');
    LCDS.FieldByName('Tributavel').AsString             := FProvider.SimNaoDescricao(LItemsServico.Items[ I ].Tributavel);
    LCDS.FieldByName('Aliquota').AsString               := FormatFloatBr(LItemsServico.Items[ I ].Aliquota, '0.00');
    LCDS.FieldByName('Unidade').AsString                := LItemsServico.Items[ I ].Unidade;
    LCDS.FieldByName('AliquotaISSST').AsString          := FormatFloatBr(LItemsServico.Items[ I ].AliqISSST, '0.00');
    LCDS.FieldByName('ValorISSST').AsString             := FormatFloatBr(LItemsServico.Items[ I ].ValorISSST, '0.00');
    LCDS.FieldByName('DescontoIncondicionado').AsString := FormatFloatBr(LItemsServico.Items[ I ].DescontoIncondicionado, '0.00');
    LCDS.Post;
  end;
end;

procedure TACBrNFSeXDANFSeFR.CarregaItermediario(ANFSe: TNFSe);
var
  LCDS     : TACBrFRDataSet;
  LIntermediario : TDadosIntermediario;
begin
  LIntermediario  := ANFSe.Intermediario;

  LCDS := cdsIntermediario;
  LCDS.EmptyDataSet;
  LCDS.Append;

  LCDS.FieldByName('RazaoSocial').AsString := LIntermediario.RazaoSocial;

  LCDS.Post;
end;

procedure TACBrNFSeXDANFSeFR.CarregaParametros(ANFSe: TNFSe);
var
  FProvider      : IACBrNFSeXProvider;
  CodigoIBGE     : Integer;
  LMunicipio, LUF: string;
  LDadosServico  : TDadosServico;
  LCDS           : TACBrFRDataSet;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;
  LCDS      := cdsParametros;

  LCDS.EmptyDataSet;
  LCDS.Append;

  LDadosServico := ANFSe.Servico;

  LCDS.FieldByName('id_sis_legado').AsInteger := ANFSe.id_sis_legado;

  LCDS.FieldByName('OutrasInformacoes').AsString := ANFSe.OutrasInformacoes;
  LCDS.FieldByName('NaturezaOperacao').AsString  := FProvider.NaturezaOperacaoDescricao(ANFSe.NaturezaOperacao);
  LCDS.FieldByName('RegimeEspecialTributacao').AsString := FProvider.RegimeEspecialTributacaoDescricao(ANFSe.RegimeEspecialTributacao);
  LCDS.FieldByName('OptanteSimplesNacional').AsString := FProvider.SimNaoDescricao(ANFSe.OptanteSimplesNacional);
  LCDS.FieldByName('IncentivadorCultural').AsString := FProvider.SimNaoDescricao(ANFSe.IncentivadorCultural);
  LCDS.FieldByName('CodigoMunicipio').AsString := IntToStr(LDadosServico.MunicipioIncidencia);
  LCDS.FieldByName('ExigibilidadeISS').AsString := FProvider.ExigibilidadeISSDescricao(LDadosServico.ExigibilidadeISS);
  LCDS.FieldByName('MunicipioIncidencia').AsString := LDadosServico.xMunicipioIncidencia;
  LCDS.FieldByName('TipoRecolhimento').AsString := ANFSe.TipoRecolhimento;
  {
  if Provedor = proEL then
  begin
    if ANFSe.RegimeEspecialTributacao = retNenhum then
      LCDS.FieldByName('RegimeEspecialTributacao').AsString := 'Tributação Normal'
    else
      LCDS.FieldByName('RegimeEspecialTributacao').AsString := FProvider.RegimeEspecialTributacaoDescricao(ANFSe.RegimeEspecialTributacao);

    if ANFSe.OptanteSimplesNacional = snSim then
      LCDS.FieldByName('OptanteSimplesNacional').AsString := 'Optante'
    else
      LCDS.FieldByName('OptanteSimplesNacional').AsString := 'Não Optante';

    LCDS.FieldByName('IncentivadorCultural').AsString := FProvider.SimNaoDescricao(ANFSe.IncentivadorCultural);

    LCDS.FieldByName('CodigoMunicipio').AsString  := ANFSe.Prestador.Endereco.xMunicipio; //xMunicipio;
    LCDS.FieldByName('ExigibilidadeISS').AsString := FProvider.ExigibilidadeISSDescricao(LDadosServico.ExigibilidadeISS);

    LCDS.FieldByName('MunicipioIncidencia').AsString := xMunicipioIncidencia;

    if LDadosServico.Valores.IssRetido = stRetencao then
      LCDS.FieldByName('TipoRecolhimento').AsString := 'Retido na Fonte'
    else
      LCDS.FieldByName('TipoRecolhimento').AsString := 'Não Retido';

  end
  else
  begin
    LCDS.FieldByName('RegimeEspecialTributacao').AsString := FProvider.RegimeEspecialTributacaoDescricao(ANFSe.RegimeEspecialTributacao);

    if Provedor = proAdm then
    begin
      if ANFSe.OptanteSimplesNacional = snSim then
        LCDS.FieldByName('OptanteSimplesNacional').AsString := 'Simples Nacinal / MEI'
      else
        LCDS.FieldByName('OptanteSimplesNacional').AsString := 'Não Optante';
    end
    else
      LCDS.FieldByName('OptanteSimplesNacional').AsString := FProvider.SimNaoDescricao(ANFSe.OptanteSimplesNacional);

    LCDS.FieldByName('IncentivadorCultural').AsString := FProvider.SimNaoDescricao(ANFSe.IncentivadorCultural);

    LCDS.FieldByName('ExigibilidadeISS').AsString := FProvider.ExigibilidadeISSDescricao(LDadosServico.ExigibilidadeISS);
    LCDS.FieldByName('TipoRecolhimento').AsString := ANFSe.TipoRecolhimento;

    LCDS.FieldByName('CodigoMunicipio').AsString := IntToStr(LDadosServico.MunicipioIncidencia); //ANFSe.CodigoMunicipio;

    try
      LMunicipio := LDadosServico.xMunicipioIncidencia;
      if LMunicipio = '' then
        LMunicipio := 'SEM INCIDENCIA DE ISS';
      LCDS.FieldByName('MunicipioIncidencia').AsString := LMunicipio;
    except
      on E: Exception do
      begin
        LMunicipio := '';
        LUF        := '';
      end;
    end;
  end;
  }
  LCDS.FieldByName('MunicipioPrestacao').AsString := LDadosServico.MunicipioPrestacaoServico;
  LCDS.FieldByName('CodigoObra').AsString         := ANFSe.ConstrucaoCivil.CodigoObra;
  LCDS.FieldByName('Art').AsString                := ANFSe.ConstrucaoCivil.Art;

  LCDS.FieldByName('InformacoesComplementares').AsString := ANFSe.InformacoesComplementares;
  LCDS.FieldByName('ValorCredito').AsCurrency            := ANFSe.ValorCredito;

  CarregaLogoPrefeitura;
  CarregaImagemPrestadora;

  LCDS.FieldByName('Sistema').AsString := IfThen(DANFSeXClassOwner.Sistema <> '', DANFSeXClassOwner.Sistema, 'Projeto ACBr - http://acbr.sf.net');
  LCDS.FieldByName('Usuario').AsString := DANFSeXClassOwner.Usuario;
  LCDS.FieldByName('Site').AsString    := DANFSeXClassOwner.Site;

  if FDANFSeXClassOwner.Cancelada
    or (ANFSe.NfseCancelamento.DataHora <> 0)
    or (ANFSe.SituacaoNfse = snCancelado)
    or (ANFSe.StatusRps = srCancelado) then
  begin
    LCDS.FieldByName('Mensagem0').AsString := 'NFSe CANCELADA';
  end;

  if (ACBrNFSe.Configuracoes.WebServices.AmbienteCodigo = 2) then
    LCDS.FieldByName('Mensagem0').AsString := Trim(LCDS.FieldByName('Mensagem0').AsString + sLineBreak + ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL'));

  LCDS.Post;
end;

procedure TACBrNFSeXDANFSeFR.CarregaPrestador(ANFSe: TNFSe);
var
  LCDS             : TACBrFRDataSet;
  LPrestador       : TDadosPrestador;
  LConfiguracaoNFSE: TGeralConfNFSe;
begin
  LCDS := cdsPrestador;

  LCDS.EmptyDataSet;
  LCDS.Append;

  LPrestador := ANFSe.Prestador;

  LCDS.FieldByName('RazaoSocial').AsString  := LPrestador.RazaoSocial;
  LCDS.FieldByName('NomeFantasia').AsString := LPrestador.NomeFantasia;

  LCDS.FieldByName('Cnpj').AsString               := FormatarCNPJ(LPrestador.IdentificacaoPrestador.Cnpj);
  LCDS.FieldByName('InscricaoMunicipal').AsString := LPrestador.IdentificacaoPrestador.InscricaoMunicipal;
  LCDS.FieldByName('InscricaoEstadual').AsString  := FormatarIE(LPrestador.IdentificacaoPrestador.InscricaoEstadual, LPrestador.Endereco.UF);

  LCDS.FieldByName('Endereco').AsString        := LPrestador.Endereco.Endereco;
  LCDS.FieldByName('Numero').AsString          := LPrestador.Endereco.Numero;
  LCDS.FieldByName('Complemento').AsString     := LPrestador.Endereco.Complemento;
  LCDS.FieldByName('Bairro').AsString          := LPrestador.Endereco.Bairro;
  LCDS.FieldByName('CodigoMunicipio').AsString := LPrestador.Endereco.CodigoMunicipio;
  LCDS.FieldByName('UF').AsString              := LPrestador.Endereco.UF;
  LCDS.FieldByName('CEP').AsString             := FormatarCEP(LPrestador.Endereco.CEP);
  LCDS.FieldByName('xMunicipio').AsString      := LPrestador.Endereco.xMunicipio;
  LCDS.FieldByName('CodigoPais').AsString      := IntToStr(LPrestador.Endereco.CodigoPais);

  LCDS.FieldByName('Telefone').AsString := FormatarFone(LPrestador.Contato.Telefone);
  LCDS.FieldByName('Email').AsString    := LPrestador.Contato.Email;

  LConfiguracaoNFSE := TACBrNFSeX(DANFSeXClassOwner.ACBrNFSe).Configuracoes.Geral;

  if LConfiguracaoNFSE.Emitente.DadosEmitente.Endereco <> EmptyStr then
  begin
    LCDS.FieldByName('RazaoSocial').AsString  := LConfiguracaoNFSE.Emitente.RazSocial;
    LCDS.FieldByName('NomeFantasia').AsString := LConfiguracaoNFSE.Emitente.DadosEmitente.NomeFantasia;

    LCDS.FieldByName('Cnpj').AsString               := FormatarCNPJ(LConfiguracaoNFSE.Emitente.Cnpj);
    LCDS.FieldByName('InscricaoMunicipal').AsString := LConfiguracaoNFSE.Emitente.InscMun;
    LCDS.FieldByName('InscricaoEstadual').AsString  := FormatarIE(LConfiguracaoNFSE.Emitente.DadosEmitente.InscricaoEstadual, LConfiguracaoNFSE.Emitente.DadosEmitente.UF);

    LCDS.FieldByName('Endereco').AsString        := LConfiguracaoNFSE.Emitente.DadosEmitente.Endereco;
    LCDS.FieldByName('Numero').AsString          := LConfiguracaoNFSE.Emitente.DadosEmitente.Numero;
    LCDS.FieldByName('Complemento').AsString     := LConfiguracaoNFSE.Emitente.DadosEmitente.Complemento;
    LCDS.FieldByName('Bairro').AsString          := LConfiguracaoNFSE.Emitente.DadosEmitente.Bairro;
    LCDS.FieldByName('CodigoMunicipio').AsString := LConfiguracaoNFSE.Emitente.DadosEmitente.CodigoMunicipio;
    LCDS.FieldByName('UF').AsString              := LConfiguracaoNFSE.Emitente.DadosEmitente.UF;
    LCDS.FieldByName('CEP').AsString             := FormatarCEP(LConfiguracaoNFSE.Emitente.DadosEmitente.CEP);
    LCDS.FieldByName('xMunicipio').AsString      := LConfiguracaoNFSE.Emitente.DadosEmitente.Municipio;

    LCDS.FieldByName('Telefone').AsString := FormatarFone(LConfiguracaoNFSE.Emitente.DadosEmitente.Telefone);
    LCDS.FieldByName('Email').AsString    := LConfiguracaoNFSE.Emitente.DadosEmitente.Email;
  end;

  LCDS.Post;

end;

procedure TACBrNFSeXDANFSeFR.CarregaServicos(ANFSe: TNFSe);
var
  FProvider   : IACBrNFSeXProvider;
  LCDS        : TACBrFRDataSet;
  LServico    : TDadosServico;
  LValores    : TValores;
  LValoresNFSe: TValoresNfse;
begin
  FProvider := TACBrNFSeX(FACBrNFSe).Provider;

  LServico     := ANFSe.Servico;
  LValores     := LServico.Valores;
  LValoresNFSe := ANFSe.ValoresNfse;

  LCDS := cdsServicos;

  LCDS.EmptyDataSet;
  LCDS.Append;

  if LServico.ItemServico.Count > 0 then
  begin
    LCDS.FieldByName('ItemListaServico').AsString := LServico.ItemServico.Items[0].ItemListaServico;
    LCDS.FieldByName('xItemListaServico').AsString := LServico.ItemServico.Items[0].xItemListaServico;
  end;

  if LCDS.FieldByName('xItemListaServico').AsString = '' then
  begin
    LCDS.FieldByName('ItemListaServico').AsString  := LServico.ItemListaServico;
    LCDS.FieldByName('xItemListaServico').AsString := LServico.xItemListaServico;
  end;

  LCDS.FieldByName('CodigoCnae').AsString                := LServico.CodigoCnae;
  LCDS.FieldByName('CodigoNbs').AsString                 := LServico.CodigoNBS;
  LCDS.FieldByName('CodigoTributacaoMunicipio').AsString := LServico.CodigoTributacaoMunicipio;
  LCDS.FieldByName('Discriminacao').AsString             := StringReplace(LServico.Discriminacao, TACBrNFSeX(DANFSeXClassOwner.ACBrNFSe).Provider.ConfigGeral.QuebradeLinha, #13,
    [ rfReplaceAll, rfIgnoreCase ]);
  LCDS.FieldByName('CodigoPais').AsString          := IntToStr(LServico.CodigoPais);
  LCDS.FieldByName('NumeroProcesso').AsString      := LServico.NumeroProcesso;
  LCDS.FieldByName('Descricao').AsString           := LServico.Descricao;
  LCDS.FieldByName('ResponsavelRetencao').AsString := FProvider.ResponsavelRetencaoToStr(LServico.ResponsavelRetencao);
  LCDS.FieldByName('Tributacao').AsString          := FProvider.TributacaoToStr(LServico.Tributacao);

  LCDS.FieldByName('ValorServicos').AsFloat          := LValores.ValorServicos;
  LCDS.FieldByName('ValorDeducoes').AsFloat          := LValores.ValorDeducoes;
  LCDS.FieldByName('ValorPis').AsFloat               := LValores.ValorPis;
  LCDS.FieldByName('ValorCofins').AsFloat            := LValores.ValorCofins;
  LCDS.FieldByName('ValorInss').AsFloat              := LValores.ValorInss;
  LCDS.FieldByName('ValorIr').AsFloat                := LValores.ValorIr;
  LCDS.FieldByName('ValorCsll').AsFloat              := LValores.ValorCsll;
  LCDS.FieldByName('IssRetido').AsString             := FProvider.SituacaoTributariaDescricao(LValores.IssRetido);
  LCDS.FieldByName('ValorIss').AsFloat               := LValores.ValorIss;
  LCDS.FieldByName('OutrasRetencoes').AsFloat        := LValores.OutrasRetencoes;
  LCDS.FieldByName('BaseCalculo').AsFloat            := LValores.BaseCalculo;
  LCDS.FieldByName('Aliquota').AsFloat               := LValores.Aliquota;
  LCDS.FieldByName('ValorLiquidoNfse').AsFloat       := LValores.ValorLiquidoNfse;
  LCDS.FieldByName('ValorIssRetido').AsFloat         := LValores.ValorIssRetido;
  LCDS.FieldByName('DescontoCondicionado').AsFloat   := LValores.DescontoCondicionado;
  LCDS.FieldByName('DescontoIncondicionado').AsFloat := LValores.DescontoIncondicionado;
  LCDS.FieldByName('OutrosDescontos').AsCurrency     := LValores.OutrosDescontos;
  LCDS.FieldByName('ValorTotalNotaFiscal').AsCurrency := LValores.ValorTotalNotaFiscal;

  if LValores.IssRetido = stRetencao then
  begin
    LCDS.FieldByName('DescricaoTotalRetDemo').AsString     := 'TOTAL RETENÇÕES';
    LCDS.FieldByName('DescriçãoTributosFederais').AsString := 'RETENÇÕES DOS TRIBUTOS FEDERAIS';
  end
  else
  begin
    LCDS.FieldByName('DescricaoTotalRetDemo').AsString     := 'TOTAL DEMONSTRATIVO';
    LCDS.FieldByName('DescriçãoTributosFederais').AsString := 'DEMONSTRATIVO DOS TRIBUTOS FEDERAIS';
  end;

    // Provedor SP
  LCDS.FieldByName('ValorCargaTributaria').AsCurrency      := LServico.ValorCargaTributaria;
  LCDS.FieldByName('PercentualCargaTributaria').AsCurrency := LServico.PercentualCargaTributaria;
  LCDS.FieldByName('FonteCargaTributaria').AsString        := LServico.FonteCargaTributaria;

  if LValoresNFSe.ValorIss > 0 then
  begin
    LCDS.FieldByName('ValorServicos').AsFloat := LValores.ValorServicos;
    LCDS.FieldByName('ValorIss').AsFloat      := LValoresNFSe.ValorIss;
    LCDS.FieldByName('BaseCalculo').AsFloat   := LValoresNFSe.BaseCalculo;
    if LValoresNFSe.Aliquota <> 0 then
      LCDS.FieldByName('Aliquota').AsFloat      := LValoresNFSe.Aliquota;

    if (LValoresNFSe.ValorLiquidoNfse = 0) and (LValores.ValorLiquidoNfse = 0) then
      LValoresNFSe.ValorLiquidoNfse := LValoresNFSe.BaseCalculo
    else
    if (LValoresNFSe.ValorLiquidoNfse = 0) and (LValores.ValorLiquidoNfse > 0) then
      LValoresNFSe.ValorLiquidoNfse := LValores.ValorLiquidoNfse;

    LCDS.FieldByName('ValorLiquidoNfse').AsFloat := LValoresNFSe.ValorLiquidoNfse;
  end;

  LCDS.Post;
end;

procedure TACBrNFSeXDANFSeFR.CarregaTomador(ANFSe: TNFSe);
var
  LCDS     : TACBrFRDataSet;
  LTomador : TDadosTomador;
  LEndereco: TEndereco;
  LContato : TContato;
begin

  LTomador  := ANFSe.Tomador;
  LEndereco := LTomador.Endereco;
  LContato  := LTomador.Contato;

  LCDS := cdsTomador;
  LCDS.EmptyDataSet;
  LCDS.Append;

  LCDS.FieldByName('RazaoSocial').AsString        := LTomador.RazaoSocial;
  LCDS.FieldByName('CpfCnpj').AsString            := ManterDocumento(LTomador.IdentificacaoTomador.CpfCnpj);
  LCDS.FieldByName('InscricaoMunicipal').AsString := LTomador.IdentificacaoTomador.InscricaoMunicipal;
  LCDS.FieldByName('InscricaoEstadual').AsString  := LTomador.IdentificacaoTomador.InscricaoEstadual;

  LCDS.FieldByName('Endereco').AsString        := LEndereco.Endereco;
  LCDS.FieldByName('Numero').AsString          := LEndereco.Numero;
  LCDS.FieldByName('Complemento').AsString     := LEndereco.Complemento;
  LCDS.FieldByName('Bairro').AsString          := LEndereco.Bairro;
  LCDS.FieldByName('CodigoMunicipio').AsString := LEndereco.CodigoMunicipio;
  LCDS.FieldByName('UF').AsString              := LEndereco.UF;
  LCDS.FieldByName('CEP').AsString             := FormatarCEP(LEndereco.CEP);
  LCDS.FieldByName('xMunicipio').AsString      := LEndereco.xMunicipio;
  LCDS.FieldByName('CodigoPais').AsString      := IntToStr(LEndereco.CodigoPais);

  LCDS.FieldByName('Telefone').AsString := FormatarFone(LContato.Telefone);
  LCDS.FieldByName('Email').AsString    := LContato.Email;

  LCDS.Post;

end;

procedure TACBrNFSeXDANFSeFR.CarregaTransportadora(ANFSe: TNFSe);
var
  LCDS           : TACBrFRDataSet;
  LTransportadora: TDadosTransportadora;
begin
  LCDS            := cdsTransportadora;
  LTransportadora := ANFSe.Transportadora;

  LCDS.EmptyDataSet;
  LCDS.Append;

  LCDS.FieldByName('Cnpj').AsString             := LTransportadora.xCpfCnpjTrans;
  LCDS.FieldByName('RazaoSocial').AsString      := LTransportadora.xNomeTrans;
  LCDS.FieldByName('InscicaoEstadual').AsString := LTransportadora.xInscEstTrans;
  LCDS.FieldByName('Placa').AsString            := LTransportadora.xPlacaTrans;
  LCDS.FieldByName('Endereco').AsString         := LTransportadora.xEndTrans;
  LCDS.FieldByName('CodigoMunicipio').AsInteger := LTransportadora.cMunTrans;
  LCDS.FieldByName('NomeMunicipio').AsString    := LTransportadora.xMunTrans;
  LCDS.FieldByName('Sigla').AsString            := LTransportadora.xUFTrans;
  LCDS.FieldByName('NomePais').AsString         := LTransportadora.xPaisTrans;
  LCDS.FieldByName('BacenPais').AsInteger       := LTransportadora.cPaisTrans;
  LCDS.FieldByName('TipoFrete').AsInteger       := Ord(LTransportadora.vTipoFreteTrans);

  LCDS.Post;
end;

procedure TACBrNFSeXDANFSeFR.CarregaLogoPrefeitura;
var
  vStream      : TMemoryStream;
  vStringStream: TStringStream;
begin
  cdsParametros.FieldByName('LogoPrefExpandido').AsString := IfThen(ExpandeLogoMarca, '0', '1'); // Prefeitura
  cdsParametros.FieldByName('Nome_Prefeitura').AsString := Prefeitura;
  if NaoEstaVazio(DANFSeXClassOwner.Logo) then
  begin
    cdsParametros.FieldByName('imgPrefeitura').AsString := Logo;
    vStream                                             := TMemoryStream.Create;
    try
      if FileExists(Logo) then
        vStream.LoadFromFile(Logo)
      else
      begin
        vStringStream := TStringStream.Create(Logo);
        try
          vStream.LoadFromStream(vStringStream);
        finally
          vStringStream.Free;
        end;
      end;
      vStream.Position := 0;
      TBlobField(cdsParametros.FieldByName('LogoPrefCarregado')).LoadFromStream(vStream);
    finally
      vStream.Free;
    end;
  end;
end;

procedure TACBrNFSeXDANFSeFR.CarregaImagemPrestadora;
var
  vStream      : TMemoryStream;
  vStringStream: TStringStream;
begin
  cdsParametros.FieldByName('LogoExpandido').AsString := IfThen(ExpandeLogoMarca, '0', '1'); // Prestador

  if NaoEstaVazio(Prestador.Logo) then
  begin
    cdsParametros.FieldByName('Imagem').AsString := Prestador.Logo;

    vStream := TMemoryStream.Create;
    try
      if FileExists(Prestador.Logo) then
        vStream.LoadFromFile(Prestador.Logo)
      else
      begin
        vStringStream := TStringStream.Create(Prestador.Logo);
        try
          vStream.LoadFromStream(vStringStream);
        finally
          vStringStream.Free;
        end;
      end;
      vStream.Position := 0;
      TBlobField(cdsParametros.FieldByName('LogoCarregado')).LoadFromStream(vStream);
    finally
      vStream.Free;
    end;
  end;
end;

function TACBrNFSeXDANFSeFR.ManterDocumento(const sCpfCnpj: String): string;
begin
  Result := sCpfCnpj;
  if NaoEstaVazio(Result) then
  begin
    if length(Result) > 11 then
      Result := FormatarCNPJ(Result)
    else
      Result := FormatarCPF(Result);
  end;
end;

procedure TACBrNFSeXDANFSeFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  LQrCode, LOutrasInformacoes: String;
  LOutrasInformacoesLength   : Integer;
  LDiscriminacao, LNomeArqFr3: string;
begin
  if Provedor <> proEL then
  begin
      // validando se encontra cada memo no relatório (permitindo melhor personalização dos DANFSe)
    if frxReport.FindObject('Memo23') <> nil then
      frxReport.FindObject('Memo23').Visible := DANFSeXClassOwner.ImprimeCanhoto;
    if frxReport.FindObject('Memo75') <> nil then
      frxReport.FindObject('Memo75').Visible := DANFSeXClassOwner.ImprimeCanhoto;
    if frxReport.FindObject('Memo77') <> nil then
      frxReport.FindObject('Memo77').Visible := DANFSeXClassOwner.ImprimeCanhoto;
    if frxReport.FindObject('Memo68') <> nil then
      frxReport.FindObject('Memo68').Visible := DANFSeXClassOwner.ImprimeCanhoto;
    if frxReport.FindObject('Memo73') <> nil then
      frxReport.FindObject('Memo73').Visible := DANFSeXClassOwner.ImprimeCanhoto;
  end;

  frxReport.FindObject('Memo13').Visible := (not ((cdsItensServico.RecordCount > 0) and (frxReport.FindObject('Page2') <> nil)) or (frxReport.FindObject('Page2') = nil));

  LOutrasInformacoes       := LowerCase(cdsParametros.FieldByName('outrasinformacoes').Value);
  LOutrasInformacoesLength := length(LOutrasInformacoes);

  LQrCode := cdsIdentificacao.FieldByName('LinkNFSe').Value;

  if Assigned(Sender) and (Sender.Name = 'imgQrCode') then
  begin
    TfrxPictureView(Sender).Visible := not (LQrCode = '');
    if (LQrCode <> '') then
      PintarQRCode(LQrCode, TfrxPictureView(Sender).Picture.Bitmap, qrAuto);
  end;
end;

end.
