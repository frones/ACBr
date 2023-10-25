{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrMDFeDAMDFEFR;

interface

uses
  SysUtils, Classes, DB, DBClient, ACBrBase, ACBrMDFeDAMDFeClass, pcnConversao,
  pmdfeMDFe, frxClass, ACBrDFeUtil, pmdfeEnvEventoMDFe, frxDBSet,
  frxExportPDF, frxBarcode;

type
  EACBrMDFeDAMDFEFR = class(Exception);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrMDFeDAMDFEFR = class(TACBrMDFeDAMDFEClass)
  private
    FDAMDFEClassOwner: TACBrMDFeDAMDFeClass;
    FMDFe            : TMDFe;
    FEvento          : TEventoMDFe;
    FFastFile:            string;
    FFastFileEvento:      string;
    FEspessuraBorda:      Integer;
    FSelecionaImpressora: Boolean;
    FTamanhoCanhoto:      Extended;
    FAlturaEstampaFiscal: Extended;

    cdsIdentificacao: TClientDataSet;
    cdsEmitente: TClientDataSet;
    cdsModalAereo: TClientDataSet;
    cdsModalAqua: TClientDataSet;
    cdsDocumentos: TClientDataSet;
    cdsMunCarrega: TClientDataSet;
    cdsModalRodo: TClientDataSet;
    cdsParametros: TClientDataSet;
    cdsPercurso: TClientDataSet;
    cdsModalFerrov: TClientDataSet;
    cdsModalFerrovVagoes: TClientDataSet;
    cdsEventos: TClientDataSet;
    cdsTermCarrega: TClientDataSet;
    cdsTermDescarrega: TClientDataSet;
    cdsEmbarcaComboio: TClientDataSet;
    cdsInfUnidCargaVazia: TClientDataSet;
    cdsUnidTranspVazia: TClientDataSet;
    cdsSeg: TClientDataSet;
    cdsContratantes: TClientDataSet;
    cdsPeri: TClientDataSet;

    frxIdentificacao: TfrxDBDataset;
    frxEmitente: TfrxDBDataset;
    frxModalAereo: TfrxDBDataset;
    frxModalAqua: TfrxDBDataset;
    frxDocumentos: TfrxDBDataset;
    frxMunCarrega: TfrxDBDataset;
    frxModalRodo: TfrxDBDataset;
    frxParametros: TfrxDBDataset;
    frxPercurso: TfrxDBDataset;
    frxModalFerrov: TfrxDBDataset;
    frxModalFerrovVagoes: TfrxDBDataset;
    frxEventos: TfrxDBDataset;
    frxTermCarrega: TfrxDBDataset;
    frxTermDescarrega: TfrxDBDataset;
    frxEmbarcaComboio: TfrxDBDataset;
    frxInfUnidCargaVazia: TfrxDBDataset;
    frxUnidTranspVazia: TfrxDBDataset;
    frxSeg: TfrxDBDataset;
    frxPeri: TfrxDBDataset;
    frxContratantes: TfrxDBDataset;
    FExibirMunicipioDescarregamento: Boolean;

    procedure CarregaIdentificacao;
    procedure CarregaParametros;
    procedure CarregaEmitente;
    procedure CarregaDocumentos;
    procedure CarregaModal;
    procedure CarregaModalRodoviario;
    procedure CarregaModalAereo;
    procedure CarregaModalAquaviario;

    procedure CarregaTermCarreg;
    procedure CarregaTermDescarreg;
    procedure CarregaEmbarcacaoComboio;
    procedure CarregaInfUnidCargaVazia;
    procedure CarregaInfUnidTranspVazia;

    procedure CarregaModalFerroviario;
    procedure CarregaMunCarrega;
    procedure CarregaPercurso;
    procedure CarregaSeguro;
    procedure CarregaContratantes;
    procedure CarregaPeri;

    function  GetPreparedReport: TfrxReport;
    function  GetPreparedReportEvento: TfrxReport;
    function  PrepareReport(AMDFe: TMDFe = nil): Boolean;
    function  PrepareReportEvento: Boolean;
    procedure CriarDataSetsFrx;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
    procedure AjustaMargensReports;
  public
    frxReport: TfrxReport;
    frxPDFExport: TfrxPDFExport;
    frxBarCodeObject: TfrxBarCodeObject;
    VersaoDAMDFe: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDAMDFe(AMDFe: TMDFe = nil); override;
    procedure ImprimirDAMDFePDF(AMDFe: TMDFe = nil); override;
    procedure ImprimirDAMDFePDF(AStream: TStream; AMDFe: TMDFe = nil); override;
    procedure ImprimirEVENTO(AMDFe: TMDFe = nil); override;
    procedure ImprimirEVENTOPDF(AMDFe: TMDFe = nil); override;

    procedure CarregaDados;
    procedure LimpaDados;
    procedure CarregaDadosEventos;
    procedure SetDataSetsToFrxReport;
    procedure frxReportGetValue(const VarName: string; var Value: Variant);

    property MDFe            : TMDFe read FMDFe write FMDFe;
    property Evento          : TEventoMDFe read FEvento write FEvento;
    property DAMDFEClassOwner: TACBrMDFeDAMDFeClass read FDAMDFEClassOwner;
    property PreparedReport:       TfrxReport read GetPreparedReport;
    property PreparedReportEvento: TfrxReport read GetPreparedReportEvento;
  published
    property FastFile:             string read FFastFile write FFastFile;
    property FastFileEvento:       string read FFastFileEvento write FFastFileEvento;
    property SelecionaImpressora:  Boolean read FSelecionaImpressora write FSelecionaImpressora;
    property EspessuraBorda:       Integer read FEspessuraBorda write FEspessuraBorda;
    property TamanhoCanhoto:       Extended read FTamanhoCanhoto write FTamanhoCanhoto;
    property AlturaEstampaFiscal:  Extended read FAlturaEstampaFiscal write FAlturaEstampaFiscal;
    property ExibirMunicipioDescarregamento: Boolean read FExibirMunicipioDescarregamento write FExibirMunicipioDescarregamento;
  end;

implementation

uses
  StrUtils,
  ACBrMDFe, pmdfeConversaoMDFe,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrUtil.Base,
  ACBrValidador, ACBrImage, ACBrDelphiZXingQRCode;

function CollateBr(Str: string): string;
var
  Resultado, Temp: string;
  vChar          : Char;
  Tamanho, i     : integer;
begin
  Result  := '';
  Tamanho := Length(Str);
  i       := 1;
  while i <= Tamanho do
  begin
    Temp  := Copy(Str, i, 1);
    vChar := Temp[1];
    case vChar of
      'á', 'â', 'ã', 'à', 'ä', 'å', 'Á', 'Â', 'Ã', 'À', 'Ä', 'Å':
        Resultado := 'A';
      'é', 'ê', 'è', 'ë', 'É', 'Ê', 'È', 'Ë':
        Resultado := 'E';
      'í', 'î', 'ì', 'ï', 'Í', 'Î', 'Ì', 'Ï':
        Resultado := 'I';
      'ó', 'ô', 'õ', 'ò', 'ö', 'Ó', 'Ô', 'Õ', 'Ò', 'Ö':
        Resultado := 'O';
      'ú', 'û', 'ù', 'ü', 'Ú', 'Û', 'Ù', 'Ü':
        Resultado := 'U';
      'ç', 'Ç':
        Resultado := 'C';
      'ñ', 'Ñ':
        Resultado := 'N';
      'ý', 'ÿ', 'Ý', 'Y':
        Resultado := 'Y';
    else
      if vChar > #127 then
        Resultado := #32

{$IFDEF DELPHI12_UP}
      else if CharInset(vChar, ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', ' ']) then

{$ELSE}
      else if vChar in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', ' '] then

{$ENDIF}
        Resultado := UpperCase(vChar);
    end;
    Result := Result + Resultado;
    i      := i + 1;
  end;
end;

constructor TACBrMDFeDAMDFEFR.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FDAMDFEClassOwner := TACBrMDFeDAMDFeClass(Self);
  FFastFile       := '';
  FEspessuraBorda := 1;
  FExibirMunicipioDescarregamento := False;
  CriarDataSetsFrx;
end;

procedure TACBrMDFeDAMDFEFR.CriarDataSetsFrx;
begin
  frxReport := TfrxReport.Create(FDAMDFEClassOwner);

  with frxReport do
  begin
    EngineOptions.UseGlobalDataSetList := False;
    ScriptLanguage := 'PascalScript';
    StoreInDFM     := False;
    OnGetValue := frxReportGetValue;
    OnBeforePrint := frxReportBeforePrint;
    OnReportPrint := 'frxReportOnReportPrint';
    PreviewOptions.Buttons :=[pbExport, pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick];
  end;

  frxPDFExport := TfrxPDFExport.Create(FDAMDFEClassOwner);
  frxPDFExport.PrintOptimized := True;
  frxPDFExport.ShowProgress := False;

  RttiSetProp(frxPDFExport, 'Transparency', 'False');

  frxBarCodeObject := TfrxBarCodeObject.Create(Self);

  cdsIdentificacao := TClientDataSet.Create(Self);
  with cdsIdentificacao, FieldDefs do
  begin
    Close;
    Clear;
    Add('Id', ftString, 44);
    Add('Chave', ftString, 60);
    Add('Protocolo', ftString, 120);
    Add('tpAmb', ftInteger);
    Add('tpEmit', ftInteger);
    Add('Modelo', ftString, 5);
    Add('serie', ftString, 3);
    Add('nMDF', ftString, 15);
    Add('modal', ftInteger);
    Add('dhEmi', ftDateTime);
    Add('tpEmis', ftInteger);
    Add('UFIni', ftString, 2);
    Add('UFFim', ftString, 2);
    Add('OBS', ftMemo);

    // Totalização
    Add('qCTe', ftInteger);
    Add('qCT', ftInteger);
    Add('qNFe', ftInteger);
    Add('qNF', ftInteger);
    Add('qMDFe', ftInteger);
    Add('qCarga', ftCurrency);
    Add('dhIniViagem', ftDateTime);
    Add('Lacres', ftMemo);
    Add('vCarga', ftCurrency);

    // Outros
    Add('qDescPeso', ftString, 20);
    Add('URL', ftString, 1000);

    CreateDataSet;
  end;

  cdsEmitente := TClientDataSet.Create(Self);
  with cdsEmitente, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('IE', ftString, 14);
    Add('xNome', ftString, 60);
    Add('xFant', ftString, 60);
    Add('xLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('xCpl', ftString, 60);
    Add('xBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('xMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('Fone', ftString, 15);
    Add('email', ftString, 60);
    Add('site', ftString, 60);
    CreateDataSet;
  end;

  cdsModalAereo := TClientDataSet.Create(Self);
  with cdsModalAereo, FieldDefs do
  begin
    Close;
    Clear;
    // Aereo
    Add('nac', ftString, 4);
    Add('matr', ftString, 6);
    Add('nVoo', ftString, 9);
    Add('cAerEmb', ftString, 4);
    Add('cAerDes', ftString, 4);
    Add('dVoo', ftDateTime);
    CreateDataSet;
  end;

  cdsModalAqua := TClientDataSet.Create(Self);
  with cdsModalAqua, FieldDefs do
  begin
    Close;
    Clear;
    // Aquaviario
    Add('CNPJAgeNav', ftString, 18);
    Add('tpEmb', ftString, 2);
    Add('cEmbar', ftString, 10);
    Add('xEmbar', ftString, 60);
    Add('nViag', ftString, 10);
    Add('cPrtEmb', ftString, 5);
    Add('cPrtDest', ftString, 5);
    CreateDataSet;
  end;

  cdsDocumentos := TClientDataSet.Create(Self);
  with cdsDocumentos, FieldDefs do
  begin
    Close;
    Clear;
    Add('Tipo', ftString, 5);
    Add('cMunDescarga', ftInteger);
    Add('xMunDescarga', ftString, 60);
    Add('Chave', ftString, 70);
    Add('idUnidTransp', ftString, 70);
    Add('idUnidCarga', ftString, 70);
    CreateDataSet;
  end;

  cdsMunCarrega := TClientDataSet.Create(Self);
  with cdsMunCarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('xMunCarrega', ftString, 60);
    CreateDataSet;
  end;

  cdsModalRodo := TClientDataSet.Create(Self);
  with cdsModalRodo, FieldDefs do
  begin
    Close;
    Clear;
    Add('RNTRC', ftString, 8);
    Add('CIOT', ftString, 12);
    // Vale Pedagio
    Add('CNPJForn', ftMemo);
    Add('CNPJPg', ftMemo);
    Add('nCompra', ftMemo);
    Add('vValePed', ftMemo);
    // Veiculos
    Add('placa', ftMemo);
    Add('RNTRCProp', ftMemo);
    Add('RENAVAM', ftMemo);
    Add('CNPJCPFProp', ftMemo);
    // Condutor
    Add('xNome', ftMemo);
    Add('CPF', ftMemo);
    //infCIOT
    Add('infCIOT_CIOT', ftString, 12);
    Add('infCIOT_CNPJCPF', ftString, 20);
    Add('codAgPorto', ftString,16);
    CreateDataSet;
  end;

  cdsParametros := TClientDataSet.Create(Self);
  with cdsParametros, FieldDefs do
  begin
    Close;
    Clear;
    Add('Versao', ftString, 5);
    Add('Imagem', ftString, 256);
    Add('Sistema', ftString, 150);
    Add('Usuario', ftString, 60);
    Add('QrCodeCarregado', ftGraphic, 1000);
    Add('LogoCarregado', ftBlob);
    CreateDataSet;
  end;

  cdsPercurso := TClientDataSet.Create(Self);
  with cdsPercurso, FieldDefs do
  begin
    Close;
    Clear;
    Add('UFPer', ftString, 2);
    CreateDataSet;
  end;

  cdsModalFerrov := TClientDataSet.Create(Self);
  with cdsModalFerrov, FieldDefs do
  begin
    Close;
    Clear;
    Add('xPref', ftString, 10);
    Add('dhTrem', ftDateTime);
    Add('xOri', ftString, 3);
    Add('xDest', ftString, 3);
    Add('qVag', ftInteger);
    CreateDataSet;
  end;

  cdsModalFerrovVagoes := TClientDataSet.Create(Self);
  with cdsModalFerrovVagoes, FieldDefs do
  begin
    Close;
    Clear;
    Add('serie', ftString, 3);
    Add('nVag', ftInteger);
    Add('nSeq', ftInteger);
    Add('TU', ftCurrency);
    CreateDataSet;
  end;

  cdsTermCarrega := TClientDataSet.Create(Self);
  with cdsTermCarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('cTermCarreg', ftString, 8);
    Add('xTermCarreg', ftString, 60);
    CreateDataSet;
  end;

  cdsTermDescarrega := TClientDataSet.Create(Self);
  with cdsTermDescarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('cTermDescarreg', ftString, 8);
    Add('xTermDescarreg', ftString, 60);
    CreateDataSet;
  end;

  cdsEmbarcaComboio := TClientDataSet.Create(Self);
  with cdsEmbarcaComboio, FieldDefs do
  begin
    Close;
    Clear;
    Add('cEmbComb', ftString, 10);
    CreateDataSet;
  end;

  cdsEventos := TClientDataSet.Create(Self);
  with cdsEventos, FieldDefs do
  begin
    Close;
    Clear;
    Add('DescricaoTipoEvento', ftString, 150);
    Add('Modelo', ftString, 2);
    Add('Serie', ftString, 3);
    Add('Numero', ftString, 9);
    Add('MesAno', ftString, 5);
    Add('Barras', ftString, 44);
    Add('ChaveAcesso', ftString, 60);
    Add('cOrgao', ftInteger);
    Add('tpAmb', ftString, 100);
    Add('dhEvento', ftDateTime);
    Add('TipoEvento', ftString, 6);
    Add('DescEvento', ftString, 100);
    Add('nSeqEvento', ftInteger);
    Add('versaoEvento', ftString, 10);
    Add('cStat', ftInteger);
    Add('xMotivo', ftString, 100);
    Add('nProt', ftString, 20);
    Add('dhRegEvento', ftDateTime);
    Add('xJust', ftBlob);
    Add('xCondUso', ftBlob);
    Add('xCorrecao', ftBlob);
    Add('xNome', ftString, 60); // Condutor
    Add('CPF', ftString, 14);
    Add('nProtEvento', ftString, 15);
    Add('dtEnc', ftDateTime);
    Add('cUf', ftInteger);
    Add('cMun', ftInteger);
    CreateDataSet;
  end;

  cdsInfUnidCargaVazia := TClientDataSet.Create(Self);
  with cdsInfUnidCargaVazia, FieldDefs do
  begin
    Close;
    Clear;
    Add('idUnidCargaVazia', ftString, 20);
    Add('tpUnidCargaVazia', ftString, 20);
    CreateDataSet;
  end;

  cdsUnidTranspVazia := TClientDataSet.Create(Self);
  with cdsUnidTranspVazia, FieldDefs do
  begin
    Close;
    Clear;
    Add('idUnidTranspVazia', ftString, 20);
    Add('tpUnidTranspVazia', ftString, 20);
    CreateDataSet;
  end;

  cdsSeg := TClientDataSet.Create(Self);
  with cdsSeg, FieldDefs do
  begin
    Close;
    Clear;
    Add('infResp_respSeg', ftInteger);
    Add('infResp_respSegStr', ftString, 20);
    Add('infResp_CNPJCPF', ftString, 20);
    Add('infSeg_xSeg', ftString, 30);
    Add('infSeg_CNPJ', ftString, 20);
    Add('nApol', ftString, 20);
    Add('nAver', ftMemo);
    CreateDataSet;
  end;

  cdsContratantes := TClientDataSet.Create(Self);
  with cdsContratantes, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJCPF',ftString,20);
    CreateDataSet;
  end;

  cdsPeri := TClientDataSet.Create(Self);
  with cdsPeri, FieldDefs do
  begin
    Close;
    Clear;
    Add('Chave', ftString, 44);
    Add('nONU', ftString, 4);
    Add('xNomeAE', ftString, 150);
    Add('xClaRisco', ftString, 40);
    Add('grEmb', ftString, 6);
    Add('qTotProd', ftString, 20);
    Add('qVolTipo', ftString, 60);
    CreateDataSet;
  end;

  frxIdentificacao := TfrxDBDataset.Create(Self);
  with frxIdentificacao do
  begin
    UserName := 'Identificacao';
    OpenDataSource := False;
    DataSet := cdsIdentificacao;
  end;

  frxEmitente := TfrxDBDataset.Create(Self);
  with frxEmitente do
  begin
    UserName := 'Emitente';
    OpenDataSource := False;
    DataSet := cdsEmitente;
  end;

  frxModalAereo := TfrxDBDataset.Create(Self);
  with frxModalAereo do
  begin
    UserName := 'ModalAereo';
    OpenDataSource := False;
    DataSet := cdsModalAereo;
  end;

  frxModalAqua := TfrxDBDataset.Create(Self);
  with frxModalAqua do
  begin
    UserName := 'ModalAqua';
    OpenDataSource := False;
    DataSet := cdsModalAqua;
  end;

  frxDocumentos := TfrxDBDataset.Create(Self);
  with frxDocumentos do
  begin
    UserName := 'Documentos';
    OpenDataSource := False;
    DataSet := cdsDocumentos;
  end;

  frxMunCarrega := TfrxDBDataset.Create(Self);
  with frxMunCarrega do
  begin
    UserName := 'MunCarrega';
    OpenDataSource := False;
    DataSet := cdsMunCarrega;
  end;

  frxModalRodo := TfrxDBDataset.Create(Self);
  with frxModalRodo do
  begin
    UserName := 'ModalRodo';
    OpenDataSource := False;
    DataSet := cdsModalRodo;
  end;

  frxParametros := TfrxDBDataset.Create(Self);
  with frxParametros do
  begin
    UserName := 'Parametros';
    OpenDataSource := False;
    DataSet := cdsParametros;
  end;

  frxPercurso := TfrxDBDataset.Create(Self);
  with frxPercurso do
  begin
    UserName := 'Percurso';
    OpenDataSource := False;
    DataSet := cdsPercurso;
  end;

  frxModalFerrov := TfrxDBDataset.Create(Self);
  with frxModalFerrov do
  begin
    UserName := 'ModalFerrov';
    OpenDataSource := False;
    DataSet := cdsModalFerrov;
  end;

  frxModalFerrovVagoes := TfrxDBDataset.Create(Self);
  with frxModalFerrovVagoes do
  begin
    UserName := 'ModalFerrovVagoes';
    OpenDataSource := False;
    DataSet := cdsModalFerrovVagoes;
  end;

  frxEventos := TfrxDBDataset.Create(Self);
  with frxEventos do
  begin
    UserName := 'Eventos';
    OpenDataSource := False;
    DataSet := cdsEventos;
  end;

  frxTermCarrega := TfrxDBDataset.Create(Self);
  with frxTermCarrega do
  begin
    UserName := 'TermCarrega';
    OpenDataSource := False;
    DataSet := cdsTermCarrega;
  end;

  frxTermDescarrega := TfrxDBDataset.Create(Self);
  with frxTermDescarrega do
  begin
    UserName := 'TermDescarrega';
    OpenDataSource := False;
    DataSet := cdsTermDescarrega;
  end;

  frxEmbarcaComboio := TfrxDBDataset.Create(Self);
  with frxEmbarcaComboio do
  begin
    UserName := 'EmbarcaComboio';
    OpenDataSource := False;
    DataSet := cdsEmbarcaComboio;
  end;

  frxInfUnidCargaVazia := TfrxDBDataset.Create(Self);
  with frxInfUnidCargaVazia do
  begin
    UserName := 'InfUnidCargaVazia';
    OpenDataSource := False;
    DataSet := cdsInfUnidCargaVazia;
  end;

  frxUnidTranspVazia := TfrxDBDataset.Create(Self);
  with frxUnidTranspVazia do
  begin
    UserName := 'InfUnidTranspVazia';
    OpenDataSource := False;
    DataSet := cdsUnidTranspVazia;
  end;

  frxSeg := TfrxDBDataset.Create(Self);
  with frxSeg do
  begin
    UserName        := 'Seg';
    OpenDataSource  := False;
    DataSet         := cdsSeg;
  end;

  frxContratantes := TfrxDBDataset.Create(Self);
  with frxContratantes do
  begin
    UserName := 'Contratantes';
    OpenDataSource := False;
    DataSet := cdsContratantes;
  end;

  frxPeri := TfrxDBDataset.Create(Self);
  with frxPeri do
  begin
    UserName := 'Peri';
    OpenDataSource := False;
    DataSet := cdsPeri;
  end;

end;

destructor TACBrMDFeDAMDFEFR.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrMDFeDAMDFEFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  qrCode: String;
begin
  if Assigned(FMDFe) then
  begin
    qrCode := FMDFe.infMDFeSupl.qrCodMDFe;
  if Assigned(Sender) and (Trim(qrCode) <> '') and (Sender.Name = 'ImgQrCode') then
     PintarQRCode(qrCode, TfrxPictureView(Sender).Picture.Bitmap, qrUTF8NoBOM);
  end;
end;

procedure TACBrMDFeDAMDFEFR.frxReportGetValue(const VarName: string; var Value: Variant);
begin
  if VarName = 'CANCELADO' then
    Value := (DAMDFEClassOwner.Cancelada) or (FMDFe.procMDFe.cStat = 101);
  if VarName = 'ENCERRADO' then
    Value := DAMDFEClassOwner.Encerrado;
  if VarName = 'PREVISAO' then
    Value := False;
end;

procedure TACBrMDFeDAMDFEFR.AjustaMargensReports;
var
  Page: TfrxReportPage;
  I: Integer;
begin
  for I := 0 to (frxReport.PreviewPages.Count - 1) do
  begin
    Page := frxReport.PreviewPages.Page[I];
    if (MargemSuperior > 0) then
      Page.TopMargin := MargemSuperior;
    if (MargemInferior > 0) then
      Page.BottomMargin := MargemInferior;
    if (MargemEsquerda > 0) then
      Page.LeftMargin := MargemEsquerda;
    if (MargemDireita > 0) then
      Page.RightMargin := MargemDireita;
    frxReport.PreviewPages.ModifyPage(I, Page);
  end;
end;

function TACBrMDFeDAMDFEFR.GetPreparedReport: TfrxReport;
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

function TACBrMDFeDAMDFEFR.GetPreparedReportEvento: TfrxReport;
begin
  if Trim(FFastFileEvento) = '' then
    Result := nil
  else
  begin
    if PrepareReportEvento then
      Result := frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirDAMDFe(AMDFe: TMDFe);
begin
  if PrepareReport(AMDFe) then
  begin
    if MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirDAMDFePDF(AMDFe: TMDFe);
const
  TITULO_PDF = 'Manifesto de Documento Eletrônico';
var
  I:          Integer;
  OldShowDialog : Boolean;
  NomeArq:string;
begin
  if PrepareReport(AMDFe) then
  begin
    for I := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count - 1 do
    begin
      frxPDFExport.Author     := Sistema;
      frxPDFExport.Creator    := Sistema;
      frxPDFExport.Producer   := Sistema;
      frxPDFExport.Title      := TITULO_PDF;
      frxPDFExport.Subject    := TITULO_PDF;
      frxPDFExport.Keywords   := TITULO_PDF;
      OldShowDialog := frxPDFExport.ShowDialog;
      try
        frxPDFExport.ShowDialog := False;
	      NomeArq := Trim(DAMDFEClassOwner.NomeDocumento);
	      if EstaVazio(NomeArq) then
	        NomeArq := OnlyNumber(TACBrMDFe(ACBrMDFe).Manifestos.Items[i].MDFe.infMDFe.ID) + '-mdfe.pdf';
	      frxPDFExport.FileName := PathWithDelim(DAMDFEClassOwner.PathPDF) + NomeArq;

        if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
          ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

        frxReport.Export(frxPDFExport);
      finally
        frxPDFExport.ShowDialog := OldShowDialog;
        FPArquivoPDF := frxPDFExport.FileName;
      end;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirDAMDFePDF(AStream: TStream;AMDFe: TMDFe);
const
  TITULO_PDF = 'Manifesto de Documento Eletrônico';
var
  I:          Integer;
  OldShowDialog : Boolean;
  NomeArq:string;
begin
  if PrepareReport(AMDFe) then
  begin
    for I := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count - 1 do
    begin
      frxPDFExport.Author     := Sistema;
      frxPDFExport.Creator    := Sistema;
      frxPDFExport.Producer   := Sistema;
      frxPDFExport.Title      := TITULO_PDF;
      frxPDFExport.Subject    := TITULO_PDF;
      frxPDFExport.Keywords   := TITULO_PDF;

      frxPDFExport.Stream        := AStream;
      OldShowDialog := frxPDFExport.ShowDialog;
      try
        frxPDFExport.ShowDialog := False;
        frxReport.Export(frxPDFExport);
      finally
        frxPDFExport.ShowDialog := OldShowDialog;
      end;
    end;
  end
  else
    frxPDFExport.FileName := '';
end;

procedure TACBrMDFeDAMDFEFR.ImprimirEVENTO(AMDFe: TMDFe);
begin
  if PrepareReportEvento then
  begin
    if MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirEVENTOPDF(AMDFe: TMDFe);
const
  TITULO_PDF = 'Manifesto de Documento Eletrônico - Evento';
var
  NomeArq      : String;
  OldShowDialog: Boolean;
begin
  if PrepareReportEvento then
  begin
    frxPDFExport.Author   := Sistema;
    frxPDFExport.Creator  := Sistema;
    frxPDFExport.Producer := Sistema;
    frxPDFExport.Title    := TITULO_PDF;
    frxPDFExport.Subject  := TITULO_PDF;
    frxPDFExport.Keywords := TITULO_PDF;
    OldShowDialog := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := False;
      NomeArq := Trim(DAMDFEClassOwner.NomeDocumento);
      if EstaVazio(NomeArq) then
        NomeArq := OnlyNumber(TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[0].InfEvento.Id) + '-procEventoMDFe.pdf';
      frxPDFExport.FileName := PathWithDelim(DAMDFEClassOwner.PathPDF) + NomeArq;

      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
        ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := OldShowDialog;
      FPArquivoPDF := frxPDFExport.FileName;
    end;
  end;
end;

function TACBrMDFeDAMDFEFR.PrepareReport(AMDFe: TMDFe): Boolean;
var
  i: Integer;
  Stream: TStringStream;
begin
  Result := False;
  SetDataSetsToFrxReport;

  if Trim(FastFile) <> '' then
  begin
    if not (UpperCase(Copy(FastFile, Length(FastFile)-3, 4)) = '.FR3') then
    begin
      Stream := TStringStream.Create(FastFile);
      frxReport.FileName := '';
      frxReport.LoadFromStream(Stream);
      Stream.Free;
    end
    else
    begin
      if FileExists(FastFile) then
        frxReport.LoadFromFile(FastFile)
      else
        raise EACBrMDFeDAMDFEFR.CreateFmt('Caminho do arquivo de impressão do DAMDFe "%s" inválido.', [FastFile]);
    end;
  end
  else
    raise EACBrMDFeDAMDFEFR.Create('Caminho do arquivo de impressão do DAMDFe não assinalado.');

  frxReport.PrintOptions.Copies      := NumCopias;
  frxReport.PrintOptions.ShowDialog  := MostraSetup;
  frxReport.ShowProgress             := MostraStatus;
  frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  if Assigned(AMDFe) then
  begin
    FMDFe := AMDFe;
    CarregaDados;
    SetDataSetsToFrxReport;
    Result := frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrMDFe) then
    begin
      for i := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count - 1 do
      begin
        FMDFe := TACBrMDFe(ACBrMDFe).Manifestos.Items[i].MDFe;
        CarregaDados;
        if (i > 0) then
          Result := frxReport.PrepareReport(False)
        else
          Result := frxReport.PrepareReport;
      end;
    end
    else
      raise EACBrMDFeDAMDFEFR.Create('Propriedade ACBrMDFe não assinalada.');
  end;

  AjustaMargensReports;

end;

function TACBrMDFeDAMDFEFR.PrepareReportEvento: Boolean;
var
  Stream: TStringStream;
begin
  SetDataSetsToFrxReport;

  if Trim(FastFileEvento) <> '' then
  begin
    if not (UpperCase(Copy(FastFileEvento, Length(FastFileEvento)-3, 4)) = '.FR3') then
    begin
      Stream := TStringStream.Create(FastFileEvento);
      frxReport.FileName := '';
      frxReport.LoadFromStream(Stream);
      Stream.Free;
    end
    else
    if FileExists(FastFileEvento) then
      frxReport.LoadFromFile(FastFileEvento)
    else
      raise EACBrMDFeDAMDFEFR.CreateFmt('Caminho do arquivo de impressão do EVENTO "%s" inválido.', [FastFileEvento]);
  end
  else
    raise EACBrMDFeDAMDFEFR.Create('Caminho do arquivo de impressão do EVENTO não assinalado.');

  frxReport.PrintOptions.Copies      := NumCopias;
  frxReport.PrintOptions.ShowDialog  := MostraSetup;
  frxReport.ShowProgress             := MostraStatus;
  frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  // preparar relatorio
  if Assigned(ACBrMDFe) then
  begin
    if assigned(TACBrMDFe(ACBrMDFe).EventoMDFe) then
    begin
      Evento := TACBrMDFe(ACBrMDFe).EventoMDFe;
      CarregaDadosEventos;
    end
    else
      raise EACBrMDFeDAMDFEFR.Create('Evento não foi assinalado.');

    if TACBrMDFe(ACBrMDFe).Manifestos.Count > 0 then
    begin
      MDFe := TACBrMDFe(ACBrMDFe).Manifestos.Items[0].MDFe;
      CarregaDados;
    end;

    Result := frxReport.PrepareReport;
  end
  else
    raise EACBrMDFeDAMDFEFR.Create('Propriedade ACBrMDFe não assinalada.');

  AjustaMargensReports;
end;

procedure TACBrMDFeDAMDFEFR.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(frxIdentificacao);
  frxReport.EnabledDataSets.Add(frxEmitente);
  frxReport.EnabledDataSets.Add(frxMunCarrega);
  frxReport.EnabledDataSets.Add(frxModalRodo);
  frxReport.EnabledDataSets.Add(frxModalAereo);
  frxReport.EnabledDataSets.Add(frxModalAqua);
  frxReport.EnabledDataSets.Add(frxModalFerrov);
  frxReport.EnabledDataSets.Add(frxModalFerrovVagoes);
  frxReport.EnabledDataSets.Add(frxDocumentos);
  frxReport.EnabledDataSets.Add(frxParametros);
  frxReport.EnabledDataSets.Add(frxPercurso);
  frxReport.EnabledDataSets.Add(frxEventos);
  frxReport.EnabledDataSets.Add(frxTermCarrega);
  frxReport.EnabledDataSets.Add(frxTermDescarrega);
  frxReport.EnabledDataSets.Add(frxEmbarcaComboio);
  frxReport.EnabledDataSets.Add(frxInfUnidCargaVazia);
  frxReport.EnabledDataSets.Add(frxUnidTranspVazia);
  frxReport.EnabledDataSets.Add(frxSeg);
  frxReport.EnabledDataSets.Add(frxContratantes);
  frxReport.EnabledDataSets.Add(frxPeri);
end;

procedure TACBrMDFeDAMDFEFR.LimpaDados;
begin
  cdsIdentificacao.EmptyDataSet;
  cdsEmitente.EmptyDataSet;
  cdsModalAereo.EmptyDataSet;
  cdsModalAqua.EmptyDataSet;
  cdsDocumentos.EmptyDataSet;
  cdsMunCarrega.EmptyDataSet;
  cdsModalRodo.EmptyDataSet;
  cdsParametros.EmptyDataSet;
  cdsPercurso.EmptyDataSet;
  cdsModalFerrov.EmptyDataSet;
  cdsModalFerrovVagoes.EmptyDataSet;
  cdsTermCarrega.EmptyDataSet;
  cdsTermDescarrega.EmptyDataSet;
  cdsEmbarcaComboio.EmptyDataSet;
  cdsInfUnidCargaVazia.EmptyDataSet;
  cdsUnidTranspVazia.EmptyDataSet;
  cdsSeg.EmptyDataSet;
  cdsContratantes.EmptyDataSet;
  cdsPeri.EmptyDataSet;
end;

procedure TACBrMDFeDAMDFEFR.CarregaContratantes;
var i:integer;
begin
  with cdsContratantes, FMDFe.rodo.infANTT do
  begin
    Append;
    for I := 0 to infContratante.Count - 1 do
    begin
      if Length(infContratante[i].CNPJCPF)=11 then
        FieldByName('CNPJCPF').AsString         := FormatarCPF(infContratante[i].CNPJCPF)
      else
        FieldByName('CNPJCPF').AsString         := FormatarCNPJ(infContratante[i].CNPJCPF);
    end;
    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaPeri;
var i,j,k : integer;
begin
  with cdsPeri, FMDFe.infDoc do
  begin
    with FMDFe.infDoc do
    begin
      for i := 0 to infMunDescarga.Count - 1 do
      begin
        with infMunDescarga.Items[i] do
        begin
          for j := 0 to infCTe.Count - 1 do
          begin
            for k := 0 to infCTe[j].peri.Count - 1 do
            begin
              Append;
              FieldByName('chave').AsString  := infCTe[j].chCTe;
              FieldByName('nONU').AsString  := infCTe[j].peri[k].nONU;
              FieldByName('xNomeAE').AsString  := infCTe[j].peri[k].xNomeAE;
              FieldByName('xClaRisco').AsString  := infCTe[j].peri[k].xClaRisco;
              FieldByName('grEmb').AsString  := infCTe[j].peri[k].grEmb;
              FieldByName('qTotProd').AsString  := infCTe[j].peri[k].qTotProd;
              FieldByName('qVolTipo').AsString  := infCTe[j].peri[k].qVolTipo;
              Post;
            end;
          end;
          for j := 0 to infNFe.Count - 1 do
          begin
            for k := 0 to infNFe[j].peri.Count - 1 do
            begin
              Append;
              FieldByName('chave').AsString  := infNFe[j].chNFe;
              FieldByName('nONU').AsString  := infNFe[j].peri[k].nONU;
              FieldByName('xNomeAE').AsString  := infNFe[j].peri[k].xNomeAE;
              FieldByName('xClaRisco').AsString  := infNFe[j].peri[k].xClaRisco;
              FieldByName('grEmb').AsString  := infNFe[j].peri[k].grEmb;
              FieldByName('qTotProd').AsString  := infNFe[j].peri[k].qTotProd;
              FieldByName('qVolTipo').AsString  := infNFe[j].peri[k].qVolTipo;
              Post;
            end;
          end;
        end;
      end;
    end;

  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaDados;
begin
  LimpaDados;
  CarregaParametros;
  CarregaIdentificacao;
  CarregaMunCarrega;
  CarregaPercurso;
  CarregaEmitente;
  CarregaDocumentos;
  CarregaModal;
  CarregaTermCarreg;
  CarregaTermDescarreg;
  CarregaEmbarcacaoComboio;
  CarregaInfUnidCargaVazia;
  CarregaInfUnidTranspVazia;

  if (FMDFe.infMDFe.versao = 3) then
  begin
    CarregaSeguro;
    CarregaContratantes;
    CarregaPeri;
  end;

  frxReport.Variables['MUNDESCARGA'] := FExibirMunicipioDescarregamento;
end;

procedure TACBrMDFeDAMDFEFR.CarregaParametros;
var
  LogoStream: TStringStream;
begin
  cdsParametros.Append;

  // Carregamento da imagem
  if NaoEstaVazio(FDAMDFEClassOwner.Logo) then
  begin
    cdsParametros.FieldByName('Imagem').AsString := DAMDFEClassOwner.Logo;

    if FileExists(FDAMDFEClassOwner.Logo) then
      TBlobField(cdsParametros.FieldByName('LogoCarregado')).LoadFromFile(DAMDFEClassOwner.Logo)
    else
    begin
      LogoStream := TStringStream.Create(FDAMDFEClassOwner.Logo);
      try
        LogoStream.Position := 0;
        TBlobField(cdsParametros.FieldByName('LogoCarregado')).LoadFromStream(LogoStream);
      finally
        LogoStream.Free;
      end;
    end;
  end;

  cdsParametros.FieldByName('Versao').AsString  := FloatToString(FMDFe.infMDFe.Versao,'.','#0.00');
  cdsParametros.FieldByName('Sistema').AsString := Ifthen(DAMDFEClassOwner.Sistema <> '',DAMDFEClassOwner.Sistema,'Projeto ACBr - http://acbr.sf.net');
  cdsParametros.FieldByName('Usuario').AsString := Ifthen(DAMDFEClassOwner.Usuario <> '', DAMDFEClassOwner.Usuario,'');
  cdsParametros.Post;

end;

procedure TACBrMDFeDAMDFEFR.CarregaIdentificacao;
var
  vTemp:TStringList;
  wObs:String;
  Campos:TSplitResult;
  IndexCampo:Integer;
  TmpStr:String;
  BufferObs:String;
  I:integer;
begin
  with cdsIdentificacao do
  begin
    Append;

    with FMDFe.infMDFe do
    begin
      FieldByName('Id').AsString    := OnlyNumber(Id);
      FieldByName('Chave').AsString := FormatarChaveAcesso(Id);
    end;

    with FMDFe.Ide do
    begin
      FieldByName('tpAmb').AsInteger  := StrToIntDef(TpAmbToStr(tpAmb), 0);
      FieldByName('tpEmit').AsInteger := StrToIntDef(TpEmitenteToStr(tpEmit), 0);
      FieldByName('Modelo').AsString  := modelo;
      FieldByName('serie').AsString   := Poem_Zeros(serie, 3);
      FieldByName('nMDF').AsString    := FormatFloat('000,000,000', nMDF);
      FieldByName('modal').AsInteger  := StrToIntDef(ModalToStr(modal), 0);
      FieldByName('dhEmi').AsDateTime := dhEmi;
      FieldByName('tpEmis').AsInteger := StrToIntDef(TpEmisToStr(tpEmis), 0);
      FieldByName('UFIni').AsString   := UFIni;
      FieldByName('UFFim').AsString   := UFFim;
      if (tpEmis = teNormal) or (not EstaVazio(FDAMDFEClassOwner.Protocolo)) or (not EstaVazio(FMDFe.procMDFe.nProt))
      then
      begin
        if not EstaVazio(FDAMDFEClassOwner.Protocolo) then
          FieldByName('Protocolo').AsString := FDAMDFEClassOwner.Protocolo
        else if not EstaVazio(FMDFe.procMDFe.nProt) then
          FieldByName('Protocolo').AsString := FMDFe.procMDFe.nProt + '   ' +
            IfThen(FMDFe.procMDFe.dhRecbto <> 0, DateTimeToStr(FMDFe.procMDFe.dhRecbto), '')
        else
          FieldByName('Protocolo').AsString := 'MDFe sem Autorização de Uso da SEFAZ';
      end
      else
        FieldByName('Protocolo').AsString := 'Impressão em contingência. Obrigatória a autorização em 168 horas' +
          ' após esta impressão (' + FormatDateTimeBr(dhEmi) + ')';

    end;
    with FMDFe.tot do
    begin
      FieldByName('qCTe').AsInteger    := qCTe;
      FieldByName('qCT').AsInteger     := qCT;
      FieldByName('qNFe').AsInteger    := qNFe;
      FieldByName('qNF').AsInteger     := qNF;
      FieldByName('qMDFe').AsInteger   := qMDFe;

      if cUnid = uTON then
        FieldByName('qDescPeso').AsString:= 'PESO TOTAL (Ton)'
      else
        FieldByName('qDescPeso').AsString := 'PESO TOTAL (Kg)';

      if cUnid = uTon then
        FieldByName('qCarga').AsCurrency := qCarga
      else
        FieldByName('qCarga').AsCurrency := qCarga;
	    FieldByName('vCarga').AsCurrency := vCarga;
    end;

    FieldByName('URL').AsString := TACBrMDFe(ACBrMDFe).GetURLConsulta(FMDFe.Ide.cUF, FMDFe.Ide.tpAmb, FMDFe.infMDFe.versao);

    // Incluido por Paulo Hostert em 18/11/2014.
    wObs := FMDFe.infAdic.infCpl;
    vTemp := TStringList.Create;
    try
      if Trim(wObs) <> '' then
      begin
        Campos := Split(';', wObs);
        for IndexCampo := 0 to Length(Campos) - 1 do
          vTemp.Add(Trim(Campos[IndexCampo]));

        TmpStr := vTemp.Text;
        BufferObs := TmpStr;
      end
      else
        BufferObs := '';
    finally
      vTemp.Free;
    end;
    FieldByName('OBS').AsString := BufferObs;
    FieldByName('dhIniViagem').AsDateTime := FMDFe.Ide.dhIniViagem;

    for I := 0 to FMDFe.lacres.Count - 1 do
    begin
      if FieldByName('Lacres').AsString<>'' then
        FieldByName('Lacres').AsString:=FieldByName('Lacres').AsString+'; ';
      FieldByName('Lacres').AsString:=FieldByName('Lacres').AsString+FMDFe.lacres[i].nLacre;
    end;


    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaMunCarrega;
var i:integer;
begin
  with cdsMunCarrega do
  begin
    for i := 0 to FMDFe.Ide.infMunCarrega.Count - 1 do
    begin
      Append;
      FieldByName('xMunCarrega').AsString:=FMDFe.Ide.infMunCarrega[i].xMunCarrega;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaPercurso;
var i:integer;
begin
  with cdsPercurso do
  begin
    for i := 0 to FMDFe.Ide.infPercurso.Count - 1 do
    begin
      Append;
      FieldByName('UFPer').AsString:=FMDFe.Ide.infPercurso[i].UFPer;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaEmitente;
begin
  with cdsEmitente do
  begin
    Append;
    with FMDFe.emit do
    begin

	  if Length(CNPJCPF)=11 then
        FieldByName('CNPJ').AsString := FormatarCPF(CNPJCPF)
      else
        FieldByName('CNPJ').AsString := FormatarCNPJ(CNPJCPF);

      FieldByName('IE').AsString    := IE;
      FieldByName('XNome').AsString := xNome;
      FieldByName('XFant').AsString := XFant;
      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('Fone').AsString    := FormatarFone(Fone);
        FieldByName('email').AsString   := email;
        FieldByName('site').AsString    := FDAMDFEClassOwner.Site;
      end;
    end;
    Post;

  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaDocumentos;
var
  i, j, x, y: integer;
begin
  with CDSDocumentos do
  begin
    with FMDFe.infDoc do
    begin
      for i := 0 to infMunDescarga.Count - 1 do
      begin
        with infMunDescarga.Items[i] do
        begin

          for j := 0 to infCTe.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'CTe';
            FieldByName('cMunDescarga').AsInteger := cMunDescarga;
            FieldByName('xMunDescarga').AsString  := xMunDescarga;
            FieldByName('Chave').AsString := FormatarChaveAcesso(infCTe.Items[j].chCTe);
            Post;
            with infCTe[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infCT.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'CT';
            FieldByName('cMunDescarga').AsInteger := cMunDescarga;
            FieldByName('xMunDescarga').AsString  := xMunDescarga;
            FieldByName('Chave').AsString := FormatarCNPJouCPF(FMDFe.emit.CNPJCPF) + '        ' +
              IntToStr(infCT.Items[j].serie) + '-' + infCT.Items[j].nCT;
            Post;
            with infCT[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infNFe.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'NFe';
            FieldByName('cMunDescarga').AsInteger := cMunDescarga;
            FieldByName('xMunDescarga').AsString  := xMunDescarga;
            FieldByName('Chave').AsString := FormatarChaveAcesso(infNFe.Items[j].chNFe);
            Post;
            with infNFe[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infNF.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'NF';
            FieldByName('cMunDescarga').AsInteger := cMunDescarga;
            FieldByName('xMunDescarga').AsString  := xMunDescarga;
            FieldByName('Chave').AsString := FormatarCNPJouCPF(FMDFe.emit.CNPJCPF) + '        ' +
              IntToStr(infNF.Items[j].serie) + '-' + IntToStr(infNF.Items[j].nNF);
            Post;
            with infNF[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infMDFeTransp.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'MDF-e';
            FieldByName('cMunDescarga').AsInteger := cMunDescarga;
            FieldByName('xMunDescarga').AsString  := xMunDescarga;
            FieldByName('Chave').AsString := FormatarChaveAcesso(infMDFeTransp.Items[j].chMDFe);
            Post;
            with infMDFeTransp[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModal;
begin
  with FMDFe do
  begin
    case Ide.modal of
      moRodoviario:
        CarregaModalRodoviario;
      moAereo:
        CarregaModalAereo;
      moAquaviario:
        CarregaModalAquaviario;
      moFerroviario:
        CarregaModalFerroviario;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaTermCarreg;
var
  i: integer;
begin
  with cdsTermCarrega, FMDFe.aquav.infTermCarreg do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cTermCarreg').AsString := Items[i].cTermCarreg;
      FieldByName('xTermCarreg').AsString := Items[i].xTermCarreg;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaTermDescarreg;
var
  i: integer;
begin
  with cdsTermDescarrega, FMDFe.aquav.infTermDescarreg do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cTermDescarreg').AsString := Items[i].cTermDescarreg;
      FieldByName('xTermDescarreg').AsString := Items[i].xTermDescarreg;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaEmbarcacaoComboio;
var
  i: integer;
begin
  with cdsEmbarcaComboio, FMDFe.aquav.infEmbComb do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cEmbComb').AsString := Items[i].cEmbComb;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaInfUnidCargaVazia;
var
  i: integer;
begin
  with cdsInfUnidCargaVazia, FMDFe.aquav.infUnidCargaVazia do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('idUnidCargaVazia').AsString := Items[i].idUnidCargaVazia;
      case Items[i].tpUnidCargaVazia of
        ucContainer : FieldByName('tpUnidCargaVazia').AsString := '1 - Container';
        ucULD : FieldByName('tpUnidCargaVazia').AsString := '2 - Carreta';
        ucPallet: FieldByName('tpUnidCargaVazia').AsString := '3 - Pallet';
        ucOutros: FieldByName('tpUnidCargaVazia').AsString := '4 - Outros';
      end;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaInfUnidTranspVazia;
var
  i: integer;
begin
  with cdsUnidTranspVazia, FMDFe.aquav.infUnidTranspVazia do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('idUnidTranspVazia').AsString := Items[i].idUnidTranspVazia;
      case Items[i].tpUnidTranspVazia of
        utRodoTracao: FieldByName('tpUnidTranspVazia').AsString := '1 - Caminhão';
        utRodoReboque: FieldByName('tpUnidTranspVazia').AsString := '2 - Carreta';
      end;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalAereo;
begin
  with cdsModalAereo, FMDFe.aereo do
  begin
    Append;
    FieldByName('nac').AsString     := nac;
    FieldByName('matr').AsString    := matr;
    FieldByName('nVoo').AsString    := nVoo;
    FieldByName('cAerEmb').AsString := cAerEmb;
    FieldByName('cAerDes').AsString := cAerDes;
    FieldByName('dVoo').AsDateTime  := dVoo;
    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalAquaviario;
begin
  with CDSModalAqua, FMDFe.aquav do
  begin
    Append;
    FieldByName('CNPJAgeNav').AsString := FormatarCNPJ(CNPJAgeNav);
    FieldByName('tpEmb').AsString      := tpEmb;
    FieldByName('cEmbar').AsString     := cEmbar;
    FieldByName('xEmbar').AsString     := xEmbar;
    FieldByName('nViag').AsString      := nViagem;
    FieldByName('cPrtEmb').AsString    := cPrtEmb;
    FieldByName('cPrtDest').AsString   := cPrtDest;
    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalFerroviario;
var
  i: integer;
begin
  with CDSModalFerrov, FMDFe.ferrov do
  begin
    Append;
    FieldByName('xPref').AsString    := xPref;
    FieldByName('dhTrem').AsDateTime := dhTrem;
    FieldByName('xOri').AsString     := xOri;
    FieldByName('xDest').AsString    := xDest;
    FieldByName('qVag').AsInteger    := qVag;
    Post;
  end;

  with CDSModalFerrovVagoes, FMDFe.ferrov.vag do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('serie').AsString := Items[i].serie;
      FieldByName('nVag').AsInteger := Items[i].nVag;
      FieldByName('nSeq').AsInteger := Items[i].nSeq;
      FieldByName('TU').AsCurrency  := Items[i].TU;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalRodoviario;
var
  i: integer;
begin
  with cdsModalRodo, FMDFe.rodo do
  begin
    Append;

    if (FMDFe.infMDFe.versao >= 3) then
      FieldByName('RNTRC').AsString := infANTT.RNTRC
    else
      FieldByName('RNTRC').AsString := RNTRC;

    FieldByName('CIOT').AsString  := CIOT;

    if veicTracao.placa <> '' then
    begin
      FieldByName('placa').AsString     := FormatarPlaca(veicTracao.placa) + ' / ' + veicTracao.UF;
      FieldByName('RENAVAM').AsString   := veicTracao.RENAVAM;
      FieldByName('RNTRCProp').AsString := IfEmptyThen(veicTracao.prop.RNTRC, FieldByName('RNTRC').AsString);
      FieldByName('CNPJCPFProp').AsString := veicTracao.prop.CNPJCPF;

      for i := 0 to veicTracao.condutor.Count - 1 do
      begin
        // Alteração proposta por Maciel Goettms (27/02/2014) Concatenação dos condutores já adicionados.
        FieldByName('CPF').AsString   := FieldByName('CPF').AsString + FormatarCPF(veicTracao.condutor.Items[i].CPF) + #13#10;
        FieldByName('xNome').AsString := FieldByName('xNome').AsString + veicTracao.condutor.Items[i].xNome + #13#10;
      end;
    end;

    for i := 0 to veicReboque.Count - 1 do
    begin
      FieldByName('placa').AsString     := FieldByName('placa').AsString + #13#10 + FormatarPlaca(veicReboque.Items[i].placa) + ' / ' + veicReboque.Items[i].UF;
      FieldByName('RENAVAM').AsString   := FieldByName('RENAVAM').AsString + #13#10 + veicReboque.Items[i].RENAVAM;
      FieldByName('RNTRCProp').AsString := FieldByName('RNTRCProp').AsString + #13#10 + IfThen(veicReboque.Items[i].prop.RNTRC <> '', veicReboque.Items[i].prop.RNTRC, FMDFe.rodo.RNTRC);
      FieldByName('CNPJCPFProp').AsString := FieldByName('CNPJCPFProp').AsString + #13#10 + veicReboque.Items[i].prop.CNPJCPF;
    end;

    if FMDFe.rodo.valePed.disp.Count > 0 then
    begin
      for i := 0 to valePed.disp.Count - 1 do
      begin
        FieldByName('CNPJForn').AsString := FieldByName('CNPJForn').AsString + FormatarCNPJ(valePed.disp.Items[i].CNPJForn) + #13#10;
        FieldByName('CNPJPg').AsString   := FieldByName('CNPJPg').AsString + FormatarCNPJ(valePed.disp.Items[i].CNPJPg) + #13#10;
        FieldByName('nCompra').AsString  := FieldByName('nCompra').AsString + valePed.disp.Items[i].nCompra + #13#10;
      end;
    end
    else
    if FMDFe.rodo.infANTT.valePed.disp.Count > 0 then
    begin
      for i := 0 to infANTT.valePed.disp.Count - 1 do
      begin
        FieldByName('CNPJForn').AsString := FieldByName('CNPJForn').AsString + FormatarCNPJ(infANTT.valePed.disp.Items[i].CNPJForn) + #13#10;
        FieldByName('CNPJPg').AsString   := FieldByName('CNPJPg').AsString + FormatarCNPJ(infANTT.valePed.disp.Items[i].CNPJPg) + #13#10;
        FieldByName('nCompra').AsString  := FieldByName('nCompra').AsString + infANTT.valePed.disp.Items[i].nCompra + #13#10;
        FieldByName('vValePed').AsString  := FieldByName('vValePed').AsString + formatFloat('#,#0.00',infANTT.valePed.disp.Items[i].vValePed) + #13#10;
      end;
    end;

    if (FMDFe.rodo.infANTT.infCIOT.Count > 0) then
    begin
      for i := 0 to FMDFe.rodo.infANTT.infCIOT.Count - 1 do
      begin
        FieldByName('infCIOT_CIOT').AsString    := FieldByName('infCIOT_CIOT').AsString + infANTT.infCIOT.Items[i].CIOT + #13#10;
        FieldByName('infCIOT_CNPJCPF').AsString := FieldByName('infCIOT_CNPJCPF').AsString + FormatarCNPJouCPF(infANTT.infCIOT.Items[i].CNPJCPF) + #13#10;
      end;
    end;

    FieldByName('codAgPorto').AsString := FMDFe.rodo.codAgPorto;

    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaDadosEventos;
var
  i: Integer;
begin
  with cdsEventos do
  begin
    EmptyDataSet;

    for i := 0 to FEvento.Evento.Count - 1 do
    begin
      Append;

      with Evento.Evento[i] do
      begin
        FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);

        // nota fiscal eletronica
        FieldByName('Modelo').AsString      := Copy(InfEvento.chMDFe, 21, 2);
        FieldByName('Serie').AsString       := Copy(InfEvento.chMDFe, 23, 3);
        FieldByName('Numero').AsString      := Copy(InfEvento.chMDFe, 26, 9);
        FieldByName('MesAno').AsString      := Copy(InfEvento.chMDFe, 05, 2) + '/' + Copy(InfEvento.chMDFe, 03, 2);
        FieldByName('Barras').AsString      := InfEvento.chMDFe;
        FieldByName('ChaveAcesso').AsString := FormatarChaveAcesso(InfEvento.chMDFe);

        // Carta de correção eletrônica
        FieldByName('cOrgao').AsInteger := InfEvento.cOrgao;

        case InfEvento.tpAmb of
          taProducao:
            FieldByName('tpAmb').AsString := 'PRODUÇÃO';
          taHomologacao:
            begin
              FieldByName('tpAmb').AsString      := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';
              frxReport.Variables['HOMOLOGACAO'] := True;
            end;
        end;

        FieldByName('dhEvento').AsDateTime    := InfEvento.dhEvento;
        FieldByName('TipoEvento').AsString    := InfEvento.TipoEvento;
        FieldByName('DescEvento').AsString    := InfEvento.DescEvento;
        FieldByName('nSeqEvento').AsInteger   := InfEvento.nSeqEvento;
        FieldByName('versaoEvento').AsString  := InfEvento.versaoEvento;
        FieldByName('cStat').AsInteger        := RetInfEvento.cStat;
        FieldByName('xMotivo').AsString       := RetInfEvento.xMotivo;
        FieldByName('nProt').AsString         := RetInfEvento.nProt;
        FieldByName('dhRegEvento').AsDateTime := RetInfEvento.dhRegEvento;
        FieldByName('xJust').AsString         := InfEvento.detEvento.xJust;
        FieldByName('xNome').AsString         := InfEvento.detEvento.xNome;

        if (InfEvento.detEvento.CPF <> '') then
          FieldByName('CPF').AsString := FormatarCPF(InfEvento.detEvento.CPF);

        FieldByName('nProtEvento').AsString   := InfEvento.detEvento.nProt;
        FieldByName('dtEnc').AsDateTime       := InfEvento.detEvento.dtEnc;
        FieldByName('cUf').AsInteger          := InfEvento.detEvento.cUF;
        FieldByName('cMun').AsInteger         := InfEvento.detEvento.cMun;
      end;

      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaSeguro;
var
  i, x: Integer;
begin
  with cdsSeg, FMDFe.seg do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('infResp_respSeg').AsInteger    := StrToInt(RspSeguroMDFeToStr(Items[i].respSeg));
      FieldByName('infResp_respSegStr').AsString  := RspSeguroMDFeToStrText(Items[i].respSeg);
      FieldByName('infResp_CNPJCPF').AsString     := FormatarCNPJouCPF(Items[i].CNPJCPF);
      FieldByName('infSeg_xSeg').AsString         := Items[i].xSeg;
      FieldByName('infSeg_CNPJ').AsString         := FormatarCNPJ(Items[i].CNPJ);
      FieldByName('nApol').AsString               := Items[i].nApol;

      with Items[i].aver do
      begin
        for x := 0 to Count - 1 do
        begin
          if not(FieldByName('nAver').AsString='') then
            FieldByName('nAver').AsString:=FieldByName('nAver').AsString + #13#10; // fiz esta verificação para não jogar quebra de linha desnecessariamente na impressão.
          FieldByName('nAver').AsString := FieldByName('nAver').AsString + Items[x].nAver;
        end;
      end;

      Post;
    end;
  end;
end;

end.
