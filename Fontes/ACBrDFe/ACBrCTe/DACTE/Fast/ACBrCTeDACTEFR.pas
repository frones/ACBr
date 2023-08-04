{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Wemerson Souto                                  }
{                              André Ferreira de Moraes                        }
{                              Jeickson Gobeti                                 }
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

unit ACBrCTeDACTEFR;

interface

uses
  SysUtils, Classes, ACBrCTeDACTEClass, ACBrBase,
  pcteCTe, pcnConversao, frxClass, DBClient, frxDBSet, frxBarcode, frxExportPDF,
  pcteEnvEventoCTe, pcteInutCTe, pcteRetInutCTe, ACBrCTe,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  StrUtils,
  DB, MaskUtils;

type
  EACBrCTeDACTEFR = class(Exception);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCTeDACTEFR = class(TACBrCTeDACTEClass)
  private
    FDACTEClassOwner: TACBrCTeDACTEClass;
    FCTe            : TCTe;
    FEvento         : TEventoCTe;
    FInutilizacao   : TRetInutCTe;
    FFastFile       : String;
    FEspessuraBorda : Integer;
    FFastFileEvento : string;
    FFastFileInutilizacao: String;

    procedure CriarDataSetsFrx;
    function GetPreparedReport: TfrxReport;
    function GetPreparedReportEvento: TfrxReport;
    function GetPreparedReportInutilizacao: TfrxReport;

    procedure SetDataSetsToFrxReport;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);

    procedure CarregaIdentificacao;
    procedure CarregaTomador;
    procedure CarregaEmitente;
    procedure CarregaRemetente;
    procedure CarregaDestinatario;
    procedure CarregaExpedidor;
    procedure CarregaRecebedor;
    procedure CarregaDadosNotasFiscais;
    procedure CarregaCalculoImposto;
    procedure CarregaParametros;
    procedure CarregaVolumes;
    procedure CarregaComponentesPrestacao;
    procedure CarregaSeguro;
    procedure CarregaModalRodoviario;
    procedure CarregaModalAquaviario;
    procedure CarregaModalAereo;
    procedure CarregaMultiModal;
    procedure CarregaInformacoesAdicionais;
    procedure CarregaDocumentoAnterior;
    procedure CarregaCTeAnuladoComplementado;
    procedure CarregaProdutosPerigosos;
    procedure CarregaVeiculosNovos;
    procedure CarregaInfServico;
    procedure CarregaInfTribFed;
    procedure CarregaPercurso;
    procedure LimpaDados;
    function ManterCep(iCep: Integer): String;
    procedure AjustaMargensReports;
  protected
    procedure CarregaDados;
    procedure CarregaDadosEventos;
    procedure CarregaDadosInutilizacao;
    function PrepareReport(ACTE: TCTe = nil): Boolean; virtual;
    function PrepareReportEvento: Boolean; virtual;
    function PrepareReportInutilizacao: Boolean; virtual;
  public
    frxReport   : TfrxReport;
    frxPDFExport: TfrxPDFExport;
    // CDS
    cdsIdentificacao        : TClientDataSet;
    cdsEmitente             : TClientDataSet;
    cdsDestinatario         : TClientDataSet;
    cdsDadosNotasFiscais    : TClientDataSet;
    cdsParametros           : TClientDataSet;
    cdsInformacoesAdicionais: TClientDataSet;
    cdsVolumes              : TClientDataSet;
    cdsTomador              : TClientDataSet;
    cdsExpedidor            : TClientDataSet;
    cdsRecebedor            : TClientDataSet;
    cdsRemetente            : TClientDataSet;
    cdsCalculoImposto       : TClientDataSet;
    cdsComponentesPrestacao : TClientDataSet;
    cdsSeguro               : TClientDataSet;
    cdsModalRodoviario      : TClientDataSet;
    cdsModalAereo           : TClientDataSet;
    cdsMultiModal           : TClientDataSet;
    cdsModalAquaviario      : TClientDataSet;
    cdsRodoVeiculos         : TClientDataSet;
    cdsRodoValePedagio      : TClientDataSet;
    cdsRodoMotorista        : TClientDataSet;
    cdsDocAnterior          : TClientDataSet;
    cdsAnuladoComple        : TClientDataSet;
    cdsEventos              : TClientDataSet;
    cdsProdutosPerigosos    : TClientDataSet;
    cdsVeiculosNovos        : TClientDataSet;
    cdsInutilizacao         : TClientDataSet;
    cdsInfServico           : TClientDataSet;
    cdsInfTribFed           : TClientDataSet;
    cdsPercurso             : TClientDataSet;

    // frxDB
    frxIdentificacao        : TfrxDBDataset;
    frxEmitente             : TfrxDBDataset;
    frxDestinatario         : TfrxDBDataset;
    frxDadosNotasFiscais    : TfrxDBDataset;
    frxParametros           : TfrxDBDataset;
    frxVolumes              : TfrxDBDataset;
    frxInformacoesAdicionais: TfrxDBDataset;
    frxTomador              : TfrxDBDataset;
    frxExpedidor            : TfrxDBDataset;
    frxRecebedor            : TfrxDBDataset;
    frxRemetente            : TfrxDBDataset;
    frxCalculoImposto       : TfrxDBDataset;
    frxComponentesPrestacao : TfrxDBDataset;
    frxSeguro               : TfrxDBDataset;
    frxModalRodoviario      : TfrxDBDataset;
    frxModalAereo           : TfrxDBDataset;
    frxMultiModal           : TfrxDBDataset;
    frxModalAquaviario      : TfrxDBDataset;
    frxRodoVeiculos         : TfrxDBDataset;
    frxRodoValePedagio      : TfrxDBDataset;
    frxRodoMotorista        : TfrxDBDataset;
    frxDocAnterior          : TfrxDBDataset;
    frxAnuladoComple        : TfrxDBDataset;
    frxEventos              : TfrxDBDataset;
    frxProdutosPerigosos    : TfrxDBDataset;
    frxVeiculosNovos        : TfrxDBDataset;
    frxInutilizacao         : TfrxDBDataset;
    frxInfServico           : TfrxDBDataset;
    frxInfTribFed           : TfrxDBDataset;
    frxPercurso             : TfrxDBDataset;

    frxBarCodeObject: TfrxBarCodeObject;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDACTe(ACTE: TCTe = nil); override;
    procedure ImprimirDACTePDF(ACTE: TCTe = nil); override;
    procedure ImprimirEVENTO(ACTE: TCTe = nil); override;
    procedure ImprimirEVENTOPDF(ACTE: TCTe = nil); override;
    procedure ImprimirINUTILIZACAO(ACTE: TCTe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(ACTE: TCTe = nil); override;
    property CTE: TCTe read FCTe write FCTe;
    property Evento: TEventoCTe read FEvento write FEvento;
    property Inutilizacao: TRetInutCTe read FInutilizacao write FInutilizacao;
    property DACTEClassOwner: TACBrCTeDACTEClass read FDACTEClassOwner;

  published
    property FastFile            : String read FFastFile write FFastFile;
    property FastFileEvento      : string read FFastFileEvento write FFastFileEvento;
    property FastFileInutilizacao: String read FFastFileInutilizacao write FFastFileInutilizacao;
    property EspessuraBorda      : Integer read FEspessuraBorda write FEspessuraBorda;
    property PreparedReport      : TfrxReport read GetPreparedReport;
    property PreparedReportEvento: TfrxReport read GetPreparedReportEvento;
    property PreparedReportInutilizacao: TfrxReport read GetPreparedReportInutilizacao;
  end;

var
  TipoEvento: TpcnTpEvento;

implementation

uses
  pcteConversaoCTe,
  ACBrDFeUtil, ACBrImage, ACBrDelphiZXingQRCode, ACBrValidador;

function CollateBr(Str: string): string;
var
  Resultado, Temp: string;
  vChar          : Char;
  Tamanho, i     : Integer;
begin
  Result  := '';
  Tamanho := Length(Str);
  i       := 1;
  while i <= Tamanho do
  begin
    Temp  := Copy(Str, i, 1);
    vChar := Temp[1];
    case vChar of
      'á', 'â', 'ã', 'à', 'ä', 'å', 'Á', 'Â', 'Ã', 'À', 'Ä', 'Å': Resultado := 'A';
      'é', 'ê', 'è', 'ë', 'É', 'Ê', 'È', 'Ë': Resultado                     := 'E';
      'í', 'î', 'ì', 'ï', 'Í', 'Î', 'Ì', 'Ï': Resultado                     := 'I';
      'ó', 'ô', 'õ', 'ò', 'ö', 'Ó', 'Ô', 'Õ', 'Ò', 'Ö': Resultado           := 'O';
      'ú', 'û', 'ù', 'ü', 'Ú', 'Û', 'Ù', 'Ü': Resultado                     := 'U';
      'ç', 'Ç': Resultado                                                   := 'C';
      'ñ', 'Ñ': Resultado                                                   := 'N';
      'ý', 'ÿ', 'Ý', 'Y': Resultado                                         := 'Y';
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

constructor TACBrCTeDACTEFR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDACTEClassOwner := TACBrCTeDACTEClass(Self);
  FFastFile        := '';
  FEspessuraBorda  := 1;
  CriarDataSetsFrx;
  SetDataSetsToFrxReport;
end;

procedure TACBrCTeDACTEFR.CriarDataSetsFrx;
begin
  frxReport := TfrxReport.Create(nil);
  frxReport.PreviewOptions.Buttons := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind,
    pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick];
  frxReport.EngineOptions.UseGlobalDataSetList := False;
  with frxReport do
  begin
    ScriptLanguage := 'PascalScript';
    StoreInDFM     := False;
    OnBeforePrint  := frxReportBeforePrint;
    OnReportPrint  := 'frxReportOnReportPrint';
    PreviewOptions.Buttons :=[pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick];
  end;

  frxPDFExport := TfrxPDFExport.Create(Self);
  with frxPDFExport do
  begin
    Background     := True;
    PrintOptimized := True;
    Subject        := 'Exportando DACTe para PDF';
    ShowProgress   := False;
  end;

  RttiSetProp(frxPDFExport, 'Transparency', 'False');

  // CDS
  cdsIdentificacao := TClientDataSet.Create(Self);
  with cdsIdentificacao, FieldDefs do
  begin
    Close;
    Clear;
    Add('Versao', ftString, 4);
    Add('Id', ftString, 44);
    Add('Chave', ftString, 60);
    Add('CUF', ftString, 2);
    Add('CCT', ftString, 9);
    Add('CFOP', ftString, 4);
    Add('NatOp', ftString, 60);
    Add('forPag', ftString, 50);
    Add('Mod_', ftString, 2);
    Add('Serie', ftString, 3);
    Add('NCT', ftString, 11);
    Add('dhEmi', ftDateTime);
    Add('TpImp', ftString, 1);
    Add('TpEmis', ftString, 50);
    Add('CDV', ftString, 1);
    Add('TpAmb', ftString, 1);
    Add('TpCT', ftString, 50);
    Add('ProcEmi', ftString, 1);
    Add('VerProc', ftString, 20);
    Add('cMunEmi', ftString, 7);
    Add('xMunEmi', ftString, 60);
    Add('UFEmi', ftString, 2);
    Add('modal', ftString, 2);
    Add('tpServ', ftString, 50);
    Add('indGlobalizado', ftString, 3);
    Add('ObsGlobalizado', ftString, 256);
    Add('cMunIni', ftString, 7);
    Add('xMunIni', ftString, 60);
    Add('UFIni', ftString, 2);
    Add('cMunFim', ftString, 7);
    Add('xMunFim', ftString, 60);
    Add('UFFim', ftString, 2);
    Add('retira', ftString, 1);
    Add('xDetRetira', ftString, 160);
    Add('toma', ftString, 50);
    Add('refCTE', ftString, 44);
    Add('URL', ftString, 1000);
    CreateDataSet;
  end;

  cdsEmitente := TClientDataSet.Create(Self);
  with cdsEmitente, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('XNome', ftString, 60);
    Add('XFant', ftString, 60);
    Add('XLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('XCpl', ftString, 60);
    Add('XBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('XMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('CPais', ftString, 4);
    Add('XPais', ftString, 60);
    Add('Fone', ftString, 15);
    Add('IE', ftString, 20);
    Add('IM', ftString, 15);
    Add('IEST', ftString, 20);
    Add('CRT', ftString, 1);

    CreateDataSet;
  end;

  cdsDestinatario := TClientDataSet.Create(Self);
  with cdsDestinatario, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJCPF', ftString, 18);
    Add('XNome', ftString, 60);
    Add('XLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('XCpl', ftString, 60);
    Add('XBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('XMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('CPais', ftString, 4);
    Add('XPais', ftString, 60);
    Add('Fone', ftString, 15);
    Add('IE', ftString, 20);
    Add('ISUF', ftString, 9);

    CreateDataSet;
  end;

  cdsDadosNotasFiscais := TClientDataSet.Create(Self);
  with cdsDadosNotasFiscais, FieldDefs do
  begin
    Close;
    Clear;
    Add('tpDoc'         , ftString,   5); // Tipo Documento
    Add('CNPJCPF'       , ftString,  18); // CNPJCPF
    Add('Serie'         , ftString,   3); // Serie
    Add('ChaveAcesso'   , ftString,  44); // Chave Acesso
    Add('NotaFiscal'    , ftString,   9); // Numero Nota Fiscal
    Add('TextoImpressao', ftString, 100); // Texto Impressao no Relatorio
    CreateDataSet;
  end;

  cdsParametros := TClientDataSet.Create(Self);
  with cdsParametros, FieldDefs do
  begin
    Close;
    Clear;
    Add('ResumoCanhoto', ftString, 200);
    Add('Mensagem0', ftString, 60);
    Add('Versao', ftString, 5);
    Add('Imagem', ftString, 256);
    Add('Sistema', ftString, 150);
    Add('Usuario', ftString, 60);
    Add('Site', ftString, 60);
    Add('Email', ftString, 60);
    Add('Desconto', ftString, 60);
    Add('ChaveAcesso_Descricao', ftString, 90);
    Add('Contingencia_ID', ftString, 36);
    Add('Contingencia_Descricao', ftString, 60);
    Add('Contingencia_Valor', ftString, 60);
    Add('PrintCanhoto',ftString,1);
    Add('LinhasPorPagina', ftInteger);
    Add('LogoCarregado', ftBlob);

    CreateDataSet;
  end;

  cdsInformacoesAdicionais := TClientDataSet.Create(Self);
  with cdsInformacoesAdicionais, FieldDefs do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('OBS', ftString, 2000);
    FieldDefs.Add('infAdFisco', ftString, 2000);
    FieldDefs.Add('ObsCont', ftString, 1800);
    FieldDefs.Add('Fluxo_xOrig', ftString, 15);
    FieldDefs.Add('Fluxo_xDest', ftString, 15);
    FieldDefs.Add('Fluxo_xRota', ftString, 15);
    FieldDefs.Add('Entrega_tpPer', ftString, 50);
    FieldDefs.Add('Entrega_dProg', ftString, 10);
    FieldDefs.Add('Entrega_dIni',  ftString, 10);
    FieldDefs.Add('Entrega_dFim',  ftString, 10);

    CreateDataSet;
  end;

  cdsVolumes := TClientDataSet.Create(Self);
  with cdsVolumes, FieldDefs do
  begin
    Close;
    Clear;

    Add('Produto', ftString, 100);
    Add('CaracteristicaCarga', ftString, 100);
    Add('ValorServico', ftFloat);

    Add('DescTipo', ftString, 60);
    Add('UnMedida', ftString, 6);
    Add('QMedida', ftFloat);
    Add('MCub', ftFloat);
    Add('QVol', ftFloat);

    CreateDataSet;
  end;

  cdsTomador := TClientDataSet.Create(Self);
  with cdsTomador, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('IE', ftString, 20);
    Add('XNome', ftString, 60);
    Add('XFant', ftString, 60);
    Add('Fone', ftString, 15);
    Add('XLgr', ftString, 255);
    Add('Nro', ftString, 60);
    Add('XCpl', ftString, 60);
    Add('XBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('XMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('CPais', ftString, 4);
    Add('XPais', ftString, 60);
    Add('Email', ftString, 60);
    CreateDataSet;
  end;

  cdsExpedidor := TClientDataSet.Create(Self);
  with cdsExpedidor, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('XNome', ftString, 60);
    Add('XFant', ftString, 60);
    Add('XLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('XCpl', ftString, 60);
    Add('XBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('XMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('CPais', ftString, 4);
    Add('XPais', ftString, 60);
    Add('Fone', ftString, 15);
    Add('IE', ftString, 20);
    Add('IM', ftString, 15);
    Add('IEST', ftString, 20);
    Add('CRT', ftString, 1);

    CreateDataSet;
  end;

  cdsRecebedor := TClientDataSet.Create(Self);
  with cdsRecebedor, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('XNome', ftString, 60);
    Add('XFant', ftString, 60);
    Add('XLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('XCpl', ftString, 60);
    Add('XBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('XMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('CPais', ftString, 4);
    Add('XPais', ftString, 60);
    Add('Fone', ftString, 15);
    Add('IE', ftString, 20);
    Add('IM', ftString, 15);
    Add('IEST', ftString, 20);
    Add('CRT', ftString, 1);

    CreateDataSet;

  end;

  cdsRemetente := TClientDataSet.Create(Self);
  with cdsRemetente, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('XNome', ftString, 60);
    Add('XFant', ftString, 60);
    Add('XLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('XCpl', ftString, 60);
    Add('XBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('XMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('CPais', ftString, 4);
    Add('XPais', ftString, 60);
    Add('Fone', ftString, 15);
    Add('IE', ftString, 20);
    Add('IM', ftString, 15);
    Add('IEST', ftString, 20);
    Add('CRT', ftString, 1);

    CreateDataSet;
  end;

  cdsCalculoImposto := TClientDataSet.Create(Self);
  with cdsCalculoImposto, FieldDefs do
  begin
    Close;
    Clear;
    Add('TXTSITTRIB', ftString, 60);
    Add('VBC', ftFloat);
    Add('PICMS', ftFloat);
    Add('VICMS', ftFloat);
    Add('pRedBC', ftFloat);
    Add('VICMSST', ftFloat);
    Add('VCREDITO', ftFloat);
    Add('vIndSN', ftInteger);

    CreateDataSet;
  end;

  cdsComponentesPrestacao := TClientDataSet.Create(Self);
  with cdsComponentesPrestacao, FieldDefs do
  begin
    Close;
    Clear;
    Add('Nome', ftString, 60);
    Add('Valor', ftFloat);
    Add('TotalServico', ftFloat);
    Add('TotalReceber', ftFloat);
    CreateDataSet;
  end;

  cdsSeguro := TClientDataSet.Create(Self);
  with cdsSeguro, FieldDefs do
  begin
    Close;
    Clear;
    Add('RESPONSAVEL', ftString, 60);
    Add('NOMESEGURADORA', ftString, 60);
    Add('NUMEROAPOLICE', ftString, 60);
    Add('NUMEROAVERBACAO', ftString, 60);
    CreateDataSet;
  end;

  cdsModalRodoviario := TClientDataSet.Create(Self);
  with cdsModalRodoviario, FieldDefs do
  begin
    Close;
    Clear;
    Add('RNTRC', ftString, 60);
    Add('DATAPREVISTA', ftString, 60);
    Add('LOTACAO', ftString, 60);
    Add('CIOT', ftString, 12);
    Add('LACRES', ftString, 255);
    CreateDataSet;
  end;

  cdsRodoVeiculos := TClientDataSet.Create(Self);
  with cdsRodoVeiculos, FieldDefs do
  begin
    Close;
    Clear;
    Add('tpVeic', ftString, 10);
    Add('placa', ftString, 7);
    Add('UF', ftString, 2);
    Add('RNTRC', ftString, 8);
    Add('RENAVAM', ftString, 11);
    Add('TAF', ftString, 12);
    Add('NroRegEstadual', ftString, 25);
    Add('CPF/CNPJ', ftString, 18);
    CreateDataSet;
  end;

  cdsRodoValePedagio := TClientDataSet.Create(Self);
  with cdsRodoValePedagio, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJPg', ftString, 18);
    Add('CNPJForn', ftString, 18);
    Add('nCompra', ftString, 14);
    Add('vValePed', ftFloat);
    CreateDataSet;
  end;

  cdsRodoMotorista := TClientDataSet.Create(Self);
  with cdsRodoMotorista, FieldDefs do
  begin
    Close;
    Clear;
    Add('xNome', ftString, 60);
    Add('CPF', ftString, 11);
    CreateDataSet;
  end;

  cdsModalAquaviario := TClientDataSet.Create(Self);
  with cdsModalAquaviario, FieldDefs do
  begin
    Close;
    Clear;
    Add('vPrest', ftFloat);
    Add('vAFRMM', ftFloat);
    Add('nBooking', ftString, 10);
    Add('nCtrl', ftString, 10);
    Add('xNavio', ftString, 60);
    Add('nViag', ftInteger);
    Add('direc', ftString, 8);
    Add('prtEmb', ftString, 60);
    Add('prtTrans', ftString, 60);
    Add('prtDest', ftString, 60);
    Add('tpNav', ftString, 10);
    Add('irin', ftString, 10);
    Add('xBalsa', ftString, 190);
    CreateDataSet;
  end;


  cdsModalAereo := TClientDataSet.Create(Self);
  with cdsModalAereo, FieldDefs do
  begin
    Close;
    Clear;
    Add('nMinu', ftInteger);
    Add('nOCA', ftString, 11);
    Add('dPrevAereo', ftDateTime);
    Add('xLAgEmi', ftString, 20);
    Add('IdT', ftString, 14);
    Add('CL', ftString, 1);
    Add('cTar', ftString, 4);
    Add('vTar', ftCurrency);
    Add('xDime', ftString, 14);
    Add('cInfManu', ftString, 30);
    Add('cIMP', ftString, 3);
    Add('xOrig',ftString,15);
    Add('xDest',ftString,15);
    Add('xRota',ftString,15);
    CreateDataSet;
  end;

  cdsMultiModal := TClientDataSet.Create(Self);
  with cdsMultiModal, FieldDefs do
  begin
    Close;
    Clear;
    Add('COTM', ftString, 20);
    Add('indNegociavel', ftString, 1);
    CreateDataSet;
  end;

  cdsDocAnterior := TClientDataSet.Create(Self);
  with cdsDocAnterior, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJCPF', ftString, 18);
    Add('xNome', ftString, 60);
    Add('UF', ftString, 2);
    Add('IE', ftString, 20);

    Add('Tipo', ftString, 33);
    Add('Serie', ftString, 3);
    Add('nDoc', ftString, 20);
    Add('dEmi', ftString, 10);
    Add('Chave', ftString, 44);
    CreateDataSet;
  end;

  cdsAnuladoComple := TClientDataSet.Create(Self);
  with cdsAnuladoComple, FieldDefs do
  begin
    Close;
    Clear;
    Add('Chave', ftString, 44);
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
    Add('grupoAlterado', ftBlob);
    Add('campoAlterado', ftBlob);
    Add('valorAlterado', ftBlob);
    Add('nroItemAlterado', ftInteger);

    Add('dhEntrega', ftDateTime);
    Add('nDoc', ftString, 20);
    Add('xNome', ftString, 60);
    Add('latitude', ftFloat);
    Add('longitude', ftFloat);

    Add('nProtCE', ftString, 15);

    CreateDataSet;
  end;

  cdsProdutosPerigosos := TClientDataSet.Create(Self);
  with cdsProdutosPerigosos, FieldDefs do
  begin
   	Close;
   	Clear;
		Add('nONU',        ftString, 4);
		Add('xNomeAE',     ftString, 150);
		Add('xClaRisco',   ftString, 40);
		Add('grEmb',       ftString, 6);
		Add('qTotProd',    ftString, 20);
		Add('qVolTipo',    ftString, 60);
		Add('pontoFulgor', ftString, 6);

		CreateDataSet;
  end;
  cdsVeiculosNovos := TClientDataSet.Create(Self);
  with cdsVeiculosNovos, FieldDefs do
  begin
   	Close;
   	Clear;
		Add('Chassi', ftString, 17);
		Add('cCor',   ftString, 4);
		Add('xCor',   ftString, 40);
		Add('cMod',   ftString, 6);
        Add('vUnit',  ftFloat);
        Add('vFrete', ftFloat);

		CreateDataSet;
  end;

  //Inutilização
  cdsInutilizacao := TClientDataSet.Create(Self);
  with cdsInutilizacao, FieldDefs do
  begin
   	Close;
   	Clear;
    Add('ID', ftString, 44);
    Add('CNPJ', ftString, 20);
    Add('nProt', ftString, 20);
    Add('Modelo', ftInteger);
    Add('Serie', ftInteger);
    Add('Ano', ftInteger);
    Add('nCTIni', ftInteger);
    Add('nCTFin', ftInteger);
    Add('xJust', ftString, 50);
    Add('versao', ftString, 20);
    Add('TpAmb', ftString, 32);
    Add('verAplic', ftString, 20);
    Add('cStat', ftInteger);
    Add('xMotivo', ftString, 50);
    Add('cUF', ftString, 2);
    Add('dhRecbto', ftDateTime);
		CreateDataSet;
  end;

  cdsInfServico := TClientDataSet.Create(Self);
  with cdsInfServico, FieldDefs do
  begin
   	Close;
   	Clear;
		Add('xDescServ', ftString, 30);
    Add('qCarga', ftFloat);
		CreateDataSet;
  end;

  cdsInfTribFed := TClientDataSet.Create(Self);
  with cdsInfTribFed, FieldDefs do
  begin
    Close;
    Clear;
    Add('vPIS', ftFloat);
    Add('vCOFINS', ftFloat);
    Add('vIR', ftFloat);
    Add('vINSS', ftFloat);
    Add('vCSLL', ftFloat);
		CreateDataSet;
  end;

  cdsPercurso := TClientDataSet.Create(Self);
  with cdsPercurso, FieldDefs do
  begin
   	Close;
   	Clear;
		Add('UFsPer', ftString, 110);
		CreateDataSet;
  end;

  // frxDB
  frxIdentificacao := TfrxDBDataset.Create(Self);
  with frxIdentificacao do
  begin
    UserName       := 'Identificacao';
    OpenDataSource := False;
    DataSet        := cdsIdentificacao;
  end;

  frxEmitente := TfrxDBDataset.Create(Self);
  with frxEmitente do
  begin
    UserName       := 'Emitente';
    OpenDataSource := False;
    DataSet        := cdsEmitente;
  end;

  frxDestinatario := TfrxDBDataset.Create(Self);
  with frxDestinatario do
  begin
    UserName       := 'Destinatario';
    OpenDataSource := False;
    DataSet        := cdsDestinatario;
  end;

  frxDadosNotasFiscais := TfrxDBDataset.Create(Self);
  with frxDadosNotasFiscais do
  begin
    UserName       := 'DadosNotasFiscais';
    OpenDataSource := False;
    DataSet        := cdsDadosNotasFiscais;
  end;

  frxParametros := TfrxDBDataset.Create(Self);
  with frxParametros do
  begin
    UserName       := 'Parametros';
    OpenDataSource := False;
    DataSet        := cdsParametros;
  end;

  frxVolumes := TfrxDBDataset.Create(Self);
  with frxVolumes do
  begin
    UserName       := 'Volumes';
    OpenDataSource := False;
    DataSet        := cdsVolumes;
  end;

  frxInformacoesAdicionais := TfrxDBDataset.Create(Self);
  with frxInformacoesAdicionais do
  begin
    UserName       := 'InformacoesAdicionais';
    OpenDataSource := False;
    DataSet        := cdsInformacoesAdicionais;
  end;

  frxTomador := TfrxDBDataset.Create(Self);
  with frxTomador do
  begin
    UserName       := 'Tomador';
    OpenDataSource := False;
    DataSet        := cdsTomador;
  end;

  frxExpedidor := TfrxDBDataset.Create(Self);
  with frxExpedidor do
  begin
    UserName       := 'Expedidor';
    OpenDataSource := False;
    DataSet        := cdsExpedidor;
  end;

  frxRecebedor := TfrxDBDataset.Create(Self);
  with frxRecebedor do
  begin
    UserName       := 'Recebedor';
    OpenDataSource := False;
    DataSet        := cdsRecebedor;
  end;

  frxRemetente := TfrxDBDataset.Create(Self);
  with frxRemetente do
  begin
    UserName       := 'Remetente';
    OpenDataSource := False;
    DataSet        := cdsRemetente;
  end;

  frxCalculoImposto := TfrxDBDataset.Create(Self);
  with frxCalculoImposto do
  begin
    UserName       := 'CalculoImposto';
    OpenDataSource := False;
    DataSet        := cdsCalculoImposto;
  end;

  frxComponentesPrestacao := TfrxDBDataset.Create(Self);
  with frxComponentesPrestacao do
  begin
    UserName       := 'ComponentesPrestacao';
    OpenDataSource := False;
    DataSet        := cdsComponentesPrestacao;
  end;

  frxSeguro := TfrxDBDataset.Create(Self);
  with frxSeguro do
  begin
    UserName       := 'Seguro';
    OpenDataSource := False;
    DataSet        := cdsSeguro;
  end;

  frxModalRodoviario := TfrxDBDataset.Create(Self);
  with frxModalRodoviario do
  begin
    UserName       := 'ModalRodoviario';
    OpenDataSource := False;
    DataSet        := cdsModalRodoviario;
  end;

  frxModalAquaviario := TfrxDBDataset.Create(Self);
  with frxModalAquaviario do
  begin
    UserName       := 'ModalAquaviario';
    OpenDataSource := False;
    DataSet        := cdsModalAquaviario;
  end;

  frxModalAereo := TfrxDBDataset.Create(Self);
  with frxModalAereo do
  begin
    UserName       := 'ModalAereo';
    OpenDataSource := False;
    DataSet := cdsModalAereo;
  end;

  frxMultiModal := TfrxDBDataset.Create(Self);
  with frxMultiModal do
  begin
    UserName       := 'MultiModal';
    OpenDataSource := False;
    DataSet := cdsMultiModal;
  end;

  frxRodoVeiculos := TfrxDBDataset.Create(Self);
  with frxRodoVeiculos do
  begin
    UserName       := 'Veiculos';
    OpenDataSource := False;
    DataSet        := cdsRodoVeiculos;
  end;

  frxRodoValePedagio := TfrxDBDataset.Create(Self);
  with frxRodoValePedagio do
  begin
    UserName       := 'ValePedagio';
    OpenDataSource := False;
    DataSet        := cdsRodoValePedagio;
  end;

  frxRodoMotorista := TfrxDBDataset.Create(Self);
  with frxRodoMotorista do
  begin
    UserName       := 'Motorista';
    OpenDataSource := False;
    DataSet        := cdsRodoMotorista;
  end;

  frxDocAnterior := TfrxDBDataset.Create(Self);
  with frxDocAnterior do
  begin
    UserName       := 'DocAnterior';
    OpenDataSource := False;
    DataSet        := cdsDocAnterior;
  end;

  frxAnuladoComple := TfrxDBDataset.Create(Self);
  with frxAnuladoComple do
  begin
    UserName       := 'AnuladoComple';
    OpenDataSource := False;
    DataSet        := cdsAnuladoComple;
  end;

  frxEventos := TfrxDBDataset.Create(Self);
  with frxEventos do
  begin
    UserName       := 'Eventos';
    OpenDataSource := False;
    DataSet        := cdsEventos;
  end;

  frxProdutosPerigosos := TfrxDBDataset.Create(Self);
  with frxProdutosPerigosos do
  begin
		UserName       := 'ProdutosPerigosos';
  	OpenDataSource := False;
		DataSet        := cdsProdutosPerigosos;
  end;
  frxVeiculosNovos := TfrxDBDataset.Create(Self);
  with frxVeiculosNovos do
  begin
		UserName       := 'VeiculosNovos';
     	OpenDataSource := False;
		DataSet        := cdsVeiculosNovos;
  end;
  frxInutilizacao  := TfrxDBDataset.Create(Self);
  with frxInutilizacao do
  begin
		UserName       := 'Inutilizacao';
    OpenDataSource := False;
		DataSet        := cdsInutilizacao;
  end;
  frxInfServico := TfrxDBDataset.Create(Self);
  with frxInfServico do
  begin
		UserName       := 'InfServico';
    OpenDataSource := False;
		DataSet        := cdsInfServico;
  end;
  frxInfTribFed := TfrxDBDataset.Create(Self);
  with frxInfTribFed do
  begin
		UserName       := 'InfTribFed';
    OpenDataSource := False;
		DataSet        := cdsInfTribFed;
  end;
  frxPercurso := TfrxDBDataset.Create(Self);
  with frxPercurso do
  begin
    UserName       := 'Percurso';
    OpenDataSource := False;
    DataSet        := cdsPercurso;
  end;
  frxBarCodeObject := TfrxBarCodeObject.Create(Self);
end;

destructor TACBrCTeDACTEFR.Destroy;
begin
  frxReport.Free;
  frxPDFExport.Free;
  // CDS
  cdsIdentificacao.Free;
  cdsEmitente.Free;
  cdsDestinatario.Free;
  cdsDadosNotasFiscais.Free;
  cdsParametros.Free;
  cdsInformacoesAdicionais.Free;
  cdsVolumes.Free;
  cdsTomador.Free;
  cdsExpedidor.Free;
  cdsRecebedor.Free;
  cdsRemetente.Free;
  cdsCalculoImposto.Free;
  cdsComponentesPrestacao.Free;
  cdsSeguro.Free;
  cdsModalRodoviario.Free;
  cdsRodoVeiculos.Free;
  cdsRodoValePedagio.Free;
  cdsRodoMotorista.Free;
  cdsModalAquaviario.Free;
  cdsModalAereo.Free;
  cdsMultiModal.Free;
  cdsDocAnterior.Free;
  cdsAnuladoComple.Free;
  cdsEventos.Free;
  cdsProdutosPerigosos.Free;
  cdsVeiculosNovos.Free;
  cdsInutilizacao.Free;
  cdsInfServico.Free;
  cdsInfTribFed.Free;
  cdsPercurso.Free;

  // frxDB
  frxIdentificacao.Free;
  frxEmitente.Free;
  frxDestinatario.Free;
  frxDadosNotasFiscais.Free;
  frxParametros.Free;
  frxVolumes.Free;
  frxInformacoesAdicionais.Free;
  frxTomador.Free;
  frxExpedidor.Free;
  frxRecebedor.Free;
  frxRemetente.Free;
  frxCalculoImposto.Free;
  frxComponentesPrestacao.Free;
  frxSeguro.Free;
  frxModalRodoviario.Free;
  frxModalAquaviario.Free;
  frxModalAereo.Free;
  frxMultiModal.Free;
  frxRodoVeiculos.Free;
  frxRodoValePedagio.Free;
  frxRodoMotorista.Free;
  frxDocAnterior.Free;
  frxAnuladoComple.Free;
  frxEventos.Free;
  frxBarCodeObject.Free;
  frxProdutosPerigosos.Free;
  frxVeiculosNovos.Free;
  frxInutilizacao.Free;
  frxInfServico.Free;
  frxInfTribFed.Free;
  frxPercurso.Free;

  inherited Destroy;
end;

procedure TACBrCTeDACTEFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  ChildEvento, Child: TfrxChild;
  DetailData: TfrxDetailData;
//  Memo      : TfrxMemoView;
//  Shape     : TfrxShapeView;
  qrCode    : string;
begin
  ChildEvento := frxReport.FindObject('ChildProcEvento') as TfrxChild;
  Child := nil;
  if ChildEvento <> nil then
  begin
    case TipoEvento of
      teCCe:
        Child := frxReport.FindObject('ChildCorrecao') as TfrxChild;
      teCancelamento, tePrestDesacordo:
         Child := frxReport.FindObject('ChildJustificativa') as TfrxChild;
      teComprEntrega:
         Child := frxReport.FindObject('ChildComprovanteEntrega') as TfrxChild;
      teCancComprEntrega:
         Child := frxReport.FindObject('ChildCancComp') as TfrxChild;

    end;
    if Child <> nil then
       ChildEvento.Child := Child;
  end;

  DetailData := frxReport.FindObject('DadosCorrecao') as TfrxDetailData;
  if DetailData <> nil then
    DetailData.Visible := TipoEvento = teCCe;


  if cdsModalRodoviario.FieldByName('LOTACAO').AsString = 'Não' then
  begin
    Child := frxReport.FindObject('ChildRodoviarioLotacao') as TfrxChild;
    if Child <> nil then
    begin
      Child.Visible := False;
    end;
  end;
  if Assigned(FCTe) then
  begin
    qrCode := FCTe.infCTeSupl.qrCodCTe;
    if Assigned(Sender) and (Trim(qrCode) <> '') and (Sender.Name = 'ImgQrCode') then
      PintarQRCode(qrCode, TfrxPictureView(Sender).Picture.Bitmap, qrUTF8NoBOM);
  end;
end;

function TACBrCTeDACTEFR.GetPreparedReport: TfrxReport;
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

function TACBrCTeDACTEFR.GetPreparedReportEvento: TfrxReport;
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

function TACBrCTeDACTEFR.GetPreparedReportInutilizacao: TfrxReport;
begin
  if Trim(FFastFileInutilizacao) = '' then
    Result := nil
  else
  begin
    if PrepareReportInutilizacao then
      Result := frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrCTeDACTEFR.ImprimirDACTe(ACTE: TCTe);
begin
  if PrepareReport(ACTE) then
  begin
    if MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrCTeDACTEFR.ImprimirDACTePDF(ACTE: TCTe);
const
  TITULO_PDF = 'Conhecimento de Transporte Eletrônico';
var
  OldShowDialog: Boolean;
  NomeArq :string;
begin
  if PrepareReport(ACTE) then
  begin
    frxPDFExport.Author   := Sistema;
    frxPDFExport.Creator  := Sistema;
    frxPDFExport.Producer := Sistema;
    frxPDFExport.Title    := TITULO_PDF;
    frxPDFExport.Subject  := TITULO_PDF;
    frxPDFExport.Keywords := TITULO_PDF;
    OldShowDialog         := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := False;
      NomeArq := Trim(DACTEClassOwner.NomeDocumento);
      if EstaVazio(NomeArq) then
        NomeArq := OnlyNumber(CTE.infCTe.Id) + '-cte.pdf';
      frxPDFExport.FileName := PathWithDelim(DACTEClassOwner.PathPDF) + NomeArq;

      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
         ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := OldShowDialog;
      FPArquivoPDF := frxPDFExport.FileName;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.ImprimirEVENTO(ACTE: TCTe);
begin
  if PrepareReportEvento then
  begin
    if MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrCTeDACTEFR.ImprimirEVENTOPDF(ACTE: TCTe);
const
  TITULO_PDF = 'Conhecimento de Transporte Eletrônico - Evento';
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
    OldShowDialog         := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := False;
      NomeArq := Trim(DACTEClassOwner.NomeDocumento);
      if EstaVazio(NomeArq) then
        NomeArq := OnlyNumber(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[0].InfEvento.Id) + '-procEventoCTe.pdf';
      frxPDFExport.FileName := PathWithDelim(DACTEClassOwner.PathPDF) + NomeArq;

      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
        ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := OldShowDialog;
      FPArquivoPDF := frxPDFExport.FileName;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.ImprimirINUTILIZACAO(ACTE: TCTe);
begin
  if PrepareReportInutilizacao then
  begin
    if MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrCTeDACTEFR.ImprimirINUTILIZACAOPDF(ACTE: TCTe);
const
  TITULO_PDF = 'Inutilização de Numeração';
var
  NomeArq      : String;
  OldShowDialog: Boolean;
begin
  if PrepareReportInutilizacao then
  begin
    frxPDFExport.Author   := Sistema;
    frxPDFExport.Creator  := Sistema;
    frxPDFExport.Producer := Sistema;
    frxPDFExport.Title    := TITULO_PDF;
    frxPDFExport.Subject  := TITULO_PDF;
    frxPDFExport.Keywords := TITULO_PDF;
    OldShowDialog         := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := False;
      NomeArq := Trim(DACTEClassOwner.NomeDocumento);
      if EstaVazio(NomeArq) then
        NomeArq := OnlyNumber(TACBrCTe(ACBrCTe).InutCTe.RetInutCTe.Id) + '-procInutCTe.pdf';
      frxPDFExport.FileName := PathWithDelim(DACTEClassOwner.PathPDF) + NomeArq;


      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
        ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := OldShowDialog;
      FPArquivoPDF := frxPDFExport.FileName;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.AjustaMargensReports;
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

procedure TACBrCTeDACTEFR.LimpaDados;
begin
  cdsIdentificacao.EmptyDataSet;
  cdsEmitente.EmptyDataSet;
  cdsDestinatario.EmptyDataSet;
  cdsDadosNotasFiscais.EmptyDataSet;
  cdsParametros.EmptyDataSet;
  cdsInformacoesAdicionais.EmptyDataSet;
  cdsVolumes.EmptyDataSet;
  cdsTomador.EmptyDataSet;
  cdsExpedidor.EmptyDataSet;
  cdsRecebedor.EmptyDataSet;
  cdsRemetente.EmptyDataSet;
  cdsCalculoImposto.EmptyDataSet;
  cdsComponentesPrestacao.EmptyDataSet;
  cdsSeguro.EmptyDataSet;
  cdsModalRodoviario.EmptyDataSet;
  cdsRodoVeiculos.EmptyDataSet;
  cdsRodoValePedagio.EmptyDataSet;
  cdsRodoMotorista.EmptyDataSet;
  cdsModalAereo.EmptyDataSet;
  cdsMultiModal.EmptyDataSet;
  cdsModalAquaviario.EmptyDataSet;
  cdsDocAnterior.EmptyDataSet;
  cdsAnuladoComple.EmptyDataSet;
  cdsEventos.EmptyDataSet;
  cdsProdutosPerigosos.EmptyDataSet;
  cdsVeiculosNovos.EmptyDataSet;
  cdsInfServico.EmptyDataSet;
  cdsInfTribFed.EmptyDataSet;
  cdsPercurso.EmptyDataSet;
end;

function TACBrCTeDACTEFR.PrepareReport(ACTE: TCTe): Boolean;
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
      frxReport.FileName := '';
      frxReport.LoadFromStream(Stream);
      Stream.Free;
    end
    else
    if FileExists(FastFile) then
      frxReport.LoadFromFile(FastFile)
    else
      raise EACBrCTeDACTEFR.CreateFmt('Caminho do arquivo de impressão do DACTE "%s" inválido.', [FastFile]);
  end
  else
    raise EACBrCTeDACTEFR.Create('Caminho do arquivo de impressão do DACTE não assinalado.');

  frxReport.PrintOptions.Copies := NumCopias;
  frxReport.PrintOptions.ShowDialog := MostraSetup;
  frxReport.ShowProgress := MostraStatus;
  frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  if Assigned(ACTE) then
  begin
    FCTe := ACTE;
    CarregaDados;
    Result := frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrCTe) then
    begin
      for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 do
      begin
        FCTe := TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTE;
        CarregaDados;

        if (i > 0) then
          Result := frxReport.PrepareReport(False)
        else
          Result := frxReport.PrepareReport;
      end;
    end
    else
      raise EACBrCTeDACTEFR.Create('Propriedade ACBrCTe não assinalada.');
  end;

  AjustaMargensReports;

end;

function TACBrCTeDACTEFR.PrepareReportEvento: Boolean;
var Stream: TStringStream;
begin
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
      raise EACBrCTeDACTEFR.CreateFmt('Caminho do arquivo de impressão do EVENTO "%s" inválido.', [FastFileEvento]);
  end
  else
    raise EACBrCTeDACTEFR.Create('Caminho do arquivo de impressão do EVENTO não assinalado.');

  frxReport.PrintOptions.Copies := NumCopias;
  frxReport.PrintOptions.ShowDialog := MostraSetup;
  frxReport.ShowProgress := MostraStatus;
  frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  // preparar relatorio
  if Assigned(ACBrCTe) then
  begin
    if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
    begin
      FCTe := TACBrCTe(ACBrCTe).Conhecimentos.Items[0].CTE;
      CarregaDados;
    end;
    if Assigned(TACBrCTe(ACBrCTe).EventoCTe) then
    begin
      Evento := TACBrCTe(ACBrCTe).EventoCTe;
      CarregaDadosEventos;
    end
    else
      raise EACBrCTeDACTEFR.Create('Evento não foi assinalado.');


    Result := frxReport.PrepareReport;
  end
  else
    raise EACBrCTeDACTEFR.Create('Propriedade ACBrCTe não assinalada.');

  AjustaMargensReports;

end;

function TACBrCTeDACTEFR.PrepareReportInutilizacao: Boolean;
begin
  if Trim(FastFileInutilizacao) <> '' then
  begin
    if FileExists(FastFileInutilizacao) then
      frxReport.LoadFromFile(FastFileInutilizacao)
    else
      raise EACBrCTeDACTEFR.CreateFmt('Caminho do arquivo de impressão de INUTILIZAÇÃO "%s" inválido.', [FastFileInutilizacao]);
  end
  else
    raise EACBrCTeDACTEFR.Create('Caminho do arquivo de impressão do INUTILIZAÇÃO não assinalado.');

  frxReport.PrintOptions.Copies := NumCopias;
  frxReport.PrintOptions.ShowDialog := MostraSetup;
  frxReport.ShowProgress := MostraStatus;
  frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  // preparar relatorio
  if Assigned(ACBrCTe) then
  begin
    if assigned(TACBrCTe(ACBrCTe).InutCTe) then
    begin
      Inutilizacao := TACBrCTe(ACBrCTe).InutCTe.RetInutCTe;
      CarregaDadosInutilizacao;
    end
    else
      raise EACBrCTeDACTEFR.Create('INUTILIZAÇÃO não foi assinalada.');

    Result := frxReport.PrepareReport;
  end
  else
    raise EACBrCTeDACTEFR.Create('Propriedade ACBrCTe não assinalada.');

  AjustaMargensReports;

end;

procedure TACBrCTeDACTEFR.SetDataSetsToFrxReport;
begin
  frxReport.DataSets.Clear;
  with frxReport.EnabledDataSets do
  begin
    Clear;
    Add(frxIdentificacao);
    Add(frxEmitente);
    Add(frxDestinatario);
    Add(frxDadosNotasFiscais);
    Add(frxParametros);
    Add(frxVolumes);
    Add(frxInformacoesAdicionais);
    Add(frxTomador);
    Add(frxExpedidor);
    Add(frxRecebedor);
    Add(frxRemetente);
    Add(frxCalculoImposto);
    Add(frxComponentesPrestacao);
    Add(frxSeguro);
    Add(frxModalRodoviario);
    Add(frxModalAquaviario);
    Add(frxModalAereo);
    Add(frxMultiModal);
    Add(frxRodoVeiculos);
    Add(frxRodoValePedagio);
    Add(frxRodoMotorista);
    Add(frxDocAnterior);
    Add(frxAnuladoComple);
  	Add(frxEventos);
	  Add(frxProdutosPerigosos);
  	Add(frxVeiculosNovos);
    Add(frxInutilizacao);
    Add(frxInfServico);
    Add(frxInfTribFed);
    Add(frxPercurso);
  end;
end;


procedure TACBrCTeDACTEFR.CarregaCalculoImposto;
begin
  with cdsCalculoImposto do
  begin
    Append;

    case FCTe.Imp.ICMS.SituTrib of
      cst00:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst00);
          FieldByName('vBC').AsFloat         := FCTe.Imp.ICMS.ICMS00.vBC;
          FieldByName('pICMS').AsFloat       := FCTe.Imp.ICMS.ICMS00.pICMS;
          FieldByName('vICMS').AsFloat       := FCTe.Imp.ICMS.ICMS00.VICMS;
        end;
      cst20:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst20);
          FieldByName('pRedBC').AsFloat      := FCTe.Imp.ICMS.ICMS20.pRedBC;
          FieldByName('vBC').AsFloat         := FCTe.Imp.ICMS.ICMS20.vBC;
          FieldByName('pICMS').AsFloat       := FCTe.Imp.ICMS.ICMS20.pICMS;
          FieldByName('vICMS').AsFloat       := FCTe.Imp.ICMS.ICMS20.VICMS;
        end;
      cst40:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst40);
        end;
      cst41:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst41);
        end;

      cst45:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst45);
        end;

      cst51:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst51);
        end;

      cst60:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst60);
          FieldByName('vBC').AsFloat         := FCTe.Imp.ICMS.ICMS60.vBCSTRet;
          FieldByName('pICMS').AsFloat       := FCTe.Imp.ICMS.ICMS60.pICMSSTRet;
          FieldByName('vICMS').AsFloat       := FCTe.Imp.ICMS.ICMS60.vICMSSTRet;
          FieldByName('vCredito').AsFloat    := FCTe.Imp.ICMS.ICMS60.vCred;
        end;
      cst90:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cst90);
          FieldByName('pRedBC').AsFloat      := FCTe.Imp.ICMS.ICMS90.pRedBC;
          FieldByName('vBC').AsFloat         := FCTe.Imp.ICMS.ICMS90.vBC;
          FieldByName('pICMS').AsFloat    := FCTe.Imp.ICMS.ICMS90.pICMS;
          FieldByName('vICMS').AsFloat    := FCTe.Imp.ICMS.ICMS90.VICMS;
          FieldByName('vCredito').AsFloat := FCTe.Imp.ICMS.ICMS90.vCred;
        end;
      cstICMSOutraUF:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cstICMSOutraUF);
          FieldByName('pRedBC').AsFloat      := FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF;
          FieldByName('vBC').AsFloat         := FCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF;
          FieldByName('pICMS').AsFloat       := FCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF; // pRedBCOutraUF;
          FieldByName('vICMS').AsFloat       := FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF;
        end;
      cstICMSSN:
        begin
          FieldByName('TXTSITTRIB').AsString := CSTICMSToStrTagPosText(cstICMSSN);
          FieldByName('vIndSN').AsFloat      := FCTe.Imp.ICMS.ICMSSN.indSN;
        end;
    end;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaComponentesPrestacao;
var
  i: Integer;
begin
  with cdsComponentesPrestacao do
  begin

    if CTE.vPrest.comp.Count > 0 then
    begin
      for i := 0 to CTE.vPrest.comp.Count - 1 do
      begin
        Append;
        FieldByName('Nome').AsString        := CTE.vPrest.comp.Items[i].xNome;
        FieldByName('Valor').AsFloat        := CTE.vPrest.comp.Items[i].vComp;
        FieldByName('TotalServico').AsFloat := CTE.vPrest.vTPrest;
        FieldByName('TotalReceber').AsFloat := CTE.vPrest.vRec;
        Post;
      end;
    end
    else
    begin
      Append;
      FieldByName('Nome').AsString        := '';
      FieldByName('Valor').AsFloat        := 0;
      FieldByName('TotalServico').AsFloat := CTE.vPrest.vTPrest;
      FieldByName('TotalReceber').AsFloat := CTE.vPrest.vRec;
      Post;
    end;
  end;

end;

procedure TACBrCTeDACTEFR.CarregaCTeAnuladoComplementado;
var i : cardinal;
begin
  with cdsAnuladoComple do
  begin
    if CTE.ide.tpCTe = tcComplemento then
    begin
      if CTE.infCTe.versao <= 3 then
      begin
        Append;
        FieldByName('Chave').AsString := CTE.InfCTeComp.Chave;
        Post;
      end
      else
      begin
        for i := 0 to Pred(CTE.infCteComp10.Count) do
        begin
          Append;
          FieldByName('Chave').AsString := CTE.infCteComp10[i].chCTe;
          Post;
        end;
      end;
    end else
    if CTE.ide.tpCTe = tcAnulacao then
    begin
      Append;
      FieldByName('Chave').AsString := CTE.infCteAnu.chCTe;
      Post;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaDados;
begin
  LimpaDados;

  CarregaIdentificacao;
  CarregaTomador;
  CarregaEmitente;
  CarregaRemetente;
  CarregaDestinatario;
  CarregaExpedidor;
  CarregaRecebedor;
  CarregaDadosNotasFiscais;
  CarregaParametros;
  CarregaCalculoImposto;
  CarregaVolumes;
  CarregaComponentesPrestacao;
  CarregaInformacoesAdicionais;
  CarregaSeguro;
  CarregaModalRodoviario;
  CarregaModalAereo;
  CarregaMultiModal;
  CarregaModalAquaviario;
  CarregaDocumentoAnterior;
  CarregaCTeAnuladoComplementado;
  CarregaProdutosPerigosos;
  CarregaVeiculosNovos;
  CarregaInfServico;
  CarregaInfTribFed;
  CarregaPercurso;
end;

procedure TACBrCTeDACTEFR.CarregaDadosEventos;
  Function MantertpAmb( s : TpcnTipoAmbiente ) : String;
  begin
    case s of
      taProducao    : Result := 'PRODUÇÃO';
      taHomologacao : Result := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';
    end;
  end;
var
  i: Integer;
  J: Integer;
begin
  with cdsEventos do
  begin
    EmptyDataSet;
    for i := 0 to FEvento.Evento.Count - 1 do
    begin
      with Evento.Evento[i] do
      begin
        case Evento.Evento[i].InfEvento.tpEvento of
          tePrestDesacordo:
            begin
              TipoEvento := tePrestDesacordo;
              Append;
              FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
              FieldByName('Modelo').AsString              := Copy(InfEvento.chCTe, 21, 2);
              FieldByName('Serie').AsString               := Copy(InfEvento.chCTe, 23, 3);
              FieldByName('Numero').AsString              := Copy(InfEvento.chCTe, 26, 9);
              FieldByName('MesAno').AsString              := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
              FieldByName('Barras').AsString              := InfEvento.chCTe;
              FieldByName('ChaveAcesso').AsString         := FormatarChaveAcesso(InfEvento.chCTe);
              FieldByName('cOrgao').AsInteger             := InfEvento.cOrgao;
              FieldByName('nSeqEvento').AsInteger         := InfEvento.nSeqEvento;
              FieldByName('tpAmb').AsString               := MantertpAmb( InfEvento.tpAmb );
              FieldByName('dhEvento').AsDateTime          := InfEvento.dhEvento;
              FieldByName('TipoEvento').AsString          := InfEvento.TipoEvento;
              FieldByName('DescEvento').AsString          := InfEvento.DescEvento;
              FieldByName('versaoEvento').AsString        := InfEvento.versaoEvento;
              FieldByName('cStat').AsInteger              := RetInfEvento.cStat;
              FieldByName('xMotivo').AsString             := RetInfEvento.xMotivo;
              FieldByName('nProt').AsString               := RetInfEvento.nProt;
              FieldByName('dhRegEvento').AsDateTime       := RetInfEvento.dhRegEvento;
              FieldByName('xJust').AsString               := InfEvento.detEvento.xOBS;
              FieldByName('xCondUso').AsString            := '';
              frxReport.Variables['HOMOLOGACAO']          := ( InfEvento.tpAmb = taHomologacao);
              Post;
            end;
          teCancPrestDesacordo:
            begin
              TipoEvento := teCancPrestDesacordo;
              Append;
              FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
              FieldByName('Modelo').AsString              := Copy(InfEvento.chCTe, 21, 2);
              FieldByName('Serie').AsString               := Copy(InfEvento.chCTe, 23, 3);
              FieldByName('Numero').AsString              := Copy(InfEvento.chCTe, 26, 9);
              FieldByName('MesAno').AsString              := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
              FieldByName('Barras').AsString              := InfEvento.chCTe;
              FieldByName('ChaveAcesso').AsString         := FormatarChaveAcesso(InfEvento.chCTe);
              FieldByName('cOrgao').AsInteger             := InfEvento.cOrgao;
              FieldByName('nSeqEvento').AsInteger         := InfEvento.nSeqEvento;
              FieldByName('tpAmb').AsString               := MantertpAmb( InfEvento.tpAmb );
              FieldByName('dhEvento').AsDateTime          := InfEvento.dhEvento;
              FieldByName('TipoEvento').AsString          := InfEvento.TipoEvento;
              FieldByName('DescEvento').AsString          := InfEvento.DescEvento;
              FieldByName('versaoEvento').AsString        := InfEvento.versaoEvento;
              FieldByName('cStat').AsInteger              := RetInfEvento.cStat;
              FieldByName('xMotivo').AsString             := RetInfEvento.xMotivo;
              FieldByName('nProt').AsString               := RetInfEvento.nProt;
              FieldByName('dhRegEvento').AsDateTime       := RetInfEvento.dhRegEvento;
              FieldByName('xJust').AsString               := InfEvento.detEvento.xOBS;
              FieldByName('xCondUso').AsString            := '';
              frxReport.Variables['HOMOLOGACAO']          := ( InfEvento.tpAmb = taHomologacao);
              Post;
            end;
          teCancelamento:
            begin
              TipoEvento := teCancelamento;
              Append;
              FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
              FieldByName('Modelo').AsString              := Copy(InfEvento.chCTe, 21, 2);
              FieldByName('Serie').AsString               := Copy(InfEvento.chCTe, 23, 3);
              FieldByName('Numero').AsString              := Copy(InfEvento.chCTe, 26, 9);
              FieldByName('MesAno').AsString              := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
              FieldByName('Barras').AsString              := InfEvento.chCTe;
              FieldByName('ChaveAcesso').AsString         := FormatarChaveAcesso(InfEvento.chCTe);
              FieldByName('cOrgao').AsInteger             := InfEvento.cOrgao;
              FieldByName('nSeqEvento').AsInteger         := InfEvento.nSeqEvento;
              FieldByName('tpAmb').AsString               := MantertpAmb( InfEvento.tpAmb );
              FieldByName('dhEvento').AsDateTime          := InfEvento.dhEvento;
              FieldByName('TipoEvento').AsString          := InfEvento.TipoEvento;
              FieldByName('DescEvento').AsString          := InfEvento.DescEvento;
              FieldByName('versaoEvento').AsString        := InfEvento.versaoEvento;
              FieldByName('cStat').AsInteger              := RetInfEvento.cStat;
              FieldByName('xMotivo').AsString             := RetInfEvento.xMotivo;
              FieldByName('nProt').AsString               := RetInfEvento.nProt;
              FieldByName('dhRegEvento').AsDateTime       := RetInfEvento.dhRegEvento;
              FieldByName('xJust').AsString               := 'Protocolo do CTe Cancelado:' + InfEvento.detEvento.nProt +sLineBreak+InfEvento.detEvento.xJust;
              FieldByName('xCondUso').AsString            := InfEvento.detEvento.xCondUso;
              frxReport.Variables['HOMOLOGACAO']          := ( InfEvento.tpAmb = taHomologacao);
              Post;
            end;
          teCCe:
            begin
              TipoEvento := teCCe;
              for J := 0 to InfEvento.detEvento.infCorrecao.Count - 1 do
              begin
                Append;
                FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
                FieldByName('Modelo').AsString              := Copy(InfEvento.chCTe, 21, 2);
                FieldByName('Serie').AsString               := Copy(InfEvento.chCTe, 23, 3);
                FieldByName('Numero').AsString              := Copy(InfEvento.chCTe, 26, 9);
                FieldByName('MesAno').AsString              := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
                FieldByName('Barras').AsString              := InfEvento.chCTe;
                FieldByName('ChaveAcesso').AsString         := FormatarChaveAcesso(InfEvento.chCTe);
                FieldByName('cOrgao').AsInteger             := InfEvento.cOrgao;
                FieldByName('nSeqEvento').AsInteger         := InfEvento.nSeqEvento;
                FieldByName('tpAmb').AsString               := MantertpAmb( InfEvento.tpAmb );
                FieldByName('dhEvento').AsDateTime          := InfEvento.dhEvento;
                FieldByName('TipoEvento').AsString          := InfEvento.TipoEvento;
                FieldByName('DescEvento').AsString          := InfEvento.DescEvento;
                FieldByName('versaoEvento').AsString        := InfEvento.versaoEvento;
                FieldByName('cStat').AsInteger              := RetInfEvento.cStat;
                FieldByName('xMotivo').AsString             := RetInfEvento.xMotivo;
                FieldByName('nProt').AsString               := RetInfEvento.nProt;
                FieldByName('dhRegEvento').AsDateTime       := RetInfEvento.dhRegEvento;
                FieldByName('xJust').AsString               := InfEvento.detEvento.xJust;
                FieldByName('xCondUso').AsString            := InfEvento.detEvento.xCondUso;
                frxReport.Variables['HOMOLOGACAO']          := ( InfEvento.tpAmb = taHomologacao);

                with InfEvento.detEvento.infCorrecao.Items[J] do
                begin
                  FieldByName('grupoAlterado').AsString    := grupoAlterado;
                  FieldByName('campoAlterado').AsString    := campoAlterado;
                  FieldByName('valorAlterado').AsString    := valorAlterado;
                  FieldByName('nroItemAlterado').AsInteger := nroItemAlterado;
                end;

                Post;
              end;
            end;
          teComprEntrega:
            begin
              TipoEvento := teComprEntrega;
              Append;
              FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
              FieldByName('Modelo').AsString              := Copy(InfEvento.chCTe, 21, 2);
              FieldByName('Serie').AsString               := Copy(InfEvento.chCTe, 23, 3);
              FieldByName('Numero').AsString              := Copy(InfEvento.chCTe, 26, 9);
              FieldByName('MesAno').AsString              := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
              FieldByName('Barras').AsString              := InfEvento.chCTe;
              FieldByName('ChaveAcesso').AsString         := FormatarChaveAcesso(InfEvento.chCTe);
              FieldByName('cOrgao').AsInteger             := InfEvento.cOrgao;
              FieldByName('nSeqEvento').AsInteger         := InfEvento.nSeqEvento;
              FieldByName('tpAmb').AsString               := MantertpAmb(InfEvento.tpAmb);
              FieldByName('dhEvento').AsDateTime          := InfEvento.dhEvento;
              FieldByName('TipoEvento').AsString          := InfEvento.TipoEvento;
              FieldByName('DescEvento').AsString          := InfEvento.DescEvento;
              FieldByName('versaoEvento').AsString        := InfEvento.versaoEvento;
              FieldByName('cStat').AsInteger              := RetInfEvento.cStat;
              FieldByName('xMotivo').AsString             := RetInfEvento.xMotivo;
              FieldByName('nProt').AsString               := RetInfEvento.nProt;
              FieldByName('dhRegEvento').AsDateTime       := RetInfEvento.dhRegEvento;
              FieldByName('dhEntrega').AsDateTime         := InfEvento.detEvento.dhEntrega;
              FieldByName('nDoc').AsString                := InfEvento.detEvento.nDoc;
              FieldByName('xNome').AsString               := InfEvento.detEvento.xNome;
              FieldByName('latitude').AsFloat             := InfEvento.detEvento.latitude;
              FieldByName('longitude').AsFloat            := InfEvento.detEvento.longitude;
              frxReport.Variables['HOMOLOGACAO']          := (InfEvento.tpAmb = taHomologacao);
              Post;
            end;
          teCancComprEntrega:
            begin
              TipoEvento := teCancComprEntrega;
              Append;
              FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);
              FieldByName('Modelo').AsString              := Copy(InfEvento.chCTe, 21, 2);
              FieldByName('Serie').AsString               := Copy(InfEvento.chCTe, 23, 3);
              FieldByName('Numero').AsString              := Copy(InfEvento.chCTe, 26, 9);
              FieldByName('MesAno').AsString              := Copy(InfEvento.chCTe, 05, 2) + '/' + Copy(InfEvento.chCTe, 03, 2);
              FieldByName('Barras').AsString              := InfEvento.chCTe;
              FieldByName('ChaveAcesso').AsString         := FormatarChaveAcesso(InfEvento.chCTe);
              FieldByName('cOrgao').AsInteger             := InfEvento.cOrgao;
              FieldByName('nSeqEvento').AsInteger         := InfEvento.nSeqEvento;
              FieldByName('tpAmb').AsString               := MantertpAmb(InfEvento.tpAmb);
              FieldByName('dhEvento').AsDateTime          := InfEvento.dhEvento;
              FieldByName('TipoEvento').AsString          := InfEvento.TipoEvento;
              FieldByName('DescEvento').AsString          := InfEvento.DescEvento;
              FieldByName('versaoEvento').AsString        := InfEvento.versaoEvento;
              FieldByName('cStat').AsInteger              := RetInfEvento.cStat;
              FieldByName('xMotivo').AsString             := RetInfEvento.xMotivo;
              FieldByName('nProt').AsString               := RetInfEvento.nProt;
              FieldByName('dhRegEvento').AsDateTime       := RetInfEvento.dhRegEvento;
              FieldByName('nProtCE').AsString             := InfEvento.detEvento.nProtCE;
              frxReport.Variables['HOMOLOGACAO']          := (InfEvento.tpAmb = taHomologacao);
              Post;
            end;
        end;
      end;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaDadosInutilizacao;
begin

   with cdsInutilizacao do
   begin
      Close;
      FieldDefs.Clear;
      FieldDefs.Add('ID', ftString, 44);
      FieldDefs.Add('CNPJ', ftString, 20);
      FieldDefs.Add('nProt', ftString, 20);
      FieldDefs.Add('Modelo', ftInteger);
      FieldDefs.Add('Serie', ftInteger);
      FieldDefs.Add('Ano', ftInteger);
      FieldDefs.Add('nCTIni', ftInteger);
      FieldDefs.Add('nCTFin', ftInteger);
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
         FieldByName('ID').AsString         := OnlyNumber(ID);
         FieldByName('CNPJ').AsString       := FormatarCNPJ(CNPJ);
         FieldByName('nProt').AsString      := nProt;
         FieldByName('Modelo').AsInteger    := Modelo;
         FieldByName('Serie').AsInteger     := Serie;
         FieldByName('Ano').AsInteger       := Ano;
         FieldByName('nCTIni').AsInteger    := nCTIni;
         FieldByName('nCTFin').AsInteger    := nCTFin;
         FieldByName('xJust').AsString      := xJust;
         FieldByName('versao').AsString     := versao;
         FieldByName('verAplic').AsString   := verAplic;
         FieldByName('cStat').AsInteger     := cStat;
         FieldByName('xMotivo').AsString    := xMotivo;
         FieldByName('dhRecbto').AsDateTime := dhRecbto;
         FieldByName('cUF').AsString        := CUFtoUF(cUF);

         case tpAmb of
            taProducao:    FieldByName('tpAmb').AsString := ACBrStr('PRODUÇÃO');
            taHomologacao: FieldByName('tpAmb').AsString := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
         end;

         Post;
      end;
   end;
end;

procedure TACBrCTeDACTEFR.CarregaDadosNotasFiscais;
var
  i       : Integer;
  DoctoRem: string;
  NroNota : Integer;
begin
  { dados das Notas Fiscais }
  DoctoRem := FCTe.Rem.CNPJCPF;
  if Length(DoctoRem) > 11 then
    DoctoRem := FormatMaskText('##.###.###\/####-##;0;_', DoctoRem)
  else
    DoctoRem := FormatMaskText('###.###.###-##;0;_', DoctoRem);

  with cdsDadosNotasFiscais do
  begin

    for i := 0 to CTE.infCTeNorm.infDoc.infNF.Count - 1 do
    begin
      with FCTe.infCTeNorm.infDoc.infNF.Items[i] do
      begin
        Append;
        FieldByName('tpDoc').AsString       := 'NF';
        FieldByName('CNPJCPF').AsString     := DoctoRem;
        FieldByName('Serie').AsString       := serie;
        FieldByName('ChaveAcesso').AsString := '';
        FieldByName('NotaFiscal').AsString  := nDoc;
        FieldByName('TextoImpressao').AsString := 'NF              ' + DoctoRem + '                              ' +
          serie + '  /  ' + FormatFloat('00000000000000000000', StrToInt64(nDoc));
      end;
      Post;
    end;

    for i := 0 to CTE.infCTeNorm.infDoc.InfNFE.Count - 1 do
    begin
      with FCTe.infCTeNorm.infDoc.InfNFE.Items[i] do
      begin
        Append;
        FieldByName('tpDoc').AsString       := 'NFe';
        FieldByName('CNPJCPF').AsString     := FCTe.Rem.CNPJCPF;
        FieldByName('Serie').AsString       := Copy(chave, 23, 3);
        FieldByName('ChaveAcesso').AsString := chave;
        FieldByName('NotaFiscal').AsString  := Copy(chave, 26, 9);
        NroNota                             := StrToInt(Copy(chave, 26, 9));
        FieldByName('TextoImpressao').AsString := 'NF-e ' + FormatFloat('000000000', NroNota) + '      ' + chave;
      end;
      Post;
    end;

    for i := 0 to CTE.infCTeNorm.infDoc.infOutros.Count - 1 do
    begin
      with FCTe.infCTeNorm.infDoc.infOutros.Items[i] do
      begin
        Append;
        FieldByName('tpDoc').AsString       := 'Outros';
        FieldByName('CNPJCPF').AsString     := FCTe.Rem.CNPJCPF;
        FieldByName('Serie').AsString       := '';
        FieldByName('ChaveAcesso').AsString := '';
        FieldByName('NotaFiscal').AsString  := '';

        case tpDoc of
          tdCFeSAT    : FieldByName('TextoImpressao').AsString := 'CF-e SAT            ' + DoctoRem + '                                        ' + nDoc;
          tdNFCe      : FieldByName('TextoImpressao').AsString := 'NFC-e               ' + DoctoRem + '                                        ' + nDoc;
          tdDeclaracao: FieldByName('TextoImpressao').AsString := 'Declaração          ' + DoctoRem + '                                        ' + nDoc;
          tdOutros    : FieldByName('TextoImpressao').AsString := 'Outros              ' + DoctoRem + '                                        ' + nDoc;
          tdDutoviario: FieldByName('TextoImpressao').AsString := 'Dutoviário          ' + DoctoRem + '                                        ' + nDoc;
        else
          FieldByName('TextoImpressao').AsString := 'Não informado       ' + DoctoRem + '                                        ' + nDoc;
        end;
      end;
      Post;
    end;
  end;

  if cdsDadosNotasFiscais.IsEmpty
    and (CTE.ide.tpServ in [tsIntermediario, tsMultimodal]) then
  begin
    // inserir registro vazio caso CTe não possua documentos (redespacho intermediario ou vinculado a multimodal)
    cdsDadosNotasFiscais.Append;
    cdsDadosNotasFiscais.Post;
  end;

end;

procedure TACBrCTeDACTEFR.CarregaDestinatario;
begin
  { destinatário }
  with cdsDestinatario do
  begin

    Append;

    with FCTe.Dest do
    begin
      FieldByName('CNPJCPF').AsString := FormatarCNPJouCPF(CNPJCPF);
      FieldByName('XNome').AsString   := xNome;
      with EnderDest do
      begin
        FieldByName('XLgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := ManterCep( CEP );
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString   := IE;
      FieldByName('ISUF').AsString := ISUF;
    end;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaDocumentoAnterior;
var
  i, ii, iii: Integer;
begin
  with cdsDocAnterior do
  begin

    for i := 0 to CTE.infCTeNorm.docAnt.emiDocAnt.Count - 1 do
    begin
      with CTE.infCTeNorm.docAnt.emiDocAnt.Items[i] do
      begin
        for ii := 0 to idDocAnt.Count - 1 do
        begin
          for iii := 0 to idDocAnt.Items[ii].idDocAntPap.Count - 1 do
          begin
            with idDocAnt.Items[ii].idDocAntPap.Items[iii] do
            begin
              Append;
              FieldByName('CNPJCPF').AsString := CNPJCPF;
              FieldByName('IE').AsString      := IE;
              FieldByName('xNome').AsString   := xNome;
              FieldByName('UF').AsString      := UF;
              case tpDoc of
                daCTRC  : FieldByName('Tipo').AsString  := 'CTRC';
                daCTAC  : FieldByName('Tipo').AsString  := 'CTAC';
                daACT   : FieldByName('Tipo').AsString  := 'ACT';
                daNF7   : FieldByName('Tipo').AsString  := 'NF 7';
                daNF27  : FieldByName('Tipo').AsString  := 'NF 27';
                daCAN   : FieldByName('Tipo').AsString  := 'CAN';
                daCTMC  : FieldByName('Tipo').AsString  := 'CTMC';
                daATRE  : FieldByName('Tipo').AsString  := 'ATRE';
                daDTA   : FieldByName('Tipo').AsString  := 'DTA';
                daCAI   : FieldByName('Tipo').AsString  := 'CAI';
                daCCPI  : FieldByName('Tipo').AsString  := 'CCPI';
                daCA    : FieldByName('Tipo').AsString  := 'CA';
                daTIF   : FieldByName('Tipo').AsString  := 'TIF';
                daOutros: FieldByName('Tipo').AsString  := 'OUTROS';
                daBL    : FieldByName('Tipo').AsString  := 'BL';
              end;
              FieldByName('Serie').AsString := idDocAnt.Items[ii].idDocAntPap.Items[iii].serie;
              FieldByName('nDoc').AsString  := idDocAnt.Items[ii].idDocAntPap.Items[iii].nDoc;
              FieldByName('dEmi').AsString  := FormatDateTime('dd/mm/yyyy', idDocAnt.Items[ii].idDocAntPap.Items[iii].dEmi);
            end;
            Post;
          end;
          for iii := 0 to idDocAnt.Items[ii].idDocAntEle.Count - 1 do
          begin
            Append;
            FieldByName('CNPJCPF').AsString := CNPJCPF;
            FieldByName('IE').AsString      := IE;
            FieldByName('xNome').AsString   := xNome;
            FieldByName('UF').AsString      := UF;
            with idDocAnt.Items[ii].idDocAntEle.Items[iii] do
            begin
              FieldByName('Tipo').AsString  := 'CT-e';

              if FCTe.infCTe.versao >= 3 then
              begin
                FieldByName('Chave').AsString := chCTe;
                FieldByName('Serie').AsString := Copy(chCTe, 23, 3);
                FieldByName('nDoc').AsString  := Copy(chCTe, 26, 9);
                FieldByName('dEmi').AsString  := Copy(chCTe, 5, 2) + '/' + Copy(chCTe, 3, 2);
              end
              else
              begin
                FieldByName('Chave').AsString := chave;
                FieldByName('Serie').AsString := Copy(chave, 23, 3);
                FieldByName('nDoc').AsString  := Copy(chave, 26, 9);
                FieldByName('dEmi').AsString  := Copy(chave, 5, 2) + '/' + Copy(chave, 3, 2);
              end;
            end;
            Post;
          end;
        end;
      end;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaEmitente;
begin
  { emitente }
  with cdsEmitente do
  begin
		Append;
		with FCTe.Emit do
		begin
			FieldByName('CNPJ').AsString  := FormatarCNPJouCPF(CNPJ);
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
		  	FieldByName('CEP').AsString     := ManterCep( CEP );
		  	FieldByName('Fone').AsString    := FormatarFone(Fone);
			end;
			FieldByName('IE').AsString := IE;
		end;

  	Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaExpedidor;
begin
  { Expedidor }
  with cdsExpedidor do
  begin
    Append;
    with FCTe.Exped do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJouCPF(CNPJCPF);
      FieldByName('XNome').AsString := xNome;
      with EnderExped do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := ManterCep( CEP );
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaIdentificacao;
begin
  with cdsIdentificacao do
  begin

    Append;
    with FCTe.infCTe do
    begin
      FieldByName('Id').AsString    := OnlyNumber(Id);
      FieldByName('Chave').AsString := FormatarChaveAcesso(Id);
    end;

    with FCTe.ide do
    begin
      FieldByName('CUF').AsString   := IntToStr(CUF);
      FieldByName('CCT').AsString   := IntToStr(CCT);
      FieldByName('CFOP').AsString  := IntToStr(CFOP);
      FieldByName('NatOp').AsString := NatOp;
      FieldByName('URL').AsString := TACBrCTe(ACBrCTE).GetURLConsulta(cUF, tpAmb, FCTe.infCTe.versao);

      case forPag of
        fpPago  : FieldByName('forPag').AsString  := 'Pago';
        fpAPagar: FieldByName('forPag').AsString  := 'A Pagar';
        fpOutros: FieldByName('forPag').AsString  := 'Outros';
      end;

      if indGlobalizado = tiSim then
        begin
          FieldByName('indGlobalizado').AsString  := 'Sim';
          if (Trim(FCTe.infCTeNorm.infGlobalizado.xObs) <> '') then
             FieldByName('ObsGlobalizado').AsString  := FCTe.infCTeNorm.infGlobalizado.xObs;
        end
      else
        FieldByName('indGlobalizado').AsString  := 'Não';

      FieldByName('Mod_').AsString    := IntToStr(modelo);
      FieldByName('Serie').AsString   := IntToStr(serie);
      FieldByName('NCT').AsString     := FormatarNumeroDocumentoFiscal(IntToStr(nCT));
      FieldByName('dhEmi').AsDateTime := dhEmi;

      case tpCTe of
        tcNormal: FieldByName('TpCT').AsString      := 'Normal';
        tcComplemento: FieldByName('TpCT').AsString := 'Complemento';
        tcAnulacao: FieldByName('TpCT').AsString    := 'Anulação';
        tcSubstituto: FieldByName('TpCT').AsString  := 'Substituto';
      end;

      FieldByName('cMunEmi').AsString := IntToStr(cMunEnv);
      FieldByName('xMunEmi').AsString := xMunEnv;
      FieldByName('UFEmi').AsString   := UFEnv;
      FieldByName('modal').AsString := TpModalToStr(CTe.ide.modal);

      case tpServ of
        tsNormal: FieldByName('tpServ').AsString         := 'Normal';
        tsSubcontratacao: FieldByName('tpServ').AsString := 'Subcontratação';
        tsRedespacho: FieldByName('tpServ').AsString     := 'Redespacho';
        tsIntermediario: FieldByName('tpServ').AsString  := 'Intermediário';
        tsMultimodal: FieldByName('tpServ').AsString  := 'Vinc. a Multimodal';
        tsTranspPessoas: FieldByName('tpServ').AsString  := 'Transporte de Pessoas';
        tsTranspValores: FieldByName('tpServ').AsString  := 'Transporte de Valores';
        tsExcessoBagagem: FieldByName('tpServ').AsString  := 'Excesso de Bagagem';
      end;

      FieldByName('cMunIni').AsString := IntToStr(cMunIni);
      FieldByName('xMunIni').AsString := xMunIni;
      FieldByName('UFIni').AsString   := UFIni;
      FieldByName('cMunFim').AsString := IntToStr(cMunFim);
      FieldByName('xMunFim').AsString := xMunFim;
      FieldByName('UFFim').AsString   := UFFim;
      FieldByName('TpImp').AsString   := IfThen(TpImp = tiRetrato, '1', '2');
      FieldByName('TpEmis').AsString  := IfThen(TpEmis = teNormal, '1', '5');
      FieldByName('CDV').AsString     := IntToStr(CDV);
      FieldByName('TpAmb').AsString   := IfThen(tpAmb = taHomologacao, '2', '1');
      FieldByName('ProcEmi').AsString := IfThen(ProcEmi = peAplicativoContribuinte, '0', '');
      FieldByName('VerProc').AsString := VerProc;

      case Toma03.Toma of
        tmRemetente: FieldByName('Toma').AsString    := 'Remetente';
        tmDestinatario: FieldByName('Toma').AsString := 'Destinatário';
        tmExpedidor: FieldByName('Toma').AsString    := 'Expedidor';
        tmRecebedor: FieldByName('Toma').AsString    := 'Recebedor';
      end;

      case Toma4.Toma of
        tmOutros: FieldByName('Toma').AsString := 'Outros';
      end;
      FieldByName('refCTE').AsString := refCTe;
    end;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaInformacoesAdicionais;
var
  vTemp        : TStringList;
  IndexCampo   : Integer;
  Campos       : TSplitResult;
  BufferObs    : string;
  TmpStr       : string;
  wContingencia: string;
  wObs         : string;
  i            : Integer;
begin
  with cdsInformacoesAdicionais do
  begin
    Append;
    with FCTe.compl do
    begin
      wObs := xObs;

      // Contingencia
      wContingencia := '';
      if (FCTe.ide.TpEmis = teContingencia) or (FCTe.ide.TpEmis = teFSDA) or (FCTe.ide.TpEmis = teSCAN) then
        wContingencia := 'DACTE EM CONTINGÊNCIA, IMPRESSO EM DECORRÊNCIA DE PROBLEMAS TÉCNICOS'
      else if FCTe.ide.TpEmis = teDPEC then
        wContingencia := 'DACTE IMPRESSO EM CONTINGÊNCIA - DPEC REGULARMENTE RECEBIDA PELA RECEITA FEDERAL DO BRASIL';
      if wContingencia <> '' then
      begin
        if Length(wObs) > 0 then
          wObs := wObs + ';';
        wObs   := wObs + wContingencia;
      end;

      vTemp := TStringList.Create;
      try
        if Trim(wObs) <> '' then
        begin
          Campos         := nil;
          Campos         := Split(';', wObs);
          for IndexCampo := 0 to Length(Campos) - 1 do
            vTemp.Add(Campos[IndexCampo]);

          TmpStr    := vTemp.Text;
          BufferObs := TmpStr;
        end
        else
          BufferObs := '';

      finally
        vTemp.Free;
      end;

      FieldByName('Fluxo_xOrig').AsString := fluxo.xOrig;
      FieldByName('Fluxo_xDest').AsString := fluxo.xDest;
      FieldByName('Fluxo_xRota').AsString := fluxo.xRota;

      with Entrega do
      begin
        case TipoData of
          tdSemData, tdNaoInformado:
          begin
            FieldByName('Entrega_tpPer').AsString := '';
            FieldByName('Entrega_dProg').AsString := '';
            FieldByName('Entrega_dIni').AsString  := '';
            FieldByName('Entrega_dFim').AsString  := '';
          end;

          tdNaData, tdAteData, tdApartirData:
          begin
            FieldByName('Entrega_tpPer').AsString := IfThen(TipoData = tdNaData, 'NA DATA', IfThen(TipoData = tdAteData, 'ATE A DATA', 'A PARTIR DE'));
            FieldByName('Entrega_dProg').AsString := FormatDateTime('dd/mm/yyyy', comData.dProg);
            FieldByName('Entrega_dIni').AsString  := '';
            FieldByName('Entrega_dFim').AsString  := '';
          end;

          tdNoPeriodo:
          begin
            FieldByName('Entrega_tpPer').AsString := 'NO PERIODO';
            FieldByName('Entrega_dProg').AsString := '';
            FieldByName('Entrega_dIni').AsString  := FormatDateTime('dd/mm/yyyy', noPeriodo.dIni);
            FieldByName('Entrega_dFim').AsString  := FormatDateTime('dd/mm/yyyy', noPeriodo.dFim);
          end;
        end;
      end;
    end;
    FieldByName('OBS').AsString := BufferObs;

    BufferObs := '';
    if Trim(FCTe.Imp.infAdFisco) <> '' then
    begin
      wObs  := FCTe.Imp.infAdFisco;
      vTemp := TStringList.Create;
      try
        if Trim(wObs) <> '' then
        begin
          Campos         := nil;
          Campos         := Split(';', wObs);
          for IndexCampo := 0 to Length(Campos) - 1 do
            vTemp.Add(Campos[IndexCampo]);

          TmpStr    := vTemp.Text;
          BufferObs := TmpStr;
        end
        else
          BufferObs := '';

      finally
        vTemp.Free;
      end;
    end;
    FieldByName('infAdFisco').AsString := BufferObs;

    BufferObs := '';
    if FCTe.compl.ObsCont.Count > 0 then
    begin
      wObs   := '';
      for i  := 0 to FCTe.compl.ObsCont.Count - 1 do
        wObs := wObs + FCTe.compl.ObsCont[i].xCampo + ' : ' + FCTe.compl.ObsCont[i].xTexto + ';';

      vTemp := TStringList.Create;
      try
        if Trim(wObs) <> '' then
        begin
          Campos         := nil;
          Campos := Split(';', wObs);
          for IndexCampo := 0 to Length(Campos) - 1 do
            vTemp.Add(Campos[IndexCampo]);

          TmpStr    := vTemp.Text;
          BufferObs := TmpStr;
        end
        else
          BufferObs := '';

      finally
        vTemp.Free;
      end;

    end;
    FieldByName('ObsCont').AsString := BufferObs;
    Post;
  end;

end;

procedure TACBrCTeDACTEFR.CarregaInfServico;
begin
  { Informações do Serviço }
  with cdsInfServico, FCTe.infCTeNorm.infServico do
  begin
    Append;
    FieldByName('xDescServ').AsString := xDescServ;
    FieldByName('qCarga').AsFloat := qCarga;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaInfTribFed;
begin
  with cdsInfTribFed, FCTe.imp.infTribFed do
  begin
    Append;
    FieldByName('vPIS').AsFloat := vPIS;
    FieldByName('vCOFINS').AsFloat := vCOFINS;
    FieldByName('vIR').AsFloat := vIR;
    FieldByName('vINSS').AsFloat := vINSS;
    FieldByName('vCSLL').AsFloat := vCSLL;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaModalAereo;
var
   i : Integer;
begin
  if FCTe.ide.modal <> mdAereo then
    Exit;

  with cdsModalAereo, CTE.infCTeNorm do
  begin
    Append;
    FieldByName('nMinu').AsInteger := aereo.nMinu;
    FieldByName('nOCA').AsString := aereo.nOCA;
    FieldByName('dPrevAereo').AsDateTime := aereo.dPrevAereo;
    FieldByName('xLAgEmi').AsString := aereo.xLAgEmi;
    FieldByName('IdT').AsString := aereo.IdT;
    FieldByName('CL').AsString := aereo.tarifa.CL;
    FieldByName('cTar').AsString := aereo.tarifa.cTar;
    FieldByName('vTar').AsCurrency := aereo.tarifa.vTar;
    FieldByName('xDime').AsString := aereo.natCarga.xDime;

    for i := 0 to CTe.infCTeNorm.aereo.natCarga.cinfManu.Count - 1 do
    begin
      if (i > 0) then
         FieldByName('cInfManu').AsString := FieldByName('cInfManu').AsString + ', ';

      if FCTe.infCTe.versao >= 3 then
         FieldByName('cInfManu').AsString := FieldByName('cInfManu').AsString + TpInfManuToStr(CTe.infCTeNorm.aereo.natCarga.cinfManu.Items[i].nInfManu)
      else
         FieldByName('cInfManu').AsString := FieldByName('cInfManu').AsString + TpInfManuToStrV2(CTe.infCTeNorm.aereo.natCarga.cinfManu.Items[i].nInfManu);
    end;
    if (FieldByName('cInfManu').AsString <> '') then
      FieldByName('cInfManu').AsString := FieldByName('cInfManu').AsString + '.';

    FieldByName('cIMP').AsString := aereo.natCarga.cIMP;
    FieldByName('xOrig').AsString := CTe.compl.fluxo.xOrig;
    FieldByName('xDest').AsString := CTe.compl.fluxo.xDest;
    FieldByName('xRota').AsString := CTe.compl.fluxo.xRota;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaMultiModal;
begin
  if FCTe.ide.modal <> mdMultimodal then
    Exit;

  with cdsMultiModal, CTE.infCTeNorm do
  begin
    Append;
    FieldByName('COTM').AsString := multimodal.COTM;
    FieldByName('indNegociavel').AsString := indNegociavelToStr(multimodal.indNegociavel);
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaModalAquaviario;
var
   i: Integer;
   xBalsa: String;
begin
  if FCTe.ide.modal <> mdAquaviario then
    Exit;

  with cdsModalAquaviario, CTe.infCTeNorm.aquav do
  begin
    Append;

    FieldByName('vPrest').AsFloat    := vPrest;
    FieldByName('vAFRMM').AsFloat    := vAFRMM;
    FieldByName('nBooking').AsString := nBooking;
    FieldByName('nCtrl').AsString    := nCtrl;
    FieldByName('xNavio').AsString   := xNavio;
    FieldByName('nViag').AsString    := nViag;

    case direc of
      drNorte: FieldByName('direc').AsString := 'NORTE';
      drLeste: FieldByName('direc').AsString := 'LESTE';
      drSul:   FieldByName('direc').AsString := 'SUL';
      drOeste: FieldByName('direc').AsString := 'OESTE';
    end;

    FieldByName('prtEmb').AsString := prtEmb;
    FieldByName('prtTrans').AsString := prtTrans;
    FieldByName('prtDest').AsString := prtDest;

    case tpNav of
      tnInterior:  FieldByName('tpNav').AsString := 'INTERIOR';
      tnCabotagem: FieldByName('tpNav').AsString := 'CABOTAGEM';
    end;

    FieldByName('irin').AsString := irin;
    FieldByName('xNavio').AsString := FieldByName('xNavio').AsString;

    for i := 0 to balsa.Count-1 do
      xBalsa := xBalsa + balsa.Items[i].xBalsa +',';

    FieldByName('xBalsa').AsString := Copy(xBalsa,1,Length(xBalsa)-1);

    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaModalRodoviario;
var
  i      : Integer;
  Child  : TfrxChild;
begin
  if FCTe.ide.modal <> mdRodoviario then
    Exit;

  if FCTe.ide.modelo = 67 then  //67-CTeOS
  begin
    with cdsRodoVeiculos do
    begin
      Append;
      with CTE.infCTeNorm.rodoOS.veic do
      begin
        FieldByName('placa').AsString := placa;
        FieldByName('RENAVAM').AsString := RENAVAM;
        if (Length(Prop.TAF) > 0) or (Length(Prop.NroRegEstadual) > 0) then
        begin
          //Terceiro
          FieldByName('UF').AsString := Prop.UF;
          FieldByName('TAF').AsString := Prop.TAF;
          FieldByName('NroRegEstadual').AsString := prop.NroRegEstadual;
          FieldByName('CPF/CNPJ').AsString := FormatarCNPJouCPF(prop.CNPJCPF);
        end
        else
        begin
          //Próprio
          FieldByName('UF').AsString  := CTe.infCTeNorm.rodoOS.veic.UF;
          FieldByName('TAF').AsString := CTe.infCTeNorm.rodoOS.TAF;
          FieldByName('NroRegEstadual').AsString := CTe.infCTeNorm.rodoOS.NroRegEstadual;
          FieldByName('CPF/CNPJ').AsString := FormatarCNPJouCPF(CTe.Emit.CNPJ);
        end;
      end;
      post;
    end;
    Exit;
  end;


  with cdsModalRodoviario do
  begin
    Append;
    //
    case CTE.infCTeNorm.rodo.lota of
      ltNao: FieldByName('LOTACAO').AsString := 'Não';
      ltSim: FieldByName('LOTACAO').AsString := 'Sim';
    end;

    with CTE.infCTeNorm.rodo do
    begin
      FieldByName('RNTRC').AsString := RNTRC;

      if dPrev > 0 then
         FieldByName('DATAPREVISTA').AsString := DateToStr(dPrev)
	  else
      begin
        if (CTE.compl.Entrega.comData.dProg > 0) then
          FieldByName('DATAPREVISTA').AsString := DateToStr(CTE.compl.Entrega.comData.dProg);
      end;
	  
      FieldByName('CIOT').AsString := CIOT;
    end;

    for i := 0 to CTE.infCTeNorm.rodo.lacRodo.Count - 1 do
    begin
      with CTE.infCTeNorm.rodo.lacRodo.Items[i] do
      begin
        if Trim(FieldByName('LACRES').AsString) <> '' then
           FieldByName('LACRES').AsString := FieldByName('LACRES').AsString + '/';
        FieldByName('LACRES').AsString   := FieldByName('LACRES').AsString + nLacre;
      end;
    end;

    Post;
  end;
  Child := frxReport.FindObject('ChildRodoLotacao') as TfrxChild;

  with cdsRodoVeiculos do
  begin

    for i := 0 to CTE.infCTeNorm.rodo.veic.Count - 1 do
    begin
      with CTE.infCTeNorm.rodo.veic.Items[i] do
      begin
        Append;
        case tpVeic of
          tvTracao: FieldByName('tpVeic').AsString  := 'Tração';
          tvReboque: FieldByName('tpVeic').AsString := 'Reboque';
        end;
        FieldByName('placa').AsString := placa;
        FieldByName('UF').AsString    := UF;
        if tpProp = tpProprio then
           FieldByName('RNTRC').AsString := CTe.infCTeNorm.rodo.RNTRC
        else
           FieldByName('RNTRC').AsString := Prop.RNTRC;
        Post;
      end;
    end;
  end;

  with cdsRodoValePedagio do
  begin
    for i := 0 to CTE.infCTeNorm.rodo.valePed.Count - 1 do
    begin
      Append;
      FieldByName('CNPJForn').AsString := FormatarCNPJouCPF(CTE.infCTeNorm.rodo.valePed.Items[i].CNPJForn);
      FieldByName('CNPJPg').AsString   := FormatarCNPJouCPF(CTE.infCTeNorm.rodo.valePed.Items[i].CNPJPg);
      FieldByName('nCompra').AsString  := CTE.infCTeNorm.rodo.valePed.Items[i].nCompra;
      FieldByName('vValePed').AsFloat := CTe.infCTeNorm.rodo.valePed.Items[i].vValePed;
      Post;
    end;
  end;

  with cdsRodoMotorista do
  begin
    for i := 0 to CTE.infCTeNorm.rodo.moto.Count - 1 do
    begin
      with CTE.infCTeNorm.rodo.moto.Items[i] do
      begin
        Append;
        FieldByName('xNome').AsString := xNome;
        FieldByName('CPF').AsString   := CPF;
        Post;
      end;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaParametros;
var
  vChave_Contingencia: string;
  vResumo            : string;
  vStream            : TMemoryStream;
  vStringStream      : TStringStream;
begin
  { parâmetros }
  with cdsParametros do
  begin

    Append;

    vResumo := '';
    if DACTEClassOwner.ExibeResumoCanhoto then
    begin
      vResumo := 'EMIT: '+ FCTe.Emit.xNome + ' - ' +
                 'EMISSÃO: ' + FormatDateTime('DD/MM/YYYY',FCTe.Ide.dhEmi) + '  - '+
                 'TOMADOR: ';
      if FCTe.Ide.modelo = 67 then
        vResumo := vResumo + FCTe.toma.xNome
      else
      begin
        if FCTe.Ide.Toma4.xNome = '' then
        begin
          case FCTe.Ide.Toma03.Toma of
            tmRemetente:    vResumo := vResumo + FCTe.Rem.xNome;
            tmExpedidor:    vResumo := vResumo + FCTe.Exped.xNome;
            tmRecebedor:    vResumo := vResumo + FCTe.Receb.xNome;
            tmDestinatario: vResumo := vResumo + FCTe.Dest.xNome;
          end
        end
        else
          vResumo := vResumo + FCTe.Ide.Toma4.xNome;
      end;

      vResumo := vResumo + ' - VALOR A RECEBER: R$ ' + FormatFloat(',0.00',FCTe.vPrest.vRec);
    end;
    FieldByName('ResumoCanhoto').AsString := vResumo;

    if DACTEClassOwner.PosCanhoto = prCabecalho then
      FieldByName('PrintCanhoto').AsString := '0'
    else
	if DACTEClassOwner.PosCanhoto = prRodape then
      FieldByName('PrintCanhoto').AsString := '1'
	else
	if DACTEClassOwner.PosCanhoto = prEsquerda then
	  FieldByName('PrintCanhoto').AsString := '2';
    if FCTe.infCTe.versao = 2 then
      FieldByName('Versao').AsString := '2.00'
    else
      FieldByName('Versao').AsString := '3.00';
    if (FCTe.ide.tpAmb = taHomologacao) then
    begin
      if FCTe.Ide.modelo = 67 then
        FieldByName('Mensagem0').AsString := 'CT-e OS sem Valor Fiscal' + sLineBreak + 'HOMOLOGAÇÃO'
      else
        FieldByName('Mensagem0').AsString := 'CT-e sem Valor Fiscal' + sLineBreak + 'HOMOLOGAÇÃO';
    end
    else
    begin
      if not(FCTe.ide.TpEmis in [teContingencia, teFSDA]) then
      begin
        if ((EstaVazio(Protocolo)) and (EstaVazio(FCTe.procCTe.nProt))) then
        begin
          if FCTe.Ide.modelo = 67 then
            FieldByName('Mensagem0').AsString := 'CT-e OS sem Autorização' + sLineBreak + 'de Uso da SEFAZ'
          else
            FieldByName('Mensagem0').AsString := 'CT-e sem Autorização' + sLineBreak + 'de Uso da SEFAZ';
        end
        else
          if (not((EstaVazio(Protocolo)) and
          (EstaVazio(FCTe.procCTe.nProt)))) and
          (FCTe.procCTe.cStat = 101) then
          begin
             if FCTe.Ide.modelo = 67 then
               FieldByName('Mensagem0').AsString := 'CT-e OS Cancelado'
             else
               FieldByName('Mensagem0').AsString := 'CT-e Cancelado';
          end
        else
        begin
          if Cancelada then
          begin
            if FCTe.Ide.modelo = 67 then
              FieldByName('Mensagem0').AsString := 'CT-e OS Cancelado'
            else
              FieldByName('Mensagem0').AsString := 'CT-e Cancelado';
          end
          else
            FieldByName('Mensagem0').AsString := '';
        end;
      end
      else
        FieldByName('Mensagem0').AsString := '';
    end;

    // Carregamento da imagem
    if DACTEClassOwner.Logo <> '' then
    begin
      FieldByName('Imagem').AsString := DACTEClassOwner.Logo;
      vStream                        := TMemoryStream.Create;
      try
        if FileExists(DACTEClassOwner.Logo) then
          vStream.LoadFromFile(DACTEClassOwner.Logo)
        else
        begin
          vStringStream := TStringStream.Create(DACTEClassOwner.Logo);
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

    FieldByName('Sistema').AsString := Ifthen(Sistema <> '', Sistema, 'Projeto ACBr - http://acbr.sf.net');
    FieldByName('Usuario').AsString := Ifthen(Usuario <> '', Usuario,'');

    FieldByName('Site').AsString  := Site;
    FieldByName('Email').AsString := Email;

    if ImprimeDescPorc then
      FieldByName('Desconto').AsString := 'DESC %'
    else
      FieldByName('Desconto').AsString := 'V.DESC.';

    if ((FCTe.ide.TpEmis = teNormal) or (FCTe.ide.TpEmis = teSCAN)) or
       (FCTe.procCTe.cStat in [100, 101, 110]) then
    begin
      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
      FieldByName('Contingencia_ID').AsString       := '';

      if ((Cancelada) or (FCTe.procCTe.cStat = 101)) then
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO'
      else if FCTe.procCTe.cStat = 110 then
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE DENEGAÇÃO DE USO'
      else
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

      if EstaVazio(Protocolo) then
      begin
        if not(FCTe.ide.TpEmis in [teContingencia, teFSDA]) and EstaVazio(FCTe.procCTe.nProt) then
        begin
          if FCTe.Ide.modelo = 67 then
            FieldByName('Contingencia_Valor').AsString := 'CT-e OS sem Autorização de Uso da SEFAZ'
          else
            FieldByName('Contingencia_Valor').AsString := 'CT-e sem Autorização de Uso da SEFAZ';
        end
        else
          FieldByName('Contingencia_Valor').AsString := FCTe.procCTe.nProt + ' ' + IfThen(FCTe.procCTe.dhRecbto <> 0,
            DateTimeToStr(FCTe.procCTe.dhRecbto), '');
      end
      else
        FieldByName('Contingencia_Valor').AsString := Protocolo;
    end
    else
    begin
      vChave_Contingencia                           := TACBrCTe(DACTEClassOwner.ACBrCTe).GerarChaveContingencia(FCTe);
      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
      FieldByName('Contingencia_ID').AsString       := vChave_Contingencia;

      case FCTe.ide.TpEmis of
        teContingencia,
        teFSDA:
          begin
            if FCTe.Ide.modelo = 67 then
              FieldByName('Contingencia_Descricao').AsString := 'DADOS DO CT-E OS'
            else
              FieldByName('Contingencia_Descricao').AsString := 'DADOS DO CT-E';

            FieldByName('Contingencia_Valor').AsString := FormatarChaveAcesso(vChave_Contingencia);
          end;

        teDPEC:
          begin
            if NaoEstaVazio(FCTe.procCTe.nProt) then // EPEC TRANSMITIDO
            begin
              FieldByName('Contingencia_Descricao').AsString := ACBrStr( 'PROTOCOLO DE AUTORIZAÇÃO DE USO');
              FieldByName('Contingencia_Valor').AsString     := FCTe.procCTe.nProt + ' ' +
                IfThen(FCTe.procCTe.dhRecbto <> 0, DateTimeToStr(FCTe.procCTe.dhRecbto), '');
            end
            else
            begin
              FieldByName('Contingencia_Descricao').AsString := ACBrStr('NÚMERO DE REGISTRO EPEC');
              if NaoEstaVazio(Protocolo) then
                FieldByName('Contingencia_Valor').AsString := Protocolo;
            end;
          end;

        teSVCSP,
        teSVCRS:
          begin
            FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';
            FieldByName('Contingencia_Valor').AsString     := FCTe.procCTe.nProt + ' ' +
              IfThen(FCTe.procCTe.dhRecbto <> 0, DateTimeToStr(FCTe.procCTe.dhRecbto), '');
          end;
      end;
    end;

    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaPercurso;
var
  i : Integer;
  UfsPercurso : string;
begin
 { Percurso }
  with cdsPercurso do
  begin
    if FCTe.ide.infPercurso.Count > 0 then
    begin
      for I := 0 to FCTe.ide.infPercurso.Count -1 do
      begin
        if i = 0 then
          UfsPercurso := FCTe.ide.infPercurso.Items[i].UFPer
        else
          UfsPercurso := UfsPercurso +' - '+ FCTe.ide.infPercurso.Items[i].UFPer;
      end;
      Append;
      FieldByName('UFsPer').AsString := UfsPercurso;
      Post;
    end;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaProdutosPerigosos;
var i : Integer;
begin
 { ProdutosPerigosos }
  with cdsProdutosPerigosos do
  begin

		with FCTe.infCTeNorm.peri do
		begin
			if FCTe.infCTeNorm.peri.Count > 0 then
				for I := 0 to FCTe.infCTeNorm.peri.Count - 1 do
				begin
		  		Append;
		  		FieldByName('nONU').AsString          := FCTe.infCTeNorm.peri.Items[i].nONU;
		  		FieldByName('xNomeAE').AsString       := Items[i].xNomeAE;
		  		FieldByName('xClaRisco').AsString     := Items[i].xClaRisco;
		  		FieldByName('grEmb').AsString         := Items[i].grEmb;
		  		FieldByName('qTotProd').AsString      := Items[i].qTotProd;
		  		FieldByName('qVolTipo').AsString      := Items[i].qVolTipo;
		  		FieldByName('pontoFulgor').AsString   := Items[i].pontoFulgor;
		  		Post;
				end;
		end;
  end;
end;
procedure TACBrCTeDACTEFR.CarregaVeiculosNovos;
var i : Integer;
begin
 { VeiculosNovos }
  with cdsVeiculosNovos do
  begin

		with FCTe.infCTeNorm.veicNovos do
		begin
			if FCTe.infCTeNorm.veicNovos.Count > 0 then
			begin
				for I := 0 to FCTe.infCTeNorm.veicNovos.Count - 1 do
				begin
		  		    Append;
		  		    FieldByName('Chassi').AsString          := FCTe.infCTeNorm.veicNovos.Items[i].chassi;
		  		    FieldByName('cCor').AsString       := Items[i].cCor;
		  		    FieldByName('xCor').AsString     := Items[i].xCor;
		  		    FieldByName('cMod').AsString         := Items[i].cMod;
		  		    FieldByName('vUnit').AsFloat      := Items[i].vUnit;
		  		    FieldByName('vFrete').AsFloat      := Items[i].vFrete;
		  		    Post;
				end;
			end;
		end;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaRecebedor;
begin
  { Recebedor }
  with cdsRecebedor do
  begin

    Append;

    with FCTe.Receb do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJouCPF(CNPJCPF);
      FieldByName('XNome').AsString := xNome;
      with EnderReceb do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := ManterCep( CEP );
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;

    Post;
  end;

end;

procedure TACBrCTeDACTEFR.CarregaRemetente;
begin
  { Remetente }
  with cdsRemetente do
  begin

    Append;
    with FCTe.Rem do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJouCPF(CNPJCPF);
      FieldByName('XNome').AsString := xNome;
      FieldByName('XFant').AsString := XFant;
      with EnderReme do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := ManterCep( CEP );
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
    end;
    Post;
  end;
end;

procedure TACBrCTeDACTEFR.CarregaSeguro;
var
  i: Integer;
begin
  with cdsSeguro do
  begin
    if CTE.infCTeNorm.seg.Count > 0 then
    begin
      for i := 0 to CTE.infCTeNorm.seg.Count - 1 do
      begin
        with CTE.infCTeNorm.seg.Items[i] do
        begin
          Append;
          case respSeg of
            rsRemetente     : FieldByName('RESPONSAVEL').AsString := 'Remetente';
            rsExpedidor     : FieldByName('RESPONSAVEL').AsString := 'Expedidor';
            rsRecebedor     : FieldByName('RESPONSAVEL').AsString := 'Recebedor';
            rsDestinatario  : FieldByName('RESPONSAVEL').AsString := 'Destinatário';
            rsEmitenteCTe   : FieldByName('RESPONSAVEL').AsString := 'Emitente';
            rsTomadorServico: FieldByName('RESPONSAVEL').AsString := 'Tomador';
          end;
          FieldByName('NOMESEGURADORA').AsString  := xSeg;
          FieldByName('NUMEROAPOLICE').AsString   := nApol;
          FieldByName('NUMEROAVERBACAO').AsString := nAver;
          Post;
        end;
      end;
    end
    else
    begin
      Append;
      FieldByName('RESPONSAVEL').AsString     := '';
      FieldByName('NOMESEGURADORA').AsString  := '';
      FieldByName('NUMEROAPOLICE').AsString   := '';
      FieldByName('NUMEROAVERBACAO').AsString := '';
      Post;
    end;

  end;

end;

procedure TACBrCTeDACTEFR.CarregaTomador;
begin
  { Tomador Outros }
  with cdsTomador do
  begin

    Append;
    if FCTe.ide.modelo = 67 then  //67-CTeOS
    begin
      FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(FCTe.toma.CNPJCPF);
      FieldByName('XNome').AsString   := FCTe.toma.xNome;
      FieldByName('XFant').AsString   := FCTe.toma.XFant;
      FieldByName('IE').AsString      := FCTe.toma.IE;
      FieldByName('Xlgr').AsString    := FCTe.toma.enderToma.XLgr;
      FieldByName('Nro').AsString     := FCTe.toma.enderToma.Nro;
      FieldByName('XCpl').AsString    := FCTe.toma.enderToma.XCpl;
      FieldByName('XBairro').AsString := FCTe.toma.enderToma.XBairro;
      FieldByName('CMun').AsString    := IntToStr(FCTe.toma.enderToma.CMun);
      FieldByName('XMun').AsString    := FCTe.toma.enderToma.XMun;
      FieldByName('UF').AsString      := FCTe.toma.enderToma.UF;
      FieldByName('CEP').AsString     := ManterCep( FCTe.toma.enderToma.CEP );
      FieldByName('CPais').AsString   := IntToStr(FCTe.toma.enderToma.CPais);
      FieldByName('XPais').AsString   := FCTe.toma.enderToma.XPais;
      FieldByName('Fone').AsString    := FormatarFone(FCTe.toma.Fone);
      FieldByName('Email').AsString   := FCTe.toma.email;
    end
    else
    begin
      case FCTe.ide.Toma03.Toma of
        tmRemetente:
          begin
            FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
            FieldByName('XNome').AsString   := FCTe.Rem.xNome;
            FieldByName('XFant').AsString   := FCTe.Rem.XFant;
            FieldByName('IE').AsString      := FCTe.Rem.IE;
            FieldByName('Xlgr').AsString    := FCTe.Rem.EnderReme.XLgr;
            FieldByName('Nro').AsString     := FCTe.Rem.EnderReme.Nro;
            FieldByName('XCpl').AsString    := FCTe.Rem.EnderReme.XCpl;
            FieldByName('XBairro').AsString := FCTe.Rem.EnderReme.XBairro;
            FieldByName('CMun').AsString    := IntToStr(FCTe.Rem.EnderReme.CMun);
            FieldByName('XMun').AsString    := FCTe.Rem.EnderReme.XMun;
            FieldByName('UF').AsString      := FCTe.Rem.EnderReme.UF;
            FieldByName('CEP').AsString     := ManterCep( FCTe.Rem.EnderReme.CEP );
            FieldByName('CPais').AsString   := IntToStr(FCTe.Rem.EnderReme.CPais);
            FieldByName('XPais').AsString   := FCTe.Rem.EnderReme.XPais;
            FieldByName('Fone').AsString    := FormatarFone(FCTe.Rem.Fone);
          end;

        tmDestinatario:
          begin
            FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(FCTe.Dest.CNPJCPF);
            FieldByName('XNome').AsString   := FCTe.Dest.xNome;
            FieldByName('IE').AsString      := FCTe.Dest.IE;
            FieldByName('Xlgr').AsString    := FCTe.Dest.EnderDest.XLgr;
            FieldByName('Nro').AsString     := FCTe.Dest.EnderDest.Nro;
            FieldByName('XCpl').AsString    := FCTe.Dest.EnderDest.XCpl;
            FieldByName('XBairro').AsString := FCTe.Dest.EnderDest.XBairro;
            FieldByName('CMun').AsString    := IntToStr(FCTe.Dest.EnderDest.CMun);
            FieldByName('XMun').AsString    := FCTe.Dest.EnderDest.XMun;
            FieldByName('UF').AsString      := FCTe.Dest.EnderDest.UF;
            FieldByName('CEP').AsString     := ManterCep( FCTe.Dest.EnderDest.CEP );
            FieldByName('CPais').AsString   := IntToStr(FCTe.Dest.EnderDest.CPais);
            FieldByName('XPais').AsString   := FCTe.Dest.EnderDest.XPais;
            FieldByName('Fone').AsString    := FormatarFone(FCTe.Dest.Fone);
          end;

        tmExpedidor:
          begin
            FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(FCTe.Exped.CNPJCPF);
            FieldByName('XNome').AsString   := FCTe.Exped.xNome;
            FieldByName('IE').AsString      := FCTe.Exped.IE;
            FieldByName('Xlgr').AsString    := FCTe.Exped.EnderExped.XLgr;
            FieldByName('Nro').AsString     := FCTe.Exped.EnderExped.Nro;
            FieldByName('XCpl').AsString    := FCTe.Exped.EnderExped.XCpl;
            FieldByName('XBairro').AsString := FCTe.Exped.EnderExped.XBairro;
            FieldByName('CMun').AsString    := IntToStr(FCTe.Exped.EnderExped.CMun);
            FieldByName('XMun').AsString    := FCTe.Exped.EnderExped.XMun;
            FieldByName('UF').AsString      := FCTe.Exped.EnderExped.UF;
            FieldByName('CEP').AsString     := ManterCep( FCTe.Exped.EnderExped.CEP );
            FieldByName('CPais').AsString   := IntToStr(FCTe.Exped.EnderExped.CPais);
            FieldByName('XPais').AsString   := FCTe.Exped.EnderExped.XPais;
            FieldByName('Fone').AsString    := FormatarFone(FCTe.Exped.Fone);
          end;

        tmRecebedor:
          begin
            FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(FCTe.Receb.CNPJCPF);
            FieldByName('XNome').AsString   := FCTe.Receb.xNome;
            FieldByName('IE').AsString      := FCTe.Receb.IE;
            FieldByName('Xlgr').AsString    := FCTe.Receb.EnderReceb.XLgr;
            FieldByName('Nro').AsString     := FCTe.Receb.EnderReceb.Nro;
            FieldByName('XCpl').AsString    := FCTe.Receb.EnderReceb.XCpl;
            FieldByName('XBairro').AsString := FCTe.Receb.EnderReceb.XBairro;
            FieldByName('CMun').AsString    := IntToStr(FCTe.Receb.EnderReceb.CMun);
            FieldByName('XMun').AsString    := FCTe.Receb.EnderReceb.XMun;
            FieldByName('UF').AsString      := FCTe.Receb.EnderReceb.UF;
            FieldByName('CEP').AsString     := ManterCep( FCTe.Receb.EnderReceb.CEP);
            FieldByName('CPais').AsString   := IntToStr(FCTe.Receb.EnderReceb.CPais);
            FieldByName('XPais').AsString   := FCTe.Receb.EnderReceb.XPais;
            FieldByName('Fone').AsString    := FormatarFone(FCTe.Receb.Fone);
          end;
      end;

      case FCTe.ide.Toma4.Toma of
        tmOutros:
          begin
            FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(FCTe.ide.Toma4.CNPJCPF);
            FieldByName('XNome').AsString   := FCTe.ide.Toma4.xNome;
            FieldByName('IE').AsString      := FCTe.ide.Toma4.IE;
            FieldByName('Xlgr').AsString    := FCTe.ide.Toma4.EnderToma.XLgr;
            FieldByName('Nro').AsString     := FCTe.ide.Toma4.EnderToma.Nro;
            FieldByName('XCpl').AsString    := FCTe.ide.Toma4.EnderToma.XCpl;
            FieldByName('XBairro').AsString := FCTe.ide.Toma4.EnderToma.XBairro;
            FieldByName('CMun').AsString    := IntToStr(FCTe.ide.Toma4.EnderToma.CMun);
            FieldByName('XMun').AsString    := FCTe.ide.Toma4.EnderToma.XMun;
            FieldByName('UF').AsString      := FCTe.ide.Toma4.EnderToma.UF;
            FieldByName('CEP').AsString     := ManterCep( FCTe.ide.Toma4.EnderToma.CEP );
            FieldByName('CPais').AsString   := IntToStr(FCTe.ide.Toma4.EnderToma.CPais);
            FieldByName('XPais').AsString   := FCTe.ide.Toma4.EnderToma.XPais;
            FieldByName('Fone').AsString    := FormatarFone(FCTe.ide.Toma4.Fone);
          end;
      end;
    end;
    Post;
  end;

end;

procedure TACBrCTeDACTEFR.CarregaVolumes;
var
  i, J                     : Integer;
  MCub, Volumes, VlrServico: Currency;
  ProdutoPred, OutrasCaract: string;
  TipoMedida               : array of string;
  UnidMedida               : array of string;
  QdtMedida                : array of Currency;
begin
  with cdsVolumes do
  begin

    J          := 0;
    VlrServico := 0;
    MCub       := 0;
    Volumes    := 0;

    for i := 0 to CTE.infCTeNorm.infCarga.infQ.Count - 1 do
    begin
      with CTE.infCTeNorm.infCarga do
      begin
        ProdutoPred  := proPred;
        OutrasCaract := xOutCat;
        VlrServico := vCarga;

        case infQ.Items[i].cUnid of
          uM3: MCub         := MCub + infQ.Items[i].qCarga;
          uUNIDADE: Volumes := Volumes + infQ.Items[i].qCarga;
        end;
        begin
          Inc(J);
          SetLength(TipoMedida, J);
          SetLength(UnidMedida, J);
          SetLength(QdtMedida, J);
          TipoMedida[J - 1] := infQ.Items[i].tpMed;
          QdtMedida[J - 1]  := infQ.Items[i].qCarga;

          case infQ.Items[i].cUnid of
            uKG     : UnidMedida[J - 1] := 'KG';
            uTON    : UnidMedida[J - 1] := 'TON';
            uLITROS : UnidMedida[J - 1] := 'LT';
            uMMBTU  : UnidMedida[J - 1] := 'MMBTU';
            uUNIDADE: UnidMedida[J - 1] := 'UND';
            uM3     : UnidMedida[J - 1] := 'M3';
          end;
        end;
      end;
    end;

    if J = 0 then
    begin
      Append;
      FieldByName('Produto').AsString             := ProdutoPred;
      FieldByName('CaracteristicaCarga').AsString := OutrasCaract;
      FieldByName('ValorServico').AsFloat         := VlrServico;
      FieldByName('MCub').AsFloat                 := MCub;
      FieldByName('QVol').AsFloat                 := Volumes;
      Post;
    end
    else
      for i := 0 to J - 1 do
      begin
        Append;
        FieldByName('Produto').AsString             := ProdutoPred;
        FieldByName('CaracteristicaCarga').AsString := OutrasCaract;
        FieldByName('ValorServico').AsFloat         := VlrServico;
        FieldByName('MCub').AsFloat                 := MCub;
        FieldByName('QVol').AsFloat                 := Volumes;
        FieldByName('UnMedida').AsString            := UnidMedida[i];
        FieldByName('DescTipo').AsString            := TipoMedida[i];
        FieldByName('QMedida').AsFloat              := QdtMedida[i];
        Post;
      end;
  end;
end;

function TACBrCTeDACTEFR.ManterCep( iCep : Integer ) : String;
begin
  Result := '';
  if iCep > 0 then
    Result := FormatarCEP(Poem_Zeros(iCEP, 8));

end;

end.
