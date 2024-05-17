{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Wemerson Souto                                  }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
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

unit ACBrNFeDANFEFRDM;

interface

uses
  SysUtils, Classes, Forms, DB, DBClient, Graphics,
  pcnEnvEventoNFe, pcnRetInutNFe, pcnNFe, pcnConversao,
  ACBrDFeReport, ACBrDFeDANFeReport, ACBrNFeDANFEClass,
  frxClass, frxExportPDF, frxDBSet, frxBarcode;

type

  TACBrNFeFRClass = class
  private
    FDANFEClassOwner: TACBrDFeDANFeReport;
    FNFe: TNFe;
    FEvento: TEventoNFe;
    FfrxReport: TfrxReport;
    FfrxPDFExport: TfrxPDFExport;
    FfrxBarCodeObject: TfrxBarCodeObject;
    cdsIdentificacao: TClientDataSet;
    FfrxIdentificacao: TfrxDBDataset;
    cdsEmitente: TClientDataSet;
    FfrxEmitente: TfrxDBDataset;
    cdsDestinatario: TClientDataSet;
    FfrxDestinatario: TfrxDBDataset;
    cdsDadosProdutos: TClientDataSet;
    FfrxDadosProdutos: TfrxDBDataset;
    cdsParametros: TClientDataSet;
    FfrxParametros: TfrxDBDataset;
    cdsDuplicatas: TClientDataSet;
    FfrxDuplicatas: TfrxDBDataset;
    cdsCalculoImposto: TClientDataSet;
    FfrxCalculoImposto: TfrxDBDataset;
    cdsTransportador: TClientDataSet;
    FfrxTransportador: TfrxDBDataset;
    cdsVeiculo: TClientDataSet;
    FfrxVeiculo: TfrxDBDataset;
    cdsVolumes: TClientDataSet;
    FfrxVolumes: TfrxDBDataset;
    cdsEventos: TClientDataSet;
    FfrxEventos: TfrxDBDataset;
    cdsISSQN: TClientDataSet;
    FfrxISSQN: TfrxDBDataset;
    cdsFatura: TClientDataSet;
    FfrxFatura: TfrxDBDataset;
    cdsLocalRetirada: TClientDataSet;
    FfrxLocalRetirada: TfrxDBDataset;
    cdsLocalEntrega: TClientDataSet;
    FfrxLocalEntrega: TfrxDBDataset;
    cdsInformacoesAdicionais: TClientDataSet;
    FfrxInformacoesAdicionais: TfrxDBDataset;
    cdsPagamento: TClientDataSet;
    FfrxPagamento: TfrxDBDataset;
    FInutilizacao: TRetInutNFe;
    FfrxInutilizacao: TfrxDBDataset;
    cdsInutilizacao: TClientDataSet;

    FFastFile: String;
    FFastFileEvento: String;
    FFastFileInutilizacao: String;
    FPrintMode: TfrxPrintMode;
    FPrintOnSheet: Integer;
    FExibeCaptionButton: Boolean;
    FZoomModePadrao: TfrxZoomMode;
    {$IFNDEF FMX}
    FBorderIcon : TBorderIcons;
    {$ENDIF}
    FIncorporarFontesPdf: Boolean;
    FIncorporarBackgroundPdf: Boolean;
    FOtimizaImpressaoPdf: Boolean;
    FThreadSafe: Boolean;

    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
    procedure frxReportPreview(Sender: TObject);

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

    function CollateBr(const Str: String): String;
    procedure AjustaMargensReports;

  public
    constructor Create(AOwner: TACBrDFeDANFeReport);
    destructor Destroy; override;

    property NFe: TNFe read FNFe write FNFe;
    property Evento: TEventoNFe read FEvento write FEvento;
    property Inutilizacao: TRetInutNFe read FInutilizacao write FInutilizacao;
    property DANFEClassOwner: TACBrDFeDANFeReport read FDANFEClassOwner;
    property frxReport: TfrxReport read FfrxReport;
    property frxPDFExport: TfrxPDFExport read FfrxPDFExport;

    procedure SetDataSetsToFrxReport;
    procedure CarregaDadosNFe;
    procedure CarregaDadosEventos;
    procedure CarregaDadosInutilizacao;

    property FastFile: String read FFastFile write FFastFile;
    property FastFileEvento: String read FFastFileEvento write FFastFileEvento;
    property FastFileInutilizacao: String read FFastFileInutilizacao write FFastFileInutilizacao;

    property PrintMode: TfrxPrintMode read FPrintMode write FPrintMode default pmDefault;
    property PrintOnSheet: Integer read FPrintOnSheet write FPrintOnSheet default 0;
    property ExibeCaptionButton: Boolean read FExibeCaptionButton write FExibeCaptionButton default False;
    property ZoomModePadrao: TfrxZoomMode read FZoomModePadrao write FZoomModePadrao default ZMDEFAULT;
    {$IFNDEF FMX}
    property BorderIcon: TBorderIcons read FBorderIcon write FBorderIcon;
    {$ENDIF}
    property IncorporarBackgroundPdf: Boolean read FIncorporarBackgroundPdf write FIncorporarBackgroundPdf;
    property IncorporarFontesPdf: Boolean read FIncorporarFontesPdf write FIncorporarFontesPdf;
    property OtimizaImpressaoPdf: Boolean read FOtimizaImpressaoPdf write FOtimizaImpressaoPdf;
    property ThreadSafe: Boolean read FThreadSafe write FThreadSafe;

    function PrepareReport(ANFE: TNFe = nil): Boolean;
    function PrepareReportEvento(ANFE: TNFe = nil): Boolean;
    function PrepareReportInutilizacao: Boolean;

    function GetPreparedReport: TfrxReport;
    function GetPreparedReportEvento: TfrxReport;
    function GetPreparedReportInutilizacao: TfrxReport;

    procedure ImprimirDANFE(ANFE: TNFe = nil);
    procedure ImprimirDANFEResumido(ANFE: TNFe = nil);
    procedure ImprimirDANFEPDF(ANFE: TNFe = nil; AStream: TStream = nil);
    procedure ImprimirEVENTO(ANFE: TNFe = nil);
    procedure ImprimirEVENTOPDF(ANFE: TNFe = nil; AStream: TStream = nil);
    procedure ImprimirINUTILIZACAO(ANFE: TNFe = nil);
    procedure ImprimirINUTILIZACAOPDF(ANFE: TNFe = nil; AStream: TStream = nil);

  end;

implementation

uses
  StrUtils, Math, DateUtils,
  ACBrNFe, ACBrNFeDANFEFR, ACBrDFeUtil,
  ACBrUtil.Strings, 
  ACBrUtil.Math, ACBrUtil.FilesIO, ACBrUtil.Base, ACBrUtil.DateTime, ACBrUtil.XMLHTML,
  ACBrValidador, ACBrImage, ACBrDelphiZXingQRCode,
  pcnConversaoNFe;

{ TACBrNFeFRClass }

constructor TACBrNFeFRClass.Create(AOwner: TACBrDFeDANFeReport);
begin
  if not (AOwner is TACBrDFeDANFeReport) then
    raise EACBrNFeException.Create('AOwner deve ser do tipo TACBrDFeDANFeReport');

  FThreadSafe := False;
  FFastFile := '';
  FExibeCaptionButton := False;
  FZoomModePadrao := ZMDEFAULT;
  {$IFNDEF FMX}
  FBorderIcon := [biSystemMenu,biMaximize,biMinimize];
  {$ENDIF}
  FIncorporarFontesPdf := True;
  FIncorporarBackgroundPdf := True;
  FOtimizaImpressaoPdf := True;

  FDANFEClassOwner := AOwner;

  FfrxReport := TfrxReport.Create(AOwner);
  //Antes de alterar a linha abaixo, queira verificar o seguinte tópico:
  //https://www.projetoacbr.com.br/forum/topic/51505-travamento-preview-de-v%C3%A1rias-danfes/
  FfrxReport.EngineOptions.UseGlobalDataSetList := False;
  FfrxReport.PreviewOptions.Buttons := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, {$IFNDEF FMX} pbFind,{$ENDIF}
    pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick];

  with FfrxReport do
  begin
     EngineOptions.DoublePass := True;
     StoreInDFM := False;
     OnBeforePrint := frxReportBeforePrint;
     OnReportPrint := 'frxReportOnReportPrint';
  end;

  FfrxPDFExport := TfrxPDFExport.Create(AOwner);
  with FfrxPDFExport do
  begin
     Background    := FIncorporarBackgroundPdf;
     EmbeddedFonts := FIncorporarFontesPdf;
     PrintOptimized := FOtimizaImpressaoPdf;
     Subject       := 'Exportando DANFE para PDF';
     ShowProgress  := False;
  end;

  RttiSetProp(FfrxPDFExport, 'Transparency', 'False');

  // cdsIdentificacao
  if not Assigned(cdsIdentificacao) then
  begin
     cdsIdentificacao := TClientDataSet.Create(AOwner);
     FfrxIdentificacao := TfrxDBDataset.Create(AOwner);
     with FfrxIdentificacao do
     begin
        DataSet := cdsIdentificacao;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Identificacao';
     end;

     with cdsIdentificacao do
     begin
        FieldDefs.Add('Id', ftString, 44);
        FieldDefs.Add('Versao', ftFloat);
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
        FieldDefs.Add('xPed', ftString, 20);
        CreateDataSet;
     end;
   end;

   // cdsEmitente
   if not Assigned(cdsEmitente) then
   begin
     cdsEmitente := TClientDataSet.Create(AOwner);
     FfrxEmitente := TfrxDBDataset.Create(AOwner);
     with FfrxEmitente do
     begin
        DataSet := cdsEmitente;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Emitente';
     end;

     with cdsEmitente do
     begin
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
        FieldDefs.Add('Fone', ftString, 17);
        FieldDefs.Add('IE', ftString, 15);
        FieldDefs.Add('IM', ftString, 15);
        FieldDefs.Add('IEST', ftString, 15);
        FieldDefs.Add('CRT', ftString, 1);
        FieldDefs.Add('DESCR_CST', ftString, 30);
        FieldDefs.Add('DADOS_ENDERECO', ftString, 1000);
        CreateDataSet;
     end;
   end;

   // cdsDestinatario
   if not Assigned(cdsDestinatario) then
   begin
     cdsDestinatario := TClientDataSet.Create(AOwner);
     FfrxDestinatario := TfrxDBDataset.Create(AOwner);
     with FfrxDestinatario do
     begin
        DataSet := cdsDestinatario;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Destinatario';
     end;

     with cdsDestinatario do
     begin
        FieldDefs.Add('CNPJCPF', ftString, 20);
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
        FieldDefs.Add('Fone', ftString, 17);
        FieldDefs.Add('IE', ftString, 18);
        FieldDefs.Add('Consumidor', ftString, 400);
        CreateDataSet;
     end;
   end;

   // cdsDadosProdutos
   if not Assigned(cdsDadosProdutos) then
   begin
     cdsDadosProdutos   := TClientDataSet.Create(AOwner);
     FfrxDadosProdutos  := TfrxDBDataset.Create(AOwner);
     with FfrxDadosProdutos do
     begin
        DataSet := cdsDadosProdutos;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'DadosProdutos';
     end;

     with cdsDadosProdutos do
     begin
        FieldDefs.Add('CProd'     , ftString, 60);
        FieldDefs.Add('cEAN'      , ftString, 60);
        FieldDefs.Add('XProd'     , ftString, 120);
        FieldDefs.Add('infAdProd' , ftString, 1000);
        FieldDefs.Add('NCM'       , ftString, 9);
        FieldDefs.Add('EXTIPI'    , ftString, 8);
        FieldDefs.Add('genero'    , ftString, 8);
        FieldDefs.Add('CFOP'      , ftString, 4);
        FieldDefs.Add('UCom'      , ftString, 6);
        FieldDefs.Add('QCom'      , ftFloat);
        FieldDefs.Add('VUnCom'    , ftFloat);
        FieldDefs.Add('VProd'     , ftString, 18);
        FieldDefs.Add('cEANTrib'  , ftString, 60);
        FieldDefs.Add('UTrib'     , ftString, 6);
        FieldDefs.Add('QTrib'     , ftFloat);
        FieldDefs.Add('vUnTrib'   , ftFloat);
        FieldDefs.Add('vFrete'    , ftString, 18);
        FieldDefs.Add('vOutro'    , ftString, 18);
        FieldDefs.Add('vSeg'      , ftString, 18);
        FieldDefs.Add('vDesc'     , ftString, 18);
        FieldDefs.Add('ORIGEM'    , ftString, 1);
        FieldDefs.Add('CST'       , ftString, 3);
        FieldDefs.Add('vBC'       , ftString, 18);
        FieldDefs.Add('pICMS'     , ftString, 18);
        FieldDefs.Add('vICMS'     , ftString, 18);
        FieldDefs.Add('vIPI'      , ftString, 18);
        FieldDefs.Add('vIPIDevol'      , ftString, 18);
        FieldDefs.Add('pIPI'      , ftString, 18);
        FieldDefs.Add('VTotTrib'  , ftString, 18);
        FieldDefs.Add('ChaveNFe'  , ftString, 50);
        FieldDefs.Add('vISSQN'    , ftString, 18);
        FieldDefs.Add('vBcISSQN'  , ftString, 18);
        FieldDefs.Add('vBcST'     , ftString, 18);
        FieldDefs.Add('pMVAST'    , ftString, 18);
        FieldDefs.Add('pICMSST'   , ftString, 18);
        FieldDefs.Add('vICMSST'   , ftString, 18);
        FieldDefs.Add('DescricaoProduto', ftString, 10000);
        FieldDefs.Add('Unidade'   , ftString, 14);
        FieldDefs.Add('Quantidade', ftString, 50);
        FieldDefs.Add('ValorUnitario'   , ftString, 50);
        FieldDefs.Add('ValorLiquido'    , ftString, 18);
        FieldDefs.Add('ValorAcrescimos' , ftString, 18);
        FieldDefs.Add('ValorDescontos'  , ftString, 18);
        FieldDefs.Add('xPed'            , ftString, 15);
        FieldDefs.Add('nItemPed'        , ftInteger);

        CreateDataSet;
     end;
   end;

   // cdsParametros
   if not Assigned(cdsParametros) then
   begin
     cdsParametros  := TClientDataSet.Create(AOwner);
     FfrxParametros := TfrxDBDataset.Create(AOwner);
     with FfrxParametros do
     begin
        DataSet         := cdsParametros;
        OpenDataSource  := False;
        Enabled := False;
        UserName        := 'Parametros';
     end;

     with cdsParametros do
     begin
        FieldDefs.Add('poscanhoto', ftString, 1);
        FieldDefs.Add('ResumoCanhoto', ftString, 200);
        FieldDefs.Add('Mensagem0', ftString, 60);
        FieldDefs.Add('Imagem', ftString, 256);
        FieldDefs.Add('Sistema', ftString, 300);
        FieldDefs.Add('Usuario', ftString, 60);
        FieldDefs.Add('Site', ftString, 60);
        FieldDefs.Add('Email', ftString, 60);
        FieldDefs.Add('Desconto', ftString, 60);
        FieldDefs.Add('TotalLiquido', ftString, 60);
        FieldDefs.Add('ChaveAcesso_Descricao', ftString, 90);
        FieldDefs.Add('Contingencia_ID', ftString, 36);
        FieldDefs.Add('Contingencia_Descricao', ftString, 60);
        FieldDefs.Add('Contingencia_Valor', ftString, 60);
        FieldDefs.Add('LinhasPorPagina', ftInteger);
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
        FieldDefs.Add('QrCodeLateral', ftString, 1);
        FieldDefs.Add('ImprimeEm1Linha', ftString, 1);
        FieldDefs.Add('ImprimeEmDuasLinhas', ftString, 1);
        FieldDefs.Add('DescricaoViaEstabelec', ftString, 30);
        FieldDefs.Add('QtdeItens', ftInteger);
        FieldDefs.Add('ExpandirDadosAdicionaisAuto', ftString, 1);
        FieldDefs.Add('ImprimeDescAcrescItem', ftInteger);
        FieldDefs.Add('nProt', ftString, 30);
        FieldDefs.Add('dhRecbto', ftDateTime);
        FieldDefs.Add('poscanhotolayout', ftString, 1);
        FieldDefs.Add('ExibeICMSDesoneradoComoDesconto', ftBoolean);
        CreateDataSet;
     end;
   end;

   // cdsDuplicatas
   if not Assigned(cdsDuplicatas) then
   begin
     cdsDuplicatas := TClientDataSet.Create(AOwner);
     FfrxDuplicatas := TfrxDBDataset.Create(AOwner);
     with FfrxDuplicatas do
     begin
        DataSet := cdsDuplicatas;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Duplicatas';
     end;

     with cdsDuplicatas do
     begin
        FieldDefs.Add('NDup', ftString, 60);
        FieldDefs.Add('DVenc', ftString, 10);
        FieldDefs.Add('VDup', ftFloat);
        FieldDefs.Add('ChaveNFe', ftString, 50);
        CreateDataSet;
     end;
   end;

   // cdsCalculoImposto
   if not Assigned(cdsCalculoImposto) then
   begin
     cdsCalculoImposto := TClientDataSet.Create(AOwner);
     FfrxCalculoImposto := TfrxDBDataset.Create(AOwner);
     with FfrxCalculoImposto do
     begin
        DataSet := cdsCalculoImposto;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'CalculoImposto';
     end;

     with cdsCalculoImposto do
     begin
        FieldDefs.Add('VBC'         , ftFloat);
        FieldDefs.Add('VICMS'       , ftFloat);
        FieldDefs.Add('VBCST'       , ftFloat);
        FieldDefs.Add('VST'         , ftFloat);
        FieldDefs.Add('VProd'       , ftFloat);
        FieldDefs.Add('VFrete'      , ftFloat);
        FieldDefs.Add('VSeg'        , ftFloat);
        FieldDefs.Add('VDesc'       , ftFloat);
        FieldDefs.Add('vICMSDeson'  , ftFloat);
        FieldDefs.Add('VII'         , ftFloat);
        FieldDefs.Add('VIPI'        , ftFloat);
        FieldDefs.Add('VPIS'        , ftFloat);
        FieldDefs.Add('VCOFINS'     , ftFloat);
        FieldDefs.Add('VOutro'      , ftFloat);
        FieldDefs.Add('VNF'         , ftFloat);
        FieldDefs.Add('VTotTrib'    , ftFloat);
        FieldDefs.Add('VTribPerc'   , ftFloat);
        FieldDefs.Add('VTribFonte'  , ftString, 100);
        FieldDefs.Add('vTotPago'    , ftFloat);
        FieldDefs.Add('vTroco'      , ftFloat);
        FieldDefs.Add('ValorApagar' , ftFloat);
        FieldDefs.Add('VFCP'        , ftFloat);
        FieldDefs.Add('VFCPST'      , ftFloat);
        FieldDefs.Add('VFCPSTRet'   , ftFloat);
        FieldDefs.Add('VIPIDevol'   , ftFloat);
        CreateDataSet;
     end;
   end;

   // cdsTransportador
   if not Assigned(cdsTransportador) then
   begin
     cdsTransportador := TClientDataSet.Create(AOwner);
     FfrxTransportador := TfrxDBDataset.Create(AOwner);
     with FfrxTransportador do
     begin
        DataSet := cdsTransportador;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Transportador';
     end;

     with cdsTransportador do
     begin
        FieldDefs.Add('ModFrete', ftString, 20);
        FieldDefs.Add('CNPJCPF', ftString, 18);
        FieldDefs.Add('XNome', ftString, 60);
        FieldDefs.Add('IE', ftString, 15);
        FieldDefs.Add('XEnder', ftString, 60);
        FieldDefs.Add('XMun', ftString, 60);
        FieldDefs.Add('UF', ftString, 2);
        CreateDataSet;
     end;
   end;

   // cdsVeiculo
   if not Assigned(cdsVeiculo) then
   begin
     cdsVeiculo := TClientDataSet.Create(AOwner);
     FfrxVeiculo := TfrxDBDataset.Create(AOwner);
     with FfrxVeiculo do
     begin
        DataSet := cdsVeiculo;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Veiculo';
     end;

     with cdsVeiculo do
     begin
        FieldDefs.Add('PLACA', ftString, 8);
        FieldDefs.Add('UF', ftString, 2);
        FieldDefs.Add('RNTC', ftString, 20);
        CreateDataSet;
     end;
   end;

   // cdsVolumes
   if not Assigned(cdsVolumes) then
   begin
     cdsVolumes := TClientDataSet.Create(AOwner);
     FfrxVolumes := TfrxDBDataset.Create(AOwner);
     with FfrxVolumes do
     begin
        DataSet := cdsVolumes;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Volumes';
     end;

     with cdsVolumes do
     begin
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
   if not Assigned(cdsEventos) then
   begin
      cdsEventos := TClientDataSet.Create(AOwner);
      FfrxEventos := TfrxDBDataset.Create(AOwner);
      with FfrxEventos do
      begin
         DataSet := cdsEventos;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'Eventos';
      end;
   end;

   // cdsISSQN
   if not Assigned(cdsISSQN) then
   begin
      cdsISSQN := TClientDataSet.Create(AOwner);
      FfrxISSQN := TfrxDBDataset.Create(AOwner);
      with FfrxISSQN do
      begin
         DataSet := cdsISSQN;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'ISSQN';
      end;

      with cdsISSQN do
      begin
         FieldDefs.Add('vSERV', ftFloat);
         FieldDefs.Add('vBC', ftFloat);
         FieldDefs.Add('vISS', ftFloat);
         FieldDefs.Add('vDescIncond', ftFloat);
         FieldDefs.Add('vISSRet', ftFloat);
         CreateDataSet;
      end;
   end;

   // cdsFatura
   if not Assigned(cdsFatura) then
   begin
      cdsFatura   := TClientDataSet.Create(AOwner);
      FfrxFatura  := TfrxDBDataset.Create(AOwner);
      with FfrxFatura do
      begin
         DataSet        := cdsFatura;
         OpenDataSource := False;
         Enabled := False;
         UserName       := 'Fatura';
      end;

      with cdsFatura do
      begin
         FieldDefs.Add('iForma'   , ftInteger);
         FieldDefs.Add('Pagamento', ftString, 20);
         FieldDefs.Add('nFat'     , ftString, 60);
         FieldDefs.Add('vOrig'    , ftFloat);
         FieldDefs.Add('vDesc'    , ftFloat);
         FieldDefs.Add('vLiq'     , ftFloat);
         CreateDataSet;
      end;
   end;

   // cdsLocalRetirada
   if not Assigned(cdsLocalRetirada) then
   begin
      cdsLocalRetirada := TClientDataSet.Create(AOwner);
      FfrxLocalRetirada := TfrxDBDataset.Create(AOwner);
      with FfrxLocalRetirada do
      begin
         DataSet := cdsLocalRetirada;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'LocalRetirada';
      end;

      with cdsLocalRetirada do
      begin
         FieldDefs.Add('CNPJ', ftString, 18);
         FieldDefs.Add('XLgr', ftString, 60);
         FieldDefs.Add('Nro', ftString, 60);
         FieldDefs.Add('XCpl', ftString, 60);
         FieldDefs.Add('XBairro', ftString, 60);
         FieldDefs.Add('CMun', ftString, 7);
         FieldDefs.Add('XMun', ftString, 60);
         FieldDefs.Add('UF', ftString, 2);
         FieldDefs.Add('XNome', ftString, 60);
         FieldDefs.Add('CEP', ftString, 9);
         FieldDefs.Add('CPais', ftString, 4);
         FieldDefs.Add('XPais', ftString, 60);
         FieldDefs.Add('Fone', ftString, 17);
         FieldDefs.Add('IE', ftString, 15);
         CreateDataSet;
      end;
   end;

   // cdsLocalEntrega
   if not Assigned(cdsLocalEntrega) then
   begin
      cdsLocalEntrega := TClientDataSet.Create(AOwner);
      FfrxLocalEntrega := TfrxDBDataset.Create(AOwner);
      with FfrxLocalEntrega do
      begin
         DataSet := cdsLocalEntrega;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'LocalEntrega';
      end;

      with cdsLocalEntrega do
      begin
         FieldDefs.Add('CNPJ', ftString, 18);
         FieldDefs.Add('XLgr', ftString, 60);
         FieldDefs.Add('Nro', ftString, 60);
         FieldDefs.Add('XCpl', ftString, 60);
         FieldDefs.Add('XBairro', ftString, 60);
         FieldDefs.Add('CMun', ftString, 7);
         FieldDefs.Add('XMun', ftString, 60);
         FieldDefs.Add('UF', ftString, 2);
         FieldDefs.Add('XNome', ftString, 60);
         FieldDefs.Add('CEP', ftString, 9);
         FieldDefs.Add('CPais', ftString, 4);
         FieldDefs.Add('XPais', ftString, 60);
         FieldDefs.Add('Fone', ftString, 17);
         FieldDefs.Add('IE', ftString, 15);
         CreateDataSet;
      end;
   end;

   // cdsInformacoesAdicionais
   if not Assigned(cdsInformacoesAdicionais) then
   begin
      cdsInformacoesAdicionais := TClientDataSet.Create(AOwner);
      FfrxInformacoesAdicionais := TfrxDBDataset.Create(AOwner);
      with FfrxInformacoesAdicionais do
      begin
         DataSet := cdsInformacoesAdicionais;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'InformacoesAdicionais';
      end;

      with cdsInformacoesAdicionais do
      begin
         FieldDefs.Add('OBS', ftString, 6900);
         FieldDefs.Add('LinhasOBS', ftInteger);
         FieldDefs.Add('MensagemSEFAZ', ftString, 200);
         CreateDataSet;
      end;
   end;

   // cdsPagamento
   if not Assigned(cdsPagamento) then
   begin
      cdsPagamento := TClientDataSet.Create(AOwner);
      FfrxPagamento := TfrxDBDataset.Create(AOwner);
      with FfrxPagamento do
      begin
         DataSet := cdsPagamento;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'Pagamento';
      end;

      with cdsPagamento do
      begin
         FieldDefs.Add('tPag', ftString, 50);
         FieldDefs.Add('vPag', ftFloat);
         FieldDefs.Add('vTroco', ftFloat);
         FieldDefs.Add('CNPJ', ftString, 50);
         FieldDefs.Add('tBand', ftString, 50);
         FieldDefs.Add('cAut', ftString, 20);
         CreateDataSet;
      end;
   end;

   //cdsInutilização
   if not Assigned(cdsInutilizacao) then
   begin
      cdsInutilizacao := TClientDataSet.Create(AOwner);
      FfrxInutilizacao := TfrxDBDataset.Create(AOwner);
      with FfrxInutilizacao do
      begin
         DataSet := cdsInutilizacao;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'Inutilizacao';
      end;
   end;
end;

destructor TACBrNFeFRClass.Destroy;
begin
  FfrxReport.Free;
  FfrxPDFExport.Free;
  FfrxBarCodeObject.Free;
  cdsIdentificacao.Free;
  FfrxIdentificacao.Free;
  cdsEmitente.Free;
  FfrxEmitente.Free;
  cdsDestinatario.Free;
  FfrxDestinatario.Free;
  cdsDadosProdutos.Free;
  FfrxDadosProdutos.Free;
  cdsParametros.Free;
  FfrxParametros.Free;
  cdsDuplicatas.Free;
  FfrxDuplicatas.Free;
  cdsCalculoImposto.Free;
  FfrxCalculoImposto.Free;
  cdsTransportador.Free;
  FfrxTransportador.Free;
  cdsVeiculo.Free;
  FfrxVeiculo.Free;
  cdsVolumes.Free;
  FfrxVolumes.Free;
  cdsEventos.Free;
  FfrxEventos.Free;
  cdsISSQN.Free;
  FfrxISSQN.Free;
  cdsFatura.Free;
  FfrxFatura.Free;
  cdsLocalRetirada.Free;
  FfrxLocalRetirada.Free;
  cdsLocalEntrega.Free;
  FfrxLocalEntrega.Free;
  cdsInformacoesAdicionais.Free;
  FfrxInformacoesAdicionais.Free;
  cdsPagamento.Free;
  FfrxPagamento.Free;
  cdsInutilizacao.Free;
  FfrxInutilizacao.Free;

  inherited Destroy;
end;

procedure TACBrNFeFRClass.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(FfrxIdentificacao);
  frxReport.EnabledDataSets.Add(FfrxEmitente);
  frxReport.EnabledDataSets.Add(FfrxDestinatario);
  frxReport.EnabledDataSets.Add(FfrxDadosProdutos);
  frxReport.EnabledDataSets.Add(FfrxCalculoImposto);
  frxReport.EnabledDataSets.Add(FfrxTransportador);
  frxReport.EnabledDataSets.Add(FfrxVeiculo);
  frxReport.EnabledDataSets.Add(FfrxVolumes);
  frxReport.EnabledDataSets.Add(FfrxEventos);
  frxReport.EnabledDataSets.Add(FfrxISSQN);
  frxReport.EnabledDataSets.Add(FfrxFatura);
  frxReport.EnabledDataSets.Add(FfrxLocalRetirada);
  frxReport.EnabledDataSets.Add(FfrxLocalEntrega);
  frxReport.EnabledDataSets.Add(FfrxInformacoesAdicionais);
  frxReport.EnabledDataSets.Add(FfrxPagamento);
  frxReport.EnabledDataSets.Add(FfrxParametros);
  frxReport.EnabledDataSets.Add(FfrxDuplicatas);
  frxReport.EnabledDataSets.Add(FfrxInutilizacao);
end;

function TACBrNFeFRClass.CollateBr(const Str: String): String;
var
  Resultado,Temp: string;
  vChar: Char;
  Tamanho, i: integer;
begin
  Result := '';
  Tamanho := Length(str);
  i := 1;
  while (i <= Tamanho) do
  begin
    Temp := Copy(str,i,1);
    vChar := Temp[1];
    case vChar of
      'á', 'â', 'ã', 'à', 'ä', 'å', 'Á', 'Â', 'Ã', 'À', 'Ä', 'Å': Resultado := 'A';
      'é', 'ê', 'è', 'ë', 'É', 'Ê', 'È', 'Ë': Resultado := 'E';
      'í', 'î', 'ì', 'ï', 'Í', 'Î', 'Ì', 'Ï': Resultado := 'I';
      'ó', 'ô', 'õ', 'ò', 'ö', 'Ó', 'Ô', 'Õ', 'Ò', 'Ö': Resultado := 'O';
      'ú', 'û', 'ù', 'ü', 'Ú', 'Û', 'Ù', 'Ü': Resultado := 'U';
      'ç', 'Ç': Resultado := 'C';
      'ñ', 'Ñ': Resultado := 'N';
      'ý', 'ÿ', 'Ý', 'Y': Resultado := 'Y';
    else
      if vChar > #127 then Resultado := #32
      {$IFDEF DELPHI12_UP}
      else if CharInset(vChar, ['a'..'z','A'..'Z','0'..'9','-',' ',Chr(39)]) then
      {$ELSE}
      else if vChar in ['a'..'z','A'..'Z','0'..'9','-',' ',Chr(39)] then
      {$ENDIF}
        Resultado := UpperCase(vCHAR);
    end;

    Result := Result + Resultado;
    i := i + 1;
  end;
end;

procedure TACBrNFeFRClass.CarregaCalculoImposto;
var
  lvTroco: Currency;
begin
  with cdsCalculoImposto do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Total.ICMSTot do
    begin
      FieldByName('VBC').AsFloat          := VBC;
      FieldByName('VICMS').AsFloat        := VICMS;
      FieldByName('VBCST').AsFloat        := VBCST;
      FieldByName('VST').AsFloat          := VST;
      FieldByName('VProd').AsFloat        := VProd;
      FieldByName('VFrete').AsFloat       := VFrete;
      FieldByName('VSeg').AsFloat         := VSeg;
      FieldByName('VDesc').AsFloat        := VDesc;
      FieldByName('vICMSDeson').AsFloat   := vICMSDeson;
      FieldByName('VII').AsFloat          := VII;
      FieldByName('VIPI').AsFloat         := VIPI;
      FieldByName('VPIS').AsFloat         := VPIS;
      FieldByName('VCOFINS').AsFloat      := VCOFINS;
      FieldByName('VOutro').AsFloat       := VOutro;
      FieldByName('VNF').AsFloat          := VNF;

      if (FDANFEClassOwner.ImprimeTributos = trbNormal) or (FNFe.Ide.Modelo = 65)  then
        FieldByName('VTotTrib').AsFloat     := VTotTrib;

      FieldByName('ValorApagar').AsFloat  := VNF;
      FieldByName('VFCP').AsFloat         := VFCP;
      FieldByName('VFCPST').AsFloat       := VFCPST;
      FieldByName('VFCPSTRet').AsFloat    := vFCPSTRet;
      FieldByName('VIPIDevol').AsFloat    := vIPIDevol;

      if (FDANFEClassOwner is TACBrNFeDANFEClass) then
        FieldByName('VTribPerc').AsFloat := TACBrNFeDANFEClass(FDANFEClassOwner).ManterVTribPerc(VTotTrib, VProd, VNF);

      if NaoEstaVazio(FDANFEClassOwner.FonteTributos) then
        FieldByName('VTribFonte').AsString := '(Fonte: '+FDANFEClassOwner.FonteTributos+')';

      lvTroco := FNFe.pag.vTroco;
      if (lvTroco = 0) and (FDANFEClassOwner is TACBrNFeDANFCEClass) then
        lvTroco := TACBrNFeDANFCEClass(FDANFEClassOwner).vTroco;
      FieldByName('vTroco').AsCurrency    := lvTroco;
      FieldByName('vTotPago').AsCurrency  := lvTroco + vNF;
    end;

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaDadosNFe;
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

procedure TACBrNFeFRClass.CarregaDadosProdutos;
var
  inItem : Integer;
begin
  if not cdsParametros.Active then
    CarregaParametros;

  cdsParametros.First;

  // dados dos produtos
  with cdsDadosProdutos do
  begin
    Close;
    CreateDataSet;

    // verificar se e DANFE detalhado
    if ((FDANFEClassOwner is TACBrNFeDANFCEClass) and not
       TACBrNFeDANFCEClass(FDANFEClassOwner).ImprimeItens) then
      Exit;

    for inItem := 0 to (NFe.Det.Count - 1) do
    begin
      Append;
      with FNFe.Det.Items[inItem] do
      begin
        FieldByName('ChaveNFe').AsString          := FNFe.infNFe.ID;
        FieldByName('cProd').AsString             := FDANFEClassOwner.ManterCodigo(Prod.cEAN,Prod.cProd);
        FieldByName('cEAN').AsString              := Prod.cEAN;
        FieldByName('XProd').AsString             := StringReplace( Prod.xProd, ';', #13, [rfReplaceAll]);
        FieldByName('VProd').AsString             := FDANFEClassOwner.ManterVprod(Prod.VProd , Prod.vDesc );
        FieldByName('vTotTrib').AsString          := FDANFEClassOwner.ManterdvTotTrib(Imposto.vTotTrib );
        FieldByName('infAdProd').AsString         := FDANFEClassOwner.ManterinfAdProd(FNFe, inItem);
        FieldByName('DescricaoProduto').AsString  := FDANFEClassOwner.ManterXProd(FNFe, inItem);
        FieldByName('NCM').AsString               := Prod.NCM;
        FieldByName('EXTIPI').AsString            := Prod.EXTIPI;
        FieldByName('genero').AsString            := '';
        FieldByName('CFOP').AsString              := Prod.CFOP;
        FieldByName('Ucom').AsString              := Prod.UCom;
        FieldByName('QCom').AsFloat               := Prod.QCom;
        FieldByName('VUnCom').AsFloat             := Prod.VUnCom;
        FieldByName('cEANTrib').AsString          := Prod.cEANTrib;
        FieldByName('UTrib').AsString             := Prod.uTrib;
        FieldByName('QTrib').AsFloat              := Prod.qTrib;
        FieldByName('VUnTrib').AsFloat            := Prod.vUnTrib;
        FieldByName('vFrete').AsString            := FormatFloatBr( Prod.vFrete ,',0.00');
        FieldByName('vSeg').AsString              := FormatFloatBr( Prod.vSeg   ,',0.00');
        FieldByName('vOutro').AsString            := FormatFloatBr( Prod.vOutro ,',0.00');

        if FDANFEClassOwner is TACBrNFeDANFEClass then
        begin
          case TACBrNFeDANFEClass(FDANFEClassOwner).ImprimeValor of
          iuComercial:
            begin
              FieldByName('Unidade').AsString       := FieldByName('Ucom').AsString;
              FieldByName('Quantidade').AsString    := FDANFEClassOwner.FormatarQuantidade( FieldByName('QCom').AsFloat );
              FieldByName('ValorUnitario').AsString := FDANFEClassOwner.FormatarValorUnitario( FieldByName('VUnCom').AsFloat );
            end;
          iuTributavel:
            begin
              FieldByName('Unidade').AsString       := FieldByName('UTrib').AsString;
              FieldByName('Quantidade').AsString    := FDANFEClassOwner.FormatarQuantidade( FieldByName('QTrib').AsFloat );
              FieldByName('ValorUnitario').AsString := FDANFEClassOwner.FormatarValorUnitario( FieldByName('VUnTrib').AsFloat);
            end;
          iuComercialETributavel:
            begin
              if FieldByName('Ucom').AsString = FieldByName('UTrib').AsString then
              begin
                FieldByName('Unidade').AsString       := FieldByName('Ucom').AsString;
                FieldByName('Quantidade').AsString    := FDANFEClassOwner.FormatarQuantidade( FieldByName('QCom').AsFloat );
                FieldByName('ValorUnitario').AsString := FDANFEClassOwner.FormatarValorUnitario( FieldByName('VUnCom').AsFloat );
              end
              else
              begin
                FieldByName('Unidade').AsString       := FDANFEClassOwner.ManterUnidades(FieldByName('Ucom').AsString, FieldByName('UTrib').AsString);
                FieldByName('Quantidade').AsString    := FDANFEClassOwner.ManterQuantidades(FieldByName('QCom').AsFloat, FieldByName('QTrib').AsFloat);
                FieldByName('ValorUnitario').AsString := FDANFEClassOwner.ManterValoresUnitarios(FieldByName('VUnCom').AsFloat, FieldByName('VUnTrib').AsFloat);
              end;
            end;
          end;
          FieldByName('vDesc').AsString           := FormatFloatBr( TACBrNFeDANFEClass(FDANFEClassOwner).ManterVDesc( Prod.vDesc , Prod.VUnCom , Prod.QCom),',0.00');
        end
        else
        begin
          FieldByName('Unidade').AsString       := FieldByName('Ucom').AsString;
          FieldByName('Quantidade').AsString    := FDANFEClassOwner.FormatarQuantidade( FieldByName('QCom').AsFloat );
          FieldByName('ValorUnitario').AsString := FDANFEClassOwner.FormatarValorUnitario( FieldByName('VUnCom').AsFloat );
          FieldByName('vDesc').AsString         := FormatFloatBr( Prod.vDesc,',0.00');
        end;

        FieldByName('ORIGEM').AsString          := OrigToStr( Imposto.ICMS.orig);
        FieldByName('CST').AsString             := FDANFEClassOwner.ManterCst( FNFe.Emit.CRT , Imposto.ICMS.CSOSN , Imposto.ICMS.CST );
        FieldByName('VBC').AsString             := FormatFloatBr( Imposto.ICMS.vBC        ,',0.00');
        FieldByName('PICMS').AsString           := FormatFloatBr( Imposto.ICMS.pICMS      ,',0.00');
        FieldByName('VICMS').AsString           := FormatFloatBr( Imposto.ICMS.vICMS      ,',0.00');
        FieldByName('VBCST').AsString           := FormatFloatBr( Imposto.ICMS.vBcST      ,',0.00');
        FieldByName('pMVAST').AsString          := FormatFloatBr( Imposto.ICMS.pMVAST     ,',0.00');
        FieldByName('pICMSST').AsString         := FormatFloatBr( Imposto.ICMS.pICMSST    ,',0.00');
        FieldByName('VICMSST').AsString         := FormatFloatBr( Imposto.ICMS.vICMSST    ,',0.00');
        FieldByName('VIPI').AsString            := FormatFloatBr( Imposto.IPI.VIPI        ,',0.00');
        FieldByName('vIPIDevol').AsString       := FormatFloatBr( vIPIDevol               ,',0.00');
        FieldByName('PIPI').AsString            := FormatFloatBr( Imposto.IPI.PIPI        ,',0.00');
        FieldByName('vISSQN').AsString          := FormatFloatBr( Imposto.ISSQN.vISSQN    ,',0.00');
        FieldByName('vBcISSQN').AsString        := FormatFloatBr( Imposto.ISSQN.vBC       ,',0.00');

        FieldByName('ValorDescontos').AsString  := FormatFloatBr( FDANFEClassOwner.CalcularValorDescontoItem(FNFe, inItem), ',0.00');
        FieldByName('ValorLiquido').AsString    := FormatFloatBr( FDANFEClassOwner.CalcularValorLiquidoItem(FNFe, inItem), ',0.00');

        (*
        if FDANFEClassOwner.ExibeICMSDesoneradoComoDesconto then
        begin
          FieldByName('ValorDescontos').AsString  := FormatFloatBr( Prod.vDesc + Imposto.ICMS.vICMSDeson, ',0.00');
          FieldByName('ValorLiquido').AsString    := FormatFloatBr( Prod.vProd - Prod.vDesc - Imposto.ICMS.vICMSDeson + Prod.vOutro + Prod.vFrete + Prod.vSeg, ',0.00');
        end
        else
        begin
          FieldByName('ValorDescontos').AsString  := FormatFloatBr( Prod.vDesc, ',0.00');
          FieldByName('ValorLiquido').AsString    := FormatFloatBr( Prod.vProd - Prod.vDesc + Prod.vOutro + Prod.vFrete + Prod.vSeg, ',0.00');

        end;
        *)
        FieldByName('ValorAcrescimos').AsString   := FormatFloatBr( Prod.vOutro + Prod.vFrete + Prod.vSeg, ',0.00');
        FieldByName('xPed').AsString              := Prod.xPed;
        FieldByName('nItemPed').AsInteger         := Prod.nItem;
        Post;
      end;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaDestinatario;
begin
  { destinatário }
  with cdsDestinatario do
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

      FieldByName('IE').AsString        := IE;
      FieldByName('XNome').AsString     := XNome;
      with EnderDest do
      begin
        FieldByName('XLgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := FormatarCEP(CEP);
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;

      FieldByName('Consumidor').AsString := '';

      if (cdsIdentificacao.FieldByName('Mod_').AsString = '65') then
      begin
        if NaoEstaVazio(idEstrangeiro) then
          FieldByName('Consumidor').AsString := 'ESTRANGEIRO: ' + Trim(FieldByName('CNPJCPF').AsString) + ' ' + trim(FieldByName('XNome').AsString)
        else
        begin
          if EstaVazio(FieldByName('CNPJCPF').AsString) then
            FieldByName('Consumidor').AsString := ACBrStr('CONSUMIDOR NÃO IDENTIFICADO')
          else
            FieldByName('Consumidor').AsString :=
              IfThen(Length(CNPJCPF) = 11, 'CONSUMIDOR CPF: ', 'CONSUMIDOR CNPJ: ') + Trim(FieldByName('CNPJCPF').AsString) + ' ' + trim(FieldByName('XNome').AsString);
        end;

        if NaoEstaVazio(Trim(FieldByName('XLgr').AsString)) then
          FieldByName('Consumidor').AsString := FieldByName('Consumidor').AsString + #13 +
            Trim(FieldByName('XLgr').AsString) + ', ' + Trim(FieldByName('Nro').AsString);
        if NaoEstaVazio(Trim(FieldByName('XCpl').AsString)) then
          FieldByName('Consumidor').AsString := FieldByName('Consumidor').AsString + #13 +
            Trim(FieldByName('XCpl').AsString);

        if NaoEstaVazio(Trim(FieldByName('XMun').AsString)) then
          FieldByName('Consumidor').AsString := FieldByName('Consumidor').AsString + #13 +
            Trim(FieldByName('XBairro').AsString) + ' - ' +
            Trim(FieldByName('XMun').AsString) + '/' +
            Trim(FieldByName('UF').AsString);
      end;
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaDuplicatas;
var
  i: Integer;
begin
  cdsDuplicatas.Close;
  cdsDuplicatas.CreateDataSet;
  if (FDANFEClassOwner is TACBrNFeDANFEClass) and
     Not ((TACBrNFeDANFEClass(FDANFEClassOwner).ExibeCampoFatura) and
          (FNFe.Ide.indPag = ipVista) and (FNFe.infNFe.Versao <= 3.10)) then
  begin

    with cdsDuplicatas do
    begin
      for i := 0 to (NFe.Cobr.Dup.Count - 1) do
      begin
        Append;
        with FNFe.Cobr.Dup[i] do
        begin
          FieldByName('ChaveNFe').AsString  := FNFe.infNFe.ID;
          FieldByName('NDup').AsString      := NDup;
          FieldByName('DVenc').AsString     := FormatDateBr(DVenc);
          FieldByName('VDup').AsFloat       := VDup;
        end;
        Post;
      end;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaEmitente;
begin
  { emitente }
  with cdsEmitente do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Emit do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJouCPF(CNPJCPF);
      FieldByName('XNome').AsString := DANFEClassOwner.ManterNomeImpresso( XNome , XFant );
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
        FieldByName('CEP').AsString     := FormatarCEP(CEP);
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString        := IE;
      FieldByName('IM').AsString        := IM;
      FieldByName('IEST').AsString      := IEST;
      FieldByName('CRT').AsString       := CRTToStr(CRT);

      case CRT of
        crtSimplesNacional: FieldByName('DESCR_CST').AsString := 'CSOSN / CST';
        crtRegimeNormal, crtSimplesExcessoReceita: FieldByName('DESCR_CST').AsString := 'CST';
      end;

      cdsEmitente.FieldByName('DADOS_ENDERECO').AsString    := Trim(FieldByName('XLgr').AsString) + ', ' +
                                                                Trim(FieldByName('Nro').AsString);
	    if NaoEstaVazio(trim(FieldByName('XCpl').AsString)) then
        cdsEmitente.FieldByName('DADOS_ENDERECO').AsString  := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + ', ' +
                                                                Trim(FieldByName('XCpl').AsString);

      cdsEmitente.FieldByName('DADOS_ENDERECO').AsString    := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + ' - ' +
  										  	                                      Trim(FieldByName('XBairro').AsString) + ' - ' +
                                                                Trim(FieldByName('XMun').AsString) + ' - ' +
                                                                Trim(FieldByName('UF').AsString) +
                                                                ' - CEP: ' + Trim(FieldByName('CEP').AsString) + #13 +
		  	  				  				                                    ' Fone: ' + Trim(FieldByName('Fone').AsString);
      if NaoEstaVazio(Trim(FDANFEClassOwner.Site)) then
        cdsEmitente.FieldByName('DADOS_ENDERECO').AsString  := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + #13 +
                                                                trim(FDANFEClassOwner.Site);
      if NaoEstaVazio(Trim(FDANFEClassOwner.Email)) then
        cdsEmitente.FieldByName('DADOS_ENDERECO').AsString  := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + #13 +
                                                                Trim(FDANFEClassOwner.Email);
    end;

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaFatura;
begin
  with cdsFatura do
  begin
    Close;
    CreateDataSet;

    if (FDANFEClassOwner is TACBrNFeDANFEClass) and
       TACBrNFeDANFEClass(FDANFEClassOwner).ExibeCampoFatura then
    begin
      Append;

      FieldByName('iForma').asInteger := Integer( FNFe.Ide.indPag);

      if FNFe.infNFe.Versao >= 4 then
        FieldByName('Pagamento').AsString := ACBrStr('DADOS DA FATURA')
      else
      begin
        case FNFe.Ide.indPag of
          ipVista : FieldByName('Pagamento').AsString := ACBrStr('PAGAMENTO À VISTA');
          ipPrazo : FieldByName('Pagamento').AsString := ACBrStr('PAGAMENTO A PRAZO');
          ipOutras: FieldByName('Pagamento').AsString := ACBrStr('OUTROS');
        end;
      end;

      if NaoEstaVazio(FNFe.Cobr.Fat.nFat) then
      begin
        with FNFe.Cobr.Fat do
        begin
          FieldByName('nfat').AsString  := nFat;
          FieldByName('vOrig').AsFloat  := vOrig;
          FieldByName('vDesc').AsFloat  := vDesc;
          FieldByName('vLiq').AsFloat   := vLiq;
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

procedure TACBrNFeFRClass.CarregaPagamento;
var
  i: Integer;
  vTroco: Currency;
begin
  cdsPagamento.Close;
  cdsPagamento.CreateDataSet;

  if (FDANFEClassOwner is TACBrNFeDANFEFR) and
     (TACBrNFeDANFEFR(FDANFEClassOwner).ExibeCampoDePagamento <> eipQuadro) then
  begin
    Exit;
  end;

  for i := 0 to NFe.Pag.Count - 1 do
  begin
    cdsPagamento.Append;
    with FNFe.Pag[i] do
    begin
      if FDANFEClassOwner is TACBrNFeDANFCEClass then
        cdsPagamento.FieldByName('tPag').AsString := TACBrNFeDANFCEClass(FDANFEClassOwner).ManterDescricaoPagamentos(FNFe.pag[i])
      else
        cdsPagamento.FieldByName('tPag').AsString := FormaPagamentoToDescricao(tPag, xPag);
      cdsPagamento.FieldByName('vPag').AsFloat   := vPag;
      // ver tpIntegra
      cdsPagamento.FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);
      cdsPagamento.FieldByName('tBand').AsString := BandeiraCartaoToDescStr( tBand );
      cdsPagamento.FieldByName('cAut').AsString  := cAut;
    end;
    cdsPagamento.Post;
  end;

  // acrescenta o troco
  vTroco := FNFe.pag.vTroco;
  if (vTroco = 0) and (FDANFEClassOwner is TACBrNFeDANFCEClass) then
    vTroco := TACBrNFeDANFCEClass(FDANFEClassOwner).vTroco;

  if vTroco > 0 then
  begin
    cdsPagamento.Append;
    cdsPagamento.FieldByName('tPag').AsString  := 'Troco R$';
    cdsPagamento.FieldByName('vPag').AsFloat   := vTroco;
    cdsPagamento.Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaIdentificacao;
begin
  with cdsIdentificacao do
  begin
    Close;
    CreateDataSet;
    Append;

    FieldByName('Id').AsString      := OnlyNumber(FNFe.infNFe.Id);
    FieldByName('Versao').AsFloat   := FNFe.infNFe.versao;
    FieldByName('Chave').AsString   := FormatarChaveAcesso(FNFe.infNFe.Id);
    FieldByName('CUF').AsString     := IntToStr(FNFe.Ide.CUF);
    FieldByName('CNF').AsString     := IntToStr(FNFe.Ide.CNF);
    FieldByName('NatOp').AsString   := FNFe.Ide.NatOp;
    FieldByName('IndPag').AsString  := IndpagToStr(FNFe.Ide.IndPag );
    FieldByName('Mod_').AsString    := IntToStr(FNFe.Ide.Modelo);
    FieldByName('Serie').AsString   := IntToStr(FNFe.Ide.Serie);

    if TACBrNFeDANFEClass(FDANFEClassOwner).FormatarNumeroDocumento then
      FieldByName('NNF').AsString     := FormatarNumeroDocumentoFiscal(IntToStr(FNFe.Ide.NNF))
    else
      FieldByName('NNF').AsString     := IntToStr(FNFe.Ide.NNF);

    FieldByName('DEmi').AsString    := FormatDateBr(FNFe.Ide.DEmi);
    FieldByName('DSaiEnt').AsString := IfThen(FNFe.Ide.DSaiEnt <> 0, FormatDateBr(FNFe.Ide.DSaiEnt));
    FieldByName('TpNF').AsString    := tpNFToStr( FNFe.Ide.TpNF );
    FieldByName('CMunFG').AsString  := IntToStr(FNFe.Ide.CMunFG);
    FieldByName('TpImp').AsString   := TpImpToStr( FNFe.Ide.TpImp );
    FieldByName('TpEmis').AsString  := TpEmisToStr( FNFe.Ide.TpEmis );
    FieldByName('CDV').AsString     := IntToStr(FNFe.Ide.CDV);
    FieldByName('TpAmb').AsString   := TpAmbToStr( FNFe.Ide.TpAmb );
    FieldByName('FinNFe').AsString  := FinNFeToStr( FNFe.Ide.FinNFe );
    FieldByName('ProcEmi').AsString := procEmiToStr( FNFe.Ide.ProcEmi );
    FieldByName('VerProc').AsString := FNFe.Ide.VerProc;

    if FNFe.infNFe.versao = 2.00 then
      FieldByName('HoraSaida').AsString := ifthen(FNFe.ide.hSaiEnt = 0, '', TimeToStr(FNFe.ide.hSaiEnt))
    else
      FieldByName('HoraSaida').AsString := ifthen(TimeOf(FNFe.ide.dSaiEnt)=0, '', TimeToStr(FNFe.ide.dSaiEnt));

    if (FNFe.Ide.Modelo = 65) then
    begin
      FieldByName('DEmi').AsString := FormatDateTimeBr(FNFe.Ide.DEmi);

      if (FNFe.Ide.tpEmis <> teNormal) and EstaVazio(FNFe.procNFe.nProt) then
        FieldByName('MensagemFiscal').AsString := ACBrStr('EMITIDA EM CONTINGÊNCIA'+LineBreak+'Pendente de autorização');

      if FNFe.Ide.TpAmb = taHomologacao then
        FieldByName('MensagemFiscal').AsString := FieldByName('MensagemFiscal').AsString+LineBreak+LineBreak+ACBrStr('EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');

      //if EstaVazio(FieldByName('MensagemFiscal').AsString) then
      //  FieldByName('MensagemFiscal').AsString := ACBrStr('ÁREA DE MENSAGEM FISCAL');

      if EstaVazio(FNFe.infNFeSupl.urlChave) then
        FieldByName('URL').AsString := TACBrNFe(DANFEClassOwner.ACBrNFe).GetURLConsultaNFCe(FNFe.Ide.cUF, FNFe.Ide.tpAmb, FNFe.infNFe.Versao)
      else
        FieldByName('URL').AsString := FNFe.infNFeSupl.urlChave;

      FieldByName('MensagemFiscal').AsString := Trim(FieldByName('MensagemFiscal').AsString);
    end
    else
    begin
      FieldByName('MensagemFiscal').AsString := '';
      FieldByName('URL').AsString            := '';
      FieldByName('xPed').AsString           := FNFe.Compra.xPed;
    end;

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaInformacoesAdicionais;
var
  vTemp         : TStringList;
  IndexCampo    : Integer;
  Campos        : TSplitResult;
  BufferInfCpl  : string;
  wObs          : string;
  wLinhasObs    : integer;
  LContingencia : string;
  LTpEmis       : string;
begin
  wLinhasObs  := 0;
  BufferInfCpl:= '';
  vTemp       := TStringList.Create;

  try
    if (FDANFEClassOwner is TACBrNFeDANFEClass) then
    begin
      case FNFe.Ide.tpEmis of
        teSVCAN : LTpEmis := 'SVC-AN';
        teSVCRS : LTpEmis := 'SVC-RS';
      end;
      if FNFe.Ide.tpEmis in [teSVCAN, teSVCRS] then
      begin
        LContingencia := '<b>CONTINGÊNCIA %s Entrada em contingência em: %s com justificativa: %s.</b>';
        LContingencia := Format(LContingencia,[LTpEmis,
                                               DateTimeToStr(FNFe.Ide.dhCont),
                                               FNFe.Ide.xJust]) + sLineBreak;
      end;



      wObs := LContingencia +
              TACBrNFeDANFEClass(FDANFEClassOwner).ManterDocreferenciados(FNFe) +
              TACBrNFeDANFEClass(FDANFEClassOwner).ManterPagamentos(FNFe) +
              TACBrNFeDANFEClass(FDANFEClassOwner).ManterSuframa(FNFe) +
              FDANFEClassOwner.ManterInfAdFisco(FNFe) +
              FDANFEClassOwner.ManterObsFisco(FNFe) +
              FDANFEClassOwner.ManterProcreferenciado(FNFe) +
              FDANFEClassOwner.ManterInfContr(FNFe) +
              FDANFEClassOwner.ManterInfCompl(FNFe) +
              TACBrNFeDANFEClass(FDANFEClassOwner).ManterContingencia(FNFe);
    end
    else
    begin
      wObs := FDANFEClassOwner.ManterInfAdFisco(FNFe) +
              FDANFEClassOwner.ManterObsFisco(FNFe) +
              FDANFEClassOwner.ManterProcreferenciado(FNFe) +
              FDANFEClassOwner.ManterInfContr(FNFe) +
              FDANFEClassOwner.ManterInfCompl(FNFe);
    end;

    if Trim(wObs) <> '' then
    begin
      Campos := Split(';', wObs);
      for IndexCampo := 0 to Length(Campos) - 1 do
        vTemp.Add(Campos[IndexCampo]);

      wLinhasObs    := 1; //TotalObS(vTemp.Text);
      BufferInfCpl  := vTemp.Text;
    end;

    with cdsInformacoesAdicionais do
    begin
      Close;
      CreateDataSet;
      Append;
      FieldByName('OBS').AsString        := BufferInfCpl;
      FieldByName('LinhasOBS').AsInteger := wLinhasObs;
      FieldByName('MensagemSEFAZ').AsString := FNFe.procNFe.xMsg;
      Post;
    end;

  finally
    vTemp.Free;
  end;
end;

procedure TACBrNFeFRClass.CarregaISSQN;
begin
  with cdsISSQN do
  begin
    Close;
    CreateDataSet;
    Append;
    with FNFe.Total.ISSQNtot do
    begin
      FieldByName('vSERV').AsFloat        := VServ;
      FieldByName('vBC').AsFloat          := VBC;
      FieldByName('vISS').AsFloat         := VISS;
      FieldByName('vDescIncond').AsFloat  := vDescIncond;
      FieldByName('vISSRet').AsFloat      := vISSRet;
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaLocalEntrega;
begin
  { local de entrega }
  with cdsLocalEntrega do
  begin
    Close;
    CreateDataSet;

    if NaoEstaVazio(FNFe.Entrega.xLgr) then
    begin
      Append;

      with FNFe.Entrega do
      begin
        FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(CNPJCPF);;
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := inttostr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('XNome').AsString   := xNome;
        FieldByName('CEP').AsString     := FormatarCEP(CEP);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
        FieldByName('IE').AsString      := IE;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaLocalRetirada;
begin
  { local de retirada }
  with cdsLocalRetirada do
  begin
    Close;
    CreateDataSet;

    if NaoEstaVazio(FNFe.Retirada.xLgr) then
    begin
      Append;

      with FNFe.Retirada do
      begin
        FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(CNPJCPF);
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := inttostr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('XPais').AsString   := XPais;
        FieldByName('XNome').AsString   := xNome;
        FieldByName('CEP').AsString     := FormatarCEP(CEP);
        FieldByName('Fone').AsString    := FormatarFone(Fone);
        FieldByName('IE').AsString      := IE;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaParametros;
var
  vChave_Contingencia : String;
  vStream             : TMemoryStream;
  vStringStream       : TStringStream;
  P: Integer;

  procedure AdicionaMensagem0(const AMensagem : String);
  var LMensagem : String;
  begin
    LMensagem := cdsParametros.FieldByName('Mensagem0').AsString;
    if LMensagem <> '' then
      LMensagem := LMensagem + sLineBreak;
      
    LMensagem := LMensagem + ACBrStr( AMensagem );
    cdsParametros.FieldByName('Mensagem0').AsString := Trim( LMensagem );
  end;

begin
  { parâmetros }
  with cdsParametros do
  begin
    Close;
    CreateDataSet;
    Append;

    FieldByName('poscanhoto').AsString            := '';
    FieldByName('poscanhotolayout').AsString      := '';
    FieldByName('ResumoCanhoto').AsString         := '';
    FieldByName('Mensagem0').AsString             := '';
    FieldByName('Contingencia_ID').AsString       := '';
    FieldByName('ConsultaAutenticidade').AsString := 'Consulta de autenticidade no portal nacional da NF-e'+#13+
                                                     'www.nfe.fazenda.gov.br/portal ou no site da Sefaz autorizadora';

    FieldByName('ExibeICMSDesoneradoComoDesconto').AsBoolean := DANFEClassOwner.ExibeICMSDesoneradoComoDesconto;

    if DANFEClassOwner is TACBrNFeDANFEClass then
    begin
      FieldByName('poscanhoto').AsString     := IntToStr( Ord(TACBrNFeDANFEClass(DANFEClassOwner).PosCanhoto));
      FieldByName('poscanhotolayout').AsString  := IntToStr( Ord(TACBrNFeDANFEClass(DANFEClassOwner).PosCanhotoLayout));
    end
    else if DANFEClassOwner is TACBrNFeDANFCEClass then
    begin
      if TACBrNFeDANFCEClass(DANFEClassOwner).ViaConsumidor then
        FieldByName('DescricaoViaEstabelec').AsString := 'Via Consumidor'
      else
        FieldByName('DescricaoViaEstabelec').AsString := 'Via Estabelecimento';
    end;

    if Assigned(FNFe) and (FNFe.InfNFe.ID <> '') then
    begin
      if (DANFEClassOwner is TACBrNFeDANFEClass) and TACBrNFeDANFEClass(DANFEClassOwner).ExibeResumoCanhoto then
      begin
         if EstaVazio(TACBrNFeDANFEClass(DANFEClassOwner).TextoResumoCanhoto) then
          FieldByName('ResumoCanhoto').AsString := ACBrStr('Emissão: ' )+ FormatDateBr(FNFe.Ide.DEmi) + '  Dest/Reme: ' + FNFe.Dest.XNome + '  Valor Total: ' + FormatFloatBr(FNFe.Total.ICMSTot.VNF)
        else
          FieldByName('ResumoCanhoto').AsString := TACBrNFeDANFEClass(DANFEClassOwner).TextoResumoCanhoto;
      end;

      if (FNFe.Ide.TpAmb = taHomologacao) then
        AdicionaMensagem0('SEM VALOR FISCAL');

      if (FNFe.procNFe.cStat in [101,135, 151, 155]) or TACBrNFeDANFEClass(DANFEClassOwner).Cancelada then
        AdicionaMensagem0('NFE CANCELADA');

      if ( FNFe.procNFe.cStat = 110 ) or
         ( FNFe.procNFe.cStat = 301 )  or
         ( FNFe.procNFe.cStat = 302 )  or
         ( FNFe.procNFe.cStat = 303 ) then
        AdicionaMensagem0('NFe DENEGADA pelo Fisco');

      if ((EstaVazio(FDANFEClassOwner.Protocolo)) and (EstaVazio(FNFe.procNFe.nProt))) then
      AdicionaMensagem0('Sem Autorização de Uso da SEFAZ');

      if (FNFe.Ide.tpImp = tiSimplificado) then
        AdicionaMensagem0( 'EMISSÃO NORMAL' );

      case FNFe.Ide.tpEmis of
        teNormal,
        teSVCAN,
        teSCAN,
        teSVCRS,
        teSVCSP :   begin
                      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
                      FieldByName('Contingencia_ID').AsString := '';

                      if ((FDANFEClassOwner.Cancelada) or (FNFe.procNFe.cStat in [101,151,155])) then
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO' )
                      else if ( FNFe.procNFe.cStat = 110 ) or
                              ( FNFe.procNFe.cStat = 301 ) or
                              ( FNFe.procNFe.cStat = 302 ) or
                              ( FNFe.procNFe.cStat = 303 ) then
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO')
                      else
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');

                      if EstaVazio(FDANFEClassOwner.Protocolo) then
                      begin
                        if EstaVazio(FNFe.procNFe.nProt) then
                          FieldByName('Contingencia_Valor').AsString := ACBrStr('NFe sem Autorização de Uso da SEFAZ')
                        else
                        begin
                          FieldByName('Contingencia_Valor').AsString := FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(FNFe.procNFe.dhRecbto), '');
                          FieldByName('nProt').AsString := FNFe.procNfe.nProt;
                          FieldByName('dhRecbto').AsDateTime := FNFe.procNFe.dhRecbto;
                        end;
                      end
                      else
                      begin
                        FieldByName('Contingencia_Valor').AsString := FDANFEClassOwner.Protocolo;
                        P := Pos('-', FDANFEClassOwner.Protocolo);
                        if P = 0 then
                        begin
                          FieldByName('nProt').AsString := Trim(FDANFEClassOwner.Protocolo);
                          FieldByName('dhRecbto').AsDateTime := 0;
                        end
                        else
                        begin
                          FieldByName('nProt').AsString := Trim(Copy(FDANFEClassOwner.Protocolo, 1, P - 1));
                          FieldByName('dhRecbto').AsDateTime := StringToDateTimeDef(Trim(
                            Copy(FDANFEClassOwner.Protocolo, P + 1, Length(FDANFEClassOwner.Protocolo) - P)
                            ), 0, 'dd/mm/yyyy hh:nn:ss');
                        end;
                      end;
                    end;

        teContingencia ,
        teFSDA :    begin
                      vChave_Contingencia := TACBrNFe(DANFEClassOwner.ACBrNFe).GerarChaveContingencia(FNFe);
                      FieldByName('ChaveAcesso_Descricao').AsString  := 'CHAVE DE ACESSO';
                      FieldByName('Contingencia_ID').AsString        := vChave_Contingencia;
                      FieldByName('Contingencia_Descricao').AsString := 'DADOS DA NF-E';
                      FieldByName('Contingencia_Valor').AsString     := FormatarChaveAcesso(vChave_Contingencia);
                      FieldByName('ConsultaAutenticidade').AsString  := '';
                    end;

         teDPEC  :  begin
                      if NaoEstaVazio(FNFe.procNFe.nProt) then // DPEC TRANSMITIDO
                      begin
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr( 'PROTOCOLO DE AUTORIZAÇÃO DE USO');
                        FieldByName('Contingencia_Valor').AsString     := FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(FNFe.procNFe.dhRecbto), '');
                      end
                      else
                      begin
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('NÚMERO DE REGISTRO DPEC');
                        if NaoEstaVazio(FDANFEClassOwner.Protocolo) then
                          FieldByName('Contingencia_Valor').AsString := FDANFEClassOwner.Protocolo;
                      end;
                    end;

         teOffLine: begin
                      FieldByName('Contingencia_Valor').AsString := FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(FNFe.procNFe.dhRecbto), '');
                      FieldByName('nProt').AsString := FNFe.procNfe.nProt;
                      FieldByName('dhRecbto').AsDateTime := FNFe.procNFe.dhRecbto;
                    end;
      end;

      FieldByName('QtdeItens').AsInteger := NFe.Det.Count;

    end;

    if NaoEstaVazio(FieldByName('Mensagem0').AsString) then
      FieldByName('Mensagem0').AsString  := FieldByName('Mensagem0').AsString+#10#13;

    FieldByName('Mensagem0').AsString                   := FieldByName('Mensagem0').AsString + IfThen(FDANFEClassOwner is TACBrNFeDANFEFR, TACBrNFeDANFEFR(FDANFEClassOwner).MarcaDaguaMSG, '');
    FieldByName('LogoExpandido').AsString               := IfThen( FDANFEClassOwner.ExpandeLogoMarca, '1' , '0' );
    FieldByName('Sistema').AsString                     := IfThen( FDANFEClassOwner.Sistema <> '' , FDANFEClassOwner.Sistema, 'Projeto ACBr - http://acbr.sf.net');
    FieldByName('Usuario').AsString                     := IfThen( FDANFEClassOwner.Usuario <> '' , ' - ' + FDANFEClassOwner.Usuario , '' );
    FieldByName('Site').AsString                        := FDANFEClassOwner.Site;
    FieldByName('Email').AsString                       := FDANFEClassOwner.Email;
    FieldByName('Desconto').AsString                    := IfThen( (FDANFEClassOwner is TACBrNFeDANFEClass) and TACBrNFeDANFEClass(FDANFEClassOwner).ImprimeDescPorPercentual , '%' , 'VALOR');
    FieldByName('TotalLiquido').AsString                := IfThen( FDANFEClassOwner.ImprimeTotalLiquido ,ACBrStr('LÍQUIDO') ,'TOTAL');
    FieldByName('LinhasPorPagina').AsInteger            := 0;
    FieldByName('ExpandirDadosAdicionaisAuto').AsString := IfThen(TACBrNFeDANFEFR(FDANFEClassOwner).ExpandirDadosAdicionaisAuto , 'S' , 'N');
    FieldByName('sDisplayFormat').AsString              := ',0.%.*d';
    FieldByName('iFormato').AsInteger                   := integer( FDANFEClassOwner.CasasDecimais.Formato );
    FieldByName('Mask_qCom').AsString                   := FDANFEClassOwner.CasasDecimais.MaskqCom;
    FieldByName('Mask_vUnCom').AsString                 := FDANFEClassOwner.CasasDecimais.MaskvUnCom;
    FieldByName('Casas_qCom').AsInteger                 := FDANFEClassOwner.CasasDecimais.qCom;
    FieldByName('Casas_vUnCom').AsInteger               := FDANFEClassOwner.CasasDecimais.vUnCom;

    if (FDANFEClassOwner is TACBrNFeDANFCEClass) then
    begin
      FieldByName('ImprimeEm1Linha').AsString        := IfThen( TACBrNFeDANFCEClass(FDANFEClassOwner).ImprimeEmUmaLinha, 'S', 'N');
      FieldByName('ImprimeEmDuasLinhas').AsString    := IfThen( TACBrNFeDANFCEClass(FDANFEClassOwner).ImprimeEmDuasLinhas, 'S', 'N');
      FieldByName('QrCodeLateral').AsString          := IfThen( TACBrNFeDANFCEClass(FDANFEClassOwner).ImprimeQRCodeLateral, 'S', 'N');
      FieldByName('ImprimeDescAcrescItem').AsInteger := IfThen( TACBrNFeDANFCEClass(FDANFEClassOwner).ImprimeDescAcrescItem, 1 , 0 );
    end;

    if (FDANFEClassOwner is TACBrNFeDANFEClass) then
    begin
      FieldByName('ImprimeDescAcrescItem').AsInteger := Integer(TACBrNFeDANFEClass(FDANFEClassOwner).ImprimeDescAcrescItem);
    end;

    // Carregamento da imagem
    if NaoEstaVazio(DANFEClassOwner.Logo) then
    begin
      FieldByName('Imagem').AsString := DANFEClassOwner.Logo;
      vStream := TMemoryStream.Create;
      try
        if FileExists(DANFEClassOwner.Logo) then
          vStream.LoadFromFile(DANFEClassOwner.Logo)
        else
        begin
          vStringStream:= TStringStream.Create(DANFEClassOwner.Logo);
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

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaTransportador;
var
  ok: Boolean;
begin
  with cdsTransportador do
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
        FieldByName('XNome').AsString   := XNome;
        FieldByName('IE').AsString      := IE;
        FieldByName('XEnder').AsString  := XEnder;
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
      end;
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaVeiculo;
begin
  with cdsVeiculo do
  begin
    Close;
    CreateDataSet;
    Append;
    with FNFe.Transp.VeicTransp do
    begin
      FieldByName('PLACA').AsString := Placa;
      FieldByName('UF').AsString    := UF;
      FieldByName('RNTC').AsString  := RNTC;
    end;

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaVolumes;
var
  i: Integer;
begin
  with cdsVolumes do
  begin
    Close;
    CreateDataSet;
    for i := 0 to NFe.Transp.Vol.Count - 1 do
    begin
      Append;
      with FNFe.Transp.Vol[i] do
      begin
        FieldByName('QVol').AsFloat   := QVol;
        FieldByName('Esp').AsString   := Esp;
        FieldByName('Marca').AsString := Marca;
        FieldByName('NVol').AsString  := NVol;
        FieldByName('PesoL').AsFloat  := PesoL;
        FieldByName('PesoB').AsFloat  := PesoB;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaDadosEventos;
var
  i: Integer;
  CondicoesUso, Correcao: String;
begin
  with cdsEventos do
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
        FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);

        // nota fiscal eletronica
        FieldByName('Modelo').AsString      := Copy(InfEvento.chNFe, 21, 2);
        FieldByName('Serie').AsString       := Copy(InfEvento.chNFe, 23, 3);
        FieldByName('Numero').AsString      := Copy(InfEvento.chNFe, 26, 9);
        FieldByName('MesAno').AsString      := Copy(InfEvento.chNFe, 05, 2) + '/' + copy(InfEvento.chNFe, 03, 2);
        FieldByName('Barras').AsString      := InfEvento.chNFe;
        FieldByName('ChaveAcesso').AsString := FormatarChaveAcesso(InfEvento.chNFe);

        // Carta de correção eletrônica
        FieldByName('cOrgao').AsInteger := InfEvento.cOrgao;

        case InfEvento.tpAmb of
          taProducao:    FieldByName('tpAmb').AsString := ACBrStr('PRODUÇÃO');
          taHomologacao: FieldByName('tpAmb').AsString := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
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

        if InfEvento.tpEvento <> teCCe then
        begin
          FieldByName('xJust').AsString := InfEvento.detEvento.xJust;
        end
        else
        begin
          CondicoesUso := InfEvento.detEvento.xCondUso;
          CondicoesUso := StringReplace(CondicoesUso, 'com: I', 'com:'+#13+' I', [rfReplaceAll]);
          CondicoesUso := StringReplace(CondicoesUso, ';', ';' + #13, [rfReplaceAll]);

          Correcao := StringReplace(InfEvento.detEvento.xCorrecao,
            TACBrNFe(DANFEClassOwner.ACBrNFe).Configuracoes.WebServices.QuebradeLinha, #13,
             [rfReplaceAll]);

          FieldByName('xCondUso').AsString  := CondicoesUso;
          FieldByName('xCorrecao').AsString := Correcao;
        end;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaDadosInutilizacao;
begin
   CarregaParametros;

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
         FieldByName('ID').AsString         := OnlyNumber(ID);
         FieldByName('CNPJ').AsString       := FormatarCNPJ(CNPJ);
         FieldByName('nProt').AsString      := nProt;
         FieldByName('Modelo').AsInteger    := Modelo;
         FieldByName('Serie').AsInteger     := Serie;
         FieldByName('Ano').AsInteger       := Ano;
         FieldByName('nNFIni').AsInteger    := nNFIni;
         FieldByName('nNFFin').AsInteger    := nNFFin;
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

function TACBrNFeFRClass.PrepareReport(ANFE: TNFe): Boolean;
var
  I: Integer;
  wProjectStream: TStringStream;
begin
  Result := False;

  SetDataSetsToFrxReport;

  if NaoEstaVazio(Trim(FastFile)) then
  begin
    if not (uppercase(copy(FastFile,length(FastFile)-3,4))='.FR3') then
    begin
      wProjectStream := TStringStream.Create(FastFile);
      frxReport.FileName := '';
      frxReport.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(FastFile) then
        frxReport.LoadFromFile(FastFile)
      else
        raise EACBrNFeDANFEFR.CreateFmt('Caminho do arquivo de impressão do DANFE "%s" inválido.', [FastFile]);
    end;
  end
  else
    raise EACBrNFeDANFEFR.Create('Caminho do arquivo de impressão do DANFE não assinalado.');

  frxReport.PrintOptions.Copies := DANFEClassOwner.NumCopias;
  frxReport.PrintOptions.ShowDialog := DANFEClassOwner.MostraSetup;
  frxReport.PrintOptions.PrintMode := FPrintMode; //Precisamos dessa propriedade porque impressoras não fiscais cortam o papel quando há muitos itens. O ajuste dela deve ser necessariamente após a carga do arquivo FR3 pois, antes da carga o componente é inicializado
  frxReport.PrintOptions.PrintOnSheet := FPrintOnSheet; //Essa propriedade pode trabalhar em conjunto com a printmode
  frxReport.ShowProgress := DANFEClassOwner.MostraStatus;
  frxReport.PreviewOptions.AllowEdit := False;
  frxReport.PreviewOptions.ShowCaptions := FExibeCaptionButton;
  frxReport.PreviewOptions.ZoomMode     := FZoomModePadrao;
  frxReport.OnPreview := frxReportPreview;

  if FThreadSafe then
  begin
    // Desabilita todo e qualquer tipo de mensagem
    frxReport.EngineOptions.SilentMode := True;
    // Habilita o FR a trabalhar com multiplas threads com segurança
    frxReport.EngineOptions.EnableThreadSafe := True;
    // Desabilita o cache, que no caso de múltiplas threas pode dar conflito de conteúdo entre arquivos.
    frxReport.EngineOptions.UseFileCache := false;
  end;

  if NaoEstaVazio(DANFEClassOwner.NomeDocumento) then
    frxReport.FileName := DANFEClassOwner.NomeDocumento;

  // Define a impressora
  if NaoEstaVazio(DANFEClassOwner.Impressora) then
    frxReport.PrintOptions.Printer := DANFEClassOwner.Impressora;

  // preparar relatorio
  if Assigned(ANFE) then
  begin
    NFe := ANFE;
    CarregaDadosNFe;

    Result := frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(DANFEClassOwner.ACBrNFe) then
    begin
      if DANFEClassOwner.FIndexImpressaoIndividual > 0  then
      begin
        NFe := TACBrNFe(DANFEClassOwner.ACBrNFe).NotasFiscais[DANFEClassOwner.FIndexImpressaoIndividual].NFe;
        CarregaDadosNFe;
        Result := frxReport.PrepareReport( DANFEClassOwner.FIndexImpressaoIndividual > 0 );
      end else
      begin
        for i := 0 to (TACBrNFe(DANFEClassOwner.ACBrNFe).NotasFiscais.Count - 1) do
        begin
          NFe := TACBrNFe(DANFEClassOwner.ACBrNFe).NotasFiscais[I].NFe;
          CarregaDadosNFe;

          //Result := frxReport.PrepareReport( not (i > 0) );
          Result := frxReport.PrepareReport( false );
        end;
      end;
    end
    else
      raise EACBrNFeDANFEFR.Create('Propriedade ACBrNFe não assinalada.');
  end;

  if Assigned(NFe) then
  begin
    AjustaMargensReports;
  end;

end;

function TACBrNFeFRClass.PrepareReportEvento(ANFE: TNFe = nil): Boolean;
var
 wProjectStream: TStringStream;
begin
  SetDataSetsToFrxReport;
  if NaoEstaVazio(Trim(FastFileEvento)) then
  begin
    if not (uppercase(copy(FastFileEvento,length(FastFileEvento)-3,4))='.FR3') then
    begin
      wProjectStream:=TStringStream.Create(FastFileEvento);
      frxReport.FileName := '';
      frxReport.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(FastFileEvento) then
        frxReport.LoadFromFile(FastFileEvento)
      else
        raise EACBrNFeDANFEFR.CreateFmt('Caminho do arquivo de impressão do EVENTO "%s" inválido.', [FastFileEvento]);
    end
  end
  else
    raise EACBrNFeDANFEFR.Create('Caminho do arquivo de impressão do EVENTO não assinalado.');

  frxReport.PrintOptions.Copies := DANFEClassOwner.NumCopias;
  frxReport.PrintOptions.ShowDialog := DANFEClassOwner.MostraSetup;
  frxReport.ShowProgress := DANFEClassOwner.MostraStatus;
  frxReport.PreviewOptions.ShowCaptions := ExibeCaptionButton;
  frxReport.PreviewOptions.ZoomMode     := ZoomModePadrao;
  frxReport.OnPreview := frxReportPreview;

  if NaoEstaVazio(DANFEClassOwner.NomeDocumento) then
    frxReport.FileName := DANFEClassOwner.NomeDocumento;

  // Define a impressora
  if NaoEstaVazio(DANFEClassOwner.Impressora) then
    frxReport.PrintOptions.Printer := DANFEClassOwner.Impressora;

  // preparar relatorio
  if Assigned(DANFEClassOwner.ACBrNFe) then
  begin
    if Assigned(TACBrNFe(DANFEClassOwner.ACBrNFe).EventoNFe) then
    begin
      Evento := TACBrNFe(DANFEClassOwner.ACBrNFe).EventoNFe;
      CarregaDadosEventos;
    end
    else
      raise EACBrNFeDANFEFR.Create('Evento não foi assinalado.');

    NFe := nil;

    if Assigned(ANFE) then
      NFe := ANFE
    else
    begin
      if (TACBrNFe(DANFEClassOwner.ACBrNFe).NotasFiscais.Count > 0) then
        NFe := TACBrNFe(DANFEClassOwner.ACBrNFe).NotasFiscais.Items[0].NFe;
    end;

    if Assigned(NFe) then
    begin
      frxReport.Variables['PossuiNFe'] := QuotedStr('S');
      CarregaDadosNFe;
    end;

    Result := frxReport.PrepareReport;
  end
  else
    raise EACBrNFeDANFEFR.Create('Propriedade ACBrNFe não assinalada.');

  AjustaMargensReports;

end;

function TACBrNFeFRClass.PrepareReportInutilizacao: Boolean;
var
 wProjectStream: TStringStream;
begin
  SetDataSetsToFrxReport;
  if NaoEstaVazio(Trim(FastFileInutilizacao)) then
  begin
    if not (uppercase(copy(FastFileInutilizacao,length(FastFileInutilizacao)-3,4))='.FR3') then
    begin
      wProjectStream:=TStringStream.Create(FastFileInutilizacao);
      frxReport.FileName := '';
      frxReport.LoadFromStream(wProjectStream);
      wProjectStream.Free;
    end
    else
    begin
      if FileExists(FastFileInutilizacao) then
        frxReport.LoadFromFile(FastFileInutilizacao)
      else
        raise EACBrNFeDANFEFR.CreateFmt('Caminho do arquivo de impressão de INUTILIZAÇÃO "%s" inválido.', [FastFileInutilizacao]);
    end
  end
  else
    raise EACBrNFeDANFEFR.Create('Caminho do arquivo de impressão de INUTILIZAÇÃO não assinalado.');

  frxReport.PrintOptions.Copies := DANFEClassOwner.NumCopias;
  frxReport.PrintOptions.ShowDialog := DANFEClassOwner.MostraSetup;
  frxReport.ShowProgress := DANFEClassOwner.MostraStatus;
  frxReport.PreviewOptions.ShowCaptions := ExibeCaptionButton;
  frxReport.PreviewOptions.ZoomMode     := ZoomModePadrao;
  frxReport.OnPreview := frxReportPreview;

  if NaoEstaVazio(DANFEClassOwner.NomeDocumento) then
    frxReport.FileName := DANFEClassOwner.NomeDocumento;

  // Define a impressora
  if NaoEstaVazio(DANFEClassOwner.Impressora) then
    frxReport.PrintOptions.Printer := DANFEClassOwner.Impressora;

  // preparar relatorio
  if Assigned(DANFEClassOwner.ACBrNFe) then
  begin
    if Assigned(TACBrNFe(DANFEClassOwner.ACBrNFe).InutNFe) then
    begin
      Inutilizacao := TACBrNFe(DANFEClassOwner.ACBrNFe).InutNFe.RetInutNFe;
      CarregaDadosInutilizacao;
    end
    else
      raise EACBrNFeDANFEFR.Create('INUTILIZAÇÃO não foi assinalada.');

    Result := frxReport.PrepareReport;
  end
  else
    raise EACBrNFeDANFEFR.Create('Propriedade ACBrNFe não assinalada.');

  AjustaMargensReports;

end;

procedure TACBrNFeFRClass.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  qrcode: String;
  CpTituloReport, CpLogomarca, CpDescrProtocolo, CpTotTrib, CpContingencia1, CpContingencia2 : TfrxComponent;
begin

  qrCode := '';
  if Assigned(NFe) then
  begin
    case NFe.Ide.modelo of
      55 :  case FNFe.Ide.tpImp of
              tiSimplificado :
                begin
                  CpTituloReport := frxReport.FindObject('PageHeader1');
                  if Assigned(CpTituloReport) then
                    CpTituloReport.Visible  := ( cdsParametros.FieldByName('Imagem').AsString <> '' );

                  CpLogomarca := frxReport.FindObject('ImgLogo');
                  if Assigned(CpLogomarca) and Assigned(CpTituloReport) then
                    CpLogomarca.Visible := CpTituloReport.Visible;
                end;
            end;

      65 :  begin
              CpTituloReport := frxReport.FindObject('ReportTitle1');
              if Assigned(CpTituloReport) then
                CpTituloReport.Visible := cdsParametros.FieldByName('Imagem').AsString <> '';

              CpLogomarca := frxReport.FindObject('ImgLogo');
              if Assigned(CpLogomarca) and Assigned(CpTituloReport) then
                CpLogomarca.Visible := CpTituloReport.Visible;

              if EstaVazio(Trim(NFe.infNFeSupl.qrCode)) then
                qrcode := TACBrNFe(DANFEClassOwner.ACBrNFe).GetURLQRCode(
                       NFe.ide.cUF,
                       NFe.ide.tpAmb,
                       OnlyNumber(NFe.InfNFe.ID),
                       IfThen(NFe.Dest.idEstrangeiro <> '',NFe.Dest.idEstrangeiro, NFe.Dest.CNPJCPF),
                       NFe.ide.dEmi,
                       NFe.Total.ICMSTot.vNF,
                       NFe.Total.ICMSTot.vICMS,
                       NFe.signature.DigestValue,
                       NFe.infNFe.Versao)
              else
                qrcode := NFe.infNFeSupl.qrCode;

              if Assigned(Sender) and (LeftStr(Sender.Name, 9) = 'ImgQrCode') then
                PintarQRCode(qrcode, TfrxPictureView(Sender).Picture{$IFNDEF FMX}.Bitmap{$ENDIF}, qrUTF8NoBOM);

              CpDescrProtocolo := frxReport.FindObject('Memo25');
              if Assigned(CpDescrProtocolo) then
                CpDescrProtocolo.Visible := cdsParametros.FieldByName('Contingencia_Valor').AsString <> '';

              CpTotTrib := frxReport.FindObject('ValorTributos');
              if Assigned(CpTotTrib) then
                CpTotTrib.Visible := cdsCalculoImposto.FieldByName('VTotTrib').AsFloat > 0;

              // ajusta Informação de contingência no NFCe
              CpContingencia1 := frxReport.FindObject('ChildContingenciaCabecalho');
              if Assigned(CpContingencia1) then
                CpContingencia1.Visible := FNFe.Ide.tpEmis <> teNormal;

              CpContingencia2 := frxReport.FindObject('ChildContingenciaIdentificacao');
              if Assigned(CpContingencia2) then
                CpContingencia2.Visible := FNFe.Ide.tpEmis <> teNormal;
            end;
    end;
  end;
end;

procedure TACBrNFeFRClass.frxReportPreview(Sender: TObject);
begin
 {$IFNDEF FMX}
 frxReport.PreviewForm.BorderIcons := FBorderIcon;
 {$ENDIF}
end;

function TACBrNFeFRClass.GetPreparedReport: TfrxReport;
begin

  if EstaVazio(Trim(FFastFile)) then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := frxReport
    else
      Result := nil;
  end;

end;

function TACBrNFeFRClass.GetPreparedReportEvento: TfrxReport;
begin

  if EstaVazio(Trim(FFastFileEvento)) then
    Result := nil
  else
  begin
    if PrepareReportEvento then
      Result := frxReport
    else
      Result := nil;
  end;

end;

function TACBrNFeFRClass.GetPreparedReportInutilizacao: TfrxReport;
begin

  if EstaVazio(Trim(FFastFileInutilizacao)) then
    Result := nil
  else
  begin
    if PrepareReportInutilizacao then
      Result := frxReport
    else
      Result := nil;
  end;

end;

procedure TACBrNFeFRClass.ImprimirDANFE(ANFE: TNFe);
begin
  if PrepareReport(ANFE) then
  begin
    if DANFEClassOwner.MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrNFeFRClass.ImprimirDANFEPDF(ANFE: TNFe = nil; AStream: TStream = nil);
const
  TITULO_PDF = 'Nota Fiscal Eletrônica';
var
  fsShowDialog : Boolean;
  NomeArq: String;
  I : Integer;
begin
  for I := 0 to TACBrNFe(DANFEClassOwner.ACBrNFe).NotasFiscais.Count -1 do
  begin
    DANFEClassOwner.FIndexImpressaoIndividual := I;
    if PrepareReport(ANFE) then
    begin
      if (AStream <> nil) then
        frxPDFExport.Stream := AStream;

      frxPDFExport.Author        := DANFEClassOwner.Sistema;
      frxPDFExport.Creator       := DANFEClassOwner.Sistema;
      frxPDFExport.Producer      := DANFEClassOwner.Sistema;
      frxPDFExport.Title         := TITULO_PDF;
      frxPDFExport.Subject       := TITULO_PDF;
      frxPDFExport.Keywords      := TITULO_PDF;
      frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;
      frxPDFExport.Background    := IncorporarBackgroundPdf;
      frxPDFExport.PrintOptimized := OtimizaImpressaoPdf;

      fsShowDialog := frxPDFExport.ShowDialog;
      try
        frxPDFExport.ShowDialog := False;

        //NomeArq := Trim(DANFEClassOwner.NomeDocumento);
        //if EstaVazio(NomeArq) then
        //  NomeArq := OnlyNumber(NFe.infNFe.ID) + '-nfe.pdf';
        //frxPDFExport.FileName := PathWithDelim(DANFEClassOwner.PathPDF) +	NomeArq;


        frxPDFExport.FileName := DefinirNomeArquivo(DANFEClassOwner.PathPDF,
                                 OnlyNumber(NFe.infNFe.ID) + '-nfe.pdf',
                                 DANFEClassOwner.NomeDocumento);

        if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
          ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

        frxReport.Export(frxPDFExport);
      finally
        frxPDFExport.ShowDialog := fsShowDialog;
      end;
    end
    else
      frxPDFExport.FileName := '';
  end;
end;

procedure TACBrNFeFRClass.ImprimirDANFEResumido(ANFE: TNFe);
begin
  if PrepareReport(ANFE) then
  begin
    if DANFEClassOwner.MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrNFeFRClass.ImprimirEVENTO(ANFE: TNFe);
begin
  if PrepareReportEvento then
  begin
    if DANFEClassOwner.MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrNFeFRClass.ImprimirEVENTOPDF(ANFE: TNFe = nil; AStream: TStream = nil);
const
  TITULO_PDF = 'Eventos Nota Fiscal Eletrônica';
var
  NomeArq: String;
  fsShowDialog: Boolean;
begin
  if PrepareReportEvento(ANFE) then
  begin
    if (AStream <> nil) then
      frxPDFExport.Stream := AStream;
    frxPDFExport.Author        := DANFEClassOwner.Sistema;
    frxPDFExport.Creator       := DANFEClassOwner.Sistema;
    frxPDFExport.Producer      := DANFEClassOwner.Sistema;
    frxPDFExport.Title         := TITULO_PDF;
    frxPDFExport.Subject       := TITULO_PDF;
    frxPDFExport.Keywords      := TITULO_PDF;
    frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;
    frxPDFExport.Background    := IncorporarBackgroundPdf;
    frxPDFExport.PrintOptimized := OtimizaImpressaoPdf;

    fsShowDialog := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := False;
      NomeArq := Trim(DANFEClassOwner.NomeDocumento);
      if EstaVazio(NomeArq) then
        NomeArq := OnlyNumber(TACBrNFe(DANFEClassOwner.ACBrNFe).EventoNFe.Evento.Items[0].InfEvento.ID) + '-procEventoNFe.pdf';
      frxPDFExport.FileName := PathWithDelim(DANFEClassOwner.PathPDF) + NomeArq;

      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
        ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := fsShowDialog;
    end;
  end
  else
    frxPDFExport.Filename := '';

end;

procedure TACBrNFeFRClass.ImprimirINUTILIZACAO(ANFE: TNFe);
begin
  if PrepareReportInutilizacao then
  begin
    if DANFEClassOwner.MostraPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrNFeFRClass.ImprimirINUTILIZACAOPDF(ANFE: TNFe = nil; AStream: TStream = nil);
const
  TITULO_PDF = 'Inutilização de Numeração';
var
  NomeArq: String;
  fsShowDialog: Boolean;
begin
  if PrepareReportInutilizacao then
  begin
    if (AStream <> nil) then
      frxPDFExport.Stream := AStream;
    frxPDFExport.Author        := DANFEClassOwner.Sistema;
    frxPDFExport.Creator       := DANFEClassOwner.Sistema;
    frxPDFExport.Producer      := DANFEClassOwner.Sistema;
    frxPDFExport.Title         := TITULO_PDF;
    frxPDFExport.Subject       := TITULO_PDF;
    frxPDFExport.Keywords      := TITULO_PDF;
    frxPDFExport.EmbeddedFonts := IncorporarFontesPdf;
    frxPDFExport.Background    := IncorporarBackgroundPdf;
    frxPDFExport.PrintOptimized := OtimizaImpressaoPdf;

    fsShowDialog := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := False;
      NomeArq := Trim(DANFEClassOwner.NomeDocumento);
      if EstaVazio(NomeArq) then
        NomeArq := OnlyNumber(TACBrNFe(DANFEClassOwner.ACBrNFe).InutNFe.RetInutNFe.Id) + '-procInutNFe.pdf';
      frxPDFExport.FileName := PathWithDelim(DANFEClassOwner.PathPDF) + NomeArq;

      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
        ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := fsShowDialog;
    end;
  end
  else
    frxPDFExport.FileName := '';

end;

procedure TACBrNFeFRClass.AjustaMargensReports;
var
  Page: TfrxReportPage;
  I: Integer;
begin
  for I := 0 to (frxReport.PreviewPages.Count - 1) do
  begin
    Page := frxReport.PreviewPages.Page[I];
    if (DANFEClassOwner.MargemSuperior > 0) then
      Page.TopMargin := DANFEClassOwner.MargemSuperior;
    if (DANFEClassOwner.MargemInferior > 0) then
      Page.BottomMargin := DANFEClassOwner.MargemInferior;
    if (DANFEClassOwner.MargemEsquerda > 0) then
      Page.LeftMargin := DANFEClassOwner.MargemEsquerda;
    if (DANFEClassOwner.MargemDireita > 0) then
      Page.RightMargin := DANFEClassOwner.MargemDireita;
    frxReport.PreviewPages.ModifyPage(I, Page);
  end;
end;

end.
