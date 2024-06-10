{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti, Victor Hugo Gonzales        }
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

unit ACBrSATExtratoFR;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrBase, ACBrSATExtratoClass, ACBrSATExtratoReportClass, pcnCFe,
  pcnCFeCanc, pcnConversao, DB, DBClient, frxClass, frxExportPDF, frxDBSet, frxBarcode
  {$IFDEF USE_EXPORT_FR_SVG} // ImprimirExtratoSVG
    , frxExportSVG
  {$ENDIF}
  {$IFDEF USE_EXPORT_FR_PNG} // ImprimirExtratoPNG
    , frxExportImage
  {$ENDIF}

  ,frxExportHTML;

type
 TTipoImpressao = (tiNormal,tiResumido,tiCancelado);

  EACBrSATExtratoFR = class(Exception);

  { TACBrSATExtratoFR }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoFR = class( TACBrSATExtratoReportClass )
  private
    FCFe: TCFe;
    FCFeCanc: TCFeCanc;
    cdsIdentificacao: TClientDataSet;
    cdsEmitente: TClientDataSet;
    cdsParametros: TClientDataSet;
    cdsDadosProdutos: TClientDataSet;
    cdsInformacoesAdicionais: TClientDataSet;
    cdsCalculoImposto: TClientDataSet;
    cdsFormaPagamento: TClientDataSet;
    cdsEntrega: TClientDataSet;
    frxIdentificacao: TfrxDBDataset;
    frxEmitente: TfrxDBDataset;
    frxParametros: TfrxDBDataset;
    frxDadosProdutos: TfrxDBDataset;
    frxInformacoesAdicionais: TfrxDBDataset;
    frxCalculoImposto: TfrxDBDataset;
    frxFormaPagamento: TfrxDBDataset;
    frxEntrega: TfrxDBDataset;
    frxReport: TfrxReport;
    frxPDFExport: TfrxPDFExport;
    frxHTMLExport: TfrxHTMLExport;
    {$IFDEF USE_EXPORT_FR_SVG}
    frxSVGExport: TfrxSVGExport;
    {$ENDIF}
    {$IFDEF USE_EXPORT_FR_PNG}
    frxPNGExport: TfrxPNGExport;
    {$ENDIF}

    frxBarCodeObject: TfrxBarCodeObject;
    FFastExtrato: string;
    FTipoImpressao : TTipoImpressao;
    FPrintMode: TfrxPrintMode;
    FPrintOnSheet: Integer;
    function PrepareReport(ACFe: TCFe; ACFeCanc:TCFeCanc = nil): Boolean;
    function GetPreparedReport: TfrxReport;
    procedure CriarDataSetsFrx;
    procedure SetDataSetsToFrxReport;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
    procedure CarregaDados;
    procedure CarregaIdentificacao;
    procedure CarregaEmitente;
    procedure CarregaParametros;
    procedure CarregaInformacoesAdicionais;
    procedure CarregaDadosEntrega;
    procedure CarregaDadosProdutos;
    procedure CarregaCalculoImposto;
    procedure CarregaFormaPagamento;
    procedure AjustaMargensReports;
    procedure TipoImpressao(AValue:TTipoImpressao);
  protected
    procedure Imprimir;
    procedure ImprimirExtratoPDF;
    procedure ImprimirExtratoHTML;
    procedure ImprimirExtratoSVG;
    procedure ImprimirExtratoPNG;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirExtrato(ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe: TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe: TCFe = nil; ACFeCanc: TCFeCanc = nil); override;

    procedure ImprimirExtrato(AStream: TStream; ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(AStream: TStream; ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(AStream: TStream; ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;
    property PrintMode: TfrxPrintMode read FPrintMode write FPrintMode default pmDefault;
    property PrintOnSheet: Integer read FPrintOnSheet write FPrintOnSheet default 0;
    property PreparedReport: TfrxReport read GetPreparedReport;
  published
    property FastExtrato: string read FFastExtrato write FFastExtrato;
  end ;

implementation

uses
  StrUtils,
  ACBrDFeUtil, ACBrSAT,
  ACBrValidador, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrImage, ACBrDelphiZXingQRCode;

{ TACBrSATExtratoFR }

procedure TACBrSATExtratoFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  qrCode,qrCodeCanc: string;
  bandaHeader, bandaProduto,bandaDadosPagamentoHeader : TfrxComponent;
begin
  if Assigned(FCFe) then
  begin
    with FCFe do
      qrCode := CalcularConteudoQRCode(infCFe.ID,
                                       ide.dEmi+ide.hEmi,
                                       Total.vCFe,
                                       Trim(Dest.CNPJCPF),
                                       ide.assinaturaQRCODE);

    if Assigned(Sender) and (Trim(qrCode) <> '') and (Sender.Name = 'ImgQrCode') then
      PintarQRCode(qrCode, TfrxPictureView(Sender).Picture.Bitmap, qrUTF8NoBOM);

    case FTipoImpressao of
      tiResumido : begin
        bandaHeader  := frxReport.FindObject('GroupHeader1');
        bandaProduto := frxReport.FindObject('DadosProdutos');
        bandaDadosPagamentoHeader := frxReport.FindObject('DadosPagamentoHeader');

        if assigned(bandaHeader) then
          bandaHeader.Visible := False;
        if assigned(bandaProduto) then
          bandaProduto.Visible := False;
        if assigned(bandaDadosPagamentoHeader) then
          bandaDadosPagamentoHeader.Visible := False;
      end;
      tiCancelado : begin
        if Assigned(Sender) and (Trim(qrCode) <> '') and (Sender.Name = 'ImgQrCodeCupom') then
          PintarQRCode(qrCode, TfrxPictureView(Sender).Picture.Bitmap, qrUTF8NoBOM);
        with FCFeCanc do
        begin
          qrCodeCanc := CalcularConteudoQRCode(infCFe.ID,
                                           ide.dEmi+ide.hEmi,
                                           Total.vCFe,
                                           Trim(Dest.CNPJCPF),
                                           ide.assinaturaQRCODE);
        end;
        if Assigned(Sender) and (Trim(qrCodeCanc) <> '') and (Sender.Name = 'ImgQrCodeCancelado') then
          PintarQRCode(qrCodeCanc, TfrxPictureView(Sender).Picture.Bitmap, qrUTF8NoBOM);
      end;
    end;


  end;
end;

procedure TACBrSATExtratoFR.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(frxIdentificacao);
  frxReport.EnabledDataSets.Add(frxEmitente);
  frxReport.EnabledDataSets.Add(frxParametros);
  frxReport.EnabledDataSets.Add(frxDadosProdutos);
  frxReport.EnabledDataSets.Add(frxInformacoesAdicionais);
  frxReport.EnabledDataSets.Add(frxCalculoImposto);
  frxReport.EnabledDataSets.Add(frxFormaPagamento);
  frxReport.EnabledDataSets.Add(frxEntrega);
end;

procedure TACBrSATExtratoFR.TipoImpressao(AValue: TTipoImpressao);
begin
  FTipoImpressao := AValue;
end;

procedure TACBrSATExtratoFR.CarregaDados;
begin
  CarregaParametros;
  CarregaIdentificacao;
  CarregaEmitente;
  CarregaDadosProdutos;
  CarregaInformacoesAdicionais;
  CarregaCalculoImposto;
  CarregaFormaPagamento;
  CarregaDadosEntrega
end;

function TACBrSATExtratoFR.GetPreparedReport: TfrxReport;
begin
  if Trim(FastExtrato) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrSATExtratoFR.CarregaParametros;
var
  LStream            : TMemoryStream;
  LStringStream      : TStringStream;
begin
  with cdsParametros do
  begin
    Close;
    CreateDataSet;
    Append;

    FieldByName('QtdeItens').AsInteger := FCFe.Det.Count;
    FieldByName('Sistema').AsString := Ifthen(Self.Sistema <> '',Self.Sistema,'Projeto ACBr - https://www.projetoacbr.com.br');
    FieldByName('Usuario').AsString := Ifthen(Self.Usuario <> '', Self.Usuario,'');
    FieldByName('MsgAppQRCode').AsString := Self.MsgAppQRCode;

    // Carregamento da imagem
    if Self.Logo <> '' then
    begin
      FieldByName('Imagem').AsString := Self.Logo;
      LStream                        := TMemoryStream.Create;
      try
        if FileExists(Self.Logo) then
          LStream.LoadFromFile(Self.Logo)
        else
        begin
          LStringStream := TStringStream.Create(Self.Logo);
          try
            LStream.LoadFromStream(LStringStream);
          finally
            LStringStream.Free;
          end;
        end;
        LStream.Position := 0;
        TBlobField(cdsParametros.FieldByName('LogoCarregado')).LoadFromStream(LStream);
      finally
        LStream.Free;
      end;
    end;
    Post;
  end;
end;

function TACBrSATExtratoFR.PrepareReport(ACFe: TCFe; ACFeCanc:TCFeCanc = nil): Boolean;
var
  Stream: TStringStream;
begin
  Result := False;

  SetDataSetsToFrxReport;

  if NaoEstaVazio(Trim(FastExtrato)) then
  begin
    if not (UpperCase(Copy(FastExtrato, Length(FastExtrato)-3, 4)) = '.FR3') then
    begin
      Stream := TStringStream.Create(FastExtrato);
      try
        frxReport.FileName := '';
        frxReport.LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    end else
    begin
      if FileExists(FastExtrato) then
        frxReport.LoadFromFile(FastExtrato)
      else
        raise EACBrSATExtratoFR.CreateFmt('Caminho do arquivo de impressão do Extrato SAT "%s" inválido.', [FastExtrato]);
    end;
  end else
    raise EACBrSATExtratoFR.Create('Caminho do arquivo de impressão do Extrato SAT não assinalado.');

  frxReport.PrintOptions.Copies      := NumCopias;
  frxReport.PrintOptions.ShowDialog  := MostraSetup;
  frxReport.ShowProgress             := MostraStatus;
  frxReport.PrintOptions.PrintMode   := FPrintMode; //Precisamos dessa propriedade porque impressoras não fiscais cortam o papel quando há muitos itens. O ajuste dela deve ser necessariamente após a carga do arquivo FR3 pois, antes da carga o componente é inicializado
  frxReport.PrintOptions.PrintOnSheet := FPrintOnSheet; //Essa propriedade pode trabalhar em conjunto com a printmode
  frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  frxReport.Variables['isCancelado'] := Ord(FTipoImpressao = tiCancelado);

  // preparar relatorio
  if Assigned(ACFe) then
  begin
    FCFe := ACFe;
    CarregaDados;

    Result := frxReport.PrepareReport;
  end else
  begin
    if Assigned(ACBrSAT) then
    begin
      FCFe := TACBrSAT(ACBrSAT).CFe;
      if Assigned(TACBrSAT(ACBrSAT).CFeCanc) then
        FCFeCanc := TACBrSAT(ACBrSAT).CFeCanc;
      CarregaDados;

      Result := frxReport.PrepareReport( true );
    end;
  end;
  if assigned(frxReport) then
    AjustaMargensReports;
end;


constructor TACBrSATExtratoFR.Create(AOwner: TComponent);
begin
   inherited create(AOwner);
   FFastExtrato   := '';
   CriarDataSetsFrx;
end;

destructor TACBrSATExtratoFR.Destroy;
begin
  if Assigned(frxReport) then
    frxReport.Free;
  inherited Destroy;
end;

procedure TACBrSATExtratoFR.Imprimir;
begin
  case Filtro of
    fiNenhum:
      Begin
        if MostraPreview then
          frxReport.ShowPreparedReport
        else
          frxReport.Print;
      end;
    fiPDF : ImprimirExtratoPDF;
    fiHTML: ImprimirExtratoHTML;
    fiSVG:  ImprimirExtratoSVG;
    fiPNG:  ImprimirExtratoPNG;
  end;
  TipoImpressao(tiNormal);
end;

procedure TACBrSATExtratoFR.ImprimirExtrato(ACFe: TCFe);
begin
  inherited;
  if PrepareReport(ACFe) then
    Imprimir;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  inherited;
  TipoImpressao(tiCancelado);
  if PrepareReport(ACFe,ACFeCanc) then
    Imprimir;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoHTML;
begin
  if EstaVazio(Trim(NomeDocumento)) then
    frxHTMLExport.FileName := 'Extrato SAT'
  else
    frxHTMLExport.FileName := NomeDocumento;
  frxHTMLExport.FileName     := PathPDF + frxHTMLExport.FileName + '.html';
  frxHTMLExport.ShowDialog   := false;
  frxHTMLExport.ShowProgress := MostraStatus;
  frxReport.Export(frxHTMLExport);
  FPArquivoPDF := frxHTMLExport.FileName;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoSVG;
begin
  {$IFDEF USE_EXPORT_FR_SVG}
    if (FStream <> nil) then
      frxSVGExport.Stream := FStream;

    if EstaVazio(Trim(NomeDocumento)) then
      frxSVGExport.FileName := 'Extrato SAT'
    else
      frxSVGExport.FileName := NomeDocumento;

    frxSVGExport.FileName     := PathPDF + ChangeFileExt(frxSVGExport.FileName, '.svg');
    frxSVGExport.ShowDialog   := false;
    frxSVGExport.ShowProgress := MostraStatus;

    frxReport.Export(frxSVGExport);
    FPArquivoPDF := frxSVGExport.FileName;
  {$ELSE}
    ImprimirExtratoPDF;
  {$ENDIF}
end;

procedure TACBrSATExtratoFR.ImprimirExtratoPNG;
begin
  {$IFDEF USE_EXPORT_FR_PNG}
    if (FStream <> nil) then
      frxPNGExport.Stream := FStream;

    if EstaVazio(Trim(NomeDocumento)) then
      frxPNGExport.FileName := 'Extrato SAT'
    else
      frxPNGExport.FileName := NomeDocumento;

    frxPNGExport.FileName     := PathPDF + ChangeFileExt(frxPNGExport.FileName, '.png');
    frxPNGExport.ShowDialog   := false;
    frxPNGExport.ShowProgress := MostraStatus;

    frxReport.Export(frxPNGExport);
    FPArquivoPDF := frxPNGExport.FileName;
  {$ELSE}
    ImprimirExtratoPDF;
  {$ENDIF}
end;

procedure TACBrSATExtratoFR.ImprimirExtratoPDF;
begin
  if (FStream <> nil) then
    frxPDFExport.Stream := FStream;

  frxPDFExport.ShowDialog        := false;
  frxPDFExport.ShowProgress      := MostraStatus;
  frxPDFExport.Author            := Sistema;
  frxPDFExport.Creator           := Sistema;
  frxPDFExport.Producer          := Sistema;
  frxPDFExport.Title             := 'Extrato SAT';
  frxPDFExport.Subject           := frxPDFExport.Title;
  frxPDFExport.Keywords          := frxPDFExport.Title;
  frxPDFExport.Background        := false;//False diminui 70% do tamanho do pdf
  frxPDFExport.EmbeddedFonts     := false;

  if EstaVazio(Trim(NomeDocumento)) then
    frxPDFExport.FileName := frxPDFExport.Title
  else
    frxPDFExport.FileName := NomeDocumento;

  frxPDFExport.FileName := PathPDF + ChangeFileExt(frxPDFExport.FileName, '.pdf');

  if frxPDFExport.FileName <> NomeDocumento then
    NomeDocumento := frxPDFExport.FileName;

  if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
    ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

  frxReport.Export(frxPDFExport);
  FPArquivoPDF := frxPDFExport.FileName;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoResumido(AStream: TStream;
  ACFe: TCFe);
begin
  inherited;
  TipoImpressao(tiResumido);
  try
    if PrepareReport(ACFe) then
      ImprimirExtratoPDF;
  finally
    TipoImpressao(tiNormal);
  end;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoResumido(ACFe: TCFe);
begin
  inherited;
  TipoImpressao(tiResumido);
  if PrepareReport(ACFe) then
  begin
    Imprimir;
  end;
end;

procedure TACBrSATExtratoFR.CriarDataSetsFrx;
begin
  frxReport := TfrxReport.Create(ACBrSAT);

  with frxReport do
  begin
    EngineOptions.UseGlobalDataSetList := False;
    ScriptLanguage := 'PascalScript';
    StoreInDFM     := False;
    OnBeforePrint  := frxReportBeforePrint;
    OnReportPrint  := 'frxReportOnReportPrint';
    PreviewOptions.Buttons :=[pbExport, pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick];
  end;

  frxPDFExport := TfrxPDFExport.Create(Self);
  frxPDFExport.PrintOptimized := True;
  frxPDFExport.ShowProgress := False;

  RttiSetProp(frxPDFExport, 'Transparency', 'False');

  frxHTMLExport := TfrxHTMLExport.Create(Self);
  {$IFDEF USE_EXPORT_FR_SVG}
  frxSVGExport  := TfrxSVGExport.Create(Self);
  {$ENDIF}
  {$IFDEF USE_EXPORT_FR_PNG}
  frxPNGExport  := TfrxPNGExport.Create(Self);
  {$ENDIF}

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
    Add('nserieSAT', ftString, 15);
    Add('nCFe', ftString, 15);
    Add('dhEmi', ftDateTime);
    Add('CPFConsumidor', ftString, 45);
    Add('IdCanc', ftString, 44);
    Add('ChaveCanc', ftString, 60);
    Add('nCFeCanc', ftString, 15);
    Add('dhEmiCanc', ftDateTime);
    CreateDataSet;
  end;

  cdsEmitente := TClientDataSet.Create(Self);
  with cdsEmitente, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('IE', ftString, 14);
    Add('IM', ftString, 14);
    Add('xNome', ftString, 60);
    Add('xFant', ftString, 60);
    Add('xLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('xBairro', ftString, 60);
    Add('xMun', ftString, 60);
    Add('CEP', ftString, 9);
    Add('email', ftString, 60);
    Add('site', ftString, 60);
    CreateDataSet;
  end;

  cdsParametros := TClientDataSet.Create(Self);
  with cdsParametros, FieldDefs do
  begin
    Close;
    Clear;
    Add('Imagem', ftString, 256);
    Add('Sistema', ftString, 150);
    Add('Usuario', ftString, 60);
    Add('QrCodeCarregado', ftGraphic, 1000);
    Add('LogoCarregado', ftBlob);
    Add('QtdeItens', ftInteger);
    Add('MsgAppQRCode',ftString,150);
    CreateDataSet;
  end;

  cdsDadosProdutos := TClientDataSet.Create(Self);
  with cdsDadosProdutos, FieldDefs do
  begin
    Close;
    Clear;
    Add('nItem', ftInteger);
    Add('ChaveCFe', ftString, 50);
    Add('cProd', ftString, 60);
    Add('xProd', ftString, 120);
    Add('CFOP', ftString, 4);
    Add('uCom', ftString, 6);
    Add('qCom', ftFloat);
    Add('vUnCom', ftFloat);
    Add('vProd' , ftFloat);
    Add('indRegra', ftString, 1);
    Add('vItem', ftFloat);
    Add('vTR', ftString, 25);
    Add('vOutro', ftString, 18);
    Add('vDesc', ftString, 18);
    Add('vBC', ftFloat);
    Add('vDeducISSQN', ftFloat);
    Add('Valorliquido', ftString, 18);
    Add('ValorAcrescimos', ftString, 18);
    CreateDataSet;
  end;

  cdsCalculoImposto := TClientDataSet.Create(Self);
  with cdsCalculoImposto, FieldDefs do
  begin
    Add('vICMS', ftFloat);
    Add('vProd', ftFloat);
    Add('vPIS', ftFloat);
    Add('vCOFINS', ftFloat);
    Add('vCFe', ftFloat);
    Add('vTotPago', ftFloat);
    Add('vTroco', ftFloat);
    Add('vPISST', ftFloat);
    Add('vCOFINSST', ftFloat);
    Add('vAcresSubtot', ftFloat);
    Add('vDescSubtot', ftFloat);
    Add('vCFeLei12741', ftFloat);
    Add('vDescAcresItens', ftString, 18);
    CreateDataSet;
  end;

  cdsFormaPagamento := TClientDataSet.Create(Self);
  with cdsFormaPagamento, FieldDefs do
  begin
    Add('tPag', ftString, 17);
    Add('vMP', ftFloat);
    CreateDataSet;
  end;

  cdsEntrega := TClientDataSet.Create(Self);
  with cdsEntrega, FieldDefs do
  begin
    Add('EnderecoEntrega', ftString, 310);
    CreateDataSet;
  end;

  cdsInformacoesAdicionais := TClientDataSet.Create(Self);
  with cdsInformacoesAdicionais, FieldDefs do
  begin
    Add('infAdic', ftString, 6900);
    Add('obsFisco', ftString, 6900);
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

  frxParametros := TfrxDBDataset.Create(Self);
  with frxParametros do
  begin
     UserName := 'Parametros';
     OpenDataSource := False;
     DataSet := cdsParametros;
  end;

  frxDadosProdutos := TfrxDBDataset.Create(Self);
  with frxDadosProdutos do
  begin
     UserName := 'DadosProdutos';
     OpenDataSource := False;
     DataSet := cdsDadosProdutos;
  end;

  frxInformacoesAdicionais := TfrxDBDataset.Create(Self);
  with frxInformacoesAdicionais do
  begin
     UserName := 'InformacoesAdicionais';
     OpenDataSource := False;
     DataSet := cdsInformacoesAdicionais;
  end;

  frxCalculoImposto := TfrxDBDataset.Create(Self);
  with frxCalculoImposto do
  begin
     UserName := 'CalculoImposto';
     OpenDataSource := False;
     DataSet := cdsCalculoImposto;
  end;

  frxFormaPagamento := TfrxDBDataset.Create(Self);
  with frxFormaPagamento do
  begin
     UserName := 'FormaPagamento';
     OpenDataSource := False;
     DataSet := cdsFormaPagamento;
  end;

  frxEntrega := TfrxDBDataset.Create(Self);
  with frxEntrega do
  begin
     UserName := 'DadosEntrega';
     OpenDataSource := False;
     DataSet := cdsEntrega;
  end;
end;

procedure TACBrSATExtratoFR.CarregaIdentificacao;
var NumExtrato: String;
begin
  with cdsIdentificacao do
  begin
    Close;
    CreateDataSet;
    Append;

    with FCFe.infCFe do
    begin
      FieldByName('Id').AsString    := OnlyNumber(Id);
      FieldByName('Chave').AsString := FormatarChaveAcesso(Id);
    end;

    with FCFe.Ide do
    begin
      if tpAmb = taHomologacao then
        NumExtrato := '000.000.000'
      else
        NumExtrato := FormatFloatBr(nCFe,'000,000,000');

      FieldByName('nCFe').AsString      := NumExtrato;
      FieldByName('tpAmb').AsInteger    := StrToIntDef(TpAmbToStr(tpAmb), 0);
      FieldByName('nserieSAT').AsString := FormatFloatBr(nserieSAT,'000,000,000');
      FieldByName('dhEmi').AsString     := FormatDateTimeBr(dEmi + hEmi, 'DD/MM/YYYY - hh:nn:ss');
    end;
    if FCFeCanc <> nil then
    begin
      with FCFeCanc.infCFe do
      begin
        FieldByName('IdCanc').AsString    := OnlyNumber(Id);
        FieldByName('ChaveCanc').AsString := FormatarChaveAcesso(Id);
      end;

      with FCFeCanc.ide do
      begin
        if FCFe.Ide.tpAmb = taHomologacao then
          NumExtrato := '000.000.000'
        else
          NumExtrato := FormatFloatBr(nCFe,'000,000,000');

        FieldByName('nCFeCanc').AsString      := NumExtrato;
        FieldByName('dhEmiCanc').AsString     := FormatDateTimeBr(dEmi + hEmi, 'DD/MM/YYYY - hh:nn:ss');
      end;
    end;

    if FCFe.Dest.CNPJCPF <> '' then
    begin
      if Length(FCFe.Dest.CNPJCPF) = 14 then
        FieldByName('CPFConsumidor').AsString := Format('CONSUMIDOR - CNPJ %s', [FormatarCNPJ(OnlyNumber(FCFe.Dest.CNPJCPF))])
      else
        FieldByName('CPFConsumidor').AsString := Format('CONSUMIDOR - CPF %s', [FormatarCPF(OnlyNumber(FCFe.Dest.CNPJCPF))]);
    end
    else
      FieldByName('CPFConsumidor').AsString := 'CONSUMIDOR NÃO IDENTIFICADO';

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaEmitente;
begin
  with cdsEmitente do
  begin
    Close;
    CreateDataSet;
    Append;

    with FCFe.emit do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);
      FieldByName('IE').AsString    := IE;
      FieldByName('IM').AsString    := IM;
      FieldByName('XNome').AsString := xNome;
      FieldByName('XFant').AsString := XFant;

      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('XMun').AsString    := XMun;
        FieldByName('CEP').AsString     := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('email').AsString   := email;
        FieldByName('site').AsString    := Self.Site;
      end;
    end;

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaDadosProdutos;
var
  inItem: Integer;
begin
  with cdsDadosProdutos do
  begin
    Close;
    CreateDataSet;
    for inItem := 0 to Pred(FCFe.Det.Count) do
    begin
      Append;

      with FCFe.Det.Items[inItem] do
      begin
        FieldByName('nItem').AsInteger   := inItem + 1;
        FieldByName('ChaveCFe').AsString := FCFe.infCFe.ID;
        FieldByName('cProd').AsString    := Trim(Prod.cProd);
        FieldByName('xProd').AsString    := StringReplace(Prod.xProd, ';', #13, [rfReplaceAll]);
        FieldByName('CFOP').AsString     := Prod.CFOP;
        FieldByName('uCom').AsString     := Prod.uCom;
        FieldByName('qCom').AsFloat      := Prod.qCom;
        FieldByName('vUnCom').AsFloat    := Prod.vUnCom;
        FieldByName('vProd').AsFloat     := Prod.vProd;
        FieldByName('indRegra').AsString := indRegraToStr(Prod.indRegra);
        FieldByName('vItem').AsFloat     := Prod.vItem;
        FieldByName('vDesc').AsString    := FormatFloatBr(Prod.vDesc,',0.00');
        FieldByName('vOutro').AsString   := FormatFloatBr(Prod.vOutro,',0.00');

        if Imposto.vItem12741 > 0 then
          FieldByName('vTR').AsString := ' ('+FormatFloatBr(Imposto.vItem12741)+') '
        else
          FieldByName('vTR').AsString := '';

        FieldByName('Valorliquido').AsString    := FormatFloatBr(Prod.vProd - Prod.vDesc ,',0.00');
        FieldByName('ValorAcrescimos').AsString := FormatFloatBr(Prod.vProd + Prod.vOutro,',0.00');

        FieldByName('vDeducISSQN').AsFloat := Imposto.ISSQN.vDeducISSQN;
        FieldByName('vBC').AsFloat         := Imposto.ISSQN.vBC;
      end;

      Post;
    end;
  end;
end;

procedure TACBrSATExtratoFR.CarregaInformacoesAdicionais;
var i: Integer;
begin
  with FCFe, cdsInformacoesAdicionais do
  begin
    Close;
    CreateDataSet;
    Append;

    if (Emit.cRegTrib = RTSimplesNacional) then
      FieldByName('ObsFisco').AsString := Msg_ICMS_123_2006;

    for i := 0 to Pred(obsFisco.Count) do
      FieldByName('ObsFisco').AsString := FieldByName('ObsFisco').AsString + obsFisco[i].xCampo + '-' + obsFisco[i].xTexto;

    if (InfAdic.infCpl <> '') or (Self.ImprimeMsgOlhoNoImposto and (Total.vCFeLei12741 > 0)) then
      FieldByName('infAdic').AsString := StringReplace(InfAdic.infCpl,';',sLineBreak,[rfReplaceAll]);;

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaCalculoImposto;
begin
  with cdsCalculoImposto do
  begin
    Close;
    CreateDataSet;
    Append;

    with FCFe.Total do
    begin
      FieldByName('vICMS').AsFloat     := ICMSTot.vICMS;
      FieldByName('vProd').AsFloat     := ICMSTot.vProd;
      FieldByName('vPIS').AsFloat      := ICMSTot.vPIS;
      FieldByName('vCOFINS').AsFloat   := ICMSTot.vCOFINS;
      FieldByName('vPISST').AsFloat    := ICMSTot.vPISST;
      FieldByName('vCOFINSST').AsFloat := ICMSTot.vCOFINSST;

      FieldByName('vDescSubtot').AsFloat  := DescAcrEntr.vDescSubtot;
      FieldByName('vAcresSubtot').AsFloat := DescAcrEntr.vAcresSubtot;

      if ICMSTot.vDesc > 0 then
         FieldByName('vDescAcresItens').AsString := FormatFloatBr(ICMSTot.vDesc, '-,0.00')
      else
      if ICMSTot.vOutro > 0 then
         FieldByName('vDescAcresItens').AsString := FormatFloatBr(ICMSTot.vOutro, '+,0.00');

      FieldByName('vCFe').AsFloat := vCFe;
      FieldByName('vCFeLei12741').AsFloat := vCFeLei12741;
    end;

    if FCFe.Pagto.vTroco > 0 then
    begin
      FieldByName('vTroco').AsCurrency   := FCFe.Pagto.vTroco;
      FieldByName('vTotPago').AsCurrency := FCFe.Pagto.vTroco+FieldByName('vCFe').AsFloat;
    end;

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaFormaPagamento;
var 
   i: Integer;
begin
  with cdsFormaPagamento do
  begin
    Close;
    CreateDataSet;
    for i := 0 to Pred(FCFe.Pagto.Count) do
    begin
      Append;

      with FCFe.Pagto.Items[i] do
      begin
        FieldByName('tPag').AsString := CodigoMPToDescricao(cMP);
        FieldByName('vMP').AsFloat   := vMP;
      end;

      Post;
    end;
  end;
end;

procedure TACBrSATExtratoFR.CarregaDadosEntrega;
begin
  with cdsEntrega, FCFe.Entrega do
  begin
    Close;
    CreateDataSet;
    if xLgr <> '' then
    begin
      Append;
      FieldByName('EnderecoEntrega').AsString := Format('%s, nº %s - %s - %s %s', [xLgr, nro, xBairro, xMun, UF]);
      Post;
    end;
  end;
end;

procedure TACBrSATExtratoFR.AjustaMargensReports;
var
  Page: TfrxReportPage;
  i: Integer;
begin
  for i := 0 to Pred(frxReport.PreviewPages.Count) do
  begin
    Page := frxReport.PreviewPages.Page[i];
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

procedure TACBrSATExtratoFR.ImprimirExtrato(AStream: TStream; ACFe: TCFe);
begin
  inherited;
   TipoImpressao(tiNormal);

   case Filtro of
      fiPDF : ImprimirExtratoPDF;
      fiSVG:  ImprimirExtratoSVG;
      fiPNG:  ImprimirExtratoPNG;
   end;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoCancelamento(AStream: TStream;
  ACFe: TCFe; ACFeCanc: TCFeCanc);
begin
  inherited;
  TipoImpressao(tiCancelado);
  try
    if PrepareReport(ACFe,ACFeCanc) then
      ImprimirExtratoPDF;
  finally
    TipoImpressao(tiNormal);
  end;
end;

end.
