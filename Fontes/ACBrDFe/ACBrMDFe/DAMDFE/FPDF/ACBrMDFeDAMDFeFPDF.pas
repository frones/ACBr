{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H. Gonzales - Pandaaa                    }
{                              Antonio Carlos Junior                           }
{                                                                              }
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

unit ACBrMDFeDAMDFeFPDF;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  Math,
  ACBr_fpdf,
  ACBr_fpdf_ext,
  ACBr_fpdf_report,
  ACBrMDFe.Classes,
  pcnConversao,
  pmdfeConversaoMDFe,
  ACBrValidador,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeUtil,
  ACBrUtil.Compatibilidade,
  ACBrMDFe,
  ACBrBase,
  ACBrMDFeUtilsFPDF,
  ACBrMDFeDAMDFEClass,
  ACBrImage;

type
  { TMDFeDAMDFeFPDF }

  TMDFeDAMDFeFPDF = class(TFPDFReport)
    private
      FMDFe: TMDFe;
      FMDFeUtils: TMDFeUtilsFPDF;
      FDAMDFEClassOwner: TACBrMDFeDAMDFEClass;
      FCancelada: boolean;
      FCanhoto: TPosRecibo;
      FLogo: TBytes;
      FLogoStretched: boolean;
      FLogoAlign: TLogoAlign;
      FMensagemRodape: string;
      FInitialized: boolean;
      property MDFe: TMDFe read FMDFe;

    protected
      procedure OnStartReport(Args: TFPDFReportEventArgs); override;

    public
      constructor Create(AMDFe: TMDFe; AACBrMDFeDAMDFEClass : TACBrMDFeDAMDFEClass); reintroduce;
      destructor Destroy; override;

    public
      property Cancelada: boolean read FCancelada write FCancelada;
      property PosCanhoto: TPosRecibo read FCanhoto write FCanhoto;
      property LogoBytes: TBytes read FLogo write FLogo;
      property LogoStretched: boolean read FLogoStretched write FLogoStretched;
      property LogoAlign: TLogoAlign read FLogoAlign write FLogoAlign;
      property MensagemRodape: string read FMensagemRodape write FMensagemRodape;
  end;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

  { TACBrMDFeDAMDFeFPDF }

  TACBrMDFeDAMDFeFPDF = class(TACBrMDFeDAMDFEClass)
    private
      FFPDFReport: TMDFeDAMDFeFPDF;
      FDAMDFEClassOwner: TACBrMDFeDAMDFEClass;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      // Adicione propriedades, métodos e eventos específicos do componente
      procedure ImprimirDAMDFe(MDFE: TMDFe = nil); override;
      procedure ImprimirDAMDFePDF(MDFE: TMDFe = nil); override;
      procedure ImprimirDAMDFePDF(AStream: TStream; MDFE: TMDFe = nil); override;

    published
      // Declare propriedades publicamente acessíveis aqui
  end;

implementation

type

  { TBlocoCanhoto }

  TBlocoCanhoto = class(TFPDFBand)
    private
      FAlign: TPosRecibo;
      FMDFeUtils: TMDFeUtilsFPDF;
      procedure DrawCanhoto(Args: TFPDFBandDrawArgs; vX, vY, vW, vH: double);
      procedure DrawTopBottom(Args: TFPDFBandDrawArgs);
      procedure DrawLeft(Args: TFPDFBandDrawArgs);
      procedure DrawRight(Args: TFPDFBandDrawArgs);

    protected
      procedure OnInit(Args: TFPDFBandInitArgs); override;
      procedure OnDraw(Args: TFPDFBandDrawArgs); override;

    public
      constructor Create(AAlign: TPosRecibo; AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
  end;

  { TBlocoDadosMDFe }
  TBlocoDadosMDFe = class(TFPDFBand)
    private
      FMDFeUtils: TMDFeUtilsFPDF;

      FLogo: TBytes;
      FLogoStretched: Boolean;
      FLogoAlign: TLogoAlign;
      FImageUtils: TImageUtils;

    protected
      procedure OnInit(Args: TFPDFBandInitArgs); override;
      procedure OnDraw(Args: TFPDFBandDrawArgs); override;

    public
      constructor Create(AMDFeUtils: TMDFeUtilsFPDF; ALogo: TBytes; ALogoStretched: Boolean; ALogoAlign: TLogoAlign); reintroduce;
      destructor Destroy; override;
  end;

{ TBlocoDocAuxiliarMDFe }
TBlocoDocAuxiliarMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoModalCargaRodoviarioMDFe }
TBlocoModalCargaRodoviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoModalCargaAereoMDFe }
TBlocoModalCargaAereoMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoModalCargaAquaviarioMDFe }
TBlocoModalCargaAquaviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoModalCargaFerroviarioMDFe }
TBlocoModalCargaFerroviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoProtocoloAutorizacaoMDFe }
TBlocoProtocoloAutorizacaoMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesModalRodoviarioMDFe }
TBlocoTabelaInformacoesModalRodoviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesModalAereoMDFe }
TBlocoTabelaInformacoesModalAereoMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesModalAquaviarioMDFe }
TBlocoTabelaInformacoesModalAquaviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesModalFerroviarioMDFe }
TBlocoTabelaInformacoesModalFerroviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesComposicaoCargaRodoviarioMDFe }
TBlocoTabelaInformacoesComposicaoCargaRodoviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesComposicaoCargaAereoMDFe }
TBlocoTabelaInformacoesComposicaoCargaAereoMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesComposicaoCargaAquaviarioMDFe }
TBlocoTabelaInformacoesComposicaoCargaAquaviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoTabelaInformacoesComposicaoCargaFerroviarioMDFe }
TBlocoTabelaInformacoesComposicaoCargaFerroviarioMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoObservacoesMDFe }
TBlocoObservacoesMDFe = class(TFPDFBand)
  private
    FMDFeUtils: TMDFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AMDFeUtils: TMDFeUtilsFPDF); reintroduce;
end;

{ TBlocoCanhoto }

const
  cDefaultFontFamily = 'Times';

procedure TBlocoCanhoto.DrawCanhoto(Args: TFPDFBandDrawArgs; vX, vY, vW, vH: double);
begin
  inherited;
end;

procedure TBlocoCanhoto.DrawTopBottom(Args: TFPDFBandDrawArgs);
begin
  DrawCanhoto(Args, 0, 0, Width, 10);
end;

procedure TBlocoCanhoto.DrawLeft(Args: TFPDFBandDrawArgs);
var
  x, y: double;
begin
  x := 0;
  y := Height;

  Args.PDF.Rotate(90, x, y);
  try
    DrawCanhoto(Args, x, y, Height, 10);
  finally
    Args.PDF.Rotate(0, x, y);
  end;
end;

procedure TBlocoCanhoto.DrawRight(Args: TFPDFBandDrawArgs);
var
  x, y: double;
begin
  x := 0;
  y := Height;

  Args.PDF.Rotate(90, x, y);
  try
    DrawCanhoto(Args, x, y, Height, 10);
  finally
    Args.PDF.Rotate(0, x, y);
  end;
end;

procedure TBlocoCanhoto.OnInit(Args: TFPDFBandInitArgs);
begin
  case FAlign of
    prCabecalho,
    prRodape:
      Height := 33;
    prEsquerda:
      Width := 33;
  else
    //
  end;

  // Por causa do double pass
  Visible := True;
end;

procedure TBlocoCanhoto.OnDraw(Args: TFPDFBandDrawArgs);
begin
    case FAlign of
    prCabecalho,
    prRodape:
      DrawTopBottom(Args);
    prEsquerda:
      DrawLeft(Args);
    //caRight:
    //  DrawRight(Args);
  else
    //
  end;

  // Para não ser exibido nas próximas páginas
  Visible := False;
end;

constructor TBlocoCanhoto.Create(AAlign: TPosRecibo; AMDFeUtils: TMDFeUtilsFPDF);
begin
  FAlign := AAlign;

    case FAlign of
    prCabecalho:
      inherited Create(btTopMargin);
    prEsquerda:
      inherited Create(btLeftMargin);
    //caRight:
    //  inherited Create(btRightMargin);
    prRodape:
      inherited Create(btBottomMargin);
  else
    inherited Create(btTopMargin);
    Visible := False;
  end;

  FMDFeUtils := AMDFeUtils;
end;

{ TMDFeDAMDFeFPDF }

procedure TMDFeDAMDFeFPDF.OnStartReport(Args: TFPDFReportEventArgs);
var
  LLogoStringStream : TStringStream;
  LStream : TMemoryStream;
  LOrientation: TFPDFOrientation;
begin
  if not FInitialized then
  begin
    if FMDFe = nil then
      raise Exception.Create('FACBrMDFe not initialized');

    if FDAMDFEClassOwner.Logo <> '' then
    begin
      LStream := TMemoryStream.Create;
      try
        if FileExists(FDAMDFEClassOwner.Logo) then
        LStream.LoadFromFile(FDAMDFEClassOwner.Logo)
        else
        begin
          LLogoStringStream := TStringStream.Create(FDAMDFEClassOwner.Logo);
          try
            LStream.LoadFromStream(LLogoStringStream);
            LStream.Position := 0;
          finally
            LLogoStringStream.Free;
          end;
        end;
        if IsPNG(LStream, false) then
        begin
          //SetLength(FLogo, LStream.Size);
          LStream.Position := 0;
          LStream.Read(FLogo[0], LStream.Size);
        end;
      finally
        LStream.Free;
      end;
    end;
    AddPage(LOrientation);
    AddBand(TBlocoCanhoto.Create(PosCanhoto, FMDFeUtils));
    AddBand(TBlocoDadosMDFe.Create(FMDFeUtils, FLogo, FLogoStretched, FLogoAlign));
    AddBand(TBlocoDocAuxiliarMDFe.Create(FMDFeUtils));

    case FMDFe.Ide.modal of
      moRodoviario:
        begin
          //Modal Carga Rodoviario
          AddBand(TBlocoModalCargaRodoviarioMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesModalRodoviarioMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesComposicaoCargaRodoviarioMDFe.Create(FMDFeUtils));
        end;
      moAereo:
        begin
          //Modal Carga Aereo
          AddBand(TBlocoModalCargaAereoMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesModalAereoMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesComposicaoCargaAereoMDFe.Create(FMDFeUtils));
        end;
      moAquaviario:
        begin
          //Modal Carga Aquaviario
          AddBand(TBlocoModalCargaAquaviarioMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesModalAquaviarioMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesComposicaoCargaAquaviarioMDFe.Create(FMDFeUtils));
        end;
      moFerroviario:
        begin
          //Modal Carga Ferroviario
          AddBand(TBlocoModalCargaFerroviarioMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesModalFerroviarioMDFe.Create(FMDFeUtils));
          AddBand(TBlocoTabelaInformacoesComposicaoCargaFerroviarioMDFe.Create(FMDFeUtils));
        end;
    end;
    //Protocolo de Autorização
    AddBand(TBlocoProtocoloAutorizacaoMDFe.Create(FMDFeUtils));

    AddBand(TBlocoObservacoesMDFe.Create(FMDFeUtils));
    FInitialized := True;
  end;
end;

constructor TMDFeDAMDFeFPDF.Create(AMDFe: TMDFe; AACBrMDFeDAMDFEClass: TACBrMDFeDAMDFEClass);
var
  LFormatSettings: TFormatSettings;
begin
  inherited Create;
  FMDFeUtils := TMDFeUtilsFPDF.Create(AMDFe, AACBrMDFeDAMDFEClass);
  FMDFe:= AMDFe;
  Self.FDAMDFEClassOwner := AACBrMDFeDAMDFEClass;
  {$IFDEF HAS_FORMATSETTINGS}
    LFormatSettings := CreateFormatSettings;
  {$ENDIF}
  LFormatSettings.DecimalSeparator  := ',';
  LFormatSettings.ThousandSeparator := '.';
  FMDFeUtils.FormatSettings := LFormatSettings;

  SetFont('Times');
  SetUTF8(false);
  EngineOptions.DoublePass := True;
end;

destructor TMDFeDAMDFeFPDF.Destroy;
begin
  FMDFeUtils.Free;
  inherited;
end;

{ TACBrMDFeDAMDFeFPDF }

constructor TACBrMDFeDAMDFeFPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TACBrMDFeDAMDFeFPDF.Destroy;
begin
  FFPDFReport.Free;
  inherited;
end;

procedure TACBrMDFeDAMDFeFPDF.ImprimirDAMDFe(MDFE: TMDFe);
begin
  inherited;
end;

procedure TACBrMDFeDAMDFeFPDF.ImprimirDAMDFePDF(MDFE: TMDFe);
var
  Report: TFPDFReport;
  Engine: TFPDFEngine;
  I: Integer;
  LMDFe: TMDFe;
  LPath: String;
begin
  for I := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count -1 do
  begin
    LMDFe := TACBrMDFe(ACBrMDFe).Manifestos[I].MDFe;
    Report := TMDFeDAMDFeFPDF.Create(LMDFe, TACBrMDFeDAMDFeClass(TACBrMDFe(ACBrMDFe).DAMDFE));

    FIndexImpressaoIndividual := I;

    TMDFeDAMDFeFPDF(Report).PosCanhoto := TMDFeDAMDFeFPDF(TACBrMDFe(ACBrMDFe).DAMDFE).PosCanhoto;

    TMDFeDAMDFeFPDF(Report).MensagemRodape := Self.Sistema;

    try
      Engine := TFPDFEngine.Create(Report, False);
      try
        Engine.Compressed := True;

        LPath := DefinirNomeArquivo(TACBrMDFe(ACBrMDFe).DAMDFE.PathPDF,
               OnlyNumber(LMDFe.infMDFe.Id) + '-mdfe.pdf',
               TACBrMDFe(ACBrMDFe).DAMDFE.NomeDocumento);

        ForceDirectories(ExtractFilePath(LPath));

        Engine.SaveToFile(LPath);
        FPArquivoPDF := LPath;
      finally
        Engine.Free;
      end;
    finally
      Report.Free;
    end;
  end;
end;

procedure TACBrMDFeDAMDFeFPDF.ImprimirDAMDFePDF(AStream: TStream; MDFE: TMDFe);
begin
  inherited ImprimirDAMDFEPDF(AStream, MDFE);
end;

{ TBlocoDadosMDFe }

procedure TBlocoDadosMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 80;
end;

procedure TBlocoDadosMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y : double;
  x1, y1: double;
  DadosQRCode: string;
  LTexto: string;
begin
  inherited OnDraw(Args);

  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  // Posição inicial
  x := 10;
  y := - 20;

  // Cabeçalho - Logo
  LPDF.SetFont('Arial', 'B', 10);
  LPDF.Rect(x, y, 40, 30); // Logo
  LPDF.Text(x + 2, y + 15, 'LOGO EMPRESA');

  // Cabeçalho - Emitente
  LPDF.SetFont('Arial', 'B', 12);
  x1 := x + 45;
  y1 := y + 2;
  LTexto := LMDFE.emit.xNome;
  LPDF.TextBox(x1, y1, 70, 5, LTexto, 'T', 'L', 0, '');

  LPDF.SetFont('Arial', '', 9);
  x1 := x + 45;
  y1 := y + 7;
  LTexto := LMDFE.emit.enderEmit.xLgr + ' ' + LMDFE.emit.enderEmit.nro + ' ' + LMDFE.emit.enderEmit.xCpl;
  LPDF.TextBox(x1, y1, 80, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 45;
  y1 := y + 11;
  LTexto := LMDFE.emit.enderEmit.xMun + ' - ' + LMDFE.emit.enderEmit.UF;
  LPDF.TextBox(x1, y1, 80, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 75;
  y1 := y + 11;
  LTexto := 'CEP: ' + FormatarCEP(LMDFE.emit.enderEmit.CEP);
  LPDF.TextBox(x1, y1, 80, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 45;
  y1 := y + 15;
  LTexto := 'CNPJ: ' + FormatarCNPJ(LMDFE.emit.CNPJCPF);
  LPDF.TextBox(x1, y1, 80, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 85;
  y1 := y + 15;
  LTexto := 'IE: ' + LMDFE.emit.IE;
  LPDF.TextBox(x1, y1, 80, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 112;
  y1 := y + 15;
  LTexto := 'RNTRC: ' + LMDFE.rodo.RNTRC;
  LPDF.TextBox(x1, y1, 80, 5, LTexto, 'T', 'L', 0, '');

  //QRCode.
  LPDF.SetFont(6, 'B');
  x1 := x + 150;
  y1 := y + 10;
  DadosQRCode := LMDFE.infMDFeSupl.qrCodMDFe;
  LPDF.QRCode(x1, y1, 38, DadosQRCode);

end;

constructor TBlocoDadosMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF; ALogo: TBytes; ALogoStretched: Boolean; ALogoAlign: TLogoAlign);
begin
  inherited Create(btPageHeader);
  FMDFeUtils     := AMDFeUtils;
  FLogo          := ALogo;
  FLogoStretched := ALogoStretched;
  FLogoAlign     := ALogoAlign;
  FImageUtils    := TImageUtils.Create;
end;

destructor TBlocoDadosMDFe.Destroy;
begin
  inherited;
end;

{ TBlocoDocAuxiliarMDFe }

constructor TBlocoDocAuxiliarMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoDocAuxiliarMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  // Documento auxiliar
  LPDF.SetFont('Arial', 'B', 10);
  x1 := x;
  y1 := y - 65;
  LTexto := 'DAMDFe - Documento Auxiliar do Manifesto Eletrônico de Documentos Fiscais';
  LPDF.TextBox(x1, y1, 150, 5, LTexto, 'T', 'L', 0, '');

  // Campos fixos
  x1 := x;
  y1 := y - 55;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Modelo';
  LPDF.TextBox(x1, y1, 14, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 51;
  LPDF.SetFont('Arial', '', 9);
  LTexto := LMDFE.Ide.modelo;
  LPDF.TextBox(x1, y1, 14, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 14;
  y1 := y - 55;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Série';
  LPDF.TextBox(x1, y1, 14, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 14;
  y1 := y - 51;
  LPDF.SetFont('Arial', '', 9);
  LTexto := FormatFloat('000', LMDFE.Ide.serie, FMDFeUtils.FormatSettings);
  LPDF.TextBox(x1, y1, 14, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 28;
  y1 := y - 55;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Número';
  LPDF.TextBox(x1, y1, 14, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 28;
  y1 := y - 51;
  LPDF.SetFont('Arial', '', 9);
  LTexto := FormatarNumeroDocumentoFiscal(IntToStr(LMDFE.Ide.nMDF));
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 50;
  y1 := y - 55;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'FL';
  LPDF.TextBox(x1, y1, 14, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 50;
  y1 := y - 51;
  LPDF.SetFont('Arial', '', 9);
  LTexto := IntToStr(LPDF.CurrentPage) + '/' + IntToStr(Args.TotalPages);
  LPDF.TextBox(x1, y1, 14, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 60;
  y1 := y - 55;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Data e hora de Emissão';
  LPDF.TextBox(x1, y1, 35, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 60;
  y1 := y - 51;
  LPDF.SetFont('Arial', '', 10);
  LTexto := DateTimeToStr(LMDFE.Ide.dhEmi);
  LPDF.TextBox(x1, y1, 35, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 97;
  y1 := y - 55;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'UF Carreg.';
  LPDF.TextBox(x1, y1, 18, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 97;
  y1 := y - 51;
  LPDF.SetFont('Arial', '', 9);
  LTexto := LMDFE.Ide.UFIni;
  LPDF.TextBox(x1, y1, 18, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 117;
  y1 := y - 55;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'UF Descarreg.';
  LPDF.TextBox(x1, y1, 18, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 117;
  y1 := y - 51;
  LPDF.SetFont('Arial', '', 9);
  LTexto := LMDFE.Ide.UFFim;
  LPDF.TextBox(x1, y1, 18, 5, LTexto, 'T', 'L', 0, '');

end;

procedure TBlocoDocAuxiliarMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 80;
end;

{ TBlocoModalCargaRodoviarioMDFe }

constructor TBlocoModalCargaRodoviarioMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoModalCargaRodoviarioMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  bW, bH, x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  // Título da carga
  x1 := x;
  y1 := y - 121;
  LPDF.SetFont('Arial', 'B', 12);
  LTexto := 'Modelo Rodoviário de Carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 121;
  LPDF.SetFont('Arial', '', 8);
  LTexto := 'CONTROLE DO FISCO';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Codigo de Barras.
  LPDF.SetFont(6, 'B');
  x1 := x + 92;
  y1 := y - 116;
  bW := 100;
  bH := 18;
  //codigo de barras
  LPDF.Code128(OnlyNumber(LMDFE.infMDFe.Id), x1 , y1, bH, bW);
  //linhas divisorias
  LPDF.SetFont(6, '');

  // Quantidade e Peso
  x1 := x;
  y1 := y - 111;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. CTe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 107;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qCTe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 22;
  y1 := y - 111;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. NFe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 22;
  y1 := y - 107;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qNFe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 44;
  y1 := y - 111;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Peso total (Kg)';
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 44;
  y1 := y - 107;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := FormatFloat('#,0.0000', LMDFE.tot.qCarga);
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');

end;

procedure TBlocoModalCargaRodoviarioMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 80;
end;

{ TBlocoModalCargaAereoMDFe }

constructor TBlocoModalCargaAereoMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoModalCargaAereoMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  bW, bH, x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  // Título da carga
  x1 := x;
  y1 := y - 121;
  LPDF.SetFont('Arial', 'B', 12);
  LTexto := 'Modelo Aéreo de Carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 121;
  LPDF.SetFont('Arial', '', 8);
  LTexto := 'CONTROLE DO FISCO';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Codigo de Barras.
  LPDF.SetFont(6, 'B');
  x1 := x + 92;
  y1 := y - 116;
  bW := 100;
  bH := 18;
  //codigo de barras
  LPDF.Code128(OnlyNumber(LMDFE.infMDFe.Id), x1 , y1, bH, bW);
  //linhas divisorias
  LPDF.SetFont(6, '');

  // Quantidade e Peso
  x1 := x;
  y1 := y - 111;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. CTe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 107;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qCTe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 22;
  y1 := y - 111;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. NFe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 22;
  y1 := y - 107;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qNFe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 44;
  y1 := y - 111;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Peso total (Kg)';
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 44;
  y1 := y - 107;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := FormatFloat('#,0.0000', LMDFE.tot.qCarga);
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');
end;

procedure TBlocoModalCargaAereoMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 80;
end;

{ TBlocoModalCargaAquaviarioMDFe }

constructor TBlocoModalCargaAquaviarioMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoModalCargaAquaviarioMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  bW, bH, x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;


  //Embarcação
  x1 := x;
  y1 := y - 126;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Embarcação';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 121;
  LPDF.SetFont('Arial', '', 10);
  LTexto := '0123456789 - Informação sobre a embarcação';
  LPDF.TextBox(x1, y1, 136, 5, LTexto, 'T', 'L', 0, '');

  // Título da carga
  x1 := x;
  y1 := y - 112;
  LPDF.SetFont('Arial', 'B', 12);
  LTexto := 'Modelo Aquaviário de Carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 112;
  LPDF.SetFont('Arial', '', 8);
  LTexto := 'CONTROLE DO FISCO';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Codigo de Barras.
  LPDF.SetFont(6, 'B');
  x1 := x + 92;
  y1 := y - 106;
  bW := 100;
  bH := 18;
  //codigo de barras
  LPDF.Code128(OnlyNumber(LMDFE.infMDFe.Id), x1 , y1, bH, bW);
  //linhas divisorias
  LPDF.SetFont(6, '');

  // Quantidade e Peso
  x1 := x;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. CTe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 102;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qCTe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 18;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. NFe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 18;
  y1 := y - 102;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qNFe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 36;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. MDFe Ref.';
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 36;
  y1 := y - 102;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qMDFe);
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 62;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Peso total (Kg)';
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 62;
  y1 := y - 102;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := FormatFloat('#,0.0000', LMDFE.tot.qCarga);
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');
end;

procedure TBlocoModalCargaAquaviarioMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 80;
end;

{ TBlocoModalCargaFerroviarioMDFe }

constructor TBlocoModalCargaFerroviarioMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoModalCargaFerroviarioMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  bW, bH, x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  //Informações da Composição do trem.
  x1 := x;
  y1 := y - 127;
  LPDF.SetFont('Arial', 'B', 11);
  LTexto := 'Informações da Composição do trem';
  LPDF.TextBox(x1, y1, 65, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 123;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Prefixo do trem';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 119;
  LPDF.SetFont('Arial', '', 10);
  LTexto := '12345';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 26;
  y1 := y - 123;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Data e hora';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 26;
  y1 := y - 119;
  LPDF.SetFont('Arial', '', 10);
  LTexto := DateTimeToStr(LMDFE.Ide.dhEmi);
  LPDF.TextBox(x1, y1, 35, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 63;
  y1 := y - 123;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Origem do trem';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 63;
  y1 := y - 119;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'SP';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 89;
  y1 := y - 123;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Dest. do trem';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 89;
  y1 := y - 119;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'RJ';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 115;
  y1 := y - 123;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Qtd. de vagões carregados';
  LPDF.TextBox(x1, y1, 45, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 115;
  y1 := y - 119;
  LPDF.SetFont('Arial', '', 10);
  LTexto := '6';
  LPDF.TextBox(x1, y1, 25, 5, LTexto, 'T', 'L', 0, '');

  // Título da carga
  x1 := x;
  y1 := y - 112;
  LPDF.SetFont('Arial', 'B', 12);
  LTexto := 'Modelo Ferroviário de Carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 112;
  LPDF.SetFont('Arial', '', 8);
  LTexto := 'CONTROLE DO FISCO';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Codigo de Barras.
  LPDF.SetFont(6, 'B');
  x1 := x + 92;
  y1 := y - 106;
  bW := 100;
  bH := 18;
  //codigo de barras
  LPDF.Code128(OnlyNumber(LMDFE.infMDFe.Id), x1 , y1, bH, bW);
  //linhas divisorias
  LPDF.SetFont(6, '');

  // Quantidade e Peso
  x1 := x;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. CTe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 102;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qCTe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 22;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Qtd. NFe';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 22;
  y1 := y - 102;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := IntToStr(LMDFE.tot.qNFe);
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 44;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := 'Peso total (Kg)';
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 44;
  y1 := y - 102;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := FormatFloat('#,0.0000', LMDFE.tot.qCarga);
  LPDF.TextBox(x1, y1, 30, 5, LTexto, 'T', 'L', 0, '');
end;

procedure TBlocoModalCargaFerroviarioMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 80;
end;

{ TBlocoProtocoloAutorizacaoMDFe }

constructor TBlocoProtocoloAutorizacaoMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoProtocoloAutorizacaoMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  //Protocolo de autorização
  x1 := x;
  y1 := y - 175;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := 'Protocolo de autorização';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 171;
  LPDF.SetFont('Arial', '', 9);
  LTexto := LMDFE.procMDFe.nProt +' - '+ DateTimeToStr(LMDFE.procMDFe.dhRecbto);
  LPDF.TextBox(x1, y1, 90, 5, LTexto, 'T', 'L', 0, '');

  //chave de acesso
  x1 := x + 92;
  y1 := y - 167;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := 'Chave de Acesso';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 163;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := FormatarChaveAcesso(LMDFE.infMDFe.Id);
  LPDF.TextBox(x1, y1, 90, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 157;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := 'Consulte em: https://dfe-portal.sefazvirtual.rs.gov.br/MDFe/consulta';
  LPDF.TextBox(x1, y1, 90, 5, LTexto, 'T', 'L', 0, '');
end;

procedure TBlocoProtocoloAutorizacaoMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesModalRodoviarioMDFe }

constructor TBlocoTabelaInformacoesModalRodoviarioMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesModalRodoviarioMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  i: Integer;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  // Tabela de Veículo
  x1 := x;
  y1 := y - 146;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Veículo';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Placa';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := LMDFE.rodo.veicTracao.placa;
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 40;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'RNTRC';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 40;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := LMDFE.rodo.infANTT.RNTRC;
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  // Tabela de Condutor
  x1 := x + 92;
  y1 := y - 146;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Condutor';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'CPF';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := FormatarCPF(LMDFE.rodo.veicTracao.condutor.Items[i].CPF);
    end;
  LPDF.TextBox(x1, y1, 20, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 132;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Nome';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 132;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := LMDFE.rodo.veicTracao.condutor.Items[i].xNome;
    end;
  LPDF.TextBox(x1, y1, 50, 5, Trim(LTexto), 'T', 'L', 0, '');

  //Vale Pedágio
  x1 := x;
  y1 := y - 116;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Vale Pedágio';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 110;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Responsável CNPJ';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.seg.Count - 1 do
    begin
      LTexto := FormatarCNPJ(LMDFE.seg.Items[i].CNPJ);
    end;
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 34;
  y1 := y - 110;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Fornecedor CNPJ';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 34;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.infANTT.infCIOT.Count - 1 do
    begin
      LTexto := FormatarCNPJouCPF(LMDFE.rodo.infANTT.infCIOT[i].CNPJCPF);
    end;
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 70;
  y1 := y - 110;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'N° Comprovante';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 70;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.seg.Count - 1 do
    begin
      LTexto := LMDFE.seg.Items[i].nApol;
    end;
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesModalRodoviarioMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesModalAereoMDFe }

constructor TBlocoTabelaInformacoesModalAereoMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesModalAereoMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  // Tabela de Aero
  x1 := x;
  y1 := y - 146;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Aeronave';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Marca de Nacionalidade';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := LMDFE.aereo.nac;
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 40;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Marca da Matricula';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 40;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := LMDFE.aereo.matr;
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  // Tabela de Voo
  x1 := x + 92;
  y1 := y - 146;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Voo';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Número';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := LMDFE.aereo.nVoo;
  LPDF.TextBox(x1, y1, 20, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 132;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Data';
  LPDF.TextBox(x1, y1, 20, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 132;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := DateTimeToStr(LMDFE.aereo.dVoo);
  LPDF.TextBox(x1, y1, 40, 5, Trim(LTexto), 'T', 'L', 0, '');

  //Aeródromo
  x1 := x;
  y1 := y - 116;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Aeródromo';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 110;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Embarque';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := LMDFE.aereo.cAerEmb;
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 110;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Destino';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 106;
  LPDF.SetFont('Arial', '', 9);
  LTexto := LMDFE.aereo.cAerDes;
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesModalAereoMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesModalAquaviarioMDFe }

constructor TBlocoTabelaInformacoesModalAquaviarioMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesModalAquaviarioMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  x, y: double;
  x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;

  //Posição Inicial
  x := 10;
  y := 0;

  // Tabela de Aquaviário
  x1 := x;
  y1 := y - 146;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Dados do Terminal';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Carregamento';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Código Carregamento
  x1 := x;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := '99999999';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Descrição Carregamento
  x1 := x + 30;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := 'Descrição sobre Carregamento';
  LPDF.TextBox(x1, y1, 60, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 92;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Descarregamento';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Código Descarregamento
  x1 := x + 92;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := '99999999';
  LPDF.TextBox(x1, y1, 40, 5, Trim(LTexto), 'T', 'L', 0, '');

  //Descrição Carregamento
  x1 := x + 130;
  y1 := y - 136;
  LPDF.SetFont('Arial', 'B', 9);
  LTexto := 'Descrição sobre Descarregamento';
  LPDF.TextBox(x1, y1, 60, 5, Trim(LTexto), 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesModalAquaviarioMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesModalFerroviarioMDFe }

constructor TBlocoTabelaInformacoesModalFerroviarioMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesModalFerroviarioMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  x, y: double;
  x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;

  //Posição Inicial
  x := 10;
  y := 0;

  //Tabela Ferroviário
  x1 := x;
  y1 := y - 146;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Informações dos vagões';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Série de Identificação
  x1 := x;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Série de ident.';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 135;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'X99';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Número de Identificação
  x1 := x + 28;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Núm. ident.';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 28;
  y1 := y - 135;
  LPDF.SetFont('Arial', '', 10);
  LTexto := '999878958';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Sequencia
  x1 := x + 52;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Seq.';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 52;
  y1 := y - 135;
  LPDF.SetFont('Arial', '', 10);
  LTexto := '999';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  //Ton. Útil
  x1 := x + 68;
  y1 := y - 140;
  LPDF.SetFont('Arial', '', 10);
  LTexto := 'Ton. Útil.';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 68;
  y1 := y - 135;
  LPDF.SetFont('Arial', '', 10);
  LTexto := '999,999';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesModalFerroviarioMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesComposicaoCargaRodoviarioMDFe }

constructor TBlocoTabelaInformacoesComposicaoCargaRodoviarioMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesComposicaoCargaRodoviarioMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  i: Integer;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  //Informações da Composição da Carga
  x1 := x;
  y1 := y - 90;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Informações da Composição da Carga';
  LPDF.TextBox(x1, y1, 100, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações dos documentos fiscais vinculados ao manifesto';
  LPDF.TextBox(x1, y1, 90, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'CTe - ' + FormatarChaveAcesso(LMDFE.infMDFe.Id);
    end;
  LPDF.TextBox(x1, y1, 90, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de transporte';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Rodoviário Tração';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Container';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesComposicaoCargaRodoviarioMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesComposicaoCargaAereoMDFe }

constructor TBlocoTabelaInformacoesComposicaoCargaAereoMDFe.Create(
  AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesComposicaoCargaAereoMDFe.OnDraw(
  Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  i: Integer;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  //Informações da Composição da Carga
  x1 := x;
  y1 := y - 90;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Informações da Composição da Carga';
  LPDF.TextBox(x1, y1, 100, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações dos documentos fiscais vinculados ao manifesto';
  LPDF.TextBox(x1, y1, 90, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'CTe - ' + FormatarChaveAcesso(LMDFE.infMDFe.Id);
    end;
  LPDF.TextBox(x1, y1, 90, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de transporte';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Aeronave';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Outros';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesComposicaoCargaAereoMDFe.OnInit(
  Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesComposicaoCargaAquaviarioMDFe }

constructor TBlocoTabelaInformacoesComposicaoCargaAquaviarioMDFe.Create(
  AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesComposicaoCargaAquaviarioMDFe.OnDraw(
  Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  i: Integer;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  //Informações da Composição da Carga
  x1 := x;
  y1 := y - 90;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Informações da Composição da Carga';
  LPDF.TextBox(x1, y1, 100, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações dos documentos fiscais vinculados ao manifesto';
  LPDF.TextBox(x1, y1, 90, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'CTe - ' + FormatarChaveAcesso(LMDFE.infMDFe.Id);
    end;
  LPDF.TextBox(x1, y1, 90, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de transporte';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Navio';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Outros';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesComposicaoCargaAquaviarioMDFe.OnInit(
  Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoTabelaInformacoesComposicaoCargaFerroviarioMDFe }

constructor TBlocoTabelaInformacoesComposicaoCargaFerroviarioMDFe.Create(
  AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoTabelaInformacoesComposicaoCargaFerroviarioMDFe.OnDraw(
  Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  i: Integer;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  //Informações da Composição da Carga
  x1 := x;
  y1 := y - 90;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Informações da Composição da Carga';
  LPDF.TextBox(x1, y1, 100, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações dos documentos fiscais vinculados ao manifesto';
  LPDF.TextBox(x1, y1, 90, 5, LTexto, 'T', 'L', 0, '');

  x1 := x;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'CTe - ' + FormatarChaveAcesso(LMDFE.infMDFe.Id);
    end;
  LPDF.TextBox(x1, y1, 90, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de transporte';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 90;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Navio';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 84;
  LPDF.SetFont('Arial', 'B', 8);
  LTexto := 'Informações da unidade de carga';
  LPDF.TextBox(x1, y1, 55, 5, LTexto, 'T', 'L', 0, '');

  x1 := x + 145;
  y1 := y - 80;
  LPDF.SetFont('Arial', '', 9);
  for i := 0 to LMDFE.rodo.veicTracao.condutor.Count - 1 do
    begin
      LTexto := 'Outros';
    end;
  LPDF.TextBox(x1, y1, 55, 5, Trim(LTexto), 'T', 'L', 0, '');
end;

procedure TBlocoTabelaInformacoesComposicaoCargaFerroviarioMDFe.OnInit(
  Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

{ TBlocoObservacoesMDFe }

constructor TBlocoObservacoesMDFe.Create(AMDFeUtils: TMDFeUtilsFPDF);
begin
  inherited Create(btData);
  FMDFeUtils := AMDFeUtils;
end;

procedure TBlocoObservacoesMDFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  LPDF: IFPDF;
  LMDFE: TMDFe;
  x, y: double;
  x1, y1: double;
  LTexto: string;
begin
  LPDF := Args.PDF;
  LMDFE := FMDFeUtils.MDFe;

  //Posição Inicial
  x := 10;
  y := 0;

  //Observações
  x1 := x;
  y1 := y - 39;
  LPDF.SetFont('Arial', 'B', 10);
  LTexto := 'Observações';
  LPDF.TextBox(x1, y1, 40, 5, LTexto, 'T', 'L', 0, '');

  LPDF.SetFont('Arial', '', 9);
  LTexto := LMDFE.infAdic.infCpl;
  x1 := x;
  y1 := y - 33;
  LPDF.TextBox(x1, y1, 190, 50, LTexto, 'T', 'L', 1, '');
end;

procedure TBlocoObservacoesMDFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 1;
end;

end.

