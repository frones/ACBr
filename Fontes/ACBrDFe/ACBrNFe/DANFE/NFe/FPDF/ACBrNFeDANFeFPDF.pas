{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Arimateia Jr - https://nuvemfiscal.com.br       }
{                              Victor H. Gonzales - Pandaaa                    }
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

unit ACBrNFeDANFeFPDF;

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
  pcnNFe,
  pcnConversao,
  pcnConversaoNFe,
  ACBrValidador,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeUtil,
  ACBrUtil.Compatibilidade,
  ACBrNFeUtilsFPDF, ACBrNFeDANFEClass, ACBrNFe, ACBrBase, ACBrDFeDANFeReport;

type
  TNFeDANFeFPDF = class(TFPDFReport)
  private
    FNFeUtils: TNFeUtilsFPDF;
    FDANFEClassOwner : TACBrNFeDANFEClass;
    FCancelada: boolean;
    FCanhoto: TPosRecibo;
    FLogo: TBytes;
    FLogoStretched: boolean;
    FLogoAlign: TLogoAlign;
    FMensagemRodape: string;
    FInitialized: boolean;
  protected
    procedure OnStartReport(Args: TFPDFReportEventArgs); override;
  public
    constructor Create(ANFe: TNFe; AACBrNFeDANFEClass : TACBrNFeDANFEClass); reintroduce;
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
  TACBrNFeDANFeFPDF = class(TACBrNFeDANFEClass)
  private
    FFPDFReport: TNFeDANFeFPDF;
    FDANFEClassOwner: TACBrNFeDANFEClass;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Adicione propriedades, métodos e eventos específicos do componente
    procedure ImprimirDANFE(NFE: TNFe = nil); override;
    procedure ImprimirDANFEResumido(NFE: TNFe = nil); override;
    procedure ImprimirDANFEPDF(NFE: TNFe = nil); override;
    procedure ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe = nil); override;
    procedure ImprimirEVENTO(NFE: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(NFE: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(AStream: TStream; ANFe: TNFe = nil); override;
    procedure ImprimirINUTILIZACAO(NFE: TNFe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(NFE: TNFe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(AStream: TStream; ANFe: TNFe = nil); override;
  published
    // Declare propriedades publicamente acessíveis aqui
  end;
implementation

type
  TBlocoCanhoto = class(TFPDFBand)
  private
    FAlign: TPosRecibo;
    FNFeUtils: TNFeUtilsFPDF;
    procedure DrawCanhoto(Args: TFPDFBandDrawArgs; vX, vY, vW, vH: double);
    procedure DrawTopBottom(Args: TFPDFBandDrawArgs);
    procedure DrawLeft(Args: TFPDFBandDrawArgs);
    procedure DrawRight(Args: TFPDFBandDrawArgs);
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(AAlign: TPosRecibo; ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoDadosNFe = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;

    FLogo: TBytes;
    FLogoStretched: boolean;
    FLogoAlign: TLogoAlign;
    FImageUtils: TImageUtils;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF; ALogo: TBytes;
      ALogoStretched: boolean; ALogoAlign: TLogoAlign); reintroduce;
    destructor Destroy; override;
  end;

  TBlocoDestinatarioRemetente = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoLocal = class (TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure DrawLocal(Args: TFPDFBandDrawArgs;
      const ATituloBloco, ACNPJCPF, AXNome, AXLgr, ANro, AXCpl, AXBairro: string;
      ACMun: integer; const AXMun, AUF: string; ACEP, ACPais: integer;
      const AXPais, AFone, AEmail, AIE: string);
    property NFeContext: TNFeUtilsFPDF read FNFeUtils;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoLocalEntrega = class(TBlocoLocal)
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  end;

  TBlocoLocalRetirada = class(TBlocoLocal)
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  end;

  TBlocoFaturaDuplicatas = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
    FDuplicatasQtdMax: integer;
    FDuplicatasMaxLinhas: integer;
    function SizeExtraTextoFatura: integer;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoPagamentos = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoCalculoImposto = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoTransporte = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoProdutosServicos = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
    FHCabecItens: double;
    FCurrentItem: integer;

    function CalculateHeightProduto(PDF: IFPDF; Item: integer;
      AMostrarUnidadeTributavel: boolean = False): double;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoCalculoISSQN = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoDadosAdicionais = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;

    function GetEspacoVerticalDadosAdicionais(const textoAdic: string): double;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF); reintroduce;
  end;

  TBlocoRodape = class(TFPDFBand)
  private
    FMensagem: string;
  protected
    procedure OnInit(Args: TFPDFBandInitArgs); override;
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(const AMensagem: string); reintroduce;
  end;

  TBlocoMarcaDagua = class(TFPDFBand)
  private
    FNFeUtils: TNFeUtilsFPDF;
    FCancelada: boolean;
    function Watermark(Args: TFPDFBandDrawArgs; x, y, w, h: double; const ATexto: string;
      const AFontFamily: string; AFontSize: Double = 50;
      const AFontStyle: string = 'B'): double; overload;
    function Watermark(Args: TFPDFBandDrawArgs; x, y, w, h: double; const ATexto: string;
      AFontSize: Double = 50; const AFontStyle: string = 'B'): double; overload;
  protected
    procedure OnDraw(Args: TFPDFBandDrawArgs); override;
  public
    constructor Create(ANFeUtils: TNFeUtilsFPDF; ACancelada: boolean); reintroduce;
  end;

const
  cDefaultFontFamily = 'Times';

{ TBlocoCanhoto }

constructor TBlocoCanhoto.Create(AAlign: TPosRecibo;
  ANFeUtils: TNFeUtilsFPDF);
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

  FNFeUtils := ANFeUtils;
end;

procedure TBlocoCanhoto.DrawTopBottom(Args: TFPDFBandDrawArgs);
begin
  DrawCanhoto(Args, 0, 0, Width, 10);
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

procedure TBlocoCanhoto.DrawCanhoto(Args: TFPDFBandDrawArgs; vX, vY, vW, vH: double);
var
  NFe: TNFe;
  PDF: IFPDF;

  procedure aFont;
  begin
    PDF.SetFont(cDefaultFontFamily, '', 7);
  end;

  procedure aFontSmall;
  begin
    PDF.SetFont(cDefaultFontFamily, '', 6);
  end;

var
  oldX, x, y, w, h, w1, w2, w3: double;
  x1: double;
  texto: string;
  Emitente, Destinatario, numNF, serie: string;
begin
  NFe := FNFeUtils.NFe;
  PDF := Args.PDF;

  x := vX;
  y := vY;

  oldX := x;

  if FAlign in [prRodape] then
  begin
    y := y + 1;
    PDF.DashedLine(x, y, x + vW, y, 1);
    y := y + 1;
  end;

  if NFe.Ide.tpNF = tnEntrada then
  begin
    Emitente :=
      NFe.Dest.xNome + ' - ' +
      NFe.Dest.EnderDest.xLgr + ', ' +
      NFe.Dest.EnderDest.nro + ' - ' +
      IfThen(NFe.Dest.EnderDest.xCpl <> '', NFe.Dest.EnderDest.xCpl + ' - ') +
      NFe.Dest.EnderDest.xBairro + ' ' +
      NFe.Dest.EnderDest.xMun + '-' +
      NFe.Dest.EnderDest.UF;
    Destinatario := NFe.Emit.xNome + ' ';
  end
  else
  begin
    Emitente := NFe.Emit.xNome + ' ';
    Destinatario :=
      NFe.Dest.xNome + ' - ' +
      NFe.Dest.EnderDest.xLgr + ', ' +
      NFe.Dest.EnderDest.nro + ' - ' +
      IfThen(NFe.Dest.EnderDest.xCpl <> '', NFe.Dest.EnderDest.xCpl + ' - ') +
      NFe.Dest.EnderDest.xBairro + ' ' +
      NFe.Dest.EnderDest.xMun + '-' +
      NFe.Dest.EnderDest.UF + ' ';
  end;
  //identificação do sistema emissor
  //linha separadora do canhoto

  w := RoundTo(vW * 0.81, 0);
  h := vH;
  //desenha caixa
  texto := '';
  aFont;
  PDF.TextBox(x, y, w, h, texto, 'C', 'L', 1, '', false);

  if TACBrNFeDANFEClass(FNFeUtils.DANFEClassOwner).FormatarNumeroDocumento then
    numNF := FormatarNumeroDocumentoFiscal(IntToStr(NFe.Ide.NNF))
  else
    numNF := IntToStr(NFe.Ide.NNF);

  serie := FormatFloat('000', NFe.Ide.serie, FNFeUtils.FormatSettings);

  texto :=
    Format(
     'RECEBEMOS DE %s OS PRODUTOS E/OU SERVIÇOS CONSTANTES DA NOTA FISCAL ELETRÔNICA INDICADA %s. ' +
     'EMISSÃO: %s VALOR TOTAL: R$ %s DESTINATÁRIO: %s'
     ,
    [
      Emitente, IfThen(PDF.Orientation = poPortrait, 'ABAIXO', 'AO LADO'),
      FormatDateTime('dd/mm/yyyy', NFe.Ide.dEmi, FNFeUtils.FormatSettings),
      FormatFloat('#,0.00', NFe.Total.ICMSTot.vNF, FNFeUtils.FormatSettings),
      Destinatario
    ]);

  PDF.TextBox(x, y, w-1, h, texto, 'C', 'L', 0, '', false);
  x1 := x + w;
  w1 := vW - w;
  texto := 'NF-e';
  PDF.SetFont(14, 'B');
  PDF.TextBox(x1, y, w1, 18, texto, 'T', 'C', 1, '');
  texto := 'Nº. ' + numNF + sLineBreak;
  texto := texto + 'Série ' + serie;
  PDF.SetFont(10, 'B');
  PDF.TextBox(x1, y + 2, w1, 18, texto, 'C', 'C', 0, '');
  //DATA DE RECEBIMENTO
  texto := 'DATA DE RECEBIMENTO';
  y := y + h;
  w2 := RoundTo(vW*0.17, 0); //35;
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, 8, texto, 'T', 'L', 1, '');
  //IDENTIFICAÇÃO E ASSINATURA DO RECEBEDOR
  x := x + w2;
  w3 := w-w2;
  texto := 'IDENTIFICAÇÃO E ASSINATURA DO RECEBEDOR';
  PDF.TextBox(x, y, w3, 8, texto, 'T', 'L', 1, '');
  x := oldX;

  if FAlign in [  prCabecalho, prEsquerda] then
  begin
    y := y + 9;
    PDF.DashedLine(x, y, x + vW, y, 1);
  end;
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

procedure TBlocoCanhoto.OnInit(Args: TFPDFBandInitArgs);
begin
  case FAlign of
    prCabecalho,
    prRodape:
      Height := 20;
    prEsquerda:
      Width := 20;
  else
    //
  end;

  // Por causa do double pass
  Visible := True;
end;

{ TBlocoDadosNFe }

constructor TBlocoDadosNFe.Create(ANFeUtils: TNFeUtilsFPDF;
  ALogo: TBytes; ALogoStretched: boolean; ALogoAlign: TLogoAlign);
begin
  inherited Create(btPageHeader);
  FNFeUtils := ANFeUtils;
  FLogo := ALogo;
  FLogoStretched := ALogoStretched;
  FLogoAlign := ALogoAlign;
  FImageUtils := TImageUtils.Create;
end;

destructor TBlocoDadosNFe.Destroy;
begin
  FImageUtils.Free;
  inherited;
end;

procedure TBlocoDadosNFe.OnDraw(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  NFe: TNFe;
  OldX, OldY: double;
  x, y, w, h, w1, w2, w3, wx: double;
  HEmit1, HEmit2: double;
  Emit1FontSize: double;
  nImgW, nImgH, xImg, yImg, logoWmm, logoHmm: double;
  logoW, logoH: word;
  x1, y1, tw, th, bW, bH: double;
  emitente_conteudo1, emitente_conteudo2: string;
  texto, cabecalhoProtoAutorizacao, chaveContingencia: string;
  CampoVariavel1Descricao, CampoVariavel1Valor: string;
  CampoVariavel2Descricao, CampoVariavel2Valor: string;
  cStat: string;
  HasLogo: boolean;
  Stream: TMemoryStream;
  LLogoStringStream : TStringStream;
begin
  PDF := Args.PDF;
  NFe := FNFeUtils.NFe;

  x := 0;
  y := 0;

  OldX := x;
  OldY := y;
  HasLogo := False;

  //####################################################################################
  //coluna esquerda identificação do emitente
  w := RoundTo(Width*0.41, 0);
  if PDF.Orientation = poPortrait then
    PDF.SetFont(6, 'I')
  else
    PDF.SetFont(8, 'B');

  w1 := w;
  h := 32;
  oldY := oldY + h;
  PDF.TextBox(x, y, w, h);
  texto := 'IDENTIFICAÇÃO DO EMITENTE';
  PDF.TextBox(x, y, w, 5, texto, 'T', 'C', 0, '');

  x1 := x;
  //y1 := RoundTo(h / 3 + y, 0);
  y1 := y;
  tw := w;
  th := h;
  
  //estabelecer o alinhamento
  //pode ser left L, center C, right R, full logo L
  //se for left separar 1/3 da largura para o tamanho da imagem
  //os outros 2/3 serão usados para os dados do emitente
  //se for center separar 1/2 da altura para o logo e 1/2 para os dados
  //se for right separa 2/3 para os dados e o terço seguinte para o logo
  //se não houver logo centraliza dos dados do emitente
  // coloca o logo
  if Args.FinalPass and (Length(FLogo) > 0) and FImageUtils.GetImageSize(FLogo, logoW, logoH) then
  begin
    HasLogo := True;
    xImg := 0;
    yImg := 0;
    nImgW := 0;
    nImgH := 0;
    logoWmm := (logoW/72)*25.4;
    logoHmm := (logoH/72)*25.4;
    if FLogoAlign = laLeft then
    begin
      nImgW := RoundTo(w/3, 0);
      if FLogoStretched then
        nImgH := h - 5
      else
        nImgH := RoundTo(logoHmm * (nImgW/logoWmm), 0);
      if nImgH > (h - 5) then
      begin
        nImgH := h - 5;
        nImgW := RoundTo(logoWmm * (nImgH/logoHmm), 0);
      end;
      xImg := x + 1;
      yImg := RoundTo((h - nImgH) / 2, 0) + y;
      //estabelecer posições do texto
      x1 := RoundTo(xImg + nImgW + 1, 0);
      //y1 := RoundTo(h / 3 + y, 0);
      tw := RoundTo(2 * w/3, 0);
    end
    else if FLogoAlign = laCenter then
    begin
      nImgH := RoundTo(h/3, 0);
      if FLogoStretched then
        nImgW := w
      else
        nImgW := RoundTo(logoWmm * (nImgH/logoHmm), 0);
      xImg := RoundTo((w-nImgW)/2+x, 0);
      yImg := y+3;
      x1 := x;
      //y1 := RoundTo(yImg + nImgH + 1, 0);
      y1 := y1 + nImgH;
      tw := w;
      th := th - nImgH;
    end
    else if FLogoAlign = laRight then
    begin
      nImgW := RoundTo(w/3, 0);
      if FLogoStretched then
        nImgH := h - 5
      else
        nImgH := RoundTo(logoHmm * (nImgW/logoWmm), 0);
      if nImgH > (h - 5) then
      begin
        nImgH := h - 5;
        nImgW := RoundTo(logoWmm * (nImgH/logoHmm), 0);
      end;
      xImg := RoundTo(x+(w-(1+nImgW)), 0);
      yImg := RoundTo((h-nImgH)/2, 0)+y;
      x1 := x;
      //y1 := RoundTo(h/3+y, 0);
      tw := RoundTo(2*w/3, 0);
    end
    else if FLogoAlign = laFull then
    begin
      nImgH := RoundTo(h - 5, 0);
      if FLogoStretched then
        nImgW := RoundTo(w - 5, 0)
      else
      begin
        nImgW := RoundTo(logoWmm * (nImgH/logoHmm), 0);
        if nImgW > RoundTo(w, 0) then
        begin
          nImgW := RoundTo(w - 5, 0);
          nImgH := RoundTo(logoHmm * (nImgW/logoWmm), 0);
        end;
      end;
      xImg := RoundTo((w-nImgW)/2+x, 0);
      yImg := RoundTo((h - nImgH) / 2, 0) + y;
      x1 := x;
      //y1 := RoundTo(yImg + nImgH + 1, 0);
      tw := w;
    end;

    if (Length(FLogo) > 0) then
    begin
      Stream := TMemoryStream.Create;
      try
        Stream.Write(FLogo[0], Length(FLogo));
        PDF.Image(xImg, yImg, nImgW, nImgH, Stream);
      finally
        Stream.Free;
      end;
    end;
  end;

  // monta as informações apenas se diferente de full logo
  if (not HasLogo) or (FLogoAlign <> laFull) then
  begin
    emitente_conteudo1 := NFe.Emit.xNome;
    emitente_conteudo2 := NFe.Emit.EnderEmit.xLgr + ', ' + NFe.Emit.EnderEmit.nro;
    if NFe.Emit.EnderEmit.xCpl <> '' then
      emitente_conteudo2 := emitente_conteudo2 + ' - ' + NFe.Emit.EnderEmit.xCpl;
    emitente_conteudo2 := emitente_conteudo2 + sLineBreak +
      NFe.Emit.EnderEmit.xBairro + ' - ' +
      NFe.Emit.EnderEmit.xMun + ' - ' +
      NFe.Emit.EnderEmit.UF + sLineBreak +
      'CEP: ' + IfThen(NFe.Emit.EnderEmit.CEP > 0, FormatarCEP(NFe.Emit.EnderEmit.CEP));
    if NFe.Emit.EnderEmit.fone <> '' then
      emitente_conteudo2 := emitente_conteudo2 + sLineBreak + 'Fone/Fax: ' + NFe.Emit.EnderEmit.fone;

    x1 := x1 + 1;
    tw := tw - 2;

    PDF.SetFont(8, '');
    PDF.WordWrap(emitente_conteudo2, tw);
    HEmit2 := PDF.GetStringHeight(emitente_conteudo2, tw);

    Emit1FontSize := 12.5;
    repeat
      if Emit1FontSize <= 8 then
        Break;
      Emit1FontSize := Emit1FontSize - 0.5;
      texto := emitente_conteudo1;
      PDF.SetFont(Emit1FontSize, 'B');
      PDF.WordWrap(texto, tw);
      HEmit1 := PDF.GetStringHeight(texto, tw);
    until HEmit1 + HEmit2 < th - 2;

    //Nome emitente
    PDF.SetFont(Emit1FontSize, 'B');

    y1 := y1 + ((th - (HEmit1 + HEmit2)) / 2) + 1;

    y1 := y1 + PDF.TextBox(x1, y1, tw, HEmit1, emitente_conteudo1, 'T', 'C', False);
    //endereço
    //y1 := y1 + 5;
    PDF.SetFont(8, '');
    PDF.TextBox(x1, y1, tw, HEmit2, emitente_conteudo2, 'T', 'C', 0, '');
  end;

  //####################################################################################
  //coluna central Danfe
  x := x + w;
  w := RoundTo(Width * 0.17, 0);//35;
  w2 := w;
  h := 32;
  PDF.TextBox(x, y, w, h);

//  if (! Self.pNotaCancelada()) {
      // A PRINCIPIO NÃO PRECISAVA, POIS A NFE ESTÁ AUTORIZADA,
      // SÓ SE RETIRA O DANFE PARA NOTAS NÃO AUTORIZADAS
      texto := 'DANFE';
      PDF.SetFont(14, 'B');
      PDF.TextBox(x, y+1, w, h, texto, 'T', 'C', 0, '');
      PDF.SetFont(8, '');
      texto := 'Documento Auxiliar da Nota Fiscal Eletrônica';
      h := 20;
      PDF.TextBox(x, y+6, w, h, texto, 'T', 'C', 0, '', false);
//  }
//
  PDF.SetFont(8, '');
  texto := '0 - ENTRADA';
  y1 := y + 14;
  h := 8;
  PDF.TextBox(x+2, y1, w, h, texto, 'T', 'L', 0, '');
  texto := '1 - SAÍDA';
  y1 := y + 17;
  PDF.TextBox(x+2, y1, w, h, texto, 'T', 'L', 0, '');
  //tipo de nF
  PDF.SetFont(12, 'B');
  y1 := y + 14;
  h := 6;
  texto := tpNFToStr(NFe.Ide.tpNF);
  PDF.TextBox(x+27, y1, 5, h, texto, 'C', 'C', 1, '');

  //numero da NF
  PDF.SetFont(10, 'B');
  y1 := y + 22;
  texto := 'Nº. ' + FormatarNumeroDocumentoFiscal(IntToStr(NFe.Ide.NNF));
  PDF.TextBox(x, y1, w, h, texto, 'C', 'C', 0, '');
  //Série
  y1 := y + 25;
  texto := 'Série ' + FormatFloat('000', NFe.Ide.serie, FNFeUtils.FormatSettings);
  PDF.TextBox(x, y1, w, h, texto, 'C', 'C', 0, '');
  //numero paginas
  PDF.SetFont(8, 'I');
  y1 := y + 28;
  texto := 'Folha ' + IntToStr(PDF.CurrentPage) + '/' + IntToStr(Args.TotalPages);
  PDF.TextBox(x, y1, w, h, texto, 'C', 'C', 0, '');

  //####################################################################################
  //coluna codigo de barras
  x := x + w;
  w := (Width-w1-w2);//85;
  w3 := w;
  h := 32;
  PDF.TextBox(x, y, w, h);
  PDF.SetFillColor(0, 0, 0);
  bW := 75;
  bH := 12;
  //codigo de barras
  PDF.Code128(OnlyNumber(NFe.infNFe.Id), x +((w-bW)/2), y + 2, bH, bW);
  //linhas divisorias
  PDF.Line(x, y+4+bH, x+w, y+4+bH);
  PDF.Line(x, y+12+bH, x+w, y+12+bH);
  PDF.SetFont(6, '');
  y1 := y+4+bH;
  h := 7;
  texto := 'CHAVE DE ACESSO';
  PDF.TextBox(x, y1, w, h, texto, 'T', 'L', 0, '');
  PDF.SetFont(10, 'B');
  y1 := y+8+bH;
  texto := FormatarChaveAcesso(NFe.infNFe.Id);
  PDF.TextBox(x + 2, y1 - 1, w - 4, h, texto, 'T', 'C', 0, '');
  y1 := y+12+bH;
  PDF.SetFont(8, '');
  if FNFeUtils.NotaEPEC then
    cabecalhoProtoAutorizacao := 'PROTOCOLO DE AUTORIZAÇÃO DO EPEC'
  else
    cabecalhoProtoAutorizacao := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

  if (NFe.Ide.tpEmis in [teContingencia, teFSDA]) and not FNFeUtils.NotaEPEC then
  begin
    cabecalhoProtoAutorizacao := 'DADOS DA NF-E';
    chaveContingencia := FNFeUtils.GetChaveContingencia;
    PDF.SetFillColor(0, 0, 0);
    PDF.Code128(chaveContingencia, x +((w-bW)/2) + 4, y1+1, bH/2, bW*0.9);
  end
  else
  begin
    texto := 'Consulta de autenticidade no portal nacional da NF-e';
    PDF.TextBox(x+2, y1, w-2, h, texto, 'T', 'C', 0, '');
    y1 := y+16+bH;
    texto := 'www.nfe.fazenda.gov.br/portal ou no site da Sefaz Autorizadora';
    PDF.TextBox(x+2, y1, w-2, h, texto, 'T', 'C', 0,
        'http://www.nfe.fazenda.gov.br/portal ou no site da Sefaz Autorizadora');
  end;

  //####################################################################################
  //Dados da NF do cabeçalho
  //natureza da operação
  texto := 'NATUREZA DA OPERAÇÃO';
  PDF.SetFont(6, '');
  w := w1+w2;
  y := oldY;
  x := oldX;
  h := 7;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.Ide.natOp;
  PDF.SetFont(10, 'B');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  x := x + w;
  w := w3;

  //PROTOCOLO DE AUTORIZAÇÃO DE USO ou DADOS da NF-E
//  Self.SetFont(6, '');
//  PDF.TextBox(x, y, w, h, cabecalhoProtoAutorizacao, 'T', 'L', 1, '');
  // algumas NFe podem estar sem o protocolo de uso portanto sua existencia deve ser
  // testada antes de tentar obter a informação.
  // NOTA : DANFE sem protocolo deve existir somente no caso de contingência !!!
  // Além disso, existem várias NFes em contingência que eu recebo com protocolo de autorização.
  // Na minha opinião, deveríamos mostra-lo, mas o  manual  da NFe v4.01 diz outra coisa...

  CampoVariavel1Descricao := '';
  CampoVariavel1Valor := '';
  CampoVariavel2Descricao := '';
  CampoVariavel2Valor := '';
  case NFe.Ide.tpEmis of
    teNormal,
    teSVCAN,
    teSCAN,
    teSVCRS,
    teSVCSP:
      begin
//        if NotaCancelada then
//          CampoVariavel2Descricao := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO' )
//        else
        if FNFeUtils.NotaDenegada then
          CampoVariavel2Descricao := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO')
        else
          CampoVariavel2Descricao := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');

//        if EstaVazio(FDANFEClassOwner.Protocolo) then
//        begin
          if NFe.procNFe.nProt = '' then
            CampoVariavel2Valor := ACBrStr('NFe sem Autorização de Uso da SEFAZ')
          else
            CampoVariavel2Valor :=
              NFe.procNFe.nProt + ' ' +
              IfThen(NFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(NFe.procNFe.dhRecbto), '');
//        end
//        else
//        begin
//          FieldByName('Contingencia_Valor').AsString := FDANFEClassOwner.Protocolo;
//          P := Pos('-', FDANFEClassOwner.Protocolo);
//          if P = 0 then
//          begin
//            FieldByName('nProt').AsString := Trim(FDANFEClassOwner.Protocolo);
//            FieldByName('dhRecbto').AsDateTime := 0;
//          end
//          else
//          begin
//            FieldByName('nProt').AsString := Trim(Copy(FDANFEClassOwner.Protocolo, 1, P - 1));
//            FieldByName('dhRecbto').AsDateTime := StringToDateTimeDef(Trim(
//              Copy(FDANFEClassOwner.Protocolo, P + 1, Length(FDANFEClassOwner.Protocolo) - P)
//              ), 0, 'dd/mm/yyyy hh:nn:ss');
//          end;
//        end;
      end;

    teContingencia,
    teFSDA :
      begin
        CampoVariavel2Descricao := ACBrStr('DADOS DA NF-E');
        CampoVariavel2Valor := chaveContingencia;
      end;

    teDPEC:
      begin
        if NFe.procNFe.nProt <> '' then // DPEC TRANSMITIDO
        begin
          CampoVariavel2Descricao := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DO EPEC');
          CampoVariavel2Valor := NFe.procNFe.nProt + ' ' +
            IfThen(NFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(NFe.procNFe.dhRecbto), '');
        end
        else
        begin
          CampoVariavel2Descricao := ACBrStr('NÚMERO DE REGISTRO DPEC');
          CampoVariavel2Valor := '';
//            if NaoEstaVazio(FDANFEClassOwner.Protocolo) then
//              FieldByName('Contingencia_Valor').AsString := FDANFEClassOwner.Protocolo;
        end;
      end;
//
//     teOffLine:
//       begin
//        FieldByName('Contingencia_Valor').AsString := NFe.procNFe.nProt + ' ' + IfThen(NFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(NFe.procNFe.dhRecbto), '');
//        FieldByName('nProt').AsString := NFe.procNfe.nProt;
//        FieldByName('dhRecbto').AsDateTime := NFe.procNFe.dhRecbto;
//      end;
  end;

  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, CampoVariavel2Descricao, 'T', 'L', 1, '');
  PDF.SetFont(10, 'B');
  PDF.TextBox(x, y, w, h, CampoVariavel2Valor, 'B', 'C', 0, '');

  //####################################################################################
  //INSCRIÇÃO ESTADUAL
  w := RoundTo(Width * 0.333, 0);
  y := y + h;
  x := oldX;
  texto := 'INSCRIÇÃO ESTADUAL';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.Emit.IE;
  PDF.SetFont(10, 'B');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //INSCRIÇÃO ESTADUAL DO SUBST. TRIBUT.
  x := x + w;
  texto := 'INSCRIÇÃO ESTADUAL DO SUBST. TRIBUT.';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.Emit.IEST;
  PDF.SetFont(10, 'B');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //CNPJ
  x := x + w;
  w := (Width-(2*w));
  texto := 'CNPJ';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := FormatarCNPJouCPF(NFe.Emit.CNPJCPF);
  PDF.SetFont(10, 'B');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
end;

procedure TBlocoDadosNFe.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 46;
end;

{ TBlocoDestinatarioRemetente }

constructor TBlocoDestinatarioRemetente.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
end;

procedure TBlocoDestinatarioRemetente.OnDraw(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  NFe: TNFe;

  procedure SetFontBoxHeader;
  begin
    PDF.SetFont(6, '');
  end;

  procedure SetFontBoxContent;
  begin
    PDF.SetFont(10, '');
  end;

  procedure SetFontBoxContentBold;
  begin
    PDF.SetFont(10, 'B');
  end;

var
  x, y: double;
  oldX, w, h, w1, w2, w3, wx: double;
  texto: string;
begin
  PDF := Args.PDF;
  NFe := FNFeUtils.NFe;

  x := 0;
  y := 0;
  y := y + 1;

  //DESTINATÁRIO / REMETENTE
  oldX := x;

  w := Width;
  h := 7;
  texto := 'DESTINATÁRIO / REMETENTE';
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 0, '');
  //NOME / RAZÃO SOCIAL
  w := RoundTo(Width * 0.61, 0);
  w1 := w;
  y := y + 3;
  texto := 'NOME / RAZÃO SOCIAL';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.Dest.xNome;
  SetFontBoxContent;
  if PDF.Orientation = poPortrait then
    PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '')
  else
    PDF.TextBox(x, y, w, h, texto, 'B', 'L', 1, '');
  //CNPJ / CPF
  x := x + w;
  w := roundto(Width*0.23, 0);
  w2 := w;
  texto := 'CNPJ / CPF';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  //Pegando valor do CPF/CNPJ
  texto := FormatarCNPJouCPF(NFe.Dest.CNPJCPF);
  SetFontBoxContentBold;
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //DATA DA EMISSÃO
  x := x + w;
  w := Width-(w1+w2);
  wx := w;
  texto := 'DATA DA EMISSÃO';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := FormatDateBr(NFe.Ide.DEmi);
  SetFontBoxContent;
  if PDF.Orientation = poPortrait then
    PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '')
  else
    PDF.TextBox(x, y, w, h, texto, 'B', 'C', 1, '');

  //ENDEREÇO
  w := RoundTo(Width * 0.47, 0);
  w1 := w;
  y := y + h;
  x := oldX;
  texto := 'ENDEREÇO';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.dest.EnderDest.xLgr;
  texto := texto + ', ' + NFe.dest.EnderDest.nro;
  if NFe.dest.EnderDest.xCpl <> '' then
    texto := texto + ' - ' + NFe.dest.EnderDest.xCpl;
  SetFontBoxContent;
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '', true);
  //BAIRRO / DISTRITO
  x := x + w;
  w := RoundTo(Width*0.21, 0);
  w2 := w;
  texto := 'BAIRRO / DISTRITO';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.dest.EnderDest.xBairro;
  SetFontBoxContent;
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //CEP
  x := x + w;
  w := Width - w1 - w2 - wx;
  texto := 'CEP';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := IfThen(NFe.dest.EnderDest.CEP > 0, FormatarCEP(NFe.dest.EnderDest.CEP));
  SetFontBoxContent;
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //DATA DA SAÍDA
  x := x + w;
  w := wx;
  texto := 'DATA DA SAÍDA/ENTRADA';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := IfThen(NFe.Ide.DSaiEnt <> 0, FormatDateBr(NFe.Ide.DSaiEnt));
  SetFontBoxContentBold;
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');
  //MUNICÍPIO
  w := w1;
  y := y + h;
  x := oldX;
  texto := 'MUNICÍPIO';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.dest.EnderDest.xMun;
  if (UpperCase(Trim(texto)) = 'EXTERIOR') and (Length(NFe.dest.EnderDest.xPais) > 0) then
    texto := texto + ' - ' + NFe.dest.EnderDest.xPais;
  SetFontBoxContent;
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //UF
  x := x + w;
  w := 8;
  texto := 'UF';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.dest.EnderDest.UF;
  SetFontBoxContent;
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');
  //FONE / FAX
  x := x + w;
  w := RoundTo((Width - w1 - wx - 8) / 2, 0);
  w3 := w;
  texto := 'FONE / FAX';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.dest.EnderDest.fone;
  SetFontBoxContent;
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');
  //INSCRIÇÃO ESTADUAL
  x := x + w;
  w := Width - w1 - wx - 8 - w3;
  texto := 'INSCRIÇÃO ESTADUAL';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.dest.IE;
  SetFontBoxContent;
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');
  //HORA DA SAÍDA
  x := x + w;
  w := wx;
  texto := 'HORA DA SAÍDA/ENTRADA';
  SetFontBoxHeader;
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  if NFe.infNFe.versao = 2.00 then
    texto := IfThen(NFe.ide.hSaiEnt = 0, '', TimeToStr(NFe.ide.hSaiEnt))
  else
    texto := IfThen(TimeOf(NFe.ide.dSaiEnt)=0, '', TimeToStr(NFe.ide.dSaiEnt));
  SetFontBoxContentBold;
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');
end;

procedure TBlocoDestinatarioRemetente.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 25;
end;

{ TBlocoLocalEntrega }

procedure TBlocoLocalEntrega.OnDraw(Args: TFPDFBandDrawArgs);
var
  Entrega: TEntrega;
begin
  Entrega := NFeContext.NFe.Entrega;
  DrawLocal(Args, 'INFORMAÇÕES DO LOCAL DE ENTREGA',
    Entrega.CNPJCPF, Entrega.xNome, Entrega.xLgr, Entrega.nro, Entrega.xCpl,
    Entrega.xBairro, Entrega.cMun, Entrega.xMun, Entrega.UF, Entrega.CEP,
    Entrega.cPais, Entrega.xPais, Entrega.fone, Entrega.Email, Entrega.IE);
end;

procedure TBlocoLocalEntrega.OnInit(Args: TFPDFBandInitArgs);
begin
  inherited OnInit(Args);
  Visible := NFeContext.NFe.Entrega.CNPJCPF <> '';
end;

{ TBlocoLocalRetirada }

procedure TBlocoLocalRetirada.OnDraw(Args: TFPDFBandDrawArgs);
var
  Retirada: TRetirada;
begin
  Retirada := NFeContext.NFe.Retirada;
  DrawLocal(Args, 'INFORMAÇÕES DO LOCAL DE RETIRADA',
    Retirada.CNPJCPF, Retirada.xNome, Retirada.xLgr, Retirada.nro, Retirada.xCpl,
    Retirada.xBairro, Retirada.cMun, Retirada.xMun, Retirada.UF, Retirada.CEP,
    Retirada.cPais, Retirada.xPais, Retirada.fone, Retirada.Email, Retirada.IE);
end;

procedure TBlocoLocalRetirada.OnInit(Args: TFPDFBandInitArgs);
begin
  inherited OnInit(Args);
  Visible := NFeContext.NFe.Retirada.CNPJCPF <> '';
end;

{ TBlocoLocal }

constructor TBlocoLocal.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
end;

procedure TBlocoLocal.DrawLocal(Args: TFPDFBandDrawArgs; const ATituloBloco,
  ACNPJCPF, AXNome, AXLgr, ANro, AXCpl, AXBairro: string; ACMun: integer;
  const AXMun, AUF: string; ACEP, ACPais: integer; const AXPais, AFone, AEmail,
  AIE: string);
var
  PDF: IFPDF;
  x, y: double;
  oldX, w, h, w1, w2, wx: double;
  texto: string;
begin
  PDF := Args.PDF;
  if ACNPJCPF = '' then
    Exit;

  x := 0;
  y := 0;
  y := y + 1;

  //LOCAL
  oldX := x;

  w := Width;
  h := 7;
  texto := ATituloBloco;
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 0, '');
  //NOME / RAZÃO SOCIAL
  w := RoundTo(Width * 0.61, 0);
  w1 := w;
  y := y + 3;
  texto := 'NOME / RAZÃO SOCIAL';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := AXNome;
  PDF.SetFont(10, '');
  if PDF.Orientation = poPortrait then
    PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '')
  else
    PDF.TextBox(x, y, w, h, texto, 'B', 'L', 1, '');
  //CNPJ / CPF
  x := x + w;
  w := RoundTo(Width * 0.23, 0);
  w2 := w;
  texto := 'CNPJ / CPF';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  //Pegando valor do CPF/CNPJ
  texto := FormatarCNPJouCPF(ACNPJCPF);
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //INSCRIÇÃO ESTADUAL
  x := x + w;
  w := Width - (w1 + w2);
  wx := w;
  texto := 'INSCRIÇÃO ESTADUAL';
  PDF.SetFont( 6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := AIE;
  PDF.SetFont(10, '');
  if PDF.Orientation = poPortrait then
    PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '')
  else
    PDF.TextBox(x, y, w, h, texto, 'B', 'C', 1, '');
  //ENDEREÇO
  w := RoundTo(Width * 0.355, 0) + wx;
  w1 := w;
  y := y + h;
  x := oldX;
  texto := 'ENDEREÇO';
  PDF.SetFont( 6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := Format('%s%s%s', [AXLgr, IfThen(ANro <> '', ', ' + ANro), IfThen(AXCpl <> '', ' - ' + AXCpl)]);
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '', true);
  //BAIRRO / DISTRITO
  x := x + w;
  w := RoundTo(Width * 0.335, 0);
  w2 := w;
  texto := 'BAIRRO / DISTRITO';
  PDF.SetFont( 6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := AXBairro;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //CEP
  x := x + w;
  w := Width - (w1 + w2);
  texto := 'CEP';
  PDF.SetFont( 6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := FormatarCEP(ACEP);
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //MUNICÍPIO
  w := RoundTo(Width * 0.805, 0);
  w1 := w;
  y := y + h;
  x := oldX;
  texto := 'MUNICÍPIO';
  PDF.SetFont( 6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := AXMun;
  if (UpperCase(Trim(texto)) = 'EXTERIOR') and (AXPais <> '') then
    texto := texto + ' - ' + AXPais;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //UF
  x := x + w;
  w := 8;
  texto := 'UF';
  PDF.SetFont( 6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := AUF;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');
  //FONE / FAX
  x := x + w;
  w := Width - w - w1;
  texto := 'FONE / FAX';
  PDF.SetFont( 6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := AFone;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');
end;

procedure TBlocoLocal.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 25;
end;

{ TBlocoFaturaDuplicatas }

constructor TBlocoFaturaDuplicatas.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
end;

procedure TBlocoFaturaDuplicatas.OnDraw(Args: TFPDFBandDrawArgs);
const
  cDashBlack = 0.4;
  cDashWhite = 0.8;
var
  NFe: TNFe;
  PDF: IFPDF;
  x, y, h, w, oldX, OldY, myH, myW: double;
  textoFatura, texto: string;
  DupCont, linha, i: integer;
  DupItem: TDupCollectionItem;
begin
  NFe := FNFeUtils.NFe;
  PDF := Args.PDF;

  x := 0;
  y := 0;
  y := y + 1;

  oldx := x;
  textoFatura := FNFeUtils.GetTextoFatura;

  //#####################################################################
  //FATURA / DUPLICATA
  texto := 'FATURA / DUPLICATA';
  w := Width;
  h := 8;
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 0, '');
  y := y + 3;
  //$dups = "";
  DupCont := 0;
  if (textoFatura <> '') and (TACBrNFeDANFEClass(FNFeUtils.DANFEClassOwner).ExibeCampoFatura) then
  begin
    myH := 4;
    myW := Width;
    PDF.SetFont(8, '');
    PDF.TextBox(x, y, myW, myH, textoFatura, 'T', 'L', 1, '');
    y := y + myH + 1;
  end;

  w := (Width - 6) / FDuplicatasQtdMax;

  OldY := y;
  linha := 1;
  for I := 0 to NFe.Cobr.Dup.Count - 1 do
  begin
    y := OldY;
    h := 8;

    Inc(DupCont);

    if DupCont mod FDuplicatasQtdMax = 1 then
    begin
      PDF.SetDash(0);
      PDF.Line(x, y, x + 6, y);
      PDF.Line(x, y, x, y + h);
      PDF.Line(x + 6, y, x + 6, y + h);
      PDF.Line(x + 6 + (w * FDuplicatasQtdMax), y, x + 6 + (w * FDuplicatasQtdMax), y + h);

      if Linha mod FDuplicatasMaxLinhas <> 1 then
        PDF.SetDash(cDashBlack, cDashWhite);
      PDF.Line(x + 6, y, x + 6 + (w * FDuplicatasQtdMax), y);

      PDF.SetFont(6, '');
      PDF.TextBox(x, y, 6, h, 'Num.', 'T', 'L', 0, '');
      y := y + 2.5;
      PDF.TextBox(x, y, 6, h, 'Venc.', 'T', 'L', 0, '');
      y := y + 2.5;
      PDF.TextBox(x, y, 6, h, 'Valor', 'T', 'L', 0, '');
      x := x + 6;
      y := OldY;
    end;

    DupItem := NFe.Cobr.Dup[I];
    texto := '';

    if DupCont mod FDuplicatasQtdMax > 0 then
    begin
      PDF.SetDash(cDashBlack, cDashWhite);
      PDF.Line(x + w, y, x + w, y + h);
    end;

    PDF.SetFont(7, '');
    if StrToIntDef(DupItem.nDup, 0) > 0 then
      PDF.TextBox(x, y, w, h, DupItem.nDup, 'T', 'R', False, False, False)
    else
      PDF.TextBox(x, y, w, h, FormatFloat('000', DupCont), 'T', 'L', 1, '');

    y := y + 2.5;
    PDF.TextBox(x, y, w, h, FormatDateBr(DupItem.dVenc), 'T', 'R', 0, '');

    y := y + 2.5;
    PDF.TextBox(x, y, w, h, FormatFloat('#,0.00', DupItem.vDup, FNFeUtils.FormatSettings), 'T', 'R', 0, '');
    x := x + w;

    if DupCont >= FDuplicatasQtdMax then
    begin
      OldY := OldY + h;
      x := OldX;
      DupCont := 0;
      Inc(linha);
    end;
    if linha > FDuplicatasMaxLinhas then
      break;
  end;
  y := OldY;

  if DupCont = 0 then
  begin
    y := y - h;
  end;

  if NFe.Cobr.Dup.Count > 0 then
  begin
    PDF.SetDash(0);
    PDF.Line(OldX, y + h, OldX + 6 + (w * FDuplicatasQtdMax), y + h);
  end;
end;

procedure TBlocoFaturaDuplicatas.OnInit(Args: TFPDFBandInitArgs);
var
  NFe: TNFe;
  LinhasDup, QtdPagamentos: integer;
begin
  NFe := FNFeUtils.NFe;

  Visible := True;
  if Visible and (NFe.Cobr.Dup.Count = 0) then
    Visible := False;
  if Visible and ((NFe.Cobr.Dup.Count = 0) and (FNFeUtils.GetTextoFatura = '')) then
    Visible := False;
  if not Visible then
    Exit;

  FDuplicatasQtdMax := IfThen(Args.Orientation = poPortrait, 15, 20);
  FDuplicatasMaxLinhas := IfThen(Args.Orientation = poPortrait, 8, 6);

  // Calculate Height
  QtdPagamentos := 0;
  if NFe.Cobr.Dup.Count > 0 then
    QtdPagamentos := NFe.Cobr.Dup.Count
  else if NFe.pag.Count > 0 then
    QtdPagamentos := NFe.pag.Count;
  LinhasDup := QtdPagamentos div FDuplicatasQtdMax;
  if QtdPagamentos mod FDuplicatasQtdMax > 0 then
    Inc(LinhasDup);
  if LinhasDup > FDuplicatasMaxLinhas then
    LinhasDup := FDuplicatasMaxLinhas;

  Height := IfThen(LinhasDup > 0, 4 + (LinhasDup * 8)) + SizeExtraTextoFatura;
end;

function TBlocoFaturaDuplicatas.SizeExtraTextoFatura: integer;
var
  textoFatura: string;
begin
  textoFatura := FNFeUtils.GetTextoFatura;
  //verificar se existem duplicatas
  if (textoFatura <> '') and (FNFeUtils.NFe.Cobr.Dup.Count > 0) then
    Result := 5
  else
    Result := 0;
end;

{ TBlocoPagamentos }

constructor TBlocoPagamentos.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
end;

procedure TBlocoPagamentos.OnDraw(Args: TFPDFBandDrawArgs);
var
  NFe: TNFe;
  PDF: IFPDF;
  x, y, linha, h, w, oldX, Increm: double;
  dups, texto: string;
  dupcont, maxDupCont, i: integer;
  PagItem: TpagCollectionItem;
begin
  NFe := FNFeUtils.NFe;
  PDF := Args.PDF;

  x := 0;
  y := 0;
  y := y + 1;

  linha := 1;
  oldx := x;

  w := Width;

  //Tipo de pagamento
  texto := 'PAGAMENTO';
  if PDF.Orientation = poPortrait then
    maxDupCont := 3
  else
    maxDupCont := 8;
  h := 8;
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 0, '');
  y := y + 3;
  dups := '';
  dupcont := 0;
  if PDF.Orientation = poPortrait then
    w := RoundTo(Width / 3.968, 0) - 1
  else
    w := 28;

  Increm := 0.7;

  //foreach ($this->detPag as $k => $d) {
  for I := 0 to NFe.pag.Count - 1 do
  begin
    PagItem := NFe.pag[I];
    h := 8;
    texto := '';

    if PagItem.tPag = fpSemPagamento then
      Continue;

    PDF.TextBox(x, y, w, h);

    PDF.SetFont(6, '');
    PDF.TextBox(x, y + 0.5, w, h, 'Forma', 'T', 'L', 0, '');
    PDF.SetFont(7, 'B');
    texto := FormaPagamentoToDescricao(PagItem.tPag, PagItem.xPag);
    PDF.TextBox(x, y + 0.5, w, h, texto, 'T', 'R', 0, '');

    PDF.SetFont(6, '');
    PDF.TextBox(x, y + 4, w, h, 'Valor', 'T', 'L', 0, '');
    PDF.SetFont(7, 'B');
    texto := FormatFloat('R$ #,0.00', PagItem.vPag, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y + 4, w, h, texto, 'T', 'R', 0, '');
    x := x + w + increm;
    dupcont := dupcont + 1;

    if dupcont > maxDupCont then
    begin
      y := y + h + Increm;
      x := oldx;
      dupcont := 0;
      linha := linha + 1;
    end;
    if (linha = 5) then
      break;
  end;
end;

procedure TBlocoPagamentos.OnInit(Args: TFPDFBandInitArgs);
var
  NFe: TNFe;
begin
  NFe := FNFeUtils.NFe;

  Visible := True;
  if (NFe.Cobr.Dup.Count > 0) or (NFe.pag.Count = 0) then
    Visible := False;
  if (NFe.pag.Count = 1) and (NFe.pag[0].tPag = fpSemPagamento) then
    Visible := False;

  Height := 12;
end;

{ TBlocoCalculoImposto }

constructor TBlocoCalculoImposto.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
end;

procedure TBlocoCalculoImposto.OnDraw(Args: TFPDFBandDrawArgs);
var
  NFe: TNFe;
  PDF: IFPDF;

  function Imposto(x, y, w, h: double;
    const campo: string; valor: double; const AFontStyle: string = ''): double;
  begin
    PDF.SetFont(6, '');
    PDF.TextBox(x, y, w - 1, h, campo, 'T', 'L', 0, '');

    PDF.SetFont(10, AFontStyle);
    PDF.TextBox(x, y, w, h, FormatFloat('#,0.00', valor, FNFeUtils.FormatSettings), 'B', 'R', 1, '');

    Result := x + w;
  end;

var
  x, y, x_inicial, w, h: double;
  campos_por_linha: integer;
  maxW, title_size: double;
  texto: string;
begin
  NFe := FNFeUtils.NFe;
  PDF := Args.PDF;

  x := 0;
  y := 0;

  y := y + 1;

  x_inicial := x;

  campos_por_linha := 9;
//  if (!Self.exibirPIS) {
//      $campos_por_linha--;
//  }
//  if (!Self.exibirIcmsInterestadual) {
//      $campos_por_linha -= 2;
//  }

  MaxW := Width;
  title_size := 31;

  w := maxW / campos_por_linha;

  PDF.SetFont(7, 'B');
  texto := 'CÁLCULO DO IMPOSTO';
  PDF.TextBox(x, y, title_size, 8, texto, 'T', 'L', 0, '');
  y := y + 3;
  h := 7;

  x := Imposto(x, y, w, h, 'BASE DE CÁLC. DO ICMS', NFe.Total.ICMSTot.vBC);
  x := Imposto(x, y, w, h, 'VALOR DO ICMS', NFe.Total.ICMSTot.vICMS);
  x := Imposto(x, y, w, h, 'BASE DE CÁLC. ICMS S.T.', NFe.Total.ICMSTot.vBCST);
  x := Imposto(x, y, w, h, 'VALOR DO ICMS SUBST.', NFe.Total.ICMSTot.vST);
  x := Imposto(x, y, w, h, 'V. IMP. IMPORTAÇÃO', NFe.Total.ICMSTot.vII);

//  if (Self.exibirIcmsInterestadual) {
      x := Imposto(x, y, w, h, 'V. ICMS UF REMET.', NFe.Total.ICMSTot.vICMSUFRemet);
      x := Imposto(x, y, w, h, 'VALOR DO FCP', NFe.Total.ICMSTot.vFCPUFDest);
//  }

//  if (Self.exibirPIS) {
      x := Imposto(x, y, w, h, 'VALOR DO PIS', NFe.Total.ICMSTot.vPIS);
//  }

  Imposto(x, y, w, h, 'V. TOTAL PRODUTOS', NFe.Total.ICMSTot.vProd);

  y := y + h;
  x := x_inicial;

  x := Imposto(x, y, w, h, 'VALOR DO FRETE', NFe.Total.ICMSTot.vFrete);
  x := Imposto(x, y, w, h, 'VALOR DO SEGURO', NFe.Total.ICMSTot.vSeg);
  x := Imposto(x, y, w, h, 'DESCONTO', NFe.Total.ICMSTot.vDesc);
  x := Imposto(x, y, w, h, 'OUTRAS DESPESAS', NFe.Total.ICMSTot.vOutro);
  x := Imposto(x, y, w, h, 'VALOR TOTAL IPI', NFe.Total.ICMSTot.vIPI);

//  if (Self.exibirIcmsInterestadual) {
      x := Imposto(x, y, w, h, 'V. ICMS UF DEST.', NFe.Total.ICMSTot.vICMSUFDest);
      x := Imposto(x, y, w, h, 'V. TOT. TRIB.', NFe.Total.ICMSTot.vTotTrib);
//  }

//  if (Self.exibirPIS) {
      x := Imposto(x, y, w, h, 'VALOR DA COFINS', NFe.Total.ICMSTot.vCOFINS);
//  }
  Imposto(x, y, w, h, 'V. TOTAL DA NOTA', NFe.Total.ICMSTot.vNF, 'B');
end;

procedure TBlocoCalculoImposto.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 18;
end;

{ TBlocoTransporte }

constructor TBlocoTransporte.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
end;

procedure TBlocoTransporte.OnDraw(Args: TFPDFBandDrawArgs);
var
  NFe: TNFe;
  PDF: IFPDF;
  x, y: double;
  oldX, maxW, h, w, w1, w2, w3: double;
  texto: string;
  ok: boolean;
  quantidade: integer;
  pesoBruto, pesoLiquido: double;
  especie, marca, numero: string;
  i: integer;
  Vol: TVolCollectionItem;
begin
  NFe := FNFeUtils.NFe;
  PDF := Args.PDF;

  x := 0;
  y := 0;
  y := y + 1;

  oldX := x;

  maxW := Width;

  //#####################################################################
  //TRANSPORTADOR / VOLUMES TRANSPORTADOS
  texto := 'TRANSPORTADOR / VOLUMES TRANSPORTADOS';
  w := maxW;
  h := 7;
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 0, '');
  //NOME / RAZÃO SOCIAL
  w1 := maxW*0.29;
  y := y + 3;
  texto := 'NOME / RAZÃO SOCIAL';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w1, h, texto, 'T', 'L', 1, '');

  texto := NFe.Transp.Transporta.xNome;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w1, h, texto, 'B', 'L', 0, '');
  //FRETE POR CONTA
  x := x + w1;
  w2 := maxW*0.15;
  texto := 'FRETE POR CONTA';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h, texto, 'T', 'L', 1, '');

  texto := modFreteToDesStr(NFe.Transp.modFrete, DblToVersaoDF(ok, NFe.infNFe.Versao));
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w2, h, texto, 'B', 'C', 1, '');
  //CÓDIGO ANTT
  x := x + w2;
  texto := 'CÓDIGO ANTT';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h, texto, 'T', 'L', 1, '');
  texto := NFe.Transp.veicTransp.RNTC;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w2, h, texto, 'B', 'C', 0, '');
  //PLACA DO VEÍCULO
  x := x + w2;
  texto := 'PLACA DO VEÍCULO';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h, texto, 'T', 'L', 1, '');

  texto := NFe.Transp.veicTransp.placa;
  if (texto = '') and (NFe.Transp.Reboque.Count > 0) then
    texto := NFe.Transp.Reboque[0].placa;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w2, h, texto, 'B', 'C', 0, '');
  //UF
  x := x + w2;
  w3 := RoundTo(maxW*0.04, 0);
  texto := 'UF';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w3, h, texto, 'T', 'L', 1, '');
  texto := NFe.Transp.veicTransp.UF;
  if (texto = '') and (NFe.Transp.Reboque.Count > 0) then
    texto := NFe.Transp.Reboque[0].UF;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w3, h, texto, 'B', 'C', 0, '');
  //CNPJ / CPF
  x := x + w3;
  w := maxW-(w1+3*w2+w3);
  texto := 'CNPJ / CPF';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');

  texto := FormatarCNPJouCPF(NFe.Transp.Transporta.CNPJCPF);
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //#####################################################################
  //ENDEREÇO
  y := y + h;
  x := oldX;
  h := 7;
  w1 := maxW*0.44;
  texto := 'ENDEREÇO';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w1, h, texto, 'T', 'L', 1, '');
  texto := NFe.Transp.Transporta.xEnder;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w1, h, texto, 'B', 'L', 0, '');
  //MUNICÍPIO
  x := x + w1;
  w2 := RoundTo(maxW*0.30, 0);
  texto := 'MUNICÍPIO';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h, texto, 'T', 'L', 1, '');
  texto := NFe.Transp.Transporta.xMun;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w2, h, texto, 'B', 'L', 0, '');
  //UF
  x := x + w2;
  w3 := RoundTo(maxW*0.04, 0);
  texto := 'UF';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w3, h, texto, 'T', 'L', 1, '');
  texto := NFe.Transp.Transporta.UF;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w3, h, texto, 'B', 'C', 0, '');
  //INSCRIÇÃO ESTADUAL
  x := x + w3;
  w := maxW-(w1+w2+w3);
  texto := 'INSCRIÇÃO ESTADUAL';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := NFe.Transp.Transporta.IE;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'C', 0, '');

  //Tratar Multiplos volumes
  quantidade := 0;
  especie := '';
  marca := '';
  numero := '';
  texto := '';
  pesoBruto := 0;
  pesoLiquido := 0;
  for i := 0 to NFe.Transp.Vol.Count - 1 do
  begin
    Vol := NFe.Transp.Vol[I];
    quantidade := quantidade + Vol.qVol;
    pesoBruto := pesoBruto + Vol.pesoB;
    pesoLiquido := pesoLiquido + Vol.pesoL;
    texto := Vol.esp;
    if (texto <> especie) and (especie <> '') then
      especie := 'VARIAS'
    else
      especie := texto;
    texto := Vol.marca;
    if (texto <> marca) and (marca <> '') then
      marca := 'VARIAS'
    else
      marca := texto;
    texto := Vol.nVol;
    if (texto <> numero) and (numero <> '') then
      numero := 'VARIAS'
    else
      numero := texto;
  end;

  //#####################################################################
  //QUANTIDADE
  y := y + h;
  x := oldX;
  h := 7;
  w1 := RoundTo(maxW*0.10, 0);
  texto := 'QUANTIDADE';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w1, h, texto, 'T', 'L', 1, '');
  if quantidade > 0 then
  begin
    texto := FormatFloat('#,0', quantidade, FNFeUtils.FormatSettings);
    PDF.SetFont(10, '');
    PDF.TextBox(x, y, w1, h, texto, 'B', 'C', 0, '');
  end;
  //ESPÉCIE
  x := x + w1;
  w2 := RoundTo(maxW*0.17, 0);
  texto := 'ESPÉCIE';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h, texto, 'T', 'L', 1, '');
  texto := especie;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w2, h, texto, 'B', 'C', 0, '');
  //MARCA
  x := x + w2;
  texto := 'MARCA';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h, texto, 'T', 'L', 1, '');
  texto := marca;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w2, h, texto, 'B', 'C', 0, '');
  //NUMERAÇÃO
  x := x + w2;
  texto := 'NUMERAÇÃO';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h, texto, 'T', 'L', 1, '');
  texto := numero;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w2, h, texto, 'B', 'C', 0, '');
  //PESO BRUTO
  x := x + w2;
  w3 := RoundTo(maxW*0.20, 0);
  texto := 'PESO BRUTO';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w3, h, texto, 'T', 'L', 1, '');
  if pesoBruto > 0 then
    texto := FormatFloat('#,0.000', pesoBruto, FNFeUtils.FormatSettings)
  else
    texto := '';
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w3, h, texto, 'B', 'R', 0, '');
  //PESO LÍQUIDO
  x := x + w3;
  w := maxW -(w1+3*w2+w3);
  texto := 'PESO LÍQUIDO';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  if pesoLiquido > 0 then
    texto := FormatFloat('#,0.000', pesoLiquido, FNFeUtils.FormatSettings)
  else
    texto := '';
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'R', 0, '');
end;

procedure TBlocoTransporte.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 25;
end;

{ TBlocoCalculoISSQN }

constructor TBlocoCalculoISSQN.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
end;

procedure TBlocoCalculoISSQN.OnDraw(Args: TFPDFBandDrawArgs);
var
  NFe: TNFe;
  PDF: IFPDF;
  x, y: double;
  w, h: double;
  texto: string;
begin
  NFe := FNFeUtils.NFe;
  PDF := Args.PDF;

  x := 0;
  y := 0;

  y := y + 1;

  //CÁLCULO DO ISSQN
  texto := 'CÁLCULO DO ISSQN';
  w := Width;
  h := 7;
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 0, '');
  //INSCRIÇÃO MUNICIPAL
  y := y + 3;
  w := RoundTo(Width * 0.23, 0);
  texto := 'INSCRIÇÃO MUNICIPAL';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  //inscrição municipal
  texto := NFe.Emit.IM;
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'L', 0, '');
  //VALOR TOTAL DOS SERVIÇOS
  x := x + w;
  texto := 'VALOR TOTAL DOS SERVIÇOS';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := FormatFloat('#,0.00', NFe.Total.ISSQNtot.vServ, FNFeUtils.FormatSettings);
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'R', 0, '');
  //BASE DE CÁLCULO DO ISSQN
  x := x + w;
  texto := 'BASE DE CÁLCULO DO ISSQN';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := FormatFloat('#,0.00', NFe.Total.ISSQNtot.vBC, FNFeUtils.FormatSettings);
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'R', 0, '');
  //VALOR TOTAL DO ISSQN
  x := x + w;
  w := Width - (3 * w);
  texto := 'VALOR TOTAL DO ISSQN';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 1, '');
  texto := FormatFloat('#,0.00', NFe.Total.ISSQNtot.vISS, FNFeUtils.FormatSettings);
  PDF.SetFont(10, '');
  PDF.TextBox(x, y, w, h, texto, 'B', 'R', 0, '');
end;

procedure TBlocoCalculoISSQN.OnInit(Args: TFPDFBandInitArgs);
var
  NFe: TNFe;
begin
  NFe := FNFeUtils.NFe;
  Visible := NFe.Total.ISSQNtot.vServ > 0;
  if NFe.Total.ISSQNtot.vBC = 0 then
    Visible := False;

  Height := 11;
end;

{ TBlocoDadosAdicionais }

constructor TBlocoDadosAdicionais.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btPageFooter);
  FNFeUtils := ANFeUtils;
end;

function TBlocoDadosAdicionais.GetEspacoVerticalDadosAdicionais(
  const textoAdic: string): double;
var
  Linhas: TStringArray;
  numlinhasdados: double;
  PDF: TFPDFExt2;
  I: Integer;
begin
  PDF := TFPDFExt2.Create;
  try
    PDF.SetFont('Times', '', 7);
    Linhas := ACBr_fpdf.Split(textoAdic, sLineBreak);
    numlinhasdados := 0;
    for I := 0 to Length(Linhas) - 1 do
      numlinhasdados := numlinhasdados + PDF.GetNumLines(Linhas[I], Width);

    Result := RoundTo((numlinhasdados + 3) * PDF.CurrentFontSize, 0);

    if Result < 10 then
      Result := 10;

    Result := Result + 4;
  finally
    PDF.Free;
  end;
end;

procedure TBlocoDadosAdicionais.OnDraw(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  x, y: double;
  h, w: double;
  texto: string;
begin
  PDF := Args.PDF;

  x := 0;
  y := 0;
  y := y + 1;

  h := Self.Height;

  //##################################################################################
  //DADOS ADICIONAIS
  texto := 'DADOS ADICIONAIS';

  w := Width;
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, 8, texto, 'T', 'L', 0, '');
  //INFORMAÇÕES COMPLEMENTARES
  texto := 'INFORMAÇÕES COMPLEMENTARES';
  y := y + 3;
//  if Args.PDF.Orientation = poPortrait then
    w := RoundTo(Width * 0.66, 0);
//  else
//    w := RoundTo(Width * 0.5, 0);
  PDF.SetFont(6, 'B');
  PDF.TextBox(x, y, w, h - 4, texto, 'T', 'L', 1, '');
  //o texto com os dados adicionais foi obtido na função montaDANFE
  //e carregado em uma propriedade privada da classe
  //$this->wAdic com a largura do campo
  //$this->textoAdic com o texto completo do campo
  y := y + 1;
  PDF.SetFont(7, '');
  PDF.TextBox(x, y+2, w-2, h-3, FNFeUtils.GetTextoAdicional, 'T', 'L', 0, '', false);
  //RESERVADO AO FISCO
  texto := 'RESERVADO AO FISCO';
  x := x + w;
  y := y - 1;

  w := Width - w;

  PDF.SetFont(6, 'B');
  PDF.TextBox(x, y, w, h - 4, texto, 'T', 'L', 1, '');
end;

procedure TBlocoDadosAdicionais.OnInit(Args: TFPDFBandInitArgs);
begin
  Visible := True;

  Height := GetEspacoVerticalDadosAdicionais(FNFeUtils.GetTextoAdicional);
end;

{ TBlocoRodape }

constructor TBlocoRodape.Create(const AMensagem: string);
begin
  inherited Create(btPageFooter);
  FMensagem := AMensagem;
end;

procedure TBlocoRodape.OnDraw(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  x, y, w, h: double;
  Mensagens: TStringArray;
begin
  PDF := Args.PDF;
  if FMensagem = '' then
    Exit;
  Mensagens := ACBr_fpdf.Split(FMensagem, '|');

  x := 0;
  y := 0;
  w := Width;
  h := Height;

  PDF.SetFont(6, 'I');
  if Length(Mensagens) >= 1 then
    PDF.TextBox(x, y, w, h, Mensagens[0], 'T', 'L', 0);
  if Length(Mensagens) >= 2 then
    PDF.TextBox(x, y, w, h, Mensagens[1], 'T', 'C', 0);
  if Length(Mensagens) >= 3 then
    PDF.TextBox(x, y, w, h, Mensagens[2], 'T', 'R', 0);
end;

procedure TBlocoRodape.OnInit(Args: TFPDFBandInitArgs);
begin
  Height := 5;
end;

{ TBlocoProdutosServicos }

function TBlocoProdutosServicos.CalculateHeightProduto(PDF: IFPDF;
  Item: integer; AMostrarUnidadeTributavel: boolean): double;
var
  w2, numlinhas: double;
  textoProduto: string;
begin
  w2 := RoundTo(Width * 0.28, 0);
  PDF.SetFont(7, '');
  textoProduto := FNFeUtils.DANFEClassOwner.ManterXProd(FNFeUtils.NFe, Item);
  numlinhas := PDF.GetNumLines(textoProduto, w2);
//  if ($mostrarUnidadeTributavel && $numlinhas == 1) {
//      $numlinhas++;
//  }
  Result := RoundTo((numlinhas * PDF.CurrentFontSize) + (numlinhas * 0.5), -2);
end;

constructor TBlocoProdutosServicos.Create(ANFeUtils: TNFeUtilsFPDF);
begin
  inherited Create(btData);
  FNFeUtils := ANFeUtils;
  FHCabecItens := 4;
end;

procedure TBlocoProdutosServicos.OnDraw(Args: TFPDFBandDrawArgs);
var
  NFe: TNFe;
  PDF: IFPDF;
  x, y: double;
  FreeSpace: double;
  oldX, oldY, h, w, y_linha: double;
  w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14: double;
  HMax, HUsado: double;
  texto, textoProduto: string;
  DetItem: TDetCollectionItem;
begin
  NFe := FNFeUtils.NFe;
  PDF := Args.PDF;

  x := 0;
  y := 0;

  if Height <= Args.FreeSpace - Args.ReservedSpace then
    FreeSpace := Args.FreeSpace - Args.ReservedSpace
  else
    FreeSpace := Args.FreeSpace;
  if FreeSpace <= 0 then
    Exit;
  Height := FreeSpace;

  y := y + 1;

  OldX := x;
  OldY := y;
  //#####################################################################
  //DADOS DOS PRODUTOS / SERVIÇOS
  texto := 'DADOS DOS PRODUTOS / SERVIÇOS';

  w := Width;

  h := 4;
  PDF.SetFont(7, 'B');
  PDF.TextBox(x, y, w, h, texto, 'T', 'L', 0, '');
  y := y + 3;
  //desenha a caixa dos dados dos itens da NF
  HMax := FreeSpace - y;

  texto := '';
  PDF.TextBox(x, y, w, HMax);
  //##################################################################################
  // cabecalho LOOP COM OS DADOS DOS PRODUTOS
  //CÓDIGO PRODUTO
  texto := 'CÓDIGO' + sLineBreak + 'PRODUTO';
  w1 := RoundTo(w*0.09, 0);
  h := 4;
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w1, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w1, y, x+w1, y+hmax);
  //DESCRIÇÃO DO PRODUTO / SERVIÇO
  x := x + w1;
  w2 := RoundTo(w*0.28, 0);
  texto := 'DESCRIÇÃO DO PRODUTO / SERVIÇO';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w2, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w2, y, x+w2, y+hmax);
  //NCM/SH
  x := x + w2;
  w3 := RoundTo(w*0.06, 0);
  texto := 'NCM/SH';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w3, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w3, y, x+w3, y+hmax);
  //O/CST
  x := x + w3;
  w4 := RoundTo(w*0.05, 0);
  if NFe.Emit.CRT = crtRegimeNormal then
    texto := 'O/CST' // SE REGIME NORMAL EXIBE CST
  else
    texto := 'O/CSOSN'; // SE REGIME SIMPLES EXIBE CSOSN

  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w4, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w4, y, x+w4, y+hmax);
  //CFOP
  x := x + w4;
  w5 := RoundTo(w*0.04, 0);
  texto := 'CFOP';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w5, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w5, y, x+w5, y+hmax);
  //UN
  x := x + w5;
  w6 := RoundTo(w*0.03, 0);
  texto := 'UN';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w6, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w6, y, x+w6, y+hmax);
  //QUANT
  x := x + w6;
  w7 := RoundTo(w*0.07, 0);
  texto := 'QUANT';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w7, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w7, y, x+w7, y+hmax);
  //VALOR UNIT
  x := x + w7;
  w8 := RoundTo(w*0.06, 0);
  texto := 'VALOR UNIT';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w8, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w8, y, x+w8, y+hmax);
  //VALOR TOTAL
  x := x + w8;
  w9 := RoundTo(w*0.06, 0);
  texto := 'VALOR TOTAL';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w9, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w9, y, x+w9, y+hmax);
  //B.CÁLC ICMS
  x := x + w9;
  w10 := RoundTo(w*0.06, 0);
  texto := 'B.CÁLC ICMS';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w10, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w10, y, x+w10, y+hmax);
  //VALOR ICMS
  x := x + w10;
  w11 := RoundTo(w*0.06, 0);
  texto := 'VALOR ICMS';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w11, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w11, y, x+w11, y+hmax);
  //VALOR IPI
  x := x + w11;
  w12 := RoundTo(w*0.05, 0);
  texto := 'VALOR IPI';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w12, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w12, y, x+w12, y+hmax);
  //ALÍQ. ICMS
  x := x + w12;
  w13 := RoundTo(w*0.035, 0);
  texto := 'ALÍQ. ICMS';
  PDF.SetFont(6, '');
  PDF.TextBox(x, y, w13, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(x+w13, y, x+w13, y+hmax);
  //ALÍQ. IPI
  x := x + w13;
  w14 := w-(w1+w2+w3+w4+w5+w6+w7+w8+w9+w10+w11+w12+w13);
  texto := 'ALÍQ. IPI';
  PDF.TextBox(x, y, w14, h + 2, texto, 'C', 'C', 0, '', false);
  PDF.Line(oldX, y+h+1, oldX + w, y+h+1);
  y := y + 5;
  //##################################################################################
  // LOOP COM OS DADOS DOS PRODUTOS
  hUsado := FHCabecItens;
  PDF.SetFont(7, '');

  while FCurrentItem < NFe.Det.Count do
  begin
    DetItem := NFe.Det[FCurrentItem];

    textoProduto := FNFeUtils.DANFEClassOwner.ManterXProd(NFe, FCurrentItem);
    h := CalculateHeightProduto(PDF, FCurrentItem);
    hUsado := hUsado + h;

    if HUsado >= HMax then
    begin
      //ultrapassa a capacidade para uma única página
      //o restante dos dados serão usados nas proximas paginas
      Break;
    end;
    y_linha := y + h;

    // linha entre itens
    if y_linha <= oldY + hmax then
    begin
      if PDF.Orientation = poPortrait then
        PDF.DashedLine(oldX, y_linha, oldX + w, y_linha)
      else
        PDF.DashedLine(oldX, y_linha, oldX + w, y_linha);
    end;
    //corrige o x
    x := oldX;
    //codigo do produto
    texto := FNFeUtils.DANFEClassOwner.ManterCodigo(DetItem.Prod.cEAN, DetItem.Prod.cProd);
    PDF.TextBox(x, y, w1, h, texto, 'T', 'C', 0, '');
    x := x + w1;
    //DESCRIÇÃO
    if PDF.Orientation = poPortrait then
      PDF.TextBox(x, y, w2, h, textoProduto, 'T', 'L', 0, '', false)
    else
      PDF.TextBox(x, y, w2, h, textoProduto, 'T', 'L', 0, '', false);
    x := x + w2;
    //NCM
    texto := DetItem.Prod.NCM;
    PDF.TextBox(x, y, w3, h, texto, 'T', 'C', 0, '');
    x := x + w3;
    //CST
    texto := Format('%s/%s', [
      OrigToStr(DetItem.Imposto.ICMS.orig),
      FNFeUtils.DANFEClassOwner.ManterCst(
        NFe.Emit.CRT , DetItem.Imposto.ICMS.CSOSN , DetItem.Imposto.ICMS.CST
      )
    ]);
    PDF.TextBox(x, y, w4, h, texto, 'T', 'C', 0, '');
    //CFOP
    x := x + w4;
    texto := DetItem.Prod.CFOP;
    PDF.TextBox(x, y, w5, h, texto, 'T', 'C', 0, '');
    //Unidade
    x := x + w5;
    texto := DetItem.Prod.uCom;
    PDF.TextBox(x, y, w6, h, texto, 'T', 'C', 0, '');
    x := x + w6;
    // QTDADE
    texto := FormatFloat('#,0.0000', DetItem.Prod.qCom, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y, w7, h, texto, 'T', 'R', 0, '');
    x := x + w7;
    // Valor Unitário
    texto := FormatFloat('#,0.0000', DetItem.Prod.vUnCom, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y, w8, h, texto, 'T', 'R', 0, '');
    x := x + w8;
    // Valor do Produto
    texto := FNFeUtils.DANFEClassOwner.ManterVprod(DetItem.Prod.VProd, DetItem.Prod.vDesc);
     PDF.TextBox(x, y, w9, h, texto, 'T', 'R', 0, '');
    //Valor da Base de calculo
    x := x + w9;
    texto := FormatFloat('#,0.00', DetItem.Imposto.ICMS.vBC, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y, w10, h, texto, 'T', 'R', 0, '');
    //Valor do ICMS
    x := x + w10;
    texto := FormatFloat('#,0.00', DetItem.Imposto.ICMS.vICMS, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y, w11, h, texto, 'T', 'R', 0, '');
    //Valor do IPI
    x := x + w11;
    texto := FormatFloat('#,0.00', DetItem.Imposto.IPI.vIPI, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y, w12, h, texto, 'T', 'R', 0, '');
    // %ICMS
    x := x + w12;
    texto := FormatFloat('#,0.00', DetItem.Imposto.ICMS.pICMS, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y, w13, h, texto, 'T', 'C', 0, '');
    //%IPI
    x := x + w13;
    texto := FormatFloat('#,0.00', DetItem.Imposto.IPI.pIPI, FNFeUtils.FormatSettings);
    PDF.TextBox(x, y, w14, h, texto, 'T', 'C', 0, '');

    y := y + h;

    Inc(FCurrentItem);
  end;

  Args.DrawAgain := FCurrentItem < NFe.Det.Count;
end;

procedure TBlocoProdutosServicos.OnInit(Args: TFPDFBandInitArgs);
begin
  FCurrentItem := 0;

  AutoHeight := True;
  // Min Height
  Height := 12;
end;

{ TBlocoMarcaDagua }

constructor TBlocoMarcaDagua.Create(ANFeUtils: TNFeUtilsFPDF; ACancelada: boolean);
begin
  inherited Create(btOverlay);
  FNFeUtils := ANFeUtils;
  FCancelada := ACancelada;
end;

procedure TBlocoMarcaDagua.OnDraw(Args: TFPDFBandDrawArgs);
var
  NFe: TNFe;
  x, y, w, h, m: double;
begin
  NFe := FNFeUtils.NFe;

  m := 5;
  x := Args.LeftMargin + m;
  y := Height - 130;


  //####################################################################################
  //Indicação de NF Homologação, cancelamento e falta de protocolo
  //indicar cancelamento
  if FCancelada or FNFeUtils.NotaCancelada then
    Watermark(Args, x, y, Width - (2 * m), 25, 'NOTA CANCELADA');

  if FNFeUtils.NotaEPEC then
    Watermark(Args, x, Height - 130, Width - (2 * m), 25,
      'DANFE impresso em contingência -' + sLineBreak +
      'DPEC regularmente recebido pela Receita' + sLineBreak +
      'Federal do Brasil', 48, 'B');
  if FNFeUtils.NotaDenegada then
  begin
    y := Watermark(Args, x, Height-130, Width-(2*m), 25, 'NFe USO DENEGADO', 48, 'B');
    Watermark(Args, x, y + 35, Width-(2*m), 5,
      'SEM VALOR FISCAL' + sLineBreak +
      NFe.procNFe.xMotivo,
      48, 'B');
  end;

  //indicar sem valor
  if NFe.Ide.tpAmb = taHomologacao then
  begin
    y := Watermark(
      Args,
      x,
      RoundTo(Height/2, 0),
      Width-(2*m),
      5,
      'SEM VALOR FISCAL', 48
    );
    Watermark(Args, x, y + 14, Width-(2*m), 5, 'AMBIENTE DE HOMOLOGAÇÃO', 30);
  end
  else
  begin
    // x := m;
    y := RoundTo(Height/2, 0);
    h := 25;
    w := Width-(2*m);
    Args.PDF.SetTextColor(200, 200, 200);

    //indicar FALTA DO PROTOCOLO se NFe não for em contingência
    if ((NFe.Ide.tpEmis = teContingencia) or (NFe.Ide.tpEmis = teFSDA)) and not FNFeUtils.NotaEPEC then
    begin
      //Contingência
      Watermark(Args, x, y, w, h, 'DANFE Emitido em Contingência', 48);
      Watermark(Args, x, y + 14, w, h, 'devido à problemas técnicos', 30);
    end
    else if NFe.procNFe.nProt = '' then
    begin
      if not FNFeUtils.NotaEPEC then
      begin
        Watermark(Args, x, y, w, h, 'SEM VALOR FISCAL', 48);
        Watermark(Args, x, y + 25, w, h, 'FALTA PROTOCOLO DE AUTORIZAÇÃO DA SEFAZ', 30);
      end
      else
        Watermark(Args, x, y + 35, w, h, 'FALTA PROTOCOLO DE AUTORIZAÇÃO DA SEFAZ', 30);
    end;
    Args.PDF.SetTextColor(0, 0, 0);
  end;
end;

function TBlocoMarcaDagua.Watermark(Args: TFPDFBandDrawArgs; x, y, w, h: double; const ATexto: string;
  AFontSize: Double; const AFontStyle: string): double;
begin
  Result := Watermark(Args, x, y, w, h, ATexto, Args.PDF.CurrentFontFamily, AFontSize, AFontStyle);
end;

function TBlocoMarcaDagua.Watermark(Args: TFPDFBandDrawArgs; x, y, w, h: double; const ATexto,
  AFontFamily: string; AFontSize: Double; const AFontStyle: string): double;
var
  PreviousTextColor: string;
begin
  //PreviousTextColor := Args.PDF.TextColor; //TODO:
  try
    Args.PDF.SetTextColor(200, 200, 200);
    Args.PDF.SetFont(AFontFamily, AFontStyle, AFontSize);
    Args.PDF.TextBox(x, y, w, h, ATexto, 'C', 'C', False, False, True);
    Result := y;
  finally
    //Args.PDF.TextColor := PreviousTextColor; //TODO:
  end;
end;

{ TNFeDANFeFPDF }

constructor TNFeDANFeFPDF.Create(ANFe: TNFe; AACBrNFeDANFEClass : TACBrNFeDANFEClass);
var
  LFormatSettings: TFormatSettings;
begin
  inherited Create;
  FNFeUtils := TNFeUtilsFPDF.Create(ANFe, AACBrNFeDANFEClass);
  Self.FDANFEClassOwner := AACBrNFeDANFEClass;
  {$IFDEF HAS_FORMATSETTINGS}
    LFormatSettings := CreateFormatSettings;
  {$ENDIF}
  LFormatSettings.DecimalSeparator  := ',';
  LFormatSettings.ThousandSeparator := '.';
  FNFeUtils.FormatSettings := LFormatSettings;

  SetFont('Times');

  EngineOptions.DoublePass := True;
end;

destructor TNFeDANFeFPDF.Destroy;
begin
  FNFeUtils.Free;
  inherited;
end;

procedure TNFeDANFeFPDF.OnStartReport(Args: TFPDFReportEventArgs);
var
  LOrientation: TFPDFOrientation;
  LStream : TMemoryStream;
  LLogoStringStream : TStringStream;
  LMargemInferior : Double;
  LMargemSuperior : Double;
  LMargemEsquerda : Double;
  LMargemDireita  : Double;
begin
  if not FInitialized then
  begin
    if FNFeUtils.NFe = nil then
      raise Exception.Create('NF-e não definida');
    if FNFeUtils.NFe.Ide.tpImp = tiPaisagem then
      LOrientation := poLandscape
    else
      LOrientation := poPortrait;

    if FDANFEClassOwner.MargemEsquerda  <> 6 then
      LMargemEsquerda := FDANFEClassOwner.MargemEsquerda
    else
      LMargemEsquerda := 2;

    if FDANFEClassOwner.MargemSuperior <> 8 then
      LMargemSuperior := FDANFEClassOwner.MargemSuperior
    else
      LMargemSuperior := 2;

    if FDANFEClassOwner.MargemDireita  <> 5.1 then
     LMargemDireita := FDANFEClassOwner.MargemDireita
    else
      LMargemDireita  := 2;

    if FDANFEClassOwner.MargemInferior <> 8 then
      LMargemInferior := FDANFEClassOwner.MargemInferior
    else
      LMargemInferior := 2;

    SetMargins(LMargemEsquerda,
               LMargemSuperior,
               LMargemDireita,
               LMargemInferior);

    if FDANFEClassOwner.Logo <> '' then
    begin
      LStream := TMemoryStream.Create;
      try
        if FileExists(FDANFEClassOwner.Logo) then
          LStream.LoadFromFile(FDANFEClassOwner.Logo)
        else
        begin
          LLogoStringStream:= TStringStream.Create(FDANFEClassOwner.Logo);
          try
            LStream.LoadFromStream(LLogoStringStream);
            LStream.Position := 0;
          finally
            LLogoStringStream.Free;
          end;
        end;
        SetLength(FLogo, LStream.Size);
        LStream.Position := 0;
        LStream.Read(FLogo[0], LStream.Size);
      finally
        LStream.Free;
      end;
    end;

    AddPage(LOrientation);
    AddBand(TBlocoCanhoto.Create(PosCanhoto, FNFeUtils));
    AddBand(TBlocoDadosNFe.Create(FNFeUtils, FLogo, FLogoStretched, FLogoAlign));
    AddBand(TBlocoDestinatarioRemetente.Create(FNFeUtils));
    AddBand(TBlocoLocalRetirada.Create(FNFeUtils));
    AddBand(TBlocoLocalEntrega.Create(FNFeUtils));
    AddBand(TBlocoFaturaDuplicatas.Create(FNFeUtils));
    if (FDANFEClassOwner.ExibeCampoDePagamento = eipQuadro) then
      AddBand(TBlocoPagamentos.Create(FNFeUtils));
    AddBand(TBlocoCalculoImposto.Create(FNFeUtils));
    AddBand(TBlocoTransporte.Create(FNFeUtils));
    AddBand(TBlocoProdutosServicos.Create(FNFeUtils));
    AddBand(TBlocoCalculoISSQN.Create(FNFeUtils));
    AddBand(TBlocoDadosAdicionais.Create(FNFeUtils));
    AddBand(TBlocoRodape.Create(FMensagemRodape));
    AddBand(TBlocoMarcaDagua.Create(FNFeUtils, Cancelada));

    FInitialized := True;
  end;
end;

{ TACBrNFeDANFeFPDFClass }

constructor TACBrNFeDANFeFPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Inicialize outras configurações do seu componente aqui

end;

destructor TACBrNFeDANFeFPDF.Destroy;
begin
  FFPDFReport.Free;
  inherited Destroy;
end;

procedure TACBrNFeDANFeFPDF.ImprimirDANFE(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFeDANFeFPDF.ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe);
begin

end;

procedure TACBrNFeDANFeFPDF.ImprimirDANFEResumido(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFeDANFeFPDF.ImprimirDANFEPDF(NFE: TNFe);
var
  Report: TFPDFReport;
  Engine: TFPDFEngine;
  I : Integer;
  LNFe : TNFe;
  LPath : String;
begin
  for I := 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count -1 do
  begin
    LNFe := TACBrNFe(ACBrNFe).NotasFiscais[I].NFe;
    Report := TNFeDANFeFPDF.Create(LNFe,TACBrNFeDANFEClass(TACBrNFe(ACBrNFe).DANFE));

    FIndexImpressaoIndividual := I;

    //TNFeDANFeFPDF(Report).PosCanhoto := TNFeDANFeFPDF(TACBrNFe(ACBrNFe).DANFE).PosCanhoto;

    TNFeDANFeFPDF(Report).MensagemRodape := Self.Sistema;

    try
      Engine := TFPDFEngine.Create(Report, False);
      try
        Engine.Compressed := True;

       // LPAth := IncludeTrailingPathDelimiter(TACBrNFe(ACBrNFe).DANFE.PathPDF) +
       //   ExtractFilePath(TACBrNFe(ACBrNFe).DANFE.NomeDocumento);

       // Engine.SaveToFile(LPath + LNFe.infNFe.ID+'.pdf');

       LPath := DefinirNomeArquivo(TACBrNFe(ACBrNFe).DANFE.PathPDF,
               OnlyNumber(LNFe.infNFe.ID) + '-nfe.pdf',
               TACBrNFe(ACBrNFe).DANFE.NomeDocumento);

       Engine.SaveToFile(LPath);
      finally
        Engine.Free;
      end;
    finally
      Report.Free;
    end;
  end;
end;

procedure TACBrNFeDANFeFPDF.ImprimirEVENTO(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFeDANFeFPDF.ImprimirEVENTOPDF(AStream: TStream; ANFe: TNFe);
begin
  inherited;
end;

procedure TACBrNFeDANFeFPDF.ImprimirEVENTOPDF(NFE: TNFe);
begin
  inherited;
end;

procedure TACBrNFeDANFeFPDF.ImprimirINUTILIZACAO(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFeDANFeFPDF.ImprimirINUTILIZACAOPDF(AStream: TStream;
  ANFe: TNFe);
begin
  inherited;
end;

procedure TACBrNFeDANFeFPDF.ImprimirINUTILIZACAOPDF(NFE: TNFe);
begin
  inherited;
end;

end.
