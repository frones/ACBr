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

unit ACBrNFCeDANFeFPDF;

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
  ACBrUtil.Compatibilidade,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrDFeUtil,
  ACBrNFeUtilsFPDF, ACBrNFeDANFEClass, ACBrNFe, ACBrBase, ACBrDFeDANFeReport;

type
  TACBrNFCeItem = record
    Height: double;
    XProd: string;
  end;
type
  TNFCeDANFeFPDF = class(TFPDFReport)
  private
    FNFe: TNFe;
    FNFeUtils: TNFeUtilsFPDF;
    FDANFEClassOwner : TACBrNFeDANFCEClass;
    FCancelada: boolean;
    FInitialized: boolean;
    FFontFamily: string;
    FPaperWidth: double;
    FPaperHeight: double;
    FLogoAlign: TLogoAlign;
    FLogo: TBytes;
    FVia: string;
    FDashWidth: double;
    FQRCodeLateral: boolean;
    FExibirNomeFantasia: boolean;
    FExibirConsumidorNome: boolean;
    FExibirConsumidorEndereco: boolean;
    FExibirItens: boolean;
    FMensagemRodape: string;
    FImageUtils: TImageUtils;
    FCanhoto: TPosRecibo;
    property NFe: TNFe read FNFe;
    function GetTextoBlocoCabecalho: string;
    function GetTextoBlocoConsumidor: string;
    function GetNFCeItem(Det: TDetCollectionItem; ADescriptionWidth: double; PDF: IFPDF): TACBrNFCeItem;
    procedure BlocoCabecalho(Args: TFPDFBandDrawArgs);
    procedure BlocoMensagemFiscal(Args: TFPDFBandDrawArgs);
    procedure BlocoItens(Args: TFPDFBandDrawArgs);
    procedure BlocoTotais(Args: TFPDFBandDrawArgs);
    procedure BlocoPagamentos(Args: TFPDFBandDrawArgs);
    procedure BlocoChaveAcesso(Args: TFPDFBandDrawArgs);
    procedure BlocoQRCodeCentralizadoOuLateral(Args: TFPDFBandDrawArgs);
    procedure BlocoConsumidor(Args: TFPDFBandDrawArgs; var y: double);
    procedure BlocoIdentificacaoNFCe(Args: TFPDFBandDrawArgs; var y: double);
    procedure BlocoQRCode(Args: TFPDFBandDrawArgs; var y: double);
    procedure BlocoMensagemContribuinte(Args: TFPDFBandDrawArgs);
    procedure BlocoRodape(Args: TFPDFBandDrawArgs);
    procedure BlocoLinha(Args: TFPDFBandDrawArgs);

  protected
    procedure OnStartReport(Args: TFPDFReportEventArgs); override;
  public
    constructor Create(ANFe: TNFe; AACBrNFeDANFCEClass : TACBrNFeDANFCEClass); reintroduce;
    destructor Destroy; override;
    property Cancelada: boolean read FCancelada write FCancelada;
    property PosCanhoto: TPosRecibo read FCanhoto write FCanhoto;
    property LogoAlign: TLogoAlign read FLogoAlign write FLogoAlign;
    property QRCodeLateral: boolean read FQRCodeLateral write FQRCodeLateral;
    property ExibirNomeFantasia: boolean read FExibirNomeFantasia write FExibirNomeFantasia;
    property ExibirConsumidorNome: boolean read FExibirConsumidorNome write FExibirConsumidorNome;
    property ExibirConsumidorEndereco: boolean read FExibirConsumidorEndereco write FExibirConsumidorEndereco;
    property ExibirItens: boolean read FExibirItens write FExibirItens;
    property MensagemRodape: string read FMensagemRodape write FMensagemRodape;
  end;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFCeDANFeFPDF = class(TACBrNFeDANFCEClass)
  private
    FFPDFReport: TNFCeDANFeFPDF;
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

  numNF := FormatarNumeroDocumentoFiscal(IntToStr(NFe.Ide.NNF));
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

{ TNFCeDANFeFPDF }

procedure TNFCeDANFeFPDF.BlocoCabecalho(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y: double;
  xRs, wRs, IncY: double;
  alignH: char;
  texto: string;
  Lines: TStringArray;
  nImgW, nImgH, xImg, yImg, logoWmm, logoHmm: double;
  MaxImgH: double;
  logoW, logoH: word;
  I: integer;
  Stream: TMemoryStream;
begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  y := 0;
  IncY := 0.5;

  if (Length(FLogo) > 0) then
  begin
    FImageUtils.GetImageSize(FLogo, logoW, logoH);
    MaxImgH := 14;

    xImg := 0;
    yImg := 0 + 1;
    logoWmm := (logoW/72)*25.4;
    logoHmm := (logoH/72)*25.4;

    nImgW := RoundTo(Args.Band.Width / 4, 0);
    nImgH := RoundTo(logoHmm * (nImgW/logoWmm), 0);
    if nImgH > MaxImgH then
    begin
      nImgH := MaxImgH;
      nImgW := RoundTo(logoWmm * (nImgH/logoHmm), 0);
    end;

    //estabelecer posições do texto
    xRs := nImgW + 0;
    wRs := Args.Band.Width - nImgW;
    alignH := 'L';

    if Args.FinalPass then
    begin
      Stream := TMemoryStream.Create;
      try
        Stream.Write(FLogo[0], Length(FLogo));
        PDF.Image(xImg, yImg, nImgW, nImgH, Stream, 'C', 'C');
        y := y + 5;
      finally
        Stream.Free;
      end;
    end;
  end
  else
  begin
    xRs := 0;
    wRs := Args.Band.Width;
    alignH := 'C';
  end;

  Texto := GetTextoBlocoCabecalho;
  Lines := ACBr_fpdf.Split(Texto, sLineBreak);
  PDF.SetFont(FFontFamily, 'B', 8);
  for I := 0 to Length(Lines) - 1 do
  begin
    y := y + PDF.TextBox(xRs + 2, y, wRs - 2, 0,
      Lines[I], 'T', alignH, 0, '', False);
    if I = 0 then
      PDF.SetFont(FFontFamily, '', 7);
  end;

  y := y + 3;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);

  y := y + 2;

  texto :=
    'DANFE NFC-e Documento Auxiliar da' + sLineBreak +
    'Nota Fiscal de Consumidor Eletronica';
  PDF.SetFont(8, 'B');
  y := y + PDF.TextBox(0, y, Args.Band.Width, 0, texto, 'T', 'C', 0, '', False);
  y := y + IncY;

  y := y + 2;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
end;

procedure TNFCeDANFeFPDF.BlocoChaveAcesso(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  texto: string;
  y1: double;
  y: double;
begin
  y := 0;
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  y1 := y;

  texto := 'Consulte pela Chave de Acesso em';
  PDF.SetFont(8, 'B');
  y1 := y1 + PDF.TextBox(0, y1, Args.Band.Width, 2, texto, 'T', 'C', 0, '', false);
  y1 := y1 + 0.5;

  texto := NFe.infNFeSupl.urlChave;
  PDF.SetFont(7, '');
  y1 := y1 + PDF.TextBox(0, y1, Args.Band.Width, 2, texto, 'T', 'C', 0, '', false);
  y1 := y1 + 0.5;

  texto := FormatarChaveAcesso(NFe.infNFe.Id);
  PDF.SetFont(7, '');
  PDF.TextBox(0, y1, Args.Band.Width, 2, texto, 'T', 'C', 0, '', true);
end;

procedure TNFCeDANFeFPDF.BlocoConsumidor(Args: TFPDFBandDrawArgs;
  var y: double);
var
  PDF: IFPDF;
  x1, y1, w, subSize: double;
  texto: string;
  hAlign: char;
begin
  PDF := Args.PDF;

  x1 := 0;
  y1 := y;
  if QRCodeLateral then
    y1 := y1 + 1;
  w := Args.Band.Width;
  hAlign := 'C';
  if QRCodeLateral then
  begin
    w := w * 0.7;
    x1 := x1 + (Args.Band.Width - w) + 2;
    hAlign := 'L';
  end;

//  PDF.TextBox(0, y1, Args.Band.Width, FBlocoConsumidorH, '', 'T', 'C', 1, '', true);

  subSize := 0;
  if FPaperWidth < 70 then
    subSize := 1.5;

  texto := GetTextoBlocoConsumidor;

  if NFe.Ide.tpEmis = teOffLine then
  begin
    PDF.SetFont((7 - subSize), '');
    y := y + PDF.TextBox(x1, y1, w, 0, texto, 'T', hAlign, 0, '', false);
  end
  else if NFe.Ide.tpEmis = teDPEC then
  begin
    PDF.SetFont(7, '');
    y := y + PDF.TextBox(x1, y1, w, 0, texto, 'T', hAlign, 0, '', false);
  end
  else
  begin
    PDF.SetFont(7, '');
    y := y + PDF.TextBox(x1, y1, w, 0, texto, 'T', hAlign, 0, '', false);
  end;
  y := y + 1;
end;

procedure TNFCeDANFeFPDF.BlocoIdentificacaoNFCe(Args: TFPDFBandDrawArgs;
  var y: double);
var
  PDF: IFPDF;

  function BlocoProtocolo(x, y, w: double; hAlign: char; subSize: double;
    const protocolo: string; dhRecbto: TDateTime): double;
  var
    texto: string;
    y1: double;
  begin
    y1 := y;
    texto := 'Protocolo de Autorização: ' + protocolo;
    PDF.SetFont((8 - subSize), '');
    y1 := y1 + PDF.TextBox(x, y1, w, 4, texto, 'T', hAlign, 0, '', True);
    y1 := y1 + 0.5;

    texto := 'Data de Autorização: ' + FormatDateTimeBr(dhRecbto);
    PDF.SetFont((8 - subSize), '');
    y1 := y1 + PDF.TextBox(x, y1, w, 4, texto, 'T', hAlign, 0, '', True);
    y1 := y1 + 0.5;

    Result := y1 - y;
  end;
var
  h, x1, y1, w, subSize: double;
  texto: string;
  hAlign: char;
begin
  PDF := Args.PDF;

  x1 := 0;
  y1 := y;
  w := Args.Band.Width;
  hAlign := 'C';
  if QRCodeLateral then
  begin
    w := (w * 0.7) - 2;
    x1 := x1 + (Args.Band.Width - w);
    hAlign := 'L';
  end;

//  PDF.TextBox(0, y1, Args.Band.Width, FBlocoIdentificacaoNFCeH, '', 'T', 'C', 1, '', true);

  subSize := 0;
  if (FPaperWidth < 70) or QRCodeLateral then
    subSize := 1.0;

  texto := Format('NFCe n. %s Série %s',
    [FormatFloat('000000000', NFe.Ide.nNF), FormatFloat('000', NFe.Ide.serie)]);
  if not QRCodeLateral then
    texto := Format('%s %s',[texto, FormatDateTimeBr(NFe.Ide.dEmi)]);

  PDF.SetFont(8-subSize, 'B');
  y1 := y1 + PDF.TextBox(x1, y1, w, 4, texto, 'T', hAlign, 0, '', False);

  if QRCodeLateral then
  begin
    texto := Format('Emissão: %s',[FormatDateTimeBr(NFe.Ide.dEmi)]);
    y1 := y1 + PDF.TextBox(x1, y1, w, 4, texto, 'T', hAlign, 0, '', False);
  end;

  if NFe.Ide.tpAmb = taHomologacao then
  begin
    y1 := y1 + 2;
    PDF.SetFont(10-subSize, 'B');
    texto := 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
    h := PDF.GetStringHeight(texto, w);
    y1 := y1 + PDF.TextBox(x1, y1, w, h, texto, 'T', hAlign, 0, '', False);
    y1 := y1 + 1;
  end;

  if NFe.Ide.tpEmis = teOffLine then
  begin
    texto := FVia;
    y1 := y1 + PDF.TextBox(x1, y1, w, 4, texto, 'T', hAlign, 0, '', true);

    y1 := y1 + 2;
    texto := 'EMITIDA EM CONTINGÊNCIA';
    PDF.SetFont(10-subSize, 'B');
    y1 := y1 + PDF.TextBox(x1, y1, w, 4, texto, 'B', hAlign, 0, '', true);

    texto := 'Pendente de autorização';
    PDF.SetFont(8-subSize, 'I');
    PDF.TextBox(x1, y1, w, 3, texto, 'B', hAlign, 0, '', true);
  end
  else if NFe.Ide.tpEmis = teDPEC then
  begin
    texto := 'DANFE-NFC-e Impresso em contingência - EPEC';
    PDF.SetFont((10-subSize), 'B');
    h := PDF.GetStringHeight(texto, w);
    y1 := y1 + PDF.TextBox(x1, y1, w, h, texto, 'T', hAlign, False);

    texto := 'Regularmente recebido pela administração tributária autorizadora';
    PDF.SetFont((8-subSize), '');
    h := PDF.GetStringHeight(texto, w);
    y1 := y1 + PDF.TextBox(x1, y1, w, h, texto, 'T', hAlign, False);

    if NFe.Ide.dhCont > 0 then
    begin
      texto := 'Data de entrada em contingência: ' + FormatDateTimeBr(NFe.Ide.dhCont);
      PDF.SetFont((7-subSize), '');
      h := PDF.GetStringHeight(texto, w);
      PDF.TextBox(x1, y1, w, h, texto, 'B', hAlign, 0, '', False);
    end;
  end
  else
  begin
    texto := FVia;
    h := PDF.GetStringHeight(texto, w);
    y1 := y1 + PDF.TextBox(x1, y1, w, h, texto, 'T', hAlign, 0, '', true);

    if NFe.procNFe.nProt = '' then
    begin
      texto := 'NFCe sem Autorização de Uso da SEFAZ';
      PDF.SetFont(8, 'I');
      h := PDF.GetStringHeight(texto, w);
      y1 := y1 + PDF.TextBox(x1, y1, w, h, texto, 'B', hAlign, 0, '', true);
    end
    else
      y1 := y1 + BlocoProtocolo(
        x1, y1, w, hAlign,
        subSize,
        NFe.procNFe.nProt,
        NFe.procNFe.dhRecbto
      );
  end;

  y := y1;
end;

procedure TNFCeDANFeFPDF.BlocoItens(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  matrix: array[0..5] of double;
  y, fsize, x, x1, x2, x3, x4, x5, y1, y2: double;
  texto: string;
  Prod: TProd;
  NFCeItem: TACBrNFCeItem;
  I: Integer;
begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  y := 0;

  matrix[0] := 0.14;
  matrix[1] := 0.36;
  matrix[2] := 0.09;
  matrix[3] := 0.09;
  matrix[4] := 0.156;
  matrix[5] := 0.156;

  fsize := 7;
  if FPaperWidth < 70 then
    fsize := 5;
  PDF.SetFont(fsize, 'B');

  y := y + 1;

  texto := 'CÓDIGO';
  x := 0;
  PDF.TextBox(x, y, (Args.Band.Width * matrix[0]), 3, texto, 'T', 'L', 0, '', true);

  texto := 'DESCRIÇÃO';
  x1 := x + (Args.Band.Width * matrix[0]);
  PDF.TextBox(x1, y, (Args.Band.Width * matrix[1]), 3, texto, 'T', 'L', 0, '', true);

  texto := 'QTDE';
  x2 := x1 + (Args.Band.Width * matrix[1]);
  PDF.TextBox(x2, y, (Args.Band.Width * matrix[2]), 3, texto, 'T', 'C', 0, '', true);

  texto := 'UN';
  x3 := x2 + (Args.Band.Width * matrix[2]);
  PDF.TextBox(x3, y, (Args.Band.Width * matrix[3]), 3, texto, 'T', 'C', 0, '', true);

  texto := 'VL UNIT';
  x4 := x3 + (Args.Band.Width * matrix[3]);
  PDF.TextBox(x4, y, (Args.Band.Width * matrix[4]), 3, texto, 'T', 'C', 0, '', true);

  texto := 'VL TOTAL';
  x5 := x4 + (Args.Band.Width * matrix[4]);
  y1 := PDF.TextBox(x5, y, (Args.Band.Width * matrix[5]), 3, texto, 'T', 'R', 0, '', true);

  y := y + 0.5;

  PDF.SetFont(fsize, '');
  y2 := y + y1;
  for I := 0 to NFe.Det.Count - 1 do
  begin
    Prod := NFe.Det[I].Prod;
    NFCeItem := GetNFCeItem(NFe.Det[I], Args.Band.Width * matrix[1], PDF);

    PDF.TextBox(x, y2, (Args.Band.Width * matrix[0]), NFCeItem.Height,
      Prod.cProd, 'T', 'L', 0, '', False);

    PDF.TextBox(x1, y2, (Args.Band.Width * matrix[1]), NFCeItem.Height,
        NFCeItem.XProd, 'T', 'L', 0, '', False);

    PDF.TextBox(x2, y2, (Args.Band.Width * matrix[2]), NFCeItem.Height,
      FormatFloat('#,0.00', Prod.qCom, FNFeUtils.FormatSettings), 'T', 'R', 0, '', False);

    PDF.TextBox(x3, y2, (Args.Band.Width * matrix[3]), NFCeItem.Height,
      Copy(Prod.uCom, 1, 2), 'T', 'C', 0, '', False);

    PDF.TextBox(x4, y2, (Args.Band.Width * matrix[4]), NFCeItem.Height,
      FormatFloat('#,0.00', Prod.vUnCom, FNFeUtils.FormatSettings), 'T', 'R', 0, '', False);

    PDF.TextBox(x5, y2, (Args.Band.Width * matrix[5]), NFCeItem.Height,
      FormatFloat('#,0.00', Prod.vProd, FNFeUtils.FormatSettings), 'T', 'R', 0, '', False);

    y2 := y2 + NFCeItem.Height;
  end;
  y := y2 + 1;

  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
end;

procedure TNFCeDANFeFPDF.BlocoLinha(Args: TFPDFBandDrawArgs);
var
  x, y: double;
begin
  x := 0;
  y := Args.Band.Height;
  Args.PDF.DashedLine(x, y, Args.Band.Width, y, FDashWidth);
end;

procedure TNFCeDANFeFPDF.BlocoMensagemContribuinte(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  texto, trib: string;
  vTotTrib: double;
  y, h: double;
begin
  y := 0;
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  PDF.SetFont(7, '');
  vTotTrib := NFe.Total.ICMSTot.vTotTrib;
  trib := IfThen(vTotTrib > 0, FormatFloat('#,0.00', vTotTrib), '-----');
  texto := Format(
    'Tributos Totais Incidentes (Lei Federal 12.741/2012): R$ %s', [trib]);
  PDF.SetFont(7, '');
  h := PDF.GetStringHeight(texto, Args.Band.Width);
  PDF.TextBox(0, y, Args.Band.Width, h, texto, 'T', 'L', 0, '', true);
  if FPaperWidth < 70 then
    PDF.SetFont(5, '');

  texto := StringReplace(NFe.InfAdic.infCpl, ';', sLineBreak, [rfReplaceAll]);
  h := PDF.GetStringHeight(texto, Args.Band.Width);
  PDF.TextBox(0, y+3, Args.Band.Width, h, texto, 'T', 'L', 0, '', false);
end;

procedure TNFCeDANFeFPDF.BlocoMensagemFiscal(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y, y1, TextoW, MargemBoxTexto: double;
  texto: string;
  BlocoMensagemFiscalH: double;
begin
  PDF := Args.PDF;

  y := 0;
  BlocoMensagemFiscalH := 9.5;
  if NFe.Ide.tpAmb = taHomologacao then
  begin
    PDF.SetFont(8, 'I');
    TextoW := PDF.GetStringWidth('EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO') + 2;
    MargemBoxTexto := (Args.Band.Width - TextoW) / 2;

    PDF.SetFillColor(224, 224, 224);
    PDF.Rect(0, y, Args.Band.Width, BlocoMensagemFiscalH, 'F');
    PDF.SetFillColor(cWhite);
    PDF.Rect(0  + MargemBoxTexto, y + 1, TextoW, BlocoMensagemFiscalH - 2, 'F');

    y1 := y + 1;

    PDF.SetFont(10, 'B');
    texto := 'SEM VALOR FISCAL';
    y1 := y1 + PDF.TextBox(0, y1, Args.Band.Width, 4, texto, 'T', 'C', 0, '', true);

    texto := 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO';
    PDF.SetFont(8, 'I');
    y1 := y1 + PDF.TextBox(0, y1, Args.Band.Width, 3, texto, 'T', 'C', 0, '', true);

    y1 := y1 + 2;

    PDF.DashedLine(0, y1, 0 + Args.Band.Width, y1, FDashWidth);
  end
  else if NFe.Ide.tpEmis = teOffLine then
  begin
    PDF.SetFont(10, 'B');
    texto := 'EMITIDA EM CONTINGÊNCIA';
    TextoW := PDF.GetStringWidth(texto) + 2;
    MargemBoxTexto := (Args.Band.Width - TextoW) / 2;

    PDF.SetFillColor(224, 224, 224);
    PDF.Rect(0, y, Args.Band.Width, BlocoMensagemFiscalH, 'F');
    PDF.SetFillColor(cWhite);
    PDF.Rect(0  + MargemBoxTexto, y + 1, TextoW, BlocoMensagemFiscalH - 2, 'F');

    y1 := y + 1;

    y1 := y1 + PDF.TextBox(0, y1, Args.Band.Width, 4, texto, 'T', 'C', 0, '', true);

    texto := 'Pendente de autorização';
    PDF.SetFont(8, 'I');
    y1 := y1 + PDF.TextBox(0, y1, Args.Band.Width, 3, texto, 'T', 'C', 0, '', true);

    y1 := y1 + 2;

    PDF.DashedLine(0, y1, 0 + Args.Band.Width, y1, FDashWidth);
  end;
end;

procedure TNFCeDANFeFPDF.BlocoPagamentos(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  texto: string;
  y1, y2, z: double;
  I: Integer;
  y: double;
begin
  y := 0;
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  PDF.SetFont(7, '');
  texto := 'FORMA PAGAMENTO';
  PDF.TextBox(0, y, Args.Band.Width, 4, texto, 'T', 'L', 0, '', false);
  texto := 'VALOR PAGO R$';
  y1 := PDF.TextBox(0, y, Args.Band.Width, 4, texto, 'T', 'R', 0, '', false);

  z := y + y1;
  for I := 0 to NFe.pag.Count - 1 do
  begin
    PDF.TextBox(0, z, Args.Band.Width, 3, FormaPagamentoToDescricao(NFe.pag[I].tPag, NFe.pag[I].xPag),
      'T', 'L', 0, '', false);

    y2 := PDF.TextBox(0, z, Args.Band.Width, 3,
      FormatFloat('#,0.00', NFe.pag[I].vPag, FNFeUtils.FormatSettings),
      'T', 'R', 0, '', false);
    z := z + y2;
  end;

  texto := 'Troco R$';
  PDF.TextBox(0, z, Args.Band.Width, 3, texto, 'T', 'L', 0, '', false);
  texto := FormatFloat('#,0.00', NFe.pag.vTroco, FNFeUtils.FormatSettings);
  PDF.TextBox(0, z, Args.Band.Width, 3, texto, 'T', 'R', 0, '', false);

  y := z + 3;

  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
end;

procedure TNFCeDANFeFPDF.BlocoQRCode(Args: TFPDFBandDrawArgs; var y: double);
var
  PDF: IFPDF;
  y1: double;
  w: double;
  QrX, QrY, QrSize: double;
begin
  PDF := Args.PDF;

  y1 := y;

  w := (Args.Band.Width * 1) + (2 * 0);
  y1 := y1 + 1;

  if FQRCodeLateral then
    QrSize := Args.Band.Width * 0.3
  else
    QrSize := Args.Band.Width * 0.4;

  QrY := y1;
  if QRCodeLateral then
    QrX := 0
  else
    QrX := (w / 2) - (QrSize / 2);

  PDF.SetFillColor(0, 0, 0);
  PDF.QRCode(QrX, QrY, QrSize, NFe.infNFeSupl.qrCode);
end;

procedure TNFCeDANFeFPDF.BlocoQRCodeCentralizadoOuLateral(
  Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y, QrY: double;
begin
  y := 1.5;
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  QrY := y;
  BlocoConsumidor(Args, y);
  BlocoIdentificacaoNFCe(Args, y);
  if QRCodeLateral then
    y := QrY;
  BlocoQRCode(Args, y);
end;

procedure TNFCeDANFeFPDF.BlocoRodape(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  Mensagens: TStringArray;
  y: double;
begin
  y := 2;
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  if FMensagemRodape = '' then
    Exit;
  Mensagens := ACBr_fpdf.Split(FMensagemRodape, '|');

  PDF.SetFont(6, 'I');
  if FPaperWidth < 70 then
    PDF.SetFont(4, 'I');

  if Length(Mensagens) >= 1 then
    PDF.TextBox(0, y, Args.Band.Width, 0, Mensagens[0], 'T', 'L', 0, '', true);
  if Length(Mensagens) >= 2 then
    PDF.TextBox(0, y, Args.Band.Width, 0, Mensagens[1], 'T', 'C', 0, '', true);
  if Length(Mensagens) >= 3 then
    PDF.TextBox(0, y, Args.Band.Width, 0, Mensagens[2], 'T', 'R', 0, '', true);
end;

procedure TNFCeDANFeFPDF.BlocoTotais(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y, y1, y2, y3, y4: double;
  texto: string;
  valor, desconto, frete, bruto: double;
  fsize: double;
begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  y := 0;

  valor := NFe.Total.ICMSTot.vNF;
  desconto := NFe.Total.ICMSTot.vDesc;
  frete := NFe.Total.ICMSTot.vFrete;
  bruto := valor + desconto - frete;

  PDF.SetFont(8, '');
  texto := 'Qtd. Total de Itens';
  PDF.TextBox(0, y, Args.Band.Width / 2, 3, texto, 'T', 'L', 0, '', false);
  y1 := PDF.TextBox(0 + Args.Band.Width / 2, y, Args.Band.Width / 2, 3,
      FormatFloat('#,0', NFe.Det.Count, FNFeUtils.FormatSettings),
      'T', 'R', 0, '', false);

  texto := 'Valor Total R$';
  PDF.TextBox(
      0,
      y + y1,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'L',
      0,
      '',
      false
  );
  texto := FormatFloat('#,0.00', bruto, FNFeUtils.FormatSettings);
  y2 := PDF.TextBox(
      0 + Args.Band.Width / 2,
      y + y1,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'R',
      0,
      '',
      false
  );

  texto := 'Desconto R$';
  PDF.TextBox(
      0,
      y + y1 + y2,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'L',
      0,
      '',
      false
  );
  texto := FormatFloat('#,0.00', desconto, FNFeUtils.FormatSettings);
  y3 := PDF.TextBox(
      0 + Args.Band.Width / 2,
      y + y1 + y2,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'R',
      0,
      '',
      false
  );

  texto := 'Frete R$';
  PDF.TextBox(
      0,
      y + y1 + y2 + y3,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'L',
      0,
      '',
      false
  );
  texto := FormatFloat('#,0.00', frete, FNFeUtils.FormatSettings);
  y4 := PDF.TextBox(
      0 + Args.Band.Width / 2,
      y + y1 + y2 + y3,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'R',
      0,
      '',
      false
  );

  fsize := 10;
  if FPaperWidth < 70 then
    fsize := 8;
  PDF.SetFont(fsize, 'B');
  texto := 'Valor a Pagar R$';
  PDF.TextBox(
      0,
      y + y1 + y2 + y3 + y4,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'L',
      0,
      '',
      false
  );
  texto := FormatFloat('#,0.00', valor, FNFeUtils.FormatSettings);
  PDF.TextBox(
      0 + Args.Band.Width / 2,
      y + y1 + y2 + y3 + y4,
      Args.Band.Width / 2,
      3,
      texto,
      'T',
      'R',
      0,
      '',
      false
  );

  PDF.DashedLine(0, Args.Band.Height + y, 0 + Args.Band.Width, Args.Band.Height + y, FDashWidth);
end;

constructor TNFCeDANFeFPDF.Create(ANFe: TNFe; AACBrNFeDANFCEClass : TACBrNFeDANFCEClass);
var
  LFormatSettings: TFormatSettings;
begin
  inherited Create;
  FNFe := ANFe;
  FNFeUtils := TNFeUtilsFPDF.Create(ANFe, AACBrNFeDANFCEClass);
  Self.FDANFEClassOwner := AACBrNFeDANFCEClass;
  FImageUtils := TImageUtils.Create;
  {$IFDEF HAS_FORMATSETTINGS}
    LFormatSettings := CreateFormatSettings;
  {$ENDIF}
  LFormatSettings.DecimalSeparator  := ',';
  LFormatSettings.ThousandSeparator := '.';
  FNFeUtils.FormatSettings := LFormatSettings;

  FFontFamily := 'Arial';
  FDashWidth := 1;
  FVia := 'Via Consumidor';
  FExibirItens := True;
  SetFont('Times');

  EngineOptions.DoublePass := True;
  FPaperWidth := 80;
  FPaperHeight := 20;
  SetFont(FFontFamily);
  SetMargins(2, 2);
end;

destructor TNFCeDANFeFPDF.Destroy;
begin
  FNFeUtils.Free;
  FImageUtils.Free;
  inherited;
end;

function TNFCeDANFeFPDF.GetNFCeItem(Det: TDetCollectionItem;
  ADescriptionWidth: double; PDF: IFPDF): TACBrNFCeItem;
var
  HFont, h, n: double;
  Limit: integer;
  XProd, p: string;
begin
  HFont := PDF.CurrentFontSize;
  Limit := 45;
  XProd := Copy(Det.Prod.xProd, 1, Limit);
  n := PDF.WordWrap(XProd, ADescriptionWidth);
  while n > 2 do
  begin
    Dec(Limit);
    XProd := Copy(Det.Prod.xProd, 1, Limit);
    p := xProd;
    n := PDF.WordWrap(p, ADescriptionWidth);
  end;
  h := (hfont * n) + 0.5;
  Result.Height := h;
  Result.XProd := XProd;
end;

function TNFCeDANFeFPDF.GetTextoBlocoCabecalho: string;
var
  Emit: TEmit;
begin
  Emit := NFe.Emit;
  Result := '';
  if ExibirNomeFantasia and (Emit.xFant <> '') then
    Result := Emit.xFant + sLineBreak;
  Result := Result +
    Emit.xNome + sLineBreak +
    Emit.EnderEmit.xLgr +
    IfThen(Emit.EnderEmit.nro <> '', ', ' + Emit.EnderEmit.nro) + ', ' +
    Emit.EnderEmit.xBairro + ', ' +
    Emit.EnderEmit.xMun + ', ' +
    Emit.EnderEmit.UF + sLineBreak +
    Format('CNPJ: %s    IE: %s', [FormatarCNPJouCPF(Emit.CNPJCPF), Emit.IE]);
end;

function TNFCeDANFeFPDF.GetTextoBlocoConsumidor: string;
begin
  Result := '';
  if NFe.Dest.CNPJCPF <> '' then
  begin
    if Length(NFe.Dest.CNPJCPF) = 14 then
      Result := 'CONSUMIDOR - CNPJ: ' + FormatarCNPJouCPF(NFe.Dest.CNPJCPF)
    else if Length(NFe.Dest.CNPJCPF) = 11 then
      Result := 'CONSUMIDOR - CPF: ' + FormatarCNPJouCPF(NFe.Dest.CNPJCPF);

    if ExibirConsumidorNome then
      Result := Result + IfThen(NFe.Dest.xNome <> '', sLineBreak + NFe.Dest.xNome);
  end
  else
    Result := 'CONSUMIDOR NÃO IDENTIFICADO';

  if ExibirConsumidorEndereco and (NFe.Dest.EnderDest.xLgr <> '') then
    Result := Result + sLineBreak +
      Format('%s, %s %s %s %s-%s',
      [NFe.Dest.EnderDest.xLgr, NFe.Dest.EnderDest.nro, NFe.Dest.EnderDest.xCpl,
       NFe.Dest.EnderDest.xBairro, NFe.Dest.EnderDest.xMun, NFe.Dest.EnderDest.UF]);
end;

procedure TNFCeDANFeFPDF.OnStartReport(Args: TFPDFReportEventArgs);
var
  Page: TFPDFPage;
  LLogoStringStream : TStringStream;
  LStream : TMemoryStream;
begin
  if not FInitialized then
  begin
    if FNFe = nil then
      raise Exception.Create('FACBrNFCe not initialized');

    if FPaperHeight > FPaperWidth  then
      Page := AddPage(poPortrait, puMM, FPaperWidth, FPaperHeight)
    else
      Page := AddPage(poLandscape, puMM, FPaperWidth, FPaperHeight);
    Page.EndlessHeight := True;

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

    AddBand(btData, 10, BlocoCabecalho);
    AddBand(btData, 10, BlocoMensagemFiscal);
    if ExibirItens then
      AddBand(btData, 10, BlocoItens);
    AddBand(btData, 16, BlocoTotais);
    AddBand(btData, 10, BlocoPagamentos);
    AddBand(btData, 10, BlocoChaveAcesso);
    AddBand(btData, 10, BlocoQRCodeCentralizadoOuLateral);
    AddBand(btData, 2, BlocoLinha);
    AddBand(btData, 10, BlocoMensagemContribuinte);
    if MensagemRodape <> '' then
      AddBand(btData, 5, BlocoRodape);

    FInitialized := True;
  end;
end;

{ TACBrNFeDANFeFPDFClass }

constructor TACBrNFCeDANFeFPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Inicialize outras configurações do seu componente aqui

end;

destructor TACBrNFCeDANFeFPDF.Destroy;
begin
  FFPDFReport.Free;
  inherited Destroy;
end;

procedure TACBrNFCeDANFeFPDF.ImprimirDANFE(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFCeDANFeFPDF.ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe);
begin

end;

procedure TACBrNFCeDANFeFPDF.ImprimirDANFEResumido(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFCeDANFeFPDF.ImprimirDANFEPDF(NFE: TNFe);
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
    Report := TNFCeDANFeFPDF.Create(LNFe,TACBrNFeDANFCEClass(TACBrNFe(ACBrNFe).DANFE));

    //TNFCeDANFeFPDF(Report).PosCanhoto := TNFCeDANFeFPDF(TACBrNFe(ACBrNFe).DANFE).PosCanhoto;

    TNFCeDANFeFPDF(Report).MensagemRodape := Self.Sistema;

    try
      Engine := TFPDFEngine.Create(Report, False);
      try
        Engine.Compressed := True;

        LPAth := IncludeTrailingPathDelimiter(TACBrNFe(ACBrNFe).DANFE.PathPDF) +
                                              ExtractFilePath(TACBrNFe(ACBrNFe).DANFE.NomeDocumento);

        Engine.SaveToFile(LPath + LNFe.infNFe.ID+'.pdf');
      finally
        Engine.Free;
      end;
    finally
      Report.Free;
    end;
  end;
end;

procedure TACBrNFCeDANFeFPDF.ImprimirEVENTO(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFCeDANFeFPDF.ImprimirEVENTOPDF(AStream: TStream; ANFe: TNFe);
begin
  inherited;
end;

procedure TACBrNFCeDANFeFPDF.ImprimirEVENTOPDF(NFE: TNFe);
begin
  inherited;
end;

procedure TACBrNFCeDANFeFPDF.ImprimirINUTILIZACAO(NFE: TNFe);
begin
  inherited;

end;

procedure TACBrNFCeDANFeFPDF.ImprimirINUTILIZACAOPDF(AStream: TStream;
  ANFe: TNFe);
begin
  inherited;
end;

procedure TACBrNFCeDANFeFPDF.ImprimirINUTILIZACAOPDF(NFE: TNFe);
begin
  inherited;
end;

end.
