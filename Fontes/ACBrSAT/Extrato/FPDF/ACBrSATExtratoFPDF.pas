{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Alexandre Ballestero de Paula                   }
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

unit ACBrSATExtratoFPDF;

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
  pcnCFe,
  pcnCFeCanc,
  pcnConversao,
  ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.Compatibilidade,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrDFeUtil,
  ACBrSATExtratoUtilsFPDF,
  ACBrSATExtratoReportClass,
  ACBrSAT,
  ACBrBase,
  ACBrSATExtratoClass;

type
  TACBrNFCeItem = record
    Height: double;
    XProd: string;
  end;

  TSATExtratoFPDF = class(TFPDFReport)
  private
    FCFe: TCFe;
    FCFeCanc: TCFeCanc;
    FCFeUtils: TCFeUtilsFPDF;
    FSATExtratoClassOwner : TACBrSATExtratoClass;
    FCancelamento: boolean;
    FFormatSettings: TFormatSettings;
    FInitialized: boolean;
    FFontFamily: string;
    FPaperWidth: double;
    FPaperHeight: double;
    FLogoAlign: TLogoAlign;
    FLogo: TBytes;
    FVia: string;
    FDashWidth: double;
    FQRCodeLateral: boolean;
    FImageUtils: TImageUtils;
    property CFe: TCFe read FCFe;
    property CFeCanc: TCFeCanc read FCFeCanc;
    procedure GerarLogo(const PDF: IFPDF; out xRs, wRs: double; out alignH: char;
        Args: TFPDFBandDrawArgs);
    function GetTextoBlocoCabecalho: string;
    function GetTextoBlocoConsumidor: string;
    procedure CarregaLogoEmFLogo;
    procedure ImprimeChave(Args: TFPDFBandDrawArgs; var y: double);
    procedure ImprimeDadosSATRodape(var x,y: Double; PDF: IFPDF; var
        Largura:Double);

  protected
    procedure GerarBlocoCabecalho(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoItens(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoTotais(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoPagamentos(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoObsFisco(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoDadosEntrega(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoObsContribuinte(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoRodape(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoDadosCancelamento(Args: TFPDFBandDrawArgs);
    procedure GerarBlocoFechamento(Args: TFPDFBandDrawArgs);

    procedure BlocoConsumidorSimples(Args: TFPDFBandDrawArgs);
    procedure BlocoQRCode(Args: TFPDFBandDrawArgs; var y: double);

    procedure OnStartReport(Args: TFPDFReportEventArgs); override;
  public
    constructor Create(ACFe: TCFe; PaiExtrato: TACBrSATExtratoReportClass); reintroduce;
    destructor Destroy; override;

    property Cancelamento: boolean read FCancelamento write FCancelamento;
    property LogoAlign: TLogoAlign read FLogoAlign write FLogoAlign;
    property QRCodeLateral: boolean read FQRCodeLateral write FQRCodeLateral;
  end;


  TACBrSATExtratoFPDF = class(TACBrSATExtratoReportClass)
  private
  protected
    procedure Imprimir;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirExtrato(ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;

    procedure ImprimirExtrato(AStream: TStream; ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(AStream: TStream; ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(AStream: TStream; ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;
  end;

const
  cDefaultFontFamily = 'Times';

implementation

{ TSATExtratoFPDF }

procedure TSATExtratoFPDF.GerarBlocoCabecalho(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y, IncY: double;

  procedure ImprimeTextoTesteIndicativoDeAmbiente;
  var
    texto: string;
    TamanhoLinha: Integer;
    i: Integer;
  begin
    PDF.SetFont(8, 'B');
    texto := ' = T E S T E =';
    y := y + PDF.TextBox(0, y, Args.Band.Width, 0, texto, 'T', 'C', 0, '', False);
    y := y + IncY;

    //Imprime Linha ">>>>>" 3 vezes
    TamanhoLinha := trunc(Args.Band.Width * 0.6);
    for i := 0 to 2 do
    begin
      texto := StringOfChar('>', TamanhoLinha);
      y := y + PDF.TextBox(0, y, Args.Band.Width, 0, texto, 'T', 'C', 0, '', False);
      y := y + IncY;
    end;
  end;
var
  xRs, wRs: double;
  alignH: char;
  texto: string;
  Lines: TStringArray;
  I: integer;
  sCFe: string;
begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  y := 0;
  IncY := 0.5;

  if (Length(FLogo) > 0) then
  begin
    GerarLogo(PDF, xRs, wRs, alignH, Args);
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

  //TODO: A linha tracejada abaixo parece ser facultativa a partir de 2019 (Ver MO 2.19.04)
  y := y + 1;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);

  //Daqui para baixo já é o Corpo do Extrato
  y := y + 2;
  if (CFe.ide.tpAmb = taHomologacao) then
    sCFe := StringOfChar('0',6)
  else if Cancelamento then
    sCFe := IntToStrZero( CFeCanc.ide.nCFe, 6)
  else
    sCFe := IntToStrZero( CFE.ide.nCFe, 6);

  texto :=
    'Extrato No. ' + sCFe + sLineBreak +
    'CUPOM FISCAL ELETRÔNICO - SAT';
  PDF.SetFont(8, 'B');
  y := y + PDF.TextBox(0, y, Args.Band.Width, 0, texto, 'T', 'C', 0, '', False);
  y := y + IncY;

  if CFe.ide.tpAmb = taHomologacao then
  begin
    ImprimeTextoTesteIndicativoDeAmbiente;
  end;

  y := y + 1;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);

end;

procedure TSATExtratoFPDF.GerarBlocoDadosEntrega(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  Texto: string;
  y: Double;
begin
  PDF := Args.PDF;

  y := 0;
  if (Trim(CFe.Entrega.xLgr)+
      Trim(CFe.Entrega.nro)+
      Trim(CFe.Entrega.xCpl)+
      Trim(CFe.Entrega.xBairro)+
      Trim(CFe.Entrega.xMun) <> '') then
  begin
     Texto := ACBrStr('DADOS PARA ENTREGA:');
     Texto := Texto+' '+Trim(CFe.Entrega.xLgr)+' '+
                     ifthen(Trim(CFe.Entrega.nro)<>'',', '+Trim(CFe.Entrega.nro),'') + ' ' +
                     ifthen(Trim(CFe.Entrega.xCpl)<>'',Trim(CFe.Entrega.xCpl) + ' ','') +
                     ifthen(Trim(CFe.Entrega.xBairro)<>'',Trim(CFe.Entrega.xBairro) + ' ','') +
                     Trim(CFe.Entrega.xMun)+'-'+CFe.Entrega.UF;

    PDF.SetFont(7, '');
    y := y + PDF.TextBox(0, y, Args.Band.Width, 0, Texto, 'T', 'C', 0, '', false);

    y := y + 2;
    PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
  end;
end;

procedure TSATExtratoFPDF.BlocoConsumidorSimples(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  Texto: string;
  y: Double;
begin
  PDF := Args.PDF;

  y := 0;
  Texto := GetTextoBlocoConsumidor;

  PDF.SetFont(7, '');
  y := y + PDF.TextBox(0, y, Args.Band.Width, 0, Texto, 'T', 'C', 0, '', false);
//  y := y + 1;
end;

procedure TSATExtratoFPDF.GerarBlocoItens(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y, h, fsize: double;
  Texto: string;
  I: Integer;

  sItem, sCodigo, sDescricao, sQuantidade, sUnidade, sVlrUnitario, sVlrImpostos, sVlrBruto:string;
  TextoItem,TextoItemAdicional: string;

begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  y := 0;

  fsize := 7;
  if FPaperWidth < 70 then
    fsize := 5;
  PDF.SetFont(fsize, 'B');

  Texto := '#|COD|DESC|QTD|UN|VL UN R$|(VL TR R$)*|VL ITEM R$';
  y := y + PDF.TextBox(0, y, Args.Band.Width, 0, Texto, 'T', 'C', 0, '', true);
  y := y + 2;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);

  PDF.SetFont(fsize, '');
  for I := 0 to FCFeUtils.CFe.Det.Count - 1 do
  begin
    TextoItem := '';
    sItem := IntToStrZero(CFe.Det.Items[i].nItem, 3);
    if FSATExtratoClassOwner.ImprimeCodigoEan and (Trim(CFe.Det.Items[i].Prod.cEAN) <> '') then
      sCodigo := Trim(CFe.Det.Items[i].Prod.cEAN)
    else
      sCodigo := Trim(CFe.Det.Items[i].Prod.cProd);

    sDescricao := Trim(CFe.Det.Items[i].Prod.xProd);

    // formatar conforme configurado somente quando houver decimais
    // caso contrário mostrar somente o número inteiro
    if (Frac(CFe.Det.Items[i].Prod.QCom) > 0) then  // Tem decimais ?
      sQuantidade := FormatFloatBr(CFe.Det.Items[i].Prod.QCom, FSATExtratoClassOwner.CasasDecimais.MaskqCom )
    else
      sQuantidade := IntToStr(Trunc(CFe.Det.Items[i].Prod.QCom));

    sUnidade := Trim(CFe.Det.Items[i].Prod.uCom);

    // formatar conforme configurado
    sVlrUnitario := FormatFloatBr(CFe.Det.Items[i].Prod.vUnCom,
      IfThen(CFe.Det.Items[i].Prod.EhCombustivel, ',0.000', FSATExtratoClassOwner.CasasDecimais.MaskvUnCom));

    if (CFe.Det.Items[i].Imposto.vItem12741 > 0) then
      sVlrImpostos := ' ('+FormatFloatBr(CFe.Det.Items[i].Imposto.vItem12741)+') '
    else
      sVlrImpostos := ' ';

    sVlrBruto := FormatFloatBr(CFe.Det.Items[i].Prod.vProd);

//   if ImprimeEmUmaLinha then
//    begin
//      LinhaCmd := sItem + ' ' + sCodigo + ' ' + '[DesProd] ' + sQuantidade + ' ' +
//        sUnidade + ' X ' + sVlrUnitario + sVlrImpostos + sVlrBruto;
//
//      // acerta tamanho da descrição
//      nTamDescricao := FPosPrinter.ColunasFonteCondensada - Length(LinhaCmd) + 9;
//      sDescricao := PadRight(Copy(sDescricao, 1, nTamDescricao), nTamDescricao);
//
//      LinhaCmd := StringReplace(LinhaCmd, '[DesProd]', sDescricao, [rfReplaceAll]);
//      FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);
//    end
//    else
    begin
      TextoItem := sItem + ' ' + sCodigo + ' ' + sDescricao;

      TextoItem := TextoItem+ sQuantidade +' '+ sUnidade + ' X ' +
                  sVlrUnitario + ' ' + sVlrImpostos + sVlrBruto;
    end;

    h := PDF.GetStringHeight(TextoItem, Args.Band.Width);
    y := y + PDF.TextBox(0, y, Args.Band.Width , h, TextoItem, 'T', 'L', 0, '', False);

    TextoItemAdicional := '';
    if FSATExtratoClassOwner.ImprimeDescAcrescItem then
    begin
      // desconto
      if (CFe.Det.Items[i].Prod.vDesc > 0) then
      begin
        TextoItemAdicional := 'desconto sobre item ' + FormatFloatBr(CFe.Det.Items[i].Prod.vDesc, '-,0.00');
        y := y + PDF.TextBox(0, y, Args.Band.Width , 0, TextoItem, 'T', 'L', 0, '', False);
      end;

      // acrescimo
      if (CFe.Det.Items[i].Prod.vOutro > 0) then
      begin
        TextoItemAdicional := ACBrStr('acréscimo sobre item ') + FormatFloatBr(CFe.Det.Items[i].Prod.vOutro, '+,0.00');
        y := y + PDF.TextBox(0, y, Args.Band.Width , 0, TextoItem, 'T', 'L', 0, '', False);
      end;

      // Rateio de Desconto
      if (CFe.Det.Items[i].Prod.vRatDesc > 0) then
      begin
        TextoItemAdicional := 'rateio de desconto sobre subtotal ' + FormatFloatBr(CFe.Det.Items[i].Prod.vRatDesc, '-,0.00');
        y := y + PDF.TextBox(0, y, Args.Band.Width , 0, TextoItem, 'T', 'L', 0, '', False);
      end;

      // Rateio de Acréscimo
      if (CFe.Det.Items[i].Prod.vRatAcr > 0) then
      begin
        TextoItemAdicional := ACBrStr('rateio de acréscimo sobre subtotal ') + FormatFloatBr(CFe.Det.Items[i].Prod.vRatAcr, '+,0.00');
        y := y + PDF.TextBox(0, y, Args.Band.Width , 0, TextoItem, 'T', 'L', 0, '', False);
      end;
    end;

    if (CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN > 0) then
    begin
      TextoItemAdicional := ACBrStr('dedução para ISSQN ') + FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN, '-,0.00');
      y := y + PDF.TextBox(0, y, Args.Band.Width , 0, TextoItem, 'T', 'L', 0, '', False);
      TextoItemAdicional := ACBrStr('base de cálculo ISSQN ') + FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vBC);
      y := y + PDF.TextBox(0, y, Args.Band.Width , 0, TextoItem, 'T', 'L', 0, '', False);
    end;

//    y := y + 2;
  end;
  y := y + 2;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
end;

procedure TSATExtratoFPDF.GerarBlocoObsContribuinte(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  Texto, trib: string;
  vTotTrib: double;
  y, h: double;
begin
  Args.Band.AutoHeight := True;
  //tem obsContribuinte?
  if (CFe.InfAdic.infCpl = '') and (FCFeUtils.CFe.Total.vCFeLei12741 = 0) then
  begin
    Exit;
  end;

  y := 0;
  PDF := Args.PDF;
  PDF.SetFont(7, '');

  Texto := ACBrStr('OBSERVAÇÕES DO CONTRIBUINTE');
  y := y + PDF.TextBox(0, y, Args.Band.Width, 0, Texto, 'T', 'L', 0, '', true);

  vTotTrib := FCFeUtils.CFe.Total.vCFeLei12741;
  trib := IfThen(vTotTrib > 0, FormatFloat('#,0.00', vTotTrib), '-----');
  Texto := Format(
    'Valor aproximado dos tributos deste cupom (Lei Fed. 12.741/2012): R$ %s', [trib]);
  h := PDF.GetStringHeight(Texto, Args.Band.Width);
  y := y + PDF.TextBox(0, y, Args.Band.Width, h, Texto, 'T', 'L', 0, '', true);
  if FPaperWidth < 70 then
    PDF.SetFont(5, '');

  Texto := StringReplace(CFe.InfAdic.infCpl, ';', sLineBreak, [rfReplaceAll]);
  h := PDF.GetStringHeight(Texto, Args.Band.Width);
  y:= y + PDF.TextBox(0, y+3, Args.Band.Width, h, Texto, 'T', 'L', 0, '', false);

  y := y + 2;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
end;

procedure TSATExtratoFPDF.GerarBlocoObsFisco(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y: double;
  Texto: string;
  i : integer;
begin
  PDF := Args.PDF;
  y := 0;
  Texto := '';

  if (CFe.InfAdic.obsFisco.Count = 0) and
     (CFe.Emit.cRegTrib <> RTSimplesNacional) then
  begin
    y := y + 2;
    PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
    Exit;
  end;

  if (CFe.Emit.cRegTrib = RTSimplesNacional) then
  begin
    y := y + PDF.TextBox(0, y, Args.Band.Width, 3, Msg_ICMS_123_2006, 'T', 'C', 0, '', true);
  end;

  for i := 0 to CFe.obsFisco.Count - 1 do
  begin
     Texto := Texto + CFe.obsFisco[i].xCampo+'-'+
                            CFe.obsFisco[i].xTexto;
    y := y + PDF.TextBox(0, y, Args.Band.Width, 0, Texto, 'T', 'C', 0, '', true);

  end;

  y := y + 2;
  PDF.DashedLine(0, y, 0 + Args.Band.Width, y, FDashWidth);
end;

procedure TSATExtratoFPDF.GerarBlocoPagamentos(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  Texto: string;
  I: Integer;
  y: double;
begin
  y := 0;
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  PDF.SetFont(7, '');
  for I := 0 to FCFeUtils.CFe.Pagto.Count - 1 do
  begin
    PDF.TextBox(0, y, Args.Band.Width, 3,  CodigoMPToDescricao(FCFeUtils.CFe.Pagto[I].cMP),
      'T', 'L', 0, '', false);

    y := y + PDF.TextBox(0, y, Args.Band.Width, 3,
      FormatFloat('#,0.00', FCFeUtils.CFe.Pagto[I].vMP, FFormatSettings),
      'T', 'R', 0, '', false);
  end;

  if FCFeUtils.CFe.Pagto.vTroco > 0 then
  begin
    Texto := 'Troco R$';
    PDF.TextBox(0, y, Args.Band.Width, 3, Texto, 'T', 'L', 0, '', false);
    Texto := FormatFloat('#,0.00', FCFeUtils.CFe.Pagto.vTroco, FFormatSettings);
    y := y + PDF.TextBox(0, y, Args.Band.Width, 3, Texto, 'T', 'R', 0, '', false);
  end;
end;

procedure TSATExtratoFPDF.BlocoQRCode(Args: TFPDFBandDrawArgs; var y: double);
var
  PDF: IFPDF;
  x1,y1: double;
  w: double;
  QrX, QrY, QrSize: double;
  DadosQRCode: String;
  Largura: Double;
begin
  PDF := Args.PDF;

  y1 := y;
  x1 := 0;

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
  DadosQRCode := FCFeUtils.SATExtratoClassOwner.CalcularConteudoQRCode(  FCFeUtils.CFe.infCFe.ID,
                                       FCFeUtils.CFe.ide.dEmi+FCFeUtils.CFe.ide.hEmi,
                                       FCFeUtils.CFe.Total.vCFe,
                                       Trim(FCFeUtils.CFe.Dest.CNPJCPF),
                                       FCFeUtils.CFe.ide.assinaturaQRCODE );
  PDF.QRCode(QrX, QrY, QrSize, DadosQRCode);

  if FQRCodeLateral then
  begin
    x1 := QrX+QrSize+0;
    Largura := Args.Band.Width - x1;
    ImprimeDadosSATRodape(x1, y1, PDF, Largura);
  end;

end;

procedure TSATExtratoFPDF.GerarBlocoRodape(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y, x, Largura: double;
begin
  x := 0;
  y := 0;
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  if not QRCodeLateral then
  begin
    Largura := Args.Band.Width;
    ImprimeDadosSATRodape(y, x, PDF, Largura);
  end;

  ImprimeChave(Args, y);

  if  FSATExtratoClassOwner.ImprimeQRCode then
  begin
    BlocoQRCode(Args, y);
  end;
end;

procedure TSATExtratoFPDF.GerarBlocoTotais(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  y: double;
  meiaLargura: double;
  Texto: string;
  ValorTotal, TotDesconto, TotBruto: double;
  fsize: double;
begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;

  y := 0;

  if Cancelamento then
  begin
    PDF.SetFont(8, '');
    Texto := 'TOTAL R$ '+ FormatFloatBr(CFe.Total.vCFe);
    y := y + PDF.TextBox(0, y, Args.Band.Width, 3, Texto, 'T', 'R', 0, '', false);
    Exit;
  end;

  ValorTotal := CFe.Total.vCFe;
  TotDesconto := CFe.Total.ICMSTot.vDesc;
  TotBruto := CFe.Total.ICMSTot.vProd;
  meiaLargura := Args.Band.Width / 2;

  if (ValorTotal <> TotBruto) then
  begin
    PDF.SetFont(8, '');
    Texto := 'Total bruto de itens';
    PDF.TextBox(0, y , meiaLargura, 3, Texto, 'T', 'L', 0, '', false);
    Texto := FormatFloat('#,0.00', TotBruto, FFormatSettings);
    y := y + PDF.TextBox(0 + meiaLargura, y, meiaLargura, 3, Texto, 'T', 'R', 0, '', false);

    if TotDesconto > 0 then
    begin
      Texto := 'Total de descontos/acréscimos sobre item';
      PDF.TextBox(0, y, meiaLargura, 3, Texto, 'T', 'L', 0, '', false);
      Texto := FormatFloat('#,0.00', TotDesconto, FFormatSettings);
      y := y + PDF.TextBox(meiaLargura, y, meiaLargura, 3, Texto, 'T', 'R', 0, '', false);
    end;

    if CFe.Total.DescAcrEntr.vDescSubtot <> 0 then
    begin
      if (CFe.Total.DescAcrEntr.vDescSubtot > 0) then
        Texto := 'Acréscimo sobre subtotal'
      else
        Texto := 'Desconto sobre subtotal';
      PDF.TextBox(0, y, meiaLargura, 3, Texto, 'T', 'L', 0, '', false);

      if (CFe.Total.DescAcrEntr.vDescSubtot > 0) then
        Texto := FormatFloatBr(CFe.Total.DescAcrEntr.vAcresSubtot, '+,0.00')
      else
        Texto := FormatFloatBr(CFe.Total.DescAcrEntr.vDescSubtot, '-,0.00');
      y := y + PDF.TextBox(meiaLargura, y, meiaLargura, 3, Texto, 'T', 'R', 0, '', false);
    end;
  end;

  fsize := 10;
  if FPaperWidth < 70 then
    fsize := 8;
  PDF.SetFont(fsize, 'B');
  Texto := 'TOTAL R$';
  PDF.TextBox(0, y, meiaLargura, 3, Texto, 'T', 'L', 0, '', false);
  Texto := FormatFloatBr(ValorTotal, ',0.00');
  y := y + PDF.TextBox(meiaLargura, y, meiaLargura, 3, Texto, 'T', 'R', 0, '', false);

  //Salta Uma linha
  y := y + PDF.TextBox(0, y, Args.Band.Width, 3, '   ', 'T', 'R', 0, '', false);

end;

procedure TSATExtratoFPDF.GerarBlocoDadosCancelamento(Args: TFPDFBandDrawArgs);
begin

end;

procedure TSATExtratoFPDF.GerarBlocoFechamento(Args: TFPDFBandDrawArgs);
var
  Texto: String;
  y: Double;
  PDF: IFPDF;
begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;
  y := 0;

  if (FSATExtratoClassOwner.Sistema = '') and (FSATExtratoClassOwner.Site = '') then
    Exit;

  Texto := Trim(FSATExtratoClassOwner.Sistema);

  if (FSATExtratoClassOwner.Site <> '') then
  begin
    if (Texto <> '') then
      Texto := Texto + ' - ';

    Texto := Texto + FSATExtratoClassOwner.Site;
  end;
  y := y + PDF.TextBox(0, y, Args.Band.Width, 0, Texto, 'T', 'C', 0, '', true);

end;

constructor TSATExtratoFPDF.Create(ACFe: TCFe; PaiExtrato:
    TACBrSATExtratoReportClass);
var
  LFormatSettings: TFormatSettings;
begin
  inherited Create;
  FCancelamento := False;
  FCFe := ACFe;
  FCFeUtils := TCFeUtilsFPDF.Create(ACFe, PaiExtrato);
  FImageUtils := TImageUtils.Create;
  {$IFDEF HAS_FORMATSETTINGS}
    LFormatSettings := CreateFormatSettings;
  {$ENDIF}
  LFormatSettings.DecimalSeparator  := ',';
  LFormatSettings.ThousandSeparator := '.';
  FCFeUtils.FormatSettings := LFormatSettings;

  FFontFamily := 'Arial';
  FDashWidth := 1;
  FVia := 'Via Consumidor';
  SetFont('Times');

  EngineOptions.DoublePass := True;
  FPaperWidth := 80;
  FPaperHeight := 20;
  SetFont(FFontFamily);
  SetMargins(2, 2);
end;

destructor TSATExtratoFPDF.Destroy;
begin
  FCFeUtils.Free;
  FImageUtils.Free;
  inherited;
end;

procedure TSATExtratoFPDF.ImprimeDadosSATRodape(var x,y: Double; PDF: IFPDF; var Largura:Double);
var
  Texto: string;
begin
  PDF.SetFont(7, 'B');
  Texto := 'SAT No. ' + FormatFloatBr(CFe.ide.nserieSAT, '000,000,000');
  y := y + PDF.TextBox(x, y, Largura, 0, Texto, 'T', 'C', 0, '', true);
  PDF.SetFont(7, '');
  Texto := FormatDateTimeBr(CFe.ide.dEmi + CFe.ide.hEmi, 'DD/MM/YYYY - hh:nn:ss');
  y := y + PDF.TextBox(x, y, Largura, 0, Texto, 'T', 'C', 0, '', true);
end;

procedure TSATExtratoFPDF.CarregaLogoEmFLogo;
var
  LStream: TMemoryStream;
  LLogoStringStream: TStringStream;
begin
  LStream := TMemoryStream.Create;
  try
    if FileExists(FSATExtratoClassOwner.Logo) then
      LStream.LoadFromFile(FSATExtratoClassOwner.Logo)
    else
    begin
      LLogoStringStream := TStringStream.Create(FSATExtratoClassOwner.Logo);
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

procedure TSATExtratoFPDF.GerarLogo(const PDF: IFPDF; out xRs, wRs: double; out
    alignH: char; Args: TFPDFBandDrawArgs);
var
  nImgW: double;
  nImgH: double;
  xImg: double;
  yImg: double;
  logoWmm: double;
  logoHmm: double;
  MaxImgH: double;
  logoW: word;
  logoH: word;
  Stream: TMemoryStream;
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
    finally
      Stream.Free;
    end;
  end;
end;

function TSATExtratoFPDF.GetTextoBlocoCabecalho: string;
  function LinhaEnderecoEmissor: String;
  begin
    Result := Trim(CFe.Emit.EnderEmit.xLgr)+
              ifthen(Trim(CFe.Emit.EnderEmit.nro)<>'',', '+Trim(CFe.Emit.EnderEmit.nro),'') + ' ' +
              ifthen(Trim(CFe.Emit.EnderEmit.xCpl)<>'',Trim(CFe.Emit.EnderEmit.xCpl) + ' ','') +
              ifthen(Trim(CFe.Emit.EnderEmit.xBairro)<>'',Trim(CFe.Emit.EnderEmit.xBairro) + ' ','') +
              Trim(CFe.Emit.EnderEmit.xMun)+'-'+CUFtoUF(CFe.ide.cUF)+' '+
              FormatarCEP(CFe.Emit.EnderEmit.CEP);
  end;

begin
  Result := '';
  if (CFe.Emit.xFant <> '') then
    Result := CFe.Emit.xFant + sLineBreak;
  Result := Result +
    CFe.Emit.xNome + sLineBreak +
    LinhaEnderecoEmissor + sLineBreak +
    Format('CNPJ: %s    IE: %s', [FormatarCNPJouCPF(CFe.Emit.CNPJ), CFe.Emit.IE]);
end;

function TSATExtratoFPDF.GetTextoBlocoConsumidor: string;
begin
  Result := 'CPF/CNPJ do Consumidor: ';
  if CFe.Dest.CNPJCPF = '' then
  begin
    Result := Result+ 'CONSUMIDOR NÃO IDENTIFICADO';
    Exit;
  end;

  if Length(CFe.Dest.CNPJCPF) = 14 then
    Result := FormatarCNPJouCPF(CFe.Dest.CNPJCPF)
  else if Length(CFe.Dest.CNPJCPF) = 11 then
    Result := FormatarCNPJouCPF(CFe.Dest.CNPJCPF);
  if CFe.Dest.xNome <> '' then
    Result := Result + sLineBreak +'Razão Social/Nome: ' + CFe.Dest.xNome;
end;

procedure TSATExtratoFPDF.ImprimeChave(Args: TFPDFBandDrawArgs; var y: double);
var
  PDF: IFPDF;
  ChaveSat: string;
  ChaveEmUmaLinha : Boolean;
  bW, bH: Double;
begin
  Args.Band.AutoHeight := True;
  PDF := Args.PDF;
  PDF.SetFont(7, 'B');

  ChaveSat := FormatarChaveAcesso(CFe.infCFe.ID);
  y := y + PDF.TextBox(0, y, Args.Band.Width, 3, ChaveSat, 'T', 'C', 0, '', true);
  y := y + 2;

  bW := 75;
  bH := 12;
  //CódigoBarrasCode128
  ChaveEmUmaLinha := True;
  if not ChaveEmUmaLinha then
  begin
    PDF.Code128(copy(CFe.infCFe.ID,1,22), 0, y, bH, bW);
    y := y+bH;
    PDF.Code128(copy(CFe.infCFe.ID,23,22), 0, y, bH, bW);
    y := y+bH;
  end
  else
  begin
    PDF.Code128(CFe.infCFe.ID, 0, y, bH, bW);
    y := y+bH;
  end;
end;

procedure TSATExtratoFPDF.OnStartReport(Args: TFPDFReportEventArgs);
var
  Page: TFPDFPage;
begin
  if not FInitialized then
  begin
    if FCFe = nil then
      raise EACBrException.Create('FACBrNFCe not initialized');

    if FPaperHeight > FPaperWidth  then
      Page := AddPage(poPortrait, puMM, FPaperWidth, FPaperHeight)
    else
      Page := AddPage(poLandscape, puMM, FPaperWidth, FPaperHeight);
    Page.EndlessHeight := True;

    if FSATExtratoClassOwner.Logo <> '' then
    begin
      CarregaLogoEmFLogo;
    end;

    AddBand(btData, 10, GerarBlocoCabecalho);

    if not QRCodeLateral then
      AddBand(btData, 10, BlocoConsumidorSimples);

    if FCFeUtils.SATExtratoClassOwner.LayOut <> lResumido then
      AddBand(btData, 10, GerarBlocoItens);

    AddBand(btData, 16, GerarBlocoTotais);
    AddBand(btData, 10, GerarBlocoPagamentos);
    AddBand(btData, 10, GerarBlocoObsFisco);
    AddBand(btData, 10, GerarBlocoDadosEntrega);
    AddBand(btData, 10, GerarBlocoObsContribuinte);
//Termina Corpo Extrato
    AddBand(btData, 10, GerarBlocoRodape);
    AddBand(btData, 10, GerarBlocoFechamento);

    FInitialized := True;
  end;
end;

constructor TACBrSATExtratoFPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TACBrSATExtratoFPDF.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrSATExtratoFPDF.Imprimir;
var
  ExtratoFPDF: TSATExtratoFPDF;
  Engine: TFPDFEngine;
  LPath : String;
begin
  ExtratoFPDF := TSATExtratoFPDF.Create(CFe, Self);
  try
    ExtratoFPDF.FSATExtratoClassOwner := TACBrSAT(ACBrSAT).Extrato;

    Engine := TFPDFEngine.Create(ExtratoFPDF, False);
    try
      Engine.Compressed := True;

      LPAth := IncludeTrailingPathDelimiter(TACBrSAT(ACBrSAT).Extrato.PathPDF) +
                                            ExtractFilePath(TACBrSAT(ACBrSAT).Extrato.NomeDocumento);

      Engine.SaveToFile(LPath + IntToStr(CFe.ide.nCFe) + '.pdf');
    finally
      Engine.Free;
    end;
  finally
    ExtratoFPDF.Free;
  end;
end;

procedure TACBrSATExtratoFPDF.ImprimirExtrato(AStream: TStream; ACFe: TCFe);
begin
  SetInternalCFe(ACFe);
  Imprimir;
end;

procedure TACBrSATExtratoFPDF.ImprimirExtrato(ACFe: TCFe);
begin
  SetInternalCFe(ACFe);
  Imprimir;
end;

procedure TACBrSATExtratoFPDF.ImprimirExtratoCancelamento(AStream: TStream;
  ACFe: TCFe; ACFeCanc: TCFeCanc);
begin
  SetInternalCFe(ACFe);
  Imprimir;
end;

procedure TACBrSATExtratoFPDF.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  SetInternalCFe(ACFe);
  Imprimir;
end;

procedure TACBrSATExtratoFPDF.ImprimirExtratoResumido(ACFe: TCFe);
begin
  SetInternalCFe(ACFe);
  Imprimir;
end;

procedure TACBrSATExtratoFPDF.ImprimirExtratoResumido(AStream: TStream;
  ACFe: TCFe);
begin
  SetInternalCFe(ACFe);
  Imprimir;
end;

end.
