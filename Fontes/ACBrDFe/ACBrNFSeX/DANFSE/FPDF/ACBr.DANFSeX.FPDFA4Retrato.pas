{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Arimatéia Jr.                                   }
{                              Elton Barbosa                                   }
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
unit ACBr.DANFSeX.FPDFA4Retrato;

{$I ACBr.inc}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  Math,
  StrUtilsEx,

  ACBr_fpdf,
  ACBr_fpdf_ext,
  ACBr_fpdf_report,

  ACBrUtil.Compatibilidade,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrValidador,
  ACBrDFeUtil,

  ACBrNFSeXClass,
  ACBr.DANFSeX.Classes;

type
  TACBrDANFSeFPDFA4Retrato = class(TFPDFReport)
  {$IFDEF SUPPORTS_STRICT}
    strict private
  {$ELSE}
    private
  {$ENDIF}
    FNFSe: TNFSe;
    FCancelada: boolean;
    FLogoPrefeitura: boolean;
    FLogoPrefeituraBytes: TBytes;
    FLogoPrestador: boolean;
    FLogoPrestadorBytes: TBytes;
    FHomologacao: boolean;
    FQRCode: boolean;
    FCabecalhoLinha1: string;
    FCabecalhoLinha2: string;
    FQuebraDeLinha: string;
    FMensagemRodape: string;
    FFormatSettings: TFormatSettings;
    FInitialized: boolean;
    FPage: TFPDFPage;
  private
    function GetTextoDiscriminacaoServicos: string;
    function GetTextoOutrasInformacoes: string;
    property NFSe: TNFSe read FNFSe;
    procedure InicializaValoresPadraoObjeto;

    function CalculateBlocoValoresH: double;
    function CalculateBlocoOutrasInformacoesH: double;

    procedure BlocoCabecalho(Args: TFPDFBandDrawArgs);
    procedure BlocoPrestador(Args: TFPDFBandDrawArgs);
    procedure BlocoTomador(Args: TFPDFBandDrawArgs);
    procedure BlocoDiscriminacaoServico(Args: TFPDFBandDrawArgs);
    procedure BlocoItens(Args: TFPDFBandDrawArgs);
    procedure BlocoValores(Args: TFPDFBandDrawArgs);
    procedure BlocoOutrasInformacoes(Args: TFPDFBandDrawArgs);
    procedure BlocoRodape(Args: TFPDFBandDrawArgs);
    procedure BlocoWatermark(Args: TFPDFBandDrawArgs);
  protected
    procedure OnStartReport(Args: TFPDFReportEventArgs); override;
  public
    constructor Create; reintroduce; overload;
    constructor Create(ANFSe: TNFSe); reintroduce; overload;

    procedure SalvarPDF(DadosAuxDANFSe: TDadosNecessariosParaDANFSeX; const NomeArquivoDestinoPDF: String); overload;
    procedure SalvarPDF(NFSe: TNFSe; DadosAuxDANFSe: TDadosNecessariosParaDANFSeX; const NomeArquivoDestinoPDF: String); overload;

    property Cancelada: boolean read FCancelada write FCancelada;
    property Homologacao: boolean read FHomologacao write FHomologacao;
    property LogoPrefeitura: boolean read FLogoPrefeitura write FLogoPrefeitura;
    property LogoPrefeituraBytes: TBytes read FLogoPrefeituraBytes write FLogoPrefeituraBytes;
    property LogoPrestador: boolean read FLogoPrestador write FLogoPrestador;
    property LogoPrestadorBytes: TBytes read FLogoPrestadorBytes write FLogoPrestadorBytes;
    property QRCode: boolean read FQRCode write FQRCode;
    property CabecalhoLinha1: string read FCabecalhoLinha1 write FCabecalhoLinha1;
    property CabecalhoLinha2: string read FCabecalhoLinha2 write FCabecalhoLinha2;
    property QuebraDeLinha: string read FQuebraDeLinha write FQuebraDeLinha;
    property MensagemRodape: string read FMensagemRodape write FMensagemRodape;
  end;

implementation

{ TNFSeDANFSeFPDF }

procedure TACBrDANFSeFPDFA4Retrato.BlocoCabecalho(Args: TFPDFBandDrawArgs);
var
  MaxW, w, h, h1, w1, w2: double;
  wLogo, wQRCode: double;
  wDadosNota: double;
  x1, y1: double;
  Texto: string;
  Linha1, Linha2: string;
  PDF: IFPDF;
  x, y: double;
  Stream: TBytesStream;
begin
  PDF := Args.PDF;
  x := 0;
  y := 0;

  MaxW := Args.Band.Width;
  h := Args.Band.Height;

  wLogo := IfThen(LogoPrefeitura, 20);
  wQRCode := IfThen(QRCode, 24);
  wDadosNota := 50;

  Linha1 := UpperCase(Trim(CabecalhoLinha1));
  if Linha1 = '' then
    Linha1 := 'PREFEITURA MUNICIPAL';
  Linha2 := UpperCase(Trim(CabecalhoLinha2));
  if Linha2 = '' then
    Linha2 := ' SECRETARIA MUNICIPAL DE FINANÇAS';
  Texto := Format('%s' + sLineBreak + '%s' + sLineBreak + '%s',
    [Linha1, Linha2, '  NOTA FISCAL DE SERVIÇOS ELETRÔNICA - NFSe']);

  w := MaxW - wQRCode - wDadosNota;
  // h := 30;
  PDF.TextBox(x, y, w, h);

  // Logomarca
  x1 := x;
  //y1 := y + 5;
  y1 := y + 3;
  if LogoPrefeitura then
  begin
    w1 := wLogo;
    h1 := wLogo;

    Stream := TBytesStream.Create(FLogoPrefeituraBytes);
    try
      PDF.Image(x1, y1, w1, h1, Stream, 'C', 'C');
    finally
      Stream.Free;
    end;

    x1 := x1 + w1;
    w1 := w - w1;
  end
  else
  begin
    w1 := w;
    h1 := 20;
  end;

  // Nome da Prefeitura
  PDF.SetFont(10, 'B');
  PDF.TextBox(x1, y1, w1, h1, Texto, 'C', 'C', False, False, True, 2);

  PDF.SetFont(8, '');
  PDF.TextBox(x1, y1 + h1 - 2, w1, 4,
    Format('RPS/SÉRIE: %s/%s (%s)',
      [NFSe.IdentificacaoRps.Numero, NFSe.IdentificacaoRps.Serie,
       FormatDateBr(NFSe.DataEmissaoRps)]),
    'T', 'C', False, False, True);

  // Dados da nota
  x1 := x1 + w1;
  w2 := wDadosNota;
  PDF.TextBox(x1, y, w2, h);

  h1 := h / 3;
  w1 := wDadosNota;

  y1 := y;
  PDF.Rect(x1, y1, w1, h1);
  PDF.SetFont(6, '');
  PDF.TextBox(x1, y1, w1, h1, 'NÚMERO DA NOTA', 'T', 'L', False);
  PDF.SetFont(10, 'B');
  Texto := NFSe.Numero;
  if Length(Texto) < 8 then
    Texto := PadLeft(NFSe.Numero, 8, '0');
  PDF.TextBox(x1, y1, w1, h1, Texto, 'B', 'C', False, False, True);

  y1 := y1 + h1;
  PDF.Rect(x1, y1, w1, h1);
  PDF.SetFont(6, '');
  PDF.TextBox(x1, y1, w1, h1, 'DATA/HORA DE EMISSÃO', 'T', 'L', False);
  PDF.SetFont(10, 'B');
  PDF.TextBox(x1, y1, w1, h1, FormatDateTimeBr(NFSe.DataEmissao), 'B', 'C', False, False, True);

  y1 := y1 + h1;
  PDF.Rect(x1, y1, w1, h1);
  PDF.SetFont(6, '');
  PDF.TextBox(x1, y1, w1, h1, 'CÓDIGO DE VERIFICAÇÃO', 'T', 'L', False);
  PDF.SetFont(10, 'B');
  PDF.TextBox(x1, y1, w1, h1, NFSe.CodigoVerificacao, 'B', 'C', False, False, True);

  // QR Code
  if QRCode then
  begin
    x1 := x1 + w2;
    w2 := wQRCode;
    PDF.TextBox(x1, y, w2, h);

    w1 := wQRCode;

    y1 := y + 1;
//    PDF.TextBox(x1, y1, w1, h1);

    PDF.SetFillColor(0, 0, 0);
    PDF.QRCode(x1 + 2, y1 + 2, w1 - 4, NFSe.Link);
  end;
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoRodape(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  x, y, w, h: double;
  Mensagens: TStringArray;
begin
  PDF := Args.PDF;
  if FMensagemRodape = '' then
    Exit;
  Mensagens := ACBr_fpdf.Split(FMensagemRodape, '|');

  x := 0;
  y := 0;
  w := Args.Band.Width;
  h := Args.Band.Height;

  PDF.SetFont(6, 'I');
  if Length(Mensagens) >= 1 then
    PDF.TextBox(x, y, w, h, Mensagens[0], 'T', 'L', 0);
  if Length(Mensagens) >= 2 then
    PDF.TextBox(x, y, w, h, Mensagens[1], 'T', 'C', 0);
  if Length(Mensagens) >= 3 then
    PDF.TextBox(x, y, w, h, Mensagens[2], 'T', 'R', 0);
end;

function TACBrDANFSeFPDFA4Retrato.CalculateBlocoOutrasInformacoesH: double;
var
  PageSize: TFPDFPageSize;
  PDF: TFPDFExt;
  w, h: double;
begin
  Result := 30;

  PageSize.w := FPage.PageWidth;
  PageSize.h := FPage.PageHeight;
  PDF := TFPDFExt.Create(FPage.Orientation, FPage.PageUnit, PageSize);
  try
    w := FPage.PageWidth - FPage.LeftMargin - FPage.RightMargin;
    // w := Args.Band.Width - 4;
    PDF.SetFont('Times', '', 8);
    h := PDF.GetStringHeight(GetTextoOutrasInformacoes, w);

    if h > Result - 5 then
      Result := h + 5;
  finally
    PDF.Free;
  end;
end;

function TACBrDANFSeFPDFA4Retrato.CalculateBlocoValoresH: double;
var
  PageSize: TFPDFPageSize;
  PDF: TFPDFExt;
  w, h: double;
  Texto: string;
begin
  Result := 30;

  PageSize.w := FPage.PageWidth;
  PageSize.h := FPage.PageHeight;
  PDF := TFPDFExt.Create(FPage.Orientation, FPage.PageUnit, PageSize);
  try
    w := FPage.PageWidth - FPage.LeftMargin - FPage.RightMargin;
    PDF.SetFont('Times', '', 6);
    h := PDF.GetStringHeight('CÓDIGO DE CLASSIFICAÇÃO DO SERVIÇO', w);
    PDF.SetFont('Times', 'B', 10);
    Texto := Format('%s - %s', [NFSe.Servico.ItemListaServico, NFSe.Servico.xItemListaServico]);
    h := h + PDF.GetStringHeight(Texto, w);
    if h < 8 then
      h := 8;
    Result := Result + h;
  finally
    PDF.Free;
  end;
end;

procedure TACBrDANFSeFPDFA4Retrato.InicializaValoresPadraoObjeto;
begin
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.DecimalSeparator := ',';
  FFormatSettings.ThousandSeparator := '.';
  SetFont('Times');
  SetMargins(8, 10, 8, 2);
end;

constructor TACBrDANFSeFPDFA4Retrato.Create;
begin
  inherited Create;
  InicializaValoresPadraoObjeto;
end;

constructor TACBrDANFSeFPDFA4Retrato.Create(ANFSe: TNFSe);
begin
  inherited Create;
  InicializaValoresPadraoObjeto;

  FNFSe := ANFSe;
end;

function TACBrDANFSeFPDFA4Retrato.GetTextoDiscriminacaoServicos: string;
begin
  Result := Trim(NFSe.Servico.Discriminacao);
  if Result = '' then
    Exit;

  Result := StringReplace(Result, sLineBreak, #10, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, #10, sLineBreak, [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, FQuebradeLinha, sLineBreak,
    [rfReplaceAll, rfIgnoreCase]);
end;

function TACBrDANFSeFPDFA4Retrato.GetTextoOutrasInformacoes: string;
begin
  Result := Trim(NFSe.OutrasInformacoes);
  if Result = '' then
    Exit;

  Result := StringReplace(Result, sLineBreak, #10, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, #10, sLineBreak, [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, FQuebradeLinha, sLineBreak,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TACBrDANFSeFPDFA4Retrato.OnStartReport(Args: TFPDFReportEventArgs);
begin
  if not FInitialized then
  begin
    if FNFSe = nil then
      raise Exception.Create('FNFSe not initialized');

    FPage := AddPage;

    AddBand(btPageHeader, 26, BlocoCabecalho);
    AddBand(btPageHeader, 25, BlocoPrestador);
    AddBand(btPageHeader, 25, BlocoTomador);
    if NFSe.Servico.ItemServico.Count = 0 then
      AddBand(btData, 10, BlocoDiscriminacaoServico)
    else
      AddBand(btData, 10, BlocoItens);
    AddBand(btPageFooter, CalculateBlocoValoresH, BlocoValores);
    AddBand(btPageFooter, CalculateBlocoOutrasInformacoesH, BlocoOutrasInformacoes);
    AddBand(btPageFooter, 5, BlocoRodape);
    AddBand(btOverlay, 10, BlocoWatermark);

    FInitialized := True;
  end;
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoDiscriminacaoServico(Args: TFPDFBandDrawArgs);
var
  MaxW, w, h, w1: double;
  x1, y1: double;
  Texto: string;
  PDF: IFPDF;
  x, y: double;
begin
  PDF := Args.PDF;
  x := 0;
  y := 0;

  Args.Band.Height := Args.FreeSpace - Args.ReservedSpace;

  MaxW := Args.Band.Width;
  y1 := y;
  w := MaxW;
  h := Args.Band.Height;
  PDF.TextBox(x, y, w, h);

  PDF.SetFont(8, 'B');
  PDF.TextBox(x, y1, w, 4, 'DISCRIMINAÇÃO DOS SERVIÇOS', 'T', 'C', True, False, True);
  x1 := x + 2;
  y1 := y1 + 5;
  w1 := w - 4;

  Texto := GetTextoDiscriminacaoServicos;
  PDF.SetFont(8, '');
  h := PDF.GetStringHeight(Texto, w1);
  PDF.TextBox(x1, y1, w1, h, Texto, 'T', 'L', False);
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoItens(Args: TFPDFBandDrawArgs);
var
  MaxW, w, h, w1, w2, w3, w4, IncY: double;
  x1, y1: double;
  I: integer;
  Item: TItemServicoCollectionItem;
  PDF: IFPDF;
  x, y: double;
begin
  PDF := Args.PDF;
  x := 0;
  y := 0;

  MaxW := Args.Band.Width;
  y1 := y;
  w := MaxW;

  Args.Band.Height := Args.FreeSpace - Args.ReservedSpace;

  h := Args.Band.Height;
  PDF.TextBox(x, y, w, h);

  PDF.SetFont(8, 'B');
  PDF.TextBox(x, y1, w, 4, 'DISCRIMINAÇÃO DOS SERVIÇOS', 'T', 'C', True, False, True);
  x1 := x;
  y1 := y1 + 4;

  //##################################################################################
  h := 4;
  PDF.SetFont(6, '');
  w2 := RoundTo(w*0.08, 0);
  w3 := RoundTo(w*0.15, 0);
  w4 := RoundTo(w*0.15, 0);
  w1 := w - w2 - w3 - w4;
  // ITEM
  PDF.TextBox(x1, y1, w1, h + 2, 'ITEM', 'C', 'L', 0, '', false);
  PDF.Line(x1 + w1, y1, x1 + w1, y1 + Args.Band.Height - 4);

  // QUANTIDADE
  x1 := x1 + w1;
  PDF.TextBox(x1, y1, w2, h + 2, 'QTDE.', 'C', 'C', 0, '', false);
  PDF.Line(x1 + w2, y1, x1 + w2, y1 + Args.Band.Height - 4);

  // VALOR UNITÁRIO
  x1 := x1 + w2;
  PDF.TextBox(x1, y1, w3, h + 2, 'VALOR UNITÁRIO (R$)', 'C', 'C', 0, '', false);
  PDF.Line(x1 + w3, y1, x1 + w3, y1 + Args.Band.Height - 4);

  // VALOR TOTAL
  x1 := x1 + w3;
  PDF.TextBox(x1, y1, w4, h + 2, 'VALOR UNITÁRIO (R$)', 'C', 'C', 0, '', false);
  // PDF.Line(x1 + w4, y1, x1 + w4, y1 + Args.Band.Height - 4);

  x1 := x;
  y1 := y1 + 4;
  PDF.SetFont(7, '');

  PDF.Line(x, y1, x + w, y1);

  for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Item := NFSe.Servico.ItemServico[I];

    IncY := PDF.TextBox(x1, y1, w1, h + 2, Item.Descricao, 'T', 'L', False);

    x1 := x1 + w1;

    PDF.TextBox(x1, y1, w2, h + 2,
      FormatFloat('#,0.00', Item.Quantidade, FFormatSettings), 'T', 'R', 0, '', false);

    x1 := x1 + w2;

    PDF.TextBox(x1, y1, w3, h + 2,
      FormatFloat('#,0.00', Item.ValorUnitario, FFormatSettings), 'T', 'R', 0, '', false);

    x1 := x1 + w3;

    PDF.TextBox(x1, y1, w4, h + 2,
      FormatFloat('#,0.00', Item.ValorTotal, FFormatSettings), 'T', 'R', 0, '', false);

    x1 := x;
    if IncY < 4 then
      IncY := 4;
    y1 := y1 + IncY + 1;

    if y1 > Args.Band.Height - IncY then
      Break;

    PDF.DashedLine(x, y1, x + w, y1);
  end;
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoOutrasInformacoes(Args: TFPDFBandDrawArgs);
var
  MaxW, w, h, w1: double;
  x1, y1: double;
  Texto: string;
  PDF: IFPDF;
  x, y: double;
begin
  PDF := Args.PDF;
  x := 0;
  y := 0;

  MaxW := Args.Band.Width;
  y1 := y;
  w := MaxW;
  h := Args.Band.Height;
  PDF.Rect(x, y, w, h);

  PDF.SetFont(8, 'B');
  PDF.TextBox(x, y1, w, 4, 'OUTRAS INFORMAÇÕES', 'T', 'C', True, False, True);
  x1 := x + 2;
  y1 := y1 + 5;
  w1 := w - 4;

  Texto := GetTextoOutrasInformacoes;
  PDF.SetFont(8, '');
  h := PDF.GetStringHeight(Texto, w1);
  PDF.TextBox(x1, y1, w1, h, Texto, 'T', 'L', False);
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoPrestador(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  x, y: double;
  OldX: double;
  MaxW, w, h, h1, w1, IncY: double;
  wLogo: double;
  x1, x2, y1: double;
  Texto: string;

  function CampoValor(const Campo, Valor: string): double;
  begin
    PDF.SetFont(8, '');
    PDF.TextBox(x1, y1, w1, 4, Campo, 'B', 'L', False, False, True);
    x2 := x1 + PDF.GetStringWidth(Campo) + 1;
    PDF.SetFont(9, 'B');
    PDF.TextBox(x2, y1, w1, 4, Valor, 'B', 'L', False, False, True);
    Result := x2 - x1 + PDF.GetStringWidth(Valor);
  end;

var
  Stream: TBytesStream;
begin
  PDF := Args.PDF;
  x := 0;
  y := 0;

  MaxW := Args.Band.Width;
  y1 := y;
  w := MaxW;
  h := Args.Band.Height;
  PDF.Rect(x, y, w, h);

  PDF.SetFont(8, 'B');
  PDF.TextBox(x, y1, w, 4, 'PRESTADOR DE SERVIÇOS', 'T', 'C', False, False, True);
  y1 := y1 + 4;

  // Logomarca
  wLogo := IfThen(LogoPrestador, 20);
  w := MaxW - wLogo;
  x1 := x;
  if LogoPrestador then
  begin
    w1 := wLogo;
    h1 := wLogo;

    //PDF.TextBox(x1, y1, w1, h1);
    Stream := TBytesStream.Create(LogoPrestadorBytes);
    try
      PDF.Image(x1 + 2, y1 + 2, w1 - 4, h1 - 2, Stream, 'C', 'C');
    finally
      Stream.Free;
    end;

    x1 := x1 + w1;
    w1 := MaxW - w1;
  end
  else
  begin
    w1 := w;
  end;

  // Dados do prestador
  // PDF.TextBox(x1, y1, w1, h1);

  IncY := 5;
  x1 := x1 + 2;
  y1 := y1 + 1;

  // CPF / CNPJ
  OldX := x1;
  x1 := x1 + 10 + CampoValor('CPF / CNPJ:', FormatarCNPJouCPF(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj));
  x1 := x1 + 10 + CampoValor('INSC. MUNICIPAL:', NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal);
  x1 := OldX;
  y1 := y1 + IncY;

  // NOME / RAZÃO SOCIAL
  CampoValor('NOME / RAZÃO SOCIAL:', NFSe.Prestador.RazaoSocial);
  y1 := y1 + IncY;

  // ENDEREÇO
  Texto := NFSe.Prestador.Endereco.Endereco +
    IfThen(NFSe.Prestador.Endereco.Numero <> '', ', ' + NFSe.Prestador.Endereco.Numero) +
    ' - ' + NFSe.Prestador.Endereco.Bairro +
    ' - CEP ' + FormatarCEP(NFSe.Prestador.Endereco.CEP);
  CampoValor('ENDEREÇO:', Texto);
  y1 := y1 + IncY;

  // MUNICÍPIO
  x1 := x1 + 10 + CampoValor('MUNICÍPIO:', IfThen(NFSe.Prestador.Endereco.xMunicipio <> '',
    UpperCase(NFSe.Prestador.Endereco.xMunicipio), NFSe.Prestador.Endereco.CodigoMunicipio) +
    ' / ' + NFSe.Prestador.Endereco.UF);
  x1 := x1 + 10 + CampoValor('EMAIL:', LowerCase(NFSe.Prestador.Contato.Email));
  x1 := x1 + 10 + CampoValor('TELEFONE:', NFSe.Prestador.Contato.Telefone);
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoTomador(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  x, y: double;
  OldX: double;
  MaxW, w, h, w1, IncY: double;
  x1, x2, y1: double;
  Texto: string;

  function CampoValor(const Campo, Valor: string): double;
  begin
    PDF.SetFont(8, '');
    PDF.TextBox(x1, y1, w1, 4, Campo, 'B', 'L', False, False, True);
    x2 := x1 + PDF.GetStringWidth(Campo) + 1;
    PDF.SetFont(9, 'B');
    PDF.TextBox(x2, y1, w1, 4, Valor, 'B', 'L', False, False, True);
    Result := x2 - x1 + PDF.GetStringWidth(Valor);
  end;

begin
  PDF := Args.PDF;
  x := 0;
  y := 0;

  MaxW := Args.Band.Width;
  y1 := y;
  w := MaxW;
  h := Args.Band.Height;
  PDF.Rect(x, y, w, h);

  PDF.SetFont(8, 'B');
  PDF.TextBox(x, y1, w, 4, 'TOMADOR DE SERVIÇOS', 'T', 'C', False, False, True);
  x1 := x;
  y1 := y1 + 4;
  w1 := w;

  // Dados do tomador
  // PDF.TextBox(x1, y1, w1, h1);

  IncY := 5;
  x1 := x1 + 2;
  y1 := y1 + 1;

  // CPF / CNPJ
  OldX := x1;
  x1 := x1 + 10 + CampoValor('CPF / CNPJ:', FormatarCNPJouCPF(NFSe.Tomador.IdentificacaoTomador.CpfCnpj));
  x1 := x1 + 10 + CampoValor('INSC. MUNICIPAL:', NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal);
  x1 := x1 + 10 + CampoValor('INSC. ESTADUAL:', NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual);
  x1 := OldX;
  y1 := y1 + IncY;

  // NOME / RAZÃO SOCIAL
  CampoValor('NOME / RAZÃO SOCIAL:', NFSe.Tomador.RazaoSocial);
  y1 := y1 + IncY;

  // ENDEREÇO
  Texto := NFSe.Tomador.Endereco.Endereco +
    IfThen(NFSe.Tomador.Endereco.Numero <> '', ', ' + NFSe.Tomador.Endereco.Numero) +
    ' - ' + NFSe.Tomador.Endereco.Bairro +
    ' - CEP ' + FormatarCEP(NFSe.Tomador.Endereco.CEP);// +
  CampoValor('ENDEREÇO:', Texto);
  y1 := y1 + IncY;

  // MUNICÍPIO
  x1 := x1 + 10 + CampoValor('MUNICÍPIO:', IfThen(NFSe.Tomador.Endereco.xMunicipio <> '',
    UpperCase(NFSe.Tomador.Endereco.xMunicipio), NFSe.Tomador.Endereco.CodigoMunicipio) +
    ' / ' + NFSe.Tomador.Endereco.UF);
  x1 := x1 + 10 + CampoValor('EMAIL:', LowerCase(NFSe.Tomador.Contato.Email));
  x1 := x1 + 10 + CampoValor('TELEFONE:', NFSe.Tomador.Contato.Telefone);
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoValores(Args: TFPDFBandDrawArgs);
var
  PDF: IFPDF;
  x, y: double;
  MaxW, w, h, h1, w1: double;
  x1, y1: double;
  Texto: string;
  Servico: TDadosServico;

  procedure AdicionarImposto(Campo: string; Valor: double);
  begin
    PDF.Rect(x1, y1, w1, h1);
    PDF.SetFont(6, '');
    PDF.TextBox(x1, y1, w1, h1, Campo, 'T', 'C', False);
    PDF.SetFont(10, 'B');
    PDF.TextBox(x1, y1, w1, h1, FormatFloat('#,0.00', Valor, FFormatSettings), 'B', 'R',
      False, False, True);
    x1 := x1 + w1;
  end;

begin
  PDF := Args.PDF;
  x := 0;
  y := 0;

  MaxW := Args.Band.Width;
  y1 := y;
  w := MaxW;
  h := Args.Band.Height;
//  PDF.SetDrawColor(cRed);
  PDF.TextBox(x, y, w, h);
  PDF.SetDrawColor(cBlack);

  Servico := NFSe.Servico;

  Texto := 'VALOR TOTAL DA NOTA = R$ ' +
    FormatFloat('#,0.00', Servico.Valores.ValorServicos, FFormatSettings);
  PDF.SetFont(12, 'B');
  PDF.TextBox(x, y1, w, 6, Texto, 'C', 'C', True, False, True);

  x1 := x;
  y1 := y1 + 6;
  w1 := w;

  PDF.SetFont(6, '');
  Texto := 'CÓDIGO DE CLASSIFICAÇÃO DO SERVIÇO';
  h1 := PDF.GetStringHeight(Texto, w1);
  PDF.SetFont(10, 'B');
  Texto := Format('%s - %s', [Servico.ItemListaServico, Servico.xItemListaServico]);
  h1 := h1 + PDF.GetStringHeight(Texto, w1);
  if h1 < 8 then
    h1 := 8;

  PDF.Rect(x1, y1, w1, h1);
  PDF.SetFont(6, '');
  PDF.TextBox(x1, y1, w1, h1, 'CÓDIGO DE CLASSIFICAÇÃO DO SERVIÇO', 'T', 'L', False);
  PDF.SetFont(10, 'B');
  PDF.TextBox(x1, y1, w1, h1, Texto, 'B', 'L', False, True);

  x1 := x;
  y1 := y1 + h1;
  w1 := w / 5;
  h1 := 8;

  AdicionarImposto('INSS (R$)', Servico.Valores.ValorInss);
  AdicionarImposto('IRRF (R$)', Servico.Valores.ValorIr);
  AdicionarImposto('CSLL (R$)', Servico.Valores.ValorCsll);
  AdicionarImposto('COFINS (R$)', Servico.Valores.ValorCofins);
  AdicionarImposto('PIS (R$)', Servico.Valores.ValorPis);

  x1 := x;
  y1 := y1 + h1;
  w1 := w / 4;
  h1 := 8;

  AdicionarImposto('DEDUÇÃO (R$)', Servico.Valores.ValorDeducoes);
  AdicionarImposto('DESCONTO INCONDICIONADO (R$)', Servico.Valores.DescontoIncondicionado);
  AdicionarImposto('DESCONTO CONDICIONADO (R$)', Servico.Valores.DescontoCondicionado);
  AdicionarImposto('OUTRAS RETENÇÕES (R$)', Servico.Valores.OutrasRetencoes);

  x1 := x;
  y1 := y1 + h1;
  w1 := w / 5;
  h1 := 8;

  AdicionarImposto('TOTAL LÍQUIDO DA NOTA (R$)', Servico.Valores.ValorLiquidoNfse);
  AdicionarImposto('BASE DE CÁLCULO ISSQN (R$)', Servico.Valores.BaseCalculo);

  if (Servico.Valores.Aliquota = 0) and (Servico.ItemServico.Count > 0) and
    (Servico.ItemServico[0].AliqISSST > 0) then
    AdicionarImposto('ALÍQUOTA ISSQN (%)', Servico.ItemServico[0].AliqISSST)
  else
    AdicionarImposto('ALÍQUOTA ISSQN (%)', Servico.Valores.Aliquota);
  AdicionarImposto('VALOR DO ISSQN (R$)', Servico.Valores.ValorIss);
  AdicionarImposto('VALOR DO ISSQN RETIDO (R$)', Servico.Valores.ValorIssRetido);


//  Texto := GetTextoDiscriminacaoServicos;
//  PDF.SetFont(8, '');
//  h := PDF.GetStringHeight(Texto, w1);
//  PDF.TextBox(x1, y1, w1, h, Texto, 'T', 'L', True, True);
end;

procedure TACBrDANFSeFPDFA4Retrato.BlocoWatermark(Args: TFPDFBandDrawArgs);
var
  Texto: string;
  PDF: IFPDF;
  PreviousTextColor: string;
begin
  PDF := Args.PDF;
  Texto := '';

  if Cancelada or Homologacao then
  begin
    Texto := 'SEM VALOR FISCAL';

    if Cancelada then
      Texto := Texto + IfThen(Texto <> '', sLineBreak) +
        'NOTA CANCELADA';

    if Homologacao then
      Texto := Texto + IfThen(Texto <> '', sLineBreak) +
        'AMBIENTE DE HOMOLOGAÇÃO';
  end;

  if Texto <> '' then
  begin
    PreviousTextColor := Args.PDF.TextColor;
    try
      PDF.SetTextColor(200, 200, 200);
      PDF.SetFont(48, 'B');
      PDF.TextBox(
        Args.LeftMargin + 2, Args.TopMargin,
        Args.Band.Width - 4, Args.Band.Height,
        Texto, 'C', 'C', False, False, True);
    finally
      Args.PDF.TextColor := PreviousTextColor;
    end;
  end;
end;

procedure TACBrDANFSeFPDFA4Retrato.SalvarPDF(DadosAuxDANFSe: TDadosNecessariosParaDANFSeX;
  const NomeArquivoDestinoPDF: String);
var
  Engine: TFPDFEngine;
begin
  Engine := TFPDFEngine.Create(Self, False);
  try
    Engine.SaveToFile(NomeArquivoDestinoPDF);
  finally
    Engine.Free;
  end;

end;

procedure TACBrDANFSeFPDFA4Retrato.SalvarPDF(NFSe: TNFSe; DadosAuxDANFSe: TDadosNecessariosParaDANFSeX;
  const NomeArquivoDestinoPDF: String);
var
  Engine: TFPDFEngine;
begin
  Self.FNFSe := NFSe;

  SalvarPDF(DadosAuxDANFSe, NomeArquivoDestinoPDF);
end;


end.
