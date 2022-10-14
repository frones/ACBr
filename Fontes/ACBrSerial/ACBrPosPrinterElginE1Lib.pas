{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

// https://elgindevelopercommunity.github.io/

{$I ACBr.inc}

unit ACBrPosPrinterElginE1Lib;

interface

uses
  Classes, SysUtils,
  FMX.Graphics,
  ACBrConsts, ACBrDevice, ACBrBase, ACBrPosPrinter,
  Elgin.JNI.E1;

resourcestring
  cErroImpressoraSemPapapel = 'Impressora sem Papel';
  cErroImpressoraNaoPronta = 'Impressora não pronta';
  cErroImpressora = 'Erro na Impressora';

const
  cTagBR = '<br>';

  // Essas Tags serão interpretadas no momento da Impressão, pois usam comandos
  // específicos da Biblioteca E1
  CTAGS_POST_PROCESS: array[0..26] of string = (
    cTagPulodeLinha, cTagPuloDeLinhas, cTagBR,
    cTagLigaExpandido, cTagDesligaExpandido,
    cTagLigaAlturaDupla, cTagDesligaAlturaDupla,
    cTagLigaNegrito, cTagDesligaNegrito,
    cTagLigaSublinhado, cTagDesligaSublinhado,
    cTagLigaCondensado, cTagDesligaCondensado,
    cTagLigaItalico, cTagDesligaItalico,
    cTagLigaInvertido, cTagDesligaInvertido,
    cTagFonteNormal,
    cTagFonteA, cTagFonteB,
    cTagFonteAlinhadaDireita, cTagFonteAlinhadaEsquerda, cTagfonteAlinhadaCentro,
    cTagBeep,
    cTagZera, cTagReset,
    cTagCorte );

  CBLOCK_POST_PROCESS: array[0..16] of string = (
    cTagBarraEAN8, cTagBarraEAN13, cTagBarraInter,
    cTagBarraCode39, cTagBarraCode93,
    cTagBarraCode128,
    cTagBarraUPCA, cTagBarraUPCE, cTagBarraCodaBar,
    cTagBMP,
    cTagBarraMostrar, cTagBarraLargura, cTagBarraAltura,
    cTagQRCode, cTagQRCodeTipo, cTagQRCodeLargura, cTagQRCodeError );

  CTAGS_CORTE: array[0..2] of string =
    (cTagCorte, cTagCorteParcial, cTagCorteTotal);

type

  TElginE1LibPrinters = (prnSmartPOS, prnM8);

  TE1LibPrinter = class
  private
    fEstiloFonte: TACBrPosFonte;
    fAlinhamento: TACBrPosTipoAlinhamento;
    fImprimindo: Boolean;
    fEspacoLinha: Integer;
    fTamanhoTexto: Integer;
    fModelo: TElginE1LibPrinters;

    procedure SetEspacoLinha(const Value: Integer);
    function GetTermicaClass: JTermicaClass;
    procedure VerificarErro(CodErro: Integer; Metodo: String);
    function AlinhamentoToPosicao: Integer;
    procedure AjustarAlinhamento;
  public
    constructor Create;
    procedure Restaurar;
    property Imprimindo: Boolean read fImprimindo;
    procedure IniciarImpressao;
    procedure FinalizarImpressao;
    procedure PularLinhas(Linhas: Integer);
    procedure CortarPapel(Linhas: Integer);
    procedure ImprimirTexto(Texto: String);
    procedure ImprimirImagem(BitMap: TBitmap);
    procedure ImprimirCodBarras(Tipo: Integer; Conteudo: String;
      Altura, Largura: Integer; hri: Boolean);
    procedure ImprimirQRCode(Conteudo: String; LarguraModulo: Integer;
      nivelCorrecao: Integer= 2);
    function Status(Sensor: Integer): Integer;
    property TermicaClass: JTermicaClass read GetTermicaClass;
    property Modelo: TElginE1LibPrinters read fModelo write fModelo;
    property EstiloFonte: TACBrPosFonte read fEstiloFonte write fEstiloFonte
      default [];
    property Alinhamento: TACBrPosTipoAlinhamento read fAlinhamento write fAlinhamento
      default TACBrPosTipoAlinhamento.alEsquerda;
    property EspacoLinha: Integer read fEspacoLinha write SetEspacoLinha
      default 10;
    property TamanhoTexto: Integer read fTamanhoTexto write fTamanhoTexto
      default 0;
  end;
  { TACBrPosPrinterElginE1Lib }

  TACBrPosPrinterElginE1Lib = class(TACBrPosPrinterClass)
  private
    fE1LibPrinter: TE1LibPrinter;
    fE1LibTagProcessor: TACBrTagProcessor;

    procedure E1LibTraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString);
    procedure E1LibAdicionarBlocoResposta(const ConteudoBloco: AnsiString);
    procedure E1LibTraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString);

    procedure ProcessarComandoBMP(ConteudoBloco: AnsiString);
    procedure ProcessarQRCode(ConteudoBloco: AnsiString);
    procedure ProcessarCodBarras(ConteudoBloco: AnsiString; ATag: String);

    function GetModelo: TElginE1LibPrinters;
    procedure SetModelo(const Value: TElginE1LibPrinters);
  protected
    procedure ImprimirE1Lib(const LinhasImpressao: String; var Tratado: Boolean);

  public
    constructor Create(AOwner: TACBrPosPrinter);
    destructor Destroy; override;

    procedure Configurar; override;
    function TraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString): Boolean;
      override;
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString): Boolean; override;
    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;

    property Modelo: TElginE1LibPrinters read GetModelo write SetModelo;
  end;

implementation

uses
  StrUtils, Math,
  Androidapi.Helpers,
  ACBrPosPrinterAndroidHelper,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrImage;

{ TE1LibPrinter }

constructor TE1LibPrinter.Create;
begin
  fImprimindo := False;
  fModelo := TElginE1LibPrinters.prnM8;
  Restaurar;
  TermicaClass.setContext( TAndroidHelper.Context );
end;

procedure TE1LibPrinter.Restaurar;
begin
  fEstiloFonte := [];
  fAlinhamento := TACBrPosTipoAlinhamento.alEsquerda;
  fEspacoLinha := 10;
  fTamanhoTexto := 0;
end;

procedure TE1LibPrinter.IniciarImpressao;
var
  TipoE1: Integer;
  ModeloE1, ConexaoE1: string;
begin
  if fImprimindo then
    Exit;

  TermicaClass.FechaConexaoImpressora();

  ConexaoE1 := 'USB';
  // Tipo: 1 	USB, 2 	RS232, 3 	TCP/IP, 4 	Bluetooth, 5 	SmartPOS, 6 Mini PDV M8
  case fModelo of
    TElginE1LibPrinters.prnSmartPOS:
    begin
      TipoE1 := 5;
      ModeloE1 := 'SmartPOS';
    end;
  else
    begin
      TipoE1 := 6;
      ModeloE1 := 'M8';
    end;
  end;

  VerificarErro( TermicaClass.AbreConexaoImpressora( TipoE1,
                                                     StringToJString(ModeloE1),
                                                     StringToJString(ConexaoE1),
                                                     0),
                 'AbreConexaoImpressora');
  fImprimindo := True;
end;

procedure TE1LibPrinter.FinalizarImpressao;
begin
  if not fImprimindo then
    Exit;

  try
    VerificarErro( TermicaClass.FechaConexaoImpressora, 'FechaConexaoImpressora' );
  finally
    fImprimindo := False;
  end;
end;

function TE1LibPrinter.GetTermicaClass: JTermicaClass;
begin
  Result := TJTermica.JavaClass;
end;

procedure TE1LibPrinter.VerificarErro(CodErro: Integer; Metodo: String);
begin
  if CodErro >= 0 then
    Exit;

  raise EPosPrinterException.CreateFmt('Error %d executando %s',[CodErro, Metodo]);
end;

procedure TE1LibPrinter.SetEspacoLinha(const Value: Integer);
begin
  fEspacoLinha := max(Value, 1);
end;

function TE1LibPrinter.AlinhamentoToPosicao: Integer;
begin
  case Alinhamento of
    alCentro: Result := 1;
    alDireita: Result := 2;
  else
    Result := 0;
  end;
end;

procedure TE1LibPrinter.AjustarAlinhamento;
begin
  TermicaClass.DefinePosicao(AlinhamentoToPosicao);
end;

procedure TE1LibPrinter.PularLinhas(Linhas: Integer);
begin
  Linhas := max(1, min(999, Linhas));
  VerificarErro( TermicaClass.AvancaPapel(Linhas),
                 'AvancaPapel' );
end;

procedure TE1LibPrinter.CortarPapel(Linhas: Integer);
begin
  if (fModelo = TElginE1LibPrinters.prnSmartPOS) then
    PularLinhas(Linhas)
  else
    VerificarErro( TermicaClass.Corte(Linhas),
                   'Corte' );
end;

procedure TE1LibPrinter.ImprimirTexto(Texto: String);
var
  stilo, tamanho: Integer;
begin
  if Texto.Trim.IsEmpty then
  begin
    PularLinhas(1);
    Exit;
  end;
  // Definindo Estilos
  stilo := 0; // Normal
  if (ftCondensado in EstiloFonte) or (ftFonteB in EstiloFonte) then
    Inc(stilo, 1);
  if (ftSublinhado in EstiloFonte) then
    Inc(stilo, 2);
  if (ftInvertido in EstiloFonte) then
    Inc(stilo, 4);
  if (ftNegrito in EstiloFonte) then
    Inc(stilo, 8);
  // Ajustando Largura e Altura do Texto
  tamanho := fTamanhoTexto;
  if (ftAlturaDupla in EstiloFonte) then
    Inc(tamanho, 2);
  if (ftExpandido in EstiloFonte) then
    Inc(tamanho, 16);
   VerificarErro( TermicaClass.ImpressaoTexto( StringToJString(Texto),
                                               AlinhamentoToPosicao,
                                               stilo,
                                               tamanho),
                  'ImpressaoTexto' );
   PularLinhas(1);
end;
procedure TE1LibPrinter.ImprimirCodBarras(Tipo: Integer; Conteudo: String;
  Altura, Largura: Integer; hri: Boolean);
begin
  // UPC_A = 0; UPC_E = 1; EAN_13 = 2; EAN_8 = 3; CODE_39 = 4; ITF = 5;
  // CODE_BAR = 6; CODE_93 = 7; CODE_128 = 8;
  // HRI: 1 - Acima do código, 2 - Abaixo do código, 3 - Ambos, 4 - Não impresso.
  if Conteudo.IsEmpty then
    Exit;
  AjustarAlinhamento;
  VerificarErro( TermicaClass.ImpressaoCodigoBarras( Tipo,
                                                     StringToJString(Conteudo),
                                                     Altura, Largura,
                                                     ifthen(hri, 2, 4) ),
                 'ImpressaoCodigoBarras');
end;

procedure TE1LibPrinter.ImprimirQRCode(Conteudo: String;
  LarguraModulo, nivelCorrecao: Integer);
begin
  if Conteudo.IsEmpty then
    Exit;
  AjustarAlinhamento;
  VerificarErro( TermicaClass.ImpressaoQRCode( StringToJString(Conteudo),
                                               LarguraModulo, nivelCorrecao),
                 'ImpressaoQRCode' );
end;

procedure TE1LibPrinter.ImprimirImagem(BitMap: TBitmap);
begin
  AjustarAlinhamento;
  VerificarErro( TermicaClass.ImprimeBitmap(BitmapToJBitmap(bitmap)),
                 'ImprimeBitmap' );
end;

function TE1LibPrinter.Status(Sensor: Integer): Integer;
var
  S: Integer;
begin
  Result := TermicaClass.StatusImpressora(Sensor);
end;

{ TACBrPosPrinterElginE1Lib }

constructor TACBrPosPrinterElginE1Lib.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'E1LibPrinter';

  TagsNaoSuportadas.Add( cTagLogotipo );
  TagsNaoSuportadas.Add( cTagLogoImprimir );
  TagsNaoSuportadas.Add( cTagBarraStd );
  TagsNaoSuportadas.Add( cTagBarraCode11 );
  TagsNaoSuportadas.Add( cTagBarraMSI );

  fE1LibTagProcessor := TACBrTagProcessor.Create;
  fE1LibTagProcessor.AddTags(CTAGS_POST_PROCESS, [],  False);
  fE1LibTagProcessor.AddTags(CBLOCK_POST_PROCESS, [], True);

  fE1LibTagProcessor.OnTraduzirTag := E1LibTraduzirTag;
  fE1LibTagProcessor.OnAdicionarBlocoResposta := E1LibAdicionarBlocoResposta;
  fE1LibTagProcessor.OnTraduzirTagBloco := E1LibTraduzirTagBloco;

  fE1LibPrinter := TE1LibPrinter.Create;
end;

destructor TACBrPosPrinterElginE1Lib.Destroy;
begin
  fE1LibTagProcessor.Free;
  fE1LibPrinter.FinalizarImpressao;
  fE1LibPrinter.Free;

  inherited;
end;

function TACBrPosPrinterElginE1Lib.GetModelo: TElginE1LibPrinters;
begin
  Result := fE1LibPrinter.Modelo;
end;

procedure TACBrPosPrinterElginE1Lib.SetModelo(const Value: TElginE1LibPrinters);
begin
  fE1LibPrinter.Modelo := Value;
end;

procedure TACBrPosPrinterElginE1Lib.Configurar;
begin
  fpPosPrinter.Porta := 'NULL';
  fpPosPrinter.OnEnviarStringDevice := ImprimirE1Lib;
  fpPosPrinter.PaginaDeCodigo := TACBrPosPaginaCodigo.pcUTF8;
  fE1LibPrinter.IniciarImpressao;
end;

function TACBrPosPrinterElginE1Lib.TraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString): Boolean;
begin
  TagTraduzida := '';
  Result := True;
  if MatchText(ATag, CTAGS_POST_PROCESS) then
    TagTraduzida := ATag
  else if MatchText(ATag, CTAGS_CORTE) then
    TagTraduzida := cTagCorte
  else if (ATag = cTagRetornoDeCarro) then
    TagTraduzida := cTagBR
  else
    Result := False;  // Deixa ACBrPosPrinter traduzir...
end;

function TACBrPosPrinterElginE1Lib.TraduzirTagBloco(const ATag,
  ConteudoBloco: AnsiString; var BlocoTraduzido: AnsiString): Boolean;

  function RemontaBloco(const ATag, ConteudoBloco: AnsiString): String;
  begin
    Result := ATag + ConteudoBloco + '</'+ copy(ATag, 2, Length(ATag));
  end;

begin
  BlocoTraduzido := '';
  Result := False;    // Deixa ACBrPosPrinter traduzir...

  if (ATag = cTagBarraCode128) or (ATag = cTagBarraCode128a) or
     (ATag = cTagBarraCode128b) or (ATag = cTagBarraCode128c) then
  begin
    BlocoTraduzido := RemontaBloco(cTagBarraCode128, ConteudoBloco);
    Result := True;
  end

  else if MatchText(ATag, CBLOCK_POST_PROCESS) then
  begin
    BlocoTraduzido := RemontaBloco(ATag, ConteudoBloco);
    Result := True;
  end;
end;

procedure TACBrPosPrinterElginE1Lib.ImprimirE1Lib(const LinhasImpressao: String;  var Tratado: Boolean);
var
  TextoAImprimir, Linha: string;
  SL: TStringList;
  i: Integer;
begin
  if LinhasImpressao.IsEmpty then
    Exit;

  TextoAImprimir := '';
  SL := TStringList.Create;
  try
    SL.Text := LinhasImpressao;
    for i := 0 to SL.Count-1 do
    begin
      Linha := TrimRight(SL[i]);
      if (Linha = '') then
        Linha := ' ';

      TextoAImprimir := TextoAImprimir + Linha + cTagBR;
    end;
  finally
    SL.Free;
  end;

  fE1LibTagProcessor.DecodificarTagsFormatacao(TextoAImprimir);
end;

procedure TACBrPosPrinterElginE1Lib.LerStatus(var AStatus: TACBrPosPrinterStatus);
var
  S: Integer;
begin
  AStatus := []; // OK
  S := fE1LibPrinter.Status(1);  // 1 - Status da Gaveta
  if (S = 1) then
    AStatus := AStatus + [stGavetaAberta];

  S := fE1LibPrinter.Status(3);  // 3 - Status do Papel
  if (S < 0) then
    AStatus := AStatus + [stErro];

  case S of
    6: AStatus := AStatus + [stPoucoPapel];
    7: AStatus := AStatus + [stSemPapel];
  end;
end;

procedure TACBrPosPrinterElginE1Lib.E1LibAdicionarBlocoResposta(const ConteudoBloco: AnsiString);
begin
  fE1LibPrinter.ImprimirTexto(ConteudoBloco);
end;

procedure TACBrPosPrinterElginE1Lib.E1LibTraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString);
begin
  TagTraduzida := '';

  with fE1LibPrinter do
  begin
    if (ATag = cTagLigaExpandido) then
      EstiloFonte := EstiloFonte + [ftExpandido]
    else if (ATag = cTagDesligaExpandido) then
      EstiloFonte := EstiloFonte - [ftExpandido]
    else if (ATag = cTagLigaAlturaDupla) then
      EstiloFonte := EstiloFonte + [ftAlturaDupla]
    else if (ATag = cTagDesligaAlturaDupla) then
      EstiloFonte := EstiloFonte - [ftAlturaDupla]
    else if (ATag = cTagLigaNegrito) then
      EstiloFonte := EstiloFonte + [ftNegrito]
    else if (ATag = cTagDesligaNegrito) then
      EstiloFonte := EstiloFonte - [ftNegrito]
    else if (ATag = cTagLigaSublinhado) then
      EstiloFonte := EstiloFonte + [ftSublinhado]
    else if (ATag = cTagDesligaSublinhado) then
      EstiloFonte := EstiloFonte - [ftSublinhado]
    else if (ATag = cTagLigaCondensado) then
      EstiloFonte := EstiloFonte + [ftCondensado]
    else if (ATag = cTagDesligaCondensado) then
      EstiloFonte := EstiloFonte - [ftCondensado]
    else if (ATag = cTagLigaItalico) then
      EstiloFonte := EstiloFonte + [ftItalico]
    else if (ATag = cTagDesligaItalico) then
      EstiloFonte := EstiloFonte - [ftItalico]
    else if (ATag = cTagLigaInvertido) then
      EstiloFonte := EstiloFonte + [ftInvertido]
    else if (ATag = cTagDesligaInvertido) then
      EstiloFonte := EstiloFonte - [ftInvertido]
    else if (ATag = cTagFonteA) then
      EstiloFonte := EstiloFonte - [ftFonteB]
    else if (ATag = cTagFonteB) then
      EstiloFonte := EstiloFonte + [ftFonteB]
    else if (ATag = cTagFonteNormal) then
      EstiloFonte := EstiloFonte - [ftCondensado, ftExpandido, ftAlturaDupla,
                                    ftNegrito, ftSublinhado, ftItalico, ftInvertido,
                                    ftFonteB]
    else if (ATag = cTagZera) or (ATag = cTagReset) then
    begin
      fE1LibPrinter.Restaurar;
      fE1LibPrinter.EspacoLinha := IfThen(fpPosPrinter.EspacoEntreLinhas = 0, 10, fpPosPrinter.EspacoEntreLinhas);
      EstiloFonte := EstiloFonte - [ftCondensado, ftExpandido, ftAlturaDupla,
                                    ftNegrito, ftSublinhado, ftItalico, ftInvertido];
    end
    else if (ATag = cTagBR) then
      TagTraduzida := ''  // Não faz nada aqui...
    else if (ATag = cTagPulodeLinha) then
      PularLinhas(1)
    else if (ATag = cTagPuloDeLinhas) then
      PularLinhas(fpPosPrinter.LinhasEntreCupons)
    else if (ATag = cTagCorte) then
      CortarPapel(fpPosPrinter.LinhasEntreCupons)
    else if (ATag = cTagBeep) then
      AndroidBeep(200)
    else if (ATag = cTagFonteAlinhadaEsquerda) then
      Alinhamento := TACBrPosTipoAlinhamento.alEsquerda
    else if (ATag = cTagFonteAlinhadaDireita) then
      Alinhamento := TACBrPosTipoAlinhamento.alDireita
    else if (ATag = cTagfonteAlinhadaCentro) then
      Alinhamento := TACBrPosTipoAlinhamento.alCentro;
  end;
end;

procedure TACBrPosPrinterElginE1Lib.E1LibTraduzirTagBloco(const ATag,
  ConteudoBloco: AnsiString; var BlocoTraduzido: AnsiString);
begin
  BlocoTraduzido := '';
  if (ATag = cTagBMP) then
    ProcessarComandoBMP(ConteudoBloco)

  else if (ATag = cTagQRCode) then
    ProcessarQRCode(ConteudoBloco)

  else if (ATag = cTagQRCodeLargura) then
    fpPosPrinter.ConfigQRCode.LarguraModulo :=
      StrToIntDef(ConteudoBloco, fpPosPrinter.ConfigQRCode.LarguraModulo)

  else if (ATag = cTagQRCodeTipo) or (ATag = cTagQRCodeError) then
    BlocoTraduzido := ''

  else if (ATag = cTagBarraMostrar) then
    BlocoTraduzido := ''

  else if (ATag = cTagBarraLargura) then
    fpPosPrinter.ConfigBarras.LarguraLinha :=
      StrToIntDef(ConteudoBloco, fpPosPrinter.ConfigBarras.LarguraLinha)

  else if (ATag = cTagBarraAltura) then
    fpPosPrinter.ConfigBarras.Altura :=
      StrToIntDef(ConteudoBloco, fpPosPrinter.ConfigBarras.Altura)

  else if (AnsiIndexText(ATag, CBLOCK_POST_PROCESS) >= 0) then
    ProcessarCodBarras(ConteudoBloco, ATag);
end;

procedure TACBrPosPrinterElginE1Lib.ProcessarComandoBMP(ConteudoBloco: AnsiString);
var
  ABitMap: TBitmap;
  AData: string;
begin
  AData := Trim(ConteudoBloco);
  if (AData = '') then
    Exit;

  ABitMap := TBitmap.Create;
  try
    ConteudoBlocoToBitmap(AData, ABitMap);
    fE1LibPrinter.ImprimirImagem(ABitMap);
  finally
    ABitMap.Free;
  end;
end;

procedure TACBrPosPrinterElginE1Lib.ProcessarQRCode(ConteudoBloco: AnsiString);
var
  L, E: Integer;
begin
  if (Trim(ConteudoBloco) = '') then
    Exit;

  L := max(min(fpPosPrinter.ConfigQRCode.LarguraModulo,6),1);
  if (fpPosPrinter.ConfigQRCode.ErrorLevel = 0) then
    E := 2
  else
    E := max(min(fpPosPrinter.ConfigQRCode.ErrorLevel, 4), 1);

  fE1LibPrinter.ImprimirQRCode(ConteudoBloco, L, E);
end;

procedure TACBrPosPrinterElginE1Lib.ProcessarCodBarras(
  ConteudoBloco: AnsiString; ATag: String);
var
  ACodBar: AnsiString;
  A, L, barCodeType: Integer;
begin
  ACodBar := fpPosPrinter.AjustarCodBarras(ConteudoBloco, ATag);
  if (ACodBar = '') then
     Exit;

  // UPC_A = 0; UPC_E = 1; EAN_13 = 2; EAN_8 = 3; CODE_39 = 4; ITF = 5;
  // CODE_BAR = 6; CODE_93 = 7; CODE_128 = 8;
  // HRI: 1 - Acima do código, 2 - Abaixo do código, 3 - Ambos, 4 - Não impresso.

  if (ATag = cTagBarraEAN8) then
    barCodeType := 3
  else if (ATag = cTagBarraEAN13) then
    barCodeType := 2
  else if (ATag = cTagBarraInter) then
    barCodeType := 5
  else if (ATag = cTagBarraCode39) then
    barCodeType := 4
  else if (ATag = cTagBarraCode93) then
    barCodeType := 7
  else if (ATag = cTagBarraCode128) then
    barCodeType := 8
  else if (ATag = cTagBarraUPCA) then
    barCodeType := 0
  else if (ATag = cTagBarraUPCE) then
    barCodeType := 1
  else if (ATag = cTagBarraCodaBar) then
    barCodeType := 6
  else
    Exit;

  A := max(min(fpPosPrinter.ConfigBarras.Altura,255),1);
  L := max(min(fpPosPrinter.ConfigBarras.LarguraLinha,6),1);
  fE1LibPrinter.ImprimirCodBarras( barCodeType, ACodBar, A, L,
                                   fpPosPrinter.ConfigBarras.MostrarCodigo );
end;

end.

