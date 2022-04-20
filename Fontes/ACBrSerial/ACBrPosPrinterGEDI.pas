{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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

{$I ACBr.inc}

unit ACBrPosPrinterGEDI;

interface

uses
  Classes, SysUtils,
  FMX.Graphics,
  Androidapi.JNI.GraphicsContentViewText,
  ACBrConsts, ACBrDevice, ACBrBase, ACBrPosPrinter,
  {$IFDEF __G800__}
   G800Interface
  {$ELSE}
   G700Interface
  {$ENDIF};

resourcestring
  cErroImpressoraSemPapapel = 'Impressora sem Papel';
  cErroImpressoraNaoPronta = 'Impressora não pronta';
  cErroImpressora = 'Erro na Impressora';

const
  cTagBR = '<br>';

  // Essas Tags serão interpretadas no momento da Impressão, pois usam comandos
  // específicos da Biblioteca GEDI
  CTAGS_POST_PROCESS: array[0..25] of string = (
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
    cTagZera, cTagReset );

  CBLOCK_POST_PROCESS: array[0..16] of string = (
    cTagBarraEAN8, cTagBarraEAN13, cTagBarraInter,
    cTagBarraCode39, cTagBarraCode93,
    cTagBarraCode128,
    cTagBarraUPCA, cTagBarraUPCE, cTagBarraCodaBar,
    cTagBMP,
    cTagBarraMostrar, cTagBarraLargura, cTagBarraAltura,
    cTagQRCode, cTagQRCodeTipo, cTagQRCodeLargura, cTagQRCodeError );

  CTAGS_AVANCO: array[0..2] of string =
    (cTagCorte, cTagCorteParcial, cTagCorteTotal);

type

  TGEDIPrinterFontFamily = (gffDefault, gffMonoSpace, gffSans, gffSerif, gffExternal);

  TGEDIPrinter = class
  private
    fiPRNTR:JIPRNTR;
    fEstiloFonte: TACBrPosFonte;
    fAlinhamento: TACBrPosTipoAlinhamento;
    fImprimindo: Boolean;
    fEspacoLinha: Integer;
    fTipoFonte: TGEDIPrinterFontFamily;
    fTamanhoTexto: Integer;
    fPathFonte: String;
    fFontTypeFace: JTypeface;

    procedure SetEspacoLinha(const Value: Integer);
    procedure SetTamanhoTexto(const Value: Integer);
    procedure SetTipoFonte(const Value: TGEDIPrinterFontFamily);
    procedure SetPathFonte(const Value: String);
  public
    constructor Create;
    procedure Restaurar;

    property Imprimindo: Boolean read fImprimindo;

    procedure IniciarImpressao;
    procedure FinalizarImpressao;

    procedure PularLinhas(Linhas: Integer);
    procedure ImprimirTexto(Texto: String);
    procedure ImprimirImagem(BitMap: TBitmap);
    procedure ImprimirCodBarras(barCodeType: JGEDI_PRNTR_e_BarCodeType;
                              Conteudo: String; Altura, Largura: Integer);

    function Status: Integer;

    property iPRNTR:JIPRNTR read fiPRNTR;

    property EstiloFonte: TACBrPosFonte read fEstiloFonte write fEstiloFonte
      default [];
    property Alinhamento: TACBrPosTipoAlinhamento read fAlinhamento write fAlinhamento
      default TACBrPosTipoAlinhamento.alEsquerda;
    property EspacoLinha: Integer read fEspacoLinha write SetEspacoLinha
      default 10;
    property TamanhoTexto: Integer read fTamanhoTexto write SetTamanhoTexto
      default 20;
    property TipoFonte: TGEDIPrinterFontFamily read fTipoFonte
      write SetTipoFonte default TGEDIPrinterFontFamily.gffMonoSpace;
    property PathFonte: String read fPathFonte write SetPathFonte;
  end;

  { TACBrPosPrinterGEDI }

  TACBrPosPrinterGEDI = class(TACBrPosPrinterClass)
  private
    fGEDIPrinter: TGEDIPrinter;
    fGEDITagProcessor: TACBrTagProcessor;

    procedure GEDITraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString);
    procedure GEDIAdicionarBlocoResposta(const ConteudoBloco: AnsiString);
    procedure GEDITraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString);

    procedure ProcessarComandoBMP(ConteudoBloco: AnsiString);
  protected
    procedure ImprimirGEDI(const LinhasImpressao: String; var Tratado: Boolean);

  public
    constructor Create(AOwner: TACBrPosPrinter);
    destructor Destroy; override;

    procedure Configurar; override;
    function TraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString): Boolean;
      override;
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString): Boolean; override;
    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;
  end;

Function BitmapToJBitmap(const ABitmap: TBitmap): JBitmap;

implementation

uses
  System.Threading,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  AndroidApi.JNI.Media,
  FMX.Helpers.Android,
  FMX.Surfaces,
  StrUtils, Math,
  synacode,
  ACBrUtil.Strings,
  ACBrImage;

//https://forums.embarcadero.com/thread.jspa?threadID=245452&tstart=0
Function BitmapToJBitmap(const ABitmap: TBitmap): JBitmap;
var
  LSurface: TBitmapSurface;
  LBitmap : JBitmap;
begin
  Result := nil;
  LSurface := TBitmapSurface.Create;
  try
    LSurface.Assign(ABitmap);
    LBitmap := TJBitmap.JavaClass.createBitmap( LSurface.Width,
                                                LSurface.Height,
                                                TJBitmap_Config.JavaClass.ARGB_8888);
    if SurfaceToJBitmap(LSurface, LBitmap) then
      Result := LBitmap;
  finally
    LSurface.Free;
  end;
end;

procedure AndroidBeep(ADuration: Integer);
begin
  // https://stackoverflow.com/questions/30938946/how-do-i-make-a-beep-sound-in-android-using-delphi-and-the-api
  TJToneGenerator.JavaClass.init( TJAudioManager.JavaClass.ERROR,
                                  TJToneGenerator.JavaClass.MAX_VOLUME)
    .startTone( TJToneGenerator.JavaClass.TONE_DTMF_0,
                ADuration );
end;

{ TGEDIPrinter }

constructor TGEDIPrinter.Create;
var
  aTask: ITask;
begin
  fImprimindo := False;
  fFontTypeFace := Nil;
  fiPRNTR := Nil;
  aTask := TTask.Create( procedure
             begin
               fiPRNTR := TJGEDI.JavaClass.getInstance(TAndroidHelper.Activity).getPRNTR;
             end);
  aTask.Start;

  Restaurar;
end;

procedure TGEDIPrinter.Restaurar;
begin
  fEstiloFonte := [];
  fAlinhamento := TACBrPosTipoAlinhamento.alEsquerda;
  fEspacoLinha := 10;
  fTamanhoTexto := 20;
  TipoFonte := TGEDIPrinterFontFamily.gffMonoSpace;
end;

procedure TGEDIPrinter.IniciarImpressao;
begin
  if fImprimindo then
    Exit;

  if not Assigned(fiPRNTR) then
    raise EPosPrinterException.Create('PRNTR not found');

  fiPRNTR.Init;
  fImprimindo := True;
end;

procedure TGEDIPrinter.FinalizarImpressao;
begin
  IniciarImpressao;

  try
    try
      fiPRNTR.Output;
    except
      On E: Exception do
      begin
        if E.Message.Contains('PRNTR_OUT_OF_PAPER') then
          raise EPosPrinterException.Create(cErroImpressoraSemPapapel)
        else if E.Message.Contains('PRNTR_NOT_READY') then
          raise EPosPrinterException.Create(cErroImpressoraNaoPronta)
        else if E.Message.Contains('PRNTR_ERROR') then
          raise EPosPrinterException.Create(cErroImpressora)
        else
          raise;
      end;
    end;
  finally
    fImprimindo := False;
  end;
end;

procedure TGEDIPrinter.SetEspacoLinha(const Value: Integer);
begin
  fEspacoLinha := max(Value, 1);
end;

procedure TGEDIPrinter.SetTipoFonte(const Value: TGEDIPrinterFontFamily);
begin
  if (Value <> gffExternal) then
    fPathFonte := '';

  case Value of
    gffMonoSpace: fFontTypeFace := TJTypeface.JavaClass.MONOSPACE;
    gffSans: fFontTypeFace := TJTypeface.JavaClass.SANS_SERIF;
    gffSerif: fFontTypeFace := TJTypeface.JavaClass.SERIF;
    gffExternal: SetPathFonte(PathFonte);
  else
    fFontTypeFace := TJTypeface.JavaClass.DEFAULT;
  end;

  fTipoFonte := Value;
end;

procedure TGEDIPrinter.SetPathFonte(const Value: String);
begin
  fPathFonte := Value;
  if (Value <> '') then
  begin
    fFontTypeFace := TJTypeFace.JavaClass.createFromFile(StringToJString(fPathFonte));
    fTipoFonte := gffExternal;
  end
  else
    TipoFonte := gffMonoSpace;
end;

procedure TGEDIPrinter.SetTamanhoTexto(const Value: Integer);
begin
  fTamanhoTexto := min(max(Value, 5), 100);
end;

procedure TGEDIPrinter.PularLinhas(Linhas: Integer);
begin
  IniciarImpressao;
  Linhas := max(1, min(999, Linhas));
  iPRNTR.DrawBlankLine( Linhas * TamanhoTexto );
end;

procedure TGEDIPrinter.ImprimirTexto(Texto: String);
var
  Apaint: JPaint;
  AConfig: JGEDI_PRNTR_st_StringConfig;
  Estilo: Integer;
  TextSize, TextScale: Single;
begin
  IniciarImpressao;

  if Texto.Trim.IsEmpty then
  begin
    PularLinhas(1);
    Exit;
  end;

  // Criando Objetos de configuração
  Apaint:= TJPaint.Create;
  AConfig := TJGEDI_PRNTR_st_StringConfig.Create;
  AConfig.offset := 0;
  AConfig.paint := Apaint;

  // Ajustando Tamanho de Espaco da Linha
  AConfig.lineSpace := EspacoLinha;

  // Ajustando Altura do Texto
  TextSize := TamanhoTexto;  // Fonte Normal
  if (ftAlturaDupla in EstiloFonte) then
    TextSize := TextSize * 2;

  Apaint.setTextSize(TextSize);

  // Ajustando Largura do Texto
  if (ftCondensado in EstiloFonte) or (ftFonteB in EstiloFonte) then
    TextScale := 0.75
  else
    TextScale := 1;

  if (ftExpandido in EstiloFonte) then
  begin
    if not (ftAlturaDupla in EstiloFonte)  then
      TextScale := TextScale * 2
  end
  else if (ftAlturaDupla in EstiloFonte) then
    TextScale := TextScale / 2;

  Apaint.setTextScaleX(TextScale);

  // Ajusta o alinhamento do Texto
  case Alinhamento of
    alCentro: Apaint.setTextAlign(TJPaint_Align.JavaClass.CENTER);
    alDireita: Apaint.setTextAlign(TJPaint_Align.JavaClass.RIGHT);
  else
    Apaint.setTextAlign(TJPaint_Align.JavaClass.LEFT);
  end;

  // Definindo Estilo Normal, Negrito e Itálico
  Estilo := 0; // Normal
  if (ftNegrito in EstiloFonte) then
    Inc(Estilo, 1);
  if (ftItalico in EstiloFonte) then
    Inc(Estilo, 2);

  // Definindo Estilo Sublinhado
  if (ftSublinhado in EstiloFonte) then
    Apaint.setFlags(TJPaint.JavaClass.UNDERLINE_TEXT_FLAG);

  // Definindo Estilo Invertido - A FAZE
  // https://stackoverflow.com/questions/8242439/how-to-draw-text-with-background-color-using-canvas
  // if (ftInvertido in EstiloFonte) then
  //  Apaint.setColor();

  Apaint.setTypeface( TJTypeface.JavaClass.create(fFontTypeFace, Estilo) );

  iPRNTR.DrawStringExt(AConfig, StringToJString(Texto));

end;

procedure TGEDIPrinter.ImprimirCodBarras(barCodeType: JGEDI_PRNTR_e_BarCodeType;
  Conteudo: String; Altura, Largura: Integer);
var
  BarCodeConfig: JGEDI_PRNTR_st_BarCodeConfig;
begin
  if Conteudo.IsEmpty then
    Exit;

  IniciarImpressao;

  BarCodeConfig := TJGEDI_PRNTR_st_BarCodeConfig.Create;
  BarCodeConfig.barCodeType := barCodeType;
  BarCodeConfig.width := Largura;
  BarCodeConfig.height := Altura;

  iPRNTR.DrawBarCode( BarCodeConfig, StringToJString(Conteudo) );
end;

procedure TGEDIPrinter.ImprimirImagem(BitMap: TBitmap);
var
  imgConfig: JGEDI_PRNTR_st_PictureConfig;
begin
  IniciarImpressao;
  imgConfig := TJGEDI_PRNTR_st_PictureConfig.Create;

  {$IFNDEF __G800__}
   imgConfig.offset := 0;
  {$ENDIF}
  // Ajusta o alinhamento da Imagem
  case Alinhamento of
    alEsquerda: imgConfig.alignment := TJGEDI_PRNTR_e_Alignment.JavaClass.LEFT;
    alDireita: imgConfig.alignment := TJGEDI_PRNTR_e_Alignment.JavaClass.RIGHT;
  else
    imgConfig.alignment := TJGEDI_PRNTR_e_Alignment.JavaClass.CENTER;
  end;

  imgConfig.height := BitMap.Height;
  imgConfig.width := BitMap.Width;

  iPRNTR.DrawPictureExt(imgConfig, BitmapToJBitmap(BitMap) );
end;

function TGEDIPrinter.Status: Integer;
begin
  Result:= iPRNTR.Status.ordinal;
end;

{ TACBrPosPrinterGEDI }

constructor TACBrPosPrinterGEDI.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'GEDIPrinter';

  TagsNaoSuportadas.Add( cTagLogotipo );
  TagsNaoSuportadas.Add( cTagLogoImprimir );
  TagsNaoSuportadas.Add( cTagBarraStd );
  TagsNaoSuportadas.Add( cTagBarraCode11 );
  TagsNaoSuportadas.Add( cTagBarraMSI );

  fGEDITagProcessor := TACBrTagProcessor.Create;
  fGEDITagProcessor.AddTags(CTAGS_POST_PROCESS, [],  False);
  fGEDITagProcessor.AddTags(CBLOCK_POST_PROCESS, [], True);

  fGEDITagProcessor.OnTraduzirTag := GEDITraduzirTag;
  fGEDITagProcessor.OnAdicionarBlocoResposta := GEDIAdicionarBlocoResposta;
  fGEDITagProcessor.OnTraduzirTagBloco := GEDITraduzirTagBloco;

  fGEDIPrinter := TGEDIPrinter.Create;
end;

destructor TACBrPosPrinterGEDI.Destroy;
begin
  fGEDITagProcessor.Free;
  fGEDIPrinter.Free;

  inherited;
end;

procedure TACBrPosPrinterGEDI.Configurar;
begin
  fpPosPrinter.Porta := 'NULL';
  fpPosPrinter.OnEnviarStringDevice := ImprimirGEDI;

  {$IFDEF __G800__}
   fpPosPrinter.PaginaDeCodigo := TACBrPosPaginaCodigo.pc1252;
  {$ELSE}
   fpPosPrinter.PaginaDeCodigo := TACBrPosPaginaCodigo.pcUTF8;
  {$ENDIF}
end;

function TACBrPosPrinterGEDI.TraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString): Boolean;
begin
  TagTraduzida := '';
  Result := True;
  if MatchText(ATag, CTAGS_POST_PROCESS) then
    TagTraduzida := ATag
  else if MatchText(ATag, CTAGS_AVANCO) then
    TagTraduzida := cTagPuloDeLinhas
  else if (ATag = cTagRetornoDeCarro) then
    TagTraduzida := cTagBR
  else
    Result := False;  // Deixa ACBrPosPrinter traduzir...
end;

function TACBrPosPrinterGEDI.TraduzirTagBloco(const ATag,
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

procedure TACBrPosPrinterGEDI.ImprimirGEDI(const LinhasImpressao: String;  var Tratado: Boolean);
var
  TextoAImprimir, Linha: string;
  SL: TStringList;
  i: Integer;
begin
  if LinhasImpressao.IsEmpty then
    Exit;

  fGEDIPrinter.IniciarImpressao;
  try
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

    fGEDITagProcessor.DecodificarTagsFormatacao(TextoAImprimir);
  finally
    fGEDIPrinter.FinalizarImpressao;
  end;
end;

procedure TACBrPosPrinterGEDI.LerStatus(var AStatus: TACBrPosPrinterStatus);
begin
  AStatus := []; // OK

  case fGEDIPrinter.Status of
    1, 3: AStatus := [TACBrPosTipoStatus.stErro];
    2: AStatus := [TACBrPosTipoStatus.stSemPapel];
  end;
end;

procedure TACBrPosPrinterGEDI.GEDIAdicionarBlocoResposta(const ConteudoBloco: AnsiString);
begin
  fGEDIPrinter.ImprimirTexto(ConteudoBloco);
end;

procedure TACBrPosPrinterGEDI.GEDITraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString);
begin
  TagTraduzida := '';

  with fGEDIPrinter do
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
      fGEDIPrinter.Restaurar;
      fGEDIPrinter.EspacoLinha := IfThen(fpPosPrinter.EspacoEntreLinhas = 0, 10, fpPosPrinter.EspacoEntreLinhas);
      EstiloFonte := EstiloFonte - [ftCondensado, ftExpandido, ftAlturaDupla,
                                    ftNegrito, ftSublinhado, ftItalico, ftInvertido];
    end
    else if (ATag = cTagBR) then
      TagTraduzida := ''  // Não faz nada aqui...
    else if (ATag = cTagPulodeLinha) then
      PularLinhas(1)
    else if (ATag = cTagPuloDeLinhas) then
      PularLinhas(fpPosPrinter.LinhasEntreCupons)
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

procedure TACBrPosPrinterGEDI.GEDITraduzirTagBloco(const ATag,
  ConteudoBloco: AnsiString; var BlocoTraduzido: AnsiString);
var
  ACodBar: String;
  barCodeType: JGEDI_PRNTR_e_BarCodeType;
  A, L: Integer;
  D: Double;
begin
  BlocoTraduzido := '';
  if (ATag = cTagBMP) then
    ProcessarComandoBMP(ConteudoBloco)

  else if (ATag = cTagQRCode) then
  begin
    barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.QR_CODE;
    D := 5 - max(min(fpPosPrinter.ConfigQRCode.LarguraModulo,4),1);
    A := min(380, Trunc(fpPosPrinter.CalcularAlturaQRCodeAlfaNumM(ConteudoBloco)/D) );
    fGEDIPrinter.ImprimirCodBarras( barCodeType, ConteudoBloco, A, A );
  end

  else if (ATag = cTagQRCodeLargura) then
    fpPosPrinter.ConfigQRCode.LarguraModulo := StrToIntDef(
       ConteudoBloco, fpPosPrinter.ConfigQRCode.LarguraModulo)

  else if (ATag = cTagQRCodeTipo) or (ATag = cTagQRCodeError) then
    BlocoTraduzido := ''

  else if (ATag = cTagBarraMostrar) then
    BlocoTraduzido := ''

  else if (ATag = cTagBarraLargura) then
    fpPosPrinter.ConfigBarras.LarguraLinha := StrToIntDef(
       ConteudoBloco, fpPosPrinter.ConfigBarras.LarguraLinha)

  else if (ATag = cTagBarraAltura) then
    fpPosPrinter.ConfigBarras.Altura := StrToIntDef(
       ConteudoBloco, fpPosPrinter.ConfigBarras.Altura)

  else if (AnsiIndexText(ATag, CBLOCK_POST_PROCESS) >= 0) then
  begin
    ACodBar := fpPosPrinter.AjustarCodBarras(ConteudoBloco, ATag);

    if (ATag = cTagBarraEAN8) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.EAN_8
    else if (ATag = cTagBarraEAN13) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.EAN_13
    else if (ATag = cTagBarraInter) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.ITF
    else if (ATag = cTagBarraCode39) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.CODE_39
    else if (ATag = cTagBarraCode93) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.CODE_93
    else if (ATag = cTagBarraCode128) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.CODE_128
    else if (ATag = cTagBarraUPCA) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.UPC_A
    else if (ATag = cTagBarraUPCE) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.UPC_E
    else if (ATag = cTagBarraCodaBar) then
      barCodeType := TJGEDI_PRNTR_e_BarCodeType.JavaClass.CODABAR
    else
      barCodeType := Nil;

    if (barCodeType <> Nil) then
    begin
      with  fpPosPrinter.ConfigBarras do
      begin
        A := IfThen(Altura = 0, 50, max(min(Altura,255),1));
        L := IfThen( LarguraLinha = 0, 2, max(min(LarguraLinha,4),1) ) * 70;
      end;
      
      fGEDIPrinter.ImprimirCodBarras( barCodeType, ACodBar, A, L );
    end;
  end;
end;

procedure TACBrPosPrinterGEDI.ProcessarComandoBMP(ConteudoBloco: AnsiString);
var
  ABitMap: TBitmap;
  ARasterStr: AnsiString;
  AHeight, AWidth: Integer;
  SL: TStringList;
  AData: String;
  MS: TMemoryStream;
  SS: TStringStream;
begin
  AData := Trim(ConteudoBloco);
  if (AData = '') then
    Exit;

  ABitMap := TBitmap.Create;
  try
    if StrIsBinary(LeftStr(AData,10)) then           // AscII Art
    begin
      SL := TStringList.Create;
      MS := TMemoryStream.Create;
      try
        SL.Text := AData;
        AWidth := 0; AHeight := 0; ARasterStr := '';
        AscIIToRasterStr(SL, AWidth, AHeight, ARasterStr);
        RasterStrToBMPMono(ARasterStr, AWidth, False, MS);
        MS.Position := 0;
        ABitMap.LoadFromStream(MS);
      finally
        SL.Free;
        MS.Free;
      end;
    end

    else if StrIsBase64(AData) then
    begin
      SS := TStringStream.Create(DecodeBase64(AData));
      try
        SS.Position := 0;
        ABitMap.LoadFromStream(SS);
      finally
        SS.Free;
      end;
    end

    else
      ABitMap.LoadFromFile(AData);

    fGEDIPrinter.ImprimirImagem(ABitMap);
  finally
    ABitMap.Free;
  end;
end;

end.


