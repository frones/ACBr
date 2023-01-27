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

// TTS = TecToySunmi

{$I ACBr.inc}

unit ACBrPosPrinterTecToySunmiLib;

interface

uses
  Classes, SysUtils,
  ACBrConsts, ACBrDevice, ACBrBase, ACBrPosPrinter,
  ACBr.Android.Sunmi.Printer;

const
  cTagBR = '<br>';
  cTamFontNormal = 24;

  // Essas Tags serão interpretadas no momento da Impressão, pois usam comandos
  // específicos da Biblioteca SunmiPrinter
  CTAGS_POST_PROCESS: array[0..25] of string = (
    cTagPulodeLinha, cTagPuloDeLinhas, cTagBR,
    cTagLigaExpandido, cTagDesligaExpandido,
    cTagLigaAlturaDupla, cTagDesligaAlturaDupla,
    cTagLigaNegrito, cTagDesligaNegrito,
    cTagLigaSublinhado, cTagDesligaSublinhado,
    cTagLigaCondensado, cTagDesligaCondensado,
    cTagLigaInvertido, cTagDesligaInvertido,
    cTagFonteNormal,
    cTagFonteA, cTagFonteB,
    cTagFonteAlinhadaDireita, cTagFonteAlinhadaEsquerda, cTagfonteAlinhadaCentro,
    cTagBeep,
    cTagZera, cTagReset,
    cTagCorte, cTagAbreGaveta );

  CBLOCK_POST_PROCESS: array[0..17] of string = (
    cTagBarraEAN8, cTagBarraEAN13, cTagBarraInter,
    cTagBarraCode39, cTagBarraCode93,
    cTagBarraCode128,
    cTagBarraUPCA, cTagBarraUPCE, cTagBarraCodaBar,
    cTagBMP,
    cTagBarraMostrar, cTagBarraLargura, cTagBarraAltura,
    cTagQRCode, cTagQRCodeTipo, cTagQRCodeLargura, cTagQRCodeError,
    cTagAbreGavetaEsp );

  CTAGS_CORTE: array[0..2] of string =
    (cTagCorte, cTagCorteParcial, cTagCorteTotal);

type

  TACBrPosPrinterTecToySunmiLib = class(TACBrPosPrinterClass)
  private
    fLibPrinter: TACBrSunmiPrinter;
    fLibTagProcessor: TACBrTagProcessor;
    fEstiloFonte: TACBrPosFonte;
    fAlinhamento: TACBrPosTipoAlinhamento;

    procedure TTSLibTraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString);
    procedure TTSLibAdicionarBlocoResposta(const ConteudoBloco: AnsiString);
    procedure TTSLibTraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString);

    procedure ProcessarComandoBMP(ConteudoBloco: AnsiString);
    procedure ProcessarQRCode(ConteudoBloco: AnsiString);
    procedure ProcessarCodBarras(ConteudoBloco: AnsiString; ATag: String);

    procedure ProcessarEstiloFonte;
    procedure ProcessarAlinhamento;

    procedure ProcessarSendRAWData(const AData: AnsiString);
  protected
    procedure ImprimirTTSLib(const LinhasImpressao: String; var Tratado: Boolean);

  public
    constructor Create(AOwner: TACBrPosPrinter);
    destructor Destroy; override;

    procedure Configurar; override;
    function TraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString): Boolean;
      override;
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString): Boolean; override;

    function ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString; override;
    function ComandoEspacoEntreLinhas(Espacos: byte): AnsiString; override;

    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;
    function LerInfo: String; override;

    property EstiloFonte: TACBrPosFonte read fEstiloFonte write fEstiloFonte default [];
    property Alinhamento: TACBrPosTipoAlinhamento read fAlinhamento write fAlinhamento
      default TACBrPosTipoAlinhamento.alEsquerda;
  end;

implementation

uses
  StrUtils, Math,
  FMX.Graphics,
  ACBrPosPrinterAndroidHelper,
  ACBrUtil.Strings,
  ACBrUtil.Math;

constructor TACBrPosPrinterTecToySunmiLib.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'SunmiPrinterLib';

  TagsNaoSuportadas.Add( cTagLogotipo );
  TagsNaoSuportadas.Add( cTagLogoImprimir );
  TagsNaoSuportadas.Add( cTagBarraStd );
  TagsNaoSuportadas.Add( cTagBarraCode11 );
  TagsNaoSuportadas.Add( cTagBarraMSI );
  TagsNaoSuportadas.Add( cTagLigaItalico );
  TagsNaoSuportadas.Add( cTagDesligaItalico );

  fLibTagProcessor := TACBrTagProcessor.Create;
  fLibTagProcessor.AddTags(CTAGS_POST_PROCESS, [],  False);
  fLibTagProcessor.AddTags(CBLOCK_POST_PROCESS, [], True);

  fLibTagProcessor.OnTraduzirTag := TTSLibTraduzirTag;
  fLibTagProcessor.OnAdicionarBlocoResposta := TTSLibAdicionarBlocoResposta;
  fLibTagProcessor.OnTraduzirTagBloco := TTSLibTraduzirTagBloco;

  with Cmd do
  begin
    Zera                    := ESC + '@';
    PuloDeLinha             := LF;
    PuloDePagina            := FF;
    EspacoEntreLinhasPadrao := ESC + '2';
    EspacoEntreLinhas       := ESC + '3';
    FonteNormal             := ESC + '!' + #0;
    FonteA                  := ESC + 'M' + #0;
    FonteB                  := ESC + 'M' + #1;
    LigaNegrito             := ESC + 'E' + #1;
    DesligaNegrito          := ESC + 'E' + #0;
    LigaExpandido           := GS  + '!' + #16;
    DesligaExpandido        := GS  + '!' + #0;
    LigaAlturaDupla         := GS  + '!' + #1;
    DesligaAlturaDupla      := GS  + '!' + #0;
    LigaSublinhado          := ESC + '-' + #1;
    DesligaSublinhado       := ESC + '-' + #0;
    LigaInvertido           := GS  + 'B' + #1;
    DesligaInvertido        := GS  + 'B' + #0;
  end;

  fLibPrinter := TACBrSunmiPrinter.Create(Nil);
end;

destructor TACBrPosPrinterTecToySunmiLib.Destroy;
begin
  fLibTagProcessor.Free;
  fLibPrinter.Free;

  inherited;
end;

function TACBrPosPrinterTecToySunmiLib.ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString;
var
  CmdPag: Integer;
begin
  case APagCodigo of
    pc437: CmdPag := 0;
    pc850: CmdPag := 2;
    pc852: CmdPag := 18;
    pc860: CmdPag := 3;
    pc1252: CmdPag := 16;
  else
    CmdPag := -1;
  end;

  Result := FS +'.';   // Cancel Chinese character mode - NÃO FUNCIONA
  if (CmdPag >= 0) then
    Result := Result + ESC + 't' + AnsiChr( CmdPag );

  ProcessarSendRAWData(Result);
end;

function TACBrPosPrinterTecToySunmiLib.ComandoEspacoEntreLinhas(Espacos: byte): AnsiString;
begin
  Result := inherited ComandoEspacoEntreLinhas(Espacos);
  ProcessarSendRAWData(Result);
end;

procedure TACBrPosPrinterTecToySunmiLib.Configurar;
begin
  fpPosPrinter.Porta := 'NULL';
  fpPosPrinter.OnEnviarStringDevice := ImprimirTTSLib;
end;

function TACBrPosPrinterTecToySunmiLib.TraduzirTag(const ATag: AnsiString;
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

function TACBrPosPrinterTecToySunmiLib.TraduzirTagBloco(const ATag,
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

procedure TACBrPosPrinterTecToySunmiLib.ImprimirTTSLib(const LinhasImpressao: String;  var Tratado: Boolean);
var
  TextoAImprimir, Linha: string;
  SL: TStringList;
  i: Integer;
begin
  if LinhasImpressao.IsEmpty then
    Exit;

  fLibPrinter.enterPrinterBuffer(True);
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

    fLibTagProcessor.DecodificarTagsFormatacao(TextoAImprimir);
  finally
    fLibPrinter.exitPrinterBuffer(True);
  end;
end;

function TACBrPosPrinterTecToySunmiLib.LerInfo: String;
var
  TemGuilhotina: Boolean;
begin
  Result := '';
  Info.Clear;

  AddInfo(cKeyFirmware, Trim(fLibPrinter.getPrinterVersion)+'/'+Trim(fLibPrinter.getServiceVersion));
  AddInfo(cKeyFabricante, 'Sunmi');
  AddInfo(cKeyModelo, fLibPrinter.getPrinterModal);
  AddInfo(cKeySerial, fLibPrinter.getPrinterSerialNo);
  TemGuilhotina := (pos('nt', LowerCase(Info.Values[cKeyModelo])) > 0);
  AddInfo(cKeyGuilhotina, TemGuilhotina );

  Result := Info.text;
end;

procedure TACBrPosPrinterTecToySunmiLib.LerStatus(var AStatus: TACBrPosPrinterStatus);
begin
  fLibPrinter.updatePrinterState;

  case fLibPrinter.PrinterState of
    TACBrSunmiPrinterState.spsOk:
      AStatus := []; // OK
    TACBrSunmiPrinterState.spsInitializing:
      AStatus := [stApenasEscrita]; // 'Impressora atualizando';
    TACBrSunmiPrinterState.spsError:
      AStatus := [stErro];          // 'Impressora em erro';
    TACBrSunmiPrinterState.spsOutOfPaper:
      AStatus := [stSemPapel];      // 'Impressora sem papel';
    TACBrSunmiPrinterState.spsOverheated:
      AStatus := [stErro];          // 'Impressora Superaquecida';
    TACBrSunmiPrinterState.spsCoverIsOpen:
      AStatus := [stTampaAberta];   // 'Impressora com tampa aberta';
    TACBrSunmiPrinterState.spsCutterAbnormal:
      AStatus := [stErro];          // 'Impressora com erro no cortador';
    TACBrSunmiPrinterState.spsCutterNormal:;
    TACBrSunmiPrinterState.spsBlackMarkNotFound:
      AStatus := [stAguardandoSlip];
    TACBrSunmiPrinterState.spsPrinterNotDetected:
      AStatus := [stErroLeitura];   // 'Impressora não encontrada';
  end;

  if fLibPrinter.getOpenDrawerTimes.DrawerIsOpen then
    AStatus := AStatus + [stGavetaAberta];
end;

procedure TACBrPosPrinterTecToySunmiLib.TTSLibAdicionarBlocoResposta(const ConteudoBloco: AnsiString);
begin
  ProcessarAlinhamento;
  ProcessarEstiloFonte;
  fLibPrinter.printTextLF(ConteudoBloco);
end;

procedure TACBrPosPrinterTecToySunmiLib.TTSLibTraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString);
begin
  TagTraduzida := '';

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
    fLibPrinter.printerInit;
    fpPosPrinter.Zerar;
    EstiloFonte := EstiloFonte - [ftCondensado, ftExpandido, ftAlturaDupla,
                                  ftNegrito, ftSublinhado, ftItalico, ftInvertido];
  end
  else if (ATag = cTagBR) then
    TagTraduzida := ''  // Não faz nada aqui...
  else if (ATag = cTagPulodeLinha) then
    fLibPrinter.lineWrap(1)
  else if (ATag = cTagPuloDeLinhas) then
    fLibPrinter.lineWrap(fpPosPrinter.LinhasEntreCupons)
  else if (ATag = cTagCorte) then
  begin
    fLibPrinter.lineWrap(fpPosPrinter.LinhasEntreCupons);
    fLibPrinter.cutPaper;
  end
  else if (ATag = cTagAbreGaveta) then
    fLibPrinter.openDrawer
  else if (ATag = cTagBeep) then
    AndroidBeep(200)
  else if (ATag = cTagFonteAlinhadaEsquerda) then
    Alinhamento := TACBrPosTipoAlinhamento.alEsquerda
  else if (ATag = cTagFonteAlinhadaDireita) then
    Alinhamento := TACBrPosTipoAlinhamento.alDireita
  else if (ATag = cTagfonteAlinhadaCentro) then
    Alinhamento := TACBrPosTipoAlinhamento.alCentro;
end;

procedure TACBrPosPrinterTecToySunmiLib.TTSLibTraduzirTagBloco(const ATag,
  ConteudoBloco: AnsiString; var BlocoTraduzido: AnsiString);
var
  ACodBar: String;
  barCodeType, barCodeTextPos: Integer;
  A: Integer;
begin
  BlocoTraduzido := '';
  if (ATag = cTagBMP) then
    ProcessarComandoBMP(ConteudoBloco)

  else if (ATag = cTagQRCode) then
    ProcessarQRCode(ConteudoBloco)

  else if (ATag = cTagAbreGavetaEsp) then
    fLibPrinter.openDrawer

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

procedure TACBrPosPrinterTecToySunmiLib.ProcessarEstiloFonte;
var
  AByte, FontSize: Integer;
  ACmd: AnsiString;
begin
  AByte := 0;
  FontSize := cTamFontNormal;

  if (ftCondensado in EstiloFonte) or (ftFonteB in EstiloFonte) then
    FontSize := Trunc(FontSize * RazaoColunaFonte.Condensada);

  if (ftNegrito in EstiloFonte) then
    SetBit(AByte, 3);

  if (ftAlturaDupla in EstiloFonte) then
    SetBit(AByte, 4);

  if (ftExpandido in EstiloFonte) then
    SetBit(AByte, 5);  // FontSize := FontSize * RazaoColunaFonte.Expandida;

  if (ftSublinhado in EstiloFonte) then
    SetBit(AByte, 7);

  ACmd := ESC + '!' + AnsiChr(Byte(AByte));

  // ESC ! desliga Invertido, enviando o comando novamente
  if (ftInvertido in EstiloFonte) then
    ACmd := ACmd + Cmd.LigaInvertido
  else
    ACmd := ACmd + Cmd.DesligaInvertido;

  fLibPrinter.setFontSize(FontSize);
  ProcessarSendRAWData(ACmd);
end;

procedure TACBrPosPrinterTecToySunmiLib.ProcessarAlinhamento;
var
  Align: Integer;
begin
  case Alinhamento of
    alEsquerda: Align := 0;
    alCentro: Align := 1;
    alDireita: Align := 2;
  end;

  fLibPrinter.setAlignment(Align);
end;

procedure TACBrPosPrinterTecToySunmiLib.ProcessarComandoBMP(ConteudoBloco: AnsiString);
var
  ABitMap: TBitmap;
begin
  if (Trim(ConteudoBloco) = '') then
     Exit;

  ABitMap := TBitmap.Create;
  try
    ConteudoBlocoToBitmap(ConteudoBloco, ABitMap);

    ProcessarAlinhamento;
    fLibPrinter.printBitmap(ABitMap);
    fLibPrinter.lineWrap(1);
  finally
    ABitMap.Free;
  end;
end;

procedure TACBrPosPrinterTecToySunmiLib.ProcessarQRCode(ConteudoBloco: AnsiString);
var
  L, E: Integer;
  AData: String;
begin
  AData := Trim(ConteudoBloco);
  if (AData = '') then
    Exit;

  L := max(min(fpPosPrinter.ConfigQRCode.LarguraModulo,6),1);
  if (fpPosPrinter.ConfigQRCode.ErrorLevel = 0) then
    E := 2
  else
    E := max(min(fpPosPrinter.ConfigQRCode.ErrorLevel, 4), 1);

  ProcessarAlinhamento;
  fLibPrinter.printQRCode(AData, L, E);
  fLibPrinter.lineWrap(1);
end;

procedure TACBrPosPrinterTecToySunmiLib.ProcessarSendRAWData(const AData: AnsiString);
var
  ab: TBytes;
begin
  if (Length(AData) = 0) then
    Exit;

  setLength(ab, length(AData));
  Move(AData[1], ab[0], Length(AData));
  fLibPrinter.sendRAWData(ab);
end;

procedure TACBrPosPrinterTecToySunmiLib.ProcessarCodBarras(ConteudoBloco: AnsiString; ATag: String);
var
  ACodBar: AnsiString;
  A, L, barCodeType, barCodeTextPos: Integer;
begin
  ACodBar := fpPosPrinter.AjustarCodBarras(ConteudoBloco, ATag);
  if (ACodBar = '') then
     Exit;

  // UPC_A = 0; UPC_E = 1; EAN_13 = 2; EAN_8 = 3; CODE_39 = 4; ITF = 5;
  // CODE_BAR = 6; CODE_93 = 7; CODE_128 = 8;
  // HRI: 1 - Acima do código, 2 - Abaixo do código, 3 - Ambos, 4 - Não impresso.

  if (ATag = cTagBarraUPCA) then
    barCodeType := 0
  else if (ATag = cTagBarraUPCE) then
    barCodeType := 1
  else if (ATag = cTagBarraEAN13) then
    barCodeType := 2
  else if (ATag = cTagBarraEAN8) then
    barCodeType := 3
  else if (ATag = cTagBarraCode39) then
    barCodeType := 4
  else if (ATag = cTagBarraInter) then
    barCodeType := 5
  else if (ATag = cTagBarraCodaBar) then
    barCodeType := 6
  else if (ATag = cTagBarraCode93) then
    barCodeType := 7
  else if (ATag = cTagBarraCode128) then
    barCodeType := 8
  else
    Exit;

  if fpPosPrinter.ConfigBarras.MostrarCodigo then
    barCodeTextPos := 2
  else
    barCodeTextPos := 0;

  if (fpPosPrinter.ConfigBarras.Altura = 0) then
    A := 50
  else
    A := max(min(fpPosPrinter.ConfigBarras.Altura,255),5);

  L := max(min(fpPosPrinter.ConfigBarras.LarguraLinha,6),1);

  ProcessarAlinhamento;
  fLibPrinter.printBarCode( ACodBar, barCodeType, A, L, barCodeTextPos );
  fLibPrinter.lineWrap(1);
end;

end.

