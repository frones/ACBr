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

{$I ACBr.inc}

unit ACBrETQZplII;

interface

uses
  Classes,
  ACBrETQClass, ACBrDevice
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrETQZplII }

  TACBrETQZplII = class(TACBrETQClass)
  private
    function ComandoCampo(const aTexto: String): String;
    function ConverterOrientacao(aOrientacao: TACBrETQOrientacao): String;
    function ConverterExibeCodigo(aExibeCodigo: TACBrETQBarraExibeCodigo): String;
    function ConverterMultiplicadorImagem(aMultiplicador: Integer): String;
    function GetDriverImagens: String;
  protected
    function ConverterPaginaDeCodigo(aPaginaDeCodigo: TACBrETQPaginaCodigo): String; virtual;
    function ComandoTamanhoBarras( aBarraFina, aBarraLargaa , aAlturaBarra:Integer ):String; virtual;
    function ComandoCoordenadas(aVertical, aHorizontal: Integer): String; virtual;
    function ComandoReverso(aImprimirReverso: Boolean): String; virtual;
    function ComandoFonte(const aFonte: String; aMultVertical, aMultHorizontal: Integer;
      aOrientacao: TACBrETQOrientacao): String; virtual;
    function ComandoCor: String; virtual;
    function ComandoBarras(const aTipo: String; aOrientacao: TACBrETQOrientacao;
      aAlturaBarras: Integer; aExibeCodigo: TACBrETQBarraExibeCodigo): String; virtual;
    function ComandoLinhaCaixa(aAltura, aLargura, Espessura, aCanto: Integer): String; virtual;
    function CrcZB64(const AString: AnsiString): String; virtual;

    function AjustarNomeArquivoImagem(const aNomeImagem: String; var aTipo: String): String;

    function ComandoAbertura: AnsiString; override;
    function ComandoGuilhotina: AnsiString; override;
    function ComandoUnidade: AnsiString; override;
    function ComandoTemperatura: AnsiString; override;
    function ComandoPaginaDeCodigo: AnsiString; override;
    function ComandoOrigemCoordenadas: AnsiString; override;
    function ComandoResolucao: AnsiString; override;
    function ComandoVelocidade: AnsiString; override;
    function ComandoBackFeed: AnsiString; override;

  public
    constructor Create(AOwner: TComponent);

    function TratarComandoAntesDeEnviar(const aCmd: AnsiString): AnsiString; override;

    function ComandoLimparMemoria: AnsiString; override;
    function ComandoCopias(const NumCopias: Integer): AnsiString; override;
    function ComandoImprimir: AnsiString; override;
    function ComandoAvancarEtiqueta(const aAvancoEtq: Integer): AnsiString; override;

    function ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao; aFonte: String;
      aMultHorizontal, aMultVertical, aVertical, aHorizontal: Integer; aTexto: String;
      aSubFonte: Integer = 0; aImprimirReverso: Boolean = False): AnsiString; override;

    function ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra): String; override;
    function ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao; aTipoBarras: String;
      aBarraLarga, aBarraFina, aVertical, aHorizontal: Integer; aTexto: String;
      aAlturaBarras: Integer; aExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao
      ): AnsiString; override;
    function ComandoImprimirQRCode(aVertical, aHorizontal: Integer;
      const aTexto: String; aLarguraModulo: Integer; aErrorLevel: Integer;
      aTipo: Integer): AnsiString; override;

    function ComandoImprimirLinha(aVertical, aHorizontal, aLargura, aAltura: Integer
      ): AnsiString; override;

    function ComandoImprimirCaixa(aVertical, aHorizontal, aLargura, aAltura,
      aEspVertical, aEspHorizontal: Integer; aCanto: Integer = 0): AnsiString; override;

    function ComandoImprimirImagem(aMultImagem, aVertical, aHorizontal: Integer;
      aNomeImagem: String): AnsiString; override;
    function ComandoCarregarImagem(aStream: TStream; var aNomeImagem: String;
      aFlipped: Boolean; aTipo: String): AnsiString; override;
    function ComandoApagarImagem(const NomeImagem: String = '*'): String; override;

    function ComandoBMP2GRF(aStream: TStream; aNomeImagem: String; Inverter: Boolean = True): AnsiString;
    function ComandoGravaRFIDHexaDecimal(aValue:String): AnsiString; override;
    function ComandoGravaRFIDASCII( aValue:String ): AnsiString; override;

  end;

implementation

uses
  Math, {$IFNDEF COMPILER6_UP} ACBrD5, Windows, {$ENDIF} sysutils, strutils,
  synautil, synacode, ACBrImage, ACBrConsts,
  ACBrETQ,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.Math;

{ TACBrETQPpla }

constructor TACBrETQZplII.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'ZPLII';
  fpLimiteCopias := 999;
end;

function TACBrETQZplII.ComandoLimparMemoria: AnsiString;
begin
  Result := '^MCY';
end;

function TACBrETQZplII.ComandoCampo(const aTexto: String): String;
var
  ConvTexto: String;
begin
  ConvTexto := aTexto;
  if (pos('\', ConvTexto) > 0) then
    ConvTexto := StringReplace(ConvTexto, '\', '\x5C', [rfReplaceAll]);

  if (pos('^', ConvTexto) > 0) then
    ConvTexto := StringReplace(ConvTexto, '^', '\x5E', [rfReplaceAll]);

  if (pos('~', ConvTexto) > 0) then
    ConvTexto := StringReplace(ConvTexto, '~', '\x7E', [rfReplaceAll]);

  ConvTexto := BinaryStringToString(ConvTexto);
  ConvTexto := StringReplace(ConvTexto, '\x', '\', [rfReplaceAll]);

  Result := '^FD' + ConvTexto + '^FS';

  if (ConvTexto <> aTexto) then
    Result := '^FH\' + Result;
end;

function TACBrETQZplII.ComandoFonte(const aFonte: String; aMultVertical,
  aMultHorizontal: Integer; aOrientacao: TACBrETQOrientacao): String;
var
  cFonte: Char;
begin
  if (aMultVertical > 32000) then
    raise Exception.Create('Multiplicador Vertical deve estar entre 10 e 32000');

  if (aMultHorizontal > 32000) then
    raise Exception.Create('Multiplicador Horizontal deve estar entre 10 e 32000');

  cFonte := PadLeft(aFonte,1,'A')[1];
  if not CharInSet(cFonte, ['0'..'9','A'..'Z']) then
    raise Exception.Create('Fonte deve "0" a "9" e "A" a "Z"');

  Result := '^A' + cFonte +
                   ConverterOrientacao(aOrientacao)   + ',' +
                   IntToStr(Max(aMultVertical,1))     + ',' +
                   IntToStr(Max(aMultHorizontal,1));
end;

function TACBrETQZplII.ComandoCor: String;
begin
  Result := EmptyStr;
end;

function TACBrETQZplII.ComandoGravaRFIDASCII(aValue:String): AnsiString;
begin
  result := '^RFW,A^FD' + aValue + '^FS';
end;

function TACBrETQZplII.ComandoGravaRFIDHexaDecimal(aValue: String): AnsiString;
begin
  result := '^RFW,H^FD' + aValue+ '^FS';
end;

function TACBrETQZplII.ComandoCoordenadas(aVertical, aHorizontal: Integer
  ): String;
begin
  if (aVertical < 0) or (aVertical > 32000) then
    raise Exception.Create('Vertical deve estar entre 0 e 32000');

  if (aHorizontal < 0) or (aHorizontal > 32000) then
    raise Exception.Create('Horizontal deve estar entre 0 e 32000');

  Result := '^FO' + IntToStr(ConverterUnidade(etqDots, aHorizontal)) + ',' +
                    IntToStr(ConverterUnidade(etqDots, aVertical));
end;

function TACBrETQZplII.ComandoReverso(aImprimirReverso: Boolean): String;
begin
  if aImprimirReverso then
    Result := '^FR'
  else
    Result := '';
end;

function TACBrETQZplII.ComandoLinhaCaixa(aAltura, aLargura, Espessura,
  aCanto: Integer): String;
var
  AlturaDots, LarguraDots, EspessuraDots, AnguloCanto: Integer;
begin
  AlturaDots    := ConverterUnidade(etqDots, aAltura);
  LarguraDots   := ConverterUnidade(etqDots, aLargura);
  EspessuraDots := Max(ConverterUnidade(etqDots, Espessura), 1);
  AnguloCanto   := Min(Max(aCanto, 0), 8);

  Result := '^GB' + IntToStr(LarguraDots)   + ',' +
                    IntToStr(AlturaDots)    + ',' +
                    IntToStr(EspessuraDots) + ',' +
                    'B'                     + ',' +
                    IntToStr(AnguloCanto)   +
            '^FS';
end;

function TACBrETQZplII.AjustarNomeArquivoImagem(const aNomeImagem: String;
  var aTipo: String): String;
var
  p: Integer;
  aNome: String;
begin
  p := PosLast('.', aNomeImagem);
  if (p > 0) then
  begin
    aTipo := copy(aNomeImagem, p+1, Length(aNomeImagem));
    aTipo := UpperCase(LeftStr(aTipo, 3));
    aNome := copy(aNomeImagem, 1, p-1);
  end
  else
  begin
    aNome := aNomeImagem;
    if (aTipo = '') then
      aTipo := 'GRF';
  end;

  if (aTipo = 'BMP') then
    aTipo := 'GRF';

  Result := UpperCase(LeftStr(OnlyAlphaNum(aNome), 8)) + '.' + aTipo;
end;

function TACBrETQZplII.ConverterMultiplicadorImagem(aMultiplicador: Integer): String;
begin
  aMultiplicador := max(aMultiplicador,1);
  if (aMultiplicador > 10) then
    raise Exception.Create('Multiplicador Imagem deve ser de 1 a 10');

  Result := IntToStr(aMultiplicador);
end;

function TACBrETQZplII.GetDriverImagens: String;
begin
  if LimparMemoria then
    Result := 'R:'
  else
    Result := 'E:';
end;

function TACBrETQZplII.ConverterPaginaDeCodigo(
  aPaginaDeCodigo: TACBrETQPaginaCodigo): String;
begin
  case aPaginaDeCodigo of
    pce437 : Result := '0';
    pce850, pce852, pce860 : Result := '13';
    pce1250, pce1252: Result := '27';
  else
    Result := '';
  end;
end;

function TACBrETQZplII.ConverterExibeCodigo(
  aExibeCodigo: TACBrETQBarraExibeCodigo): String;
begin
  if (aExibeCodigo = becSIM) then
    Result := 'Y'
  else
    Result := 'N';
end;

function TACBrETQZplII.ConverterOrientacao(aOrientacao: TACBrETQOrientacao
  ): String;
begin
  case aOrientacao of
    or270: Result := 'B';  // 270
    or180: Result := 'I';  // 180
    or90:  Result := 'R';  // 90
  else
    Result := 'N';  // Normal
  end;
end;

function TACBrETQZplII.ComandoBarras(const aTipo: String;
  aOrientacao: TACBrETQOrientacao; aAlturaBarras: Integer;
  aExibeCodigo: TACBrETQBarraExibeCodigo): String;
var
  cTipo: Char;
begin
  cTipo := PadLeft(aTipo,1)[1];
  if not CharInSet(cTipo, ['0'..'9','A'..'Z']) then
    raise Exception.Create('Tipo Cod.Barras deve estar "0" a "9" ou "A" a "Z"');

  Result := '^B' + aTipo +
            ConverterOrientacao(aOrientacao)                   + ',' +
            IntToStr(ConverterUnidade(etqDots, aAlturaBarras)) + ',' +
            ConverterExibeCodigo(aExibeCodigo)                 + ',' +
            'N';
end;

function TACBrETQZplII.ComandoAbertura: AnsiString;
begin
  Result := '^XA';
end;

function TACBrETQZplII.ComandoGuilhotina: AnsiString;
var
  m: Char;
begin
  if Guilhotina then
    m := 'C'
  else
    m := 'T';

  Result := '^MM'+m
end;

function TACBrETQZplII.ComandoUnidade: AnsiString;
//var
//  a: Char;
begin
  //case Unidade of
  //  etqDots       : a := 'D';
  //  etqPolegadas  : a := 'I';
  //else
  //  a := 'M';
  //end;
  //
  //Result := '^MU'+d;
  Result := '';  // Todos os comandos são convertidos para etqDots;
end;

function TACBrETQZplII.ComandoTamanhoBarras(aBarraFina, aBarraLargaa , aAlturaBarra:Integer): String;
begin
  result := '^BY' + intToStr( aBarraFina )+ ',,'+ intToStr( aAlturaBarra );
end;

function TACBrETQZplII.ComandoTemperatura: AnsiString;
begin
  if (Temperatura < 0) or (Temperatura > 30) then
    raise Exception.Create('Temperatura deve ser de 0 a 30');

  Result := '~SD' + IntToStrZero(Temperatura, 2);
end;

function TACBrETQZplII.ComandoPaginaDeCodigo: AnsiString;
var
  APagCod: String;
begin
  APagCod := ConverterPaginaDeCodigo(PaginaDeCodigo);
  if (APagCod <> '') then
    Result := '^CI' + APagCod
  else
    Result := '';
end;

function TACBrETQZplII.ComandoOrigemCoordenadas: AnsiString;
begin
  if (Origem = ogBottom) then
    Result := '^POI'
  else
    Result := '^PON';
end;

function TACBrETQZplII.ComandoResolucao: AnsiString;
//var
//  aCmdRes: Char;
begin
  //if (DPI = dpi600) then
  //  aCmdRes := 'A'  // A = 24 dots/mm, 12 dots/mm, 8 dots/mm or 6 dots/mm
  //else
  //  aCmdRes := 'B';
  //
  //Result := '^JM'+aCmdRes+'^FS';
  Result := '';  // Usa a resolução definida na Impressora (configure ACBrETQ.DPI de acordo com a impressora)
end;

function TACBrETQZplII.ComandoVelocidade: AnsiString;
begin
  if (Velocidade > 12) then
    raise Exception.Create('Velocidade deve ser de 2 a 12 ');

  if (Velocidade > 0) then
    Result := '^PR' + IntToStr(Max(Velocidade,2))
  else
    Result := EmptyStr;
end;

function TACBrETQZplII.ComandoBackFeed: AnsiString;
begin
  case BackFeed of
    bfOn : Result := '~JSA';
    bfOff: Result := '~JSO';
  else
    Result := EmptyStr;
  end;
end;

function TACBrETQZplII.ComandoCopias(const NumCopias: Integer): AnsiString;
begin
  inherited ComandoCopias(NumCopias);
  Result := '^PQ' + IntToStr(Max(NumCopias,1));
end;

function TACBrETQZplII.ComandoImprimir: AnsiString;
begin
  Result := '^XZ';
end;

function TACBrETQZplII.ComandoAvancarEtiqueta(const aAvancoEtq: Integer
  ): AnsiString;
begin
  if (aAvancoEtq > 0) then
    Result := '^PH'
  else
    Result := EmptyStr;
end;

function TACBrETQZplII.TratarComandoAntesDeEnviar(const aCmd: AnsiString): AnsiString;
begin
  Result := ChangeLineBreak( aCmd, CRLF );
end;

function TACBrETQZplII.ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao;
  aFonte: String; aMultHorizontal, aMultVertical, aVertical,
  aHorizontal: Integer; aTexto: String; aSubFonte: Integer;
  aImprimirReverso: Boolean): AnsiString;
begin
  if (Length(aTexto) > 255) then
    raise Exception.Create(ACBrStr('Tamanho máximo para o texto 255 caracteres'));

  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            ComandoFonte(aFonte, aMultVertical, aMultHorizontal, aOrientacao) +
            ComandoReverso(aImprimirReverso) +
            ComandoCor +
            ComandoCampo(aTexto);
end;

function TACBrETQZplII.ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra
  ): String;
begin
  case TipoBarras of
    barEAN13      : Result := 'E';
    barEAN8       : Result := '8';
    barSTANDARD   : Result := 'J';
    barINTERLEAVED: Result := '2';
    barCODE128    : Result := 'C';
    barCODE39     : Result := '3';
    barCODE93     : Result := 'A';
    barUPCA       : Result := 'U';
    barCODABAR    : Result := 'K';
    barMSI        : Result := 'M';
    barCODE11     : Result := '1';
  else
    Result := '';
  end;
end;

function TACBrETQZplII.ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao;
  aTipoBarras: String; aBarraLarga, aBarraFina, aVertical,
  aHorizontal: Integer; aTexto: String; aAlturaBarras: Integer;
  aExibeCodigo: TACBrETQBarraExibeCodigo): AnsiString;
begin
  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            ComandoCor +
            ComandoTamanhoBarras(aBarraFina, aBarraLarga , aAlturaBarras ) +
            ComandoBarras(aTipoBarras, aOrientacao, aAlturaBarras, aExibeCodigo ) +
            ComandoCampo(aTexto);
end;

function TACBrETQZplII.ComandoImprimirQRCode(aVertical, aHorizontal: Integer;
  const aTexto: String; aLarguraModulo: Integer; aErrorLevel: Integer;
  aTipo: Integer): AnsiString;
begin
  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            ComandoCor +
            '^BQ'+
            ConverterOrientacao(orNormal) + ',' +
            IntToStr(aTipo) + ',' +
            IntToStr(aLarguraModulo) + ',' +
            ComandoCampo( ConverterQRCodeErrorLevel(aErrorLevel) +'A,'+ aTexto);
end;

function TACBrETQZplII.ComandoImprimirLinha(aVertical, aHorizontal, aLargura,
  aAltura: Integer): AnsiString;
begin
  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            ComandoCor +
            ComandoLinhaCaixa(aAltura, aLargura, Min(aAltura, aLargura), 0);
end;

function TACBrETQZplII.ComandoImprimirCaixa(aVertical, aHorizontal, aLargura,
  aAltura, aEspVertical, aEspHorizontal: Integer; aCanto: Integer): AnsiString;
begin
  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            ComandoCor +
            ComandoLinhaCaixa(aAltura, aLargura, Max(aEspVertical, aEspHorizontal), aCanto)+
            '^FS';
end;

function TACBrETQZplII.ComandoImprimirImagem(aMultImagem, aVertical,
  aHorizontal: Integer; aNomeImagem: String): AnsiString;
var
  ATipo: String;
begin
  ATipo := '';
  aNomeImagem := AjustarNomeArquivoImagem(aNomeImagem, ATipo);
  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            '^XG' + GetDriverImagens +
                    aNomeImagem + ',' +
                    ConverterMultiplicadorImagem(aMultImagem) + ',' +
                    ConverterMultiplicadorImagem(aMultImagem) +
            '^FS';
end;

function TACBrETQZplII.ComandoCarregarImagem(aStream: TStream;
  var aNomeImagem: String; aFlipped: Boolean; aTipo: String): AnsiString;
var
  Format, Extension: String;
  ImgData: AnsiString;
  Len: Integer;
begin
  aNomeImagem := AjustarNomeArquivoImagem(aNomeImagem, aTipo);
  if (pos(aTipo,'PCX,BMP,GRF,PNG') = 0) then
    raise Exception.Create(ACBrStr('Formato de Imagem deve ser: PNG, BMP, GRF ou PCX e Monocromática'));

  aStream.Position := 0;
  VerificarConteudoETipoImagemMono(aStream, aTipo);

  if (aTipo = 'GRF') then  // BMP = GRF
    Result := ComandoBMP2GRF(aStream, aNomeImagem)
  else
  begin
    ImgData := ReadStrFromStream(aStream, aStream.Size);
    Len := Length(ImgData);

    if (aTipo = 'PNG') then
    begin
      Format := 'P';
      Extension := 'P';
      ImgData := EncodeBase64(ImgData);
      ImgData := ':B64:'+ImgData+':'+CrcZB64(ImgData);
      Len := Len+6;
    end
    else   // PCX
    begin
      Format := 'B';
      Extension := 'X';
    end;

    Result := '~DY'+                // Download Graphics command
              GetDriverImagens +    // File Location
              aNomeImagem + ',' +   // Filename
              Format + ',' +        // Format - A = uncompressed (ZB64, ASCII), B = uncompressed (binary), C = AR-compressed (used only by Zebra’s BAR-ONE® v5), P = portable network graphic (.PNG) - ZB64 encoded
              Extension + ',' +     // Extension - B = bitmap, G = raw bitmap (.GRF), P = store as compressed (.PNG), T = TrueType (.TTF) or X = Paintbrush (.PCX)
              IntToStr(Len) + ',' + // Bytes total
              '0,' +                // Bytes per Row (BMP only)
              ImgData;
  end;

  Result := '^ID' + GetDriverImagens + aNomeImagem + '^FS' +  // Apaga a imagem existente com o mesmo nome
            Result;
end;

function TACBrETQZplII.ComandoApagarImagem(const NomeImagem: String): String;
var
  s, t: String;
begin
  if (NomeImagem = '*') then
    Result := '^ID' + GetDriverImagens + '*.*^FS'
  else
  begin
    t := '';
    s := AjustarNomeArquivoImagem(NomeImagem, t);
    Result := '^ID' + GetDriverImagens + s + '^FS';
  end;

  if not TACBrETQ(fpOwner).EtqInicializada then
    Result := ComandoAbertura +
              Result +
              ComandoImprimir;
end;

// Fonte: https://github.com/asharif/img2grf/blob/master/src/main/java/org/orphanware/App.java
function TACBrETQZplII.ComandoBMP2GRF(aStream: TStream; aNomeImagem: String;
  Inverter: Boolean): AnsiString;
var
  ARasterImg: AnsiString;
  AHeight, AWidth, LenImg, BytesPerRow: Integer;
  ImgHex: String;
begin
  AWidth := 0; AHeight := 0; ARasterImg := '';
  BMPMonoToRasterStr(aStream, Inverter, AWidth, AHeight, ARasterImg);

  LenImg := Length(ARasterImg);
  ImgHex := AsciiToHex(ARasterImg);
  BytesPerRow := ceil(AWidth / 8);

  Result := '~DG' + GetDriverImagens + aNomeImagem + ',' + IntToStr(LenImg)+ ',' +
            IntToStr(BytesPerRow) + ',' + ImgHex;
end;

function TACBrETQZplII.CrcZB64(const AString: AnsiString): String;
var
  crc: Word;
begin
  crc := StringCrcCCITT(AString, $0000);
  Result := IntToHex(crc, 4);
end;

end.

