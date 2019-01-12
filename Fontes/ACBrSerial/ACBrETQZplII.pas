{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2007 Andrews Ricardo Bejatto                }
{                                       Anderson Rogerio Bejatto               }
{                                                                              }
{ Colaboradores nesse arquivo:          Daniel Simooes de Almeida              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrETQZplII;

interface

uses
  Classes,
  ACBrETQClass, ACBrDevice;

type

  { TACBrETQZplII }

  TACBrETQZplII = class(TACBrETQClass)
  private
    FImagensPCX: String;

    function ComandoCampo(const aTexto: String): String;

    function ConverterOrientacao(aOrientacao: TACBrETQOrientacao): String;

    function ComandoCoordenadas(aVertical, aHorizontal: Integer): String;
    function ComandoReverso(aImprimirReverso: Boolean): String;
    function ComandoFonte(const aFonte: String; aMultVertical, aMultHorizontal: Integer;
      aOrientacao: TACBrETQOrientacao): String;

    function ComandoBarras(const aTipo: String; aOrientacao: TACBrETQOrientacao;
      aAlturaBarras: Integer; aExibeCodigo: TACBrETQBarraExibeCodigo): String;
    function ConverterExibeCodigo(aExibeCodigo: TACBrETQBarraExibeCodigo): String;

    function ComandoLinhaCaixa(aAltura, aLargura, Espessura: Integer): String;
    function AjustarNomeArquivoImagem( const aNomeImagem: String): String;
    function ConverterMultiplicadorImagem(aMultiplicador: Integer): String;
  protected
    function ComandoAbertura: AnsiString; override;
    function ComandoUnidade: AnsiString; override;
    function ComandoTemperatura: AnsiString; override;
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

    function ComandoImprimirLinha(aVertical, aHorizontal, aLargura, aAltura: Integer
      ): AnsiString; override;

    function ComandoImprimirCaixa(aVertical, aHorizontal, aLargura, aAltura, aEspVertical,
      aEspHorizontal: Integer): AnsiString; override;

    function ComandoImprimirImagem(aMultImagem, aVertical, aHorizontal: Integer;
      aNomeImagem: String): AnsiString; override;
    function ComandoCarregarImagem(aStream: TStream; aNomeImagem: String;
      aFlipped: Boolean; aTipo: String): AnsiString; override;
    function ComandoBMP2GRF(aStream: TStream; aNomeImagem: String; Inverter: Boolean = True): AnsiString;
  end;

implementation

uses
  math, {$IFNDEF COMPILER6_UP} ACBrD5, Windows, {$ENDIF} sysutils, strutils,
  synautil,
  ACBrUtil, ACBrConsts;

{ TACBrETQPpla }

constructor TACBrETQZplII.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'ZPLII';
  fpLimiteCopias := 999;
  FImagensPCX := '';
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

function TACBrETQZplII.ComandoLinhaCaixa(aAltura, aLargura, Espessura: Integer
  ): String;
var
  AlturaDots, LarguraDots, EspessuraDots: Integer;
begin
  AlturaDots    := ConverterUnidade(etqDots, aAltura);
  LarguraDots   := ConverterUnidade(etqDots, aLargura);
  EspessuraDots := Max(ConverterUnidade(etqDots, Espessura), 1);

  Result := '^GB' + IntToStr(LarguraDots)   + ',' +
                    IntToStr(AlturaDots)    + ',' +
                    IntToStr(EspessuraDots) + ',' +
                    'B'                     + ',' +
                    '0'                     +
            '^FS';
end;

function TACBrETQZplII.AjustarNomeArquivoImagem(const aNomeImagem: String): String;
begin
  Result := UpperCase(LeftStr(OnlyAlphaNum(aNomeImagem), 8));
end;

function TACBrETQZplII.ConverterMultiplicadorImagem(aMultiplicador: Integer
  ): String;
begin
  aMultiplicador := max(aMultiplicador,1);
  if (aMultiplicador > 10) then
    raise Exception.Create('Multiplicador Imagem deve ser de 1 a 10');

  Result := IntToStr(aMultiplicador);
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

function TACBrETQZplII.ComandoTemperatura: AnsiString;
begin
  if (Temperatura < 0) or (Temperatura > 30) then
    raise Exception.Create('Temperatura deve ser de 0 a 30');

  Result := '~SD' + IntToStrZero(Temperatura, 2);
end;

function TACBrETQZplII.ComandoOrigemCoordenadas: AnsiString;
begin
  if (fpOrigem = ogBottom) then
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
  case fpBackFeed of
    bfOn : Result := '~JSA';
    bfOff: Result := '~JSO';
  else
    Result := EmptyStr;
  end;
end;

function TACBrETQZplII.ComandoCopias(const NumCopias: Integer): AnsiString;
begin
  Result := EmptyStr;
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
            ComandoBarras(aTipoBarras, aOrientacao, aAlturaBarras, aExibeCodigo ) +
            ComandoCampo(aTexto);
end;

function TACBrETQZplII.ComandoImprimirLinha(aVertical, aHorizontal, aLargura,
  aAltura: Integer): AnsiString;
begin
  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            ComandoLinhaCaixa(aAltura, aLargura, Min(aAltura, aLargura) );
end;

function TACBrETQZplII.ComandoImprimirCaixa(aVertical, aHorizontal, aLargura,
  aAltura, aEspVertical, aEspHorizontal: Integer): AnsiString;
begin
  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            ComandoLinhaCaixa(aAltura, aLargura, Max(aEspVertical, aEspHorizontal) )+
            '^FS';
end;

function TACBrETQZplII.ComandoImprimirImagem(aMultImagem, aVertical,
  aHorizontal: Integer; aNomeImagem: String): AnsiString;
var
  ANome, ATipo: String;
begin
  ATipo := ExtractFileExt(aNomeImagem);
  if (ATipo <> '') then
  begin
    ATipo := UpperCase(RightStr(ATipo, 3));
    ANome := ExtractFileName(aNomeImagem);
  end
  else
    ANome := aNomeImagem;

  ANome := AjustarNomeArquivoImagem(ANome);

  if (ATipo = '') then
    if (pos(ANome, FImagensPCX) > 0) then
      ATipo := 'PCX';

  if (ATipo <> 'PCX') then
    ATipo := 'GRF';

  Result := ComandoCoordenadas(aVertical, aHorizontal) +
            '^XGE:' + ANome + '.' + ATipo +  ',' +
                      ConverterMultiplicadorImagem(aMultImagem) + ',' +
                      ConverterMultiplicadorImagem(aMultImagem) +
            '^FS';
end;

function TACBrETQZplII.ComandoCarregarImagem(aStream: TStream;
  aNomeImagem: String; aFlipped: Boolean; aTipo: String): AnsiString;
var
  ANome: String;
begin
  if (aTipo = '') then
    aTipo := 'BMP'
  else
    aTipo := UpperCase(RightStr(aTipo, 3));

  if (aTipo = 'PCX') then
  begin
    if not ImgIsPCX(aStream, True) then
      raise Exception.Create(ACBrStr(cErrImgPCXMono));
  end
  else if (aTipo <> 'BMP') then
    raise Exception.Create(ACBrStr(
      'Formato de Imagem deve ser: BMP ou PCX e Monocromática'));

  ANome := AjustarNomeArquivoImagem(aNomeImagem);
  aStream.Position := 0;

  if (aTipo = 'BMP') then
  begin
    ANome := ANome + '.GRF';
    Result := ComandoBMP2GRF(aStream, ANome);
  end
  else
  begin
    FImagensPCX := FImagensPCX + ANome + ',';
    ANome := ANome + '.PCX';
    Result := '~DYE:' + ANome + ',B,X,' +
              IntToStr(aStream.Size)   + ',0,' +
              ReadStrFromStream(aStream, aStream.Size);
  end;

  Result := '^IDE:' + ANome + '^FS' +  // Apaga a imagem existente com o mesmo nome
            Result;
end;

// Fonte: https://github.com/asharif/img2grf/blob/master/src/main/java/org/orphanware/App.java
function TACBrETQZplII.ComandoBMP2GRF(aStream: TStream; aNomeImagem: String;
  Inverter: Boolean): AnsiString;
var
  bWidth, bHeight, bPixelOffset: LongWord;
  bPixel: Byte;
  i, j, tmp, byteW, LenImg{, lbCount, k}: Int64;
  ImgHex: String;
begin
  if not ImgIsBMP(aStream, True) then
    raise Exception.Create(ACBrStr(cErrImgBMPMono));

  // Lendo posição do Off-set da imagem
  AStream.Position := 10;
  bPixelOffset := 0;
  AStream.ReadBuffer(bPixelOffset, 4);

  // Lendo dimensões da imagem
  AStream.Position := 18;
  bWidth := 0; bHeight := 0;
  AStream.ReadBuffer(bWidth, 4);
  AStream.ReadBuffer(bHeight, 4);

  LenImg := aStream.Size - bPixelOffset;
  byteW := ceil(bWidth / 8);
  //lbCount := ceil(bWidth / 4);

  ImgHex := '';
  //k := 0;
  i := aStream.Size-1;
  while (i >= bPixelOffset) do
  begin
    tmp := i - (byteW-1);
    j := tmp;
    aStream.Position := tmp;
    while (j < (tmp + byteW)) do
    begin
      bPixel := 0;
      aStream.ReadBuffer(bPixel,1);
      if Inverter then
        bPixel := bPixel xor $FF;

      //if ((k mod lbCount) = 0) then
      //  ImgHex := ImgHex + LF;

      ImgHex := ImgHex + IntToHex(bPixel,2);

      inc(j);
      //inc(k);
    end;
    i := tmp-1;
  end;

  //DEBUG
  //WriteToFile('c:\temp\ImgHex.txt',ImgHex);

  Result := '~DGE:' + aNomeImagem + ',' + IntToStr(LenImg)+ ',' +
            IntToStr(byteW) + ',' + ImgHex;
end;

end.

