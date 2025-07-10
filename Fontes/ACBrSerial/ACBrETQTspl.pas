{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Daniel de Morais InfoCotidiano                }
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

unit ACBrETQTspl;

interface

uses
  Classes,
  ACBrETQClass, ACBrDevice
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrETQTspl }

  TACBrETQTspl = class(TACBrETQClass)
  private
    function ConverterOrientacao(aOrientacao: TACBrETQOrientacao): String; // Ok
    function ConverterMultiplicador(aMultiplicador: Integer): String; //ok
    function ConverterCoordenadas(aVertical, aHorizontal: Integer): String;
    function ConverterFonte(const aFonte: String; var aTexto: String): String; //ok
    function ConverterReverso(aImprimirReverso: Boolean; aHorizontal, aVertical, aLarguraTexto, aAlturaTexto: integer): String;
    function ConverterLarguraBarras(aBarraLarga, aBarraFina: Integer): String;
    function ConverterUnidadeAlturaBarras(aAlturaBarras: Integer): String;
    function ConverterPaginaDeCodigo(aPaginaDeCodigo: TACBrETQPaginaCodigo): String; //ok

    function FormatarTexto(const aTexto: String): String; //ok

    procedure VerificarTipoBarras(const aTipo: String; aBarraFina: Integer); // ok I.A.

    function ConverterExibeCodigo(aExibeCodigo: TACBrETQBarraExibeCodigo): String; // ok

    function CalcularEspessuraLinha(aVertical, aHorizontal: Integer): String;

    function AjustarNomeArquivoImagem( const aNomeImagem: String): String;

    function AlturaFonteEmDots(const Fonte: String; Mult: Integer): Integer;
    function ComprimentoTextoEmDots(const Texto, Fonte: String;
      Mult: Integer): Integer;

  public
    constructor Create(AOwner: TComponent);

    function ComandoLimparMemoria: AnsiString; override; //ok
    function ComandoAbertura: AnsiString; override; // ok
    function ComandoGuilhotina: AnsiString; override; // ok
    function ComandoBackFeed: AnsiString; override; //ok
    function ComandoTemperatura: AnsiString; override; //ok
    function ComandoPaginaDeCodigo: AnsiString; override; //ok
    function ComandoOrigemCoordenadas: AnsiString; override; //ok
    function ComandoVelocidade: AnsiString; override; //ok

    function ComandoCopias(const NumCopias: Integer): AnsiString; override; //ok

    function ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao; aFonte: String;
      aMultHorizontal, aMultVertical, aVertical, aHorizontal: Integer; aTexto: String;
      aSubFonte: Integer = 0; aImprimirReverso: Boolean = False): AnsiString; override; //ok

    function ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra): String; override;

    function ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao; aTipoBarras: String; //ok
      aBarraLarga, aBarraFina, aVertical, aHorizontal: Integer; aTexto: String;
      aAlturaBarras: Integer; aExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao
      ): AnsiString; override;

    function ComandoImprimirQRCode(aVertical, aHorizontal: Integer; // ok
      const aTexto: String; aLarguraModulo: Integer; aErrorLevel: Integer;
      aTipo: Integer): AnsiString; override;

    function ComandoImprimirLinha(aVertical, aHorizontal, aLargura, aAltura: Integer //ok
      ): AnsiString; override;

    function ComandoImprimirCaixa(aVertical, aHorizontal, aLargura, aAltura, //ok
      aEspVertical, aEspHorizontal: Integer; aCanto: Integer = 0): AnsiString; override;

    function ComandoImprimirImagem(aMultImagem, aVertical, aHorizontal: Integer; //ok
      aNomeImagem: String): AnsiString; override;
    function ComandoCarregarImagem(aStream: TStream; var aNomeImagem: String; //ok
      aFlipped: Boolean; aTipo: String): AnsiString; override;
    function ComandoApagarImagem(const NomeImagem: String = '*'): String; override; //ok
  end;

implementation

uses
  math, SysUtils,
  {$IFDEF COMPILER6_UP} StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
  ACBrImage, ACBrConsts, synautil,
  ACBrUtil.Compatibilidade, ACBrUtil.Strings, ACBrUtil.Math;

{ TACBrETQTspl }

constructor TACBrETQTspl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Unidade := etqMilimetros;

  fpModeloStr    := 'TSPL';
  fpLimiteCopias := 65535;
end;

function TACBrETQTspl.FormatarTexto(const aTexto: String): String;
begin
  Result := aTexto;

  if (LeftStr(Result, 1) = '"') then  // Usuário já faz os ajustes ?
    Exit;

  // Ajustando o TEXTO com caraceteres especiais //
  Result := StringReplace(Result, '\', '\\"', [rfReplaceAll]);

  {If there is any double quote (") within the text, please change it to \["].}
  Result := StringReplace(aTexto, '"',  '\["]' ,[rfReplaceAll, rfIgnoreCase]);

  //epl Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);

  Result := '"' + Result + '"';
end;

function TACBrETQTspl.ConverterFonte(const aFonte: String; var aTexto: String
  ): String;
var
  cFonte: Char;
begin
  {Nem todas as fontes estão disponíveis para todos modelos,
  vide manual pagina 65}
  cFonte := PadLeft(aFonte,1,'3')[1];

  if not CharInSet(cFonte, ['0'..'8']) then
    raise Exception.Create('Fonte deve ser de "0" a "8"');

  {O pitch da fonte interna (fontes #1 a #5) entre TSPL e TSPL2 é diferente.}

  Result := cFonte;
end;

function TACBrETQTspl.ConverterMultiplicador(aMultiplicador: Integer): String;
begin
  if (aMultiplicador < 1) or (aMultiplicador > 10) then
    raise Exception.Create('Multiplicador deve ser de 1 a 10');

  Result := IntToStr(aMultiplicador);
end;

function TACBrETQTspl.ConverterCoordenadas(aVertical, aHorizontal: Integer
  ): String;
begin
  Result := IntToStr(ConverterUnidade(etqDots, aHorizontal)) + ',' +
            IntToStr(ConverterUnidade(etqDots, aVertical));
end;

function TACBrETQTspl.CalcularEspessuraLinha(aVertical, aHorizontal: Integer
  ): String;
var
  wEspessura: Integer;
begin
  wEspessura := Max(aHorizontal, aVertical);
  wEspessura := ConverterUnidade(etqDots, wEspessura);

  Result := IntToStr(max(wEspessura,1));
end;

function TACBrETQTspl.AjustarNomeArquivoImagem(const aNomeImagem: String): String;
begin
  Result := UpperCase(LeftStr(OnlyAlphaNum(aNomeImagem), 8));
end;

function TACBrETQTspl.ConverterUnidadeAlturaBarras(aAlturaBarras: Integer
  ): String;
begin
  Result := IntToStr(ConverterUnidade(etqDots, aAlturaBarras));
end;

function TACBrETQTspl.ConverterPaginaDeCodigo(
  aPaginaDeCodigo: TACBrETQPaginaCodigo): String;
begin
  case aPaginaDeCodigo of
    pce437 : Result := '437';
    pce850 : Result := '850';
    pce852 : Result := '852';
    pce860 : Result := '860';
    pce1250: Result := '1250';
    pce1252: Result := '1252';
  else
    Result := '';
  end;
end;

function TACBrETQTspl.ConverterOrientacao(aOrientacao: TACBrETQOrientacao
  ): String;
begin
  case aOrientacao of
    or270: Result := '270';  // 270
    or180: Result := '180';  // 180
    or90:  Result := '90';   // 90
  else
    Result := '0';  // Normal
  end;
end;

procedure TACBrETQTspl.VerificarTipoBarras(const aTipo: String; aBarraFina: Integer);
var
  MinBarraFina, MaxBarraFina: Integer;
begin
  MinBarraFina := 1;
  MaxBarraFina := 10;

  //(estreita)narrow: valor de 1 a 10 (representa a largura da barra fina em dots)

  //(larga)wide: valor de 2 a 30 (representa a largura da barra larga em dots)

  // Tipos com faixa mais restrita (2 a 4, exceto 2G que é 3 a 4)
  if (pos(aTipo, 'CODE128;CODE128M;EAN128;CODE39;CODE39C;CODE39S;CODE93;CODE93EXT') > 0) then
  begin
    MinBarraFina := 2;
    MaxBarraFina := 4;
    if aTipo = 'CODE128M' then
      MinBarraFina := 3;
  end;

  if (aBarraFina < MinBarraFina) or (aBarraFina > MaxBarraFina) then
    raise Exception.CreateFmt('Barra Fina para o Cod.Barras %s deve ser de %d a %d',
                              [aTipo, MinBarraFina, MaxBarraFina]);
end;

function TACBrETQTspl.ConverterExibeCodigo(
  aExibeCodigo: TACBrETQBarraExibeCodigo): String;
begin
  if (aExibeCodigo = becSIM) then
    Result := '1'
  else
    Result := '0';
end;

function TACBrETQTspl.ConverterLarguraBarras(aBarraLarga, aBarraFina: Integer): String;
begin
  if (aBarraFina < 1) or (aBarraFina > 10) then
    raise Exception.Create('Barra Fina deve ser de 1 a 10');

  if (aBarraLarga < 2) or (aBarraLarga > 30) then
    raise Exception.Create('Barra Larga deve ser de 2 a 30');

  if (aBarraFina > aBarraLarga) then
    raise Exception.Create('Barra Fina deve ser inferior ou Igual a Barra Larga');

  Result := IntToStr(aBarraFina) + ',' + IntToStr(aBarraLarga);
end;

function TACBrETQTspl.ConverterReverso(aImprimirReverso: Boolean; aHorizontal, aVertical, aLarguraTexto, aAlturaTexto:integer): String;
begin
  If aImprimirReverso then
     begin

      Result := 'REVERSE ' +
            IntToStr(aHorizontal) + ',' +
            IntToStr(aVertical) + ',' +
            IntToStr(aLarguraTexto) + ',' +
            IntToStr(aAlturaTexto) + LF;
     end;
end;

function TACBrETQTspl.ComandoOrigemCoordenadas: AnsiString;
begin
  if (Origem = ogTop) then
    Result := 'DIRECTION 0'
  else
    Result := 'DIRECTION 1';  // ACBr Default
end;

function TACBrETQTspl.ComandoLimparMemoria: AnsiString;
begin
  Result := 'CLS';
end;

function TACBrETQTspl.ComandoTemperatura: AnsiString;
begin
  Result := EmptyStr;

  if (Temperatura < 0) or (Temperatura > 15) then
    raise Exception.Create('Temperatura deve ser de 0 a 15');

  Result := 'DENSITY ' + IntToStr(Temperatura);
end;

function TACBrETQTspl.ComandoPaginaDeCodigo: AnsiString;
var
  APagCod: String;
begin
  APagCod := ConverterPaginaDeCodigo(PaginaDeCodigo);
  if (APagCod <> '') then
    Result := 'CODEPAGE '+APagCod
  else
    Result := '';
end;

function TACBrETQTspl.ComandoVelocidade: AnsiString;
begin
  Result := EmptyStr;
  if Velocidade < 0 then
     Exit;

  if (Velocidade > 6) then
    raise Exception.Create('Velocidade deve ser de 0 a 6');

  Result := 'SPEED ' + IntToStr(Velocidade);
end;

function TACBrETQTspl.ComandoCopias(const NumCopias: Integer): AnsiString;
begin
  inherited ComandoCopias(NumCopias);
  Result := 'PRINT ' + IntToStr(NumCopias);
end;

function TACBrETQTspl.ComandoBackFeed: AnsiString;
begin
  case BackFeed of
    bfOn : Result := 'SET BACK ON';
    bfOff: Result := 'SET BACK OFF';
  else
    Result := EmptyStr;
  end;
end;

function TACBrETQTspl.ComandoAbertura: AnsiString;
begin
  Result := 'AUTODETECT';
  //+sLineBreak+'REFERENCE 0,0';
end;

function TACBrETQTspl.ComandoGuilhotina: AnsiString;
begin
  if Guilhotina then
    Result := 'SET CUTTER BATCH'  // Set printer to cut label at the end of printing job.
  else
    Result := 'SET CUTTER OFF';  // Disable cutter function
end;

function TACBrETQTspl.ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao;
  aFonte: String; aMultHorizontal, aMultVertical, aVertical,
  aHorizontal: Integer; aTexto: String; aSubFonte: Integer;
  aImprimirReverso: Boolean): AnsiString;

var
  LLarguraTexto, LAlturaTexto: Integer;
begin

  // Calcular altura e largura estimadas do texto com base na fonte e multiplicadores
  LLarguraTexto := ComprimentoTextoEmDots(aTexto, aFonte, aMultHorizontal);
  LAlturaTexto  := AlturaFonteEmDots(aFonte, aMultVertical);


  Result := ConverterReverso(aImprimirReverso, aHorizontal, aVertical,LLarguraTexto,LAlturaTexto) + 'TEXT ' +
            ConverterCoordenadas(aVertical, aHorizontal) + ',' +
            '"'+ConverterFonte(aFonte, aTexto)+'"'       + ',' +
            ConverterOrientacao(aOrientacao)             + ',' +
            ConverterMultiplicador(aMultHorizontal)      + ',' +
            ConverterMultiplicador(aMultVertical)        + ',' +
            FormatarTexto(aTexto);
end;

function TACBrETQTspl.ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra
  ): String;
begin
  case TipoBarras of
    barEAN13      : Result := 'EAN13';
    barEAN8       : Result := 'EAN8';
    barINTERLEAVED: Result := '25';
    barCODE128    : Result := '128';
    barCODE39     : Result := '39';
    barCODE93     : Result := '99';
    barUPCA       : Result := 'UPCA';
    barCODABAR    : Result := 'CODA';
    barMSI        : Result := 'MSI';
  else
    Result := '';
  end;
end;

function TACBrETQTspl.ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao;
  aTipoBarras: String; aBarraLarga, aBarraFina, aVertical,
  aHorizontal: Integer; aTexto: String; aAlturaBarras: Integer;
  aExibeCodigo: TACBrETQBarraExibeCodigo): AnsiString;
begin
  VerificarTipoBarras(aTipoBarras, aBarraFina);

//  BARCODE VVV,HHH,BBBB,AL,H,R,E,L
//  BARCODE 100,100,”39”,96,1,0,2,4,”1000”

  Result := 'BARCODE ' +                                              // Barcode
            ConverterCoordenadas(aVertical, aHorizontal)    + ',' +   // Coordenadas Horizontal x Vertical
            '"'+aTipoBarras+'"'                             + ',' +   // Tipo Barras
            ConverterUnidadeAlturaBarras(aAlturaBarras)     + ',' +   // Altura
            ConverterExibeCodigo(aExibeCodigo)              + ',' +   // Exibe Codigo (human readable)
            ConverterOrientacao(aOrientacao)                + ',' +   // Rotacao
            ConverterLarguraBarras(aBarraLarga, aBarraFina) + ',' +   // Barra fina, Barra Larga
            FormatarTexto(aTexto);                                    // Texto
end;

function TACBrETQTspl.ComandoImprimirQRCode(aVertical, aHorizontal: Integer;
  const aTexto: String; aLarguraModulo: Integer; aErrorLevel: Integer;
  aTipo: Integer): AnsiString;
begin
 // QRCODE X  , Y, ECC Level, cell width, mode, rotation, [model, mask,]"Data string"
 // QRCODE 100,10, L        ,7          ,M    ,0          ,M1   ,S1    ,"ATHE FIRMWARE HAS BEEN UPDATED"
  result := 'QRCODE ' +                                          // qrcode
            ConverterCoordenadas(aVertical, aHorizontal) + ',' + //  X, Y,
            '7' + ',' +                                          // ECC Level
            IntToStr(aLarguraModulo) + ',' +                     //c ell width
            'A' + ',' +                                          // mode  A = Auto / manual encode
            '0' + ',' +                                          // rotation
            FormatarTexto(aTexto);                               // text


end;

function TACBrETQTspl.ComandoImprimirLinha(aVertical, aHorizontal, aLargura,
  aAltura: Integer): AnsiString;
begin
  Result := 'BAR' +
            ConverterCoordenadas(aVertical, aHorizontal) + ',' +
            ConverterCoordenadas(aAltura, aLargura);
end;

function TACBrETQTspl.ComandoImprimirCaixa(aVertical, aHorizontal, aLargura,
  aAltura, aEspVertical, aEspHorizontal: Integer; aCanto: Integer): AnsiString;
begin
  Result := 'BOX ' +
            ConverterCoordenadas(aVertical, aHorizontal)     + ',' +
            ConverterCoordenadas(aVertical+aAltura, aHorizontal+aLargura)  + ',' +
            CalcularEspessuraLinha(aEspVertical, aEspHorizontal);
end;

function TACBrETQTspl.ComandoImprimirImagem(aMultImagem, aVertical,
  aHorizontal: Integer; aNomeImagem: String): AnsiString;
begin
  // PUTPCX X,Y,"filename
  Result := 'PUTPCX' +
            ConverterCoordenadas(aVertical, aHorizontal) + ',' +
            '"' + AjustarNomeArquivoImagem(aNomeImagem)+ '"';
end;

function TACBrETQTspl.ComandoCarregarImagem(aStream: TStream;
  var aNomeImagem: String; aFlipped: Boolean; aTipo: String): AnsiString;
var
  ImgData: AnsiString;
begin
  Result := EmptyStr;

  if (aTipo = '') then
     aTipo := 'PCX'
  else
    aTipo := UpperCase(RightStr(aTipo, 3));

  if (aTipo <> 'PCX') or (not IsPCX(aStream, True)) then
    raise Exception.Create(ACBrStr(cErrImgNotPCXMono));

  aStream.Position := 0;
  ImgData := ReadStrFromStream(aStream, aStream.Size);
  aNomeImagem := AjustarNomeArquivoImagem(aNomeImagem);

  Result := 'KILL F,"' + aNomeImagem + '"' + LF +      // Delete the specify file in FLASH.
            'DOWNLOAD F,"' + aNomeImagem + '",' + IntToStr(aStream.Size) + ',' + ImgData + LF;
end;

function TACBrETQTspl.ComandoApagarImagem(const NomeImagem: String): String;
var
  LNomeImagem: String;
begin
  if (NomeImagem = '*') then
  begin
    Result := 'KILL F,"*"' + LF;
  end
  else
  begin
    LNomeImagem := AjustarNomeArquivoImagem(NomeImagem);
    Result := 'KILL F,"' + LNomeImagem + '"' + LF;
  end;
end;

function TACBrETQTspl.ComprimentoTextoEmDots(const Texto, Fonte: String; Mult: Integer): Integer;
begin
  Result := Length(Texto) * 8 * Mult; // 8 dots por caractere base
end;

function TACBrETQTspl.AlturaFonteEmDots(const Fonte: String; Mult: Integer): Integer;
begin
  Result := 16 * Mult; // 16 dots de altura como base (ajuste conforme fonte)
end;

end.
