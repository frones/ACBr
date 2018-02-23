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

unit ACBrETQPpla;

interface

uses
  Classes,
  ACBrETQClass, ACBrDevice;

type

  { TACBrETQPpla }

  TACBrETQPpla = class(TACBrETQClass)
  private
    function AjustarTipoBarras(aTipo: String; aExibeCodigo: TACBrETQBarraExibeCodigo): String;
    function ConverterMultiplicador(aMultiplicador: Integer): String;
    function ConverterCoordenadas(aVertical, aHorizontal: Integer): String;

    function ConverterUnidade(AValue: Integer): Integer; reintroduce; overload;
    function ConverterOrientacao(aOrientacao: TACBrETQOrientacao): String;
    function ConverterSubFonte(aFonte: String; aSubFonte: Integer): String;
    function ConverterAlturaBarras(aAlturaBarras: Integer): String;

    function ComandoReverso(aImprimirReverso: Boolean): String;
    function PrefixoComandoLinhaECaixa(aOrientacao: TACBrETQOrientacao): String;
    function ConverterDimensao(aAltura, aLargura: Integer): String;

    function AjustarNomeArquivoImagem( aNomeImagem: String): String;

    function ConverterVelocidade(Velocidade: Integer): Char;

    function ComandoTipoImagem(aNomeImagem: String; aFlipped: Boolean; aTipo: String): String;
    function ConverterMultiplicadorImagem(aMultiplicador: Integer): String;

    function ConverterEspessura(aVertical, aHorizontal: Integer): String;

  protected
    function ComandoAbertura: AnsiString; override;
    function ComandoUnidade: AnsiString; override;
    function ComandoTemperatura: AnsiString; override;
    function ComandoResolucao: AnsiString; override;
    function ComandoVelocidade: AnsiString; override;

  public
    constructor Create(AOwner: TComponent);

    function TratarComandoAntesDeEnviar(aCmd: AnsiString): AnsiString; override;

    function ComandoLimparMemoria: AnsiString; override;
    function ComandoCopias(const NumCopias: Integer): AnsiString; override;
    function ComandoImprimir: AnsiString; override;
    function ComandoAvancarEtiqueta(const aAvancoEtq: Integer): AnsiString; override;

    function ComandosFinalizarEtiqueta(NumCopias: Integer = 1; aAvancoEtq: Integer = 0): AnsiString; override;

    function ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao; aFonte: String;
      aMultHorizontal, aMultVertical, aVertical, aHorizontal: Integer; aTexto: String;
      aSubFonte: Integer = 0; aImprimirReverso: Boolean = False): AnsiString; override;

    function ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra): String; override;
    function ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao; aTipoBarras: String;
      aBarraLarga, aBarraFina, aVertical, aHorizontal: Integer; aTexto: String;
      aAlturaBarras: Integer; aExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao
      ): AnsiString; override;

    function ComandoImprimirLinha(aVertical, aHorizontal, aLargura,
      aAltura: Integer): AnsiString; override;

    function ComandoImprimirCaixa(aVertical, aHorizontal, aLargura, aAltura,
      aEspVertical, aEspHorizontal: Integer): AnsiString; override;

    function ComandoImprimirImagem(aMultImagem, aVertical, aHorizontal: Integer;
      aNomeImagem: String): AnsiString; override;

    function ComandoCarregarImagem(aStream: TStream; aNomeImagem: String;
      aFlipped: Boolean; aTipo: String): AnsiString; override;
  end;

implementation

uses
  math, sysutils, strutils,
  {$IFNDEF COMPILER6_UP} ACBrD5, Windows, {$ENDIF}
  ACBrUtil, ACBrConsts, synautil;

{ TACBrETQPpla }

constructor TACBrETQPpla.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Unidade := etqMilimetros;

  fpModeloStr    := 'PPLA';
  fpLimiteCopias := 9999;
end;

function TACBrETQPpla.ConverterMultiplicador(aMultiplicador: Integer): String;
begin
  // Multiplicador Horizontal, Multiplicador Vertical:
  // - De 0 a 9 e de A até O representa as escalas de multiplicação (A=10, B=11, ..., O=24)
  if (aMultiplicador >= 0) and (aMultiplicador < 10) then
    Result := IntToStr(aMultiplicador)
  else if (aMultiplicador < 24) then
    Result := chr(aMultiplicador + 55)  //Ex: 10 + 55 = 65 = A
  else
    raise Exception.Create(ACBrStr('Informe um valor entre 0 e 24'));
end;

function TACBrETQPpla.ConverterVelocidade(Velocidade: Integer): Char;
begin
  case Velocidade of
    1: Result := 'A';
    2: Result := 'B';
    3: Result := 'C';
    4: Result := 'D';
  else
    Result := 'C';
  end;
end;

function TACBrETQPpla.ConverterCoordenadas(aVertical, aHorizontal: Integer
  ): String;
var
  wAuxVert, wAuxHoriz: Integer;
begin
  wAuxVert  := ConverterUnidade(aVertical);
  wAuxHoriz := ConverterUnidade(aHorizontal);

  if (wAuxVert < 0) or (wAuxVert > 9999) then
    raise Exception.Create('Vertical deve ser de 0 a 9999');

  if (wAuxHoriz < 0) or (wAuxHoriz > 9999) then
    raise Exception.Create('Horizontal deve ser de 0 a 9999');

  Result := IntToStrZero(wAuxVert,  4) + IntToStrZero(wAuxHoriz, 4);
end;

function TACBrETQPpla.ConverterOrientacao(aOrientacao: TACBrETQOrientacao
  ): String;
begin
  Result := IntToStr(Integer(aOrientacao) + 1);
end;

function TACBrETQPpla.ConverterSubFonte(aFonte: String; aSubFonte: Integer
  ): String;
begin
  if (aSubFonte < 0) or (aSubFonte > 999) then
    raise Exception.Create('Subfonte deve ser de 0 a 999');

  // SubFonte é utilizado para acessar fontes diferenciadas. Para mais informações
  //  consulte os apêndices AC e AD do manual PPLA&PPLB.pdf
  if (aFonte <> '9') then
    Result := '000'
  else
    Result := IntToStrZero(aSubFonte, 3);
end;

function TACBrETQPpla.ConverterEspessura(aVertical, aHorizontal: Integer
  ): String;
begin
  if (aHorizontal < 0) or (aHorizontal > 999) then
    raise Exception.Create('Espessura Horizontal deve ser de 0 a 999');

  if (aVertical < 0) or (aVertical > 999) then
    raise Exception.Create('Espessura Vertical deve ser de 0 a 999');

  Result := IntToStrZero(aHorizontal, 3) + IntToStrZero(aVertical, 3);
end;

function TACBrETQPpla.ConverterDimensao(aAltura, aLargura: Integer): String;
begin
  if (aLargura < 0) or (aLargura > 999) then
    raise Exception.Create('Largura deve ser de 0 a 999');

  if (aAltura < 0) or (aAltura > 999) then
    raise Exception.Create('Altura deve ser de 0 a 999');

  Result := IntToStrZero(aLargura, 3) + IntToStrZero(aAltura, 3);
end;

function TACBrETQPpla.ConverterAlturaBarras(aAlturaBarras: Integer
  ): String;
var
  wAlturaConv: Integer;
begin
  wAlturaConv := ConverterUnidade(aAlturaBarras);

  if (wAlturaConv < 0) or (wAlturaConv > 999) then
    raise Exception.Create('Altura Barras deve ser de 0 a 999');

  Result := IntToStrZero(wAlturaConv, 3);
end;

function TACBrETQPpla.ComandoReverso(aImprimirReverso: Boolean): String;
begin
  Result := IfThen(aImprimirReverso, 'A5', 'A1');
end;

function TACBrETQPpla.PrefixoComandoLinhaECaixa(aOrientacao: TACBrETQOrientacao
  ): String;
begin
  Result := ConverterOrientacao(aOrientacao) + 'X11000';
end;

function TACBrETQPpla.ComandoTipoImagem(aNomeImagem: String; aFlipped: Boolean;
  aTipo: String): String;
var
  Cmd: Char;
begin
  aTipo := UpperCase(LeftStr(aTipo,3));

  if (aTipo = 'PCX') then
    Cmd := 'p'
  else if (aTipo = 'IMG') then
    Cmd := 'i'
  else if (aTipo = 'HEX') then
    Cmd := 'f'
  else if (aTipo = 'BMP') then
    Cmd := 'b'
  else
    raise Exception.Create(ACBrStr(
      'Formato de Imagem deve ser Monocromático e do atipo: BMP, PCX, IMG ou HEX'));

  if aFlipped then
    Cmd := UpCase(Cmd);

  Result := STX + 'IA' + Cmd + AjustarNomeArquivoImagem(aNomeImagem);
end;

function TACBrETQPpla.AjustarNomeArquivoImagem(aNomeImagem: String): String;
begin
  Result := UpperCase(LeftStr(OnlyAlphaNum(aNomeImagem), 16));
end;

function TACBrETQPpla.ConverterMultiplicadorImagem(aMultiplicador: Integer
  ): String;
begin
  aMultiplicador := max(aMultiplicador,1);
  if (aMultiplicador > 99) then
    raise Exception.Create('Multiplicador Imagem deve ser de 0 a 99');

  Result := IntToStrZero(aMultiplicador, 2);
end;

function TACBrETQPpla.ComandoTemperatura: AnsiString;
begin
  if (Temperatura < 0) or (Temperatura > 20) then
    raise Exception.Create('Temperatura deve ser de 0 a 20');

  if (Temperatura > 0) then
    Result := 'H' + IntToStrZero(Temperatura, 2)
  else
    Result := EmptyStr;
end;

function TACBrETQPpla.ComandoVelocidade: AnsiString;
begin
  if (Velocidade > 4) then
    raise Exception.Create('Velocidade deve ser de 1 a 4 ');

  if (Velocidade > 0) then
    Result := 'P' + ConverterVelocidade(Velocidade)
  else
    Result := EmptyStr;
end;

function TACBrETQPpla.ComandoCopias(const NumCopias: Integer): AnsiString;
begin
  inherited ComandoCopias(NumCopias);
  Result := 'Q' + IntToStrZero(NumCopias, 4);
end;

function TACBrETQPpla.ComandoImprimir: AnsiString;
begin
  Result := 'E';
end;

function TACBrETQPpla.ComandoAvancarEtiqueta(const aAvancoEtq: Integer
  ): AnsiString;
begin
  if (aAvancoEtq > 0) then
    Result := STX + 'f' + IntToStrZero(aAvancoEtq, 3)
  else
    Result := EmptyStr;
end;

function TACBrETQPpla.ComandoUnidade: AnsiString;
begin
  if (Unidade = etqPolegadas) then
    Result := 'n'
  else
    Result := 'm';

  Result := STX + Result;
end;

function TACBrETQPpla.ComandoResolucao: AnsiString;
begin
  Result := 'D11';  // Fixo em alta resolucao
end;

function TACBrETQPpla.ConverterUnidade(AValue: Integer): Integer;
begin
  if (Unidade = etqDots) then
    Result := inherited ConverterUnidade(etqMilimetros, AValue)
  else
    Result := AValue;

  if (Unidade <> etqDecimoDeMilimetros) then
    Result := AValue * 10;

  if (Unidade = etqPolegadas) then
    Result := Result * 10;
end;

function TACBrETQPpla.AjustarTipoBarras(aTipo: String;
  aExibeCodigo: TACBrETQBarraExibeCodigo): String;
begin
  // Tipo de Código de Barras:
  // - De 'a' até 't' ... De 'A' até 'T'

  Result := PadLeft(aTipo, 1, 'a');

  if (aExibeCodigo = becNAO) then
    Result := LowerCase(Result)
  else
    Result := UpperCase(Result);

  if not CharInSet(Result[1], ['a'..'t','A'..'T']) then
    raise Exception.Create('Tipo Cod.Barras deve ser de "A" a "T"');
end;

function TACBrETQPpla.ComandoAbertura: AnsiString;
begin
  Result := STX + 'L';
end;

function TACBrETQPpla.ComandosFinalizarEtiqueta(NumCopias: Integer;
  aAvancoEtq: Integer): AnsiString;
var
  wAvanco: Integer;
begin
  if (aAvancoEtq < 0) or (aAvancoEtq > 779) then
    raise Exception.Create('Avanço de Etiquetas deve ser de 0 a 779');

  // Valor mínimo para Back-feed é 220 (Manual "PPLA&PPLB.pdf" ... pág. 18)
  wAvanco := aAvancoEtq + 220;
  Result  := Inherited ComandosFinalizarEtiqueta(NumCopias, wAvanco);
end;

function TACBrETQPpla.TratarComandoAntesDeEnviar(aCmd: AnsiString): AnsiString;
begin
  Result := ChangeLineBreak( aCmd, CR );
end;

function TACBrETQPpla.ComandoLimparMemoria: AnsiString;
begin
  Result :=  STX + 'Q';
end;

function TACBrETQPpla.ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao;
  aFonte: String; aMultHorizontal, aMultVertical, aVertical,
  aHorizontal: Integer; aTexto: String; aSubFonte: Integer;
  aImprimirReverso: Boolean): AnsiString;
begin

  if (Length(aTexto) > 255) then
    raise Exception.Create(ACBrStr('Tamanho máximo para o texto 255 caracteres'));

  aFonte := PadLeft(aFonte,1,'0');
  if (aFonte < '0') or (aFonte > '9') then
    raise Exception.Create('Fonte deve ser de 0 a 9');

  Result := ComandoReverso(aImprimirReverso) + sLineBreak +
            ConverterOrientacao(aOrientacao) +
            aFonte +
            ConverterMultiplicador(aMultHorizontal) +
            ConverterMultiplicador(aMultVertical) +
            ConverterSubFonte(aFonte, aSubFonte) +
            ConverterCoordenadas(aVertical, aHorizontal) +
            LeftStr(aTexto, 255);
end;

function TACBrETQPpla.ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra
  ): String;
begin
  case TipoBarras of
    barCODE39      : Result := 'A';
    barUPCA        : Result := 'B';
    barINTERLEAVED : Result := 'D';
    barCODE128     : Result := 'E';
    barEAN13       : Result := 'F';
    barEAN8        : Result := 'G';
    barCODABAR     : Result := 'I';
    barCODE93      : Result := 'O';
    barMSI         : Result := 'K';
  else
    Result := '';
  end;
end;

function TACBrETQPpla.ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao;
  aTipoBarras: String; aBarraLarga, aBarraFina, aVertical,
  aHorizontal: Integer; aTexto: String; aAlturaBarras: Integer;
  aExibeCodigo: TACBrETQBarraExibeCodigo): AnsiString;
begin
  // Largura da Barra Larga e Largura da Barra Fina:
  // - De 0 a 9 ... de 'A' até 'O'

  Result := ConverterOrientacao(aOrientacao) +
            AjustarTipoBarras(aTipoBarras, aExibeCodigo) +
            ConverterMultiplicador(aBarraLarga) +
            ConverterMultiplicador(aBarraFina) +
            ConverterAlturaBarras(aAlturaBarras) +
            ConverterCoordenadas(aVertical, aHorizontal) +
            LeftStr(aTexto, 255);
end;

function TACBrETQPpla.ComandoImprimirLinha(aVertical, aHorizontal, aLargura,
  aAltura: Integer): AnsiString;
begin
  Result := PrefixoComandoLinhaECaixa(orNormal) +
            ConverterCoordenadas(aVertical, aHorizontal) +
            'L' +
            ConverterDimensao(aAltura, aLargura);
end;

function TACBrETQPpla.ComandoImprimirCaixa(aVertical, aHorizontal, aLargura,
  aAltura, aEspVertical, aEspHorizontal: Integer): AnsiString;
begin
  Result := PrefixoComandoLinhaECaixa(orNormal) +
            ConverterCoordenadas(aVertical, aHorizontal) +
            'B' +
            ConverterDimensao(aAltura, aLargura) +
            ConverterEspessura(aEspVertical, aEspHorizontal);
end;

function TACBrETQPpla.ComandoImprimirImagem(aMultImagem, aVertical,
  aHorizontal: Integer; aNomeImagem: String): AnsiString;
begin
  Result := '1Y' +
            ConverterMultiplicadorImagem(aMultImagem) + '000' +
            ConverterCoordenadas(aVertical, aHorizontal) +
            AjustarNomeArquivoImagem(aNomeImagem);
end;

function TACBrETQPpla.ComandoCarregarImagem(aStream: TStream;
  aNomeImagem: String; aFlipped: Boolean; aTipo: String): AnsiString;
begin
  aStream.Position := 0;
  Result := ComandoTipoImagem(aNomeImagem, aFlipped, aTipo) + CR +
            ReadStrFromStream(aStream, aStream.Size);
end;

end.
