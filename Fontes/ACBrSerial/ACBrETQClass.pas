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

unit ACBrETQClass;

interface

uses
  Classes,
  ACBrDevice
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

const
  CInchCM = 2.54;

resourcestring
  cErrImgNotPCXMono = 'Imagem não é PCX Monocromática';
  cErrImgNotPNG = 'Imagem não é PNG Monocromático';

type

  TACBrETQUnidade = (etqMilimetros, etqPolegadas, etqDots, etqDecimoDeMilimetros);

  TACBrETQDPI = (dpi203, dpi300, dpi600);

  TACBrETQOrientacao = (orNormal, or270, or180, or90);

  TACBrETQBarraExibeCodigo = (becPadrao, becSIM, becNAO);

  TACBrETQBackFeed = (bfNone, bfOn, bfOff);

  TACBrETQOrigem = (ogNone, ogTop, ogBottom);

  TACBrETQPaginaCodigo = (pceNone, pce437, pce850, pce852, pce860, pce1250, pce1252);

  TACBrETQDeteccaoEtiqueta = (mdeNone, mdeGap, mdeBlackMark);

  { TACBrETQDimensoes }

  TACBrETQDimensoes = class
  private
    fAltura: Integer;
    fEspacoEsquerda: Integer;
    fEspacoEntreEtiquetas: Integer;
    fLargura: Integer;
  public
    constructor Create;
    procedure Clear;
    property Largura: Integer read fLargura write fLargura;
    property Altura: Integer read fAltura write fAltura;
    property EspacoEsquerda: Integer read fEspacoEsquerda write fEspacoEsquerda;
    property EspacoEntreEtiquetas: Integer read fEspacoEntreEtiquetas write fEspacoEntreEtiquetas;
  end;

  { TACBrETQCor }

  TACBrETQCor = class
  private
    fB: Byte;
    fColor: Cardinal;
    fG: Byte;
    fOpacidade: Byte;
    fR: Byte;
    procedure SetR(AValue: Byte);
    procedure SetG(AValue: Byte);
    procedure SetB(AValue: Byte);
    procedure SetColor(AValue: Cardinal);
    procedure ColorToRGB;
    procedure RGBToColor;
  public
    constructor Create;
    procedure Clear;
    property Color: Cardinal read fColor write SetColor;
    property R: Byte read fR write SetR;
    property G: Byte read fG write SetG;
    property B: Byte read fB write SetB;
    property Opacidade: Byte read fOpacidade write fOpacidade;
  end;

  { Classe generica de ETIQUETA, nao implementa nenhum modelo especifico, apenas
    declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
    as demais Classes de Impressora como por exemplo a classe TACBrETQPpla }

  { TACBrETQClass }

  TACBrETQClass = class
  private
    fLimparMemoria : Boolean;
    fOrigem: TACBrETQOrigem;
    fDeteccaoEtiqueta: TACBrETQDeteccaoEtiqueta;
    fBackFeed: TACBrETQBackFeed;
    fGuilhotina: Boolean;
    fPaginaDeCodigo: TACBrETQPaginaCodigo;
    fUnidade: TACBrETQUnidade;
    fTemperatura: Integer;
    fVelocidade: Integer;
    fDPI: TACBrETQDPI;
    fAvanco: Integer;
    fCorFrente: TACBrETQCor;
    fCorFundo: TACBrETQCor;
    fDimensoes: TACBrETQDimensoes;

    procedure ErroNaoImplementado(const aNomeMetodo: String);

  protected
    fpModeloStr: String;
    fpDevice: TACBrDevice;
    fpOwner: TComponent;
    fpLimiteCopias: Integer;

    function ConverterUnidade(UnidadeSaida: TACBrETQUnidade; AValue: Integer): Integer; virtual;
    procedure VerificarConteudoETipoImagemMono(ImgStream: TStream; const TipoImg: String);

    procedure AdicionarComandos( const ACmd: AnsiString; var ACmdList: AnsiString);
    procedure VerificarLimiteCopias( const NumCopias: Integer);

  protected
    function ComandoAbertura: AnsiString; virtual;
    function ComandoBackFeed: AnsiString; virtual;
    function ComandoDeteccao: AnsiString; virtual;
    function ComandoGuilhotina: AnsiString; virtual;
    function ComandoDimensoes: AnsiString; virtual;
    function ComandoUnidade: AnsiString; virtual;
    function ComandoTemperatura: AnsiString; virtual;
    function ComandoPaginaDeCodigo: AnsiString; virtual;
    function ComandoResolucao: AnsiString; virtual;
    function ComandoOrigemCoordenadas: AnsiString; virtual;
    function ComandoVelocidade: AnsiString; virtual;

    function ConverterQRCodeErrorLevel(aErrorLevel: Integer): String; virtual;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function TratarComandoAntesDeEnviar(const aCmd: AnsiString): AnsiString; virtual;

    function ComandoLimparMemoria: AnsiString; virtual;
    function ComandoCopias(const NumCopias: Integer): AnsiString; virtual;
    function ComandoImprimir: AnsiString; virtual;
    function ComandoAvancarEtiqueta(const aAvancoEtq: Integer): AnsiString; virtual;

    function ComandosIniciarEtiqueta: AnsiString; virtual;
    function ComandosFinalizarEtiqueta(NumCopias: Integer = 1; aAvancoEtq: Integer = 0): AnsiString; virtual;

    function ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao; aFonte: String;
      aMultHorizontal, aMultVertical, aVertical, aHorizontal: Integer; aTexto: String;
      aSubFonte: Integer = 0; aImprimirReverso: Boolean = False): AnsiString; virtual;

    function ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra): String; virtual;
    function ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao;
      aTipoBarras: String; aBarraLarga, aBarraFina, aVertical,
      aHorizontal: Integer; aTexto: String; aAlturaBarras: Integer;
      aExibeCodigo: TACBrETQBarraExibeCodigo = becPadrao): AnsiString; virtual;
    function ComandoImprimirQRCode(aVertical, aHorizontal: Integer;
      const aTexto: String; aLarguraModulo: Integer; aErrorLevel: Integer;
      aTipo: Integer): AnsiString; virtual;

    function ComandoImprimirLinha(aVertical, aHorizontal, aLargura,
      aAltura: Integer): AnsiString; virtual;
    function ComandoImprimirCaixa(aVertical, aHorizontal, aLargura, aAltura,
      aEspVertical, aEspHorizontal: Integer; aCanto: Integer = 0): AnsiString; virtual;

    function ComandoImprimirImagem(aMultImagem, aVertical, aHorizontal: Integer;
      aNomeImagem: String): AnsiString; virtual;
    function ComandoCarregarImagem(aStream: TStream; var aNomeImagem: String;
      aFlipped: Boolean; aTipo: String): AnsiString; virtual;
    function ComandoApagarImagem(const NomeImagem: String = '*'): String; virtual;

    function ComandoGravaRFIDHexaDecimal(aValue:String): AnsiString; virtual;
    function ComandoGravaRFIDASCII( aValue:String ): AnsiString; virtual;

    property ModeloStr:       String           read fpModeloStr;
    property PaginaDeCodigo:  TACBrETQPaginaCodigo read fPaginaDeCodigo write fPaginaDeCodigo;
    property Temperatura:     Integer          read fTemperatura      write fTemperatura;
    property Velocidade:      Integer          read fVelocidade       write fVelocidade;
    property BackFeed:        TACBrETQBackFeed read fBackFeed         write fBackFeed;
    property LimparMemoria:   Boolean          read fLimparMemoria    write fLimparMemoria;
    property Unidade:         TACBrETQUnidade  read fUnidade          write fUnidade;
    property Origem:          TACBrETQOrigem   read fOrigem           write fOrigem;
    property Avanco:          Integer          read fAvanco           write fAvanco;
    property Guilhotina:      Boolean          read fGuilhotina       write fGuilhotina default False;
    property DPI:             TACBrETQDPI      read fDPI              write fDPI;
    property DeteccaoEtiqueta: TACBrETQDeteccaoEtiqueta read fDeteccaoEtiqueta write fDeteccaoEtiqueta;

    property CorFrente: TACBrETQCor read fCorFrente;
    property CorFundo: TACBrETQCor read fCorFundo;
    property Dimensoes: TACBrETQDimensoes read fDimensoes;
  end;

implementation

uses
  SysUtils, math, ACBrConsts, ACBrETQ, ACBrImage,
  ACBrUtil.Base, ACBrUtil.Strings;

{ TACBrETQDimensoes }

constructor TACBrETQDimensoes.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrETQDimensoes.Clear;
begin
  fLargura := -1;
  fAltura := -1;
  fEspacoEsquerda := -1;
  fEspacoEntreEtiquetas := -1;
end;

{ TACBrETQCor }

constructor TACBrETQCor.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrETQCor.Clear;
begin
  Color := 0;    // clBlack = $000000;  clWhite = $FFFFFF;
  fOpacidade := 0;  // 0-Transparente, 255-opaco
end;

procedure TACBrETQCor.SetR(AValue: Byte);
begin
  fR := AValue;
  RGBToColor;
end;

procedure TACBrETQCor.SetG(AValue: Byte);
begin
  fG := AValue;
  RGBToColor;
end;

procedure TACBrETQCor.SetB(AValue: Byte);
begin
  fB := AValue;
  RGBToColor;
end;

procedure TACBrETQCor.SetColor(AValue: Cardinal);
begin
  fColor := AValue;
  ColorToRGB;
end;

procedure TACBrETQCor.ColorToRGB;
begin
  fR := fColor and $000000ff;
  fG := (fColor shr 8) and $000000ff;
  fB := (fColor shr 16) and $000000ff;
end;

procedure TACBrETQCor.RGBToColor;
begin
  fColor := (fB shl 16) or (fG shl 8) or fR;
end;

{ TACBrBAETQClass }

constructor TACBrETQClass.Create(AOwner: TComponent);
begin
  if (not (AOwner is TACBrETQ)) then
    raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrETQ'));

  fPaginaDeCodigo := pceNone;
  fDPI := dpi203;
  fLimparMemoria := True;
  fOrigem := ogNone;
  fAvanco := 0;
  fGuilhotina := False;
  fTemperatura := 10;
  fVelocidade := -1;
  fUnidade := etqDecimoDeMilimetros;
  fDeteccaoEtiqueta := mdeGap;
  fCorFrente := TACBrETQCor.Create;
  fCorFrente.Color := $000000; // Black
  fCorFrente.Opacidade := 255;
  fCorFundo := TACBrETQCor.Create;
  fCorFundo.Color := $FFFFFF; // White
  fCorFundo.Opacidade := 0;
  fDimensoes := TACBrETQDimensoes.Create;

  fpOwner := AOwner;
  fpModeloStr := 'Não Definida';
  fpLimiteCopias := 999;
end;

destructor TACBrETQClass.Destroy;
begin
  fCorFrente.Free;
  fCorFundo.Free;
  fDimensoes.Free;
  inherited Destroy;
end;

procedure TACBrETQClass.ErroNaoImplementado(const aNomeMetodo: String);
begin
  raise Exception.Create(ACBrStr('Metodo: ' + aNomeMetodo + ' não implementada em: ' + ModeloStr));
end;

function TACBrETQClass.ConverterUnidade(UnidadeSaida: TACBrETQUnidade;
  AValue: Integer): Integer;
var
  ADouble: Double;
begin
  Result := AValue;
  if (UnidadeSaida = Unidade) then
    Exit;

  ADouble := ACBrETQ.ConverterUnidade(Unidade, AValue, UnidadeSaida, DPI);
  Result := trunc(RoundTo(ADouble, 0));
end;

procedure TACBrETQClass.VerificarConteudoETipoImagemMono(ImgStream: TStream;
  const TipoImg: String);
begin
  if (TipoImg = 'PCX') then
  begin
    if not IsPCX(ImgStream, True) then
      raise Exception.Create(ACBrStr(cErrImgNotPCXMono));
  end
  else if (TipoImg = 'PNG') then
  begin
    if not IsPNG(ImgStream, True) then
      raise Exception.Create(ACBrStr(cErrImgNotPNG));
  end
  else if (TipoImg = 'BMP') then
  begin
    if not IsBMP(ImgStream, True) then
      raise Exception.Create(ACBrStr(cErrImgNotBMPMono));
  end;
end;

procedure TACBrETQClass.AdicionarComandos(const ACmd: AnsiString;
  var ACmdList: AnsiString);
begin
  if EstaVazio( ACmd ) then
    Exit;

  if NaoEstaVazio(ACmdList) then
    ACmdList := ACmdList + sLineBreak;

  ACmdList := ACmdList + ACmd;
end;

procedure TACBrETQClass.VerificarLimiteCopias(const NumCopias: Integer);
begin
  if (NumCopias < 1) or (NumCopias > fpLimiteCopias) then
    raise Exception.Create('NumCopias deve ser de 1 a '+IntToStr(fpLimiteCopias));
end;

function TACBrETQClass.ComandoAbertura: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandosIniciarEtiqueta: AnsiString;
var
  ListaComandos: AnsiString;
begin
  ListaComandos := '';

  AdicionarComandos( ComandoBackFeed, ListaComandos );
  AdicionarComandos( ComandoAbertura, ListaComandos );
  AdicionarComandos( ComandoDeteccao, ListaComandos );
  AdicionarComandos( ComandoGuilhotina, ListaComandos );
  AdicionarComandos( ComandoDimensoes, ListaComandos );
  AdicionarComandos( ComandoUnidade, ListaComandos );
  if LimparMemoria then
    AdicionarComandos( ComandoLimparMemoria, ListaComandos );

  AdicionarComandos( ComandoTemperatura, ListaComandos );
  AdicionarComandos( ComandoResolucao, ListaComandos );
  AdicionarComandos( ComandoOrigemCoordenadas, ListaComandos );
  AdicionarComandos( ComandoVelocidade, ListaComandos );
  if (PaginaDeCodigo <> pceNone) then
    AdicionarComandos( ComandoPaginaDeCodigo, ListaComandos );

  Result := ListaComandos;
end;

function TACBrETQClass.ComandosFinalizarEtiqueta(NumCopias: Integer;
  aAvancoEtq: Integer): AnsiString;
var
  ListaComandos: AnsiString;
begin
  ListaComandos := '';

  if (NumCopias <= 0) then
    NumCopias := 1;

  if (aAvancoEtq <= 0) then
    aAvancoEtq := Avanco;

  AdicionarComandos( ComandoCopias(NumCopias), ListaComandos );
  AdicionarComandos( ComandoImprimir, ListaComandos );
  AdicionarComandos( ComandoAvancarEtiqueta(aAvancoEtq), ListaComandos );

  Result := ListaComandos;
end;

function TACBrETQClass.ComandoLimparMemoria: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoDeteccao: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoGuilhotina: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoDimensoes: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoBackFeed: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoUnidade: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoTemperatura: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoPaginaDeCodigo: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoResolucao: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoOrigemCoordenadas: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoVelocidade: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ConverterQRCodeErrorLevel(aErrorLevel: Integer): String;
begin
  case aErrorLevel of
    1: Result := 'M';
    2: Result := 'Q';
    3: Result := 'H';
  else
    Result := 'L';
  end;
end;

function TACBrETQClass.ComandoCopias(const NumCopias: Integer): AnsiString;
begin
  VerificarLimiteCopias(NumCopias);
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoGravaRFIDASCII(aValue: String): AnsiString;
begin
  ErroNaoImplementado('ComandoGravaRFIDASCII');
  result := EmptySTr;
end;

function TACBrETQClass.ComandoGravaRFIDHexaDecimal(aValue: String): AnsiString;
begin
  ErroNaoImplementado('ComandoGravaRFIDHexaDecimal');
  result := EmptySTr;
end;

function TACBrETQClass.ComandoImprimir: AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.ComandoAvancarEtiqueta(const aAvancoEtq: Integer
  ): AnsiString;
begin
  Result := EmptyStr;
end;

function TACBrETQClass.TratarComandoAntesDeEnviar(const aCmd: AnsiString): AnsiString;
begin
  Result := ChangeLineBreak( aCmd, LF );
end;

function TACBrETQClass.ComandoImprimirTexto(aOrientacao: TACBrETQOrientacao;
  aFonte: String; aMultHorizontal, aMultVertical, aVertical,
  aHorizontal: Integer; aTexto: String; aSubFonte: Integer;
  aImprimirReverso: Boolean): AnsiString;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoImprimirTexto');
end;

function TACBrETQClass.ConverterTipoBarras(TipoBarras: TACBrTipoCodBarra
  ): String;
begin
  Result := IntToStr(Integer(TipoBarras));
end;

function TACBrETQClass.ComandoImprimirBarras(aOrientacao: TACBrETQOrientacao;
  aTipoBarras: String; aBarraLarga, aBarraFina, aVertical,
  aHorizontal: Integer; aTexto: String; aAlturaBarras: Integer;
  aExibeCodigo: TACBrETQBarraExibeCodigo): AnsiString;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoImprimirBarras');
end;

function TACBrETQClass.ComandoImprimirQRCode(aVertical, aHorizontal: Integer;
  const aTexto: String; aLarguraModulo: Integer; aErrorLevel: Integer;
  aTipo: Integer): AnsiString;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoImprimirQRCode');
end;

function TACBrETQClass.ComandoImprimirLinha(aVertical, aHorizontal, aLargura,
  aAltura: Integer): AnsiString;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoImprimirLinha');
end;

function TACBrETQClass.ComandoImprimirCaixa(aVertical, aHorizontal, aLargura,
  aAltura, aEspVertical, aEspHorizontal: Integer; aCanto: Integer): AnsiString;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoImprimirCaixa');
end;

function TACBrETQClass.ComandoImprimirImagem(aMultImagem, aVertical,
  aHorizontal: Integer; aNomeImagem: String): AnsiString;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoImprimirImagem');
end;

function TACBrETQClass.ComandoCarregarImagem(aStream: TStream;
  var aNomeImagem: String; aFlipped: Boolean; aTipo: String): AnsiString;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoCarregarImagem');
end;

function TACBrETQClass.ComandoApagarImagem(const NomeImagem: String): String;
begin
  Result := EmptyStr;
  ErroNaoImplementado('ComandoApagarImagem');
end;

end.
