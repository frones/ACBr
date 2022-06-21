{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrExtenso;

interface

uses
  SysUtils, Classes, ACBrBase, ACBrUtil.Math;

const

  cMilharSinPTBr: array[0..2] of string = ('Bilhão','Milhão','Mil');
  cMilharPluPTBr: array[0..2] of string = ('Bilhões','Milhões','Mil');
  cUnidadePTBr:   array[0..8] of string = ('Um','Dois','Três','Quatro','Cinco','Seis','Sete','Oito','Nove');
  cDezPTBr:       array[0..9] of string = ('Dez','Onze','Doze','Treze','Quatorze','Quinze','Dezesseis','Dezessete','Dezoito','Dezenove');
  cDezenasPTBr:   array[0..7] of string = ('Vinte','Trinta','Quarenta','Cinquenta','Sessenta','Setenta','Oitenta','Noventa');
  cCentenasPTBr:  array[0..9] of string = ('Cem','Cento','Duzentos','Trezentos','Quatrocentos','Quinhentos','Seiscentos', 'Setecentos','Oitocentos','Novecentos');

  cMilharSinES:   array[0..2] of string = ('Mil Millones','Millones','Miles');
  cMilharPluES:   array[0..2] of string = ('Mil Millones','Millones','Miles');
  cUnidadeES:     array[0..8] of string = ('Uno','Dos','Tres','Cuatro','Cinco','Seis','Siete','Ocho','Nueve');
  cDezES:         array[0..9] of string = ('Diez','Once','Doce','Trece','Catorce','Quince','Dieciséis','Diecisiete','Dieciocho','Diecinueve');
  cDezenasES:     array[0..7] of string = ('Veinte','Treinta','Cuarenta','Cincuenta','Sesenta','Setenta','Ochenta', 'Noventa');
  cCentenasES:    array[0..9] of string = ('Cien','Ciento','Doscientos','Trescientos','Cuatrocientos','Quinientos','Seiscientos','Setecientos','Ochocientos','Novecientos');

  cMilharSinEN:   array[0..2] of string = ('Billion','Million','Thousand');
  cMilharPluEN:   array[0..2] of string = ('Billions','Millions','Thousands');
  cUnidadeEN:     array[0..8] of string = ('One','Two','Three','Four','Five','Six','Seven','Eight','Nine');
  cDezEN:         array[0..9] of string = ('Ten','Eleven','Twelve','Thirteen','Fourteen','Fifteen','Sixteen','Seventeen','Eighteen','Nineteen');
  cDezenasEN:     array[0..7] of string = ('Twenty','Thirty','Forty','Fifty','Sixty','Seventy','Eighty','Ninety');
  cCentenasEN:    array[0..9] of string = ('One hundred','One hundred','Two hundred','Three hundred','Four hundred','Five hundred','Six hundred', 'Seven hundred','Eight hundred','Nine hundred');

type

  TACBrExtensoFormato = (extPadrao, extDolar, extQuilo);
  TACBrExtensoIdioma = (idiCustom, idiPortuguesBr, idiEspanhol, idiIngles);

  { TACBrExtensoIdiomaClass }

  TACBrExtensoIdiomaClass = class
  private
    procedure SetFormato(aValue: TACBrExtensoFormato);
  protected
    fsZero: String;
    fsInteiro: String;
    fsInteiros: String;
    fsDecimal: String;
    fsDecimais: String;
    fsConjuncao: String;
    fsPreposicao: String;        
    fsZeroAEsquerda: Boolean;
    fsFormato: TACBrExtensoFormato;

    fsUnidade:   array[0..8] of string;
    fsDez:       array[0..9] of string;
    fsDezenas:   array[0..7] of string;
    fsCentenas:  array[0..9] of string;
    fsMilharSin: array[0..2] of string;
    fsMilharPlu: array[0..2] of string;

    procedure AtribuirValoresDefault; virtual; abstract;
    procedure AtribuirReal; virtual; abstract;
    procedure AtribuirDolar; virtual; abstract;
    procedure AtribuirQuilo; virtual; abstract;

    function TraduzInteiros(const aValue: Int64; TemDecimal: Boolean): String;
    function TraduzDecimais(const aValue: Integer): String;
    function StringTresDigitos(aStr3: String): String;
  public
    constructor Create;
    procedure Clear; virtual;

    function TraduzValor(const aValor: Double): String; virtual;
    
    property StrZero:     String read fsZero       write fsZero;
    property StrInteiro:  String read fsInteiro    write fsInteiro;
    property StrInteiros: String read fsInteiros   write fsInteiros;
    property StrDecimal:  String read fsDecimal    write fsDecimal;
    property StrDecimais: String read fsDecimais   write fsDecimais;
    property Conjuncao:   String read fsConjuncao  write fsConjuncao;
    property Preposicao:  String read fsPreposicao write fsPreposicao;
    property ZeroAEsquerda: Boolean read fsZeroAEsquerda write fsZeroAEsquerda default True;

    property Formato: TACBrExtensoFormato read fsFormato write SetFormato;
  end;

  { TACBrExtensoIdiomaPTBr }

  TACBrExtensoIdiomaPTBr = class(TACBrExtensoIdiomaClass)
  protected
    procedure AtribuirReal; override;
    procedure AtribuirDolar; override;
    procedure AtribuirQuilo; override;
    procedure AtribuirValoresDefault; override;
  public
    function TraduzValor(const aValor: Double): String; override;
  end;

  { TACBrExtensoIdiomaEN }

  TACBrExtensoIdiomaEN = class(TACBrExtensoIdiomaClass)
  protected
    procedure AtribuirReal; override;
    procedure AtribuirDolar; override;
    procedure AtribuirQuilo; override;
    procedure AtribuirValoresDefault; override;
  public
  end;

  { TACBrExtensoIdiomaES }

  TACBrExtensoIdiomaES = class(TACBrExtensoIdiomaClass)
  protected
    procedure AtribuirReal; override;
    procedure AtribuirDolar; override;
    procedure AtribuirQuilo; override;
    procedure AtribuirValoresDefault; override;
  public
  end;

  { TACBrExtensoIdiomaCustom }

  TACBrExtensoIdiomaCustom = class(TACBrExtensoIdiomaClass)
  protected
    procedure AtribuirReal; override;
    procedure AtribuirDolar; override;
    procedure AtribuirQuilo; override;
    procedure AtribuirValoresDefault; override;
  public
  end;

{ Componente ACBrExtenso }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

  { TACBrExtenso }

  TACBrExtenso = class( TACBrComponent )
  private
    fsValor: Double;
    fsTexto: String;
    fsInteiro: String;
    fsInteiros: String;
    fsDecimal: String;
    fsDecimais: String;
    fsIdioma: TACBrExtensoIdioma;
    fsIdiomaObj: TACBrExtensoIdiomaClass;

    function GetCentavo: String;
    function GetCentavos: String;
    function GetFormato: TACBrExtensoFormato;
    function GetMoeda: String;
    function GetMoedas: String;
    function GetZeroAEsquerda: Boolean;
    procedure SetCentavo(aValue: String);
    procedure SetCentavos(aValue: String);
    procedure SetIdioma(aValue: TACBrExtensoIdioma);
    procedure SetMoeda(aValue: String);
    procedure SetMoedas(aValue: String);
    procedure SetTexto(aValue: String);
    procedure SetValor(aValue: Double);
    procedure SetZeroAEsquerda(const Value: Boolean);
    procedure SetFormato(const aValue: TACBrExtensoFormato);

    procedure AtribuirIdiomaCustom;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;

    function ValorToTexto(AValor: Double; AFormato: TACBrExtensoFormato = extPadrao): String;
  published
    property Texto:       String read fsTexto     write SetTexto stored False;
    property Valor:       Double read fsValor     write SetValor stored False;
    property StrMoeda:    String read GetMoeda    write SetMoeda;
    property StrMoedas:   String read GetMoedas   write SetMoedas;
    property StrCentavo:  String read GetCentavo  write SetCentavo;
    property StrCentavos: String read GetCentavos write SetCentavos;

    property ZeroAEsquerda: Boolean read GetZeroAEsquerda write SetZeroAEsquerda default True;
    property Formato: TACBrExtensoFormato read GetFormato write SetFormato default extPadrao;
    property Idioma: TACBrExtensoIdioma read fsIdioma write SetIdioma default idiCustom;
end;

implementation

uses
  Math,
  ACBrUtil.Base,
  ACBrUtil.Strings;

{ TACBrExtensoIdiomaCustom }

procedure TACBrExtensoIdiomaCustom.AtribuirReal;
begin
  // Custom utiliza sempre propriedades definidas no componente
end;

procedure TACBrExtensoIdiomaCustom.AtribuirDolar;
begin
  // Custom utiliza sempre propriedades definidas no componente
end;

procedure TACBrExtensoIdiomaCustom.AtribuirQuilo;
begin
  // Custom utiliza sempre propriedades definidas no componente
end;

procedure TACBrExtensoIdiomaCustom.AtribuirValoresDefault;
var
  I: Integer;
begin
  fsZero := 'Zero';
  fsConjuncao := 'e';
  fsPreposicao := 'de';

  for I := Low(fsMilharSin) to High(fsMilharSin) do
    fsMilharSin[I] := ACBrStr(cMilharSinPTBr[I]);

  for I := Low(fsMilharPlu) to High(fsMilharPlu) do
    fsMilharPlu[I] := ACBrStr(cMilharPluPTBr[I]);

  for I := Low(fsUnidade) to High(fsUnidade) do
    fsUnidade[I] := ACBrStr(cUnidadePTBr[I]);

  for I := Low(fsDez) to High(fsDez) do
    fsDez[I] := ACBrStr(cDezPTBr[I]);

  for I := Low(fsDezenas) to High(fsDezenas) do
    fsDezenas[I] := ACBrStr(cDezenasPTBr[I]);

  for I := Low(fsCentenas) to High(fsCentenas) do
    fsCentenas[I] := ACBrStr(cCentenasPTBr[I]);
end;

{ TACBrExtensoIdiomaES }

procedure TACBrExtensoIdiomaES.AtribuirReal;
begin
  fsInteiro  := 'Real';
  fsInteiros := 'Reales';
  fsDecimal  := 'Centavo';
  fsDecimais := 'Centavos';
end;

procedure TACBrExtensoIdiomaES.AtribuirDolar;
begin 
  fsInteiro   := ACBrStr('Dólar');
  fsInteiros  := 'Dolares';
  fsDecimal   := 'Centavo';
  fsDecimais  := 'Centavos';
end;

procedure TACBrExtensoIdiomaES.AtribuirQuilo;
begin
  fsInteiro   := 'Kilo';
  fsInteiros  := 'Kilos';
  fsDecimal := 'Gramo';
  fsDecimais:= 'Gramos';
end;

procedure TACBrExtensoIdiomaES.AtribuirValoresDefault;
var
  I: Integer;
begin
  fsZero := 'Cero';
  fsConjuncao := 'y';
  fsPreposicao := 'de';

  case fsFormato of
    extPadrao: AtribuirReal;
    extDolar:  AtribuirDolar;
    extQuilo:  AtribuirQuilo;
  end;

  for I := Low(fsMilharSin) to High(fsMilharSin) do
    fsMilharSin[I] := ACBrStr(cMilharSinES[I]);

  for I := Low(fsMilharPlu) to High(fsMilharPlu) do
    fsMilharPlu[I] := ACBrStr(cMilharPluES[I]);

  for I := Low(fsUnidade) to High(fsUnidade) do
    fsUnidade[I] := ACBrStr(cUnidadeES[I]);

  for I := Low(fsDez) to High(fsDez) do
    fsDez[I] := ACBrStr(cDezES[I]);

  for I := Low(fsDezenas) to High(fsDezenas) do
    fsDezenas[I] := ACBrStr(cDezenasES[I]);

  for I := Low(fsCentenas) to High(fsCentenas) do
    fsCentenas[I] := ACBrStr(cCentenasES[I]);
end;

{ TACBrExtensoIdiomaEN }

procedure TACBrExtensoIdiomaEN.AtribuirReal;
begin
  fsInteiro  := 'Real';
  fsInteiros := 'Reais';
  fsDecimal  := 'Cent';
  fsDecimais := 'Cents';
end;

procedure TACBrExtensoIdiomaEN.AtribuirDolar;
begin
  fsInteiro   := 'Dollar';
  fsInteiros  := 'Dollars';
  fsDecimal := 'Cent';
  fsDecimais:= 'Cents';
end;

procedure TACBrExtensoIdiomaEN.AtribuirQuilo;
begin
  fsInteiro   := 'Kilo';
  fsInteiros  := 'Kilos';
  fsDecimal := 'Gram';
  fsDecimais:= 'Grams';
end;

procedure TACBrExtensoIdiomaEN.AtribuirValoresDefault;
var
  I: Integer;
begin
  fsZero := 'Zero';
  fsConjuncao := 'and';
  fsPreposicao := '';

  case fsFormato of
    extPadrao: AtribuirReal;
    extDolar:  AtribuirDolar;
    extQuilo:  AtribuirQuilo;
  end;

  for I := Low(fsMilharSin) to High(fsMilharSin) do
    fsMilharSin[I] := ACBrStr(cMilharSinEN[I]);

  for I := Low(fsMilharPlu) to High(fsMilharPlu) do
    fsMilharPlu[I] := ACBrStr(cMilharPluEN[I]);

  for I := Low(fsUnidade) to High(fsUnidade) do
    fsUnidade[I] := ACBrStr(cUnidadeEN[I]);

  for I := Low(fsDez) to High(fsDez) do
    fsDez[I] := ACBrStr(cDezEN[I]);

  for I := Low(fsDezenas) to High(fsDezenas) do
    fsDezenas[I] := ACBrStr(cDezenasEN[I]);

  for I := Low(fsCentenas) to High(fsCentenas) do
    fsCentenas[I] := ACBrStr(cCentenasEN[I]);
end;

{ TACBrExtensoIdiomaPTBr }

procedure TACBrExtensoIdiomaPTBr.AtribuirReal;
begin
  fsInteiro   := 'Real';
  fsInteiros  := 'Reais';
  fsDecimal   := 'Centavo';
  fsDecimais  := 'Centavos';
end;

procedure TACBrExtensoIdiomaPTBr.AtribuirDolar;
begin
  fsInteiro   := ACBrStr('Dólar');
  fsInteiros  := 'Dolares';
  fsDecimal   := ACBrStr('Centavo de Dólar');
  fsDecimais  := ACBrStr('Centavos de Dólar');
end;

procedure TACBrExtensoIdiomaPTBr.AtribuirQuilo;
begin
  fsInteiro   := 'Quilo';
  fsInteiros  := 'Quilos';
  fsDecimal   := 'Grama';
  fsDecimais  := 'Gramas';
end;

procedure TACBrExtensoIdiomaPTBr.AtribuirValoresDefault;
var
  I: Integer;
begin
  fsZero := 'Zero';
  fsConjuncao := 'e';
  fsPreposicao := 'de';

  case fsFormato of
    extPadrao: AtribuirReal;
    extDolar:  AtribuirDolar;
    extQuilo:  AtribuirQuilo;
  end;

  for I := Low(fsMilharSin) to High(fsMilharSin) do
    fsMilharSin[I] := ACBrStr(cMilharSinPTBr[I]);

  for I := Low(fsMilharPlu) to High(fsMilharPlu) do
    fsMilharPlu[I] := ACBrStr(cMilharPluPTBr[I]);

  for I := Low(fsUnidade) to High(fsUnidade) do
    fsUnidade[I] := ACBrStr(cUnidadePTBr[I]);

  for I := Low(fsDez) to High(fsDez) do
    fsDez[I] := ACBrStr(cDezPTBr[I]);

  for I := Low(fsDezenas) to High(fsDezenas) do
    fsDezenas[I] := ACBrStr(cDezenasPTBr[I]);

  for I := Low(fsCentenas) to High(fsCentenas) do
    fsCentenas[I] := ACBrStr(cCentenasPTBr[I]);
end;

function TACBrExtensoIdiomaPTBr.TraduzValor(const aValor: Double): String;
begin
  Result := inherited TraduzValor(aValor);

  { Verificando se é na casa de 1 HUM mil }
  if (fsFormato = extPadrao) and (aValor >= 1000.00) and (aValor < 2000.00) then
    Result := 'Hum' + Copy(Result, 3, Length(Result));
end;

{ TACBrExtensoIdiomaClass }

procedure TACBrExtensoIdiomaClass.SetFormato(aValue: TACBrExtensoFormato);
begin
  if (fsFormato = aValue) then
    Exit;

  fsFormato := aValue;
  case aValue of
    extPadrao: AtribuirReal;
    extDolar:  AtribuirDolar;
    extQuilo:  AtribuirQuilo;
  end;
end;

function TACBrExtensoIdiomaClass.TraduzInteiros(const aValue: Int64;
  TemDecimal: Boolean): String;
var
  wCasa: Integer;
  StrInteiros: String;
  wStrCasas: array[0..3] of String;
begin
  Result := EmptyStr;
  StrInteiros := IntToStrZero(aValue, 12);

  { Achando a CASAS dos Bilhoes, Milhoes, Mil, e Cem }
  for wCasa := 0 to 3 do
    wStrCasas[wCasa] := Copy(StrInteiros, (wCasa * 3)+1, 3);

  for wCasa := 0 to 3 do
  begin
    if (StrToIntDef(wStrCasas[wCasa], 0) = 0)  then  // wCasa vazia ?
      Continue;

    if (Result <> EmptyStr) then  // Se ja existe texto, concatena com ',' ou conjunção ('e', 'and'...)
    begin
      if (wCasa = 3) and (not TemDecimal) then
        Result := Result + ' ' + Conjuncao + ' '
      else
        Result := Result + ', ';
    end;

    Result := Result + StringTresDigitos(wStrCasas[wCasa]);

    { Se for acima da casa dos Cem pegue um título }
    if (wCasa < 3) then
    begin
      { Se a CASA tiver valor de UM usa singular, senao usa o plural }
      if (StrToIntDef(wStrCasas[wCasa], 0) = 1) then
        Result := Result + ' ' + fsMilharSin[wCasa]
      else
        Result := Result + ' ' + fsMilharPlu[wCasa];
    end;
  end;

  { Se nao possui valores na casa dos MIL ou dos CEM, concatena preposição }
  { Ex: 1 milhão DE reais }
  if (StrToIntDef(wStrCasas[2] + wStrCasas[3], 0) = 0) and NaoEstaVazio(Preposicao) then
    Result := Result + ' ' + Preposicao;

  { Se o valor total for UM usa moeda singular, senao no Plural }
  if (Result <> EmptyStr) then
  begin
    if (aValue = 1) Then
      Result := Result + ' ' + fsInteiro
    else
      Result := Result + ' ' + fsInteiros;
  end;
end;

function TACBrExtensoIdiomaClass.TraduzDecimais(const aValue: Integer): String;
begin
  Result := StringTresDigitos(IntToStrZero(aValue, 3));

  { Se o valor total dos decimais for UM usa singular, senao no Plurar }
  if (aValue = 1) Then
    Result := Result + ' ' + fsDecimal
  else
    Result := Result + ' ' + fsDecimais;
end;
                                                                          
// Esta funcão monta o extenso de um Str3
//  ( Nao acressenta moeda ou título (Mil, Milhao, etc..))
//  ( Str3 = String com 3 casas com valor a transformar em extenso )
function TACBrExtensoIdiomaClass.StringTresDigitos(aStr3: String): String;
var
  wPos1, wPos2, wPos3: Integer;
begin
  Result := EmptyStr;
  wPos1    := StrToIntDef(Copy(aStr3, 1, 1), 0);
  wPos2    := StrToIntDef(Copy(aStr3, 2, 1), 0);
  wPos3    := StrToIntDef(Copy(aStr3, 3, 1), 0);

  { Se possuir número na casa da centena processe }
  if (wPos1 > 0) then
  begin
    { Se nao possuir números a seguir e wPos1 for o numero UM entao = "Cem" }
    if (wPos1 = 1) and ((wPos2 + wPos3) = 0) then
      wPos1 := 0;

    Result := fsCentenas[wPos1];
  end;

  { Se possuir numero na casa da dezena processe }
  if (wPos2 > 0) then
  begin
    { Se já possui algum texto adiciona conjunção (e, with, y) }
    if NaoEstaVazio(Result) then
      Result := Result + ' ' + Conjuncao + ' ';

    { Se for na casa dos dez usa vetor de Dezenas }
    if (wPos2 = 1) then
    begin
      Result := Result + fsDez[wPos3];  // Dez, Onze, Doze...
      wPos3 := 0;
    end
    else
      Result := Result + fsDezenas[wPos2 - 2]; {Vinte, Trinta...}
  end;

  { Se possuir numero na casa da unidade processe }
  if (wPos3 > 0) then
  begin
    if NaoEstaVazio(Result) then  { Se ja possui algum Texto adiciona o ' e ' }
      Result := Result + ' ' + Conjuncao + ' ';

    Result := Result + fsUnidade[wPos3 - 1];
  end;

  Result := Result;
end;

constructor TACBrExtensoIdiomaClass.Create;
begin
  Clear;
  AtribuirValoresDefault;
end;

procedure TACBrExtensoIdiomaClass.Clear;
begin                    
  fsFormato := extPadrao;

  fsZero := EmptyStr;
  fsInteiro := EmptyStr;
  fsInteiros := EmptyStr;
  fsDecimal := EmptyStr;
  fsDecimais := EmptyStr;
  fsConjuncao := EmptyStr;
  fsPreposicao := EmptyStr;
  fsZeroAEsquerda := True;
end;

function TACBrExtensoIdiomaClass.TraduzValor(const aValor: Double): String;
var
  wValor: Double;
  wInteiros: Int64;
  wDecimais: Integer;
begin
  Result := EmptyStr;

  if (aValor > 999999999999.99) then
    raise Exception.Create('Valor acima do permitido');

  if (Formato = extQuilo) then
    wValor := RoundABNT(aValor, -3)   // Kg arredonda em 3 casas wdecimais
  else
    wValor := RoundABNT(aValor, -2);  // Val.Monetário arredonda em 2 casas wdecimais

  wInteiros := Trunc(wValor);
  wDecimais := Round(Frac(wValor) * 100);

  if (wInteiros > 0) then  // Se tiver inteiros, processe
    Result := TraduzInteiros(wInteiros, (wDecimais > 0))
  else if fsZeroAEsquerda then
    Result := StrZero;

  if (wDecimais > 0) then  // Se tiver decimais, processe
  begin
    { Se ja possui algum Texto adiciona conjunção (e, with, y...) }
    if NaoEstaVazio(Result) then
      Result := Result + ' ' + Conjuncao + ' ';

    Result := Result + TraduzDecimais(wDecimais);
  end;
end;

{ TACBrExtenso }

constructor TACBrExtenso.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Clear;
end;

destructor TACBrExtenso.Destroy;
begin
  if Assigned(fsIdiomaObj) then
    fsIdiomaObj.Free;
  inherited Destroy;
end;

procedure TACBrExtenso.Clear;
begin
  fsValor := 0;
  fsTexto := EmptyStr;
  fsIdioma := idiCustom;

  if Assigned(fsIdiomaObj) then
    fsIdiomaObj.Free;

  fsIdiomaObj := TACBrExtensoIdiomaCustom.Create;
end;

function TACBrExtenso.GetCentavo: String;
begin
  if (Idioma = idiCustom) then
    Result := fsDecimal
  else
    Result := fsIdiomaObj.StrDecimal;
end;

function TACBrExtenso.GetCentavos: String;
begin
  if (Idioma = idiCustom) then
    Result := fsDecimais
  else
    Result := fsIdiomaObj.StrDecimais;
end;

function TACBrExtenso.GetFormato: TACBrExtensoFormato;
begin
  Result := fsIdiomaObj.Formato;
end;

function TACBrExtenso.GetMoeda: String;
begin
  if (Idioma = idiCustom) then
    Result := fsInteiro
  else
    Result := fsIdiomaObj.StrInteiro;
end;

function TACBrExtenso.GetMoedas: String;
begin
  if (Idioma = idiCustom) then
    Result := fsInteiros
  else
    Result := fsIdiomaObj.StrInteiros;
end;

function TACBrExtenso.GetZeroAEsquerda: Boolean;
begin
  Result := fsIdiomaObj.ZeroAEsquerda;
end;

procedure TACBrExtenso.SetCentavo(aValue: String);
begin
  if (fsIdiomaObj.StrDecimal = aValue) then
    Exit;

  AtribuirIdiomaCustom;
  fsDecimal := aValue;
  fsIdiomaObj.StrDecimal := aValue;
  fsValor := 0;  // Zerando valor para recalcular o texto
end;

procedure TACBrExtenso.SetCentavos(aValue: String);
begin
  if (Idioma <> idiCustom) and (fsIdiomaObj.StrDecimais = aValue) then
    Exit;

  AtribuirIdiomaCustom;
  fsDecimais := aValue;
  fsIdiomaObj.StrDecimais := aValue;
  fsValor := 0;  // Zerando valor para recalcular o texto
end;

procedure TACBrExtenso.SetIdioma(aValue: TACBrExtensoIdioma);
begin
  if (fsIdioma = aValue) then
    Exit;

  fsIdioma := aValue;
  fsIdiomaObj.Free;

  case fsIdioma of
    idiPortuguesBr: fsIdiomaObj := TACBrExtensoIdiomaPTBr.Create;
    idiIngles:      fsIdiomaObj := TACBrExtensoIdiomaEN.Create;
    idiEspanhol:    fsIdiomaObj := TACBrExtensoIdiomaES.Create;
    idiCustom:      fsIdiomaObj := TACBrExtensoIdiomaCustom.Create;
  end;

  fsValor := 0;  // Zera o valor, para recalcular o texto
end;

procedure TACBrExtenso.SetMoeda(aValue: String);
begin
  if (fsIdiomaObj.StrInteiro = aValue) then
    Exit;

  AtribuirIdiomaCustom;
  fsInteiro := aValue;
  fsIdiomaObj.StrInteiro := aValue;
  fsValor := 0;  // Zerando valor para recalcular o texto
end;

procedure TACBrExtenso.SetMoedas(aValue: String);
begin
  if (fsIdiomaObj.StrInteiros = aValue) then
    Exit;

  AtribuirIdiomaCustom;
  fsInteiros := aValue;
  fsIdiomaObj.StrInteiros := aValue;
  fsValor := 0;  // Zerando valor para recalcular o texto
end;

procedure TACBrExtenso.SetTexto(aValue: String);
begin
  // Função apenas para tornar visível a Propriedade Texto no Object Inspector
end;

procedure TACBrExtenso.SetValor(aValue: Double);
begin
  if (fsValor = aValue) then
    Exit;

  fsValor := aValue;
  fsTexto := fsIdiomaObj.TraduzValor(fsValor);
end;

procedure TACBrExtenso.SetZeroAEsquerda(const Value: Boolean);
begin
  fsIdiomaObj.ZeroAEsquerda := Value;
  fsValor := 0;  // Reseta Valor para recalcular o extenso
end;

procedure TACBrExtenso.SetFormato(const aValue: TACBrExtensoFormato);
begin
  fsIdiomaObj.Formato := aValue;
  fsValor := 0;  // Reseta Valor para recalcular o extenso
end;

procedure TACBrExtenso.AtribuirIdiomaCustom;
begin
  if (fsIdioma = idiCustom) then
    Exit;

  if Assigned(fsIdiomaObj) then
    fsIdiomaObj.Free;

  Idioma := idiCustom;
  fsIdiomaObj := TACBrExtensoIdiomaCustom.Create;
end;

function TACBrExtenso.ValorToTexto(AValor: Double; AFormato: TACBrExtensoFormato): String;
begin
  Formato := AFormato;
  Valor   := AValor;
  Result  := Texto;
end;

end.
