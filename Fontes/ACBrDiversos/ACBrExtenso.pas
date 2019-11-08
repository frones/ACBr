{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 16/08/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrExtenso
|* 30/09/2006: Daniel Simoes de Almeida
|*  - Adicionada a propriedade ZeroAEsquerda : Boolean (default true)
|*    Quando True , o valor: 0,21 = Zero Reais e Vinte e Um Centavos
|*    Quando False, o valor: 0,21 = Vinte e Um Centavos
|* 22/02/2007: Daniel Simoes de Almeida
|*  - Adicionada a propriedade Formato : TACBrExtensoFormato (default extPadrao)
|*    extPadrao -> 15,10 - Quinze Reais e Dez Centavos
|*    extDolar  -> 15,10 - Dolares Americanos Quinze com 010/100 
******************************************************************************}

{$I ACBr.inc}

unit ACBrExtenso;

interface
uses SysUtils, Classes,
  ACBrBase;

const
   cMilharSin  : array[0..2] of string = ('Bilhão','Milhão','Mil') ;
   cMilharPlu  : array[0..2] of string = ('Bilhões','Milhões','Mil') ;
   cUnidade    : array[0..8] of string =
      ('Um','Dois','Três','Quatro','Cinco','Seis','Sete','Oito','Nove') ;
   cDez        : array[0..9] of string =
      ('Dez','Onze','Doze','Treze','Quatorze','Quinze','Dezesseis','Dezessete',
       'Dezoito','Dezenove') ;
   cDezenas    : array[0..7] of string =
      ('Vinte','Trinta','Quarenta','Cinquenta','Sessenta','Setenta','Oitenta',
       'Noventa') ;
   cCentenas   : array[0..9] of string =
      ('Cem','Cento','Duzentos','Trezentos','Quatrocentos','Quinhentos','Seiscentos',
       'Setecentos','Oitocentos','Novecentos') ;

type

  TACBrExtensoFormato = (extPadrao, extDolar) ;

{ Componente ACBrExtenso }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrExtenso = class( TACBrComponent )
  private
    { Propriedades do Componente ACBrExtenso }
    fsMoeda: String;
    fsMoedas: String;
    fsCentavos: String;
    fsCentavo: String;
    fsValor: Double;
    fsTexto: String;
    fsZeroAEsquerda: Boolean;
    fsFormato: TACBrExtensoFormato;

    Function ExtensoAux(const Str3 : String) : String ;
    procedure TraduzValor(const Value: Double);
    procedure SetfsTexto(const Value: String);
    procedure SetZeroAEsquerda(const Value: Boolean);
    procedure SetFormato(const Value: TACBrExtensoFormato);
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override ;
    function ValorToTexto( AValor : Double; AFormato : TACBrExtensoFormato = extPadrao  ) : string;

  published
    property StrMoeda      : String read fsMoeda    write fsMoeda   ;
    property StrMoedas     : String read fsMoedas   write fsMoedas  ;
    property StrCentavo    : String read fsCentavo  write fsCentavo ;
    property StrCentavos   : String read fsCentavos write fsCentavos;
    property Valor         : Double read fsValor    write TraduzValor stored false;
    property Texto         : String read fsTexto    write SetfsTexto  stored false;
    property ZeroAEsquerda : Boolean read fsZeroAEsquerda
       write SetZeroAEsquerda default True ;
    property Formato       : TACBrExtensoFormato read fsFormato write SetFormato
       default extPadrao ;
end ;

implementation

uses Math, ACBrUtil;

{ TACBrExtenso }

constructor TACBrExtenso.Create(AOwner: TComponent);
begin
  inherited Create( AOwner ) ;

  fsMoeda   := 'Real' ;
  fsMoedas  := 'Reais' ;
  fsCentavo := 'Centavo' ;
  fsCentavos:= 'Centavos' ;
  fsValor   := 0 ;
  fsTexto   := '' ;
  fsZeroAEsquerda := True ;
  fsFormato       := extPadrao ;
end;

destructor TACBrExtenso.Destroy;
begin

  inherited Destroy ;
end;

procedure TACBrExtenso.TraduzValor(const Value: Double);
Var 
  Inteiros: Int64;
  Casa, Decimais: Integer;
  StrInteiros: String;
  aStrCasas: array[0..3] of string;
begin
  if Value = fsValor then exit ;

  if Value > 999999999999.99 then
     raise Exception.Create('Valor acima do permitido');

  fsValor     := RoundTo( Value, -2) ;  { Apenas 2 decimais }
  fsTexto     := '' ;
  Inteiros    := Trunc( fsValor ) ;
  Decimais    := Round( Frac(fsValor) * 100 ) ;
  StrInteiros := IntToStrZero( Inteiros, 12 ) ;

  if Inteiros > 0 then   { Se tiver inteiros processe }
   begin
     { Achando a CASAS dos Bilhoes, Milhoes, Mil, e Cem }
     for Casa := 0 to 3 do
        aStrCasas[ Casa ] := copy( StrInteiros, (Casa * 3)+1, 3);

     for Casa := 0 to 3 do
     begin
        if StrToIntDef(aStrCasas[Casa],0) = 0  then { Casa vazia ? }
           continue ;

        if fsTexto <> '' then { Se ja existe texto, concatena com "," ou "e"}
           if Casa = 3 then
              fsTexto := fsTexto + ' e '
           else
              fsTexto := fsTexto + ', ' ;

        fsTexto := fsTexto + ExtensoAux( aStrCasas[ Casa ] ) ;

        if Casa < 3 then { Se for acima da casa dos Cem pegue um titulo }
           { Se a CASA tiver valor de UM usa singular, senao usa o plural }
           if StrToIntDef(aStrCasas[Casa],0) = 1 then
              fsTexto := fsTexto + ' ' + cMilharSin[Casa]
           else
              fsTexto := fsTexto + ' ' + cMilharPlu[Casa]  ;
     end ;

     { Se nao possui valores na casa dos MIL ou dos CEM, concatena ' de' }
     if StrToIntDef(aStrCasas[2] + aStrCasas[3],0) = 0 then
        fsTexto := fsTexto + ' de' ;
   end
  else
     if fsZeroAEsquerda then
        fsTexto := 'Zero' ;

  { Se o valor total for UM usa moeda singular, senao no Plurar }
  if fsTexto <> '' then
     if fsFormato = extDolar then
      begin
        If Inteiros = 1 then
           fsTexto := fsMoeda  + ' ' + fsTexto
        else
           fsTexto := fsMoedas + ' ' + fsTexto ;
      end  
     else
      begin
        If Inteiros = 1 Then
           fsTexto := fsTexto + ' ' + fsMoeda
        else
           fsTexto := fsTexto + ' ' + fsMoedas ;
      end ;

  { PROCESSANDO OS DECIMAIS }
  if fsFormato = extDolar then
   begin
     if fsTexto <> '' then  { Se ja possui algum Texto adiciona o ' com ' }
        fsTexto := fsTexto + ' com ' ;

     fsTexto := fsTexto + IntToStrZero(Decimais,2) + '/100'  ;
   end
  else
     if Decimais > 0 then
     begin
        if fsTexto <> '' then  { Se ja possui algum Texto adiciona o ' e ' }
           fsTexto := fsTexto + ' e ' ;

        fsTexto := fsTexto + ExtensoAux( IntToStrZero(Decimais,3) ) ;

        { Se o valor total dos decimais for UM usa singular, senao no Plurar }
        If Decimais = 1 Then
           fsTexto := fsTexto + ' ' + fsCentavo
        else
           fsTexto := fsTexto + ' ' + fsCentavos ;
     end ;

  {Verificando se é na casa de 1 HUM mil}
  if fsFormato<>extDolar then
    if (fsValor>=1000.00) then
      if (fsValor<2000.00) then
        fsTexto := 'Hum'+copy(fsTexto, 3, Length(fsTexto));

end;

function TACBrExtenso.ExtensoAux(const Str3 : String) : String;
{ Funcao de auxilio a Extenso. Retorna uma string contendo o extenso
  de Str3. Esta funcao e apenas monta o extenso de uma string de 3
  digitos, (nao acressenta a moeda ou Titulo (Mil, Milhao, etc..))
  Str3 -> String com 3 casas com Valor a transformar em extenso }
Var POS1, POS2, POS3 : Integer ;
Var Resultado : String;
begin

  Resultado := '' ;

  POS1 := StrToIntDef(copy(Str3,1,1),0) ;
  POS2 := StrToIntDef(copy(Str3,2,1),0) ;
  POS3 := StrToIntDef(copy(Str3,3,1),0) ;

  if POS1 > 0 then    { Se possuir numero na casa da centena processe }
  begin
    { Se nao possuir numeros a seguir e POS1 for o numero UM entao = "Cem" }
     if (POS1 = 1) and ((POS2 + POS3) = 0) then
        POS1 := 0 ;

     Resultado := cCentenas[ POS1 ] ;
  end ;

  if POS2 > 0 then    { Se possuir numero na casa da dezena processe }
  begin
     if Resultado <> '' then  { Se ja possui algum Texto adiciona o ' e ' }
        Resultado := Resultado + ' e ' ;

     if POS2 = 1 then {  Se for na casa dos dez usa vetor de Dezenas }
      begin
        Resultado := Resultado + cDez[ POS3 ] ;   { Dez, Onze, Doze... }
        POS3 := 0 ;
      end
     else
        Resultado := Resultado + cDezenas[ POS2 - 2 ] ; {Vinte, Trinta...}
  end ;

  if POS3 > 0 then    {  Se possuir numero na casa da unidade processe }
  begin
     if Resultado <> '' then  { Se ja possui algum Texto adiciona o ' e ' }
        Resultado := Resultado + ' e ' ;

     Resultado := Resultado + cUnidade[ POS3 - 1 ] ;
  end ;

  Result := ACBrStr(Resultado);
end;

procedure TACBrExtenso.SetfsTexto(const Value: String);
begin
  { funçao existente apenas para permitir a Propriedade Texto ser visivel no
    Object Inspector...  }
end;

procedure TACBrExtenso.SetZeroAEsquerda(const Value: Boolean);
begin
  fsZeroAEsquerda := Value;
  fsValor         := 0 ;  { Reseta Valor para recalcular o extenso }
end;

procedure TACBrExtenso.SetFormato(const Value: TACBrExtensoFormato);
begin
  fsFormato := Value;
  fsValor   := 0 ;  { Reseta Valor para recalcular o extenso }
end;

function TACBrExtenso.ValorToTexto(AValor: Double;
  AFormato: TACBrExtensoFormato): string;
begin
  Formato := AFormato;
  Valor   := AValor;
  Result  := Texto;
end;

end.
