{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchéla Rubinho                         }
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

unit ACBrBALWeightechWT27R_ETH;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};
  
type

  { TACBrbalWeightechWT27R_ETH }

  TACBrbalWeightechWT27R_ETH = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils, Math,
  ACBrConsts, ACBrUtil.Strings,
  {$IFDEF COMPILER6_UP}
   DateUtils, StrUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};

{ TACBrbalWeightechWT27R_ETH }

constructor TACBrbalWeightechWT27R_ETH.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'WeightechWT27R_ETH';
end;

function TACBrbalWeightechWT27R_ETH.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  decimais: integer;
begin
{
E B , k g , B : - 0 0 0 0 0 0 , T : 0 0 0 0 0 0 , L : - 0 0 0 0 0 0 CR LF

EB,kg,B:-000000,T:000000,L:-000000CRLF

E  >>  E=Estável / I= instável
B  >>  Tipo de peso mostrado no display: B = peso bruto L= peso líquido
,  >>  separador
k  >>  Unidade de medida (2 bytes)
g  >>  Unidade de medida (2 bytes)
,  >>  separador
B  >>  2 dígitos identificando tipo de peso
:  >>  2 dígitos identificando tipo de peso
-  >>  Se valor positivo= se negativo= -
0  >>  6 dígitos valor PESO BRUTO
0  >>  6 dígitos valor PESO BRUTO
0  >>  6 dígitos valor PESO BRUTO
0  >>  6 dígitos valor PESO BRUTO
0  >>  6 dígitos valor PESO BRUTO
0  >>  6 dígitos valor PESO BRUTO
,  >>  separador
T  >>  2 dígitos identificando tipo de peso
:  >>  2 dígitos identificando tipo de peso
0  >>  6 dígitos valor TARA
0  >>  6 dígitos valor TARA
0  >>  6 dígitos valor TARA
0  >>  6 dígitos valor TARA
0  >>  6 dígitos valor TARA
0  >>  6 dígitos valor TARA
,  >>  separador
L  >>  2 dígitos identificando tipo de peso
:  >>  2 dígitos identificando tipo de peso
-  >>  Se valor positivo= se negativo= -
0  >>  6 dígitos valor PESO LIQUIDO
0  >>  6 dígitos valor PESO LIQUIDO
0  >>  6 dígitos valor PESO LIQUIDO
0  >>  6 dígitos valor PESO LIQUIDO
0  >>  6 dígitos valor PESO LIQUIDO
0  >>  6 dígitos valor PESO LIQUIDO
CR >>  TERMINADOR
LF >>  TERMINADOR
}

  Result := 0;

  if (aResposta = EmptyStr) then
    Exit;

  // Verifica Instabilidade
  if PadLeft(Trim(aResposta),1)[1] = 'I' then
  begin
    Result := -1;    // Instavel
    Exit;
  end;

  // Verifica Peso negativo
  if PadLeft(Trim(aResposta),9)[9] = '-' then
  begin
    Result := -2;   // Peso Negativo
    Exit;
  end;

  wResposta := Trim(Copy(Trim(aResposta),10,6));

  if Length(wResposta) <> 6 then
    Exit;

  // Verifica Sobrecarga
  if AnsiUpperCase(PadLeft(wResposta,6)) = '999999' then
  begin
    Result := -10;  // Sobrecarga de Peso
    Exit;
  end;

  if Result < 0 then
    Exit;

  decimais := 1;
  if Pos(DecimalSeparator,wResposta) = 0 then
  begin
    if AnsiLowerCase(Trim(Copy(Trim(aResposta),4,2))) <> 'kg' then
      decimais := 1000
  end;

  // Ajustando o separador de Decimal corretamente
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    Result := StrToFloat(wResposta);
    Result := Result / decimais;
  except
    Result := 0;
  end;
end;

end.
