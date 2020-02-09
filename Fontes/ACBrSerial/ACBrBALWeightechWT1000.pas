{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Daniel Sonda                                  }
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

{******************************************************************************
|* Historico
|*
|* 30/07/2019: Daniel Sonda
|*  - Primeira versão
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALWeightechWT1000;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrBALWeightechWT1000 }

  TACBrBALWeightechWT1000 = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function LePeso(MillisecTimeOut: Integer = 3000): Double; override;

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils, ACBrConsts,
  {$IFDEF COMPILER6_UP}
   DateUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};

{ TACBrBALWeightechWT1000 }

constructor TACBrBALWeightechWT1000.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Weightech WT1000';
end;

function TACBrBALWeightechWT1000.LePeso(MillisecTimeOut : Integer) : Double;
begin
  Result := AguardarRespostaPeso(MillisecTimeOut, True);
end;

function TACBrBALWeightechWT1000.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: String;
  wPos: integer;
begin
  Result    := 0;
  wResposta := aResposta;

  { S , B B B . B B B , T T T . T T T , L L L . L L L CR LF

    S: Flag de estabilidade e pode assumir os seguintes valores:
      0: Peso estável;
      1: Peso instável.
    B: 7 bytes de peso bruto incluindo o ponto decimal e sinal de peso negativo;
    T: 7 bytes de peso tara incluindo o ponto decimal e sinal de peso negativo;
    L: 7 bytes de peso líquido incluindo o ponto decimal e sinal de peso negativo;
    CR Carriage return (0X0D)
    LF Line feed (0x0A) }

  // quando em transmissão contínua, pode concatenar vários envios na resposta
  wPos := Pos(LF, wResposta);
  if (wPos > 0) then
    wResposta := Copy(wResposta, wPos + 1, 25);

  if Length(wResposta) <> 25 then
    Exit;

  if wResposta[1] = '1' then
  begin
    // instável
    Result := -1;
    Exit; 
  end;

  wResposta := Copy(wResposta, 19, 7); // peso líquido

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  Result := StrToFloatDef(wResposta, 0);
end;

end.

