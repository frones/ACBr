{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Ivan Carlos Martello                                                }
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
|* 26/08/2019: Andre Adami
|*  - Primeira versão
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALWeightechWT3000_ABS;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALWeightechWT3000_ABS }

  TACBrBALWeightechWT3000_ABS = class(TACBrBALClass)
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

{ TACBrBALWeightechWT3000_ABS }

constructor TACBrBALWeightechWT3000_ABS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Weightech WT3000_ABS';
end;

function TACBrBALWeightechWT3000_ABS.LePeso(MillisecTimeOut : Integer) : Double;
begin
  Result := AguardarRespostaPeso(MillisecTimeOut, True);
end;

function TACBrBALWeightechWT3000_ABS.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: String;
  wPos: integer;
begin
  Result    := 0;
  wResposta := aResposta;

  {
  Exemplo:  +0.876kg quando o peso estiver estável e valor de peso líquido
  S T , N T , +   0 . 8 7 6 k g 0D 0A

   1 2    3   4 5     6     7 8 9 10 11 12 13 14     15 16    17   18
  HEAD1   ,   HEAD2   ,             DATA            UNIDADE   CR   LF


  HEAD1 ( 2 BYTES )
   OL  -   Sobrecarga , Subcarga
   ST  -   Display está estável
   US  -   Display está instável

  HEAD2 ( 2 BYTES )
   NT  -   Modo NET (Líquido)
   GS  -   Modo GROSS (Bruto)
   }

  // quando em transmissão contínua, pode concatenar vários envios na resposta
  wPos := Pos(LF, wResposta);
  if (wPos > 0) then
    wResposta := Copy(wResposta, wPos + 1, 16);

  if Length(wResposta) <> 16 then
    Exit;

  if wResposta[1] = '1' then
  begin
    // instável
    Result := -1;
    Exit;
  end;

  wResposta := Copy(wResposta, 8, 7); // peso líquido

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  Result := StrToFloatDef(wResposta, 0);
end;

end.

