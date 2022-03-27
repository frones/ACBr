{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Fabio Farias                                    }
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

unit ACBrBALFilizola;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};
  
type

  { TACBrBALFilizola }

  TACBrBALFilizola = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function LePeso(MillisecTimeOut: Integer = 3000): Double; override;

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils, strutils,
  ACBrConsts, ACBrUtil.Strings,
  {$IFDEF COMPILER6_UP}
    DateUtils
  {$ELSE}
    ACBrD5, Windows
  {$ENDIF};

{ TACBrBALGertecSerial }

constructor TACBrBALFilizola.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Filizola';
end;

function TACBrBALFilizola.LePeso(MillisecTimeOut: Integer): Double;
begin
  { A Filizola pode responder com Instavel inicalmente, mas depois ela poderia
    estabilizar... Portanto utilizará a função AguardarRespostaPeso }
  Result := AguardarRespostaPeso(MillisecTimeOut, True);
end;

function TACBrBALFilizola.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  Pi, Pf: Integer;
begin
  Result := 0;

  { Retira STX, ETX }
  wResposta := aResposta;
  Pi := pos(STX, wResposta);  // Inicio do Peso
  Pf := PosEx(ETX, wResposta, Pi+1);  // Fim do Peso
  if (Pf = 0) then
    Pf := PosEx(CR, wResposta, Pi+1);  // Fim do Peso, tratativa para o modelo C&F C6MT
  if (Pf = 0) then
    Pf := Length(wResposta)+1;

  wResposta := Trim(copy(wResposta, Pi+1, (Pf-Pi)-1));
  if (wResposta = EmptyStr) then
    Exit;

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    if (Length(wResposta) > 10) then
      Result := (StrToFloat(Copy(wResposta, 1, 6)) / 1000)
    else if (Pos(DecimalSeparator, wResposta) > 0) then
      Result := StrToFloat(wResposta)
    else
      Result := (StrToInt(wResposta) / 1000)
  except
    case PadLeft(Trim(wResposta),1)[1] of
      'I': Result := -1;   { Instavel }
      'N': Result := -2;   { Peso Negativo }
      'S': Result := -10;  { Sobrecarga de Peso }
    else
      Result := 0;
    end;
  end;
end;

end.
