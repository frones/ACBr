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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 23/01/2020: Fabiano Cunha
|*  - Primeira Versao ACBrBALToledoTi420
|*
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALToledoTi420;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrBALToledoTi420 }

  TACBrBALToledoTi420 = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils, Math, ACBrConsts,
  {$IFDEF COMPILER6_UP}
   DateUtils, StrUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};

{ TACBrBALToledoTi420 }

constructor TACBrBALToledoTi420.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Toledo Ti420';
end;

procedure TACBrBALToledoTi420.LeSerial(MillisecTimeOut: Integer);
var
  wPesos: array[1..5] of Double;
  I: Integer;
begin
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';

  try
    for I := 1 to 5 do
    begin
      fpUltimaResposta := fpDevice.LeString(MillisecTimeOut, 0, #13);
      GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);

      wPesos[I] := InterpretarRepostaPeso(fpUltimaResposta);
    end;

    if (wPesos[3] = wPesos[4]) and (wPesos[4] = wPesos[5]) then
      fpUltimoPesoLido := wPesos[5]
    else
      fpUltimoPesoLido := -1;
  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;

  GravarLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) +
            ' - Resposta: ' + fpUltimaResposta);
end;

function TACBrBALToledoTi420.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wPos: Integer;
  wResposta: AnsiString;
begin
  Result := 0;

  if (aResposta = EmptyStr) then
    Exit;

  wPos := Pos(#96, aResposta);

  if (wPos > 0) then
  begin
    wResposta := Copy(aResposta, wPos + 1, Length(aResposta));
    wResposta := Copy(wResposta, 1, 6);
    // Para a linha abaixo, queira ver: https://www.projetoacbr.com.br/forum/topic/54533-balan%C3%A7a-toledo-2180/
    //Insert('.', wResposta, 6);
  end
  else
    wResposta := 'I';

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    Result := StrToFloat(wResposta);
  except
    Result := 0;
  end;
end;

end.
