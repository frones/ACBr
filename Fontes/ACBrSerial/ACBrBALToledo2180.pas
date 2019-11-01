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
|* 04/10/2005: Daniel Simões de Almeida
|*  - Primeira Versao ACBrBALToledo
|* 11/04/2007 Daniel Simões de Almeida
|*  - Corrigido para trabalhar com diversos protocolos da Toledo
|*
|* 11/10/2016 - Elias César Vieira
|*  - Refatoração de ACBrBALToledo2180
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALToledo2180;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALToledo2180 }

  TACBrBALToledo2180 = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils, Math,
  ACBrConsts, ACBrUtil,
  {$IFDEF COMPILER6_UP}
   DateUtils, StrUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};

{ TACBrBALToledo2180 }

constructor TACBrBALToledo2180.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Toledo 2180';
end;

procedure TACBrBALToledo2180.LeSerial(MillisecTimeOut: Integer);
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
      GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);

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

  GravaLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) +
           ' - Resposta: ' + fpUltimaResposta);
end;

function TACBrBALToledo2180.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
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
