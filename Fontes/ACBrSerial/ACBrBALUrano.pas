{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Ivan Carlos Martello                   }
{                                       Daniel Simoes de Almeida               }
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
|* 17/02/2009: Ivan Carlos Martello
|*  - Primeira Versao ACBrBALUrano
|*
|* 11/10/2016 - Elias César Vieira
|*  - Refatoração de ACBrBALUrano
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALUrano;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALUrano }

  TACBrBALUrano = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function LePeso(MillisecTimeOut: Integer = 3000): Double; override;

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  ACBrConsts, SysUtils,
  {$IFDEF COMPILER6_UP}
  DateUtils
  {$ELSE}
  ACBrD5, Windows
  {$ENDIF};

{ TACBrBALGertecSerial }

constructor TACBrBALUrano.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Urano';
end;

function TACBrBALUrano.LePeso(MillisecTimeOut : Integer) : Double;
begin
  Result := AguardarRespostaPeso(MillisecTimeOut, True);
end;

function TACBrBALUrano.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
   wResposta: String;
   wQtd, wPos: integer;
begin
  //aResposta := '1BT11BA131BN01BS21BD41BQ1931BB * PESO: 5,10kg1BE1BP01';
  Result    := 0;
  wResposta := aResposta;

  wPos := Pos(':', wResposta);
  if (wPos = 0) then
    wPos := Pos('N0', wResposta);

  if (Copy(wResposta, Pos('PESO', wResposta) - 2, 1) = ' ') then
    wResposta := 'I'
  else if (Copy(wResposta, wPos + 1, 1) = '-') then
    wResposta := 'N';

  if (Length(wResposta) > 1) then
  begin
    wQtd      := (Pos('g', wResposta) - 2);
    wQtd      := wQtd - (wPos + 1);
    wResposta := Copy(wResposta, wPos + 2, wQtd); //123456
  end;

  if (wResposta = EmptyStr) then
    Exit;

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    { Já existe ponto decimal ? }
    if (Pos(DecimalSeparator, wResposta) > 0) then
      Result := StrToFloat(wResposta)
    else
      Result := (StrToInt(wResposta) / 1000);
  except
    case Trim(wResposta)[1] of
      'I': Result := -1;     { Instavel }
      'N': Result := -2;     { Peso Negativo }
      //'S': Result := -10;  { Sobrecarga de Peso }
    else
      Result := 0;
    end;
  end;
end;

end.

