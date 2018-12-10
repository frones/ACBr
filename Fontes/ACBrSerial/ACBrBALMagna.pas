{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Fabio Farias                           }
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
|* 04/10/2005: Fabio Farias  / Daniel Simões de Almeida
|*  - Primeira Versao ACBrBALFilizola
|* 05/11/2011: Levindo Damasceno
|*  - Primeira Versao ACBrBALFilizola
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALMagna;

interface

uses ACBrBALClass, Classes;

type

  { TACBrBALMagna }

  TACBrBALMagna = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
    procedure SolicitarPeso; override;
  end;

implementation

uses
  ACBrConsts, SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF};

{ TACBrBALGertecSerial }

constructor TACBrBALMagna.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Magna';
end;

function TACBrBALMagna.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wPosIni, wPosFim, wDecimais: Integer;
  wResposta: AnsiString;
begin
  Result    := 0;
  wResposta := Copy(aResposta, 1, Pos(#10, aResposta) - 1);
  wPosIni   := Pos(' ', wResposta);
  wPosFim   := Pos('k', wResposta);
  wDecimais := 1000;

  if (wPosFim = 0) then
    wPosFim := Length(wResposta);

  wResposta := Copy(wResposta, wPosIni, wPosFim - wPosIni);

  if (wResposta = EmptyStr) then
    Exit;

  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    if (Length(wResposta) > 10) then
      Result := (StrToFloat(Copy(wResposta, 1, 6)) / wDecimais)
    else if (Pos(DecimalSeparator, wResposta) > 0) then
      Result := StrToFloat(wResposta)
    else
      Result := (StrToInt(wResposta) / wDecimais);
  except
    case Trim(wResposta)[1] of
    //'I' : Result := -1  ;  { Instavel }
    //'S' : Result := -10 ;  { Sobrecarga de Peso }
      'N' :                  { Peso Negativo }
      begin
        Result    := -2;
        wResposta := ' Peso Negativo ';
      end;
    else
      Result := 0;
    end;
  end
end;

procedure TACBrBALMagna.SolicitarPeso;
begin
  GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' TX -> ' + #80);
  fpDevice.Limpar;
  fpDevice.EnviaString(#80);  { Envia comando Solicitando o Peso }
end;

end.
