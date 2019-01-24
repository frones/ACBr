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
|* 27/08/2010: João Paulo
|*  - Primeira Versao ACBrBALLucasTec
|*
|* 11/10/2016: Elias César Vieira
|*  - Refatoração de ACBrBALLucasTec
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALLucasTec;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALLucasTec }

  TACBrBALLucasTec = class(TACBrBALClass)
  private
    function StrToPeso(const StrPeso, aResposta: AnsiString): Double;

  public
    constructor Create(AOwner: TComponent);

    procedure LeSerial(MillisecTimeOut: Integer = 500); override;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils,
  ACBrConsts, ACBrUtil,
  {$IFDEF COMPILER6_UP}
    DateUtils, StrUtils
  {$ELSE}
    ACBrD5, Windows
  {$ENDIF};

{ TACBrBALGertecSerial }

function TACBrBALLucasTec.StrToPeso(const StrPeso, aResposta: AnsiString): Double;
var
  I, wPos: Integer;
  wStr: String;
begin
  Result := 0;
  wPos   := Pos(UpperCase(StrPeso), UpperCase(aResposta));

  if (wPos > 0) then
  begin
    wStr := Trim(Copy(aResposta, 1, wPos - 1));

    for I := Length(wStr) downto 1 do
    begin
      if (wStr[I] = ' ') then
      begin
        wStr := RightStr(wStr, Length(wStr) - I + 1);

        { Ajustando o separador de Decimal corretamente }
        wStr := StringReplace(wStr, '.', DecimalSeparator, [rfReplaceAll]);
        wStr := StringReplace(wStr, ',', DecimalSeparator, [rfReplaceAll]);
        try
          if (Pos(DecimalSeparator, wStr) > 0) then  { Já existe ponto decimal ? }
            Result := StrToFloat(wStr)
          else
            Result := (StrToInt(wStr) / 1000);
        except
          Result := 0;
        end;

        Break;
      end;
    end
  end;
end;

constructor TACBrBALLucasTec.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'LucasTec';
end;

procedure TACBrBALLucasTec.LeSerial(MillisecTimeOut: Integer);
var
  Pesos: array[1..4] of Double;
  I: Integer;
begin
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';

  try
    for I := 1 to 4 do
    begin
      fpUltimaResposta := fpDevice.LeString(MillisecTimeOut, 0, #13);
      GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);

      Pesos[I] := InterpretarRepostaPeso(fpUltimaResposta);
    end;

    if (Pesos[2] = Pesos[3]) and (Pesos[3] = Pesos[4]) then
      fpUltimoPesoLido := Pesos[4]
    else
      fpUltimoPesoLido := -1;

  except
    { Peso não foi recebido (TimeOut) }
    On E: Exception do
    begin
      fpUltimoPesoLido := -9;
      fpUltimaResposta := E.Message;
    end;
  end;

  GravaLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido)+' , Resposta: ' + fpUltimaResposta);
end;

function TACBrBALLucasTec.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  PesoL, PesoB, PesoT: Double;
  wResposta: AnsiString;
begin
  Result := 0;

  if (aResposta = EmptyStr) then
    Exit;

  //aResposta := #2+'0000 000001 20/06/05 50.00 kg B  0.60 kg T  49.40 kg L'+#13#180;
  wResposta := Copy(aResposta, Pos(#2, aResposta) + 1, Length(aResposta));
  Result    := 0;

  if not (Pos('kg B ', wResposta) > 0) then
    wResposta := 'I';

  if (Length(wResposta) > 1) then
  begin
    PesoL := StrToPeso('kg L', wResposta);
    PesoT := StrToPeso('kg T', wResposta);
    PesoB := StrToPeso('kg B', wResposta);

    if (PesoT > PesoB) then
    begin
      Result    := -2;
      wResposta := 'N';
    end
    else if (PesoL > 0) then
      Result := PesoL
    else
      Result := PesoB;
  end;
end;

end.

