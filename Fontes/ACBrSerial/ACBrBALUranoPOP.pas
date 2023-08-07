{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:    Ivan Carlos Martello                         }
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

unit ACBrBALUranoPOP;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrBALUranoPOP }

  TACBrBALUranoPOP = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function LePeso(MillisecTimeOut: Integer = 3000): Double; override;

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils,
  ACBrConsts, ACBrUtil.Strings,
  {$IFDEF COMPILER6_UP}
    DateUtils, StrUtils
  {$ELSE}
    ACBrD5, Windows
  {$ENDIF};

{ TACBrBALGertecSerial }

constructor TACBrBALUranoPOP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Urano';
end;

function TACBrBALUranoPOP.LePeso(MillisecTimeOut : Integer) : Double;
begin
  fpUltimoPesoLido := -1;

  SolicitarPeso;
  Sleep(600);

  LeSerial(MillisecTimeOut);
  Result := fpUltimoPesoLido;
end;

function TACBrBALUranoPOP.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  wPos, wQtd, wsPosTara: Integer;
begin
  Result    := 0;
  wResposta := aResposta;

  wPos := Pos('PESO L:', wResposta);  // Protocolo USE-CB2
  if (wPos > 0) then
  begin
    wResposta := Copy(wResposta,wPos, 16);
    wPos := Pos(':', wResposta);

    if (Length(wResposta) > 1) then
    begin
      wQtd      := (Pos('g', wResposta) - 2);
      wQtd      := wQtd - (wPos + 1);
      wResposta := Copy(wResposta, wPos + 2, wQtd); //123456
    end;
  end
  else
  begin
    wsPosTara := Pos('TARA:', wResposta);
    if (wsPosTara > 0) then
    begin
       wResposta := '';
    end
    else
    begin
         wPos := Pos('kg', wResposta);  // Protocolo USE-P2
         if (wPos > 0) then
            wResposta := copy(wResposta, wPos-7, 6)
         else
             wResposta := '';
    end;
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
    case  PadLeft(Trim(wResposta),1)[1] of
      'I': Result := -1;     { Instavel }
      'N': Result := -2;     { Peso Negativo }
      //'S': Result := -10;  { Sobrecarga de Peso }
    else
      Result := 0;
    end;
  end;
end;

end.

