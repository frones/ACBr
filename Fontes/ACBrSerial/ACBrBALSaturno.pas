{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Wislei de Brito Fernandes                     }
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

unit ACBrBALSaturno;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};
  
type

  { TACBrBALSaturno }

  TACBrBALSaturno = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils, ACBrUtil.Strings,
  {$IFDEF COMPILER6_UP}
   DateUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};

{ TACBrBALSaturno }

constructor TACBrBALSaturno.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Saturno';
end;

function TACBrBALSaturno.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wAchouE, wAchouO: Boolean;
  wPosEO: Integer;
  wResposta: AnsiString;

  function SanitizarRespostaPeso(const aResposta: AnsiString) : AnsiString;
  begin
    Result := Trim(aResposta);

    if Result = EmptyStr then
      Exit;

    Result := StringReplace(Result, '°', '0', [rfReplaceAll]);
    Result := StringReplace(Result, '±', '1', [rfReplaceAll]);
    Result := StringReplace(Result, '²', '2', [rfReplaceAll]);
    Result := StringReplace(Result, '³', '3', [rfReplaceAll]);
    Result := StringReplace(Result, '´', '4', [rfReplaceAll]);
    Result := StringReplace(Result, 'µ', '5', [rfReplaceAll]);
    Result := StringReplace(Result, '¶', '6', [rfReplaceAll]);
    Result := StringReplace(Result, '·', '7', [rfReplaceAll]);
    Result := StringReplace(Result, '¸', '8', [rfReplaceAll]);
    Result := StringReplace(Result, '¹', '9', [rfReplaceAll]);
    Result := StringReplace(Result, #13, '', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '', [rfReplaceAll]);
    Result := StringReplace(Result, '[CR]', '', [rfReplaceAll]);
    Result := StringReplace(Result, '[LF]', '', [rfReplaceAll]);
  end;

begin
  Result := 0;
  { Removendo caracteres especiais, caso encontre algum }
  wResposta := SanitizarRespostaPeso(aResposta);
  if (Trim(wResposta) = EmptyStr) then
    Exit;

  wResposta := UpperCase(wResposta);
  wAchouE := (Pos('E', wResposta) > 0);
  wAchouO := (Pos('O', wResposta) > 0);

  // Se encontrar a letra 'E' (Estável) ou 'O' (Oscilante), captura o peso da
  // posição 1 a 7 da string
  if wAchouE or wAchouO then
  begin
    if wAchouE then
      wPosEO := Pos('E', wResposta)
    else
      wPosEO := Pos('O', wResposta);

    wResposta := Copy(wResposta, 0, wPosEO - 1);
  end
  else
  begin
    wResposta := Copy(wResposta, 1, 9);
  end;

  if (Length(wResposta) > 0) then
  begin
    try
      Result := StrToFloat(wResposta);
    except
      case PadLeft(Trim(wResposta),1)[1] of
        'I': Result := -1;   { Instavel }
        'N': Result := -2;   { Peso Negativo }
        'S': Result := -10;  { Sobrecarga de Peso }
      else
        Result := 0;
      end;
    end;
  end
  else
    Result := 0;
end;

end.

