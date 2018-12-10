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
|* 29/03/2016: Wislei de Brito Fernandes
|*  - Primeira Versao ACBrBALSaturno
|*
|* 10/10/2016: Elias César Vieira
|*  - Refatoração de ACBrBALSaturno
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALSaturno;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALSaturno }

  TACBrBALSaturno = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  SysUtils,
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} synaser, Windows{$ENDIF};

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
begin
  Result := 0;

  if (aResposta = EmptyStr) then
    Exit;

  wAchouE := (Pos('E', UpperCase(aResposta)) > 0);
  wAchouO := (Pos('O', UpperCase(aResposta)) > 0);

  // Se encontrar a letra 'E' (Estável) ou 'O' (Oscilante), captura o peso da
  // posição 1 a 7 da string
  if wAchouE or wAchouO then
  begin
    if wAchouE then
      wPosEO := Pos('E', UpperCase(aResposta))
    else
      wPosEO := Pos('O', UpperCase(aResposta));

    wResposta := Copy(aResposta, 0, wPosEO - 1);

    { Removendo caracteres especiais, caso encontre algum }
    wResposta := StringReplace(wResposta, '°', '0', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '±', '1', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '²', '2', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '³', '3', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '´', '4', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, 'µ', '5', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '¶', '6', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '·', '7', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '¸', '8', [rfReplaceAll]);
    wResposta := StringReplace(wResposta, '¹', '9', [rfReplaceAll]);
  end;

  if (Length(wResposta) > 0) then
  begin
    try
      Result := StrToFloat(wResposta);
    except
      case wResposta[1] of
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

