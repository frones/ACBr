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
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALToledoBCS21;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALToledoBCS21 }

  TACBrBALToledoBCS21 = class(TACBrBALClass)
  private
    //fpProtocolo: AnsiString;
    fpDecimais: Integer;

    function InterpretarProtocoloC(const aResposta: AnsiString): AnsiString;
  public
    constructor Create(AOwner: TComponent);

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  ACBrUtil, ACBrConsts, SysUtils,
  {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, synaser, Windows{$ENDIF};

{ TACBrBALToledo }

constructor TACBrBALToledoBCS21.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Toledo BCS21';
  //fpProtocolo := 'Não Definido';
  fpDecimais  := 1000;
end;

function TACBrBALToledoBCS21.InterpretarProtocoloC(const aResposta: AnsiString): AnsiString;
var
  wPosIni, wPosFim: Integer;
begin
  { Protocolo C = [ STX ] [ PESO ] [ CR ]
    Linha Automacao:
      [ STX ] [ PPPPP ] [ ETX ]  - Peso Estável;
      [ STX ] [ IIIII ] [ ETX ]  - Peso Instável;
      [ STX ] [ NNNNN ] [ ETX ]  - Peso Negativo;
      [ STX ] [ SSSSS ] [ ETX ]  - Peso Acima (Sobrecarga) }

  wPosIni     := PosLast(STX, aResposta);
  wPosFim     := PosEx(CR, aResposta, wPosIni + 1);

  if (wPosFim < 0) then
    wPosFim := Length(aResposta) + 1;  // Não achou? ...Usa a String inteira

  Result := Trim(Copy(aResposta, wPosIni + 1, wPosFim - wPosIni - 1));
end;

function TACBrBALToledoBCS21.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
//  wPosIni: Integer;
  wResposta: AnsiString;
//  teste: string;
begin
  Result  := 0;

  if (aResposta = EmptyStr) then
    Exit;

//  wPosIni := PosLast(STX, aResposta);

  wResposta := InterpretarProtocoloC(aResposta);

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

//  teste := Copy(wResposta,3,8);

  try
    case AnsiIndexText(Copy(wResposta, 1, 2), ['S+','S-','D+', 'D-']) of
      0: Begin
           Result := StrToFloat(Copy(wResposta,3,8));
         End;
      1: Result := -2;
      2: Result := -1;
      3: Result := -1;
    end;
  except
    Result := -9;
  end;
end;

end.
