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

{$I ACBr.inc}

unit ACBrBALToledo2090;

interface

uses
  ACBrBALClass, Classes, Dialogs;

type

  { TACBrBALToledo2090 }

  TACBrBALToledo2090 = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    procedure SolicitarPeso; override;

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  ACBrConsts, SysUtils, Math,
  {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, synaser, Windows{$ENDIF};

{ TACBrBALToledo }

constructor TACBrBALToledo2090.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Toledo 2090';
end;

procedure TACBrBALToledo2090.SolicitarPeso;
begin
  GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' TX -> ' + #05);
  //fpDevice.Limpar;          { Limpa a Porta }
  fpDevice.EnviaString(#05);  { Envia comando solicitando o Peso }
end;

function TACBrBALToledo2090.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  wStrListDados: TStringList;
  wDecimais: Integer;
begin
  Result := 0;

  if (aResposta = EmptyStr) then
    Exit;

  Result    := 0;
  wResposta := '';
  wDecimais := 100;

  wStrListDados := TStringList.Create;
  try
    wStrListDados.Text := StringReplace(aResposta, #$D, #13, [rfReplaceAll, rfIgnoreCase]);

    { PACOTE DE DADOS INVALIDO PARA PROCESSAR }
    if ((Copy(wStrListDados[1], 2, 1) <> #2) and (Copy(wStrListDados[1], 1, 1) <> #2)) or
       ((Length(wStrListDados[1]) <> 17) and (Length(wStrListDados[1]) <> 16)) then
      Exit;

    if (Length(wStrListDados[1]) = 16) then
      wDecimais := 1000;

    {APENAS BLOCO PROCESSADO}
    wResposta := wStrListDados[1];
    wResposta := Copy(wStrListDados[1], 5, 7);

    if (Length(wResposta) <= 0) then
      Exit;

    { Ajustando o separador de Decimal corretamente }
    wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
    wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);
    try
      { Já existe ponto decimal ? }
      if (Pos(DecimalSeparator, wResposta) > 0)then
        Result := StrToFloat(wResposta)
      else
        Result := (StrToInt(wResposta) / wDecimais);

      case AnsiIndexText(Copy(wStrListDados[1], 3, 1), ['x','r','s']) of
        0: Result := Result;         { Instavel }
        1: Result := Result * (-1);  { Peso Negativo }
        2: Result := -10;            { Sobrecarga de Peso }
      end;
    except
      Result := 0;
    end;
  finally
    wStrListDados.Free;
  end;
end;

end.
