{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Andre Adami                                    }
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
|* 04/09/2019: Andre Adami
|*  - Primeira versão
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALDigitron_UL;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};
  
type

  { TACBrBALDigitron_UL }

  TACBrBALDigitron_UL = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

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

{ TACBrBALDigitron_UL }

constructor TACBrBALDigitron_UL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Digitron_UL';
end;

function TACBrBALDigitron_UL.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  wDecimais: Integer;
begin
  Result := -9;

  if (aResposta = EmptyStr) then
    Exit;

  //Fonte: "Manual de Operação Balanças dePiso, Balança de Bancada e Barra de Pesagem"
  //-- manual digitron_UL.pdf
  //A balança pode ser configurada como Modo Contadora ou Modo Pesadora.
  //A string transmitida no Modo Contadora é visualizada no Hyper Terminal como
  // "D00005" (sem aspas)
  //A string transmitida no Modo Pesadora é visualizada no Hyper Terminal como
  // "E1234.5" (sem aspas)

  wDecimais := 100;
  wResposta := Copy(aResposta, 1, 8);

  //TODO: O primeiro byte que está sendo retirado abaixo além de mostrar o modo
  // também traz o status da balança.
  // Mas esse status não está sendo tratado...

  { Retira D }
  if (Copy(wResposta, 1, 1) = 'D') then
    wResposta := Copy(wResposta, 2, Length(wResposta));

  { Retira F }
  if (Copy(wResposta, 1, 1) = 'F') then
    wResposta := Copy(wResposta, 2, Length(wResposta));

  { Retira @ }
  if (Copy(wResposta, 1, 1) = '@') then
    wResposta := Copy(wResposta, 2, Length(wResposta));


  if (wResposta = EmptyStr) then
    Exit;

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    { Já existe ponto decimal ? }
    if (Pos(DecimalSeparator, String(wResposta)) > 0) then
      Result := StrToFloat(wResposta)
    else
      Result := (StrToInt(wResposta) / wDecimais);
  except
    case PadLeft(Trim(wResposta),1)[1] of
      'I': Result := -1;   { Instavel }
      'N': Result := -2;   { Peso Negativo }
      'S': Result := -10;  { Sobrecarga de Peso }
    else
      Result := -9;
    end;
  end;
end;

end.
