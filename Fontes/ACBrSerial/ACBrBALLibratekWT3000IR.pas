{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Andre Adami                                     }
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

{ ******************************************************************************
  |* Historico
  |*
  |* 19/12/2019 - Andre Adami
  |*  - Primeira Versao ACBrBALLibratekWT3000IR
  ****************************************************************************** }

{$I ACBr.inc}

unit ACBrBALLibratekWT3000IR;

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  TACBrBALLibratekWT3000IR = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

Uses
  SysUtils, Math,
  ACBrConsts, ACBrUtil,
  {$IFDEF COMPILER6_UP}
   DateUtils, StrUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};

{ TACBrBALLibratekWT3000IRWT3000IR }

constructor TACBrBALLibratekWT3000IR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpModeloStr := 'Libratek WT3000IR';
end;

function TACBrBALLibratekWT3000IR.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  TempResposta: string;
begin
  TempResposta := Trim(Copy(aResposta, 1, 19));

  //ST,GS,+   0.298  kg[CR][LF]

  //ST,GS,+   0.298  kg[CR][LF]

  { Ajustando o separador de Decimal corretamente }
  TempResposta := trim(StringReplace(TempResposta, '.', DecimalSeparator, [rfReplaceAll]));
  TempResposta := trim(StringReplace(TempResposta, ',', DecimalSeparator, [rfReplaceAll]));

  case AnsiIndexStr(Copy(aResposta, 1, 2), ['ST', 'OL', 'US']) of
    0:
      fpUltimoPesoLido := StrToFloat(copy(TempResposta, 8, 8)); // Estável
    1:
      if fpUltimaResposta[7] = '-' then
        fpUltimoPesoLido := -2 // Peso negativo
      else
        fpUltimoPesoLido := -10; // Sobrecarga de peso
    2:
      fpUltimoPesoLido := -1; // Instável
  else
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;
end;

end.
