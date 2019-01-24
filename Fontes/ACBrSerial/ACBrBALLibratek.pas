{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016 Marciano da Rocha                      }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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

{ ******************************************************************************
  |* Historico
  |*
  |* 12/07/2016 - Marciano da Rocha
  |*  - Primeira Versao ACBrBALLibratek
  ****************************************************************************** }

{$I ACBr.inc}
unit ACBrBALLibratek;

interface

uses
  ACBrBALClass, Classes;

type

  TACBrBALLibratek = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);
    function LePeso(MillisecTimeOut: Integer = 3000): Double; override;
    procedure LeSerial(MillisecTimeOut: Integer = 500); override;
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

{ TACBrBALLibratek }

constructor TACBrBALLibratek.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Libratek';
  fpPosIni := 7;
  fpPosFim := 9;
end;

function TACBrBALLibratek.LePeso(MillisecTimeOut: Integer): Double;
Var
  TempoFinal: TDateTime;
begin
  Result := 0;
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';
  TempoFinal := IncMilliSecond(now, MillisecTimeOut);

  while (Result <= 0) and (TempoFinal > now) do
  begin
    MillisecTimeOut := MilliSecondsBetween(now, TempoFinal);

    LeSerial(MillisecTimeOut);

    Result := fpUltimoPesoLido;
  end;
end;

procedure TACBrBALLibratek.LeSerial(MillisecTimeOut: Integer);
Var
  Resposta: AnsiString;
begin
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';

  Try
    fpUltimaResposta := fpDevice.LeString(MillisecTimeOut);
    GravaLog('- ' + FormatDateTime('hh:nn:ss:zzz', now) + ' RX <- ' + fpUltimaResposta);

    Resposta := Trim(Copy(fpUltimaResposta, fpPosIni, fpPosFim));

    { Ajustando o separador de Decimal corretamente }
    Resposta := StringReplace(Resposta, '.', DecimalSeparator, [rfReplaceAll]);
    Resposta := StringReplace(Resposta, ',', DecimalSeparator, [rfReplaceAll]);

    case AnsiIndexStr(Copy(fpUltimaResposta, 1, 2), ['ST', 'OL', 'US']) of
      0:
        fpUltimoPesoLido := StrToFloat(Resposta); // Estável
      1:
        if fpUltimaResposta[7] = '-' then
          fpUltimoPesoLido := -2 // Peso negativo
        else
          fpUltimoPesoLido := -10; // Sobrecarga de peso
      2:
        fpUltimoPesoLido := -1; // Instável
    end;
  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;

  GravaLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) + ' , Resposta: ' + Resposta);
end;

end.
