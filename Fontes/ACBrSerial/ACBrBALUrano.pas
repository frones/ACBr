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
|* 17/02/2009: Ivan Carlos Martello
|*  - Primeira Versao ACBrBALUrano
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALUrano;

interface
uses ACBrBALClass,
     Classes;

type
  TACBrBALUrano = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses
  ACBrConsts,
  {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
  SysUtils, math ;

{ TACBrBALGertecSerial }

constructor TACBrBALUrano.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  fpModeloStr := 'Urano' ;
end;

function TACBrBALUrano.LePeso(MillisecTimeOut : Integer) : Double;
var
  TempoFinal : TDateTime ;
begin
  Result := 0 ;
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;
  TempoFinal := IncMilliSecond(now,MillisecTimeOut) ;

  while (Result <= 0) and (TempoFinal > now) do
  begin
    GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' TX -> '+#05 );
    fpDevice.Serial.Purge;
    fpDevice.EnviaString(#05); { Envia comando solicitando o Peso }
    sleep(200);
    MillisecTimeOut := max( MilliSecondsBetween(now,TempoFinal), 1000) ;
    LeSerial( MillisecTimeOut );
    Result := fpUltimoPesoLido;
  end;
end;

procedure TACBrBALUrano.LeSerial( MillisecTimeOut : Integer) ;
 var
   Resposta: String;
   Quantos, PosDelim: integer;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;
  try
    fpUltimaResposta := fpDevice.Serial.RecvPacket( MillisecTimeOut );
    GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- '+fpUltimaResposta );

    //fpUltimaResposta := '1BT11BA131BN01BS21BD41BQ1931BB * PESO: 5,10kg1BE1BP01';
    Resposta := fpUltimaResposta;

    PosDelim := pos(':', Resposta);
    if PosDelim = 0 then
       PosDelim := pos('N0', Resposta);

    if Copy(Resposta, pos('PESO', Resposta)-2, 1) = ' ' then
      Resposta := 'I'
    else if Copy(Resposta, PosDelim + 1, 1) = '-' then
      Resposta := 'N';

    if Length(Resposta) > 1  then
    begin
      Quantos := (pos('g', Resposta) - 2);
      Quantos := Quantos - (PosDelim + 1);
      Resposta := Copy(Resposta, PosDelim + 2, Quantos); //123456
    end;

    { Ajustando o separador de Decimal corretamente }
    Resposta := StringReplace(Resposta, '.', DecimalSeparator, [rfReplaceAll]);
    Resposta := StringReplace(Resposta, ',', DecimalSeparator, [rfReplaceAll]);

    try
      if pos(DecimalSeparator, Resposta) > 0 then  { Já existe ponto decimal ? }
        fpUltimoPesoLido := StrToFloat(Resposta)
      else
        fpUltimoPesoLido := StrToInt(Resposta) / 1000 ;
    except
      case Trim(Resposta)[1] of
        'I' : fpUltimoPesoLido := -1  ;  { Instavel }
        'N' : fpUltimoPesoLido := -2  ;  { Peso Negativo }
        //'S' : fpUltimoPesoLido := -10 ;  { Sobrecarga de Peso }
      else
        fpUltimoPesoLido := 0 ;
      end;
    end;
  except
     { Peso não foi recebido (TimeOut) }
     fpUltimoPesoLido := -9;
  end ;

  GravaLog('              UltimoPesoLido: '+FloatToStr(fpUltimoPesoLido)+' , Resposta: '+Resposta );
end;

end.

