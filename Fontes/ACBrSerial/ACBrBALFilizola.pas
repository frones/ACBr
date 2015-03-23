{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Fabio Farias                           }
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
|* 04/10/2005: Fabio Farias  / Daniel Simões de Almeida
|*  - Primeira Versao ACBrBALFilizola
|* 25/05/2005: Daniel Simoes de Almeida
|*  - Adaptado para funcionar com vários modelos de Filizola (MF, BP) permitindo
|*    variação na posição do ponto flutante
|* 16/02/2007: Juliano Pereira dos Santos
|*  - Adaptado para funcionar com modelo "CS"
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALFilizola;

interface
uses ACBrBALClass,
     Classes;

type
  TACBrBALFilizola = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses ACBrConsts, math,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
     SysUtils ;

{ TACBrBALGertecSerial }

constructor TACBrBALFilizola.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Filizola' ;
end;

function TACBrBALFilizola.LePeso( MillisecTimeOut : Integer) : Double;
Var TempoFinal : TDateTime ;
begin
  { A Filizola pode responder com Instavel inicalmente, mas depois ela poderia
    estabilizar... Portanto o Loop abaixo tenta ler um Peso válido até o limite
    de tempo estabelecido em "MilliSecTimeOut" ser atingido ou um Peso valido
    retornado }
  Result := -1 ;
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;
  TempoFinal := IncMilliSecond(now,MillisecTimeOut) ;

  while (Result = -1) and (TempoFinal > now) do
  begin
     GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' TX -> '+#05 );
     fpDevice.Serial.Purge ;
     fpDevice.EnviaString( #05 );      { Envia comando solicitando o Peso }
     sleep(200) ;
     MillisecTimeOut := max( MilliSecondsBetween(now,TempoFinal), 1000) ;

     LeSerial( MillisecTimeOut );

     Result := fpUltimoPesoLido ;
  end ;
end;

procedure TACBrBALFilizola.LeSerial( MillisecTimeOut : Integer) ;
Var Resposta : AnsiString ;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  Try
     fpUltimaResposta := fpDevice.Serial.RecvPacket( MillisecTimeOut );
     GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- '+fpUltimaResposta );

     { Retira STX, ETX }
     Resposta := fpUltimaResposta;
     if copy(Resposta, 1, 1) = STX then
        Resposta := copy(Resposta, 2, Length(Resposta));

     if copy(Resposta, Length(Resposta), 1) = ETX then
        Resposta := copy(Resposta, 1, Length(Resposta) - 1);
     
     { Ajustando o separador de Decimal corretamente }
     Resposta := StringReplace(Resposta, '.', DecimalSeparator, [rfReplaceAll]);
     Resposta := StringReplace(Resposta, ',', DecimalSeparator, [rfReplaceAll]);

     try
        if Length(Resposta) > 10 then
           fpUltimoPesoLido := StrToFloat(copy(Resposta, 1, 6)) / 1000
        else if pos(DecimalSeparator, Resposta) > 0 then
           fpUltimoPesoLido := StrToFloat(Resposta)
        else
           fpUltimoPesoLido := StrToInt(Resposta) / 1000
     except
        case Trim(Resposta)[1] of
          'I' : fpUltimoPesoLido := -1  ;  { Instavel }
          'N' : fpUltimoPesoLido := -2  ;  { Peso Negativo }
          'S' : fpUltimoPesoLido := -10 ;  { Sobrecarga de Peso }
        else
           fpUltimoPesoLido := 0 ;
        end;
     end;
  except
     { Peso não foi recebido (TimeOut) }
     fpUltimoPesoLido := -9 ;
  end ;

  GravaLog('              UltimoPesoLido: '+FloatToStr(fpUltimoPesoLido)+' , Resposta: '+Resposta );
end;

end.
