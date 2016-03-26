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
|* 30/08/2007: Marcio Jose da Silva
|*  - Primeira Versao ACBrBALRinnert
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALRinnert;

interface
uses ACBrBALClass, Classes;

const STX = #02 ;
      ETX = #03 ;

type
  TACBrBALRinnert = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses ACBrBAL,  
     {$IFDEF Delphi6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
     SysUtils, ACBrConsts;

{ TACBrBALGertecSerial }

constructor TACBrBALRinnert.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Rinnert' ;
end;

function TACBrBALRinnert.LePeso( MillisecTimeOut : Integer) : Double;
Var TempoFinal : TDateTime ;
begin
  { A Rinnert pode responder com Instavel inicalmente, mas depois ela poderia
    estabilizar... Portanto o Loop abaixo tenta ler um Peso válido até o limite
    de tempo estabelecido em "MilliSecTimeOut" ser atingido ou um Peso valido
    retornado }
  Result     := 0 ;
  TempoFinal := IncMilliSecond(now,MillisecTimeOut) ;

  while (Result <= 0) and (TempoFinal > now) do
  begin
     fpDevice.Serial.Purge ;
     fpDevice.EnviaString( #05 );      { Envia comando solicitando o Peso }
     sleep(200) ;
     MillisecTimeOut := MilliSecondsBetween(now,TempoFinal) ;

     LeSerial( MillisecTimeOut );

     Result := fpUltimoPesoLido ;
  end ;
end;

procedure TACBrBALRinnert.LeSerial( MillisecTimeOut : Integer) ;
Var
  Resposta : WideString ;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  Try
     fpUltimaResposta := fpDevice.Serial.RecvPacket( MillisecTimeOut );

     { Retira STX, ETX }
     Resposta := fpUltimaResposta ;
     if Pos('+',Resposta) > 0 then
     begin
       Resposta := copy(Resposta,Pos('+',Resposta)+1,7) ;
     end;

     if copy(Resposta,Length(Resposta),1) = ETX then
        Resposta := copy(Resposta,1,Length(Resposta)-1) ;

     { Ajustando o separador de Decimal corretamente }
     Resposta := StringReplace(Resposta,'.',DecimalSeparator,[rfReplaceAll]) ;
     Resposta := StringReplace(Resposta,',',DecimalSeparator,[rfReplaceAll]) ;

     try
       fpUltimoPesoLido := StrToFloat(Resposta)
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
  end ;
end;

end.
