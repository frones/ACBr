{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016 Daniel Simoes de Almeida               }
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
|* 31/03/2016: Paulo Henrique de Castro
|*  - Primeira Versao ACBrBALSaturno
******************************************************************************}
{$I ACBr.inc}

unit ACBrBALAFTS;

interface
uses ACBrBALClass, ACBrConsts,
     Classes;

type
          
  TACBrBALAFTS = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses ACBrUtil,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, SysUtils, Windows{$ENDIF},
     SysUtils, Math ;

{ TACBrBALAFTS }

constructor TACBrBALAFTS.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'AFTS' ;
end;

function TACBrBALAFTS.LePeso( MillisecTimeOut : Integer) : Double;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' TX -> '+#05 );
  fpDevice.Limpar ;                 { Limpa a Porta }
  fpDevice.EnviaString( #05 );      { Envia comando solicitando o Peso }
  sleep(200) ;

  LeSerial( MillisecTimeOut );

  Result := fpUltimoPesoLido ;
end;

procedure TACBrBALAFTS.LeSerial(MillisecTimeOut: Integer);
Var
  Resposta : AnsiString ;
  i : integer;
  pesoAFTS :String;
  resultado :String;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;
  resultado := '';


  Try
     fpUltimaResposta := fpDevice.LeString( MillisecTimeOut );
     GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- '+fpUltimaResposta );

     fpUltimaResposta := Copy(fpUltimaResposta,1,10);
     Resposta := Trim( copy( fpUltimaResposta, 2, 7 )) ;

     pesoAFTS := Resposta;
     for i := length(pesoAFTS) downto 1 do
     begin
       resultado := resultado + pesoAFTS[i];
     end;
      Resposta := resultado ;
      
     { Ajustando o separador de Decimal corretamente }
     Resposta := StringReplace(Resposta, '.', DecimalSeparator, [rfReplaceAll]);
     Resposta := StringReplace(Resposta, ',', DecimalSeparator, [rfReplaceAll]);
     try
       fpUltimoPesoLido := StrToFloat(Resposta)

     except
        case Resposta[1] of
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
