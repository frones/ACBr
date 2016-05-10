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

{$I ACBr.inc}

unit ACBrBALGenerica;

interface
uses ACBrBALClass, ACBrConsts,
     Classes;

type

  TACBrBALGenerica = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses ACBrUtil,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
     SysUtils, Math ;

{ TACBrBALGenerica }

constructor TACBrBALGenerica.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Genérica' ;
end;

function TACBrBALGenerica.LePeso( MillisecTimeOut : Integer) : Double;
Var
  TempoFinal : TDateTime ;
begin
   Result := 0;
   fpUltimoPesoLido := 0 ;
   fpUltimaResposta := '' ;
   TempoFinal := IncMilliSecond(now, MillisecTimeOut) ;

   GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' TX -> '+#05 );
   
   while (Result <= 0) and (TempoFinal > now) do
   begin
      fpDevice.Limpar ;                 { Limpa a Porta }
      fpDevice.EnviaString( #05 );      { Envia comando solicitando o Peso }
      Sleep(200) ;
      MillisecTimeOut := MilliSecondsBetween(Now, TempoFinal) ;

      LeSerial( MillisecTimeOut );

      Result := fpUltimoPesoLido ;
   end;
end;

procedure TACBrBALGenerica.LeSerial( MillisecTimeOut : Integer);
Var 
  Resposta : AnsiString ;
  Decimais : Integer ;
begin
   fpUltimoPesoLido := 0 ;
   fpUltimaResposta := '' ;

  Decimais := 1000 ;
   Try
     fpUltimaResposta := fpDevice.LeString( MillisecTimeOut );
      GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- '+fpUltimaResposta );

      Resposta := Trim( Copy( fpUltimaResposta, fpPosIni, fpPosFim)) ;

      { Ajustando o separador de Decimal corretamente }
      Resposta := StringReplace(Resposta, '.', DecimalSeparator, [rfReplaceAll]);
      Resposta := StringReplace(Resposta, ',', DecimalSeparator, [rfReplaceAll]);

      try
        if pos(DecimalSeparator,Resposta) > 0 then  { Já existe ponto decimal ? }
            fpUltimoPesoLido := StrToFloat(Resposta)
         else
           fpUltimoPesoLido := StrToInt(Resposta) / Decimais ;
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
