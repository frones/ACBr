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
|* 05/11/2011: Levindo Damasceno
|*  - Primeira Versao ACBrBALFilizola
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALMagellan;

interface
uses ACBrBALClass,
     Classes;

type
  TACBrBALMagellan = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses ACBrUtil, ACBrConsts,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, synaser, Windows{$ENDIF},
     SysUtils, Math ;

{ TACBrBALGertecSerial }

constructor TACBrBALMagellan.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Magellan' ;
end;

function TACBrBALMagellan.LePeso( MillisecTimeOut : Integer) : Double;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' TX -> '+#087 );
  fpDevice.Serial.Purge ;           { Limpa a Porta }
  fpDevice.EnviaString( #87 );      { Envia comando solicitando o Peso }
  sleep(200) ;

  LeSerial( MillisecTimeOut );

  Result := fpUltimoPesoLido ;
end;

procedure TACBrBALMagellan.LeSerial( MillisecTimeOut : Integer) ;
Var
  Resposta : AnsiString ;
  Decimais : Integer ;
  St2      : AnsiChar ;
  PI,PF    : Integer ;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  Decimais := 1000 ;
  Try
     fpUltimaResposta := fpDevice.Serial.RecvPacket( MillisecTimeOut );
     GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- '+fpUltimaResposta );

     if Length(fpUltimaResposta) > 20 then
      begin
        { Protocolo A
          [ STX ] [ S1 ] [ PPPPPP ] [ S2 ] [ TTTTTT ] [ UUUUUU ] [ CR ] [ CS ]
          S1 = 1 byte - Status 1
          PPPPPP = 6 bytes - peso
          S2 = 1 byte - Status 2
          TTTTTT = 6 bytes - Preço Total
          UUUUUU = 6 bytes - Preço/kg
          CS = 1 byte - Checksum. O cálculo do checksum é feito pelo complemento
             de 2 da soma de todos os bytes transmitidos de STX, incluindo o CR.

          S1 - STATUS 1
          bit 0 = motion flag
          bit 1 = print flag
          bit 2 = data do sistema ( 0 = pesagem e 1 = data )
          bit 3 = out of range
          bit 4 = tipo de balança ( 0 = Prix II/ Prix III e 1 = Prix I )
          bit 5 = número de casas decimais ( 0 = 2 casas e 1 = 0 casas )
          bit 6 = autorização de totalização ( 0 = não e 1 = sim totaliza )

          S2 - STATUS 2
          bit 0 = sétimo dígito no preço total ( 0 = sem 1 = com )
          bit 1 = reservado
          bit 2 = reservado
          bit 3 = casas decimais no peso ( 0 = 3 casas e 1 = 2 casas )
          bit 4 = reservado
          bit 5 = reservado
          bit 6 = operação com tara ( 0 = sem tara e 1 = com tara ) deve
                  imprimir peso líquido         }

        St2 := fpUltimaResposta[9] ;
        if TestBit(Ord(St2),3) then   { Bit 3 de ST2 ligado = 2 casas decimais }
           Decimais := 100 ;
        Resposta := Trim(Copy(fpUltimaResposta,3,6));
      end

     else
      begin
      { Protocolo B = [ ENQ ] [ STX ] [ PESO ] [ ETX ]
        Protocolo C = [ STX ] [ PESO ] [ CR ]
        Linha Automacao = [ STX ] [ PPPPP ] [ ETX ]  - Peso Estável;
                          [ STX ] [ IIIII ] [ ETX ]  - Peso Instável;
                          [ STX ] [ NNNNN ] [ ETX ]  - Peso Negativo;
                          [ STX ] [ SSSSS ] [ ETX ]  - Peso Acima (Sobrecarga) }

        PI := pos(STX, fpUltimaResposta) ;
        PF := pos(ETX, fpUltimaResposta) ;
        if PF = 0 then                       { Não achou ETX, procura por CR }
           PF := pos(CR, fpUltimaResposta) ;
        if PF = 0 then                       { Não achou CR, usa toda a String }
           PF := Length(fpUltimaResposta) + 1 ;

        Resposta := Trim( copy( fpUltimaResposta, PI+1, PF-PI-1 )) ;
      end ;

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
