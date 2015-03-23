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
|* 27/08/2010: João Paulo
|*  - Primeira Versao ACBrBALLucasTec
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALLucasTec;

interface
uses ACBrBALClass,
     Classes;

type
  TACBrBALLucasTec = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses
   ACBrConsts,
  {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
  SysUtils ;

{ TACBrBALGertecSerial }

constructor TACBrBALLucasTec.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  fpModeloStr := 'LucasTec' ;
end;

function TACBrBALLucasTec.LePeso(MillisecTimeOut : Integer) : Double;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' TX -> '+#05 );
  fpDevice.Serial.Purge;
  fpDevice.EnviaString( #05 );      { Envia comando solicitando o Peso }
  sleep(200) ;

  LeSerial( MillisecTimeOut );

  Result := fpUltimoPesoLido ;
end;

procedure TACBrBALLucasTec.LeSerial( MillisecTimeOut : Integer) ;
var Resposta: String;
    Peso, PesoL, PesoB, PesoT:Double;
    i:Integer;
    pesos:array[1..4] of Double;

    function StrToPeso(StrPeso:string):Double;
    var ii,pp:Integer;
        s:String;
    begin
      Result:=0;
      pp:=Pos(UpperCase(StrPeso),UpperCase(Resposta));
      if pp>0 then begin
         s:=trim(Copy(Resposta,1,pp-1));
         for ii:=Length(s) downto 1 do
           if s[ii]=' ' then begin
              s:=RightStr(s,Length(s)-ii+1);
              { Ajustando o separador de Decimal corretamente }
              s := StringReplace(s, '.', DecimalSeparator, [rfReplaceAll]);
              s := StringReplace(s, ',', DecimalSeparator, [rfReplaceAll]);
              try
                if pos(DecimalSeparator, s) > 0 then  { Já existe ponto decimal ? }
                   Result := StrToFloat(s)
                  else
                   Result := StrToInt(s) / 1000 ;
              except
                Result := 0 ;
              end;
              break;
           end;

      end;
    end;

begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;
  try
    for i:=1 to 4 do begin
      //fpUltimaResposta := fpDevice.Serial.RecvPacket( MillisecTimeOut);
      fpUltimaResposta := fpDevice.Serial.RecvTerminated( MillisecTimeOut, #13 );
      GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- '+fpUltimaResposta );

      //fpUltimaResposta := #2+'0000 000001 20/06/05 50.00 kg B  0.60 kg T  49.40 kg L'+#13#180;
      fpUltimaResposta := Copy(fpUltimaResposta,Pos(#2,fpUltimaResposta)+1,Length(fpUltimaResposta));

      Resposta := fpUltimaResposta;

      if not ((Pos('kg B ',Resposta)>0)) then
      begin
        Resposta := 'I';
      end;

      Peso := 0;
      if Length(Resposta) > 1  then
      begin
         PesoL:=StrToPeso('kg L');
         PesoT:=StrToPeso('kg T');
         PesoB:=StrToPeso('kg B');
         if PesoT > PesoB then begin
            Peso:=-2;
            Resposta:='N';
           end
           else
            if PesoL>0 then
               Peso:=PesoL
              else
               Peso:=PesoB;
      end;

      pesos[i]:=Peso;
    end;

    if (pesos[2]=pesos[3]) and (pesos[3]=pesos[4]) then
       fpUltimoPesoLido := pesos[4]
      else begin
       fpUltimoPesoLido := -1 ;
    end;

  except
     { Peso não foi recebido (TimeOut) }
     on E:Exception do begin
        fpUltimoPesoLido := -9;
        fpUltimaResposta := e.Message;
     end;
  end ;

  GravaLog('              UltimoPesoLido: '+FloatToStr(fpUltimoPesoLido)+' , Resposta: '+Resposta );
end;

end.

