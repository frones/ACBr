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

unit ACBrBALUranoPOP;

interface
uses ACBrBALClass,
     Classes;

type
  TACBrBALUranoPOP = class( TACBrBALClass )
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

constructor TACBrBALUranoPOP.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  fpModeloStr := 'Urano' ;
end;

function TACBrBALUranoPOP.LePeso(MillisecTimeOut : Integer) : Double;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  fpDevice.Serial.Purge;
  fpDevice.EnviaString(#05); { Envia comando solicitando o Peso }
  sleep(600);

  LeSerial( MillisecTimeOut );

  Result := fpUltimoPesoLido;
end;

procedure TACBrBALUranoPOP.LeSerial( MillisecTimeOut : Integer) ;
 var
   Resposta: String;
   Quantos, PosDelim: integer;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;
  try
    fpUltimaResposta := fpDevice.Serial.RecvPacket( MillisecTimeOut );
    //fpUltimaResposta := 'DATA:  00/00/00 VALID.: 00/00/00      TARA:   0.000kg       PESO L:  1.542kg      R$/kg:      0.00      TOTAL R$:      0.00200000000000';
    Resposta := fpUltimaResposta;
    Resposta := Copy(Resposta, Pos('PESO L:', Resposta), 16);

    PosDelim := pos(':', Resposta);

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
end;

end.

