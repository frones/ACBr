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
|* 27/05/2014: Laércio S Amici | Emerson Virissimo da Silva
|*  - Primeira Versao ACBrBALLider
|*
      Protocolo Lider   -   Baseado no modelo LD2052
      SOH 00000. #32 <E|I> STX

      E=Estável  I=Instável

      A balança Lider manda leituras continuamente para a COM, não necessita de
      um comando para ler o peso.
      A idéia é buscar o último peso estável lido, então a busca deve iniciar
      no final do Buffer. Uma outra implementação possível seria tentar as 3
      últimas leituras estáveis pra ver se coincidem, mas no momento não vejo
      necessidade.

      Configuração de leitura:
      Baud Rate = 2400, Data Bits = 8, Parity = None, Stop Bits = 1,
                        Handshaking = None
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALLider;

interface

uses ACBrBALClass,
     Classes;

type

  TACBrBALLider = class( TACBrBALClass )
  public

    constructor Create(AOwner: TComponent);

    function LePeso(MillisecTimeOut: integer = 3000): Double; override;

    procedure LeSerial(MillisecTimeOut: integer = 500); override;
  end ;


implementation

// TESTE DE LEITURA UTILIZANDO LOG GERADO NA BALANÇA
// {$DEFINE DEBUG}

uses ACBrConsts, math,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
     SysUtils;


{ Retorna a posição da Substr em Str a partir do final }
function RightPos( ASubstr, AStr: string ): integer;
var
  iLen, iLenSub: integer;
begin
  Result := 0;

  iLen := Length(AStr);
  iLenSub := Length(ASubstr);

  if iLenSub <= iLen then
  begin
    Result := iLen - (iLenSub-1);


    while (Result > 0) and
          (not SameText( ASubstr, Copy(AStr, Result, iLenSub) )) do
      Dec(Result);

  end;
end;


{ TACBrBALLider }

constructor TACBrBALLider.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Lider' ;
end;


function TACBrBALLider.LePeso(MillisecTimeOut: integer): Double;
var
  TempoFinal: TDateTime;
begin
  { baseado inicialmente na leitura da Filisola }

  Result := -1 ;

  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  TempoFinal := IncMilliSecond(Now, MillisecTimeOut);

  while (Result <= -1) and (TempoFinal > Now) do
  begin
    fpDevice.Serial.Purge;

    // Não precisa enviar comando de leitura de peso
    //fpDevice.EnviaString( #05 );      { Envia comando solicitando o Peso }

    Sleep(200);

    MillisecTimeOut := Max(MilliSecondsBetween(Now, TempoFinal), 1000);

    LeSerial( MillisecTimeOut );

    Result := UltimoPesoLido;
  end;
end;

procedure TACBrBALLider.LeSerial(MillisecTimeOut: integer);
var
  Resposta: AnsiString;
  StatusPeso: AnsiChar;
  iPos: integer;

{$IFDEF DEBUG}
  StrDebug: TStringList;
{$ENDIF}

begin

  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  try
    { Protocolo Lider   -   Baseado no modelo LD2052
      SOH 00000. #32 <E|I> STX

      A balança Lider manda leituras continuamente para a COM, não necessita de
      um comando para ler o peso. E=Estável I=Instável
      A idéia é buscar o último peso estável lido, então a busca deve iniciar
      no final do Buffer. Uma outra implementação possível seria tentar as 3
      últimas leituras estáveis pra ver se coincidem, mas no momento não vejo
      necessidade.

      Configuração de leitura:
      Baud Rate = 2400, Data Bits = 8, Parity = None, Stop Bits = 1, Handshaking = None
      ----------------------}
{$IFDEF DEBUG}
    StrDebug := TStringList.Create;
    StrDebug.LoadFromFile('LiderDebug.txt');
    fpUltimaResposta := StrDebug.Text;
    StrDebug.Free;
{$ELSE}
    fpUltimaResposta := fpDevice.Serial.RecvPacket( MillisecTimeOut );
    GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- '+fpUltimaResposta );
{$ENDIF}

    Resposta := fpUltimaResposta;

    { Buscar último peso estável no buffer }
    iPos := RightPos('E'+ STX, Resposta);

    { Se não achou leitura estáve. buscar último peso instável no buffer }
    if iPos = 0 then
      iPos := RightPos('I'+ STX, Resposta);

    { Se achou, isolar a leitura }
    if (iPos > 0) then
    begin
      Resposta := Copy(Resposta, 1, iPos + 1);

      iPos := RightPos(SOH, Resposta);

      if (iPos > 0) then
        Resposta := Copy(Resposta, iPos, Length(Resposta));
    end;

    { Retira SOH, STX }
    if Copy(Resposta, 1, 1) = SOH then
      Resposta := Copy(Resposta, 2, Length(Resposta));

    if Copy(Resposta, Length(Resposta), 1) = STX then
      Resposta := Copy(Resposta, 1, Length(Resposta) - 1);

    { Substituir o espaço que vem após o ponto }
    Resposta := StringReplace(Resposta, ' ', '0', [rfReplaceAll]);

    { Ajustando o separador de Decimal corretamente }
    Resposta := StringReplace(Resposta, '.', DecimalSeparator, [rfReplaceAll]);
    Resposta := StringReplace(Resposta, ',', DecimalSeparator, [rfReplaceAll]);

    try
      StatusPeso := ' ';
      if Length(Resposta) >= 1 then
      begin
        StatusPeso := Copy(Resposta, Length(Resposta), 1)[1];

        if (StatusPeso in ['E', 'I']) then
          Resposta := Copy(Resposta, 1, Length(Resposta) - 1);
      end;

      fpUltimoPesoLido := StrToFloat(Resposta);

      case StatusPeso of
        'I': fpUltimoPesoLido := -1;  { Instavel }
        'E': fpUltimoPesoLido := StrToFloat(Resposta);  { Estável }

          //'N' : fpUltimoPesoLido := -2  ;  { Peso Negativo }
          //'S' : fpUltimoPesoLido := -10 ;  { Sobrecarga de Peso }
      else
        fpUltimoPesoLido := 0;
      end;

    except
      fpUltimoPesoLido := 0 ;
    end;

  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;

  GravaLog('              UltimoPesoLido: '+FloatToStr(fpUltimoPesoLido)+' , Resposta: '+Resposta );
end;

end.
