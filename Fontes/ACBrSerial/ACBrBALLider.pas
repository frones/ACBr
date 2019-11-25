{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Fabio Farias                                                }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/05/2014: Laércio S Amici | Emerson Virissimo da Silva
|*  - Primeira Versao ACBrBALLider
|*
|*      Protocolo Lider   -   Baseado no modelo LD2052
|*      SOH 00000. #32 <E|I> STX
|*
|*      E=Estável  I=Instável
|*
|*      A balança Lider manda leituras continuamente para a COM, não necessita
|*      de um comando para ler o peso.
|*      A idéia é buscar o último peso estável lido, então a busca deve iniciar
|*      no final do Buffer. Uma outra implementação possível seria tentar as 3
|*      últimas leituras estáveis pra ver se coincidem, mas no momento não vejo
|*      necessidade.
|*
|*      Configuração de leitura:
|*      Baud Rate = 2400, Data Bits = 8, Parity = None, Stop Bits = 1,
|*                        Handshaking = None
|*
|* 10/10/2016: Elias César Vieira
|*  - Refatoração de ACBrBALLider
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALLider;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALLider }

  TACBrBALLider = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    function LePeso(MillisecTimeOut: Integer = 3000): Double; override;

    procedure LeSerial(MillisecTimeOut: integer = 500); override;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;


implementation

// TESTE DE LEITURA UTILIZANDO LOG GERADO NA BALANÇA
// {$DEFINE DEBUG}

uses
  math, SysUtils,
  ACBrConsts,
  {$IFDEF COMPILER6_UP}
   DateUtils, StrUtils
  {$ELSE}
   ACBrD5, Windows
  {$ENDIF};


{ Retorna a posição da Substr em Str a partir do final }
function RightPos( const ASubstr, AStr: string ): integer;
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
  inherited Create(AOwner);

  fpModeloStr := 'Lider';
end;


function TACBrBALLider.LePeso(MillisecTimeOut: Integer): Double;
begin
  { Balança Lider não é necessário enviar comando de leitura... Portanto... }
  { Utilizará a função AguardarRespostaPeso com ReenviarSolicitação = False }
  Result := AguardarRespostaPeso(MillisecTimeOut);
end;

procedure TACBrBALLider.LeSerial(MillisecTimeOut: Integer);
{$IFDEF DEBUG}
var
  StrDebug: TStringList;
{$ENDIF}

begin
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';

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
    fpUltimaResposta := fpDevice.LeString(MillisecTimeOut);
    GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);
    {$ENDIF}

    fpUltimoPesoLido := InterpretarRepostaPeso(fpUltimaResposta);
  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;

  GravaLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) +
           ' , Resposta: ' + fpUltimaResposta);
end;

function TACBrBALLider.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  StatusPeso: AnsiChar;
  wPos: Integer;
begin
  Result := 0;

  if (aResposta = EmptyStr) then
    Exit;

  wResposta := aResposta;

  { Buscar último peso estável no buffer }
  wPos := RightPos('E' + STX, wResposta);

  { Se não achou leitura estáve. buscar último peso instável no buffer }
  if (wPos = 0) then
    wPos := RightPos('I' + STX, wResposta);

  { Se achou, isolar a leitura }
  if (wPos > 0) then
  begin
    wResposta := Copy(wResposta, 1, wPos + 1);

    wPos := RightPos(SOH, wResposta);

    if (wPos > 0) then
      wResposta := Copy(wResposta, wPos, Length(wResposta));
  end;

  { Retira SOH, STX }
  if (Copy(wResposta, 1, 1) = SOH) then
    wResposta := Copy(wResposta, 2, Length(wResposta));

  if (Copy(wResposta, Length(wResposta), 1) = STX) then
    wResposta := Copy(wResposta, 1, Length(wResposta) - 1);

  { Substituir o espaço que vem após o ponto }
  wResposta := StringReplace(wResposta, ' ', '0', [rfReplaceAll]);

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    StatusPeso := ' ';
    if (Length(wResposta) >= 1) then
    begin
      StatusPeso := Copy(wResposta, Length(wResposta), 1)[1];

      if (StatusPeso in ['E', 'I']) then
        wResposta := Copy(wResposta, 1, Length(wResposta) - 1);
    end;

    case StatusPeso of
      'I': Result := -1;                     { Instavel }
      'E': Result := StrToFloat(wResposta);  { Estável }
    else
      Result := 0;
    end;
  except
    Result := 0;
  end;
end;

end.
