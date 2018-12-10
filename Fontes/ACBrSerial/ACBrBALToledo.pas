{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
|* 04/10/2005: Daniel Simões de Almeida
|*  - Primeira Versao ACBrBALToledo
|* 11/04/2007 Daniel Simões de Almeida
|*  - Corrigido para trabalhar com diversos protocolos da Toledo
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALToledo;

interface

uses
  ACBrBALClass, Classes;

type

  { TACBrBALToledo }

  TACBrBALToledo = class(TACBrBALClass)
  private
    fpProtocolo: AnsiString;
    fpDecimais: Integer;

    function InterpretarProtocoloA(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloB(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloC(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloEth(const aResposta: AnsiString): AnsiString;
  public
    constructor Create(AOwner: TComponent);

    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override;

    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
  end;

implementation

uses
  ACBrUtil, ACBrConsts, SysUtils,
  {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, synaser, Windows{$ENDIF};

{ TACBrBALToledo }

function TACBrBALToledo.InterpretarProtocoloA(const aResposta: AnsiString): AnsiString;
var
  wStatus2: AnsiChar;
  wPosIni: Integer;
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

  fpProtocolo := 'Protocolo A';
  wStatus2    := aResposta[9];
  wPosIni     := PosLast(STX, aResposta);

  if TestBit(Ord(wStatus2), 3) then  { Bit 3 de wStatus2 ligado = 2 casas decimais }
    fpDecimais := 100;

  Result := Trim(Copy(aResposta, wPosIni + 2, 6));
end;

function TACBrBALToledo.InterpretarProtocoloB(const aResposta: AnsiString): AnsiString;
var
  wPosIni, wPosFim: Integer;
begin
  { Protocolo B = [ ENQ ] [ STX ] [ PESO ] [ ETX ]
    Linha Automacao:
      [ STX ] [ PPPPP ] [ ETX ]  - Peso Estável;
      [ STX ] [ IIIII ] [ ETX ]  - Peso Instável;
      [ STX ] [ NNNNN ] [ ETX ]  - Peso Negativo;
      [ STX ] [ SSSSS ] [ ETX ]  - Peso Acima (Sobrecarga) }

  wPosIni     := PosLast(STX, aResposta);
  wPosFim     := PosEx(ETX, aResposta, wPosIni + 1);

  if (wPosFim > 0) then
    fpProtocolo := 'Protocolo B'
  else
    wPosFim := Length(aResposta) + 1;  // Não achou? ...Usa a String inteira

  Result := Trim(Copy(aResposta, wPosIni + 1, wPosFim - wPosIni - 1));
end;

function TACBrBALToledo.InterpretarProtocoloC(const aResposta: AnsiString): AnsiString;
var
  wPosIni, wPosFim: Integer;
begin
  { Protocolo C = [ STX ] [ PESO ] [ CR ]
    Linha Automacao:
      [ STX ] [ PPPPP ] [ ETX ]  - Peso Estável;
      [ STX ] [ IIIII ] [ ETX ]  - Peso Instável;
      [ STX ] [ NNNNN ] [ ETX ]  - Peso Negativo;
      [ STX ] [ SSSSS ] [ ETX ]  - Peso Acima (Sobrecarga) }

  wPosIni     := PosLast(STX, aResposta);
  wPosFim     := PosEx(CR, aResposta, wPosIni + 1);

  if (wPosFim > 0) then
    fpProtocolo := 'Protocolo C'
  else
    wPosFim := Length(aResposta) + 1;  // Não achou? ...Usa a String inteira

  Result := Trim(Copy(aResposta, wPosIni + 1, wPosFim - wPosIni - 1));
end;

function TACBrBALToledo.InterpretarProtocoloEth(
  const aResposta: AnsiString): AnsiString;
var
  wPosIni, wPosFim: Integer;
begin
  { Protocolo Ethernet sem Criptografia
    [ STX ] [ OPCODE ] [ DADOS ] [ DLE] [ ETX ] [ CS ]

    OPCODE = 2 bytes - ASCII "02"

    DADOS : SWA = 1 byte
            SWB = 1 byte
            SWC = 1 byte
            PESO = 6 bytes
            TARA = 6 bytes ____ Até aqui segue um padrão o demais dados abaixo varia conforme o modelo
            PEÇAS = 6 bytes
            PMP = 6 bytes
            CÓDIGO = 11 bytes
            Reservado = 1 byte
            Habilita Escrita = 1 byte
                               0 = Não permite
                               1 = Permite somente pela página Web
                               2 = Permite somente pelo Easylink
                               3 = Permite pela página Web e pelo Easylink
            Capacidade = 1 byte
            Flag da Cap. de Zero = 1 byte
                                   "P" - Acima de zero
                                   "N" - Abaixo de zero
            Captura de zero = 6 bytes
            Consecutivo = 6 bytes
            Classificaçãodo Peso = 1 byte
            Memória Utilizada = 1 byte
            
    DLE = 1 byte - 10
    ETX = 1 byte - 03
    CS = 1 byte - Checksum. O cálculo do checksum é feito pelo complemento
       de 2 da soma de todos os bytes transmitidos de OPCODE e DADOS.
  }

  wPosIni := PosLast(STX, aResposta);
  wPosFim := PosEx(ETX, aResposta, wPosIni + 1);

  if (wPosFim > 0) then
    fpProtocolo := 'Protocolo Eth'
  else
    wPosFim := Length(aResposta) + 1;  // Não achou? ...Usa a String inteira

  // Contem a String inteira
  fpUltimaResposta := Copy(aResposta, wPosIni, wPosFim - wPosIni);

  // Contem somente o Peso
  Result := Trim(Copy(aResposta, wPosIni + 6, 6));
end;

constructor TACBrBALToledo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Toledo';
  fpProtocolo := 'Não Definido';
  fpDecimais  := 1000;
end;

procedure TACBrBALToledo.LeSerial(MillisecTimeOut: Integer);
begin
  // Reescreve LeSerial para incluir Protocolo no Log
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';

  try
    fpUltimaResposta := fpDevice.LeString(MillisecTimeOut);
    GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);

    fpUltimoPesoLido := InterpretarRepostaPeso(fpUltimaResposta);
  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;

  GravaLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) +
           ' - Resposta: ' + fpUltimaResposta + ' - Protocolo: ' + fpProtocolo);
end;

function TACBrBALToledo.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wPosIni: Integer;
  wResposta: AnsiString;
begin
  Result  := 0;
  wPosIni := PosLast(STX, aResposta);

  if (Copy(aResposta, wPosIni + 1, 2) = '02') and
     (Copy(aResposta, wPosIni + 60, 1) = ETX) then
    wResposta := InterpretarProtocoloEth(aResposta)
  else if (Copy(aResposta, wPosIni + 21, 1) = CR) then
    wResposta := InterpretarProtocoloA(aResposta)
  else if (PosEx(ETX, aResposta, wPosIni + 1) > 0) then
    wResposta := InterpretarProtocoloB(aResposta)
  else
    wResposta := InterpretarProtocoloC(aResposta);

  if (aResposta = EmptyStr) then
    Exit;

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    { Já existe ponto decimal ? }
    if (Pos(DecimalSeparator, wResposta) > 0) then
      Result := StrToFloat(wResposta)
    else
      Result := (StrToInt(wResposta) / fpDecimais);
  except
    case wResposta[1] of
      'I': Result := -1;   { Instavel }
      'N': Result := -2;   { Peso Negativo }
      'S': Result := -10;  { Sobrecarga de Peso }
    else
      Result := 0;
    end;
  end;
end;

end.
