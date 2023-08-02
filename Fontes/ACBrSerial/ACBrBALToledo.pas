{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}
{$I ACBr.inc}

unit ACBrBALToledo;

interface

uses
  Classes,
  ACBrBALSelfCheckout
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type

  { TACBrBALToledo }

  TACBrBALToledo = class(TACBrBALSelfCheckout)
  private
    fpProtocolo: AnsiString;
    fpDecimais: Integer;

    function ProtocoloADetectado(const wPosIni: Integer; const aResposta: AnsiString): Boolean;
    function ProtocoloBDetectado(const wPosIni: Integer; const aResposta: AnsiString): Boolean;
    function ProtocoloEthDetectado(const wPosIni: Integer; const aResposta: AnsiString): Boolean;
    function ProtocoloP03Detectado(const wPosIni: Integer; const aResposta: AnsiString): Boolean;
    function InterpretarProtocoloA(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloB(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloC(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloEth(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloP03(const aResposta: AnsiString): AnsiString;
  public
    constructor Create(AOwner: TComponent);
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
    function EnviarPrecoKg(const aValor: Currency; aMillisecTimeOut: Integer = 3000): Boolean; override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP}
  DateUtils, StrUtils,
  {$ELSE}
  ACBrD5, Windows,
  {$ENDIF}
  SysUtils,
  ACBrConsts,
  ACBrUtil.Compatibilidade,
  ACBrUtil.Math,
  ACBrUtil.Strings,
  ACBrUtil.Base;

{ TACBrBALToledo }

function TACBrBALToledo.ProtocoloEthDetectado(const  wPosIni:Integer; const aResposta: AnsiString): Boolean;
begin
  Result := (Copy(aResposta, wPosIni + 1, 2) = '02') and (Copy(aResposta, wPosIni + 60, 1) = ETX);
end;

function TACBrBALToledo.ProtocoloADetectado(const  wPosIni:Integer; const aResposta: AnsiString): Boolean;
begin
  Result := (Copy(aResposta, wPosIni + 21, 1) = CR);
end;

function TACBrBALToledo.ProtocoloBDetectado(const  wPosIni:Integer; const aResposta: AnsiString): Boolean;
begin
  Result := (PosEx(ETX, aResposta, wPosIni + 1) > 0);
end;

function TACBrBALToledo.ProtocoloP03Detectado(const  wPosIni:Integer; const aResposta: AnsiString): Boolean;
var
  l_posini, l_posfim: Integer;
begin
  if (aResposta = EmptyStr) then
  begin
    Result := False;
    Exit;
  end;

  // detecta o padrão p03 na string.
  //                   1     2      3    4    567890   123456    7    8 (8 é opcional)
  // Protocolo P03 = [STX] [SWA] [SWB] [SWC] [IIIIII] [TTTTTT] [CR] [CS]
  if  (aresposta[1] = STX) and (aresposta[17] = CR) then
      // primeiro caracter da string é STX e o 17 é CR
    Result := True
  else
  begin
    // pode ocorrer da string ser lida quebrada, assim procura o primeiro CR, depois do primeiro STX
    // [IIII] [STX] [SWA] [SWB] [SWC] [IIIIII] [TTTTTT] [CR]
    l_posini := Pos(STX, aResposta);
    l_posfim := PosEX(CR, aResposta, l_posini + 1);
    if  l_posfim = 0 then
      l_posfim := Length(aResposta) + 1;

    Result := l_posfim - l_posini = 16;
  end;
end;

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
  vRetorno : String;
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

  vRetorno := Trim(Copy(aResposta, wPosIni + 1, wPosFim - wPosIni - 1));
  // A linha abaixo é para o modelo 9098... Veja: https://www.projetoacbr.com.br/forum/topic/58381-ajuste-leitura-peso-balan%C3%A7a-toleto-9098/
  vRetorno := StringReplace(vRetorno, 'kg', '', [rfReplaceAll]);
  Result := vRetorno;
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

function TACBrBALToledo.InterpretarProtocoloP03(const aResposta: AnsiString): AnsiString;
var
  l_strpso: string;
  l_swa: Char;
  l_posini, l_posfim: Integer;
begin
  fpProtocolo := 'Protocolo P03';
  // localiza o primeiro STX, e depois procura pelo CR para obter a string do peso
  l_posini := Pos(STX, aResposta);
  l_posfim := PosEX(CR, aResposta, l_posini + 1);
  if  l_posfim = 0 then
      l_posfim := Length(aResposta);

  // [TTTT][STX] [SWA] [SWB] [SWC] [IIIIII] [TTTTTT] [CR] [CS][STX] [SWA] [SWB] [SWC] [IIIIII] [TTTTTT] [CR]
  //         ^                                         ^
  // Separa a primeira string contendo o peso
  l_strpso := Copy(aResposta, l_posini, l_posfim - l_posini + 1);

  {Protocolo P03 = [STX] [SWA] [SWB] [SWC] [IIIIII] [TTTTTT] [CR] [CS]
   SWA = Status Word "A"
         bit 2, 1 e 0:
            001 - display / 0,1     10^-1
            010 - display / 1       10^0
            011 - display / 10      10^2
            100 - display / 100     10^3
            101 - display / 1000    10^4
            110 - display / 10000   10^5
         bits 4 e 3:
            01 - tamanho incrementado é 1
            10 - tamanho incrementado é 2
            11 - tamanho incrementado é 5
         bit 6 = sempre 01
         bit 5 = sempre 01
         bit 7 = paridade par
   SWB = Status Word "B"
   SWC = Status Word "C"
   III = Peso
   TTT = Tara
   CR  = Carriage Return
   CS  = Byte de Check-Sum (se C12 = L)
  }

  l_swa    := l_strpso[2];
  if  TestBit(Ord(l_swa), 3) then
      // Bit 3 de l_swa ligado = 2 casas decimais
      fpDecimais := 100;

  // obtem o peso da string lida
  Result := Copy(l_strpso, 5, 6);
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
    GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);

    fpUltimoPesoLido := InterpretarRepostaPeso(fpUltimaResposta);
  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;

  GravarLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) +
            ' - Resposta: ' + fpUltimaResposta + ' - Protocolo: ' + fpProtocolo);
end;

function TACBrBALToledo.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wPosIni: Integer;
  wResposta: AnsiString;
begin
  Result  := 0;

  if (aResposta = EmptyStr) then Exit;

  wPosIni := PosLast(STX, aResposta);

  if ProtocoloEthDetectado(wPosIni, aResposta) then
    wResposta := InterpretarProtocoloEth(aResposta)
  else if ProtocoloADetectado(wPosIni, aResposta) then
    wResposta := InterpretarProtocoloA(aResposta)
  else if ProtocoloBDetectado(wPosIni, aResposta) then
    wResposta := InterpretarProtocoloB(aResposta)
  else if ProtocoloP03Detectado(wPosIni, aResposta) then
    wResposta := InterpretarProtocoloP03(aResposta)
  else
    //protocolo P05
    wResposta := InterpretarProtocoloC(aResposta);

  { Convertendo novos formatos de retorno para balanças com pesagem maior que 30 kg}
  wResposta := CorrigirRespostaPeso(wResposta);

  if  (wResposta = EmptyStr) then Exit;

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
    case PadLeft(Trim(wResposta),1)[1] of
      'I': Result := -1;   { Instavel }
      'N': Result := -2;   { Peso Negativo }
      'S': Result := -10;  { Sobrecarga de Peso }
      'C': Result := -11;  { Indica peso em captura inicial de zero (dispositivo não está pronta para pesar) }
      'E': Result := -12;  { indica erro de calibração. }
    else
      Result := 0;
    end;
  end;
end;

function TACBrBALToledo.EnviarPrecoKg(const aValor: Currency;
  aMillisecTimeOut: Integer): Boolean;
var
  s, cmd: String;
begin
  s := PadLeft(FloatToIntStr(aValor), 6, '0');
  cmd := STX + s + ETX;

  GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' TX -> ' + cmd);

  fpDevice.Limpar;
  fpDevice.EnviaString(cmd);
  Sleep(200);

  Result := (fpDevice.LeString(aMillisecTimeOut) = ACK);
end;

end.
