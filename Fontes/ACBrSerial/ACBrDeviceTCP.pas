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

unit ACBrDeviceTCP;

interface

uses
  Classes, SysUtils,
  blcksock,
  ACBrDeviceClass, ACBrBase;

type

  { TACBrDeviceTCP }

  TACBrDeviceTCP = class(TACBrDeviceClass)
  private
    fsSocket: TBlockSocket;
    fsIP: String;
    fsPorta: String;
  protected
    function GetMaxSendBandwidth: Integer; override;
    function GetTimeOutMilissegundos: Integer; override;
    procedure SetMaxSendBandwidth(AValue: Integer); override;
    procedure SetTimeOutMilissegundos(AValue: Integer); override;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Conectar(const APorta: String; const ATimeOutMilissegundos: Integer); override;
    procedure Desconectar(IgnorarErros: Boolean = True); override;

    function BytesParaLer: Integer; override;

    procedure EnviaString(const AString: AnsiString); override;
    procedure EnviaByte(const AByte: byte); override;
    function LeString(ATimeOutMilissegundos: Integer = 0; NumBytes: Integer = 0;
      const Terminador: AnsiString = ''): AnsiString; override;
    function LeByte(ATimeOutMilissegundos: Integer = 0): byte; override;
    procedure Limpar; override;

    property Socket: TBlockSocket read fsSocket;
    property IP: String read fsIP;
    property Porta: String read fsPorta;
  end;

implementation

uses
  ACBrDevice;

{ TACBrDeviceTCP }

constructor TACBrDeviceTCP.Create(AOwner: TComponent);
begin
  inherited;
  fsSocket := TTCPBlockSocket.Create;
  fsSocket.RaiseExcept := True;
end;

destructor TACBrDeviceTCP.Destroy;
begin
  fsSocket.Free;
  inherited Destroy;
end;

procedure TACBrDeviceTCP.Conectar(const APorta: String;
  const ATimeOutMilissegundos: Integer);
var
  ip: String;
  p: Integer;
begin
  inherited;
  ip := copy(APorta,5 , 255);  //     "TCP:ip_maquina:porta"
  p := pos(':', ip);
  if p = 0 then
    p := Length(ip)+1;

  fsPorta := copy(ip, p + 1, 5);
  if fsPorta = '' then
    fsPorta := '9100';

  fsIP := copy(ip, 1, p - 1);

  Desconectar(True);

  GravaLog('  IP:'+fsIP+', Porta:'+fsPorta+')');
  fsSocket.ConnectionTimeout := ATimeOutMilissegundos;
  fsSocket.Connect(fsIP, fsPorta);
  Limpar;
end;

procedure TACBrDeviceTCP.Desconectar(IgnorarErros: Boolean);
begin
  inherited;
  try
    fsSocket.RaiseExcept := not IgnorarErros;
    fsSocket.CloseSocket;
  finally
    fsSocket.RaiseExcept := True;
  end;
end;

function TACBrDeviceTCP.GetMaxSendBandwidth: Integer;
begin
  Result := fsSocket.MaxSendBandwidth;
end;

function TACBrDeviceTCP.GetTimeOutMilissegundos: Integer;
begin
  Result := fsSocket.ConnectionTimeout;
end;

procedure TACBrDeviceTCP.SetMaxSendBandwidth(AValue: Integer);
begin
  fsSocket.MaxSendBandwidth := AValue;
end;

procedure TACBrDeviceTCP.SetTimeOutMilissegundos(AValue: Integer);
begin
  fsSocket.ConnectionTimeout := AValue;
end;

function TACBrDeviceTCP.BytesParaLer: Integer;
begin
  Result := fsSocket.WaitingDataEx;
end;

procedure TACBrDeviceTCP.EnviaByte(const AByte: byte);
begin
  fsSocket.SendByte(AByte);
end;

procedure TACBrDeviceTCP.EnviaString(const AString: AnsiString);
Var
  I, Max, NBytes : Integer ;
begin
  GravaLog('  TACBrDeviceTCP.EnviaString');

  with TACBrDevice(fpOwner) do
  begin
    I := 1;
    Max := Length(AString);
    NBytes := SendBytesCount;
    if NBytes = 0 then
      NBytes := Max ;

    while I <= Max do
    begin
      GravaLog('  BytesToSend:'+IntToStr(NBytes));

      fsSocket.SendString( copy(AString, I, NBytes ) ) ;    { Envia por TCP }
      if SendBytesInterval > 0 then
      begin
        GravaLog('  Sleep('+IntToStr(SendBytesInterval)+')');
        Sleep( SendBytesInterval ) ;
      end;

      I := I + NBytes ;
    end;
  end;
end;

function TACBrDeviceTCP.LeString(ATimeOutMilissegundos: Integer;
  NumBytes: Integer; const Terminador: AnsiString): AnsiString;
begin
  if (NumBytes > 0) then
    Result := fsSocket.RecvBufferStr(NumBytes, ATimeOutMilissegundos)
  else if (Terminador <> '') then
    Result := fsSocket.RecvTerminated(ATimeOutMilissegundos, Terminador)
  else
    Result := fsSocket.RecvPacket(ATimeOutMilissegundos);
end;

function TACBrDeviceTCP.LeByte(ATimeOutMilissegundos: Integer): byte;
begin
  Result := fsSocket.RecvByte(ATimeOutMilissegundos);
end;

procedure TACBrDeviceTCP.Limpar;
begin
  fsSocket.Purge;
end;

end.

