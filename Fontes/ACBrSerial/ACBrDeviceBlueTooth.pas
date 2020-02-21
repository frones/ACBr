{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Regys Silveira                                }
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

unit ACBrDeviceBlueTooth;

interface

uses
  Classes, SysUtils,
  ACBrDeviceClass, ACBrBase,
  System.Bluetooth, System.Bluetooth.Components;

const
  GUID_BLUETOOTH_PRINTER : TGUID =  '{00001101-0000-1000-8000-00805F9B34FB}';

type

  { TACBrDeviceWinUSB }

  TACBrDeviceBlueTooth = class(TACBrDeviceClass)
  private
    fsBluetooth: TBluetooth;
    fsBlueToothSocket: TBluetoothSocket;
    fsInternalBuffer: AnsiString;

    procedure AtivarBlueTooth;
    function GetDeviceName: String;
    function PedirPermissoes: Boolean;

    function ReceiveNumBytes(BytesToRead, ATimeout: Integer): AnsiString;
    function ReceivePacket(ATimeOut: Integer): AnsiString;
    function ReceiveTerminated(ATerminator: AnsiString; ATimeOut: Integer): AnsiString;
  protected
    function GetTimeOutMilissegundos: Integer; override;
    procedure SetTimeOutMilissegundos(AValue: Integer); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Conectar(const APorta: String; const ATimeOutMilissegundos: Integer); override;
    procedure Desconectar(IgnorarErros: Boolean = True); override;
    procedure AcharPortasBlueTooth(const AStringList: TStrings);

    procedure EnviaString(const AString: AnsiString); override;
    function LeString(ATimeOutMilissegundos: Integer = 0; NumBytes: Integer = 0;
      const Terminador: AnsiString = ''): AnsiString; override;

    function GetBluetoothDevice(AName: String): TBluetoothDevice;

    property BlueTooth: TBluetooth read fsBlueTooth;
    property BlueToothSocket: TBluetoothSocket read fsBlueToothSocket;
  end;

ResourceString
  SErrDispositivoNaoEncontrado = 'Dispositivo [%s] não encontrado ou não pareado';
  SErrSemPermissaoParaBlueTooth = 'Sem Permissão para acesso a Dispositivos BlueTooth';
  SErrConectar = 'Erro ao conectar com Dispositivo [%s]';

implementation

uses
  dateutils,
  {$IfDef ANDROID}
   Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, System.Permissions,
  {$EndIf}
  ACBrDevice, ACBrUtil;

{ TACBrDeviceBlueTooth }

constructor TACBrDeviceBlueTooth.Create(AOwner: TComponent);
begin
  inherited;
  fsBluetooth := TBluetooth.Create(Nil);
  fsBlueToothSocket := Nil;
  fsInternalBuffer := '';
end;

destructor TACBrDeviceBlueTooth.Destroy;
begin
  Desconectar(True);
  fsBluetooth.Free;
  inherited;
end;

function TACBrDeviceBlueTooth.PedirPermissoes: Boolean;
Var
  Ok: Boolean;
begin
  Ok := True;
  {$IfDef ANDROID}
   PermissionsService.RequestPermissions( [JStringToString(TJManifest_permission.JavaClass.BLUETOOTH),
                                           JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_ADMIN),
                                           JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_PRIVILEGED),
                                           JStringToString(TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION),
                                           JStringToString(TJManifest_permission.JavaClass.BIND_PRINT_SERVICE),
                                           JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)],
      procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
      var
        GR: TPermissionStatus;
      begin
        for GR in AGrantResults do
          if (GR <> TPermissionStatus.Granted) then
          begin
            Ok := False;
            Break;
          end;
      end );
  {$EndIf}

  Result := Ok;
end;

procedure TACBrDeviceBlueTooth.AcharPortasBlueTooth(const AStringList: TStrings);
var
  DevBT: TBluetoothDevice;
begin
  // https://www.ampedrftech.com/guides/cod_definition.pdf
  // Bits 12 a 8 - 00110 Imaging (printing, scanner, camera, display, ...)
  // 1536 - 00110 0000 0000

  GravaLog('AcharPortasBlueTooth');
  AtivarBlueTooth;
  for DevBT in fsBluetooth.PairedDevices do
    //if TestBit(DevBT.ClassDevice, 10) then //  if (1536 and DevBT.ClassDevice) = 1536 then
      AStringList.Add('BTH:'+DevBT.DeviceName {+ ' - ' + IntToStr(DevBT.ClassDevice)});
end;

procedure TACBrDeviceBlueTooth.AtivarBlueTooth;
begin
  if not fsBluetooth.Enabled then
    fsBluetooth.Enabled := True;
end;

procedure TACBrDeviceBlueTooth.Conectar(const APorta: String;
  const ATimeOutMilissegundos: Integer);
var
  DevBT: TBluetoothDevice;
  DeviceName: String;
begin
  inherited;

  if not PedirPermissoes then
    DoException( Exception.Create(ACBrStr(SErrSemPermissaoParaBlueTooth)));

  AtivarBlueTooth;
  DeviceName := GetDeviceName;

  DevBT := GetBluetoothDevice(DeviceName);
  if not Assigned(DevBT) then
    DoException( Exception.CreateFmt(ACBrStr(SErrDispositivoNaoEncontrado), [APorta]));

  fsBlueToothSocket := DevBT.CreateClientSocket(GUID_BLUETOOTH_PRINTER, False);
  if Assigned(fsBlueToothSocket) then
    fsBlueToothSocket.Connect
  else
    DoException( Exception.CreateFmt(ACBrStr(SErrConectar), [DevBT.DeviceName]));

  Self.TimeOutMilissegundos := ATimeOutMilissegundos;
end;

procedure TACBrDeviceBlueTooth.Desconectar(IgnorarErros: Boolean);
begin
  if Assigned(fsBlueToothSocket) then
  begin
    fsBlueToothSocket.Close;
    FreeAndNil(fsBlueToothSocket);
  end;
end;

procedure TACBrDeviceBlueTooth.EnviaString(const AString: AnsiString);
begin
  GravaLog('  TACBrDeviceBlueTooth.EnviaString');
  if not Assigned(fsBlueToothSocket) then
    Exit;

  fsBlueToothSocket.SendData( TEncoding.ANSI.GetBytes(AString) );
end;

function TACBrDeviceBlueTooth.GetBluetoothDevice(AName: String): TBluetoothDevice;
var
  DevBT: TBluetoothDevice;
  UpperName: String;
begin
  // Busca Sem Case e Não Completaa
  Result := Nil;
  UpperName := UpperCase(AName);
  AtivarBlueTooth;
  for DevBT in fsBluetooth.PairedDevices do
  begin
    if UpperCase(copy(DevBT.DeviceName,1, Length(AName))) = UpperName then
    begin
      Result := DevBT;
      Break;
    end;
  end;
end;

function TACBrDeviceBlueTooth.GetDeviceName: String;
begin
  Result := fpPorta;
  if (copy(UpperCase(Result), 1, 4) = 'BTH:') then
    Result := copy(Result, 5, Length(Result)) ;

  Result := Trim(Result);
end;

function TACBrDeviceBlueTooth.GetTimeOutMilissegundos: Integer;
begin
  AtivarBlueTooth;
  if Assigned(fsBluetooth.CurrentManager) then
    Result := fsBluetooth.CurrentManager.SocketTimeout
  else
    Result := 100;
end;

procedure TACBrDeviceBlueTooth.SetTimeOutMilissegundos(AValue: Integer);
begin
  AtivarBlueTooth;
  if Assigned(fsBluetooth.CurrentManager) then
    fsBluetooth.CurrentManager.SocketTimeout := AValue;
end;

function TACBrDeviceBlueTooth.LeString(ATimeOutMilissegundos, NumBytes: Integer;
  const Terminador: AnsiString): AnsiString;
begin
  if ATimeOutMilissegundos = 0 then
    ATimeOutMilissegundos := TimeOutMilissegundos;

  if (NumBytes > 0) then
    Result := ReceiveNumBytes( NumBytes, ATimeOutMilissegundos)
  else if (Terminador <> '') then
    Result := ReceiveTerminated( Terminador, ATimeOutMilissegundos)
  else
    Result := ReceivePacket( ATimeOutMilissegundos );
end;


function TACBrDeviceBlueTooth.ReceiveNumBytes(BytesToRead: Integer;
  ATimeout: Integer): AnsiString;
var
  TimeoutTime: TDateTime;
begin
  Result := '';
  if BytesToRead <= 0 then Exit;

  //DEBUG
  //GravaLog( 'ReceiveNumBytes: BytesToRead: '+IntToStr(BytesToRead)+
  //          ', TimeOut: '+IntToStr(ATimeOut));

  TimeoutTime := IncMilliSecond(Now, ATimeOut);
  while (Length(fsInternalBuffer) < BytesToRead) and (now < TimeoutTime) do
    fsInternalBuffer := fsInternalBuffer + ReceivePacket(ATimeout);

  Result := copy(fsInternalBuffer, 1, BytesToRead);
  if (Result <> '') then
    Delete(fsInternalBuffer, 1, Length(Result));
end;

function TACBrDeviceBlueTooth.ReceiveTerminated(ATerminator: AnsiString;
  ATimeOut: Integer): AnsiString;
var
  TimeoutTime: TDateTime;
  LenTer, p: Integer;
begin
  Result := '';
  LenTer := Length(ATerminator);
  if (LenTer = 0) then
    Exit;

  //DEBUG
  //GravaLog('ReceiveTerminated: Terminator: '+ATerminator+
  //         ', TimeOut: '+IntToStr(ATimeOut), True);
  p := pos(ATerminator, fsInternalBuffer);
  TimeoutTime := IncMilliSecond(Now, ATimeOut);
  while (p < 1) and (now < TimeoutTime) do
  begin
    fsInternalBuffer := fsInternalBuffer + ReceivePacket(ATimeout);
    p := pos(ATerminator, fsInternalBuffer);
  end;

  if (p > 0) then
  begin
    Result := copy(fsInternalBuffer, 1, p-1);
    Delete(fsInternalBuffer, 1, p + LenTer-1);
  end;
end;

function TACBrDeviceBlueTooth.ReceivePacket(ATimeOut: Integer): AnsiString;
var
  AData: TBytes;
  DataStr: String;
  TimeoutTime: TDateTime;
begin
  //DEBUG
  //GravaLOg('  ReceivePacket: TimeOut: '+IntToStr(ATimeOut));

  Result := '';
  if not Assigned(fsBlueToothSocket) then
    Exit;

  TimeoutTime := IncMilliSecond(now, ATimeOut);
  repeat
    AData := fsBlueToothSocket.ReceiveData(ATimeOut);

    if (Length(AData) > 0) then
    begin
      DataStr := TEncoding.ANSI.GetString(AData);
      //DEBUG
      //GravaLog('  Buffer Lido: '+DataStr, True);
      Result := Result + AnsiString(DataStr);
    end;
  until (Length(AData) > 0) or (now > TimeoutTime);
end;

end.



