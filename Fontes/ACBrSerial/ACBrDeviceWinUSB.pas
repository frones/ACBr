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

unit ACBrDeviceWinUSB;

interface

uses
  Classes, SysUtils,
  ACBrDeviceClass,
  ACBrWinUSBDevice;

type

  { TACBrDeviceWinUSB }

  TACBrDeviceWinUSB = class(TACBrDeviceClass)
  private
    fsWinUSB: TACBrUSBWinDeviceAPI;
  protected
    function GetTimeOutMilissegundos: Integer; override;
    procedure SetTimeOutMilissegundos(AValue: Integer); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Conectar(const APorta: String; const ATimeOutMilissegundos: Integer); override;
    procedure Desconectar(IgnorarErros: Boolean = True); override;
    procedure AcharPortasUSB(const AStringList: TStrings);

    procedure EnviaString(const AString: AnsiString); override;
    function LeString(ATimeOutMilissegundos: Integer = 0; NumBytes: Integer = 0;
      const Terminador: AnsiString = ''): AnsiString; override;

    procedure DetectarTipoEProtocoloDispositivoUSB(var TipoHardware: TACBrUSBHardwareType; var ProtocoloACBr: Integer);

    property WinUSB: TACBrUSBWinDeviceAPI read fsWinUSB;
  end;

implementation

uses
  strutils,
  ACBrDevice;

{ TACBrDeviceWinUSB }

constructor TACBrDeviceWinUSB.Create(AOwner: TComponent);
begin
  inherited;
  fsWinUSB := TACBrUSBWinDeviceAPI.Create;
  fsWinUSB.LogFile := TACBrDevice(fpOwner).ArqLOG;
end;

destructor TACBrDeviceWinUSB.Destroy;
begin
  fsWinUSB.Free;
  inherited Destroy;
end;

procedure TACBrDeviceWinUSB.AcharPortasUSB(const AStringList: TStrings);
var
  i: Integer;
begin
  GravaLog('AcharPortasUSB');
  fsWinUSB.FindUSBPrinters();
   for i := 0 to fsWinUSB.DeviceList.Count-1 do
     AStringList.Add('USB:'+fsWinUSB.DeviceList.Items[i].DeviceName);
end;

procedure TACBrDeviceWinUSB.Conectar(const APorta: String; const ATimeOutMilissegundos: Integer);
begin
  inherited;
  fsWinUSB.Connect(APorta);
  fsWinUSB.TimeOut := ATimeOutMilissegundos;
end;

procedure TACBrDeviceWinUSB.Desconectar(IgnorarErros: Boolean);
begin
  inherited;
  fsWinUSB.Close;
end;

function TACBrDeviceWinUSB.GetTimeOutMilissegundos: Integer;
begin
  Result := fsWinUSB.TimeOut;
end;

procedure TACBrDeviceWinUSB.SetTimeOutMilissegundos(AValue: Integer);
begin
  fsWinUSB.TimeOut := AValue;
end;

procedure TACBrDeviceWinUSB.EnviaString(const AString: AnsiString);
var
  I, Max, NBytes: Integer;
begin
  GravaLog('  TACBrDeviceWinUSB.EnviaString');

  with TACBrDevice(fpOwner) do
  begin
    I := 1;
    Max := Length(AString);
    NBytes := SendBytesCount;
    if NBytes = 0 then
      NBytes := Max;

    while I <= Max do
    begin
      GravaLog('  BytesToSend:' + IntToStr(NBytes));

      fsWinUSB.SendData(copy(AString, I, NBytes));
      if (SendBytesInterval > 0) then
      begin
        GravaLog('  Sleep(' + IntToStr(SendBytesInterval) + ')');
        Sleep(SendBytesInterval);
      end;

      I := I + NBytes;
    end;
  end;
end;

function TACBrDeviceWinUSB.LeString(ATimeOutMilissegundos: Integer; NumBytes: Integer;
  const Terminador: AnsiString): AnsiString;
begin
  if (NumBytes > 0) then
    Result := fsWinUSB.ReceiveNumBytes( NumBytes, ATimeOutMilissegundos)
  else if (Terminador <> '') then
    Result := fsWinUSB.ReceiveTerminated( Terminador, ATimeOutMilissegundos)
  else
    Result := fsWinUSB.ReceivePacket( ATimeOutMilissegundos );
end;

procedure TACBrDeviceWinUSB.DetectarTipoEProtocoloDispositivoUSB(var TipoHardware: TACBrUSBHardwareType; var ProtocoloACBr: Integer);
var
  i, f: Integer;
  uPorta, TipoPorta, DescPorta: String;
  DispositivoUSB: TACBrUSBWinDevice;
  Achou: Boolean;
begin
  if (WinUSB.DeviceList.Count < 1) then
    WinUSB.FindUSBPrinters;

  if (WinUSB.DeviceList.Count < 1) then
    Exit;

  DispositivoUSB := nil;
  uPorta := UpperCase(fpPorta);
  TipoPorta := copy(uPorta, 1, 3);
  DescPorta := copy(uPorta, 5, Length(uPorta));

  i := 0;
  Achou := False;
  while (not Achou) and (i < WinUSB.DeviceList.Count) do
  begin
    DispositivoUSB := WinUSB.DeviceList.Items[i];

    if (TipoPorta = 'COM') then
      Achou := (pos('(' + TipoPorta + ')', UpperCase(DispositivoUSB.FrendlyName)) > 0)
    else if (TipoPorta = '\\?') then
      Achou := (uPorta = UpperCase(copy(DispositivoUSB.DeviceInterface, 1, Length(uPorta))))
    else if (TipoPorta = 'USB') then
      Achou := (DescPorta = UpperCase(copy(DispositivoUSB.DeviceName, 1, Length(DescPorta))));

    Inc(i);
  end;

  if Achou then
  begin
    TipoHardware := DispositivoUSB.DeviceKind;
    ProtocoloACBr := DispositivoUSB.ACBrProtocol;

    with TACBrDevice(fpOwner) do
    begin
      if (DeviceType = dtUSB) then
      begin
        // Se dispositivo Usa COM Virtual, prefira ela...
        i := pos('(COM', DispositivoUSB.FrendlyName);
        if (i > 0) then
        begin
          f := PosEx(')', DispositivoUSB.FrendlyName, i + 1);
          if (f > 0) then
            Porta := copy(DispositivoUSB.FrendlyName, i + 1, f - i - 1);
        end;
      end;
    end;
  end;
end;

end.

