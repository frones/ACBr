{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

{******************************************************************************
|* 13/12/2019: Primeira Versao: Daniel Simoes de Almeida
|* Fontes:
|* https://web.archive.org/web/20130126085547/http://peter.skarpetis.com/wiki/index.php/Usbprint.sys
******************************************************************************}

{$I ACBr.inc}

unit ACBrWinUSBDevice;

interface

uses
  Classes, SysUtils, Windows, IniFiles, contnrs
  {$IfDef FPC}
   ,dynlibs
  {$EndIf}
  ,ACBrUtil.Base, ACBrBase;

const
  CUSBIDDataBaseResourceName = 'ACBrUSBID';
  CUSBIDDataBaseFileName = 'ACBrUSBID.ini';
  CSecVendors = 'Vendors';
  CReceiveBufferSize = 1024;

resourcestring
  sErrACBrWinUSBInvalidID = '%S inválido [%s]';
  sErrACBrWinUSBDeviceOutOfRange = 'Dispositivo USB num: %d não existe';
  sErrACBrWinUSBOpening = 'Erro %s ao abrir o Porta USB %s';
  sErrACBrWinUSBClosing = 'Erro %s ao fechar a Porta USB %s';
  sErrACBrWinUSBDescriptionNotFound = 'Erro, dispositivo [%s] não encontrado';
  sErrACBrWinUSBNotDeviceFound = 'Nenhum dispositivo USB encontrado';
  sErrACBrWinUSBDeviceIsClosed = 'Dispositivo USB não está aberto';
  sErrACBrWinUSBSendData = 'Erro %s, ao enviar %d bytes para USB';
  sErrACBrWinUSBReadData = 'Erro %s, ao ler da USB';

  sDescDevPosPrinter = 'Impressora';
  sDescDevLabelPrinter = 'Etiquetadora';
  sDescDevFiscal = 'SAT';

type

  TACBrUSBHardwareType = (htUnknown, htPOSPrinter, htLabelPrinter, htFiscal);

  { TACBrUSBIDDataBase - https://www.usb.org/developers }

  TACBrUSBIDDataBase = class
  private
    FIni: TMemIniFile;
    FFileName: String;
    FResourceName: String;
    FLoaded: Boolean;
    function LoadFromFile: Boolean;
    function LoadFromResource: Boolean;
    procedure SetFileName(AValue: String);
  public
    constructor Create(ADataBaseFileName: String = ''; AResourceName: String = '');
    destructor Destroy; override;
    procedure Clear;
    function Load: Boolean;

    property FileName: String read FFileName write SetFileName;
    property ResourceName: String read FResourceName;
    function FindDeviceByID(AVendorID, AProductID: String; out AVendorName: String;
      out AProductModel: String; out AACBrProtocol: Integer;
      out ADeviceKind: TACBrUSBHardwareType): Boolean;
  end;

  TACBrUSBWinDeviceList = class;

  { TACBrUSBWinDevice }

  TACBrUSBWinDevice = class
  private
    FACBrProtocol: Integer;
    FClassGUID: String;
    FDeviceKind: TACBrUSBHardwareType;
    FFrendlyName: String;
    FGUID: String;
    FHardwareID: String;
    FOwner: TACBrUSBWinDeviceList;
    FDeviceInterface: String;
    FProductID, FProductModel: String;
    FUSBPort: String;
    FVendorID, FVendorName: String;
    FDescriptionsLoaded: Boolean;

    function GetDeviceKind: TACBrUSBHardwareType;
    function GetDeviceName: String;
    function GetDeviceACBrName: String;
    function GetProductModel: String;
    function GetVendorName: String;
    procedure SetClassGUID(AValue: String);
    procedure SetGUID(AValue: String);
    procedure SetProductID(AValue: String);
    procedure SetVendorID(AValue: String);
    procedure LoadDescriptions;
  public
    constructor Create(AOwner: TACBrUSBWinDeviceList);
    procedure Clear;

    property DeviceKind: TACBrUSBHardwareType read GetDeviceKind;
    property DeviceACBrName: String read GetDeviceACBrName;
    property DeviceName: String read GetDeviceName;
    property VendorID: String read FVendorID write SetVendorID;
    property VendorName: String read GetVendorName;
    property ProductID: String read FProductID write SetProductID;
    property ProductModel: String read GetProductModel;
    property DeviceInterface: String read FDeviceInterface write FDeviceInterface;
    property USBPort: String read FUSBPort write FUSBPort;
    property ClassGUID: String read FClassGUID write SetClassGUID;
    property GUID: String read FGUID write SetGUID;
    property FrendlyName: String read FFrendlyName write FFrendlyName;
    property HardwareID: String read FHardwareID write FHardwareID;
    property ACBrProtocol: Integer read FACBrProtocol write FACBrProtocol;
  end;

  { TACBrUSBWinDeviceList }

  TACBrUSBWinDeviceList = class(TObjectList)
  private
    FDataBase: TACBrUSBIDDataBase;

    function GetItem(Index: Integer): TACBrUSBWinDevice;
  public
    constructor Create(const ADataBaseFileName: String = '');
    destructor Destroy; override;

    function New(const AVendorID, AProductID: String): TACBrUSBWinDevice;
    property Items[Index: Integer]: TACBrUSBWinDevice read GetItem;
    property Database: TACBrUSBIDDataBase read FDataBase;

    function FindDeviceByGUID(const AGUID: String): Integer;
    function FindDeviceByDescription(const ADeviceName: String): Integer;
  end;


{ SetupAPI Bind }
const
  CSetupAPILibName = 'SetupApi.dll';

  GUID_DEVCLASS_NET : TGUID =  '{4D36E972-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PORT : TGUID = '{4D36E978-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PRINTER : TGUID = '{4D36E979-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PNPPRINTERS: TGUID = '{4658EE7E-F050-11D1-B6BD-00C04FA372A7}';
  GUID_DEVCLASS_NETCLIENT: TGUID = '{4D36E973-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVINTERFACE_USBPRINT: TGUID = '{28D78FAD-5A12-11D1-AE5B-0000F803A8C2}';
  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  GUID_EPSON_VENDORPRINTER_CLASS: TGUID = '{5dd901c0-bcf4-11d1-9738-008029e6a5b3}';


  DIGCF_PRESENT = $00000002;
  DIGCF_DEVICEINTERFACE = $00000010;

  SPDRP_DEVICEDESC           = $00000000; // DeviceDesc (R/W)
  SPDRP_HARDWAREID           = $00000001; // HardwareID (R/W)
  SPDRP_CLASS                = $00000007; // Class (R--tied to ClassGUID)
  SPDRP_CLASSGUID            = $00000008; // ClassGUID (R/W)
  SPDRP_DRIVER               = $00000009; // Driver (R/W)
  SPDRP_FRIENDLYNAME         = $0000000C; // FriendlyName (R/W)
  SPDRP_LOCATION_INFORMATION = $0000000D; // LocationInformation (R/W)

type
  HDEVINFO = Pointer;

  PSPDevInfoData = ^TSPDevInfoData;

  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD;
    Reserved: ULONG_PTR;
  end;
  TSPDevInfoData = SP_DEVINFO_DATA;

  PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;
  SP_DEVICE_INTERFACE_DATA = packed record
    cbSize: DWORD;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: ULONG_PTR;
  end;
  TSPDeviceInterfaceData = SP_DEVICE_INTERFACE_DATA;

  PSPDeviceInterfaceDetailData = ^TSPDeviceInterfaceDetailData;
  {$IfDef UNICODE}
   SP_DEVICE_INTERFACE_DETAIL_DATA_W = packed record
     cbSize: DWORD;
     DevicePath: array [0..ANYSIZE_ARRAY - 1] of WideChar;
   end;
   TSPDeviceInterfaceDetailData = SP_DEVICE_INTERFACE_DETAIL_DATA_W;
  {$Else}
   SP_DEVICE_INTERFACE_DETAIL_DATA_A = packed record
     cbSize: DWORD;
     DevicePath: array [0..ANYSIZE_ARRAY - 1] of AnsiChar;
   end;
   TSPDeviceInterfaceDetailData = SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  {$EndIf}

  { TACBrUSBWinDeviceAPI }

  TACBrUSBWinDeviceAPI = class
  private
    FLoaded: Boolean;
    FDeviceList: TACBrUSBWinDeviceList;
    FUSBHandle: THandle;
    FInterfaceName: String;
    FDeviceIndex: Integer;
    FTimeOut: Integer;
    FInternalBuffer: AnsiString;
    FLogFile: String;
    FOnLog: TACBrGravarLog;
    FHardwareType: TACBrUSBHardwareType;

    xSetupDiEnumDeviceInfo: function(DeviceInfoSet: HDEVINFO; MemberIndex: DWORD;
      var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;

    xSetupDiGetClassDevsA : function(ClassGuid: PGUID; const AEnumerator: PChar;
      hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;

    xSetupDiGetDeviceRegistryProperty : function(DeviceInfoSet: HDEVINFO;
      const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
      var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
      var RequiredSize: DWORD): BOOL; stdcall;

    xSetupDiEnumDeviceInterfaces: function(DeviceInfoSet: HDEVINFO;
      DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID;
      MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;

    xSetupDiGetDeviceInterfaceDetail: function(DeviceInfoSet: HDEVINFO;
      DeviceInterfaceData: PSPDeviceInterfaceData;
      DeviceInterfaceDetailData: PSPDeviceInterfaceDetailData;
      DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
      Device: PSPDevInfoData): BOOL; stdcall;

    xSetupDiDestroyDeviceInfoList: function(DeviceInfoSet: HDEVINFO): BOOL; stdcall;

  private
    procedure ExtractVidAndPid(const ADeviceInterface: String; out AVid: String; out APid: String);
    function GetActive: Boolean;
    function GetDataBase: TACBrUSBIDDataBase;

    function GetDeviceRegistryPropertyString(DevInfo: HDEVINFO;
      DeviceInfoData: TSPDevInfoData; Prop: DWORD): AnsiString;
    procedure SetDeviceIndex(AValue: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadAPI;
    procedure UnLoadAPI;

    property Loaded: Boolean read FLoaded;
    property DeviceList: TACBrUSBWinDeviceList read FDeviceList;
    property DeviceIndex: Integer read FDeviceIndex write SetDeviceIndex;
    property DataBase: TACBrUSBIDDataBase read GetDataBase;
    property HardwareType: TACBrUSBHardwareType read FHardwareType write FHardwareType;

    property USBHandle: THandle read FUSBHandle;
    property InterfaceName: String read FInterfaceName;
    property Active: Boolean read GetActive;
    property TimeOut: Integer read FTimeOut write FTimeOut default 1000;

    function FindUSBPrinters(ADeviceList: TACBrUSBWinDeviceList = Nil): Integer;
    function FindUSBKnownDevices(ADeviceList: TACBrUSBWinDeviceList = Nil): Integer;
    function FindUSBDevicesByGUID(AGUID: TGUID; ADeviceList: TACBrUSBWinDeviceList = Nil;
      DeviceSufix: String = ''): Integer;

    procedure Connect(AInterfaceName: String);
    procedure Close;
    procedure Purge;
    function SendData(const AData: AnsiString; ATimeout: Integer = 0): Integer;
    function ReceiveNumBytes(BytesToRead: Integer; ATimeout: Integer = 0): AnsiString;
    function ReceiveTerminated(const ATerminator: AnsiString; ATimeOut: Integer = 0): AnsiString;
    function ReceivePacket(ATimeOut: Integer = 0): AnsiString;

    property OnLog: TACBrGravarLog read FOnLog write FOnLog;
    property LogFile: String read FLogFile write FLogFile;
    procedure DoLog(const AMessage: AnsiString; Translate: Boolean = False;
      AddTimeStamp: Boolean = True);
  end;

  function DeviceKindDescription(ADeviceKind: TACBrUSBHardwareType): String;
  function FindCOMPortInDeviceName(ADeviceName: String): String;
  function DeviceNameWithoutCOMPort(ADeviceName: String): String;

implementation

{$IFDEF FPC}
 {$R ACBrUSBID.rc}
{$ELSE}
 {$R ACBrUSBID.res}
{$ENDIF}

uses
  Types, dateutils, strutils, math, 
  ACBrConsts,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

function DeviceKindDescription(ADeviceKind: TACBrUSBHardwareType): String;
begin
  case ADeviceKind of
    htPOSPrinter: Result := sDescDevPosPrinter;
    htLabelPrinter: Result := sDescDevLabelPrinter;
    htFiscal: Result := sDescDevFiscal;
  else
    Result := '';
  end;
end;

function FindCOMPortInDeviceName(ADeviceName: String): String;
var
  i, f: Integer;
begin
  Result := '';
  i := pos('(COM', ADeviceName);
  if (i > 0) then
  begin
    f := PosEx(')', ADeviceName, i + 1);
    if (f > 0) then
      Result := copy(ADeviceName, i + 1, f - i - 1);
  end;
end;

function DeviceNameWithoutCOMPort(ADeviceName: String): String;
var
  VCOMPort: String;
begin
  VCOMPort := FindCOMPortInDeviceName(ADeviceName);
  if (VCOMPort <> '') then
    Result := StringReplace(ADeviceName, VCOMPort, 'VCOM', [rfReplaceAll])
  else
    Result := ADeviceName;
end;


{ TACBrUSBIDDataBase }

constructor TACBrUSBIDDataBase.Create(ADataBaseFileName: String; AResourceName: String);
begin
  inherited Create;
  FIni := TMemIniFile.Create('');

  if ADataBaseFileName = '' then
    FFileName := ApplicationPath + CUSBIDDataBaseFileName
  else
    FFileName := ADataBaseFileName;

  if AResourceName = '' then
    FResourceName := CUSBIDDataBaseResourceName
  else
    FResourceName := AResourceName;

  Clear;
end;

destructor TACBrUSBIDDataBase.Destroy;
begin
  FIni.Free;
  inherited Destroy;
end;

procedure TACBrUSBIDDataBase.Clear;
begin
  FLoaded := False;
  FIni.Clear;
end;

function TACBrUSBIDDataBase.Load: Boolean;
begin
  Clear;
  Result := LoadFromFile or LoadFromResource;
  FLoaded := Result;
end;

procedure TACBrUSBIDDataBase.SetFileName(AValue: String);
begin
  if (FFileName = AValue) then
    Exit;

  if (AValue <> '') and (not FileExists(AValue)) then
    raise Exception.CreateFmt(ACBrStr(cACBrArquivoNaoEncontrado), [AValue]);

  FFileName := AValue;
  Load;
end;

function TACBrUSBIDDataBase.LoadFromFile: Boolean;
var
  SL: TStringList;
begin
  Result := False;
  if (FFileName <> '') and FileExists(FFileName) then
  begin
    FIni.Free;
    FIni := TMemIniFile.Create(FFileName);

    SL := TStringList.Create;
    try
      SL.LoadFromFile(FFileName);
      FIni.SetStrings(SL);
      Result := True;
    finally
      SL.Free;
    end;
  end;
end;

function TACBrUSBIDDataBase.LoadFromResource: Boolean;
var
  RS: TResourceStream;
  SL: TStringList;
begin
  {$IfDef FPC}Result := False;{$EndIf}
  RS := TResourceStream.Create(HInstance, FResourceName, Windows.RT_RCDATA);
  SL := TStringList.Create;
  try
    // Leitura do Resource pode falhar
    RS.Position := 0;
    SL.LoadFromStream(RS);
    FIni.SetStrings(SL);
    Result := True;
  finally
    RS.Free;
    SL.Free;
  end;
end;

function TACBrUSBIDDataBase.FindDeviceByID(AVendorID, AProductID: String; out
  AVendorName: String; out AProductModel: String; out AACBrProtocol: Integer;
  out ADeviceKind: TACBrUSBHardwareType): Boolean;
var
  SL: TStringList;
begin
  {$IfDef FPC}Result := False;{$EndIf}
  AVendorName := '';
  AProductModel := '';
  AACBrProtocol := 0;
  ADeviceKind := htUnknown;

  if not StrIsHexa(AVendorID) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['VendorID', AVendorID]);

  if not StrIsHexa(AProductID) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['ProductID', AProductID]);

  if not FLoaded then
    Load;

  AVendorName := FIni.ReadString(CSecVendors, AVendorID, '');
  AProductModel := FIni.ReadString(AVendorID, AProductID, '');

  SL := TStringList.Create;
  try
    AddDelimitedTextToList(AProductModel, ';', SL, #0);

    if SL.Count > 0 then
      AProductModel := SL[0];

    if SL.Count > 1 then
      ADeviceKind := TACBrUSBHardwareType(StrToIntDef(SL[1],0));

    if SL.Count > 2 then
      AACBrProtocol := StrToIntDef(SL[2],0);
  finally
    SL.Free;
  end;

  Result := (AVendorName <> '') or (AProductModel <> '');
end;


{ TACBrUSBWinDevice }

constructor TACBrUSBWinDevice.Create(AOwner: TACBrUSBWinDeviceList);
begin
  inherited Create;

  FOwner := AOwner;
  Clear;
end;

procedure TACBrUSBWinDevice.Clear;
begin
  FVendorID := '';
  FProductID := '';
  FVendorName := '';
  FProductModel := '';
  FDeviceInterface := '';
  FUSBPort := '';
  FGUID := '';
  FFrendlyName := '';
  FHardwareID := '';
  FACBrProtocol := 0;
  FDeviceKind := htUnknown;
  FDescriptionsLoaded := False;
end;

function TACBrUSBWinDevice.GetDeviceName: String;
var
  p: Integer;
  vid, pid: String;
begin
  Result := DeviceACBrName;
  if (FrendlyName <> '') then
  begin
    p := pos(',', Result);
    if (p > 0) then
    begin
      vid := Trim(copy(Result, 1, p-1));
      pid := Trim(copy(Result, p+1, Length(Result)));

      if (p = 5) and StrIsHexa(vid) and StrIsHexa(pid) then // retornará hexa: PID, VID ?
        Result := FrendlyName;

      if (vid = copy(FrendlyName, 1, Length(vid))) then  // Tem o mesmo fabricante ?
       Result := FrendlyName;
    end;
  end;
end;

function TACBrUSBWinDevice.GetDeviceACBrName: String;

  function ConcatDescription(ActualDescription, AddDescription: String): String;
  begin
    Result := ActualDescription;
    if (Trim(AddDescription) = '') then
      Exit;

    if (Result <> '') then
      Result := Result + ', ';

    Result := Result + AddDescription;
  end;

begin
  Result := '';
  LoadDescriptions;
  if (pos(',',ProductModel) = 0) then
    Result := ConcatDescription(Result, VendorName);

  Result := ConcatDescription(Result, ProductModel);

  if (Result = '') then
    Result := VendorID + ', '+ ProductID;

  //Result := ConcatDescription(Result, DeviceKindDescription(DeviceKind));
end;


function TACBrUSBWinDevice.GetDeviceKind: TACBrUSBHardwareType;
begin
  LoadDescriptions;
  Result := FDeviceKind;
end;

function TACBrUSBWinDevice.GetProductModel: String;
begin
  LoadDescriptions;
  Result := FProductModel;
end;

function TACBrUSBWinDevice.GetVendorName: String;
begin
  LoadDescriptions;
  Result := FVendorName;
end;

procedure TACBrUSBWinDevice.SetClassGUID(AValue: String);
begin
  if FClassGUID = AValue then Exit;
  FClassGUID := LowerCase(AValue);
end;

procedure TACBrUSBWinDevice.SetGUID(AValue: String);
begin
  if FGUID = AValue then Exit;
  FGUID := LowerCase(AValue);
end;

procedure TACBrUSBWinDevice.SetProductID(AValue: String);
begin
  if FProductID = AValue then
    Exit;
  if not StrIsHexa(AValue) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['ProductID', AValue]);

  FProductID := LowerCase(AValue);
end;

procedure TACBrUSBWinDevice.SetVendorID(AValue: String);
begin
  if FVendorID = AValue then
    Exit;
  if not StrIsHexa(AValue) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['VendorID', AValue]);

  FVendorID := LowerCase(AValue);
end;

procedure TACBrUSBWinDevice.LoadDescriptions;
begin
  if FDescriptionsLoaded then Exit;

  FVendorName := '';
  FProductModel := '';
  FDeviceKind := htUnknown;
  FOwner.Database.FindDeviceByID( FVendorID, FProductID,
                                  FVendorName, FProductModel, FACBrProtocol, FDeviceKind);
  FDescriptionsLoaded := True;
end;


{ TACBrUSBWinDeviceList }

function TACBrUSBWinDeviceList.GetItem(Index: Integer): TACBrUSBWinDevice;
begin
  Result := TACBrUSBWinDevice(inherited Items[Index]);
end;

constructor TACBrUSBWinDeviceList.Create(const ADataBaseFileName: String);
begin
  inherited Create(True);
  FDataBase := TACBrUSBIDDataBase.Create(ADataBaseFileName);
end;

destructor TACBrUSBWinDeviceList.Destroy;
begin
  FDataBase.Free;
  inherited Destroy;
end;

function TACBrUSBWinDeviceList.New(const AVendorID, AProductID: String
  ): TACBrUSBWinDevice;
begin
  Result := TACBrUSBWinDevice.Create(Self);
  try
    Result.VendorID := AVendorID;
    Result.ProductID := AProductID;
    inherited Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TACBrUSBWinDeviceList.FindDeviceByGUID(const AGUID: String): Integer;
var
  i: Integer;
  LGUID: String;
begin
  Result := -1;
  if (Count < 1) or (Trim(AGUID) = '') then Exit;

  LGUID := LowerCase(AGUID);
  for i := 0 to Count-1 do
  begin
    if (LGUID = Items[i].GUID) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TACBrUSBWinDeviceList.FindDeviceByDescription(const ADeviceName: String): Integer;
var
  i, s: Integer;
  UDeviceName1, UDeviceName2: String;
begin
  // Partial Search and Ignore Case
  Result := -1;
  if Count < 1 then Exit;

  s := Length(ADeviceName);
  if (s = 0) then  // No DeviceName informed, get the first one
  begin
    Result := 0;
    Exit;
  end;

  UDeviceName1 := UpperCase(DeviceNameWithoutCOMPort(ADeviceName));

  for i := 0 to Count - 1 do
  begin
    UDeviceName2 := UpperCase(copy(DeviceNameWithoutCOMPort(Items[i].DeviceName), 1, s));
    if (UDeviceName1 = UDeviceName2) then
    begin
      Result := i;
      Break;
    end;
  end;
end;


{ TACBrUSBWinDeviceAPI }

constructor TACBrUSBWinDeviceAPI.Create;
begin
  inherited Create;

  FDeviceList := TACBrUSBWinDeviceList.Create();
  FDeviceIndex := -1;
  FLoaded := False;
  FUSBHandle := INVALID_HANDLE_VALUE;
  FInterfaceName := '';
  FTimeOut := 1000;
  FInternalBuffer := '';
  FHardwareType := htUnknown;

  xSetupDiEnumDeviceInfo := Nil;
  xSetupDiGetClassDevsA := Nil;
  xSetupDiGetDeviceRegistryProperty := Nil;
  xSetupDiEnumDeviceInterfaces := Nil;
  xSetupDiGetDeviceInterfaceDetail := Nil;
  xSetupDiDestroyDeviceInfoList := Nil;
end;

destructor TACBrUSBWinDeviceAPI.Destroy;
begin
  UnLoadAPI;
  FDeviceList.Free;
  inherited Destroy;
end;

procedure TACBrUSBWinDeviceAPI.LoadAPI;
const
  {$IfDef UNICODE}
   ApiSuffix = 'W';
  {$Else}
   ApiSuffix = 'A';
  {$EndIf}

begin
  if FLoaded then Exit;

  ACBrUtil.FilesIO.FunctionDetect(CSetupAPILibName, 'SetupDiEnumDeviceInfo', @xSetupDiEnumDeviceInfo);
  ACBrUtil.FilesIO.FunctionDetect(CSetupAPILibName, 'SetupDiGetClassDevsA', @xSetupDiGetClassDevsA);
  ACBrUtil.FilesIO.FunctionDetect(CSetupAPILibName, 'SetupDiGetDeviceRegistryProperty'+ApiSuffix, @xSetupDiGetDeviceRegistryProperty);
  ACBrUtil.FilesIO.FunctionDetect( CSetupAPILibName, 'SetupDiEnumDeviceInterfaces', @xSetupDiEnumDeviceInterfaces);
  ACBrUtil.FilesIO.FunctionDetect( CSetupAPILibName, 'SetupDiGetDeviceInterfaceDetail'+ApiSuffix, @xSetupDiGetDeviceInterfaceDetail);
  ACBrUtil.FilesIO.FunctionDetect( CSetupAPILibName, 'SetupDiDestroyDeviceInfoList', @xSetupDiDestroyDeviceInfoList);

  FLoaded := True;
end;

procedure TACBrUSBWinDeviceAPI.UnLoadAPI;
begin
  if not FLoaded then Exit;

  UnLoadLibrary( CSetupAPILibName );

  xSetupDiEnumDeviceInfo := Nil;
  xSetupDiGetClassDevsA := Nil;
  xSetupDiGetDeviceRegistryProperty := Nil;
  xSetupDiEnumDeviceInterfaces := Nil;
  xSetupDiGetDeviceInterfaceDetail := Nil;
  xSetupDiDestroyDeviceInfoList := Nil;

  FLoaded := False;
end;

procedure TACBrUSBWinDeviceAPI.ExtractVidAndPid(const ADeviceInterface: String; out
  AVid: String; out APid: String);
var
  lowInt: String;
  p: Integer;
begin
  AVid := '';
  APid := '';
  lowInt := LowerCase(ADeviceInterface);
  p := pos('vid_', lowInt);
  if (p > 0) then
    AVid := copy(ADeviceInterface, p+4, 4);

  p := pos('pid_', lowInt);
  if (p > 0) then
    APid := copy(ADeviceInterface, p+4, 4);
end;

function TACBrUSBWinDeviceAPI.GetActive: Boolean;
begin
  Result := (FUSBHandle <> INVALID_HANDLE_VALUE);
end;

function TACBrUSBWinDeviceAPI.GetDeviceRegistryPropertyString(DevInfo: HDEVINFO;
  DeviceInfoData: TSPDevInfoData; Prop: DWORD): AnsiString;
var
  Buffer: PByte;
  RequiredSize, PropertyRegDataTye: DWORD;
  AResult: {$IfDef UNICODE}WideString;{$Else}AnsiString;{$EndIf}
  Err, LenChar: DWORD;
begin
  Result := '';
  // Obtendo o tamanho do Buffer em "RequiredSize"
  PropertyRegDataTye := 0;
  RequiredSize := 0;
  xSetupDiGetDeviceRegistryProperty( DevInfo, DeviceInfoData, Prop,
                                     PropertyRegDataTye, Nil, 0, RequiredSize);
  Err := GetLastError;
  if ( (Err = ERROR_INSUFFICIENT_BUFFER) and (RequiredSize > 0) ) then
  begin
    Buffer := AllocMem(RequiredSize);
    try
      if xSetupDiGetDeviceRegistryProperty( DevInfo, DeviceInfoData, Prop,
                                            PropertyRegDataTye, Buffer,
                                            RequiredSize, RequiredSize) then
      begin
        {$IfDef UNICODE}
         LenChar := 2;
        {$Else}
         LenChar := 1;
        {$EndIf}

        SetLength(AResult, Trunc(RequiredSize/LenChar)-1);
        Move(Buffer^, AResult[1], RequiredSize-LenChar);
        Result := Trim(String(AResult));
        RequiredSize := Pos(NUL, Result);
        if (RequiredSize > 1) then
          Result := copy(Result, 1, RequiredSize-1);
      end;
    finally
      Freemem(Buffer);
    end;
  end;
end;

procedure TACBrUSBWinDeviceAPI.SetDeviceIndex(AValue: Integer);
begin
  if FDeviceIndex = AValue then Exit;

  if AValue < 0 then
    FDeviceIndex := -1
  else if (AValue >= FDeviceList.Count) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBDeviceOutOfRange), [AValue] );

  Close;
  FDeviceIndex := AValue;
end;

function TACBrUSBWinDeviceAPI.GetDataBase: TACBrUSBIDDataBase;
begin
  Result := FDeviceList.Database;
end;

function TACBrUSBWinDeviceAPI.FindUSBPrinters(ADeviceList: TACBrUSBWinDeviceList): Integer;
var
  ADeviceListToAdd: TACBrUSBWinDeviceList;
begin
  if (ADeviceList <> Nil) then
    ADeviceListToAdd := ADeviceList
  else
    ADeviceListToAdd := FDeviceList;

  ADeviceListToAdd.Clear;

  Result := FindUSBDevicesByGUID( GUID_DEVINTERFACE_USBPRINT, ADeviceListToAdd);
  Result := Result + FindUSBDevicesByGUID( GUID_DEVCLASS_PORT, ADeviceListToAdd);
  Result := Result + FindUSBDevicesByGUID( GUID_DEVCLASS_PRINTER, ADeviceListToAdd);
  Result := Result + FindUSBDevicesByGUID( GUID_EPSON_VENDORPRINTER_CLASS, ADeviceListToAdd, '\TM');
  //Result := Result + FindUSBDevicesByGUID( GUID_DEVINTERFACE_USB_DEVICE, ADeviceListToAdd);
  Result := Result + FindUSBKnownDevices(ADeviceListToAdd);
end;

function TACBrUSBWinDeviceAPI.FindUSBKnownDevices(ADeviceList: TACBrUSBWinDeviceList): Integer;
var
  ADeviceListToAdd: TACBrUSBWinDeviceList;
  i, j: Integer;
  ok: Boolean;
begin
  if (ADeviceList <> Nil) then
    ADeviceListToAdd := ADeviceList
  else
    ADeviceListToAdd := FDeviceList;

  i := ADeviceListToAdd.Count;
  Result := 0;
  FindUSBDevicesByGUID( GUID_DEVINTERFACE_USB_DEVICE, ADeviceListToAdd);
  while i < ADeviceListToAdd.Count do
  begin
    with ADeviceListToAdd.Items[i] do
    begin
      ok := (HardwareType = htUnknown) or (HardwareType = DeviceKind);
      ok := ok and (VendorName <> '');
      if ok then
      begin     // Remove if already exists
        j := ADeviceListToAdd.FindDeviceByGUID(GUID);
        ok := (j = i);
      end;

      if ok then
      begin
        inc(i);
        inc(Result);
      end
      else
        ADeviceListToAdd.Delete(i);
    end;
  end;
end;

function TACBrUSBWinDeviceAPI.FindUSBDevicesByGUID(AGUID: TGUID;
  ADeviceList: TACBrUSBWinDeviceList; DeviceSufix: String): Integer;
var
  DevInfo: HDEVINFO;
  DeviceInterface: TSPDeviceInterfaceData;
  DeviceInfoData: TSPDevInfoData;
  InterfaceDetail: PSPDeviceInterfaceDetailData;
  MemberIndex, RequiredSize, LenChar: DWORD;
  InterfaceName: {$IfDef UNICODE}WideString{$Else}AnsiString{$EndIf};
  DevInterface, DevClassGUID, DevLocation, DevFrendlyName, DevHardwareID,
    VendorId, ProductId: String;
  ADevice: TACBrUSBWinDevice;
  ADeviceListToAdd: TACBrUSBWinDeviceList;
  //E: DWORD;

  function TryGetDeviceRegistryPropertyString(AProp: Cardinal): String;
  begin
    try
      Result := String( GetDeviceRegistryPropertyString(DevInfo, DeviceInfoData, AProp) );
    except
      Result := '';
    end;
  end;

begin
  LoadAPI;
  Result := -1;
  if (ADeviceList <> Nil) then
    ADeviceListToAdd := ADeviceList
  else
    ADeviceListToAdd := FDeviceList;

  DevInfo := xSetupDiGetClassDevsA(@AGUID, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if (DevInfo = Pointer(INVALID_HANDLE_VALUE)) then
    Exit;

  try
    MemberIndex := 0;
    Result := 0;
    DeviceInterface.cbSize := SizeOf(TSPDeviceInterfaceData);
    while (xSetupDiEnumDeviceInterfaces(DevInfo, Nil, AGUID, MemberIndex, DeviceInterface)) do
    begin
      DevInterface := '';

      RequiredSize := 0;
      xSetupDiGetDeviceInterfaceDetail(DevInfo, @DeviceInterface, Nil, 0, RequiredSize, Nil);
      if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (RequiredSize > 0) then
      begin
        InterfaceDetail := AllocMem(RequiredSize);
        try
          InterfaceDetail^.cbSize := max(SizeOf(SizeUInt), SizeOf(TSPDeviceInterfaceDetailData));  // 64bits = 8, 32bits = 6, 32bits,ANSI = 5
          DeviceInfoData.cbSize := SizeOf(TSPDevInfoData);
          if xSetupDiGetDeviceInterfaceDetail(DevInfo, @DeviceInterface, InterfaceDetail, RequiredSize, RequiredSize, @DeviceInfoData) then
          begin
            {$IfDef UNICODE}
             LenChar := 2;
            {$Else}
             LenChar := 1;
            {$EndIf}

            SetLength(InterfaceName, Trunc(RequiredSize/LenChar)-1);
            Move(InterfaceDetail^.DevicePath[0], InterfaceName[1], RequiredSize-LenChar);
            DevInterface := Trim(String(InterfaceName));
            RequiredSize := Pos(NUL, DevInterface);
            if (RequiredSize > 1) then
              DevInterface := copy(DevInterface, 1, RequiredSize-1);
          end;
          //else
          //  E := GetLastError;

        finally
          Freemem(InterfaceDetail);
        end;
      end;

      if (DevInterface <> '') then
      begin
        ExtractVidAndPid( DevInterface, VendorId, ProductId);
        if (VendorId <> '') and (ProductId <> '') then
        begin
          DevClassGUID := TryGetDeviceRegistryPropertyString(SPDRP_CLASSGUID);
          DevLocation := TryGetDeviceRegistryPropertyString(SPDRP_LOCATION_INFORMATION);
          DevHardwareID := TryGetDeviceRegistryPropertyString(SPDRP_HARDWAREID);
          DevFrendlyName := TryGetDeviceRegistryPropertyString(SPDRP_FRIENDLYNAME);

          ADevice := ADeviceListToAdd.New(VendorId, ProductId);
          ADevice.ClassGUID := GUIDToString(AGUID);
          ADevice.DeviceInterface := DevInterface + DeviceSufix;
          ADevice.USBPort := DevLocation;
          ADevice.GUID :=  DevClassGUID;
          ADevice.FrendlyName := DevFrendlyName;
          ADevice.HardwareID := DevHardwareID;
          Inc( Result );
        end;
      end;

      Inc(MemberIndex);
    end;
  finally
    xSetupDiDestroyDeviceInfoList(DevInfo);
  end;
end;

procedure TACBrUSBWinDeviceAPI.Connect(AInterfaceName: String);
var
  APort: String;

  procedure CheckListIsLoaded;
  begin
    if FDeviceList.Count < 1 then
      FindUSBPrinters();
  end;

begin
  if Active then
    Close;

  APort := Trim(AInterfaceName);
  if UpperCase(copy(APort,1,3)) = 'USB' then  // USB ou USB:Nome
    APort := copy(AInterfaceName, 5, Length(AInterfaceName));

  CheckListIsLoaded;

  if (APort = '') then
  begin
    if FDeviceList.Count > 0 then
      DeviceIndex := 0
    else
      raise Exception.Create(sErrACBrWinUSBNotDeviceFound);
  end

  else if (copy(APort,1,2) = '\\') then
    DeviceIndex := -1

  else
  begin
    DeviceIndex := FDeviceList.FindDeviceByDescription(APort);
    if (DeviceIndex < 0) then
      raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBDescriptionNotFound), [AInterfaceName]);
  end;

  if DeviceIndex >= 0 then
    APort := FDeviceList.Items[DeviceIndex].DeviceInterface;

  {$IfDef UNICODE}
  FUSBHandle := CreateFileW( PWideChar(WideString(APort)),
  {$Else}
  FUSBHandle := CreateFileA( PAnsiChar(APort),
  {$EndIf}
                            GENERIC_WRITE or GENERIC_READ,
                            FILE_SHARE_READ,
                            Nil,
                            OPEN_ALWAYS,
                            FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN or FILE_FLAG_OVERLAPPED,
                            0);

  if FUSBHandle = INVALID_HANDLE_VALUE then
    raise Exception.CreateFmt(sErrACBrWinUSBOpening, [GetLastErrorAsHexaStr(), AInterfaceName]);

  SetupComm(FUSBHandle, CReceiveBufferSize, CReceiveBufferSize);
  FInterfaceName := AInterfaceName;
end;

procedure TACBrUSBWinDeviceAPI.Close;
begin
  if not Active then Exit;

  if not CloseHandle(FUSBHandle) then
    raise Exception.CreateFmt(sErrACBrWinUSBClosing, [GetLastErrorAsHexaStr(), FInterfaceName]);

  FUSBHandle := INVALID_HANDLE_VALUE;
  FInterfaceName := '';
end;

procedure TACBrUSBWinDeviceAPI.Purge;
begin
  DoLog('Purge');
  if Active then
  begin
    DoLog('  RX');
    PurgeComm(FUSBHandle, PURGE_RXABORT);
    DoLog('  TX');
    PurgeComm(FUSBHandle, PURGE_TXABORT);
  end;
end;

function TACBrUSBWinDeviceAPI.SendData(const AData: AnsiString; ATimeout: Integer
  ): Integer;
var
  x, BytesWritten, Err: DWORD;
  s: Integer;
  AOverlapped: OVERLAPPED;
begin
  {$IfDef FPC}Result := 0;{$EndIf}
  if not Active then
    raise Exception.Create(ACBrStr(sErrACBrWinUSBDeviceIsClosed));

  if ATimeout = 0 then
    ATimeout := FTimeOut;

  s := Length(AData);
  BytesWritten := 0;
  FillChar(AOverlapped, Sizeof(AOverlapped), 0);
  if ( Not WriteFile(FUSBHandle, AData[1], s, BytesWritten, @AOverlapped ) ) then
  begin
    Err := GetLastError;
    if Err = ERROR_IO_PENDING then
    begin
      x := WaitForSingleObject(FUSBHandle, ATimeOut);
      if (x = WAIT_TIMEOUT) then
      begin
        PurgeComm(FUSBHandle, PURGE_TXABORT);
        raise Exception.CreateFmt(sErrACBrWinUSBSendData, ['TimeOut', s]);
      end;

      GetOverlappedResult(FUSBHandle, AOverlapped, BytesWritten, False);
    end
    else if (Err <> ERROR_SUCCESS) then
      raise Exception.CreateFmt(sErrACBrWinUSBSendData, [GetLastErrorAsHexaStr(Err), s]);
  end;

  Result := BytesWritten;
end;

function TACBrUSBWinDeviceAPI.ReceiveNumBytes(BytesToRead: Integer;
  ATimeout: Integer): AnsiString;
var
  TimeoutTime: TDateTime;
begin
  Result := '';
  if BytesToRead <= 0 then Exit;

  if ATimeOut = 0 then
    ATimeOut := FTimeOut;

  DoLog('ReceiveNumBytes: BytesToRead: '+IntToStr(BytesToRead)+
                         ', TimeOut: '+IntToStr(ATimeOut));

  TimeoutTime := IncMilliSecond(Now, ATimeOut);
  while (Length(FInternalBuffer) < BytesToRead) and (now < TimeoutTime) do
    FInternalBuffer := FInternalBuffer + ReceivePacket(ATimeout);

  Result := copy(FInternalBuffer, 1, BytesToRead);
  if (Result <> '') then
    Delete(FInternalBuffer, 1, Length(Result));
end;

function TACBrUSBWinDeviceAPI.ReceiveTerminated(const ATerminator: AnsiString;
  ATimeOut: Integer): AnsiString;
var
  TimeoutTime: TDateTime;
  LenTer, p: Integer;
begin
  Result := '';
  LenTer := Length(ATerminator);
  if (LenTer = 0) then
    Exit;

  if ATimeOut = 0 then
    ATimeOut := FTimeOut;

  DoLog('ReceiveTerminated: Terminator: '+ATerminator+
                         ', TimeOut: '+IntToStr(ATimeOut), True);
  p := pos(ATerminator, FInternalBuffer);
  TimeoutTime := IncMilliSecond(Now, ATimeOut);
  while (p < 1) and (now < TimeoutTime) do
  begin
    FInternalBuffer := FInternalBuffer + ReceivePacket(ATimeout);
    p := pos(ATerminator, FInternalBuffer);
  end;

  if (p > 0) then
  begin
    Result := copy(FInternalBuffer, 1, p-1);
    Delete(FInternalBuffer, 1, p + LenTer-1);
  end;
end;

function TACBrUSBWinDeviceAPI.ReceivePacket(ATimeOut: Integer): AnsiString;
var
  ABuffer: AnsiString;
  AOverlapped: OVERLAPPED;
  BytesReaded, Err, x: DWORD;
  TimeoutTime: TDateTime;

  procedure CalcOverallTimeOut;
  begin
    TimeoutTime := IncMilliSecond(now, ATimeOut);
  end;

begin
  if not Active then
    raise Exception.Create(ACBrStr(sErrACBrWinUSBDeviceIsClosed));

  if ATimeOut = 0 then
    ATimeOut := FTimeOut;

  DoLog('ReceivePacket: TimeOut: '+IntToStr(ATimeOut));

  Result := '';
  CalcOverallTimeOut;
  repeat
    BytesReaded := 0;
    ABuffer := StringOfChar(#0,CReceiveBufferSize);
    FillChar(AOverlapped, Sizeof(AOverlapped), 0);
    ReadFile(FUSBHandle, ABuffer[1], CReceiveBufferSize, BytesReaded, @AOverlapped);
    Err := GetLastError;
    DoLog('  ReadFile: Err: '+IntToStr(Err));
    if (Err = ERROR_IO_PENDING) then
    begin
      x := WaitForSingleObject(FUSBHandle, ATimeOut);
      if (x = WAIT_TIMEOUT) then
      begin
        DoLog('  ReadFile: TimeOut');
        PurgeComm(FUSBHandle, PURGE_RXABORT);
        Break;
      end;

      GetOverlappedResult(FUSBHandle, AOverlapped, BytesReaded, False);
      Err := GetLastError;
    end;

    DoLog('  Err: '+IntToStr(Err)+ ', BytesReaded: '+IntToStr(BytesReaded));

    if (BytesReaded > 0) then
    begin
      DoLog('  Buffer Lido: '+copy(ABuffer, 1, BytesReaded), True);
      Result := Result + copy(ABuffer, 1, BytesReaded);
    end;

    if (Err = ERROR_IO_INCOMPLETE) then
      CalcOverallTimeOut;

  until (BytesReaded > 0) or (now > TimeoutTime);
end;

procedure TACBrUSBWinDeviceAPI.DoLog(const AMessage: AnsiString; Translate: Boolean;
  AddTimeStamp: Boolean);
var
  Handled: Boolean;
  MessageToLog: String;
begin
  Handled := False;

  if Translate then
    MessageToLog := TranslateUnprintable(AMessage)
  else
    MessageToLog := String(AMessage);

  if Assigned(FOnLog) then
    FOnLog(MessageToLog, Handled);

  if (not Handled) and (FLogFile <> '') then
  begin
    if AddTimeStamp then
      MessageToLog := '-- ' + FormatDateTime('dd/mm hh:nn:ss:zzz', now) + ' - ' + MessageToLog;

    WriteLog(FLogFile, MessageToLog);
  end;
end;

end.
