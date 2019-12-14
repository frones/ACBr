{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}
{******************************************************************************
|* 13/12/2019: Primeira Versao: Daniel Simoes de Almeida
|* Fontes:
|* //https://web.archive.org/web/20130126085547/http://peter.skarpetis.com/wiki/index.php/Usbprint.sys
******************************************************************************}

{$I ACBr.inc}

unit ACBrWinUSBPrinter;

interface

uses
  Classes, SysUtils, Windows, IniFiles, contnrs
  {$IfDef FPC}
  , dynlibs
  {$EndIf};

const
  CUSBIDDataBaseResourceName = 'ACBrUSBID';
  CUSBIDDataBaseFileName = 'ACBrUSBID.ini';
  CSecVendors = 'Vendors';

resourcestring
  sErrACBrWinUSBInvalidID = '%S inválido [%s]';
  sErrACBrWinUSBBufferSize = '%s: Falha ao obter Tamanho do Buffer. Erro: %s';
  sErrACBrWinUSBBufferRead = '%s: Falha ao Ler Buffer. Erro: %s';

type

  { TACBrUSBIDDataBase }

  TACBrUSBIDDataBase = class
  private
    FIni: TMemIniFile;
    FDataBaseFileName: String;
    FResourceName: String;
    FLoaded: Boolean;
    function LoadFromFile: Boolean;
    function LoadFromResource: Boolean;
    procedure SetDataBaseFileName(AValue: String);
  public
    constructor Create(ADataBaseFileName: String = ''; AResourceName: String = '');
    destructor Destroy; override;
    procedure Clear;
    function Load: Boolean;

    property DataBaseFileName: String read FDataBaseFileName write SetDataBaseFileName;
    property ResourceName: String read FResourceName;
    function GetDescription(AVendorID, AProductID: String; out AVendorName: String; out AProductModel: String): Boolean;
  end;

  TACBrUSBWinPrinterList = class;

  { TACBrUSBWinPrinter }

  TACBrUSBWinPrinter = class
  private
    FFrendlyName: String;
    FGUID: String;
    FHardwareID: String;
    FOwner: TACBrUSBWinPrinterList;
    FPrinterInterface: String;
    FProductID, FProductModel: String;
    FUSBPort: String;
    FVendorID, FVendorName: String;
    FDescriptionsLoaded: Boolean;

    function GetPrinterName: String;
    function GetProductModel: String;
    function GetVendorName: String;
    procedure SetProductID(AValue: String);
    procedure SetVendorID(AValue: String);
    procedure LoadDescriptions;
  public
    constructor Create(AOwner: TACBrUSBWinPrinterList);
    procedure Clear;

    property PrinterName: String read GetPrinterName;
    property VendorID: String read FVendorID write SetVendorID;
    property VendorName: String read GetVendorName;
    property ProductID: String read FProductID write SetProductID;
    property ProductModel: String read GetProductModel;
    property PrinterInterface: String read FPrinterInterface write FPrinterInterface;
    property USBPort: String read FUSBPort write FUSBPort;
    property GUID: String read FGUID write FGUID;
    property FrendlyName: String read FFrendlyName write FFrendlyName;
    property HardwareID: String read FHardwareID write FHardwareID;
  end;

  { TACBrUSBWinPrinterList }

  TACBrUSBWinPrinterList = class(TObjectList)
  private
    FDataBase: TACBrUSBIDDataBase;

    function GetItem(Index: Integer): TACBrUSBWinPrinter;
  public
    constructor Create(ADataBaseFileName: String = '');
    destructor Destroy; override;

    function New(AVendorID, AProductID: String): TACBrUSBWinPrinter;
    property Items[Index: Integer]: TACBrUSBWinPrinter read GetItem;
    property Database: TACBrUSBIDDataBase read FDataBase;
  end;


{ SetupAPI Bind }
const
  CSetupAPILibName = 'SetupApi.dll';

  GUID_DEVCLASS_NET : TGUID = '{4D36E972-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PORT : TGUID = '{4D36E978-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVINTERFACE_USBPRINT: TGUID = '{28D78FAD-5A12-11D1-AE5B-0000F803A8C2}';

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
    cbSize: NativeUInt;
    ClassGuid: TGUID;
    DevInst: DWORD;
    Reserved: ULONG_PTR;
  end;
  TSPDevInfoData = SP_DEVINFO_DATA;

  PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;
  SP_DEVICE_INTERFACE_DATA = packed record
    cbSize: NativeUInt;
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

  { TACBrUSBWinSetupAPI }

  TACBrUSBWinSetupAPI = class
  private
    FLoaded: Boolean;

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
    procedure ExtractVidAndPid( APrinterInterface: String; out AVid: String; out APid: String);

    function GetDeviceRegistryPropertyString(DevInfo: HDEVINFO;
      DeviceInfoData: TSPDevInfoData; Prop: DWORD): AnsiString;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadAPI;
    procedure UnLoadAPI;

    property Loaded: Boolean read FLoaded;

    function FindUSBPrinters(PrinterList: TACBrUSBWinPrinterList): Integer;
    function FindUSBDevicesByGUID(AGUID: TGUID; PrinterList: TACBrUSBWinPrinterList): Integer;
  end;

implementation

uses
  Types,
  synautil,
  ACBrUtil, ACBrConsts;

{$IFDEF FPC}
 {$R ACBrUSBID.rc}
{$ELSE}
 {$R ACBrUSBID.res}
{$ENDIF}


{ TACBrUSBIDDataBase }

procedure TACBrUSBIDDataBase.SetDataBaseFileName(AValue: String);
begin
  if (FDataBaseFileName = AValue) then
    Exit;

  if (AValue <> '') and (not FileExists(AValue)) then
    raise Exception.CreateFmt(ACBrStr(cACBrArquivoNaoEncontrado), [AValue]);

  FDataBaseFileName := AValue;
  Load;
end;

function TACBrUSBIDDataBase.LoadFromFile: Boolean;
var
  SL: TStringList;
begin
  Result := False;
  if (FDataBaseFileName <> '') and FileExists(FDataBaseFileName) then
  begin
    FIni.Free;
    FIni.Create(FDataBaseFileName);

    SL := TStringList.Create;
    try
      SL.LoadFromFile(FDataBaseFileName);
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
  Result := False;
  RS := TResourceStream.Create(HInstance, FResourceName, Windows.RT_RCDATA);
  SL := TStringList.Create;
  try
    RS.Position := 0;
    SL.LoadFromStream(RS);
    FIni.SetStrings(SL);
    Result := True;
  finally
    RS.Free;
    SL.Free;
  end;
end;

constructor TACBrUSBIDDataBase.Create(ADataBaseFileName: String; AResourceName: String);
begin
  inherited Create;
  FIni := TMemIniFile.Create('');

  if ADataBaseFileName = '' then
    FDataBaseFileName := ApplicationPath + CUSBIDDataBaseFileName
  else
    FDataBaseFileName := ADataBaseFileName;

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
end;

function TACBrUSBIDDataBase.GetDescription(AVendorID, AProductID: String; out AVendorName: String; out AProductModel: String): Boolean;
begin
  Result := False;
  AVendorName := '';
  AProductModel := '';

  if not StrIsHexa(AVendorID) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['VendorID', AVendorID]);

  if not StrIsHexa(AProductID) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['ProductID', AProductID]);

  if not FLoaded then
    Load;

  AVendorName := FIni.ReadString(CSecVendors, AVendorID, '');
  AProductModel := FIni.ReadString(AVendorID, AProductID, '');

  Result := ((AVendorName + AProductModel) <> '');
end;


{ TACBrUSBWinPrinter }

constructor TACBrUSBWinPrinter.Create(AOwner: TACBrUSBWinPrinterList);
begin
  inherited Create;

  FOwner := AOwner;
  Clear;
end;

procedure TACBrUSBWinPrinter.Clear;
begin
  FVendorID := '';
  FProductID := '';
  FVendorName := '';
  FProductModel := '';
  FPrinterInterface := '';
  FUSBPort := '';
  FGUID := '';
  FFrendlyName := '';
  FHardwareID := '';
  FDescriptionsLoaded := False;
end;

function TACBrUSBWinPrinter.GetPrinterName: String;
begin
  Result := IfEmptyThen( FrendlyName,
                         IfEmptyThen(VendorName, VendorID) + ' - ' +
                         IfEmptyThen(ProductModel, ProductID));
end;

function TACBrUSBWinPrinter.GetProductModel: String;
begin
  if not FDescriptionsLoaded then
    LoadDescriptions;

  Result := FProductModel;
end;

function TACBrUSBWinPrinter.GetVendorName: String;
begin
  if not FDescriptionsLoaded then
    LoadDescriptions;

  Result := FVendorName;
end;

procedure TACBrUSBWinPrinter.SetProductID(AValue: String);
begin
  if FProductID = AValue then
    Exit;
  if not StrIsHexa(AValue) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['ProductID', AValue]);

  FProductID := AValue;
end;

procedure TACBrUSBWinPrinter.SetVendorID(AValue: String);
begin
  if FVendorID = AValue then
    Exit;
  if not StrIsHexa(AValue) then
    raise Exception.CreateFmt(ACBrStr(sErrACBrWinUSBInvalidID), ['VendorID', AValue]);

  FVendorID := AValue;
end;

procedure TACBrUSBWinPrinter.LoadDescriptions;
begin
  FVendorName := '';
  FProductModel := '';
  FOwner.Database.GetDescription(FVendorID, FProductID, FVendorName, FProductModel);
  FDescriptionsLoaded := True;
end;


{ TACBrUSBWinPrinterList }

function TACBrUSBWinPrinterList.GetItem(Index: Integer): TACBrUSBWinPrinter;
begin
  Result := TACBrUSBWinPrinter(inherited Items[Index]);
end;

constructor TACBrUSBWinPrinterList.Create(ADataBaseFileName: String);
begin
  inherited Create(True);
  FDataBase := TACBrUSBIDDataBase.Create(ADataBaseFileName);
end;

destructor TACBrUSBWinPrinterList.Destroy;
begin
  FDataBase.Free;
  inherited Destroy;
end;

function TACBrUSBWinPrinterList.New(AVendorID, AProductID: String
  ): TACBrUSBWinPrinter;
begin
  Result := TACBrUSBWinPrinter.Create(Self);
  try
    Result.VendorID := AVendorID;
    Result.ProductID := AProductID;
    inherited Add(Result);
  except
    Result.Free;
    raise;
  end;
end;


{ TACBrUSBWinSetupAPI }

constructor TACBrUSBWinSetupAPI.Create;
begin
  inherited Create;
  FLoaded := False;
  xSetupDiEnumDeviceInfo := Nil;
  xSetupDiGetClassDevsA := Nil;
  xSetupDiGetDeviceRegistryProperty := Nil;
  xSetupDiEnumDeviceInterfaces := Nil;
  xSetupDiGetDeviceInterfaceDetail := Nil;
  xSetupDiDestroyDeviceInfoList := Nil;
end;

destructor TACBrUSBWinSetupAPI.Destroy;
begin
  UnLoadAPI;
  inherited Destroy;
end;

procedure TACBrUSBWinSetupAPI.LoadAPI;
const
  {$IfDef UNICODE}
   ApiSuffix = 'W';
  {$Else}
   ApiSuffix = 'A';
  {$EndIf}

begin
  if FLoaded then Exit;

  FunctionDetect(CSetupAPILibName, 'SetupDiEnumDeviceInfo', @xSetupDiEnumDeviceInfo);
  FunctionDetect(CSetupAPILibName, 'SetupDiGetClassDevsA', @xSetupDiGetClassDevsA);
  FunctionDetect(CSetupAPILibName, 'SetupDiGetDeviceRegistryProperty'+ApiSuffix, @xSetupDiGetDeviceRegistryProperty);
  FunctionDetect( CSetupAPILibName, 'SetupDiEnumDeviceInterfaces', @xSetupDiEnumDeviceInterfaces);
  FunctionDetect( CSetupAPILibName, 'SetupDiGetDeviceInterfaceDetail'+ApiSuffix, @xSetupDiGetDeviceInterfaceDetail);
  FunctionDetect( CSetupAPILibName, 'SetupDiDestroyDeviceInfoList', @xSetupDiDestroyDeviceInfoList);

  FLoaded := True;
end;

procedure TACBrUSBWinSetupAPI.UnLoadAPI;
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

procedure TACBrUSBWinSetupAPI.ExtractVidAndPid(APrinterInterface: String; out
  AVid: String; out APid: String);
var
  lowInt: String;
  p: Integer;
begin
  AVid := '';
  APid := '';
  lowInt := LowerCase(APrinterInterface);
  p := pos('vid_', lowInt);
  if (p > 0) then
    AVid := copy(APrinterInterface, p+4, 4);

  p := pos('pid_', lowInt);
  if (p > 0) then
    APid := copy(APrinterInterface, p+4, 4);
end;

function TACBrUSBWinSetupAPI.GetDeviceRegistryPropertyString(DevInfo: HDEVINFO;
  DeviceInfoData: TSPDevInfoData; Prop: DWORD): AnsiString;
var
  Buffer: PByte;
  RequiredSize, PropertyRegDataTye: DWORD;
  {$IfDef UNICODE}
   AResult: WideString;
  {$Else}
   AResult: AnsiString;
  {$EndIf}
  Err: DWORD;
begin
  // Obtendo o tamanho do Buffer em "RequiredSize"
  PropertyRegDataTye := 0;
  RequiredSize := 0;
  xSetupDiGetDeviceRegistryProperty( DevInfo, DeviceInfoData, Prop,
                                     PropertyRegDataTye, Nil, 0, RequiredSize);
  Err := GetLastError;
  if (Err <> ERROR_INSUFFICIENT_BUFFER) or (RequiredSize < 1) then
    raise Exception.CreateFmt( sErrACBrWinUSBBufferSize, ['SetupDiGetDeviceRegistryProperty',  GetLastErrorAsHexaStr(Err)] );

  Buffer := AllocMem(RequiredSize);
  try
    if not xSetupDiGetDeviceRegistryProperty( DevInfo, DeviceInfoData, Prop,
                                              PropertyRegDataTye, Buffer,
                                              RequiredSize, RequiredSize) then
      raise Exception.CreateFmt( sErrACBrWinUSBBufferRead, ['SetupDiGetDeviceRegistryProperty',  GetLastErrorAsHexaStr] );

    SetLength(AResult, RequiredSize-1);
    Move(Buffer^, AResult[1], RequiredSize-1);

    Result := String(AResult);
  finally
    Freemem(Buffer);
  end;
end;

function TACBrUSBWinSetupAPI.FindUSBPrinters(PrinterList: TACBrUSBWinPrinterList
  ): Integer;
begin
  Result := FindUSBDevicesByGUID( GUID_DEVINTERFACE_USBPRINT, PrinterList);
end;

function TACBrUSBWinSetupAPI.FindUSBDevicesByGUID(AGUID: TGUID;
  PrinterList: TACBrUSBWinPrinterList): Integer;
var
  DevInfo: HDEVINFO;
  DeviceInterface: TSPDeviceInterfaceData;
  DeviceInfoData: TSPDevInfoData;
  InterfaceDetail: PSPDeviceInterfaceDetailData;
  MemberIndex, RequiredSize: DWORD;
  {$IfDef UNICODE}
   InterfaceName: WideString;
  {$Else}
   InterfaceName:AnsiString;
  {$EndIf}
  PrinterInterface, PrinterClassGUID, PrinterLocation,
    PrinterFrendlyName, PrinterHardwareID, VendorId, ProductId: String;
  APrinter: TACBrUSBWinPrinter;
begin
  LoadAPI;
  Result := -1;
  PrinterList.Clear;

  DevInfo := xSetupDiGetClassDevsA(@AGUID, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if (DevInfo = Pointer(INVALID_HANDLE_VALUE)) then
    Exit;

  try
    MemberIndex := 0;
    DeviceInterface.cbSize := SizeOf(TSPDeviceInterfaceData);
    while (xSetupDiEnumDeviceInterfaces(DevInfo, Nil, AGUID, MemberIndex, DeviceInterface)) do
    begin
      PrinterInterface := '';

      RequiredSize := 0;
      xSetupDiGetDeviceInterfaceDetail(DevInfo, @DeviceInterface, Nil, 0, RequiredSize, Nil);
      if (GetLastError = ERROR_INSUFFICIENT_BUFFER) and (RequiredSize > 0) then
      begin
        InterfaceDetail := AllocMem(RequiredSize);
        try
          InterfaceDetail^.cbSize := SizeOf(TSPDeviceInterfaceDetailData);
          DeviceInfoData.cbSize := SizeOf(TSPDevInfoData);
          if xSetupDiGetDeviceInterfaceDetail(DevInfo, @DeviceInterface, InterfaceDetail, RequiredSize, RequiredSize, @DeviceInfoData) then
          begin
            SetLength(InterfaceName, RequiredSize-1);
            Move(InterfaceDetail^.DevicePath[0], InterfaceName[1], RequiredSize-1);
            PrinterInterface := String(InterfaceName);
          end;
        finally
          Freemem(InterfaceDetail);
        end;
      end;

      if (PrinterInterface <> '') then
      begin
        ExtractVidAndPid( PrinterInterface, VendorId, ProductId);
        if (VendorId <> '') and (ProductId <> '') then
        begin
          PrinterClassGUID := GetDeviceRegistryPropertyString(DevInfo, DeviceInfoData, SPDRP_CLASSGUID);
          PrinterLocation := GetDeviceRegistryPropertyString(DevInfo, DeviceInfoData, SPDRP_LOCATION_INFORMATION);
          PrinterHardwareID := GetDeviceRegistryPropertyString(DevInfo, DeviceInfoData, SPDRP_HARDWAREID);
          try
            PrinterFrendlyName := GetDeviceRegistryPropertyString(DevInfo, DeviceInfoData, SPDRP_FRIENDLYNAME);
          except
            PrinterFrendlyName := '';
          end;

          APrinter := PrinterList.New(VendorId, ProductId);
          APrinter.PrinterInterface := PrinterInterface;
          APrinter.USBPort := PrinterLocation;
          APrinter.GUID := PrinterClassGUID;
          APrinter.FrendlyName := PrinterFrendlyName;
          APrinter.HardwareID := PrinterHardwareID;
        end;
      end;

      Inc(MemberIndex);
    end;
  finally
    xSetupDiDestroyDeviceInfoList(DevInfo);
  end;

  Result := PrinterList.Count;
end;


end.
