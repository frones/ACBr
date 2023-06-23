{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Jaques Nascimento                               }
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

unit ACBr.Android.Info;

interface

uses
  System.Classes,
  System.Types,
{$IFDEF ANDROID}
  Androidapi.JNI.Os,
{$ENDIF}
  ACBrBase;

type

  [ComponentPlatformsAttribute(piacbrAllAndroidPlatforms)]
  TACBrAndroidInfo = Class(TComponent)
  Private
{$IFDEF ANDROID}
    FBuild: TJBuild;
    FBuild_VERSION: TJBuild_VERSION;
{$ENDIF}
    FTTY: TSTringList;
    FSecondDisplays: Integer;

    function GetBOARD: String;
    function GetDEVICE: String;
    function GetDISPLAY: String;
    function GetMANUFACTURER: String;
    function GetMODEL: String;
    function GetPRODUCT: String;
    function GetUSER: String;
    function GetFINGERPRINT: String;
    function GetRADIO: String;
    function GetHOST: String;
    function GetID: String;
    function GetTAGS: String;
    function GetBASE_OS: String;
    function GetCODENAME: String;
    function GetINCREMENTAL: String;
    function GetPREVIEW_SDK_INT: Integer;
    function GetRELEASE: String;
    function GetSDK: String;
    function GetSDK_INT: Integer;
    function GetSECURITY_PATCH: String;
    Function GetSERIAL: String;

    function GetSecondDisplay: Integer;
    function GetHasSecondDisplay: Boolean;
{$IFDEF ANDROID}
    procedure GetTTYDevices;
{$ENDIF}
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    property MANUFACTURER: String read GetMANUFACTURER;
    property MODEL: String read GetMODEL;
    property DEVICE: String read GetDEVICE;
    property PRODUCT: String read GetPRODUCT;
    property BOARD: String read GetBOARD;
    property DISPLAY: String read GetDISPLAY;
    property USER: String read GetUSER;
    property FINGERPRINT: String read GetFINGERPRINT;
    property RADIO: String read GetRADIO;
    property HOST: String read GetHOST;
    property ID: String read GetID;
    property TAGS: String read GetTAGS;
    property BASE_OS: String read GetBASE_OS;
    property CODENAME: String read GetCODENAME;
    property INCREMENTAL: String read GetINCREMENTAL;
    property PREVIEW_SDK_INT: Integer read GetPREVIEW_SDK_INT;
    property RELEASE: String read GetRELEASE;
    property SDK: String read GetSDK;
    property SDK_INT: Integer read GetSDK_INT;
    property SECURITY_PATCH: String read GetSECURITY_PATCH;
    property SERIAL: String read GetSERIAL;

    property TTY: TStringList read FTTY;
    property SecondDisplay: Integer read GetSecondDisplay;
    property HasSecondDisplay: Boolean read GetHasSecondDisplay;
  End;

implementation

uses
{$IFDEF ANDROID}
  FMX.Platform.Android,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.GraphicsContentViewText,
{$ENDIF}
  System.SysUtils;

{ TACBrAndroidInfo }

constructor TACBrAndroidInfo.Create(aOwner: TComponent);
begin
  inherited;
  FSecondDisplays := -1;
  FTTY := TStringList.Create;
{$IFDEF ANDROID}
  GetTTYDevices;
{$ENDIF}
end;

destructor TACBrAndroidInfo.Destroy;
begin
  FTTY.Free;
  inherited;
end;

{$IFDEF ANDROID}
procedure TACBrAndroidInfo.GetTTYDevices;
const
  ATTR = {$IFDEF POSIX}$7FFFFFFF{$ELSE}$FFFFFFFF{$ENDIF};
Var
  ret: Integer;
  sr: TSearchRec;
begin
  FTTY.Clear;

  if FindFirst('/dev/tty*', ATTR, sr) = 0 then
  begin
    repeat
      if ((sr.Attr and ATTR) = sr.Attr) then
        FTTY.Add('/dev/' + sr.Name);
    until (FindNext(sr) <> 0);
  end;

  FindClose(sr);
end;
{$ENDIF}

function TACBrAndroidInfo.GetBASE_OS: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild_VERSION.JavaClass.BASE_OS);
{$ELSE}
  Result := 'BASE_OS';
{$ENDIF}
end;

function TACBrAndroidInfo.GetBOARD: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.BOARD);
{$ELSE}
  Result := 'BOARD';
{$ENDIF}
end;

function TACBrAndroidInfo.GetCODENAME: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild_VERSION.JavaClass.CODENAME);
{$ELSE}
  Result := 'CODENAME';
{$ENDIF}
end;

function TACBrAndroidInfo.GetDEVICE: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.DEVICE);
{$ELSE}
  Result := 'DEVICE';
{$ENDIF}
end;

function TACBrAndroidInfo.GetDISPLAY: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.DISPLAY);
{$ELSE}
  Result := 'DISPLAY';
{$ENDIF}
end;

function TACBrAndroidInfo.GetFINGERPRINT: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.FINGERPRINT);
{$ELSE}
  Result := 'FINGERPRINT';
{$ENDIF}
end;

function TACBrAndroidInfo.GetHOST: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.HOST);
{$ELSE}
  Result := 'HOST';
{$ENDIF}
end;

function TACBrAndroidInfo.GetID: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.ID);
{$ELSE}
  Result := 'ID';
{$ENDIF}
end;

function TACBrAndroidInfo.GetINCREMENTAL: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild_VERSION.JavaClass.INCREMENTAL);
{$ELSE}
  Result := 'INCREMENTAL';
{$ENDIF}
end;

function TACBrAndroidInfo.GetMANUFACTURER: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.MANUFACTURER);
{$ELSE}
  Result := 'MANUFACTURER';
{$ENDIF}
end;

function TACBrAndroidInfo.GetMODEL: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.MODEL);
{$ELSE}
  Result := 'MODEL';
{$ENDIF}
end;

function TACBrAndroidInfo.GetPREVIEW_SDK_INT: Integer;
begin
{$IFDEF ANDROID}
  Result := FBuild_VERSION.JavaClass.PREVIEW_SDK_INT;
{$ELSE}
  Result := -1;
{$ENDIF}
end;

function TACBrAndroidInfo.GetPRODUCT: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.PRODUCT);
{$ELSE}
  Result := 'PRODUCT';
{$ENDIF}
end;

function TACBrAndroidInfo.GetRADIO: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.RADIO);
{$ELSE}
  Result := 'RADIO';
{$ENDIF}
end;

function TACBrAndroidInfo.GetRELEASE: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild_VERSION.JavaClass.RELEASE);
{$ELSE}
  Result := 'RELEASE';
{$ENDIF}
end;

function TACBrAndroidInfo.GetSDK: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild_VERSION.JavaClass.SDK);
{$ELSE}
  Result := 'SDK';
{$ENDIF}
end;

function TACBrAndroidInfo.GetSDK_INT: Integer;
begin
{$IFDEF ANDROID}
  Result := FBuild_VERSION.JavaClass.SDK_INT;
{$ELSE}
  Result := -1;
{$ENDIF}
end;

function TACBrAndroidInfo.GetSecondDisplay: Integer;
{$IFDEF ANDROID}
Var
   LObject: JObject;
   LManager: JDisplayManager;
   LDisplays: TJavaObjectArray<JDisplay>;
{$ENDIF}
begin
  if (FSecondDisplays < 0) then
  begin
{$IFDEF ANDROID}
    LObject   := MainActivity.getBaseContext.getSystemService(TJContext.JavaClass.DISPLAY_SERVICE);
    LManager  := TJDisplayManager.Wrap(LObject);
    LDisplays := LManager.getDisplays(TJDisplayManager.JavaClass.DISPLAY_CATEGORY_PRESENTATION); //JDisplayManager(LObject).getDisplays;
    FSecondDisplays := LDisplays.Length;
{$ELSE}
    FSecondDisplays := 0;
{$ENDIF}
  end;

  Result := FSecondDisplays;
end;

function TACBrAndroidInfo.GetHasSecondDisplay: Boolean;
begin
  Result := (SecondDisplay > 0)
end;

function TACBrAndroidInfo.GetSECURITY_PATCH: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild_VERSION.JavaClass.SECURITY_PATCH);
{$ELSE}
  Result := 'SECURITY_PATCH';
{$ENDIF}
end;

function TACBrAndroidInfo.GetSERIAL: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.SERIAL);
{$ELSE}
  Result := 'SERIAL';
{$ENDIF}
end;

function TACBrAndroidInfo.GetTAGS: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.TAGS);
{$ELSE}
  Result := 'TAGS';
{$ENDIF}
end;

function TACBrAndroidInfo.GetUSER: String;
begin
{$IFDEF ANDROID}
  Result := JStringToString(FBuild.JavaClass.USER);
{$ELSE}
  Result := 'USER';
{$ENDIF}
end;

end.
