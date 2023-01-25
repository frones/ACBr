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

unit ACBr.Android.Sunmi.Leds;

{$I ACBr.inc}

interface

Uses
  ACBrBase,
  {$IFDEF ANDROID}
  FMX.Helpers.Android,
  FMX.Platform.Android,
  ACBr.Androidapi.JNI,
  Androidapi.AppGlue,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI.Net,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Media,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  {$ENDIF}
  System.Types,
  System.UITypes,
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Messaging,
  System.Generics.Collections,
  FMX.Platform,
  FMX.Types,
  FMX.Forms,
  FMX.Controls,
  FMX.Layouts,
  FMX.Surfaces,
  FMX.Graphics;
Type

   [ComponentPlatformsAttribute (piacbrAllAndroidPlatforms)]
   TACBrSunmiLeds = Class(TComponent)
      Private
         {$IFDEF ANDROID}
         FLeds : JStateLeds;
         {$ENDIF}
         FLed1: Boolean;
         FLed2: Boolean;
         FLed3: Boolean;
         FLed4: Boolean;
         FLed5: Boolean;
         FLed6: Boolean;
         procedure SetLed1(const Value: Boolean);
         procedure SetLed2(const Value: Boolean);
         procedure SetLed3(const Value: Boolean);
         procedure SetLed4(const Value: Boolean);
         procedure SetLed5(const Value: Boolean);
         procedure SetLed6(const Value: Boolean);
      Public
         Constructor Create(aOwner : TComponent); Override;
         Destructor Destroy; Override;
         Procedure StartFlash(aLed, aOnTime, aOffTime : LongInt);
         Procedure StopFlash(aLed : Integer);
         Procedure TurnOffAll;
      Published
         Property Led1 : Boolean  Read FLed1  Write SetLed1;
         Property Led2 : Boolean  Read FLed2  Write SetLed2;
         Property Led3 : Boolean  Read FLed3  Write SetLed3;
         Property Led4 : Boolean  Read FLed4  Write SetLed4;
         Property Led5 : Boolean  Read FLed5  Write SetLed5;
         Property Led6 : Boolean  Read FLed6  Write SetLed6;
      End;

implementation

{ TACBrLeds }

constructor TACBrSunmiLeds.Create(aOwner: TComponent);
begin
  inherited;
  {$IFDEF ANDROID}
   FLeds := TJStateLeds.JavaClass.init(MainActivity);
  {$ENDIF}
end;

destructor TACBrSunmiLeds.Destroy;
begin
  inherited;
end;

procedure TACBrSunmiLeds.StartFlash(aLed, aOnTime, aOffTime: LongInt);
begin
  {$IFDEF ANDROID}
   FLeds.controlLampForLoop(0, aOnTime, aOffTime, StringToJString('Led-'+aLed.ToString));
  {$ENDIF}
  case aLed of
    1 : FLed1 := True;
    2 : FLed2 := True;
    3 : FLed3 := True;
    4 : FLed4 := True;
    5 : FLed5 := True;
    6 : FLed6 := True;
  end;
end;

procedure TACBrSunmiLeds.StopFlash(aLed: Integer);
begin
  {$IFDEF ANDROID}
  FLeds.controlLampForLoop(1, 0, 0, StringToJString('Led-'+aLed.ToString));
  {$ENDIF}
  case aLed of
    1 : FLed1 := False;
    2 : FLed2 := False;
    3 : FLed3 := False;
    4 : FLed4 := False;
    5 : FLed5 := False;
    6 : FLed6 := False;
  end;
end;

procedure TACBrSunmiLeds.TurnOffAll;
begin
{$IFDEF ANDROID}
FLeds.closeAllLamp;
{$ENDIF}
FLed1 := False;
FLed2 := False;
FLed3 := False;
FLed4 := False;
FLed5 := False;
FLed6 := False;
end;

procedure TACBrSunmiLeds.SetLed1(const Value: Boolean);
begin
FLed1 := Value;
{$IFDEF ANDROID}
case Value of
   True  : FLeds.controlLamp(0, StringToJString('Led-1'));
   False : FLeds.controlLamp(1, StringToJString('Led-1'));
   end;
{$ENDIF}
end;

procedure TACBrSunmiLeds.SetLed2(const Value: Boolean);
begin
FLed2 := Value;
{$IFDEF ANDROID}
case Value of
   True  : FLeds.controlLamp(0, StringToJString('Led-2'));
   False : FLeds.controlLamp(1, StringToJString('Led-2'));
   end;
{$ENDIF}
end;

procedure TACBrSunmiLeds.SetLed3(const Value: Boolean);
begin
FLed3 := Value;
{$IFDEF ANDROID}
case Value of
   True  : FLeds.controlLamp(0, StringToJString('Led-3'));
   False : FLeds.controlLamp(1, StringToJString('Led-3'));
   end;
{$ENDIF}
end;

procedure TACBrSunmiLeds.SetLed4(const Value: Boolean);
begin
FLed4 := Value;
{$IFDEF ANDROID}
case Value of
   True  : FLeds.controlLamp(0, StringToJString('Led-4'));
   False : FLeds.controlLamp(1, StringToJString('Led-4'));
   end;
{$ENDIF}
end;

procedure TACBrSunmiLeds.SetLed5(const Value: Boolean);
begin
FLed5 := Value;
{$IFDEF ANDROID}
case Value of
   True  : FLeds.controlLamp(0, StringToJString('Led-5'));
   False : FLeds.controlLamp(1, StringToJString('Led-5'));
   end;
{$ENDIF}
end;

procedure TACBrSunmiLeds.SetLed6(const Value: Boolean);
begin
FLed6 := Value;
{$IFDEF ANDROID}
case Value of
   True  : FLeds.controlLamp(0, StringToJString('Led-6'));
   False : FLeds.controlLamp(1, StringToJString('Led-6'));
   end;
{$ENDIF}
end;

end.
