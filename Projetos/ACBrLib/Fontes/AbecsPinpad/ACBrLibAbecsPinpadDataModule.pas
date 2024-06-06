{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibAbecsPinpadDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, ACBrLibComum, ACBrLibConfig, syncobjs, ACBrAbecsPinPad, ACBrUtil.FilesIO;

type

  { TLibAbecsPinpadDM }

  TLibAbecsPinpadDM = class(TDataModule)
    ACBrAbecsPinPad1: TACBrAbecsPinPad;
    fpLib: TACBrLib;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;

  public
    procedure AplicarConfiguracoes;
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;

    property Lib: TACBrLib read fpLib write fpLib;

  end;

implementation

uses
  ACBrLibAbecsPinpadConfig, ACBrDeviceSerial;

{$R *.lfm}

{ TLibAbecsPinpadDM }

procedure TLibAbecsPinpadDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibAbecsPinpadDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibAbecsPinpadDM.AplicarConfiguracoes;
var
  pLibConfig: TLibAbecsPinpadConfig;
begin
  pLibConfig := TLibAbecsPinpadConfig(Lib.Config);

  with ACBrAbecsPinPad1 do
  begin
    Port := pLibConfig.AbecsPinpadConfig.Port;
    TimeOut := pLibConfig.AbecsPinpadConfig.TimeOut;
    LogFile := pLibConfig.AbecsPinpadConfig.LogFile;
    MsgAlign := pLibConfig.AbecsPinpadConfig.MsgAlign;
    MsgWordWrap := pLibConfig.AbecsPinpadConfig.MsgWordWrap;

    Device.Baud := pLibConfig.AbecsPinpadDeviceConfig.Baud;
    Device.Data := pLibConfig.AbecsPinpadDeviceConfig.Data;
    Device.TimeOut := pLibConfig.AbecsPinpadDeviceConfig.TimeOut;
    Device.Parity := TACBrSerialParity(pLibConfig.AbecsPinpadDeviceConfig.Parity);
    Device.Stop := TACBrSerialStop(pLibConfig.AbecsPinpadDeviceConfig.Stop);
    Device.MaxBandwidth := pLibConfig.AbecsPinpadDeviceConfig.MaxBandwidth;
    Device.SendBytesCount := pLibConfig.AbecsPinpadDeviceConfig.SendBytesCount;
    Device.SendBytesInterval := pLibConfig.AbecsPinpadDeviceConfig.SendBytesInterval;
    Device.HandShake := TACBrHandShake(pLibConfig.AbecsPinpadDeviceConfig.HandShake);
    Device.HardFlow := pLibConfig.AbecsPinpadDeviceConfig.HardFlow;
    Device.SoftFlow := pLibConfig.AbecsPinpadDeviceConfig.SoftFlow;
  end;
end;

procedure TLibAbecsPinpadDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(Lib) then
    Lib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibAbecsPinpadDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibAbecsPinpadDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.

