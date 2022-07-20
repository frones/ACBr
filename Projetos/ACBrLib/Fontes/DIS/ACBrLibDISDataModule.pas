{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrLibDISDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil,
  ACBrLibDataModule, ACBrDIS;

type

  { TLibDISDM }

  TLibDISDM = class(TLibDataModule)
    ACBrDIS1: TACBrDIS;
  public
    procedure AplicarConfiguracoes; override;

  end;

implementation

uses
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrDeviceSerial,
  ACBrLibDISConfig, ACBrLibComum, ACBrLibDISBase;

{$R *.lfm}

{ TLibDISDM }
procedure TLibDISDM.AplicarConfiguracoes;
var
  pLibConfig: TLibDISConfig;
begin
  pLibConfig := TLibDISConfig(TACBrLibDIS(Lib).Config);

  with ACBrDIS1 do
  begin
    Porta               := pLibConfig.DISConfig.Porta;
    Modelo              := pLibConfig.DISConfig.Modelo;
    Alinhamento         := pLibConfig.DISConfig.Alinhamento;
    LinhasCount         := pLibConfig.DISConfig.LinhasCount;
    Colunas             := pLibConfig.DISConfig.Colunas;
    Intervalo           := pLibConfig.DISConfig.Intervalo;
    Passos              := pLibConfig.DISConfig.Passos;
    IntervaloEnvioBytes := pLibConfig.DISConfig.IntervaloEnvioBytes;
    RemoveAcentos       := pLibConfig.DISConfig.RemoveAcentos;

    Device.Baud := pLibConfig.DeviceConfig.Baud;
    Device.Data := pLibConfig.DeviceConfig.Data;
    Device.TimeOut := pLibConfig.DeviceConfig.TimeOut;
    Device.Parity := TACBrSerialParity(pLibConfig.DeviceConfig.Parity);
    Device.Stop := TACBrSerialStop(pLibConfig.DeviceConfig.Stop);
    Device.MaxBandwidth := pLibConfig.DeviceConfig.MaxBandwidth;
    Device.SendBytesCount := pLibConfig.DeviceConfig.SendBytesCount;
    Device.SendBytesInterval := pLibConfig.DeviceConfig.SendBytesInterval;
    Device.HandShake := TACBrHandShake(pLibConfig.DeviceConfig.HandShake);
    Device.HardFlow := pLibConfig.DeviceConfig.HardFlow;
    Device.SoftFlow := pLibConfig.DeviceConfig.SoftFlow;
  end;
end;

end.

