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

unit ACBrLibPIXCDDataModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ACBrLibComum, ACBrLibConfig, syncobjs,
  ACBrLibDataModule, ACBrPIXCD, ACBrPIXPSPBradesco, ACBrPIXPSPItau,
  ACBrPIXPSPBancoDoBrasil, ACBrPIXPSPSantander, ACBrPIXPSPShipay,
  ACBrPIXPSPSicredi, ACBrPIXPSPSicoob, ACBrPIXPSPPagSeguro,
  ACBrPIXPSPGerenciaNet, ACBrPIXPSPPixPDV, ACBrPIXPSPInter, ACBrPIXPSPAilos,
  ACBrPIXPSPMatera;

type

  TACBrPIXPSP = (Bradesco,
                 Itau,
                 BancoDoBrasil,
                 Santander,
                 Shipay,
                 Sicredi,
                 Sicoob,
                 PagSeguro,
                 GerenciaNet,
                 PixPDV,
                 Inter,
                 Ailos,
                 Matera);

  { TLibPIXCDDM }

  TLibPIXCDDM = class(TLibDataModule)
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPBradesco1: TACBrPSPBradesco;
    ACBrPSPAilos1: TACBrPSPAilos;
    ACBrPSPBancoDoBrasil1: TACBrPSPBancoDoBrasil;
    ACBrPSPGerenciaNet1: TACBrPSPGerenciaNet;
    ACBrPSPInter1: TACBrPSPInter;
    ACBrPSPItau1: TACBrPSPItau;
    ACBrPSPMatera1: TACBrPSPMatera;
    ACBrPSPPagSeguro1: TACBrPSPPagSeguro;
    ACBrPSPPixPDV1: TACBrPSPPixPDV;
    ACBrPSPSantander1: TACBrPSPSantander;
    ACBrPSPShipay1: TACBrPSPShipay;
    ACBrPSPSicoob1: TACBrPSPSicoob;
    ACBrPSPSicredi1: TACBrPSPSicredi;

  public
    procedure AplicarConfiguracoes; override;
  end;

implementation

uses
  ACBrLibPIXCDConfig, ACBrLibPIXCDBase;

{$R *.lfm}

{ TLibPIXCDDM }

procedure TLibPIXCDDM.AplicarConfiguracoes;
var
  pLibPIXCDConfig: TLibPIXCDConfig;
begin
    pLibPIXCDConfig := TLibPIXCDConfig(Lib.Config);

    case TACBrPIXPSP(pLibPIXCDConfig.PIXCDConfig.PSP) of
      Bradesco: ACBrPixCD1.PSP := ACBrPSPBradesco1;
      Itau: ACBrPixCD1.PSP := ACBrPSPItau1;
      BancoDoBrasil: ACBrPixCD1.PSP := ACBrPSPBancoDoBrasil1;
      Santander: ACBrPixCD1.PSP := ACBrPSPSantander1;
      Shipay: ACBrPixCD1.PSP := ACBrPSPShipay1;
      Sicredi: ACBrPixCD1.PSP := ACBrPSPSicredi1;
      Sicoob: ACBrPixCD1.PSP := ACBrPSPSicoob1;
      PagSeguro: ACBrPixCD1.PSP := ACBrPSPPagSeguro1;
      GerenciaNet: ACBrPixCD1.PSP := ACBrPSPGerenciaNet1;
      PixPDV: ACBrPixCD1.PSP := ACBrPSPPixPDV1;
      Inter: ACBrPixCD1.PSP := ACBrPSPInter1;
      Ailos: ACBrPixCD1.PSP := ACBrPSPAilos1;
      Matera: ACBrPixCD1.PSP := ACBrPSPMatera1;
    end;
end;

end.

