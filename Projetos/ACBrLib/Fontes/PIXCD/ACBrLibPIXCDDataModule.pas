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
  Classes, SysUtils, FileUtil, ACBrLibComum, ACBrLibDataModule, ACBrPIXCD,
  ACBrPIXPSPBradesco, ACBrPIXPSPItau, ACBrPIXPSPBancoDoBrasil,
  ACBrPIXPSPSantander, ACBrPIXPSPShipay, ACBrPIXPSPSicredi, ACBrPIXPSPSicoob,
  ACBrPIXPSPPagSeguro, ACBrPIXPSPGerenciaNet, ACBrPIXPSPPixPDV, ACBrPIXPSPInter,
  ACBrPIXPSPAilos, ACBrPIXPSPMatera, ACBrPIXPSPCielo, ACBrPIXPSPMercadoPago,
  ACBrPIXPSPBanrisul, ACBrPIXPSPGate2All, ACBrPIXPSPC6Bank;

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
                 Matera,
                 Cielo,
                 MercadoPago,
                 Gate2All,
                 Banrisul,
                 C6Bank);

  { TLibPIXCDDM }

  TLibPIXCDDM = class(TLibDataModule)
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPBanrisul1: TACBrPSPBanrisul;
    ACBrPSPBradesco1: TACBrPSPBradesco;
    ACBrPSPAilos1: TACBrPSPAilos;
    ACBrPSPBancoDoBrasil1: TACBrPSPBancoDoBrasil;
    ACBrPSPC6Bank1: TACBrPSPC6Bank;
    ACBrPSPCielo1: TACBrPSPCielo;
    ACBrPSPGate2All1: TACBrPSPGate2All;
    ACBrPSPGerenciaNet1: TACBrPSPGerenciaNet;
    ACBrPSPInter1: TACBrPSPInter;
    ACBrPSPItau1: TACBrPSPItau;
    ACBrPSPMatera1: TACBrPSPMatera;
    ACBrPSPMercadoPago1: TACBrPSPMercadoPago;
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
  ACBrLibPIXCDConfig;

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
      Cielo: ACBrPixCD1.PSP := ACBrPSPCielo1;
      MercadoPago: ACBrPixCD1.PSP := ACBrPSPMercadoPago1;
      Gate2All: ACBrPixCD1.PSP := ACBrPSPGate2All1;
      Banrisul: ACBrPixCD1.PSP := ACBrPSPBanrisul1;
      C6Bank: ACBrPixCD1.PSP := ACBrPSPC6Bank1;
    end;

    with ACBrPixCD1 do
    begin
      Ambiente := pLibPIXCDConfig.PIXCDConfig.Ambiente;
      ArqLOG   := pLibPIXCDConfig.PIXCDConfig.ArqLog;
      NivelLog := pLibPIXCDConfig.PIXCDConfig.NivelLog;
      TimeOut  := pLibPIXCDConfig.PIXCDConfig.TimeOut;
    end;

    with ACBrPixCD1.PSP do
    begin
      TipoChave := pLibPIXCDConfig.PIXCDConfig.TipoChave;
    end;

    with ACBrPixCD1.DadosAutomacao do
    begin
      CNPJSoftwareHouse := pLibPIXCDConfig.PIXCDConfig.DadosAutomacao.CNPJSoftwareHouse;
      NomeAplicacao     := pLibPIXCDConfig.PIXCDConfig.DadosAutomacao.NomeAplicacao;
      NomeSoftwareHouse := pLibPIXCDConfig.PIXCDConfig.DadosAutomacao.NomeSoftwareHouse;
      VersaoAplicacao   := pLibPIXCDConfig.PIXCDConfig.DadosAutomacao.VersaoAplicacao;
    end;

    with ACBrPixCD1.Proxy do
    begin
      Host := pLibPIXCDConfig.PIXCDConfig.Proxy.Host;
      Pass := pLibPIXCDConfig.PIXCDConfig.Proxy.Pass;
      Port := pLibPIXCDConfig.PIXCDConfig.Proxy.Port;
      User := pLibPIXCDConfig.PIXCDConfig.Proxy.User;
    end;

    with ACBrPixCD1.Recebedor do
    begin
      CodCategoriaComerciante := pLibPIXCDConfig.PIXCDConfig.Recebedor.CodCategoriaComerciante;
      CEP                     := pLibPIXCDConfig.PIXCDConfig.Recebedor.CEP;
      Cidade                  := pLibPIXCDConfig.PIXCDConfig.Recebedor.Cidade;
      Nome                    := pLibPIXCDConfig.PIXCDConfig.Recebedor.Nome;
      UF                      := pLibPIXCDConfig.PIXCDConfig.Recebedor.UF;
    end;

    with ACBrPSPBradesco1 do
    begin
      ChavePIX     := pLibPIXCDConfig.PIXCDBradesco.ChavePIX;
      ClientID     := pLibPIXCDConfig.PIXCDBradesco.ClientID;
      ClientSecret := pLibPIXCDConfig.PIXCDBradesco.ClientSecret;
      ArquivoPFX   := pLibPIXCDConfig.PIXCDBradesco.ArqPFX;
      SenhaPFX     := pLibPIXCDConfig.PIXCDBradesco.SenhaPFX;
      Scopes       := pLibPIXCDConfig.PIXCDBradesco.Scopes;
    end;

    with ACBrPSPSicredi1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDSicredi.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDSicredi.ClientID;
      ClientSecret        := pLibPIXCDConfig.PIXCDSicredi.ClientSecret;
      ArquivoChavePrivada := pLibPIXCDConfig.PIXCDSicredi.ArqChavePrivada;
      ArquivoCertificado  := pLibPIXCDConfig.PIXCDSicredi.ArqCertificado;
      APIVersion          := pLibPIXCDConfig.PIXCDSicredi.APIVersion;
      Scopes              := pLibPIXCDConfig.PIXCDSicredi.Scopes;
    end;

    with ACBrPSPSicoob1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDSiccob.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDSiccob.ClientID;
      TokenSandbox        := pLibPIXCDConfig.PIXCDSiccob.TokenSandbox;
      ArquivoChavePrivada := pLibPIXCDConfig.PIXCDSiccob.ArqChavePrivada;
      ArquivoCertificado  := pLibPIXCDConfig.PIXCDSiccob.ArqCertificado;
      APIVersion          := pLibPIXCDConfig.PIXCDSiccob.APIVersion;
      Scopes              := pLibPIXCDConfig.PIXCDSiccob.Scopes;
    end;

    with ACBrPSPShipay1 do
    begin
      ClientID  := pLibPIXCDConfig.PIXCDShipay.ClientID;
      SecretKey := pLibPIXCDConfig.PIXCDShipay.SecretKey;
      AccessKey := pLibPIXCDConfig.PIXCDShipay.AccessKey;
      Scopes    := pLibPIXCDConfig.PIXCDShipay.Scopes;
    end;

    with ACBrPSPSantander1 do
    begin
      ChavePIX           := pLibPIXCDConfig.PIXCDSantander.ChavePIX;
      ConsumerKey        := pLibPIXCDConfig.PIXCDSantander.ConsumerKey;
      ConsumerSecret     := pLibPIXCDConfig.PIXCDSantander.ConsumerSecret;
      ArquivoCertificado := pLibPIXCDConfig.PIXCDSantander.ArqCertificadoPFX;
      SenhaPFX           := pLibPIXCDConfig.PIXCDSantander.SenhaCertificadoPFX;
      APIVersion         := pLibPIXCDConfig.PIXCDSantander.APIVersion;
      Scopes             := pLibPIXCDConfig.PIXCDSantander.Scopes;
    end;

    with ACBrPSPPixPDV1 do
    begin
      CNPJ  := pLibPIXCDConfig.PIXCDPixPDV.CNPJ;
      Token := pLibPIXCDConfig.PIXCDPixPDV.Token;
      Scopes:= pLibPIXCDConfig.PIXCDPixPDV.Scopes;
    end;

    with ACBrPSPPagSeguro1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDPagSeguro.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDPagSeguro.ClientID;
      ClientSecret        := pLibPIXCDConfig.PIXCDPagSeguro.ClientSecret;
      ArquivoChavePrivada := pLibPIXCDConfig.PIXCDPagSeguro.ArqChavePrivada;
      ArquivoCertificado  := pLibPIXCDConfig.PIXCDPagSeguro.ArqCertificado;
      Scopes              := pLibPIXCDConfig.PIXCDPagSeguro.Scopes;
    end;

    with ACBrPSPItau1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDItau.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDItau.ClientID;
      ClientSecret        := pLibPIXCDConfig.PIXCDItau.ClientSecret;
      ArquivoChavePrivada := pLibPIXCDConfig.PIXCDItau.ArqChavePrivada;
      ArquivoCertificado  := pLibPIXCDConfig.PIXCDItau.ArqCertificado;
      APIVersion          := pLibPIXCDConfig.PIXCDItau.APIVersion;
      Scopes              := pLibPIXCDConfig.PIXCDItau.Scopes;
    end;

    with ACBrPSPInter1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDInter.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDInter.ClientID;
      ClientSecret        := pLibPIXCDConfig.PIXCDInter.ClientSecret;
      ArquivoChavePrivada := pLibPIXCDConfig.PIXCDInter.ArqChavePrivada;
      ArquivoCertificado  := pLibPIXCDConfig.PIXCDInter.ArqCertificado;
      Scopes              := pLibPIXCDConfig.PIXCDInter.Scopes;
    end;

    with ACBrPSPGerenciaNet1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDGerenciaNet.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDGerenciaNet.ClientID;
      ClientSecret        := pLibPIXCDConfig.PIXCDGerenciaNet.ClientSecret;
      ArquivoPFX          := pLibPIXCDConfig.PIXCDGerenciaNet.ArqPFX;
      Scopes              := pLibPIXCDConfig.PIXCDGerenciaNet.Scopes;
    end;

    with ACBrPSPBancoDoBrasil1 do
    begin
      ChavePIX                := pLibPIXCDConfig.PIXCDBancoDoBrasil.ChavePIX;
      ClientID                := pLibPIXCDConfig.PIXCDBancoDoBrasil.ClientID;
      ClientSecret            := pLibPIXCDConfig.PIXCDBancoDoBrasil.ClientSecret;
      DeveloperApplicationKey := pLibPIXCDConfig.PIXCDBancoDoBrasil.DeveloperApplicationKey;
      ArquivoChavePrivada     := pLibPIXCDConfig.PIXCDBancoDoBrasil.ArqChavePrivada;
      ArquivoCertificado      := pLibPIXCDConfig.PIXCDBancoDoBrasil.ArqCertificado;
      ArquivoPFX              := pLibPIXCDConfig.PIXCDBancoDoBrasil.ArqPFX;
      SenhaPFX                := pLibPIXCDConfig.PIXCDBancoDoBrasil.SenhaPFX;
      BBAPIVersao             := pLibPIXCDConfig.PIXCDBancoDoBrasil.BBAPIVersao;
      APIVersion              := pLibPIXCDConfig.PIXCDBancoDoBrasil.APIVersion;
      Scopes                  := pLibPIXCDConfig.PIXCDBancoDoBrasil.Scopes;
    end;

    with ACBrPSPAilos1 do
    begin
      ChavePIX                := pLibPIXCDConfig.PIXCDAilos.ChavePIX;
      ClientID                := pLibPIXCDConfig.PIXCDAilos.ClientID;
      ClientSecret            := pLibPIXCDConfig.PIXCDAilos.ClientSecret;
      ArquivoChavePrivada     := pLibPIXCDConfig.PIXCDAilos.ArqChavePrivada;
      ArquivoCertificado      := pLibPIXCDConfig.PIXCDAilos.ArqCertificado;
      RootCrt                 := pLibPIXCDConfig.PIXCDAilos.ArqCertificadoRoot;
      Scopes                  := pLibPIXCDConfig.PIXCDAilos.Scopes;
    end;

    with ACBrPSPMatera1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDMatera.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDMatera.ClientID;
      SecretKey           := pLibPIXCDConfig.PIXCDMatera.SecretKey;
      ClientSecret        := pLibPIXCDConfig.PIXCDMatera.ClientSecret;
      ArquivoCertificado  := pLibPIXCDConfig.PIXCDMatera.ArqCertificado;
      ArquivoChavePrivada := pLibPIXCDConfig.PIXCDMatera.ArqChavePrivada;
      AccountId           := pLibPIXCDConfig.PIXCDMatera.AccountID;
      MediatorFee         := pLibPIXCDConfig.PIXCDMatera.MediatorFee;
      Scopes              := pLibPIXCDConfig.PIXCDMatera.Scopes;
    end;

    with ACBrPSPCielo1 do
    begin
      ChavePIX     := pLibPIXCDConfig.PIXCDCielo.ChavePIX;
      ClientID     := pLibPIXCDConfig.PIXCDCielo.ClientID;
      ClientSecret := pLibPIXCDConfig.PIXCDCielo.ClientSecret;
      Scopes       := pLibPIXCDConfig.PIXCDCielo.Scopes;
    end;

    with ACBrPSPMercadoPago1 do
    begin
      ChavePIX    := pLibPIXCDConfig.PIXCDMercadoPago.ChavePIX;
      AccessToken := pLibPIXCDConfig.PIXCDMercadoPago.AccessToken;
      Scopes      := pLibPIXCDConfig.PIXCDMercadoPago.Scopes;
    end;

    with ACBrPSPGate2All1 do
    begin
      AuthenticationApi := pLibPIXCDConfig.PIXCDGate2All.AuthenticationApi;
      AuthenticationKey := pLibPIXCDConfig.PIXCDGate2All.AuthenticationKey;
    end;

    with ACBrPSPBanrisul1 do
    begin
      ChavePIX           := pLibPIXCDConfig.PIXCDBanrisul.ChavePIX;
      ClientID           := pLibPIXCDConfig.PIXCDBanrisul.ClientID;
      ClientSecret       := pLibPIXCDConfig.PIXCDBanrisul.ClientSecret;
      ArquivoCertificado := pLibPIXCDConfig.PIXCDBanrisul.ArquivoCertificado;
      SenhaPFX           := pLibPIXCDConfig.PIXCDBanrisul.SenhaPFX;
    end;

    with ACBrPSPC6Bank1 do
    begin
      ChavePIX            := pLibPIXCDConfig.PIXCDC6Bank.ChavePIX;
      ClientID            := pLibPIXCDConfig.PIXCDC6Bank.ClientID;
      ClientSecret        := pLibPIXCDConfig.PIXCDC6Bank.ClientSecret;
      ArquivoChavePrivada := pLibPIXCDConfig.PIXCDC6Bank.ArqChavePrivada;
      ArquivoCertificado  := pLibPIXCDConfig.PIXCDC6Bank.ArqCertificado;
    end;

    {$IFDEF Demo}
    ACBrPixCD1.Ambiente := ambTeste;
    {$ENDIF}
end;

end.

