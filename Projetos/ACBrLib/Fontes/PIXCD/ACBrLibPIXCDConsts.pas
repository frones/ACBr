{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                            }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibPIXCDConsts;

interface

uses
  Classes, SysUtils;

const
  CLibPIXCDNome = 'ACBrLibPIXCD';
  CLibPIXCDVersao = '1.0.0.73';

  CSessaoRespPIXCD = 'Resposta';

  CSessaoPIXCDConfig = 'PIXCD';
  CChaveAmbiente = 'Ambiente';
  CChavePSP = 'PSP';
  CChaveTipoChave = 'TipoChave';
  CChaveTimeOut = 'TimeOut';
  CChaveNivelLog = 'NivelLog';
  CChaveArqLogPixCD = 'ArqLog';
  CChaveCNPJSoftwareHouse = 'CNPJSoftwareHouse';
  CChaveNomeAplicacao = 'NomeAplicacao';
  CChaveNomeSoftwareHouse = 'NomeSoftwareHouse';
  CChaveVersaoAplicacao = 'VersaoAplicacao';
  CChaveProxyHost = 'ProxyHost';
  CChaveProxyPass = 'ProxyPass';
  CChaveProxyPort = 'ProxyPort';
  CChaveProxyUser = 'ProxyUser';
  CChaveCodCategoriaComerciante = 'ChaveCategoriaComerciante';
  CChaveCEPRecebedor = 'CEPRecebedor';
  CChaveCidadeRecebedor = 'CidadeRecebedor';
  CChaveNomeRecebedor = 'NomeRecebedor';
  CChaveUFRecebedor = 'UFRecebedor';

  CSessaoPIXCDBradescoConfig = 'Bradesco';
  CChavePIXBradesco = 'ChavePIX';
  CChaveClientIDBradesco = 'ClientID';
  CChaveClientSecretBradesco = 'ClientSecret';
  CChaveArqPFXBradesco = 'ArqPFX';
  CChaveSenhaPFXBradesco = 'SenhaPFX';

  CSessaoPIXCDSicrediConfig = 'Sicredi';
  CChavePIXSicredi = 'ChavePIX';
  CChaveClientIDSicredi = 'ClientID';
  CChaveClientSecretSicredi = 'ClientSecret';
  CChaveArqChavePrivadaSicredi = 'ArqChavePrivada';
  CChaveArqCertificadoSicredi = 'ArqCertificado';
  CChaveAPIVersionSicredi = 'APIVersion';

  CSessaoPIXCDSicoobConfig = 'Sicoob';
  CChavePIXSicoob = 'ChavePIX';
  CChaveClientIDSicoob = 'ClientID';
  CChaveTokenSandboxSicoob = 'TokenSandbox';
  CChaveArqChavePrivadaSicoob = 'ArqChavePrivada';
  CChaveArqCertificadoSicoob = 'ArqCertificado';
  CChaveAPIVersionSicoob = 'APIVersion';

  CSessaoPIXCDShipayConfig = 'Shipay';
  CChaveClientIDShipay = 'ClientID';
  CChaveSecretKeyShipay = 'SecretKey';
  CChaveAccessKeyShipay = 'AccessKey';

  CSessaoPIXCDSantanderConfig = 'Santander';
  CChavePIXSantander = 'ChavePIX';
  CChaveConsumerKeySantander = 'ConsumerKey';
  CChaveConsumerSecretSantander = 'ConsumerSecret';
  CChaveArqCertificadoPFXSantander = 'ArqCertificadoPFX';
  CChaveSenhaCertificadoPFXSantander = 'SenhaCertificadoPFX';
  CChaveAPIVersionSantander = 'APIVersion';

  CSessaoPIXCDPixPDVConfig = 'PixPDV';
  CChaveCNPJPixPDV = 'CNPJ';
  CChaveToken = 'Token';
  CChaveSecretKeyPixPDV = 'SecretKey';

  CSessaoPIXCDPagSeguroConfig = 'PagSeguro';
  CChavePIXPagSeguro = 'ChavePIX';
  CChaveClientIDPagSeguro = 'ClientID';
  CChaveClientSecretPagSeguro = 'ClientSecret';
  CChaveArqChavePrivadaPagSeguro = 'ArqChavePrivada';
  CChaveArqCertificadoPagSeguro = 'ArqCertificado';

  CSessaoPIXCDItauConfig = 'Itau';
  CChavePIXItau = 'ChavePIX';
  CChaveClientIDItau = 'ClientID';
  CChaveClientSecretItau = 'ClientSecret';
  CChaveArqChavePrivadaItau = 'ArqChavePrivada';
  CChaveArqCertificadoItau = 'ArqCertificado';
  CChaveAPIVersionItau = 'APIVersion';

  CSessaoPIXCDInterConfig = 'Inter';
  CChavePIXInter = 'ChavePIX';
  CChaveClientIDInter = 'ClientID';
  CChaveClientSecretInter = 'ClientSecret';
  CChaveArqChavePrivadaInter = 'ArqChavePrivada';
  CChaveArqCertificadoInter = 'ArqCertificado';

  CSessaoPIXCDGerenciaNetConfig = 'GerenciaNet';
  CChavePIXGerenciaNet = 'ChavePIX';
  CChaveClientIDGerenciaNet = 'ClientID';
  CChaveClientSecretGerenciaNet = 'ClientSecret';
  CChaveArqPFXGerenciaNet = 'ArqPFX';

  CSessaoPIXCDBancoBrasilConfig = 'BancoBrasil';
  CChavePIXBancoBrasil = 'ChavePIX';
  CChaveClientIDBancoBrasil = 'ClientID';
  CChaveClientSecretBancoBrasil = 'ClientSecret';
  CChaveDeveloperApplicationKeyBancoBrasil = 'DeveloperApplicationKey';
  CChaveArqChavePrivadaBancoBrasil = 'ArqChavePrivada';
  CChaveArqCertificadoBancoBrasil = 'ArqCertificado';
  CChaveArqPFXBancoBrasil = 'ArqPFX';
  CChaveSenhaPFXBancoBrasil = 'SenhaPFX';
  CChaveBBAPIVersaoBancoBrasil = 'BBAPIVersao';
  CChaveAPIVersionBancoBrasil = 'APIVersion';

  CSessaoPIXCDAilosConfig = 'Ailos';
  CChavePIXAilos = 'ChavePIX';
  CChaveClientIDAilos = 'ClientID';
  CChaveClientSecretAilos = 'ClientSecret';
  CChaveArqChavePrivadaAilos = 'ArqChavePrivada';
  CChaveArqCertificadoAilos = 'ArqCertificado';
  CChaveArqCertificadoRootAilos = 'ArqCertificadoRoot';

  CSessaoPIXCDMateraConfig = 'Matera';
  CChaveClientIDMatera = 'ClientID';
  CChaveSecretKeyMatera = 'SecretKey';
  CChaveClientSecretMatera = 'ClientSecret';
  CChaveArqCertificadoMatera = 'ArqCertificado';
  CChaveArqChavePrivadaMatera = 'ArqChavePrivada';
  CChaveAccountIDMatera = 'AccountID';
  CChavePIXMatera = 'ChavePIX';
  CChaveMediatorFeeMatera = 'MediatorFee';

  CSessaoPIXCDCieloConfig = 'Cielo';
  CChavePIXCielo = 'ChavePIX';
  CChaveClientIDCielo = 'ClientID';
  CChaveClientSecretCielo = 'ClientSecret';

  CSessaoPIXCDMercadoPagoConfig = 'MercadoPago';
  CChaveAccesTokenMercadoPago = 'AccessToken';

implementation

end.

