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
  CLibPIXCDVersao = '1.0.0.57';

  CSessaoRespPIXCD = 'Resposta';

  CSessaoPixCDConfig = 'PIXCD';
  CChaveAmbiente = 'Ambiente';
  CChaveArqLogPixCD = 'ArqLog';
  CChaveCNPJSoftwareHouse = 'CNPJSoftwareHouse';
  CChaveNomeAplicacao = 'NomeAplicacao';
  CChaveNomeSoftwareHouse = 'NomeSoftwareHouse';
  CChaveVersaoAplicacao = 'VersaoAplicacao';
  CChaveNivelLog = 'NivelLog';
  CChaveProxyHost = 'ProxyHost';
  CChaveProxyPass = 'ProxyPass';
  CChaveProxyPort = 'ProxyPort';
  CChaveProxyUser = 'ProxyUser';
  CChaveCEPRecebedor = 'CEPRecebedor';
  CChaveCidadeRecebedor = 'CidadeRecebedor';
  CChaveCodCategoriaComerciante = 'ChaveCategoriaComerciante';
  CChaveNomeRecebedor = 'NomeRecebedor';
  CChaveUFRecebedor = 'UFRecebedor';
  CChaveTimeOut = 'TimeOut';
  CChavePSP = 'PSP';
  CChaveTipoChave = 'TipoChave';

  CSessaoPixCDBradescoConfig = 'Bradesco';
  CChavePIXBradesco = 'ChavePIX';
  CChaveClientIDBradesco = 'ClientID';
  CChaveClientSecretBradesco = 'ClientSecret';
  CChaveArqPFXBradesco = 'ArqPFX';
  CChaveSenhaPFXBradesco = 'SenhaPFX';

  CSessaoPixCDSicrediConfig = 'Sicredi';
  CChavePIXSicredi = 'ChavePIX';
  CChaveClientIDSicredi = 'ClientID';
  CChaveClientSecretSicredi = 'ClientSecret';
  CChaveArqChavePrivadaSicredi = 'ArqChavePrivada';
  CChaveArqCertificadoSicredi = 'ArqCertificado';

  CSessaoPixCDSicoobConfig = 'Sicoob';
  CChavePIXSicoob = 'ChavePIX';
  CChaveClientIDSicoob = 'ClientID';
  CChaveArqChavePrivadaSicoob = 'ArqChavePrivada';
  CChaveArqCertificadoSicoob = 'ArqCertificado';

  CSessaoPixCDShipayConfig = 'Shipay';
  CChaveClientIDShipay = 'ClientID';
  CChaveSecretKeyShipay = 'SecretKey';
  CChaveAccessKeyShipay = 'AccessKey';

  CSessaoPixCDSantanderConfig = 'Santander';
  CChavePIXSantander = 'ChavePIX';
  CChaveConsumerKeySantander = 'ConsumerKey';
  CChaveConsumerSecretSantander = 'ConsumerSecret';
  CChaveArqCertificadoPFXSantander = 'ArqCertificadoPFX';
  CChaveSenhaCertificadoPFXSantander = 'SenhaCertificadoPFX';

  CSessaoPixCDPixPDVConfig = 'PixPDV';
  CChaveCNPJPixPDV = 'CNPJ';
  CChaveToken = 'Token';
  CChaveSecretKeyPixPDV = 'SecretKey';

  CSessaoPixCDPagSeguroConfig = 'PagSeguro';
  CChavePIXPagSeguro = 'ChavePIX';
  CChaveClientIDPagSeguro = 'ClientID';
  CChaveClientSecretPagSeguro = 'ClientSecret';
  CChaveArqChavePrivadaPagSeguro = 'ArqChavePrivada';
  CChaveArqCertificadoPagSeguro = 'ArqCertificado';

  CSessaoPixCDItauConfig = 'Itau';
  CChavePIXItau = 'ChavePIX';
  CChaveClientIDItau = 'ClientID';
  CChaveClientSecretItau = 'ClientSecret';
  CChaveArqChavePrivadaItau = 'ArqChavePrivada';
  CChaveArqCertificadoItau = 'ArqCertificado';

  CSessaoPixCDInterConfig = 'Inter';
  CChavePIXInter = 'ChavePIX';
  CChaveClientIDInter = 'ClientID';
  CChaveClientSecretInter = 'ClientSecret';
  CChaveArqChavePrivadaInter = 'ArqChavePrivada';
  CChaveArqCertificadoInter = 'ArqCertificado';

  CSessaoPixCDGerenciaNetConfig = 'GerenciaNet';
  CChavePIXGerenciaNet = 'ChavePIX';
  CChaveClientIDGerenciaNet = 'ClientID';
  CChaveClientSecretGerenciaNet = 'ClientSecret';
  CChaveArqPFXGerenciaNet = 'ArqPFX';

  CSessaoPixCDBancoBrasilConfig = 'BancoBrasil';
  CChavePIXBancoBrasil = 'ChavePIX';
  CChaveClientIDBancoBrasil = 'ClientID';
  CChaveClientSecretBancoBrasil = 'ClientSecret';
  CChaveDeveloperApplicationKeyBancoBrasil = 'DeveloperApplicationKey';
  CChaveArqChavePrivadaBancoBrasil = 'ArqChavePrivada';
  CChaveArqCertificadoBancoBrasil = 'ArqCertificado';
  CChaveArqPFXBancoBrasil = 'ArqPFX';
  CChaveSenhaPFXBancoBrasil = 'SenhaPFX';
  CChaveVersaoAPIBancoBrasil = 'VersaoAPI';
  CChaveTipoCertificadoBancoBrasil = 'TipoCertificado';

  CSessaoPixCDAilosConfig = 'Ailos';
  CChavePIXAilos = 'ChavePIX';
  CChaveClientIDAilos = 'ClientID';
  CChaveClientSecretAilos = 'ClientSecret';
  CChaveArqChavePrivadaAilos = 'ArqChavePrivada';
  CChaveArqCertificadoAilos = 'ArqCertificado';
  CChaveArqCertificadoRootAilos = 'ArqCertificadoRoot';

  CSessaoPixCDMateraConfig = 'Matera';
  CChaveClientIDMatera = 'ClientID';
  CChaveSecretKeyMatera = 'SecretKey';
  CChaveClientSecretMatera = 'ClientSecret';
  CChaveArqCertificadoMatera = 'ArqCertificado';
  CChaveArqChavePrivadaMatera = 'ArqChavePrivada';
  CChaveAccountIDMatera = 'AccountID';
  CChavePIXMatera = 'ChavePIX';
  CChaveMediatorFeeMatera = 'MediatorFee';

  CSessaoPixCDCieloConfig = 'Cielo';
  CChavePIXCielo = 'ChavePIX';
  CChaveClientIDCielo = 'ClientID';
  CChaveClientSecretCielo = 'ClientSecret';

  CSessaoPixCDMercadoPagoConfig = 'MercadoPago';
  CChaveAccesTokenMercadoPago = 'AccesToken';

implementation

end.

