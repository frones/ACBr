/* {******************************************************************************}
// { Projeto: Componentes ACBr                                                    }
// {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
// { mentos de Automação Comercial utilizados no Brasil                           }
// {                                                                              }
// { Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
// {                                                                              }
// { Colaboradores nesse arquivo: Renato Rubinho                                  }
// {                                                                              }
// {  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
// { Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
// {                                                                              }
// {  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
// { sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
// { Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
// { qualquer versão posterior.                                                   }
// {                                                                              }
// {  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
// { NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÝFICA. Consulte a Licença Pública Geral Menor}
// { do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
// {                                                                              }
// {  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
// { com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
// { no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
// { Você também pode obter uma copia da licença em:                              }
// { http://www.opensource.org/licenses/lgpl-license.php                          }
// {                                                                              }
// { Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
// {       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
// {******************************************************************************}
*/

int Reinf_Inicializar(const uintptr_t* libHandle, const char* eArqConfig, const char* eChaveCrypt);
int Reinf_ConfigLer(const uintptr_t libHandle, const char* eArqConfig);
int Reinf_ConfigLerValor (const uintptr_t libHandle, const char* eSessao, const char* eChave, char* sValor, long* esTamanho);
int Reinf_ConfigGravar(const uintptr_t libHandle, const char* eArqConfig);
int Reinf_ConfigGravarValor(const uintptr_t libHandle, const char* eSessao, const char* eChave, const char* eValor);
int Reinf_UltimoRetorno(const uintptr_t libHandle, char* sMensagem, long* esTamanho);
int Reinf_Finalizar(const uintptr_t libHandle);

int Reinf_Nome(const uintptr_t libHandle, const char* sNome, long* esTamanho);
int Reinf_Versao(const uintptr_t libHandle, const char* sVersao, long* esTamanho);
int Reinf_OpenSSLInfo(const uintptr_t libHandle, const char* sOpenSSLInfo, long* esTamanho);
int Reinf_ConfigImportar(const uintptr_t libHandle, const char* eArqConfig);
int Reinf_ConfigExportar(const uintptr_t libHandle, const char* sMensagem, long* esTamanho);

int Reinf_CriarEventoReinf(const uintptr_t libHandle, const char* eArqIni);
int Reinf_EnviarReinf(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int Reinf_ConsultarReinf(const uintptr_t libHandle, const char* eProtocolo, const char* sResposta, long* esTamanho);
int Reinf_ConsultarReciboReinf(const uintptr_t libHandle, const char* ePerApur, long aTipoEvento, const char* eNrInscEstab, 
    const char* eCnpjPrestador, const char* eNrInscTomador, const char* eDtApur, const char* eCpfCnpjBenef, 
    const char* eCnpjFonte, const char* sResposta, long* esTamanho);
int Reinf_CriarEnviarReinf(const uintptr_t libHandle, const char* eArqIni, const char* sResposta, long* esTamanho);
int Reinf_LimparReinf(const uintptr_t libHandle);
int Reinf_CarregarXMLEventoReinf (const uintptr_t libHandle, const char* eArquivoOuXML);
int Reinf_SetIDContribuinte (const uintptr_t libHandle, const char* aIdContribuinte);
int Reinf_SetIDTransmissor (const uintptr_t libHandle, const char* aIdTransmissor);
int Reinf_SetTipoContribuinte (const uintptr_t libHandle, long aTipoContribuinte);
int Reinf_SetVersaoDF (const uintptr_t libHandle, const char* sVersao);
int Reinf_ObterCertificados (const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int Reinf_Validar(const uintptr_t libHandle);
