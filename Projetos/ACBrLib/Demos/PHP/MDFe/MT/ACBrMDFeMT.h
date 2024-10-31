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

int MDFE_Inicializar(const uintptr_t* libHandle, const char* eArqConfig, const char* eChaveCrypt);
int MDFE_ConfigLer(const uintptr_t libHandle, const char* eArqConfig);
int MDFE_ConfigLerValor (const uintptr_t libHandle, const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int MDFE_ConfigGravarValor(const uintptr_t libHandle, const char* eSessao, const char* eChave, const char* eValor);
int MDFE_UltimoRetorno(const uintptr_t libHandle, char* sMensagem, long* esTamanho);
int MDFE_Finalizar(const uintptr_t libHandle);

int MDFE_Nome(const uintptr_t libHandle, const char* sNome, long* esTamanho);
int MDFE_Versao(const uintptr_t libHandle, const char* sVersao, long* esTamanho);
int MDFE_OpenSSLInfo(const uintptr_t libHandle, const char* sOpenSSLInfo, long* esTamanho);
int MDFE_ConfigImportar(const uintptr_t libHandle, const char* eArqConfig);
int MDFE_ConfigExportar(const uintptr_t libHandle, const char* sMensagem, long* esTamanho);
int MDFE_ConfigGravar(const uintptr_t libHandle, const char* eArqConfig);

int MDFE_StatusServico(const uintptr_t libHandle, char* sMensagem, long* esTamanho);
int MDFE_CarregarXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int MDFE_CarregarINI(const uintptr_t libHandle, const char* eArquivoOuINI);
int MDFE_ObterXml(const uintptr_t libHandle, long AIndex, char* sResposta, long* esTamanho);
int MDFE_GravarXml(const uintptr_t libHandle, long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int MDFE_ObterIni(const uintptr_t libHandle, int AIndex, char* sResposta, long* esTamanho);
int MDFE_GravarIni(const uintptr_t libHandle, int AIndex, char* sResposta, long* esTamanho);
int MDFE_CarregarEventoXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int MDFE_CarregarEventoINI(const uintptr_t libHandle, const char* eArquivoOuINI);
int MDFE_LimparLista(const uintptr_t libHandle);
int MDFE_LimparListaEventos(const uintptr_t libHandle);
int MDFE_Assinar(const uintptr_t libHandle);
int MDFE_Validar(const uintptr_t libHandle);
int MDFE_ValidarRegrasdeNegocios(const uintptr_t libHandle, char* sResposta, long* esTamanho);
int MDFE_VerificarAssinatura(const uintptr_t libHandle, char* sResposta, long* esTamanho);

int MDFE_Consultar(const uintptr_t libHandle, const char* eChaveOuMDFe, int AExtrairEventos, char* sResposta, long* esTamanho);
int MDFE_Enviar(const uintptr_t libHandle, long ALote, char* Imprimir, char* Sincrono, char* sResposta, long* esTamanho);
int MDFE_ConsultarRecibo(const uintptr_t libHandle, const char* ARecibo, char* sResposta, long* esTamanho);
int MDFE_Cancelar(const uintptr_t libHandle, const char* eChave, const char* eJustificativa, const char* eCNPJCPF, long ALote, char* sResposta, long* esTamanho);
int MDFE_EncerrarMDFe(const uintptr_t libHandle, const char* eChaveOuMDFe, const char* eDtEnc, const char* cMunicipioDescarga, const char* nCNPJ,
  const char* nProtocolo, char* sResposta, long* esTamanho);
int MDFE_EnviarEvento(const uintptr_t libHandle, long idLote, char* sResposta, long* esTamanho);
int MDFE_DistribuicaoDFePorUltNSU(const uintptr_t libHandle, const char* eCNPJCPF, const char* eultNSU, char* sResposta, long* esTamanho);
int MDFE_DistribuicaoDFePorNSU(const uintptr_t libHandle, const char* eCNPJCPF, const char* eNSU, char* sResposta, long* esTamanho);
int MDFE_DistribuicaoDFePorChave(const uintptr_t libHandle, const char* eCNPJCPF, const char* echMDFe, char* sResposta, long* esTamanho);
int MDFE_EnviarEmail(const uintptr_t libHandle, const char* ePara, const char* eArquivoXmlMDFe, char* AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int MDFE_EnviarEmailEvento(const uintptr_t libHandle, const char* ePara, const char* eArquivoXmlEvento, const char* eArquivoXmlMDFe, char* AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int MDFE_Imprimir(const uintptr_t libHandle, const char* cImpressora, long nNumCopias, const char* cProtocolo, const char* bMostrarPreview);
int MDFE_ImprimirPDF(const uintptr_t libHandle);
int MDFE_SalvarPDF(const uintptr_t libHandle, char* sResposta, long* esTamanho);
int MDFE_ImprimirEvento(const uintptr_t libHandle, const char* eArquivoXmlMDFe, const char* eArquivoXmlEvento);
int MDFE_ImprimirEventoPDF(const uintptr_t libHandle, const char* eArquivoXmlMDFe, const char* eArquivoXmlEvento);
int MDFE_SalvarEventoPDF(const uintptr_t libHandle, const char* eArquivoXmlMDFe, const char* eArquivoXmlEvento, char* sResposta, long* esTamanho);
int MDFE_GerarChave(const uintptr_t libHandle, long ACodigoUF, long ACodigoNumerico, long AModelo, 
    long ASerie, long ANumero, long ATpEmi, const char* AEmissao, const char* ACNPJCPF, 
    const char* sResposta, long* esTamanho);
int MDFE_ConsultaMDFeNaoEnc(const uintptr_t libHandle, const char* nCNPJ, char* sResposta, long* esTamanho);
int MDFE_ObterCertificados(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int MDFE_GetPath(const uintptr_t libHandle, int ATipo, char* sResposta, long* esTamanho);
int MDFE_GetPathEvento(const uintptr_t libHandle, char* ACodEvento, char* sResposta, long* esTamanho);
