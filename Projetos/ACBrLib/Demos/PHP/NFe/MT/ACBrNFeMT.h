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

int NFE_Inicializar(const uintptr_t* libHandle, const char* eArqConfig, const char* eChaveCrypt);
int NFE_ConfigLer(const uintptr_t libHandle, const char* eArqConfig);
int NFE_ConfigLerValor (const uintptr_t libHandle, const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int NFE_ConfigGravarValor(const uintptr_t libHandle, const char* eSessao, const char* eChave, const char* eValor);
int NFE_UltimoRetorno(const uintptr_t libHandle, char* sMensagem, long* esTamanho);
int NFE_Finalizar(const uintptr_t libHandle);
int NFE_StatusServico(const uintptr_t libHandle, char* sMensagem, long* esTamanho);

int NFE_Nome(const uintptr_t libHandle, const char* sNome, long* esTamanho);
int NFE_Versao(const uintptr_t libHandle, const char* sVersao, long* esTamanho);
int NFE_OpenSSLInfo(const uintptr_t libHandle, const char* sOpenSSLInfo, long* esTamanho);
int NFE_ConfigImportar(const uintptr_t libHandle, const char* eArqConfig);
int NFE_ConfigExportar(const uintptr_t libHandle, const char* sMensagem, long* esTamanho);
int NFE_ConfigGravar(const uintptr_t libHandle, const char* eArqConfig);

int NFE_CarregarXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int NFE_CarregarINI(const uintptr_t libHandle, const char* eArquivoOuINI);
int NFE_ObterXml(const uintptr_t libHandle, long AIndex, const char* sResposta, long* esTamanho);
int NFE_GravarXml(const uintptr_t libHandle, long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFE_ObterIni(const uintptr_t libHandle, long AIndex, const char* sResposta, long* esTamanho);
int NFE_GravarIni(const uintptr_t libHandle, long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFE_CarregarEventoXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int NFE_CarregarEventoINI(const uintptr_t libHandle, const char* eArquivoOuINI);
int NFE_LimparLista(const uintptr_t libHandle);
int NFE_LimparListaEventos(const uintptr_t libHandle);
int NFE_Assinar(const uintptr_t libHandle);
int NFE_Validar(const uintptr_t libHandle);
int NFE_ValidarRegrasdeNegocios(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int NFE_VerificarAssinatura(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int NFE_GerarChave(const uintptr_t libHandle, long ACodigoUF, long ACodigoNumerico, long AModelo, 
    long ASerie, long ANumero, long ATpEmi, const char* AEmissao, const char* ACNPJCPF, 
    const char* sResposta, long* esTamanho);

int NFE_ObterCertificados(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int NFE_GetPath(const uintptr_t libHandle, long ATipo, const char* sResposta, long* esTamanho);
int NFE_GetPathEvento(const uintptr_t libHandle, const char* ACodEvento, const char* sResposta, long* esTamanho);
int NFE_Consultar(const uintptr_t libHandle, const char* eChaveOuNFe, int AExtrairEventos, const char* sResposta, long* esTamanho);
int NFE_Inutilizar(const uintptr_t libHandle, const char* ACNPJ, const char* AJustificativa,
    int Ano, int Modelo, int Serie, int NumeroInicial, int NumeroFinal,
    const char* sResposta, long* esTamanho);
int NFE_Enviar(const uintptr_t libHandle, int ALote, int AImprimir, int ASincrono, int AZipado,
    const char* sResposta, long* esTamanho);
int NFE_ConsultarRecibo(const uintptr_t libHandle, const char* ARecibo, const char* sResposta, long* esTamanho);
int NFE_Cancelar(const uintptr_t libHandle, const char* eChave, const char* eJustificativa, const char* eCNPJCPF,
    int ALote, const char* sResposta, long* esTamanho);
int NFE_EnviarEvento(const uintptr_t libHandle, int idLote, const char* sResposta, long* esTamanho);
int NFE_ConsultaCadastro(const uintptr_t libHandle, const char* cUF, const char* nDocumento, int nIE,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFePorUltNSU(const uintptr_t libHandle, int AcUFAutor, const char* eCNPJCPF, const char* eultNSU,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFe(const uintptr_t libHandle, int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* eArquivoOuXML,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFePorNSU(const uintptr_t libHandle, int AcUFAutor, const char* eCNPJCPF, const char* eNSU,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFePorChave(const uintptr_t libHandle, int AcUFAutor, const char* eCNPJCPF, const char* echNFe,
    const char* sResposta, long* esTamanho);
int NFE_EnviarEmail(const uintptr_t libHandle, const char* ePara, const char* eChaveNFe, int AEnviaPDF,
    const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int NFE_EnviarEmailEvento(const uintptr_t libHandle, const char* ePara, const char* eChaveEvento, const char* eChaveNFe,
    int AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int NFE_Imprimir(const uintptr_t libHandle, const char* cImpressora, int nNumCopias, const char* cProtocolo,
    const char* bMostrarPreview, const char* cMarcaDagua, const char* bViaConsumidor, const char* bSimplificado);
int NFE_ImprimirPDF(const uintptr_t libHandle);
int NFE_SalvarPDF(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int NFE_ImprimirEvento(const uintptr_t libHandle, const char* eArquivoXmlNFe, const char* eArquivoXmlEvento);
int NFE_ImprimirEventoPDF(const uintptr_t libHandle, const char* eArquivoXmlNFe, const char* eArquivoXmlEvento);
int NFE_SalvarEventoPDF(const uintptr_t libHandle, const char* eArquivoXmlNFe, const char* eArquivoXmlEvento, const char* sResposta, long* esTamanho);
int NFE_ImprimirInutilizacao(const uintptr_t libHandle, const char* eArquivoXml);
int NFE_ImprimirInutilizacaoPDF(const uintptr_t libHandle, const char* eArquivoXml);
int NFE_SalvarInutilizacaoPDF(const uintptr_t libHandle, const char* eArquivoXml, const char* sResposta, long* esTamanho);
