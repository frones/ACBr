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

int CTE_Inicializar(const uintptr_t* libHandle, const char* eArqConfig, const char* eChaveCrypt);
int CTE_ConfigLer(const uintptr_t libHandle, const char* eArqConfig);
int CTE_ConfigLerValor (const uintptr_t libHandle, const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int CTE_ConfigGravarValor(const uintptr_t libHandle, const char* eSessao, const char* eChave, const char* eValor);
int CTE_UltimoRetorno(const uintptr_t libHandle, char* sMensagem, long* esTamanho);
int CTE_Finalizar(const uintptr_t libHandle);

int CTE_Nome(const uintptr_t libHandle, const char* sNome, long* esTamanho);
int CTE_Versao(const uintptr_t libHandle, const char* sVersao, long* esTamanho);
int CTE_OpenSSLInfo(const uintptr_t libHandle, const char* sOpenSSLInfo, long* esTamanho);
int CTE_ConfigImportar(const uintptr_t libHandle, const char* eArqConfig);
int CTE_ConfigExportar(const uintptr_t libHandle, const char* sMensagem, long* esTamanho);
int CTE_ConfigGravar(const uintptr_t libHandle, const char* eArqConfig);

int CTE_CarregarXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int CTE_CarregarINI(const uintptr_t libHandle, const char* eArquivoOuINI);
int CTE_ObterXml(const uintptr_t libHandle, int AIndex, const char* sResposta, long* esTamanho); 
int CTE_GravarXml(const uintptr_t libHandle, int AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int CTE_ObterIni(const uintptr_t libHandle, int AIndex, const char* sResposta, long* esTamanho); 
int CTE_GravarIni(const uintptr_t libHandle, int AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int CTE_CarregarEventoXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int CTE_CarregarEventoINI(const uintptr_t libHandle, const char* eArquivoOuINI);
int CTE_LimparLista(const uintptr_t libHandle);
int CTE_LimparListaEventos(const uintptr_t libHandle);
int CTE_Assinar(const uintptr_t libHandle);
int CTE_Validar(const uintptr_t libHandle);
int CTE_ValidarRegrasdeNegocios(const uintptr_t libHandle, const char* sResposta, long* esTamanho); 
int CTE_VerificarAssinatura(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int CTE_GerarChave(const uintptr_t libHandle, int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero, int ATpEmi, const char* AEmissao, const char* ACNPJCPF, const char* sResposta, long* esTamanho);
int CTE_ObterCertificados(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int CTE_GetPath(const uintptr_t libHandle, int ATipo, const char* sResposta, long* esTamanho); 
int CTE_GetPathEvento(const uintptr_t libHandle, const char* ACodEvento, const char* sResposta, long* esTamanho); 
int CTE_StatusServico(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int CTE_Consultar(const uintptr_t libHandle, const char* eChaveOuCTe, int AExtrairEventos, const char* sResposta, long* esTamanho);
int CTE_Inutilizar(const uintptr_t libHandle, const char* ACNPJ, const char* AJustificativa, int Ano, int Modelo, int Serie, int NumeroInicial, int NumeroFinal, const char* sResposta, long* esTamanho);
int CTE_Enviar(const uintptr_t libHandle, int ALote, int AImprimir, int ASincrono, const char* sResposta, long* esTamanho);
int CTE_ConsultarRecibo(const uintptr_t libHandle, const char* ARecibo, const char* sResposta, long* esTamanho);
int CTE_Cancelar(const uintptr_t libHandle, const char* eChave, const char* eJustificativa, const char* eCNPJ, int ALote, const char* sResposta, long* esTamanho);
int CTE_EnviarEvento(const uintptr_t libHandle, int idLote, const char* sResposta, long* esTamanho);
int CTE_ConsultaCadastro(const uintptr_t libHandle, const char* cUF, const char* nDocumento, int nIE, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFePorUltNSU(const uintptr_t libHandle, const int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFe(const uintptr_t libHandle, int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* eArquivoOuXML, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFePorNSU(const uintptr_t libHandle, int AcUFAutor, const char* eCNPJCPF, const char* eNSU, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFePorChave(const uintptr_t libHandle, int AcUFAutor, const char* eCNPJCPF, const char* echCTe, const char* sResposta, long* esTamanho);
int CTE_EnviarEmail(const uintptr_t libHandle, const char* ePara, const char* eChaveCTe, int AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int CTE_EnviarEmailEvento(const uintptr_t libHandle, const char* ePara, const char* eChaveEvento, const char* eChaveCTe, int AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int CTE_Imprimir(const uintptr_t libHandle, const char* cImpressora, int nNumCopias, const char* cProtocolo, const char* bMostrarPreview); 
int CTE_ImprimirPDF(const uintptr_t libHandle);
int CTE_SalvarPDF(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int CTE_ImprimirEvento(const uintptr_t libHandle, const char* eArquivoXmlCTe, const char* eArquivoXmlEvento);
int CTE_ImprimirEventoPDF(const uintptr_t libHandle, const char* eArquivoXmlCTe, const char* eArquivoXmlEvento);
int CTE_SalvarEventoPDF(const uintptr_t libHandle, const char* eArquivoXmlCTe, const char* eArquivoXmlEvento, const char* sResposta, long* esTamanho);
int CTE_ImprimirInutilizacao(const uintptr_t libHandle, const char* eArquivoXml);
int CTE_ImprimirInutilizacaoPDF(const uintptr_t libHandle, const char* eArquivoXml);
