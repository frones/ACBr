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

int NFSE_Inicializar(const uintptr_t* libHandle, const char* eArqConfig, const char* eChaveCrypt);
int NFSE_Finalizar(const uintptr_t libHandle);
int NFSE_Nome(const uintptr_t libHandle, const char* sNome, long* esTamanho);
int NFSE_Versao(const uintptr_t libHandle, const char* sVersao, long* esTamanho);
int NFSE_OpenSSLInfo(const uintptr_t libHandle, const char* sOpenSSLInfo, long* esTamanho);
int NFSE_UltimoRetorno(const uintptr_t libHandle, const char* sMensagem, long* esTamanho);
int NFSE_ConfigImportar(const uintptr_t libHandle, const char* eArqConfig);
int NFSE_ConfigExportar(const uintptr_t libHandle, const char* sMensagem, long* esTamanho);
int NFSE_ConfigLer(const uintptr_t libHandle, const char* eArqConfig);
int NFSE_ConfigGravar(const uintptr_t libHandle, const char* eArqConfig);
int NFSE_ConfigLerValor(const uintptr_t libHandle, const char* eSessao, const char* eChave, const char* sValor, long* esTamanho);
int NFSE_ConfigGravarValor(const uintptr_t libHandle, const char* eSessao, const char* eChave, const char* eValor);
int NFSE_CarregarXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int NFSE_CarregarLoteXML(const uintptr_t libHandle, const char* eArquivoOuXML);
int NFSE_CarregarINI(const uintptr_t libHandle, const char* eArquivoOuINI);
int NFSE_ObterXml(const uintptr_t libHandle, long AIndex, char* sResposta, long* esTamanho);
int NFSE_GravarXml(const uintptr_t libHandle, long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFSE_ObterIni(const uintptr_t libHandle, long AIndex, char* sResposta, long* esTamanho);
int NFSE_GravarIni(const uintptr_t libHandle, long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFSE_LimparLista(const uintptr_t libHandle);
int NFSE_ObterCertificados(const uintptr_t libHandle, char* sResposta, long* esTamanho);
int NFSE_Emitir(const uintptr_t libHandle, const char* aLote, long aModoEnvio, char* aImprimir, char* sResposta, long* esTamanho);
int NFSE_Cancelar(const uintptr_t libHandle, const char* aInfCancelamentoNFSe, char* sResposta, long* esTamanho);
int NFSE_SubstituirNFSe(const uintptr_t libHandle, const char* aNumeroNFSe, const char* aSerieNFSe, const char* aCodigoCancelamento, const char* aMotivoCancelamento, const char* aNumeroLote, const char* aCodigoVerificacao, const char* sResposta, long* esTamanho);
int NFSE_LinkNFSe(const uintptr_t libHandle, const char* aNumeroNFSe, const char* aCodigoVerificacao, const char* aChaveAcesso, const char* aValorServico, const char* sResposta, long* esTamanho);
int NFSE_GerarLote(const uintptr_t libHandle, const char* aLote, long aQtdMaximaRps, long aModoEnvio, char* sResposta, long* esTamanho);
int NFSE_GerarToken(const uintptr_t libHandle, char* sResposta, long* esTamanho);
int NFSE_ConsultarSituacao(const uintptr_t libHandle, const char* AProtocolo, const char* ANumLote, const char* sResposta, long* esTamanho);
int NFSE_ConsultarLoteRps(const uintptr_t libHandle, const char* AProtocolo, const char* ANumLote, const char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorRps(const uintptr_t libHandle, const char* ANumeroRps, const char* ASerie, const char* ATipo, const char* ACodigoVerificacao, const char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorNumero(const uintptr_t libHandle, const char* ANumero, long APagina, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorPeriodo(const uintptr_t libHandle, double aDataInicial, double aDataFinal, long aPagina, const char* aNumeroLote, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorFaixa(const uintptr_t libHandle, const char* aNumeroInicial, const char* aNumeroFinal, long aPagina, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeGenerico(const uintptr_t libHandle, const char* aInfConsultaNFSe, char* sResposta, long* esTamanho);
int NFSE_ConsultarLinkNFSe(const uintptr_t libHandle, const char* aInfConsultaLinkNFSe, char* sResposta, long* esTamanho);
int NFSE_EnviarEmail(const uintptr_t libHandle, const char* ePara, const char* eXmlNFSe, char* AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int NFSE_Imprimir(const uintptr_t libHandle, const char* cImpressora, long nNumCopias, const char* bGerarPDF, const char* bMostrarPreview, const char* cCancelada);
int NFSE_ImprimirPDF(const uintptr_t libHandle);
int NFSE_SalvarPDF(const uintptr_t libHandle, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorNumero(const uintptr_t libHandle, const char* aNumero, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(const uintptr_t libHandle, double aDataInicial, double aDataFinal, long aPagina, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorTomador(const uintptr_t libHandle, const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const uintptr_t libHandle, const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorNumero(const uintptr_t libHandle, const char* aNumero, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorPrestador(const uintptr_t libHandle, const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorTomador(const uintptr_t libHandle, const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorPeriodo(const uintptr_t libHandle, double aDataInicial, double aDataFinal, long aPagina, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const uintptr_t libHandle, const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_EnviarEvento(const uintptr_t libHandle, const char* aInfEvento, char* sResposta, long* esTamanho);
int NFSE_ConsultarDPSPorChave(const uintptr_t libHandle, const char* aChaveDPS, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorChave(const uintptr_t libHandle, const char* aChaveNFSe, char* sResposta, long* esTamanho);
int NFSE_ConsultarEvento(const uintptr_t libHandle, const char* aChave, long aTipoEvento, long aNumSeq, char* sResposta, long* esTamanho);
int NFSE_ConsultarDFe(const uintptr_t libHandle, long aNSU, const char* sResposta, long* esTamanho);
int NFSE_ObterDANFSE(const uintptr_t libHandle, const char* aChaveNFSe, char* sResposta, long* esTamanho);
int NFSE_ConsultarParametros(const uintptr_t libHandle, long aTipoParametroMunicipio, const char* aCodigoServico, double aCompetencia, const char* aNumeroBeneficio, char* sResposta, long* esTamanho);
int NFSE_ObterInformacoesProvedor(const uintptr_t libHandle, char* sResposta, long* esTamanho);
