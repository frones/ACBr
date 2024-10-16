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
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
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

int NFSE_Inicializar(const char* eArqConfig, const char* eChaveCrypt);
int NFSE_Finalizar();
int NFSE_Nome(const char* sNome, long* esTamanho);
int NFSE_Versao(const char* sVersao, long* esTamanho);
int NFSE_OpenSSLInfo(const char* sOpenSSLInfo, long* esTamanho);
int NFSE_UltimoRetorno(const char* sMensagem, long* esTamanho);
int NFSE_ConfigImportar(const char* eArqConfig);
int NFSE_ConfigExportar(const char* sMensagem, long* esTamanho);
int NFSE_ConfigLer(const char* eArqConfig);
int NFSE_ConfigGravar(const char* eArqConfig);
int NFSE_ConfigLerValor(const char* eSessao, const char* eChave, const char* sValor, long* esTamanho);
int NFSE_ConfigGravarValor(const char* eSessao, const char* eChave, const char* eValor);
int NFSE_CarregarXML(const char* eArquivoOuXML);
int NFSE_CarregarLoteXML(const char* eArquivoOuXML);
int NFSE_CarregarINI(const char* eArquivoOuINI);
int NFSE_ObterXml(long AIndex, char* sResposta, long* esTamanho);
int NFSE_GravarXml(long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFSE_ObterIni(long AIndex, char* sResposta, long* esTamanho);
int NFSE_GravarIni(long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFSE_LimparLista();
int NFSE_ObterCertificados(char* sResposta, long* esTamanho);
int NFSE_Emitir(const char* aLote, long aModoEnvio, char* aImprimir, char* sResposta, long* esTamanho);
int NFSE_Cancelar(const char* aInfCancelamentoNFSe, char* sResposta, long* esTamanho);
int NFSE_SubstituirNFSe(const char* aNumeroNFSe, const char* aSerieNFSe, const char* aCodigoCancelamento, const char* aMotivoCancelamento, const char* aNumeroLote, const char* aCodigoVerificacao, const char* sResposta, long* esTamanho);
int NFSE_LinkNFSe(const char* aNumeroNFSe, const char* aCodigoVerificacao, const char* aChaveAcesso, const char* aValorServico, const char* sResposta, long* esTamanho);
int NFSE_GerarLote(const char* aLote, long aQtdMaximaRps, long aModoEnvio, char* sResposta, long* esTamanho);
int NFSE_GerarToken(char* sResposta, long* esTamanho);
int NFSE_ConsultarSituacao(const char* AProtocolo, const char* ANumLote, const char* sResposta, long* esTamanho);
int NFSE_ConsultarLoteRps(const char* AProtocolo, const char* ANumLote, const char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorRps(const char* ANumeroRps, const char* ASerie, const char* ATipo, const char* ACodigoVerificacao, const char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorNumero(const char* ANumero, long APagina, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorPeriodo(double aDataInicial, double aDataFinal, long aPagina, const char* aNumeroLote, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorFaixa(const char* aNumeroInicial, const char* aNumeroFinal, long aPagina, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeGenerico(const char* aInfConsultaNFSe, char* sResposta, long* esTamanho);
int NFSE_ConsultarLinkNFSe(const char* aInfConsultaLinkNFSe, char* sResposta, long* esTamanho);
int NFSE_EnviarEmail(const char* ePara, const char* eXmlNFSe, char* AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int NFSE_Imprimir(const char* cImpressora, long nNumCopias, const char* bGerarPDF, const char* bMostrarPreview, const char* cCancelada);
int NFSE_ImprimirPDF();
int NFSE_SalvarPDF(char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorNumero(const char* aNumero, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(double aDataInicial, double aDataFinal, long aPagina, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorTomador(const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorNumero(const char* aNumero, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorPrestador(const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorTomador(const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorPeriodo(double aDataInicial, double aDataFinal, long aPagina, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const char* aCNPJ, const char* aInscMun, long aPagina, double aDataInicial, double aDataFinal, long aTipoPeriodo, char* sResposta, long* esTamanho);
int NFSE_EnviarEvento(const char* aInfEvento, char* sResposta, long* esTamanho);
int NFSE_ConsultarDPSPorChave(const char* aChaveDPS, char* sResposta, long* esTamanho);
int NFSE_ConsultarNFSePorChave(const char* aChaveNFSe, char* sResposta, long* esTamanho);
int NFSE_ConsultarEvento(const char* aChave, long aTipoEvento, long aNumSeq, char* sResposta, long* esTamanho);
int NFSE_ConsultarDFe(long aNSU, const char* sResposta, long* esTamanho);
int NFSE_ObterDANFSE(const char* aChaveNFSe, char* sResposta, long* esTamanho);
int NFSE_ConsultarParametros(long aTipoParametroMunicipio, const char* aCodigoServico, double aCompetencia, const char* aNumeroBeneficio, char* sResposta, long* esTamanho);
int NFSE_ObterInformacoesProvedor(char* sResposta, long* esTamanho);
