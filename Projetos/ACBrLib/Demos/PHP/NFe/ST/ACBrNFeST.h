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

int NFE_Inicializar(const char* eArqConfig, const char* eChaveCrypt);
int NFE_ConfigLer(const char* eArqConfig);
int NFE_ConfigLerValor (const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int NFE_ConfigGravarValor(const char* eSessao, const char* eChave, const char* eValor);
int NFE_UltimoRetorno(char* sMensagem, long* esTamanho);
int NFE_Finalizar();
int NFE_StatusServico(char* sMensagem, long* esTamanho);

int NFE_Nome(const char* sNome, long* esTamanho);
int NFE_Versao(const char* sVersao, long* esTamanho);
int NFE_OpenSSLInfo(const char* sOpenSSLInfo, long* esTamanho);
int NFE_ConfigImportar(const char* eArqConfig);
int NFE_ConfigExportar(const char* sMensagem, long* esTamanho);
int NFE_ConfigGravar(const char* eArqConfig);

int NFE_CarregarXML(const char* eArquivoOuXML);
int NFE_CarregarINI(const char* eArquivoOuINI);
int NFE_ObterXml(long AIndex, const char* sResposta, long* esTamanho);
int NFE_GravarXml(long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFE_ObterIni(long AIndex, const char* sResposta, long* esTamanho);
int NFE_GravarIni(long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int NFE_CarregarEventoXML(const char* eArquivoOuXML);
int NFE_CarregarEventoINI(const char* eArquivoOuINI);
int NFE_LimparLista();
int NFE_LimparListaEventos();
int NFE_Assinar();
int NFE_Validar();
int NFE_ValidarRegrasdeNegocios(const char* sResposta, long* esTamanho);
int NFE_VerificarAssinatura(const char* sResposta, long* esTamanho);
int NFE_GerarChave(long ACodigoUF, long ACodigoNumerico, long AModelo, 
    long ASerie, long ANumero, long ATpEmi, const char* AEmissao, const char* ACNPJCPF, 
    const char* sResposta, long* esTamanho);

int NFE_ObterCertificados(const char* sResposta, long* esTamanho);
int NFE_GetPath(long ATipo, const char* sResposta, long* esTamanho);
int NFE_GetPathEvento(const char* ACodEvento, const char* sResposta, long* esTamanho);
int NFE_Consultar(const char* eChaveOuNFe, int AExtrairEventos, const char* sResposta, long* esTamanho);
int NFE_Inutilizar(const char* ACNPJ, const char* AJustificativa,
    int Ano, int Modelo, int Serie, int NumeroInicial, int NumeroFinal,
    const char* sResposta, long* esTamanho);
int NFE_Enviar(int ALote, int AImprimir, int ASincrono, int AZipado,
    const char* sResposta, long* esTamanho);
int NFE_ConsultarRecibo(const char* ARecibo, const char* sResposta, long* esTamanho);
int NFE_Cancelar(const char* eChave, const char* eJustificativa, const char* eCNPJCPF,
    int ALote, const char* sResposta, long* esTamanho);
int NFE_EnviarEvento(int idLote, const char* sResposta, long* esTamanho);
int NFE_ConsultaCadastro(const char* cUF, const char* nDocumento, int nIE,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFePorUltNSU(int AcUFAutor, const char* eCNPJCPF, const char* eultNSU,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFe(int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* eArquivoOuXML,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFePorNSU(int AcUFAutor, const char* eCNPJCPF, const char* eNSU,
    const char* sResposta, long* esTamanho);
int NFE_DistribuicaoDFePorChave(int AcUFAutor, const char* eCNPJCPF, const char* echNFe,
    const char* sResposta, long* esTamanho);
int NFE_EnviarEmail(const char* ePara, const char* eChaveNFe, int AEnviaPDF,
    const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int NFE_EnviarEmailEvento(const char* ePara, const char* eChaveEvento, const char* eChaveNFe,
    int AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int NFE_Imprimir(const char* cImpressora, int nNumCopias, const char* cProtocolo,
    const char* bMostrarPreview, const char* cMarcaDagua, const char* bViaConsumidor, const char* bSimplificado);
int NFE_ImprimirPDF();
int NFE_SalvarPDF(const char* sResposta, long* esTamanho);
int NFE_ImprimirEvento(const char* eArquivoXmlNFe, const char* eArquivoXmlEvento);
int NFE_ImprimirEventoPDF(const char* eArquivoXmlNFe, const char* eArquivoXmlEvento);
int NFE_SalvarEventoPDF(const char* eArquivoXmlNFe, const char* eArquivoXmlEvento, const char* sResposta, long* esTamanho);
int NFE_ImprimirInutilizacao(const char* eArquivoXml);
int NFE_ImprimirInutilizacaoPDF(const char* eArquivoXml);
int NFE_SalvarInutilizacaoPDF(const char* eArquivoXml, const char* sResposta, long* esTamanho);
