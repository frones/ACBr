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



int CTE_Inicializar(const char* eArqConfig, const char* eChaveCrypt);
int CTE_ConfigLer(const char* eArqConfig);
int CTE_ConfigLerValor (const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int CTE_ConfigGravarValor(const char* eSessao, const char* eChave, const char* eValor);
int CTE_UltimoRetorno(char* sMensagem, long* esTamanho);
int CTE_Finalizar();

int CTE_Nome(const char* sNome, long* esTamanho);
int CTE_Versao(const char* sVersao, long* esTamanho);
int CTE_OpenSSLInfo(const char* sOpenSSLInfo, long* esTamanho);
int CTE_ConfigImportar(const char* eArqConfig);
int CTE_ConfigExportar(const char* sMensagem, long* esTamanho);
int CTE_ConfigGravar(const char* eArqConfig);

int CTE_CarregarXML(const char* eArquivoOuXML);
int CTE_CarregarINI(const char* eArquivoOuINI);
int CTE_ObterXml(int AIndex, const char* sResposta, long* esTamanho); 
int CTE_GravarXml(int AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int CTE_ObterIni(int AIndex, const char* sResposta, long* esTamanho); 
int CTE_GravarIni(int AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int CTE_CarregarEventoXML(const char* eArquivoOuXML);
int CTE_CarregarEventoINI(const char* eArquivoOuINI);
int CTE_LimparLista();
int CTE_LimparListaEventos();
int CTE_Assinar();
int CTE_Validar();
int CTE_ValidarRegrasdeNegocios(const char* sResposta, long* esTamanho); 
int CTE_VerificarAssinatura(const char* sResposta, long* esTamanho);
int CTE_GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero, int ATpEmi, const char* AEmissao, const char* ACNPJCPF, const char* sResposta, long* esTamanho);
int CTE_ObterCertificados(const char* sResposta, long* esTamanho);
int CTE_GetPath(int ATipo, const char* sResposta, long* esTamanho); 
int CTE_GetPathEvento(const char* ACodEvento, const char* sResposta, long* esTamanho); 
int CTE_StatusServico(const char* sResposta, long* esTamanho);
int CTE_Consultar(const char* eChaveOuCTe, int AExtrairEventos, const char* sResposta, long* esTamanho);
int CTE_Inutilizar(const char* ACNPJ, const char* AJustificativa, int Ano, int Modelo, int Serie, int NumeroInicial, int NumeroFinal, const char* sResposta, long* esTamanho);
int CTE_Enviar(int ALote, int AImprimir, int ASincrono, const char* sResposta, long* esTamanho);
int CTE_ConsultarRecibo(const char* ARecibo, const char* sResposta, long* esTamanho);
int CTE_Cancelar(const char* eChave, const char* eJustificativa, const char* eCNPJ, int ALote, const char* sResposta, long* esTamanho);
int CTE_EnviarEvento(int idLote, const char* sResposta, long* esTamanho);
int CTE_ConsultaCadastro(const char* cUF, const char* nDocumento, int nIE, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFePorUltNSU(const int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFe(int AcUFAutor, const char* eCNPJCPF, const char* eultNSU, const char* eArquivoOuXML, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFePorNSU(int AcUFAutor, const char* eCNPJCPF, const char* eNSU, const char* sResposta, long* esTamanho);
int CTE_DistribuicaoDFePorChave(int AcUFAutor, const char* eCNPJCPF, const char* echCTe, const char* sResposta, long* esTamanho);
int CTE_EnviarEmail(const char* ePara, const char* eChaveCTe, int AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int CTE_EnviarEmailEvento(const char* ePara, const char* eChaveEvento, const char* eChaveCTe, int AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int CTE_Imprimir(const char* cImpressora, int nNumCopias, const char* cProtocolo, const char* bMostrarPreview); 
int CTE_ImprimirPDF();
int CTE_SalvarPDF(const char* sResposta, long* esTamanho);
int CTE_ImprimirEvento(const char* eArquivoXmlCTe, const char* eArquivoXmlEvento);
int CTE_ImprimirEventoPDF(const char* eArquivoXmlCTe, const char* eArquivoXmlEvento);
int CTE_SalvarEventoPDF(const char* eArquivoXmlCTe, const char* eArquivoXmlEvento, const char* sResposta, long* esTamanho);
int CTE_ImprimirInutilizacao(const char* eArquivoXml);
int CTE_ImprimirInutilizacaoPDF(const char* eArquivoXml);
