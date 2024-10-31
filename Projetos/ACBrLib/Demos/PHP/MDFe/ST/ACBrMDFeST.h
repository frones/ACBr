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

int MDFE_Inicializar(const char* eArqConfig, const char* eChaveCrypt);
int MDFE_ConfigLer(const char* eArqConfig);
int MDFE_ConfigLerValor (const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int MDFE_ConfigGravarValor(const char* eSessao, const char* eChave, const char* eValor);
int MDFE_UltimoRetorno(char* sMensagem, long* esTamanho);
int MDFE_Finalizar();

int MDFE_Nome(const char* sNome, long* esTamanho);
int MDFE_Versao(const char* sVersao, long* esTamanho);
int MDFE_OpenSSLInfo(const char* sOpenSSLInfo, long* esTamanho);
int MDFE_ConfigImportar(const char* eArqConfig);
int MDFE_ConfigExportar(const char* sMensagem, long* esTamanho);
int MDFE_ConfigGravar(const char* eArqConfig);

int MDFE_StatusServico(char* sMensagem, long* esTamanho);
int MDFE_CarregarXML(const char* eArquivoOuXML);
int MDFE_CarregarINI(const char* eArquivoOuINI);
int MDFE_ObterXml(long AIndex, char* sResposta, long* esTamanho);
int MDFE_GravarXml(long AIndex, const char* eNomeArquivo, const char* ePathArquivo);
int MDFE_ObterIni(int AIndex, char* sResposta, long* esTamanho);
int MDFE_GravarIni(int AIndex, char* sResposta, long* esTamanho);
int MDFE_CarregarEventoXML(const char* eArquivoOuXML);
int MDFE_CarregarEventoINI(const char* eArquivoOuINI);
int MDFE_LimparLista();
int MDFE_LimparListaEventos();
int MDFE_Assinar();
int MDFE_Validar();
int MDFE_ValidarRegrasdeNegocios(char* sResposta, long* esTamanho);
int MDFE_VerificarAssinatura(char* sResposta, long* esTamanho);

int MDFE_Consultar(const char* eChaveOuMDFe, int AExtrairEventos, char* sResposta, long* esTamanho);
int MDFE_Enviar(long ALote, char* Imprimir, char* Sincrono, char* sResposta, long* esTamanho);
int MDFE_ConsultarRecibo(const char* ARecibo, char* sResposta, long* esTamanho);
int MDFE_Cancelar(const char* eChave, const char* eJustificativa, const char* eCNPJCPF, long ALote, char* sResposta, long* esTamanho);
int MDFE_EncerrarMDFe(const char* eChaveOuMDFe, const char* eDtEnc, const char* cMunicipioDescarga, const char* nCNPJ,
  const char* nProtocolo, char* sResposta, long* esTamanho);
int MDFE_EnviarEvento(long idLote, char* sResposta, long* esTamanho);
int MDFE_DistribuicaoDFePorUltNSU(const char* eCNPJCPF, const char* eultNSU, char* sResposta, long* esTamanho);
int MDFE_DistribuicaoDFePorNSU(const char* eCNPJCPF, const char* eNSU, char* sResposta, long* esTamanho);
int MDFE_DistribuicaoDFePorChave(const char* eCNPJCPF, const char* echMDFe, char* sResposta, long* esTamanho);
int MDFE_EnviarEmail(const char* ePara, const char* eArquivoXmlMDFe, char* AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int MDFE_EnviarEmailEvento(const char* ePara, const char* eArquivoXmlEvento, const char* eArquivoXmlMDFe, char* AEnviaPDF, const char* eAssunto, const char* eCC, const char* eAnexos, const char* eMensagem);
int MDFE_Imprimir(const char* cImpressora, long nNumCopias, const char* cProtocolo, const char* bMostrarPreview);
int MDFE_ImprimirPDF();
int MDFE_SalvarPDF(char* sResposta, long* esTamanho);
int MDFE_ImprimirEvento(const char* eArquivoXmlMDFe, const char* eArquivoXmlEvento);
int MDFE_ImprimirEventoPDF(const char* eArquivoXmlMDFe, const char* eArquivoXmlEvento);
int MDFE_SalvarEventoPDF(const char* eArquivoXmlMDFe, const char* eArquivoXmlEvento, char* sResposta, long* esTamanho);
int MDFE_GerarChave(long ACodigoUF, long ACodigoNumerico, long AModelo, 
    long ASerie, long ANumero, long ATpEmi, const char* AEmissao, const char* ACNPJCPF, 
    const char* sResposta, long* esTamanho);
int MDFE_ConsultaMDFeNaoEnc(const char* nCNPJ, char* sResposta, long* esTamanho);
int MDFE_ObterCertificados(const char* sResposta, long* esTamanho);
int MDFE_GetPath(int ATipo, char* sResposta, long* esTamanho);
int MDFE_GetPathEvento(char* ACodEvento, char* sResposta, long* esTamanho);
