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

int Boleto_Inicializar(const uintptr_t* libHandle, const char* eArqConfig, const char* eChaveCrypt);
int Boleto_ConfigLer(const uintptr_t libHandle, const char* eArqConfig);
int Boleto_ConfigLerValor (const uintptr_t libHandle, const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int Boleto_ConfigGravarValor(const uintptr_t libHandle, const char* eSessao, const char* eChave, const char* eValor);
int Boleto_UltimoRetorno(const uintptr_t libHandle, char* sMensagem, long* esTamanho);
int Boleto_Finalizar(const uintptr_t libHandle);
int Boleto_Nome(const uintptr_t libHandle, const char* sNome, long* esTamanho);
int Boleto_Versao(const uintptr_t libHandle, const char* sVersao, long* esTamanho);
int Boleto_OpenSSLInfo(const uintptr_t libHandle, const char* sOpenSSLInfo, long* esTamanho);
int Boleto_ConfigImportar(const uintptr_t libHandle, const char* eArqConfig);
int Boleto_ConfigExportar(const uintptr_t libHandle, const char* sMensagem, long* esTamanho);
int Boleto_ConfigGravar(const uintptr_t libHandle, const char* eArqConfig);
int Boleto_ConfigurarDados(const uintptr_t libHandle, const char* eArquivoIni);
int Boleto_IncluirTitulos(const uintptr_t libHandle, const char* eArquivoIni, const char* eTpSaida);
int Boleto_LimparLista(const uintptr_t libHandle);
int Boleto_TotalTitulosLista(const uintptr_t libHandle);
int Boleto_Imprimir(const uintptr_t libHandle, const char* eNomeImpressora);
int Boleto_ImprimirBoleto(const uintptr_t libHandle, long eIndice, const char* eNomeImpressora);
int Boleto_GerarPDF(const uintptr_t libHandle);
int Boleto_SalvarPDF(const uintptr_t libHandle, const char* sResposta, long* esTamanho); 
int Boleto_GerarPDFBoleto(const uintptr_t libHandle, long eIndice); 
int Boleto_SalvarPDFBoleto(const uintptr_t libHandle, long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_GerarHTML(const uintptr_t libHandle);
int Boleto_GerarRemessa(const uintptr_t libHandle, const char* eDir, long eNumArquivo, const char* eNomeArq);
int Boleto_GerarRemessaStream(const uintptr_t libHandle, long eNumArquivo, const char* sResposta, long* esTamanho);
int Boleto_LerRetorno(const uintptr_t libHandle, const char* eDir, const char* eNomeArq);
int Boleto_LerRetornoStream(const uintptr_t libHandle, const char* ARetornoBase64, const char* sResposta, long* esTamanho);
int Boleto_ObterRetorno(const uintptr_t libHandle, const char* eDir, const char* eNomeArq, const char* sResposta, long* esTamanho);
int Boleto_EnviarEmail(const uintptr_t libHandle, const char* ePara, const char* eAssunto, const char* eMensagem, const char* eCC);
int Boleto_EnviarEmailBoleto(const uintptr_t libHandle, long eIndice, const char* ePara, const char* eAssunto, const char* eMensagem, const char* eCC);
int Boleto_SetDiretorioArquivo(const uintptr_t libHandle, const char* eDir, const char* eArq);
int Boleto_ListaBancos(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int Boleto_ListaCaractTitulo(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int Boleto_ListaOcorrencias(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int Boleto_ListaOcorrenciasEX(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int Boleto_TamNossoNumero(const uintptr_t libHandle, const char* eCarteira, const char* enossoNumero, const char* eConvenio);
int Boleto_CodigosMoraAceitos(const uintptr_t libHandle, const char* sResposta, long* esTamanho);
int Boleto_SelecionaBanco(const uintptr_t libHandle, const char* eCodBanco);
int Boleto_MontarNossoNumero(const uintptr_t libHandle, long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_RetornaLinhaDigitavel(const uintptr_t libHandle, long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_RetornaCodigoBarras(const uintptr_t libHandle, long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_EnviarBoleto(const uintptr_t libHandle, long eCodigoOperacao, const char* sResposta, long* esTamanho); 
int Boleto_ConsultarTitulosPorPeriodo(const uintptr_t libHandle, const char* eArquivoIni, const char* sResposta, long* esTamanho); 