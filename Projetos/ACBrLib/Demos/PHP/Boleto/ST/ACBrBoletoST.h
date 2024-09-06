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

int Boleto_Inicializar(const char* eArqConfig, const char* eChaveCrypt);
int Boleto_ConfigLer(const char* eArqConfig);
int Boleto_ConfigLerValor (const char* eSessao, char* eChave, char* sValor, long* esTamanho);
int Boleto_ConfigGravarValor(const char* eSessao, const char* eChave, const char* eValor);
int Boleto_UltimoRetorno(char* sMensagem, long* esTamanho);
int Boleto_Finalizar();
int Boleto_Nome(const char* sNome, long* esTamanho);
int Boleto_Versao(const char* sVersao, long* esTamanho);
int Boleto_OpenSSLInfo(const char* sOpenSSLInfo, long* esTamanho);
int Boleto_ConfigImportar(const char* eArqConfig);
int Boleto_ConfigExportar(const char* sMensagem, long* esTamanho);
int Boleto_ConfigGravar(const char* eArqConfig);
int Boleto_ConfigurarDados(const char* eArquivoIni);
int Boleto_IncluirTitulos(const char* eArquivoIni, const char* eTpSaida);
int Boleto_LimparLista();
int Boleto_TotalTitulosLista();
int Boleto_Imprimir(const char* eNomeImpressora);
int Boleto_ImprimirBoleto(long eIndice, const char* eNomeImpressora);
int Boleto_GerarPDF();
int Boleto_SalvarPDF(const char* sResposta, long* esTamanho); 
int Boleto_GerarPDFBoleto(long eIndice); 
int Boleto_SalvarPDFBoleto(long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_GerarHTML();
int Boleto_GerarRemessa(const char* eDir, long eNumArquivo, const char* eNomeArq);
int Boleto_GerarRemessaStream(long eNumArquivo, const char* sResposta, long* esTamanho);
int Boleto_LerRetorno(const char* eDir, const char* eNomeArq);
int Boleto_LerRetornoStream(const char* ARetornoBase64, const char* sResposta, long* esTamanho);
int Boleto_ObterRetorno(const char* eDir, const char* eNomeArq, const char* sResposta, long* esTamanho);
int Boleto_EnviarEmail(const char* ePara, const char* eAssunto, const char* eMensagem, const char* eCC);
int Boleto_EnviarEmailBoleto(long eIndice, const char* ePara, const char* eAssunto, const char* eMensagem, const char* eCC);
int Boleto_SetDiretorioArquivo(const char* eDir, const char* eArq);
int Boleto_ListaBancos(const char* sResposta, long* esTamanho);
int Boleto_ListaCaractTitulo(const char* sResposta, long* esTamanho);
int Boleto_ListaOcorrencias(const char* sResposta, long* esTamanho);
int Boleto_ListaOcorrenciasEX(const char* sResposta, long* esTamanho);
int Boleto_TamNossoNumero(const char* eCarteira, const char* enossoNumero, const char* eConvenio);
int Boleto_CodigosMoraAceitos(const char* sResposta, long* esTamanho);
int Boleto_SelecionaBanco(const char* eCodBanco);
int Boleto_MontarNossoNumero(long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_RetornaLinhaDigitavel(long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_RetornaCodigoBarras(long eIndice, const char* sResposta, long* esTamanho); 
int Boleto_EnviarBoleto(long eCodigoOperacao, const char* sResposta, long* esTamanho); 
int Boleto_ConsultarTitulosPorPeriodo(const char* eArquivoIni, const char* sResposta, long* esTamanho); 