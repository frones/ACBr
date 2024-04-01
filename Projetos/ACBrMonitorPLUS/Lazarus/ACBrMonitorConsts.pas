{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

unit ACBrMonitorConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

ResourceString
  SErrTempoUsoExpirou = 'Tempo de Uso Expirou!';
  SErrArqConfNaoEncontrado = 'Arquivo de configuração não encontrado';
  SErrArqConfigNaoDefinido = 'Arquivo de configuração não definido';
  SErrDiretorioInvalido = 'Diretório Invalido: %s';
  SErroNaoImplementado = 'Metodo não Implementado';
  SErrArqNaoEncontrado = 'Arquivo % não encontrado';
  SErrSATHashLibInvalido = 'Biblioteca do SAT [%s] com assinatura inválida';
  SErrSATMarcaNaoEncontrada =  'Marca [%s] não encontrada no arquivo: %s';

  SErroNFeAbrir = 'Erro ao abrir o arquivo da Nota Fiscal: %s';
  SErroNFeCarregar = 'Erro ao carregar Nota Fiscal';

  SErrocteAbrir = 'Erro ao abrir o arquivo do Conhecimento: %s';
  SErroCTeCarregar = 'Erro ao carregar Conhecimento';

  SErroGNReAbrir = 'Erro ao abrir o arquivo da GNRe: %s';
  SErroGNReCarregar = 'Erro ao carregar GNRe';

  SErroMDFeCarregar = 'Erro ao carregar MDFe';
  SErroArqNaoEncontado = 'Arquivo %s nao encontrado.';
  SErroMDFeAbrir = 'Erro ao abrir o arquivo do Manifesto: %s';

  SErroBPeCarregar = 'Erro ao carregar BPe';
  SErroBPeAbrir = 'Erro ao abrir o arquivo do Bilhete: %s';

  SErroeSocialCarregar = 'Erro ao carregar eSocial';
  SErroeSocialAbrir = 'Erro ao abrir o arquivo do eSocial: %s';
  SMsgeSocialEventoAdicionado = 'Evento Adicionado: %s';
  SMsgeSocialLimparLista = 'Lista de Eventos Limpas com Sucesso';
  SErroeSocialNenhumEvento = 'Erro: Nenhum evento na lista';
  SErroIDEmpregadorTransmissor = 'ID do Empregador/Transmissor Inválido.';
  SErroeSocialConsulta = 'Erro ao Consultar Evento - Parâmetro não Preenchido';

  SErroReinfCarregar = 'Erro ao carregar Reinf';
  SErroReinfAbrir = 'Erro ao abrir o arquivo do Reinf: %s';
  SMsgReinfEventoAdicionado = 'Evento Adicionado: %s';
  SMsgReinfLimparLista = 'Lista de Eventos Limpas com Sucesso';
  SErroReinfNenhumEvento = 'Erro: Nenhum evento na lista';
  SErroIDContribuinteTransmissor = 'ID do Contribuinte/Transmissor Inválido.';
  SErroReinfConsulta = 'Erro ao Consultar Evento - Parâmetro não Preenchido.';
  SErroSSLDesabilitado = 'Será utilizado a configuração padrão SSL: %s !';

  SErroVersaoInvalida = 'Versão Inválida.';
  SErroTipoContribuinteInvalido = 'Tipo de Contribuinte Inválido.';

const
  CMonitorIni = 'ACBrMonitor.ini';
  C_PROJETO_ACBR = 'Projeto ACBr';
  C_PROJETOACBR_COM_BR = 'www.projetoacbr.com.br';
  CODIGO_HOMOLOGACAO = 1;

  _C = 'tYk*5W@';
  C_LEVEL0 = 0;
  C_LEVEL1 = 1;
  C_LEVEL2 = 2;

  CModeloNFe55 = 55;
  CModeloNFe65 = 65;
  CEmissaoESCPOS = 1;
  CEmissaoFortes = 0;

  CACBrNFeServicosIni =                'ACBrNFeServicos.ini';
  CACBrCTeServicosIni =                'ACBrCTeServicos.ini';
  CACBrMDFeServicosIni =               'ACBrMDFeServicos.ini';
  CACBrGNREServicosIni =               'ACBrGNREServicos.ini';
  CACBreSocialServicosIni =            'ACBreSocialServicos.ini';
  CACBrReinfServicosIni =              'ACBrReinfServicos.ini';
  CACBrBPeServicosIni =                'ACBrBPeServicos.ini';
  CACBrNFSeServicosIni =               'ACBrNFSeXServicos.ini';

  CMetodoSATAtivar =                   'ativar';
  CMetodoInicializar =                 'inicializar';
  CMetodoDesInicializar =              'desinicializar';
  CMetodoAssociarAssinatura =          'associarassinatura';
  CMetodoBloquear =                    'bloquear';
  CMetodoDesbloquear =                 'desbloquear';
  CMetodotrocarcodigoativacao =        'trocarcodigoativacao';
  CMetodoConsultarSat =                'consultarsat';
  CMetodoConsultarStatusOperacional =  'consultarstatusoperacional';
  CMetodoConsultarSessao =             'consultarsessao';
  CMetodoConsultarNumeroSessao =       'consultarnumerosessao';
  CMetodoAtualizaSoftware =            'atualizasoftware';
  CMetodoAtualizarSoftwareSAT =        'atualizarsoftwaresat';
  CMetodoComunicarCertificado =        'comunicarcertificado';
  CMetodoComunicarCertificadoICPBrasil='comunicarcertificadoicpbrasil';
  CMetodoCarregarDadosVenda =          'carregardadosvenda';
  CMetodoCarregarDadosCancelamento =   'carregardadoscancelamento';
  CMetodoCriarCFe =                    'criarcfe';
  CMetodoCriarEnviarCFe =              'criarenviarcfe';
  CMetodoEnviarCFe =                   'enviarcfe';
  CMetodoCancelarCFe =                 'cancelarcfe';
  CMetodoImprimirExtratoVenda =        'imprimirextratovenda';
  CMetodoImprimirExtratoResumido =     'imprimirextratoresumido';
  CMetodoImprimirExtratoCancelamento = 'imprimirextratocancelamento';
  CMetodoGerarImpressaoFiscalMFe =     'gerarimpressaofiscalmfe';
  CMetodoGerarPDFExtratoVenda =        'gerarpdfextratovenda';
  CMetodoGerarPDFExtratoCancelamento = 'gerarpdfextratocancelamento';
  CMetodoExtrairLogs =                 'extrairlogs';
  CMetodoTesteFimaFim =                'testefimafim';
  CMetodoSetNumeroSessao =             'setnumerosessao';
  CMetodoSetlogomarcaSAT =             'setlogomarca';
  CMetodoGerarAssinaturaSAT =          'gerarassinaturasat';
  CMetodoEnviarEmailCFe =              'enviaremailcfe';
  CMetodoConsultarModeloSAT =          'consultarmodelosat';
  CMetodoConsultarUltimaSessaoFiscal=  'consultarultimasessaofiscal';

  CMetodoECFachar =                                      'achar';
  CMetodoECFativar =                                     'ativar';
  CMetodoECFdesativar =                                  'desativar';
  CMetodoECFativo =                                      'ativo';
  CMetodoECFcolunas =                                    'colunas';
  CMetodoECFparamdescontoissqn =                         'paramdescontoissqn';
  CMetodoECFenviainfo =                                  'enviainfo';
  CMetodoECFretornainfoecf =                             'retornainfoecf';
  CMetodoECFcomandoenviado =                             'comandoenviado';
  CMetodoECFrespostacomando =                            'respostacomando';
  CMetodoECFmodelostr =                                  'modelostr';
  CMetodoECFmodelo =                                     'modelo';
  CMetodoECFporta =                                      'porta';
  CMetodoECFtimeout =                                    'timeout';
  CMetodoECFsettimeout =                                 'settimeout';
  CMetodoECFintervaloaposcomando =                       'intervaloaposcomando';
  CMetodoECFdescricaogrande =                            'descricaogrande';
  CMetodoECFgavetasinalinvertido =                       'gavetasinalinvertido';
  CMetodoECFignorartagsformatacao =                      'ignorartagsformatacao';
  CMetodoECFcontroleporta =                              'controleporta';
  CMetodoECFoperador =                                   'operador';
  CMetodoECFmsgaguarde =                                 'msgaguarde';
  CMetodoECFmsgtrabalhando =                             'msgtrabalhando';
  CMetodoECFmsgpoucopapel =                              'msgpoucopapel';
  CMetodoECFexibemensagem =                              'exibemensagem';
  CMetodoECFbloqueiamouseteclado =                       'bloqueiamouseteclado';
  CMetodoECFlinhasentrecupons =                          'linhasentrecupons';
  CMetodoECFpaginadecodigo =                             'paginadecodigo';
  CMetodoECFmaxlinhasbuffer =                            'maxlinhasbuffer';
  CMetodoECFsetmaxlinhasbuffer =                         'setmaxlinhasbuffer';
  CMetodoECFdatahora =                                   'datahora';
  CMetodoECFnumcupom =                                   'numcupom';
  CMetodoECFnumcoo =                                     'numcoo';
  CMetodoECFnumloja =                                    'numloja';
  CMetodoECFnumcro =                                     'numcro';
  CMetodoECFnumccf =                                     'numccf';
  CMetodoECFnumgrg =                                     'numgrg';
  CMetodoECFnumgnf =                                     'numgnf';
  CMetodoECFnumgnfc =                                    'numgnfc';
  CMetodoECFnumcdc =                                     'numcdc';
  CMetodoECFnumcfc =                                     'numcfc';
  CMetodoECFnumccdc =                                    'numccdc';
  CMetodoECFnumcfd =                                     'numcfd';
  CMetodoECFnumncn =                                     'numncn';
  CMetodoECFnumcrz =                                     'numcrz';
  CMetodoECFnumecf =                                     'numecf';
  CMetodoECFnumserie =                                   'numserie';
  CMetodoECFnumseriemfd =                                'numseriemfd';
  CMetodoECFnumversao =                                  'numversao';
  CMetodoECFmfadicional =                                'mfadicional';
  CMetodoECFrfdid =                                      'rfdid';
  CMetodoECFdatamovimento =                              'datamovimento';
  CMetodoECFcnpj =                                       'cnpj';
  CMetodoECFie =                                         'ie';
  CMetodoECFim =                                         'im';
  CMetodoECFcliche =                                     'cliche';
  CMetodoECFusuarioatual =                               'usuarioatual';
  CMetodoECFdatahorasb =                                 'datahorasb';
  CMetodoECFdatahoraultimareducaoz =                     'datahoraultimareducaoz';
  CMetodoECFdecimaisqtd =                                'decimaisqtd';
  CMetodoECFdecimaispreco =                              'decimaispreco';
  CMetodoECFsubmodeloecf =                               'submodeloecf';
  CMetodoECFpaf =                                        'paf';
  CMetodoECFnumcooinicial =                              'numcooinicial';
  CMetodoECFvendabruta =                                 'vendabruta';
  CMetodoECFgrandetotal =                                'grandetotal';
  CMetodoECFtotaltroco =                                 'totaltroco';
  CMetodoECFtotalcancelamentos =                         'totalcancelamentos';
  CMetodoECFtotaldescontos =                             'totaldescontos';
  CMetodoECFtotalacrescimos =                            'totalacrescimos';
  CMetodoECFtotalsubstituicaotributaria =                'totalsubstituicaotributaria';
  CMetodoECFtotalnaotributado =                          'totalnaotributado';
  CMetodoECFtotalisencao =                               'totalisencao';
  CMetodoECFtotalcancelamentosissqn =                    'totalcancelamentosissqn';
  CMetodoECFtotaldescontosissqn =                        'totaldescontosissqn';
  CMetodoECFtotalacrescimosissqn =                       'totalacrescimosissqn';
  CMetodoECFtotalsubstituicaotributariaissqn =           'totalsubstituicaotributariaissqn';
  CMetodoECFtotalnaotributadoissqn =                     'totalnaotributadoissqn';
  CMetodoECFtotalisencaoissqn =                          'totalisencaoissqn';
  CMetodoECFtotalnaofiscal =                             'totalnaofiscal';
  CMetodoECFtotalcancelamentosopnf =                     'totalcancelamentosopnf';
  CMetodoECFtotaldescontosopnf =                         'totaldescontosopnf';
  CMetodoECFtotalacrescimosopnf =                        'totalacrescimosopnf';
  CMetodoECFnumultitem =                                 'numultitem';
  CMetodoECFdadosreducaoz =                              'dadosreducaoz';
  CMetodoECFdadosultimareducaoz =                        'dadosultimareducaoz';
  CMetodoECFnumreducoeszrestantes =                      'numreducoeszrestantes';
  CMetodoECFaliquotas =                                  'aliquotas';
  CMetodoECFcarregaaliquotas =                           'carregaaliquotas';
  CMetodoECFlertotaisaliquota =                          'lertotaisaliquota';
  CMetodoECFprogramaaliquota =                           'programaaliquota';
  CMetodoECFachaicmsaliquota =                           'achaicmsaliquota';
  CMetodoECFformaspagamento =                            'formaspagamento';
  CMetodoECFcarregaformaspagamento =                     'carregaformaspagamento';
  CMetodoECFlertotaisformapagamento =                    'lertotaisformapagamento';
  CMetodoECFprogramaformapagamento =                     'programaformapagamento';
  CMetodoECFachafpgdescricao =                           'achafpgdescricao';
  CMetodoECFcomprovantesnaofiscais =                     'comprovantesnaofiscais';
  CMetodoECFcarregacomprovantesnaofiscais =              'carregacomprovantesnaofiscais';
  CMetodoECFlertotaiscomprovantenaofiscal =              'lertotaiscomprovantenaofiscal';
  CMetodoECFprogramacomprovantenaofiscal =               'programacomprovantenaofiscal';
  CMetodoECFachacnfdescricao =                           'achacnfdescricao';
  CMetodoECFunidadesmedida =                             'unidadesmedida';
  CMetodoECFcarregaunidadesmedida =                      'carregaunidadesmedida';
  CMetodoECFprogramaunidademedida =                      'programaunidademedida';
  CMetodoECFrelatoriosgerenciais =                       'relatoriosgerenciais';
  CMetodoECFcarregarelatoriosgerenciais =                'carregarelatoriosgerenciais';
  CMetodoECFlertotaisrelatoriosgerenciais =              'lertotaisrelatoriosgerenciais';
  CMetodoECFprogramarelatoriosgerenciais =               'programarelatoriosgerenciais';
  CMetodoECFachargdescricao =                            'achargdescricao';
  CMetodoECFtestapodeabrircupom =                        'testapodeabrircupom';
  CMetodoECFidentificaoperador =                         'identificaoperador';
  CMetodoECFidentificaconsumidor =                       'identificaconsumidor';
  CMetodoECFidentificapaf =                              'identificapaf';
  CMetodoECFabrecupom =                                  'abrecupom';
  CMetodoECFlegendainmetroproximoitem =                  'legendainmetroproximoitem';
  CMetodoECFvendeitem =                                  'vendeitem';
  CMetodoECFdescontoacrescimoitemanterior =              'descontoacrescimoitemanterior';
  CMetodoECFsubtotalizacupom =                           'subtotalizacupom';
  CMetodoECFefetuapagamento =                            'efetuapagamento';
  CMetodoECFestornapagamento =                           'estornapagamento';
  CMetodoECFfechacupom =                                 'fechacupom';
  CMetodoECFcancelacupom =                               'cancelacupom';
  CMetodoECFcancelaitemvendido =                         'cancelaitemvendido';
  CMetodoECFcancelaitemvendidoparcial =                  'cancelaitemvendidoparcial';
  CMetodoECFcanceladescontoacrescimoitem =               'canceladescontoacrescimoitem';
  CMetodoECFcanceladescontoacrescimosubtotal =           'canceladescontoacrescimosubtotal';
  CMetodoECFsubtotal =                                   'subtotal';
  CMetodoECFtotalpago =                                  'totalpago';
  CMetodoECFsangria =                                    'sangria';
  CMetodoECFsuprimento =                                 'suprimento';
  CMetodoECFcortapapel =                                 'cortapapel';
  CMetodoECFnaofiscalcompleto =                          'naofiscalcompleto';
  CMetodoECFabrenaofiscal =                              'abrenaofiscal';
  CMetodoECFregistraitemnaofiscal =                      'registraitemnaofiscal';
  CMetodoECFcancelaitemnaofiscal =                       'cancelaitemnaofiscal';
  CMetodoECFsubtotalizanaofiscal =                       'subtotalizanaofiscal';
  CMetodoECFefetuapagamentonaofiscal =                   'efetuapagamentonaofiscal';
  CMetodoECFfechanaofiscal =                             'fechanaofiscal';
  CMetodoECFcancelanaofiscal =                           'cancelanaofiscal';
  CMetodoECFleiturax =                                   'leiturax';
  CMetodoECFpafmf_lx_impressao =                         'pafmf_lx_impressao';
  CMetodoECFleituraxserial =                             'leituraxserial';
  CMetodoECFreducaoz =                                   'reducaoz';
  CMetodoECFtipoultimodocumento =                        'tipoultimodocumento';
  CMetodoECFpoucopapel =                                 'poucopapel';
  CMetodoECFhorarioverao =                               'horarioverao';
  CMetodoECFarredonda =                                  'arredonda';
  CMetodoECFarredondaporqtd =                            'arredondaporqtd';
  CMetodoECFarredondaitemmfd =                           'arredondaitemmfd';
  CMetodoECFsetarredondaitemmfd =                        'setarredondaitemmfd';
  CMetodoECFmfd =                                        'mfd';
  CMetodoECFtermica =                                    'termica';
  CMetodoECFidentificaconsumidorrodape =                 'identificaconsumidorrodape';
  CMetodoECFestado =                                     'estado';
  CMetodoECFabregaveta =                                 'abregaveta';
  CMetodoECFgavetaaberta =                               'gavetaaberta';
  CMetodoECFimprimecheque =                              'imprimecheque';
  CMetodoECFcancelaimpressaocheque =                     'cancelaimpressaocheque';
  CMetodoECFchequepronto =                               'chequepronto';
  CMetodoECFleituracmc7 =                                'leituracmc7';
  CMetodoECFmudahorarioverao =                           'mudahorarioverao';
  CMetodoECFmudaarredondamento =                         'mudaarredondamento';
  CMetodoECFpreparatef =                                 'preparatef';
  CMetodoECFcorrigeestadoerro =                          'corrigeestadoerro';
  CMetodoECFabrerelatoriogerencial =                     'abrerelatoriogerencial';
  CMetodoECFrelatoriogerencial =                         'relatoriogerencial';
  CMetodoECFpulalinhas =                                 'pulalinhas';
  CMetodoECFlinharelatoriogerencial =                    'linharelatoriogerencial';
  CMetodoECFabrecupomvinculado =                         'abrecupomvinculado';
  CMetodoECFlinhacupomvinculado =                        'linhacupomvinculado';
  CMetodoECFcupomvinculado =                             'cupomvinculado';
  CMetodoECFestornaccd =                                 'estornaccd';
  CMetodoECFsegundaviavinculado =                        'segundaviavinculado';
  CMetodoECFreimpressaovinculado =                       'reimpressaovinculado';
  CMetodoECFfecharelatorio =                             'fecharelatorio';
  CMetodoECFleituramemoriafiscal =                       'leituramemoriafiscal';
  CMetodoECFleituramemoriafiscalserial =                 'leituramemoriafiscalserial';
  CMetodoECFleituramfdserial =                           'leituramfdserial';
  CMetodoECFarquivomfd_dll =                             'arquivomfd_dll';
  CMetodoECFespelhomfd_dll =                             'espelhomfd_dll';
  CMetodoECFpafmf_lmfc_impressao =                       'pafmf_lmfc_impressao';
  CMetodoECFpafmf_lmfc_espelho =                         'pafmf_lmfc_espelho';
  CMetodoECFpafmf_lmfc_cotepe1704 =                      'pafmf_lmfc_cotepe1704';
  CMetodoECFpafmf_lmfs_impressao =                       'pafmf_lmfs_impressao';
  CMetodoECFpafmf_lmfs_espelho =                         'pafmf_lmfs_espelho';
  CMetodoECFpafmf_mfd_espelho =                          'pafmf_mfd_espelho';
  CMetodoECFpafmf_mfd_cotepe1704 =                       'pafmf_mfd_cotepe1704';
  CMetodoECFpafmf_gerarcat52 =                           'pafmf_gerarcat52';
  CMetodoECFassinarblocoxestoque =                       'assinarblocoxestoque';
  CMetodoECFassinarblocoxreducaoz =                      'assinarblocoxreducaoz';
  CMetodoECFassinarblocox =                              'assinarblocox';
  CMetodoECFvalidarblocoxestoque =                       'validarblocoxestoque';
  CMetodoECFvalidarblocoxreducaoz =                      'validarblocoxreducaoz';
  CMetodoECFvalidarblocox =                              'validarblocox';
  CMetodoECFenviarblocoxestoque =                        'enviarblocoxestoque';
  CMetodoECFenviarblocoxreducaoz =                       'enviarblocoxreducaoz';
  CMetodoECFenviarblocox =                               'enviarblocox';
  CMetodoECFconsultarblocox =                            'consultarblocox';
  CMetodoECFenviacomando =                               'enviacomando';
  CMetodoECFassinaarquivo =                              'assinaarquivo';
  CMetodoECFconfigbarras =                               'configbarras';
  CMetodoECFpafmf_arqmf_binario =                        'pafmf_arqmf_binario';
  CMetodoECFpafmf_arquivomf =                            'pafmf_arquivomf';
  CMetodoECFpafmf_arqmf =                                'pafmf_arqmf';
  CMetodoECFpafmf_arqmfd_binario =                       'pafmf_arqmfd_binario';
  CMetodoECFpafmf_arquivomfd =                           'pafmf_arquivomfd';
  CMetodoECFpafmf_arqmfd =                               'pafmf_arqmfd';
  CMetodoECFTransmitirArquivo =                          'transmitirarquivo';
  CMetodoECFConsultarProcessamentoArq =                  'consultarprocessamentoarq';
  CMetodoECFCancelarArquivo =                            'cancelararquivo';
  CMetodoECFConsultarHistoricoArq =                      'consultarhistoricoarq';
  CMetodoECFConsultarPendenciasContrib =                 'consultarpendenciascontrib';
  CMetodoECFConsultarPendenciasDevPAFECF =               'consultarpendenciasdevpafecf';
  CMetodoECFDownloadArquivo =                            'downloadarquivo';
  CMetodoECFListarArquivos =                             'listararquivos';
  CMetodoECFReprocessarArquivo =                         'reprocessararquivo';

  CMetodostatusservico =               'statusservico';
  CMetodoValidarmdfe =                 'validarmdfe';
  CMetodoAssinarmdfe =                 'assinarmdfe';
  CMetodoConsultarmdfe =               'consultarmdfe';
  CMetodoCancelarmdfe =                'cancelarmdfe';
  CMetodoEncerrarmdfe =                'encerrarmdfe';
  CMetodoConsultamdfenaoenc =          'consultamdfenaoenc';
  CMetodoImprimirdamdfe =              'imprimirdamdfe';
  CMetodoImprimirdamdfepdf =           'imprimirdamdfepdf';
  CMetodoImprimirevento =              'imprimirevento';
  CMetodoImprimireventopdf =           'imprimireventopdf';
  CMetodoEnviarmdfe =                  'enviarmdfe';
  CMetodoCriarmdfe =                   'criarmdfe';
  CMetodoCriarenviarmdfe =             'criarenviarmdfe';
  CMetodoAdicionarmdfe =               'adicionarmdfe';
  CMetodoEnviarlotemdfe =              'enviarlotemdfe';
  CMetodoRecibomdfe =                  'recibomdfe';
  CMetodoInutilizarmdfe =              'inutilizarmdfe';
  CMetodoConsultacadastro =            'consultacadastro';
  CMetodoEnviaremail =                 'enviaremail';
  CMetodoSetambiente =                 'setambiente';
  CMetodoSetlogomarca =                'setlogomarca';
  CMetodoSetformaemissao =             'setformaemissao';
  CMetodoSetTipoImpressao =            'settipoimpressao';
  CMetodoSetversaodf =                 'setversaodf';
  CMetodoSetTipoContribuinte =         'settipocontribuinte';
  CMetodoLermdfe =                     'lermdfe';
  CMetodoMdfetotxt =                   'mdfetotxt';
  CMetodoFileexist =                   'fileexists';
  CMetodoCertificadodatavencimento =   'certificadodatavencimento';
  CMetodoGerarchave =                  'gerarchave';
  CMetodoVersao =                      'versao';
  CMetodoSavetofile =                  'savetofile';
  CMetodoLoadfromfile =                'loadfromfile';
  CMetodoLerini =                      'lerini';
  CMetodoSetcertificado =              'setcertificado';
  CMetodoDistribuicaoDFeporChaveMDFe = 'distribuicaodfeporchavemdfe';
  CMetodoGetPathMDFe =                 'getpathmdfe';
  CMetodoRestaurar =                   'restaurar';
  CMetodoOcultar =                     'ocultar';
  CMetodoEncerrarmonitor =             'encerrarmonitor';
  CMetodoAtivo =                       'ativo';
  CMetodoDatahora =                    'datahora';
  CMetodoData =                        'data';
  CMetodoHora =                        'hora';
  CMetodoExit =                        'exit';
  CMetodoBye  =                        'bye';
  CMetodoFim  =                        'fim';
  CMetodoSair =                        'sair';
  CMetodoObterCertificados =           'obtercertificados';
  CMetodoRun =                         'run';
  CMetodoSendKeys =                    'sendkeys';
  CMetodoAppActivate =                 'appactivate';
  CMetodoAppExists =                   'appexists';
  CMetodoBlockInput =                  'blockinput';
  CMetodoFileExists =                  'filesexists';
  CMetodoCopyFile =                    'copyfile';
  CMetodoDeleteFiles =                 'deletefiles';
  CMetodoAjustaLnhasLog =              'ajustalinhaslog';
  CMetodoSetWebservice =               'setwebservice';
  CMetodoEncodeBase64 =                'encodebase64';
  CMetodoDecodeBase64 =                'decodebase64';
  CMetodoRoundABNT =                   'roundabnt';
  CMetodoSetTimeZone =                 'settimezone';

  CMetodoValidarnfe =                  'validarnfe';
  CMetodoAssinarnfe =                  'assinarnfe';
  CMetodoConsultarnfe =                'consultarnfe';
  CMetodoCancelarnfe =                 'cancelarnfe';
  CMetodoImprimirdanfe =               'imprimirdanfe';
  CMetodoImprimirdanfepdf =            'imprimirdanfepdf';
  CMetodoEnviarnfe =                   'enviarnfe';
  CMetodoCriarnfe =                    'criarnfe';
  CMetodoCriarenviarnfe =              'criarenviarnfe';
  CMetodoAdicionarnfe =                'adicionarnfe';
  CMetodoEnviarlotenfe =               'enviarlotenfe';
  CMetodoRecibonfe =                   'recibonfe';
  CMetodoInutilizarnfe =               'inutilizarnfe';
  CMetodoLernfe =                      'lernfe';
  CMetodonfetotxt =                    'nfetotxt';
  CMetodoValidarRegrasNegocios =       'validarnferegranegocios';
  CMetodoImprimirInutilizacao =        'imprimirinutilizacao';
  CMetodoImprimirInutilizacaoPDF =     'imprimirinutilizacaopdf';
  CMetodoEnviarEvento =                'enviarevento';
  CMetodoCartaCorrecao =               'cartadecorrecao';
  CMetodoXMLEnviarEvento =             'xmlenviarevento';
  CMetodoDistribuicaoDFeporChaveNFe =  'distribuicaodfeporchavenfe';
  CMetodoDistribuicaoDFeporNSU =       'distribuicaodfepornsu';
  CMetodoDistribuicaoDFeporUltNSU =    'distribuicaodfeporultnsu';
  CMetodoEnviaremailEvento =           'enviaremailevento';
  CMetodoEnviaremailInutilizacao =     'enviaremailinutilizacao';
  CMetodoSetModeloDF =                 'setmodelodf';
  CMetodoSetToken =                    'settoken';
  CMetodoSetCSC =                      'setcsc';
  CMetodoSetIdToken =                  'setidtoken';
  CMetodoSetIdCSC =                    'setidcsc';
  CMetodoGerarININFe =                 'gerarininfe';
  CMetodoCNPJCertificado =             'cnpjcertificado';
  CMetodoGetPathNFe =                  'getpathnfe';
  CMetodoGetPathCCe =                  'getpathcce';
  CMetodoGetPathCan =                  'getpathcan';
  CMetodoGetPathEvento =               'getpathevento';
  CMetodoGetPathInu =                  'getpathinu';
  CMetodoImprimirRelatorio =           'imprimirrelatorio';
  CMetodoCriarNFeSEFAZ =               'criarnfesefaz';
  CMetodoCriarEnviarNFeSEFAZ =         'criarenviarnfesefaz';
  CMetodoAdicionarNFeSEFAZ =           'adicionarnfesefaz';
  CMetodoDistribuicaoDFe =             'distribuicaodfe';
  CMetodoDataVencimentoCertificado =   'datavencimentocertificado';

  CMetodoCriarEnviarRPS =              'CriarEnviarRPS';
  CMetodoAdicionarRPS =                'AdicionarRPS';
  CMetodoLimparLoteRPS =               'LimparLoteRPS';
  CMetodoTotalRPSLote =                'TotalRPSLote';
  CMetodoEnviarLoteRPS =               'EnviarLoteRPS';
  CMetodoGerarLoteRPS =                'GerarLoteRPS';
  CMetodoConsultarSituacaoLote =       'ConsultarSituacaoLote';
  CMetodoConsultarLote =               'ConsultarLote';
  CMetodoConsultarNFSeporRPS =         'ConsultarNFSeporRPS';
  CMetodoConsultarNFSeporNumero =      'ConsultarNFSeporNumero';
  CMetodoConsultarNFSeporPeriodo =     'ConsultarNFSeporPeriodo';
  CMetodoConsultarNFSeporFaixa =       'ConsultarNFSeporFaixa';
  CMetodoConsultarNFSeGenerico =       'ConsultarNFSeGenerico';
  CMetodoConsultarLinkNFSe =           'ConsultarLinkNFSe';
  CMetodoConsultarNFSeServicoPrestadoPorNumero = 'ConsultarNFSeServicoPrestadoPorNumero';
  CMetodoConsultarNFSeServicoPrestadoPorTomador = 'ConsultarNFSeServicoPrestadoPorTomador';
  CMetodoConsultarNFSeServicoPrestadoPorIntermediario = 'ConsultarNFSeServicoPrestadoPorIntermediario';
  CMetodoConsultarNFSeServicoPrestadoPorPeriodo = 'ConsultarNFSeServicoPrestadoPorPeriodo';
  CMetodoConsultarNFSeServicoTomadoPorNumero = 'ConsultarNFSeServicoTomadoPorNumero';
  CMetodoConsultarNFSeServicoTomadoPorPrestador = 'ConsultarNFSeServicoTomadoPorPrestador';
  CMetodoConsultarNFSeServicoTomadoPorTomador = 'ConsultarNFSeServicoTomadoPorTomador';
  CMetodoConsultarNFSeServicoTomadoPorIntermediario = 'ConsultarNFSeServicoTomadoPorIntermediario';
  CMetodoConsultarNFSeServicoTomadoPorPeriodo = 'ConsultarNFSeServicoTomadoPorPeriodo';
  CMetodoCancelarNFSe =                'CancelarNFSe';
  CMetodoLinkNFSe =                    'LinkNFSe';
  CMetodoSubstituirNFSe =              'SubstituirNFSe';
  CMetodoEnviarEmailNFSe =             'EnviarEmailNFSe';
  CMetodoImprimirNFSe =                'ImprimirNFSe';
  CMetodoImprimirPDFNFSe =             'ImprimirPDFNFSe';
  // Utilizado por alguns provedores
  CMetodoGerarTokenNFSe =              'GerarTokenNFSe';
  // Utilizados pelo Padrão Nacional
  CMetodoConsultarDPSPorChave =        'ConsultarDPSPorChave';
  CMetodoConsultarNFSePorChave =       'ConsultarNFSePorChave';
  CMetodoObterDANFSE =                 'ObterDANFSE';
  CMetodoEnviarEventoNFSe =            'EnviarEventoNFSe';
  CMetodoConsultarEventoNFSe =         'ConsultarEventoNFSe';
  CMetodoConsultarDFeNFSePorNSU =      'ConsultarDFeNFSePorNSU';
  CMetodoConsultarDFeNFSePorChave =    'ConsultarDFeNFSePorChave';
  CMetodoConsultarParametrosNFSe =     'ConsultarParametrosNFSe';
  CMetodoObterInformacoesProvedor =    'ObterInformacoesProvedor';
  CMetodoSetLayoutNFSe = 'SetLayoutNFSe';
  CMetodoSetCodigoMunicipio = 'SetCodigoMunicipio';
  CMetodoSetEmitente = 'SetEmitente';
  CMetodoSetAutenticacaoNFSe = 'SetAutenticacaoNFSe';

  CMetodoValidarCTe =                  'validarcte';
  CMetodoAssinarCTe =                  'assinarcte';
  CMetodoConsultarCTe =                'consultarcte';
  CMetodoCancelarCTe =                 'cancelarcte';
  CMetodoImprimirDACTe =               'imprimirdacte';
  CMetodoImprimirDACTePDF =            'imprimirdactepdf';

  CMetodoEnviarCTe =                   'enviarcte';
  CMetodoCriarCTe =                    'criarcte';
  CMetodoCriarEnviarCTe =              'criarenviarcte';
  CMetodoAdicionarCTe =                'adicionarcte';
  CMetodoEnviarLoteCTe =               'enviarlotecte';
  CMetodoReciboCTe =                   'recibocte';
  CMetodoInutilizarCTe =               'inutilizarcte';
  CMetodoLerCTe =                      'lercte';
  CMetodoCTetotxt =                    'ctetotxt';
  CMetodoDistribuicaoDFeporChaveCTe =  'distribuicaodfeporchavecte';
  CMetodoGerarINICTe =                 'gerarinicte';
  CMetodoGetPathCTe =                  'getpathcte';

  CMetodoValidarBPe =                  'validarbpe';
  CMetodoAssinarBPe =                  'assinarbpe';
  CMetodoConsultarBPe =                'consultarbpe';
  CMetodoCancelarBPe =                 'cancelarbpe';
  CMetodoImprimirDABPe =               'imprimirdabpe';
  CMetodoImprimirDABPePDF =            'imprimirdabpepdf';

  CMetodoEnviarBPe =                   'enviarbpe';
  CMetodoCriarBPe =                    'criarbpe';
  CMetodoCriarEnviarBPe =              'criarenviarbpe';
  CMetodoLerBPe =                      'lerbpe';
  CMetodoGetPathBPe =                  'getpathbpe';
  CMetodoGerarINIBPe =                 'gerarinibpe';
  CMetodoDistribuicaoDFeporChaveBPe =  'distribuicaodfeporchavebpe';

  CMetodoCriarEventoeSocial =          'criareventoesocial';
  CMetodoCriarEnviareSocial =          'criarenviaresocial';
  CMetodoEnviareSocial =               'enviaresocial';
  CMetodoConsultareSocial =            'consultaresocial';
  CMetodoLimpareSocial =               'limparesocial';
  CMetodoCarregarXMLEventoeSocial =    'carregarxmleventoesocial';
  CMetodoSetIDEmpregadoreSocial =      'setidempregador';
  CMetodoSetTipoEmpregadoreSocial =    'settipoempregador';
  CMetodoSetIDTransmissoresocial =     'setidtransmissor';
  CMetodoConsultaIdentEventosEmpreg =  'ConsultaIdentificadoresEventosEmpregador';
  CMetodoConsultaIdentEventosTabela =  'ConsultaIdentificadoresEventosTabela';
  CMetodoConsultaIdentEventosTrab =    'ConsultaIdentificadoresEventosTrabalhador';
  CMetodoDownloadEventos =             'DownloadEventos';
  CMetodoValidareSocial =              'validaresocial';

  CMetodoCriarEventoReinf =          'criareventoreinf';
  CMetodoCriarEnviarReinf =          'criarenviarreinf';
  CMetodoEnviarReinf =               'enviarreinf';
  CMetodoConsultarReinf =            'consultarreinf';
  CMetodoLimparReinf =               'limparreinf';
  CMetodoCarregarXMLEventoReinf =    'carregarxmleventoreinf';
  CMetodoSetIDContribuinteReinf =    'setidcontribuinte';
  CMetodoSetIDTransmissorReinf =     'setidtransmissor';
  CMetodoConsultarReciboReinf =      'consultarreciboreinf';

  CMetodoConfigurarDados =           'configurardados';
  CMetodoLimparLista =               'limparlista';
  CMetodoTotalTitulosLista =         'totaltituloslista';
  CMetodoImprimir =                  'imprimir';
  CMetodoGerarPDF =                  'gerarpdf';
  CMetodoGerarHTML =                 'gerarhtml';
  CMetodoGerarRemessa =              'gerarremessa';
  CMetodoLerRetorno =                'lerretorno';
  CMetodoIncluirTitulos =            'incluirtitulos';
  CMetodoSetDiretorioArquivo =       'setdiretorioarquivo';
  CMetodoListaBancos =               'listabancos';
  CMetodoListaCaractTitulo =         'listacaracttitulo';
  CMetodoListaOcorrencias =          'listaocorrencias';
  CMetodoListaOcorrenciasEX =        'listaocorrenciasex';
  CMetodoTamNossoNumero =            'tamnossonumero';
  CMetodoCodigosMoraAceitos =        'codigosmoraaceitos';
  CMetodoSelecionaBanco =            'selecionabanco';
  CMetodoMontarNossoNumero =         'montarnossonumero';
  CMetodoRetornaLinhaDigitavel =     'retornalinhadigitavel';
  CMetodoRetornaCodigoBarras =       'retornacodigobarras';
  CMetodoImprimirBoleto =            'imprimirboleto';
  CMetodoGerarPDFBoleto =            'gerarpdfboleto';
  CMetodoEnviarEmailBoleto =         'enviaremailboleto';
  CMetodoEnviarBoleto =              'enviarboleto';
  CMetodoSetOperacaoWS =             'setoperacaows';
  CMetodoConsultarTitulosPorPeriodo= 'consultartitulosporperiodo';
  CMetodoGerarPDFComSenha=           'gerarpdfcomsenha';
  CMetodoGerarPDFBoletoComSenha =    'gerarpdfboletocomsenha';
  CMetodoSetMotorBoletoRelatorio =   'setmotorboletorelatorio';
  CMetodoSetMargem  =                'setmargem';

  CMetodoAtivar =               'ativar';
  CMetodoDesativar =            'desativar';
  CMetodoModeloStr =            'modelostr';
  CMetodoModelo =               'modelo';
  CMetodoPorta =                'porta';
  CMetodoIntervalo =            'intervalo';
  CMetodoSetIntervalo =         'setintervalor';
  CMetodoLePeso =               'lepeso';
  CMetodoUltimoPesoLido =       'ultimopesolido';
  CMetodoUltimaResposta =       'ultimaresposta';
  CMetodoMonitorarBalanca =     'monitorarbalanca';

  CMetodoNovo =                 'novo';
  CMetodoAdicionaPara =         'adicionapara';
  CMetodoAdicionaResponderA =   'adicionarespondera';
  CMetodoAdicionaCC =           'adicionacc';
  CMetodoAdicionaBCC =          'adicionabcc';
  CMetodoAssunto =              'assunto';
  CMetodoConfirmarLeitura =     'confirmarleitura';
  CMetodoUsarHTML =             'usarhtml';
  CMetodoTentativasEnvio =      'tentativasenvio';
  CMetodoSetPrioridade =        'setprioridade';
  CMetodoSetCodificacao =       'setcodificacao';
  CMetodoTextoMensagem =        'textomensagem';
  CMetodoTextoAlternativo =     'textoalternativo';
  CMetodoAdicionaAnexo =        'adicionaanexo';
  CMetodoEnviar =               'enviar';

  CMetodoBuscarPorCEP =         'buscarporcep';
  CMetodoBuscarPorLogradouro =  'buscarporlogradouro';

  CMetodoChequePronto    = 'chequepronto';
  CMetodoBanco           = 'banco';
  CMetodoSetBanco        = 'setbanco';
  CMetodoCidade          = 'cidade';
  CMetodoSetCidade       = 'setcidade';
  CMetodoFavorecido      = 'favorecido';
  CMetodoSetFavorecido   = 'setfavorecido';
  CMetodoObservacao      = 'observacao';
  CMetodoSetObservacao   = 'setobservacao';
  CMetodoValor           = 'valor';
  CMetodoSetValor        = 'setvalor';
  CMetodoSetData         = 'setdata';
  CMetodoBomPara         = 'bompara';
  CMetodoSetBomPara      = 'setbompara';
  CMetodoImprimirCheque  = 'imprimircheque';
  CMetodoTravarCheque    = 'travarcheque';
  CMetodoDestravarCheque = 'destravarcheque';
  CMetodoCMC7            = 'cmc7';
  CMetodoImprimirLinha   = 'imprimirlinha';
  CMetodoImprimirVerso   = 'imprimirverso';

  CMetodoAbreGaveta           = 'abregaveta';
  CMetodoGavetaAberta         = 'gavetaaberta';
  CMetodoStrComando           = 'strcomando';
  CMetodoSetStrComando        = 'setstrcomando';
  CMetodoAberturaIntervalo    = 'aberturaintervalo';
  CMetodoSetAberturaIntervalo = 'setaberturaintervalo';
  CMetodoAberturaAntecipada   = 'aberturaantecipada';

  CMetodoBuscarPorCodigo    = 'buscarporcodigo';
  CMetodoBuscarPorNome      = 'buscarpornome';
  CMetodoBuscarPorDescricao = 'buscarpordescricao';

  CMetodoValidar      = 'validar';
  CMetodoObterNCMs    = 'obterncms';
  CMetodoBaixarLista  = 'baixarlista';
  CMetodoDescricaoNCM = 'descricaoncm';

  CMetodoLerFila            = 'lerfila';
  CMetodoApagarFila         = 'apagarfila';
  CMetodoFilaCount          = 'filacount';
  CMetodoPrefixoaExcluir    = 'prefixoaexcluir';
  CMetodoSetPrefixoaExcluir = 'setprefixoaexcluir';
  CMetodoSufixo             = 'sufixo';
  CMetodoSetSufixo          = 'setsufixo';
  CMetodoExcluirSufixo      = 'excluirsufixo';
  CMetodoSetExcluirSufixo   = 'setexcluirsufixo';
  CMetodoUsarFila           = 'usarfila';
  CMetodoSetUsarFila        = 'setusarfila';
  CMetodoFilaMaxItens       = 'filamaxitens';
  CMetodoSetFilaMaxItens    = 'setfilamaxitens';
  CMetodoUltimaLeitura      = 'ultimaleitura';
  CMetodoUltimoCodigo       = 'ultimocodigo';
  CMetodoEnviarString       = 'enviarstring';
  CMetodoLerString          = 'lerstring';

  CMetodoTrabalhando      = 'trabalhando';
  CMetodoLinhasCount      = 'linhascount';
  CMetodoSetLinhasCount   = 'setlinhascount';
  CMetodoColunas          = 'colunas';
  CMetodoSetColunas       = 'setcolunas';
  CMetodoAlinhamento      = 'alinhamento';
  CMetodoSetAlinhamento   = 'setalinhamento';
  CMetodoPassos           = 'passos';
  CMetodoSetPassos        = 'setpassos';
  CMetodoLimparDisplay    = 'limpardisplay';
  CMetodoEscrever         = 'escrever';
  CMetodoPosicionarCursor = 'posicionarcursor';
  CMetodoParar            = 'parar';
  CMetodoContinuar        = 'continuar';
  CMetodoPararLinha       = 'pararlinha';
  CMetodoContinuarLinha   = 'continuarlinha';
  CMetodoExibirLinha      = 'exibirlinha';
  CMetodoRolarLinha       = 'rolarlinha';

  CMetodoConsultar = 'consultar';
  CMetodoRastrear  = 'rastrear';
  CMetodoConsultarCaptcha = 'consultarcaptcha';
  CMetodoSetProvedor       = 'setprovedor';

  CMetodoSetPorta          = 'setporta';
  CMetodoTemperatura       = 'temperatura';
  CMetodoSetTemperatura    = 'settemperatura';
  CMetodoIniciarEtiqueta   = 'iniciaretiqueta';
  CMetodoFinalizarEtiqueta = 'finalizaretiqueta';
  CMetodoAvanco            = 'avanco';
  CMetodoSetAvanco         = 'setavanco';
  CMetodoUnidade           = 'unidade';
  CMetodoSetUnidade        = 'setunidade';
  CMetodoDPI               = 'dpi';
  CMetodoSetDPI            = 'setdpi';
  CMetodoOrigem            = 'origem';
  CMetodoSetOrigem         = 'setorigem';
  CMetodoBackFeed          = 'backfeed';
  CMetodoSetBackFeed       = 'setbackfeed';
  CMetodoVelocidade        = 'velocidade';
  CMetodoSetVelocidade     = 'setvelocidade';
  CMetodoMargemEsquerda    = 'margemesquerda';
  CMetodoSetMargemEsquerda = 'setmargemesquerda';
  CMetodoImprimirTexto     = 'imprimirtexto';
  CMetodoImprimirBarras    = 'imprimirbarras';
  CMetodoImprimirCaixa     = 'imprimircaixa';
  CMetodoImprimirImagem    = 'imprimirimagem';
  CMetodoCarregarImagem    = 'carregarimagem';
  CMetodoLimparMemoria     = 'limparmemoria';
  CMetodoSetLimparMemoria  = 'setlimparmemoria';
  CMetodoImprimirQRCode    = 'imprimirqrcode';

  CMetodoConsultaConfig  = 'consultaconfig';
  CMetodoImprimirGNRe    = 'imprimirgnre';
  CMetodoImprimirGNRePDF = 'imprimirgnrepdf';
  CMetodoGerarGuia       = 'gerarguia';
  CMetodoGerarXMLGNRe    = 'gerarxml';

  CMetodoImprimirCMD            = 'imprimircmd';
  CMetodoImprimirTAGs           = 'imprimirtags';
  CMetodoLerStatusImpressora    = 'lerstatusimpressora';
  CMetodoLerInfoImpressora      = 'lerinfoimpressora';
  CMetodoSetModelo              = 'setmodelo';
  CMetodoEspacoEntreLinhas      = 'espacoentrelinhas';
  CMetodoSetEspacoEntreLinhas   = 'setespacoentrelinhas';
  CMetodoLinhasEntreCupons      = 'linhasentrecupos';
  CMetodoSetLinhasEntreCupons   = 'setlinhasentrecupos';
  CMetodoLinhasBuffer           = 'linhasbuffer';
  CMetodoSetLinhasBuffer        = 'setlinhasbuffer';
  CMetodoColunasFonteExpandida  = 'colunasfonteexpandida';
  CMetodoColunasFonteCondensada = 'colunasfontecondensada';
  CMetodoPaginaDeCodigo         = 'paginadecodigo';
  CMetodoSetPaginaDeCodigo      = 'setpaginadecodigo';
  CMetodoColunasFonteNormal     = 'colunasfontenormal';
  CMetodoSetColunasFonteNormal  = 'setcolunasfontenormal';
  CMetodoCortaPapel             = 'cortapapel';
  CMetodoSetCortaPapel          = 'setcortapapel';
  CMetodoImprimirImagemArquivo  = 'imprimirimagemarquivo';
  CMetodoImprimirLogo           = 'imprimirlogo';
  CMetodoGravarLogoArquivo      = 'gravarlogoarquivo';
  CMetodoApagarLogo             = 'apagarlogo';
  CMetodoAcharPortasSeriais     = 'acharportasseriais';
  CMetodoAcharPortasUSB         = 'acharportasusb';
  CMetodoAcharPortasRAW         = 'acharportasraw';

  CExtensaoXML =                     '.xml';

  CExtensaoXmlNFe =                  '-nfe.xml';
  CExtensaoXmlNFeEve =               '-eve.xml';
  CExtensaoXmlNFeInu =               '-inu.xml';

  CExtensaoXmlCTe =                  '-cte.xml';
  CExtensaoXmlCTeEve =               '-eve.xml';
  CExtensaoXmlCTeInu =               '-inu.xml';

  CExtensaoXmlMdfe =                 '-mdfe.xml';
  CExtensaoXmlMdfeEve =              '-eve.xml';
  CExtensaoXmlGNRe =                 '-gnre.xml';

  CExtensaoXmlBPe =                 '-bpe.xml';
  CExtensaoXmlBPeEve =              '-eve.xml';

  CExtensaoXmlNFSe =                 '-nfse.xml';

  CPathLogs =                        'Logs';

  CSecACBrMonitor =                  'ACBrMonitor';
  CKeyModo_TCP =                     'Modo_TCP';
  CKeyModo_TXT =                     'Modo_TXT';
  CKeyMonitorarPasta =               'MonitorarPasta';
  CKeyTCP_Porta =                    'TCP_Porta';
  CKeyTCP_TimeOut =                  'TCP_TimeOut';
  CKeyConverte_TCP_Ansi =            'Converte_TCP_Ansi';
  CKeyTXT_Entrada =                  'TXT_Entrada';
  CKeyTXT_Saida =                    'TXT_Saida';
  CKeyConverte_TXT_Entrada_Ansi =    'Converte_TXT_Entrada_Ansi';
  CKeyConverte_TXT_Saida_Ansi =      'Converte_TXT_Saida_Ansi';
  CKeyIntervalo =                    'Intervalo';
  CKeyGravar_Log =                   'Gravar_Log';
  CKeyArquivo_Log =                  'Arquivo_Log';
  CKeyLinhas_Log =                   'Linhas_Log';
  CKeyComandos_Remotos =             'Comandos_Remotos';
  CKeyUma_Instancia =                'Uma_Instancia';
  CKeyMostraAbas =                   'MostraAbas';
  CKeyMostrarNaBarraDeTarefas =      'MostrarNaBarraDeTarefas';
  CKeyRetirarAcentosNaResposta =     'RetirarAcentosNaResposta';
  CKeyMostraLogEmRespostasEnviadas = 'MostraLogEmRespostasEnviadas';
  CKeyHashSenha =                    'HashSenha';
  CKeyMonitorSenha =                 'Senha';
  CKeyVersaoSSL =                    'VersaoSSL';
  CKeyTipoResposta =                 'TipoResposta';

  CSecECF =                          'ECF';
  CKeyModelo =                       'Modelo';
  CKeyPorta =                        'Porta';
  CKeySerialParams =                 'SerialParams';
  CKeyTimeout =                      'Timeout';
  CKeyIntervaloAposComando =         'IntervaloAposComando';
  CKeyMaxLinhasBuffer =              'MaxLinhasBuffer';
  CKeyPaginaCodigo =                 'PaginaCodigo';
  CKeyLinhasEntreCupons =            'LinhasEntreCupons';
  CKeyArredondamentoPorQtd =         'ArredondamentoPorQtd';
  CKeyArredondamentoItemMFD =        'ArredondamentoItemMFD';
  CKeyDescricaoGrande =              'DescricaoGrande';
  CKeyGavetaSinalInvertido =         'GavetaSinalInvertido';
  CKeyIgnorarTagsFormatacao =        'IgnorarTagsFormatacao';
  CKeyControlePorta =                'ControlePorta';
  CKeyArqLog =                       'ArqLog';

  CSecCHQ =                          'CHQ';
  CKeyCHQModelo =                    'Modelo';
  CKeyCHQPorta =                     'Porta';
  CKeyCHQSerialParams =              'SerialParams';
  CKeyCHQVerificaFormulario =        'VerificaFormulario';
  CKeyCHQFavorecido =                'Favorecido';
  CKeyCHQCidade =                    'Cidade';
  CKeyCHQPathBemafiINI =             'PathBemafiINI';

  CSecGAV =                          'GAV';
  CKeyGAVModelo =                    'Modelo';
  CKeyGAVPorta =                     'Porta';
  CKeyGAVStringAbertura =            'StringAbertura';
  CKeyGAVAberturaIntervalo =         'AberturaIntervalo';
  CKeyGAVAcaoAberturaAntecipada =    'AcaoAberturaAntecipada';

  CSecDIS =                          'DIS';
  CKeyDISModelo =                    'Modelo';
  CKeyDISPorta =                     'Porta';
  CKeyDISIntervalo =                 'Intervalo';
  CKeyDISPassos =                    'Passos';
  CKeyDISIntervaloEnvioBytes =       'IntervaloEnvioBytes';

  CSecLCB =                          'LCB';
  CKeyLCBPorta =                     'Porta';
  CKeyLCBIntervalo =                 'Intervalo';
  CKeyLCBSufixoLeitor =              'SufixoLeitor';
  CKeyLCBExcluirSufixo =             'ExcluirSufixo';
  CKeyLCBPrefixoAExcluir =           'PrefixoAExcluir';
  CKeyLCBSufixoIncluir =             'SufixoIncluir';
  CKeyLCBDispositivo =               'Dispositivo';
  CKeyLCBTeclado =                   'Teclado';
  CKeyLCBDevice =                    'Device';

  CSecRFD =                          'RFD';
  CKeyRFDGerarRFD =                  'GerarRFD';
  CKeyRFDDirRFD =                    'DirRFD';
  CKeyRFDIgnoraECF_MFD =             'IgnoraECF_MFD';

  CSecBAL =                          'BAL';
  CKeyBALModelo =                    'Modelo';
  CKeyBALPorta =                     'Porta';
  CKeyBALIntervalo =                 'Intervalo';
  CKeyBALArqLog =                    'ArqLog';
  CKeyBALDevice =                    'Device';

  CSecETQ =                          'ETQ';
  CKeyETQModelo =                    'Modelo';
  CKeyETQPorta =                     'Porta';
  CKeyETQDPI =                       'DPI';
  CKeyETQLimparMemoria =             'LimparMemoria';
  CKeyETQTemperatura =               'Temperatura';
  CKeyETQVelocidade =                'Velocidade';
  CKeyETQBackFeed =                  'BackFeed';
  CKeyETQMargemEsquerda =            'MargemEsquerda';
  CKeyETQOrigem =                    'Origem';
  CKeyETQCopias =                    'Copias';
  CKeyETQUnidade =                   'Unidade';
  CKeyETQAvanco =                    'Avanco';

  CSecCEP =                          'CEP';
  CKeyCEPWebService =                'WebService';
  CKeyCEPChave_BuscarCEP =           'Chave_BuscarCEP';
  CKeyCEPProxy_Host =                'Proxy_Host';
  CKeyCEPProxy_Port =                'Proxy_Port';
  CKeyCEPProxy_User =                'Proxy_User';
  CKeyCEPProxy_Pass =                'Proxy_Pass';
  CKeyCEPIBGEAcentos =               'IBGEAcentos';
  CKeyCEPIBGEUTF8 =                  'IBGEUTF8';

  CSecConsultaCNPJ =                 'ConsultaCNPJ';
  CKeyConsultaCNPJProvedor =         'Provedor';
  CKeyConsultaCNPJUsuario =          'Usuario';
  CKeyConsultaCNPJSenha =            'Senha';

  CSecTC =                           'TC';
  CKeyTCModelo =                     'Modelo';
  CKeyTCTCP_Porta =                  'TCP_Porta';
  CKeyTCArq_Precos =                 'Arq_Precos';
  CKeyTCNao_Econtrado =              'Nao_Econtrado';

  CSecSEDEX =                        'SEDEX';
  CKeyContratoSEDEX =                'Contrato';
  CKeySenhaSEDEX =                   'SenhaSedex';

  CSecNCM =                          'NCM';
  CKeyDirNCMSalvar =                 'DirNCMSalvar';
  CKeyDiasValidadeCache =            'DiasValidadeCache';

  CSecCertificado =                  'Certificado';
  CKeySSLLib =                       'SSLLib';
  CKeyCryptLib =                     'CryptLib';
  CKeyHttpLib =                      'HttpLib';
  CKeyXmlSignLib =                   'XmlSignLib';
  CKeySSLType =                      'SSLType';
  CKeyArquivoPFX =                   'ArquivoPFX';
  CKeyURLPFX =                       'URLPFX';
  CKeyNumeroSerie =                  'NumeroSerie';
  CKeySenha =                        'Senha';
  CKeyExibeRazaoSocialCertificado =  'ExibeRazaoSocialCertificado';
  CKeyVerificarValidade =            'VerificarValidade';


  CSecACBrNFeMonitor =                'ACBrNFeMonitor';
  CKeyIgnorarComandoModoEmissao =     'IgnorarComandoModoEmissao';
  CKeyModoXML =                       'ModoXML';
  CKeyRetirarAcentos =                'RetirarAcentos';
  CKeyRetirarEspacos =                'RetirarEspacos';
  CKeyGravar_Log_Comp =               'Gravar_Log_Comp';
  CKeyArquivo_Log_Comp =              'Arquivo_Log_Comp';
  CKeyLinhas_Log_Comp =               'Linhas_Log_Comp';
  CKeyArquivoWebServices =            'ArquivoWebServices';
  CKeyArquivoWebServicesCTe =         'ArquivoWebServicesCTe';
  CKeyArquivoWebServicesMDFe =        'ArquivoWebServicesMDFe';
  CKeyArquivoWebServicesGNRe =        'ArquivoWebServicesGNRe';
  CKeyArquivoWebServiceseSocial =     'ArquivoWebServiceseSocial';
  CKeyArquivoWebServicesReinf =       'ArquivoWebServicesReinf';
  CKeyArquivoWebServicesBPe =         'ArquivoWebServicesBPe';
  CKeyArquivoWebServicesNFSe =        'ArquivoWebServicesNFSe';
  CKeyValidarDigest =                 'ValidarDigest';
  CKeyTimeoutWebService =             'TimeoutWebService';

  CSecGeral =                        'Geral';
  CKeyDANFE =                        'DANFE';
  CKeyFormaEmissao =                 'FormaEmissao';
  CKeyLogomarca =                    'Logomarca';
  CKeyLogoMarcaNFCeSAT =             'LogoMarcaNFCeSAT';
  CKeyLogoMarcaPrefeitura =          'LogoMarcaPrefeitura';
  CKeySalvar =                       'Salvar';
  CKeyPathSalvar =                   'PathSalvar';
  CKeyImpressora =                   'Impressora';

  CSecWebService =                   'WebService';
  CKeyVersao =                       'Versao';
  CKeyVersaoCTe =                    'VersaoCTe';
  CKeyVersaoMDFe =                   'VersaoMDFe';
  CKeyVersaoeSocial =                'VersaoeSocial';
  CKeyVersaoReinf =                  'VersaoReinf';
  CKeyVersaoQRCode =                 'VersaoQRCode';
  CKeyVersaoBPe =                    'VersaoBPe';
  CKeyVersaoGNRe =                   'VersaoGNRe';
  CKeyFormaEmissaoCTe =              'FormaEmissaoCTe';
  CKeyFormaEmissaoNFe =              'FormaEmissaoNFe';
  CKeyFormaEmissaoMDFe =             'FormaEmissaoMDFe';
  CKeyFormaEmissaoGNRe =             'FormaEmissaoGNRe';
  CKeyFormaEmissaoBPe =              'FormaEmissaoBPe';
  CKeyUF =                           'UF';
  CKeyAmbiente =                     'Ambiente';
  CKeyAjustarAut =                   'AjustarAut';
  CKeyAguardar =                     'Aguardar';
  CKeyTentativas =                   'Tentativas';
  CKeyWebServiceIntervalo =          'Intervalo';
  CKeyTimeZoneMode =                 'TimeZoneMode';
  CKeyTimeZoneStr =                  'TimeZoneStr';
  CKeyCamposFatObrig =               'CamposFatObrig';
  CKeyTagRejeicao938 =               'ForcarGerarTagRejeicao938';
  CKeyTagQRCodeCTe =                 'TagQRCodeCTe';

  CKeyIdEmpregador =                 'IdEmpregador';
  CKeyIdTransmissor =                'IdTransmissor';
  CKeyTipoEmpregador =               'TipoEmpregador';
  CKeyIdContribuinte =               'IdContribuinte';
  CKeyTipoContribuinte =             'TipoContribuinte';

  CSecRespTecnico =                  'RespTecnico';
  CKeyCSRT =                         'CSRT';
  CKeyidCSRT =                       'idCSRT';

  CSecProxy =                        'Proxy';
  CKeyProxyHost =                    'Host';
  CKeyProxyPorta =                   'Porta';
  CKeyProxyUser =                    'User';
  CKeyProxyPass =                    'Pass';

  CSecEmail =                        'Email';
  CKeyEmailNomeExibicao =            'NomeExibicao';
  CKeyEmailEndereco =                'Endereco';
  CKeyEmail =                        'Email';
  CKeyEmailUsuario =                 'Usuario';
  CKeyEmailSenha =                   'Senha';
  CKeyEmailPorta =                   'Porta';
  CKeyEmailExigeSSL =                'ExigeSSL';
  CKeyEmailExigeTLS =                'ExigeTLS';
  CKeyEmailConfirmacao =             'Confirmacao';
  CKeyEmailSegundoPlano =            'SegundoPlano';
  CKeyEmailCodificacao =             'Codificacao';
  CKeyEmailHTML =                    'HTML';
  CKeyEmailSSLType =                 'SSLType';

  CKeyAttemptsMail =                 'AttemptsMail';
  CKeyTimeoutMail =                  'TimeOutMail';
  CKeyMensagemNFe =                  'MensagemNFe';
  CKeyMensagemCTe =                  'MensagemCTe';
  CKeyMensagemMDFe =                 'MensagemMDFe';
  CKeyMensagemBPe =                  'AssuntoBPe';
  CKeyAssuntoNFe =                   'AssuntoNFe';
  CKeyAssuntoCTe =                   'AssuntoCTe';
  CKeyAssuntoMDFe =                  'AssuntoMDFe';
  CKeyAssuntoBPe =                   'AssuntoBPe';
  CKeyMensagemNFSe =                 'MensagemNFSe';
  CKeyAssuntoNFSe =                  'AssuntoNFSe';

  CSecNFe =                          'NFe';
  CKeyNFeCNPJContador =              'CNPJContador';

  CSecNFCe =                           'NFCe';
  CKeyNFCeIdToken =                    'IdToken';
  CKeyNFCeToken =                      'Token';
  CKeyNFCeTagQrCode =                  'TagQrCode';
  CKeyNFCeModelo =                     'Modelo';
  CKeyNFCeModoImpressaoEvento =        'ModoImpressaoEvento';
  CKeyNFCeImprimirItem1Linha =         'ImprimirItem1Linha';
  CKeyNFCeImprimirDescAcresItem =      'ImprimirDescAcresItem';
  CKeyNFCeImpressoraPadrao =           'ImpressoraPadrao';
  CKeyNFCeQRCodeLateral =              'QRCodeLateral';
  CKeyNFCeUsaCodigoEanImpressao =      'UsaCodigoEanImpressao';
  CKeyNFCeImprimeNomeFantasia =        'ImprimeNomeFantasia';
  CKeyNFCeUsarIntegrador =             'UsarIntegrador';
  CKeyNFCEImprimeTributos =            'ImprimeTributos';
  CKeyNFCeExibeTotalTributosItem =     'ExibeTotalTributosItem';
  CKeyNFCeLogoLateral =                'LogoLateral';
  CKeyNFCeImprimeItens =               'ImprimeItens';

  CSecDANFE =                          'DANFE';
  CKeyDANFEModelo =                    'Modelo';
  CKeyDANFETamanhoPapel =              'TamanhoPapel';
  CKeyDANFESite =                      'Site';
  CKeyDANFEEmail =                     'Email';
  CKeyDANFEFax =                       'Fax';
  CKeyDANFEImpDescPorc =               'ImpDescPorc';
  CKeyDANFEMostrarPreview =            'MostrarPreview';
  CKeyDANFECopias =                    'Copias';
  CKeyDANFECopiasNFCe =                'CopiasNFCe';
  CKeyDANFELarguraCodigoProduto =      'LarguraCodigoProduto';
  CKeyDANFEEspacoEntreProdutos =       'EspacoEntreProdutos';
  CKeyDANFEFonteRazao =                'FonteRazao';
  CKeyDANFEFonteEndereco =             'FonteEndereco';
  CKeyDANFEFonteCampos =               'FonteCampos';
  CKeyDANFEFonteAdicionais =           'FonteAdicionais';
  CKeyDANFEAlturaCampos =              'AlturaCampos';
  CKeyDANFEMargem =                    'Margem';
  CKeyDANFEMargemSup =                 'MargemSup';
  CKeyDANFEMargemDir =                 'MargemDir';
  CKeyDANFEMargemEsq  =                'MargemEsq';
  CKeyDANFEPathPDF =                   'PathPDF';
  CKeyDANFEDecimaisQTD =               'DecimaisQTD';
  CKeyDANFEDecimaisValor =             'DecimaisValor';
  CKeyDANFEExibeResumo =               'ExibeResumo';
  CKeyDANFETextoResumoCanhoto =        'TextoResumoCanhoto';
  CKeyDANFEImprimirTributosItem =      'ImprimirTributosItem';
  CKeyDANFEImprimirValLiq =            'ImprimirValLiq';
  CKeyDANFEUNComercialETributavel =    'UNComercialETributavel';
  CKeyDANFEPreImpresso =               'PreImpresso';
  CKeyDANFEMostrarStatus =             'MostrarStatus';
  CKeyDANFEExibirEAN =                 'ExibirEAN';
  CKeyDANFEExibirCampoFatura =         'ExibirCampoFatura';
  CKeyDANFEExpandirLogo =              'ExpandirLogo';
  CKeyDANFEFonte =                     'Fonte';
  CKeyDANFELocalCanhoto =              'LocalCanhoto';
  CKeyDANFELayoutCanhoto =             'LayoutCanhoto';
  CKeyDANFEQuebrarLinhasDetalheItens =     'QuebrarLinhasDetalheItens';
  CKeyDANFEImprimirDetalhamentoEspecifico ='ImprimirDetalhamentoEspecifico';
  CKeyDANFEImprimirDadosDocReferenciados = 'ImprimirDadosDocReferenciados';
  CKeyDANFEExibirBandInforAdicProduto =    'ExibirBandInforAdicProduto';
  CKeyDANFELogoEmCima =                    'LogoEmCima';
  CKeyDANFEImprimeInscSuframa =            'ImprimeInscSuframa';
  CKeyDANFEExpandirDadosAdicionaisAuto =   'ExpandirDadosAdicionaisAuto';
  CKeyDANFEImprimeContinuacaoDadosAdicionaisPrimeiraPagina = 'ImprimeContinuacaoDadosAdicionaisPrimeiraPagina';
  CKeyDANFEImprimeDescAcrescItemNFe =      'ImprimeDescAcrescItemNFe';
  CKeyDANFEImprimirCampoFormaPagamento =   'ImprimirCampoFormaPagamento';
  CKeyDANFEImprimeXPedNitemPed =           'ImprimeXPedNitemPed';


  CSecDANFCe =                         'DANFCe';
  CKeyDANFCeMargemInf =                'MargemInf';
  CKeyDANFCeMargemSup =                'MargemSup';
  CKeyDANFCeMargemDir =                'MargemDir';
  CKeyDANFCeMargemEsq =                'MargemEsq';
  CKeyDANFCeLarguraBobina =            'LarguraBobina';

  CSecDANFCeTipoPagto =                'DANFCeTipoPagto';
  CKeyDANFCeTipoPagtoTipo =            'Tipo';
  CKeyDANFCeTipoPagtoBandeira =        'Bandeira';
  CKeyDANFCeTipoPagtoAutorizacao =     'Autorizacao';

  CSecFonte =                          'FonteLinhaItem';
  CKeyFonteName =                      'Name';
  CKeyFonteColor =                     'Color';
  CKeyFonteSize =                      'Size';
  CKeyFonteStyleBold =                 'Bold';
  CKeyFonteStyleItalic =               'Italic';
  CKeyFonteStyleUnderline =            'Underline';
  CKeyFonteStyleStrckout =             'Strckout';

  CSecDACTE =                          'DACTE';
  CKeyDACTETamanhoPapel =              'TamanhoPapel';

  CSecDAMFE =                          'DAMFE';
  CKeyDAMFEExibirMunicipioDescar =     'ExibirMunicipioDescarregamento';

  CSecArquivos =                            'Arquivos';
  CKeyArquivosSalvar =                      'Salvar';
  CKeyArquivosPastaMensal =                 'PastaMensal';
  CKeyArquivosAddLiteral=                   'AddLiteral';
  CKeyArquivosEmissaoPathNFe =              'EmissaoPathNFe';
  CKeyArquivosSalvarCCeCanPathEvento =      'SalvarCCeCanPathEvento';
  CKeyArquivosSepararPorCNPJ =              'SepararPorCNPJ';
  CKeyArquivosSepararPorModelo =            'SepararPorModelo';
  CKeyArquivosSalvarApenasNFesAutorizadas = 'SalvarApenasNFesAutorizadas';
  CKeyArquivosAtualizarXMLCancelado =       'AtualizarXMLCancelado';
  CKeyArquivosNormatizarMunicipios =        'NormatizarMunicipios';
  CKeyArquivosUsarSeparadorPathPDF =        'UsarSeparadorPathPDF';
  CKeyArquivosSepararPorNome =              'SepararPorNome';
  CKeyArquivosPathNFe =                     'PathNFe';
  CKeyArquivosPathInu =                     'PathInu';
  CKeyArquivosPathDPEC =                    'PathDPEC';
  CKeyArquivosPathEvento =                  'PathEvento';
  CKeyArquivosPathArqTXT =                  'PathArqTXT';
  CKeyArquivosPathDownload =                'PathDonwload';

  CKeyArquivosPathSchemasDFe =              'PathSchemasDFe';
  {CKeyArquivosPathSchemaNFe =               'PathSchemaNFe';
  CKeyArquivosPathSchemaCTe =               'PathSchemaCTe';
  CKeyArquivosPathSchemaMDFe =              'PathSchemaMDFe';
  CKeyArquivosPathSchemaGNRe =              'PathSchemaGNRe';
  CKeyArquivosPathSchemaBPe =               'PathSchemaBPe';
  CKeyArquivosPathSchemaeSocial =           'PathSchemaeSocial';
  CKeyArquivosPathSchemaReInf =             'PathSchemaReInf';}

  CSeceSocial =                             'eSocial';
  CSecReinf =                               'Reinf';
  CKey =                                    'Host';

  CSecSAT =                                 'SAT';
  CKeySATModelo =                           'Modelo';
  CKeySATMarca =                            'Marca';
  CKeySATArqLog =                           'ArqLog';
  CKeySATNomeDLL =                          'NomeDLL';
  CKeySATCodigoAtivacao =                   'CodigoAtivacao';
  CKeySATCodigoUF =                         'CodigoUF';
  CKeySATNumeroCaixa =                      'NumeroCaixa';
  CKeySATAmbiente =                         'Ambiente';
  CKeySATPaginaDeCodigo =                   'PaginaDeCodigo';
  CKeySATversaoDadosEnt =                   'versaoDadosEnt';
  CKeySATFormatarXML =                      'FormatarXML';
  CKeySATPathCFe =                          'PathCFe';
  CKeySATSalvarCFe =                        'SalvarCFe';
  CKeySATSalvarCFeCanc =                    'SalvarCFeCanc';
  CKeySATSalvarEnvio =                      'SalvarEnvio';
  CKeySATSepararPorCNPJ =                   'SepararPorCNPJ';
  CKeySATSepararPorMES =                    'SepararPorMES';
  CKeySATSepararPorANO =                    'SepararPorANO';
  CKeySATSepararPorDIA =                    'SepararPorDIA';
  CKeySATSepararPorModelo =                 'SepararPorModelo';
  CKeySATValidarNumeroSessaoResposta =      'ValidarNumeroSessaoResposta';
  CKeySATPathCFeCanc =                      'PathCFeCanc';
  CKeySATPathCFeEnvio =                     'PathCFeEnvio';
  CKeySATPrefixoArqCFe =                    'PrefixoArqCFe';
  CKeySATPrefixoArqCFeCanc =                'PrefixoArqCFeCanc';
  CKeySATPastaOrigemLib =                   'PastaOrigem';
  CKeySATPastaOrigemLib64 =                 'PastaOrigem64';
  CKeySATPastaDestLib =                     'PastaDestino';
  CKeySATLibLinux =                         'LibLinux';
  CKeySATLibWin32 =                         'LibWin32';
  CKeySATLibWin64 =                         'LibWin64';
  //CKeySATHashLib =                          'Hash';

  CSecSATExtrato =                          'SATExtrato';
  CKeySATExtMostrarStatus =                 'MostrarStatus';
  CKeySATExtParamsString =                  'ParamsString';
  CKeySATExtImprimeDescAcrescItem =         'ImprimeDescAcrescItem';
  CKeySATExtImprimeEmUmaLinha =             'ImprimeEmUmaLinha';
  CKeySATExtImprimeChaveEmUmaLinha =        'ImprimeChaveEmUmaLinha';
  CKeySATExtUsaCodigoEanImpressao =         'UsaCodigoEanImpressao';
  CKeySATExtLogoLateral =                   'LogoLateral';
  CKeySATExtQRCodeLateral =                 'QRCodeLateral';
  CKeySATExtDecimaisQTD =                   'ExtratoDecimaisQTD';
  CKeySATExtDecimaisValor =                 'ExtratoDecimaisValor';
  CKeySATExtMaskQTD =                       'ExtratoMaskQTD';
  CKeySATExtMaskValor =                     'ExtratoMaskValor';
  CKeySATExtFormatoDecimal =                'FormatoDecimal';

  CSecSATEmit =                             'SATEmit';
  CKeySATEmitCNPJ =                         'CNPJ';
  CKeySATEmitIE =                           'IE';
  CKeySATEmitIM =                           'IM';
  CKeySATEmitRegTributario =                'RegTributario';
  CKeySATEmitRegTribISSQN =                 'RegTribISSQN';
  CKeySATEmitIndRatISSQN =                  'IndRatISSQN';

  CSecSATFortes =                           'SATFortes';
  CKeySATFortesUsarFortes =                 'UsarFortes';
  CKeySATFortesLargura =                    'Largura';
  CKeySATFortesMargemTopo =                 'MargemTopo';
  CKeySATFortesMargemFundo =                'MargemFundo';
  CKeySATFortesMargemEsquerda =             'MargemEsquerda';
  CKeySATFortesMargemDireita =              'MargemDireita';
  CKeySATFortesPreview =                    'Preview';

  CSecSATRede =                             'SATRede';
  CKeySATRedetipoInter =                    'tipoInter';
  CKeySATRedetipoLan =                      'tipoLan';
  CKeySATRedeSSID =                         'SSID';
  CKeySATRedeseg =                          'seg';
  CKeySATRedecodigo =                       'codigo';
  CKeySATRedelanIP =                        'lanIP';
  CKeySATRedelanMask =                      'lanMask';
  CKeySATRedelanGW =                        'lanGW';
  CKeySATRedelanDNS1 =                      'lanDNS1';
  CKeySATRedelanDNS2 =                      'lanDNS2';
  CKeySATRedeusuario =                      'usuario';
  CKeySATRedesenha =                        'senha';
  CKeySATRedeproxy =                        'proxy';
  CKeySATRedeproxy_ip =                     'proxy_ip';
  CKeySATRedeproxy_porta =                  'proxy_porta';
  CKeySATRedeproxy_user =                   'proxy_user';
  CKeySATRedeproxy_senha =                  'proxy_senha';

  CSecSATPrinter =                          'SATPrinter';
  CKeySATPrinterName =                      'Name';

  CSecSATSwH =                              'SATSwH';
  CKeySATSwHCNPJ =                          'CNPJ';
  CKeySATSwHAssinatura =                    'Assinatura';

  CSecSATEmail =                            'SATemail';
  CKeySATEmailAssunto =                     'Assunto';
  CKeySATEmailMensagem =                    'Mensagem';

  CSecSATIntegrador =                       'SATIntegrador';
  CKeySATIntegradorInput =                  'Input';
  CKeySATIntegradorOutput =                 'Output';
  CKeySATIntegradorTimeout =                'Timeout';

  CSecPosPrinter =                          'PosPrinter';
  CKeyPosPrinterModelo =                    'Modelo';
  CKeyPosPrinterPorta =                     'Porta';
  CKeyPosPrinterColunas =                   'Colunas';
  CKeyPosPrinterEspacoEntreLinhas =         'EspacoEntreLinhas';
  CKeyPosPrinterLinhasBuffer =              'LinhasBuffer';
  CKeyPosPrinterLinhasPular =               'LinhasPular';
  CKeyPosPrinterPaginaDeCodigo =            'PaginaDeCodigo';
  CKeyPosPrinterControlePorta =             'ControlePorta';
  CKeyPosPrinterCortarPapel =               'CortarPapel';
  CKeyPosPrinterTraduzirTags =              'TraduzirTags';
  CKeyPosPrinterIgnorarTags =               'IgnorarTags';
  CKeyPosPrinterArqLog =                    'ArqLog';
  CKeyPosPrinterSerialParams =              'SerialParams';

  CSecBarras =                              'Barras';
  CKeyBarrasLargura =                       'Largura';
  CKeyBarrasAltura =                        'Altura';
  CKeyBarrasHRI =                           'HRI';

  CSecQRCode =                              'QRCode';
  CKeyQRCodeTipo =                          'Tipo';
  CKeyQRCodeLarguraModulo =                 'LarguraModulo';
  CKeyQRCodeErrorLevel =                    'ErrorLevel';

  CSecLogo =                                'Logo';
  CKeyLogoImprimir =                        'Imprimir';
  CKeyLogoKC1 =                             'KC1';
  CKeyLogoKC2 =                             'KC2';
  CKeyLogoFatorX =                          'FatorX';
  CKeyLogoFatorY =                          'FatorY';

  CSecGaveta =                              'Gaveta';
  CKeyGavetaTempoON =                       'TempoON';
  CKeyGavetaTempoOFF =                      'TempoOFF';
  CKeyGavSinalInvertido =                   'SinalInvertido';

  CSecBOLETO =                              'BOLETO';
  CKeyBOLETONome =                          'Nome';
  CKeyBOLETOCNPJCPF =                       'CNPJCPF';
  CKeyBOLETOLogradouro =                    'Logradouro';
  CKeyBOLETONumero =                        'Numero';
  CKeyBOLETOBairro =                        'Bairro';
  CKeyBOLETOCodCidade =                     'CodCidade';
  CKeyBOLETOCidade =                        'Cidade';
  CKeyBOLETOCEP =                           'CEP';
  CKeyBOLETOComplemento =                   'Complemento';
  CKeyBOLETOUF =                            'UF';
  CKeyBOLETORespEmis =                      'RespEmis';
  CKeyBOLETOPessoa =                        'Pessoa';
  CKeyBOLETOCodTransmissao =                'CodTransmissao';
  CKeyBOLETOModalidade =                    'Modalidade';
  CKeyBOLETOConvenio =                      'Convenio';
  CKeyBOLETOCodigoOperacao =                'CodigoOperacao';
  CKeyBOLETOBanco =                         'Banco';
  CKeyBOLETOConta =                         'Conta';
  CKeyBOLETODigitoConta =                   'DigitoConta';
  CKeyBOLETOAgencia =                       'Agencia';
  CKeyBOLETODigitoAgencia =                 'DigitoAgencia';
  CKeyBOLETODigitoAgenciaConta =            'DigitoAgenciaConta';
  CKeyBOLETOCodCedente =                    'CodCedente';
  CKeyBOLETOLocalPagamento =                'LocalPagamento';
  CKeyBOLETODirLogos =                      'DirLogos';
  CKeyBOLETOCopias =                        'Copias';
  CKeyBOLETOPreview =                       'Preview';
  CKeyBOLETOProgresso =                     'Progresso';
  CKeyBOLETOSetup =                         'Setup';
  CKeyBOLETOAlteraEscala =                  'AlteraEscala';
  CKeyBOLETOEscala =                        'Escala';
  CKeyBOLETOLayout =                        'Layout';
  CKeyBOLETOFiltro =                        'Filtro';
  CKeyBOLETODirArquivoBoleto =              'DirArquivoBoleto';
  CKeyBOLETODirArquivoRemessa =             'DirArquivoRemessa';
  CKeyBOLETODirArquivoRetorno =             'DirArquivoRetorno';
  CKeyBOLETOCNAB =                          'CNAB';
  CKeyBOLETOLerCedenteRetorno =             'LerCedenteRetorno';
  CKeyBOLETOMostraPreviewRelRetorno =       'MostraPreviewRelRetorno';
  CKeyBOLETORemoveAcentos =                 'RemoveAcentos';
  CKeyBoletoPrefixArqRemessa =              'PrefixArqRemessa';
  CKeyBOLETOVersaoArquivo =                 'VersaoArquivo';
  CKeyBOLETOVersaoLote =                    'VersaoLote';
  CKeyBOLETOLogoEmpresa =                   'LogoEmpresa';
  CKeyBOLETOEmailAssuntoBoleto =            'EmailAssuntoBoleto';
  CKeyBOLETOEmailMensagemBoleto =           'EmailMensagemBoleto';
  CKeyBOLETOEmailFormatoHTML =              'EmailFormatoHTML';
  CKeyBOLETOImpressora =                    'Impressora';
  CKeyBOLETONomeArquivoBoleto =             'NomeArquivoBoleto';
  CKeyBOLETOTipoMotorRelatorio =            'TipoMotorRelatorio';
  CKeyBOLETOMargemInferior =                'MargemInferior';
  CKeyBOLETOMargemSuperior =                'MargemSuperior';
  CKeyBOLETOMargemEsquerda =                'MargemEsquerda';
  CKeyBOLETOMargemDireita =                 'MargemDireita';

  //Manter Compatibilidade
  CKeyBOLETOCedenteNome =                   'Cedente.Nome';
  CKeyBOLETOCedenteCNPJCPF =                'Cedente.CNPJCPF';
  CKeyBOLETOCedenteLogradouro =             'Cedente.Logradouro';
  CKeyBOLETOCedenteNumero =                 'Cedente.Numero';
  CKeyBOLETOCedenteBairro =                 'Cedente.Bairro';
  CKeyBOLETOCedenteCidade =                 'Cedente.Cidade';
  CKeyBOLETOCedenteCEP =                    'Cedente.CEP';
  CKeyBOLETOCedenteComplemento =            'Cedente.Complemento';
  CKeyBOLETOCedenteUF =                     'Cedente.UF';
  CKeyBOLETOCedenteRespEmis =               'Cedente.RespEmis';
  CKeyBOLETOCedentePessoa =                 'Cedente.Pessoa';
  CKeyBOLETOCedenteCodTransmissao =         'Cedente.CodTransmissao';
  CKeyBOLETOCedenteModalidade =             'Cedente.Modalidade';
  CKeyBOLETOCedenteConvenio =               'Cedente.Convenio';

  CKeyBOLETOChavePix =                      'ChavePix';
  CKeyBOLETOTipoChavePix =                  'TipoChavePix';
  CKeyBOLETOClientID =                      'ClientID';
  CKeyBOLETOClientSecret =                  'ClientSecret';
  CKeyBOLETOKeyUser =                       'KeyUser';
  CKeyBOLETOScope =                         'Scope';
  CKeyBOLETOIndicadorPix =                  'IndicadorPix';
  CKeyBOLETOLogNivel =                      'LogNivel';
  CKeyBOLETOPathGravarRegistro =            'PathGravarRegistro';
  CKeyBOLETONomeArquivoLog =                'NomeArquivoLog';
  CKeyBOLETOAmbiente =                      'Ambiente';
  CKeyBOLETOOperacao =                      'Operacao';
  CKeyBOLETOProxyHost =                     'ProxyHost';
  CKeyBOLETOProxyPass =                     'ProxyPass';
  CKeyBOLETOProxyPort =                     'ProxyPort';
  CKeyBOLETOProxyUser =                     'ProxyUser';
  CKeyBOLETOCryptLib =                      'CryptLib';
  CKeyBOLETOHttpLib =                       'HttpLib';
  CKeyBOLETOXmlSignLib =                    'XmlSignLib';
  CKeyBOLETOSSLType =                       'SSLType';
  CKeyBOLETOTimeOut =                       'TimeOut';
  CKeyBOLETOCertificadoHTTP =               'CertificadoHTTP';
  CKeyBOLETOVersaoDF =                      'VersaoDF';
  CKeyBOLETOArquivoCRT =                    'ArquivoCRT';
  CKeyBOLETOArquivoKEY =                    'ArquivoKEY';

  CValueTipoEmpregador =                    'tePessoaJuridica';
  CValueTipoContribuinte =                  'tcPessoaJuridica';
  CvalueVersaoeSocial =                     '02_04_02';
  CvalueVersaoReinf =                       '1_03_02';
  CvalueVersaoQRCode =                      '0';

  CDirSAT =                                 'SAT';
  CDFeSATIniFile =                          'dfesat.ini';
  CObjSAT =                                 'SAT';

  CSecNFSE =                                'NFSe';
  CKeyNFSELayoutProvedor =                  'LayoutProvedor';
  CKeyNFSECodigoMunicipio =                 'CodigoMunicipio';
  CKeyNFSENomeMunicipio =                   'NomeMunicipio';
  CKeyNFSEUFMunicipio =                     'UFMunicipio';
  CKeyNFSeUsuario =                         'Usuario';
  CKeyNFSeSenha =                           'Senha';
  CKeyNFSeChaveAcesso =                     'ChaveAcesso';
  CKeyNFSeChaveAutenticacao =               'ChaveAutenticacao';
  CKeyNFSeFraseSecreta =                    'FraseSecreta';
  CKeyNFSeCNPJEmitente =                    'CNPJEmitente';
  CKeyNFSeIMEmitente =                      'IMEmitente';
  CKeyNFSeNomeEmitente =                    'NomeEmitente';
  CKeyNFSeMontarAutoPathSchema =            'MontarAutoPathSchema';
  CKeyNFSeConsultarLoteAposEnvio =          'ConsultarLoteAposEnvio';
  CKeyNFSeConsultarAposCancelar =           'ConsultarAposCancelar';
  CKeyNFSeNomePrefeitura =                  'NomePrefeitura';
  CKeyNFSeCNPJPrefeitura =                  'CNPJPrefeitura';
  CKeyNFSeNomeLongoNFSe =                   'NomeLongoNFSe';


implementation

end.

