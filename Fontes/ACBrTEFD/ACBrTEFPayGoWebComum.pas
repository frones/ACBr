{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrTEFPayGoWebComum;

interface

uses
  Classes, SysUtils,
  ACBrTEFPayGoComum, ACBrTEFComum, ACBrBase;

resourcestring
  sPerVenctoCartao = 'VENCIMENTO CARTAO';
  sInfoRemovaCartao = 'REMOVER O CARTAO';
  sErrLibJaInicializda = 'Biblioteca PGWebLib já foi inicializada';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';
  sErrLibVersaoInvalida = 'Biblioteca %s tem versão %s, inferior a %s';
  sErrPWRET_WRITERR = 'Falha de gravação no diretório %s';
  sErrPWRET_INVCALL = 'Já foi efetuada uma chamada à função PW_iInit';
  sErrPWRET_INVCALL2 = 'Não há captura de dados no PIN-pad em curso.';
  sErrPWRET_INVCALL3 = 'Não é possível capturar dados em um PIN-pad não ABECS';
  sErrPWRET_NODATA = 'A informação solicitada não está disponível';
  sErrPWRET_BUFOVFLW = 'O valor da informação solicitada não cabe no Buffer alocado';
  sErrPWRET_DLLNOTINIT = 'Biblioteca PGWebLib não foi inicializada';
  sErrPWRET_TRNINIT = 'Já foi iniciada uma Transação';
  sErrPWRET_TRNNOTINIT = 'Não foi iniciada uma Transação';
  sErrPWRET_INVALIDTRN = 'A transação informada para confirmação não existe ou já foi confirmada anteriormente';
  sErrPWRET_NOTINST = 'É necessário efetuar uma transação de Instalação';
  sErrPWRET_INVPARAM = 'Valor %s Inválido para parâmetro %s';
  sErrPWRET_INVPARAM2 = 'O valor de uiIndex informado não corresponde a uma captura de dados deste tipo.';
  sErrPWRET_NOMANDATORY = 'Parâmetros obrigatórios não informados';
  sErrPWRET_PPCOMERR = 'Falha na comunicação com o PIN-pad.';
  sErrPWDAT_UNKNOWN = 'Não sei tratar Tipo de Dado: %d';
  sErrPWINF_INVALID = 'ParametrosAdicionais: Valor %s é inválido para %s';

const
  CACBrTEFPGWebAPIName = 'ACBrTEFPGWebAPI';
  CACBrTEFPGWebAPIVersao = '1.0.1';

  {$IFDEF MSWINDOWS}
   CACBrTEFPGWebLib = 'PGWebLib.dll';
  {$ELSE}
   {$IFDEF ANDROID}
    CACBrTEFPGWebLib = 'libPGWebLib.so';
   {$ELSE}
    CACBrTEFPGWebLib = 'PGWebLib.so';
   {$ENDIF}
 {$ENDIF}

  CACBrTEFPGWebLibMinVersion = '0004.0000.0082.0003';

  CSleepNothing = 300;
  CMilissegundosMensagem = 5000;  // 5 seg
  CMilissegundosOcioso = 300000;  // 5 min

//==========================================================================================
// Número maximo de itens em um menu de seleção
//==========================================================================================
  PWMENU_MAXINTENS = 40;

//=========================================================================================
// Tipos de evento a serem ativados para monitoração no PIN-pad
//==========================================================================================
  PWPPEVTIN_KEYS = 1;   // Acionamento de teclas
  PWPPEVTIN_MAG  = 2;   // Passagem de cartão magnético
  PWPPEVTIN_ICC  = 4;   // Inserção de cartão com chip.
  PWPPEVTIN_CTLS = 8;   // Aproximação de um cartão sem contato

//==========================================================================================
//  Tipos de evento retornados pelo PIN-pad
//==========================================================================================
  PWPPEVT_MAGSTRIPE = 1;     //  01h Foi passado um cartão magnético.
  PWPPEVT_ICC       = 2;     //  02h Foi detectada a presença de um cartão com chip.
  PWPPEVT_CTLS      = 3;     //  03h Foi detectada a presença de um cartão sem contato.
  PWPPEVT_KEYCONF   = 17;    //  11h Foi pressionada a tecla [OK].
  PWPPEVT_KEYBACKSP = 18;    //  12h Foi pressionada a tecla [CORRIGE].
  PWPPEVT_KEYCANC   = 19;    //  13h Foi pressionada a tecla [CANCELA].
  PWPPEVT_KEYF1     = 33;    //  21h Foi pressionada a tecla [F1].
  PWPPEVT_KEYF2     = 34;    //  22h Foi pressionada a tecla [F2].
  PWPPEVT_KEYF3     = 35;    //  23h Foi pressionada a tecla [F3].
  PWPPEVT_KEYF4     = 36;    //  24h Foi pressionada a tecla [F4].

  //===========================================================
  //  Tabela de Códigos de Erro de Retorno da Biblioteca
  //===========================================================
  PWRET_OK                 = 0;      // Operação bem sucedida
  PWRET_FROMHOSTPENDTRN    = -2599;  // Existe uma transação pendente, é necessário confirmar ou desfazer essa transação através de PW_iConfirmation.
  PWRET_FROMHOSTPOSAUTHERR = -2598;  // Falha de autenticação do ponto de captura com a infraestrutura do Pay&Go Web.
  PWRET_FROMHOSTUSRAUTHERR = -2597;  // Falha de autenticação do usuário
  PWRET_FROMHOST           = -2596;  // Erro retornado pela infraestrutura do Pay&Go Web. Verificar a mensagem (PWINFO_RESULTMSG) para mais informações
  PWRET_TLVERR             = -2595;  // Falha de comunicação com a infraestrutura do Pay&Go Web (codificação da mensagem).
  PWRET_SRVINVPARAM        = -2594;  // Falha de comunicação com a infraestrutura do Pay&Go Web (parâmetro inválido).
  PWRET_REQPARAM           = -2593;  // Falha de comunicação com a infraestrutura do Pay&Go Web (falta parâmetro obrigatório).
  PWRET_HOSTCONNUNK        = -2592;  // Erro interno da biblioteca (conexão ao host).
  PWRET_INTERNALERR        = -2591;  // Erro interno da biblioteca
  PWRET_BLOCKED            = -2590;  // O ponto de captura foi bloqueado para uso
  PWRET_FROMHOSTTRNNFOUND  = -2589;  // A transação referenciada (cancelamento, confirmação, etc.) não foi encontrada.
  PWRET_PARAMSFILEERR      = -2588;  // Inconsistência dos parâmetros de operação recebidos da infraestrutura do Pay&Go Web
  PWRET_NOCARDENTMODE      = -2587;  // O Ponto de Captura não tem a capacidade de efetuar a captura do cartão através dos tipos de entrada especificados pelo Pay&Go Web
  PWRET_INVALIDVIRTMERCH   = -2586;  // Falha de comunicação com a infraestrutura do Pay&Go Web (código de afiliação inválido).
  PWRET_HOSTTIMEOUT        = -2585;  // Falha de comunicação com a infraestrutura do Pay&Go Web (tempo de resposta esgotado).
  PWRET_CONFIGREQUIRED     = -2584;  // Erro de configuração. É necessário acionar a função de configuração.
  PWRET_HOSTCONNERR        = -2583;  // Falha de conexão à infraestrutura do Pay&Go Web
  PWRET_HOSTCONNLOST       = -2582;  // A conexão com a infraestrutura do Pay&Go Web foi interrompida
  PWRET_FILEERR            = -2581;  // Falha no acesso aos arquivos da biblioteca de integração
  PWRET_PINPADERR          = -2580;  // Falha de comunicação com o PIN-pad (aplicação).
  PWRET_MAGSTRIPEERR       = -2579;  // Formato de tarja magnética não reconhecido
  PWRET_PPCRYPTERR         = -2578;  // Falha de comunicação com o PIN-pad (comunicação segura).
  PWRET_SSLCERTERR         = -2577;  // Falha no certificado SSL
  PWRET_SSLNCONN           = -2576;  // Falha ao tentar estabelecer conexão SSL
  PWRET_GPRSATTACHFAILED   = -2575;  // Falha no registro GPRS.
  PWRET_EMVDENIEDCARD      = -2574;  // Transação EMV negada pelo cartão.
  PWRET_EMVDENIEDHOST      = -2573;  // Transação EMV negada pelo host.
  PWRET_NOLINE             = -2572;  // Sem tom de linha.
  PWRET_NOANSWER           = -2571;  // Sem resposta (Linha não atende).
  PWRET_SYNCERROR          = -2570;  // Falha de sincronismo.
  PWRET_CRCERR             = -2569;  // Falha no CRC da mensagem.
  PWRET_DECOMPERR          = -2568;  // Falha na descompressão da mensagem.
  PWRET_PROTERR            = -2567;  // Falha no protocolo de conexão.
  PWRET_NOSIM              = -2566;  // SIM Card não encontrado.
  PWRET_SIMERROR           = -2565;  // Erro no SIM Card.
  PWRET_SIMBLOCKED         = -2564;  // SIM Card está bloqueado.
  PWRET_PPPNEGFAILED       = -2563;  // Falha na autenticação PPP.
  PWRET_WIFICONNERR        = -2562;  // Falha de comunicação WiFi.
  PWRET_WIFINOTFOUND       = -2561;  // Falha rede WiFi não encontrada.
  PWRET_COMPERR            = -2560;  // Falha na compactação da mensagem.
  PWRET_INVALIDCPFCNPJ     = -2559;  // Erro CPF ou CNPJ inválido.
  PWRET_APNERROR           = -2558;  // Erro de falha na APN do SIM Card.
  PWRET_WIFIAUTHERROR      = -2557;  // Erro na autenticação da rede WIFi.
  PWRET_QRCODEERR          = -2556;  // Erro no processamento do QR Code.
  PWRET_QRCODENOTSUPPORTED = -2555;  // Erro QR Code não suportado pelo terminal.
  PWRET_QRCODENOTFOUND     = -2554;  // Erro QR Code não encontrado.
  PWRET_DEFAULT_COMM_ERROR = -2553;  // Erro genérico de comunicação.
  PWRET_CTLSMAGSTRIPENOTALLOW= -2552; // Aplicação não permite fallback contactless.
  PWRET_PARAMSFILEERRSIZE  = -2551;  //Erro de tamanho do arquivo de parâmetros.
  PWRET_EXPLOGMEMERR       = -2550;  // Erro ao exportar os logs para o servidor, não foi possível alocar memória para tratar os arquivos.
  PWRET_EXPLOGPOSTERR      = -2549;  // Erro ao exportar os logs para o servidor, falha no comando POST executado.
  PWRET_EXPLOGCONFIGERR    = -2548;  // Não foi possível exportar os logs, pois os dados do servidor para exportação não foram configurados
  PWRET_INVPARAM           = -2499;  // Parâmetro inválido passado à função
  PWRET_NOTINST            = -2498;  // Ponto de Captura não instalado. É necessário acionar a função de Instalação.
  PWRET_MOREDATA           = -2497;  // Ainda existem dados que precisam ser capturados para a transação poder ser realizada
  PWRET_NODATA             = -2496;  // A informação solicitada não está disponível.
  PWRET_DISPLAY            = -2495;  // A Automação deve apresentar uma mensagem para o operador
  PWRET_INVCALL            = -2494;  // Função chamada no momento incorreto
  PWRET_NOTHING            = -2493;  // Nada a fazer, continuar o processamento
  PWRET_BUFOVFLW           = -2492;  // O tamanho da área de memória informado é insuficiente.
  PWRET_CANCEL             = -2491;  // Operação cancelada pelo operador
  PWRET_TIMEOUT            = -2490;  // Tempo limite excedido para ação do operador
  PWRET_PPNOTFOUND         = -2489;  // PIN-pad não encontrado na busca efetuada.
  PWRET_TRNNOTINIT         = -2488;  // Não foi chamada a função PW_iNewTransac
  PWRET_DLLNOTINIT         = -2487;  // Não foi chamada a função PW_iInit
  PWRET_FALLBACK           = -2486;  // Ocorreu um erro no cartão magnético, passar a aceitar o cartão digitado, caso já não esteja sendo aceito
  PWRET_WRITERR            = -2485;  // Falha de gravação no diretório de trabalho.
  PWRET_PPCOMERR           = -2484;  // Falha na comunicação com o PIN-pad (protocolo).
  PWRET_NOMANDATORY        = -2483;  // Algum dos parâmetros obrigatórios não foi adicionado
  PWRET_INVALIDTRN         = -2482;  // A transação informada para confirmação não existe ou já foi confirmada anteriormente.
  PWRET_OFFINVCAP          = -2481;  // Falha onde contenha um número diferente de itens de menu e texto a exibir.
  PWRET_OFFNOCARDENTMODE   = -2480;  // Falha caso não tenha nenhum meio de captura habilitado.
  PWRET_OFFINVCARDENTMODE  = -2479;  // Falha onde o meio de captura utilizado não esteja habilitado.
  PWRET_OFFNOTABLECARDRANGE= -2478;  // Falha quando não existir tabela de cartão para o range inserido.
  PWRET_OFFNOTABLEPRODUCT  = -2477;  // Falha quando não existir tabela de produto para a transação em execução.
  PWRET_OFFNOCARDFULLPAN   = -2475;  // Falha obtendo o número do cartão.
  PWRET_OFFINVCARDEXPDT    = -2474;  // Falha de data de validade do cartão inválida
  PWRET_OFFCARDEXP         = -2473;  // Falha cartão expirado.
  PWRET_OFFNOTRACKS        = -2472;  // Falha cartão sem trilha.
  PWRET_OFFTRACKERR        = -2471;  // Falha erro na leitura da trilha do cartão.
  PWRET_OFFCHIPMANDATORY   = -2470;  // Falha transação com chip é mandatória.
  PWRET_OFFINVCARD         = -2469;  // Falha cartão inválido.
  PWRET_OFFINVCURR         = -2468;  // Falha moeda inválida.
  PWRET_OFFINVAMOUNT       = -2467;  // Falha valor inválido.
  PWRET_OFFGREATERAMNT     = -2466;  // Falha valor excede o máximo permitido.
  PWRET_OFFLOWERAMNT       = -2465;  // Falha valor não atinge o mínimo permitido.
  PWRET_OFFGREATERINST     = -2464;  // Falha valor da parcela excede o valor permitido.
  PWRET_OFFLOWERINST       = -2463;  // Falha valor da parcela não atinge o mínimo permitido.
  PWRET_OFFINVCARDTYPE     = -2462;  // Falha tipo de cartão inválido.
  PWRET_OFFINVFINTYPE      = -2461;  // Falha tipo de financiamento inválido.
  PWRET_OFFINVINST         = -2460;  // Falha número de parcelas inválida.
  PWRET_OFFGREATERINSTNUM  = -2459;  // Falha número de parcelas excede o máximo permitido.
  PWRET_OFFLOWERINSTNUM    = -2458;  // Falha número de parcelas não atinge o mínimo permitido.
  PWRET_OFFMANDATORYCVV    = -2457;  // Falha código de segurança do cartão obrigatório.
  PWRET_OFFINVLASTFOUR     = -2456;  // Falha 4 últimos dígitos do cartão inválidos.
  PWRET_OFFNOAID           = -2455;  // Falha AID do cartão não se encontra nas tabelas de inicialização.
  PWRET_OFFNOFALLBACK      = -2454;  // Falha fallback não permitido.
  PWRET_OFFNOPINPAD        = -2453;  // Falha PIN-Pad não encontrado.
  PWRET_OFFNOAPOFF         = -2452;  // Falha transação offline não permitida.
  PWRET_OFFTRNNEEDPP       = -2451;  // Falha transação necessita de PIN-pad.
  PWRET_OFFCARDNACCEPT     = -2450;  // Falha cartão não aceito.
  PWRET_OFFTABLEERR        = -2449;  // Falha nas tabelas de inicialização.
  PWOFF_OFFMAXTABERR       = -2448;  // Falha número de tabelas excede o máximo.
  PWRET_OFFINTERNAL1       = -2447;  // Falha caso exista mais do que uma tabela de produto para a transação em execução.
  PWRET_OFFINTERNAL2       = -2446;  // Falha caso exista mais do que uma tabela de produto para a transação em execução.
  PWRET_OFFINTERNAL3       = -2445;  // Falha caso não exista no buffer a tag MUXTAG_CARDFULLPAN.
  PWRET_OFFINTERNAL4       = -2444;  // Falha caso exista mais do que uma tabela de produto para a transação em execução.
  PWRET_OFFINTERNAL5       = -2443;  // Falha na recuperação de valor da tag MUXTAG_EMVRESOFF.
  PWRET_OFFINTERNAL6       = -2442;  // Falha caso exista mais do que uma tabela de produto para a transação em execução.
  PWRET_OFFINTERNAL7       = -2441;  // Falha caso exista mais do que uma tabela de produto para a transação em execução.
  PWRET_OFFINTERNAL8       = -2440;  // Falha na obtenção e validação da trilha 2.
  PWRET_OFFINTERNAL9       = -2439;  // Falha no tamanho da trilha 2 do cartão.
  PWRET_OFFINTERNAL10      = -2438;  // Falha na obtenção e validação da trilha 1.
  PWRET_OFFINTERNAL11      = -2437;  // Falha caso exista mais do que uma tabela de produto para a transação em execução.
  PWRET_OFFNOPRODUCT       = -2436;  // Falha para quando não existir produtos compatíveis nas tabelas para a transação em execução.
  PWRET_OFFINTERNAL12      = -2435;  // Falha na obtenção e validação do PAN do cartão.
  PWRET_OFFINTERNAL13      = -2434;  // Falha na criptografia genérica da transação.
  PWRET_OFFINTERNAL14      = -2433;  // Falha na criptografia genérica da transação.
  PWRET_NOPINPAD           = -2432;  // Falha PIN-Pad não encontrado.
  PWRET_OFFINTERNAL15      = -2431;  // Falha na obtenção da informação de valor da parcela.
  PWRET_OFFINTERNAL16      = -2430;  // Falha trilha do cartão fora do formato padrão.
  PWRET_ABECSERRCOM        = -2429;  // Falha PIN-Pad incompatível.
  PWRET_OFFCFGNOCARDRANGE  = -2428;  // Falha inconsistência nas informações de cartão recebidas.
  PWRET_OFFCFGNOPRODUCT    = -2427;  // Falha inconsistência nas informações de produto recebidas.
  PWRET_OFFCFGNOTRANSACTION= -2426;  // Falha inconsistência nas informações de transação recebidas.
  PWRET_OFFINTERNAL17      = -2425;  // Falha na criptografia genérica da transação.
  PWRET_OFFINTERNAL18      = -2424;  // Falha processamento offline da PGWebLib.
  PWRET_PPABORT            = -2423;  // Falha abortar comando PIN-Pad.
  PWRET_OFFINTERNAL19      = -2422;  // Falha caso exista mais do que uma tabela de produto para a transação em execução.
  PWRET_PPERRTREATMENT     = -2421;  // Erro de tratamento PIN-Pad.
  PWRET_INVPAYMENTMODE     = -2420;  // Falha modalidade de pagamento inválida.
  PWRET_OFFINVALIDOPER     = -2419;  // Operação selecionada não está disponível.
  PWRET_OFFINTERNAL20      = -2418;  // Falha processamento offline tag EMV.
  PWRET_OFFINTERNAL21      = -2417;  // Erro processamento offline do QR Code

  //==========================================================
  // Erros específicos da biblioteca compartilhada de PIN-pad
  //==========================================================
  PWRET_PPS_MAX  = -2100;
  PWRET_PPS_MIN  = PWRET_PPS_MAX - 100;
  PWRET_PPS_OK             = -2100;  // PIN-pad não encontrado na busca efetuada.
  PWRET_PPS_PROCESSING     = -2101;  // Não foi chamada a função PW_iNewTransac.
  PWRET_PPS_NOTIFY         = -2102;  // Não foi chamada a função PW_iInit.
  PWRET_PPS_F1             = -2104;  // Ocorreu um erro no cartão magnético, passar a aceitar o cartão digitado, caso já não esteja sendo aceito.
  PWRET_PPS_F2             = -2105;  // Pressionada tecla de função #3.
  PWRET_PPS_F3             = -2106;  // Falha na comunicação com o PIN-pad (protocolo).
  PWRET_PPS_F4             = -2107;  // Pressionada tecla de função #4.
  PWRET_PPS_BACKSP         = -2108;  // Pressionada tecla de apagar (backspace)
  PWRET_PPS_INVCALL        = -2110;  // Chamada inválida à função. Operações prévias são necessárias
  PWRET_PPS_INVPARM        = -2111;  // Parâmetro inválido passado a função.
  PWRET_PPS_TIMEOUT        = -2112;  // Esgotado o tempo máximo estipulado para a operação.
  PWRET_PPS_TIMEOUT2       = -400;   // TimeOut na Operação em curso no PinPad
  PWRET_PPS_CANCEL         = -2113;  // Operação cancelada pelo operador.
  PWRET_PPS_CANCEL2        = -402;   // Operação cancelada pelo operador no PinPad
  PWRET_PPS_ALREADYOPEN    = -2114;  // Pinpad já aberto.
  PWRET_PPS_NOTOPEN        = -2115;  // Pinpad não foi aberto.
  PWRET_PPS_EXECERR        = -2116;  // Erro interno de execução - problema de implementação da biblioteca (software).
  PWRET_PPS_INVMODEL       = -2117;  // Função não suportada pelo modelo de pinpad.
  PWRET_PPS_NOFUNC         = -2118;  // Função não disponível na Biblioteca do pinpad.
  PWRET_PPS_TABEXP         = -2120;  // Tabelas expiradas (pelo “time-stamp”).
  PWRET_PPS_TABERR         = -2121;  // Erro ao tentar gravar tabelas (falta de espaço, por exemplo)
  PWRET_PPS_NOAPPSLIC      = -2122;  // Aplicação da rede adquirente não existe no pinpad.
  PWRET_PPS_PORTERR        = -2130;  // Erro de comunicação: porta serial do pinpad provavelmente ocupada
  PWRET_PPS_COMMERR        = -2131;  // Erro de comunicação: pinpad provavelmente desconectado ou problemas com a interface serial.
  PWRET_PPS_UNKNOWNSTAT    = -2132;  // Status informado pelo pinpad não é conhecido.
  PWRET_PPS_RSPERR         = -2133;  // Mensagem recebida do pinpad possui formato inválido.
  PWRET_PPS_COMMTOUT       = -2134;  // Tempo esgotado ao esperar pela resposta do pinpad (no caso de comandos não blocantes).
  PWRET_PPS_INTERR         = -2140;  // Erro interno do pinpad.
  PWRET_PPS_MCDATAERR      = -2141;  // Erro de leitura do cartão magnético.
  PWRET_PPS_ERRPIN         = -2142;  // Erro na captura do PIN - Master Key pode não estar presente.
  PWRET_PPS_NOCARD         = -2143;  // Não há cartão com chip presente no acoplador.
  PWRET_PPS_PINBUSY        = -2144;  // Pinpad não pode processar a captura de PIN temporariamente devido a questões de segurança (como quando é atingido o limite de capturas dentro de um intervalo de tempo).
  PWRET_PPS_SAMERR         = -2150;  // Erro genérico no módulo SAM.
  PWRET_PPS_NOSAM          = -2151;  // SAM ausente, “mudo”, ou com erro de comunicação.
  PWRET_PPS_SAMINV         = -2152;  // SAM inválido, desconhecido ou com problemas.
  PWRET_PPS_DUMBCARD       = -2160;  // Cartão não responde (“mudo”) ou chip não presente.
  PWRET_PPS_ERRCARD        = -2161;  // Erro de comunicação do pinpad com o cartão com chip.
  PWRET_PPS_CARDINV        = -2162;  // Cartão do tipo inválido ou desconhecido, não pode ser tratado (não é EMV nem TIBC v1).
  PWRET_PPS_CARDBLOCKED    = -2163;  // Cartão bloqueado por número excessivo de senhas incorretas (somente para Easy-Entry TIBC v1 e moedeiro VISA Cash).
  PWRET_PPS_CARDNAUTH      = -2164;  // Cartão TIBC v1 não autenticado pelo módulo SAM (somente para Easy-Entry TIBC v1 e moedeiro VISA Cash).
  PWRET_PPS_CARDEXPIRED    = -2165;  // Cartão TIBC v1 expirado (somente para Easy-Entry TIBC v1 e moedeiro VISA Cash).
  PWRET_PPS_CARDERRSTRUCT  = -2166;  // Cartão com erro de estrutura - arquivos estão faltando.
  PWRET_PPS_CARDINVALIDAT  = -2167;  // Cartão foi invalidado. Se o cartão for TIBC v1, quando seleção de arquivo ou ATR retornar status ‘6284’. Se o cartão for EMV, quando seleção de aplicação retornar status ‘6A81’.
  PWRET_PPS_CARDPROBLEMS   = -2168;  // Cartão com problemas. Esse status é válido para muitas ocorrências no processamento de cartões TIBC v1 e EMV onde o cartão não se comporta conforme o esperado e a transação deve ser finalizada.
  PWRET_PPS_CARDINVDATA    = -2169;  // O cartão, seja TIBC v1 ou EMV, comporta-se corretamente porém possui dados inválidos ou inconsistentes.
  PWRET_PPS_CARDAPPNAV     = -2170;  // Cartão sem nenhuma aplicação disponível para as condições pedidas (ou cartão é reconhecido como TIBC v1 ou EMV mas não possui nenhuma aplicação compatível com a requerida).
  PWRET_PPS_CARDAPPNAUT    = -2171;  // Somente para cartão EMV. A aplicação selecionada não pode ser utilizada (o Get Processing Options retornou status ‘6985’ ou houve erro no comando Select final), e não há outra aplicação compatível na lista de candidatas.
  PWRET_PPS_NOBALANCE      = -2172;  // Somente para aplicação de moedeiro. O saldo do moedeiro é insuficiente para a operação.
  PWRET_PPS_LIMITEXC       = -2173;  // Somente para aplicação de moedeiro. O limite máximo para a operação foi excedido.
  PWRET_PPS_CARDNOTEFFECT  = -2174;  // Cartão ainda não efetivo, data de ativação posterior à data atual (somente para moedeiro VISA Cash sobre TIBCv3).
  PWRET_PPS_VCINVCURR      = -2175;  // Moeda inválida (somente para moedeiro VISA Cash).
  PWRET_PPS_ERRFALLBACK    = -2176;  // Erro de alto nível no cartão EMV que é passível de “fallback” para tarja magnética.
  PWRET_PPS_CTLSSMULTIPLE  = -2180;  // Mais de um cartão sem contato foi apresentado ao leitor (este código de retorno é opcional e depende da capacidade do equipamento em detectar esta situação).
  PWRET_PPS_CTLSSCOMMERR   = -2181;  // Erro de comunicação entre o terminal (antena) e o cartão com chip sem contato.
  PWRET_PPS_CTLSSINVALIDAT = -2182;  // Cartão foi invalidado (seleção de aplicação retornou status ‘6A81’).
  PWRET_PPS_CTLSSPROBLEMS  = -2183;  // Cartão com problemas. Esse status é válido para muitas ocorrências no processamento de cartões sem contato em que o cartão não se comporta conforme o esperado e a transação deve ser finalizada.
  PWRET_PPS_CTLSSAPPNAV    = -2184;  // Cartão sem nenhuma aplicação disponível para as condições pedidas (nenhum AID encontrado).
  PWRET_PPS_CTLSSAPPNAUT   = -2185;  // A aplicação selecionada não pode ser utilizada (o Get Processing Options retornou status ‘6985’ ou houve erro no comando Select final), e não há outra aplicação compatível na lista de candidatas.
  PWRET_PPS_XXX            = -2200;  // Erros retornados pelo PIN-pad, conforme seção 10.2

//==========================================================================================
// Tipos utilizados na captura de dados dinamica
//==========================================================================================
  PWDAT_NONE         = 0;   // tipo inválido, ignorar...
  PWDAT_MENU         = 1;   // menu de opções
  PWDAT_TYPED        = 2;   // entrada digitada
  PWDAT_CARDINF      = 3;   // dados de cartão
  PWDAT_PPENTRY      = 5;   // entrada digitada no PIN-pad
  PWDAT_PPENCPIN     = 6;   // senha criptografada
  PWDAT_CARDOFF      = 9;   // processamento off-line de cartão com chip
  PWDAT_CARDONL      = 10;  // processamento on-line de cartão com chip
  PWDAT_PPCONF       = 11;  // confirmação de informação no PIN-pad
  PWDAT_BARCODE      = 12;  // Código de barras, lido ou digitado
  PWDAT_PPREMCRD     = 13;  // Remoção do cartão do PIN-pad.
  PWDAT_PPGENCMD     = 14;  // comando proprietário da rede no PIN-pad.
  PWDAT_PPDATAPOSCNF = 16;  // confirmação positiva de dados no PIN-pad.
  PWDAT_USERAUTH     = 17;  // validação da senha.
  PWDAT_DSPCHECKOUT  = 18;  // Mensagem a ser exibida no checkout
  PWDAT_TSTKEY       = 19;  // Teste de chaves presentes no PIN-pad.
  PWDAT_DSPQRCODE    = 20;  // QR Code, a ser exibido no checkout.
//==========================================================================================
// Tipos de operação, utilizados na função PW_iGetOperations
//==========================================================================================
  PWOPTYPE_ADMIN  = 1;  // Operações administrativas (relatório, reimpressão, etc).
  PWOPTYPE_SALE   = 2;  // Operações financeiras.

//==========================================================================================
// Dados digitado pelo portador do cartão no PIN-pad.
//==========================================================================================
  PWDPIN_DIGITE_O_DDD                = 1;
  PWDPIN_REDIGITE_O_DDD              = 2;
  PWDPIN_DIGITE_O_TELEFONE           = 3;
  PWDPIN_REDIGITE_O_TELEFONE         = 4;
  PWDPIN_DIGITE_DDD_TELEFONE         = 5;
  PWDPIN_REDIGITE_DDD_TELEFONE       = 6;
  PWDPIN_DIGITE_O_CPF                = 7;
  PWDPIN_REDIGITE_O_CPF              = 8;
  PWDPIN_DIGITE_O_RG                 = 9;
  PWDPIN_REDIGITE_O_RG               = 10;
  PWDPIN_DIGITE_OS_4_ULTIMOS_DIGITOS = 11;
  PWDPIN_DIGITE_CODIGO_DE_SEGURANCA  = 12;
  PWDPIN_DIGITE_O_CNPJ               = 13;
  PWDPIN_REDIGITE_O_CNPJ             = 14;

//==========================================================================================
//  Tipos de Cartões
//==========================================================================================
  PWCARTAO_NaoDefinido = 0;
  PWCARTAO_Credito     = 1;
  PWCARTAO_Debito      = 2;
  PWCARTAO_Voucher     = 4;
  PWCARTAO_Outros      = 8;

//==========================================================================================
//  Tipos de Vendas
//==========================================================================================
  PWTVENDA_NaoDefinido              = 0;
  PWTVENDA_AVista                   = 1;
  PWTVENDA_ParceladoEmissor         = 2;
  PWTVENDA_ParceladoEstabelecimento = 4;
  PWTVENDA_PreDatado                = 8;

//==========================================================================================
//  Tipos de Validação em PW_GetData.bValidacaoDado
//==========================================================================================
  PWVAL_Nenhuma        = 0;  // sem validação
  PWVAL_NaoVazio       = 1;  // o dado não pode ser vazio
  PWVAL_Modulo10       = 2;  // (último) dígito verificador, algoritmo módulo 10
  PWVAL_CPF_CNPJ       = 3;  // CPF ou CNPJ
  PWVAL_MMAA           = 4;  // data no formato “MMAA”
  PWVAL_DDMMAA         = 5;  // data no formato “DDMMAA”
  PWVAL_DuplaDigitacao = 6;  // solicitar a digitação duas vezes iguais (confirmação)

//==========================================================================================
//  Tipos de Entrada Permitidos em PW_GetData.bTiposEntradaPermitidos
//==========================================================================================
  PWTYP_ReadOnly   = 0; // deve exibir o dado contido em szValorInicial, sem permitir a edição do mesmo;
  PWTYP_Numerico   = 1; // somente numéricos;
  PWTYP_Alfabetico = 2; // somente alfabéticos;
  PWTYP_AlfaNume   = 3; // numéricos e alfabéticos;
  PWTYP_AlfaNumEsp = 7; // numéricos, alfabéticos e especiais.


type
  //========================================================
  // Record que descreve cada membro da estrutura PW_GetData:
  //========================================================
  TPW_GetData = record
    wIdentificador : Word;
    bTipoDeDado : Byte;
    szPrompt: Array[0..83] of AnsiChar;
    bNumOpcoesMenu: Byte;
    vszTextoMenu: Array[0..PWMENU_MAXINTENS-1] of Array[0..40] of AnsiChar;
    vszValorMenu: Array[0..PWMENU_MAXINTENS-1] of Array[0..255] of AnsiChar;
    szMascaraDeCaptura: Array[0..40] of AnsiChar;
    bTiposEntradaPermitidos: Byte;
    bTamanhoMinimo: Byte;
    bTamanhoMaximo: Byte;
    ulValorMinimo : LongWord;
    ulValorMaximo : LongWord;
    bOcultarDadosDigitados: Byte;
    bValidacaoDado: Byte;
    bAceitaNulo: Byte;
    szValorInicial: Array[0..40] of AnsiChar;
    bTeclasDeAtalho: Byte;
    szMsgValidacao: Array[0..83] of AnsiChar;
    szMsgConfirmacao: Array[0..83] of AnsiChar;
    szMsgDadoMaior: Array[0..83] of AnsiChar;
    szMsgDadoMenor: Array[0..83] of AnsiChar;
    bCapturarDataVencCartao: Byte;
    ulTipoEntradaCartao: LongWord;
    bItemInicial: Byte;
    bNumeroCapturas: Byte;
    szMsgPrevia: Array[0..83] of AnsiChar;
    bTipoEntradaCodigoBarras: Byte;
    bOmiteMsgAlerta: Byte;
    bIniciaPelaEsquerda: Byte;
    bNotificarCancelamento: Byte;
    bAlinhaPelaDireita: Byte;
    bIndice: Byte;
  end;

  TArrPW_GetData = Array[0..10] of TPW_GetData;

  //====================================================================
  // Estrutura para armazenamento de dados para Tipos de Operação
  //====================================================================
  TPW_Operations = record
    bOperType: Byte;
    szText: Array[0..21] of AnsiChar;
    szValue: Array[0..21] of AnsiChar;
  end;

  TArrPW_Operations = Array[0..10] of TPW_Operations;

  TPW_OperationsEx = record
    bOperType: Byte;
    szOperName: Array[0..20] of AnsiChar;
    szAuthSyst: Array[0..20] of AnsiChar;
    szValue: Array[0..20] of AnsiChar;
    fAuthPreferential: Boolean;
  end;

  TArrPW_OperationsEx = Array[0..10] of TPW_OperationsEx;

  TACBrTEFPGWebAPITiposEntrada =
    (pgApenasLeitura = 0,
     pgtNumerico = 1,
     pgtAlfabetico = 2,
     pgtAlfaNum = 3,
     pgtAlfaNumEsp = 7);

  TACBrTEFPGWebAPIValidacaoDado =
    (pgvNenhuma = 0,
     pgvNaoVazio = 1,
     pgvDigMod10 = 2,
     pgvCPF_CNPJ = 3,
     pgvMMAA = 4,
     pgvDDMMAA = 5,
     pgvDuplaDigitacao = 6,
     pgvSenhaLojista = 100,
     pgvSenhaTecnica = 101);

  TACBrTEFPGWebAPITipoBarras =
    (pgbDigitado = 1,
     pgbLeitor = 2,
     pgbDigitadoOuLeitor = 3);

  TACBrTEFPGWebAPIExibicaoQRCode = (qreNaoSuportado, qreAuto, qreExibirNoPinPad, qreExibirNoCheckOut);

  TACBrTEFPGWebAPIExibeMenu = procedure(
    Titulo: String;
    Opcoes: TStringList;
    var ItemSelecionado: Integer;
    var Cancelado: Boolean) of object ;

  TACBrTEFPGWebAPIDefinicaoCampo = record
    Titulo: String;
    MascaraDeCaptura: String;
    TiposEntradaPermitidos: TACBrTEFPGWebAPITiposEntrada;
    TamanhoMinimo: Integer;
    TamanhoMaximo: Integer;
    ValorMinimo : LongWord;
    ValorMaximo : LongWord;
    OcultarDadosDigitados: Boolean;
    ValidacaoDado: TACBrTEFPGWebAPIValidacaoDado;
    AceitaNulo: Boolean;
    ValorInicial: String;
    bTeclasDeAtalho: Boolean;
    MsgValidacao: String;
    MsgConfirmacao: String;
    MsgDadoMaior: String;
    MsgDadoMenor: String;
    TipoEntradaCodigoBarras: TACBrTEFPGWebAPITipoBarras;
    OmiteMsgAlerta: Boolean;
  end;

  TACBrTEFPGWebAPIObtemCampo = procedure(
    DefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo;
    var Resposta: String;
    var Validado: Boolean;
    var Cancelado: Boolean) of object ;

  TACBrTEFPGWebAPITerminalMensagem = (tmTodas, tmOperador, tmCliente);

  TACBrTEFPGWebAPIExibeMensagem = procedure(
    Mensagem: String;
    Terminal: TACBrTEFPGWebAPITerminalMensagem;
    MilissegundosExibicao: Integer  // 0 - Para com OK; Positivo - aguarda Ok ou N milissegundos; Negativo - Apenas exibe a Msg (não aguarda)
    ) of object;

  TACBrTEFPGWebAPIExibeQRCode = procedure(const Dados: String) of object;

  TACBrTEFPGWebAPIOperacaoPinPad = (ppGetCard, ppGetPIN, ppGetData, ppGoOnChip,
    ppFinishChip, ppConfirmData, ppGenericCMD, ppDataConfirmation, ppDisplay,
    ppGetUserData, ppWaitEvent, ppRemoveCard, ppGetPINBlock, ppTestKey,
    ppLerQRCode);

  TACBrTEFPGWebAPIAguardaPinPad = procedure(
    OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad; var Cancelar: Boolean)
     of object;

  TACBrTEFPGWebAPIAvaliarTransacaoPendente = procedure(var Status: LongWord;
    pszReqNum: String; pszLocRef: String; pszExtRef: String; pszVirtMerch: String;
    pszAuthSyst: String) of object;

  { TACBrTEFPGWebAPI }

  TACBrTEFPGWebAPI = class
  private
    fCNPJEstabelecimento: String;
    fConfirmarTransacoesPendentesNoHost: Boolean;
    fDadosTransacao: TACBrTEFParametros;
    fDiretorioTrabalho: String;
    fEnderecoIP: String;
    fExibeMensagemCheckout: Boolean;
    fExibicaoQRCode: TACBrTEFPGWebAPIExibicaoQRCode;
    fImprimeViaClienteReduzida: Boolean;
    fInicializada: Boolean;
    fCarregada: Boolean;
    fEmTransacao: Boolean;
    fPerguntarCartaoDigitadoAposCancelarLeitura: Boolean;
    fUsouPinPad: Boolean;
    fNomeAplicacao: String;
    fNomeEstabelecimento: String;
    fOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
    fOnAvaliarTransacaoPendente: TACBrTEFPGWebAPIAvaliarTransacaoPendente;
    fOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
    fOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
    fOnExibeQRCode: TACBrTEFPGWebAPIExibeQRCode;
    fOnGravarLog: TACBrGravarLog;
    fOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
    fParametrosAdicionais: TACBrTEFParametros;
    fPathLib: String;
    fPontoCaptura: String;
    fPortaPinPad: Integer;
    fPortaTCP: String;
    fRemocaoCartaoPinPad: Boolean;
    fSoftwareHouse: String;
    fSuportaDesconto: Boolean;
    fSuportaSaque: Boolean;
    fSuportaViasDiferenciadas: Boolean;
    fUtilizaSaldoTotalVoucher: Boolean;
    fVersaoAplicacao: String;
    fTimerOcioso: TACBrThreadTimer;
    fTempoOcioso: TDateTime;
    fUltimoQRCode: String;

    fTempoTarefasAutomaticas: String;

    xPW_iInit: function (const pszWorkingDir: PAnsiChar): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iGetResult: function(iInfo: Word; pszData: PAnsiChar; ulDataSize: LongWord): Smallint;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iNewTransac: function(bOper: Byte): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iAddParam: function(wParam: Word; const pszValue: PAnsiChar): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iExecTransac: function(pvstParam: TArrPW_GetData; var piNumParam: SmallInt): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iConfirmation: function(ulResult: LongWord; const pszReqNum: PAnsiChar;
              const pszLocRef: PAnsiChar; const pszExtRef: PAnsiChar;
              const pszVirtMerch: PAnsiChar; const pszAuthSyst: PAnsiChar): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iWaitConfirmation: function(): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iIdleProc: function(): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iGetOperations: function(bOperType: Byte; vstOperations: TArrPW_Operations;
              var piNumOperations: SmallInt): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iGetOperationsEx: function(bOperType: Byte; vstOperations: TArrPW_OperationsEx;
              var piNumOperations: SmallInt; iStructSize: SmallInt): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPEventLoop: function(pszDisplay: PAnsiChar; ulDisplaySize: LongWord): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPAbort: function(): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPGetCard: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPGetPIN: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPGetData: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPGoOnChip: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPFinishChip: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPConfirmData: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPGenericCMD: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPDataConfirmation: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPTestKey: function(uiIndex: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPDisplay: function(const pszMsg: PAnsiChar): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPGetUserData: function(uiMessageId: Word; bMinLen: Byte; bMaxLen: Byte;
              iToutSec:  SmallInt; pszData: PAnsiChar): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPWaitEvent: function(var pulEvent: LongWord): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPRemoveCard: function(): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPGetPINBlock: function(bKeyID: Byte; const pszWorkingKey: PAnsiChar;
              bMinLen: Byte; bMaxLen: Byte; iToutSec: SmallInt;
              const pszPrompt: PAnsiChar; pszData: PAnsiChar): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iPPCommTest: function(pszMsg: PAnsiChar; uiMsgSize: Word;
              var pbCommPort: Byte): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iTransactionInquiry: function(const pszXmlRequest: PAnsiChar;
              pszXmlResponse: PAnsiChar; ulXmlResponseLen: Word): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    procedure SetCNPJEstabelecimento(const AValue: String);
    procedure SetDiretorioTrabalho(const AValue: String);
    procedure SetEnderecoIP(const AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetNomeAplicacao(const AValue: String);
    procedure SetNomeEstabelecimento(const AValue: String);
    procedure SetPathLib(const AValue: String);
    procedure SetPontoCaptura(const AValue: String);
    procedure SetPortaTCP(const AValue: String);
    procedure SetSoftwareHouse(const AValue: String);
    procedure SetVersaoAplicacao(const AValue: String);

    procedure SetEmTransacao(AValue: Boolean);
    procedure OnTimerOcioso(Sender: TObject);
  protected
    function LibFullName: String;
    procedure LoadLibFunctions;
    procedure UnLoadLibFunctions;
    procedure ClearMethodPointers;

    procedure DoException( const AErrorMsg: String );
    procedure VerificarOK(iRET: SmallInt);
    function ObterUltimoRetorno: String;
    procedure AjustarTempoOcioso(const IdleTimeStr: String = '');

    procedure AdicionarDadosObrigatorios;
    function CalcularCapacidadesDaAutomacao: Integer;

    function ObterDados(ArrGetData: TArrPW_GetData; ArrLen: SmallInt): SmallInt;
    function ObterDadosDeParametrosAdicionais(AGetData: TPW_GetData): Boolean;
    function ObterDadoMenu(AGetData: TPW_GetData): SmallInt;
    function ObterDadoDigitado(AGetData: TPW_GetData): SmallInt;
    function ObterDadoDigitadoGenerico(AGetData: TPW_GetData; var AResposta: String): Boolean;
    function ObterDadoCodBarra(AGetData: TPW_GetData): SmallInt;
    function ObterDadoCartao(AGetData: TPW_GetData): SmallInt;
    function ObterDadoCartaoDigitado(AGetData: TPW_GetData): SmallInt;

    function RealizarOperacaoPinPad(AGetData: TPW_GetData; OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad): SmallInt;
    function AguardarOperacaoPinPad(OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad): SmallInt;
    procedure ExibirMensagem(const AMsg: String; Terminal: TACBrTEFPGWebAPITerminalMensagem = tmTodas; TempoEspera: Integer = -1);
    procedure ExibirQRCode(const Dados: String);
    procedure ChamarOnAguardaPinPad(OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad; var Cancelado: Boolean);

    function PW_GetDataToDefinicaoCampo(AGetData: TPW_GetData): TACBrTEFPGWebAPIDefinicaoCampo;
    procedure LogPWGetData(AGetData: TPW_GetData);

    function ValidarDDMM(const AString: String): Boolean;
    function ValidarDDMMAA(const AString: String): Boolean;
    function ValidarModulo10(const AString: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;

    function ObterInfo(iINFO: Word): String;
    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);

    procedure IniciarTransacao(iOPER: Byte; ParametrosAdicionaisTransacao: TStrings = nil);
    procedure AdicionarParametro(iINFO: Word; const AValor: AnsiString); overload;
    procedure AdicionarParametro(const AKeyValueStr: String); overload;
    function ExecutarTransacao: Boolean;
    function AbortarTransacao: SmallInt;
    procedure ObterDadosDaTransacao;
    procedure FinalizarTransacao(Status: LongWord; const pszReqNum: String;
      const pszLocRef: String; const pszExtRef: String; const pszVirtMerch: String;
      const pszAuthSyst: String);
    procedure EstornarTransacaoEmAndamento;
    procedure TratarTransacaoPendente;
    function VerificarPresencaPinPad: Byte;
    procedure ExibirMensagemPinPad(const MsgPinPad: String);
    function ObterDadoPinPad(iMessageId: Word; MinLen, MaxLen: Byte;
      TimeOutSec: SmallInt): String;
    function VersaoLib: String;
    procedure ObterOperacoes(TipoOperacao: Byte; Operacoes: TArrPW_OperationsEx);

    function ValidarRespostaCampo(var AResposta: String;
      ADefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo): String;

    property PathLib: String read fPathLib write SetPathLib;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
    property Carregada: Boolean read fCarregada;
    property Inicializada: Boolean read fInicializada write SetInicializada;

    property EmTransacao: Boolean read fEmTransacao;
    property DadosDaTransacao: TACBrTEFParametros read fDadosTransacao;

    property SoftwareHouse: String read fSoftwareHouse write SetSoftwareHouse;
    property NomeAplicacao: String read fNomeAplicacao write SetNomeAplicacao ;
    property VersaoAplicacao: String read fVersaoAplicacao write SetVersaoAplicacao ;

    Property NomeEstabelecimento: String read fNomeEstabelecimento write SetNomeEstabelecimento;
    property CNPJEstabelecimento: String read fCNPJEstabelecimento write SetCNPJEstabelecimento;
    property PontoCaptura: String read fPontoCaptura write SetPontoCaptura;
    property EnderecoIP: String  read fEnderecoIP write SetEnderecoIP;
    property PortaTCP: String read fPortaTCP write SetPortaTCP;
    property PortaPinPad: Integer read fPortaPinPad write fPortaPinPad;
    property ParametrosAdicionais: TACBrTEFParametros read fParametrosAdicionais;

    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto;
    property ImprimeViaClienteReduzida: Boolean read fImprimeViaClienteReduzida
      write fImprimeViaClienteReduzida;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas
      write fSuportaViasDiferenciadas;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher
      write fUtilizaSaldoTotalVoucher;
    property RemocaoCartaoPinPad: Boolean read fRemocaoCartaoPinPad
      write fRemocaoCartaoPinPad;
    property ExibeMensagemCheckout: Boolean read fExibeMensagemCheckout
      write fExibeMensagemCheckout;
    property ExibicaoQRCode: TACBrTEFPGWebAPIExibicaoQRCode read fExibicaoQRCode
      write fExibicaoQRCode;

    property ConfirmarTransacoesPendentesNoHost: Boolean
      read fConfirmarTransacoesPendentesNoHost
      write fConfirmarTransacoesPendentesNoHost;
    property PerguntarCartaoDigitadoAposCancelarLeitura: Boolean
      read fPerguntarCartaoDigitadoAposCancelarLeitura
      write fPerguntarCartaoDigitadoAposCancelarLeitura;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnExibeMenu: TACBrTEFPGWebAPIExibeMenu read fOnExibeMenu
      write fOnExibeMenu;
    property OnObtemCampo: TACBrTEFPGWebAPIObtemCampo read fOnObtemCampo
      write fOnObtemCampo;
    property OnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    property OnExibeQRCode: TACBrTEFPGWebAPIExibeQRCode read fOnExibeQRCode
      write fOnExibeQRCode;
    property OnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad read fOnAguardaPinPad
      write fOnAguardaPinPad;
    property OnAvaliarTransacaoPendente: TACBrTEFPGWebAPIAvaliarTransacaoPendente
      read fOnAvaliarTransacaoPendente write fOnAvaliarTransacaoPendente;
  end;

function PWRETToString(iRET: SmallInt): String;
function PWDATToString(bTipoDeDado: Byte): String;
function PWVALToString(bValidacaoDado: Byte): String;
function PWTYPToString(bTiposEntradaPermitidos: Byte): String;
function PWDPINToString(iMessageId: Word): String;

implementation

uses
  StrUtils, dateutils, math, typinfo,
  ACBrConsts,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.Math,
  ACBrValidador
  {$IfDef ANDROID}
   ,System.IOUtils
  {$EndIf};

function PWRETToString(iRET: SmallInt): String;
begin
  case iRET of
    PWRET_OK:                   Result := 'PWRET_OK';
    PWRET_FROMHOSTPENDTRN:      Result := 'PWRET_FROMHOSTPENDTRN';
    PWRET_FROMHOSTPOSAUTHERR:   Result := 'PWRET_FROMHOSTPOSAUTHERR';
    PWRET_FROMHOSTUSRAUTHERR:   Result := 'PWRET_FROMHOSTUSRAUTHERR';
    PWRET_FROMHOST:             Result := 'PWRET_FROMHOST';
    PWRET_TLVERR:               Result := 'PWRET_TLVERR';
    PWRET_SRVINVPARAM:          Result := 'PWRET_SRVINVPARAM';
    PWRET_REQPARAM:             Result := 'PWRET_REQPARAM';
    PWRET_HOSTCONNUNK:          Result := 'PWRET_HOSTCONNUNK';
    PWRET_INTERNALERR:          Result := 'PWRET_INTERNALERR';
    PWRET_BLOCKED:              Result := 'PWRET_BLOCKED';
    PWRET_FROMHOSTTRNNFOUND:    Result := 'PWRET_FROMHOSTTRNNFOUND';
    PWRET_PARAMSFILEERR:        Result := 'PWRET_PARAMSFILEERR';
    PWRET_NOCARDENTMODE:        Result := 'PWRET_NOCARDENTMODE';
    PWRET_INVALIDVIRTMERCH:     Result := 'PWRET_INVALIDVIRTMERCH';
    PWRET_HOSTTIMEOUT:          Result := 'PWRET_HOSTTIMEOUT';
    PWRET_CONFIGREQUIRED:       Result := 'PWRET_CONFIGREQUIRED';
    PWRET_HOSTCONNERR:          Result := 'PWRET_HOSTCONNERR';
    PWRET_HOSTCONNLOST:         Result := 'PWRET_HOSTCONNLOST';
    PWRET_FILEERR:              Result := 'PWRET_FILEERR';
    PWRET_PINPADERR:            Result := 'PWRET_PINPADERR';
    PWRET_MAGSTRIPEERR:         Result := 'PWRET_MAGSTRIPEERR';
    PWRET_PPCRYPTERR:           Result := 'PWRET_PPCRYPTERR';
    PWRET_SSLCERTERR:           Result := 'PWRET_SSLCERTERR';
    PWRET_SSLNCONN:             Result := 'PWRET_SSLNCONN';
    PWRET_GPRSATTACHFAILED:     Result := 'PWRET_GPRSATTACHFAILED';
    PWRET_EMVDENIEDCARD:        Result := 'PWRET_EMVDENIEDCARD';
    PWRET_EMVDENIEDHOST:        Result := 'PWRET_EMVDENIEDHOST';
    PWRET_NOLINE:               Result := 'PWRET_NOLINE';
    PWRET_NOANSWER:             Result := 'PWRET_NOANSWER';
    PWRET_SYNCERROR:            Result := 'PWRET_SYNCERROR';
    PWRET_CRCERR:               Result := 'PWRET_CRCERR';
    PWRET_DECOMPERR:            Result := 'PWRET_DECOMPERR';
    PWRET_PROTERR:              Result := 'PWRET_PROTERR';
    PWRET_NOSIM:                Result := 'PWRET_NOSIM';
    PWRET_SIMERROR:             Result := 'PWRET_SIMERROR';
    PWRET_SIMBLOCKED:           Result := 'PWRET_SIMBLOCKED';
    PWRET_PPPNEGFAILED:         Result := 'PWRET_PPPNEGFAILED';
    PWRET_WIFICONNERR:          Result := 'PWRET_WIFICONNERR';
    PWRET_WIFINOTFOUND:         Result := 'PWRET_WIFINOTFOUND';
    PWRET_COMPERR:              Result := 'PWRET_COMPERR';
    PWRET_INVALIDCPFCNPJ:       Result := 'PWRET_INVALIDCPFCNPJ';
    PWRET_APNERROR:             Result := 'PWRET_APNERROR';
    PWRET_WIFIAUTHERROR:        Result := 'PWRET_WIFIAUTHERROR';
    PWRET_QRCODEERR:            Result := 'PWRET_QRCODEERR';
    PWRET_QRCODENOTSUPPORTED:   Result := 'PWRET_QRCODENOTSUPPORTED';
    PWRET_QRCODENOTFOUND:       Result := 'PWRET_QRCODENOTFOUND';
    PWRET_DEFAULT_COMM_ERROR:   Result := 'PWRET_DEFAULT_COMM_ERROR';
    PWRET_CTLSMAGSTRIPENOTALLOW:Result := 'PWRET_CTLSMAGSTRIPENOTALLOW';
    PWRET_PARAMSFILEERRSIZE:    Result := 'PWRET_PARAMSFILEERRSIZE';
    PWRET_EXPLOGMEMERR:         Result := 'PWRET_EXPLOGMEMERR';
    PWRET_EXPLOGPOSTERR:        Result := 'PWRET_EXPLOGPOSTERR';
    PWRET_EXPLOGCONFIGERR:      Result := 'PWRET_EXPLOGCONFIGERR';
    PWRET_INVPARAM:             Result := 'PWRET_INVPARAM';
    PWRET_NOTINST:              Result := 'PWRET_NOTINST';
    PWRET_MOREDATA:             Result := 'PWRET_MOREDATA';
    PWRET_NODATA:               Result := 'PWRET_NODATA';
    PWRET_DISPLAY:              Result := 'PWRET_DISPLAY';
    PWRET_INVCALL:              Result := 'PWRET_INVCALL';
    PWRET_NOTHING:              Result := 'PWRET_NOTHING';
    PWRET_BUFOVFLW:             Result := 'PWRET_BUFOVFLW';
    PWRET_CANCEL:               Result := 'PWRET_CANCEL';
    PWRET_TIMEOUT:              Result := 'PWRET_TIMEOUT';
    PWRET_PPNOTFOUND:           Result := 'PWRET_PPNOTFOUND';
    PWRET_TRNNOTINIT:           Result := 'PWRET_TRNNOTINIT';
    PWRET_DLLNOTINIT:           Result := 'PWRET_DLLNOTINIT';
    PWRET_FALLBACK:             Result := 'PWRET_FALLBACK';
    PWRET_WRITERR:              Result := 'PWRET_WRITERR';
    PWRET_PPCOMERR:             Result := 'PWRET_PPCOMERR';
    PWRET_NOMANDATORY:          Result := 'PWRET_NOMANDATORY';
    PWRET_INVALIDTRN:           Result := 'PWRET_INVALIDTRN';
    PWRET_OFFINVCAP:            Result := 'PWRET_OFFINVCAP';
    PWRET_OFFNOCARDENTMODE:     Result := 'PWRET_OFFNOCARDENTMODE';
    PWRET_OFFINVCARDENTMODE:    Result := 'PWRET_OFFINVCARDENTMODE';
    PWRET_OFFNOTABLECARDRANGE:  Result := 'PWRET_OFFNOTABLECARDRANGE';
    PWRET_OFFNOTABLEPRODUCT:    Result := 'PWRET_OFFNOTABLEPRODUCT';
    PWRET_OFFNOCARDFULLPAN:     Result := 'PWRET_OFFNOCARDFULLPAN';
    PWRET_OFFINVCARDEXPDT:      Result := 'PWRET_OFFINVCARDEXPDT';
    PWRET_OFFCARDEXP:           Result := 'PWRET_OFFCARDEXP';
    PWRET_OFFNOTRACKS:          Result := 'PWRET_OFFNOTRACKS';
    PWRET_OFFTRACKERR:          Result := 'PWRET_OFFTRACKERR';
    PWRET_OFFCHIPMANDATORY:     Result := 'PWRET_OFFCHIPMANDATORY';
    PWRET_OFFINVCARD:           Result := 'PWRET_OFFINVCARD';
    PWRET_OFFINVCURR:           Result := 'PWRET_OFFINVCURR';
    PWRET_OFFINVAMOUNT:         Result := 'PWRET_OFFINVAMOUNT';
    PWRET_OFFGREATERAMNT:       Result := 'PWRET_OFFGREATERAMNT';
    PWRET_OFFLOWERAMNT:         Result := 'PWRET_OFFLOWERAMNT';
    PWRET_OFFGREATERINST:       Result := 'PWRET_OFFGREATERINST';
    PWRET_OFFLOWERINST:         Result := 'PWRET_OFFLOWERINST';
    PWRET_OFFINVCARDTYPE:       Result := 'PWRET_OFFINVCARDTYPE';
    PWRET_OFFINVFINTYPE:        Result := 'PWRET_OFFINVFINTYPE';
    PWRET_OFFINVINST:           Result := 'PWRET_OFFINVINST';
    PWRET_OFFGREATERINSTNUM:    Result := 'PWRET_OFFGREATERINSTNUM';
    PWRET_OFFLOWERINSTNUM:      Result := 'PWRET_OFFLOWERINSTNUM';
    PWRET_OFFMANDATORYCVV:      Result := 'PWRET_OFFMANDATORYCVV';
    PWRET_OFFINVLASTFOUR:       Result := 'PWRET_OFFINVLASTFOUR';
    PWRET_OFFNOAID:             Result := 'PWRET_OFFNOAID';
    PWRET_OFFNOFALLBACK:        Result := 'PWRET_OFFNOFALLBACK';
    PWRET_OFFNOPINPAD:          Result := 'PWRET_OFFNOPINPAD';
    PWRET_OFFNOAPOFF:           Result := 'PWRET_OFFNOAPOFF';
    PWRET_OFFTRNNEEDPP:         Result := 'PWRET_OFFTRNNEEDPP';
    PWRET_OFFCARDNACCEPT:       Result := 'PWRET_OFFCARDNACCEPT';
    PWRET_OFFTABLEERR:          Result := 'PWRET_OFFTABLEERR';
    PWOFF_OFFMAXTABERR:         Result := 'PWOFF_OFFMAXTABERR';
    PWRET_OFFINTERNAL1:         Result := 'PWRET_OFFINTERNAL1';
    PWRET_OFFINTERNAL2:         Result := 'PWRET_OFFINTERNAL2';
    PWRET_OFFINTERNAL3:         Result := 'PWRET_OFFINTERNAL3';
    PWRET_OFFINTERNAL4:         Result := 'PWRET_OFFINTERNAL4';
    PWRET_OFFINTERNAL5:         Result := 'PWRET_OFFINTERNAL5';
    PWRET_OFFINTERNAL6:         Result := 'PWRET_OFFINTERNAL6';
    PWRET_OFFINTERNAL7:         Result := 'PWRET_OFFINTERNAL7';
    PWRET_OFFINTERNAL8:         Result := 'PWRET_OFFINTERNAL8';
    PWRET_OFFINTERNAL9:         Result := 'PWRET_OFFINTERNAL9';
    PWRET_OFFINTERNAL10:        Result := 'PWRET_OFFINTERNAL10';
    PWRET_OFFINTERNAL11:        Result := 'PWRET_OFFINTERNAL11';
    PWRET_OFFNOPRODUCT:         Result := 'PWRET_OFFNOPRODUCT';
    PWRET_OFFINTERNAL12:        Result := 'PWRET_OFFINTERNAL12';
    PWRET_OFFINTERNAL13:        Result := 'PWRET_OFFINTERNAL13';
    PWRET_OFFINTERNAL14:        Result := 'PWRET_OFFINTERNAL14';
    PWRET_NOPINPAD:             Result := 'PWRET_NOPINPAD';
    PWRET_OFFINTERNAL15:        Result := 'PWRET_OFFINTERNAL15';
    PWRET_OFFINTERNAL16:        Result := 'PWRET_OFFINTERNAL16';
    PWRET_ABECSERRCOM:          Result := 'PWRET_ABECSERRCOM';
    PWRET_OFFCFGNOCARDRANGE:    Result := 'PWRET_OFFCFGNOCARDRANGE';
    PWRET_OFFCFGNOPRODUCT:      Result := 'PWRET_OFFCFGNOPRODUCT';
    PWRET_OFFCFGNOTRANSACTION:  Result := 'PWRET_OFFCFGNOTRANSACTION';
    PWRET_OFFINTERNAL17:        Result := 'PWRET_OFFINTERNAL17';
    PWRET_OFFINTERNAL18:        Result := 'PWRET_OFFINTERNAL18';
    PWRET_PPABORT:              Result := 'PWRET_PPABORT';
    PWRET_OFFINTERNAL19:        Result := 'PWRET_OFFINTERNAL19';
    PWRET_PPERRTREATMENT:       Result := 'PWRET_PPERRTREATMENT';
    PWRET_INVPAYMENTMODE:       Result := 'PWRET_INVPAYMENTMODE';
    PWRET_OFFINVALIDOPER:       Result := 'PWRET_OFFINVALIDOPER';
    PWRET_OFFINTERNAL20:        Result := 'PWRET_OFFINTERNAL20';
    PWRET_OFFINTERNAL21:        Result := 'PWRET_OFFINTERNAL21';
    PWRET_PPS_OK:               Result := 'PWRET_PPS_OK';
    PWRET_PPS_PROCESSING:       Result := 'PWRET_PPS_PROCESSING';
    PWRET_PPS_NOTIFY:           Result := 'PWRET_PPS_NOTIFY';
    PWRET_PPS_F1:               Result := 'PWRET_PPS_F1';
    PWRET_PPS_F2:               Result := 'PWRET_PPS_F2';
    PWRET_PPS_F3:               Result := 'PWRET_PPS_F3';
    PWRET_PPS_F4:               Result := 'PWRET_PPS_F4';
    PWRET_PPS_BACKSP:           Result := 'PWRET_PPS_BACKSP';
    PWRET_PPS_INVCALL:          Result := 'PWRET_PPS_INVCALL';
    PWRET_PPS_INVPARM:          Result := 'PWRET_PPS_INVPARM';
    PWRET_PPS_TIMEOUT:          Result := 'PWRET_PPS_TIMEOUT';
    PWRET_PPS_TIMEOUT2:         Result := 'PWRET_PPS_TIMEOUT2';
    PWRET_PPS_CANCEL:           Result := 'PWRET_PPS_CANCEL';
    PWRET_PPS_CANCEL2:          Result := 'PWRET_PPS_CANCEL2';
    PWRET_PPS_ALREADYOPEN:      Result := 'PWRET_PPS_ALREADYOPEN';
    PWRET_PPS_NOTOPEN:          Result := 'PWRET_PPS_NOTOPEN';
    PWRET_PPS_EXECERR:          Result := 'PWRET_PPS_EXECERR';
    PWRET_PPS_INVMODEL:         Result := 'PWRET_PPS_INVMODEL';
    PWRET_PPS_NOFUNC:           Result := 'PWRET_PPS_NOFUNC';
    PWRET_PPS_TABEXP:           Result := 'PWRET_PPS_TABEXP';
    PWRET_PPS_TABERR:           Result := 'PWRET_PPS_TABERR';
    PWRET_PPS_NOAPPSLIC:        Result := 'PWRET_PPS_NOAPPSLIC';
    PWRET_PPS_PORTERR:          Result := 'PWRET_PPS_PORTERR';
    PWRET_PPS_COMMERR:          Result := 'PWRET_PPS_COMMERR';
    PWRET_PPS_UNKNOWNSTAT:      Result := 'PWRET_PPS_UNKNOWNSTAT';
    PWRET_PPS_RSPERR:           Result := 'PWRET_PPS_RSPERR';
    PWRET_PPS_COMMTOUT:         Result := 'PWRET_PPS_COMMTOUT';
    PWRET_PPS_INTERR:           Result := 'PWRET_PPS_INTERR';
    PWRET_PPS_MCDATAERR:        Result := 'PWRET_PPS_MCDATAERR';
    PWRET_PPS_ERRPIN:           Result := 'PWRET_PPS_ERRPIN';
    PWRET_PPS_NOCARD:           Result := 'PWRET_PPS_NOCARD';
    PWRET_PPS_PINBUSY:          Result := 'PWRET_PPS_PINBUSY';
    PWRET_PPS_SAMERR:           Result := 'PWRET_PPS_SAMERR';
    PWRET_PPS_NOSAM:            Result := 'PWRET_PPS_NOSAM';
    PWRET_PPS_SAMINV:           Result := 'PWRET_PPS_SAMINV';
    PWRET_PPS_DUMBCARD:         Result := 'PWRET_PPS_DUMBCARD';
    PWRET_PPS_ERRCARD:          Result := 'PWRET_PPS_ERRCARD';
    PWRET_PPS_CARDINV:          Result := 'PWRET_PPS_CARDINV';
    PWRET_PPS_CARDBLOCKED:      Result := 'PWRET_PPS_CARDBLOCKED';
    PWRET_PPS_CARDNAUTH:        Result := 'PWRET_PPS_CARDNAUTH';
    PWRET_PPS_CARDEXPIRED:      Result := 'PWRET_PPS_CARDEXPIRED';
    PWRET_PPS_CARDERRSTRUCT:    Result := 'PWRET_PPS_CARDERRSTRUCT';
    PWRET_PPS_CARDINVALIDAT:    Result := 'PWRET_PPS_CARDINVALIDAT';
    PWRET_PPS_CARDPROBLEMS:     Result := 'PWRET_PPS_CARDPROBLEMS';
    PWRET_PPS_CARDINVDATA:      Result := 'PWRET_PPS_CARDINVDATA';
    PWRET_PPS_CARDAPPNAV:       Result := 'PWRET_PPS_CARDAPPNAV';
    PWRET_PPS_CARDAPPNAUT:      Result := 'PWRET_PPS_CARDAPPNAUT';
    PWRET_PPS_NOBALANCE:        Result := 'PWRET_PPS_NOBALANCE';
    PWRET_PPS_LIMITEXC:         Result := 'PWRET_PPS_LIMITEXC';
    PWRET_PPS_CARDNOTEFFECT:    Result := 'PWRET_PPS_CARDNOTEFFECT';
    PWRET_PPS_VCINVCURR:        Result := 'PWRET_PPS_VCINVCURR';
    PWRET_PPS_ERRFALLBACK:      Result := 'PWRET_PPS_ERRFALLBACK';
    PWRET_PPS_CTLSSMULTIPLE:    Result := 'PWRET_PPS_CTLSSMULTIPLE';
    PWRET_PPS_CTLSSCOMMERR:     Result := 'PWRET_PPS_CTLSSCOMMERR';
    PWRET_PPS_CTLSSINVALIDAT:   Result := 'PWRET_PPS_CTLSSINVALIDAT';
    PWRET_PPS_CTLSSPROBLEMS:    Result := 'PWRET_PPS_CTLSSPROBLEMS';
    PWRET_PPS_CTLSSAPPNAV:      Result := 'PWRET_PPS_CTLSSAPPNAV';
    PWRET_PPS_CTLSSAPPNAUT:     Result := 'PWRET_PPS_CTLSSAPPNAUT';
    PWRET_PPS_XXX:              Result := 'PWRET_PPS_XXX';
  else
    Result := 'PWRET_'+IntToStr(iRET);
  end;
end;

function PWDATToString(bTipoDeDado: Byte): String;
begin
  case bTipoDeDado of
    PWDAT_MENU:         Result := 'PWDAT_MENU';
    PWDAT_TYPED:        Result := 'PWDAT_TYPED';
    PWDAT_CARDINF:      Result := 'PWDAT_CARDINF';
    PWDAT_PPENTRY:      Result := 'PWDAT_PPENTRY';
    PWDAT_PPENCPIN:     Result := 'PWDAT_PPENCPIN';
    PWDAT_CARDOFF:      Result := 'PWDAT_CARDOFF';
    PWDAT_CARDONL:      Result := 'PWDAT_CARDONL';
    PWDAT_PPCONF:       Result := 'PWDAT_PPCONF';
    PWDAT_BARCODE:      Result := 'PWDAT_BARCODE';
    PWDAT_PPREMCRD:     Result := 'PWDAT_PPREMCRD';
    PWDAT_PPGENCMD:     Result := 'PWDAT_PPGENCMD';
    PWDAT_PPDATAPOSCNF: Result := 'PWDAT_PPDATAPOSCNF';
    PWDAT_USERAUTH:     Result := 'PWDAT_USERAUTH';
    PWDAT_DSPCHECKOUT:  Result := 'PWDAT_DSPCHECKOUT';
    PWDAT_TSTKEY:       Result := 'PWDAT_TSTKEY';
    PWDAT_DSPQRCODE:    Result := 'PWDAT_DSPQRCODE';
  else
    Result := 'PWDAT_'+IntToStr(bTipoDeDado);
  end;
end;

function PWVALToString(bValidacaoDado: Byte): String;
begin
  case bValidacaoDado of
    PWVAL_Nenhuma:        Result := 'PWVAL_Nenhuma';
    PWVAL_NaoVazio:       Result := 'PWVAL_NaoVazio';
    PWVAL_Modulo10:       Result := 'PWVAL_Modulo10';
    PWVAL_CPF_CNPJ:       Result := 'PWVAL_CPF_CNPJ';
    PWVAL_MMAA:           Result := 'PWVAL_MMAA';
    PWVAL_DDMMAA:         Result := 'PWVAL_DDMMAA';
    PWVAL_DuplaDigitacao: Result := 'PWVAL_DuplaDigitacao';
  else
    Result := 'PWVAL_'+IntToStr(bValidacaoDado);
  end;
end;

function PWTYPToString(bTiposEntradaPermitidos: Byte): String;
begin
  case bTiposEntradaPermitidos of
    PWTYP_ReadOnly:   Result := 'PWTYP_ReadOnly';
    PWTYP_Numerico:   Result := 'PWTYP_Numerico';
    PWTYP_Alfabetico: Result := 'PWTYP_Alfabetico';
    PWTYP_AlfaNume:   Result := 'PWTYP_AlfaNume';
    PWTYP_AlfaNumEsp: Result := 'PWTYP_AlfaNumEsp';
  else
    Result := 'PWTYP_'+IntToStr(bTiposEntradaPermitidos);
  end;
end;

function PWDPINToString(iMessageId: Word): String;
begin
  case iMessageId of
    PWDPIN_DIGITE_O_DDD:                Result := 'PWDPIN_DIGITE_O_DDD';
    PWDPIN_REDIGITE_O_DDD:              Result := 'PWDPIN_REDIGITE_O_DDD';
    PWDPIN_DIGITE_O_TELEFONE:           Result := 'PWDPIN_DIGITE_O_TELEFONE';
    PWDPIN_REDIGITE_O_TELEFONE:         Result := 'PWDPIN_REDIGITE_O_TELEFONE';
    PWDPIN_DIGITE_DDD_TELEFONE:         Result := 'PWDPIN_DIGITE_DDD_TELEFONE';
    PWDPIN_REDIGITE_DDD_TELEFONE:       Result := 'PWDPIN_REDIGITE_DDD_TELEFONE';
    PWDPIN_DIGITE_O_CPF:                Result := 'PWDPIN_DIGITE_O_CPF';
    PWDPIN_REDIGITE_O_CPF:              Result := 'PWDPIN_REDIGITE_O_CPF';
    PWDPIN_DIGITE_O_RG:                 Result := 'PWDPIN_DIGITE_O_RG';
    PWDPIN_REDIGITE_O_RG:               Result := 'PWDPIN_REDIGITE_O_RG';
    PWDPIN_DIGITE_OS_4_ULTIMOS_DIGITOS: Result := 'PWDPIN_DIGITE_OS_4_ULTIMOS_DIGITOS';
    PWDPIN_DIGITE_CODIGO_DE_SEGURANCA:  Result := 'PWDPIN_DIGITE_CODIGO_DE_SEGURANCA';
    PWDPIN_DIGITE_O_CNPJ:               Result := 'PWDPIN_DIGITE_O_CNPJ';
    PWDPIN_REDIGITE_O_CNPJ:             Result := 'PWDPIN_REDIGITE_O_CNPJ';
  else
    Result := 'PWDPIN_'+IntToStr(iMessageId);
  end;
end;

{ TACBrTEFPGWebAPI }

constructor TACBrTEFPGWebAPI.Create;
begin
  inherited Create;
  ClearMethodPointers;

  fSuportaSaque := False;
  fSuportaDesconto := False;
  fSuportaViasDiferenciadas := True;
  fImprimeViaClienteReduzida := False;
  fUtilizaSaldoTotalVoucher := False;
  fRemocaoCartaoPinPad := False;
  fExibeMensagemCheckout := True;
  fExibicaoQRCode := qreAuto;
  fInicializada := False;
  fCarregada := False;
  fDiretorioTrabalho := '';
  fEmTransacao := False;
  fUsouPinPad := False;
  fTempoTarefasAutomaticas := '';
  fUltimoQRCode := '';

  fSoftwareHouse := '';
  fNomeAplicacao := '';
  fVersaoAplicacao := '';
  fNomeEstabelecimento := '';
  fCNPJEstabelecimento := '';
  fEnderecoIP := '';
  fPortaTCP := '';
  fPortaPinPad := 0;
  fConfirmarTransacoesPendentesNoHost := True;
  fPerguntarCartaoDigitadoAposCancelarLeitura := False;

  fDadosTransacao := TACBrTEFParametros.Create;
  fParametrosAdicionais := TACBrTEFParametros.Create;

  fOnGravarLog := Nil;
  fOnExibeMenu := Nil;
  fOnObtemCampo := Nil;
  fOnExibeMensagem := Nil;
  fOnExibeQRCode := Nil;
  fOnAguardaPinPad := Nil;
  fOnAvaliarTransacaoPendente := Nil;

  fTempoOcioso := 0;
  fTimerOcioso := TACBrThreadTimer.Create;
  fTimerOcioso.OnTimer := OnTimerOcioso;
  fTimerOcioso.Interval := CMilissegundosOcioso;
  fTimerOcioso.Enabled := False;
end;

destructor TACBrTEFPGWebAPI.Destroy;
begin
  //GravarLog('TACBrTEFPGWebAPI.Destroy');
  fDadosTransacao.Free;
  fParametrosAdicionais.Free;
  fTimerOcioso.Enabled := False;
  fTimerOcioso.Free;
  UnLoadLibFunctions;
  inherited Destroy;
end;

procedure TACBrTEFPGWebAPI.Inicializar;
var
  iRet: SmallInt;
  MsgError, ver: String;
begin
  if fInicializada then
    Exit;

  GravarLog('TACBrTEFPGWebAPI.Inicializar');

  if not Assigned(fOnObtemCampo) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnObtemCampo']));
  if not Assigned(fOnExibeMenu) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnExibeMenu']));
  if not Assigned(fOnExibeMensagem) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnExibeMensagem']));
  if not Assigned(fOnAguardaPinPad) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnAguardaPinPad']));

  if (not Assigned(fOnExibeQRCode)) and (ExibicaoQRCode = qreExibirNoCheckOut) then
    ExibicaoQRCode := qreExibirNoPinPad;

  if (fDiretorioTrabalho = '') then
    fDiretorioTrabalho := ApplicationPath + 'TEF' + PathDelim + 'PGWeb';

  if not DirectoryExists(fDiretorioTrabalho) then
    ForceDirectories(fDiretorioTrabalho);

  {$IfDef ANDROID}
   if (PathLib = '') then     // Try to load from "./assets/internal/" first
   begin
     PathLib := TPath.GetDocumentsPath;
     if not FileExists(LibFullName) then
       PathLib := '';
   end;
  {$EndIf}

  LoadLibFunctions;

  GravarLog('PW_iInit( '+fDiretorioTrabalho+' )');
  iRet := xPW_iInit(PAnsiChar(AnsiString(fDiretorioTrabalho)));
  GravarLog('  '+PWRETToString(iRet));
  case iRet of
    PWRET_OK: MsgError := '';
    PWRET_WRITERR: MsgError := Format(sErrPWRET_WRITERR, [fDiretorioTrabalho]);
    PWRET_INVCALL: MsgError := '';  //sErrPWRET_INVCALL;
  else
    MsgError := ObterUltimoRetorno;
  end;

  if (MsgError <> '') then
    DoException(ACBrStr(MsgError));

  fInicializada := True;
  SetEmTransacao(False);

  ver := VersaoLib;
  if CompareVersions(ver, CACBrTEFPGWebLibMinVersion) < 0 then
    DoException(Format( ACBrStr(sErrLibVersaoInvalida),
                        [LibFullName, ver, CACBrTEFPGWebLibMinVersion]) );
end;

procedure TACBrTEFPGWebAPI.DesInicializar;
begin
  GravarLog('TACBrTEFPGWebAPI.DesInicializar');
  UnLoadLibFunctions;
  fInicializada := False;
end;

procedure TACBrTEFPGWebAPI.ClearMethodPointers;
begin
  xPW_iInit := Nil;
  xPW_iGetResult := Nil;
  xPW_iNewTransac := Nil;
  xPW_iAddParam := Nil;
  xPW_iExecTransac := Nil;
  xPW_iConfirmation := Nil;
  xPW_iWaitConfirmation := Nil;
  xPW_iIdleProc := Nil;
  xPW_iGetOperations := Nil;
  xPW_iGetOperationsEx := Nil;
  xPW_iPPEventLoop := Nil;
  xPW_iPPAbort := Nil;
  xPW_iPPGetCard := Nil;
  xPW_iPPGetPIN := Nil;
  xPW_iPPGetData := Nil;
  xPW_iPPGoOnChip := Nil;
  xPW_iPPFinishChip := Nil;
  xPW_iPPConfirmData := Nil;
  xPW_iPPGenericCMD := Nil;
  xPW_iPPDataConfirmation := Nil;
  xPW_iPPTestKey := Nil;
  xPW_iPPDisplay := Nil;
  xPW_iPPGetUserData := Nil;
  xPW_iPPWaitEvent := Nil;
  xPW_iPPRemoveCard := Nil;
  xPW_iPPGetPINBlock := Nil;
  xPW_iPPCommTest := Nil;
  xPW_iTransactionInquiry := Nil;
end;

procedure TACBrTEFPGWebAPI.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFPayGoWeb: '+AErrorMsg);
  raise EACBrTEFPayGoWeb.Create(AErrorMsg);
end;

procedure TACBrTEFPGWebAPI.VerificarOK(iRET: SmallInt);
var
  MsgError: String;
begin
  if (iRET = PWRET_OK) then
    Exit;

  MsgError := ObterUltimoRetorno;
  if (MsgError <> '') then
    DoException(Format('%s (%s)', [ACBrStr(MsgError), PWRETToString(iRET)]));
end;

function TACBrTEFPGWebAPI.ObterInfo(iINFO: Word): String;
var
  pszData: PAnsiChar;
  ulDataSize: LongWord;
  iRet: SmallInt;
  MsgError: String;
begin
  LoadLibFunctions;
  Result := #0;
  ulDataSize := 10240;   // 10K
  pszData := AllocMem(ulDataSize);
  try
    GravarLog('PW_iGetResult( '+ PWINFOToString(iINFO)+' )');
    iRet := xPW_iGetResult(iINFO, pszData, ulDataSize);
    if (iRet = PWRET_OK) then
    begin
      Result := String(pszData);
      GravarLog('  '+Result, True);
    end
    else
    begin
      GravarLog('  '+PWRETToString(iRet));
      case iRet of
        PWRET_NODATA: MsgError := ''; //sErrPWRET_NODATA;
        PWRET_BUFOVFLW: MsgError := sErrPWRET_BUFOVFLW;
        PWRET_DLLNOTINIT: MsgError := sErrPWRET_DLLNOTINIT;
        PWRET_TRNNOTINIT: MsgError := sErrPWRET_TRNNOTINIT;
        PWRET_NOTINST: MsgError := sErrPWRET_NOTINST;
      else
        MsgError := PWRETToString(iRet);
      end;

      if (MsgError <> '') then
        DoException(ACBrStr(MsgError));
    end;
  finally
    Freemem(pszData);
  end;
end;

function TACBrTEFPGWebAPI.ObterUltimoRetorno: String;
begin
  Result := ObterInfo(PWINFO_RESULTMSG);
end;

procedure TACBrTEFPGWebAPI.AjustarTempoOcioso(const IdleTimeStr: String);
var
  AStr, AnoStr: String;
  IdleProcTime: TDateTime;
begin
  if (IdleTimeStr = '') and (fTempoOcioso > Now) then
    Exit;

  if (IdleTimeStr = '') then
    AStr := Trim(ObterInfo(PWINFO_IDLEPROCTIME))  // Formato “AAMMDDHHMMSS”.
  else
    AStr := IdleTimeStr;

  if (AStr <> '') then
  begin
    AnoStr := IntToStr(YearOf(Today));
    IdleProcTime := EncodeDateTime( StrToIntDef(Copy(AnoStr,1,2)+copy(AStr,1,2),0),  // YYYY
                                    StrToIntDef(copy(AStr, 3,2),0),  // MM
                                    StrToIntDef(copy(AStr, 5,2),0),  // DD
                                    StrToIntDef(copy(AStr, 7,2),0),  // hh
                                    StrToIntDef(copy(AStr, 9,2),0),  // nn
                                    StrToIntDef(copy(AStr,11,2),0),  // ss
                                    0 );
    if (IdleProcTime <> 0) then
    begin
      if (IdleProcTime < Now) then
        OnTimerOcioso(nil)

      else if (IdleProcTime <> fTempoOcioso) then
      begin
        fTimerOcioso.Enabled := False;
        fTimerOcioso.Interval := MilliSecondsBetween(now, IdleProcTime);
        fTimerOcioso.Enabled := True;
      end;

      fTempoOcioso := IdleProcTime;
    end;
  end;
end;

procedure TACBrTEFPGWebAPI.GravarLog(const AString: AnsiString; Traduz: Boolean);
Var
  Tratado: Boolean;
  AStringLog: AnsiString;
begin
  if not Assigned(fOnGravarLog) then
    Exit;

  if Traduz then
    AStringLog := TranslateUnprintable(AString)
  else
    AStringLog := AString;

  Tratado := False;
  fOnGravarLog(AStringLog, Tratado);
end;

procedure TACBrTEFPGWebAPI.IniciarTransacao(iOPER: Byte;
  ParametrosAdicionaisTransacao: TStrings);
var
  iRet: SmallInt;
  MsgError: String;
  i: Integer;
begin
  if EmTransacao then
    DoException(ACBrStr(sErrPWRET_TRNINIT));

  GravarLog('PW_iNewTransac( '+PWOPERToString(iOPER)+' )');
  iRet := xPW_iNewTransac(iOPER);
  GravarLog('  '+PWRETToString(iRet));
  if (iRet <> PWRET_OK) then
  begin
    case iRet of
      PWRET_DLLNOTINIT: MsgError := sErrPWRET_DLLNOTINIT;
      PWRET_NOTINST: MsgError := sErrPWRET_NOTINST;
    else
      MsgError := ObterUltimoRetorno;
    end;

    DoException(ACBrStr(MsgError));
  end;

  SetEmTransacao(True);
  try
    AdicionarDadosObrigatorios;

    For i := 0 to ParametrosAdicionais.Count-1 do
      AdicionarParametro(ParametrosAdicionais[i]);

    ParametrosAdicionais.Clear;  // Limpa para não usar nas próximas transações

    if Assigned(ParametrosAdicionaisTransacao) then
    begin
      For i := 0 to ParametrosAdicionaisTransacao.Count-1 do
        AdicionarParametro(ParametrosAdicionaisTransacao[i]);
    end;

  except
    On E: Exception do
    begin
      EstornarTransacaoEmAndamento;
      DoException(E.Message);
    end;
  end;
end;

procedure TACBrTEFPGWebAPI.AdicionarParametro(iINFO: Word; const AValor: AnsiString);
var
  iRet: SmallInt;
  MsgError: String;
begin
  GravarLog('PW_iAddParam( '+PWINFOToString(iINFO)+', '+AValor+' )');
  iRet := xPW_iAddParam(iINFO, PAnsiChar(AValor));
  GravarLog('  '+PWRETToString(iRet));
  if (iRet <> PWRET_OK) then
  begin
    case iRet of
      PWRET_INVPARAM: MsgError := Format(sErrPWRET_INVPARAM, [AValor, PWINFOToString(iINFO)]);
      PWRET_DLLNOTINIT: MsgError := sErrPWRET_DLLNOTINIT;
      PWRET_NOTINST: MsgError := sErrPWRET_NOTINST;
      PWRET_TRNNOTINIT: MsgError := sErrPWRET_TRNNOTINIT;
    else
      MsgError := ObterUltimoRetorno;
    end;

    DoException(ACBrStr(MsgError));
  end;
end;

procedure TACBrTEFPGWebAPI.AdicionarParametro(const AKeyValueStr: String);
var
  AInfo: Integer;
  AInfoStr, AValue: String;
begin
  if ParseKeyValue(AKeyValueStr, AInfoStr, AValue) then
  begin
    AInfo := StrToIntDef(AInfoStr, -1);
    if (AInfo >= 0) then
      AdicionarParametro(AInfo, AValue);
  end;
end;

function TACBrTEFPGWebAPI.ExecutarTransacao: Boolean;
var
  iRet, iRetPP: SmallInt;
  ArrParams: TArrPW_GetData;
  NumParams: SmallInt;
  MsgError, MsgProcess, MsgPinPad: String;
begin
  GravarLog('TACBrTEFPGWebAPI.ExecutarTransacao');
  fUltimoQRCode := '';
  fUsouPinPad := False;
  iRet := PWRET_CANCEL;
  try
    try
      MsgProcess := Trim(ObterInfo(PWINFO_PROCESSMSG));
      if (MsgProcess <> '') then
        ExibirMensagem(MsgProcess, tmCliente);

      iRet := PWRET_OK;
      while (iRet = PWRET_OK) or (iRet = PWRET_NOTHING) or (iRet = PWRET_MOREDATA) do
      begin
        {$IfDef FPC}
        Initialize(ArrParams);
        {$EndIf}
        NumParams := Length(ArrParams)-1;
        GravarLog('PW_iExecTransac()');
        iRet := xPW_iExecTransac(ArrParams, NumParams);
        GravarLog('  '+PWRETToString(iRet)+', NumParams: '+IntToStr(NumParams));

        case iRet of
          PWRET_OK:
            Break;
          PWRET_NOTHING:
            Sleep(CSleepNothing);
          PWRET_MOREDATA:
            iRet := ObterDados(ArrParams, NumParams);
        end;
      end;

      if (fUltimoQRCode <> '') then // Remove QRCode da tela, Se houver...
      begin
        fUltimoQRCode := '';
        ExibirQRCode(fUltimoQRCode);
      end;

      case iRet of
        PWRET_OK: MsgError := '';
        PWRET_CANCEL: MsgError := ObterInfo(PWINFO_CNCDSPMSG);
        PWRET_NOMANDATORY: MsgError := sErrPWRET_NOMANDATORY;
        PWRET_DLLNOTINIT: MsgError := sErrPWRET_DLLNOTINIT;
        PWRET_NOTINST: MsgError := sErrPWRET_NOTINST;
        PWRET_TRNNOTINIT: MsgError := sErrPWRET_TRNNOTINIT;
      else
        MsgError := ObterUltimoRetorno;
      end;
    except
      On E: Exception do
        MsgError := E.Message;
    end;

    // ERRO //
    if (Trim(MsgError) <> '') then
    begin
      ExibirMensagem(MsgError, tmOperador, CMilissegundosMensagem);

      MsgPinPad := ObterInfo(PWINFO_CNCPPMSG);
      if (Trim(MsgPinPad) <> '') then
      begin
        GravarLog('xPW_iPPDisplay( '+MsgPinPad+' )');
        iRetPP := xPW_iPPDisplay( PAnsiChar(AnsiString(MsgPinPad)) );
        GravarLog('  '+PWRETToString(iRetPP));
      end;

      EstornarTransacaoEmAndamento;
      if (iRet = PWRET_OK) then
        iRet := PWRET_INVCALL;
    end;
  finally
    ObterDadosDaTransacao;
    fDadosTransacao.ValueInfo[PWINFO_RET] := IntToStr(iRet);
    SetEmTransacao(False);
  end;

  if (iRet = PWRET_FROMHOSTPENDTRN) then
    TratarTransacaoPendente;

  Result := (iRet = PWRET_OK);
end;

function TACBrTEFPGWebAPI.AbortarTransacao: SmallInt;
begin
  GravarLog('PW_iPPAbort');
  Result := xPW_iPPAbort;
  GravarLog('  '+PWRETToString(Result));
  case Result of
    PWRET_OK:
      Result := PWRET_CANCEL;  // Sinaliza Cancelado, para função chamadora

    PWRET_PPCOMERR:
      DoException(ACBrStr(sErrPWRET_PPCOMERR));

    PWRET_DLLNOTINIT:
      DoException(ACBrStr(sErrPWRET_DLLNOTINIT));

  else
    DoException(ACBrStr(ObterUltimoRetorno));
  end;
end;

procedure TACBrTEFPGWebAPI.ObterDadosDaTransacao;
var
  pszData: PAnsiChar;
  ulDataSize: LongWord;
  iRet: SmallInt;
  i: Word;
  AData, InfoStr, TempoOcioso: String;
begin
  GravarLog('TACBrTEFPGWebAPI.ObterDadosDaTransacao');
  TempoOcioso := '';
  fDadosTransacao.Clear;
  ulDataSize := 10240;   // 10K
  pszData := AllocMem(ulDataSize);
  try
    For i := MIN_PWINFO to MAX_PWINFO do
    begin
      InfoStr := PWINFOToString(i);
      if (i <> PWINFO_PPINFO) and  // Ler PWINFO_PPINFO é lento (e desnecessário)
         (pos(IntToStr(i), InfoStr) = 0) then  // i equivale a um PWINFO_ conhecido ?
      begin
        //GravarLog('- '+InfoStr);
        iRet := xPW_iGetResult(i, pszData, ulDataSize);
        if (iRet = PWRET_OK) then
        begin
          AData := BinaryStringToString(AnsiString(pszData));
          GravarLog('  '+Format('%s=%s', [InfoStr, AData]));
          fDadosTransacao.Add(Format('%d=%s', [i, Adata]));  // Add é mais rápido que usar "ValueInfo[i]"
          if (i = PWINFO_IDLEPROCTIME) then
            TempoOcioso := AData;
        end;
      end;
    end;
  finally
    Freemem(pszData);
    //GravarLog('  Done');
    if (TempoOcioso <> '') then
      AjustarTempoOcioso(TempoOcioso);
  end;
end;

procedure TACBrTEFPGWebAPI.FinalizarTransacao(Status: LongWord;
  const pszReqNum: String; const pszLocRef: String; const pszExtRef: String;
  const pszVirtMerch: String; const pszAuthSyst: String);
var
  MsgError: String;
  iRet: SmallInt;
begin
  GravarLog('PW_iConfirmation( '+PWCNFToString(Status)+', '+
                                 pszReqNum+', '+
                                 pszLocRef+', '+
                                 pszExtRef+', '+
                                 pszVirtMerch+', '+
                                 pszAuthSyst+' ) ');
  iRet := xPW_iConfirmation( Status,
                             PAnsiChar(AnsiString(pszReqNum)),
                             PAnsiChar(AnsiString(pszLocRef)),
                             PAnsiChar(AnsiString(pszExtRef)),
                             PAnsiChar(AnsiString(pszVirtMerch)),
                             PAnsiChar(AnsiString(pszAuthSyst)) );
  GravarLog('  '+PWRETToString(iRet));
  if (iRet <> PWRET_OK) then
  begin
    case iRet of
      PWRET_DLLNOTINIT: MsgError := sErrPWRET_DLLNOTINIT;
      PWRET_NOTINST: MsgError := sErrPWRET_NOTINST;
      PWRET_INVALIDTRN: MsgError := sErrPWRET_INVALIDTRN;
    else
      MsgError := ObterUltimoRetorno;
    end;

    DoException(ACBrStr(MsgError));
  end;
end;

procedure TACBrTEFPGWebAPI.EstornarTransacaoEmAndamento;
var
  pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst: String;
begin
  GravarLog('TACBrTEFPGWebAPI.EstornarTransacaoEmAndamento');
  if EmTransacao and (Trim(ObterInfo(PWINFO_CNFREQ)) = '1') then
  begin
    pszReqNum := Trim(ObterInfo(PWINFO_REQNUM));
    pszLocRef := Trim(ObterInfo(PWINFO_AUTLOCREF));
    pszExtRef := Trim(ObterInfo(PWINFO_AUTEXTREF));
    pszVirtMerch := Trim(ObterInfo(PWINFO_VIRTMERCH));
    pszAuthSyst := Trim(ObterInfo(PWINFO_AUTHSYST));

    FinalizarTransacao(PWCNF_REV_ABORT, pszReqNum, pszLocRef, pszExtRef,
                                      pszVirtMerch, pszAuthSyst);
  end;
end;

procedure TACBrTEFPGWebAPI.TratarTransacaoPendente;
var
  pszAuthSyst, pszVirtMerch, pszReqNum, pszLocRef, pszExtRef: String;
  AStatus: LongWord;

  function ObterDadoTransacao(iINFO: Word): String;
  begin
    Result := fDadosTransacao.ValueInfo[iINFO];
    if (Result = '') then
      Result := Trim(ObterInfo(iINFO));
  end;

begin
  pszAuthSyst := ObterDadoTransacao(PWINFO_PNDAUTHSYST);
  pszVirtMerch := ObterDadoTransacao(PWINFO_PNDVIRTMERCH);
  pszReqNum := ObterDadoTransacao(PWINFO_PNDREQNUM);
  pszLocRef := ObterDadoTransacao(PWINFO_PNDAUTLOCREF);
  pszExtRef := ObterDadoTransacao(PWINFO_PNDAUTEXTREF);
  AStatus := PWCNF_CNF_MANU_AUT;

  if Assigned(fOnAvaliarTransacaoPendente) then
    fOnAvaliarTransacaoPendente(AStatus, pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst)
  else
  begin
    if not fConfirmarTransacoesPendentesNoHost then
      AStatus := PWCNF_REV_MANU_AUT;
  end;

  if (AStatus > 0) then
    FinalizarTransacao(AStatus, pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst);
end;

function TACBrTEFPGWebAPI.VerificarPresencaPinPad: Byte;
var
  iRetPP: SmallInt;
  pszMsg: PAnsiChar;
  uiMsgSize: Word;
  pbCommPort: Byte;
  MsgError: String;
begin
  Result := 0;
  pbCommPort := 0;
  uiMsgSize := 512;
  pszMsg := AllocMem(uiMsgSize); // 512 bytes
  try
    iRetPP := PWRET_NOTHING;
    while (iRetPP = PWRET_NOTHING) do
    begin
      GravarLog('PW_iPPCommTest');
      iRetPP := xPW_iPPCommTest( pszMsg, uiMsgSize, pbCommPort );
      GravarLog('  '+PWRETToString(iRetPP)+', '+String(pszMsg)+', '+IntToStr(pbCommPort));
      MsgError := '';
      case iRetPP of
        PWRET_OK:
          begin
            Result := pbCommPort;
            Break;
          end;
        PWRET_DLLNOTINIT:
          MsgError := sErrPWRET_DLLNOTINIT;
        PWRET_INVPARAM:
          MsgError := sErrPWRET_INVPARAM2;
        PWRET_NOTHING:
          ExibirMensagem(String(pszMsg), tmOperador);
      else
        MsgError := ObterUltimoRetorno;
      end;

      if (MsgError <> '') then
      begin
        DoException(ACBrStr(MsgError));
        Break;
      end;
    end;
  finally
    Freemem(pszMsg);
  end;
end;

procedure TACBrTEFPGWebAPI.ExibirMensagemPinPad(const MsgPinPad: String);
var
  iRetPP: SmallInt;
  MsgError, AMsg: String;
begin
  AMsg := StringReplace(MsgPinPad, '|', CR, [rfReplaceAll]);
  GravarLog('PW_iPPDisplay( '+AMsg+' )');
  iRetPP := xPW_iPPDisplay( PAnsiChar(AnsiString(AMsg)) );
  GravarLog('  '+PWRETToString(iRetPP));

  MsgError := '';
  if (iRetPP <> PWRET_OK) then
    MsgError := ObterUltimoRetorno;

  if (MsgError <> '') then
    DoException(ACBrStr(MsgError));
end;

function TACBrTEFPGWebAPI.ObterDadoPinPad(iMessageId: Word; MinLen,
  MaxLen: Byte; TimeOutSec: SmallInt): String;
var
  iRet: SmallInt;
  MsgError: String;
  pszData: PAnsiChar;
begin
  Result := '';
  MsgError := '';
  GravarLog('PW_iPPGetUserData( '+PWDPINToString(iMessageId)+', '+
                                  IntToStr(MinLen)+', '+
                                  IntToStr(MaxLen)+', '+
                                  IntToStr(TimeOutSec)+' )');

  pszData := AllocMem(max(50, MaxLen));
  try
    iRet := xPW_iPPGetUserData(iMessageId, MinLen, MaxLen, TimeOutSec, pszData);
    GravarLog('  '+PWRETToString(iRet));
    case iRet of
      PWRET_OK:
      begin
        Result := String(pszData);
        GravarLog('  '+Result, True);
      end;

      PWRET_CANCEL, PWRET_PPS_CANCEL, PWRET_PPS_CANCEL2,
      PWRET_TIMEOUT, PWRET_PPS_TIMEOUT, PWRET_PPS_TIMEOUT2:
        MsgError := '';
      PWRET_DLLNOTINIT: MsgError := sErrPWRET_DLLNOTINIT;
      PWRET_NOTINST: MsgError := sErrPWRET_NOTINST;
      PWRET_PPCOMERR: MsgError := sErrPWRET_PPCOMERR;
      PWRET_INVCALL: MsgError := sErrPWRET_INVCALL3;
    else
      MsgError := ObterUltimoRetorno;
    end;

    if (MsgError <> '') then
      DoException(ACBrStr(MsgError));
  finally
    Freemem(pszData);
  end;
end;

function TACBrTEFPGWebAPI.VersaoLib: String;
begin
  if not fInicializada then
    Inicializar;

  IniciarTransacao(PWOPER_VERSION);
  ExecutarTransacao;
  Result := fDadosTransacao.ValueInfo[PWINFO_RESULTMSG];
end;

procedure TACBrTEFPGWebAPI.ObterOperacoes(TipoOperacao: Byte;
  Operacoes: TArrPW_OperationsEx);
var
  iRet, piNumOperations, iStructSize: SmallInt;
  MsgError: String;
begin
  if not Assigned(xPW_iGetOperationsEx) then
    Exit;

  piNumOperations := Length(Operacoes);
  iStructSize := SizeOf(TPW_OperationsEx);
  GravarLog('xPW_iGetOperationsEx');
  iRet := xPW_iGetOperationsEx(TipoOperacao, Operacoes, piNumOperations, iStructSize );
  GravarLog('  '+PWRETToString(iRet));
  case iRet of
    PWRET_DLLNOTINIT: MsgError := sErrPWRET_DLLNOTINIT;
    PWRET_NOTINST: MsgError := sErrPWRET_NOTINST;
  else
    MsgError := ObterUltimoRetorno;
  end;

  if (MsgError <> '') then
    DoException(ACBrStr(MsgError));
end;

function TACBrTEFPGWebAPI.ValidarRespostaCampo(var AResposta: String;
  ADefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo): String;
var
  Valido: Boolean;
  ARespInt: Int64;
  Erro: String;
begin
  Valido := True;
  Erro := Trim(ADefinicaoCampo.MsgValidacao);

  case ADefinicaoCampo.ValidacaoDado of
    pgvDigMod10, pgvCPF_CNPJ, pgvMMAA, pgvDDMMAA:
      begin
        AResposta := OnlyNumber(AResposta);

        case ADefinicaoCampo.ValidacaoDado of
          pgvDigMod10:
            Valido := ValidarModulo10(AResposta);
          pgvCPF_CNPJ:
            Valido := (ACBrValidador.ValidarCNPJouCPF(AResposta) = '');
          pgvMMAA:
            Valido := ValidarDDMM(AResposta);
          pgvDDMMAA:
            Valido := ValidarDDMMAA(AResposta);
        end;
      end;
  else
    if (ADefinicaoCampo.ValidacaoDado = pgvNaoVazio) then
      Valido := (AResposta <> '');

    if Valido and (ADefinicaoCampo.TiposEntradaPermitidos = pgtNumerico) then
    begin
      ARespInt := StrToInt64Def(AResposta, -1);
      if (ADefinicaoCampo.ValorMaximo > 0) and (ARespInt > ADefinicaoCampo.ValorMaximo) then
      begin
        Valido := False;
        Erro := Trim(ADefinicaoCampo.MsgDadoMaior)
      end
      else if (ADefinicaoCampo.ValorMinimo > 0) and (ARespInt < ADefinicaoCampo.ValorMinimo) then
      begin
        Valido := False;
        Erro := Trim(ADefinicaoCampo.MsgDadoMenor);
      end
    end
  end;

  if Valido then
    Result := ''
  else
    Result := Erro;
end;

function TACBrTEFPGWebAPI.ObterDados(ArrGetData: TArrPW_GetData;
  ArrLen: SmallInt): SmallInt;
var
  AGetData: TPW_GetData;
  i, j: Integer;
  AMsg, DadosQRCode: String;
  iRet: SmallInt;
  Cancelado: Boolean;
begin
  GravarLog('TACBrTEFPGWebAPI.ObterDados( '+IntToStr(ArrLen)+' )');

  iRet := PWRET_OK;
  i := 0;
  while (iRet = PWRET_OK) and (i < ArrLen) do
  begin
    AGetData := ArrGetData[i];
    AGetData.bIndice := i;
    LogPWGetData(AGetData);

    if not ObterDadosDeParametrosAdicionais(AGetData) then
    begin
      AMsg := Trim(AGetData.szMsgPrevia);
      if (AMsg <> '') then
        ExibirMensagem(AMsg, tmOperador, CMilissegundosMensagem);

      j := 1;
      while (iRet = PWRET_OK) and (j <= max(AGetData.bNumeroCapturas,1)) do
      begin
        case AGetData.bTipoDeDado of
          PWDAT_NONE:
            ;  // Registro inválido, ignore...
          PWDAT_MENU:
            iRet := ObterDadoMenu(AGetData);
          PWDAT_TYPED, PWDAT_USERAUTH:
            iRet := ObterDadoDigitado(AGetData);
          PWDAT_CARDINF:
            iRet := ObterDadoCartao(AGetData);
          PWDAT_PPENTRY:
            iRet := RealizarOperacaoPinPad(AGetData, ppGetData);
          PWDAT_PPENCPIN:
            iRet := RealizarOperacaoPinPad(AGetData, ppGetPIN);
          PWDAT_CARDOFF:
            iRet := RealizarOperacaoPinPad(AGetData, ppGoOnChip);
          PWDAT_CARDONL:
            iRet := RealizarOperacaoPinPad(AGetData, ppFinishChip);
          PWDAT_PPCONF:
            iRet := RealizarOperacaoPinPad(AGetData, ppConfirmData);
          PWDAT_BARCODE:
            iRet := ObterDadoCodBarra(AGetData);
          PWDAT_PPREMCRD:
          begin
            if fUsouPinPad then
            begin
              ExibirMensagem(sInfoRemovaCartao);
              fUsouPinPad := False;
            end;
            iRet := RealizarOperacaoPinPad(AGetData, ppRemoveCard);
          end;

          PWDAT_PPGENCMD:
            iRet := RealizarOperacaoPinPad(AGetData, ppGenericCMD);
          PWDAT_PPDATAPOSCNF:
            iRet := RealizarOperacaoPinPad(AGetData, ppDataConfirmation);
          PWDAT_TSTKEY:
            iRet := RealizarOperacaoPinPad(AGetData, ppTestKey);
          PWDAT_DSPCHECKOUT:
          begin
            AMsg := Trim(AGetData.szPrompt);
            if (AMsg <> '') then
              ExibirMensagem(AMsg, tmCliente);

            Cancelado := False;
            ChamarOnAguardaPinPad(ppLerQRCode, Cancelado);
            if Cancelado then
              iRet := AbortarTransacao
            else
              AdicionarParametro(AGetData.wIdentificador, '');
          end;

          PWDAT_DSPQRCODE:
          begin
            DadosQRCode := ObterInfo(PWINFO_AUTHPOSQRCODE);
            if (fUltimoQRCode <> DadosQRCode) then
            begin
              fUltimoQRCode := DadosQRCode;
              ExibirQRCode(DadosQRCode);
            end;

            AMsg := Trim(AGetData.szPrompt);
            if (AMsg <> '') then
              ExibirMensagem(AMsg, tmCliente);

            Cancelado := False;
            ChamarOnAguardaPinPad(ppLerQRCode, Cancelado);
            if Cancelado then
              iRet := AbortarTransacao
            else
              AdicionarParametro(AGetData.wIdentificador, '');
          end
        else
          DoException(Format(ACBrStr(sErrPWDAT_UNKNOWN), [AGetData.bTipoDeDado]));
        end;

        Inc(j);
      end;
    end;

    Inc(i);
  end;

  Result := iRet;
end;

function TACBrTEFPGWebAPI.ObterDadosDeParametrosAdicionais(AGetData: TPW_GetData
  ): Boolean;
var
  i, m: Integer;
  AResposta, AKey: String;
  Ok: Boolean;
begin
  Result := False;
  if not (AGetData.bTipoDeDado in [PWDAT_MENU, PWDAT_TYPED, PWDAT_USERAUTH]) then
    Exit;

  AResposta := '';
  i := fParametrosAdicionais.IndexOfName(IntToStr(AGetData.wIdentificador));
  if (i >= 0) then
  begin
    if not ParseKeyValue(fParametrosAdicionais[i], AKey, AResposta) then
      DoException(Format(ACBrStr(sErrPWINF_INVALID), [fParametrosAdicionais[i], PWINFOToString(AGetData.wIdentificador)]));
  end
  else
  begin
    case AGetData.wIdentificador of
      PWINFO_MERCHNAMEPDC:
        AResposta := NomeEstabelecimento;
      PWINFO_MERCHCNPJCPF:
        AResposta := OnlyNumber(CNPJEstabelecimento);
      PWINFO_POSID:
        AResposta := PontoCaptura;
      PWINFO_USINGPINPAD:
        AResposta := IfThen(PortaPinPad >= 0, '1','0');
      PWINFO_PPCOMMPORT:
        AResposta := IntToStr(PortaPinPad);
      PWINFO_DESTTCPIP:
      begin
        if (EnderecoIP <> '') then
        begin
          if (PortaTCP = '') then
            PortaTCP := '17502';
          AResposta := EnderecoIP+':'+PortaTCP;
        end;
      end;
    end
  end;

  Result := (AResposta <> '');
  if Result then
  begin
    GravarLog( '  ParametrosAdicionais' );
    m := 0;
    Ok := (m > AGetData.bNumOpcoesMenu-1);
    while (not Ok) and (m < AGetData.bNumOpcoesMenu) do
    begin
      Ok := (AResposta = AGetData.vszValorMenu[m]);
      Inc(m);
    end;

    if not Ok then
      DoException(Format(ACBrStr(sErrPWINF_INVALID), [AResposta, PWINFOToString(AGetData.wIdentificador)]));

    AdicionarParametro(AGetData.wIdentificador, AResposta);
  end;
end;

function TACBrTEFPGWebAPI.ObterDadoMenu(AGetData: TPW_GetData): SmallInt;
var
  SL: TStringList;
  ItemSelecionado, i: Integer;
  AOpcao: String;
  Cancelado: Boolean;
begin
  Cancelado := False;
  SL := TStringList.Create;
  try
    for i := 0 to AGetData.bNumOpcoesMenu-1 do
    begin
      AOpcao := Trim(AGetData.vszTextoMenu[i]);
      if (AGetData.bTeclasDeAtalho = 1) then
        AOpcao := IntToStr(i+1)+' - '+AOpcao;

      SL.Add(AOpcao);
    end;

    ItemSelecionado := AGetData.bItemInicial;
    GravarLog('  OnExibeMenu( '+AGetData.szPrompt+' )', True);
    fOnExibeMenu(Trim(AGetData.szPrompt), SL, ItemSelecionado, Cancelado);
    GravarLog('    Resposta: '+IntToStr(ItemSelecionado)+', Cancelado: '+BoolToStr(Cancelado, True));

    Cancelado := Cancelado or (ItemSelecionado < 0) or (ItemSelecionado >= AGetData.bNumOpcoesMenu);

    if not Cancelado then
      AdicionarParametro(AGetData.wIdentificador, AGetData.vszValorMenu[ItemSelecionado]);
  finally
    SL.Free;
  end;

  Result := IfThen(Cancelado, PWRET_CANCEL, PWRET_OK);
end;

function TACBrTEFPGWebAPI.ObterDadoDigitado(AGetData: TPW_GetData): SmallInt;
var
  AResposta: String;
begin
  AResposta := '';
  if ObterDadoDigitadoGenerico(AGetData, AResposta) then
  begin
    if (AGetData.wIdentificador = PWINFO_MERCHCNPJCPF) then
      AResposta := OnlyNumber(AResposta);

    Result := PWRET_OK;
    AdicionarParametro(AGetData.wIdentificador, AResposta);
  end
  else
    Result := PWRET_CANCEL;
end;

function TACBrTEFPGWebAPI.ObterDadoDigitadoGenerico(AGetData: TPW_GetData;
  var AResposta: String): Boolean;
var
  ARespostaAnterior, MsgValidacao: String;
  Cancelado, Valido: Boolean;
  ADefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo;
begin
  ADefinicaoCampo := PW_GetDataToDefinicaoCampo(AGetData);

  AResposta := ADefinicaoCampo.ValorInicial;
  ARespostaAnterior := '';
  Valido := False;
  Cancelado := False;

  repeat
    GravarLog('  OnObtemCampo');
    fOnObtemCampo(ADefinicaoCampo, AResposta, Valido, Cancelado);
    GravarLog('    Resposta: '+AResposta+', Valido: '+BoolToStr(Valido, True)+
                                         ', Cancelado: '+BoolToStr(Cancelado, True));

    if not (Valido or Cancelado) then
    begin
      AResposta := Trim(AResposta);
      MsgValidacao := '';
      if (ADefinicaoCampo.ValidacaoDado = pgvDuplaDigitacao) then
      begin
        if (ARespostaAnterior = '') then
        begin
          ARespostaAnterior := AResposta;
          ADefinicaoCampo.Titulo := Trim(AGetData.szMsgConfirmacao);
          AResposta := ADefinicaoCampo.ValorInicial;
          Continue;
        end
        else
        begin
          if (AResposta <> ARespostaAnterior)then
            MsgValidacao := ADefinicaoCampo.MsgValidacao;
        end
      end
      else
        MsgValidacao := ValidarRespostaCampo(AResposta, ADefinicaoCampo);

      Valido := (MsgValidacao = '');

      if not (Valido or ADefinicaoCampo.OmiteMsgAlerta) then
        ExibirMensagem(MsgValidacao, tmOperador, CMilissegundosMensagem);
    end;
  until (Valido or Cancelado);

  Result := not Cancelado;
end;

function TACBrTEFPGWebAPI.ObterDadoCodBarra(AGetData: TPW_GetData): SmallInt;
var
  AResposta: String;
begin
  AResposta := '';
  if ObterDadoDigitadoGenerico(AGetData, AResposta) then
  begin
    Result := PWRET_OK;
    AdicionarParametro(PWINFO_BARCODE, AResposta);
    AdicionarParametro(PWINFO_BARCODENTMODE, IntToStr(AGetData.bTipoEntradaCodigoBarras));
  end
  else
    Result := PWRET_CANCEL;
end;

function TACBrTEFPGWebAPI.ObterDadoCartao(AGetData: TPW_GetData): SmallInt;
var
  ObterDigitado: Boolean;
  iRet: SmallInt;
begin
  iRet := PWRET_CANCEL;
  ObterDigitado := False;

  case AGetData.ulTipoEntradaCartao of
    1: ObterDigitado := True;

    2:
    begin
      iRet := RealizarOperacaoPinPad(AGetData, ppGetCard);
      ObterDigitado := (iRet = PWRET_FALLBACK);
    end;

  else  // 0 ou 3
    begin
      iRet := RealizarOperacaoPinPad(AGetData, ppGetCard);
      ObterDigitado := (iRet = PWRET_FALLBACK) or
                       (fPerguntarCartaoDigitadoAposCancelarLeitura and (iRet = PWRET_CANCEL));
    end;
  end;

  if ObterDigitado then
    Result := ObterDadoCartaoDigitado(AGetData)
  else
    Result := iRet;
end;

function TACBrTEFPGWebAPI.RealizarOperacaoPinPad(AGetData: TPW_GetData;
  OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad): SmallInt;
var
  iRet: SmallInt;
begin
  fUsouPinPad := True;

  iRet := PWRET_CANCEL;
  case OperacaoPinPad of
    ppGetCard:
    begin
      GravarLog('PW_iPPGetCard( '+IntToStr(AGetData.bIndice)+' )');
      iRet := xPW_iPPGetCard(AGetData.bIndice);
    end;
    ppGetData:
    begin
      GravarLog('PW_iPPGetData( '+IntToStr(AGetData.bIndice)+' )');
      iRet := xPW_iPPGetData(AGetData.bIndice);
    end;
    ppGetPIN:
    begin
      GravarLog('PW_iPPGetPIN( '+IntToStr(AGetData.bIndice)+' )');
      iRet := xPW_iPPGetPIN(AGetData.bIndice);
    end;
    ppGoOnChip:
    begin
      GravarLog('PW_iPPGoOnChip( '+IntToStr(AGetData.bIndice)+' )');
      iRet := xPW_iPPGoOnChip(AGetData.bIndice);
    end;
    ppFinishChip:
    begin
      GravarLog('PW_iPPFinishChip( '+IntToStr(AGetData.bIndice)+' )');
      iRet := xPW_iPPFinishChip(AGetData.bIndice);
    end;
    ppConfirmData:
    begin
      GravarLog('PW_iPPConfirmData( '+IntToStr(AGetData.bIndice)+' )');
      iRet := xPW_iPPConfirmData(AGetData.bIndice);
    end;
    ppRemoveCard:
    begin
      GravarLog('PW_iPPRemoveCard');
      iRet := xPW_iPPRemoveCard();
    end;
    ppGenericCMD:
    begin
      GravarLog('PW_iPPGenericCMD( '+IntToStr(AGetData.bIndice)+' )');
      iRet := xPW_iPPGenericCMD(AGetData.bIndice);
    end;
    ppDataConfirmation:
    begin
      if Assigned(xPW_iPPDataConfirmation) then
      begin
        GravarLog('PW_iPPDataConfirmation( '+IntToStr(AGetData.bIndice)+' )');
        iRet := xPW_iPPDataConfirmation(AGetData.bIndice);
      end;
    end;
    ppTestKey:
    begin
      if Assigned(xPW_iPPTestKey) then
      begin
        GravarLog('PW_iPPTestKey( '+IntToStr(AGetData.bIndice)+' )');
        iRet := xPW_iPPTestKey(AGetData.bIndice);
      end;
    end
  else
    DoException(ACBrStr(sErrPWRET_NOMANDATORY));
  end;

  GravarLog('  '+PWRETToString(iRet));
  case iRet of
    PWRET_OK:
      iRet := AguardarOperacaoPinPad(OperacaoPinPad);

    PWRET_DLLNOTINIT:
      DoException(ACBrStr(sErrPWRET_DLLNOTINIT));

    PWRET_INVPARAM:
      DoException(ACBrStr(sErrPWRET_INVPARAM2));

  else
    DoException(ACBrStr(ObterUltimoRetorno));
  end;

  Result := iRet;
end;

function TACBrTEFPGWebAPI.AguardarOperacaoPinPad(
  OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad): SmallInt;
var
  iRet: SmallInt;
  pszDisplay: PAnsiChar;
  Cancelado: Boolean;
  AMsg: String;
  ulDisplaySize: LongWord;
begin
  Cancelado := False;
  ulDisplaySize := 512;
  pszDisplay := AllocMem(ulDisplaySize); // 512 bytes
  try
    iRet := PWRET_CANCEL;
    while not Cancelado do
    begin
      GravarLog('PW_iPPEventLoop');
      iRet := xPW_iPPEventLoop(pszDisplay, ulDisplaySize);
      GravarLog('  '+PWRETToString(iRet));

      case iRet of
        PWRET_NOTHING:
          Sleep(CSleepNothing);

        PWRET_DISPLAY:
        begin
          AMsg := String(pszDisplay);
          ExibirMensagem(AMsg);
        end;

        PWRET_OK, PWRET_CANCEL, PWRET_TIMEOUT, PWRET_FALLBACK:
          Break;

        PWRET_PPCOMERR:
          DoException(ACBrStr(sErrPWRET_PPCOMERR));

        PWRET_DLLNOTINIT:
          DoException(ACBrStr(sErrPWRET_DLLNOTINIT));

        PWRET_INVCALL:
          DoException(ACBrStr(sErrPWRET_INVCALL2));

      else
        DoException(ACBrStr(ObterUltimoRetorno));
      end;

      ChamarOnAguardaPinPad(OperacaoPinPad, Cancelado);
    end;
  finally
    Freemem(pszDisplay);
  end;

  if Cancelado then
    iRet := AbortarTransacao;

  Result := iRet;
end;

function TACBrTEFPGWebAPI.ObterDadoCartaoDigitado(AGetData: TPW_GetData
  ): SmallInt;
var
  AResposta: String;
  AStr: AnsiString;
begin
  AResposta := '';
  if AGetData.bTiposEntradaPermitidos = PWTYP_ReadOnly then
    AGetData.bTiposEntradaPermitidos := PWTYP_Numerico;

  if ObterDadoDigitadoGenerico(AGetData, AResposta) then
  begin
    Result := PWRET_OK;
    AdicionarParametro(PWINFO_CARDFULLPAN, AResposta);

    if (AGetData.bCapturarDataVencCartao = 0) then
    begin
      AStr := sPerVenctoCartao + NUL;
      Move(AStr[1], AGetData.szPrompt[0], Length(AStr));
      AStr := '@@/@@' + NUL;
      Move(AStr[1], AGetData.szMascaraDeCaptura[0], Length(AStr));
      AGetData.bValidacaoDado := PWVAL_MMAA;
      AGetData.szValorInicial[0] := NUL;

      AResposta := '';
      if ObterDadoDigitadoGenerico(AGetData, AResposta) then
        AdicionarParametro(PWINFO_CARDEXPDATE, AResposta)
      else
        Result := PWRET_CANCEL;
    end;
  end
  else
    Result := PWRET_CANCEL;
end;

procedure TACBrTEFPGWebAPI.ExibirMensagem(const AMsg: String;
  Terminal: TACBrTEFPGWebAPITerminalMensagem; TempoEspera: Integer);
var
  wMsg: String;
begin
  GravarLog('  OnExibeMensagem( '+AMsg+
                                ', '+GetEnumName(TypeInfo(TACBrTEFPGWebAPITerminalMensagem), integer(Terminal) )+
                                ', '+IntToStr(TempoEspera)+' )', True);
  if (copy(AMsg,1,1) = CR) then
    wMsg := copy(AMsg, 2, Length(AMsg))
  else
    wMsg := AMsg;

  fOnExibeMensagem(wMsg, Terminal, TempoEspera);
end;

procedure TACBrTEFPGWebAPI.ExibirQRCode(const Dados: String);
begin
  GravarLog('  OnExibeQRCode( '+Dados+' )');
  fOnExibeQRCode(Dados);
end;

procedure TACBrTEFPGWebAPI.ChamarOnAguardaPinPad(
  OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad; var Cancelado: Boolean);
begin
  Cancelado := False;
  GravarLog('  OnAguardaPinPad( '+GetEnumName(TypeInfo(TACBrTEFPGWebAPIOperacaoPinPad),
                                              integer(OperacaoPinPad))+' )');
  fOnAguardaPinPad(OperacaoPinPad, Cancelado);
  GravarLog('    Cancelado: '+BoolToStr(Cancelado, True) );
end;

function TACBrTEFPGWebAPI.PW_GetDataToDefinicaoCampo(AGetData: TPW_GetData
  ): TACBrTEFPGWebAPIDefinicaoCampo;
begin
  Result.Titulo := Trim(AGetData.szPrompt);
  Result.MascaraDeCaptura := Trim(AGetData.szMascaraDeCaptura);
  Result.TiposEntradaPermitidos := TACBrTEFPGWebAPITiposEntrada(AGetData.bTiposEntradaPermitidos);
  Result.TamanhoMinimo := AGetData.bTamanhoMinimo;
  Result.TamanhoMaximo := AGetData.bTamanhoMaximo;
  Result.ValorMinimo := AGetData.ulValorMinimo;
  Result.ValorMaximo := AGetData.ulValorMaximo;
  Result.OcultarDadosDigitados := (AGetData.bOcultarDadosDigitados = 1);
  Result.ValidacaoDado := TACBrTEFPGWebAPIValidacaoDado(AGetData.bValidacaoDado);
  Result.AceitaNulo := (AGetData.bAceitaNulo = 1);
  Result.ValorInicial := Trim(AGetData.szValorInicial);
  Result.bTeclasDeAtalho := (AGetData.bTeclasDeAtalho = 1);
  Result.MsgValidacao := Trim(AGetData.szMsgValidacao);
  Result.MsgConfirmacao := Trim(AGetData.szMsgConfirmacao);
  Result.MsgDadoMaior := Trim(AGetData.szMsgDadoMaior);
  Result.MsgDadoMenor := Trim(AGetData.szMsgDadoMenor);
  Result.TipoEntradaCodigoBarras := TACBrTEFPGWebAPITipoBarras(AGetData.bTipoEntradaCodigoBarras);
  Result.OmiteMsgAlerta := (AGetData.bOmiteMsgAlerta = 1);

  // Verificando tipos inválidos, que podem ser retornados pela API
  if (Result.TiposEntradaPermitidos < Low(TACBrTEFPGWebAPITiposEntrada)) or
     (Result.TiposEntradaPermitidos > High(TACBrTEFPGWebAPITiposEntrada)) then
    Result.TiposEntradaPermitidos := pgtAlfaNumEsp;

  if (Result.ValidacaoDado < Low(TACBrTEFPGWebAPIValidacaoDado)) or
     (Result.ValidacaoDado > pgvDuplaDigitacao) then
    Result.ValidacaoDado := pgvNenhuma;

  case AGetData.wIdentificador of
    PWINFO_AUTHMNGTUSER:
    begin
      Result.ValidacaoDado := pgvSenhaLojista;
      Result.TiposEntradaPermitidos := pgtAlfaNumEsp;
      Result.OcultarDadosDigitados := True;
      if (Result.Titulo = '') then
        Result.Titulo := 'INFORME A SENHA DO GERENTE';
    end;
    PWINFO_AUTHTECHUSER:
    begin
      Result.ValidacaoDado := pgvSenhaTecnica;
      Result.TiposEntradaPermitidos := pgtAlfaNumEsp;
      Result.OcultarDadosDigitados := True;
      if (Result.Titulo = '') then
        Result.Titulo := ACBrStr('INFORME A SENHA TÉCNICA');
    end;
  end;
end;

procedure TACBrTEFPGWebAPI.LogPWGetData(AGetData: TPW_GetData);
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add(' PW_GetData: '+IntToStr(AGetData.bIndice));
    SL.Add('  wIdentificador: ' + PWINFOToString(AGetData.wIdentificador));
    SL.Add('  bTipoDeDado: ' + PWDATToString(AGetData.bTipoDeDado));
    SL.Add('  szPrompt: ' + TranslateUnprintable(AGetData.szPrompt));
    SL.Add('  bNumOpcoesMenu: ' + IntToStr(AGetData.bNumOpcoesMenu));
    for i := 0 to AGetData.bNumOpcoesMenu-1 do
    begin
      SL.Add('  vszTextoMenu_'+IntToStr(i)+': '+AGetData.vszTextoMenu[i]);
      SL.Add('  vszValorMenu_'+IntToStr(i)+': '+AGetData.vszValorMenu[i]);
    end;
    SL.Add('  szMascaraDeCaptura: '+AGetData.szMascaraDeCaptura);
    SL.Add('  bTiposEntradaPermitidos: '+PWTYPToString(AGetData.bTiposEntradaPermitidos));
    SL.Add('  bTamanhoMinimo: '+IntToStr(AGetData.bTamanhoMinimo));
    SL.Add('  bTamanhoMaximo: '+IntToStr(AGetData.bTamanhoMaximo));
    SL.Add('  ulValorMinimo: '+IntToStr(AGetData.ulValorMinimo));
    SL.Add('  ulValorMaximo: '+IntToStr(AGetData.ulValorMaximo));
    SL.Add('  bOcultarDadosDigitados: '+IntToStr(AGetData.bOcultarDadosDigitados));
    SL.Add('  bValidacaoDado: '+PWVALToString(AGetData.bValidacaoDado));
    SL.Add('  bAceitaNulo: '+IntToStr(AGetData.bAceitaNulo));
    SL.Add('  szValorInicial: '+AGetData.szValorInicial);
    SL.Add('  bTeclasDeAtalho: '+IntToStr(AGetData.bTeclasDeAtalho));
    SL.Add('  szMsgValidacao: '+TranslateUnprintable(AGetData.szMsgValidacao));
    SL.Add('  szMsgConfirmacao: '+TranslateUnprintable(AGetData.szMsgConfirmacao));
    SL.Add('  szMsgDadoMaior: '+TranslateUnprintable(AGetData.szMsgDadoMaior));
    SL.Add('  szMsgDadoMenor: '+TranslateUnprintable(AGetData.szMsgDadoMenor));
    SL.Add('  bCapturarDataVencCartao: '+IntToStr(AGetData.bCapturarDataVencCartao));
    SL.Add('  ulTipoEntradaCartao: '+IntToStr(AGetData.ulTipoEntradaCartao));
    SL.Add('  bItemInicial: '+IntToStr(AGetData.bItemInicial));
    SL.Add('  bNumeroCapturas: '+IntToStr(AGetData.bNumeroCapturas));
    SL.Add('  szMsgPrevia: '+TranslateUnprintable(AGetData.szMsgPrevia));
    SL.Add('  bTipoEntradaCodigoBarras: '+IntToStr(AGetData.bTipoEntradaCodigoBarras));
    SL.Add('  bOmiteMsgAlerta: '+IntToStr(AGetData.bOmiteMsgAlerta));
    SL.Add('  bIniciaPelaEsquerda: '+IntToStr(AGetData.bIniciaPelaEsquerda));
    SL.Add('  bNotificarCancelamento: '+IntToStr(AGetData.bNotificarCancelamento));
    SL.Add('  bAlinhaPelaDireita: '+IntToStr(AGetData.bAlinhaPelaDireita));
    GravarLog(SL.Text);
  finally
    SL.Free;
  end;
end;

function TACBrTEFPGWebAPI.ValidarDDMM(const AString: String): Boolean;
begin
  Result := False;
  if Length(AString) <> 4 then
    Exit;

  Result := ValidarDDMMAA(AString + '00');
end;

function TACBrTEFPGWebAPI.ValidarDDMMAA(const AString: String): Boolean;
var
  AnoStr: String;
begin
  Result := False;
  if (Length(AString) <> 6) then
    Exit;
  if not StrIsNumber(AString) then
    Exit;

  AnoStr := IntToStr(YearOf(Today));
  try
    EncodeDate( StrToInt( Copy(AnoStr , 1, 2) + Copy(AString, 5, 2) ),
                StrToInt( Copy(AString, 3, 2) ),
                StrToInt( Copy(AString, 1, 2) ) );
    Result := True;
  except
  end;
end;

function TACBrTEFPGWebAPI.ValidarModulo10(const AString: String): Boolean;
var
  AModulo: TACBrCalcDigito;
begin
  Result := False;
  if not StrIsNumber(AString) then
    Exit;

  AModulo := TACBrCalcDigito.Create;
  try
    AModulo.CalculoPadrao;
    AModulo.FormulaDigito := frModulo10;
    AModulo.Documento := copy(AString, 1, Length(AString)-1);
    AModulo.Calcular;
    Result := (AModulo.DigitoFinal = StrToInt(RightStr(AString,1)));
  finally
    AModulo.Free;
  end;
end;

procedure TACBrTEFPGWebAPI.AdicionarDadosObrigatorios;
begin
  GravarLog('TACBrTEFPGWebAPI.AdicionarDadosObrigatorios');
  AdicionarParametro(PWINFO_AUTNAME, NomeAplicacao);
  AdicionarParametro(PWINFO_AUTVER, VersaoAplicacao);
  AdicionarParametro(PWINFO_AUTDEV, SoftwareHouse);
  AdicionarParametro(PWINFO_AUTCAP, IntToStr(CalcularCapacidadesDaAutomacao));
  AdicionarParametro(PWINFO_MERCHADDDATA4, CACBrTEFPGWebAPIName+' '+CACBrTEFPGWebAPIVersao);
  if (fExibicaoQRCode > qreAuto) then
    AdicionarParametro(PWINFO_DSPQRPREF, IfThen(fExibicaoQRCode=qreExibirNoCheckOut, '2', '1') );
end;

function TACBrTEFPGWebAPI.CalcularCapacidadesDaAutomacao: Integer;
begin
  Result := 4;            // 4: valor fixo, sempre incluir;
  if fSuportaSaque then
    Inc(Result, 1);       // 1: funcionalidade de troco/saque;
  if fSuportaDesconto then
    Inc(Result, 2);       // 2: funcionalidade de desconto;
  if fSuportaViasDiferenciadas then
    Inc(Result, 8);       // 8: impressão das vias diferenciadas do comprovante para Cliente/Estabelecimento;
  if fImprimeViaClienteReduzida then
    Inc(Result, 16);      // 16: impressão do cupom reduzido
  if fUtilizaSaldoTotalVoucher then
    Inc(Result, 32);      // 32: utilização de saldo total do voucher para abatimento do valor da compra
  if fRemocaoCartaoPinPad then
    Inc(Result, 64);      // 64: Remoção do cartão do PIN-pad
  if fExibeMensagemCheckout then
    Inc(Result, 128);     // 128: exibição de mensagem no checkout.
  if (fExibicaoQRCode > qreNaoSuportado) then
    Inc(Result, 256);     // 256: exibição de QR Code no checkout
end;

procedure TACBrTEFPGWebAPI.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFPGWebAPI.SetPathLib(const AValue: String);
begin
  if fPathLib = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAPI.SetPathLib( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fPathLib := AValue;
end;

procedure TACBrTEFPGWebAPI.SetPontoCaptura(const AValue: String);
begin
  if fPontoCaptura = AValue then Exit;
  fPontoCaptura := LeftStr(OnlyNumber(AValue),11);
end;

procedure TACBrTEFPGWebAPI.SetPortaTCP(const AValue: String);
begin
  if fPortaTCP = AValue then Exit;
  fPortaTCP := Trim(AValue);
end;

procedure TACBrTEFPGWebAPI.SetSoftwareHouse(const AValue: String);
begin
  if fSoftwareHouse = AValue then Exit;
  fSoftwareHouse := LeftStr(Trim(AValue),50);
end;

procedure TACBrTEFPGWebAPI.SetNomeAplicacao(const AValue: String);
begin
  if fNomeAplicacao = AValue then Exit;
  fNomeAplicacao := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFPGWebAPI.SetNomeEstabelecimento(const AValue: String);
begin
  if fNomeEstabelecimento = AValue then Exit;
  fNomeEstabelecimento := LeftStr(Trim(AValue),100);
end;

procedure TACBrTEFPGWebAPI.SetVersaoAplicacao(const AValue: String);
begin
  if fVersaoAplicacao = AValue then Exit;
  fVersaoAplicacao := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFPGWebAPI.SetEmTransacao(AValue: Boolean);
begin
  if (not fInicializada) or (AValue = fEmTransacao) then
    Exit;

  fEmTransacao := AValue;

  if fEmTransacao then
  begin
    fDadosTransacao.Clear;
    fUsouPinPad := False;
  end
  else
  begin
    AjustarTempoOcioso;

    // Limpando mensagens do Operador e Cliente
    ExibirMensagem('', tmTodas);
  end;
end;

procedure TACBrTEFPGWebAPI.OnTimerOcioso(Sender: TObject);
var
  iRet: SmallInt;
begin
  fTimerOcioso.Enabled := False;
  if not Inicializada then
    Exit;

  GravarLog('PW_iIdleProc');
  iRet := xPW_iIdleProc();
  GravarLog('  '+PWRETToString(iRet));
end;

function TACBrTEFPGWebAPI.LibFullName: String;
var
  APath: String;
begin
  APath := PathWithDelim(PathLib);
  Result := APath + CACBrTEFPGWebLib;
end;

procedure TACBrTEFPGWebAPI.SetDiretorioTrabalho(const AValue: String);
begin
  if fDiretorioTrabalho = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAPI.SetDiretorioTrabalho( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fDiretorioTrabalho := AValue;
end;

procedure TACBrTEFPGWebAPI.SetEnderecoIP(const AValue: String);
begin
  if fEnderecoIP = AValue then Exit;
  fEnderecoIP := Trim(AValue);
end;

procedure TACBrTEFPGWebAPI.SetCNPJEstabelecimento(const AValue: String);
var
  ACNPJ, ErroMsg: String;
begin
  if fCNPJEstabelecimento = AValue then
    Exit;

  ACNPJ := OnlyNumber(AValue);
  if (ACNPJ <> '') then
  begin
    ErroMsg := ACBrValidador.ValidarCNPJ(ACNPJ);
    if (ErroMsg <> '') then
      DoException('SetCNPJEstabelecimento: '+ErroMsg);
  end;

  fCNPJEstabelecimento := ACNPJ;
end;

procedure TACBrTEFPGWebAPI.LoadLibFunctions;

  procedure PGWebFunctionDetect( FuncName: AnsiString; var LibPointer: Pointer;
    FuncIsRequired: Boolean = True) ;
  var
    sLibName: string;
  begin
    if not Assigned( LibPointer )  then
    begin
      GravarLog('   '+FuncName);

      sLibName := LibFullName;
      if not FunctionDetect(sLibName, FuncName, LibPointer) then
      begin
        LibPointer := NIL ;
        if FuncIsRequired then
          DoException(Format(ACBrStr('Erro ao carregar a função: %s de: %s'),[FuncName, sLibName]))
        else
          GravarLog(Format(ACBrStr('     Função não requerida: %s não encontrada em: %s'),[FuncName, sLibName]));
        end ;
    end ;
  end;

 begin
   if fCarregada then
     Exit;

   GravarLog('TACBrTEFPGWebAPI.LoadDLLFunctions');

   PGWebFunctionDetect('PW_iInit', @xPW_iInit);
   PGWebFunctionDetect('PW_iGetResult', @xPW_iGetResult);
   PGWebFunctionDetect('PW_iNewTransac', @xPW_iNewTransac);
   PGWebFunctionDetect('PW_iAddParam', @xPW_iAddParam);
   PGWebFunctionDetect('PW_iExecTransac', @xPW_iExecTransac);
   PGWebFunctionDetect('PW_iConfirmation', @xPW_iConfirmation);
   PGWebFunctionDetect('PW_iIdleProc', @xPW_iIdleProc);
   PGWebFunctionDetect('PW_iGetOperations', @xPW_iGetOperations);
   PGWebFunctionDetect('PW_iPPEventLoop', @xPW_iPPEventLoop);
   PGWebFunctionDetect('PW_iPPAbort', @xPW_iPPAbort);
   PGWebFunctionDetect('PW_iPPGetCard', @xPW_iPPGetCard);
   PGWebFunctionDetect('PW_iPPGetPIN', @xPW_iPPGetPIN);
   PGWebFunctionDetect('PW_iPPGetData', @xPW_iPPGetData);
   PGWebFunctionDetect('PW_iPPGoOnChip', @xPW_iPPGoOnChip);
   PGWebFunctionDetect('PW_iPPFinishChip', @xPW_iPPFinishChip);
   PGWebFunctionDetect('PW_iPPConfirmData', @xPW_iPPConfirmData);
   PGWebFunctionDetect('PW_iPPGenericCMD', @xPW_iPPGenericCMD);
   PGWebFunctionDetect('PW_iPPDisplay', @xPW_iPPDisplay);
   PGWebFunctionDetect('PW_iPPGetUserData', @xPW_iPPGetUserData);
   PGWebFunctionDetect('PW_iPPWaitEvent', @xPW_iPPWaitEvent);
   PGWebFunctionDetect('PW_iPPRemoveCard', @xPW_iPPRemoveCard);
   PGWebFunctionDetect('PW_iPPGetPINBlock', @xPW_iPPGetPINBlock, False);
   PGWebFunctionDetect('PW_iPPCommTest', @xPW_iPPCommTest, False);
   PGWebFunctionDetect('PW_iTransactionInquiry', @xPW_iTransactionInquiry);
   PGWebFunctionDetect('PW_iPPDataConfirmation', @xPW_iPPDataConfirmation, False);
   PGWebFunctionDetect('PW_iPPTestKey', @xPW_iPPTestKey, False);
   PGWebFunctionDetect('PW_iWaitConfirmation', @xPW_iWaitConfirmation, False);
   PGWebFunctionDetect('PW_iGetOperationsEx', @xPW_iGetOperationsEx, False);

   fCarregada := True;
end;

procedure TACBrTEFPGWebAPI.UnLoadLibFunctions;
var
  sLibName: String;
begin
  if not fCarregada then
    Exit;

  //GravarLog('TACBrTEFPGWebAPI.UnLoadDLLFunctions');

  sLibName := LibFullName;
  UnLoadLibrary( sLibName );
  fCarregada := False;
  ClearMethodPointers;
end;

end.

