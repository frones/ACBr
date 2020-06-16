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
  ACBrTEFComum, ACBrBase;

resourcestring
  sPerVenctoCartao = 'VENCIMENTO CARTAO';
  sErrLibJaInicializda = 'PW_iInit já executado';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';
  sErrPWRET_WRITERR = 'Falha de gravação no diretório %s';
  sErrPWRET_INVCALL = 'Já foi efetuada uma chamada à função PW_iInit';
  sErrPWRET_INVCALL2 = 'Não há captura de dados no PIN-pad em curso.';
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
  CACBrTEFPGWebAPIVersao = '1.0.0';

  {$IFDEF LINUX}
   CACBrTEFPGWebLib = 'PGWebLib.so';
  {$ELSE}
   CACBrTEFPGWebLib = 'PGWebLib.dll';
  {$ENDIF}

  CSleepNothing = 300;
  CMilissegundosMensagem = 5000;  // 5 seg
  CMilissegundosOcioso = 300000;  // 5 min

//==========================================================================================
// Número maximo de itens em um menu de seleção
//==========================================================================================
  PWMENU_MAXINTENS = 40;

//==========================================================
// Erros específicos da biblioteca compartilhada de PIN-pad
//==========================================================
  PWRET_PPS_MAX  = -2100;
  PWRET_PPS_MIN  = PWRET_PPS_MAX - 100;


//==========================================================================================
//    Códigos de Confirmação de Transação
//==========================================================================================
  PWCNF_CNF_AUTO     = 289;     // A transação foi confirmada pelo Ponto de Captura, sem intervenção do usuário.
  PWCNF_CNF_MANU_AUT = 12833;   // A transação foi confirmada manualmente na Automação.*/
  PWCNF_REV_MANU_AUT = 12849;   // A transação foi desfeita manualmente na Automação.*/
  PWCNF_REV_PRN_AUT  = 78129;   // A transação foi desfeita pela Automação, devido a uma falha na impressão do comprovante (não fiscal). A priori, não usar. Falhas na impressão não devem gerar desfazimento, deve ser solicitada a reimpressão da transação.*/
  PWCNF_REV_DISP_AUT = 143665;  // A transação foi desfeita pela Automação, devido a uma falha no mecanismo de liberação da mercadoria.*/
  PWCNF_REV_COMM_AUT = 209201;  // A transação foi desfeita pela Automação, devido a uma falha de comunicação/integração com o ponto de captura (Cliente Muxx).*/
  PWCNF_REV_ABORT    = 274737;  // A transação não foi finalizada, foi interrompida durante a captura de dados.*/
  PWCNF_REV_OTHER_AUT= 471345;  // A transação foi desfeita a pedido da Automação, por um outro motivo não previsto.*/
  PWCNF_REV_PWR_AUT  = 536881;  // A transação foi desfeita automaticamente pela Automação, devido a uma queda de energia (reinício abrupto do sistema).*/
  PWCNF_REV_FISC_AUT = 602417;  // A transação foi desfeita automaticamente pela Automação, devido a uma falha de registro no sistema fiscal (impressora S@T, on-line, etc.).*/

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

//==========================================================================================
//  Tabela de Códigos de retorno das transações
//==========================================================================================
  PWOPER_NULL        = 0;   // Testa comunicação com a infraestrutura do Pay&Go Web
  PWOPER_INSTALL     = 1;   // Registra o Ponto de Captura perante a infraestrutura do Pay&Go Web, para que seja autorizado a realizar transações
  PWOPER_PARAMUPD    = 2;   // Obtém da infraestrutura do Pay&Go Web os parâmetros de operação atualizados do Ponto de Captura.
  PWOPER_REPRINT     = 16;  // Obtém o último comprovante gerado por uma transação
  PWOPER_RPTTRUNC    = 17;  // Obtém um relatório sintético das transações realizadas desde a última obtenção deste relatório
  PWOPER_RPTDETAIL   = 18;  // Relatório detalhado das transações realizadas na data informada, ou data atual.
  PWOPER_ADMIN       = 32;  // Acessa qualquer transação que não seja disponibilizada pelo comando PWOPER_SALE. Um menu é apresentado para o operador selecionar a transação desejada.
  PWOPER_SALE        = 33;  // (Venda) Realiza o pagamento de mercadorias e/ou serviços vendidos pelo Estabelecimento ao Cliente (tipicamente, com cartão de crédito/débito), transferindo fundos entre as respectivas contas.
  PWOPER_SALEVOID    = 34;  // (Cancelamento de venda) Cancela uma transação PWOPER_SALE, realizando a transferência de fundos inversa
  PWOPER_PREPAID     = 35;  // Realiza a aquisição de créditos pré-pagos (por exemplo, recarga de celular).
  PWOPER_CHECKINQ    = 36;  // Consulta a validade de um cheque papel
  PWOPER_RETBALINQ   = 37;  // Consulta o saldo/limite do Estabelecimento (tipicamente, limite de crédito para venda de créditos pré-pagos).
  PWOPER_CRDBALINQ   = 38;  // Consulta o saldo do cartão do Cliente
  PWOPER_INITIALIZ   = 39;  // (Inicialização/abertura) Inicializa a operação junto ao Provedor e/ou obtém/atualiza os parâmetros de operação mantidos por este
  PWOPER_SETTLEMNT   = 40;  // (Fechamento/finalização) Finaliza a operação junto ao Provedor
  PWOPER_PREAUTH     = 41;  // (Pré-autorização) Reserva o valor correspondente a uma venda no limite do cartão de crédito de um Cliente, porém sem efetivar a transferência de fundos.
  PWOPER_PREAUTVOID  = 42;  // (Cancelamento de pré-autorização) Cancela uma transação PWOPER_PREAUTH, liberando o valor reservado no limite do cartão de crédito
  PWOPER_CASHWDRWL   = 43;  // (Saque) Registra a retirada de um valor em espécie pelo Cliente no Estabelecimento, para transferência de fundos nas respectivas contas
  PWOPER_LOCALMAINT  = 44;  // (Baixa técnica) Registra uma intervenção técnica no estabelecimento perante o Provedor.
  PWOPER_FINANCINQ   = 45;  // Consulta as taxas de financiamento referentes a uma possível venda parcelada, sem efetivar a transferência de fundos ou impactar o limite de crédito do Cliente
  PWOPER_ADDRVERIF   = 46;  // Verifica junto ao Provedor o endereço do Cliente
  PWOPER_SALEPRE     = 47;  // Efetiva uma pré-autorização (PWOPER_PREAUTH), previamente realizada, realizando a transferência de fundos entre as contas do Estabelecimento e do Cliente
  PWOPER_LOYCREDIT   = 48;  // Registra o acúmulo de pontos pelo Cliente, a partir de um programa de fidelidade.
  PWOPER_LOYCREDVOID = 49;  // Cancela uma transação PWOPER_LOYCREDIT
  PWOPER_LOYDEBIT    = 50;  // Registra o resgate de pontos/prêmio pelo Cliente, a partir de um programa de fidelidade.
  PWOPER_LOYDEBVOID  = 51;  // Cancela uma transação PWOPER_LOYDEBIT
  PWOPER_VOID        = 57;  // Exibe um menu com os cancelamentos disponíveis, caso só exista um tipo, este é selecionado automaticamente
  PWOPER_VERSION     = 252; // (Versão) Permite consultar a versão da biblioteca atualmente em uso.
  PWOPER_CONFIG      = 253; // (Configuração) Visualiza e altera os parâmetros de operação locais da biblioteca
  PWOPER_MAINTENANCE = 254; // (Manutenção) Apaga todas as configurações do Ponto de Captura, devendo ser novamente realizada uma transação de Instalação.

//==========================================================================================
//   Tipos de dados que podem ser informados pela Automação
//==========================================================================================
  PWINFO_RET            = 0;      // Código do último Retorno (PWRET)  (uso interno do ACBr)
  PWINFO_OPERATION      = 2;      // Tipo de transação (PWOPER_xxx). Consultar os valores possíveis na descrição da função PW_iNewTransac
  PWINFO_POSID          = 17;     // Identificador do Ponto de Captura.
  PWINFO_AUTNAME        = 21;     // Nome do aplicativo de Automação
  PWINFO_AUTVER         = 22;     // Versão do aplicativo de Automação
  PWINFO_AUTDEV         = 23;     // Empresa desenvolvedora do aplicativo de Automação.
  PWINFO_DESTTCPIP      = 27;     // Endereço TCP/IP para comunicação com a infraestrutura Pay&Go Web, no formato <endereço IP>:<porta TCP> ou <nome do servidor>:<porta TCP>
  PWINFO_MERCHCNPJCPF   = 28;     // CNPJ (ou CPF) do Estabelecimento, sem formatação. No caso de estarem sendo utilizadas afiliações de mais de um estabelecimento, este dado pode ser adicionado pela automação para selecionar previamente o estabelecimento a ser utilizado para determinada transação. Caso este dado não seja informado, será solicitada a exibição de um menu para a escolha dentre os vários estabelecimentos disponíveis.
  PWINFO_AUTCAP         = 36;     // Capacidades da Automação (soma dos valores abaixo): 1: funcionalidade de troco/saque; 2: funcionalidade de desconto; 4: valor fixo, sempre incluir; 8: impressão das vias diferenciadas do comprovante para Cliente/Estabelecimento; 16: impressão do cupom reduzido. 32: utilização de saldo total do voucher para abatimento do valor da compra.
  PWINFO_TOTAMNT        = 37;     // Valor total da operação, considerando PWINFO_CURREXP (em centavos se igual a 2), incluindo desconto, saque, gorjeta, taxa de embarque, etc.
  PWINFO_CURRENCY       = 38;     // Moeda (padrão ISO4217, 986 para o Real)
  PWINFO_CURREXP        = 39;     // Expoente da moeda (2 para centavos)
  PWINFO_FISCALREF      = 40;     // Identificador do documento fiscal
  PWINFO_CARDTYPE       = 41;     // Tipo de cartão utilizado (PW_iGetResult), ou tipos de cartão aceitos (soma dos valores abaixo, PW_iAddParam): 1: crédito 2: débito 4: voucher/PAT 8: outros
  PWINFO_PRODUCTNAME    = 42;     // Nome/tipo do produto utilizado, na nomenclatura do Provedor.
  PWINFO_DATETIME       = 49;     // Data e hora local da transação, no formato “AAAAMMDDhhmmss”
  PWINFO_REQNUM         = 50;     // Referência local da transação
  PWINFO_AUTHSYST       = 53;     // Nome do Provedor: “ELAVON”; “FILLIP”; “LIBERCARD”; “RV”; etc
  PWINFO_VIRTMERCH      = 54;     // Identificador do Estabelecimento
  PWINFO_AUTMERCHID     = 56;     // Identificador do estabelecimento para o Provedor (código de afiliação).
  PWINFO_PHONEFULLNO    = 58;     // Número do telefone, com o DDD (10 ou 11 dígitos).
  PWINFO_FINTYPE        = 59;     // Modalidade de financiamento da transação: 1: à vista 2: parcelado pelo emissor 4: parcelado pelo estabelecimento 8: pré-datado
  PWINFO_INSTALLMENTS   = 60;     // Quantidade de parcelas
  PWINFO_INSTALLMDATE   = 61;     // Data de vencimento do pré-datado, ou da primeira parcela. Formato “DDMMAA
  PWINFO_PRODUCTID      = 62;     // Identificação do produto utilizado, de acordo com a nomenclatura do Provedor.
  PWINFO_RESULTMSG      = 66;     // Mensagem descrevendo o resultado final da transação, seja esta bem ou mal sucedida (conforme “4.3.Interface com o usuário”, página 8
  PWINFO_CNFREQ         = 67;     // Necessidade de confirmação: 0: não requer confirmação; 1: requer confirmação.
  PWINFO_AUTLOCREF      = 68;     // Referência da transação para a infraestrutura Pay&Go Web
  PWINFO_AUTEXTREF      = 69;     // Referência da transação para o Provedor (NSU host).
  PWINFO_AUTHCODE       = 70;     // Código de autorização
  PWINFO_AUTRESPCODE    = 71;     // Código de resposta da transação (campo ISO8583:39)
  PWINFO_AUTDATETIME    = 72;     // Data/hora da transação para o Provedor, formato “AAAAMMDDhhmmss”.
  PWINFO_DISCOUNTAMT    = 73;     // Valor do desconto concedido pelo Provedor, considerando PWINFO_CURREXP, já deduzido em PWINFO_TOTAMNT
  PWINFO_CASHBACKAMT    = 74;     // Valor do saque/troco, considerando PWINFO_CURREXP, já incluído em PWINFO_TOTAMNT
  PWINFO_CARDNAME       = 75;     // Nome do cartão ou do emissor do cartão
  PWINFO_ONOFF          = 76;     // Modalidade da transação: 1: online 2: off-line
  PWINFO_BOARDINGTAX    = 77;     // Valor da taxa de embarque, considerando PWINFO_CURREXP, já incluído em PWINFO_TOTAMNT
  PWINFO_TIPAMOUNT      = 78;     // Valor da taxa de serviço (gorjeta), considerando PWINFO_CURREXP, já incluído em PWINFO_TOTAMNT
  PWINFO_INSTALLM1AMT   = 79;     // Valor da entrada para um pagamento parcelado, considerando PWINFO_CURREXP, já incluído em PWINFO_TOTAMNT
  PWINFO_INSTALLMAMNT   = 80;     // Valor da parcela, considerando PWINFO_CURREXP, já incluído em PWINFO_TOTAMNT
  PWINFO_RCPTFULL       = 82;     // Comprovante para impressão – Via completa. Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh
  PWINFO_RCPTMERCH      = 83;     // Comprovante para impressão – Via diferenciada para o Estabelecimento. Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
  PWINFO_RCPTCHOLDER    = 84;     // Comprovante para impressão – Via diferenciada para o Cliente. Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
  PWINFO_RCPTCHSHORT    = 85;     // Comprovante para impressão – Cupom reduzido (para o Cliente). Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh
  PWINFO_TRNORIGDATE    = 87;     // Data da transação original, no caso de um cancelamento ou uma confirmação de pré-autorização (formato “DDMMAA”).
  PWINFO_TRNORIGNSU     = 88;     // NSU da transação original, no caso de um cancelamento ou uma confirmação de pré-autorizaçã
  PWINFO_SALDOVOUCHER   = 89;     // Saldo do cartão voucher recebido do autorizador
  PWINFO_TRNORIGAMNT    = 96;     // Valor da transação original, no caso de um cancelamento ou uma confirmação de pré-autorização.
  PWINFO_TRNORIGAUTH    = 98;     // Código de autorização da transação original, no caso de um cancelamento ou uma confirmação de pré-autorização
  PWINFO_LANGUAGE       = 108;    // Idioma a ser utilizado para a interface com o cliente: 0: Português 1: Inglês 2: Espanhol
  PWINFO_PROCESSMSG     = 111;    // Mensagem a ser exibida para o cliente durante o processamento da transação
  PWINFO_TRNORIGREQNUM  = 114;    // Número da solicitação da transação original, no caso de um cancelamento ou uma confirmação de pré-autorização
  PWINFO_TRNORIGTIME    = 115;    // Hora da transação original, no caso de um cancelamento ou uma confirmação de pré-autorização (formato “HHMMSS”).
  PWINFO_CNCDSPMSG      = 116;    // Mensagem a ser exibida para o operador no terminal no caso da transação ser abortada (cancelamento ou timeout).
  PWINFO_CNCPPMSG       = 117;    // Mensagem a ser exibida para o portador no PIN-pad no caso da transação ser abortada (cancelamento ou timeout).
  PWINFO_CARDENTMODE    = 192;    // Modo(s) de entrada do cartão: 1: digitado 2: tarja magnética 4: chip com contato 16: fallback de chip para tarja 32: chip sem contato simulando tarja (cliente informa tipo efetivamente utilizado) 64: chip sem contato EMV (cliente informa tipo efetivamente utilizado) 256: fallback de tarja para digitado
  PWINFO_CARDFULLPAN    = 193;    // Número do cartão completo, para transação digitada. Este dado não pode ser recuperado pela função PW_iGetResult
  PWINFO_CARDEXPDATE    = 194;    // Data de vencimento do cartão (formato “MMAA”).
  PWINFO_CARDNAMESTD    = 196;    // Descrição do produto bandeira padrão relacionado ao BIN.
  PWINFO_CARDPARCPAN    = 200;    // Número do cartão, truncado ou mascarado
  PWINFO_CHOLDVERIF     = 207;    // Verificação do portador, soma dos seguintes valores: “1”: Assinatura do portador em papel. “2”: Senha verificada off-line. “4”: Senha off-line bloqueada no decorrer desta transação. “8”: Senha verificada online
  PWINFO_AID            = 216;    // Aplicação do cartão utilizada durante a transação
  PWINFO_BARCODENTMODE  = 233;    // Modo(s) de entrada do código de barras: 1:  digitado; 2:  lido através de dispositivo eletrônico.
  PWINFO_BARCODE        = 234;    // Código de barras completo, lido ou digitado
  PWINFO_MERCHADDDATA1  = 240;    // Dados adicionais relevantes para a Automação (#1)
  PWINFO_MERCHADDDATA2  = 241;    // Dados adicionais relevantes para a Automação (#2)
  PWINFO_MERCHADDDATA3  = 242;    // Dados adicionais relevantes para a Automação (#3)
  PWINFO_MERCHADDDATA4  = 243;    // Dados adicionais relevantes para a Automação (#4)
  PWINFO_RCPTPRN        = 244;    // Indica quais vias de comprovante devem ser impressas: 0: não há comprovante 1: imprimir somente a via do Cliente 2: imprimir somente a via do Estabelecimento 3: imprimir ambas as vias do Cliente e do Estabelecimento
  PWINFO_AUTHMNGTUSER   = 245;    // Identificador do usuário autenticado com a senha do lojista
  PWINFO_AUTHTECHUSER   = 246;    // Identificador do usuário autenticado com a senha técnica.
  PWINFO_PAYMNTTYPE     = 7969;   // Modalidade de pagamento: 1: cartão 2: dinheiro 3: cheque
  PWINFO_CHOLDERNAME    = 7992;   // Nome do portador do cartão utilizado, o tamanho segue o mesmo padrão da tag 5F20 EMV.
  PWINFO_MERCHNAMEPDC   = 7993;   // Nome do estabelecimento em que o ponto de captura está cadastrado. (até 100)
  PWINFO_DEFAULTCARDPARCPAN = 8002; // Número do cartão mascarado no formato BIN + *** + 4 últimos dígitos. Ex: 543211******987
  PWINFO_AUTHPOSQRCODE  = 8055;   // Conteúdo do QR Code identificando o checkout para o autorizador.
  PWINFO_USINGPINPAD    = 32513;  // Indica se o ponto de captura faz ou não o uso de PIN-pad: 0: Não utiliza PIN-pad; 1: Utiliza PIN-pad.
  PWINFO_PPCOMMPORT     = 32514;  // Número da porta serial à qual o PIN-pad está conectado. O valor 0 (zero) indica uma busca automática desta porta
  PWINFO_IDLEPROCTIME   = 32516;  // Próxima data e horário em que a função PW_iIdleProc deve ser chamada pela Automação. Formato “AAMMDDHHMMSS”
  PWINFO_PNDAUTHSYST    = 32517;  // Nome do provedor para o qual existe uma transação pendente.
  PWINFO_PNDVIRTMERCH   = 32518;  // Identificador do Estabelecimento para o qual existe uma transação pendente
  PWINFO_PNDREQNUM      = 32519;  // Referência local da transação que está pendente.
  PWINFO_PNDAUTLOCREF   = 32520;  // Referência para a infraestrutura Pay&Go Web da transação que está pendente.
  PWINFO_PNDAUTEXTREF   = 32521;  // Referência para o Provedor da transação que está pendente
  PWINFO_LOCALINFO1     = 32522;  // Texto exibido para um item de menu selecionado pelo usuário
  PWINFO_SERVERPND      = 32523;  // Indica se o ponto de captura possui alguma pendência a ser resolvida com o Pay&Go Web: 0: não possui pendência; 1: possui pendência
  PWINFO_PPINFO         = 32533;  // Informações do PIN-pad conectado, seguindo o padrão posição/informação abaixo: 001-020 / Nome do fabricante do PIN-pad. 021-039 / Modelo/versão do hardware. 040 / Se o PIN-pad suporta cartão com chip sem contato, este campo deve conter a letra “C”, caso contrário um espaço em branco. 041-060 / Versão do software básico/firmware. 061-064 / Versão da especificação, no formato “V.VV”. 065-080 / Versão da aplicação básica, no formato “VVV.VV AAMMDD” (com 3 espaços à direita). 081-100 / Número de série do PIN-pad (com espaços à direita)
  PWINFO_DUEAMNT        = 48902;  // Valor devido pelo usuário, considerando PWINFO_CURREXP, já deduzido em PWINFO_TOTAMNT
  PWINFO_READJUSTEDAMNT = 48905;  // Valor total da transação reajustado, este campo será utilizado caso o autorizador, por alguma regra de negócio específica dele, resolva alterar o valor total que foi solicitado para a transação

  MIN_PWINFO = PWINFO_OPERATION;
  MAX_PWINFO = PWINFO_READJUSTEDAMNT;

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
  PWRET_PPS_XXX            = -2200;  // Erros retornados pelo PIN-pad, conforme seção 10.2

//==========================================================================================
// Tipos utilizados na captura de dados dinamica
//==========================================================================================
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
  EACBrTEFPayGoWeb = class(EACBrTEFErro);

  TACBrTEFRespHack = class(TACBrTEFResp);    // Hack para acessar conteudo Protected

  { TACBrTEFRespPGWeb }

  TACBrTEFRespPGWeb = class(TACBrTEFResp)
  public
    procedure ConteudoToProperty; override;
  end;

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

  PW_Operations = Array[0..9] of TPW_Operations;

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

  TACBrTEFPGWebAPITerminalMensagem = (tmOperador, tmCliente);

  TACBrTEFPGWebAPIExibeMensagem = procedure(
    Mensagem: String;
    Terminal: TACBrTEFPGWebAPITerminalMensagem;
    MilissegundosExibicao: Integer  // 0 - Para com OK; Positivo - aguarda Ok ou N milissegundos; Negativo - Apenas exibe a Msg (não aguarda)
    ) of object;

  TACBrTEFPGWebAPIOperacaoPinPad = (ppGetCard, ppGetPIN, ppGetData, ppGoOnChip,
    ppFinishChip, ppConfirmData, ppGenericCMD, ppDataConfirmation, ppDisplay,
    ppGetUserData, ppWaitEvent, ppRemoveCard, ppGetPINBlock);

  TACBrTEFPGWebAPIAguardaPinPad = procedure(
    OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad; var Cancelar: Boolean) of object;

  { TACBrTEFPGWebAPIParametrosAdicionais }

  TACBrTEFPGWebAPIParametrosAdicionais = class(TStringList)
  private
    function GetValueInfo(AInfo: Word): string;
    procedure SetValueInfo(AInfo: Word; AValue: string);
  public
    property ValueInfo[AInfo: Word]: string read GetValueInfo write SetValueInfo;
  end;

  { TACBrTEFPGWebAPI }

  TACBrTEFPGWebAPI = class
  private
    fCNPJEstabelecimento: String;
    fDadosTransacao: TStringList;
    fDiretorioTrabalho: String;
    fEnderecoIP: String;
    fImprimirViaClienteReduzida: Boolean;
    fInicializada: Boolean;
    fEmTransacao: Boolean;
    fNomeAplicacao: String;
    fNomeEstabelecimento: String;
    fOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
    fOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
    fOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
    fOnGravarLog: TACBrGravarLog;
    fOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
    fParametrosAdicionais: TACBrTEFPGWebAPIParametrosAdicionais;
    fPathDLL: String;
    fPontoCaptura: String;
    fPortaPinPad: Integer;
    fPortaTCP: String;
    fSoftwareHouse: String;
    fSuportaDesconto: Boolean;
    fSuportaSaque: Boolean;
    fSuportaViasDiferenciadas: Boolean;
    fUtilizaSaldoTotalVoucher: Boolean;
    fVersaoAplicacao: String;
    fTimerOcioso: TACBrThreadTimer;
    fTempoOcioso: TDateTime;

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
    xPW_iIdleProc: function(): SmallInt;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPW_iGetOperations: function(bOperType: Byte; vstOperations: PW_Operations;
              piNumOperations: SmallInt): SmallInt;
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
    xPW_iTransactionInquiry: function(const pszXmlRequest: PAnsiChar;
             pszXmlResponse: PAnsiChar; ulXmlResponseLen: Word): SmallInt;
             {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetEnderecoIP(AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetNomeAplicacao(AValue: String);
    procedure SetNomeEstabelecimento(AValue: String);
    procedure SetPathDLL(AValue: String);
    procedure SetPontoCaptura(AValue: String);
    procedure SetPortaTCP(AValue: String);
    procedure SetSoftwareHouse(AValue: String);
    procedure SetVersaoAplicacao(AValue: String);

    procedure SetEmTransacao(AValue: Boolean);
    procedure OnTimerOcioso(Sender: TObject);
  protected
    procedure LoadDLLFunctions;
    procedure UnLoadDLLFunctions;
    procedure ClearMethodPointers;

    procedure DoException( AErrorMsg: String );
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
    procedure ExibirMensagem(const AMsg: String; Terminal: TACBrTEFPGWebAPITerminalMensagem = tmOperador; TempoEspera: Integer = -1);

    function PW_GetDataToDefinicaoCampo(AGetData: TPW_GetData): TACBrTEFPGWebAPIDefinicaoCampo;
    procedure LogPWGetData(AGetData: TPW_GetData);

    function ValidarDDMM(AString: String): Boolean;
    function ValidarDDMMAA(AString: String): Boolean;
    function ValidarModulo10(AString: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;

    function ObterInfo(iINFO: Word): String;
    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);

    procedure IniciarTransacao(iOPER: SmallInt; ParametrosAdicionaisTransacao: TStrings = nil);
    procedure AdicionarParametro(iINFO: Word; const AValor: AnsiString); overload;
    procedure AdicionarParametro(AKeyValueStr: String); overload;
    function ExecutarTransacao: Boolean;
    procedure ObterDadosDaTransacao;
    procedure FinalizarTrancao(Status: LongWord; pszReqNum: String = '';
      pszLocRef: String = ''; pszExtRef: String = ''; pszVirtMerch: String = '';
      pszAuthSyst: String = '');
    procedure AbortarTransacao;

    function ValidarRespostaCampo(AResposta: String;
      ADefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo): String;

    property PathDLL: String read fPathDLL write SetPathDLL;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
    property Inicializada: Boolean read fInicializada write SetInicializada;

    property EmTransacao: Boolean read fEmTransacao;
    property DadosDaTransacao: TStringList read fDadosTransacao;

    property SoftwareHouse: String read fSoftwareHouse write SetSoftwareHouse;
    property NomeAplicacao: String read fNomeAplicacao write SetNomeAplicacao ;
    property VersaoAplicacao: String read fVersaoAplicacao write SetVersaoAplicacao ;

    Property NomeEstabelecimento: String read fNomeEstabelecimento write SetNomeEstabelecimento;
    property CNPJEstabelecimento: String read fCNPJEstabelecimento write SetCNPJEstabelecimento;
    property PontoCaptura: String read fPontoCaptura write SetPontoCaptura;
    property EnderecoIP: String  read fEnderecoIP write SetEnderecoIP;
    property PortaTCP: String read fPortaTCP write SetPortaTCP;
    property PortaPinPad: Integer read fPortaPinPad write fPortaPinPad;
    property ParametrosAdicionais: TACBrTEFPGWebAPIParametrosAdicionais read fParametrosAdicionais;

    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto;
    property ImprimirViaClienteReduzida : Boolean read fImprimirViaClienteReduzida
      write fImprimirViaClienteReduzida;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas
      write fSuportaViasDiferenciadas;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher
      write fUtilizaSaldoTotalVoucher;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnExibeMenu: TACBrTEFPGWebAPIExibeMenu read fOnExibeMenu
      write fOnExibeMenu;
    property OnObtemCampo: TACBrTEFPGWebAPIObtemCampo read fOnObtemCampo
      write fOnObtemCampo;
    property OnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    property OnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad read fOnAguardaPinPad
      write fOnAguardaPinPad;
  end;

function ParseKeyValue(AKeyValueStr: String; out TheKey: String; out TheValue: String): Boolean;
function PWINFOToString(iINFO: Word): String;
function PWRETToString(iRET: SmallInt): String;
function PWOPERToString(iOPER: SmallInt): String;
function PWCNFToString(iCNF: LongWord): String;
function PWDATToString(bTipoDeDado: Byte): String;
function PWVALToString(bValidacaoDado: Byte): String;
function PWTYPToString(bTiposEntradaPermitidos: Byte): String;

implementation

uses
  StrUtils, dateutils, math, typinfo,
  ACBrConsts, ACBrUtil, ACBrValidador;

function ParseKeyValue(AKeyValueStr: String; out TheKey: String;
  out TheValue: String): Boolean;
var
  p: Integer;
begin
  Result := False;
  p := pos('=', AKeyValueStr);
  if (p > 0) then
  begin
    TheKey := copy(AKeyValueStr, 1, p-1);
    if (TheKey <> '') then
    begin
      TheValue := copy(AKeyValueStr, P+1, Length(AKeyValueStr));
      Result := True;
    end;
  end;
end;

function PWINFOToString(iINFO: Word): String;
begin
  case iINFO of
    PWINFO_OPERATION:       Result := 'PWINFO_OPERATION';
    PWINFO_POSID:           Result := 'PWINFO_POSID';
    PWINFO_AUTNAME:         Result := 'PWINFO_AUTNAME';
    PWINFO_AUTVER:          Result := 'PWINFO_AUTVER';
    PWINFO_AUTDEV:          Result := 'PWINFO_AUTDEV';
    PWINFO_DESTTCPIP:       Result := 'PWINFO_DESTTCPIP';
    PWINFO_MERCHCNPJCPF:    Result := 'PWINFO_MERCHCNPJCPF';
    PWINFO_AUTCAP:          Result := 'PWINFO_AUTCAP';
    PWINFO_TOTAMNT:         Result := 'PWINFO_TOTAMNT';
    PWINFO_CURRENCY:        Result := 'PWINFO_CURRENCY';
    PWINFO_CURREXP:         Result := 'PWINFO_CURREXP';
    PWINFO_FISCALREF:       Result := 'PWINFO_FISCALREF';
    PWINFO_CARDTYPE:        Result := 'PWINFO_CARDTYPE';
    PWINFO_PRODUCTNAME:     Result := 'PWINFO_PRODUCTNAME';
    PWINFO_DATETIME:        Result := 'PWINFO_DATETIME';
    PWINFO_REQNUM:          Result := 'PWINFO_REQNUM';
    PWINFO_AUTHSYST:        Result := 'PWINFO_AUTHSYST';
    PWINFO_VIRTMERCH:       Result := 'PWINFO_VIRTMERCH';
    PWINFO_AUTMERCHID:      Result := 'PWINFO_AUTMERCHID';
    PWINFO_PHONEFULLNO:     Result := 'PWINFO_PHONEFULLNO';
    PWINFO_FINTYPE:         Result := 'PWINFO_FINTYPE';
    PWINFO_INSTALLMENTS:    Result := 'PWINFO_INSTALLMENTS';
    PWINFO_INSTALLMDATE:    Result := 'PWINFO_INSTALLMDATE';
    PWINFO_PRODUCTID:       Result := 'PWINFO_PRODUCTID';
    PWINFO_RESULTMSG:       Result := 'PWINFO_RESULTMSG';
    PWINFO_CNFREQ:          Result := 'PWINFO_CNFREQ';
    PWINFO_AUTLOCREF:       Result := 'PWINFO_AUTLOCREF';
    PWINFO_AUTEXTREF:       Result := 'PWINFO_AUTEXTREF';
    PWINFO_AUTHCODE:        Result := 'PWINFO_AUTHCODE';
    PWINFO_AUTRESPCODE:     Result := 'PWINFO_AUTRESPCODE';
    PWINFO_AUTDATETIME:     Result := 'PWINFO_AUTDATETIME';
    PWINFO_DISCOUNTAMT:     Result := 'PWINFO_DISCOUNTAMT';
    PWINFO_CASHBACKAMT:     Result := 'PWINFO_CASHBACKAMT';
    PWINFO_CARDNAME:        Result := 'PWINFO_CARDNAME';
    PWINFO_ONOFF:           Result := 'PWINFO_ONOFF';
    PWINFO_BOARDINGTAX:     Result := 'PWINFO_BOARDINGTAX';
    PWINFO_TIPAMOUNT:       Result := 'PWINFO_TIPAMOUNT';
    PWINFO_INSTALLM1AMT:    Result := 'PWINFO_INSTALLM1AMT';
    PWINFO_INSTALLMAMNT:    Result := 'PWINFO_INSTALLMAMNT';
    PWINFO_RCPTFULL:        Result := 'PWINFO_RCPTFULL';
    PWINFO_RCPTMERCH:       Result := 'PWINFO_RCPTMERCH';
    PWINFO_RCPTCHOLDER:     Result := 'PWINFO_RCPTCHOLDER';
    PWINFO_RCPTCHSHORT:     Result := 'PWINFO_RCPTCHSHORT';
    PWINFO_TRNORIGDATE:     Result := 'PWINFO_TRNORIGDATE';
    PWINFO_TRNORIGNSU:      Result := 'PWINFO_TRNORIGNSU';
    PWINFO_SALDOVOUCHER:    Result := 'PWINFO_SALDOVOUCHER';
    PWINFO_TRNORIGAMNT:     Result := 'PWINFO_TRNORIGAMNT';
    PWINFO_TRNORIGAUTH:     Result := 'PWINFO_TRNORIGAUTH';
    PWINFO_LANGUAGE:        Result := 'PWINFO_LANGUAGE';
    PWINFO_PROCESSMSG:      Result := 'PWINFO_PROCESSMSG';
    PWINFO_TRNORIGREQNUM:   Result := 'PWINFO_TRNORIGREQNUM';
    PWINFO_TRNORIGTIME:     Result := 'PWINFO_TRNORIGTIME';
    PWINFO_CNCDSPMSG:       Result := 'PWINFO_CNCDSPMSG';
    PWINFO_CNCPPMSG:        Result := 'PWINFO_CNCPPMSG';
    PWINFO_CARDENTMODE:     Result := 'PWINFO_CARDENTMODE';
    PWINFO_CARDFULLPAN:     Result := 'PWINFO_CARDFULLPAN';
    PWINFO_CARDEXPDATE:     Result := 'PWINFO_CARDEXPDATE';
    PWINFO_CARDNAMESTD:     Result := 'PWINFO_CARDNAMESTD';
    PWINFO_CARDPARCPAN:     Result := 'PWINFO_CARDPARCPAN';
    PWINFO_CHOLDVERIF:      Result := 'PWINFO_CHOLDVERIF';
    PWINFO_AID:             Result := 'PWINFO_AID';
    PWINFO_BARCODENTMODE:   Result := 'PWINFO_BARCODENTMODE';
    PWINFO_BARCODE:         Result := 'PWINFO_BARCODE';
    PWINFO_MERCHADDDATA1:   Result := 'PWINFO_MERCHADDDATA1';
    PWINFO_MERCHADDDATA2:   Result := 'PWINFO_MERCHADDDATA2';
    PWINFO_MERCHADDDATA3:   Result := 'PWINFO_MERCHADDDATA3';
    PWINFO_MERCHADDDATA4:   Result := 'PWINFO_MERCHADDDATA4';
    PWINFO_RCPTPRN:         Result := 'PWINFO_RCPTPRN';
    PWINFO_AUTHMNGTUSER:    Result := 'PWINFO_AUTHMNGTUSER';
    PWINFO_AUTHTECHUSER:    Result := 'PWINFO_AUTHTECHUSER';
    PWINFO_PAYMNTTYPE:      Result := 'PWINFO_PAYMNTTYPE';
    PWINFO_CHOLDERNAME:     Result := 'PWINFO_CHOLDERNAME';
    PWINFO_MERCHNAMEPDC:    Result := 'PWINFO_MERCHNAMEPDC';
    PWINFO_DEFAULTCARDPARCPAN: Result := 'PWINFO_DEFAULTCARDPARCPAN';
    PWINFO_AUTHPOSQRCODE:   Result := 'PWINFO_AUTHPOSQRCODE';
    PWINFO_USINGPINPAD:     Result := 'PWINFO_USINGPINPAD';
    PWINFO_PPCOMMPORT:      Result := 'PWINFO_PPCOMMPORT';
    PWINFO_IDLEPROCTIME:    Result := 'PWINFO_IDLEPROCTIME';
    PWINFO_PNDAUTHSYST:     Result := 'PWINFO_PNDAUTHSYST';
    PWINFO_PNDVIRTMERCH:    Result := 'PWINFO_PNDVIRTMERCH';
    PWINFO_PNDREQNUM:       Result := 'PWINFO_PNDREQNUM';
    PWINFO_PNDAUTLOCREF:    Result := 'PWINFO_PNDAUTLOCREF';
    PWINFO_PNDAUTEXTREF:    Result := 'PWINFO_PNDAUTEXTREF';
    PWINFO_LOCALINFO1:      Result := 'PWINFO_LOCALINFO1';
    PWINFO_SERVERPND:       Result := 'PWINFO_SERVERPND';
    PWINFO_PPINFO:          Result := 'PWINFO_PPINFO';
    PWINFO_DUEAMNT:         Result := 'PWINFO_DUEAMNT';
    PWINFO_READJUSTEDAMNT:  Result := 'PWINFO_READJUSTEDAMNT';
  else
    Result := 'PWINFO_'+IntToStr(iINFO);
  end;
end;

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
    PWRET_PPS_XXX:              Result := 'PWRET_PPS_XXX';
  else
    Result := 'PWRET_'+IntToStr(iRET);
  end;
end;

function PWOPERToString(iOPER: SmallInt): String;
begin
  case iOPER of
    PWOPER_NULL:         Result := 'PWOPER_NULL';
    PWOPER_INSTALL:      Result := 'PWOPER_INSTALL';
    PWOPER_PARAMUPD:     Result := 'PWOPER_PARAMUPD';
    PWOPER_REPRINT:      Result := 'PWOPER_REPRINT';
    PWOPER_RPTTRUNC:     Result := 'PWOPER_RPTTRUNC';
    PWOPER_RPTDETAIL:    Result := 'PWOPER_RPTDETAIL';
    PWOPER_ADMIN:        Result := 'PWOPER_ADMIN';
    PWOPER_SALE:         Result := 'PWOPER_SALE';
    PWOPER_SALEVOID:     Result := 'PWOPER_SALEVOID';
    PWOPER_PREPAID:      Result := 'PWOPER_PREPAID';
    PWOPER_CHECKINQ:     Result := 'PWOPER_CHECKINQ';
    PWOPER_RETBALINQ:    Result := 'PWOPER_RETBALINQ';
    PWOPER_CRDBALINQ:    Result := 'PWOPER_CRDBALINQ';
    PWOPER_INITIALIZ:    Result := 'PWOPER_INITIALIZ';
    PWOPER_SETTLEMNT:    Result := 'PWOPER_SETTLEMNT';
    PWOPER_PREAUTH:      Result := 'PWOPER_PREAUTH';
    PWOPER_PREAUTVOID:   Result := 'PWOPER_PREAUTVOID';
    PWOPER_CASHWDRWL:    Result := 'PWOPER_CASHWDRWL';
    PWOPER_LOCALMAINT:   Result := 'PWOPER_LOCALMAINT';
    PWOPER_FINANCINQ:    Result := 'PWOPER_FINANCINQ';
    PWOPER_ADDRVERIF:    Result := 'PWOPER_ADDRVERIF';
    PWOPER_SALEPRE:      Result := 'PWOPER_SALEPRE';
    PWOPER_LOYCREDIT:    Result := 'PWOPER_LOYCREDIT';
    PWOPER_LOYCREDVOID:  Result := 'PWOPER_LOYCREDVOID';
    PWOPER_LOYDEBIT:     Result := 'PWOPER_LOYDEBIT';
    PWOPER_LOYDEBVOID:   Result := 'PWOPER_LOYDEBVOID';
    PWOPER_VOID:         Result := 'PWOPER_VOID';
    PWOPER_VERSION:      Result := 'PWOPER_VERSION';
    PWOPER_CONFIG:       Result := 'PWOPER_CONFIG';
    PWOPER_MAINTENANCE:  Result := 'PWOPER_MAINTENANCE';
  else
    Result := 'PWOPER_'+IntToStr(iOPER);
  end;
end;

function PWCNFToString(iCNF: LongWord): String;
begin
  case iCNF of
    PWCNF_CNF_AUTO:      Result := 'PWCNF_CNF_AUTO';
    PWCNF_CNF_MANU_AUT:  Result := 'PWCNF_CNF_MANU_AUT';
    PWCNF_REV_MANU_AUT:  Result := 'PWCNF_REV_MANU_AUT';
    PWCNF_REV_PRN_AUT:   Result := 'PWCNF_REV_PRN_AUT';
    PWCNF_REV_DISP_AUT:  Result := 'PWCNF_REV_DISP_AUT';
    PWCNF_REV_COMM_AUT:  Result := 'PWCNF_REV_COMM_AUT';
    PWCNF_REV_ABORT:     Result := 'PWCNF_REV_ABORT';
    PWCNF_REV_OTHER_AUT: Result := 'PWCNF_REV_OTHER_AUT';
    PWCNF_REV_PWR_AUT:   Result := 'PWCNF_REV_PWR_AUT';
    PWCNF_REV_FISC_AUT:  Result := 'PWCNF_REV_FISC_AUT';
  else
    Result := 'PWCNF_'+IntToStr(iCNF);
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

{ TACBrTEFPGWebAPIParametrosAdicionais }

function TACBrTEFPGWebAPIParametrosAdicionais.GetValueInfo(AInfo: Word): string;
begin
   Result := Values[IntToStr(AInfo)];
end;

procedure TACBrTEFPGWebAPIParametrosAdicionais.SetValueInfo(AInfo: Word; AValue: string);
begin
  Values[IntToStr(AInfo)] := AValue;
end;

{ TACBrTEFRespPGWeb }

procedure TACBrTEFRespPGWeb.ConteudoToProperty;
begin
  inherited ConteudoToProperty;
end;

{ TACBrTEFPGWebAPI }

constructor TACBrTEFPGWebAPI.Create;
begin
  inherited Create;

  fSuportaSaque := False;
  fSuportaDesconto := False;
  fSuportaViasDiferenciadas := True;
  fImprimirViaClienteReduzida := False;
  fUtilizaSaldoTotalVoucher := False;
  fInicializada := False;
  fDiretorioTrabalho := '';
  ClearMethodPointers;
  fEmTransacao := False;
  fDadosTransacao := TStringList.Create;
  fTempoTarefasAutomaticas := '';

  fSoftwareHouse := '';
  fNomeAplicacao := '';
  fVersaoAplicacao := '';
  fNomeEstabelecimento := '';
  fCNPJEstabelecimento := '';
  fEnderecoIP := '';
  fPortaTCP := '';
  fPortaPinPad := 0;
  fParametrosAdicionais := TACBrTEFPGWebAPIParametrosAdicionais.Create;

  fOnGravarLog := Nil;
  fOnExibeMenu := Nil;
  fOnObtemCampo := Nil;
  fOnExibeMensagem := Nil;
  fOnAguardaPinPad := Nil;

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
  UnLoadDLLFunctions;
  inherited Destroy;
end;

procedure TACBrTEFPGWebAPI.Inicializar;
var
  iRet: SmallInt;
  MsgErro: String;
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

  if (fDiretorioTrabalho = '') then
    fDiretorioTrabalho := ApplicationPath + 'TEF' + PathDelim + 'PGWeb';

  LoadDLLFunctions;
  if not DirectoryExists(fDiretorioTrabalho) then
    ForceDirectories(fDiretorioTrabalho);

  GravarLog('PW_iInit( '+fDiretorioTrabalho+' )');
  iRet := xPW_iInit(PAnsiChar(fDiretorioTrabalho));
  GravarLog('  '+PWRETToString(iRet));
  case iRet of
    PWRET_OK: MsgErro := '';
    PWRET_WRITERR: MsgErro := Format(sErrPWRET_WRITERR, [fDiretorioTrabalho]);
    PWRET_INVCALL: MsgErro := '';  //sErrPWRET_INVCALL;
  else
    MsgErro := ObterUltimoRetorno;
  end;

  if (MsgErro <> '') then
    DoException(ACBrStr(MsgErro));

  fInicializada := True;
  SetEmTransacao(False);
end;

procedure TACBrTEFPGWebAPI.DesInicializar;
begin
  GravarLog('TACBrTEFPGWebAPI.DesInicializar');
  UnLoadDLLFunctions;
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
  xPW_iIdleProc := Nil;
  xPW_iGetOperations := Nil;
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
  xPW_iPPDisplay := Nil;
  xPW_iPPGetUserData := Nil;
  xPW_iPPWaitEvent := Nil;
  xPW_iPPRemoveCard := Nil;
  xPW_iPPGetPINBlock := Nil;
  xPW_iTransactionInquiry := Nil;
end;

procedure TACBrTEFPGWebAPI.DoException(AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFPayGoWeb: '+AErrorMsg);
  raise EACBrTEFPayGoWeb.Create(AErrorMsg);
end;

procedure TACBrTEFPGWebAPI.VerificarOK(iRET: SmallInt);
var
  MsgErro: String;
begin
  if (iRET = PWRET_OK) then
    Exit;

  MsgErro := ObterUltimoRetorno;
  if (MsgErro <> '') then
    DoException(Format('%s (%s)', [ACBrStr(MsgErro), PWRETToString(iRET)]));
end;

function TACBrTEFPGWebAPI.ObterInfo(iINFO: Word): String;
var
  pszData: PAnsiChar;
  ulDataSize: LongWord;
  iRet: SmallInt;
  MsgErro: String;
begin
  LoadDLLFunctions;
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
        PWRET_NODATA: MsgErro := ''; //sErrPWRET_NODATA;
        PWRET_BUFOVFLW: MsgErro := sErrPWRET_BUFOVFLW;
        PWRET_DLLNOTINIT: MsgErro := sErrPWRET_DLLNOTINIT;
        PWRET_TRNNOTINIT: MsgErro := sErrPWRET_TRNNOTINIT;
        PWRET_NOTINST: MsgErro := sErrPWRET_NOTINST;
      else
        MsgErro := PWRETToString(iRet);
      end;

      if (MsgErro <> '') then
        DoException(ACBrStr(MsgErro));
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

procedure TACBrTEFPGWebAPI.IniciarTransacao(iOPER: SmallInt;
  ParametrosAdicionaisTransacao: TStrings);
var
  iRet: SmallInt;
  MsgErro: String;
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
      PWRET_DLLNOTINIT: MsgErro := sErrPWRET_DLLNOTINIT;
      PWRET_NOTINST: MsgErro := sErrPWRET_NOTINST;
    else
      MsgErro := ObterUltimoRetorno;
    end;

    DoException(ACBrStr(MsgErro));
  end;

  SetEmTransacao(True);
  try
    AdicionarDadosObrigatorios;

    For i := 0 to ParametrosAdicionais.Count-1 do
      AdicionarParametro(ParametrosAdicionais[i]);

    if Assigned(ParametrosAdicionaisTransacao) then
    begin
      For i := 0 to ParametrosAdicionaisTransacao.Count-1 do
        AdicionarParametro(ParametrosAdicionaisTransacao[i]);
    end;

  except
    On E: Exception do
    begin
      AbortarTransacao;
      DoException(E.Message);
    end;
  end;
end;

procedure TACBrTEFPGWebAPI.AdicionarParametro(iINFO: Word; const AValor: AnsiString);
var
  iRet: SmallInt;
  MsgErro: String;
begin
  GravarLog('PW_iAddParam( '+PWINFOToString(iINFO)+', '+AValor+' )');
  iRet := xPW_iAddParam(iINFO, PAnsiChar(AValor));
  GravarLog('  '+PWRETToString(iRet));
  if (iRet <> PWRET_OK) then
  begin
    case iRet of
      PWRET_INVPARAM: MsgErro := Format(sErrPWRET_INVPARAM, [AValor, PWINFOToString(iINFO)]);
      PWRET_DLLNOTINIT: MsgErro := sErrPWRET_DLLNOTINIT;
      PWRET_NOTINST: MsgErro := sErrPWRET_NOTINST;
      PWRET_TRNNOTINIT: MsgErro := sErrPWRET_TRNNOTINIT;
    else
      MsgErro := ObterUltimoRetorno;
    end;

    DoException(ACBrStr(MsgErro));
  end;
end;

procedure TACBrTEFPGWebAPI.AdicionarParametro(AKeyValueStr: String);
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
  AMsg: String;
begin
  GravarLog('TACBrTEFPGWebAPI.ExecutarTransacao');
  iRet := PWRET_CANCEL;
  try
    try
      AMsg := Trim(ObterInfo(PWINFO_PROCESSMSG));
      if (AMsg <> '') then
        ExibirMensagem(AMsg, tmCliente);

      repeat
        NumParams := Length(ArrParams);
        GravarLog('PW_iExecTransac()');
        {$warnings off}
        iRet := xPW_iExecTransac(ArrParams, NumParams);
        {$warnings on}
        GravarLog('  '+PWRETToString(iRet)+', NumParams: '+IntToStr(NumParams));

        case iRet of
          PWRET_OK:
            Break;

          PWRET_NOTHING:
            Sleep(CSleepNothing);

          PWRET_MOREDATA:
            begin
              if (ObterDados(ArrParams, NumParams) <> PWRET_OK) then
              begin
                AbortarTransacao;
                iRet := PWRET_CANCEL;
              end;
            end;

          PWRET_NOMANDATORY:
            raise EACBrTEFPayGoWeb.Create(sErrPWRET_NOMANDATORY);

          PWRET_DLLNOTINIT:
            raise EACBrTEFPayGoWeb.Create(sErrPWRET_DLLNOTINIT);

          PWRET_NOTINST:
            raise EACBrTEFPayGoWeb.Create(sErrPWRET_NOTINST);

          PWRET_TRNNOTINIT:
            raise EACBrTEFPayGoWeb.Create(sErrPWRET_TRNNOTINIT);

        else
          raise EACBrTEFPayGoWeb.Create(ObterUltimoRetorno);

        end;

      until (iRet = PWRET_OK) or (iRet = PWRET_CANCEL);
    except
      On E: Exception do
      begin
        AbortarTransacao;
        DoException(E.Message);
      end;
    end;

    if (iRet = PWRET_CANCEL) then
    begin
      AMsg := ObterInfo(PWINFO_CNCPPMSG);
      if (Trim(AMsg) <> '') then
      begin
        GravarLog('xPW_iPPDisplay( '+AMsg+' )');
        iRetPP := xPW_iPPDisplay( PAnsiChar(AnsiString(AMsg)) );
        GravarLog('  '+PWRETToString(iRetPP));
      end;

      AMsg := ObterInfo(PWINFO_CNCDSPMSG);
      if (Trim(AMsg) <> '') then
        ExibirMensagem(AMsg, tmOperador, CMilissegundosMensagem);
    end;

    ObterDadosDaTransacao;
    fDadosTransacao.Add('0='+IntToStr(iRet));  // 0 = PWINFO_RET (último retorno)
  finally
    SetEmTransacao(False);
  end;

  Result := (iRet = PWRET_OK);
end;

procedure TACBrTEFPGWebAPI.ObterDadosDaTransacao;
var
  pszData: PAnsiChar;
  ulDataSize: LongWord;
  iRet: SmallInt;
  i: Word;
  AData: String;
begin
  GravarLog('TACBrTEFPGWebAPI.ObterDadosTransacao');
  fDadosTransacao.Clear;
  ulDataSize := 10240;   // 10K
  pszData := AllocMem(ulDataSize);
  try
    For i := MIN_PWINFO to MAX_PWINFO do
    begin
      iRet := xPW_iGetResult(i, pszData, ulDataSize);
      if (iRet = PWRET_OK) then
      begin
        AData := BinaryStringToString(AnsiString(pszData));
        fDadosTransacao.Add(Format('%d=%s', [i, Adata]));
        GravarLog('  '+Format('%s=%s', [PWINFOToString(i), AData]));
        if (i = PWINFO_IDLEPROCTIME) then
          AjustarTempoOcioso(AData);
      end;
    end;
  finally
    Freemem(pszData);
  end;
end;

procedure TACBrTEFPGWebAPI.FinalizarTrancao(Status: LongWord;
  pszReqNum: String; pszLocRef: String; pszExtRef: String;
  pszVirtMerch: String; pszAuthSyst: String);
var
  bConfirma: Boolean;
  MsgErro: String;
  iRet: SmallInt;
begin
  GravarLog('TACBrTEFPGWebAPI.FinalizarTrancao');
  bConfirma := (Trim(ObterInfo(PWINFO_CNFREQ)) = '1');
  if not bConfirma then
    Exit;

  if (pszReqNum = '') then
    pszReqNum := Trim(ObterInfo(PWINFO_REQNUM));

  if (pszLocRef = '') then
    pszLocRef := Trim(ObterInfo(PWINFO_AUTLOCREF));

  if (pszExtRef = '') then
    pszExtRef := Trim(ObterInfo(PWINFO_AUTEXTREF));

  if (pszVirtMerch = '') then
    pszVirtMerch := Trim(ObterInfo(PWINFO_VIRTMERCH));

  if (pszAuthSyst = '') then
    pszAuthSyst := Trim(ObterInfo(PWINFO_AUTHSYST));

  GravarLog('PW_iConfirmation( '+PWCNFToString(Status)+', '+
                                 pszReqNum+', '+
                                 pszLocRef+', '+
                                 pszExtRef+', '+
                                 pszVirtMerch+', '+
                                 pszAuthSyst+' ) ');
  iRet := xPW_iConfirmation( Status,
                             PAnsiChar(pszReqNum),
                             PAnsiChar(pszLocRef),
                             PAnsiChar(pszExtRef),
                             PAnsiChar(pszVirtMerch),
                             PAnsiChar(pszAuthSyst) );
  GravarLog('  '+PWRETToString(iRet));
  if (iRet <> PWRET_OK) then
  begin
    case iRet of
      PWRET_DLLNOTINIT: MsgErro := sErrPWRET_DLLNOTINIT;
      PWRET_NOTINST: MsgErro := sErrPWRET_NOTINST;
      PWRET_INVALIDTRN: MsgErro := sErrPWRET_INVALIDTRN;
    else
      MsgErro := ObterUltimoRetorno;
    end;

    DoException(ACBrStr(MsgErro));
  end;
end;

procedure TACBrTEFPGWebAPI.AbortarTransacao;
begin
  GravarLog('TACBrTEFPGWebAPI.AbortarTransacao');
  if EmTransacao then
    FinalizarTrancao(PWCNF_REV_ABORT);
end;

function TACBrTEFPGWebAPI.ValidarRespostaCampo(AResposta: String;
  ADefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo): String;
var
  Valido: Boolean;
  ARespInt: Int64;
begin
  Valido := True;
  Result := '';
  if (ADefinicaoCampo.TiposEntradaPermitidos = pgtNumerico) then
  begin
    ARespInt := StrToInt64Def(AResposta, -1);
    if ARespInt > ADefinicaoCampo.ValorMaximo then
      Result := Trim(ADefinicaoCampo.MsgDadoMaior)
    else if ARespInt < ADefinicaoCampo.ValorMinimo then
      Result := Trim(ADefinicaoCampo.MsgDadoMenor);
  end
  else
  case ADefinicaoCampo.ValidacaoDado of
    pgvNaoVazio:
      Valido := (AResposta <> '');
    pgvDigMod10:
      Valido := ValidarModulo10(AResposta);
    pgvCPF_CNPJ:
      Valido := (ACBrValidador.ValidarCNPJouCPF(AResposta) = '');
    pgvMMAA:
      Valido := ValidarDDMM(AResposta);
    pgvDDMMAA:
      Valido := ValidarDDMMAA(AResposta);
  end;

  if not Valido then
    Result := ADefinicaoCampo.MsgValidacao;
end;

function TACBrTEFPGWebAPI.ObterDados(ArrGetData: TArrPW_GetData;
  ArrLen: SmallInt): SmallInt;
var
  AGetData: TPW_GetData;
  i, j: Integer;
  MsgPrevia: String;
  iRet: SmallInt;
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
      MsgPrevia := Trim(AGetData.szMsgPrevia);
      if (MsgPrevia <> '') then
        ExibirMensagem(MsgPrevia, tmOperador, CMilissegundosMensagem);

      j := 1;
      while (iRet = PWRET_OK) and (j <= max(AGetData.bNumeroCapturas,1)) do
      begin
        case AGetData.bTipoDeDado of
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
            iRet := RealizarOperacaoPinPad(AGetData, ppRemoveCard);
          PWDAT_PPGENCMD:
            iRet := RealizarOperacaoPinPad(AGetData, ppGenericCMD);
          //PWDAT_PPDATAPOSCNF:
          //  iRet := RealizarOperacaoPinPad(AGetData, ppDataConfirmation);
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
  i: Integer;
begin
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

  if Cancelado then
    AbortarTransacao;

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
  ObterDigitado := (AGetData.ulTipoEntradaCartao = 1);

  case AGetData.ulTipoEntradaCartao of
    2:
    begin
      iRet := RealizarOperacaoPinPad(AGetData, ppGetCard);
      ObterDigitado := (iRet = PWRET_FALLBACK);
    end;

    3:
    begin
      iRet := RealizarOperacaoPinPad(AGetData, ppGetCard);
      ObterDigitado := (iRet = PWRET_CANCEL) or (iRet = PWRET_FALLBACK);
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
    //ppDataConfirmation:
    //begin
    //  GravarLog('PW_iPPDataConfirmation( '+IntToStr(AGetData.bIndice)+' )');
    //  iRet := xPW_iPPDataConfirmation(AGetData.bIndice);
    //end
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
          ExibirMensagem(AMsg, tmOperador);
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

      GravarLog('  OnAguardaPinPad');
      fOnAguardaPinPad(OperacaoPinPad, Cancelado);
    end;
  finally
    Freemem(pszDisplay);
  end;

  if Cancelado then
  begin
    GravarLog('PW_iPPAbort');
    iRet := xPW_iPPAbort;
    GravarLog('  '+PWRETToString(iRet));
    case iRet of
      PWRET_OK:
        iRet := PWRET_CANCEL;  // Sinaliza Cancelado, para função chamadora

      PWRET_PPCOMERR:
        DoException(ACBrStr(sErrPWRET_PPCOMERR));

      PWRET_DLLNOTINIT:
        DoException(ACBrStr(sErrPWRET_DLLNOTINIT));

    else
      DoException(ACBrStr(ObterUltimoRetorno));
    end;
  end;

  Result := iRet;
end;

function TACBrTEFPGWebAPI.ObterDadoCartaoDigitado(AGetData: TPW_GetData
  ): SmallInt;
var
  AResposta: String;
  AStr: AnsiString;
begin
  AResposta := '';
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

function TACBrTEFPGWebAPI.ValidarDDMM(AString: String): Boolean;
begin
  Result := False;
  if Length(AString) <> 4 then
    Exit;

  Result := ValidarDDMMAA(AString + '00');
end;

function TACBrTEFPGWebAPI.ValidarDDMMAA(AString: String): Boolean;
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

function TACBrTEFPGWebAPI.ValidarModulo10(AString: String): Boolean;
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
  if fImprimirViaClienteReduzida then
    Inc(Result, 16);      // 16: impressão do cupom reduzido
  if fUtilizaSaldoTotalVoucher then
    Inc(Result, 32);      // 32: utilização de saldo total do voucher para abatimento do valor da compra
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

procedure TACBrTEFPGWebAPI.SetPathDLL(AValue: String);
begin
  if fPathDLL = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAPI.SetPathDLL( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fPathDLL := AValue;
end;

procedure TACBrTEFPGWebAPI.SetPontoCaptura(AValue: String);
begin
  if fPontoCaptura = AValue then Exit;
  fPontoCaptura := LeftStr(OnlyNumber(AValue),11);
end;

procedure TACBrTEFPGWebAPI.SetPortaTCP(AValue: String);
begin
  if fPortaTCP = AValue then Exit;
  fPortaTCP := Trim(AValue);
end;

procedure TACBrTEFPGWebAPI.SetSoftwareHouse(AValue: String);
begin
  if fSoftwareHouse = AValue then Exit;
  fSoftwareHouse := LeftStr(Trim(AValue),50);
end;

procedure TACBrTEFPGWebAPI.SetNomeAplicacao(AValue: String);
begin
  if fNomeAplicacao = AValue then Exit;
  fNomeAplicacao := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFPGWebAPI.SetNomeEstabelecimento(AValue: String);
begin
  if fNomeEstabelecimento = AValue then Exit;
  fNomeEstabelecimento := LeftStr(Trim(AValue),100);
end;

procedure TACBrTEFPGWebAPI.SetVersaoAplicacao(AValue: String);
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
    fDadosTransacao.Clear
  else
    AjustarTempoOcioso;
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

procedure TACBrTEFPGWebAPI.SetDiretorioTrabalho(AValue: String);
begin
  if fDiretorioTrabalho = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAPI.SetDiretorioTrabalho( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fDiretorioTrabalho := AValue;
end;

procedure TACBrTEFPGWebAPI.SetEnderecoIP(AValue: String);
begin
  if fEnderecoIP = AValue then Exit;
  fEnderecoIP := Trim(AValue);
end;

procedure TACBrTEFPGWebAPI.SetCNPJEstabelecimento(AValue: String);
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

procedure TACBrTEFPGWebAPI.LoadDLLFunctions;

  procedure PGWebFunctionDetect( FuncName: AnsiString; var LibPointer: Pointer ) ;
  var
    sLibName: string;
  begin
    if not Assigned( LibPointer )  then
    begin
      GravarLog('   '+FuncName);

      // Verifica se exite o caminho das DLLs
      sLibName := '';
      if Length(PathDLL) > 0 then
        sLibName := PathWithDelim(PathDLL);

      // Concatena o caminho se exitir mais o nome da DLL.
      sLibName := sLibName + CACBrTEFPGWebLib;

      if not FunctionDetect( sLibName, FuncName, LibPointer) then
      begin
        LibPointer := NIL ;
        DoException(Format(ACBrStr('Erro ao carregar a função: %s de: %s'),[FuncName, sLibName]));
      end ;
    end ;
  end;

 begin
   if fInicializada then
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
   //PGWebFunctionDetect('PW_iPPDataConfirmation', @xPW_iPPDataConfirmation);
   PGWebFunctionDetect('PW_iPPDisplay', @xPW_iPPDisplay);
   PGWebFunctionDetect('PW_iPPGetUserData', @xPW_iPPGetUserData);
   PGWebFunctionDetect('PW_iPPWaitEvent', @xPW_iPPWaitEvent);
   PGWebFunctionDetect('PW_iPPRemoveCard', @xPW_iPPRemoveCard);
   PGWebFunctionDetect('PW_iPPGetPINBlock', @xPW_iPPGetPINBlock);
   PGWebFunctionDetect('PW_iTransactionInquiry', @xPW_iTransactionInquiry);
end;

procedure TACBrTEFPGWebAPI.UnLoadDLLFunctions;
var
  sLibName: String;
begin
  if not fInicializada then
    Exit;

  GravarLog('TACBrTEFPGWebAPI.UnLoadDLLFunctions');

  sLibName := '';
  if Length(PathDLL) > 0 then
     sLibName := PathWithDelim(PathDLL);

  UnLoadLibrary( sLibName + CACBrTEFPGWebLib );
  ClearMethodPointers;
end;

end.

