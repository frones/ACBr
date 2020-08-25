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

unit ACBrTEFPayGoComum;

interface

uses
  Classes, SysUtils,
  ACBrTEFComum;

const
  PWRET_OK = 0;      // Operação bem sucedida

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
  PWINFO_TRNORIGLOCREF  = 120;    // Referência local da transação original, no caso de um cancelamento.
  PWINFO_CARDENTMODE    = 192;    // Modo(s) de entrada do cartão: 1: digitado 2: tarja magnética 4: chip com contato 16: fallback de chip para tarja 32: chip sem contato simulando tarja (cliente informa tipo efetivamente utilizado) 64: chip sem contato EMV (cliente informa tipo efetivamente utilizado) 256: fallback de tarja para digitado
  PWINFO_CARDFULLPAN    = 193;    // Número do cartão completo, para transação digitada. Este dado não pode ser recuperado pela função PW_iGetResult
  PWINFO_CARDEXPDATE    = 194;    // Data de vencimento do cartão (formato “MMAA”).
  PWINFO_CARDNAMESTD    = 196;    // Descrição do produto bandeira padrão relacionado ao BIN.
  PWINFO_PRODNAMEDESC   = 197;    // Descrição do nome do produto ou bandeira.
  PWINFO_CARDPARCPAN    = 200;    // Número do cartão, truncado ou mascarado
  PWINFO_CHOLDVERIF     = 207;    // Verificação do portador, soma dos seguintes valores: “1”: Assinatura do portador em papel. “2”: Senha verificada off-line. “4”: Senha off-line bloqueada no decorrer desta transação. “8”: Senha verificada online
  PWINFO_EMVRESPCODE    = 214;    // Identificador do resultado final do processamento de cartão com chip: 1: Transação aprovada. 2: Transação negada pelo cartão. 3.Transação negada pelo Host. Caso não seja uma transação com chip, o valor não irá existir.
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
  PWINFO_MERCHNAMERCPT  = 250;    // Nome que identifica o estabelecimento nos comprovantes.
  PWINFO_PRODESTABRCPT  = 251;    // Descrição do produto/cartão utilizado na transação, para o estabelecimento.
  PWINFO_PRODCLIRCPT    = 252;    // Descrição do produto/cartão utilizado na transação, para o cliente.
  PWINFO_EMVCRYPTTYPE   = 253;    // Tipo de criptograma gerado no 1º Generate AC do processo   EMV:   “ARQC” para transações submetidas à autorização do   emissor.   “TC” para transações efetuadas sem autorização do emissor.
  PWINFO_TRNORIGAUTHCODE= 254;    // Código de autorização da transação original, no caso de um cancelamento.
  PWINFO_PAYMNTMODE     = 7969;   // Modalidade de pagamento:   1: cartão   2: dinheiro   4: cheque   8: carteira virtual
  PWINFO_GRAPHICRCPHEADER= 7990;  // Até 100 Cabeçalho do comprovante gráfico recebido do servidor.
  PWINFO_GRAPHICRCPFOOTER= 7991;  // Rodapé do comprovante gráfico recebido do servidor.
  PWINFO_CHOLDERNAME    = 7992;   // Nome do portador do cartão utilizado, o tamanho segue o mesmo padrão da tag 5F20 EMV.
  PWINFO_MERCHNAMEPDC   = 7993;   // Nome do estabelecimento em que o ponto de captura está cadastrado. (até 100)
  PWINFO_TRANSACDESCRIPT= 8000;   // Descritivo da transação realizada, por exemplo, CREDITO A VISTA ou VENDA PARCELADA EM DUAS VEZES.
  PWINFO_ARQC           = 8001;   // ARQC
  PWINFO_DEFAULTCARDPARCPAN = 8002; // Número do cartão mascarado no formato BIN + *** + 4 últimos dígitos. Ex: 543211******987
  PWINFO_SOFTDESCRIPTOR = 8003;   // Texto que será de identificação na fatura do portador do cartão
  PWINFO_SPLITPAYMENT   = 8025;   // O campo PWINFO_SPLITPAYMENT deverá possuir as seguintes informações separadas por vírgula ‘,’: Afiliação: Identificador do lojista do ponto de vista do adquirente. Valor: Valor parcial a ser enviado para a afiliação do split. OBS: A soma de todos os valores referente ao split de pagamento deverá ser igual a PWINFO_TOTAMNT Exemplo: 8DA10E01A6A6213, 1200 Para cada conjunto de informações de split de pagamento, conforme o exemplo acima, deverá ser feito o PW_iAddParam informando a tag PWINFO_SPLITPAYMENT e as informações atualizadas.
  PWINFO_AUTHPOSQRCODE  = 8055;   // Conteúdo do QR Code identificando o checkout para o autorizador.
  PWINFO_WALLETUSERIDTYPE=8065;   // Forma de identificação do portador da carteira virtual: 1: QRCode do checkout (lido pelo celular do portador) 2: CPF 128: outros
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
  PWINFO_RESULTID       = 32534;  // Identificador do resultado da operação do ponto de vista do Servidor.
  PWINFO_DSPCHECKOUT1   = 32535;  // Mensagem a ser exibida no cliente durante as transições de determinadas capturas. A automação deverá informar a capacidade desse tratamento no PWINFO_AUTCAP
  PWINFO_DSPCHECKOUT2   = 32536;  // Mensagem a ser exibida no cliente durante as transições de determinadas capturas. A automação deverá informar a capacidade desse tratamento no PWINFO_AUTCAP
  PWINFO_DSPCHECKOUT3   = 32537;  // Mensagem a ser exibida no cliente durante as transições de determinadas capturas. A automação deverá informar a capacidade desse tratamento no PWINFO_AUTCAP
  PWINFO_DSPCHECKOUT4   = 32538;  // Mensagem a ser exibida no cliente durante as transições de determinadas capturas. A automação deverá informar a capacidade desse tratamento no PWINFO_AUTCAP
  PWINFO_DSPCHECKOUT5   = 32539;  // Mensagem a ser exibida no cliente durante as transições de determinadas capturas. A automação deverá informar a capacidade desse tratamento no PWINFO_AUTCAP
  PWINFO_CTLSCAPTURE    = 32540;  // Deve ser adicionado para sinalizar que a automação deseja fazer uma captura de cartão sem contato. Se o autorizador permitir, a captura será executada. Não deverá ser adicionada caso já tenha sido capturado cartão digitado, trilha magnética ou chip.
  PWINFO_CHOLDERGRARCP  = 32541;  // Deve ser adicionado para sinalizar que a vila do cliente foi impressa utilizando o comprovante gráfico.
  PWINFO_MERCHGRARCP    = 32542;  // Deve ser adicionado para sinalizar que a via do estabelecimento foi impressa utilizando o comprovante gráfico.
  PWINFO_GRAPHICRCP     = 40722;  // Indica, se possível, a necessidade de impressão de um comprovante gráfico: 0: Não necessário. 1: Necessário.
  PWINFO_OPERATIONORIG  = 40727;  // Tipo de transação (PWOPER_xxx) da transação original, no caso de reimpressões. Consultar os valores possíveis na descrição da função PW_iNewTransac (página 14)
  PWINFO_DUEAMNT        = 48902;  // Valor devido pelo usuário, considerando PWINFO_CURREXP, já deduzido em PWINFO_TOTAMNT
  PWINFO_READJUSTEDAMNT = 48905;  // Valor total da transação reajustado, este campo será utilizado caso o autorizador, por alguma regra de negócio específica dele, resolva alterar o valor total que foi solicitado para a transação
  PWINFO_TRNORIGDATETIME= 48909;  // Data e hora da transação original, no formato “AAAAMMDDhhmmss”, no caso de um cancelamento.
  PWINFO_DATETIMERCPT   = 48910;  // Data/hora da transação para exibição no comprovante, no formato “AAAAMMDDhhmmss”.
  PWINFO_UNIQUEID       = 49040;  // ID único da transação armazenada no banco de dados

  MIN_PWINFO = PWINFO_OPERATION;
  MAX_PWINFO = PWINFO_UNIQUEID;

type
  { TACBrTEFPGWebAPIParametros }

  TACBrTEFPGWebAPIParametros = class(TStringList)
  private
    function GetValueInfo(AInfo: Word): string;
    procedure SetValueInfo(AInfo: Word; AValue: string);
  public
    property ValueInfo[AInfo: Word]: string read GetValueInfo write SetValueInfo;
  end;

procedure ConteudoToPropertyPayGoWeb(AACBrTEFResp: TACBrTEFResp);

function ParseKeyValue(AKeyValueStr: String; out TheKey: String; out TheValue: String): Boolean;
function PWINFOToString(iINFO: Word): String;
function PWOPERToString(iOPER: SmallInt): String;

function MoedaToISO4217(AMoeda: Byte): Word;
function ISO4217ToMoeda(AIso4217: Word): Byte;

implementation

uses
  Math, DateUtils, StrUtils,
  ACBrConsts;

procedure ConteudoToPropertyPayGoWeb(AACBrTEFResp: TACBrTEFResp);

  procedure ConteudoToComprovantes;
  var
    ImprimirViaCliente, ImprimirViaEstabelecimento: Boolean;
    ViaCompleta, ViaDiferenciada, ViasDeComprovante: String;
  begin
    with AACBrTEFResp do
    begin
      {
      PWINFO_RCPTPRN      F4h 1 Indica quais vias de comprovante devem ser impressas:
                              0: não há comprovante, 1: imprimir somente a via do Cliente,
                              2: imprimir somente a via do Estabelecimento, 3: imprimir ambas as vias do Cliente e do Estabelecimento
      PWINFO_RCPTFULL     52h Comprovante para impressão – Via completa.
                              Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
      PWINFO_RCPTMERCH    53h Comprovante para impressão – Via diferenciada para o Estabelecimento.
                              Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
      PWINFO_RCPTCHOLDER  54h Comprovante para impressão – Via diferenciada para o Cliente.
                              Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
      PWINFO_RCPTCHSHORT  55h Comprovante para impressão – Cupom reduzido (para o Cliente).
                              Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
      }

      ViasDeComprovante := Trim(LeInformacao(PWINFO_RCPTPRN, 0).AsString);

      if (ViasDeComprovante <> '') then
      begin
        ImprimirViaCliente := (ViasDeComprovante = '1') or (ViasDeComprovante = '3');
        ImprimirViaEstabelecimento := (ViasDeComprovante = '2') or (ViasDeComprovante = '3');
      end
      else
      begin
        ImprimirViaCliente := (Trim(LeInformacao(PWINFO_RCPTCHOLDER, 0).AsBinary) <> '') or
                              (Trim(LeInformacao(PWINFO_RCPTCHSHORT, 0).AsBinary) <> '');
        ImprimirViaEstabelecimento := (Trim(LeInformacao(PWINFO_RCPTMERCH, 0).AsBinary) <> '') or
                                      (Trim(LeInformacao(PWINFO_RCPTFULL, 0).AsBinary) <> '');
      end;

      ViaCompleta := LeInformacao(PWINFO_RCPTFULL, 0).AsBinary;

      // Verificando Via do Estabelecimento
      if ImprimirViaEstabelecimento then
      begin
        ViaDiferenciada := LeInformacao(PWINFO_RCPTMERCH, 0).AsBinary;
        if (Trim(ViaDiferenciada) <> '') then
          ImagemComprovante2aVia.Text := ViaDiferenciada
        else
          ImagemComprovante2aVia.Text := ViaCompleta;
      end
      else
        ImagemComprovante2aVia.Clear;

      // Verificando Via do Cliente
      if ImprimirViaCliente then
      begin
        ViaDiferenciada := LeInformacao(PWINFO_RCPTCHSHORT, 0).AsBinary;
        if (Trim(ViaDiferenciada) = '') then
          ViaDiferenciada := LeInformacao(PWINFO_RCPTCHOLDER, 0).AsBinary;

        if (Trim(ViaDiferenciada) <> '') then
          ImagemComprovante1aVia.Text := ViaDiferenciada
        else
          ImagemComprovante1aVia.Text := ViaCompleta;
      end
      else
        ImagemComprovante1aVia.Clear;

      QtdLinhasComprovante := max(ImagemComprovante1aVia.Count, ImagemComprovante2aVia.Count);
    end;
  end;

  procedure ConteudoToParcelas;
  var
    DataParcela: TDateTime;
    ValorParcela: Double;
    I: Integer;
    Parc: TACBrTEFRespParcela;
  begin
    with AACBrTEFResp do
    begin
      Parcelas.Clear;

      QtdParcelas := LeInformacao(PWINFO_INSTALLMENTS, 0).AsInteger;
      if (QtdParcelas > 0) then
      begin
        DataParcela := LeInformacao(PWINFO_INSTALLMDATE, 0).AsDate;
        ValorParcela := LeInformacao(PWINFO_INSTALLM1AMT, 0).AsFloat;

        for I := 1 to QtdParcelas do
        begin
          Parc := TACBrTEFRespParcela.create;
          Parc.Vencimento := DataParcela;
          Parc.Valor := ValorParcela;
          Parc.NSUParcela := NSU;
          Parcelas.Add(Parc);

          ValorParcela := LeInformacao(PWINFO_INSTALLMAMNT, 0).AsFloat;
          DataParcela := IncDay(DataParcela,30);
        end;
      end;
    end;
  end;

var
  I, AInt: Integer;
  LinStr: String;
  Linha: TACBrTEFLinha;
begin
  with AACBrTEFResp do
  begin
    ImagemComprovante1aVia.Clear;
    ImagemComprovante2aVia.Clear;
    Debito := False;
    Credito := False;
    Digitado := False;
    TaxaServico := 0;
    DataHoraTransacaoCancelada := 0;

    for I := 0 to Conteudo.Count - 1 do
    begin
      Linha := Conteudo.Linha[I];
      LinStr := Linha.Informacao.AsBinary;

      case Linha.Identificacao of
        PWINFO_TOTAMNT:
          ValorTotal := Linha.Informacao.AsFloat;

        PWINFO_DISCOUNTAMT:
          Desconto := Linha.Informacao.AsFloat;

        PWINFO_CASHBACKAMT:
          Saque := Linha.Informacao.AsFloat;

        PWINFO_CURRENCY:
        begin
          AInt := Linha.Informacao.AsInteger;
          Moeda := ISO4217ToMoeda(AInt);
        end;

        PWINFO_CNFREQ:
          Confirmar := (Trim(Linstr)='1');

        PWINFO_FISCALREF:
          DocumentoVinculado := LinStr;

        PWINFO_CARDTYPE:
        begin
          // 1: crédito, 2: débito, 4: voucher/PAT, 8: private label, 16: frota, 128: outros
          AInt := Linha.Informacao.AsInteger;
          Credito := (AInt = 1);
          Debito := (AInt = 2);
        end;

        PWINFO_CARDENTMODE:
        begin
          // 1: digitado, 2: tarja magnética, 4: chip com contato, 16: fallback de chip para tarja,
          // 32: chip sem contato simulando tarja (cliente informa tipo efetivamente utilizado),
          // 64: chip sem contato EMV (cliente informa tipo efetivamente, utilizado),
          // 256: fallback de tarja para digitado
          AInt := Linha.Informacao.AsInteger;
          Digitado := (AInt = 1) or (AInt = 256);
        end;

        PWINFO_CARDFULLPAN:
        begin
          Bin := LinStr;
          NFCeSAT.UltimosQuatroDigitos := RightStr(LinStr,4);
        end;

        PWINFO_CARDPARCPAN:
        begin
          if (NFCeSAT.UltimosQuatroDigitos = '') then
            NFCeSAT.UltimosQuatroDigitos := RightStr(LinStr,4);
        end;

        PWINFO_DEFAULTCARDPARCPAN:
          NFCeSAT.UltimosQuatroDigitos := RightStr(LinStr,4);

        PWINFO_CARDEXPDATE:
          NFCeSAT.DataExpiracao := LinStr;

        PWINFO_DATETIME:
          DataHoraTransacaoLocal := Linha.Informacao.AsTimeStampSQL;

        PWINFO_AUTDATETIME:
          DataHoraTransacaoHost :=  Linha.Informacao.AsTimeStampSQL;

        PWINFO_AUTHSYST:
          Rede := LinStr;

        PWINFO_CARDNAME:
        begin
          CodigoBandeiraPadrao := LinStr;
          if (NFCeSAT.Bandeira = '') then
            NFCeSAT.Bandeira := LinStr;
        end;

        PWINFO_CARDNAMESTD:
        begin
          NomeAdministradora := LinStr;
          NFCeSAT.Bandeira := LinStr;
        end;

        PWINFO_CHOLDERNAME:
          NFCeSAT.DonoCartao := LinStr;

        PWINFO_AUTLOCREF:
          Finalizacao := LinStr;

        PWINFO_AUTEXTREF:
        begin
          NSU := LinStr;
          NFCeSAT.Autorizacao := NSU;
        end;

        PWINFO_REQNUM:
          NumeroLoteTransacao := Linha.Informacao.AsInt64;

        PWINFO_VIRTMERCH:
          Estabelecimento := LinStr;

        PWINFO_AUTHCODE:
          CodigoAutorizacaoTransacao := LinStr;

        //PWINFO_AUTRESPCODE:
        //  Autenticacao := LinStr;

        PWINFO_FINTYPE:
        begin
          // 1: à vista, 2: parcelado pelo emissor, 4: parcelado pelo estabelecimento, 8: pré-datado, 16: crédito emissor
          AInt := Linha.Informacao.AsInteger;
          if (AInt = 2) then
          begin
            ParceladoPor := parcADM;
            TipoOperacao := opParcelado;
          end
          else if (AInt = 4) then
          begin
            ParceladoPor := parcLoja;
            TipoOperacao := opParcelado;
          end
          else if (AInt = 8) then
          begin
            ParceladoPor := parcNenhum;
            TipoOperacao := opPreDatado;
          end
          else if (AInt = 16) then
          begin
            ParceladoPor := parcNenhum;
            TipoOperacao := opOutras;
          end
          else
          begin
            ParceladoPor := parcNenhum;
            TipoOperacao := opAvista;
          end
        end;

        PWINFO_INSTALLMDATE:
          DataPreDatado := Linha.Informacao.AsDate;

        PWINFO_BOARDINGTAX, PWINFO_TIPAMOUNT:
          TaxaServico := TaxaServico + Linha.Informacao.AsFloat;

        PWINFO_TRNORIGDATE:
          DataHoraTransacaoCancelada := DataHoraTransacaoCancelada + Linha.Informacao.AsDate;

        PWINFO_TRNORIGTIME:
          DataHoraTransacaoCancelada := DataHoraTransacaoCancelada + Linha.Informacao.AsTime;

        PWINFO_TRNORIGNSU:
          NSUTransacaoCancelada := LinStr;

        PWINFO_TRNORIGAMNT:
          ValorOriginal := Linha.Informacao.AsFloat;

        PWINFO_AUTHPOSQRCODE:
          QRCode := LinStr;

        //PWINFO_PRODUCTID   3Eh até 8 Identificação do produto utilizado, de acordo com a nomenclatura do Provedor.
        //PWINFO_PRODUCTNAME 2Ah até 20 Nome/tipo do produto utilizado, na nomenclatura do Provedor
        //PWINFO_AUTMERCHID  38h até 50 Identificador do estabelecimento para o Provedor (código de afiliação)
        //PWINFO_TRANSACDESCRIPT 1F40h Até 80 Descritivo da transação realizada, por exemplo, CREDITO A VISTA ou VENDA PARCELADA EM DUAS VEZES.
      else
        ProcessarTipoInterno(Linha);
      end;
    end;

    ConteudoToComprovantes;
    ConteudoToParcelas;

    if (TipoOperacao <> opPreDatado) then
      DataPreDatado := 0;

    if (LeInformacao(PWINFO_RET, 0).AsInteger = PWRET_OK) then
      TextoEspecialOperador := LeInformacao(PWINFO_RESULTMSG, 0).AsBinary
    else
      TextoEspecialOperador := LeInformacao(PWINFO_CNCDSPMSG, 0).AsBinary;

    if (Trim(TextoEspecialOperador) = '') then
      TextoEspecialOperador := 'TRANSACAO FINALIZADA'
    else if (copy(TextoEspecialOperador,1,1) = CR) then
      TextoEspecialOperador := copy(TextoEspecialOperador, 2, Length(TextoEspecialOperador));
  end;
end;

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
    PWINFO_TRNORIGLOCREF:   Result := 'PWINFO_TRNORIGLOCREF';
    PWINFO_CARDENTMODE:     Result := 'PWINFO_CARDENTMODE';
    PWINFO_CARDFULLPAN:     Result := 'PWINFO_CARDFULLPAN';
    PWINFO_CARDEXPDATE:     Result := 'PWINFO_CARDEXPDATE';
    PWINFO_CARDNAMESTD:     Result := 'PWINFO_CARDNAMESTD';
    PWINFO_PRODNAMEDESC:    Result := 'PWINFO_PRODNAMEDESC';
    PWINFO_CARDPARCPAN:     Result := 'PWINFO_CARDPARCPAN';
    PWINFO_CHOLDVERIF:      Result := 'PWINFO_CHOLDVERIF';
    PWINFO_EMVRESPCODE:     Result := 'PWINFO_EMVRESPCODE';
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
    PWINFO_MERCHNAMERCPT:   Result := 'PWINFO_MERCHNAMERCPT';
    PWINFO_PRODESTABRCPT:   Result := 'PWINFO_PRODESTABRCPT';
    PWINFO_PRODCLIRCPT:     Result := 'PWINFO_PRODCLIRCPT';
    PWINFO_EMVCRYPTTYPE:    Result := 'PWINFO_EMVCRYPTTYPE';
    PWINFO_TRNORIGAUTHCODE: Result := 'PWINFO_TRNORIGAUTHCODE';
    PWINFO_PAYMNTMODE:      Result := 'PWINFO_PAYMNTMODE';
    PWINFO_GRAPHICRCPHEADER: Result := 'PWINFO_GRAPHICRCPHEADER';
    PWINFO_GRAPHICRCPFOOTER: Result := 'PWINFO_GRAPHICRCPFOOTER';
    PWINFO_CHOLDERNAME:     Result := 'PWINFO_CHOLDERNAME';
    PWINFO_MERCHNAMEPDC:    Result := 'PWINFO_MERCHNAMEPDC';
    PWINFO_TRANSACDESCRIPT: Result := 'PWINFO_TRANSACDESCRIPT';
    PWINFO_ARQC:            Result := 'PWINFO_ARQC';
    PWINFO_DEFAULTCARDPARCPAN: Result := 'PWINFO_DEFAULTCARDPARCPAN';
    PWINFO_SOFTDESCRIPTOR:  Result := 'PWINFO_SOFTDESCRIPTOR';
    PWINFO_SPLITPAYMENT:    Result := 'PWINFO_SPLITPAYMENT';
    PWINFO_AUTHPOSQRCODE:   Result := 'PWINFO_AUTHPOSQRCODE';
    PWINFO_WALLETUSERIDTYPE:Result := 'PWINFO_WALLETUSERIDTYPE';
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
    PWINFO_RESULTID:        Result := 'PWINFO_RESULTID';
    PWINFO_DSPCHECKOUT1:    Result := 'PWINFO_DSPCHECKOUT1';
    PWINFO_DSPCHECKOUT2:    Result := 'PWINFO_DSPCHECKOUT2';
    PWINFO_DSPCHECKOUT3:    Result := 'PWINFO_DSPCHECKOUT3';
    PWINFO_DSPCHECKOUT4:    Result := 'PWINFO_DSPCHECKOUT4';
    PWINFO_DSPCHECKOUT5:    Result := 'PWINFO_DSPCHECKOUT5';
    PWINFO_CTLSCAPTURE:     Result := 'PWINFO_CTLSCAPTURE';
    PWINFO_CHOLDERGRARCP:   Result := 'PWINFO_CHOLDERGRARCP';
    PWINFO_MERCHGRARCP:     Result := 'PWINFO_MERCHGRARCP';
    PWINFO_GRAPHICRCP:      Result := 'PWINFO_GRAPHICRCP';
    PWINFO_OPERATIONORIG:   Result := 'PWINFO_OPERATIONORIG';
    PWINFO_DUEAMNT:         Result := 'PWINFO_DUEAMNT';
    PWINFO_READJUSTEDAMNT:  Result := 'PWINFO_READJUSTEDAMNT';
    PWINFO_TRNORIGDATETIME: Result := 'PWINFO_TRNORIGDATETIME';
    PWINFO_DATETIMERCPT:    Result := 'PWINFO_DATETIMERCPT';
    PWINFO_UNIQUEID:        Result := 'PWINFO_UNIQUEID';
  else
    Result := 'PWINFO_'+IntToStr(iINFO);
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

function MoedaToISO4217(AMoeda: Byte): Word;
begin
  case AMoeda of
    0: Result := 986;    // BRL
    1: Result := 840;    // USD
    2: Result := 978;    // EUR
  else
    Result := AMoeda;
  end;
end;

function ISO4217ToMoeda(AIso4217: Word): Byte;
begin
  case AIso4217 of
    986: Result := 0;    // BRL
    840: Result := 1;    // USD
    978: Result := 2;    // EUR
  else
    Result := AIso4217;
  end;
end;

{ TACBrTEFPGWebAPIParametros }

function TACBrTEFPGWebAPIParametros.GetValueInfo(AInfo: Word): string;
begin
   Result := Values[IntToStr(AInfo)];
end;

procedure TACBrTEFPGWebAPIParametros.SetValueInfo(AInfo: Word; AValue: string);
begin
  Values[IntToStr(AInfo)] := AValue;
end;


end.

