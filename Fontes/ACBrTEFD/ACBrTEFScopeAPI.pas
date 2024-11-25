{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrTEFScopeAPI;

interface

uses
  Classes, SysUtils;

{------------------------------------------------------------------------------
  DECLARACAO DE CONSTANTES GLOBAIS
------------------------------------------------------------------------------}
const
  {$IFDEF MSWINDOWS}
   CScopeLib = 'scopeapi.dll';
  {$ELSE}
   CScopeLib = 'libScopeApi.so';
  {$ENDIF}

  CScopeINi = 'scope.ini';

resourcestring
  sErrLibJaInicializda = 'Biblioteca ScopeAPI já foi inicializada';
  sErrDirTrabalhoInvalido = 'Diretório de Trabalho não encontrado: %s';
  sErrScopeINIInvalido = 'Arquivo de Configuração ' + CScopeINI + ' não encontrado em: %s';
  sErrEndServNaoEncontrado = 'Endereço do Servidor não encontrado em '+CScopeINi;
  sErrEndServNaoInformado = 'Endereço do Servidor não informada em "EnderecoIP"';
  sErrPortServNaoInformado = 'Porta do Servidor não informada em "PortTCP"';
  sErrNaoConectado = 'Não conectado ao Servidor Scope';
  sErrSemSessao = 'Sessão TEF não foi aberta';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';
  sErrTransacaoNaoIniciada = 'Transação não foi iniciada';
  sErrTransacaoJaIniciada = 'Transação já foi iniciada';

  sMsgAbrindoConexao = 'Abrindo comunicação...'+sLineBreak+'Empresa: %s, Filial %s';
  sMsgConctadoAoServidor = 'Conectado Scope em: %s';
  sMsgDesconectado = 'Desconectado Scope';
  sMsgInicioSessaoTEF = 'Iniciando sessão TEF';
  sMsgTransacaoEmAndamento = 'Transação em Andamento %s';
  sMsgTransacaoDesfeita = 'A TRANSAÇÃO TEF ANTERIOR FOI DESFEITA.'+sLineBreak+'RETER O CUPOM TEF.';

const
  CINTERVALO_COLETA = 200;
  SCO_SUCESSO = 0;
  PC_OK = 0;

  PP_NAO_UTILIZA = 0;
  PP_INTERFACE_LIB_VISA = 1;
  PP_INTERFACE_LIB_COMPARTILHADA = 2;

  PC_MODO_NONE = 0;       // Pinpad
  PC_MODO_COMPART = 1;    // Pinpad compartilhado
  PC_MODO_ABECS = 2;      // Pinpad ABECS

  PC_COMM_NONE = 0;       // Conforme configuracao do scope.ini
  PC_COMM_SERIAL = 1;     // Comunicacao serial
  PC_COMM_USB = 2;        // Comunicacao USB
  PC_COMM_BLUETOOTH = 3;  // Comunicacao Bluetooth

  SCO_DESFAZ_TEF = 0;
  SCO_CONFIRMA_TEF = 1;

  SCO_ERRO_PARM_1 = 64001; //0xFA01
  SCO_ERRO_PARM_2 = 64002; //0xFA02
  SCO_ERRO_PARM_3 = 64003; //0xFA03
  SCO_ERRO_PARM_4 = 64004; //0xFA04
  SCO_ERRO_NUM_MULTI_TEF = 64265; // 0xFB09
  SCO_ERRO_SEM_ARQUIVO_DADOS = 65037; // 0xFE0D

  SCO_ERRO_ARQ_CICLO_TEF = 64264; //0xFB08
  SCO_TRN_EM_ANDAMENTO = 65024; //0xFE00
  SCO_API_NAO_INICIALIZADA = 65025; //0xFE01
  SCO_API_JA_INICIALIZADA = 65026; //0xFE02
  SCO_SRV_NOT_CFG = 65033; //0xFE09
  SCO_ERRO_LOGON_PDV = 65349; //0xFF45
  SCO_ERRO_CONFIG_PDV = 65430; //0xFF96

  OP_DESABILITA	= 0;
  OP_HABILITA = 1;
  OP_SOMENTE_PCI = 2;

  CFG_CANCELAR_OPERACAO_PINPAD = 1;     // Permite cancelar a interacao (leitura do cartao, senha e ...) no pinpad (default: desabilitado)
  CFG_OBTER_SERVICOS = 2;               // Permite retornar o estado TC_OBTEM_SERVICOS durante o fluxo de TEF (default: desabilitado)
  CFG_NAO_ABRIR_DIGITADO_COM_PP = 4;    // Permite nao abrir o digitado na leitura do cartao com o PP Compartilhado (default: desabilitado)
  CFG_DEVOLVER_SENHA_CRIPTOGRAFADA = 8; // Permite devolver a senha criptografada com a master key da Itautec (default: desabilitado, ou seja, devolve senha aberta)
  CFG_IMPRESSORA_CARBONADA = 16;        // Permite configurar a impressora como carbonada para nao imprimir 2a via... (default: desabilitado, ou seja, no cupom exibira 1a e 2a via)
  CFG_ARMAZENA_EM_QUEDA = 32;           // Armazena dados da coleta para recuperar em queda de energia. (default: desabilitado)
  CFG_MASCARAR_DADOS = 64;              // Configura se mascaramento de dados pelo ObtemCampo esta habilitado. (default: habilitado)
  CFG_ATUALIZA_TRANSACAO_EM_QUEDA = 128;// Permite confirmar/desfazer a transacao em caso de queda de energia. (default: desabilitado, ou seja, sempre desfazer)
  CFG_PERMITIR_SAQUE = 256;             // Habilita coleta de saque em operacoes de Debito A Vista da rede Cielo
  CFG_COLETA_RECARGA_PP	= 512;          // Permite desabilitar a coleta do ddd e telefone no pinpad em recarga de celular (default: conforme configuracao do SCOPECNF)


  // Identificacao das redes
  R_GWCEL = 90;

  {- entrada dos dados -}
  //E 
  //verificar esses nomes que foram mudados de constantes... era SCO_NONE, mudou para ENT_NONE//???
  ENT_NONE = 0; //$0000;
  ENT_TECLADO = 4; //$0004;
  ENT_PIN_PAD = 8; //$0008;
  ENT_CMC_7 = 16; //$0010;
  ENT_CARTAO_MAGNETICO = 32; // $0020;
  ENT_SCANNER = 64; // $0040;

  {- ainda não sei para que servem estes ??? -}
  SCO_AUTO_ERRO_CRD_RLV_INVALIDO:   LongInt = $F900;
  SCO_AUTO_ERRO_CRD_TRK_INVALIDA:   LongInt = $F901;
  SCO_AUTO_ERRO_CRD_INVALIDO:       LongInt = $F902;
  SCO_AUTO_ERRO_CRD_VALIDADE:       LongInt = $F903;
  SCO_AUTO_ERRO_PARM_INVALIDO:      LongInt = $F904;

  {- erros relacionados ao windows -}
  SCO_THREAD_API_NOT_INIT:          LongInt = $FB01;
  SCO_ERRO_CRIA_SERV:               LongInt = $FB02;
  SCO_ERRO_CRITICA_MSG:             LongInt = $FB03;
  SCO_ERRO_MONTA_MSG:               LongInt = $FB04;

  {-coleta de dados -}
  SCO_COLETAR_CARTAO = 64512;
  SCO_COLETAR_VALIDADE_CARTAO = 64513;
  SCO_IMPRIMIR_CUPOM = 64514;
  SCO_COLETAR_CGC_CPF = 64515;
  SCO_COLETAR_BANCO = 64516;
  SCO_COLETAR_AGENIA = 64517;
  SCO_COLTAR_NUM_CHEQUE = 64518;
  SCO_COLETAR_DATA_CHEQUE = 64519;
  SCO_IMPRIMIR_CHEQUE = 64520;
  SCO_COLETAR_TRANSACAO_VISIVEL = 64521;
  SCO_COLETAR_TIPO_PARCELAMENTO = 64522;
  SCO_COLETAR_PREDATADO = 64523;
  SCO_COLETAR_PARCELA_AVISTA = 64524;
  SCO_COLETAR_DIAS_ENTRE_PARCELAS = 64525;
  SCO_COLETAR_QTD_PARCELAS = 64526;
  SCO_COLETAR_PLANO_FINANC = 64527;
  SCO_COLETAR_DDMM = 64528;
  SCO_COLETAR_SENHA = 64529;
  SCO_COLETAR_CONTROLE_SCOPE = 64530;
  SCO_COLETAR_FORMA_PAGTO = 64531;
  SCO_COLETAR_PRIMEIRO_VENCTO = 64532;
  SCO_COLETAR_VALOR_ENTRADA = 64533;
  SCO_COLETAR_FORMA_ENTRADA = 64534;
  SCO_COLETAR_CONTA_CORRENTE = 64535;
  SCO_COLETAR_QUARTO_ULT_DIGITOS = 64536;
  SCO_REIMPIRESSAO_COMPROVANTE = 64537;
  SCO_COLETAR_DEVE_CONSULTAR_PARCELAS = 64538;
  SCO_IMPRIMIR_CONSULTA = 64539;
  SCO_COLETAR_CONTINUAR = 64540;
  SCO_COLETAR_DECIDE_ULTIMO = 64541;
  SCO_COLETAR_NUM_CHEQUE_CDC = 64542;
  SCO_COLETAR_QTD_DIAS = 64543;
  SCO_COLETAR_NUM_PREAUTORIZACAO = 64544;
  SCO_COLETAR_DIA_MES_FECHADO = 64545;
  SCO_IMPRIMIR_NOTA_PROMISSORIA = 64546;
  SCO_COLETAR_CEP = 64547;
  SCO_COLETAR_NUM_ENDERECO = 64548;
  SCO_COLETAR_NUM_COMPLEMENTO = 64549;
  SCO_COLETAR_PLANO_PAGTO = 64550;
  SCO_COLETAR_CICLOS_PULAR = 64551;
  SCO_COLETAR_NUM_ITEM = 64552;
  SCO_COLETAR_COD_SEGURANCA = 64553;
  SCO_COLETAR_COD_SEGURANCA_VISIVEL = 64554;
  SCO_COLETAR_PREDATADO_TEM_GARANTIA = 64555;
  SCO_COLETAR_ACEITA_RISCO = 64556;
  SCO_COLETAR_VALOR_SAQUE = 64557;
  SCO_COLETAR_VALOR_RECARGA = 64558;
  SCO_COLETAR_DDD = 64559;
  SCO_COLETAR_FONE = 64560;
  SCO_COLETAR_DIG_VERIF_FONE = 64561;
  SCO_COLETAR_DDMMAA =64562;
  SCO_COLETAR_TAXA_SERVICO = 64563;
  SCO_COLETAR_VALOR = 64564;
  SCO_COLETAR_REALIZA_SAQUE = 64565;
  SCO_COLETAR_SIMULACAO_SAQUE = 64566;
  SCO_COLETAR_SALDO_OU_EXTRATO = 64567;
  SCO_COLETAR_EXTRATO_RESUM_OU_SEGVIA = 64568;
  SCO_COLETAR_CONSULTA_INVEST_OU_RESGATE = 64569;
  SCO_COLETAR_COD_AUTORIZACAO = 64573;
  SCO_COLETAR_RESGATE_AVULSO = 64576;
  SCO_COLETAR_DDMMAAAA = 64577;
  SCO_COLETAR_COD_AUT_PBMS = 64578;
  SCO_COLETAR_LISTA_MEDICAM = 64579;
  SCO_RETORNAR_LISTA_MEDICAM = 64580;
  SCO_EXIBIR_MSG = 64581;
  SCO_IMPRIMIR_CUPOM_PARCIAL = 64582;
  SCO_COLETAR_QTD_PARCELAS_ACEITA1 = 64583;
  SCO_COLETAR_COD_BARRAS = 64584;
  SCO_COLETAR_COD_CONSULTA_PBM = 64585;
  SCO_COLETAR_CRM_MEDICO = 64586;
  SCO_COLETAR_UF_CRM_MEDICO = 64587;
  SCO_COLETAR_ADERIR_SEGURO = 64588;
  SCO_COLETAR_PAGTO_POR_CARTAO = 64589;
  SCO_COLETAR_DADOS_TOKORO = 64590;
  SCO_COLETAR_PAGAR_APOS_VENCTO = 64591;
  SCO_COLETAR_USAR_SENHA = 64592;
  SCO_IMPRIMIR_CUPOM_PROMO = 64593;
  SCO_COLETAR_UTILIZA_SALDO = 64594;
  SCO_COLETAR_COD_MATERIAL = 64595;
  SCO_COLETAR_COD_PLANO = 64596;
  SCO_COLETAR_PAGTO_POR_CHEQUE = 64597;
  SCO_COLETAR_CONFIRMACAO = 64598;
  SCO_COLETAR_PAGTO_NO_ROTATIVO = 64599;
  SCO_COLETAR_CMC7 = 64600;
  SCO_COLETAR_PAGTO_POR_DINHEIRO = 64601;
  SCO_COLETAR_GRUPO_SERV = 64602;
  SCO_COLETAR_CODIGO_REDE = 64603;
  SCO_COLETAR_COD_ESTAB = 64604;
  SCO_COLETAR_NSU_HOST = 64605;
  SCO_COLETAR_ddmmaaaa_EXT = 64606;
  SCO_COLETAR_E_CONSULTA = 64607;
  SCO_COLETAR_CONT_APOS_VERIF_CTA = 64608;
  SCO_COLETAR_COD_BANDEIRA = 64609;
  SCO_COLETAR_CONTA_OU_FATURA = 64610;
  SCO_COLETAR_VALOR_TOTAL = 64611;
  SCO_COLETAR_RG = 64612;
  SCO_COLETAR_RETENTAR = 64613;
  SCO_COLETAR_CPF_NO_HOST = 64614;
  SCO_COLETAR_ENDERECO = 64615;
  SCO_COLETAR_ANDAR = 64616;
  SCO_COLETAR_CONJUNTO = 64617;
  SCO_COLETAR_BLOCO = 64618;
  SCO_COLETAR_BAIRRO = 64619;
  SCO_COLETAR_AUTORIZ_OU_CARTAO = 64620;
  SCO_COLETAR_EMISSAO_CARTAO = 64621;
  SCO_COLETAR_PLANO_INFOCARDS = 64622;
  SCO_COLETAR_NUM_CUPOM_FISCAL = 64623;
  SCO_COLETAR_COD_OPERADORA_RECARGA = 64624;
  SCO_COLETAR_DADOS_SAB = 64625;
  SCO_COLETAR_FONE_COM_DIGITO = 64626;
  SCO_COLETAR_DADOS_SAB_FORCADO = 64627;
  SCO_COLETAR_TIPO_SERVIC_TEC = 64628;
  SCO_COLETAR_NUM_OS = 64629;
  SCO_COLETAR_ID_TECNICO = 64630;
  SCO_COLETAR_COD_OCORRENCIA = 64631;
  SCO_COLETAR_EPS_CREDENC = 64632;
  SCO_COLETAR_VALOR_PRIMEIRA_PARC = 64635;
  SCO_COLETAR_DADOS_ADICION = 64636;
  SCO_COLETAR_CANCELAR = 64637;
  SCO_GO_ONCHIP = 64638;
  SCO_RETIRAR_CARTAO = 64639;
  SCO_COLETAR_TAXA_EMBARQUE = 64640;
  SCO_EXIBIR_MSG_SALDO = 64641;
  SCO_OBTER_SERVICOS = 64644;
  SCO_COLETAR_CARTAO_DIGITADO = 64645;
  SCO_COLETAR_COD_PRODUTO = 64646;
  SCO_EXBIR_MENU = 64647;
  SCO_COLETAR_INSS_OU_CHEQUE = 64648;
  SCO_COLETAR_CONTRATO = 64649;
  SCO_COLETAR_DATA_CARTAO = 64650;
  SCO_EXIBIR_VALOR_VALE_GAS = 64651;
  SCO_COLETAR_DDMMAA2 = 64652;
  SCO_COLETAR_NSU_ORIGINAL = 64653;
  SCO_COLETAR_VIA_IMPRESAO = 64655;
  SCO_COLETAR_DDD_PINPAD = 64656;
  SCO_COLETAR_FONE_PINPAD = 64657;
  SCO_COLETAR_FONE_COM_DIGITO_PINPAD = 64658;
  SCO_COLETAR_REDIGITACAO_PINPAD = 64659;
  SCO_TRANSACAO_APROVADA_PARCIAL = 64660;
  SCO_COLETAR_VALOR_PARCELAS = 64661;
  SCO_COLETAR_PARC1_30_OU_60 = 64662;
  SCO_COLETAR_NUM_CARTAO = 64666;
  SCO_COLETAR_COD_SERV_POSTO_BANRISUL = 64667;
  SCO_COLETAR_MATRICULA_POSTO_BANRISUL = 64668;
  SCO_COLETAR_QTD_POSTO_BANRISUL = 64669;
  SCO_COLETAR_HODOMETRO_POSTO_BANRISUL = 64670;
  SCO_COLETAR_PLACA_VEICULO_POSTO_BANRISUL = 64671;
  SCO_COLETAR_NUM_RESGATE_PREMIO = 64673;
  SCO_OPCAO_RESGATE_PREMIO = 64674;
  SCO_COLETAR_CONFIRM_RESGATE = 64675;
  SCO_COLETAR_NUM_RESGATE = 64676;
  SCO_COLETAR_NUM_VOUCHER = 64677;
  SCO_COLETAR_DARF_OU_GPS = 64678;

  SCO_COLETAR_COD_IDENT_FATURA = 64735;
  SCO_COLETAR_CONSULTA_PLANOS = 64754;
  SCO_COLETAR_DADOS_ESPECIAL = 64756;
  SCO_COLTAR_CPF_PINPAD = 64758;
  SCO_COLETAR_PARCELA_GRATIS = 64761;
  SCO_COLETA_CARTAO_EM_ANDAMENTO = 64764;
  SCO_COLETA_EM_ANDAMENTO = 64765;
  SCO_MOSTRA_INFO_RET_SCOPE = 64766;  //SCO_MOSTRAR_INFO_RETORNAR_FLUXO??? mostra informações e retorna para scope
  SCO_MOSTRA_INFO_AGUARDA_CONF = 64767;  //SCO_MOSTRAR_INFO_AGUARDAR_CONFIRM???? mostra informações e retorna para scope

  SCO_ESTADO_COLETA_INICIAL = SCO_COLETAR_CARTAO; // SCO_PRIMEIRO_COLETA_DADOS???
  SCO_ESTADO_COLETA_FINAL = SCO_MOSTRA_INFO_AGUARDA_CONF; //SCO_ULTIMO_COLETA_DADOS???

  SCO_EXISTE_TRN_SUSPENSA:          LongInt = $FE03;  // existe transação suspensa
  SCO_NAO_EXISTE_TRN_SUSPENSA:      LongInt = $FE04;  // não existe transação suspensa
  SCO_API_NAO_FEZ_TRN:              LongInt = $FE05;  //
  SCO_POS_JA_LOGADO:                LongInt = $FE06;  // Logon duplicado
  SCO_POS_NAO_CADASTRADO:           LongInt = $FE08;  // Codigo POS não cadastrado no BD

  SCO_SERVER_OFF:                   LongInt = $FF00;
  SCO_INSTITUICAO_OFF:              LongInt = $FF01;
  SCO_CANCELADA_PELO_OPERADOR:      LongInt = $FF02;
  SCO_BIN_SERV_INV:                 LongInt = $FF03; // BIN não configurado
  SCO_TRN_JA_CANCELADA:             LongInt = $FF04;
  SCO_TRN_NOT_FOUND_BD:             LongInt = $FF05;
  SCO_TRN_NAO_REVERSIVEL:           LongInt = $FF06; // transação não pode ser cancelada
  SCO_PARMS_INCOMPATIVEIS:          LongInt = $FF07; // Dados não conferem com a transação original
  SCO_ERRO_BD:                      LongInt = $FF08;
  SCO_TIMEOUT_BD:                   LongInt = $FF09;
  SCO_BD_OFFLINE:                   LongInt = $FF0A;
  SCO_ABORTADA_PELO_APLICATIVO:     LongInt = $FF0B;
  SCO_TRN_NAO_IMPLEMENTADA:         LongInt = $FF0C;
  SCO_HANDLE_INVALIDO:              LongInt = $FF0D;
  SCO_TX_SERV_INVALIDA:             LongInt = $FF0E;
  SCO_TX_SERV_EXCEDE_LIM:           LongInt = $FF0F;
  SCO_DADO_INVALIDO:                LongInt = $FF10;
  SCO_NAO_EXITE_CUPOM_VALIDO:       LongInt = $FF11;
  SCO_AREA_RESERVADA_INSUFICIENTE:  LongInt = $FF12;
  SCO_ERRO_GENERICO:                LongInt = $FFFF;

  {- Define os parametros para a funcao ScopeObtemHandle -}
  HDL_TRANSACAO_ANTERIOR:           LongInt = $0000;
  HDL_TRANSACAO_EM_ARQUIVO:         LongInt = $0008;
  HDL_TRANSACAO_EM_ANDAMENTO:       LongInt = $0009;

  {- codigos das bandeiras -}
  SCO_SCOPE:                        LongInt = $0000;
  SCO_VISA:                         LongInt = $0001;
  SCO_MASTERCARD:                   LongInt = $0002;
  SCO_AMEX:                         LongInt = $0003;
  SCO_FININCARD:                    LongInt = $0004;
  SCO_DINERS:                       LongInt = $0005;
  SCO_SOLO:                         LongInt = $0006;
  SCO_CHEQUE_ELETRONICO:            LongInt = $0007;
  SCO_REDESHOP:                     LongInt = $0008;
  SCO_ITAU:                         LongInt = $0009;
  SCO_BRADESCO:                     LongInt = $000A;
  SCO_TRISHOP_ITAU:                 LongInt = $000B;
  SCO_SERASA:                       LongInt = $000C;
  SCO_TELECHEQUE:                   LongInt = $000D;
  SCO_CREDICARD:                    LongInt = $000E;
  SCO_RVA:                          LongInt = $000F;
  SCO_TICKET:                       LongInt = $0010;
  SCO_HIPERCARD:                    LongInt = $0011;
  SCO_CNS:                          LongInt = $0012;
  SCO_CSS:                          LongInt = $0013;
  SCO_BANRISUL:                     LongInt = $0014;
  SCO_ELECTRON:                     LongInt = $0015;
  SCO_REDECARD:                     LongInt = $0016;
  SCO_JBC:                          LongInt = $0017;
  SCO_QUALITY_CARD:                 LongInt = $0018;
  SCO_UNNISA:                       LongInt = $0019;
  SCO_FININVEST:                    LongInt = $001A;

  { Bits para indicacao do estado da comunicacao c/
    o SCOPE - possiveis vals da var iSinc }
  INI_COMUNIC:                      LongInt = $0001;
  INI_SESSAO:                       LongInt = $0002;
  INI_APLCOLET:                     LongInt = $0004;

//E?
//  ACAO_PROX:                        LongInt = $0008;
//  ACAO_ANTER:                       LongInt = $0010;
//  ACAO_CANCELAR:                    LongInt = $0020;

//E
// Nomes? 
  ACAO_COLETA_PROXIMO = 0;
  ACAO_COLETA_ANTERIOR = 1;
  ACAO_COLETA_CANCELAR = 2;
  ACAO_COLETA_ERRO = 2;

  BIT6_ON:                          LongInt = $0040;
  BIT7_ON:                          LongInt = $0080;
  BIT8_ON:                          LongInt = $0100;
  BIT9_ON:                          LongInt = $0200;

{--------------------------------------------------------------------------------------------
   Constantes de uso interno, não retornadas por Mask
--------------------------------------------------------------------------------------------}
const
  CUPOM_LOJA     = $00000001;  { CupomLoja }
  CUPOM_CLIENTE  = $00000002;  { CupomCliente }
  CUPOM_REDUZIDO = $00000004;  { CupomReduzido }


{--------------------------------------------------------------------------------------------
   Constantes da máscara 1 da função ScopeObtemCampoExt2
--------------------------------------------------------------------------------------------}
const
  Numero_Conta_PAN                   = $00000001;  { Personal Account Number (Card number) }
  Valor_transacao                    = $00000002;  { Amount }
  NSU_transacao                      = $00000004;  { Transaction Id assigned by Scope }
  Hora_local_transacao               = $00000008;  { Transaction time }
  Data_local_transacao               = $00000010;  { Transaction date }
  Data_vencimento_cartao             = $00000020;  { Card due date }
  Data_referencia                    = $00000040;  { Account date }
  Numero_cheque                      = $00000080;  { Check number }
  Codigo_autorizacao                 = $00000100;  { Authorization code }
  Codigo_resposta                    = $00000200;  { Action code }
  Identificacao_terminal             = $00000400;  { POS Id }
  Codigo_Origem_Mensagem             = $00000800;  { Store Id assigned by the acquirer at agreement time }
  Plano_Pagamento                    = $00001000;  { Number of parcels }
  Valor_Taxa_Servico                 = $00002000;  { Tip value }
  NSU_Host                           = $00004000;  { Transaction Id assigned by Acquirer }
  Cod_Banco                          = $00008000;  { Bank code }
  Cod_Agencia                        = $00010000;  { Branch code }
  Data_Vencimento                    = $00020000;  { Due date (99ddmmyyyy [...]) }
  Cod_Bandeira                       = $00040000;  { Acquirer Code }
  Cod_Servico                        = $00080000;  { Service Code }
  Texto_BIT_62                       = $00100000;  { BIT 62 }
  Controle_Dac                       = $00200000;  { Control DAC }
  Cod_Rede                           = $00400000;  { Net Code }
  Nome_Bandeira                      = $00800000;  { Acquirer Name }
  Nome_Rede                          = $01000000;  { Net Name }
  Cartao_Trilha02                    = $02000000;  { Card - Track 02 }
  Numero_Promissorias                = $04000000;  { Number of promissory note }
  Cod_Estab_Impresso                 = $08000000;  { Establishment code printed on ticket }
  Numero_CMC7                        = $10000000;  { CMC7 Number }
  CGC_Convenio                       = $20000000;  { CGC Number }    // Modo_Pagamento
  Msg_Autentic_Cheque                = $40000000;  { Check Autentic Message }
  Saldo_Disponivel                   = $80000000;  { Available Cache }

{--------------------------------------------------------------------------------------------
   Constantes da máscara 2 da função ScopeObtemCampoExt2
--------------------------------------------------------------------------------------------}
const
  NSU_transacao_Original             = $00000001;  { Cancel Transaction Id assigned by Scope }
  Cliente_Com_Seguro                 = $00000002;  { Ensured Client }
  Dados_Parcelado_Cetelem            = $00000004;  { Informations about parcels Cetelem }
  Data_Movimento                     = $00000008;  { Interchange: Data Movimento }
  Nome_Convenio                      = $00000010;  { Interchange: Nome da Empresa de Convênio }
  Lista_TEF_Permitidas               = $00000020;  { Interchange: Lista das formas de pagamento em TEF permitidas }
  Linha_Autenticacao                 = $00000040;  { Interchange - Fininvest: Linha de autenticação }
  Dados_Consulta_Fatura              = $00000080;  { Interchange - Fininvest: Dados da Consulta Fatura }
  Forma_Financiamento                = $00000100;  { Type of Financing }
  Codigo_Resposta_AVS                = $00000200;  { Return Code for AVS }
  Pontos_AdquiridosOuResgatados      = $00000400;  { Pontos adquiridos ou resgatados }
  Fator_Compra                       = $00000800;  { Fator de compra }
  NSU_Host_Transacao_Original        = $00001000;  { NSU Host da transação original (cancelamento) }
  Identificacao_Cliente_PBM          = $00002000;  { Identificação do Cliente junto a Autorizadora }
  Cod_Operadora                      = $00004000;  { Código da Operadora de Celular }
  Cod_Local_Telefone                 = $00008000;  { DDD }
  Num_Telefone                       = $00010000;  { Telefone }
  Dados_ValeGas                      = $00020000;  { ULTRAGAZ: Dados do ValeGás }
  Codigo_IF                          = $00040000;  { Código IF (Instituição Financeira) }
  Numero_Item                        = $00080000;  { Fininvest, Cetelem }
  Num_Contrato                       = Numero_Item; { IBI: Numero do contrato (CPCHEQUE/INSS) }
  Valor_Taxa_Embarque                = $00100000;  { Taxa de embarque }
  Digitos_TR2SON                     = $00200000;  { Uso exclusivo sonae }
  Taxa_Cliente_Lojista               = $00400000;  { Informação bit 124 - CDC Orbitall }
  Cod_Servico_Original               = $00800000;  { Transação de cancelamento: Código de Serviço da transação original }
  Cod_Barras                         = $01000000;  { Código de Barras }
  Permite_Desfazimento               = $02000000;  { Permite cancelamento }
  Logo_PAN                           = $04000000;  { Retorna o LOGO do cartão: bytes 7 e 8 do PAN }
  Cod_Empresa                        = $08000000;  { Código da Empresa - HSBC }
  Cod_Autenticacao                   = $10000000;  { Código de autenticação - ISOGCB }
  Dados_Pagto_ISOGCB                 = $20000000;  { Dados do pagamento ISOGCB }
  UsoRes_63                          = $40000000;  { BIT 63 - Projeto Vale Gás GetNet }
  Numero_PDV                         = $80000000;  { Número do PDV - HSBC }

{--------------------------------------------------------------------------------------------
   Constantes da máscara 3 da função ScopeObtemCampoExt2
--------------------------------------------------------------------------------------------}
const
  DadosQtdeECupons                   = $00000001;  { Informações sobre a quantidade e os e-cupons disponíveis ao cliente }
  DescResgateMonetario               = $00000002;  { Desconto do resgate monetário }
  Dados_Pagto_Bit48_BRADESCO         = $00000004;  { Bradesco - Informações sobre o Bit 48 }
  Modo_Entrada                       = $00000008;  { Modo de entrada da transação (Entry Mode) }
  Valor_Saque                        = $00000010;  { Valor do Saque }
  Resposta_Consulta_Infocards        = $00000020;  { Resposta da consulta Infocards (bit 62 da 0110) }
  Dados_Resposta_Consulta_EPAY       = $00000040;  { Dados da resposta de Consulta da EPAY. Os dados retornados
                                                      consistem em 3 valores de concatenados:
                                                      1. Valor Mínimo ( 12 dígitos )
                                                      2. Valor Máximo ( 12 dígitos )
                                                      3. Saldo Disponível ( 12 dígitos )}
  Dados_Resposta_Consulta_INCOMM     = $00000040;  { Dados da resposta de Consulta valor Gift Card (INCOMM)
                                                      Os dados retornados consistem em 3 valores de concatenados:
                                                      1. Valor Mínimo ( 12 dígitos )
                                                      2. Valor Máximo ( 12 dígitos )
                                                      3. Saldo Disponível ( 12 dígitos )}
  Max_Mercadorias_TicketCar          = $00000100;  { Máximo de mercadorias permitidas para uma transação (Cartão TicketCar, Valecard, entre outras)
                                                     O dado retornado é um campo de 2 dígitos.}
  Codigo_SAT                         = $00000200;  { Código SAT (ver códigos em Código das redes)}
  Versao_Carga_Tabelas_Host          = $00000400;  { Versão corrente de Carga de Tabelas do Host Formato: 10 dígitos
                                                    (Preenchido com zeros a esquerda, caso necessário). Disponível em
                                                    transações com as seguintes Redes: SAVS}
  CNPJ_Rede_Credenciadora_SAT        = $00002000;  { CNPJ da rede credenciadora - SAT }
  Dados_Correspondente_Bancario      = $00008000;  { Dados do Correspondente Bancário }
  Dados_Adicionais_Gift_Card         = $00010000;  { Dados Adicionais Gift Card:
                                                      - Para Incomm: código UPC11
                                                      - Para BlackHawk: código EAN12
                                                      - Para EPAY: código do produto com 8 dígitos, com brancos à direita se menor }
  Dados_Operacao_Fidelidade_SGF      = $00020000;  { Dados retornados da Operação Fidelidade (SGF) }
  Valor_Total_Pagamento              = $00040000;  { Valor Total do Pagamento }
  Valor_Descontos_Pagamento          = $00080000;  { Valor de Descontos do Pagamento }
  Valor_Entrada_IATA                 = $00800000;  { Valor de Entrada (IATA) }
  Valor_Acrescimos_Pagamento         = $00100000;  { Valor de Acréscimos do Pagamento }
  Dados_Perfil_Pagamento_Recorrente  = $01000000;  { Dados do Perfil de Pagamento Recorrente }
  Dados_Assinatura_Pagamento         = $02000000;  { Dados da Assinatura do Pagamento Recorrente }
  Dados_Consulta_BACEN               = $04000000;  { Dados Consulta BACEN – para títulos registrados }
  Valor_Documento                    = $08000000;  { Valor Documento }
  Resposta_Consulta_BACEN_Comprovante = $10000000; { Resposta Consulta BACEN – comprovante }
  Modo_Pagamento                     = $20000000;  { Modo Pagamento:
                                                      ‘00’ – não informado
                                                      '01' – cheque (Utilizado para pagamento de contas)
                                                      '02' – dinheiro (Utilizado para pagamento de contas)
                                                      '03' – debito em conta (Utilizado para carteiras virtuais/pagamento de contas)
                                                      '04' – cartao credito (Utilizado para carteiras virtuais)
                                                      '05' – pix (Utilizado para carteiras virtuais)
                                                      ‘06’ – cartao de debito (Utilizado para carteiras virtuais)
                                                      ‘07’ – saldo + cartao (Utilizado para carteiras virtuais) }
  Consulta_Cedente_BACEN_BRADESCO    = $40000000;  { Consulta Cedente - Dados da Consulta BACEN BRADESCO }
  Data_Vencimento_CORBAN             = $80000000;  { Data Vencimento CORBAN – do título/conta }

  {--------------------------------------------------------------------------------------------
     Constantes da máscara 4 da função ScopeObtemCampoExt3
  --------------------------------------------------------------------------------------------}
const
    Nome_Portador_Cartao           = $00000001;  { Nome do Portador do Cartão (Informação com até 26 caracteres) }
    Data_Validade_Cartao           = $00000002;  { Data de Validade do Cartão (YYMMDD) }
    Merchant_ID                    = $00000004;  { Merchant ID (informação com até 32 caracteres) }
    Codigo_Estab_Externo           = $00000008;  { Código do Estabelecimento Externo (informação com até 15 caracteres) }
    String_QRCode                  = $00000020;  { String para gerar o QRCode }
    Relacao_Descontos_Item         = $00000080;  { Relação de Descontos por Item, recebidos da Ticket Log no bit 54 da 0210 }
    Indicador_Saldo_Disponivel     = $00000100;  { Informa se o Saldo_Disponivel está em [0] = Reais (default) ou [1] = Litros }
    Numero_CPF                     = $00000200;  { Número do CPF }
    ARQC_Chip                      = $00000400;  { ARQC do chip, se disponibilizado pelo cartão }
    AID_Chip                       = $00000800;  { AID do chip, se disponibilizado pelo cartão }
    Transacao_Autorizada_Por_Senha = $00001000;  { Indicação se a transação foi autorizada mediante uso de senha pessoal [1] = Sim, [0] = Não }
    //????????                     = $00002000;  { ?????????? }
    Campo_TID_Pix                  = $00004000;  { campo TID da tabela Mensagem (do Banco de Dados) - transações pix (txid) }
    Campo_Referencia_Pix           = $00008000;  { campo Referência da tabela Mensagem (do Banco de Dados) - transações pix (end2endId) }
    Tamanho_BIN                    = $00010000;  { Tamanho do BIN }
    Estrutura_Strings_DCC          = $00020000;  { Estrutura de strings finalizadas com null:
                                                    Dado Tamanho + null Obs
                                                    Valor Convertido 12 + 1
                                                    Cotação de Conversão 8 + 1
                                                    Taxa Markup 5 + 1 %
                                                    Sigla da Moeda Estrangeira 3 + 1 ISO 4217
                                                    Código da Moeda Estrangeira 3 + 1 ISO 4217
                                                    Pode ser usada a estrutura stDadosDCC definida em scopeapi.h }
    Status_DCC                      = $00040000;  { Status DCC:
                                                    ‘0’ = Não Realizado
                                                    ‘1’ = Cliente Não Aceitou
                                                    ‘2’ = Cliente Aceitou
                                                    ‘3’ = Não Elegível
                                                    ‘4’ = Erro de Comunicação
                                                    Qualquer outro valor = Desconhecido }



//------------------------------------------------------------------------------
// DECLARACAO DAS ESTRUTURAS
//------------------------------------------------------------------------------
type
  //** Enumerador dos tipos das operadoras de celular */
  TEnumCelOperModelo = (
        REC_CEL_OPERADORAS_MODELO_1 = 1,
        REC_CEL_OPERADORAS_MODELO_2);


  { Enumerador dos tipos de estruturas retornadas
        para os valores de recarga }
  TEnumCelOperVals = (
        REC_CEL_VALORES_MODELO_1 = 1,
        REC_CEL_VALORES_MODELO_2,
        REC_CEL_VALORES_MODELO_3);


  //** dados utlizados na coleta de parametros */
  PParam_Coleta = ^TParam_Coleta;

  TParam_Coleta = packed record
    Bandeira: Word;
    FormatoDado: Word;
    HabTeclas: Word;
    MsgOp1: array [1..64]  of AnsiChar;
    MsgOp2: array [1..64]  of AnsiChar;
    MsgCl1: array [1..64]  of AnsiChar;
    MsgCl2: array [1..64]  of AnsiChar;
    WrkKey: array [1..17]  of AnsiChar;
    PosMasterKey: Word;
    PAN: array [1..20]  of AnsiChar;
    UsaCriptoPinpad: Byte;
    IdModoPagto: Byte;
    AceitaCartaoDigitado: Byte;
    Reservado: array [1..105] of AnsiChar;
  end;


  //** Estrutura devolvida pela funcao ScopeGetLastMsg() */
  PColeta_Msg = ^TColeta_Msg;

  TColeta_Msg = packed record
    Op1: array [1..64] of AnsiChar;
    Op2: array [1..64] of AnsiChar;
    Cl1: array [1..64] of AnsiChar;
    Cl2: array [1..64] of AnsiChar;
  end;


  //** Estrutura devolvida pela funcao ScopeGetCheque() */
  PParam_Cheq = ^TParam_Cheq;

  TParam_Cheq = packed record
    Banco:     array [1..04] of AnsiChar;
    Agencia:   array [1..05] of AnsiChar;
    NumCheque: array [1..13] of AnsiChar;
    Valor:     array [1..13] of AnsiChar;
    BomPara:   array [1..09] of AnsiChar;
    CodAut:    array [1..11] of AnsiChar;
    Municipio: array [1..41] of AnsiChar;
    Ordem:     SmallInt;
  end;


  //** Lista de Operadoras de Recarga de Celular retornadas pelo Servidor */
  PRec_Cel_Oper = ^TRec_Cel_Oper;

  TRec_Cel_Oper = packed record
    NumOperCel: SmallInt;
    OperCel:    array [1..2000] of AnsiChar;
  end;


  //** Lista de Operadoras de Recarga de Celular retornadas pelo Servidor */
  PRec_Cel_ID_Oper = ^TRec_Cel_ID_Oper;

  TRec_Cel_ID_Oper = packed record
    CodOperCel:  AnsiChar;
    NomeOperCel: array [1..21] of AnsiChar;
  end;


  //** Formato do valor para Recarga de Celular */
  PRec_Cel_Valor = ^TRec_Cel_Valor;

  TRec_Cel_Valor = packed record
    Valor: array [1..12] of AnsiChar;
    Bonus: array [1..12] of AnsiChar;
    Custo: array [1..12] of AnsiChar;
  end;

  TRec_Cel_Faixa_Valores = packed record
    ValorMin: array [1..12] of AnsiChar;
    ValorMax: array [1..12] of AnsiChar;
  end;


  //** Lista de Valores de Recarga de Celular retornadas pelo Servidor */
  PRec_Cel_Valores = ^TRec_Cel_Valores;

  TRec_Cel_Valores = packed record
    TipoValor:      AnsiChar;               { Tipo dos valores
                                              'V' - variavel(val min e val maximo)
                                              'F' - Fixo (apenas um valor fixo)
                                              'T' - Todos (tabela de valores) }
    ValorMinimo:    array [1..12] of AnsiChar;
    ValorMaximo:    array [1..12] of AnsiChar;
    Totvalor:       AnsiChar;
    TabValores:     array [1..10] of TRec_Cel_Valor;
    MsgPromocional: array [1..41] of AnsiChar;
    TotFaixaValores: AnsiChar;
    TabFaixaValores:array [1..10] of TRec_Cel_Faixa_Valores;
  end;

type
  EACBrTEFScopeAPI = class(Exception);

  TACBrTEFScopeOperacao = ( scoCredito, scoDebito, scoPagto, scoConsCDC, scoCheque, scoCanc,
                    scoReimpComp, scoResVenda, scoRecargaCel, scoPreAutCredito );

  TACBrTEFScopeGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TACBrTEFScopeTerminalMensagem = (tmOperador, tmCliente);

  TACBrTEFScopeExibeMensagem = procedure(
    Mensagem: String;
    Terminal: TACBrTEFScopeTerminalMensagem;
    MilissegundosExibicao: Integer  // 0 - Para com OK; Positivo - aguarda Ok ou N milissegundos; Negativo - Apenas exibe a Msg (não aguarda)
    ) of object;

  TACBrTEFScopeExibeMenu = procedure(
    Titulo: String;
    Opcoes: TStringList;
    var ItemSelecionado: Integer) of object ;  // -1 = Cancelado

  TACBrTEFScopeEstadoOperacao = ( scoestFluxoAPI,
                                  scoestAguardaUsuario,
                                  scoestPinPad,
                                  scoestPinPadLerCartao,
                                  scoestPinPadDigitacao,
                                  scoestRemoveCartao,
                                  scoestLeituraQRCode );

  TACBrTEFScopeTransacaoEmAndamento = procedure(
    EstadoOperacao: TACBrTEFScopeEstadoOperacao; out Cancelar: Boolean) of object;

  { TACBrTEFScopeAPI }

  TACBrTEFScopeAPI = Class
  private
    fCarregada: Boolean;
    fConectado: Boolean;
    fConfirmarTransacoesPendentes: Boolean;
    fControleConexao: Boolean;
    fCupomReduzido: Boolean;
    fDiretorioTrabalho: String;
    fEmpresa: String;
    fEmTransacao: Boolean;
    fEnderecoIP: String;
    fFilial: String;
    fInicializada: Boolean;
    fMsgPinPad: String;
    fDadosTransacao: TStringList;
    fOnExibeMensagem: TACBrTEFScopeExibeMensagem;
    fOnExibeMenu: TACBrTEFScopeExibeMenu;
    fOnGravarLog: TACBrTEFScopeGravarLog;
    fOnTransacaoEmAndamento: TACBrTEFScopeTransacaoEmAndamento;
    fPathLib: String;
    fPDV: String;
    fPermitirCancelarOperacaoPinPad: Boolean;
    fPermitirCartaoDigitado: Boolean;
    fPermitirSaque: Boolean;
    fPinPadSeguro: Boolean;
    fPortaPinPad: String;
    fPortaTCP: String;
    fSessaoAberta: Boolean;
    fVersaoAutomacao: String;

    // Funcoes originais do SCOPE
    xScopeOpen: function(Modo, Empresa, Filial, Pdv: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeClose: function(): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeVersao: function(_Versao: PAnsiChar; _TamBufVersao: LongInt): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeCompraCartaoCredito: function (Valor, TxServico: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeAbreSessaoTEF: function(): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeSetAplColeta: function(): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeStatus: function(): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeGetParam: function (_TipoParam: LongInt; _lpParam: PParam_Coleta): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeResumeParam: function(_CodTipoColeta: LongInt; _Dados: PAnsiChar;
      _DadosParam: Word; _Acao: LongInt): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeGetLastMsg: function(_ptParamColetaMsg: PColeta_Msg): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeGetCheque: function (_ptParamCheque: PParam_Cheq): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeGetCupomEx: function(_CabecLen: Word; _Cabec: PAnsiChar;
      _CupomClienteLen: Word; _CupomCliente: PAnsiChar; _CupomLojaLen: Word;
      _CupomLoja: PAnsiChar; _CupomReduzLen: Word; _CupomReduz: PAnsiChar;
      _NroLinhasReduz: PByte): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeFechaSessaoTEF: function(_Acao: Byte; _DesfezTEFAposQuedaEnergia: PByte): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeConsultaCDC: function(_Valor, _TxServico: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeCompraCartaoDebito: function(Valor: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeConsultaCheque: function(Valor: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeCancelamento: function(_Valor, _TxServico: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeReimpressaoComprovante: function(): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeResumoVendas: function(): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeObtemCampoExt: function(_Handle, _Masc, _Masc2: LongInt;
      _FieldSeparator: Byte; _Buffer: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeObtemCampoExt2: function(_Handle, _Masc, _Masc2, _Masc3: LongInt;
      _FieldSeparator: Byte; _Buffer: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeObtemCampoExt3: function(_Handle, _Masc, _Masc2, _Masc3, _Masc4: LongInt;
      _FieldSeparator: Byte; _Buffer: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeObtemHandle: function(_Desloc: LongInt): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePagamento: function(_Servico, _CodBandeira: Word): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeRecargaCelular: function(): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePreAutorizacaoCredito: function(_Valor, _TxServico: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeRecuperaOperadorasRecCel: function(_TipoTabela: Byte; _Buffer: PAnsiChar;
      _TamBuffer: Word): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeRecuperaValoresRecCel: function(_TipoTabela: Byte; _Buffer: PAnsiChar;
      _TamBuffer: Word): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeConfigura: function(_Id, _Param: LongInt): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeValidaInterfacePP: function(IntPP: Byte): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //Busca informações de configurações no servidor deles...
    xScopeConsultaPP: function(Configurado, UsoExclusivoScope, Porta: PByte): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xScopePPOpenSecure: function(TipoCanal: word; Endereco: PAnsiChar): LongInt
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPClose: function(IdleMsg: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPGetCOMPort: function(szComEndereco: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
//E
  //Implementar??
    //xScopePPGetInfoEx
    //ScopePPStartGetData
    //ScopePPGetData
    //ScopePPStartOptionMenu
    //ScopePPOptionMenu]
    //ScopePPGetOperationMode
    xScopePPDisplay: function(Msg: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //ScopePPDisplayEx
  //Descontinuadas (pág. 142)
    xScopePPOpen: function(Porta: Word): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //xScopePPGetInfo
    //xScopePPStartGetPIN
  //.... Descontinuadas

    procedure SetPathLib(const AValue: String);
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetEmpresa(const AValue: String);
    procedure SetFilial(const AValue: String);
    procedure SetPDV(const AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetControleConexao(AValue: Boolean);
    procedure SetEnderecoIP(AValue: String);
    procedure SetPortaTCP(AValue: String);

    procedure SetEmTransacao(AValue: Boolean);
    procedure ChamarTransacaoEmAndamento(EstadoOperacao: TACBrTEFScopeEstadoOperacao;
      out Cancelar: Boolean);

  protected
    function GetLibFullPath: String;
    function GetScopeIniFullPath: String;

    procedure LoadLibFunctions;
    procedure UnLoadLibFunctions;
    procedure ClearMethodPointers;

    procedure DoException(const AErrorMsg: String );

    procedure TratarErroScope(AErrorCode: LongInt);

    procedure AbrirComunicacaoScope;
    procedure FecharComunicacaoScope;
    procedure VerificarSeEstaConectadoScope;
    procedure VerificarSeMantemConexaoScope;

    procedure VerificaSessaoTEFAnterior;

    procedure AbrirPinPad;
    procedure ConfigurarColeta;
    function ConfigurarScope(AId: LongInt; Ligado: Boolean): Boolean;
    procedure FecharPinPad;

    procedure VerificarDiretorioDeTrabalho;
    procedure VerificarEAjustarScopeINI;
    function ConfigurarPortaPinPad(const APortaPinPad: String): Word;
    procedure ObterDadosScopeINI(out AEmpresa: String; out AFilial: String;
      out AEnderecoIP: String; out APortaTCP: String);

    procedure ExibirMensagem(const AMsg: String;
      Terminal: TACBrTEFScopeTerminalMensagem = tmOperador; TempoEspera: Integer = -1);

    function ObterScopeStatus: Longint;
    function ObterDadosCupom: Longint;
    procedure ObterDadosDaTransacao;

  public
    constructor Create;
    destructor Destroy; override;

    property PathLib: String read fPathLib write SetPathLib;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
    property ControleConexao: Boolean read fControleConexao write SetControleConexao default True;

    property Empresa: String read fEmpresa write SetEmpresa;
    property Filial: String read fFilial write SetFilial;
    property PDV: String read fPDV write SetPDV;
    property EnderecoIP: String  read fEnderecoIP write SetEnderecoIP;
    property PortaTCP: String read fPortaTCP write SetPortaTCP;

    property PortaPinPad: String read fPortaPinPad write fPortaPinPad;
    property MsgPinPad: String read fMsgPinPad write fMsgPinPad;
    property VersaoAutomacao: String read fVersaoAutomacao write fVersaoAutomacao;
    property PinPadSeguro: Boolean read fPinPadSeguro write fPinPadSeguro default True;
    property CupomReduzido: Boolean read fCupomReduzido write fCupomReduzido default False;
    property PermitirCartaoDigitado: Boolean read fPermitirCartaoDigitado
      write fPermitirCartaoDigitado default False;
    property PermitirCancelarOperacaoPinPad: Boolean read fPermitirCancelarOperacaoPinPad
      write fPermitirCancelarOperacaoPinPad default True;
    property PermitirSaque: Boolean read fPermitirSaque write fPermitirSaque default True;
    property ConfirmarTransacoesPendentes: Boolean read fConfirmarTransacoesPendentes
      write fConfirmarTransacoesPendentes default True;

    property Carregada: Boolean read fCarregada;
    property Inicializada: Boolean read fInicializada write SetInicializada;
    property Conectado: Boolean read fConectado;
    property SessaoAberta: Boolean read fSessaoAberta;
    property EmTransacao: Boolean read fEmTransacao;

    property OnExibeMensagem: TACBrTEFScopeExibeMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    property OnExibeMenu: TACBrTEFScopeExibeMenu read fOnExibeMenu
      write fOnExibeMenu;
    property OnTransacaoEmAndamento: TACBrTEFScopeTransacaoEmAndamento read fOnTransacaoEmAndamento
      write fOnTransacaoEmAndamento;

    property OnGravarLog: TACBrTEFScopeGravarLog read fOnGravarLog write fOnGravarLog;

    procedure Inicializar;
    procedure DesInicializar;

    procedure AbrirSessaoTEF;
    procedure FecharSessaoTEF(Confirmar: Boolean; out TransacaoFoiDesfeita: Boolean);
    procedure IniciarTransacao(Operacao: TACBrTEFScopeOperacao; const Param1,
      Param2, Param3: String);
    procedure ExecutarTransacao;
    function EnviarParametroTransacao(Acao: LongInt; codTipoColeta: LongInt = -1; Dados: AnsiString = '';
      dadosParam: Word = 0): LongInt;
    procedure AbortarTransacao;

    function ObterVersaoScope: String;
    function AcharPortaPinPad: String;
    procedure ExibirMensagemPinPad(const MsgPinPad: String);

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);
  end;

implementation

uses
  IniFiles, StrUtils, TypInfo,
  ACBrUtil.Strings, ACBrUtil.Math, ACBrUtil.FilesIO;

{ TACBrTEFScopeAPI }

constructor TACBrTEFScopeAPI.Create;
begin
  inherited;
  fCarregada := False;
  fInicializada := False;
  fConectado := False;
  fSessaoAberta := False;
  fControleConexao := True;
  fPathLib := '';
  fDiretorioTrabalho := '';
  fEnderecoIP := '';
  fPortaTCP := '';
  fMsgPinPad := '';
  fVersaoAutomacao := '';
  fPinPadSeguro := True;
  fPortaPinPad := '';
  fCupomReduzido := False;
  fPermitirCartaoDigitado := False;
  fPermitirCancelarOperacaoPinPad := True;
  fPermitirSaque := True;
  fConfirmarTransacoesPendentes := True;
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnExibeMenu := Nil;
  fOnTransacaoEmAndamento := Nil;
  fDadosTransacao := TStringList.Create;
end;

destructor TACBrTEFScopeAPI.Destroy;
begin
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  DesInicializar;
  fDadosTransacao.Free;
  inherited Destroy;
end;

procedure TACBrTEFScopeAPI.Inicializar;
begin
  if fInicializada then
    Exit;

  GravarLog('TACBrTEFScopeAPI.Inicializar');

  //if not Assigned(fOnExibeMenu) then
    //DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnExibeMenu']));
  if not Assigned(fOnExibeMensagem) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnExibeMensagem']));

  VerificarDiretorioDeTrabalho;
  //E: Se o arquivo INI estiver configurado, a chamada abaixo talvez possa ser dispensada.
  //Ver também função ScopeOpenVerify() como possível adição para essa feature.
  VerificarEAjustarScopeINI;
  LoadLibFunctions;

  fInicializada := True;
  fConectado := False;
  fSessaoAberta := False;

  AbrirPinPad;

  if not ControleConexao then
    AbrirComunicacaoScope;
end;

procedure TACBrTEFScopeAPI.DesInicializar;
var
  b: Boolean;
begin
  if not fInicializada then
    Exit;

  GravarLog('TACBrTEFScopeAPI.DesInicializar');
  FecharSessaoTEF(True, b);
  FecharComunicacaoScope;
  FecharPinPad;

  UnLoadLibFunctions;
  fInicializada := False;
end;

procedure TACBrTEFScopeAPI.GravarLog(const AString: AnsiString; Traduz: Boolean);
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

procedure TACBrTEFScopeAPI.SetPathLib(const AValue: String);
begin
  if fPathLib = AValue then
    Exit;

  GravarLog('TACBrTEFScopeAPI.SetPathLib( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fPathLib := PathWithDelim(ExtractFilePath(AValue));
end;

procedure TACBrTEFScopeAPI.SetDiretorioTrabalho(AValue: String);
begin
  if fDiretorioTrabalho = AValue then
    Exit;

  GravarLog('TACBrTEFScopeAPI.SetDiretorioTrabalho( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fDiretorioTrabalho := AValue;
end;

procedure TACBrTEFScopeAPI.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  GravarLog('TACBrTEFScopeAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFScopeAPI.SetEmpresa(const AValue: String);
begin
  if fEmpresa = AValue then
    Exit;

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  if (AValue = '') then
    fEmpresa := AValue
  else
    fEmpresa := Format('%.4d',[StrToIntDef(AValue, 0)]);
end;

procedure TACBrTEFScopeAPI.SetFilial(const AValue: String);
begin
  if fFilial = AValue then
    Exit;

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  if (AValue = '') then
    fFilial := AValue
  else
    fFilial := Format('%.4d',[StrToIntDef(AValue, 0)]);
end;

procedure TACBrTEFScopeAPI.SetPDV(const AValue: String);
begin
  if fPDV = AValue then
    Exit;

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  if (AValue = '') then
    fPDV := AValue
  else
    fPDV := Format('%.3d',[StrToIntDef(AValue, 0)]);
end;


procedure TACBrTEFScopeAPI.SetControleConexao(AValue: Boolean);
begin
  if fControleConexao = AValue then
    Exit;

  GravarLog('TACBrTEFScopeAPI.SetControleConexao( '+BoolToStr(AValue, True)+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fControleConexao := AValue;
end;

procedure TACBrTEFScopeAPI.SetEnderecoIP(AValue: String);
begin
  if fEnderecoIP = AValue then
    Exit;

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fEnderecoIP := Trim(AValue);
end;


procedure TACBrTEFScopeAPI.SetPortaTCP(AValue: String);
begin
  if fPortaTCP = AValue then
    Exit;

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fPortaTCP := Trim(AValue);
end;

procedure TACBrTEFScopeAPI.SetEmTransacao(AValue: Boolean);
begin
  if (not fInicializada) or (AValue = fEmTransacao) then
    Exit;

  fEmTransacao := AValue;

  if not fEmTransacao then
    ExibirMensagem('');
end;

procedure TACBrTEFScopeAPI.ChamarTransacaoEmAndamento(
  EstadoOperacao: TACBrTEFScopeEstadoOperacao; out Cancelar: Boolean);
begin
  Cancelar := False;
  if not Assigned(fOnTransacaoEmAndamento) then
    Exit;

  GravarLog('  OnTransacaoEmAndamento( '+GetEnumName(TypeInfo(TACBrTEFScopeEstadoOperacao),
            integer(EstadoOperacao))+' )');
  fOnTransacaoEmAndamento(EstadoOperacao, Cancelar);
  GravarLog('    Cancelar: '+BoolToStr(Cancelar, True) );
end;

function TACBrTEFScopeAPI.GetLibFullPath: String;
begin
  if (PathLib <> '') then
  begin
    GravarLog(ACBrStr('TACBrTEFScopeAPI.LibFullName: Usando "PathLib" informado pela aplicação: ')+PathLib);
    Result := PathLib + CScopeLib
  end
  else
    Result := ApplicationPath + CScopeLib;
end;

function TACBrTEFScopeAPI.GetScopeIniFullPath: String;
var
  sLibName: String;
begin
  sLibName := GetLibFullPath;
  Result := ExtractFilePath(sLibName) + CScopeINI;
end;

procedure TACBrTEFScopeAPI.LoadLibFunctions;

  procedure ScopeFunctionDetect(LibName, FuncName: AnsiString; var LibPointer: Pointer;
    FuncIsRequired: Boolean = True) ;
  begin
    if not Assigned( LibPointer )  then
    begin
      GravarLog('   '+FuncName);
      if not FunctionDetect(LibName, FuncName, LibPointer) then
      begin
        LibPointer := NIL ;
        if FuncIsRequired then
          DoException(Format(ACBrStr('Erro ao carregar a função: %s de: %s'),[FuncName, LibName]))
        else
          GravarLog(Format(ACBrStr('     Função não requerida: %s não encontrada em: %s'),[FuncName, LibName]));
        end ;
    end ;
  end;

var
  sLibName: string;
begin
  if fCarregada then
    Exit;

  sLibName := GetLibFullPath;
  GravarLog('TACBrTEFScopeAPI.LoadDLLFunctions - '+sLibName);

  ScopeFunctionDetect(sLibName, 'ScopeOpen', @xScopeOpen);
  ScopeFunctionDetect(sLibName, 'ScopeClose', @xScopeClose);
  ScopeFunctionDetect(sLibName, 'ScopeVersao', @xScopeVersao);
  ScopeFunctionDetect(sLibName, 'ScopeCompraCartaoCredito', @xScopeCompraCartaoCredito);
  ScopeFunctionDetect(sLibName, 'ScopeAbreSessaoTEF', @xScopeAbreSessaoTEF);
  ScopeFunctionDetect(sLibName, 'ScopeSetAplColeta', @xScopeSetAplColeta);
  ScopeFunctionDetect(sLibName, 'ScopeStatus', @xScopeStatus);
  ScopeFunctionDetect(sLibName, 'ScopeGetParam', @xScopeGetParam);
  ScopeFunctionDetect(sLibName, 'ScopeResumeParam', @xScopeResumeParam);
  ScopeFunctionDetect(sLibName, 'ScopeGetLastMsg', @xScopeGetLastMsg);
  ScopeFunctionDetect(sLibName, 'ScopeGetCheque', @xScopeGetCheque);
  ScopeFunctionDetect(sLibName, 'ScopeGetCupomEx', @xScopeGetCupomEx);
  ScopeFunctionDetect(sLibName, 'ScopeFechaSessaoTEF', @xScopeFechaSessaoTEF);
  ScopeFunctionDetect(sLibName, 'ScopeConsultaCDC', @xScopeConsultaCDC);
  ScopeFunctionDetect(sLibName, 'ScopeCompraCartaoDebito', @xScopeCompraCartaoDebito);
  ScopeFunctionDetect(sLibName, 'ScopeConsultaCheque', @xScopeConsultaCheque);
  ScopeFunctionDetect(sLibName, 'ScopeCancelamento', @xScopeCancelamento);
  ScopeFunctionDetect(sLibName, 'ScopeReimpressaoComprovante', @xScopeReimpressaoComprovante);
  ScopeFunctionDetect(sLibName, 'ScopeResumoVendas', @xScopeResumoVendas);
  ScopeFunctionDetect(sLibName, 'ScopeObtemCampoExt', @xScopeObtemCampoExt);
  ScopeFunctionDetect(sLibName, 'ScopeObtemCampoExt2', @xScopeObtemCampoExt2);
  ScopeFunctionDetect(sLibName, 'ScopeObtemCampoExt3', @xScopeObtemCampoExt3);
  ScopeFunctionDetect(sLibName, 'ScopeObtemHandle', @xScopeObtemHandle);
  ScopeFunctionDetect(sLibName, 'ScopePagamento', @xScopePagamento);
  ScopeFunctionDetect(sLibName, 'ScopeRecargaCelular', @xScopeRecargaCelular);
  ScopeFunctionDetect(sLibName, 'ScopePreAutorizacaoCredito', @xScopePreAutorizacaoCredito);
  ScopeFunctionDetect(sLibName, 'ScopeRecuperaOperadorasRecCel', @xScopeRecuperaOperadorasRecCel);
  ScopeFunctionDetect(sLibName, 'ScopeRecuperaValoresRecCel', @xScopeRecuperaValoresRecCel);
  ScopeFunctionDetect(sLibName, 'ScopeConfigura', @xScopeConfigura);
  ScopeFunctionDetect(sLibName, 'ScopeValidaInterfacePP', @xScopeValidaInterfacePP);
  ScopeFunctionDetect(sLibName, 'ScopeConsultaPP', @xScopeConsultaPP);
  ScopeFunctionDetect(sLibName, 'ScopePPOpen', @xScopePPOpen);
  ScopeFunctionDetect(sLibName, 'ScopePPOpenSecure', @xScopePPOpenSecure);
  ScopeFunctionDetect(sLibName, 'ScopePPClose', @xScopePPClose);
  ScopeFunctionDetect(sLibName, 'ScopePPGetCOMPort', @xScopePPGetCOMPort);
  ScopeFunctionDetect(sLibName, 'ScopePPDisplay', @xScopePPDisplay);

  fCarregada := True;
end;

procedure TACBrTEFScopeAPI.UnLoadLibFunctions;
var
  sLibName: String;
begin
  if not fCarregada then
    Exit;

  GravarLog('TACBrTEFScopeAPI.UnLoadDLLFunctions');

  sLibName := GetLibFullPath;
  UnLoadLibrary( sLibName );
  fCarregada := False;
  ClearMethodPointers;
end;

procedure TACBrTEFScopeAPI.ClearMethodPointers;
begin
  xScopeOpen := Nil;
  xScopeClose := Nil;
  xScopeVersao := Nil;
  xScopeCompraCartaoCredito := Nil;
  xScopeAbreSessaoTEF := Nil;
  xScopeSetAplColeta := Nil;
  xScopeStatus := Nil;
  xScopeGetParam := Nil;
  xScopeResumeParam := Nil;
  xScopeGetLastMsg := Nil;
  xScopeGetCheque := Nil;
  xScopeGetCupomEx := Nil;
  xScopeFechaSessaoTEF := Nil;
  xScopeConsultaCDC := Nil;
  xScopeCompraCartaoDebito := Nil;
  xScopeConsultaCheque := Nil;
  xScopeCancelamento := Nil;
  xScopeReimpressaoComprovante := Nil;
  xScopeResumoVendas := Nil;
  xScopeObtemCampoExt := Nil;
  xScopeObtemCampoExt2 := Nil;
  xScopeObtemCampoExt3 := Nil;
  xScopeObtemHandle := Nil;
  xScopePagamento := Nil;
  xScopeRecargaCelular := Nil;
  xScopePreAutorizacaoCredito := Nil;
  xScopeRecuperaOperadorasRecCel := Nil;
  xScopeRecuperaValoresRecCel := Nil;
  xScopeConfigura := Nil;
  xScopeValidaInterfacePP := Nil;
  xScopeConsultaPP := Nil;
  xScopePPOpen := Nil;
  xScopePPOpenSecure := Nil;
  xScopePPClose := Nil;
  xScopePPGetCOMPort := Nil;
  xScopePPDisplay := Nil;
end;

procedure TACBrTEFScopeAPI.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('TACBrTEFScopeAPI: '+AErrorMsg);
  raise EACBrTEFScopeAPI.Create(AErrorMsg);
end;

procedure TACBrTEFScopeAPI.VerificarDiretorioDeTrabalho;
begin
  if (fDiretorioTrabalho = '') then
    fDiretorioTrabalho := ApplicationPath + 'TEF' + PathDelim + 'ScopeAPI';

  if not DirectoryExists(fDiretorioTrabalho) then
    ForceDirectories(fDiretorioTrabalho);

  if not DirectoryExists(fDiretorioTrabalho) then
    DoException(ACBrStr(Format(sErrDirTrabalhoInvalido, [fDiretorioTrabalho])));
end;

procedure TACBrTEFScopeAPI.VerificarEAjustarScopeINI;
var
  ini: TMemIniFile;
  sl: TStringList;
  i: Integer;
  sPathScopeIni, SecName, sEmpresa, sFilial, {sName,} sPort: String;
  ApagaSessoPrincipal, SemSessaoPrincipal: Boolean;

  procedure AjustarParamSeNaoExistir(const ASessao: String; const AChave: String; ValorPadrao: String);
  begin
    if not ini.ValueExists(ASessao, AChave) then
      ini.WriteString(ASessao, AChave, ValorPadrao);
  end;

  procedure AjusarSessaoLogAPI(const ASessao: String);
  begin
    AjustarParamSeNaoExistir(ASessao, 'TraceLevel', '8');
    AjustarParamSeNaoExistir(ASessao, 'LogFiles', '4');
    AjustarParamSeNaoExistir(ASessao, 'LogSize', '3072000');
    ini.WriteString(ASessao, 'LogPath', fDiretorioTrabalho + PathDelim + 'logs');
  end;

begin
  sPathScopeIni := GetScopeIniFullPath;
  ApagaSessoPrincipal := (fEmpresa <> '') and (fFilial <> '');
  SemSessaoPrincipal := True;
  ini := TMemIniFile.Create(sPathScopeIni);
  sl := TStringList.Create;
  try
    ini.ReadSections(sl);
    for i := 0 to sl.Count-1 do
    begin
      SecName := sl[i];
      if (Length(SecName) = 8) and StrIsNumber(SecName) then
      begin
        sEmpresa := copy(SecName,1,4);
        sFilial := copy(SecName,5,4);

        if ApagaSessoPrincipal and ((fEmpresa <> sEmpresa) or (fFilial <> sFilial)) then
          ini.EraseSection(SecName)
        else
        begin
          SemSessaoPrincipal := False;

          if (fEnderecoIP <> '') then
            ini.WriteString(SecName, 'Name', fEnderecoIP)
          else
            if not ini.ValueExists(SecName, 'Name') then
              DoException(ACBrStr(sErrEndServNaoEncontrado));

          if (fPortaTCP <> '') then
            ini.WriteString(SecName, 'Port', fPortaTCP)
          else
            AjustarParamSeNaoExistir(SecName, 'Port', '2046');
        end;

        AjustarParamSeNaoExistir(SecName, 'TimeOutAdm', '120');
        AjustarParamSeNaoExistir(SecName, 'VersaoAutomacao', fVersaoAutomacao);
        AjustarParamSeNaoExistir(SecName, 'CupomReduzido', IfThen(fCupomReduzido, 's', 'n'));
        AjustarParamSeNaoExistir(SecName, 'NaoAbrirDigitado', IfThen(fPermitirCartaoDigitado, 'n', 's'));
        AjustarParamSeNaoExistir(SecName, 'WKPAN', IfThen(fPinPadSeguro, 's', 'n'));
        Break;
      end;
    end;

    if SemSessaoPrincipal then
    begin
      if (fEmpresa = '') then
        sEmpresa := '0001'
      else
        sEmpresa := fEmpresa;
      if (fFilial = '') then
        sFilial := '0001'
      else
        sFilial := fFilial;

      SecName := sEmpresa + sFilial;
      if (fEnderecoIP = '') then
        DoException(ACBrStr(sErrEndServNaoInformado))
      else
        ini.WriteString(SecName, 'Name', fEnderecoIP);

      if (fPortaTCP <> '') then
        sPort := fPortaTCP
      else
        sPort := '2046';

      ini.WriteString(SecName, 'Port', sPort);
      ini.WriteString(SecName, 'TimeOutAdm', '120');
      ini.WriteString(SecName, 'VersaoAutomacao', fVersaoAutomacao);
      ini.WriteString(SecName, 'CupomReduzido', IfThen(fCupomReduzido, 's', 'n'));
      ini.WriteString(SecName, 'NaoAbrirDigitado', IfThen(fPermitirCartaoDigitado, 'n', 's'));
      ini.WriteString(SecName, 'WKPAN', IfThen(fPinPadSeguro, 's', 'n'));
    end;

    AjustarParamSeNaoExistir('PINDPAD', 'TamMinDados', '4');

    SecName := 'SCOPEAPI';
    AjustarParamSeNaoExistir(SecName, 'TraceApi', 's');
    AjustarParamSeNaoExistir(SecName, 'TraceSrl', 's');
    AjustarParamSeNaoExistir(SecName, 'TracePin', 's');
    AjustarParamSeNaoExistir(SecName, 'RedecardBit47Tag6', '1');
    ini.WriteString(SecName, 'ArqControlPath', fDiretorioTrabalho + PathDelim + 'control');
    ini.WriteString(SecName, 'ArqTracePath', fDiretorioTrabalho + PathDelim + 'trace');

    AjusarSessaoLogAPI('SCOPELOGAPI');
    AjusarSessaoLogAPI('SCOPELOGPRF');

    ini.UpdateFile;
  finally
    sl.Free;
    ini.Free;
  end;
end;

procedure TACBrTEFScopeAPI.ObterDadosScopeINI(out AEmpresa: String; out
  AFilial: String; out AEnderecoIP: String; out APortaTCP: String);
var
  ini: TMemIniFile;
  sl: TStringList;
  SecName, sPathScopeIni: String;
  i: Integer;
begin
  AEmpresa := ''; AFilial := ''; AEnderecoIP := ''; APortaTCP := '';
  sPathScopeIni := GetScopeIniFullPath;
  if not FileExists(sPathScopeIni) then
    Exit;

  ini := TMemIniFile.Create(sPathScopeIni);
  sl := TStringList.Create;
  try
    ini.ReadSections(sl);
    for i := 0 to sl.Count-1 do
    begin
      SecName := sl[i];
      if (Length(SecName) = 8) and StrIsNumber(SecName) then
      begin
        AEmpresa := copy(SecName,1,4);
        AFilial := copy(SecName,5,4);
        AEnderecoIP := Trim(ini.ReadString(SecName, 'Name', ''));
        APortaTCP := Trim(ini.ReadString(SecName, 'Port', ''));
        Break;
      end;
    end;
  finally
    sl.Free;
    ini.Free;
  end;
end;

procedure TACBrTEFScopeAPI.ExibirMensagem(const AMsg: String;
  Terminal: TACBrTEFScopeTerminalMensagem; TempoEspera: Integer);
begin
  GravarLog('  OnExibeMensagem( '+AMsg+
            ', '+GetEnumName(TypeInfo(TACBrTEFScopeTerminalMensagem), integer(Terminal) )+
            ', '+IntToStr(TempoEspera)+' )', True);
  fOnExibeMensagem(AMsg, Terminal, TempoEspera);
end;

function TACBrTEFScopeAPI.ConfigurarPortaPinPad(const APortaPinPad: String
  ): Word;
var
  sPorta, sPathScopeIni: String;
  ini: TMemIniFile;
begin
  sPorta := APortaPinPad;
  Result := StrToIntDef(sPorta, 0);
  if (Result = 0) then
  begin
    sPorta := AcharPortaPinPad;
    Result := StrToIntDef(sPorta, 0);
  end;

  if (Result = 0) and (sPorta <> '') then
  begin
    sPathScopeIni := GetScopeIniFullPath;
    ini := TMemIniFile.Create(sPathScopeIni);
    try
      ini.WriteString('PPCOMP', 'SerialNumPorts', '1');
      ini.WriteString('PPCOMP', 'SerialPort0', sPorta);
      Result := 1;
    finally
      ini.Free;
    end;
  end;
end;

function TACBrTEFScopeAPI.ObterVersaoScope: String;
var
  ret: longint;
  pszData: PAnsiChar;
begin
  Result := '';
  pszData := AllocMem(13);
  try
    GravarLog('ScopeVersao()');
    ret := xScopeVersao(pszData, 13);
    GravarLog('  ret: '+IntToStr(ret));
    if (ret = SCO_SUCESSO) then
    begin
      Result := String(pszData);
      GravarLog('  Result: '+Result);
    end
    else
      TratarErroScope(ret);
  finally
    Freemem(pszData);
  end;
end;

function TACBrTEFScopeAPI.AcharPortaPinPad: String;
var
  ret: longint;
  pszData: PAnsiChar;
begin
  Result := '';
  pszData := AllocMem(48);
  try
    GravarLog('ScopePPGetCOMPort()');
    ret := xScopePPGetCOMPort(pszData);
    GravarLog('  ret: '+IntToStr(ret));
    if (ret = SCO_SUCESSO) then
    begin
      Result := String(pszData);
      GravarLog('  Result: '+Result);
    end;
  finally
    Freemem(pszData);
  end;
end;

procedure TACBrTEFScopeAPI.ExibirMensagemPinPad(const MsgPinPad: String);
var
  ret: LongInt;
begin
  GravarLog('xScopePPDisplay( '+MsgPinPad+' )');
  ret := xScopePPDisplay(PAnsiChar(MsgPinPad));
  GravarLog('  ret: '+IntToStr(ret));

  if ret <> PC_OK then
    TratarErroScope(ret);
end;

procedure TACBrTEFScopeAPI.TratarErroScope(AErrorCode: LongInt);
var
  MsgErro: String;
begin
//E
  //Adicionar erros como os do Pinpad (Ex.: PC_NAO_ABERTO_APP ver pág 148)
  case AErrorCode of
    SCO_SUCESSO: MsgErro := '';
    SCO_TRN_EM_ANDAMENTO: MsgErro := ''; //'Transação em andamento';
    SCO_ERRO_PARM_1: MsgErro := 'Parâmetro 1 inválido';
    SCO_ERRO_PARM_2: MsgErro := 'Parâmetro 2 inválido';
    SCO_ERRO_PARM_3: MsgErro := 'Parâmetro 3 inválido';
    SCO_ERRO_PARM_4: MsgErro := 'Parâmetro 4 inválido';
    SCO_ERRO_ARQ_CICLO_TEF: MsgErro := 'Erro no arquivo de controle, finalização multi-TEF';
    //D SCO_API_NAO_FEZ_TRN: MsgErro := 'Ainda não fez nenhuma transação após a inicialização';
    SCO_API_NAO_INICIALIZADA: MsgErro := 'SCOPE API não foi inicializada';
    SCO_API_JA_INICIALIZADA: MsgErro := 'SCOPE API já foi inicializada';
    SCO_SRV_NOT_CFG: MsgErro := 'Servidor não configurado no arquivo '+CScopeINi;
    SCO_ERRO_LOGON_PDV: MsgErro := 'Verificar o erro retornado no log do ScopeSrv';
    SCO_ERRO_CONFIG_PDV: MsgErro := 'Verifique a configuração do perfil do PDV';
    //D SCO_THREAD_API_NOT_INIT: MsgErro := 'Não foi possível criar a “thread” na coleta de dados';
    SCO_ERRO_NUM_MULTI_TEF: MsgErro := 'Estourou o número máximo de TEF numa sessão multi-TEF';
    SCO_ERRO_SEM_ARQUIVO_DADOS: MsgErro := 'Não há arquivo com dados da transação anterior salvo';
  else
    MsgErro := Format('Erro: %d', [AErrorCode]);
  end;

  if (MsgErro <> '') then
    DoException(ACBrStr(MsgErro));
end;

procedure TACBrTEFScopeAPI.AbrirComunicacaoScope;
var
  ret: LongInt;
  sEmpresa, sFilial, sPDV, sEnderecoIP, sPorta: String;
begin
  if fConectado then
    Exit;

  GravarLog('AbrirComunicacaoScope');

  ObterDadosScopeINI(sEmpresa, sFilial, sEnderecoIP, sPorta);
  ExibirMensagem( ACBrStr(Format(sMsgAbrindoConexao, [sEmpresa, sFilial]) ));
  if (fPDV = '') then
    sPDV := '001'
  else
    sPDV:= fPDV;

  GravarLog('ScopeOpen( 2, '+sEmpresa+', '+sFilial+', '+sPDV+' )');
  ret := xScopeOpen( PAnsiChar('2'),
                     PAnsiChar(AnsiString(sEmpresa)),
                     PAnsiChar(AnsiString(sFilial)),
                     PAnsiChar(AnsiString(sPDV)) );
  GravarLog('  ret: '+IntToStr(ret));
  if ret <> SCO_SUCESSO then
    TratarErroScope(ret);

  fConectado := True;
  ExibirMensagem(Format(ACBrStr(sMsgConctadoAoServidor), [sEnderecoIP+':'+sPorta]));

  ConfigurarColeta;
  VerificaSessaoTEFAnterior;
end;

procedure TACBrTEFScopeAPI.FecharComunicacaoScope;
var
  ret:LongInt;
begin
  if not fConectado then
    Exit;

  GravarLog('ScopeClose()');
  ret := xScopeClose();
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> SCO_SUCESSO) and (ret <> SCO_API_NAO_INICIALIZADA) then
    TratarErroScope(ret);

  fConectado := False;
  ExibirMensagem(ACBrStr(sMsgDesconectado));
end;

procedure TACBrTEFScopeAPI.VerificarSeEstaConectadoScope;
begin
  if fConectado then
    Exit;

  if not fControleConexao then
    DoException(ACBrStr(sErrNaoConectado));

  AbrirComunicacaoScope;
end;

procedure TACBrTEFScopeAPI.VerificarSeMantemConexaoScope;
begin
  if not fControleConexao then
    Exit;

  FecharComunicacaoScope;
end;

procedure TACBrTEFScopeAPI.AbrirSessaoTEF;
var
  ret: LongInt;
begin
  if fSessaoAberta then
    Exit;

  GravarLog('AbrirSessaoTEF');

  VerificarSeEstaConectadoScope;

  ExibirMensagem(ACBrStr(sMsgInicioSessaoTEF));
  GravarLog('ScopeAbreSessaoTEF()');
  ret := xScopeAbreSessaoTEF;
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> SCO_SUCESSO) then
    TratarErroScope(ret);

  ExibirMensagem('');
  fSessaoAberta := True;
end;

procedure TACBrTEFScopeAPI.FecharSessaoTEF(Confirmar: Boolean; out
  TransacaoFoiDesfeita: Boolean);
var
  Acao, DesfezTEF: Byte;
  ret: LongInt;
begin
  TransacaoFoiDesfeita := False;
  if not fSessaoAberta then
    Exit;

  GravarLog('FecharSessaoTEF( '+BoolToStr(Confirmar, True)+' )');
  if Confirmar then
    Acao := SCO_CONFIRMA_TEF
  else
    Acao := SCO_DESFAZ_TEF;

  DesfezTEF := 0;
  GravarLog('ScopeFechaSessaoTEF( '+IntToStr(Acao)+' )');
  ret := xScopeFechaSessaoTEF(Acao, @DesfezTEF);
  GravarLog('  ret: '+IntToStr(ret)+', DesfezTEF: '+IntToStr(DesfezTEF));
  if (ret <> SCO_SUCESSO) then
    TratarErroScope(ret);

  fSessaoAberta := False;

  VerificarSeMantemConexaoScope;
end;

procedure TACBrTEFScopeAPI.VerificaSessaoTEFAnterior;
var
  DesfezTEF: Boolean;
begin
  GravarLog('VerificaSessaoTEFAnterior');
  FecharSessaoTEF(fConfirmarTransacoesPendentes, DesfezTEF);
  if DesfezTEF then
  begin
    ExibirMensagem(sMsgTransacaoDesfeita, tmOperador, 0);
    ExibirMensagem('');
  end;
end;

procedure TACBrTEFScopeAPI.IniciarTransacao(Operacao: TACBrTEFScopeOperacao;
  const Param1, Param2, Param3: String);
var
  p1, p2, p3: PAnsiChar;
  ret: LongInt;
  b: Boolean;
begin
  GravarLog('IniciarTransacao( '+GetEnumName(TypeInfo(TACBrTEFScopeOperacao), integer(Operacao))+
            ', '+Param1+', '+Param2+', '+Param3+' )' );

  if fEmTransacao then
    DoException(ACBrStr(sErrTransacaoJaIniciada));

  if not fSessaoAberta then
    AbrirSessaoTEF;

  p1 := PAnsiChar(AnsiString(Param1));
  p2 := PAnsiChar(AnsiString(Param2));
  p3 := PAnsiChar(AnsiString(Param3));
  ret := 0;

  case Operacao of
    scoCredito:
      begin
        GravarLog('ScopeCompraCartaoCredito( '+Param1+', '+Param2+' )');
        ret := xScopeCompraCartaoCredito(p1, p2);
      end;

    scoDebito:
      begin
        GravarLog('ScopeCompraCartaoDebito( '+Param1+' )');
        ret := xScopeCompraCartaoDebito(p1);
      end;

    //TODO: escrever outras outras operações
  end;

  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> SCO_SUCESSO) then
  begin
    FecharSessaoTEF(False, b);
    TratarErroScope(ret)
  end
  else
    SetEmTransacao(True);
end;

procedure TACBrTEFScopeAPI.ExecutarTransacao;
var
  ret, Status, Acao, TipoColeta: LongInt;
  iBarra: Byte;
  Cancelar: Boolean;
  pColeta: TParam_Coleta;
  pBuffer: PAnsiChar;
  s: String;
const
  cBarras = '|/-\';
begin
  GravarLog('ExecutarTransacao');

  if not fEmTransacao then
    DoException(ACBrStr(sErrTransacaoNaoIniciada));

  fDadosTransacao.Clear;
  pBuffer := AllocMem(2048); // Buffer de 2K
  try
    repeat
      // Enquanto a transacao estiver em andamento, aguarda....Loop
      Status := ObterScopeStatus;
      iBarra := 0;
      while (Status = SCO_TRN_EM_ANDAMENTO) or (Status =  SCO_COLETA_CARTAO_EM_ANDAMENTO) do
      begin
        //Apenas Exibe uma mensagem na tela para não parecer que está travado?
        inc(iBarra);
        if (iBarra) > 4 then
          iBarra := 1;
        ExibirMensagem(Format(ACBrStr(sMsgTransacaoEmAndamento), [cBarras[iBarra]]));

        ChamarTransacaoEmAndamento(scoestFluxoAPI, Cancelar);
        if Cancelar then
        begin
          AbortarTransacao;
          Exit;
        end;

        Sleep(CINTERVALO_COLETA);
        Status := ObterScopeStatus;
      end;

      //Ainda está em modo Coleta de dados?
      if ((Status < SCO_ESTADO_COLETA_INICIAL) or
          (Status > SCO_ESTADO_COLETA_FINAL)) then
      begin
        if (Status = SCO_SUCESSO) then
        begin
          ObterDadosDaTransacao;
          ExibirMensagem('Transacao completa')
        end
        else
          TratarErroScope(Status);

        Break;
      end;

      // Inicializa variveis do fluxo
      Acao := ACAO_COLETA_PROXIMO;
      TipoColeta := ENT_TECLADO;
      FillChar(pBuffer , length(pBuffer), #0);
      {$IfDef FPC}
       Initialize(pColeta);
      {$Else}
       FillChar(pColeta, SizeOf(TParam_Coleta), 0);
      {$EndIf}

      // Obtem dados do Scope e exibe as mensagens do cliente e operador */
      ret := xScopeGetParam(Status, @PColeta);
      if (ret <> SCO_SUCESSO) then
        TratarErroScope(Status);

      s := Trim(pColeta.MsgOp1 + sLineBreak + pColeta.MsgOp2);
      if (s <> '') then
        ExibirMensagem(s, tmOperador);

      s := Trim(pColeta.MsgCl1 + sLineBreak + pColeta.MsgCl2);
      if (s <> '') then
        ExibirMensagem(s, tmCliente);

      case Status of
        SCO_COLETAR_CARTAO,
        SCO_COLETAR_AUTORIZ_OU_CARTAO:
          begin

          end;

        SCO_IMPRIMIR_CUPOM,
        SCO_IMPRIMIR_CUPOM_PARCIAL,
        SCO_IMPRIMIR_CONSULTA:
        begin
          ObterDadosCupom;
        end;
      end;

    until False;

  finally
    Freemem(pBuffer);
  end;
end;

function TACBrTEFScopeAPI.EnviarParametroTransacao(Acao: LongInt;
  codTipoColeta: LongInt; Dados: AnsiString; dadosParam: Word): LongInt;
begin
  if (codTipoColeta < 0) then
    codTipoColeta := ObterScopeStatus;

  GravarLog('ScopeResumeParam( '+IntToStr(codTipoColeta)+', '+Dados+', '+IntToStr(dadosParam)+', '+IntToStr(Acao)+' )');
  Result := xScopeResumeParam(codTipoColeta, PAnsiChar(Dados), dadosParam, Acao);
  GravarLog('  ret: '+IntToStr(Result));
end;

procedure TACBrTEFScopeAPI.AbortarTransacao;
begin
  EnviarParametroTransacao(ACAO_COLETA_CANCELAR);
  SetEmTransacao(False);
end;

function TACBrTEFScopeAPI.ObterScopeStatus: Longint;
begin
  GravarLog('xScopeStatus');
  Result := xScopeStatus;
  GravarLog('  ret: '+IntToStr(Result));
end;

function TACBrTEFScopeAPI.ObterDadosCupom: Longint;
var
  pCabec, pCupomCliente, pCupomLoja, pCupomReduzido: PAnsiChar;
  NumeroLinhasReduzido: Byte;
  sCabec: String;
begin
  pCabec := AllocMem(1024);
  pCupomCliente := AllocMem(2048);
  pCupomLoja := AllocMem(2048);
  pCupomReduzido := AllocMem(2048);
  try
    GravarLog('ScopeGetCupomEx');
    Result := xScopeGetCupomEx( SizeOf(pCabec), pCabec,
                                SizeOf(pCupomCliente), pCupomCliente,
                                SizeOf(pCupomLoja), pCupomLoja,
                                SizeOf(pCupomReduzido), pCupomReduzido,
                                @NumeroLinhasReduzido);
    GravarLog('  ret: '+IntToStr(Result));

    if (Result = SCO_SUCESSO) then
    begin
      sCabec := String(pCabec);
      fDadosTransacao.Values['mask0-'+IntToHex(CUPOM_LOJA, 8)] := BinaryStringToString( sCabec + sLineBreak + String(pCupomLoja) );
      fDadosTransacao.Values['mask0-'+IntToHex(CUPOM_CLIENTE, 8)] := BinaryStringToString( sCabec + sLineBreak + String(pCupomCliente) );
      fDadosTransacao.Values['mask0-'+IntToHex(CUPOM_REDUZIDO, 8)] := BinaryStringToString( String(pCupomReduzido) );
    end;
  finally
    Freemem(pCabec);
    Freemem(pCupomCliente);
    Freemem(pCupomLoja);
    Freemem(pCupomReduzido);
  end;
end;

procedure TACBrTEFScopeAPI.ObterDadosDaTransacao;
var
  pBuffer: PAnsiChar;
  h, i, ret, mask: LongInt;
  val, hmask: string;
begin
  fDadosTransacao.Clear;

  //* receber o identificador da transacao */
  GravarLog('ScopeObtemHandle');
  h := xScopeObtemHandle(0);
  GravarLog('  ret: '+IntToStr(h));

  if (h <= SCO_ERRO_GENERICO) then
    TratarErroScope(h);

  pBuffer := AllocMem(1024);
  try
    mask := 1;
    for i := 1 to 32 do
    begin
      FillChar(pBuffer , length(pBuffer), #0);
      hmask := '$'+IntToHex(mask, 8);
      GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', '+hmask+', 0, 0, 0, : )');
      ret := xScopeObtemCampoExt3(h, mask, 0, 0, 0, Byte(':'), pBuffer);
      GravarLog('  ret: '+IntToStr(ret));
      val := String(pBuffer);
      fDadosTransacao.Add(Format('%s-%s=%s', ['mask1', hmask, val]));
      mask := mask << 1;
    end;

    for i := 1 to 32 do
    begin
      FillChar(pBuffer , length(pBuffer), #0);
      hmask := '$'+IntToHex(mask, 8);
      GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', 0, '+hmask+', 0, 0, : )');
      ret := xScopeObtemCampoExt3(h, 0, mask, 0, 0, Byte(':'), pBuffer);
      GravarLog('  ret: '+IntToStr(ret));
      val := String(pBuffer);
      fDadosTransacao.Add(Format('%s-%s=%s', ['mask2', hmask, val]));
      mask := mask << 1;
    end;

    for i := 1 to 32 do
    begin
      FillChar(pBuffer , length(pBuffer), #0);
      hmask := '$'+IntToHex(mask, 8);
      GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', 0, 0, '+hmask+', 0, : )');
      ret := xScopeObtemCampoExt3(h, 0, 0, mask, 0, Byte(':'), pBuffer);
      GravarLog('  ret: '+IntToStr(ret));
      val := String(pBuffer);
      fDadosTransacao.Add(Format('%s-%s=%s', ['mask3', hmask, val]));
      mask := mask << 1;
    end;

    for i := 1 to 32 do
    begin
      FillChar(pBuffer , length(pBuffer), #0);
      hmask := '$'+IntToHex(mask, 8);
      GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', 0, 0, 0, '+hmask+', : )');
      ret := xScopeObtemCampoExt3(h, 0, 0, 0, mask, Byte(':'), pBuffer);
      GravarLog('  ret: '+IntToStr(ret));
      val := String(pBuffer);
      fDadosTransacao.Add(Format('%s-%s=%s', ['mask4', hmask, val]));
      mask := mask << 1;
    end;
  finally
    Freemem(pBuffer);
  end;
end;

procedure TACBrTEFScopeAPI.AbrirPinPad;
var
  ret: LongInt;
  bConfig, bExclusivo, bPorta: Byte;
  Canal: Word;
  endereco: AnsiString;
begin
  FecharComunicacaoScope;
  GravarLog('AbrirPinPad');

  GravarLog('ScopeValidaInterfacePP( '+IntToStr(PP_INTERFACE_LIB_COMPARTILHADA)+' )');
  ret := xScopeValidaInterfacePP( PP_INTERFACE_LIB_COMPARTILHADA );
  GravarLog('  ret: '+IntToStr(ret));
  if ret <> PC_OK then
    TratarErroScope(ret);

  try
    VerificarSeEstaConectadoScope;

    GravarLog('ScopeConsultaPP()');
    ret := xScopeConsultaPP(@bConfig, @bExclusivo, @bPorta);
    GravarLog('  ret: '+IntToStr(ret)+
              ', Config:'+IntToStr(bConfig)+
              ', Exclusivo:'+IntToStr(bExclusivo)+
              ', Porta:'+IntToStr(bPorta) );
    if ret <> PC_OK then
      TratarErroScope(ret);

    if (bExclusivo = 0) then
    begin
      if (bPorta < 1) or (fPortaPinPad <> '') then
        bPorta := ConfigurarPortaPinPad(fPortaPinPad);

      if (bConfig = PC_MODO_ABECS) then
        fPinPadSeguro := True;

      if fPinPadSeguro then
      begin
        if (bPorta = 0) then
          Canal := PC_COMM_NONE
        else
          Canal := PC_COMM_SERIAL;

        GravarLog('ScopePPOpenSecure( '+IntToStr(Canal)+', '+IntToStr(bPorta)+' )');
        endereco := IntToStr(bPorta);
        ret := xScopePPOpenSecure(Canal, PAnsiChar(endereco));
      end
      else
      begin
        GravarLog('ScopePPOpen( '+IntToStr(bPorta)+' )');
        ret := xScopePPOpen(bPorta);
      end;

      GravarLog('  ret: '+IntToStr(ret));
      if ret <> PC_OK then
        DoException(ACBrStr(Format('Erro %d ao abrir o PinPad', [ret])));
    end;
  finally
    VerificarSeMantemConexaoScope;
  end;
end;

procedure TACBrTEFScopeAPI.ConfigurarColeta;
var
  ret: LongInt;
begin
  GravarLog('ScopeSetAplColeta()');
  ret := xScopeSetAplColeta();
  GravarLog('  ret: '+IntToStr(ret));

  ConfigurarScope( CFG_CANCELAR_OPERACAO_PINPAD, fPermitirCancelarOperacaoPinPad);
  ConfigurarScope( CFG_NAO_ABRIR_DIGITADO_COM_PP, not fPermitirCartaoDigitado);
  ConfigurarScope( CFG_DEVOLVER_SENHA_CRIPTOGRAFADA, True);
  ConfigurarScope( CFG_IMPRESSORA_CARBONADA, False);
  ConfigurarScope( CFG_ARMAZENA_EM_QUEDA, False);
  ConfigurarScope( CFG_ATUALIZA_TRANSACAO_EM_QUEDA, fConfirmarTransacoesPendentes);
  ConfigurarScope( CFG_PERMITIR_SAQUE, fPermitirSaque);
end;

function TACBrTEFScopeAPI.ConfigurarScope(AId: LongInt; Ligado: Boolean
  ): Boolean;
var
  ret, AParam: LongInt;
begin
  if Ligado then
    AParam := OP_HABILITA
  else
    AParam := OP_DESABILITA;

  GravarLog('ScopeConfigura( '+IntToStr(AId)+', '+IntToStr(AParam) +' )');
  ret := xScopeConfigura(AId, AParam);
  GravarLog('  ret: '+IntToStr(ret));
  Result := (ret = SCO_SUCESSO);
end;

procedure TACBrTEFScopeAPI.FecharPinPad;
var
  msg: AnsiString;
  ret: LongInt;
begin
  GravarLog('FecharPinPad');

  if (Trim(fMsgPinPad) = '') then
    msg := 'ACBR - SCOPE'
  else
    msg := fMsgPinPad;

  GravarLog('ScopePPClose( '+msg+' )');
  ret := xScopePPClose(PAnsiChar(msg));
  GravarLog('  ret: '+IntToStr(ret));
end;

end.


(*Verificar Daniel
// Guardar os bits q ligamos nos passos anteriores
iAux := PColeta.HabTeclas;
// Obtem dados do Scope e exibe as mensagens do cliente e operador
ret := xScopeGetParam(status, @PColeta);
// Restaurar os bits armazenados anteriormente
PColeta.HabTeclas := PColeta.HabTeclas or iAux;

// Tratamento dos estados
case ret of
  SCO_COLETAR_CARTAO, SCO_COLETAR_AUTORIZ_OU_CARTAO :
  begin
    sBufEntrAux := 'Digite o numero do cartao:';
  end;

  // imprime Cheque
  SCO_IMPRIME_CHEQUE:
  begin
    //// P/ indicarmos p/ a funcao execFormAux exibir esse texto no memo
    //// de exibicao
    //PColeta.HabTeclas := PColeta.HabTeclas or BIT7_ON;
    //sBufEntrAux := ObtemDadosCupom();
    //Imprime(sBufEntrAux);
    DoException('Não Implementado: SCO_IMPRIME_CHEQUE');
  end;

  // recupera a lista de valores da Recarga de Celular
  SCO_COLETA_VALOR_RECARGA:
  begin
    //sBufEntrAux := ObtemValoresRecarga(PColeta);
    //PColeta.HabTeclas := PColeta.HabTeclas or BIT7_ON;
    //Imprime(sBufEntrAux);
    DoException('Não Implementado: SCO_COLETA_VALOR_RECARGA');
  end;

  // recupera a lista de operadoras da Recarga de Celular
  SCO_COLETA_OPERADORA:
  begin
    //sBufEntrAux := ObtemOperadorasRecarga(PColeta);
    //PColeta.HabTeclas := PColeta.HabTeclas or BIT7_ON;
    //Imprime(sBufEntrAux);
    DoException('Não Implementado: SCO_COLETA_OPERADORA');
  end;

  // imprime Cupom + Nota Promissoria + Cupom Promocional
  SCO_IMPRIME_CUPOM,
  SCO_IMPRIME_CUPOM_PARCIAL, // imprime Cupom Parcial
  SCO_IMPRIME_CONSULTA: // imprime Consulta
  begin
    //{ P/ indicarmos p/ a funcao execFormAux exibir esse texto
    //no memo de exibicao }
    //PColeta.HabTeclas := PColeta.HabTeclas or BIT7_ON;
    //sBufEntrAux := ObtemCupons(PColeta);
    //Imprime(sBufEntrAux);
    DoException('Não Implementado: SCO_IMPRIME_CXXXX');
  end;

  // SCOPE aguardando o cartao que foi digitado
  SCO_CARTAO_DIGITADO:
  begin
    // Vamos ver c o usr ja' digitou o texto
    //sBufEntrAux := frmAuxParallelCtl (2, @bAux);
    //if bAux = 0 then
    //begin
    //  if Length (sBufEntrAux) <> 0 then
    //  begin
    //    // O texto ja' esta' em sBufEntrAux
    //    iAux := 0;  // Proximo estado
    //    bAux := 1;  // Nao podemos executar a funcao execFormAux
    //  end
    //  else
    //    // O usr teclou 'Cancelar' no PPad
    //    sBufEntrAux := 'Coleta:';
    //end;
    DoException('Não Implementado: SCO_CARTAO_DIGITADO');
  end;

  // mostra informacao e aguarda confirmacao do usuario
  SCO_MOSTRA_INFO_RET_SCOPE:
  begin
    if Length (sBufEntrAux) = 0 then
      sBufEntrAux := 'Coleta:';
    PColeta.HabTeclas := BIT9_ON;
    iAux := 0; // Garantir o envio da acao PROXIMO ao SCOPE
  end;

  SCO_COLETA_EM_ANDAMENTO:
  begin
    bAux := 1
  end;

  // Todos os outros estados nao tratados
  else
  begin
    if Length (sBufEntrAux) = 0 then
      sBufEntrAux := 'Coleta:';
  end;
end;

// Exibe as msgs retornadas pelo SCOPE
if (bAux <> 1) then
begin
  //iSinc := iSinc and not (ACAO_PROX + ACAO_ANTER + ACAO_CANCELAR);
  //Chama Form para usuário.
  //sBufEntrAux := execFormAux ( TrimRight ( string (PColeta.MsgOp1) ),
  //                          TrimRight ( string (PColeta.MsgOp2) ),
  //                          TrimRight ( string (PColeta.MsgCl1) ),
  //                          TrimRight ( string (PColeta.MsgCl2) ),
  //                          sBufEntrAux,
  //                          PColeta.HabTeclas);
  ExibirMensagem(TrimRight(string(PColeta.MsgOp1))+ TrimRight(string(PColeta.MsgOp2))+
                 TrimRight(string(PColeta.MsgCl1))+ TrimRight(string(PColeta.MsgCl2)) );
  // Em qual botao o usr clicou?
  // Proximo
  //if (iSinc and ACAO_PROX) <> 0 then
  //  iAux := COLETA_PROXIMO_ESTADO
  //// Anterior
  //else if (iSinc and ACAO_ANTER) <> 0 then
  //  iAux := COLETA_ANTERIOR_ESTADO
  //// Cancelar
  //else if (iSinc and ACAO_CANCELAR) <> 0 then
  //  iAux := COLETA_CANCELAR;
end;

//if ( iSinc and (ACAO_PROX + ACAO_ANTER + ACAO_CANCELAR) = 0 ) then
  //sBufEntrAux := '';

iRet := xScopeResumeParam(iRet, PAnsiChar(AnsiString(sBufEntrAux)), SCO_TECLADO, iAux);

if (iRet <> SCO_SUCESSO) then
begin
  iAux := xScopeGetLastMsg (@PColetaMsg);
  if (iAux <> SCO_SUCESSO) then
  begin
  //E
    //MostramensagemErro
    //showMensagemErro('Erro ao obter mensagens', iRet);
    //Application.MessageBox(PWideChar('ERRO NO ScopeGetLastMsg() = ' + IntToStr(iRet)), 'TEF', MB_OK);
  end;

  // Se o erro for de dado invalido, vamos permitir o usuario tentar novamente
  if (iRet <> SCO_DADO_INVALIDO) then
    Break
  else
    Continue;

  //E
  //Nunca Chega aqui!!! Código desnecessário.
  //execFormAux ( TrimRight ( string (PColeta.MsgOp1) ),
  //              TrimRight ( string (PColeta.MsgOp2) ),
  //              TrimRight ( string (PColeta.MsgCl1) ),
  //              TrimRight ( string (PColeta.MsgCl2) ),
  //              'Coleta:',
  //              2);
end;


until ((iRet < SCO_PRIMEIRO_COLETA_DADOS) and
        (iRet > SCO_ULTIMO_COLETA_DADOS));

//fEnd:

//sBufEntrAux := execFormAux ('Confirmar essa transacao? (1/0)',
                                  //'', '', '', 'Opcao', 2);

//if ((Length(sBufEntrAux) = 0) or (StrToInt (sBufEntrAux) <> 0)) then
//  bAux := SCO_CONFIRMA_TEF
//else
//  bAux := SCO_DESFAZ_TEF;
//FechaSessaoTEF (bAux);

//Result := iRet;

*)
