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
  sErrNaoImplementado = 'Não implementado %s';
  sErrEstruturaMenu = 'Erro na estrutura do Menu retornado';
  sErrUltTransDesfeita = 'A transação TEF anterior foi desfeita (cancelada). Reter o cupom TEF';
  sErrVersaoAutomacaoInvalido = 'Versão Automação deve ser:'+sLineBreak+
                                'RRAAAACCCC, onde:'+sLineBreak+
                                'RR - Release Certificação TEF'+sLineBreak+
                                'AAAA - Nome da Automação'+sLineBreak+
                                'CCCC - Código do Memorando'+sLineBreak+
                                'Exemplo: "01HOTK0000"';

  sMsgTituloMenu = 'Escolha uma opção';
  sMsgTituloAVista = 'A Vista ?';
  sMsgItemSim = 'SIM';
  sMsgItemNao = 'NÃO';
  sMsgAbrindoConexao = 'Abrindo comunicação...'+sLineBreak+'Empresa: %s, Filial %s, PDV: %S';
  sMsgConctadoAoServidor = 'Conectado Scope em: %s';
  sMsgDesconectado = 'Desconectado Scope';
  sMsgInicioSessaoTEF = 'Iniciando sessão TEF';
  sMsgTransacaoEmAndamento = 'Transação em Andamento';
  sMsgTransacaoCompleta = 'Transação Completa';
  sMsgTransacaoDesfeita = 'A TRANSAÇÃO TEF ANTERIOR FOI DESFEITA.'+sLineBreak+'RETER O CUPOM TEF.';

const
  CINTERVALO_COLETA = 300;

  {--------------------------------------------------------------------------------------------
                Codigos/erros devolvidos pelo Scope
  --------------------------------------------------------------------------------------------}
  RCS_SUCESSO                                 = $0000;

  RCS_COD_AUTORIZADORA_NAO_NUMERICO           = $0100;
  RCS_ALC_RESTRICAO_CLIENTE                   = $0101;
  RCS_ALC_JACONSULTOU_BANCOAG_IGUAIS          = $0102;
  RCS_ALC_JACONSULTOU_BANCOAG_DIFERENTES      = $0103;
  RCS_COD_GAR_NAO_AUTORIZADA                  = $0104;
  RCS_COMPRE_SAQUE_APROVADO_PARCIAL           = $0105;
  RCS_VOUCHER_APROVADO_PARCIAL                = $0106;
  RCS_APROVADO_COM_PREMIO                     = $0107;

  RCS_SERVER_JAVA_OFF                         = $EE00;
  RCS_SERVER_JAVA_TIMEOUT                     = $EE01;
  RCS_NAO_AUTORIZADO                          = $EE02;
  RCS_SENHA_INVALIDA                          = $EE03;

  RCS_PAGREC_RESPOSTA_INCOMPLETA              = $F800;
  RCS_PAGREC_RESPOSTA_INVALIDA                = $F801;

  RCS_AUTO_ERRO_CRD_RLV_INVALIDO              = $F900;
  RCS_AUTO_ERRO_CRD_TRK_INVALIDA              = $F901;
  RCS_AUTO_ERRO_CRD_INVALIDO                  = $F902;
  RCS_AUTO_ERRO_CRD_VALIDADE                  = $F903;
  RCS_AUTO_ERRO_PARM_INVALIDO                 = $F904;

  RCS_ERRO_PARM_1                             = $FA01;
  RCS_ERRO_PARM_2                             = $FA02;
  RCS_ERRO_PARM_3                             = $FA03;
  RCS_ERRO_PARM_4                             = $FA04;
  RCS_ERRO_PARM_5                             = $FA05;

  RCS_THREAD_API_NOT_INIT                     = $FB01;
  RCS_ERRO_CRIA_SERV                          = $FB02;
  RCS_ERRO_CRITICA_MSG                        = $FB03;
  RCS_ERRO_MONTA_MSG                          = $FB04;
  RCS_ERRO_ARQ_TEF                            = $FB05;
  RCS_ERRO_CONTEXTO_TEF                       = $FB06;
  RCS_ERRO_TOTAL_TEF                          = $FB07;
  RCS_ERRO_ARQ_CICLO_TEF                      = $FB08;
  RCS_ERRO_NUM_MAX_TEF_SESSAO                 = $FB09;
  RCS_ERRO_MONTANDO_CONFIRMACAO               = $FB0A;
  RCS_ERRO_MONTANDO_DESFAZIMENTO              = $FB0B;
  RCS_ERRO_CRIPTOGRAFIA                       = $FB0C;

  RCS_PRIMEIRO_COLETA_DADOS                   = $FC00;
  RCS_COLETAR_CARTAO                          = $FC00;

  RCS_MOSTRA_INFO_AGUARDA_CONF                = $FCFF;
  RCS_ULTIMO_COLETA_DADOS                     = $FCFF;

  RCS_TRN_EM_ANDAMENTO                        = $FE00;
  RCS_API_NAO_INICIALIZADA                    = $FE01;
  RCS_API_JA_INICIALIZADA                     = $FE02;
  RCS_EXISTE_TRN_SUSPENSA                     = $FE03;
  RCS_NAO_EXISTE_TRN_SUSPENSA                 = $FE04;
  RCS_API_NAO_FEZ_TRN                         = $FE05;
  RCS_POS_JA_LOGADO                           = $FE06;
  RCS_PROTOCOLO_NAO_SUPORTADO                 = $FE07;
  RCS_POS_NAO_CADASTRADO                      = $FE08;
  RCS_SRV_NOT_CFG                             = $FE09;
  RCS_NAO_HA_PDVS_DISPONIVEIS                 = $FE0A;
  RCS_PROTOCOLO_INCOMPATIVEL                  = $FE0B;
  RCS_NAO_PODE_DESFAZER_TRN_ENCERRADA         = $FE0C;
  RCS_NAO_HA_CAMPOS_SALVOS                    = $FE0D;
  RCS_CONTEUDO_INVALIDO                       = $FE0E;

  RCS_SERVER_OFF                              = $FF00;
  RCS_ACQUIRER_OFF                            = $FF01;
  RCS_CANCELADA_PELO_OPERADOR                 = $FF02;
  RCS_BIN_SERV_INV                            = $FF03;
  RCS_TRN_JA_CANCELADA                        = $FF04;
  RCS_TRN_NOT_FOUND_BD                        = $FF05;
  RCS_TRN_NAO_REVERSIVEL                      = $FF06;
  RCS_PARMS_INCOMPATIVEIS                     = $FF07;
  RCS_ERRO_BD                                 = $FF08;
  RCS_TIMEOUT_BD                              = $FF09;
  RCS_BD_OFFLINE                              = $FF0A;
  RCS_ABORTADA_PELO_APLICATIVO                = $FF0B;
  RCS_TRN_NAO_IMPLEMENTADA                    = $FF0C;
  RCS_HANDLE_INVALIDO                         = $FF0D;
  RCS_TX_SERV_INVALIDA                        = $FF0E;
  RCS_TX_SERV_EXCEDE_LIM                      = $FF0F;
  RCS_DADO_INVALIDO                           = $FF10;
  RCS_NAO_EXITE_CUPOM_VALIDO                  = $FF11;
  RCS_AREA_RESERVADA_INSUFICIENTE             = $FF12;
  RCS_ERRO_LIMITE                             = $FF13;
  RCS_TRN_DESFEITA                            = $FF14;
  RCS_DIGITACAO_NAO_PERMITIDA                 = $FF15;
  RCS_MEMORIA_INSUFICIENTE                    = $FF16;
  RCS_SERVICE_CODE_INVALIDO                   = $FF17;
  RCS_DATA_INVALIDA                           = $FF18;
  RCS_CARTAO_VENCIDO                          = $FF19;
  RCS_CARTAO_INVALIDO                         = $FF1A;
  RCS_DESFAZIMENTO_NAO_DISPONIVEL             = $FF1B;
  RCS_ERRO_IMPRESSAO_CUPOM                    = $FF1C;
  RCS_SESSAO_MTEF_EM_ANDAMENTO                = $FF1D;
  RCS_TRANSACAO_JA_EFETUADA                   = $FF1E;
  RCS_INSERIR_CARTAO_CHIP                     = $FF1F;
  RCS_CONTROLE_OBRIGATORIO                    = $FF20;
  RCS_PRE_AUTORIZACAO_OBRIGATORIA             = $FF21;
  RCS_SERVICO_NAO_CONFIGURADO                 = $FF22;
  RCS_SERVICO_NAO_DEFINIDO                    = $FF23;
  RCS_NUM_PARCELAS_INVALIDAS                  = $FF24;
  RCS_VALOR_INVALIDO                          = $FF25;
  RCS_BIN_SERV_INV_VISANET                    = $FF26;
  RCS_ESTADO_NAO_DEFINIDO                     = $FF27;
  RCS_OPERACAO_NAO_PERMITIDA                  = $FF28;
  RCS_CNPG_CPF_INVALIDO                       = $FF29;
  RCS_ERRO_DAC_BLK1                           = $FF2A;
  RCS_ERRO_DAC_BLK2                           = $FF2B;
  RCS_ERRO_DAC_BLK3                           = $FF2C;
  RCS_ERRO_DAC_BLK4                           = $FF2D;
  RCS_AID_INVALIDO                            = $FF2E;
  RCS_DISPONIVEL2                             = $FF2F;
  RCS_AUT_RETORNOU_DADOS_INVALIDOS            = $FF30;
  RCS_CONTA_NAO_PERMITIDA                     = $FF31;
  RCS_CONTA_VENCIDA                           = $FF32;
  RCS_NAO_EXISTE_RESUMO                       = $FF33;
  RCS_CODBAR_INVALIDO                         = $FF34;
  RCS_ERRO_DAC                                = $FF35;
  RCS_ERRO_FINALIZACAO_TRN_ANTERIOR           = $FF36;
  RCS_SERVICO_INVERTIDO                       = $FF37;
  RCS_CARTAO_NAO_PERMITIDO                    = $FF38;
  RCS_SCPC_CPF_ONLY                           = $FF39;
  RCS_ERRO_INTERNO_EXECUCAO_COLETA            = $FF3A;
  RCS_LISTA_NAO_DISPONIVEL                    = $FF3B;
  RCS_ERRO_LEITURA_CARTAO                     = $FF3C;
  RCS_CONTROLE_INVALIDO                       = $FF3D;
  RCS_ERRO_AO_ENVIAR_MSG_SERVIDOR             = $FF3E;
  RCS_INTERFACE_SAB_NAO_INICIALIZADA          = $FF3F;
  RCS_ERRO_DADOS_AINDA_NAO_DISPONIVEIS        = $FF40;
  RCS_ERRO_DADOS_INDISPONIVEIS                = $FF41;
  RCS_SERVIDOR_SAB_OFF                        = $FF42;
  RCS_ERRO_CONEXAO_SCOPE_E_SAB                = $FF43;
  RCS_ERRO_NSU_RECEBIDO                       = $FF44;
  RCS_ERRO_LOGON_PDV                          = $FF45;
  RCS_ERRO_PROCESSAMENTO_CHIP                 = $FF46;
  RCS_OPERADORA_INVALIDA                      = $FF47;
  RCS_DADOS_RECARGA_NAO_ENCONTRADOS           = $FF48;
  RCS_CANCELADA_PELO_CLIENTE                  = $FF49;
  RCS_APROVADA_OFFLINE                        = $FF50;
  RCS_VERSAO_BD_INCOMPATIVEL                  = $FF51;
  RCS_FORA_PRAZO                              = $FF52;
  RCS_MENSAGEM_INVALIDA                       = $FF53;
  RCS_PINPAD_AINDA_NAO_FOI_ABERTO             = $FF54;
  RCS_PINPAD_JA_FOI_ABERTO                    = $FF55;
  RCS_ESTADO_INVALIDO                         = $FF56;
  RCS_PP_COMPARTILHADO_NAO_CONFIGURADO        = $FF57;
  RCS_PP_COMPARTILHADO_NAO_TRABALHA_VISA2000  = $FF58;
  RCS_USO_EXCLUSIVO_INTERFACE_COLETA          = $FF59;
  RCS_AREA_ATRIBUTOS_SERV_INSUFICIENTE        = $FF5A;
  RCS_SCOPE_CONFIGURADO_PP_COMPARTILHADO      = $FF5B;
  RCS_SCOPE_NAO_CONFIGURADO_PP_COMPARTILHADO  = $FF5C;
  RCS_ERRO_ABERTURA_PERIFERICO                = $FF5D;
  RCS_ERRO_DESMONTA_ISO                       = $FF5E;
  RCS_BANDEIRA_NAO_CONFIGURADA                = $FF5F;
  RCS_FUNCAO_NAO_DISPONIVEL                   = $FF60;
  RCS_VALOR_MIN_PARC_INVALIDO                 = $FF61;
  RCS_VALOR_NAO_DISPONIVEL                    = $FF62;
  RCS_NUMTEL_INVALIDO                         = $FF63;
  RCS_DDD_INVALIDO                            = $FF64;
  RCS_ERRO_REDE_MODELO_2                      = $FF65;
  RCS_ERRO_REDE_MODELO_3                      = $FF66;
  RCS_PROMPTS_NAO_ENCONTRADOS                 = $FF67;
  RCS_USE_REIMPRESSAO_OFFLINE                 = $FF68;
  RCS_CONTRATO_SUSPENSO                       = $FF69;
  RCS_PERMITE_SOMENTE_DIGITADO                = $FF6A;
  RCS_NOT_FOUND                               = $FF6B;
  RCS_CODEAN128_INVALIDO                      = $FF6C;
  RCS_MOBILE_NAO_PERMITIDA                    = $FF6D;
  RCS_ACQUIRER_TIMEOUT                        = $FF6E;
  RCS_ERRO_ARQ_CONTEXTO                       = $FF6F;
  RCS_PLACA_INVALIDA                          = $FF70;
  RCS_CONSULTA_BRADESCO_NAO_HABILITADA        = $FF71;
  RCS_CODIGO_BANDEIRA_MAIOR_255               = $FF72;
  RCS_REDE_INICIANDO                          = $FF73;
  RCS_PINPAD_NAO_SUPORTADO_PERFIL             = $FF74;
  RCS_ERRO_ESTATISTICA_REDECARD               = $FF75;
  RCS_PINPAD_TABELAS_VAZIAS                   = $FF76;  { PINPad com tabelas vazias. Verifique se todas as redes desta filial fizeram inicialização de tabelas com sucesso. }
  RCS_CTLSS_NAO_HABILITADO_TERMINAL           = $FF77;  { Contactless não habilitado na carga de tabelas pela rede em questão para este estabelecimento. No caso da Cielo a habilitação é na tabela 1B – TerminalConfiguration. }
  RCS_CTLSS_NAO_PERMITIDO                     = $FF78;  { Contactless não permitido. AID em questão não possui dados contactless habilitados na carga de tabelas. }
  RCS_CTLSS_SEM_PRODUTO_HABILITADO            = $FF79;  { Contactless sem produto habilitado pela rede em questão. No caso da Cielo o produto não está habilitado na tabela 7C – GrupoRangeFuncao. }
  RCS_CTLSS_EXCEDE_VALOR_LIMITE               = $FF7A;  { Excede limite contactless. No caso da Cielo esse limite está estabelecido na tabela 4ª – RiscoContactless. }
  RCS_MODO_INVALIDO_INSIRA_CHIP               = $FF7B;  { Modo Inválido – Insira cartão com chip na leitora. }
  RCS_FALLBACK_NAO_PERMITIDO                  = $FF7C;  { Fallback não permitido. }
  RCS_MOEDA_INVALIDA                          = $FF7D;  { Moeda inválida. }
  RCS_MODO_ENTRADA_DIFERE_ORIGINAL            = $FF7E;  { Modo de entrada difere da transação original. Use o mesmo modo de entrada da transação original. }
  RCS_DOCUMENTO_INVALIDO                      = $FF7F;  { Documento inválido. }
  RCS_FUNCAO_NAO_PERMITIDA                    = $FF80;  { Chamada à função ScopeObtemTransacaoId não permitida. É permitida somente durante uma operação de crédito ou débito. }
  RCS_VALOR_MAX_PARC_INVALIDO                 = $FF81;  { Valor máximo da parcela inválido. }
  RCS_VERIFY_PAYMENT_PROFILE_DENIED           = $FF82;  { Verificação do Perfil de Pagamento Recorrente negada. }
  RCS_ERRO_ALOCACAO_MEMORIA                   = $FF83;  { Erro de alocação de memória na função ScopeObtemTransacaoId. }
  RCS_TOKEN_INVALIDO                          = $FF84;  { Token utilizado nas funções de Pagamento Recorrente inválido. }
  RCS_CARTAO_DIFERENTE                        = $FF85;  { Cartão utilizado na função ScopeAlteraPreAutorizacaoCredito não corresponde ao utilizado na função ScopePreAutorizacaoCredito. }
  RCS_TERMINAL_ORIGEM_OBRIGATORIO             = $FF86;  { Coleta do Terminal Origem nas funções ScopeAlteraPreAutorizacaoCredito e ScopeCapturaPreAutorizacaoCredito obrigatória. }
  RCS_PRE_AUTORIZACAO_JA_CANCELADA            = $FF87;  { Pré-Autorização foi cancelada. }
  RCS_PRE_AUTORIZACAO_JA_CONFIRMADA           = $FF88;  { Pré-Autorização já foi confirmada. }
  RCS_TERMINAL_ORIGEM_INVALIDO                = $FF89;  { Terminal Origem coletado nas funções ScopeAlteraPreAutorizacaoCredito e ScopeCapturaPreAutorizacaoCredito inválido. }
  RCS_DEBITO_PAGAMENTO_SEM_REDE_DISPONIVEL    = $FF8A;  { Sem rede disponível para efetuar o débito para pagamento. }
  RCS_CTLSS_VALOR_ZERO_NAO_PERMITIDO          = $FF8B;  { Contactless com valor zerado não permitido. }
  RCS_DESCONTO_NAO_PERMITIDO                  = $FF8C;  { Desconto não permitido. }
  RCS_PRODUTO_NAO_DISPONIVEL                  = $FF8D;  { Produto não disponível. }
  RCS_ERRO_CONSULTA_DINAMICA                  = $FF8F;  { Erro na consulta dinâmica. }

  RCS_ERRO_MODO_ABECS                         = $FF90;  { Chamada válida somente para PINpads ABECS. }
  RCS_ERRO_TAMANHO_INVALIDO                   = $FF91;  { Tamanho inválido, obrigatoriamente deve ser 8 bytes. }
  RCS_ERRO_ARQUIVO_INVALIDO                   = $FF92;  { Não foi possível abrir o arquivo. }
  RCS_ERRO_NOME_ARQUIVO_PP_INVALIDO           = $FF93;  { Nome do arquivo multimídia deve possuir somente caracteres numéricos e letras, sem espaços ou símbolos. Além disso, ele não é case sensitive. }
  RCS_ERRO_TIPO_ARQUIVO_PP_INVALIDO           = $FF94;  { Tipo de arquivo inválido. Deve ser um dos seguintes valores: ‘1’ = PNG, ‘2’ = JPG, ‘3’ = GIF. }
  RCS_REDE_LOGON_REQUIRED                     = $FF95;
  RCS_REDE_LISTA_PRIOR_AID_INDISPONIVEL       = $FF96;

  RCS_PP_NAO_ENCONTRADO                       = $FF97;
  RCS_NAO_EXISTEM_PRODUTOS                    = $FF98;
  RCS_CTLSS_MAGNETICO_NAO_SUPORTADO           = $FF99;
  RCS_ERRO_CONSULTA_PARAMETRO                 = $FF9A;
  RCS_TRANS_CTLS_NAO_PERMITIDA                = $FF9E; // BUG#071232
  RCS_PDV_SIGNATURE_REQUIRED                  = $FF9B; // BUG#069876
  RCS_PDV_SIGNATURE_UNCHECK                   = $FF9C; // BUG#069876
  RCS_SERVER_SIGNATURE_EMPTY                  = $FF9D; // BUG#069876

  RCS_ERRO_GET_HASH                           = $FF9F;
  RCS_ERRO_HASH                               = $FFA0;
  RCS_ERRO_TAMANHO_HASH                       = $FFA1;

  RCS_ERRO_GENERICO                           = $FFFF;

  RCS_ERRO_DESMONTANDO_PACOTE_RECEBIDO        = -5000;  { Erro desmontando o pacote recebido }
  RCS_ERRO_NAO_ACHOU_MASTERKEY                = -5001;  { Não achou a Master Key referente }
  RCS_ERRO_TIMEOUT_PACOTE                     = -5002;  { Ocorreu timeout do pacote }
  RCS_ERRO_CONFIGURANDO_REGISTRADOR           = -5003;  { Erro configurando o registrador }
  RCS_ERRO_PARAMETRO_LIB_INCORRETO            = -5004;  { Parâmetro lib incorreto }
  RCS_ERRO_LEITURA_TRILHA_OU_CARTAO           = -5005;  { Erro Leitura Cartao/Trilha }

  {--------------------------------------------------------------------------------------------
                 Define os parametros para a funcao ScopeObtemHandle
  --------------------------------------------------------------------------------------------}
  HDL_TRANSACAO_ANTERIOR     = $0000;
  HDL_TRANSACAO_EM_ARQUIVO   = $0008;
  HDL_TRANSACAO_EM_ANDAMENTO = $0009;

  {--------------------------------------------------------------------------------------------
                 Define as teclas que podem ser habilitadas
  --------------------------------------------------------------------------------------------}
  T_CANCELA = $01;
  T_PROXIMO = $02;
  T_RETORNA = $04;

  {--------------------------------------------------------------------------------------------
                 Codigos devolvidos pelas funcoes de acesso ao PIN-Pad Compartilhado
  --------------------------------------------------------------------------------------------}
  PC_OK                       = 0; // Operacao efetuada com sucesso - parametros de retorno(OUTPUT) contem dados validos.
  PC_PROCESSING               = 1; // Em processamento. Deve-se chamar a funcao novamente ou PC_Abort para finalizar.
  PC_NOTIFY                   = 2; // Em processamento. Deve-se apresentar no "checkout" uma mensagem retornada pela funcao e chama-la novamente ou PC_Abort para finalizar.
  PC_F1                       = 4; // Pressionada tecla de funcao #1.
  PC_F2                       = 5; // Pressionada tecla de funcao #2.
  PC_F3                       = 6; // Pressionada tecla de funcao #3.
  PC_F4                       = 7; // Pressionada tecla de funcao #4.
  PC_BACKSP                   = 8; // Pressionada tecla de apagar (backspace)

  // Status de 10 a 29 : Erros basicos da biblioteca
  PC_INVCALL                  = 10; // Chamada invalida ? funcao. Operacoes previas sao necessarias.
  PC_INVPARM                  = 11; // Parametro invalido passado a funcao.
  PC_TIMEOUT                  = 12; // Esgotado o tempo maximo estipulado para a operacao.
  PC_CANCEL                   = 13; // Operacao cancelada pelo operador.

  PC_ALREADYOPEN              = 14; // Pinpad ja aberto.
  PC_NOTOPEN                  = 15; // Pinpad nao foi aberto.
  PC_EXECERR                  = 16; // Erro interno de execucao - problema de implementao da biblioteca (software).
  PC_INVMODEL                 = 17; // Funcao nao suportada pelo modelo de pinpad.
  PC_NOFUNC                   = 18; // Funcao nao disponivel na Biblioteca do pinpad.
  PC_ERRMANDAT                = 19; // Ausencia de dado mandatorio para o processamento.
  PC_TABEXP                   = 20; // Tabelas expiradas (pelo "time-stamp").
  PC_TABERR                   = 21; // Erro ao tentar gravar tabelas (falta de espaco, por exemplo)
  PC_NOAPPLIC                 = 22; // Aplicacao da rede adquirente nao existe no pinpad.

  // 23 a 29 Reservado para uso futuro
  // Status de 30 a 39 : Erros de comunicacao/protocolo com o pinpad
  PC_PORTERR                  = 30; // Erro de comunicacao: porta serial do pinpad provavelmente ocupada.
  PC_COMMERR                  = 31; // Erro de comunicacao: pinpad provavelmente desconectado ou problemas com a interface serial.
  PC_UNKNOWNSTAT              = 32; // Status informado pelo pinpad nao e conhecido.
  PC_RSPERR                   = 33; // Mensagem recebida do pinpad possui formato invalido.
  PC_COMMTOUT                 = 34; // Tempo esgotado ao esperar pela resposta do pinpad (no caso decomandos nao blocantes).

  // 35 a 39 Reservado para uso futuro
  // Status de 40 a 49 : Erros b?sicos reportados pelo pinpad
  PC_INTERR                   = 40; // Erro interno do pinpad.
  PC_MCDATAERR                = 41; // Erro de leitura do cartao magnetico.
  PC_ERRPIN                   = 42; // Erro na captura do PIN - Master Key pode nao estar presente.
  PC_NOCARD                   = 43; // Nao ha cartao inteligente presente no acoplador.
  PC_PINBUSY                  = 44; // Pinpad nao pode processar a captura de PIN temporariamente devido a questoes de seguranca (como quando e atingido o limite de capturas dentro de um intervalo de tempo).

  // 45 a 49 Reservado para uso futuro.
  // Status de 50 a 59 : Erros de processamento de cart?o com chip (SAM)
  PC_SAMERR                   = 50; // Erro generico no modulo SAM.
  PC_NOSAM                    = 51; // SAM ausente, "mudo", ou com erro de comunicacao.
  PC_SAMINV                   = 52; // SAM invalido, desconhecido ou com problemas.

  // 53 a 59 Reservado para uso futuro.
  // Status de 60 a 99 : Erros de processamento de cartao com chip (usuario)
  PC_DUMBCARD                 = 60; // Cartao nao responde ("mudo") ou chip nao presente.
  PC_ERRCARD                  = 61; // Erro de comunica??o do pinpad com o cart?o inteligente.
  PC_CARDINV                  = 62; // Cartao do tipo invalido ou desconhecido, nao pode ser tratado (nao EMV nem TIBC v1).
  PC_CARDBLOCKED              = 63; // Cartao bloqueado por numero excessivo de senhas incorretas (somente para Easy-Entry TIBC v1).
  PC_CARDNAUTH                = 64; // Cartao TIBC v1 nao autenticado pelo modulo SAM (somente para Easy-Entry TIBC v1).
  PC_CARDEXPIRED              = 65; // Cartao TIBC v1 expirado (somente para Easy-Entry TIBC v1).
  PC_CARDERRSTRUCT            = 66; // Cartao com erro de estrutura - arquivos estao faltando.
  PC_CARDINVALIDAT            = 67; // Cartao foi invalidado.
                                    // Se o cartao for TIBC v1, quando selecao de arquivo ou ATR retornar status 6284.
                                    // Se o cartao for EMV, quando selecao de aplicacao retornar status 6A81.
  PC_CARDPROBLEMS             = 68;  // Cartao com problemas. Esse status e valido para muitas
                                     // ocorrencias no processamento de cartoes TIBC v1 e EMV onde o
                                     // cartao nao se comporta conforme o esperado e a transacao deve ser finalizada.
  PC_CARDINVDATA              = 69; // O cartao, seja TIBC v1 ou EMV, comporta-se corretamente porem possui dados invalidos ou inconsistentes.
  PC_CARDAPPNAV               = 70; // Cartao sem nenhuma aplicacao disponivel para as condicoes pedidas (ou cartao e reconhecido como TIBC v1 ou EMV mas nao
                                    // possui nenhuma aplicacao compativel com a requerida).
  PC_CARDAPPNAUT              = 71; // Somente para cartao EMV. A aplicacao selecionada nao pode ser
                                    // utilizada neste terminal pois o Get Processing Options retornou status 6985.
  PC_NOBALANCE                = 72; // Somente para aplicacao de moedeiro. O saldo do moedeiro e insuficiente para a operacao.
  PC_LIMITEXC                 = 73; // Somente para aplicacao de moedeiro. O limite maximo para a operacao foi excedido.
  PC_CARDNOTEFFECT            = 74; // Cartao ainda nao efetivo.
  PC_VCINVCURR                = 75; // Moeda ?nv?lida.
  PC_ERRFALBACK               = 76; // Erro de alto nivel no cartao EMV que e passivel de Fallback.

  // 77 a 79 Reservado para uso futuro.
  PC_CARDBLOCKED_ABECS        = 79; // Cartão bloqueado

  // 80 a 99 Erros de processamento de cartão com chip sem contato
  PC_CTLSSMULTIPLE            = 80; // Mais de um cartão sem contato foi apresentado ao leitor (este código de retorno é opcional e depende da capacidade do equipamento em detectar esta situação).
  PC_CTLSSCOMMERR             = 81; // Erro de comunicação entre o terminal (antena) e o cartão com chip sem contato.

  PC_CTLSSINVALIDAT           = 82; //0x0052 82 Cartão foi invalidado (seleção de aplicação retornou status ‘6ª81’).
  PC_CTLSSPROBLEMS            = 83; //0x0053 83 Cartão com problemas. Esse status é válido para muitas ocorrências no
                                    //processamento de cartões sem contato em que o cartão não se comporta
                                    //conforme o esperado e a transação deve ser finalizada.
  PC_CTLSSAPPNAV              = 84; //0x0054 84 Cartão sem nenhuma aplicação disponível para as condições pedidas
                                    //(nenhum AID encontrado).
  PC_CTLSSAPPNAUT             = 85; //0x0055 85 A aplicação selecionada não pode ser utilizada (o Get Processing Options
                                    //retornou status ‘6985’ ou houve erro no comando Select final), e não há
                                    //outra aplicação compatível na lista de candidatas.
  PC_CTLSSEXTCVM              = 86; // Holder must execute operation on device and tap again
  PC_CTLSFALLBACK             = 87; // Fallback from CTLS to contact is required

  // 200 a 299 Reservado para uso do Scope
  PC_RESERVADO                = 200; // Transacao negada na funcao PP_GoOnChip()
  PC_TRN_NEGADA_CHIP          = 201; // Transacao negada na funcao PP_GoOnChip()
  PC_MEM_NAO_ALOCADA          = 202; // Memoria nao alocada para a estrutura do pinpad compartilhado
  PC_ERRO_ALOCANDO_MEMORIA    = 203; // Erro alocando memoria
  PC_MEMORIA_INSUFICIENTE     = 204; // Memoria insuficiente para receber os dados
  PC_JA_ABERTO_VIA_SCOPE      = 205; // Pinpad ja aberto via Scope
  PC_MKEY_NAO_DEFINIDA        = 206; // Nao foi possivel definir a Master Key a ser utilizada
  PC_ESTADO_NAO_DEFINIDO      = 207; // Nao foi possivel definir o Estado de coleta no PIN-Pad
  PC_ERRO_PRM_GET_PIN         = 208; // Erro no parametro da funcao GetPIN
  PC_PINPAD_NAO_CONFIGURADO   = 209; // Pinpad nao configurado
  PC_DISPLAY_NAO_PERMITIDO    = 210; // Display nao permitido neste momento ou situacao
  PC_NAO_ABERTO_APP           = 211; // Pinpad nao foi aberto pela aplicacao
  PC_TIMEOUT_USER             = 212; // Timeout do cliente/usuario
  PC_DATA_NOT_FOUND           = 213; // Dado no chip nao encontrado

  PC_COMANDA_VAZIA            = 214; // Comanda nao possui itens
  PC_COMANDA_INVALIDA         = 215; // A Leitura da comanda apresentou erros
  PC_PINPAD_TABELAS_VAZIAS    = 216; // As tabelas do pinpad estao vazias
  PC_PINPAD_TABELAS_INDISP    = 217; // Problema na carga de tabelas do PINPad
  PC_NAO_PERMITIDA_NO_MODO    = 218; // A operação não é permitida no modo, pois não está disponível
  PC_SETA_UP                  = 219; // Tecla seta para cima
  PC_SETA_DOWN                = 220; // Tecla seta para BAIXO
  PC_ERR_RSA_KEY_MAX_ATTEMPT  = 221; // Não conseguiu gerar chave RSA

  PC_ERR_CARACTER_INVALIDO    = 222; // Caracter inválido na criptografia do bufffer - PinC_EncryptBuffer
  PC_PINPAD_TAB_IND_RUNNING   = 223; // Problema na carga de tabelas do Pinpad em andamento
  PC_PINPAD_TAB_INCOMPATIVEL  = 224; // Carga incompativel com pinpad compartilhado, tente ABECS 2.12

  // 225 a 299 Reservado para uso futuro.
  PC_MAX_ERRO                 = 300; // Indica o fim da tabela de erros

  {--------------------------------------------------------------------------------------------
                Tipo de aplicacao do PIN-Pad Compartilhado
  --------------------------------------------------------------------------------------------}
  PC_APL_CREDITO  = 1; // Aplicacao de Credito
  PC_APL_DEBITO   = 2; // Aplicacao de Debito
  PC_APL_QUALQUER = 99; // Qualquer aplicacao

  {--------------------------------------------------------------------------------------------
                Retornos do Parâmetro "Config" do método "ScopeConsultaPP"
  --------------------------------------------------------------------------------------------}
  PPCONF_MODO_NONE    = 0;    // Pinpad
  PPCONF_MODO_COMPART = 1;    // Pinpad compartilhado
  PPCONF_MODO_ABECS   = 2;    // Pinpad ABECS

  {--------------------------------------------------------------------------------------------
                Valores válidos para Parâmetro "Canal" do método "ScopePPOpenSecure"
  --------------------------------------------------------------------------------------------}
  CANAL_COMM_NONE      = 0;  // Conforme configuracao do scope.ini
  CANAL_COMM_SERIAL    = 1;  // Comunicacao serial
  CANAL_COMM_USB       = 2;  // Comunicacao USB
  CANAL_COMM_BLUETOOTH = 3;  // Comunicacao Bluetooth

  {--------------------------------------------------------------------------------------------
                Valores válidos para Parâmetro "Dado" do método "ScopePPStartGetData"
  --------------------------------------------------------------------------------------------}
  PP_DIGITE_O_DDD                = 1;
  PP_REDIGITE_O_DDD              = 2;
  PP_DIGITE_O_TELEFONE           = 3;
  PP_REDIGITE_O_TELEFONE         = 4;
  PP_DIGITE_DDD_TELEFONE         = 5;
  PP_REDIGITE_DDD_TELEFONE       = 6;
  PP_DIGITE_O_CPF                = 7;
  PP_REDIGITE_O_CPF              = 8;
  PP_DIGITE_O_RG                 = 9;
  PP_REDIGITE_O_RG               = 10;
  PP_DIGITE_OS_4_ULTIMOS_DIGITOS = 11;
  PP_DIGITE_CODIGO_DE_SEGURANCA  = 12;
  PP_DIGITE_O_CNPJ               = 13;
  PP_REDIGITE_O_CNPJ             = 14;

  {--------------------------------------------------------------------------------------------
                Valores válidos para Parâmetro "id" do método "ScopeConfigura"
  --------------------------------------------------------------------------------------------}
  CFG_CANCELAR_OPERACAO_PINPAD     = 1;   // Permite cancelar a interacao (leitura do cartao, senha e ...) no pinpad (default: desabilitado)
  CFG_OBTER_SERVICOS               = 2;   // Permite retornar o estado TC_OBTEM_SERVICOS durante o fluxo de TEF (default: desabilitado)
  CFG_NAO_ABRIR_DIGITADO_COM_PP    = 4;   // Permite nao abrir o digitado na leitura do cartao com o PP Compartilhado (default: desabilitado)
  CFG_DEVOLVER_SENHA_CRIPTOGRAFADA = 8;   // Permite devolver a senha criptografada com a master key da NCR (default: desabilitado, ou seja, devolve senha aberta)
  CFG_IMPRESSORA_CARBONADA         = 16;  // Permite configurar a impressora como carbonada para nao imprimir 2a via... (default: desabilitado, ou seja, no cupom exibira 1a e 2a via)
  CFG_ARMAZENA_EM_QUEDA            = 32;  // Armazena dados da coleta para recuperar em queda de energia. (default: desabilitado)
  CFG_MASCARAR_DADOS               = 64;  // Configura se mascaramento de dados pelo ObtemCampo esta habilitado. (default: habilitado)
  CFG_ATUALIZA_TRANSACAO_EM_QUEDA  = 128; // Permite confirmar/desfazer a transacao em caso de queda de energia. (default: desabilitado, ou seja, sempre desfazer)
  CFG_PERMITIR_SAQUE               = 256; // Habilita coleta de saque em operacoes de Debito A Vista da rede Cielo
  CFG_COLETA_RECARGA_PP            = 512; // Permite desabilitar a coleta do ddd e telefone no pinpad em recarga de celular (default: conforme configuracao do SCOPECNF)
  CFG_HABILITA_QRCODE_TELA         = 1024; // $00000400, Permite exibir o QRCode na tela, quando estiver habilitado, a automacao deve tratar o TC_OBTEM_QRCODE e obter o campo String_QRCode
  CFG_SIMULA_PP_PRECALL            = 269488145; // $10101011, Prepara a chamada para ligar simulação
  CFG_SIMULA_PP                    = 269488144; // $10101010, Liga a simulação de PP para Debug

  {--------------------------------------------------------------------------------------------
                Valores válidos para Parâmetro "Param" do método "ScopeConfigura"
  --------------------------------------------------------------------------------------------}
  OP_DESABILITA   = 0;
  OP_HABILITA     = 1;
  OP_SOMENTE_PCI  = 2;

  {--------------------------------------------------------------------------------------------
                Define os estados para a interface coleta
  --------------------------------------------------------------------------------------------}
  TC_CARTAO                          = $FC00;
  TC_VALIDADE_CARTAO                 = $FC01;
  TC_IMPRIME_CUPOM                   = $FC02;
  TC_CPF_CGC                         = $FC03;
  TC_BANCO                           = $FC04;
  TC_AGENCIA                         = $FC05;
  TC_NUMERO_CHEQUE                   = $FC06;
  TC_BOM_PARA                        = $FC07;
  TC_IMPRIME_CHEQUE                  = $FC08;
  TC_DECIDE_AVISTA                   = $FC09;
  TC_DECIDE_P_ADM_EST                = $FC0A;
  TC_DECIDE_P_DATADO                 = $FC0B;
  TC_DECIDE_P_AVISTA                 = $FC0C;
  TC_DECIDE_D_E_PARC                 = $FC0D;
  TC_QTDE_PARCELAS                   = $FC0E;
  TC_DECIDE_P_FINANC                 = $FC0F;
  TC_DIA_MES                         = $FC10;
  TC_SENHA                           = $FC11;
  TC_CONTROLE                        = $FC12;
  TC_FORMA_PAGAMENTO                 = $FC13;
  TC_PRIMEIRO_VENCIMENTO             = $FC14;
  TC_VALOR_ENTRADA                   = $FC15;
  TC_FORMA_ENTRADA                   = $FC16;
  TC_CONTA_CORRENTE                  = $FC17;
  TC_ULTIMOS_DIGITOS                 = $FC18;
  TC_REIMPRESSAO_COMPROVANTE         = $FC19;
  TC_DECISAO_C_PARC                  = $FC1A;
  TC_IMPRIME_CONSULTA                = $FC1B;
  TC_DECISAO_CONT                    = $FC1C;
  TC_DECIDE_ULTIMO                   = $FC1D;
  TC_NUMERO_CHEQUE_CDC               = $FC1E;
  TC_QTD_DIAS                        = $FC1F;
  TC_NUM_PRE_AUTORIZACAO             = $FC20;
  TC_DIA_MES_FECHADO                 = $FC21;
  TC_IMPRIME_NOTA_PROMISSORIA        = $FC22;
  TC_CEP                             = $FC23;
  TC_NUMERO_ENDERECO                 = $FC24;
  TC_COMPLEMENTO                     = $FC25;
  TC_PLANO_PAGAMENTO                 = $FC26;
  TC_CICLOS_A_PULAR                  = $FC27;
  TC_NRO_ITEM                        = $FC28;
  TC_CVV_CVC_2                       = $FC29;
  TC_AUSENCIA_CVV_CVC_2              = $FC2A;
  TC_DECIDE_GARANTIA                 = $FC2B;
  TC_DECIDE_RISCO                    = $FC2C;
  TC_COLETA_VALOR_SAQUE              = $FC2D;
  TC_COLETA_VALOR_RECARGA            = $FC2E;
  TC_COLETA_COD_LOC_TELEFONE         = $FC2F;
  TC_COLETA_NUM_TELEFONE             = $FC30;
  TC_COLETA_DIG_VERIFICADOR          = $FC31;
  TC_COLETA_DDMMAA                   = $FC32;
  TC_COLETA_VALOR_TX_SERVICO         = $FC33;
  TC_COLETA_VALOR                    = $FC34;
  TC_DECIDE_SAQUE                    = $FC35;
  TC_DECIDE_SAQUE_SIMULADO           = $FC36;
  TC_DECIDE_SALDO_EXTRATO            = $FC37;
  TC_DECIDE_RESUMIDO_SEGVIA          = $FC38;
  TC_DECIDE_CONSULTA_RESGATE         = $FC39;
  TC_COLETA_NSU_HOST                 = $FC3A;
  TC_COLETA_SERVICO                  = $FC3B;
  TC_COLETA_COD_REDE                 = $FC3C;
  TC_COLETA_COD_AUTORIZACAO_CREDITO  = $FC3D;

  TC_DECIDE_RESGATE_AVULSO           = $FC40;
  TC_COLETA_DDMMAAAA                 = $FC41;
  TC_COLETA_AUT_MEDICAMENTO          = $FC42;
  TC_COLETA_REG_MEDICAMENTO          = $FC43;
  TC_DISP_LISTA_MEDICAMENTO          = $FC44;
  TC_EXIBE_MSG                       = $FC45;
  TC_IMPRIME_CUPOM_PARCIAL           = $FC46;
  TC_COLETA_QTD_PARC_ACEITA1         = $FC47;
  TC_COLETA_COD_BARRAS               = $FC48;
  TC_COLETA_COD_CONSULTA_PBM         = $FC49;
  TC_COLETA_CRM_MEDICO               = $FC4A;
  TC_COLETA_COD_UF_CRM_MEDICO        = $FC4B;
  TC_COLETA_SEGURO                   = $FC4C;
  TC_DECIDE_CARTAO                   = $FC4D;
  TC_COLETA_DADOS_TOKORO             = $FC4E;
  TC_DECIDE_PAG_APOS_VENC            = $FC4F;
  TC_COLETA_DECIDE_COL_SENHA         = $FC50;
  TC_IMPRIME_CUPOM_PROMOCIONAL       = $FC51;
  TC_COLETA_UTILIZA_SALDO            = $FC52;
  TC_COLETA_CODIGO_MATERIAL          = $FC53;
  TC_COLETA_PLANO                    = $FC54;
  TC_DECIDE_PAGTO_CHEQUE             = $FC55;
  TC_DECIDE_CONFIRMA_TRN             = $FC56;
  TC_DECIDE_PAGTO_ROTATIVO           = $FC57;
  TC_COLETA_CMC7                     = $FC58;
  TC_DECIDE_DIN_TEF                  = $FC59;
  TC_COLETA_TEF_EXT_COD_GRUPO        = $FC5A;
  TC_COLETA_TEF_EXT_COD_REDE         = $FC5B;
  TC_COLETA_TEF_EXT_COD_ESTAB        = $FC5C;
  TC_COLETA_TEF_EXT_NSU_HOST         = $FC5D;
  TC_COLETA_TEF_EXT_DDMMAAAA         = $FC5E;
  TC_DECIDE_CONSULTA                 = $FC5F;
  TC_CONTA_PERMITIDA_CONTINUA        = $FC60;
  TC_COLETA_COD_BANDEIRA             = $FC61;
  TC_DECIDE_CONTA_FATURA             = $FC62;
  TC_COLETA_VALOR_TOTAL              = $FC63;
  TC_COLETA_RG                       = $FC64;
  TC_DECIDE_RETENTATIVA              = $FC65;
  TC_CPF                             = $FC66;
  TC_COLETA_ENDERECO                 = $FC67;
  TC_COLETA_ANDAR                    = $FC68;
  TC_COLETA_CONJUNTO                 = $FC69;
  TC_COLETA_BLOCO                    = $FC6A;
  TC_COLETA_BAIRRO                   = $FC6B;
  TC_COLETA_AUT_OU_CARTAO            = $FC6C;
  TC_COLETA_DATA_EMISSAO_CARTAO      = $FC6D;
  TC_COLETA_PLANO_INFOCARDS          = $FC6E;
  TC_COLETA_NUM_CUPOM_FISCAL         = $FC6F;
  TC_COLETA_OPERADORA                = $FC70;
  TC_COLETA_DADOS_SAB                = $FC71;
  TC_COLETA_NUM_TELEFONE_COM_DV      = $FC72;
  TC_COLETA_DADOS_TRN_FORCADA_SAB    = $FC73;
  TC_DECIDE_SERVICO_TECNICO          = $FC74;
  TC_COLETA_NUMERO_OS                = $FC75;
  TC_COLETA_ID_TECNICO               = $FC76;
  TC_COLETA_COD_OCORRENCIA           = $FC77;
  TC_COLETA_EPS_CREDENCIADA          = $FC78;
  TC_DECIDE_VALOR_ENTRADA            = $FC79;
  TC_DECIDE_COLETA_VALOR_1aPARCELA   = $FC7A;
  TC_COLETA_VALOR_1aPARCELA          = $FC7B;
  TC_COLETA_DADOS_ADICIONAIS         = $FC7C;
  TC_COLETA_CANCELA_TRANSACAO        = $FC7D;
  TC_GO_ON_CHIP                      = $FC7E;
  TC_RETIRA_CARTAO                   = $FC7F;
  TC_COLETA_VALOR_TAXA_EMBARQUE      = $FC80;
  TC_EXIBE_MSG_SALDO                 = $FC81;
  TC_EXIBE_MSG_RETORNA_FLUXO         = $FC82;
  TC_EXIBE_MSG_AGUARDA_OPERADOR      = $FC83;
  TC_OBTEM_SERVICOS                  = $FC84;
  TC_CARTAO_DIGITADO                 = $FC85;
  TC_COLETA_COD_PRODUTO              = $FC86;
  TC_EXIBE_MENU                      = $FC87;
  TC_DECIDE_INSS                     = $FC88;
  TC_COLETA_CONTRATO                 = $FC89;
  TC_COLETA_DATA_CLIENTE_DESDE       = $FC8A;
  TC_DISP_VALOR                      = $FC8B;
  TC_COLETA_DATA_TRN_ORIG            = $FC8C;
  TC_COLETA_NSU_TRN_ORIG             = $FC8D;
  TC_EXIBE_DADOS_CANC                = $FC8E;
  TC_DECIDE_VIAS_REIMPRESSAO         = $FC8F;
  TC_COLETA_DDD_PP                   = $FC90;
  TC_COLETA_NUM_TEL_PP               = $FC91;
  TC_COLETA_NUM_TELEFONE_COM_DV_PP   = $FC92;
  TC_COLETA_REDIGITACAO_RECARGA_PP   = $FC93;
  TC_TRANSACAO_APROVADA_PARCIAL      = $FC94;
  TC_COLETA_VALOR_PARCELAS           = $FC95;
  TC_PRIMEIRA_PARCELA_30_60          = $FC96;
  TC_DECIDE_CDC_PARCELE_MAIS         = $FC97;
  TC_DECIDE_VENDAS_PAGAMENTOS        = $FC98;
  TC_DECIDE_AVISTA_CJUROS            = $FC99;
  TC_COLETA_TEF_EXT_NUMERO_CARTAO    = $FC9A;
  TC_TIPO_SERVICO_COMBUSTIVEL        = $FC9B;
  TC_MATRICULA                       = $FC9C;
  TC_QUANTIDADE_COMBUSTIVEL          = $FC9D;
  TC_HODOMETRO                       = $FC9E;
  TC_PLACA_VEICULO                   = $FC9F;
  TC_COLETA_CEL_COD_ATIVACAO         = $FCA0;
  TC_EXIBE_MENU_RESGATE_PREMIO       = $FCA1;
  TC_CONFIRMA_OPCAO_RESGATE_PREMIO   = $FCA2;
  TC_CLIENTE_CONFIRMA_RESGATE        = $FCA3;
  TC_NRO_RESGATE_PREMIO              = $FCA4;
  TC_COLETA_NRO_VOUCHER              = $FCA5;
  TC_DECIDE_DARF_GPS                 = $FCA6;
  TC_DECIDE_TIPO_DARF                = $FCA7;
  TC_COLETA_CODIGO_RECEITA           = $FCA8;
  TC_COLETA_NUMERO_REFERENCIA        = $FCA9;
  TC_COLETA_VALOR_JUROS              = $FCAA;
  TC_CPF_PORTADOR                    = $FCAB;
  TC_CNPJ                            = $FCAC;
  TC_COLETA_PERCENTUAL               = $FCAD;
  TC_COLETA_MMAAAA                   = $FCAE;
  TC_COLETA_NUMERO_IDENTIFICADOR     = $FCAF;
  TC_COLETA_VALOR_INSS               = $FCB0;
  TC_COLETA_RECEITA_BRUTA            = $FCB1;
  TC_CONTA_PERMIT_CONT_BRAD_TIT      = $FCB2;
  TC_COLETA_VALOR_ACRESCIMO          = $FCB3;
  TC_COLETA_VALOR_DEDUCAO            = $FCB4;
  TC_COLETA_REDIGITA_DDD_PP          = $FCB5;
  TC_COLETA_CARTAO_DIGITADO_PP       = $FCB6;
  TC_COLETA_DIG_FINAIS_CARTAO_PP     = $FCB7;
  TC_COLETA_TIPO_CONSULTA            = $FCB8;
  TC_CONFIRMA_CARTAO_DIGITADO_PP     = $FCB9;
  TC_VALIDA_SAQUE_AUTOMACAO          = $FCBA;
  TC_SAQUE_PP_EM_ANDAMENTO           = $FCBB;
  TC_COLETA_DDD_NUMTEL_PP            = $FCBC;
  TC_REDIGITA_DDD_NUMTEL_PP          = $FCBD;
  TC_COLETA_DADOS_ECF                = $FCBE;
  TC_COLETA_COD_EAN                  = $FCBF;
  TC_COLETA_HORA_TRN_ORIG            = $FCC0;
  TC_DECIDE_PGTO_CARNE               = $FCC1;
  TC_COLETA_MODALIDADE               = $FCC2;
  TC_COLETA_CODIGO_CODUTOR           = $FCC3;
  TC_COLETA_COD_COMBUSTIVEL          = $FCC4;
  TC_COLETA_VOUCHER_FROTA            = $FCC5;
  TC_COLETA_DADOS_CARTAO_PRESENTE    = $FCC6;
  TC_PRIMEIROS_DIGITOS               = $FCC7;
  TC_COLETA_CAMPO_AUT                = $FCC8;
  TC_COLETA_CAMPO_DOC                = $FCC9;
  TC_COLETA_LISTA_PRECOS             = $FCCA;
  TC_COLETA_LISTA_MERCADORIAS        = $FCCB;
  TC_HORIMETRO                       = $FCCC;
  TC_COLETA_CARTAO_MAGNETICO         = $FCCD;

  TC_COLETA_LISTA_PLANOS             = $FCCE;
  TC_DECIDE_EMPRESTIMO_SAQUE         = $FCCF;
  TC_COLETA_NUM_DOCUMENTO            = $FCD0;
  TC_COLETA_PROJETO                  = $FCD1;
  //TC_SENHA_NOVA                    = $FCD2;
  //TC_SENHA_NOVA_CONF               = $FCD3;
  TC_COLETA_SEGMENTO_SAV             = $FCD4;
  TC_COLETA_FORNECEDOR_SAV           = $FCD5;
  TC_COLETA_PRODUTO_SAV              = $FCD6;
  TC_COLETA_QUANTIDADE               = $FCD7;
  TC_COLETA_CLIENTE_PREFERENCIAL     = $FCD8;

  TC_COLETA_RAMO_PRINC_CIELO_AUTO    = $FCD9;  { Coleta ramo principal - Cielo Auto }
  TC_COLETA_COD_MERC_CIELO_AUTO      = $FCDA;  { Coleta código do mercado - Cielo Auto }
  TC_COLETA_DEBITO_OU_CREDITO        = $FCDB;  { Coleta débito ou crédito }
  TC_COLETA_PONTOS                   = $FCDC;  { Coleta pontos }
  TC_RESP_CONS_RESG_PROD             = $FCDD;  { Resposta consulta resgate de produto }
  TC_RESP_CONS_RESG_PONTOS           = $FCDE;  { Resposta consulta resgate de pontos }

  TC_COLETA_MOEDA                    = $FCE0;  { Coleta moeda }
  TC_COLETA_FORNECEDOR_SGF           = TC_COLETA_FORNECEDOR_SAV;  { Coleta fornecedor SGF - equivalente ao SAV }
  TC_COLETA_CPF_CARTAO_OU_CODBARRAS  = $FCDF;  { Coleta CPF, cartão ou código de barras }
  TC_REDIGITA_COLETA_DDMMAAAA        = $FCE4;  { Redigita coleta data (DDMMAAAA) }
  TC_REDIGITA_VALOR                  = $FCE5;  { Redigita valor }
  TC_COLETA_NOME_PORTADOR            = $FCE6;  { Coleta nome do portador }
  TC_COLETA_COD_SEGURANCA_PP         = $FCE7;  { Coleta código de segurança do PP }

  TC_COLETA_CPFCNPJ_BENEFICIARIO     = $FCE8;  { Coleta CPF/CNPJ do beneficiário }
  TC_COLETA_CPFCNPJ_SACADOR          = $FCE9;  { Coleta CPF/CNPJ do sacador }
  TC_COLETA_CPFCNPJ_PAGADOR          = $FCEA;  { Coleta CPF/CNPJ do pagador }
  TC_COLETA_VALOR_DOCUMENTO          = $FCEB;  { Coleta valor do documento }
  TC_RESP_CONS_BACEN                 = $FCEC;  { Resposta consulta BACEN }

  TC_TERMINAL_ORIGEM                 = $FCED;  { Terminal origem }
  TC_COLETA_OPERADORA_ONLINE         = $FCEE;  { Coleta operadora online }
  TC_COLETA_VALOR_RECARGA_ONLINE     = $FCEF;  { Coleta valor de recarga online }
  TC_DECIDE_CARTAO_OU_DINHEIRO       = $FCF0;  { Decide entre cartão ou dinheiro }

  TC_DECIDE_CONSULTA_PARCELAMENTO    = $FCF2;  { Decide consulta parcelamento }
  TC_OBTEM_QRCODE                    = $FCF3;  { Obtém QRCode }
  TC_COLETA_DADO_ESPECIAL            = $FCF4;  { Coleta dado especial }
  TC_DECIDE_CREDIARIO                = $FCF5;  { Decide crediário }
  TC_COLETA_CPF_PP                   = $FCF6;  { Coleta CPF PP }
  TC_COLETA_DADOS_SPLIT_PAGAMENTO    = $FCF7;  { Coleta dados para split de pagamento }
  TC_DECIDE_NOVA_CONSULTA            = $FCF8;  { Decide nova consulta }
  TC_COLETA_PARCELA_GRATIS           = $FCF9;  { Coleta parcela grátis }
  TC_COLETA_LEITURA_SCANNER          = $FCFA;  { Coleta leitura por scanner }

  TC_COLETA_EXT                      = $FCFB;  { Coleta extendida - Último TC }

  // --> Proximo Tipo Coleta AQUI (ACIMA) //
  TC_COLETA_CARTAO_EM_ANDAMENTO      = $FCFC;
  TC_COLETA_EM_ANDAMENTO             = $FCFD;
  TC_INFO_RET_FLUXO                  = $FCFE;
  TC_INFO_AGU_CONF_OP                = $FCFF;

  TC_PRIMEIRO_TIPO_COLETA            = TC_CARTAO;
  TC_MAX_TIPO_COLETA                 = TC_INFO_AGU_CONF_OP;


  {--------------------------------------------------------------------------------------------
                Valores possiveis para o parametro TParam_Coleta.FormatoDado
  --------------------------------------------------------------------------------------------}
  TM_DDMMAA          = 0;  // Data no formato DDMMAA
  TM_DDMM            = 1;  // Data no formato DDMM
  TM_MMAA            = 2;  // Data no formato MMAA
  TM_HHMMSS          = 3;  // Hora no formato HHMMAA
  TM_NUM             = 4;  // Número inteiro
  TM_SENHA           = 5;  // Senha (interno ao SCOPE)
  TM_ULTIMOS_DIGITOS = 6;  // Últimos dígitos
  TM_ALFANUMERICO    = 7;  // Alfanumérico
  TM_DDMMAAAA        = 8;  // Data no formado DDMMAAAA
  TM_CONFIRMACAO     = 9;  // Display para exibição (não há coleta)
  TM_MMAAAA          = 10; // Data no formato MMAAAA
  TM_MASCARADO       = 11; // Exibir ‘*’ na tela, mas enviar em claro
  TM_HHMM            = 12; // Hora no formato HHMM
  TM_BOOL            = 13; // Booleano, a resposta deve ser 0=Não ou 1=Sim
  TM_VALOR_MONETARIO = 14; // Valor monetário, de tam=10+2 casas decimais, total=12
  TM_NUM_DECIMAL     = 15; // Número não inteiro (com casas decimais)
  TM_SELECAO         = 16; // Seleção de opção (Menu)
  TM_PAN             = 17;// PAN do cartão

  {--------------------------------------------------------------------------------------------
                Valores possiveis para o parametro TipoTabela de ScopeMenuRecuperaItens
  --------------------------------------------------------------------------------------------}
  MNU_TAB_TIPO_CIELO             = 1;  // Menu Dinâmico da CIELO
  MNU_TAB_TIPO_GENERICO          = 2;  // Menu Genérico para Transação POS
  MNU_TAB_TIPO_SAVS              = 3;  // Menu de Itens da Plataforma de Serviço;
  MNU_TAB_TIPO_GENERICO_DINAMICO = 4;  // Menu Genérico para Transação POS Dinâmico
  MNU_TAB_TIPO_SELECAO_ESPECIAL  = 5;  // Menu de Seleção, usado quando ScopeGetParamExt retornar TM_SELECAO em FormatoDado da stPARAM_COLETA_EXT
  QTD_MAX_ITENS_PERG_DIN_SEL     = 21;
  QTD_MAX_ITENS_PERG_GEN_SEL     = 16;

  {--------------------------------------------------------------------------------------------
                Valores possiveis para o parametro da funcao ScopeValidaInterfacePP()
  --------------------------------------------------------------------------------------------}
  PP_NAO_UTILIZA                 = 0;
  PP_INTERFACE_LIB_VISA          = 1;
  PP_INTERFACE_LIB_COMPARTILHADA = 2;

  {--------------------------------------------------------------------------------------------
                Valores possiveis para o parametro <Acao> da funcao ScopeFechaSessaoTEF()
  --------------------------------------------------------------------------------------------}
  ACAO_FECHA_DESFAZ_TEF   = 0;
  ACAO_FECHA_CONFIRMA_TEF = 1;


  {--------------------------------------------------------------------------------------------
                Valores possiveis para o parametro <Acao> da funcao ScopeResumeParam()
  --------------------------------------------------------------------------------------------}
  COLETA_TECLADO          = $0004;
  COLETA_CARTAO_MAGNETICO = $0020;

  {--------------------------------------------------------------------------------------------
                Valores possiveis para o parametro <Acao> da funcao ScopeResumeParam()
  --------------------------------------------------------------------------------------------}
  ACAO_PROXIMO_ESTADO  = 0;
  ACAO_ESTADO_ANTERIOR = 1;
  ACAO_CANCELAR        = 2;
  ACAO_APL_ERRO        = 3;
  ACAO_COLETAR         = 99;

  {--------------------------------------------------------------------------------------------
                Enumerador dos tipos das operadoras de celular
  --------------------------------------------------------------------------------------------}
  REC_CEL_OPERADORAS_MODELO_1 = 1;
  REC_CEL_OPERADORAS_MODELO_2 = 2;
  REC_CEL_OPERADORAS_MODELO_3 = 3;

  {--------------------------------------------------------------------------------------------
                Enumerador dos tipos de estruturas retornadas para os valores de recarga
  --------------------------------------------------------------------------------------------}
  REC_CEL_VALORES_MODELO_1 = 1;
  REC_CEL_VALORES_MODELO_2 = 2;
  REC_CEL_VALORES_MODELO_3 = 3;

  {--------------------------------------------------------------------------------------------
     Constantes retornadas por 'ScopeGetCupomEx'
  --------------------------------------------------------------------------------------------}
  RET_CUPOM_LOJA         = 'CUPOM_LOJA';
  RET_CUPOM_CLIENTE      = 'CUPOM_CLIENTE';
  RET_CUPOM_REDUZIDO     = 'CUPOM_REDUZIDO';
  RET_NUMLINHAS_REDUZIDO = 'NUMLINHAS_REDUZIDO';

  {--------------------------------------------------------------------------------------------
     Constantes retornadas por 'ScopeObtemConsultaValeGas'
  --------------------------------------------------------------------------------------------}
  RET_VALOR_VALE_GAS = 'VALOR_VALE_GAS';

  {--------------------------------------------------------------------------------------------
     Constantes retornadas por 'ScopeGetCheque'
  --------------------------------------------------------------------------------------------}
  RET_CHEQUE_BANCO   = 'CHEQUE_BANCO';
  RET_CHEQUE_AGENCIA = 'CHEQUE_AGENCIA';
  RET_CHEQUE_NUMERO  = 'CHEQUE_NUMERO';
  RET_CHEQUE_VALOR   = 'CHEQUE_VALOR';
  RET_CHEQUE_DATA    = 'CHEQUE_DATA';
  RET_CHEQUE_CODAUT  = 'CHEQUE_CODAUT';
  RET_CHEQUE_MUNICIP = 'CHEQUE_MUNICIP';


  {--------------------------------------------------------------------------------------------
     Constantes de Serviços usadas em 'ScopePagamento' e 'ScopePagamentoConta'
  --------------------------------------------------------------------------------------------}
  SRV_DEBITO_AVISTA = 006;           // Compra com cartão de débito á vista
  SRV_CREDITO_AVISTA = 009;          // Compra com cartão de crédito á vista
  SRV_PREAUT_CREDITO = 013;          // Pré-Autorização com cartão de crédito
  SRV_CONS_CHEQUE = 017;             // Consulta de cheques – a vista
  SRV_CONS_CHEQUE_PRE = 018;         // Consulta de cheques – pré-datados
  SRV_DEBITO_AVISTA_FORC = 020;      // Compra com cartão de débito à vista forçada
  SRV_DEBITO_PRE = 021;              // Compra com cartão de débito pré-datada
  SRV_DEBITO_PARC_SEM_ENT = 022;     // Compra com cartão de débito parcelada sem parcela à vista
  SRV_DEBITO_PARC_COM_ENT = 023;     // Compra com cartão de débito parcelada – parcela à vista
  SRV_DEBITO_PARC_COM_ENT_FORC = 024;// Compra com cartão de débito parcelada – parcela à vista forçada
  SRV_VALE_GAS = 025;                // Compra Vale Gás
  SRV_CREDITO_PARC_ADM = 027;        // Compra com cartão de crédito parcelado pela administradora
  SRV_CREDITO_PARC_ESTAB = 028;      // Compra com cartão de crédito parcelado pelo estabelecimento
  SRV_CANC_DEBITO = 031;             // Cancelamento de compra de débito
  SRV_CANC_CREDITO = 032;            // Cancelamento de compra de crédito
  SRV_CDC = 034;                     // Compra CDC (CNS)
  SRV_GRANTIA_CHEQUE = 035;          // Garantia de cheques
  SRV_DESCONTO_CHEQUE = 036;         // Desconto de cheques
  SRV_RESUMO_VENDA = 037;            // Solicitação de resumo de vendas
  SRV_CREDITO_IATA = 039;            // Compra com cartão de crédito IATA
  SRV_CREDITO_IATA_PARC_JUROS = 040; // Compra com cartão de crédito IATA parcelado com juros
  SRV_CREDITO_IATA_PARC_SEM_JUROS = 041; // Compra com cartão de crédito IATA parcelado sem juros
  SRV_CANC_CREDITO_IATA = 042;       // Cancelamento de compra de credito IATA
  SRV_CANC_CDC = 043;                // Cancelamento compra com cartão CDC
  SRV_CONS_CDC = 044;                // Consulta planos de pagamento para cartão CDC
  SRV_CARTAO_CDC = 045;              // Compra com cartão CDC
  SRV_CONS_PARC_CRED = 047;          // Consulta parcelas de crédito
  SRV_CONS_PARC_DEB = 048;           // Consulta parcelas de débito
  SRV_DEBITO_VOUCHER = 050;          // Compra com cartão de débito Voucher (Alimentação)
  SRV_CANC_DEBITO_VOUCHER = 051;     // Cancelamento de compra com cartão de débito Voucher (Alimentação)
  SRV_CANC_GARANTIA_CHEQUE = 056;    // Cancelamento de garantia de cheque
  SRV_CONS_AVS = 058;                // Consulta AVS
  SRV_CASH = 059;                    // Cash
  SRV_CANC_CASH = 060;               // Cancelamento de Cash
  SRV_CONFIRMA_PREAUT = 061;         // Confirmação de Pré-Autorização
  SRV_ESTORNO_PREAUT = 062;          // Estorno de Pré-Autorização
  SRV_CONS_FIDELIDADE = 063;         // Consulta de Pontos - Fidelidade
  SRV_CONS_SALDO_CRED = 064;         // Consulta saldo de crédito
  SRV_CONS_CASH = 065;               // Consulta Cash
  SRV_CONS_VAL_RECARGA_CEL = 068;    // Consulta valores possíveis de recarga de celular
  SRV_RECARGA_CEL = 069;             // Recarga de celular
  SRV_CONS_SALDO = 070;              // Consulta saldo
  SRV_CONS_EXTR_RESUMIDO = 071;      // Consulta extrato resumido
  SRV_CONS_EXTRATO = 072;            // Consulta extrato
  SRV_SIMULA_SAQUE = 073;            // Simulação de saque
  SRV_SAQUE = 074;                   // Saque – Crédito
  SRV_CONS_SALDO_INVEST = 075;       // Consulta saldo de investimento
  SRV_CONS_EXTRATO_INVEST = 076;     // Consulta extrato de investimento
  SRV_RESGATE_AVULSO = 077;          // Resgate avulso
  SRV_RESGATE = 078;                 // Resgate
  SRV_CANC_SAQUE = 079;              // Cancelamento de saque
  SRV_CANC_RESGATE = 080;            // Cancelamento de resgate
  SRV_OBTEM_CART_INVEST = 081;       // Obtém cartão de investimento
  SRV_CONS_MEDICAMENTO = 082;        // Consulta medicamento
  SRV_COMPRA_MEDICAMENTO = 083;      // Compra medicamento
  SRV_ESTORNO_COMPRA_MEDICAMENTO = 084; // Estorno compra medicamento
  SRV_PAGTO_CONTA_CARTAO = 085;      // Pagamento de conta com cartão
  SRV_SOLIC_AUTORIZ = 086;           // Solicitação de autorização
  SRV_PAGTO_CONTA_SEM_CARTAO = 087;  // Pagamento de conta sem cartão
  SRV_DEBITO_VOUCHER_PARC = 088;     // Débito Voucher parcelado
  SRV_CONS_PAGTO_CONTA = 089;        // onsulta pagamento de conta
  SRV_ESTORNO_PAGTO_CONTA = 090;     // Estorno de pagamento de conta
  SRV_PAGTO_FUTURA = 091;            // Pagamento de fatura
  SRV_CONS_SALDO_DEBITO = 092;       // Consulta Saldo Dèbito
  SRV_RESUMO_PAGTOS = 093;           // Resumo de Pagamentos
  SRV_BAIXA_OS = 094;                // Baixa de O.S.
  SRV_TESTE_COMUNIC = 095;           // Teste de Comunicação
  SRV_ESTATISTICA = 096;             // Estatística
  SRV_MOEDEIRO = 097;                // Moedeiro
  SRV_CARTAO_DINHEIRO = 098;         // Compra com Cartão Dinheiro
  SRV_ESTORNO_CARTAO_DINHEIRO = 099; // Estorno da compra com Cartão Dinheiro

  {--------------------------------------------------------------------------------------------
     Constantes retornadas por 'ScopeGetParam'
  --------------------------------------------------------------------------------------------}
  CBANDEIRA = 'BANDEIRA';

  {--------------------------------------------------------------------------------------------
     Constantes da máscara 1 da função ScopeObtemCampoExt2
  --------------------------------------------------------------------------------------------}
  MASK1_Numero_Conta_PAN                   = $00000001;  { Personal Account Number (Card number) }
  MASK1_Valor_transacao                    = $00000002;  { Amount }
  MASK1_NSU_transacao                      = $00000004;  { Transaction Id assigned by Scope }
  MASK1_Hora_local_transacao               = $00000008;  { Transaction time }
  MASK1_Data_local_transacao               = $00000010;  { Transaction date }
  MASK1_Data_vencimento_cartao             = $00000020;  { Card due date }
  MASK1_Data_referencia                    = $00000040;  { Account date }
  MASK1_Numero_cheque                      = $00000080;  { Check number }
  MASK1_Codigo_autorizacao                 = $00000100;  { Authorization code }
  MASK1_Codigo_resposta                    = $00000200;  { Action code }
  MASK1_Identificacao_terminal             = $00000400;  { POS Id }
  MASK1_Codigo_Origem_Mensagem             = $00000800;  { Store Id assigned by the acquirer at agreement time }
  MASK1_Plano_Pagamento                    = $00001000;  { Number of parcels }
  MASK1_Valor_Taxa_Servico                 = $00002000;  { Tip value }
  MASK1_NSU_Host                           = $00004000;  { Transaction Id assigned by Acquirer }
  MASK1_Cod_Banco                          = $00008000;  { Bank code }
  MASK1_Cod_Agencia                        = $00010000;  { Branch code }
  MASK1_Data_Vencimento                    = $00020000;  { Due date (99ddmmyyyy [...]) }
  MASK1_Cod_Bandeira                       = $00040000;  { Acquirer Code }
  MASK1_Cod_Servico                        = $00080000;  { Service Code }
  MASK1_Texto_BIT_62                       = $00100000;  { BIT 62 }
  MASK1_Controle_Dac                       = $00200000;  { Control DAC }
  MASK1_Cod_Rede                           = $00400000;  { Net Code }
  MASK1_Nome_Bandeira                      = $00800000;  { Acquirer Name }
  MASK1_Nome_Rede                          = $01000000;  { Net Name }
  MASK1_Cartao_Trilha02                    = $02000000;  { Card - Track 02 }
  MASK1_Numero_Promissorias                = $04000000;  { Number of promissory note }
  MASK1_Cod_Estab_Impresso                 = $08000000;  { Establishment code printed on ticket }
  MASK1_Numero_CMC7                        = $10000000;  { CMC7 Number }
  MASK1_CGC_Convenio                       = $20000000;  { CGC Number }    // Modo_Pagamento
  MASK1_Msg_Autentic_Cheque                = $40000000;  { Check Autentic Message }
  MASK1_Saldo_Disponivel                   = $80000000;  { Available Cache }

  {--------------------------------------------------------------------------------------------
     Constantes da máscara 2 da função ScopeObtemCampoExt2
  --------------------------------------------------------------------------------------------}
  MASK2_NSU_transacao_Original             = $00000001;  { Cancel Transaction Id assigned by Scope }
  MASK2_Cliente_Com_Seguro                 = $00000002;  { Ensured Client } {(IBICred)}
  MASK2_Dados_Parcelado_Cetelem            = $00000004;  { Informations about parcels Cetelem }
  MASK2_Data_Movimento                     = $00000008;  { Interchange: Data Movimento }
  MASK2_Nome_Convenio                      = $00000010;  { Interchange: Nome da Empresa de Convênio }
  MASK2_Lista_TEF_Permitidas               = $00000020;  { Interchange: Lista das formas de pagamento em TEF permitidas }
  MASK2_Linha_Autenticacao                 = $00000040;  { Interchange - Fininvest: Linha de autenticação }
  MASK2_Dados_Consulta_Fatura              = $00000080;  { Interchange - Fininvest: Dados da Consulta Fatura }
  MASK2_Forma_Financiamento                = $00000100;  { Type of Financing }
  MASK2_Codigo_Resposta_AVS                = $00000200;  { Return Code for AVS }
  MASK2_Pontos_AdquiridosOuResgatados      = $00000400;  { Pontos adquiridos ou resgatados }
  MASK2_Fator_Compra                       = $00000800;  { Fator de compra }
  MASK2_NSU_Host_Transacao_Original        = $00001000;  { NSU Host da transação original (cancelamento) }
  MASK2_Identificacao_Cliente_PBM          = $00002000;  { Identificação do Cliente junto a Autorizadora }
  MASK2_Cod_Operadora                      = $00004000;  { Código da Operadora de Celular }
  MASK2_Cod_Local_Telefone                 = $00008000;  { DDD }
  MASK2_Num_Telefone                       = $00010000;  { Telefone }
  MASK2_Dados_ValeGas                      = $00020000;  { ULTRAGAZ: Dados do ValeGás }
  MASK2_Codigo_IF                          = $00040000;  { Código IF (Instituição Financeira) }
  MASK2_Num_Item_Finivest_ou_Contrato      = $00080000;  { Fininvest ou Cetelem
                                                           IBI: Numero do contrato (CPCHEQUE/INSS) }
  MASK2_Valor_Taxa_Embarque                = $00100000;  { Taxa de embarque }
  MASK2_Digitos_TR2SON                     = $00200000;  { Uso exclusivo sonae }
  MASK2_Taxa_Cliente_Lojista               = $00400000;  { Informação bit 124 - CDC Orbitall }
  MASK2_Cod_Servico_Original               = $00800000;  { Transação de cancelamento: Código de Serviço da transação original }
  MASK2_Cod_Barras                         = $01000000;  { Código de Barras }
  MASK2_Permite_Desfazimento               = $02000000;  { Permite cancelamento }
  MASK2_Logo_PAN                           = $04000000;  { Retorna o LOGO do cartão: bytes 7 e 8 do PAN }
  MASK2_Cod_Empresa                        = $08000000;  { Código da Empresa - HSBC }
  MASK2_Cod_Autenticacao                   = $10000000;  { Código de autenticação - ISOGCB }
  MASK2_Dados_Pagto_ISOGCB                 = $20000000;  { Dados do pagamento ISOGCB }
  MASK2_UsoRes_63                          = $40000000;  { BIT 63 - Projeto Vale Gás GetNet }
  MASK2_Numero_PDV                         = $80000000;  { Número do PDV - HSBC }

  {--------------------------------------------------------------------------------------------
     Constantes da máscara 3 da função ScopeObtemCampoExt2
  --------------------------------------------------------------------------------------------}
  MASK3_DadosQtdeECupons                   = $00000001;  { Informações sobre a quantidade e os e-cupons disponíveis ao cliente }
  MASK3_DescResgateMonetario               = $00000002;  { Desconto do resgate monetário }
  MASK3_Dados_Pagto_Bit48_BRADESCO         = $00000004;  { Bradesco - Informações sobre o Bit 48 }
  MASK3_Modo_Entrada                       = $00000008;  { Modo de entrada da transação (Entry Mode) }
  MASK3_Valor_Saque                        = $00000010;  { Valor do Saque }
  MASK3_Resposta_Consulta_Infocards        = $00000020;  { Resposta da consulta Infocards (bit 62 da 0110) }
  MASK3_Dados_Resposta_Consulta_EPAY_INCOMM= $00000040;  { Dados da resposta de Consulta da EPAY. Os dados retornados
                                                      consistem em 3 valores de concatenados:
                                                      1. Valor Mínimo ( 12 dígitos )
                                                      2. Valor Máximo ( 12 dígitos )
                                                      3. Saldo Disponível ( 12 dígitos )}
  MASK3_Dados_Resposta_Consulta_INCOMM     = $00000040;  { Dados da resposta de Consulta valor Gift Card (INCOMM)
                                                      Os dados retornados consistem em 3 valores de concatenados:
                                                      1. Valor Mínimo ( 12 dígitos )
                                                      2. Valor Máximo ( 12 dígitos )
                                                      3. Saldo Disponível ( 12 dígitos )}
  MASK3_Max_Mercadorias_TicketCar          = $00000100;  { Máximo de mercadorias permitidas para uma transação (Cartão TicketCar, Valecard, entre outras)
                                                     O dado retornado é um campo de 2 dígitos.}
  MASK3_Codigo_SAT                         = $00000200;  { Código SAT (ver códigos em Código das redes)}
  MASK3_Versao_Carga_Tabelas_Host          = $00000400;  { Versão corrente de Carga de Tabelas do Host Formato: 10 dígitos
                                                    (Preenchido com zeros a esquerda, caso necessário). Disponível em
                                                    transações com as seguintes Redes: SAVS}
  MASK3_CNPJ_Rede_Credenciadora_SAT        = $00002000;  { CNPJ da rede credenciadora - SAT }
  MASK3_Dados_Correspondente_Bancario      = $00008000;  { Dados do Correspondente Bancário }
  MASK3_Dados_Adicionais_Gift_Card         = $00010000;  { Dados Adicionais Gift Card:
                                                      - Para Incomm: código UPC11
                                                      - Para BlackHawk: código EAN12
                                                      - Para EPAY: código do produto com 8 dígitos, com brancos à direita se menor }
  MASK3_Dados_Operacao_Fidelidade_SGF      = $00020000;  { Dados retornados da Operação Fidelidade (SGF) }
  MASK3_Valor_Total_Pagamento              = $00040000;  { Valor Total do Pagamento }
  MASK3_Valor_Descontos_Pagamento          = $00080000;  { Valor de Descontos do Pagamento }
  MASK3_Valor_Entrada_IATA                 = $00800000;  { Valor de Entrada (IATA) }
  MASK3_Valor_Acrescimos_Pagamento         = $00100000;  { Valor de Acréscimos do Pagamento }
  MASK3_Dados_Perfil_Pagamento_Recorrente  = $01000000;  { Dados do Perfil de Pagamento Recorrente }
  MASK3_Dados_Assinatura_Pagamento         = $02000000;  { Dados da Assinatura do Pagamento Recorrente }
  MASK3_Dados_Consulta_BACEN               = $04000000;  { Dados Consulta BACEN – para títulos registrados }
  MASK3_Valor_Documento                    = $08000000;  { Valor Documento }
  MASK3_Resposta_Consulta_BACEN_Comprovante = $10000000; { Resposta Consulta BACEN – comprovante }
  MASK3_Modo_Pagamento                     = $20000000;  { Modo Pagamento:
                                                      ‘00’ – não informado
                                                      '01' – cheque (Utilizado para pagamento de contas)
                                                      '02' – dinheiro (Utilizado para pagamento de contas)
                                                      '03' – debito em conta (Utilizado para carteiras virtuais/pagamento de contas)
                                                      '04' – cartao credito (Utilizado para carteiras virtuais)
                                                      '05' – pix (Utilizado para carteiras virtuais)
                                                      ‘06’ – cartao de debito (Utilizado para carteiras virtuais)
                                                      ‘07’ – saldo + cartao (Utilizado para carteiras virtuais) }
  MASK3_Consulta_Cedente_BACEN_BRADESCO    = $40000000;  { Consulta Cedente - Dados da Consulta BACEN BRADESCO }
  MASK3_Data_Vencimento_CORBAN             = $80000000;  { Data Vencimento CORBAN – do título/conta }

  {--------------------------------------------------------------------------------------------
     Constantes da máscara 4 da função ScopeObtemCampoExt3
  --------------------------------------------------------------------------------------------}
  MASK4_Nome_Portador_Cartao           = $00000001;  { Nome do Portador do Cartão (Informação com até 26 caracteres) }
  MASK4_Data_Validade_Cartao           = $00000002;  { Data de Validade do Cartão (YYMMDD) }
  MASK4_Merchant_ID                    = $00000004;  { Merchant ID (informação com até 32 caracteres) }
  MASK4_Codigo_Estab_Externo           = $00000008;  { Código do Estabelecimento Externo (informação com até 15 caracteres) }
  MASK4_String_QRCode                  = $00000020;  { String para gerar o QRCode }
  MASK4_Relacao_Descontos_Item         = $00000080;  { Relação de Descontos por Item, recebidos da Ticket Log no bit 54 da 0210 }
  MASK4_Indicador_Saldo_Disponivel     = $00000100;  { Informa se o Saldo_Disponivel está em [0] = Reais (default) ou [1] = Litros }
  MASK4_Numero_CPF                     = $00000200;  { Número do CPF }
  MASK4_ARQC_Chip                      = $00000400;  { ARQC do chip, se disponibilizado pelo cartão }
  MASK4_AID_Chip                       = $00000800;  { AID do chip, se disponibilizado pelo cartão }
  MASK4_Transacao_Autorizada_Por_Senha = $00001000;  { Indicação se a transação foi autorizada mediante uso de senha pessoal [1] = Sim, [0] = Não }
  MASK4_Ind_Versao_Especificacao       = $00002000;  { Índice da versão de especificação a ser devolvido para Automação Comercial }
  MASK4_Campo_TID_Pix                  = $00004000;  { campo TID da tabela Mensagem (do Banco de Dados) - transações pix (txid) }
  MASK4_Campo_Referencia_Pix           = $00008000;  { campo Referência da tabela Mensagem (do Banco de Dados) - transações pix (end2endId) }
  MASK4_Tamanho_BIN                    = $00010000;  { Tamanho do BIN - PR069279}
  MASK4_Dados_DCC                      = $00020000;  { Estrutura de strings finalizadas com null:
                                                  Dado Tamanho + null Obs
                                                  Valor Convertido 12 + 1
                                                  Cotação de Conversão 8 + 1
                                                  Taxa Markup 5 + 1 %
                                                  Sigla da Moeda Estrangeira 3 + 1 ISO 4217
                                                  Código da Moeda Estrangeira 3 + 1 ISO 4217
                                                  Pode ser usada a estrutura stDadosDCC definida em scopeapi.h }
  MASK4_Status_DCC                      = $00040000;  { Status DCC:
                                                  ‘0’ = Não Realizado
                                                  ‘1’ = Cliente Não Aceitou
                                                  ‘2’ = Cliente Aceitou
                                                  ‘3’ = Não Elegível
                                                  ‘4’ = Erro de Comunicação
                                                  Qualquer outro valor = Desconhecido }

  // Identificacao das redes
  R_GWCEL = 90;

  // Tamanhos da Estrutura TRec_Cel_Valores
  TAM_NOME_OP     = 21;
  TAM_VALOR_OP    = 12;
  NUM_VLRS_RC     = 10;
  TAM_MSG_PROM_OP = 41;

{------------------------------------------------------------------------------
 DECLARACAO DAS ESTRUTURAS
------------------------------------------------------------------------------}
type
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

  //** dados utlizados na coleta de parametros Extendida */
  PStDadosExt = ^TDadosExt;
  TDadosExt = packed record
    Sigla: array [1..3] of AnsiChar;    // Identificacao da coleta extendida, se existir
    Rotulo: array [1..41] of AnsiChar;  // Rotulo a ser exibido
    AceitaVazio: Byte;                  // Aceita ou não dado vazio
    QtdCasasDecimais: Byte;             // qtd de casas decimais, se pertinente
    TamMin: array [1..2] of AnsiChar;   // Tamanho minimo do campo
    TamMax: array [1..2] of AnsiChar;   // Tamanho maximo do campo
  end;

  PDadosLimite = ^TDadosLimite;
  TDadosLimite = packed record
    Inferior: array [1..12] of AnsiChar;  // Limite inferior (uso futuro)
    Superior: array [1..12] of AnsiChar;  // Limite superior (uso futuro)
  end;

  PParam_Coleta_Ext = ^TParam_Coleta_Ext;
  TParam_Coleta_Ext = packed record
    FormatoDado: Byte;
    HabTeclas: Byte;
    CodBandeira: array [1..3] of AnsiChar;
    CodRede: array [1..3] of AnsiChar;
    MsgOp1: array [1..64]  of AnsiChar;
    MsgOp2: array [1..64]  of AnsiChar;
    MsgCl1: array [1..64]  of AnsiChar;
    MsgCl2: array [1..64]  of AnsiChar;
    UsaExt: Byte;                         // Indica se deve ou não usar os campos sExt abaixo
    Ext: TDadosExt;                       // Dados extendidos
    UsaLimites: Byte;                     // Indica se deve ou não usar os campos sLimite abaixo
    Limite: TDadosLimite;                 // Limites inferior e superior: uso futuro
    IdColetaExt: Word;                    // Identificacao da coleta estendida
    Reservado: array [1..97]  of AnsiChar;
  end;


  //** Estruturas devolvida pela funcao ScopeMenuRecuperaItens */
  TScopeMenuCieloItem = packed record
    CodFuncao: array [1..5] of AnsiChar;
    Descricao: array [1..41] of AnsiChar;
    CodGrupoServico: array [1..3] of AnsiChar;
    CodFluxoPDV: array [1..4] of AnsiChar;

    CodRede: array [1..4] of AnsiChar;
    CodBandeira: array [1..4] of AnsiChar;
    CodFuncaoRede: array [1..5] of AnsiChar;
  end;

  TScopeMenuCielo = packed record
    TipoTabela: AnsiChar; // MNU_TAB_TIPO_GENERICO = 2
    QtdItens: Byte;
    Itens: array [1..QTD_MAX_ITENS_PERG_GEN_SEL] of TScopeMenuCieloItem;
  end;

  TScopeMenuGenerico = packed record
    TipoTabela: AnsiChar;  // MNU_TAB_TIPO_GENERICO_DINAMICO = 4
    Itens: array [1..QTD_MAX_ITENS_PERG_GEN_SEL] of array [1..41] of AnsiChar; // Máximo 15 itens em formato "string" de no máximo 40 caracteres
  end;

  TScopeMenuDinamico = packed record
    TipoTabela: AnsiChar; // MNU_TAB_TIPO_GENERICO = 2
    QtdItens: Byte;
    ItemGenerico: TScopeMenuGenerico;
  end;

  TScopeMenuEspecialItem = packed record
    Sigla: array [1..3] of AnsiChar;   // Sigla da seleção (informativo)
    Rotulo: array [1..41] of AnsiChar; // Rótulo da seleção (para display)
  end;

  TScopeMenuEspecial = packed record
    QtdItens: array [1..2] of AnsiChar;   // // Qtd de itens de seleção
    Itens: array [1..QTD_MAX_ITENS_PERG_DIN_SEL] of TScopeMenuEspecialItem;
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
    NumOperCel:   SmallInt;
    OperCel:      array [1..2000] of AnsiChar;
  end;

  //** Lista de Operadoras de Recarga de Celular retornadas pelo Servidor */
  PRec_Cel_ID_OperM3 = ^TRec_Cel_ID_OperM3;
  TRec_Cel_ID_OperM3 = packed record
    CodOperCel:   SmallInt;
    NomeOperCel:  array [1..TAM_NOME_OP] of AnsiChar;
  end;

  //** Formato do valor para Recarga de Celular */
  PRec_Cel_Valor = ^TRec_Cel_Valor;
  TRec_Cel_Valor = packed record
    Valor: array [1..TAM_VALOR_OP] of AnsiChar;
    Bonus: array [1..TAM_VALOR_OP] of AnsiChar;
    Custo: array [1..TAM_VALOR_OP] of AnsiChar;
  end;

  TRec_Cel_Faixa_Valores = packed record
    ValorMin: array [1..TAM_VALOR_OP] of AnsiChar;
    ValorMax: array [1..TAM_VALOR_OP] of AnsiChar;
  end;

  //** Lista de Valores de Recarga de Celular retornadas pelo Servidor */
  PRec_Cel_Valores = ^TRec_Cel_Valores;
  TRec_Cel_Valores = packed record
    TipoValor:      AnsiChar;               { Tipo dos valores
                                              'V' - variavel(val min e val maximo)
                                              'F' - Fixo (apenas um valor fixo)
                                              'T' - Todos (tabela de valores) }
    ValorMinimo:    array [1..TAM_VALOR_OP] of AnsiChar;
    ValorMaximo:    array [1..TAM_VALOR_OP] of AnsiChar;
    TotValor:       Byte;
    TabValores:     array [1..NUM_VLRS_RC] of TRec_Cel_Valor;
    MsgPromocional: array [1..TAM_MSG_PROM_OP] of AnsiChar;
    TotFaixaValores: Byte;
    TabFaixaValores:array [1..NUM_VLRS_RC] of TRec_Cel_Faixa_Valores;
  end;

type
  EACBrTEFScopeAPI = class(Exception);

  TACBrTEFScopeOperacao = ( scoNone, scoMenu,
                            scoCredito, scoDebito, scoPagto,
                            scoConsCDC, scoCheque, scoCanc,
                            scoReimpComp, scoResVenda, scoRecargaCel,
                            scoPreAutCredito );

  TACBrTEFScopeGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TACBrTEFScopeTerminalMensagem = (tmTodas, tmOperador, tmCliente);

  TACBrTEFScopeExibeMensagem = procedure(
    const Mensagem: String;
    Terminal: TACBrTEFScopeTerminalMensagem;
    MilissegundosExibicao: Integer  // 0 - Para com OK; Positivo - aguarda Ok ou N milissegundos; Negativo - Apenas exibe a Msg (não aguarda)
    ) of object;

  TACBrTEFScopePerguntarMenu = procedure(
    const Titulo: String;
    Opcoes: TStringList;
    var ItemSelecionado: Integer) of object ;  // -1 = Cancelado

  TACBrTEFScopePerguntarCampo = procedure(
    const TituloCampo: String;
    const Param_Coleta_Ext: TParam_Coleta_Ext;
    var Resposta: String;
    var AcaoResposta: Byte) of object ;

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
    fIntervaloColeta: Integer;
    fMsgPinPad: String;
    fDadosDaTransacao: TStringList;
    fRespostasPorEstados: TStringList;
    fOnExibeMensagem: TACBrTEFScopeExibeMensagem;
    fOnPerguntarMenu: TACBrTEFScopePerguntarMenu;
    fOnGravarLog: TACBrTEFScopeGravarLog;
    fOnPerguntaCampo: TACBrTEFScopePerguntarCampo;
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
    xScopeGetParamExt: function (_TipoParam: LongInt; _lpParam: PParam_Coleta_Ext): LongInt;
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
    xScopeReimpressaoOffLine: function(): LongInt;
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
    xScopePagamentoConta: function(_Servico: Word): LongInt;
        {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeRecargaCelular: function(): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePreAutorizacaoCredito: function(_Valor, _TxServico: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeRecuperaOperadorasRecCel: function(_TipoTabela: Byte; rListaOperadoras: PRec_Cel_Oper;
      _TamBuffer: Word): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeRecuperaValoresRecCel: function(_TipoTabela: Byte; rCelValores: PRec_Cel_Valores;
      _TamBuffer: Word): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeConfigura: function(_Id, _Param: LongInt): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeValidaInterfacePP: function(IntPP: Byte): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeConsultaPP: function(Configurado, UsoExclusivoScope, Porta: PByte): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeMenuRecuperaItens: function(_TipoTabela: Byte; _Buffer: PAnsiChar;
      _TamBuffer: Word): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeMenuSelecionaItem: function(_Item: Byte): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPOpenSecure: function(TipoCanal: word; Endereco: PAnsiChar): LongInt
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPClose: function(IdleMsg: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPGetCOMPort: function(szComEndereco: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPStartGetData: function(Dado: Word; TamMin: Word; TamMax: Word): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPGetData: function(LenDados: Word; pDados: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPAbort: function(): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPStartOptionMenu: function(Titulo: PAnsiChar; Lista: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPOptionMenu: function(Indice: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeMenu: function(_UsoFuturo: LongInt): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPDisplay: function(Msg: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPOpen: function(Porta: Word): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopeObtemConsultaValeGas: function(_Valor: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    function PAnsiCharToString(APAnsiChar: PAnsiChar): String;
    function ArrayOfCharToString(Arr: array of AnsiChar): String;

    procedure SetIntervaloColeta(AValue: Integer);
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
    procedure ChamarEventoTransacaoEmAndamento(EstadoOperacao: TACBrTEFScopeEstadoOperacao;
      out Cancelar: Boolean);
    procedure SetVersaoAutomacao(AValue: String);

  protected
    function GetLibFullPath: String;
    function GetScopeIniFullPath: String;

    procedure LoadLibFunctions;
    procedure UnLoadLibFunctions;
    procedure ClearMethodPointers;

    procedure DoException(const AErrorMsg: String );
    procedure TratarErroScope(AErrorCode: LongInt);
    procedure TratarErroPinPadScope(AErrorCode: LongInt);

    procedure AbrirComunicacaoScope;
    procedure FecharComunicacaoScope;

    procedure VerificarSeEstaConectadoScope;
    procedure VerificarSeMantemConexaoScope;
    procedure VerificarSessaoTEFAnterior;

    procedure AbrirPinPad;
    procedure FecharPinPad;
    procedure ConfigurarColeta;
    function ConfigurarPropriedadeScope(AId: LongInt; Ligado: Boolean): Boolean;

    procedure VerificarDiretorioDeTrabalho;
    procedure VerificarEAjustarScopeINI;
    function ConfigurarPortaPinPad(const APortaPinPad: String): Word;
    procedure ObterDadosScopeINI(out AEmpresa: String; out AFilial: String;
      out AEnderecoIP: String; out APortaTCP: String);

    procedure ExibirMensagem(const AMsg: String;
      Terminal: TACBrTEFScopeTerminalMensagem = tmOperador; TempoEspera: Integer = -1);

    function ObterScopeStatus: Longint;
    procedure ObterDadosComprovantes;
    procedure ColetarValoresConsulta;

    procedure ObterDadosCheque;
    function ObterHandleScope(TipoHandle: LongInt): LongInt;
    procedure ObterDadosDaTransacao;
    function ObterCampoMask1(AMask: LongInt): String;

    procedure ExibirErroUltimaMsg;
    procedure LogColeta(AColeta: TParam_Coleta);
    procedure LogColetaEx(AColetaEx: TParam_Coleta_Ext);

    procedure PerguntarMenuScope(const TipoMenu: Byte; var Resposta: String; var Acao: Byte);
    procedure PerguntarSimNao(const rColetaEx: TParam_Coleta_Ext; var Resposta: String; var Acao: Byte);
    procedure PerguntarMenuOperadora(var rColetaEx: TParam_Coleta_Ext; var Resposta: String; var Acao: Byte);
    procedure PerguntarMenuValorRecargaCel(var rColetaEx: TParam_Coleta_Ext; var Resposta: String; var Acao: Byte);

    procedure ColetarParametrosScope(const iStatus: Word; var rColetaEx: TParam_Coleta_Ext);
    procedure ExibirMsgColeta(const rColetaEx: TParam_Coleta_Ext);
    procedure AssignColetaToColetaEx(const rColeta: TParam_Coleta; var rColetaEx: TParam_Coleta_Ext);

    function MsgOperador(const rColetaEx: TParam_Coleta_Ext): String;
    function MsgCliente(const rColetaEx: TParam_Coleta_Ext): String;
    function FormatarMsgPinPad(const MsgPinPad: String): String;
  public
    constructor Create;
    destructor Destroy; override;

    property PathLib: String read fPathLib write SetPathLib;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
    property ControleConexao: Boolean read fControleConexao write SetControleConexao default False;

    property Empresa: String read fEmpresa write SetEmpresa;
    property Filial: String read fFilial write SetFilial;
    property PDV: String read fPDV write SetPDV;
    property EnderecoIP: String  read fEnderecoIP write SetEnderecoIP;
    property PortaTCP: String read fPortaTCP write SetPortaTCP;
    property IntervaloColeta: Integer read fIntervaloColeta write SetIntervaloColeta default CINTERVALO_COLETA;

    property DadosDaTransacao: TStringList read fDadosDaTransacao;
    property RespostasPorEstados: TStringList read fRespostasPorEstados;

    property PortaPinPad: String read fPortaPinPad write fPortaPinPad;
    property MsgPinPad: String read fMsgPinPad write fMsgPinPad;
    property VersaoAutomacao: String read fVersaoAutomacao write SetVersaoAutomacao;
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
    property OnExibeMenu: TACBrTEFScopePerguntarMenu read fOnPerguntarMenu
      write fOnPerguntarMenu;
    property OnPerguntaCampo: TACBrTEFScopePerguntarCampo read fOnPerguntaCampo
      write fOnPerguntaCampo;
    property OnTransacaoEmAndamento: TACBrTEFScopeTransacaoEmAndamento read fOnTransacaoEmAndamento
      write fOnTransacaoEmAndamento;

    property OnGravarLog: TACBrTEFScopeGravarLog read fOnGravarLog write fOnGravarLog;

    procedure Inicializar;
    procedure DesInicializar;

    procedure AbrirSessaoTEF;
    procedure FecharSessaoTEF; overload;
    procedure FecharSessaoTEF(Confirmar: Boolean; out TransacaoFoiDesfeita: Boolean); overload;

    procedure IniciarTransacao(Operacao: TACBrTEFScopeOperacao;
      const Param1: String = ''; const Param2: String = ''; const Param3: String = '');
    procedure ExecutarTransacao;
    function EnviarParametroTransacao(Acao: LongInt; codTipoColeta: LongInt = -1;
      Dados: AnsiString = ''; dadosParam: Word = 0): LongInt;
    procedure AbortarTransacao;

    function ObterVersaoScope: String;
    function AcharPortaPinPad: String;
    procedure ExibirMensagemPinPad(const MsgPinPad: String);
    function ObterDadoPinPad(Dado: Word; MinLen, MaxLen: Word;
      TimeOutMiliSec: Integer = 30000): String;
    function MenuPinPad(const Titulo: String; Opcoes: TStrings;
      TimeOutMiliSec: Integer = 30000): Integer;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);
  end;

implementation

uses
  IniFiles, StrUtils, TypInfo, Math, DateUtils,
  ACBrUtil.Strings,
  ACBrUtil.Math,
  ACBrUtil.FilesIO;

{ TACBrTEFScopeAPI }

constructor TACBrTEFScopeAPI.Create;
begin
  inherited;
  fCarregada := False;
  fInicializada := False;
  fConectado := False;
  fSessaoAberta := False;
  fControleConexao := False;
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
  fIntervaloColeta := CINTERVALO_COLETA;
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnPerguntarMenu := Nil;
  fOnPerguntaCampo := Nil;
  fOnTransacaoEmAndamento := Nil;
  fDadosDaTransacao := TStringList.Create;
  fRespostasPorEstados := TStringList.Create;
end;

destructor TACBrTEFScopeAPI.Destroy;
begin
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  DesInicializar;
  fDadosDaTransacao.Free;
  fRespostasPorEstados.Free;

  inherited Destroy;
end;

procedure TACBrTEFScopeAPI.Inicializar;
begin
  if fInicializada then
    Exit;

  fConectado := False;
  fSessaoAberta := False;
  GravarLog('TACBrTEFScopeAPI.Inicializar');

  if not Assigned(fOnTransacaoEmAndamento) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnTransacaoEmAndamento']));
  if not Assigned(fOnPerguntarMenu) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnExibeMenu']));
  if not Assigned(fOnPerguntaCampo) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnPerguntaCampo']));
  if not Assigned(fOnExibeMensagem) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnExibeMensagem']));

  VerificarDiretorioDeTrabalho;
  VerificarEAjustarScopeINI;
  LoadLibFunctions;

  try
    AbrirPinPad;   // Chama AbrirComunicacaoScope
  except
    FecharComunicacaoScope;
    raise;
  end;

  fInicializada := True;
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
    DoException(sErrLibJaInicializda);

  fPathLib := PathWithDelim(ExtractFilePath(AValue));
end;

function TACBrTEFScopeAPI.PAnsiCharToString(APAnsiChar: PAnsiChar): String;
begin
  Result := TrimRight(String(APAnsiChar));
end;

function TACBrTEFScopeAPI.ArrayOfCharToString(Arr: array of AnsiChar): String;
begin
  Result := TrimRight(String(Arr));
end;

procedure TACBrTEFScopeAPI.SetIntervaloColeta(AValue: Integer);
begin
  if fIntervaloColeta = AValue then
    Exit;

  fIntervaloColeta := max(AValue, CINTERVALO_COLETA);
end;

procedure TACBrTEFScopeAPI.SetDiretorioTrabalho(AValue: String);
begin
  if fDiretorioTrabalho = AValue then
    Exit;

  GravarLog('TACBrTEFScopeAPI.SetDiretorioTrabalho( '+AValue+' )');

  if fInicializada then
    DoException(sErrLibJaInicializda);

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
    DoException(sErrLibJaInicializda);

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
    DoException(sErrLibJaInicializda);

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
    DoException(sErrLibJaInicializda);

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
    DoException(sErrLibJaInicializda);

  fControleConexao := AValue;
end;

procedure TACBrTEFScopeAPI.SetEnderecoIP(AValue: String);
begin
  if fEnderecoIP = AValue then
    Exit;

  if fInicializada then
    DoException(sErrLibJaInicializda);

  fEnderecoIP := Trim(AValue);
end;


procedure TACBrTEFScopeAPI.SetPortaTCP(AValue: String);
begin
  if fPortaTCP = AValue then
    Exit;

  if fInicializada then
    DoException(sErrLibJaInicializda);

  fPortaTCP := Trim(AValue);
end;

procedure TACBrTEFScopeAPI.SetEmTransacao(AValue: Boolean);
begin
  if (not fInicializada) or (AValue = fEmTransacao) then
    Exit;

  fEmTransacao := AValue;
  //if not fEmTransacao then
  //  ExibirMensagem('');
end;

procedure TACBrTEFScopeAPI.ChamarEventoTransacaoEmAndamento(
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

procedure TACBrTEFScopeAPI.SetVersaoAutomacao(AValue: String);
var
  s: String;
  l: Integer;
  ok: Boolean;
begin
  if fVersaoAutomacao = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') then
  begin
    s := UpperCase(copy(AValue,1,10));
    l := Length(s);
    ok := (l = 10) and
          StrIsNumber(copy(s,1,2)) and
          StrIsAlphaNum(copy(s,3,4)) and
          StrIsNumber(copy(s,7,4));

    if not ok then
      DoException(sErrVersaoAutomacaoInvalido);
  end;

  fVersaoAutomacao := s;
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
          DoException(Format('Erro ao carregar a função: %s de: %s',[FuncName, LibName]))
        else
          GravarLog(Format('     Função não requerida: %s não encontrada em: %s',[FuncName, LibName]));
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
  ScopeFunctionDetect(sLibName, 'ScopeGetParamExt', @xScopeGetParamExt, False);
  ScopeFunctionDetect(sLibName, 'ScopeResumeParam', @xScopeResumeParam);
  ScopeFunctionDetect(sLibName, 'ScopeGetLastMsg', @xScopeGetLastMsg);
  ScopeFunctionDetect(sLibName, 'ScopeGetCheque', @xScopeGetCheque);
  ScopeFunctionDetect(sLibName, 'ScopeGetCupomEx', @xScopeGetCupomEx);
  ScopeFunctionDetect(sLibName, 'ScopeFechaSessaoTEF', @xScopeFechaSessaoTEF);
  ScopeFunctionDetect(sLibName, 'ScopeConsultaCDC', @xScopeConsultaCDC);
  ScopeFunctionDetect(sLibName, 'ScopeCompraCartaoDebito', @xScopeCompraCartaoDebito);
  ScopeFunctionDetect(sLibName, 'ScopeConsultaCheque', @xScopeConsultaCheque);
  ScopeFunctionDetect(sLibName, 'ScopeCancelamento', @xScopeCancelamento);
  ScopeFunctionDetect(sLibName, 'ScopeReimpressaoOffLine', @xScopeReimpressaoOffLine);
  ScopeFunctionDetect(sLibName, 'ScopeResumoVendas', @xScopeResumoVendas);
  ScopeFunctionDetect(sLibName, 'ScopeObtemCampoExt', @xScopeObtemCampoExt);
  ScopeFunctionDetect(sLibName, 'ScopeObtemCampoExt2', @xScopeObtemCampoExt2);
  ScopeFunctionDetect(sLibName, 'ScopeObtemCampoExt3', @xScopeObtemCampoExt3);
  ScopeFunctionDetect(sLibName, 'ScopeObtemHandle', @xScopeObtemHandle);
  ScopeFunctionDetect(sLibName, 'ScopePagamento', @xScopePagamento);
  ScopeFunctionDetect(sLibName, 'ScopePagamentoConta', @xScopePagamentoConta);
  ScopeFunctionDetect(sLibName, 'ScopeRecargaCelular', @xScopeRecargaCelular);
  ScopeFunctionDetect(sLibName, 'ScopePreAutorizacaoCredito', @xScopePreAutorizacaoCredito);
  ScopeFunctionDetect(sLibName, 'ScopeRecuperaOperadorasRecCel', @xScopeRecuperaOperadorasRecCel);
  ScopeFunctionDetect(sLibName, 'ScopeRecuperaValoresRecCel', @xScopeRecuperaValoresRecCel);
  ScopeFunctionDetect(sLibName, 'ScopeConfigura', @xScopeConfigura);
  ScopeFunctionDetect(sLibName, 'ScopeValidaInterfacePP', @xScopeValidaInterfacePP);
  ScopeFunctionDetect(sLibName, 'ScopeConsultaPP', @xScopeConsultaPP);
  ScopeFunctionDetect(sLibName, 'ScopeMenuRecuperaItens', @xScopeMenuRecuperaItens);
  ScopeFunctionDetect(sLibName, 'ScopeMenuSelecionaItem', @xScopeMenuSelecionaItem);
  ScopeFunctionDetect(sLibName, 'ScopePPOpen', @xScopePPOpen);
  ScopeFunctionDetect(sLibName, 'ScopePPOpenSecure', @xScopePPOpenSecure);
  ScopeFunctionDetect(sLibName, 'ScopePPClose', @xScopePPClose);
  ScopeFunctionDetect(sLibName, 'ScopePPGetCOMPort', @xScopePPGetCOMPort);
  ScopeFunctionDetect(sLibName, 'ScopePPStartGetData', @xScopePPStartGetData);
  ScopeFunctionDetect(sLibName, 'ScopePPGetData', @xScopePPGetData);
  ScopeFunctionDetect(sLibName, 'ScopePPAbort', @xScopePPAbort);
  ScopeFunctionDetect(sLibName, 'ScopePPStartOptionMenu', @xScopePPStartOptionMenu);
  ScopeFunctionDetect(sLibName, 'ScopePPOptionMenu', @xScopePPOptionMenu);
  ScopeFunctionDetect(sLibName, 'ScopePPDisplay', @xScopePPDisplay);
  ScopeFunctionDetect(sLibName, 'ScopeMenu', @xScopeMenu);
  ScopeFunctionDetect(sLibName, 'ScopeObtemConsultaValeGas', @xScopeObtemConsultaValeGas);

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
  xScopeGetParamExt := Nil;
  xScopeResumeParam := Nil;
  xScopeGetLastMsg := Nil;
  xScopeGetCheque := Nil;
  xScopeGetCupomEx := Nil;
  xScopeFechaSessaoTEF := Nil;
  xScopeConsultaCDC := Nil;
  xScopeCompraCartaoDebito := Nil;
  xScopeConsultaCheque := Nil;
  xScopeCancelamento := Nil;
  xScopeReimpressaoOffLine := Nil;
  xScopeResumoVendas := Nil;
  xScopeObtemCampoExt := Nil;
  xScopeObtemCampoExt2 := Nil;
  xScopeObtemCampoExt3 := Nil;
  xScopeObtemHandle := Nil;
  xScopePagamento := Nil;
  xScopePagamentoConta := Nil;
  xScopeRecargaCelular := Nil;
  xScopePreAutorizacaoCredito := Nil;
  xScopeRecuperaOperadorasRecCel := Nil;
  xScopeRecuperaValoresRecCel := Nil;
  xScopeConfigura := Nil;
  xScopeValidaInterfacePP := Nil;
  xScopeConsultaPP := Nil;
  xScopeMenuRecuperaItens := Nil;
  xScopeMenuSelecionaItem := Nil;
  xScopePPOpen := Nil;
  xScopePPOpenSecure := Nil;
  xScopePPClose := Nil;
  xScopePPGetCOMPort := Nil;
  xScopePPStartGetData := Nil;
  xScopePPGetData := Nil;
  xScopePPAbort := Nil;
  xScopePPStartOptionMenu := Nil;
  xScopePPOptionMenu := Nil;
  xScopePPDisplay := Nil;
  xScopeObtemConsultaValeGas := Nil;
end;

procedure TACBrTEFScopeAPI.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('TACBrTEFScopeAPI: '+AErrorMsg);
  raise EACBrTEFScopeAPI.Create(ACBrStr(AErrorMsg));
end;

procedure TACBrTEFScopeAPI.VerificarDiretorioDeTrabalho;
begin
  if (fDiretorioTrabalho = '') then
    fDiretorioTrabalho := ApplicationPath + 'TEF' + PathDelim + 'ScopeAPI';

  if not DirectoryExists(fDiretorioTrabalho) then
    ForceDirectories(fDiretorioTrabalho);

  if not DirectoryExists(fDiretorioTrabalho) then
    DoException(Format(sErrDirTrabalhoInvalido, [fDiretorioTrabalho]));
end;

procedure TACBrTEFScopeAPI.VerificarEAjustarScopeINI;
var
  ini: TMemIniFile;
  sl: TStringList;
  i: Integer;
  sPathScopeIni, SecName, sEmpresa, sFilial, sName, sPort: String;
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
              DoException(sErrEndServNaoEncontrado);

          if (fPortaTCP <> '') then
            ini.WriteString(SecName, 'Port', fPortaTCP)
          else
            AjustarParamSeNaoExistir(SecName, 'Port', '2046');
        end;

        AjustarParamSeNaoExistir(SecName, 'TimeOutAdm', '120');
        if (fVersaoAutomacao <> '') then
          sName := fVersaoAutomacao
        else
          sName := '01ACBR0000';

        AjustarParamSeNaoExistir(SecName, 'VersaoAutomacao', sName);
        AjustarParamSeNaoExistir(SecName, 'CupomReduzido', IfThen(fCupomReduzido, 's', 'n'));
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
        DoException(sErrEndServNaoInformado)
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
      ini.WriteString(SecName, 'WKPAN', IfThen(fPinPadSeguro, 's', 'n'));
    end;

    AjustarParamSeNaoExistir('PINDPAD', 'TamMinDados', '4');
    AjustarParamSeNaoExistir('PPCOMP', 'NaoAbrirDigitado', IfThen(fPermitirCartaoDigitado, 'n', 's'));

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
  fOnExibeMensagem(ACBrStr(AMsg), Terminal, TempoEspera);
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
    if (ret = RCS_SUCESSO) then
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
    if (ret = PC_OK) then
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
  s: String;
begin
  s := FormatarMsgPinPad(MsgPinPad);
  GravarLog('ScopePPDisplay( '+s+' )');
  ret := xScopePPDisplay(PAnsiChar(s));
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> PC_OK) then
    TratarErroPinPadScope(ret);
end;

function TACBrTEFScopeAPI.ObterDadoPinPad(Dado: Word; MinLen, MaxLen: Word;
  TimeOutMiliSec: Integer): String;
var
  ret: LongInt;
  pBuffer: PAnsiChar;
  tf: TDateTime;
const
  BUFFER_SIZE = 1024;
begin
  Result := '';
  GravarLog('ScopePPStartGetData( '+IntToStr(Dado)+', '+IntToStr(MinLen)+', '+IntToStr(MaxLen)+' )');
  ret := xScopePPStartGetData(Dado, MinLen, MaxLen);
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> PC_OK) then
    TratarErroPinPadScope(ret);

  PBuffer := AllocMem(BUFFER_SIZE);
  try
    ret := PC_PROCESSING;
    tf := IncMilliSecond(now, TimeOutMiliSec);
    while (ret = PC_PROCESSING) and (now < tf) do
    begin
      GravarLog('ScopePPGetData( '+IntToStr(BUFFER_SIZE)+' )');
      ret := xScopePPGetData(BUFFER_SIZE, pBuffer);
      GravarLog('  ret: '+IntToStr(ret));
      Sleep(fIntervaloColeta);
    end;

    if (ret <> PC_OK) then
    begin
      if (ret = PC_PROCESSING) and (now > tf) then
      begin
        GravarLog('ScopePPAbort');
        ret := xScopePPAbort;
        GravarLog('  ret: '+IntToStr(ret));
        if (ret <> PC_OK) then
          TratarErroPinPadScope(ret);

        ret := PC_TIMEOUT;
      end;

      if (ret <> PC_CANCEL) then
        TratarErroPinPadScope(ret);
    end;

    if (ret = PC_OK) then
      Result := PAnsiCharToString(pBuffer);
  finally
    Freemem(pBuffer);
  end;
end;

function TACBrTEFScopeAPI.MenuPinPad(const Titulo: String; Opcoes: TStrings;
  TimeOutMiliSec: Integer): Integer;
var
  ret, i: LongInt;
  pBuffer: PAnsiChar;
  tf: TDateTime;
  Lin, Lista: AnsiString;
const
  BUFFER_SIZE = 10;
begin
  Result := -1;
  Lista := '';
  for i := 0 to Opcoes.Count-1 do
  begin
    Lin := copy(Opcoes[i],1,24);
    Lista := Lista + Format('%.2d',[Length(Lin)]) + Lin;
  end;

  GravarLog('ScopePPStartOptionMenu( '+Titulo+', '+Lista+' )');
  ret := xScopePPStartOptionMenu(PAnsiChar(Titulo), PAnsiChar(Lista));
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> PC_OK) then
    TratarErroPinPadScope(ret);

  PBuffer := AllocMem(BUFFER_SIZE);
  try
    ret := PC_PROCESSING;
    tf := IncMilliSecond(now, TimeOutMiliSec);
    while (ret = PC_PROCESSING) and (now < tf) do
    begin
      GravarLog('ScopePPOptionMenu( '+IntToStr(BUFFER_SIZE)+' )');
      ret := xScopePPOptionMenu(pBuffer);
      GravarLog('  ret: '+IntToStr(ret));
      Sleep(fIntervaloColeta);
    end;

    if (ret <> PC_OK) then
    begin
      if (ret = PC_PROCESSING) and (now > tf) then
      begin
        GravarLog('ScopePPAbort');
        ret := xScopePPAbort;
        GravarLog('  ret: '+IntToStr(ret));
        if (ret <> PC_OK) then
          TratarErroPinPadScope(ret);

        ret := PC_TIMEOUT;
      end;

      if (ret <> PC_CANCEL) then
        TratarErroPinPadScope(ret);
    end;

    if (ret = PC_OK) then
      Result := StrToIntDef(String(pBuffer), -1);
  finally
    Freemem(pBuffer);
  end;
end;

procedure TACBrTEFScopeAPI.TratarErroScope(AErrorCode: LongInt);
var
  MsgErro: String;
begin
  case AErrorCode of
    RCS_SUCESSO: MsgErro := '';
    RCS_TRN_EM_ANDAMENTO: MsgErro := ''; //'Transação em andamento';
    RCS_ERRO_PARM_1: MsgErro := 'Parâmetro 1 inválido';
    RCS_ERRO_PARM_2: MsgErro := 'Parâmetro 2 inválido';
    RCS_ERRO_PARM_3: MsgErro := 'Parâmetro 3 inválido';
    RCS_ERRO_PARM_4: MsgErro := 'Parâmetro 4 inválido';
    RCS_ERRO_ARQ_CICLO_TEF: MsgErro := 'Erro no arquivo de controle, finalização multi-TEF';
    RCS_API_NAO_INICIALIZADA: MsgErro := 'SCOPE API não foi inicializada';
    RCS_API_JA_INICIALIZADA: MsgErro := 'SCOPE API já foi inicializada';
    RCS_SRV_NOT_CFG: MsgErro := 'Servidor não configurado no arquivo '+CScopeINi;
    RCS_ERRO_LOGON_PDV: MsgErro := 'Verificar o erro retornado no log do ScopeSrv';
    RCS_REDE_LISTA_PRIOR_AID_INDISPONIVEL: MsgErro := 'Verifique a configuração do perfil do PDV';
    RCS_ERRO_NUM_MAX_TEF_SESSAO: MsgErro := 'Estourou o número máximo de TEF numa sessão multi-TEF';
    RCS_NAO_HA_CAMPOS_SALVOS: MsgErro := 'Não há arquivo com dados da transação anterior salvo';
    RCS_CANCELADA_PELO_OPERADOR: MsgErro := 'Transação cancelada pelo operador';
    RCS_PP_COMPARTILHADO_NAO_CONFIGURADO: MsgErro := 'PIN-Pad compartilhado não está configurado, mas a rede exige que seja compartilhado.';
    RCS_AREA_RESERVADA_INSUFICIENTE: MsgErro := 'Área reservada para o buffer é insuficiente para o SCOPE Client preencher com os dados solicitados';
    RCS_NOT_FOUND: MsgErro := 'Não Encontrado';
  else
    MsgErro := Format('Erro: %d', [AErrorCode]);
  end;

  if (MsgErro <> '') then
    DoException(MsgErro);
end;

procedure TACBrTEFScopeAPI.TratarErroPinPadScope(AErrorCode: LongInt);
var
  MsgErro: String;
begin
  case AErrorCode of
    PC_INVCALL: MsgErro := 'Chamada invalida funcao. Operacoes previas sao necessarias.';
    PC_INVPARM: MsgErro := 'Parametro invalido passado a funcao.';
    PC_TIMEOUT: MsgErro := 'Esgotado o tempo maximo estipulado para a operacao.';
    PC_CANCEL: MsgErro := 'Operacao cancelada pelo operador.';
    PC_ALREADYOPEN: MsgErro := 'Pinpad ja aberto.';
    PC_NOTOPEN: MsgErro := 'Pinpad nao foi aberto.';
    PC_EXECERR: MsgErro := 'Erro interno de execucao - problema de implementao da biblioteca (software).';
    PC_INVMODEL: MsgErro := 'Funcao nao suportada pelo modelo de pinpad.';
    PC_NOFUNC: MsgErro := 'Funcao nao disponivel na Biblioteca do pinpad.';
    PC_ERRMANDAT: MsgErro := 'Ausencia de dado mandatorio para o processamento.';
    PC_PORTERR: MsgErro := 'Erro de comunicacao: porta serial do pinpad provavelmente ocupada.';
    PC_COMMERR: MsgErro := 'Erro de comunicacao: pinpad provavelmente desconectado ou problemas com a interface serial.';
    PC_UNKNOWNSTAT: MsgErro := 'Status informado pelo pinpad nao e conhecido.';
    PC_RSPERR: MsgErro := 'Mensagem recebida do pinpad possui formato invalido.';
    PC_COMMTOUT: MsgErro := 'Tempo esgotado ao esperar pela resposta do pinpad.';
    PC_INTERR: MsgErro := 'Erro interno do pinpad.';
    PC_PINPAD_NAO_CONFIGURADO: MsgErro := 'Pinpad nao configurado';
    PC_DISPLAY_NAO_PERMITIDO: MsgErro := 'Display nao permitido neste momento ou situacao';
    PC_NAO_ABERTO_APP: MsgErro := 'Pinpad nao foi aberto pela aplicacao';
    PC_TIMEOUT_USER: MsgErro := 'Timeout do cliente/usuario';
  else
    MsgErro := Format('Erro: %d', [AErrorCode]);
  end;

  if (MsgErro <> '') then
    DoException(MsgErro);
end;

procedure TACBrTEFScopeAPI.AbrirComunicacaoScope;
var
  ret: LongInt;
  sEmpresa, sFilial, sPDV, sEnderecoIP, sPorta: String;
begin
  if fConectado then
    Exit;

  ObterDadosScopeINI(sEmpresa, sFilial, sEnderecoIP, sPorta);
  if (fPDV = '') then
    sPDV := '001'
  else
    sPDV:= fPDV;

  // ExibirMensagem( Format(sMsgAbrindoConexao, [sEmpresa, sFilial, sPDV]) );
  GravarLog('ScopeOpen( 2, '+sEmpresa+', '+sFilial+', '+sPDV+' )');
  ret := xScopeOpen( PAnsiChar('2'),
                     PAnsiChar(AnsiString(sEmpresa)),
                     PAnsiChar(AnsiString(sFilial)),
                     PAnsiChar(AnsiString(sPDV)) );
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> RCS_SUCESSO) then
    TratarErroScope(ret);

  fConectado := True;
  // ExibirMensagem(Format(sMsgConctadoAoServidor, [sEnderecoIP+':'+sPorta]));

  ConfigurarColeta;
  VerificarSessaoTEFAnterior;
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
  if (ret <> RCS_SUCESSO) and (ret <> RCS_API_NAO_INICIALIZADA) then
    TratarErroScope(ret);

  fConectado := False;
  // ExibirMensagem(sMsgDesconectado);
end;

procedure TACBrTEFScopeAPI.VerificarSeEstaConectadoScope;
begin
  if fConectado then
    Exit;

  if not fControleConexao then
    DoException(sErrNaoConectado)
  else
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

  ExibirMensagem(sMsgInicioSessaoTEF);
  GravarLog('ScopeAbreSessaoTEF()');
  ret := xScopeAbreSessaoTEF;
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> RCS_SUCESSO) then
    TratarErroScope(ret);

  ExibirMensagem('');
  fSessaoAberta := True;
end;

procedure TACBrTEFScopeAPI.FecharSessaoTEF;
var
  DesfezTEF: Boolean;
begin
  FecharSessaoTEF(True, DesfezTEF);
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
    Acao := ACAO_FECHA_CONFIRMA_TEF
  else
    Acao := ACAO_FECHA_DESFAZ_TEF;

  DesfezTEF := 0;
  GravarLog('ScopeFechaSessaoTEF( '+IntToStr(Acao)+' )');
  ret := xScopeFechaSessaoTEF(Acao, @DesfezTEF);
  GravarLog('  ret: '+IntToStr(ret)+', DesfezTEF: '+IntToStr(DesfezTEF));
  if (ret <> RCS_SUCESSO) then
    TratarErroScope(ret);

  TransacaoFoiDesfeita := (DesfezTEF = 1);
  fSessaoAberta := False;
  VerificarSeMantemConexaoScope;
end;

procedure TACBrTEFScopeAPI.VerificarSessaoTEFAnterior;
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
  const Param1: String; const Param2: String; const Param3: String);
var
  p1, p2, p3: PAnsiChar;
  w1, w2: Word;
  ret: LongInt;
begin
  GravarLog('IniciarTransacao( '+GetEnumName(TypeInfo(TACBrTEFScopeOperacao), integer(Operacao))+
            ', '+Param1+', '+Param2+', '+Param3+' )' );

  if fEmTransacao then
    DoException(sErrTransacaoJaIniciada);

  if not fSessaoAberta then
    AbrirSessaoTEF;

  p1 := PAnsiChar(AnsiString(Param1));
  p2 := PAnsiChar(AnsiString(Param2));
  p3 := PAnsiChar(AnsiString(Param3));
  ret := 0;

  case Operacao of
    scoMenu:
      begin
        GravarLog('ScopeMenu( 0 )');
        ret := xScopeMenu(0);
      end;

    scoCredito:
      begin
        GravarLog('ScopeCompraCartaoCredito( '+Param1+', '+Param2+' )');
        ret := xScopeCompraCartaoCredito(p1, p2);    // Valor, Taxa Serviço
      end;

    scoDebito:
      begin
        GravarLog('ScopeCompraCartaoDebito( '+Param1+' )');
        ret := xScopeCompraCartaoDebito(p1);         // Valor
      end;

    scoReimpComp:
      begin
        GravarLog('ScopeReimpressaoOffLine');
        ret := xScopeReimpressaoOffLine();
      end;

    scoCheque:
      begin
        GravarLog('ScopeConsultaCheque( '+Param1+' )');
        ret := xScopeConsultaCheque(p1);             // Valor
      end;

    scoConsCDC:
      begin
        GravarLog('ScopeConsultaCDC( '+Param1+', '+Param2+' )');
        ret := xScopeConsultaCDC(p1, p2);            // Valor, Taxa Serviço
      end;

    scoCanc:
      begin
        GravarLog('ScopeCancelamento( '+Param1+', '+Param2+' )');
        ret := xScopeCancelamento(p1, p2);           // Valor, Taxa Serviço
      end;

    scoResVenda:
      begin
        GravarLog('ScopeResumoVendas()');
        ret := xScopeResumoVendas();
      end;

    scoRecargaCel:
      begin
        GravarLog('ScopeRecargaCelular');
        ret := xScopeRecargaCelular();
      end;

    scoPagto:
      begin
        GravarLog('ScopePagamentoConta( '+Param1+' )');
        w1 := StrToIntDef(Param1, 0);
        ret := xScopePagamentoConta(w1);              // Serviço
      end;

    scoPreAutCredito:
      begin
        GravarLog('ScopePreAutorizacaoCredito( '+Param1+', '+Param2+' )');
        ret := xScopePreAutorizacaoCredito(p1, p2);   // Valor, Taxa Serviço
      end;
  end;

  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> RCS_SUCESSO) then
    TratarErroScope(ret)   // Raise exception
  else
    SetEmTransacao(True);
end;

procedure TACBrTEFScopeAPI.ExecutarTransacao;
var
  ret, iStatus: LongInt;
  Acao: Byte;
  rColetaEx: TParam_Coleta_Ext;
  TipoCaptura: Word;
  Resposta: String;

  function VerificarSeUsuarioCancelouTransacao(Fluxo: TACBrTEFScopeEstadoOperacao): Boolean;
  var
    Cancelar: Boolean;
  begin
    // Chama evento, permitindo ao usuário cancelar
    Cancelar := False;
    ChamarEventoTransacaoEmAndamento(Fluxo, Cancelar);
    Result := Cancelar;
  end;

begin
  GravarLog('ExecutarTransacao');

  if not fEmTransacao then
    DoException(sErrTransacaoNaoIniciada);

  try
    fDadosDaTransacao.Clear;
    ExibirMensagem(sMsgTransacaoEmAndamento);

    while True do
    begin
      // Le o Status da Operação
      iStatus := ObterScopeStatus;

      // Iniciliza as variáveis
      Resposta := '';
      Acao := ACAO_PROXIMO_ESTADO;
      TipoCaptura := COLETA_TECLADO;

      // Enquanto a transacao estiver em andamento, aguarda, mas verifica se o usuário Cancelou //
      if (iStatus = RCS_TRN_EM_ANDAMENTO) then
      begin
        if VerificarSeUsuarioCancelouTransacao(scoestFluxoAPI) then
          EnviarParametroTransacao(ACAO_CANCELAR)
        else
          Sleep(fIntervaloColeta);

        Continue;
      end;

      // Efetuando Leitura do Cartão. Verifica se o operador cancelou a operacao via teclado //
      if (iStatus = TC_COLETA_CARTAO_EM_ANDAMENTO) then
      begin
        if VerificarSeUsuarioCancelouTransacao(scoestPinPadLerCartao) then
          Acao := ACAO_CANCELAR;

        EnviarParametroTransacao(Acao);
        Continue;
      end;

      // Se estiver fora da faixa FC00 a FCFF, finaliza o processo //
      if ((iStatus < TC_PRIMEIRO_TIPO_COLETA) or (iStatus > TC_MAX_TIPO_COLETA)) then
        Break;

      // Verifica se já tem resposta para esse estado //
      Resposta := RespostasPorEstados.Values[IntToStr(iStatus)];

      if (Resposta = '') and (iStatus = TC_EXIBE_MENU) then
        PerguntarMenuScope(MNU_TAB_TIPO_CIELO, Resposta, Acao);  // Deveria ser MNU_TAB_TIPO_GENERICO

      // Se ainda não tem a Resposta colete os Parâmetros //
      if (Resposta = '') then
      begin
        ColetarParametrosScope(iStatus, rColetaEx);
        ExibirMsgColeta(rColetaEx);

        // Coleta recebeu instrução para Menu ? //
        if (rColetaEx.FormatoDado = TM_SELECAO) then
          PerguntarMenuScope(MNU_TAB_TIPO_SELECAO_ESPECIAL, Resposta, Acao);
      end;

      // Se não recebeu resposta para esse estado, trate a coleta
      if (Resposta = '') then
      begin
        // Trata os estados //
        case iStatus of
          TC_EXIBE_MENU:                // Já tratado acima por 'PerguntarMenuScope'
            {Nada a fazer};

          TC_INFO_RET_FLUXO,            // apenas mostra informacao e deve retornar ao scope //
          TC_COLETA_EM_ANDAMENTO:       // transacao em andamento //
            Acao := ACAO_PROXIMO_ESTADO;

          TC_DECIDE_AVISTA, TC_COLETA_CANCELA_TRANSACAO, TC_DECIDE_ULTIMO:
            begin
              PerguntarSimNao(rColetaEx, Resposta, Acao);
            end;

          TC_CARTAO_DIGITADO:
            begin
              if not fPermitirCartaoDigitado then
                Acao := ACAO_CANCELAR
              else
                Acao := ACAO_COLETAR;
            end;

          TC_CARTAO,                    // cartao //
          TC_COLETA_AUT_OU_CARTAO:
            Acao := ACAO_COLETAR;

          TC_IMPRIME_CHEQUE:            // imprime Cheque //
            ObterDadosCheque;

          TC_IMPRIME_CUPOM,             // imprime Cupom + Nota Promissoria + Cupom Promocional //
          TC_IMPRIME_CUPOM_PARCIAL,     // imprime Cupom Parcial //
          TC_IMPRIME_CONSULTA:
            ObterDadosComprovantes;

          //TC_DISP_LISTA_MEDICAMENTO:; // recupera lista de Medicamentos //
            //TODO

          //TC_COLETA_REG_MEDICAMENTO:; // se coletou lista de medicamentos, deve tambem atualizar o valor. //
            //TODO

          TC_DISP_VALOR:                // recupera valor do Vale Gas e da consulta de Cartão Dinheiro //
            ColetarValoresConsulta;

          //TC_OBTEM_SERVICOS:;         // recupera os servicos configurados //
            //TODO:

          TC_COLETA_OPERADORA:          // recupera a lista de operadoras da Recarga de Celular //
            PerguntarMenuOperadora(rColetaEx, Resposta, Acao);

          TC_COLETA_VALOR_RECARGA:      // recupera a lista de valores da Recarga de Celular //
            PerguntarMenuValorRecargaCel(rColetaEx, Resposta, Acao);

          TC_COLETA_DDD_PP:
            Resposta := ObterDadoPinPad(PP_DIGITE_O_DDD, 3, 3);
          TC_COLETA_REDIGITA_DDD_PP:
            Resposta := ObterDadoPinPad(PP_REDIGITE_O_DDD, 3, 3);
          TC_COLETA_NUM_TEL_PP:
            Resposta := ObterDadoPinPad(PP_DIGITE_O_TELEFONE, 8, 9);
          TC_COLETA_DDD_NUMTEL_PP:
            Resposta := ObterDadoPinPad(PP_DIGITE_DDD_TELEFONE, 10, 11);
          TC_REDIGITA_DDD_NUMTEL_PP:
            Resposta := ObterDadoPinPad(PP_REDIGITE_DDD_TELEFONE, 10, 11);

          TC_SENHA:;                    // captura da senha do usuario //
            //TODO:

          TC_INFO_AGU_CONF_OP:;         // mostra informacao e aguarda confirmacao do usuario //
            //TODO:

          TC_OBTEM_QRCODE:;
            //TODO:

          TC_COLETA_DADOS_ECF:;         // coleta dados do ECF e do cupom fiscal para a transacao de debito voucher com o TICKET CAR //
            //TODO:

          TC_COLETA_LISTA_MERCADORIAS:; // coleta Lista de Mercadorias para a transacao de debito voucher com o TICKET CAR //
            //TODO:

          TC_COLETA_LISTA_PRECOS:;      // coleta Lista para Atualizacao de Precos (TICKET CAR)
            //TODO:

        else                            // deve coletar algo... //
          Acao := ACAO_COLETAR;
        end;

        if (Acao = ACAO_COLETAR) then
        begin
          ExibirMsgColeta(rColetaEx);
          Acao := ACAO_PROXIMO_ESTADO;
          fOnPerguntaCampo(MsgOperador(rColetaEx), rColetaEx, Resposta, Acao);
        end;
      end;

      ret := EnviarParametroTransacao(Acao, iStatus, Resposta, TipoCaptura);
      if (ret <> RCS_SUCESSO) then
      begin
        if (ret <> RCS_DADO_INVALIDO) then
        begin
          iStatus := ret;
          Break;
        end
        else
          ExibirErroUltimaMsg;
      end;
    end;

    if (iStatus = RCS_SUCESSO) then
    begin
      ExibirMensagem(sMsgTransacaoCompleta);
      ObterDadosDaTransacao;
    end
    else
    begin
      ExibirErroUltimaMsg;
      if (iStatus <> RCS_CANCELADA_PELO_OPERADOR) then
        TratarErroScope(iStatus);
    end;
  finally
    SetEmTransacao(False);
    RespostasPorEstados.Clear;
  end;
end;

function TACBrTEFScopeAPI.EnviarParametroTransacao(Acao: LongInt;
  codTipoColeta: LongInt; Dados: AnsiString; dadosParam: Word): LongInt;
begin
  if (codTipoColeta < 0) then
    codTipoColeta := ObterScopeStatus;

  GravarLog('ScopeResumeParam( '+IntToStr(codTipoColeta)+', "'+Dados+'", '+IntToStr(dadosParam)+', '+IntToStr(Acao)+' )');
  Result := xScopeResumeParam(codTipoColeta, PAnsiChar(Dados), dadosParam, Acao);
  GravarLog('  ret: '+IntToStr(Result));
end;

procedure TACBrTEFScopeAPI.AbortarTransacao;
begin
  EnviarParametroTransacao(ACAO_CANCELAR);
  SetEmTransacao(False);
end;

function TACBrTEFScopeAPI.ObterScopeStatus: Longint;
begin
  GravarLog('ScopeStatus');
  Result := xScopeStatus;
  GravarLog('  ret: '+IntToStr(Result) + ' - $'+IntToHex(Result, 4));
end;

procedure TACBrTEFScopeAPI.ObterDadosComprovantes;
var
  ret: LongInt;
  pCabec, pCupomCliente, pCupomLoja, pCupomReduzido: PAnsiChar;
  NumeroLinhasReduzido: Byte;
  sCabecalho, sCupomLoja, sCupomCliente, sCupomReduzido: String;
begin
  pCabec := AllocMem(1024);
  pCupomCliente := AllocMem(2048);
  pCupomLoja := AllocMem(2048);
  pCupomReduzido := AllocMem(2048);
  try
    GravarLog('ScopeGetCupomEx');
    ret := xScopeGetCupomEx( 1024, pCabec,
                             2048, pCupomCliente,
                             2048, pCupomLoja,
                             2048, pCupomReduzido,
                             @NumeroLinhasReduzido);
    GravarLog('  ret: '+IntToStr(ret));
    if (ret <> RCS_SUCESSO) then
      TratarErroScope(ret);

    sCabecalho := PAnsiCharToString(pCabec);
    sCupomLoja := PAnsiCharToString(pCupomLoja);
    sCupomCliente:= PAnsiCharToString(pCupomCliente);
    sCupomReduzido := PAnsiCharToString(pCupomReduzido);

    GravarLog('Cabecalho: ' + sLineBreak + sCabecalho);
    GravarLog('Cupom Loja: ' + sLineBreak + sCupomLoja);
    GravarLog('Cupom Cliente: ' + sLineBreak + sCupomCliente);
    GravarLog('Cupom Reduzido: '+IntToStr(NumeroLinhasReduzido)+' linhas' + sLineBreak + sCupomReduzido);

    fDadosDaTransacao.Values[RET_CUPOM_LOJA]     := BinaryStringToString( sCabecalho + sLineBreak + sCupomLoja );
    fDadosDaTransacao.Values[RET_CUPOM_CLIENTE]  := BinaryStringToString( sCabecalho + sLineBreak + sCupomCliente );
    fDadosDaTransacao.Values[RET_CUPOM_REDUZIDO] := BinaryStringToString( sCupomReduzido );
    fDadosDaTransacao.Values[RET_NUMLINHAS_REDUZIDO] := IntToStr(NumeroLinhasReduzido);
  finally
    Freemem(pCabec);
    Freemem(pCupomCliente);
    Freemem(pCupomLoja);
    Freemem(pCupomReduzido);
  end;
end;

procedure TACBrTEFScopeAPI.ColetarValoresConsulta;
var
  ret: LongInt;
  pValor: PAnsiChar;
  s: String;
begin
  pValor := AllocMem(100);
  try
    GravarLog('ScopeObtemConsultaValeGas');
    ret := xScopeObtemConsultaValeGas(pValor);
    s := PAnsiCharToString(pValor);
    GravarLog('  ret: '+IntToStr(ret)+', Valor: '+s);
    if (ret = RCS_SUCESSO) then
      fDadosDaTransacao.Values[RET_VALOR_VALE_GAS] := s;
  finally
    Freemem(pValor);
  end;

  //TODO: Como obter Valor de consulta de Cartão Dinheiro ?
  //TODO: Como obter valores de Pagamentos de Contas ?
end;

procedure TACBrTEFScopeAPI.ObterDadosCheque;
var
  rCheque: TParam_Cheq;
  ret: LongInt;
begin
  GravarLog('ScopeGetCheque');
  ret := xScopeGetCheque(@rCheque);
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> RCS_SUCESSO) then
    TratarErroScope(ret);

  fDadosDaTransacao.Values[RET_CHEQUE_BANCO]   := ArrayOfCharToString(rCheque.Banco);
  fDadosDaTransacao.Values[RET_CHEQUE_AGENCIA] := ArrayOfCharToString(rCheque.Agencia);
  fDadosDaTransacao.Values[RET_CHEQUE_NUMERO]  := ArrayOfCharToString(rCheque.NumCheque);
  fDadosDaTransacao.Values[RET_CHEQUE_VALOR]   := ArrayOfCharToString(rCheque.Valor);
  fDadosDaTransacao.Values[RET_CHEQUE_DATA]    := ArrayOfCharToString(rCheque.BomPara);
  fDadosDaTransacao.Values[RET_CHEQUE_CODAUT]  := ArrayOfCharToString(rCheque.CodAut);
  fDadosDaTransacao.Values[RET_CHEQUE_MUNICIP] := ArrayOfCharToString(rCheque.Municipio);

  GravarLog('Dados do Cheque'+sLineBreak+
            '  Banco: '+fDadosDaTransacao.Values[RET_CHEQUE_BANCO] + sLineBreak +
            '  Agencia: '+fDadosDaTransacao.Values[RET_CHEQUE_AGENCIA] + sLineBreak +
            '  NumCheque: '+fDadosDaTransacao.Values[RET_CHEQUE_NUMERO] + sLineBreak +
            '  Valor: '+fDadosDaTransacao.Values[RET_CHEQUE_VALOR] + sLineBreak +
            '  BomPara: '+fDadosDaTransacao.Values[RET_CHEQUE_DATA] + sLineBreak +
            '  CodAut: '+fDadosDaTransacao.Values[RET_CHEQUE_CODAUT] + sLineBreak +
            '  Municipio: '+fDadosDaTransacao.Values[RET_CHEQUE_MUNICIP]);
end;

function TACBrTEFScopeAPI.ObterHandleScope(TipoHandle: LongInt): LongInt;
begin
  GravarLog('ScopeObtemHandle( '+IntToStr(TipoHandle)+' )');
  Result := xScopeObtemHandle(TipoHandle);
  GravarLog('  ret: '+IntToStr(Result));
  if (Result <= RCS_ERRO_GENERICO) then
    TratarErroScope(Result);
end;

procedure TACBrTEFScopeAPI.ObterDadosDaTransacao;
var
  pBuffer: PAnsiChar;
  h, i, ret, mask: LongInt;
  sBuffer, hmask: String;
const
  BUFFER_SIZE = 40960;
begin
  h := ObterHandleScope(HDL_TRANSACAO_ANTERIOR);
  pBuffer := AllocMem(BUFFER_SIZE);
  try
    GravarLog('-- Obtendo campos de Mask1 --');
    mask := 1;
    for i := 1 to 32 do
    begin
      pBuffer^ := #0;
      hmask := '$'+IntToHex(mask, 8);
      GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', '+hmask+', 0, 0, 0 )');
      ret := xScopeObtemCampoExt3(h, mask, 0, 0, 0, 0, pBuffer);
      sBuffer := String(pBuffer);
      GravarLog('  ret: '+IntToStr(ret)+', Buffer: '+sBuffer);
      fDadosDaTransacao.Add(Format('%s-%s=%s', ['mask1', hmask, sBuffer]));
      mask := mask shl 1;
    end;

    GravarLog('-- Obtendo campos de Mask2 --');
    mask := 1;
    for i := 1 to 32 do
    begin
      pBuffer^ := #0;
      hmask := '$'+IntToHex(mask, 8);
      GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', 0, '+hmask+', 0, 0 )');
      ret := xScopeObtemCampoExt3(h, 0, mask, 0, 0, 0, pBuffer);
      sBuffer := String(pBuffer);
      GravarLog('  ret: '+IntToStr(ret)+', Buffer: '+sBuffer);
      fDadosDaTransacao.Add(Format('%s-%s=%s', ['mask2', hmask, sBuffer]));
      mask := mask shl 1;
    end;

    GravarLog('-- Obtendo campos de Mask3 --');
    mask := 1;
    for i := 1 to 32 do
    begin
      pBuffer^ := #0;
      hmask := '$'+IntToHex(mask, 8);
      GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', 0, 0, '+hmask+', 0 )');
      ret := xScopeObtemCampoExt3(h, 0, 0, mask, 0, 0, pBuffer);
      sBuffer := String(pBuffer);
      GravarLog('  ret: '+IntToStr(ret)+', Buffer: '+sBuffer);
      fDadosDaTransacao.Add(Format('%s-%s=%s', ['mask3', hmask, sBuffer]));
      mask := mask shl 1;
    end;

    GravarLog('-- Obtendo campos de Mask4 --');
    mask := 1;
    for i := 1 to 32 do
    begin
      if not
         ( (mask = MASK4_String_QRCode) or (mask = MASK4_Campo_TID_Pix) or (mask = MASK4_Campo_Referencia_Pix) or
           (mask = MASK4_Tamanho_BIN) or (mask = MASK4_Dados_DCC) or (mask = MASK4_Status_DCC) ) then
      begin
        pBuffer^ := #0;
        hmask := '$'+IntToHex(mask, 8);
        GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', 0, 0, 0, '+hmask+' )');
        ret := xScopeObtemCampoExt3(h, 0, 0, 0, mask, 0, pBuffer);
        sBuffer := String(pBuffer);
        GravarLog('  ret: '+IntToStr(ret)+', Buffer: '+sBuffer);
        fDadosDaTransacao.Add(Format('%s-%s=%s', ['mask4', hmask, sBuffer]));
        mask := mask shl 1;
      end;
    end;
  finally
    Freemem(pBuffer);
  end;
end;

function TACBrTEFScopeAPI.ObterCampoMask1(AMask: LongInt): String;
var
  pBuffer: PAnsiChar;
  hmask: String;
  ret, h: LongInt;
const
  BUFFER_SIZE = 40960;
begin
  Result := '';
  h := ObterHandleScope(HDL_TRANSACAO_EM_ANDAMENTO);
  pBuffer := AllocMem(BUFFER_SIZE);
  try
    pBuffer^ := #0;
    hmask := '$'+IntToHex(AMask, 8);
    GravarLog('ScopeObtemCampoExt3( '+IntToStr(h)+', '+hmask+', 0, 0, 0 )');
    ret := xScopeObtemCampoExt3(h, AMask, 0, 0, 0, 0, pBuffer);
    Result := PAnsiCharToString(pBuffer);
    GravarLog('  ret: '+IntToStr(ret)+', Buffer: '+Result);
  finally
    Freemem(pBuffer);
  end;
end;

procedure TACBrTEFScopeAPI.ExibirErroUltimaMsg;
var
  MsgColetada: TColeta_Msg;
  ret: LongInt;
  s: String;
begin
  // Coleta dados do Scope para esse passo //
  GravarLog('ScopeGetLastMsg');
  FillChar(MsgColetada, SizeOf(TColeta_Msg), #0);
  ret := xScopeGetLastMsg(@MsgColetada);
  GravarLog('  ret: '+IntToStr(ret));

  if (ret = RCS_SUCESSO) then
  begin
    s := 'Coleta_Msg.' + sLineBreak +
         '  Op1: '+String(MsgColetada.Op1) + sLineBreak +
         '  Op2: '+String(MsgColetada.Op2) + sLineBreak +
         '  Cl1: '+String(MsgColetada.Cl1) + sLineBreak +
         '  Cl2: '+String(MsgColetada.Cl2);
    GravarLog(s);

    // Exibe as mensagens do cliente e operador //
    s := Trim(String(MsgColetada.Cl1)) + sLineBreak + Trim(String(MsgColetada.Cl2));
    if (s <> '') then
      ExibirMensagem(s, tmCliente);

    s := Trim(String(MsgColetada.Op1)) + sLineBreak + Trim(String(MsgColetada.Op2));
    if (s <> '') then
    begin
      ExibirMensagem(s, tmOperador);
      ExibirMensagem(s, tmOperador, 0);
    end;
  end;
end;

procedure TACBrTEFScopeAPI.LogColeta(AColeta: TParam_Coleta);
var
  s: String;
begin
  s := 'Param_Coleta.' + sLineBreak +
       '  Bandeira: '+IntToStr(AColeta.Bandeira) + sLineBreak +
       '  FormatoDado: '+IntToStr(AColeta.FormatoDado) + sLineBreak +
       '  HabTeclas: '+IntToStr(AColeta.HabTeclas) + sLineBreak +
       '  MsgOp1: '+ArrayOfCharToString(AColeta.MsgOp1) + sLineBreak +
       '  MsgOp2: '+ArrayOfCharToString(AColeta.MsgOp2) + sLineBreak +
       '  MsgCl1: '+ArrayOfCharToString(AColeta.MsgCl1) + sLineBreak +
       '  MsgCl2: '+ArrayOfCharToString(AColeta.MsgCl2) + sLineBreak +
       '  WrkKey: '+ArrayOfCharToString(AColeta.WrkKey) + sLineBreak +
       '  PosMasterKey: '+IntToStr(AColeta.PosMasterKey) + sLineBreak +
       '  PAN: '+ArrayOfCharToString(AColeta.PAN) + sLineBreak +
       '  UsaCriptoPinpad: '+IntToStr(AColeta.UsaCriptoPinpad) + sLineBreak +
       '  IdModoPagto: '+IntToStr(AColeta.IdModoPagto) + sLineBreak +
       '  AceitaCartaoDigitado: '+IntToStr(AColeta.AceitaCartaoDigitado) + sLineBreak +
       '  Reservado: '+ArrayOfCharToString(AColeta.Reservado);

  GravarLog(s);
end;

procedure TACBrTEFScopeAPI.LogColetaEx(AColetaEx: TParam_Coleta_Ext);
var
  s: String;
begin
  s := 'Param_Coleta_Ext.' + sLineBreak +
       '  FormatoDado: '+IntToStr(AColetaEx.FormatoDado) + sLineBreak +
       '  HabTeclas: '+IntToStr(AColetaEx.HabTeclas) + sLineBreak +
       '  CodBandeira: '+AColetaEx.CodBandeira + sLineBreak +
       '  CodRede: '+AColetaEx.CodRede + sLineBreak +
       '  MsgOp1: '+ArrayOfCharToString(AColetaEx.MsgOp1) + sLineBreak +
       '  MsgOp2: '+ArrayOfCharToString(AColetaEx.MsgOp2) + sLineBreak +
       '  MsgCl1: '+ArrayOfCharToString(AColetaEx.MsgCl1) + sLineBreak +
       '  MsgCl2: '+ArrayOfCharToString(AColetaEx.MsgCl2) + sLineBreak +
       '  UsaExt: '+IntToStr(AColetaEx.UsaExt) + sLineBreak +
       '    Ext.Sigla: '+ ArrayOfCharToString(AColetaEx.Ext.Sigla) + sLineBreak +
       '    Ext.Rotulo: '+ ArrayOfCharToString(AColetaEx.Ext.Rotulo) + sLineBreak +
       '    Ext.AceitaVazio: '+IntToStr(AColetaEx.Ext.AceitaVazio) + sLineBreak +
       '    Ext.QtdCasasDecimais: '+IntToStr(AColetaEx.Ext.QtdCasasDecimais) + sLineBreak +
       '    Ext.TamMin: '+ ArrayOfCharToString(AColetaEx.Ext.TamMin) + sLineBreak +
       '    Ext.TamMax: '+ ArrayOfCharToString(AColetaEx.Ext.TamMax) + sLineBreak +
       '  UsaLimites: '+IntToStr(AColetaEx.UsaLimites) + sLineBreak +
       '    Limite.Inferior: '+ ArrayOfCharToString(AColetaEx.Limite.Inferior) + sLineBreak +
       '    Limite.Superior: '+ ArrayOfCharToString(AColetaEx.Limite.Superior) + sLineBreak +
       '  IdColetaExt: '+IntToStr(AColetaEx.IdColetaExt) + sLineBreak +
       '  Reservado: '+ArrayOfCharToString(AColetaEx.Reservado);

  GravarLog(s);
end;

procedure TACBrTEFScopeAPI.PerguntarMenuScope(const TipoMenu: Byte;
  var Resposta: String; var Acao: Byte);
var
  ret: LongInt;
  rMenuCielo: TScopeMenuCielo;
  rMenuGenerico: TScopeMenuGenerico;
  rMenuDinamico: TScopeMenuDinamico;
  rMenuEspecial: TScopeMenuEspecial;
  Titulo, s: String;
  Opcoes: TStringList;
  i, j, size: Word;
  item: Integer;
begin
  GravarLog('PerguntarMenu( '+IntToStr(TipoMenu)+' )');
  Resposta := '';
  Acao := ACAO_PROXIMO_ESTADO;
  item := -1;

  Opcoes := TStringList.Create;
  try
    Titulo := sMsgTituloMenu;

    case TipoMenu of
      MNU_TAB_TIPO_CIELO:
        begin
          size := SizeOf(TScopeMenuCielo);
          ret := xScopeMenuRecuperaItens( TipoMenu, @rMenuCielo, size) ;
          GravarLog('  ret: '+IntToStr(ret));
          if (ret <> RCS_SUCESSO) then
            TratarErroScope(ret);
          if (Byte(rMenuCielo.TipoTabela) <> MNU_TAB_TIPO_CIELO) then
            DoException(sErrEstruturaMenu);
          for i := 1 to rMenuCielo.QtdItens do
          begin
            s := Trim(rMenuCielo.Itens[i].Descricao);
            Opcoes.Add(s);
          end;
        end;

      MNU_TAB_TIPO_GENERICO:
        begin
          size := SizeOf(TScopeMenuGenerico);
          ret := xScopeMenuRecuperaItens( TipoMenu, @rMenuGenerico, size) ;
          GravarLog('  ret: '+IntToStr(ret));
          if (ret <> RCS_SUCESSO) then
            TratarErroScope(ret);
          if (StrToInt(rMenuGenerico.TipoTabela) <> MNU_TAB_TIPO_GENERICO) then
            DoException(sErrEstruturaMenu);
          for i := 1 to QTD_MAX_ITENS_PERG_GEN_SEL do
          begin
            s := Trim(rMenuGenerico.Itens[i]);
            if (s = '') then
              Break;
            Opcoes.Add(s);
          end;
        end;

      MNU_TAB_TIPO_SAVS:
        DoException(Format(sErrNaoImplementado, ['MNU_TAB_TIPO_SAVS']));

      MNU_TAB_TIPO_GENERICO_DINAMICO:
        begin
          size := SizeOf(TScopeMenuDinamico);
          ret := xScopeMenuRecuperaItens( TipoMenu, @rMenuDinamico, size) ;
          GravarLog('  ret: '+IntToStr(ret));
          if (ret <> RCS_SUCESSO) then
            TratarErroScope(ret);
          if (StrToInt(rMenuDinamico.TipoTabela) <> MNU_TAB_TIPO_GENERICO_DINAMICO) then
            DoException(sErrEstruturaMenu);
          for i := 1 to rMenuDinamico.QtdItens do
          begin
            s := Trim(rMenuDinamico.ItemGenerico.Itens[i]);
            Opcoes.Add(s);
          end;
        end;

      MNU_TAB_TIPO_SELECAO_ESPECIAL:
        begin
          size := SizeOf(TScopeMenuEspecial);
          ret := xScopeMenuRecuperaItens( TipoMenu, @rMenuEspecial, size) ;
          GravarLog('  ret: '+IntToStr(ret));
          if (ret <> RCS_SUCESSO) then
            TratarErroScope(ret);
          j := StrToInt(rMenuEspecial.QtdItens);
          for i := 1 to j do
          begin
            s := Trim(rMenuEspecial.Itens[i].Rotulo);
            Opcoes.Add(s);
          end;
        end;
    end;

    if (Opcoes.Count > 0) then
    begin
      fOnPerguntarMenu(ACBrStr(Titulo), Opcoes, item);
      if (item = -2) then
        Acao := ACAO_ESTADO_ANTERIOR
      else if (item = -1) then
        Acao := ACAO_CANCELAR
      else
        Resposta := IntToStr(item+1);;
    end;

  finally
    Opcoes.Free;
  end;

  if (Acao = ACAO_PROXIMO_ESTADO) then
  begin
    GravarLog('ScopeMenuSelecionaItem( '+Resposta+' )');
    ret := xScopeMenuSelecionaItem(Byte(item+1)) ;
    GravarLog('  ret: '+IntToStr(ret));
    if (ret <> RCS_SUCESSO) then
      TratarErroScope(ret);
  end;
end;

procedure TACBrTEFScopeAPI.PerguntarSimNao(const rColetaEx: TParam_Coleta_Ext;
  var Resposta: String; var Acao: Byte);
var
  op: TStringList;
  item: Integer;
  Titulo: String;
begin
  Titulo := StringReplace(MsgOperador(rColetaEx), sLineBreak, ' ', [rfReplaceAll])+'?';
  Titulo := copy(Titulo, 1, Pos('?', Titulo));

  Resposta := '0';
  Acao := ACAO_PROXIMO_ESTADO;

  op := TStringList.Create;
  try
    op.Add(ACBrStr(sMsgItemSim));
    op.Add(ACBrStr(sMsgItemNao));
    item := 0;
    fOnPerguntarMenu(ACBrStr(Titulo), op, item);
    if (item = -2) then
      Acao := ACAO_ESTADO_ANTERIOR
    else if (item = -1) then
      Acao := ACAO_CANCELAR
    else if (item = 0) then  // Sim
      Resposta := '1';
  finally
    op.Free;
  end;
end;

procedure TACBrTEFScopeAPI.PerguntarMenuOperadora(
  var rColetaEx: TParam_Coleta_Ext; var Resposta: String; var Acao: Byte);
var
  rListaOperadoras: TRec_Cel_Oper;
  rOperadora: TRec_Cel_ID_OperM3;
  ret, i: LongInt;
  sl: TStringList;
  item: Integer;
begin
  Resposta := '';
  Acao := ACAO_CANCELAR;

  FillChar(rListaOperadoras, SizeOf(TRec_Cel_Oper), #0);
  GravarLog('ScopeRecuperaOperadorasRecCel( '+IntToStr(REC_CEL_OPERADORAS_MODELO_3)+' )');
  ret := xScopeRecuperaOperadorasRecCel(REC_CEL_OPERADORAS_MODELO_3, @rListaOperadoras, SizeOf(TRec_Cel_Oper));
  GravarLog('  ret: '+IntToStr(ret));
  if (ret = RCS_SUCESSO) then
  begin
    sl := TStringList.Create;
    try
      for i := 0 to rListaOperadoras.NumOperCel do
      begin
        move( rListaOperadoras.OperCel[i * SizeOf(TRec_Cel_ID_OperM3) + 1],
          rOperadora, SizeOf(TRec_Cel_ID_OperM3) );
        sl.Add(Format('%.3d - %s',[rOperadora.CodOperCel, ArrayOfCharToString(rOperadora.NomeOperCel)]));
      end;

      if (sl.Count > 0) then
      begin
        item := 0;
        fOnPerguntarMenu('Escolha a Operadora', sl, item);
        if (item = -2) then
          Acao := ACAO_ESTADO_ANTERIOR
        else
        begin
          Resposta := copy(sl[item], 1, 3);
          Acao := ACAO_PROXIMO_ESTADO;
        end;
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TACBrTEFScopeAPI.PerguntarMenuValorRecargaCel(
  var rColetaEx: TParam_Coleta_Ext; var Resposta: String; var Acao: Byte);
var
  ret, i: LongInt;
  rCelValores: TRec_Cel_Valores;
  slMenu, slResp: TStringList;
  tab: Byte;
  Valor, Bonus, Custo: Double;
  s: String;
  item: Integer;
begin
  Resposta := '';
  Acao := ACAO_CANCELAR;

  tab := REC_CEL_VALORES_MODELO_2;
  s := ObterCampoMask1(MASK1_Cod_Rede);
  if (StrToIntDef(s, 0) = R_GWCEL) then
    tab := REC_CEL_VALORES_MODELO_3;

  FillChar(rCelValores, SizeOf(TRec_Cel_Valores), #0);
  GravarLog('ScopeRecuperaValoresRecCel( '+IntToStr(tab)+' )');
  ret := xScopeRecuperaValoresRecCel(tab, @rCelValores, SizeOf(TRec_Cel_Valores));
  GravarLog('  ret: '+IntToStr(ret));
  if (ret = RCS_SUCESSO) then
  begin
    slMenu := TStringList.Create;
    slResp := TStringList.Create;
    try
      if (rCelValores.TipoValor in ['F','T']) and (rCelValores.TotValor > 0) then
      begin
        for i := 1 to rCelValores.TotValor do
        begin
          s := ArrayOfCharToString(rCelValores.TabValores[i].Valor);
          slResp.Add(s);
          Valor := StrToIntDef(s, 0)/100;
          s := ArrayOfCharToString(rCelValores.TabValores[i].Bonus);
          Bonus := StrToIntDef(s, 0)/100;
          s := ArrayOfCharToString(rCelValores.TabValores[i].Custo);
          Custo := StrToIntDef(s, 0)/100;

          slMenu.add( 'Valor: R$ '+Format('%5.2f', [Valor]) +
                  ' Bonus: R$ '+Format('%5.2f', [Bonus]) +
                  ' Custo: R$ '+Format('%5.2f', [Custo]) );
          item := 0;
          fOnPerguntarMenu('Selecione o Valor', slMenu, item);
          if (item = -2) then
            Acao := ACAO_ESTADO_ANTERIOR
          else
          begin
            Resposta := slResp[item];
            Acao := ACAO_PROXIMO_ESTADO;
          end;
        end;
      end
      else
      begin
        // Não tem valores fixos, colete o valor...
        Acao := ACAO_COLETAR;
        rColetaEx.UsaLimites := 1;
        move(rCelValores.ValorMinimo, rColetaEx.Limite.Inferior, SizeOf(rColetaEx.Limite.Inferior));
        move(rCelValores.ValorMaximo, rColetaEx.Limite.Superior, SizeOf(rColetaEx.Limite.Superior));
        if (ArrayOfCharToString(rColetaEx.MsgCl2) <> '') then
          move(rCelValores.MsgPromocional, rColetaEx.MsgCl2, SizeOf(rColetaEx.MsgCl2));
      end;
    finally
      slMenu.Free;
      slResp.Free;
    end;
  end;
end;

procedure TACBrTEFScopeAPI.ColetarParametrosScope(const iStatus: Word;
  var rColetaEx: TParam_Coleta_Ext);
var
  rColeta: TParam_Coleta;
  ret: LongInt;
begin
  // Obtendo informações da Coleta em curso
  FillChar(rColeta, SizeOf(TParam_Coleta), #0);
  FillChar(rColetaEx, SizeOf(TParam_Coleta_Ext), #0);

  if ((iStatus = TC_COLETA_EXT) or (iStatus = TC_COLETA_DADO_ESPECIAL)) and
     Assigned(xScopeGetParamExt) then
  begin
    GravarLog('ScopeGetParamExt');
    ret := xScopeGetParamExt(iStatus, @rColetaEx);
    GravarLog('  ret: '+IntToStr(ret));
    if (ret <> RCS_SUCESSO) then
      TratarErroScope(ret);

    LogColetaEx(rColetaEx);
  end
  else
  begin
    // Coleta dados do Scope para esse passo //
    GravarLog('ScopeGetParam');
    ret := xScopeGetParam(iStatus, @rColeta);
    GravarLog('  ret: '+IntToStr(ret));
    if (ret <> RCS_SUCESSO) then
      TratarErroScope(ret);

    LogColeta(rColeta);
    AssignColetaToColetaEx(rColeta, rColetaEx);
  end;

  // Salva em DadosDaTransacao as informaçoes retornadas na Coleta //;
  if (Trim(rColetaEx.CodBandeira) <> '') then
    fDadosDaTransacao.Values[CBANDEIRA] := rColetaEx.CodBandeira;
end;

procedure TACBrTEFScopeAPI.ExibirMsgColeta(const rColetaEx: TParam_Coleta_Ext);
var
  s: String;
begin
  // Exibe as mensagens do cliente e operador //
  s := MsgOperador(rColetaEx);
  if (s <> '') then
    ExibirMensagem(s, tmOperador);

  s := MsgCliente(rColetaEx);
  if (s <> '') then
    ExibirMensagem(s, tmCliente);
end;

procedure TACBrTEFScopeAPI.AssignColetaToColetaEx(const rColeta: TParam_Coleta;
  var rColetaEx: TParam_Coleta_Ext);
var
  s: String;
begin
  FillChar(rColetaEx, SizeOf(TParam_Coleta_Ext), #0);
  rColetaEx.FormatoDado := rColeta.FormatoDado;
  rColetaEx.HabTeclas := rColeta.HabTeclas;
  s := Format('%.3d',[rColeta.Bandeira]);
  move(s[1], rColetaEx.CodBandeira, SizeOf(rColetaEx.CodBandeira) );
  move(rColeta.MsgOp1,  rColetaEx.MsgOp1, SizeOf(rColetaEx.MsgOp1) );
  move(rColeta.MsgOp2,  rColetaEx.MsgOp2, SizeOf(rColetaEx.MsgOp2) );
  move(rColeta.MsgCl1,  rColetaEx.MsgCl1, SizeOf(rColetaEx.MsgCl1) );
  move(rColeta.MsgCl2,  rColetaEx.MsgCl2, SizeOf(rColetaEx.MsgCl2) );
  move(rColeta.Reservado,  rColetaEx.Reservado, SizeOf(rColetaEx.Reservado) );
end;

function TACBrTEFScopeAPI.MsgOperador(const rColetaEx: TParam_Coleta_Ext): String;
begin
  Result := ArrayOfCharToString(rColetaEx.MsgOp1) + sLineBreak + ArrayOfCharToString(rColetaEx.MsgOp2);
end;

function TACBrTEFScopeAPI.MsgCliente(const rColetaEx: TParam_Coleta_Ext): String;
begin
  Result := ArrayOfCharToString(rColetaEx.MsgCl1) + sLineBreak + ArrayOfCharToString(rColetaEx.MsgCl2);
end;

function TACBrTEFScopeAPI.FormatarMsgPinPad(const MsgPinPad: String): String;
var
  s, l1, l2: String;
  p: Integer;
begin
  s := MsgPinPad;
  p := pos('|', s);
  if (p = 0) then
  begin
    p := 17;
    Insert('|', s, p);
  end;

  l1 := PadRight(copy(s, 1, p-1), 16);
  l2 := PadRight(copy(s, p+1, Length(s)), 16);
  Result := l1 + l2;
end;

procedure TACBrTEFScopeAPI.AbrirPinPad;
var
  ret: LongInt;
  bConfig, bExclusivo, aPorta, bPorta: Byte;
  Canal: Word;
  endereco: AnsiString;
begin
  FecharComunicacaoScope;
  GravarLog('AbrirPinPad');

  GravarLog('ScopeValidaInterfacePP( '+IntToStr(PP_INTERFACE_LIB_COMPARTILHADA)+' )');
  ret := xScopeValidaInterfacePP( PP_INTERFACE_LIB_COMPARTILHADA );
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> PC_OK) then
    TratarErroPinPadScope(ret);

  try
    AbrirComunicacaoScope;
    GravarLog('ScopeConsultaPP()');
    ret := xScopeConsultaPP(@bConfig, @bExclusivo, @bPorta);
    GravarLog('  ret: '+IntToStr(ret)+
              ', Config:'+IntToStr(bConfig)+
              ', Exclusivo:'+IntToStr(bExclusivo)+
              ', Porta:'+IntToStr(bPorta) );
    if (ret <> PC_OK) then
      TratarErroPinPadScope(ret);

    if (bExclusivo = 0) then
    begin
      aPorta := ConfigurarPortaPinPad(fPortaPinPad);
      if (aPorta = 0) then
        aPorta := bPorta;

      if (bConfig = PPCONF_MODO_ABECS) then
        fPinPadSeguro := True;

      if fPinPadSeguro then
      begin
        if (aPorta = 0) then
          Canal := CANAL_COMM_NONE
        else
          Canal := CANAL_COMM_SERIAL;

        GravarLog('ScopePPOpenSecure( '+IntToStr(Canal)+', '+IntToStr(aPorta)+' )');
        endereco := IntToStr(aPorta);
        ret := xScopePPOpenSecure(Canal, PAnsiChar(endereco));
      end
      else
      begin
        GravarLog('ScopePPOpen( '+IntToStr(aPorta)+' )');
        ret := xScopePPOpen(bPorta);
      end;

      GravarLog('  ret: '+IntToStr(ret));
      if (ret <> PC_OK) then
        TratarErroPinPadScope(ret);
    end;
  finally
    VerificarSeMantemConexaoScope;
  end;
end;

procedure TACBrTEFScopeAPI.FecharPinPad;
var
  ret: LongInt;
  s: String;
begin
  s := FormatarMsgPinPad(fMsgPinPad);
  GravarLog('ScopePPClose( '+s+' )');
  ret := xScopePPClose(PAnsiChar(s));
  GravarLog('  ret: '+IntToStr(ret));
  if (ret <> PC_OK) then
    TratarErroPinPadScope(ret);
end;

procedure TACBrTEFScopeAPI.ConfigurarColeta;
var
  ret: LongInt;
begin
  GravarLog('ScopeSetAplColeta()');
  ret := xScopeSetAplColeta();
  GravarLog('  ret: '+IntToStr(ret));

  ConfigurarPropriedadeScope( CFG_CANCELAR_OPERACAO_PINPAD, fPermitirCancelarOperacaoPinPad);
  ConfigurarPropriedadeScope( CFG_NAO_ABRIR_DIGITADO_COM_PP, not fPermitirCartaoDigitado);
  ConfigurarPropriedadeScope( CFG_DEVOLVER_SENHA_CRIPTOGRAFADA, True);
  ConfigurarPropriedadeScope( CFG_IMPRESSORA_CARBONADA, False);
  ConfigurarPropriedadeScope( CFG_ARMAZENA_EM_QUEDA, False);
  ConfigurarPropriedadeScope( CFG_ATUALIZA_TRANSACAO_EM_QUEDA, fConfirmarTransacoesPendentes);
  ConfigurarPropriedadeScope( CFG_PERMITIR_SAQUE, fPermitirSaque);
end;

function TACBrTEFScopeAPI.ConfigurarPropriedadeScope(AId: LongInt; Ligado: Boolean
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
  Result := (ret = RCS_SUCESSO);
end;

end.

