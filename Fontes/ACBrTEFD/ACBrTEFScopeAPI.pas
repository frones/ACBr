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

  sMsgAbrindoConexao = 'Abrindo comunicação...'+sLineBreak+'Empresa: %s, Filial %s';
  sMsgConctadoAoServidor = 'Conectado Scope em: %s';
  sMsgDesconectado = 'Desconectado Scope';
  sMsgInicioSessaoTEF = 'Iniciando sessão TEF';
  sMsgTransacaoDesfeita = 'A TRANSAÇÃO TEF ANTERIOR FOI DESFEITA.'+sLineBreak+'RETER O CUPOM TEF.';

const
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

  SCO_ERRO_PARM_1 = 64001;
  SCO_ERRO_PARM_2 = 64002;
  SCO_ERRO_PARM_3 = 64003;
  SCO_ERRO_PARM_4 = 64004;

  SCO_ERRO_ARQ_CICLO_TEF = 64264;
  SCO_TRN_EM_ANDAMENTO = 65024;
  SCO_API_NAO_INICIALIZADA = 65025;
  SCO_API_JA_INICIALIZADA = 65026;
  SCO_SRV_NOT_CFG = 65033;
  SCO_ERRO_LOGON_PDV = 65349;
  SCO_ERRO_CONFIG_PDV = 65430;

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
  R_GWCEL:                          LongInt = 90;

  {- entrada dos dados -}
  SCO_NONE:                         LongInt = $0000;
  SCO_TECLADO:                      LongInt = $0004;
  SCO_PIN_PAD:                      LongInt = $0008;
  SCO_CMC_7:                        LongInt = $0010;
  SCO_CARTAO_MAGNETICO:             LongInt = $0020;
  SCO_SCANNER:                      LongInt = $0040;

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
  SCO_PRIMEIRO_COLETA_DADOS:        LongInt = $FC00;
  SCO_COLETAR_CARTAO:               LongInt = $FC00;

  SCO_CARTAO:                       LongInt = $FC00;
  SCO_IMPRIME_CUPOM:                LongInt = $FC02;
  SCO_IMPRIME_CHEQUE:               LongInt = $FC08;
  SCO_SENHA:                        LongInt = $FC11;
  SCO_IMPRIME_CONSULTA:             LongInt = $FC1B;
  SCO_COLETA_VALOR_RECARGA:         LongInt = $FC2E;
  SCO_IMPRIME_CUPOM_PARCIAL:        LongInt = $FC46;
  SCO_COLETA_AUT_OU_CARTAO:         LongInt = $FC6C;
  SCO_COLETA_OPERADORA:             LongInt = $FC70;
  SCO_CARTAO_DIGITADO:              LongInt = $FC85;

  SCO_COLETA_CARTAO_EM_ANDAMENTO:   LongInt = $FCFC;
  SCO_COLETA_EM_ANDAMENTO:          LongInt = $FCFD;
  SCO_MOSTRA_INFO_RET_SCOPE:        LongInt = $FCFE; // mostra informações e retorna para scope
  SCO_MOSTRA_INFO_AGUARDA_CONF:     LongInt = $FCFF; // mostra informações e aguarda operador
  SCO_ULTIMO_COLETA_DADOS:          LongInt = $FCFF;

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

  {- Define os campos para as funcoes ScopeObtemCampo() e para ScopeObtemCampoExt() -}
  Cod_Rede:                         LongInt = $400000;

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

  ACAO_PROX:                        LongInt = $0008;
  ACAO_ANTER:                       LongInt = $0010;
  ACAO_CANCELAR:                    LongInt = $0020;

  COLETA_PROXIMO_ESTADO:            LongInt = 0;
  COLETA_ANTERIOR_ESTADO:           LongInt = 1;
  COLETA_CANCELAR:                  LongInt = 2;

  BIT6_ON:                          LongInt = $0040;
  BIT7_ON:                          LongInt = $0080;
  BIT8_ON:                          LongInt = $0100;
  BIT9_ON:                          LongInt = $0200;



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
    Bandeira:             Word;
    FormatoDado:          Word;
    HabTeclas:            Word;
    MsgOp1:               array [1..64]  of AnsiChar;
    MsgOp2:               array [1..64]  of AnsiChar;
    MsgCl1:               array [1..64]  of AnsiChar;
    MsgCl2:               array [1..64]  of AnsiChar;
    WrkKey:               array [1..17]  of AnsiChar;
    PosMasterKey:         Word;
    PAN:                  array [1..20]  of AnsiChar;
    UsaCriptoPinpad:      Byte;
    IdModoPagto:          Byte;
    AceitaCartaoDigitado: Byte;
    Reservado:            array [1..105] of AnsiChar;
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

  TACBrTEFScopeGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TACBrTEFScopeExibeMensagem = procedure(
    Mensagem: String;
    MilissegundosExibicao: Integer  // 0 - Para com OK; Positivo - aguarda Ok ou N milissegundos; Negativo - Apenas exibe a Msg (não aguarda)
    ) of object;

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
    fEnderecoIP: String;
    fFilial: String;
    fInicializada: Boolean;
    fMsgPinPad: String;
    fOnExibeMensagem: TACBrTEFScopeExibeMensagem;
    fOnGravarLog: TACBrTEFScopeGravarLog;
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
    xScopeConsultaPP: function(Configurado, UsoExclusivoScope, Porta: PByte): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPOpen: function(Porta: Word): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPOpenSecure: function(TipoCanal: word; Endereco: PAnsiChar): LongInt
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPClose: function(IdleMsg: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xScopePPGetCOMPort: function(szComEndereco: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    procedure SetPathLib(const AValue: String);
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetEmpresa(const AValue: String);
    procedure SetFilial(const AValue: String);
    procedure SetPDV(const AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetControleConexao(AValue: Boolean);
    procedure SetEnderecoIP(AValue: String);
    procedure SetPortaTCP(AValue: String);

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

    procedure AbrirSessaoTEF;
    procedure FecharSessaoTEF(Confirmar: Boolean; out TransacaoFoiDesfeita: Boolean);
    procedure VerificaSessaoTEFAnterior;

    procedure ContinuarTransacaoTEF;
    procedure AbrirPinPad;
    procedure ConfigurarColeta;
    function ConfigurarScope(AId: LongInt; Ligado: Boolean): Boolean;
    procedure FecharPinPad;

    procedure VerificarDiretorioDeTrabalho;
    procedure VerificarEAjustarScopeINI;
    function ConfigurarPortaPinPad(const APortaPinPad: String): Word;
    procedure ObterDadosScopeINI(out AEmpresa: String; out AFilial: String;
      out AEnderecoIP: String; out APortaTCP: String);

    procedure ExibirMensagem(const AMsg: String; TempoEspera: Integer = -1);

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

    property OnExibeMensagem: TACBrTEFScopeExibeMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    property OnGravarLog: TACBrTEFScopeGravarLog read fOnGravarLog write fOnGravarLog;

    procedure Inicializar;
    procedure DesInicializar;

    function ObterVersaoScope: String;
    function AcharPortaPinPad: String;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);
  end;

implementation

uses
  IniFiles, StrUtils,
  ACBrUtil.Strings, ACBrUtil.FilesIO;

{ TACBrTEFScopeAPI }

constructor TACBrTEFScopeAPI.Create;
begin
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
end;

destructor TACBrTEFScopeAPI.Destroy;
begin
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  DesInicializar;
  inherited Destroy;
end;

procedure TACBrTEFScopeAPI.Inicializar;
begin
  if fInicializada then
    Exit;

  GravarLog('TACBrTEFScopeAPI.Inicializar');

  VerificarDiretorioDeTrabalho;
  VerificarEAjustarScopeINI;
  LoadLibFunctions;
  AbrirPinPad;

  fInicializada := True;
  fConectado := False;
  fSessaoAberta := False;

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
  Result := ExtractFilePath(sLibName) + PathDelim + CScopeINI;
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
  TempoEspera: Integer);
begin
  GravarLog('  OnExibeMensagem( '+AMsg+
                                ', '+IntToStr(TempoEspera)+' )', True);
  fOnExibeMensagem(AMsg, TempoEspera);
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

procedure TACBrTEFScopeAPI.TratarErroScope(AErrorCode: LongInt);
var
  MsgErro: String;
begin
  case AErrorCode of
    SCO_SUCESSO: MsgErro := '';
    SCO_ERRO_PARM_1: MsgErro := 'Parâmetro 1 inválido';
    SCO_ERRO_PARM_2: MsgErro := 'Parâmetro 2 inválido';
    SCO_ERRO_PARM_3: MsgErro := 'Parâmetro 3 inválido';
    SCO_ERRO_PARM_4: MsgErro := 'Parâmetro 4 inválido';
    SCO_ERRO_ARQ_CICLO_TEF: MsgErro := 'Erro no arquivo de controle, finalização multi-TEF';
    SCO_TRN_EM_ANDAMENTO: MsgErro := 'Transação em andamento';
    SCO_API_NAO_INICIALIZADA: MsgErro := 'SCOPE API não foi inicializada';
    SCO_API_JA_INICIALIZADA: MsgErro := 'SCOPE API já foi inicializada';
    SCO_SRV_NOT_CFG: MsgErro := 'Servidor não configurado no arquivo '+CScopeINi;
    SCO_ERRO_LOGON_PDV: MsgErro := 'Verificar o erro retornado no log do ScopeSrv';
    SCO_ERRO_CONFIG_PDV: MsgErro := 'Verifique a configuração do perfil do PDV';
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
    sPDV := '001';

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
  if (ret <> SCO_SUCESSO) then
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
    ExibirMensagem(sMsgTransacaoDesfeita, 0);
    ExibirMensagem('');
  end;
end;

procedure TACBrTEFScopeAPI.ContinuarTransacaoTEF;
begin
  GravarLog('ContinuarTransacaoTEF');

end;

procedure TACBrTEFScopeAPI.AbrirPinPad;
var
  ret: LongInt;
  bConfig, bExclusivo, bPorta: Byte;
  Canal: Word;
  endereco: AnsiString;
begin
  GravarLog('AbrirPinPad');

  GravarLog('ScopeValidaInterfacePP( '+IntToStr(PP_INTERFACE_LIB_COMPARTILHADA)+' )');
  ret := xScopeValidaInterfacePP( PP_INTERFACE_LIB_COMPARTILHADA );
  GravarLog('  ret: '+IntToStr(ret));

  GravarLog('ScopeConsultaPP()');
  ret := xScopeConsultaPP(@bConfig, @bExclusivo, @bPorta);
  GravarLog('  ret: '+IntToStr(ret)+
            ', Config:'+IntToStr(bConfig)+
            ', Exclusivo:'+IntToStr(bExclusivo)+
            ', Porta:'+IntToStr(bPorta) );
  if ret <> PC_OK then
    DoException(ACBrStr(Format('Erro %d ao consultar o PinPad', [ret])));

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

