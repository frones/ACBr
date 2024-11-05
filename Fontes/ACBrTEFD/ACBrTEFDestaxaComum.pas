{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

unit ACBrTEFDestaxaComum;

interface

uses
  Classes, SysUtils, ACBrBase, blcksock;

const
  CACBRTEFDESTAXA_TERMINADOR = #13+#10+#09+#09+#13+#10+#09+#09+#09+#13+#10+#09+#09+#13+#10+#09;

type

  EACBrTEFDestaxaErro = class(Exception);

  TACBrTEFDestaxaServico = (
    dxsNenhum,
    dxsColetar,
    dxsConsultar,
    dxsExecutar,
    dxsFinalizar,
    dxsIniciar,
    dxsMostrar
  );

  TACBrTEFDestaxaTipoSolicitacao = (
    dtsNenhum,
    dtsDigiteDDD,
    dtsRedigiteDDD,
    dtsDigiteTelefone,
    dtsRedigiteTelefone,
    dtsDigiteDDDTelefone,
    dtsRedigiteDDDTelefone,
    dtsDigiteCPF,
    dtsRedigiteCPF,
    dtsDigiteRG,
    dtsRedigiteRG,
    dtsDigite4Ultimos,
    dtsDigiteCodigoSeguranca,
    dtsDigiteCNPJ,
    dtsRedigiteCNPJ,
    dtsDigiteDataDDMMAAAA,
    dtsDigiteDataDDMMAA,
    dtsDigiteDataDDMM,
    dtsDigiteDia,
    dtsDigiteMes,
    dtsDigiteAnoAA,
    dtsDigiteAnoAAAA,
    dtsDigiteDataNascimentoDDMMAAAA,
    dtsDigiteDataNascimentoDDMMAA,
    dtsDigiteDataNascimentoDDMM,
    dtsDigiteDiaNascimento,
    dtsDigiteMesNascimento,
    dtsDigiteAnoNascimentoAA,
    dtsDigiteAnoNascimentoAAAA,
    dtsDigiteIdentificacao,
    dtsDigiteCodigoFidelidade,
    dtsDigiteNumeroMesa,
    dtsDigiteQuantidadePessoas,
    dtsDigiteQuantidade,
    dtsDigiteNumeroBomba,
    dtsDigiteNumeroVaga,
    dtsDigiteNumeroCaixa,
    dtsDigiteCodigoVendedor,
    dtsDigiteCodigoGarcom,
    dtsDigiteNotaAtendimento,
    dtsDigiteNumeroNotaFiscal,
    dtsDigiteNumeroComanda,
    dtsDigitePlacaVeiculo,
    dtsDigiteQuilometragem,
    dtsDigiteQuilometragemInicial,
    dtsDigiteQuilometragemFinal,
    dtsDigitePorcentagem,
    dtsDigitePesquisaSatisfacao,
    dtsDigiteAvalieAtendimento,
    dtsDigiteToken,
    dtsDigiteNumeroCartao,
    dtsDigiteNumeroParcelas,
    dtsDigiteCodigoPlano,
    dtsDigiteCodigoProduto
  );

  TACBrTEFDestaxaRetornoRequisicao = (
    drqNenhum,
    drqConfirmarTransacao,
    drqExecutarServico,
    drqTempoLimiteExcedido,
    drqCancelarTransacao
  );

  TACBrTEFDestaxaRetornoResposta = (
    drsNenhum,
    drsSucessoComConfirmacao,            // Sucesso com confirmação da Aplicação Comercial
    drsSucessoSemConfirmacao,            // Sucesso sem confirmação da Aplicação Comercial
    drsErroSequencialInvalido,           // Erro: Sequencial inválido
    drsErroTransacaoCanceladaOperador,   // Erro: Transação cancelada pelo operador
    drsErroTransacaoCanceladaCliente,    // Erro: Transação cancelada pelo cliente
    drsErroParametrosInvalidos,          // Erro: Parâmetros insuficientes ou inválidos
    drsErroComunicacaoClienteServidor,   // Erro: Problemas entre o V$PagueClient e V$PagueServer
    drsErroComunicacaoServidorRede,      // Erro: Problemas entre o V$PagueServer e a Rede
    drsErroTempoLimiteExcedido,          // Erro: Tempo limite de espera excedido
    drsErroDesconhecido                  // Erro: Problema desconhecido
  );

  TACBrTEFDestaxaColetaRetorno = (
    dcrExecutarProcedimento,        // Executar procedimento / Procedimento Executado
    dcrFinalizarProcedimento,       // Finalizar procedimento
    dcrErroParametrosInvalidos,     // Parâmetros insuficientes ou inválidos
    dcrErroTempoLimiteExcedido,     // Tempo limite de espera excedido
    dcrCancelarProcedimento         // Cancelar o procedimento
  );

  TACBrTEFDestaxaColetaTipo = (
    dctNenhum,
    dctNaoExibivel,
    dctAlfabetico,
    dctDataHora,
    dctNumerico,
    dctAlfanumerico
  );

  TACBrTEFDestaxaBinarioTipo = (
    dbtNenhum,
    dbtGIF,
    dbtJPEG,
    dbtMP3,
    dbtMPEG,
    dbtPDF,
    dbtPNG,
    dbtTXT
  );

  TACBrTEFDestaxaFinanciado = (
    dxfNenhum,
    dxfEstabelecimento,
    dxfAdministradora,
    dxfAVista
  );

  TACBrTEFDestaxaPagamento = (
    dpgNenhum,
    dpgAVista,
    dpgParcelado,
    dpgPreDatado
  );

  TACBrDestaxaTipoCartao = (
    dtcNenhum,
    dtcDebito,
    dtcCredito
  );

  TACBrTEFDestaxaMensagem = (
    dmsNenhum,
    dmsSemComunicacao,
    dmsAguardeLiberar,
    dmsSelecioneOpcao,
    dmsAguardeLiberarProduto,
    dmsConfirmeSelecaoProduto,
    dmsProdutoLiberado,
    dmsProdutoNaoLiberado,
    dmsRetireProduto,
    dmsSelecioneProduto,
    dmsConfirmeSelecao,
    dmsSemSinal,
    dmsTransacaoAprovada,
    dmsTransacaoCancelada,
    dmsAguarde
  );

  TACBrTEFDestaxaClient = class;

  { TACBrTEFDestaxaTransacaoClass }

  TACBrTEFDestaxaTransacaoClass = class
  private
    fOwner: TACBrTEFDestaxaClient;
    faplicacao: String;
    fautomacao_coleta_mensagem: String;
    fautomacao_coleta_sequencial: Integer;
    fautomacao_coleta_timeout: Integer;
    fautomacao_coleta_transacao_resposta: String;
    fmensagem: String;
    fsequencial: Integer;
    fColetaRetornoSequencial: Integer;
    ftransacao: String;
    ftransacao_banco: Integer;
    ftransacao_binario: AnsiString;
    ftransacao_cheque_cmc7: String;
    ftransacao_cheque_vencimento: String;
    ftransacao_codigo_barras: String;
    ftransacao_concessionaria: String;
    ftransacao_data: TDateTime;
    ftransacao_financiado: String;
    ftransacao_informacao: String;
    ftransacao_linha_digitavel: String;
    ftransacao_nsu: String;
    ftransacao_opcao: String;
    ftransacao_parcela: Integer;
    ftransacao_parcela_entrada: Double;
    ftransacao_parcela_valor: Double;
    ftransacao_parcela_vencimento: TDateTime;
    ftransacao_produto: String;
    ftransacao_rede: String;
    ftransacao_telefone_ddd: String;
    ftransacao_telefone_numero: String;
    ftransacao_timeout: String;
    ftransacao_valor: Double;
    ftransacao_valor_ajuste: Double;
    ftransacao_vencimento: TDateTime;
    fversao: String; 
    fservico: TACBrTEFDestaxaServico;
    ftransacao_tipo_cartao: TACBrDestaxaTipoCartao;
    ftransacao_pagamento: TACBrTEFDestaxaPagamento;
    ftransacao_binario_tipo: TACBrTEFDestaxaBinarioTipo;
    fautomacao_coleta_retorno: TACBrTEFDestaxaColetaRetorno;
    fautomacao_coleta_mensagem_tipo: TACBrTEFDestaxaBinarioTipo;
    function GetAsString: AnsiString;
    procedure SetAsString(aValue: AnsiString);
  protected
    procedure PreencherCampo(const aStrList: TStringList; const aCampo: String; const aConteudo: Double); overload;
    procedure PreencherCampo(const aStrList: TStringList; const aCampo: String; const aConteudo: Integer); overload;
    procedure PreencherCampo(const aStrList: TStringList; const aCampo, aConteudo: AnsiString; PreencherVazio: Boolean = False); overload;

    procedure PreencherCampos(const aStrList: TStringList); virtual;
    procedure CarregarCampos(const aStrList: TStringList); virtual;
  public
    constructor Create(aOwner: TACBrTEFDestaxaClient); virtual;
    procedure Clear; virtual;

    property servico: TACBrTEFDestaxaServico read fServico write fServico;
    property aplicacao: String read faplicacao write faplicacao;
    property mensagem: String read fmensagem write fmensagem;
    property sequencial: Integer read fsequencial write fsequencial;
    property transacao: String read ftransacao write ftransacao;
    property transacao_banco: Integer read ftransacao_banco write ftransacao_banco;
    property transacao_binario: AnsiString read ftransacao_binario write ftransacao_binario;
    property transacao_binario_tipo: TACBrTEFDestaxaBinarioTipo read ftransacao_binario_tipo write ftransacao_binario_tipo;
    property transacao_cheque_cmc7: String read ftransacao_cheque_cmc7 write ftransacao_cheque_cmc7;
    property transacao_cheque_vencimento: String read ftransacao_cheque_vencimento write ftransacao_cheque_vencimento;
    property transacao_codigo_barras: String read ftransacao_codigo_barras write ftransacao_codigo_barras;
    property transacao_concessionaria: String read ftransacao_concessionaria write ftransacao_concessionaria;
    property transacao_data: TDateTime read ftransacao_data write ftransacao_data;
    property transacao_financiado: String read ftransacao_financiado write ftransacao_financiado;
    property transacao_informacao: String read ftransacao_informacao write ftransacao_informacao;
    property transacao_linha_digitavel: String read ftransacao_linha_digitavel write ftransacao_linha_digitavel;
    property transacao_nsu: String read ftransacao_nsu write ftransacao_nsu;
    property transacao_opcao: String read ftransacao_opcao write ftransacao_opcao;
    property transacao_pagamento: TACBrTEFDestaxaPagamento read ftransacao_pagamento write ftransacao_pagamento;
    property transacao_parcela: Integer read ftransacao_parcela write ftransacao_parcela;
    property transacao_parcela_entrada: Double read ftransacao_parcela_entrada write ftransacao_parcela_entrada;
    property transacao_parcela_valor: Double read ftransacao_parcela_valor write ftransacao_parcela_valor;
    property transacao_parcela_vencimento: TDateTime read ftransacao_parcela_vencimento write ftransacao_parcela_vencimento;
    property transacao_produto: String read ftransacao_produto write ftransacao_produto;
    property transacao_rede: String read ftransacao_rede write ftransacao_rede;
    property transacao_telefone_ddd: String read ftransacao_telefone_ddd write ftransacao_telefone_ddd;
    property transacao_telefone_numero: String read ftransacao_telefone_numero write ftransacao_telefone_numero;
    property transacao_timeout: String read ftransacao_timeout write ftransacao_timeout;
    property transacao_tipo_cartao: TACBrDestaxaTipoCartao read ftransacao_tipo_cartao write ftransacao_tipo_cartao;
    property transacao_valor: Double read ftransacao_valor write ftransacao_valor;
    property transacao_valor_ajuste: Double read ftransacao_valor_ajuste write ftransacao_valor_ajuste;
    property transacao_vencimento: TDateTime read ftransacao_vencimento write ftransacao_vencimento;
    property versao: String read fversao write fversao;
    property automacao_coleta_mensagem: String read fautomacao_coleta_mensagem write fautomacao_coleta_mensagem;
    property automacao_coleta_mensagem_tipo: TACBrTEFDestaxaBinarioTipo read fautomacao_coleta_mensagem_tipo write fautomacao_coleta_mensagem_tipo;
    property automacao_coleta_retorno: TACBrTEFDestaxaColetaRetorno read fautomacao_coleta_retorno write fautomacao_coleta_retorno;
    property automacao_coleta_sequencial: Integer read fautomacao_coleta_sequencial write fautomacao_coleta_sequencial;
    property automacao_coleta_transacao_resposta: String read fautomacao_coleta_transacao_resposta write fautomacao_coleta_transacao_resposta;
    property automacao_coleta_timeout: Integer read fautomacao_coleta_timeout write fautomacao_coleta_timeout;

    property AsString: AnsiString read GetAsString write SetAsString;
  end;

  { TACBrTEFDestaxaTransacaoRequisicao }

  TACBrTEFDestaxaTransacaoRequisicao = class(TACBrTEFDestaxaTransacaoClass)
  private
    faplicacao_tela: String;
    fautomacao_coleta_informacao: String;
    fcomputador_endereco: String;
    fcomputador_nome: String;
    festabelecimento: String;
    floja: String;
    fretorno: TACBrTEFDestaxaRetornoRequisicao;
    fterminal: String;
    ftransacao_cartao_codigo_seguranca: Integer;
    ftransacao_cartao_nome: String;
    ftransacao_cartao_numero: String;
    ftransacao_cartao_validade: String;
    ftransacao_cnpj_cpf: String;
    ftransacao_comprovante_email: String;
    ftransacao_comprovante_sms: String;
    ftransacao_documento_fiscal: String;
    ftransacao_loja_cnpj_cpf: String;
    ftransacao_nome: String;
    ftransacao_processar: String;
    ftransacao_subadquirente: String;
    ftransacao_valor_maximo: Double;
    ftransacao_valor_minimo: Double;
    ftransacao_vendedor: String;
    procedure Setaplicacao_tela(aValue: String);
  protected
    procedure PreencherCampos(const aStrList: TStringList); override;
  public
    procedure Clear; override;

    property retorno: TACBrTEFDestaxaRetornoRequisicao read fretorno write fretorno;
    property aplicacao_tela: String  read faplicacao_tela write Setaplicacao_tela;
    property computador_endereco: String read fcomputador_endereco write fcomputador_endereco;
    property computador_nome: String read fcomputador_nome write fcomputador_nome;
    property estabelecimento: String read festabelecimento write festabelecimento;
    property loja: String read floja write floja;
    property terminal: String read fterminal write fterminal;
    property transacao_cartao_codigo_seguranca: Integer read ftransacao_cartao_codigo_seguranca write ftransacao_cartao_codigo_seguranca;
    property transacao_cartao_nome: String read ftransacao_cartao_nome write ftransacao_cartao_nome; 
    property transacao_cartao_numero: String read ftransacao_cartao_numero write ftransacao_cartao_numero;
    property transacao_cartao_validade: String read ftransacao_cartao_validade write ftransacao_cartao_validade;
    property transacao_cnpj_cpf: String read ftransacao_cnpj_cpf write ftransacao_cnpj_cpf;
    property transacao_comprovante_email: String read ftransacao_comprovante_email write ftransacao_comprovante_email;
    property transacao_comprovante_sms: String read ftransacao_comprovante_sms write ftransacao_comprovante_sms;
    property transacao_documento_fiscal: String read ftransacao_documento_fiscal write ftransacao_documento_fiscal;
    property transacao_loja_cnpj_cpf: String read ftransacao_loja_cnpj_cpf write ftransacao_loja_cnpj_cpf;
    property transacao_nome: String read ftransacao_nome write ftransacao_nome;
    property transacao_processar: String read ftransacao_processar write ftransacao_processar; 
    property transacao_subadquirente: String read ftransacao_subadquirente write ftransacao_subadquirente;
    property transacao_valor_minimo: Double read ftransacao_valor_minimo write ftransacao_valor_minimo;
    property transacao_valor_maximo: Double read ftransacao_valor_maximo write ftransacao_valor_maximo;
    property transacao_vendedor: String read ftransacao_vendedor write ftransacao_vendedor;
    property automacao_coleta_informacao: String read fautomacao_coleta_informacao write fautomacao_coleta_informacao;
  end;

  { TACBrTEFDestaxaTransacaoResposta }

  TACBrTEFDestaxaTransacaoResposta = class(TACBrTEFDestaxaTransacaoClass)
  private
    fautomacao_coleta_mascara: String;
    fautomacao_coleta_opcao: String;
    fautomacao_coleta_palavra_chave: String;
    fautomacao_coleta_tipo: TACBrTEFDestaxaColetaTipo;
    fcodigo_bandeira: String;
    festado: Integer;
    fretorno: TACBrTEFDestaxaRetornoResposta;
    ftransacao_autorizacao: String;
    ftransacao_comprovante_1via: String;
    ftransacao_comprovante_2via: String;
    ftransacao_comprovante_resumido: String;
    ftransacao_identificacao: String;
    ftransacao_nsu_rede: String;
    ftransacao_operadora: String;
    ftransacao_payment_id: String;
    ftransacao_rede_cnpj: String;
    ftransacao_resposta: String;
    ftransacao_subadquirente: String;
    ftransacao_taxa: String;
    ftransacao_valor_saque: Double;
    ftransacao_valor_taxa_embarque: String;
    ftransacao_valor_taxa_servico: String;
  protected
    procedure PreencherCampos(const aStrList: TStringList); override;
    procedure CarregarCampos(const aStrList: TStringList); override;
  public
    procedure Clear; override;

    property retorno: TACBrTEFDestaxaRetornoResposta read fretorno;
    property estado: Integer read festado;
    property transacao_autorizacao: String read ftransacao_autorizacao;
    property transacao_comprovante_1via: String read ftransacao_comprovante_1via;
    property transacao_comprovante_2via: String read ftransacao_comprovante_2via;
    property transacao_comprovante_resumido: String read ftransacao_comprovante_resumido;
    property transacao_identificacao: String read ftransacao_identificacao write ftransacao_identificacao;  // Número Lógico
    property transacao_nsu_rede: String read ftransacao_nsu_rede write ftransacao_nsu_rede;
    property transacao_operadora: String read ftransacao_operadora write ftransacao_operadora;
    property transacao_payment_id: String read ftransacao_payment_id write ftransacao_payment_id;  // End2End Pix/Wallet
    property codigo_bandeira: String read fcodigo_bandeira write fcodigo_bandeira;
    property transacao_rede_cnpj: String read ftransacao_rede_cnpj write ftransacao_rede_cnpj;
    property transacao_resposta: String read ftransacao_resposta write ftransacao_resposta;
    property transacao_subadquirente: String read ftransacao_subadquirente write ftransacao_subadquirente;
    property transacao_taxa: String read ftransacao_taxa write ftransacao_taxa;
    property transacao_valor_saque: Double read ftransacao_valor_saque write ftransacao_valor_saque;
    property transacao_valor_taxa_embarque: String read ftransacao_valor_taxa_embarque write ftransacao_valor_taxa_embarque;
    property transacao_valor_taxa_servico: String read ftransacao_valor_taxa_servico write ftransacao_valor_taxa_servico;
    property automacao_coleta_mascara: String read fautomacao_coleta_mascara write fautomacao_coleta_mascara;
    property automacao_coleta_opcao: String read fautomacao_coleta_opcao write fautomacao_coleta_opcao;
    property automacao_coleta_palavra_chave: String read fautomacao_coleta_palavra_chave write fautomacao_coleta_palavra_chave;
    property automacao_coleta_tipo: TACBrTEFDestaxaColetaTipo read fautomacao_coleta_tipo write fautomacao_coleta_tipo;
  end;

  { TACBrTEFDestaxaSocket }

  TACBrTEFDestaxaSocket = class(TTCPBlockSocket)
  private
    fOnGravarLog: TACBrGravarLog;
    fDestaxaClient: TACBrTEFDestaxaClient;
    fResposta: TACBrTEFDestaxaTransacaoResposta;
    fRequisicao: TACBrTEFDestaxaTransacaoRequisicao;
    function GetRequisicao: TACBrTEFDestaxaTransacaoRequisicao;
    function GetResposta: TACBrTEFDestaxaTransacaoResposta;

    procedure Transmitir;
    procedure TratarErro;
  public
    constructor Create(aOwner: TACBrTEFDestaxaClient);
    destructor Destroy; override;

    function Conectar: Integer;
    function Desconectar: Integer;
    procedure Reconectar;

    procedure EnviarComando;

    property Requisicao: TACBrTEFDestaxaTransacaoRequisicao read GetRequisicao;
    property Resposta: TACBrTEFDestaxaTransacaoResposta read GetResposta;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
  end;

  { TACBrTEFDestaxaClient }

  TACBrTEFDestaxaClient = class
  private
    fAplicacao: String;
    fAplicacaoTela: String;
    fAplicacaoVersao: String;
    fEnderecoIP: String;
    fEstabelecimento: String;
    fLoja: String;
    fPorta: String;
    fTerminador: AnsiString;
    fTerminal: String;
    fEmTransacao: Boolean;
    fOnGravarLog: TACBrGravarLog;
    fSocket: TACBrTEFDestaxaSocket;
    fTimeOut: Integer;

    function GetRequisicao: TACBrTEFDestaxaTransacaoRequisicao;
    function GetResposta: TACBrTEFDestaxaTransacaoResposta;
    function Socket: TACBrTEFDestaxaSocket;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure GravarLog(const aString: AnsiString; Traduz: Boolean = False);
    procedure Executar(aTransacao: String);

    function Iniciar: Boolean;
    function Finalizar: Boolean;
    function Consultar: Boolean;
    function Mostrar(aMensagem: TACBrTEFDestaxaMensagem): Boolean;
    function Coletar(const aSolicitacao: TACBrTEFDestaxaTipoSolicitacao; const aTamanhoMin: Integer = 0;
      const aTamanhoMax: Integer = 0; const aTempoEspera: Integer = 0): AnsiString;

    property Resposta: TACBrTEFDestaxaTransacaoResposta read GetResposta;
    property Requisicao: TACBrTEFDestaxaTransacaoRequisicao read GetRequisicao;

    property Loja: String read fLoja write fLoja;
    property Terminal: String read fTerminal write fTerminal;
    property Aplicacao: String read fAplicacao write fAplicacao;
    property AplicacaoTela: String read fAplicacaoTela write fAplicacaoTela;
    property AplicacaoVersao: String read fAplicacaoVersao write fAplicacaoVersao;
    property Estabelecimento: String read fEstabelecimento write fEstabelecimento;

    property EnderecoIP: String read fEnderecoIP write fEnderecoIP;
    property Porta: String read fPorta write fPorta;
    property TimeOut: Integer read fTimeOut write fTimeOut;
    property Terminador: AnsiString read fTerminador write fTerminador;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
  end;

  function DestaxaServicoToString(const aServico: TACBrTEFDestaxaServico): String;
  function StringToDestaxaServico(const aString: String): TACBrTEFDestaxaServico;
  function DestaxaBinarioTipoToString(const aTipo: TACBrTEFDestaxaBinarioTipo): String;
  function StringToDestaxaBinarioTipo(const aString: String): TACBrTEFDestaxaBinarioTipo;
  function DestaxaTipoCartaoToString(const aCartao: TACBrDestaxaTipoCartao): String;
  function StringToDestaxaTipoCartao(const aString: String): TACBrDestaxaTipoCartao;
  function DestaxaPagamentoToString(const aPagamento: TACBrTEFDestaxaPagamento): String;
  function StringToDestaxaPagamento(const aString: String): TACBrTEFDestaxaPagamento;
  function DestaxaColetaRetornoToInteger(const aColetaRetorno: TACBrTEFDestaxaColetaRetorno): Integer;
  function IntegerToDestaxaColetaRetorno(const aColetaRetorno: Integer): TACBrTEFDestaxaColetaRetorno;
  function DestaxaRetornoRequisicaoToInteger(const aRetorno: TACBrTEFDestaxaRetornoRequisicao): Integer;
  function IntegerToDestaxaRetornoRequisicao(const aRetorno: Integer): TACBrTEFDestaxaRetornoRequisicao;
  function DestaxaRetornoRespostaToInteger(const aRetorno: TACBrTEFDestaxaRetornoResposta): Integer;
  function IntegerToDestaxaRetornoResposta(const aRetorno: Integer): TACBrTEFDestaxaRetornoResposta;
  function DestaxaColetaTipoToString(const aTipo: TACBrTEFDestaxaColetaTipo): String;
  function StringToDestaxaColetaTipo(const aString: String): TACBrTEFDestaxaColetaTipo;
  function DestaxaMensagemToString(const aMensagem: TACBrTEFDestaxaMensagem): String;
  function StringToDestaxaMensagem(const aString: String): TACBrTEFDestaxaMensagem;

implementation

uses
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Base;

function DestaxaServicoToString(const aServico: TACBrTEFDestaxaServico): String;
begin
  Result := EmptyStr;
  case aServico of
    dxsColetar: Result := 'coletar';
    dxsConsultar: Result := 'consultar';
    dxsExecutar: Result := 'executar';
    dxsFinalizar: Result := 'finalizar';
    dxsIniciar: Result := 'iniciar';
    dxsMostrar: Result := 'mostrar';
  end;
end;

function StringToDestaxaServico(const aString: String): TACBrTEFDestaxaServico;
begin
  if aString = 'coletar' then
    Result := dxsColetar
  else if aString = 'consultar' then
    Result := dxsConsultar
  else if aString = 'executar' then
    Result := dxsExecutar
  else if aString = 'finalizar' then
    Result := dxsFinalizar
  else if aString = 'iniciar' then
    Result := dxsIniciar
  else if aString = 'mostrar' then
    Result := dxsMostrar
  else
    Result := dxsNenhum;
end;

function DestaxaBinarioTipoToString(const aTipo: TACBrTEFDestaxaBinarioTipo): String;
begin
  Result := EmptyStr;
  case aTipo of
    dbtGIF: Result := 'GIF';
    dbtJPEG: Result := 'JPEG';
    dbtMP3: Result := 'MP3';
    dbtMPEG: Result := 'MPEG';
    dbtPDF: Result := 'PDF';
    dbtPNG: Result := 'PNG';
    dbtTXT: Result := 'TXT';
  end;
end;

function StringToDestaxaBinarioTipo(const aString: String): TACBrTEFDestaxaBinarioTipo;
begin
  Result := dbtNenhum;
  if aString = 'GIF' then
    Result := dbtGIF
  else if aString = 'JPEG' then
    Result := dbtJPEG
  else if aString = 'MP3' then
    Result := dbtMP3
  else if aString = 'MPEG' then
    Result := dbtMPEG
  else if aString = 'PDF' then
    Result := dbtPDF
  else if aString = 'PNG' then
    Result := dbtPNG
  else if aString = 'TXT' then
    Result := dbtTXT;
end;

function DestaxaTipoCartaoToString(const aCartao: TACBrDestaxaTipoCartao): String;
begin
  Result := EmptyStr;
  case aCartao of
    dtcDebito: Result := 'Débito';
    dtcCredito: Result := 'Credito';
  end;
end;

function StringToDestaxaTipoCartao(const aString: String): TACBrDestaxaTipoCartao;
begin
  Result := dtcNenhum;
  if aString = 'Débito' then
    Result := dtcDebito
  else if aString = 'Credito' then
    Result := dtcCredito;
end;

function DestaxaPagamentoToString(const aPagamento: TACBrTEFDestaxaPagamento): String;
begin
  Result := EmptyStr;
  case APagamento of
    dpgAVista: Result := 'A vista';
    dpgParcelado: Result := 'Parcelado';
    dpgPreDatado: Result := 'Pré-datado';
  end;
end;

function StringToDestaxaPagamento(const aString: String): TACBrTEFDestaxaPagamento;
begin
  Result := dpgNenhum;
  if AString = 'A vista' then
    Result := dpgAVista
  else if AString = 'Parcelado' then
    Result := dpgParcelado
  else if AString = 'Pré-datado' then
    Result := dpgPreDatado;
end;

function DestaxaColetaRetornoToInteger(const aColetaRetorno: TACBrTEFDestaxaColetaRetorno): Integer;
begin
  case aColetaRetorno of
    dcrExecutarProcedimento: Result := 0;
    dcrFinalizarProcedimento: Result := 1;
    dcrErroParametrosInvalidos: Result := 5;
    dcrErroTempoLimiteExcedido: Result := 8;
    dcrCancelarProcedimento: Result := 9;
  end;
end;

function IntegerToDestaxaColetaRetorno(const aColetaRetorno: Integer): TACBrTEFDestaxaColetaRetorno;
begin
  if aColetaRetorno = 0 then
    Result := dcrExecutarProcedimento
  else if aColetaRetorno = 1 then
    Result := dcrFinalizarProcedimento
  else if aColetaRetorno = 5 then
    Result := dcrErroParametrosInvalidos
  else if aColetaRetorno = 8 then
    Result := dcrErroTempoLimiteExcedido
  else if aColetaRetorno = 9 then
    Result := dcrCancelarProcedimento
end;

function IntegerToDestaxaRetornoRequisicao(const aRetorno: Integer): TACBrTEFDestaxaRetornoRequisicao;
begin
  Result := drqNenhum;
  if aRetorno = 0 then
    Result := drqConfirmarTransacao
  else if aRetorno = 1 then
    Result := drqExecutarServico
  else if aRetorno = 8 then
    Result := drqTempoLimiteExcedido
  else if aRetorno = 9 then
    Result := drqCancelarTransacao;
end;

function DestaxaRetornoRequisicaoToInteger(
  const aRetorno: TACBrTEFDestaxaRetornoRequisicao): Integer;
begin
  Result := -1;
  case aRetorno of
    drqConfirmarTransacao: Result := 0;
    drqExecutarServico: Result := 1;
    drqTempoLimiteExcedido: Result := 8;
    drqCancelarTransacao: Result := 9;
  end;
end;

function DestaxaRetornoRespostaToInteger(
  const aRetorno: TACBrTEFDestaxaRetornoResposta): Integer;
begin
  Result := -1;
  case aRetorno of
    drsSucessoComConfirmacao: Result := 0;
    drsSucessoSemConfirmacao: Result := 1;
    drsErroSequencialInvalido: Result := 2;
    drsErroTransacaoCanceladaOperador: Result := 3;
    drsErroTransacaoCanceladaCliente: Result := 4;
    drsErroParametrosInvalidos: Result := 5;
    drsErroComunicacaoClienteServidor: Result := 6;
    drsErroComunicacaoServidorRede: Result := 7;
    drsErroTempoLimiteExcedido: Result := 8;
    drsErroDesconhecido: Result := 9;
  end;
end;

function IntegerToDestaxaRetornoResposta(const aRetorno: Integer
  ): TACBrTEFDestaxaRetornoResposta;
begin
  Result := drsNenhum;
  case aRetorno of
    0: Result := drsSucessoComConfirmacao;
    1: Result := drsSucessoSemConfirmacao;
    2: Result := drsErroSequencialInvalido;
    3: Result := drsErroTransacaoCanceladaOperador;
    4: Result := drsErroTransacaoCanceladaCliente;
    5: Result := drsErroParametrosInvalidos;
    6: Result := drsErroComunicacaoClienteServidor;
    7: Result := drsErroComunicacaoServidorRede;
    8: Result := drsErroTempoLimiteExcedido;
    9: Result := drsErroDesconhecido;
  end;
end;

function DestaxaColetaTipoToString(const aTipo: TACBrTEFDestaxaColetaTipo): String;
begin
  Result := EmptyStr;
  case aTipo of
    dctNaoExibivel: Result := '(*)';
    dctAlfabetico: Result := '(A)';
    dctDataHora: Result := '(D)';
    dctNumerico: Result := '(N)';
    dctAlfanumerico: Result := '(X)';
  end;
end;

function StringToDestaxaColetaTipo(const aString: String): TACBrTEFDestaxaColetaTipo;
begin
  Result := dctNenhum;
  if aString = '(A)' then
    Result := dctAlfabetico
  else if aString = '(D)' then
    Result := dctDataHora
  else if aString = '(N)' then
    Result := dctNumerico
  else if aString = '(X)' then
    Result := dctAlfanumerico;
end;

function DestaxaMensagemToString(const aMensagem: TACBrTEFDestaxaMensagem): String;
begin
  Result := EmptyStr;
  case aMensagem of
    dmsSemComunicacao: Result := 'CMCE';
    dmsAguardeLiberar: Result := 'LIBA';
    dmsSelecioneOpcao: Result := 'OPCS';
    dmsAguardeLiberarProduto: Result := 'PDTA';
    dmsConfirmeSelecaoProduto: Result := 'PDTC';
    dmsProdutoLiberado: Result := 'PDTL';
    dmsProdutoNaoLiberado: Result := 'PDTN';
    dmsRetireProduto: Result := 'PDTR';
    dmsSelecioneProduto: Result := 'PDTS';
    dmsConfirmeSelecao: Result := 'SELC';
    dmsSemSinal: Result := 'SIGE';
    dmsTransacaoAprovada: Result := 'TRAA';
    dmsTransacaoCancelada: Result := 'TRAC';
    dmsAguarde: Result := 'WAIT';
  end;
end;

function StringToDestaxaMensagem(const aString: String): TACBrTEFDestaxaMensagem;
begin
  Result := dmsNenhum;
  if aString = 'CMCE' then
    Result := dmsSemComunicacao
  else if aString = 'LIBA' then
    Result := dmsAguardeLiberar
  else if aString = 'OPCS' then
    Result := dmsSelecioneOpcao
  else if aString = 'PDTA' then
    Result := dmsAguardeLiberarProduto
  else if aString = 'PDTC' then
    Result := dmsConfirmeSelecaoProduto
  else if aString = 'PDTL' then
    Result := dmsProdutoLiberado
  else if aString = 'PDTN' then
    Result := dmsProdutoNaoLiberado
  else if aString = 'PDTR' then
    Result := dmsRetireProduto
  else if aString = 'PDTS' then
    Result := dmsSelecioneProduto
  else if aString = 'SELC' then
    Result := dmsConfirmeSelecao
  else if aString = 'SIGE' then
    Result := dmsSemSinal
  else if aString = 'TRAA' then
    Result := dmsTransacaoAprovada
  else if aString = 'TRAC' then
    Result := dmsTransacaoCancelada
  else if aString = 'WAIT' then
    Result := dmsAguarde;
end;

{ TACBrTEFDestaxaTransacaoResposta }

procedure TACBrTEFDestaxaTransacaoResposta.PreencherCampos(const aStrList: TStringList);
begin
  inherited PreencherCampos(aStrList);
  PreencherCampo(aStrList, 'estado', festado);
  PreencherCampo(aStrList, 'transacao_valor_saque', ftransacao_valor_saque);
  PreencherCampo(aStrList, 'automacao_coleta_mascara', fautomacao_coleta_mascara);
  PreencherCampo(aStrList, 'automacao_coleta_opcao', fautomacao_coleta_opcao);
  PreencherCampo(aStrList, 'automacao_coleta_palavra_chave', fautomacao_coleta_palavra_chave);
  PreencherCampo(aStrList, 'codigo_bandeira', fcodigo_bandeira);
  PreencherCampo(aStrList, 'transacao_autorizacao', ftransacao_autorizacao);
  PreencherCampo(aStrList, 'transacao_comprovante_1via', ftransacao_comprovante_1via);
  PreencherCampo(aStrList, 'transacao_comprovante_2via', ftransacao_comprovante_2via);
  PreencherCampo(aStrList, 'transacao_comprovante_resumido', ftransacao_comprovante_resumido);
  PreencherCampo(aStrList, 'transacao_identificacao', ftransacao_identificacao);
  PreencherCampo(aStrList, 'transacao_nsu_rede', ftransacao_nsu_rede);
  PreencherCampo(aStrList, 'transacao_operadora', ftransacao_operadora);
  PreencherCampo(aStrList, 'transacao_payment_id', ftransacao_payment_id);
  PreencherCampo(aStrList, 'transacao_rede_cnpj', ftransacao_rede_cnpj);
  PreencherCampo(aStrList, 'transacao_resposta', ftransacao_resposta);
  PreencherCampo(aStrList, 'transacao_subadquirente', ftransacao_subadquirente);
  PreencherCampo(aStrList, 'transacao_taxa', ftransacao_taxa);
  PreencherCampo(aStrList, 'transacao_valor_taxa_embarque', ftransacao_valor_taxa_embarque);
  PreencherCampo(aStrList, 'transacao_valor_taxa_servico', ftransacao_valor_taxa_servico);
  PreencherCampo(aStrList, 'automacao_coleta_tipo', DestaxaColetaTipoToString(automacao_coleta_tipo));
  PreencherCampo(aStrList, 'retorno', DestaxaRetornoRespostaToInteger(fretorno));
end;

procedure TACBrTEFDestaxaTransacaoResposta.CarregarCampos(const aStrList: TStringList);
begin
  inherited CarregarCampos(aStrList);
  
  fautomacao_coleta_mascara := aStrList.Values['automacao_coleta_mascara'];
  fautomacao_coleta_opcao := aStrList.Values['automacao_coleta_opcao'];
  fautomacao_coleta_palavra_chave := aStrList.Values['automacao_coleta_palavra_chave'];
  fcodigo_bandeira := aStrList.Values['codigo_bandeira'];
  festado := StrToIntDef(aStrList.Values['estado'], -1);
  ftransacao_autorizacao := aStrList.Values['transacao_autorizacao'];
  ftransacao_comprovante_1via := aStrList.Values['transacao_comprovante_1via'];
  ftransacao_comprovante_2via := aStrList.Values['transacao_comprovante_2via'];
  ftransacao_comprovante_resumido := aStrList.Values['transacao_comprovante_resumido'];
  ftransacao_identificacao := aStrList.Values['transacao_identificacao'];
  ftransacao_nsu_rede := aStrList.Values['transacao_nsu_rede'];
  ftransacao_operadora := aStrList.Values['transacao_operadora'];
  ftransacao_payment_id := aStrList.Values['transacao_payment_id'];
  ftransacao_rede_cnpj := aStrList.Values['transacao_rede_cnpj'];
  ftransacao_resposta := aStrList.Values['transacao_resposta'];
  ftransacao_subadquirente := aStrList.Values['transacao_subadquirente'];
  ftransacao_taxa := aStrList.Values['transacao_taxa'];
  ftransacao_valor_saque := StrToFloatDef(aStrList.Values['transacao_valor_saque'], 0);
  ftransacao_valor_taxa_embarque := aStrList.Values['transacao_valor_taxa_embarque'];
  ftransacao_valor_taxa_servico := aStrList.Values['transacao_valor_taxa_servico'];
  fautomacao_coleta_tipo := StringToDestaxaColetaTipo(aStrList.Values['automacao_coleta_tipo']);
  fretorno := IntegerToDestaxaRetornoResposta(StrToIntDef(aStrList.Values['retorno'], -1));
end;

procedure TACBrTEFDestaxaTransacaoResposta.Clear;
begin
  inherited Clear;
  fautomacao_coleta_mascara := EmptyStr;
  fautomacao_coleta_opcao := EmptyStr;
  fautomacao_coleta_palavra_chave := EmptyStr;
  fautomacao_coleta_tipo := dctNenhum;
  fcodigo_bandeira := EmptyStr;
  festado := -1;
  fretorno := drsNenhum;
  ftransacao_autorizacao := EmptyStr;
  ftransacao_comprovante_1via := EmptyStr;
  ftransacao_comprovante_2via := EmptyStr;
  ftransacao_comprovante_resumido := EmptyStr;
  ftransacao_identificacao := EmptyStr;
  ftransacao_nsu_rede := EmptyStr;
  ftransacao_operadora := EmptyStr;
  ftransacao_payment_id := EmptyStr;
  ftransacao_rede_cnpj := EmptyStr;
  ftransacao_resposta := EmptyStr;
  ftransacao_subadquirente := EmptyStr;
  ftransacao_taxa := EmptyStr;
  ftransacao_valor_saque := -1;
  ftransacao_valor_taxa_embarque := EmptyStr;
  ftransacao_valor_taxa_servico := EmptyStr;
end;

{ TACBrTEFDestaxaTransacaoRequisicao }

procedure TACBrTEFDestaxaTransacaoRequisicao.Setaplicacao_tela(aValue: String);
begin
  if (faplicacao_tela = aValue) then
    Exit;
  faplicacao_tela := Copy(aValue, 1, 32);  // Deve conter no máximo 32 posições
end;

procedure TACBrTEFDestaxaTransacaoRequisicao.PreencherCampos(const aStrList: TStringList);
begin
  inherited PreencherCampos(aStrList);

  PreencherCampo(aStrList, 'aplicacao_tela', faplicacao_tela);
  PreencherCampo(aStrList, 'automacao_coleta_informacao', fautomacao_coleta_informacao);
  PreencherCampo(aStrList, 'computador_endereco', fcomputador_endereco);
  PreencherCampo(aStrList, 'computador_nome', fcomputador_nome);
  PreencherCampo(aStrList, 'estabelecimento', festabelecimento);
  PreencherCampo(aStrList, 'loja', floja);
  PreencherCampo(aStrList, 'terminal', fterminal);
  PreencherCampo(aStrList, 'transacao_cartao_nome', ftransacao_cartao_nome);
  PreencherCampo(aStrList, 'transacao_cartao_numero', ftransacao_cartao_numero);
  PreencherCampo(aStrList, 'transacao_cartao_validade', ftransacao_cartao_validade);
  PreencherCampo(aStrList, 'transacao_cnpj_cpf', ftransacao_cnpj_cpf);
  PreencherCampo(aStrList, 'transacao_comprovante_email', ftransacao_comprovante_email);
  PreencherCampo(aStrList, 'transacao_comprovante_sms', ftransacao_comprovante_sms);
  PreencherCampo(aStrList, 'transacao_documento_fiscal', ftransacao_documento_fiscal);
  PreencherCampo(aStrList, 'transacao_loja_cnpj_cpf', ftransacao_loja_cnpj_cpf);
  PreencherCampo(aStrList, 'transacao_nome', ftransacao_nome);
  PreencherCampo(aStrList, 'transacao_processar', ftransacao_processar);
  PreencherCampo(aStrList, 'transacao_subadquirente', ftransacao_subadquirente);
  PreencherCampo(aStrList, 'transacao_cartao_codigo_seguranca', ftransacao_cartao_codigo_seguranca);
  PreencherCampo(aStrList, 'transacao_valor_maximo', ftransacao_valor_maximo);
  PreencherCampo(aStrList, 'transacao_valor_minimo', ftransacao_valor_minimo);
  PreencherCampo(aStrList, 'retorno', DestaxaRetornoRequisicaoToInteger(fretorno));
end;

procedure TACBrTEFDestaxaTransacaoRequisicao.Clear;
begin
  inherited Clear;
  faplicacao_tela := EmptyStr;
  fcomputador_endereco := EmptyStr;
  fcomputador_nome := EmptyStr;
  festabelecimento := EmptyStr;
  floja := EmptyStr;
  fterminal := EmptyStr;
  ftransacao_cartao_codigo_seguranca := -1;
  ftransacao_cartao_nome := EmptyStr;
  ftransacao_cartao_numero := EmptyStr;
  ftransacao_cartao_validade := EmptyStr;
  ftransacao_cnpj_cpf := EmptyStr;
  ftransacao_comprovante_email := EmptyStr;
  ftransacao_comprovante_sms := EmptyStr;
  ftransacao_documento_fiscal := EmptyStr;
  ftransacao_loja_cnpj_cpf := EmptyStr;
  ftransacao_nome := EmptyStr;
  ftransacao_processar := EmptyStr;
  ftransacao_subadquirente := EmptyStr;
  ftransacao_valor_minimo := -1;
  ftransacao_valor_maximo := -1;
  ftransacao_vendedor := EmptyStr;
  fautomacao_coleta_informacao := EmptyStr;
  fretorno := drqNenhum;
end;

{ TACBrTEFDestaxaSocket }

function TACBrTEFDestaxaSocket.GetRequisicao: TACBrTEFDestaxaTransacaoRequisicao;
begin
  if (not Assigned(fRequisicao)) then
    fRequisicao := TACBrTEFDestaxaTransacaoRequisicao.Create(fDestaxaClient);
  Result := fRequisicao;
end;

function TACBrTEFDestaxaSocket.GetResposta: TACBrTEFDestaxaTransacaoResposta;
begin
  if (not Assigned(fResposta)) then
    fResposta := TACBrTEFDestaxaTransacaoResposta.Create(fDestaxaClient);
  Result := fResposta;
end;

procedure TACBrTEFDestaxaSocket.Transmitir;
var
  TX: AnsiString;
  Erro: Integer;
begin
  TX := Requisicao.ToString +
          sLineBreak + '		' +
          sLineBreak + '			' +
          sLineBreak + '		' +
          sLineBreak + '	';
  fDestaxaClient.GravarLog('TACBrTEFDestaxaSocket.Transmitir: ' + TX);
  SendString(TX);
  Erro := LastError;
  fDestaxaClient.GravarLog(sLineBreak +
    '  TRANSMITIDO' + sLineBreak +
    '  - LastError: ' + IntToStr(Erro) + GetErrorDesc(Erro));
  if NaoEstaZerado(Erro) then
    raise EACBrTEFDestaxaErro.Create(
      ACBrStr('Erro ao Transmitir Comando' + sLineBreak +
      'Endereço: ' + fDestaxaClient.EnderecoIP + sLineBreak +
      'Porta: ' + fDestaxaClient.Porta + sLineBreak +
      'Erro: ' + IntToStr(Erro) + '-' + GetErrorDesc(Erro)));
end;

procedure TACBrTEFDestaxaSocket.TratarErro;
begin
  fDestaxaClient.GravarLog('TACBrTEFDestaxaSocket.TratarErro: ' + IntToStr(LastError) + ' - ' + GetErrorDesc(LastError));

  if (LastError = 10060) then   // TimeOut
  begin
    // ToDo: Enviar Evento para usuário cancelar
  end
  else
    raise EACBrTEFDestaxaErro.Create(
      ACBrStr('Erro ao Receber resposta do V&SPague' + sLineBreak +
        'Endereço: ' + fDestaxaClient.EnderecoIP + sLineBreak +
        'Porta: ' + fDestaxaClient.Porta + sLineBreak +
        'Erro: ' + IntToStr(LastError) + '-' + GetErrorDesc(LastError)));
end;

constructor TACBrTEFDestaxaSocket.Create(aOwner: TACBrTEFDestaxaClient);
begin
  fDestaxaClient := aOwner;
  fOnGravarLog := Nil;
end;

destructor TACBrTEFDestaxaSocket.Destroy;
begin
  if Assigned(fResposta) then
    fResposta.Free;
  if Assigned(fRequisicao) then
    fRequisicao.Free;
  inherited Destroy;
end;

function TACBrTEFDestaxaSocket.Conectar: Integer;
begin
  CloseSocket;
  Connect(fDestaxaClient.EnderecoIP, fDestaxaClient.Porta);
  Result := LastError;
  fDestaxaClient.GravarLog('TACBrTEFDestaxaSocket.Conectar - Result: ' + IntToStr(Result));
end;

procedure TACBrTEFDestaxaSocket.Reconectar;
begin
  fDestaxaClient.GravarLog('TACBrTEFDestaxaSocket - Reconectando...');
  Desconectar;
  Sleep(1000);
  Conectar;
end;

procedure TACBrTEFDestaxaSocket.EnviarComando;
var
  RX: AnsiString;
begin
  Resposta.Clear;

  try
    Transmitir;
  except
    // 10054-Connection reset by peer; 10057-Socket is not connected
    if (LastError <> 10054) and (LastError <> 10057) then
      raise;

    Reconectar;
    Transmitir;
  end;

  while NaoEstaZerado(LastError) do
  begin
    fDestaxaClient.GravarLog('TACBrTEFDestaxaSocket - Aguardando Resposta...');
    RX := RecvTerminated(fDestaxaClient.TimeOut, fDestaxaClient.Terminador);

    if EstaZerado(LastError) then
    begin
      fDestaxaClient.GravarLog('TACBrTEFDestaxaSocket.Resposta - RX: ' + RX);
      Resposta.AsString := RX;
    end
    else
      TratarErro;
  end;
end;

function TACBrTEFDestaxaSocket.Desconectar: Integer;
begin
  Result := -1;

  CloseSocket;
  Result := LastError;

  fDestaxaClient.GravarLog('TACBrTEFDestaxaSocket.Desconectar - Result: ' + IntToStr(Result));
end;

{ TACBrTEFDestaxaClient }

function TACBrTEFDestaxaClient.Socket: TACBrTEFDestaxaSocket;
begin
  if (not Assigned(fSocket)) then
    fSocket := TACBrTEFDestaxaSocket.Create(Self);
  Result := fSocket;
end;

function TACBrTEFDestaxaClient.GetRequisicao: TACBrTEFDestaxaTransacaoRequisicao;
begin
  Result := Socket.Requisicao;
end;

function TACBrTEFDestaxaClient.GetResposta: TACBrTEFDestaxaTransacaoResposta;
begin
  Result := Socket.Resposta;
end;

constructor TACBrTEFDestaxaClient.Create;
begin
  Clear;
  fOnGravarLog := Nil;
  fTimeOut := 5000;
  fTerminador := CACBRTEFDESTAXA_TERMINADOR;
end;

destructor TACBrTEFDestaxaClient.Destroy;
begin
  if Assigned(fSocket) then
    fSocket.Free;

  inherited Destroy;
end;

procedure TACBrTEFDestaxaClient.Clear;
begin
  fAplicacao := EmptyStr;
  fAplicacaoTela := EmptyStr;
  fAplicacaoVersao := EmptyStr;
  fEstabelecimento := EmptyStr;
  fEmTransacao := False;
  fLoja := EmptyStr;
  fTerminal := EmptyStr;
end;

procedure TACBrTEFDestaxaClient.GravarLog(const aString: AnsiString; Traduz: Boolean);
var
  wTratado: Boolean;
  wLog: AnsiString;
begin
  if (not Assigned(fOnGravarLog)) then
    Exit;

  if Traduz then
    wLog := TranslateUnprintable(aString)
  else
    wLog := aString;

  wTratado := False;
  fOnGravarLog(wLog, wTratado);
end;

procedure TACBrTEFDestaxaClient.Executar(aTransacao: String);
begin
  if EstaVazio(aTransacao) then
    Exit;

  Iniciar;
  try
    Requisicao.Clear;
    Requisicao.servico := dxsExecutar;
    Requisicao.transacao := aTransacao;
    Socket.EnviarComando;
  finally
    Finalizar;
  end;
end;

function TACBrTEFDestaxaClient.Iniciar: Boolean;
begin
  Result := False;
  if fEmTransacao then
    Exit;

  repeat 
    Requisicao.Clear;
    Requisicao.servico := dxsIniciar;
    Requisicao.loja := fLoja;
    Requisicao.terminal := fTerminal;
    Requisicao.aplicacao := fAplicacao;
    Requisicao.versao := fAplicacaoVersao;
    Requisicao.aplicacao_tela := FAplicacaoTela;
    Requisicao.estabelecimento := fEstabelecimento;
    //Requisicao.computador_nome := fComputadorNome;
    //Requisicao.computador_endereco := fComputadorEndereco;

    Socket.EnviarComando;

    if (Resposta.Sequencial < Requisicao.Sequencial) then
      Requisicao.Sequencial := Resposta.Sequencial;

  until (Resposta.retorno = drsSucessoSemConfirmacao) and (Resposta.Servico = Requisicao.Servico);
  fEmTransacao := True;
end;

function TACBrTEFDestaxaClient.Finalizar: Boolean;
begin
  Result := False;
  if not fEmTransacao then
    Exit;

  Requisicao.servico := dxsFinalizar;

  Socket.EnviarComando;

  if Resposta.Sequencial < Requisicao.Sequencial then
    Requisicao.Sequencial := Resposta.Sequencial;
  fEmTransacao := False;
end;

function TACBrTEFDestaxaClient.Consultar: Boolean;
begin
  Iniciar;
  try
    Requisicao.Clear;
    Requisicao.servico := dxsConsultar;
    Requisicao.retorno := drqExecutarServico;
    Socket.EnviarComando;
    Result := (Resposta.servico = dxsConsultar) and (Resposta.retorno = drsSucessoSemConfirmacao);
  finally
    Finalizar;
  end;
end;

function TACBrTEFDestaxaClient.Mostrar(aMensagem: TACBrTEFDestaxaMensagem): Boolean;
begin
  if (aMensagem = dmsNenhum) then
    Exit;

  Iniciar;
  try
    Requisicao.Clear;
    Requisicao.servico := dxsMostrar;
    Requisicao.mensagem := DestaxaMensagemToString(aMensagem);
    Socket.EnviarComando;
    Result := (Resposta.servico = dxsMostrar) and (Resposta.retorno = drsSucessoSemConfirmacao);
  finally
    Finalizar;
  end;
end;

function TACBrTEFDestaxaClient.Coletar(const aSolicitacao: TACBrTEFDestaxaTipoSolicitacao;
  const aTamanhoMin: Integer; const aTamanhoMax: Integer; const aTempoEspera: Integer): AnsiString;
var
  msg: String;
  tipo: Integer;
begin
  Result := EmptyStr;
  tipo := Ord(aSolicitacao);
  if EstaZerado(tipo) then
    Exit;

  Iniciar;
  try
    Requisicao.Clear;
    Requisicao.servico := dxsColetar;

    msg := IntToStr(tipo);
    if NaoEstaZerado(aTamanhoMin) then
      msg := msg + ';' + IntToStr(aTamanhoMin);
    if NaoEstaZerado(aTamanhoMax) then
      msg := msg + ';' + IntToStr(aTamanhoMax);
    if NaoEstaZerado(aTempoEspera) then
      msg := msg + ';' + IntToStr(aTempoEspera);
    Requisicao.mensagem := msg;

    Socket.EnviarComando;
    if (Resposta.retorno = drsSucessoSemConfirmacao) then
      Result := Resposta.transacao_informacao;
  finally
    Finalizar;
  end;
end;

{ TACBrTEFDestaxaTransacaoClass }

function TACBrTEFDestaxaTransacaoClass.GetAsString: AnsiString;
var
  Campos: TStringList;
begin
  Campos := TStringList.Create;
  try
    PreencherCampos(Campos);
    Result := Campos.Text;
  finally
    Campos.Free;
  end;
end;

procedure TACBrTEFDestaxaTransacaoClass.SetAsString(aValue: AnsiString);
var
  Campos: TStringList;
begin
  if EstaVazio(aValue) then
    Exit;

  Campos := TStringList.Create;
  try
    Campos.Text := aValue;
    CarregarCampos(Campos);
  finally
    Campos.Free;
  end;
end;

procedure TACBrTEFDestaxaTransacaoClass.PreencherCampo(const aStrList: TStringList; const aCampo: String; const aConteudo: Double);
var
  s: String;
begin
  if not Assigned(aStrList) or (aConteudo <= 0) then
    Exit;

  s := FormatFloat(',0.00', aConteudo);
  s := StringReplace(s, '.', ',',[rfReplaceAll]);
  aStrList.Values[aCampo] := '"' + s + '"';
end;

procedure TACBrTEFDestaxaTransacaoClass.PreencherCampo(const aStrList: TStringList; const aCampo: String; const aConteudo: Integer);
begin
  if not Assigned(aStrList) or (aConteudo <= 0) then
    Exit;

  aStrList.Values[aCampo] := '"' + IntToStr(aConteudo) + '"';
end;

procedure TACBrTEFDestaxaTransacaoClass.PreencherCampo(const aStrList: TStringList; const aCampo, aConteudo: AnsiString; PreencherVazio: Boolean);
begin
  if not Assigned(aStrList) or (EstaVazio(aConteudo) and (not PreencherVazio)) then
    Exit;

  aStrList.Values[aCampo] := '"' + aConteudo + '"';
end;

procedure TACBrTEFDestaxaTransacaoClass.PreencherCampos(const aStrList: TStringList);
begin
  if not Assigned(aStrList) then
    Exit;

  PreencherCampo(aStrList, 'aplicacao', faplicacao);
  PreencherCampo(aStrList, 'mensagem', fmensagem);
  PreencherCampo(aStrList, 'sequencial', fsequencial);
  PreencherCampo(aStrList, 'transacao', ftransacao);
  PreencherCampo(aStrList, 'transacao_banco', ftransacao_banco);
  PreencherCampo(aStrList, 'transacao_binario', ftransacao_binario);
  PreencherCampo(aStrList, 'transacao_cheque_cmc7', ftransacao_cheque_cmc7);
  PreencherCampo(aStrList, 'transacao_cheque_vencimento', ftransacao_cheque_vencimento);
  PreencherCampo(aStrList, 'transacao_codigo_barras', ftransacao_codigo_barras);
  PreencherCampo(aStrList, 'transacao_concessionaria', ftransacao_concessionaria);
  PreencherCampo(aStrList, 'transacao_data', ftransacao_data);
  PreencherCampo(aStrList, 'transacao_financiado', ftransacao_financiado);
  PreencherCampo(aStrList, 'transacao_informacao', ftransacao_informacao);
  PreencherCampo(aStrList, 'transacao_linha_digitavel', ftransacao_linha_digitavel);
  PreencherCampo(aStrList, 'transacao_nsu', ftransacao_nsu);
  PreencherCampo(aStrList, 'transacao_opcao', ftransacao_opcao);
  PreencherCampo(aStrList, 'transacao_parcela', ftransacao_parcela);
  PreencherCampo(aStrList, 'transacao_parcela_entrada', ftransacao_parcela_entrada);
  PreencherCampo(aStrList, 'transacao_parcela_valor', ftransacao_parcela_valor);
  PreencherCampo(aStrList, 'transacao_parcela_vencimento', ftransacao_parcela_vencimento);
  PreencherCampo(aStrList, 'transacao_produto', ftransacao_produto);
  PreencherCampo(aStrList, 'transacao_rede', ftransacao_rede);
  PreencherCampo(aStrList, 'transacao_telefone_ddd', ftransacao_telefone_ddd);
  PreencherCampo(aStrList, 'transacao_telefone_numero', ftransacao_telefone_numero);
  PreencherCampo(aStrList, 'transacao_timeout', ftransacao_timeout);
  PreencherCampo(aStrList, 'transacao_valor', ftransacao_valor);
  PreencherCampo(aStrList, 'transacao_valor_ajuste', ftransacao_valor_ajuste);
  PreencherCampo(aStrList, 'transacao_vencimento', ftransacao_vencimento);
  PreencherCampo(aStrList, 'versao', fversao);
  PreencherCampo(aStrList, 'automacao_coleta_mensagem', fautomacao_coleta_mensagem);
  PreencherCampo(aStrList, 'automacao_coleta_sequencial', fautomacao_coleta_sequencial);
  PreencherCampo(aStrList, 'automacao_coleta_transacao_resposta', fautomacao_coleta_transacao_resposta);
  PreencherCampo(aStrList, 'automacao_coleta_timeout', fautomacao_coleta_timeout);
  PreencherCampo(aStrList, 'servico', DestaxaServicoToString(servico));
  PreencherCampo(aStrList, 'transacao_tipo_cartao', DestaxaTipoCartaoToString(transacao_tipo_cartao));
  PreencherCampo(aStrList, 'transacao_pagamento', DestaxaPagamentoToString(transacao_pagamento));
  PreencherCampo(aStrList, 'transacao_binario_tipo', DestaxaBinarioTipoToString(transacao_binario_tipo));
  PreencherCampo(aStrList, 'automacao_coleta_retorno', DestaxaColetaRetornoToInteger(automacao_coleta_retorno));
  PreencherCampo(aStrList, 'automacao_coleta_mensagem_tipo', DestaxaBinarioTipoToString(automacao_coleta_mensagem_tipo));
end;

procedure TACBrTEFDestaxaTransacaoClass.CarregarCampos(const aStrList: TStringList);
begin
  if not Assigned(aStrList) then
    Exit;

  faplicacao := aStrList.Values['aplicacao'];
  fautomacao_coleta_mensagem := aStrList.Values['automacao_coleta_mensagem'];
  fautomacao_coleta_sequencial := StrToIntDef(aStrList.Values['automacao_coleta_sequencial'], -1);
  fautomacao_coleta_timeout := StrToIntDef(aStrList.Values['automacao_coleta_timeout'], -1);
  fautomacao_coleta_transacao_resposta := aStrList.Values['automacao_coleta_transacao_resposta'];
  fmensagem := aStrList.Values['mensagem'];
  fsequencial := StrToIntDef(aStrList.Values['sequencial'], -1);
  fColetaRetornoSequencial := StrToIntDef(aStrList.Values['ColetaRetornoSequencial'], -1);
  ftransacao := aStrList.Values['transacao'];
  ftransacao_banco := StrToIntDef(aStrList.Values['transacao_banco'], -1);
  ftransacao_binario := AnsiString(aStrList.Values['transacao_binario']);
  ftransacao_cheque_cmc7 := aStrList.Values['transacao_cheque_cmc7'];
  ftransacao_cheque_vencimento := aStrList.Values['transacao_cheque_vencimento'];
  ftransacao_codigo_barras := aStrList.Values['transacao_codigo_barras'];
  ftransacao_concessionaria := aStrList.Values['transacao_concessionaria'];
  ftransacao_data := StrToDateTimeDef(aStrList.Values['transacao_data'], 0);
  ftransacao_financiado := aStrList.Values['transacao_financiado'];
  ftransacao_informacao := aStrList.Values['transacao_informacao'];
  ftransacao_linha_digitavel := aStrList.Values['transacao_linha_digitavel'];
  ftransacao_nsu := aStrList.Values['transacao_nsu'];
  ftransacao_opcao := aStrList.Values['transacao_opcao'];
  ftransacao_parcela := StrToIntDef(aStrList.Values['transacao_parcela'], -1);
  ftransacao_parcela_entrada := StrToFloatDef(aStrList.Values['transacao_parcela_entrada'], 0);
  ftransacao_parcela_valor := StrToFloatDef(aStrList.Values['transacao_parcela_valor'], 0);
  ftransacao_parcela_vencimento := StrToDateTimeDef(aStrList.Values['transacao_parcela_vencimento'], 0);
  ftransacao_produto := aStrList.Values['transacao_produto'];
  ftransacao_rede := aStrList.Values['transacao_rede'];
  ftransacao_telefone_ddd := aStrList.Values['transacao_telefone_ddd'];
  ftransacao_telefone_numero := aStrList.Values['transacao_telefone_numero'];
  ftransacao_timeout := aStrList.Values['transacao_timeout'];
  ftransacao_valor := StrToFloatDef(aStrList.Values['transacao_valor'], 0);
  ftransacao_valor_ajuste := StrToFloatDef(aStrList.Values['transacao_valor_ajuste'], 0);
  ftransacao_vencimento := StrToDateTimeDef(aStrList.Values['transacao_vencimento'], 0);
  fversao := aStrList.Values['versao'];
  fservico := StringToDestaxaServico(aStrList.Values['servico']);
  ftransacao_tipo_cartao := StringToDestaxaTipoCartao(aStrList.Values['transacao_tipo_cartao']);
  ftransacao_pagamento := StringToDestaxaPagamento(aStrList.Values['transacao_pagamento']);
  ftransacao_binario_tipo := StringToDestaxaBinarioTipo(aStrList.Values['transacao_binario_tipo']);
  fautomacao_coleta_retorno := IntegerToDestaxaColetaRetorno(StrToIntDef(aStrList.Values['automacao_coleta_retorno'], -1));
  fautomacao_coleta_mensagem_tipo := StringToDestaxaBinarioTipo(aStrList.Values['automacao_coleta_mensagem_tipo']);
end;

constructor TACBrTEFDestaxaTransacaoClass.Create(aOwner: TACBrTEFDestaxaClient);
begin
  fOwner := aOwner;
  fsequencial := -1;
  Clear;
end;

procedure TACBrTEFDestaxaTransacaoClass.Clear;
begin
  faplicacao := EmptyStr;
  fmensagem := EmptyStr;
  ftransacao := EmptyStr;
  ftransacao_rede := EmptyStr;
  ftransacao_cheque_cmc7 := EmptyStr;
  ftransacao_cheque_vencimento := EmptyStr;
  ftransacao_codigo_barras := EmptyStr;
  ftransacao_concessionaria := EmptyStr;
  ftransacao_financiado := EmptyStr;
  ftransacao_informacao := EmptyStr;
  ftransacao_linha_digitavel := EmptyStr;
  ftransacao_nsu := EmptyStr;
  ftransacao_opcao := EmptyStr;
  ftransacao_produto := EmptyStr;
  ftransacao_telefone_ddd := EmptyStr;
  ftransacao_telefone_numero := EmptyStr;
  ftransacao_timeout := EmptyStr;
  fversao := EmptyStr;
  fautomacao_coleta_mensagem := EmptyStr;
  fautomacao_coleta_transacao_resposta := EmptyStr;
  ftransacao_banco := -1;
  ftransacao_parcela := -1;
  fautomacao_coleta_sequencial := -1;
  fautomacao_coleta_timeout := -1;
  ftransacao_parcela_entrada := -1;
  ftransacao_parcela_valor := -1;
  ftransacao_valor := -1;
  ftransacao_valor_ajuste := -1;
  ftransacao_data := 0;
  ftransacao_parcela_vencimento := 0;
  ftransacao_vencimento := 0;
  fservico := dxsNenhum;
  ftransacao_tipo_cartao := dtcNenhum;
  fautomacao_coleta_mensagem_tipo := dbtNenhum;
  ftransacao_pagamento := dpgNenhum;
  ftransacao_binario_tipo := dbtNenhum;
end;

end.

