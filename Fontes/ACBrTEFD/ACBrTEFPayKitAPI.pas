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

unit ACBrTEFPayKitAPI;

interface

uses
  Classes, SysUtils;

const
  {$IFDEF MSWINDOWS}
   CPayKitLib = 'DPOSDRV.dll';
  {$ELSE}
   CPayKitLib = 'libDPOSDRV.so';
  {$ENDIF}

  CPayKitDirBin = 'Bin';
  CPayKitConf = 'dposlocal.ini';
  CSecMovimentoACBr = 'Movimento_ACBr';

  CPayKitURLCertificado = 'https://tef.linxsaas.com.br/certificados/Gerenciador_Certificado.cgi';

  RET_OK = 0;

resourcestring
  sErrArqPayKitNaoEncontrado = 'Arquivo do PayKit: "%s", não encontrado em: %s';
  sErrLibJaInicializada = 'Biblioteca DPOSDRV já foi inicializada';
  sErrLibNaoInicializada = 'Biblioteca DPOSDRV ainda NÃO foi carregada';
  sErrDirPayKitInvalido = 'Subdiretório "%s" do PayKit, não encontrado na Pasta: %s';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';

type
  EACBrTEFPayKitAPI = class(Exception);

  TACBrTEFPayKitTipoOperacao = ( opAV,   // À Vista
                                 opFL,   // Financiamento Lojista
                                 opFA ); // Financiamento Administradora

  TACBrTEFPayKitGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TACBrTEFPayKitTipoMensagem = (msgInfo, msgAlerta, msgErro, msgAdicional, msgTerminal, msgPreview);

  TACBrTEFPayKitQuandoExibirMensagem = procedure(
    const Mensagem: String;
    TipoMensagem: TACBrTEFPayKitTipoMensagem;
    MilissegundosExibicao: Integer  // 0 - Para com OK;
    ) of object;                    // Positivo - Aguarda N milissegundos, e apaga a msg;
                                    // Negativo - Apenas exibe a Msg (não aguarda e não apaga msg)

  TACBrTEFPayKitQuandoPerguntarMenu = procedure(
    const Titulo: String;
    Opcoes: TStringList;
    var ItemSelecionado: Integer) of object;  // Retorna o Item Selecionado, iniciando com 0
                                              // -2 - Volta no Fluxo
                                              // -1 - Cancela o Fluxo

  TACBrTEFPayKitTiposEntrada = ( teCartao,
                                 teBarrasDigitado,
                                 teBarrasLido,
                                 teValidade,
                                 teData,
                                 teCodSeguranca,
                                 teValor,
                                 teNumero,
                                 teString,
                                 teValorEspecial);

  TACBrTEFPayKitDefinicaoCampo = record
    TituloPergunta: String;
    MascaraDeCaptura: String;
    TipoDeEntrada: TACBrTEFPayKitTiposEntrada;
    TamanhoMinimo: Integer;
    TamanhoMaximo: Integer;
    ValorMinimo : Double;
    ValorMaximo : Double;
    ValorInicial: String;
  end;

  TACBrTEFAPIQuandoPerguntarCampo = procedure(
    DefinicaoCampo: TACBrTEFPayKitDefinicaoCampo;
    var Resposta: String;
    var Acao: Integer) of object ;     // 0 - O operador digitou o valor, -1 - A operação foi cancelada

  TACBrTEFPayKitQuandoExibirQRCode = procedure(const DadosQRCode: String) of object ;

  TACBrTEFPayKitEstadoOperacao = ( pkeOperacaoCancelada );

  TACBrTEFPayKitTransacaoEmAndamento = procedure(
    EstadoOperacao: TACBrTEFPayKitEstadoOperacao; out Cancelar: Boolean) of object;

  TACBrTEFPayKitCallBackDisplayTerminal = procedure(pMensagem: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackDisplayErro = procedure(pMensagem: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackMensagem = procedure(pMensagem: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackBeep = procedure;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackSolicitaConfirmacao = function(pMensagem: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraCartao = function(pLabel, pCartao: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraDataValidade = function(pLabel, pDataValidade: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraData = function(pLabel, pDataValidade: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraCodigoSeguranca = function(pLabel, pEntraCodigoSeguranca: PAnsiChar; iTamanhoMax: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackSelecionaOpcao = function(pLabel, pOpcoes: PAnsiChar; iOpcaoSelecionada: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraValor = function(pLabel, pValor, pValorMinimo, pValorMaximo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraNumero = function(pLabel, pNumero, pNumeroMinimo, pNumeroMaximo: PAnsiChar; iMinimoDigitos, iMaximoDigitos, iDigitosExatos: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackOperacaoCancelada = function: LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackSetaOperacaoCancelada = function(iCancelada: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackProcessaMensagens = procedure;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraString = function(pLabel, pString, pTamanhoMaximo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackConsultaAVS = function(cEndereco, cNumero, cApto, cBloco, cCEP, cBairro, cCPF: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackMensagemAdicional = function(pMensagemAdicional: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackImagemAdicional = function(iIndiceImagem: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraCodigoBarras = function(pLabel, pCampo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraCodigoBarrasLido = function(pLabel, pCampo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackMensagemAlerta = procedure(pMensagemAlerta: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackPreviewComprovante = procedure(cComprovante: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackSelecionaPlanos = function(iCodigoRede, iCodigoTransacao, iTipoFinanciamento, iMaximoParcelas: Longint; pValorMinimoParcela: PAnsiChar; iMaxDiasPreDatado: LongInt; pNumeroParcelas, pValorTransacao, pValorParcela, pValorEntrada, pData: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackSelecionaPlanosEx = function(pSolicitacao, pRetorno: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackEntraValorEspecial = function(pLabel, pValor, pParametros: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  TACBrTEFPayKitCallBackComandos = function(pParametrosEntrada, pDadosRetorno: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

  { TACBrTEFPayKitAPI }

  TACBrTEFPayKitAPI = Class
  private
    fCarregada: Boolean;
    fCNPJEstabelecimento: String;
    fConfiguracaoIpPortaSsl: String;
    fOnExibeMensagem: TACBrTEFPayKitQuandoExibirMensagem;
    fOnTransacaoEmAndamento: TACBrTEFPayKitTransacaoEmAndamento;
    fQuandoExibirQRCode: TACBrTEFPayKitQuandoExibirQRCode;
    fQuandoPerguntarCampo: TACBrTEFAPIQuandoPerguntarCampo;
    fQuandoPerguntarMenu: TACBrTEFPayKitQuandoPerguntarMenu;
    fPathPayKit: String;
    fEmTransacao: Boolean;
    fInicializada: Boolean;
    fModoDesfazimento: Byte;
    fNomeAutomacao: String;
    fNumeroEmpresa: Integer;
    fNumeroLoja: Integer;
    fNumeroPDV: Integer;
    fOnGravarLog: TACBrTEFPayKitGravarLog;
    fPortaPinPad: String;
    fURLCertificado: String;
    fVersaoAutomacao: String;

    xTransacaoCheque: function(pValorTransacao, pNumeroCupomVenda, pNumeroControle,
      pQuantidadeCheques, pPeriodicidadeCheques, pDataPrimeiroCheque,
      pCarenciaPrimeiroCheque: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCartaoCredito: function(pValorTransacao, pNumeroCupomVenda,
      pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //xConfirmacaoCartaoCredito: function(pNumeroControle: PAnsiChar): LongInt;
    //  {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCartaoDebito: function(pValorTransacao, pNumeroCupomVenda,
      pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //xConfirmacaoCartaoDebito: function(pNumeroControle: PAnsiChar): LongInt;
    //  {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCartaoVoucher: function (pValorTransacao, pNumeroCupomVenda,
      pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    //xConfirmacaoCartaoVoucher: function(pNumeroControle: PAnsiChar): LongInt;
    //  {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoCancelamentoPagamento: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoPreAutorizacaoCartaoCredito: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoConsultaParcelas: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoResumoVendas: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xTransacaoReimpressaoCupom: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfirmaCartao: function(pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xFinalizaTransacao: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xObtemLogUltimaTransacao: procedure(oLogUltimaTransacao: PAnsiChar);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xVersaoDPOS: procedure(pVersao: PAnsiChar);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xInicializaDPOS: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xFinalizaDPOS: function: LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xIdentificacaoAutomacaoComercial: function(pNomeAutomacao, pVersaoAutomacao,
      pReservado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfiguraModoDesfazimento: function(iModoDesfazimento: LongInt): LongInt
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xConfiguraCNPJEstabelecimento: function(pCNPJEstabelecimento: PAnsiChar): LongInt
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfiguraEmpresaLojaPDV: function(pNumeroEmpresa, pNumeroLoja, pNumeroPDV: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xConfiguraComunicacaoDTEF: function(pConfiguracaoIpPortaSsl: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xBuscaCertificado: function(pURL, pPathCertificado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xTransacaoEspecial: function(iCodigoTransacao: LongInt; pDados: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xLeIdentificacaoPinPad: function(pDados: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    // Definicao das funcoes de transacao completa
    xTransacaoCartaoCreditoCompleta: function( pValorTransacao, pNumeroCupomVenda,
      pNumeroControle, pTipoOperacao, pNumeroParcelas, pValorParcela,
      pValorTaxaServico, pPermiteAlteracao, pReservado: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xTransacaoCartaoDebitoCompleta: function ( pValorTransacao, pNumeroCupomVenda,
      pNumeroControle, pTipoOperacao, pNumeroParcelas, pSequenciaParcela, pDataDebito,
      pValorParcela, pValorTaxaServico, pPermiteAlteracao, pReservado: PAnsiChar): LongInt;
     {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xTransacaoCartaoVoucherCompleta: function (pValorTransacao, pNumeroCupomVenda,
      pNumeroControle, pReservado: PAnsiChar): LongInt;
     {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xTransacaoManualPOSCompleta: function(pValorTransacao, pCodigoEstabelecimento,
      pData, pHora, pNumeroAutorizadora, pNumeroCartao, pTipoOperacao,
      pNumeroParcelas, pDataPreDatado, pNumeroControle: PAnsiChar): LongInt;
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xRegPDVDisplayTerminal: procedure(pCallBackDisplayTerminal: TACBrTEFPayKitCallBackDisplayTerminal);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVDisplayErro: procedure(pCallBackDisplayErro: TACBrTEFPayKitCallBackDisplayErro);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVMensagem: procedure(pCallBackMensagem: TACBrTEFPayKitCallBackMensagem);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVBeep: procedure(pCallBackBeep: TACBrTEFPayKitCallBackBeep);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVSolicitaConfirmacao: procedure(pCallBackSolicitaConfirmacao: TACBrTEFPayKitCallBackSolicitaConfirmacao);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraCartao: procedure(pCallBackEntraCartao: TACBrTEFPayKitCallBackEntraCartao);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraDataValidade: procedure(pCallBackEntraDataValidade: TACBrTEFPayKitCallBackEntraDataValidade);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraData: procedure(pCallBackEntraData: TACBrTEFPayKitCallBackEntraData);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraCodigoSeguranca: procedure(pCallBackEntraCodigoSeguranca: TACBrTEFPayKitCallBackEntraCodigoSeguranca);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVSelecionaOpcao: procedure(pCallBackSelecionaOpcao: TACBrTEFPayKitCallBackSelecionaOpcao);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraValor: procedure(pCallBackEntraValor: TACBrTEFPayKitCallBackEntraValor);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraNumero: procedure(pCallBackEntraNumero: TACBrTEFPayKitCallBackEntraNumero);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVOperacaoCancelada: procedure(pCallBackOperacaoCancelada: TACBrTEFPayKitCallBackOperacaoCancelada);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVSetaOperacaoCancelada: procedure(pCallBackSetaOperacaoCancelada: TACBrTEFPayKitCallBackSetaOperacaoCancelada);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVProcessaMensagens: procedure(pCallBackProcessaMensagens: TACBrTEFPayKitCallBackProcessaMensagens);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraString: procedure(pCallBackEntraString: TACBrTEFPayKitCallBackEntraString);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVConsultaAVS: procedure(pCallBackConsultaAVS: TACBrTEFPayKitCallBackConsultaAVS);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVMensagemAdicional: procedure(pCallBackMensagemAdicional: TACBrTEFPayKitCallBackMensagemAdicional);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVImagemAdicional: procedure(pCallBackImagemAdicional: TACBrTEFPayKitCallBackImagemAdicional);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraCodigoBarras: procedure(pCallBackEntraCodigoBarras: TACBrTEFPayKitCallBackEntraCodigoBarras);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraCodigoBarrasLido: procedure(pCallBackEntraCodigoBarrasLido: TACBrTEFPayKitCallBackEntraCodigoBarrasLido);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVMensagemAlerta: procedure(pCallBackMensagemAlerta: TACBrTEFPayKitCallBackMensagemAlerta);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVPreviewComprovante: procedure(pCallBackPreviewComprovante: TACBrTEFPayKitCallBackPreviewComprovante);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVSelecionaPlanos: procedure(pCallBackSelecionaPlanos: TACBrTEFPayKitCallBackSelecionaPlanos);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVSelecionaPlanosEx: procedure(pCallBackSelecionaPlanosEx: TACBrTEFPayKitCallBackSelecionaPlanosEx);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVEntraValorEspecial: procedure(pCallBackEntraValorEspecial: TACBrTEFPayKitCallBackEntraValorEspecial);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xRegPDVComandos: procedure(pCallBackComandos: TACBrTEFPayKitCallBackComandos);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetConfiguracaoIpPortaSsl(AValue: String);
    procedure SetURLCertificado(AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetModoDesfazimento(AValue: Byte);
    procedure SetNomeAutomacao(AValue: String);
    procedure SetNumeroEmpresa(AValue: Integer);
    procedure SetNumeroLoja(AValue: Integer);
    procedure SetNumeroPDV(AValue: Integer);
    procedure SetVersaoAutomacao(AValue: String);
    procedure SetPathPayKit(AValue: String);

  protected
    procedure LoadLibFunctions;
    procedure UnLoadLibFunctions;
    procedure ClearMethodPointers;
    procedure RegisterCallBackFunctions;

    procedure TratarErroPayKit(AErrorCode: LongInt);

    procedure DoException(const AErrorMsg: String );

    procedure VerificarCarregada;
    procedure VerificarEAjustarConf;
    procedure CriarArquivoConfSeNaoExistir;
    function VerificarSeEstaEmTransacao: Boolean;

    procedure IdentificacaoAutomacaoComercial;
    procedure ConfiguraModoDesfazimento;
    procedure ConfiguraCNPJEstabelecimento;
    procedure ConfiguraEmpresaLojaPDV;
    procedure ConfiguraComunicacaoDTEF;
    procedure BuscaCertificado;

    procedure ExibirMensagem(const AMsg: String; TipoMensagem: TACBrTEFPayKitTipoMensagem = msgInfo;
      MilissegundosExibicao: Integer = -1);
    procedure PerguntarMenu(const Titulo: String; Opcoes: TStringList; var ItemSelecionado: Integer);
    procedure PerguntarCampo(DefinicaoCampo: TACBrTEFPayKitDefinicaoCampo;
      var Resposta: String; var Acao: Integer);
    function VerificarTransacaoEmAndamento(EstadoOperacao: TACBrTEFPayKitEstadoOperacao): Boolean;

    procedure ExibirQRCode(const DadosQRCode: String);
  public
    constructor Create;
    destructor Destroy; override;

    property PathPayKit: String read fPathPayKit write SetPathPayKit;

    property Carregada: Boolean read fCarregada;
    property Inicializada: Boolean read fInicializada write SetInicializada;
    property EmTransacao: Boolean read fEmTransacao;

    property OnGravarLog: TACBrTEFPayKitGravarLog read fOnGravarLog write fOnGravarLog;
    property OnExibeMensagem: TACBrTEFPayKitQuandoExibirMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    property OnTransacaoEmAndamento: TACBrTEFPayKitTransacaoEmAndamento read fOnTransacaoEmAndamento
      write fOnTransacaoEmAndamento;
    property QuandoPerguntarMenu: TACBrTEFPayKitQuandoPerguntarMenu read fQuandoPerguntarMenu
      write fQuandoPerguntarMenu;
    property QuandoPerguntarCampo: TACBrTEFAPIQuandoPerguntarCampo read fQuandoPerguntarCampo
      write fQuandoPerguntarCampo;
    property QuandoExibirQRCode: TACBrTEFPayKitQuandoExibirQRCode read fQuandoExibirQRCode
      write fQuandoExibirQRCode;

    procedure Inicializar;
    procedure DesInicializar;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);

    function VersaoDPOS: String;
    procedure InicializaDPOS(Forcar: Boolean = False);
    procedure FinalizaDPOS(Forcar: Boolean = False);
    function CalcPayKitPath(ASubFolder: String): String;

    procedure TransacaoEspecial(iCodigoTransacao: LongInt; var Dados: AnsiString);
    procedure ExibirMensagemPinPad(const MsgPinPad: String; Tempo: Integer);
    function LeIdentificacaoPinPad: String;

    procedure TransacaoCartaoCreditoCompleta(ValorTransacao: Double;
      const NumeroCupomVenda: Integer; var NumeroControle: String;
      TipoOperacao: TACBrTEFPayKitTipoOperacao; NumeroParcelas: Integer;
      ValorParcela, ValorTaxaServico: Double; PermiteAlteracao: Boolean;
      const Reservado: String);

    property NomeAutomacao: String read fNomeAutomacao write SetNomeAutomacao;
    property VersaoAutomacao: String read fVersaoAutomacao write SetVersaoAutomacao;
    property ModoDesfazimento: Byte read fModoDesfazimento write SetModoDesfazimento default 1;  // 0 - Automático, 1 - Explícito

    property CNPJEstabelecimento: String read fCNPJEstabelecimento write SetCNPJEstabelecimento;
    property NumeroEmpresa: Integer read fNumeroEmpresa write SetNumeroEmpresa;
    property NumeroLoja: Integer read fNumeroLoja write SetNumeroLoja;
    property NumeroPDV: Integer read fNumeroPDV write SetNumeroPDV;

    property ConfiguracaoIpPortaSsl: String read fConfiguracaoIpPortaSsl write SetConfiguracaoIpPortaSsl;
    property URLCertificado: String read fURLCertificado write SetURLCertificado;

    property PortaPinPad: String read fPortaPinPad write fPortaPinPad;
  end;

  function GetTEFPayKitAPI: TACBrTEFPayKitAPI;

  procedure CallBackDisplayTerminal(pMensagem: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  procedure CallBackDisplayErro(pMensagem: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  procedure CallBackMensagem(pMensagem: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  procedure CallBackBeep;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackSolicitaConfirmacao(pMensagem: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraCartao(pLabel, pCartao: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraDataValidade(pLabel, pDataValidade: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraData(pLabel, pData: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraCodigoSeguranca(pLabel, pEntraCodigoSeguranca: PAnsiChar; iTamanhoMax: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackSelecionaOpcao(pLabel, pOpcoes: PAnsiChar; iOpcaoSelecionada: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraValor(pLabel, pValor, pValorMinimo, pValorMaximo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraNumero(pLabel, pNumero, pNumeroMinimo, pNumeroMaximo: PAnsiChar; iMinimoDigitos, iMaximoDigitos, iDigitosExatos: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackOperacaoCancelada: LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackSetaOperacaoCancelada(iCancelada: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  procedure CallBackProcessaMensagens;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraString(pLabel, pString, pTamanhoMaximo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackConsultaAVS(cEndereco, cNumero, cApto, cBloco, cCEP, cBairro, cCPF: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackMensagemAdicional(pMensagemAdicional: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackImagemAdicional(iIndiceImagem: LongInt): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraCodigoBarras(pLabel, pCampo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraCodigoBarrasLido(pLabel, pCampo: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  procedure CallBackMensagemAlerta(pMensagemAlerta: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  procedure CallBackPreviewComprovante(cComprovante: PAnsiChar);
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackSelecionaPlanos(iCodigoRede, iCodigoTransacao, iTipoFinanciamento, iMaximoParcelas: Longint; pValorMinimoParcela: PAnsiChar; iMaxDiasPreDatado: LongInt; pNumeroParcelas, pValorTransacao, pValorParcela, pValorEntrada, pData: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackSelecionaPlanosEx(pSolicitacao, pRetorno: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackEntraValorEspecial(pLabel, pValor, pParametros: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
  function CallBackComandos(pParametrosEntrada, pDadosRetorno: PAnsiChar): LongInt;
    {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

var
  vTEFPayKit: TACBrTEFPayKitAPI;

implementation

uses
  IniFiles, Math, StrUtils,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

function GetTEFPayKitAPI: TACBrTEFPayKitAPI;
begin
  if not Assigned(vTEFPayKit) then
    vTEFPayKit := TACBrTEFPayKitAPI.Create;

  Result := vTEFPayKit;
end;

procedure CallBackDisplayTerminal(pMensagem: PAnsiChar); {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s: String;
begin
  with GetTEFPayKitAPI do
  begin
    s := String(pMensagem);
    GravarLog('CallBackDisplayTerminal( '+s+' )');
    ExibirMensagem(s, msgTerminal);
  end;
end;

procedure CallBackDisplayErro(pMensagem: PAnsiChar); {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s: String;
begin
  with GetTEFPayKitAPI do
  begin
    s := String(pMensagem);
    GravarLog('CallBackDisplayErro( '+s+' )');
    ExibirMensagem(s, msgErro);
  end;
end;

procedure CallBackMensagem(pMensagem: PAnsiChar); {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s: String;
begin
  with GetTEFPayKitAPI do
  begin
    s := String(pMensagem);
    GravarLog('CallBackMensagem( '+s+' )');
    ExibirMensagem(s, msgInfo);
  end;
end;

procedure CallBackBeep; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackBeep');
    Beep;
  end;
end;

function CallBackSolicitaConfirmacao(pMensagem: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s: String;
  sl: TStringList;
  ItemSelecionado: Integer;
begin
  Result := 1;   // NAO
  with GetTEFPayKitAPI do
  begin
    s := String(pMensagem);
    GravarLog('CallBackSolicitaConfirmacao( '+s+' )');
    sl := TStringList.Create;
    try
      ItemSelecionado := 0;
      sl.Add('1-SIM');
      sl.Add('2-NAO');
      PerguntarMenu(s, sl, ItemSelecionado);
      if (ItemSelecionado = 0) then     //-2 - Volta no Fluxo, -1 - Cancela o Fluxo
        Result := 0;  // SIM
    finally
      sl.Free;
    end;
    GravarLog('  ret:'+IntToStr(Result))
  end;
end;

function CallBackEntraCartao(pLabel, pCartao: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    GravarLog('CallBackEntraCartao( '+s+' )');
    def.TituloPergunta := s;
    def.TipoDeEntrada := teCartao;
    def.TamanhoMaximo := 19;

    resp := String(pCartao);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pCartao, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', Cartao:'+String(pCartao))
  end;
end;

function CallBackEntraDataValidade(pLabel, pDataValidade: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    GravarLog('CallBackEntraDataValidade( '+s+' )');
    def.TituloPergunta := s;
    def.TipoDeEntrada := teValidade;

    resp := String(pDataValidade);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    resp := OnlyNumber(resp);
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pDataValidade, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', DataValidade:'+String(pDataValidade));
  end;
end;

function CallBackEntraData(pLabel, pData: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    GravarLog('CallBackEntraData( '+s+' )');
    def.TituloPergunta := s;
    def.TipoDeEntrada := teData;

    resp := String(pData);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    resp := OnlyNumber(resp);
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pData, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', Data:'+String(pData))
  end;
end;

function CallBackEntraCodigoSeguranca(pLabel, pEntraCodigoSeguranca: PAnsiChar; iTamanhoMax: LongInt): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    GravarLog('CallBackEntraCodigoSeguranca( '+s+', '+IntToStr(iTamanhoMax)+' )');
    def.TituloPergunta := s;
    def.TipoDeEntrada := teCodSeguranca;
    def.TamanhoMaximo := iTamanhoMax;

    resp := String(pEntraCodigoSeguranca);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pEntraCodigoSeguranca, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', Cod:'+String(pEntraCodigoSeguranca));
  end;
end;

function CallBackSelecionaOpcao(pLabel, pOpcoes: PAnsiChar; iOpcaoSelecionada: LongInt): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  titulo, opcoes, s: String;
  sl: TStringList;
  i, p1, p2: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    titulo := String(pLabel);
    opcoes := String(pOpcoes);
    GravarLog('CallBackSelecionaOpcao( '+titulo+', '+opcoes+' )');
    sl := TStringList.Create;
    try
      sl.Text := StringReplace(opcoes, '#', sLineBreak, [rfReplaceAll]);
      for i := 0 to sl.Count-1 do
      begin
        s := sl[i];
        p1 := max(Pos('"' ,s),1);
        p2 := min(PosEx('"' ,s, p1+1), Length(s));
        sl[i] := copy(s, p1+1, p2-p1-1);
      end;

      PerguntarMenu(titulo, sl, iOpcaoSelecionada);
      if (iOpcaoSelecionada < 0) then     // -2 - Volta no Fluxo, -1 - Cancela o Fluxo
        Result := -1
      else
        Inc(iOpcaoSelecionada);  // Converte de Indice 0 para indice 1
    finally
      sl.Free;
    end;
    GravarLog('  ret:'+IntToStr(Result)+', OpcaoSelecionada:'+IntToStr(iOpcaoSelecionada));
  end;
end;

function CallBackEntraValor(pLabel, pValor, pValorMinimo, pValorMaximo: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp, vmin, vmax: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    vmin := String(pValorMinimo);
    vmax := String(pValorMaximo);
    GravarLog('CallBackEntraValor( '+s+', '+vmin+', '+vmax+' )');
    def.TituloPergunta := s;
    def.ValorMinimo := StrToIntDef(vmin, 0)/100;
    def.ValorMaximo := StrToIntDef(vmax, 0)/100;
    def.TamanhoMinimo := 1;
    def.TamanhoMaximo := 12;
    def.MascaraDeCaptura := '@@@.@@@.@@@.@@@,@@';
    def.TipoDeEntrada := teValor;

    resp := String(pValor);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    resp := Format('%.12d',[StrToIntDef(OnlyNumber(resp), 0)]) ;
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pValor, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', Valor:'+String(pValor));
  end;
end;

function CallBackEntraNumero(pLabel, pNumero, pNumeroMinimo,
  pNumeroMaximo: PAnsiChar; iMinimoDigitos, iMaximoDigitos,
  iDigitosExatos: LongInt): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp, vmin, vmax: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    vmin := String(pNumeroMinimo);
    vmax := String(pNumeroMaximo);
    GravarLog( 'CallBackEntraNumero( '+s+', '+vmin+', '+vmax+', '+
               IntToStr(iMinimoDigitos)+', '+IntToStr(iMaximoDigitos)+', '+IntToStr(iDigitosExatos)+' )');
    def.TituloPergunta := s;
    def.ValorMinimo := StrToIntDef(vmin, 0);
    def.ValorMaximo := StrToIntDef(vmax, 0);
    def.TamanhoMinimo := iMinimoDigitos;
    def.TamanhoMaximo := iMaximoDigitos;
    if (iDigitosExatos <> 0) then
    begin
      def.TamanhoMinimo := iDigitosExatos;
      def.TamanhoMaximo := iDigitosExatos;
    end;

    def.TipoDeEntrada := teNumero;

    resp := String(pNumero);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pNumero, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', Valor:'+String(pNumero));
  end;
end;

function CallBackOperacaoCancelada: LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackOperacaoCancelada');
    if not VerificarTransacaoEmAndamento(pkeOperacaoCancelada) then
      Result := 1;
    GravarLog('  ret:'+IntToStr(Result));
  end;
end;

function CallBackSetaOperacaoCancelada(iCancelada: LongInt): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackSetaOperacaoCancelada( '+IntToStr(iCancelada)+' )');
    { O que fazer aqui ? }
    GravarLog('  ret:'+IntToStr(Result));
  end;
end;

procedure CallBackProcessaMensagens; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackProcessaMensagens');
  end;
end;

function CallBackEntraString(pLabel, pString, pTamanhoMaximo: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    GravarLog('CallBackEntraString( '+s+', '+String(pTamanhoMaximo)+' )');
    def.TituloPergunta := s;
    def.TamanhoMaximo := StrToIntDef(String(pTamanhoMaximo), 0);
    def.TipoDeEntrada := teString;

    resp := String(pString);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pString, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', String:'+String(pString));
  end;
end;

function CallBackConsultaAVS(cEndereco, cNumero, cApto, cBloco, cCEP, cBairro,
  cCPF: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  Result := -1;  // A operação foi cancelada;
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackConsultaAVS');
    GravarLog('  ret:'+IntToStr(Result))
  end;
end;

function CallBackMensagemAdicional(pMensagemAdicional: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s: String;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pMensagemAdicional);
    GravarLog('CallBackMensagemAdicional( '+s+' )');
    ExibirMensagem(s, msgAdicional);
    GravarLog('  ret:'+IntToStr(Result))
  end;
end;

function CallBackImagemAdicional(iIndiceImagem: LongInt): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackImagemAdicional( '+IntToStr(iIndiceImagem)+' )');
    { code here }
    GravarLog('  ret:'+IntToStr(Result))
  end;
end;

function CallBackEntraCodigoBarras(pLabel, pCampo: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    with GetTEFPayKitAPI do
    begin
      GravarLog('CallBackEntraCodigoBarras( '+s+' )');
      def.TituloPergunta := s;
      def.TipoDeEntrada := teBarrasDigitado;
      resp := '';
      acao := 0;
      PerguntarCampo(def, resp, acao);
      Result := acao;
      if (acao >= 0) then
        move(resp[1], pCampo, Length(resp));

      GravarLog('  ret:'+IntToStr(Result)+', Campo:'+String(pCampo));
    end;
  end;
end;

function CallBackEntraCodigoBarrasLido(pLabel, pCampo: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    with GetTEFPayKitAPI do
    begin
      GravarLog('CallBackEntraCodigoBarrasLido( '+s+' )');
      def.TituloPergunta := s;
      def.TipoDeEntrada := teBarrasLido;
      resp := '';
      acao := 0;
      PerguntarCampo(def, resp, acao);
      Result := acao;
      if (acao >= 0) then
        move(resp[1], pCampo, Length(resp));

      GravarLog('  ret:'+IntToStr(Result)+', Campo:'+String(pCampo));
    end;
  end;
end;

procedure CallBackMensagemAlerta(pMensagemAlerta: PAnsiChar); {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s: String;
begin
  with GetTEFPayKitAPI do
  begin
    s := String(pMensagemAlerta);
    GravarLog('CallBackMensagemAlerta( '+s+' )');
    ExibirMensagem(s, msgAlerta, 0);
  end;
end;

procedure CallBackPreviewComprovante(cComprovante: PAnsiChar); {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s: String;
begin
  with GetTEFPayKitAPI do
  begin
    s := String(cComprovante);
    GravarLog('CallBackPreviewComprovante( '+s+' )');
    ExibirMensagem(s, msgPreview, 0);
  end;
end;

function CallBackSelecionaPlanos(iCodigoRede, iCodigoTransacao,
  iTipoFinanciamento, iMaximoParcelas: Longint; pValorMinimoParcela: PAnsiChar;
  iMaxDiasPreDatado: LongInt; pNumeroParcelas, pValorTransacao, pValorParcela,
  pValorEntrada, pData: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  Result := -1;  // Os planos não serão tratados
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackSelecionaPlanos');
    GravarLog('  ret:'+IntToStr(Result))
  end;
end;

function CallBackSelecionaPlanosEx(pSolicitacao, pRetorno: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
begin
  Result := -1;  // Os planos não serão tratados
  with GetTEFPayKitAPI do
  begin
    GravarLog('CallBackSelecionaPlanosEx');
    GravarLog('  ret:'+IntToStr(Result))
  end;
end;

function CallBackEntraValorEspecial(pLabel, pValor, pParametros: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, resp, p: String;
  def: TACBrTEFPayKitDefinicaoCampo;
  acao, decimais: Integer;
begin
  Result := 0;
  with GetTEFPayKitAPI do
  begin
    s := String(pLabel);
    p := String(pParametros);
    GravarLog('CallBackEntraValorEspecial( '+s+', '+p+' )');
    def.TituloPergunta := s;
    def.ValorMinimo := StrToIntDef(copy(p,  1, 12), 0);
    def.ValorMaximo := StrToIntDef(copy(p, 13, 12), 0);
    decimais := StrToIntDef(copy(p, 25, 1), 0);
    def.TipoDeEntrada := teValorEspecial;
    def.MascaraDeCaptura := '@@@.@@@.@@@.@@@,'+StringOfChar('@', decimais);

    resp := String(pValor);
    acao := 0;
    PerguntarCampo(def, resp, acao);
    resp := Format('%.12d',[StrToIntDef(OnlyNumber(resp), 0)]) ;
    Result := acao;
    if (acao >= 0) then
      move(resp[1], pValor, Length(resp));

    GravarLog('  ret:'+IntToStr(Result)+', Valor:'+String(pValor));
  end;
end;

function CallBackComandos(pParametrosEntrada, pDadosRetorno: PAnsiChar): LongInt; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
var
  s, comando: String;
  tamanho: integer;
begin
  Result := 0;
  //0 	O comando foi executado corretamente.
  //-1 	O comando não foi implementado
  //-2 	Ocorreu um erro ao executar o comando

  // QRCODE
  // Os comandos são enviados no formato TLV, com código de comando com 3 bytes e tamanho com 6 bytes.
  // O tamanho se refere ao tamanho dos dados e não ao tamanho total.
  // Por exemplo, o comando para apresentação de um código QR Code fica assim:
  // 001000030LINXeFuiMzDmu85TcYCimGcOeMvUwM
  // Código de comando: 001
  // Tamanho: 000030
  // QRCode: LINXeFuiMzDmu85TcYCimGcOeMvUwM

  with GetTEFPayKitAPI do
  begin
    s := String(pParametrosEntrada);
    GravarLog('CallBackComandos( '+s+' )');

    comando := copy(s,1,3);
    tamanho := StrToIntDef(copy(s, 4, 6), 0);
    s := copy(s, 11, tamanho);

    if (comando = '001') then
      ExibirQRCode(s)
    else
      Result := -1;

    GravarLog('  ret:'+IntToStr(Result))
  end;
end;

{ TACBrTEFPayKitAPI }

constructor TACBrTEFPayKitAPI.Create;
begin
  inherited;
  fCarregada := False;
  fPathPayKit := '';
  fEmTransacao := False;
  fInicializada := False;

  fNomeAutomacao := '';
  fPortaPinPad := '';
  fVersaoAutomacao := '';
  fModoDesfazimento := 1;

  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnTransacaoEmAndamento := Nil;
  fQuandoPerguntarMenu := Nil;
  fQuandoExibirQRCode := Nil;
  fQuandoPerguntarCampo := Nil;
end;

destructor TACBrTEFPayKitAPI.Destroy;
begin
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnTransacaoEmAndamento := Nil;
  fQuandoPerguntarMenu := Nil;
  fQuandoExibirQRCode := Nil;
  fQuandoPerguntarCampo := Nil;

  inherited Destroy;
end;

procedure TACBrTEFPayKitAPI.Inicializar;
begin
  if fInicializada then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.Inicializar');

  if not Assigned(fOnExibeMensagem) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnExibeMensagem']));
  if not Assigned(fOnTransacaoEmAndamento) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnTransacaoEmAndamento']));
  if not Assigned(fQuandoPerguntarMenu) then
    DoException(Format(sErrEventoNaoAtribuido, ['QuandoPerguntarMenu']));
  if not Assigned(fQuandoPerguntarCampo) then
    DoException(Format(sErrEventoNaoAtribuido, ['QuandoPerguntarCampo']));

  VerificarEAjustarConf;
  LoadLibFunctions;
  RegisterCallBackFunctions;

  IdentificacaoAutomacaoComercial;
  ConfiguraModoDesfazimento;
  ConfiguraCNPJEstabelecimento;
  ConfiguraEmpresaLojaPDV;
  ConfiguraComunicacaoDTEF;
  BuscaCertificado;
  InicializaDPOS;

  fInicializada := True;
end;

procedure TACBrTEFPayKitAPI.DesInicializar;
begin
  if not fInicializada then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.DesInicializar');
  UnLoadLibFunctions;
  fInicializada := False;
end;

procedure TACBrTEFPayKitAPI.GravarLog(const AString: AnsiString; Traduz: Boolean);
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

function TACBrTEFPayKitAPI.VersaoDPOS: String;
var
  pVersao: PAnsiChar;
begin
  Result := '';
  pVersao := AllocMem(100);
  try
    GravarLog('VersaoDPOS');
    xVersaoDPOS(pVersao);
    Result := String(pVersao);
    GravarLog('  Result: '+Result);
  finally
    Freemem(pVersao);
  end;
end;

procedure TACBrTEFPayKitAPI.InicializaDPOS(Forcar: Boolean);
var
  iRet: LongInt;
  ini: TMemIniFile;
  datamov: TDateTime;
  fechado: Boolean;
  p, f: String;
begin
  GravarLog('TACBrTEFPayKitAPI.InicializaDPOS');
  p := CalcPayKitPath(CPayKitDirBin);
  f := p + CPayKitConf;
  if not FileExists(f) then
    DoException(Format(sErrArqPayKitNaoEncontrado, [CPayKitConf, p]));

  datamov := 0;
  fechado := False;
  ini := TMemIniFile.Create(f);
  try
    if not Forcar then
    begin
      datamov := ini.ReadDateTime(CSecMovimentoACBr, 'Data', 0);
      fechado := ini.ReadBool(CSecMovimentoACBr, 'Fechado', False);
    end;

    if (datamov = Date) then
    begin
      if fechado then
        GravarLog('  Movimento estava fechado')
      else
        GravarLog('  Movimento estava aberto');

      Exit;
    end;

    GravarLog('InicializaDPOS');
    iRet := xInicializaDPOS;
    GravarLog('  ret: '+IntToStr(iRet));
    TratarErroPayKit(iRet);

    ini.WriteDateTime(CSecMovimentoACBr, 'Data', Date);
    ini.WriteBool(CSecMovimentoACBr, 'Fechado', False);
  finally
    ini.Free;
  end;
end;

procedure TACBrTEFPayKitAPI.FinalizaDPOS(Forcar: Boolean);
var
  iRet: LongInt;
  ini: TMemIniFile;
  fechado: Boolean;
  datamov: TDateTime;
  p, f: String;
begin
  GravarLog('TACBrTEFPayKitAPI.InicializaDPOS');
  p := CalcPayKitPath(CPayKitDirBin);
  f := p + CPayKitConf;
  if not FileExists(f) then
    DoException(Format(sErrArqPayKitNaoEncontrado, [CPayKitConf, p]));

  fechado := False;
  datamov := 0;
  ini := TMemIniFile.Create(f);
  try
    if not Forcar then
    begin
      datamov := ini.ReadDateTime(CSecMovimentoACBr, 'Data', 0);
      fechado := ini.ReadBool(CSecMovimentoACBr, 'Fechado', False);
    end;

    if fechado and (datamov = Date) then
    begin
      GravarLog('  Movimento estava fechado');
      Exit;
    end;

    GravarLog('FinalizaDPOS');
    iRet := xFinalizaDPOS;
    GravarLog('  ret: '+IntToStr(iRet));
    TratarErroPayKit(iRet);

    ini.WriteDateTime(CSecMovimentoACBr, 'Data', Date);
    ini.WriteBool(CSecMovimentoACBr, 'Fechado', True);
  finally
    ini.Free;
  end;
end;

procedure TACBrTEFPayKitAPI.TransacaoEspecial(iCodigoTransacao: LongInt;
  var Dados: AnsiString);
var
  p: PAnsiChar;
  iRet: LongInt;
begin
  GravarLog('TransacaoEspecial( '+IntToStr(iCodigoTransacao)+', '+Dados+' )');
  //pDados := PadRight(Dados, 2048);
  fEmTransacao := True;
  try
    p := PAnsiChar(Dados);
    iRet := xTransacaoEspecial(iCodigoTransacao, p);
    Dados := Trim(Dados);
    GravarLog('  ret: '+IntToStr(iRet)+', Dados: '+Dados);
    TratarErroPayKit(iRet);
  finally
    fEmTransacao := False;
  end;
end;

procedure TACBrTEFPayKitAPI.ExibirMensagemPinPad(const MsgPinPad: String;
  Tempo: Integer);
var
  Dados: AnsiString;
begin
  Dados := PadRight(MsgPinPad, 32) + Format('%.6d',[Tempo]);
  TransacaoEspecial(106, Dados);
end;

function TACBrTEFPayKitAPI.LeIdentificacaoPinPad: String;
var
  iRet: LongInt;
  pDados: PAnsiChar;
begin
  pDados := AllocMem(110);
  fEmTransacao := True;
  try
    GravarLog('LeIdentificacaoPinPad');
    iRet := xLeIdentificacaoPinPad(pDados);
    Result := String(pDados);
    GravarLog('  ret: '+IntToStr(iRet)+', Dados: '+Result);
  finally
    Freemem(pDados);
    fEmTransacao := False;
  end;
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.TransacaoCartaoCreditoCompleta(
  ValorTransacao: Double; const NumeroCupomVenda: Integer;
  var NumeroControle: String; TipoOperacao: TACBrTEFPayKitTipoOperacao;
  NumeroParcelas: Integer; ValorParcela, ValorTaxaServico: Double;
  PermiteAlteracao: Boolean; const Reservado: String);
var
  iRet: LongInt;
  pValorTransacao, pNumeroCupomVenda, pTipoOperacao, pNumeroParcelas,
    pValorParcela, pValorTaxaServico, pPermiteAlteracao, pReservado: AnsiString;
  pNumeroControle: PAnsiChar;
begin
  pValorTransacao := Format('%.12d',[RoundTo(ValorTransacao*100, 0)]);
  pNumeroCupomVenda := Format('%.6d',[NumeroCupomVenda]);
  pTipoOperacao := IfThen(TipoOperacao=opAV, 'AV', IfThen(TipoOperacao=opFA, 'FA', 'FL'));
  pNumeroParcelas := Format('%.2d',[NumeroParcelas]);
  pValorParcela := Format('%.12d',[RoundTo(ValorParcela*100, 0)]);
  pValorTaxaServico := Format('%.12d',[RoundTo(ValorTaxaServico*100, 0)]);
  pPermiteAlteracao := IfThen(PermiteAlteracao, 'S', 'N');
  pReservado := PadRight('0000', 161);

  fEmTransacao := True;
  pNumeroControle := AllocMem(6);
  try
    GravarLog('TransacaoCartaoCreditoCompleta( '+pValorTransacao+', '+pNumeroCupomVenda+', ,'+
      pTipoOperacao+', '+pNumeroParcelas+', '+pValorParcela+', '+pValorTaxaServico+', '+
      pPermiteAlteracao+', '+ pReservado+' )');
    iRet := xTransacaoCartaoCreditoCompleta( PAnsiChar(pValorTransacao),
                                             PAnsiChar(pNumeroCupomVenda),
                                             pNumeroControle,
                                             PAnsiChar(pTipoOperacao),
                                             PAnsiChar(pNumeroParcelas),
                                             PAnsiChar(pValorParcela),
                                             PAnsiChar(pValorTaxaServico),
                                             PAnsiChar(pPermiteAlteracao),
                                             PAnsiChar(pReservado));
    NumeroControle := String(pNumeroControle);
    GravarLog('  ret: '+IntToStr(iRet)+', NumeroControle: '+NumeroControle);
    TratarErroPayKit(iRet);
  finally
    Freemem(pNumeroControle);
    fEmTransacao := False;
  end;
end;

procedure TACBrTEFPayKitAPI.ExibirMensagem(const AMsg: String;
  TipoMensagem: TACBrTEFPayKitTipoMensagem; MilissegundosExibicao: Integer);
begin
  if not VerificarSeEstaEmTransacao then
    Exit;
  if Assigned(fOnExibeMensagem) then
    fOnExibeMensagem(AMsg, TipoMensagem, MilissegundosExibicao);
end;

procedure TACBrTEFPayKitAPI.PerguntarMenu(const Titulo: String;
  Opcoes: TStringList; var ItemSelecionado: Integer);
begin
  if not VerificarSeEstaEmTransacao then
    Exit;
  if Assigned(fQuandoPerguntarMenu) then
    fQuandoPerguntarMenu(Titulo, Opcoes, ItemSelecionado);
end;

procedure TACBrTEFPayKitAPI.PerguntarCampo(
  DefinicaoCampo: TACBrTEFPayKitDefinicaoCampo; var Resposta: String;
  var Acao: Integer);
begin
  if not VerificarSeEstaEmTransacao then
    Exit;
  Acao := 0; // 0 - O operador digitou o valor, -1 - A operação foi cancelada
  if Assigned(fQuandoPerguntarCampo) then
    fQuandoPerguntarCampo(DefinicaoCampo, Resposta, Acao);
  Acao := min(max(Acao, -1), 0);
end;

function TACBrTEFPayKitAPI.VerificarTransacaoEmAndamento(
  EstadoOperacao: TACBrTEFPayKitEstadoOperacao): Boolean;
var
  Cancelar: Boolean;
begin
  if not VerificarSeEstaEmTransacao then
  begin
    Cancelar := True;
    Exit;
  end;

  Cancelar := False;
  if Assigned(fOnTransacaoEmAndamento) then
    fOnTransacaoEmAndamento(EstadoOperacao, Cancelar);

  Result := not Cancelar;
end;

procedure TACBrTEFPayKitAPI.ExibirQRCode(const DadosQRCode: String);
begin
  if not VerificarSeEstaEmTransacao then
    Exit;
  if Assigned(fQuandoExibirQRCode) then
    fQuandoExibirQRCode(DadosQRCode);
end;

procedure TACBrTEFPayKitAPI.IdentificacaoAutomacaoComercial;
var
  iRet: LongInt;
  pNomeAutomacao, pVersaoAutomacao, pReservado: AnsiString;
begin
  pNomeAutomacao := PadRight(fNomeAutomacao, 20);
  pVersaoAutomacao := PadRight(fVersaoAutomacao, 20);
  pReservado := PadRight('010', 256);   // O segundo byte informa se a automação está integrada com QR Code ('1' se sim, '0' se não)
  GravarLog('IdentificacaoAutomacaoComercial( '+pNomeAutomacao+', '+pVersaoAutomacao+', '+pReservado+ ')');
  iRet := xIdentificacaoAutomacaoComercial(PAnsiChar(pNomeAutomacao), PAnsiChar(pVersaoAutomacao), PAnsiChar(pReservado));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraModoDesfazimento;
var
  iRet: LongInt;
begin
  GravarLog('ConfiguraModoDesfazimento( '+IntToStr(fModoDesfazimento)+' )');
  iRet := xConfiguraModoDesfazimento(fModoDesfazimento);
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraCNPJEstabelecimento;
var
  iRet: LongInt;
  pCNPJEstabelecimento: AnsiString;
begin
  pCNPJEstabelecimento := PadRight(fCNPJEstabelecimento, 14);
  GravarLog('ConfiguraCNPJEstabelecimento( '+pCNPJEstabelecimento+' )');
  iRet := xConfiguraCNPJEstabelecimento(PAnsiChar(pCNPJEstabelecimento));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraEmpresaLojaPDV;
var
  iRet: LongInt;
  pNumeroEmpresa, pNumeroLoja, pNumeroPDV: AnsiString;
begin
  pNumeroEmpresa := Format('%.4d',[fNumeroEmpresa]);
  pNumeroLoja := Format('%.4d',[fNumeroLoja]);
  pNumeroPDV := Format('%.4d',[fNumeroPDV]);
  GravarLog('ConfiguraEmpresaLojaPDV( '+pNumeroEmpresa+', '+pNumeroLoja+', '+pNumeroPDV+' )');
  iRet := xConfiguraEmpresaLojaPDV(PAnsiChar(pNumeroEmpresa), PAnsiChar(pNumeroLoja), PAnsiChar(pNumeroPDV));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.ConfiguraComunicacaoDTEF;
var
  iRet: LongInt;
  pConfiguracaoIpPortaSsl: AnsiString;
begin
  pConfiguracaoIpPortaSsl := Trim(fConfiguracaoIpPortaSsl);
  if (pConfiguracaoIpPortaSsl = '') then
    Exit;

  GravarLog('ConfiguraComunicacaoDTEF( '+pConfiguracaoIpPortaSsl+' )');
  iRet := xConfiguraComunicacaoDTEF(PAnsiChar(pConfiguracaoIpPortaSsl));
  GravarLog('  ret: '+IntToStr(iRet));
  TratarErroPayKit(iRet);
end;

procedure TACBrTEFPayKitAPI.BuscaCertificado;
var
  iRet: LongInt;
  pURL, pPathCertificado: AnsiString;
begin
  pURL := Trim(fURLCertificado);
  if (pURL = '') then
    pURL := CPayKitURLCertificado;

  fEmTransacao := True;
  try
    pPathCertificado := PathWithoutDelim(CalcPayKitPath(CPayKitDirBin));
    GravarLog('BuscaCertificado( '+pURL+', '+pPathCertificado+' )');
    iRet := xBuscaCertificado(PAnsiChar(pURL), PAnsiChar(pPathCertificado));
    GravarLog('  ret: '+IntToStr(iRet));
    TratarErroPayKit(iRet);
  finally
    fEmTransacao := False;
  end;
end;

procedure TACBrTEFPayKitAPI.SetInicializada(AValue: Boolean);
begin
  if (fInicializada = AValue) then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFPayKitAPI.SetCNPJEstabelecimento(AValue: String);
begin
  if fCNPJEstabelecimento = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fCNPJEstabelecimento := LeftStr(OnlyNumber(AValue), 14);
end;

procedure TACBrTEFPayKitAPI.SetConfiguracaoIpPortaSsl(AValue: String);
begin
  if fConfiguracaoIpPortaSsl = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fConfiguracaoIpPortaSsl := AValue;
end;

procedure TACBrTEFPayKitAPI.SetURLCertificado(AValue: String);
begin
  if fURLCertificado = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fURLCertificado := AValue;
end;

procedure TACBrTEFPayKitAPI.SetModoDesfazimento(AValue: Byte);
begin
  if fModoDesfazimento = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fModoDesfazimento := max(0, min(1, AValue));
end;

procedure TACBrTEFPayKitAPI.SetNomeAutomacao(AValue: String);
begin
  if fNomeAutomacao = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNomeAutomacao := LeftStr(AValue, 20);
end;

procedure TACBrTEFPayKitAPI.SetNumeroEmpresa(AValue: Integer);
begin
  if fNumeroEmpresa = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNumeroEmpresa := max(AValue, 0);
end;

procedure TACBrTEFPayKitAPI.SetNumeroLoja(AValue: Integer);
begin
  if fNumeroLoja = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNumeroLoja := max(AValue, 0);
end;

procedure TACBrTEFPayKitAPI.SetNumeroPDV(AValue: Integer);
begin
  if fNumeroPDV = AValue then
    Exit;
  if fInicializada then
    DoException(sErrLibJaInicializada);
  fNumeroPDV := max(AValue, 0);
end;

procedure TACBrTEFPayKitAPI.SetVersaoAutomacao(AValue: String);
begin
  if fVersaoAutomacao = AValue then Exit;
  fVersaoAutomacao := LeftStr(AValue, 20);
end;

procedure TACBrTEFPayKitAPI.SetPathPayKit(AValue: String);
var
  p: String;
begin
  if (fPathPayKit = AValue) then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.SetPathPayKit( '+AValue+' )');

  if fInicializada then
    DoException(sErrLibJaInicializada);

  if (AValue = '') then
  begin
    fPathPayKit := '';
    Exit;
  end;

  p := PathWithDelim(ExtractFilePath(AValue));
  if FileExists(p + CPayKitLib) then  // Informou o diretório Bin... voltando um Path
    p := copy(p, 1, Length(p)-Length(CPayKitDirBin)-1);

  if not DirectoryExists(p + CPayKitDirBin) then
    DoException(Format(sErrDirPayKitInvalido, [CPayKitDirBin, p]));

  fPathPayKit := p;
end;

function TACBrTEFPayKitAPI.CalcPayKitPath(ASubFolder: String): String;
var
  p: String;
begin
  p := PathPayKit;
  if (p = '') then
    p := ApplicationPath;

  if not DirectoryExists(p + ASubFolder) then
    DoException(Format(sErrDirPayKitInvalido, [ASubFolder, p]));

  Result := p + ASubFolder + PathDelim;
end;

procedure TACBrTEFPayKitAPI.VerificarEAjustarConf;
var
  p, f: String;
  Ini: TMemIniFile;
begin
  CriarArquivoConfSeNaoExistir;

  p := CalcPayKitPath(CPayKitDirBin);
  f := p + CPayKitConf;
  if not FileExists(f) then
    DoException(Format(sErrArqPayKitNaoEncontrado, [CPayKitConf, p]));

  Ini := TMemIniFile.Create(f);
  try
    Ini.WriteString('CONFIG', 'DIRETORIOBASE', PathWithoutDelim(p) );
    Ini.WriteString('CONFIG', 'QTDIRETORIOBASE', p + 'QtApplication' );
    Ini.WriteString('DIRETORIOS', 'CUPONS', PathWithoutDelim(CalcPayKitPath('Cupons')) );
    Ini.WriteString('DIRETORIOS', 'INTERNO', PathWithoutDelim(CalcPayKitPath('Interno')) );
    if (fPortaPinPad <> '') then
      Ini.WriteString('PIN', 'PORTA', fPortaPinPad );
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TACBrTEFPayKitAPI.CriarArquivoConfSeNaoExistir;
var
  p, f: String;
  Ini: TMemIniFile;
begin
  p := CalcPayKitPath(CPayKitDirBin);
  f := p + CPayKitConf;
  if FileExists(f) then
    Exit;

  Ini := TMemIniFile.Create(f);
  try
    Ini.WriteString('CONFIG', 'DIRETORIOBASE', PathWithoutDelim(p) );
    Ini.WriteString('CONFIG', 'VERSAODPOS8', '0');
    Ini.WriteInteger('CONFIG', 'NIVELDEBUG', 10);
    Ini.WriteString('CONFIG', 'QTDIRETORIOBASE', p + 'QtApplication' );
    Ini.WriteInteger('CONFIG', 'QTPORTASOCKET', 30000);

    Ini.WriteString('DIRETORIOS', 'CUPONS', PathWithoutDelim(CalcPayKitPath('Cupons')) );
    Ini.WriteString('DIRETORIOS', 'INTERNO', PathWithoutDelim(CalcPayKitPath('Interno')) );

    Ini.WriteInteger('PIN', 'ProcurarPinpad', 1);
    Ini.WriteInteger('PIN', 'Ativo', 1);
    Ini.WriteInteger('PIN', 'TipoBiblioteca', 0);
    Ini.WriteString('PIN', 'PORTA', fPortaPinPad);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function TACBrTEFPayKitAPI.VerificarSeEstaEmTransacao: Boolean;
begin
  Result := EmTransacao;
  if not Result then
    GravarLog('  Sem Transacao');
end;

procedure TACBrTEFPayKitAPI.LoadLibFunctions;

  procedure PayKitFunctionDetect(LibName, FuncName: AnsiString; var LibPointer: Pointer;
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
  sLibName, p: string;
begin
  if fCarregada then
    Exit;

  p := CalcPayKitPath(CPayKitDirBin);
  if not FileExists(p + CPayKitLib) then
    DoException(Format(sErrArqPayKitNaoEncontrado, [CPayKitLib, p]));

  sLibName := p + CPayKitLib;
  GravarLog('TACBrTEFPayKitAPI.LoadLibFunctions - '+sLibName);

  PayKitFunctionDetect(sLibName, 'TransacaoCheque', @xTransacaoCheque);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoCredito', @xTransacaoCartaoCredito);
  //PayKitFunctionDetect(sLibName, 'ConfirmacaoCartaoCredito', @xConfirmacaoCartaoCredito);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoDebito', @xTransacaoCartaoDebito);
  //PayKitFunctionDetect(sLibName, 'ConfirmacaoCartaoDebito', @xConfirmacaoCartaoDebito);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoVoucher', @xTransacaoCartaoVoucher);
  //PayKitFunctionDetect(sLibName, 'ConfirmacaoCartaoVoucher', @xConfirmacaoCartaoVoucher);
  PayKitFunctionDetect(sLibName, 'TransacaoCancelamentoPagamento', @xTransacaoCancelamentoPagamento);
  PayKitFunctionDetect(sLibName, 'TransacaoPreAutorizacaoCartaoCredito', @xTransacaoPreAutorizacaoCartaoCredito);
  PayKitFunctionDetect(sLibName, 'TransacaoConsultaParcelas', @xTransacaoConsultaParcelas);
  PayKitFunctionDetect(sLibName, 'TransacaoResumoVendas', @xTransacaoResumoVendas);
  PayKitFunctionDetect(sLibName, 'TransacaoReimpressaoCupom', @xTransacaoReimpressaoCupom);
  PayKitFunctionDetect(sLibName, 'ConfirmaCartao', @xConfirmaCartao);
  PayKitFunctionDetect(sLibName, 'FinalizaTransacao', @xFinalizaTransacao);
  PayKitFunctionDetect(sLibName, 'ObtemLogUltimaTransacao', @xObtemLogUltimaTransacao);
  PayKitFunctionDetect(sLibName, 'VersaoDPOS', @xVersaoDPOS);
  PayKitFunctionDetect(sLibName, 'InicializaDPOS', @xInicializaDPOS);
  PayKitFunctionDetect(sLibName, 'FinalizaDPOS', @xFinalizaDPOS);
  PayKitFunctionDetect(sLibName, 'IdentificacaoAutomacaoComercial', @xIdentificacaoAutomacaoComercial);
  PayKitFunctionDetect(sLibName, 'ConfiguraModoDesfazimento', @xConfiguraModoDesfazimento);
  PayKitFunctionDetect(sLibName, 'ConfiguraCNPJEstabelecimento', @xConfiguraCNPJEstabelecimento);
  PayKitFunctionDetect(sLibName, 'ConfiguraEmpresaLojaPDV', @xConfiguraEmpresaLojaPDV);
  PayKitFunctionDetect(sLibName, 'ConfiguraComunicacaoDTEF', @xConfiguraComunicacaoDTEF);
  PayKitFunctionDetect(sLibName, 'BuscaCertificado', @xBuscaCertificado);
  PayKitFunctionDetect(sLibName, 'TransacaoEspecial', @xTransacaoEspecial);
  PayKitFunctionDetect(sLibName, 'LeIdentificacaoPinPad', @xLeIdentificacaoPinPad);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoCreditoCompleta', @xTransacaoCartaoCreditoCompleta);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoDebitoCompleta', @xTransacaoCartaoDebitoCompleta);
  PayKitFunctionDetect(sLibName, 'TransacaoCartaoVoucherCompleta', @xTransacaoCartaoVoucherCompleta);
  PayKitFunctionDetect(sLibName, 'TransacaoManualPOSCompleta', @xTransacaoManualPOSCompleta);

  PayKitFunctionDetect(sLibName, 'RegPDVDisplayTerminal', @xRegPDVDisplayTerminal);
  PayKitFunctionDetect(sLibName, 'RegPDVDisplayErro', @xRegPDVDisplayErro);
  PayKitFunctionDetect(sLibName, 'RegPDVMensagem', @xRegPDVMensagem);
  PayKitFunctionDetect(sLibName, 'RegPDVBeep', @xRegPDVBeep);
  PayKitFunctionDetect(sLibName, 'RegPDVSolicitaConfirmacao', @xRegPDVSolicitaConfirmacao);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraCartao', @xRegPDVEntraCartao);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraDataValidade', @xRegPDVEntraDataValidade);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraData', @xRegPDVEntraData);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraCodigoSeguranca', @xRegPDVEntraCodigoSeguranca);
  PayKitFunctionDetect(sLibName, 'RegPDVSelecionaOpcao', @xRegPDVSelecionaOpcao);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraValor', @xRegPDVEntraValor);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraNumero', @xRegPDVEntraNumero);
  PayKitFunctionDetect(sLibName, 'RegPDVOperacaoCancelada', @xRegPDVOperacaoCancelada);
  PayKitFunctionDetect(sLibName, 'RegPDVSetaOperacaoCancelada', @xRegPDVSetaOperacaoCancelada);
  PayKitFunctionDetect(sLibName, 'RegPDVProcessaMensagens', @xRegPDVProcessaMensagens);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraString', @xRegPDVEntraString);
  PayKitFunctionDetect(sLibName, 'RegPDVConsultaAVS', @xRegPDVConsultaAVS);
  PayKitFunctionDetect(sLibName, 'RegPDVMensagemAdicional', @xRegPDVMensagemAdicional);
  PayKitFunctionDetect(sLibName, 'RegPDVImagemAdicional', @xRegPDVImagemAdicional);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraCodigoBarras', @xRegPDVEntraCodigoBarras);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraCodigoBarrasLido', @xRegPDVEntraCodigoBarrasLido);
  PayKitFunctionDetect(sLibName, 'RegPDVMensagemAlerta', @xRegPDVMensagemAlerta);
  PayKitFunctionDetect(sLibName, 'RegPDVPreviewComprovante', @xRegPDVPreviewComprovante);
  PayKitFunctionDetect(sLibName, 'RegPDVSelecionaPlanos', @xRegPDVSelecionaPlanos);
  PayKitFunctionDetect(sLibName, 'RegPDVSelecionaPlanosEx', @xRegPDVSelecionaPlanosEx);
  PayKitFunctionDetect(sLibName, 'RegPDVEntraValorEspecial', @xRegPDVEntraValorEspecial);
  PayKitFunctionDetect(sLibName, 'RegPDVComandos', @xRegPDVComandos);

  fCarregada := True;
end;

procedure TACBrTEFPayKitAPI.UnLoadLibFunctions;
var
  sLibName: String;
begin
  if not fCarregada then
    Exit;

  GravarLog('TACBrTEFPayKitAPI.UnLoadLibFunctions');

  sLibName := CalcPayKitPath(CPayKitDirBin) + CPayKitLib;
  UnLoadLibrary( sLibName );
  fCarregada := False;
  ClearMethodPointers;
end;

procedure TACBrTEFPayKitAPI.ClearMethodPointers;
begin
  xTransacaoCheque := Nil;
  xTransacaoCartaoCredito := Nil;
  //xConfirmacaoCartaoCredito := Nil;
  xTransacaoCartaoDebito := Nil;
  //xConfirmacaoCartaoDebito := Nil;
  xTransacaoCartaoVoucher := Nil;
  //xConfirmacaoCartaoVoucher := Nil;
  xTransacaoCancelamentoPagamento := Nil;
  xTransacaoPreAutorizacaoCartaoCredito := Nil;
  xTransacaoConsultaParcelas := Nil;
  xTransacaoResumoVendas := Nil;
  xTransacaoReimpressaoCupom := Nil;
  xConfirmaCartao := Nil;
  xFinalizaTransacao := Nil;
  xObtemLogUltimaTransacao := Nil;
  xVersaoDPOS := Nil;
  xInicializaDPOS := Nil;
  xFinalizaDPOS := Nil;
  xIdentificacaoAutomacaoComercial := Nil;
  xConfiguraModoDesfazimento := Nil;
  xConfiguraCNPJEstabelecimento := Nil;
  xConfiguraEmpresaLojaPDV := Nil;
  xConfiguraComunicacaoDTEF := Nil;
  xBuscaCertificado := Nil;
  xTransacaoEspecial := Nil;
  xLeIdentificacaoPinPad := Nil;
  xTransacaoCartaoCreditoCompleta := Nil;
  xTransacaoCartaoDebitoCompleta := Nil;
  xTransacaoCartaoVoucherCompleta := Nil;
  xTransacaoManualPOSCompleta := Nil;

  xRegPDVDisplayTerminal := Nil;
  xRegPDVDisplayErro := Nil;
  xRegPDVMensagem := Nil;
  xRegPDVBeep := Nil;
  xRegPDVSolicitaConfirmacao := Nil;
  xRegPDVEntraCartao := Nil;
  xRegPDVEntraDataValidade := Nil;
  xRegPDVEntraData := Nil;
  xRegPDVEntraCodigoSeguranca := Nil;
  xRegPDVSelecionaOpcao := Nil;
  xRegPDVEntraValor := Nil;
  xRegPDVEntraNumero := Nil;
  xRegPDVOperacaoCancelada := Nil;
  xRegPDVSetaOperacaoCancelada := Nil;
  xRegPDVProcessaMensagens := Nil;
  xRegPDVEntraString := Nil;
  xRegPDVConsultaAVS := Nil;
  xRegPDVMensagemAdicional := Nil;
  xRegPDVImagemAdicional := Nil;
  xRegPDVEntraCodigoBarras := Nil;
  xRegPDVEntraCodigoBarrasLido := Nil;
  xRegPDVMensagemAlerta := Nil;
  xRegPDVPreviewComprovante := Nil;
  xRegPDVSelecionaPlanos := Nil;
  xRegPDVSelecionaPlanosEx := Nil;
  xRegPDVEntraValorEspecial := Nil;
  xRegPDVComandos := Nil;
end;

procedure TACBrTEFPayKitAPI.RegisterCallBackFunctions;
begin
  xRegPDVDisplayTerminal(CallBackDisplayTerminal);
  xRegPDVDisplayErro(CallBackDisplayErro);
  xRegPDVMensagem(CallBackMensagem);
  xRegPDVBeep(CallBackBeep);
  xRegPDVSolicitaConfirmacao(CallBackSolicitaConfirmacao);
  xRegPDVEntraCartao(CallBackEntraCartao);
  xRegPDVEntraDataValidade(CallBackEntraDataValidade);
  xRegPDVEntraData(CallBackEntraData);
  xRegPDVEntraCodigoSeguranca(CallBackEntraCodigoSeguranca);
  xRegPDVSelecionaOpcao(CallBackSelecionaOpcao);
  xRegPDVEntraValor(CallBackEntraValor);
  xRegPDVEntraNumero(CallBackEntraNumero);
  xRegPDVOperacaoCancelada(CallBackOperacaoCancelada);
  xRegPDVSetaOperacaoCancelada(CallBackSetaOperacaoCancelada);
  xRegPDVProcessaMensagens(CallBackProcessaMensagens);
  xRegPDVEntraString(CallBackEntraString);
  xRegPDVConsultaAVS(CallBackConsultaAVS);
  xRegPDVMensagemAdicional(CallBackMensagemAdicional);
  xRegPDVImagemAdicional(CallBackImagemAdicional);
  xRegPDVEntraCodigoBarras(CallBackEntraCodigoBarras);
  xRegPDVEntraCodigoBarrasLido(CallBackEntraCodigoBarrasLido);
  xRegPDVMensagemAlerta(CallBackMensagemAlerta);
  xRegPDVPreviewComprovante(CallBackPreviewComprovante);
  xRegPDVSelecionaPlanos(CallBackSelecionaPlanos);
  xRegPDVSelecionaPlanosEx(CallBackSelecionaPlanosEx);
  xRegPDVEntraValorEspecial(CallBackEntraValorEspecial);
  xRegPDVComandos(CallBackComandos);
end;

procedure TACBrTEFPayKitAPI.TratarErroPayKit(AErrorCode: LongInt);
var
  MsgErro: String;
begin
  case AErrorCode of
    RET_OK: MsgErro := '';
  else
    MsgErro := Format('Erro retornado: %d', [AErrorCode]);
  end;

  if (MsgErro <> '') then
    DoException(MsgErro);
end;

procedure TACBrTEFPayKitAPI.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFPayKitAPI: '+AErrorMsg);
  raise EACBrTEFPayKitAPI.Create(ACBrStr(AErrorMsg));
end;

procedure TACBrTEFPayKitAPI.VerificarCarregada;
begin
  if not fCarregada then
    DoException(sErrLibNaoInicializada);
end;

finalization
  if Assigned(vTEFPayKit) then
    FreeAndNil(vTEFPayKit);

end.

