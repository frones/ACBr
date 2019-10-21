{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Alexandre Rocha Lima e Marcondes                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

Unit ACBrECF ;

interface
uses ACBrBase, ACBrDevice, ACBrECFClass, ACBrECFVirtual, ACBrPAFClass, ACBrRFD,
     ACBrAAC, ACBrEAD,{Units da ACBr}
     SysUtils , ACBrConsts, Classes
     {$IFNDEF NOGUI}
        {$IF DEFINED(VisualCLX)}
           ,QControls, QForms, QDialogs, QGraphics, QStdCtrls
        {$ELSEIF DEFINED(FMX)}
           ,FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Graphics, FMX.StdCtrls
           ,FMX.Memo, System.UITypes
        {$ELSE}
           ,Controls, Forms, Dialogs, Graphics, StdCtrls
           {$IFDEF DELPHIXE2_UP}
            , System.UITypes
           {$ENDIF}
        {$IFEND}
     {$ENDIF} ;
{$IFDEF FRAMEWORK}
{$UNDEF NOGUI}
{$ENDIF}
type

{ Modelos de ECF Suportados pelo Componente TACBrECF atualmente
  Diveresos fabricantes usam o chipset da Mecaf como por exemplo:
  Elgin, Digiarte, Zanthus, Acr, Aoki, Chronos, Promcomp, TrendsSTS, Unigraph.
  Isso pode ser indentificado por alguma indicação no corpo do equipamento.
  Entretanto não há garantia de plena compatibilidade entre os diferentes
  equipamentos. (Favor reportar possiveis BUGS) }
TACBrECFModelo = (ecfNenhum, ecfNaoFiscal, ecfBematech, ecfSweda, ecfDaruma,
                  ecfSchalter, ecfMecaf, ecfYanco, ecfDataRegis, ecfUrano,
                  ecfICash, ecfQuattro, ecfFiscNET, ecfEpson, ecfNCR,
                  ecfSwedaSTX, ecfEscECF, ecfECFVirtual );
                  
TACBrECFEventoOnError = procedure( var Tratado : Boolean) of object ;
TACBrECFOnAbreCupom = procedure(const CPF_CNPJ, Nome, Endereco : String ) of object ;
TACBrECFOnAbreBilhetePassagem = procedure(const Origem, Destino, Linha, Agencia: String;
  const DataHora: TDateTime; const Poltrona, Plataforma: String; const Tipo: TACBrECFTipoBilhete;
  const UFDestino, PassageiroRG, PassageiroNome, PassageiroEnd: String) of object;
TACBrECFOnVendeItem = procedure(const Codigo, Descricao, AliquotaICMS : String ;
  const Qtd, ValorUnitario, ValorDescontoAcrescimo : Double;
  const Unidade, TipoDescontoAcrescimo, DescontoAcrescimo : String ) of object ;
TACBrECFOnSubtotalizaCupom = procedure( const DescontoAcrescimo: Double;
  const MensagemRodape : AnsiString ) of object ;
TACBrECFOnEfetuaPagamento = procedure( const CodFormaPagto: String;
  const Valor: Double; const Observacao: AnsiString;
  const ImprimeVinculado: Boolean ) of object ;
TACBrECFOnFechaCupom = procedure( const Observacao: AnsiString;
  const IndiceBMP : Integer) of object ;
TACBrECFOnCancelaItemVendido = procedure( const NumItem: Integer) of object ;
TACBrECFOnCancelaItemNaoFiscal = procedure( const NumItem: Integer) of object ;
TACBrECFOnAbreNaoFiscal = procedure( const CPF_CNPJ, Nome, Endereco : String ) of object ;
TACBrECFOnSubtotalizaNaoFiscal = procedure( const DescontoAcrescimo: Double;
  const MensagemRodape: AnsiString ) of object ;
TACBrECFOnEfetuaPagamentoNaoFiscal = procedure(const CodFormaPagto: String;
  const Valor: Double; const Observacao: AnsiString;
  const ImprimeVinculado: Boolean ) of object ;
TACBrECFOnFechaNaoFiscal = procedure( const Observacao: AnsiString;
  const IndiceBMP : Integer ) of object ;
TACBrECFOnSangria = procedure(const Valor: Double; const Obs: AnsiString;
  const DescricaoCNF, DescricaoFPG: String ) of object ;
TACBrECFOnSuprimento = procedure( const Valor: Double; const Obs: AnsiString;
  const DescricaoCNF, DescricaoFPG: String) of object ;
TACBrECFOnRelatorioGerencial = procedure( const Indice: Integer ) of object ;
TACBrECFOnErrorAbreRelatorioGerencial = procedure(var Tratado: Boolean;
  const Indice: Integer ) of object ;
TACBrECFOnChangeEstado = procedure( const EstadoAnterior, EstadoAtual :
  TACBrECFEstado ) of object ;


{ Componente ACBrECF }

{ TACBrECF }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}
TACBrECF = class( TACBrComponent )
  private
    fsDevice : TACBrDevice ;   { SubComponente ACBrDevice }
    fsConfigBarras: TACBrECFConfigBarras;
    fsTagProcessor: TACBrTagProcessor;

    { Propriedades do Componente ACBrECF }
    fsAtivo  : Boolean;
    fsProcurandoECF   : Boolean ;
    fsProcurandoPorta : Boolean ;
    fsModelo : TACBrECFModelo;
    fsMensagemRodape : String ;
    fsRegistrouRFDCNF : Boolean ;
    fsSubTotalPagto :Double;
    fsTotalPago: Double;
    fsIndiceGerencial : Integer ;
    {$IFNDEF NOGUI}
      {$IFNDEF FRAMEWORK}
      {$IFDEF FMX}
      fsFormMsgColor : TAlphaColor ;
      fsFormMsgColorFont: TAlphaColor;
      {$ELSE}
      fsFormMsgColor : TColor ;
      {$ENDIF}
      fsFormMsgFont: TFont ;
      fsMemoBobina: TMemo ;
      {$ENDIF}
      fsMemoOperacao : String ;
      fsMemoParams: TStrings;
      fsOnBobinaAdicionaLinhas: TACBrECFBobinaAdicionaLinhas;
      fsMemoColunas: Integer ;
      fsMemoHTML: Boolean ;
      fsMemoHTMLTitleSise: Integer ;
      fsMemoHTMLFont: String ;
      fsMemoCabecalho, fsMemoRodape ,fsMemoCabItens, fsMemoMascItens : String ;
      fsMemoItens: Integer ;
    {$ENDIF}

    fsECF : TACBrECFClass ;  { Classe com instancia do ECF de fsModelo }
    fsECFVirtual : TACBrECFVirtual;
    fsRFD : TACBrRFD ;
    fsAAC : TACBrAAC ;
    fsEADInterno : TACBrEAD ;
    fsEAD : TACBrEAD ;       { Classe usada para AssinarArquivo com assinatura EAD. }

    fOnAntesAbreCupom : TACBrECFOnAbreCupom;
    fOnAntesAbreBilhetePassagem: TACBrECFOnAbreBilhetePassagem;
    fOnDepoisAbreCupom  : TACBrECFOnAbreCupom;
    fOnErrorAbreCupom  : TACBrECFEventoOnError;
    fOnAntesVendeItem : TACBrECFOnVendeItem;
    fOnDepoisVendeItem  : TACBrECFOnVendeItem;
    fOnErrorVendeItem  : TACBrECFEventoOnError;
    fOnAntesSubtotalizaCupom: TACBrECFOnSubtotalizaCupom;
    fOnDepoisSubtotalizaCupom: TACBrECFOnSubtotalizaCupom;
    fOnErrorSubtotalizaCupom: TACBrECFEventoOnError;
    fOnAntesEfetuaPagamento : TACBrECFOnEfetuaPagamento;
    fOnDepoisEfetuaPagamento  : TACBrECFOnEfetuaPagamento;
    fOnErrorEfetuaPagamento  : TACBrECFEventoOnError;
    fOnAntesFechaCupom : TACBrECFOnFechaCupom;
    fOnDepoisFechaCupom  : TACBrECFOnFechaCupom;
    fOnErrorFechaCupom  : TACBrECFEventoOnError;
    fOnAntesCancelaCupom : TNotifyEvent;
    fOnDepoisCancelaCupom  : TNotifyEvent;
    fOnErrorCancelaCupom  : TACBrECFEventoOnError;
    fOnAntesCancelaItemVendido : TACBrECFOnCancelaItemVendido;
    fOnDepoisCancelaItemVendido  : TACBrECFOnCancelaItemVendido;
    fOnErrorCancelaItemVendido  : TACBrECFEventoOnError;
    fOnAntesAbreNaoFiscal : TACBrECFOnAbreNaoFiscal;
    fOnDepoisAbreNaoFiscal  : TACBrECFOnAbreNaoFiscal;
    fOnErrorAbreNaoFiscal  : TACBrECFEventoOnError;
    fOnAntesSubtotalizaNaoFiscal : TACBrECFOnSubtotalizaNaoFiscal;
    fOnDepoisSubtotalizaNaoFiscal  : TACBrECFOnSubtotalizaNaoFiscal;
    fOnErrorSubtotalizaNaoFiscal  : TACBrECFEventoOnError;
    fOnAntesEfetuaPagamentoNaoFiscal : TACBrECFOnEfetuaPagamentoNaoFiscal;
    fOnDepoisEfetuaPagamentoNaoFiscal  : TACBrECFOnEfetuaPagamentoNaoFiscal;
    fOnErrorEfetuaPagamentoNaoFiscal  : TACBrECFEventoOnError;
    fOnAntesFechaNaoFiscal : TACBrECFOnFechaNaoFiscal;
    fOnDepoisFechaNaoFiscal  : TACBrECFOnFechaNaoFiscal;
    fOnErrorFechaNaoFiscal  : TACBrECFEventoOnError;
    fOnAntesCancelaNaoFiscal : TNotifyEvent;
    fOnDepoisCancelaNaoFiscal  : TNotifyEvent;
    fOnErrorCancelaNaoFiscal  : TACBrECFEventoOnError;
    fOnAntesCancelaItemNaoFiscal : TACBrECFOnCancelaItemNaoFiscal;
    fOnDepoisCancelaItemNaoFiscal  : TACBrECFOnCancelaItemNaoFiscal;
    fOnErrorCancelaItemNaoFiscal  : TACBrECFEventoOnError;
    fOnAntesSangria : TACBrECFOnSangria;
    fOnDepoisSangria  : TACBrECFOnSangria;
    fOnErrorSangria  : TACBrECFEventoOnError;
    fOnAntesSuprimento : TACBrECFOnSuprimento;
    fOnDepoisSuprimento  : TACBrECFOnSuprimento;
    fOnErrorSuprimento  : TACBrECFEventoOnError;
    fOnAntesLeituraX : TNotifyEvent;
    fOnDepoisLeituraX  : TNotifyEvent;
    fOnErrorLeituraX  : TACBrECFEventoOnError;
    fOnAntesReducaoZ : TNotifyEvent;
    fOnDepoisReducaoZ  : TNotifyEvent;
    fOnErrorReducaoZ  : TACBrECFEventoOnError;
    fOnAntesAbreRelatorioGerencial : TACBrECFOnRelatorioGerencial;
    fOnDepoisAbreRelatorioGerencial  : TACBrECFOnRelatorioGerencial;
    fOnErrorAbreRelatorioGerencial  : TACBrECFOnErrorAbreRelatorioGerencial;
    fOnAntesAbreCupomVinculado : TNotifyEvent;
    fOnDepoisAbreCupomVinculado  : TNotifyEvent;
    fOnErrorAbreCupomVinculado  : TACBrECFEventoOnError;
    fOnAntesFechaRelatorio : TNotifyEvent;
    fOnDepoisFechaRelatorio  : TNotifyEvent;
    fOnErrorFechaRelatorio  : TACBrECFEventoOnError;
    fOnChangeEstado : TACBrECFOnChangeEstado;
    fsOnPAFCalcEAD  : TACBrEADCalc;
    fsOnPAFGetKeyRSA : TACBrEADGetChave ;

    fsGavetaSinalInvertido  : Boolean;
    fsIdentificarOperador   : Boolean;
    fsNumSerieCache         : String ;

    FDAVItemCount: Integer;
    FDAVTotal: Double;
    FQuebraLinhaRodape : Boolean;
    FVerificarApenasNumeroSerieNoAAC: Boolean;

    function GetArredondaItemMFD : Boolean ;
    function GetIgnorarErroSemPapel : Boolean ;
    function GetIgnorarTagsFormatacao: Boolean;
    function GetNumMaxLinhasRodapeClass: Integer;
    function GetOnGravarLog: TACBrGravarLog;
    function GetPaginaDeCodigoClass : Word ;
    function GetTipoUltimoDocumentoClass : TACBrECFTipoDocumento ;
    function GetTotalCancelamentosEmAbertoClass: Double;

    procedure SetArredondaItemMFD(const AValue : Boolean) ;
    procedure SetAtivo(const AValue: Boolean);
    procedure SetIgnorarErroSemPapel(AValue : Boolean) ;
    procedure SetIgnorarTagsFormatacao(AValue: Boolean);
    procedure SetOnGravarLog(AValue: TACBrGravarLog);
    procedure SetPaginaDeCodigoClass(const AValue : Word) ;
    procedure SetModelo(const AValue: TACBrECFModelo);
    procedure SetPorta(const AValue: String);
    procedure SetRetentar(const AValue: Boolean);
    procedure SetControlePorta(AValue: Boolean);
    procedure SetTimeOut(const AValue: Integer);
    procedure SetIntervaloAposComando(const AValue: Integer);
    procedure SetMsgAguarde(const AValue: String);
    procedure SetMsgTrabalhando(const AValue: String);
    procedure SetMsgRelatorio(const AValue: String);
    procedure SetPausaRelatorio(const AValue: Integer);
    procedure SetTempoInicioMsg(const AValue: Integer);
    procedure SetBloqueiaMouseTeclado(const AValue: Boolean);
    procedure SetExibeMensagem(const AValue: Boolean);
    procedure SetMsgPoucoPapel(const AValue: Integer);
    procedure SetDescricaoGrande(const AValue: Boolean);
    procedure SetOnMsgAguarde(const AValue: TACBrECFMsgAguarde);
    procedure SetOnAguardandoRespostaChange(const AValue: TNotifyEvent);
    procedure SetOnMsgPoucoPapel(const AValue: TNotifyEvent);
    procedure SetOnMsgRetentar(const AValue: TACBrECFMsgRetentar);
    procedure SetOnErrorSemPapel(AValue : TNotifyEvent) ;

    function GetPorta: String;
    function GetRetentar: Boolean;
    function GetControlePorta: Boolean;
    function GetTimeOut: Integer;
    function GetIntervaloAposComando: Integer ;
    function GetBloqueiaMouseTeclado: Boolean;
    function GetMsgAguarde: String;
    function GetMsgTrabalhando: String;
    function GetMsgRelatorio: String;
    function GetPausaRelatorio: Integer;
    function GetExibeMensagem: Boolean;
    function GetTempoInicioMsg: Integer;
    function GetMsgPoucoPapel: Integer;
    function GetOnMsgAguarde: TACBrECFMsgAguarde;
    function GetOnAguardandoRespostaChange: TNotifyEvent;
    function GetOnMsgPoucoPapel: TNotifyEvent;
    function GetOnErrorSemPapel : TNotifyEvent ;
    function GetOnMsgRetentar: TACBrECFMsgRetentar;

    function GetAguardandoRespostaClass: Boolean;
    function GetColunasClass: Integer;
    function GetComandoEnviadoClass: AnsiString;
    function GetRespostaComandoClass: AnsiString;
    function GetDataHoraClass: TDateTime;
    function GetNumCupomClass: String;
    function GetNumECFClass: String;
    function GetNumSerieClass: String;
    function GetNumSerieMFDClass: String;
    function GetNumVersaoClass: String;
    function GetNumReducoesZRestantesClass: String;
    function GetEstadoClass: TACBrECFEstado;
    function GetPoucoPapelClass: Boolean;
    function GetChequeProntoClass: Boolean;
    function GetGavetaAbertaClass: Boolean;
    function GetSubTotalClass: Double;
    function GetTotalPagoClass: Double;
    function GetAliquotasClass: TACBrECFAliquotas;
    function GetTotalizadoresNaoTributadosClass: TACBrECFTotalizadoresNaoTributados;
    function GetFormasPagamentoClass: TACBrECFFormasPagamento;
    function GetComprovantesNaoFiscaisClass : TACBrECFComprovantesNaoFiscais;
    function GetRelatoriosGerenciaisClass : TACBrECFRelatoriosGerenciais;
    function GetModeloStrClass: String;
    function GetRFDIDClass: String;
    function GetDescricaoGrande: Boolean;
    function GetMsgPausaRelatorio: String;
    procedure SetMsgPausaRelatorio(const AValue: String);
    function GetLinhasEntreCupons: Integer;
    procedure SetLinhasEntreCupons(const AValue: Integer);
    function GetMaxLinhasBuffer: Integer;
    procedure SetMaxLinhasBuffer(const AValue: Integer);

    Function MemoAssigned: Boolean;
    {$IFNDEF NOGUI}
      {$IFNDEF FRAMEWORK}
      procedure SetFormMsgFonte(const AValue: TFont);
      procedure SetMemoBobina(const AValue: TMemo);
      {$ENDIF}
      procedure SetMemoParams(const AValue: TStrings);
      procedure MemoAdicionaLinha( Linhas : String ) ;

      procedure MemoAdicionaCabecalho ;
      Function MemoTraduzCode( Linha : String ) : String ;
      procedure MemoTitulo(ATitulo : String) ;
      procedure MemoSubtotaliza(DescontoAcrescimo: Double);
      procedure MemoEfetuaPagamento(Descricao: String; Valor: Double;
          Observacao: String) ;

      {$IFDEF FMX}
        function GetOnCriarFormMsg: TACBrFMXCustomForm;
        function GetOnDrawFormMsg: TACBrECFMsgAguarde;
        procedure SetOnCriarFormMsg(const Value: TACBrFMXCustomForm);
        procedure SetOnDrawFormMsg(const Value: TACBrECFMsgAguarde);
      {$ENDIF}

    {$ENDIF}
    function GetOperador: String;
    procedure SetOperador(const AValue: String);
    function GetHorarioVeraoClass: Boolean;
    function GetArredondaClass: Boolean;
    function GetMFDClass: Boolean;
    function GetTermicaClass: Boolean;
    function GetIdentificaConsumidorRodapeClass: Boolean;
    function GetNumLojaClass: String;
    function GetNumCROClass: String;
    function GetNumCCFClass: String;
    function GetNumGNFClass: String;
    function GetNumGRGClass: String;
    function GetNumCDCClass: String;
    function GetNumCFCClass: String;
    function GetNumGNFCClass: String;
    function GetNumCFDClass: String;
    function GetNumCRZClass: String ;
    function GetNumNCNClass: String;
    function GetNumCCDCClass: String;
    function GetArredondaPorQtd: Boolean;
    procedure SetArredondaPorQtd(const AValue: Boolean);
    function GetDecimaisPreco: Integer;
    procedure SetDecimaisPreco(const AValue: Integer);
    function GetDecimaisQtd: Integer;
    procedure SetDecimaisQtd(const AValue: Integer);
    function GetAguardaImpressao: Boolean;
    procedure SetAguardaImpressao(const AValue: Boolean);
    function GetUnidadesMedidaClass: TACBrECFUnidadesMedida;

    procedure DoAcharPorta ;

    function GetArqLOG: String;
    procedure SetArqLOG(const AValue: String);
    function GetComandoLOGClass: AnsiString;
    procedure SetComandoLOGClass(const AValue: AnsiString);
    function GetCNPJClass: String;
    function GetIEClass: String;
    function GetIMClass: String;
    function GetClicheClass: AnsiString;
    function GetUsuarioAtualClass: String;
    function GetDataHoraSBClass: TDateTime;
    function GetSubModeloECFClass: String ;

    function GetPAFClass: String;
    function GetDadosReducaoZ: String;
    function GetDadosUltimaReducaoZ: String;
    function GetDataMovimentoClass: TDateTime;
    function GetDataHoraUltimaReducaoZClass : TDateTime ;
    function GetGrandeTotalClass: Double;
    function GetNumCOOInicialClass: String;
    function GetVendaBrutaClass: Double;
    function GetTotalAcrescimosClass: Double;
    function GetTotalCancelamentosClass: Double;
    function GetTotalDescontosClass: Double;
    function GetTotalTrocoClass: Double;
    function GetTotalSubstituicaoTributariaClass: Double;
    function GetTotalNaoTributadoClass: Double;
    function GetTotalIsencaoClass: Double;
    function GetTotalNaoFiscalClass: Double;
    function GetTotalAcrescimosISSQNClass: Double;
    function GetTotalCancelamentosISSQNClass: Double;
    function GetTotalDescontosISSQNClass: Double;
    function GetTotalAcrescimosOPNFClass: Double;
    function GetTotalCancelamentosOPNFClass: Double;
    function GetTotalDescontosOPNFClass: Double;
    function GetTotalIsencaoISSQNClass: Double;
    function GetTotalNaoTributadoISSQNClass: Double;
    function GetTotalSubstituicaoTributariaISSQNClass: Double;
    function GetNumUltimoItemClass: Integer;
    function GetConsumidorClass: TACBrECFConsumidor;
    function GetDadosReducaoZClass: TACBrECFDadosRZ;
    procedure SetECFVirtual(AValue: TACBrECFVirtual);
    procedure SetRFD(const AValue: TACBrRFD);
    procedure SetAAC(const AValue: TACBrAAC);
    procedure SetEAD(const AValue: TACBrEAD);
    Function RFDAtivo : Boolean ;
    function GetParamDescontoISSQNClass: Boolean;
    function GetMFAdicional: String;
    function GetInfoRodapeCupom: TACBrECFRodape;
    procedure SetInfoRodapeCupom(const Value: TACBrECFRodape);
    function GetRespostasComandoClass: TACBrInformacoes;
    function GetRodapePostos: String;
    function GetRodapeRestaurante: String;
    function GetRodapeUF: String;
    function GetRodapeImposto: String;
    function GetOnChequeEstado: TACBrECFOnChequeEstado;
    procedure SetOnChequeEstado(const Value: TACBrECFOnChequeEstado);
    function GetQuebraLinha(const Value :string):String;

    procedure IniciaVendeItem(var Codigo, Descricao, AliquotaICMS, AliquotaECF: String;
      var Qtd, ValorUnitario, ValorDescontoAcrescimo: Double;
      var Unidade, TipoDescontoAcrescimo, DescontoAcrescimo: String;
      var CodDepartamento: Integer);
    procedure FinalizaVendeItem(Codigo, Descricao, AliquotaICMS, AliquotaECF: String;
      Qtd: Double; ValorUnitario, ValorDescontoAcrescimo: Double;
      Unidade, TipoDescontoAcrescimo, DescontoAcrescimo: String);

  protected
    fpUltimoEstadoObtido: TACBrECFEstado;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    Function GetACBrEAD : TACBrEAD ;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override ;

    procedure Ativar ;
    procedure Desativar ;
    property Ativo : Boolean read fsAtivo write SetAtivo ;

    property ECF : TACBrECFClass read fsECF ;

    function TestarDialog : Boolean ;
    function AcharECF( ProcuraModelo: Boolean = true;
                       ProcuraPorta : Boolean = true;
                       ATimeOut : Integer = 3): Boolean ;
    function AcharPorta(ATimeOut: Integer = 3): Boolean;

    { Propriedades lidas da Classe Instanciada em fsECF }
    Property Colunas            : Integer     read GetColunasClass ;
    Property AguardandoResposta : Boolean     read GetAguardandoRespostaClass ;
    Property ComandoEnviado     : AnsiString  read GetComandoEnviadoClass ;
    Property RespostaComando    : AnsiString  read GetRespostaComandoClass ;
    Property RespostasComando    : TACBrInformacoes  read GetRespostasComandoClass ;
    property ComandoLOG         : AnsiString  read GetComandoLOGClass
       write SetComandoLOGClass ;
    property IgnorarErroSemPapel : Boolean read GetIgnorarErroSemPapel
                write SetIgnorarErroSemPapel ;

    Property AguardaImpressao : Boolean read GetAguardaImpressao
                 write SetAguardaImpressao ;

    { ECF - Variaveis }
    Property ModeloStr : String    read GetModeloStrClass;
    Property RFDID     : String    read GetRFDIDClass;
    Property DataHora  : TDateTime read GetDataHoraClass ;
    Property NumCupom  : String    read GetNumCupomClass ;
    Property NumCOO    : String    read GetNumCupomClass ;   //Contador de Ordem de Operação
    Property NumLoja   : String    read GetNumLojaClass  ;
    Property NumECF    : String    read GetNumECFClass   ;
    Property NumSerie  : String    read GetNumSerieClass ;
    Property NumSerieMFD  : String read GetNumSerieMFDClass ;
    Property NumVersao : String    read GetNumVersaoClass;
    Property NumReducoesZRestantes: String read GetNumReducoesZRestantesClass;
    Property NumMaxLinhasRodape: Integer read GetNumMaxLinhasRodapeClass;

    { Dados da Reducao Z - Registro 60M }
    Property DadosReducaoZ : String  read GetDadosReducaoZ ;
    Property DadosUltimaReducaoZ : String  read GetDadosUltimaReducaoZ ;
    Property DadosReducaoZClass: TACBrECFDadosRZ read GetDadosReducaoZClass;

    { Retorna String com todos os valores no formato: Campo = Valor (1 por linha)}
    Property DataMovimento      : TDateTime  read GetDataMovimentoClass ;
    Property DataHoraUltimaReducaoZ : TDateTime read GetDataHoraUltimaReducaoZClass ;
    Property DataHoraSB         : TDateTime  read GetDataHoraSBClass ;
    Property CNPJ               : String     read GetCNPJClass ;
    Property IE                 : String     read GetIEClass ;
    Property IM                 : String     read GetIMClass ;
    Property Cliche             : AnsiString read GetClicheClass ;
    Property UsuarioAtual       : String     read GetUsuarioAtualClass ;
    Property SubModeloECF       : String     read GetSubModeloECFClass ;
    Property MFAdicional        : String     read GetMFAdicional ;

    Property PAF                : String     read GetPAFClass ;
    Property NumCRZ             : String     read GetNumCRZClass ;     //Contador de Reduções Z
    Property NumCRO             : String     read GetNumCROClass ;     //Contador de Reinício de Operação
    Property NumCCF             : String     read GetNumCCFClass ;     //Contador de Cupom Fiscal
    Property NumGNF             : String     read GetNumGNFClass ;     //Contador Geral de Operação Não-Fiscal
    Property NumGRG             : String     read GetNumGRGClass ;     //Contador Geral de Relatório Gerencial
    Property NumCDC             : String     read GetNumCDCClass ;     //Contador de Comprovante de Crédito ou Débito
    Property NumCFC             : String     read GetNumCFCClass ;     //Contador de Cupom Fiscal Cancelado
    Property NumGNFC            : String     read GetNumGNFCClass ;    //GNFC ou NFC - Contador Geral de Operação Não-Fiscal Cancelada
    Property NumCFD             : String     read GetNumCFDClass ;     //Contador de Fita-detalhe
    Property NumNCN             : String     read GetNumNCNClass ;     //Contador de Comprovantes de Crédito ou Débito Não Emitidos
    Property NumCCDC            : String     read GetNumCCDCClass ;    //Contador de Débito ou Crédito Cancelados
    Property NumCOOInicial      : String     read GetNumCOOInicialClass ;
    Property VendaBruta         : Double     read GetVendaBrutaClass ;
    Property GrandeTotal        : Double     read GetGrandeTotalClass ;
    Property TotalCancelamentos : Double     read GetTotalCancelamentosClass ;
    Property TotalCancelamentosEmAberto : Double  read GetTotalCancelamentosEmAbertoClass ;
    Property TotalDescontos     : Double     read GetTotalDescontosClass ;
    Property TotalAcrescimos    : Double     read GetTotalAcrescimosClass ;
    Property TotalTroco         : Double     read GetTotalTrocoClass ;
    Property TotalSubstituicaoTributaria : Double
       read GetTotalSubstituicaoTributariaClass ;
    Property TotalNaoTributado  : Double     read GetTotalNaoTributadoClass ;
    Property TotalIsencao       : Double     read GetTotalIsencaoClass ;

    Property TotalCancelamentosISSQN          : Double read GetTotalCancelamentosISSQNClass;
    Property TotalDescontosISSQN              : Double read GetTotalDescontosISSQNClass;
    Property TotalAcrescimosISSQN             : Double read GetTotalAcrescimosISSQNClass;
    Property TotalCancelamentosOPNF           : Double read GetTotalCancelamentosOPNFClass;
    Property TotalDescontosOPNF               : Double read GetTotalDescontosOPNFClass;
    Property TotalAcrescimosOPNF              : Double read GetTotalAcrescimosOPNFClass;
    Property TotalSubstituicaoTributariaISSQN : Double read GetTotalSubstituicaoTributariaISSQNClass;
    Property TotalNaoTributadoISSQN           : Double read GetTotalNaoTributadoISSQNClass;
    Property TotalIsencaoISSQN                : Double read GetTotalIsencaoISSQNClass;

    Property TotalNaoFiscal     : Double     read GetTotalNaoFiscalClass ;
    Property NumUltItem         : Integer    read GetNumUltimoItemClass ;

    { Aliquotas de ICMS }
    Property Aliquotas : TACBrECFAliquotas read GetAliquotasClass ;
    procedure CarregaAliquotas ;
    procedure LerTotaisAliquota ;
    function AchaICMSAliquota( Aliquota : Double; Tipo : Char = ' ' ) :
       TACBrECFAliquota ;  overload ;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  overload ;
    function AchaICMSIndice( Indice : String ) : TACBrECFAliquota ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ;

    { TotalizadoresNaoTributados, F1, N1, I1, FS1, NS1, IS1 }
    Property TotalizadoresNaoTributados : TACBrECFTotalizadoresNaoTributados
       read GetTotalizadoresNaoTributadosClass ;
    procedure CarregaTotalizadoresNaoTributados ;
    procedure LerTotaisTotalizadoresNaoTributados ;
    function AchaTotalizadorNaoTributadoIndice( Indice : String ) : TACBrECFTotalizadorNaoTributado ;
    function SomaTotalizadorNaoTributadoIndice( Indice : String ) : Double;

    { Formas de Pagamento }
    Property FormasPagamento : TACBrECFFormasPagamento
                               read GetFormasPagamentoClass;
    procedure CarregaFormasPagamento ;
    procedure LerTotaisFormaPagamento ;
    function AchaFPGDescricao( Descricao : String;
       BuscaExata : Boolean = False;
       IgnorarCase : Boolean = True;
       IgnorarAcentos : Boolean = False) : TACBrECFFormaPagamento ;
    function AchaFPGIndice( Indice : String ) : TACBrECFFormaPagamento ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '') ;

    { Comprovantes Nao Fiscais (CNF) }
    Property ComprovantesNaoFiscais : TACBrECFComprovantesNaoFiscais
                               read GetComprovantesNaoFiscaisClass;
    procedure CarregaComprovantesNaoFiscais ;
    procedure LerTotaisComprovanteNaoFiscal ;
    function AchaCNFDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True  ) :
       TACBrECFComprovanteNaoFiscal ;
    function AchaCNFIndice( Indice : String ) : TACBrECFComprovanteNaoFiscal ;
    function AchaCNFFormaPagamento( CodFPG : String ) :
       TACBrECFComprovanteNaoFiscal ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ;

    { RelatoriosGerenciais (RG) }
    Property RelatoriosGerenciais : TACBrECFRelatoriosGerenciais
                               read GetRelatoriosGerenciaisClass;
    procedure CarregaRelatoriosGerenciais ;
    procedure LerTotaisRelatoriosGerenciais ;
    function AchaRGDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True  ) :
       TACBrECFRelatorioGerencial ;
    function AchaRGIndice( Indice : String ) : TACBrECFRelatorioGerencial ;
    Procedure ProgramaRelatoriosGerenciais( var Descricao: String;
       Posicao : String = '') ;

    { Unidades de Medida (UMD) }
    Property UnidadesMedida : TACBrECFUnidadesMedida
                               read GetUnidadesMedidaClass;
    procedure CarregaUnidadesMedida ;
    function AchaUMDDescricao( Descricao : String ) : TACBrECFUnidadeMedida ;
    function AchaUMDIndice( Indice : String ) : TACBrECFUnidadeMedida ;
    Procedure ProgramaUnidadeMedida( var Descricao: String) ;

    { ECF - Flags }
    Function EmLinha( lTimeOut : Integer = 1) : Boolean ;
    Property PoucoPapel   : Boolean        read GetPoucoPapelClass ;
    Property Estado       : TACBrECFEstado read GetEstadoClass ;
    Property HorarioVerao : Boolean        read GetHorarioVeraoClass ;
    Property Arredonda    : Boolean        read GetArredondaClass ;
    Property Termica      : Boolean        read GetTermicaClass ;
    Property MFD          : Boolean        read GetMFDClass ;
    Property ParamDescontoISSQN : Boolean  read GetParamDescontoISSQNClass ;
    Property IdentificaConsumidorRodape : Boolean read GetIdentificaConsumidorRodapeClass ;

    Property TipoUltimoDocumento: TACBrECFTipoDocumento read GetTipoUltimoDocumentoClass ;

    { Procedimentos de Cupom Fiscal }
    property Consumidor : TACBrECFConsumidor read GetConsumidorClass ;
    { Grava dados do Consumidor para ser usado na Abertura ou Fechamento do Cupom }
    Procedure IdentificaConsumidor( const CPF_CNPJ : String; const Nome : String = '';
       const Endereco : String = '') ;
    Procedure AbreCupom( const CPF_CNPJ : String = ''; const Nome : String = '';
       const Endereco : String = ''; ModoPreVenda: Boolean = False) ;
    Procedure AbreBilhetePassagem( const Origem, Destino, Linha, Agencia: String;
      DataHora: TDateTime;
      const Poltrona, Plataforma: String; Tipo: TACBrECFTipoBilhete;
      const UFDestino, PassageiroRG, PassageiroNome, PassageiroEnd: String);
    procedure LegendaInmetroProximoItem;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaICMS : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = 'UN'; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ;

    Procedure VendeItemEx( Codigo, Descricao : String; AliquotaICMS : String;
           Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
           Unidade : String = 'UN'; TipoDescontoAcrescimo : String = '%';
           DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1;
           EAN13: String = '';              // Código Barras do Produto (GTIN-13)
           CasasDecimaisQtde: Integer = 0;  // Se 0 assume o valor de DecimaisQtd
           CasasDecimaisValor: Integer = 0; // Se 0 assume o valor de DecimaisPreco
           ArredondaTrunca: Char = 'A';     // Se diferente de 'A' ou 'T' assume o valor de "Arredonda"
           NCM: String = '';                // Código da Nomenclatura Comum do MERCOSUL
           CFOP: String = '';               // Código Fiscal de Operações e Prestações
           InformacaoAdicional: String = '';// Texto Livro, até 500 caracteres
           TotalDosTributos: Double = 0;    // Valor da lei "De olho no Imposto)
           OrigemProduto: Integer = 0;      // 0–Nacional; 1–Estrangeira Import.direta; 2–Estrangeira–Mercado interno

           CST_ICMS: String = '';           // ICMS: Código de Situação Tributária
           ModalidadeBCICMS: Integer = 0;   // ICMS: Modalidade Base de Calculo: 0 – Margem do valor agregado (%)
                                            //                                   1 – Pauta (Valor)
                                            //                                   2 – Preço tabelado máx. (Valor)
                                            //                                   3 – Valor da operação
           PercentualReducaoBCICMS: Double = 0; // ICMS:
           CSOSN: String = '';                  // Simples Nacional: Código de Situação da Operação
           ValorBaseCalculoSN: Double = 0;      // Simples Nacional: Base de Calculo
           ValorICMSRetidoSN: Double = 0;       // Simples Nacional: Valor Retido para ICMS
           AliquotaCalculoCreditoSN: Double = 0;// Simples Nacional:
           ValorCreditoICMSSN: Double = 0;      // Simples Nacional:
           ItemListaServico: String = '';   // Serviço apenas: código do serviço prestado: lista de serviços anexa à Lei Complementar nº 116,
           CodigoISS: String = '';          // Serviço apenas: Código do Imposto Sobre Serviço
           NaturezaOperacaoISS: String = '';// Serviço apenas: com os seguintes valores possíveis: '00' até '08',
           IndicadorIncentivoFiscalISS: Integer = 1;  // Serviço apenas: para indicar se o estado é participante ou não da (Lei do Incentivo Fiscal – ISS), valores: 1 (participante) ou 2 (não participante)
           CodigoIBGE: String = '';         // Serviço apenas: Código do município
           ModalidadeBCICMSST: Integer = 0; // ICMS ST: Modalidade Base de Calculo, 0 – Preço tabelado ou máximo sugerido
                                            //       Substituição Tributária        1 – Lista negativa (valor)
                                            //                                      2 – Lista positiva (valor)
                                            //                                      3 – Lista neutra (valor)
                                            //                                      4 – Margem do valor agregado (%)
                                            //                                      5 – Pauta (valor)
           PercentualMargemICMSST: Double = 0;    // ICMS ST:
           PercentualReducaoBCICMSST: Double = 0; // ICMS ST:
           ValorReducaoBCICMSST: Double = 0;      // ICMS ST:
           AliquotaICMSST: Double = 0;            // ICMS ST:
           ValorICMSST: Double = 0;               // ICMS ST:
           ValorICMSDesonerado: Double = 0;
           MotivoDesoneracaoICMS: Integer = 9;    // 3 – Uso na agropecuária; 9 – Outros; 12 – Órgão de fomento e desenvolvimento agropecuário
           CST_PIS: String = '';
           BaseCalculoPIS: Double = 0;
           AliquotaPIS: Double = 0;
           ValorPIS: Double = 0;
           QuantidadeVendidaPIS: Double = 0;
           ValorAliquotaPIS: Double = 0;
           CST_COFINS: String = '';
           BaseCalculoCOFINS: Double = 0;
           AliquotaCOFINS: Double = 0;
           ValorCOFINS: Double = 0;
           QuantidadeVendidaCOFINS: Double = 0;
           ValorAliquotaCOFINS: Double = 0;
           CEST: String = '');   // Código do CEST para esse produto (7 dígitos)

    Procedure DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo : Double = 0;
       DescontoAcrescimo : String = 'D'; TipoDescontoAcrescimo : String = '%';
       NumItem : Integer = 0 ) ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString = '') ;
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ;
    { Para quebrar linhas nos parametros Observacao use: '|' (pipe),
       #10 ou chr(10).      Geralmente o ECF aceita no máximo 8 linhas }
    procedure EstornaPagamento(CodFormaPagtoEstornar,
      CodFormaPagtoEfetivar : String; Valor: Double;
      Observacao : AnsiString = '') ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer  = 0) ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0) ;
    Procedure CancelaItemVendido( NumItem : Integer ) ;
    procedure CancelaItemVendidoParcial( NumItem : Integer; Quantidade : Double);
    procedure CancelaDescontoAcrescimoItem( NumItem : Integer;
      TipoAcrescimoDesconto: String = 'D');
    procedure CancelaDescontoAcrescimoSubTotal(TipoAcrescimoDesconto: Char); //A -> Acrescimo D -> Desconto

    Property Subtotal  : Double read GetSubTotalClass ;
    Property TotalPago : Double read GetTotalPagoClass ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure NaoFiscalCompleto( CodCNF : String; Valor : Double;
       CodFormaPagto  : String; Obs : AnsiString = ''; IndiceBMP : Integer = 0 ) ;
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ;

    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '' ) ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ;
    Procedure EfetuaPagamentoNaoFiscal( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false) ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ;
    Procedure CancelaNaoFiscal ;
    Procedure CancelaItemNaoFiscal(const AItem: Integer);

    procedure Sangria( Valor: Double; Obs: AnsiString;
       DescricaoCNF: String = 'SANGRIA'; DescricaoFPG: String = 'DINHEIRO';
       IndiceBMP: Integer = 0) ;
    procedure Suprimento( Valor: Double; Obs: AnsiString;
       DescricaoCNF: String = 'SUPRIMENTO'; DescricaoFPG: String = 'DINHEIRO';
       IndiceBMP: Integer = 0) ;

    Function EstornaCCD( const Todos: Boolean = True ) : Integer;

    { Gaveta de dinheiro }
    Procedure AbreGaveta  ;
    Property GavetaAberta : Boolean read GetGavetaAbertaClass ;

    { Relatorios }
    Procedure LeituraX ;
    Procedure LeituraXSerial( Linhas : TStringList ) ; overload ;
    Procedure LeituraXSerial( NomeArquivo : String  ); overload ;
    Procedure ReducaoZ( DataHora : TDateTime = 0 ) ;
    Procedure RelatorioGerencial(Relatorio : TStrings; const Vias : Integer = 1;
      const Indice: Integer = 0) ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ;
    Procedure LinhaRelatorioGerencial( const Linha : AnsiString;
      const IndiceBMP: Integer = 0 ) ;
    Procedure CupomVinculado(COO, CodFormaPagto : String; Valor : Double;
              Relatorio : TStrings; Vias : Integer = 1) ; overload ;
    Procedure CupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double;  Relatorio : TStrings;
       Vias : Integer = 1) ; overload ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto : String;
       Valor : Double) ; overload ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; overload ;
    Procedure LinhaCupomVinculado( const Linha : AnsiString ) ;
    Procedure SegundaViaVinculado;
    procedure ReimpressaoVinculado;
    Procedure FechaRelatorio ;
    Procedure PulaLinhas( const NumLinhas : Integer = 0 ) ;
    Procedure CortaPapel( const CorteParcial : Boolean = false) ;

    { Cheques }
    Procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ;
    Procedure CancelaImpressaoCheque ;
    Function LeituraCMC7 : AnsiString ;
    Property ChequePronto : Boolean read GetChequeProntoClass ;

    { Utilitarios e Diversos }
    Procedure MudaHorarioVerao ; overload ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ;
    Procedure MudaArredondamento( Arredondar : Boolean ) ;
    Procedure PreparaTEF ; { Carrega as Formas, de Pagamento e CNF,
                             verifica por Vinculos, etc Particular de cada ECF }
    Procedure CorrigeEstadoErro( ReducaoZ: Boolean = True ) ; { Verifica o estado da impressora e
                                    tenta deixar em estado Livre }
    Procedure ImpactoAgulhas( NivelForca : Integer = 2);
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; overload ;

    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False) ; overload ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False ) ; overload ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal : Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ; overload ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       NomeArquivo : String; Simplificada : Boolean = False ) ; overload ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal : Integer;
       NomeArquivo : String; Simplificada : Boolean = False ) ; overload ;

    procedure TestaSeE_MFD;
    Procedure LeituraMFDSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ;
    Procedure LeituraMFDSerial( COOInicial, COOFinal : Integer;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ;
    Procedure LeituraMFDSerial( DataInicial, DataFinal : TDateTime;
       NomeArquivo : String; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ;
    Procedure LeituraMFDSerial( COOInicial, COOFinal : Integer;
       NomeArquivo : String; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ;

    Procedure EspelhoMFD_DLL( DataInicial, DataFinal : TDateTime;
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ;
    Procedure EspelhoMFD_DLL( COOInicial, COOFinal : Integer;
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ;
    Procedure ArquivoMFD_DLL( DataInicial, DataFinal : TDateTime;
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD ) ; overload ;
    Procedure ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD; TipoContador: TACBrECFTipoContador = tpcCOO) ; overload ;

    Procedure ArquivoMF_Binario_DLL(NomeArquivo: AnsiString);
    Procedure ArquivoMFD_Binario_DLL(NomeArquivo: AnsiString); overload;
    Procedure ArquivoMFD_Binario_DLL(NomeArquivo: AnsiString; DataInicial, DataFinal: TDateTime); overload;
    Procedure ArquivoMFD_Binario_DLL(NomeArquivo: AnsiString; COOInicial, COOFinal: Integer); overload;

    Procedure IdentificaOperador( Nome : String) ;
    Procedure IdentificaPAF( NomeVersao, MD5 : String) ;
    Function RetornaInfoECF( Registrador : String ) : AnsiString;

    procedure ArredondarPorQtd( var Qtd: Double; const ValorUnitario: Double;
       const Precisao : Integer = -2 ) ;

    Function EnviaComando( cmd : AnsiString): AnsiString; overload;
    { Permitindo mudar o TimeOut padrao }
    Function EnviaComando( cmd : AnsiString; lTimeOut : Integer): AnsiString;
       overload ;

    { Gera erro se nao puder abrir Cupom, informando o motivo }
    Procedure TestaPodeAbrirCupom ;

    { Obs: De/Codifica e Verifica Textos C-->  Codifica D--> Decodifica V--> Verifica }
     //---Informação De/Codificada
     //---No Caso da opção "V" a função ira retornar:
     //--- True  se as informaçõe coicidem com os valores atuais
     //--- False se não coicidem
    function DecodificaTexto(Operacao: Char; Texto: String; var Resposta: String): Boolean;

    function CalculaTotalItem(AValorUnitario: Double; AQtd: Double = 1;
       ADecimais: Integer = 2; FlagAT: Char = ' '): Double;

    {$IFNDEF NOGUI}
     Procedure MemoLeParams ;
     Property MemoItens : Integer read fsMemoItens write fsMemoItens ;
    {$ENDIF}

    procedure PafMF_LX_Impressao;

    procedure PafMF_LMFC_Impressao(const CRZInicial, CRZFinal: Integer); overload;
    procedure PafMF_LMFC_Impressao(const DataInicial, DataFinal: TDateTime); overload;
    procedure PafMF_LMFC_Espelho(const CRZInicial, CRZFinal: Integer;
      const PathArquivo: String); overload;
    procedure PafMF_LMFC_Espelho(const DataInicial, DataFinal: TDateTime;
      const PathArquivo: String); overload;
    procedure PafMF_LMFC_Cotepe1704(const CRZInicial, CRZFinal: Integer;
      const PathArquivo: String); overload;
    procedure PafMF_LMFC_Cotepe1704(const DataInicial, DataFinal: TDateTime;
      const PathArquivo: String); overload;

    procedure PafMF_LMFS_Impressao(const CRZInicial, CRZFinal: Integer); overload;
    procedure PafMF_LMFS_Impressao(const DataInicial, DataFinal: TDateTime); overload;
    procedure PafMF_LMFS_Espelho(const CRZInicial, CRZFinal: Integer;
      const PathArquivo: String); overload;
    procedure PafMF_LMFS_Espelho(const DataInicial, DataFinal: TDateTime;
      const PathArquivo: String); overload;

    procedure PafMF_MFD_Espelho(const COOInicial, COOFinal: Integer;
      const PathArquivo: String); overload;
    procedure PafMF_MFD_Espelho(const DataInicial, DataFinal: TDateTime;
      const PathArquivo: String); overload;
    procedure PafMF_MFD_Cotepe1704(const COOInicial, COOFinal: Integer;
      const PathArquivo: String); overload;
    procedure PafMF_MFD_Cotepe1704(const DataInicial, DataFinal: TDateTime;
      const PathArquivo: String); overload;

    procedure PafMF_RelMeiosPagamento(
      const AFormasPagamento: TACBrECFFormasPagamento;
      const ATituloRelatorio: String = '';
      const AIndiceRelatorio: Integer = 0);

    procedure PafMF_RelDAVEmitidos(const DAVsEmitidos: TACBrECFDAVs;
      const TituloRelatorio: String = '';
      const IndiceRelatorio: Integer = 0);

    procedure PafMF_RelIdentificacaoPafECF(
      IdentificacaoPaf: TACBrECFIdentificacaoPAF = nil;
      const IndiceRelatorio: Integer = 0);

    procedure PafMF_RelParametrosConfiguracao(
      AInfoPafECF: TACBrECFInfoPaf; const AIndiceRelatorio: Integer = 1); overload;

    procedure PafMF_RelParametrosConfiguracao(
      const APerfilRequisitos: String; const AIndiceRelatorio: Integer = 1); overload;

    procedure PafMF_GerarCAT52(const DataInicial, DataFinal: TDateTime;
      const DirArquivos: String);

    procedure PafMF_ArqMF_Binario(const APathArquivo: String; Assinar: Boolean = True);
    procedure PafMF_ArqMFD_Binario(const APathArquivo: String; DataInicial: TDateTime = 0;
      DataFinal: TDateTime = 0; Assinar: Boolean = True);

    procedure GerarNotaGaucha(const DataInicial, DataFinal: TDateTime;
      const PathArquivo: String);

    procedure DoVerificaValorGT ;
    procedure DoAtualizarValorGT ;
    function AssinaArquivoComEAD(Arquivo: String): Boolean;

    procedure ProgramarBitmapPromocional(const AIndice: Integer;
      const APathArquivo: AnsiString;
      const AAlinhamento: TACBrAlinhamento = alCentro);

    function DecodificarTagsFormatacao(AString: AnsiString;
      CodificarPaginaDeCodigo: Boolean =  True): AnsiString;
    procedure TraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString);
    procedure TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString);

    function CodificarPaginaDeCodigoECF(ATexto: String): AnsiString;
    function DecodificarPaginaDeCodigoECF(ATexto: AnsiString): String;

    function MontaDadosReducaoZ: String;

    procedure DAV_Abrir(const AEmissao: TDateTime;
      const ADescrDocumento, ANumero, ASituacao, AVendedor, AObservacao,
      ACNPJCPF, ANomeCliente, AEndereco: String; const ANumFabricacao : string = '';
      const AMarca : string = ''; const AModelo :string = ''; const AAno :string ='' ;
      const APlaca : string = ''; const ARenavam: string = ''; const AIndice: Integer = 0);
    procedure DAV_Fechar(const AObservacao: String; AVlrDesconto : Double = 0; AVlrAcrescimo: Double = 0);
    procedure DAV_RegistrarItem(const ACodigo, ADescricao, AUnid: String;
      const AQuantidade, AVlrUnitario, AVlrDesconto, AVlrAcrescimo: Double;
      const ACancelado: Boolean);

    function GetRodapePaf: String;

    {$IFNDEF NOGUI}
    {$IFNDEF FRAMEWORK}
       {$IFDEF FMX}
       property FormMsgColor : TAlphaColor read  fsFormMsgColor write fsFormMsgColor ;
       property FormMsgColorFont : TAlphaColor read  fsFormMsgColorFont write fsFormMsgColorFont ;
       {$ELSE}
       property FormMsgColor : TColor read  fsFormMsgColor write fsFormMsgColor ;
       {$ENDIF}
       property FormMsgFonte : TFont read  fsFormMsgFont  write SetFormMsgFonte ;
    {$ENDIF}
    {$ENDIF}

  published
     property QuebraLinhaRodape : Boolean read FQuebraLinhaRodape write FQuebraLinhaRodape;
     property Modelo : TACBrECFModelo read fsModelo write SetModelo
                 default ecfNenhum ;
     property Porta : String read GetPorta write SetPorta ;
     property ReTentar : Boolean read GetRetentar write SetRetentar
                 default true;
     property ControlePorta : Boolean read GetControlePorta write SetControlePorta
                 default false ;
     property TimeOut : Integer read GetTimeOut write SetTimeOut
                 default cTimeout ;
     property IntervaloAposComando : Integer read GetIntervaloAposComando
                 write SetIntervaloAposComando default cACBrIntervaloAposComando ;
     property DescricaoGrande : Boolean read GetDescricaoGrande
                 write SetDescricaoGrande default false ;
     property GavetaSinalInvertido : Boolean read fsGavetaSinalInvertido
                 write fsGavetaSinalInvertido default false ;
     property IgnorarTagsFormatacao : Boolean read GetIgnorarTagsFormatacao
                 write SetIgnorarTagsFormatacao default false ;

     property Operador   : String read GetOperador   write SetOperador ;
     property MsgAguarde : String read GetMsgAguarde write SetMsgAguarde ;
     property MsgTrabalhando : String read GetMsgTrabalhando write SetMsgTrabalhando ;
     property ExibeMensagem : Boolean read GetExibeMensagem write SetExibeMensagem
                 default true ;
     property TempoInicioMsg : Integer read  GetTempoInicioMsg
                 write SetTempoInicioMsg default cACBrTempoInicioMsg ;
     property ArredondaPorQtd : Boolean read GetArredondaPorQtd
                 write SetArredondaPorQtd default false ;
     property ArredondaItemMFD : Boolean read GetArredondaItemMFD
                 write SetArredondaItemMFD default false ;
     property BloqueiaMouseTeclado : Boolean read  GetBloqueiaMouseTeclado
                 write SetBloqueiaMouseTeclado default true ;
     property MsgPoucoPapel : Integer read  GetMsgPoucoPapel
                 write SetMsgPoucoPapel  default cACBrMsgPoucoPapel ;
     property MsgRelatorio : String read  GetMsgRelatorio
                 write SetMsgRelatorio ;
     property PausaRelatorio : Integer read  GetPausaRelatorio
                 write SetPausaRelatorio default cACBrPausaRelatorio ;
     property MsgPausaRelatorio : String read  GetMsgPausaRelatorio
                 write SetMsgPausaRelatorio ;
     property LinhasEntreCupons : Integer read  GetLinhasEntreCupons
                 write SetLinhasEntreCupons default cACBrLinhasEntreCupons ;
     property MaxLinhasBuffer : Integer read  GetMaxLinhasBuffer
                 write SetMaxLinhasBuffer default cACBrMaxLinhasBuffer ;
     property PaginaDeCodigo : Word read GetPaginaDeCodigoClass
                 write SetPaginaDeCodigoClass;

     property OnMsgAguarde : TACBrECFMsgAguarde   read  GetOnMsgAguarde
                                                  write SetOnMsgAguarde ;
     property OnAguardandoRespostaChange : TNotifyEvent
        read GetOnAguardandoRespostaChange write SetOnAguardandoRespostaChange ;
     property OnMsgPoucoPapel : TNotifyEvent read  GetOnMsgPoucoPapel
                                             write SetOnMsgPoucoPapel ;
     property OnMsgRetentar : TACBrECFMsgRetentar read  GetOnMsgRetentar
                                                  write SetOnMsgRetentar ;
     property OnErrorSemPapel : TNotifyEvent read GetOnErrorSemPapel
                                            write SetOnErrorSemPapel ;
     property OnChequeEstado: TACBrECFOnChequeEstado read GetOnChequeEstado
                                                     write SetOnChequeEstado;

    property OnAntesAbreCupom : TACBrECFOnAbreCupom
       read FOnAntesAbreCupom write FOnAntesAbreCupom;
    property OnAntesAbreBilhetePassagem: TACBrECFOnAbreBilhetePassagem
       read FOnAntesAbreBilhetePassagem write FOnAntesAbreBilhetePassagem;
    property OnDepoisAbreCupom : TACBrECFOnAbreCupom
       read FOnDepoisAbreCupom write FOnDepoisAbreCupom;
    property OnErrorAbreCupom : TACBrECFEventoOnError
       read FOnErrorAbreCupom write FOnErrorAbreCupom;
    property OnAntesVendeItem : TACBrECFOnVendeItem
       read FOnAntesVendeItem write FOnAntesVendeItem;
    property OnDepoisVendeItem : TACBrECFOnVendeItem
       read FOnDepoisVendeItem write FOnDepoisVendeItem;
    property OnErrorVendeItem : TACBrECFEventoOnError
       read FOnErrorVendeItem write FOnErrorVendeItem;
    property OnAntesSubtotalizaCupom: TACBrECFOnSubtotalizaCupom
       read FOnAntesSubtotalizaCupom write FOnAntesSubtotalizaCupom;
    property OnDepoisSubtotalizaCupom: TACBrECFOnSubtotalizaCupom
       read FOnDepoisSubtotalizaCupom write FOnDepoisSubtotalizaCupom;
    property OnErrorSubtotalizaCupom: TACBrECFEventoOnError
       read FOnErrorSubtotalizaCupom write FOnErrorSubtotalizaCupom;
    property OnAntesEfetuaPagamento : TACBrECFOnEfetuaPagamento
       read FOnAntesEfetuaPagamento write FOnAntesEfetuaPagamento;
    property OnDepoisEfetuaPagamento : TACBrECFOnEfetuaPagamento
       read FOnDepoisEfetuaPagamento write FOnDepoisEfetuaPagamento;
    property OnErrorEfetuaPagamento : TACBrECFEventoOnError
       read FOnErrorEfetuaPagamento write FOnErrorEfetuaPagamento;
    property OnAntesFechaCupom : TACBrECFOnFechaCupom
       read FOnAntesFechaCupom write FOnAntesFechaCupom;
    property OnDepoisFechaCupom : TACBrECFOnFechaCupom
       read FOnDepoisFechaCupom write FOnDepoisFechaCupom;
    property OnErrorFechaCupom : TACBrECFEventoOnError
       read FOnErrorFechaCupom write FOnErrorFechaCupom;
    property OnAntesCancelaCupom : TNotifyEvent
       read FOnAntesCancelaCupom write FOnAntesCancelaCupom;
    property OnDepoisCancelaCupom : TNotifyEvent
       read FOnDepoisCancelaCupom write FOnDepoisCancelaCupom;
    property OnErrorCancelaCupom : TACBrECFEventoOnError
       read FOnErrorCancelaCupom write FOnErrorCancelaCupom;
    property OnAntesCancelaItemVendido : TACBrECFOnCancelaItemVendido
       read FOnAntesCancelaItemVendido write FOnAntesCancelaItemVendido;
    property OnDepoisCancelaItemVendido : TACBrECFOnCancelaItemVendido
       read FOnDepoisCancelaItemVendido write FOnDepoisCancelaItemVendido;
    property OnErrorCancelaItemVendido : TACBrECFEventoOnError
       read FOnErrorCancelaItemVendido write FOnErrorCancelaItemVendido;
    property OnAntesAbreNaoFiscal : TACBrECFOnAbreNaoFiscal
       read FOnAntesAbreNaoFiscal write FOnAntesAbreNaoFiscal;
    property OnDepoisAbreNaoFiscal : TACBrECFOnAbreNaoFiscal
       read FOnDepoisAbreNaoFiscal write FOnDepoisAbreNaoFiscal;
    property OnErrorAbreNaoFiscal : TACBrECFEventoOnError
       read FOnErrorAbreNaoFiscal write FOnErrorAbreNaoFiscal;
    property OnAntesSubtotalizaNaoFiscal : TACBrECFOnSubtotalizaNaoFiscal
       read FOnAntesSubtotalizaNaoFiscal write FOnAntesSubtotalizaNaoFiscal;
    property OnDepoisSubtotalizaNaoFiscal : TACBrECFOnSubtotalizaNaoFiscal
       read FOnDepoisSubtotalizaNaoFiscal write FOnDepoisSubtotalizaNaoFiscal;
    property OnErrorSubtotalizaNaoFiscal : TACBrECFEventoOnError
       read FOnErrorSubtotalizaNaoFiscal write FOnErrorSubtotalizaNaoFiscal;
    property OnAntesEfetuaPagamentoNaoFiscal : TACBrECFOnEfetuaPagamentoNaoFiscal
       read FOnAntesEfetuaPagamentoNaoFiscal write FOnAntesEfetuaPagamentoNaoFiscal;
    property OnDepoisEfetuaPagamentoNaoFiscal : TACBrECFOnEfetuaPagamentoNaoFiscal
       read FOnDepoisEfetuaPagamentoNaoFiscal write FOnDepoisEfetuaPagamentoNaoFiscal;
    property OnErrorEfetuaPagamentoNaoFiscal : TACBrECFEventoOnError
       read FOnErrorEfetuaPagamentoNaoFiscal write FOnErrorEfetuaPagamentoNaoFiscal;
    property OnAntesFechaNaoFiscal : TACBrECFOnFechaNaoFiscal
       read FOnAntesFechaNaoFiscal write FOnAntesFechaNaoFiscal;
    property OnDepoisFechaNaoFiscal : TACBrECFOnFechaNaoFiscal
       read FOnDepoisFechaNaoFiscal write FOnDepoisFechaNaoFiscal;
    property OnErrorFechaNaoFiscal : TACBrECFEventoOnError
       read FOnErrorFechaNaoFiscal write FOnErrorFechaNaoFiscal;
    property OnAntesCancelaNaoFiscal : TNotifyEvent
       read FOnAntesCancelaNaoFiscal write FOnAntesCancelaNaoFiscal;
    property OnDepoisCancelaNaoFiscal : TNotifyEvent
       read FOnDepoisCancelaNaoFiscal write FOnDepoisCancelaNaoFiscal;
    property OnErrorCancelaNaoFiscal : TACBrECFEventoOnError
       read FOnErrorCancelaNaoFiscal write FOnErrorCancelaNaoFiscal;
    property OnAntesCancelaItemNaoFiscal : TACBrECFOnCancelaItemNaoFiscal
       read FOnAntesCancelaItemNaoFiscal write FOnAntesCancelaItemNaoFiscal;
    property OnDepoisCancelaItemNaoFiscal : TACBrECFOnCancelaItemNaoFiscal
       read FOnDepoisCancelaItemNaoFiscal write FOnDepoisCancelaItemNaoFiscal;
    property OnErrorCancelaItemNaoFiscal : TACBrECFEventoOnError
       read FOnErrorCancelaItemNaoFiscal write FOnErrorCancelaItemNaoFiscal;
    property OnAntesSangria : TACBrECFOnSangria
       read FOnAntesSangria write FOnAntesSangria;
    property OnDepoisSangria : TACBrECFOnSangria
       read FOnDepoisSangria write FOnDepoisSangria;
    property OnErrorSangria : TACBrECFEventoOnError
       read FOnErrorSangria write FOnErrorSangria;
    property OnAntesSuprimento : TACBrECFOnSuprimento
       read FOnAntesSuprimento write FOnAntesSuprimento;
    property OnDepoisSuprimento : TACBrECFOnSuprimento
       read FOnDepoisSuprimento write FOnDepoisSuprimento;
    property OnErrorSuprimento : TACBrECFEventoOnError
       read FOnErrorSuprimento write FOnErrorSuprimento;
    property OnAntesLeituraX : TNotifyEvent
       read FOnAntesLeituraX write FOnAntesLeituraX;
    property OnDepoisLeituraX : TNotifyEvent
       read FOnDepoisLeituraX write FOnDepoisLeituraX;
    property OnErrorLeituraX : TACBrECFEventoOnError
       read FOnErrorLeituraX write FOnErrorLeituraX;
    property OnAntesReducaoZ : TNotifyEvent
       read FOnAntesReducaoZ write FOnAntesReducaoZ;
    property OnDepoisReducaoZ : TNotifyEvent
       read FOnDepoisReducaoZ write FOnDepoisReducaoZ;
    property OnErrorReducaoZ : TACBrECFEventoOnError
       read FOnErrorReducaoZ write FOnErrorReducaoZ;
    property OnAntesAbreRelatorioGerencial : TACBrECFOnRelatorioGerencial
       read FOnAntesAbreRelatorioGerencial write FOnAntesAbreRelatorioGerencial;
    property OnDepoisAbreRelatorioGerencial : TACBrECFOnRelatorioGerencial
       read FOnDepoisAbreRelatorioGerencial write FOnDepoisAbreRelatorioGerencial;
    property OnErrorAbreRelatorioGerencial : TACBrECFOnErrorAbreRelatorioGerencial
       read FOnErrorAbreRelatorioGerencial write FOnErrorAbreRelatorioGerencial;
    property OnAntesAbreCupomVinculado : TNotifyEvent
       read FOnAntesAbreCupomVinculado write FOnAntesAbreCupomVinculado;
    property OnDepoisAbreCupomVinculado : TNotifyEvent
       read FOnDepoisAbreCupomVinculado write FOnDepoisAbreCupomVinculado;
    property OnErrorAbreCupomVinculado : TACBrECFEventoOnError
       read FOnErrorAbreCupomVinculado write FOnErrorAbreCupomVinculado;
    property OnAntesFechaRelatorio : TNotifyEvent
       read FOnAntesFechaRelatorio write FOnAntesFechaRelatorio;
    property OnDepoisFechaRelatorio : TNotifyEvent
       read FOnDepoisFechaRelatorio write FOnDepoisFechaRelatorio;
    property OnErrorFechaRelatorio : TACBrECFEventoOnError
       read FOnErrorFechaRelatorio write FOnErrorFechaRelatorio;
    property OnChangeEstado : TACBrECFOnChangeEstado
       read FOnChangeEstado write FOnChangeEstado;

    // eventos para assinatura de arquivos do menu fiscl
    property OnPAFCalcEAD: TACBrEADCalc
      read fsOnPAFCalcEAD write fsOnPAFCalcEAD;
    property OnPAFGetKeyRSA: TACBrEADGetChave
      read fsOnPAFGetKeyRSA write fsOnPAFGetKeyRSA;

     property DecimaisPreco : Integer read GetDecimaisPreco
        write SetDecimaisPreco default 3 ;
     property DecimaisQtd : Integer read GetDecimaisQtd
        write SetDecimaisQtd default 3 ;

    {$IFNDEF NOGUI}
    {$IFNDEF FRAMEWORK}
       property MemoBobina : TMemo    read fsMemoBobina write SetMemoBobina ;
     {$ENDIF}
       property MemoParams : TStrings read fsMemoParams write SetMemoParams ;
       property OnBobinaAdicionaLinhas : TACBrECFBobinaAdicionaLinhas
          read  fsOnBobinaAdicionaLinhas write fsOnBobinaAdicionaLinhas ;
      {$IFDEF FMX}
         property OnCriarFormMsg: TACBrFMXCustomForm read GetOnCriarFormMsg
                     write SetOnCriarFormMsg;
         property OnDrawFormMsg: TACBrECFMsgAguarde read GetOnDrawFormMsg
                     write SetOnDrawFormMsg;
      {$ENDIF}
    {$ENDIF}
     { Instancia do Componente ACBrDevice, será passada para fsECF.create }
     property Device     : TACBrDevice     read fsDevice ;
     property RFD        : TACBrRFD        read fsRFD        write SetRFD ;
     property AAC        : TACBrAAC        read fsAAC        write SetAAC ;
     property EAD        : TACBrEAD        read fsEAD        write SetEAD ;
     property ECFVirtual : TACBrECFVirtual read fsECFVirtual write SetECFVirtual ;

     property ArqLOG : String      read GetArqLOG write SetArqLOG ;
     property OnGravarLog : TACBrGravarLog read GetOnGravarLog write SetOnGravarLog;
     property ConfigBarras: TACBrECFConfigBarras read fsConfigBarras write fsConfigBarras;

     property InfoRodapeCupom: TACBrECFRodape read GetInfoRodapeCupom write SetInfoRodapeCupom;
     property VerificarApenasNumeroSerieNoAAC: Boolean read FVerificarApenasNumeroSerieNoAAC write FVerificarApenasNumeroSerieNoAAC default False;
end ;

Function NomeArqCAT52( const CAT52ID, NumSerie: String; DtMov : TDatetime ) : String ;
Function GetECFComponente( AECFClass: TACBrECFClass ): TACBrECF;

implementation
Uses {$IFDEF COMPILER6_UP} StrUtils {$ELSE}ACBrD5 ,Windows {$ENDIF},
     Math, IniFiles, TypInfo,
     ACBrUtil, ACBrECFBematech, ACBrECFNaoFiscal, ACBrECFDaruma, ACBrECFSchalter,
     ACBrECFMecaf, ACBrECFSweda, ACBrECFDataRegis, ACBrECFUrano, ACBrECFYanco,
     ACBrECFICash, ACBrECFQuattro, ACBrECFFiscNET, ACBrECFEpson, ACBrECFNCR,
     ACBrECFSwedaSTX, ACBrECFEscECF;

function NomeArqCAT52(const CAT52ID, NumSerie: String; DtMov: TDatetime): String;
  function IntToLetra(AInt : Integer): Char ;
  begin
     if AInt < 10 then
        Result := IntToStr(AInt)[1]
     else
        Result := Chr( 55 + Min(AInt,35) ) ; // 55+10=chr(65)=>'A' ; 55+35 => Máximo Z
  end ;
Var
  DtStr : String ;
begin
  DtStr  := FormatDateTime('ddmmyy', DtMov) ;
  Result := PadRight(CAT52ID,3,'1')+
            Poem_Zeros( RightStr(NumSerie,5), 5 )+'.'+
            IntToLetra(StrToInt(copy(DtStr,1,2)))+
            IntToLetra(StrToInt(copy(DtStr,3,2)))+
            IntToLetra(StrToInt(copy(DtStr,5,2))) ;
end;

function GetECFComponente(AECFClass: TACBrECFClass): TACBrECF;
begin
  Result := nil;
  if not Assigned( AECFClass ) then
     exit ;

  if (AECFClass is TACBrECFVirtualClass) then
     Result := TACBrECF(TACBrECFVirtualClass(AECFClass).ECFVirtual.ECF)
  else
     Result := TACBrECF(AECFClass.Owner);
end;

{ TACBrECF }
constructor TACBrECF.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  { Inicializando as Variaveis Internas }
  fsTotalPago       := 0;
  fsSubTotalPagto   := 0;
  fsIndiceGerencial := 0;
  fsAtivo           := false ;
  fsProcurandoECF   := false ;
  fsProcurandoPorta := false ;
  fsMensagemRodape  := '' ;
  fsRegistrouRFDCNF := False ;
  fsIdentificarOperador := True ;
  fsNumSerieCache   := '';
  fsGavetaSinalInvertido  := False;

  FQuebraLinhaRodape := False;
  FDAVItemCount := 0;
  FDAVTotal     := 0.00;

  fsECFVirtual     := nil;
  fsEADInterno     := nil;
  fsEAD            := nil;
  fsAAC            := nil;
  fsRFD            := nil;
  fsOnPAFGetKeyRSA := nil;
  fsOnPAFCalcEAD   := nil;

  { Instanciando SubComponente TACBrDevice }
  fsDevice := TACBrDevice.Create( self ) ;  { O dono é o proprio componente }
  fsDevice.Name := 'ACBrDevice' ;      { Apenas para aparecer no Object Inspector}
  {$IFDEF COMPILER6_UP}
  fsDevice.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
  fsDevice.Porta := 'COM1';

  fsTagProcessor := TACBrTagProcessor.Create;
  fsTagProcessor.AddTags(cTAGS_CARACTER, cTAGS_CARACTER_HELP, False);
  fsTagProcessor.AddTags(cTAGS_LINHAS, cTAGS_LINHAS_HELP, False);
  fsTagProcessor.AddTags(cTAGS_ALINHAMENTO, cTAGS_ALINHAMENTO_HELP, True);
  fsTagProcessor.AddTags(cTAGS_BARRAS, cTAGS_BARRAS_HELP, True);
  fsTagProcessor.OnTraduzirTag := TraduzirTag;
  fsTagProcessor.OnTraduzirTagBloco := TraduzirTagBloco;

  {$IFNDEF NOGUI}
  {$IFNDEF FRAMEWORK}
    fsFormMsgFont  := TFont.create ;
    fsFormMsgColor := {$IF DEFINED(VisualCLX)} clNormalHighlight
                      {$ELSEIF DEFINED(FMX)} TAlphaColors.Dodgerblue
                      {$ELSE} clHighlight {$IFEND};

    fsMemoBobina := nil ;
    {$ENDIF}

    fsMemoItens     := 0 ;
    {$IFDEF MSWINDOWS}
    fsMemoHTMLFont  := '<font size="2" face="Lucida Console">' ;
    {$ELSE}
    fsMemoHTMLFont  := '<font size="5" face="Fixed">' ;
    {$ENDIF}
    fsMemoCabecalho := '' ;
    fsMemoRodape    := '' ;
    fsMemoCabItens  := '' ;
    fsMemoColunas   := 48 ;
    fsMemoHTML      := True ;
    fsMemoHTMLTitleSise:= 2 ;
                       {....+....1....+....2....+....3....+....4....+....5
                        ITEM   CODIGO      DESCRICAO
                        QTD         x UNITARIO       Aliq     VALOR (R$) }
    fsMemoMascItens := 'III CCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDD'+
                       'QQQQQQQQ UU x VVVVVVVVVVVVV AAAAAA TTTTTTTTTTTTT' ;

    fsMemoParams := TStringList.Create ;
    fsMemoParams.Add( '[Cabecalho]' ) ;
    fsMemoParams.Add( 'LIN000=<center><b>Nome da Empresa</b></center>' ) ;
    fsMemoParams.Add( 'LIN001=<center>Nome da Rua , 1234  -  Bairro</center>' );
    fsMemoParams.Add( 'LIN002=<center>Cidade  -  UF  -  99999-999</center>' );
    fsMemoParams.Add( 'LIN003=<center>CNPJ: 01.234.567/0001-22    IE: 012.345.678.90</center>' );
    fsMemoParams.Add( 'LIN004=<table width=100%><tr><td align=left><code>Data</code> <code>Hora</code></td><td align=right>COO: <b><code>NumCupom</code></b></td></tr></table>' );
    fsMemoParams.Add( 'LIN005=<hr>' ) ;
    fsMemoParams.Add( ' ' ) ;
    fsMemoParams.Add( '[Cabecalho_Item]' );
    fsMemoParams.Add( 'LIN000=ITEM   CODIGO      DESCRICAO' ) ;
    fsMemoParams.Add( 'LIN001=QTD         x UNITARIO       Aliq     VALOR (R$)' );
    fsMemoParams.Add( 'LIN002=<hr>' ) ;
    fsMemoParams.Add( 'MascaraItem='+fsMemoMascItens );
    fsMemoParams.Add( ' ' ) ;
    fsMemoParams.Add( '[Rodape]' ) ;
    fsMemoParams.Add( 'LIN000=<hr>' ) ;
    fsMemoParams.Add( 'LIN001=<table width=100%><tr><td align=left><code>Data</code> <code>Hora</code></td><td align=right>Projeto ACBr: <b><code>ACBR</code></b></td></tr></table>' );
    fsMemoParams.Add( 'LIN002=<center>Obrigado Volte Sempre</center>' ) ;
    fsMemoParams.Add( 'LIN003=<hr>' ) ;
    fsMemoParams.Add( ' ' ) ;
    fsMemoParams.Add( '[Formato]' );
    fsMemoParams.Add( 'Colunas=48' ) ;
    fsMemoParams.Add( 'HTML=1' ) ;
    fsMemoParams.Add( 'HTML_Title_Size=2' ) ;
    fsMemoParams.Add( 'HTML_Font='+fsMemoHTMLFont ) ;
  {$ENDIF}

  { Instanciando fsECF com modelo Generico (ECFClass) }
  fsECF := TACBrECFClass.create( self ) ;

  // configurações de codigos de barras impressos por tags
  fsConfigBarras := TACBrECFConfigBarras.Create;
  fsConfigBarras.LarguraLinha := 0;
  fsConfigBarras.Altura := 0;
  fsConfigBarras.MostrarCodigo := True;

  FVerificarApenasNumeroSerieNoAAC := False;

end;

destructor TACBrECF.Destroy;
begin
  Ativo := false ;

  if Assigned( fsECF ) then
     if not (fsECF is TACBrECFVirtualClass) then
        FreeAndNil( fsECF ) ;

  if Assigned( fsEADInterno ) then
     FreeAndNil( fsEADInterno );

  FreeAndNil( fsDevice ) ;
  FreeAndNil( fsTagProcessor );

  {$IFNDEF NOGUI}
    {$IFNDEF FRAMEWORK}
      fsFormMsgFont.Free ;
    {$ENDIF}
    fsMemoParams.Free ;
  {$ENDIF}

  FreeAndNil(fsConfigBarras);

  inherited Destroy;
end;

procedure TACBrECF.SetModelo(const AValue: TACBrECFModelo);
var wRetentar : Boolean ;   { Variaveis de Trabalho, usadas para transportar }
    wTimeOut  : Integer ;   { as informações de uma Classe ECF antiga para a }
    wMsgAguarde : String ;  { do novo modelo que será instanciada }
    wMsgTrabalhando : String ;
    wControlePorta: Boolean;
    wOperador   : String ;
    wMsgRelatorio : String ;
    wPausaRelatorio : Integer ;
    wLinhasEntreCupons : Integer ;
    wMaxLinhasBuffer : Integer ;
    wArredondaPorQtd : Boolean ;
    wArredondaItemMFD : Boolean ;
    wDecimaisPreco : Integer ;
    wDecimaisQtd : Integer ;
    wArqLOG : String ;
    wExibeMensagem   : Boolean ;
    wTempoInicioMsg : Integer ;
    wBloqueiaMouseTeclado : Boolean ;
    wMsgPoucoPapel : Integer ;
    wOnMsgAguarde : TACBrECFMsgAguarde ;
    wOnMsgPoucoPapel : TNotifyEvent ;
    wOnMsgRetentar : TACBrECFMsgRetentar ;
    wOnAguardandoRespostaChange : TNotifyEvent ;
    wDescricaoGrande : Boolean ;
    wIntervaloAposComando : Integer ;
    wIgnorarTagsFormatacao: Boolean;
    wInfoRodapeCupomMania: Boolean;
    wInfoRodapeMinasLegal: Boolean;
    wInfoRodapeParaibaLegal: Boolean;
    wInfoRodapeMD5: String;
    wInfoRodapeNotaLegalDFImprimir: Boolean;
    wInfoRodapeNotaLegalDFProgramaDeCredito: Boolean;
    wInfoRodapeRestauranteImprimir: Boolean;
    wOnGravarLog: TACBrGravarLog;
begin
  if fsModelo = AValue then exit ;

  if fsAtivo then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFSetModeloException));

  if (AValue = ecfECFVirtual) and (not Assigned( fsECFVirtual) ) then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFSemECFVirtualException));

  wRetentar             := ReTentar ;
  wControlePorta        := ControlePorta;
  wTimeOut              := TimeOut ;
  wIntervaloAposComando := IntervaloAposComando ;
  wBloqueiaMouseTeclado := BloqueiaMouseTeclado ;
  wMsgAguarde           := MsgAguarde ;
  wMsgTrabalhando       := MsgTrabalhando ;
  wOperador             := Operador ;
  wMsgRelatorio         := MsgRelatorio ;
  wPausaRelatorio       := PausaRelatorio ;
  wLinhasEntreCupons    := LinhasEntreCupons ;
  wMaxLinhasBuffer      := MaxLinhasBuffer ;
  wArredondaPorQtd      := ArredondaPorQtd ;
  wArredondaItemMFD     := ArredondaItemMFD ;
  wDecimaisPreco        := DecimaisPreco ;
  wArqLOG               := ArqLOG ;
  wDecimaisQtd          := DecimaisQtd ;
  wExibeMensagem        := ExibeMensagem ;
  wTempoInicioMsg       := TempoInicioMsg ;
  wMsgPoucoPapel        := MsgPoucoPapel ;
  wOnMsgAguarde         := OnMsgAguarde ;
  wOnMsgPoucoPapel      := OnMsgPoucoPapel ;
  wOnMsgRetentar        := OnMsgRetentar ;
  wDescricaoGrande      := DescricaoGrande ;
  wOnAguardandoRespostaChange := OnAguardandoRespostaChange ;
  wOnGravarLog                := OnGravarLog;
  wIgnorarTagsFormatacao:= IgnorarTagsFormatacao;

  wInfoRodapeCupomMania   := InfoRodapeCupom.CupomMania ;
  wInfoRodapeMinasLegal   := InfoRodapeCupom.MinasLegal ;
  wInfoRodapeParaibaLegal := InfoRodapeCupom.ParaibaLegal ;
  wInfoRodapeMD5          := InfoRodapeCupom.MD5;
  wInfoRodapeNotaLegalDFImprimir := InfoRodapeCupom.NotaLegalDF.Imprimir;
  wInfoRodapeNotaLegalDFProgramaDeCredito := InfoRodapeCupom.NotaLegalDF.ProgramaDeCredito;
  wInfoRodapeRestauranteImprimir := InfoRodapeCupom.Restaurante.Imprimir;

  if Assigned( fsECF ) then
     if not (fsECF is TACBrECFVirtualClass) then
        FreeAndNil( fsECF ) ;

  { Instanciando uma nova classe de acordo com fsModelo }
  case AValue of
    ecfBematech  : fsECF := TACBrECFBematech.create( Self ) ;
    ecfDaruma    : fsECF := TACBrECFDaruma.create( Self ) ;
    ecfSchalter  : fsECF := TACBrECFSchalter.create( Self ) ;
    ecfMecaf     : fsECF := TACBrECFMecaf.create( Self ) ;
    ecfSweda     : fsECF := TACBrECFSweda.Create( self );
    ecfYanco     : fsECF := TACBrECFYanco.create( Self ) ;
    ecfDataRegis : fsECF := TACBrECFDataRegis.create( Self ) ;
    ecfUrano     : fsECF := TACBrECFUrano.create( Self ) ;
    ecfNaoFiscal : fsECF := TACBrECFNaoFiscal.create( Self ) ;
    ecfICash     : fsECF := TACBrECFICash.create( Self ) ;
    ecfQuattro   : fsECF := TACBrECFQuattro.create( Self ) ;
    ecfFiscNET   : fsECF := TACBrECFFiscNET.create( Self ) ;
    ecfEpson     : fsECF := TACBrECFEpson.create( Self ) ;
    ecfNCR       : fsECF := TACBrECFNCR.create( Self ) ;
    ecfSwedaSTX  : fsECF := TACBrECFSwedaSTX.Create( self );
    ecfEscECF    : fsECF := TACBrECFEscECF.Create( self );
    ecfECFVirtual: fsECF := fsECFVirtual.ECFVirtualClass;
  else
    fsECF := TACBrECFClass.create( Self ) ;
  end;

  { Passando propriedades da Classe anterior para a Nova Classe }
  Retentar             := wRetentar ;
  ControlePorta        := wControlePorta;
  TimeOut              := wTimeOut ;
  IntervaloAposComando := wIntervaloAposComando ;
  TempoInicioMsg       := wTempoInicioMsg ;
  Operador             := wOperador ;
  MsgAguarde           := wMsgAguarde ;
  MsgTrabalhando       := wMsgTrabalhando ;
  MsgRelatorio         := wMsgRelatorio ;
  PausaRelatorio       := wPausaRelatorio ;
  LinhasEntreCupons    := wLinhasEntreCupons ;
  MaxLinhasBuffer      := wMaxLinhasBuffer ;
  ArredondaPorQtd      := wArredondaPorQtd ;
  ArredondaItemMFD     := wArredondaItemMFD ;
  DecimaisPreco        := wDecimaisPreco ;
  ArqLOG               := wArqLOG ;
  DecimaisQtd          := wDecimaisQtd ;
  ExibeMensagem        := wExibeMensagem ;
  BloqueiaMouseTeclado := wBloqueiaMouseTeclado ;
  MsgPoucoPapel        := wMsgPoucoPapel ;
  OnMsgAguarde         := wOnMsgAguarde ;
  OnMsgPoucoPapel      := wOnMsgPoucoPapel ;
  OnMsgRetentar        := wOnMsgRetentar ;
  OnAguardandoRespostaChange := wOnAguardandoRespostaChange ;
  OnGravarLog                := wOnGravarLog;
  DescricaoGrande      := wDescricaoGrande ;
  IgnorarTagsFormatacao:= wIgnorarTagsFormatacao;

  InfoRodapeCupom.CupomMania           := wInfoRodapeCupomMania;
  InfoRodapeCupom.MinasLegal           := wInfoRodapeMinasLegal;
  InfoRodapeCupom.ParaibaLegal         := wInfoRodapeParaibaLegal;
  InfoRodapeCupom.MD5                  := wInfoRodapeMD5;
  InfoRodapeCupom.NotaLegalDF.Imprimir := wInfoRodapeNotaLegalDFImprimir;
  InfoRodapeCupom.NotaLegalDF.ProgramaDeCredito := wInfoRodapeNotaLegalDFProgramaDeCredito;
  InfoRodapeCupom.Restaurante.Imprimir := wInfoRodapeRestauranteImprimir;

  if (fsECF is TACBrECFVirtualClass) then
     TACBrECFVirtualClass(fsECF).Device := fsDevice;

  fsModelo := AValue;
end;

procedure TACBrECF.SetAtivo(const AValue: Boolean);
begin
  if AValue then
     Ativar
  else
     Desativar ;
end;

procedure TACBrECF.SetIgnorarErroSemPapel(AValue : Boolean) ;
begin
  fsECF.IgnorarErroSemPapel := AValue;
end;

procedure TACBrECF.SetIgnorarTagsFormatacao(AValue: Boolean);
begin
   fsECF.IgnorarTagsFormatacao := AValue;
end;

procedure TACBrECF.SetOnGravarLog(AValue: TACBrGravarLog);
begin
  fsECF.OnGravarLog := AValue;
end;

procedure TACBrECF.SetOnErrorSemPapel(AValue : TNotifyEvent) ;
begin
  fsECF.OnErrorSemPapel := AValue;
end;

procedure TACBrECF.SetPaginaDeCodigoClass(const AValue : Word) ;
begin
   fsECF.PaginaDeCodigo := AValue;
end;

procedure TACBrECF.Ativar;
begin
  if fsAtivo then exit ;

  if fsModelo = ecfNenhum then
     raise EACBrECFErro.Create( ACBrStr('Modelo não definido'));

  if ((Porta = '') or (LowerCase(Porta) = 'procurar')) then
  begin
     if (fsECF is TACBrECFVirtualClass) then
        raise EACBrECFErro.Create( ACBrStr('Porta não definida'))
     else
        AcharPorta ;
  end;

  fsECF.Ativar ;
  fsAtivo := true ;
  fsNumSerieCache := '';

  //Se o ecf foi desligado durante o cupom, continua do ultimo
  if (Estado in [estVenda, estPagamento]) then
  begin
     {$IFNDEF NOGUI}
     fsMemoItens     := NumUltItem;
     {$ENDIF}
     fsSubTotalPagto := Subtotal;
     fsTotalPago     := TotalPago;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      MemoLeParams ;
   end ;
  {$ENDIF}

  if Assigned( fsRFD ) then
  begin
     if ( not (Modelo in [ecfNaoFiscal,ecfECFVirtual])) and
        ( not (MFD and fsRFD.IgnoraEcfMfd)) then
     begin
        try
           fsRFD.Ativar ;
        except
           Desativar ;
           raise ;
        end ;
     end ;
  end ;

  {
    // removido porque no paf-ecf tem que permitir
    // ativar o ECF mesmo não encontrando o AACend;
  if Assigned( fsAAC ) then
  begin
     if (Modelo <> ecfNaoFiscal) then
     begin
        try
          fsAAC.AbrirArquivo ;
        except
          Desativar ;
          raise ;
        end ;
     end ;
  end ;
  }

  if fsIdentificarOperador then
  begin
     try
        IdentificaOperador(Operador);
     except
     end ;
  end ;
end;

procedure TACBrECF.Desativar;
begin
  if not fsAtivo then exit ;

  ComandoLOG := DateToStr(now)+ ' Desativar' ;

  fsECF.Desativar ;
  fsAtivo := false;

  if RFDAtivo then
     fsRFD.Desativar ;
end;

function TACBrECF.GetPorta: String;
begin
  result := fsDevice.Porta ;
end;

procedure TACBrECF.SetPorta(const AValue: String);
begin
  fsDevice.Porta := AValue ;
end;

function TACBrECF.GetRetentar: Boolean;
begin
  result := fsECF.Retentar ;
end;

procedure TACBrECF.SetRetentar(const AValue: Boolean);
begin
   fsECF.Retentar := AValue ;
end;

function TACBrECF.GetControlePorta: Boolean;
begin
  result := fsECF.ControlePorta ;
end;

procedure TACBrECF.SetControlePorta(AValue: Boolean);
begin
  fsECF.ControlePorta := AValue ;
end;

function TACBrECF.GetTimeOut: Integer;
begin
  Result := fsDevice.TimeOut ;
end;

procedure TACBrECF.SetTimeOut(const AValue: Integer);
begin
   fsDevice.TimeOut := AValue ;
end;

function TACBrECF.GetInfoRodapeCupom: TACBrECFRodape;
begin
  Result := fsECF.InfoRodapeCupom;
end;

function TACBrECF.GetIntervaloAposComando: Integer;
begin
  Result := fsECF.IntervaloAposComando ;
end;

procedure TACBrECF.SetInfoRodapeCupom(const Value: TACBrECFRodape);
begin
  fsECF.InfoRodapeCupom := Value;
end;

procedure TACBrECF.SetIntervaloAposComando(const AValue: Integer);
begin
  fsECF.IntervaloAposComando := AValue ;
end;

function TACBrECF.GetOperador: String;
begin
  Result := fsECF.Operador ;
end;

procedure TACBrECF.SetOperador(const AValue: String);
begin
  if Operador = AValue then exit ;

  fsECF.Operador        := AValue ;
  fsIdentificarOperador := True ;
end;

function TACBrECF.GetMsgAguarde: String;
begin
  Result := fsECF.MsgAguarde ;
end;

procedure TACBrECF.SetMsgAguarde(const AValue: String);
begin
  fsECF.MsgAguarde := AValue ;
end;

function TACBrECF.GetMsgTrabalhando: String;
begin
  Result := fsECF.MsgTrabalhando
end;

procedure TACBrECF.SetMsgTrabalhando(const AValue: String);
begin
  fsECF.MsgTrabalhando := AValue ;
end;

function TACBrECF.GetMsgRelatorio: String;
begin
  result := fsECF.MsgRelatorio ;
end;

procedure TACBrECF.SetMsgRelatorio(const AValue: String);
begin
  fsECF.MsgRelatorio := AValue ;
end;

function TACBrECF.GetPausaRelatorio: Integer;
begin
  result := fsECF.PausaRelatorio ;
end;

procedure TACBrECF.SetPausaRelatorio(const AValue: Integer);
begin
  fsECF.PausaRelatorio := AValue ;
end;

function TACBrECF.GetLinhasEntreCupons: Integer;
begin
  result := fsECF.LinhasEntreCupons ;
end;

procedure TACBrECF.SetLinhasEntreCupons(const AValue: Integer);
begin
  fsECF.LinhasEntreCupons := AValue ;
end;

function TACBrECF.GetMaxLinhasBuffer: Integer;
begin
  result := fsECF.MaxLinhasBuffer ;
end;

procedure TACBrECF.SetMaxLinhasBuffer(const AValue: Integer);
begin
  fsECF.MaxLinhasBuffer := AValue ;
end;

 function TACBrECF.MemoAssigned: Boolean;
 begin
   {$IFDEF NOGUI}
     Result := False;
   {$ELSE}
     Result := {$IFNDEF FRAMEWORK}Assigned( fsMemoBobina ) or {$ENDIF}Assigned( fsOnBobinaAdicionaLinhas ) ;
   {$ENDIF}
 end;

function TACBrECF.GetMsgPausaRelatorio: String;
begin
  result := fsECF.MsgPausaRelatorio ;
end;

procedure TACBrECF.SetMsgPausaRelatorio(const AValue: String);
begin
  fsECF.MsgPausaRelatorio := AValue ;
end;

function TACBrECF.GetTempoInicioMsg: Integer;
begin
  Result := fsECF.TempoInicioMsg ;
end;

procedure TACBrECF.SetTempoInicioMsg(const AValue: Integer);
begin
  if AValue > TimeOut then
     fsECF.TempoInicioMsg := TimeOut
  else
     fsECF.TempoInicioMsg := AValue ;
end;

function TACBrECF.GetArredondaPorQtd: Boolean;
begin
  Result := fsECF.ArredondaPorQtd ;
end;

procedure TACBrECF.SetArredondaPorQtd(const AValue: Boolean);
begin
  fsECF.ArredondaPorQtd := AValue ;
end;

function TACBrECF.GetArredondaItemMFD : Boolean ;
begin
  Result := fsECF.ArredondaItemMFD;
end;

function TACBrECF.GetDataHoraUltimaReducaoZClass : TDateTime ;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'DataHoraUltimaReducaoZ' ;
  Result := fsECF.DataHoraUltimaReducaoZ ;
end;

function TACBrECF.GetIgnorarErroSemPapel : Boolean ;
begin
  Result := fsECF.IgnorarErroSemPapel;
end;

function TACBrECF.GetIgnorarTagsFormatacao: Boolean;
begin
  Result := fsECF.IgnorarTagsFormatacao;
end;

function TACBrECF.GetNumMaxLinhasRodapeClass: Integer;
begin
  Result := fsECF.NumMaxLinhasRodape ;
end;

function TACBrECF.GetOnGravarLog: TACBrGravarLog;
begin
  Result := fsECF.OnGravarLog;
end;

function TACBrECF.GetOnErrorSemPapel : TNotifyEvent ;
begin
  Result := fsECF.OnErrorSemPapel;
end;

function TACBrECF.GetPaginaDeCodigoClass : Word ;
begin
   Result := fsECF.PaginaDeCodigo;
end;

function TACBrECF.GetTipoUltimoDocumentoClass : TACBrECFTipoDocumento ;
begin
  if ComandoLOG = '' then
    ComandoLOG := 'TipoUltimoDocumento' ;
  Result := fsECF.TipoUltimoDocumento ;
end;

function TACBrECF.GetTotalizadoresNaoTributadosClass: TACBrECFTotalizadoresNaoTributados;
begin
  ComandoLOG := 'TotalizadoresNaoTributados' ;
  Result := fsECF.TotalizadoresNaoTributados ;
end;

procedure TACBrECF.SetArredondaItemMFD(const AValue : Boolean) ;
begin
  fsECF.ArredondaItemMFD := AValue;
end;

function TACBrECF.GetDecimaisPreco: Integer;
begin
  Result := fsECF.DecimaisPreco ;
end;

procedure TACBrECF.SetDecimaisPreco(const AValue: Integer);
begin
  if (AValue < 0) or (AValue > 3) then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFSetDecimaisPrecoException));

  fsECF.DecimaisPreco := AValue ;
end;

function TACBrECF.GetDecimaisQtd: Integer;
begin
  Result := fsECF.DecimaisQtd ;
end;

procedure TACBrECF.SetDecimaisQtd(const AValue: Integer);
begin
  if (AValue < 0) or (AValue > 4) then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFSetDecimaisQtdException));

  fsECF.DecimaisQtd := AValue ;
end;

function TACBrECF.GetArqLOG: String;
begin
  Result := fsECF.ArqLOG ;
end;

procedure TACBrECF.SetArqLOG(const AValue: String);
begin
  fsECF.ArqLOG := AValue ;
end;

function TACBrECF.GetAguardaImpressao: Boolean;
begin
  Result := fsECF.AguardaImpressao ;
end;

procedure TACBrECF.SetAguardaImpressao(const AValue: Boolean);
begin
  fsECF.AguardaImpressao := AValue ;
end;

function TACBrECF.GetExibeMensagem: Boolean;
begin
  Result := fsECF.ExibeMensagem ;
end;

procedure TACBrECF.SetExibeMensagem(const AValue: Boolean);
begin
  fsECF.ExibeMensagem := AValue;
end;

function TACBrECF.GetBloqueiaMouseTeclado: Boolean;
begin
  Result := fsECf.BloqueiaMouseTeclado ;
end;

procedure TACBrECF.SetBloqueiaMouseTeclado(const AValue: Boolean);
begin
  fsECF.BloqueiaMouseTeclado := AValue;
end;

function TACBrECF.GetMsgPoucoPapel: Integer;
begin
  Result := fsECf.MsgPoucoPapel ;
end;

procedure TACBrECF.SetMsgPoucoPapel(const AValue: Integer);
begin
  fsECF.MsgPoucoPapel := AValue;
end;

function TACBrECF.GetParamDescontoISSQNClass: Boolean;
begin
  ComandoLOG := 'ParamDescontoISSQNClass' ;
  Result := fsECF.ParamDescontoISSQN ;
end;

function TACBrECF.GetDescricaoGrande: Boolean;
begin
  Result := fsECF.DescricaoGrande ;
end;

procedure TACBrECF.SetDescricaoGrande(const AValue: Boolean);
begin
  fsECF.DescricaoGrande := AValue ;
end;

{$IFNDEF NOGUI}
{$IFNDEF FRAMEWORK}
  procedure TACBrECF.SetFormMsgFonte(const AValue: TFont);
  begin
    fsFormMsgFont.Assign( AValue ) ;
  end;
{$ENDIF}
{$ENDIF}

function TACBrECF.GetOnMsgAguarde: TACBrECFMsgAguarde;
begin
  Result := fsECF.OnMsgAguarde ;
end;

procedure TACBrECF.SetOnMsgAguarde(const AValue: TACBrECFMsgAguarde);
begin
  fsECF.OnMsgAguarde := AValue ;
end;

function TACBrECF.GetOnMsgPoucoPapel: TNotifyEvent;
begin
  Result := fsECF.OnMsgPoucoPapel ;
end;

procedure TACBrECF.SetOnMsgPoucoPapel(const AValue: TNotifyEvent);
begin
  fsECF.OnMsgPoucoPapel := AValue ;
end;

function TACBrECF.GetOnMsgRetentar: TACBrECFMsgRetentar;
begin
  Result := fsECF.OnMsgRetentar ;
end;

procedure TACBrECF.SetOnMsgRetentar(const AValue: TACBrECFMsgRetentar);
begin
  fsECF.OnMsgRetentar := AValue ;
end;

procedure TACBrECF.SetOnAguardandoRespostaChange( const AValue: TNotifyEvent);
begin
  fsECF.OnAguardandoRespostaChange := AValue ;
end;

procedure TACBrECF.SetOnChequeEstado(const Value: TACBrECFOnChequeEstado);
begin
  fsECF.OnChequeEstado := Value
end;

{$IFNDEF NOGUI}
  {$IFDEF FMX}
  function TACBrECF.GetOnCriarFormMsg: TACBrFMXCustomForm;
  begin
    Result := fsECF.OnCriarFormMsg
  end;

  function TACBrECF.GetOnDrawFormMsg: TACBrECFMsgAguarde;
  begin
    Result := fsECF.OnDrawFormMsg
  end;

  procedure TACBrECF.SetOnCriarFormMsg(const Value: TACBrFMXCustomForm);
  begin
    fsECF.OnCriarFormMsg := Value;
  end;

  procedure TACBrECF.SetOnDrawFormMsg(const Value: TACBrECFMsgAguarde);
  begin
    fsECF.OnDrawFormMsg := Value;
  end;
  {$ENDIF}
{$ENDIF}

function TACBrECF.GetOnAguardandoRespostaChange: TNotifyEvent;
begin
  Result := fsECF.OnAguardandoRespostaChange ;
end;

function TACBrECF.GetOnChequeEstado: TACBrECFOnChequeEstado;
begin
  Result := fsECF.OnChequeEstado;
end;

function TACBrECF.TestarDialog: Boolean;
var wAtivo : Boolean ;
    wNumSerie, wNumECF, wStrDate : String ;
    WDataHora : TDateTime ;
    Msg : String ;
begin
  wAtivo := Ativo ;

  try
     if not Ativo then
        Ativo := true ;

     wNumSerie := NumSerie ;
     wNumECF   := NumECF ;
     WDataHora := DataHora ;
     DateTimeToString(wStrDate,'dd/mm/yyyy hh:nn:ss ', wDataHora) ;

     Msg := 'Impressora: '+fsECF.ModeloStr + sLineBreak +
            'Versão: '+NumVersao + sLineBreak +
            'Colunas: '+IntToStr(Colunas)+ sLineBreak + sLineBreak +
            'Numero de Serie: '+wNumSerie+ sLineBreak +
            'Numero do ECF: '+wNumECF+ sLineBreak +
            'Data / Hora: '+wStrDate ;

     Msg := ACBrStr(Msg);
     {$IFNDEF NOGUI}
     {$IFNDEF FRAMEWORK}
       {$IFDEF FMX}
        MessageDlg(Msg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK],0);
       {$ELSE}
        MessageDlg(Msg ,mtInformation ,[mbOk],0) ;
       {$ENDIF}
     {$ELSE}
       writeln( Msg ) ;
     {$ENDIF}
     {$ELSE}
       writeln( Msg ) ;
     {$ENDIF}
  finally
     Result := Ativo ;
     Ativo := wAtivo ;
  end ;
end;

function TACBrECF.GetAguardandoRespostaClass: Boolean;
begin
  Result := fsECF.AguardandoResposta ;
end;

function TACBrECF.GetColunasClass: Integer;
begin
  Result := fsECF.Colunas ;
end;

function TACBrECF.GetComandoEnviadoClass: AnsiString;
begin
  Result := fsECF.ComandoEnviado ;
end;

function TACBrECF.GetRespostaComandoClass: AnsiString;
begin
  Result := fsECF.RespostaComando ;
end;

function TACBrECF.GetRespostasComandoClass: TACBrInformacoes;
begin
  Result := fsECF.RespostasComando ;
end;

function TACBrECF.GetComandoLOGClass: AnsiString;
begin
  Result := fsECF.ComandoLOG ;
end;

procedure TACBrECF.SetComandoLOGClass(const AValue: AnsiString);
begin
  fsECF.ComandoLOG      := AValue ;
  fsECF.ComandoEnviado  := '' ;
  fsECF.RespostaComando := '' ;
end;

function TACBrECF.GetModeloStrClass: String;
begin
  Result := ACBrStr( fsECF.ModeloStr );
end;

function TACBrECF.GetRFDIDClass: String;
begin
  Result := fsECF.RFDID ;
end;

function TACBrECF.GetDataHoraClass: TDateTime;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'DataHora' ;
  Result := fsECF.DataHora ;
end;

function TACBrECF.GetNumCupomClass: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'NumCupom' ;
  Result := fsECF.NumCupom ;
end;

function TACBrECF.GetNumECFClass: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'NumECF' ;
  Result := fsECF.NumECF ;
end;

function TACBrECF.GetNumCROClass: String;
begin
  ComandoLOG := 'NumCRO' ;
   Result := fsECF.NumCRO ;
end;

function TACBrECF.GetNumCCFClass: String;
begin
  ComandoLOG := 'NumCCF' ;
  Result := fsECF.NumCCF ;
end;

function TACBrECF.GetNumGNFCClass: String;
begin
  ComandoLOG := 'NumGNFC' ;
  Result := fsECF.NumGNFC ;
end;

function TACBrECF.GetNumGNFClass: String;
begin
  ComandoLOG := 'NumGNF' ;
  Result := fsECF.NumGNF ;
end;

function TACBrECF.GetNumGRGClass: String;
begin
  ComandoLOG := 'NumGRG' ;
  Result := fsECF.NumGRG ;
end;

function TACBrECF.GetNumCDCClass: String;
begin
  ComandoLOG := 'NumCDC' ;
  Result := fsECF.NumCDC ;
end;

function TACBrECF.GetNumCFCClass: String;
begin
  ComandoLOG := 'NumCFC' ;
  Result := fsECF.NumCFC ;
end;

function TACBrECF.GetNumCFDClass: String;
begin
  ComandoLOG := 'NumCFD' ;
  Result := fsECF.NumCFD ;
end;

function TACBrECF.GetNumNCNClass: String;
begin
  ComandoLOG := 'NumNCN' ;
  Result := fsECF.NumNCN ;
end;

function TACBrECF.GetNumCCDCClass: String;
begin
  ComandoLOG := 'NumCCDC' ;
  Result := fsECF.NumCCDC ;
end;

function TACBrECF.GetNumLojaClass: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'NumLoja' ;
  Result := fsECF.NumLoja ;
end;

function TACBrECF.GetNumSerieClass: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'NumSerie' ;
  Result := fsECF.NumSerie ;
end;

function TACBrECF.GetNumSerieMFDClass: String;
begin
  ComandoLOG := 'NumSerieMFD' ;
  Result := fsECF.NumSerieMFD ;
end;

function TACBrECF.GetNumVersaoClass: String;
begin
  ComandoLOG := 'NumVersao' ;
  Result := fsECF.NumVersao ;
end;

function TACBrECF.GetNumReducoesZRestantesClass: String;
begin
  ComandoLOG := 'NumReducoesZRestantes' ;
  Result := fsECF.NumReducoesZRestantes ;
end;

function TACBrECF.EmLinha(lTimeOut: Integer): Boolean;
begin
  Result := fsECF.EmLinha(lTimeOut) ;
end;

function TACBrECF.GetEstadoClass: TACBrECFEstado;
var
  wIgnorarErroSemPapel : Boolean;
begin
  Result := estDesconhecido;
  wIgnorarErroSemPapel := IgnorarErroSemPapel;
  try
    ComandoLOG := 'Estado' ;
    IgnorarErroSemPapel := True;
    Result := fsECF.Estado ;

    if Result <> fpUltimoEstadoObtido then
    begin
       try
          if Assigned( FOnChangeEstado ) then
             FOnChangeEstado( fpUltimoEstadoObtido, Result);
       finally
          fpUltimoEstadoObtido := Result;
       end;
    end ;
  finally
     fsECF.GravaLog('  '+GetEnumName(TypeInfo(TACBrECFEstado), Integer(Result)));
     IgnorarErroSemPapel := wIgnorarErroSemPapel;
  end;
end;

function TACBrECF.GetPoucoPapelClass: Boolean;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'PoucoPapel' ;
  Result := fsECF.PoucoPapel ;
end;

function TACBrECF.GetArredondaClass: Boolean;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'Arredonda' ;
  Result := fsECF.Arredonda or (fsECF.MFD and fsECF.ArredondaItemMFD) ;
end;

function TACBrECF.GetHorarioVeraoClass: Boolean;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'HorarioVerao' ;
  Result := fsECF.HorarioVerao ;
end;

function TACBrECF.GetMFAdicional: String;
var
  Letra: Char;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'MF Adicional' ;

  Letra := PadRight(RightStr(fsECF.NumSerie, 1),1)[1];
  if CharIsAlpha(Letra) then
    Result := Letra
  else
    Result := '';
end;

function TACBrECF.GetMFDClass: Boolean;
begin
  Result := fsECF.MFD ;
end;

function TACBrECF.GetTermicaClass: Boolean;
begin
  Result := fsECF.Termica ;
end;

function TACBrECF.MontaDadosReducaoZ: String;
begin
  Result := fsECF.DadosReducaoZClass.MontaDadosReducaoZ;
end;

function TACBrECF.GetIdentificaConsumidorRodapeClass: Boolean;
begin
  Result := fsECF.IdentificaConsumidorRodape ;
end;


function TACBrECF.GetCNPJClass: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'CNPJ' ;
  Result := Trim(fsECF.CNPJ) ;
end;

function TACBrECF.GetIEClass: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'IE' ;
  Result := Trim(fsECF.IE) ;
end;

function TACBrECF.GetIMClass: String;
begin
  ComandoLOG := 'IM' ;
  Result := Trim(fsECF.IM) ;
end;
function TACBrECF.GetClicheClass: AnsiString;
begin
  ComandoLOG := 'Cliche' ;
  Result := StringReplace(fsECF.Cliche,#0,'',[rfReplaceAll]) ;  // remove eventuais #0
  Result := DecodificarPaginaDeCodigoECF( Result );
end;

function TACBrECF.GetUsuarioAtualClass: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'UsuarioAtual' ;
  Result := fsECF.UsuarioAtual ;
end;

function TACBrECF.GetDataHoraSBClass: TDateTime;
begin
  ComandoLOG := 'DataHoraSB' ;
  Result := fsECF.DataHoraSB ;
end;

function TACBrECF.GetSubModeloECFClass: String;
begin
  ComandoLOG := 'SubModeloECF' ;
  Result := fsECF.SubModeloECF ;
end;

function TACBrECF.GetPAFClass: String;
begin
  ComandoLOG := 'PAF' ;
  Result := fsECF.PAF ;
end;

function TACBrECF.GetDataMovimentoClass: TDateTime;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'DataMovimento' ;
  Result := fsECF.DataMovimento ;
end;

function TACBrECF.GetGrandeTotalClass: Double;
var
  wIgnorarErroSemPapel : Boolean;
begin
  wIgnorarErroSemPapel := IgnorarErroSemPapel;
  try
    IgnorarErroSemPapel := True;
    if ComandoLOG = '' then
       ComandoLOG := 'GrandeTotal' ;
    Result := RoundTo( fsECF.GrandeTotal, -2) ;
  finally
    IgnorarErroSemPapel := wIgnorarErroSemPapel;
  end;
end;

function TACBrECF.GetNumCOOInicialClass: String;
begin
  ComandoLOG := 'NumCOOInicial' ;
  Result := fsECF.NumCOOInicial ;
end;

function TACBrECF.GetNumCRZClass: String;
begin
  ComandoLOG := 'NumCRZ' ;
  Result := fsECF.NumCRZ ;
end;

function TACBrECF.GetTotalAcrescimosClass: Double;
begin
  ComandoLOG := 'TotalAcrescimos' ;
  Result := RoundTo( fsECF.TotalAcrescimos, -2) ;
end;

function TACBrECF.GetTotalCancelamentosClass: Double;
begin
  ComandoLOG := 'TotalCancelamentos' ;
  Result := RoundTo( fsECF.TotalCancelamentos, -2) ;
end;

function TACBrECF.GetTotalCancelamentosEmAbertoClass: Double;
begin
  ComandoLOG := 'TotalCancelamentosEmAberto' ;
  Result := RoundTo( fsECF.TotalCancelamentosEmAberto, -2) ;
end;

function TACBrECF.GetTotalDescontosClass: Double;
begin
  ComandoLOG := 'TotalDescontos' ;
  Result := RoundTo( fsECF.TotalDescontos, -2) ;
end;

function TACBrECF.GetTotalTrocoClass: Double;
begin
  ComandoLOG := 'TotalTroco' ;
  Result := RoundTo( fsECF.TotalTroco, -2) ;
end;

function TACBrECF.GetTotalSubstituicaoTributariaClass: Double;
begin
  ComandoLOG := 'TotalSubstituicaoTributaria' ;
  Result := RoundTo( fsECF.TotalSubstituicaoTributaria, -2) ;
end;

function TACBrECF.GetTotalIsencaoClass: Double;
begin
  ComandoLOG := 'TotalIsencao' ;
  Result := RoundTo( fsECF.TotalIsencao, -2) ;
end;

function TACBrECF.GetTotalNaoFiscalClass: Double;
begin
  ComandoLOG := 'TotalNaoFiscal' ;
  Result := RoundTo( fsECF.TotalNaoFiscal, -2) ;
end;

function TACBrECF.GetTotalNaoTributadoClass: Double;
begin
  ComandoLOG := 'TotalNaoTributado' ;
  Result := RoundTo( fsECF.TotalNaoTributado, -2) ;
end;

function TACBrECF.GetTotalAcrescimosISSQNClass: Double;
begin
  ComandoLOG := 'TotalAcrescimosISSQN';
  Result := RoundTo( fsECF.TotalAcrescimosISSQN, -2);
end;

function TACBrECF.GetTotalCancelamentosISSQNClass: Double;
begin
  ComandoLOG := 'TotalCancelamentosISSQN';
  Result := RoundTo( fsECF.TotalCancelamentosISSQN, -2);
end;

function TACBrECF.GetTotalDescontosISSQNClass: Double;
begin
  ComandoLOG := 'TotalDescontosISSQN';
  Result := RoundTo( fsECF.TotalDescontosISSQN, -2);
end;

function TACBrECF.GetTotalAcrescimosOPNFClass: Double;
begin
  ComandoLOG := 'TotalAcrescimosOPNF';
  Result := RoundTo( fsECF.TotalAcrescimosOPNF, -2);
end;

function TACBrECF.GetTotalCancelamentosOPNFClass: Double;
begin
  ComandoLOG := 'TotalCancelamentosOPNF';
  Result := RoundTo( fsECF.TotalCancelamentosOPNF, -2);
end;

function TACBrECF.GetTotalDescontosOPNFClass: Double;
begin
  ComandoLOG := 'TotalDescontosOPNF';
  Result := RoundTo( fsECF.TotalDescontosOPNF, -2);
end;

function TACBrECF.GetTotalSubstituicaoTributariaISSQNClass: Double;
begin
  ComandoLOG := 'TotalSubstituicaoTributariaISSQN';
  Result := RoundTo( fsECF.TotalSubstituicaoTributariaISSQN, -2);
end;

function TACBrECF.GetTotalIsencaoISSQNClass: Double;
begin
  ComandoLOG := 'TotalIsencaoISSQN';
  Result := RoundTo( fsECF.TotalIsencaoISSQN, -2);
end;

function TACBrECF.GetTotalNaoTributadoISSQNClass: Double;
begin
  ComandoLOG := 'TotalNaoTributadoISSQN';
  Result := RoundTo( fsECF.TotalNaoTributadoISSQN, -2);
end;

function TACBrECF.GetVendaBrutaClass: Double;
begin
  ComandoLOG := 'VendaBruta' ;
  Result := RoundTo( fsECF.VendaBruta, -2) ;
end;

function TACBrECF.GetNumUltimoItemClass: Integer;
begin
  ComandoLOG := 'NumUltimoItem' ;
  Result := fsECF.NumUltItem ;
end;

function TACBrECF.GetDadosReducaoZ: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'DadosReducaoZ' ;
  Result := fsECF.DadosReducaoZ;
end;

function TACBrECF.GetDadosReducaoZClass: TACBrECFDadosRZ;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'DadosReducaoZClass' ;
   Result := fsECF.DadosReducaoZClass;
end;

function TACBrECF.GetDadosUltimaReducaoZ: String;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'DadosUltimaReducaoZ' ;
  Result := fsECF.DadosUltimaReducaoZ ;
end ;


procedure TACBrECF.AbreBilhetePassagem(const Origem, Destino, Linha, Agencia: String;
  DataHora: TDateTime; const Poltrona, Plataforma: String; Tipo: TACBrECFTipoBilhete;
  const UFDestino, PassageiroRG, PassageiroNome, PassageiroEnd: String);
var
  Tratado   : Boolean;
begin
  ComandoLOG := 'AbreBilhetePassagem( ' +
    Origem + ', ' +
    Destino + ', ' +
    Linha + ', ' +
    Agencia + ', ' +
    FormatDateTime('dd/nn/yyyy hh:mm:ss', DataHora) + ', ' +
    Poltrona + ', ' +
    Plataforma + ', ' +
    IntToStr(Integer(Tipo)) + ', ' +
    UFDestino + ', ' +
    PassageiroRG + ', ' +
    PassageiroNome + ', ' +
    PassageiroEnd + ' )' ;

  fsNumSerieCache := '' ;
  DoVerificaValorGT ;

  if Assigned( fOnAntesAbreBilhetePassagem ) then
  begin
    fOnAntesAbreBilhetePassagem(
      Origem, Destino, Linha, Agencia,
      DataHora, Poltrona, Plataforma, Tipo, UFDestino,
      PassageiroRG, PassageiroNome, PassageiroEnd
    );
  end;

  try
    Tratado := False;
    fsECF.AbreBilhetePassagem(
      Origem, Destino, Linha, Agencia,
      DataHora, Poltrona, Plataforma, Tipo, UFDestino,
      PassageiroRG, PassageiroNome, PassageiroEnd
    );
  except
     if Assigned( FOnErrorAbreCupom ) then
        FOnErrorAbreCupom(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'abrebilhetepassagem' ;
      MemoAdicionaCabecalho ;

      if fsModelo = ecfNaoFiscal then
         MemoTitulo('CUPOM NAO FISCAL')
      else
         MemoTitulo('CUPOM FISCAL');

      MemoAdicionaLinha( fsMemoCabItens );
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.AbreCupom ;

  if Assigned( FOnDepoisAbreCupom ) then
     FOnDepoisAbreCupom(PassageiroRG, PassageiroNome, PassageiroEnd);
end;

procedure TACBrECF.AbreCupom(const CPF_CNPJ: String = ''; const Nome : String = '';
   const Endereco : String = ''; ModoPreVenda: Boolean = False) ;
var
  Tratado   : Boolean;
begin
  ComandoLOG := 'AbreCupom( '+CPF_CNPJ+', '+Nome+', '+Endereco+' )' ;

  if RFDAtivo then
     fsRFD.VerificaParametros ;

  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  if Assigned( fOnAntesAbreCupom ) then
     fOnAntesAbreCupom( CPF_CNPJ, Nome, Endereco);

  if Trim(CPF_CNPJ) <> '' then
     IdentificaConsumidor(CPF_CNPJ, Nome, Endereco);

  if fsIdentificarOperador then
  begin
     try
        IdentificaOperador(Operador);
     except
     end ;
  end ;

  try
    Tratado := False;
    fsECF.ModoPreVenda := ModoPreVenda;
    fsECF.AbreCupom ;
  except
     if Assigned( FOnErrorAbreCupom ) then
        FOnErrorAbreCupom(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'abrecupom' ;
      MemoAdicionaCabecalho ;

      if Trim(CPF_CNPJ) <> '' then
      begin
         MemoAdicionaLinha(LeftStr('CPF/CNPJ Consumidor: '+CPF_CNPJ, fsMemoColunas)) ;
         MemoAdicionaLinha( '<hr>' ) ;
      end ;

      if fsModelo = ecfNaoFiscal then
         MemoTitulo('CUPOM NAO FISCAL')
      else
         MemoTitulo('CUPOM FISCAL');

      MemoAdicionaLinha( fsMemoCabItens );
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.AbreCupom ;

  if Assigned( FOnDepoisAbreCupom ) then
     FOnDepoisAbreCupom(CPF_CNPJ, Nome, Endereco);
end;

procedure TACBrECF.IdentificaConsumidor(const CPF_CNPJ : String ; const Nome : String ;
  const Endereco : String) ;
begin
  fsECF.Consumidor.AtribuiConsumidor( CPF_CNPJ,
                                      CodificarPaginaDeCodigoECF( Nome ),
                                      CodificarPaginaDeCodigoECF( Endereco ));
end;

function TACBrECF.GetConsumidorClass: TACBrECFConsumidor;
begin
  Result := fsECF.Consumidor ;
end;

procedure TACBrECF.CancelaCupom(NumCOOCancelar: Integer);
  Var Docto     : String ;
      OldEstado : TACBrECFEstado ;
      SubTot    : Double ;
      Tratado   : Boolean;
begin
  ComandoLOG := 'CancelaCupom' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  if RFDAtivo then
     fsRFD.VerificaParametros ;

  OldEstado := estDesconhecido ;
  SubTot    := 0 ;
  Docto     := '' ;

  if RFDAtivo {$IFNDEF NOGUI}or MemoAssigned {$ENDIF} then
  begin
     OldEstado := Estado ;
     SubTot    := Subtotal ;
     Docto     := IntToStrZero( StrToInt(NumCupom) ,6) ;
  end ;

  if Assigned( fOnAntesCancelaCupom ) then
     fOnAntesCancelaCupom(Self);

  try
    Tratado := False;
    fsECF.CancelaCupom( NumCOOCancelar ) ;
  except
     if Assigned( FOnErrorCancelaCupom ) then
        FOnErrorCancelaCupom(Tratado);

     if not Tratado then
        raise;
  end;

  DoAtualizarValorGT;

  if RFDAtivo then
     fsRFD.CancelaCupom( StrToInt(Docto) ) ;

  fsMensagemRodape := '';
  Consumidor.Zera;
  InfoRodapeCupom.Clear;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'cancelacupom' ;

      if OldEstado in [estVenda, estPagamento] then
       begin
          MemoTitulo('*** CUPOM CANCELADO ***');

          if OldEstado = estVenda then
             MemoAdicionaLinha( '<table width=100%><tr>'+
              '<td align=left>TOTAL R$</td>'+
              '<td align=right><b>'+FormatFloatBr(SubTot,'###,##0.00')+'</b></td>'+
              '</tr></table>') ;

           MemoAdicionaLinha( fsMemoRodape );
       end
      else
       begin
          MemoAdicionaCabecalho ;
          MemoTitulo('*** CUPOM CANCELADO ***');
          MemoAdicionaLinha( '<table width=100%><tr>'+
              '<td align=left>COO do Cupom Cancelado:</td>'+
              '<td align=right><b>'+Docto+'</b></td>'+
              '</tr></table>' + sLineBreak + sLineBreak +
              '<table width=100%><tr>'+
              '<td align=left>Valor da Operacao  R$:</td>'+
              '<td align=right><b>'+FormatFloatBr(SubTot,'#,###,##0.00')+'</b></td>'+
              '</tr></table>' + sLineBreak + sLineBreak +
              fsMemoRodape ) ;
       end ;
   end ;
  {$ENDIF}

  if Assigned( fOnDepoisCancelaCupom ) then
     FOnDepoisCancelaCupom(Self);

end;

procedure TACBrECF.TestaPodeAbrirCupom;
begin
  ComandoLOG := 'TestaPodeAbrirCupom' ;
  fsECF.TestaPodeAbrirCupom ;
end;

{ Insere Legenda do Imetro para o Proximo Item a ser vendido }
procedure TACBrECF.LegendaInmetroProximoItem;
begin
  ComandoLOG := 'LegendaInmetroProximoItem' ;
  fsECF.LegendaInmetroProximoItem ;
end;

{ Vende o Item }
procedure TACBrECF.VendeItem(Codigo, Descricao: String; AliquotaICMS : String ;
  Qtd: Double; ValorUnitario: Double; ValorDescontoAcrescimo: Double;
  Unidade: String; TipoDescontoAcrescimo : String; DescontoAcrescimo : String ;
  CodDepartamento: Integer);
Var
  AliquotaECF : String ;
  Tratado     : Boolean;
begin
  AliquotaECF := '';
  IniciaVendeItem(Codigo, Descricao, AliquotaICMS, AliquotaECF, Qtd, ValorUnitario,
    ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo, DescontoAcrescimo,
    CodDepartamento);

  try
     Tratado := False;
     fsECF.VendeItem( Codigo, CodificarPaginaDeCodigoECF( Descricao ),
                      AliquotaECF, Qtd, ValorUnitario,
                      ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo,
                      DescontoAcrescimo, CodDepartamento );
  except
     if Assigned( FOnErrorVendeItem ) then
        FOnErrorVendeItem(Tratado);

     if not Tratado then
        raise;
  end;

  FinalizaVendeItem(Codigo, Descricao, AliquotaICMS, AliquotaECF, Qtd, ValorUnitario,
    ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo, DescontoAcrescimo);
end;

procedure TACBrECF.VendeItemEx(Codigo, Descricao: String; AliquotaICMS: String;
  Qtd: Double; ValorUnitario: Double; ValorDescontoAcrescimo: Double;
  Unidade: String; TipoDescontoAcrescimo: String; DescontoAcrescimo: String;
  CodDepartamento: Integer; EAN13: String; CasasDecimaisQtde: Integer;
  CasasDecimaisValor: Integer; ArredondaTrunca: Char; NCM: String;
  CFOP: String; InformacaoAdicional: String; TotalDosTributos: Double;
  OrigemProduto: Integer; CST_ICMS: String; ModalidadeBCICMS: Integer;
  PercentualReducaoBCICMS: Double; CSOSN: String; ValorBaseCalculoSN: Double;
  ValorICMSRetidoSN: Double; AliquotaCalculoCreditoSN: Double;
  ValorCreditoICMSSN: Double; ItemListaServico: String; CodigoISS: String;
  NaturezaOperacaoISS: String; IndicadorIncentivoFiscalISS: Integer;
  CodigoIBGE: String; ModalidadeBCICMSST: Integer;
  PercentualMargemICMSST: Double; PercentualReducaoBCICMSST: Double;
  ValorReducaoBCICMSST: Double; AliquotaICMSST: Double; ValorICMSST: Double;
  ValorICMSDesonerado: Double; MotivoDesoneracaoICMS: Integer; CST_PIS: String;
  BaseCalculoPIS: Double; AliquotaPIS: Double; ValorPIS: Double;
  QuantidadeVendidaPIS: Double; ValorAliquotaPIS: Double; CST_COFINS: String;
  BaseCalculoCOFINS: Double; AliquotaCOFINS: Double; ValorCOFINS: Double;
  QuantidadeVendidaCOFINS: Double; ValorAliquotaCOFINS: Double; CEST: String);
Var
  AliquotaECF : String ;
  Tratado     : Boolean;
begin
  AliquotaECF := '';
  IniciaVendeItem(Codigo, Descricao, AliquotaICMS, AliquotaECF, Qtd, ValorUnitario,
    ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo, DescontoAcrescimo,
    CodDepartamento);

  if CasasDecimaisQtde = 0 then
     CasasDecimaisQtde := Self.DecimaisQtd;

  if CasasDecimaisValor = 0 then
     CasasDecimaisValor := Self.DecimaisPreco;

  if not CharInSet(ArredondaTrunca , ['A','T']) then
     ArredondaTrunca := IfThen(Self.Arredonda,'A','T')[1];

  try
     Tratado := False;
     fsECF.VendeItemEx( Codigo, CodificarPaginaDeCodigoECF( Descricao ),
                      AliquotaECF, Qtd, ValorUnitario,
                      ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo,
                      DescontoAcrescimo, CodDepartamento,
                      EAN13, CasasDecimaisQtde, CasasDecimaisValor,
                      ArredondaTrunca, NCM, CFOP, InformacaoAdicional, TotalDosTributos,
                      OrigemProduto, CST_ICMS, ModalidadeBCICMS, PercentualReducaoBCICMS,
                      CSOSN, ValorBaseCalculoSN, ValorICMSRetidoSN,
                      AliquotaCalculoCreditoSN, ValorCreditoICMSSN,
                      ItemListaServico, CodigoISS, NaturezaOperacaoISS,
                      IndicadorIncentivoFiscalISS, CodigoIBGE,
                      ModalidadeBCICMSST, PercentualMargemICMSST, PercentualReducaoBCICMSST,
                      ValorReducaoBCICMSST, AliquotaICMSST, ValorICMSST,
                      ValorICMSDesonerado, MotivoDesoneracaoICMS,
                      CST_PIS, BaseCalculoPIS, AliquotaPIS, ValorPIS,
                      QuantidadeVendidaPIS, ValorAliquotaPIS, CST_COFINS,
                      BaseCalculoCOFINS, AliquotaCOFINS,
                      ValorCOFINS, QuantidadeVendidaCOFINS, ValorAliquotaCOFINS,
                      CEST);
  except
     if Assigned( FOnErrorVendeItem ) then
        FOnErrorVendeItem(Tratado);

     if not Tratado then
        raise;
  end;

  FinalizaVendeItem(Codigo, Descricao, AliquotaICMS, AliquotaECF, Qtd, ValorUnitario,
    ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo, DescontoAcrescimo);
end;


procedure TACBrECF.IniciaVendeItem(var Codigo, Descricao, AliquotaICMS,
  AliquotaECF: String; var Qtd, ValorUnitario, ValorDescontoAcrescimo: Double;
  var Unidade, TipoDescontoAcrescimo, DescontoAcrescimo: String;
  var CodDepartamento: Integer);
begin
  AliquotaICMS      := UpperCase( Trim(AliquotaICMS) ) ;
  DescontoAcrescimo := UpperCase( Trim(DescontoAcrescimo) ) ;
  if DescontoAcrescimo = '' then
     DescontoAcrescimo := 'D' ;

  if TipoDescontoAcrescimo = '' then
     TipoDescontoAcrescimo := '%' ;

  { Usando unidade Default "UN" para evitar problemas em RFD e alguns ECFs que
    não permitem a Unidade vazia }
  if Trim( Unidade ) = '' then
     Unidade := 'UN' ;

  Qtd           := RoundTo( Qtd, -DecimaisQtd) ;
  ValorUnitario := RoundTo( ValorUnitario, -DecimaisPreco) ;

  ComandoLOG := 'VendeItem( '+Codigo+' , '+Descricao+' , '+
                     AliquotaICMS+' , '+FloatToStr(Qtd)+' , '+
                     FloatToStr(ValorUnitario)+' , '+
                     FloatToStr(ValorDescontoAcrescimo)+' , '+Unidade+' , '+
                     TipoDescontoAcrescimo+' , '+DescontoAcrescimo+
                     ' , '+IntToStr(CodDepartamento)+' )';

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  if Qtd <= 0 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemQtdeException) );

  if ValorUnitario <= 0 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemValorUnitException) );

  if ValorDescontoAcrescimo < 0 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemValDescAcreException) );

  if AliquotaICMS = '' then
     raise EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemAliqICMSException) );

  if not CharInSet(DescontoAcrescimo[1] , ['A','D']) then
     raise EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemDescAcreException) );

  if not CharInSet(TipoDescontoAcrescimo[1] , ['%','$']) then
     raise  EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemTipoDescAcreException) );

  { Retorna em "AliquotaECF" (por referencia) a String de aliquota que deve
    ser enviada para o ECF }
  AliquotaECF := AliquotaICMS ;

  AchaICMSAliquota( AliquotaECF ) ;  // modifica AliquotaECF por referencia;

  { Verificando se precisa Arredondar por Qtd }
  if ArredondaPorQtd and (not Arredonda) then
     ArredondarPorQtd( Qtd, ValorUnitario );

  if Assigned( fOnAntesVendeItem ) then
     fOnAntesVendeItem( Codigo, Descricao, AliquotaECF, Qtd, ValorUnitario,
                     ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo,
                     DescontoAcrescimo);
end;

procedure TACBrECF.FinalizaVendeItem(Codigo, Descricao, AliquotaICMS,
  AliquotaECF: String; Qtd: Double; ValorUnitario,
  ValorDescontoAcrescimo: Double; Unidade, TipoDescontoAcrescimo,
  DescontoAcrescimo: String);
Var
  Aliquota : TACBrECFAliquota ;
{$IFNDEF NOGUI}
  Linha, Buffer, StrQtd, StrPreco, StrDescAcre : String ;
  Total, PorcDesc, ValDesc : Double ;
{$ENDIF}
begin
  DoAtualizarValorGT;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'vendeitem' ;

     Inc( fsMemoItens ) ;

     if Qtd = Round( Qtd ) then
        StrQtd := FormatFloat('#######0',Qtd )
     else
        StrQtd := FormatFloatBr(Qtd,'###0.000') ;

     if RoundTo( ValorUnitario, -2 ) = ValorUnitario then
        StrPreco := FormatFloatBr(ValorUnitario,'###,##0.00')
     else
        StrPreco := FormatFloatBr(ValorUnitario,'##,##0.000') ;

     if self.Arredonda then
        Total := RoundABNT( Qtd * ValorUnitario, -2)
     else
        Total := TruncFix( Qtd * ValorUnitario * 100 ) / 100 ;

     { Inserindo na String da fsMascaraItem }
     Linha := fsMemoMascItens ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'I', IntToStrZero(fsMemoItens,3)) ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'C', Codigo ) ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'D', Descricao ) ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'Q', StrQtd, True ) ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'U', Unidade ) ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'V', StrPreco ) ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'A', AliquotaICMS ) ;
     Linha := StuffMascaraItem( Linha, fsMemoMascItens, 'T', FormatFloatBr(Total,'###,##0.00'), True) ;

     { Quebrando a linha em várias de acordo com o tamanho das colunas }
     Buffer := '' ;
     while Linha <> '' do
     begin
        Buffer := Buffer + copy(Linha,1,fsMemoColunas) + sLineBreak ;
        Linha  := copy(Linha,fsMemoColunas + 1,Length(Linha)) ;
     end ;

     MemoAdicionaLinha( Buffer );

     if ValorDescontoAcrescimo > 0 then
     begin
        if TipoDescontoAcrescimo = '%' then
         begin
           PorcDesc := -ValorDescontoAcrescimo ;
           if self.Arredonda then
              ValDesc  := -RoundTo(Total * (ValorDescontoAcrescimo / 100), -2)
           else
              ValDesc  := TruncFix(Total * ValorDescontoAcrescimo) / -100 ;
         end
        else
         begin
           PorcDesc := -RoundTo( (ValorDescontoAcrescimo / Total) * 100, -2) ;
           ValDesc  := -ValorDescontoAcrescimo ;
         end ;

        StrDescAcre := 'DESCONTO' ;
        if DescontoAcrescimo <> 'D' then  // default, DescontoAcrescimo = 'D'
        begin
           ValDesc     := -ValDesc ;
           PorcDesc    := -PorcDesc ;
           StrDescAcre := 'ACRESCIMO' ;
        end ;

        Total := RoundTo(Total + ValDesc, -2) ;

        MemoAdicionaLinha( '<table width=100%><tr>'+
           '<td align=left>'+StrDescAcre+' '+FormatFloatBr(PorcDesc,'#0.00')+'%</td>'+
           '<td align=center>'+FormatFloatBr(ValDesc,'##,##0.00')+'</td>'+
           '<td align=right>'+FormatFloatBr(Total,'###,##0.00')+'</td></tr></table>') ;
     end ;
   end ;
  {$ENDIF}

  if Assigned( FOnDepoisVendeItem ) then
     FOnDepoisVendeItem( Codigo, Descricao, AliquotaICMS, Qtd, ValorUnitario,
                         ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo,
                         DescontoAcrescimo);

  if RFDAtivo then
  begin
     case AliquotaICMS[1] of
       'I' : AliquotaICMS := 'I1' ;
       'N' : AliquotaICMS := 'N1' ;
       'F' : AliquotaICMS := 'F1' ;
     else
        Aliquota := AchaICMSAliquota( AliquotaICMS ) ;

        if Aliquota <> nil then
           AliquotaICMS := IntToStrZero(Aliquota.Sequencia,2) + Aliquota.Tipo +
                           IntToStrZero(Round(Aliquota.Aliquota*100),4)
        else
           AliquotaICMS := '' ;
     end ;

     if ValorDescontoAcrescimo > 0 then
     begin
        { RFD aceita apenas desconto em Valor, convertendo... }
        if TipoDescontoAcrescimo = '%' then
           ValorDescontoAcrescimo := RoundTo( RoundABNT(ValorUnitario*Qtd,-2) *
                                              ValorDescontoAcrescimo / 100  , -2 ) ;
        { RFD considera Descontos, valores negativos, Acrescimos positivos }
        if DescontoAcrescimo = 'D' then
           ValorDescontoAcrescimo := -ValorDescontoAcrescimo ;
     end ;

     fsRFD.VendeItem( Codigo, Descricao, Qtd, ValorUnitario, Unidade,
                      ValorDescontoAcrescimo, AliquotaICMS ) ;

  end ;
end;

procedure TACBrECF.DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo: Double;
  DescontoAcrescimo: String; TipoDescontoAcrescimo : String; NumItem : Integer);
{$IFNDEF NOGUI}
Var
  StrDescAcre : String ;
{$ENDIF}
begin
  DescontoAcrescimo := UpperCase(DescontoAcrescimo) ;
  if DescontoAcrescimo = '' then
     DescontoAcrescimo := 'D' ;

  if TipoDescontoAcrescimo = '' then
     TipoDescontoAcrescimo := '%' ;

  ComandoLOG := 'DescontoAcrescimoItemAnterior( '+
                     FloatToStr(ValorDescontoAcrescimo)+' , '+
                     DescontoAcrescimo+' , '+TipoDescontoAcrescimo+' , '+
                     IntToStr(NumItem)+' )';

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  if ValorDescontoAcrescimo <= 0 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemValDescAcreException) );

  if not CharInSet(DescontoAcrescimo[1] , ['A','D']) then
     raise  EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemDescAcreException) );

  if not CharInSet(TipoDescontoAcrescimo[1] , ['%','$']) then
     raise  EACBrECFCMDInvalido.Create( ACBrStr(cACBrECFVendeItemTipoDescAcreException) );

  fsECF.DescontoAcrescimoItemAnterior(ValorDescontoAcrescimo, DescontoAcrescimo,
     TipoDescontoAcrescimo, NumItem );

  if DescontoAcrescimo <> 'D' then
     DoAtualizarValorGT ;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'descontoacrescimoitemanterior' ;

      StrDescAcre := 'DESCONTO' ;
      if DescontoAcrescimo <> 'D' then  // default, DescontoAcrescimo = 'D'
         StrDescAcre := 'ACRESCIMO' ;

      StrDescAcre := StrDescAcre + ' ' +
                     FormatFloatBr(ValorDescontoAcrescimo,'##,##0.00') ;

      MemoAdicionaLinha( StrDescAcre ) ;
   end ;
  {$ENDIF}

  { TODO: inserir Desconto no RFD }
end;


{ Cancela o Acrescimo ou o Desconto do Item informado }
procedure TACBrECF.CancelaDescontoAcrescimoItem(NumItem: Integer;
  TipoAcrescimoDesconto: String);
begin
  ComandoLOG := 'CancelaDescontoAcrescimoItem' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  fsECF.CancelaDescontoAcrescimoItem(NumItem, TipoAcrescimoDesconto) ;
end;

procedure TACBrECF.CancelaImpressaoCheque;
begin
  ComandoLOG := 'CancelaImpressaoCheque' ;
  fsECF.CancelaImpressaoCheque ;
end;

procedure TACBrECF.CancelaItemVendido(NumItem: Integer);
var
  Tratado : Boolean;
begin
  ComandoLOG := 'CancelaItemVendido( '+IntToStr(NumItem)+' )' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  if Assigned( fOnAntesCancelaItemVendido ) then
     fOnAntesCancelaItemVendido( NumItem);

  try
    Tratado := False;
    fsECF.CancelaItemVendido( NumItem );
  except
     if Assigned( FOnErrorCancelaItemVendido ) then
        FOnErrorCancelaItemVendido(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'cancelaitemvendido' ;
      MemoAdicionaLinha( '<b>CANCELADO ITEM:</b> '+IntToStrZero( NumItem,3) ) ;
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.CancelaItemVendido( NumItem ) ;

  if Assigned( fOnDepoisCancelaItemVendido ) then
     FOnDepoisCancelaItemVendido( NumItem);

end;

procedure TACBrECF.SubtotalizaCupom(DescontoAcrescimo: Double;
   MensagemRodape : AnsiString );
var
  wSubtotal: Double;
  Tratado : Boolean;
begin
  { Ajustando valores acima de 2 Decimais }
  DescontoAcrescimo := RoundTo( DescontoAcrescimo, -2) ;

  { Tirando os Acentos e trocando todos os #13+#10 e '|' por #10 }
  fsMensagemRodape := StringReplace(  MensagemRodape,CR+LF,#10,[rfReplaceAll]) ;
  fsMensagemRodape := StringReplace(fsMensagemRodape,'|',#10,[rfReplaceAll]) ;

  ComandoLOG := 'SubtotalizaCupom( '+FloatToStr(DescontoAcrescimo)+' , '+
                    fsMensagemRodape+' )';

  wSubtotal := 0;
  if MemoAssigned then
    wSubtotal := Subtotal + DescontoAcrescimo;  // Le Subtotal do ECF

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  fsECF.ModoPreVenda := False;

  { Acumula fsMensagemRodape na memória para usar em FechaCupom. Alguns ECFs
    como a DataRegis não possuem metodo de Fechamento. O Fechamento é efetuado
    assim que o Total Pago atinge o valor do Cupom, nesse caso, a Msg de rodapé
    deve ser enviada antes dos Pagamentos }

  if Assigned( fOnAntesSubtotalizaCupom ) then
     fOnAntesSubtotalizaCupom( DescontoAcrescimo, MensagemRodape);

  try
    Tratado := False;
    fsECF.SubtotalizaCupom( DescontoAcrescimo, fsMensagemRodape );
  except
     if Assigned( FOnErrorSubtotalizaCupom ) then
        FOnErrorSubtotalizaCupom(Tratado);

     if not Tratado then
        raise;
  end;

  if DescontoAcrescimo > 0 then
     DoAtualizarValorGT ;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsSubTotalPagto := wSubtotal;
      MemoSubtotaliza(DescontoAcrescimo);
   end;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.SubTotalizaCupom( DescontoAcrescimo ) ;

  if Assigned( FOnDepoisSubtotalizaCupom ) then
     FOnDepoisSubtotalizaCupom( DescontoAcrescimo, fsMensagemRodape);

end;

{ Cancela o Acrescimo ou Desconto do Subtotal do Cupom }
procedure TACBrECF.CancelaDescontoAcrescimoSubTotal(
  TipoAcrescimoDesconto: Char);
begin
  ComandoLOG := 'CancelaDescontoAcrescimoSubTotal' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  fsECF.CancelaDescontoAcrescimoSubTotal(TipoAcrescimoDesconto) ;
end;

{ Cancela um item parcialmente }
procedure TACBrECF.CancelaItemVendidoParcial(NumItem: Integer;
  Quantidade: Double);
begin
  ComandoLOG := 'CancelaItemVendidoParcial' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  fsECF.CancelaItemVendidoParcial(NumItem,Quantidade);

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'cancelaitemvendidoparcial' ;
      MemoAdicionaLinha( '<b>CANC PARCIAL DE ITEM:</b> '+IntToStrZero( NumItem,3) ) ;
   end ;
  {$ENDIF}
end;

procedure TACBrECF.EfetuaPagamento(CodFormaPagto : String ; Valor : Double ;
   Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
Var
  FPG     : TACBrECFFormaPagamento ;
  Tratado : Boolean;
  wTotalPago: Double;
begin
  CodFormaPagto := Trim(CodFormaPagto);
  Observacao    := TrimRight(Observacao) ;
  { Tirando os #13 e #10 }
  Observacao := StringReplace(Observacao,CR,'',[rfReplaceAll]) ;
  Observacao := StringReplace(Observacao,LF,'',[rfReplaceAll]) ;
  Valor      := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }

  ComandoLOG := 'EfetuaPagamento( '+CodFormaPagto+' , '+
                    FloatToStr(Valor)+' , '+Observacao+', '+
                    BoolToStr( ImprimeVinculado)+', '+IntToStr(CodMeioPagamento)+' )';

  wTotalPago := 0;
  if MemoAssigned then
    wTotalPago := TotalPago + Valor;     // Lê TotalPago do ECF

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  FPG := AchaFPGIndice( CodFormaPagto ) ;
  if FPG = nil then
     raise EACBrECFErro.Create( Format(ACBrStr(cACBrECFAchaFPGIndiceException), [ CodFormaPagto ])) ;

  if ImprimeVinculado and (not FPG.PermiteVinculado) then
     raise EACBrECFErro.Create( Format(ACBrStr(cACBrECFFPGPermiteVinculadoException), [ CodFormaPagto ])) ;

  if Assigned( fOnAntesEfetuaPagamento ) then
     fOnAntesEfetuaPagamento( CodFormaPagto, Valor, Observacao, ImprimeVinculado);

  try
    Tratado := False;
    fsECF.EfetuaPagamento( CodFormaPagto, Valor, Observacao, ImprimeVinculado,
                           CodMeioPagamento );
  except
     if Assigned( fOnErrorEfetuaPagamento ) then
        fOnErrorEfetuaPagamento(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsTotalPago := wTotalPago;
      MemoEfetuaPagamento(FPG.Descricao, Valor, Observacao);
   end;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.EfetuaPagamento( FPG.Descricao, Valor ) ;

  if Assigned( fOnDepoisEfetuaPagamento ) then
     fOnDepoisEfetuaPagamento( CodFormaPagto, Valor, Observacao, ImprimeVinculado);

end;

{ Estorna um Pagamento Efetuado }
procedure TACBrECF.EstornaPagamento(CodFormaPagtoEstornar,
   CodFormaPagtoEfetivar : String; Valor: Double;
   Observacao : AnsiString = '') ;
begin
  CodFormaPagtoEstornar := Trim(CodFormaPagtoEstornar);
  Observacao := TrimRight(Observacao) ;
  { Tirando os #13 e #10 }
  Observacao := StringReplace(Observacao,CR,'',[rfReplaceAll]) ;
  Observacao := StringReplace(Observacao,LF,'',[rfReplaceAll]) ;
  Valor      := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }

  ComandoLOG := 'EstornaPagamento( '+CodFormaPagtoEstornar+', '+
           CodFormaPagtoEfetivar+', '+ FloatToStr(Valor)+' , '+Observacao +' )';

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  fsECF.EstornaPagamento( CodFormaPagtoEstornar,CodFormaPagtoEfetivar,
                          Valor, Observacao);
end;

procedure TACBrECF.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
var
  Tratado : Boolean;
  RodapePafECF, InfoConsumidorRodapeBobina: String;
begin
  if (Observacao = '') then
     Observacao := fsMensagemRodape ;

  if (not Consumidor.Enviado) and (not IdentificaConsumidorRodape) then
  begin
     if Consumidor.Documento <> '' then
        Observacao := Observacao + '|CPF/CNPJ consumidor: '+Consumidor.Documento ;

     if Consumidor.Nome <> '' then
        Observacao := Observacao + '|Nome: '+Consumidor.Nome ;

     if Consumidor.Endereco <> '' then
        Observacao := Observacao + '|Endereco: '+Consumidor.Endereco ;
  end
  else
  begin
    {$IFNDEF NOGUI}
    if IdentificaConsumidorRodape and MemoAssigned then
    begin
      InfoConsumidorRodapeBobina := '';

      if Consumidor.Documento <> '' then
        InfoConsumidorRodapeBobina := InfoConsumidorRodapeBobina + '|CPF/CNPJ consumidor: '+Consumidor.Documento ;

      if Consumidor.Nome <> '' then
        InfoConsumidorRodapeBobina := InfoConsumidorRodapeBobina + '|Nome: '+Consumidor.Nome ;

      if Consumidor.Endereco <> '' then
        InfoConsumidorRodapeBobina := InfoConsumidorRodapeBobina + '|Endereco: '+Consumidor.Endereco ;

      InfoConsumidorRodapeBobina := StringReplace(InfoConsumidorRodapeBobina,CR+LF,#10,[rfReplaceAll]) ;
      InfoConsumidorRodapeBobina := StringReplace(InfoConsumidorRodapeBobina,'|',#10,[rfReplaceAll]) ;

      InfoConsumidorRodapeBobina := Trim(DecodificarPaginaDeCodigoECF(InfoConsumidorRodapeBobina));

      InfoConsumidorRodapeBobina := AjustaLinhas( InfoConsumidorRodapeBobina, fsMemoColunas, 8 ) ;
      MemoAdicionaLinha( InfoConsumidorRodapeBobina );
    end ;
    {$ENDIF}
  end;

  { Trocando todos os #13, #13+#10 e '|' por #10 }
  Observacao := ChangeLineBreak(Observacao, #10);
  Observacao := StringReplace(Observacao,'|',#10,[rfReplaceAll]) ;

  { montar o rodape quando as informações de rodapé forem passadas }
  RodapePafECF := Trim(GetRodapePaf);

  if RodapePafECF <> EmptyStr then
    Observacao := RodapePafECF + #10 + Observacao;

  { Todos ECFs suportam no máximo 8 Linhas no Rodapé. Ajusta se necessário,
    para evitar erro na Impressão, no caso de mais linhas serem enviadas }
  if NumMaxLinhasRodape > 0 then
    Observacao := AjustaLinhas(Observacao, Colunas, NumMaxLinhasRodape);

  ComandoLOG := 'FechaCupom( '+Observacao+' )' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  if Assigned( fOnAntesFechaCupom ) then
     fOnAntesFechaCupom( Observacao, IndiceBMP);

  try
    Tratado := False;
    fsECF.FechaCupom( DecodificarTagsFormatacao( Observacao ), IndiceBMP ) ;
  except
     if Assigned( fOnErrorFechaCupom ) then
        fOnErrorFechaCupom(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'fechacupom' ;

      Observacao := AjustaLinhas( Observacao, fsMemoColunas, NumMaxLinhasRodape ) ;
      MemoAdicionaLinha( Observacao + sLineBreak + fsMemoRodape );
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.FechaCupom ;

  if Assigned( fOnDepoisFechaCupom ) then
  begin
     Observacao := DecodificarPaginaDeCodigoECF(Observacao);
     fOnDepoisFechaCupom( Observacao, IndiceBMP);
  end;

  fsMensagemRodape := '' ;
  Consumidor.Zera ;
  InfoRodapeCupom.Clear;
end;

function TACBrECF.GetRodapePaf: String;
begin
  Result := EmptyStr;

  // atende ao requisito do Paf-ECF
  if Trim(InfoRodapeCupom.MD5) <> EmptyStr then
  begin
    Result := 'MD-5:' + Trim(InfoRodapeCupom.MD5);

    { "NL" adicionado conforme requisito VIII-B item 6, somente para o DF e
      se existir consumidor atribuido }
    if InfoRodapeCupom.NotaLegalDF.Imprimir and
       (Consumidor.Enviado and (Consumidor.Documento <> '') ) then
      Result := Result + '"NL"'
  end;

  // atende ao requisito do paf-ECF V item 2
  if Trim(InfoRodapeCupom.PreVenda) <> EmptyStr then
    Result := Result + GetQuebraLinha('PV' + Trim(InfoRodapeCupom.PreVenda));

  // atende ao requisito do paf-ECF VI item 5
  if Trim(InfoRodapeCupom.Dav) <> EmptyStr then
    Result := Result + GetQuebraLinha('DAV' + Trim(InfoRodapeCupom.Dav));

  // atende ao requisito do paf-ECF VI item 5
  if Trim(InfoRodapeCupom.DavFarm) <> EmptyStr then
    Result := Result + GetQuebraLinha('Fórmula manipulada conf. DAV nº : ' + Trim(InfoRodapeCupom.DavFarm));

  // atende ao requisito do paf-ECF LI item 1
  if Trim(InfoRodapeCupom.DavOs) <> EmptyStr then
    Result := Result + GetQuebraLinha('DAV-OS' + Trim(InfoRodapeCupom.DavOs));

  // atende ao requisito do paf-ECF XXVIII item 8
  if Trim(InfoRodapeCupom.NF) <> EmptyStr then
    Result := Result + GetQuebraLinha('NF:' + Trim(InfoRodapeCupom.NF));

  Result := Trim(Result) + #10 + Trim(GetRodapePostos);
  Result := Trim(Result) + #10 + Trim(GetRodapeRestaurante);
  Result := Trim(Result) + #10 + Trim(GetRodapeUF);
  Result := Trim(Result) + #10 + Trim(GetRodapeImposto);

  // atende ao requisito XXXVI do paf-ECF
  if Trim(InfoRodapeCupom.Placa) <> '' then 
     Result := Result + #10 + 'Placa: ' + Trim(InfoRodapeCupom.Placa);

  Result := Trim(Result);
  InfoRodapeCupom.PostoCombustivel.Clear;
  InfoRodapeCupom.PostoCombustivel.Imprimir := False;
end;

function TACBrECF.GetRodapePostos: String;
var
  Rodape: String;
  I: Integer;
begin
  Rodape := EmptyStr;
    // atende ao requisito do PAF-ECF REQUISITO XXXVIII do BLOCO II ( REVENDEDOR VAREJISTA DE COMBUSTÍVEL AUTOMOTIVO )
  if ( InfoRodapeCupom.PostoCombustivel.Imprimir ) and
     (  InfoRodapeCupom.PostoCombustivel.Count > 0 )  then
  begin
      {
            1. O PAF-ECF deve imprimir no Cupom Fiscal, exclusivamente em uma única linha:
          a) a “Referência ao Sistema de Abastecimento de Combustíveis”;
          b) no campo "informações suplementares" ou “mensagens promocionais”, conforme o ECF que está em uso, na ordem dos abastecimentos, a partir do primeiro caractere ou a partir do caractere imediatamente seguinte aos registros do PV”N” ou do DAV“N”, quando for o caso, a expressão “#CF:” imediatamente antes da Referência ao Sistema de Abastecimento de Combustíveis de todos os bicos de abastecimento de combustíveis objeto da comercialização.
          Exemplo:
          #CF:B02 EI0008188,752 EF00020328,797 V12140,045
          Deve ser observado que não há espaço entre a expressão #CF: e o número do bico B02.
          c) se o Cupom Fiscal for emitido automaticamente, conforme previsto nas alíneas “c1” e “c2” do item 1 do Requisito XXXV, deve ser impressa a letra “A” imediatamente ao final do último caractere impresso.
          Exemplo:
          #CF:B02 EI0008188,752 EF00020328,797 V12140,045A
          Deve ser observado que não há espaço entre o número 12140,045 e a letra “A”.
      }
    with InfoRodapeCupom do
    begin
      for I := 0 to PostoCombustivel.Count - 1 do
      begin
        Rodape := Rodape + #10 +
        '#CF:B' + Format('%2.2d', [ PostoCombustivel[I].Bico ] ) +
        ' EI' + FormatFloatBr( PostoCombustivel[I].EI, '0000000.000' ) +
        ' EF' + FormatFloatBr( PostoCombustivel[I].EF, '0000000.000' ) +
        ' V' + FormatFloatBr( PostoCombustivel[I].Volume, '0.000' ) +
        ifthen( PostoCombustivel[I].Automatico, 'A', '' );

        if not PostoCombustivel[I].Automatico then
          Rodape := Rodape + ifthen( PostoCombustivel[I].Manual, 'M', '' ) ;
      end;
    end;
  end;
  Result := Rodape;
end;

function TACBrECF.GetRodapeRestaurante: String;
Var
  Rodape : String ;

  function DescricaoTipoConta: String;
  begin
    if InfoRodapeCupom.Restaurante.ContaCliente then
       Result := 'Conta de Cliente'
    else
       Result := 'Mesa';
  end;

begin
  Rodape := EmptyStr;

   // atende ao requisito do paf-ECF XXXVIII do Bloco III (Bares, Restaurantes e Similares)
  If InfoRodapeCupom.Restaurante.Imprimir then
  begin

    {
    no Cupom Fiscal a que se refere o item 8A deste requisito,
    tratando-se de ECF que imprima o campo "informações suplementares",
    imprimir neste campo, a partir do primeiro caracter, a seguinte informação:
    a) ECF: nnn - Conferência de Mesa - CER nº xxxxxx - COO nº yyyyyy,
       onde “nnn” é o número seqüencial do ECF atribuído pelo usuário onde foi emitido o Conferência de Mesa,
       “xxxxxx” é o número do Contador Específico de Relatório Gerencial (CER)
       e “yyyyyy” é o número do Contador de Ordem de Operação (COO) do Relatório Gerencial - Conferência de Mesa,
       quando for o caso de impressão da Conferência de Mesa.
    b) Consumo da Mesa xxx – SEM EMISSÃO DE CONFERÊNCIA DE MESA, onde xxx é o número da “Mesa Aberta”.
    }
      if (InfoRodapeCupom.Restaurante.COO > 0) then
      begin

        Rodape := Rodape + #10 + 'ECF:'
        + Format('%3.3d', [InfoRodapeCupom.Restaurante.ECF])
        + ' - Conf. de ' + DescricaoTipoConta;

          if InfoRodapeCupom.Restaurante.CER > 0 then
          begin
            Rodape := Rodape + ' - CER:' +
            Format('%4.4d', [InfoRodapeCupom.Restaurante.CER]);
          end;

        Rodape := Rodape + ' – COO:' +
        Format('%6.6d', [InfoRodapeCupom.Restaurante.COO]);

      end
      else
      begin
        Rodape := Rodape + #10 +
                  DescricaoTipoConta + ' ' + InfoRodapeCupom.Restaurante.Mesa +
                  ' – SEM EMISSÃO DE CONFERÊNCIA DE ' + UpperCase(DescricaoTipoConta);
      end;
  end;

  Result := ACBrStr( Rodape );
end;

function TACBrECF.GetQuebraLinha(const Value :string):String;
begin
  Result := Ifthen(QuebraLinhaRodape, sLineBreak, EmptyStr) + Value;
end;

function TACBrECF.GetRodapeUF: String;
Var
  Rodape : String ;
begin
  Rodape := EmptyStr;

  if InfoRodapeCupom.CupomMania then
  begin
    Rodape := Rodape + #10 + 'CUPOM MANIA, CONCORRA A PRÊMIOS';
    Rodape := Rodape + #10 + 'ENVIE SMS P/ 6789: ' +
      Copy(OnlyNumber(fsECF.IE), 1, 8) +    // 8 primeiros digitos da Inscr.Estadual
      FormatDateTime('ddmmyy', Self.DataHora) +      // data atual
      Format('%6.6d', [StrToInt(NumCOO)]) + // numero do coo do cupom
      Format('%3.3d', [StrToInt(NumECF)]);  // numero do ecf

    Rodape := Rodape + #10 + 'PROCON: Av. Rio Branco, 25 - Centro–RJ. Tel: 151';
    Rodape := Rodape + #10 + 'ALERJ - Rua da Alfandega, 8 - Tel. 0800 2827060';
  end
  else if InfoRodapeCupom.MinasLegal then
  begin
    Rodape := Rodape + #10 + Format(
      'MINAS LEGAL: %s %s %s', [
      OnlyNumber(Self.CNPJ),
      FormatDateTime('ddmmyyyy', Self.DataHora),
      IntToStr(TruncFix(Self.Subtotal * 100))
    ]);
  end
  else if InfoRodapeCupom.ParaibaLegal then
  begin
    Rodape := Rodape + #10 +
      'PARAÍBA LEGAL – RECEITA CIDADÃ' + #10 +
      Format(
        'TORPEDO PREMIADO:' + #10 +
        '%s %s %s %s', [
        OnlyNumber(Self.IE),
        FormatDateTime('ddmmyyyy', Self.DataHora),
        Self.NumCOO,
        IntToStr(TruncFix(Self.Subtotal * 100))
      ]) ;
  end
  else if InfoRodapeCupom.NotaLegalDF.Imprimir then
  begin
    if InfoRodapeCupom.NotaLegalDF.ProgramaDeCredito then
    begin
      Rodape := Rodape + #10 +
        'ESTABELECIMENTO INCLUÍDO NO PROGRAMA DE'#10 +
        'CONCESSÃO DE CRÉDITOS - LEI Nº 4.159/2008.';
    end;

    Rodape := Rodape + #10 + '<n>NOTA LEGAL:</n>';

    Rodape := Rodape + ' ICMS = ' + FormatFloatBr(InfoRodapeCupom.NotaLegalDF.ValorICMS, ',#0.00');
    Rodape := Rodape + ' ISS = ' + FormatFloatBr(InfoRodapeCupom.NotaLegalDF.ValorISS, ',#0.00');
  end;

  Result := ACBrStr( Rodape );
end;

function TACBrECF.GetRodapeImposto: String;
var
  VlImposto: Double;
  VlPercentual: Double;
  VlImpostoFederal, VlImpostoEstadual, VlImpostoMunicipal: Double;
  VlPercentualFederal, VlPercentualEstadual, VlPercentualMunicipal: Double;
  InformouValorAproxFederal, InformouValorAproxEstadual, InformouValorAproxMunicipal: Boolean;
  SubtotalSemImpostos: Double;
  SubtotalCache: Double;
begin
  Result := '';
  if InfoRodapeCupom.Imposto.ValorAproximado > 0 then
  begin
    // valor aproximado informado pelo usuário
    VlImposto := InfoRodapeCupom.Imposto.ValorAproximado;

    // valor aproximado percentual
    VlPercentual := VlImposto / Subtotal * 100;

    // impressão do texto
    // se o usuário informou a propriedade Texto, utilizar
    // se não imprimir como no IBPT
    if Trim(InfoRodapeCupom.Imposto.Texto) <> '' then
    begin
      Result := Format(InfoRodapeCupom.Imposto.Texto, [VlImposto, VlPercentual]);
    end
    else
    begin
      Result := 'Val.Aprox.Impostos R$' +
                FormatFloatBr(VlImposto, ',#0.00') +
                FormatFloatBr(VlPercentual, '(,#0.00%)') +
                IfThen(Trim(InfoRodapeCupom.Imposto.Fonte) <> '',
                       ' Fonte:' + InfoRodapeCupom.Imposto.Fonte, '');
    end;
  end
  else
  begin
    InformouValorAproxFederal   := (InfoRodapeCupom.Imposto.ValorAproximadoFederal > 0);
    InformouValorAproxEstadual  := (InfoRodapeCupom.Imposto.ValorAproximadoEstadual > 0);
    InformouValorAproxMunicipal := (InfoRodapeCupom.Imposto.ValorAproximadoMunicipal > 0);

    if InformouValorAproxFederal or InformouValorAproxEstadual or InformouValorAproxMunicipal then
    begin
      //evita ter que fazer várias leituras na serial
      SubtotalCache := Subtotal;
      // valor aproximado informado pelo usuário
      VlImpostoFederal := InfoRodapeCupom.Imposto.ValorAproximadoFederal;
      VlImpostoEstadual := InfoRodapeCupom.Imposto.ValorAproximadoEstadual;
      VlImpostoMunicipal := InfoRodapeCupom.Imposto.ValorAproximadoMunicipal;

      // valor aproximado percentual (Não utilizado nas sugestões do IBPT no momento...)
      VlPercentualFederal := VlImpostoFederal / SubtotalCache * 100;
      VlPercentualEstadual := VlImpostoEstadual / SubtotalCache * 100;
      VlPercentualMunicipal := VlImpostoMunicipal / SubtotalCache * 100;

      // impressão do texto
      // se o usuário informou a propriedade Texto, utilizar
      // se não imprimir como no IBPT
      if Trim(InfoRodapeCupom.Imposto.Texto) <> '' then
      begin
        Result := Format(InfoRodapeCupom.Imposto.Texto, [VlImpostoFederal, VlPercentualFederal, VlImpostoEstadual, VlPercentualEstadual, VlImpostoMunicipal, VlPercentualMunicipal]);
      end
      else
      begin
        if InfoRodapeCupom.Imposto.ModoCompacto  then
        begin
          // IBPT opção 2
          // Impressão supercompacta caso informe os tres impostos.
          if InformouValorAproxEstadual and InformouValorAproxMunicipal then
          begin
            Result := 'Trib aprox R$:' +
              FormatFloatBr(VlImpostoFederal, ',#0.00')   + ' Fed, '+
              FormatFloatBr(VlImpostoEstadual, ',#0.00')  + ' Est e '+
              FormatFloatBr(VlImpostoMunicipal, ',#0.00') + ' Mun'+
              IfThen(Trim(InfoRodapeCupom.Imposto.Fonte) <> '', #10 +
                     'Fonte:' + InfoRodapeCupom.Imposto.Fonte, '') + ' ' +
              Trim(InfoRodapeCupom.Imposto.Chave);
          end
          else
          begin
            Result := 'Trib aprox R$:' +
              FormatFloatBr(VlImpostoFederal, ',#0.00') + ' Federal'+
              IfThen(InformouValorAproxEstadual,
                     ', '+FormatFloatBr(VlImpostoEstadual,',#0.00') + ' Estadual', '') +
              IfThen(InformouValorAproxMunicipal,
                     ', '+FormatFloatBr(VlImpostoMunicipal,',#0.00') + ' Municipal', '') +
              IfThen(Trim(InfoRodapeCupom.Imposto.Fonte) <> '',
                     #10 + 'Fonte:' + InfoRodapeCupom.Imposto.Fonte, '') + ' ' +
              Trim(InfoRodapeCupom.Imposto.Chave);
          end;
        end
        else
        begin
          // IBPT opção 1
          SubtotalSemImpostos := SubtotalCache - (VlImpostoFederal + VlImpostoEstadual + VlImpostoMunicipal);

          Result := 'Você pagou aproximadamente:' + #10 +
            'R$ ' + FormatFloatBr(VlImpostoFederal, ',#0.00') + ' de tributos federais'+
            IfThen(InformouValorAproxEstadual,
                   #10 + 'R$ ' + FormatFloatBr(VlImpostoEstadual, ',#0.00') + ' de tributos estaduais', '') +
            IfThen(InformouValorAproxMunicipal,
                   #10 + 'R$ ' + FormatFloatBr(VlImpostoMunicipal, ',#0.00') + ' de tributos municipais', '') +
            #10 + 'R$ ' + FormatFloatBr(SubtotalSemImpostos, ',#0.00') + ' pelos produtos/serviços'+
            IfThen(Trim(InfoRodapeCupom.Imposto.Fonte) <> '',
                   #10 + 'Fonte:' + InfoRodapeCupom.Imposto.Fonte, '') + ' ' +
            Trim(InfoRodapeCupom.Imposto.Chave);
        end;
      end;
    end;
  end;

  Result := ACBrStr(Result);
end;

procedure TACBrECF.Sangria(Valor: Double; Obs: AnsiString;
   DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer ) ;
Var
  Tratado : Boolean;
begin
  Valor := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }
  Obs   := TrimRight(Obs);
  if DescricaoCNF = '' then
     DescricaoCNF := 'SANGRIA' ;

  if DescricaoFPG = '' then
     DescricaoFPG := 'DINHEIRO' ;

  ComandoLOG := 'Sangria( '+FloatToStr(Valor)+', '+Obs+', '+DescricaoCNF+', '+DescricaoFPG+' )' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  if Assigned( fOnAntesSangria ) then
     fOnAntesSangria( Valor, Obs, DescricaoCNF, DescricaoFPG);

  try
    Tratado := False;
    fsECF.Sangria( Valor, Obs, DescricaoCNF, DescricaoFPG, IndiceBMP );
  except
     if Assigned( FOnErrorSangria ) then
        FOnErrorSangria(Tratado);

     if not Tratado then
        raise;
  end;

  if Assigned( FOnDepoisSangria ) then
     FOnDepoisSangria( Valor, Obs, DescricaoCNF, DescricaoFPG);

end;

procedure TACBrECF.Suprimento(Valor: Double; Obs: AnsiString;
   DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer ) ;
Var
  Tratado : Boolean;
begin
  Valor := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }
  Obs   := TrimRight(Obs);
  if DescricaoCNF = '' then
     DescricaoCNF := 'SUPRIMENTO' ;

  if DescricaoFPG = '' then
     DescricaoFPG := 'DINHEIRO' ;

  ComandoLOG := 'Suprimento( '+FloatToStr(Valor)+', '+Obs+', '+DescricaoCNF+', '+DescricaoFPG+' )' ;

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  if Assigned( fOnAntesSuprimento ) then
     fOnAntesSuprimento( Valor, Obs, DescricaoCNF, DescricaoFPG);

  try
    Tratado := False;
    fsECF.Suprimento( Valor, Obs, DescricaoCNF, DescricaoFPG, IndiceBMP );
  except
     if Assigned( fOnErrorSuprimento ) then
        fOnErrorSuprimento(Tratado);

     if not Tratado then
        raise;
  end;

  if Assigned( fOnDepoisSuprimento ) then
     fOnDepoisSuprimento( Valor, Obs, DescricaoCNF, DescricaoFPG);
end;

function TACBrECF.EstornaCCD(const Todos : Boolean) : Integer ;
Var
  Tratado : Boolean;
  Docto, I : Integer ;
begin
  ComandoLOG := 'EstornaCCD( '+BoolToStr( Todos )+' )';

  if Assigned( fsAAC ) then
     fsAAC.VerificaReCarregarArquivo;

  Docto  := -1 ;
  Result := 0;

  {$IFNDEF NOGUI}
   if MemoAssigned  then
   begin
      Docto := StrToInt(NumCupom) ;
   end ;
  {$ENDIF}

  if Assigned( fOnAntesCancelaNaoFiscal ) then
     fOnAntesCancelaNaoFiscal(Self);

  try
    Tratado := False;
    Result  := fsECF.EstornaCCD( Todos ) ;
  except
     if Assigned( FOnErrorCancelaNaoFiscal ) then
        FOnErrorCancelaNaoFiscal(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'EstornaCCD' ;

      For I := 1 to Result do
      begin
         MemoTitulo('*** ESTORNO de CCD  ****');
         MemoAdicionaLinha('       COO: '+IntToStrZero(Docto-I+1,6));
         MemoAdicionaLinha( fsMemoRodape );
      end
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.Documento('NC');

  if Assigned( FOnDepoisCancelaNaoFiscal ) then
     FOnDepoisCancelaNaoFiscal(Self);
end ;


procedure TACBrECF.NaoFiscalCompleto(CodCNF: String; Valor: Double;
  CodFormaPagto: String; Obs: AnsiString; IndiceBMP : Integer);
begin
  CodCNF := Trim( CodCNF );
  Valor  := RoundTo( Valor, -2) ;     { Ajustando valores acima de 2 Decimais }
  CodFormaPagto := Trim( CodFormaPagto );
  Obs    := TrimRight(Obs);

  ComandoLOG := 'NaoFiscalCompleto(' +CodCNF+' , '+FloatToStr(Valor)+' , '+
                CodFormaPagto+' , '+Obs+' )' ;

  if RFDAtivo then
     fsRFD.VerificaParametros ;

  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  fsECF.NaoFiscalCompleto(CodCNF, Valor, CodFormaPagto, Obs, IndiceBMP);

  if RFDAtivo and (not fsRegistrouRFDCNF) then
     fsRFD.Documento('CN');

  fsRegistrouRFDCNF := False ;
end;

procedure TACBrECF.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
Var
  Tratado : Boolean;
begin
  ComandoLOG := 'AbreNaoFiscal( '+CPF_CNPJ+','+Nome+','+Endereco+' )' ;

  if RFDAtivo then
     fsRFD.VerificaParametros ;

  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  if Assigned( fOnAntesAbreNaoFiscal ) then
     fOnAntesAbreNaoFiscal(CPF_CNPJ, Nome, Endereco);

  try
    Tratado := False;
    fsECF.AbreNaoFiscal( CPF_CNPJ, Nome, Endereco );
  except
     if Assigned( fOnErrorAbreNaoFiscal ) then
        fOnErrorAbreNaoFiscal(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'abrenaofiscal' ;
      MemoAdicionaCabecalho ;

      if Trim(CPF_CNPJ) <> '' then
      begin
         MemoAdicionaLinha(LeftStr('CPF/CNPJ Consumidor: '+CPF_CNPJ, fsMemoColunas)) ;
         MemoAdicionaLinha( '<hr>' ) ;
      end ;

      MemoTitulo('CUPOM NAO FISCAL');
   end ;
  {$ENDIF}

  if RFDAtivo then
  begin
     fsRFD.Documento('CN');
     fsRegistrouRFDCNF := True ;
  end ;

  if Assigned( fOnDepoisAbreNaoFiscal ) then
     fOnDepoisAbreNaoFiscal( CPF_CNPJ, Nome, Endereco);

end;

procedure TACBrECF.RegistraItemNaoFiscal(CodCNF: String; Valor: Double;
  Obs: AnsiString = '');
 Var CNF : TACBrECFComprovanteNaoFiscal ;
begin
  CodCNF := Trim( CodCNF );
  Valor  := RoundTo( Valor, -2) ;     { Ajustando valores acima de 2 Decimais }
  Obs    := TrimRight(Obs);

  ComandoLOG := 'RegistraItemNaoFiscal( '+CodCNF+' , '+ FloatToStr( Valor )+
                ' , '+Obs + ' )';

  CNF := AchaCNFIndice( CodCNF ) ;
  if CNF = nil then
     raise EACBrECFErro.Create( Format(ACBrStr(cACBrECFRegistraItemNaoFiscalException), [CodCNF] )) ;

  Obs := TrimRight(Obs) ;
  { Ajustando valores acima de 2 Decimais }
  Valor := RoundTo( Valor, -2) ;

  fsECF.RegistraItemNaoFiscal(CodCNF, Valor, Obs);

  DoAtualizarValorGT ;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      Inc( fsMemoItens ) ;

      if Trim(Obs) <> '' then
         Obs := sLineBreak + Obs ;

      MemoAdicionaLinha( '<table width=100%><tr>'+
                         '<td align=left>'+IntToStrZero( fsMemoItens, 3)+'</td>'+
                         '<td align=left>'+CNF.Descricao+'</td>'+
                         '<td align=right>'+FormatFloatBr(Valor,'#,###,##0.00')+'</td>'+
                         '</tr></table>' + Obs ) ;
   end ;
  {$ENDIF}
end;

procedure TACBrECF.CancelaNaoFiscal;
Var
  Tratado : Boolean;
  OldEstado : TACBrECFEstado ;
  Docto     : String ;
  SubTot    : Double ;
begin
  ComandoLOG := 'CancelaNaoFiscal';

  OldEstado := estDesconhecido ;
  SubTot    := 0 ;
  Docto     := '' ;

  {$IFNDEF NOGUI}
  if MemoAssigned  then
  begin
     OldEstado := Estado ;
     SubTot    := Subtotal ;
     Docto     := IntToStrZero( StrToInt(NumCupom) ,6) ;
  end ;
  {$ENDIF}

  if Assigned( fOnAntesCancelaNaoFiscal ) then
     fOnAntesCancelaNaoFiscal(Self);

  try
    Tratado := False;
    fsECF.CancelaNaoFiscal ;
  except
     if Assigned( FOnErrorCancelaNaoFiscal ) then
        FOnErrorCancelaNaoFiscal(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'CancelaNaoFiscal' ;

      if OldEstado in [estNaoFiscal] then
       begin
          MemoTitulo('* COMPROVANTE NÃO-FISCAL *');
          MemoTitulo('***      CANCELADO     ***');

          if OldEstado = estVenda then
             MemoAdicionaLinha( '<table width=100%><tr>'+
              '<td align=left>TOTAL R$</td>'+
              '<td align=right><b>'+FormatFloatBr(SubTot,'###,##0.00')+'</b></td>'+
              '</tr></table>') ;

           MemoAdicionaLinha( fsMemoRodape );
       end
      else
       begin
          MemoAdicionaCabecalho ;
          MemoTitulo('* COMPROVANTE NÃO-FISCAL *');
          MemoTitulo('***      CANCELADO     ***');
          MemoAdicionaLinha( '<table width=100%><tr>'+
              '<td align=left>COO do CNF Cancelado:</td>'+
              '<td align=right><b>'+Docto+'</b></td>'+
              '</tr></table>' + sLineBreak + sLineBreak +
              '<table width=100%><tr>'+
              '<td align=left>Valor da Operacao  R$:</td>'+
              '<td align=right><b>'+FormatFloatBr(SubTot,'#,###,##0.00')+'</b></td>'+
              '</tr></table>' + sLineBreak + sLineBreak +
              fsMemoRodape ) ;
       end ;
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.Documento('NC');

  if Assigned( FOnDepoisCancelaNaoFiscal ) then
     FOnDepoisCancelaNaoFiscal(Self);
end;

procedure TACBrECF.CancelaItemNaoFiscal(const AItem : Integer) ;
var
  Tratado: Boolean;
begin
  ComandoLOG := 'CancelaItemNaoFiscal';

  if Assigned( fOnAntesCancelaItemNaoFiscal ) then
    fOnAntesCancelaItemNaoFiscal(AItem);

  try
    Tratado := False;
    fsECF.CancelaItemNaoFiscal(AItem);
  except
    if Assigned( FOnErrorCancelaItemNaoFiscal ) then
      FOnErrorCancelaItemNaoFiscal(Tratado);

    if not Tratado then
      raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
     fsMemoOperacao := 'CancelaItemNaoFiscal' ;
     MemoAdicionaLinha( '<b>CANCELADO ITEM:</b> '+IntToStrZero(AItem, 3) ) ;
   end ;
  {$ENDIF}

  if Assigned( FOnDepoisCancelaItemNaoFiscal ) then
    FOnDepoisCancelaItemNaoFiscal(AItem);
end;

procedure TACBrECF.EfetuaPagamentoNaoFiscal(CodFormaPagto: String;
  Valor: Double; Observacao: AnsiString; ImprimeVinculado: Boolean);
Var
  FPG     : TACBrECFFormaPagamento ;
  Tratado : Boolean;
begin
  CodFormaPagto := Trim(CodFormaPagto);
  Observacao    := TrimRight(Observacao) ;
  { Tirando os #13 e #10 }
  Observacao := StringReplace(Observacao,CR,'',[rfReplaceAll]) ;
  Observacao := StringReplace(Observacao,LF,'',[rfReplaceAll]) ;
  Valor      := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }

  ComandoLOG := 'EfetuaPagamentoNaoFiscal( '+CodFormaPagto+' , '+
                    FloatToStr(Valor)+' , '+Observacao+', '+
                    BoolToStr( ImprimeVinculado)+' )';

  FPG := AchaFPGIndice( CodFormaPagto ) ;
  if FPG = nil then
     raise EACBrECFErro.Create( Format(ACBrStr(cACBrECFAchaFPGIndiceException),
                                   [ CodFormaPagto ] )) ;

  if ImprimeVinculado and (not FPG.PermiteVinculado) then
     raise EACBrECFErro.Create( Format(ACBrStr(cACBrECFFPGPermiteVinculadoException),
                                   [ CodFormaPagto ] )) ;

  if Assigned( fOnAntesEfetuaPagamentoNaoFiscal ) then
     fOnAntesEfetuaPagamentoNaoFiscal( CodFormaPagto, Valor, Observacao, ImprimeVinculado);

  try
    Tratado := False;
    fsECF.EfetuaPagamentoNaoFiscal( CodFormaPagto, Valor, Observacao,
                                  ImprimeVinculado);
  except
     if Assigned( FOnErrorEfetuaPagamentoNaoFiscal ) then
        FOnErrorEfetuaPagamentoNaoFiscal(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
      MemoEfetuaPagamento(FPG.Descricao, Valor, Observacao);
  {$ENDIF}

  if RFDAtivo then
     fsRFD.EfetuaPagamento( FPG.Descricao, Valor ) ;

  if Assigned( FOnDepoisEfetuaPagamentoNaoFiscal ) then
     FOnDepoisEfetuaPagamentoNaoFiscal( CodFormaPagto, Valor, Observacao, ImprimeVinculado);

end;

procedure TACBrECF.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
   MensagemRodape: AnsiString);
Var
  Tratado : Boolean;
begin
  { Ajustando valores acima de 2 Decimais }
  DescontoAcrescimo := RoundTo( DescontoAcrescimo, -2) ;

  { Tirando os Acentos e trocando todos os #13+#10 por #10 }
  fsMensagemRodape := StringReplace(MensagemRodape,CR+LF,#10,[rfReplaceAll]) ;
  fsMensagemRodape := StringReplace(fsMensagemRodape,'|',#10,[rfReplaceAll]) ;

  { Acumula fsMensagemRodape na memória para usar em FechaCupom. Alguns ECFs
    como a DataRegis não possuem metodo de Fechamento. O Fechamento é efetuado
    assim que o Total Pago atinge o valor do Cupom, nesse caso, a Msg de rodapé
    deve ser enviada antes dos Pagamentos }

  ComandoLOG := 'SubtotalizaNaoFiscal( '+FloatToStr(DescontoAcrescimo)+' , '+
                    fsMensagemRodape+' )';

  if Assigned( fOnAntesSubtotalizaNaoFiscal ) then
     fOnAntesSubtotalizaNaoFiscal( DescontoAcrescimo, MensagemRodape);

  try
    Tratado := False;
    fsECF.SubtotalizaNaoFiscal( DescontoAcrescimo, fsMensagemRodape );
  except
     if Assigned( fOnErrorSubtotalizaNaoFiscal ) then
        fOnErrorSubtotalizaNaoFiscal(Tratado);

     if not Tratado then
        raise;
  end;

  if DescontoAcrescimo > 0 then
     DoAtualizarValorGT ;

  {$IFNDEF NOGUI}
   if MemoAssigned then
      MemoSubtotaliza( DescontoAcrescimo );
  {$ENDIF}

  if Assigned( fOnDepoisSubtotalizaNaoFiscal ) then
     fOnDepoisSubtotalizaNaoFiscal( DescontoAcrescimo, MensagemRodape);

end;

procedure TACBrECF.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
Var
  Tratado : Boolean;
begin
  if (Observacao = '') then
     Observacao := fsMensagemRodape ;

  Observacao := TrimRight( Observacao );
  { Trocando todos os #13+#10 e '|' por #10 }
  Observacao := StringReplace(Observacao,CR+LF,#10,[rfReplaceAll]) ;
  Observacao := StringReplace(Observacao,'|',#10,[rfReplaceAll]) ;

  ComandoLOG := 'FechaNaoFiscal( '+Observacao+' )' ;

  if Assigned( fOnAntesFechaNaoFiscal ) then
     fOnAntesFechaNaoFiscal(Observacao, IndiceBMP);

  try
    Tratado := False;
    fsECF.FechaNaoFiscal( DecodificarTagsFormatacao( Observacao ), IndiceBMP ) ;
  except
     if Assigned( FOnErrorFechaNaoFiscal ) then
        FOnErrorFechaNaoFiscal(Tratado);

     if not Tratado then
        raise;
  end;

  fsMensagemRodape := '' ;
  Consumidor.Zera ;
  InfoRodapeCupom.Clear;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'fechanaofiscal' ;

      Observacao := AjustaLinhas( Observacao, fsMemoColunas, 8 ) ;
      MemoAdicionaLinha( Observacao + sLineBreak + fsMemoRodape );
   end ;
  {$ENDIF}

  if Assigned( FOnDepoisFechaNaoFiscal ) then
  begin
     Observacao := DecodificarPaginaDeCodigoECF(Observacao);
     FOnDepoisFechaNaoFiscal(Observacao, IndiceBMP);
  end;
end;

procedure TACBrECF.CorrigeEstadoErro(ReducaoZ: Boolean);
begin
  ComandoLOG := 'CorrigeEstadoErro' ;
  fsECF.CorrigeEstadoErro(ReducaoZ) ;
end;

function TACBrECF.EnviaComando(cmd: AnsiString ): AnsiString;
begin
  ComandoLOG := 'EnviaComando( '+cmd+' )' ;
  Result := fsECF.EnviaComando( cmd ) ;
end;

function TACBrECF.EnviaComando(cmd: AnsiString; lTimeOut: Integer): AnsiString;
begin
  ComandoLOG := 'EnviaComando( '+cmd+', '+IntToStr(lTimeOut)+' )' ;
  Result := fsECF.EnviaComando( cmd, lTimeOut ) ;
end;

procedure TACBrECF.FechaRelatorio;
{$IFNDEF NOGUI}
 Var OldEstado : TACBrECFEstado ;
{$ENDIF}
Var
  Tratado : Boolean;
begin
  ComandoLOG := 'FechaRelatorio' ;

  {$IFNDEF NOGUI}
  OldEstado := estDesconhecido ;
   if MemoAssigned then
      OldEstado := Estado ;
  {$ENDIF}

  if Assigned( fOnAntesFechaRelatorio ) then
     fOnAntesFechaRelatorio(Self);

  try
    Tratado := False;
    fsECF.FechaRelatorio ;
  except
     if Assigned( FOnErrorFechaRelatorio ) then
        FOnErrorFechaRelatorio(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      if OldEstado <> Estado then
      begin
         fsMemoOperacao := 'fecharelatorio' ;
         MemoAdicionaLinha( fsMemoRodape );
      end ;
   end ;
  {$ENDIF}

  if Assigned( FOnDepoisFechaRelatorio ) then
     FOnDepoisFechaRelatorio(Self);

end;

procedure TACBrECF.PulaLinhas(const NumLinhas: Integer);
begin
  ComandoLOG := 'PulaLinhas( '+IntToStr(NumLinhas)+' ) ';
  fsECF.PulaLinhas( NumLinhas );

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'pulalinhas' ;
      MemoAdicionaLinha( StringReplace(StringOfChar(#10, NumLinhas ),#10,sLineBreak,[rfReplaceAll]) );
   end ;
  {$ENDIF}
end;

procedure TACBrECF.CortaPapel(const CorteParcial : Boolean) ;
begin
  ComandoLOG := 'CortaPapel';
  fsECF.CortaPapel( CorteParcial ) ;
end;

function TACBrECF.GetChequeProntoClass: Boolean;
begin
  ComandoLOG := 'ChequePronto' ;
  Result := fsECF.ChequePronto ;
end;

procedure TACBrECF.AbreGaveta;
begin
  ComandoLOG := 'AbreGaveta' ;
  fsECF.AbreGaveta ;
end;

function TACBrECF.GetGavetaAbertaClass: Boolean;
begin
  ComandoLOG := 'GavetaAberta' ;
  Result := fsECF.GavetaAberta ;

  if fsGavetaSinalInvertido then
     Result := not Result ;
end;

function TACBrECF.GetSubTotalClass: Double;
begin
  ComandoLOG := 'Subtotal' ;
  Result := RoundTo( fsECF.Subtotal, -2) ;
end;

function TACBrECF.GetTotalPagoClass: Double;
begin
  ComandoLOG := 'TotalPago' ;
  Result := RoundTo( fsECF.TotalPago, -2) ;
end;

procedure TACBrECF.ImpactoAgulhas( NivelForca : Integer = 2);
begin
  ComandoLOG := 'ImpactoAgulhas( '+IntToStr(NivelForca)+' )' ;
  fsECF.ImpactoAgulhas( NivelForca ) ;
end;

procedure TACBrECF.LeituraMemoriaFiscal(ReducaoInicial, ReducaoFinal: Integer;
   Simplificada : Boolean = False);
begin
  ComandoLOG := 'LeituraMemoriaFiscal( '+IntToStr(ReducaoInicial)+' , '+
                    IntToStr(ReducaoFinal)+' ,'+BoolToStr(Simplificada)+' )';

  if RFDAtivo then
     fsRFD.VerificaParametros ;

  fsECF.LeituraMemoriaFiscal(ReducaoInicial, ReducaoFinal, Simplificada);

  if RFDAtivo then
     fsRFD.Documento('MF');
end;

procedure TACBrECF.LeituraMemoriaFiscal(DataInicial, DataFinal: TDateTime;
   Simplificada : Boolean);
begin
  ComandoLOG := 'LeituraMemoriaFiscal( '+DateToStr(DataInicial)+' , '+
                    DateToStr(DataFinal)+' ,'+BoolToStr(Simplificada)+' )';

  if RFDAtivo then
     fsRFD.VerificaParametros ;

  fsECF.LeituraMemoriaFiscal(DataInicial, DataFinal, Simplificada);

  if RFDAtivo then
     fsRFD.Documento('MF');
end;

procedure TACBrECF.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas : TStringList; Simplificada : Boolean );
begin
  if ComandoLOG = '' then
     ComandoLOG := 'LeituraMemoriaFiscalSerial( '+IntToStr(ReducaoInicial)+' , '+
                       IntToStr(ReducaoFinal)+' , Linhas ,'+
                       BoolToStr(Simplificada)+' )';
  fsECF.LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal, Linhas,
                                    Simplificada ) ;

  Linhas.Text := DecodificarPaginaDeCodigoECF( Linhas.Text );
end;

procedure TACBrECF.LeituraMemoriaFiscalSerial(DataInicial,
   DataFinal: TDateTime; Linhas : TStringList; Simplificada : Boolean );
begin
  if ComandoLOG = '' then
     ComandoLOG := 'LeituraMemoriaFiscalSerial( '+DateToStr(DataInicial)+' , '+
                       DateToStr(DataFinal)+' , Linhas ,'+BoolToStr(Simplificada)+' )';
  fsECF.LeituraMemoriaFiscalSerial( DataInicial, DataFinal, Linhas,
                                    Simplificada) ;

  Linhas.Text := DecodificarPaginaDeCodigoECF( Linhas.Text );
end;

procedure TACBrECF.LeituraMemoriaFiscalSerial(ReducaoInicial,
  ReducaoFinal: Integer; NomeArquivo: String; Simplificada: Boolean);
 Var AStringList : TStringList ;
begin
  ComandoLOG := 'LeituraMemoriaFiscalSerial( '+IntToStr(ReducaoInicial)+' , '+
                    IntToStr(ReducaoFinal)+' , '+NomeArquivo+' ,'+
                    BoolToStr(Simplificada)+' )';
  AStringList := TStringList.Create ;
  try
     LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal, AStringList,
                                 Simplificada ) ;

     AStringList.SaveToFile(NomeArquivo);
  finally
     AStringList.Free ;
  end ;
end;

procedure TACBrECF.LeituraMemoriaFiscalSerial(DataInicial,
  DataFinal: TDateTime; NomeArquivo: String; Simplificada: Boolean);
 Var AStringList : TStringList ;
begin
  ComandoLOG := 'LeituraMemoriaFiscalSerial( '+DateToStr(DataInicial)+' , '+
                    DateToStr(DataFinal)+' , '+NomeArquivo+' ,'+
                    BoolToStr(Simplificada)+' )';
  AStringList := TStringList.Create ;
  try
     LeituraMemoriaFiscalSerial( DataInicial, DataFinal, AStringList,
                                 Simplificada ) ;

     AStringList.SaveToFile(NomeArquivo);
  finally
     AStringList.Free ;
  end ;
end;

procedure TACBrECF.TestaSeE_MFD ;
begin
  if not MFD then
     raise EACBrECFErro.Create( ACBrStr('ECF '+fsECF.ModeloStr+' não é MFD') ) ;
end ;

procedure TACBrECF.LeituraMFDSerial(DataInicial, DataFinal: TDateTime;
  Linhas: TStringList; Documentos : TACBrECFTipoDocumentoSet );
begin
  TestaSeE_MFD ;

  if ComandoLOG = '' then
     ComandoLOG := 'LeituraMFDSerial( '+DateToStr(DataInicial)+' , '+
                       DateToStr(DataFinal)+' , Linhas) ';
  fsECF.LeituraMFDSerial( DataInicial, DataFinal, Linhas, Documentos ) ;

  Linhas.Text := DecodificarPaginaDeCodigoECF( Linhas.Text );
end;

procedure TACBrECF.LeituraMFDSerial(COOInicial, COOFinal: Integer;
  Linhas: TStringList; Documentos : TACBrECFTipoDocumentoSet );
begin
  TestaSeE_MFD ;

  if ComandoLOG = '' then
     ComandoLOG := 'LeituraMFDSerial( '+IntToStr(COOInicial)+' , '+
                       IntToStr(COOFinal)+' , Linhas) ';
  fsECF.LeituraMFDSerial( COOInicial, COOFinal, Linhas, Documentos ) ;

  Linhas.Text := DecodificarPaginaDeCodigoECF( Linhas.Text );
end;

procedure TACBrECF.LeituraMFDSerial(DataInicial, DataFinal: TDateTime;
  NomeArquivo: String; Documentos: TACBrECFTipoDocumentoSet);
 Var AStringList : TStringList ;
begin
  TestaSeE_MFD ;

  ComandoLOG := 'LeituraMFDSerial( '+DateToStr(DataInicial)+' , '+
                    DateToStr(DataFinal)+' , '+NomeArquivo+' ) ';
  AStringList := TStringList.Create ;
  try
     LeituraMFDSerial( DataInicial, DataFinal, AStringList, Documentos ) ;

     AStringList.SaveToFile(NomeArquivo);
  finally
     AStringList.Free ;
  end ;
end;

procedure TACBrECF.LeituraMFDSerial(COOInicial, COOFinal: Integer;
  NomeArquivo: String; Documentos: TACBrECFTipoDocumentoSet);
 Var AStringList : TStringList ;
begin
  TestaSeE_MFD ;

  ComandoLOG := 'LeituraMFDSerial( '+IntToStr(COOInicial)+' , '+
                    IntToStr(COOFinal)+' , '+NomeArquivo+' ) ';
  AStringList := TStringList.Create ;
  try
     LeituraMFDSerial( COOInicial, COOFinal, AStringList, Documentos ) ;

     AStringList.SaveToFile(NomeArquivo);
  finally
     AStringList.Free ;
  end ;
end;

procedure TACBrECF.EspelhoMFD_DLL(DataInicial, DataFinal: TDateTime;
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
begin
  TestaSeE_MFD ;

  ComandoLOG := 'EspelhoMFD_DLL( '+DateToStr(DataInicial)+' , '+
                    DateToStr(DataFinal)+' , '+NomeArquivo+' ) ';
  fsECF.EspelhoMFD_DLL( DataInicial, DataFinal, NomeArquivo, Documentos ) ;
end;

procedure TACBrECF.EspelhoMFD_DLL(COOInicial, COOFinal: Integer;
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
begin
  TestaSeE_MFD ;

  ComandoLOG := 'EspelhoMFD_DLL( '+IntToStr(COOInicial)+' , '+
                    IntToStr(COOFinal)+' , '+NomeArquivo+' ) ';
  fsECF.EspelhoMFD_DLL( COOInicial, COOFinal, NomeArquivo, Documentos ) ;
end;

procedure TACBrECF.ArquivoMFD_DLL(DataInicial, DataFinal: TDateTime;
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD);
begin
  TestaSeE_MFD ;

  ComandoLOG := 'ArquivoMFD_DLL( '+DateToStr(DataInicial)+' , '+
                    DateToStr(DataFinal)+' , '+NomeArquivo+' ) ';
  fsECF.ArquivoMFD_DLL( DataInicial, DataFinal, NomeArquivo, Documentos, Finalidade ) ;
end;

procedure TACBrECF.ArquivoMFD_DLL(ContInicial, ContFinal: Integer;
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD; TipoContador: TACBrECFTipoContador);
begin
  TestaSeE_MFD ;

  ComandoLOG := 'ArquivoMFD_DLL( '+IntToStr(ContInicial)+' , '+
                    IntToStr(ContFinal)+' , '+NomeArquivo+' ) ';
  fsECF.ArquivoMFD_DLL( ContInicial, ContFinal, NomeArquivo, Documentos, Finalidade, TipoContador ) ;
end;

procedure TACBrECF.ArquivoMF_Binario_DLL(NomeArquivo: AnsiString);
begin
   TestaSeE_MFD ;

   ComandoLOG := 'ArquivoMF_Binario_DLL( ' + NomeArquivo +' ) ';
   fsECF.ArquivoMF_Binario_DLL(NomeArquivo) ;
end;

procedure TACBrECF.ArquivoMFD_Binario_DLL(NomeArquivo: AnsiString);
begin
  TestaSeE_MFD ;

  ComandoLOG := 'ArquivoMFD_Binario_DLL( ' + NomeArquivo +' ) ';
  fsECF.ArquivoMFD_Binario_DLL( tdmfdTotal, NomeArquivo, '', '') ;
end;

procedure TACBrECF.ArquivoMFD_Binario_DLL(NomeArquivo: AnsiString; DataInicial,
  DataFinal: TDateTime);
var
  StrInicial, StrFinal: String;
begin
  TestaSeE_MFD ;

  StrInicial :=  FormatDateTime('ddmmyyyy', DataInicial);
  StrFinal   :=  FormatDateTime('ddmmyyyy', DataFinal);

  ComandoLOG := 'ArquivoMFD_Binario_DLL( ' + NomeArquivo + ', '+
                StrInicial+', '+StrFinal+' ) ';

  fsECF.ArquivoMFD_Binario_DLL( tdmfdData, NomeArquivo, StrInicial, StrFinal) ;
end;

procedure TACBrECF.ArquivoMFD_Binario_DLL(NomeArquivo: AnsiString; COOInicial,
  COOFinal: Integer);
var
  StrInicial, StrFinal: String;
begin
  TestaSeE_MFD ;

  StrInicial :=  IntToStrZero(COOInicial, 6);
  StrFinal   :=  IntToStrZero(COOFinal, 6);

  ComandoLOG := 'ArquivoMFD_Binario_DLL( ' + NomeArquivo + ', '+
                StrInicial+', '+StrFinal+' ) ';

  fsECF.ArquivoMFD_Binario_DLL( tdmfdCOO, NomeArquivo, StrInicial, StrFinal) ;
end;

procedure TACBrECF.ImprimeCheque(Banco: String; Valor: Double; Favorecido,
  Cidade: String; Data: TDateTime; Observacao: String);
begin
  Observacao := TrimRight(Observacao) ;
  { Tirando os #13 e #10 }
  Observacao := StringReplace(Observacao,CR,'',[rfReplaceAll]) ;
  Observacao := StringReplace(Observacao,LF,'',[rfReplaceAll]) ;
  Valor      := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }

  ComandoLOG := 'ImprimeCheque( '+Banco+' , '+FloatToStr(Valor)+' , '+
                    Favorecido+' , '+Cidade+' , '+DateToStr(Data)+' , '+
                    Observacao+' )';
  fsECF.ImprimeCheque( Banco, Valor, Favorecido, Cidade, Data, Observacao );
end;

function TACBrECF.LeituraCMC7 : AnsiString ;
begin
  ComandoLOG := 'LeituraCMC7';
  Result := fsECF.LeituraCMC7 ;
end;

procedure TACBrECF.LeituraX;
Var
  wRespostaComando : AnsiString ;
  Tratado          : Boolean;
begin
  if RFDAtivo then
     fsRFD.VerificaParametros ;

  ComandoLOG := 'LeituraX' ;
  if Assigned( fOnAntesLeituraX ) then
     fOnAntesLeituraX(Self);

  try
    Tratado := False;
    fsECF.LeituraX ;
  except
     if Assigned( fOnErrorLeituraX ) then
        fOnErrorLeituraX(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'leiturax' ;
      MemoAdicionaCabecalho ;
      MemoTitulo('LEITURA X');
      MemoAdicionaLinha( fsMemoRodape );
   end ;
  {$ENDIF}

  if RFDAtivo then
  begin
     wRespostaComando := fsECF.RespostaComando ;
     fsRFD.Documento('LX') ;
     fsECF.RespostaComando := wRespostaComando ;
  end ;

  if Assigned( fOnDepoisLeituraX ) then
     fOnDepoisLeituraX(Self);

end;

procedure TACBrECF.LeituraXSerial(Linhas: TStringList);
begin
  ComandoLOG := 'LeituraXSerial( Linhas )' ;
  fsECF.LeituraXSerial( Linhas ) ;

  Linhas.Text := DecodificarPaginaDeCodigoECF( Linhas.Text );
end;

procedure TACBrECF.LeituraXSerial(NomeArquivo: String);
 Var AStringList : TStringList ;
begin
  ComandoLOG := 'LeituraXSerial( '+NomeArquivo+' )' ;
  AStringList := TStringList.Create ;
  try
     LeituraXSerial( AStringList );
     AStringList.SaveToFile( NomeArquivo );
  finally
     AStringList.Free ;
  end ;
end;


procedure TACBrECF.MudaHorarioVerao;
begin
  ComandoLOG := 'MudaHorarioVerao' ;
  fsECF.MudaHorarioVerao ;
end;

procedure TACBrECF.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  ComandoLOG := 'MudaHorarioVerao( '+BoolToStr(EHorarioVerao)+' )' ;
  fsECF.MudaHorarioVerao(EHorarioVerao) ;
end;

procedure TACBrECF.MudaArredondamento(Arredondar: Boolean);
begin
  ComandoLOG := 'MudaArredondamento( '+BoolToStr(Arredondar)+' )' ;
  fsECF.MudaArredondamento(Arredondar) ;
end;

procedure TACBrECF.PreparaTEF;
begin
  ComandoLOG := 'PreparaTEF' ;
  fsECF.PreparaTEF ;
end;

procedure TACBrECF.ReducaoZ(DataHora: TDateTime);
Var
  RedZ    : AnsiString ;
  Est     : TACBrECFEstado ;
  Tratado : Boolean;
begin
  ComandoLOG := 'ReducaoZ( '+DateTimeToStr(DataHora)+' )' ;

  Est := Estado ;
  if RFDAtivo then
  begin
     fsRFD.VerificaParametros ;
     RedZ := DadosReducaoZ ;  { Salva antes, pois alguns ECFs zeram valores após a Z }
  end ;

  ComandoLOG := 'ReducaoZ( '+DateTimeToStr(DataHora)+' )' ;

  try
     if Assigned( fOnAntesReducaoZ ) then
        fOnAntesReducaoZ(Self);

     try
       Tratado := False;
       fsECF.ReducaoZ( DataHora ) ;
     except
        if Assigned( fOnErrorReducaoZ ) then
           fOnErrorReducaoZ(Tratado);

        if not Tratado then
           raise;
     end;
  finally
      if Est <> Estado then
      begin
        {$IFNDEF NOGUI}
         if MemoAssigned then
         begin
            fsMemoOperacao := 'reducaoz' ;
            MemoAdicionaCabecalho ;
            MemoTitulo('REDUCAO Z');
            MemoAdicionaLinha( fsMemoRodape );
         end ;
        {$ENDIF}

        if RFDAtivo then
           fsRFD.ReducaoZ( RedZ ) ;
      end ;
  end ;

  if Assigned( fOnDepoisReducaoZ ) then
     fOnDepoisReducaoZ(Self);

end;

function TACBrECF.GetAliquotasClass: TACBrECFAliquotas;
begin
  ComandoLOG := 'Aliquotas' ;
  Result := fsECF.Aliquotas ;
end;

procedure TACBrECF.CarregaAliquotas;
begin
  ComandoLOG := 'CarregaAliquotas' ;
  fsECF.CarregaAliquotas ;
end;

procedure TACBrECF.LerTotaisAliquota;
begin
  ComandoLOG := 'LerTotaisAliquota' ;
  fsECF.LerTotaisAliquota ;
end;

function TACBrECF.AchaICMSAliquota( var AliquotaICMS: String): TACBrECFAliquota;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'AchaICMSAliquota( '+AliquotaICMS+' )' ;
  Result := fsECF.AchaICMSAliquota( AliquotaICMS ) ;
end;

function TACBrECF.AchaICMSAliquota(Aliquota: Double; Tipo : Char ):
   TACBrECFAliquota;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'AchaICMSAliquota( '+FloatToStr(Aliquota)+' , '+Tipo+' )' ;
  Result := fsECF.AchaICMSAliquota( Aliquota, Tipo ) ;
end;

function TACBrECF.AchaICMSIndice(Indice: String): TACBrECFAliquota;
begin
  Indice := Trim( Indice );
  if ComandoLOG = '' then
     ComandoLOG := 'AchaICMSIndice( '+Indice+' )' ;
  Result := fsECF.AchaICMSIndice( Indice ) ;
end;

procedure TACBrECF.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String);
begin
  ComandoLOG := 'ProgramaAliquota( '+FloatToStr(Aliquota)+' , '+Tipo+' , '+
                    Posicao+' )';
  fsECF.ProgramaAliquota(Aliquota, Tipo, Posicao);
end;

procedure TACBrECF.CarregaTotalizadoresNaoTributados;
begin
  ComandoLOG := 'CarregaTotalizadoresNaoTributados' ;
  fsECF.CarregaTotalizadoresNaoTributados ;
end;

procedure TACBrECF.LerTotaisTotalizadoresNaoTributados;
begin
  ComandoLOG := 'LerTotaisTotalizadoresNaoTributados' ;
  fsECF.LerTotaisTotalizadoresNaoTributados ;
end;

function TACBrECF.AchaTotalizadorNaoTributadoIndice(Indice: String
  ): TACBrECFTotalizadorNaoTributado;
begin
  Indice := Trim( Indice );
  if ComandoLOG = '' then
     ComandoLOG := 'AchaTotalizadorNaoTributadoIndice( '+Indice+' )' ;
  Result := fsECF.AchaTotalizadorNaoTributadoIndice( Indice ) ;
end;

function TACBrECF.SomaTotalizadorNaoTributadoIndice(Indice: String): Double;
begin
  Result := fsECF.SomaTotalizadorNaoTributadoIndice(Trim(Indice));
end;

function TACBrECF.GetFormasPagamentoClass: TACBrECFFormasPagamento;
begin
  ComandoLOG := 'FormasPagamento' ;
  Result := fsECF.FormasPagamento ;
end;

procedure TACBrECF.CarregaFormasPagamento;
var
   I : Integer ;
begin
  ComandoLOG := 'CarregaFormasPagamento' ;
  fsECF.CarregaFormasPagamento ;

  For I := 0 to FormasPagamento.Count-1 do
    FormasPagamento[I].Descricao := DecodificarPaginaDeCodigoECF(
      FormasPagamento[I].Descricao );
end;

procedure TACBrECF.LerTotaisFormaPagamento;
begin
  ComandoLOG := 'LerTotaisFormaPagamento' ;
  fsECF.LerTotaisFormaPagamento ;
end;

function TACBrECF.AchaFPGDescricao(Descricao: String; BuscaExata: Boolean;
  IgnorarCase: Boolean; IgnorarAcentos: Boolean): TACBrECFFormaPagamento;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'AchaFPGDescricao( '+Descricao+', '+BoolToStr(BuscaExata)+', '+
                   BoolToStr(IgnorarCase)+', ' +BoolToStr(IgnorarAcentos)+' )';
  Result := fsECF.AchaFPGDescricao( Descricao, BuscaExata, IgnorarCase, IgnorarAcentos  ) ;
end;

function TACBrECF.AchaFPGIndice(Indice: String): TACBrECFFormaPagamento;
begin
  Indice := Trim( Indice );
  if ComandoLOG = '' then
     ComandoLOG := 'AchaFPGIndice( '+Indice+' )' ;
  Result := fsECF.AchaFPGIndice( Indice ) ;
end;

procedure TACBrECF.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String);
begin
  ComandoLOG := 'ProgramaFormaPagamento( '+Descricao+' , '+
                    BoolToStr(PermiteVinculado)+' , '+Posicao+' )' ;

  Descricao := CodificarPaginaDeCodigoECF( Descricao );
  fsECF.ProgramaFormaPagamento( Descricao, PermiteVinculado, Posicao);
end;

function TACBrECF.GetRelatoriosGerenciaisClass: TACBrECFRelatoriosGerenciais;
begin
  ComandoLOG := 'RelatoriosGerenciais' ;
  Result := fsECF.RelatoriosGerenciais ;
end;

procedure TACBrECF.CarregaRelatoriosGerenciais;
var
   I : Integer ;
begin
  ComandoLOG := 'CarregaRelatoriosGerenciais' ;
  fsECF.CarregaRelatoriosGerenciais ;

  For I := 0 to RelatoriosGerenciais.Count-1 do
    RelatoriosGerenciais[I].Descricao := DecodificarPaginaDeCodigoECF(
      RelatoriosGerenciais[I].Descricao );
end;

procedure TACBrECF.LerTotaisRelatoriosGerenciais ;
begin
  ComandoLOG := 'LerTotaisRelatoriosGerenciais' ;
  fsECF.LerTotaisRelatoriosGerenciais ;
end ;

function TACBrECF.AchaRGDescricao(Descricao: String; BuscaExata: Boolean;
   IgnorarCase : Boolean ): TACBrECFRelatorioGerencial;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'AchaRGDescricao( '+Descricao+', '+BoolToStr(BuscaExata)+', '+
                                                   BoolToStr(IgnorarCase)+' )' ;
  Result := fsECF.AchaRGDescricao( Descricao, BuscaExata, IgnorarCase ) ;
end;

function TACBrECF.AchaRGIndice(
  Indice: String): TACBrECFRelatorioGerencial;
begin
  Indice := Trim( Indice );
  if ComandoLOG = '' then
     ComandoLOG := 'AchaRGIndice( '+Indice+' )' ;
  Result := fsECF.AchaRGIndice( Indice ) ;
end;


procedure TACBrECF.ProgramaRelatoriosGerenciais(var Descricao: String;
  Posicao: String);
begin
  ComandoLOG := 'ProgramaRelatoriosGerenciais( '+Descricao+' , '+ Posicao+' )';
  Descricao := CodificarPaginaDeCodigoECF( Descricao ) ;
  fsECF.ProgramaRelatorioGerencial( Descricao, Posicao);
end;


function TACBrECF.GetComprovantesNaoFiscaisClass: TACBrECFComprovantesNaoFiscais;
begin
  ComandoLOG := 'ComprovantesNaoFiscais' ;
  Result := fsECF.ComprovantesNaoFiscais ;
end;

procedure TACBrECF.CarregaComprovantesNaoFiscais;
var
   I : Integer ;
begin
  ComandoLOG := 'CarregaComprovantesNaoFiscais' ;
  fsECF.CarregaComprovantesNaoFiscais ;

  For I := 0 to ComprovantesNaoFiscais.Count-1 do
    ComprovantesNaoFiscais[I].Descricao := DecodificarPaginaDeCodigoECF(
       ComprovantesNaoFiscais[I].Descricao );
end;

procedure TACBrECF.LerTotaisComprovanteNaoFiscal;
begin
  ComandoLOG := 'LerTotaisComprovanteNaoFiscal' ;
  fsECF.LerTotaisComprovanteNaoFiscal ;
end;

function TACBrECF.AchaCNFDescricao( Descricao: String; BuscaExata : Boolean;
   IgnorarCase : Boolean ): TACBrECFComprovanteNaoFiscal;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'AchaCNFDescricao( '+Descricao+', '+BoolToStr(BuscaExata)+', '+
                                                    BoolToStr(IgnorarCase)+' )' ;
  Result := fsECF.AchaCNFDescricao( Descricao, BuscaExata, IgnorarCase ) ;
end;

function TACBrECF.AchaCNFIndice(
  Indice: String): TACBrECFComprovanteNaoFiscal;
begin
  Indice := Trim( Indice );
  if ComandoLOG = '' then
     ComandoLOG := 'AchaCNFIndice( '+Indice+' )' ;
  Result := fsECF.AchaCNFIndice( Indice ) ;
end;

function TACBrECF.AchaCNFFormaPagamento(
  CodFPG: String): TACBrECFComprovanteNaoFiscal;
begin
  CodFPG := Trim( CodFPG );
  ComandoLOG := 'AchaCNFFormaPagamento( '+CodFPG+' )' ;
  Result := fsECF.AchaCNFFormaPagamento( CodFPG ) ;
end;

procedure TACBrECF.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String);
begin
  ComandoLOG := 'ProgramaComprovanteNaoFiscal( '+Descricao+' , '+Tipo+' , '+
                    Posicao+' )';
  Descricao := CodificarPaginaDeCodigoECF( Descricao ) ;
  fsECF.ProgramaComprovanteNaoFiscal( Descricao, Tipo, Posicao);
end;


function TACBrECF.AchaUMDDescricao(
  Descricao: String): TACBrECFUnidadeMedida;
begin
  if ComandoLOG = '' then
     ComandoLOG := 'AchaUMDDescricao( '+Descricao+' )' ;
  Result := fsECF.AchaUMDDescricao( Descricao ) ;
end;

function TACBrECF.AchaUMDIndice(Indice: String): TACBrECFUnidadeMedida;
begin
  Indice := Trim( Indice );
  if ComandoLOG = '' then
     ComandoLOG := 'AchaUMDIndice( '+Indice+' )' ;
  Result := fsECF.AchaUMDIndice( Indice ) ;
end;

procedure TACBrECF.CarregaUnidadesMedida;
var
   I : Integer ;
begin
  ComandoLOG := 'CarregaUnidadesMedida' ;
  fsECF.CarregaUnidadesMedida ;

  For I := 0 to UnidadesMedida.Count-1 do
    UnidadesMedida[I].Descricao := DecodificarPaginaDeCodigoECF(
      UnidadesMedida[I].Descricao );
end;

function TACBrECF.GetUnidadesMedidaClass: TACBrECFUnidadesMedida;
begin
  ComandoLOG := 'UnidadesMedida' ;
  Result := fsECF.UnidadesMedida ;
end;

procedure TACBrECF.ProgramaUnidadeMedida(var Descricao: String);
begin
  ComandoLOG := 'ProgramaUnidadeMedida( '+Descricao+' )';
  Descricao := CodificarPaginaDeCodigoECF(Descricao) ;
  fsECF.ProgramaUnidadeMedida( Descricao );
end;

procedure TACBrECF.RelatorioGerencial(Relatorio: TStrings; const Vias: Integer;
  const Indice: Integer);
begin
  ComandoLOG := 'RelatorioGerencial( ' + Relatorio.Text + ' , ' +
                    IntToStr(Vias) + ' , ' + IntToStr(indice) + ' )' ;
  fsECF.RelatorioGerencial( Relatorio, Vias, Indice ) ;
end;

procedure TACBrECF.AbreRelatorioGerencial(Indice: Integer);
Var
  Tratado : Boolean;
begin
  ComandoLOG := 'AbreRelatorioGerencial' ;
  if RFDAtivo then
     fsRFD.VerificaParametros ;

  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  fsIndiceGerencial := Indice;

  if Assigned( fOnAntesAbreRelatorioGerencial ) then
     fOnAntesAbreRelatorioGerencial( Indice);

  try
    Tratado := False;
    fsECF.AbreRelatorioGerencial(Indice) ;
  except
     if Assigned( FOnErrorAbreRelatorioGerencial ) then
        FOnErrorAbreRelatorioGerencial(Tratado, Indice);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'abrerelatoriogerencial' ;
      MemoAdicionaCabecalho ;
      MemoTitulo('RELATORIO GERENCIAL');
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.Documento('RG') ;

  if Assigned( FOnDepoisAbreRelatorioGerencial ) then
     FOnDepoisAbreRelatorioGerencial( Indice);

end;

function TACBrECF.DecodificarTagsFormatacao(AString : AnsiString ;
   CodificarPaginaDeCodigo : Boolean) : AnsiString ;
begin
  if CodificarPaginaDeCodigo then
     Result := CodificarPaginaDeCodigoECF( AString )
  else
     Result := AString;

  Result := fsTagProcessor.DecodificarTagsFormatacao(Result);
end;

procedure TACBrECF.TraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString);
begin
  {*************************************************

    TAGS ACEITAS
    ============
      </linha_simples>    - ------------------...
      </linha_dupla>      - ==================...
      <e></e>             - Expandido
      <n></n>             - Negrito
      <s></s>             - Sublinhado
      <c></c>             - Condensado
      <i></i>             - Itálico
      </fn>               - Fonte Normal
      <ad></ad>           - Alinhado a direita
      <ae></ae>           - Alinhado a esquerda
      <ce></ce>           - centralizado

      <ean8></ean8>       - ean 8
      <ean13></ean13>     - ean 13
      <std></std>         - standart
      <inter></inter>     - interleave
      <code11></code11>   - code 11
      <code39></code39>   - code 39
      <code93></code93>   - code 93
      <code128></code128> - code 128
      <upca></upca>       - upca
      <codabar></codabar> - codabar
      <msi></msi>         - msi

  *************************************************}

  if not IgnorarTagsFormatacao then
    TagTraduzida := fsECF.TraduzirTag( ATag )
  else
    TagTraduzida := '';

  if TagTraduzida = '' then
  begin
    if ATag = cTagLinhaSimples then
      TagTraduzida := StringOfChar('-', Colunas)
    else if ATag = cTagLinhaDupla then
      TagTraduzida := StringOfChar('=', Colunas)
    else if ATag = cTagPuloDeLinhas then
      TagTraduzida := StringOfChar(#10, LinhasEntreCupons);
  end ;
end ;

procedure TACBrECF.TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
  var BlocoTraduzido: AnsiString);
begin
  if not IgnorarTagsFormatacao then
    BlocoTraduzido := fsECF.TraduzirTagBloco( ATag, ConteudoBloco )
  else
    BlocoTraduzido := ConteudoBloco;

  if ConteudoBloco = BlocoTraduzido then  // Não traduziu...
  begin
    if ATag = cTagAlinhadoDireita then
      BlocoTraduzido := PadLeft( ConteudoBloco, Colunas )
    else if ATag = cTagAlinhadoEsquerda then
      BlocoTraduzido := PadRight( ConteudoBloco, Colunas )
    else if ATag = cTagAlinhadoCentro then
      BlocoTraduzido := PadCenter( ConteudoBloco, Colunas );
  end ;
end ;

function TACBrECF.CodificarPaginaDeCodigoECF(ATexto: String): AnsiString;
begin
   Result := fsECF.CodificarPaginaDeCodigoECF( ATexto )
end;

function TACBrECF.DecodificarPaginaDeCodigoECF(ATexto : AnsiString) : String ;
begin
  Result := fsECF.DecodificarPaginaDeCodigoECF( ATexto )
end ;

procedure TACBrECF.LinhaRelatorioGerencial(const Linha: AnsiString;
  const IndiceBMP: Integer);
Var
  Texto, Buffer : AnsiString ;
  Pos : Integer ;

  Procedure TentaImprimirLinhas( Texto: AnsiString; IndiceBMP: Integer )  ;
  var
     Est : TACBrECFEstado ;
     OldTimeOut : LongInt ;
     Erro : String ;
  begin
     ComandoLOG := 'LinhaRelatorioGerencial( "'+Texto+'", '+IntToStr(IndiceBMP)+' )';

     try
        fsECF.LinhaRelatorioGerencial( Texto, IndiceBMP ) ;
     except
       On E : Exception do
       begin
         Erro := E.Message ;

         // Não conseguiu imprimir ? Verifique se o relatório foi fechado pelo ECF //
         OldTimeOut := TimeOut;
         TimeOut    := max(TimeOut,5);  // Tenta ler o Estado por 5 seg ou mais
         Est        := estDesconhecido;
         try
           Est := Estado;              // Lendo o estado do ECF

           if Est = estLivre then
           begin
             // Está Livre, provavelmente foi fechado por longo tempo de
             // impressao... (O ECF é obrigado a fechar o Gerencial após 2
             // minutos de Impressão). Vamos abrir um Novo Gerencial e Tentar
             // novamente
             AbreRelatorioGerencial(fsIndiceGerencial);
             fsECF.LinhaRelatorioGerencial( Texto, IndiceBMP );
           end ;
         finally
           TimeOut := OldTimeOut;

           if Est <> estLivre then
              raise EACBrECFErro.Create( ACBrStrToAnsi(Erro) );

         end ;
       end ;
     end ;
  end ;

begin
  if MaxLinhasBuffer < 1 then
   begin
     ComandoLOG := 'LinhaRelatorioGerencial( "'+Linha+'", '+IntToStr(IndiceBMP)+' )';
     fsECF.LinhaRelatorioGerencial( DecodificarTagsFormatacao( Linha ), IndiceBMP ) ;
   end
  else
   begin
     Texto  := '' ;
     Buffer := DecodificarTagsFormatacao( Linha );
     Buffer := AjustaLinhas(Buffer, Colunas) ;

     while Buffer <> '' do
     begin
       Pos := PosAt(#10, Buffer, MaxLinhasBuffer );
       if Pos < 1 then
         Pos := Length(Buffer);

       Texto := Copy(Buffer, 1, Pos);
       if Length(Texto) > 0 then
         TentaImprimirLinhas( Texto, IndiceBMP ) ;

       Buffer := Copy(Buffer, Pos+1, Length(Buffer) );
     end;
   end ;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'linharelatoriogerencial' ;
      Buffer := AjustaLinhas( Linha, fsMemoColunas) ;
      MemoAdicionaLinha( Buffer );
   end ;
  {$ENDIF}
end;

procedure TACBrECF.CupomVinculado(COO, CodFormaPagto: String;
  Valor: Double; Relatorio: TStrings; Vias: Integer);
begin
  Valor := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }
  CodFormaPagto := Trim( CodFormaPagto );

  ComandoLOG := 'CupomVinculado( '+COO+' , '+CodFormaPagto+' , '+
                    FloatToStr(Valor)+' , '+Relatorio.Text+' , '+
                    IntToStr(Vias)+' )';
  fsECF.CupomVinculado( COO, CodFormaPagto, '' , Valor, Relatorio, Vias );
end;

procedure TACBrECF.CupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double; Relatorio: TStrings;
  Vias: Integer);
begin
  Valor := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }
  CodFormaPagto := Trim( CodFormaPagto );

  ComandoLOG := 'CupomVinculado( '+COO+' , '+CodFormaPagto+' , '+
                    CodComprovanteNaoFiscal+' , '+
                    FloatToStr(Valor)+' , '+Relatorio.Text+' , '+
                    IntToStr(Vias)+' )';
  fsECF.CupomVinculado( COO, CodFormaPagto, CodComprovanteNaoFiscal, Valor,
                        Relatorio, Vias );
end;

procedure TACBrECF.SegundaViaVinculado;
begin
  ComandoLOG := 'SegundaViaVinculado()';
  fsECF.SegundaViaVinculado;
end;

procedure TACBrECF.ReimpressaoVinculado;
begin
  ComandoLOG := 'ReimpressaoVinculado()';
  fsECF.ReimpressaoVinculado;
end;

procedure TACBrECF.AbreCupomVinculado(COO, CodFormaPagto: String;
   Valor: Double);
Var
  Tratado : Boolean;
begin
  Valor := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }
  CodFormaPagto := Trim( CodFormaPagto );

  ComandoLOG := 'AbreCupomVinculado( '+COO+' , '+CodFormaPagto+' , '+
                    FloatToStr(Valor)+' )';

  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  if Assigned( fOnAntesAbreCupomVinculado ) then
     fOnAntesAbreCupomVinculado(Self);

  try
    Tratado := False;
    fsECF.AbreCupomVinculado( COO, CodFormaPagto, '', Valor);
  except
     if Assigned( fOnErrorAbreCupomVinculado ) then
        fOnErrorAbreCupomVinculado(Tratado);

     if not Tratado then
        raise;
  end;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'abrecupomvinculado' ;
      MemoAdicionaCabecalho ;
      MemoTitulo('CUPOM VINCULADO');
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.Documento('CC') ;

  if Assigned( fOnDepoisAbreCupomVinculado ) then
     fOnDepoisAbreCupomVinculado(Self);

end;

procedure TACBrECF.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
begin
  Valor := RoundTo( Valor, -2) ;  { Ajustando valores acima de 2 Decimais }
  CodFormaPagto := Trim( CodFormaPagto );
  CodComprovanteNaoFiscal := Trim( CodComprovanteNaoFiscal );

  ComandoLOG := 'AbreCupomVinculado( '+COO+' , '+CodFormaPagto+' , '+
                    CodComprovanteNaoFiscal+' , '+FloatToStr(Valor)+' )';
  fsECF.AbreCupomVinculado( COO, CodFormaPagto, CodComprovanteNaoFiscal, Valor);

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'abrecupomvinculado' ;
      MemoAdicionaCabecalho ;
      MemoTitulo('CUPOM VINCULADO');
   end ;
  {$ENDIF}

  if RFDAtivo then
     fsRFD.Documento('CC') ;
end;

procedure TACBrECF.LinhaCupomVinculado(const Linha: AnsiString);
Var
  Texto, Buffer : AnsiString ;
  Lin   : Integer ;
  SL    : TStringList ;
begin
  if MaxLinhasBuffer < 1 then
   begin
     ComandoLOG := 'LinhaCupomVinculado( '+Linha+' )';
     fsECF.LinhaCupomVinculado( DecodificarTagsFormatacao( Linha ) )
   end
  else
   begin
     Texto  := '' ;
     Buffer := DecodificarTagsFormatacao( Linha );
     Buffer := AjustaLinhas(Buffer, Colunas) ;
     SL     := TStringList.Create ;
     try
        SL.Text := Buffer ;

        For Lin := 0 to SL.Count - 1 do
        begin
           Texto := Texto + SL[Lin] + sLineBreak;
           if (Lin mod MaxLinhasBuffer) = 0 then
           begin
              ComandoLOG := 'LinhaCupomVinculado( '+Texto+' )';
              fsECF.LinhaCupomVinculado( Texto ) ;
              Texto := '' ;
           end ;
        end ;

        if Texto <> '' then
        begin
           ComandoLOG := 'LinhaCupomVinculado( '+Texto+' )';
           fsECF.LinhaCupomVinculado( Texto ) ;
        end ;
     finally
        SL.Free ;
     end ;
   end ;

  {$IFNDEF NOGUI}
   if MemoAssigned then
   begin
      fsMemoOperacao := 'linhacupomvinculado' ;
      Buffer := AjustaLinhas( Linha, fsMemoColunas) ;
      MemoAdicionaLinha( Buffer );
   end ;
  {$ENDIF}
end;


{------------------------------------------------------------------------------}
function TACBrECF.AcharECF( ProcuraModelo : Boolean; ProcuraPorta  : Boolean;
                            ATimeOut : Integer) : Boolean ;
Var I : Integer ;
begin
  Result := false ;
  if fsProcurandoECF then exit ; { para evitar chamadas recursivas }

  if not (ProcuraModelo or ProcuraPorta) then exit ; { nada a fazer }

  if ProcuraPorta then
     Porta := 'procurar' ;

  fsECF.GravaLog( 'AcharECF: '+ifthen(ProcuraModelo,' ProcuraModelo','')+
                               ifthen(ProcuraPorta,' ProcuraPorta','') );
  try
     fsProcurandoECF := true ;

     if not ProcuraModelo then
        Result := AcharPorta(ATimeOut)
     else
      begin
        For I := 2 to Integer(High(TACBrECFModelo)) do { Pula ecfNenhum, ecfNaoFiscal }
        begin
           try
              if not fsProcurandoECF then
                 break ;

              Modelo := TACBrECFModelo( I ) ;

              if AcharPorta( ATimeOut ) then
              begin
                 Result := true ;
                 Break ;
              end ;
           except
           end ;
        end ;
      end
  finally
     fsProcurandoECF := false ;
     Desativar ;
     if (not Result) and ProcuraModelo then
        Modelo := ecfNenhum ;
  end ;
end;

function TACBrECF.AcharPorta(ATimeOut : Integer): Boolean;
Var wPorta : String ;
    wTimeOut  : Integer ;
    wRetentar : Boolean ;
begin
  Result := false ;
  if fsProcurandoPorta then exit ;  { para evitar chamadas recursivas }

  fsECF.GravaLog( 'AcharPorta( '+IntToStr(ATimeOut)+' )' );

  if fsModelo = ecfNenhum then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFModeloNaoDefinidoException));

  if Modelo = ecfNaoFiscal then
     raise EACBrECFErro.Create( Format(ACBrStr(cACBrECFModeloBuscaPortaException),
                                   [ ModeloStr ])) ;

  wPorta   := Porta ;
  wTimeOut := TimeOut ;
  wRetentar:= ReTentar ;
  try
     fsProcurandoPorta := true ;
     ReTentar := false ;
     if ATimeOut <> 0 then
        TimeOut := ATimeOut ;

     {$IFNDEF NOGUI}
     {$IFNDEF FRAMEWORK}
       fsECF.FormMsgControla := False ;
       fsECF.FormMsgDoProcedure( DoAcharPorta, ord('C') ) ;
     {$ELSE}
       DoAcharPorta ;
     {$ENDIF}
     {$ELSE}
       DoAcharPorta ;
     {$ENDIF}

     if Porta = 'abortado' then
     begin
        Porta := '' ;
        fsProcurandoECF := false ;
     end ;

     Result := (Porta <> '') ;
  finally
     {$IFNDEF NOGUI}
     {$IFNDEF FRAMEWORK}
       fsECF.FormMsgControla := True ;
     {$ENDIF}
     {$ENDIF}
     fsProcurandoPorta := false ;
     TimeOut := wTimeOut ;
     Retentar:= wReTentar ;
     if not Result then
     begin
        Desativar ;
        Porta := wPorta ;
     end ;
  end ;
end;

{------------------------------------------------------------------------------}
procedure TACBrECF.DoAcharPorta ;
  Function AtivarECF : Boolean ;
    Var Msg : String ;

  begin
     Result := false ;
     try
        Desativar ;
        try
           Device.Limpar ;
        except
        end ;
        Msg := Format( ACBrStr(cACBrECFMsgDoAcharPorta), [ModeloStr, Porta] );
        if (ExibeMensagem or BloqueiaMouseTeclado) then
           Msg := Msg {$IFDEF VisualCLX}+sLineBreak{$ENDIF} +
                  ' Pressione <C> para cancelar' ;

        {$IFNDEF NOGUI}
        {$IFNDEF FRAMEWORK}
          fsECF.FormMsgPinta( Msg ) ;
          if not (fsECF.FormMsgEstado = fmsProcessando) then
             exit ;
        {$ENDIF}
        {$ENDIF}

        Ativar ;      { Se não Ativar gera uma Exceção }
        Result := true ;
     except
     end ;
  end ;

Var I : Integer ;
    Achou : Boolean ;
    SL : TStringList ;
begin
  Achou := false ;

  if (Porta = '') or (LowerCase(Porta) = 'procurar') then
   begin
     SL := TStringList.Create;
     try
        Device.AcharPortasSeriais( SL );

        I := 0 ;
        while (I < SL.Count) and (not Achou)
          {$IFNDEF NOGUI}{$IFNDEF FRAMEWORK} and (fsECF.FormMsgEstado = fmsProcessando) {$ENDIF}{$ENDIF} do
        begin
           Porta := SL[I] ;
           Achou := AtivarECF ;
           Inc( I ) ;
        end ;
     finally
        SL.Free ;
     end ;
   end
  else
     Achou := AtivarECF ;

  if not Achou then
  begin
     sleep(200) ;
     {$IFNDEF NOGUI}
     {$IFNDEF FRAMEWORK}
       if fsECF.FormMsgEstado = fmsAbortado then
          Porta := 'abortado'
       else
     {$ENDIF}
     {$ENDIF}
          Porta := '' ;
  end ;
end;

{$IFNDEF NOGUI}
{$IFNDEF FRAMEWORK}
 procedure TACBrECF.SetMemoBobina(const AValue: TMemo);
 begin
   if AValue <> fsMemoBobina then
   begin
      if Assigned(fsMemoBobina) then
         fsMemoBobina.RemoveFreeNotification(Self);

      fsMemoBobina := AValue;

      if AValue <> nil then
         AValue.FreeNotification(self);
   end ;
 end;
{$ENDIF}

 procedure TACBrECF.SetMemoParams(const AValue: TStrings);
 begin
   fsMemoParams.Assign( AValue );
 end;

 procedure TACBrECF.MemoAdicionaCabecalho;
 begin
   MemoLeParams;
   MemoAdicionaLinha( fsMemoCabecalho ) ;
   fsMemoItens     := 0 ;  { Zera contador de Itens }
   fsSubTotalPagto := 0;   {Zera o total pago verificador da bobina}
   fsTotalPago     := 0;
 end;

 procedure TACBrECF.MemoTitulo(ATitulo: String) ;
 begin
   MemoAdicionaLinha( '<h' +IntToStr(fsMemoHTMLTitleSise)+'><center>'+
                      ATitulo+'</center></h'+IntToStr(fsMemoHTMLTitleSise)+'>' ) ;
 end;

 procedure TACBrECF.MemoSubtotaliza(DescontoAcrescimo: Double );
 Var
    S : String ;
 begin
   fsMemoOperacao  := 'subtotalizanaofiscal' ;

   {Para que na condição de pagamento, não precise ficar pegando toda hora}
   {o subtotal, evitando ficar mandando para a impressora }
   if DescontoAcrescimo <> 0 then
   begin
      if DescontoAcrescimo < 0 then
         S := 'Desconto '
      else
         S := 'Acrescimo' ;

      MemoAdicionaLinha( '<table width=100%><tr>'+
                          '<td align=left><b>SUBTOTAL   R$</b></td>'+
                          '<td align=right>'+
                            FormatFloatBr(fsSubTotalPagto - DescontoAcrescimo,'#,###,##0.00')+
                          '</td>'+
                         '</tr></table>' ) ;
      MemoAdicionaLinha( '<table width=100%><tr>'+
                          '<td align=left><b>'+S+'  R$</b></td>'+
                          '<td align=right>'+
                            FormatFloatBr(DescontoAcrescimo,'#,###,##0.00')+
                          '</td>'+
                         '</tr></table>' ) ;
   end ;

   MemoTitulo( '<table width=100%><tr>'+
                '<td align=left>TOTAL  R$</td>'+
                '<td align=right>'+
                  FormatFloatBr(fsSubTotalPagto,'#,###,##0.00')+
                '</td>'+
               '</tr></table>' ) ;

   fsMemoItens := 0 ;  { Zera para acumular o numero de Pagamentos }
 end ;

 procedure TACBrECF.MemoEfetuaPagamento(Descricao: String; Valor: Double;
    Observacao: String) ;
 Var
    Troco : Double ;
 begin
    fsMemoOperacao := 'efetuapagamento' ;
    Inc( fsMemoItens ) ;

    MemoAdicionaLinha( '<table width=100%><tr>'+
                        '<td align=left><b>'+Descricao+'</b></td>'+
                        '<td align=right>'+
                         FormatFloatBr(Valor,'#,###,##0.00')+
                        '</td>'+
                       '</tr></table>' ) ;

    Observacao := AjustaLinhas( Observacao, fsMemoColunas, 2 ) ;
    MemoAdicionaLinha( Observacao );

    if fsTotalPago >= fsSubTotalPagto then   { Ultrapassou o Valor do Cupom }
    begin
       if fsMemoItens > 1 then
          MemoAdicionaLinha( '<table width=100%><tr>'+
                              '<td align=left><b>S O M A  R$</b></td>'+
                              '<td align=right>'+
                                FormatFloatBr(fsTotalPago,'#,###,##0.00')+
                              '</td>'+
                             '</tr></table>' ) ;

      if fsTotalPago > fsSubTotalPagto then  { Tem TROCO ? }
      begin
         Troco  := RoundTo(fsTotalPago - fsSubTotalPagto,-2) ;
         MemoTitulo( '<table width=100%><tr>'+
                      '<td align=left>TROCO  R$</td>'+
                      '<td align=right>'+
                        FormatFloatBr(Troco,'#,###,##0.00')+
                      '</td>'+
                     '</tr></table>' ) ;
      end ;
   end ;
 end ;


 procedure TACBrECF.MemoLeParams;
  Var I : Integer ;
      T, S : String ;
      INI : TMemIniFile ;
 begin
   INI := TMemIniFile.Create('acbrecfmemo.ini') ;
   try
      INI.SetStrings( fsMemoParams );

      fsMemoColunas       := INI.ReadInteger('Formato','Colunas', fsMemoColunas) ;
      fsMemoHTML          := INI.ReadBool('Formato','HTML', fsMemoHTML) ;
      fsMemoHTMLTitleSise := INI.ReadInteger('Formato','HTML_Title_Size',
                                             fsMemoHTMLTitleSise) ;
      fsMemoHTMLFont      := INI.ReadString('Formato','HTML_Font', fsMemoHTMLFont) ;
      fsMemoMascItens     := INI.ReadString('Cabecalho_Item','MascaraItem',
                                             fsMemoMascItens) ;

      fsMemoCabecalho := '' ;
      I := 0 ;
      while true do
      begin
         S := 'LIN'+IntToStrZero( I, 3) ;
         T := INI.ReadString('Cabecalho', S, '*FIM*') ;

         if T = '*FIM*' then break ;

         fsMemoCabecalho := fsMemoCabecalho + T + sLineBreak ;
         Inc( I ) ;
      end ;

      fsMemoRodape := '' ;
      I := 0 ;
      while true do
      begin
         S := 'LIN'+IntToStrZero( I, 3) ;
         T := INI.ReadString('Rodape', S, '*FIM*') ;

         if T = '*FIM*' then break ;

         fsMemoRodape := fsMemoRodape + T + sLineBreak ;
         Inc( I ) ;
      end ;

      fsMemoCabItens := '' ;
      I := 0 ;
      while true do
      begin
         S := 'LIN'+IntToStrZero( I, 3) ;
         T := INI.ReadString('Cabecalho_Item', S, '*FIM*') ;

         if T = '*FIM*' then break ;

         fsMemoCabItens := fsMemoCabItens + T + sLineBreak ;
         Inc( I ) ;
      end ;
   finally
      INI.Free ;
   end ;
 end;

 procedure TACBrECF.MemoAdicionaLinha(Linhas: String);
  Var SL : TStringList ;
      I,P : Integer ;
      NewLinhas,L,L2  : String ;
      InHTML, Centraliza, Tabela : Boolean ;
 begin
   if Linhas = '' then exit ;

   Linhas := StringReplace(Linhas,CR+LF,#10,[rfReplaceAll]) ;
   Linhas := StringReplace(Linhas,sLineBreak,#10,[rfReplaceAll]) ;
   Linhas := StringReplace(Linhas,#10,sLineBreak,[rfReplaceAll]) ;

   Linhas := MemoTraduzCode( Linhas ) ;

   { Processando linha a linha conforme o valor de fsMemoHTML }
   NewLinhas := '' ;
   SL := TStringList.Create ;
   try
      SL.Text := Linhas ;

      For I := 0 to SL.Count-1 do
      begin
         L := SL[I] ;

         if fsMemoHTML then
          begin
            if pos(LowerCase(RightStr(L,9)),'</center></left></right></table><br><hr>') = 0 then
               L := L + '<br>' ;

            { Html não respeita espaços grandes }
            L := StringReplace(L,'  ','&nbsp;&nbsp;',[rfReplaceAll]) ;
            if LeftStr(L,1) = ' ' then
               L := '&nbsp;' + copy(L,2,Length(L)) ;
          end
         else
          begin
            if LowerCase( Trim(L) ) = '<hr>' then
               L2 := StringofChar('-',fsMemoColunas)
            else
             begin
               L          := StringReplace(L,'<br>',sLineBreak,[rfReplaceAll]) ;
               Centraliza := (pos('<center>',LowerCase(L)) > 0) ;  { é para centralizar ? }
               Tabela     := (pos('<tr>',LowerCase(L)) > 0) ;      { é Tabela }
               if Tabela then
               begin
                  L := StringReplace(L,'</td>','|',[rfReplaceAll]) ;
                  P := LastDelimiter('|',L) ; { Remove ultimo separador }
                  L := StuffString(L,P,1,'') ;
               end ;

               { Removendo todas as TAGs HTML }
               L2         := '' ;
               InHTML     := False ;
               For P := 1 to Length( L ) do
               begin
                  if not InHTML then
                   begin
                     if (L[P] = '<') and (PosEx('>',L,P) > 0)  then
                        InHTML := True
                     else
                        L2 := L2 + L[P] ;
                   end
                  else
                     if L[P] = '>' then
                        InHTML := False ;
               end ;

               if Tabela then
                  L2 := PadSpace( L2, fsMemoColunas,'|')
               else if Centraliza then
                  L2 := PadCenter( L2, fsMemoColunas) ;
             end ;

            L := L2 + sLineBreak ;
          end ;

         NewLinhas := NewLinhas + L  ;
      end ;

      if not fsMemoHTML then
       begin
         { Remove ultima quebra de linha (para não ficar um pulo a mais ) }
         if RightStr(NewLinhas,Length(sLineBreak)) = sLineBreak then
            NewLinhas := LeftStr(NewLinhas,Length(NewLinhas)-Length(sLineBreak)) ;
       end
      else
         { Insere comando de formatação de fonte }
         if fsMemoHTMLFont <> '' then
            NewLinhas := fsMemoHTMLFont + NewLinhas + '</font>' ;
   finally
      SL.Free ;
   end ;

   {$IFNDEF FRAMEWORK}
   if Assigned( fsMemoBobina ) then
      fsMemoBobina.Lines.Add( NewLinhas ) ;
   {$ENDIF}

   if Assigned( fsOnBobinaAdicionaLinhas ) then
      fsOnBobinaAdicionaLinhas( NewLinhas, fsMemoOperacao ) ;
 end;

function TACBrECF.MemoTraduzCode(Linha: String): String;
var P1, P2 : Integer ;
    Code, Buffer : AnsiString ;
begin
  while True do
  begin
     Buffer := LowerCase(Linha) ;
     P1     := pos('<code>',Buffer) ;
     P2     := pos('</code>',Buffer) ;

     if (P1 = 0) or (P2 < P1) then
        break ;

     Code := copy( Buffer, P1+6, P2-P1-6) ;

     if Code = 'data' then
        Code := FormatDateTime('dd/mm/yyyy',now)
     else if Code = 'hora' then
        Code := FormatDateTime('hh:nn:ss',now)
     else if Code = 'numcupom' then
        Code := NumCupom
     else if Code = 'numcoo' then
        Code := NumCOO
     else if Code = 'numserie' then
        Code := NumSerie
     else if Code = 'numseriemfd' then
        Code := NumSerieMFD
     else if Code = 'numecf' then
        Code := NumECF
     else if Code = 'acbr' then
        Code := ACBR_VERSAO ;

     Linha := copy(Linha,1,P1-1) + Code + copy(Linha,P2+7,Length(Linha)) ;
  end ;

  Result := Linha ;
end;

{$ENDIF}

procedure TACBrECF.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    {$IFNDEF NOGUI}
     {$IFNDEF FRAMEWORK}
      if (fsMemoBobina <> nil) and (AComponent is TMemo) then
      begin
         fsMemoBobina := nil ;
      end ;
     {$ENDIF}
    {$ENDIF}

    if (AComponent is TACBrRFD) and (fsRFD <> nil) then
       fsRFD := nil ;

    if (AComponent is TACBrEAD) and (fsEAD <> nil) then
       fsEAD := nil ;

    if (AComponent is TACBrAAC) and (fsAAC <> nil) then
       fsAAC := nil ;

    if (AComponent is TACBrECFVirtual) and (fsECFVirtual <> nil) then
       fsECFVirtual := nil ;
  end;
end;

procedure TACBrECF.DoVerificaValorGT ;
var
   ValorGT_ECF : Double ;
   Erro : Integer ;
begin
  if not Assigned( fsAAC ) then
     Exit ;

  if fsNumSerieCache = '' then
     fsNumSerieCache := NumSerie;

  if VerificarApenasNumeroSerieNoAAC then
  begin
    Erro := fsAAC.AchaIndiceECF(fsNumSerieCache);
  end
  else
  begin
    ValorGT_ECF := GrandeTotal ;
    Erro := fsAAC.VerificarGTECF( fsNumSerieCache, ValorGT_ECF );
  end;

  if Erro = -1 then
     raise EACBrAAC_NumSerieNaoEncontrado.Create( ACBrStr( Format(
           cACBrAACNumSerieNaoEncontardoException, [ fsNumSerieCache ] )) )
  else if Erro = -2 then
     raise EACBrAAC_ValorGTInvalido.Create( ACBrStr(
           cACBrAACValorGTInvalidoException ) );
end ;

procedure TACBrECF.DoAtualizarValorGT ;
Var
  ValorGT  : Double ;
begin
  if not Assigned( fsAAC ) then
     Exit ;

  if VerificarApenasNumeroSerieNoAAC then
  begin
    //Não é necessário atualizar GT se está verificando apenas o número de série;
    Exit;
  end;

  if fsNumSerieCache = '' then
     fsNumSerieCache := NumSerie;

  try
     ValorGT := GrandeTotal;
     fsAAC.AtualizarValorGT( fsNumSerieCache, ValorGT );
  except
     On E: EACBrECFSemPapel do
     begin
        Exit;
     end
     else
        raise;
  end ;
end ;

function TACBrECF.GetACBrEAD : TACBrEAD ;
begin
  if Assigned(fsEAD) then
     Result := fsEAD
  else
   begin
     if not Assigned( fsEADInterno ) then
     begin
        fsEADInterno := TACBrEAD.Create(Self);
        fsEADInterno.OnGetChavePrivada := fsOnPAFGetKeyRSA;
     end ;
     Result := fsEADInterno;
   end ;
end ;

procedure TACBrECF.SetECFVirtual(AValue: TACBrECFVirtual);
Var
  OldValue: TACBrECFVirtual ;
begin
  if fsAtivo then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFSetECFVirtualException));

  if AValue <> fsECFVirtual then
  begin
     if Assigned(fsECFVirtual) then
        fsECFVirtual.RemoveFreeNotification(Self);

     OldValue     := fsECFVirtual ;   // Usa outra variavel para evitar Loop Infinito
     fsECFVirtual := AValue;          // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.ECF) then
           OldValue.ECF := nil ;

     if AValue <> nil then
     begin
        AValue.FreeNotification(self);
        AValue.ECF := Self ;

        if Modelo = ecfECFVirtual then   // Atualizando ECF Virtual em Classe interna
          fsECF := fsECFVirtual.ECFVirtualClass;
     end ;
  end ;
end;

procedure TACBrECF.SetRFD(const AValue: TACBrRFD);
Var
  OldValue: TACBrRFD ;
begin
  if fsAtivo then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFSetRFDException));

  if AValue <> fsRFD then
  begin
     if Assigned(fsRFD) then
        fsRFD.RemoveFreeNotification(Self);

     OldValue := fsRFD ;   // Usa outra variavel para evitar Loop Infinito
     fsRFD    := AValue;   // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.ECF) then
           OldValue.ECF := nil ;

     if AValue <> nil then
     begin
        AValue.FreeNotification(self);
        AValue.ECF := self ;
     end ;
  end ;
end;

procedure TACBrECF.SetAAC(const AValue : TACBrAAC) ;
begin
  if fsAtivo then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFSetAACException));

  if AValue <> fsAAC then
  begin
     if Assigned(fsAAC) then
        fsAAC.RemoveFreeNotification(Self);

     fsAAC := AValue;

     if AValue <> nil then
        AValue.FreeNotification(self);
  end ;
end ;

procedure TACBrECF.SetEAD(const AValue : TACBrEAD) ;
begin
  if AValue <> fsEAD then
  begin
     if Assigned(fsEAD) then
        fsEAD.RemoveFreeNotification(Self);

     fsEAD := AValue;

     if AValue <> nil then
     begin
        AValue.FreeNotification(self);

        if Assigned( fsEADInterno ) then
           FreeAndNil( fsEADInterno );
     end ;
  end ;
end ;

function TACBrECF.RFDAtivo: Boolean;
begin
  Result := False ;
  if Assigned( fsRFD ) then
     Result := fsRFD.Ativo ;
end;

procedure TACBrECF.ArredondarPorQtd(var Qtd: Double; const ValorUnitario: Double;
       const Precisao : Integer = -2 );
Var Pot, AddVal : Double ;
    TotalRound, TotalTrunc : Integer ;
begin
  Pot    := Power(10, -Precisao) ;
  AddVal := 1 / Power(10,DecimaisQtd) ;

  TotalRound := Trunc(SimpleRoundTo( Qtd * ValorUnitario * Pot, 0)) ;
  TotalTrunc := TruncFix(Qtd * ValorUnitario * Pot)  ;

  while TotalTrunc < TotalRound do
  begin
     Qtd        := RoundTo(Qtd + AddVal, -DecimaisQtd) ;
     TotalTrunc := TruncFix(Qtd * ValorUnitario * Pot)  ;

   { Se passou do Valor Arredondado, é melhor deixar utimo valor (truncado)
     Isso pode ocorrer se o Preço Unitário é grande...
     nesses casos não há como ajustar... :-(                                }
     if TotalTrunc > TotalRound then
     begin
        Qtd := RoundTo(Qtd - AddVal, -DecimaisQtd) ;
        break ;
     end ;
  end ;
end;

function TACBrECF.AssinaArquivoComEAD(Arquivo: String): Boolean;
begin
  if Assigned( fsOnPAFCalcEAD ) then
     fsOnPAFCalcEAD( Arquivo )
  else
     GetACBrEAD.AssinarArquivoComEAD( Arquivo, True ) ;

  Result := True;
end;

procedure TACBrECF.IdentificaOperador(Nome: String);
begin
  ComandoLOG := 'IdentificaOperador('+Nome+')';

  fsECF.IdentificaOperador(Nome);
  fsECF.Operador := Nome ;
  fsIdentificarOperador := False ;
end;

procedure TACBrECF.IdentificaPAF(NomeVersao, MD5: String);
var
  MD5Texto: String;
  P : Integer ;
begin
  ComandoLOG := 'IdentificaPAF('+NomeVersao+' , '+MD5+')';

  if Trim(MD5) = '' then
    MD5Texto := ''
  else
  begin
    // Verificando se usuário já informou o pre-fixo "MD5", "MD5:" ou "MD-5"
    MD5Texto := UpperCase(MD5);
    P        := 1 ;
    if LeftStr(MD5Texto,5) = 'MD-5:' then
       P := 6
    else if LeftStr(MD5Texto,4) = 'MD-5:' then
       P := 5
    else if LeftStr(MD5Texto,3) = 'MD-5' then
       P := 4 ;

    // acertar para que saia o texto "MD-5" antes do numero
    MD5Texto := 'MD-5:' + copy(MD5, P, Length(MD5) );
  end ;

  try
     { Nota Legal, impede o uso de IdentificaPAF, pois o sufixo "NL" no MD5
       deve ser adicionado apenas se o Consumidor for identificado, o que
       ocorre após a abertura do Cupom.
       conforme requisito VIII-B item 6, somente para o DF }

     if not InfoRodapeCupom.NotaLegalDF.Imprimir  then
       fsECF.IdentificaPAF(NomeVersao, MD5Texto)
     else
     begin
       try
          fsECF.IdentificaPAF('','');  // Remove programação da memoria do ECF
       except
       end;
       InfoRodapeCupom.MD5 := MD5;
     end;
  except
     // Se não conseguiu programar os dados PAF-ECF,
     // usa o InfoRodapeCupom para imprimir o MD5
     InfoRodapeCupom.MD5 := MD5;
  end ;
end;

function TACBrECF.RetornaInfoECF(Registrador : String) : AnsiString ;
begin
  ComandoLOG := 'RetornaInfoECF('+Registrador+')';

  Result := fsECF.RetornaInfoECF( Registrador );
end;

function TACBrECF.DecodificaTexto(Operacao: Char; Texto: String;
  var Resposta: String): Boolean;
begin
   ComandoLOG := 'DecodificaTexto';
   Result := fsECF.DecodificaTexto(Operacao,Texto,Resposta) ;
end;

function TACBrECF.CalculaTotalItem(AValorUnitario: Double; AQtd: Double;
  ADecimais: Integer; FlagAT: Char): Double;
var
  TotalItem, Pot: Double;
  EhArredondamento: Boolean;
begin
  TotalItem := AValorUnitario * AQtd;

  case upcase(FlagAT) of
    'A': EhArredondamento := True;
    'T': EhArredondamento := False;
  else
     EhArredondamento := Arredonda or ArredondaItemMFD;
  end;

  if EhArredondamento then
  begin
    Result := RoundABNT(TotalItem, ADecimais)
  end
  else
  begin
    if ArredondaPorQtd then
    begin
      ArredondarPorQtd(AQtd, AValorUnitario, -ADecimais ); // Modifica AQtd
      TotalItem := AValorUnitario * AQtd;
    end ;

    Pot := Power(10, ADecimais) ;
    Result := TruncFix(TotalItem * Pot) / Pot;
  end;
end;

//*** Opcoes do menu fiscal do paf-ecf

procedure TACBrECF.PafMF_LX_Impressao;
begin
  Self.LeituraX;
end;

procedure TACBrECF.PafMF_LMFC_Impressao(const CRZInicial, CRZFinal: Integer);
begin
  Self.LeituraMemoriaFiscal(CRZInicial, CRZFinal, False);
end;

procedure TACBrECF.PafMF_LMFC_Impressao(const DataInicial,
  DataFinal: TDateTime);
begin
  Self.LeituraMemoriaFiscal(DataInicial, DataFinal, False);
end;

procedure TACBrECF.PafMF_LMFC_Espelho(const CRZInicial, CRZFinal: Integer;
  const PathArquivo: String);
begin
  Self.LeituraMemoriaFiscalSerial(CRZInicial, CRZFinal, PathArquivo, False);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_LMFC_Espelho(const DataInicial, DataFinal: TDateTime;
  const PathArquivo: String);
begin
  Self.LeituraMemoriaFiscalSerial(DataInicial, DataFinal, PathArquivo, False);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_LMFC_Cotepe1704(const CRZInicial, CRZFinal: Integer;
  const PathArquivo: String);
begin
  Self.ArquivoMFD_DLL(CRZInicial, CRZFinal, PathArquivo, [docTodos], finMF, tpcCRZ);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_GerarCAT52(const DataInicial, DataFinal: TDateTime;
  const DirArquivos: String);
begin
  { ATENÇÃO !!  Para geração de arquivos programas de cidadania, como Nota
    Fiscal Paulista, Nota Alogoana, etc.. se o seu ECF é MFD (termico), utilize
    o método  ** PafMF_MFD_Cotepe1704 **...
    O Layout de arquivo CAT52 foi criado pelo governo de SP, para ser utilizado
    somente nas seguintes situações:
    - ECF sem MFD (as antigas matriciais)
    - Geração do arquivo com base nas informações do Banco de Dados e não da MFD
    }

  fsECF.PafMF_GerarCAT52(DataInicial, DataFinal, IncludeTrailingPathDelimiter(DirArquivos));
end;

procedure TACBrECF.PafMF_LMFC_Cotepe1704(const DataInicial, DataFinal: TDateTime;
  const PathArquivo: String);
begin
  Self.ArquivoMFD_DLL(DataInicial, DataFinal, PathArquivo, [docTodos], finMF);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_LMFS_Impressao(const CRZInicial, CRZFinal: Integer);
begin
  Self.LeituraMemoriaFiscal(CRZInicial, CRZFinal, True);
end;

procedure TACBrECF.PafMF_LMFS_Impressao(const DataInicial,
  DataFinal: TDateTime);
begin
  Self.LeituraMemoriaFiscal(DataInicial, DataFinal, True);
end;

procedure TACBrECF.PafMF_LMFS_Espelho(const CRZInicial, CRZFinal: Integer;
  const PathArquivo: String);
begin
  Self.LeituraMemoriaFiscalSerial(CRZInicial, CRZFinal, PathArquivo, True);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_LMFS_Espelho(const DataInicial, DataFinal: TDateTime;
  const PathArquivo: String);
begin
  Self.LeituraMemoriaFiscalSerial(DataInicial, DataFinal, PathArquivo, True);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_MFD_Espelho(const COOInicial, COOFinal: Integer;
  const PathArquivo: String);
begin
  Self.EspelhoMFD_DLL(CooInicial, CooFinal, PathArquivo, [docTodos]);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_MFD_Espelho(const DataInicial, DataFinal: TDateTime;
  const PathArquivo: String);
begin
  Self.EspelhoMFD_DLL(DataInicial, DataFinal, PathArquivo, [docTodos]);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_MFD_Cotepe1704(const COOInicial, COOFinal: Integer;
  const PathArquivo: String);
begin
  Self.ArquivoMFD_DLL(CooInicial, CooFinal, PathArquivo, [docTodos], finMFD, tpcCOO);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_MFD_Cotepe1704(const DataInicial, DataFinal: TDateTime;
  const PathArquivo: String);
begin
  Self.ArquivoMFD_DLL(DataInicial, DataFinal, PathArquivo, [docTodos], finMFD);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

procedure TACBrECF.PafMF_RelMeiosPagamento(
  const AFormasPagamento: TACBrECFFormasPagamento;
  const ATituloRelatorio: String;
  const AIndiceRelatorio: Integer);
var
  DataAtual: TDateTime;
  Relatorio: TStringList;
  I: Integer;
  TamLin: Integer;
  SubTotalCalculado: Double;
  FPAcumuladas: TACBrECFFormasPagamento;
  FPTotalizado: TACBrECFFormasPagamento;

  function ProcurarFormaPagamento(const AFormaPagto: TACBrECFFormaPagamento;
    const AFormasPagamento: TACBrECFFormasPagamento;
    const AConsideraTipoDoc: Boolean = True) :TACBrECFFormaPagamento;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to AFormasPagamento.Count - 1 do
    begin
      if AConsideraTipoDoc then
      begin
        if (AFormaPagto.Data = AFormasPagamento[I].Data) and
          (AnsiUpperCase(AFormasPagamento[I].Descricao) = AnsiUpperCase(AFormaPagto.Descricao)) and
          (AnsiUpperCase(AFormasPagamento[I].TipoDoc) = AnsiUpperCase(AFormaPagto.TipoDoc)) then
        begin
          Result := AFormasPagamento[I];
          Exit;
        end;
      end
      else
      begin
        if (AFormaPagto.Data = AFormasPagamento[I].Data) and
          (AnsiUpperCase(AFormasPagamento[I].Descricao) = AnsiUpperCase(AFormaPagto.Descricao)) then
        begin
          Result := AFormasPagamento[I];
          Exit;
        end;
      end;
    end;
  end;

  procedure AcumularValorFP(const AFormaPagto: TACBrECFFormaPagamento;
    const AConsideraTipoDoc: Boolean; var ALista: TACBrECFFormasPagamento);
  var
    FP: TACBrECFFormaPagamento;
  begin
    FP := ProcurarFormaPagamento(AFormaPagto, ALista, AConsideraTipoDoc);

    if FP <> nil then
      FP.Total := FP.Total + AFormaPagto.Total
    else
    begin
      with ALista.New do
      begin
        Data      := AFormaPagto.Data;
        Descricao := AFormaPagto.Descricao;
        Total     := AFormaPagto.Total;
        TipoDoc   := AFormaPagto.TipoDoc;
      end;
    end;
  end;

  procedure AddCabecalho(const AData: TDateTime);
  begin
    if AData > 0 then
    begin
      Relatorio.Add('');
      Relatorio.Add(ACBrStr('<n>DATA DE ACUMULAÇÃO: ' + FormatDateTime('dd/mm/yyyy', AData) + '</n>'));
    end;

    Relatorio.Add(ACBrStr('Identificação   Tipo                    Valor R$'));
    Relatorio.Add(
      PadRight('', 15, '-') + ' ' +
      PadRight('', 19, '-') + ' ' +
      PadRight('', 12, '-')
    );
  end;

  procedure AddSubTotal;
  begin
    Relatorio.Add('</linha_simples>');
    Relatorio.Add(Format('Sub-Total                           %12.2n', [SubTotalCalculado]));
    Relatorio.Add('');
    Relatorio.Add('');

    SubTotalCalculado  := 0.00;
  end;

begin
  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  TamLin := ECF.Colunas;

  // montagem do relatorio
  Relatorio := TStringList.Create;
  try
    Relatorio.Clear;
    Relatorio.Add('');

    if AIndiceRelatorio <= 0 then
    begin
      Relatorio.Add('</linha_dupla>');
      Relatorio.Add('<ce>MEIOS DE PAGAMENTO</ce>');
      Relatorio.Add('</linha_dupla>');
      Relatorio.Add('');
    end;

    if Trim(ATituloRelatorio) <> '' then
      Relatorio.Add(PadCenter(ATituloRelatorio, TamLin));

    // *************************************************************************
    // impressão do relatório acumulando por data, descricao e tipo de documento
    // *************************************************************************
    FPAcumuladas := TACBrECFFormasPagamento.Create;
    FPTotalizado := TACBrECFFormasPagamento.Create;
    try
      // acumular as formas por data, descricao e tipo de documento
      FPAcumuladas.Clear;
      AFormasPagamento.Ordenar;
      for I := 0 to AFormasPagamento.Count - 1 do
        AcumularValorFP(AFormasPagamento[I], True, FPAcumuladas);

      // gerar o relatório
      DataAtual := 0.0;
      FPAcumuladas.Ordenar;
      for I := 0 to FPAcumuladas.Count - 1 do
      begin
        if DataAtual <> FPAcumuladas[I].Data then
        begin
          // SUB-TOTAL
          if (DataAtual > 0) then
            AddSubTotal;

          // cabecalho
          DataAtual := FPAcumuladas[I].Data;
          AddCabecalho(DataAtual);
        end;

        // dados dos pagamentos
        Relatorio.Add(Format('%s %s %s', [
          PadRight(FPAcumuladas[I].Descricao, 15),
          PadRight(FPAcumuladas[I].TipoDoc, 19),
          Format('%12.2n', [FPAcumuladas[I].Total])
        ]));

        // acumuladores
        AcumularValorFP(FPAcumuladas[I], True, FPTotalizado);
        SubTotalCalculado := SubTotalCalculado + FPAcumuladas[I].Total;
      end;

      // sub-total do ultimo dia
      AddSubTotal;

      // ***********************************************************************
      // impressão do total geral
      // ***********************************************************************
      Relatorio.Add('');
      Relatorio.Add(PadCenter('TOTAL GERAL', TamLin));
      if Trim(ATituloRelatorio) <> '' then
        Relatorio.Add(PadCenter(ATituloRelatorio, TamLin));

      Relatorio.Add('');
      Relatorio.Add(ACBrStr('Identificação                           Valor R$'));
      Relatorio.Add(
        PadLeft('', 27, '-') + ' ' +
        PadLeft('', 20, '-')
      );

      // acumular os valores totais das formas
      FPAcumuladas.Clear;
      FPTotalizado.Ordenar;
      for I := 0 to FPTotalizado.Count - 1 do
      begin
        FPTotalizado[I].Data := 0;
        AcumularValorFP(FPTotalizado[I], False, FPAcumuladas);
      end;

      // impressão das linhas de totalização
      SubTotalCalculado  := 0.00;
      FPAcumuladas.Ordenar;
      for I := 0 to FPAcumuladas.Count - 1 do
      begin
        Relatorio.Add(Format('%s %s', [
          PadRight(FPAcumuladas[I].Descricao, 27),
          Format('%20.2n', [FPAcumuladas[I].Total]) ]));

        SubTotalCalculado := SubTotalCalculado + FPAcumuladas[I].Total;
      end;

      // somatorio total
      Relatorio.Add('</linha_simples>');
      Relatorio.Add(Format('TOTAL                %27.2n', [SubTotalCalculado]));
      Relatorio.Add('');
    finally
      FreeAndNil(FPAcumuladas);
      FPTotalizado.Free;
    end;

    // impressão do relatório
    Self.RelatorioGerencial(Relatorio, 1, AIndiceRelatorio);
  finally
    Relatorio.Free;
  end;
end;

procedure TACBrECF.PafMF_RelDAVEmitidos(const DAVsEmitidos: TACBrECFDAVs;
  const TituloRelatorio: String;
  const IndiceRelatorio: Integer);
var
  Relatorio: TStringList;
  TamanhoLinha: Integer;
  I: Integer;
begin
  // montagem do relatorio
  Relatorio := TStringList.Create;
  try
    TamanhoLinha  := fsECF.Colunas;

    Relatorio.Clear;
    Relatorio.Add('');

    if IndiceRelatorio <= 0 then
    begin
      Relatorio.Add('</linha_dupla>');
      Relatorio.Add('<ce>DAV EMITIDOS</ce>');
      Relatorio.Add('</linha_dupla>');
      Relatorio.Add('');
    end;

    if Trim(TituloRelatorio) <> '' then
    begin
      Relatorio.Add(PadCenter(TituloRelatorio, TamanhoLinha));
      Relatorio.Add('');
    end;

    Relatorio.Add('NUMERO  TITULO  EMISSAO COO_DAV COO_CUP VL.TOTAL');
    Relatorio.Add('</linha_simples>');

    DAVsEmitidos.Ordenar;
    for I := 0 to DAVsEmitidos.Count - 1 do
    begin
      Relatorio.Add(Format('%s %s', [PadLeft(DAVsEmitidos[I].Numero, 13), PadRight(DAVsEmitidos[I].Titulo, TamanhoLinha - 14)]));
      Relatorio.Add(Format('%s %6.6d %6.6d R$ %s', [
        FormatDateTime('dd/mm/yyyy', DAVsEmitidos[I].DtEmissao),
        DAVsEmitidos[I].COO_Dav,
        DAVsEmitidos[I].COO_Cupom,
        PadLeft(FormatFloatBr(DAVsEmitidos[I].Valor, ',#0.00'), TamanhoLinha - 28, ' ')
      ]));
      Relatorio.Add('');
    end;

    Relatorio.Add('</linha_simples>');
    Relatorio.Add(Format('%d DAV listado(s)', [DAVsEmitidos.Count]));

    // impressão do relatório
    Self.RelatorioGerencial(Relatorio, 1, IndiceRelatorio);
  finally
    Relatorio.Free;
  end;
end;

procedure TACBrECF.PafMF_RelIdentificacaoPafECF(
  IdentificacaoPaf: TACBrECFIdentificacaoPAF;
  const IndiceRelatorio: Integer);
var
  Relatorio: TStringList;
  I: Integer;
begin
  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  if (not Assigned(IdentificacaoPaf)) then
  begin
     if Assigned(fsAAC) then
        IdentificacaoPaf := fsAAC.IdentPAF
     else
        raise EACBrECFErro.Create( ACBrStr('IdentificacaoPaf não informado') ) ;
  end ;

  Relatorio := TStringList.Create;
  try
    Relatorio.Clear;
    Relatorio.Add('');

    if IndiceRelatorio <= 0 then
    begin
      Relatorio.Add('</linha_dupla>');
      Relatorio.Add('<ce>IDENTIFICACAO DO PAF-ECF</ce>');
      Relatorio.Add('</linha_dupla>');
      Relatorio.Add('');
    end;

    Relatorio.Add('</linha_dupla>');
    Relatorio.Add('LAUDO NUMERO: <n>' + IdentificacaoPaf.NumeroLaudo + '</n>');
    Relatorio.Add('EMISSÃO DO LAUDO: <n>' + FormatDateTime('dd/MM/yyyy', IdentificacaoPaf.DataLaudo) + '</n>');
    Relatorio.Add('</linha_dupla>');

    Relatorio.Add('');
    Relatorio.Add('<n>EMPRESA DESENVOLVEDORA</n>');
    Relatorio.Add('</linha_simples>');
    Relatorio.Add('CNPJ........: ' + IdentificacaoPaf.Empresa.CNPJ);
    Relatorio.Add('Razao Social: ' + IdentificacaoPaf.Empresa.RazaoSocial);
    Relatorio.Add('Endereco....: ' + IdentificacaoPaf.Empresa.Endereco);
    Relatorio.Add('Cidade/UF...: ' + IdentificacaoPaf.Empresa.Cidade + '/' + IdentificacaoPaf.Empresa.Uf);
    Relatorio.Add('CEP.........: ' + IdentificacaoPaf.Empresa.Cep);
    Relatorio.Add('Telefone....: ' + IdentificacaoPaf.Empresa.Telefone);
    Relatorio.Add('Contato.....: ' + IdentificacaoPaf.Empresa.Contato);

    // Removido e-mail por solicitação da POLIMIG para homologação.
    //Relatorio.Add('e-mail......: ' + IdentificacaoPaf.Empresa.Email);

    Relatorio.Add('');
    Relatorio.Add('<n>IDENTIFICACAO DO PAF-ECF</n>');
    Relatorio.Add('</linha_simples>');
    Relatorio.Add('Nome Comerc.: ' + IdentificacaoPaf.Paf.Nome);
    Relatorio.Add('Versao......: ' + IdentificacaoPaf.Paf.Versao);
//    Relatorio.Add('Laudo.......: ' + IdentificacaoPaf.NumeroLaudo);
    Relatorio.Add('Princ. Exec.: ' + IdentificacaoPaf.Paf.PrincipalExe.Nome);
    Relatorio.Add('MD-5........: ' + IdentificacaoPaf.Paf.PrincipalExe.MD5);

    Relatorio.Add('');
    Relatorio.Add('<n>OUTROS ARQUIVOS UTILIZADOS</n>');
    Relatorio.Add('</linha_simples>');
    for I := 0 to IdentificacaoPaf.OutrosArquivos.Count - 1 do
    begin
      Relatorio.Add(ExtractFileName(IdentificacaoPaf.OutrosArquivos[I].Nome));
      Relatorio.Add('MD-5: ' + IdentificacaoPaf.OutrosArquivos[I].MD5);
      Relatorio.Add('');
    end;

    Relatorio.Add('');
    Relatorio.Add('<n>ARQ. LISTA AUTENTICADOS</n>');
    Relatorio.Add('</linha_simples>');
    Relatorio.Add(ExtractFileName(IdentificacaoPaf.ArquivoListaAutenticados.Nome));
    Relatorio.Add('MD-5: ' + IdentificacaoPaf.ArquivoListaAutenticados.MD5);

    Relatorio.Add('');
    Relatorio.Add('<n>VERSAO ER-PAF-ECF</n>');
    Relatorio.Add('</linha_simples>');
    Relatorio.Add('ER-PAF-ECF..: ' + IdentificacaoPaf.VersaoER);

    Relatorio.Add('');
    Relatorio.Add('<n>ECFS AUTORIZADOS</n>');
    Relatorio.Add('</linha_simples>');
    for I := 0 to IdentificacaoPaf.ECFsAutorizados.Count - 1 do
      Relatorio.Add(IdentificacaoPaf.ECFsAutorizados[I].NumeroSerie);

    // impressão do relatório
    Self.RelatorioGerencial(Relatorio, 1, IndiceRelatorio);
  finally
    Relatorio.Free;
  end;
end;

procedure TACBrECF.PafMF_RelParametrosConfiguracao(const APerfilRequisitos: String;
  const AIndiceRelatorio: Integer);
var
  Relatorio: TStringList;
begin
  fsNumSerieCache := '' ;
  DoVerificaValorGT ;

  Relatorio := TStringList.Create;
  try
    Relatorio.Clear;

    Relatorio.Add('</linha_dupla>');
    Relatorio.Add('<ce>PARÂMETROS DE CONFIGURAÇÃO</ce>');
    Relatorio.Add('</linha_dupla>');
    Relatorio.Add('');
    Relatorio.Add('Perfil de Requisitos Configurado: ' + APerfilRequisitos);

    {$IFDEF UNICODE}
     Relatorio.Text := ACBrStr( Relatorio.Text );
    {$ENDIF}

    Self.RelatorioGerencial(Relatorio, 1, AIndiceRelatorio);
  finally
    Relatorio.Free
  end;
end;

procedure TACBrECF.PafMF_RelParametrosConfiguracao(AInfoPafECF: TACBrECFInfoPaf;
  const AIndiceRelatorio: Integer);
var
  Relatorio: TStringList;
  TamColSimNao: Integer;
  TamColDescr: Integer;
  versaoPafECF: Integer;

  function GetDescrFlag(const ALigado: Boolean): String;
  begin
    Result := IfThen(ALigado, ': S', ': N');
  end;

  function GetTipoIntegracao(const ATipo: TACBrPAFTipoIntegracao): String;
  begin
    case ATipo of
      tpiRetaguarda: Result := ': Retaguarda';
      tpiPED:        Result := ': Sistema PED';
      tpiAmbos:      Result := ': Ambos';
      tpiNaoIntegra: Result := ': Não Integrado';
    end;

  end;

  function GetTipoDesenvolvimento(const ATipo: TACBrPAFTipoDesenvolvimento): String;
  begin
    case ATipo of
      tpdComercializavel:       Result := ': Comercializável';
      tpdExclusivoProprio:      Result := ': Exclusivo-Próprio';
      tpdExclusivoTerceirizado: Result := ': Exclusivo-Terceirizado';
    end;
  end;

  function GetTipoFuncionamento(const ATipo: TACBrPAFTipoFuncionamento): String;
  begin
    case ATipo of
      tpfStandAlone:     Result := ': Stand-Alone';
      tpfEmRede:         Result := ': Em Rede';
      tpfParametrizavel: Result := ': Parametrizável';
    end;
  end;

begin
  versaoPafECF := 0;
  fsNumSerieCache := '' ;  // Isso força a Leitura do Numero de Série
  DoVerificaValorGT ;

  if (not Assigned(AInfoPafECF)) then
  begin
    if Assigned(fsAAC) then
    begin
      AInfoPafECF  := fsAAC.IdentPAF.Paf;
      versaoPafECF := StrToInt(OnlyNumber(fsAAC.IdentPAF.VersaoER));
    end
    else
      raise EACBrECFErro.Create( ACBrStr('Parâmetros de configuração do Paf-ECF não informados') ) ;
  end;

  TamColSimNao := Self.Colunas - 3;
  TamColDescr  := Self.Colunas - 25;

  Relatorio := TStringList.Create;
  try
    Relatorio.Clear;

    Relatorio.Add('</linha_dupla>');
    Relatorio.Add('<ce>PARÂMETROS DE CONFIGURAÇÃO</ce>');
    Relatorio.Add('</linha_dupla>');
    Relatorio.Add('');

    // Incluída UF após texto, por solicitação da POLIMIG durante Homologação
    if versaoPafECF >= 201 then
      Relatorio.Add('Perfil de Requisitos Configurado: ' + AInfoPafECF.PerfilRequisitos +
        ' - UF: ' + fsAAC.IdentPAF.Paf.UFContribuinte)
    else
    begin
      Relatorio.Add(QuebraLinhas(
        'Todas as parametrizações relacionadas neste relatório são de ' +
        'configuração inacessível ao usuário do PAF-ECF.',
        Colunas)
      );
      Relatorio.Add(QuebraLinhas(
        'A ativação ou não destes parâmetros é determinada pela unidade ' +
        'federada e somente pode ser feita pela intervenção da empresa ' +
        'desenvolvedora do PAF-ECF.',
        Colunas)
      );

      Relatorio.Add('');
      Relatorio.Add('<n>IDENTIFICAÇÃO E CARACTERISTICAS DO</n>');
      Relatorio.Add('<n>PROGRAMA APLICATIVO FISCAL</n>');
      Relatorio.Add('</linha_simples>');
      Relatorio.Add(PadRight('Nome Comercial', TamColDescr, '.')    + ': ' + AInfoPafECF.Nome);
      Relatorio.Add(PadRight('Versão', TamColDescr, '.')            + ': ' + AInfoPafECF.Versao);
      Relatorio.Add(PadRight('Ling. Programação', TamColDescr, '.') + ': ' + AInfoPafECF.Linguagem);
      Relatorio.Add(PadRight('Sist. Operacional', TamColDescr, '.') + ': ' + AInfoPafECF.SistemaOperacional);
      Relatorio.Add(PadRight('Banco de Dados.', TamColDescr, '.')   + ': ' + AInfoPafECF.BancoDados);

      Relatorio.Add('');
      Relatorio.Add('<n>FUNCIONALIDADES</n>');
      Relatorio.Add('</linha_simples>');
      Relatorio.Add(PadRight('Tipo de Funcionamento', TamColDescr, '.') + GetTipoFuncionamento(AInfoPafECF.TipoFuncionamento));
      Relatorio.Add(PadRight('Tipo de Desenvolvimento', TamColDescr, '.') + GetTipoDesenvolvimento(AInfoPafECF.TipoDesenvolvimento));
      Relatorio.Add(PadRight('Integração com PAF-ECF', TamColDescr, '.') + GetTipoIntegracao(AInfoPafECF.IntegracaoPAFECF));

      Relatorio.Add('');
      Relatorio.Add('<n>PARÂMETROS PARA NÃO CONCOMITÂNCIA</n>');
      Relatorio.Add('</linha_simples>');

      Relatorio.Add('<n>Req IV</n>');
      Relatorio.Add('ITEM 2: Realiza registros de pré-venda');
      Relatorio.Add(PadRight('conforme definido no inciso II do art. 1º', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.RealizaPreVenda));
      Relatorio.Add('ITEM 3: Emitir DAV, impresso em equip. não');
      Relatorio.Add(PadRight('fiscal, conf. defin. no inciso III do art.1º', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.RealizaDAVNaoFiscal));
      Relatorio.Add('ITEM 4: Emite DAV, impresso no ECF, como');
      Relatorio.Add('Relat. Ger.,conforme defin. no inciso');
      Relatorio.Add(PadRight('III do art.1º', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.RealizaDAVECF ));
      Relatorio.Add('ITEM 6: Registro de lançamento de mesa ou');
      Relatorio.Add(PadRight('conta de cliente', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.RealizaLancamentoMesa ));
      Relatorio.Add('<n>Req VI</n>');
      Relatorio.Add('ITEM 2: imprime o DAV conforme o modelo');
      Relatorio.Add(PadRight('constante no Anexo II', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.DAVConfAnexoII ));

      Relatorio.Add('');
      Relatorio.Add('<n>OFICINA DE CONSERTO</n>');
      Relatorio.Add('</linha_simples>');

      Relatorio.Add('<n>Req. XLI</n>');
      Relatorio.Add(PadRight('ITEM 1: Emite DAV-OS para serviços', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.RealizaDAVOS ));

      Relatorio.Add('');
      Relatorio.Add('<n>APLICAÇÕES ESPECIAIS</n>');
      Relatorio.Add('</linha_simples>');

      Relatorio.Add('<n>Req. VII</n>');
      Relatorio.Add('ITEM 20: Tabela de Indice Técnico');
      Relatorio.Add(PadRight('de Producao', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.IndiceTecnicoProd ));
      Relatorio.Add('<n>Req. XXXVII</n>');
      Relatorio.Add(PadRight('ITEM 1: Bar,Restaurante e Similares', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.BarSimilarECFRestaurante ));
      Relatorio.Add('<n>Req. XXXVIII-A</n>');
      Relatorio.Add(PadRight('ITEM 1: Bar,Restaurante e Similar c/ balança', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.BarSimilarBalanca ));
      Relatorio.Add('<n>Req. XXXIX</n>');
      Relatorio.Add('ITEM 1: Usa impressora não fiscal no ambiente');
      Relatorio.Add(PadRight('ambiente de produção', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.UsaImpressoraNaoFiscal ));
      Relatorio.Add('<n>Req. XL</n>');
      Relatorio.Add('ITENS 1 e 2: Imprime DAV descriminando a');
      Relatorio.Add(PadRight('fórmula manipulada', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.DAVDiscrFormula ));

      Relatorio.Add('');
      Relatorio.Add('<n>REVENDA COMBUSTÍVEL</n>');
      Relatorio.Add('</linha_simples>');

      Relatorio.Add('<n>Req. XXXII</n>');
      Relatorio.Add('ITEM 1: Acumula por dia, o volume de cada');
      Relatorio.Add(PadRight('combustível', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.AcumulaVolumeDiario ));
      Relatorio.Add('ITEM 2: Armazena os encerrantes inicial');
      Relatorio.Add(PadRight('e final a cada abastecimento', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.ArmazenaEncerranteIniFinal ));

      Relatorio.Add('<n>REQ XXXIII</n>');
      Relatorio.Add('ITENS 1 e 2: Emite controle de encerrantes');
      Relatorio.Add(PadRight('após a Redução Z e Leitura X', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.EmiteContrEncerrAposREDZLEIX ));

      Relatorio.Add('<n>REQ XXXV</n>');
      Relatorio.Add(PadRight('ITEM 1: PAF integrado com bombas', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.IntegradoComBombas ));
      Relatorio.Add('ITEM 3: Cria um abastecimento em caso de');
      Relatorio.Add(PadRight('divergência de encerrantes', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.CriaAbastDivergEncerrante ));

      Relatorio.Add('<n>Requisito XXXVI-A</n>');
      Relatorio.Add('ITEM 1: impede registro venda com valor');
      Relatorio.Add(PadRight('zerado ou negativo', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.ImpedeVendaVlrZero ));

      Relatorio.Add('<n>REQ XXXVI-B</n>');
      Relatorio.Add('ITENS 1, 2, 3 e 4: possui cadastro da(s)');
      Relatorio.Add(PadRight('placa(s) da(s) bomba(s)', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.CadastroPlacaBomba ));

      Relatorio.Add('');
      Relatorio.Add('<n>CRITÉRIOS POR UNIDADE FEDERADA</n>');
      Relatorio.Add('</linha_simples>');

      Relatorio.Add('<n>Req. XVII</n>');
      Relatorio.Add('ITEM 1 alinea b: emissão de documento');
      Relatorio.Add(PadRight('fiscal por PED', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.EmitePED ));

      Relatorio.Add('<n>REQ XVIII</n>');
      Relatorio.Add('ITEM 1 - Tela de Consulta de Produtos');
      Relatorio.Add(PadRight('Alinea a: Totalização dos valores da lista', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.TotalizaValoresLista ));
      Relatorio.Add(PadRight('Alinea b: Transf. das info em Pré-venda', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.TransfPreVenda ));
      Relatorio.Add(PadRight('Alinea c: Transf. das info em DAV', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.TransfDAV ));

      Relatorio.Add('<n>REQ. XXII</n>');
      Relatorio.Add(PadRight('ITEM 7 Alinea b: Recompõe valor GT', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.RecompoeGT ));
      Relatorio.Add(PadRight('ITEM 8 Recompõe Número Série', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.RecompoeNumSerie ));

      Relatorio.Add('<n>Req. VIII-A</n>');
      Relatorio.Add(PadRight('ITEM 2 : MINAS LEGAL', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.MinasLegal ));
      Relatorio.Add(PadRight('ITEM 2A: CUPOM MANIA', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.CupomMania ));
      Relatorio.Add(PadRight('ITEM 2B: NOTA LEGAL', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.NotaLegalDF ));
      Relatorio.Add(PadRight('ITEM 2C: PARAIBA LEGAL', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.ParaibaLegal ));

      Relatorio.Add('<n>REQUISITO XIV</n>');
      Relatorio.Add(PadRight('ITEM 4: TROCO EM CARTÃO', TamColSimNao, '.') + GetDescrFlag( AInfoPafECF.TrocoEmCartao ));

      Relatorio.Add('');
    end;

    {$IFDEF UNICODE}
     Relatorio.Text := ACBrStr( Relatorio.Text );
    {$ENDIF}

    Self.RelatorioGerencial(Relatorio, 1, AIndiceRelatorio);
  finally
    Relatorio.Free
  end;
end;

procedure TACBrECF.PafMF_ArqMF_Binario(const APathArquivo: String;
  Assinar: Boolean);
var
  EADStr: String;
begin
  if (not fsAtivo) then
     raise EACBrECFNaoInicializado.create( ACBrStr(cACBrECFNaoInicializadoException) );

  Self.ArquivoMF_Binario_DLL(APathArquivo);

  if not FileExists(APathArquivo) then
    raise EACBrEADException.CreateFmt('Arquivo MF: "%s" não foi gerado', [APathArquivo]);

  if Assinar then
  begin
    // assinar o arquivo baixado da impressora
    EADStr := 'EAD' + GetACBrEAD.CalcularEADArquivo(APathArquivo);

    // gravar o arquivo texto com a assinatura EAD
    WriteToTXT(ChangeFileExt(APathArquivo, '.TXT'), EADStr, False, True);
  end;
end;

procedure TACBrECF.PafMF_ArqMFD_Binario(const APathArquivo: String;
  DataInicial: TDateTime; DataFinal: TDateTime; Assinar: Boolean);
var
  EADStr: String;
begin
  if (not fsAtivo) then
     raise EACBrECFNaoInicializado.create( ACBrStr(cACBrECFNaoInicializadoException) );

  if (DataInicial = 0) or (DataFinal = 0) then
    Self.ArquivoMFD_Binario_DLL(APathArquivo)
  else
    Self.ArquivoMFD_Binario_DLL(APathArquivo, DataInicial, DataFinal);

  if not FileExists(APathArquivo) then
    raise EACBrEADException.CreateFmt('Arquivo MFD: "%s" não foi gerado', [APathArquivo]);

  if Assinar then
  begin
    // assinar o arquivo baixado da impressora
    EADStr := 'EAD' + GetACBrEAD.CalcularEADArquivo(APathArquivo);

    // gravar o arquivo texto com a assinatura EAD
    WriteToTXT(ChangeFileExt(APathArquivo, '.TXT'), EADStr, False, True);
  end
end;

procedure TACBrECF.ProgramarBitmapPromocional(const AIndice: Integer;
  const APathArquivo: AnsiString; const AAlinhamento: TACBrAlinhamento);
var
  Alinhamento: String;
begin
  case AAlinhamento of
    alDireita: Alinhamento := 'Direita';
    alEsquerda: Alinhamento := 'Esquerda';
    alCentro: Alinhamento := 'Centro';
  end;

  ComandoLOG := 'ProgramarBitmapPromocional('+
    IntToStr(AIndice)+','+
    APathArquivo+','+
    Alinhamento+
    ')';

  fsECF.ProgramarBitmapPromocional(AIndice, APathArquivo, AAlinhamento);
end;

procedure TACBrECF.DAV_Abrir(const AEmissao: TDateTime; const ADescrDocumento,
  ANumero, ASituacao, AVendedor, AObservacao, ACNPJCPF, ANomeCliente,
  AEndereco: String; const ANumFabricacao: string; const AMarca: string;
  const AModelo: string; const AAno: string; const APlaca: string;
  const ARenavam: string; const AIndice: Integer);
var
  TextoRel: TStringList;
begin
  FDAVItemCount := 0;
  FDAVTotal     := 0.00;

  TextoRel := TStringList.Create;
  try
    TextoRel.Clear;
    TextoRel.Add('</linha_simples>');
    TextoRel.Add('<ce>DOCUMENTO AUXILIAR DE VENDA</ce>');
    TextoRel.Add('</linha_simples>');
    TextoRel.Add('<ce>' + ADescrDocumento + ' N.' + ANumero + '</ce>');
    TextoRel.Add('</linha_simples>');
    TextoRel.Add('<ce>NÃO É DOCUMENTO FISCAL</ce>');
    TextoRel.Add('<ce>NÃO COMPROVA PAGAMENTO</ce>');
    TextoRel.Add('<ce>NÃO É VÁLIDO COMO RECIBO E COMO</ce>');
    TextoRel.Add('<ce>GARANTIA DE MERCADORIA</ce>');
    TextoRel.Add('</linha_simples>');
    TextoRel.Add('Emissao.: ' + FormatDateTime('dd/mm/yyyy hh:mm', AEmissao));

    if AVendedor > EmptyStr then
      TextoRel.Add('Vendedor: ' + Copy(AVendedor, 0, 38));

    if ASituacao <> '' then
      TextoRel.Add('Situacao: ' + Copy(ASituacao, 0, 38));

    if AObservacao <> '' then
      TextoRel.Add(AObservacao);

    TextoRel.Add('');

    TextoRel.Add('IDENTIFICACAO DO DESTINATÁRIO');
    TextoRel.Add('</linha_simples>');
    TextoRel.Add('CNPJ/CPF: ' + ACNPJCPF);
    TextoRel.Add('Nome: '     + ANomeCliente);
    TextoRel.Add('Endereço: ' + AEndereco);

    if (ANumFabricacao <> EmptyStr) then
    begin
      TextoRel.Add('');
      TextoRel.Add('OBJETO DE CONSERTO');
      TextoRel.Add('</linha_simples>');
      TextoRel.Add('Núm. Fabricação:');
      TextoRel.Add(ANumFabricacao);
    end;

    if (AMarca <> EmptyStr) or
      (AModelo <> EmptyStr) or
      (AAno <> EmptyStr) or
      (APlaca <> EmptyStr) or
    (ARenavam <> EmptyStr) then
    begin
      TextoRel.Add('');
      TextoRel.Add('VEÍCULO');
      TextoRel.Add('</linha_simples>');
      if AMarca <> EmptyStr then
        TextoRel.Add('Marca: '+Copy(AMarca, 0, 38));
      if AModelo <> EmptyStr then
        TextoRel.Add('Modelo: '+Copy(AModelo, 0, 38));
      if AAno <> EmptyStr then
        TextoRel.Add('Ano: '+Copy(AAno, 0, 38));
      if APlaca <> EmptyStr then
        TextoRel.Add('Placa: '+Copy(APlaca, 0, 38));
      if ARenavam <> EmptyStr then
        TextoRel.Add('Renavam: '+Copy(ARenavam, 0, 38));
    end;


    TextoRel.Add('');
    TextoRel.Add('</linha_simples>');
    TextoRel.Add('CODIGO DESCRICAO QTD UN VL.UNIT VL.DESC VL.TOTAL');
    TextoRel.Add('</linha_simples>');

    AbreRelatorioGerencial( AIndice );
    LinhaRelatorioGerencial( ACBrStr(TextoRel.Text) );
  finally
    TextoRel.Free;
  end;
end;

procedure TACBrECF.DAV_RegistrarItem(const ACodigo, ADescricao, AUnid: String;
 const AQuantidade, AVlrUnitario, AVlrDesconto, AVlrAcrescimo: Double;
 const ACancelado: Boolean);
var
  TextoRel: TStringList;
  ValorTotal: Double;
  strQuant: String;
begin
  if Self.Estado <> estRelatorio then
    raise EACBrECFErro.Create('Efetue a abertura do DAV antes de continuar.');

  ValorTotal := ((AVlrUnitario * AQuantidade) + AVlrAcrescimo) - AVlrDesconto;

  TextoRel := TStringList.Create;
  try
    FDAVItemCount := FDAVItemCount + 1;
    if not ACancelado then
      FDAVTotal := FDAVTotal + ValorTotal;

    TextoRel.Add(
      Format('%3.3d', [FDAVItemCount]) + ' ' +
      PadRight(ACodigo, 14) + ' ' +
      ADescricao
    );

    if Frac(AQuantidade) > 0 then
      strQuant := Format('%11.3f', [AQuantidade])
    else
      strQuant := PadLeft(IntToStr(Trunc(AQuantidade)), 11);

    TextoRel.Add(
      strQuant + ' ' +
      PadRight(AUnid, 3, ' ') +
      Format('R$ %10.2f', [AVlrUnitario]) +
      Format('R$ %11.2f', [ValorTotal]) +
      IfThen(ACancelado, ' Cancelado', '')
    );

    if AVlrDesconto > 0 then
      TextoRel.Add(Format('desconto item %3.3d: R$ %.2f', [FDAVItemCount, AVlrDesconto]));

    if AVlrAcrescimo > 0 then
      TextoRel.Add(Format('acréscimo item %3.3d: R$ %.2f', [FDAVItemCount, AVlrAcrescimo]));

    LinhaRelatorioGerencial( ACBrStr(TextoRel.Text) );
  finally
    TextoRel.Free;
  end;
end;

procedure TACBrECF.DAV_Fechar(const AObservacao: String; AVlrDesconto : Double; AVlrAcrescimo: Double);
var
  DescrItem: String;
  TextoRel: TStringList;
begin
  if Self.Estado <> estRelatorio then
    raise EACBrECFErro.Create('Efetue a abertura do DAV antes de continuar.');

  TextoRel := TStringList.Create;
  try
    if Trim(AObservacao) <> EmptyStr then
    begin
      TextoRel.Add('');
      TextoRel.Add(AObservacao);
    end;

    if FDAVItemCount > 1 then
      DescrItem := PadRight(IntToStr(FDAVItemCount) + ' itens', 12, ' ')
    else
      DescrItem := PadRight(IntToStr(FDAVItemCount) + ' item', 12, ' ');

    TextoRel.Add('</linha_simples>');
    TextoRel.Add(DescrItem + PadLeft('Valor Total: ' + Format('R$ %11.2f', [FDAVTotal]), 36, ' '));

    if AVlrDesconto > 0 then
     TextoRel.Add(PadLeft('Total Desc.: ' + Format('R$ %11.2f', [AVlrDesconto]), 48, ' '));

    if AVlrAcrescimo > 0 then
     TextoRel.Add(PadLeft('Total Acre.: ' + Format('R$ %11.2f', [AVlrAcrescimo]), 48, ' '));

    TextoRel.Add('');
    TextoRel.Add('');
    TextoRel.Add('</linha_dupla>');
    TextoRel.Add('<ce>É VEDADA A AUTENTICAÇÃO DESTE DOCUMENTO</ce>');
    TextoRel.Add('</linha_dupla>');
    TextoRel.Add('');
    TextoRel.Add('');

    LinhaRelatorioGerencial( ACBrStr(TextoRel.Text) );
  finally
    TextoRel.Free;
  end;

  FechaRelatorio;

  FDAVItemCount := 0;
  FDAVTotal     := 0.00;
end;

procedure TACBrECF.GerarNotaGaucha(const DataInicial, DataFinal: TDateTime;
  const PathArquivo: String);
begin
  Self.ArquivoMFD_DLL(DataInicial, DataFinal, PathArquivo, [docTodos], finTDM);
  Self.AssinaArquivoComEAD(PathArquivo);
end;

{$IFDEF FRAMEWORK}
{$DEFINE NOGUI}
{$ENDIF}
end.

