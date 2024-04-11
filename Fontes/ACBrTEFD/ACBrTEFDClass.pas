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

unit ACBrTEFDClass ;

interface

uses
  Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrTEFComum
  {$IFNDEF NOGUI}
    {$IfDef MSWINDOWS}
      ,Windows, Messages
    {$EndIf}
    {$If DEFINED(VisualCLX)}
      ,Qt, QControls, QForms
    {$ElseIf DEFINED(FMX)}
      ,System.UITypes, FMX.Forms, FMX.Controls
    {$ElseIf DEFINED(DELPHICOMPILER16_UP)}
      ,System.UITypes, Vcl.Forms, Vcl.Controls
    {$Else}
      ,Controls, Forms
    {$IfEnd}
  {$ENDIF},
  sysutils;

{$IFDEF NOGUI}
type TModalResult = (mrNone = 0, mrYes = 6, mrNo = 7, mrOK = 1, mrCancel = 2, mrAbort = 3, mrRetry = 4, mrIgnore = 5, mrAll = 8, mrNoToAll = 9, mrYesToAll = 10);
{$ENDIF}

const
   CACBrTEFD_EsperaSTS   = 7 ;
   CACBrTEFD_EsperaMinimaMensagemFinal = 5 ;
   CACBrTEFD_EsperaSleep = 250 ;
   CACBrTEFD_NumVias     = 2 ;
   CACBrTEFD_DestaqueVia = 'Destaque a %dª Via' ;
   CACBrTEFD_Erro_ECFNaoLivre = 'ECF não está LIVRE' ;
   CACBrTEFD_Erro_ECFEstado = 'Erro ao obter Estado do ECF' ;
   CACBrTEFD_Erro_ECFNaoResponde = 'Erro na impressão.'+sLineBreak+
                                   'Deseja tentar novamente ?' ;
   CACBrTEFD_Erro_ECFNaoRespondeInfo = 'Impressora não responde.'+sLineBreak+
                                       'Deseja continuar ?' ;
   CACBrTEFD_Erro_NaoAtivo = 'O gerenciador padrão %s não está ativo !' ;
   CACBrTEFD_Erro_SemRequisicao = 'Nenhuma Requisição Iniciada' ;
   CACBrTEFD_Erro_OutraFormaPagamento = 'Gostaria de continuar a transação com '+
                                   'outra(s) forma(s) de pagamento ?';
   CACBrTEFD_Erro_CNCNaoEfetuado = 'Não foi possível Cancelar a Transação TEF.'+sLineBreak+
                                   'Deseja tentar novamente ?';

type

  { Tipos de TEF Existente. Cado novo Tipo de Tef precisa de uma NOVA Classe,
    filha de  TACBrTEFDClass }
  TACBrTEFDTipo = ( gpNenhum, gpTefDial, gpTefDisc, gpHiperTef, gpCliSiTef,
                    gpTefGpu, gpVeSPague, gpBanese, gpTefAuttar, gpGoodCard,
                    gpFoxWin, gpCliDTEF, gpPetrocard, gpCrediShop, gpTicketCar,
                    gpConvCard, gpCappta, gpPayGo, gpPayGoWeb, gpCliSiTefModular, gpTefDirecao,
                    gpTefDialScopeGetcard, gpTefElgin) ;

  TACBrTEFDReqEstado = ( reqNenhum,             // Nennhuma Requisição em andamento
                         reqIniciando,          // Iniciando uma nova Requisicao
                         reqCriandoArquivo,     // Arquivo Temporário de requisição está sendo criado
                         reqAguardandoResposta, // Requisição Escrita, Aguardando Resposta
                         reqConferindoResposta, // Verifica se o STS é válido
                         reqFinalizada ) ;

  TACBrTEFDRespEstado = ( respNenhum,              // Nennhuma Resposta em andamento
                          respAguardandoResposta,  // Requisição Escrita, Aguardando Resposta
                          respProcessando,         // Processando a Resposta
                          respConcluida ) ;

  EACBrTEFDErro              = class(EACBrTEFErro) ;
  EACBrTEFDGPNaoResponde     = class(EACBrTEFDErro) ;
  EACBrTEFDGPNaoInicializado = class(EACBrTEFDErro) ;
  EACBrTEFDSTSInvalido       = class(EACBrTEFDErro) ;
  EACBrTEFDECF               = class(EACBrTEFDErro) ;

  TACBrTEFDAguardaRespEvent = procedure( Arquivo: String;
     SegundosTimeOut : Integer; var Interromper : Boolean) of object ;

  TACBrTEFDOperacaoMensagem = ( opmOK, opmYesNo,
                                opmExibirMsgOperador, opmRemoverMsgOperador,
                                opmExibirMsgCliente, opmRemoverMsgCliente,
                                opmDestaqueVia ) ;

  TACBrTEFDReq = class;

  { TACBrTEFDResp }

  TACBrTEFDResp = class(TACBrTEFResp)
  protected
    fpTipoGP: TACBrTEFDTipo;
    fpOrdemPagamento: Integer;
    fpIndiceFPG_ECF: String;

    procedure SetIndiceFPG_ECF(const AValue: String);
    procedure SetOrdemPagamento(const AValue: Integer);
  public
    procedure ProcessarTipoInterno(ALinha: TACBrTEFLinha); override;

    procedure Clear; override;
    procedure Assign(Source: TACBrTEFResp); override;

    property TipoGP: TACBrTEFDTipo read fpTipoGP write fpTipoGP;

    property IndiceFPG_ECF: String read fpIndiceFPG_ECF write SetIndiceFPG_ECF;
    property OrdemPagamento: Integer read fpOrdemPagamento write SetOrdemPagamento;
  end;

  { TACBrTEFDRespostasPendentes }

  TACBrTEFDRespostasPendentes = class(TACBrTEFRespostasPendentes)
  private
    fSaldoAPagar: Double;
    function GetSaldoRestante: Double;
  public
    constructor Create(FreeObjects : boolean);

    property SaldoAPagar: Double read fSaldoAPagar write fSaldoAPagar;
    property SaldoRestante: Double read GetSaldoRestante;
  end;

  TACBrTEFDAntesFinalizarReq = procedure( Req : TACBrTEFDReq ) of object ;
  TACBrTEFDMudaEstadoReq     = procedure( EstadoReq  : TACBrTEFDReqEstado  ) of object ;
  TACBrTEFDMudaEstadoResp    = procedure( EstadoResp : TACBrTEFDRespEstado ) of object ;

  TACBrTEFDProcessarTransacoesPendentes = procedure( RespostasPendentes :
     TACBrTEFDRespostasPendentes ) of object ;

  TACBrTEFDAntesCancelarTransacao = procedure( RespostaPendente :
     TACBrTEFDResp ) of object ;

  TACBrTEFDExibeMsg = procedure( Operacao : TACBrTEFDOperacaoMensagem;
     Mensagem : String; var AModalResult : TModalResult ) of object ;

  TACBrTEFDExibeQRCode = procedure(const Dados: String) of object ;

  TACBrTEFDOperacaoECF = ( opeAbreGerencial, opeFechaGerencial,
                           opePulaLinhas, opeSubTotalizaCupom, opeFechaCupom,
                           opeFechaVinculado, opeCancelaCupom,
                           opeImprimePagamentos ) ;

  TACBrTEFDBloqueiaMouseTeclado = procedure( Bloqueia : Boolean;
     var Tratado : Boolean ) of object ;

  TACBrTEFDExecutaAcao = procedure( var Tratado : Boolean ) of object ;

  TACBrTEFDComandaECF = procedure( Operacao : TACBrTEFDOperacaoECF; Resp : TACBrTEFDResp;
     var RetornoECF : Integer ) of object ; { -1 - Não tratado, 0 - Erro na Execucao, 1 - Sucesso }

  TACBrTEFDComandaECFSubtotaliza = procedure( DescAcre : Double;
     var RetornoECF : Integer ) of object ; { -1 - Não tratado, 0 - Erro na Execucao, 1 - Sucesso }

  TACBrTEFDComandaECFPagamento = procedure( IndiceECF : String; Valor : Double;
     var RetornoECF : Integer ) of object ; { -1 - Não tratado, 0 - Erro na Execucao, 1 - Sucesso }

  TACBrTEFDComandaECFAbreVinculado = procedure( COO, IndiceECF : String; Valor : Double;
     var RetornoECF : Integer ) of object ; { -1 - Não tratado, 0 - Erro na Execucao, 1 - Sucesso }

  TACBrTEFDTipoRelatorio = ( trGerencial, trVinculado ) ;

  TACBrTEFDComandaECFImprimeVia = procedure( TipoRelatorio : TACBrTEFDTipoRelatorio;
     Via : Integer; ImagemComprovante : TStringList;
     var RetornoECF : Integer ) of object ; { -1 - Não tratado, 0 - Erro na Execucao, 1 - Sucesso }

  TACBrTEFDInfoECF = ( ineSubTotal,  // Valor do Saldo restante "A Pagar" do Cupom
                       ineEstadoECF, // Estado do ECF "L" Livre, "V" Em Venda de Itens,
                                     //               "P" Em Pagamento,
                                     //               "C" CDC ou Vinculado
                                     //               "G" Relatório Gerencial
                                     //               "N" Não Fiscal (em qq fase, pois é dificil detectar a fase)
                                     //               "O" Outro
                       ineTotalAPagar// Valor Total de Pagamentos registrados, na Aplicação, e não enviados ao ECF
                     ) ;

  TACBrTEFDObterInfoECF = procedure( Operacao : TACBrTEFDInfoECF;
     var RetornoECF : String  ) of object ;

   { TACBrTEFDReq }

   TACBrTEFDReq = class
   private
     fAgencia : String;
     fAgenciaDC : String;
     fBanco : String;
     fCheque : String;
     fChequeDC : String;
     fCMC7 : String;
     fCodigoAutorizacaoTransacao: String;
     fConta : String;
     fContaDC : String;
     fConteudo   : TACBrTEFArquivo;
     fDataCheque : TDateTime;
     fDataHoraTransacaoComprovante : TDateTime;
     fDocumentoPessoa : String;
     fFinalizacao : String;
     fInformacao : TACBrInformacao;
     fHeader : String;
     fID : Integer;
     fMoeda : Integer;
     fNSU : String;
     fRede : String;
     fTipoPessoa : AnsiChar;
     fValorTotal : Double;
     fDocumentoVinculado : String;
     procedure SetAgencia(const AValue : String);
     procedure SetAgenciaDC(const AValue : String);
     procedure SetBanco(const AValue : String);
     procedure SetCheque(const AValue : String);
     procedure SetChequeDC(const AValue : String);
     procedure SetCMC7(const AValue : String);
     procedure SetCodigoAutorizacaoTransacao(AValue: String);
     procedure SetConta(const AValue : String);
     procedure SetContaDC(const AValue : String);
     procedure SetDataCheque(const AValue : TDateTime);
     procedure SetDataHoraTransacaoComprovante(const AValue : TDateTime);
     procedure SetDocumentoPessoa(const AValue : String);
     procedure SetFinalizacao(const AValue : String);
     procedure SetHeader(const AValue : String);
     procedure SetID(const AValue : Integer);
     procedure SetMoeda(const AValue : Integer);
     procedure SetNSU(const AValue : String);
     procedure SetRede(const AValue : String);
     procedure SetTipoPessoa(const AValue : AnsiChar);
     procedure SetValorTotal(const AValue : Double);
     procedure SetDocumentoVinculado(const AValue : String);
   public
     constructor Create ;
     destructor Destroy ; override;

     procedure Clear ;

     property Conteudo : TACBrTEFArquivo read fConteudo ;

     property Header            : String    read fHeader             write SetHeader ;
     property ID                : Integer   read fID                 write SetID ;
     property DocumentoVinculado: String    read fDocumentoVinculado write SetDocumentoVinculado ;
     property ValorTotal        : Double    read fValorTotal         write SetValorTotal ;
     property Moeda             : Integer   read fMoeda              write SetMoeda ;
     property CMC7              : String    read fCMC7               write SetCMC7 ;
     property TipoPessoa        : AnsiChar  read fTipoPessoa         write SetTipoPessoa ;
     property DocumentoPessoa   : String    read fDocumentoPessoa    write SetDocumentoPessoa ;
     property DataCheque        : TDateTime read fDataCheque         write SetDataCheque ;
     property Rede              : String    read fRede               write SetRede ;
     property NSU               : String    read fNSU                write SetNSU ;
     property Finalizacao       : String    read fFinalizacao        write SetFinalizacao ;
     property Banco             : String    read fBanco              write SetBanco ;
     property Agencia           : String    read fAgencia            write SetAgencia ;
     property AgenciaDC         : String    read fAgenciaDC          write SetAgenciaDC ;
     property Conta             : String    read fConta              write SetConta ;
     property ContaDC           : String    read fContaDC            write SetContaDC ;
     property Cheque            : String    read fCheque             write SetCheque ;
     property ChequeDC          : String    read fChequeDC           write SetChequeDC ;
     property DataHoraTransacaoComprovante : TDateTime read fDataHoraTransacaoComprovante write SetDataHoraTransacaoComprovante ;
     property CodigoAutorizacaoTransacao: String read fCodigoAutorizacaoTransacao write SetCodigoAutorizacaoTransacao;

     procedure GravaInformacao( const Identificacao : Integer;
        const Sequencia : Integer; const Informacao : String ) ;
   end;

   { TACBrTEFDRespTXT }

   TACBrTEFDRespTXT = class( TACBrTEFDResp )
   private
     function GetTrailerOK : Boolean;
   protected
     function AjustaLinhaImagemComprovante(Linha: AnsiString): AnsiString;
     function GetTransacaoAprovada : Boolean; override;
   public
     procedure ConteudoToProperty; override;
     property TrailerOk : Boolean read GetTrailerOK ;
   end;

   TACBrTEFDArrayGrupoRespostasPendentes = array of record
      IndiceFPG_ECF  : String ;
      OrdemPagamento : Integer ;
      Total  : Double ;
   end ;

   TACBrTEFDGravarLog = procedure(const GP: TACBrTEFDTipo; ALogLine: String; var Tratado: Boolean) of object ;

   { TACBrTEFDClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllDesktopPlatforms)]
  {$ENDIF RTL230_UP}
   TACBrTEFDClass = class( TComponent )
   private
     fArqLOG : String;
     fArqReq : String;
     fArqTmp : String;
     fArqResp: String;
     fArqSTS : String;

     fAutoAtivarGP : Boolean;
     fEsperaSTS : Integer;
     fGPExeName : String;
     fHabilitado : Boolean;
     fLogDebug: Boolean;

     procedure SetArqReq(const AValue : String);
     procedure SetArqResp(const AValue : String);
     procedure SetArqSTS(const AValue : String);
     procedure SetArqTmp(const AValue : String);
     procedure SetAutoAtivarGP(AValue: Boolean);
     procedure SetInicializado(const AValue : Boolean);
     procedure SetGPExeName(const AValue : String);

   protected
     fpInicializado : Boolean;
     fpReq  : TACBrTEFDReq ;
     fpResp : TACBrTEFDResp ;
     fpTipo : TACBrTEFDTipo;
     fpIDSeq: Integer ;
     fpNumVias : Integer;
     fpAguardandoResposta : Boolean ;
     fpSalvarArquivoBackup: Boolean;

     procedure SetNumVias(const AValue : Integer); virtual;

     procedure IniciarRequisicao( AHeader : String; AID : Integer = 0 ); virtual;
     procedure AdicionarIdentificacao ; virtual;
     procedure FinalizarRequisicao ; virtual;

     Function VerificarRespostaRequisicao : Boolean ; virtual;
     Procedure LerRespostaRequisicao ; virtual;
     procedure FinalizarResposta( ApagarArqResp : Boolean ) ; virtual;

     Function CopiarResposta : String ; virtual;

     procedure ProcessarResposta ; virtual;
     Function ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean; virtual;

     procedure VerificarIniciouRequisicao; virtual;

     procedure ImprimirRelatorio ; virtual;
     procedure ConfirmarESolicitarImpressaoTransacoesPendentes ; virtual ;

     Procedure VerificarTransacaoPagamento(Valor : Double); virtual;
     Function TransacaoEPagamento( AHeader: String ): Boolean; virtual;

   protected
     property EsperaSTS : Integer read fEsperaSTS write fEsperaSTS
        default CACBrTEFD_EsperaSTS ;

     property ArqTemp  : String read fArqTmp    write SetArqTmp ;
     property ArqReq   : String read fArqReq    write SetArqReq ;
     property ArqSTS   : String read fArqSTS    write SetArqSTS  ;
     property ArqResp  : String read fArqResp   write SetArqResp ;
     property GPExeName: String read fGPExeName write SetGPExeName ;
   public
     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     property AutoAtivarGP : Boolean read fAutoAtivarGP write SetAutoAtivarGP default True ;
     property Tipo : TACBrTEFDTipo read fpTipo ;

     property NumVias : Integer read fpNumVias write SetNumVias default CACBrTEFD_NumVias ;

     property Req  : TACBrTEFDReq  read fpReq  ;
     property Resp : TACBrTEFDResp read fpResp ;

     property AguardandoResposta : Boolean read fpAguardandoResposta ;

     procedure GravaLog(AString: AnsiString; Traduz: Boolean = False);
     Function CriarResposta( Tipo: TACBrTEFDTipo ): TACBrTEFDResp;

     property Inicializado : Boolean read fpInicializado write SetInicializado ;
     procedure VerificaInicializado ;

     procedure Inicializar ; virtual;
     procedure DesInicializar ; virtual;

     procedure AtivarGP ; virtual;
     procedure VerificaAtivo ; virtual;

     procedure VerificarTransacoesPendentesClass(aVerificarCupom: Boolean); virtual;
     procedure CancelarTransacoesPendentesClass; virtual;
     procedure ConfirmarTransacoesAnteriores; virtual;

     Procedure ATV ; virtual;
     Function ADM : Boolean; virtual;
     Function CRT( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; virtual;
     Function CDP(Const EntidadeCliente: String; Out Resposta: String): Boolean; virtual;
     Function CHQ( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; CMC7 : String = '';
        TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
        DataCheque : TDateTime = 0; Banco   : String = '';
        Agencia    : String = ''; AgenciaDC : String = '';
        Conta      : String = ''; ContaDC   : String = '';
        Cheque     : String = ''; ChequeDC  : String = '';
        Compensacao: String = '' ) : Boolean ; virtual;
     Procedure NCN ; overload; virtual;
     Procedure NCN(Rede, NSU, Finalizacao : String;
        Valor : Double = 0; DocumentoVinculado : String = '') ;
        overload; virtual; 
     Procedure CNF ; overload; virtual;
     Procedure CNF(Rede, NSU, Finalizacao : String;
        DocumentoVinculado : String = ''); overload; virtual;
     Function CNC : Boolean ; overload; virtual;
     Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
        Valor : Double; CodigoAutorizacaoTransacao: String = '') : Boolean; overload; virtual;
     Function PRE(Valor : Double; DocumentoVinculado : String = '';
        Moeda : Integer = 0) : Boolean; virtual;

     procedure ExibirMensagemPinPad(const MsgPinPad: String); virtual;
   published
     property ArqLOG : String read fArqLOG write fArqLOG ;
     property LogDebug : Boolean read fLogDebug write fLogDebug default false;

     Property Habilitado: Boolean read fHabilitado write fHabilitado
       default False ;

   end;

   { Lista de Objetos do tipo TACBrTEFDClass }

   { TACBrTEFDClasses }

   TACBrTEFDClassList = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TObject>{$EndIf})
     protected
       procedure SetObject (Index: Integer; Item: TACBrTEFDClass);
       function GetObject (Index: Integer): TACBrTEFDClass;
       procedure Insert (Index: Integer; Obj: TACBrTEFDClass);
     public
       function Add (Obj: TACBrTEFDClass): Integer;
       property Objects [Index: Integer]: TACBrTEFDClass
         read GetObject write SetObject; default;
     end;

   { TACBrTEFDClassTXT }

   TACBrTEFDClassTXT = class( TACBrTEFDClass )
   public
     constructor Create( AOwner : TComponent ) ; override;

   published
     property AutoAtivarGP ;

     property NumVias;
     property EsperaSTS;

     property ArqTemp  ;
     property ArqReq   ;
     property ArqSTS   ;
     property ArqResp  ;
     property GPExeName;
   end;


implementation

Uses
  dateutils, StrUtils, Math, {$IFDEF FMX} System.Types {$ELSE} types{$ENDIF},
  ACBrTEFD, ACBrTEFDCliSiTef, ACBrTEFCliSiTefComum, ACBrTEFDVeSPague,
  ACBrTEFDPayGo, ACBrTEFDPayGoWeb, ACBrTEFDDialScopeGetcard,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.FilesIO;

{ TACBrTEFDRespostasPendentes }

constructor TACBrTEFDRespostasPendentes.Create(FreeObjects: boolean);
begin
  inherited;
  fSaldoAPagar := 0;
end;

function TACBrTEFDRespostasPendentes.GetSaldoRestante: Double;
var
  I: Integer;
  TotalPagoENaoImpresso: Double;
begin
  TotalPagoENaoImpresso := 0;

  for I := 0 to Count - 1 do
  begin
    if Items[I] is TACBrTEFDResp then
    begin
      with TACBrTEFDResp(Items[I]) do
      begin
        if (OrdemPagamento = 0) then  // Ainda nao imprimiu no ECF ?
          TotalPagoENaoImpresso := TotalPagoENaoImpresso + (ValorTotal - Saque + Desconto);
      end;
    end;
  end;

  TotalPagoENaoImpresso := RoundTo(TotalPagoENaoImpresso, -2);

  Result := RoundTo(SaldoAPagar - TotalPagoENaoImpresso, -2);
end;

{ TACBrTEFDResp }

procedure TACBrTEFDResp.ProcessarTipoInterno(ALinha: TACBrTEFLinha);
begin
  if (ALinha.Identificacao = 899) then
  begin
    case ALinha.Sequencia of
      2 : fpIndiceFPG_ECF  := ALinha.Informacao.AsString;
      3 : fpOrdemPagamento := ALinha.Informacao.AsInteger;
    else
      inherited ProcessarTipoInterno(ALinha);
    end;
  end
  else
    inherited ProcessarTipoInterno(ALinha);
end;

procedure TACBrTEFDResp.SetIndiceFPG_ECF(const AValue: String);
begin
  fpConteudo.GravaInformacao(899, 2, AValue);
  fpIndiceFPG_ECF := AValue;
end;

procedure TACBrTEFDResp.SetOrdemPagamento(const AValue: Integer);
begin
  fpConteudo.GravaInformacao(899, 3, IntToStr(AValue));
  fpOrdemPagamento := AValue;
end;

procedure TACBrTEFDResp.Clear;
begin
  inherited Clear;
  fpIndiceFPG_ECF := '';
  fpOrdemPagamento := 0;
end;

procedure TACBrTEFDResp.Assign(Source: TACBrTEFResp);
begin
  inherited Assign(Source);

  if (Source is TACBrTEFDResp) then
    TipoGP := TACBrTEFDResp(Source).TipoGP;    { TipoGP não é salva em Conteudo (memory only) }
end;

{ TACBrTEFDReq }

constructor TACBrTEFDReq.Create ;
begin
  inherited Create;

  fConteudo := TACBrTEFArquivo.Create;
  fInformacao := TACBrInformacao.Create;

  Clear;
end;

destructor TACBrTEFDReq.Destroy;
begin
  fInformacao.Free ;
  fConteudo.Free;

  inherited ;
end;

procedure TACBrTEFDReq.Clear;
begin
  fConteudo.Clear;

  fID := 0 ;
  fHeader := '' ;
  fAgencia := '';
  fAgenciaDC := '';
  fBanco := '';
  fCheque := '';
  fChequeDC := '';
  fCMC7 := '';
  fCodigoAutorizacaoTransacao := '';
  fConta := '';
  fContaDC := '';
  fDataCheque := 0;
  fDataHoraTransacaoComprovante := 0;
  fDocumentoPessoa := '';
  fFinalizacao := '';
  fMoeda := 0;
  fNSU := '';
  fRede := '';
  fTipoPessoa := 'F';
  fValorTotal := 0;
  fDocumentoVinculado := '';
end;

procedure TACBrTEFDReq.GravaInformacao(const Identificacao : Integer;
   const Sequencia : Integer; const Informacao : String);
begin
   fConteudo.GravaInformacao( Identificacao, Sequencia, Informacao);
end;

procedure TACBrTEFDReq.SetHeader(const AValue : String);
begin
  fConteudo.GravaInformacao(0,0,AValue);
  fHeader := AValue;
end;

procedure TACBrTEFDReq.SetID(const AValue : Integer);
begin
  fInformacao.AsInteger := AValue;
  fConteudo.GravaInformacao(1,0,fInformacao);
  fID := AValue;
end;

procedure TACBrTEFDReq.SetDocumentoVinculado(const AValue : String);
begin
  fDocumentoVinculado := OnlyNumber(AValue);
  fConteudo.GravaInformacao(2,0,fDocumentoVinculado);
end;

procedure TACBrTEFDReq.SetValorTotal(const AValue : Double);
begin
  fInformacao.AsFloat := AValue;
  fConteudo.GravaInformacao(3,0,fInformacao);
  fValorTotal := AValue;
end;

procedure TACBrTEFDReq.SetMoeda(const AValue : Integer);
begin
  fInformacao.AsString := IntToStr(AValue);   // Converte para String para garantir a existencia
  fConteudo.GravaInformacao(4,0,fInformacao);
  fMoeda := AValue;
end;

procedure TACBrTEFDReq.SetCMC7(const AValue : String);
begin
  fConteudo.GravaInformacao(5,0,AValue);
  fCMC7 := AValue;
end;

procedure TACBrTEFDReq.SetTipoPessoa(const AValue : AnsiChar);
begin
  fConteudo.GravaInformacao(6,0,AValue);
  fTipoPessoa := AValue;
end;

procedure TACBrTEFDReq.SetDocumentoPessoa(const AValue : String);
begin
  fConteudo.GravaInformacao(7,0,AValue);
  fDocumentoPessoa := AValue;
end;

procedure TACBrTEFDReq.SetDataCheque(const AValue : TDateTime);
begin
  fInformacao.AsDate := AValue;
  fConteudo.GravaInformacao(8,0,fInformacao);
  fDataCheque := AValue;
end;

procedure TACBrTEFDReq.SetRede(const AValue : String);
begin
  fConteudo.GravaInformacao(10,0,AValue);
  fRede := AValue;
end;

procedure TACBrTEFDReq.SetNSU(const AValue : String);
begin
  fNSU := Trim(AValue);
  fConteudo.GravaInformacao(12,0,fNSU);
end;

procedure TACBrTEFDReq.SetCodigoAutorizacaoTransacao(AValue: String);
begin
  fCodigoAutorizacaoTransacao := Trim(AValue);
  fConteudo.GravaInformacao(13,0,fCodigoAutorizacaoTransacao);
end;

procedure TACBrTEFDReq.SetDataHoraTransacaoComprovante(const AValue : TDateTime);
begin
  fInformacao.AsDate := AValue;
  fConteudo.GravaInformacao(22,0,fInformacao);
  fInformacao.AsTime := AValue;
  fConteudo.GravaInformacao(23,0,fInformacao);
  fDataHoraTransacaoComprovante := AValue;
end;

procedure TACBrTEFDReq.SetFinalizacao(const AValue : String);
begin
  fConteudo.GravaInformacao(27,0,AValue);
  fFinalizacao := AValue;
end;

procedure TACBrTEFDReq.SetBanco(const AValue : String);
begin
  fConteudo.GravaInformacao(33,0,AValue);
  fBanco := AValue;
end;

procedure TACBrTEFDReq.SetAgencia(const AValue : String);
begin
  fConteudo.GravaInformacao(34,0,AValue);
  fAgencia := AValue;
end;

procedure TACBrTEFDReq.SetAgenciaDC(const AValue : String);
begin
  fConteudo.GravaInformacao(35,0,AValue);
  fAgenciaDC := AValue;
end;

procedure TACBrTEFDReq.SetConta(const AValue : String);
begin
  fConteudo.GravaInformacao(36,0,AValue);
  fConta := AValue;
end;

procedure TACBrTEFDReq.SetContaDC(const AValue : String);
begin
  fConteudo.GravaInformacao(37,0,AValue);
  fContaDC := AValue;
end;

procedure TACBrTEFDReq.SetCheque(const AValue : String);
begin
  fConteudo.GravaInformacao(38,0,AValue);
  fCheque := AValue;
end;

procedure TACBrTEFDReq.SetChequeDC(const AValue : String);
begin
  fConteudo.GravaInformacao(39,0,AValue);
  fChequeDC := AValue;
end;

{ TACBrTEFDRespTXT }

procedure TACBrTEFDRespTXT.ConteudoToProperty;
var
   Linha: TACBrTEFLinha ;
   I, L: Integer;
   Parc: TACBrTEFRespParcela;
   TemParcelas: Boolean ;
   LinhaComprovante: String;
begin
   fpDataHoraTransacaoComprovante := 0 ;
   fpImagemComprovante1aVia.Clear;
   fpImagemComprovante2aVia.Clear;
   TemParcelas := False;

   for I := 0 to Conteudo.Count - 1 do
   begin
     Linha := Conteudo.Linha[I];

     case Linha.Identificacao of
       0   : fpHeader := Linha.Informacao.AsString;
       1   : fpID := Linha.Informacao.AsInteger;
       2   : fpDocumentoVinculado := Linha.Informacao.AsString;
       3   : fpValorTotal := Linha.Informacao.AsFloat;
       4   : fpMoeda := Linha.Informacao.AsInteger;
       5   : fpCMC7 := Linha.Informacao.AsString;
       6   : fpTipoPessoa := AnsiChar(PadRight(Linha.Informacao.AsString, 1 )[ 1 ]);
       7   : fpDocumentoPessoa := Linha.Informacao.AsString;
       8   : fpDataCheque := Linha.Informacao.AsDate;
       9   : fpStatusTransacao := Linha.Informacao.AsString;
       10  :
         begin
           case Linha.Sequencia of
             0 : fpRede := Linha.Informacao.AsString;
             1 : fpNFCeSAT.CNPJCredenciadora := Linha.Informacao.AsString;
             4 : fpBin := Linha.Informacao.AsString; //Seis primeiros digitos do cartão
             5 : fpNFCeSAT.UltimosQuatroDigitos := Linha.Informacao.AsString;
           end;
         end;
       11  : fpTipoTransacao := Linha.Informacao.AsInteger;
       12  : fpNSU := Linha.Informacao.AsString;
       13  : fpCodigoAutorizacaoTransacao := Linha.Informacao.AsString;
       14  : fpNumeroLoteTransacao := Linha.Informacao.AsInteger;
       15  : fpDataHoraTransacaoHost := Linha.Informacao.AsTimeStamp;
       16  : fpDataHoraTransacaoLocal := Linha.Informacao.AsTimeStamp;
       17  :
         begin
           fpTipoParcelamento := Linha.Informacao.AsInteger;
           case fpTipoParcelamento  of
              0 : fpParceladoPor := parcLoja;
              1 : fpParceladoPor := parcADM;
           else
              fpParceladoPor := parcNenhum;
           end;
         end;
       18  : fpQtdParcelas := Linha.Informacao.AsInteger;
       19  : TemParcelas := True ;
       22  : fpDataHoraTransacaoComprovante := fpDataHoraTransacaoComprovante +
                                               Linha.Informacao.AsDate;
       23  : fpDataHoraTransacaoComprovante := fpDataHoraTransacaoComprovante +
                                               Linha.Informacao.AsTime;
       24  : fpDataPreDatado := Linha.Informacao.AsDate;
       25  : fpNSUTransacaoCancelada := Linha.Informacao.AsString;
       26  : fpDataHoraTransacaoCancelada := Linha.Informacao.AsTimeStamp;
       27  : fpFinalizacao := Linha.Informacao.AsString;
       28  : fpQtdLinhasComprovante := Linha.Informacao.AsInteger;
       30  : fpTextoEspecialOperador := Linha.Informacao.AsString;
       31  : fpTextoEspecialCliente := Linha.Informacao.AsString;
       32  : fpAutenticacao := Linha.Informacao.AsString;
       33  : fpBanco := Linha.Informacao.AsString;
       34  : fpAgencia := Linha.Informacao.AsString;
       35  : fpAgenciaDC := Linha.Informacao.AsString;
       36  : fpConta := Linha.Informacao.AsString;
       37  : fpContaDC := Linha.Informacao.AsString;
       38  : fpCheque := Linha.Informacao.AsString;
       39  : fpChequeDC  := Linha.Informacao.AsString;
       40  : fpNomeAdministradora := Linha.Informacao.AsString;
       131 : fpInstituicao := Linha.Informacao.AsString;
       132 : fpCodigoBandeiraPadrao := Linha.Informacao.AsString;
       136 : fpBin := Linha.Informacao.AsString;

       300 :
         case Linha.Sequencia of
           1 : fpNFCeSAT.DataExpiracao := Linha.Informacao.AsString;
           2 : fpNFCeSAT.DonoCartao := Linha.Informacao.AsString;
         end;

       600 : fpNFCeSAT.CNPJCredenciadora := Linha.Informacao.AsString;
       601 : fpNFCeSAT.Bandeira := Linha.Informacao.AsString;
       602 : fpNFCeSAT.Autorizacao := Linha.Informacao.AsString;
       603 : fpNFCeSAT.CodCredenciadora := Linha.Informacao.AsString;
       565 :
        case Linha.Sequencia of
           10 :
           begin
             L := StrToIntDef(copy(Linha.Informacao.AsString,3,2),2);
             fpDocumentoPessoa := copy(Linha.Informacao.AsString, 5, L);
           end;
        end;
       999 : fpTrailer := Linha.Informacao.AsString ;
     else
        ProcessarTipoInterno(Linha);
     end;
   end ;

   // Processando as Vias dos Comprovantes //
   if (fpQtdLinhasComprovante > 0) then
   begin
     I := 1;
     while (I <= fpQtdLinhasComprovante) do
     begin
       LinhaComprovante := LeInformacao(29 , I).AsString;
       fpImagemComprovante1aVia.Add( AjustaLinhaImagemComprovante(LinhaComprovante) );
       Inc(I);
     end;

     while (Conteudo.AchaLinha(29, I) >= 0 ) do
     begin
       LinhaComprovante := Trim(LeInformacao(29 , I).AsString);
       fpImagemComprovante2aVia.Add( AjustaLinhaImagemComprovante(LinhaComprovante) );
       Inc(I);
     end;

     if (fpImagemComprovante2aVia.Count = 0) then
        fpImagemComprovante2aVia.Text := fpImagemComprovante1aVia.Text;
   end;

   fpConfirmar := (fpQtdLinhasComprovante > 0);
   fpSucesso := (fpStatusTransacao = '0');

   fpParcelas.Clear;
   if TemParcelas then
   begin
     for I := 1 to fpQtdParcelas do
     begin
       Parc := TACBrTEFRespParcela.create;
       Parc.Vencimento := LeInformacao( 19 , I).AsDate ;
       Parc.Valor := LeInformacao( 20 , I).AsFloat ;
       Parc.NSUParcela := LeInformacao( 21 , I).AsString ;

       fpParcelas.Add(Parc);
     end;
   end;

   // Tipo da transação se foi Crédito ou Débito
   fpDebito := ((fpTipoTransacao >= 20) and (fpTipoTransacao <= 25)) or (fpTipoTransacao = 40) ;
   fpCredito := (fpTipoTransacao >= 10) and (fpTipoTransacao <= 12) ;

   case fpTipoTransacao of
     10,20,23:
       fpTipoOperacao := opAvista;
     11,12,22:
       fpTipoOperacao := opParcelado;
     21,24,25:
       begin
         fpTipoOperacao := opPreDatado;
         fpDataPreDatado := LeInformacao(24).AsDate;
       end;
     40 :
       begin
         fpTipoOperacao := opParcelado;
         fpParceladoPor := parcADM;
       end;
   else
     fpTipoOperacao := opOutras;
   end;
end;

function TACBrTEFDRespTXT.GetTransacaoAprovada : Boolean;
begin
   Result := (StrToIntDef(Trim(fpStatusTransacao),-1) = 0) or
             (UpperCase(Trim(fpStatusTransacao)) = 'P1') ;  // Consulta de Cheque
end;

function TACBrTEFDRespTXT.AjustaLinhaImagemComprovante( Linha: AnsiString ) : AnsiString;
begin
   Result := Linha;

   if LeftStr(Result,1) = '"' then
      Delete(Result,1,1);
   if RightStr(Result,1) = '"' then
      Delete(Result,Length(Result),1);
end;

function TACBrTEFDRespTXT.GetTrailerOK : Boolean;
begin
  Result :=  (fpTrailer <> '') ;
end;


{ TACBrTEFDClassTXT }

constructor TACBrTEFDClassTXT.Create(AOwner : TComponent);
begin
   inherited Create(AOwner);

   if Assigned( fpResp ) then
      fpResp.Free ;
   fpResp := TACBrTEFDRespTXT.Create;
   fpResp.TipoGP := Tipo;
end;

{ TACBrTEFDClass }

constructor TACBrTEFDClass.Create(AOwner : TComponent);
begin
  if not (AOwner is TACBrTEFD) then
     raise EACBrTEFDErro.Create( ACBrStr('TACBrTEFDClass deve ser criado por TACBrTEFD') ) ;

  inherited Create(AOwner);

  fAutoAtivarGP := True ;
  fLogDebug     := False;
  fArqLOG       := '' ;
  fArqReq       := '' ;
  fArqTmp       := '' ;
  fArqResp      := '' ;
  fArqSTS       := '' ;
  fpTipo        := gpNenhum ;
  fpIDSeq       := SecondOfTheDay(now) ;
  fpNumVias     := CACBrTEFD_NumVias ;

  fpAguardandoResposta := False ;
  fpSalvarArquivoBackup := True;

  fEsperaSTS := CACBrTEFD_EsperaSTS ;
  fpResp     := TACBrTEFDResp.Create;
  fpReq      := TACBrTEFDReq.Create;

  TACBrTEFD(Owner).EstadoReq  := reqNenhum;
  TACBrTEFD(Owner).EstadoResp := respNenhum;
end;

destructor TACBrTEFDClass.Destroy;
begin
  fpResp.Free ;
  fpReq.Free ;

  inherited Destroy;
end;

procedure TACBrTEFDClass.VerificaInicializado;
begin
  if not fpInicializado then
     raise EACBrTEFDGPNaoInicializado.Create(
        ACBrStr('Gerenciador Padrão: '+Name+' não foi inicializado')) ;
end;

procedure TACBrTEFDClass.SetInicializado(const AValue : Boolean);
begin
  if AValue then
     Inicializar
  else
     DesInicializar ;
end;

procedure TACBrTEFDClass.Inicializar;
begin
  if Inicializado then exit ;

  ApagaEVerifica( ArqTemp );  // Apagando Arquivo Temporario anterior //
  ApagaEVerifica( ArqReq );   // Apagando Arquivo de Requisicao anterior //
  ApagaEVerifica( ArqSTS );   // Apagando Arquivo de Status anterior //

  fpInicializado := True ;
  GravaLog( Name +' Inicializado' );

  { Verificando se o arquivo de Resposta é invalido ou seja, gerado quando
    clica-se em 9 - CANCELAR sem selecionar nenhuma Bandeira }
  if FileExists( ArqResp ) then
  begin
     Resp.LeArquivo( ArqResp );

     // Amex retorna 101 e não FF
     if (pos(UpperCase('|'+Trim(Resp.Conteudo.LeInformacao(9,0).AsString)+'|'), '|FF|101|') > 0) then
        ApagaEVerifica( ArqResp );

     Resp.Clear;
  end ;

  VerificarTransacoesPendentesClass(TACBrTEFD(Owner).ConfirmarAntesDosComprovantes);

  VerificaAtivo;
end;

procedure TACBrTEFDClass.DesInicializar;
begin
  fpInicializado := False ;
  GravaLog( Name +' DesInicializado' );
end;

procedure TACBrTEFDClass.VerificaAtivo;
begin
  try
    ATV;
  except
    on E : EACBrTEFDGPNaoResponde do
    begin
      if AutoAtivarGP then
      begin
        TACBrTEFD(Owner).DoExibeMsg( opmOK,
              'O Gerenciador Padrão não está ativo e será ativado automaticamente!');
        AtivarGP;
        ATV;
      end
      else
        raise;
    end
    else
      raise;
  end;
end;

procedure TACBrTEFDClass.VerificarTransacoesPendentesClass(aVerificarCupom: Boolean);
var
  wEstadoECF: AnsiChar;
begin
  if aVerificarCupom then
  begin
    try
      wEstadoECF := TACBrTEFD(Owner).EstadoECF;
    except
      wEstadoECF := 'O';
      { Se o ECF estiver desligado, será retornado 'O', o que fará o código
        abaixo Cancelar Todas as Transações Pendentes, porém, pelo Roteiro do
        TEF dedicado, é necessário confirmar a Transação se o Cupom foi
        finalizado com sucesso.
          Criar um arquivo de Status que seja atualizado no Fim do Cupom e no
        inicio do CCD, de maneira que seja possível identificar o Status do
        Documento no ECF indepentende do mesmo estar ou não ligado

          Como alteranativa, é possível implementar código no Evento "OnInfoECF"
        para buscar o Status do Documento no Banco de dados da sua aplicação, e
        responder diferente de 'O',   (Veja exemplo nos fontes do TEFDDemo) }
    end;

    TACBrTEFD(Owner).GPAtual := Tipo;

    // Cupom Ficou aberto?? ...Se SIM, Cancele tudo... //
    if (wEstadoECF in ['V', 'P', 'N', 'O']) then
      CancelarTransacoesPendentesClass
    else
      // NAO, Cupom Fechado, Pode confirmar e Mandar aviso para re-imprimir //
      ConfirmarESolicitarImpressaoTransacoesPendentes;
  end
  else
    CancelarTransacoesPendentesClass;
end;

procedure TACBrTEFDClass.ATV;
begin
  IniciarRequisicao('ATV');
  FinalizarRequisicao;
end;

function TACBrTEFDClass.ADM: Boolean;
begin
  IniciarRequisicao('ADM');
  AdicionarIdentificacao;
  FinalizarRequisicao;

  LerRespostaRequisicao;
  Result := Resp.TransacaoAprovada ;
  try
     ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
  finally
     FinalizarResposta( True ) ; { True = Apaga Arquivo de Resposta }
  end;
end;

function TACBrTEFDClass.CRT(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
begin
  VerificarTransacaoPagamento( Valor );

  IniciarRequisicao('CRT');
  Req.DocumentoVinculado  := DocumentoVinculado;
  Req.ValorTotal          := Valor;
  Req.Moeda               := Moeda;
  AdicionarIdentificacao;
  FinalizarRequisicao;

  Result := ProcessarRespostaPagamento( IndiceFPG_ECF, Valor);
end;

function TACBrTEFDClass.CHQ(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; CMC7: String; TipoPessoa: AnsiChar;
  DocumentoPessoa: String; DataCheque: TDateTime; Banco: String;
  Agencia: String; AgenciaDC: String; Conta: String; ContaDC: String;
  Cheque: String; ChequeDC: String; Compensacao: String): Boolean;
begin
  // Compensacao não é utilizado em TEF discado //

  VerificarTransacaoPagamento( Valor );

  IniciarRequisicao('CHQ');
  Req.DocumentoVinculado := DocumentoVinculado;
  Req.ValorTotal         := Valor;
  Req.CMC7               := CMC7;
  if TipoPessoa <> ' ' then
     Req.TipoPessoa      := TipoPessoa;
  Req.DocumentoPessoa    := DocumentoPessoa;
  Req.DataCheque         := DataCheque;
  Req.Banco              := Banco;
  Req.Agencia            := Agencia;
  Req.AgenciaDC          := AgenciaDC;
  Req.Conta              := Conta;
  Req.ContaDC            := ContaDC;
  Req.Cheque             := Cheque;
  Req.ChequeDC           := ChequeDC;
  Req.Moeda              := 0;            // Moeda 0 = Real
  AdicionarIdentificacao;
  FinalizarRequisicao;

  LerRespostaRequisicao;

  try
    if (Resp.QtdLinhasComprovante <= 0) and (Resp.StatusTransacao = '0') then
       ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
  finally
     Result := ProcessarRespostaPagamento( IndiceFPG_ECF, Valor);
  end;
end;

function TACBrTEFDClass.CNC: Boolean;
Var
  OldResp : TACBrTEFDResp ;
begin
  OldResp := CriarResposta(fpTipo);
  try
     { Salvando dados da Resposta Atual, pois 'Resp' será zerado em "IniciarRequisicao" }
     OldResp.Assign(Resp);

     IniciarRequisicao('CNC');
     Req.DocumentoVinculado           := OldResp.DocumentoVinculado;
     Req.ValorTotal                   := OldResp.ValorTotal;
     Req.CMC7                         := OldResp.CMC7;
     if OldResp.TipoPessoa <> ' ' then
        Req.TipoPessoa                := OldResp.TipoPessoa;
     Req.DocumentoPessoa              := OldResp.DocumentoPessoa;
     Req.DataCheque                   := OldResp.DataCheque;
     Req.Rede                         := OldResp.Rede;
     Req.NSU                          := OldResp.NSU;
     Req.DataHoraTransacaoComprovante := OldResp.DataHoraTransacaoComprovante;
     Req.CodigoAutorizacaoTransacao   := OldResp.CodigoAutorizacaoTransacao;
     Req.Banco                        := OldResp.Banco;
     Req.Agencia                      := OldResp.Agencia;
     Req.AgenciaDC                    := OldResp.AgenciaDC;
     Req.Conta                        := OldResp.Conta;
     Req.ContaDC                      := OldResp.ContaDC;
     Req.Cheque                       := OldResp.Cheque;
     Req.ChequeDC                     := OldResp.ChequeDC;
     Req.Moeda                        := OldResp.Moeda;
     AdicionarIdentificacao;
     FinalizarRequisicao;

     LerRespostaRequisicao;
     Result := Resp.TransacaoAprovada;

     try
        ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
     finally
        FinalizarResposta( True ) ; { True = Apaga Arquivo de Resposta }
     end;
  finally
     OldResp.Free;
  end;
end;

function TACBrTEFDClass.CNC(Rede, NSU: String; DataHoraTransacao: TDateTime;
  Valor: Double; CodigoAutorizacaoTransacao: String): Boolean;
begin
  IniciarRequisicao('CNC');
  Req.ValorTotal := Valor;
  Req.Rede := Rede;
  Req.NSU := NSU;
  Req.DataHoraTransacaoComprovante := DataHoraTransacao;
  if (CodigoAutorizacaoTransacao <> '') then
    Req.CodigoAutorizacaoTransacao := CodigoAutorizacaoTransacao;

  AdicionarIdentificacao;
  FinalizarRequisicao;

  LerRespostaRequisicao;
  Result := Resp.TransacaoAprovada;
  try
     ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
  finally
     FinalizarResposta( True ) ; { True = Apaga Arquivo de Resposta }
  end;
end;

procedure TACBrTEFDClass.CNF;
begin
  CNF( Resp.Rede, Resp.NSU, Resp.Finalizacao, Resp.DocumentoVinculado ) ;
end;

procedure TACBrTEFDClass.CNF(Rede, NSU, Finalizacao: String;
  DocumentoVinculado: String);
begin
  IniciarRequisicao('CNF');
  Req.DocumentoVinculado := DocumentoVinculado;
  Req.Rede               := Rede;
  Req.NSU                := NSU;
  Req.Finalizacao        := Finalizacao;

  FinalizarRequisicao;
end;

procedure TACBrTEFDClass.NCN;
begin
  NCN( Resp.Rede, Resp.NSU, Resp.Finalizacao, Resp.ValorTotal,
       Resp.DocumentoVinculado );
end;

procedure TACBrTEFDClass.NCN(Rede, NSU, Finalizacao : String;
  Valor : Double = 0; DocumentoVinculado : String = '');
Var
  MsgStr : String ;
begin
  IniciarRequisicao('NCN');
  Req.DocumentoVinculado := DocumentoVinculado;
  Req.Rede               := Rede;
  Req.NSU                := NSU;
  Req.Finalizacao        := Finalizacao;

  FinalizarRequisicao;

  MsgStr := '' ;
  if Rede <> '' then
     MsgStr := MsgStr + 'Rede: '+Rede + sLineBreak;
  if NSU <> '' then
     MsgStr := MsgStr + 'NSU: '+NSU + sLineBreak;
  if Valor <> 0 then
     MsgStr := MsgStr + 'Valor: '+FormatFloat('0.00',Valor);

  MsgStr := 'Última Transação TEF foi cancelada' +
            IfThen(MsgStr <> '' , sLineBreak+sLineBreak, '' ) +
            MsgStr ;

  { TEF Auttar deve emitir msg apenas no Final de todos Desfazimentos }
  if Tipo <> gpTefAuttar then
     TACBrTEFD(Owner).DoExibeMsg( opmOK, MsgStr ) ;
end;

procedure TACBrTEFDClass.AtivarGP;
begin
  if (GPExeName = '') then
    raise EACBrTEFDErro.Create(ACBrStr('Nome do executável do Gerenciador Padrão não definido'));

  VerificaInicializado;

  RunCommand( GPExeName );
  Sleep(2000);
  TACBrTEFD(Owner).RestaurarFocoAplicacao;
end;

procedure TACBrTEFDClass.IniciarRequisicao( AHeader : String;
   AID : Integer = 0);
begin
  if fpAguardandoResposta then
     raise EACBrTEFDErro.Create( ACBrStr( 'Requisição anterior não concluida' ) ) ;

  GravaLog( Name +' IniciarRequisicao: '+AHeader );

  { Verificando se a Classe de TEF está Inicializado }
  VerificaInicializado ;

  { Transação a ser enviada é pagamento ? (CRT ou CHQ) }
  if TransacaoEPagamento(AHeader) then
  begin
     { Para TEF DISCADO tradicional, se for MultiplosCartoes, precisa
       confirma a transação anterior antes de enviar uma nova }
     if TACBrTEFD(Owner).MultiplosCartoes then      // É multiplos cartoes ?
        ConfirmarTransacoesAnteriores;
  end;

  { VisaNET exige um ATV antes de cada transação }
  if (AHeader <> 'ATV') then
     VerificaAtivo;

  TACBrTEFD(Owner).EstadoReq := reqIniciando;

  { Limpando da Memória os dados do Objeto de Requisicao e Resposta }
  Req.Clear;
  Resp.Clear ;

  ApagaEVerifica( ArqTemp );  // Apagando Arquivo Temporario anterior //
  ApagaEVerifica( ArqReq  );  // Apagando Arquivo de Requisicao anterior //
  ApagaEVerifica( ArqResp );  // Apagando Arquivo de Resposta anterior //
  ApagaEVerifica( ArqSTS  );  // Apagando Arquivo de Status anterior //

  if AID > 0 then
     fpIDSeq := AID
  else
     fpIDSeq := fpIDSeq + 1 ;

  Req.Header := AHeader;
  Req.ID     := fpIDSeq;
  Req.Conteudo.GravarArquivo( ArqTemp );
end;

procedure TACBrTEFDClass.FinalizarRequisicao;
Var
  TempoInicioEspera, TempoFimEspera : TDateTime ;
  Interromper : Boolean ;
begin
  VerificarIniciouRequisicao;

  with TACBrTEFD(Owner) do
  begin
     EstadoReq := reqCriandoArquivo;

     if Assigned( OnAntesFinalizarRequisicao ) then
        OnAntesFinalizarRequisicao( Self.Req );
  end ;

  GravaLog( Name +' FinalizarRequisicao: '+Req.Header+', Fechando arquivo: '+ArqTemp);
  Req.Conteudo.GravaInformacao(999,999,'0');
  Req.Conteudo.GravarArquivo( ArqTemp, True ); { True = DoFlushToDisk }

  if fLogDebug then
     GravaLog( Req.Conteudo.Conteudo.Text );

  GravaLog( Name +' FinalizarRequisicao: '+Req.Header+', Renomeando: '+ArqTemp+' para: '+ArqReq);
  if not RenameFile( ArqTemp, ArqReq ) then
     raise EACBrTEFArquivo.Create( ACBrStr( 'Erro ao Renomear:' + sLineBreak +
                                ArqTemp + 'para:' + sLineBreak + ArqReq ) ) ;

  TACBrTEFD(Owner).EstadoReq := reqAguardandoResposta;

  TempoInicioEspera := now ;
  TempoFimEspera    := IncSecond(TempoInicioEspera, EsperaSTS );
  Interromper       := False ;
  fpAguardandoResposta := True ;
  try
     GravaLog( Name +' FinalizarRequisicao: '+Req.Header+', Aguardando: '+ArqSTS );

     repeat
        Sleep( TACBrTEFD(Owner).EsperaSleep );  // Necessário Para não sobrecarregar a CPU //

        with TACBrTEFD(Owner) do
        begin
           if Assigned( OnAguardaResp ) then
              OnAguardaResp( ArqSTS, SecondsBetween(TempoFimEspera, Now), Interromper ) ;
        end;
     until FileExists( ArqSTS ) or ( now > TempoFimEspera ) or Interromper;

     GravaLog( Name +' FinalizarRequisicao: '+Req.Header+', Fim da Espera de: '+
               ArqSTS+' '+ifthen(FileExists( ArqSTS ),'Recebido','Não recebido') );
  finally
     fpAguardandoResposta := False ;
     with TACBrTEFD(Owner) do
     begin
        if Assigned( OnAguardaResp ) then
           OnAguardaResp( ArqSTS, -1, Interromper ) ;
     end;
  end;

  if not FileExists( ArqSTS ) then
     raise EACBrTEFDGPNaoResponde.Create(
        ACBrStr( Format(CACBrTEFD_Erro_NaoAtivo, [Self.Name]) ) ) ;

  TACBrTEFD(Owner).EstadoReq := reqConferindoResposta;

  GravaLog( Name +' FinalizarRequisicao: '+Req.Header+', Verificando conteudo de: '+ArqSTS );
  Resp.LeArquivo( ArqSTS );

  DeleteFile( ArqTemp );
  DeleteFile( ArqSTS );

  if not VerificarRespostaRequisicao then
  begin
    DeleteFile( ArqResp );

    Req.Clear;
    Resp.Clear;

    raise EACBrTEFDSTSInvalido.Create( ACBrStr('Falha na comunicação com o Gereciador Padrão: '+Name) ) ;
  end;

  TACBrTEFD(Owner).EstadoReq := reqFinalizada;
end;

procedure TACBrTEFDClass.AdicionarIdentificacao;
begin
  { Código migrado para classe TACBrTEFDPayGo }
end;

function TACBrTEFDClass.VerificarRespostaRequisicao: Boolean;
begin
  Result := True ;
  if Resp is TACBrTEFDRespTXT then
    with TACBrTEFDRespTXT( Resp ) do
      Result := (Header = Req.Header) and (ID = Req.ID) and TrailerOk ;
end;

procedure TACBrTEFDClass.LerRespostaRequisicao;
var
   TempoInicioEspera : Double;
   Interromper, OK   : Boolean;

begin
  VerificarIniciouRequisicao;
  Resp.Clear;

  if pos(Req.Header,'CNF|ATV') = 0 then      // Se nao for ATV ou CNF...
  begin
     with TACBrTEFD(Owner) do
     begin
        if TecladoBloqueado then             // Teclado está bloqueado ?
           BloquearMouseTeclado( False );    // Desbloqueia o Teclado para o G.P. funcionar
     end;
  end;

  TACBrTEFD(Owner).EstadoResp  := respAguardandoResposta;
  Interromper := False ;
  OK          := False ;
  try
     while not (OK or Interromper) do
     begin
        TempoInicioEspera := now ;
        fpAguardandoResposta := True ;
        try
           GravaLog( Name +' LerRespostaRequisicao: '+Req.Header+', Aguardando: '+ArqResp );

           repeat
              Sleep( TACBrTEFD(Owner).EsperaSleep );  // Necessário Para não sobrecarregar a CPU //

              with TACBrTEFD(Owner) do
              begin
                 if Assigned( OnAguardaResp ) then
                    OnAguardaResp( ArqResp, SecondsBetween(TempoInicioEspera, Now),
                                   Interromper ) ;
              end ;
           until FileExists( ArqResp ) or Interromper ;

           GravaLog( Name +' LerRespostaRequisicao: '+Req.Header+', Fim da Espera de: '+
                     ArqResp+' '+ifthen(FileExists( ArqResp ),'Recebido','Não recebido') );
        finally
           fpAguardandoResposta := False ;
           with TACBrTEFD(Owner) do
           begin
              if Assigned( OnAguardaResp ) then
                 OnAguardaResp( ArqResp, -1, Interromper ) ;
           end ;
        end;

        GravaLog( Name +' LerRespostaRequisicao: '+Req.Header+', Verificando conteudo de: '+ArqResp );
        Resp.LeArquivo( ArqResp );
        Ok := VerificarRespostaRequisicao ;

        if not Ok then
        begin
           GravaLog( Name +' LerRespostaRequisicao: '+Req.Header+', Arquivo inválido desprezado: "'+ArqResp+'"'+sLineBreak +
                     Resp.Conteudo.Conteudo.Text );
           Resp.Clear;
           DeleteFile( ArqResp );
        end ;

        if fLogDebug then
           GravaLog( Resp.Conteudo.Conteudo.Text );
     end ;
  finally
    Resp.TipoGP := Tipo;
    TACBrTEFD(Owner).EstadoResp := respNenhum;
    DeleteFile( ArqReq );  // Apaga a Requisicao (caso o G.P. nao tenha apagado)
  end ;
end;

function TACBrTEFDClass.PRE(Valor: Double; DocumentoVinculado: String;
  Moeda: Integer): Boolean;
begin
  IniciarRequisicao('PRE');
  Req.DocumentoVinculado  := DocumentoVinculado;
  Req.ValorTotal          := Valor;
  Req.Moeda               := Moeda;
  AdicionarIdentificacao;
  FinalizarRequisicao;
  LerRespostaRequisicao;
  Result := Resp.TransacaoAprovada ;
  try
     ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
  finally
     FinalizarResposta( True ) ; { True = Apaga Arquivo de Resposta }
  end;
end;

procedure TACBrTEFDClass.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  raise EACBrTEFDErro.Create('ExibirMensagemPinPad não disponível para: '+ClassName) ;
end;

procedure TACBrTEFDClass.ProcessarResposta ;
var
   RespostaPendente: TACBrTEFDResp;
begin
  VerificarIniciouRequisicao;

  GravaLog( Name +' ProcessarResposta: '+Req.Header );

  TACBrTEFD(Owner).EstadoResp := respProcessando;

  if Resp.QtdLinhasComprovante > 0 then
  begin
    { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
    RespostaPendente := CriarResposta(fpTipo);
    try
      RespostaPendente.Assign( Resp );
      TACBrTEFD(Owner).RespostasPendentes.Add( RespostaPendente );

      ImprimirRelatorio ;

      with TACBrTEFD(Owner) do
      begin
        if Assigned( OnDepoisConfirmarTransacoes ) then
          OnDepoisConfirmarTransacoes( RespostasPendentes );
      end ;
    finally
      TACBrTEFD(Owner).RespostasPendentes.Clear;
    end;
  end
  else
    if Resp.TextoEspecialOperador <> '' then
      TACBrTEFD(Owner).DoExibeMsg( opmOK, Resp.TextoEspecialOperador )
end;

procedure TACBrTEFDClass.FinalizarResposta( ApagarArqResp : Boolean );
begin
   TACBrTEFD(Owner).EstadoResp := respConcluida;

   GravaLog( Name +' FinalizarResposta: '+Req.Header );

   if ApagarArqResp then
      ApagaEVerifica( ArqResp );

   Req.Clear;
   Resp.Clear;
end;

procedure TACBrTEFDClass.CancelarTransacoesPendentesClass;
Var
  ArquivosVerficar    : TStringList ;
  RespostaCancela     : TACBrTEFDResp ;
  RespostasCanceladas : TACBrTEFDRespostasPendentes ;
  I, Topo             : Integer;
  JaCancelado, Ok     : Boolean ;
  ArqMask             : String;
begin
  GravaLog( Name +' CancelarTransacoesPendentesClass ');

  ArquivosVerficar    := TStringList.Create;
  RespostasCanceladas := TACBrTEFDRespostasPendentes.create(True);

  try
     ArquivosVerficar.Clear;
     RespostasCanceladas.Clear;

     { Achando Arquivos de Backup deste GP }
     ArqMask := TACBrTEFD(Owner).PathBackup + PathDelim + 'ACBr_' + Self.Name + '_*.tef' ;
     FindFiles( ArqMask, ArquivosVerficar, True );

     { Vamos processar primeiro os CNCs e ADMs, e as Não Confirmadas }
     I    := ArquivosVerficar.Count-1 ;
     Topo := 0 ;
     while I > Topo do
     begin
        Resp.LeArquivo( ArquivosVerficar[ I ] );

        if (pos(Resp.Header, 'CNC,ADM') > 0) or (not Resp.CNFEnviado) then
         begin
           ArquivosVerficar.Move(I,Topo);
           Topo := Topo + 1;
         end
        else
           I := I - 1 ;
     end;

     { Adicionando Arquivo de Resposta deste GP (se ainda não foi apagado) }
     if FileExists( ArqResp ) then
        ArquivosVerficar.Add( ArqResp );

     { Enviando NCN ou CNC para todos os arquivos encontrados }
     while ArquivosVerficar.Count > 0 do
     begin
        if not FileExists( ArquivosVerficar[ 0 ] ) then
        begin
           ArquivosVerficar.Delete( 0 );
           Continue;
        end;

        Resp.LeArquivo( ArquivosVerficar[ 0 ] );

        {  Verificando se essa Resposta já foi cancela em outro arquivo }
        JaCancelado := False ;
        I := 0 ;
        while (not JaCancelado) and (I < RespostasCanceladas.Count) do
        begin
           if (RespostasCanceladas[I] is TACBrTEFDRespTXT) or
              (Tipo = gpVeSPague) then
            begin
              with RespostasCanceladas[I] do
              begin
                 JaCancelado := (Resp.Rede        = Rede)        and
                                (Resp.NSU         = NSU)         and
                                (Resp.Finalizacao = Finalizacao) and
                                (Resp.ValorTotal  = ValorTotal) ;
              end;
            end
           else
            begin
              with RespostasCanceladas[I] do
              begin
                 JaCancelado := (Resp.DocumentoVinculado = DocumentoVinculado) ;

                 // Se a resposta já foi cancelada em outro arquivo, adiciona a lista de respostas canceladas para que no evento
                 // OnDepoisCancelarTransacoes a lista retornada no argumento RespostasCanceladas esteja devidamente preenchida com todas transações canceladas.
                 if JaCancelado then
                 begin
                   RespostaCancela := CriarResposta( Tipo );
                   RespostaCancela.Assign( Resp );
                   RespostasCanceladas.Add( RespostaCancela );
                 end;
              end;
            end;

           Inc( I ) ;
        end;

        if not JaCancelado then
         begin
           { Criando cópia da Resposta Atual }
           RespostaCancela := CriarResposta( Tipo );
           RespostaCancela.Assign( Resp );

           { Enviando NCN ou CNC }
           try
              with TACBrTEFD(Owner) do
              begin
                 if Assigned( OnAntesCancelarTransacao ) then
                 try
                    OnAntesCancelarTransacao( RespostaCancela ) ;
                 except
                    { Nao deixa exceptions em OnAntesCancelarTransacao interromper }
                 end;
              end;

              Ok := True;
              if Resp.CNFEnviado and (Resp.Header <> 'CHQ') then
               begin
                 Ok := CNC;
                 if not Ok then
                   if (TACBrTEFD(Owner).DoExibeMsg( opmYesNo, CACBrTEFD_Erro_CNCNaoEfetuado ) = mrYes) then
                      raise EACBrTEFDErro.Create('CNC nao efetuado');
               end
              else
                 NCN;

              DeleteFile( ArquivosVerficar[ 0 ] );
              ArquivosVerficar.Delete( 0 );

              { Adicionando na lista de Respostas Canceladas }
              if Ok then
                RespostasCanceladas.Add( RespostaCancela );
           except
           end;
         end
        else
         begin
           DeleteFile( ArquivosVerficar[ 0 ] );
           ArquivosVerficar.Delete( 0 );
         end;
     end;

     with TACBrTEFD(Owner) do
     begin
       if Assigned( OnDepoisCancelarTransacoes ) then
          OnDepoisCancelarTransacoes( RespostasCanceladas );

       { TEF Auttar deve emitir msg apenas no Final de todos Desfazimentos }
       if Tipo = gpTefAuttar then
          if RespostasCanceladas.count > 0 then
             DoExibeMsg( opmOK, 'Transação TEF não Efetuada Reter o Cupom Fiscal!' ) ;
     end;
  finally
     ArquivosVerficar.Free;
     RespostasCanceladas.Free;
  end;
end;

function TACBrTEFDClass.CDP(const EntidadeCliente: String; out Resposta: String): Boolean;
begin
  Resposta := '';
  IniciarRequisicao('CDP');
  Req.Conteudo.GravaInformacao(006, 000, EntidadeCliente);
  AdicionarIdentificacao;
  FinalizarRequisicao;
  Try
     LerRespostaRequisicao;
     Resposta := Resp.DocumentoPessoa;
     Result := Resp.TransacaoAprovada;
  Finally
     FinalizarResposta(True); { True = Apaga Arquivo de Resposta }
  End;
end;

procedure TACBrTEFDClass.ConfirmarTransacoesAnteriores;
var
   I : Integer;
begin
  GravaLog( 'ConfirmarTransacoesAnteriores' ) ;

  { Se for Multiplos Cartoes precisa confirmar a transação anterior antes de
    enviar uma Nova }

  I := 0 ;
  while I < TACBrTEFD(Owner).RespostasPendentes.Count do
  begin
     with TACBrTEFDResp(TACBrTEFD(Owner).RespostasPendentes[I]) do
     begin
       if (not CNFEnviado)  and                     // Ainda não confirmou ?
          (TipoGP = TACBrTEFD(Owner).GPAtual) and   // É do mesmo GP ?
          TransacaoEPagamento(Header) then          // É CRT ou CHQ ?
       begin
          Self.CNF( Rede, NSU, Finalizacao, DocumentoVinculado );
          CNFEnviado := True ;
          if ArqBackup <> '' then
             Conteudo.GravarArquivo( ArqBackup, True ) ;   { True = DoFlushToDisk }
          ApagaEVerifica( ArqRespPendente );
       end;
     end ;

     Inc( I ) ;
  end ;
end;

procedure TACBrTEFDClass.ImprimirRelatorio;
Var
  I : Integer;
  TempoInicio : TDateTime ;
  ImpressaoOk, RemoverMsg, GerencialAberto : Boolean ;
  Est : AnsiChar ;
  ArqBackup : String ;
  ImagemComprovante : TStringList ;
begin
  VerificarIniciouRequisicao;

  if Resp.QtdLinhasComprovante < 1 then
     exit ;

  GravaLog( Name +' ImprimirRelatorio: '+Req.Header );

  if fpSalvarArquivoBackup then
     CopiarResposta ;

  ImpressaoOk  := False ;
  RemoverMsg   := False ;
  TempoInicio  := now ;

  with TACBrTEFD( Owner ) do
  begin
     try
        BloquearMouseTeclado( True );

        while not ImpressaoOk do
        begin
           try
              try
                 Est := EstadoECF;

                 if Est <> 'L' then
                 begin
                    { Fecha Vinculado ou Gerencial ou Cupom, se ficou algum aberto por Desligamento }
                    case Est of
                      'C'         : ComandarECF( opeFechaVinculado );
                      'G','R'     : ComandarECF( opeFechaGerencial );
                      'V','P','N' : ComandarECF( opeCancelaCupom );
                    end;

                    if EstadoECF <> 'L' then
                       raise EACBrTEFDECF.Create( ACBrStr(CACBrTEFD_Erro_ECFNaoLivre) ) ;
                 end;

                 GerencialAberto := False ;
                 TempoInicio     := now ;

                 if Self.Resp.TextoEspecialOperador <> '' then
                 begin
                    RemoverMsg := True ;
                    DoExibeMsg( opmExibirMsgOperador, Self.Resp.TextoEspecialOperador ) ;
                 end;

                 if Self.Resp.TextoEspecialCliente <> '' then
                 begin
                    RemoverMsg := True ;
                    DoExibeMsg( opmExibirMsgCliente, Self.Resp.TextoEspecialCliente ) ;
                 end;

                 I := 1 ;
                 while I <= self.NumVias do
                 begin
                   if I = 1 then
                      ImagemComprovante := Self.Resp.ImagemComprovante1aVia
                   else
                      ImagemComprovante := Self.Resp.ImagemComprovante2aVia ;

                   if ImagemComprovante.Count > 0 then
                   begin
                     if not GerencialAberto then
                      begin
                        ComandarECF( opeAbreGerencial ) ;
                        GerencialAberto := True;
                      end
                     else
                      begin
                        if I <> 1 then 
                           ComandarECF( opePulaLinhas ) ;
                        DoExibeMsg( opmDestaqueVia,
                                    Format( CACBrTEFD_DestaqueVia, [I]) ) ;
                      end ;

                     ECFImprimeVia( trGerencial, I, ImagemComprovante  )
                   end ;

                   Inc( I ) ;
                 end;

                 if GerencialAberto then
                    ComandarECF( opeFechaGerencial );

                 ImpressaoOk := True ;

              finally
                 { Removendo a mensagem do Operador }
                 if RemoverMsg then
                 begin
                    { Verifica se Mensagem Ficou pelo menos por 5 segundos }
                    if ImpressaoOk then
                    begin
                       while SecondsBetween(now,TempoInicio) < 5 do
                       begin
                          Sleep(EsperaSleep);
                          {$IFNDEF NOGUI}
                          Application.ProcessMessages;
                          {$ENDIF}
                       end;
                    end;

                    DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                    DoExibeMsg( opmRemoverMsgCliente, '' ) ;
                 end;
              end;

           except
              on EACBrTEFDECF do
                 ImpressaoOk := False ;
              else
                 raise ;
           end;

           if not ImpressaoOk then
           begin
             if DoExibeMsg( opmYesNo, CACBrTEFD_Erro_ECFNaoResponde ) <> mrYes then
                break ;
           end;
        end;
     finally
       { Enviando CNF ou NCN e apagando Arquivo de Backup }
       ArqBackup := Resp.ArqBackup ;
       while FileExists( ArqBackup ) do
       begin
          try
             if Resp.Confirmar then
             begin
               if ImpressaoOk then
                  self.CNF
               else
                  self.NCN ;
             end;
          except
          end;

          DeleteFile( ArqBackup ) ;
       end ;

       BloquearMouseTeclado( False );
     end ;

     if not ImpressaoOk then
        raise EACBrTEFDECF.Create( ACBrStr('Impressão de Relatório Falhou' ) ) ;
  end;
end;

procedure TACBrTEFDClass.ConfirmarESolicitarImpressaoTransacoesPendentes;
Var
  ArquivosVerficar : TStringList ;
  ArqMask, NSUs    : AnsiString;
  ExibeMsg         : Boolean ;
  RespostaConfirmada : TACBrTEFDResp ;
begin
  ArquivosVerficar := TStringList.Create;

  try
     ArquivosVerficar.Clear;
     TACBrTEFD(Owner).RespostasPendentes.Clear;

     { Achando Arquivos de Backup deste GP }
     ArqMask  := TACBrTEFD(Owner).PathBackup + PathDelim + 'ACBr_' + Self.Name + '_*.tef' ;
     FindFiles( ArqMask, ArquivosVerficar, True );
     NSUs     := '' ;
     ExibeMsg := (ArquivosVerficar.Count > 0) ;

     { Enviando NCN ou CNC para todos os arquivos encontrados }
     while ArquivosVerficar.Count > 0 do
     begin
        if not FileExists( ArquivosVerficar[ 0 ] ) then
        begin
           ArquivosVerficar.Delete( 0 );
           Continue;
        end;

        Resp.LeArquivo( ArquivosVerficar[ 0 ] );

        try
           CNF;   {Confirma}

           { Criando cópia da Resposta Atual }
           RespostaConfirmada := CriarResposta( Resp.TipoGP );
           RespostaConfirmada.Assign( Resp );
           TACBrTEFD(Owner).RespostasPendentes.Add( RespostaConfirmada );

           if Trim(Resp.NSU) <> '' then
              NSUs := NSUs + 'NSU: ' + Resp.NSU + sLineBreak;

           SysUtils.DeleteFile( ArquivosVerficar[ 0 ] );
           ArquivosVerficar.Delete( 0 );
        except
        end;
     end;

     // Chamando evento de confirmação das Respostas //
     with TACBrTEFD(Owner) do
     begin
       try
          if Assigned( OnDepoisConfirmarTransacoes ) and
             (RespostasPendentes.Count > 0) then
             OnDepoisConfirmarTransacoes( RespostasPendentes );
       finally
          RespostasPendentes.Clear;
       end;
     end;

     if ExibeMsg then
        TACBrTEFD(Owner).DoExibeMsg( opmOK,
           Format( CACBrTEFCliSiTef_TransacaoEfetuadaReImprimir, [NSUs] ) ) ;
  finally
     ArquivosVerficar.Free;
  end;
end;

function TACBrTEFDClass.CopiarResposta: String;
Var
   I : Integer ;
begin
  Result := '' ;
//VerificarIniciouRequisicao;

  with TACBrTEFD(Owner) do
  begin
     I := 1 ;
     repeat
        Result := PathBackup + PathDelim + 'ACBr_' +
                  Self.Name + '_' + IntToStrZero(I,3) + '.tef' ;
        Inc( I ) ;
     until not FileExists( Result );

     GravaLog( Name + ' CopiarResposta: '+Resp.Header+' - '+IntToStr(Resp.ID)+
                      ' Arq: '+Result);

     Resp.Conteudo.GravarArquivo( Result, True );   { True = DoFlushToDisk }
     Resp.ArqBackup := Result ;
  end;
end;

function TACBrTEFDClass.ProcessarRespostaPagamento(const IndiceFPG_ECF: String;
  const Valor: Double): Boolean;
var
  UltimaTransacao, ImpressaoOk, TecladoEstavaLivre : Boolean;
  RespostaPendente : TACBrTEFDResp;
begin
  LerRespostaRequisicao;

  GravaLog( Name +' ProcessarRespostaPagamento: '+Resp.Header+' - '+IntToStr(Resp.ID)+
            ' Indice: '+IndiceFPG_ECF+' Valor:'+FormatFloat('0.00',Valor) );

  Result := Resp.TransacaoAprovada ;

  with TACBrTEFD(Owner) do
  begin
     UltimaTransacao := (Valor >= RespostasPendentes.SaldoRestante );

     {Se a transação não foi aprovada, faz tratamento e sai}
     if not Self.Resp.TransacaoAprovada then
     begin
       ProcessarResposta;           { Exibe a Mensagem ao Operador }
       FinalizarResposta( True ) ;  { True = Apaga Arquivo de Resposta }

                               { Ja tem RespostasPendentes ? }
       if UltimaTransacao and ( RespostasPendentes.Count > 0 ) then
       begin
          if DoExibeMsg( opmYesNo, CACBrTEFD_Erro_OutraFormaPagamento ) <> mrYes then
          begin
             ComandarECF( opeCancelaCupom );
             CancelarTransacoesPendentes;
          end;
       end;

       exit ;
     end ;

     {...Se está aqui, então a Transação foi aprovada...}

     Self.Resp.IndiceFPG_ECF := IndiceFPG_ECF;

     { Cria Arquivo de Backup, contendo inclusive informações internas como :
       899 - 001 : CNFEnviado (S, N)
       899 - 002 : IndiceFPG_ECF : String
       899 - 003 : OrdemPagamento : Integer
     }
     CopiarResposta ;

     { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
     Resp.ArqRespPendente := ArqResp;
     Resp.ViaClienteReduzida := ImprimirViaClienteReduzida;
     RespostaPendente := CriarResposta(fpTipo);
     RespostaPendente.Assign( Resp );
     RespostasPendentes.Add( RespostaPendente );

     ImpressaoOk        := True;
     TecladoEstavaLivre := True ;

     {Efetuar o pagamento automaticamente?}
     if AutoEfetuarPagamento then
     begin
        try //Faz tratamento para caso ocorra erro na impressora durante impressão
           ImpressaoOk := False ;

           while not ImpressaoOk do
           begin
              try
                 try
                    TecladoEstavaLivre := not TecladoBloqueado;
                    BloquearMouseTeclado( True );
                    ECFPagamento( IndiceFPG_ECF, Valor );
                    RespostasPendentes.SaldoAPagar  := RoundTo( RespostasPendentes.SaldoAPagar - Valor, -2 ) ;
                    if (RespostaPendente.Header = 'CHQ') and CHQEmGerencial then
                       RespostaPendente.OrdemPagamento := 999
                    else
                       RespostaPendente.OrdemPagamento := RespostasPendentes.Count + 1 ;
                    ImpressaoOk := True ;
                 finally
                    if TecladoEstavaLivre then
                       BloquearMouseTeclado( False );
                  end ;
              except
                 on EACBrTEFDECF do
                    ImpressaoOk := False ;
                 else
                    raise ;
              end;

              if not ImpressaoOk then
              begin
                 if DoExibeMsg( opmYesNo, CACBrTEFD_Erro_ECFNaoResponde ) <> mrYes then
                 begin
                    try ComandarECF(opeCancelaCupom); except {Exceção Muda} end ;
                    break ;
                 end;
              end;
           end;
        finally
           if not ImpressaoOk then
              CancelarTransacoesPendentes;
        end; //end do "if AutoEfetuarPagamento try ..."
     end;

     if not ImpressaoOk then exit ;

     FinalizarResposta( False );    { False = NAO Apaga Arquivo de Resposta }

     if AutoFinalizarCupom and (RespostasPendentes.SaldoRestante <= 0) then
     begin
        FinalizarCupom( False );  { False não desbloqueia o MouseTeclado }
        ImprimirTransacoesPendentes;
     end ;
  end;
end;

procedure TACBrTEFDClass.VerificarIniciouRequisicao;
begin
  if Req.Header = '' then
     raise EACBrTEFDErro.Create( ACBrStr( CACBrTEFD_Erro_SemRequisicao ) ) ;
end;

procedure TACBrTEFDClass.VerificarTransacaoPagamento(Valor: Double);
var
  SaldoAPagar : Double ;
begin
  Valor := RoundTo( Valor, -2);

  if (Valor <= 0) then
     raise EACBrTEFDErro.Create( ACBrStr( 'Valor inválido' ) );

  { Lendo o SubTotal do ECF }
  with TACBrTEFD(Owner) do
  begin
    if not (EstadoECF in ['V','P','N']) then
       raise EACBrTEFDErro.Create(
          ACBrStr('ECF deve estar em Estado de "Venda", "Pagamento" ou "Não Fiscal"') );

    SaldoAPagar := InfoECFAsDouble(ineSubTotal) ;
    SaldoAPagar := SaldoAPagar - InfoECFAsDouble(ineTotalAPagar,0);
    RespostasPendentes.SaldoAPagar := SaldoAPagar ;

    if TrocoMaximo <= 0 then
     begin
       if (Valor > RespostasPendentes.SaldoRestante ) then
          raise EACBrTEFDErro.Create( ACBrStr( 'Operação TEF deve ser limitada ao '+
                                               'Saldo restante a Pagar' ) );
     end
    else
     begin
       if CompareValue(Valor, RespostasPendentes.SaldoRestante + TrocoMaximo, 0.01) = GreaterThanValue then
          raise EACBrTEFDErro.Create( ACBrStr( 'Operação TEF permite '+
                                               'Troco Máximo de R$ '+FormatCurr('0.00',TrocoMaximo) ) );
     end ;

    if MultiplosCartoes and (NumeroMaximoCartoes > 0) and      // Tem multiplos Cartoes ?
       (RespostasPendentes.Count >= NumeroMaximoCartoes) then  // Já informou todos cartões ?
       raise EACBrTEFDErro.Create( ACBrStr( 'Multiplos Cartões Limitado a '+
             IntToStr(NumeroMaximoCartoes)+' operações.' ) );

    if Self is TACBrTEFDClassTXT then   // Limita Saldo Restante se for derivado de TEF discado
    begin
      if MultiplosCartoes and (NumeroMaximoCartoes > 0) and   // Tem multiplos Cartoes ?
         (Valor <> RespostasPendentes.SaldoRestante) and      // Valor é diferente do Saldo Restante a Pagar ?
         ((NumeroMaximoCartoes - RespostasPendentes.Count) <= 1) then  // Está no último cartão ?
         raise EACBrTEFDErro.Create( ACBrStr( 'Multiplos Cartões Limitado a '+
               IntToStr(NumeroMaximoCartoes)+' operações.'+sLineBreak+
               'Esta Operação TEF deve ser igual ao Saldo a Pagar' ) );
    end ;
  end;
end;

function TACBrTEFDClass.TransacaoEPagamento(AHeader: String): Boolean;
begin
   Result := (pos(AHeader,'CRT|CHQ') > 0);
end;

procedure TACBrTEFDClass.SetArqReq(const AValue : String);
begin
  fArqReq := Trim( AValue ) ;
end;

procedure TACBrTEFDClass.SetArqResp(const AValue : String);
begin
  fArqResp := Trim( AValue ) ;
end;

procedure TACBrTEFDClass.SetArqSTS(const AValue : String);
begin
  fArqSTS := Trim( AValue ) ;
end;

procedure TACBrTEFDClass.SetArqTmp(const AValue : String);
begin
  fArqTmp := Trim( AValue ) ;
end;

procedure TACBrTEFDClass.SetAutoAtivarGP(AValue: Boolean);
begin
  fAutoAtivarGP:=AValue;
end;

procedure TACBrTEFDClass.SetGPExeName(const AValue : String);
begin
  fGPExeName := Trim( AValue ) ;
end;

procedure TACBrTEFDClass.SetNumVias(const AValue : Integer);
begin
   fpNumVias := AValue;
end;

procedure TACBrTEFDClass.GravaLog(AString : AnsiString; Traduz : Boolean);
Var
  Tratado: Boolean;
begin
  if Traduz then
    AString := TranslateUnprintable(AString);

  Tratado := False;

  with TACBrTEFD(Owner) do
  begin
    if Assigned( OnGravarLog ) then
      OnGravarLog( GPAtual, AString, Tratado);
  end;

  if Tratado or (fArqLOG = '') then
     exit ;

  try
     WriteToTXT( fArqLOG, '-- '+FormatDateTime('dd/mm hh:nn:ss:zzz',now) +
                          ' - ' + AString, True);
  except
  end ;
end;

function TACBrTEFDClass.CriarResposta(Tipo: TACBrTEFDTipo): TACBrTEFDResp;
begin
  case Tipo of
    gpCliSiTef: Result := TACBrTEFDRespCliSiTef.Create;
    gpVeSPague: Result := TACBrTEFDRespVeSPague.Create;
    gpPayGo: Result := TACBrTEFDRespPayGo.Create;
    gpPayGoWeb: Result := TACBrTEFDRespPayGoWeb.Create;
    gpTefDialScopeGetcard: Result := TACBrTEFDRespScopeGetcard.Create;
  else
    Result := TACBrTEFDRespTXT.Create;
  end;
end;

{ TACBrTEFDClasses }

procedure TACBrTEFDClassList.SetObject(Index : Integer; Item : TACBrTEFDClass);
begin
  inherited Items[Index] := Item;
end;

function TACBrTEFDClassList.GetObject(Index : Integer) : TACBrTEFDClass;
begin
  Result := TACBrTEFDClass(inherited Items[Index]);
end;

procedure TACBrTEFDClassList.Insert(Index : Integer; Obj : TACBrTEFDClass);
begin
  inherited Insert(Index, Obj);
end;

function TACBrTEFDClassList.Add(Obj : TACBrTEFDClass) : Integer;
begin
  Result := inherited Add(Obj) ;
end;

end.

