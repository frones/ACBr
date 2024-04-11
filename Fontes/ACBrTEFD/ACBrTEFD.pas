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

unit ACBrTEFD;

interface

uses
  Classes, SysUtils, ACBrBase, ACBrTEFDClass, ACBrTEFDPayGo, ACBrTEFDPayGoWeb,
  ACBrTEFDAuttar, ACBrTEFDDial, ACBrTEFDDisc, ACBrTEFDHiper, ACBrTEFDCliSiTef,
  ACBrTEFDGpu, ACBrTEFDVeSPague, ACBrTEFDBanese, ACBrTEFDGoodCard, ACBrTEFDFoxWin,
  ACBrTEFDCliDTEF, ACBrTEFDPetroCard, ACBrTEFDCrediShop, ACBrTEFDTicketCar,
  ACBrTEFDConvCard, ACBrTEFDCappta, ACBrTEFDCliSiTefModular, ACBrTEFDDirecao,
  ACBrTEFDDialScopeGetcard , ACBrTEFDElgin
  {$IfNDef NOGUI}
    {$IfDef FPC}
      ,LResources
    {$EndIf}
    {$IfDef MSWINDOWS}
      ,Windows, Messages
      {$IFDEF FMX}
        ,FMX.Platform.Win
      {$ENDIF}
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
  {$EndIf};

type

   { TACBrTEFDIdentificacao }

   TACBrTEFDIdentificacao = class( TPersistent )
   private
      FNomeAplicacao: String;
      FRazaoSocial: String;
      FSoftwareHouse: String;
      FVersaoAplicacao: String;
      FRegistroCertificacao : String;
      procedure SetNomeAplicacao(AValue: String);
      procedure SetRazaoSocial(AValue: String);
      procedure SetRegistroCertificacao(AValue: String);
      procedure SetSoftwareHouse(AValue: String);
      procedure SetVersaoAplicacao(AValue: String);
   published
     property NomeAplicacao   : String read FNomeAplicacao   write SetNomeAplicacao ;
     property VersaoAplicacao : String read FVersaoAplicacao write SetVersaoAplicacao ;
     property SoftwareHouse   : String read FSoftwareHouse   write SetSoftwareHouse ;
     property RazaoSocial     : String read FRazaoSocial     write SetRazaoSocial ;
     property RegistroCertificacao : String read FRegistroCertificacao write SetRegistroCertificacao;

   end ;


   { TACBrTEFD }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllDesktopPlatforms)]
  {$ENDIF RTL230_UP}
   TACBrTEFD = class( TACBrComponent )
   private
     fAutoAtivarGP : Boolean;
     fAutoFinalizarCupom : Boolean;
     fAutoEfetuarPagamento : Boolean;
     fCHQEmGerencial: Boolean;
     fConfirmarAntesDosComprovantes: Boolean;
     fOnExibeQRCode: TACBrTEFDExibeQRCode;
     fTrocoMaximo: Double;
     fEsperaSleep : Integer;
     fEstadoReq : TACBrTEFDReqEstado;
     fEstadoResp : TACBrTEFDRespEstado;
     fExibirMsgAutenticacao: Boolean;
     fIdentificacao: TACBrTEFDIdentificacao;
     fMultiplosCartoes : Boolean;
     fNumeroMaximoCartoes: Integer;
     fNumVias : Integer;
     fImprimirViaClienteReduzida : Boolean;
     fOnAguardaResp : TACBrTEFDAguardaRespEvent;
     fOnAntesCancelarTransacao: TACBrTEFDAntesCancelarTransacao;
     fOnAntesFinalizarRequisicao : TACBrTEFDAntesFinalizarReq;
     fOnBloqueiaMouseTeclado : TACBrTEFDBloqueiaMouseTeclado;
     fOnComandaECF : TACBrTEFDComandaECF;
     fOnComandaECFAbreVinculado : TACBrTEFDComandaECFAbreVinculado;
     fOnComandaECFImprimeVia : TACBrTEFDComandaECFImprimeVia;
     fOnComandaECFPagamento : TACBrTEFDComandaECFPagamento;
     fOnComandaECFSubtotaliza: TACBrTEFDComandaECFSubtotaliza;
     fOnDepoisCancelarTransacoes : TACBrTEFDProcessarTransacoesPendentes ;
     fOnDepoisConfirmarTransacoes : TACBrTEFDProcessarTransacoesPendentes;
     fOnExibeMsg : TACBrTEFDExibeMsg;
     fOnInfoECF : TACBrTEFDObterInfoECF;
     fOnLimpaTeclado : TACBrTEFDExecutaAcao;
     fOnMudaEstadoReq : TACBrTEFDMudaEstadoReq;
     fOnMudaEstadoResp : TACBrTEFDMudaEstadoResp;
     fOnRestauraFocoAplicacao : TACBrTEFDExecutaAcao;
     fOnGravarLog: TACBrTEFDGravarLog;

     fPathBackup   : String;
     fGPAtual      : TACBrTEFDTipo;
     fTecladoBloqueado : Boolean;
     fSuportaDesconto : Boolean;
     fSuportaSaque : Boolean;
     fTempoInicialMensagemOperador: TDateTime;
     fTempoInicialMensagemCliente: TDateTime;

     fTefClass     : TACBrTEFDClass ;
     fTefPayGo     : TACBrTEFDPayGo ;
     fTefPayGoWeb  : TACBrTEFDPayGoWeb ;
     fTefDial      : TACBrTEFDDial ;
     fTefGPU       : TACBrTEFDGpu ;
     fTefCliSiTef  : TACBrTEFDCliSiTef;
     fTefVeSPague  : TACBrTEFDVeSPague;
     fTefDisc      : TACBrTEFDDisc ;
     fTefHiper     : TACBrTEFDHiper ;
     fTefBanese    : TACBrTEFDBanese ;
     fTefAuttar    : TACBrTEFDAuttar ;
     fTefGood      : TACBrTEFDGoodCard ;
     fTefFW        : TACBrTEFDFoxWin ;
     fTefCliDTEF   : TACBrTEFDCliDTEF ;
     fTefPetrocard : TACBrTEFDPetroCard ;
     fTefCrediShop : TACBrTEFDCrediShop ;
     fTefTicketCar : TACBrTEFDTicketCar ;
     fTefConvCard  : TACBrTEFDConvCard ;
     fTefCappta    : TACBrTEFDCappta;
     fTefCliSiTefModular: TACBrTEFDCliSiTefModular;
     fTefDirecao   : TACBrTEFDDirecao;
     fTefDialScopeGetcard: TACBrTEFDDialScopeGetcard;
     fTefElgin      : TACBrTEFDElgin ;

     fEsperaSTS    : Integer;
     fEsperaMinimaMensagemFinal: Integer;
     fTEFList      : TACBrTEFDClassList ;
     fpRespostasPendentes : TACBrTEFDRespostasPendentes;
     fArqLOG: string;

     function GetAguardandoResposta: Boolean;
     function GetArqReq : String;
     function GetArqResp : String;
     function GetArqSTS : String;
     function GetArqTmp : String;
     function GetGPExeName : String;
     function GetPathBackup : String;
     function GetReq : TACBrTEFDReq;
     function GetResp : TACBrTEFDResp;
     procedure SetAutoAtivarGP(AValue: Boolean);
     procedure SetAutoEfetuarPagamento(const AValue : Boolean);
     procedure SetAutoFinalizarCupom(const AValue : Boolean);
     procedure SetEsperaSleep(const AValue : Integer);
     procedure SetEsperaSTS(const AValue : Integer);
     procedure SetEstadoReq(const AValue : TACBrTEFDReqEstado);
     procedure SetEstadoResp(const AValue : TACBrTEFDRespEstado);
     procedure SetMultiplosCartoes(const AValue : Boolean);
     procedure SetNumeroMaximoCartoes(const AValue: Integer);
     procedure SetNumVias(const AValue : Integer);
     procedure SetPathBackup(const AValue : String);
     procedure SetGPAtual(const AValue : TACBrTEFDTipo);
     procedure SetArqLOG(const AValue : String);

     procedure AguardarTempoMinimoDeExibicao(const TempoInicial: TDateTime);
   public
     Function InfoECFAsString( Operacao : TACBrTEFDInfoECF ) : String ;
     Function InfoECFAsDouble( Operacao : TACBrTEFDInfoECF;
        DefaultValue: Integer = -98787158) : Double ;
     Function EstadoECF : AnsiChar ;
     function DoExibeMsg( Operacao : TACBrTEFDOperacaoMensagem;
        const Mensagem : String; ManterTempoMinimo : Boolean = False ) : TModalResult;
     procedure DoExibeQRCode(const Dados: String);
     function ComandarECF(Operacao : TACBrTEFDOperacaoECF) : Integer;
     function ECFSubtotaliza(DescAcres: Double) : Integer;
     function ECFPagamento(Indice : String; Valor : Double) : Integer;
     function ECFAbreVinculado(COO, Indice : String; Valor : Double) : Integer;
     function ECFImprimeVia( TipoRelatorio : TACBrTEFDTipoRelatorio;
        Via : Integer; ImagemComprovante : TStringList) : Integer;

     procedure BloquearMouseTeclado(Bloqueia : Boolean);
     procedure LimparTeclado;
     procedure RestaurarFocoAplicacao ;

   public
     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     Procedure Inicializar( GP : TACBrTEFDTipo = gpNenhum ) ;
     Procedure DesInicializar( GP : TACBrTEFDTipo = gpNenhum ) ;
     function Inicializado( GP : TACBrTEFDTipo = gpNenhum ) : Boolean ;

     property GPAtual : TACBrTEFDTipo read fGPAtual write SetGPAtual ;
     property TecladoBloqueado : Boolean read fTecladoBloqueado ;
     property AguardandoResposta : Boolean read GetAguardandoResposta ;

     property TEF  : TACBrTEFDClass read fTefClass ;
     property Req  : TACBrTEFDReq   read GetReq  ;
     property Resp : TACBrTEFDResp  read GetResp ;
     property RespostasPendentes : TACBrTEFDRespostasPendentes
        read fpRespostasPendentes ;

     property ArqTemp  : String read GetArqTmp ;
     property ArqReq   : String read GetArqReq ;
     property ArqSTS   : String read GetArqSTS ;
     property ArqResp  : String read GetArqResp ;
     property GPExeName: String read GetGPExeName ;

     property EstadoReq  : TACBrTEFDReqEstado  read fEstadoReq  write SetEstadoReq ;
     property EstadoResp : TACBrTEFDRespEstado read fEstadoResp write SetEstadoResp ;

     procedure AtivarGP(GP : TACBrTEFDTipo);

     procedure ATV( GP : TACBrTEFDTipo = gpNenhum ) ;
     Function ADM( GP : TACBrTEFDTipo = gpNenhum ) : Boolean ;
     Function CRT( const Valor : Double; const IndiceFPG_ECF : String;
        const DocumentoVinculado : String = ''; const Moeda : Integer = 0 )
        : Boolean ;
     Function CDP(Const EntidadeCliente:String; Out Resposta:String): Boolean;
     Function CHQ( const Valor : Double; const IndiceFPG_ECF : String;
        const DocumentoVinculado : String = ''; const CMC7 : String = '';
        const TipoPessoa : AnsiChar = 'F'; const DocumentoPessoa : String = '';
        const DataCheque : TDateTime = 0; const Banco   : String = '';
        const Agencia    : String = ''; const AgenciaDC : String = '';
        const Conta      : String = ''; const ContaDC   : String = '';
        const Cheque     : String = ''; const ChequeDC  : String = '';
        const Compensacao: String = '' ) : Boolean ;
     Function CNC(const Rede, NSU : String; const DataHoraTransacao :
        TDateTime; const Valor : Double; CodigoAutorizacaoTransacao: String = '') : Boolean ;
     procedure CNF(const Rede, NSU, Finalizacao : String;
        const DocumentoVinculado : String = '');
     procedure NCN(const Rede, NSU, Finalizacao : String;
        const Valor : Double = 0; const DocumentoVinculado : String = '');
     Function PRE(Valor : Double; DocumentoVinculado : String = '';
        Moeda : Integer = 0 ) : Boolean; virtual;

     procedure FinalizarCupom( DesbloquearMouseTecladoNoTermino: Boolean = True);
     procedure CancelarTransacoesPendentes;
     procedure ConfirmarTransacoesPendentes(ApagarRespostasPendentes: Boolean = True);
     procedure ImprimirTransacoesPendentes;
     procedure ApagarRespostasPendentes;

     procedure AgruparRespostasPendentes(
        var Grupo : TACBrTEFDArrayGrupoRespostasPendentes) ;
     procedure ExibirMensagemPinPad(const MsgPinPad: String);

   published
     property Identificacao : TACBrTEFDIdentificacao read fIdentificacao
        write fIdentificacao ;

     property MultiplosCartoes : Boolean read fMultiplosCartoes
       write SetMultiplosCartoes default False ;
     property NumeroMaximoCartoes : Integer read fNumeroMaximoCartoes
       write SetNumeroMaximoCartoes default 0;
     property AutoAtivarGP : Boolean read fAutoAtivarGP write SetAutoAtivarGP default True ;
     property ExibirMsgAutenticacao : Boolean read fExibirMsgAutenticacao
       write fExibirMsgAutenticacao default True ;
     property AutoEfetuarPagamento : Boolean read fAutoEfetuarPagamento
       write SetAutoEfetuarPagamento default False ;
     property AutoFinalizarCupom : Boolean read fAutoFinalizarCupom
       write SetAutoFinalizarCupom default True ;
     property NumVias   : Integer read fNumVias   write SetNumVias
       default CACBrTEFD_NumVias ;
     property ImprimirViaClienteReduzida : Boolean read fImprimirViaClienteReduzida
       write fImprimirViaClienteReduzida default False;
     property EsperaSTS : Integer read fEsperaSTS write SetEsperaSTS
        default CACBrTEFD_EsperaSleep ;
     property EsperaSleep : Integer read fEsperaSleep write SetEsperaSleep
        default CACBrTEFD_EsperaSleep ;
     property EsperaMinimaMensagemFinal : Integer read fEsperaMinimaMensagemFinal
        write fEsperaMinimaMensagemFinal default CACBrTEFD_EsperaMinimaMensagemFinal;
     property PathBackup : String read GetPathBackup write SetPathBackup ;
     property ArqLOG : String read fArqLOG write SetArqLOG ;
     property CHQEmGerencial : Boolean read fCHQEmGerencial write fCHQEmGerencial default False ;
     property TrocoMaximo : Double read fTrocoMaximo write fTrocoMaximo ;
     Property SuportaSaque : Boolean read fSuportaSaque write fSuportaSaque default True;
     Property SuportaDesconto : Boolean read fSuportaDesconto write fSuportaDesconto default True;
     property ConfirmarAntesDosComprovantes: Boolean read fConfirmarAntesDosComprovantes
       write fConfirmarAntesDosComprovantes default False;

     property TEFPayGo   : TACBrTEFDPayGo    read fTefPayGo ;
     property TEFPayGoWeb: TACBrTEFDPayGoWeb read FTEFPayGoWeb ;
     property TEFDial    : TACBrTEFDDial     read fTefDial ;
     property TEFDisc    : TACBrTEFDDisc     read fTefDisc ;
     property TEFHiper   : TACBrTEFDHiper    read fTefHiper ;
     property TEFCliSiTef: TACBrTEFDCliSiTef read fTefCliSiTef ;
     property TEFVeSPague: TACBrTEFDVeSPague read fTefVeSPague ;
     property TEFGPU     : TACBrTEFDGpu      read fTefGPU ;
     property TEFBanese  : TACBrTEFDBanese   read fTefBanese ;
     property TEFAuttar  : TACBrTEFDAuttar   read fTefAuttar ;     
     property TEFGood    : TACBrTEFDGoodCard read fTefGood ;
     property TEFFoxWin  : TACBrTEFDFoxWin   read fTefFW ;
     property TEFCliDTEF : TACBrTEFDCliDTEF  read fTefCliDTEF ;
     property TEFPetrocard : TACBrTEFDPetroCard  read fTefPetrocard ;
     property TEFCrediShop : TACBrTEFDCrediShop  read fTefCrediShop ;
     property TEFTicketCar : TACBrTEFDTicketCar  read fTefTicketCar ;
     property TEFConvCard : TACBrTEFDConvCard read fTefConvCard ;     
     property TEFCappta    : TACBrTEFDCappta    read fTefCappta;
     property TEFCliSiTefModular : TACBrTEFDCliSiTefModular read fTefCliSiTefModular ;
     property TEFDirecao   : TACBrTEFDDirecao  read fTefDirecao;
     property TEFElgin    : TACBrTEFDElgin     read fTefElgin ;

     property OnAguardaResp : TACBrTEFDAguardaRespEvent read fOnAguardaResp
        write fOnAguardaResp ;
     property OnExibeMsg    : TACBrTEFDExibeMsg read fOnExibeMsg
        write fOnExibeMsg ;
     property OnExibeQRCode: TACBrTEFDExibeQRCode read fOnExibeQRCode
        write fOnExibeQRCode;
     property OnBloqueiaMouseTeclado : TACBrTEFDBloqueiaMouseTeclado
        read fOnBloqueiaMouseTeclado write fOnBloqueiaMouseTeclado ;
     property OnRestauraFocoAplicacao : TACBrTEFDExecutaAcao
        read fOnRestauraFocoAplicacao write fOnRestauraFocoAplicacao ;
     property OnLimpaTeclado : TACBrTEFDExecutaAcao read fOnLimpaTeclado
        write fOnLimpaTeclado ;
     property OnComandaECF  : TACBrTEFDComandaECF read fOnComandaECF
        write fOnComandaECF ;
     property OnComandaECFSubtotaliza  : TACBrTEFDComandaECFSubtotaliza
        read fOnComandaECFSubtotaliza write fOnComandaECFSubtotaliza ;
     property OnComandaECFPagamento  : TACBrTEFDComandaECFPagamento
        read fOnComandaECFPagamento write fOnComandaECFPagamento ;
     property OnComandaECFAbreVinculado : TACBrTEFDComandaECFAbreVinculado
        read fOnComandaECFAbreVinculado write fOnComandaECFAbreVinculado ;
     property OnComandaECFImprimeVia : TACBrTEFDComandaECFImprimeVia
        read fOnComandaECFImprimeVia write fOnComandaECFImprimeVia ;
     property OnInfoECF : TACBrTEFDObterInfoECF read fOnInfoECF write fOnInfoECF ;
     property OnAntesFinalizarRequisicao : TACBrTEFDAntesFinalizarReq
        read fOnAntesFinalizarRequisicao write fOnAntesFinalizarRequisicao ;
     property OnDepoisConfirmarTransacoes : TACBrTEFDProcessarTransacoesPendentes
        read fOnDepoisConfirmarTransacoes write fOnDepoisConfirmarTransacoes ;
     property OnAntesCancelarTransacao : TACBrTEFDAntesCancelarTransacao
        read fOnAntesCancelarTransacao write fOnAntesCancelarTransacao ;
     property OnDepoisCancelarTransacoes : TACBrTEFDProcessarTransacoesPendentes
        read fOnDepoisCancelarTransacoes write fOnDepoisCancelarTransacoes ;
     property OnMudaEstadoReq  : TACBrTEFDMudaEstadoReq read fOnMudaEstadoReq
        write fOnMudaEstadoReq ;
     property OnMudaEstadoResp : TACBrTEFDMudaEstadoResp read fOnMudaEstadoResp
        write fOnMudaEstadoResp ;
     property OnGravarLog : TACBrTEFDGravarLog read fOnGravarLog write fOnGravarLog;

   end;

procedure ApagaEVerifica( const Arquivo : String ) ;

implementation

Uses
  dateutils, TypInfo, StrUtils, Math,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrTEFComum;

{$IFNDEF FPC}
   {$R ACBrTEFD.dcr}
{$ENDIF}

procedure ApagaEVerifica( const Arquivo : String ) ;
begin
  if Arquivo = '' then exit ;

  SysUtils.DeleteFile( Arquivo );
  if FileExists( Arquivo ) then
     raise EACBrTEFArquivo.Create( ACBrStr( 'Erro ao apagar o arquivo:' + sLineBreak + Arquivo ) );
end;

{ TACBrTEFDIdentificacao }

procedure TACBrTEFDIdentificacao.SetSoftwareHouse(AValue: String);
begin
   if FSoftwareHouse=AValue then Exit;
   FSoftwareHouse := Trim(AValue);
end;

procedure TACBrTEFDIdentificacao.SetNomeAplicacao(AValue: String);
begin
   if FNomeAplicacao=AValue then Exit;
   FNomeAplicacao := Trim(AValue);
end;

procedure TACBrTEFDIdentificacao.SetRazaoSocial(AValue: String);
begin
   if FRazaoSocial=AValue then Exit;
   FRazaoSocial := Trim(AValue);
end;

procedure TACBrTEFDIdentificacao.SetRegistroCertificacao(AValue: String);
begin
  if FRegistroCertificacao = AValue then Exit;
  FRegistroCertificacao := Trim(AValue);
end;

procedure TACBrTEFDIdentificacao.SetVersaoAplicacao(AValue: String);
begin
   if FVersaoAplicacao=AValue then Exit;
   FVersaoAplicacao := Trim(AValue);
end;

{ TACBrTEFDClass }

constructor TACBrTEFD.Create(AOwner : TComponent);
begin
  inherited Create(AOWner);

  fPathBackup           := '' ;
  fAutoAtivarGP         := True ;
  fAutoEfetuarPagamento := False ;
  fExibirMsgAutenticacao:= True ;
  fAutoFinalizarCupom   := True ;
  fMultiplosCartoes     := False ;
  fNumeroMaximoCartoes  := 0 ;
  fImprimirViaClienteReduzida := False;
  fGPAtual              := gpNenhum ;
  fNumVias              := CACBrTEFD_NumVias ;
  fEsperaSTS            := CACBrTEFD_EsperaSTS ;
  fEsperaSleep          := CACBrTEFD_EsperaSleep ;
  fEsperaMinimaMensagemFinal := CACBrTEFD_EsperaMinimaMensagemFinal;
  fTecladoBloqueado     := False ;
  fArqLOG               := '' ;
  fCHQEmGerencial       := False;
  fTrocoMaximo          := 0;
  fSuportaDesconto      := True;
  fSuportaSaque         := True;

  fTempoInicialMensagemCliente  := 0;
  fTempoInicialMensagemOperador := 0;

  fOnAguardaResp              := nil ;
  fOnAntesFinalizarRequisicao := nil ;
  fOnDepoisConfirmarTransacoes:= nil ;
  fOnDepoisCancelarTransacoes := nil ;
  fOnAguardaResp              := nil ;
  fOnComandaECF               := nil ;
  fOnComandaECFSubtotaliza    := nil ;
  fOnComandaECFPagamento      := nil ;
  fOnComandaECFAbreVinculado  := nil ;
  fOnComandaECFImprimeVia     := nil ;
  fOnInfoECF                  := nil ;
  fOnExibeMsg                 := nil ;
  fOnExibeQRCode              := nil ;
  fOnMudaEstadoReq            := nil ;
  fOnMudaEstadoResp           := nil ;
  fOnBloqueiaMouseTeclado     := nil ;
  fOnLimpaTeclado             := nil ;
  fOnRestauraFocoAplicacao    := nil ;
  fOnGravarLog                := nil ;
  fIdentificacao              := TACBrTEFDIdentificacao.Create;

  { Lista de Objetos com todas as Classes de TEF }
  fTEFList := TACBrTEFDClassList.create(True);
  { Lista de Objetos TACBrTEFDresp com todas as Respostas Pendentes para Impressao }
  fpRespostasPendentes := TACBrTEFDRespostasPendentes.create(True);

  { Criando Classe TEF_PayGo }
  fTefPayGo := TACBrTEFDPayGo.Create(self);
  fTEFList.Add(fTefPayGo);     // Adicionando "fTefPayGo" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefPayGo.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF_PayGo }
  fTefPayGoWeb := TACBrTEFDPayGoWeb.Create(self);
  fTEFList.Add(fTefPayGoWeb);     // Adicionando "fTefPayGoWeb" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefPayGoWeb.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF_DIAL }
  fTefDial := TACBrTEFDDial.Create(self);
  fTEFList.Add(fTefDial);     // Adicionando "fTefDial" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefDial.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF_AUTTAR IP }
  fTefAuttar := TACBrTEFDAuttar.Create(self);
  fTEFList.Add(fTefAuttar);     // Adicionando "fTefAuttar" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefAuttar.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF_DISC }
  fTefDisc := TACBrTEFDDisc.Create(self);
  fTEFList.Add(fTefDisc);     // Adicionando "fTefDisc" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefDisc.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe HIPER TEF }
  fTefHiper := TACBrTEFDHiper.Create(self);
  fTEFList.Add(fTefHiper);     // Adicionando "fTefHiper" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefHiper.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF CliSiTEF }
  fTefCliSiTef := TACBrTEFDCliSiTef.Create(self);
  fTEFList.Add(fTefCliSiTef);     // Adicionando "fTefCliSiTef" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefCliSiTef.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF VeSPague }
  fTefVeSPague := TACBrTEFDVeSPague.Create(self);
  fTEFList.Add(fTefVeSPague);     // Adicionando "fTefVeSPague" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefVeSPague.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF_GPU }
  fTefGPU := TACBrTEFDGpu.Create(self);
  fTEFList.Add(fTefGPU);     // Adicionando "fTefGPU" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefGPU.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe CliBanese }
  fTefBanese := TACBrTEFDBanese.Create(self);
  fTEFList.Add(fTefBanese);     // Adicionando "fTefBanese" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefBanese.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe GOOD CARD }
  fTefGood := TACBrTEFDGoodCard.Create(self);
  fTEFList.Add(fTefGood);     // Adicionando "fTefGood" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefGood.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe Fox Win }
  fTefFW := TACBrTEFDFoxWin.Create(self);
  fTEFList.Add(fTefFW);     // Adicionando "fTefFW" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefFW.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe CliDTEF }
  fTefCliDTEF := TACBrTEFDCliDTEF.Create(self);
  fTEFList.Add(fTefCliDTEF);     // Adicionando "fTefCliDTEF" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefCliDTEF.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe Petrocard }
  fTefPetrocard := TACBrTEFDPetroCard.Create(self);
  fTEFList.Add(fTefPetrocard);     // Adicionando "fTefPetrocard" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefPetrocard.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe CrediShop }
  fTefCrediShop := TACBrTEFDCrediShop.Create(self);
  fTEFList.Add(fTefCrediShop);     // Adicionando "fTefCrediShop" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefCrediShop.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TicketCar }
  fTefTicketCar := TACBrTEFDTicketCar.Create(self);
  fTEFList.Add(fTefTicketCar);     // Adicionando "fTefTicketCar" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefTicketCar.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe CONV CARD }
  fTefConvCard := TACBrTEFDConvCard.Create(self);
  fTEFList.Add(fTefConvCard);     // Adicionando "fTefConvCard" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefConvCard.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF Cappta }
  fTefCappta := TACBrTEFDCappta.Create(self);
  fTEFList.Add(fTefCappta);     // Adicionando "fTefCappta" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefCappta.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}


  { Criando Classe TEF CliSiTEFMODULAR }
  fTefCliSiTefmodular := TACBrTEFDCliSiTefModular.Create(self);
  fTEFList.Add(fTefCliSiTefModular);     // Adicionando "fTefCliSiTefModular" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefCliSiTefmodular.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF_DIRECAO }
  fTefDirecao := TACBrTEFDDirecao.Create(self);
  fTEFList.Add(fTefDirecao);     // Adicionando "fTefDiracao" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefDirecao.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe Dial_ScopeGetcard }
  fTefDialScopeGetcard := TACBrTEFDDialScopeGetcard.Create(self);
  fTEFList.Add(fTefDialScopeGetcard);     // Adicionando "fTefDialScopeGetcard" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefDialScopeGetcard.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  { Criando Classe TEF_DIAL }
  fTefElgin := TACBrTEFDElgin.Create(self);
  fTEFList.Add(fTefElgin);     // Adicionando "fTefElgin" na Lista Objetos de Classes de TEF
  {$IFDEF COMPILER6_UP}
   fTefElgin.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  GPAtual := gpPayGo;
end;

destructor TACBrTEFD.Destroy;
begin
  fIdentificacao.Free ;
  fTEFList.Free;  // Destroi Lista de Classes TEF e Objetos internos
  fpRespostasPendentes.Free ;  // Destroi Lista de Respostas pendentes e Objetos internos

  inherited Destroy;
end;

procedure TACBrTEFD.Inicializar(GP : TACBrTEFDTipo);
Var
  I : Integer;
  Erros : String ;
begin
  if not Assigned( OnExibeMsg ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnExibeMsg" não programado' ) ) ;

  if not Assigned( OnComandaECF )  then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnComandaECF" não programado' ) ) ;

  if not Assigned( OnAguardaResp) and (GP = gpCliSiTef) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnAguardaResp" não programado' ) ) ;

  if not Assigned( OnComandaECFAbreVinculado )  then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnComandaECFAbreVinculado" não programado' ) ) ;

  if not Assigned( OnComandaECFImprimeVia )  then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnComandaECFImprimeVia" não programado' ) ) ;

  if not Assigned( OnInfoECF )  then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnInfoECF" não programado' ) ) ;

  if not DirectoryExists( PathBackup ) then
     ForceDirectories( PathBackup );

  if not DirectoryExists( PathBackup ) then
     raise EACBrTEFDErro.Create( ACBrStr('Diretório de Backup não existente:'+sLineBreak+PathBackup) ) ;
	
  if GP = gpNenhum then
   begin
     Erros := '' ;

     For I := 0 to fTEFList.Count-1 do
     begin
       if fTEFList[I].Habilitado then
       begin
         try
           fTEFList[I].Inicializado := True ;
         except
           On E : Exception do
           begin
             fTEFList[I].Inicializado := False ;
             Erros := Erros + E.Message + sLineBreak ;
           end;
         end;
       end;
     end;

     if Erros <> '' then
        raise EACBrTEFDErro.Create( ACBrStr( Erros ) ) ;
   end
  else
   begin
     GPAtual := GP ;
     try
       fTefClass.Inicializado := True;
       fTefClass.Habilitado   := True;
     except
       fTefClass.Inicializado := False;
       raise ;
     end;
   end;
end;

procedure TACBrTEFD.DesInicializar(GP : TACBrTEFDTipo);
var
   I : Integer;
begin
  if GP = gpNenhum then
   begin
     For I := 0 to fTEFList.Count-1 do
     begin
       if fTEFList[I].Habilitado then
          fTEFList[I].Inicializado := False ;
     end;
   end
  else
   begin
     GPAtual := GP ;
     fTefClass.Inicializado := False;
   end;
end;

procedure TACBrTEFD.SetGPAtual(const AValue : TACBrTEFDTipo);
begin
  if AValue = fGPAtual then exit ;

  case AValue of
    gpPayGo     : fTefClass := fTefPayGo ;
    gpPayGoWeb  : fTefClass := fTefPayGoWeb ;
    gpTefDial   : fTefClass := fTefDial ;
    gpTefDisc   : fTefClass := fTefDisc ;
    gpHiperTef  : fTefClass := fTefHiper ;
    gpCliSiTef  : fTefClass := fTefCliSiTef;
    gpVeSPague  : fTefClass := fTefVeSPague;
    gpTefGpu    : fTefClass := fTefGPU;
    gpBanese    : fTefClass := fTefBanese ;
    gpTefAuttar : fTefClass := fTefAuttar ;
    gpGoodCard  : fTefClass := fTefGood ;
    gpFoxWin    : fTefClass := fTefFW ;
    gpCliDTEF   : fTefClass := fTefCliDTEF ;
    gpPetroCard : fTefClass := fTefPetrocard ;
    gpCrediShop : fTefClass := fTefCrediShop ;
    gpTicketCar : fTefClass := fTefTicketCar ;
    gpConvCard  : fTefClass := fTefConvCard ;
    gpCappta    : fTefClass := fTefCappta ;
    gpCliSiTefModular : fTefClass := fTefCliSiTefModular;
    gpTefDirecao : fTefClass := fTefDirecao ;
    gpTefDialScopeGetcard : fTefClass := fTefDialScopeGetcard ;
    gpTefElgin: fTefClass := fTefElgin ;
  end;

  fGPAtual := AValue;
end;

function TACBrTEFD.EstadoECF : AnsiChar;
Var
  Retorno : String ;
begin
  Retorno := InfoECFAsString( ineEstadoECF ) ;
  Result  := upcase( AnsiChar(PadRight(Retorno,1)[1]) );

  if not (Result in ['L','V','P','C','G','R','N','O']) then
     raise EACBrTEFDECF.Create(
        ACBrStr( 'Retorno de "OnInfoEcf( ineEstadoECF, Retorno )" deve ser:'+sLineBreak+
                 '"L" = Livre'+sLineBreak+
                 '"V" = Venda de Itens'+sLineBreak+
                 '"P" - Pagamento (ou SubTotal efetuado)'+sLineBreak+
                 '"C" ou "R" - CDC ou Cupom Vinculado'+sLineBreak+
                 '"G" ou "R" - Relatório Gerencial'+sLineBreak+
                 '"N" - Recebimento Não Fiscal'+sLineBreak+
                 '"O" - Outro' ) );
end;

procedure TACBrTEFD.AtivarGP(GP : TACBrTEFDTipo);
Var
  I : Integer;
begin
  if GP = gpNenhum then
   begin
     For I := 0 to fTEFList.Count-1 do
     begin
       if fTEFList[I].Habilitado then
          fTEFList[I].AtivarGP;
     end;
   end
  else
   begin
     GPAtual := GP ;
     fTefClass.AtivarGP;
   end;
end;

procedure TACBrTEFD.ATV( GP : TACBrTEFDTipo = gpNenhum );
begin
  if GP <> gpNenhum then
     GPAtual := GP ;

  fTefClass.ATV;
end;

function TACBrTEFD.ADM(GP: TACBrTEFDTipo): Boolean;
begin
  if GP <> gpNenhum then
     GPAtual := GP ;

  Result := fTefClass.ADM;
end;

function TACBrTEFD.CRT(const Valor: Double; const IndiceFPG_ECF: String;
  const DocumentoVinculado: String; const Moeda: Integer): Boolean;
begin
   Result := fTefClass.CRT( Valor, IndiceFPG_ECF, DocumentoVinculado, Moeda );
end;

function TACBrTEFD.CHQ(const Valor: Double; const IndiceFPG_ECF: String;
  const DocumentoVinculado: String; const CMC7: String;
  const TipoPessoa: AnsiChar; const DocumentoPessoa: String;
  const DataCheque: TDateTime; const Banco: String; const Agencia: String;
  const AgenciaDC: String; const Conta: String; const ContaDC: String;
  const Cheque: String; const ChequeDC: String; const Compensacao: String
  ): Boolean;
begin
   Result := fTefClass.CHQ( Valor, IndiceFPG_ECF, DocumentoVinculado, CMC7,
                            TipoPessoa,  DocumentoPessoa, DataCheque,
                            Banco, Agencia, AgenciaDC,
                            Conta, ContaDC, Cheque, ChequeDC, Compensacao);
end;

function TACBrTEFD.CNC(const Rede, NSU: String;
  const DataHoraTransacao: TDateTime; const Valor: Double;
  CodigoAutorizacaoTransacao: String): Boolean;
begin
  Result := fTefClass.CNC( Rede, NSU, DataHoraTransacao, Valor, CodigoAutorizacaoTransacao);
end;

procedure TACBrTEFD.CNF(const Rede, NSU, Finalizacao : String;
  const DocumentoVinculado : String = '');
begin
  fTefClass.CNF( Rede, NSU, Finalizacao, DocumentoVinculado);
end;

procedure TACBrTEFD.NCN(const Rede, NSU, Finalizacao : String;
  const Valor : Double = 0; const DocumentoVinculado : String = '');
begin
  fTefClass.NCN( Rede, NSU, Finalizacao, Valor, DocumentoVinculado);
end;

function TACBrTEFD.PRE(Valor: Double; DocumentoVinculado: String;
  Moeda: Integer): Boolean;
begin
  Result := fTefClass.PRE(Valor, DocumentoVinculado, Moeda);
end;

procedure TACBrTEFD.CancelarTransacoesPendentes;
Var
  I : Integer;
begin
  { Ajustando o mesmo valor nas Classes de TEF, caso elas usem o valor default }
  try
    For I := 0 to fTEFList.Count-1 do
    begin
      if fTEFList[I].Habilitado then
        fTEFList[I].CancelarTransacoesPendentesClass;
    end;
  finally
    RespostasPendentes.Clear;
  end;
end;

function TACBrTEFD.CDP(const EntidadeCliente: String; out Resposta: String): Boolean;
begin
  Result := fTefClass.CDP(EntidadeCliente, Resposta);
end;

procedure TACBrTEFD.ConfirmarTransacoesPendentes(ApagarRespostasPendentes: Boolean);
var
  I : Integer;
begin
  fTefClass.GravaLog( 'ConfirmarTransacoesPendentes' );

  I := 0;
  while I < RespostasPendentes.Count do
  begin
    try
      with TACBrTEFDResp(RespostasPendentes[I]) do
      begin
        GPAtual := TipoGP;   // Seleciona a Classe do GP

        if not CNFEnviado then
        begin
          CNF( Rede, NSU, Finalizacao, DocumentoVinculado );
          CNFEnviado := True;
        end;

        if ApagarRespostasPendentes then
        begin
          ApagaEVerifica( ArqRespPendente );
          ApagaEVerifica( ArqBackup );
        end;

        Inc( I ) ;
      end;
    except
      { Exceção Muda... Fica em Loop até conseguir confirmar e apagar Backup }
    end;
  end ;

  try
    if (RespostasPendentes.Count > 0) and Assigned( fOnDepoisConfirmarTransacoes ) then
      fOnDepoisConfirmarTransacoes( RespostasPendentes );
  finally
    if ApagarRespostasPendentes then
      RespostasPendentes.Clear;
  end;
end;

procedure TACBrTEFD.ImprimirTransacoesPendentes;
var
   I, J, K, NVias, Ordem : Integer;
   GrupoVinc : TACBrTEFDArrayGrupoRespostasPendentes ;
   ImpressaoOk, Gerencial, RemoverMsg, GerencialAberto : Boolean ;
   TempoInicio : Double;
   Est : AnsiChar ;
   MsgAutenticacaoAExibir : String ;
begin
  if RespostasPendentes.Count <= 0 then
     exit ;

  fTefClass.GravaLog( 'ImprimirTransacoesPendentes' ) ;

  Est := EstadoECF;

  if Est <> 'L' then
  begin
     case Est of
       'V', 'P', 'N' : FinalizarCupom( False );  { False não desbloqueia o MouseTeclado }
       'R', 'G'      : ComandarECF( opeFechaGerencial );
       'C'           : ComandarECF( opeFechaVinculado );
     end;

     if EstadoECF <> 'L' then
        raise EACBrTEFDECF.Create( ACBrStr(CACBrTEFD_Erro_ECFNaoLivre) ) ;
  end;

  if fConfirmarAntesDosComprovantes then
    ConfirmarTransacoesPendentes(False); //Não apaga agora, pois será usado na impressão

  ImpressaoOk            := False ;
  Gerencial              := False ;
  RemoverMsg             := False ;
  GerencialAberto        := False ;
  MsgAutenticacaoAExibir := '' ;

  GrupoVinc := nil ;
  AgruparRespostasPendentes( GrupoVinc );

  try
     BloquearMouseTeclado( True );

     while not ImpressaoOk do
     begin
        try
           try
              if Gerencial then    //// Impressão em Gerencial ////
               begin
                 Est := EstadoECF;

                 if Est <> 'L' then
                 begin
                    { Fecha Vinculado ou Gerencial, se ficou algum aberto por Desligamento }
                    case Est of
                      'C'      : ComandarECF( opeFechaVinculado );
                      'G', 'R' : ComandarECF( opeFechaGerencial );
                    end;

                    if EstadoECF <> 'L' then
                       raise EACBrTEFDECF.Create( ACBrStr(CACBrTEFD_Erro_ECFNaoLivre) ) ;
                 end;

                 GerencialAberto := False ;

                 For J := 0 to RespostasPendentes.Count-1 do
                 begin
                    with TACBrTEFDResp(RespostasPendentes[J]) do
                    begin
                       GPAtual := TipoGP;  // Seleciona a Classe do GP

                       TempoInicio := now ;

                       // Calcula numero de vias //
                       NVias := fTefClass.NumVias ;
                       if (ImagemComprovante2aVia.Count = 0) then   // Tem 2a via ?
                          NVias := 1 ;
                       if (ImagemComprovante1aVia.Count = 0) then   // Tem alguma via ?
                          NVias := 0 ;

                       if NVias > 0 then   // Com Impressao, deixe a MSG na Tela
                        begin
                          if TextoEspecialOperador <> '' then
                          begin
                             RemoverMsg := True ;
                             DoExibeMsg( opmExibirMsgOperador, TextoEspecialOperador ) ;
                          end;

                          if TextoEspecialCliente <> '' then
                          begin
                             RemoverMsg := True ;
                             DoExibeMsg( opmExibirMsgCliente, TextoEspecialCliente ) ;
                          end;
                        end
                       else  // Sem Impressao, Exiba a Msg com OK
                        begin
                          if (TextoEspecialOperador + TextoEspecialCliente) <> '' then
                          begin
                            DoExibeMsg( opmOK,
                                        TextoEspecialOperador +
                                        ifthen( TextoEspecialOperador <> '',
                                                sLineBreak+sLineBreak, '') +
                                        TextoEspecialCliente ) ;
                          end ;
                        end ;

                       if (not GerencialAberto) and (NVias > 0) then
                       begin
                          ComandarECF( opeAbreGerencial ) ;
                          GerencialAberto := True ;
                       end;

                       I := 1 ;
                       while I <= NVias do
                       begin
                          if I = 1 then
                             ECFImprimeVia( trGerencial, I, ImagemComprovante1aVia  )
                          else
                             ECFImprimeVia( trGerencial, I, ImagemComprovante2aVia  ) ;

                          if (I < NVias) or (J < RespostasPendentes.Count-1) then
                          begin
                             ComandarECF( opePulaLinhas ) ;
                             DoExibeMsg( opmDestaqueVia,
                                         Format( CACBrTEFD_DestaqueVia, [I]) ) ;
                          end;

                          Inc( I ) ;
                       end;

                       { Removendo a mensagem do Operador }
                       if RemoverMsg then
                       begin
                          AguardarTempoMinimoDeExibicao( TempoInicio );
                          DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                          DoExibeMsg( opmRemoverMsgCliente, '' ) ;
                          RemoverMsg := False ;
                       end;

                       if ExibirMsgAutenticacao and (Autenticacao <> '') then
                          MsgAutenticacaoAExibir := 'Favor anotar no verso do Cheque:'+sLineBreak+
                                                    Autenticacao ;

                       if (J < RespostasPendentes.Count-1) and // Nao é o ultimo ? (se for a última é preferivel fechar o comprovante antes)
                          (MsgAutenticacaoAExibir <> '') then  // Tem autenticação ?
                       begin
                          DoExibeMsg( opmOK, MsgAutenticacaoAExibir ) ;
                          MsgAutenticacaoAExibir := '' ;
                       end;
                    end;
                 end ;

                 if GerencialAberto then
                    ComandarECF( opeFechaGerencial );
               end
              else                 //// Impressão em Vinculado ////
               begin
                 Ordem := -1 ;

                 For K := 0 to Length( GrupoVinc )-1 do
                 begin
                    if GrupoVinc[K].OrdemPagamento >= 999 then
                       Gerencial := True;

                    For J := 0 to RespostasPendentes.Count-1 do
                    begin
                       with TACBrTEFDResp(RespostasPendentes[J]) do
                       begin
                          if GrupoVinc[K].OrdemPagamento <> OrdemPagamento then
                             continue ;

                          GPAtual := TipoGP;    // Seleciona a Classe do GP

                          TempoInicio := now ;

                          // Calcula numero de vias //
                          NVias := fTefClass.NumVias ;
                          if (ImagemComprovante2aVia.Count = 0) then   // Tem 2a via ?
                             NVias := 1 ;
                          if (ImagemComprovante1aVia.Count = 0) then   // Tem alguma via ?
                             NVias := 0 ;

                          if NVias > 0 then   // Com Impressao, deixe a MSG na Tela
                           begin
                             if TextoEspecialOperador <> '' then
                             begin
                                RemoverMsg := True ;
                                DoExibeMsg( opmExibirMsgOperador, TextoEspecialOperador ) ;
                             end;

                             if TextoEspecialCliente <> '' then
                             begin
                                RemoverMsg := True ;
                                DoExibeMsg( opmExibirMsgCliente, TextoEspecialCliente ) ;
                             end;
                           end
                          else  // Sem Impressao, Exiba a Msg com OK
                           begin
                             if (TextoEspecialOperador + TextoEspecialCliente) <> '' then
                             begin
                               DoExibeMsg( opmOK,
                                           TextoEspecialOperador +
                                           ifthen( TextoEspecialOperador <> '',
                                                   sLineBreak+sLineBreak, '') +
                                           TextoEspecialCliente ) ;
                             end ;
                           end ;

                          if (NVias > 0) and (Ordem <> OrdemPagamento) then
                          begin
                             Ordem := OrdemPagamento ;
                             if Gerencial then
                              begin
                                ComandarECF( opeAbreGerencial );
                                GerencialAberto := True;
                              end
                             else
                                ECFAbreVinculado( DocumentoVinculado,
                                                  GrupoVinc[K].IndiceFPG_ECF,
                                                  GrupoVinc[K].Total ) ;
                          end ;

                          I := 1 ;
                          while I <= NVias do
                          begin
                             if Gerencial then
                              begin
                                if I = 1 then
                                   ECFImprimeVia( trGerencial, I, ImagemComprovante1aVia )
                                else
                                   ECFImprimeVia( trGerencial, I, ImagemComprovante2aVia ) ;
                              end
                             else
                              begin
                                if I = 1 then
                                   ECFImprimeVia( trVinculado, I, ImagemComprovante1aVia )
                                else
                                   ECFImprimeVia( trVinculado, I, ImagemComprovante2aVia ) ;
                              end ;

                             if (I < NVias) or (J < RespostasPendentes.Count-1) then
                             begin
                                ComandarECF( opePulaLinhas ) ;
                                DoExibeMsg( opmDestaqueVia,
                                            Format( CACBrTEFD_DestaqueVia, [I]) ) ;
                             end;

                             Inc( I ) ;
                          end;

                          { Removendo a mensagem do Operador }
                          if RemoverMsg then
                          begin
                             AguardarTempoMinimoDeExibicao( TempoInicio );
                             DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                             DoExibeMsg( opmRemoverMsgCliente, '' ) ;
                             RemoverMsg := False ;
                          end;

                          if ExibirMsgAutenticacao and (Autenticacao <> '') then
                             MsgAutenticacaoAExibir := 'Favor anotar no verso do Cheque:'+sLineBreak+
                                                       Autenticacao ;

                          if (J < RespostasPendentes.Count-1) and // Nao é o ultimo ? (se for a última é preferivel fechar o comprovante antes)
                             (MsgAutenticacaoAExibir <> '') then  // Tem autenticação ?
                          begin
                             DoExibeMsg( opmOK, MsgAutenticacaoAExibir ) ;
                             MsgAutenticacaoAExibir := '' ;
                          end;
                       end;
                    end ;

                    if Ordem > -1 then
                    begin
                       if GerencialAberto then
                        begin
                          ComandarECF( opeFechaGerencial );
                          GerencialAberto := False;
                        end
                       else
                          ComandarECF( opeFechaVinculado ) ;
                    end;
                 end;
               end;

              ImpressaoOk := True ;
           finally
              { Removendo a mensagem do Operador }
              if RemoverMsg then
              begin
                 DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                 DoExibeMsg( opmRemoverMsgCliente, '' ) ;
              end;
           end;
        except
           on EACBrTEFDECF do ImpressaoOk := False ;
           else
              raise ;
        end;

        if not ImpressaoOk then
        begin
          if DoExibeMsg( opmYesNo, CACBrTEFD_Erro_ECFNaoResponde ) <> mrYes then
             break ;
        end;

        Gerencial := True ;
     end;
  finally
    if not (fConfirmarAntesDosComprovantes or ImpressaoOk) then
    begin
      try ComandarECF(opeCancelaCupom); except {Exceção Muda} end;
      CancelarTransacoesPendentes;
    end
    else if (not fConfirmarAntesDosComprovantes) then
      ConfirmarTransacoesPendentes
    else
      ApagarRespostasPendentes;


    BloquearMouseTeclado( False );

    if (MsgAutenticacaoAExibir <> '') then  // Tem autenticação ?
      DoExibeMsg( opmOK, MsgAutenticacaoAExibir ) ;
  end;

  RespostasPendentes.Clear;
end;

procedure TACBrTEFD.ApagarRespostasPendentes;
var
  I : Integer;
begin
  fTefClass.GravaLog( 'ApagarRespostasPendentes' );

  I := 0;
  while I < RespostasPendentes.Count do
  begin
    try
      with TACBrTEFDResp(RespostasPendentes[I]) do
      begin
        GPAtual := TipoGP;   // Seleciona a Classe do GP

        ApagaEVerifica( ArqRespPendente );
        ApagaEVerifica( ArqBackup );

        Inc( I ) ;
      end;
    except
      { Exceção Muda... Fica em Loop até conseguir confirmar e apagar Backup }
    end;
  end ;

  RespostasPendentes.Clear;
end;

function TACBrTEFD.DoExibeMsg(Operacao: TACBrTEFDOperacaoMensagem;
  const Mensagem: String; ManterTempoMinimo: Boolean): TModalResult;
var
  OldTecladoBloqueado : Boolean;
  TempoInicial : TDateTime;
begin
  fTefClass.GravaLog( fTefClass.Name +' DoExibeMsg: Oper: '+
     GetEnumName(TypeInfo(TACBrTEFDOperacaoMensagem), Integer(Operacao) )+
     ' Mensagem: '+Mensagem ) ;

  if Operacao = opmExibirMsgCliente then
     TempoInicial := fTempoInicialMensagemCliente
  else
     TempoInicial := fTempoInicialMensagemOperador;

  if TempoInicial > 0 then    // A mensagem anterior fixou um Tempo mínimo de exibição ?
     AguardarTempoMinimoDeExibicao( TempoInicial );

  if (Operacao in [opmOK, opmYesNo, opmDestaqueVia]) then
     RestaurarFocoAplicacao ;

  OldTecladoBloqueado := TecladoBloqueado;

  if OldTecladoBloqueado and ( Operacao in [opmOK, opmYesNo] ) then
     BloquearMouseTeclado( False ) ;

  Result := mrNone ;
  OnExibeMsg( Operacao, ACBrStr( Mensagem ), Result );

  if OldTecladoBloqueado and ( Operacao in [opmOK, opmYesNo] ) then
     BloquearMouseTeclado( True ) ;

  if ManterTempoMinimo then
     TempoInicial := Now
  else
     TempoInicial := 0;

  if Operacao = opmExibirMsgCliente then
     fTempoInicialMensagemCliente  := TempoInicial
  else
     fTempoInicialMensagemOperador := TempoInicial ;
end;

procedure TACBrTEFD.DoExibeQRCode(const Dados: String);
begin
  if not Assigned(fOnExibeQRCode) then
    DoExibeMsg( opmExibirMsgCliente, 'QRCODE=' + Dados)
  else
    fOnExibeQRCode(Dados);
end;

function TACBrTEFD.ComandarECF(Operacao: TACBrTEFDOperacaoECF): Integer;
var
   OpName, Erro : String;
begin
  fTefClass.GravaLog( fTefClass.Name +' ComandarECF: Oper: '+
    GetEnumName( TypeInfo(TACBrTEFDOperacaoECF), Integer(Operacao) ) ) ;

  Result := -1 ;  // -1 = Não tratado
  OnComandaECF( Operacao, fTefClass.Resp, Result ) ;

  if Result < 1 then
  begin
     OpName := GetEnumName( TypeInfo(TACBrTEFDOperacaoECF), Integer(Operacao) ) ;

     if Result = 0 then
        Erro := 'Erro ao executar Operação: ['+OpName+']'
     else
        Erro := 'Operação ['+OpName+'] não tratada em "OnComandaECF"' ;

     fTefClass.GravaLog(Erro);

     raise EACBrTEFDECF.Create( ACBrStr( Erro ) )
  end;
end;

function TACBrTEFD.ECFSubtotaliza(DescAcres: Double): Integer;
Var
   Erro : String ;
begin
  fTefClass.GravaLog( fTefClass.Name +' ECFSubtotaliza: DescAcres: '+
    FormatFloat('0.00',DescAcres) ) ;

  if not Assigned( OnComandaECFSubtotaliza ) then
  begin
     ComandarECF( opeSubTotalizaCupom );
     exit;
  end;

  Result := -1 ;  // -1 = Não tratado
  OnComandaECFSubtotaliza( DescAcres, Result ) ;

  if Result < 1 then
  begin
     if Result = 0 then
        Erro := 'Erro ao executar "OnComandaECFSubtotaliza"'
     else
        Erro := '"OnComandaECFSubtotaliza" não tratada' ;

     fTefClass.GravaLog(Erro);

     raise EACBrTEFDECF.Create( ACBrStr( Erro ) )
  end;

  ComandarECF( opeImprimePagamentos ) ;  // Deve imprimir Dinheiro antes dos pagamentos do TEF

end;

function TACBrTEFD.ECFPagamento(Indice: String; Valor: Double): Integer;
Var
   Erro : String ;
begin
  fTefClass.GravaLog( fTefClass.Name +' ECFPagamento: Indice: '+
    Indice + ' Valor: '+FormatFloat('0.00',Valor) ) ;

  Result := -1 ;  // -1 = Não tratado
  OnComandaECFPagamento( Indice, Valor, Result ) ;

  if Result < 1 then
  begin
     if Result = 0 then
        Erro := 'Erro ao executar "OnComandaECFPagamento"'
     else
        Erro := '"OnComandaECFPagamento" não tratada' ;

     fTefClass.GravaLog(Erro);

     raise EACBrTEFDECF.Create( ACBrStr( Erro ) )
  end;
end;

function TACBrTEFD.ECFAbreVinculado(COO, Indice : String; Valor : Double
   ) : Integer;
Var
   Erro : String ;
begin
  fTefClass.GravaLog( fTefClass.Name +' ECFAbreVinculado: COO: '+COO+' Indice: '+
    Indice + ' Valor: '+FormatFloat('0.00',Valor) ) ;

  Result := -1 ;  // -1 = Não tratado
  OnComandaECFAbreVinculado( COO, Indice, Valor, Result ) ;

  if Result < 1 then
  begin
     if Result = 0 then
        Erro := 'Erro ao executar "OnComandaECFAbreVinculado"'
     else
        Erro := '"OnComandaECFAbreVinculado" não tratado'  ;

     fTefClass.GravaLog(Erro);

     raise EACBrTEFDECF.Create( ACBrStr( Erro ) )
  end;
end;

function TACBrTEFD.ECFImprimeVia( TipoRelatorio : TACBrTEFDTipoRelatorio;
   Via : Integer; ImagemComprovante : TStringList) : Integer;
Var
   Erro : String ;
begin
  fTefClass.GravaLog( fTefClass.Name +' ECFImprimeVia: '+
    GetEnumName( TypeInfo(TACBrTEFDTipoRelatorio), Integer(TipoRelatorio) ) +
    ' Via: '+IntToStr(Via) ) ;

  Result := -1 ;  // -1 = Não tratado
  OnComandaECFImprimeVia( TipoRelatorio, Via, ImagemComprovante, Result ) ;

  if Result < 1 then
  begin
     if Result = 0 then
        Erro := 'Erro ao executar "OnComandaECFImprimeVia"'
     else
        Erro := '"OnComandaECFImprimeVia" não tratado' ;

     fTefClass.GravaLog(Erro);

     raise EACBrTEFDECF.Create( ACBrStr( Erro ) )
  end;
end;

procedure TACBrTEFD.FinalizarCupom(DesbloquearMouseTecladoNoTermino: Boolean);
Var
  I, J, Ordem : Integer;
  Est, EstNaoFiscal  : AnsiChar;
  EhNaoFiscal: Boolean;
  ImpressaoOk : Boolean ;
  GrupoFPG    : TACBrTEFDArrayGrupoRespostasPendentes ;
begin
  ImpressaoOk := False ;
  fTefClass.GravaLog( 'FinalizarCupom'+IfThen(DesbloquearMouseTecladoNoTermino,
                      ', DesbloquearMouseTecladoNoTermino','') ) ;

  try
     while not ImpressaoOk do
     begin
        try
           BloquearMouseTeclado( True );

           try
              EstNaoFiscal := 'N';
              Est          := EstadoECF;
              EhNaoFiscal  := (Est = 'N');

              while (Est <> 'L') do
              begin
                 if Est = 'O' then
                   raise EACBrTEFDECF.Create(CACBrTEFD_Erro_ECFEstado);

                 // É não fiscal ? Se SIM, vamos passar por todas as fases...
                 if EhNaoFiscal then
                 begin
                    case EstNaoFiscal of
                      'N' : EstNaoFiscal := 'V' ;
                      'V' : EstNaoFiscal := 'P' ;
                      'P' : EstNaoFiscal := 'N' ;
                    end ;

                    Est := EstNaoFiscal ;
                 end ;

                 try
                    Case Est of
                      'V' : ECFSubtotaliza( RespostasPendentes.TotalDesconto );

                      'P' :
                        begin
                          if not AutoEfetuarPagamento then
                          begin
                             GrupoFPG := nil ;
                             AgruparRespostasPendentes( GrupoFPG );
                             Ordem := 0 ;

                             For I := 0 to Length( GrupoFPG )-1 do
                             begin
                                if GrupoFPG[I].OrdemPagamento = 0 then
                                 begin
                                   Inc( Ordem ) ;

                                   if (InfoECFAsDouble(ineSubTotal) > 0) and (GrupoFPG[I].Total > 0)  then
                                      ECFPagamento( GrupoFPG[I].IndiceFPG_ECF, GrupoFPG[I].Total );

                                   For J := 0 to RespostasPendentes.Count-1 do
                                   with TACBrTEFDResp(RespostasPendentes[J]) do
                                   begin
                                      if IndiceFPG_ECF = GrupoFPG[I].IndiceFPG_ECF then
                                      begin
                                        if (Header = 'CHQ') and CHQEmGerencial then
                                         begin
                                           OrdemPagamento := 999;
                                           Dec( Ordem ) ;
                                         end
                                        else
                                           OrdemPagamento := Ordem;
                                      end;
                                   end;
                                 end
                                else
                                   Ordem := GrupoFPG[I].OrdemPagamento ;
                             end;
                          end;

                          if (InfoECFAsDouble(ineSubTotal) > 0) then
                          begin
                             if (InfoECFAsDouble(ineTotalAPagar,0) > 0) then
                             begin
                                ComandarECF( opeImprimePagamentos ) ;

                                if InfoECFAsDouble(ineSubTotal) > 0 then
                                   Break;
                              end
                             else
                                Break;
                          end ;

                          ComandarECF( opeFechaCupom )
                        end ;

                      'N' :     // Usado apenas no Fechamento de NaoFiscal
                        begin
                          if (InfoECFAsDouble(ineSubTotal) > 0) then
                          begin
                             if (InfoECFAsDouble(ineTotalAPagar,0) > 0) then
                             begin
                                ComandarECF( opeImprimePagamentos ) ;

                                if InfoECFAsDouble(ineSubTotal) > 0 then
                                   Break;
                              end
                             else
                                Break;
                          end ;

                          ComandarECF( opeFechaCupom )
                        end ;
                    else
                      raise EACBrTEFDErro.Create(
                         ACBrStr('ECF deve estar em Venda ou Pagamento'));
                    end;
                 except
                    { A condição abaixo, será True se não for Cupom Nao Fiscal,
                       ou se já tentou todas as fases do Cupom Nao Fiscal
                       (SubTotaliza, Pagamento, Fechamento)...
                      Se for NaoFiscal não deve disparar uma exceção até ter
                       tentado todas as fases descritas acima, pois o ACBrECF
                       não é capaz de detectar com precisão a fase atual do
                       Cupom Não Fiscal (poucos ECFs possuem flags para isso) }

                    if EstNaoFiscal = 'N' then
                       raise ;
                 end ;

                 Est := EstadoECF;
              end;

              ImpressaoOk := True ;

           finally
              if DesbloquearMouseTecladoNoTermino then
                 BloquearMouseTeclado( False );
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
          begin
             try ComandarECF(opeCancelaCupom); except {Exceção Muda} end ;
             break ;
          end;
        end;
     end;
  finally
    if not ImpressaoOk then
       CancelarTransacoesPendentes;
  end;
end;

procedure TACBrTEFD.AgruparRespostasPendentes(
   var Grupo : TACBrTEFDArrayGrupoRespostasPendentes);
var
   I, J      : Integer;
   LenArr    : Integer;
   IndiceFPG : String;
   Ordem     : Integer;
   wTotal: Double;
   wIndiceFPG_ECF: String;
   wOrdemPagamento: Integer;
   Trocou: Boolean;
begin
  SetLength( Grupo, 0) ;

  For I := 0 to RespostasPendentes.Count-1 do
  begin
     with TACBrTEFDResp(RespostasPendentes[I]) do
     begin
       IndiceFPG := IndiceFPG_ECF ;
       Ordem     := OrdemPagamento ;
     end;

     J := 0 ;
     LenArr := Length( Grupo ) ;
     while J < LenArr do
     begin
       if (Grupo[J].IndiceFPG_ECF  = IndiceFPG) and
          (Grupo[J].OrdemPagamento = Ordem) then
          break ;
       Inc( J ) ;
     end;

     if J >= LenArr then
     begin
        SetLength( Grupo, J+1 ) ;
        Grupo[J].IndiceFPG_ECF  := IndiceFPG ;
        Grupo[J].OrdemPagamento := Ordem ;
        Grupo[J].Total  := 0 ;
     end;

     Grupo[J].Total := Grupo[J].Total + RespostasPendentes[I].ValorTotal ;
  end;

  // Ordenando por OrdemPagamento (usando Bubble sort) //
  LenArr := Length( Grupo ) ;
  repeat
     J      := 1 ;
     Trocou := False ;
     while J < LenArr do
     begin
        if Grupo[J].OrdemPagamento < Grupo[J-1].OrdemPagamento then
        begin
          Trocou := True;
          wOrdemPagamento := Grupo[J].OrdemPagamento ;
          wIndiceFPG_ECF  := Grupo[J].IndiceFPG_ECF ;
          wTotal          := Grupo[J].Total ;

          Grupo[J].OrdemPagamento := Grupo[J-1].OrdemPagamento ;
          Grupo[J].IndiceFPG_ECF  := Grupo[J-1].IndiceFPG_ECF ;
          Grupo[J].Total          := Grupo[J-1].Total ;

          Grupo[J-1].OrdemPagamento := wOrdemPagamento ;
          Grupo[J-1].IndiceFPG_ECF  := wIndiceFPG_ECF ;
          Grupo[J-1].Total          := wTotal ;
        end;

        Inc( J ) ;
     end;

     Dec( LenArr ) ;
  until not Trocou;
end;

procedure TACBrTEFD.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  fTefClass.ExibirMensagemPinPad(MsgPinPad);
end;

function TACBrTEFD.Inicializado( GP : TACBrTEFDTipo = gpNenhum ) : Boolean;
begin
  if GP <> gpNenhum then
     GPAtual := GP ;

  Result := fTefClass.Inicializado ;
end;

procedure TACBrTEFD.SetEsperaSTS(const AValue : Integer);
Var
  I : Integer;
begin
  { Ajustando o mesmo valor nas Classes de TEF, caso elas usem o valor default }
  For I := 0 to fTEFList.Count-1 do
  begin
    if fTEFList[I] is TACBrTEFDClassTXT then
    begin
       with TACBrTEFDClassTXT( fTEFList[I] ) do
       begin
          if EsperaSTS = fEsperaSTS then
             EsperaSTS := AValue;
       end;
    end;
  end;

  fEsperaSTS := AValue;
end;

procedure TACBrTEFD.SetEstadoReq(const AValue : TACBrTEFDReqEstado);
begin
   if fEstadoReq = AValue then exit;
   fEstadoReq := AValue;

   if Assigned( OnMudaEstadoReq ) then
      OnMudaEstadoReq( EstadoReq );
end;

procedure TACBrTEFD.SetEstadoResp(const AValue : TACBrTEFDRespEstado);
begin
   if fEstadoResp = AValue then exit;
   fEstadoResp := AValue;

   if Assigned( OnMudaEstadoResp ) then
      OnMudaEstadoResp( EstadoResp );
end;

procedure TACBrTEFD.SetEsperaSleep(const AValue : Integer);
begin
   if fEsperaSleep = AValue then exit;

   if AValue < 10 then
      raise EACBrTEFDErro.Create( ACBrStr('Valor mínimo de EsperaSleep deve ser: 10' )) ;
   if AValue > 500 then
      raise EACBrTEFDErro.Create( ACBrStr('Valor máximo de EsperaSleep deve ser: 500' )) ;

   fEsperaSleep := AValue;
end;

procedure TACBrTEFD.SetMultiplosCartoes(const AValue : Boolean);
begin
  if fMultiplosCartoes = AValue then
     exit;

  if RespostasPendentes.Count > 0 then
     raise EACBrTEFDErro.Create( ACBrStr( 'Existem Respostas Pendentes. '+
                              'Não é possível alterar "MultiplosCartoes"') ) ;

   fMultiplosCartoes := AValue;
end;

procedure TACBrTEFD.SetNumeroMaximoCartoes(const AValue: Integer);
begin
   fNumeroMaximoCartoes := max(AValue,0);
end;

procedure TACBrTEFD.SetAutoEfetuarPagamento(const AValue : Boolean);
begin
  if fAutoEfetuarPagamento = AValue then
     exit;

  if RespostasPendentes.Count > 0 then
     raise EACBrTEFDErro.Create( ACBrStr( 'Existem Respostas Pendentes. '+
                             'Não é possível alterar "AutoEfetuarPagamento"') ) ;

  fAutoEfetuarPagamento := AValue;
end;

procedure TACBrTEFD.SetAutoFinalizarCupom(const AValue : Boolean);
begin
  if fAutoFinalizarCupom = AValue then
     exit ;

  if RespostasPendentes.Count > 0 then
     raise EACBrTEFDErro.Create( ACBrStr( 'Existem Respostas Pendentes. '+
                             'Não é possível alterar "AutoFinalizarCupom"') ) ;

  fAutoFinalizarCupom := AValue;
end;

procedure TACBrTEFD.SetNumVias(const AValue : Integer);
var
   I : Integer;
begin
  For I := 0 to fTEFList.Count-1 do
  begin
    if fTEFList[I] is TACBrTEFDClass then
    begin
       with TACBrTEFDClass( fTEFList[I] ) do
       begin
          if NumVias = fNumVias then
             NumVias := AValue;
       end;
    end;
  end;

  fNumVias := AValue;
end;

function TACBrTEFD.GetPathBackup : String;
begin
  if fPathBackup = '' then
     if not (csDesigning in Self.ComponentState) then
        fPathBackup := ExtractFilePath( ParamStr(0) ) + 'TEF' ;

  Result := fPathBackup ;
end;

function TACBrTEFD.GetArqReq : String;
begin
  if fTefClass is TACBrTEFDClassTXT then
     Result := TACBrTEFDClassTXT(fTefClass).ArqReq
  else
     Result := '' ;
end;

procedure TACBrTEFD.AguardarTempoMinimoDeExibicao(const TempoInicial: TDateTime
  );
var
  Interromper: Boolean;
  TempoCorrido: Int64;
begin

  { Verifica se Mensagem Ficou pelo menos por 5 segundos }
  TempoCorrido := SecondsBetween(now,TempoInicial);

  while TempoCorrido < fEsperaMinimaMensagemFinal do
  begin
     fTefClass.GravaLog( fTefClass.Name + ' AguardarTempoMinimoDeExibicao: ' +
                         FormatFloat('###.##', TempoCorrido) + ' segundos' );

     // Avisa a aplicação, que está em Espera //
     Interromper := False;
     if Assigned( OnAguardaResp ) then
       OnAguardaResp( 'TempoMinimoMensagemFinal', TempoCorrido, Interromper ) ;

     Sleep(EsperaSleep) ;
     {$IFNDEF NOGUI}
      Application.ProcessMessages;
     {$ENDIF}

     TempoCorrido := SecondsBetween(now,TempoInicial);
  end;
end;

function TACBrTEFD.GetAguardandoResposta: Boolean;
begin
  Result := fTefClass.AguardandoResposta ;
end;

procedure TACBrTEFD.SetArqLOG(const AValue: String);
var
   I : Integer;
begin
  { Ajustando o mesmo valor nas Classes de TEF, caso elas usem o valor default }
  For I := 0 to fTEFList.Count-1 do
  begin
    with TACBrTEFDClassTXT( fTEFList[I] ) do
    begin
       if ArqLOG = fArqLOG then
          ArqLOG := AValue;
    end;
  end;

  fArqLOG := AValue;
end;

function TACBrTEFD.InfoECFAsString(Operacao: TACBrTEFDInfoECF): String;
var
   Retorno: String;
begin
   Retorno := '';
   fTefClass.GravaLog( 'InfoECF: '+
     GetEnumName(TypeInfo(TACBrTEFDInfoECF), Integer(Operacao) ) ) ;

   while Retorno = '' do
   begin
      try
         OnInfoEcf( Operacao, Retorno ) ;
      except
         On E : Exception do
         begin
            fTefClass.GravaLog( fTefClass.Name +'   Erro: '+E.Message ) ;

            if DoExibeMsg( opmYesNo, CACBrTEFD_Erro_ECFNaoRespondeInfo ) <> mrYes then
               raise EACBrTEFDECF.Create(E.Message);
         end;
      end;
   end;

   fTefClass.GravaLog( '    Ret: '+Retorno ) ;
   Result := Retorno;
end;

function TACBrTEFD.InfoECFAsDouble(Operacao: TACBrTEFDInfoECF;
   DefaultValue: Integer): Double;
var
   Retorno: String;
begin
   Retorno := InfoECFAsString( Operacao );
   Result  := RoundTo( StringToFloatDef( Retorno, DefaultValue), -2 );

   if Result = -98787158 then
      raise EACBrTEFDErro.Create( ACBrStr(
         'Erro na conversão do Valor Retornado de: OnInfoECF( '+
         GetEnumName(TypeInfo(TACBrTEFDInfoECF), Integer(Operacao)) + ')' ) );
end;

function TACBrTEFD.GetArqResp: String;
begin
  if fTefClass is TACBrTEFDClassTXT then
     Result := TACBrTEFDClassTXT(fTefClass).ArqResp
  else
     Result := '' ;
end;

function TACBrTEFD.GetArqSTS : String;
begin
  if fTefClass is TACBrTEFDClassTXT then
     Result := TACBrTEFDClassTXT(fTefClass).ArqSTS
  else
     Result := '' ;
end;

function TACBrTEFD.GetArqTmp : String;
begin
  if fTefClass is TACBrTEFDClassTXT then
     Result := TACBrTEFDClassTXT(fTefClass).ArqTemp
  else
     Result := '' ;
end;

function TACBrTEFD.GetGPExeName : String;
begin
  if fTefClass is TACBrTEFDClassTXT then
     Result := TACBrTEFDClassTXT(fTefClass).GPExeName
  else
     Result := '' ;
end;

function TACBrTEFD.GetReq : TACBrTEFDReq;
begin
   Result := fTefClass.Req;
end;

function TACBrTEFD.GetResp : TACBrTEFDResp;
begin
   Result := fTefClass.Resp;
end;

procedure TACBrTEFD.SetAutoAtivarGP(AValue: Boolean);
var
   I : Integer;
begin
  For I := 0 to fTEFList.Count-1 do
  begin
    if fTEFList[I] is TACBrTEFDClass then
    begin
       with TACBrTEFDClass( fTEFList[I] ) do
         AutoAtivarGP := AValue;
    end;
  end;

  fAutoAtivarGP := AValue;
end;

procedure TACBrTEFD.SetPathBackup(const AValue : String);
begin
  if fPathBackup = AValue then exit ;

  if Inicializado then
     raise EACBrTEFDErro.Create(ACBrStr('PathBackup não pode ser modificado com o ACBrTEFD Inicializado'));

  fPathBackup := Trim(AValue) ;

  if RightStr(fPathBackup,1) = PathDelim then   { Remove ultimo PathDelim }
     fPathBackup := copy( fPathBackup,1,Length(fPathBackup)-1 ) ;
end;

procedure TACBrTEFD.BloquearMouseTeclado( Bloqueia : Boolean );
var
   Tratado : Boolean;
begin
  Tratado := False ;
  fTecladoBloqueado := Bloqueia ;

  fTefClass.GravaLog( 'BloquearMouseTeclado: '+ IfThen( Bloqueia, 'SIM', 'NAO'));

  if Assigned( fOnBloqueiaMouseTeclado ) then
     fOnBloqueiaMouseTeclado( Bloqueia, Tratado ) ;

  if not Bloqueia then
     LimparTeclado;

  if not Tratado then
  begin
   {$IFNDEF Framework}
   {$IFDEF MSWINDOWS}
     LoadBlockInput;
     if Assigned( xBlockInput ) then
        xBlockInput( Bloqueia ) ;
   {$ENDIF}
   {$ENDIF}
  end;
end;

 procedure TACBrTEFD.LimparTeclado;
 Var
   Tratado : Boolean ;
   {$IFNDEF NOGUI}
     {$IFDEF MSWINDOWS}
     Msg: TMsg;
     {$ENDIF}
   {$ENDIF}
 begin
   Tratado := False ;

   if Assigned( fOnLimpaTeclado ) then
      fOnLimpaTeclado( Tratado ) ;

   {$IFNDEF NOGUI}
   {$IFDEF MSWINDOWS}
    if not Tratado then
    begin
      try
         // Remove todas as Teclas do Buffer do Teclado //
         while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE or PM_NOYIELD) do;{%h-}
      except
      end
    end;
   {$ENDIF}
   {$ENDIF};
 end;

 procedure TACBrTEFD.RestaurarFocoAplicacao ;
 var
    Tratado : Boolean;
 begin
   Tratado := False ;
   if Assigned( fOnRestauraFocoAplicacao ) then
      fOnRestauraFocoAplicacao( Tratado ) ;

   {$IFNDEF NOGUI}
   if not Tratado then
   begin
      {$IFNDEF FMX}
        Application.BringToFront ;
      {$ELSE}
        if Assigned(Application.MainForm) then
          Application.MainForm.BringToFront ;
      {$ENDIF}

      {$IFDEF MSWINDOWS}
        {$IFNDEF FMX}
           if Assigned( Screen.ActiveForm ) then
           begin
             {$IFDEF VisualCLX}
                QWidget_setActiveWindow( Screen.ActiveForm.Handle );
             {$ELSE}
                SetForeGroundWindow( Screen.ActiveForm.Handle );
             {$ENDIF}
           end;
        {$ELSE}
          if Assigned(Application.MainForm) then
            SetForegroundWindow(FormToHWND(Application.MainForm));
        {$ENDIF}
      {$ENDIF}
   end;
  {$ENDIF}
 end;

{$ifdef FPC}
{$IFNDEF NOGUI}
initialization
   {$I ACBrTEFD.lrs}
{$endif}
{$endif}

end.

