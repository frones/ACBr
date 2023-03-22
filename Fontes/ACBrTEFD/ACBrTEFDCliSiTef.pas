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

unit ACBrTEFDCliSiTef;

interface

uses
  Classes, SysUtils,
  {$IfNDef NOGUI}
    {$If DEFINED(VisualCLX)}
      QControls,
    {$ElseIf DEFINED(FMX)}
      System.UITypes,
    {$ElseIf DEFINED(DELPHICOMPILER16_UP)}
      System.UITypes,
    {$Else}
      Controls,
    {$IfEnd}
  {$EndIf}
  ACBrTEFDClass, ACBrTEFCliSiTefComum;


type
  { TACBrTEFDRespCliSiTef }

  TACBrTEFDRespCliSiTef = class( TACBrTEFDResp )
  public
    procedure ConteudoToProperty; override;
    procedure GravaInformacao( const Identificacao : Integer;
      const Informacao : AnsiString );
  end;

  TACBrTEFDCliSiTefExibeMenu = procedure( Titulo : String; Opcoes : TStringList;
    var ItemSelecionado : Integer; var VoltarMenu : Boolean ) of object ;

  TACBrTEFDCliSiTefOperacaoCampo = (tcString, tcDouble, tcCMC7, tcBarCode, tcStringMask) ;

  TACBrTEFDCliSiTefObtemCampo = procedure( Titulo : String;
    TamanhoMinimo, TamanhoMaximo : Integer ;
    TipoCampo : Integer; Operacao : TACBrTEFDCliSiTefOperacaoCampo;
    var Resposta : AnsiString; var Digitado : Boolean; var VoltarMenu : Boolean )
    of object ;

  { TACBrTEFDCliSiTef }

   TACBrTEFDCliSiTef = class( TACBrTEFDClass )
   private
      fPinPadChaveAcesso: AnsiString;
      fPinPadIdentificador: AnsiString;
      fSiTefAPI: TACBrTEFCliSiTefAPI;
      fExibirErroRetorno: Boolean;
      fIniciouRequisicao: Boolean;
      fReimpressao: Boolean; {Indica se foi selecionado uma reimpressão no ADM}
      fCancelamento: Boolean; {Indica se foi selecionado Cancelamento no ADM}
      fCodigoLoja : AnsiString;
      fEnderecoIP : AnsiString;
      fNumeroTerminal : AnsiString;
      fCNPJEstabelecimento : AnsiString;
      fCNPJSoftwareHouse : AnsiString;
      fOnExibeMenu : TACBrTEFDCliSiTefExibeMenu;
      fOnObtemCampo : TACBrTEFDCliSiTefObtemCampo;
      fOperacaoADM : Integer;
      fOperacaoATV : Integer;
      fOperacaoCHQ : Integer;
      fOperacaoCNC : Integer;
      fOperacaoCRT : Integer;
      fOperacaoPRE : Integer;
      fOperacaoReImpressao: Integer;
      fOperador : AnsiString;
      fParametrosAdicionais : TStringList;
      fRespostas: TStringList;
      fRestricoes : AnsiString;
      fDocumentoFiscal: AnsiString;
      fDataHoraFiscal: TDateTime;
      fDocumentosProcessados : AnsiString ;
      fPortaPinPad: Integer;
      fUsaUTF8: Boolean;
      fArqBackUp: String;
      fUltimoSTS: Integer;

     procedure AvaliaErro(Sts : Integer);
     function GetDataHoraFiscal: TDateTime;
     function GetDocumentoFiscal: AnsiString;
     function GetPathDLL: string;
     procedure SetPathDLL(AValue: string);
     procedure SetParametrosAdicionais(const AValue : TStringList) ;
   protected
     procedure SetNumVias(const AValue : Integer); override;

     Function FazerRequisicao( Funcao : Integer; AHeader : AnsiString = '';
        Valor : Double = 0; Documento : AnsiString = '';
        ListaRestricoes : AnsiString = '') : Integer ;
     Function ContinuarRequisicao( ImprimirComprovantes : Boolean ) : Integer ;

     procedure ProcessarResposta ; override;
     Function ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean; override;

     procedure VerificarIniciouRequisicao; override;

     Function SuportaDesconto : Boolean ;
     Function HMS : String ;

     function CopiarResposta: string; override;
   public
     property Respostas : TStringList read fRespostas ;
     property PathDLL: string read GetPathDLL write SetPathDLL;
     property PinPadChaveAcesso: AnsiString read fPinPadChaveAcesso write fPinPadChaveAcesso;
     property PinPadIdentificador: AnsiString read fPinPadIdentificador write fPinPadIdentificador;

     property UltimoSTS : Integer read fUltimoSTS;

     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     procedure Inicializar ; override;
     procedure DesInicializar ; override;

     procedure AtivarGP ; override;
     procedure VerificaAtivo ; override;

     Procedure ATV ; override;
     Function ADM : Boolean ; override;
     Function CRT( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; override;
     Function CHQ( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; CMC7 : String = '';
        TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
        DataCheque : TDateTime = 0; Banco   : String = '';
        Agencia    : String = ''; AgenciaDC : String = '';
        Conta      : String = ''; ContaDC   : String = '';
        Cheque     : String = ''; ChequeDC  : String = '';
        Compensacao: String = '' ) : Boolean ; override;
     Procedure NCN(Rede, NSU, Finalizacao : String;
        Valor : Double = 0; DocumentoVinculado : String = ''); override;
     Procedure CNF(Rede, NSU, Finalizacao : String;
        DocumentoVinculado : String = ''); override;
     Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
        Valor : Double; CodigoAutorizacaoTransacao: String = '') : Boolean; overload; override;
     Function PRE(Valor : Double; DocumentoVinculado : String = '';
        Moeda : Integer = 0) : Boolean; override;
     function CDP(const EntidadeCliente: string; out Resposta: string): Boolean; override;

     procedure ExibirMensagemPinPad(const MsgPinPad: String); override;

     procedure FinalizarTransacao( Confirma : Boolean;
        DocumentoVinculado : AnsiString; ParamAdic: AnsiString = '');

     Function DefineMensagemPermanentePinPad(Mensagem:AnsiString):Integer;
     Function ObtemQuantidadeTransacoesPendentes(Data:TDateTime;
        CupomFiscal:AnsiString):Integer;
     function EnviaRecebeSiTefDireto(RedeDestino: SmallInt; FuncaoSiTef: SmallInt;
       OffsetCartao: SmallInt; DadosTx: AnsiString; var DadosRx: AnsiString;
       var CodigoResposta: SmallInt; TempoEsperaRx: SmallInt;
       CupomFiscal: AnsiString; Confirmar: Boolean; Operador: AnsiString): Integer;
     function ValidaCampoCodigoEmBarras(Dados: AnsiString;
        var Tipo: SmallInt): Integer;
     function VerificaPresencaPinPad: Boolean;
     function ObtemDadoPinPadDiretoEx(TipoDocumento: Integer; ChaveAcesso,
       Identificador: AnsiString): AnsiString;

   published
     property EnderecoIP: AnsiString                    read fEnderecoIP           write fEnderecoIP;
     property CodigoLoja: AnsiString                    read fCodigoLoja           write fCodigoLoja;
     property NumeroTerminal: AnsiString                read fNumeroTerminal       write fNumeroTerminal;
     property CNPJEstabelecimento: AnsiString           read fCNPJEstabelecimento  write fCNPJEstabelecimento;
     property CNPJSoftwareHouse: AnsiString             read fCNPJSoftwareHouse    write fCNPJSoftwareHouse;
     property Operador: AnsiString                      read fOperador             write fOperador;
     property PortaPinPad: Integer                      read fPortaPinPad          write fPortaPinPad default 0;
     property ParametrosAdicionais: TStringList         read fParametrosAdicionais write SetParametrosAdicionais;
     property Restricoes: AnsiString                    read fRestricoes           write fRestricoes;
     property DocumentoFiscal: AnsiString               read GetDocumentoFiscal    write fDocumentoFiscal;
     property DataHoraFiscal: TDateTime                 read GetDataHoraFiscal     write fDataHoraFiscal;
     property OperacaoATV: Integer                      read fOperacaoATV          write fOperacaoATV default 111;
     property OperacaoADM: Integer                      read fOperacaoADM          write fOperacaoADM default 110;
     property OperacaoCRT: Integer                      read fOperacaoCRT          write fOperacaoCRT default 0;
     property OperacaoCHQ: Integer                      read fOperacaoCHQ          write fOperacaoCHQ default 1;
     property OperacaoCNC: Integer                      read fOperacaoCNC          write fOperacaoCNC default 200;
     property OperacaoPRE: Integer                      read fOperacaoPRE          write fOperacaoPRE default 115;
     property OperacaoReImpressao: Integer              read fOperacaoReImpressao  write fOperacaoReImpressao default 112;
     property OnExibeMenu: TACBrTEFDCliSiTefExibeMenu   read fOnExibeMenu          write fOnExibeMenu;
     property OnObtemCampo: TACBrTEFDCliSiTefObtemCampo read fOnObtemCampo         write fOnObtemCampo;
     property ExibirErroRetorno: Boolean                read fExibirErroRetorno    write fExibirErroRetorno default False;
     property UsaUTF8: Boolean                          read fUsaUTF8              write fUsaUTF8 default False;
   end;

implementation

Uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  StrUtils, Math, ACBrTEFD, ACBrTEFComum, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.Math;

{ TACBrTEFDRespCliSiTef }

procedure TACBrTEFDRespCliSiTef.ConteudoToProperty;
begin
  ConteudoToPropertyCliSiTef( Self );
end;

procedure TACBrTEFDRespCliSiTef.GravaInformacao(const Identificacao: Integer;
  const Informacao: AnsiString);
var
  Sequencia: Integer;
begin
  Sequencia := 0;

  case Identificacao of
    141, 142,            // 141 - Data Parcela, 142 - Valor Parcela
    600..607, 611..624:  // Dados do Corresp. Bancário
    begin
      Sequencia := 1;

      while (Trim(LeInformacao(Identificacao, Sequencia).AsString) <> '') do
        Inc(Sequencia);
    end;
  end;

  fpConteudo.GravaInformacao( Identificacao, Sequencia,
                              BinaryStringToString(Informacao) ); // Converte #10 para "\x0A"
end;


{ TACBrTEFDClass }

constructor TACBrTEFDCliSiTef.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  fSiTefAPI := TACBrTEFCliSiTefAPI.Create;

  fIniciouRequisicao := False;
  fReimpressao       := False;
  fCancelamento      := False;
  ArqReq    := '' ;
  ArqResp   := '' ;
  ArqSTS    := '' ;
  ArqTemp   := '' ;
  GPExeName := '' ;
  fpTipo    := gpCliSiTef;
  Name      := 'CliSiTef' ;

  fEnderecoIP := '' ;
  fCodigoLoja := '' ;
  fNumeroTerminal := '' ;
  fCNPJEstabelecimento := '';
  fCNPJSoftwareHouse := '';
  fOperador := '' ;
  fRestricoes := '' ;
  fPinPadChaveAcesso := '';
  fPinPadIdentificador := '';

  fDocumentoFiscal := '';
  fDataHoraFiscal  := 0;

  fOperacaoATV         := 111 ; // 111 - Teste de comunicação com o SiTef
  fOperacaoReImpressao := 112 ; // 112 - Menu Re-impressão
  fOperacaoPRE         := 115 ; // 115 - Pré-autorização
  fOperacaoADM         := 110 ; // 110 - Abre o leque das transações Gerenciais
  fOperacaoCRT         := 0 ;   // A CliSiTef permite que o operador escolha a forma
                                // de pagamento através de menus
  fOperacaoCHQ         := 1 ;   // Cheque
  fOperacaoCNC         := 200 ; // 200 Cancelamento Normal: Inicia a coleta dos dados
                                // no ponto necessário para fazer o cancelamento de uma
                                // transação de débito ou crédito, sem ser necessário
                                // passar antes pelo menu de transações administrativas
  fDocumentosProcessados := '' ;
  fExibirErroRetorno     := False;
  fUsaUTF8               := False;

  fParametrosAdicionais := TStringList.Create;
  fRespostas            := TStringList.Create;

  fOnExibeMenu  := nil ;
  fOnObtemCampo := nil ;

  if Assigned( fpResp ) then
     fpResp.Free ;

  fpResp := TACBrTEFDRespCliSiTef.Create;
  fpResp.TipoGP := fpTipo;
end;

destructor TACBrTEFDCliSiTef.Destroy;
begin
   fSiTefAPI.Free;
   fParametrosAdicionais.Free ;
   fRespostas.Free ;

   inherited Destroy;
end;

procedure TACBrTEFDCliSiTef.SetParametrosAdicionais(const AValue : TStringList
   ) ;
begin
   fParametrosAdicionais.Assign( AValue ) ;
end ;

procedure TACBrTEFDCliSiTef.SetPathDLL(AValue: string);
begin
  fSiTefAPI.PathDLL := AValue;
end;

procedure TACBrTEFDCliSiTef.SetNumVias(const AValue : Integer);
begin
   fpNumVias := AValue;
end;

procedure TACBrTEFDCliSiTef.Inicializar;
Var
  Sts : Integer ;
  ParamAdic : AnsiString ;
  Erro : String;
begin
  if Inicializado then exit ;

  if not Assigned( OnExibeMenu ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnExibeMenu" não programado' ) ) ;

  if not Assigned( OnObtemCampo ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnObtemCampo" não programado' ) ) ;

  fSiTefAPI.Inicializada := True;

  // configuração da porta do pin-pad
  if PortaPinPad > 0 then
  begin
    if ParametrosAdicionais.Values['PortaPinPad'] = '' then
      ParametrosAdicionais.Add('PortaPinPad=' + IntToStr(PortaPinPad))
    else
      ParametrosAdicionais.Values['PortaPinPad'] := IntToStr(PortaPinPad);
  end;

  // cielo premia
  if ParametrosAdicionais.Values['VersaoAutomacaoCielo'] = '' then
  begin
    if SuportaDesconto then
      ParametrosAdicionais.Add('VersaoAutomacaoCielo=' + PadRight( TACBrTEFD(Owner).Identificacao.SoftwareHouse, 8 ) + '10');
  end;

  // acertar quebras de linhas e abertura e fechamento da lista de parametros
  ParamAdic := StringReplace(Trim(ParametrosAdicionais.Text), sLineBreak, ';', [rfReplaceAll]);

  if not(ParamAdic = EmptyStr) then
    ParamAdic := '['+ ParamAdic + ']';

  if NaoEstaVazio(CNPJEstabelecimento) and NaoEstaVazio(CNPJSoftwareHouse) then
    if (ParamAdic <> '') then
      ParamAdic := ParamAdic + ';[ParmsClient=1='+CNPJEstabelecimento+';2='+CNPJSoftwareHouse+']'
    else
      ParamAdic := '[ParmsClient=1='+CNPJEstabelecimento+';2='+CNPJSoftwareHouse+']';

  GravaLog( '*** ConfiguraIntSiTefInterativoEx. EnderecoIP: '   +fEnderecoIP+
                                            ' CodigoLoja: '     +fCodigoLoja+
                                            ' NumeroTerminal: ' +fNumeroTerminal+
                                            ' Resultado: 0'     +
                                            ' ParametrosAdicionais: '+ParamAdic ) ;

  Sts := fSiTefAPI.ConfiguraIntSiTefInterativo( PAnsiChar(fEnderecoIP),
                                                PAnsiChar(fCodigoLoja),
                                                PAnsiChar(fNumeroTerminal),
                                                0,
                                                PAnsiChar(ParamAdic) );
  Erro := fSiTefAPI.TraduzirErroInicializacao(Sts);
  if Erro <> '' then
     raise EACBrTEFDErro.Create( ACBrStr( Erro ) ) ;

  GravaLog( Name +' Inicializado CliSiTEF' );

  VerificarTransacoesPendentesClass(True);
  fpInicializado := True;
end;

procedure TACBrTEFDCliSiTef.DesInicializar;
begin
  fSiTefAPI.Inicializada := False; ;
  inherited DesInicializar;
end;

procedure TACBrTEFDCliSiTef.AtivarGP;
begin
   raise EACBrTEFDErro.Create( ACBrStr( 'CliSiTef não pode ser ativado localmente' )) ;
end;

procedure TACBrTEFDCliSiTef.VerificaAtivo;
begin
   {Nada a Fazer}
end;

procedure TACBrTEFDCliSiTef.ATV;
var
   Sts : Integer;
begin
  Sts := FazerRequisicao( fOperacaoATV, 'ATV' ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFCliSiTef_ImprimeGerencialConcomitante ) ;

  if ( Sts <> 0 ) then
     AvaliaErro( Sts )
  else
     if not CACBrTEFCliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

function TACBrTEFDCliSiTef.ADM: Boolean;
var
   Sts : Integer;
begin
  Sts := FazerRequisicao( fOperacaoADM, 'ADM', 0, '', fRestricoes ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFCliSiTef_ImprimeGerencialConcomitante ) ;

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     if not CACBrTEFCliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

function TACBrTEFDCliSiTef.CRT(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
var
  Sts : Integer;
  Restr : AnsiString ;
begin
  if (Valor <> 0) then
    VerificarTransacaoPagamento( Valor );

  Restr := fRestricoes;
  if Restr = '' then
     Restr := '[10]' ;     // 10 - Cheques

  if DocumentoVinculado = '' then
     DocumentoVinculado := fDocumentoFiscal;

  Sts := FazerRequisicao( fOperacaoCRT, 'CRT', Valor, DocumentoVinculado, Restr ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( False ) ;  { False = NAO Imprimir Comprovantes agora }

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );
end;

function TACBrTEFDCliSiTef.CHQ(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; CMC7: String; TipoPessoa: AnsiChar;
  DocumentoPessoa: String; DataCheque: TDateTime; Banco: String;
  Agencia: String; AgenciaDC: String; Conta: String; ContaDC: String;
  Cheque: String; ChequeDC: String; Compensacao: String): Boolean;
var
  Sts : Integer;
  Restr : AnsiString ;

  Function FormataCampo( Campo : AnsiString; Tamanho : Integer ) : AnsiString ;
  begin
    Result := PadLeft( OnlyNumber( Trim( Campo ) ), Tamanho, '0') ;
  end ;

begin
  if (DocumentoVinculado <> '') and (Valor <> 0) then
     VerificarTransacaoPagamento( Valor );

  Respostas.Values['501'] := ifthen(TipoPessoa = 'J','1','0');

  if DocumentoPessoa <> '' then
     Respostas.Values['502'] := OnlyNumber(Trim(DocumentoPessoa));

  if DataCheque <> 0  then
     Respostas.Values['506'] := FormatDateTime('DDMMYYYY',DataCheque) ;

  if CMC7 <> '' then
     Respostas.Values['517'] := '1:'+CMC7
  else
     Respostas.Values['517'] := '0:'+FormataCampo(Compensacao,3)+
                                     FormataCampo(Banco,3)+
                                     FormataCampo(Agencia,4)+
                                     FormataCampo(AgenciaDC,1)+
                                     FormataCampo(Conta,10)+
                                     FormataCampo(ContaDC,1)+
                                     FormataCampo(Cheque,6)+
                                     FormataCampo(ChequeDC,1) ;

  Restr := fRestricoes;
  if Restr = '' then
     Restr := '[15;25]' ;  // 15 - Cartão Credito; 25 - Cartao Debito

  if DocumentoVinculado = '' then
     DocumentoVinculado := fDocumentoFiscal;

  Sts := FazerRequisicao( fOperacaoCHQ, 'CHQ', Valor, DocumentoVinculado, Restr ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( False ) ;  { False = NAO Imprimir Comprovantes agora }

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );
end;

procedure TACBrTEFDCliSiTef.CNF(Rede, NSU, Finalizacao: String;
  DocumentoVinculado: String);
begin
  // CliSiTEF não usa Rede, NSU e Finalizacao

  FinalizarTransacao( True, DocumentoVinculado, Finalizacao );
end;

function TACBrTEFDCliSiTef.CNC(Rede, NSU: String; DataHoraTransacao: TDateTime;
  Valor: Double; CodigoAutorizacaoTransacao: String): Boolean;
var
   Restr : AnsiString;
   Sts : Integer;
begin
  Respostas.Values['146'] := FormatFloat('0.00',Valor);
  Respostas.Values['147'] := FormatFloat('0.00',Valor);
  Respostas.Values['515'] := FormatDateTime('DDMMYYYY',DataHoraTransacao) ;
  Respostas.Values['516'] := NSU ;

  Restr := fRestricoes;
  if Restr = '' then
     Restr := '[10]';

  Sts := FazerRequisicao( fOperacaoCNC, 'CNC', Valor, '', Restr);

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFCliSiTef_ImprimeGerencialConcomitante ) ;

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     if not CACBrTEFCliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

function TACBrTEFDCliSiTef.PRE(Valor: Double; DocumentoVinculado: String;
  Moeda: Integer): Boolean;
var
   Sts : Integer;
begin
  Sts := FazerRequisicao( fOperacaoPRE, 'PRE', Valor, '', fRestricoes ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFCliSiTef_ImprimeGerencialConcomitante ) ;

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     if not CACBrTEFCliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

procedure TACBrTEFDCliSiTef.NCN(Rede, NSU, Finalizacao: String; Valor: Double;
  DocumentoVinculado: String);
begin
  // CliSiTEF não usa Rede, NSU, Finalizacao e Valor
  FinalizarTransacao( False, DocumentoVinculado, Finalizacao );
end;

function TACBrTEFDCliSiTef.FazerRequisicao(Funcao: Integer;
  AHeader: AnsiString; Valor: Double; Documento: AnsiString;
  ListaRestricoes: AnsiString): Integer;
Var
  ValorStr, DataStr, HoraStr : AnsiString;
  DataHora : TDateTime ;
begin
   if not fSiTefAPI.Inicializada then
      raise EACBrTEFDErro.Create(ACBrStr(CACBrTEFCliSiTef_NaoInicializado));

   if Documento = '' then
      Documento := DocumentoFiscal;

   Req.DocumentoVinculado  := Documento;
   Req.ValorTotal          := Valor;

   if fpAguardandoResposta then
      raise EACBrTEFDErro.Create( ACBrStr( CACBrTEFCliSiTef_NaoConcluido ) ) ;

   if (pos('{TipoTratamento=4}',ListaRestricoes) = 0) and
      (pos(AHeader,'CRT,CHQ') > 0 ) and
      SuportaDesconto then
   begin
      ListaRestricoes := ListaRestricoes + '{TipoTratamento=4}';
   end;

   fIniciouRequisicao := True;

   DataHora := DataHoraFiscal ;
   DataStr  := FormatDateTime('YYYYMMDD', DataHora );
   HoraStr  := FormatDateTime('HHNNSS', DataHora );
   ValorStr := FormatFloatBr( Valor );
   fDocumentosProcessados := '' ;

   GravaLog( '*** IniciaFuncaoSiTefInterativo. Modalidade: '+IntToStr(Funcao)+
                                             ' Valor: '     +ValorStr+
                                             ' Documento: ' +Documento+
                                             ' Data: '      +DataStr+
                                             ' Hora: '      +HoraStr+
                                             ' Operador: '  +fOperador+
                                             ' Restricoes: '+ListaRestricoes ) ;

   Result := fSiTefAPI.IniciaFuncaoSiTefInterativo( Funcao,
                                                    PAnsiChar( ValorStr ),
                                                    PAnsiChar( Documento ),
                                                    PAnsiChar( DataStr ),
                                                    PAnsiChar( HoraStr ),
                                                    PAnsiChar( fOperador ),
                                                    PAnsiChar( ListaRestricoes ) ) ;

   { Adiciona Campos já conhecidos em Resp, para processa-los em
     métodos que manipulam "RespostasPendentes" (usa códigos do G.P.)  }
   Resp.Clear;

   with TACBrTEFDRespCliSiTef( Resp ) do
   begin
     fpIDSeq := fpIDSeq + 1 ;
     if Documento = '' then
        Documento := IntToStr(fpIDSeq) ;

     Conteudo.GravaInformacao(899,100, AHeader ) ;
     Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
     Conteudo.GravaInformacao(899,102, Documento ) ;
     Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );

     Resp.TipoGP := fpTipo;
   end;
end;

function TACBrTEFDCliSiTef.ContinuarRequisicao(ImprimirComprovantes: Boolean
  ): Integer;
var
  Continua, ItemSelecionado, I: Integer;
  ProximoComando,TamanhoMinimo, TamanhoMaximo : SmallInt;
  TipoCampo: LongInt;
  Buffer: array [0..20000] of AnsiChar;
  Mensagem, MensagemOperador, MensagemCliente, CaptionMenu : String ;
  Resposta : AnsiString;
  SL : TStringList ;
  Interromper, Digitado, GerencialAberto, FechaGerencialAberto, ImpressaoOk,
     HouveImpressao, Voltar, FinalizarTransacaoInterrompida, EhCarteiraDigital: Boolean ;
  Est : AnsiChar;

  function ProcessaMensagemTela( AMensagem : String ) : String ;
  begin
    Result := StringReplace( AMensagem, '@', sLineBreak, [rfReplaceAll] );
    Result := StringReplace( Result, '/n', sLineBreak, [rfReplaceAll] );
  end;

begin
   ProximoComando    := 0;
   TipoCampo         := 0;
   TamanhoMinimo     := 0;
   TamanhoMaximo     := 0;
   Continua          := 0;
   Mensagem          := '' ;
   MensagemOperador  := '' ;
   MensagemCliente   := '' ;
   CaptionMenu       := '' ;
   GerencialAberto   := False ;
   ImpressaoOk       := True ;
   HouveImpressao    := False ;
   fIniciouRequisicao:= True ;
   fCancelamento     := False ;
   fReimpressao      := False;
   Interromper       := False;
   EhCarteiraDigital := False;
   fArqBackUp        := '' ;
   Resposta          := '' ;

   FinalizarTransacaoInterrompida := False;
   fpAguardandoResposta := True ;
   FechaGerencialAberto := True ;

   with TACBrTEFD(Owner) do
   begin
      try
         BloquearMouseTeclado( True );

         repeat
            GravaLog( 'ContinuaFuncaoSiTefInterativo, Chamando: Continua = '+
                      IntToStr(Continua)+' Buffer = '+Resposta ) ;

            Result := fSiTefAPI.ContinuaFuncaoSiTefInterativo( ProximoComando,
                                                               TipoCampo,
                                                               TamanhoMinimo,
                                                               TamanhoMaximo,
                                                               Buffer, sizeof(Buffer),
                                                               Continua );
            Continua := 0;
            Mensagem := TrimRight( Buffer ) ;
            Resposta := '' ;
            Voltar   := False;
            Digitado := True ;

            if fUsaUTF8 then
              Mensagem := ACBrStrToAnsi(Mensagem);  // Será convertido para UTF8 em TACBrTEFD.DoExibeMsg

            GravaLog( 'ContinuaFuncaoSiTefInterativo, Retornos: STS = '+IntToStr(Result)+
                      ' ProximoComando = '+IntToStr(ProximoComando)+
                      ' TipoCampo = '+IntToStr(TipoCampo)+
                      ' Buffer = '+Mensagem +
                      ' Tam.Min = '+IntToStr(TamanhoMinimo) +
                      ' Tam.Max = '+IntToStr(TamanhoMaximo)) ;

            if Result = 10000 then
            begin
              if TipoCampo > 0 then
                 Resposta := fRespostas.Values[IntToStr(TipoCampo)];

              case ProximoComando of
                 0 :
                   begin
                     TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, Mensagem ) ;

                     case TipoCampo of
                        15 : TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, 'True') ;//Selecionou Debito;
                        25 : TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, 'True') ;//Selecionou Credito;
                        29 : TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, 'True') ;//Cartão Digitado;
                        {Indica que foi escolhido menu de reimpressão}
                        56,57,58 : fReimpressao := True;
                        107      : EhCarteiraDigital := True;
                        110      : fCancelamento:= True;
                        121, 122 :
                          if ImprimirComprovantes then
                          begin
                             { Impressão de Gerencial, deve ser Sob demanda }
                             if not HouveImpressao then
                             begin
                                HouveImpressao := True ;
                                fArqBackUp := CopiarResposta;
                             end;

                             SL := TStringList.Create;
                             ImpressaoOk := False ;
                             I := TipoCampo ;
                             try
                                while not ImpressaoOk do
                                begin
                                  try
                                    while I <= TipoCampo do
                                    begin
                                       if FechaGerencialAberto then
                                       begin
                                         Est := EstadoECF;

                                         { Fecha Vinculado ou Gerencial ou Cupom, se ficou algum aberto por Desligamento }
                                         case Est of
                                           'C'         : ComandarECF( opeFechaVinculado );
                                           'G','R'     : ComandarECF( opeFechaGerencial );
                                           'V','P','N' : ComandarECF( opeCancelaCupom );
                                         end;

                                         GerencialAberto      := False ;
                                         FechaGerencialAberto := False ;

                                         if EstadoECF <> 'L' then
                                           raise EACBrTEFDECF.Create( ACBrStr(CACBrTEFD_Erro_ECFNaoLivre) ) ;
                                       end;

                                       Mensagem := Self.Resp.LeInformacao(I).AsString ;
                                       Mensagem := StringToBinaryString( Mensagem ) ;
                                       if Mensagem <> '' then
                                       begin
                                          SL.Text  := StringReplace( Mensagem, #10, sLineBreak, [rfReplaceAll] ) ;

                                          if not GerencialAberto then
                                           begin
                                             ComandarECF( opeAbreGerencial ) ;
                                             GerencialAberto := True ;
                                           end
                                          else
                                           begin
                                             ComandarECF( opePulaLinhas ) ;
                                             DoExibeMsg( opmDestaqueVia,
                                                Format(CACBrTEFD_DestaqueVia, [1]) ) ;
                                           end;

                                          ECFImprimeVia( trGerencial, I-120, SL );

                                          ImpressaoOk := True ;
                                       end;

                                       Inc( I ) ;
                                    end;

                                    if (TipoCampo = 122) and GerencialAberto then
                                    begin
                                       ComandarECF( opeFechaGerencial );
                                       GerencialAberto := False;
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

                                    I := 121 ;
                                    FechaGerencialAberto := True ;
                                  end;
                                end ;
                             finally
                                if not ImpressaoOk then
                                   Continua := -1 ;

                                SL.Free;
                             end;
                          end ;
                        1, 133, 952:
                        begin
                          fArqBackUp := CopiarResposta;
                        end;
                     end;
                   end;

                 1 :
                   begin
                     MensagemOperador := ProcessaMensagemTela( Mensagem );
                     DoExibeMsg( opmExibirMsgOperador, MensagemOperador, (TipoCampo=5005) ) ;
                   end ;

                 2 :
                   begin
                     MensagemCliente := ProcessaMensagemTela( Mensagem );
                     DoExibeMsg( opmExibirMsgCliente, MensagemCliente, (TipoCampo=5005) ) ;
                   end;

                 3 :
                   begin
                     MensagemOperador := ProcessaMensagemTela( Mensagem );
                     MensagemCliente  := MensagemOperador;
                     DoExibeMsg( opmExibirMsgOperador, MensagemOperador, (TipoCampo=5005) ) ;
                     DoExibeMsg( opmExibirMsgCliente, MensagemCliente, (TipoCampo=5005) ) ;

                     if EhCarteiraDigital then
                     begin
                       Interromper := False;
                       OnAguardaResp('52', 0, Interromper);
                       if Interromper then
                       begin
                         Continua := -1 ;
                         FinalizarTransacaoInterrompida := True;
                       end;
                     end;
                   end ;

                 4 : CaptionMenu := ACBrStr( Mensagem ) ;

                 11 :
                   begin
                     MensagemOperador := '' ;
                     DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                   end;

                 12 :
                   begin
                     MensagemCliente := '' ;
                     DoExibeMsg( opmRemoverMsgCliente, '' ) ;
                   end;

                 13 :
                   begin
                     DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                     DoExibeMsg( opmRemoverMsgCliente, '' ) ;
                   end ;

                 14 : CaptionMenu := '' ;
                   {Deve limpar o texto utilizado como cabeçalho na apresentação do menu}

                 20 :
                   begin
                     if Mensagem = '' then
                        Mensagem := 'CONFIRMA ?';

                     Resposta := ifThen( (DoExibeMsg( opmYesNo, Mensagem ) = mrYes), '0', '1' ) ;

                     // se a resposta a mensagem for não, não deixar interromper
                     // voltar ao loop
                     if (TipoCampo = 5013) and (Resposta = '1') then
                       Interromper := False;
                   end ;

                 21 :
                   begin
                     SL := TStringList.Create;
                     try
                        ItemSelecionado := -1 ;
                        SL.Text := StringReplace( Mensagem, ';',
                                                         sLineBreak, [rfReplaceAll] ) ;
                        BloquearMouseTeclado(False);
                        OnExibeMenu( CaptionMenu, SL, ItemSelecionado, Voltar ) ;
                        BloquearMouseTeclado(True);

                        if (not Voltar) then
                        begin
                           if (ItemSelecionado >= 0) and
                              (ItemSelecionado < SL.Count) then
                              Resposta := copy( SL[ItemSelecionado], 1,
                                             pos(':',SL[ItemSelecionado])-1 )
                           else
                              Digitado := False ;
                        end;
                     finally
                        SL.Free ;
                     end ;
                   end;

                 22 :
                   begin
                     if Mensagem = '' then
                        Mensagem := CACBrTEFCliSiTef_PressioneEnter;

                     DoExibeMsg( opmOK, Mensagem );
                   end ;

                 23 :
                   begin
                     Interromper := False ;
                     OnAguardaResp( '23', 0, Interromper ) ;
                     if Interromper then
                        Continua := -1 ;
                   end;

                 30 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcString, Resposta, Digitado, Voltar) ;
                     {Se o tipo campo for 505 é a quantidade de parcelas}
                     TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, Resposta ) ;
                     BloquearMouseTeclado(True);
                   end;

                 31 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcCMC7, Resposta, Digitado, Voltar ) ;
                     BloquearMouseTeclado(True);
                   end;

                 34 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcDouble, Resposta, Digitado, Voltar ) ;
                     BloquearMouseTeclado(True);

                     // Garantindo que a Resposta é Float //
                     Resposta := FormatFloatBr( StringToFloatDef(Resposta, 0));
                   end;

                 35 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcBarCode, Resposta, Digitado, Voltar ) ;
                     BloquearMouseTeclado(True);
                   end;

                 41 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcStringMask, Resposta, Digitado, Voltar ) ;
                     BloquearMouseTeclado(True);
                   end;

                 50 :  // Exibir QRCode
                   begin
                     DoExibeQRCode( ProcessaMensagemTela( Mensagem ) );
                   end;

                 51 :   // Remover QRCode
                   begin
                     DoExibeQRCode( '' ) ;
                   end;

                 52 :   // Mensagem de rodapé QRCode
                   begin
                     Interromper := False;
                     OnAguardaResp('52', 0, Interromper);
                     if Interromper then
                     begin
                       Continua := -1 ;
                       FinalizarTransacaoInterrompida := True;
                     end;
                   end;
              end;
            end
            else
               GravaLog( '*** ContinuaFuncaoSiTefInterativo, Finalizando: STS = '+IntToStr(Result) ) ;

            if Voltar then
               Continua := 1     { Volta para o menu anterior }
            else if (not Digitado) or Interromper then
               Continua := -1 ;  { Cancela operacao }

            if (Voltar and (Result = 10000)) or (not Digitado) then
            begin
              DoExibeMsg( opmRemoverMsgOperador, '' ) ;
              DoExibeMsg( opmRemoverMsgCliente, '' ) ;
            end ;

            StrPCopy(Buffer, Resposta);

         until Result <> 10000;
      finally
         if GerencialAberto then
         try
            ComandarECF( opeFechaGerencial );
         except
            ImpressaoOk := False ;
         end;

         if (fArqBackUp <> '') and FileExists( fArqBackUp ) then
            SysUtils.DeleteFile( fArqBackUp );

         if HouveImpressao or ( ImprimirComprovantes and fCancelamento) then
            FinalizarTransacao( ImpressaoOk, Resp.DocumentoVinculado );

         if FinalizarTransacaoInterrompida then
             FinalizarTransacao( False, Resp.DocumentoVinculado );

         BloquearMouseTeclado( False );

         { Transfere valore de "Conteudo" para as propriedades }
         if LogDebug then
            GravaLog( Self.Resp.Conteudo.Conteudo.Text );

         TACBrTEFDRespCliSiTef( Self.Resp ).ConteudoToProperty ;

         fUltimoSTS := Result;

         if (HouveImpressao and fCancelamento) then
            DoExibeMsg( opmOK,
                        Format( CACBrTEFCliSiTef_TransacaoEfetuadaReImprimir,
                                [Resp.NSU]) ) ;

         fpAguardandoResposta := False ;
      end;
   end ;
end;

function TACBrTEFDCliSiTef.CopiarResposta: string;
begin
  if Trim(fArqBackUp) <> '' then
  begin
    if FileExists(fArqBackUp) then
    begin
      if not SysUtils.DeleteFile(fArqBackUp) then
        raise EFilerError.CreateFmt('Não foi possivel apagar o arquivo "%s" de backup!', [fArqBackUp]);
    end;

    fArqBackUp := '';
  end;

  Result := inherited CopiarResposta;
end;

procedure TACBrTEFDCliSiTef.AvaliaErro( Sts : Integer );
var
   Erro : String;
begin
   if not fExibirErroRetorno then
     Exit;

   Erro := fSiTefAPI.TraduzirErroTransacao(Sts);
   if (Erro <> '') then
      TACBrTEFD(Owner).DoExibeMsg( opmOK, Erro );
end ;

procedure TACBrTEFDCliSiTef.ProcessarResposta;
var
   RespostaPendente: TACBrTEFDRespCliSiTef;
begin
  GravaLog( Name +' ProcessarResposta: '+Req.Header );

  TACBrTEFD(Owner).EstadoResp := respProcessando;

  if Resp.QtdLinhasComprovante > 0 then
  begin
      { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
      RespostaPendente := TACBrTEFDRespCliSiTef.Create ;
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
  end ;
end;

procedure TACBrTEFDCliSiTef.FinalizarTransacao(Confirma: Boolean;
  DocumentoVinculado: AnsiString; ParamAdic: AnsiString);
Var
   DataStr, HoraStr : AnsiString;
   Finalizacao : SmallInt;
   AMsg: String;
   Est: AnsiChar;
   DataHora: TDateTime;
begin
   fRespostas.Clear;
   fIniciouRequisicao := False;

   { Re-Impressão não precisa de Finalização }
   if fReimpressao then
      exit ;

   { Já Finalizou este Documento por outra Transação ? }
   if (pos(DocumentoVinculado, fDocumentosProcessados) > 0) then
      exit;

  fDocumentosProcessados := fDocumentosProcessados + DocumentoVinculado + '|' ;

  if (fDataHoraFiscal <> 0) then
  begin
    // DataHoraFiscal foi definida antes da chamada a "FazerRequisicao" pelo aplicativo
    DataHora := DataHoraFiscal;
    fDataHoraFiscal := 0;
  end
  else if Assigned(Resp) and (Resp.DataHoraTransacaoComprovante > (date - 3)) then
  begin
    // Leu com sucesso o arquivo pendente.
    // Transações com mais de três dias são finalizadas automaticamente pela SiTef
    DataHora := Resp.DataHoraTransacaoComprovante
  end
  else
    DataHora := Now;

  DataStr  := FormatDateTime('YYYYMMDD', DataHora );
  HoraStr  := FormatDateTime('HHNNSS', DataHora );
  Finalizacao := ifthen(Confirma or fCancelamento,1,0);

  GravaLog( '*** FinalizaTransacaoSiTefInterativo. Confirma: '+
                                          IfThen(Finalizacao = 1,'SIM','NAO')+
                                          ' Documento: ' +DocumentoVinculado+
                                          ' Data: '      +DataStr+
                                          ' Hora: '      +HoraStr ) ;

  fSiTefAPI.FinalizaFuncaoSiTefInterativo( Finalizacao,
                                           PAnsiChar( DocumentoVinculado ),
                                           PAnsiChar( DataStr ),
                                           PAnsiChar( HoraStr ),
                                           PAnsiChar( ParamAdic ) ) ;

  if not Confirma then
  begin
     if fCancelamento then
        AMsg := Format( CACBrTEFCliSiTef_TransacaoEfetuadaReImprimir, [Resp.NSU])
     else
     begin
        try
           Est := TACBrTEFD(Owner).EstadoECF;
        except
           Est := 'O' ;
        end;

        if (Est = 'O') then
           AMsg := CACBrTEFCliSiTef_TransacaoNaoEfetuada
        else
           AMsg := CACBrTEFCliSiTef_TransacaoNaoEfetuadaReterCupom;
     end;

     TACBrTEFD(Owner).DoExibeMsg( opmOK, AMsg );
  end;
end;

function TACBrTEFDCliSiTef.DefineMensagemPermanentePinPad(Mensagem: AnsiString): Integer;
begin
  Result := fSiTefAPI.DefineMensagemPermanentePinPad(Mensagem);
end;

function TACBrTEFDCliSiTef.ObtemQuantidadeTransacoesPendentes(Data: TDateTime;
  CupomFiscal: AnsiString): Integer;
begin
  Result := fSiTefAPI.ObtemQuantidadeTransacoesPendentes(Data, CupomFiscal);
end;

function TACBrTEFDCliSiTef.EnviaRecebeSiTefDireto(RedeDestino: SmallInt;
  FuncaoSiTef: SmallInt; OffsetCartao: SmallInt; DadosTx: AnsiString;
  var DadosRx: AnsiString; var CodigoResposta: SmallInt;
  TempoEsperaRx: SmallInt; CupomFiscal: AnsiString; Confirmar: Boolean;
  Operador: AnsiString): Integer;
begin
  GravaLog( 'EnviaRecebeSiTefDireto -> Rede:' +IntToStr(RedeDestino) +', Funcao:'+
            IntToStr(FuncaoSiTef)+', OffSetCartao:'+IntToStr(OffsetCartao)+', DadosTX:'+
            DadosTx+', TimeOut'+IntToStr(TempoEsperaRx)+' Cupom:'+CupomFiscal+', '+
            ifthen(Confirmar,'Confirmar','NAO Confirmar'), True );

  Result := fSiTefAPI.EnviaRecebeSiTefDireto( RedeDestino, FuncaoSiTef, OffsetCartao,
                                              DadosTx, DadosRx, CodigoResposta,
                                              TempoEsperaRx, CupomFiscal,
                                              Confirmar, Operador);
end;

function TACBrTEFDCliSiTef.ValidaCampoCodigoEmBarras(Dados: AnsiString;
  var Tipo: SmallInt): Integer;
begin
  GravaLog('ValidaCampoCodigoEmBarras -> Dados:' + Dados, True);
  Result := fSiTefAPI.ValidaCampoCodigoEmBarras(Dados, Tipo);
end;

function TACBrTEFDCliSiTef.VerificaPresencaPinPad: Boolean;
begin
  GravaLog('VerificaPresencaPinpad', True);
  Result := fSiTefAPI.VerificaPresencaPinPad;
end;

function TACBrTEFDCliSiTef.ObtemDadoPinPadDiretoEx(TipoDocumento: Integer;
  ChaveAcesso, Identificador: AnsiString): AnsiString;
begin
  Result := fSiTefAPI.ObtemDadoPinPadDiretoEx(TipoDocumento, ChaveAcesso, Identificador);
end;

function TACBrTEFDCliSiTef.GetDataHoraFiscal: TDateTime;
begin
  if (csDesigning in Owner.ComponentState) then
     Result := fDataHoraFiscal
  else
     if fDataHoraFiscal = 0 then
        Result := Now
     else
        Result := fDataHoraFiscal;
end;

function TACBrTEFDCliSiTef.GetDocumentoFiscal: AnsiString;
begin
  if (csDesigning in Owner.ComponentState) then
     Result := fDocumentoFiscal
  else
     if fDocumentoFiscal = '' then
        Result := HMS
     else
        Result := fDocumentoFiscal;
end;

function TACBrTEFDCliSiTef.GetPathDLL: string;
begin
  Result := fSiTefAPI.PathDLL;
end;


function TACBrTEFDCliSiTef.ProcessarRespostaPagamento(
  const IndiceFPG_ECF: String; const Valor: Double): Boolean;
var
  ImpressaoOk : Boolean;
  RespostaPendente : TACBrTEFDResp ;
begin
  Result := True ;

  with TACBrTEFD(Owner) do
  begin
     Self.Resp.IndiceFPG_ECF := IndiceFPG_ECF;

     { Cria Arquivo de Backup, contendo Todas as Respostas }

     CopiarResposta ;

     { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
     Resp.ViaClienteReduzida := ImprimirViaClienteReduzida;
     RespostaPendente := TACBrTEFDRespCliSiTef.Create ;
     RespostaPendente.Assign( Resp );
     RespostasPendentes.Add( RespostaPendente );

     if AutoEfetuarPagamento then
     begin
        ImpressaoOk := False ;

        try
           while not ImpressaoOk do
           begin
              try
                 ECFPagamento( IndiceFPG_ECF, Valor );
                 RespostasPendentes.SaldoAPagar  := RoundTo( RespostasPendentes.SaldoAPagar - Valor, -2 ) ;
                 RespostaPendente.OrdemPagamento := RespostasPendentes.Count + 1 ;
                 ImpressaoOk := True ;
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

     if RespostasPendentes.SaldoRestante <= 0 then
     begin
        if AutoFinalizarCupom then
        begin
           FinalizarCupom( False );  { False não desbloqueia o MouseTeclado }
           ImprimirTransacoesPendentes;
        end;
     end ;
  end;
end;

procedure TACBrTEFDCliSiTef.VerificarIniciouRequisicao;
begin
  if not fIniciouRequisicao then
     raise EACBrTEFDErro.Create( ACBrStr( CACBrTEFD_Erro_SemRequisicao ) ) ;
end;

function TACBrTEFDCliSiTef.SuportaDesconto: Boolean;
begin
  with TACBrTEFD(Owner) do
  begin
     Result := (Identificacao.SoftwareHouse <> '') and
               Assigned( OnComandaECFSubtotaliza ) and
               (not AutoEfetuarPagamento) ;
  end;
end;

function TACBrTEFDCliSiTef.HMS: String;
begin
   Result := FormatDateTime('hhmmss',Now);
end;

function TACBrTEFDCliSiTef.CDP(const EntidadeCliente: string; out Resposta: string): Boolean;
var
  ATipo: Integer;
begin
  if EntidadeCliente = 'F' then
    ATipo := 1
  else if EntidadeCliente = 'J' then
    ATipo := 2
  else
    ATipo := 3;

  Resposta := ObtemDadoPinPadDiretoEx(ATipo, PinPadChaveAcesso, PinPadIdentificador);
  Result := (Resposta <> '');
end;

procedure TACBrTEFDCliSiTef.ExibirMensagemPinPad(const MsgPinPad: String);
var
  Ret: Integer;
  Erro: String;
begin
  Ret := DefineMensagemPermanentePinPad(MsgPinPad);

  if (Ret <> 0) then
  begin
    Erro := fSiTefAPI.TraduzirErroTransacao(Ret);
    if (Erro <> '') then
      raise EACBrTEFDErro.Create( ACBrStr(Erro) ) ;
  end;
end;

end.
