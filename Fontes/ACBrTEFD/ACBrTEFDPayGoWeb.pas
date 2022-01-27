{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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

unit ACBrTEFDPayGoWeb;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrTEFDClass, ACBrTEFPayGoWebComum, ACBrTEFPayGoComum, ACBrTEFComum;

resourcestring
  SACBrTEFDPayGoWeb_TransacaoPendente =
    'Transação Pendente.' + sLineBreak +
    '%s' + sLineBreak +
    'R$ %s' + sLineBreak +
    '%s' + sLineBreak +
    'NSU: %s - Autorização: %s';

  SACBrTEFDPayGoWeb_TransacaoPendenteAPI =
    'Transação Pendente.' + sLineBreak +
    '%s' + sLineBreak +
    'NSU: %s';

  SACBrTEFDPayGoWeb_TransacaoConfirmadaReImprimir =
    'Transação TEF Confirmada.' + sLineBreak +
    'Favor reimprimir último Comprovante.' + sLineBreak +
    '%s';

  SACBrTEFDPayGoWeb_TransacaoCancelada =
    'Transação TEF Cancelada.' + sLineBreak +
    '%s';


type

  { TACBrTEFDRespPayGoWeb }

  TACBrTEFDRespPayGoWeb = class( TACBrTEFDResp )
  public
    procedure ConteudoToProperty; override;
  end;

  TACBrTEFDAvaliarTransacaoPendente = procedure(var Status: LongWord;
    const Mensagem: String; Resp: TACBrTEFDResp) of object;

  { TACBrTEFDPayGoWeb }

  TACBrTEFDPayGoWeb = class( TACBrTEFDClass )
  private
    fOnAvaliarTransacaoPendente: TACBrTEFDAvaliarTransacaoPendente;
    fOperacaoADM: Word;
    fOperacaoATV: Word;
    fOperacaoCHQ: Word;
    fOperacaoCNC: Word;
    fOperacaoCRT: Word;
    fOperacaoPRE: Word;
    fsPGWebAPI: TACBrTEFPGWebAPI;
    function GetConfirmarTransacoesPendentes: Boolean;
    function GetExibicaoQRCode: TACBrTEFPGWebAPIExibicaoQRCode;
    function GetParametrosAdicionais: TACBrTEFParametros;
    function GetPerguntarCartaoDigitadoAposCancelarLeitura: Boolean;
    function GetPortaPinPad: Integer;
    procedure GravaLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure EXibirQRCodeAPI(const Dados: String);

    function GetCNPJEstabelecimento: String;
    function GetDiretorioTrabalho: String;
    function GetOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
    function GetOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
    function GetOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
    function GetOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
    function GetPathDLL: string;
    function GetPontoCaptura: String;
    function GetSuportaViasDiferenciadas: Boolean;
    function GetUtilizaSaldoTotalVoucher: Boolean;
    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetConfirmarTransacoesPendentes(AValue: Boolean);
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetExibicaoQRCode(AValue: TACBrTEFPGWebAPIExibicaoQRCode);
    procedure SetOnAguardaPinPad(AValue: TACBrTEFPGWebAPIAguardaPinPad);
    procedure SetOnExibeMensagem(AValue: TACBrTEFPGWebAPIExibeMensagem);
    procedure SetOnExibeMenu(AValue: TACBrTEFPGWebAPIExibeMenu);
    procedure SetOnObtemCampo(AValue: TACBrTEFPGWebAPIObtemCampo);
    procedure SetPathDLL(AValue: string);
    procedure SetPerguntarCartaoDigitadoAposCancelarLeitura(AValue: Boolean);
    procedure SetPontoCaptura(AValue: String);
    procedure SetPortaPinPad(AValue: Integer);
    procedure SetSuportaViasDiferenciadas(AValue: Boolean);
    procedure SetUtilizaSaldoTotalVoucher(AValue: Boolean);
  protected
    Procedure LerRespostaRequisicao; override;
    procedure FinalizarResposta( ApagarArqResp : Boolean ); override;

    procedure FazerRequisicao(PWOPER: Word; AHeader: String = '';
        AValor: Double = 0; ADocumentoVinculado: String = ''; AMoeda : Integer = 0;
         ParametrosAdicionaisTransacao: TStrings = nil);
    function ContinuarRequisicao: Boolean;
    procedure ObterDadosTransacao;
    procedure FinalizarTransacao(Status: LongWord; pszReqNum: String;
      pszLocRef: String; pszExtRef: String; pszVirtMerch: String;
      pszAuthSyst: String);

    procedure AvaliarTransacaoPendenteAPI(var Status: LongWord;
      pszReqNum: String; pszLocRef: String; pszExtRef: String; pszVirtMerch: String;
      pszAuthSyst: String);
  public
    property PGWebAPI: TACBrTEFPGWebAPI read fsPGWebAPI;
    property PathDLL: string read GetPathDLL write SetPathDLL;
    property DiretorioTrabalho: String read GetDiretorioTrabalho write SetDiretorioTrabalho;
    property ParametrosAdicionais: TACBrTEFParametros read GetParametrosAdicionais;

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
    Procedure NCN; overload; override;
    Procedure NCN(Rede, NSU, Finalizacao : String;
       Valor : Double = 0; DocumentoVinculado : String = ''); override;
    Procedure CNF ; overload; override;
    Procedure CNF(Rede, NSU, Finalizacao : String;
       DocumentoVinculado : String = ''); overload; override;
    Function CNC : Boolean ; overload; override;
    function CNC(Rede, NSU: String; DataHoraTransacao: TDateTime; Valor: Double;
      CodigoAutorizacaoTransacao: String = ''): Boolean; overload; override;
    Function PRE(Valor : Double; DocumentoVinculado : String = '';
       Moeda : Integer = 0) : Boolean; override;
    function CDP(const EntidadeCliente: string; out Resposta: string): Boolean; override;

    procedure ExibirMensagemPinPad(const MsgPinPad: String); override;

    procedure VerificarTransacoesPendentesClass(aVerificarCupom: Boolean); override;
  published
    property CNPJEstabelecimento: String read GetCNPJEstabelecimento write SetCNPJEstabelecimento;
    property PontoCaptura: String read GetPontoCaptura write SetPontoCaptura;
    property PortaPinPad: Integer read GetPortaPinPad write SetPortaPinPad default 0;  // -1 = Sem PinPad
    property SuportaViasDiferenciadas: Boolean read GetSuportaViasDiferenciadas
      write SetSuportaViasDiferenciadas;
    property UtilizaSaldoTotalVoucher: Boolean read GetUtilizaSaldoTotalVoucher
      write SetUtilizaSaldoTotalVoucher;
    property ConfirmarTransacoesPendentes: Boolean read GetConfirmarTransacoesPendentes
      write SetConfirmarTransacoesPendentes;
    property PerguntarCartaoDigitadoAposCancelarLeitura: Boolean
      read GetPerguntarCartaoDigitadoAposCancelarLeitura
      write SetPerguntarCartaoDigitadoAposCancelarLeitura;
    property ExibicaoQRCode: TACBrTEFPGWebAPIExibicaoQRCode read GetExibicaoQRCode
      write SetExibicaoQRCode default qreAuto;

    property OperacaoATV: Word read fOperacaoATV write fOperacaoATV default PWOPER_NULL;
    property OperacaoADM: Word read fOperacaoADM write fOperacaoADM default PWOPER_ADMIN;
    property OperacaoCRT: Word read fOperacaoCRT write fOperacaoCRT default PWOPER_SALE;
    property OperacaoCHQ: Word read fOperacaoCHQ write fOperacaoCHQ default PWOPER_CHECKINQ;
    property OperacaoCNC: Word read fOperacaoCNC write fOperacaoCNC default PWOPER_SALEVOID;
    property OperacaoPRE: Word read fOperacaoPRE write fOperacaoPRE default PWOPER_PREPAID;

    property OnExibeMenu: TACBrTEFPGWebAPIExibeMenu read GetOnExibeMenu
      write SetOnExibeMenu;
    property OnObtemCampo: TACBrTEFPGWebAPIObtemCampo read GetOnObtemCampo
      write SetOnObtemCampo;
    property OnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem read GetOnExibeMensagem
      write SetOnExibeMensagem;
    property OnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad read GetOnAguardaPinPad
      write SetOnAguardaPinPad;
    property OnAvaliarTransacaoPendente: TACBrTEFDAvaliarTransacaoPendente
      read fOnAvaliarTransacaoPendente write fOnAvaliarTransacaoPendente;
  end;

implementation

uses
  Math,
  ACBrTEFD, ACBrUtil, ACBrConsts;

{ TACBrTEFDRespPayGoWeb }

procedure TACBrTEFDRespPayGoWeb.ConteudoToProperty;
begin
  ConteudoToPropertyPayGoWeb( Self );
end;

{ TACBrTEFDPayGoWeb }

constructor TACBrTEFDPayGoWeb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fsPGWebAPI := TACBrTEFPGWebAPI.Create;
  fsPGWebAPI.OnGravarLog := GravaLogAPI;
  fsPGWebAPI.OnAvaliarTransacaoPendente := AvaliarTransacaoPendenteAPI;
  fsPGWebAPI.OnExibeQRCode := EXibirQRCodeAPI;

  ArqReq := '';
  ArqResp := '';
  ArqSTS := '';
  ArqTemp := '';
  GPExeName := '';
  fpTipo := gpPayGoWeb;
  Name := 'PayGoWeb';

  fOperacaoATV := PWOPER_NULL;
  fOperacaoADM := PWOPER_ADMIN;
  fOperacaoCRT := PWOPER_SALE;
  fOperacaoCHQ := PWOPER_CHECKINQ;
  fOperacaoCNC := PWOPER_SALEVOID;
  fOperacaoPRE := PWOPER_PREPAID;

  if Assigned(fpResp) then
    fpResp.Free ;

  fpResp := TACBrTEFDRespPayGoWeb.Create;
  fpResp.TipoGP := fpTipo;
  fOnAvaliarTransacaoPendente := Nil;
end;

destructor TACBrTEFDPayGoWeb.Destroy;
begin
  fsPGWebAPI.Free;
  inherited Destroy;
end;

procedure TACBrTEFDPayGoWeb.Inicializar;
begin
  if Inicializado then
    Exit;

  fsPGWebAPI.SoftwareHouse := TACBrTEFD(Owner).Identificacao.SoftwareHouse;
  fsPGWebAPI.NomeAplicacao := TACBrTEFD(Owner).Identificacao.NomeAplicacao;
  fsPGWebAPI.VersaoAplicacao := TACBrTEFD(Owner).Identificacao.VersaoAplicacao;
  fsPGWebAPI.SuportaDesconto := TACBrTEFD(Owner).SuportaDesconto;
  fsPGWebAPI.SuportaSaque := TACBrTEFD(Owner).SuportaSaque;
  fsPGWebAPI.ImprimeViaClienteReduzida := TACBrTEFD(Owner).ImprimirViaClienteReduzida;

  fsPGWebAPI.Inicializada := True;
  GravaLog( Name +' Inicializado '+Name );

  VerificarTransacoesPendentesClass(True);
  fpInicializado := True;
end;

procedure TACBrTEFDPayGoWeb.DesInicializar;
begin
  fsPGWebAPI.Inicializada := False; ;
  inherited DesInicializar;
end;

procedure TACBrTEFDPayGoWeb.AtivarGP;
begin
  raise EACBrTEFDErro.Create( ACBrStr( 'AtivarGP não se aplica a '+Name )) ;
end;

procedure TACBrTEFDPayGoWeb.LerRespostaRequisicao;
begin
  {Nada a Fazer}
end;

procedure TACBrTEFDPayGoWeb.FinalizarResposta(ApagarArqResp: Boolean);
begin
  {Nada a Fazer}
end;

procedure TACBrTEFDPayGoWeb.VerificaAtivo;
begin
  {Nada a Fazer}
end;

procedure TACBrTEFDPayGoWeb.ATV;
begin
  FazerRequisicao(fOperacaoATV, 'ATV');
  if ContinuarRequisicao then
    ProcessarResposta;
end;

function TACBrTEFDPayGoWeb.ADM: Boolean;
begin
  FazerRequisicao(fOperacaoADM, 'ADM');
  Result := ContinuarRequisicao;
  if Result then
    ProcessarResposta;
end;

function TACBrTEFDPayGoWeb.CRT(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
begin
  if (Valor <> 0) then
    VerificarTransacaoPagamento( Valor );

  FazerRequisicao(fOperacaoCRT, 'CRT', Valor, DocumentoVinculado, Moeda);
  Result := ContinuarRequisicao and
            ProcessarRespostaPagamento(IndiceFPG_ECF, Valor);
end;

function TACBrTEFDPayGoWeb.CHQ(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; CMC7: String; TipoPessoa: AnsiChar;
  DocumentoPessoa: String; DataCheque: TDateTime; Banco: String;
  Agencia: String; AgenciaDC: String; Conta: String; ContaDC: String;
  Cheque: String; ChequeDC: String; Compensacao: String): Boolean;
begin
  if (Valor <> 0) then
    VerificarTransacaoPagamento( Valor );

  FazerRequisicao(fOperacaoCHQ, 'CHQ', Valor, DocumentoVinculado);
  Result := ContinuarRequisicao and
            ProcessarRespostaPagamento(IndiceFPG_ECF, Valor);
end;

procedure TACBrTEFDPayGoWeb.NCN;
begin
  if Resp.Confirmar then
    inherited NCN;
end;

procedure TACBrTEFDPayGoWeb.NCN(Rede, NSU, Finalizacao: String; Valor: Double;
  DocumentoVinculado: String);
begin
  FinalizarTransacao( PWCNF_REV_FISC_AUT,
                      IntToStr(Resp.NumeroLoteTransacao),
                      Finalizacao,
                      NSU,
                      Resp.Estabelecimento,
                      Rede);
end;

procedure TACBrTEFDPayGoWeb.CNF;
begin
  if Resp.Confirmar then
    inherited CNF;
end;

procedure TACBrTEFDPayGoWeb.CNF(Rede, NSU, Finalizacao: String;
  DocumentoVinculado: String);
begin
  FinalizarTransacao( PWCNF_CNF_AUTO,
                      IntToStr(Resp.NumeroLoteTransacao),
                      Finalizacao,
                      NSU,
                      Resp.Estabelecimento,
                      Rede);
end;

function TACBrTEFDPayGoWeb.CNC: Boolean;
begin
  Result := CNC(Resp.Rede, Resp.NSU, Resp.DataHoraTransacaoLocal, Resp.ValorTotal, Resp.CodigoAutorizacaoTransacao);
end;

function TACBrTEFDPayGoWeb.CNC(Rede, NSU: String; DataHoraTransacao: TDateTime;
  Valor: Double; CodigoAutorizacaoTransacao: String): Boolean;
var
  PA: TACBrTEFParametros;

  procedure CopiarValorDaUltimaResposta(AInfo: Integer);
  var
    AStr: String;
  begin
    AStr := Resp.LeInformacao(AInfo).AsString;
    if (Trim(AStr) <> '') then
      PA.ValueInfo[AInfo] := AStr;
  end;

begin
  PA := TACBrTEFParametros.Create;
  try
    PA.ValueInfo[PWINFO_AUTHSYST] := Rede;
    PA.ValueInfo[PWINFO_TRNORIGNSU] := NSU;                                            // Mandatorio
    PA.ValueInfo[PWINFO_TRNORIGDATE] := FormatDateTime('DDMMYY', DataHoraTransacao);   // Mandatorio
    PA.ValueInfo[PWINFO_TRNORIGTIME] := FormatDateTime('hhnnss', DataHoraTransacao);   // Mandatorio
    //PA.ValueInfo[PWINFO_TRNORIGDATETIME] := FormatDateTime('YYYYMMDDhhnnss', DataHoraTransacao);
    PA.ValueInfo[PWINFO_TRNORIGAMNT] :=  IntToStr(Trunc(RoundTo(Valor * 100,-2)));     // Mandatorio
    if (CodigoAutorizacaoTransacao <> '') then
    begin
      PA.ValueInfo[PWINFO_TRNORIGAUTH] := CodigoAutorizacaoTransacao;                  // Mandatorio
      PA.ValueInfo[PWINFO_TRNORIGAUTHCODE] := CodigoAutorizacaoTransacao;
    end;

    // Se a transação em memória for a mesma que estamos tentando cancelar, vamos copiar mais dados dela...
    if (Resp.Rede = Rede) and (Resp.NSU = NSU) and
       (Resp.ValorTotal = Valor) and (Resp.DataHoraTransacaoLocal = DataHoraTransacao) then
    begin
      PA.ValueInfo[PWINFO_TRNORIGLOCREF] := Resp.Finalizacao;
      PA.ValueInfo[PWINFO_TRNORIGREQNUM] := IntToStr(Resp.NumeroLoteTransacao);
      //CopiarValorDaUltimaResposta(PWINFO_MERCHCNPJCPF);
      CopiarValorDaUltimaResposta(PWINFO_CARDTYPE);
      CopiarValorDaUltimaResposta(PWINFO_VIRTMERCH);
      CopiarValorDaUltimaResposta(PWINFO_AUTMERCHID);
      CopiarValorDaUltimaResposta(PWINFO_FINTYPE);
    end;

    FazerRequisicao(fOperacaoCNC, 'CNC', Valor, '', 0, PA);
  finally
    PA.Free;
  end;

  Result := ContinuarRequisicao;
  if Result then
    ProcessarResposta;
end;

function TACBrTEFDPayGoWeb.PRE(Valor: Double; DocumentoVinculado: String;
  Moeda: Integer): Boolean;
begin
  FazerRequisicao(fOperacaoPRE, 'PRE', Valor, DocumentoVinculado, Moeda);
  Result := ContinuarRequisicao;
  if Result then
    ProcessarResposta;
end;

function TACBrTEFDPayGoWeb.CDP(const EntidadeCliente: string; out
  Resposta: string): Boolean;
var
  TipoMsg: Word;
  MinLen, MaxLen: Byte;
begin
  MinLen := 0; MaxLen := 0;

  if EntidadeCliente = 'F' then
    TipoMsg := PWDPIN_DIGITE_O_CPF
  else if EntidadeCliente = 'X' then
    TipoMsg := PWDPIN_DIGITE_O_TELEFONE
  else if EntidadeCliente = 'J' then
    TipoMsg := PWDPIN_DIGITE_O_CNPJ
  else
    TipoMsg := StrToIntDef(EntidadeCliente, 0);

  case TipoMsg of
    PWDPIN_DIGITE_O_DDD, PWDPIN_REDIGITE_O_DDD:
    begin
      MinLen := 2; MaxLen := 2;
    end;
    PWDPIN_DIGITE_O_TELEFONE, PWDPIN_REDIGITE_O_TELEFONE:
    begin
      MinLen := 8; MaxLen := 9;
    end;
    PWDPIN_DIGITE_DDD_TELEFONE, PWDPIN_REDIGITE_DDD_TELEFONE:
    begin
      MinLen := 10; MaxLen := 11;
    end;
    PWDPIN_DIGITE_O_CPF, PWDPIN_REDIGITE_O_CPF:
    begin
      MinLen := 11; MaxLen := 11;
    end;
    PWDPIN_DIGITE_O_RG, PWDPIN_REDIGITE_O_RG:
    begin
      MinLen := 5; MaxLen := 11;
    end;
    PWDPIN_DIGITE_OS_4_ULTIMOS_DIGITOS:
    begin
      MinLen := 4; MaxLen := 4;
    end;
    PWDPIN_DIGITE_CODIGO_DE_SEGURANCA:
    begin
      MinLen := 3; MaxLen := 5;
    end;
    PWDPIN_DIGITE_O_CNPJ, PWDPIN_REDIGITE_O_CNPJ:
    begin
      MinLen := 14; MaxLen := 14;
    end;

  else
    raise EACBrTEFDErro.CreateFmt( ACBrStr('Captura Tipo: %s não suportada por: %s'), [EntidadeCliente, ClassName] )
  end;

  Resposta := fsPGWebAPI.ObterDadoPinPad(TipoMsg, MinLen, MaxLen, 30);  // 30 Segundos de Timeout
  Result := (Resposta <> '');
end;

procedure TACBrTEFDPayGoWeb.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  fsPGWebAPI.ExibirMensagemPinPad(MsgPinPad);
end;

procedure TACBrTEFDPayGoWeb.VerificarTransacoesPendentesClass(
  aVerificarCupom: Boolean);
Var
  ArquivosVerficar: TStringList;
  ArqMask, NomeArqTEF, NSUsConf, NSUsCanc: String;
  AResposta: TACBrTEFDResp;
  Confirmar: Boolean;
  ulResult: LongWord;
  RespostasCanceladas, RespostasConfirmadas: TACBrTEFDRespostasPendentes;
  I, Topo: Integer;
begin
  ArquivosVerficar := TStringList.Create;
  RespostasCanceladas := TACBrTEFDRespostasPendentes.create(True);
  RespostasConfirmadas := TACBrTEFDRespostasPendentes.create(True);
  try
    TACBrTEFD(Owner).RespostasPendentes.Clear;
    NSUsConf := '';
    NSUsCanc := '';
    { Achando Arquivos de Backup deste GP }
    ArqMask := TACBrTEFD(Owner).PathBackup + PathDelim + 'ACBr_' + Self.Name + '_*.tef';
    FindFiles(ArqMask, ArquivosVerficar, True);

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

    { Enviando CNF, NCN ou CNC para todos os arquivos encontrados, conforme valores da
      Propriedade "ConfirmarTransacoesPendentes" ou evento "OnAvaliarTransacaoPendente" }
    while ArquivosVerficar.Count > 0 do
    begin
      NomeArqTEF := ArquivosVerficar[0];

      if FileExists(NomeArqTEF) then
      begin
        Resp.LeArquivo(NomeArqTEF);

        if Resp.Confirmar then
        begin
          if ConfirmarTransacoesPendentes then
            ulResult :=  PWCNF_CNF_MANU_AUT
          else
            ulResult :=  PWCNF_REV_PWR_AUT;

          if Assigned(fOnAvaliarTransacaoPendente) then
          begin
            fOnAvaliarTransacaoPendente( ulResult,
                                         Format( ACBrStr(SACBrTEFDPayGoWeb_TransacaoPendente),
                                                 [FormatDateTimeBr(Resp.DataHoraTransacaoLocal),
                                                  FormatFloatBr(Resp.ValorTotal),
                                                  Resp.Rede,
                                                  Resp.NSU, Resp.CodigoAutorizacaoTransacao] ),
                                         Resp);
          end;

          Confirmar := (ulResult = PWCNF_CNF_AUTO) or (ulResult = PWCNF_CNF_MANU_AUT);

          { Criando cópia da Resposta Atual }
          AResposta := TACBrTEFDRespPayGoWeb.Create;
          AResposta.Assign(Resp);

          if (not Confirmar) and Resp.CNFEnviado then
          begin
            if not CNC then
              Confirmar := True;  // Se não conseguiu cancelar a transação, informe que foi confirmada
          end

          else if not Resp.CNFEnviado then
          begin
              FinalizarTransacao( ulResult,
                                  IntToStr(AResposta.NumeroLoteTransacao),
                                  AResposta.Finalizacao,
                                  AResposta.NSU,
                                  AResposta.Estabelecimento,
                                  AResposta.Rede);
          end;

          if Confirmar then
          begin
            RespostasConfirmadas.Add(AResposta);
            if (Trim(AResposta.NSU) <> '') then
              NSUsConf := NSUsConf + AResposta.NSU + sLineBreak;
          end
          else
          begin
            RespostasCanceladas.Add(AResposta);
            if (Trim(AResposta.NSU) <> '') then
              NSUsCanc := NSUsCanc + AResposta.NSU + sLineBreak;
          end;
        end;

        SysUtils.DeleteFile(NomeArqTEF);
        ArquivosVerficar.Delete(0);
      end;
    end;

    // Chamando evento de Confirmação/Cancelamento das Respostas //
    with TACBrTEFD(Owner) do
    begin
      if (RespostasConfirmadas.Count > 0) then
      begin
        if Assigned(OnDepoisConfirmarTransacoes) then
          OnDepoisConfirmarTransacoes( RespostasConfirmadas )
        else if (NSUsConf <> '') then
          DoExibeMsg(opmOK, Format( SACBrTEFDPayGoWeb_TransacaoConfirmadaReImprimir, [NSUsConf]));
      end;

      if (RespostasCanceladas.Count > 0) then
      begin
        if Assigned(OnDepoisCancelarTransacoes) then
          OnDepoisCancelarTransacoes( RespostasCanceladas )
        else if (NSUsCanc <> '') then
          DoExibeMsg(opmOK, Format( SACBrTEFDPayGoWeb_TransacaoCancelada, [NSUsCanc]));
      end;
    end;
  finally
    ArquivosVerficar.Free;
    RespostasCanceladas.Free;
    RespostasConfirmadas.Free;
  end;
end;

procedure TACBrTEFDPayGoWeb.FazerRequisicao(PWOPER: Word; AHeader: String;
  AValor: Double; ADocumentoVinculado: String; AMoeda: Integer;
  ParametrosAdicionaisTransacao: TStrings);
var
  PA: TACBrTEFParametros;
begin
  GravaLog('FazerRequisicao: Oper:'+PWOPERToString(PWOPER)+', Header:'+AHeader+
           ', Valor:'+FloatToStr(AValor)+', Documento:'+ADocumentoVinculado);

  { Transação a ser enviada é pagamento ? (CRT ou CHQ) }
  if TransacaoEPagamento(AHeader) then
  begin
     { PayGo não permite multiplas transações Pendentes... precisamos confirma a
       transação anterior antes de enviar uma nova }
     if TACBrTEFD(Owner).MultiplosCartoes then      // É multiplos cartoes ?
        ConfirmarTransacoesAnteriores;
  end;

  Req.Header := AHeader;
  Req.DocumentoVinculado := ADocumentoVinculado;
  Req.ValorTotal := AValor;

  PA := TACBrTEFParametros.Create;
  try
    if (AValor > 0) then
    begin
      PA.ValueInfo[PWINFO_CURREXP] := '2'; // centavos
      PA.ValueInfo[PWINFO_TOTAMNT] := IntToStr(Trunc(RoundTo(AValor * 100,-2)));
      PA.ValueInfo[PWINFO_CURRENCY] := IntToStr(MoedaToISO4217(AMoeda));
    end;

    if (ADocumentoVinculado <> '') then
      PA.ValueInfo[PWINFO_FISCALREF] := Trim(ADocumentoVinculado);

    if Assigned(ParametrosAdicionaisTransacao) then
      PA.AddStrings(ParametrosAdicionaisTransacao);

    fsPGWebAPI.IniciarTransacao(PWOPER, PA);
  finally
    PA.Free;
  end;

  { Adiciona Campos já conhecidos em Resp, para processa-los em
    métodos que manipulam "RespostasPendentes" (usa códigos do G.P.)  }
  Resp.Clear;
  with TACBrTEFDRespPayGoWeb( Resp ) do
  begin
    fpIDSeq := fpIDSeq + 1 ;
    if ADocumentoVinculado = '' then
      ADocumentoVinculado := IntToStr(fpIDSeq) ;

    Conteudo.GravaInformacao(899,100, AHeader ) ;
    Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
    Conteudo.GravaInformacao(899,102, ADocumentoVinculado ) ;

    Resp.TipoGP := fpTipo;
  end;
end;

function TACBrTEFDPayGoWeb.ContinuarRequisicao: Boolean;
begin
  try
    Result := fsPGWebAPI.ExecutarTransacao;
  finally
    ObterDadosTransacao;
  end;
end;

procedure TACBrTEFDPayGoWeb.ObterDadosTransacao;
var
  i, p, AInfo: Integer;
  Lin, AValue: String;
begin
  if (fsPGWebAPI.DadosDaTransacao.Count = 0) then
    fsPGWebAPI.ObterDadosDaTransacao;

  with TACBrTEFDRespPayGoWeb(Resp) do
  begin
    for i := 0 to fsPGWebAPI.DadosDaTransacao.Count-1 do
    begin
      Lin := fsPGWebAPI.DadosDaTransacao[i];
      p := pos('=', Lin);
      if (p > 0) then
      begin
        AInfo := StrToIntDef(copy(Lin, 1, p-1), -1);
        if (AInfo >= 0) then
        begin
          AValue := copy(Lin, P+1, Length(Lin));
          Conteudo.GravaInformacao(Ainfo, 0, AValue);
        end;
      end;
    end;
    
    //DEBUG
    //Conteudo.Conteudo.SaveToFile('c:\temp\PGWeb.txt');
    Resp.ViaClienteReduzida := TACBrTEFD(Owner).ImprimirViaClienteReduzida;
    ConteudoToProperty;
  end;
end;

procedure TACBrTEFDPayGoWeb.FinalizarTransacao(Status: LongWord;
  pszReqNum: String; pszLocRef: String; pszExtRef: String;
  pszVirtMerch: String; pszAuthSyst: String);
begin
  fsPGWebAPI.FinalizarTransacao( Status, pszReqNum, pszLocRef,
                                 pszExtRef, pszVirtMerch, pszAuthSyst);
end;

procedure TACBrTEFDPayGoWeb.AvaliarTransacaoPendenteAPI(var Status: LongWord;
  pszReqNum: String; pszLocRef: String; pszExtRef: String;
  pszVirtMerch: String; pszAuthSyst: String);
begin
  if ConfirmarTransacoesPendentes then
    Status := PWCNF_CNF_MANU_AUT
  else
    Status := PWCNF_REV_MANU_AUT;

  if Assigned(fOnAvaliarTransacaoPendente) then
    fOnAvaliarTransacaoPendente( Status,
                                 Format( ACBrStr(SACBrTEFDPayGoWeb_TransacaoPendenteAPI),
                                         [pszAuthSyst, pszExtRef] ), Resp);
end;

function TACBrTEFDPayGoWeb.GetPathDLL: string;
begin
  Result := fsPGWebAPI.PathLib;
end;

function TACBrTEFDPayGoWeb.GetPontoCaptura: String;
begin
  Result := fsPGWebAPI.PontoCaptura;
end;

function TACBrTEFDPayGoWeb.GetSuportaViasDiferenciadas: Boolean;
begin
  Result := fsPGWebAPI.SuportaViasDiferenciadas;
end;

function TACBrTEFDPayGoWeb.GetUtilizaSaldoTotalVoucher: Boolean;
begin
  Result := fsPGWebAPI.UtilizaSaldoTotalVoucher;
end;

procedure TACBrTEFDPayGoWeb.SetCNPJEstabelecimento(AValue: String);
begin
  fsPGWebAPI.CNPJEstabelecimento := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetConfirmarTransacoesPendentes(AValue: Boolean);
begin
  fsPGWebAPI.ConfirmarTransacoesPendentesNoHost := AValue;
end;

function TACBrTEFDPayGoWeb.GetDiretorioTrabalho: String;
begin
  Result := fsPGWebAPI.DiretorioTrabalho;
end;

function TACBrTEFDPayGoWeb.GetCNPJEstabelecimento: String;
begin
  Result := fsPGWebAPI.CNPJEstabelecimento;
end;

function TACBrTEFDPayGoWeb.GetOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
begin
  Result := fsPGWebAPI.OnAguardaPinPad;
end;

function TACBrTEFDPayGoWeb.GetOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
begin
  Result := fsPGWebAPI.OnExibeMensagem;
end;

function TACBrTEFDPayGoWeb.GetOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
begin
  Result := fsPGWebAPI.OnExibeMenu;
end;

function TACBrTEFDPayGoWeb.GetOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
begin
  Result := fsPGWebAPI.OnObtemCampo;
end;

procedure TACBrTEFDPayGoWeb.SetDiretorioTrabalho(AValue: String);
begin
  fsPGWebAPI.DiretorioTrabalho := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetExibicaoQRCode(AValue: TACBrTEFPGWebAPIExibicaoQRCode);
begin
  fsPGWebAPI.ExibicaoQRCode := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnAguardaPinPad(AValue: TACBrTEFPGWebAPIAguardaPinPad);
begin
  fsPGWebAPI.OnAguardaPinPad := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnExibeMensagem(AValue: TACBrTEFPGWebAPIExibeMensagem);
begin
  fsPGWebAPI.OnExibeMensagem := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnExibeMenu(AValue: TACBrTEFPGWebAPIExibeMenu);
begin
  fsPGWebAPI.OnExibeMenu := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnObtemCampo(AValue: TACBrTEFPGWebAPIObtemCampo);
begin
  fsPGWebAPI.OnObtemCampo := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPathDLL(AValue: string);
begin
  fsPGWebAPI.PathLib := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPerguntarCartaoDigitadoAposCancelarLeitura(
  AValue: Boolean);
begin
  fsPGWebAPI.PerguntarCartaoDigitadoAposCancelarLeitura := AValue;
end;

procedure TACBrTEFDPayGoWeb.GravaLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  GravaLog(ALogLine);
end;

procedure TACBrTEFDPayGoWeb.EXibirQRCodeAPI(const Dados: String);
begin
  TACBrTEFD(Owner).DoExibeQRCode(Dados);
end;

function TACBrTEFDPayGoWeb.GetParametrosAdicionais: TACBrTEFParametros;
begin
  Result := fsPGWebAPI.ParametrosAdicionais;
end;

function TACBrTEFDPayGoWeb.GetPerguntarCartaoDigitadoAposCancelarLeitura: Boolean;
begin
  Result := fsPGWebAPI.PerguntarCartaoDigitadoAposCancelarLeitura;
end;

function TACBrTEFDPayGoWeb.GetPortaPinPad: Integer;
begin
  Result := fsPGWebAPI.PortaPinPad;
end;

function TACBrTEFDPayGoWeb.GetConfirmarTransacoesPendentes: Boolean;
begin
  Result := fsPGWebAPI.ConfirmarTransacoesPendentesNoHost;
end;

function TACBrTEFDPayGoWeb.GetExibicaoQRCode: TACBrTEFPGWebAPIExibicaoQRCode;
begin
  Result := fsPGWebAPI.ExibicaoQRCode;
end;

procedure TACBrTEFDPayGoWeb.SetSuportaViasDiferenciadas(AValue: Boolean);
begin
  fsPGWebAPI.SuportaViasDiferenciadas := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetUtilizaSaldoTotalVoucher(AValue: Boolean);
begin
  fsPGWebAPI.UtilizaSaldoTotalVoucher := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPontoCaptura(AValue: String);
begin
  fsPGWebAPI.PontoCaptura := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPortaPinPad(AValue: Integer);
begin
  fsPGWebAPI.PortaPinPad := AValue;
end;

end.

