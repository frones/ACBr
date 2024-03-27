{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

{******************************************************************************
|* Historico
|*
|* 21/11/2009: Daniel Simoes de Almeida
|*  - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrSATClass ;

interface

uses
  Classes, SysUtils,
  pcnConversao, ACBrDFeSSL, ACBrBase;

const
  cLIBSAT = 'SAT.DLL';
  cversaoDadosEnt = 0.07;
  CPREFIXO_ArqCFe = 'AD';
  CPREFIXO_ArqCFeCanc = 'ADC';
  CPastaVendas = 'Vendas';
  CPastaCancelamentos = 'Cancelamentos';
  CPastaEnviados = 'Enviado';

  cACBrSATClassCreateException = 'Essa Classe deve ser instanciada por TACBrSAT' ;
  cACBrSATSetModeloException   = 'Não é possível mudar o Modelo com o SAT Inicializado' ;
  cACBrSATStreamException      = 'Este componente de impressão não da suporte a impressão em Stream' ;
  cACBrSATModeloNaoDefinido    = 'Modelo de SAT não definido' ;
  cACBrSATNaoInicializado      = 'ACBrSAT não foi inicializado corretamente' ;
  cACBrSATOcupadoException     = 'SAT ocupado!' + sLineBreak +
                                 'Aguardando resposta da sessão %d' ;
  cACBrSATFuncaoNaoEncontrada  = 'Erro ao carregar a função: %s na Biblioteca: %s' ;
  cACBrSATCMDInvalidoException = 'Procedure: %s '+ sLineBreak +
                                 ' não implementada para o SAT: %s'+sLineBreak + sLineBreak +
                                 'Ajude no desenvolvimento do ACBrSAT. '+ sLineBreak+
                                 'Acesse nosso Forum em: http://projetoacbr.com.br/' ;
  cACBrSATSemIntegrador        = 'Componente ACBrIntegrador não atribuido a ACBrSAT' ;
type

  TACBrSATModelo = ( satNenhum, satDinamico_cdecl, satDinamico_stdcall, mfe_Integrador_XML ) ;

  { EACBrSATErro }

  EACBrSATErro = class(Exception)
  public
    constructor Create(const msg : string);
  end ;

  { Eventos do componente }
  TACBrSATGetChave = procedure(var Chave: AnsiString) of object ;
  TACBrSATGetNumeroSessao = procedure(var NumeroSessao: Integer) of object ;

  { TACBrSATConfig }

  TACBrSATConfig = Class(TComponent)
  private
    fsOwner: TComponent;
    fsemit_CNPJ : String ;
    fsemit_cRegTrib : TpcnRegTrib ;
    fsemit_cRegTribISSQN : TpcnRegTribISSQN ;
    fsemit_IE : String ;
    fsemit_IM : String ;
    fsemit_indRatISSQN : TpcnindRatISSQN ;
    fside_CNPJ : String ;
    fside_numeroCaixa : Integer ;
    fsinfCFe_versaoDadosEnt : Real ;
    fside_tpAmb : TpcnTipoAmbiente ;
    fsPaginaDeCodigo: Word;
    fsArqSchema: String;
    fsXmlSignLib: TSSLXmlSignLib;

    function GetEhUTF8: Boolean;
    procedure SetEhUTF8(AValue: Boolean);
    procedure SetXmlSignLib(AValue: TSSLXmlSignLib);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
  published
    property infCFe_versaoDadosEnt : Real read fsinfCFe_versaoDadosEnt write fsinfCFe_versaoDadosEnt ;
    property ide_CNPJ : String  read fside_CNPJ write fside_CNPJ;
    property ide_numeroCaixa : Integer read fside_numeroCaixa write fside_numeroCaixa ;
    property ide_tpAmb : TpcnTipoAmbiente read fside_tpAmb write fside_tpAmb;
    property emit_CNPJ : String read fsemit_CNPJ write fsemit_CNPJ;
    property emit_IE   : String read fsemit_IE   write fsemit_IE;
    property emit_IM   : String read fsemit_IM   write fsemit_IM;
    property emit_cRegTrib: TpcnRegTrib read fsemit_cRegTrib write fsemit_cRegTrib ;
    property emit_cRegTribISSQN: TpcnRegTribISSQN read fsemit_cRegTribISSQN write fsemit_cRegTribISSQN ;
    property emit_indRatISSQN: TpcnindRatISSQN read fsemit_indRatISSQN write fsemit_indRatISSQN;
    property EhUTF8: Boolean read GetEhUTF8 write SetEhUTF8;
    property PaginaDeCodigo : Word read fsPaginaDeCodigo write fsPaginaDeCodigo;
    property ArqSchema : String read fsArqSchema write fsArqSchema ;
    property XmlSignLib: TSSLXmlSignLib read fsXmlSignLib write SetXmlSignLib;
  end;

  { TACBrSATConfigArquivos }

  TACBrSATConfigArquivos = class(TComponent)
  private
    fsOwner: TComponent;
    fsPrefixoArqCFe: String;
    fsPrefixoArqCFeCanc: String;
    fsSalvarCFe: Boolean;
    fsPastaCFeCancelamento: String;
    fsPastaCFeVenda: String;
    fsSalvarCFeCanc: Boolean;
    fsPastaEnvio: String;
    fsSalvarEnvio: Boolean;
    fsSepararPorCNPJ: Boolean;
    fsSepararPorAno: Boolean;
    fsSepararPorMes: Boolean;
    fsSepararPorDia: Boolean;
    fsSepararPorModelo: Boolean;
    function GetPastaCFeCancelamento: String;
    function GetPastaCFeVenda: String;
    function GetPastaEnvio: String;
    procedure SetPastaCFeCancelamento(const AValue: String);
    procedure SetPastaCFeVenda(const AValue: String);
    procedure SetPastaEnvio(const AValue: String);
    procedure SetSepararPorDia(const Value: Boolean);
    procedure SetSepararPorMes(const Value: Boolean);
    procedure SetSepararPorAno(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    function CalcPath(const APath: String; CNPJ: String; Data: TDateTime): String;
  published
    property SalvarCFe: Boolean read fsSalvarCFe write fsSalvarCFe default false;
    property SalvarCFeCanc: Boolean read fsSalvarCFeCanc write fsSalvarCFeCanc default false;
    property SalvarEnvio: Boolean read fsSalvarEnvio write fsSalvarEnvio default false;

    property SepararPorCNPJ: Boolean read fsSepararPorCNPJ write fsSepararPorCNPJ default False;
    property SepararPorModelo: Boolean read fsSepararPorModelo write fsSepararPorModelo default False;
    property SepararPorAno: Boolean read fsSepararPorAno write SetSepararPorAno default False;
    property SepararPorMes: Boolean read fsSepararPorMes write SetSepararPorMes default False;
    property SepararPorDia: Boolean read fsSepararPorDia write SetSepararPorDia default False;

    property PastaCFeVenda: String read GetPastaCFeVenda write SetPastaCFeVenda;
    property PastaCFeCancelamento: String read GetPastaCFeCancelamento write SetPastaCFeCancelamento;
    property PastaEnvio: String read GetPastaEnvio write SetPastaEnvio;

    property PrefixoArqCFe: String read fsPrefixoArqCFe write fsPrefixoArqCFe;
    property PrefixoArqCFeCanc: String read fsPrefixoArqCFeCanc write fsPrefixoArqCFeCanc;
  end;

  { TACBrSATRespostaClass }

  TACBrSATResposta = class
  private
    fcodigoDeErro: Integer;
    fnumeroSessao : Integer ;
    fcodigoDeRetorno : Integer ;
    fmensagemRetorno : String;
    fcodigoSEFAZ : Integer ;
    fmensagemSEFAZ : String;
    fRetornoLst : TStringList ;
    fRetornoStr : String ;
    procedure SetRetornoStr(const AValue : String) ;
  public
    constructor Create ;
    Destructor Destroy ; override ;
    procedure Clear ;

    property numeroSessao : Integer read fnumeroSessao ;
    property codigoDeRetorno : Integer read  fcodigoDeRetorno;
    property codigoDeErro : Integer read  fcodigoDeErro;
    property mensagemRetorno : String read fmensagemRetorno;
    property codigoSEFAZ : Integer read  fcodigoSEFAZ;
    property mensagemSEFAZ : String read fmensagemSEFAZ;
    property RetornoLst : TStringList read fRetornoLst ;
    property RetornoStr : String read fRetornoStr write SetRetornoStr ;
  end ;

  TACBrSATStatusLan = ( lanCONECTADO, lanNAO_CONECTADO ) ;
  TACBrSATNivelBateria = ( batALTO, batMEDIO, batBAIXO ) ;
  TACBrSATEstadoOperacao = ( opDESBLOQUEADO, opBLOQUEIO_SEFAZ,
                             opBLOQUEIO_CONTRIBUINTE, opBLOQUEIO_AUTONOMO,
                             opBLOQUEIO_DESATIVACAO );

  { TACBrSATStatus }

  TACBrSATStatus = class
  private
    fCERT_EMISSAO: TDateTime;
    fCERT_VENCIMENTO: TDateTime;
    fDH_ATUAL: TDateTime;
    fDH_CFe: TDateTime;
    fDH_ULTIMA: TDateTime;
    fESTADO_OPERACAO: TACBrSATEstadoOperacao;
    fLAN_MAC: String;
    fLISTA_FINAL: String;
    fLISTA_INICIAL: String;
    fMT_TOTAL: String;
    fMT_USADA: String;
    fNIVEL_BATERIA: TACBrSATNivelBateria;
    fNSERIE: String;
    fSTATUS_LAN: TACBrSATStatusLan;
    fULTIMO_CFe: String;
    fVER_LAYOUT: String;
    fVER_SB: String;
  public
    procedure Clear;
    function StatusLanToStr(const t: TACBrSATStatusLan ): string;
    function StrToStatusLan(var ok: boolean; const s: string): TACBrSATStatusLan ;
    function NivelBateriaToStr(const t: TACBrSATNivelBateria ): string;
    function StrToNivelBateria(var ok: boolean; const s: string): TACBrSATNivelBateria ;
    function EstadoOperacaoToStr(const t: TACBrSATEstadoOperacao ): string;
    function StrToEstadoOperacao(var ok: boolean; const s: string): TACBrSATEstadoOperacao ;

    property NSERIE: String read fNSERIE write fNSERIE;
    property LAN_MAC: String read fLAN_MAC write fLAN_MAC;
    property STATUS_LAN: TACBrSATStatusLan read fSTATUS_LAN
       write fSTATUS_LAN;
    property NIVEL_BATERIA: TACBrSATNivelBateria read fNIVEL_BATERIA
       write fNIVEL_BATERIA;
    property MT_TOTAL: String read fMT_TOTAL write fMT_TOTAL;
    property MT_USADA: String read fMT_USADA write fMT_USADA;
    property DH_ATUAL: TDateTime read fDH_ATUAL write fDH_ATUAL;
    property VER_SB: String read fVER_SB write fVER_SB;
    property VER_LAYOUT: String read fVER_LAYOUT write fVER_LAYOUT;
    property ULTIMO_CFe: String read fULTIMO_CFe write fULTIMO_CFe;
    property LISTA_INICIAL: String read fLISTA_INICIAL write fLISTA_INICIAL;
    property LISTA_FINAL: String read fLISTA_FINAL write fLISTA_FINAL;
    property DH_CFe: TDateTime read fDH_CFe write fDH_CFe;
    property DH_ULTIMA: TDateTime read fDH_ULTIMA write fDH_ULTIMA;
    property CERT_EMISSAO: TDateTime read fCERT_EMISSAO write fCERT_EMISSAO;
    property CERT_VENCIMENTO: TDateTime read fCERT_VENCIMENTO write fCERT_VENCIMENTO;
    property ESTADO_OPERACAO: TACBrSATEstadoOperacao read fESTADO_OPERACAO
       write fESTADO_OPERACAO;
  end;

  { TACBrSATClass }

   TACBrSATClass = class( TComponent )
   private
     function GetcodigoDeAtivacao : AnsiString ;
     function GetnumeroSessao : Integer ;
     function GetNomeDLL : string ;

     procedure ErroAbstract( const NomeProcedure : String ) ;
     function GetsignAC : AnsiString ;
   protected
     fpOwner : TComponent ;   { Componente ACBrSAT }
     fpModeloStr: String;

     function GetModeloStr: String; virtual ;

     property NomeDLL: String read GetNomeDLL ;

     property codigoDeAtivacao : AnsiString read GetcodigoDeAtivacao ;
     property signAC : AnsiString read GetsignAC ;
     property numeroSessao : Integer read GetnumeroSessao ;

     procedure LoadDLLFunctions ; virtual ;
     procedure UnLoadDLLFunctions ; virtual ;
     procedure FunctionDetectLibSAT(FuncName : String ;
       var LibPointer : Pointer) ; virtual ;
   public
     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     procedure Inicializar; virtual ;
     procedure DesInicializar ; virtual ;

     Property ModeloStr: String read GetModeloStr ;

     function AssociarAssinatura(const CNPJvalue, assinaturaCNPJs : AnsiString ):
       String ; virtual;
     function AtivarSAT( subComando : Integer; CNPJ: AnsiString; cUF : Integer )
       : String ; virtual;
     function AtualizarSoftwareSAT : String ; virtual;
     function BloquearSAT : String ; virtual;
     function CancelarUltimaVenda( chave, dadosCancelamento : AnsiString ) :
       String ; virtual;
     function ComunicarCertificadoICPBRASIL( certificado : AnsiString ) :
       String ; virtual;
     function ConfigurarInterfaceDeRede( dadosConfiguracao : AnsiString ) :
       String ; virtual;
     function ConsultarNumeroSessao( cNumeroDeSessao : Integer) : String ;
       virtual;
     function ConsultarSAT : String ; virtual;
     function ConsultarStatusOperacional : String ; virtual;
     function DesbloquearSAT : String ; virtual;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; virtual;
     function ExtrairLogs : String ; virtual;
     function TesteFimAFim( dadosVenda : AnsiString) : String ; virtual;
     function TrocarCodigoDeAtivacao( codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao : Integer; novoCodigo: AnsiString ) : String ; virtual;
     function ConsultarUltimaSessaoFiscal : String ; virtual;
   end;

implementation

Uses ACBrSAT, ACBrUtil.FilesIO, ACBrUtil.Base, ACBrUtil.Strings, ACBrConsts ;

{ TACBrSATConfigArquivos }

constructor TACBrSATConfigArquivos.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrSAT) then
    raise EACBrSATErro.Create('Dono de TACBrSATConfig deve ser TACBrSAT');

  inherited Create(AOwner);
  fsOwner := AOwner;

  Clear;
end;

destructor TACBrSATConfigArquivos.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrSATConfigArquivos.Clear;
begin
  fsPastaCFeCancelamento := '';
  fsPastaCFeVenda := '';

  fsSalvarCFe := False;
  fsSalvarCFeCanc := False;
  fsSalvarEnvio := False;

  fsSepararPorCNPJ := False;
  fsSepararPorModelo := False;
  fsSepararPorMes := False;
  fsSepararPorDia := False;

  fsPrefixoArqCFe := CPREFIXO_ArqCFe;
  fsPrefixoArqCFeCanc := CPREFIXO_ArqCFeCanc;
end;

function TACBrSATConfigArquivos.GetPastaCFeCancelamento: String;
begin
  if fsPastaCFeCancelamento = '' then
     if not (csDesigning in fsOwner.ComponentState) then
        fsPastaCFeCancelamento := ExtractFilePath( ParamStr(0) ) + CPastaCancelamentos ;

  Result := fsPastaCFeCancelamento ;
end;

function TACBrSATConfigArquivos.GetPastaCFeVenda: String;
begin
  if fsPastaCFeVenda = '' then
     if not (csDesigning in fsOwner.ComponentState) then
        fsPastaCFeVenda := ExtractFilePath( ParamStr(0) ) + CPastaVendas ;

  Result := fsPastaCFeVenda ;
end;

function TACBrSATConfigArquivos.GetPastaEnvio: String;
begin
  if fsPastaEnvio = '' then
     if not (csDesigning in fsOwner.ComponentState) then
        fsPastaEnvio := ExtractFilePath( ParamStr(0) ) + CPastaEnviados ;

  Result := fsPastaEnvio ;
end;

procedure TACBrSATConfigArquivos.SetPastaCFeCancelamento(const AValue: String);
begin
  fsPastaCFeCancelamento := PathWithoutDelim( AValue );
end;

procedure TACBrSATConfigArquivos.SetPastaCFeVenda(const AValue: String);
begin
  fsPastaCFeVenda := PathWithoutDelim( AValue );
end;

procedure TACBrSATConfigArquivos.SetPastaEnvio(const AValue: String);
begin
  fsPastaEnvio := PathWithoutDelim( AValue );
end;

procedure TACBrSATConfigArquivos.SetSepararPorDia(const Value: Boolean);
begin
  fsSepararPorDia := Value;
  if fsSepararPorDia then
    fsSepararPorMes := True;
end;

procedure TACBrSATConfigArquivos.SetSepararPorMes(const Value: Boolean);
begin
  fsSepararPorMes := Value;
  if not fsSepararPorMes then
    fsSepararPorDia := False;
end;

procedure TACBrSATConfigArquivos.SetSepararPorAno(const Value: Boolean);
begin
  fsSepararPorAno := Value;
end;

function TACBrSATConfigArquivos.CalcPath(const APath: String; CNPJ: String;
  Data: TDateTime): String;
var
  wDia, wMes, wAno: Word;
  Dir, Modelo, sAno, sMes, sDia: String;
begin
  if EstaVazio(APath) then
    Dir := PastaCFeVenda
  else
    Dir := APath;

  if SepararPorCNPJ then
  begin
    CNPJ := OnlyNumber(CNPJ);
    if EstaVazio(CNPJ) then
      CNPJ := OnlyNumber(TACBrSAT(fsOwner).Config.emit_CNPJ);

    if NaoEstaVazio(CNPJ) then
      Dir := PathWithDelim(Dir) + CNPJ;
  end;

  if SepararPorModelo then
  begin
    Modelo := TACBrSAT(fsOwner).GetNomeModeloCFe;
    Dir := PathWithDelim(Dir) + Modelo;
  end;

  if (SepararPorAno or SepararPorMes or SepararPorDia) then
  begin
    if Data = 0 then
      Data := Now;

    DecodeDate(Data, wAno, wMes, wDia);
    sDia := IntToStrZero(wDia, 2);
    sMes := IntToStrZero(wMes, 2);
    sAno := IntToStrZero(wAno, 4);
  end;

  if SepararPorAno then
    Dir := PathWithDelim(Dir) + sAno;

  if SepararPorMes then
  begin
    if SepararPorAno then
      Dir := PathWithDelim(Dir) + sMes
    else
      Dir := PathWithDelim(Dir) + sAno + sMes;

    if SepararPorDia then
      Dir := PathWithDelim(Dir) + sDia;
  end;

  with TACBrSAT(fsOwner) do
  begin
    if Assigned( OnCalcPath ) then
      OnCalcPath(Dir, CNPJ, Data);
  end;

  Result := PathWithDelim(Dir);
end;

{ TACBrSATStatus }

procedure TACBrSATStatus.Clear;
begin
  fCERT_EMISSAO     := 0;
  fCERT_VENCIMENTO  := 0;
  fDH_ATUAL         := 0;
  fDH_CFe           := 0;
  fDH_ULTIMA        := 0;
  fESTADO_OPERACAO  := opDESBLOQUEADO;
  fLAN_MAC          := '';
  fLISTA_FINAL      := '';
  fLISTA_INICIAL    := '';
  fMT_TOTAL         := '';
  fMT_USADA         := '';
  fNIVEL_BATERIA    := batALTO;
  fNSERIE           := '';
  fSTATUS_LAN       := lanCONECTADO;
  fULTIMO_CFe       := '';
  fVER_LAYOUT       := '';
  fVER_SB           := '';
end;

function TACBrSATStatus.StatusLanToStr(const t: TACBrSATStatusLan): string;
begin
  result := EnumeradoToStr(t, ['CONECTADO', 'NAO_CONECTADO'], [lanCONECTADO, lanNAO_CONECTADO]);
end;

function TACBrSATStatus.StrToStatusLan(var ok: boolean; const s: string
  ): TACBrSATStatusLan;
begin
  if (UpperCase(s) = 'CONECTADO') then
    Result := lanCONECTADO
  else
    Result := lanNAO_CONECTADO;
end;

function TACBrSATStatus.NivelBateriaToStr(const t: TACBrSATNivelBateria
  ): string;
begin
  result := EnumeradoToStr(t, ['ALTO', 'MEDIO', 'BAIXO'], [batALTO, batMEDIO, batBAIXO]);
end;

function TACBrSATStatus.StrToNivelBateria(var ok: boolean; const s: string
  ): TACBrSATNivelBateria;
begin
  result := StrToEnumerado(ok, s, ['ALTO', 'MEDIO', 'BAIXO'], [batALTO, batMEDIO, batBAIXO]);
end;

function TACBrSATStatus.EstadoOperacaoToStr(const t: TACBrSATEstadoOperacao
  ): string;
begin
  result := EnumeradoToStr(t,
                ['DESBLOQUEADO', 'BLOQUEIO_SEFAZ', 'BLOQUEIO_CONTRIBUINTE',
                 'BLOQUEIO_AUTONOMO', 'BLOQUEIO_DESATIVACAO'],
                [opDESBLOQUEADO, opBLOQUEIO_SEFAZ, opBLOQUEIO_CONTRIBUINTE,
                 opBLOQUEIO_AUTONOMO, opBLOQUEIO_DESATIVACAO]);
end;

function TACBrSATStatus.StrToEstadoOperacao(var ok: boolean; const s: string
  ): TACBrSATEstadoOperacao;
begin
  result := StrToEnumerado(ok, s,
                ['DESBLOQUEADO', 'BLOQUEIO_SEFAZ', 'BLOQUEIO_CONTRIBUINTE',
                 'BLOQUEIO_AUTONOMO', 'BLOQUEIO_DESATIVACAO'],
                [opDESBLOQUEADO, opBLOQUEIO_SEFAZ, opBLOQUEIO_CONTRIBUINTE,
                 opBLOQUEIO_AUTONOMO, opBLOQUEIO_DESATIVACAO]);
end;

{ TACBrSATRespostaClass }

procedure TACBrSATResposta.SetRetornoStr(const AValue : String) ;
var
  index : integer;
  AStr: String;
begin
{  ***** RETORNOS DO SAT POR COMANDO *****
AtivarSAT....................: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ, CSR
ComunicarCertificadoICPBRASIL: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
EnviarDadosVenda.............: numeroSessao, EEEEE, CCCC, mensagem, cod, mensagemSEFAZ, base64, timeStamp, chaveConsulta, valorTotalCFe, CPFCNPJValue, assinaturaQRCOD
CancelarUltimaVenda..........: numeroSessao, EEEEE, CCCC, mensagem, cod, mensagemSEFAZ, base64, timeStamp, chaveConsulta, valorTotalCFe, CPFCNPJValue, assinaturaQRCOD
ConsultarSAT.................: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
TesteFimAFim.................: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ, base64, timeStamp, numDocFiscal, chaveConsulta
ConsultarStatusOperacional...: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ, ConteudoRetorno
ConsultarNumeroSessao........: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ   (ou retorno da Sessão consultada)
ConfigurarInterfaceDeRede....: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
AssociarAssinatura...........: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
AtualizarSoftwareSAT.........: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
ExtrairLogs..................: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ, base64
BloquearSAT..................: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
DesbloquearSAT...............: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
TrocarCodigoDeAtivacao.......: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ
ConsultarUltimaSessaoFiscal..: numeroSessao, EEEEE, mensagem, cod, mensagemSEFAZ

}
  Clear;
  fRetornoStr := AValue;

  AddDelimitedTextToList(fRetornoStr, '|', fRetornoLst, #0);

  if fRetornoLst.Count > 1 then
  begin
    fnumeroSessao    := StrToIntDef( fRetornoLst[0], 0);
    fcodigoDeRetorno := StrToIntDef( fRetornoLst[1], 0);
  end;

  index := 2;
  if fRetornoLst.Count > index then
  begin
    AStr := Trim(fRetornoLst[index]);

    if (Length(AStr) = 4) and StrIsNumber(AStr) then //Enviar e Cancelar venda tem um campo a mais no inicio da resposta(CCCC)
    begin
      fcodigoDeErro := StrToIntDef(AStr, 0);
      index := 3
    end;
  end;

  if fRetornoLst.Count > index+2 then
  begin
    fmensagemRetorno := Trim(fRetornoLst[index]);
    fcodigoSEFAZ     := StrToIntDef( fRetornoLst[index+1], 0);
    fmensagemSEFAZ   := Trim(fRetornoLst[index+2]);
  end
  else
    fmensagemRetorno := AValue;
end;

constructor TACBrSATResposta.Create ;
begin
  inherited Create;
  fRetornoLst := TStringList.Create;
  Clear;
end ;

destructor TACBrSATResposta.Destroy ;
begin
  fRetornoLst.Free;
  inherited Destroy;
end ;

procedure TACBrSATResposta.Clear ;
begin
  fRetornoLst.Clear;
  fRetornoStr      := '';
  fnumeroSessao    := 0;
  fcodigoDeRetorno := 0;
  fcodigoDeErro    := 0;
  fmensagemRetorno := '';
  fcodigoSEFAZ := 0;
  fmensagemSEFAZ := '';
end ;

{ TACBrSATConfig }

function TACBrSATConfig.GetEhUTF8: Boolean;
begin
  Result := (fsPaginaDeCodigo = CUTF8CodPage);
end;

procedure TACBrSATConfig.SetEhUTF8(AValue: Boolean);
begin
   if AValue then
     fsPaginaDeCodigo := CUTF8CodPage
   else
   begin
     if fsPaginaDeCodigo = CUTF8CodPage then
       fsPaginaDeCodigo := 0;
   end ;

end;

procedure TACBrSATConfig.SetXmlSignLib(AValue: TSSLXmlSignLib);
begin
  TACBrSAT(fsOwner).SSL.SSLXmlSignLib := AValue;
  fsXmlSignLib := AValue;
end;

constructor TACBrSATConfig.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrSAT) then
    raise EACBrSATErro.Create('Dono de TACBrSATConfig deve ser TACBrSAT');

  inherited Create(AOwner);
  fsOwner := AOwner;

  Clear;
end ;

destructor TACBrSATConfig.Destroy ;
begin
  inherited Destroy ;
end ;

procedure TACBrSATConfig.Clear ;
begin
  fsemit_CNPJ             := '' ;
  fsemit_cRegTrib         := RTSimplesNacional;
  fsemit_cRegTribISSQN    := RTISSMicroempresaMunicipal;
  fsemit_IE               := '' ;
  fsemit_IM               := '' ;
  fsemit_indRatISSQN      := irSim ;
  fside_CNPJ              := '' ;
  fside_numeroCaixa       := 0 ;
  fside_tpAmb             := taHomologacao;
  fsinfCFe_versaoDadosEnt := cversaoDadosEnt;

  fsArqSchema  := '';
  fsXmlSignLib := xsNone;
end ;

{ EACBrSATErro }

constructor EACBrSATErro.Create(const msg : string) ;
begin
  inherited Create( ACBrStr(msg) );
end ;

{ TACBrSATClass }

constructor TACBrSATClass.Create(AOwner : TComponent) ;
begin
  if not (AOwner is TACBrSAT) then
     raise EACBrSATErro.create( cACBrSATClassCreateException );

  inherited Create(AOwner) ;

  fpOwner := AOwner ;
  fpModeloStr := 'Não Definido' ;
end ;

destructor TACBrSATClass.Destroy ;
begin
  UnLoadDLLFunctions;
  inherited Destroy ;
end ;

procedure TACBrSATClass.Inicializar ;
begin
  LoadDLLFunctions;
end ;

procedure TACBrSATClass.DesInicializar ;
begin
  UnLoadDLLFunctions;
end ;

function TACBrSATClass.GetNomeDLL : string ;
begin
  Result := TACBrSAT(fpOwner).NomeDLL;
end;

procedure TACBrSATClass.ErroAbstract(const NomeProcedure : String) ;
begin
  raise EACBrSATErro.create( Format( cACBrSATCMDInvalidoException,
                                     [NomeProcedure, ModeloStr] )) ;
end ;

function TACBrSATClass.GetsignAC : AnsiString ;
begin
  Result := TACBrSAT(fpOwner).signAC;
end;

function TACBrSATClass.GetcodigoDeAtivacao : AnsiString ;
begin
  Result := TACBrSAT(fpOwner).codigoDeAtivacao;
end;

function TACBrSATClass.GetnumeroSessao : Integer ;
begin
  Result := TACBrSAT(fpOwner).numeroSessao;
end;

procedure TACBrSATClass.LoadDLLFunctions;
begin
  ErroAbstract('LoadDLLFunctions');
end;

procedure TACBrSATClass.UnLoadDLLFunctions ;
begin
  UnLoadLibrary( NomeDLL );
end ;

function TACBrSATClass.AssociarAssinatura(const CNPJvalue,
  assinaturaCNPJs : AnsiString) : String ;
begin
  ErroAbstract('AssociarAssinatura');
  Result := '';
end ;

function TACBrSATClass.AtivarSAT(subComando : Integer ; CNPJ : AnsiString ;
  cUF : Integer) : String ;
begin
  ErroAbstract('AtivarSAT');
  Result := '';
end ;

function TACBrSATClass.AtualizarSoftwareSAT : String ;
begin
  ErroAbstract('AtualizarSoftwareSAT');
  Result := '';
end ;

function TACBrSATClass.BloquearSAT : String ;
begin
  ErroAbstract('BloquearSAT');
  Result := '';
end ;

function TACBrSATClass.CancelarUltimaVenda(chave, dadosCancelamento : AnsiString
  ) : String ;
begin
  ErroAbstract('CancelarUltimaVenda');
  Result := '';
end ;

function TACBrSATClass.ComunicarCertificadoICPBRASIL(certificado : AnsiString
  ) : String ;
begin
  ErroAbstract('ComunicarCertificadoICPBRASIL');
  Result := '';
end ;

function TACBrSATClass.ConfigurarInterfaceDeRede(dadosConfiguracao : AnsiString
  ) : String ;
begin
  ErroAbstract('ConfigurarInterfaceDeRede');
  Result := '';
end ;

function TACBrSATClass.ConsultarNumeroSessao(cNumeroDeSessao : Integer
  ) : String ;
begin
  ErroAbstract('ConsultarNumeroSessao');
  Result := '';
end ;

function TACBrSATClass.ConsultarSAT : String ;
begin
  ErroAbstract('ConsultarSAT');
  Result := '';
end ;

function TACBrSATClass.ConsultarStatusOperacional : String ;
begin
  ErroAbstract('ConsultarStatusOperacional');
  Result := '';
end ;

function TACBrSATClass.ConsultarUltimaSessaoFiscal: String;
begin
  ErroAbstract('ConsultarUltimaSessaoFiscal');
  Result := '';
end;

function TACBrSATClass.DesbloquearSAT : String ;
begin
  ErroAbstract('DesbloquearSAT');
  Result := '';
end ;

function TACBrSATClass.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
begin
  ErroAbstract('EnviarDadosVenda');
  Result := '';
end ;

function TACBrSATClass.ExtrairLogs : String ;
begin
  ErroAbstract('ExtrairLogs');
  Result := '';
end ;

function TACBrSATClass.TesteFimAFim(dadosVenda : AnsiString) : String ;
begin
  ErroAbstract('TesteFimAFim');
  Result := '';
end ;

function TACBrSATClass.TrocarCodigoDeAtivacao(
  codigoDeAtivacaoOuEmergencia: AnsiString; opcao: Integer; novoCodigo: AnsiString
  ): String;
begin
  ErroAbstract('TrocarCodigoDeAtivacao');
  Result := '';
end ;

function TACBrSATClass.GetModeloStr : String ;
begin
  Result := fpModeloStr;
end ;

procedure TACBrSATClass.FunctionDetectLibSAT(FuncName : String ;
  var LibPointer : Pointer) ;
var
  sLibName: String;
begin
  if not Assigned( LibPointer )  then
  begin
    sLibName := NomeDLL;
    if ExtractFilePath(sLibName) <> '' then
      if not FileExists(sLibName) then
        raise EACBrSATErro.Create( 'Arquivo não encontrado: '+sLibName );

    if not FunctionDetect( sLibName, FuncName, LibPointer) then
    begin
       LibPointer := NIL ;
       raise EACBrSATErro.Create( Format(cACBrSATFuncaoNaoEncontrada, [FuncName,sLibName]) ) ;
    end ;
  end ;
end ;

end.

