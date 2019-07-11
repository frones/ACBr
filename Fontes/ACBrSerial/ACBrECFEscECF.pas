{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 20/10/2012:  Daniel Simoes de Almeida
|*   Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
|*   http://www1.fazenda.gov.br/confaz/confaz/atos/atos_cotepe/2011/ac042_11.htm
******************************************************************************}

{$I ACBr.inc}

unit ACBrECFEscECF ;

interface
uses Classes,
    ACBrECFClass, ACBrDevice;

const
    cEscECFMaxBuffer = 512 ;
    cNumFalhasMax = 5;
    cEsperaWAK = 50;

type

{ TACBrECFEscECFRET }

TACBrECFEscECFRET = class
private
   fsECF: Byte;
   fsFabricante: Byte;
   fsFisco: Byte;
   fsRET: AnsiString;
   fsSPR: Byte;
   procedure SetRET(const AValue: AnsiString);
 public
    constructor Create;
    property ECF        : Byte read fsECF write fsECF;
    property Fisco      : Byte read fsFisco write fsFisco;
    property SPR        : Byte read fsSPR write fsSPR;
    property Fabricante : Byte read fsFabricante write fsFabricante;

    property RET : AnsiString read fsRET write SetRET ;

    procedure Clear;
end;


{ TACBrECFEscECFComando }

TACBrECFEscECFComando = class
  private
    fsCMD     : Byte ;
    fsEXT     : Byte ;
    fsSEQ     : Byte ;
    fsParams  : TStringList ;
    fsTimeOut : Integer;

    function GetComando: AnsiString;
    procedure SetCMD(const Value: Byte);
 public
    constructor Create ;
    destructor Destroy ; override ;

    property CMD : Byte read fsCMD write SetCMD ;
    property EXT : Byte read fsEXT write fsEXT ;
    property SEQ : Byte read fsSEQ write fsSEQ ;
    property TimeOut : Integer read fsTimeOut write fsTimeOut ;

    property Comando : AnsiString  read GetComando ;
    property Params  : TStringList read fsParams ;

    Procedure AddParamString(const AString : AnsiString) ;
    Procedure AddParamInteger(AInteger : Integer) ;
    Procedure AddParamDouble(ADouble : Double; Decimais: Byte = 2) ;
    Procedure AddParamDateTime( ADateTime: TDateTime; Tipo : Char = 'D';
                                const FlagHV : String = '' ) ;
 end ;

{ TACBrECFEscECFResposta }

TACBrECFEscECFResposta = class
  private
    fsResposta : AnsiString ;
    fsParams   : TStringList ;
    fsRET      : TACBrECFEscECFRET;
    fsSEQ      : Byte ;
    fsCMD      : Byte ;
    fsEXT      : Byte ;
    fsCAT      : Byte ;
    fsTBR      : Integer ;
    fsBRS      : AnsiString ;
    fsCHK      : Byte ;

    procedure SetResposta(const AValue: AnsiString);
 public
    constructor Create ;
    destructor Destroy ; override ;

    procedure Clear( ClearParams: Boolean = True ) ;

    property Resposta : AnsiString  read fsResposta write SetResposta ;
    property Params   : TStringList read fsParams ;
    property SEQ      : Byte        read fsSEQ ;
    property CMD      : Byte        read fsCMD ;
    property EXT      : Byte        read fsEXT ;
    property CAT      : Byte        read fsCAT write fsCAT;
    property RET      : TACBrECFEscECFRET read fsRET ;
    property TBR      : Integer     read fsTBR write fsTBR;
    property BRS      : AnsiString  read fsBRS write fsBRS;
    property CHK      : Byte        read fsCHK ;
 end ;


TACBrECFEscECF = class;

{ TACBrECFEscECFProtocolo }

TACBrECFEscECFProtocolo = class
  private
     fsFalhas: Byte;
     fsACK: Boolean;
     fsWAKCounter: Integer;
     fsTimeOutStatus: TDateTime;
     fsSincronizou: Boolean;
     fsTentativasSincronizacao: Integer;
     fsSPR: Byte;

     procedure Sincronizar;
  protected
    fpECFEscECF: TACBrECFEscECF;

    function PreparaCmd(const CmdExtBcd: AnsiString): AnsiString;
  public
    constructor Create(AECFEscECF: TACBrECFEscECF); virtual;
    procedure Ativar ; virtual;
    procedure Desativar ; virtual;
    function EnviaComando_ECF( ACmd : AnsiString = '') : AnsiString ; virtual;
    function VerificaFimLeitura(var Retorno: AnsiString;
      var TempoLimite: TDateTime): Boolean; virtual;
end;

{ TACBrECFEscECFProtocoloEpsonDLL }

TACBrECFEscECFProtocoloEpsonDLL = class( TACBrECFEscECFProtocolo )
  private
     xEPSON_Serial_Abrir_Porta : function (dwVelocidade:Integer;
        wPorta:Integer):Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
     xEPSON_Serial_Fechar_Porta : function : Integer;
        {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
     xEPSON_Send_From_FileEX : function (pszLineIn:AnsiString;
        pszLineOut:PAnsiChar ) : Integer;
        {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

     BufferOut : array [0..65536] of AnsiChar;  // 64kb

     procedure LoadDLLFunctions;
  public
    constructor Create(AECFEscECF: TACBrECFEscECF); override;
    procedure Ativar ; override;
    procedure Desativar ; override;
    function EnviaComando_ECF( ACmd : AnsiString = '') : AnsiString ; override;
    function VerificaFimLeitura(var Retorno: AnsiString;
      var TempoLimite: TDateTime): Boolean; override;
end;

 { Classe filha de TACBrECFClass com implementaçao para EscECF }
TACBrECFEscECF = class( TACBrECFClass )
 private
    fsPAF            : AnsiString ;
    fsNumVersao      : String ;
    fsVersaoEscECF   : String ;
    fsNumECF         : String ;
    fsNumCRO         : String ;
    fsNumLoja        : String ;
    fsDataHoraSB     : TDateTime;
    fsEscECFComando  : TACBrECFEscECFComando;
    fsEscECFResposta : TACBrECFEscECFResposta;
    fsEscECFProtocolo: TACBrECFEscECFProtocolo;
    fsMarcaECF       : String ;
    fsModeloECF      : String ;
    fsEmPagamento    : Boolean ;
    fsNomeArqMemoria : String ;
    fsArqMemoria     : String ;
    fsDeviceParams   : String;

    procedure EnviaConsumidor;
    Function AjustaDescricao( const ADescricao: String ): String;

    Procedure SalvaRespostasMemoria( AtualizaVB: Boolean = True );
    Procedure LeRespostasMemoria;

    function CriarECFClassPorMarca : TACBrECFClass;
    procedure DestruirECFClass( AECFClass: TACBrECFClass );

    procedure AjustaComandosControleImpressao(var Linha: AnsiString);
    procedure AjustaLinhasColunas(var Linhas: AnsiString; MaxLin: Integer = 0);

 protected
    procedure AtivarDevice;
    Function TraduzErroMsg(EscECFResposta: TACBrECFEscECFResposta) : String;

    function VerificaFimLeitura(var Retorno: AnsiString;
      var TempoLimite: TDateTime): Boolean; override;

    function GetModeloStr: String; override ;
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumECF: String; override ;
    function GetNumLoja: String; override ;
    function GetNumCRO: String; override ;
    function GetNumSerie: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;

    function GetNumReducoesZRestantes: String; override;

    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetIM: String; override ;
    function GetCliche: AnsiString; override ;
    function GetUsuarioAtual: String; override ;
    function GetDataHoraSB: TDateTime; override ;
    function GetSubModeloECF: String ; override ;

    function GetPAF: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetGrandeTotal: Double; override ;
    function GetNumCCF: String; override ;
    function GetNumGNF: String; override ;
    function GetNumGRG: String; override ;
    function GetNumCDC: String; override ;
    function GetNumCFC: String; override ;
    function GetNumGNFC: String; override ;
    function GetNumCRZ: String; override ;
    function GetNumCFD: String; override ;
    function GetNumNCN: String; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalTroco: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;
    function GetTotalAcrescimosISSQN: Double; override ;
    function GetTotalCancelamentosISSQN: Double; override ;
    function GetTotalDescontosISSQN: Double; override ;
    function GetTotalIsencaoISSQN: Double; override ;
    function GetTotalNaoTributadoISSQN: Double; override ;
    function GetTotalSubstituicaoTributariaISSQN: Double; override ;
    {TODO (não encontrado):
    function GetTotalAcrescimosOPNF: Double; override ;
    function GetTotalCancelamentosOPNF: Double; override ;
    function GetTotalDescontosOPNF: Double; override ;
    }
    function GetNumCOOInicial: String; override ;

    function GetNumUltimoItem: Integer; override ;

    function GetDadosUltimaReducaoZ: String; override ;
    procedure BematechObtemDadosUltimaReducaoZDeLeituraMemoriaFiscal(ANumCRZ: Integer);

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;

    function GetParamDescontoISSQN: Boolean; override;
    function GetTipoUltimoDocumento : TACBrECFTipoDocumento ; override ;

    { TODO (não encontrado): function GetTipoUltimoDocumento : TACBrECFTipoDocumento ; override ; }
 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    property NomeArqMemoria : String read fsNomeArqMemoria write fsNomeArqMemoria;

    property EscECFComando   : TACBrECFEscECFComando  read fsEscECFComando ;
    property EscECFResposta  : TACBrECFEscECFResposta read fsEscECFResposta ;
    property EscECFProtocolo : TACBrECFEscECFProtocolo read fsEscECFProtocolo;

    Function EnviaComando_ECF( cmd : AnsiString = '' ) : AnsiString ; override ;

    { Aliquotas de ICMS }
    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota ; override ;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  overload ; override;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;

    procedure CarregaTotalizadoresNaoTributados ; override;
    procedure LerTotaisTotalizadoresNaoTributados ; override;

    { Formas de Pagamento }
    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;
    function AchaFPGDescricao( Descricao : String;
       BuscaExata : Boolean = False;
       IgnorarCase : Boolean = True;
       IgnorarAcentos : Boolean = False) : TACBrECFFormaPagamento ; override ;

    { Relatório Gerencial (RG) }
    procedure CarregaRelatoriosGerenciais ; override ;
    procedure LerTotaisRelatoriosGerenciais ; override ;
    Procedure ProgramaRelatorioGerencial( var Descricao: String;
       Posicao : String = '') ; override ;
    function AchaRGDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True ) :
       TACBrECFRelatorioGerencial ; override ;

    { Comprovantes Nao Fiscais (CNF) }
    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;
    function AchaCNFDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True ) :
       TACBrECFComprovanteNaoFiscal ; override ;

    { Cupom Fiscal }
    Procedure AbreCupom ; override ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1) ; override ;
    Procedure DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo : Double = 0;
       DescontoAcrescimo : String = 'D'; TipoDescontoAcrescimo : String = '%';
       NumItem : Integer = 0 ) ;  override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString = '' ) ;  override ;
    procedure CancelaDescontoAcrescimoSubTotal(TipoAcrescimoDesconto: Char) ;
       override ;{ A -> Acrescimo D -> Desconto }
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    procedure EstornaPagamento(const CodFormaPagtoEstornar,
      CodFormaPagtoEfetivar : String; const Valor: Double;
      Observacao : AnsiString = '') ; override ;

    { Para quebrar linhas nos parametros Observacao use #10 ou chr(10),
      Geralmente o ECF aceita no máximo 8 linhas }
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;
    procedure CancelaItemVendidoParcial( NumItem : Integer; Quantidade : Double) ; override ;
    procedure CancelaDescontoAcrescimoItem( NumItem : Integer;
      TipoAcrescimoDesconto: String = 'D') ; override ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaItemNaoFiscal(const AItem: Integer); override;

    procedure Sangria( const Valor: Double;  Obs : AnsiString;
       DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer ) ; override ;
    procedure Suprimento( const Valor: Double; Obs : AnsiString;
       DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer ) ; override ;

    Function EstornaCCD( const Todos: Boolean = True) : Integer; overload; override;
    Function EstornaCCD( NumCupomFiscal: Integer; ADiante: Boolean = True ) : Integer; reintroduce; overload;

    { Gaveta de dinheiro }
    Procedure AbreGaveta  ; override ;

    { Relatorios }
    Procedure LeituraX ; override ;
    Procedure LeituraXSerial( Linhas : TStringList) ; override ;
    Procedure ReducaoZ( DataHora : TDateTime = 0 ) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;

    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;

    Procedure SegundaViaVinculado; override;
    procedure ReimpressaoVinculado; override;

    Procedure FechaRelatorio ; override ;
    Procedure CortaPapel( const CorteParcial : Boolean = false) ; override ;

    { TODO: Cheques
    Procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ; override ;
    Procedure CancelaImpressaoCheque ; override ;
    Function LeituraCMC7 : AnsiString ; override ;
    function GetChequePronto: Boolean; override ;
    }

    { Utilitarios e Diversos }
    Procedure MudaHorarioVerao ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure MudaArredondamento( Arredondar : Boolean ) ; override ;

    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; overload ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False ); overload ; override ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False ) ;
       overload ; override ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal: Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ;
       overload ; override ;

    Procedure LeituraMFDSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;
    Procedure LeituraMFDSerial( COOInicial, COOFinal : Integer;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;

    Procedure EspelhoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ; override ;
    Procedure EspelhoMFD_DLL( COOInicial, COOFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ; override ;
    Procedure ArquivoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD  ) ; overload ; override ;
    Procedure ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD;
       TipoContador: TACBrECFTipoContador = tpcCOO ) ; overload ; override ;

    procedure PafMF_GerarCAT52(const DataInicial, DataFinal: TDateTime;
      const DirArquivos: String; NumeroSerie: String = ''); override;

    Procedure ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString); override;
    Procedure ArquivoMFD_Binario_DLL(Tipo: TACBrECFTipoDownloadMFD; const NomeArquivo: AnsiString;
      StrInicial, StrFinal: AnsiString); override;

    Procedure IdentificaOperador(Nome : String); override;
    Procedure IdentificaPAF( NomeVersao, MD5 : String) ; override ;

    function TraduzirTag(const ATag: AnsiString): AnsiString; override;
    function TraduzirTagBloco(const ATag, Conteudo: AnsiString): AnsiString; override;

    Function RetornaInfoECF( Registrador: String) : AnsiString; override ;

    Function CapturaXMLCupom( const Inicial, Final: String; Tipo: Integer = 2 ): AnsiString;

    function IsBematech: Boolean;
    function IsEpson: Boolean;
    function IsDaruma: Boolean;

    property MarcaECF: String read fsMarcaECF;
 end ;

implementation
Uses
  SysUtils, Math,
  {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
  synautil,
  ACBrECF, ACBrECFBematech, ACBrECFEpson, ACBrECFDaruma, ACBrConsts, ACBrUtil;

{ TACBrECFEscECFProtocoloEpsonDLL }

constructor TACBrECFEscECFProtocoloEpsonDLL.Create(AECFEscECF: TACBrECFEscECF);
begin
  inherited Create(AECFEscECF);

  xEPSON_Serial_Abrir_Porta := Nil;
  xEPSON_Serial_Fechar_Porta := Nil;
  xEPSON_Send_From_FileEX := Nil;

  LoadDLLFunctions ;
end;

procedure TACBrECFEscECFProtocoloEpsonDLL.Ativar;
Var
  Resp : Integer ;
begin
  with fpECFEscECF do
  begin
    PaginaDeCodigo := 852;

    GravaLog( '   xEPSON_Serial_Abrir_Porta( 115200, 0 )' );
    Resp := xEPSON_Serial_Abrir_Porta( 115200, 0 ) ;  // 0 = USB
    if Resp <> 0 then
       raise EACBrECFERRO.Create( 'Erro: '+IntToStr(Resp)+' ao abrir a Porta: '+
                                  Device.Porta+', usando: '+cLIB_Epson );

    AtivarDevice ;  // Apenas para que fpAtivo seja ligado
  end;
end;

procedure TACBrECFEscECFProtocoloEpsonDLL.Desativar;
Var
  Resp : Integer ;
begin
  with fpECFEscECF do
  begin
    GravaLog( '   xEPSON_Serial_Fechar_Porta' );
    Resp := xEPSON_Serial_Fechar_Porta ;
    if Resp <> 0 then
       raise EACBrECFERRO.Create( 'Erro: '+IntToStr(Resp)+' ao Fechar a PortaPorta: '+
                                    Device.Porta+', usando: '+cLIB_Epson );
  end;

  inherited Desativar;
end;

function TACBrECFEscECFProtocoloEpsonDLL.EnviaComando_ECF(ACmd: AnsiString
  ): AnsiString;
var
  I: Integer;
  Resp : Integer ;
  CmdResp: AnsiString;
  SL: TStringList;
  ErroMsg: String;
begin
  Result := '';

  with fpECFEscECF do
  begin
    if ACmd <> '' then
      ACmd := PreparaCmd(ACmd) ;  // Ajusta e move para EscECFcomando

    EscECFResposta.Clear( True ) ;       // Zera toda a Resposta

    ACmd := IntToHex(EscECFComando.CMD, 2) + '|';
    For I := 0 to EscECFComando.Params.Count-1 do
      ACmd := ACmd + StringToBinaryString( AnsiString(EscECFComando.Params[I]) ) + '|';

    BufferOut[0] := #0; // Zera Buffer de Saida
    ACmd := ReplaceString(ACmd, NUL, '[NULL]');
    //ACmd := ChangeLineBreak(ACmd, LF);

    GravaLog( '   xEPSON_Send_From_FileEX -> '+ACmd, True );
    Resp := xEPSON_Send_From_FileEX( ACmd, BufferOut ) ;

    ComandoEnviado  := ACmd ;
    RespostaComando := TrimRight( BufferOut );
    CmdResp := RespostaComando;
    Result  := RespostaComando;

    GravaLog( '      Resp: '+IntToStr(Resp)+'  Retorno:'+CmdResp, True );

    if (Resp <> 0) and (CmdResp = '') then
      raise EACBrECFERRO.Create( 'Erro: '+IntToStr(Resp)+' ao executar EPSON_Send_From_FileEX' );

    SL := TStringList.Create;
    try
      AddDelimitedTextToList( CmdResp, '|', SL, #0);
      if SL.Count < 5 then
        raise EACBrECFCMDInvalido.Create('Resposta Inválida de EPSON_Send_From_FileEX');

      EscECFResposta.CAT            := StrToInt(SL[0]);
      EscECFResposta.RET.ECF        := StrToInt(SL[1]);
      EscECFResposta.RET.Fisco      := StrToInt(SL[2]);
      EscECFResposta.RET.SPR        := StrToInt(SL[3]);
      EscECFResposta.RET.Fabricante := StrToInt(SL[4]);

      I := PosAt('|',CmdResp,5);
      if I > 0 then
        EscECFResposta.BRS := copy(CmdResp, I+1, Length(CmdResp));

      EscECFResposta.TBR := Length(EscECFResposta.BRS);

      if SL.Count > 5 then
      begin
        For I := 5 to SL.Count-1 do
          EscECFResposta.Params.Add(SL[I]);
      end;

      ErroMsg := '' ;
      if EscECFResposta.CAT > 0 then
        ErroMsg := TraduzErroMsg(EscECFResposta) ;

      if ErroMsg <> '' then
      begin
        ErroMsg := ACBrStr('Erro retornado pela Impressora: '+ModeloStr+
                            sLineBreak+sLineBreak + ErroMsg ) ;

        if (EscECFResposta.CAT = 12) then
          DoOnErrorSemPapel
        else
          raise EACBrECFSemResposta.create(ErroMsg) ;
      end
      else
        Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }
    finally
      SL.Free;
    end;
  end;
end;

function TACBrECFEscECFProtocoloEpsonDLL.VerificaFimLeitura(
  var Retorno: AnsiString; var TempoLimite: TDateTime): Boolean;
begin
  Result := True;
end;

procedure TACBrECFEscECFProtocoloEpsonDLL.LoadDLLFunctions ;
  procedure EpsonFunctionDetect( FuncName: String; var LibPointer: Pointer ) ;
  var
    sLibName: string;
  begin
    if not Assigned( LibPointer )  then
    begin
      with fpECFEscECF do
      begin
        // Verifica se exite o caminho das DLLs
        if Length(PathDLL) > 0 then
          sLibName := PathWithDelim(PathDLL);

        // Concatena o caminho se exitir mais o nome da DLL.
        sLibName := sLibName + cLIB_Epson;

        if not FunctionDetect( sLibName, FuncName, LibPointer) then
        begin
          LibPointer := NIL ;
          raise EACBrECFERRO.Create( ACBrStr( 'Erro ao carregar a função:'+FuncName+' de: '+cLIB_Epson ) ) ;
        end ;
      end;
    end ;
  end ;
begin
   EpsonFunctionDetect('EPSON_Serial_Abrir_Porta', @xEPSON_Serial_Abrir_Porta);
   EpsonFunctionDetect('EPSON_Serial_Fechar_Porta', @xEPSON_Serial_Fechar_Porta);
   EpsonFunctionDetect('EPSON_Send_From_FileEX', @xEPSON_Send_From_FileEX);
end ;


{ TACBrECFEscECFProtocolo }

constructor TACBrECFEscECFProtocolo.Create(AECFEscECF: TACBrECFEscECF);
begin
  inherited create;

  fpECFEscECF := AECFEscECF;

  fsFalhas            := 0;
  fsACK               := False;
  fsWAKCounter        := 0;
  fsSincronizou       := False;
  fsTentativasSincronizacao := 0;
  fsTimeOutStatus     := 0;
  fsSPR               := 0;
end;

procedure TACBrECFEscECFProtocolo.Sincronizar;
var
  Resp: AnsiString;
begin
  with fpECFEscECF do
  begin
    fsFalhas := 0;
    while (not fsSincronizou) and (fsFalhas <= cNumFalhasMax) do
    begin
      GravaLog( '    Sincronismo TX -> '+SYN, True);
      Device.Serial.SendByte( ord(SYN) );
      Resp := Device.Serial.RecvBufferStr(2,2000);
      GravaLog( '    Sincronismo RX <- '+Resp, True);

      if Length(Resp) = 2 then
      begin
        if Resp[1] = SYN then
        begin
          EscECFComando.SEQ := ord(Resp[2])+1;
          fsSincronizou := True;
        end;
      end;

      if not fsSincronizou then
      begin
        Inc( fsFalhas ) ;
        GravaLog('     Falha SYN: '+IntToStr(fsFalhas));
        Device.Serial.Purge;
        Sleep(100);
      end;
    end;
  end;
end;

function TACBrECFEscECFProtocolo.PreparaCmd(const CmdExtBcd: AnsiString): AnsiString;
Var
  CMD, EXT : Byte ;
  BCD : AnsiString ;
begin
  if Length(CmdExtBcd) < 7 then
     raise EACBrECFERRO.Create(ACBrStr('Comando EscECF inválido')) ;

  CMD := ord( CmdExtBcd[1] ) ;
  EXT := ord( CmdExtBcd[2] ) ;
  BCD := copy( CmdExtBcd, 3, Length(CmdExtBcd) ) ;

  if (CMD = 255) and (EXT = 0) then
     raise EACBrECFERRO.Create(ACBrStr('Erro ! CMD = 255 e EXT = 0')) ;

  if (CMD <> 255) and (EXT <> 0) then
     raise EACBrECFERRO.Create(ACBrStr('Erro ! EXT deve ser 0 quando comando <> 255')) ;

  with fpECFEscECF do
  begin
    EscECFComando.CMD := CMD ;
    EscECFComando.EXT := EXT ;
    EscECFComando.Params.Text := BCD ;

    Result := EscECFComando.Comando ;
  end;
end;

function TACBrECFEscECFProtocolo.EnviaComando_ECF(ACmd: AnsiString): AnsiString;
Var
  ErroMsg : String ;
  OldTimeOut : Integer ;
begin
  with fpECFEscECF do
  begin
    fsWAKCounter := 0;
    fsTimeOutStatus := 0;
    Sincronizar;

    if ACmd <> '' then
       ACmd := PreparaCmd(ACmd) ;  // Ajusta e move para EscECFcomando

    EscECFResposta.Clear( True ) ;       // Zera toda a Resposta
    ACmd := EscECFComando.Comando ;

    if (fsTentativasSincronizacao > 0) then
      GravaLog( '         Status TX -> '+ACmd, True);

    fsACK           := False;
    fsFalhas        := 0 ;
    fsSPR           := 0 ;
    Result          := '' ;
    ErroMsg         := '' ;
    ComandoEnviado  := '' ;
    RespostaComando := '' ;
    OldTimeOut      := TimeOut ;
    TimeOut         := max(EscECFComando.TimeOut, TimeOut) ;

    try
       Device.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }
       Device.Serial.Purge ;                   { Limpa a Porta }

       while ComandoEnviado = '' do
       begin
          Device.Serial.Purge ;                   { Limpa a Porta }

          if not TransmiteComando( ACmd ) then
             continue ;

          ComandoEnviado := ACmd ;
       end ;

       { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
         falha na leitura LeResposta dispara Exceçao.
         Resposta fica gravada na váriavel "fpRespostaComando" }
       LeResposta ;

       with EscECFResposta do
       begin
         GravaLog( '            Resposta: SEQ:'+IntToStr(SEQ)+' CMD:'+IntToStr(CMD)+
           ' EXT:'+IntToStr(EXT)+ ' CAT:'+IntToStr(CAT)+ ' RET:'+RET.RET+
           ' TBR:'+IntToStr(TBR)+ ' BRS:"'+BRS+'" CHK:'+IntToStr(CHK), True );
       end;

       EscECFComando.SEQ := EscECFComando.SEQ + 1;

       Try
          //EscECFResposta.Resposta := fpRespostaComando ;
          // Resposta já foi atribuida em VerificaFimLeitura();

          ErroMsg := '' ;
          if EscECFResposta.CAT > 0 then
             ErroMsg := TraduzErroMsg(EscECFResposta) ;
       except
          On E : Exception do
          begin
             ErroMsg := 'Resposta do ECF inválida' + sLineBreak + E.Message ;
          end ;
       end ;

       if ErroMsg <> '' then
        begin
          if (fsTentativasSincronizacao < cNumFalhasMax ) and
             (EscECFResposta.CAT = 15) and (EscECFResposta.RET.ECF = 1) then    // Erro de sincronização
          begin
            GravaLog( '    Falha SYN - RX <- '+EscECFResposta.Resposta, True);
            fsSincronizou       := False;   // Força a sincronização
            Inc(fsTentativasSincronizacao); // Evita loop infinito, no caso de ocorrer o mesmo erro
            Self.EnviaComando_ECF();        // Gera chamada recursiva
            exit;
          end;

          if (fsTentativasSincronizacao < cNumFalhasMax) and IsDaruma and
             (EscECFResposta.CAT = 16) and (EscECFResposta.RET.ECF = 140) then // 140-Relógio está travado
          begin
            GravaLog( '    Daruma Erro:140 - RX <- '+EscECFResposta.Resposta, True);
            Sleep(200);
            fsSincronizou       := False;   // Força a sincronização
            Inc(fsTentativasSincronizacao); // Evita loop infinito, no caso de ocorrer o mesmo erro
            Self.EnviaComando_ECF();        // Gera chamada recursiva
            exit;
          end;

          ErroMsg := ACBrStr('Erro retornado pela Impressora: '+ModeloStr+
                             sLineBreak+sLineBreak + ErroMsg ) ;

          if (EscECFResposta.CAT = 12) then
             DoOnErrorSemPapel
          else
             raise EACBrECFSemResposta.create(ErroMsg) ;
        end
       else
          Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }

    finally
       TimeOut := OldTimeOut ;
       fsTentativasSincronizacao := 0;
    end ;
  end;
end;

function TACBrECFEscECFProtocolo.VerificaFimLeitura(var Retorno: AnsiString;
  var TempoLimite: TDateTime): Boolean;
var
  LenRet, TBR : Integer;
  Byte1  : AnsiChar;

   procedure PedeStatus;
   begin
     with fpECFEscECF do
     begin
       GravaLog( '         Status TX -> '+ENQ+chr(fsSPR), True);
       Device.Serial.SendBlock( ENQ + chr(fsSPR) );
       Retorno := '';
       fsTimeOutStatus := IncMilliSecond(now,1000);  // Espera Status por 1 seg..
       TempoLimite := IncSecond(now, TimeOut);
     end;
   end;

begin
  LenRet := Length( Retorno );
  Result := False;
  Byte1  := #0;
  with fpECFEscECF do
  begin
    if LenRet > 0 then
    begin
      Byte1 := Retorno[1];

      case Byte1 of
        SOH :
          begin
            if LenRet >= 11 then
            begin
              TBR    := LEStrToInt( copy(Retorno,10,2) ) ;
              Result := ( LenRet >=  (11 + TBR + 1) ) ;
            end ;
          end;

        ACK :
          begin
            fsSPR := 0;
            fsACK := True;
            GravaLog( '                RX <- '+Retorno, True);
            PedeStatus ;
          end;

        WAK :
          begin
            Result := (LenRet >= 6) ;

            if Result and (not fsACK) then  // Comando não foi recebido, re-envie
            begin
              GravaLog('                RX <- '+Retorno, True);
              Sleep(100);
              GravaLog('        Reenvio TX -> '+ComandoEnviado, True);
              Device.EnviaString( ComandoEnviado ) ;
              Retorno     := '';
              Result      := False;
              TempoLimite := IncSecond(now, TimeOut);
            end ;
          end ;

        NAK :
          Result := (LenRet >= 6) ;
      end;

      if Result then
      begin
        try
          { Esta atribuição, Já verifica o ChkSum, em caso de erro gera exception }
          EscECFResposta.Resposta := Retorno ;

          if (Byte1 = SOH) and
             (EscECFResposta.SEQ <> EscECFComando.SEQ) then  // Despreza esse Bloco
          begin
            raise EACBrECFCMDInvalido.Create(
                 'Sequencia de Resposta ('+IntToStr(EscECFResposta.SEQ)+')'+
                 'diferente da enviada ('+IntToStr(EscECFComando.SEQ)+
                 '). Bloco Desprezado' ) ;
          end;
        except
          on E : EACBrECFCMDInvalido do
          begin
            GravaLog( '              Erro <- '+E.Message + ' - ' + Retorno  , True ) ;
            Result  := False ;
            Retorno := '' ;
            Inc( fsFalhas ) ;
            GravaLog('         Falha: '+IntToStr(fsFalhas));
            if fsFalhas <= cNumFalhasMax then
            begin
              Device.Serial.Purge;
              PedeStatus
            end;
          end
          else
            raise ;
        end ;
      end ;
    end;

    if Result then
    begin
      if (Byte1 = WAK) then // Ocupado, aguarde e solicite novo Status
      begin
        if (fsWAKCounter > 0) and                // Já esteve ocupada antes ?
           (EscECFComando.fsCMD in [20,22,26]) and  // Foi um comando de "Leitura X Serial" ou "Leitura de Informações" ?
           (IsBematech or IsDaruma) and
           (( fsWAKCounter * cEsperaWAK ) >= (TimeOut*1000)) then  // Atingiu o TimeOut ?
        begin
          // Muitos pedidos de Status... Bematech entrou em Loop... envie o comando novamente...
          GravaLog('*** ECF em possível loop infinito: '+IntToStr(fsWAKCounter)+
                   ' respostas de ocupado. Reenviando o último comando');
          GravaLog('        Reenvio TX -> ' + ComandoEnviado, True);
          Device.EnviaString(ComandoEnviado);
          Retorno := '';
          fsWAKCounter := 0;
          TempoLimite := IncSecond(Now, TimeOut);
        end
        else
        begin
          Inc( fsWAKCounter );
          GravaLog('                RX <- '+Retorno+ ' ('+IntToStr(fsWAKCounter)+')', True);
          Sleep( cEsperaWAK );
          PedeStatus;
        end;

        Result := False;
      end
      else
      begin
        fsWAKCounter := 0;

        if (Byte1 = SOH) and (EscECFResposta.CAT = 0) then
        begin
          if not TestBit( EscECFResposta.RET.ECF, 0 ) then // Existem mais dados ?
          begin
            GravaLog('     '+IntToStrZero(EscECFResposta.TBR,4)+' bytes RX <- '+Retorno, True);
            Inc( fsSPR );
            PedeStatus;
            Result := False;
          end;
        end;
      end;
    end
    else
    begin
      // Se ECF não respondeu a status... Envia comando novamente ou solicite status
      if (fsTimeOutStatus > 0) and (fsTimeOutStatus < Now) and (fsFalhas < cNumFalhasMax) then
      begin
        Inc(fsFalhas);
        GravaLog('         Falha: '+IntToStr(fsFalhas));
        Device.Serial.Purge;
        Sleep( cEsperaWAK );

        // Se for leitura de informações e Falha 1, Primeiro vamos tentar um novo pedido de Status
        if (fsFalhas = 1) and (EscECFComando.fsCMD = 26) then
          PedeStatus
        else
        begin
          GravaLog('        Reenvio TX -> '+ComandoEnviado, True);
          Device.EnviaString( ComandoEnviado ) ;
          fsTimeOutStatus := IncMilliSecond(now,1000);  // Espera Status por 1 seg..
        end;
      end;
    end;
  end;
end;

procedure TACBrECFEscECFProtocolo.Ativar;
begin
  with fpECFEscECF do
  begin
    if not Device.IsSerialPort  then
       raise EACBrECFERRO.Create(ACBrStr('A impressora: '+ModeloStr+' requer'+sLineBreak+
                              'Porta Serial:  (COM1, COM2, COM3, ...)'));

    AtivarDevice ;

    if not EmLinha( TimeOut ) then
    begin
       if Device.HandShake <> hsDTR_DSR then
          Device.HandShake := hsDTR_DSR;

       if not EmLinha( TimeOut ) then
       begin
          if Device.HandShake <> hsNenhum then
             Device.HandShake := hsNenhum;
       end;
    end;

    fsSincronizou := False;
    fsTentativasSincronizacao := 0;
    fsTimeOutStatus := 0;

    RespostasComando.Clear;

    { Ajusta a sequencia }
    Sincronizar;
  end;
end;

procedure TACBrECFEscECFProtocolo.Desativar;
begin
  fpECFEscECF.Desativar;
end;

{ TACBrECFEscECFRET }

constructor TACBrECFEscECFRET.Create;
begin
  inherited create ;

  Clear;
end;

procedure TACBrECFEscECFRET.SetRET(const AValue: AnsiString);
begin
   if fsRET=AValue then Exit;

   if Length(AValue) < 4 then
      raise EACBrECFERRO.Create(ACBrStr('RET deve ter tamanho de 4 bytes')) ;

   fsRET        := AValue;
   fsECF        := ord( fsRET[1] );
   fsFisco      := ord( fsRET[2] );
   fsSPR        := ord( fsRET[3] );
   fsFabricante := ord( fsRET[4] );
end;

procedure TACBrECFEscECFRET.Clear;
begin
   fsECF        := 0;
   fsFabricante := 0;
   fsFisco      := 0;
   fsSPR        := 0;
   fsRET        := '';
end;

{ ------------------------------ TACBrECFEscECFComando -------------------------- }

constructor TACBrECFEscECFComando.Create;
begin
  inherited Create ;

  fsParams := TStringList.create ;
  fsSEQ    := 0 ;
end;

destructor TACBrECFEscECFComando.destroy;
begin
  fsParams.Free ;

  inherited destroy ;
end;


procedure TACBrECFEscECFComando.SetCMD(const Value: Byte);
begin
  fsCMD     := Value ;
  fsEXT     := 0;
  fsTimeOut := 0 ;
  fsParams.Clear ;
end;

function TACBrECFEscECFComando.GetComando: AnsiString;
Var
  I, LenCmd, Soma : Integer ;
  Buffer, BCD : AnsiString ;
  TBC : Integer ;
  CHK : Byte ;
begin
  if (fsCMD = 255) and (fsEXT = 0) then
     raise EACBrECFERRO.Create(ACBrStr('Para comandos 255, EXT deve ser especificado')) ;

  BCD := '' ;
  For I := 0 to fsParams.Count-1 do
    BCD := BCD + StringToBinaryString( AnsiString(fsParams[I]) ) + '|';
  BCD := LeftStr( BCD, cEscECFMaxBuffer);
  TBC := Length( BCD ) ;

  Buffer := AnsiChr(fsSEQ) + AnsiChr(fsCMD) + AnsiChr(fsEXT) +
            IntToLEStr(TBC) + BCD ;

  Soma := 0 ;
  LenCmd := Length( Buffer ) ;
  For I := 1 to LenCmd do
     Soma := Soma + ord( Buffer[I] ) ;
  CHK := Soma mod 256  ;

  Result := SOH + Buffer + AnsiChr( CHK ) ;
end;

procedure TACBrECFEscECFComando.AddParamString(const AString: AnsiString);
var
  Buf : AnsiString ;
begin
  { Convertendo caracteres de comando para Hexa para poder armazenar
    corretamente no TStringList }
  Buf := BinaryStringToString( AString );

  fsParams.Add( TrimRight( Buf ) ) ;
end;

procedure TACBrECFEscECFComando.AddParamDouble(ADouble: Double; Decimais: Byte);
begin
  ADouble := ADouble * power(10, Decimais);
  AddParamInteger( TruncFix( ADouble ) ) ;
end;

procedure TACBrECFEscECFComando.AddParamInteger(AInteger: Integer);
begin
  AddParamString( IntToStr( AInteger ) ) ;
end;

procedure TACBrECFEscECFComando.AddParamDateTime(ADateTime: TDateTime;
   Tipo : Char = 'D'; const FlagHV : String = ''  ) ;
Var
  Formato : String ;
begin
  case Tipo of
    'T','H' : Formato := 'hhnnss' ;
        'D' : Formato := 'ddmmyyyy' ;
  else
     Formato := 'ddmmyyyyhhnnss' ;
  end ;

  AddParamString( FormatDateTime(Formato, ADateTime) + FlagHV ) ;
end;


{ ----------------------------- TACBrECFEscECFResposta -------------------------- }

constructor TACBrECFEscECFResposta.Create;
begin
  inherited create ;

  fsParams := TStringList.create ;
  fsRET    := TACBrECFEscECFRET.Create;
  Clear;
end;

destructor TACBrECFEscECFResposta.Destroy;
begin
  fsParams.Free ;
  fsRET.Free;
  inherited destroy ;
end;

procedure TACBrECFEscECFResposta.Clear(ClearParams: Boolean);
begin
  if ClearParams then
     fsParams.Clear ;

  fsRET.Clear;
  fsSEQ := 5 ;
  fsCMD := 0 ;
  fsEXT := 0  ;
  fsCAT := 0  ;
  fsTBR := 0  ;
  fsBRS := '' ;
  fsCHK := 0 ;
end;

procedure TACBrECFEscECFResposta.SetResposta(const AValue: AnsiString);
Var
  Soma, I, F, LenCmd : Integer ;
  vCHK  : Byte ;
begin
  Clear( False ) ;    // Não Zera Params, pois pode acumular 2 retornos

  if AValue = '' then exit ;

  LenCmd := Length( AValue ) ;

  if (LenCmd = 6) or (AValue[1] in [WAK,NAK]) then  // Retorno de NAK ou WAK
  begin
    fsResposta := AValue ;
    fsCAT      := ord( AValue[2] ) ;
    fsRET.RET  := Copy( AValue, 3, 4 );
    exit ;
  end;

  if LenCmd < 12 then
     raise EACBrECFSemResposta.Create('Tamanho de Resposta muito curto: '+
                                      IntToStr(LenCmd)+' bytes');

  fsResposta := AValue ;
  fsSEQ      := ord( AValue[2] ) ;
  fsCMD      := ord( AValue[3] ) ;
  fsEXT      := ord( AValue[4] ) ;
  fsCAT      := ord( AValue[5] ) ;
  fsRET.RET  := Copy( AValue, 6, 4 );
  fsTBR      := LEStrToInt( copy(AValue,10,2) );
  fsBRS      := copy( AValue, 12, fsTBR ) ;
  fsCHK      := ord( AValue[ 12 + fsTBR ] ) ;

  Soma := 0 ;
  LenCmd := fsTBR+11;
  For I := 2 to LenCmd do
     Soma := Soma + ord( AValue[I] ) ;
  vCHK := Soma mod 256  ;

  if vCHK <> fsCHK then
     raise EACBrECFSemResposta.Create(ACBrStr('Erro CHK Resposta. '+
        'Calculado:'+IntToStr(vCHK)+' Recebido:'+IntToStr(fsCHK)));

  { Quebrando Parametros Separados por '|' e inserindo-os em fsParams }
  I := 1;
  while I <= fsTBR do
  begin
     F := PosEx('|',fsBRS,I) ;
     if F < I then
        Break ;

     fsParams.Add( Copy(fsBRS, I, F-I) ) ;
     I := F+1;
  end ;

  if (I < fsTBR) and (fsTBR > 0) then   // Resposta sem último '|'
     fsParams.Add( copy( fsBRS, I, fsTBR+1-I ) ) ;
end;

{ ----------------------------- TACBrECFEscECF ------------------------------ }

constructor TACBrECFEscECF.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fsEscECFComando  := TACBrECFEscECFComando.Create;
  fsEscECFResposta := TACBrECFEscECFResposta.Create;
  fsEscECFProtocolo := TACBrECFEscECFProtocolo.Create(Self);

  fpDevice.HandShake := hsDTR_DSR ;
  fpPaginaDeCodigo   := 1252;
  fsArqMemoria       := '';

  fpModeloStr := 'EscECF' ;
  fpColunas   := 48 ;
  fpMFD       := True ;
  fpTermica   := True ;
  fpIdentificaConsumidorRodape := True ;

  { Variaveis internas dessa classe }
  fsNumVersao     := '' ;
  fsVersaoEscECF  := '' ;
  fsPAF           := '' ;
  fsNumECF        := '' ;
  fsNumCRO        := '' ;
  fsNumLoja       := '' ;
  fsDataHoraSB    := 0 ;
  fsMarcaECF      := '' ;
  fsModeloECF     := '' ;
  fsEmPagamento   := False ;
  fsDeviceParams  := '';

  RespostasComando.Clear;
end;

destructor TACBrECFEscECF.Destroy;
begin
  fsEscECFComando.Free ;
  fsEscECFResposta.Free ;
  fsEscECFProtocolo.Free;

  inherited Destroy ;
end;

procedure TACBrECFEscECF.Ativar;
var
  Params: String;
begin
  fsNumVersao    := '' ;
  fsVersaoEscECF := '' ;
  fsPAF          := '' ;
  fsNumECF       := '' ;
  fsNumCRO       := '' ;
  fsNumLoja      := '' ;
  fsDataHoraSB   := 0 ;
  fsMarcaECF     := '' ;
  fsModeloECF    := '' ;
  fsDeviceParams := '';

  fpMFD     := True ;
  fpTermica := True ;
  fpColunas := 48 ;
  fpNumMaxLinhasRodape := 8;

  if fpDevice.IsDLLPort then
  begin
    if not (fsEscECFProtocolo is TACBrECFEscECFProtocoloEpsonDLL) then
    begin
      fsEscECFProtocolo.Free;
      fsEscECFProtocolo := TACBrECFEscECFProtocoloEpsonDLL.Create(Self);
    end
  end
  else
  begin
     if (fsEscECFProtocolo is TACBrECFEscECFProtocoloEpsonDLL) then
     begin
       fsEscECFProtocolo.Free;
       fsEscECFProtocolo := TACBrECFEscECFProtocolo.Create(Self);
     end
  end;

  fpAtivo := False;
  fsEscECFProtocolo.Ativar;

  try
     { Testando a comunicaçao com a porta }
     Params := RetornaInfoECF( '15|0' );

     if Params = '' then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+ModeloStr ));

     fsMarcaECF      := EscECFResposta.Params[0];
     fsModeloECF     := EscECFResposta.Params[1];
     fsNumECF        := EscECFResposta.Params[4];
     fpDecimaisPreco := min( StrToIntDef( EscECFResposta.Params[11], 2), 3);
     fpDecimaisQtd   := min( StrToIntDef( EscECFResposta.Params[12], 3), 3);
     fsNumVersao     := EscECFResposta.Params[13];
     fsVersaoEscECF  := EscECFResposta.Params[19];

     if NomeArqMemoria <> '' then
        fsArqMemoria := NomeArqMemoria
     else
        fsArqMemoria := ExtractFilePath( ParamStr(0) )+'ACBrECFEscECF'+
                        Poem_Zeros( fsNumECF, 3 )+'.txt';

     if IsBematech then
     begin
       if MaxLinhasBuffer = 0 then  // Bematech congela se receber um Buffer muito grande
         MaxLinhasBuffer := 5;

       if (CompareVersions(fsNumVersao, '01.00.02') >= 0) then
       begin
         // http://partners.bematech.com.br/bemacast/paginas/post.aspx?title=edicao-241---o-ecf-bematech-mp-4200-th-fi-ii&id=6220
         fpNumMaxLinhasRodape := 20;
       end;
     end
     else if IsEpson then
     begin
       fpPaginaDeCodigo := 850;
       fpColunas := 57;
     end
     else if IsDaruma then
     begin
       ControlePorta := False;  // Daruma não trabalha bem com Controle de Porta ativo
     end;

     LeRespostasMemoria;
  except
     Desativar ;
     raise ;
  end ;

end;

function TACBrECFEscECF.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
begin
  Result := fsEscECFProtocolo.EnviaComando_ECF(cmd);
end;

function TACBrECFEscECF.VerificaFimLeitura(var Retorno : AnsiString ;
  var TempoLimite : TDateTime) : Boolean ;
begin
  Result := fsEscECFProtocolo.VerificaFimLeitura( Retorno, TempoLimite);
end;

function TACBrECFEscECF.GetModeloStr: String;
begin
  if Trim(fsMarcaECF) <> '' then
    Result := fsMarcaECF
  else
    Result := fpModeloStr;
end;

function TACBrECFEscECF.IsBematech : Boolean ;
begin
  Result := (pos('BEMATECH',UpperCase(fsMarcaECF)) > 0) ;
end;

function TACBrECFEscECF.IsDaruma: Boolean;
begin
  Result := (pos('DARUMA',UpperCase(fsMarcaECF)) > 0) ;
end;

function TACBrECFEscECF.IsEpson: Boolean;
begin
  Result := (pos('EPSON',UpperCase(fsMarcaECF)) > 0) ;
end;

procedure TACBrECFEscECF.EnviaConsumidor;
begin
  try
     with EscECFComando do
     begin
        CMD := 149;
        AddParamString(LeftStr(OnlyNumber(Consumidor.Documento), 14)) ;
        AddParamString(LeftStr(Consumidor.Nome, 30)) ;
        AddParamString(LeftStr(Consumidor.Endereco, 79)) ;
     end;
     EnviaComando;
     Consumidor.Enviado := True ;
  except
  end ;
end;

function TACBrECFEscECF.TraduzErroMsg(EscECFResposta: TACBrECFEscECFResposta
  ): String;
var
   MsgCategoria, MsgMotivo: String;
   Categoria, Motivo: Byte;
begin
   Categoria    := EscECFResposta.CAT;
   Motivo       := EscECFResposta.RET.ECF ;
   MsgCategoria := '';
   MsgMotivo    := '';

   case Categoria of
     01 :
       begin
         MsgCategoria := 'Comando Inválido';

         case Motivo of
           01 : MsgMotivo := 'O comando enviado para a impressora não existe no Software Básico' ;
         end;
       end;

     02 :
       begin
         MsgCategoria := 'Erro em parâmetro do comando';

         case Motivo of
           01 : MsgMotivo := 'Conteúdo de parâmetro inválido no comando.' ;
           02 : MsgMotivo := 'Falta parâmetro no comando' ;
           03 : MsgMotivo := 'Excesso de parâmetros no comando' ;
           04 : MsgMotivo := 'COO inicial maior que COO final.' ;
           05 : MsgMotivo := 'CRZ inicial maior que CRZ final' ;
           06 : MsgMotivo := 'Data inicial maior que Data final' ;
         end;
       end;

     03 :
       begin
         MsgCategoria := 'Overflow de capacidade';

         case Motivo of
           01 : MsgMotivo := 'Excedeu a capacidade máxima do totalizador.' ;
         end;
       end;

     04 :
       begin
         MsgCategoria := 'Erro de contexto';

         case Motivo of
           01 : MsgMotivo := 'Comando só pode ser executado em intervenção' ;
           02 : MsgMotivo := 'Comando não pode ser executado em intervenção' ;
           03 : MsgMotivo := 'Comando não pode ser executado localmente' ;
           04 : MsgMotivo := 'Comando não pode ser executado remotamente' ;
         end;
       end;

     05 :
       begin
         MsgCategoria := 'Erro em Cupom Fiscal';

         case Motivo of
           01 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Cupom Fiscal aberto.' ;
           02 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Comprovante Não Fiscal aberto.' ;
           03 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Comprovante de Crédito ou Débito aberto.' ;
           04 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Estorno de Comprovante de Crédito ou Débito aberto.' ;
           05 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Relatório Gerencial aberto.' ;
           06 : MsgMotivo := 'Comando enviado não pode ser executado, pois o ECF está em repouso.' ;
           07 : MsgMotivo := 'A quantidade máxima de itens em um Cupom Fiscal foi ultrapassada.' ;
           08 : MsgMotivo := 'A quantidade de parcelas somente pode ser especificada para os pagamentos que envolvam meios que aceitem a emissão de CCD.' ;
           09 : MsgMotivo := 'Limite máximo de pagamentos por documento já foi atingido.' ;
           10 : MsgMotivo := 'Cancelamento de um Cupom Fiscal somente será permitido após o estorno de todos os CCDs emitidos.' ;
           11 : MsgMotivo := 'Comando não pode ser executado em documento não pago.';
           12 : MsgMotivo := 'Comando não pode ser executado após desconto ou acréscimo em Subtotal' ;
           13 : MsgMotivo := 'Comando de acréscimo/desconto já executado.' ;
           14 : MsgMotivo := 'Comando de consumidor já executado no clichê' ;
         end;
       end;

     06 :
       begin
         MsgCategoria := 'Erro em Comprovante Não Fiscal';

         case Motivo of
           01 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Cupom Fiscal aberto.' ;
           02 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Comprovante Não Fiscal aberto.' ;
           03 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Comprovante de Crédito ou Débito aberto.' ;
           04 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Estorno de Comprovante de Crédito ou Débito aberto.' ;
           05 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Relatório Gerencial aberto.' ;
           06 : MsgMotivo := 'A quantidade máxima de itens em um Comprovante Não Fiscal foi ultrapassada.' ;
           07 : MsgMotivo := 'A quantidade de parcelas somente pode ser especificada para os pagamentos que envolvam meios que aceitem a emissão de CCD.' ;
           08 : MsgMotivo := 'Limite máximo de pagamentos por documento já foi atingido.' ;
           09 : MsgMotivo := 'Cancelamento de um Comprovante Não Fiscal somente será permitido após o estorno de todos os CCDs emitidos.' ;
           10 : MsgMotivo := 'Comando não pode ser executado em documento não pago.' ;
           11 : MsgMotivo := 'Comando não pode ser executado após desconto ou acréscimo em Subtotal';
           12 : MsgMotivo := 'Comando de acréscimo/desconto já executado.' ;
           13 : MsgMotivo := 'Comando de consumidor já executado no clichê' ;
         end;
       end;

     07 :
       begin
         MsgCategoria := 'Erro em Relatório Gerencial ou CCD';

         case Motivo of
           01 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Cupom Fiscal aberto.' ;
           02 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Comprovante Não Fiscal aberto.' ;
           03 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Comprovante de Crédito ou Débito aberto.' ;
           04 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Estorno de Comprovante de Crédito ou Débito aberto.' ;
           05 : MsgMotivo := 'Comando enviado não pode ser executado, pois existe um Relatório Gerencial aberto.' ;
           06 : MsgMotivo := 'Não existe CCD para o pagamento especificado.' ;
           07 : MsgMotivo := 'CCD especificado já foi impresso.' ;
           08 : MsgMotivo := 'CCD especificado já foi re-impresso' ;
           09 : MsgMotivo := 'CCD especificado já foi estornado.' ;
           10 : MsgMotivo := 'CDD não especificado no estorno não foi impresso' ;
           11 : MsgMotivo := 'limite máximo de CCD’s por cupom foi excedido.';
           12 : MsgMotivo := 'Comando enviado não pode ser executado dentro de CCD' ;
           13 : MsgMotivo := 'Documento anterior diferente de Cupom Fiscal e Comprovante Não fiscal.' ;
           14 : MsgMotivo := 'Envio de texto genérico para CCD ou Relatório Gerencial já fechado.' ;
         end;
       end;

     08 :
       begin
         MsgCategoria := 'Erro em Redução Z';

         case Motivo of
           01 : MsgMotivo := 'Redução Z pendente ou já realizada na data' ;
         end;
       end;

     09 :
       begin
         MsgCategoria := 'Integridade';

         case Motivo of
           01 : MsgMotivo := 'Memória Fiscal inicializada em outro ECF' ;
           02 : MsgMotivo := 'Memória de Fita Detalhe inicializada em outro de ECF.' ;
           03 : MsgMotivo := 'Marca do ECF, Tipo ou Modelo incompatível com o gravado na Memória Fiscal.' ;
           04 : MsgMotivo := 'Número de série da MF diferente do gravado na MFD.' ;
           05 : MsgMotivo := 'Não foi localizado o número de série na MF.' ;
           06 : MsgMotivo := 'Não foi localizado na MF o registro do BR.' ;
           07 : MsgMotivo := 'Não foi localizado na MF o Símbolo da moeda.' ;
           08 : MsgMotivo := 'Não foram localizados na MF os símbolos de criptografia do GT.' ;
           09 : MsgMotivo := 'Não foi localizado na MF o CNPJ/ IE ou IM do usuário' ;
           10 : MsgMotivo := 'Versão do Software básico inválida.' ;
           11 : MsgMotivo := 'Memória Fiscal foi desconectada.';
           12 : MsgMotivo := 'MFD foi desconectada' ;
           13 : MsgMotivo := 'Erro de gravação na Memória fiscal.' ;
           14 : MsgMotivo := 'Erro de gravação na MFD' ;
           15 : MsgMotivo := 'Erro na recuperação de dados da MF.' ;
           16 : MsgMotivo := 'Erro na recuperação de dados da MFD' ;
           17 : MsgMotivo := 'Checksum inválido no comando recebido pelo ECF.' ;
         end;
       end;

     10 :
       begin
         MsgCategoria := 'Cheque/CMC-7';

         case Motivo of
           01 : MsgMotivo := 'Documento não inserido' ;
         end;
       end;

     11 :
       begin
         MsgCategoria := 'Autenticação';

         case Motivo of
           01 : MsgMotivo := 'Excedida a quantidade permitida.' ;
           02 : MsgMotivo := 'Não permitida na condição' ;
         end;
       end;

     12 :
       begin
         MsgCategoria := 'Sem Papel';
       end;

     13 :
       begin
         MsgCategoria := 'Relógio';

         case Motivo of
           01 : MsgMotivo := 'Qualquer alteração do relógio não permitida.' ;
           02 : MsgMotivo := 'Entrada ou saída de verão não permitida' ;
           03 : MsgMotivo := 'Relógio com data/hora anterior ao último documento gravado na MFD.' ;
           04 : MsgMotivo := 'Data/hora do relógio inválida' ;
         end;
       end;

     14 :
       begin
         MsgCategoria := 'Programação';

         case Motivo of
           01 : MsgMotivo := 'Índice de alíquota de ICMS já existente.' ;
           02 : MsgMotivo := 'Índice de alíquota de ISSQN já existente  ' ;
           03 : MsgMotivo := 'Índice de ISSQN não permitido.' ;
           04 : MsgMotivo := 'Índice de Meio de pagamento já existente' ;
           05 : MsgMotivo := 'Índice de Não Fiscal já existente.' ;
           06 : MsgMotivo := 'Índice de relatório gerencial já existente' ;
           07 : MsgMotivo := 'Excedida a quantidade máxima' ;
         end;
       end;

     15 :
       begin
         MsgCategoria := 'Protocolo';

         case Motivo of
           01 : MsgMotivo := 'Caractere de controle inválido no comando recebido pelo ECF.' ;
           02 : MsgMotivo := 'Checksum inválido no comando recebido pelo ECF' ;
         end;
       end;

     16 :
       begin
         MsgCategoria := 'Erro específico do Fabricante';

         if IsEpson then
           MsgMotivo := DescricaoRetornoEpson( EscECFResposta.RET.SPR, Motivo )
         else if IsDaruma then
           MsgMotivo := DescricaoRetornoDaruma( EscECFResposta.RET.SPR, Motivo );
       end;
   end;

   Result := 'Categoria: '+IntToStr(Categoria)+ '-' + MsgCategoria + sLineBreak+
             'Motivo: '+IntToStr(Motivo)  ;
   if MsgMotivo <> '' then
      Result := Result + '-'+MsgMotivo;
end;

function TACBrECFEscECF.AjustaDescricao(const ADescricao : String) : String ;
begin
  { Ajusta uma descrição de acordo com as regras do protocolo EscECF
    Máximo de 15, Mínimo de 4 caracteres ASCII de posição 65 a 90 (letras maiúsculas)
    ou 97 a 122 (letras minúsculas) }

  Result := LeftStr( TiraAcentos( ADescricao ),15);
end ;

procedure TACBrECFEscECF.SalvaRespostasMemoria(AtualizaVB : Boolean) ;
Var
  ValVB : Double;
begin
  AtualizaVB := AtualizaVB or (not Assigned(RespostasComando.FindFieldByName('VendaBruta')));

  if AtualizaVB then
  begin
    try
      ValVB := GetVendaBruta;
      RespostasComando.AddField( 'VendaBruta', FloatToIntStr(ValVB) );
    except
    end;
  end ;

  RespostasComando.SaveToFile( fsArqMemoria );
end ;

procedure TACBrECFEscECF.LeRespostasMemoria ;
Var
  ValVB : Double;
begin
  if not FileExists( fsArqMemoria ) then
    exit ;

  RespostasComando.LoadFromFile( fsArqMemoria );

  try
     ValVB := RespostasComando.FieldByName('VendaBruta').AsFloat;
     if ValVB <> GetVendaBruta then
        RespostasComando.Clear;    // Arquivo invalido
  except
     RespostasComando.Clear;       // Arquivo invalido
  end;
end ;

function TACBrECFEscECF.CriarECFClassPorMarca: TACBrECFClass;
begin
  Result := Nil;
  fsDeviceParams := '';

  if IsBematech then
    Result := TACBrECFBematech.create(fpOwner)
  else if IsDaruma then 
  begin
    Result := TACBrECFDaruma.create(fpOwner);
    Result.Device.Baud  := 115200;
  end else if IsEpson then
  begin
    Result := TACBrECFEpson.create(fpOwner);
    Result.Device.Desativar;
    fsDeviceParams := fpDevice.Porta+':'+fpDevice.ParamsString;
    Result.Device.Porta := 'USB';         // Força DLL em USB
    Result.Device.Baud  := 115200;
  end;

  if Result <> Nil then
  begin
    Result.PathDLL := Self.PathDLL;
    Result.ArqLOG  := Self.ArqLOG;
  end;
end;

procedure TACBrECFEscECF.DestruirECFClass(AECFClass: TACBrECFClass);
var
  P: Integer;
begin
  AECFClass.Free;

  if fsDeviceParams <> '' then
  begin
     P := pos(':',fsDeviceParams);
     if fpDevice.Ativo then
        fpDevice.Desativar;

     fpDevice.Porta        := copy(fsDeviceParams,1,P-1);
     fpDevice.ParamsString := copy(fsDeviceParams,P+1,Length(fsDeviceParams));
     fsDeviceParams := '';
  end;

  Self.Ativar;
end;

procedure TACBrECFEscECF.AjustaComandosControleImpressao(var Linha: AnsiString);
var
  P: Integer;
  EhControle: Boolean;
begin
  if IsDaruma or (IsEpson and (not fpDevice.IsDLLPort)) then
    Exit;

  P := pos(LF, Linha);
  while P > 0 do
  begin
    EhControle := (Linha[max(P-1,1)] = ESC);

    if (not EhControle) then
    begin
      Linha := StuffString(Linha, P, 0, CR );  // Adiciona CR antes de LF
      Inc( P );
    end
    else
    begin
      if IsBematech then
      begin
        Delete(Linha, P-1, 1);  // Remove "ESC" (carcater de controle)
        Dec( P );
      end;
    end;

    P := PosEx( LF, Linha, P+1);
  end;
end;

procedure TACBrECFEscECF.AjustaLinhasColunas(var Linhas: AnsiString;
  MaxLin: Integer);
begin
  Linhas := AjustaLinhas(Linhas, fpColunas, MaxLin, IsBematech);

  if RightStr(Linhas, 1) = #10 then
    Linhas := LeftStr(Linhas, Length(Linhas)-1) + #0;
end;

procedure TACBrECFEscECF.AtivarDevice;
begin
  inherited Ativar;
end;

function TACBrECFEscECF.RetornaInfoECF(Registrador: String): AnsiString;
begin
  if Pos('|',Registrador) = 0 then
     Registrador := Registrador + '|0' ;

  EscECFComando.CMD := 26;
  EscECFComando.AddParamString(Registrador);
  EnviaComando;

  Result := EscECFResposta.BRS;
  while (RightStr(Result,1) = '|') do
     Delete( Result, Length(Result), 1 );
end;

function TACBrECFEscECF.CapturaXMLCupom(const Inicial, Final : String ; Tipo : Integer
   ) : AnsiString ;
Var
  I: Integer;
begin
  with EscECFComando do
  begin
    CMD := 150;
    AddParamInteger(Tipo);
    AddParamString(Inicial);
    AddParamString(Final);
    EnviaComando;
  end ;

  Result := '';
  For I := 0 to EscECFResposta.Params.Count-1 do
     Result := Result + EscECFResposta.Params[I] ;
end ;

function TACBrECFEscECF.GetDataHora: TDateTime;
Var
  RetCmd : String ;
begin
  RetCmd := RetornaInfoECF( '9' ) ;

  Result := EncodeDateTime( StrToInt(copy(RetCmd,5,4)),  // Ano
                            StrToInt(copy(RetCmd,3,2)),  // Mes
                            StrToInt(copy(RetCmd,1,2)),  // Dia
                            StrToInt(copy(RetCmd,9,2)),  // Hora
                            StrToInt(copy(RetCmd,11,2)), // Minuto
                            StrToInt(copy(RetCmd,13,2)), // Segundo
                            0 ) ;
end;

function TACBrECFEscECF.GetNumCupom: String;
var
  I: Integer;
begin
  I := 0;
  Result := '';
  while (I < cNumFalhasMax) and (Result = '') do
  begin
    RetornaInfoECF( '1|1' );
    if EscECFResposta.Params.Count > 1 then
    begin
      if EscECFResposta.Params[0] = '1' then
        Result := EscECFResposta.Params[1] ;
    end;

    Inc( I );
  end;
end;

function TACBrECFEscECF.GetNumECF: String;
begin
  Result := fsNumECF ;
end;

function TACBrECFEscECF.GetNumLoja : String ;
begin
  if Trim(fsNumLoja) = '' then
  begin
    if IsBematech then
    begin
      RetornaInfoECF( '99|5' ) ;
      fsNumLoja := EscECFResposta.Params[1] ;
    end
    else if IsEpson then
    begin
      RetornaInfoECF('99|15');
      fsNumLoja := EscECFResposta.Params[9] ;
    end
    else if IsDaruma then
    begin
      fsNumLoja := RetornaInfoECF('200|129');
    end;
  end;

  Result := fsNumLoja ;
end ;

function TACBrECFEscECF.GetNumCRO: String;
begin
  if Trim(fsNumCRO) = '' then
  begin
    RetornaInfoECF( '1|3' ) ;
    fsNumCRO := EscECFResposta.Params[1] ;
  end ;

  Result := fsNumCRO ;
end;

function TACBrECFEscECF.GetNumSerie: String;
begin
  Result := RetornaInfoECF( '15|4' ) ;
end;

function TACBrECFEscECF.GetNumVersao: String ;
begin
  Result := fsNumVersao ;
end;

function TACBrECFEscECF.GetTotalPago: Double;
var
  APagar : Double ;
begin
  // Obs: Não há comando que retorne o TotalPago...
  try
    Result := RespostasComando.FieldByName('TotalPago').AsFloat;
  except
    Result := -1;
  end ;

  if Result < 0 then
  begin
     try
        APagar := RespostasComando.FieldByName('TotalAPagar').AsFloat;
     except
        APagar := 0;
     end;

     if APagar > 0 then
       Result := (GetSubTotal - APagar)
     else
       Result := 0;
  end ;
end;

function TACBrECFEscECF.GetNumReducoesZRestantes: String;
begin
  RetornaInfoECF( '1|15' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetSubTotal: Double;
begin
  // TODO: Obs: Não há comando que retorne o SubTotal...
  try
     Result := RespostasComando.FieldByName('SubTotal').AsFloat;
  except
     Result := 0;
  end;
end;

function TACBrECFEscECF.GetEstado: TACBrECFEstado;
Var
  FlagEst : Integer ;
begin
  fpEstado := estNaoInicializada ;
  if (not fpAtivo) then
  begin
    Result := fpEstado ;
    Exit ;
  end;

  try
   fpEstado := estDesconhecido ;

    FlagEst := StrToIntDef( RetornaInfoECF( '16|5' ), -1 );
    Case FlagEst of
      0  :             fpEstado := estLivre;
      10 :             fpEstado := estVenda;
      11..13, 21..23 : fpEstado := estPagamento;
      20 :             fpEstado := estNaoFiscal;
      30..32 :         fpEstado := estRelatorio;
    end;

    if (fpEstado = estLivre) and IsDaruma then  // Daruma sinaliza estados em Byte 3 de RET
    begin
      if TestBit(EscECFResposta.RET.Fabricante, 4) then
      begin
        fpEstado := estBloqueada;
        Exit;
      end
      else if TestBit(EscECFResposta.RET.Fabricante, 5) then
      begin
        fpEstado := estRequerZ;
        Exit;
      end;
    end;

    if (fpEstado = estVenda) and fsEmPagamento then   // Já Subtotalizou ?
      fpEstado := estPagamento;

    if (fpEstado in [estLivre,estDesconhecido]) then
    begin
      FlagEst := StrToIntDef( RetornaInfoECF( '16|4' ), 0 );
      if FlagEst = 3 then
        fpEstado := estBloqueada ;
    end;

    if fpEstado in [estLivre, estBloqueada] then
    begin
      RetornaInfoECF( '8' ) ;
      FlagEst := StrToIntDef( EscECFResposta.Params[1], 0 );

      if FlagEst = 2 then    //  2 - Redução Z Pendente
      begin
        fpEstado := estRequerZ;

        if IsBematech then  // Workaround para Bematech, que não responde corretamente após Z emitida
        begin
          RetornaInfoECF( '99|10' ) ;
          if TestBit(StrToIntDef(EscECFResposta.Params[0], 0),3) then
            fpEstado := estBloqueada;
        end;
      end
      // Workaround para Epson que não responde Flag de Status de Movimento corretamente
      else if (fpEstado = estBloqueada) and (FlagEst = 0) and IsEpson then
      begin
        RetornaInfoECF( '99|21' ) ;
        if (EscECFResposta.Params.Count > 11) and (EscECFResposta.Params[11] = 'S') then
          fpEstado := estRequerZ;
      end
      { 0 - Não houve movimento, 1 - Com movimento aberto. Provavelmente
        motivo do Bloqueio é Tampa Aberto ou Falta de Papel, e não falta da Z }
      else if (fpEstado = estBloqueada) and (FlagEst in [0,1]) then
        fpEstado := estLivre;
    end;
  finally
    Result := fpEstado ;
  end;
end;

function TACBrECFEscECF.GetGavetaAberta: Boolean;
begin
  Result := RetornaInfoECF( '16|1' ) = '1' ;
end;

function TACBrECFEscECF.GetPoucoPapel: Boolean;
begin
  Result := RetornaInfoECF( '16|2' ) > '0' ;
end;

function TACBrECFEscECF.GetHorarioVerao: Boolean;
begin
  Result := UpperCase(RightStr( RetornaInfoECF( '9' ) ,1 )) =  'V' ;
end;

function TACBrECFEscECF.GetParamDescontoISSQN : Boolean ;
begin
  Result := True; // ESCEcf sempre permite Desconto de ISSQN
end ;

function TACBrECFEscECF.GetTipoUltimoDocumento: TACBrECFTipoDocumento;
begin
  if IsEpson then
  begin
    RetornaInfoECF( '99|09' );

    case StrToIntDef(EscECFResposta.Params[0],0) of
      01: Result := docCF;
      02: Result := docRZ;
      03: Result := docLX;
      05: Result := docLMF;
      22: Result := docCupomAdicional;
      23: Result := docCFCancelamento;
      24: Result := docCNF;
      25: Result := docCNFCancelamento;
      26: Result := docEstornoPagto;
      27: Result := docCCD;
      28: Result := docEstornoCCD;
      29: Result := docRG;
    else
       Result := docNenhum;
    end;
  end
  else
    Result := inherited GetTipoUltimoDocumento;
end;

procedure TACBrECFEscECF.LeituraX ;
begin
  EscECFComando.CMD := 20;
  EscECFComando.AddParamInteger(0); // Imprime no ECF
  EnviaComando;
end;

procedure TACBrECFEscECF.LeituraXSerial(Linhas: TStringList);
Var
  Buffer : AnsiString;
  I: Integer;
begin
  EscECFComando.CMD := 20;
  EscECFComando.AddParamInteger(1); // Envia pela Serial
  EnviaComando;

  Buffer := '';
  For I := 0 to EscECFResposta.Params.Count-1 do
     Buffer := Buffer + EscECFResposta.Params[I] ;

  Linhas.Clear;
  Linhas.Text := Buffer;
end;

procedure TACBrECFEscECF.AbreGaveta ;
begin
  EscECFComando.CMD := 6;
  EnviaComando;
end;

procedure TACBrECFEscECF.ReducaoZ(DataHora : TDateTime) ;
begin
  if DataHora = 0 then  { Aparentemente a DataHora é obrigatória na EscECF }
     DataHora := -1;

  EscECFComando.CMD := 21;
  if DataHora = -1 then  { Sem Data e Hora }
   begin
     EscECFComando.AddParamString('');   // Sem Data
     EscECFComando.AddParamString('');   // Sem Hora
   end
  else
   begin
     EscECFComando.AddParamDateTime(DataHora, 'D' );
     EscECFComando.AddParamDateTime(DataHora, 'H' );
   end;

  EscECFComando.AddParamInteger(0); // Imprime no ECF
  EscECFComando.TimeOut := max(TimeOut, 120); // TimeOut de no mínimo 2 minutos

  try
     EnviaComando ;
     Sleep(800);  // intervalo para ECF ficar operacional...

     RespostasComando.Clear;
     SalvaRespostasMemoria(True);
  except
     on E : Exception do
     begin
       // Categoria: 13-Relógio. Motivo: 3-Relógio com data/hora anterior ao último documento gravado na MFD.
       if (EscECFResposta.CAT in [13,16]) and (EscECFResposta.RET.ECF = 3) then
         begin
           ReducaoZ(-1);
         end
        else if (pos('5-1',E.Message) <> 0) then    // Comando inválido para o documento atual.
         begin                                      //  Ficou algum Cupom aberto ?
           // Cancelando o Cupom em aberto
           EscECFComando.CMD := 31;
           EnviaComando;

           ReducaoZ(DataHora);
         end 
        else
           raise ;
     end ;
  end ;
end;

procedure TACBrECFEscECF.AbreRelatorioGerencial(Indice: Integer);
begin
  Indice := max(Indice,1) ;

  EscECFComando.CMD := 12;
  EscECFComando.AddParamInteger(Indice);
  EnviaComando;

  RespostasComando.Clear;
  if (EscECFResposta.Params.Count > 3) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
    RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  end;
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.LinhaRelatorioGerencial(Linha: AnsiString;
   IndiceBMP: Integer);
var
  P, Espera, LenMaxBuffer: Integer;
  Buffer : AnsiString ;
begin
  { Muda caracteres de controle com CR e LF, para evitar quebra de linhas em "AjustaLinhas" abaixo }
  Linha := ReplaceString(Linha, ESC+CR, ESC+#213);
  Linha := ReplaceString(Linha, ESC+LF, ESC+#210);

  AjustaLinhasColunas( Linha, 0);  { Formata as Linhas de acordo com "Coluna" }

  Linha := ReplaceString(Linha, ESC+#213, ESC+CR);
  Linha := ReplaceString(Linha, ESC+#210, ESC+LF);

  LenMaxBuffer := cEscECFMaxBuffer;

  AjustaComandosControleImpressao(Linha);

  while Length( Linha ) > 0 do
  begin
     P := Length( Linha ) ;
     if P > LenMaxBuffer then    { Acha o fim de Linha mais próximo do limite máximo }
        P := PosLast(LF, copy(Linha, 1 , LenMaxBuffer) ) ;

     if P = 0 then
        P := Trunc( LenMaxBuffer / Colunas ) * Colunas;

     Buffer := copy( Linha, 1, P);

     { ignora a quebra de linha se sua posição for igual ao LenMaxBuffer }
     if (Length(Buffer) = LenMaxBuffer) and (Buffer[P] = LF) then
       Delete( Buffer, P, 1 );

     Espera := Trunc( CountStr( Buffer, LF ) / 4);

     EscECFComando.CMD := 9;
     EscECFComando.TimeOut := Espera;
     EscECFComando.AddParamString(Buffer);
     EnviaComando;

     { ficou apenas um LF sozinho ? }
     if (P = Colunas) and (RightStr( Buffer, 1) <> LF) and
        (copy( Linha, P+1, 1) = LF) then
        P := P + 1 ;

     Linha := copy( Linha, P+1, Length(Linha) ) ;   // O Restante
  end ;
end;

procedure TACBrECFEscECF.AbreCupomVinculado(COO, CodFormaPagto,
   CodComprovanteNaoFiscal: String; Valor: Double);
Var
  Sequencia, I, NumPagtos, P1, P2 : Integer ;
  APagto, CodPagto : String ;
  ValorPagto: Double;
  FPG: TACBrECFFormaPagamento;
begin
  // Achando a Sequencia do Pagamento de acordo com o Indice //
  Sequencia := 1;
  I := 1;
  try
    NumPagtos := RespostasComando.FieldByName('NumPagtos').AsInteger;
    if NumPagtos > 1 then
    begin
      repeat
        APagto := RespostasComando.FieldByName('Pagto'+IntToStr(I)).AsString;
        P1 := pos('|',APagto);
        CodPagto := Copy(APagto,1,P1-1);

        if (CodPagto = CodFormaPagto) then
        begin
          ValorPagto := 0;
          if Valor > 0 then
          begin
            P2 := PosEx('|', APagto, P1+1);
            if P2 > P1 then
              ValorPagto := StringToFloatDef(Copy(APagto,P1+1,P2-P1-1), 0);
          end;

          if (Valor = ValorPagto) then
          begin
            APagto := '*'+APagto;  // sinaliza que já usou;
            break;
          end;
        end ;

        Inc( I );

        if IsEpson then
        begin
          // Verificando se esse Pagamento permite Vinculado. Se não permitir, não considera como sequencia
          if LeftStr(CodPagto,1) = '*' then
            CodPagto := Copy(CodPagto, 2, Length(CodPagto));

          FPG := AchaFPGIndice(CodPagto);
          if Assigned(FPG) then
            if FPG.PermiteVinculado then
              Inc( Sequencia );
        end
        else
          Inc( Sequencia );

      until (I > NumPagtos);
    end ;
  except
  end ;

  with EscECFComando do
  begin
     CMD := 8;
     AddParamInteger(Sequencia) ;
     AddParamString(CodFormaPagto) ;
     AddParamInteger(1) ;   // Qtd Parcelas ??
     AddParamInteger(1) ;   // Num Parcela ??
     AddParamString(LeftStr(OnlyNumber(Consumidor.Documento),14)) ;
     AddParamString(LeftStr(Consumidor.Nome,30)) ;
     AddParamString(LeftStr(Consumidor.Endereco,79)) ;
  end;
  EnviaComando;

  //RespostasComando.Clear;
  if (EscECFResposta.Params.Count > 6) then
  begin
    RespostasComando.AddField( 'Pagto'+IntToStr(I), APagto );
    RespostasComando.AddField( 'COO',            EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',       EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta',     EscECFResposta.Params[2] );
    RespostasComando.AddField( 'NumSerie',       EscECFResposta.Params[3] );
    RespostasComando.AddField( 'SeqPagto',       EscECFResposta.Params[4] );
    RespostasComando.AddField( 'NumParcela',     EscECFResposta.Params[5] );
    RespostasComando.AddField( 'NumParcelaFalta',EscECFResposta.Params[6] );
  end;

  Consumidor.Enviado := True ;
  fsEmPagamento := false ;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.MudaHorarioVerao ;
begin
  MudaHorarioVerao( not HorarioVerao) ;
end;

procedure TACBrECFEscECF.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  EscECFComando.CMD := 80;
  EscECFComando.AddParamInteger( IfThen(EHorarioVerao,1,0) );
  EnviaComando;
end;

procedure TACBrECFEscECF.MudaArredondamento(Arredondar: Boolean);
begin
   inherited MudaArredondamento(Arredondar);
end;

procedure TACBrECFEscECF.LeituraMemoriaFiscal(DataInicial,
   DataFinal: TDateTime; Simplificada: Boolean);
begin
  EscECFComando.CMD := 22;
  EscECFComando.AddParamInteger( 0 ); // Imprime no ECF
  EscECFComando.AddParamInteger( ifthen(Simplificada,2,1) );
  EscECFComando.AddParamInteger( 1 );   // Por Data
  EscECFComando.AddParamDateTime( DataInicial );
  EscECFComando.AddParamDateTime( DataFinal );

  EnviaComando;
end;

procedure TACBrECFEscECF.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada: Boolean);
begin
  EscECFComando.CMD := 22;
  EscECFComando.AddParamInteger( 0 ); // Imprime no ECF
  EscECFComando.AddParamInteger( ifthen(Simplificada,2,1) );
  EscECFComando.AddParamInteger( 2 );   // Por CRZ
  EscECFComando.AddParamInteger( ReducaoInicial );
  EscECFComando.AddParamInteger( ReducaoFinal );

  EnviaComando;
end;

procedure TACBrECFEscECF.LeituraMemoriaFiscalSerial(DataInicial,
   DataFinal: TDateTime; Linhas: TStringList; Simplificada: Boolean);
var
   Buffer: AnsiString;
   I: Integer;
begin
  EscECFComando.CMD := 22;
  EscECFComando.AddParamInteger( 1 ); // Pela Serial
  EscECFComando.AddParamInteger( ifthen(Simplificada,2,1) );
  EscECFComando.AddParamInteger( 1 );   // Por Data
  EscECFComando.AddParamDateTime( DataInicial );
  EscECFComando.AddParamDateTime( DataFinal );

  EnviaComando;

  Buffer := '';
  For I := 0 to EscECFResposta.Params.Count-1 do
     Buffer := Buffer + EscECFResposta.Params[I] ;

  Linhas.Clear;
  Linhas.Text := Buffer;
end;

procedure TACBrECFEscECF.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas: TStringList; Simplificada: Boolean);
var
   Buffer: AnsiString;
   I: Integer;
begin
  EscECFComando.CMD := 22;
  EscECFComando.AddParamInteger( 1 ); // Pela Serial
  EscECFComando.AddParamInteger( ifthen(Simplificada,2,1) );
  EscECFComando.AddParamInteger( 2 );   // Por CRZ
  EscECFComando.AddParamInteger( ReducaoInicial );
  EscECFComando.AddParamInteger( ReducaoFinal );

  EnviaComando;

  Buffer := '';
  For I := 0 to EscECFResposta.Params.Count-1 do
     Buffer := Buffer + EscECFResposta.Params[I] ;

  Linhas.Clear;
  Linhas.Text := Buffer;
end;

procedure TACBrECFEscECF.LeituraMFDSerial(DataInicial, DataFinal: TDateTime;
   Linhas: TStringList; Documentos: TACBrECFTipoDocumentoSet);
{var
   Buffer: AnsiString;
   I: Integer;}
begin
  // TODO:  Não há como retornar o espelho pela Serial ??

  Inherited LeituraMFDSerial(DataFinal, DataFinal, Linhas, Documentos);

  {
  EscECFComando.CMD := 100;
  EscECFComando.AddParamInteger( 1 );   // Por Data
  EscECFComando.AddParamDateTime( DataInicial );
  EscECFComando.AddParamDateTime( DataFinal );

  EnviaComando;

  Buffer := '';
  For I := 0 to EscECFResposta.Params.Count-1 do
     Buffer := Buffer + EscECFResposta.Params[I] ;

  Linhas.Clear;
  Linhas.Text := Buffer;
  }
end;

procedure TACBrECFEscECF.LeituraMFDSerial(COOInicial, COOFinal: Integer;
   Linhas: TStringList; Documentos: TACBrECFTipoDocumentoSet);
{var
   Buffer: AnsiString;
   I: Integer;}
begin
  // TODO:  Não há como retornar a MFD pela Serial ??

  Inherited LeituraMFDSerial(COOInicial, COOFinal, Linhas, Documentos);

  {
  EscECFComando.CMD := 100;
  EscECFComando.AddParamInteger( 2 );   // Por COO
  EscECFComando.AddParamDateTime( COOInicial );
  EscECFComando.AddParamDateTime( COOFinal );

  EnviaComando;

  Buffer := '';
  For I := 0 to EscECFResposta.Params.Count-1 do
     Buffer := Buffer + EscECFResposta.Params[I] ;

  Linhas.Clear;
  Linhas.Text := Buffer;
  }
end;

procedure TACBrECFEscECF.EspelhoMFD_DLL(DataInicial, DataFinal: TDateTime;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
  begin
    inherited;
    Exit;
  end;

  try
    Self.Desativar;
    ECFClass.EspelhoMFD_DLL(DataInicial, DataFinal, NomeArquivo, Documentos);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.EspelhoMFD_DLL(COOInicial, COOFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
  begin
    inherited;
    Exit;
  end;

  try
    Self.Desativar;
    ECFClass.EspelhoMFD_DLL(COOInicial, COOFinal, NomeArquivo, Documentos);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMFD_DLL(DataInicial, DataFinal: TDateTime;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
  begin
    inherited;
    Exit;
  end;

  try
    Self.Desativar;
    ECFClass.ArquivoMFD_DLL(DataInicial, DataFinal, NomeArquivo, Documentos, Finalidade);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMFD_DLL(ContInicial, ContFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD; TipoContador: TACBrECFTipoContador);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
  begin
    inherited;
    Exit;
  end;

  try
    Self.Desativar;
    ECFClass.ArquivoMFD_DLL(ContInicial, ContFinal, NomeArquivo, Documentos, Finalidade, TipoContador);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.PafMF_GerarCAT52(const DataInicial,
  DataFinal: TDateTime; const DirArquivos: String; NumeroSerie: String);
var
  ECFClass: TACBrECFClass;
begin
  if NumeroSerie = '' then
    NumeroSerie := GetNumSerie;

  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
  begin
    inherited;
    Exit;
  end;

  try
    Self.Desativar;
    ECFClass.PafMF_GerarCAT52(DataInicial, DataFinal, DirArquivos, NumeroSerie);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.ArquivoMF_Binario_DLL(NomeArquivo);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMFD_Binario_DLL(Tipo: TACBrECFTipoDownloadMFD;
  const NomeArquivo: AnsiString; StrInicial, StrFinal: AnsiString);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.ArquivoMFD_Binario_DLL(Tipo, NomeArquivo, StrInicial, StrFinal);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.IdentificaOperador(Nome: String);
var
  aOperAtual : String;
begin
  aOperAtual := '';
  Nome := LeftStr( Nome, 20 );

  try
    if IsBematech then
    begin
      RetornaInfoECF( '99|05' );
      aOperAtual := EscECFResposta.fsParams[2];
    end

    else if IsDaruma then
      aOperAtual := RetornaInfoECF( '130' )

    else if IsEpson then
      aOperAtual := RetornaInfoECF( '99|17' );
  except
  end;

  if (aOperAtual <> Nome) then
  begin
    EscECFComando.CMD := 154;
    EscECFComando.AddParamString( Nome );
    EnviaComando;
  end;
end;

procedure TACBrECFEscECF.IdentificaPAF(NomeVersao, MD5: String);
var
  AGap: String;
begin
  AGap := '';
  if IsBematech then
    AGap := ' ';

  // para garantir que NomeVersao inicie na linha 2... o Máximo suportado é 84 chars
  fsPAF := LeftStr( PadRight(MD5,fpColunas)+ AGap + NomeVersao, 84) ;
  EscECFComando.CMD := 24;
  EscECFComando.AddParamString( fsPAF ) ;
  EnviaComando;
end;

function TACBrECFEscECF.TraduzirTag(const ATag : AnsiString) : AnsiString ;
begin
  if IsBematech then
    Result := BematechTraduzirTag( ATag )
  else if IsEpson then
    Result := EpsonTraduzirTag( ATag, Self )
  else if IsDaruma then
    Result := DarumaTraduzirTag( ATag )
  else
    Result := inherited TraduzirTag( ATag );
end ;

function TACBrECFEscECF.TraduzirTagBloco(const ATag, Conteudo : AnsiString
   ) : AnsiString ;
begin
  if IsBematech then
    Result := BematechTraduzirTagBloco( ATag, Conteudo, Self)
  else if IsEpson then
    Result := EpsonTraduzirTagBloco( ATag, Conteudo, Self)
  else if IsDaruma then
    Result := DarumaTraduzirTagBloco( ATag, Conteudo, Self)
  else
    Result := inherited TraduzirTagBloco(ATag, Conteudo) ;

  // Carcateres de Controle, devem ser precedidos de ESC //
  if not IsDaruma then
  begin
    Result := ReplaceString( Result, NUL, ESC+NUL);
    Result := ReplaceString( Result, LF , ESC+LF);
  end;

  if IsEpson then
    Result := ReplaceString( Result, CR , ESC+CR);
end ;

procedure TACBrECFEscECF.AbreCupom ;
begin
  EscECFComando.CMD := 1;
  EscECFComando.AddParamString(LeftStr(OnlyNumber(Consumidor.Documento),14)) ;
  EscECFComando.AddParamString(LeftStr(Consumidor.Nome,30)) ;
  EscECFComando.AddParamString(LeftStr(Consumidor.Endereco,79)) ;
  EnviaComando;

  RespostasComando.Clear;
  if (EscECFResposta.Params.Count > 3) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
    RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  end;

  Consumidor.Enviado := True ;
  fsEmPagamento := false ;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.VendeItem(Codigo, Descricao: String;
   AliquotaECF: String; Qtd: Double; ValorUnitario: Double;
   ValorDescontoAcrescimo: Double; Unidade: String;
   TipoDescontoAcrescimo: String; DescontoAcrescimo: String;
   CodDepartamento: Integer);
begin
  Codigo := LeftStr(Codigo,14) ;
  if Length(Codigo) < 3 then
    Codigo := PadLeft(Codigo,3,'0');

  with EscECFComando do
  begin
     CMD := 2 ;
     AddParamString( Codigo );
     AddParamString( LeftStr(Descricao,233) );
     AddParamString( AliquotaECF );
     AddParamString( Trim(LeftStr( OnlyAlphaNum(Unidade),3)) );
     AddParamDouble( Qtd, fpDecimaisQtd );
     AddParamInteger( fpDecimaisQtd );
     AddParamDouble( ValorUnitario, fpDecimaisPreco );
     AddParamInteger( fpDecimaisPreco );
     AddParamString( ifthen(ArredondaItemMFD,'A','T') );
  end ;

  EnviaComando ;

  if (EscECFResposta.Params.Count > 2) then
  begin
    RespostasComando.AddField( 'NumUltItem', EscECFResposta.Params[0] );
    RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[1] );
    RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[2] );
  end;

  fsEmPagamento := false ;

  SalvaRespostasMemoria(True);

  { Se o desconto é maior que zero envia o comando de desconto/acrescimo de item anterior }

  if ValorDescontoAcrescimo > 0 then
     DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo, DescontoAcrescimo,
        TipoDescontoAcrescimo);
end;

procedure TACBrECFEscECF.DescontoAcrescimoItemAnterior(
   ValorDescontoAcrescimo: Double; DescontoAcrescimo: String;
   TipoDescontoAcrescimo: String; NumItem: Integer);
begin
  with EscECFComando do
  begin
     CMD := 27 ;
     AddParamInteger( ifthen(DescontoAcrescimo    ='D',0,1) );
     AddParamInteger( ifthen(TipoDescontoAcrescimo='%',0,1) );
     AddParamDouble( ValorDescontoAcrescimo );
     AddParamInteger( NumItem )
  end ;

  EnviaComando ;

  if (EscECFResposta.Params.Count > 1) then
  begin
    RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[0] );
    RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
  end;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CancelaCupom(NumCOOCancelar: Integer);
var
   UltimoCOO, NumCCDEstornado: Integer;
   Est: TACBrECFEstado;
   TipoUltDocto: TACBrECFTipoDocumento;
   EhUltimoCupom: Boolean;
   TentarEstornarCCDAnteriores, TentarEstornarCCDSeguintes,
     ErroGenericoCancelamenetoBematech: Boolean;
begin
  RespostasComando.Clear;
  Est := TACBrECF( fpOwner ).Estado;

  case Est of
    estRelatorio :
      FechaRelatorio ;

    estVenda, estPagamento, estNaoFiscal :
      begin
        EscECFComando.CMD := 31;
        try
          EnviaComando;
        except
          if fsEmPagamento and (EscECFResposta.CAT = 11) then
          begin
            fsEmPagamento := False;
            CancelaCupom;
          end
          else
            raise;
        end;
      end;
  else
    begin
      NumCCDEstornado := 0;
      UltimoCOO := StrToInt( TACBrECF( fpOwner ).NumCOO );
      EhUltimoCupom := (NumCOOCancelar = 0) or (UltimoCOO = NumCOOCancelar);

      if EhUltimoCupom then
      begin
        TipoUltDocto := TACBrECF( fpOwner ).TipoUltimoDocumento;
        if TipoUltDocto = docCCD then
        begin
          NumCCDEstornado := EstornaCCD( UltimoCOO, False );   // Estorna CCD para trás
          UltimoCOO := UltimoCOO - NumCCDEstornado;
        end;
      end
      else
        UltimoCOO := NumCOOCancelar;

      try
        EscECFComando.CMD := 7;
        EscECFComando.AddParamInteger( UltimoCOO );
        EnviaComando;
      except
        on E: Exception do
        begin
          TentarEstornarCCDSeguintes :=  // Erro específico de Cancelamento por Falta de Cancelamento de CCD //
                         ((EscECFResposta.CAT = 05) and (EscECFResposta.RET.ECF = 10)) or
             (IsEpson and (EscECFResposta.CAT = 16) and (EscECFResposta.RET.ECF = 21));

          ErroGenericoCancelamenetoBematech :=
            (IsBematech and (EscECFResposta.CAT = 02) and (EscECFResposta.RET.ECF = 02));

          TentarEstornarCCDAnteriores :=  // Erro genérico de Cancelamento //
             (not TentarEstornarCCDSeguintes) and
             ( ErroGenericoCancelamenetoBematech or
              (IsEpson    and (EscECFResposta.CAT = 16) and (EscECFResposta.RET.ECF = 18)) or
              (IsDaruma   and (EscECFResposta.CAT = 16) and (EscECFResposta.RET.ECF = 146)));

          if TentarEstornarCCDAnteriores then
          begin
            try
              NumCCDEstornado := EstornaCCD( UltimoCOO, False);   // Estorna CCD para trás
              UltimoCOO := UltimoCOO - NumCCDEstornado;
            except
            end;
          end;

          if TentarEstornarCCDSeguintes then
          begin
            try
              NumCCDEstornado := EstornaCCD(UltimoCOO+1, True);  // Estorna CCD para frente
            except
            end;
          end;

          if NumCCDEstornado > 0 then
            CancelaCupom(UltimoCOO)
          else
          begin
            if ErroGenericoCancelamenetoBematech then
              raise EACBrECFERRO.Create(ACBrStr('Cancelamento não permitido'))
            else
              raise;
          end;
        end;
      end ;
    end;
  end;

  fsEmPagamento := false ;
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CancelaItemVendido(NumItem: Integer);
begin
  EscECFComando.CMD := 3;
  EscECFComando.AddParamInteger( NumItem );
  EnviaComando;

  if (EscECFResposta.Params.Count > 0) then
    RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[0] );

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CancelaItemVendidoParcial(NumItem: Integer;
   Quantidade: Double);
begin
  with EscECFComando do
  begin
     CMD := 151 ;
     AddParamInteger( NumItem );
     AddParamDouble( Quantidade, 3 )
  end ;

  EnviaComando ;

  if (EscECFResposta.Params.Count > 1) then
  begin
    RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[0] );
    RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
  end;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CancelaDescontoAcrescimoItem(NumItem: Integer;
  TipoAcrescimoDesconto: String);
begin
  with EscECFComando do
  begin
     CMD := 28 ;
     AddParamInteger( ifthen(TipoAcrescimoDesconto='D',0,1) );
     AddParamInteger( NumItem )
  end ;

  EnviaComando ;

  if (EscECFResposta.Params.Count > 1) then
  begin
    RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[0] );
    RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
  end;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.AbreNaoFiscal(CPF_CNPJ: String; Nome: String;
   Endereco: String);
begin
  if Trim(CPF_CNPJ) <> '' then
     Consumidor.AtribuiConsumidor(CPF_CNPJ,Nome,Endereco);

  EscECFComando.CMD := 16;
  EscECFComando.AddParamString(LeftStr(OnlyNumber(Consumidor.Documento),14)) ;
  EscECFComando.AddParamString(LeftStr(Consumidor.Nome,30)) ;
  EscECFComando.AddParamString(LeftStr(Consumidor.Endereco,79)) ;
  Consumidor.Zera;

  EnviaComando;

  RespostasComando.Clear;
  if (EscECFResposta.Params.Count > 3) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
    RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  end;

  fsEmPagamento := false ;
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.RegistraItemNaoFiscal(CodCNF: String; Valor: Double;
   Obs: AnsiString);
begin
  EscECFComando.CMD := 17;
  EscECFComando.AddParamString( CodCNF ) ;
  EscECFComando.AddParamDouble( Valor ) ;
  EnviaComando;

  if (EscECFResposta.Params.Count > 1) then
  begin
    RespostasComando.AddField( 'NumUltItem', EscECFResposta.Params[0] );
    RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
  end;

  fsEmPagamento := false ;
  SalvaRespostasMemoria(True);
end;

procedure TACBrECFEscECF.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
Var
  NumPagtos : Integer;
  TotPag : Double;
  TotalAPagar: String;
begin
  if (CodMeioPagamento <= 0) or (CodMeioPagamento > 7) then
    CodMeioPagamento := 7;
  { 1-Dinheiro, 2-Cheque, 3-Cartão de Crédito, 4-Cartão de Débito,
     5-Cartão Refeição/Alimentação, 6-Vale Refeição/Alimentação (em papel),
     7-Outros }

  with EscECFComando do
  begin
     CMD := 4 ;
     AddParamString( CodFormaPagto );
     AddParamDouble( Valor );
     AddParamInteger( 1 );  // Parcelas ??
     AddParamString( LeftStr(Observacao, 84) );
     AddParamInteger( CodMeioPagamento );
  end ;

  EnviaComando ;

  try
    NumPagtos := RespostasComando.FieldByName('NumPagtos').AsInteger;
  except
    NumPagtos := 0;
  end ;

  try
    TotPag := RespostasComando.FieldByName('TotalPago').AsFloat;
  except
    TotPag := 0;
  end ;

  try
    TotalAPagar := EscECFResposta.Params[0]
  except
    TotalAPagar := '000';
  end;

  Inc( NumPagtos ) ;
  RespostasComando.AddField( 'NumPagtos', IntToStr(NumPagtos) );
  RespostasComando.AddField( 'Pagto'+IntToStr(NumPagtos),
     CodFormaPagto+'|'+FloatToStr(Valor)+'|'+IntToStr(CodMeioPagamento) );
  RespostasComando.AddField( 'TotalAPagar',  TotalAPagar );
  RespostasComando.AddField( 'TotalPago',  FloatToIntStr(Valor + TotPag) );

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.EstornaPagamento(const CodFormaPagtoEstornar,
   CodFormaPagtoEfetivar: String; const Valor: Double; Observacao: AnsiString);
begin
  with EscECFComando do
  begin
     CMD := 19 ;
     AddParamString( CodFormaPagtoEstornar );
     AddParamString( CodFormaPagtoEfetivar );
     AddParamDouble( Valor );
     AddParamInteger( 1 );  // Parcelas ??
     AddParamString( LeftStr(Observacao, 84) );
  end ;

  EnviaComando ;

  if (EscECFResposta.Params.Count > 3) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
    RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  end;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.FechaCupom(Observacao: AnsiString; IndiceBMP: Integer);
begin
  if not Consumidor.Enviado then
     EnviaConsumidor;

  AjustaComandosControleImpressao(Observacao);

  with EscECFComando do
  begin
     CMD := 5 ;
     AddParamInteger( 0 );  // Sem Cupom Adicional
     AddParamInteger( 1 );  // Aciona a Guilhotina
     AddParamString( Observacao );
  end ;
  EnviaComando ;

  if (EscECFResposta.Params.Count > 2) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  end;

  fsEmPagamento := false ;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString );
begin
  if DescontoAcrescimo = 0 then
  begin
    fsEmPagamento := True ;
    Exit ;
  end;

  with EscECFComando do
  begin
     CMD := 29 ;
     AddParamInteger( ifthen(DescontoAcrescimo < 0,0,1) );
     AddParamInteger( 1 );
     AddParamDouble( abs(DescontoAcrescimo) );
  end ;
  EnviaComando ;

  if (EscECFResposta.Params.Count > 0) then
    RespostasComando.AddField( 'SubTotal', EscECFResposta.Params[0] );

  SalvaRespostasMemoria(False);
  fsEmPagamento := True ;
end;

procedure TACBrECFEscECF.CancelaDescontoAcrescimoSubTotal(
   TipoAcrescimoDesconto: Char);
begin
  with EscECFComando do
  begin
     CMD := 30 ;
     AddParamInteger( ifthen(TipoAcrescimoDesconto = 'D',0,1) );
  end ;
  EnviaComando ;

  if (EscECFResposta.Params.Count > 0) then
    RespostasComando.AddField( 'SubTotal', EscECFResposta.Params[0] );

  SalvaRespostasMemoria(False);
end;


procedure TACBrECFEscECF.CarregaAliquotas;
Var
  I, N :Integer ;
  Aliquota: TACBrECFAliquota;
begin
  try
    RetornaInfoECF( '5' );
  except
    on Exception do
    begin
      if not ( (EscECFResposta.CAT = 2) and (EscECFResposta.RET.ECF = 1) and IsEpson ) then
         raise;
    end;
  end;

  inherited CarregaAliquotas;

  try
     N := Trunc(EscECFResposta.Params.Count / 4) - 1;
     For I := 0 to N do
     begin
       Aliquota := TACBrECFAliquota.create;
       Aliquota.Sequencia := I;
       Aliquota.Tipo      := EscECFResposta.Params[ 4*I + 1 ][1] ;
       { Adiciona o tipo no Indice, pois no comando de Venda de Item ele será necessario }
       Aliquota.Indice    := Aliquota.Tipo + EscECFResposta.Params[ 4*I ] ;
       Aliquota.Aliquota  := StrToIntDef( OnlyNumber(EscECFResposta.Params[ 4*I + 2 ]), 0 ) / 100 ;

       { IMPORTANTE: MP4200TH-FI retorna o Valor do Imposto e não a BASE DE CALCULO, como esperado }
       Aliquota.Total     := StrToFloatDef( EscECFResposta.Params[ 4*I + 3 ], 0 ) / 100 ;

       fpAliquotas.Add(Aliquota);
     end;
  except
     fpAliquotas.Free ;
     fpAliquotas := nil ;
     raise ;
  end;
end;

procedure TACBrECFEscECF.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String );
var
   PosAliq: Integer;
begin
  if not Assigned( fpAliquotas ) then
     CarregaAliquotas ;

  EscECFComando.CMD := 81;

  if Aliquotas.Count < 1 then
    PosAliq := 1
  else
    PosAliq := StrToIntDef( Aliquotas[ Aliquotas.Count-1 ].Indice, Aliquotas.Count)+1;

  if Posicao <> '' then
     PosAliq := StrToIntDef( Posicao, PosAliq );

  EscECFComando.AddParamInteger( PosAliq ) ;
  EscECFComando.AddParamString( Tipo ) ;
  EscECFComando.AddParamString( IntToStrZero( TruncFix(Aliquota*100), 4 ) );
  EnviaComando;

  CarregaAliquotas;
end;

procedure TACBrECFEscECF.CarregaTotalizadoresNaoTributados;
var
  I: Integer;
begin
  if Assigned( fpTotalizadoresNaoTributados ) then
     fpTotalizadoresNaoTributados.Free ;

  fpTotalizadoresNaoTributados := TACBrECFTotalizadoresNaoTributados.create( true ) ;

  RetornaInfoECF( '6|0' );

  I := 0;
  while I < EscECFResposta.Params.Count-1 do
  begin
    with fpTotalizadoresNaoTributados.New do
    begin
      Indice := EscECFResposta.Params[I];
      Tipo   := IfThen(pos('S',Indice)>0,'S','T')[1];
      Total  := StrToFloatDef(EscECFResposta.Params[I+1],0)/100;
    end;

    Inc( I, 2 );
  end;
end;

procedure TACBrECFEscECF.LerTotaisTotalizadoresNaoTributados;
begin
  CarregaTotalizadoresNaoTributados;
end;

procedure TACBrECFEscECF.CarregaFormasPagamento;
Var
  I, N :Integer ;
  FPG: TACBrECFFormaPagamento;
begin
  RetornaInfoECF( '14' );

  inherited CarregaFormasPagamento;

  try
     N := Trunc(EscECFResposta.Params.Count / 3) - 1;
     For I := 0 to N do
     begin
       FPG := TACBrECFFormaPagamento.create;
       FPG.Indice           := EscECFResposta.Params[ 3*I ] ;
       FPG.Descricao        := EscECFResposta.Params[ 3*I + 1 ] ;
       FPG.PermiteVinculado := (EscECFResposta.Params[ 3*I + 2 ] = '1') ;

       fpFormasPagamentos.Add(FPG);
     end;
  except
     fpFormasPagamentos.Free ;
     fpFormasPagamentos := nil ;
     raise ;
  end;
end;

procedure TACBrECFEscECF.LerTotaisFormaPagamento;
Var
  I, N :Integer ;
  FPG: TACBrECFFormaPagamento;
begin
  if not Assigned( fpFormasPagamentos ) then
     CarregaFormasPagamento ;

  RetornaInfoECF( '7' );

  N := Trunc(EscECFResposta.Params.Count / 2) - 1;
  For I := 0 to N do
  begin
    FPG := AchaFPGIndice( IntToStr(StrToIntDef(EscECFResposta.Params[ 2*I ], 0)) ) ;
    if Assigned( FPG ) then
       FPG.Total := StrToFloatDef( EscECFResposta.Params[ 2*I + 1 ], 0 ) / 100;
  end;
end;

procedure TACBrECFEscECF.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String);
var
   PosFPG : Integer;
begin
  if not Assigned( fpFormasPagamentos ) then
     CarregaFormasPagamento ;

  Descricao := AjustaDescricao( Descricao );

  EscECFComando.CMD := 84;

  if FormasPagamento.Count < 1 then
    PosFPG := 1
  else
    PosFPG := StrToIntDef( FormasPagamento[ FormasPagamento.Count-1 ].Indice, FormasPagamento.Count)+1;

  if Posicao <> '' then
     PosFPG := StrToIntDef( Posicao, PosFPG );

  EscECFComando.AddParamInteger( PosFPG ) ;
  EscECFComando.AddParamString( Descricao ) ;
  EscECFComando.AddParamInteger( ifthen(PermiteVinculado,1,0) ) ;
  EnviaComando;

  CarregaFormasPagamento;
end;

function TACBrECFEscECF.AchaFPGDescricao(Descricao: String;
  BuscaExata: Boolean; IgnorarCase: Boolean; IgnorarAcentos: Boolean
  ): TACBrECFFormaPagamento;
begin
  Descricao := AjustaDescricao( Descricao );
  Result    := inherited AchaFPGDescricao(Descricao, BuscaExata, IgnorarCase, IgnorarAcentos) ;
end ;

procedure TACBrECFEscECF.CarregaRelatoriosGerenciais;
Var
  I, N :Integer ;
  RelGer: TACBrECFRelatorioGerencial;
begin
  RetornaInfoECF( '13' );

  inherited CarregaRelatoriosGerenciais;

  try
     N := Trunc(EscECFResposta.Params.Count / 2) - 1;
     For I := 0 to N do
     begin
       RelGer := TACBrECFRelatorioGerencial.create;
       RelGer.Indice    := EscECFResposta.Params[ 2*I ] ;
       RelGer.Descricao := EscECFResposta.Params[ 2*I + 1 ] ;

       fpRelatoriosGerenciais.Add(RelGer);
     end;
  except
     fpRelatoriosGerenciais.Free ;
     fpRelatoriosGerenciais := nil ;
     raise ;
  end;
end;

procedure TACBrECFEscECF.LerTotaisRelatoriosGerenciais;
Var
  I, N :Integer ;
  RelGer : TACBrECFRelatorioGerencial;
begin
  if not Assigned( fpRelatoriosGerenciais ) then
     CarregaRelatoriosGerenciais ;

  RetornaInfoECF( '2' );

  N := Trunc(EscECFResposta.Params.Count / 2) - 1;
  For I := 0 to N do
  begin
    RelGer := AchaRGIndice( EscECFResposta.Params[ 2*I ] ) ;
    if Assigned( RelGer ) then
       RelGer.Contador := StrToIntDef( EscECFResposta.Params[ 2*I + 1 ], 0 ) ;
  end;
end;

procedure TACBrECFEscECF.ProgramaRelatorioGerencial(var Descricao: String;
   Posicao: String);
var
   PosRel : Integer;
begin
  if not Assigned( fpRelatoriosGerenciais ) then
     CarregaRelatoriosGerenciais ;

  Descricao := AjustaDescricao( Descricao );

  EscECFComando.CMD := 86;

  if RelatoriosGerenciais.Count < 1 then
    PosRel := 1
  else
    PosRel := StrToIntDef( RelatoriosGerenciais[ RelatoriosGerenciais.Count-1 ].Indice, RelatoriosGerenciais.Count)+1;

  if Posicao <> '' then
     PosRel := StrToIntDef( Posicao, PosRel );

  EscECFComando.AddParamInteger( PosRel ) ;
  EscECFComando.AddParamString( Descricao ) ;
  EnviaComando;

  CarregaRelatoriosGerenciais;
end;

function TACBrECFEscECF.AchaRGDescricao(Descricao : String ;
   BuscaExata : Boolean ; IgnorarCase : Boolean) : TACBrECFRelatorioGerencial ;
begin
  Descricao := AjustaDescricao( Descricao );
  Result    := inherited AchaRGDescricao(Descricao, BuscaExata, IgnorarCase) ;
end ;

procedure TACBrECFEscECF.CarregaComprovantesNaoFiscais;
Var
  I, N :Integer ;
  CNF : TACBrECFComprovanteNaoFiscal;
begin
  RetornaInfoECF( '12' );

  inherited CarregaComprovantesNaoFiscais;

  try
     N := Trunc(EscECFResposta.Params.Count / 2) - 1;
     For I := 0 to N do
     begin
       CNF := TACBrECFComprovanteNaoFiscal.create;
       CNF.Indice    := EscECFResposta.Params[ 2*I ] ;
       CNF.Descricao := EscECFResposta.Params[ 2*I + 1 ] ;

       fpComprovantesNaoFiscais.Add(CNF);
     end;
  except
     fpComprovantesNaoFiscais.Free ;
     fpComprovantesNaoFiscais := nil ;
     raise ;
  end;
end;

procedure TACBrECFEscECF.LerTotaisComprovanteNaoFiscal;
Var
  I, N :Integer ;
  CNF : TACBrECFComprovanteNaoFiscal;
begin
  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais ;

  RetornaInfoECF( '3' );

  N := Trunc(EscECFResposta.Params.Count / 3) - 1;
  For I := 0 to N do
  begin
    CNF := AchaCNFIndice( EscECFResposta.Params[ 3*I ] ) ;
    if Assigned( CNF ) then
    begin
       CNF.Contador := StrToIntDef( EscECFResposta.Params[ 3*I + 1 ], 0 ) ;
       CNF.Total    := StrToFloatDef( EscECFResposta.Params[ 3*I + 2 ], 0 ) / 100 ;
    end;
  end;
end;

procedure TACBrECFEscECF.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String );
var
   PosCNF : Integer;
begin
  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais ;

  Descricao := AjustaDescricao( Descricao );

  if (Tipo = '-') then
     Tipo := 'S'
  else
     Tipo := 'E' ;

  EscECFComando.CMD := 85;

  if ComprovantesNaoFiscais.Count < 1 then
    PosCNF := 1
  else
    PosCNF := StrToIntDef( ComprovantesNaoFiscais[ ComprovantesNaoFiscais.Count-1 ].Indice, ComprovantesNaoFiscais.Count)+1;

  if Posicao <> '' then
     PosCNF := StrToIntDef( Posicao, PosCNF );

  EscECFComando.AddParamInteger( PosCNF ) ;
  EscECFComando.AddParamString( Descricao ) ;
  EscECFComando.AddParamString( Tipo ) ;
  EnviaComando;

  CarregaComprovantesNaoFiscais;
end;

function TACBrECFEscECF.AchaCNFDescricao(Descricao : String ;
   BuscaExata : Boolean ; IgnorarCase : Boolean
   ) : TACBrECFComprovanteNaoFiscal ;
begin
  Descricao := AjustaDescricao( Descricao );
  Result    := inherited AchaCNFDescricao(Descricao, BuscaExata, IgnorarCase) ;
end ;

procedure TACBrECFEscECF.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial(Linha);
end;

procedure TACBrECFEscECF.SegundaViaVinculado;
begin
  EscECFComando.CMD := 14;
  EnviaComando;
end;

procedure TACBrECFEscECF.ReimpressaoVinculado;
begin
  EscECFComando.CMD := 15;
  EnviaComando;
end;

procedure TACBrECFEscECF.FechaRelatorio;
begin
  EscECFComando.CMD := 10;
  EscECFComando.AddParamInteger( 1 );  // Aciona a Guilhotina
  EnviaComando;

  if (EscECFResposta.Params.Count > 2) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  end;

  fsEmPagamento := false ;
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CortaPapel(const CorteParcial: Boolean);
begin
  EscECFComando.CMD := 11;
  EnviaComando;
  Sleep( 100 );
end;

function TACBrECFEscECF.GetNumCRZ: String;
begin
  RetornaInfoECF( '1|4' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumCFD: String;
begin
  RetornaInfoECF( '1|7' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumNCN: String;
begin
  RetornaInfoECF( '1|14' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetGrandeTotal: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|1' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end;

function TACBrECFEscECF.GetNumCCF: String;
begin
  RetornaInfoECF( '1|5' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumGNF: String;
begin
  RetornaInfoECF( '1|2' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumGRG: String;
begin
  RetornaInfoECF( '1|9' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumCDC: String;
begin
  RetornaInfoECF( '1|8' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumCFC: String;
begin
  RetornaInfoECF( '1|11' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumGNFC: String;
begin
  RetornaInfoECF( '1|10' ) ;
  Result := EscECFResposta.Params[1] ;
end;

function TACBrECFEscECF.GetNumCOOInicial: String;
begin
  RetornaInfoECF( '8' ) ;
  Result := EscECFResposta.Params[2] ;
end;

function TACBrECFEscECF.GetNumUltimoItem: Integer;
begin
  if IsEpson then
  begin
    RetornaInfoECF('99|03');
    Result := StrToIntDef( EscECFResposta.Params[0], 0) ;
  end
  else
  begin
    try
      Result := RespostasComando.FieldByName('NumUltItem').AsInteger;
    except
      Result := 0;
    end ;
  end;
end;

function TACBrECFEscECF.GetDadosUltimaReducaoZ : String ;
var
  DataStr, Reg : String ;
  I, J, N, ECFCRZ, RZCRZ, Tentativas: Integer;
  AliqZ : TACBrECFAliquota ;
  ValReg: Double;

  function AchaValorRegistrador(Registrador: String; Aliq: Double = 0): Double ;
  var
    I: Integer;
    AliqStr: String;
    AliqDbl: Double;
    Achou: Boolean;
  begin
    I      := 0 ;
    Result := -1;
    Achou  := False;
    while (not Achou) and (I+2 < EscECFResposta.Params.Count) do
    begin
      if (EscECFResposta.Params[I] = Registrador) then
      begin
        if (Aliq = 0) and IsDaruma then
        begin
          { Daruma não retorna Linha com Aliquota "00.00", quando é um
            Totalizador não Tributado, Portanto I+1 = Valor do Totalizador... }
          Achou := True;
          Dec( I );
        end
        else
        begin
          AliqStr := EscECFResposta.Params[I+1];
          if (pos('.',AliqStr) = 0) and (pos(',',AliqStr) = 0) then  // Não tem ponto decimal ?
            AliqDbl := StrToIntDef(AliqStr,0) / 100
          else
            AliqDbl := StringToFloatDef(AliqStr, 0);

          Achou := (AliqDbl = Aliq);
        end;
      end;

      if not Achou then
        Inc( I ) ;
    end ;

    if Achou then
      Result := RoundTo( StrToFloatDef(EscECFResposta.Params[ I+2 ], -100)/100, -2);
  end ;
begin
  // Zerar variaveis e inicializa Dados do ECF //
  InitDadosUltimaReducaoZ;

  if not Assigned( fpAliquotas ) then
    CarregaAliquotas ;

  with TACBrECF(fpOwner) do
  begin
    ECFCRZ := StrToIntDef(Trim(NumCRZ), -1);
    if ECFCRZ < 0 then
      raise EACBrECFERRO.Create('Erro ao obter CRZ atual') ;
  end;

  Tentativas := 0;
  repeat
    RetornaInfoECF( '17|'+IntToStr(ECFCRZ) ) ;
    // DEBUG
    //WriteToTXT('C:\TEMP\REDZ.TXT', EscECFResposta.Params.Text, False, False);
    if (UpperCase(copy(EscECFResposta.Params.Text, 0, 5)) = 'ERRO:')  then
      raise EACBrECFERRO.Create(ACBrStr(EscECFResposta.Params.Text)) ;
    Inc( Tentativas );
  until (EscECFResposta.TBR > 0) or (Tentativas > 2);

  if (EscECFResposta.TBR = 0) then
    raise EACBrECFERRO.Create(ACBrStr('Erro ao obter Dados da Redução Z: '+IntToStr(ECFCRZ))) ;

  RZCRZ := StrToIntDef( EscECFResposta.Params[0], 0) ;

  if IsBematech and (ECFCRZ <> RZCRZ) then
  begin
    { Bematech MP4200 TH-FI II, versões até 01.00.02 e inferiores, tem problemas
      para ler dados da Ultima Redução Z, se o CRZ for maior que 256.
      Se o CRZ lido for difernte do solicitado, é porque o problema ocorreu...
      Nesse caso, vamos capturar as informações de uma LeituraMemoriaFiscalSerial }
    BematechObtemDadosUltimaReducaoZDeLeituraMemoriaFiscal( ECFCRZ );
    RetornaInfoECF( '8' ) ;
    fpDadosReducaoZClass.ValorGrandeTotal := RoundTo( StrToFloatDef(EscECFResposta.Params[3],0)/100, -2);
  end
  else
  begin
    { Alimenta a class com os dados atuais do ECF }
    with fpDadosReducaoZClass do
    begin
      CRZ              := RZCRZ;
      DataStr          := EscECFResposta.Params[1];
      DataDoMovimento  := EncodeDate( StrToInt(copy(DataStr,5,4)),   // Ano
                                      StrToInt(copy(DataStr,3,2)),   // Mes
                                      StrToInt(copy(DataStr,1,2)) ); // Dia

      DataStr := EscECFResposta.Params[2];
      DataHoraEmissao := EncodeDateTime(StrToInt(copy(DataStr, 5,4)),   // Ano
                                        StrToInt(copy(DataStr, 3,2)),   // Mes
                                        StrToInt(copy(DataStr, 1,2)),   // Dia
                                        StrToInt(copy(DataStr, 9,2)),   // Hora
                                        StrToInt(copy(DataStr, 11,2)),   // Min
                                        StrToInt(copy(DataStr, 13,2)),   // Seg
                                        0 );

      CRO              := StrToIntDef( EscECFResposta.Params[3], 0) ;
      NumeroCOOInicial := EscECFResposta.Params[4];
      COO              := StrToIntDef( EscECFResposta.Params[5], 0) ;
      ValorVendaBruta  := RoundTo( StrToFloatDef(EscECFResposta.Params[07],0)/100, -2);
      DescontoICMS     := RoundTo( StrToFloatDef(EscECFResposta.Params[08],0)/100, -2);
      AcrescimoICMS    := RoundTo( StrToFloatDef(EscECFResposta.Params[09],0)/100, -2);
      CancelamentoICMS := RoundTo( StrToFloatDef(EscECFResposta.Params[10],0)/100, -2);
      DescontoISSQN    := RoundTo( StrToFloatDef(EscECFResposta.Params[11],0)/100, -2);
      AcrescimoISSQN   := RoundTo( StrToFloatDef(EscECFResposta.Params[12],0)/100, -2);
      CancelamentoISSQN:= RoundTo( StrToFloatDef(EscECFResposta.Params[13],0)/100, -2);

      {Aliquotas}
      {Percorrendo as aliquotas cadastradas no ECF para procurar por todas}
      for I := 0 to Aliquotas.Count - 1 do
      begin
        AliqZ := TACBrECFAliquota.Create ;
        AliqZ.Assign( fpAliquotas[I] );
        {Procura pela aliquota no formato T/Snnnn na string}
        ValReg := AchaValorRegistrador( AliqZ.Tipo, AliqZ.Aliquota ) ;
        if ValReg >= 0 then
          AliqZ.Total := ValReg;

        AdicionaAliquota( AliqZ );
      end ;

      ValReg := AchaValorRegistrador('F1');
      if ValReg >= 0 then
        SubstituicaoTributariaICMS := ValReg;

      ValReg := AchaValorRegistrador('F2');
      if ValReg >= 0 then
        SubstituicaoTributariaICMS := max(SubstituicaoTributariaICMS,0) + ValReg;

      ValReg := AchaValorRegistrador('F3');
      if ValReg >= 0 then
        SubstituicaoTributariaICMS := max(SubstituicaoTributariaICMS,0) + ValReg;


      ValReg := AchaValorRegistrador('N1');
      if ValReg >= 0 then
        NaoTributadoICMS := ValReg;

      ValReg := AchaValorRegistrador('N2');
      if ValReg >= 0 then
        NaoTributadoICMS := max(NaoTributadoICMS,0) + ValReg;

      ValReg := AchaValorRegistrador('N3');
      if ValReg >= 0 then
        NaoTributadoICMS := max(NaoTributadoICMS,0) + ValReg;


      ValReg := AchaValorRegistrador('I1');
      if ValReg >= 0 then
        IsentoICMS := ValReg;

      ValReg := AchaValorRegistrador('I2');
      if ValReg >= 0 then
        IsentoICMS := max(IsentoICMS,0) + ValReg;

      ValReg := AchaValorRegistrador('I3');
      if ValReg >= 0 then
        IsentoICMS := max(IsentoICMS,0) + ValReg;


      ValReg := AchaValorRegistrador('FS1');
      if ValReg >= 0 then
        SubstituicaoTributariaISSQN := ValReg;

      ValReg := AchaValorRegistrador('FS2');
      if ValReg >= 0 then
        SubstituicaoTributariaISSQN := max(SubstituicaoTributariaISSQN,0) + ValReg;

      ValReg := AchaValorRegistrador('FS3');
      if ValReg >= 0 then
        SubstituicaoTributariaISSQN := max(SubstituicaoTributariaISSQN,0) + ValReg;


      ValReg := AchaValorRegistrador('NS1');
      if ValReg >= 0 then
        NaoTributadoISSQN := ValReg;

      ValReg := AchaValorRegistrador('NS2');
      if ValReg >= 0 then
        NaoTributadoISSQN := max(NaoTributadoISSQN,0) + ValReg;

      ValReg := AchaValorRegistrador('NS3');
      if ValReg >= 0 then
        NaoTributadoISSQN := max(NaoTributadoISSQN,0) + ValReg;


      ValReg := AchaValorRegistrador('IS1');
      if ValReg >= 0 then
        IsentoISSQN := ValReg;

      ValReg := AchaValorRegistrador('IS2');
      if ValReg >= 0 then
        IsentoISSQN := max(IsentoISSQN,0) + ValReg;

      ValReg := AchaValorRegistrador('IS3');
      if ValReg >= 0 then
        IsentoISSQN := max(IsentoISSQN,0) + ValReg;

      { Epson não retorna Totalizadores não tributados no comando padrão, usando
        comando específico do Fabricante }
      if IsEpson then
      begin
        RetornaInfoECF('99|30&'+IntToStr(ECFCRZ));

        // Calculando a posição do "Total de alíquotas não tributadas"
        I := 4 + (StrToIntDef( EscECFResposta.Params[4], 0) * 3) + 1;
        I := I + (StrToIntDef( EscECFResposta.Params[I], 0) * 3) + 2;
        I := I + (StrToIntDef( EscECFResposta.Params[I], 0) * 3) + 2;
        I := I + (StrToIntDef( EscECFResposta.Params[I], 0) * 2) + 2;

        GNF := StrToIntDef( EscECFResposta.Params[I], 0) ;
        I := I + 3;
        CCF := StrToIntDef( EscECFResposta.Params[I], 0) ;
        I := I + 1;
        CFD := StrToIntDef( EscECFResposta.Params[I], 0) ;
        I := I + 1;
        CCDC := StrToIntDef( EscECFResposta.Params[I], 0) ;
        I := I + 1;
        GRG := StrToIntDef( EscECFResposta.Params[I], 0) ;
        I := I + 2;
        CFC := StrToIntDef( EscECFResposta.Params[I], 0) ;
        I := I + 3;
        ValorGrandeTotal := RoundTo( StrToFloatDef(EscECFResposta.Params[I],0)/100, -2);
        I := I + 14;
        I := I + (StrToIntDef( EscECFResposta.Params[I], 0) * 3) + 2;
        I := I + (StrToIntDef( EscECFResposta.Params[I], 0) * 3) + 3;

        N := StrToIntDef( EscECFResposta.Params[I], 0);
        for J := 1 to N do
        begin
          Reg    := Copy( EscECFResposta.Params[I+J], 1, 1);
          ValReg := RoundTo( StrToFloatDef(EscECFResposta.Params[I+J+N],0)/100, -2);

          if (Reg = 'F') then
            SubstituicaoTributariaICMS := max(SubstituicaoTributariaICMS,0) + ValReg
          else if (Reg = 'N') then
            NaoTributadoICMS := max(NaoTributadoICMS,0) + ValReg
          else if (Reg = 'I') then
            IsentoICMS := max(IsentoICMS,0) + ValReg
        end ;

        I := I + (StrToIntDef( EscECFResposta.Params[I], 0) * 2) + 1;

        N := StrToIntDef( EscECFResposta.Params[I], 0);
        for J := 1 to N do
        begin
          Reg    := Copy( EscECFResposta.Params[I+J], 1, 2);
          ValReg := RoundTo( StrToFloatDef(EscECFResposta.Params[I+J+N],0)/100, -2);

          if (Reg = 'FS') then
            SubstituicaoTributariaISSQN := max(SubstituicaoTributariaISSQN,0) + ValReg
          else if (Reg = 'NS') then
            NaoTributadoISSQN := max(NaoTributadoISSQN,0) + ValReg
          else if (Reg = 'IS') then
            IsentoISSQN := max(IsentoISSQN,0) + ValReg
        end ;
      end
      else
      begin
        { EscESC não retorna o GT em leitura de Dados da Ultima Reducao Z,
          Usando o GTInicial deste movimento }

        RetornaInfoECF( '8' ) ;
        ValorGrandeTotal := RoundTo( StrToFloatDef(EscECFResposta.Params[3],0)/100, -2);
      end;
    end;
  end;

  with fpDadosReducaoZClass do
  begin
    CalculaValoresVirtuais;
    Result := MontaDadosReducaoZ;
  end;
end ;

{ Bematech MP4200 TH-FI II, versões até 01.00.02 e inferiores, tem problemas
  para ler dados da Ultima Redução Z, se o CRZ for maior que 256.
  Nesse caso, vamos capturar as informações de uma LeituraMemoriaFiscalSerial }
procedure TACBrECFEscECF.BematechObtemDadosUltimaReducaoZDeLeituraMemoriaFiscal(
  ANumCRZ: Integer);
var
  SL, Totalizadores: TStringList;
  I, J: Integer;
  AStr, NomeTotalizador: String;
  AliqZ: TACBrECFAliquota;
  ValReg: Double;

  function EncontrarInicioLinha(StringEncontrar: String): Boolean;
  var
    wTamanho: Integer;
  begin
    Result   := False;
    wTamanho := Length(StringEncontrar);

    while (I < SL.Count) do
    begin
      Result := (Copy(SL[I], 1, wTamanho) = StringEncontrar);
      if Result then
        Break;

      Inc(I);
    end;
  end;

  function ValorStrToFloat(AString: String): Double;
  begin
    AString := StringReplace(Trim(AString), '.', '', [rfReplaceAll]);  // Remove pontos
    Result  := StringToFloatDef(AString, -1 ) ;
  end;

  procedure AdicionarTotalizador(Linha: String);
  var
    P: Integer;
    NomeTot: String;
    ValorTot: Double;
  begin
    P := pos('=', Linha);
    if P = 0  then
      exit;

    NomeTot  := Trim(copy(Linha, 1, P-1));
    ValorTot := ValorStrToFloat(copy(Linha, P+1, Length(Linha)));

    Totalizadores.Add(NomeTot + '=' + FloatToStr(ValorTot));
  end;

  procedure ExtrairTotalizadores;
  var
    LinhaTracejada: String;
  begin
    LinhaTracejada := StringOfChar('-',40);
    Totalizadores.Clear;
    while (I < SL.Count) do
    begin
      if (pos(LinhaTracejada , SL[I] ) > 0) then
        Break;

      AdicionarTotalizador( copy( SL[I], 1, 24) );
      AdicionarTotalizador( copy( SL[I],25, 24) );

      Inc(I);
    end;
  end;

  function AcharValorTotalizador(NomeTot: String): Double;
  var
    P: Integer;
  begin
    Result := -1;
    P := Totalizadores.IndexOfName(NomeTot);
    if P >= 0 then
    begin
      Result := StrToFloatDef(Totalizadores.ValueFromIndex[P], -1);
      Totalizadores.Delete(P);  // Remove para não achar novamente
    end;
  end;

begin
{
------------------------------------------------
                   RAZÃO SOCIAL
                  NOME FANTASIA
                 EMDEREÇO E BAIRRO
            CEP: 99999-999 CIDADE - UF
     CNPJ: 99.999.999/9999-99 IE: ISENTO
------------------------------------------------
19/08/2016 19:03:07V               COO:000005080

             LEITURA MEMÓRIA FISCAL
              REDUÇÃO: 0192 a 0192

Geral de Operação Não-Fiscal:          000001884
Contador de Reduções Z:                     0194
Contador de Reinício de Operação:            001
Contador de Fita-Detalhe:                 000000
Contador Geral de CF Cancelado              0558
Tentativas Mal Suced. de Sub. SB            0000
------------------INTERVENÇÕES------------------
CRO  TIPO  DATA       HORA       CRZ   COO
001  L     10/07/2013 11:34:57   0000  000000003
-----------IMPRESSÃO DE FITA DETALHE------------
CFD      DATA       HORA        COOi     COOf
---------------REDUÇÕES Z DIÁRIAS---------------
CRZ    TR   CRO   COO      DATA       HORA
CFC                       VENDA BRUTA DIÁRIA(R$)
DT = desconto ICMS      DS = desconto ISSQN
AT = acréscimo ICMS     AS = acréscimo ISSQN
CT = canc. ICMS         CS = canc. ISSQN
ONE = Operação não-fiscal de entrada
ONS = Operação não-fiscal de saída
------------------------------------------------
CRZ    TR   CRO   COO      DATA       HORA
0192   0   001  000005067  04/07/2016 08:32:35V
CFC                       VENDA BRUTA DIÁRIA(R$)
000005                                     51,66
DT = 0,00               DS = 0,00
AT = 0,00               AS = 0,00
CT = 49,66              CS = 0,00
T18,00% = 2,00          T18,00% = 0,00
T07,00% = 0,00          T12,00% = 0,00
T25,00% = 0,00          T17,00% = 0,00
T03,20% = 0,00          T05,20% = 0,00
T12,00% = 0,00          T07,00% = 0,00
T25,00% = 0,00          T04,00% = 0,00
F1 = 0,00               I1 = 0,00
N1 = 0,00               S05,00% = 0,00
S12,00% = 0,00          S08,00% = 0,00
FS1 = 0,00              IS1 = 0,00
NS1 = 0,00
ONE = 0,00              ONS = 0,00
------------------------------------------------
-----------TOTAL DO MES JULHO DE 2016-----------
Venda Bruta(R$)                            51,66
DT = 0,00               DS = 0,00
AT = 0,00               AS = 0,00
CT = 49,66              CS = 0,00
T03,20% = 0,00          T04,00% = 0,00
T05,20% = 0,00          T07,00% = 0,00
T12,00% = 0,00          T17,00% = 0,00
T18,00% = 2,00          T25,00% = 0,00
F1 = 0,00               I1 = 0,00
N1 = 0,00               S05,00% = 0,00
S08,00% = 0,00          S12,00% = 0,00
FS1 = 0,00              IS1 = 0,00
NS1 = 0,00
ONE = 0,00              ONS = 0,00
-----------TOTAL DO PERÍODO DA LEITURA----------
Venda Bruta(R$)                            51,66
DT = 0,00               DS = 0,00
AT = 0,00               AS = 0,00
CT = 49,66              CS = 0,00
T03,20% = 0,00          T04,00% = 0,00
T05,20% = 0,00          T07,00% = 0,00
T12,00% = 0,00          T17,00% = 0,00
T18,00% = 2,00          T25,00% = 0,00
F1 = 0,00               I1 = 0,00
N1 = 0,00               S05,00% = 0,00
S08,00% = 0,00          S12,00% = 0,00
FS1 = 0,00              IS1 = 0,00
NS1 = 0,00
ONE = 0,00              ONS = 0,00
------------------DADOS GERAIS------------------
Quantidade de Reduções Restantes            3456
MOEDA: R$                   19/06/2013 14:00:06
-----------------SOFTWARE BÁSICO----------------
01.00.00                    19/06/2013 14:00:02
--------TENTATIVAS DE SUBSTITUIÇÃO DO SB--------
----------------CODIFICAÇÃO DO GT---------------
        a partir de: 19/06/2013 14:00:06
0=Q, 1=W, 2=E, 3=R, 4=T, 5=Y, 6=U, 7=I, 8=O, 9=P
------------------------------------------------
MD-5:5AA6AEFF751859376631060EF2A86125
DJPDV 1.4.3e
------------------------------------------------
  DUn6OqqRcrrxj5+x67ool9l4quiUhb6x3vWwwdOLXCU=
MARCA: BEMATECH MOD: MP-4200 TH FI ECF-IF  VERSÃO
: 01.00.00
ECF: 001 LJ:      OPR: SUPER USUÃRIO (ALTE
QQQQQQQQQWWQWERYYP          19/08/2016 19:03:08V
FAB: BE101310100700000348                    BR
---------------------------------
  }

  Totalizadores := TStringList.Create;
  SL := TStringList.Create;
  try
    LeituraMemoriaFiscalSerial(ANumCRZ, ANumCRZ, SL);

    //DEBUG
    //SL.SaveToFile('C:\TEMP\LeituraMemoriaFiscal'+IntToStrZero(ANumCRZ,4)+'.txt');

    with fpDadosReducaoZClass do
    begin
      I := 0;
      // Achando a linha de inicio //
      if not EncontrarInicioLinha('CRZ') then
        Exit;

      if not EncontrarInicioLinha(IntToStrZero(ANumCRZ, 4)) then
        Exit;

      CRZ  := StrToIntDef( copy(SL[I],1 ,4), -1 ) ;
      CRO  := StrToIntDef( copy(SL[I],12,3), -1 ) ;
      COO  := StrToIntDef( copy(SL[I],17,9), -1 ) ;
      AStr := copy(SL[I],28,19);
      try
        DataDoMovimento := EncodeDate(StrToInt(copy(AStr, 7,4)),   // Ano
                                      StrToInt(copy(AStr, 4,2)),   // Mes
                                      StrToInt(copy(AStr, 1,2)));  // Dia
      except
        DataDoMovimento := 0;
      end;

      Inc(I, 2);
      CFC              := StrToIntDef( copy(SL[I],1, 6), -1 ) ;
      ValorVendaBruta  := ValorStrToFloat( copy(SL[I],8,Length(SL[I])) ) ;

      Inc(I);
      ExtrairTotalizadores;

      ValReg := AcharValorTotalizador( 'DT' );
      if ValReg >= 0 then
        DescontoICMS := ValReg;

      ValReg := AcharValorTotalizador( 'DS' );
      if ValReg >= 0 then
        DescontoISSQN := ValReg;

      ValReg := AcharValorTotalizador( 'AT' );
      if ValReg >= 0 then
        AcrescimoICMS := ValReg;

      ValReg := AcharValorTotalizador( 'AS' );
      if ValReg >= 0 then
        AcrescimoISSQN := ValReg;

      ValReg := AcharValorTotalizador( 'CT' );
      if ValReg >= 0 then
        CancelamentoICMS := ValReg;

      ValReg := AcharValorTotalizador( 'CS' );
      if ValReg >= 0 then
        CancelamentoISSQN := ValReg;

      {Aliquotas}
      {Percorrendo as aliquotas cadastradas no ECF para procurar por todas}
      for J := 0 to fpAliquotas.Count - 1 do
      begin
        AliqZ := TACBrECFAliquota.Create ;
        AliqZ.Assign( fpAliquotas[J] );

        NomeTotalizador := fpAliquotas[J].Tipo+FormatFloat('00.00',fpAliquotas[J].Aliquota)+'%';
        ValReg := AcharValorTotalizador( NomeTotalizador ) ;
        if ValReg >= 0 then
          AliqZ.Total := ValReg;

        AdicionaAliquota( AliqZ );
      end ;

      ValReg := AcharValorTotalizador('F1');
      if ValReg >= 0 then
        SubstituicaoTributariaICMS := ValReg;

      ValReg := AcharValorTotalizador('F2');
      if ValReg >= 0 then
        SubstituicaoTributariaICMS := max(SubstituicaoTributariaICMS,0) + ValReg;

      ValReg := AcharValorTotalizador('F3');
      if ValReg >= 0 then
        SubstituicaoTributariaICMS := max(SubstituicaoTributariaICMS,0) + ValReg;


      ValReg := AcharValorTotalizador('N1');
      if ValReg >= 0 then
        NaoTributadoICMS := ValReg;

      ValReg := AcharValorTotalizador('N2');
      if ValReg >= 0 then
        NaoTributadoICMS := max(NaoTributadoICMS,0) + ValReg;

      ValReg := AcharValorTotalizador('N3');
      if ValReg >= 0 then
        NaoTributadoICMS := max(NaoTributadoICMS,0) + ValReg;


      ValReg := AcharValorTotalizador('I1');
      if ValReg >= 0 then
        IsentoICMS := ValReg;

      ValReg := AcharValorTotalizador('I2');
      if ValReg >= 0 then
        IsentoICMS := max(IsentoICMS,0) + ValReg;

      ValReg := AcharValorTotalizador('I3');
      if ValReg >= 0 then
        IsentoICMS := max(IsentoICMS,0) + ValReg;


      ValReg := AcharValorTotalizador('FS1');
      if ValReg >= 0 then
        SubstituicaoTributariaISSQN := ValReg;

      ValReg := AcharValorTotalizador('FS2');
      if ValReg >= 0 then
        SubstituicaoTributariaISSQN := max(SubstituicaoTributariaISSQN,0) + ValReg;

      ValReg := AcharValorTotalizador('FS3');
      if ValReg >= 0 then
        SubstituicaoTributariaISSQN := max(SubstituicaoTributariaISSQN,0) + ValReg;


      ValReg := AcharValorTotalizador('NS1');
      if ValReg >= 0 then
        NaoTributadoISSQN := ValReg;

      ValReg := AcharValorTotalizador('NS2');
      if ValReg >= 0 then
        NaoTributadoISSQN := max(NaoTributadoISSQN,0) + ValReg;

      ValReg := AcharValorTotalizador('NS3');
      if ValReg >= 0 then
        NaoTributadoISSQN := max(NaoTributadoISSQN,0) + ValReg;


      ValReg := AcharValorTotalizador('IS1');
      if ValReg >= 0 then
        IsentoISSQN := ValReg;

      ValReg := AcharValorTotalizador('IS2');
      if ValReg >= 0 then
        IsentoISSQN := max(IsentoISSQN,0) + ValReg;

      ValReg := AcharValorTotalizador('IS3');
      if ValReg >= 0 then
        IsentoISSQN := max(IsentoISSQN,0) + ValReg;
    end;
  finally
    SL.Free;
    Totalizadores.Free;
  end
end;

function TACBrECFEscECF.GetVendaBruta: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|2' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end;

procedure TACBrECFEscECF.FechaNaoFiscal(Observacao: AnsiString;
   IndiceBMP: Integer);
var
  Obs: AnsiString;
begin
  Obs := Observacao ;
  AjustaLinhasColunas(Obs, 8);

  if not Consumidor.Enviado then
     EnviaConsumidor;

  with EscECFComando do
  begin
     CMD := 18 ;
     AddParamInteger( 1 );  // Aciona a Guilhotina
     AddParamString( Obs );
  end ;
  EnviaComando ;

  if (EscECFResposta.Params.Count > 2) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  end;

  fsEmPagamento := false ;
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CancelaItemNaoFiscal(const AItem: Integer);
begin
   CancelaItemVendido( AItem );
end;

procedure TACBrECFEscECF.Sangria(const Valor: Double; Obs: AnsiString;
   DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer);
begin
  AjustaLinhasColunas(Obs, 8);

  EscECFComando.CMD := 23;
  EscECFComando.AddParamInteger( 2 ) ;  // Sangria
  EscECFComando.AddParamDouble( Valor ) ;
  EscECFComando.AddParamString( Obs ) ;
  EnviaComando;

  RespostasComando.Clear;
  if (EscECFResposta.Params.Count > 3) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
    RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  end;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.Suprimento(const Valor: Double; Obs: AnsiString;
   DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer);
begin
  AjustaLinhasColunas(Obs, 8);

  EscECFComando.CMD := 23;
  EscECFComando.AddParamInteger( 1 ) ;  // Fundo de Troco
  EscECFComando.AddParamDouble( Valor ) ;
  EscECFComando.AddParamString( Obs ) ;
  EnviaComando;

  RespostasComando.Clear;
  if (EscECFResposta.Params.Count > 3) then
  begin
    RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
    RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
    RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
    RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  end;

  SalvaRespostasMemoria(False);
end;

function TACBrECFEscECF.EstornaCCD(const Todos: Boolean): Integer;
var
   UltimoCOO: Integer;
begin
  Result := 0 ;
  UltimoCOO := StrToInt( TACBrECF( fpOwner ).NumCOO );
  EstornaCCD( UltimoCOO, False );
  Consumidor.Enviado := True ;
  fsEmPagamento := false ;
end;

function TACBrECFEscECF.EstornaCCD(NumCupomFiscal: Integer; ADiante: Boolean
  ): Integer;
var
   ProximoCOO: Integer;
begin
  Result := 0 ;
  ProximoCOO := NumCupomFiscal;

  try
    while True do
    begin
      with EscECFComando do
      begin
        CMD := 13;
        AddParamInteger( ProximoCOO ) ;
        AddParamString(LeftStr(OnlyNumber(Consumidor.Documento),14)) ;
        AddParamString(LeftStr(Consumidor.Nome,30)) ;
        AddParamString(LeftStr(Consumidor.Endereco,79)) ;
      end;

      EnviaComando;
      RespostasComando.Clear;

      if (EscECFResposta.Params.Count > 5) then
      begin
        RespostasComando.AddField( 'COO',            EscECFResposta.Params[0] );
        RespostasComando.AddField( 'DataHora',       EscECFResposta.Params[1] );
        RespostasComando.AddField( 'VendaBruta',     EscECFResposta.Params[2] );
        RespostasComando.AddField( 'NumSerie',       EscECFResposta.Params[3] );
        RespostasComando.AddField( 'SeqPagto',       EscECFResposta.Params[4] );
        RespostasComando.AddField( 'NumParcela',     EscECFResposta.Params[5] );
      end;

      FechaRelatorio;

      Inc( Result );

      if ADiante then
        Inc( ProximoCOO )
      else
        Dec( ProximoCOO );
    end;
  except
    if Result = 0 then  // Não cancelou nada ?
      raise;
  end ;
end;

function TACBrECFEscECF.GetTotalAcrescimos: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|8' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end;

function TACBrECFEscECF.GetTotalCancelamentos: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|3' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end;

function TACBrECFEscECF.GetTotalDescontos: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|4' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end;

function TACBrECFEscECF.GetTotalTroco: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '7|21' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end;

function TACBrECFEscECF.GetTotalIsencao: Double;
begin
  LerTotaisTotalizadoresNaoTributados;
  Result := SomaTotalizadorNaoTributadoIndice('I');
end;

function TACBrECFEscECF.GetTotalAcrescimosISSQN : Double ;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|9' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end ;

function TACBrECFEscECF.GetTotalCancelamentosISSQN : Double ;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|5' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end ;

function TACBrECFEscECF.GetTotalDescontosISSQN : Double ;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|6' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToFloatDef( StrValue, 0 ) / 100;
end ;

function TACBrECFEscECF.GetTotalIsencaoISSQN : Double ;
begin
  LerTotaisTotalizadoresNaoTributados;
  Result := SomaTotalizadorNaoTributadoIndice('IS');
end ;

function TACBrECFEscECF.GetTotalNaoTributadoISSQN : Double ;
begin
  LerTotaisTotalizadoresNaoTributados;
  Result := SomaTotalizadorNaoTributadoIndice('NS');
end ;

function TACBrECFEscECF.GetTotalSubstituicaoTributariaISSQN : Double ;
begin
  LerTotaisTotalizadoresNaoTributados;
  Result := SomaTotalizadorNaoTributadoIndice('FS');
end ;

function TACBrECFEscECF.GetTotalNaoTributado: Double;
begin
  LerTotaisTotalizadoresNaoTributados;
  Result := SomaTotalizadorNaoTributadoIndice('N');
end;

function TACBrECFEscECF.GetTotalSubstituicaoTributaria: Double;
begin
  LerTotaisTotalizadoresNaoTributados;
  Result := SomaTotalizadorNaoTributadoIndice('F');
end;

function TACBrECFEscECF.GetCNPJ: String;
begin
  Result := RetornaInfoECF( '15|8' ) ;
end;

function TACBrECFEscECF.GetIE: String;
begin
  Result := RetornaInfoECF( '15|9' ) ;
end;

function TACBrECFEscECF.GetIM: String;
begin
  Result := RetornaInfoECF( '15|10' ) ;
end;

function TACBrECFEscECF.GetCliche: AnsiString;
begin
  Result := RetornaInfoECF( '15|15' ) + sLineBreak +  // Razao Social
            RetornaInfoECF( '15|16' ) + sLineBreak +  // Nome Fantasia
            RetornaInfoECF( '15|17' ) ;               // Endereço
end;

function TACBrECFEscECF.GetUsuarioAtual : String ;
begin
  Result := '1' ; // No convênio 09/09 não é mais possível cadastrar outros usuários
end ;

function TACBrECFEscECF.GetDataHoraSB : TDateTime ;
begin
  if fsDataHoraSB = 0 then
  begin
    fsDataHoraSB := inherited GetDataHoraSB;
  end;

  Result := fsDataHoraSB ;
end ;

function TACBrECFEscECF.GetSubModeloECF: String;
begin
   Result := fsModeloECF;
end;

function TACBrECFEscECF.GetPAF: String;
begin
  Result := fsPAF ;
end;

function TACBrECFEscECF.GetDataMovimento: TDateTime;
var
   DataStr: String;
begin
  RetornaInfoECF( '8' ) ;
  DataStr := EscECFResposta.Params[0];

  try
     Result := EncodeDate( StrToInt(copy(DataStr,5,4)),   // Ano
                           StrToInt(copy(DataStr,3,2)),   // Mes
                           StrToInt(copy(DataStr,1,2)) ); // Dia
  except
     Result := 0;
  end;
end;

procedure TACBrECFEscECF.LerTotaisAliquota;
begin
  CarregaAliquotas;
end;

function TACBrECFEscECF.AchaICMSAliquota(var AliquotaICMS: String
   ): TACBrECFAliquota;
begin
  { EscECF usa a letra T/S no Indice, e ACBrECFClass.AchaICMSAliquota(), que é
   chamada logo abaixo, irá remove-lo, portanto vamos adicionar um T/S extra }
  if CharInSet(upcase(AliquotaICMS[1]) , ['T','S']) then
    AliquotaICMS := AliquotaICMS[1]+AliquotaICMS[1]+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice T01, T1, T02}

  Result := inherited AchaICMSAliquota( AliquotaICMS );
end;


{ Observaçoes:

- Registro E01 do comando 139 traz a Data de Sw.Basico no formato DDMMAAAA quando o correto é AAAAMMDD

- Não encontrado:
-- Num.Loja
-- Total Acrescimos OPNF
-- Total Cancelamentos OPNF
-- Total Descontos OPNF
}

end.



