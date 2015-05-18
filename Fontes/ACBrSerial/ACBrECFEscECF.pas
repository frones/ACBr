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
    cEscECFMaxBuffer = 4096 ;
    SYN = #22;

type

TACBrECFClassHack = class ( TACBrECFClass );

{ TACBrECFEscECFRET }

TACBrECFEscECFRET = class
private
   fsECF: Byte;
   fsFabricante: Byte;
   fsFisco: Byte;
   fsRET: AnsiString;
   fsSPR: Byte;
   procedure SetRET(AValue: AnsiString);
 public
    constructor Create;
    property ECF        : Byte read fsECF ;
    property Fisco      : Byte read fsFisco ;
    property SPR        : Byte read fsSPR ;
    property Fabricante : Byte read fsFabricante ;

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

    Procedure AddParamString(AString : AnsiString) ;
    Procedure AddParamInteger(AInteger : Integer) ;
    Procedure AddParamDouble(ADouble : Double; Decimais: Byte = 2) ;
    Procedure AddParamDateTime( ADateTime: TDateTime; Tipo : Char = 'D';
                                FlagHV : String = '' ) ;
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

    procedure SetResposta(const Value: AnsiString);
 public
    constructor Create ;
    destructor Destroy ; override ;

    procedure Clear( ClearParams: Boolean = True ) ;

    property Resposta : AnsiString  read fsResposta write SetResposta ;
    property Params   : TStringList read fsParams ;
    property SEQ      : Byte        read fsSEQ ;
    property CMD      : Byte        read fsCMD ;
    property EXT      : Byte        read fsEXT ;
    property CAT      : Byte        read fsCAT ;
    property RET      : TACBrECFEscECFRET read fsRET ;
    property TBR      : Integer     read fsTBR ;
    property BRS      : AnsiString  read fsBRS ;
    property CHK      : Byte        read fsCHK ;
 end ;


 { Classe filha de TACBrECFClass com implementaçao para EscECF }
TACBrECFEscECF = class( TACBrECFClass )
 private
    fsFalhas : Byte;
    fsACK    : Boolean;
    fsSincronizou : Boolean;
    fsTentouSincronizar : Boolean;
    fsDeviceParams : String;

    fsSPR            : Byte;
    fsPAF            : AnsiString ;
    fsNumVersao      : String ;
    fsVersaoEscECF   : String ;
    fsNumECF         : String ;
    fsNumCRO         : String ;
    fsNumLoja        : String ;
    fsDataHoraSB     : TDateTime;
    fsEscECFComando  : TACBrECFEscECFComando;
    fsEscECFResposta : TACBrECFEscECFResposta;
    fsMarcaECF       : String ;
    fsModeloECF      : String ;
    fsEmPagamento    : Boolean ;
    fsNomeArqMemoria : String ;
    fsArqMemoria     : String ;

    function IsBematech: Boolean;
    function IsEpson: Boolean;
    function IsDaruma: Boolean;

    procedure EnviaConsumidor;
    function PreparaCmd(CmdExtBcd: AnsiString): AnsiString;
    Function TraduzErroMsg(EscECFResposta: TACBrECFEscECFResposta) : String;
    procedure Sincronizar;

    Function AjustaDescricao( ADescricao: String ): String;

    Procedure SalvaRespostasMemoria( AtualizaVB: Boolean = True );
    Procedure LeRespostasMemoria;

    function CriarECFClassPorMarca : TACBrECFClass;
    procedure DestruirECFClass( AECFClass: TACBrECFClass );

 protected
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

    function GetDadosUltimaReducaoZ: AnsiString; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;

    function GetParamDescontoISSQN: Boolean; override;

    { TODO (não encontrado): function GetTipoUltimoDocumento : TACBrECFTipoDocumento ; override ; }
 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    property NomeArqMemoria : String read fsNomeArqMemoria write fsNomeArqMemoria;

    property EscECFComando  : TACBrECFEscECFComando  read fsEscECFComando ;
    property EscECFResposta : TACBrECFEscECFResposta read fsEscECFResposta ;

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
    Procedure CancelaCupom ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;
    procedure CancelaItemVendidoParcial( NumItem : Integer; Quantidade : Double) ; override ;
    procedure CancelaDescontoAcrescimoItem( NumItem : Integer) ; override ;

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

    Function EstornaCCD( const Todos: Boolean = True) : Integer; override ;

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
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ; override ;
    Procedure EspelhoMFD_DLL( COOInicial, COOFinal : Integer;
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ; override ;
    Procedure ArquivoMFD_DLL( DataInicial, DataFinal : TDateTime;
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD  ) ; overload ; override ;
    Procedure ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
       NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD;
       TipoContador: TACBrECFTipoContador = tpcCOO ) ; overload ; override ;

    Procedure ArquivoMF_DLL(  NomeArquivo : AnsiString  ) ; overload ; override ;
    Procedure ArquivoMFD_DLL(NomeArquivo: AnsiString); overload ; override ;

    Procedure IdentificaOperador(Nome : String); override;
    Procedure IdentificaPAF( NomeVersao, MD5 : String) ; override ;

    function TraduzirTag(const ATag: AnsiString): AnsiString; override;
    function TraduzirTagBloco(const ATag, Conteudo: AnsiString): AnsiString; override;

    Function RetornaInfoECF( Registrador: String) : AnsiString; override ;

    Function CapturaXMLCupom( Inicial, Final: String; Tipo: Integer = 2 ): AnsiString;
 end ;

implementation
Uses SysUtils, Math,
    {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
    ACBrECF, ACBrECFBematech, ACBrECFEpson, ACBrConsts, ACBrUtil,
  ACBrECFDaruma;

{ TACBrECFEscECFRET }

constructor TACBrECFEscECFRET.Create;
begin
  inherited create ;

  Clear;
end;

procedure TACBrECFEscECFRET.SetRET(AValue: AnsiString);
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

  Buffer := AnsiChar(chr(fsSEQ)) + AnsiChar(chr(fsCMD)) + AnsiChar(chr(fsEXT)) +
            IntToLEStr(TBC) + BCD ;

  Soma := 0 ;
  LenCmd := Length( Buffer ) ;
  For I := 1 to LenCmd do
     Soma := Soma + ord( Buffer[I] ) ;
  CHK := Soma mod 256  ;

  Result := SOH + Buffer + AnsiChar(Chr( CHK )) ;
end;

procedure TACBrECFEscECFComando.AddParamString(AString: AnsiString);
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
  AddParamInteger( Round( ADouble * power(10, Decimais) ) ) ;
end;

procedure TACBrECFEscECFComando.AddParamInteger(AInteger: Integer);
begin
  AddParamString( IntToStr( AInteger ) ) ;
end;

procedure TACBrECFEscECFComando.AddParamDateTime(ADateTime: TDateTime;
   Tipo : Char = 'D'; FlagHV : String = ''  ) ;
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

procedure TACBrECFEscECFResposta.SetResposta(const Value: AnsiString);
Var
  Soma, I, F, LenCmd : Integer ;
  CHK  : Byte ;
begin
  Clear( False ) ;    // Não Zera Params, pois pode acumular 2 retornos

  if Value = '' then exit ;

  LenCmd := Length( Value ) ;

  if (LenCmd = 6) then  // Retorno de NAK ou WAK
  begin
    fsResposta := Value ;
    fsCAT      := ord( Value[2] ) ;
    fsRET.RET  := Copy( Value, 3, 4 );
    exit ;
  end;

  if LenCmd < 12 then
     raise EACBrECFSemResposta.Create('Tamanho de Resposta muito curto: '+
                                      IntToStr(LenCmd)+' bytes');

  fsResposta := Value ;
  fsSEQ      := ord( Value[2] ) ;
  fsCMD      := ord( Value[3] ) ;
  fsEXT      := ord( Value[4] ) ;
  fsCAT      := ord( Value[5] ) ;
  fsRET.RET  := Copy( Value, 6, 4 );
  fsTBR      := LEStrToInt( copy(Value,10,2) );
  fsBRS      := copy( Value, 12, fsTBR ) ;
  fsCHK      := ord( Value[ 12 + fsTBR ] ) ;

  Soma := 0 ;
  LenCmd := LenCmd-1 ;  { -1 por causa do CHK }
  For I := 2 to LenCmd do  
     Soma := Soma + ord( Value[I] ) ;
  CHK := Soma mod 256  ;

  if CHK <> fsCHK then
     raise EACBrECFSemResposta.Create(ACBrStr('Erro CHK Resposta. '+
        'Calculado:'+IntToStr(CHK)+' Recebido:'+IntToStr(fsCHK)));

  { Quebrando Parametros Separados por '|' e inserindo-os em fsParams }
  I := 1;
  while I < fsTBR do
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

  fsEscECFComando  := TACBrECFEscECFComando.create ;
  fsEscECFResposta := TACBrECFEscECFResposta.create ;

  fpDevice.HandShake := hsDTR_DSR ;
  fpPaginaDeCodigo   := 1252;
  fsArqMemoria       := '';

  fpModeloStr := 'EscECF' ;
  fpColunas   := 57 ;
  fpMFD       := True ;
  fpTermica   := True ;
  fpIdentificaConsumidorRodape := True ;
  fsDeviceParams := '';

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

  fsFalhas        := 0;
  fsACK           := False;

  fsSincronizou       := False;
  fsTentouSincronizar := False;

  RespostasComando.Clear;
end;

destructor TACBrECFEscECF.Destroy;
begin
  fsEscECFComando.Free ;
  fsEscECFResposta.Free ;

  inherited Destroy ;
end;

procedure TACBrECFEscECF.Ativar;
var
   Params: String;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+ModeloStr+' requer'+sLineBreak+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

  inherited Ativar ; { Abre porta serial }

  if not EmLinha( TimeOut ) then
  begin
     if fpDevice.HandShake <> hsDTR_DSR then
        fpDevice.HandShake := hsDTR_DSR
  end;

  fsNumVersao    := '' ;
  fsVersaoEscECF := '' ;
  fsPAF          := '' ;
  fsNumECF       := '' ;
  fsNumCRO       := '' ;
  fsNumLoja      := '' ;
  fsDataHoraSB   := 0 ;
  fsMarcaECF     := '' ;
  fsModeloECF    := '' ;
  fsSincronizou       := False;
  fsTentouSincronizar := False;

  fpMFD     := True ;
  fpTermica := True ;
  fsDeviceParams := '';

  RespostasComando.Clear;

  try
     { Ajusta a sequencia }
     Sincronizar;

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
        fpColunas := 48;

        if MaxLinhasBuffer = 0 then  // Bematech congela se receber um Buffer muito grande
           MaxLinhasBuffer := 5;
      end
     else if IsEpson then
      begin
        fpPaginaDeCodigo := 850;
      end;





     LeRespostasMemoria;
  except
     Desativar ;
     raise ;
  end ;
end;


function TACBrECFEscECF.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
Var
  ErroMsg : String ;
  OldTimeOut : Integer ;
begin
  Sincronizar;

  if cmd <> '' then
     cmd := PreparaCmd(cmd) ;  // Ajusta e move para EscECFcomando

  EscECFResposta.Clear( True ) ;       // Zera toda a Resposta
  cmd := EscECFComando.Comando ;

  if fsTentouSincronizar then
    GravaLog( '         Status TX -> '+cmd, True);

  fsACK             := False;
  fsFalhas          := 0 ;
  fsSPR             := 0 ;
  Result            := '' ;
  ErroMsg           := '' ;
  fpComandoEnviado  := '' ;
  fpRespostaComando := '' ;
  OldTimeOut        := TimeOut ;
  TimeOut           := max(EscECFComando.TimeOut, TimeOut) ;

  try
     fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }
     fpDevice.Serial.Purge ;                   { Limpa a Porta }

     while fpComandoEnviado = '' do
     begin
        fpDevice.Serial.Purge ;                   { Limpa a Porta }

        if not TransmiteComando( cmd ) then
           continue ;

        fpComandoEnviado := cmd ;
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
        if (not fsTentouSincronizar) and
           (EscECFResposta.CAT = 15) and (EscECFResposta.RET.ECF = 1) then    // Erro de sincronização
        begin
          GravaLog( '    Falha SYN - RX <- '+EscECFResposta.Resposta, True);
          fsSincronizou       := False;  // Força a sincronização
          fsTentouSincronizar := True;   // Evita loop infinito, no caso de ocorrer o mesmo erro
          EnviaComando_ECF();            // Gera chamada recursiva
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
     fsTentouSincronizar := False;
  end ;
end;

function TACBrECFEscECF.VerificaFimLeitura(var Retorno : AnsiString ;
  var TempoLimite : TDateTime) : Boolean ;
var
  LenRet, TBR : Integer;
  Byte1  : AnsiChar;

   procedure PedeStatus;
   begin
      GravaLog( '         Status TX -> '+ENQ+chr(fsSPR), True);
      fpDevice.Serial.SendBlock( ENQ + chr(fsSPR) ); // ACK ok, Pede Resposta
      Retorno := '';
      TempoLimite := IncSecond(now, TimeOut);
   end;

begin
  LenRet := Length( Retorno );
  Result := False;

  if LenRet < 1 then exit;

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
          GravaLog('        Reenvio TX -> '+fpComandoEnviado, True);
          fpDevice.EnviaString( fpComandoEnviado ) ;
          Retorno     := '';
          Result      := False;
          TempoLimite := IncSecond(now, TimeOut);
        end ;

      end ;

    NAK :  Result := (LenRet >= 6) ;
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
           if fsFalhas < 6 then
             PedeStatus;
         end
        else
           raise ;
     end ;
  end ;

  if Result then
  begin
     if (Byte1 = WAK) then // Ocupado, aguarde e solicite novo Status
      begin
        GravaLog('                RX <- '+Retorno, True);
        Sleep( 50 );
        PedeStatus;
        Result := False;
      end
     else if (Byte1 = SOH) and (EscECFResposta.CAT = 0) then
      begin
        if not TestBit( EscECFResposta.RET.ECF, 0 ) then
        begin
          GravaLog('     '+IntToStrZero(EscECFResposta.TBR,4)+' bytes RX <- '+Retorno, True);
          Inc( fsSPR );
          PedeStatus;
          Result := False;
        end;
      end;
  end;
end;

function TACBrECFEscECF.GetModeloStr: String;
begin
  Result := fsMarcaECF ;
  if fsModeloECF <> '' then
     Result := Result + ' - ' + fsModeloECF;
  if Result = '' then
     Result := fpModeloStr;
end;


function TACBrECFEscECF.PreparaCmd(CmdExtBcd: AnsiString): AnsiString;
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

  EscECFComando.CMD := CMD ;
  EscECFComando.EXT := EXT ;
  EscECFComando.Params.Text := BCD ;

  Result := EscECFComando.Comando ;
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

procedure TACBrECFEscECF.Sincronizar;
var
  Resp: AnsiString;
begin
  if fsSincronizou then exit;

  GravaLog( '    Sincronismo TX -> '+SYN, True);
  fpDevice.Serial.SendByte( ord(SYN) );
  Resp := fpDevice.Serial.RecvBufferStr(2,2000);
  GravaLog( '         Status RX <- '+Resp, True);

  if Length(Resp) = 2 then
  begin
    if Resp[1] = SYN then
    begin
      EscECFComando.SEQ := ord(Resp[2])+1;
      fsSincronizou := True;
    end;
  end;
end;

function TACBrECFEscECF.AjustaDescricao(ADescricao : String) : String ;
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
  try
     ValVB := RespostasComando.FieldByName('VendaBruta').AsFloat;
  except
     AtualizaVB := True;
  end ;

  if AtualizaVB then
  begin
    ValVB := GetVendaBruta;
    RespostasComando.AddField( 'VendaBruta', FloatToIntStr(ValVB) );
    RespostasComando.AddField( 'EmPagamento', ifthen( fsEmPagamento,'1','0') );
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

     fsEmPagamento := (RespostasComando.FieldByName( 'EmPagamento' ).AsInteger = 1);
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
  else if IsEpson then
  begin
    Result := TACBrECFEpson.create(fpOwner);
    with TACBrECFClassHack( Result ) do
    begin
      fpDevice.Desativar;
      fsDeviceParams := fpDevice.Porta+':'+fpDevice.ParamsString;
      fpDevice.Porta := 'USB';         // Força DLL em USB
      fpDevice.Baud  := 115200;
    end;
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
     fpDevice.Porta        := copy(fsDeviceParams,1,P-1);
     fpDevice.ParamsString := copy(fsDeviceParams,P+1,Length(fsDeviceParams));
     fsDeviceParams := '';
  end;

  Self.Ativar;
end;

function TACBrECFEscECF.RetornaInfoECF(Registrador: String): AnsiString;
begin
  if Pos('|',Registrador) = 0 then
     Registrador := Registrador + '|' ;

  EscECFComando.CMD := 26;
  EscECFComando.AddParamString(Registrador);
  EnviaComando;

  Result := EscECFResposta.BRS;
  while (RightStr(Result,1) = '|') do
     Delete( Result, Length(Result), 1 );
end;

function TACBrECFEscECF.CapturaXMLCupom(Inicial, Final : String ; Tipo : Integer
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
begin
  RetornaInfoECF( '1|1' ) ;
  Result := EscECFResposta.Params[1] ;
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
    end ;
  end ;

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
  Result := fpEstado;
  try
     if (not fpAtivo) then
        fpEstado := estNaoInicializada
     else
      begin
        fpEstado := estDesconhecido ;

        FlagEst := StrToInt( RetornaInfoECF( '16|5' ) );
        Case FlagEst of
          0  :             fpEstado := estLivre;
          10 :             fpEstado := estVenda;
          11..13, 21..23 : fpEstado := estPagamento;
          20 :             fpEstado := estNaoFiscal;
          30..32 :         fpEstado := estRelatorio;
        end;

        if (fpEstado in [estLivre,estDesconhecido]) then
        begin
          FlagEst := StrToInt( RetornaInfoECF( '16|4' ) );
          if FlagEst = 3 then
             fpEstado := estBloqueada
          else if fsEmPagamento then
             fpEstado := estPagamento ;
        end;

        if fpEstado in [estLivre, estBloqueada] then
        begin
           RetornaInfoECF( '8' ) ;
           FlagEst := StrToInt( EscECFResposta.Params[1] );

           if FlagEst = 2 then
              fpEstado := estRequerZ
           // Workaround para Epson que não responde Flag de Status de Movimento corretamente
           else if (fpEstado = estBloqueada) and (FlagEst = 0) and IsEpson then
           begin
              RetornaInfoECF( '99|21' ) ;
              if (EscECFResposta.Params.Count > 11) and (EscECFResposta.Params[11] = 'S') then
                fpEstado := estRequerZ;
           end;
        end;
      end ;
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
     DataHora := now ;

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

  try
     EnviaComando ;
     RespostasComando.Clear;
     SalvaRespostasMemoria(True);
  except
     on E : Exception do
     begin
        // Woraround para Epson, para Erro de data na Redução Z
        if IsEpson and (EscECFResposta.CAT = 16) and (EscECFResposta.RET.ECF = 3) then
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
  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.LinhaRelatorioGerencial(Linha: AnsiString;
   IndiceBMP: Integer);
var
  P, Espera: Integer;
  Buffer   : AnsiString ;
begin
  Linha := AjustaLinhas( Linha, Colunas, 0, (IsEpson or IsBematech) );  { Formata as Linhas de acordo com "Coluna" }

  while Length( Linha ) > 0 do
  begin
     P := Length( Linha ) ;
     if P > cEscECFMaxBuffer then    { Acha o fim de Linha mais próximo do limite máximo }
        P := PosLast(LF, LeftStr(Linha,cEscECFMaxBuffer) ) ;

     if P = 0 then
        P := Colunas ;

     Buffer := copy( Linha, 1, P)  ;
     Espera := Trunc( CountStr( Buffer, LF ) / 4) ;

     EscECFComando.CMD := 9                                ;
     EscECFComando.TimeOut := Espera ;
     EscECFComando.AddParamString(Buffer);
     EnviaComando;

     { ficou apenas um LF sozinho ? }
     if (P = Colunas) and (RightStr( Buffer, 1) <> LF) and
        (copy( Linha, P+1, 1) = LF) then
        P := P + 1 ;

     Linha  := copy( Linha, P+1, Length(Linha) ) ;   // O Restante
  end ;
end;

procedure TACBrECFEscECF.AbreCupomVinculado(COO, CodFormaPagto,
   CodComprovanteNaoFiscal: String; Valor: Double);
Var
  Sequencia, NumPagtos, P : Integer ;
  APagto, CodPagto : String ;

  procedure EnviaComandoCCD;
  begin
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
  end;

begin
  // Achando a Sequencia do Pagamento de acordo com o Indice //
  Sequencia := 1;
  try
    NumPagtos := RespostasComando.FieldByName('NumPagtos').AsInteger;
    if NumPagtos > 1 then
    begin
      repeat
        APagto := RespostasComando.FieldByName('Pagto'+IntToStr(Sequencia)).AsString;
        P := pos('|',APagto);
        CodPagto := Copy(APagto,1,P-1);
        if CodPagto = CodFormaPagto then
        begin
          APagto := '*'+APagto;  // sinaliza que já usou;
          break;
        end ;

        Inc( Sequencia );
      until (Sequencia >= NumPagtos);
    end ;
  except
  end ;

  try
    EnviaComandoCCD;
  except
    On Exception do
    begin
      // Woraround para Epson, que em algumas situações não reconhece o Nu. de Sequencia corretamente
      if IsEpson and (EscECFResposta.CAT = 16) and (EscECFResposta.RET.ECF = 11) then
      begin
        Dec(Sequencia);
        EnviaComandoCCD;
      end
      else
        raise;
    end;
  end;

  //RespostasComando.Clear;
  RespostasComando.AddField( 'Pagto'+IntToStr(Sequencia), APagto );
  RespostasComando.AddField( 'COO',            EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',       EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta',     EscECFResposta.Params[2] );
  RespostasComando.AddField( 'NumSerie',       EscECFResposta.Params[3] );
  RespostasComando.AddField( 'SeqPagto',       EscECFResposta.Params[4] );
  RespostasComando.AddField( 'NumParcela',     EscECFResposta.Params[5] );
  RespostasComando.AddField( 'NumParcelaFalta',EscECFResposta.Params[6] );

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
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.EspelhoMFD_DLL(DataInicial, DataFinal, NomeArquivo, Documentos);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.EspelhoMFD_DLL(COOInicial, COOFinal: Integer;
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.EspelhoMFD_DLL(COOInicial, COOFinal, NomeArquivo, Documentos);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMFD_DLL(DataInicial, DataFinal: TDateTime;
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.ArquivoMFD_DLL(DataInicial, DataFinal, NomeArquivo, Documentos, Finalidade);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMFD_DLL(ContInicial, ContFinal: Integer;
  NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD; TipoContador: TACBrECFTipoContador);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.ArquivoMFD_DLL(ContInicial, ContFinal, NomeArquivo, Documentos, Finalidade, TipoContador);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMF_DLL(NomeArquivo: AnsiString);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.ArquivoMF_DLL(NomeArquivo);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.ArquivoMFD_DLL(NomeArquivo: AnsiString);
var
  ECFClass: TACBrECFClass;
begin
  ECFClass := CriarECFClassPorMarca;

  if ECFClass = nil then
    inherited;

  try
    Self.Desativar;
    ECFClass.ArquivoMFD_DLL(NomeArquivo);
  finally
    DestruirECFClass( ECFClass );
  end;
end;

procedure TACBrECFEscECF.IdentificaOperador(Nome: String);
begin
  EscECFComando.CMD := 154;
  EscECFComando.AddParamString(LeftStr(Nome,20)) ;
  EnviaComando;
end;

procedure TACBrECFEscECF.IdentificaPAF(NomeVersao, MD5: String);
begin
  // 48 e 36 para garantir que NomeVersao inicie na linha 2
  fsPAF := PadRight(MD5,48) + PadRight(NomeVersao,36) ;
  EscECFComando.CMD := 24;
  EscECFComando.AddParamString( fsPAF ) ;
  EnviaComando;
end;

function TACBrECFEscECF.TraduzirTag(const ATag : AnsiString) : AnsiString ;
begin
  // TODO: Usar Tradução de classe original  ??

  if IsBematech then
    Result := BematechTraduzirTag( ATag )
  else
    Result := inherited TraduzirTag( ATag );
end ;

function TACBrECFEscECF.TraduzirTagBloco(const ATag, Conteudo : AnsiString
   ) : AnsiString ;
begin
  if IsBematech then
    Result := BematechTraduzirTagBloco( ATag, Conteudo, Self)
  else
    Result := inherited TraduzirTagBloco(ATag, Conteudo) ;
end ;

procedure TACBrECFEscECF.AbreCupom ;
begin
  EscECFComando.CMD := 1;
  EscECFComando.AddParamString(LeftStr(OnlyNumber(Consumidor.Documento),14)) ;
  EscECFComando.AddParamString(LeftStr(Consumidor.Nome,30)) ;
  EscECFComando.AddParamString(LeftStr(Consumidor.Endereco,79)) ;
  EnviaComando;

  RespostasComando.Clear;
  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );

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

  RespostasComando.AddField( 'NumUltItem', EscECFResposta.Params[0] );
  RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[1] );
  RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[2] );
  fsEmPagamento := false ;

  SalvaRespostasMemoria(True);

  { Se o desconto é maior que zero dá o comando de desconto de item }

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

  RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[0] );
  RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CancelaCupom;
var
   UltimoCOO: Integer;
   Est: TACBrECFEstado;
begin
  RespostasComando.Clear;
  Est := TACBrECF( fpOwner ).Estado;

  case Est of
    estRelatorio : FechaRelatorio ;

    estVenda, estPagamento, estNaoFiscal :
      begin
        EscECFComando.CMD := 31;
        EnviaComando;
      end;
  else
    begin
      // Tenta cancelar todos os CCDs anteriores, gera exceção muda se não for CCD
      UltimoCOO := StrToInt( TACBrECF( fpOwner ).NumCOO );
      try
        repeat
           with EscECFComando do
           begin
             CMD := 13;
             AddParamInteger( UltimoCOO ) ;
             AddParamString(LeftStr(OnlyNumber(Consumidor.Documento),14)) ;
             AddParamString(LeftStr(Consumidor.Nome,30)) ;
             AddParamString(LeftStr(Consumidor.Endereco,79)) ;
           end ;
           EnviaComando;
           FechaRelatorio;

           Dec( UltimoCOO );
        until UltimoCOO < 1;
      except
      end ;

      EscECFComando.CMD := 7;
      EscECFComando.AddParamInteger( UltimoCOO );
      EnviaComando;
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

  RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[0] );
  RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.CancelaDescontoAcrescimoItem(NumItem: Integer);
begin
  with EscECFComando do
  begin
     CMD := 28 ;
     AddParamInteger( 0 );
     AddParamInteger( NumItem )
  end ;

  EnviaComando ;

  RespostasComando.AddField( 'TotalItem',  EscECFResposta.Params[0] );
  RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
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
  EnviaComando;

  RespostasComando.Clear;
  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );

  Consumidor.Enviado := True ;
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

  RespostasComando.AddField( 'NumUltItem', EscECFResposta.Params[0] );
  RespostasComando.AddField( 'SubTotal',   EscECFResposta.Params[1] );
  fsEmPagamento := false ;
  SalvaRespostasMemoria(True);
end;

procedure TACBrECFEscECF.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
Var
  NumPagtos : Integer;
  TotPag : Double;
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

  Inc( NumPagtos ) ;
  RespostasComando.AddField( 'NumPagtos', IntToStr(NumPagtos) );
  RespostasComando.AddField( 'Pagto'+IntToStr(NumPagtos),
     CodFormaPagto+'|'+FloatToStr(Valor)+'|'+IntToStr(CodMeioPagamento) );
  RespostasComando.AddField( 'TotalAPagar', EscECFResposta.Params[0] );
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

  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.FechaCupom(Observacao: AnsiString; IndiceBMP: Integer);
begin
  if not Consumidor.Enviado then
     EnviaConsumidor;

  with EscECFComando do
  begin
     CMD := 5 ;
     AddParamInteger( 0 );  // Sem Cupom Adicional
     AddParamInteger( 1 );  // Aciona a Guilhotina
     AddParamString( Observacao );
  end ;
  EnviaComando ;

  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  fsEmPagamento := false ;

  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString );
begin
  fsEmPagamento := True ;
  if DescontoAcrescimo = 0 then exit ;
  
  with EscECFComando do
  begin
     CMD := 29 ;
     AddParamInteger( ifthen(DescontoAcrescimo < 0,0,1) );
     AddParamInteger( 1 );
     AddParamDouble( abs(DescontoAcrescimo) );
  end ;
  EnviaComando ;

  RespostasComando.AddField( 'SubTotal', EscECFResposta.Params[0] );
  SalvaRespostasMemoria(False);
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
       Aliquota.Total     := StrToIntDef( EscECFResposta.Params[ 4*I + 3 ], 0 ) / 100 ;

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
  if Posicao = '' then
     PosAliq := Aliquotas.Count + 1
  else
     PosAliq := StrToInt( Posicao );
  EscECFComando.AddParamInteger( PosAliq ) ;
  EscECFComando.AddParamString( Tipo ) ;
  EscECFComando.AddParamString( IntToStrZero( Trunc(Aliquota*100), 4 ) ) ;
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
    FPG := AchaFPGIndice( IntToStr(StrToInt(EscECFResposta.Params[ 2*I ])) ) ;
    if Assigned( FPG ) then
       FPG.Total := StrToInt( EscECFResposta.Params[ 2*I + 1 ] ) / 100;
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
  if Posicao = '' then
     PosFPG := FormasPagamento.Count + 1
  else
     PosFPG := StrToInt( Posicao );
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
       RelGer.Contador := StrToInt( EscECFResposta.Params[ 2*I + 1 ] ) ;
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
  if Posicao = '' then
     PosRel := RelatoriosGerenciais.Count + 1
  else
     PosRel := StrToInt( Posicao );
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
       CNF.Contador := StrToInt( EscECFResposta.Params[ 3*I + 1 ] ) ;
       CNF.Total    := StrToInt( EscECFResposta.Params[ 3*I + 2 ] ) / 100 ;
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
  if Posicao = '' then
     PosCNF := ComprovantesNaoFiscais.Count + 1
  else
     PosCNF := StrToInt( Posicao );
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

  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
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
  Result   := StrToInt( StrValue ) / 100;
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
  try
    Result := RespostasComando.FieldByName('NumUltItem').AsInteger;
  except
    Result := 0;
  end ;
end;

function TACBrECFEscECF.GetDadosUltimaReducaoZ : AnsiString ;
var
  DataStr, ECFCRZ  : String ;
  I: Integer;
  AliqZ : TACBrECFAliquota ;

  function AchaValorRegistrador(Registrador: String; Aliq: Double = 0): Double ;
  var
    I: Integer;
  begin
    I := 0 ; Result := 0;
    while  (I+2 < EscECFResposta.Params.Count) do
    begin
      if (EscECFResposta.Params[I] = Registrador) and
         (StringToFloatDef(EscECFResposta.Params[I+1],0) = Aliq) then
      begin
         Result := RoundTo( StrToFloatDef(EscECFResposta.Params[ I+2 ],0)/100, -2);
         Break;
      end ;

      Inc( I ) ;
    end ;
  end ;
begin
  // Zerar variaveis e inicializa Dados do ECF //
  InitDadosUltimaReducaoZ;

  if not Assigned( fpAliquotas ) then
    CarregaAliquotas ;

  with TACBrECF(fpOwner) do
  begin
    ECFCRZ := Trim(NumCRZ);
  end;

  RetornaInfoECF( '17|'+ECFCRZ ) ;

  // DEBUG
  //WriteToTXT('C:\TEMP\REDZ.TXT', EscECFResposta.Params.Text);
  if (UpperCase(copy(EscECFResposta.Params.Text, 0, 5)) = 'ERRO:')  then
  begin
    raise EACBrECFERRO.Create(ACBrStr(EscECFResposta.Params.Text)) ;
  end;

  { Alimenta a class com os dados atuais do ECF }
  with fpDadosReducaoZClass do
  begin
    CRZ              := StrToIntDef( EscECFResposta.Params[0], 0) ;
    DataStr          := EscECFResposta.Params[1];
    DataDoMovimento  := EncodeDate( StrToInt(copy(DataStr,5,4)),   // Ano
                                    StrToInt(copy(DataStr,3,2)),   // Mes
                                    StrToInt(copy(DataStr,1,2)) ); // Dia
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
      AliqZ.Total := AchaValorRegistrador( AliqZ.Tipo, AliqZ.Aliquota ) ;

      AdicionaAliquota( AliqZ );
    end ;

    SubstituicaoTributariaICMS := AchaValorRegistrador('F1') +
                                  AchaValorRegistrador('F2') +
                                  AchaValorRegistrador('F3') ;

    NaoTributadoICMS           := AchaValorRegistrador('N1') +
                                  AchaValorRegistrador('N2') +
                                  AchaValorRegistrador('N3') ;

    IsentoICMS                 := AchaValorRegistrador('I1') +
                                  AchaValorRegistrador('I2') +
                                  AchaValorRegistrador('I3') ;

    SubstituicaoTributariaISSQN:= AchaValorRegistrador('FS1') +
                                  AchaValorRegistrador('FS2') +
                                  AchaValorRegistrador('FS3') ;

    NaoTributadoISSQN          := AchaValorRegistrador('NS1') +
                                  AchaValorRegistrador('NS2') +
                                  AchaValorRegistrador('NS3') ;

    IsentoISSQN                := AchaValorRegistrador('IS1') +
                                  AchaValorRegistrador('IS2') +
                                  AchaValorRegistrador('IS3') ;

    CalculaValoresVirtuais;
    Result := MontaDadosReducaoZ;
  end;
end ;

function TACBrECFEscECF.GetVendaBruta: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|2' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToInt( StrValue ) / 100;
end;

procedure TACBrECFEscECF.FechaNaoFiscal(Observacao: AnsiString;
   IndiceBMP: Integer);
var
  Obs: String;
begin
  Obs := Observacao ;

  if not Consumidor.Enviado then
     EnviaConsumidor;

  with EscECFComando do
  begin
     CMD := 18 ;
     AddParamInteger( 1 );  // Aciona a Guilhotina
     AddParamString( Obs );
  end ;
  EnviaComando ;

  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
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
  EscECFComando.CMD := 23;
  EscECFComando.AddParamInteger( 2 ) ;  // Sangria
  EscECFComando.AddParamDouble( Valor ) ;
  EscECFComando.AddParamString( Obs ) ;
  EnviaComando;

  RespostasComando.Clear;
  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  SalvaRespostasMemoria(False);
end;

procedure TACBrECFEscECF.Suprimento(const Valor: Double; Obs: AnsiString;
   DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer);
begin
  EscECFComando.CMD := 23;
  EscECFComando.AddParamInteger( 1 ) ;  // Fundo de Troco
  EscECFComando.AddParamDouble( Valor ) ;
  EscECFComando.AddParamString( Obs ) ;
  EnviaComando;

  RespostasComando.Clear;
  RespostasComando.AddField( 'COO',        EscECFResposta.Params[0] );
  RespostasComando.AddField( 'DataHora',   EscECFResposta.Params[1] );
  RespostasComando.AddField( 'VendaBruta', EscECFResposta.Params[2] );
  RespostasComando.AddField( 'NumSerie',   EscECFResposta.Params[3] );
  SalvaRespostasMemoria(False);
end;

function TACBrECFEscECF.EstornaCCD(const Todos: Boolean): Integer;
var
   UltimoCOO, AtualCOO: Integer;
begin
  Result := 0 ;
  UltimoCOO := StrToInt( TACBrECF( fpOwner ).NumCOO );
  AtualCOO  := UltimoCOO;

  try
    repeat
      with EscECFComando do
      begin
        CMD := 13;
        AddParamInteger( UltimoCOO ) ;
        AddParamString(LeftStr(OnlyNumber(Consumidor.Documento),14)) ;
        AddParamString(LeftStr(Consumidor.Nome,30)) ;
        AddParamString(LeftStr(Consumidor.Endereco,79)) ;
      end;
      EnviaComando;

      RespostasComando.Clear;
      RespostasComando.AddField( 'COO',            EscECFResposta.Params[0] );
      RespostasComando.AddField( 'DataHora',       EscECFResposta.Params[1] );
      RespostasComando.AddField( 'VendaBruta',     EscECFResposta.Params[2] );
      RespostasComando.AddField( 'NumSerie',       EscECFResposta.Params[3] );
      RespostasComando.AddField( 'SeqPagto',       EscECFResposta.Params[4] );
      RespostasComando.AddField( 'NumParcela',     EscECFResposta.Params[5] );

      FechaRelatorio;

      Dec( UltimoCOO );
    until not Todos;
  except
    if UltimoCOO = AtualCOO then  // Não cancelou nada ?
      raise;
  end ;

  Consumidor.Enviado := True ;
  fsEmPagamento := false ;
end;

function TACBrECFEscECF.GetTotalAcrescimos: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|8' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToInt( StrValue ) / 100;
end;

function TACBrECFEscECF.GetTotalCancelamentos: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|3' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToInt( StrValue ) / 100;
end;

function TACBrECFEscECF.GetTotalDescontos: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|4' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToInt( StrValue ) / 100;
end;

function TACBrECFEscECF.GetTotalTroco: Double;
var
  StrValue: String;
begin
  RetornaInfoECF( '7|21' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToInt( StrValue ) / 100;
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
  Result   := StrToInt( StrValue ) / 100;
end ;

function TACBrECFEscECF.GetTotalCancelamentosISSQN : Double ;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|5' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToInt( StrValue ) / 100;
end ;

function TACBrECFEscECF.GetTotalDescontosISSQN : Double ;
var
  StrValue: String;
begin
  RetornaInfoECF( '4|6' ) ;
  StrValue := EscECFResposta.Params[1] ;
  Result   := StrToInt( StrValue ) / 100;
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
  if (upcase(AliquotaICMS[1]) = 'T') then
    AliquotaICMS := 'TT'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}

  Result := inherited AchaICMSAliquota( AliquotaICMS );
end;

end.

{ Observaçoes:

- Registro E01 do comando 139 traz a Data de Sw.Basico no formato DDMMAAAA quando o correto é AAAAMMDD

- Não encontrado:
-- Num.Loja
-- Total Acrescimos OPNF
-- Total Cancelamentos OPNF
-- Total Descontos OPNF
}

