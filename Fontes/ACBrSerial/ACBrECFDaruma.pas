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

unit ACBrECFDaruma ;

interface
uses
  Classes,
  {$IFDEF NEXTGEN}
   ACBrBase,
  {$ENDIF}
  ACBrECFClass, ACBrDevice, ACBrDeviceSerial;

const
   ARQ_MFD_DLL = 'Espelho_MFD.txt';
   cDELIMITADOR = #255 ;

  {$IFDEF LINUX}
   cLIB_Daruma = 'libDarumaFramework.so';
  {$ELSE}
   cLIB_Daruma = 'DarumaFrameWork.dll';
  {$ENDIF}

type

{ Tipo enumerado para separar os modelos daruma }
TACBrModelosDaruma = (fs315, fs345, fs2000, fs600, fs2100T, fs600USB, fs700L, fs700H,
                      fs700M, fsMACH1, fsMACH2, fsMACH3, fsIndefinido);

{ Classe filha de TACBrECFClass com implementaçao para Daruma }

{ TACBrECFDaruma }

TACBrECFDaruma = class( TACBrECFClass )
 private
    fsNumVersao   : String ;
    fsNumECF      : String ;
    fsNumLoja     : String ;
    fsUsuarioAtual: String ;
    fsNumCupom    : String ; //COO
    fsNumCCF      : String ;
    fsNumUltimoItem : Integer ;
    fsSubTotal    : Double ;
    fsArredonda   : Char ;
    fsTotalAPagar : Double ;
    fsEmPagamento : Boolean ;
    fsCNFVinc     : TACBrECFComprovantesNaoFiscais ;
    fsTipoRel     : Char ;
    fsEsperaFFCR  : Boolean ;
    fsComandosImpressao : array[0..22] of AnsiString ;

    fsModeloDaruma: TACBrModelosDaruma;
    fsSubModeloECF: String ;

    fsRet244: AnsiString ;
    fsNumCRO: String ;
    fsErro, fsErroSTD, fsCodAviso : Integer ;

    xeCarregarBitmapPromocional_ECF_Daruma: function(APathArquivo, ANumBitmap, AOrientacao: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xeDefinirModoRegistro_Daruma: function(Local: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xeDefinirProduto: function(Tipo: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xregAlterarValor_Daruma: function(Chave: AnsiString; Valor: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xrGerarEspelhoMFD_ECF_Daruma: function(ATipo , AInicial, AFinal: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xrGerarRelatorio_ECF_Daruma: function(ARelatorio, ATipo , AInicial, AFinal: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xrGerarRelatorioOffline_ECF_Daruma: function (ARelatorio, ATipo,
      AInicial, AFinal, AArquivo_MF, AArquivo_MFD, AArquivo_INF: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xrEfetuarDownloadMF_ECF_Daruma: function(pszNomeArquivo:AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xrEfetuarDownloadMFD_ECF_Daruma: function(pszTipo, pszInicio, pszFim, pszNomeArquivo:AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

    procedure LoadDLLFunctions;
    procedure UnloadDLLFunctions;
    procedure ConfigurarDLL(Path : AnsiString );

    Function PreparaCmd( const cmd : AnsiString ) : AnsiString ;
    function GetComprovantesNaoFiscaisVinculado: TACBrECFComprovantesNaoFiscais;
    function LimpaRetorno( const Retorno : AnsiString ) : AnsiString ;

    function GetRet244: AnsiString;
    procedure ZeraTotalApagar ;
    procedure VerificarBmpTexto(var IndiceBMP: Integer; const ATexto: String);
    function ModoPreVendaAtivado: Boolean;
    property Ret244 : AnsiString read GetRet244 ;
    function LimpaChr0(const AString: AnsiString): AnsiString;
    function EnviaComando_ECF_Daruma(cmd: AnsiString): AnsiString;
    Function DocumentosToStr(Documentos : TACBrECFTipoDocumentoSet) : String ;
    function GetDescricaoErroDLL(const ACodigo: Integer): String;
 protected
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumCCF: String; override ;
    function GetNumECF: String; override ;
    function GetNumLoja: String; override ;
    function GetNumCRO: String; override ;
    function GetNumGNF: String; override ;
    function GetNumGRG: String; override ;
    function GetNumCDC: String; override ;
    function GetNumCFC: String; override ;
    function GetNumSerie: String; override ;
    function GetNumSerieMFD: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;
    function GetNumReducoesZRestantes: String; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;
    function GetArredonda : Boolean; override ;
    function GetParamDescontoISSQN: Boolean; override ;

    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetIM: String; override ;
    function GetCliche: AnsiString; override ;
    function GetUsuarioAtual: String; override ;
    function GetDataHoraSB: TDateTime; override ;
    function GetSubModeloECF: String ; override ;

    function GetPAF: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetDataHoraUltimaReducaoZ : TDateTime ; override ;

    function GetNumCRZ: String; override ;
    function GetGrandeTotal: Double; override ;
    function GetVendaBruta: Double; override ;

    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalTroco: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;
    function GetTotalNaoFiscal: Double; override ;

    function GetTotalAcrescimosISSQN: Double; override;
    function GetTotalCancelamentosISSQN: Double; override;
    function GetTotalDescontosISSQN: Double; override;
    function GetTotalSubstituicaoTributariaISSQN: Double; override;
    function GetTotalIsencaoISSQN: Double; override;
    function GetTotalNaoTributadoISSQN: Double; override;

    function GetTotalAcrescimosOPNF: Double; override ;
    function GetTotalCancelamentosOPNF: Double; override ;
    function GetTotalDescontosOPNF: Double; override ;

    function GetNumCOOInicial: String; override ;
    function GetNumUltimoItem: Integer; override ;

    function GetDadosUltimaReducaoZ: String; override ;

    procedure SetDecimaisPreco(AValue: Integer); override;
    procedure SetDecimaisQtd(AValue: Integer); override;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;
    function VerificaFimImpressao(var TempoLimite: TDateTime) : Boolean ; override ;

    function ErroEstendidoTexto(AErro: integer): string;
 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    procedure LegendaInmetroProximoItem ; override ; // Função implementada até o momento apenas para Daruma
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    Procedure DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo : Double = 0;
       DescontoAcrescimo : String = 'D'; TipoDescontoAcrescimo : String = '%';
       NumItem : Integer = 0 ) ;  override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '' ) ; override ;
    procedure CancelaDescontoAcrescimoSubTotal(TipoAcrescimoDesconto: Char) ;
       override ;//A -> Acrescimo D -> Desconto  // Função implementada até o momento apenas para Daruma
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    procedure EstornaPagamento(const CodFormaPagtoEstornar,
      CodFormaPagtoEfetivar : String; const Valor: Double;
      Observacao : AnsiString = '') ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;
    procedure CancelaItemVendidoParcial( NumItem : Integer;
      Quantidade : Double) ; override ; // Função implementada até o momento apenas para Daruma
    procedure CancelaDescontoAcrescimoItem( NumItem : Integer;
      TipoAcrescimoDesconto: String = 'D') ;override ;
    Procedure LeituraX ; override ;
    Procedure LeituraXSerial( Linhas : TStringList) ; override ;
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;
    procedure SegundaViaVinculado; override;
    procedure ReimpressaoVinculado; override;

   //Cheque
    Procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ; override ;
    Procedure CancelaImpressaoCheque ; override ;       

    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure MudaArredondamento( Arredondar : Boolean ) ; override ;
    Procedure ImpactoAgulhas( NivelForca : Integer = 2) ; override ;
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False ); override ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False  ) ; override ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal : Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMFDSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;
    Procedure LeituraMFDSerial( COOInicial, COOFinal : Integer;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;

    Procedure EspelhoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; override ;
    Procedure EspelhoMFD_DLL( COOInicial, COOFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; override ;
    Procedure ArquivoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]; Finalidade: TACBrECFFinalizaArqMFD = finMFD  ) ; override ;
    Procedure ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD;
       TipoContador: TACBrECFTipoContador = tpcCOO  ) ; override ;



    Procedure ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString); override;
    Procedure ArquivoMFD_Binario_DLL(Tipo: TACBrECFTipoDownloadMFD;
      const NomeArquivo: AnsiString; StrInicial, StrFinal: AnsiString); override;

    Procedure IdentificaOperador ( Nome: String); override;
    Procedure IdentificaPAF( NomeVersao, MD5 : String) ; override ;
    Function RetornaInfoECF( Registrador : String ) : AnsiString; override ;
    function DecodificaTexto(Operacao: Char; Texto: String; var Resposta: String): Boolean; override;

    { Procedimentos de Cupom Não Fiscal }
    procedure NaoFiscalCompleto(CodCNF: String; Valor: Double;
      CodFormaPagto: String; Obs: AnsiString; IndiceBMP : Integer = 0); override ;
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ); override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ; override ;
    Procedure EfetuaPagamentoNaoFiscal( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false) ; override ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaNaoFiscal ; override ;
    Procedure CancelaItemNaoFiscal(const AItem: Integer); override;

    procedure Sangria( const Valor: Double;  Obs: AnsiString; DescricaoCNF,
       DescricaoFPG: String; IndiceBMP: Integer ) ; override ;
    procedure Suprimento( const Valor: Double; Obs: AnsiString; DescricaoCNF,
       DescricaoFPG: String; IndiceBMP: Integer ) ; override ;

    Procedure AbreGaveta ; override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota ; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  override;

    procedure CarregaTotalizadoresNaoTributados ; override;
    procedure LerTotaisTotalizadoresNaoTributados ; override;

    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaRelatoriosGerenciais ; override ;
    procedure LerTotaisRelatoriosGerenciais ; override ;
    Procedure ProgramaRelatorioGerencial( var Descricao: String;
       Posicao : String = '') ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;
    procedure PafMF_GerarCAT52(const DataInicial, DataFinal: TDateTime;
      const DirArquivos: String; NumeroSerie: String = ''); override;

    Property ComprovantesNaoFiscaisVinculado : TACBrECFComprovantesNaoFiscais
       read GetComprovantesNaoFiscaisVinculado ;
    function AchaCNFVincIndice( const Indice: String): TACBrECFComprovanteNaoFiscal;
    function AchaCNFVincDescricao( Descricao : String ) :
       TACBrECFComprovanteNaoFiscal ;

    Procedure CortaPapel( const CorteParcial : Boolean = false) ; override ;
    // Função para mudar inpressora para modo online automaticamente
    procedure ComutaOnLine;

    procedure ProgramarBitmapPromocional(const AIndice: Integer;
      const APathArquivo: AnsiString;
      const AAlinhamento: TACBrAlinhamento = alCentro); override;

    function TraduzirTag(const ATag: AnsiString): AnsiString; override;
    function TraduzirTagBloco(const ATag, Conteudo: AnsiString): AnsiString; override;

end ;

function DescricaoRetornoDaruma( Byte1, Byte2 : Byte ): String;
function DarumaTraduzirTag(const ATag: AnsiString): AnsiString;
function DarumaTraduzirTagBloco(const ATag, Conteudo: AnsiString;
  AECFClass: TACBrECFClass): AnsiString;



implementation
Uses
    {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
    SysUtils, Math,
    {$IFDEF COMPILER6_UP} DateUtils, StrUtils, {$ELSE} ACBrD5, {$ENDIF}
    ACBrUtil, ACBrECF, ACBrECFEscECF, ACBrConsts;

{ ----------------------------- TACBrECFDaruma ------------------------------ }

const


  ErrosEstendidos: array [1 .. 403] of string = (
    { 1 } 'ECF com falha mecânica',
    { 2 } 'MF não conectada',
    { 3 } 'MFD não conectada',
    { 4 } 'MFD esgotada',
    { 5 } 'Erro na comunicação com a MF',
    { 6 } 'Erro na comunicação com a MFD',
    { 7 } 'MF não inicializada',
    { 8 } 'MFD não inicializada',
    { 9 } 'MFD já inicializada',
    { 10 } 'MFD foi substituída',
    { 11 } 'MFD já cadastrada',
    { 12 } 'Erro na inicialização da MFD',
    { 13 } 'Faltam parâmetros de inicialização na MF',
    { 14 } 'Comando não suportado',
    { 15 } 'Superaquecimento da cabeça de impressão',
    { 16 } 'Perda de dados da MT',
    { 17 } 'Operação habilitada apenas em MIT',
    { 18 } 'Operação habilitada apenas em modo fiscal',
    { 19 } 'Data inexistente',
    { 20 } 'Data inferior ao do último documento',
    { 21 } 'Intervalo inconsistente',
    { 22 } 'Não existem dados',
    { 23 } 'Clichê de formato inválido',
    { 24 } 'Erro no verificador da comunicação',
    { 25 } 'Senha incorreta',
    { 26 } 'Número de decimais para quantidade inválido',
    { 27 } 'Número de decimais para valor unitário inválido',
    { 28 } 'Tipo de impressão de FD inválido',
    { 29 } 'Caracter não estampável',
    { 30 } 'Caracter não estampável ou em branco',
    { 31 } 'Caracteres não podem ser repetidos',
    { 32 } 'Limite de itens atingido',
    { 33 } 'Todos os totalizadores fiscais já estão programados',
    { 34 } 'Totalizador fiscal já programado',
    { 35 } 'Todos os totalizadores não fiscais já estão programados',
    { 36 } 'Totalizador não fiscal já programado',
    { 37 } 'Todos os relatórios gerenciais já estão programados',
    { 38 } 'Relatório gerencial já programado',
    { 39 } 'Meio de pagamento já programado',
    { 40 } 'Índice inválido',
    { 41 } 'Índice do meio de pagamento inválido',
    { 42 } 'Erro gravando número de decimais na MF',
    { 43 } 'Erro gravando moeda na MF',
    { 44 } 'Erro gravando símbolos de decodificação do GT na MF',
    { 45 } 'Erro gravando número de fabricação da MFD na MF',
    { 46 } 'Erro gravando usuário na MF',
    { 47 } 'Erro gravando GT do usuário anterior na MF',
    { 48 } 'Erro gravando registro de marcação na MF',
    { 49 } 'Erro gravando CRO na MF',
    { 50 } 'Erro gravando impressão de FD na MF',
    { 51 } 'Campo em branco ou zero não permitido',
    { 52 } 'Campo reservado a gravação da moeda na MF esgotado',
    { 53 } 'Campo reservado a gravação da tabela de GT na MF esgo',
    { 54 } 'Campo reservado a gravação do NS da MFD na MF esgo',
    { 55 } 'Campo reservado a gravação de usuário na MF esgotado',
    { 56 } 'CNPJ inválido',
    { 57 } 'CRZ e CRO em zero',
    { 58 } 'Intervalo invertido',
    { 59 } 'Utilize apenas 0 ou 1',
    { 60 } 'Configuração permitida apenas imediatamente a RZ',
    { 61 } 'Símbolo gráfico inválido',
    { 62 } 'Falta pelo menos 1 campo no nome da moeda para chequ',
    { 63 } 'Código supera o valor 255',
    { 64 } 'Utilize valores entre 25 e 80',
    { 65 } 'Utilize valores entre 1 e 15',
    { 66 } 'Utilize valores entre 0 e 7250',
    { 67 } 'Data informada não coincide com a data do ECF',
    { 68 } 'Deve ajustar o relógio ( utilize o comando [FS] M <200> )',
    { 69 } 'Erro ao ajustar o relógio',
    { 70 } 'Capacidade da MF esgotada',
    { 71 } 'Versão do SB gravado na MF incorreta',
    { 72 } 'Fim do papel',
    { 73 } 'Nenhum usuário programado',
    { 74 } 'Utilize apenas dígitos numéricos',
    { 75 } 'Campo não pode estar em zero',
    { 76 } 'Campo não pode estar em branco',
    { 77 } 'Valor da operação não pode ser zero',
    { 78 } 'CF aberto',
    { 79 } 'CNF aberto',
    { 80 } 'CCD aberto',
    { 81 } 'RG aberto',
    { 82 } 'CF não aberto',
    { 83 } 'CNF não aberto',
    { 84 } 'CCD não aberto',
    { 85 } 'RG não aberto',
    { 86 } 'CCD ou RG não aberto',
    { 87 } 'Documento já totalizado',
    { 88 } 'RZ do movimento anterior pendente',
    { 89 } 'Já emitiu RZ de hoje',
    { 90 } 'Totalizador sem alíquota programada',
    { 91 } 'Campo de código ausente',
    { 92 } 'Campo de descrição ausente',
    { 93 } 'VU ou quantidade em zero',
    { 94 } 'Item ainda não vendido',
    { 95 } 'Desconto ou acréscimo não pode ser zero',
    { 96 } 'Item já possui desconto ou acréscimo',
    { 97 } 'Ítem cancelado',
    { 98 } 'Operação inibida por configuração',
    { 99 } 'Opção não suportada',
    { 100 } 'Desconto ou acréscimo supera valor bruto',
    { 101 } 'Desconto ou acréscimo final de valor zero',
    { 102 } 'Valor bruto zero',
    { 103 } 'Overflow no valor do item',
    { 104 } 'Overflou no valor do desconto ou acréscimo',
    { 105 } 'Overflow na capacidade do documento',
    { 106 } 'Overflow na capacidade do totalizador',
    { 107 } 'Item não possui desconto',
    { 108 } 'Item já possui desconto',
    { 109 } 'Quantidade possui mais de 2 decimais',
    { 110 } 'Valor unitário possui mais de 2 decimais',
    { 111 } 'Quantidade a cancelar deve ser inferior a total',
    { 112 } 'Campo de descrição deste item não mais presente na MT',
    { 113 } 'Subtotal não possui desconto ou acréscimo',
    { 114 } 'Não em fase de totalização',
    { 115 } 'Não em fase de venda ou totalização',
    { 116 } 'Mais de 1 desconto ou acréscimo não permitido',
    { 117 } 'Valor do desconto ou acréscimo supera subtotal',
    { 118 } 'Meio de pagamento não programado',
    { 119 } 'Não em fase de pagamento ou totalização',
    { 120 } 'Não em fase de finalização de documento',
    { 121 } 'Já emitiu mais CCDs que poderia estornar',
    { 122 } 'Último documento não é cancelável',
    { 123 } 'Estorne CCDs',
    { 124 } 'Último documento não foi CF',
    { 125 } 'Último documento não foi CNF',
    { 126 } 'Não pode cancelar',
    { 127 } 'Pagamento não mais na MT',
    { 128 } 'Já emitiu CCD deste pagamento',
    { 129 } 'RG não programado',
    { 130 } 'CNF não programado',
    { 131 } 'Cópia não disponível',
    { 132 } 'Já emitiu segunda via',
    { 133 } 'Já emitiu reimpressão',
    { 134 } 'Informações sobre o pagamento não disponíveis',
    { 135 } 'Já emitiu todas as parcelas',
    { 136 } 'Parcelamento somente na sequência',
    { 137 } 'CCD não encontrado',
    { 138 } 'Não pode utilizar SANGRIA ou SUPRIMENTO',
    { 139 } 'Pagamento não admite CCD',
    { 140 } 'Relógio inoperante',
    { 141 } 'Usuário sem CNPJ',
    { 142 } 'Usuário sem IM',
    { 143 } 'Não se passou 1 hora após o fechamento do último documen',
    { 144 } 'ECF OFF LINE',
    { 145 } 'Documento em emissão',
    { 146 } 'COO não coincide',
    { 147 } 'Erro na autenticação',
    { 148 } 'Erro na impressão de cheque',
    { 149 } 'Data não pertence ao século XXI',
    { 150 } 'Usuário já programado',
    { 151 } 'Descrição do pagamento já utilizada',
    { 152 } 'Descrição do totalizador já utilizada',
    { 153 } 'Descrição do RG já utilizada',
    { 154 } 'Já tem desconto após acréscimo ( ou vice versa )',
    { 155 } 'Já programou 15 totalizadores para ICMS',
    { 156 } 'Já programou 15 totalizadores para ISS',
    { 157 } 'MFD com problemas',
    { 158 } 'Razão social excede 48 caracteres',
    { 159 } 'Nome fantasia excede 48 caracteres',
    { 160 } 'Endereço excede 120 caracteres',
    { 161 } 'Identificação do programa aplicativo ausente',
    { 162 } 'Valor de desconto supera valor acumulado em totalizador',
    { 163 } 'Número de parcelas no pagamento não pode exceder 24',
    { 164 } 'MFD não cadastrada',
    { 165 } 'Excedeu limite de impressão de FD ( capacidade na MF esgotada )',
    { 166 } 'Efetivado é igual ao estornado',
    { 167 } 'Símbolo da moeda já programado',
    { 168 } 'UF inválida',
    { 169 } 'UF já programada',
    { 170 } 'Erro gravando UF',
    { 171 } 'Leitor CMC-7 não instalado',
    { 172 } 'Erro de leitura do código CMC-7',
    { 173 } 'Autenticação não permitida',
    { 174 } 'Operação somente com mecanismo matricial de impacto',
    { 175 } 'Coordenadas de cheque inválidas',
    { 176 } 'Impressão do verso do cheque somente após a impressão da frente',
    { 177 } 'Indice do bitmap inválido',
    { 178 } 'Bitmap de tamanho inválido',
    { 179 } 'Última RZ a mais de 30 dias. Comando de RZ deve informar data correta',
    { 180 } 'Erro não documentado',
    { 181 } 'Erro não documentado',
    { 182 } 'Erro não documentado',
    { 183 } 'Erro não documentado',
    { 184 } 'Parâmetro só pode ser "A" ou "T"',
    { 185 } 'Falta unidade do produto',
    { 186 } 'Velocidade não permitida',
    { 187 } 'Código repetido',
    { 188 } 'Fora dos limites',
    { 189 } 'Já identificou o consumidor',
    { 190 } 'Número de Fabricação incorreto',
    { 191 } 'Informação disponível não corresponde a MF informada',
    { 192 } 'MF já em uso',
    { 193 } 'Falha não recuperável durante a operação',
    { 194 } 'Opção inválida',
    { 195 } 'Parâmetros inválidos',
    { 196 } 'Caracter HEXA inválido',
    { 197 } 'Valor insuficiente de pagamento',
    { 198 } 'IE inválido',
    { 199 } 'IM inválido',
    { 200 } 'Erro não documentado',
    { 201 } 'Erro não documentado',
    { 202 } 'Erro não documentado',
    { 203 } 'Erro não documentado',
    { 204 } 'Erro não documentado',
    { 205 } 'Erro não documentado',
    { 206 } 'Erro não documentado',
    { 207 } 'Erro não documentado',
    { 208 } 'Erro não documentado',
    { 209 } 'Erro não documentado',
    { 210 } 'Erro não documentado',
    { 211 } 'Erro não documentado',
    { 212 } 'Erro não documentado',
    { 213 } 'Erro não documentado',
    { 214 } 'Erro não documentado',
    { 215 } 'Erro não documentado',
    { 216 } 'Erro não documentado',
    { 217 } 'Erro não documentado',
    { 218 } 'Erro não documentado',
    { 219 } 'Erro não documentado',
    { 220 } 'Erro não documentado',
    { 221 } 'Erro não documentado',
    { 222 } 'Erro não documentado',
    { 223 } 'Erro não documentado',
    { 224 } 'Erro não documentado',
    { 225 } 'Erro não documentado',
    { 226 } 'Erro não documentado',
    { 227 } 'Erro não documentado',
    { 228 } 'Erro não documentado',
    { 229 } 'Erro não documentado',
    { 230 } 'Erro não documentado',
    { 231 } 'Erro não documentado',
    { 232 } 'Erro não documentado',
    { 233 } 'Erro não documentado',
    { 234 } 'Erro não documentado',
    { 235 } 'Erro não documentado',
    { 236 } 'Erro não documentado',
    { 237 } 'Erro não documentado',
    { 238 } 'Erro não documentado',
    { 239 } 'Erro não documentado',
    { 240 } 'Erro não documentado',
    { 241 } 'Erro não documentado',
    { 242 } 'Erro não documentado',
    { 243 } 'Erro não documentado',
    { 244 } 'Erro não documentado',
    { 245 } 'Erro não documentado',
    { 246 } 'Erro não documentado',
    { 247 } 'Erro não documentado',
    { 248 } 'Erro não documentado',
    { 249 } 'Erro não documentado',
    { 250 } 'Erro não documentado',
    { 251 } 'Erro não documentado',
    { 252 } 'Erro não documentado',
    { 253 } 'Erro não documentado',
    { 254 } 'Erro não documentado',
    { 255 } 'Erro não documentado',
    { 256 } 'Erro não documentado',
    { 257 } 'Erro não documentado',
    { 258 } 'Erro não documentado',
    { 259 } 'Erro não documentado',
    { 260 } 'Erro não documentado',
    { 261 } 'Erro não documentado',
    { 262 } 'Erro não documentado',
    { 263 } 'Erro não documentado',
    { 264 } 'Erro não documentado',
    { 265 } 'Erro não documentado',
    { 266 } 'Erro não documentado',
    { 267 } 'Erro não documentado',
    { 268 } 'Erro não documentado',
    { 269 } 'Erro não documentado',
    { 270 } 'Erro não documentado',
    { 271 } 'Erro não documentado',
    { 272 } 'Erro não documentado',
    { 273 } 'Erro não documentado',
    { 274 } 'Erro não documentado',
    { 275 } 'Erro não documentado',
    { 276 } 'Erro não documentado',
    { 277 } 'Erro não documentado',
    { 278 } 'Erro não documentado',
    { 279 } 'Erro não documentado',
    { 280 } 'Erro não documentado',
    { 281 } 'Erro não documentado',
    { 282 } 'Erro não documentado',
    { 283 } 'Erro não documentado',
    { 284 } 'Erro não documentado',
    { 285 } 'Erro não documentado',
    { 286 } 'Erro não documentado',
    { 287 } 'Erro não documentado',
    { 288 } 'Erro não documentado',
    { 289 } 'Erro não documentado',
    { 290 } 'Erro não documentado',
    { 291 } 'Erro não documentado',
    { 292 } 'Erro não documentado',
    { 293 } 'Erro não documentado',
    { 294 } 'Erro não documentado',
    { 295 } 'Erro não documentado',
    { 296 } 'Erro não documentado',
    { 297 } 'Erro não documentado',
    { 298 } 'Erro não documentado',
    { 299 } 'Erro não documentado',
    { 300 } 'Erro não documentado',
    { 301 } 'CFBP Inibido',
    { 302 } 'Modalidade de Transporte inválida',
    { 303 } 'Categoria de Transporte inválida',
    { 304 } 'UF incompatível',
    { 305 } 'Comando disponível apenas em CF genérico',
    { 306 } 'Erro não documentado',
    { 307 } 'Erro não documentado',
    { 308 } 'Erro não documentado',
    { 309 } 'Erro não documentado',
    { 310 } 'Erro não documentado',
    { 311 } 'Erro não documentado',
    { 312 } 'Erro não documentado',
    { 313 } 'Erro não documentado',
    { 314 } 'Erro não documentado',
    { 315 } 'Erro não documentado',
    { 316 } 'Erro não documentado',
    { 317 } 'Erro não documentado',
    { 318 } 'Erro não documentado',
    { 319 } 'Erro não documentado',
    { 320 } 'Erro não documentado',
    { 321 } 'Erro não documentado',
    { 322 } 'Erro não documentado',
    { 323 } 'Erro não documentado',
    { 324 } 'Erro não documentado',
    { 325 } 'Erro não documentado',
    { 326 } 'Erro não documentado',
    { 327 } 'Erro não documentado',
    { 328 } 'Erro não documentado',
    { 329 } 'Erro não documentado',
    { 330 } 'Erro não documentado',
    { 331 } 'Erro não documentado',
    { 332 } 'Erro não documentado',
    { 333 } 'Erro não documentado',
    { 334 } 'Erro não documentado',
    { 335 } 'Erro não documentado',
    { 336 } 'Erro não documentado',
    { 337 } 'Erro não documentado',
    { 338 } 'Erro não documentado',
    { 339 } 'Erro não documentado',
    { 340 } 'Erro não documentado',
    { 341 } 'Erro não documentado',
    { 342 } 'Erro não documentado',
    { 343 } 'Erro não documentado',
    { 344 } 'Erro não documentado',
    { 345 } 'Erro não documentado',
    { 346 } 'Erro não documentado',
    { 347 } 'Erro não documentado',
    { 348 } 'Erro não documentado',
    { 349 } 'Erro não documentado',
    { 350 } 'Erro não documentado',
    { 351 } 'Erro não documentado',
    { 352 } 'Erro não documentado',
    { 353 } 'Erro não documentado',
    { 354 } 'Erro não documentado',
    { 355 } 'Erro não documentado',
    { 356 } 'Erro não documentado',
    { 357 } 'Erro não documentado',
    { 358 } 'Erro não documentado',
    { 359 } 'Erro não documentado',
    { 360 } 'Erro não documentado',
    { 361 } 'Erro não documentado',
    { 362 } 'Erro não documentado',
    { 363 } 'Erro não documentado',
    { 364 } 'Erro não documentado',
    { 365 } 'Erro não documentado',
    { 366 } 'Erro não documentado',
    { 367 } 'Erro não documentado',
    { 368 } 'Erro não documentado',
    { 369 } 'Erro não documentado',
    { 370 } 'Erro não documentado',
    { 371 } 'Erro não documentado',
    { 372 } 'Erro não documentado',
    { 373 } 'Erro não documentado',
    { 374 } 'Erro não documentado',
    { 375 } 'Erro não documentado',
    { 376 } 'Erro não documentado',
    { 377 } 'Erro não documentado',
    { 378 } 'Erro não documentado',
    { 379 } 'Erro não documentado',
    { 380 } 'Erro não documentado',
    { 381 } 'Erro não documentado',
    { 382 } 'Erro não documentado',
    { 383 } 'Erro não documentado',
    { 384 } 'Erro não documentado',
    { 385 } 'Erro não documentado',
    { 386 } 'Erro não documentado',
    { 387 } 'Erro não documentado',
    { 388 } 'Erro não documentado',
    { 389 } 'Erro não documentado',
    { 390 } 'Erro não documentado',
    { 391 } 'Erro não documentado',
    { 392 } 'Erro não documentado',
    { 393 } 'Erro não documentado',
    { 394 } 'Erro não documentado',
    { 395 } 'Erro não documentado',
    { 396 } 'Erro não documentado',
    { 397 } 'Erro não documentado',
    { 398 } 'Erro não documentado',
    { 399 } 'Erro não documentado',
    { 400 } 'Chave não carregada',
    { 401 } 'Chave inválida',
    { 402 } 'Erro na decodificação',
    { 403 } 'Erro na codificação');

function DescricaoRetornoDaruma( Byte1, Byte2 : Byte ): String;
begin
  case Byte2 of
      1: Result := 'Falha mecânica reportada pela bios';
      4: Result := 'MFD esgotada';
      6: Result := 'MFD em falha (não responde)';
      8: Result := 'MFD não inicializada';
      9: Result := 'MFD já inicializada';
     12: Result := 'Erro inicializando MFD';
     13: Result := 'Faltam parâmetros obrigató ios na MF';
     15: Result := 'Superaquecimento da cabeça térmica 16.Dados da MT inconsistentes';
     16: Result := 'Dados da MT inconsistentes';
     17: Result := 'Operação permitida apenas em MIL';
     18: Result := 'Operação permitida apenas em Modo Fiscal';
     19: Result := 'Data inválida';
     22: Result := 'Não existem dados no intervalo solicitado';
     25: Result := 'Senha incorreta';
     26: Result := 'Casas decimais para quantidade inválida';
     27: Result := 'Casas decimais para valor unitário inválida';
     28: Result := 'Tipo de reimpressão inválido (1=por Data  2=por COO)';
     29: Result := 'Encontrado caracter não estampável';
     40: Result := 'Índice inválido';
     41: Result := 'Índice de pagamento inválido';
     47: Result := 'Não pode utilizar totalizadores de entrada e saída em um mesmo CNF';
     51: Result := 'Parâmetro não pode ser zero ou estar em branco';
     56: Result := 'CNPJ inválido';
     57: Result := 'MF vazia';
     58: Result := 'Referência final inferior a referência inicial';
     59: Result := 'Valores aceitos são 0 ou 1 apenas';
     60: Result := 'Configuração permitida apenas imediatamente após a RZ';
     67: Result := 'Data informada na RZ não pode ser aceita';
     69: Result := 'Falha irrecuperável ao ajustar o relógio';
     70: Result := 'MF esgotada';
     71: Result := 'Versão do SB gravada na MF incorreta';
     73: Result := 'Usuário não programado';
     74: Result := 'Campo admite apenas valores numéricos';
     75: Result := 'Encontrado campo em zero';
     76: Result := 'Encontrado campo em branco';
     77: Result := 'Valor da operação não pode ser zero';
     82: Result := 'CF não está aberto';
     83: Result := 'CNF não está aberto';
     84: Result := 'CCD/ CCD Estorno não está aberto';
     85: Result := 'RG não está aberto';
     86: Result := 'Não existe CCD ou RG aberto';
     90: Result := 'Carga tributária não programada';
     91: Result := 'Campo do código vazio';
     92: Result := 'Campo de descrição vazio';
     93: Result := 'Valor unitário ou quantidade em zero';
     94: Result := 'Ítem ainda nã   foi registrado';
     95: Result := 'Desconto não pode   er zero';
     97: Result := 'Item foi cancelado';
     98: Result := 'Operação inibida por configuração';
     99: Result := 'Opção inválida';
    100: Result := 'Desconto supera valor bruto';
    101: Result := 'Desconto final resulta em zero';
    102: Result := 'Valor bruto resulta em zero';
    103: Result := 'Overflow no valor do item';
    104: Result := 'Overflow no valor do desconto';
    105: Result := 'Overflow no valor do documento';
    107: Result := 'Ítem não teve desconto';
    108: Result := 'Ítem com desconto';
    109: Result := 'Utilizado quantidade com mais de 2 decimais';
    110: Result := 'Utilizado valor unitário com mais de 2 decimais';
    111: Result := 'Quantidade a cancelar deve ser inferior à vendida';
    113: Result := 'Subtotal não teve desconto';
    114: Result := 'Não em estado de totalização';
    115: Result := 'Não em estado de venda ou totalização';
    116: Result := 'Permitido apenas 1 desconto e 1 acréscimo';
    117: Result := 'Desconto supera subtotal';
    118: Result := 'Pagamento não programado';
    119: Result := 'Não em estado de pagamento ou totalização';
    126: Result := 'Cancelamento não foi possível';
    127: Result := 'Informação sobre o pagamento não mais disponível';
    128: Result := 'Foram emididos CCDs relativos a este pagamento';
    129: Result := 'RG não programado';
    130: Result := 'Totalizador Não-Fiscal não programado';
    131: Result := 'Segunda via não disponível';
    132: Result := 'Já emitiu segunda via';
    138: Result := 'Totalizador de Sangria e Suprimento inibidos neste CNF';
    140: Result := 'Relógio está travado';
    141: Result := 'Usuário sem CNPJ';
    142: Result := 'Usuário sem IM';
    143: Result := 'Nova configuração igual a atual';
    146: Result := 'Documento especificado não foi encontrado';
    149: Result := 'Século inválido';
    150: Result := 'Usuário já programado';
    151: Result := 'Pagamento já programado';
    152: Result := 'Totalizador Não-Fiscal já programado';
    153: Result := 'RG já programado';
    155: Result := 'Atingiu limite de totalizadores de cada tipo';
    157: Result := 'MFD em falha';
    158: Result := 'Razão Social excede 40 caracteres';
    159: Result := 'Nome Fantasia excede 40 caracteres';
    160: Result := 'Endereço excede 120 caracteres';
    166: Result := 'Efetivado igual ao estornado';
    167: Result := 'Este símbolo da moeda já está programado';
    176: Result := 'Informação programada é a mesma da atual';
    177: Result := 'Utilize índice de 1 a 5';
    178: Result := 'Tamanho da imagem inválido';
    179: Result := 'Última data gravada a mais de 1 mês';
    180: Result := 'Atingido o limite de documentos autorizados';
    184: Result := 'Opção deve ser T ou A';
    185: Result := 'Unidade de medida não encontrada';
    194: Result := 'Seleção inválida';
    196: Result := 'Chave não carregada';
    197: Result := 'Chave inválida';
    198: Result := 'IE inválida';
    199: Result := 'IM inválida';
    200: Result := 'Não é possível alterar dados';
    201: Result := 'CPF ou CPNJ inválido';
    202: Result := 'Capacidade de programação esgotada';
    203: Result := 'Validade do comando expirada';
    204: Result := 'Comando não pode ser executado';
    205: Result := 'Comando não permitido';
    206: Result := 'Inibido por comando';
  else
    Result := 'Erro não catalogado!';
  end;
end;

function DarumaTraduzirTag(const ATag: AnsiString): AnsiString;
const
  C_ON  = #1;
  C_OFF = #0;

  // <e></e>
  cExpandidoOn   = ESC + 'W' + C_ON;
  cExpandidoOff  = ESC + 'W' + C_OFF;

  // <n></n>
  cNegritoOn     = ESC + 'G' + C_ON;
  cNegritoOff    = ESC + 'G' + C_OFF;

  // <s></s>
  cSublinhadoOn  = ESC + '-' + C_ON;
  cSublinhadoOff = ESC + '-' + C_OFF;

  // <c></c>
  cCondensadoOn  = ESC + SI;
  cCondensadoOff = DC2;

  //<i></i>
  cItalicoOn  = '';
  cITalicoOff = '';
begin

  if ATag = cTagLigaExpandido then
    Result := cExpandidoOn
  else if ATag = cTagDesligaExpandido then
    Result := cExpandidoOff
  else if ATag = cTagLigaNegrito then
    Result := cNegritoOn
  else if ATag = cTagDesligaNegrito then
    Result := cNegritoOff
  else if ATag = cTagLigaSublinhado then
    Result := cSublinhadoOn
  else if ATag = cTagDesligaSublinhado then
    Result := cSublinhadoOff
  else if ATag = cTagLigaCondensado then
    Result := cCondensadoOn
  else if ATag = cTagDesligaCondensado then
    Result := cCondensadoOff
  else if ATag = cTagLigaItalico then
    Result := cItalicoOn
  else if ATag = cTagDesligaItalico then
    Result := cITalicoOff
  else
     Result := '' ;
end;

function DarumaTraduzirTagBloco(const ATag, Conteudo: AnsiString;
  AECFClass: TACBrECFClass): AnsiString;
const
  // bAABCCDDEEEEEEEEEEEEE..EE
  // --------
  // ESC b = Comando para impressão das barras
  // A = Tipo de codigo FIXO
  // B = Largura 1..5
  // C = Altura 50..200
  // D = imprimir ou não codigo abaixo da barra
  //     00 - Não imprime
  //     01 - Imprime
  // E = Codigo de barra
  // termina o comando com null
  cEAN13    = 01; // <ean13></ean13>
  cEAN8     = 02; // <ean8></ean8>
  cSTD25    = 03; // <std></std>
  cINTER25  = 04; // <inter></inter>
  cCODE128  = 05; // <code128></code128>
  cCODE39   = 06; // <code39></code39>
  cCODE93   = 07; // <code93></code93>
  cUPCA     = 08; // <upca></upca>
  cCODABAR  = 09; // <codabar></codabar>
  cMSI      = 10; // <msi></msi>
  cCODE11   = 11; // <code11></code11>
var
  IsEscECF: Boolean;

  function MontaCodBarras(const ATipo: Byte; ACodigo: AnsiString): AnsiString;
  var
    Largura, Altura, Mostrar: Byte;
  begin
    with AECFClass do
    begin
      Largura := max( min( ConfigBarras.LarguraLinha, 5), 2);
      if ConfigBarras.Altura = 0 then
        Altura := ifthen(IsEscECF, 50, 5)
      else
        Altura  := max( min( ConfigBarras.Altura, 99), 5);

      Mostrar := IfThen(ConfigBarras.MostrarCodigo, 1, 0);
    end;

    if IsEscECF then
      Result := ESC + 'b' + AnsiChr(ATipo) + AnsiChr(Largura) + AnsiChr(Altura) +
                            AnsiChr(Mostrar) + ACodigo + NUL + SYN + DC2
    else
      Result := ESC + 'b' + IntToStrZero(ATipo,2)  + IntToStr(Largura) +
                            IntToStrZero(Altura,2) + IntToStrZero(Mostrar,2) +
                            ACodigo + NUL;
  end;

begin
  IsEscECF := (AECFClass is TACBrECFEscECF);

  if ATag = cTagBarraEAN8 then
    Result := MontaCodBarras(cEAN8, Conteudo)
  else if ATag = cTagBarraEAN13 then
    Result := MontaCodBarras(cEAN13, Conteudo)
  else if ATag = cTagBarraStd then
    Result := MontaCodBarras(cSTD25, Conteudo)
  else if ATag = cTagBarraInter then
    Result := MontaCodBarras(cINTER25, Conteudo)
  else if ATag = cTagBarraCode11 then
    Result := MontaCodBarras(cCODE11, Conteudo)
  else if ATag = cTagBarraCode39 then
    Result := MontaCodBarras(cCODE39, Conteudo)
  else if ATag = cTagBarraCode93 then
    Result := MontaCodBarras(cCODE93, Conteudo)
  else if ATag = cTagBarraCode128 then
    Result := MontaCodBarras(cCODE128, Conteudo)
  else if ATag = cTagBarraUPCA then
    Result := MontaCodBarras(cUPCA, Conteudo)
  else if ATag = cTagBarraCodaBar then
    Result := MontaCodBarras(cCODABAR, Conteudo)
  else if ATag = cTagBarraMSI then
    Result := MontaCodBarras(cMSI, Conteudo)
  else
     Result := Conteudo;
end;


constructor TACBrECFDaruma.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.HandShake := hsRTS_CTS ;
  { Variaveis internas dessa classe }
  fsEmPagamento := false ;
  fsNumVersao   := '' ;
  fsNumECF      := '' ;
  fsNumLoja     := '' ;
  fsUsuarioAtual:= '' ;
  fsRet244      := '' ;
  fsNumCRO      := '' ;
  fsnumcupom    := '' ;
  fsNumCCF      := '' ;
  fsNumUltimoItem := 0;
  fsSubTotal    := 0 ;
  fsArredonda   := ' ';
  fsCNFVinc     := nil ;
  fsTipoRel     := ' ' ;
  fsEsperaFFCR  := False ;
  fpIdentificaConsumidorRodape := True ;
  ZeraTotalApagar;

  fpModeloStr := 'Daruma' ;
  fsSubModeloECF := '';
  fpRFDID     := 'DR' ;
  fpPaginaDeCodigo := 28591 ;

  { Criando Lista de String com comandos de Impressao a Remover de Leituras }
  fsComandosImpressao[0]  := #0 ;
  fsComandosImpressao[1]  := #3 ;
  fsComandosImpressao[2]  := #14 ;
  fsComandosImpressao[3]  := #15 ;
  fsComandosImpressao[4]  := #18 ;
  fsComandosImpressao[5]  := #20 ;
  fsComandosImpressao[6]  := #22 ;
  fsComandosImpressao[7]  := #27+#14 ;
  fsComandosImpressao[8]  := #27+#15 ;
  fsComandosImpressao[9]  := #27+'W0' ;
  fsComandosImpressao[10] := #27+'W1' ;
  fsComandosImpressao[11] := #27+'G0' ;
  fsComandosImpressao[12] := #27+'G1' ;
  fsComandosImpressao[13] := #27+'E'+#0 ;
  fsComandosImpressao[14] := #27+'E'+#1 ;
  fsComandosImpressao[15] := #27+'-'+#0 ;
  fsComandosImpressao[16] := #27+'-'+#1 ;
  fsComandosImpressao[17] := #27+'.'+#0 ;
  fsComandosImpressao[18] := #27+'.'+#1 ;
  fsComandosImpressao[19] := #27+'*'+#0 ;
  fsComandosImpressao[20] := #27+'*'+#1 ;
  fsComandosImpressao[21] := #27+'@'+#24 ;
  fsComandosImpressao[22] := #27+'@' ;
end;

destructor TACBrECFDaruma.Destroy;
begin
  if Assigned( fsCNFVinc ) then
     fsCNFVinc.Free ;

  inherited Destroy ;
end;

function TACBrECFDaruma.ModoPreVendaAtivado: Boolean;
begin
  Result := (ModoPreVenda) and (fsModeloDaruma >= fsMACH1);
end;

procedure TACBrECFDaruma.ZeraTotalApagar;
begin
  fsTotalAPagar := -987654 ;
  fsEmPagamento := false ;
  fsRet244      := '' ;
end ;

procedure TACBrECFDaruma.VerificarBmpTexto(var IndiceBMP: Integer;
  const ATexto: String);
begin
  if IndiceBMP > 5 then
    raise EACBrECFERRO.Create( ACBrStr('Indice do bitmap deve ser um valor entre 1 e 5, ou 0 para nenhum.') );

  // Se possui código de barras e Bitmap no texto remover o bitmap,
  // porque a Daruma imprime um em cima do outro
  if TACBrECF(fpOwner).ECF.PossuiTagCodBarra(ATexto) and (IndiceBMP > 0) then
    IndiceBMP := 0;
end;

procedure TACBrECFDaruma.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+#10+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

//  fpDevice.HandShake := hsRTS_CTS ;
  inherited Ativar ; { Abre porta serial }

  fsNumVersao := '' ;
  fsNumECF    := '' ;
  fsNumLoja   := '' ;
  fsUsuarioAtual:= '' ;
  fsRet244    := '' ;
  fsNumCRO    := ''  ;
  fsArredonda := ' ' ;
  fsnumcupom  := '' ;
  fsNumCCF    := '' ;
  fsSubTotal  := 0 ;
  fsNumUltimoItem := 0;

  fpMFD       := False ;
  fpTermica   := False ;
  fsTipoRel   := ' ' ;
  fsEsperaFFCR:= False ;
  fsModeloDaruma := fsIndefinido;
  fsSubModeloECF := '';

  try
     { Testando a comunicaçao com a porta }
     Estado;

     { Testando se é MFD }
     if NumVersao = '' then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));

     // Função para comutar as impressoras termicas para OnLine
     ComutaOnLine;
  except
     Desativar ;
     raise ;
  end ;
end;

function TACBrECFDaruma.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
 Var Tentativas : Integer ;
begin
  {   Nas Darumas com MFD, em algumas situações o ECF pode ficar temporariamente
    inoperantente, enquanto a compactação da MFD está sendo efetuada. Nessa
    situação, o ECF retorna os seguintes códigos de erro:
     35 - Relogio Inoperante ou 99 (não documentado)

      Segundo o Suporte Técnico da Daruma, quando este problema ocorre, devemos
    aguardar até que ele consiga responder corretamente.

      Esta rotina irá tentar enviar o comando por 10 vezes. Caso ela recebe os
    erros 35 ou 99... ele aguarda 100 milisegundos e tenta um novo envio...   }

  Tentativas := 0 ;
  while (Tentativas < 10) do  // Tenta enviar o comando por 10 vezes
  begin
     try
        Result := EnviaComando_ECF_Daruma( cmd ) ; // Envia o comando
        Break ;                                    // Tudo OK, saindo..
     except
        GravaLog('Daruma: Falha no Envio do CMD. Tentativa: '+IntToStr(Tentativas+1)+
                 ' - Erro: '+IntToStr(fsErro)+' - Estendido: '+IntToStr(fsErroSTD) +
                 ' -> ' + ErroEstendidoTexto(fsErro) +
                 ' Cod.Aviso: '+IntToStr(fsCodAviso) );
        if (fsCodAviso = 40) or (fsErro = 35) or (fsErro = 99) then  // Está compactando MFD ?
         begin
           GravaLog('        Tentando novamente');
           Sleep(100) ;
         end
        else
           raise ;
     end ;

     Inc( Tentativas ) ;
  end ;
end ;

function TACBrECFDaruma.EnviaComando_ECF_Daruma(cmd : AnsiString) : AnsiString ;
Var
  ErroMsg : String ;
  PoucoPapel : Boolean ;
begin
  result  := '' ;
  ErroMsg := '' ;
  fsErro    := 0 ;
  fsErroSTD := 0 ;
  fsCodAviso:= 0 ;
  fpComandoEnviado   := '' ;
  fpRespostaComando  := '' ;
  PoucoPapel         := False;

  try
     { Codificando CMD de acordo com o protocolo da Daruma }
     cmd := PreparaCmd( cmd ) ;

     fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }

     while fpComandoEnviado = '' do
     begin
        fpDevice.Serial.Purge ;                   { Limpa a Porta }

        if not TransmiteComando( cmd ) then
           continue ;

        fpComandoEnviado := cmd ;
     end ;

//   sleep(20) ;

     { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
       falha na leitura LeResposta dispara Exceçao }
     LeResposta ;
     
     try
        fpDevice.Serial.Purge ;  { Limpa buffer de Entrada e Saida }
     except
     end ;

     { Retorno pode ter várias respostas... capturando apenas a última }
     Result := LimpaRetorno(fpRespostaComando) ;
     fpRespostaComando := Result ;

     { Verificando por erros }
     if (copy(Result,1,2) = ':E') then
        fsErro := StrToIntDef(copy(Result,3,2),0) ;

     if fsNumVersao = '2000' then
        if (copy(result,3,2) <>  '00') then
           fsErro := StrToIntDef(copy(result,3,2),0);
           
     if (Length(fpComandoEnviado) > 2) and
        (fpComandoEnviado[1] = FS)     and
        (fpComandoEnviado[2] <> 'R') then
     begin
        fsErro    := StrToIntDef(copy(Result,2,2),0) ;
        fsErroSTD := StrToIntDef(copy(Result,4,3),0) ;
        fsCodAviso:= StrToIntDef(copy(Result,7,2),0) ;
     end ;
        
     //if (copy(result,1,2) = ':'+#200 ) then
     //   fsNumCupom := Trim(copy(result,6,6))
     //else if (copy(result,1,2) <> ':B') then
     //   fsNumCupom := '';

     if fsNumVersao = '2000' then
        if fsErro = 8 then
           fsErro := 0;

     if fsErro <> 0 then
     begin
        case fsErro of
           0 : ErroMsg := 'ECF em modo de Intervenção Técnica' ;
           1 : ErroMsg := 'Comando disponível somente em modo de Intervenção Técnica' ;
           2 : ErroMsg := 'Erro gravando memória fiscal' ;
           3 : ErroMsg := 'Memória fiscal esgotada' ;
           4 : ErroMsg := 'Erro no relógio interno' ;
           5 : ErroMsg := 'Falha mecânica' ;
           6 : ErroMsg := 'Erro lendo memória fiscal' ;
          10 : ErroMsg := 'Documento sendo emitido' ;
          11 : ErroMsg := 'Documento não foi aberto' ;
          12 : ErroMsg := 'Não existe documento a cancelar' ;
          13 : ErroMsg := 'Erro nos parâmetros: Não numérico' ;
          14 : ErroMsg := 'Não há memória disponível para esta operação' ;
          15 : ErroMsg := 'Item a cancelar não encontrado' ;
          16 : ErroMsg := 'Erro de sintaxe no comando' ;
          17 : ErroMsg := 'Numeric overflow' ;
          18 : ErroMsg := 'Totalizador Tributário selecionado não possui '+
                          'aliquota definida' ;
          19 : ErroMsg := 'Memória fiscal vazia' ;
          21 : ErroMsg := 'Detectado proximidade do final da bobina de papel' ;
          22 : ErroMsg := 'Redução Z já foi emitida. ECF Bloqueado ate 00:00' ;
          23 : ErroMsg := 'Redução Z do dia anterior ainda pendente '+
                          'Efetue uma Redução Z.' ;
          24 : ErroMsg := 'Valor de desconto ou acrescimo inválido' ;
          25 : ErroMsg := 'Caracter inválido nos Parâmetros' ;
          30 : ErroMsg := 'Comprovante NÃO Fiscal inválido ou não programado' ;
          38 : ErroMsg := 'Forma de pagamento selecionada não é‚ válida' ;
          39 : ErroMsg := 'Erro na sequência de fechamento do cupom fiscal' ;
          41 : ErroMsg := 'Data inválida. Data fornecida é inferior à última gravada na Memória Fiscal' ;
          42 : ErroMsg := 'Leitura X inicial ainda não foi emitida' ;
          43 : ErroMsg := 'Não pode mais emitir CNF Vinculado solicitado';
          50 : ErroMsg := 'Sem Papel' ;
          61 : ErroMsg := 'Queda de energia durante a emissão do Cupom Fiscal' ;
          84 : ErroMsg := 'Impressora não está respondendo' ;
        else
           ErroMsg := 'Erro retornado pelo ECF: '+IntToStrZero(fsErro,2) ;
        end ;
     end ;

     if fsErro = 21 then       { Verifica se possui erro "Pouco Papel" }
     begin
        PoucoPapel := True;
        ErroMsg    := '' ;   { Apaga Msg de Erro para nao gerar Exceção }
     end ;

     if ( TestBit(fsCodAviso,0) ) and (ErroMsg = '') then  { Verifica se possui Aviso de "Pouco Papel" }
        PoucoPapel := True;

     if PoucoPapel then
        DoOnMsgPoucoPapel ;

     if ErroMsg <> '' then
      begin
        ErroMsg := 'Erro retornado pela Impressora: '+fpModeloStr + sLineBreak +
                   sLineBreak + ErroMsg ;
        if fsErroSTD <> 0 then
           ErroMsg := ErroMsg + sLineBreak + 'Erro estendido ('+
           IntToStrZero(fsErroSTD, 3) + ' -> ' + ErroEstendidoTexto(fsErroSTD)+ ')';

        if (fsErro = 50) or (fsErroSTD = 72) then
           DoOnErrorSemPapel
        else
           raise EACBrECFSemResposta.create(ACBrStr(ErroMsg)) ;
      end
     else
        Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }

  finally
     fsEsperaFFCR := False ;
  end ;

end;

function TACBrECFDaruma.ErroEstendidoTexto(AErro: integer): string;
begin
  Result := 'Erro não documentado';
  if (AErro > 0) and (AErro < 302) then
    Result := ErrosEstendidos[AErro];
end;

procedure TACBrECFDaruma.PafMF_GerarCAT52(const DataInicial,
  DataFinal: TDateTime; const DirArquivos: String; NumeroSerie: String);
begin
  Self.ArquivoMFD_DLL(DataInicial, DataFinal, DirArquivos, [docTodos], finNFPTDM);
end;

function TACBrECFDaruma.PreparaCmd(const cmd : AnsiString) : AnsiString ;
Var I, chksum, LenCmd : Integer ;
begin
{ Recomendações da Daruma:
  - Você deve escrever na porta serial o Checksum mesmo que ele seja Nulo
  - E Mesmo após a escrita do Checksum sendo nulo ou não, qualquer que seja o
    Checksum, envie mais um Nulo para a porta serial, porque caso o Checksum
    seja ESC, FS, GS que é o começo de um comando então o envio do Nulo quebra
    a seqüência e faz com que o ECF entenda que aquilo é um Checksum e não o
    começo de um comando. }

  result := '' ;
  if cmd = '' then exit ;

  // É comando da FS600 ? //
  if (cmd[1] = FS) or (copy(cmd,1,2) = GS+ACK) then
   begin
     chksum := 0 ;
     LenCmd := Length( cmd ) ;

     For I := 1 to LenCmd do   { Aplicando XOR nos Bytes do comando }
        chksum := chksum xor ord( cmd[ I ] ) ;

     Result := cmd + AnsiChar( chr( chksum ) );

     if (cmd[1] = FS) and (Result[Length(Result)] in [ESC,FS,GS]) then
        Result := Result + #0 ;
   end
  else
     Result := cmd + CR ; { Adcionando Sufixo padrao }
end ;

function TACBrECFDaruma.LimpaRetorno(const Retorno: AnsiString): AnsiString;
Var P1, P2 : Integer ;
begin
  Result := '' ;
  P1     := Pos(':',Retorno) ;
  if P1 > 0 then
  begin
     // Tratando retorno com vários sinais de ":" no inicio
     while (P1 < Length(Retorno)) and ( Retorno[P1+1] = ':') do
       Inc( P1 ) ;

     P2 := LastDelimiter(CR,Retorno) ;
     if P2 > P1 then
        Result := copy( Retorno, P1, Length( Retorno ) ) ;
  end ;
end;


function TACBrECFDaruma.VerificaFimLeitura(var Retorno : AnsiString ;
  var TempoLimite : TDateTime) : Boolean ;
 Var LenRet : Integer ;
     EndStr : AnsiString ;
begin
  LenRet := Length(Retorno) ;
  Result := (fpDevice.Serial.WaitingDataEx <= 0) and (LenRet > 0) ;

  if Result then
  begin
     if not fsEsperaFFCR then
        Result := (LimpaRetorno(Retorno) <> '') 
     else
      begin
        // Na Leitura da Memoria Fiscal, aguarda até chegar cDELIMTADOR+CR
        EndStr := RightStr(Retorno,5) ;
        Result := (pos( cDELIMITADOR+CR, EndStr ) > 0) ;
      end ;
  end ;

  { Nota sobre o VerificaFimLeitura: A DARUMA responde muito antes da
    Impressao terminar, o que pode causar problemas com comandos enviados logo
    após impressoes demoradas como a Leitura X (por exemplo). Para esses casos,
    é necessário ativar a propriedade "AguardaImpressao := True" }
end;


function TACBrECFDaruma.VerificaFimImpressao(var TempoLimite: TDateTime): Boolean;
Var Cmd , RetCmd : AnsiString ;
    I : Integer ;
    DT : TDateTime ;
begin
  { Essa função só é chamada se AguardaImpressao = True,
    Como essa função é executada dentro da "LeResposta", que por sua vez foi
    chamada por "EnviaComando", não podemos usar o método "EnviaComando" (ou
    teriamos uma chamada recursiva infinita), por isso o Loop abaixo envia o
    comando "Palavra Status" diretamente para a Serial, e aguarda por .5 segundo
    a resposta... Se a Daruma consegir responder, verifica se o Bit 0 de S8,
    está desligado, o que significa que a Impressão Terminou }
  Result := false ;
  DT     := 0 ;
  if not EmLinha() then
   begin
     Sleep(100) ;
//   GravaLog(DateTimeToStr(now)+ ' - Not EmLinha');
   end
  else
   begin
     RetCmd := '' ;
     I      := 0 ;
//   GravaLog(DateTimeToStr(now)+ ' - Solicitando Status');
     if fsNumVersao = '2000' then
        Cmd := PreparaCmd( GS + ENQ )    { Palavra de Status }
     else if fpMFD then
        Cmd := PreparaCmd( GS + ACK )
     else
        Cmd := PreparaCmd( GS + cDELIMITADOR ) ;   { Palavra de Status }

     try
        Sleep(100) ;
        fpDevice.Serial.Purge ;  { Limpa buffer de Entrada e Saida }
        fpDevice.EnviaString( Cmd );
     except
     end ;

     repeat
        try
           RetCmd := RetCmd + fpDevice.Serial.RecvPacket(200) ;
        except
           if fpDevice.Serial.WaitingDataEx <= 0 then
           begin
              Inc( I ) ;   // Nao achou dados para ler, incrementa num falhas
              sleep(50) ;  // Aguarda um pouco, para ECF enviar os dados
//            GravaLog(DateTimeToStr(now)+ ' - Erro '+IntToStr(I)+': '+RetCmd, True);
           end ;
        end ;
     until (I > 5) or ( VerificaFimLeitura( RetCmd, DT ) )  ;

     RetCmd := LimpaRetorno( RetCmd ) ;
     Result := (RetCmd <> '') ;

     try
        fpDevice.Serial.Purge ;  { Limpa buffer de Entrada e Saida }
     except
     end ;

//   GravaLog(DateTimeToStr(now)+ ' - VerificaFimImpressao: '+RetCmd, True);
     if Result then
     begin
        // ECF Respondeu corretamente, portanto está trabalhando //
        TempoLimite := IncSecond(now, TimeOut);

        try
           if fsNumVersao = '2000' then
              Result := TestBit(StrToInt('$'+RetCmd[6]),0)        { Bit0 - S5 }
           else if fpMFD then
              Result := not TestBit(StrToInt('$'+RetCmd[7]),0)    { Bit0 - S6 }
           else
              Result := not TestBit(StrToInt('$'+RetCmd[9]),0) ;  { Bit0 - S8 }
        except
           Result := False ;
        end ;
//      GravaLog(DateTimeToStr(now)+ ' - VerificaFimImpressao '+IfThen(Result,'TRUE','FALSE')+' : '+RetCmd, True);
     end ;
   end ;
end;


function TACBrECFDaruma.GetDataHora: TDateTime;
Var
  RetCmd: AnsiString;
  PrefixAno: String;
  dia, mes, ano, hora, minuto, segundo: Word;
begin
  if fpMFD then
  begin
    RetCmd  :=  RetornaInfoECF('66');
    { Retorna a data/hora no formato: ddmmaaaahhnnss }

    dia     := StrToInt(copy(RetCmd, 1,2));
    mes     := StrToInt(copy(RetCmd, 3,2));
    ano     := StrToInt(copy(RetCmd, 5,4));
    hora    := StrToInt(copy(RetCmd, 9,2));
    minuto  := StrToInt(copy(RetCmd,11,2));
    segundo := StrToInt(copy(RetCmd,13,2));

    Result := EncodeDateTime(ano, mes, dia, hora, minuto, segundo, 0);
  end
  else
  begin
    RetCmd := EnviaComando( ESC + #230 ) ;
    // MFD, 2000 -> :[230]EEWWddmmaahhMMss[CR]
    // fs345     -> :TddmmaahhMMss[CR]
    RetCmd := copy(RetCmd, Length(RetCmd)-12, 12) ;  {Pega apenas a Data/Hora}

    // pega o inicio do ano atual para complementar o que vem da impressora
    PrefixAno := FormatDateTime('yyyy', DATE);
    PrefixAno := Copy(PrefixAno, 1, 2);

    dia     := StrToInt(copy(RetCmd, 1,2));
    mes     := StrToInt(copy(RetCmd, 3,2));
    ano     := StrToInt(PrefixAno + copy(RetCmd, 5,2));
    hora    := StrToInt(copy(RetCmd, 7,2));
    minuto  := StrToInt(copy(RetCmd, 9,2));
    segundo := StrToInt(copy(RetCmd,11,2));

    Result := EncodeDateTime(ano, mes, dia, hora, minuto, segundo, 0);
  end;
end;

function TACBrECFDaruma.GetNumCupom: String;
Var RetCmd : AnsiString ;
    Num : Integer ;
begin
  Result := '' ;

  if fpMFD then
  begin
    if fsNumCupom <> '' then
      Result := Trim(fsNumCupom)
    else
    begin
      RetCmd :=  RetornaInfoECF('26');
      Result := RetCmd;
    end;
  end
  else if fsNumVersao='2000' then
  begin
    if fsNumCupom <> '' then
      Result := fsNumCupom
    else
    begin
      RetCmd := EnviaComando( ESC + #235 ) ;

      if LeftStr(RetCmd, 1) = ':' then
      begin
        Num    := StrToIntDef(copy(RetCmd,8,6),0) ;
        Result := IntToStrZero(Num,6) ;
      end ;
    end ;
  end
  else
  begin
    RetCmd := EnviaComando( ESC + #239 ) ;

    if LeftStr(RetCmd, 3) = ':' + ESC + #239 then
    begin
      Num := StrToIntDef(copy(RetCmd,9,6),0) ;

      if copy(RetCmd,8,1) = '2' then  { Nao ha cupom aberto, retorna o proximo }
        Num := Num - 1 ;

        Result := IntToStrZero(Num,6) ;
    end ;
  end ;
end;

function TACBrECFDaruma.GetNumCCF: String;
begin
  Result := '' ;
  if fpMFD then
  begin
    if fsNumCCF <> '' then
      Result := fsNumCCF
    else
      Result :=  RetornaInfoECF('30');
  end
  else
    Result := GetNumCupom;
end;

function TACBrECFDaruma.GetNumReducoesZRestantes: String;
begin
  Result := '' ;
  if fpMFD then
    Result :=  RetornaInfoECF('25');
end;

function TACBrECFDaruma.GetNumGNF: String;
 Var RetCmd : AnsiString ;
begin
  Result := '' ;

  if fpMFD then
    Result  :=  RetornaInfoECF('28')
  else if fsNumVersao='2000' then
  begin
    RetCmd := EnviaComando( ESC + #237 ) ;
    if LeftStr(RetCmd, 1) = ':' then
      Result := copy(RetCmd,19,6) ;
  end
  else
     Result := copy(Ret244,16,6) ;
end;

function TACBrECFDaruma.GetNumGRG: String;
begin
  Result := '0' ;
  if fpMFD then
    Result  :=  RetornaInfoECF('33');
end;

function TACBrECFDaruma.GetNumCDC: String;
begin
  Result := '0' ;
  if fpMFD then
    Result  :=  RetornaInfoECF('45');
end;

function TACBrECFDaruma.GetNumCFC: String;
begin
  Result := '0' ;
  if fpMFD then
    Result  :=  RetornaInfoECF('39');
end;

function TACBrECFDaruma.GetNumECF: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumECF) = '' then
  begin
    if fpMFD then
      fsNumECF  :=  RetornaInfoECF('107')
    else if fsNumVersao='2000' then
    begin
      RetCmd  := EnviaComando( ESC + #233);
      fsNumECF:=copy(RetCmd,29,4);
    end
    else
    begin
      RetCmd := EnviaComando( ESC + #239 ) ;
      if LeftStr(RetCmd, 3) = ':' + ESC + #239 then
        fsNumECF := copy(RetCmd,4,4) ;
    end;
  end ;

  Result := fsNumECF ;
end;

function TACBrECFDaruma.GetNumLoja: String;
begin
  if Trim(fsNumLoja) = '' then
  begin
    if fpMFD then
       fsNumLoja := RetornaInfoECF('129')
    else
       fsNumLoja := inherited GetNumLoja ;
  end ;

  Result := fsNumLoja ;
end;

function TACBrECFDaruma.GetNumCRO: String;
Var RetCmd : AnsiString ;
begin
  if fpMFD then
    fsNumCRO := RetornaInfoECF('23')
  else if fsNumVersao='2000' then
  begin
    if Trim(fsNumCRO) = '' then
    begin
      RetCmd := EnviaComando( ESC + #237 ) ;
      if LeftStr(RetCmd, 1) = ':' then
        fsNumCRO := copy(RetCmd,49,6) ;
    end ;
  end
  else
     fsNumCRO := copy(Ret244,38,4) ;

  Result := fsNumCRO ;
end;

function TACBrECFDaruma.GetNumSerie: String;
Var RetCmd : AnsiString ;
    Tam : Integer ;
begin
  Result := '' ;
  Tam    := 8 ;

  if fpMFD then
    Result := RetornaInfoECF('78')
  else if fsNumVersao='2000' then
  begin
    RetCmd := EnviaComando( ESC + #233);
    Result := copy(RetCmd,17,Tam);
  end
  else
  begin
    If StrToIntDef(NumVersao,0) < 345 then
      Tam := 6 ;

    RetCmd := EnviaComando(ESC + #236) ;
    if LeftStr(RetCmd, 2) = ':V' then
      Result := copy(RetCmd,3,Tam) ;
  end;
end;

function TACBrECFDaruma.GetNumSerieMFD : String ;
begin
  Result := '';
  if fpMFD then
    Result := RetornaInfoECF('77');
end ;

function TACBrECFDaruma.GetNumVersao: String ;
Var RetCmd    : AnsiString ;
    wRetentar : Boolean ;
    wTimeOut  : Integer ;
begin
  if fsNumVersao = '' then
  begin
    try
      wRetentar := Retentar ;
      wTimeOut  := TimeOut ;

      try
        TimeOut  := 1 ;
        Retentar := false ;

        RetCmd := EnviaComando(FS + 'R' + #200 + '082') ;
        RetCmd := copy(RetCmd,6,6) ;
        fsSubModeloECF := '' ;

        if RetCmd = '010053' then
        begin
          fsModeloDaruma := fs600 ;
          fsSubModeloECF := 'FS-600' ;
        end
        else if RetCmd = '010054' then
        begin
          fsModeloDaruma := fs2100T ;
          fsSubModeloECF := 'FS-2100T' ;
        end
        else if RetCmd = '010058' then
        begin
          fsModeloDaruma := fs600USB ;
          fsSubModeloECF := 'FS-600 USB' ;
        end
        else if RetCmd = '010059' then
        begin
          fsModeloDaruma := fs700L ;
          fsSubModeloECF := 'FS-700L' ;
        end
        else if RetCmd = '010060' then
        begin
          fsModeloDaruma := fs700H ;
          fsSubModeloECF := 'FS-700H' ;
        end
        else if RetCmd = '010061' then
        begin
          fsModeloDaruma := fs700M ;
          fsSubModeloECF := 'FS-700M' ;
        end
        else if RetCmd = '010063' then
        begin
          fsModeloDaruma := fsMACH1 ;
          fsSubModeloECF := 'MACH1' ;
        end
        else if RetCmd = '010064' then
        begin
          fsModeloDaruma := fsMACH2 ;
          fsSubModeloECF := 'MACH2' ;
        end
        else if RetCmd = '010062' then
        begin
          fsModeloDaruma := fsMACH3;
          fsSubModeloECF := 'MACH3' ;
        end ;

        fpMFD       := True ;
        fpTermica   := True ;
      finally
        Retentar := wRetentar ;
        TimeOut  := wTimeOut ;
      end ;
    except
      fpMFD     := False ;
      fpTermica := False ;
      wTimeOut  := TimeOut ;
      try
        TimeOut := 1 ;
        RetCmd  := copy(EnviaComando(ESC + #195), 1, 6) ;
      finally
        TimeOut := wTimeOut;
      end ;

      if RetCmd = ':10043' then
      begin
        fsNumVersao    := '345';
        fsModeloDaruma := fs345;
        fsSubModeloECF := 'FS-345'
      end
      else if RetCmd = ':10033' then
      begin
        fsNumVersao    := '315';
        fsModeloDaruma := fs315;
        fsSubModeloECF := 'FS-315'
      end
      else if retcmd=':10004' then
      begin
        fsNumVersao    := '2000';
        fsModeloDaruma := fs2000;
        fsSubModeloECF := 'FS-2000'
      end
      else
      begin
        fsNumVersao    := copy(RetCmd,2,length(RetCmd)-2) ;
        fsModeloDaruma := fsIndefinido;
        fsSubModeloECF := '' ;
      end;
    end ;

    if fpMFD then
    begin
      RetCmd :=  RetornaInfoECF('83') ;
      fsNumVersao := RetCmd ;

      fpDecimaisQtd   := 3 ;
      fpDecimaisPreco := 2 ;

      RetCmd :=  RetornaInfoECF('139') ;

      fpDecimaisQtd   := StrToIntDef(copy(RetCmd,1,1),fpDecimaisQtd) ;
      fpDecimaisPreco := StrToIntDef(copy(RetCmd,2,1),fpDecimaisPreco) ;
    end ;
  end ;

  Result := fsNumVersao ;
end;

function TACBrECFDaruma.GetTotalPago: Double;
Var RetCmd : AnsiString ;
begin
  if fpMFD then
   begin
     RetCmd :=  RetornaInfoECF('48') ;
     Result := RoundTo( StrToFloatDef(RetCmd,0) / 100,-2) ;
   end
  else
   begin
     if fsTotalAPagar = -987654 then
        Result := 0
     else
        Result := Subtotal - fsTotalAPagar ;
   end ;
end;

function TACBrECFDaruma.GetSubTotal: Double;
var
  RetCmd : AnsiString ;
begin
  if fpMFD then
  begin
    if fsSubTotal > 0 then
      RetCmd := FloatToStr(fsSubTotal * 100)
    else
      RetCmd := RetornaInfoECF('47');
  end
  else
  if fsNumVersao = '2000' then
  begin
    RetCmd := EnviaComando(ESC + #235);
    if LeftStr(RetCmd, 1) <> ':' then
      RetCmd := '0'
    else
      RetCmd := copy(RetCmd,29,12) ;
  end
  else
  begin
    RetCmd := EnviaComando(ESC + #239) ;

    if LeftStr(RetCmd, 3) <> ':' + ESC + #239 then
      RetCmd := '0'
    else
      if StrToIntDef(NumVersao,0) >= 345 then
        RetCmd := copy(RetCmd,29,14)
      else
        RetCmd := copy(RetCmd,31,21) ;
  end;

  Result := RoundTo( StrToFloatDef(RetCmd,0) / 100,-2) ;
end;

{  Ordem de Retorno do Estado da Impressora
   estNaoInicializada - Não Inicializada (Nova)
   estDesconhecido    - Desconhecido
   estPagamento       - Cupom Venda Aberto em Pagamento
   estVenda           - Cupom Venda Aberto em Itens
   estNaoFiscal       - Cupom Não Fiscal Aberto
   estRelatorio       - Cupom Vinculado Aberto | Relatório Gerencial Aberto
   estBloqueada       - Impressora Bloqueada para venda
   estRequerZ         - Requer Emissão da Redução da Z
   estRequerX         - Requer Leitura X
   estLivre           - Livre para vender
}
function TACBrECFDaruma.GetEstado: TACBrECFEstado;
Var RetCmd1,RetCmd2 : AnsiString ;
    Flag1, Flag2, Flag3, I: Byte ;
begin
  fpEstado := estNaoInicializada ;
  if (not fpAtivo) then
  begin
    Result := fpEstado ;
    Exit ;
  end;

  try
    fpEstado := estDesconhecido ;

    if fsNumVersao = '2000' then
    begin
      if fsEmPagamento then
        fpEstado := estPagamento
      else
      begin
        RetCmd2 := EnviaComando( ESC + #235 ) ;
        case RetCmd2[7] of
          '0'             : fpEstado := estLivre ;
          '1'             : fpEstado := estVenda ;
          '2','3','4','5' : fpEstado := estRelatorio ;
          '7','8','9'     : fpEstado := estPagamento ;
        end ;
      end ;

      if fpEstado = estLivre then
      begin
        RetCmd1 := EnviaComando( GS + ENQ ) ;

        if TestBit(StrToInt('$'+RetCmd1[5]),2) then
          fpEstado := estBloqueada
        else if not TestBit(StrToInt('$'+RetCmd1[5]),1) then
          fpEstado := estRequerX
      end ;
    end

    else if fpMFD then
    begin
      RetCmd2 := RetornaInfoECF('56') ;

      Flag1 := StrToIntDef(RetCmd2,0) ;
      case Flag1 of
        1 :
        begin
          RetCmd2 := RetornaInfoECF('57') ;
          Flag2   := StrToIntDef(RetCmd2,0) ;
          case Flag2 of
            1     : fpEstado := estVenda ;
            2,3,4 : fpEstado := estPagamento ;
          end ;
        end ;

        2 : fpEstado := estNaoFiscal ;
        3 : fpEstado := estRelatorio ; { CCD }
        4 : fpEstado := estRelatorio ; { RG }
      else
        begin
           fpEstado := estLivre;
           Flag3 := 255;
           I := 0;
           while (Flag3 = 255) and (I < 3) do
           begin
             RetCmd1 := EnviaComando( GS + ACK, 1);
             Flag3 := StrToIntDef('$'+RetCmd1[5], 255);
             if (Flag3 = 255) then
               Sleep(200);
             Inc(I);
           end;

           if TestBit(Flag3,3) then
             fpEstado := estBloqueada
           else if TestBit(Flag3,2) then
              fpEstado := estRequerZ ;
//  TODO: Daruma FS600 continua informando Bit Requer X mesmo apos emitir a LeituraX
//         else if not TestBit(StrToInt('$'+RetCmd1[4]),0) then
//            fpEstado := estRequerX

        end;
      end ;
    end
    else
    begin
      if fsEmPagamento then
        fpEstado := estPagamento
      else
      begin
        RetCmd2 := EnviaComando( ESC + #239 ) ;

        if (copy(RetCmd2,8,1) = '1') then
          fpEstado := estVenda
        else if pos(copy(RetCmd2,8,1),'03') > 0 then
          fpEstado := estRelatorio
        else
          fpEstado := estLivre ;
      end ;

      if fpEstado = estLivre then
      begin
        RetCmd1 := EnviaComando( GS + cDELIMITADOR ) ;

        if TestBit(StrToInt('$'+RetCmd1[5]),2) then
          fpEstado := estVenda
        else if TestBit(StrToInt('$'+RetCmd1[7]),1) then
          fpEstado := estBloqueada
        else if TestBit(StrToInt('$'+RetCmd1[3]),1) then
          fpEstado := estRequerZ
        else if not TestBit(StrToInt('$'+RetCmd1[7]),2) then
          fpEstado := estRequerX
      end;
    end ;
  finally
    Result := fpEstado ;
  end ;
end;

function TACBrECFDaruma.GetGavetaAberta: Boolean;
Var RetCmd : AnsiString ;
begin
  if fsNumVersao = '2000' then
     RetCmd := EnviaComando( GS + ENQ ) 
  else
     RetCmd := EnviaComando( GS + cDELIMITADOR ) ;
     
  Result := TestBit(StrToInt('$'+RetCmd[2]),3) ;
end;

function TACBrECFDaruma.GetPoucoPapel: Boolean;
Var RetCmd : AnsiString ;
begin
  if fsNumVersao = '2000' then
   begin
     RetCmd := EnviaComando( GS + ENQ ) ;
     Result := TestBit(StrToInt('$'+RetCmd[3]),0) or
               TestBit(StrToInt('$'+RetCmd[3]),1)  ;
   end
  else
   begin
     RetCmd := EnviaComando( GS + cDELIMITADOR ) ;
     Result := TestBit(StrToInt('$'+RetCmd[3]),0) or
               TestBit(StrToInt('$'+RetCmd[2]),0)  ;
   end
end;

function TACBrECFDaruma.GetHorarioVerao: Boolean;
Var RetCmd : AnsiString ;
begin
  if fpMFD then
  begin
    RetCmd  :=  RetornaInfoECF('108');
    Result  :=  (RetCmd = '1');
  end
  else
  begin
    RetCmd := EnviaComando( ESC + #229 ) ;
    if fsNumVersao = '2000' then
       Result := (copy(RetCmd,9,1) = '1')
    else
       Result := (copy(RetCmd,7,1) = '1') ;
  end;
end;

function TACBrECFDaruma.GetArredonda: Boolean;
Var RetCmd : AnsiString ;
begin
  if fsArredonda = ' ' then
  begin
     if fpMFD or (fsNumVersao = '2000') then
        fsArredonda := 'N'
     else
      begin
        RetCmd := EnviaComando( ESC + #229 ) ;
        if (copy(RetCmd,6,1) = '1') then
           fsArredonda := 'S'
        else
           fsArredonda := 'N' ;
     end ;
  end ;

  Result := (fsArredonda = 'S') ;
end;

function TACBrECFDaruma.GetParamDescontoISSQN : Boolean ;
var
   RetCmd : AnsiString ;
begin
  if fpMFD then
   begin
     RetCmd := RetornaInfoECF('109');
     Result := (RetCmd = '1');
   end
  else
     Result := False;
end ;

procedure TACBrECFDaruma.LeituraX ;
begin
  fsNumCupom := '';
  AguardaImpressao := True ;

  if fpMFD then
     EnviaComando(FS + 'F' + #235 + '0')
  else if fsNumVersao = '2000' then
     EnviaComando(ESC + #250, 40 )
  else
     EnviaComando(ESC + #207, 40 ) ;
  fsRet244 := '' ;
end;

procedure TACBrECFDaruma.LeituraXSerial(Linhas: TStringList);
 Var RetCmd : AnsiString ;
begin
  fsNumCupom := '';
  Linhas.Clear ;
  if fpMFD then
  begin
     fsEsperaFFCR := True ;
     RetCmd := EnviaComando(FS + 'F' + #235 + '1', 10) ;
     RetCmd := RemoveStrings( RetCmd, fsComandosImpressao ) ;
     Linhas.Text := RetCmd ;
     fsRet244 := '' ;
  end ;
end;


procedure TACBrECFDaruma.AbreGaveta ;
Var wRetentar : Boolean ;
begin
  wRetentar := Retentar ;
  Retentar  := false ;
  try
     try
        EnviaComando( ESC + 'p000') ;
     except
     { exceçao silenciosa pois AbreGaveta nao tem resposta, Sempre gera erro }
     end ;

     try
  { pede a Data somente para esperar a impressora entrar em linha. A DARUMA
   imprime uma linha na bobina (demora + ou - 4 seg), após a abertura da gaveta}
        EnviaComando( ESC + #230, 5) ;
     except
     end ;
  finally
     Retentar := wRetentar ;
  end ;
end;

procedure TACBrECFDaruma.ReducaoZ(DataHora : TDateTime) ;
begin
  fsNumCupom := '';

  if DataHora = 0 then  { Aparentemente a DataHora é obrigatória na Daruma }
     DataHora := TACBrECF(fpOwner).DataHora ;

  AguardaImpressao := True ;
  if fpMFD then
   begin
     try
        EnviaComando(FS + 'F' + #234 + FormatDateTime('ddmmyyhhnnss', DataHora), 15)
     except
        on E : Exception do
        begin
           if (pos('078',E.Message) <> 0) then   // Comando inválido para o documento atual.
            begin                                //  Ficou algum Cupom aberto ?
              CancelaCupom ;
              ReducaoZ(DataHora);
            end
           else
              raise ;
        end ;
     end ;
   end
  else if fsNumVersao = '2000' then
     EnviaComando( ESC + #252 + FormatDateTime('ddmmyyhhnnss',DataHora), 180 )
  else
     EnviaComando( ESC + #208 + FormatDateTime('ddmmyyhhnnss',DataHora), 180 ) ;
  fsRet244 := '' ;
end;

procedure TACBrECFDaruma.MudaHorarioVerao ;
begin
  MudaHorarioVerao( not HorarioVerao) ;
end;

procedure TACBrECFDaruma.MudaHorarioVerao(EHorarioVerao: Boolean);
 Var FlagVerao : String ;
begin
  If EHorarioVerao then FlagVerao := '1' else FlagVerao := '0' ;

  if ( fsModeloDaruma >= fs600) then
    EnviaComando(FS + 'C' + #200 + FlagVerao )
  else
    EnviaComando(ESC + #228 + 'XXXXX' + FlagVerao + StringOfChar('X',34) ) ;
end;

procedure TACBrECFDaruma.MudaArredondamento(Arredondar: Boolean);
 Var FlagArredondar : Char ;
begin
  If Arredondar then FlagArredondar := '1' else FlagArredondar := '0' ;
  EnviaComando( ESC + #228 + 'XXXX' + FlagArredondar + StringOfChar('X',35) ) ;
  fsArredonda := ' ' ;
end;

procedure TACBrECFDaruma.AbreCupom ;
Var StrConsumidor : String ;
begin
  fsSubTotal := 0;
  fsNumCupom := '';
  fsNumCCF   := '';
  fsNumUltimoItem := 0;

  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }
  AguardaImpressao := True ;

  if fpMFD then
  begin
    StrConsumidor := LeftStr(Consumidor.Documento,20) + cDELIMITADOR +
                     LeftStr(Consumidor.Nome,30) + cDELIMITADOR +
                     LeftStr(Consumidor.Endereco,79) + cDELIMITADOR ;

    EnviaComando( FS + 'F' + #200 + StrConsumidor ) ;
    Consumidor.Enviado := True ;

    RespostasComando.Clear;
    RespostasComando.AddField('COO', Trim(Copy(fpRespostaComando, 10, 6)));
    RespostasComando.AddField('CCF', Trim(Copy(fpRespostaComando, 16, 6)));

    fsNumCupom := RespostasComando['COO'].AsString;
    fsNumCCF   := RespostasComando['CCF'].AsString;

    if ModoPreVendaAtivado then
      EnviaComando( FS + 'C' + #226 + '1' ) ;
  end
  else
    EnviaComando(ESC + #200, 8) ;

  ZeraTotalApagar;
end;

procedure TACBrECFDaruma.CancelaCupom(NumCOOCancelar: Integer);
var
  RetCmd : String ;
  NumUltimoCupom : String ;
  NumCupomCancelavel: String;
  iNumUltimoCupom, iNumCupomCancelavel: integer;
begin
  fsNumCupom := '';
  AguardaImpressao := True ;

  if fpMFD then
  begin
    RetCmd := EnviaComando( FS + 'R' + #200 + '046');  // Verifica se precisa cancelar CCD;
    if copy(RetCmd, 6, 1) <> '0' then
    begin
      try
        RetCmd := EnviaComando( FS + 'R' + #200 + '050'); // retorna numero do cupom cancelavel
        NumCupomCancelavel  := copy(RetCmd, 6, 6);
        NumUltimoCupom := GetNumCupom;
        iNumUltimoCupom := StrToInt(NumUltimoCupom);
        iNumCupomCancelavel := StrToInt(NumCupomCancelavel);

        while iNumCupomCancelavel < iNumUltimoCupom do
        begin
          NumUltimoCupom := FormatFloat('000000',iNumUltimoCupom);
          EnviaComando(FS + 'F' + #214 , 15); // Fecho o CCD caso ainda não esteja fechado
          EnviaComando(FS + 'F' + #218 + NumUltimoCupom +#255+#255+#255, 15); // Cancela Conprovante Não Fiscal
          EnviaComando(FS + 'F' + #214 , 15); // Fecha Comprovante de estorno Cancela Conprovante Não Fiscal
          dec(iNumUltimoCupom);
        end;

      except
      end;
      AguardaImpressao := True;
      EnviaComando(FS + 'F' + #211, 15) ;  // Cancela Cupom

      RespostasComando.Clear;
      RespostasComando.AddField('COO', Copy(fpRespostaComando, 10, 6));
      RespostasComando.AddField('CCF', Copy(fpRespostaComando, 16, 6));
      RespostasComando.AddField('ValorCancelado', Copy(fpRespostaComando, 22, 12));

      // Cancelamento retorna dados do Cupom Fiscal Cancelado e nao do Cancelamento
      fsNumCupom := '' ; // RespostasComando['COO'].AsString;
      fsNumCCF   := '' ; // RespostasComando['CCF'].AsString;
    end
    else
      raise EACBrECFCMDInvalido.Create( ACBrStr('Não existe documento para cancelar.') );
  end

  else if fsNumVersao = '2000' then
     EnviaComando(ESC + #211, 15)
  else
     EnviaComando(ESC + #206, 15) ;

  ZeraTotalApagar;

  FechaRelatorio ;   { Fecha relatorio se ficou algum aberto (só por garantia)}
end;

procedure TACBrECFDaruma.CancelaItemVendido(NumItem: Integer);
begin
  if fpMFD then
  begin
    EnviaComando(FS + 'F' + #203 + IntToStrZero(NumItem ,3) );

    RespostasComando.Clear;
    RespostasComando.AddField('ValorCancelado', Copy(fpRespostaComando, 13, 11));
  end
  else
  if fsNumVersao = '2000' then
     EnviaComando(ESC + #204 + IntToStrZero( NumItem ,3) )
  else
     EnviaComando(ESC + #205 + IntToStrZero( NumItem ,3) ) ;
end;

procedure TACBrECFDaruma.CancelaItemVendidoParcial(NumItem: Integer;
  Quantidade: Double);
begin
  if fpMFD then
  begin
    EnviaComando(FS + 'F' + #204 +  IntToStrZero(NumItem,3) +
                  IntToStrZero(Round(Quantidade * power(10,fpDecimaisQtd)), 7));

    RespostasComando.Clear;
    RespostasComando.AddField('ValorCancelado', Copy(fpRespostaComando, 10, 11));
  end;
end;

procedure TACBrECFDaruma.CancelaDescontoAcrescimoItem(NumItem: Integer;
  TipoAcrescimoDesconto: String);
begin
  if fpMFD then
    EnviaComando(FS + 'F' + #205 +  IntToStrZero(NumItem,3));
end;

procedure TACBrECFDaruma.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
var
  RetCmd : AnsiString ;
  Sinal: AnsiString ;
  Saldo: AnsiString ;
begin
  if fpMFD then
  begin
    Observacao := LeftStr(Observacao,84) + cDELIMITADOR ;
    RetCmd := EnviaComando( FS + 'F' + #209 + CodFormaPagto +
                IntToStrZero( Round( Valor * 100),12) + Observacao, 2) ;
    fsTotalAPagar := RoundTo( StrToFloatDef( copy(RetCmd,10,13),0 ) / 100, -2) ;

    // retornos da daruma
    Sinal := Copy(fpRespostaComando, 10,  1);
    Saldo := Copy(fpRespostaComando, 11, 12);

    RespostasComando.Clear;
    if Sinal = '+' then
      RespostasComando.AddField('Saldo', Saldo)
    else
      RespostasComando.AddField('Saldo', FloatToStr(StrToFloat(Saldo) * -1));
  end
  else
  begin
    Observacao := LeftStr(Observacao,48) + cDELIMITADOR ;

    if fsNumVersao = '2000' then
      RetCmd := EnviaComando( ESC + #207 + CodFormaPagto +
                  IntToStrZero( Round( Valor * 100),12) + Observacao, 2)
    else
      RetCmd := EnviaComando( ESC + #242 + CodFormaPagto +
                  IntToStrZero( Round( Valor * 100),12) + Observacao, 2) ;

    //fsTotalAPagar := RoundTo( StrToFloatDef( copy(RetCmd,2,12),0 ) / 100, -2) ;
    fsTotalAPagar := RoundTo( fsTotalAPagar - Valor, -2) ;
  end ;

  fsEmPagamento := true ;
  fsRet244      := '' ;
end;

procedure TACBrECFDaruma.EstornaPagamento(const CodFormaPagtoEstornar,
  CodFormaPagtoEfetivar : String; const Valor: Double;
  Observacao : AnsiString = '') ;
begin
  if fpMFD then
  begin
    EnviaComando(FS + 'F' + #228 +  CodFormaPagtoEstornar +
            CodFormaPagtoEfetivar + IntToStrZero( Round( Valor * 100),12) +
            LeftStr( Observacao, 619 ) + cDELIMITADOR);
  end ;
end;

procedure TACBrECFDaruma.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
Var
  Obs, StrConsumidor : AnsiString ;
begin
  Obs := Observacao ;
  if (not Consumidor.Enviado) then
  begin
    try
      AguardaImpressao := True ;
      if (fpMFD) and not(fsNumVersao = '010400') then
      begin
        StrConsumidor := LeftStr(Consumidor.Documento,20) + cDELIMITADOR +
                         LeftStr(Consumidor.Nome,30) + cDELIMITADOR +
                         LeftStr(Consumidor.Endereco,79) + cDELIMITADOR ;

        EnviaComando( FS + 'F' + #240 + StrConsumidor ) ;
      end
      else if fsNumVersao = '2000' then
      begin
        StrConsumidor := PadRight( PadRight(Consumidor.Documento,27) +
                               PadRight(Consumidor.Nome,42)+
                               PadRight(Consumidor.Endereco,42), 153) ;

        EnviaComando( ESC + #208 + StrConsumidor ) ;
      end
      else
      begin
        StrConsumidor := PadRight(Consumidor.Nome,84) +
                         PadRight(Consumidor.Endereco,84) +
                         PadRight(Consumidor.Documento,84) ;

        EnviaComando( ESC + #201 + StrConsumidor ) ;
      end ;

      Consumidor.Enviado := True ;
    except
      Obs := Observacao;
    end ;
  end ;

  Obs := StringReplace(Obs, #10, CR+LF, [rfReplaceAll]) + cDELIMITADOR ;

  AguardaImpressao := True ;
  if fpMFD then
  begin
    VerificarBmpTexto(IndiceBMP, Observacao);

    if (IndiceBMP > 0) then
      Obs :=  ESC + 'B' + IntToStrZero(IndiceBMP, 1) + Obs ;

    EnviaComando( FS + 'F' + #210 + '0' + Obs, 5 );

    RespostasComando.Clear;
    RespostasComando.AddField('COO', Copy(fpRespostaComando, 10,  6));
    RespostasComando.AddField('TotalLiquido', Copy(fpRespostaComando, 16, 12));

    fsSubTotal := RespostasComando['TotalLiquido'].AsFloat;
  end
  else
  if fsNumVersao = '2000' then
    EnviaComando( ESC + #209 + Obs, 10)
  else
    EnviaComando( ESC + #243 + Obs, 10) ;

  ZeraTotalApagar;
end;

procedure TACBrECFDaruma.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString );
Var
  A_D : AnsiChar ;
begin
  if DescontoAcrescimo > 0 then
    A_D := '3'
  else
    A_D := '1' ;

  DescontoAcrescimo := abs(DescontoAcrescimo) ;

  { Inicia fechamento com formas de Pagamento }
  if fpMFD then
  begin
    EnviaComando(FS + 'F' + #206 + A_D + IntToStrZero(Round(DescontoAcrescimo * 100), 12), 5);

    RespostasComando.Clear;
    RespostasComando.AddField('SubTotal', Copy(fpRespostaComando, 10, 12));
    fsSubTotal := RespostasComando['SubTotal'].AsFloat;
  end
  else
  if fsNumVersao = '2000' then
    EnviaComando(ESC + #206 + A_D + IntToStrZero(Round( DescontoAcrescimo * 100 ), 12), 5)
  else
    EnviaComando(ESC + #241 + A_D + IntToStrZero(Round( DescontoAcrescimo * 100 ), 12), 5) ;

  fsEmPagamento := true ;
  fsTotalAPagar := Subtotal ;
  fsRet244      := '' ;
end;

procedure TACBrECFDaruma.CancelaDescontoAcrescimoSubTotal(
  TipoAcrescimoDesconto: Char);
begin
  if fpMFD then
  begin
    if TipoAcrescimoDesconto = 'D' then
      TipoAcrescimoDesconto:= '0'
    else if TipoAcrescimoDesconto = 'A' then
      TipoAcrescimoDesconto:= '1';

    EnviaComando(FS + 'F' + #208 +  TipoAcrescimoDesconto);
  end;
end;

procedure TACBrECFDaruma.LegendaInmetroProximoItem;
begin
  if fpMFD then
    EnviaComando(FS + 'C' + #215 + '1');
end;

procedure TACBrECFDaruma.VendeItem(Codigo, Descricao : String ;
  AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
  ValorDescontoAcrescimo : Double ; Unidade : String ;
  TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
  CodDepartamento : Integer) ;
Var
  QtdStr, ValorStr, DescontoStr, SepDec, FlagDesc, ModoCalculo : String;
  LenQtd : Integer ;
  RetCmd : AnsiString ;
  Cmd : AnsiChar ;
begin
  if Qtd > 99999 then
    raise EACBrECFCMDInvalido.Create( ACBrStr('Quantidade deve ser inferior a 99999.'));

  if fpMFD then
  begin
    Codigo    := PadRight(Codigo,14) ;
    Unidade   := PadRight(Unidade,3) ;
    Descricao := TrimRight(LeftStr(Descricao,233)) + cDELIMITADOR ;

    if DescricaoGrande then
      FlagDesc := '00'
    else
      FlagDesc := '18' ;

    QtdStr      := IntToStrZero( Round(Qtd * power(10,fpDecimaisQtd)), 7) ;
    ValorStr    := IntToStrZero( Round( ValorUnitario * power(10,fpDecimaisPreco)),8 ) ;
    DescontoStr := StringOfChar('0',12) ;

    if ( fsModeloDaruma > fs700L) or
       ( (fsModeloDaruma = fs700L) and (StrToIntDef(fsNumVersao, -1) > 10000) ) then
    begin
      ModoCalculo :=  ifthen(fpArredondaItemMFD, 'A', 'T' );
      RetCmd := EnviaComando(FS + 'F' + #207 + AliquotaECF + QtdStr + ValorStr +
                DescontoStr + FlagDesc + Codigo + Unidade + ModoCalculo + Descricao ) ;
    end
    else
    begin
      if fpArredondaItemMFD then
      begin
        try
          // Tenta enviar o comando, se o ECF não reconhecer (except), desativa o ArredondaItemMFD;
          EnviaComando(FS + 'C' + #219 + 'A'); // A = Arredondamento / T = Truncamento
        except
          fpArredondaItemMFD := False ;
        end ;
      end ;

      RetCmd := EnviaComando(FS + 'F' + #201 + AliquotaECF + QtdStr + ValorStr +
                DescontoStr + FlagDesc + Codigo + Unidade + Descricao ) ;
    end ;

    RespostasComando.Clear;
    RespostasComando.AddField('NumeroItem',   Copy(fpRespostaComando, 10,  3));
    RespostasComando.AddField('ValorLiquido', Copy(fpRespostaComando, 14, 11));

    fsNumUltimoItem := RespostasComando['NumeroItem'].AsInteger;

    if ValorDescontoAcrescimo > 0 then
      DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo, DescontoAcrescimo,
        TipoDescontoAcrescimo, StrToIntDef(copy(RetCmd,10,3),0) ) ;
  end
  else
  if fsNumVersao = '2000' then
  begin
    fpArredondaItemMFD := False;
    Codigo      := PadRight(Codigo,18) ;    { Ajustando Tamanhos }
    Descricao   := TrimRight(LeftStr(Descricao,200)) + cDELIMITADOR ;
    ValorStr    := IntToStrZero( Round(ValorUnitario * 1000), 10) ;
    QtdStr      := IntToStrZero( Round(Qtd * 1000), 8) ;
    Unidade     := PadRight(Unidade,2) ;
    DescontoStr := StringOfChar('0',10) ;

    if ValorDescontoAcrescimo > 0 then
    begin
      if TipoDescontoAcrescimo = '%' then
      begin
        if DescontoAcrescimo = 'D' then
          DescontoStr := '0'
        else
          DescontoStr := '2' ;

        DescontoStr := DescontoStr + '00000'+
                       IntToStrZero( Round(ValorDescontoAcrescimo * 100), 4) ;
      end
      else
      begin
        if DescontoAcrescimo = 'D' then
          DescontoStr := '1'
        else
          DescontoStr := '3' ;

        DescontoStr := DescontoStr +
                       IntToStrZero( Round(ValorDescontoAcrescimo * 100), 9) ;
      end ;
    end ;

    EnviaComando(ESC + #202 + AliquotaECF + Codigo + DescontoStr +
                 ValorStr + QtdStr + Unidade + Descricao ) ;
  end
  else
  begin
    fpArredondaItemMFD := False;
    Codigo  := PadRight(Codigo,13) ;    // Ajustando Tamanhos
    Unidade := PadRight(Unidade,2) ;

    if TipoDescontoAcrescimo = '%' then
      DescontoStr := IntToStrZero( Round(ValorDescontoAcrescimo * 100), 4)
    else
      // FS345 não tem Desconto por Valor, calculando a Percentagem
      DescontoStr := IntToStrZero( Round( ValorDescontoAcrescimo/(ValorUnitario*Qtd) * 100 * 100), 4) ;

    if DescontoAcrescimo = 'D' then   // Desconto ou Acrescimo ?
      DescontoStr := '0' + DescontoStr
    else
      DescontoStr := '1' + DescontoStr ;

    if StrToInt(NumVersao) >= 345 then
    begin
      Descricao := TrimRight(LeftStr(Descricao,174)) + cDELIMITADOR ;

      if RoundTo(Qtd,-2) <> Qtd then //Tem mais de 2 casas dec na QTD ?
      begin
        LenQtd   := 8 ;
        Cmd      := #223 ;
        Qtd      := RoundTo(Qtd,-3) ;   // Venda fixa com 3 decimais
        Qtd      := (Qtd * 1000) ;
        SepDec   := '';
        ValorStr := IntToStrZero( Round(ValorUnitario * 1000), 10);
      end
      else
      begin
        LenQtd   := 6 ;                 // Venda com Posicao decimal variavel
        Cmd      := #225 ;
        SepDec   := ',';
        ValorStr := IntToStrZero( Round(ValorUnitario * 1000), 9);
        Codigo   := Codigo + '000'  // Reserva, compatib. modelos anteriores
      end ;
    end
    else
    begin
      LenQtd    := 5 ;
      Cmd       := #215 ;
      Descricao := PadRight(Descricao,30) ;
      ValorStr  := IntToStrZero( Round(ValorUnitario * 100), 9) ;
      Codigo    := Codigo + '000'  // Reserva, compatib. modelos anteriores
    end ;

    QtdStr := FloatToStr(Qtd) ;
    if Length(QtdStr) > LenQtd then
      QtdStr := FloatToStr(RoundTo(Qtd,-(LenQtd-pos(DecimalSeparator,QtdStr)))) ;

    QtdStr := PadLeft(StringReplace(QtdStr,DecimalSeparator,SepDec,[rfReplaceAll]), LenQtd,'0');

    EnviaComando( ESC + Cmd + AliquotaECF + Codigo + DescontoStr +
                   ValorStr + QtdStr + Unidade + Descricao) ;
  end ;

  ZeraTotalApagar;
end;

procedure TACBrECFDaruma.DescontoAcrescimoItemAnterior(
   ValorDescontoAcrescimo : Double ; DescontoAcrescimo : String ;
   TipoDescontoAcrescimo : String ; NumItem : Integer) ;
var
   DescontoStr : String ;
begin
  if not fpMFD then
     exit ;

  if TipoDescontoAcrescimo = '%' then
   begin
     if DescontoAcrescimo = 'D' then
        DescontoStr := '0'
     else
        DescontoStr := '2' ;
     DescontoStr := DescontoStr +
                    IntToStrZero( Round(ValorDescontoAcrescimo * 100), 4) +
                    StringOfChar('0',7) ;
   end
  else
   begin
     if DescontoAcrescimo = 'D' then
        DescontoStr := '1'
     else
        DescontoStr := '3' ;
     DescontoStr := DescontoStr +
                    IntToStrZero( Round(ValorDescontoAcrescimo * 100), 11) ;
   end ;

  EnviaComando(FS + 'F' + #202 + IntToStrZero(NumItem,3) + DescontoStr ) ;
end ;

procedure TACBrECFDaruma.ImprimeCheque(Banco: String; Valor: Double;
  Favorecido, Cidade: String; Data: TDateTime; Observacao: String);
Var
  ValStr,
  DataStr: String;
  Continuar: Boolean;

begin
  Continuar := True;
  DoOnChequeEstado(chqIdle, Continuar);

  Banco      := IntToStrZero( StrToInt(Banco), 3) ;
  Favorecido := LeftStr(Trim(Favorecido),65) ;
  Cidade     := LeftStr(Trim(Cidade),25) ;
  Observacao := LeftStr(Trim(Observacao),80) ;
  DataStr    := FormatDateTime('ddmmyyyy',Data) ;
  ValStr     := IntToStrZero( Round(abs(Valor)*100),12 ) ;

  // enviar comando para acertar o protocolo, conforme orientação da daruma
  EnviaComando(ESC + #190 + '20') ;

  // enviar todos os comando menos o valor
  EnviaComando(ESC + 'b' + Banco) ;
  EnviaComando(ESC + 'c' + Cidade + cDELIMITADOR) ;
  EnviaComando(ESC + 'd' + DataStr ) ;
  EnviaComando(ESC + 'f' + Favorecido + cDELIMITADOR ) ;

  // enviar o valor para iniciar a impressão do cheque
  AguardaImpressao := True ;
  DoOnChequeEstado(chqPosicione, Continuar);
  if Continuar then
  begin
    DoOnChequeEstado(chqImprimindo, Continuar);
    EnviaComando(ESC + 'v' + ValStr ) ;
    DoOnChequeEstado(chqRetire, Continuar);

    // impressão no verso
    if Trim(Observacao) <> EmptyStr then
    begin
      AguardaImpressao := True ;

      DoOnChequeEstado(chqAutenticacao, Continuar);
      if Continuar then
        EnviaComando(ESC + 't' + Observacao + cDELIMITADOR )
      else
        CancelaImpressaoCheque;
    end;
  end
  else
    CancelaImpressaoCheque;
end;

procedure TACBrECFDaruma.CancelaImpressaoCheque;
begin
  EnviaComando( ESC+'mE' ) ; 
end;

procedure TACBrECFDaruma.CarregaAliquotas;
Var RetCmd    : AnsiString ;
    Aliquota  : TACBrECFAliquota ;
    ValAliq   : Double ;
    AliquotaStr:String;
    Cont, UltimaAliquota : Integer ;
begin
  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  if fpMFD then
  begin
    RetCmd :=  RetornaInfoECF('125');
    UltimaAliquota := 16 ;

    {Procura qual foi a última alíquota cadastrada diferente de alíquota zero}
    for Cont := 16 Downto 1 do
    begin
       AliquotaStr := Trim(copy(RetCmd, ((Cont-1) * 5) + 1, 5)) ;

       if (AliquotaStr <> '') and (AliquotaStr[2] <> #255) then
       begin
          ValAliq := RoundTo( StrToIntDef( copy(AliquotaStr,2,4) ,0) / 100, -2) ;
          if ValAliq > 0 then
          begin
             UltimaAliquota := Cont;
             Break;
          end;
       end ;
    end;

    for Cont := 1 to UltimaAliquota do
    begin
      AliquotaStr := Trim(copy(RetCmd, ((Cont-1) * 5) + 1, 5)) ;

      if (AliquotaStr <> '') and (AliquotaStr[2] <> #255) then
      begin
        ValAliq := RoundTo( StrToIntDef( copy(AliquotaStr,2,4) ,0) / 100, -2) ;

        Aliquota          := TACBrECFAliquota.create ;
        Aliquota.Indice   := IntToStrZero(Cont, 2);
        Aliquota.Aliquota := ValAliq ;
        if AliquotaStr[1] = '0' then // Isso pode não estar certo!!!!  verificar com Daruma
          Aliquota.Tipo     := 'T'
        else
          Aliquota.Tipo     := 'S';

        fpAliquotas.Add( Aliquota ) ;
      end ;
    end ;
  end
  else
  begin
    RetCmd := EnviaComando( ESC + #231 ) ;

    if fsNumVersao = '2000' then
       RetCmd := copy(RetCmd,3,Length(RetCmd)-5)   {Retira :% e CR }
    else
       RetCmd := copy(RetCmd,3,Length(RetCmd)-3) ;  {Retira :% e CR }

    while Length(RetCmd) > 0 do
    begin
      if fsNumVersao = '2000' then
         ValAliq := RoundTo( StrToIntDef( copy(RetCmd,1,4) ,-100) / 100, -2)
      else
         ValAliq := RoundTo( StrToIntDef( copy(RetCmd,2,4) ,-100) / 100, -2) ;

      if ValAliq >= 0 then
      begin
         Aliquota := TACBrECFAliquota.create ;

         if fpMFD then
            Aliquota.Indice := IntToStrZero(fpAliquotas.Count+1,2)
         else
            Aliquota.Indice := 'T'+UpCase(RetCmd[1]) ;

         Aliquota.Aliquota := ValAliq ;
         if UpCase(RetCmd[1]) <> RetCmd[1] then { é minuscula ? }
            Aliquota.Tipo := 'S' ;

         fpAliquotas.Add( Aliquota ) ;
      end ;

      RetCmd := copy(RetCmd,6,Length(RetCmd) ) ;
    end ;
  end;
end;

procedure TACBrECFDaruma.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String );
Var ValStr, TipoStr : String ;
    ProxIndice  : Integer;
begin
  { Esse comando na Daruma nao usa o parametro Posicao }
  ValStr  := IntToStrZero( Round(Aliquota * 100) ,4) ;
  TipoStr := UpperCase(Tipo) ;

  if fsNumVersao = '2000' then
     raise EACBrECFERRO.Create(ACBrStr('ProgramaAliquota ainda não implemenado na FS2000'))

  else if fpMFD then
  begin
    CarregaAliquotas ;

    if AchaICMSAliquota(Aliquota, TipoStr[1]) <> nil then
      raise EACBrECFERRO.Create(ACBrStr('Aliquota (' + FormatFloat('###,##0.00', Aliquota) + ') já existe.')) ;

    ProxIndice := StrToIntDef(Posicao,0) ;
    if (ProxIndice < 1) or (ProxIndice > 16) then { Indice passado é válido ? }
    begin
      For ProxIndice := 1 to 16 do  { Procurando Lacuna }
      begin
        if AchaICMSIndice(IntToStrZero(ProxIndice,2)) = nil then
          break ;
      end ;
    end ;

    if ProxIndice > 16 then
      raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Aliquotas !'));

{  Código comentado, pois o comando abaixo está errado...
    if TipoStr <> 'S' then
      TipoStr := '1' // Serviço
    else
      TipoStr := '0';// Produto

    EnviaComando( FS + 'C' + #201 + IntToStrZero(ProxIndice,2) + TipoStr + ValStr ) ;
}    
    if TipoStr <> 'S' then
       TipoStr := '' ;
    EnviaComando( ESC + #220 + TipoStr + ValStr ) ;

  end
  else
  begin
    if TipoStr <> 'S' then
       TipoStr := '' ;
    EnviaComando( ESC + #220 + TipoStr + ValStr ) ;
  end;
  
  CarregaAliquotas ;  { Re-avalia as aliquotas }
end;

function TACBrECFDaruma.AchaICMSAliquota( var AliquotaICMS: String) :
  TACBrECFAliquota ;
begin
  if upcase(AliquotaICMS[1]) = 'T' then
  begin
    if StrIsAlpha(copy(AliquotaICMS,2,1)) then
      AliquotaICMS := 'T'+copy(AliquotaICMS,1,2)       {Indice TA, TB, TC}
    else
      AliquotaICMS := 'T'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice T01, T1, T02}
  end;

  Result := inherited AchaICMSAliquota( AliquotaICMS );

  if not fpMFD then
  begin
    if CharInSet(AliquotaICMS[1] , ['F','I','N']) then
      AliquotaICMS := AliquotaICMS[1]+' ';
  end
  else
  begin
    if AliquotaICMS = 'F1' then
      AliquotaICMS := '17'
    else if AliquotaICMS = 'F2' then
      AliquotaICMS := '18'
    else if AliquotaICMS = 'I1' then
      AliquotaICMS := '19'
    else if AliquotaICMS = 'I2' then
      AliquotaICMS := '20'
    else if AliquotaICMS = 'N1' then
      AliquotaICMS := '21'
    else if AliquotaICMS = 'N2' then
      AliquotaICMS := '22'
    else if AliquotaICMS = 'FS1' then
      AliquotaICMS := '23'
    else if AliquotaICMS = 'FS2' then
      AliquotaICMS := '24'
    else if AliquotaICMS = 'IS1' then
      AliquotaICMS := '25'
    else if AliquotaICMS = 'IS2' then
      AliquotaICMS := '26'
    else if AliquotaICMS = 'NS1' then
      AliquotaICMS := '27'
    else if AliquotaICMS = 'NS2' then
      AliquotaICMS := '28'
  end;
end ;

procedure TACBrECFDaruma.CarregaTotalizadoresNaoTributados;
begin
  if not fpMFD then
  begin
    inherited CarregaTotalizadoresNaoTributados;
    exit;
  end;

  if Assigned( fpTotalizadoresNaoTributados ) then
     fpTotalizadoresNaoTributados.Free ;

  fpTotalizadoresNaoTributados := TACBrECFTotalizadoresNaoTributados.create( true ) ;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'F1';
  end;
  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'F2';
  end;
  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'I1';
  end;
  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'I2';
  end;
  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'N1';
  end;
  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'N2';
  end;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'FS1';
    Tipo := 'S';
  end;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'FS2';
    Tipo := 'S';
  end;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'IS1';
    Tipo := 'S';
  end;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'IS2';
    Tipo := 'S';
  end;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'NS1';
    Tipo := 'S';
  end;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'NS2';
    Tipo := 'S';
  end;
end;

procedure TACBrECFDaruma.LerTotaisTotalizadoresNaoTributados;
var
   RetCmd: AnsiString;
   A: Integer;
begin
  if not fpMFD then
  begin
    inherited LerTotaisTotalizadoresNaoTributados;
    exit;
  end;

  if not Assigned(fpTotalizadoresNaoTributados) then
    CarregaTotalizadoresNaoTributados;

  RetCmd := RetornaInfoECF('003');

  For A := 0 to fpTotalizadoresNaoTributados.Count -1 do
    fpTotalizadoresNaoTributados[A].Total :=
       RoundTo( StrToFloatDef(Copy(RetCmd, 209 + (13*A), 13),0)/100, -2);
end;

procedure TACBrECFDaruma.CarregaFormasPagamento;  { funçao Lenta +- 3 sec. }
Var RetCmd, StrFPG, StrCNF, StrCNFVinc, Token, Descricao : AnsiString ;
    Cont,Indice : Integer ;
    FPagto : TACBrECFFormaPagamento ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  inherited CarregaFormasPagamento ;       {Inicializa fpFormasPagamentos}
  inherited CarregaComprovantesNaoFiscais ;{Inicializa fpComprovantesNaoFiscais}

  { Inicializando a Tabela de CNF vinculados (de uso interno) }
  if Assigned( fsCNFVinc ) then
     fsCNFVinc.Free ;
  fsCNFVinc := TACBrECFComprovantesNaoFiscais.Create( true ) ;

  try
    if fsNumVersao = '2000' then
    begin
      RetCmd := EnviaComando( ESC + #234 ) ;

      StrCNF := copy(RetCmd, 33 , 352) ;
      StrFPG := copy(RetCmd, 721, 704) ;

      for Cont := 1 to 32 do
      begin
        { Adicionando as Formas de Pagamento }
        Token     := copy(StrFPG, ((Cont-1) * 22) + 1, 22) ;
        Descricao := Trim(copy(Token,2,17)) ;
        if (Descricao <> '') and (Token[2] <> #255) then
        begin
          FPagto := TACBrECFFormaPagamento.create ;

          Indice := Cont ;
          if Cont > 16 then
            Indice := Indice + 34 ;

          FPagto.Indice    := IntToStrZero(Indice,2) ;
          FPagto.Descricao := Descricao ;
          FPagto.PermiteVinculado := (Token[1] = 'V');

          fpFormasPagamentos.Add( FPagto ) ;
        end ;
      end ;

      for Cont := 1 to 16 do
      begin
        { Adicionando os Comprovantes Nao Fiscais NAO Vinculados }
        Token := copy(StrCNF, ((Cont-1) * 22) + 1, 22) ;
        if (Token <> '') and (Token[2] <> #255) then
        begin
          CNF := TACBrECFComprovanteNaoFiscal.create ;

          CNF.Indice := chr(64+Cont);
          CNF.Descricao := Trim(Token) ;

          fpComprovantesNaoFiscais.Add( CNF ) ;
        end ;
      end ;
    end
    else if fpMFD then
    begin // Para demais impressoras termicas com mfd
      StrFPG  :=  RetornaInfoECF('126');
      StrCNF  :=  RetornaInfoECF('127');

      for Cont := 1 to 20 do
      begin
        { Adicionando as Formas de Pagamento }
        Descricao  := Trim(Copy(StrFPG, ((Cont-1) * 15) + 1, 15)) ;

        if (Descricao <> '') and (Descricao[2] <> #255) then
        begin
          FPagto := TACBrECFFormaPagamento.create ;

          FPagto.Indice := IntToStrZero(Cont,2);
          FPagto.Descricao := Descricao ;
          FPagto.PermiteVinculado := (Cont <> 1);

          fpFormasPagamentos.Add( FPagto ) ;
        end ;

        { Adicionando os Comprovantes Nao Fiscais NAO Vinculados }
        Descricao := Trim(Copy(StrCNF, ((Cont-1) * 15) + 1, 15)) ;
        if (Descricao <> '') and (Descricao[2] <> #255) then
        begin
          CNF := TACBrECFComprovanteNaoFiscal.create ;

          CNF.Indice    := IntToStrZero(Cont,2);
          CNF.Descricao := Descricao ;
          CNF.PermiteVinculado  := False ;

          fpComprovantesNaoFiscais.Add( CNF ) ;
        end ;
      end ;
    end
    else
    begin
      RetCmd := EnviaComando( ESC + #238 ) ;

      StrCNF     := copy(RetCmd, 22, 352) ;
      StrCNFVinc := copy(RetCmd, 374, 336) ;
      StrFPG     := copy(RetCmd, 710, 288) ;

      for Cont := 1 to 16 do
      begin
        { Adicionando as Formas de Pagamento }
        Token     := copy(StrFPG, ((Cont-1) * 18) + 1, 18) ;
        Descricao := Trim(copy(Token,2,17)) ;
        if (Descricao <> '') and (Descricao[2] <> #255) and
          (UpperCase(Trim(Descricao)) <> 'PAGAMENTO TIPO '+chr(64+Cont)) then
        begin
          FPagto := TACBrECFFormaPagamento.create ;

          if fpMFD then
            FPagto.Indice := IntToStrZero(Cont,2)
          else
            FPagto.Indice := chr(64+Cont) ;
          FPagto.Descricao := Descricao ;
          FPagto.PermiteVinculado := (Token[1] = 'V');

          fpFormasPagamentos.Add( FPagto ) ;
        end ;

        { Adicionando os Comprovantes Nao Fiscais NAO Vinculados }
        Token := copy(StrCNF, ((Cont-1) * 22) + 1, 22) ;
        Descricao := Trim(copy(Token,2,21));
        if (Token <> '') and (Descricao[2] <> #255) then
        begin
          CNF := TACBrECFComprovanteNaoFiscal.create ;

          if fpMFD then
            CNF.Indice := IntToStrZero(Cont,2)
          else
            CNF.Indice := chr(64+Cont) ;
          CNF.Descricao := Descricao ;
          CNF.PermiteVinculado  := False ;

          fpComprovantesNaoFiscais.Add( CNF ) ;
        end ;

        { Adicionando os Comprovantes Nao Fiscais Vinculados (tabela interna) }
        Token := Copy(StrCNFVinc, ((Cont-1) * 21) + 1, 21) ;
        if (Token <> '') and (Token[2] <> #255) then
        begin
          CNF := TACBRECFComprovanteNaoFiscal.create ;

          if fpMFD then
            CNF.Indice := IntToStrZero(Cont,2)
          else
            CNF.Indice := chr(64+Cont) ;
          CNF.Descricao := Trim(Token);
          CNF.PermiteVinculado  := True ;

          fsCNFVinc.Add( CNF ) ;
        end ;
      end ;
    end
  except
    { Se falhou ao carregar, deve "nilzar" as variaveis para que as rotinas
    "Acha*" tentem carregar novamente }
    fpFormasPagamentos.Free ;
    fpFormasPagamentos := nil ;

    fpComprovantesNaoFiscais.Free ;
    fpComprovantesNaoFiscais := nil ;

    fsCNFVinc.Free ;
    fsCNFVinc := nil ;

    raise ;
  end ;
end;

procedure TACBrECFDaruma.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String);
Var ProxIndice : Integer ;
    FPagto : TACBrECFFormaPagamento ;
    FlagVinculado : Char ;
begin
  { Força uma recarga para garantir que tem o dados atualizados }
  CarregaFormasPagamento ;

  { Daruma cadastra qualquer descrição mesmo repetida, por isso vamos ver se ja existe antes }
  FPagto:= AchaFPGDescricao(Trim(Descricao),True);
  if FPagto <> nil then
     raise EACBrECFERRO.Create(ACBrStr('Forma de Pagamento já cadastrada'));

  ProxIndice := StrToIntDef(Posicao,-1) ;

  if fsNumVersao = '2000' then
     raise EACBrECFERRO.Create(ACBrStr('ProgramaFormaPagamento ainda não implemenado na FS2000'))

  else if fpMFD then
  begin
    Descricao := PadRight(Descricao,15) ;

    if (ProxIndice < 2) or (ProxIndice > 15) then { Indice passado é válido ? }
    begin
      For ProxIndice := 2 to 20 do  { Procurando Lacuna }
      begin
        if AchaFPGIndice(IntToStrZero(ProxIndice,2)) = nil then
          break ;
      end ;
    end ;

    if ProxIndice > 20 then
      raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Formas de '+
                               'Pagamento'));

    EnviaComando( FS + 'C' + #203 + IntToStrZero(ProxIndice,2) + Descricao ) ;

    { Força uma recarga para garantir que tem o dados atualizados }
    CarregaFormasPagamento ;
  end
  else
  begin
    Descricao := PadRight(Descricao,17) ;

    if (ProxIndice < 0) or (ProxIndice > 15) then { Indice passado é válido ? }
    begin
      For ProxIndice := 0 to 16 do  { Procurando Lacuna }
      begin
        if AchaFPGIndice(chr(65+ProxIndice)) = nil then
          break ;
      end ;
    end ;

    if ProxIndice > 15 then
      raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Formas de '+
                               'Pagamento'));

    If PermiteVinculado then FlagVinculado := 'V' else FlagVinculado := 'X' ;
    EnviaComando( ESC + #218 + 'PG' + FlagVinculado + chr(65+ProxIndice) +
                    Descricao ) ;

    { Adcionanodo nova FPG no ObjectList }
    FPagto := TACBrECFFormaPagamento.create ;
    FPagto.Indice    := chr(65+ProxIndice) ;
    FPagto.Descricao := Descricao ;
    FPagto.PermiteVinculado := PermiteVinculado ;
    fpFormasPagamentos.Add( FPagto ) ;

    if PermiteVinculado then  { Equalizando Formas de Pagamento com CNFs }
      if AchaCNFVincDescricao(Descricao) = nil then
        ProgramaComprovanteNaoFiscal(Descricao,'V');
  end ;
end;

procedure TACBrECFDaruma.CarregaRelatoriosGerenciais;
Var
  Token1, Token2, Descricao,
  StrRG, StrCER : AnsiString ;
  Cont, CER : Integer ;
  RG  : TACBrECFRelatorioGerencial ;
begin
  inherited CarregaRelatoriosGerenciais ;   {Inicializa fpRelatoriosGerenciais}

  try
    if fsNumVersao = '2000' then
      raise EACBrECFERRO.Create(ACBrStr('ProgramaRelatorioGerencial ainda não implemenado na FS2000'))

    else if fpMFD then // Para daruma FS600 e FS700
    begin
      StrRG   := RetornaInfoECF('128') ;

      StrCER  := RetornaInfoECF('044') ;

      for Cont := 1 to 20 do
      begin
        { Adicionando os Relatorios Gerenciais }
        Token1    := copy(StrRG, ((Cont-1) * 15) + 1, 15) ;
        Descricao := Trim(Token1) ;

        Token2:= copy(StrCER, ((Cont-1) * 4) + 1, 4) ;
        CER   := StrToIntDef(Token2, 0) ;
        if (Descricao <> '') and (Descricao[2] <> #255) then
        begin
          RG := TACBrECFRelatorioGerencial.create ;

          RG.Indice    := IntToStrZero(Cont,2);
          RG.Descricao := Descricao ;
          RG.Contador  := CER;

          fpRelatoriosGerenciais.Add( RG ) ;
        end ;
      end ;
    end
    else
      raise EACBrECFERRO.Create(ACBrStr('ECF FS345 não suporta RelatorioGerencial'));
  except
      { Se falhou ao carregar, deve "nilzar" as variaveis para que as rotinas
        "Acha*" tentem carregar novamente }
      fpRelatoriosGerenciais.Free ;
      fpRelatoriosGerenciais := nil ;

      raise ;
  end ;

end;

procedure TACBrECFDaruma.LerTotaisRelatoriosGerenciais ;
begin
  CarregaRelatoriosGerenciais;
end ;

procedure TACBrECFDaruma.ProgramaRelatorioGerencial(var Descricao: String; Posicao: String);
Var
  ProxIndice : Integer ;
begin
  CarregaRelatoriosGerenciais ;

  Descricao := Trim(Descricao) ;
  ProxIndice := StrToIntDef(Posicao, -1) ;

  if fsNumVersao = '2000' then
     raise EACBrECFERRO.Create(ACBrStr('ProgramaRelatorioGerencial ainda não implemenado na FS2000'))

  else if fpMFD then
  begin
    if AchaRGDescricao(Descricao, True) <> nil then
      raise EACBrECFERRO.Create(ACBrStr('Relatório Gerencial ('+Descricao+') já existe.')) ;

    if (ProxIndice < 2) or (ProxIndice > 20) then { Indice passado é válido ? }
    begin
      For ProxIndice := 2 to 20 do  { Procurando Lacuna }
      begin
        if AchaRGIndice(IntToStrZero(ProxIndice,2)) = nil then
        break ;
      end ;
    end ;

    if ProxIndice > 20 then
      raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novos RGs'));

    EnviaComando( FS + 'C' + #205 + IntToStrZero(ProxIndice,2) + PadRight(Descricao,15) ) ;
  end
  else
    raise EACBrECFERRO.Create(ACBrStr('ECF FS345 não suporta RelatorioGerencial'));

  CarregaRelatoriosGerenciais ;
end;

procedure TACBrECFDaruma.CarregaComprovantesNaoFiscais;
begin
  CarregaFormasPagamento ;
end;

procedure TACBrECFDaruma.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String );
Var ProxIndice : Integer ;
begin
  CarregaComprovantesNaoFiscais ;

  Descricao := Trim(Descricao) ;
  ProxIndice := StrToIntDef(Posicao,-1) ;

  if fsNumVersao = '2000' then
     raise EACBrECFERRO.Create(ACBrStr('ProgramaComprovanteNaoFiscal ainda não implemenado na FS2000'))

  else if fpMFD then
  begin
    if AchaCNFDescricao(Descricao, True) <> nil then
      raise EACBrECFERRO.Create(ACBrStr('Comprovante não fiscal ('+Descricao+') já existe.')) ;

    if (ProxIndice < 3) or (ProxIndice > 20) then { Indice passado é válido ? }
    begin
      For ProxIndice := 3 to 20 do  { Procurando Lacuna }
      begin
        if AchaCNFIndice(IntToStrZero(ProxIndice,2)) = nil then
          break ;
      end ;
    end ;

    if ProxIndice > 20 then
      raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas CNFs'));

    EnviaComando( FS + 'C' + #204 + IntToStrZero(ProxIndice,2) + PadRight(Descricao,15) ) ;

    CarregaComprovantesNaoFiscais ;
  end
  else
  begin
    { Esse comando na Daruma nao usa o parametro Posicao }
    Descricao := PadRight(Descricao,21) ;
    if Tipo = '' then
      Tipo := 'V'
    else
      Tipo := UpperCase(Tipo) ;

    if pos(Tipo,'V+-') = 0 then
      raise EACBrECFERRO.Create(ACBrStr('Os Tipos válidos para Daruma são:'+sLineBreak+
                               'V  Comprovante Vinculado'+sLineBreak+
                               '+  Entrada de Recursos'+sLineBreak+
                               '-  Saida de Recursos')) ;
    if Tipo = 'V' then
    begin
      if AchaCNFVincDescricao(Descricao) <> nil then
        raise EACBrECFERRO.Create(ACBrStr('Comprovante não fiscal ('+Descricao+') já existe.')) ;
    end
    else
      if AchaCNFDescricao(Descricao, True) <> nil then
        raise EACBrECFERRO.Create(ACBrStr('Comprovante não fiscal ('+Descricao+') já existe.')) ;

    EnviaComando( ESC + #226 + Tipo + Descricao ) ;
    CarregaComprovantesNaoFiscais ;
  end ;
end;


procedure TACBrECFDaruma.AbreRelatorioGerencial(Indice: Integer);
Var
  IndiceStr : String;
  RG  : TACBrECFRelatorioGerencial;
begin
  fsNumCupom := '';
  AguardaImpressao := True ;
  IndiceStr :=  IntToStrZero(Indice, 2);
  if fpMFD then
  begin
    if Indice > 0 then
    begin
      RG  := AchaRGIndice( IndiceStr ) ;
      if RG = nil then
        raise EACBrECFERRO.create( ACBrStr('Relatório Gerencial: '+IndiceStr+
                                ' não foi cadastrado.') ) ;

      EnviaComando(FS + 'F' + #230 + IndiceStr, 5) ;
    end
    else
      EnviaComando(FS + 'F' + #230 + '01', 5) ;

    fsTipoRel := 'G' ;
  end
  else if fsNumVersao='2000' then
     EnviaComando(ESC + #214, 30)
  else
     EnviaComando(ESC + #211, 30) ;
end;

procedure TACBrECFDaruma.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
Var Linhas : TStringList ;
    P, Espera : Integer ;
    Buffer : AnsiString ;
    MaxChars : Integer ;
    RetCmd : AnsiString ;
begin
  Linha := AjustaLinhas( Linha, Colunas );  { Formata as Linhas de acordo com "Coluna" }
  MaxChars := 619 ;     { Daruma MFD aceita no máximo 619 caract. por comando }

  if not fpMFD then
  begin
     Linhas := TStringList.Create ;
     try
        Linhas.Text := Linha ;

        for P := 0 to Linhas.Count-1  do
           if fsNumVersao = '2000' then
              EnviaComando( ESC + #215 + Linhas[P] + LF )
           else
              EnviaComando( ESC + #213 + Linhas[P] + LF ) ;
     finally
        Linhas.Free ;
     end ;
  end
  else
  begin
    VerificarBmpTexto(IndiceBMP, Linha);

    if not CharInSet(fsTipoRel , ['G','V']) then   // Achando o Tipo de Relatorio //
    begin
      RetCmd := RetornaInfoECF('056') ;

      if RetCmd = '4' then // RG
        fsTipoRel := 'G'
      else if RetCmd = '3' then // CCD
        fsTipoRel := 'V'
      else
        fsTipoRel := ' ' ;
    end ;

    while Length( Linha ) > 0 do
    begin
      P := Length( Linha ) ;
      if P > MaxChars then    { Acha o fim de Linha mais próximo do limite máximo }
        P := PosLast(#10, LeftStr(Linha,MaxChars) ) ;

      if P = 0 then
        P := Colunas ;

      Buffer := copy( Linha, 1, P)  ;
      Espera := Trunc( CountStr( Buffer, #10 ) / 4) ;

      if (IndiceBMP > 0) then
        Buffer  :=  ESC + 'B' + IntToStrZero(IndiceBMP, 1) + Buffer ;

      AguardaImpressao := (Espera > 3) ;
      if fsTipoRel = 'V' then
        EnviaComando( FS + 'F' + #213 + Buffer + cDELIMITADOR, Espera )
      else
        EnviaComando( FS + 'F' + #231 + Buffer + cDELIMITADOR, Espera ) ;

      { ficou apenas um LF sozinho ? }
      if (P = Colunas) and (RightStr( Buffer, 1) <> #10) and
        (copy( Linha, P+1, 1) = #10) then
        P := P + 1 ;

      Linha  := copy( Linha, P+1, Length(Linha) ) ;   // O Restante
    end ;
  end ;
end;

procedure TACBrECFDaruma.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
var
  FPG: TACBrECFFormaPagamento ;
  CNF: TACBrECFComprovanteNaoFiscal ;
  StrValor, StrConsumidor: String ;
begin
  fsNumCupom := '';
  COO      := Poem_Zeros( trim(COO) ,6) ;
  StrValor := IntToStrZero( Round(Valor * 100) ,12) ;

  AguardaImpressao := True ;

  if fpMFD then
  begin
    if StrIsAlpha( Trim(CodFormaPagto) ) then
      CodFormaPagto := IntToStrZero(Ord(CodFormaPagto[1]) - 64,2) ;

    StrConsumidor := LeftStr(Consumidor.Documento,20) + cDELIMITADOR +
                     LeftStr(Consumidor.Nome,30)      + cDELIMITADOR +
                     LeftStr(Consumidor.Endereco,79)  + cDELIMITADOR ;

    EnviaComando(FS + 'F' + #212 + CodFormaPagto + '01' + COO + StrValor +
                   StrConsumidor ) ;

    Consumidor.Enviado := True ;
    fsTipoRel := 'V'
  end
  else
  if fsNumVersao = '2000' then
    EnviaComando(ESC + #213 +CodFormaPagto+ COO + StrValor, 8)
  else
  begin
    FPG := AchaFPGIndice( CodFormaPagto ) ;
    if FPG = nil then
      raise EACBrECFERRO.create( ACBrStr('Forma de Pagamento: '+CodFormaPagto+
                                ' não foi cadastrada.') ) ;

    if CodComprovanteNaoFiscal <> '' then
    begin
      CNF := AchaCNFVincIndice( CodComprovanteNaoFiscal ) ;
      if CNF = nil then
        raise EACBrECFERRO.create( ACBrStr('Comprovante NÃO Fiscal Vinculado: '+
                            CodComprovanteNaoFiscal+' não cadastrado.') ) ;
    end
    else
    begin
      CNF := AchaCNFVincDescricao( FPG.Descricao ) ;
      if CNF = nil then
        raise EACBrECFERRO.create( ACBrStr('Não existe nenhum Comprovante NÃO Fiscal Vinculado'+
                            ' com a Descrição: '+FPG.Descricao)) ;
    end ;

    AguardaImpressao := True ;
    EnviaComando(ESC + #219 + CNF.Indice + FPG.Indice + COO + StrValor, 8);
  end ;
end;

procedure TACBrECFDaruma.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha )
end;

procedure TACBrECFDaruma.FechaRelatorio;
Var RetCmd : AnsiString ;
begin
  if fpMFD then
  begin
    // Busco o tipo de Documento a Fechar
    RetCmd  :=  Trim(RetornaInfoECF('056'));

    if RetCmd = '2' then // CNF
      EnviaComando(FS + 'F' + #226, 8 )
    else if RetCmd = '3' then  // CCD
      EnviaComando(FS + 'F' + #214, 8 )
    else if RetCmd = '4' then  // RG
      EnviaComando(FS + 'F' + #232, 8 ) ;

    fsTipoRel := ' '
  end
  else
  if fsNumVersao = '2000' then
  begin
    RetCmd := EnviaComando( ESC + #235 ) ;

    if pos(copy(RetCmd,7,1),'2345') > 0 then
    begin
      AguardaImpressao := True ;
      EnviaComando(ESC + #216, 8 ) ;
    end ;
  end
  else
  begin
    RetCmd := EnviaComando( ESC + #239 ) ;
    if pos(copy(RetCmd,8,1),'03') > 0 then
    begin
      AguardaImpressao := True ;
      EnviaComando(ESC + #212, 8 ) ;
    end ;
  end;
end;

procedure TACBrECFDaruma.ImpactoAgulhas( NivelForca : Integer = 2);
Var Cmd : AnsiString ;
begin
  if not fpMFD then // Somente se não for MFD
  begin
    if NivelForca > 2 then
       Cmd := '2'
    else
      Cmd := '0' ;

    EnviaComando(ESC + #228 + StringOfChar('X',13) + Cmd + StringOfChar('X',26) ) ;
  end;
end;

procedure TACBrECFDaruma.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : Char ;
begin
  fsNumCupom := '';

  Espera := 20 + (ReducaoFinal - ReducaoInicial) * 2 ;
  Flag   := 'x' ;
  if Simplificada then
     Flag := 'X' ;

  AguardaImpressao := True ;
  if fpMFD then
    EnviaComando( FS + 'F' + #233 + IfThen(Simplificada, '2', '3') + IntToStrZero(ReducaoInicial,6)+
                     IntToStrZero(ReducaoFinal  ,6), Espera )
  else if fsNumVersao = '2000' then
    EnviaComando( ESC + #251 + Flag + IntToStrZero(ReducaoInicial,6)+
                     IntToStrZero(ReducaoFinal  ,6), Espera )
  else
     EnviaComando( ESC + #209 + Flag + IntToStrZero(ReducaoInicial,6)+
                     IntToStrZero(ReducaoFinal  ,6), Espera ) ;
end;

procedure TACBrECFDaruma.LeituraMemoriaFiscal(DataInicial,
   DataFinal: TDateTime; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : Char ;
begin
  fsNumCupom := '';

  Espera := 20 + DaysBetween(DataInicial,DataFinal) * 2 ;
  Flag   := 'x' ;
  if Simplificada then
     Flag := 'X' ;

  AguardaImpressao := True ;
  if fpMFD then
    EnviaComando( FS + 'F' + #233 + IfThen(Simplificada, '2', '3') + FormatDateTime('ddmmyy',DataInicial) +
                     FormatDateTime('ddmmyy',DataFinal), Espera )
  else if fsNumVersao = '2000' then
     EnviaComando(ESC + #251 + Flag + FormatDateTime('ddmmyy',DataInicial)+
                      FormatDateTime('ddmmyy',DataFinal), Espera )
  else
     EnviaComando( ESC + #209 + Flag + FormatDateTime('ddmmyy',DataInicial)+
                         FormatDateTime('ddmmyy',DataFinal), Espera ) ;
end;

procedure TACBrECFDaruma.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas: TStringList; Simplificada : Boolean);
 Var Espera : Integer ;
     RetCmd : AnsiString ;
     Flag   : Char ;
begin
  fsNumCupom := '';

  Espera := 20 + (ReducaoFinal - ReducaoInicial) * 2  ;
  Flag   := 's' ;
  if Simplificada then
     Flag := 'S' ;

  fsEsperaFFCR := True ;
  if fpMFD then
    RetCmd :=  EnviaComando( FS + 'F' + #233 + IfThen(Simplificada, '0', '1') + IntToStrZero(ReducaoInicial,6)+
                     IntToStrZero(ReducaoFinal  ,6), Espera )
  else
    RetCmd := EnviaComando(ESC + #209 + Flag +
                            IntToStrZero(ReducaoInicial,6)+
                            IntToStrZero(ReducaoFinal  ,6), Espera ) ;

  RetCmd := RemoveStrings( RetCmd, fsComandosImpressao ) ;
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

procedure TACBrECFDaruma.LeituraMemoriaFiscalSerial(DataInicial,
   DataFinal: TDateTime; Linhas: TStringList; Simplificada : Boolean);
 Var Espera : Integer ;
     RetCmd : AnsiString ;
     Flag   : Char ;
begin
  fsNumCupom := '';

  Espera := 20 + DaysBetween(DataInicial,DataFinal) * 2;
  Flag   := 's' ;
  if Simplificada then
     Flag := 'S' ;

  fsEsperaFFCR := True;
  if fpMFD then
    RetCmd :=  EnviaComando( FS + 'F' + #233 + IfThen(Simplificada, '0', '1') + FormatDateTime('ddmmyy',DataInicial)+
                     FormatDateTime('ddmmyy',DataFinal), Espera )
  else
    RetCmd := EnviaComando(ESC + #209 + Flag +
                            FormatDateTime('ddmmyy',DataInicial)+
                            FormatDateTime('ddmmyy',DataFinal), Espera ) ;
  RetCmd := RemoveStrings( RetCmd, fsComandosImpressao ) ;
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

function TACBrECFDaruma.DocumentosToStr(Documentos : TACBrECFTipoDocumentoSet
  ) : String ;
begin
  if (Documentos - [docTodos]) = [] then
     Result := StringOfChar('1',18)
  else
   begin
     Result := '' ;
     Result := Result + IfThen(docRZ              in Documentos, '1', '0');
     Result := Result + IfThen(docLX              in Documentos, '1', '0');
     Result := Result + IfThen(docCF              in Documentos, '1', '0');
     Result := Result + IfThen(docCFBP            in Documentos, '1', '0');
     Result := Result + IfThen(docCupomAdicional  in Documentos, '1', '0');
     Result := Result + IfThen(docCFCancelamento  in Documentos, '1', '0');
     Result := Result + IfThen(docCCD             in Documentos, '1', '0');
     Result := Result + IfThen(docAdicionalCCD    in Documentos, '1', '0');
     Result := Result + IfThen(docSegViaCCD       in Documentos, '1', '0');
     Result := Result + IfThen(docReimpressaoCCD  in Documentos, '1', '0');
     Result := Result + IfThen(docEstornoCCD      in Documentos, '1', '0');
     Result := Result + IfThen(docCNF             in Documentos, '1', '0');
     Result := Result + IfThen(docCNFCancelamento in Documentos, '1', '0');
     Result := Result + IfThen(docSangria         in Documentos, '1', '0');
     Result := Result + IfThen(docSuprimento      in Documentos, '1', '0');
     Result := Result + IfThen(docEstornoPagto    in Documentos, '1', '0');
     Result := Result + IfThen(docRG              in Documentos, '1', '0');
     Result := Result + IfThen(docLMF             in Documentos, '1', '0');
   end ;

  Result := PadRight( Result, 31, '1') ;
end ;

procedure TACBrECFDaruma.LeituraMFDSerial(COOInicial, COOFinal: Integer;
  Linhas: TStringList; Documentos : TACBrECFTipoDocumentoSet);
 Var Espera : Integer;
     RetCmd : AnsiString ;
begin
  { O download da MFD é um processo bastante demorado por isso forcei um TimeOut
    maior. Dependendo da Faixa de COO's a ser baixada pode ser necessário aumantar
    ainda mais o TimeOut. Cfe. testes realizados, faixas de 100 COOs ainda são
    grandes demais p/ um TimeOut de 300 seg., aconselha-se fazer a leitura por
    faixas de 50 em 50 COOs ( Aprox. 220 COOs em 8min em uma FS600 V.1.03) }

  fsNumCupom := '';

  Espera := 20 + ((COOFinal - COOInicial) * 5) ;
  fsEsperaFFCR  :=  True ;

  RetCmd := EnviaComando( FS + 'R' + #201 + '024' +
                          IntToStrZero(COOInicial,6) +
                          IntToStrZero(COOFinal  ,6) + '12' +
                          DocumentosToStr(Documentos), Espera );

//WriteToTXT('d:\temp\mfd_ret.txt',RetCmd, False);
  RetCmd := RemoveStrings( RetCmd, fsComandosImpressao ) ;
//WriteToTXT('d:\temp\mfd_limpo.txt',RetCmd, False);
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

procedure TACBrECFDaruma.LeituraMFDSerial(DataInicial,
  DataFinal: TDateTime; Linhas: TStringList;
  Documentos : TACBrECFTipoDocumentoSet);
 Var Espera : Integer;
     RetCmd : AnsiString ;
begin
  fsNumCupom := '';

  Espera := 20 + ((DaysBetween(DataInicial,DataFinal)) * 30) ;
  fsEsperaFFCR := True ;
  
  RetCmd := EnviaComando( FS + 'R' + #201 + '024' +
                          FormatDateTime('ddmmyy',DataInicial)+
                          FormatDateTime('ddmmyy',DataFinal) + '11' +
                          DocumentosToStr(Documentos), Espera );

//WriteToTXT('d:\temp\mfd_ret.txt',RetCmd, False);
  RetCmd := RemoveStrings( RetCmd, fsComandosImpressao ) ;
//WriteToTXT('d:\temp\mfd_limpo.txt',RetCmd, False);
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

function TACBrECFDaruma.LimpaChr0(const AString: AnsiString): AnsiString;
begin
  // Substituindo #0 por ' ' //
  Result := StringReplace( AString, #0, ' ', [rfReplaceAll] ) ;
end ;

function TACBrECFDaruma.GetComprovantesNaoFiscaisVinculado: TACBrECFComprovantesNaoFiscais;
begin
  if not Assigned( fsCNFVinc ) then
     CarregaComprovantesNaoFiscais ;

  Result := fsCNFVinc ;
end;

function TACBrECFDaruma.AchaCNFVincDescricao(
  Descricao: String): TACBrECFComprovanteNaoFiscal;
var A : Integer ;
begin
  if not Assigned( fsCNFVinc ) then
     CarregaComprovantesNaoFiscais ;

  Descricao := Trim(UpperCase( Descricao )) ;
  Result    := nil ;

  with fsCNFVinc do
  begin
    For A := 0 to Count -1 do
      if Trim(UpperCase( Objects[A].Descricao )) = Descricao then
      begin
        Result := Objects[A] ;
        Break ;
      end ;
  end ;
end;

function TACBrECFDaruma.AchaCNFVincIndice( const Indice: String): TACBrECFComprovanteNaoFiscal;
var A : Integer ;
begin
  if not Assigned( fsCNFVinc ) then
     CarregaComprovantesNaoFiscais ;

  Result := nil ;
  with fsCNFVinc do
  begin
    For A := 0 to Count -1 do
      if Objects[A].Indice = Indice then
      begin
        Result := Objects[A] ;
        Break ;
      end ;
  end ;
end;

function TACBrECFDaruma.GetNumCRZ: String;
Var RetCmd : AnsiString ;
begin
  Result := '' ;

  if fpMFD then
    Result  :=  RetornaInfoECF('024')
  else if fsNumVersao = '2000' then
  begin
    RetCmd := EnviaComando( ESC + #237 ) ;
    if LeftStr(RetCmd, 1) = ':' then
      Result := copy(RetCmd,55,6) ;
  end
  else
    Result := copy(Ret244,42,4) ;
end;

function TACBrECFDaruma.GetGrandeTotal: Double;
Var RetCmd : AnsiString ;
begin
  Result:=0;

  if fpMFD then
    Result  :=  (StrToFloatDef(RetornaInfoECF('001'), 0)/100)
  else if fsNumVersao='2000' then
  begin
    RetCmd := EnviaComando( ESC + #235 ) ;
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd,41,18),0)/100 ;
  end
  else
  begin
    RetCmd := EnviaComando( ESC + #239 ) ;
    if LeftStr(RetCmd, 3) = ':' + ESC + #239 then
      Result := StrToFloatDef(copy(RetCmd,43,18),0)/100 ;
  end;

  Result := RoundTo( Result, -2);
end;

function TACBrECFDaruma.GetNumCOOInicial: String;
Var RetCmd : AnsiString ;
begin
  Result := '' ;

  if fpMFD then
    { a FS600 retorna o da Ultima Z, Pág 20 do Manual }    
    Result  :=  IntToStr(StrToInt(RetornaInfoECF('027')) + 1)
  else if fsNumVersao='2000' then
  begin
    RetCmd := EnviaComando( ESC + #237 ) ;
    if LeftStr(RetCmd, 1) = ':' then
      Result := copy(RetCmd,7,6) ;
  end
  else
    Result := copy(Ret244,4,6) ;
end;

function TACBrECFDaruma.GetVendaBruta: Double;
Var RetCmd : AnsiString ;
begin
  Result := 0 ;

  if fpMFD then
    Result  :=  (StrToFloatDef(RetornaInfoECF('002'),0)/100)
  else if fsNumVersao = '2000' then
  begin
    RetCmd := EnviaComando( ESC + #236 ) ;
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd,7,18),0)/100 ;
  end
  else
  begin
    RetCmd := EnviaComando( ESC + #240 ) ;
    if LeftStr(RetCmd, 3) = ':' + ESC + #240 then
      Result := StrToFloatDef(copy(RetCmd,4,18),0)/100 ;
  end;

  Result := RoundTo( ( GrandeTotal - Result), -2 );
end;

function TACBrECFDaruma.GetNumUltimoItem: Integer;
Var RetCmd : AnsiString ;
begin
  { Nota: não encontrado a leitura do ultimo item na FS345 }
  Result := 0 ;

  if fpMFD then
  begin
    if fsNumUltimoItem > 0 then
      Result := fsNumUltimoItem
    else
      Result := StrToIntDef(RetornaInfoECF('058'), 0)
  end
  else
  if (fsNumVersao = '2000') then
  begin
    RetCmd := EnviaComando( ESC + #235 ) ;

    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToIntDef(copy(RetCmd,14,3),0) ;
  end ;
end;

procedure TACBrECFDaruma.AbreNaoFiscal(CPF_CNPJ: String; Nome: String;
   Endereco: String);
begin
  fsNumCupom := '';
  fsSubTotal := 0;

  if fpMFD then
  begin
    EnviaComando( FS + 'F' + #219 + Trim(CPF_CNPJ) + cDELIMITADOR +
                                    Trim(Nome)     + cDELIMITADOR +
                                    Trim(Endereco) + cDELIMITADOR) ;
  end;

  fsRet244 := '' ;
end;

procedure TACBrECFDaruma.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString);
begin
  if fpMFD then
     EnviaComando( FS + 'F' + #220 + CodCNF +
                   IntToStrZero( Round(Valor*100),11) + StringOfchar('0',12) )
  else
     EnviaComando( ESC + #217 + CodCNF + StringOfChar('0',13) +
                   IntToStrZero( Round(Valor*100),12) +
                   LeftStr(Obs,40) + cDELIMITADOR ) ;
  fsRet244 := '' ;
end;

procedure TACBrECFDaruma.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
   MensagemRodape: AnsiString);
  Var A_D : Char ;
begin
  if DescontoAcrescimo > 0 then
     A_D := '3'
  else
     A_D := '1' ;

  DescontoAcrescimo := abs(DescontoAcrescimo) ;

  if fpMFD then
     EnviaComando( FS + 'F' + #223 + A_D +
                   IntToStrZero( Round(DescontoAcrescimo*100),12) ) ;
  fsRet244 := '' ;
end;

procedure TACBrECFDaruma.EfetuaPagamentoNaoFiscal(CodFormaPagto: String;
  Valor: Double; Observacao: AnsiString; ImprimeVinculado: Boolean);
begin
  if fpMFD then
     EnviaComando( FS + 'F' + #225 + CodFormaPagto +
                 IntToStrZero( Round( Valor * 100),12) +
                 LeftStr(Observacao,84) + cDELIMITADOR )
  else
     EfetuaPagamento(CodFormaPagto, Valor, Observacao, ImprimeVinculado);
  fsRet244 := '' ;
end;

procedure TACBrECFDaruma.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
var
  Obs: String;
begin
  if fpMFD then
  begin
    VerificarBmpTexto(IndiceBMP, Observacao);

    Obs := StringReplace(Observacao, #10, CR+LF, [rfReplaceAll]) ;
    Obs := LeftStr(Obs, 619);

    if IndiceBMP > 0 then
      Obs :=  ESC + 'B' + IntToStrZero(IndiceBMP, 1) + Obs;

    EnviaComando( FS + 'F' + #226 + Obs + cDELIMITADOR ) ;
  end;

  fsEmPagamento := False;   { Linha adicionada por Marciano Lizzoni }
  fsRet244      := '' ;
end;

procedure TACBrECFDaruma.CancelaNaoFiscal;
begin
  fsNumCupom := '';

  if fpMFD then
     EnviaComando( FS + 'F' + #229 )
  else
     CancelaCupom ;
  fsRet244 := '' ;
end;

procedure TACBrECFDaruma.CancelaItemNaoFiscal(const AItem: Integer);
begin
  EnviaComando( FS + 'F' + #221 + IntToStrZero(AItem, 3) );
end;

function TACBrECFDaruma.GetTotalAcrescimos: Double;
var
   RetCmd: AnsiString;
begin
   Result := 0;

  if fpMFD then
    Result := (StrToFloatDef(RetornaInfoECF('012'),0)/100)
  else if fsNumVersao = '2000' then
  begin
     //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #240 );
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd, 316, 14), 0) /100;
  end;

  Result := RoundTo( Result, -2);
end;

function TACBrECFDaruma.GetTotalCancelamentos: Double;
 var
   RetCmd: AnsiString;
begin
  Result := 0;

  if fpMFD then
    Result := (StrToFloatDef(RetornaInfoECF('013'),0)/100)
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #240 );
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd, 36, 14), 0) /100;
  end;

  Result := RoundTo( Result, -2);
end;

function TACBrECFDaruma.GetTotalDescontos: Double;
 var
   RetCmd: AnsiString;
begin
  Result := 0;

  if fpMFD then
    Result := (StrToFloatDef(RetornaInfoECF('011'),0)/100)
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #240 );
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd, 22, 14), 0) /100;
  end;

  Result := RoundTo( Result, -2);
end;

function TACBrECFDaruma.GetTotalTroco: Double;
begin
  if fpMFD then
     Result := StrToFloatDef(RetornaInfoECF('007'),0)/100
  else
     Result := StrToFloatDef(copy(Ret244,274,14),0)/100 ;

  Result := RoundTo( Result, -2);
end;

function TACBrECFDaruma.GetTotalAcrescimosISSQN: Double;
begin
  Result := 0;

  if fpMFD then
    Result := RoundTo( StrToFloatDef(RetornaInfoECF('015'),0)/100, -2) ;
end;

function TACBrECFDaruma.GetTotalCancelamentosISSQN: Double;
begin
  Result := 0;

  if fpMFD then
    Result := RoundTo( StrToFloatDef(RetornaInfoECF('016'),0)/100, -2) ;
end;

function TACBrECFDaruma.GetTotalDescontosISSQN: Double;
begin
  Result := 0;

  if fpMFD then
    Result := RoundTo( StrToFloatDef(RetornaInfoECF('014'),0)/100, -2) ;
end;


function TACBrECFDaruma.GetTotalSubstituicaoTributariaISSQN: Double;
begin
  Result := 0;

  if fpMFD then
  begin
    LerTotaisTotalizadoresNaoTributados;
    Result := SomaTotalizadorNaoTributadoIndice('FS');
  end;
end;

function TACBrECFDaruma.GetTotalIsencaoISSQN: Double;
begin
  Result := 0;

  if fpMFD then
  begin
    LerTotaisTotalizadoresNaoTributados;
    Result := SomaTotalizadorNaoTributadoIndice('IS');
  end;
end;


function TACBrECFDaruma.GetTotalNaoTributadoISSQN: Double;
begin
  Result := 0;

  if fpMFD then
  begin
    LerTotaisTotalizadoresNaoTributados;
    Result := SomaTotalizadorNaoTributadoIndice('NS');
  end;
end;

function TACBrECFDaruma.GetTotalAcrescimosOPNF: Double;
begin
  Result := 0;

  if fpMFD then
    Result := RoundTo( StrToFloatDef(RetornaInfoECF('018'),0)/100, -2) ;
end;

function TACBrECFDaruma.GetTotalCancelamentosOPNF: Double;
begin
  Result := 0;

  if fpMFD then
    Result := RoundTo( StrToFloatDef(RetornaInfoECF('019'),0)/100, -2) ;
end;

function TACBrECFDaruma.GetTotalDescontosOPNF: Double;
begin
  Result := 0;

  if fpMFD then
    Result := RoundTo( StrToFloatDef(RetornaInfoECF('017'),0)/100, -2) ;
end;

function TACBrECFDaruma.GetTotalIsencao: Double;
var
  RetCmd: AnsiString;
begin
  Result := 0;

  if fpMFD then
  begin
    LerTotaisTotalizadoresNaoTributados;
    Result := SomaTotalizadorNaoTributadoIndice('I');
  end
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #240 );
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd, 50, 14), 0) /100;
  end;

  Result := RoundTo( Result, -2);
end;


function TACBrECFDaruma.GetTotalNaoTributado: Double;
 var
    RetCmd: AnsiString;
begin
  Result := 0;

  if fpMFD then
  begin
    LerTotaisTotalizadoresNaoTributados;
    Result := SomaTotalizadorNaoTributadoIndice('N');
  end
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #240 );
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd, 64, 14), 0) /100;
  end;

  Result := RoundTo( Result, -2);
end;

function TACBrECFDaruma.GetTotalSubstituicaoTributaria: Double;
 var
    RetCmd: AnsiString;
begin
  Result := 0;

  if fpMFD then
  begin
    LerTotaisTotalizadoresNaoTributados;
    Result := SomaTotalizadorNaoTributadoIndice('F');
  end
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #240 );
    if LeftStr(RetCmd, 1) = ':' then
      Result := StrToFloatDef(copy(RetCmd, 78, 14), 0) /100;
  end;

  Result := RoundTo( Result, -2);
end;

function TACBrECFDaruma.GetCNPJ: String;
 var
   RetCmd: AnsiString;
begin
  Result := '';

  if fpMFD then
    Result := Trim(RetornaInfoECF('090'))
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #251 + '00' );
    if LeftStr(RetCmd, 1) = ':' then
      Result := copy(RetCmd, 7, 18);
  end;
end;

function TACBrECFDaruma.GetIE: String;
var
   RetCmd: AnsiString;
begin
  Result := '';

  if fpMFD then
    Result := Trim(RetornaInfoECF('091'))
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #251 + '00' );
    if LeftStr(RetCmd, 1) = ':' then
      Result := copy(RetCmd, 30, 15);
  end;
end;

function TACBrECFDaruma.GetIM: String;
var
   RetCmd: AnsiString;
begin
  Result := '';

  if fpMFD then
    Result := copy( RetornaInfoECF('092'), 06, 20)
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #251 + '00' );
    if LeftStr(RetCmd, 1) = ':' then
      Result := copy(RetCmd, 56, 15);
  end;
end;

function TACBrECFDaruma.GetCliche: AnsiString;
var
   RetCmd: AnsiString;
begin
  Result := '';

  if fpMFD then
    Result := Trim(RetornaInfoECF('132'))
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #232 );
    if LeftStr(RetCmd, 1) = ':' then
      Result := copy(RetCmd, 02, 144);
  end;
end;

function TACBrECFDaruma.GetUsuarioAtual: String;
var
   RetCmd: AnsiString;
   intFor: integer;
begin
  Result := '';

  if fpMFD then
    fsUsuarioAtual := Trim(RetornaInfoECF('094'))
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    if Length(fsUsuarioAtual) = 0 then
    begin
      for intFor := 1 to 50 do
      begin
        RetCmd := EnviaComando( ESC + #251 + FormatFloat('00', intFor) );
        if LeftStr(RetCmd, 1) = ':' then
        begin
          if copy(RetCmd, 02, 02) = '??' then
          begin
            fsUsuarioAtual := FormatFloat('00', intFor -1);
            Break;
          end;
        end;
      end;
    end;
  end;
  Result := fsUsuarioAtual;
end;

function TACBrECFDaruma.GetDataHoraSB: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
begin
  OldShortDateFormat := ShortDateFormat ;
  try
    if fpMFD then
    begin
      RetCmd  :=  RetornaInfoECF('085') ;

      ShortDateFormat := 'dd/mm/yyyy' ;
      result := StrToDate(copy(RetCmd,1,2)+ DateSeparator +
                          copy(RetCmd,3,2)+ DateSeparator +
                          copy(RetCmd,5,4)) ;

      Result := RecodeHour(  Result,StrToInt(copy(RetCmd, 9,2))) ;
      Result := RecodeMinute(Result,StrToInt(copy(RetCmd,11,2))) ;
      Result := RecodeSecond(Result,StrToInt(copy(RetCmd,13,2))) ;
    end
    else
    begin
      RetCmd := EnviaComando( ESC + #221 ) ;
      RetCmd := copy(RetCmd,Length(RetCmd)-12,12) ;

      ShortDateFormat := 'dd/mm/yy' ;
      result := StrToDate(copy(RetCmd,1,2)+ DateSeparator +
                          copy(RetCmd,3,2)+ DateSeparator +
                          copy(RetCmd,5,2)) ;

      Result := RecodeHour(  Result,StrToInt(copy(RetCmd, 7,2))) ;
      Result := RecodeMinute(Result,StrToInt(copy(RetCmd, 9,2))) ;
      Result := RecodeSecond(Result,StrToInt(copy(RetCmd,11,2))) ;
    end ;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;

end;

function TACBrECFDaruma.GetSubModeloECF: String;
begin
  if Trim(fsSubModeloECF) <> '' then
    Result := fsSubModeloECF
  else
  begin
    Result := Trim(RetornaInfoECF('81'));
    fsSubModeloECF := Result;
  end;
end;

function TACBrECFDaruma.GetDataMovimento: TDateTime;
 var
    RetCmd: AnsiString;
    OldShortDateFormat : String;
begin
  Result := 0;

  if fpMFD then
  begin
    RetCmd := RetornaInfoECF('070');

    // Daruma MFD retorna 01012000 quando não tem movimento
    if RetCmd <> '01012000' then
    begin
      OldShortDateFormat := ShortDateFormat;
      try
        ShortDateFormat := 'dd/mm/yyyy';
        Result := StrToDate( copy(RetCmd,1,2) + DateSeparator +
                             copy(RetCmd,3,2) + DateSeparator +
                             copy(RetCmd,5,4) );
      finally
        ShortDateFormat := OldShortDateFormat;
      end;
    end;
  end
  else if fsNumVersao = '2000' then
  begin
    Result := 0; // Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #250 );
    if LeftStr(RetCmd, 1) = ':' then
    begin
      OldShortDateFormat := ShortDateFormat;
      Try
        ShortDateFormat := 'dd/mm/yyyy';
        Try(* Data do movimento *)
          Result := StrToDate( copy(RetCmd,2,2) + DateSeparator +
                                    copy(RetCmd,4,2) + DateSeparator +
                                    copy(RetCmd,6,2) );
        Except
          Result := 0;
        end;
      finally
        ShortDateFormat := OldShortDateFormat;
      end;
    end;
  end;
end;

function TACBrECFDaruma.GetDataHoraUltimaReducaoZ : TDateTime ;
var
  RetCmd : String ;
begin
  if fpMFD then
  begin
    RetCmd := RetornaInfoECF('154');
    Result := StringToDateTime( copy(RetCmd, 1,2) + DateSeparator +
                                copy(RetCmd, 3,2) + DateSeparator +
                                copy(RetCmd, 5,4) + ' ' +
                                copy(RetCmd, 9,2) + TimeSeparator +
                                copy(RetCmd,11,2) + TimeSeparator +
                                copy(RetCmd,13,2),
                                'dd/mm/yyyy hh:nn:ss' ) ;
  end
  else
    Result := 0;
end ;

procedure TACBrECFDaruma.LerTotaisAliquota;
 var
    A: Integer;
    RetCmd: AnsiString;
begin
  CarregaAliquotas;

  if fpMFD then
  begin
    RetCmd := RetornaInfoECF('003');
    for A := 0 to fpAliquotas.Count-1 do
    begin
      fpAliquotas[A].Total := RoundTo( StrToFloatDef(Copy(RetCmd,(A*13)+1,13),0)
                                         / 100, -2 );
    end;
  end
  else if fsNumVersao = '2000' then
  begin
      // Falta Implementar
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := EnviaComando( ESC + #240 );
    RetCmd := Copy(RetCmd, 92, Length(RetCmd));
    for A := 0 to fpAliquotas.Count-1 do
    begin
      fpAliquotas[A].Total := RoundTo( StrToFloatDef(Copy(RetCmd,(A*14)+1,14),0)
                                         / 100, -2 );
    end;
  end;
end;

procedure TACBrECFDaruma.LerTotaisFormaPagamento;
var
   A: Integer;
   RetCmd, RetCmdAux,
   StrCNFVinc, StrCNF : AnsiString;
begin
  if not Assigned( fpFormasPagamentos ) then
    CarregaFormasPagamento;

  if not Assigned( fpComprovantesNaoFiscais ) then
    CarregaComprovantesNaoFiscais;

  fsRet244 := '' ;  { força a leitura do 244 }


  if fpMFD then
  begin
    for A := 0 to fpFormasPagamentos.Count-1 do
    begin
      RetCmd := EnviaComando( FS + 'R' + #201 + '009'+ IntToStrZero(A+1,2) );
      RetCmd := Copy(RetCmd, 6, Length(RetCmd));

      fpFormasPagamentos[A].Total := RoundTo( StrToFloatDef(Copy(RetCmd,1,13),0)
                                          / 100, -2 );
    end;
  end
  else if fsNumVersao = '2000' then
  begin
    RetCmd := EnviaComando( ESC + #237 );
    RetCmd := Copy(RetCmd, 163, Length(RetCmd));
    for A := 0 to fpAliquotas.Count-1 do
    begin
      fpFormasPagamentos[A].Total := RoundTo( StrToFloatDef(Copy(RetCmd,(A*14)+1,14),0)
                                         / 100, -2 );
    end;
  end
  else if StrToIntDef(fsNumVersao, -1) >= 345 then
  begin
    RetCmd := Ret244 ;
    RetCmd := Copy(RetCmd, 50, 224);
    for A := 0 to fpFormasPagamentos.Count-1 do
    begin
      fpFormasPagamentos[A].Total := RoundTo( StrToFloatDef(Copy(RetCmd,(A*14)+1,14),0)
                                         / 100, -2 );
    end;
  end;


  { ----- Calculando LerTotaisComprovanteNaoFiscal ----- }
   
  if fpMFD then
  begin
    for A := 0 to fpComprovantesNaoFiscais.Count -1 do
    begin
      RetCmd := EnviaComando( FS + 'R' + #201 + '010'+ IntToStrZero(A+1,2) );
      RetCmd := Copy(RetCmd, 6, Length(RetCmd));

      fpComprovantesNaoFiscais[A].Total   := RoundTo( StrToFloatDef(Copy(RetCmd,1,13),0)
                                          / 100, -2 );
    end;
      
    for A := 0 to fpComprovantesNaoFiscais.Count -1 do
    begin
      RetCmd := EnviaComando( FS + 'R' + #201 + '011'+ IntToStrZero(A+1,2) );
      RetCmd := Copy(RetCmd, 6, Length(RetCmd));

      fpComprovantesNaoFiscais[A].Contador   := StrToIntDef(Copy(RetCmd,1,4),0);
    end;
  end
  else if fsNumVersao = '2000' then
  begin
    //Falta Implementar
  end
  else if (StrToIntDef(fsNumVersao, -1) >= 345) then
  begin
    RetCmd := Ret244 ;

    (* Ler Comprovante não fiscal não vinculado *)
    StrCNF  := Copy(RetCmd, 288, 224);
    for A := 0 to fpComprovantesNaoFiscais.Count -1 do
    begin
      If Not fpComprovantesNaoFiscais[A].PermiteVinculado then
      begin
        RetCmdAux := Copy(StrCNF,(A*18)+1,18);

        fpComprovantesNaoFiscais[A].Contador:= StrToIntDef( Copy( RetCmdAux, 15, 4 ), 0);
        fpComprovantesNaoFiscais[A].Total   := RoundTo( StrToFloatDef( Copy( RetCmdAux, 1, 14 ), 0) / 100, -2 );
        end;
    end;

    (* Ler Comprovante não fiscal vinculado *)
    StrCNFVinc  := Copy(RetCmd, 618, 224);
    for A := 0 to fsCNFVinc.Count -1 do
    begin
      If fsCNFVinc[A].PermiteVinculado then
      begin
        RetCmdAux   := Copy(StrCNFVinc,(A*18)+1,18);

        fsCNFVinc[A].Contador:= StrToIntDef( Copy( RetCmdAux, 15, 4 ), 0);
        fsCNFVinc[A].Total   := RoundTo( StrToFloatDef( Copy( RetCmdAux, 1, 14 ), 0) / 100, -2 );
      end;
    end;
  end;
end;

procedure TACBrECFDaruma.LerTotaisComprovanteNaoFiscal;
begin
  LerTotaisFormaPagamento ;
end;

function TACBrECFDaruma.GetTotalNaoFiscal: Double;
var
   Cont        :  Integer;
   TotalCNFVinc,
   TotalCNF    :  Double;
begin
  LerTotaisComprovanteNaoFiscal;

  TotalCNF := 0 ;
  for Cont := 0 to fpComprovantesNaoFiscais.Count -1 do
  begin
    If Not fpComprovantesNaoFiscais[Cont].PermiteVinculado then
      TotalCNF := TotalCNF + RoundTo( fpComprovantesNaoFiscais[Cont].Total, -2);
  end;

  TotalCNFVinc := 0 ;
  for Cont := 0 to fsCNFVinc.Count -1 do
  begin
    If fsCNFVinc[cont].PermiteVinculado then
         TotalCNFVinc:= TotalCNFVinc + RoundTo( fsCNFVinc[Cont].Total, -2);
  end;

   Result := RoundTo( ( TotalCNFVinc + TotalCNF ), -2);
end;

function TACBrECFDaruma.GetRet244: AnsiString;
begin
  if fsRet244 = '' then
  begin
     fsRet244 := EnviaComando( ESC + #244 ) ;
     if LeftStr(fsRet244, 3) <> ':' + ESC + #244 then
        fsRet244 := '' ;
  end ;

  Result := fsRet244 ;
end;

procedure TACBrECFDaruma.IdentificaOperador(Nome: String);
begin
  if fpMFD then
    EnviaComando( FS + 'C' + #209 + PadRight(Nome,20) )
  else
    EnviaComando( ESC + #218 + 'O' + PadRight(Nome,20) );
end;

function TACBrECFDaruma.GetPAF: String;
begin
  Result := '' ;
  if fpMFD then
     Result := RetornaInfoECF('131');
end;

procedure TACBrECFDaruma.IdentificaPAF(NomeVersao, MD5 : String);
var
  Resp: Integer;
begin
  EnviaComando( FS + 'C' + #214 + PadRight(MD5,42) + PadRight(NomeVersao,42) );

  try
    LoadDLLFunctions;
    try
      ConfigurarDLL('');

      // gravar no registro para evitar a perda, algumas funções da dll leem dessas chaves
      Resp := xregAlterarValor_Daruma('ECF\MensagemApl1', MD5 );
      if Resp <> 1 then
         raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' ao chamar: '+sLineBreak+
         'xregAlterarValor_Daruma( "ECF\MensagemApl1",  "'+MD5+'" )') );

      Resp := xregAlterarValor_Daruma('ECF\MensagemApl2', NomeVersao );
      if Resp <> 1 then
         raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' ao chamar: '+sLineBreak+
         'xregAlterarValor_Daruma( "ECF\MensagemApl2",  "'+NomeVersao+'" )') );
    finally
      UnloadDLLFunctions;
    end;
  except
    // Exceção muda... pode falhar se não achar a DLL
  end;
end;

function TACBrECFDaruma.RetornaInfoECF(Registrador : String) : AnsiString ;
Var
  Indice , I: Integer ;
  OK: Boolean;
begin
  Result := '' ;
  
  if fpMFD then
  begin
    // Implementação pedida pela daruma apenas para facilitar a vida de alguns
    // desenvolvedores que necessitam de alguma informação que não esta
    // disponivel em propriedades do ACBrECF

    Indice := StrToIntDef(Registrador,0) ;
    if (Indice < 1) or (Indice > 578) then
      raise EACBrECFERRO.create( ACBrStr('Não existe nenhum Informação com o Registrador: '+Registrador+'('+IntToStr(Indice)+')')) ;

    Registrador := IntToStrZero(Indice, 3);

    I := 0;
    OK := False;
    while (not OK) and (I < 3) do
    begin
      Inc(I);
      Result := EnviaComando( FS + 'R' + #200 + Registrador);
      OK := (LeftStr(Result, 5) =  ':' + #200 + Registrador);
      if not OK then
        Sleep(200)
      else
        Result :=  Trim(LimpaChr0(Copy(Result, 6, Length(Result) - 6)));
    end;
  end;
end;

procedure TACBrECFDaruma.CortaPapel(const CorteParcial: Boolean);
Var
   RetCmd, FlagCorte : AnsiString ;
begin
  if not fpMFD then
     inherited CortaPapel
  else
   begin
     // Daruma TRAVA se enviarmos o comando de guilhotina e ela não existir //
     RetCmd    := RetornaInfoECF('113');
     FlagCorte := '' ;
   
     if (RetCmd >= '1') and (fsModeloDaruma <> fs700L) then    // Tem Guilhotina ? // verifico se o modelo permite o acionamento da guilhotina
      begin
        if fsModeloDaruma > fs700L then
           FlagCorte := ifthen(CorteParcial, #0, #1);

        EnviaComando( FS + 'N' + #202 + FlagCorte ) ;
        Sleep( 100 );
      end
     else
       inherited CortaPapel ;
   end ;
end;

procedure TACBrECFDaruma.Sangria(const Valor: Double; Obs: AnsiString;
  DescricaoCNF, DescricaoFPG: String; IndiceBMP: Integer );
var
  CmdBitmap: AnsiString;
begin
  fsNumCupom := '';
  VerificarBmpTexto(IndiceBMP, Obs);

  if fpMFD then
  begin
    if IndiceBMP > 0 then
      CmdBitmap := ESC + 'B' + IntToStr(IndiceBMP)
    else
      CmdBitmap := EmptyStr;

    EnviaComando( FS + 'F' + #227 +
      IntToStrZero(Round(Valor * 100), 11) + LeftStr( CmdBitmap + Obs, 619) +
      cDELIMITADOR );
  end
  else
    Inherited Sangria(Valor, Obs, DescricaoCNF, DescricaoFPG, IndiceBMP);
end;

procedure TACBrECFDaruma.Suprimento(const Valor: Double; Obs: AnsiString;
  DescricaoCNF, DescricaoFPG: String; IndiceBMP: Integer );
var
  CmdBitmap: AnsiString;
begin
  fsNumCupom := '';
  VerificarBmpTexto(IndiceBMP, Obs);

  if fpMFD then
  begin
    if IndiceBMP > 0 then
      CmdBitmap := ESC + 'B' + IntToStr(IndiceBMP)
    else
      CmdBitmap := EmptyStr;

    EnviaComando( FS + 'F' + #236 +
      IntToStrZero(Round(Valor * 100), 11) + LeftStr( CmdBitmap + Obs, 619) +
      cDELIMITADOR );
  end
  else
     Inherited Suprimento(Valor, Obs, DescricaoCNF, DescricaoFPG, IndiceBMP);
end;

procedure TACBrECFDaruma.NaoFiscalCompleto(CodCNF: String; Valor: Double;
  CodFormaPagto: String; Obs: AnsiString; IndiceBMP : Integer);
begin
  fsNumCupom := '';
  VerificarBmpTexto(IndiceBMP, Obs);

  { Chama rotinas da classe Pai (fpOwner) para atualizar os Memos }
  with TACBrECF(fpOwner) do
  begin
     AbreNaoFiscal ;
     try
        RegistraItemNaoFiscal(CodCNF, Valor);
        try
           SubtotalizaNaoFiscal(0);
           EfetuaPagamentoNaoFiscal(CodFormaPagto, Valor );
        except
        end ;
        FechaNaoFiscal( Obs, IndiceBMP );
     except
        try
           CancelaNaoFiscal
        except
        end;

        raise ;
     end ;
  end ;
end;

procedure TACBrECFDaruma.ComutaOnLine;
begin
  if fpMFD then
     EnviaComando(GS + BS {+ BELL});
end;

function TACBrECFDaruma.DecodificaTexto(Operacao: Char; Texto: String;
  var Resposta: String): Boolean;
Var
  RetCmd : AnsiString ;
begin
   //Result := False;
   if (fpMFD) then
   begin
      if ( (fsModeloDaruma in [fs600, fs600USB]) and (StrToIntDef(fsNumVersao, -1) > 10400) ) or
         ( (fsModeloDaruma in [fs700L, fs700H, fs700M]) and (StrToIntDef(fsNumVersao, -1) > 10000) ) then
       begin
         RetCmd   := EnviaComando(FS + 'F' + #244 + Operacao + Texto + cDELIMITADOR, 10);
         Resposta := copy(RetCmd, 10, Length(RetCmd) - 12);
         Result   := True;
         if (Operacao = 'V') then
            if Resposta = '0' then
               Result := False;
       end
      else
         raise EACBrECFERRO.Create( ACBrStr('Versão do Firmeware da Impressora não suporta este comando ! ') );
   end
   else
      raise EACBrECFERRO.Create( ACBrStr('A Impressora não suporta este comando ! ') );
end;


function TACBrECFDaruma.GetDadosUltimaReducaoZ: String;
Var RetCmd, S, SS : AnsiString ;
    I :Integer;
    AliqZ : TACBrECFAliquota ;
    CNFZ  : TACBrECFComprovanteNaoFiscal ;
    RGZ   : TACBrECFRelatorioGerencial ;
    GTInicial :Double;
    DHUltZ : TDateTime ;
begin
//Comando 140 - Retorno:1164 caracteres
//                         INICIO    TAM
//Data do Movimento 8     ( 1         08 )
//Grande Total 18         ( 9         18 )
//Grande Total Inicial 18 ( 27        18 )
//Descontos ICMS 14       ( 45        14 )
//Descontos ISS 14        ( 59        14 )
//Cancelamentos ICMS 14   ( 73        14 )
//Cancelamentos ISS 14    ( 87        14 )
//Acréscimos ICMS 14      ( 101       14 )
//Acréscimos ISS 14       ( 115       14 )
//Tributados ICMS/ISS 224 ( 129       224)
//F1 ICMS 14              ( 353       14 )
//F2 ICMS 14              ( 367       14 )
//I1 ICMS 14              ( 381       14 )
//I2 ICMS 14              ( 395       14 )
//N1 ICMS 14              ( 409       14 )
//N2 ICMS 14              ( 423       14 )
//F1 ISS 14               ( 437       14 )
//F2 ISS 14               ( 451       14 )
//I1 ISS 14               ( 465       14 )
//I2 ISS 14               ( 479       14 )
//N1 ISS 14               ( 493       14 )
//N2 ISS 14               ( 507       14 )
//Totalizadores NF 280    ( 521       280)
//Descontos NF 14         ( 801       14 )
//Cancelamentos NF 14     ( 815       14 )
//Acréscimos NF 14        ( 829       14 )
//Alíquotas 80            ( 843       80 )
//CRO 4                   ( 923       04 )
//CRZ 4                   ( 927       04 )
//CRZ Restante 4          ( 931       04 )
//COO 6                   ( 935       06 )
//GNF 6                   ( 941       06 )
//CCF 6                   ( 947       06 )
//CVC 6                   ( 953       06 )
//GRG 6                   ( 959       06 )
//CFD 6                   ( 965       06 )
//CBP 6                   ( 971       06 )
//NFC 4                   ( 977       04 )
//CMV 4                   ( 981       04 )
//CFC 4                   ( 985       04 )
//CNC 4                   ( 989       04 )
//CBC 4                   ( 993       04 )
//NCN 4                   ( 997       04 )
//CDC 4                   ( 1001      04 )
//CON 80                  ( 1005      80 )
//CER 80                  ( 1085      80 )
  if not fpMFD then
  begin
    Result := '';
    exit ;
  end;

  // Zerar variaveis e inicializa Dados do ECF //
  InitDadosUltimaReducaoZ;

  if not Assigned( fpAliquotas ) then
    CarregaAliquotas ;

  if not Assigned( fpComprovantesNaoFiscais ) then
    CarregaComprovantesNaoFiscais ;

  if not Assigned( fpRelatoriosGerenciais ) then
    CarregaRelatoriosGerenciais ;

  with TACBrECF(fpOwner) do
  begin
    DHUltZ := DataHoraUltimaReducaoZ;
  end;

  RetCmd := RetornaInfoECF('140');

  { Alimenta a class com os dados atuais do ECF }
  with fpDadosReducaoZClass do
  begin
    { REDUÇÃO Z }
    DataHoraEmissao := DHUltZ;
    DataDoMovimento := StringToDateTimeDef( copy(RetCmd,1,2)+DateSeparator+
                                            copy(RetCmd,3,2)+DateSeparator+
                                            copy(RetCmd,7,2), 0, 'dd/mm/yy' );

    { TOTALIZADORES }
    ValorGrandeTotal  := RoundTo( StrToFloatDef( copy(RetCmd,  9,18),0) / 100, -2 ) ;
    GTInicial         := RoundTo( StrToFloatDef( copy(RetCmd, 27,18),0) / 100, -2 ) ;
    DescontoICMS      := RoundTo( StrToFloatDef( copy(RetCmd, 45,14),0) / 100, -2 ) ;
    DescontoISSQN     := RoundTo( StrToFloatDef( copy(RetCmd, 59,14),0) / 100, -2 ) ;
    CancelamentoICMS  := RoundTo( StrToFloatDef( copy(RetCmd, 73,14),0) / 100, -2 ) ;
    CancelamentoISSQN := RoundTo( StrToFloatDef( copy(RetCmd, 87,14),0) / 100, -2 ) ;
    AcrescimoICMS     := RoundTo( StrToFloatDef( copy(RetCmd,101,14),0) / 100, -2 ) ;
    AcrescimoISSQN    := RoundTo( StrToFloatDef( copy(RetCmd,115,14),0) / 100, -2 ) ;
    DescontoOPNF      := RoundTo( StrToFloatDef( copy(RetCmd,801,14),0) / 100, -2 ) ;
    CancelamentoOPNF  := RoundTo( StrToFloatDef( copy(RetCmd,815,14),0) / 100, -2 ) ;
    AcrescimoOPNF     := RoundTo( StrToFloatDef( copy(RetCmd,829,14),0) / 100, -2 ) ;
    ValorVendaBruta   := RoundTo( ValorGrandeTotal - GTInicial, -2 ) ;

    { CONTADORES }
    CRO := StrToIntDef(Copy(RetCmd,923, 4),0);
    CRZ := StrToIntDef(Copy(RetCmd,927, 4),0);
    COO := StrToIntDef(copy(RetCmd,935, 6),0);
    GNF := StrToIntDef(Copy(RetCmd,941, 6),0);
    CCF := StrToIntDef(Copy(RetCmd,947, 6),0);
    GRG := StrToIntDef(Copy(RetCmd,959, 6),0);
    CFD := StrToIntDef(Copy(RetCmd,965, 6),0);
    CFC := StrToIntDef(Copy(RetCmd,985, 4),0);
    NCN := StrToIntDef(Copy(RetCmd,997, 4),0);
    CDC := StrToIntDef(Copy(RetCmd,1001,4),0);

    {Copiando objetos de ICMS e ISS}
    S := Copy(RetCmd,129,224);  // 16 * 14
    for I := 0 to fpAliquotas.Count - 1 do
    begin
      AliqZ := TACBrECFAliquota.Create ;
      AliqZ.Assign( fpAliquotas[I] );
      AliqZ.Total := RoundTo(StrToFloatDef( copy(S,(I*14)+1,14),0) / 100, -2) ;

      AdicionaAliquota( AliqZ );
    end;

    { ICMS }
    SubstituicaoTributariaICMS  := RoundTo( StrToFloatDef(Copy(RetCmd,353,14),0)/100,-2 ) +
                                   RoundTo( StrToFloatDef(Copy(RetCmd,367,14),0)/100,-2 ) ;
    IsentoICMS                  := RoundTo( StrToFloatDef(Copy(RetCmd,381,14),0)/100,-2 ) +
                                   RoundTo( StrToFloatDef(Copy(RetCmd,395,14),0)/100,-2 ) ;
    NaoTributadoICMS            := RoundTo( StrToFloatDef(Copy(RetCmd,409,14),0)/100,-2 ) +
                                   RoundTo( StrToFloatDef(Copy(RetCmd,423,14),0)/100,-2 ) ;
    { ISSQN }
    SubstituicaoTributariaISSQN := RoundTo( StrToFloatDef(Copy(RetCmd,437,14),0)/100,-2 ) +
                                   RoundTo( StrToFloatDef(Copy(RetCmd,451,14),0)/100,-2 ) ;
    IsentoISSQN                 := RoundTo( StrToFloatDef(Copy(RetCmd,465,14),0)/100,-2 ) +
                                   RoundTo( StrToFloatDef(Copy(RetCmd,479,14),0)/100,-2 ) ;
    NaoTributadoISSQN           := RoundTo( StrToFloatDef(Copy(RetCmd,493,14),0)/100,-2 ) +
                                   RoundTo( StrToFloatDef(Copy(RetCmd,507,14),0)/100,-2 ) ;

    { TOTALIZADORES NÃO FISCAIS }
    S  := Copy(RetCmd,521,280);   // 20 * 14
    SS := Copy(RetCmd,1005,80);   // 20 * 4
    for I := 0 to fpComprovantesNaoFiscais.Count - 1 do
    begin
      CNFZ := TACBrECFComprovanteNaoFiscal.Create ;
      CNFZ.Assign( fpComprovantesNaoFiscais[I] );
      CNFZ.Total    := RoundTo(StrToFloatDef( copy(S,(I*14)+1,14),0) / 100, -2 ) ;
      CNFZ.Contador := StrToIntDef( copy(SS,(I*4)+1,4), 0);

      TotalizadoresNaoFiscais.Add( CNFZ ) ;
    end;

    { RELATÓRIO GERENCIAL }
    S := copy(RetCmd,1085,80) ; // 20 * 4
    For I := 0 to fpRelatoriosGerenciais.Count-1 do
    begin
      RGZ := TACBrECFRelatorioGerencial.Create ;
      RGZ.Assign( fpRelatoriosGerenciais[I] );
      RGZ.Contador := StrToIntDef(copy(S,(I*4)+1,4), 0) ;

      RelatorioGerencial.Add( RGZ ) ;
    end ;

    CalculaValoresVirtuais;
    Result := MontaDadosReducaoZ;
  end;
end ;

procedure TACBrECFDaruma.SetDecimaisPreco(AValue: Integer);
begin
  { Daruma não permite mudar Decimais }
end;

procedure TACBrECFDaruma.SetDecimaisQtd(AValue: Integer);
begin
  { Daruma não permite mudar Decimais }
end;

procedure TACBrECFDaruma.LoadDLLFunctions;

  procedure DarumaFunctionDetect( FuncName: String; var LibPointer: Pointer ) ;
  var
    sLibName: string;
  begin
    if not Assigned( LibPointer )  then
    begin
      // Verifica se existe o caminho das DLLs
      if Length(PathDLL) > 0 then
        sLibName := PathWithDelim(PathDLL);

      // Concatena o caminho se exitir mais o nome da DLL.
      sLibName := sLibName + cLIB_Daruma;

      if not FunctionDetect( sLibName, FuncName, LibPointer) then
      begin
        LibPointer := NIL ;
        raise EACBrECFERRO.Create( ACBrStr( 'Erro ao carregar a função: '+FuncName+' de: '+cLIB_Daruma ) ) ;
      end ;
    end ;
  end ;

begin
  DarumaFunctionDetect('eCarregarBitmapPromocional_ECF_Daruma',@xeCarregarBitmapPromocional_ECF_Daruma);
  DarumaFunctionDetect('eDefinirModoRegistro_Daruma', @xeDefinirModoRegistro_Daruma);
  DarumaFunctionDetect('eDefinirProduto', @xeDefinirProduto);
  DarumaFunctionDetect('regAlterarValor_Daruma', @xregAlterarValor_Daruma);
  DarumaFunctionDetect('rGerarEspelhoMFD_ECF_Daruma', @xrGerarEspelhoMFD_ECF_Daruma);
  DarumaFunctionDetect('rGerarRelatorio_ECF_Daruma', @xrGerarRelatorio_ECF_Daruma);
  DarumaFunctionDetect('rGerarRelatorioOffline_ECF_Daruma', @xrGerarRelatorioOffline_ECF_Daruma);
  DarumaFunctionDetect('rEfetuarDownloadMF_ECF_Daruma', @xrEfetuarDownloadMF_ECF_Daruma);
  DarumaFunctionDetect('rEfetuarDownloadMFD_ECF_Daruma', @xrEfetuarDownloadMFD_ECF_Daruma);
end;

procedure TACBrECFDaruma.UnloadDLLFunctions;
var
  sLibName: String;
begin
  // Verifica se existe o caminho das DLLs
  if Length(PathDLL) > 0 then
    sLibName := PathWithDelim(PathDLL);

  // Concatena o caminho se exitir mais o nome da DLL.
  sLibName := sLibName + cLIB_Daruma;

  UnLoadLibrary(sLibName);

  xeCarregarBitmapPromocional_ECF_Daruma := Nil;
  xeDefinirModoRegistro_Daruma           := Nil;
  xeDefinirProduto                       := Nil;
  xregAlterarValor_Daruma                := Nil;
  xrGerarEspelhoMFD_ECF_Daruma           := Nil;
  xrGerarRelatorio_ECF_Daruma            := Nil;
  xrGerarRelatorioOffline_ECF_Daruma     := Nil;

  // Sleep(300) evita falhas na re-ativação do ECF pelo ACBrECF
  Sleep(300);
end;

procedure TACBrECFDaruma.ConfigurarDLL(Path : AnsiString );
Var
  Resp: Integer ;
  {$IFDEF LINUX}
   ComNr : Integer ;
  {$ENDIF}
  Porta, Velocidade : AnsiString ;
begin
  if Trim(Path) = '' then
    Path := ApplicationPath;

  Porta := fpDevice.Porta ;
  {$IFDEF LINUX}
   if pos('/DEV/TTYS', uppercase(Porta)) = 1 then
    begin
      ComNr := StrToIntdef(copy(Porta, 10, Length(Porta) - 9), -1) + 1;
      Porta := 'COM'+IntToStr(ComNr);
    end
  else if pos('/DEV/TTYUSB', uppercase(Porta)) = 1 then
    begin
      ComNr := StrToIntdef(copy(Porta, 12, Length(Porta) - 11), -1);
      Porta := 'COM'+IntToStr(10+ComNr);
    end ;
  {$ENDIF}

  Velocidade := IntToStr(fpDevice.Baud) ;

  // configurar a daruma para gravar somente no XML
  Resp := xeDefinirModoRegistro_Daruma('2');
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
       'ao chamar: '+sLineBreak+
       'xeDefinirModoRegistro_Daruma( "2" )') );

  // Configurações gerais de funcionamento da DLL
  Resp := xregAlterarValor_Daruma( 'START\Produto', 'ECF' );
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
       'ao chamar: '+sLineBreak+
       'xregAlterarValor_Daruma( "START\Produto", "ECF" ) ') );

  Resp := xregAlterarValor_Daruma( 'ECF\ControleAutomatico', '1' );
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
       'ao chamar: '+sLineBreak+
       'xregAlterarValor_Daruma( "ECF\ControleAutomatico", "1" ) ') );

  Resp := xregAlterarValor_Daruma( 'ECF\PortaSerial', Porta );
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
       'ao chamar: '+sLineBreak+
       'xregAlterarValor_Daruma( "ECF\PortaSerial", "'+Porta+'" ) ') );

  if TACBrECF(fpOwner).Modelo = ecfEscECF then 
  begin
    Resp := xregAlterarValor_Daruma( 'ECF\SCU\Habilitar', '1' );
    if Resp <> 1 then
       raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
         'ao chamar: '+sLineBreak+
         'xregAlterarValor_Daruma( "ECF\SCU\Habilitar", "1" ) ') );
  end;

  Resp := xregAlterarValor_Daruma( 'ECF\Velocidade', Velocidade );
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
       'ao chamar: '+sLineBreak+
       'xregAlterarValor_Daruma( "ECF\Velocidade", "'+Velocidade+'" ) ') );

  Resp := xregAlterarValor_Daruma( 'START\LocalArquivos', Path );
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
       'ao chamar:'+sLineBreak+
       'xregAlterarValor_Daruma( "START\LocalArquivos",  "'+Path+'" ) ') );

  Resp := xregAlterarValor_Daruma( 'START\LocalArquivosRelatorios', Path );
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp)+sLineBreak+
       'ao chamar:'+sLineBreak+
       'xregAlterarValor_Daruma( "START\LocalArquivos", "'+Path+'" ) ') );

end;

function TACBrECFDaruma.GetDescricaoErroDLL(const ACodigo: Integer): String;
begin
  case ACodigo of
     0:   Result := 'Erro durante a execução.';
     1:   Result := 'Operação realizada com sucesso.';
    -1:   Result := 'Erro do Método.';
    -2:   Result := 'Parâmetro incorreto.';
    -3:   Result := 'Alíquota (Situação tributária) não programada.';
    -4:   Result := 'Chave do Registry não encontrada.';
    -5:   Result := 'Erro ao Abrir a porta de Comunicação.';
    -6:   Result := 'Impressora Desligada.';
    -7:   Result := 'Erro no Número do Banco.';
    -8:   Result := 'Erro ao Gravar as informações no arquivo de Status ou de Retorno de Info.';
    -9:   Result := 'Erro ao Fechar a porta de Comunicação.';
    -10:  Result := 'O ECF não tem a forma de pagamento e não permite cadastrar esta forma.';
    -12:  Result := 'A função executou o comando porém o ECF sinalizou Erro, chame a rStatusUltimoCmdInt_ECF_Daruma para identificar o Erro.';
    -24:  Result := 'Forma de Pagamento não Programada.';
    -25:  Result := 'Totalizador nao ECF Não Vinculado não Programado.';
    -27:  Result := 'Foi Detectado Erro ou Warning na Impressora.';
    -28:  Result := 'Time-Out.';
    -40:  Result := 'Tag XML Inválida.';
    -50:  Result := 'Problemas ao Criar Chave no Registry.';
    -51:  Result := 'Erro ao Gravar LOG.';
    -52:  Result := 'Erro ao abrir arquivo.';
    -53:  Result := 'Fim de arquivo.';
    -60:  Result := 'Erro na tag de formatação DHTML.';
    -90:  Result := 'Erro Configurar a Porta de Comunicação.';
    -99:  Result := 'Parâmetro inválido ou ponteiro nulo de parâmetro.';
    -101: Result := 'Erro ao LER ou ESCREVER arquivo.';
    -102: Result := 'Erro ao LER ou ESCREVER arquivo.';
    {$IFDEF LINUX}
     -103: Result := 'Não foram encontradas as bibliotecas auxiliares (liblebin.so e libLeituraMFDBin.so)';
    {$ELSE}
     -103: Result := 'Não foram encontradas as DLLs auxiliares (lebin.dll e LeituraMFDBin.dll)';
    {$ENDIF}
    -104: Result := 'Data informada é inferior ao primeiro documento emitido';
    -105: Result := 'Data informada é maior que a ultima redução Z impressa';
    -106: Result := 'Nao possui movimento fiscal';
    -107: Result := 'Porta de comunicação ocupada';
    -110: Result := 'Indica que o GT foi atualizado no arquivo de registro do PAF';
    -112: Result := 'O numero de serie ja existe no arquivo do PAF';
    -113: Result := 'ECF conectado nao cadastrado no arquivo do PAF';
    -114: Result := 'MFD Danificada';
    -115: Result := 'Erro ao abrir arquivos .idx/.dat/.mf';
    -116: Result := 'Intervalo solicitado não é válido';
    -117: Result := 'Impressora não identificada durante download dos binários';
    -118: Result := 'Erro ao abrir porta serial';
    -119: Result := 'Leitura dos binários abortada';
  else
    Result := 'Erro desconhecido.';
  end;
end;

procedure TACBrECFDaruma.EspelhoMFD_DLL(COOInicial, COOFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
var
  Resp: Integer ;
  Tipo, CooIni, CooFim, DirDest, PathDest: AnsiString ;
  OldAtivo: Boolean ;
begin
  OldAtivo := Ativo;
  try
    DirDest  := IncludeTrailingPathDelimiter(ExtractFilePath(NomeArquivo));
    PathDest := DirDest + ARQ_MFD_DLL;

    LoadDLLFunctions;
    ConfigurarDLL(DirDest);

    Ativo  := False;
    Tipo   := '2';
    CooIni := IntToStrZero(COOInicial, 6);
    CooFim := IntToStrZero(COOFinal,   6);

    Resp := xrGerarEspelhoMFD_ECF_Daruma(Tipo, CooIni, CooFim);
    if (Resp <> 1) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar rGerarEspelhoMFD_ECF_Daruma.'+sLineBreak+
                                       'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;

    if not FileExists( PathDest ) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de rGerarEspelhoMFD_ECF_Daruma.'+sLineBreak+
                                       'Arquivo: "'+ ARQ_MFD_DLL +'" não gerado' )) ;

    if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
      CopyFileTo(PathDest, NomeArquivo) ;
  finally
    UnloadDLLFunctions;
    Ativo := OldAtivo;

    if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
      SysUtils.DeleteFile(PathDest);
  end;
end;

procedure TACBrECFDaruma.EspelhoMFD_DLL(DataInicial, DataFinal: TDateTime;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
var
  Resp: Integer ;
  Tipo, DiaIni, DiaFim, DirDest, PathDest: AnsiString ;
  OldAtivo: Boolean ;
begin
  OldAtivo := Ativo;
  try
    DirDest  := IncludeTrailingPathDelimiter(ExtractFilePath(NomeArquivo));
    PathDest := DirDest + ARQ_MFD_DLL;

    LoadDLLFunctions;
    ConfigurarDLL(DirDest);

    Ativo  := False;
    Tipo   := '1';
    DiaIni := FormatDateTime('ddmmyy', DataInicial);
    DiaFim := FormatDateTime('ddmmyy', DataFinal);

    Resp := xrGerarEspelhoMFD_ECF_Daruma(Tipo, DiaIni, DiaFim );
    if (Resp <> 1) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar rGerarEspelhoMFD_ECF_Daruma.'+sLineBreak+
                                        'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;


    if not FileExists(PathDest) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de rGerarEspelhoMFD_ECF_Daruma.'+sLineBreak+
                              'Arquivo: "'+ ARQ_MFD_DLL +'" não gerado' )) ;

    if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
      CopyFileTo(PathDest, NomeArquivo);
  finally
    UnloadDLLFunctions;
    Ativo := OldAtivo;

    if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
      SysUtils.DeleteFile(PathDest);
  end;
end;

procedure TACBrECFDaruma.ArquivoMFD_Binario_DLL(Tipo: TACBrECFTipoDownloadMFD;
  const NomeArquivo: AnsiString; StrInicial, StrFinal: AnsiString);
var
  Resp: Integer;
  DirDest, ArqDest, TipoDaruma: AnsiString;
  OldAtivo: Boolean;
begin
  OldAtivo := Ativo;
  DirDest  := ExtractFilePath( NomeArquivo );
  ArqDest  := ExtractFileName( NomeArquivo );

  case Tipo of
    tdmfdData : TipoDaruma := 'DATAM';
    tdmfdCOO  : TipoDaruma := 'COO';
  else
    TipoDaruma := 'COO';
    StrInicial := '000001';
    StrFinal   := '999999';
  end;

  LoadDLLFunctions;
  ConfigurarDLL(DirDest);

  Ativo := False;
  try
     SysUtils.DeleteFile( NomeArquivo );

     GravaLog( '   xrEfetuarDownloadMFD_ECF_Daruma' );
     Resp := xrEfetuarDownloadMFD_ECF_Daruma( TipoDaruma, StrInicial, StrFinal, ArqDest ) ;

     if (Resp <> 1) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar rEfetuarDownloadMFD_ECF_Daruma.'+sLineBreak+
                                            'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;
  finally
     UnloadDLLFunctions;
     Ativo := OldAtivo;
  end;
end;

procedure TACBrECFDaruma.ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString);
var
  Resp: Integer;
  DirDest: AnsiString;
  ArqDest: AnsiString;
  OldAtivo: Boolean;
begin
  OldAtivo := Ativo;
  DirDest  := ExtractFilePath( NomeArquivo );
  ArqDest  := ExtractFileName( NomeArquivo );

  LoadDLLFunctions;
  ConfigurarDLL(DirDest);

  Ativo := False;
  try
     SysUtils.DeleteFile( NomeArquivo );

     GravaLog( '   xrEfetuarDownloadMF_ECF_Daruma' );
     Resp := xrEfetuarDownloadMF_ECF_Daruma( ArqDest ) ;

     if (Resp <> 1) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar rEfetuarDownloadMF_ECF_Daruma.'+sLineBreak+
                                            'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;
  finally
     UnloadDLLFunctions;
     Ativo := OldAtivo;
  end;
end;

procedure TACBrECFDaruma.ArquivoMFD_DLL(ContInicial, ContFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD;
  TipoContador: TACBrECFTipoContador);
var
  Resp: Integer ;
  NomeArq, Relatorio, Tipo, Inicio, Fim, DirDest, PathDest: AnsiString ;
  OldAtivo, OnLine: Boolean ;
begin
  OldAtivo := Ativo;
  try
    if Finalidade in [finSintegra, finSPED] then
      raise EACBrECFERRO.Create(ACBrStr('Finalidades SINTEGRA e SPED somente podem ser utilizadas por DATA DE MOVIMENTO'));

    case Finalidade of
      finMF: Relatorio := 'MF';
      finMFD: Relatorio := 'MFD';
      finTDM: Relatorio := 'TDM';
      finNFP: Relatorio := 'NFP';
      finNFPTDM: Relatorio := 'NFPTDM';
    else
      raise EACBrECFERRO.Create(ACBrStr('Finalidade não reconhecida, finalidades válidas: MF, MFD, TDM, NFP, NFPTDM'));
    end;

    case TipoContador of
      tpcCRZ: Tipo := 'CRZ';
      tpcCOO: Tipo := 'COO';
    else
      raise EACBrECFERRO.Create(ACBrStr('Tipo de contador desconhecido, tipos válidos: CRZ, COO'));
    end;

    NomeArq  := 'ATO_' + Relatorio + '_' + Tipo + '.TXT';
    DirDest  := IncludeTrailingPathDelimiter(ExtractFilePath(NomeArquivo));
    PathDest := DirDest + NomeArq;

    LoadDLLFunctions;
    ConfigurarDLL(DirDest);

    Ativo  := False;
    Inicio := IntToStrZero(ContInicial, 6);
    Fim    := IntToStrZero(ContFinal,   6);
    OnLine := (OldAtivo) or ((TACBrECF(fpOwner).Modelo = ecfEscECF) and (TACBrECF(fpOwner).Ativo));

    if OnLine then
    begin
      Resp := xrGerarRelatorio_ECF_Daruma(Relatorio, Tipo, Inicio, Fim);
      if (Resp <> 1) then
        raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar xrGerarRelatorio_ECF_Daruma.'+sLineBreak+
                                         'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;
    end
    else
    begin
      Resp := xrGerarRelatorioOffline_ECF_Daruma(Relatorio, Tipo, Inicio, Fim,
                                                 DirDest + 'Daruma.mf',
                                                 DirDest + 'Daruma.mfd',
                                                 DirDest + 'Daruma.inf');
      if (Resp <> 1) then
        raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar rGerarRelatorioOffline_ECF_Daruma.'+sLineBreak+
                                         'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;
    end;

    if not FileExists( PathDest ) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de xrGerarRelatorio_ECF_Daruma.'+sLineBreak+
                                       'Cod: '+ IntToStr(Resp) + ' ' + GetDescricaoErroDLL(Resp) + sLineBreak +
                                       'Modo: ' + IfThen(OldAtivo, 'On-Line', 'Off-Line') + sLineBreak +
                                       'Arquivo: "'+ NomeArq +'" não gerado' )) ;

    if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
      CopyFileTo(PathDest, NomeArquivo) ;
  finally
    UnloadDLLFunctions;
    Ativo := OldAtivo;

    if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
      SysUtils.DeleteFile(PathDest);
  end;
end;

procedure TACBrECFDaruma.ArquivoMFD_DLL(DataInicial, DataFinal: TDateTime;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD);
var
  Resp: Integer ;
  NomeArq, Relatorio, Tipo, DtInicial, DtFinal, DirDest, PathDest: AnsiString ;
  OldAtivo, OnLine: Boolean ;
begin
  OldAtivo := Ativo;
  try
    case Finalidade of
      finMF: Relatorio := 'MF';
      finMFD: Relatorio := 'MFD';
      finTDM: Relatorio := 'TDM';
      finNFP: Relatorio := 'NFP';
      finNFPTDM: Relatorio := 'NFPTDM';
      finSintegra:
      begin
        Relatorio := 'SINTEGRA';
        NomeArq  := Relatorio + '.TXT';
      end;
      finSPED:
      begin
        Relatorio := 'SPED';
        NomeArq  := Relatorio + '_DRM.TXT';
      end
    else
      raise EACBrECFERRO.Create(ACBrStr('Finalidade não reconhecida, finalidades válidas: MF, MFD, TDM, NFP, NFPTDM, SINTEGRA, SPED'));
    end;

    if not (Finalidade in [finSintegra, finSPED]) then
      NomeArq  := 'ATO_' + Relatorio + '_DATA.TXT';

    DirDest  := IncludeTrailingPathDelimiter(ExtractFilePath(NomeArquivo));
    PathDest := DirDest + NomeArq;

    LoadDLLFunctions;
    ConfigurarDLL(DirDest);

    Ativo     := False;
    Tipo      := 'DATAM';
    DtInicial := FormatDateTime('ddmmyyyy', DataInicial);
    DtFinal   := FormatDateTime('ddmmyyyy', DataFinal);
    OnLine    := (OldAtivo) or ((TACBrECF(fpOwner).Modelo = ecfEscECF) and (TACBrECF(fpOwner).Ativo));

    // utilizar o modo on-line quando a impressora estiver ativa e o off-line quando não estiver
    if OnLine then
    begin
      Resp := xrGerarRelatorio_ECF_Daruma(Relatorio, Tipo, DtInicial, DtFinal);
      if (Resp <> 1) then
        raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar xrGerarRelatorio_ECF_Daruma.'+sLineBreak+
                                         'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;
    end
    else
    begin
      Resp := xrGerarRelatorioOffline_ECF_Daruma(Relatorio, Tipo, DtInicial, DtFinal,
                                                 DirDest + 'Daruma.mf',
                                                 DirDest + 'Daruma.mfd',
                                                 DirDest + 'Daruma.inf');
      if (Resp <> 1) then
        raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar rGerarRelatorioOffline_ECF_Daruma.'+sLineBreak+
                                         'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;
    end;

    // tratar por a NFP gera os arquivos com nome no formato conforme a legislação
    if not(Finalidade in [finNFP, finNFPTDM]) then
    begin
      if not FileExists( PathDest ) then
        raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de xrGerarRelatorio_ECF_Daruma.'+sLineBreak+
                                         'Cod: '+ IntToStr(Resp) + ' ' + GetDescricaoErroDLL(Resp) + sLineBreak +
                                         'Modo: ' + IfThen(OldAtivo, 'On-Line', 'Off-Line') + sLineBreak +
                                         'Arquivo: "'+ NomeArq +'" não gerado' )) ;

      if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
        CopyFileTo(PathDest, NomeArquivo) ;
    end;
  finally
    UnloadDLLFunctions;
    Ativo := OldAtivo;

    if AnsiUpperCase(PathDest) <> AnsiUpperCase(NomeArquivo) then
      SysUtils.DeleteFile(PathDest);
  end;
end;

procedure TACBrECFDaruma.ProgramarBitmapPromocional(const AIndice: Integer;
  const APathArquivo: AnsiString; const AAlinhamento: TACBrAlinhamento);
var
  Posicao: String;
  Indice: String;
  Resp: Integer;
  OldAtivo: Boolean;
begin
  if fpMFD then
  begin
    if AIndice > 5 then
      raise EACBrECFERRO.Create( ACBrStr('Posição do Bitmap dever ser um número entre 1 e 5.') );

    if Trim(APathArquivo) = EmptyStr then
      raise EACBrECFERRO.Create( ACBrStr('Caminho para o arquivo de imagem não foi informado.') );

    if not FileExists(APathArquivo) then
      raise EACBrECFERRO.Create( ACBrStr( 'Arquivo "'+APathArquivo+'", não foi encontrado.') );

    if AnsiUpperCase(ExtractFileExt(APathArquivo)) <> '.BMP' then
      raise EACBrECFERRO.Create( ACBrStr( 'Arquivo "'+APathArquivo+'", deve ser um arquivo do tipo bitmap.') );

    Indice := Format('%2.2d', [AIndice]);

    if AAlinhamento = alCentro then
      Posicao := '000'
    else
      Posicao := '001';

    OldAtivo := Ativo;
    try
      LoadDLLFunctions;
      ConfigurarDLL('');

      Ativo := False;

      Resp := xeCarregarBitmapPromocional_ECF_Daruma(APathArquivo, Indice, Posicao);
      if (Resp <> 1) then
        raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar eCarregarBitmapPromocional_ECF_Daruma.'+sLineBreak+
                                         'Cod.: '+IntToStr(Resp)+' '+GetDescricaoErroDLL(Resp) )) ;
    finally
      UnloadDLLFunctions;
      Ativo := OldAtivo;
    end;
  end;
end;

procedure TACBrECFDaruma.SegundaViaVinculado;
begin
  fsNumCupom := '';
  EnviaComando( FS + 'F' + #216);
end;

procedure TACBrECFDaruma.ReimpressaoVinculado;
begin
  fsNumCupom := '';
  EnviaComando( FS + 'F' + #217);
end;

function TACBrECFDaruma.TraduzirTag(const ATag: AnsiString): AnsiString;
begin
  Result := DarumaTraduzirTag(ATag);
end;

function TACBrECFDaruma.TraduzirTagBloco(const ATag, Conteudo: AnsiString
  ): AnsiString;
begin
  Result := DarumaTraduzirTagBloco( ATag, Conteudo, Self);
end;

end.


