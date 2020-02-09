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

{******************************************************************************
|* Historico
|*
|* 21/02/2005:  Daniel Simoes de Almeida
|*   Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
|* 22/06/2005:  Daniel Simoes de Almeida e Licerio Jose Rodrigues Neto
|*   - Corrigido diversos Bugs nos ECFs versao 0.3, em VendeItem,
|*     EfetuaPagamento
|* 28/09/2005:  Daniel Simoes de Almeida
|*   - Corrigido BUG na Rotina EnviaComando, que causava TimeOuts em vários
|*     métodos e propriedades. ( O Controle de Fluxo de Hardware estava errado)
|*   - Modificado mecanismo de Espera do termino da Impressao usando a
|*     a propriedade AguardaImpressao:=True e a função VerificaFimImpressao....
|*     - Removido o método EnviaComandoEspera
|*   - Adcionado suporte a Impressao de cheques
|*   - Rotinas testadas e compatibilizadas com a serie 9000
|*   - Modificado o sistema de detecção de Firmaware através da propriedade
|*     Publica "Versao : TACBrECFSwedaVersao"
|* 29/09/2005:  Ederson Selvati
|*   - Corrigido BUG em VendeItem quando havia desconto.
|* 08/12/2005:  Daniel Simoes de Almeida
|*   VerificaFimImpressao poderia disparar excessão com ECF off-line
|*   Bug reportado por: Adriano Alves Dornelas
|* 16/12/2005:  Daniel Simoes de Almeida
|*   - Adicionda a propriedade UsaDTR : Boolean (False)... Se ela for Verdadeiro
|*     o DTR é desligado para ler a resposta do ECF, (isso pode ser necessário
|*     em alguns modelos de ECF. No metodo Ativar o componente tenta detctar
|*     o estado necessário para o UsaDTR)
|*   - Melhorado o Suporte a linha ST (MFD), graças ao emprestimo de um modelo
|*     ST100, por "Mauro Sano" da Sweda
|*   - Corrigido Bug de TimeOut durante a Redução Z em ECFs Matriciais
|*   - ProgramaFormaPagamento e ProgramaComprovanteNaoFiscal ajustados para a
|*     linha ST (MFD)
|* 13/03/2006:  Daniel Simoes de Almeida
|*   - Removida a propriedade UsaDTR, Revisto mecanismo de comunicação usando o
|*     DTR. Se tiver problemas com o DTR use ACBrECF1.Device.Hardflow := True ;
|*   - Corrigido bugs na detectção da Versao e Modelo do ECF
|* 09/05/2006:  Daniel Simoes de Almeida  e  Ederson Selvati
|*   - Corrigido Bug na detecção do Modelo / Versão quando Versao = 1.1
|* 09/05/2006:  Daniel Simoes de Almeida
|*   - GavetaAberta: modificado para ficar de acordo com o Manual. Se o sinal
|*     estiver invertido use a nova propriedade ACBrECF.GavetaSinalInvertido
|*   - Inserido pequeno Interavalo na leitura do Estado se o ECF for swdC
|* 03/10/2006:  Daniel Simoes de Almeida
|*   - Comando ImprimeCheque adaptado para funcionar na Serie detectada como (B)
|* 07/11/2006:  Daniel Simoes de Almeida
|*   - Corrigido problemas na Detecção do estado após ocorrencia de Erro no ECF
|*   - Corrigido problemas no Calculo do Valor Total em VendeItem (TruncFix)
|*   - Corrigido problema na VendeItem quando executado várias vezes rapidamente
|*     na sequencia
|* 10/01/2007:  Daniel Simoes de Almeida / Ederson Selvati
|*   - Corrigido bug nos Descontos de "VendeItem" e "SubtotalizaCupom" em alguns
|*      ECFs NAO MFD ( matriciais linha 7000 )
|* 10/01/2007:  Ederson Selvati / Daniel Simoes de Almeida
|*   - Corrigido bug no EfetuaPagamento dos modelos 'C' e 'B' em FPG com flag
|*     de Vinculado ativado
|*   - Aumentado o TimeOut da Reduçao Z para (150 sec)
|*   - Ignorado Flag "SLIP" em modelo 'B' na verificaçao de erros de "EnviaComando"
|*   - Corrigida a abertura da Gaveta de Dinheiro nos ECFs MFD
|*   - Corrigido o TimeOut da Leitura da Memoria Fiscal no modelo 'B'
|*   - Corrigido bug em "FechaRelatorio" no modelo 'B'
|*   - Ativada a Impressao da palavra "SubTotal" na subtotalização do Cupom
|* 21/03/2007:  Alan Lucas
|*   - Método SubtotalizaCupom adaptado para maior compatibilidade com as várias
|*     versões de ECF Sweda
|* 22/03/2007:  Ederson Selvati
|*   - Gato para resolver o erro -P111+0065 no modelo 7000 1.1 logo após
|*     um vinculado
|*   - Novas propriedades: DataMovimento, GrandeTotal, NumCRZ, VendaBruta,
|*     TotalCancelamentos, TotalDescontos, TotalSubstituicaoTributaria,
|*     TotalNaoTributado, TotalIsencao,  NumCOOInicial
|* 23/03/2007:  Alan Lucas
|*   - Novas propriedades: CNPJ, TotalAcrescimos, NumUltimoItem
|* 25/03/2007:  José Nilton Pace
|*   - Corrigido NumCOOInicial
|* 05/07/2007:  WPSouto
|*   - Corrigido a carga de Formas de Pagamento na MFD (le todas as 20)
|* 05/07/2007:  Daniel Simões de Almeida
|*   - Corrigida a Leitura de Memoria Fiscal por Serial (não responde)
|* 17/08/2007:  Daniel Simões de Almeida
|*   - Adicionado os métodos: NaoFiscalCompleto, AbreNaoFiscal,
|*     RegistraItemNaoFiscal, LerTotaisFormaPagamento,
|*     LerTotaisComprovanteNaoFiscal
|* 05/09/2007:  Daniel Simões de Almeida
|*   - Corrigido Retorno do COO em Impressoras MFD (ST100)
|* 10/04/2008:  Daniel Simões de Almeida
|*   - Override em AchaCNFDescricao para pesquisar ignorando o 1 caracter
|*     (simbolo)
|*   - Bug Sweda, não carrega ou acha Aliquotas com T1 ao invez de T01
|* 29/04/2008:  Daniel Simões de Almeida
|*   - Corrigido o retorno de NumSerie e NumVersao em modelos MFD
|* 24/12/2008:  Daniel Simoes de Almeida
|*  - Método Interno Purge poderia causar Loop Infinito, corrigido
|*    (bug reportado por Waldir Paim)
|*  - Método GetTotalPago não retornava o valor correto quando pagamento excedia
|*    o subtotal do Cupom, corrigido (bug reportado por Jocelio)
|* 12/01/2009: Ederson Selvati
|*  - Ajuste no método GetVersaoSweda para identificar o modelo ST200
|*    (bug reportado por Waldir Paim)
|* 17/01/2009: Daniel Simoes de Almeida
|*  - Ajustes no LeResposta.
|     - Alguns modelos de 9000IIIE, retorna ocasionalmente  um caracter de
|*      controle, (#17) no inicio da Resposta, corrigido com um
|*      TrimLeft( Resposta )
|*    - Flag fsEsperaMinima, causava lentidão em comandos com TimeOut muito alto
|*      ajustado para usar no máximo 10 seg em fsEsperaMinima
|*  - LeituraMemoriaFiscalSerial, aumentado TimeOut (10) das respostas
|*    intermediárias (++), evitando leituras incompletas por TimeOut
|* 25/06/2009: José Nilton Pace
|*   - Ajustado Retorno do Estado da Impressora
|*   - Ajustado Retorno do COOInicial, estava trazendo o CCF Inicial 
******************************************************************************}
{$I ACBr.inc}

unit ACBrECFSweda ;

interface
uses
  Classes,
  {$IFNDEF NOGUI}
    {$IF DEFINED(VisualCLX)}
       QControls, QForms, QDialogs,
    {$ELSEIF DEFINED(FMX)}
       FMX.Controls, FMX.Forms, FMX.Dialogs, System.UITypes,
    {$ELSE}
       Controls, Forms, Dialogs,
      {$IFDEF DELPHIXE2_UP}
       System.UITypes,
      {$ENDIF}
    {$IFEND}
  {$ENDIF}
  {$IFDEF NEXTGEN}
   ACBrBase,
  {$ENDIF}
  ACBrECFClass, ACBrDevice, ACBrDeviceSerial;


const
  {$IFDEF LINUX}
   cLIB_Sweda = 'libconvecf.so';
  {$ELSE}
   cLIB_Sweda = 'CONVECF.DLL';
  {$ENDIF}

type TACBrECFSwedaVersao = ( swdNenhum, swdA, swdB, swdC, swdD, swdST ) ;
{ swdA   =  Serie 7000 versao 0.3 e 0.4
  swdB   =  Serie 7000 versao 1.0
  swdC   =  Serie 7000 versao 1.A, 1.5, 1.6
  swdD   =  Serie 9000 versao 1.0 e 1.1
  swdST  =  Serie MFD 01.00.03 e 01.00.04 }

type
{ Classe filha de TACBrECFClass com implementaçao para Sweda }

{ TACBrECFSweda }

TACBrECFSweda = class( TACBrECFClass )
 private
    fsNumVersao : String ;
    fsNumCRO    : String ;
    fsSubModeloECF  : String ;
    fsNumECF    : String ;
    fsModeloSweda : AnsiChar ; { A - IF-7000III, B - IF-7000I,   C - IF-7000II,
                             D - IF-7000IE,  E - IF-7000IIE, F - IFS-9000,
                             G - IFS9000IIIE }
    fsArredonda  : Boolean ;
    fsVinculado  : Integer ;  { 0 = Nenhum, ou numero de Vinculados pendentes }
    fsOldSeq     : AnsiString ;
    fsTotalPago  : Double ;
    fsCMDVinculado : AnsiString ;
    fsVersaoSweda  : TACBrECFSwedaVersao;
    fsEsperaMinima : TDateTime ;
    fsEmVinculado  : Boolean;

    xECF_AbrePortaSerial : Function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xECF_DownloadMFD : Function (Arquivo: AnsiString; TipoDownload: AnsiString;
      ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ):
      Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xECF_ReproduzirMemoriaFiscalMFD : Function (tipo: AnsiString; fxai: AnsiString;
      fxaf:  AnsiString; asc: AnsiString; bin: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xECF_FechaPortaSerial : Function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;
    xECF_DownloadMF : Function(Arquivo:AnsiString):Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

    procedure LoadDLLFunctions;
    procedure AbrePortaSerialDLL;
    
    function GetVersaoSweda : TACBrECFSwedaVersao;

    procedure EsperaEstado( EstadoAEsperar : TACBrECFEstadoSet;
       TimeOut : Integer = 2000 ) ;
    Procedure Purge(const Id: AnsiString) ;
    procedure LeBufferSerial(const Cmd : String; AStringList: TStringList);
 protected
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumCCF: String; override ;
    function GetNumGNF: String; override ;
    function GetNumGRG: String; override ;
    function GetNumCDC: String; override ;
    function GetNumECF: String; override ;
    function GetNumCRO: String; override ;
    function GetNumSerie: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;
    function GetArredonda: Boolean; override ;
    function GetChequePronto: Boolean; override ;

    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetIM: String; override ;
    function GetCliche: AnsiString; override ;
    function GetUsuarioAtual: String; override ;
    function GetDataHoraSB: TDateTime; override ;
    function GetSubModeloECF: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetGrandeTotal: Double; override ;
    function GetNumCRZ: String; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalTroco: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;

    function GetTotalAcrescimosISSQN: Double; override;
    function GetTotalCancelamentosISSQN: Double; override;
    function GetTotalDescontosISSQN: Double; override;
    function GetTotalSubstituicaoTributariaISSQN: Double; override;
    function GetTotalIsencaoISSQN: Double; override;
    function GetTotalNaoTributadoISSQN: Double; override;

    function GetNumCOOInicial: String; override ;
    function GetNumUltimoItem: Integer; override ;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;
    function VerificaFimImpressao(var TempoLimite: TDateTime) : Boolean ; override ;

    procedure ListaRelatorioGerencial(Relatorio : TStrings; Vias : Integer = 1; Indice: Integer = 0);
       override ;
    Procedure ListaCupomVinculado( Relatorio : TStrings; Vias : Integer = 1) ;
      override ;
    function GetNumReducoesZRestantes: String; override;

 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    Property VersaoSweda : TACBrECFSwedaVersao read GetVersaoSweda ;

    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    Procedure DescontoAcrescimoItemAnterior(ValorDescontoAcrescimo : Double;
       DescontoAcrescimo : String = 'D'; TipoDescontoAcrescimo : String = '%';
       NumItem : Integer = 0); override ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '' ) ; override ;

    procedure CancelaDescontoAcrescimoSubTotal(TipoAcrescimoDesconto: Char) ;
       override ;//A -> Acrescimo D -> Desconto  // Função implementada até o momento apenas para Daruma

    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;

    { Procedimentos de Cupom Não Fiscal }
    function AchaCNFDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True ) :
       TACBrECFComprovanteNaoFiscal ; override ;
    Procedure NaoFiscalCompleto( CodCNF : String; Valor : Double;
       CodFormaPagto  : String; Obs : AnsiString; IndiceBMP : Integer = 0 ) ; override ;
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;

    Procedure LeituraX ; override ;
    Procedure LeituraXSerial( Linhas : TStringList) ; override ;
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;
    Procedure PulaLinhas( NumLinhas : Integer = 0 ) ; override ;
    Procedure CortaPapel( const CorteParcial : Boolean = false) ; override ;

    Procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ; override ;
    Procedure CancelaImpressaoCheque ; override ;
    Function LeituraCMC7 : AnsiString ; override ;

    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure CorrigeEstadoErro(Reducao: Boolean = True) ; override ;
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal : Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMFDSerial(DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;
    Procedure LeituraMFDSerial( COOInicial, COOFinal : Integer;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;

    procedure IdentificaPAF( NomeVersao, MD5 : String); override ;

    Procedure AbreGaveta ; override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  override;

    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaRelatoriosGerenciais ; override ;
    procedure LerTotaisRelatoriosGerenciais ; override ;
    Procedure ProgramaRelatorioGerencial( var Descricao: String;
       Posicao : String = '') ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;

    Procedure EspelhoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; override ;
    Procedure EspelhoMFD_DLL( COOInicial, COOFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; override ;
    Procedure ArquivoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD  ) ; override ;
    Procedure ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD;
       TipoContador: TACBrECFTipoContador = tpcCOO ) ; override ;
    Procedure ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString); override;

 end ;

implementation
Uses
   {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
   SysUtils, Math,
    {$IFDEF COMPILER6_UP} DateUtils, StrUtils, {$ELSE} ACBrD5, {$ENDIF}
     ACBrECF, ACBrConsts, ACBrUtil ;

{ ----------------------------- TACBrECFSweda ------------------------------ }

constructor TACBrECFSweda.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.HandShake := hsDTR_DSR ;
  { Variaveis internas dessa classe }
  fsVinculado   := 0 ;
  fsTotalPago   := -1 ;
  fsNumVersao   := '' ;
  fsNumECF      := '' ;
  fsNumCRO      := '' ;
  fsSubModeloECF  := '' ;
  fsModeloSweda := ' ' ;
  fsCMDVinculado:= '' ;
  fsEsperaMinima:= 0;
  fsOldSeq      := '' ;

  fpColunas     := 40 ;
  fsVersaoSweda := swdNenhum ;
  fsArredonda   := False ;

  fsEmVinculado := False;

  fpModeloStr   := 'Sweda' ;
  fpRFDID       := 'SW' ;

  xECF_AbrePortaSerial := nil;
  xECF_DownloadMFD := nil;
  xECF_ReproduzirMemoriaFiscalMFD := nil;
  xECF_FechaPortaSerial := nil;
end;

destructor TACBrECFSweda.Destroy;
begin

  inherited Destroy ;
end;

procedure TACBrECFSweda.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+#10+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

//  fpDevice.HandShake := hsDTR_DSR ;
  inherited Ativar ; { Abre porta serial }

  fsNumVersao   := '' ;
  fsNumECF      := '' ;
  fsNumCRO      := '' ;
  fsSubModeloECF  := '' ;
  fsModeloSweda := ' ' ;
  fpModeloStr   := 'Sweda' ;
  fsVersaoSweda := swdNenhum ;
  fsArredonda   := False ;
  fsEsperaMinima:= 0;

  try
     fpDevice.Serial.Purge ;
     { Testando a comunicaçao com a porta }
     GetVersaoSweda ;     

     if VersaoSweda = swdNenhum then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));
  except
     Desativar ;
     raise ;
  end ;
end;


function TACBrECFSweda.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
Var ErroMsg : String ;
    AUT, SLIP, STATUS : AnsiChar ;
    Verificar : Boolean ;
    LeituraMF : Boolean ;
begin
  result    := '' ;
  ErroMsg   := '' ;
  Verificar := false ;
  LeituraMF := (cmd = '++') ;
  fpComandoEnviado   := '' ;
  fpRespostaComando  := '' ;

  if AguardaImpressao and (not fpMFD) and (fsEsperaMinima = 0) and (TimeOut > 5) then
     fsEsperaMinima := IncSecond( now, min(max(Trunc(TimeOut/IfThen(fsVersaoSweda >= swdST,10,5)),1),10)) ;

  try
     { Codificando CMD de acordo com o protocolo da Sweda }
     cmd := #27 + '.' + cmd + '}' ;

     fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }
     if fpDevice.HandShake = hsDTR_DSR then
        fpDevice.Serial.DTR := False ;  { DesLiga o DTR para enviar }

     if fpDevice.HandShake = hsRTS_CTS then
        fpDevice.Serial.RTS := False ;  { DesLiga o RTS para enviar }

     while fpComandoEnviado = '' do
     begin
        if not LeituraMF then  {Se estiver lendo Mem.Fiscal tem dados no Buffer}
           fpDevice.Serial.Purge ;

        if not TransmiteComando( cmd ) then
           continue ;

        fpComandoEnviado := cmd ;
     end ;

     if fpDevice.HandShake = hsDTR_DSR then
        fpDevice.Serial.DTR := True ;  { Liga o DTR para ler a Resposta }

     if fpDevice.HandShake = hsRTS_CTS then
        fpDevice.Serial.RTS := True  ;  { sLiga o RTS para para ler a Resposta }

{    *** Mini - LeResposta para uso de Debug ***
     while (not VerificaFimLeitura(fpRespostaComando)) do
     begin
        try
           fpRespostaComando := fpRespostaComando + // Le conteudo da porta
                                fpDevice.Serial.RecvPacket(100) ;
        except
        end ;
     end ;
}
     { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
       falha na leitura LeResposta dispara Exceçao }
     LeResposta ;
     Result := TrimLeft( fpRespostaComando ) ;

     if Result = '' then
        Result := '.-0001^TIMEOUT' ;

     { Verificando por erros }
     ErroMsg := '' ;
     if (copy(Result, 1, 5) = '.-P00') and (Length(Result) = 7) then
      begin
        case Result[6] of
          '2' : ErroMsg := 'Comando não foi enviado a Impressora.' ;
          '6' : ErroMsg := 'Não foi recebida a resposta da Impressora.' ;
        end;
        Verificar := true ;
      end

     else if copy(Result, 1, 3) = '.-P' then
      begin
        try AUT    := Result[4] except AUT    := ' ' end ;
        try SLIP   := Result[5] except SLIP   := ' ' end ;
        try STATUS := Result[6] except STATUS := ' ' end ;
        
        if fsModeloSweda = 'B' then  // No Modelo 'B' SLIP deve ser ignorado
           SLIP := ' ' ;

        if (AUT = '1') or (SLIP = '1') or (STATUS = '1') then
         begin
           ErroMsg   := 'Impressora fora de linha'+sLineBreak ;
           Verificar := true ;
         end
        else if (AUT = '2') or (SLIP = '2') or (STATUS = '2') then
         begin
           ErroMsg   := 'Time-out de Transmissao'+sLineBreak ;
           Verificar := true ;
         end
        else if (AUT = '6') or (SLIP = '6') or (STATUS = '6') then
         begin
           ErroMsg   := 'Impressora não está pronta para aceitar novo comando'+
                        sLineBreak ;
           Verificar := true ;
         end ;

{       if AUT = '5' then
           ErroMsg := ErroMsg + 'SEM documento para AUTENTICAR'+sLineBreak ;

        if SLIP = '5' then
           ErroMsg := ErroMsg + 'SEM FOLHA solta presente'+sLineBreak ;
}
        if STATUS = '5' then
        begin
           if fsOldSeq <> copy(Result,9,4) then
              DoOnMsgPoucoPapel( 'Papel acabando' )
           else
              ErroMsg := cACBrECFSemPapelException ;
        end ;
      end
     else if copy(Result, 1, 2) = '.-' then
      begin
        ErroMsg := copy(Result,7,Length(Result)-7) ;
      end
     else if copy(Result, 1, 2) = '.+' then
        fsOldSeq := copy(Result,3,4) ;

     if Verificar then
     begin
        ErroMsg := ErroMsg + sLineBreak+
                   'Verifique:'+sLineBreak+
                   '- A impressora está ligada ? '+sLineBreak+
                   '- A tampa está aberta ?'+sLineBreak+
                   '- Os cabos estão conectados ?' ;
     //  Debug     +sLineBreak+'Comando enviado: ('+fpComandoEnviado+')' ;
     end ;

     if ErroMsg <> '' then
      begin
        ErroMsg := ACBrStr('Erro retornado pela Impressora: '+fpModeloStr+
                   sLineBreak + sLineBreak+
                   ErroMsg );

        if ErroMsg = cACBrECFSemPapelException then
           DoOnErrorSemPapel
        else
           raise EACBrECFSemResposta.create( ErroMsg ) ;
      end
     else
        Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }

  finally
     fsEsperaMinima := 0;
  end ;
end;

function TACBrECFSweda.VerificaFimLeitura(var Retorno : AnsiString ;
   var TempoLimite : TDateTime) : Boolean ;
begin
  { Nota sobre o VerificaFimLeitura: A SWEDA responde muito antes da
    Impressao terminar, o que pode causar problemas com comandos enviados logo
    após impressoes demoradas como a Leitura X (por exemplo). Para esses casos,
    é necessário ativar a propriedade "AguardaImpressao := True" }
  Result := ( (RightStr(Retorno,3) = '}'+#13+#10) or
              (pos('}',RightStr(Retorno,2)) > 0)     ) and (Length(Retorno) >= 3) ;

  { Se respondeu com Erro, não precisa Aguardar a Impressao }
  if AguardaImpressao and (copy(Retorno, 1, 2) = '.-') then
     AguardaImpressao := False ;
end;


function TACBrECFSweda.VerificaFimImpressao(var TempoLimite: TDateTime): Boolean;
Var Ret, RetCmd : AnsiString ;
    I  : Integer ;
begin
  { Alguns comandos da sweda respondem ok a essa função caso eles ainda não
    tenham iniciado (é o caso da ReduçãoZ).... O fsEsperaMinima aguarda pelo
    menos 1/5 do Tempo definido em TimeOut }
  Result := false ;
  if ((fsEsperaMinima <> 0) and (now < fsEsperaMinima) and (not fpMFD))  or
     ( not EmLinha() ) then
     Sleep(100)
  else
   begin
     { Essa função só é chamada se AguardaImpressao = True,
       Como essa função é executada dentro da "LeResposta", que por sua vez foi
       chamada por "EnviaComando", não podemos usar o método "EnviaComando" (ou
       teriamos uma chamada recursiva infinita), por isso o Loop abaixo envia o
       comando .27 diretamente para a Serial, e aguarda por segundos a resposta...
        Se a Sweda consegir responder, significa que a Impressão Terminou }
     RetCmd := '' ;
     I      := 0 ;
     try
        fpDevice.Serial.Purge ;

        if fpDevice.HandShake = hsDTR_DSR then
           fpDevice.Serial.DTR := False ;  { DesLiga o DTR para enviar }

        if fpDevice.HandShake = hsRTS_CTS then
           fpDevice.Serial.RTS := False ;  { DesLiga o RTS para enviar }

        if fsVersaoSweda = swdA then
           fpDevice.EnviaString( #27 + '.271}' )   { Pede Numcupom }
        else
           fpDevice.EnviaString( #27 + '.23}' );   { Pede Status }
           
        if fpDevice.HandShake = hsDTR_DSR then
           fpDevice.Serial.DTR := True ;  { Liga o DTR para ler a Resposta }

        if fpDevice.HandShake = hsRTS_CTS then
           fpDevice.Serial.RTS := True  ;  { sLiga o RTS para para ler a Resposta }

        repeat
           try
              Ret := fpDevice.Serial.RecvPacket(200) ;
              if Ret = '' then
                 raise EACBrECFERRO.create('Sem resposta') ;

              RetCmd := RetCmd + Ret ;
              TempoLimite := IncSecond(now, TimeOut);
              I := 0
           except
              Inc( I ) ;   // Nao achou dados para ler, incrementa num falhas
           end ;
        until (I > 5) or ( VerificaFimLeitura( RetCmd, TempoLimite) )  ;
     except
     end ;

     if fsVersaoSweda = swdA then
        Result := VerificaFimLeitura(RetCmd, TempoLimite) and
                  (LeftStr(RetCmd, 3) = '.+C') and
                  ( StrToIntDef( copy(RetCmd,14,4), 0) > 0 )
     else
        { Se tiver "6" em AUT, SLIP ou STATUS é porque o ECF não está pronto para
          aceitar novos comandos }
        Result := VerificaFimLeitura(RetCmd, TempoLimite) and
                  (pos('6', copy(RetCmd,4,3)) = 0) ;

     { Efetuando Purge de comandos pendentes }
     if Result then
        Purge('VerificaFimImpressao') ;  { Limpa buffer de Entrada e Saida }
   end ;

end;

procedure TACBrECFSweda.Purge(const Id : AnsiString) ;
 Var RetCmd, Ret : AnsiString ;
     I : Integer ;
begin
  RetCmd := '' ;
  I      := 0 ;
  repeat
     try
        if fpDevice.HandShake = hsDTR_DSR then
           fpDevice.Serial.DTR := True ;  { Liga o DTR para ler a Resposta }

        if fpDevice.HandShake = hsRTS_CTS then
           fpDevice.Serial.RTS := True  ;  { sLiga o RTS para para ler a Resposta }
        sleep(10) ;

        Ret := fpDevice.Serial.RecvPacket(200) ;
        if Ret = '' then
           Inc( I )  // Nao achou dados para ler, incrementa num falhas
        else
         begin
           RetCmd := RetCmd + Ret ;
           I := max( 0, I-1) ;
         end ;
     except
        Inc( I ) ;   // Nao achou dados para ler, incrementa num falhas
     end ;
  until (I > 2)  ;

  if RetCmd <> '' then
     GravaLog('  Purge '+Id+': '+ RetCmd, True );

  fpDevice.Serial.Purge ;
end ;

function TACBrECFSweda.GetDataHora: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
    P : Integer ;
begin
  RetCmd := EnviaComando( '28' ) ;
  P      := pos('!',RetCmd) ;
  Result := 0 ;
  if P > 0 then
  begin
     OldShortDateFormat := ShortDateFormat ;
     try
        ShortDateFormat := 'dd/mm/yy' ;
        result := StrToDate(copy(RetCmd,P+1,2)+ DateSeparator +
                            copy(RetCmd,P+3,2)+ DateSeparator +
                            copy(RetCmd,P+5,2)) ;
        result := RecodeHour(  result,StrToIntDef(copy(RetCmd,P+7,2),0)) ;
        result := RecodeMinute(result,StrToIntDef(copy(RetCmd,P+9,2),0)) ;
        { Obs.: Sweda nao retorna os Segundos }
     finally
        ShortDateFormat := OldShortDateFormat ;
     end ;
  end ;
end;

function TACBrECFSweda.GetNumCupom: String;
 Var RetCmd : AnsiString ;
     Tentativas : Integer ;
begin
  Result := '' ;

  For Tentativas := 1 to 3 do
  begin
     if fpMFD then
      begin
        RetCmd := EnviaComando( '27'+'H' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,73,6), 0), 6) ;
      end
     else
      begin
        RetCmd := EnviaComando( '27'+'1' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,14,4), 0), 6) ;
      end ;

      if Result <> '' then
         break ;

      Sleep(100) ;
  end ;
end;

function TACBrECFSweda.GetNumCCF: String;
 Var RetCmd : AnsiString ;
     Tentavias : Integer ;
begin
  Result    := '' ;

  For Tentavias := 1 to 3 do
  begin
     if fpMFD then
      begin
        RetCmd := EnviaComando( '27'+'H' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,61,6), 0), 6) ;
      end
     else
      begin
        RetCmd := EnviaComando( '27'+'1' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,121,4), 0), 6) ;
      end ;

      if Result <> '' then
         break ;

      Sleep(100) ;
  end ;
end;

function TACBrECFSweda.GetNumCRO: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumCRO) = '' then
  begin
     if fsVersaoSweda < swdB then                { 0.3 tem outro retorno }
      begin
        RetCmd := EnviaComando( '27'+'9' ) ;
        if LeftStr(RetCmd, 3) = '.+C' then
           fsNumCRO := IntToStrZero( StrToIntDef(copy(RetCmd,116,4),0),4) ;
      end
     else
      begin
        RetCmd := EnviaComando( '27'+'G' ) ;
        if LeftStr(RetCmd, 3) = '.+C' then
           fsNumCRO := IntToStrZero( StrToIntDef(copy(RetCmd,16,4),0),4) ;
      end ;
  end ;

  Result := fsNumCRO ;
end;

function TACBrECFSweda.GetNumECF: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumECF) = '' then
  begin
     RetCmd := EnviaComando( '27'+'F' ) ;  { F ‚ o menor, mais rapido de ler }
        
     if LeftStr(RetCmd, 3) = '.+C' then
        fsNumECF := IntToStrZero( StrToIntDef(copy(RetCmd,4,3),0),4) ;
  end ;

  Result := fsNumECF ;
end;

function TACBrECFSweda.GetNumSerie: String;
Var RetCmd : AnsiString ;
begin
  Result := '' ;

  if fsVersaoSweda >= swdST then
   begin
     RetCmd := EnviaComando( '27'+'H' ) ;
     if (LeftStr(RetCmd, 3) = '.+C') then
        Result := Trim(copy(RetCmd,8,21))
   end
  else if fsVersaoSweda < swdB then                { 0.3 tem outro retorno }
   begin
     RetCmd := EnviaComando( '27'+'2' ) ;
     if LeftStr(RetCmd, 3) = '.+C' then
        Result := Trim(Copy(RetCmd,8,11)) ;
   end
  else
   begin
     RetCmd := EnviaComando( '27'+'3' ) ;
     if LeftStr(RetCmd, 3) = '.+C' then
        Result := Trim(Copy(RetCmd,13,9)) ;
   end ;
end;

function TACBrECFSweda.GetNumVersao: String ;
Var RetCmd : AnsiString ;
begin
  if fsModeloSweda = ' ' then
  begin
     RetCmd := EnviaComando( '27'+'1' ) ;
     if (LeftStr(RetCmd, 3) = '.+C') and (copy(RetCmd, 7, 1) = '1') then
        try
           fsModeloSweda := RetCmd[125] ;
        except
           fsModeloSweda := ' ' ;
        end ;
//     sleep(100)
  end ;

  if fsNumVersao = '' then
  begin
     RetCmd := EnviaComando( '27'+'G' ) ;
     if (LeftStr(RetCmd, 3) = '.+C') and (copy(RetCmd, 7, 1) = 'G') then
        fsNumVersao := copy(RetCmd,28,3)
     else
        fsNumVersao := '0.3';
  end ;

  Result := fsNumVersao ;
end;

function TACBrECFSweda.GetVersaoSweda: TACBrECFSwedaVersao;
Var VerInt : Integer ;
    SubModelo, VerString, RetCmd : AnsiString ;
begin
(*  Correspondência de Modelos:
       Linha 7000             Linha 9000             Linha ST
  "A"  IFS-7000-III 0.3
  "B"  IFS-7000 I   1.0       IFS-9000 I    1.1
  "C"  IFS-7000 II  1.0       IFS-9000 IIE  1.1
  "D"  IFS-7000 IE  1.0       IFS-9000 IE   1.1
  "E"  IFS-7000 IEE           IFS-9000 II
  "F"                         IFS-9000 IIIE 1.0
  "G"                                                IF ST1000   "G"
  "H"                                                IF ST100    "H"
  "I"                                                IF ST200    "I"


  Modelo      Primeira Versao
  7000-III      0.3
  7000-I        1.0
  7000 II       1.0
  7000 IE       1.0
  7000 IEE      ??

  9000 I        1.1
  9000 IE       1.1
  9000 II       1.1
  9000 IIE      1.1
  9000 IIIE     1.0

  ST            1.0   (01.00.03)


  Compatibilidade de Comandos entre versões:
  swdA   =  Serie 7000 versao 0.3 e 0.4
  swdB   =  Serie 7000/9000 versao (7000 1.0 , 1.A) (9000 1.1)
  swdC   =  Serie 7000/9000 versao 1.5, 1.6
  swdD   =  Serie 9000IIIE versao 1.0 e 1.1 e 1.7
  swdST  =  Serie MFD
*)

  Result := fsVersaoSweda ;

  if fsVersaoSweda = swdNenhum then
  begin
     fpModeloStr := 'Sweda' ;
     fsArredonda := False ;
     fpMFD       := False ;
     fpTermica   := False ;

     VerString := NumVersao ;
     if (VerString = '') or (fsModeloSweda = ' ') then
        exit ;

     case fsModeloSweda of
       'A' : SubModelo := '7000-III' ;
       'B' : SubModelo := '7000-I / 9000-I' ;
       'C' : SubModelo := '7000-II / 9000-IIE' ;
       'D' : SubModelo := '7000-IE / 9000-IE' ;
       'E' : SubModelo := '7000-IEE / 9000-II' ;
       'F' : SubModelo := '9000-IIIE' ;
       'G' : SubModelo := 'ST1000' ;
       'H' : SubModelo := 'ST100' ;
       'I' : SubModelo := 'ST200' ;
       'J' : SubModelo := 'ST120' ;
     else
        SubModelo := '???' ;
     end ;

     fpModeloStr := Trim(fpModeloStr + ' ' +SubModelo) ;
     fsSubModeloECF := SubModelo ;
     if VerString = '1.A' then
        VerString := '1.0' ;

     VerString := StringReplace(VerString,'.',DecimalSeparator,[]) ;
     { Usando Versao x 10 pois o Delphi se perde com Double em 1.1 }
     VerInt    := TruncFix(StrToFloatDef(VerString,0)*10) ;
     if VerInt = 0 then
        exit ;

     if fsModeloSweda < 'F' then
      begin
        if VerInt < 10 then
         begin
           fsVersaoSweda := swdA ;
           fsArredonda   := True ;
           fpModeloStr := fpModeloStr + ' (A)' ;
         end
        else if (VerInt = 10) or (VerInt = 11) then
         begin
           fsVersaoSweda := swdB ;
           fsArredonda   := (VerInt = 10) ;
           fpModeloStr := fpModeloStr + ' (B)' ;
         end
        else if (VerInt > 11) then
         begin
           fsVersaoSweda := swdC ;
           fpModeloStr   := fpModeloStr + ' (C)' ;
         end ;
      end
     else if fsModeloSweda = 'F' then
      begin
        fsVersaoSweda := swdD ;
        fpModeloStr := fpModeloStr + ' (D)' ;
      end
     else if fsModeloSweda >= 'G' then
      begin
        fsVersaoSweda := swdST ;
        fpModeloStr   := fpModeloStr + ' (ST)' ;
        fpMFD         := True ;
        fpTermica     := True ;

        // Re-lendo o numero de versão no registrador correto //
        RetCmd := EnviaComando( '27'+'H' ) ;
        if (LeftStr(RetCmd, 3) = '.+C') and (copy(RetCmd, 7, 1) = 'H') then
           fsNumVersao := copy(RetCmd,28,12)
     else
        fsNumVersao := '0.3';

      end
     else
        fsVersaoSweda := swdNenhum ;
  end ;

  Result := fsVersaoSweda ;
end;

function TACBrECFSweda.GetTotalPago: Double;
Var RetCmd, Status : AnsiString ;
    SubTot, Falta : Double ;
    P : Integer ;
begin
  if fsTotalPago >= 0 then  // Possui TotalPago acumulado ?
  begin
     Result := fsTotalPago ;
     exit ;
  end ;
  
  Result := 0 ;
  RetCmd := EnviaComando('28') ;
  If fsVersaoSweda > swdA then P := 10 else P := 07 ;
  { Status pode ser: C - concluida, P - Pendente, E - Erro no Comando }
  Status  := UpperCase(copy(RetCmd,P,1)) ;

  if Status = 'P' then
  begin
     P := pos('!',RetCmd) ;
     if P > 0 then
     begin
        SubTot := RoundTo(StrToFloatDef(copy(RetCmd,P-24,12),0) / 100, -2) ;
        Falta  := RoundTo(StrToFloatDef(copy(RetCmd,P+52,12),0) / 100, -2) ;
        Result := max( RoundTo(SubTot - Falta,-2) ,0) ; { evitar negativo }
     end ;
  end ;
end;

function TACBrECFSweda.GetSubTotal: Double;
Var RetCmd : AnsiString ;
    P : Integer ;
begin
  RetCmd := EnviaComando('28') ;
  P      := pos('!',RetCmd) ;
  Result := 0 ;
  if P > 0 then
     Result := RoundTo(StrToFloatDef(copy(RetCmd,P-24,12),0) / 100, -2) ;
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
function TACBrECFSweda.GetEstado: TACBrECFEstado;
Var RetCmd, Status, Transacao : AnsiString ;
    P, I : Integer ;
    FlagZ, FlagX : AnsiChar ;
    SubTot, Falta, Receb : Double ;
begin
  fpEstado := estNaoInicializada ;
  if (not fpAtivo) then
  begin
    Result := fpEstado ;
    Exit ;
  end;

  try

    fpEstado := estDesconhecido ;

    RetCmd := EnviaComando( '28' ) ;
    P      := pos('!',RetCmd) ;
    if P = 0 then
       exit ;

    try FlagX := RetCmd[P+76] except FlagX := ' ' end ;
    try FlagZ := RetCmd[P+77] except FlagZ := ' ' end ;

    SubTot := RoundTo(StrToFloatDef(copy(RetCmd,P-24,12),0) / 100, -2) ;
    Falta  := RoundTo(StrToFloatDef(copy(RetCmd,P+52,12),0) / 100, -2) ;
    Receb  := RoundTo(StrToFloatDef(copy(RetCmd,P+64,12),0) / 100, -2) ;

    If fsVersaoSweda > swdA then I := 10 else I := 07 ;

    { Status pode ser: C - concluida, P - Pendente, E - Erro no Comando }
    Status    := UpperCase(copy(RetCmd,I,1)) ;
    Transacao := UpperCase(Trim(copy(RetCmd,I+1,8))) ;

    if not (Status[1] in ['C','E']) then
     begin
      if (Transacao = 'N.FISCAL') and (Receb > 0) then
        fpEstado := estNaoFiscal
      else if (Transacao = 'N.FISCAL') and (Receb = 0) then
        fpEstado := estRelatorio
      else if (Transacao = 'LEIT. X')  then
        fpEstado := estRelatorio
      else if (fsTotalPago >= 0) or ((SubTot <> 0) and (SubTot <> Falta) and (Status<>'E')) then
        fpEstado := estPagamento  { Verificação de estPagamento, antes, falha em algumas situações }
      else if (Transacao = 'VENDAS')  then
        fpEstado := estVenda
     end
    else
     begin
      if FlagZ = 'S' then
        fpEstado := estBloqueada
      else if FlagZ = 'F' then
        fpEstado := estRequerZ
      else if FlagX = 'F' then
        fpEstado := estRequerX
      else 
        fpEstado := estLivre ;
    end ;
  finally
    Result := fpEstado ;
  end ;
end;

function TACBrECFSweda.GetGavetaAberta: Boolean;
Var RetCmd : AnsiString ;
begin
  if fsModeloSweda = 'B' then
     RetCmd := EnviaComando('22')
  else
     RetCmd := EnviaComando('43') ;

  Result := (copy(RetCmd,1,6) = '.+G000')
end;


function TACBrECFSweda.GetPoucoPapel: Boolean;
Var RetCmd : AnsiString ;
begin
  fsOldSeq := ''; // tem que limpar a variavel de controle (por: Waldir Paim)
  RetCmd := EnviaComando( '23' ) ;
  Result := (copy( RetCmd, 6,1) = '5') ;
end;

function TACBrECFSweda.GetHorarioVerao: Boolean;
Var RetCmd : AnsiString ;
    P : Integer ;
begin
  RetCmd := EnviaComando( '28' ) ;
  P      := pos('!',RetCmd) ;
  Result := False ;
  if P > 0 then
     Result := (copy(RetCmd,P+11,1) = 'S') ;
end;

function TACBrECFSweda.GetArredonda: Boolean;
begin
  Result := fsArredonda  // (fsVersaoSweda < swdD) ;
end;

procedure TACBrECFSweda.LeituraX ;
Var Espera : Integer ;
begin
  Espera := IfThen( fsVersaoSweda >= swdST, 20, 120) ;
  AguardaImpressao := True ;
  EnviaComando('13N' , Espera ) ;
end;

procedure TACBrECFSweda.LeituraXSerial(Linhas: TStringList);
begin
  Linhas.Clear ;
  if fsVersaoSweda < swdST then
     exit ;

  LeBufferSerial('13|', Linhas);
end;

procedure TACBrECFSweda.LeBufferSerial(const Cmd : String ; AStringList : TStringList
   ) ;
  Var P1,P2 : Integer ;
      Resp, Ret, Linha : String ;
      wTempoInicioMsg : Integer ;
      wRetentar : Boolean ;
begin
  wRetentar       := Retentar ;
  wTempoInicioMsg := TempoInicioMsg ;
  AStringList.Clear ;
  try
     Retentar       := false ;
     TempoInicioMsg := 10 ;
     try
        Resp := EnviaComando( Cmd, 10) ;
        Repeat
           try
              Ret := EnviaComando('++',10) ;
           except
              EnviaComando('41') ;   // Cancela o envio de dados
              break ;
           end ;

           Resp := Resp  + Ret ;
        Until (Pos('.]}',Ret) > 0) ;

        Resp := StringReplace(Resp,'.]}','',[rfReplaceAll]) ;
        while Resp <> '' do
        begin
           P2 := pos('}',Resp) ;
           if P2 = 0 then P2 := Length(Resp) ;
           P1 := pos('.+',Resp) ;
           if (P1 = 0) or (P1 > P2) then P1 := -5 ;

           Linha := Copy(Resp,P1+6, P2-P1-6) ;
           AStringList.Add( Linha ) ;
           Resp := copy(Resp,P2+1,Length(Resp)) ;
        end ;
     except
        EnviaComando('41') ;   // Cancela o envio de dados
        raise ;
     end ;
  finally
     Retentar       := wRetentar ;
     TempoInicioMsg := wTempoInicioMsg ;
  end ;
end ;

procedure TACBrECFSweda.ReducaoZ(DataHora : TDateTime) ;
Var Cmd, DataStr : AnsiString ;
    Espera : Integer ;
begin
  DataStr := '' ;
  if DataHora <> 0 then
     DataStr := FormatDateTime('ddmmyy',DataHora) ;

  Cmd := '14' ;
  if fsVersaoSweda > swdA then  { .03 nao tem o flag }
     Cmd := Cmd + 'N' ;

  Espera := IfThen( fsVersaoSweda >= swdST, 180,  180) ;
  AguardaImpressao := true ;
  if fsVersaoSweda <= swdD then
     fsEsperaMinima := IncSecond( now, 30) ;  { Espera no minimo 30 seg }
  EnviaComando( Cmd + DataStr, Espera ) ;

  { Sweda pode demorar um pouco para entrar no estado Livre, se tentar
    Abrir cupom antes, o ECF pode para de responder }
  EsperaEstado([estLivre, estRequerX], 5000 );
end;

procedure TACBrECFSweda.AbreGaveta ;
begin
  try
    if (fsModeloSweda='B') or
       (fsModeloSweda='C') or
       (fsModeloSweda='E') or
       (fsModeloSweda='H') then  // MFD tb usa 21 - testado por Ederson
       EnviaComando('21')
    else
       EnviaComando('42') ;
  except
    Sleep(300);
  end;
end;

procedure TACBrECFSweda.MudaHorarioVerao ;
begin
  MudaHorarioVerao( not HorarioVerao) ;
end;

procedure TACBrECFSweda.MudaHorarioVerao(EHorarioVerao: Boolean);
Var Cmd  : AnsiString ;
begin
  If EHorarioVerao then Cmd := 'S' else Cmd := 'N' ;

  EnviaComando( '36' + Cmd, 3 ) ;
end;

procedure TACBrECFSweda.CorrigeEstadoErro(Reducao: Boolean);
begin
  inherited CorrigeEstadoErro(Reducao) ;

  { Na MFD em algumas ocasioes e necessario "confirmar a Data/Hora" }
  try
     EnviaComando('35' + FormatDateTime('hhnnssddmmyy', Now));
  except
  end ;
end;

procedure TACBrECFSweda.AbreCupom ;
Var Espera   : Integer ;
    CPF_CNPJ : String ;
begin
  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }

  CPF_CNPJ := '' ;
  if (fsVersaoSweda > swdA) and (Consumidor.Documento <> '') then
     CPF_CNPJ := PadRight(Consumidor.Documento,20) ;

  Espera := IfThen( fsVersaoSweda >= swdST, 5,  10);
  AguardaImpressao := True ;
  EnviaComando('17'+CPF_CNPJ, Espera) ;

  fsTotalPago := -1 ;
  Consumidor.Enviado := (CPF_CNPJ <> '') and
                        (Trim(Consumidor.Nome)+Trim(Consumidor.Endereco)='') ;
  EsperaEstado([estVenda]);
end;

procedure TACBrECFSweda.CancelaCupom(NumCOOCancelar: Integer);
Var Cmd, Erro : AnsiString ;
    SubTot    : Double ;
    Espera    : Integer ;
begin
  Erro   := '' ;
  Espera := IfThen( fsVersaoSweda >= swdST, 10,  25) ;
  AguardaImpressao := True ;
  fsTotalPago := -1 ;

  Try
     EnviaComando( '05' ,Espera) ;
  except
     On E : Exception do
     begin
        Erro := E.Message ;

        if fsVersaoSweda >= swdST then   { É MFD ou superior ?  Verifica se tem CDC aberto}
         begin
           while (pos('ESTORNAR CCD',Erro) > 0) do
           begin
              Erro := '' ;
              try
                 EnviaComando( '53' ) ;       { Cancela CCD }
                 EnviaComando( '12' ) ;       { 12-Fecha }
                 EnviaComando( '05' ) ;       { 05-Cancela }
              Except
                 On E : Exception do
                    Erro := E.Message ;
              end ;
           end ;
         end
        else      { Não é MFD, Veja se Pagamento aberto, Efetua Pagamento, Fecha e Cancela }
         begin
           Try
              SubTot := Subtotal ;
              EnviaComando('10'+'01'+IntToStrZero( Round(SubTot*100) ,12) ) ; { 10-Paga }

              Cmd := '12' ;
              if fsVersaoSweda > swdA then
              begin
                 Cmd := Cmd + 'N' ;
                 if fsVersaoSweda < swdD then
                    Cmd := Cmd + 'N' ;
              end ;

              AguardaImpressao := True ;
              EnviaComando( Cmd, 10 ) ;       { 12-Fecha }
              AguardaImpressao := True ;
              EnviaComando('05' ,Espera) ;    { 05-Cancela }
           Except
              On E : Exception do
                 Erro := E.Message ;
           end ;
         end ;
     end ;
  end ;

  if Erro <> '' then
     raise EACBrECFERRO.Create(Erro);
     
  FechaRelatorio ;   { Fecha relatorio se ficou algum aberto (só por garantia)}
end;

procedure TACBrECFSweda.CancelaItemVendido(NumItem: Integer);
begin
  EnviaComando( '04' + IntToStrZero(NumItem,3) ) ;
end;

procedure TACBrECFSweda.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
Var Espera : Integer ;
begin
  if ImprimeVinculado then
     if (fsVersaoSweda < swdD) and (fsVinculado > 0) then
        raise EACBrECFERRO.Create(ACBrStr('Já existe Forma de Pagamento com '+sLineBreak+
                       'comprovante NAO fiscal vinculado pendente. '+sLineBreak+
                       'Impressora: '+fpModeloStr+' Modelo: '+fsModeloSweda+sLineBreak+
                       ' aceita apenas 1 Compr.NAO Fiscal Viculado por Cupom.'))
     else
        fsVinculado := fsVinculado + 1 ;

  If (fsVersaoSweda < swdB) or (Length(Trim(Observacao)) = 0) then
     Observacao := ''
  else
     Observacao := '{' + copy(Observacao,1,80) ;

  AguardaImpressao := ((fsModeloSweda = 'C') or (fsModeloSweda = 'B')) ;
  EnviaComando('10' + CodFormaPagto + IntToStrZero( Round(Valor*100) ,12) +
                     Observacao );

 { Sweda pode demorar um pouco para entrar no estado de Pagmento, se tentar
    fechar o Cupom antes, o ECF pode para de responder }
  if fsTotalPago <= 0 then         // É o primeiro pagamento ?
  begin
     if fsVersaoSweda < swdST then
     begin
        Espera := 0 ;
        try
           while (TotalPago = 0 ) and (Espera < 10) do
           begin
              sleep( 200 ) ;
              Inc(Espera) ;
           end;
        except
        end;
     end;

     fsTotalPago := 0 ;
  end ;

  fsTotalPago := fsTotalPago + Valor ;
end;

procedure TACBrECFSweda.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
Var Linhas   : TStringList ;
    I        : Integer ;
    Obs, Cmd : AnsiString ;
    Espera   : Integer ;
begin
  Observacao := AjustaLinhas( Observacao, Colunas, 8 );
  Obs        := '' ;

  Linhas := TStringList.Create ;
  try
     Linhas.Text := Observacao ;

     for I := 0 to min(Linhas.Count-1 ,7) do
        Obs := Obs + '0' + PadRight( Linhas[I] , Colunas) ;
  finally
     Linhas.Free ;
  end ;

  { Fecha cupom }
  Cmd := '12' ;
  if fsVersaoSweda > swdA then          {V0.3 nao tem parametros adcionais }
  begin
     {Versao 1.1 e 9000IIIE nao possui param VINC }
     if (Trim(NumVersao) <> '1.1') and (fsVersaoSweda <> swdD) then
        if fsVinculado > 0 then
           Cmd := Cmd + 'S'
        else
           Cmd := Cmd + 'N' ;

     Cmd := Cmd + 'N' ;              {N = Sem cupom Adcional }
  end ;

  { Pausa após os pagamentos para fechar o cupom, Versao 9000IIIE para de responder }
  if (fsVersaoSweda < swdST) then
     sleep(1000);

  Espera := IfThen( fsVersaoSweda >= swdST, 5,  20) ;
  AguardaImpressao := True ;
  EnviaComando( Cmd + Obs, Espera ) ;

  fsVinculado := 0 ;
  fsTotalPago := -1 ;

  { Sweda pode demorar um pouco para entrar no estado Livre, se tentar
    Abrir cupom antes, o ECF pode para de responder }
  EsperaEstado([estLivre], 5000);
end;

procedure TACBrECFSweda.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString );
Var FlagSubTotal : AnsiString;
begin
  if DescontoAcrescimo < 0 then
   begin
     DescontoAcrescimo := abs(DescontoAcrescimo);

     if fsVersaoSweda  = swdA  then
        EnviaComando( '03'+'        R$'+
                      IntToStrZero(Round(DescontoAcrescimo*100),12) ,10)

     else if (fsVersaoSweda = swdB) then
     begin
       if fsNumVersao='1.1' then
          EnviaComando( '03'+'0000'+
                        IntToStrZero(Round(DescontoAcrescimo*100),12)+'S' ,10)
       else
          EnviaComando( '03'+'        R$'+
                        IntToStrZero(Round(DescontoAcrescimo*100),12)+'S' ,10);
     end
     else if (fsVersaoSweda = swdC) then
        EnviaComando( '03'+'        R$'+
                      IntToStrZero(Round(DescontoAcrescimo*100),12)+'S' ,10)

     else if (fsVersaoSweda = swdD) then
        EnviaComando( '03'+'0000'+
                      IntToStrZero(Round(DescontoAcrescimo*100),12)+'S' ,10)

     else  //if fsVersaoSweda  = swdST then
        EnviaComando( '03'+'0000'+
                      IntToStrZero(Round(DescontoAcrescimo*100),12) ,10);
   end

  else if DescontoAcrescimo > 0 then
   begin
     If fsVersaoSweda < swdB then
        EnviaComando('11' + '53' + '0000' +
                 IntToStrZero(Round(DescontoAcrescimo*100),11) + '   ' + 'N', 3)
     else
      begin
        FlagSubTotal := '';
        if fsVersaoSweda <> swdD then
           FlagSubTotal := 'S';
        EnviaComando('11' + '51' + '0000' +
                  IntToStrZero(Round(DescontoAcrescimo*100),11) +
                  FlagSubTotal , 3);
      end;
   end;

  { Inicia fechamento com formas de Pagamento }
  If fsVersaoSweda > swdA then
     EnviaComando('10');

  fsVinculado := 0;
  fsTotalPago := 0; // 0 define estado em estPagamento
end;


procedure TACBrECFSweda.CancelaDescontoAcrescimoSubTotal(
  TipoAcrescimoDesconto: Char);
begin
{    if TipoAcrescimoDesconto = 'D' then
      TipoAcrescimoDesconto:= '2'
    else if TipoAcrescimoDesconto = 'A' then
      TipoAcrescimoDesconto:= '1';

    EnviaComando('63'+  TipoAcrescimoDesconto);
}
    
    EnviaComando('63'+ '0');
end;


procedure TACBrECFSweda.DescontoAcrescimoItemAnterior(
   ValorDescontoAcrescimo : Double ; DescontoAcrescimo : String ;
   TipoDescontoAcrescimo : String ; NumItem : Integer) ;
begin
  if (fsVersaoSweda >= swdD) then
     EnviaComando('02' + '0000' + IntToStrZero( Round(ValorDescontoAcrescimo * 100) ,12) )
  else
     EnviaComando('02' + PadRight(FormatFloat('00.00',ValorDescontoAcrescimo),10) +
                  IntToStrZero( Round(ValorDescontoAcrescimo * 100) ,12) ) ;
end;

procedure TACBrECFSweda.VendeItem(Codigo, Descricao : String ;
   AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
   ValorDescontoAcrescimo : Double ; Unidade : String ;
   TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
   CodDepartamento : Integer) ;
Var QtdStr, ValorStr, TotalStr, Descr2 : String ;
    ValDesc, ValTotal : Double ;
    wIntervaloAposComando : Integer ;
begin
  { Obs.: Sweda nao usa parametro Unidade }
  if Qtd > 9999 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'Quantidade deve ser inferior a 9999.'));

  { Sweda não permite Acrescimo por Item }
  if (ValorDescontoAcrescimo > 0) and (DescontoAcrescimo = 'A') then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'ECF '+fpModeloStr+' não permite Acréscimo por Item') );

  Codigo  := PadRight(Codigo,13) ;    { Ajustando Tamanhos }
  if Unidade <> '' then
     Descricao := Descricao + ' ' + PadRight(Unidade,2);

  Descr2  := '' ;                 { Usa descriçao Grande ? }
  if DescricaoGrande Then
     Descr2 := copy(Descricao,24,IfThen(fsVersaoSweda >= swdST,209, 40)) ;

  if Length(Trim(Descr2)) = 0 then
     Descr2 := ''
  else
     if (fsVersaoSweda >= swdST) then
        Descr2 := TrimRight(Descr2) 
     else
        Descr2 := PadRight(Descr2,40) ;

  Descricao   := PadRight(Descricao,23) ; {23 e nao 24 porque adiciona o campo Sinal}
  QtdStr      := IntToStrZero( Round( Qtd*1000 ) ,7) ;
  ValorStr    := IntToStrZero( Round( ValorUnitario*1000 ) ,9) ;

  if Arredonda then
     ValTotal := RoundABNT( Qtd*ValorUnitario, -2 )
  else
     ValTotal := RoundTo(TruncFix(Qtd*ValorUnitario*100)/100,-2) ;

  TotalStr := IntToStrZero( Round( ValTotal*100 ) ,12) ;

  // Aumentando IntervaloAposComando para evitar problemas nos comandos abaixo //
  wIntervaloAposComando := IntervaloAposComando ;
  if not fpMFD then
     IntervaloAposComando := max(IntervaloAposComando, 200 ) ;
  try
     EnviaComando( '01' + Codigo + QtdStr + ValorStr + TotalStr + '~' +
                   Descricao + AliquotaECF + Descr2, 5, 5) ;
     fsTotalPago := -1 ;

     if ValorDescontoAcrescimo > 0 then
     begin
        if TipoDescontoAcrescimo = '%' then
           ValDesc := RoundTo( ValTotal * (ValorDescontoAcrescimo / 100), -2)
        else
           ValDesc := ValorDescontoAcrescimo ;

        if (fsVersaoSweda >= swdD) then
           EnviaComando('02' + '0000' + IntToStrZero( Round(ValDesc * 100) ,12) )
        else
           EnviaComando('02' + PadRight(FormatFloat('00.00',ValorDescontoAcrescimo),10) +
                               IntToStrZero( Round(ValDesc * 100) ,12) ) ;
     end ;
  finally
     IntervaloAposComando := wIntervaloAposComando ;
  end ;
end;

procedure TACBrECFSweda.CarregaAliquotas;
Var RetCmd, AliquotasStr, AliqStr : AnsiString ;
    Aliquota : TACBrECFAliquota ;
    ValAliq : Double ;
    A : Integer ;
begin
  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  AliquotasStr := '' ;
  AliqStr      := '' ;

  try
     if fsVersaoSweda < swdB then   { Retorno da 0.3 é muito diferente }
      begin
        RetCmd := EnviaComando('29'+'3') ;
        if copy(RetCmd,1,3) = '.+T' then
        begin
           AliquotasStr := AliquotasStr + copy(RetCmd,49,64) ;

           RetCmd := EnviaComando('29'+'4') ;
           if copy(RetCmd,1,3) = '.+T' then
           begin
              AliquotasStr := AliquotasStr + copy(RetCmd,8,112) ;

              RetCmd := EnviaComando('29'+'5') ;
              if copy(RetCmd,1,3) = '.+T' then
                 AliquotasStr := AliquotasStr + copy(RetCmd,8,64) ;
           end ;
        end ;

        for A := 1 to 15 do
        begin
           AliqStr := copy(AliquotasStr,((A-1) * 16) + 1,16) ;
           ValAliq := StrToIntDef(copy(AliqStr,9,4),0) / 100 ;
           if (StrToIntDef( copy(AliqStr,2,2) ,0) > 0) and
              (pos(copy(AliqStr,1,1),'TS') > 0)        then
           begin
              Aliquota := TACBrECFAliquota.create ;

              Aliquota.Indice   := copy(AliqStr,1,3) ;
              Aliquota.Aliquota := ValAliq ;
              if copy(AliqStr,1,1) = 'S' then
                 Aliquota.Tipo := 'S' ;

              fpAliquotas.Add( Aliquota ) ;
           end ;
        end ;
      end
     else
      begin
        RetCmd := EnviaComando('29'+'3') ;
        if copy(RetCmd,1,3) = '.+T' then
        begin
           AliquotasStr := AliquotasStr + copy(RetCmd,49,28) ;

           RetCmd := EnviaComando('29'+'4') ;
           if copy(RetCmd,1,3) = '.+T' then
           begin
              AliquotasStr := AliquotasStr + copy(RetCmd,8,49) ;

              RetCmd := EnviaComando('29'+'5') ;
              if copy(RetCmd,1,3) = '.+T' then
                  AliquotasStr :=  AliquotasStr + copy(RetCmd,8,28) ;
           end ;
        end ;

        for A := 1 to 15 do
        begin
           AliqStr := copy(AliquotasStr,((A-1) * 7) + 1,7) ;
           ValAliq := StrToIntDef(copy(AliqStr,4,4),0) / 100 ;
           if (StrToIntDef( Trim(copy(AliqStr,2,2)) ,0) > 0) and
              (pos(copy(AliqStr,1,1),'TS') > 0)        then
           begin
              Aliquota := TACBrECFAliquota.create ;

              Aliquota.Indice   := copy(AliqStr,1,3) ;
              Aliquota.Aliquota := ValAliq ;
              if copy(AliqStr,1,1) = 'S' then
                 Aliquota.Tipo := 'S' ;

              fpAliquotas.Add( Aliquota ) ;
           end ;
        end ;
      end ;
  except
     fpAliquotas.Free ;
     fpAliquotas := nil ;
     raise ;
  end ;
end;

procedure TACBrECFSweda.LerTotaisAliquota;
 Var A, Posicao : Integer;
     RetCmd , AliquotasStr : AnsiString;
begin
  if not Assigned( fpAliquotas ) then
     CarregaAliquotas;

  if fsVersaoSweda > swdA then
  begin
    AliquotasStr := '';
    Posicao      := 1;

    RetCmd := EnviaComando( '27'+'3' );
    if LeftStr(RetCmd, 3) = '.+C' then
      AliquotasStr := AliquotasStr + copy(RetCmd,94 ,15);

    RetCmd := EnviaComando( '27'+'4' );
    if LeftStr(RetCmd, 3) = '.+C' then
      AliquotasStr := AliquotasStr + copy(RetCmd,8 ,105);

    RetCmd := EnviaComando( '27'+'5' );
    if LeftStr(RetCmd, 3) = '.+C' then
      AliquotasStr := AliquotasStr + copy(RetCmd,8 ,105);

    For A := 0 to fpAliquotas.Count-1 do
    begin
      fpAliquotas[A].Total := RoundTo( StrToFloatDef(
                    copy(copy(AliquotasStr, Posicao, 15), 4, 12), 0) / 100, -2);
      Posicao := Posicao + 15;
    end;
  end;
end;

procedure TACBrECFSweda.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String);
Var ProxIndice : Integer ;
    ValStr : String ;
    Aliq : TACBrECFAliquota ;
begin
  ValStr := IntToStrZero( Round(Aliquota * 100) ,4) ;
  Tipo := UpCase(Tipo) ;
  if Tipo <> 'S' then
     Tipo := 'T' ;

  CarregaAliquotas ;

  ProxIndice := StrToIntDef(Posicao,0) ;
  if (ProxIndice < 1) or (ProxIndice > 15) then { Indice passado é válido ? }
  begin
     For ProxIndice := 1 to 16 do  { Procurando Lacuna }
     begin
        if (AchaICMSIndice('T'+IntToStrZero(ProxIndice,2)) = nil) and
           (AchaICMSIndice('S'+IntToStrZero(ProxIndice,2)) = nil) then
           break ;
     end ;
  end ;

  if ProxIndice > 15 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Aliquotas'));

  EnviaComando( '33' + Tipo + IntToStrZero(ProxIndice,2) + ValStr ) ;

  { Adcionanodo nova Aliquota no ObjectList }
  Aliq := TACBrECFAliquota.create ;
  Aliq.Indice   := Tipo + IntToStrZero(ProxIndice,2) ;
  Aliq.Aliquota := Aliquota ;
  Aliq.Tipo     := Tipo ;
  fpAliquotas.Add( Aliq ) ;
end;

function TACBrECFSweda.AchaICMSAliquota( var AliquotaICMS: String):
   TACBrECFAliquota;
  Var AliquotaStr : String ;
begin
  AliquotaStr := '' ;
  Result      := nil ;

  if pos(copy(AliquotaICMS,1,2), 'TT,SS') > 0 then { Corrige Duplo T ou S }
     AliquotaICMS := Trim(Copy(AliquotaICMS,2,5));

  if copy(AliquotaICMS,1,2) = 'SF' then
     AliquotaStr := 'FS1'
  else if copy(AliquotaICMS,1,2) = 'SN' then
     AliquotaStr := 'NS1'
  else if copy(AliquotaICMS,1,2) = 'SI' then
     AliquotaStr := 'IS1'
  else
     case AliquotaICMS[1] of
       'I' : AliquotaStr := 'I  ' ;
       'N' : AliquotaStr := 'N  ' ;
       'F' : AliquotaStr := 'F  ' ;
       'T' : AliquotaICMS := 'TT'+PadRight(copy(AliquotaICMS,2,2),2) ; {Indice}
       'S' : AliquotaICMS := 'TS'+PadRight(copy(AliquotaICMS,2,2),2) ; {Indice}
     end ;

  if AliquotaStr = '' then
     Result := inherited AchaICMSAliquota( AliquotaICMS )
  else
     AliquotaICMS := AliquotaStr ;
end;


procedure TACBrECFSweda.CarregaFormasPagamento;
Var RetCmd, Str, Descricao, FlagVinc : AnsiString ;
    Cont, Inicio : Integer ;
    FPagto : TACBrECFFormaPagamento ;
begin
  Str := '' ;
  if fsVersaoSweda < swdD then
   begin
     { Retorno da 0.3 é diferente }
     if fsVersaoSweda > swdA then Inicio := 36 else Inicio := 72 ;
     RetCmd := EnviaComando('29' + '5') ;
     if copy(RetCmd,1,3) = '.+T' then
     begin
        Str := Str + PadRight(copy(RetCmd,81,1),1,'S') +
                     copy(RetCmd, Inicio   , 15) ;
        Str := Str + PadRight(copy(RetCmd,82,1),1,'S') +
                     copy(RetCmd, Inicio+15, 15) ;
        Str := Str + PadRight(copy(RetCmd,83,1),1,'S') +
                     copy(RetCmd, Inicio+30, 15) ;

        RetCmd := EnviaComando('29' + '6') ;
        if copy(RetCmd,1,3) = '.+T' then
        begin
           Str := Str + PadRight(copy(RetCmd,113,1),1,'S') +
                        copy(RetCmd, 8 , 15) ;
           Str := Str + PadRight(copy(RetCmd,114,1),1,'S') +
                        copy(RetCmd, 23, 15) ;
           Str := Str + PadRight(copy(RetCmd,115,1),1,'S') +
                        copy(RetCmd, 38, 15) ;
           Str := Str + PadRight(copy(RetCmd,116,1),1,'S') +
                        copy(RetCmd, 53, 15) ;
           Str := Str + PadRight(copy(RetCmd,117,1),1,'S') +
                        copy(RetCmd, 68, 15) ;
           Str := Str + PadRight(copy(RetCmd,118,1),1,'S') +
                        copy(RetCmd, 83, 15) ;
           Str := Str + PadRight(copy(RetCmd,119,1),1,'S') +
                        copy(RetCmd, 98, 15) ;
        end ;
     end ;
   end
  else
   begin
     RetCmd := EnviaComando('29' + '5') ;
     if copy(RetCmd,1,3) = '.+T' then
     begin
        Str := Str + copy(RetCmd, 36, 48) ;

        RetCmd := EnviaComando('29' + '6') ;
        if copy(RetCmd,1,3) = '.+T' then
        begin
           Str := Str + copy(RetCmd, 8, 112) ;

           RetCmd := EnviaComando('29' + 'Q') ;
           if copy(RetCmd,1,3) = '.+T' then
           begin
              Str := Str + copy(RetCmd, 8, 96) ;

              RetCmd := EnviaComando('29' + 'R') ;
              if copy(RetCmd,1,3) = '.+T' then
                 Str := Str + copy(RetCmd, 8, 64) ;
           end;
        end ;
     end ;
   end ;

  inherited CarregaFormasPagamento ;       {Inicializa fpFormasPagamentos}

  for Cont := 1 to 20 do
  begin
    Descricao := TrimRight( copy(Str, (Cont * 16) - 15, 16) ) ;
    FlagVinc  := copy(Descricao,1,1) ;
    Descricao := copy(Descricao,2,15) ;

    if (Descricao <> '') then
     begin
       FPagto := TACBrECFFormaPagamento.create ;

       FPagto.Indice    := IntToStrZero(Cont,2) ;
       FPagto.Descricao := Descricao ;
       FPagto.PermiteVinculado := (FlagVinc <> 'N') ;

       fpFormasPagamentos.Add( FPagto ) ;
     end
    else
       Break ;
  end
end;

procedure TACBrECFSweda.CarregaRelatoriosGerenciais;
Var
  StrLeg, StrCER, RetCmd, Token1, Token2, Descricao : AnsiString ;
  Cont, CER : Integer ;
  RG  : TACBrECFRelatorioGerencial ;
begin
  inherited CarregaRelatoriosGerenciais ;   {Inicializa fpRelatoriosGerenciais}

  try
    if fpMFD then
    begin
      // Descricao do relatorio gerencial
      RetCmd := EnviaComando('29' + 'M') ;
      if copy(RetCmd,1,3) <> '.+T' then exit ;
      StrLeg := copy(RetCmd, 8, 120) ;

      RetCmd := EnviaComando('29' + 'N') ;
      if copy(RetCmd,1,3) <> '.+T' then exit ;
      StrLeg := StrLeg + copy(RetCmd, 8, 120) ;

      RetCmd := EnviaComando('29' + 'O') ;
      if copy(RetCmd,1,3) <> '.+T' then exit ;
      StrLeg := StrLeg + copy(RetCmd, 8, 120) ;

      RetCmd := EnviaComando('29' + 'P') ;
      if copy(RetCmd,1,3) <> '.+T' then exit ;
      StrLeg := StrLeg + copy(RetCmd, 8, 90) ;

      // Contador de relatorio gerencial
      RetCmd := EnviaComando('27' + 'K') ;
      if copy(RetCmd,1,3) <> '.+C' then exit ;
      StrCER := copy(RetCmd, 8, 120) ;

      for Cont := 1 to 30 do
      begin
        { Adicionando os Relatorios Gerenciais }
        Token1    := copy(StrLeg, ((Cont-1) * 15), 15);
        Descricao := Trim(Token1) ;
        Token2    := copy(StrCER, ((Cont-1) * 4) , 04);
        CER       := StrToIntDef(Token2, 0) ;

        if (Descricao <> '') and (Descricao[2] <> #255) then
        begin
          RG := TACBrECFRelatorioGerencial.create ;
          RG.Indice    := IntToStrZero(Cont,2);
          RG.Descricao := Descricao ;
          RG.Contador  := CER;

          fpRelatoriosGerenciais.Add( RG ) ;
        end ;
      end ;
    end ;
  except
    { Se falhou ao carregar, deve "nilzar" as variaveis para que as rotinas
      "Acha*" tentem carregar novamente }
    fpRelatoriosGerenciais.Free ;
    fpRelatoriosGerenciais := nil ;

    raise ;
  end ;

end;

procedure TACBrECFSweda.LerTotaisRelatoriosGerenciais ;
begin
  CarregaRelatoriosGerenciais;
end ;

procedure TACBrECFSweda.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String);
Var ProxIndice, I : Integer ;
    LenMax        : Integer ;
    FPagto        : TACBrECFFormaPagamento ;
    Cmd, CmdIns   : AnsiString ;
    TemVinculado  : Boolean ;
begin
  if fsVersaoSweda < swdD then
     PermiteVinculado := true ;

  TemVinculado := ( fsVersaoSweda > swdB ) ;

  Cmd    := '' ;
  CmdIns := '' ;
  If TemVinculado then
     If PermiteVinculado then CmdIns := 'S' else CmdIns := 'N' ;
  CmdIns := CmdIns + PadRight(Descricao,15) ;

  if fsVersaoSweda >=  swdST then
     Cmd := CmdIns
  else
   begin
     CarregaFormasPagamento ;
     { Sweda permite reprogramar Todas as FPG, por isso é preciso enviar uma String
       com as FPG já existentes... Criando String com as existentes}
     For I := 0 to FormasPagamento.Count - 1  do
     begin
        if TemVinculado then    { Modelos novos tem Flag de Vinculado }
        begin
           If FormasPagamento[I].PermiteVinculado Then
              Cmd := Cmd + 'S'
           else
              Cmd := Cmd + 'N' ;
           Cmd := Cmd + PadRight(FormasPagamento[I].Descricao,15) ;
        end ;
     end ;

     ProxIndice := StrToIntDef(Posicao,0) ;
     if (ProxIndice < 1) or (ProxIndice > 10) then { Indice passado é válido ? }
        ProxIndice := 0 ;

     if ProxIndice = 0 then
        Cmd := Cmd + CmdIns
     else
        If TemVinculado then
           Cmd := StuffString(Cmd, (ProxIndice*16)-15,0, CmdIns )
        else
           Cmd := StuffString(Cmd, (ProxIndice*15)-14,0, CmdIns ) ;

     if TemVinculado then LenMax := 160 else LenMax := 150 ;
     if Length(Cmd) > LenMax then
        raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Formas de '+
                               'Pagamento'));
   end ;

  EnviaComando( '39' + Cmd ) ;

  { Adcionanodo nova FPG no ObjectList }
  FPagto := TACBrECFFormaPagamento.create ;
  FPagto.Indice    := IntToStrZero(FormasPagamento.Count,2) ;
  FPagto.Descricao := Descricao ;
  FPagto.PermiteVinculado := PermiteVinculado ;
  fpFormasPagamentos.Add( FPagto ) ;
end;

procedure TACBrECFSweda.ProgramaRelatorioGerencial(var Descricao: String; Posicao: String);
Var
  ProxIndice : Integer ;
begin
  CarregaRelatoriosGerenciais ;

  Descricao := Trim(Descricao) ;
  ProxIndice := StrToIntDef(Posicao, -1) ;

  if fpMFD then
   begin
     if AchaRGDescricao(Descricao, True) <> nil then
        raise EACBrECFERRO.Create(ACBrStr('Relatório Gerencial ('+Descricao+') já existe.')) ;

     if (ProxIndice < 2) or (ProxIndice > 30) then { Indice passado é válido ? }
     begin
        For ProxIndice := 2 to 30 do  { Procurando Lacuna }
        begin
           if AchaRGIndice(IntToStrZero(ProxIndice,2)) = nil then
              break ;
        end ;
     end ;

     if ProxIndice > 30 then
        raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novos RGs'));

     EnviaComando( '32' + PadRight(Descricao,15) ) ;
   end
  else
     raise EACBrECFERRO.Create(ACBrStr('Impressoras sem MFD não suportam Programação de Relatórios Gerenciais'));

  CarregaRelatoriosGerenciais ;
end;

procedure TACBrECFSweda.CarregaComprovantesNaoFiscais;
Var RetCmd, Str, Descricao : AnsiString ;
    Cont : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  Str    := '' ;
  RetCmd := EnviaComando('29' + '7') ;
  if copy(RetCmd,1,3) <> '.+T' then exit ;
  Str := Str + copy(RetCmd, 8, 120) ;

  RetCmd := EnviaComando('29' + '8') ;
  if copy(RetCmd,1,3) <> '.+T' then exit ;
  Str := Str + copy(RetCmd, 8, 120) ;

  if ( fsVersaoSweda < swdB ) then { Versão 0.3 tem apenas 20 CNFs }
   begin
     RetCmd := EnviaComando('29' + '9') ;
     if copy(RetCmd,1,3) <> '.+T' then exit ;
     Str := Str + copy(RetCmd, 8, 60) ;
   end
  else
   begin
     RetCmd := EnviaComando('29' + '9') ;
     if copy(RetCmd,1,3) <> '.+T' then exit ;
     Str := Str + copy(RetCmd, 8, 120) ;

     RetCmd := EnviaComando('29' + 'A') ;
     if copy(RetCmd,1,3) <> '.+T' then exit ;
     Str := Str + copy(RetCmd, 8, 120) ;

     RetCmd := EnviaComando('29' + 'B') ;
     if copy(RetCmd,1,3) <> '.+T' then exit ;
     Str := Str + copy(RetCmd, 8, 120) ;

     RetCmd := EnviaComando('29' + 'C') ;
     if copy(RetCmd,1,3) <> '.+T' then exit ;
     Str := Str + copy(RetCmd, 8, 120) ;

     RetCmd := EnviaComando('29' + 'D') ;
     if copy(RetCmd,1,3) <> '.+T' then exit ;
     Str := Str + copy(RetCmd, 8, 60) ;
   end ;

  inherited CarregaComprovantesNaoFiscais ;{Inicializa fpComprovantesNaoFiscais}

  for Cont := 1 to 50 do
  begin
    Descricao := Trim( copy(Str, (Cont * 15) - 14, 15) ) ;

    if Descricao <> '' then
    begin
       CNF := TACBrECFComprovanteNaoFiscal.create ;

       if Cont = 1 then
          CNF.Indice := 'A '
       else if Cont = 2 then
          CNF.Indice := 'B '
       else
          CNF.Indice := IntToStrZero(Cont-2,2) ;
       CNF.Descricao := Descricao ;

       fpComprovantesNaoFiscais.Add( CNF ) ;
    end
  end ;
end;

procedure TACBrECFSweda.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String);
Var ProxIndice, I : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
    Cmd : AnsiString ;
begin
  if Trim(Tipo) = '' then
     Tipo := '+' ;

  if (pos(Tipo,'&+-') = 0) or (Length(Tipo) > 1) then
     raise EACBrECFERRO.Create(ACBrStr('Os Tipos válidos para Sweda são:'+sLineBreak+
                            '&  Criaçao de um novo Grupo (Titulo)'+sLineBreak+
                            '+  Entrada de Recursos'+sLineBreak+
                            '-  Saida de Recursos'+sLineBreak+sLineBreak+
                            'Dentro de um Grupo (titulo) deve ter'+sLineBreak+
                            'apenas legendas de mesmo sinal')) ;

  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais;

  Cmd := '' ;
  ProxIndice := 0 ;
  if fsVersaoSweda < swdST then
  begin
     { Sweda permite reprogramar Todas as CNF, por isso é preciso enviar uma String
       com as CNF já existentes... Criando String com as existentes}
     For I := 2 to ComprovantesNaoFiscais.Count - 1  do
        Cmd := Cmd + PadRight(ComprovantesNaoFiscais[I].Descricao,15) ;

     ProxIndice := StrToIntDef(Posicao,0) ;
     if (ProxIndice < 1) or (ProxIndice > 50) then { Indice passado é válido ? }
        ProxIndice := 0 ;
  end ;

  if ProxIndice = 0 then
     Cmd := Cmd + PadRight(Tipo + Descricao,15)
  else
     Cmd := StuffString(Cmd, (ProxIndice*15)-14,0, PadRight(Tipo + Descricao,15) ) ;

  if Length(Cmd) > 750 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novos Comprovantes'+
                            ' não Fiscais'));

  EnviaComando( '38' + 'N' + Cmd ) ;

  { Adcionanodo novo CNF no ObjectList }
  CNF := TACBrECFComprovanteNaoFiscal.create ;
  CNF.Indice    := IntToStrZero(ProxIndice,2) ;
  CNF.Descricao := Descricao ;
  fpComprovantesNaoFiscais.Add( CNF ) ;
end;


procedure TACBrECFSweda.AbreRelatorioGerencial(Indice: Integer = 0);
Var Espera : Integer ;
    Cmd    : AnsiString ;
  RG  : TACBrECFRelatorioGerencial;
begin
  Espera := IfThen( fsVersaoSweda >= swdST, 10,  50) ;
  Cmd := 'S' ;
{  if fsVersaoSweda >= swdST then
     Cmd := Cmd + PadRight(' ',15) ;
}
  AguardaImpressao := True ;
  if fsVersaoSweda <= swdD then
     fsEsperaMinima := IncSecond( now, 30) ;  { Espera no minimo 30 seg }

  if Indice > 0 then
     begin
       RG  := AchaRGIndice( IntToStrZero(Indice, 2 ) ) ;
       if RG = nil then
          raise EACBrECFERRO.create( ACBrStr('Relatório Gerencial: '+IntToStr(Indice)+
                                  ' não foi cadastrado.' ));
       Cmd := Cmd + PadRight(RG.Descricao,15);
     end;

  EnviaComando( '13' + Cmd ,Espera ) ;
end;

procedure TACBrECFSweda.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  ImprimirLinhaALinha( Linha, '080' ) ;
end;

procedure TACBrECFSweda.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
Var Parcelas : AnsiString ;
    Espera   : Integer ;
begin
  Try
     COO := IntToStrZero(StrToInt(COO),4) ;
  except
     COO := '    ' ;
  end ;

  fsCMDVinculado := COO + CodFormaPagto ;
  Parcelas       := '' ;
  if (fsVersaoSweda >= swdD) or (fsVersaoSweda = swdB) then
     Parcelas:=StringOfChar(' ',20) + '01';
//   Parcelas:=StringOfChar(' ',20) + IntToStrZero(max(fsVinculado,1),2);

  Espera := IfThen( fsVersaoSweda >= swdST, 5,  10) ;
  AguardaImpressao := True ;
  EnviaComando( '1900' + fsCMDVinculado + Parcelas ,Espera ) ;

  { Sweda pode demorar um pouco para entrar no estado Relatorio }
  EsperaEstado([estNaoFiscal]);
  fsEmVinculado := True;
end;

procedure TACBrECFSweda.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha ) ;
end;

procedure TACBrECFSweda.FechaRelatorio;
Var RetCmd,Status,Transacao,Cmd,Param : AnsiString ;
    P,I : Integer ;
    Fechou : Boolean ;
begin
  RetCmd := EnviaComando( '28' ) ;
  P      := pos('!',RetCmd) ;
  Fechou := False ;
  
  if P > 0 then
  begin
     Param := '' ;
     I     := 07 ;
     if (fsVersaoSweda > swdA) then
     begin
        I := 10 ;
        if (fsVersaoSweda = swdB) and (Trim(NumVersao) = '1.1') then
          Param := ''  // Modelo 'B' nao fecha relatorio se envia FLAG - Testado por Ederson
        else if fsVersaoSweda = swdD then
           Param := ''
        else if fsVersaoSweda < swdD then
           Param := 'NN'
        else
           Param := 'N' ;
     end ;

     { Status pode ser: C - concluida, P - Pendente, E - Erro no Comando }
     Status    := UpperCase(copy(RetCmd,I,1)) ;
     Transacao := UpperCase(Trim(copy(RetCmd,I+1,8))) ;
     Cmd       := Trim(copy(RetCmd,I+9,2)) ;

     if ((Status <> 'C') and (Transacao = 'N.FISCAL')) or
        ((Cmd = '08')    and (Transacao = 'VENDAS'))   then
      begin
        AguardaImpressao := True ;
        EnviaComando( '12' + Param, 10 ) ;
        if fsVersaoSweda = swdD then     { Solução provisoria para Liberar o ECFC }
        begin
           AguardaImpressao := True ;
           EnviaComando( '12N', 10 ) ;
           Fechou := True ;
        end ;
      end
     else if (Status <> 'C') and (Transacao = 'LEIT. X') then
      begin
        AguardaImpressao := True ;
        EnviaComando('08', 10) ;
        Fechou := True ;
      end ;
  end ;

  {Gato para resolver o erro -P111+0065 neste modelo logo após um vinculado}
  if (fsVersaoSweda=swdB) and (NumVersao='1.1') then 
  begin
     if fsEmVinculado then 
     begin
        fsEmVinculado := False;
       
        try
          EnviaComando('13N');
          DataHora;
        except
        end;
     end;
  end;

  { Sweda pode demorar um pouco para entrar no estado Livre, se tentar
    Abrir cupom antes, o ECF pode para de responder }
  if Fechou then
     EsperaEstado([estLivre]);
end;

procedure TACBrECFSweda.PulaLinhas(NumLinhas: Integer);
begin
  if NumLinhas = 0 then
     NumLinhas := LinhasEntreCupons ;

  NumLinhas := min(NumLinhas,9) ;

  if fsVersaoSweda = swdA then   // Correção por SAULI BUENO
     EnviaComando('089D'+IntToStrZero(NumLinhas,1))
  else
     EnviaComando('089'+IntToStrZero(NumLinhas,1));
end;

procedure TACBrECFSweda.CortaPapel(const CorteParcial: Boolean);
begin
  { Infelizmente parece que a Sweda não permite acionar a guilhotina com o
    Vinculado ou Gerencia aberto  :( }
  if not fpMFD then
     inherited CortaPapel
  else
     EnviaComando('087') ;
end;


procedure TACBrECFSweda.ListaRelatorioGerencial(Relatorio: TStrings;
  Vias: Integer; Indice: Integer);
Var Imp, Linha : Integer ;
    Texto : AnsiString ;
    Bufferiza : Boolean ;
begin
(* OBS: A SWEDA 1.0 e modelo < F, apenas inicia a impressão das Linhas do
Relatório apos o envio do comando de Fechamento .08} por isso não há como
fazer Pausa entre as vias a Mensagem enviada ao usuário também foi modificada *)

  Imp := 0 ;
  Bufferiza := (fsVersaoSweda < swdC) ;

  while Imp < Vias do
  begin
    if Bufferiza then
       Texto :=  Format('Enviando %s  %dª Via ',['Relatório Gerencial',Imp+1 ])
    else
       try
          Texto := Format(MsgRelatorio,['Relatório Gerencial',Imp+1 ]) ;
       except
          Texto := MsgRelatorio ;
       end ;

     {$IFNDEF NOGUI}
       FormMsgPinta( Texto );
     {$ENDIF}

     Linha := 0 ;
     while Linha <= ( Relatorio.Count - 1) do
     begin
        TACBrECF(fpOwner).LinhaRelatorioGerencial( LeftStr( Relatorio[ Linha ], fpColunas) ) ;
        Linha := Linha + 1 ;

        {$IFNDEF NOGUI}
          if fpDevice.ProcessMessages then
             Application.ProcessMessages ;
        {$ENDIF}
     end ;

     Imp := Imp + 1 ;

     if Imp < Vias then
     begin
        TACBrECF(fpOwner).PulaLinhas ;
        TACBrECF(fpOwner).CortaPapel ;
        if not Bufferiza then
           PausarRelatorio( Imp ) ;
     end ;
  end ;

  {$IFNDEF NOGUI}
    if Bufferiza then
       FormMsgPinta( 'Imprimindo Relatório Gerencial' )
    else
       FormMsgPinta( 'Fechando Relatório Gerencial' );
  {$ENDIF}

  TACBrECF(fpOwner).FechaRelatorio ;
end;

procedure TACBrECFSweda.ListaCupomVinculado(Relatorio: TStrings;
  Vias: Integer);
Var Imp, Linha : Integer ;
    Texto : AnsiString ;
    Bufferiza : Boolean ;
begin
(* OBS: A SWEDA 1.0 e modelo < F, apenas inicia a impressão das Linhas do
Relatório apos o envio do comando de Fechamento .08} por isso não há como
fazer Pausa entre as vias a Mensagem enviada ao usuário também foi modificada *)

  Imp := 0 ;
  Bufferiza := (fsVersaoSweda < swdC) ;

  if Bufferiza then
   begin
     while Imp < Vias do
     begin
        Texto :=  Format('Enviando %s  %dª Via ',['Cupom Vinculado',Imp+1 ]) ;
        {$IFNDEF NOGUI}
          FormMsgPinta( Texto );
        {$ENDIF}

        Linha := 0 ;
        while Linha <= ( Relatorio.Count - 1) do
        begin
           LinhaCupomVinculado( LeftStr( Relatorio[ Linha ], fpColunas) ) ;
           Linha := Linha + 1 ;

           {$IFNDEF NOGUI}
             if fpDevice.ProcessMessages then
                Application.ProcessMessages ;
           {$ENDIF}
        end ;

        Imp := Imp + 1 ;

        if Imp < Vias then
        begin
           PulaLinhas ;
           EnviaComando('1900'+fsCMDVinculado) ;
        end ;
     end ;

     { Buffer já está cheio, imprimindo todas as vias}
     while Imp > 0 do
     begin
        FechaRelatorio ;

        if Imp > 1 then
           PausarRelatorio( Imp ) ;

        Imp := Imp - 1 ;
     end ;
   end
  else
     inherited ListaCupomVinculado(Relatorio, Vias);

end;

function TACBrECFSweda.LeituraCMC7: AnsiString;
Var
  RetCmd : AnsiString ;
begin
  Result := '' ;
  if (fpMFD) and (fsVersaoSweda >= swdST) then
  begin
     RetCmd := EnviaComando( '49'+'2', 13) ;

     if LeftStr(RetCmd, 2) = '.{' then
        Result := Copy( RetCmd, 3, Length(RetCmd)-3 );
  end;
  Sleep(100) ;
end;

procedure TACBrECFSweda.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : String ;
begin
  Espera := 180 ;
  Flag   := '' ;
  if fsVersaoSweda >= swdST then
   begin
     Espera := 40 ;
     if Simplificada then
        Flag := 'NS' ;  // N - captura, S - simples
   end
  else if fsVersaoSweda = swdB then
     Espera := 400 ;

  Espera := Espera + (ReducaoFinal - ReducaoInicial) ;

  AguardaImpressao := True ;
  EnviaComando( '15' + IntToStrZero(ReducaoInicial,4) +
                       IntToStrZero(ReducaoFinal  ,4) + Flag , Espera ) ;
end;

procedure TACBrECFSweda.LeituraMemoriaFiscal(DataInicial, DataFinal: TDateTime;
   Simplificada : Boolean);
  Var Espera : Integer ;
       Flag  : String ;
begin
  Espera := 180 ;
  Flag   := '' ;
  if fsVersaoSweda >= swdST then
   begin
     Espera := 40 ;
     if Simplificada then
        Flag := 'NS' ;  // N - captura, S - simples
   end
  else if fsVersaoSweda = swdB then
     Espera := 400 ;

  Espera := Espera + DaysBetween(DataInicial, DataFinal) ;

  AguardaImpressao := True ;
  EnviaComando('16' + FormatDateTime('ddmmyy',DataInicial) +
                      FormatDateTime('ddmmyy',DataFinal)   + Flag, Espera ) ;
end;

procedure TACBrECFSweda.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas : TStringList; Simplificada : Boolean);
 Var Flag : String ;
begin
  Flag := '' ;
  if fsVersaoSweda >= swdST then
     if Simplificada then
        Flag := 'S' ;

  LeBufferSerial( '15' + IntToStrZero(ReducaoInicial,4) +
                         IntToStrZero(ReducaoFinal  ,4) + '|' +
                         Flag, Linhas );
end;

procedure TACBrECFSweda.LeituraMemoriaFiscalSerial(DataInicial,
  DataFinal: TDateTime; Linhas : TStringList; Simplificada : Boolean);
 Var Flag      : String ;
begin
  Flag := '' ;
  if fsVersaoSweda >= swdST then
     if Simplificada then
        Flag := 'S' ;

  LeBufferSerial( '16' + FormatDateTime('ddmmyy',DataInicial)+
                         FormatDateTime('ddmmyy',DataFinal)  + '|' +
                         Flag , Linhas );
end;

function TACBrECFSweda.GetChequePronto: Boolean;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando('23') ;
  Result := (copy( RetCmd, 5,1) = '0') ;
end;

procedure TACBrECFSweda.LeituraMFDSerial(COOInicial, COOFinal: Integer;
  Linhas: TStringList; Documentos : TACBrECFTipoDocumentoSet);

begin

  if fsVersaoSweda >= swdST then

  LeBufferSerial( '55' + IntToStrZero(COOInicial,6) +
                         IntToStrZero(COOFinal  ,6) + '#', Linhas );
  Sleep(300) ;

end;

procedure TACBrECFSweda.LeituraMFDSerial(DataInicial,
  DataFinal: TDateTime; Linhas: TStringList;
  Documentos : TACBrECFTipoDocumentoSet);

begin

  if fsVersaoSweda >= swdST then

  LeBufferSerial( '56' + FormatDateTime('ddmmyy',DataInicial)+
                         FormatDateTime('ddmmyy',DataFinal)  + '#', Linhas );
//WriteToTXT('d:\temp\mfd_limpo.txt',Linhas.Text, False);
  Sleep(300) ;

end;

procedure TACBrECFSweda.CancelaImpressaoCheque;
begin
  EnviaComando( '25' ) ;
end;

procedure TACBrECFSweda.ImprimeCheque(Banco: String; Valor: Double;
  Favorecido, Cidade: String; Data: TDateTime; Observacao: String);
Var Dia,Mes,Ano   : String ;
    Moeda, Moedas : String ;
    wRetentar     : Boolean ;
    ComPapel      : Boolean ;
    SecRestante   : Integer ;
    TempoInicio   : TDateTime ;
    Espera        : Integer ;
begin
  if not ChequePronto then
     raise EACBrECFERRO.Create(ACBrStr('Cheque não está posicionado')) ;

  Espera     := 25 ;
  Banco      := IntToStrZero(StrToIntDef(Banco,1),3) ;
  Favorecido := PadRight(Favorecido,80) ;
  Cidade     := PadRight(Cidade,30) ;
  Moeda      := PadRight('Real',20) ;
  Moedas     := PadRight('Reais',20) ;

  EnviaComando('44' + Favorecido + Cidade + Moeda + Moedas) ;
  Sleep(300) ;

  Observacao := PadRight(Observacao,120) ;
  Dia        := IntToStrZero(  DayOf(Data),2) ;
  Mes        := IntToStrZero(MonthOf(Data),2) ;
  Ano        := IntToStrZero( YearOf(Data),4) ;

  ComPapel   := True ;
  wRetentar  := Retentar ;
  try
     Retentar    := False ;
     TempoInicio := now  ;
     AguardaImpressao := True ;
     EnviaComando('24' + Banco + IntToStrZero(Round(Valor * 100),12) +
                  ' ' + Observacao + '4' + Dia + Mes + Ano, Espera ) ;

     { Sweda série B precisa de 2 comandos 24 para imprimir o cheque. O primeiro
       comando apenas puxa a folha, o segundo realmente imprime
       http://www.forumweb.com.br/foruns/index.php?s=&showtopic=50409&view=findpost&p=224119 }
     if fsVersaoSweda = swdB then
     begin
       TempoInicio := now  ;
       AguardaImpressao := True ;
       EnviaComando('24' + Banco + IntToStrZero(Round(Valor * 100),12) +
                    ' ' + Observacao + '4' + Dia + Mes + Ano, Espera ) ;
     end ;

     while ComPapel do
     begin
        SecRestante := 1 ;
        while ComPapel and (SecRestante > 0 ) do
        begin
           try
              ComPapel := ChequePronto
           except
              ComPapel := True ;
              Sleep(100) ;
           end ;
           SecRestante := Espera - SecondsBetween(now, TempoInicio) ;
        end ;

        if ComPapel then
           {$IFNDEF NOGUI}
              if (MessageDlg( ACBrStr('Favor remover o cheque e pressionar OK'),
                             {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtConfirmation,[{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbOk,{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbCancel],0) = mrCancel) then
                 break ;
           {$ELSE}
              writeln('Favor remover o cheque') ;
           {$ENDIF}

        TempoInicio := now ;
        Espera      := 5 ;
     end ;
  finally
     Retentar := wRetentar ;
  end ;
end;

procedure TACBrECFSweda.EsperaEstado(EstadoAEsperar: TACBrECFEstadoSet;
   TimeOut : Integer = 2000 );
Var TFim : TDateTime ;
begin
  TFim := IncMilliSecond(now,TimeOut) ;

  while (now <= TFim) do
  begin
     try
        if Estado in EstadoAEsperar then
           break ;
     except
     end ;

     GravaLog('   Esperando Estado' );
     sleep( 100 ) ;
  end ;
end;

function TACBrECFSweda.GetCNPJ: String;
 var
  wretorno: Ansistring;
  I : Integer ;
begin
  Result := '';
  I      := 0 ;
  while I < 5 do
  begin
     wretorno := EnviaComando('29'+ AnsiChar( chr(72+I) ));   // 72 = H em ASCII
     if (copy(wretorno,1,3) = '.+T') and (copy(wretorno,8,22) <> Space(22) ) then
        Result := Copy(wretorno,8,22);
     I := I + 1 ;
  end ;
end;

function TACBrECFSweda.GetIE: String;
 var
  wretorno: Ansistring;
  I : Integer ;
begin
  Result   := '';
  I      := 0 ;
  while I < 5 do
  begin
     wretorno := EnviaComando('29'+ AnsiChar( chr(72+I) ));   // 72 = H em ASCII
     if (copy(wretorno,1,3) = '.+T') and (copy(wretorno,8,22) <> Space(22) ) then
        Result := Copy(wretorno,30,21);
     I := I + 1 ;
  end ;
end;

function TACBrECFSweda.GetIM: String;
 var
  wretorno: Ansistring;
  I : Integer ;
begin
  Result   := '';
  I      := 0 ;
  while I < 5 do
  begin
     wretorno := EnviaComando('29'+ AnsiChar( chr(72+I) ));   // 72 = H em ASCII
     if (copy(wretorno,1,3) = '.+T') and (copy(wretorno,8,22) <> Space(22) ) then
        Result := Copy(wretorno,51,16);
     I := I + 1 ;
  end ;
end;

function TACBrECFSweda.GetCliche: AnsiString;
var
  wretorno, Tipo1, Tipo2, Tipo3 : Ansistring;
begin
  Result   := '';
  Tipo1    := '';
  Tipo2    := '';
  Tipo3    := '';

  begin
     wretorno := EnviaComando('29'+ '1');
     if copy(wretorno,1,3) = '.+T' then
        Tipo1 := Copy(wretorno,12,40) +
                 AnsiChar( chr(13)) + AnsiChar( chr(10)) +   //IMS Força a Quebra de linha
                 Copy(wretorno,53,40) +
                 AnsiChar( chr(13)) + AnsiChar( chr(10)) ;

     wretorno := EnviaComando('29'+ '2');
     if copy(wretorno,1,3) = '.+T' then
        Tipo2 := Copy(wretorno,09,40) +
                 AnsiChar( chr(13)) + AnsiChar( chr(10)) +
                 Copy(wretorno,50,40) +
                 AnsiChar( chr(13)) + AnsiChar( chr(10)) ;

     wretorno := EnviaComando('29'+ '3');
     if copy(wretorno,1,3) = '.+T' then
        Tipo3 := Copy(wretorno,9,40) ;

     Result := Tipo1 + Tipo2 + Tipo3 ;

  end ;
end;

function TACBrECFSweda.GetUsuarioAtual: String;
 var
  wretorno: Ansistring;
  I      : Integer ;
begin
  Result := '';
  I      := 0 ;
  while I < 5 do
  begin
     wretorno := EnviaComando('29'+ AnsiChar( chr(72+I) ));   // 72 = H em ASCII
     if (copy(wretorno,1,3) = '.+T') and (copy(wretorno,8,22) <> Space(22) ) then
        if copy(wretorno,7,1) = 'H' then
           Result := '0001'
        else if copy(wretorno,7,1) = 'I' then
           Result := '0002'
        else if copy(wretorno,7,1) = 'J' then
           Result := '0003'
        else if copy(wretorno,7,1) = 'K' then
           Result := '0004'
        else if copy(wretorno,7,1) = 'L' then
           Result := '0005' ;

     I := I + 1 ;
  end ;
end;

function TACBrECFSweda.GetDataHoraSB: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
    P : Integer ;
begin
  RetCmd := EnviaComando( '29H' ) ;
  P      := pos('H',RetCmd) ;
  Result := 0 ;
  if P > 0 then
     if fpMFD then
     begin
        OldShortDateFormat := ShortDateFormat ;
        try
           ShortDateFormat := 'dd/mm/yy' ;
           result := StrToDate(copy(RetCmd,71,2)+ DateSeparator +
                               copy(RetCmd,73,2)+ DateSeparator +
                               copy(RetCmd,75,2)) ;
           result := RecodeHour(  result,StrToIntDef(copy(RetCmd,77,2),0)) ;
           result := RecodeMinute(result,StrToIntDef(copy(RetCmd,79,2),0)) ;
           { Obs.: Sweda nao retorna os Segundos }
        finally
           ShortDateFormat := OldShortDateFormat
        end
     end
  else
     begin
        OldShortDateFormat := ShortDateFormat ;
        try
           ShortDateFormat := 'dd/mm/yy' ;
           result := StrToDate(copy(RetCmd,67,2)+ DateSeparator +
                               copy(RetCmd,69,2)+ DateSeparator +
                               copy(RetCmd,71,2)) ;
           result := RecodeHour(  result,StrToIntDef(copy(RetCmd,73,2),0)) ;
           result := RecodeMinute(result,StrToIntDef(copy(RetCmd,75,2),0)) ;
           { Obs.: Sweda nao retorna os Segundos }
        finally
           ShortDateFormat := OldShortDateFormat
        end
     end
end;


function TACBrECFSweda.GetSubModeloECF: String; //Tem que aprimorar esta rotina, para identificar somente um tipo
begin
  Result := fsSubModeloECF ;
end;

function TACBrECFSweda.GetDataMovimento: TDateTime;
var
  wretorno, OldShortDateFormat: AnsiString;
begin
  Result   := Date;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
  begin
     OldShortDateFormat := ShortDateFormat ;
     try
        ShortDateFormat := 'dd/mm/yy' ;
        Result := StrToDate( copy(wretorno,8 ,2) + DateSeparator +
                             copy(wretorno,10,2) + DateSeparator +
                             copy(wretorno,12,2) );
     finally
        ShortDateFormat := OldShortDateFormat ;
     end ;
  end ;
end;

function TACBrECFSweda.GetGrandeTotal: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,20,17),0)/100;
end;

function TACBrECFSweda.GetNumCOOInicial: String;
var
  wretorno: AnsiString;
begin
  Result   := '';

  if fsVersaoSweda = swdA then
   begin
     wretorno := EnviaComando('27'+'1');
     if copy(wretorno,1,3) = '.+C' then
        Result := Copy(wretorno,14,4)
   end
  else
  if (fsVersaoSweda > swdA) and (fsVersaoSweda < swdST) then
   begin
     wretorno := EnviaComando('27'+'G');
     if copy(wretorno,1,3) = '.+C' then
        Result := Copy(wretorno,8,4)
   end
  else
   begin
     wretorno := EnviaComando('27'+'H');
     if copy(wretorno,1,3) = '.+C' then
        Result := Copy(wretorno,49,6)
  end;
end;

function TACBrECFSweda.GetNumCRZ: String;
var
  wretorno: AnsiString;
begin
  Result   := '';
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
     Result:=Copy(wretorno,41,4);
end;

function TACBrECFSweda.GetNumUltimoItem: Integer;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('28');
  if copy(wretorno,1,3) = '.+0' then
     Result := StrToInt(copy(wretorno,7,3));
end;

function TACBrECFSweda.GetTotalAcrescimos: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'F');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,12,12),0)/100;
end;

function TACBrECFSweda.GetTotalAcrescimosISSQN: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'F');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,44,12),0)/100;
end;

function TACBrECFSweda.GetTotalCancelamentos: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
  begin
     Result := StrToFloatDef(copy(wretorno,61,12),0)/100;
     Result := Result + (StrToFloatDef(copy(wretorno,77,12),0)/100);
  end;
end;

function TACBrECFSweda.GetTotalCancelamentosISSQN: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'2');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,28,12),0)/100;
end;

function TACBrECFSweda.GetTotalDescontos: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,93,12),0)/100;
end;

function TACBrECFSweda.GetTotalTroco: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'7');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,56,12),0)/100;
end;

function TACBrECFSweda.GetTotalDescontosISSQN: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'2');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,44,12),0)/100;
end;

function TACBrECFSweda.GetTotalIsencao: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'3');
  if copy(wretorno,1,3) = '.+C' then
  begin
     if (StrToFloatDef(copy(wretorno,22,12),0)/100) > 0 then
        Result := StrToFloatDef(copy(wretorno,22,12),0) / 100;
  end;
end;

function TACBrECFSweda.GetTotalIsencaoISSQN: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'J');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,80,12),0)/100;
end;

function TACBrECFSweda.GetTotalNaoTributado: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'3');
  if copy(wretorno,1,3) = '.+C' then
  begin
     if StrToFloatDef(copy(wretorno,34,12),0)/100 > 0 then
        Result := StrToFloatDef(copy(wretorno,34,12),0) / 100;
end;
end;

function TACBrECFSweda.GetTotalNaoTributadoISSQN: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'J');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,44,12),0)/100;
end;

function TACBrECFSweda.GetTotalSubstituicaoTributaria: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'3');
  if copy(wretorno,1,3) = '.+C' then
  begin
     if StrToFloatDef(copy(wretorno,46,12),0)/100 > 0 then
        Result := StrToFloatDef(copy(wretorno,46,12),0) / 100;
  end;
end;

function TACBrECFSweda.GetTotalSubstituicaoTributariaISSQN: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'J');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,8,12),0)/100;
end;

function TACBrECFSweda.GetVendaBruta: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,45,12),0)/100;
end;

procedure TACBrECFSweda.LerTotaisComprovanteNaoFiscal;
 Var I : Integer;
     RetCmd , CNFStr, Token : AnsiString;
begin
  if fsVersaoSweda <= swdA then exit ;  { Incompativel com o retorno da 0.3 }

  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais;

  CNFStr := '';

  RetCmd := EnviaComando( '27'+'7' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,88 ,32);

  RetCmd := EnviaComando( '27'+'8' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'9' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'A' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'B' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'C' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'D' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'E' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,96);

  For I := 2 to fpComprovantesNaoFiscais.Count-1 do
  begin
     Token := copy( CNFStr, ((I-2)*16)+1, 16 ) ;
     fpComprovantesNaoFiscais[I].Contador :=  StrToIntDef( copy(Token,1,4), 0);
     fpComprovantesNaoFiscais[I].Total :=
         RoundTo( StrToFloatDef( copy(Token, 5, 12), 0) / 100, -2);
  end;
end;

procedure TACBrECFSweda.LerTotaisFormaPagamento;
 Var A : Integer;
     RetCmd , FPGStr, Token : AnsiString;
begin
  if fsVersaoSweda <= swdA then exit ;  { Incompativel com o retorno da 0.3 }

  if not Assigned( fpFormasPagamentos ) then
     CarregaFormasPagamento ;

  FPGStr := '';

  RetCmd := EnviaComando( '27'+'6' );
  if LeftStr(RetCmd, 3) = '.+C' then
     FPGStr := FPGStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'7' );
  if LeftStr(RetCmd, 3) = '.+C' then
     FPGStr := FPGStr + copy(RetCmd,8 ,48);

  if fsVersaoSweda >= swdST then
  begin
     RetCmd := EnviaComando( '27'+'L' );
     if LeftStr(RetCmd, 3) = '.+C' then
     begin
        RetCmd := copy(RetCmd,8,120) ;
        Token := '' ;
        For A := 0 to 9 do  { Ajustando para o formato de '6' e '7' }
           Token := Token + '0000' + copy( RetCmd, (A*12)+1, 12 ) ;
        FPGStr := FPGStr + Token ;
     end ;
  end ;

  For A := 0 to fpFormasPagamentos.Count-1 do
  begin
     Token := copy( FPGStr, (A*16)+1, 16 ) ;
     fpFormasPagamentos[A].Total :=
                RoundTo( StrToFloatDef( copy(Token, 5, 12), 0) / 100, -2);
  end;
end;

procedure TACBrECFSweda.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  { Nao abre ainda... Precisa saber o CodCNF que será registrado para achar o
    Título... veja abaixo }
end;

procedure TACBrECFSweda.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString);
 Var Descr : String ;
     Titulo: String ;
     Espera, P : Integer ;
     CNF : TACBrECFComprovanteNaoFiscal ;
begin
  if Estado <> estNaoFiscal then
  begin
     { Achando o Titulo para o indice informado (primeiro CNF anteriro com "&") }
     Titulo := '' ;
     try
        P := StrToInt(CodCNF) - 1;
        while (P > 0) and (Titulo = '') do
        begin
           CNF := AchaCNFIndice(IntToStrZero(P,2)) ;
           if CNF = nil then
              break ;

           if Pos('&', copy(CNF.Descricao,1,3)) > 0  then
              Titulo := CNF.Indice ;

           Dec( P ) ;
        end ;
     except
     end ;

     if Titulo = '' then
        Titulo := CodCNF ;
        
     { Abrindo o NaoFiscal com o Titulo que achou }
     Espera := IfThen( fsVersaoSweda >= swdST, 5,  15);
     AguardaImpressao := True ;
     EnviaComando('19'+Titulo, Espera) ;
     fsTotalPago := -1 ;
  end ;

  { Registrando o Item Não fiscal }
  Descr := '' ;
  if fsVersaoSweda > swdA then
     Descr := StringOfChar(' ',40) ;

  EnviaComando('07'+ CodCNF +
                     IntToStrZero(Round(Valor * 100) ,12) + Descr ) ;
end;

procedure TACBrECFSweda.NaoFiscalCompleto(CodCNF: String; Valor: Double;
  CodFormaPagto: String; Obs: AnsiString; IndiceBMP : Integer);
begin
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
        FechaNaoFiscal( Obs );
     except
        try
           CancelaNaoFiscal
        except
        end;

        raise ;
     end ;
  end ;
end;

function TACBrECFSweda.AchaCNFDescricao( Descricao: String;
   BuscaExata : Boolean; IgnorarCase : Boolean ): TACBrECFComprovanteNaoFiscal;
 var A, P : Integer ;
     DescECF : String ;
begin
  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais ;

  result := nil ;
  with fpComprovantesNaoFiscais do
  begin
     Descricao := Trim(Descricao) ;
     if IgnorarCase then
        Descricao := UpperCase(Descricao) ;
     For A := 0 to Count -1 do
     begin
        DescECF := Objects[A].Descricao ;
        if IgnorarCase then
           DescECF := UpperCase(DescECF) ;
        P := pos( Descricao, DescECF ) ;
        if (P > 0) and (P < 3) and
           (( not BuscaExata) or (copy(DescECF,P,Length(DescECF)) = Descricao))  then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFSweda.GetNumGNF: String;
 Var RetCmd : AnsiString ;
     Tentavias : Integer ;
begin
  Result    := '' ;

  For Tentavias := 1 to 3 do
  begin
     if fpMFD then
      begin
        RetCmd := EnviaComando( '27'+'H' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,49,6), 0), 6) ;
      end
     else
      begin
        RetCmd := EnviaComando( '27'+'1' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,117,4), 0), 6) ;
      end ;

      if Result <> '' then
         break ;

      Sleep(100) ;
  end ;
end;

function TACBrECFSweda.GetNumGRG: String;
 Var RetCmd : AnsiString ;
     Tentavias : Integer ;
begin
  Result    := '' ;

  For Tentavias := 1 to 3 do
  begin
     if fpMFD then
      begin
        RetCmd := EnviaComando( '27'+'H' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,55,6), 0), 6) ;
      end;

      if Result <> '' then
         break ;

      Sleep(100) ;
  end ;
end;

function TACBrECFSweda.GetNumCDC: String;
 Var RetCmd : AnsiString ;
     Tentavias : Integer ;
begin
  Result    := '' ;

  For Tentavias := 1 to 3 do
  begin
     if fpMFD then
      begin
        RetCmd := EnviaComando( '27'+'H' ) ;

        if LeftStr(RetCmd, 3) = '.+C' then
           Result := IntToStrZero( StrToIntDef( copy(RetCmd,79,4), 0), 4) ;
      end;

      if Result <> '' then
         break ;

      Sleep(100) ;
  end ;
end;

procedure TACBrECFSweda.IdentificaPAF(NomeVersao, MD5 : String) ;
begin
  EnviaComando('57'+ PadRight(MD5,42) + PadRight(NomeVersao,42)) ;
end;

procedure TACBrECFSweda.LoadDLLFunctions;
var
  sLibName: string;

 procedure SwedaFunctionDetect( FuncName: String; var LibPointer: Pointer ) ;
 begin
   if not Assigned( LibPointer )  then
   begin
     if not FunctionDetect( sLibName, FuncName, LibPointer) then
     begin
        LibPointer := NIL ;
        raise EACBrECFERRO.Create( ACBrStr( 'Erro ao carregar a função:'+FuncName+' de: '+cLIB_Sweda ) ) ;
     end ;
   end ;
 end ;
begin
  // Verifica se exite o caminho das DLLs
  if Length(PathDLL) > 0 then
     sLibName := PathWithDelim(PathDLL);

  // Concatena o caminho se exitir mais o nome da DLL.
  sLibName := sLibName + cLIB_Sweda;

  {$IFDEF MSWINDOWS}
    if not FileExists( ExtractFilePath( sLibName ) + 'Swmfd.dll') then
       raise EACBrECFERRO.Create( ACBrStr( 'Não foi encontrada a dll auxiliar Swmfd.dll.' ) ) ;
   {$ENDIF}
   SysUtils.DeleteFile( ExtractFilePath( sLibName ) + 'SWC.INI');

   SwedaFunctionDetect('ECF_AbrePortaSerial', @xECF_AbrePortaSerial);
   SwedaFunctionDetect('ECF_DownloadMFD', @xECF_DownloadMFD);
   SwedaFunctionDetect('ECF_ReproduzirMemoriaFiscalMFD', @xECF_ReproduzirMemoriaFiscalMFD);
   SwedaFunctionDetect('ECF_FechaPortaSerial', @xECF_FechaPortaSerial);
   SwedaFunctionDetect('ECF_DownloadMF',@xECF_DownloadMF);
end ;


procedure TACBrECFSweda.AbrePortaSerialDLL ;
Var
//Porta : Integer ;
  Resp : Integer ;
begin
//Porta := StrToIntDef( OnlyNumber( fpDevice.Porta ), 0) ;

  Resp := xECF_AbrePortaSerial;
  if Resp <> 1 then
     raise EACBrECFERRO.Create( ACBrStr('Erro: '+IntToStr(Resp)+' ao abrir a Porta com:'+sLineBreak+
        'ECF_AbrePortaSerial'));
end ;

procedure TACBrECFSweda.EspelhoMFD_DLL(DataInicial,
  DataFinal: TDateTime; const NomeArquivo: AnsiString;
  Documentos: TACBrECFTipoDocumentoSet);
Var
  Resp : Integer ;
  DiaIni, DiaFim : AnsiString ;
  OldAtivo : Boolean ;
  OldDateSeparator: Char;
  OldShortDateFormat : String;
begin
  LoadDLLFunctions ;

  OldAtivo := Ativo ;
  OldShortDateFormat := ShortDateFormat ;
  OldDateSeparator   := DateSeparator;
  try
    Ativo := False ;

    AbrePortaSerialDLL ;

    DateSeparator   := '/';
    ShortDateFormat := 'dd/mm/yy' ;
    DiaIni := FormatDateTime('DD/MM/YY',DataInicial) ;
    DiaFim := FormatDateTime('DD/MM/YY',DataFinal) ;

    Resp := xECF_DownloadMFD( NomeArquivo, '1', DiaIni, DiaFim, '0');
    if (Resp <> 1) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar ECF_DownloadMFD.'+sLineBreak+
                                       'Cod.: '+IntToStr(Resp) ))
  finally
    DateSeparator   := OldDateSeparator;
    ShortDateFormat := OldShortDateFormat;
    xECF_FechaPortaSerial ;
    Ativo := OldAtivo ;
  end ;

  if not FileExists( NomeArquivo ) then
     raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de ECF_DownloadMFD.'+sLineBreak+
                            'Arquivo: "'+NomeArquivo+'" não gerado' ))
end;


procedure TACBrECFSweda.EspelhoMFD_DLL(COOInicial,
  COOFinal: Integer; const NomeArquivo: AnsiString;
  Documentos: TACBrECFTipoDocumentoSet);
Var
  Resp : Integer ;
  CooIni, CooFim : String ;
  OldAtivo : Boolean ;
begin
  LoadDLLFunctions ;

  OldAtivo := Ativo ;
  try
    Ativo := False ;

    AbrePortaSerialDLL ;

    CooIni := IntToStrZero( COOInicial, 6 ) ;
    CooFim := IntToStrZero( COOFinal, 6 ) ;

    Resp := xECF_DownloadMFD( NomeArquivo, '2', CooIni, CooFim, '0');
    if (Resp <> 1) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar ECF_DownloadMFD.'+sLineBreak+
                                       'Cod.: '+IntToStr(Resp) ));
  finally
    xECF_FechaPortaSerial ;
    Ativo := OldAtivo ;
  end ;

  if not FileExists( NomeArquivo ) then
     raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de ECF_DownloadMFD.'+sLineBreak+
                            'Arquivo: "'+NomeArquivo + '" não gerado' ))
end;


procedure TACBrECFSweda.ArquivoMFD_DLL(DataInicial, DataFinal: TDateTime;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD);
Var
  Resp : Integer ;
  DiaIni, DiaFim : AnsiString ;
  OldAtivo : Boolean ;
  PathBin:AnsiString;
  oldDateSeparator: Char;
  OldShortDateFormat : String;
begin
  LoadDLLFunctions ;

  OldAtivo := Ativo ;
  OldShortDateFormat := ShortDateFormat ;
  OldDateSeparator   := DateSeparator;

  try
    Ativo := False ;

    DateSeparator      :='/';
    ShortDateFormat    := 'dd/mm/yy' ;

    AbrePortaSerialDLL ;

    PathBin := ExtractFilePath(NomeArquivo);
    PathBin:= PathBin + 'MF.BIN';
    SysUtils.DeleteFile( PathBin );

    Resp := xECF_DownloadMF( pathBin );
    if Resp <> 1 then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar xECFDownloadMF'+sLineBreak+
                                       'Cod.: '+IntToStr(Resp) ));

    DiaIni := FormatDateTime('DD/MM/YY',DataInicial) ;
    DiaFim := FormatDateTime('DD/MM/YY',DataFinal) ;

    Resp := xECF_ReproduzirMemoriaFiscalMFD('2', DiaIni, DiaFim, NomeArquivo, pathBin);

    if (Resp <> 1) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar ECF_DownloadMFD.'+sLineBreak+
                                       'Cod.: '+IntToStr(Resp) ))
  finally
    xECF_FechaPortaSerial ;
    DateSeparator   := OldDateSeparator;
    ShortDateFormat := OldShortDateFormat;
    Ativo := OldAtivo ;
  end ;

  if not FileExists( NomeArquivo ) then
     raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de ECF_DownloadMFD.'+sLineBreak+
                            'Arquivo: "'+NomeArquivo+'" não gerado' ))
end;

procedure TACBrECFSweda.ArquivoMFD_DLL(ContInicial, ContFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD;
  TipoContador: TACBrECFTipoContador);
Var
  Resp : Integer ;
  CooIni, CooFim : AnsiString ;
  OldAtivo : Boolean ;
  PathBin:AnsiString;
begin
  LoadDLLFunctions ;

  OldAtivo := Ativo ;
  try
    Ativo := False ;

    AbrePortaSerialDLL ;
    if TipoContador = tpcCRZ then
    begin
       {Por CRZ}
       CooIni := IntToStrZero( ContInicial, 4 ) ;
       CooFim := IntToStrZero( ContFinal, 4 ) ;
    end
    else
    if TipoContador = tpcCOO then
    begin
       {POr COO}
       CooIni := IntToStrZero( ContInicial, 7);
       CooFim := IntToStrZero( ContFinal, 7 ) ;
    end
    else
      raise EACBrECFERRO.Create('Tipo de contador desconhecido, tipos válidos: CRZ, COO');

    PathBin := ExtractFilePath(NomeArquivo);
    PathBin:= PathBin + 'MF.BIN';
    SysUtils.DeleteFile( PathBin );

    Resp := xECF_DownloadMF( pathBin );
    if Resp <> 1 then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar xECFDownloadMF'+sLineBreak+
                                       'Cod.: '+IntToStr(Resp) ));

    Resp := xECF_ReproduzirMemoriaFiscalMFD('2', CooIni, CooFim, NomeArquivo, PathBin);

    if (Resp <> 1) then
      raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar xECF_ReproduzirMemoriaFiscalMFD.'+sLineBreak+
                                       'Cod.: '+IntToStr(Resp) ))
  finally
    xECF_FechaPortaSerial ;
    Ativo := OldAtivo ;
  end ;

  if not FileExists( NomeArquivo ) then
     raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de ECF_DownloadMFD.'+sLineBreak+
                            'Arquivo: "'+NomeArquivo + '" não gerado' ))
end;

procedure TACBrECFSweda.ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString);
var
  Resp: Integer;
  FileMF : AnsiString;
  OldAtivo: Boolean;
begin
  LoadDLLFunctions;

  OldAtivo := Ativo ;
  try
    AbrePortaSerialDLL;

    // fazer o download da MF
    GravaLog( '   xECF_DownloadMF' );
    FileMF := ExtractFileName( NomeArquivo );
    Resp := xECF_DownloadMF( FileMF );
    if (Resp <> 1) then
       raise EACBrECFERRO.Create( ACBrStr( 'Erro ao executar ECF_DownloadMF.'+sLineBreak+
                                  'Cod.: '+IntToStr(Resp) ));
  finally
     xECF_FechaPortaSerial ;
     try
        Ativo := OldAtivo ;
     except
     end;
  end;
  if not FileExists( NomeArquivo ) then
     raise EACBrECFERRO.Create( ACBrStr( 'Erro na execução de ECF_DownloadMF.'+sLineBreak+
                            'Arquivo: "'+NomeArquivo+'" não gerado' ))
end;

function TACBrECFSweda.GetNumReducoesZRestantes: String;
var
  NumRedAtual: Integer;
const
  // o numero 3693 e número de reduções possiveis para as impressoras sweda
  // conforme informado pelo atendimento sweda
  NumMaximoReducoes = 3693;
begin
  NumRedAtual := StrToIntDef(Self.NumCRZ, 0);
  Result := Format('%4.4d', [NumMaximoReducoes - NumRedAtual]);
end;

end.

