{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados(c)2007 André Bohn (AMCOM Sistemas de Informação)}
{                                                                              }
{ Colaboradores nesse arquivo: Daniel Simoes de Almeida                        }
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
|* 04/12/2007: André Bohn (AMCOM Sistemas de Informação)
|* - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrECFNCR ;

interface
uses ACBrECFClass, ACBrDevice, ACBrUtil, Synautil, ACBrConsts,
     Classes ;

const  SOH = #01 ;
       ENQ = #05 ;
       ACK = 06 ;
       WAK = 17 ;
       NAK = 21 ;
       SYN = 22 ;
       CAN = #24 ;

type

TACBrECFNCRComando = class
  private
    fsComando : AnsiString ;
    fsParams  : TStringList ;
    fsSeq     : Byte ;
    fsTimeOut : Integer;

    function GetPacoteEnvio: AnsiString;
    procedure SetComando(const Value: AnsiString);
 public
    constructor create ;
    destructor Destroy ; override ;

    property Comando     : AnsiString  write SetComando  ;
    property TimeOut     : Integer     read fsTimeOut write fsTimeOut ;
    property PacoteEnvio : AnsiString  read GetPacoteEnvio ;
    property Params      : TStringList read fsParams ;
    property Seq         : Byte read fsSeq  write fsSeq;

    Procedure AddParam(const AString : AnsiString) ;
 end ;

TACBrPosParam = Array of Byte;
 
TACBrECFNCRResposta = class
  private
    fsResposta     : AnsiString ;

    fsSeq          : Byte ;
    fsCmd          : Byte ;
    fsRetorno      : AnsiString ;
    fsParams       : TStringList ;
    fsChkSum       : AnsiString ;
    fsPosParam     : TACBrPosParam;

    procedure SetResposta(const Value: AnsiString) ;
    function  GetDescRetorno: AnsiString ;
    procedure CarregaPosParam ;
 public
    constructor create ;
    destructor Destroy ; override ;

    property Resposta     : AnsiString    read fsResposta write SetResposta ;
    property Seq          : Byte          read fsSeq;
    property Retorno      : AnsiString    read fsRetorno ;
    property DescRetorno  : AnsiString    read GetDescRetorno ;
    property Params       : TStringList   read fsParams ;
    property ChkSum       : AnsiString    read fsChkSum ;
 end ;

{ Classe filha de TACBrECFClass com implementaçao para NCR }

{ TACBrECFNCR }

TACBrECFNCR = class( TACBrECFClass )
 private
    fsNumLoja       : String ;
    fsCNPJ          : String ;
    fsIE            : String ;
    fsHorarioVerao  : String ;
    fsNCRComando    : TACBrECFNCRComando ;
    fsNCRResposta   : TACBrECFNCRResposta ;
    fsImprimeCheque : Boolean ;
    fsLeituraCMC7   : Boolean ;

    procedure Sincroniza ;
    function  BuscaSequenciaVinculado( const CodFormaPagto : String = '' ) : String ;
 protected
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumCCF: String; override ;
    function GetNumECF: String; override ;
    function GetNumLoja: String; override ;
    function GetNumSerie: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;

    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetGrandeTotal: Double; override ;
    function GetNumCRO: String; override ;
    function GetNumCRZ: String; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;
    function GetNumCOOInicial: String; override ;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;

    function GetChequePronto: Boolean; override ;
 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    property NCRComando : TACBrECFNCRComando  read fsNCRComando ;
    property NCRResposta: TACBrECFNCRResposta read fsNCRResposta ;

    Function EnviaComando_ECF( cmd : AnsiString = '') : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '' ) ; override ;
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;

    Procedure LeituraX ; override ;
    Procedure ReducaoZ(DataHora : TDateTime) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;
    Procedure PulaLinhas( NumLinhas : Integer = 0 ) ; override ;

    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure CorrigeEstadoErro(Reducao: Boolean = True) ; override ;


    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False ); override ;
    Procedure AbreGaveta ; override ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '' ) ; override ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ; override ;
    Procedure EfetuaPagamentoNaoFiscal( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false) ; override ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaNaoFiscal ; override ;
    procedure NaoFiscalCompleto(CodCNF: String; Valor: Double;
      CodFormaPagto: String; Obs: AnsiString; IndiceBMP : Integer = 0); override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota ; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;
    procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ; override ;
    Function LeituraCMC7 : AnsiString ; override ;
 end ;

function NCRCheckSum(const Dados: AnsiString): Char;

implementation
Uses ACBrECF,
 {$IFDEF COMPILER6_UP}
   DateUtils, StrUtils, Variants
 {$ELSE}
   ACBrD5, Windows
 {$ENDIF},
     SysUtils,  Math ;

function NCRCheckSum(const Dados: AnsiString): Char;
var
 i : Integer;
begin
  i := SomaAscII(Dados) ;
  Result :=  chr( i ) ;
end;

{ -------------------------  TACBrECFNCRComando -------------------------- }
constructor TACBrECFNCRComando.create;
begin
  inherited create ;

  fsParams := TStringList.create ;
  fsSeq    := 1 ;
end;

destructor TACBrECFNCRComando.destroy;
begin
  fsParams.Free ;

  inherited destroy ;
end;

procedure TACBrECFNCRComando.SetComando(const Value: AnsiString);
begin
  if fsSeq >= 255 then
     fsSeq := 1
  else
     Inc( fsSeq ) ;

  { Zerando instrucoes adicionais do comando }
  fsParams.Clear ;
  fsTimeOut := 0 ;

  fsComando := Value
end;

procedure TACBrECFNCRComando.AddParam(const AString: AnsiString);
begin
  fsParams.Add(  AString  ) ;
end;

function TACBrECFNCRComando.GetPacoteEnvio: AnsiString;
 Var I : Integer ;
     ParamsStr : AnsiString ;
     TBC : Byte;
begin
  { Montando pacote com Parametros }
  ParamsStr := '' ;
  For I := 0 to fsParams.Count-1 do
    ParamsStr := ParamsStr +  fsParams[I] + '\' ;

  TBC := Length( ParamsStr ) ;

  { Montando Pacote de Envio }
  Result := AnsiChar( chr( fsSeq ) ) + AnsiChar( Chr( StrToInt(fsComando) ) ) +
            AnsiChar( Chr( TBC ) ) + ParamsStr ;

  { Calculando o Checksum }
  Result := SOH + Result + NCRCheckSum( Result ) ;
end;


{ ------------------------- TACBrECFNCRResposta -------------------------- }

constructor TACBrECFNCRResposta.create;
begin
  inherited create ;

  fsParams        := TStringList.create ;
  fsSeq           := 0 ;
  fsRetorno       := '' ;
  fsChkSum        := '' ;
  fsResposta      := '' ;
end;

destructor TACBrECFNCRResposta.destroy;
begin
  fsParams.Free ;

  inherited destroy ;
end;

function CharToBin(const sChar:string):string;
var iBit, iChr, iPos : integer;
    sSts : AnsiString;

begin
   iChr := Ord(sChar[1]);
   iBit := 128;
   iPos := 1;
   sSts := '00000000';

   while iBit > 0 do
   begin
      if iChr >= iBit then
      begin
         iChr := iChr - iBit;
         sSts[iPos] := '1';
      end;
      iBit := trunc(iBit / 2);
      inc(iPos);
   end;
   Result := sSts;
end;

{ CarregaPosParam - Criado para pegar o tamanho dos campos de retorno,
  para que ao ler a resposta ja carrega os campos de retorno do comando no array Params,
  pois a NCR não possui separador de campos no retorno, e manter o objeto comando
  e resposta como foi feito com a ECF Epson. Isso facilitao tratamento de com
  o campos de retorno da ECF

Exemplo: Comando 1 - Leitura do Numero de Série

Descrição                               Formato    Min    Máx   Conteúdo   Default
----------------------------------------------------------------------------------
Número de série do ECF                     Q         20    20
Identificação da Memória Fiscal            Q          1     1
Fabricante                                 Q         20    20
Tipo do equipamento                        Q          7     7
Modelo do ECF                              Q         20    20
Número de série da MFD                     Q         19    19
Data de gravação                           D          6     6
Horário de gravação                        H          6     6
Indicador do horário de verão da gravação  N          1     1

  O Array neste caso será todos os valores da coluna "Máx".

   fsPosParam := VarArrayOf([20, 1, 20, 7, 20, 19, 6, 6, 1]);             }
procedure TACBrECFNCRResposta.CarregaPosParam ;
begin
  case fsCmd of
    1 : fsPosParam := VarArrayOf([20, 1, 20, 7, 20, 19, 6, 6, 1]); { Leitura do número de série }
    3 : fsPosParam := VarArrayOf([3, 20, 20, 20, 18, 6, 6, 1]);    { Leitura e Usuarios } 
    7 : fsPosParam := VarArrayOf([3]);                             { Leitura de número de seqüência do ECF no estabelecimento }
   12 : fsPosParam := VarArrayOf([6, 6, 1]);                       { Leitura do relógio interno do ECF }
   15 : fsPosParam := VarArrayOf([4]);                             { Leitura do horário mínimo para liberação da Redução Z }
   60 : fsPosParam := VarArrayOf([1, 6, 6, 6, 1, 4, 6, 3, 18, 6]); { Leitura de dados da reducao Z }
   63 : fsPosParam := VarArrayOf([1]);                             { Leitura do modo de operação }
   64 : fsPosParam := VarArrayOf([2, 2, 4, 1, 1]);                 { Leitura do contexto de operação }
   65 : fsPosParam := VarArrayOf([1, 6]);                          { Leitura do status da Redução Z }
   66 : fsPosParam := VarArrayOf([1, 1, 1]);                       { Leitura do status do mecanismo impressor }
   67 : fsPosParam := VarArrayOf([1]);                             { Leitura do status da gaveta }
   68 : fsPosParam := VarArrayOf([1]);                             { Leitura do status do cancelamento }
   69 : fsPosParam := VarArrayOf([18]);                            { Leitura de valor dos acumuladores - Valor atual }
   70 : fsPosParam := VarArrayOf([18]);                            { Leitura de valor dos acumuladores - Documento corrente }
   71 : fsPosParam := VarArrayOf([18]);                            { Leitura de valor dos acumuladores - Valor no início do dia }
   72 : fsPosParam := VarArrayOf([18, 18]);                        { Leitura de valor dos registradores parciais tributados - Valor atual }
   74 : fsPosParam := VarArrayOf([18, 4, 4, 4, 4]);                { Leitura de valor dos registradores de meios de pagamento - Valor atual }
   76 : fsPosParam := VarArrayOf([6, 18]);                         { Leitura de valor dos registradores parciais não fiscais - Valor atual }
  160 : fsPosParam := VarArrayOf([2]);                             { Leitura da quantidade de registradores fiscais parciais programados }
  161 : fsPosParam := VarArrayOf([4, 6, 6, 1]);                    { Leitura de dados de registradores fiscais parciais programados }
  162 : fsPosParam := VarArrayOf([15, 6, 6, 1]);                   { Leitura de dados de registradores não fiscais parciais programados }
  163 : fsPosParam := VarArrayOf([2]);                             { Leitura da quantidade de registradores não fiscais parciais programados }
  165 : fsPosParam := VarArrayOf([2]);                             { Leitura do próximo registrador não fiscal parcial programado }
  166 : fsPosParam := VarArrayOf([1, 15, 6, 6, 1]);                { Leitura de dados de registradores de meio de pagamento programados }
  167 : fsPosParam := VarArrayOf([2]);                             { Leitura da quantidade de registradores de meios de pagamento programados }
  169 : fsPosParam := VarArrayOf([2]);                             { Leitura do próximo registrador de meio de pagamento programado }
  187 : fsPosParam := VarArrayOf([3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 2, 4]); { Leitura de características do ECF }
  189 : fsPosParam := VarArrayOf([2, 2, 2, 6, 6, 1]);              { Leitura de atualizações do firmware }
  220 : fsPosParam := VarArrayOf([6]);                             { Leitura dos contadores - Valor atual }
  221 : fsPosParam := VarArrayOf([6]);                             { Leitura dos contadores - Valor no início do dia }
  224 : fsPosParam := VarArrayOf([4, 4, 4]);                       { Leitura do status dos Comprovantes de Crédito ou Débito – Último documento }
  225 : fsPosParam := VarArrayOf([2, 2, 1, 2, 18]);                { Leitura dos dados dos Comprovantes de Crédito ou Débito – Último documento }
  226 : fsPosParam := VarArrayOf([1, 1, 1, 1, 1, 1, 1, 1, 1 , 1]); { Leitura do estado das definições do ECF }
  else
    SetLength( fsPosParam, 0) ;
  end;
end;

procedure TACBrECFNCRResposta.SetResposta(const Value: AnsiString);
Var Buf : AnsiString ;
    P   : Integer ;
    TamRetorno : Integer;
begin
  fsParams.Clear ;
  fsSeq           := 0 ;
  fsRetorno       := '' ;
  fsChkSum        := '' ;
  fsResposta      := '' ;

  if Value = '' then exit ;

  fsResposta := Value ;

  if ( LeftStr(fsResposta,1) <> SOH ) and ( LeftStr(fsResposta,1) <> CAN ) then
     raise EACBrECFERRO.Create(ACBrStr('Resposta inválida. Não inicia com SOH (01) ou CAN (24) ')) ;

  if ( LeftStr(fsResposta,1) = CAN ) then
  begin
    fsRetorno := copy( fsResposta, 4, 2 ) ;
  end
  else if ( LeftStr(fsResposta,1) = SOH ) then
  begin
    fsChkSum := RightStr(fsResposta,1) ;
    if NCRCheckSum( copy(fsResposta,2,Length(fsResposta)-2) ) <> fsChkSum then
       raise EACBrECFERRO.create(copy(fsResposta,2,Length(fsResposta)-2) + ' | ' + fsResposta+ACBrStr('Resposta inválida. CheckSum da Resposta não está correto.')) ;

    try
       fsSeq := ord(fsResposta[2]) ;
    except
       raise EACBrECFERRO.Create(ACBrStr('Resposta inválida. Num.Sequencia inválido')) ;
    end ;

    fsCmd := ord(fsResposta[3]) ;
    TamRetorno := Synautil.BinToInt( CharToBin( fsResposta[5] ) + CharToBin( fsResposta[4] ) );
    { Pega apenas o BRS }
    Buf := copy(fsResposta, 6, TamRetorno) ;  //  Remove SOH, SEQ, CMD, TBR e CHKSUM

    CarregaPosParam ;
    { Quebrando Parametros com os tamanhos carregados em fsPosParam }
    for P := low( fsPosParam ) to high( fsPosParam ) do
    begin
      fsParams.Add( LeftStr(Buf, fsPosParam[P])  ) ;
      delete(buf, 1, fsPosParam[P]) ;
    end;
  end;
end;


function TACBrECFNCRResposta.GetDescRetorno: AnsiString;
 Var  sValorSaida : String;
      sRetorno    : AnsiString ;
begin
  sValorSaida := '';
  sRetorno    := fsRetorno ;

  Case sRetorno[1] of
    #1 : sValorSaida := 'Tamanho do parâmetro menor que o mínimo permitido.' ;
    #2 : sValorSaida := 'Tamanho do parâmetro maior que o máximo permitido.' ;
    #3 : sValorSaida := 'Conteúdo do parâmetro inválido.' ;
    #4 : sValorSaida := 'Falta parâmetro.' ;
    #5 : sValorSaida := 'Excesso de parâmetros.' ;
    #6 : sValorSaida := 'Caractere inválido no parâmetro.' ;
    #7 : case sRetorno[2] of
           #1 : sValorSaida := 'Ocorreu erro no envio do pacote de comando.' ;
           #2 : sValorSaida := 'Ocorreu timeout na recepção da resposta do protocolo confirmando a recepção do pacote de comando.' ;
           #3 : sValorSaida := 'O pacote de comando recebido pelo ECF contém erro de protocolo ou checksum.' ;
           #4 : sValorSaida := 'Foi recebido um caractere de controle diferente de ACK, WAK e NAK como resposta a um pacote de comando enviado.' ;
           #5 : sValorSaida := 'Ocorreu erro no envio de uma solicitação de status.' ;
           #6 : sValorSaida := 'Ocorreu timeout na recepção da resposta de uma solicitação de status.' ;
           #7 : sValorSaida := 'Foi recebido um caractere de controle diferente de CAN, SOH e WAK como resposta a uma solicitação de status.' ;
           #9 : sValorSaida := 'Ocorreu erro no envio do pacote de sincronismo.' ;
          #10 : sValorSaida := 'Ocorreu timeout na recepção da resposta de um pedido de sincronismo.' ;
          #11 : sValorSaida := 'Foi recebido um caractere de controle diferente de SYN e WAK como resposta a um pedido de sincronismo.' ;
          #12 : sValorSaida := 'A porta serial não foi iniciada.' ;
         end;
    #8 : case sRetorno[2] of
           #1 : sValorSaida := 'A área destinada a armazenar as linhas para impressão da 2a via do Comprovante de Crédito ou Débito esgotou-se. O Comprovante de Crédito ou Débito deverá ser encerrado.' ;
         end;
    #9 : case sRetorno[2] of
           #0 : sValorSaida := 'O comando de Início do Dia não pode ser executado sem que o último dia de movimento seja reduzido.' ;
           #1 : sValorSaida := 'Sem a realização do comando Início do Dia, é permitida apenas a emissão de Leitura X e Leitura da Memória Fiscal.' ;
           #2 : sValorSaida := 'Esgotada a quantidade máxima de Reduções Z suportada pelo ECF.' ;
           #3 : sValorSaida := 'Esgotada a área da Memória Fiscal destinada a gravação de Reduções Z.' ;
         #253 : sValorSaida := 'Já passam de 2:00 hs da manhã do dia seguinte ao dia do movimento corrente. O início de um novo Cupom Fiscal, Comprovante Não Fiscal e Relatório Gerencial somente será permitido após a abertura de uma nova data de movimento.' ;
         #254 : sValorSaida := 'O horário do ECF é anterior ao horário mínimo programado para a emissão de Reduções Z.' ;
         #255 : sValorSaida := 'Já foi realizada uma Redução Z na data corrente.' ;
         end;
   #10 : case sRetorno[2] of
           #0 : sValorSaida := 'Comando inválido recebido pelo firmware.' ;
           #1 : sValorSaida := 'Não é possível a impressão de texto genérico na seção corrente do documento.' ;
           #3 : sValorSaida := 'Excedida a quantidade de avanços de linha na seção corrente do documento.' ;
         end;
   #11 : case sRetorno[2] of
           #0 : sValorSaida := 'É mandatório realizar o acerto do relógio interno do ECF.' ;
           #1 : sValorSaida := 'É mandatória a definição do número de série do ECF.' ;
           #2 : sValorSaida := 'É mandatória a programação de um usuário.' ;
           #3 : sValorSaida := 'É mandatória a programação de pelo menos uma linha do tipo Nome no clichê.' ;
           #4 : sValorSaida := 'É mandatória a programação de pelo menos uma linha do tipo Endereço no clichê.' ;
           #5 : sValorSaida := 'É mandatória a definição do Número de Seqüência no Estabelecimento.' ;
           #6 : sValorSaida := 'É mandatória a definição dos Parâmetros de Configuração do ECF.' ;
           #7 : sValorSaida := 'É mandatória a definição do Símbolo da Moeda.' ;
           #8 : sValorSaida := 'É mandatória a definição da Criptografia do GT.' ;
          #16 : sValorSaida := 'Excedida a quantidade de autenticações permitida na seção corrente do documento.' ;
          #32 : sValorSaida := 'Comando somente poderá ser enviado com a utilização do cabo do fisco pela porta serial do fisco.' ;
          #48 : sValorSaida := 'Mecanismo incompatível com o modelo do número de série.' ;
          #49 : sValorSaida := 'Marca incompatível com o número de série.' ;
          #50 : sValorSaida := 'Sigla da marca do número de série inválida.' ;
          #51 : sValorSaida := 'Tipo do ECF inválido.' ;
          #53 : sValorSaida := 'Mecanismo inválido.' ;
          #64 : sValorSaida := 'O símbolo da moeda já se encontra programado com o conteúdo passado como parâmetro.' ;
          #65 : sValorSaida := 'Ainda não foi realizada nenhuma programação de símbolo de moeda no EC.' ;
          #66 : sValorSaida := 'O seqüencial da programação de símbolo de moeda ainda não foi utilizado.' ;
          #67 : sValorSaida := 'Ainda não foi realizada nenhuma programação de criptografia do GT no ECF.' ;
          #68 : sValorSaida := 'O seqüencial da programação de criptografia do GT ainda não foi utilizado.' ;
          #69 : sValorSaida := 'A criptografia do GT já se encontra programada com o conteúdo passado como parâmetro.' ;
          #80 : sValorSaida := 'O primeiro boot do ECF somente pode ser realizado em Intervenção.' ;
         #157 : sValorSaida := 'O ECF detectou uma Memória de Fita Detalhe gravada por outro ECF. Apenas os comandos de leitura estão habilitados.' ;
         #158 : sValorSaida := 'O valor do CFD solicitado ainda não foi utilizado.' ;
         #159 : sValorSaida := 'Ainda não foi realizada nenhuma reimpressão de Memória de Fita Detalhe.' ;
         #179 : sValorSaida := 'Inscrição Estadual diferente da Inscrição Estadual cadastrada no usuário.' ;
         #180 : sValorSaida := 'Inscrição Municipal diferente da Inscrição Municipal cadastrada no usuário.' ;
         #181 : sValorSaida := 'CNPJ diferente do CNPJ cadastrado no usuário.' ;
         #182 : sValorSaida := 'A programação de um novo usuário no ECF somente poderá ocorrer se não houver nenhum documento emitido na data da programação.' ;
         #183 : sValorSaida := 'A senha informada para a programação do usuário não confere.' ;
         #184 : sValorSaida := 'A senha deve, obrigatoriamente, ser informada na programação do usuário.' ;
         #186 : sValorSaida := 'Existem caracteres repetidos na string de criptografia do GT.' ;
         #187 : sValorSaida := 'Já existe um usuário definido no ECF.' ;
         #188 : sValorSaida := 'É obrigatória a informação de pelo menos uma inscrição (Estadual ou Municipal) para o usuário.' ;
         #189 : sValorSaida := 'A quantidade máxima de usuários permitida no ECF foi atingida.' ;
         #190 : sValorSaida := 'O seqüencial de usuário solicitado ainda não foi utilizado.' ;
         #191 : sValorSaida := 'Os dados informados na programação do usuário coincidem com o usuário corrente do ECF.' ;
         #204 : sValorSaida := 'É mandatório que a NVRAM seja zerada para efetivar a troca da versão.' ;
         #205 : sValorSaida := 'É mandatório que o ECF esteja em Modo de Intervenção Técnica para efetivar a troca da versão.' ;
         #206 : sValorSaida := 'O seqüencial de troca de versão do firmware solicitado ainda não foi utilizado.' ;
         #207 : sValorSaida := 'Ainda não foi realizada nenhuma troca de versão do firmware no ECF.' ;
         #222 : sValorSaida := 'O valor do CRO solicitado ainda não foi utilizado.' ;
         #238 : sValorSaida := 'O valor do CRZ solicitado ainda não foi utilizado.' ;
         #246 : sValorSaida := 'Excedido o limite de Intervenções Técnicas  .' ;
         #247 : sValorSaida := 'Limite de reimpressões de MFD esgotado.' ;
         #248 : sValorSaida := 'Período não encontrado.' ;
         #249 : sValorSaida := 'O COO inicial é maior do que o COO final.' ;
         #251 : sValorSaida := 'O CRZ inicial é maior do que o CRZ final.' ;
         #252 : sValorSaida := 'A data inicial do intervalo é maior do que data final.' ;
         #253 : sValorSaida := 'O comando enviado somente pode ser submetido ao ECF em Modo de Operação Fiscal.' ;
         #254 : sValorSaida := 'O comando enviado somente pode ser submetido ao ECF em Modo de Intervenção Técnica.' ;
         #255 : sValorSaida := 'O número de série do ECF já foi definido.' ;
         end;
   #12 : case sRetorno[2] of
           #4 : sValorSaida := 'O comando enviado não pode ser executado pois existe um Cupom Fiscal aberto.' ;
           #7 : sValorSaida := 'O comando enviado não pode ser executado pois existe um Comprovante Não Fiscal aberto.' ;
           #9 : sValorSaida := 'O comando enviado não pode ser executado pois existe um Comprovante de Crédito ou Débito aberto.' ;
          #13 : sValorSaida := 'O comando enviado não pode ser executado pois existe um Estorno de Comprovante de Crédito ou Débito aberto.' ;
          #14 : sValorSaida := 'O comando enviado não pode ser executado pois existe um Relatório Gerencial aberto.' ;
          #15 : sValorSaida := 'O comando enviado não pode ser executado pois o ECF está em repouso.' ;
         #227 : sValorSaida := 'A quantidade máxima de itens em um Cupom Fiscal ou registros em um Comprovante Não Fiscal foi excedida.' ;
         #228 : sValorSaida := 'O tipo de leitura solicitada não é compatível com o documento em emissão.' ;
         #229 : sValorSaida := 'A segunda via do CCD já foi emitida.' ;
         #230 : sValorSaida := 'O tipo de documento solicitado é diferente de Cupom Fiscal e Comprovante Não Fiscal.' ;
         #231 : sValorSaida := 'A emissão de segunda via do CCD não é possível pois o conteúdo do comprovante não foi salvo.' ;
         #233 : sValorSaida := 'O CCD especificado já foi impresso.' ;
         #234 : sValorSaida := 'O CCD especificado já foi reimpresso.' ;
         #235 : sValorSaida := 'O CCD especificado não foi impresso.' ;
         #238 : sValorSaida := 'O cancelamento de um Cupom Fiscal ou Comprovante Não Fiscal somente será permitido após o estorno de todos os CCDs emitidos.' ;
         #239 : sValorSaida := 'O limite máximo de CCDs por cupom foi excedido.' ;
         #240 : sValorSaida := 'A quantidade de parcelas somente pode ser especificada para os pagamentos que envolvam meios que aceitem a emissão de CCD.' ;
         #243 : sValorSaida := 'A leitura de dados dos meios de pagamento não foi possível pois o documento corrente ou anterior não permitem meios de pagamento.' ;
         #245 : sValorSaida := 'O Cupom Adicional somente pode ser emitido uma única vez.' ;
         #246 : sValorSaida := 'O documento anterior não permite estorno de meios de pagamento.' ;
         #247 : sValorSaida := 'O Comprovante de Crédito ou Débito solicitado já foi emitido.' ;
         #248 : sValorSaida := 'O documento anterior não permite a emissão de Cupom Adicional.' ;
         #249 : sValorSaida := 'O comando não pode ser aceito pois o documento anterior está cancelado.' ;
         #250 : sValorSaida := 'O limite máximo de 30 pagamentos por documento já foi atingido.' ;
         #251 : sValorSaida := 'O Comprovante de Crédito ou Débito informado nos parâmetros não existe.' ;
         #252 : sValorSaida := 'O documento anterior não permite a utilização de Comprovante de Crédito ou Débito.' ;
         #253 : sValorSaida := 'O documento anterior não permite cancelamento.' ;
         #254 : sValorSaida := 'O documento anterior já foi cancelado.' ;
         #255 : sValorSaida := 'O documento atual não permite cancelamento.' ;
         end;  
   #13 : case sRetorno[2] of
           #0 : sValorSaida := 'Comando não pode ser executado antes da impressão do clichê.' ;
           #1 : sValorSaida := 'Comando não pode ser executado após a impressão do clichê.' ;
           #2 : sValorSaida := 'Comando não pode ser executado em documento com registros efetuados.' ;
           #3 : sValorSaida := 'Comando não pode ser executado após desconto ou acréscimo em subtotal.' ;
           #4 : sValorSaida := 'Comando não pode ser executado em documento sem pagamentos.' ;
           #5 : sValorSaida := 'Comando não pode ser executado em documentos ainda não totalmente pagos.' ;
           #6 : sValorSaida := 'Comando não pode ser executado em documentos totalmente pagos.' ;
           #7 : sValorSaida := 'Comando não pode ser executado após a impressão do consumidor.' ;
           #8 : sValorSaida := 'Comando não pode ser executado dentro de Relatórios Gerenciais.' ;
           #9 : sValorSaida := 'Comando não pode ser executado dentro de Comprovantes de Crédito ou Débito.' ;
          #10 : sValorSaida := 'Comando não pode ser executado antes do término do documento.' ;
          #11 : sValorSaida := 'Comando não pode ser executado durante a emissão do clichê.' ;
          end;
   #16 : case sRetorno[2] of
           #0 : sValorSaida := 'O avanço do relógio interno do ECF não pode ser superior a 31 dias.' ;
           #1 : sValorSaida := 'O relógio interno do ECF não pode ser atrasado para um horário anterior a última gravação realizada na Memória Fiscal.' ;
           #2 : sValorSaida := 'O relógio interno do ECF não encontra-se no horário de verão.' ;
           #3 : sValorSaida := 'O relógio interno do ECF encontra-se no horário de verão.' ;
           #4 : sValorSaida := 'A saída do horário de verão somente pode ocorrer entre 01:00hs e 23:59hs.' ;
           #5 : sValorSaida := 'A entrada no horário de verão somente pode ocorrer entre 00:00hs e 22:59hs.' ;
           #8 : sValorSaida := 'O relógio interno do ECF não pode ser atrasado para um horário anterior a última gravação realizada na Memória de Fita Detalhe.' ;
           #9 : sValorSaida := 'A alteração do horário de verão somente é permitida imediatamente após a Redução Z.' ;
          #10 : sValorSaida := 'O relógio interno do ECF não pode ser atrasado para um horário anterior ao impresso no rodapé do último documento emitido.' ;
         end;  
   #17 : case sRetorno[2] of
           #0 : sValorSaida := 'Registrador parcial já existente.' ;
           #1 : sValorSaida := 'A quantidade máxima de parciais foi excedida.' ;
           #2 : sValorSaida := 'Registrador parcial não existente.' ;
           #3 : sValorSaida := 'Final da lista de registradores parciais.' ;
           #4 : sValorSaida := 'Registrador parcial não fiscal já existente.' ;
           #6 : sValorSaida := 'Registrador parcial não fiscal não existente.' ;
           #7 : sValorSaida := 'Final da lista de registradores parciais não fiscais.' ;
           #8 : sValorSaida := 'Registrador de meio de pagamento já existente.' ;
          #10 : sValorSaida := 'Registrador de meio de pagamento não existente.' ;
          #11 : sValorSaida := 'Final da lista de registradores de meio de pagamento.' ;
          #13 : sValorSaida := 'Registrador de unidade de medida não existente.' ;
          #14 : sValorSaida := 'Registrador de unidade de medida já existente.' ;
          #15 : sValorSaida := 'Descrição de unidade de medida já existente.' ;
          #16 : sValorSaida := 'Não é possível criar registradores parciais sujeitos a tributação do ISSQN para usuários sem Inscrição Municipal.' ;
          #17 : sValorSaida := 'Não é possível criar registradores parciais sujeitos a tributação do ICMS para usuários sem Inscrição Estadual.' ;
          #18 : sValorSaida := 'Registrador de Relatório Gerencial não existente.' ;
          #19 : sValorSaida := 'Registrador de Relatório Gerencial já existente.' ;
          #20 : sValorSaida := 'Final da lista de registradores de Relatório Gerencial.' ;
          #21 : sValorSaida := 'Final da lista de registradores de unidade de medida.' ;
          end;
   #18 : case sRetorno[2] of
           #0 : sValorSaida := 'Registrador fiscal parcial não programado.' ;
           #1 : sValorSaida := 'Seqüencial do item de Cupom Fiscal ou registro de Comprovante Não Fiscal inexistente.' ;
           #2 : sValorSaida := 'Desconto maior ou igual a venda líquida.' ;
           #3 : sValorSaida := 'Desconto igual a zeros.' ;
           #5 : sValorSaida := 'Documento com total igual a zeros.' ;
           #6 : sValorSaida := 'O seqüencial do registro informado no parâmetro já foi excluído do buffer e portanto não pode mais ser referenciado.' ;
           #7 : sValorSaida := 'O registro referente ao seqüencial informado já foi cancelado.' ;
           #8 : sValorSaida := 'O valor do desconto no subtotal deve ser menor do que o total.' ;
           #9 : sValorSaida := 'Não foi concedido desconto no subtota.' ;
          #10 : sValorSaida := 'O seqüencial do pagamento informado não existe.' ;
          #11 : sValorSaida := 'O código do item é obrigatório para produtos sujeitos ao ICMS.' ;
          #12 : sValorSaida := 'O registrador parcial não fiscal não foi programado.' ;
          #13 : sValorSaida := 'O registrador do meio de pagamento que está substituindo o meio de pagamento estornado, não foi programado.' ;
          #17 : sValorSaida := 'O desconto em registros sujeitos a tributação do ISS não está habilitado.' ;
          #18 : sValorSaida := 'O valor do desconto no subtotal é maior que a soma dos valores líquidos dos registros sujeitos ao ICMS e o desconto em registros sujeitos a tributação do ISS não está habilitado.' ;
          #19 : sValorSaida := 'Acréscimo maior ou igual a venda líquida.' ;
          #20 : sValorSaida := 'Acréscimo igual a zeros.' ;
          #21 : sValorSaida := 'O valor do acréscimo no subtotal deve ser menor do que o total.' ;
          #22 : sValorSaida := 'Não foi concedido acréscimo no subtotal.' ;
          #23 : sValorSaida := 'Não é possível conceder outro desconto no item.' ;
          #24 : sValorSaida := 'Não é possível conceder outro acréscimo no item.' ;
          #25 : sValorSaida := 'Não é possível conceder outro desconto no subtotal.' ;
          #26 : sValorSaida := 'Não é possível conceder outro acréscimo no subtotal.' ;
          #27 : sValorSaida := 'O valor total do registro deve ser igual ao produto da quantidade pelo preço unitário.' ;
         end;
   #19 : case sRetorno[2] of
           #1 : sValorSaida := 'A NVRAM somente poderá ser zerada quando o ECF estiver em Modo de Intervenção Técnica.' ;
         end;  
   #20 : case sRetorno[2] of
           #4 : sValorSaida := 'As leituras manuais não podem ser executadas pois existe um Cupom Fiscal aberto.' ;
           #7 : sValorSaida := 'As leituras manuais não podem ser executadas pois existe um Comprovante Não Fiscal aberto.' ;
           #9 : sValorSaida := 'As leituras manuais não podem ser executadas pois existe um Comprovante de Crédito ou Débito aberto.' ;
          #13 : sValorSaida := 'As leituras manuais não podem ser executadas pois existe um Estorno de Comprovante de Crédito ou Débito aberto.' ;
          #14 : sValorSaida := 'As leituras manuais não podem ser executadas pois existe um Relatório Gerencial aberto.' ;
         #255 : sValorSaida := 'As leituras manuais não podem ser executadas com a iniciação do ECF incompleta.' ;
         end;
   #21 : case sRetorno[2] of
          #18 : sValorSaida := 'Excedido o limite superior da área de índices da Memória Fiscal em uma operação de SEEK.' ;
          #19 : sValorSaida := 'Excedido o limite inferior da área de índices da Memória Fiscal em uma operação de SEEK.' ;
          #20 : sValorSaida := 'Excedido o limite superior da área de dados da Memória Fiscal em uma operação de SEEK.' ;
          #21 : sValorSaida := 'Excedido o limite inferior da área de dados da Memória Fiscal em uma operação de SEEK.' ;
          #27 : sValorSaida := 'Erro na leitura de dados após a gravação na Memória Fiscal.' ;
          #28 : sValorSaida := 'Erro checando a gravação na Memória Fiscal.' ;
          #29 : sValorSaida := 'Erro de gravação na Memória Fisca.' ;
          #30 : sValorSaida := 'Término do espaço disponível para gravação de registros na área de índice da Memória Fiscal em função de erros ocorridos anteriormente.' ;
          #31 : sValorSaida := 'Tentativa de leitura além do limite superior da área de índice da Memória Fiscal.' ;
          #34 : sValorSaida := 'Tentativa de leitura além do limite superior da área de dados da Memória Fiscal.' ;
          #35 : sValorSaida := 'Término do espaço disponível para gravação de registros na área de dados da Memória Fiscal em função de erros ocorridos anteriormente.' ;
          #36 : sValorSaida := 'Erro de checksum em uma leitura de registro gravado na Memória Fiscal.' ;
          #37 : sValorSaida := 'Tentativa de endereçar além do limite superior da Memória Fiscal.' ;
          #39 : sValorSaida := 'Memória Fiscal não está limpa.' ;
          #40 : sValorSaida := 'Memória Fiscal foi desconectada. É necessário realizar intervenção técnica.' ;
          #41 : sValorSaida := 'Erro de leitura na Memória Fiscal.' ;
          #42 : sValorSaida := 'Memória fiscal inconsistente.' ;
          #43 : sValorSaida := 'Erro na verificação da data de registro na Memória Fiscal.' ;
          #44 : sValorSaida := 'Erro na verificação do buffer do registro na Memória Fiscal.' ;
          #47 : sValorSaida := 'Erro na verificação da quantidade de registros gravados na Memória Fiscal.' ;
          #64 : sValorSaida := 'Fim de bobina.' ;
          #65 : sValorSaida := 'Bobina acabando.' ;
          #80 : sValorSaida := 'Aguardando a inserção de cheque.' ;
          #81 : sValorSaida := 'Cheque presente.' ;
          #82 : sValorSaida := 'Cheque ausente.' ;
          #95 : sValorSaida := 'Timeout aguardando a inserção do cheque para impressão.' ;
          #96 : sValorSaida := 'Aguardando a inserção de documento para autenticação.' ;
          #97 : sValorSaida := 'Documento para autenticação presente.' ;
          #98 : sValorSaida := 'Documento para autenticação ausente.' ;
         #111 : sValorSaida := 'Timeout aguardando a inserção do documento para autenticação.' ;
         #131 : sValorSaida := 'Tampa aberta.' ;
         #132 : sValorSaida := 'Modelo de ECF não realiza autenticação de documentos.' ;
         #133 : sValorSaida := 'Modelo de ECF não realiza a impressão de cheques.' ;
         #136 : sValorSaida := 'Mecanismo impressor desconhecido.' ;
         #137 : sValorSaida := 'Erro de timeout no mecanismo impressor.' ;
         #138 : sValorSaida := 'Tecla de FEED do mecanismo impressor pressionada.' ;
         #144 : sValorSaida := 'Modelo de ECF não possui leitor de CMC-7.' ;
         #145 : sValorSaida := 'Erro na leitura do CMC-7.' ;
         #146 : sValorSaida := 'Erro de timeout aguardando a inserção de cheque para a leitura de CMC-7.' ;
         #161 : sValorSaida := 'Memória de fita detalhe não está limpa.' ;
         #162 : sValorSaida := 'MFD desconectada.' ;
         #163 : sValorSaida := 'A MFD foi desconectada em uma execução anterior.' ;
         #164 : sValorSaida := 'Erro de gravação na MFD.' ;
         end;
   #22 : case sRetorno[2] of
           #0 : sValorSaida := 'Overflow na parte inteira durante a conversão de um número representado por uma string para BCD.' ;
           #1 : sValorSaida := 'Overflow na parte decimal durante a conversão de um número representado por uma string para BCD.' ;
           #3 : sValorSaida := 'Overflow em registrador de situação tributária.' ;
           #4 : sValorSaida := 'Overflow em registrador parcial de acréscimos.' ;
           #5 : sValorSaida := 'Overflow em registrador parcial de descontos.' ;
           #6 : sValorSaida := 'Overflow em registrador não fiscal.' ;
           #7 : sValorSaida := 'Overflow em registrador de meio de pagamento.' ;
           #8 : sValorSaida := 'Overflow em registrador de troco.' ;
           #9 : sValorSaida := 'Overflow em registrador de cancelamento.' ;
         end;
   #23 : case sRetorno[2] of
           #0 : sValorSaida := 'Caractere de controle inválido recebido no ECF.' ;
           #1 : sValorSaida := 'Checksum inválido no pacote recebido no ECF.' ;
           #2 : sValorSaida := 'ECF encontra-se em erro fatal.' ;
          #17 : sValorSaida := 'Checksum da NVRAM inválido.' ;
          #33 : sValorSaida := 'Memória de Fita detalhe esgotada.' ;
         end;
   #24 : case sRetorno[2] of
           #1 : sValorSaida := 'A Memória Fiscal não está presente.' ;
           #3 : sValorSaida := 'Erro de gravação na Memória Fiscal.' ;
           #8 : sValorSaida := 'Tentativa de regravação na Memória Fiscal.' ;
           #9 : sValorSaida := 'Erro de verificação da gravação na Memória Fiscal.' ;
          #17 : sValorSaida := 'Data e hora do relógio interno do ECF inválidas.' ;
          #18 : sValorSaida := 'Falha no relógio interno do ECF.' ;
          #64 : sValorSaida := 'Falha na abertura da porta serial de comandos.' ;
          #65 : sValorSaida := 'Falha na abertura da porta serial do fisco.' ;
          #66 : sValorSaida := 'Falha na configuração do buffer da porta serial de comandos.' ;
          #67 : sValorSaida := 'Falha na configuração do buffer da porta serial do fisco.' ;
          #70 : sValorSaida := 'Falha na configuração dos parâmetros de comunicação da porta serial de comandos e do fisco.' ;
          #86 : sValorSaida := 'Falha na configuração do buffer da porta serial do mecanismo impressor.' ;
          #87 : sValorSaida := 'Falha na operação de flush da porta serial do mecanismo impressor.' ;
          #88 : sValorSaida := 'Falha na configuração dos parâmetros de comunicação do mecanismo impressor.' ;
         #160 : sValorSaida := 'Configuração inválida na DIP SWITCH.' ;
         #181 : sValorSaida := 'Erro de leitura na Memória de Fita Detalhe.' ;
         end;
   #25 : case sRetorno[2] of
         #145 : sValorSaida := 'Overflow na quantidade máxima de campos em um template.' ;
         #146 : sValorSaida := 'Overflow no buffer do template.' ;
         #149 : sValorSaida := 'Erro na recuperação de registros da Memória de Fita Detalhe.' ;
         #150 : sValorSaida := 'Reimpressão de documentos da Memória de Fita Detalhe interrompida.' ;
         #151 : sValorSaida := 'Término da área disponível para gravação na memória de fita-detalhe.' ;
         #152 : sValorSaida := 'Final dos registros na área de dados.' ;
         #153 : sValorSaida := 'Overflow no buffer de template da Memória de Fita Detalhe.' ;
         #154 : sValorSaida := 'Overflow no buffer de bloco da Memória de Fita Detalhe.' ;
         #155 : sValorSaida := 'Overflow no buffer de comando da Memória de Fita Detalhe.' ;
         #156 : sValorSaida := 'Início dos registros na área de dados.' ;
         end;
   #26 : case sRetorno[2] of
           #1 : sValorSaida := 'O relógio interno do ECF com horário anterior ao do último comando enviado.' ;
           #2 : sValorSaida := 'O relógio interno do ECF encontra-se adiantado em 30 dias ou mais em relação ao último comando enviado.' ;
          #16 : sValorSaida := 'Não foi localizado o registro de número de série na Memória Fiscal.' ;
          #17 : sValorSaida := 'Número de série do ECF incompatível no registro da Memória Fiscal.' ;
          #18 : sValorSaida := 'Marca do ECF incompatível no registro da Memória Fiscal.' ;
          #19 : sValorSaida := 'Tipo do ECF incompatível no registro da Memória Fiscal.' ;
          #20 : sValorSaida := 'Modelo do ECF incompatível no registro da Memória Fiscal.' ;
          #21 : sValorSaida := 'Número de série da Memória de Fita Detalhe incompatível no registro da Memória Fiscal.' ;
          #22 : sValorSaida := 'Logotipo da NCR do ECF incompatível no registro da Memória Fiscal.' ;
          #23 : sValorSaida := 'Não foi localizado o registro de usuário na Memória Fiscal.' ;
          #24 : sValorSaida := 'CNPJ incompatível no registro da Memória Fiscal.' ;
          #25 : sValorSaida := 'Inscrição Estadual incompatível no registro da Memória Fiscal.' ;
          #26 : sValorSaida := 'Inscrição Municipal incompatível no registro da Memória Fiscal.' ;
          #27 : sValorSaida := 'Criptografia do GT incompatível no registro da Memória Fiscal.' ;
          #28 : sValorSaida := 'Logotipo BR incompatível no registro da Memória Fiscal.' ;
          #29 : sValorSaida := 'Mecanismo impressor incompatível no registro da Memória Fiscal.' ;
          #31 : sValorSaida := 'Não foi localizado o registro de símbolo da moeda na Memória Fiscal.' ;
          #32 : sValorSaida := 'Símbolo da moeda incompatível no registro da Memória Fiscal.' ;
          #33 : sValorSaida := 'Não foi localizado o registro de criptografia do GT na Memória Fiscal.' ;
          #34 : sValorSaida := 'Não foi localizado o registro de troca de versão na Memória Fiscal.' ;
          #35 : sValorSaida := 'Erro de integridade no MD5.' ;
         end;
   #27 : case sRetorno[2] of
           #1 : sValorSaida := 'Configuração das DIP switches inválida.' ;
         end;  
   #29 : case sRetorno[2] of
           #1 : sValorSaida := 'Configuração de baudrate inválida para o mecanismo impressor.' ;
           #2 : sValorSaida := 'Erro mecânico no mecanismo impressor.' ;
           #3 : sValorSaida := 'Erro irrecuperável no mecanismo impressor.' ;
           #4 : sValorSaida := 'Erro no autocutter do mecanismo impressor.' ;
           #5 : sValorSaida := 'Erro auto recuperável no mecanismo impressor.' ;
         end;
   #31 : case sRetorno[2] of
           #1 : sValorSaida := 'O tamanho do texto do valor por extenso é maior do que o espaço disponível para impressão.' ;
           #2 : sValorSaida := 'O tamanho do texto do favorecido é maior do que o espaço disponível para impressão.' ;
           #3 : sValorSaida := 'O tamanho do texto do local é maior do que o espaço disponível para impressão.' ;
           #4 : sValorSaida := 'Posição inicial para impressão do dia da data do cheque inválida.' ;
           #5 : sValorSaida := 'O tamanho do texto do dia da data do cheque é maior do que o espaço disponível para impressão.' ;
           #6 : sValorSaida := 'Posição inicial para impressão do mês da data do cheque inválida.' ;
           #7 : sValorSaida := 'O tamanho do texto do mês da data do cheque é maior do que o espaço disponível para impressão.' ;
           #8 : sValorSaida := 'O tamanho do texto do ano da data do cheque é maior do que o espaço disponível para impressão.' ;
           #9 : sValorSaida := 'Posição inicial para impressão da linha adicional do cheque inválida.' ;
          #10 : sValorSaida := 'Altura da primeira linha de extenso inválida.' ;
          #11 : sValorSaida := 'Altura da segunda linha de extenso inválida.' ;
          #12 : sValorSaida := 'Altura da linha do favorecido inválida.' ;
          #13 : sValorSaida := 'Altura da linha do local e data inválida.' ;
          #14 : sValorSaida := 'Altura da linha adicional inválida.' ;
          #15 : sValorSaida := 'Altura da linha do valor inválida.' ;
          #16 : sValorSaida := 'Posição inicial para impressão da primeira linha do extenso do cheque inválida.' ;
          #17 : sValorSaida := 'Posição inicial para impressão do local inválida.' ;
          #18 : sValorSaida := 'Tamanho da linha adicional inválido.' ;
         end;
   #32 : case sRetorno[2] of
           #1 : sValorSaida := 'Velocidade da porta serial inválida.' ;
         end;

  end;

  Result := sValorSaida;
end;



{ ----------------------------- TACBrECFNCR ----------------------------- }

constructor TACBrECFNCR.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fsNCRComando   := TACBrECFNCRComando.create ;
  fsNCRResposta  := TACBrECFNCRResposta.create ;

  fpDevice.Baud      := 115200;
  fpDevice.Parity    := pNone;
  fpDevice.Stop      := s1;
  fpDevice.Data      := 8;
  fpDevice.HandShake := hsNenhum;
  fpDecimaisQtd      := 3 ;
  fpDecimaisPreco    := 2 ;

  { Variaveis internas dessa classe }
  fsNumLoja   := '' ;
  fsCNPJ      := '' ;
  fsIE        := '' ;
  fpModeloStr := 'NCR 7167/7197' ;
  fpMFD       := True ;
  fpTermica   := True ;
  fpIdentificaConsumidorRodape := True ;
  fsImprimeCheque := False ;
  fsLeituraCMC7   := False ;
  fsHorarioVerao  := '' ;
end;

destructor TACBrECFNCR.Destroy;
begin
  fsNCRComando.Free ;
  fsNCRResposta.Free ;

  inherited Destroy ;
end;

procedure TACBrECFNCR.Sincroniza ;
var
  ByteSYN : Byte;
begin
  fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }

  ByteSYN := 0;
  while (ByteSYN <> SYN) do     { Se ACK = 6 Comando foi reconhecido }
  begin
    fpDevice.Serial.Purge ;                   { Limpa a Porta }
    fpDevice.Serial.SendByte( SYN );  { Solicita sincronismo }

    ByteSYN := fpDevice.Serial.RecvByte( 200 ) ; { Resposta sincronismo }
    Try
      if ByteSYN = SYN then
      begin
         try
          NCRComando.Seq := fpDevice.Serial.RecvByte( 200 ) ; { Pega numero de sequencia }
         except
         end;
      end
      else
      if ByteSYN = WAK then
      begin
        sleep( 1000 ) ;
        continue ;
      end
      else
        raise EACBrECFSemResposta.create( ACBrStr(
              'Impressora '+fpModeloStr+' não responde ' ) );

    except
      on E : EACBrECFSemResposta do
      begin
        fpDevice.Serial.Purge ;

        if not DoOnMsgRetentar( E.Message +sLineBreak+sLineBreak+
          'Se o problema persistir, verifique os cabos, ou'+sLineBreak+
          'experimente desligar a impressora durante 5 seg,'+sLineBreak+
          'liga-la novamente, e repetir a operação...'
          , 'Sincronismo') then
        raise ;
      end ;
      else
         raise ;
    end ;
  end;
end;

procedure TACBrECFNCR.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+sLineBreak+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

  inherited Ativar ; { Abre porta serial }

  fsNumLoja       := '' ;
  fsCNPJ          := '' ;
  fsIE            := '' ;
  fsHorarioVerao  := '' ;
  fsImprimeCheque := False ;
  fsLeituraCMC7   := False ;
  try
     try
        Sincroniza ; // Sincroniza o sequencial de comando com a impressora

        // Verifica Modo de Operação
        NCRComando.Comando := '63' ;
        EnviaComando ;

        if NCRResposta.Params[0] <> '0' then  // Diferente de Modo Normal ?
           raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' esta em'+sLineBreak+
                                  'modo de intervenção técnica.'));
                                  
        NCRComando.Comando := '187' ;  // Obtendo o numero de colunas
        EnviaComando ;

        fpColunas := StrToIntDef( NCRResposta.Params[11], 48 ) ;
        fsImprimeCheque :=  NCRResposta.Params[2] = '1';
        fsLeituraCMC7   :=  NCRResposta.Params[5] = '1';
     except
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));
     end ;
  except
     Desativar ;
     raise ;
  end ;
end;

function TACBrECFNCR.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
Var ErroMsg    : String ;
    OldTimeOut : Integer ;
    ByteACK    : Byte ;
    ByteSOH    : Byte ;
begin
  cmd := NCRComando.PacoteEnvio ;

  ByteACK := 0 ;
  Result  := '' ;
  ErroMsg := '' ;
  fpComandoEnviado   := '' ;
  fpRespostaComando  := '' ;
  NCRResposta.Resposta := '' ;  // Zera resposta
  OldTimeOut := TimeOut ;
  TimeOut    := max(NCRComando.TimeOut, TimeOut) ;
  fpDevice.Serial.DTR := false ;  
  fpDevice.Serial.RTS := false ;

  try
     fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }

     while (ByteACK <> ACK) do     { Se ACK = 6 Comando foi reconhecido }
     begin
        ByteACK := 0 ;
        fpDevice.Serial.Purge ;                   { Limpa a Porta }

        if not TransmiteComando( cmd ) then
           continue ;

        try
           { espera ACK chegar na Porta por 2s  }
           try
              ByteACK := fpDevice.Serial.RecvByte( 2000 ) ;
           except
           end ;

           if ByteACK = 0 then
              raise EACBrECFSemResposta.create( ACBrStr(
                    'Impressora '+fpModeloStr+' não responde (ACK = 0)' ) )
           else if ByteACK = NAK then    { retorno em caracter 21d=15h=NACK }
              raise EACBrECFSemResposta.create( ACBrStr(
                    'Impressora '+fpModeloStr+' não reconheceu o Comando'+
                    sLineBreak+' (NAK)') )
           else if ByteACK = WAK then { retorno = WAK espera 1 segundo e reenvia solicitação }
           begin
             sleep( 1000 ) ;
             continue ;
           end
           else if ByteACK <> 6 then
              raise EACBrECFSemResposta.create( ACBrStr(
                    'Erro. Resposta da Impressora '+fpModeloStr+' inválida'+
                    sLineBreak+' (ACK = '+IntToStr(ByteACK)+')') ) ;
        except
           on E : EACBrECFSemResposta do
            begin
              fpDevice.Serial.Purge ;

              if not DoOnMsgRetentar( E.Message +sLineBreak+sLineBreak+
                 'Se o problema persistir, verifique os cabos, ou'+sLineBreak+
                 'experimente desligar a impressora durante 5 seg,'+sLineBreak+
                 'liga-la novamente, e repetir a operação...'
                 , 'LerACK') then
                 raise ;
            end ;
           else
              raise ;
        end ;

     end ;

     fpComandoEnviado := cmd ;
     sleep( 100 ) ;
     ByteSOH := 0 ;
     while ByteSOH <> Ord( SOH ) do
     begin
       { Envia pedido para encaminhar a resposta da ECF }
       fpDevice.Serial.SendByte( ord(ENQ) );

       { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
         falha na leitura LeResposta dispara Exceçao.
         Resposta fica gravada na váriavel "fpRespostaComando" }
       LeResposta ;

       Try
          ByteSOH := ord( fpRespostaComando[1] ) ;

          if ByteSOH = 0 then
             raise EACBrECFSemResposta.create( ACBrStr(
                   'Impressora '+fpModeloStr+' não responde (SOH = 0)' ) )
          else if ByteSOH = NAK then    { retorno em caracter 21d=15h=NACK }
             raise EACBrECFSemResposta.create( ACBrStr(
                   'Impressora '+fpModeloStr+' não reconheceu o Comando'+
                   sLineBreak+' (NAK)') )
          else if ByteSOH = WAK then { retorno = WAK espera 1 segundo e reenvia solicitação }
          begin
            sleep( 1000 ) ;
            continue ;
          end
          else if ByteSOH = Ord( CAN ) then { retorno = CAN erro na execução }
          begin
            NCRResposta.Resposta := fpRespostaComando ;
            ErroMsg := 'Erro: '+ IntToStrZero(Ord(NCRResposta.Retorno[1]), 3)+'/'+
                                 IntToStrZero(Ord(NCRResposta.Retorno[2]), 3)+
                                 ' - '+NCRResposta.DescRetorno  ;
            break ;
          end
          else if ByteSOH <> 1 then
             raise EACBrECFSemResposta.create( ACBrStr(
                   'Erro. Resposta da Impressora '+fpModeloStr+' inválida'+
                   sLineBreak+' (SOH = '+IntToStr(ByteSOH)+')') ) ;

          NCRResposta.Resposta := fpRespostaComando ;
          if NCRResposta.Seq <> NCRComando.Seq then
             raise EACBrECFERRO.Create(ACBrStr('Sequencia de Resposta diferente da enviada')) ;
           
       except
          on E : Exception do
          begin
             ErroMsg := E.Message ;
             break ;
          end
          else
            break ;
       end ;
     end;

     if ErroMsg <> '' then
      begin
        ErroMsg := ACBrStr('Erro retornado pela Impressora: '+fpModeloStr+#10+#10+
                   ErroMsg );
        raise EACBrECFSemResposta.create(ErroMsg) ;
      end
     else
        Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }

  finally
     TimeOut := OldTimeOut ;
  end ;
end;

function TACBrECFNCR.VerificaFimLeitura(var Retorno : AnsiString ;
   var TempoLimite : TDateTime) : Boolean ;
var
//  ByteWAK : Byte;
  TamRetorno : Integer;
begin
  Result := (LeftStr(Retorno,1) = chr( WAK )) ;
  if not result then
  begin
//  Result := (LeftStr(Retorno,1) = SOH) or (LeftStr(Retorno,1) = CAN) ;

    Result := Length( Retorno ) >= 5 ;
    if Result then
    begin
      // Verifica se pacote veio completo, caso comando sem erro
      if LeftStr(Retorno,1) = SOH then
      begin
        TamRetorno := Synautil.BinToInt( CharToBin( Retorno[5] ) + CharToBin( Retorno[4] ) );
        Result := Length( Retorno ) >= (TamRetorno + 6) ;
      end;
    end ;
  end ;
end;

function TACBrECFNCR.GetDataHora: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
begin
  NCRComando.Comando := '12' ;
  EnviaComando ;
  RetCmd := NCRResposta.Params[0] ;
  OldShortDateFormat := ShortDateFormat ;
  try
     ShortDateFormat := 'dd/mm/yy' ;
     Result := StrToDate(copy(RetCmd, 1,2) + DateSeparator +
                         copy(RetCmd, 3,2) + DateSeparator +
                         copy(RetCmd, 5,2)) ;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;

  RetCmd := NCRResposta.Params[1] ;
  Result := RecodeHour(  Result,StrToInt(copy(RetCmd,1,2))) ;
  Result := RecodeMinute(Result,StrToInt(copy(RetCmd,3,2))) ;
  Result := RecodeSecond(Result,StrToInt(copy(RetCmd,5,2))) ;

  fsHorarioVerao := NCRResposta.Params[2] ;
end;

function TACBrECFNCR.GetNumCupom: String;
begin
  NCRComando.Comando := '220' ;
  NCRComando.AddParam('02');
  EnviaComando ;
  Result := NCRResposta.Params[0] ;
end;

function TACBrECFNCR.GetNumCCF: String;
begin
  NCRComando.Comando := '220' ;
  NCRComando.AddParam('05');
  EnviaComando ;
  Result := NCRResposta.Params[0] ;
end;

function TACBrECFNCR.GetNumCRO: String;
begin
  NCRComando.Comando := '220' ;
  NCRComando.AddParam('01');
  EnviaComando ;

  Result := NCRResposta.Params[0] ;
end;

function TACBrECFNCR.GetNumLoja: String;
begin
  if fsNumLoja = '' then
  begin
     NCRComando.Comando := '3' ;
     NCRComando.AddParam('0');
     EnviaComando ;

     fsNumLoja := NCRResposta.Params[0] ;
     fsCNPJ    := NCRResposta.Params[1] ;
     fsIE      := NCRResposta.Params[2] ;
  end ;

  Result := fsNumLoja ;
end;

function TACBrECFNCR.GetNumECF: String;
begin
  NCRComando.Comando := '7' ;
  EnviaComando ;

  Result := NCRResposta.Params[0] ;
end;

function TACBrECFNCR.GetNumSerie: String;
begin
  NCRComando.Comando := '1' ;
  EnviaComando ;

  Result      := NCRResposta.Params[0] ;
end;

function TACBrECFNCR.GetNumVersao: String ;
begin
  NCRComando.Comando := '189' ;
  NCRComando.AddParam('0') ;
  EnviaComando ;

  Result :=  NCRResposta.Params[0] +'.'+NCRResposta.Params[1]+'.'+NCRResposta.Params[2] ;
end;

function TACBrECFNCR.GetTotalPago: Double;
begin
  try
     NCRComando.Comando := '70' ;
     NCRComando.AddParam('98');
     EnviaComando ;

     Result := StrToFloatDef(NCRResposta.Params[0],0) /100 ;
     Result := RoundTo( Result, -2) ;
  except
     on E : Exception do
     begin
       Result := 0 ;
       if Pos('012/015', E.Message) = 0 then
          raise ;
     end ;
  end ;
end;

function TACBrECFNCR.GetSubTotal: Double;
begin
  try
     NCRComando.Comando := '70' ;
     NCRComando.AddParam('95');
     EnviaComando ;

     Result := StrToFloatDef(NCRResposta.Params[0],0) /100 ;
     Result := RoundTo( Result, -2) ;
  except
     on E : Exception do
     begin
       Result := 0 ;
       if Pos('012/015', E.Message) = 0 then
          raise ;
     end ;
  end ;
end;


function TACBrECFNCR.GetEstado: TACBrECFEstado;
  Var
//    BitS : String ;
      StatusRZ : String ;
      DataMov  : String ;
      DataMovLiber  : TDateTime ;
      Dia, Mes, Ano : Word ;
begin
//  fpEstadoGeral := [];
  if (not fpAtivo) then
     fpEstado := estNaoInicializada
  else
  begin
    fpEstado := estDesconhecido ;

    // Verifica Estado da Redução Z
    NCRComando.Comando := '65' ;
    EnviaComando ;
    StatusRZ := NCRResposta.Params[0] ;
    DataMov  := NCRResposta.Params[1] ;

    if (StatusRZ = '9') then
    begin
      if fpEstado = estDesconhecido then
         fpEstado := estRequerZ ;
//      Include(fpEstadoGeral, estRequerZ) ;
    end;

    if (StatusRZ = '1') and
       (DataMov  = '000000') then
    begin
      NCRComando.Comando := '60' ; // Pega dados da ultima Reducao Z
      EnviaComando ;

      // Pega a Data e hora limite que a ECF estará bloqueada
      DataMovLiber := 0 ;
      if NCRResposta.Params[1] <> '000000' then
      begin
        Dia := StrToInt(Copy(NCRResposta.Params[1], 1, 2));
        Mes := StrToInt(Copy(NCRResposta.Params[1], 3, 2));
        Ano := StrToInt('20' + Copy(NCRResposta.Params[1], 5, 2));

        // Pois a ECF fica bloqueada até as 02:00h do dia seguinte da data do movimento
        DataMovLiber := EncodeDate(Ano, Mes, Dia);
        DataMovLiber := IncDay(DataMovLiber, 1) ;
        DataMovLiber := RecodeHour( DataMovLiber, 2) ;
        DataMovLiber := RecodeMinute( DataMovLiber, 0) ;
        DataMovLiber := RecodeSecond( DataMovLiber, 0) ;
      end;

      if GetDataHora <= DataMovLiber then
      begin
        if fpEstado = estDesconhecido then
           fpEstado := estBloqueada ;
//        Include(fpEstadoGeral, estBloqueada) ;
      end
      else
      begin
        if fpEstado = estDesconhecido then
           fpEstado := estRequerX ;
//        Include(fpEstadoGeral, estRequerX) ;
      end;
    end;

    // Verifica Contexto Atual
    NCRComando.Comando := '64' ;
    EnviaComando ;

    if NCRResposta.Params[0] = '15' then
    begin
      if fpEstado = estDesconhecido then
         fpEstado := estLivre ;
//      Include(fpEstadoGeral, estLivre) ;
    end;

    if (NCRResposta.Params[1] >= '04') then
    begin
      if fpEstado = estDesconhecido then
         fpEstado := estPagamento ;
//      Include(fpEstadoGeral, estPagamento) ;
    end;

    if (NCRResposta.Params[0] = '04') then
    begin
      if fpEstado = estDesconhecido then
         fpEstado := estVenda ;
//      Include(fpEstadoGeral, estVenda);
    end ;

    if (NCRResposta.Params[0] = '14') then
    begin
      if fpEstado = estDesconhecido then
         fpEstado := estRelatorio ;
//      Include(fpEstadoGeral, estRelatorio) ;
    end ;

    if (NCRResposta.Params[0] = '07') or
       (NCRResposta.Params[0] = '09') then
    begin
      if fpEstado = estDesconhecido then
         fpEstado := estNaoFiscal ;
//      Include(fpEstadoGeral, estNaoFiscal) ;
    end ;
    
  end ;

  Result := fpEstado ;
end;

function TACBrECFNCR.GetGavetaAberta: Boolean;
begin
  NCRComando.Comando := '67' ;
  EnviaComando ;

  Result := (NCRResposta.Params[0] = '1') ;
end;

function TACBrECFNCR.GetPoucoPapel: Boolean;
begin
  NCRComando.Comando := '66' ;
  EnviaComando ;

  Result := (NCRResposta.Params[0] = '1') ;
end;

function TACBrECFNCR.GetHorarioVerao: Boolean;
begin
  if fsHorarioVerao = '' then
     GetDataHora ;

  Result := (fsHorarioVerao = '1') ;
end;

procedure TACBrECFNCR.LeituraX ;
begin
  if fpEstado = estRequerx then
     NCRComando.Comando := '18'
  else
     NCRComando.Comando := '19' ;

  NCRComando.TimeOut := TempoInicioMsg + 2 ;  // apenas para o bloqueio de teclado funcionar
  EnviaComando ;
end;

procedure TACBrECFNCR.AbreGaveta ;
begin
  NCRComando.Comando := '206' ;
  EnviaComando ;
end;

procedure TACBrECFNCR.ReducaoZ(DataHora : TDateTime) ;
begin
  NCRComando.Comando := '17' ;
  NCRComando.AddParam('0');
  NCRComando.AddParam('0');
  NCRComando.TimeOut := TempoInicioMsg + 2 ;  // apenas para o bloqueio de teclado funcionar

  EnviaComando ;
end;

procedure TACBrECFNCR.MudaHorarioVerao ;
begin
  MudaHorarioVerao( not HorarioVerao ) ;
end;

procedure TACBrECFNCR.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  NCRComando.Comando := '11' ;
  if not EHorarioVerao then
     NCRComando.AddParam('0')
  else
     NCRComando.AddParam('1');

  EnviaComando ;
end;

procedure TACBrECFNCR.AbreCupom ;
begin
  NCRComando.Comando  := '21' ;
  NCRComando.AddParam('4');
  
  EnviaComando ;
end;

function  TACBrECFNCR.BuscaSequenciaVinculado( const CodFormaPagto : String ) : String ;
Var A, TotVinc : Integer ;
begin
  result := '' ; 
  NCRComando.Comando  := '224' ; // Verifica se tem documento vinculado para cancelar
  EnviaComando ;
  if CodFormaPagto <> '' then
     TotVinc := StrToIntDef( NCRResposta.Params[2], 0 )
  else
     TotVinc := StrToIntDef( NCRResposta.Params[0], 0 ) ;

  for A := 1 to  TotVinc do
  begin
    NCRComando.Comando  := '225' ; // Verifica a sequencia do pagamento a cancelar
    NCRComando.AddParam( IntToStr(A) );
    EnviaComando ;
    if CodFormaPagto <> '' then
    begin
       if NCRResposta.Params[3] = CodFormaPagto then
       begin
         result := NCRResposta.Params[0] ;
         break ;
       end;
    end
    else
    begin
      if NCRResposta.Params[2] = '2' then // Comprovante Emitido
      begin
        result := NCRResposta.Params[0] ;
        break ;
      end;
    end
  end;
end ;
 
procedure TACBrECFNCR.CancelaCupom(NumCOOCancelar: Integer);
var SeqVinculado : String ;
begin
  NCRComando.Comando  := '68' ;
  EnviaComando ;

  if NCRResposta.Params[0] = '0' then
     raise EACBrECFERRO.create(ACBrStr('Não existe documento para ser cancelado.')) ;

  SeqVinculado := BuscaSequenciaVinculado;
  if SeqVinculado <> '' then
  begin
    NCRComando.Comando  := '211' ;          // Cancela comprovante vinculado
    NCRComando.AddParam( SeqVinculado ) ;   // Seqüência do pagamento
    NCRComando.AddParam( '1' ) ;            // Numero de Parcelas
    NCRComando.AddParam( '0' ) ;            // Armazena Linhas na NVRAM 0/1
    EnviaComando ;

    NCRComando.Comando  := '22' ;           // Fecha o comprovante de estorno de CCD
    EnviaComando ;
  end;

  NCRComando.Comando  := '37' ;
  EnviaComando ;
end;

procedure TACBrECFNCR.CancelaItemVendido(NumItem: Integer);
begin
  NCRComando.Comando  := '35' ;
  NCRComando.AddParam(IntToStr(NumItem)) ;
  EnviaComando ;
end;

procedure TACBrECFNCR.EfetuaPagamento(CodFormaPagto : String ; Valor : Double ;
   Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
//Var
//  IdxSeq : Integer ;
begin
  NCRComando.Comando  := '42' ;
  NCRComando.AddParam( CodFormaPagto ) ;
  NCRComando.AddParam( '1' ) ;
  NCRComando.AddParam( FloatToStr( Valor ) ) ;
  NCRComando.AddParam( Observacao ) ;
  EnviaComando ;
end;

procedure TACBrECFNCR.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
 Var SL : TStringList ;
     I  : Integer ;
begin
  if not Consumidor.Enviado then
  begin
     NCRComando.Comando  := '207' ;
     NCRComando.AddParam(LeftStr(Consumidor.Documento,20));
     NCRComando.AddParam(LeftStr(Consumidor.Nome,30));
     NCRComando.AddParam(copy(Consumidor.Endereco, 1,40));
     NCRComando.AddParam(copy(Consumidor.Endereco,41,39));
     EnviaComando ;
  end ;

  if Trim(Observacao) <> '' then
  begin
     Observacao := AjustaLinhas(Observacao, 42) ;
     SL := TStringList.create ;
     try
        SL.Text := Observacao ;
        NCRComando.Comando  := '233' ;
        for I := 0 to SL.Count-1 do
           if I <= 1 then
              NCRComando.AddParam(SL[I]) ;
{ ???
        for A := I to 1 do
           NCRComando.AddParam('') ;
}
        EnviaComando ;
     finally
        SL.Free ;
     end ;
  end ;

  NCRComando.Comando  := '22' ;
  EnviaComando ;
end;

procedure TACBrECFNCR.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString);
begin
  if DescontoAcrescimo <> 0 then
  begin
    NCRComando.Comando  := '38' ; // Desconto no SubTotal
    if DescontoAcrescimo > 0 then
       NCRComando.Comando  := '40' ; // Acrescimo no SubTotal
    NCRComando.AddParam( FloatToStr( abs(DescontoAcrescimo) )  );
    EnviaComando ;
  end;
  
  NCRComando.Comando  := '36' ;
  EnviaComando ;
end;

procedure TACBrECFNCR.VendeItem(Codigo, Descricao : String ;
   AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
   ValorDescontoAcrescimo : Double ; Unidade : String ;
   TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
   CodDepartamento : Integer) ;
 Var
  ValDesc, ValTotal : Double ;
  IndAliq : Integer ;  
begin
  with NCRComando do
  begin
    ValTotal := RoundABNT(TruncFix(Qtd*ValorUnitario*100)/100,-2) ;
    Comando := '30' ;
    AddParam( LeftStr(Codigo,14) );
    AddParam( LeftStr(Descricao,233) );
    AddParam( FloatToStr(Qtd)  );
    AddParam( LeftStr(Unidade,3) );
    AddParam( FloatToStr(ValorUnitario)  );
    AddParam( FloatToStr(ValTotal)  );

    IndAliq := StrToIntDef( AliquotaECF, -1);
    if IndAliq >= 0 then
    begin
      AddParam( '1' ) ;
      AddParam( FormatFloat('###,##0.00',Aliquotas[Pred(IndAliq)].Aliquota) );
    end
    else
    begin
      if AliquotaECF = 'F' then
         AddParam( '3' )
      else if AliquotaECF = 'N' then
         AddParam( '4' )
      else
         AddParam( '2' ) ;

      AddParam( '0,00' );
    end;
  end ;
  EnviaComando ;

  { Se o desconto é maior que zero dá o comando de desconto de item }
  if ValorDescontoAcrescimo > 0 then
  begin
    if TipoDescontoAcrescimo = '%' then
       ValDesc := RoundTo( ValTotal * (ValorDescontoAcrescimo / 100), -2)
    else
       ValDesc := ValorDescontoAcrescimo ;

    if DescontoAcrescimo = 'D' then
       NCRComando.Comando  := '31'
    else
       NCRComando.Comando  := '33' ;
    NCRComando.AddParam( '0' ) ;
    NCRComando.AddParam( FloatToStr(ValDesc) ) ;
    EnviaComando ;
  end;
end;

procedure TACBrECFNCR.CarregaAliquotas;
var
  A         : Integer;
  Aliquota  : TACBrECFAliquota ;
  Aliq, TotAliq : Double ;
  IndAliq   : String ;
  iAliquotas: Integer ;
begin
  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  // Le a Quantidade de Aliquotas Cadastradas
  NCRComando.Comando := '160' ;
  NCRComando.Params.Add( '1' ) ;
  EnviaComando ;

  iAliquotas :=  StrToIntDef( NCRResposta.Params[0], 0 ) ;
  for A := 1 to iAliquotas do
  begin
    // Le a Aliquota Cadastrada
    IndAliq := IntToStrZero( A , 2 ) ;
    NCRComando.Comando := '161' ;
    NCRComando.Params.Add( '1' ) ;
    NCRComando.Params.Add( IndAliq ) ;
    EnviaComando ;

    Aliq := RoundTo( StrToIntDef( NCRResposta.Params[0], 0) / 100, -2 ) ;

    // Le Valores Acumulados na Aliquota
    NCRComando.Comando := '72' ;
    NCRComando.Params.Add( '1' ) ;
    NCRComando.Params.Add( FloatToStr(Aliq) ) ;
    EnviaComando ;

    TotAliq  := StrToFloatDef(NCRResposta.Params[1], 0 ) ;

    Aliquota := TACBrECFAliquota.create;

    Aliquota.Indice   := IndAliq ;
    Aliquota.Tipo     := 'T';
    Aliquota.Aliquota := Aliq ;
    Aliquota.Total    := TotAliq ;

    fpAliquotas.Add(Aliquota);
  end;
end;

procedure TACBrECFNCR.LerTotaisAliquota;
begin
  CarregaAliquotas ;
end;


procedure TACBrECFNCR.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String);
begin
  Tipo := UpCase(Tipo) ;

  NCRComando.Comando := '100' ;
  if Tipo = 'T' then
     NCRComando.Params.Add('1')
  else
     NCRComando.Params.Add('0');
  NCRComando.Params.Add( FloatToStr( Aliquota ) ) ;
  EnviaComando ;

  CarregaAliquotas ;
end;

procedure TACBrECFNCR.CarregaFormasPagamento;
Var A     : Integer ;
    Qtde  : Integer ;
    Ident : String ;
    FPagto : TACBrECFFormaPagamento ;

begin
  inherited CarregaFormasPagamento ;   { Cria fpFormasPagamentos }

  // Le a Quantidade de Formas de Pagamento Cadastradas
  NCRComando.Comando := '167' ;
  EnviaComando ;

  Qtde :=  StrToIntDef( NCRResposta.Params[0], 0 ) ;

  // Posiciona na primeira forma de pagamento cadastrado
  NCRComando.Comando := '168' ;
  EnviaComando ;

  for A := 1 to Qtde do
  begin
    // Pega a proxima forma de pagamento a ser lida
    NCRComando.Comando := '169' ;
    EnviaComando ;
    Ident := NCRResposta.Params[0] ;

    // Pega descricao da forma de pagamento
    NCRComando.Comando := '166' ;
    NCRComando.AddParam( Ident );
    EnviaComando ;

    try
       FPagto := TACBrECFFormaPagamento.create ;
       FPagto.Indice    := Ident ;
       FPagto.Descricao := NCRResposta.Params[1] ;
       FPagto.PermiteVinculado := NCRResposta.Params[0] = '1' ;

       fpFormasPagamentos.Add( FPagto ) ;
    except
       on E : Exception do
       begin
         fpFormasPagamentos.Free ;
         fpFormasPagamentos := nil ;
         raise ;
       end ;
    end;
  end;
end;

procedure TACBrECFNCR.LerTotaisFormaPagamento;
Var A : Integer ;
begin
  if not Assigned( fpFormasPagamentos ) then
     CarregaFormasPagamento ;

  For A := 0 to FormasPagamento.Count-1 do
  begin
    // Le Valores Acumulados na Forma de Pagamento
    NCRComando.Comando := '74' ;
    NCRComando.AddParam( FormasPagamento[A].Indice );
    EnviaComando ;
    FormasPagamento[A].Total := RoundTo( StrToFloatDef( NCRResposta.Params[0], 0) / 100, -2) ;
  end ;
end;

procedure TACBrECFNCR.ProgramaFormaPagamento( var Descricao: String;
  PermiteVinculado : Boolean; Posicao : String) ;
var  FPagto: TACBrECFFormaPagamento ;
     ProxIndice : Integer ;
begin
  ProxIndice := StrToIntDef(Posicao,0) ;
  if (ProxIndice < 1) or (ProxIndice > 20) then { Indice passado é válido ? }
  begin
     For ProxIndice := 2 to 20 do  { Procurando Lacuna }
     begin
        if AchaFPGIndice(IntToStr(ProxIndice)) = nil then
           break ;
     end ;
  end ;

  if ProxIndice > 20 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Formas de '+
                            'Pagamento'));

  NCRComando.Comando := '109' ;
  NCRComando.AddParam( IntToStr(ProxIndice) ) ;
  if PermiteVinculado then
     NCRComando.AddParam( '1' )
  else
     NCRComando.AddParam( '0' ) ;
  NCRComando.AddParam( LeftStr(Descricao,15) ) ;
  EnviaComando ;

  { Adicionando nova FPG no ObjectList }
  if Assigned( fpFormasPagamentos ) then
  begin
     FPagto := TACBrECFFormaPagamento.create ;
     FPagto.Indice    := IntToStr(ProxIndice) ;
     FPagto.Descricao := LeftStr(Descricao,15) ;
     FPagto.PermiteVinculado := PermiteVinculado  ;

     fpFormasPagamentos.Add( FPagto ) ;
  end ;
end;

procedure TACBrECFNCR.CarregaComprovantesNaoFiscais;
Var A    : Integer ;
    CNF  : TACBrECFComprovanteNaoFiscal ;
    Qtde : Integer ;
    Ident : String ;
begin
  inherited CarregaComprovantesNaoFiscais ;

  // Le a quantidade de totalizadores não fiscais cadastrados
  NCRComando.Comando := '163' ;
  EnviaComando ;

  Qtde :=  StrToIntDef( NCRResposta.Params[0], 0 ) ;

  // Posiciona no primeiro totalizador cadastrado
  NCRComando.Comando := '164' ;
  EnviaComando ;

  for A := 1 to Qtde do
  begin
    // Pega o proximo totalizador a ser lido
    NCRComando.Comando := '165' ;
    EnviaComando ;
    Ident := NCRResposta.Params[0] ;

    // Pega descricao da forma de pagamento
    NCRComando.Comando := '162' ;
    NCRComando.AddParam( Ident );
    EnviaComando ;

    try
      CNF := TACBrECFComprovanteNaoFiscal.create ;

      CNF.Indice    := Ident ;
      CNF.Descricao := NCRResposta.Params[0] ;

      fpComprovantesNaoFiscais.Add( CNF ) ;
    except
       on E : Exception do
       begin
         fpComprovantesNaoFiscais.Free ;
         fpComprovantesNaoFiscais := nil ;
         raise ;
       end ;
    end;
  end;
end;

procedure TACBrECFNCR.LerTotaisComprovanteNaoFiscal;
Var A : Integer ;
begin
  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais ;

  For A := 0 to ComprovantesNaoFiscais.Count-1 do
  begin
    // Le Valores Acumulados na Forma de Pagamento
    NCRComando.Comando := '76' ;
    NCRComando.AddParam( ComprovantesNaoFiscais[A].Indice );
    EnviaComando ;
    ComprovantesNaoFiscais[A].Contador := StrToIntDef( NCRResposta.Params[0], 0) ;
    ComprovantesNaoFiscais[A].Total    := RoundTo( StrToFloatDef( NCRResposta.Params[1], 0) / 100, -2) ;
  end ;
end;

procedure TACBrECFNCR.ProgramaComprovanteNaoFiscal(var Descricao : String;
   Tipo: String; Posicao : String);
 var ProxIndice : Integer ;
begin
  For ProxIndice := 1 to 20 do  { Procurando Lacuna }
  begin
    if AchaCNFIndice(IntToStr(ProxIndice)) = nil then
       break ;
  end ;

  if ProxIndice > 20 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Formas de '+
                            'Pagamento'));

  NCRComando.Comando := '104' ;
  NCRComando.AddParam( IntToStr(ProxIndice) ) ;
  NCRComando.AddParam( Descricao ) ;
  EnviaComando ;

  CarregaComprovantesNaoFiscais ;
end;

procedure TACBrECFNCR.AbreRelatorioGerencial(Indice : Integer) ;
begin
  NCRComando.Comando  := '28' ;
  NCRComando.AddParam( '1' ) ;
  EnviaComando ;
end;

procedure TACBrECFNCR.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
Var I  : Integer ;
    SL : TStringList ;
begin
  Linha := AjustaLinhas( Linha, Colunas );  { Formata as Linhas de acordo com "Coluna" }

  SL := TStringList.create ;
  try
     SL.Text := Linha ;
     For I := 0 to SL.Count-1 do
     begin
        NCRComando.Comando  := '204' ;
        NCRComando.AddParam(SL[I]) ;
        EnviaComando ;
     end ;
  finally
     SL.Free ;
  end ;
end;

procedure TACBrECFNCR.AbreCupomVinculado(COO, CodFormaPagto,
   CodComprovanteNaoFiscal :  String; Valor : Double ) ;
Var SeqVinculado : String ;
begin
  SeqVinculado := BuscaSequenciaVinculado( CodFormaPagto ) ;
  if SeqVinculado = '' then
     raise EACBrECFERRO.create(ACBrStr('Não registrado nenhum pagamento para comprovante vinculado.'));

  NCRComando.Comando := '210' ;
  NCRComando.AddParam( SeqVinculado ) ; // Seqüência do pagamento
  NCRComando.AddParam( '1' ) ;            // Numero de Parcelas
  NCRComando.AddParam( '0' ) ;            // Armazena Linhas na NVRAM 0/1
  EnviaComando ;
end;

procedure TACBrECFNCR.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha );
end;

procedure TACBrECFNCR.FechaRelatorio;
begin
  try
    FechaCupom ;
  except
    on E : Exception do
    begin
      if Pos('012/015', E.Message) = 0 then
         raise ;
    end;
  end;  
end;

procedure TACBrECFNCR.PulaLinhas(NumLinhas: Integer);
begin
  if NumLinhas = 0 then
     NumLinhas := LinhasEntreCupons ;

  NCRComando.Comando := '205' ;
  NCRComando.AddParam( IntToStr(NumLinhas) );
  EnviaComando ;
end;

procedure TACBrECFNCR.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal : Integer; Simplificada : Boolean);
begin
  with NCRComando do
  begin
     if Simplificada then
        Comando := '24'
     else
        Comando := '26' ;
        
     AddParam( IntToStr(ReducaoInicial) ) ;
     AddParam( IntToStr(ReducaoFinal) ) ;
  end ;
  EnviaComando ;
end;

procedure TACBrECFNCR.LeituraMemoriaFiscal(DataInicial,
   DataFinal: TDateTime; Simplificada : Boolean);
begin
  with NCRComando do
  begin
     if Simplificada then
        Comando := '23'
     else
        Comando := '25' ;
        
     AddParam( FormatDateTime('ddmmyy',DataInicial) ) ;
     AddParam( FormatDateTime('ddmmyy',DataFinal) ) ;
  end ;
  EnviaComando ;
end;

procedure TACBrECFNCR.CorrigeEstadoErro(Reducao: Boolean);
begin
  inherited CorrigeEstadoErro(Reducao) ;

  if Estado <> estLivre then
     try
        NCRComando.Comando := '20' ;
        EnviaComando ;
        sleep(500) ;
     except
     end ;
end;

function TACBrECFNCR.GetCNPJ: String;
begin
  if fsCNPJ = '' then
     GetNumLoja ;

  Result := fsCNPJ ;
end;

function TACBrECFNCR.GetIE: String;
begin
  if fsIE = '' then
     GetNumLoja ;

  Result := fsIE ;
end;

function TACBrECFNCR.GetDataMovimento: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
begin
  NCRComando.Comando := '65' ;
  EnviaComando ;
  RetCmd := NCRResposta.Params[1] ;
  OldShortDateFormat := ShortDateFormat ;
  try
     if NCRResposta.Params[1] = '000000' then
        result := 0
     else
     begin
       ShortDateFormat := 'dd/mm/yy' ;
       Result := StrToDate(copy(RetCmd, 1,2) + DateSeparator +
                           copy(RetCmd, 3,2) + DateSeparator +
                           copy(RetCmd, 5,2)) ;
     end;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;
end;

function TACBrECFNCR.GetGrandeTotal: Double;
begin
  NCRComando.Comando := '69' ;
  NCRComando.AddParam('10');
  EnviaComando ;
  
  Result := RoundTo( StrToFloatDef(NCRResposta.Params[0],0) / 100, -2);
end;

function TACBrECFNCR.GetNumCRZ: String;
begin
  NCRComando.Comando := '220' ;
  NCRComando.AddParam('03');
  EnviaComando ;
  
  Result := NCRResposta.Params[0] ;
end;

function TACBrECFNCR.GetTotalAcrescimos: Double;
begin
  NCRComando.Comando := '69' ;
  NCRComando.AddParam('08');
  EnviaComando ;

  Result := RoundTo( StrToFloatDef(NCRResposta.Params[0],0) / 100, -2) ;
end;

function TACBrECFNCR.GetTotalCancelamentos: Double;
begin
  NCRComando.Comando := '69' ;
  NCRComando.AddParam('09');
  EnviaComando ;
  
  Result := RoundTo( StrToFloatDef(NCRResposta.Params[0],0) / 100, -2);
end;

function TACBrECFNCR.GetTotalDescontos: Double;
begin
  NCRComando.Comando := '69' ;
  NCRComando.AddParam('07');
  EnviaComando ;

  Result := RoundTo( StrToFloatDef(NCRResposta.Params[0],0) / 100, -2);
end;

function TACBrECFNCR.GetTotalSubstituicaoTributaria: Double;
begin
  NCRComando.Comando := '69' ;
  NCRComando.AddParam('02');
  EnviaComando ;

  Result := RoundTo( StrToFloatDef(NCRResposta.Params[0],0) / 100, -2) ;
end;

function TACBrECFNCR.GetTotalIsencao: Double;
begin
  NCRComando.Comando := '69' ;
  NCRComando.AddParam('01');
  EnviaComando ;

  Result := RoundTo( StrToFloatDef(NCRResposta.Params[0],0) / 100, -2) ;
end;

function TACBrECFNCR.GetTotalNaoTributado: Double;
begin
  NCRComando.Comando := '69' ;
  NCRComando.AddParam('03');
  EnviaComando ;

  Result := RoundTo( StrToFloatDef(NCRResposta.Params[0],0) / 100, -2) ;
end;

function TACBrECFNCR.GetVendaBruta: Double;
Var
  VendaBrutaInicial : Double ;
begin
  NCRComando.Comando := '71' ;
  NCRComando.AddParam('10');
  EnviaComando ;

  VendaBrutaInicial := RoundTo( StrToFloatDef( NCRResposta.Params[0], 0) / 100, -2) ;
  Result := abs(GetGrandeTotal - VendaBrutaInicial) ;
end;

function TACBrECFNCR.GetNumCOOInicial: String;
begin
  NCRComando.Comando := '221' ;
  NCRComando.AddParam('02');
  EnviaComando ;

  Result := NCRResposta.Params[0] ;
end;

procedure TACBrECFNCR.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  if Trim(CPF_CNPJ) <> '' then
     Consumidor.AtribuiConsumidor(CPF_CNPJ,'','');
     
  NCRComando.Comando := '21' ;
  NCRComando.AddParam( '07' );
  EnviaComando ;
end;

procedure TACBrECFNCR.CancelaNaoFiscal;
begin
  CancelaCupom ;
end;

procedure TACBrECFNCR.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString);
begin
  NCRComando.Comando := '201' ;
  NCRComando.AddParam( CodCNF );
  NCRComando.AddParam( '' );
  NCRComando.AddParam( FloatToStr( Valor ) );
  EnviaComando ;
end;

procedure TACBrECFNCR.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
   MensagemRodape: AnsiString);
begin
  SubtotalizaCupom( DescontoAcrescimo ) ;
end;

procedure TACBrECFNCR.EfetuaPagamentoNaoFiscal(CodFormaPagto: String;
  Valor: Double; Observacao: AnsiString; ImprimeVinculado: Boolean);
begin
  EfetuaPagamento(CodFormaPagto, Valor, Observacao, ImprimeVinculado) ;
end;

procedure TACBrECFNCR.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
begin
  FechaCupom( Observacao ) ;
end;

procedure TACBrECFNCR.NaoFiscalCompleto(CodCNF: String; Valor: Double;
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

// Andre Bohn - Segundo suporte da NCR é mais seguro imprimir o cheque com
// o cupom fechado, eu fiz um teste com o cupom aberto e tive que trocar a MFD.
// Ele disse que futuramente vão preparar a impressão do cheque para ter
// o mesmo funcionamento das outras ECFs.
procedure TACBrECFNCR.ImprimeCheque(Banco: String; Valor: Double;
  Favorecido, Cidade: String; Data: TDateTime; Observacao: String);
begin
  if fsImprimeCheque then
  begin
    with NCRComando do
    begin
       Comando := '232' ;
       AddParam( FloatToStr( Valor ) ) ;
       AddParam( LeftStr(Favorecido,40) ) ;
       AddParam( LeftStr(Cidade,30) ) ;
       AddParam( Observacao ) ;
    end ;
    EnviaComando ;         // Envia comando para imprimir o Cheque
  end ;
end;

// Andre Bohn - Comando para fazer a leitura do CMC7
function TACBrECFNCR.LeituraCMC7 : AnsiString ;
begin
  Result :=  '';
  if fsLeituraCMC7 then
  begin
    with NCRComando do
    begin
       Comando := '216' ;
       AddParam( '0' ) ;
       AddParam( '' ) ;
    end ;
    EnviaComando ;       // Envia o comando para Ler o CMC7

    Result := NCRResposta.Params[0] ;
  end;
end;

function TACBrECFNCR.GetChequePronto: Boolean;
begin
  NCRComando.Comando := '66' ;
  EnviaComando ;         // Obtem o Estado da ECF

  Result := (NCRResposta.Params[2] = '1') ;
end;




end.
