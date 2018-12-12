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
|* 15/03/2006:  Daniel Simoes de Almeida
|*   Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
|* 15/05/2006:  Daniel Simoes de Almeida
|*   - Corrigido vários bugs: GetEstado, GetTotalPago, FechaRelatorio,
|*     FechaCupom
|* 18/06/2008:  Joao Victor Maia Fernandes
|*   LerTotaisAliquota, LerTotaisFormaPagamento, LerTotaisComprovanteNaoFiscal,
|*   TotalSubstituicaoTributaria, CNPJ, IE, DataMovimento, GrandeTotal,
|*   VendaBruta, TotalDescontos, TotalCancelamentos, TotalAcrescimos,
|*   NumCOOInicial, NumUltimoItem, TotalIsencao, TotalNaoTributado
******************************************************************************}
{$I ACBr.inc}

unit ACBrECFQuattro ;

interface
uses Classes,
     ACBrECFClass, ACBrDevice;

type
{ Classe filha de TACBrECFClass com implementaçao para Quattro }

{ TACBrECFQuattro }

TACBrECFQuattro = class( TACBrECFClass )
 private
    fsArredonda : AnsiChar ;
    fsNumVersao : String ;
    fsNumCRO    : String ;
    fsNumECF    : String ;
    fsOldSeq     : String ;
    fsFechando   : Boolean ;

    fsModelosCheque : TStringList ;
    fsArqFormato_ChequeTXT: String;

    procedure CarregaFormato_ChequeTXT ;
    function GetModelosCheque: TStringList;
    procedure SetArqFormato_ChequeTXT(const Value: String);
    procedure LeBufferSerial(const Cmd : String; AStringList: TStringList);

 protected
    function GetDataHora: TDateTime; override ;
    function GetNumCCF: String; override ;
    function GetNumCupom: String; override ;
    function GetNumECF: String; override ;
    function GetNumCRO: String; override ;
    function GetNumCRZ: String; override ;
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

    function GetNumUltimoItem: Integer; override ;
    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetNumCOOInicial: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetVendaBruta: Double; override ;
    function GetGrandeTotal: Double; override ;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalIsencao: Double; override ;
    function GetTotalNaoTributado: Double; override ;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;
    function VerificaFimImpressao(var TempoLimite: TDateTime) : Boolean ; override ;

 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    { Manupilaçao do Arquivo com a configuração de cheques }
    property ArqFormato_ChequeTXT : String read fsArqFormato_ChequeTXT
       write SetArqFormato_ChequeTXT ;
    property ModelosCheque : TStringList read GetModelosCheque
       write fsModelosCheque ;
    Function AchaModeloBanco( Banco : String ) : String ;

    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

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
    Procedure LeituraXSerial( Linhas : TStringList) ; override ;
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;
    Procedure PulaLinhas( NumLinhas : Integer = 0 ) ; override ;

    Procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ; override ;
    Procedure CancelaImpressaoCheque ; override ;
    
    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure MudaArredondamento( Arredondar : Boolean ) ; override ;
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False ); override ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal : Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;

    Procedure AbreGaveta ; override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  override;

    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal; override ;
    procedure ProgramaComprovanteNaoFiscal(var Descricao: string;
      Tipo: string = ''; Posicao: string = ''); override;

    procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ); override;
    procedure RegistraItemNaoFiscal(CodCNF: string;Valor: Double;
       Obs: AnsiString); override;
    procedure EfetuaPagamentoNaoFiscal(CodFormaPagto: string; Valor: Double;
       Observacao: AnsiString; ImprimeVinculado: Boolean); override;
    procedure FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer = 0);override;


 end ;

implementation
Uses {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows {$ENDIF},
     SysUtils, Math, ACBrConsts, ACBrUtil;

{ ----------------------------- TACBrECFQuattro ------------------------------ }

constructor TACBrECFQuattro.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.HandShake := hsDTR_DSR ;
//  fpDevice.HandShake := hsRTS_CTS ;

  { Variaveis internas dessa classe }
  fsFechando    := false ;
  fsArredonda   := ' ' ; 
  fsNumVersao   := '' ;
  fsNumECF      := '' ;
  fsNumCRO      := '' ;
  fsOldSeq      := '' ;

  fsModelosCheque := TStringList.create;

  fpColunas     := 40 ;
  fpModeloStr   := 'Quattro' ;
  fpRFDID       := 'QT' ;
end;

destructor TACBrECFQuattro.Destroy;
begin
  fsModelosCheque.Free ;

  inherited Destroy ;
end;

procedure TACBrECFQuattro.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+#10+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

//  fpDevice.HandShake := hsDTR_DSR ;
  inherited Ativar ; { Abre porta serial }

  fsNumVersao   := '' ;
  fsNumECF      := '' ;
  fsNumCRO      := '' ;
  fpModeloStr   := 'Quattro' ;

  try
     fpDevice.Serial.Purge ;
     sleep(100) ;

     { Testando a comunicaçao com a porta }
     if NumVersao = '' then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));
  except
     Desativar ;
     raise ;
  end ;
end;


function TACBrECFQuattro.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
Var ErroMsg : String ;
    Erro : Integer ;
    LeituraMF : Boolean ;
    Verificar : Boolean ;
    STATUS : AnsiChar ;
begin
  result    := '' ;
  ErroMsg   := '' ;
  Erro      := 0 ;
  Verificar := false ;
  LeituraMF := (cmd = '++') ;
  fpComandoEnviado   := '' ;
  fpRespostaComando  := '' ;

  { Codificando CMD de acordo com o protocolo da Quattro }
  cmd := #27 + '.' + cmd + '}' ;

  fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }

  while fpComandoEnviado = '' do
  begin
     if not LeituraMF then  {Se estiver lendo Mem.Fiscal tem dados no Buffer}
        fpDevice.Serial.Purge ;                   { Limpa a Porta }

     if not TransmiteComando( cmd ) then
        continue ;

     fpComandoEnviado := cmd ;
  end ;

  if fpDevice.HandShake = hsDTR_DSR then
     fpDevice.Serial.DTR := True ;  { Liga o DTR para ler a Resposta }

  if not fpDevice.Serial.CTS then
     fpDevice.Serial.RTS := false ;

  { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
    falha na leitura LeResposta dispara Exceçao }
  LeResposta ;
  Result := fpRespostaComando ;

  if Result = '' then
     Result := '.-0001ERRO:         98}' ;

  { Verificando por erros }
  ErroMsg := '' ;
  if (copy(Result, 1, 5) = '.-P00') and (Length(Result) = 7) then
   begin
     case Result[6] of
       '2'     : ErroMsg := 'Comando não foi enviado a Impressora.' ;
       '3'     : ErroMsg := 'Impressora fora de linha ou sem papel.' ;
       '4','6' : ErroMsg := 'Não foi recebida a resposta da Impressora.' ;
     end;
     Verificar := true ;
   end

  else if copy(Result, 1, 3) = '.-P' then
   begin
     try STATUS := Result[6] except STATUS := ' ' end ;

     if STATUS = '1' then
     begin
        ErroMsg   := 'Impressora fora de linha'+sLineBreak ;
        Verificar := true ;
     end ;

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
     Erro := StrToInt(copy(Result,Length(Result)-3,3));
     case Erro of
        02 : ErroMsg := 'O parâmetro de código enviado no comando não pode ser zero.' ;
        03 : ErroMsg := 'O parâmetro de valor enviado no comando possui caracteres não numéricos.' ;
        04 : ErroMsg := 'O parâmetro de quantidade enviado no comando não pode ser zero.' ;
        05 : ErroMsg := 'O primeiro dígito do parâmetro deve ser obrigatoriamente zero.' ;
        06 : ErroMsg := 'O valor informado no comando não pode ser zero.' ;
        07 : ErroMsg := 'Foi detectado um byte não alfanumérico.' ;
        08 : ErroMsg := 'O parâmetro informado só possui espaços ou zeros' ;
        09 : ErroMsg := 'O código de tributação não é válido.' ;
        10 : ErroMsg := 'O código de tributação não foi programado.' ;
        11 : ErroMsg := 'No há documento para autenticar.' ;
        12 : ErroMsg := 'O comando não foi terminado corretamente' ;
        13 : ErroMsg := 'O preço total do item não pode ser zero.' ;
        14 : ErroMsg := 'O cabeçalho não foi programado' ;
        15 : ErroMsg := 'O relógio não foi programado.' ;
        16 : ErroMsg := 'O número do caixa não foi programado.' ;
        17 : ErroMsg := 'O cupom está aberto.' ;
        18 : ErroMsg := 'O desconto não pode ser maior ou igual ao total.' ;
        19 : ErroMsg := 'A operação comandada não é permitida.' ;
        20 : ErroMsg := 'Código do par Contador/Totalizador é inválido.' ;
        23 : ErroMsg := 'Código de indicador inválido' ;
        25 : ErroMsg := 'Número de linhas maior que 10.' ;
        26 : ErroMsg := 'Parâmetro diferente de “S”ou “N”.' ;
        27 : ErroMsg := 'A porcentagem não confere.' ;
        28 : ErroMsg := 'O cupom não está aberto.' ;
        29 : ErroMsg := 'Não está em intervenção técnica.' ;
        31 : ErroMsg := 'Taxa não pode ser zero ou espaços.' ;
        32 : ErroMsg := 'O comando deve ser terminado com 4 zeros.' ;
        33 : ErroMsg := 'A Tabela está completa.' ;
        34 : ErroMsg := 'Primeiro caracter diferente de “T” ou “S”' ;
        35 : ErroMsg := 'Código de tributação inválido.' ;
        36 : ErroMsg := 'O valor não é válido.' ;
        37 : ErroMsg := 'A legenda não é válida.' ;
        38 : ErroMsg := 'O número da redução não é válido.' ;
        39 : ErroMsg := 'Registro não encontrado.' ;
        40 : ErroMsg := 'Não pode autenticar.' ;
        42 : ErroMsg := 'Comando não pode ser realizado neste modo de operação.' ;
        43 : ErroMsg := 'É preciso tirar leitura X.' ;
        44 : ErroMsg := 'É preciso executar redução Z.' ;
        45 : ErroMsg := 'Não é possível executar duas reduções no mesmo dia.' ;
        49 : ErroMsg := 'Comando permitido somente antes da primeira venda.' ;
        50 : ErroMsg := 'Não é permitido entrar no horário de verão das 23 às 24h.' ;
        51 : ErroMsg := 'Não é permitido sair do horário de verão das 24h à 1h.' ;
        52 : ErroMsg := 'Venda Bruta diária excederia 12 dígitos após o comando ou o valor total do item tem mais de 11 dígitos.' ;
        53 : ErroMsg := 'Número de caracteres ímpar.' ;
        54 : ErroMsg := 'Caracteres inválidos.' ;
        55 : ErroMsg := 'Modalidade de pagamento não ativa.' ;
        56 : ErroMsg := 'Acréscimo maior que o total.' ;
        57 : ErroMsg := 'Contador/Totalizador não fiscal desativado ou Tentativa de acumular em totalizadores não fiscais positivos e negativos no mesmo comprovante não fiscal.';
        58 : ErroMsg := 'Número de modalidades de pagamento maior que 10.' ;
        59 : ErroMsg := 'Cupom já foi totalizado. Falta fechar.' ;
        60 : ErroMsg := 'Não pode executar cancelamento.' ;
        61 : ErroMsg := 'O dia já foi encerrado.' ;
        62 : ErroMsg := 'O cupom está sendo totalizado.' ;
        63 : ErroMsg := 'A data é inválida.' ;
        64 : ErroMsg := 'Cupom com acumulador negativo não pode ser totalizado.' ;
        65 : ErroMsg := 'Comando válido apenas em cupom não fiscal.' ;
        66 : ErroMsg := 'Comando válido apenas em cupom fiscal.' ;
        67 : ErroMsg := 'Só é permitido um lançamento (ESC.07) em um comprovante não-fiscal' ;
        68 : ErroMsg := 'Impressora sem papel.' ;
        69 : ErroMsg := 'Ocorreu falta de energia durante execução de comando.' ;
        70 : ErroMsg := 'A palavra TOTAL não é permitida.' ;
        71 : ErroMsg := 'Acerto de relógio no modo venda só pode ser feita com data igual.' ;
        72 : ErroMsg := 'Acerto de relógio em venda só até 5 minutos.' ;
        73 : ErroMsg := 'Acerto de relógio em venda só uma vez após a redução.' ;
        75 : ErroMsg := 'Já se encontra no horário de verão' ;
        76 : ErroMsg := 'Já saiu do horário de verão' ;
        77 : ErroMsg := 'Comando não-definido' ;
        78 : ErroMsg := 'COO informado não consta na tabela' ;
        79 : ErroMsg := 'Mais de 2 minutos dentro do Comprovante Vinculado' ;
        80 : ErroMsg := '<esc>.08 não é aceito neste modo' ;
        81 : ErroMsg := 'Mais de 10 minutos dentro do relatório gerencial' ;
        83 : ErroMsg := 'Item a cancelar não existe' ;
        84 : ErroMsg := 'Item já foi cancelado' ;
        85 : ErroMsg := 'Comando com tamanho errado' ;
        86 : ErroMsg := 'Tabela de Vinculados vazia' ;
        87 : ErroMsg := 'Cancelamento de item em cupom com acréscimo / Apenas um acréscimo por cupom / Apenas um desconto no subtotal por cupom / Nenhum item após acréscimo ou desconto no subtotal.' ;
        88 : ErroMsg := 'Comando não aceito em cupom vinculado' ;
        89 : ErroMsg := 'Comando não aceito em relatório gerencial' ;
        90 : ErroMsg := 'Não há Segunda via para imprimir' ;
        91 : ErroMsg := 'Tipo de modalidade de pagamento não existe/ Tipo de parâmetro do comando não existe.' ;
        92 : ErroMsg := 'Valor do cheque não pode ser nulo ou valor não cabe no extenso.' ;
        93 : ErroMsg := 'Layout do cheque é inválido ou mecanismo de impressão do cheque não está disponível.' ;
        94 : ErroMsg := 'Não pode sair do horário de verão : existe uma redução com menos de uma hora de diferença.' ;
        95 : ErroMsg := 'Segunda via do comprovante não fiscal vinculado já foi impressa.' ;
        96 : ErroMsg := 'Erro na gravação de modelo com cheque' ;
        97 : ErroMsg := 'Desconto em ISS desabilitado / Desconto maior que o valor tributado.' ;
        98 : ErroMsg := 'Impressora não está respondendo.' ;
     else
        ErroMsg := copy(Result,7,Length(Result)-7) ;
     end ;
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
  end ;

  if ErroMsg <> '' then
   begin
     ErroMsg := ACBrStr('Erro retornado pela Impressora: '+fpModeloStr+
                sLineBreak + sLineBreak+
                ErroMsg );

     if (Erro = 68) or (ErroMsg = cACBrECFSemPapelException) then
        DoOnErrorSemPapel
     else
        raise EACBrECFSemResposta.create( ErroMsg ) ;
   end
  else
     Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }
end;

function TACBrECFQuattro.VerificaFimLeitura(var Retorno : AnsiString ;
   var TempoLimite : TDateTime) : Boolean ;
begin
  { Nota sobre o VerificaFimLeitura: A Quattro responde muito antes da
    Impressao terminar, o que pode causar problemas com comandos enviados logo
    após impressoes demoradas como a Leitura X (por exemplo). Para esses casos,
    é necessário ativar a propriedade "AguardaImpressao := True" }
  Result := (RightStr(Retorno,1) = '}') and (Length(Retorno) >= 3) ;
end;


function TACBrECFQuattro.VerificaFimImpressao(var TempoLimite: TDateTime): Boolean;
Var RetCmd : AnsiString ;
    I : Integer ;
    DT : TDateTime ;
begin
  { Essa função só é chamada se AguardaImpressao = True,
    Como essa função é executada dentro da "LeResposta", que por sua vez foi
    chamada por "EnviaComando", não podemos usar o método "EnviaComando" (ou
    teriamos uma chamada recursiva infinita), por isso o Loop abaixo envia o
    comando .23 diretamente para a Serial, e aguarda por 1 segundo a resposta...
     Se a Quattro consegir responder, significa que a Impressão Terminou }
  Result := false ;
  DT     := 0 ;
  if not EmLinha() then
     Sleep(100)
  else
   begin
     RetCmd := '' ;
     I      := 0 ;
     try
        fpDevice.Serial.Purge ;  { Limpa buffer de Entrada e Saida }
        fpDevice.EnviaString( #27 + '.23}' );   { Pede Status }
        Sleep(20) ;
     except
     end ;

     while (not VerificaFimLeitura( RetCmd, DT ) ) and (I < 10) do
     begin
        try
           RetCmd := RetCmd + fpDevice.Serial.RecvPacket(100) ;
        except
        end ;
        Inc( I ) ;
     end ;

     Result := VerificaFimLeitura(RetCmd, DT) ;
     if Result then
        Result := (copy(RetCmd,6,1) = '0')
   end ;
end;

function TACBrECFQuattro.GetDataHora: TDateTime;
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
        { Obs.: Quattro nao retorna os Segundos }
     finally
        ShortDateFormat := OldShortDateFormat ;
     end ;
  end ;
end;

function TACBrECFQuattro.GetNumCupom: String;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '56'+'09' ) ;
  Result := '      ' ;

  if LeftStr(RetCmd, 4) = '.+09' then
     Result := IntToStrZero( StrToIntDef( copy(RetCmd,5,4), 0), 6) ;
end;

function TACBrECFQuattro.GetNumCCF: String;
Var RetCmd : AnsiString ;
begin
  Result := '' ;
  RetCmd := EnviaComando( '27'+'1' ) ;

  if LeftStr(RetCmd, 3) = '.+C' then
     Result := IntToStrZero( StrToIntDef( copy(RetCmd,121,4), 0), 6) ;
end;

function TACBrECFQuattro.GetNumCRO: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumCRO) = '' then
  begin
     RetCmd := EnviaComando( '56'+'02' ) ;
     if LeftStr(RetCmd, 4) = '.+02' then
        fsNumCRO := IntToStrZero( StrToIntDef(copy(RetCmd,5,4),0),4) ;
  end ;

  Result := fsNumCRO ;
end;

function TACBrECFQuattro.GetNumECF: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumECF) = '' then
  begin
     RetCmd := EnviaComando( '27' ) ;  { F ‚ o menor, mais rapido de ler }
        
     if LeftStr(RetCmd, 3) = '.+C' then
        fsNumECF := IntToStrZero( StrToIntDef(copy(RetCmd,4,3),0),4) ;
  end ;

  Result := fsNumECF ;
end;

function TACBrECFQuattro.GetNumSerie: String;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '56'+'03' ) ;
  Result := '' ;
  if LeftStr(RetCmd, 4) = '.+03' then
     Result := Trim(Copy(RetCmd,5,6)) ;
end;

function TACBrECFQuattro.GetNumVersao: String ;
Var RetCmd : AnsiString ;
begin
  if fsNumVersao = '' then
  begin
     RetCmd := EnviaComando( '56'+'04' ) ;
     if (LeftStr(RetCmd, 4) = '.+04') then
        fsNumVersao := copy(RetCmd,5,5) ;
  end ;

  Result := fsNumVersao ;
end;

function TACBrECFQuattro.GetTotalPago: Double;
Var RetCmd : AnsiString ;
    SubTot, Falta : Double ;
    P : Integer ;
begin
  Result := 0 ;

  RetCmd := EnviaComando( '56'+'08' ) ;
  if pos('S', copy(RetCmd,5,2)) > 0 then   // Verificando se o Estado é Pagamento 
  begin
     RetCmd := EnviaComando('28') ;
     P      := pos('!',RetCmd) ;
     if P > 0 then
     begin
        SubTot := StrToFloatDef(copy(RetCmd,19,12),0) / 100;
        SubTot := RoundTo(SubTot, -2) ;
        Falta  := StrToFloatDef(copy(RetCmd,100,12),0) / 100 ;
        Falta  := RoundTo(Falta, -2) ;
        Result := max( RoundTo(SubTot - Falta,-2) ,0) ; { evitar negativo }
     end ;
  end ;
end;

function TACBrECFQuattro.GetSubTotal: Double;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando('28') ;
  Result := 0 ;
  if pos('!',RetCmd) > 0 then
     Result := RoundTo(StrToFloatDef(copy(RetCmd,19,12),0) / 100, -2) ;
end;

function TACBrECFQuattro.GetEstado: TACBrECFEstado;
Var
  RetCmd : AnsiString ;
  FlagZ, FlagX : AnsiChar ;
begin
  if (not fpAtivo) then
     fpEstado := estNaoInicializada
  else
   begin
     RetCmd := EnviaComando( '28' ) ;
     if pos('!',RetCmd) > 0 then
     begin
        try FlagX := RetCmd[124] except FlagX := ' ' end ;
        try FlagZ := RetCmd[18]  except FlagZ := ' ' end ;

        { Status pode ser: C - concluida, P - Pendente, E - Erro no Comando }
        //Status  := UpperCase(copy(RetCmd,7,1)) ;
        //Transacao := UpperCase(Trim(copy(RetCmd,8,8))) ;

        fpEstado := estDesconhecido ;

        if FlagZ = 'F' then
           fpEstado := estRequerZ
        else if FlagX = 'N' then
           fpEstado := estRequerX
        else if FlagZ = 'S' then
           fpEstado := estBloqueada
        else if fsFechando then
           fpEstado := estPagamento ;

        if fpEstado = estDesconhecido then
        begin
           RetCmd := EnviaComando( '56'+'08' ) ;

           if pos('S', copy(RetCmd,5,2)) > 0 then
              fpEstado := estPagamento ;
        end ;

        if fpEstado = estDesconhecido then
        begin
           RetCmd := EnviaComando( '56'+'06' ) ;

           if (LeftStr(RetCmd, 5) = '.+06S') then
           begin
              if copy(RetCmd,6,1) = 'S' then        // Cupom Fiscal Aberto
                 fpEstado := estVenda
              else if copy(RetCmd,7,1) = 'S' then   // Cupom Vinculado Aberto
                 fpEstado := estRelatorio ;
           end ;
        end ;

        if fpEstado = estDesconhecido then
        begin
           RetCmd := EnviaComando( '56'+'05' ) ;

           if (LeftStr(RetCmd, 5) = '.+05S') then
              fpEstado := estRelatorio ;
        end ;

        if fpEstado = estDesconhecido then
           fpEstado := estLivre ;
     end ;
   end ;

  Result := fpEstado ;
end;

function TACBrECFQuattro.GetGavetaAberta: Boolean;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando('22') ;
  Result := (copy(RetCmd,1,6) <> '.+G000')
end;

function TACBrECFQuattro.GetPoucoPapel: Boolean;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '23' ) ;
  Result := (copy( RetCmd, 6,1) = '5') ;
end;

function TACBrECFQuattro.GetHorarioVerao: Boolean;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '28' ) ;
  Result := False ;
  if pos('!',RetCmd) > 0 then
     Result := (copy(RetCmd,54,1) = 'S') ;
end;

function TACBrECFQuattro.GetArredonda: Boolean;
Var RetCmd : AnsiString ;
begin
  if fsArredonda = ' ' then
  begin
     RetCmd := EnviaComando( '56'+'10' , 6) ;
     if (LeftStr(RetCmd, 4) = '.+10') then
        fsArredonda := RetCmd[5] ;
  end ;

  Result := (fsArredonda = 'S') ;
end;

procedure TACBrECFQuattro.LeituraX ;
begin
  AguardaImpressao := True ;
  EnviaComando('13N' , 45 ) ;
end;

procedure TACBrECFQuattro.LeituraXSerial(Linhas: TStringList);
begin
  Linhas.Clear ;
  LeBufferSerial('13|', Linhas);
end;

procedure TACBrECFQuattro.LeBufferSerial(const Cmd: String;
  AStringList: TStringList);
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
end;

procedure TACBrECFQuattro.ReducaoZ(DataHora : TDateTime) ;
begin
  AguardaImpressao := true ;
  EnviaComando( '14N', 50 ) ;

  if DataHora <> 0 then
     try
        EnviaComando('35'+FormatDateTime('hhnnssddmmyy',DataHora) );
     except
     end ;
end;

procedure TACBrECFQuattro.AbreGaveta ;
begin
  EnviaComando('21',7) ;
end;

procedure TACBrECFQuattro.MudaHorarioVerao ;
begin
  MudaHorarioVerao( not HorarioVerao );
end;

procedure TACBrECFQuattro.MudaHorarioVerao(EHorarioVerao: Boolean);
Var Cmd  : AnsiString ;
begin
  If EHorarioVerao then Cmd := 'S' else Cmd := 'N' ;

  EnviaComando( '36' + Cmd, 3 ) ;
end;

procedure TACBrECFQuattro.MudaArredondamento(Arredondar: Boolean);
Var Cmd  : AnsiString ;
begin
  If Arredondar then Cmd := 'S' else Cmd := 'N' ;

  EnviaComando( '57' + '01' + Cmd, 3 ) ;
  fsArredonda := ' ' ;
end;

procedure TACBrECFQuattro.AbreCupom ;
Var CPF_CNPJ : String ;
begin
  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }
//  if Estado = estRequerX then
//     raise EACBrECFERRO.create(ACBrStr('Leitura X inicial ainda não foi emitida'));

  CPF_CNPJ := '' ;
  if (Consumidor.Documento <> '') then
     CPF_CNPJ := PadRight(Consumidor.Documento,20) ;

  AguardaImpressao := True ;
  EnviaComando('17'+CPF_CNPJ, 10) ;
  fsFechando := false ;
  Consumidor.Enviado := (CPF_CNPJ <> '') and
                        (Trim(Consumidor.Nome)+Trim(Consumidor.Endereco)='')
end;

procedure TACBrECFQuattro.CancelaCupom(NumCOOCancelar: Integer);
Var RetCmd   : AnsiString ;
    Cancelou : Boolean ;
    SubTot   : Double ;
begin
  AguardaImpressao := True ;
  RetCmd   := EnviaComando( '05' ,25) ;
  Cancelou := (copy(RetCmd,1,2) = '.+') ;

  if (not Cancelou) and (Estado = estPagamento) then { Pagamento aberto, Efetua Pagamento, Fecha e Cancela }
  begin
     SubTot := Subtotal ;
     EnviaComando('10'+'01'+IntToStrZero( Round(SubTot*100) ,12) ) ; { 10-Paga }

     EnviaComando('12', 10 ) ;   { 12-Fecha }
     EnviaComando('05' ,25) ;    { 05-Cancela }
  end ;

  FechaRelatorio ;   { Fecha relatorio se ficou algum aberto (só por garantia)}
end;

procedure TACBrECFQuattro.CancelaItemVendido(NumItem: Integer);
begin
  EnviaComando( '04' + IntToStrZero(NumItem,3) ) ;
end;

procedure TACBrECFQuattro.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
begin
  Observacao := '{' + copy(Observacao,1,80) ;

  EnviaComando('10' + CodFormaPagto + IntToStrZero( Round(Valor*100) ,12) +
                     Observacao , 5, 5) ;

  fsFechando := true ;
end;

procedure TACBrECFQuattro.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
Var Linhas : TStringList ;
    I      : Integer ;
    Obs    : AnsiString ;
begin
  Observacao := AjustaLinhas( Observacao, Colunas, 8 );
  Obs        := '' ;

  Linhas := TStringList.Create ;
  try
     Linhas.Text := Observacao ;

     for I := 0 to min(Linhas.Count-1 ,8) do
        Obs := Obs + '0' + PadRight( Linhas[I] , Colunas) ;
  finally
     Linhas.Free ;
  end ;

  { Fecha cupom }
  AguardaImpressao := True ;
  EnviaComando( '12' + Obs, 10 ) ;

  fsFechando := false ;
end;

procedure TACBrECFQuattro.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString );
begin
  if DescontoAcrescimo < 0 then
   begin
     DescontoAcrescimo := abs(DescontoAcrescimo) ;
     EnviaComando('03' + StringofChar(' ',10) +
                  IntToStrZero(Round(DescontoAcrescimo*100),12), 3) ;
   end
  else if DescontoAcrescimo > 0 then
      EnviaComando('11' + '  ' + '0000' +
                  IntToStrZero(Round(DescontoAcrescimo*100),11), 3) ;

  { Inicia fechamento com formas de Pagamento }
//  EnviaComando('10') ;

  fsFechando  := true ;
end;

procedure TACBrECFQuattro.VendeItem(Codigo, Descricao : String ;
   AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
   ValorDescontoAcrescimo : Double ; Unidade : String ;
   TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
   CodDepartamento : Integer) ;
Var QtdStr, ValorStr, Descr2 : String ;
    ValDesc, ValTotal : Double ;
begin
  { Obs.: Quattro nao usa parametro Unidade }
  if Qtd > 9999 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'Quantidade deve ser inferior a 9999.'));

  { Quattro não permite Acrescimo por Item }
  if (ValorDescontoAcrescimo > 0) and (DescontoAcrescimo = 'A') then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'ECF '+fpModeloStr+' não permite Acréscimo por Item'));

  Codigo  := PadRight(Codigo,13) ;    { Ajustando Tamanhos }
  if Unidade <> '' then
     Descricao := Descricao + ' ' + PadRight(Unidade,2);

  Descr2  := '' ;                 { Usa descriçao Grande ? }
  if DescricaoGrande Then
     Descr2 := copy(Descricao,24, Length(Descricao)) ;

  Descr2 := Trim(Descr2) ;
  if Length(Descr2) > 0 then
     Descr2 := PadRight(Descr2, min(Trunc(Length(Descr2)/40)+1,4)*40 ) ;

  Descricao   := PadRight(Descricao,23) ; {23 e nao 24 porque adiciona o campo Sinal}
  QtdStr      := IntToStrZero( Round( Qtd*1000 ) ,7) ;
  ValorStr    := IntToStrZero( Round( ValorUnitario*1000 ) ,9) ;

  if Arredonda then
     ValTotal := RoundABNT( Qtd*ValorUnitario, -2 )
  else
     ValTotal := RoundTo(TruncFix(Qtd*ValorUnitario*100)/100,-2) ;
     
  EnviaComando( '01' + Codigo + QtdStr + ValorStr + StringOfChar(' ',12) + '~' +
                Descricao + AliquotaECF + Descr2 ) ;
  fsFechando := false ;

  if ValorDescontoAcrescimo > 0 then
  begin
     if TipoDescontoAcrescimo = '%' then
        ValDesc := RoundTo( ValTotal * (ValorDescontoAcrescimo / 100), -2)
     else
        ValDesc := ValorDescontoAcrescimo ;
        
     EnviaComando('02' + StringOfChar(' ',10) +
                         IntToStrZero( Round(ValDesc*100) ,12) ) ;
  end ;
end;

procedure TACBrECFQuattro.CarregaAliquotas;
Var RetCmd, AliquotasStr, AliqStr : AnsiString ;
    Aliquota : TACBrECFAliquota ;
    ValAliq : Double ;
    A : Integer ;
begin
  AliquotasStr := '' ;
  AliqStr      := '' ;

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

  inherited CarregaAliquotas ;   { Cria fpAliquotas }

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
end;

procedure TACBrECFQuattro.LerTotaisAliquota;
// Autor João Victor Maia Fernandes
Var A, Posicao : Integer;
     RetCmd , AliquotasStr : AnsiString;
begin
  if not Assigned( fpAliquotas ) then
     CarregaAliquotas;

  AliquotasStr := '';
  Posicao      := 1;

  RetCmd := EnviaComando( '27'+'2' );
  if LeftStr(RetCmd, 3) = '.+C' then
     AliquotasStr := Copy(RetCmd, 95, 15);

  RetCmd := EnviaComando( '27'+'3' );
  if LeftStr(RetCmd, 3) = '.+C' then
     AliquotasStr := AliquotasStr + Copy(RetCmd,8,115);

  RetCmd := EnviaComando( '27'+'4' );
  if LeftStr(RetCmd, 3) = '.+C' then
     AliquotasStr := AliquotasStr + Copy(RetCmd,8,115);

  RetCmd := EnviaComando( '27'+'5' );
  if LeftStr(RetCmd, 3) = '.+C' then
     AliquotasStr := AliquotasStr + Copy(RetCmd,8,115);

  For A := 0 to fpAliquotas.Count-1 do
  begin
     fpAliquotas[A].Total := RoundTo( StrToFloatDef(
                  copy(copy(AliquotasStr, Posicao, 15), 4, 12), 0) / 100, -2);
     Delete(aliquotasstr,1,15)
//   Posicao := Posicao + 15;
  end;
end;


procedure TACBrECFQuattro.ProgramaAliquota(Aliquota: Double; Tipo: Char;
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


function TACBrECFQuattro.AchaICMSAliquota( var AliquotaICMS: String):
  TACBrECFAliquota;
  Var AliquotaStr : String ;
begin
  AliquotaStr := '' ;
  Result      := nil ;

  {Por indice, permite T01, TT01 ou T1 => todas devem ser indice = T01 }
  
  if pos(copy(AliquotaICMS,1,2), 'TT,SS') > 0 then { Corrige Duplo T ou S }
     AliquotaICMS := copy(AliquotaICMS,2,5) ;

  case AliquotaICMS[1] of
    'I' : AliquotaStr := 'I  ' ;
    'N' : AliquotaStr := 'N  ' ;
    'F' : AliquotaStr := 'F  ' ;
    'T' : AliquotaICMS := 'TT'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}
    'S' : AliquotaICMS := 'TS'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}
  end ;

  if AliquotaStr = '' then
     Result := inherited AchaICMSAliquota( AliquotaICMS )
  else
     AliquotaICMS := AliquotaStr ;
end;

procedure TACBrECFQuattro.CarregaFormasPagamento;
Var RetCmd, Str, Descricao : AnsiString ;
    Cont : Integer ;
    FPagto : TACBrECFFormaPagamento ;
begin
  Str := '' ;
  RetCmd := EnviaComando('29' + '5') ;
  if copy(RetCmd,1,3) = '.+T' then
  begin
     Str := Str + copy(RetCmd, 72, 45) ;

     RetCmd := EnviaComando('29' + '6') ;
     if copy(RetCmd,1,3) = '.+T' then
        Str := Str + copy(RetCmd, 8, 105) ;
  end ;

  inherited CarregaFormasPagamento ;       {Inicializa fpFormasPagamentos}

  for Cont := 1 to 10 do
  begin
    Descricao   := TrimRight( copy(Str, (Cont * 15) - 14, 15) ) ;

    if Descricao <> '' then
    begin
       FPagto := TACBrECFFormaPagamento.create ;

       FPagto.Indice    := IntToStrZero(Cont,2) ;
       FPagto.Descricao := Descricao ;
       FPagto.PermiteVinculado := (Descricao[1] = '$') ;

       fpFormasPagamentos.Add( FPagto ) ;
    end ;
  end
end;

procedure TACBrECFQuattro.LerTotaisFormaPagamento;
// autor joao victor maia fernandes
 Var A : Integer;
     RetCmd , FPGStr, Token : AnsiString;
begin
  if not Assigned( fpFormasPagamentos ) then
     CarregaFormasPagamento ;

  FPGStr := '';

  RetCmd := EnviaComando( '27'+'5' );
  if LeftStr(RetCmd, 3) = '.+C' then
     FPGStr := FPGStr + copy(RetCmd,8 ,119);

  RetCmd := EnviaComando( '27'+'6' );
  if LeftStr(RetCmd, 3) = '.+C' then
     FPGStr := FPGStr + copy(RetCmd,8 ,48);

  For A := 0 to fpFormasPagamentos.Count-1 do
  begin
     Token := copy( FPGStr, (A*16)+1, 16 ) ;
     fpFormasPagamentos[A].Total :=
                RoundTo( StrToFloatDef( copy(Token, 5, 12), 0) / 100, -2);
  end;
end;

procedure TACBrECFQuattro.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String);
Var FPagto : TACBrECFFormaPagamento ;
begin
  If PermiteVinculado then
     Descricao := '$'+Descricao ;

  Descricao := PadRight(Descricao,15) ;

  EnviaComando( '37' + Descricao ) ;

  { Adcionanodo nova FPG no ObjectList }
  FPagto := TACBrECFFormaPagamento.create ;
  FPagto.Indice    := IntToStrZero(FormasPagamento.Count,2) ;
  FPagto.Descricao := Descricao ;
  FPagto.PermiteVinculado := PermiteVinculado ;
  fpFormasPagamentos.Add( FPagto ) ;
end;

procedure TACBrECFQuattro.CarregaComprovantesNaoFiscais;
Var RetCmd, Str, Descricao : AnsiString ;
    Cont : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  Str    := '';
  RetCmd := EnviaComando('29' + '7') ;
  if copy(RetCmd,1,3) <> '.+T' then exit ;
  Str := Str + copy(RetCmd, 8, 120) ;

  RetCmd := EnviaComando('29' + '8') ;
  if copy(RetCmd,1,3) <> '.+T' then exit ;
  Str := Str + copy(RetCmd, 8, 120) ;

  RetCmd := EnviaComando('29' + '9') ;
  if copy(RetCmd,1,3) <> '.+T' then exit ;
  Str := Str + copy(RetCmd, 8, 60) ;

  inherited CarregaComprovantesNaoFiscais ;{Inicializa fpComprovantesNaoFiscais}

  for Cont := 1 to 20 do
  begin
    Descricao := Trim( copy(Str, (Cont * 15) - 14, 15) ) ;

    if (Descricao <> '') and
       (Descricao <> StringOfChar('-',15)) then
    begin
       CNF := TACBrECFComprovanteNaoFiscal.create ;
       CNF.Indice := IntToStrZero(Cont+10,2) ;
       CNF.Descricao := Descricao ;

       fpComprovantesNaoFiscais.Add( CNF ) ;
    end
  end ;
end;


procedure TACBrECFQuattro.LerTotaisComprovanteNaoFiscal;
/// autor joao victor maia fernandes
 Var I : Integer;
     RetCmd , CNFStr, Token : AnsiString;
begin
  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais;

  CNFStr := '';

  RetCmd := EnviaComando( '27'+'6' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,56 ,64);

  RetCmd := EnviaComando( '27'+'7' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'8' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,112);

  RetCmd := EnviaComando( '27'+'9' );
  if LeftStr(RetCmd, 3) = '.+C' then
     CNFStr := CNFStr + copy(RetCmd,8 ,60);

  For I := 0 to fpComprovantesNaoFiscais.Count-1 do
  begin
     Token := copy( CNFStr, (I*16)+1, 16 ) ;
     fpComprovantesNaoFiscais[I].Contador :=  StrToIntDef( copy(Token,1,4), 0);
     fpComprovantesNaoFiscais[I].Total :=
         RoundTo( StrToFloatDef( copy(Token, 5, 12), 0) / 100, -2);
  end;
end;

procedure TACBrECFQuattro.ProgramaComprovanteNaoFiscal(var Descricao : string ;
   Tipo : string ; Posicao : string) ;
Var CNF : TACBrECFComprovanteNaoFiscal ;
begin
  if Trim(Tipo) = '' then
     Tipo := '+' ;

  if (pos(Tipo,'&+-') = 0) or (Length(Tipo) > 1) then
     raise EACBrECFERRO.Create(ACBrStr('Os Tipos válidos para Quattro são:'+sLineBreak+
                            '&  Criaçao de um novo Grupo (Titulo)'+sLineBreak+
                            '+  Entrada de Recursos'+sLineBreak+
                            '-  Saida de Recursos') ) ;

  EnviaComando( '38' + 'N' + PadRight(Tipo + Descricao,15) ) ;

  { Adcionanodo novo CNF no ObjectList }
  CNF := TACBrECFComprovanteNaoFiscal.create ;
  CNF.Indice    := IntToStrZero(ComprovantesNaoFiscais.Count,2) ;
  CNF.Descricao := Descricao ;
  fpComprovantesNaoFiscais.Add( CNF ) ;
end;


procedure TACBrECFQuattro.AbreRelatorioGerencial(Indice : Integer) ;
begin
  AguardaImpressao := True ;
  EnviaComando( '13' + 'S' ,50 );
end;

procedure TACBrECFQuattro.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  ImprimirLinhaALinha( Linha, '080' );
end;

procedure TACBrECFQuattro.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
begin
  COO := IntToStrZero(StrToInt(COO),4) ;

  AguardaImpressao := True ;
  EnviaComando( '1900' + COO + CodFormaPagto ,10 ) ;
end;

procedure TACBrECFQuattro.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha ) ;
end;

procedure TACBrECFQuattro.FechaRelatorio;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '56'+'05' ) ;
  if (LeftStr(RetCmd, 5) = '.+05S') then  // Relatorio Gerencial ? //
  begin
     AguardaImpressao := True ;
     EnviaComando('08', 10) ;
  end ;

  RetCmd := EnviaComando( '56'+'06' ) ;
  if (LeftStr(RetCmd, 5) = '.+06S') then  // Relatorio Gerencial ? //
  begin
     AguardaImpressao := True ;
     EnviaComando( '12', 10 ) ;
   end ;
end;

procedure TACBrECFQuattro.PulaLinhas(NumLinhas: Integer);
begin
  if NumLinhas = 0 then
     NumLinhas := LinhasEntreCupons ;

  NumLinhas := min(NumLinhas,9) ;

  EnviaComando('089'+IntToStrZero(NumLinhas,1)) ;
end;

procedure TACBrECFQuattro.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada : Boolean);
Var Espera : Integer ;
begin
  // Quattro não possui Leitura Simplificada
  Espera := 20 + (ReducaoFinal - ReducaoInicial) ;
  AguardaImpressao := True ;
  EnviaComando( '15' + IntToStrZero(ReducaoInicial,4) +
                       IntToStrZero(ReducaoFinal  ,4) , Espera ) ;
end;

procedure TACBrECFQuattro.LeituraMemoriaFiscal(DataInicial, DataFinal: TDateTime;
   Simplificada : Boolean);
  Var Espera : Integer ;
begin
  // Quattro não possui Leitura Simplificada
  Espera := 20 + DaysBetween(DataInicial,DataFinal) ;
  AguardaImpressao := True ;
  EnviaComando('16' + FormatDateTime('ddmmyy',DataInicial) +
                      FormatDateTime('ddmmyy',DataFinal  ) ,  Espera ) ;
end;

procedure TACBrECFQuattro.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas : TStringList; Simplificada : Boolean);
begin
  // Quattro não possui Leitura Simplificada
  LeBufferSerial( '15' + IntToStrZero(ReducaoInicial,4) +
                         IntToStrZero(ReducaoFinal  ,4) + '|', Linhas) ;
end;

procedure TACBrECFQuattro.LeituraMemoriaFiscalSerial(DataInicial,
  DataFinal: TDateTime; Linhas : TStringList; Simplificada : Boolean);
begin
  // Quattro não possui Leitura Simplificada
  LeBufferSerial( '16' + FormatDateTime('ddmmyy',DataInicial)+
                         FormatDateTime('ddmmyy',DataFinal)  + '|', Linhas );
end;

function TACBrECFQuattro.GetChequePronto: Boolean;
//Var RetCmd : AnsiString ;
begin
//  RetCmd := EnviaComando('23') ;
//  Result := (copy( RetCmd, 5,1) = '0') ;
  Result := True ;
end;

procedure TACBrECFQuattro.ImprimeCheque(Banco: String; Valor: Double;
  Favorecido, Cidade: String; Data: TDateTime; Observacao: String);
Var Dia,Mes,Ano   : String ;
    Formato, Obs1, Obs2 : String ;
begin
  if fsModelosCheque.Count = 0 then
     CarregaFormato_ChequeTXT ;

  EnviaComando('59' + '01' + LeftStr(Favorecido,65) ) ;
  EnviaComando('59' + '02' + LeftStr(Cidade,30) ) ;

  Dia        := IntToStrZero(  DayOf(Data),2) ;
  Mes        := IntToStrZero(MonthOf(Data),2) ;
  Ano        := RightStr(IntToStrZero( YearOf(Data),4),2) ;
  EnviaComando('59' + '03' + Dia + Mes + Ano ) ;

  EnviaComando('59' + '04' + IntToStrZero(Round(Valor * 100),12) ) ;

  Obs1 := copy(Observacao, 1,60) ;
  Obs2 := copy(Observacao,61,60) ;
  EnviaComando('59' + '05' + Obs1) ;
  EnviaComando('59' + '06' + Obs2) ;

  EnviaComando('59' + '07' + 'Real') ;
  EnviaComando('59' + '08' + 'Reais') ;

  Banco   := IntToStrZero(StrToIntDef(Banco,1),3) ;
  Formato := AchaModeloBanco( Banco ) ;
  AguardaImpressao := True;
  EnviaComando('59' + '09' + Banco + 'S' + Formato, 30 ) ;
end;

procedure TACBrECFQuattro.CancelaImpressaoCheque;
begin
   { Quattro não possui comando para cancelar a impressão de cheques}
   raise EACBrECFERRO.Create(ACBrStr('Impressora '+fpModeloStr+ ' não possui comando para '+
                          'cancelar a impressão de cheques. ' + sLineBreak +
                          'Por favor desligue e ligue a impressora ou insira '+
                          'um documento.'));
end;

function TACBrECFQuattro.AchaModeloBanco(Banco: String): String;
Var I : Integer ;
begin
  Result := '' ;
  Banco  := Poem_Zeros(Trim(Banco),3) ;

  if fsModelosCheque.Count = 0 then
     CarregaFormato_ChequeTXT ;

  For I := 0 to fsModelosCheque.Count -1 do
  begin
     if Banco = Copy(fsModelosCheque[I],1,3) then
     begin
        Result := OnlyNumber(
                    Copy(fsModelosCheque[I],4,Length(fsModelosCheque[I])) ) ;
        Break ;
     end ;
  end ;
end;

procedure TACBrECFQuattro.CarregaFormato_ChequeTXT;
Var
  Msg, ArqTemp : String ;
begin
  { Verificando se o arquivo é válido }
  if (fsArqFormato_ChequeTXT <> '') and
     (not FileExists( fsArqFormato_ChequeTXT )) then
  begin
     Msg := ACBrStr( 'Arquivo '+fsArqFormato_ChequeTXT+' não encontrado. '+
                     'Valores padrões serão utilizados.' ) ;
     raise EACBrECFErro.Create( Msg );

     fsArqFormato_ChequeTXT := '' ;
  end ;

  if fsArqFormato_ChequeTXT = '' then
     ArqTemp := ExtractFilePath( ParamStr(0) )+'Formato_cheque.txt'
  else
     ArqTemp := fsArqFormato_ChequeTXT ;

  fsModelosCheque.Clear ;
  { Adcionando valores default }
  if (not FileExists( ArqTemp )) then
   begin
     with fsModelosCheque do
     begin
        Add('00108080607071005') ;
        Add('00308080707061005') ;
        Add('00408080707071005') ;
        Add('00808080607061005') ;
        Add('02108080706071005') ;
        Add('02208080607071005') ;
        Add('02408080607071005') ;
        Add('02708080707061005') ;
        Add('02908120607081005') ;
        Add('03108080706071005') ;
        Add('03208070607081005') ;
        Add('03308080608071005') ;
        Add('03408070607071005') ;
        Add('03608070707061005') ;
        Add('03708080706071005') ;
        Add('03808110707061005') ;
        Add('04108070708071005') ;
        Add('04808080606061005') ;
        Add('07008080707071005') ;
        Add('10408080708091005') ;
        Add('15110070607071005') ;
        Add('15308080608061005') ;
        Add('18408080707071005') ;
        Add('20108090706061005') ;
        Add('21408080808081005') ;
        Add('23008080805091005') ;
        Add('23108080706081005') ;
        Add('23308080707061005') ;
        Add('23709090706071005') ;
        Add('24408080607071005') ;
        Add('25808100607061005') ;
        Add('26708080808061005') ;
        Add('29108060706071005') ;
        Add('29408080607071005') ;
        Add('30808080607071005') ;
        Add('32008080707081005') ;
        Add('34108100808081005') ;
        Add('34708090607081005') ;
        Add('35108080606081005') ;
        Add('35308080606061005') ;
        Add('35608110506061005') ;
        Add('35609100605071005') ;
        Add('36608090607061005') ;
        Add('36908090607061005') ;
        Add('37008070707061005') ;
        Add('37208080706081005') ;
        Add('37608100808081005') ;
        Add('38908100807081005') ;
        Add('39208070807071005') ;
        Add('39408060707091005') ;
        Add('39908080707071005') ;
        Add('40909090706061005') ;
        Add('42008070706071005') ;
        Add('42208080707071005') ;
        Add('42408100706071005') ;
        Add('47208060707081005') ;
        Add('47908100607061005') ;
        Add('48708080607071005') ;
        Add('48908070707071005') ;
        Add('49908070707071005') ;
        Add('61108080707061005') ;
        Add('74514070706071005') ;
        Add('90005050505050505') ;
     end ;
   end
  else
     fsModelosCheque.LoadFromFile( ArqTemp );
end;

function TACBrECFQuattro.GetModelosCheque: TStringList;
begin
  if fsModelosCheque.Count = 0 then
     CarregaFormato_ChequeTXT ;

  Result := fsModelosCheque;
end;

procedure TACBrECFQuattro.SetArqFormato_ChequeTXT(const Value: String);
begin
  fsArqFormato_ChequeTXT := Value;
  CarregaFormato_ChequeTXT ;
end;

procedure TACBrECFQuattro.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  EnviaComando('19',5);
end;

procedure TACBrECFQuattro.RegistraItemNaoFiscal(CodCNF: string;Valor: Double;
    Obs: AnsiString);
begin
  EnviaComando('07' + CodCNF + IntToStrZero(Round(Valor * 100), 12),3);
end;

procedure TACBrECFQuattro.EfetuaPagamentoNaoFiscal(CodFormaPagto: string;
   Valor: Double; Observacao: AnsiString; ImprimeVinculado: Boolean);
begin
  Observacao := '{' + copy(Observacao,1,80) ;

  EnviaComando('10' + CodFormaPagto + IntToStrZero( Round(Valor*100) ,12) +
                     Observacao , 5, 5) ;
end;

procedure TACBrECFQuattro.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
begin
  EnviaComando('12', 10); { 12-Fecha Não fiscla }
end;

function TACBrECFQuattro.GetNumCRZ: String;   // Maicon da Silva Evangelista
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '56'+'01' ) ;

  if LeftStr(RetCmd, 4) = '.+01' then
     Result := IntToStrZero( StrToIntDef(copy(RetCmd,5,4),0),4) ;
end;

function TACBrECFQuattro.GetTotalSubstituicaoTributaria: Double;
// autor João Victor Maia Fernandes
var
  vsString : AnsiString;
begin
  Result := 0;
  vsString := EnviaComando('27'+'2');
  if LeftStr(vsString,3) = '.+C' then
    Result := StrToFloatDef(Copy(vsString,43,12),0)/100;
end;

function TACBrECFQuattro.GetCNPJ: String;
// autor joao victor maia fernandes
 const letras : array[0..4] of string = ('A','B','C','D','E');
 var
  Retorno, Traco : AnsiString;
  tListaCnpj : TStringList;
  I : Integer;
begin
  Result := '';
  Traco  := StringOfChar('-',22) ;
  tListaCnpj := TStringList.Create;
  try
     for I := 0 to 4 do
     begin
        Retorno := EnviaComando( '29' + letras[I] );
        if (LeftStr(Retorno,3)='.+T') then
        begin
           if Copy(Retorno,8,22) = Traco then
              break;

           tListaCnpj.Add( copy(Retorno,8,22) );

           if Copy(Retorno,61,22) = Traco  then
              break;

           tListaCnpj.Add( Copy(Retorno,61,22) );
        end;
     end;

     for I := 0 to tListaCnpj.Count-1 do
     begin
        if tListaCnpj.Strings[i] <> Traco  then
           Result := Trim(tListaCnpj.Strings[i])
       else
          Break;
     end;
  finally
     tListaCnpj.Free ;
  end ;
end;

function TACBrECFQuattro.GetIE: String;
// autor joao victor maia fernandes
 const letras : array[0..4] of string = ('A','B','C','D','E');
 var
  Retorno, Traco :AnsiString;
  tListaInsc :TStringList;
  I : Integer;
begin
  Result := '';
  Traco  := StringOfChar('-',21) ;
  tListaInsc:=TStringList.Create;
  try
     for I := 0 to 4 do
     begin
        Retorno:=EnviaComando( '29' + letras[I] );
        if (LeftStr(Retorno,3) = '.+T') then
        begin
           if Copy(Retorno,30,21) = Traco then
              break;

           tListainsc.Add( copy(Retorno,30,21) );

           if Copy(Retorno,83,21) = Traco then
              break;

           tListainsc.Add( Copy(Retorno,83,21) );
        end;
     end;

     for I := 0 to tListainsc.Count-1 do
     begin
        if tListainsc.Strings[i] <> Traco then
           Result := Trim( tListainsc.Strings[i] )
        else
           Break;
     end;
  finally
     tListaInsc.Free ;
  end ;
end;

function TACBrECFQuattro.GetDataMovimento: TDateTime;
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

function TACBrECFQuattro.GetGrandeTotal: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,20,17),0) / 100;
end;

function TACBrECFQuattro.GetTotalAcrescimos: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'9');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,76,12),0) / 100;
end;

function TACBrECFQuattro.GetTotalCancelamentos: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
  begin
     Result := StrToFloatDef(copy(wretorno,61,12),0)/100;
     Result := Result + (StrToFloatDef(copy(wretorno,77,12),0) / 100);
  end;
end;

function TACBrECFQuattro.GetTotalDescontos: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,93,12),0) / 100;
end;

function TACBrECFQuattro.GetVendaBruta: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,45,12),0) / 100;
end;

function TACBrECFQuattro.GetNumCOOInicial: String;
var
  wretorno: AnsiString;
begin
  Result   := '';
  wretorno := EnviaComando('27'+'1');
  if copy(wretorno,1,3) = '.+C' then
    Result := Copy(wretorno,14,4);
end;

function TACBrECFQuattro.GetNumUltimoItem: Integer;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('28');
  if copy(wretorno,1,3) = '.+0' then
     Result := StrToInt(copy(wretorno,3,4));
end;

function TACBrECFQuattro.GetTotalIsencao: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'2');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,19,12),0) / 100;
end;

function TACBrECFQuattro.GetTotalNaoTributado: Double;
var
  wretorno: AnsiString;
begin
  Result   := 0;
  wretorno := EnviaComando('27'+'2');
  if copy(wretorno,1,3) = '.+C' then
     Result := StrToFloatDef(copy(wretorno,31,12),0) / 100;
end;


end.


