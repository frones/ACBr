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
|* 06/09/2004:  Daniel Simoes de Almeida
|*   Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
|* 22/03/2005:  Daniel Simoes de Almeida  / Valmor Florez
|*   Corrigido Bug na AbreCupom, que omitia o Endereço do Emissor,
|*    -  Bug Reportado por: Valmor Florez - Evoluthiva
|*   Inserido novo comando que permite imprimir o CNPJ / CPF do Cliente, na
|*      AbreCupom
|* 22/06/2005:  Valmor Florez
|*   Corrigido Bug na VendeItem, que interpretava errado Produtos vendidos sem
|*   sem nenhuma casa decimal como 3 casas decimais.
|* 08/12/2005:  Daniel Simoes de Almeida
|*  - Diminuido tempo de alguns Sleeps de 100 para 10 a fim de agilizar a
|*    comunicaçao com o ECF (experimental)
|* 07/05/2006:  Alan Lucas
|*  - Corrigido Bug em Método ZeraVinculados, que não cancelava o Vinculo 0
|* 25/03/2007:  Alan Lucas
|*   - Método: GetNumUltimoItem
|* 01/04/2007:  Daniel Simoes de Almeida
|*  - Implementados métodos de Cupom Não Fiscal
|* 20/04/2007:  Daniel Simoes de Almeida
|*  - Corrigido BUG na leitura do NumCupom após a abertura do Cupom Fiscal
|*  - Corrigido BUG na detecção de Estado estPagamento, quando ocorre
|*    SubtotalizaCupom sem desconto ou acrescimo
|*  - Corrigido BUG no VerificaFimImpressao (não estava lendo toda a resposta)
|* 18/08/2007:  Daniel Simoes de Almeida
|*  - Adicionada as propriedades: NumLoja, IE, CNPJ, NumCRO, DataMovimento,
|*    VendaBruta, TotalDescontos, TotalAcrescimos, TotalCancelamentos,
|*    NumCOOInicial, TotalIsenca, TotalNaoTributado, TotalSubstituicaoTributaria,
|*  - Adicionado os métodos: LerTotaisAliquota, LerTotaisFormaPagamento,
|*    LerTotaisComprovanteNaoFiscal,
|*  - Corrigido Bug em: LeituraMemoriaFiscalSerial (TimeOut e VerificaFimLeitura)
******************************************************************************}

{$I ACBr.inc}

unit ACBrECFSchalter ;

interface
uses
  Classes,
  {$IFNDEF NOGUI}
   {$IF DEFINED(VisualCLX)}
      QControls, QDialogs,
   {$ELSEIF DEFINED(FMX)}
      FMX.Controls, FMX.Dialogs, System.UITypes,
   {$ELSE}
      Controls, Dialogs,
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
   ByteCabecalho = #146 ;        { #146 -> Ligando os bits 1,4,7 }
   
type
{ Classe filha de TACBrECFClass com implementaçao para Schalter }

{ TACBrECFSchalter }

TACBrECFSchalter = class( TACBrECFClass )
 private
    fsBytesResp : Integer ;
    fsNumVersao : String ;
    fsNumECF    : String ;
    fsTotalPago : Double ;
    fsEmPagamento : Boolean ;
    fsDadosUsuario : AnsiString ;
    fsDadosLeituraX : AnsiString ;

    Function PreparaCmd( cmd : AnsiString ) : AnsiString ;
    Procedure ZeraVinculados ;

    function GetDadosUsuario: AnsiString;
    function GetDadosLeituraX: AnsiString;

    property DadosUsuario  : AnsiString read GetDadosUsuario ;
    property DadosLeituraX : AnsiString read GetDadosLeituraX ;

 protected
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumECF: String; override ;
    function GetNumCRO: String; override ;
    function GetNumCRZ: String; override ;
    function GetNumSerie: String; override ;
    function GetNumLoja: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetArredonda : Boolean; override ;

    function GetNumCOOInicial: String; override ;
    function GetNumUltimoItem: Integer; override ;
    function GetGrandeTotal: Double; override ;
    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetDataMovimento: TDateTime; override ;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;
    function VerificaFimImpressao(var TempoLimite: TDateTime) : Boolean ; override ;

 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    Property BytesResp : Integer read fsBytesResp write fsBytesResp ;
    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0 ;
       MensagemRodape : AnsiString  = '') ; override ;
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;

    Property CNPJ : String read GetCNPJ ;
    Property IE   : String read GetIE ;

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

    function GetHorarioVerao: Boolean; override ;
    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure CorrigeEstadoErro(Reducao: Boolean = True) ; override ;
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False); override ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal : Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ; override ;

    Procedure AbreGaveta ; override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota ; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  override;

    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;
 end ;

implementation
Uses SysUtils, Math,
    {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
    ACBrConsts, ACBrUtil;

{ ----------------------------- TACBrECFSchalter ----------------------------- }

constructor TACBrECFSchalter.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.Parity    := pEven ;
  fpDevice.HandShake := hsRTS_CTS ;
  { Variaveis internas dessa classe }
  fsBytesResp   := 1 ;
  fsNumVersao   := '' ;
  fsNumECF      := '' ;
  fsTotalPago   := 0 ;
  fsEmPagamento := false ;
  fsDadosUsuario:= '' ;

  fpColunas   := 48 ;  { Na verdade Schalter imprime apenas 40 colunas, mas
                         todos os comandos de Impressao de Linha precisam
                         receber uma string de 48 caracteres.... Vai entender..}
  fpModeloStr := 'Schalter' ;
  fpRFDID     := 'SC' ;
end;

destructor TACBrECFSchalter.Destroy;
begin

  inherited Destroy ;
end;

procedure TACBrECFSchalter.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+#10+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

  fpDevice.HandShake := hsRTS_CTS ;
  inherited Ativar ; { Abre porta serial }

  fsNumVersao := '' ;
  fsNumECF    := '' ;
  fsBytesResp := 1  ;
  fsDadosUsuario:= '' ;
  fsDadosLeituraX:= '' ;

  try
     { Testando a comunicaçao com a porta }
     if Trim(NumSerie) = '' then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));
  except
     Desativar ;
     raise ;
  end ;
end;


function TACBrECFSchalter.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
Var ErroMsg : String ;
    Erro : Integer ;
begin

  Result  := '' ;
  Erro    := 0 ;
  ErroMsg := '' ;
  fpComandoEnviado   := '' ;
  fpRespostaComando  := '' ;

  try
     { Codificando CMD de acordo com o protocolo da Schalter }
     cmd := PreparaCmd( cmd ) ;

     fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }

     while fpComandoEnviado = '' do
     begin
        fpDevice.Serial.Purge ;                   { Limpa a Porta }

        if not TransmiteComando( cmd ) then
           continue ;

        fpComandoEnviado := cmd ;
     end ;

     sleep(20) ;

     { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
       falha na leitura LeResposta dispara Exceçao }
     LeResposta ;
     Result := fpRespostaComando ;
     if Result = '' then
        Result := #0 ;

     { Verificando por erros }
     if (BytesResp >= 0) or (Length(fpRespostaComando) = 1) then   
        Erro := ord( Result[1] ) ;

     case Erro of
        0 : ErroMsg := '' ;
        2 : ErroMsg := 'Impressora não responde.' ;
       27 :  { Tudo OK.  Verificando o CheckSum recebido }
          if Result <> PreparaCmd( copy(Result,3,Length(Result)-3) ) then
          begin
             ErroMsg := 'CheckSum do Retorno não é válido.' ;
             Result  := #01 ;
          end ;
       65 : ErroMsg := 'Comando não permitido com a Impressora EM VENDA.' ;
       68 : ErroMsg := 'Cabeçalho de Venda já impresso' ;
       69 : ErroMsg := 'Cabeçalho de Venda ainda não foi impresso' ;
       70 : ErroMsg := 'Valor da venda do Item é inválido' ;
       73, 118 : ErroMsg := 'Valor Total de Pagamentos‚ inferior ao Total da'+
                            ' Venda  Erro:('+IntToStr(Erro)+')' ;
       79 : ErroMsg := 'Checksum errado. Comando inválido' ;
       80 : ErroMsg := 'A palavra "TOTAL" não pode ser enviada para a Impressora' ;
       81 : ErroMsg := 'Papel próximo do fim.' ;
       86 : ErroMsg := 'Nao houve nenhum pagamento. Impossível fechar o cupom.';
       87 : ErroMsg := 'Cupom já totalizado' ;
       88 : ErroMsg := 'Não é possível cancelar VENDA, pois ainda não foi iniciada.' ;
       89 : ErroMsg := 'Comando incompleto' ;
       90 : ErroMsg := 'Cupom ainda está aberto. Termine a venda primeiro, ou cancele-a.' ;
       91 : ErroMsg := 'Impossivel cancelar. Ultimo documento não é um Cupom Fiscal.' ;
       92, 94, 112 : ErroMsg := 'Aliquota nao programada  Erro:('+IntToStr(Erro)+')' ;
       99 : ErroMsg := 'Impressora em Intervenção Técnica' ;
      100 : ErroMsg := 'Impressora em periodo de Venda. '+
                       'É necessário a impressão de uma REDUÇAO Z' ;
      101 : ErroMsg := 'Comando permitido somente sob Intervenção Técnica' ;
      102 : ErroMsg := 'Desconto Inválido, Item já possui desconto' ;
      103 : ErroMsg := 'Número de linhas comerciais excedido (8)' ;
      107 : ErroMsg := 'Comando inexistente' ;
      108 : ErroMsg := 'Não houve fechamento do dia Efetue uma Redução Z' ;
      110, 133 : ErroMsg := 'Data/Hora Inválida ('+IntToStr(Erro)+')' ;
      111, 116, 147 : ErroMsg := 'Somente é permitida a alteração da hora '+
                       'para mais ou menos uma hora, uma vez por dia '+
                       'e após a Redução Z. (Horario de verão) '+
                       'Erro: ('+IntToStr(Erro)+')' ;
      113 : ErroMsg := 'Fechamento do dia anterior não foi realizado. '+
                       'Efetue uma REDUÇÃO Z para imprimir as Vendas de HOJE.' ;
      114 : ErroMsg := 'Dia já foi fechado (REDUÇÃO Z). Impossível vender hoje'+
                       ' Impressora bloqueada até amanha' ;
      115 : ErroMsg := 'Dia já foi fechado REDUÇÃO Z já foi efetuada' ;
      126 : ErroMsg := 'Comando fora da sequencia' ;
      132 : ErroMsg := 'Houve uma queda de energia' ;
      136, 141 : ErroMsg := 'Parâmetros de Venda de Produtos errados ('+
                            IntToStr(Erro)+')' ;
      137 : ErroMsg := 'Valor da Venda muito alto' ;
      138, 148 : ErroMsg := 'Relógio fora de operação ('+IntToStr(Erro)+
                            '). Será necessária uma Intervenção Técnica.' ;
      144 : ErroMsg := 'Cupom Vinculado Pendente. Utilize a rotina '+
                       'CorrigeEstadoErro' ;
      146 : ErroMsg := 'Não há Cupom Vinculado pendente' ;
      149 : ErroMsg := 'Nenhuma forma de pagamento foi programada' ;
     else
        ErroMsg := 'Erro na impressora '+fpModeloStr+' ('+IntToStr(ERRO)+')' ;
     end ;

     { NAO faz verificaça de tempo de msg para o erro "Pouco Papel", pois:
        Schalter não permite Abrir Cupom ou Vender Itens se o Erro
        81 - Pouco Papel estiver ocorrendo, por isso não podemos ignorar o erro,
        devemos gerar exceção }
     if ErroMsg <> '' then
      begin
        ErroMsg := ACBrStr('Erro retornado pela Impressora: '+fpModeloStr+#10+#10+
                   ErroMsg );

        if (Erro = 81) then
           DoOnErrorSemPapel
        else
           raise EACBrECFSemResposta.create(ErroMsg) ;
      end
     else
        Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }

  finally
     BytesResp := 1 ;
  end ;

end;

function TACBrECFSchalter.PreparaCmd(cmd: AnsiString): AnsiString;
Var iSoma, POS : Integer ;
begin

  Result := cmd ;
  if cmd = '' then exit ;

  { comando 110 - Imp.Linha nao tem 'f' e nao tem CHKSUM }
  if ord(cmd[1]) = 110 then
  begin
     Result := #27 + cmd + #13 ;
     exit ;
  end ;

  cmd   := #27 + 'f' + cmd ;
  POS   := 1 ;
  iSoma := 0 ;

  while POS <= Length(cmd) do
  begin
     iSoma := iSoma + ord( cmd[POS] ) ;

     while iSoma > 256 do
        iSoma := iSoma - 256 ;

     POS := POS + 1 ;
  end ;
  iSoma := 256 - iSoma ;

  Result := cmd + AnsiChar( chr( iSoma ) );
end;

function TACBrECFSchalter.VerificaFimLeitura(var Retorno : AnsiString ;
   var TempoLimite : TDateTime) : Boolean ;
begin
  { Lê até atingir todos os Bytes esperados (BytesResp) e ECF entra EmLinha}
  { BytesResp é necessário, pois a Schalter nao usa um Sufixo padrão no fim
    da resposta da Impressora. }

  { Comandos com Tamanho de Resposta Variavel. Na Leitura da Memoria Fiscal,
    aguarda até chegar #4 }
  if BytesResp < 0 then
   begin
     Result := (Length( Retorno ) > 0)  and (pos(#4,RightStr(Retorno,3))>0) and
               (fpDevice.Serial.WaitingDataEx <= 0) ;
     if not Result then
        Sleep( 100 ) ;
   end 
  else
   begin
     Result := Length( Retorno ) >= BytesResp ;

     { Verifica se impressora retornou erro e NAO o Retorno esperado }
     if (BytesResp <> 1) and (Length( Retorno ) = 1) and
        (copy(Retorno,1,1) <> #27) then
     begin
        Result := true ;
        AguardaImpressao := False ;
     end ;
   end ;
end;

function TACBrECFSchalter.VerificaFimImpressao(var TempoLimite: TDateTime): Boolean;
Var Cmd , RetCmd : AnsiString ;
    I : Integer ;
    DT : TDateTime ;
begin
  { Essa função só é chamada se AguardaImpressao = True,
    Como essa função é executada dentro da "LeResposta", que por sua vez foi
    chamada por "EnviaComando", não podemos usar o método "EnviaComando" (ou
    teriamos uma chamada recursiva infinita), por isso o Loop abaixo envia o
    comando #223 diretamente para a Serial, e aguarda por 1 segundo a resposta...
     Se a Schalter consegir responder, significa que a Impressão Terminou }
  Result := false ;
  DT     := 0 ;
  if not EmLinha() then
     Sleep(100)
  else
     try
        RetCmd := '' ;
        I      := 0 ;
        BytesResp := 8 ;
        Cmd    := PreparaCmd( #223 + '  ') ;
        try
           fpDevice.Serial.Purge ;  { Limpa buffer de Entrada e Saida }
           fpDevice.EnviaString( Cmd );   { Pede Status }
           Sleep(100) ;
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
     finally
        BytesResp := 1 ;
     end ;
end;

function TACBrECFSchalter.GetDataHora: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
begin
  BytesResp := 73 ;
  RetCmd := EnviaComando( #224 + '  ' ) ;
  OldShortDateFormat := ShortDateFormat ;
  try
     ShortDateFormat := 'dd/mm/yy' ;
     result := StrToDate(copy(RetCmd,23,2)+ DateSeparator +
                         copy(RetCmd,26,2)+ DateSeparator +
                         copy(RetCmd,29,2)) ;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;
  result := RecodeHour(  result,StrToInt(copy(RetCmd,17,2))) ;
  result := RecodeMinute(result,StrToInt(copy(RetCmd,19,2))) ;
  result := RecodeSecond(result,StrToInt(copy(RetCmd,21,2))) ;
end;

function TACBrECFSchalter.GetNumCupom: String;
Var RetCmd : AnsiString ;
    Num : Integer ;
    ForaDeCupom : Boolean ;
    Ajuste : Integer ;
    Tentativas : Integer ;
begin
  Tentativas := 0 ;
  Result     := '      ' ;
  Ajuste     := 0 ;
  while Tentativas < 3 do
  begin
     BytesResp := 73 ;
     try
        RetCmd := EnviaComando( #224 + '  ' ) ;
        break ;
     except
        if Tentativas < 3 then
           Inc( Tentativas )
        else
           raise ;
     end ;
  end ;

  Num         := StrToInt( copy(RetCmd,9,6) ) ;
  ForaDeCupom := (RetCmd[8] = '2') ;

  If ForaDeCupom then
     if (Estado <> estVenda) then
        Ajuste := 1 ;

  Result := IntToStrZero( Num - Ajuste, 6 ) ;
end;

function TACBrECFSchalter.GetNumECF: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumECF) = '' then
  begin
     BytesResp := 73 ;
     RetCmd    := EnviaComando( #224 + '  ' ) ;
     fsNumECF  := copy(RetCmd,4,4) ;
  end ;

  Result := fsNumECF ;
end;

function TACBrECFSchalter.GetNumSerie: String;
Var RetCmd : AnsiString ;
begin
  BytesResp  := 53 ;
  RetCmd     := EnviaComando( #228 + '  ') ;
  Result     := trim(copy(RetCmd,36,10)) ;
end;

function TACBrECFSchalter.GetNumVersao: String ;
Var RetCmd : AnsiString ;
begin
  if fsNumVersao = '' then
  begin
     try
        BytesResp   := 53 ;
        RetCmd      := EnviaComando( #228 + '  ' ) ;
        if copy(RetCmd,1,3) = #27 + 'f' + #228 then
        begin
           fsNumVersao := copy(RetCmd,31,4) ;
        end ;
     except
        fsNumVersao := '2.04'
     end ;

     fsNumVersao := StringReplace(fsNumVersao,',',DecimalSeparator,[rfReplaceAll]) ;
     fsNumVersao := StringReplace(fsNumVersao,'.',DecimalSeparator,[rfReplaceAll]) ;
  end ;

  Result := fsNumVersao ;
end;

function TACBrECFSchalter.GetTotalPago: Double;
begin
  Result := fsTotalPago ;
end;

function TACBrECFSchalter.GetSubTotal: Double;
Var RetCmd : AnsiString ;
begin
  BytesResp := 73 ;
  RetCmd    := copy( EnviaComando( #224 + '  ' ),31,21) ;
  RetCmd    := RemoveString('.',RetCmd) ;

  Result    := StringToFloatDef( RetCmd, 0 ) ;
end;


function TACBrECFSchalter.GetEstado: TACBrECFEstado;
Var RetCmd : AnsiString ;
    EstInt : Integer ;
begin
  if (not fpAtivo) then
     fpEstado := estNaoInicializada
     
  else if fsEmPagamento then
     fpEstado := estPagamento
     
  else
   begin
     BytesResp := 8 ;
     RetCmd    := EnviaComando( #223 + '  ' ) ;
     EstInt    := ord( RetCmd[6] ) ;

     case EstInt of
        99     : fpEstado := estDesconhecido ;
       115     : fpEstado := estBloqueada ;
       113     : fpEstado := estRequerZ ;
       122     : fpEstado := estRelatorio ;
       123,124 : fpEstado := estPagamento ;
        65, 90 : fpEstado := estVenda ;
     else ;
        fpEstado := estLivre ;
     end;

     if fpEstado = estVenda then
     begin
        BytesResp := 73 ;
        RetCmd := EnviaComando( #224 + '  ' ) ;
        if (RetCmd[8] = '0') then  
           fpEstado := estNaoFiscal ;
     end ;
   end ;

  Result := fpEstado ;
end;

function TACBrECFSchalter.GetGavetaAberta: Boolean;
Var RetCmd : AnsiString ;
begin
  BytesResp := 8 ;
  RetCmd    := EnviaComando( #223 + '  ' ) ;
  Result    := TestBit(ord(RetCmd[7]),3) ;
end;

function TACBrECFSchalter.GetPoucoPapel: Boolean;
Var RetCmd : AnsiString ;
begin
  BytesResp := 8 ;
  RetCmd    := EnviaComando( #223 + '  ' ) ;
  Result    := TestBit(ord(RetCmd[7]),1) ;
end;

function TACBrECFSchalter.GetArredonda: Boolean;
begin
  Result := true ;  { ACBrECF sempre tenta arredondar na Schalter }
end;

procedure TACBrECFSchalter.LeituraX ;
begin
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #20 + PadRight(Operador,8), 35) ;
  PulaLinhas ;
end;

procedure TACBrECFSchalter.LeituraXSerial(Linhas: TStringList);
begin
  // OBS: LeituraX pela Serial na Schalter não tem o mesmo formato da Leitura Impressa // 
  Linhas.Clear ;
  Linhas.Text := DadosLeituraX ;
end;

procedure TACBrECFSchalter.AbreGaveta ;
begin
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #250 + '  ' ) ;
  sleep(100) ;
end;

procedure TACBrECFSchalter.ReducaoZ(DataHora : TDateTime) ;
begin
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #19 + PadRight(Operador,8), 45) ;   { Schalter NAO usa DataHora }
  PulaLinhas ;

  fsDadosLeituraX := '' ;
  ZeraVinculados ;
end;

function TACBrECFSchalter.GetHorarioVerao: Boolean;
begin
  { Nao encontrei Flag de Horário de Verao na Schalter }
  Result := False ;
end;

procedure TACBrECFSchalter.MudaHorarioVerao ;
begin
  MudaHorarioVerao( True ) ;  // True nunca será usado, mas é necessário
end;

procedure TACBrECFSchalter.MudaHorarioVerao(EHorarioVerao: Boolean);
Var Hora : String ;
begin
{ A Schalter não possui um Flag para determinar se está em Horário de Verão,
  portanto apenas tenta programar a hora atual do Micro no ECF }

  Hora := TimeToStr( now ) ;             //  12345678
  BytesResp := 1 ;                       //  HH:MM:SS
  EnviaComando( #22 + copy(Hora,7,2) + copy(Hora,4,2) + copy(Hora,1,2) +
                       ' ' + FormatDateTime('ddmmyy',now) )
end;


procedure TACBrECFSchalter.AbreCupom ;
var TamDoc   : Integer ;
    CPF_CNPJ : String ;
    OldTimeOut : Integer ;
    Cupom : AnsiString ;
    Est : TACBrECFEstado ;
begin
  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }

(*  { Efetuando carga dos CNF's pois Nao permite carregar depois que abrir cupom }
  if not Assigned( fpComprovantesNaoFiscais ) then
     CarregaComprovantesNaoFiscais ;
  Removido a pedido dos usuários por deixar a abertura do cupom muito lenta *)

  CPF_CNPJ := Trim(Consumidor.Documento) ;
  TamDoc := Length(CPF_CNPJ) ;

  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #01 + ByteCabecalho ,10) ;

  fsTotalPago      := 0 ;
  fsEmPagamento    := false ;
  AguardaImpressao := True ;

  if ( StrToFloat( NumVersao ) > 3 ) then
   begin
     BytesResp  := 1 ;
     OldTimeOut := TimeOut ;
     TimeOut    := 1 ;
     try
        try
           { O comando #05 NUNCA envia a resposta (apesar de funcionar) } 
           if TamDoc > 11 then    { É CNPJ ? }
              EnviaComando( #05 + '  ' + PadRight(CPF_CNPJ,18) + StringOfChar(' ',11))
           else if TamDoc > 0 then  { Enviou algum Documento ? }
              EnviaComando( #05 + '  ' + StringOfChar(' ',18) + PadRight(CPF_CNPJ,11));
        except
          { O comando acima é problemático... o ECF para de responder os próximos comandos
            Vamos tentar ler COO e Estado até que ele retorne valores válidos }
          repeat
            Cupom := '' ;
            Est   := estDesconhecido ;
            
            try
               Cupom := NumCupom ;
               Est   := Estado ;
            except
               sleep(200) ;
            end ;
          until (Cupom <> '') and (Est <> estDesconhecido) ;

        end ;
     finally
        TimeOut := OldTimeOut ;
     end ;

     Consumidor.Enviado := (CPF_CNPJ <> '') and
                           (Trim(Consumidor.Nome)+Trim(Consumidor.Endereco)='');
   end
  else
   begin
    if ( StrToFloat( NumVersao ) > 2.04 ) then
     begin
       EnviaComando( #110 + PadRight('Item  Codigo      Descricao',Colunas) ) ;
       EnviaComando( #110 + PadRight('                    Qtd            Preco',
          Colunas) ) ;
     end ;
   end ;

  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.CancelaCupom(NumCOOCancelar: Integer);
Var EstImp : TACBrECFEstado ;
    SubTot, TotPag : Double ;
    FPG : TACBrECFFormaPagamento ;
begin

  BytesResp := 1 ;
  EstImp    := Estado ;  { para ler estado da impressora apenas 1 vez }

  if EstImp = estVenda then
   begin
     { Cancelamento de Documento Atual }
     AguardaImpressao := True ;
     EnviaComando( #07 + '  ' + PadRight(Operador,8) + '    ', 15)
   end 
  else
   begin
     if EstImp = estPagamento then
     begin
        SubTot := Subtotal ;  { para ler SubTotal da impressora apenas 1 vez }
        TotPag := TotalPago ;

        if (SubTot > 0) and (TotPag < SubTot) then
        begin
           try
              FPG := AchaFPGDescricao('DINHEIRO') ;
              if FPG = nil then
                 FPG := FormasPagamento[0] ;

              EfetuaPagamento(FPG.Indice, SubTot-TotPag,'CUPOM SERA CANCELADO');
           except
           end ;
        end ;

        { Fechando Cupom }
        AguardaImpressao := True ;
        EnviaComando( #06 + '  ' + PadRight(Operador,8), 15) ;
     end ;

     { Cancelamento de Documento Anterior }
     AguardaImpressao := True ;
     EnviaComando( #199 + '  ' + PadRight(Operador,8) ,15) ;
   end ;

  fsDadosLeituraX := '' ;
  fsEmPagamento := false ;
  fsTotalPago   := 0 ;
  PulaLinhas ;
  FechaRelatorio ;   { Fecha relatorio se ficou algum aberto (só por garantia)}
end;

procedure TACBrECFSchalter.CancelaItemVendido(NumItem: Integer);
begin
  BytesResp := 1 ;
  EnviaComando(#212 + IntToStrZero( NumItem ,4) + StringOfChar(' ',30) ) ;
  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
Var FlagObs : AnsiChar ;
begin
  if StrToFloat( NumVersao ) > 3 then
    Observacao := LeftStr( Observacao,48 )
  else
    // Nas impressoras SPrint a observação é usada para
    // definir forma de pagamento
    Observacao := LeftStr( Observacao,10 ) ;

  If Observacao = '' then
     FlagObs := chr(0)
  else
     FlagObs := chr(1) ;

  BytesResp  := 1 ;

  if StrToFloat( NumVersao ) > 3 then
   begin
     AguardaImpressao := True ;
     EnviaComando( #213 + #0 + FlagObs + #0 + StringOfChar(' ',10) +
                   CodFormaPagto + IntToStrZero( Round( Valor * 100),10), 5 )
   end
  else
   begin
     AguardaImpressao := True ;
     EnviaComando( #213 + #0 + #0 + #0 + StringOfChar(' ',10) + '  ' +
                   IntToStrZero( Round( Valor * 100),10), 5 );
   end ;

  fsTotalPago   := fsTotalPago + Valor ;
  fsEmPagamento := true ;
  fsDadosLeituraX := '' ;

  if ( Observacao <> '' ) and ( StrToFloat( NumVersao ) > 3 ) then
  begin
     AguardaImpressao := True ;
     EnviaComando( #110 + PadRight( Observacao, Colunas) ) ;
  end ;
end;

procedure TACBrECFSchalter.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
begin
  Observacao := AjustaLinhas(Observacao, Colunas,  8 ) ;

  { Imprimindo as Observacoes }
  if Observacao <> '' then
  begin
     BytesResp  := 1 ;
     ImprimirLinhaALinha( Observacao, #110);
  end ;

  { Fechando o Documento }
  AguardaImpressao := True ;
  EnviaComando( #06 + '  ' + PadRight(Operador,8) ,10) ;

  fsTotalPago   := 0 ;
  fsEmPagamento := false ;
  fsDadosLeituraX := '' ;
  PulaLinhas ;
end;

procedure TACBrECFSchalter.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString);
begin
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #209 + ' ' ) ;

  if DescontoAcrescimo < 0 then
   begin
     AguardaImpressao := True ;
     EnviaComando( #217 + chr(0) + #0 + PadRight('Desconto:',25) +
        IntToStrZero( Round(abs(DescontoAcrescimo) * 100),10) ) ;
   end
  else if DescontoAcrescimo > 0 then
   begin
     AguardaImpressao := True ;
     EnviaComando( #217 + chr(1) + #0 + PadRight('Acrescimo:',25) +
        IntToStrZero( Round(DescontoAcrescimo * 100),10) ) ;
   end ;

  fsTotalPago   := 0 ;
  fsDadosLeituraX := '' ;
  fsEmPagamento := true ;
end;

procedure TACBrECFSchalter.VendeItem(Codigo, Descricao : String ;
   AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
   ValorDescontoAcrescimo : Double ; Unidade : String ;
   TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
   CodDepartamento : Integer) ;
Var QtdStr, ValorStr, DescontoStr, TotalStr : String ;
    Decimais : Integer ;
begin
  if Qtd > 99999 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'Quantidade deve ser inferior a 99999.'));

  { Schalter não permite Acrescimo por Item }
  if (ValorDescontoAcrescimo > 0) and (DescontoAcrescimo = 'A') then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'ECF '+fpModeloStr+' não permite Acréscimo por Item'));

(*  { SCHALTER TRUNCA quando PrecoUnit tem 3 ou mais decimais, com 2 decimais ela
   arredonda. Caso o PrecoUnit tenha 3 decimais, aumentamos a QTD até o valor
   TOTAL truncado atingir o Valor Esperado }
  if ArredondaPorQtd then
     if RoundTo(ValorUnitario,-2) <> ValorUnitario then {Tem mais de 2 casas dec ?}
        ArredondarPorQtd( Qtd, ValorUnitario );  { ajusta a Qtd para Arredondar }*)

  if StrToFloat( NumVersao ) > 3 then
   begin
     Codigo      := PadRight(Codigo,13) ;
     Descricao   := PadRight(Descricao,62) ;
     Unidade     := PadRight(Unidade,2) ;
     QtdStr      := FloatToStr(Qtd) ;
     if Length(QtdStr) > 7 then
        QtdStr := FloatToStr(RoundTo(Qtd,-(7-pos(DecimalSeparator,QtdStr)))) ;
     QtdStr   := PadLeft(StringReplace(QtdStr,DecimalSeparator,',',[rfReplaceAll]),7,'0') ;
     ValorStr := FloatToStr(ValorUnitario) ;

     Decimais := pos(DecimalSeparator,ValorStr) ;
     if Decimais = 0 then
        Decimais := 2
     else
      begin
        Decimais := Length(ValorStr) - Decimais ;
        If Decimais > 2 then
           Decimais := 3
        else
           Decimais := 2 ;
      end ;

     BytesResp := 1 ;
     EnviaComando( #208 + '    ' + Codigo + ' ' + Descricao + QtdStr + 'x' +
                   Unidade + StringOfChar(' ',9) + IntToStr(Decimais) +
                   IntToStrZero( Round(ValorUnitario * IntPower(10,Decimais)),9) +
                   AliquotaECF ) ;
   end
  else
   begin
     Codigo    := PadRight(Codigo,10) ;
     Descricao := Copy(PadRight(Descricao,48),1,48) ;
     Unidade   := PadRight(Unidade,2) ;
     QtdStr    := Format('%9s',[FormatFloat('##,##0.00', Qtd)]) ;
     ValorStr  := Format('%10s',[FormatFloat('###,##0.00', ValorUnitario)]);
     TotalStr  := Format('%.10d', [Round(Qtd * ValorUnitario * 100)]);

     BytesResp := 1 ;
     EnviaComando( #214 + Codigo + Descricao + QtdStr + ValorStr + TotalStr + AliquotaECF);
   end;

  fsTotalPago   := 0 ;
  fsEmPagamento := false ;
  fsDadosLeituraX := '' ;

  if ValorDescontoAcrescimo > 0 then
  begin
     { Schalter tem apenas desconto por Valor }
     if TipoDescontoAcrescimo = '%' then
        DescontoStr := IntToStrZero( Round(ValorUnitario*Qtd*ValorDescontoAcrescimo), 9)
     else
        DescontoStr := IntToStrZero( Round(ValorDescontoAcrescimo*100), 9) ;

     EnviaComando( #211 + #3 + ' ' + #0 + StringofChar(' ',65) + DescontoStr ) ;
  end ;
end;

procedure TACBrECFSchalter.CarregaAliquotas;
Var RetCmd,ValStr : AnsiString ;
    Aliquota : TACBrECFAliquota ;
    ValAliq, TotAliq : Double ;
    A :Integer ;
begin
  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  try
     for A := 0 to 15 do
     begin
        BytesResp := 42 ;
        RetCmd    := EnviaComando( #225 + IntToStrZero(A,2)) ;
        ValStr    := copy(RetCmd,5,5);
        ValAliq   := StringToFloatDef( ValStr, 0 ) ;
        ValStr    := RemoveString('.',copy(RetCmd,11,16));
        TotAliq   := StringToFloatDef( ValStr,  0 ) ;

        if ValAliq > 0 then
        begin
           Aliquota := TACBrECFAliquota.create ;

           Aliquota.Indice   :=IntToStrZero(A,2) ;
           Aliquota.Aliquota := ValAliq ;
           Aliquota.Total    := TotAliq ;
           if copy(RetCmd,4,1) = 'S' then
              Aliquota.Tipo  := 'S';

           fpAliquotas.Add( Aliquota ) ;
       end ;
     end ;
  except
     fpAliquotas.Free ;
     fpAliquotas := nil ;

     raise ;
  end ;
end;

procedure TACBrECFSchalter.LerTotaisAliquota;
begin
  CarregaAliquotas ;
end;

procedure TACBrECFSchalter.ProgramaAliquota(Aliquota: Double; Tipo: Char;
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

  ProxIndice := StrToIntDef(Posicao,-1) ;
  if (ProxIndice < 0) or (ProxIndice > 15) then { Indice passado é válido ? }
  begin
     For ProxIndice := 0 to 16 do  { Procurando Lacuna }
     begin
        if AchaICMSIndice(IntToStrZero(ProxIndice,2)) = nil then
           break ;
     end ;
  end ;

  if ProxIndice > 15 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Aliquotas'));

  BytesResp := 1 ;
  EnviaComando( #34 + IntToStrZero(ProxIndice,2) + Tipo + ValStr ) ;

  { Adcionanodo nova Aliquota no ObjectList }
  Aliq := TACBrECFAliquota.create ;
  Aliq.Indice   := IntToStrZero(ProxIndice,2) ;
  Aliq.Aliquota := Aliquota ;
  Aliq.Tipo     := Tipo ;
  fpAliquotas.Add( Aliq ) ;
end;

function TACBrECFSchalter.AchaICMSAliquota( var AliquotaICMS: String):
  TACBrECFAliquota;
  Var AliquotaStr : String ;
begin
  AliquotaStr := '' ;
  Result      := nil ;
  
  case AliquotaICMS[1] of
    'F' : AliquotaStr := IfThen(StrToFloat( NumVersao ) < 3,'12','16') ;
    'I' : AliquotaStr := IfThen(StrToFloat( NumVersao ) < 3,'13','17') ;
    'N' : AliquotaStr := IfThen(StrToFloat( NumVersao ) < 3,'14','18') ;
    'T' : AliquotaICMS := 'T'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}
  end ;

  if AliquotaStr = '' then
     Result := inherited AchaICMSAliquota( AliquotaICMS )
  else
     AliquotaICMS := AliquotaStr ;
end;



procedure TACBrECFSchalter.CarregaFormasPagamento;  { funçao Lenta +- 3 sec. }
Var RetCmd, Descricao : AnsiString ;
    Cont : Integer ;
    FPagto : TACBrECFFormaPagamento ;
begin
  inherited CarregaFormasPagamento ;       {Inicializa fpFormasPagamentos}

  if StrToFloat( NumVersao ) > 3 then
   begin
     try
        for Cont := 0 to 19 do
        begin
           BytesResp := 48 ;
           RetCmd    := EnviaComando( #226 + IntToStrZero(Cont,2)) ;
           Descricao := Trim(copy(RetCmd,6,20)) ;

           if (RetCmd[4] = 'S') and (Descricao <> '') then
           begin
              FPagto := TACBrECFFormaPagamento.create ;

              FPagto.Indice    := IntToStrZero(Cont,2) ;
              FPagto.Descricao := Descricao ;
              FPagto.PermiteVinculado := (RetCmd[5] = 'S');
              FPagto.Total := StringToFloatDef(
                                 RemoveString( '.',copy(RetCmd,27,20)),0) ;

              fpFormasPagamentos.Add( FPagto ) ;
           end ;
        end ;
     except
        fpFormasPagamentos.Free ;
        fpFormasPagamentos := nil ;

        raise ;
     end ;
   end
  else
   begin
     FPagto := TACBrECFFormaPagamento.create ;
     FPagto.Indice    := '00' ;
     FPagto.Descricao := 'Dinheiro';
     FPagto.PermiteVinculado := False ;

     fpFormasPagamentos.Add( FPagto ) ;
   end;
end;

procedure TACBrECFSchalter.LerTotaisFormaPagamento;
begin
  CarregaFormasPagamento ;
end;

procedure TACBrECFSchalter.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String);
Var ProxIndice : Integer ;
    FPagto : TACBrECFFormaPagamento ;
begin
  Descricao := PadRight(Descricao,20) ;

  CarregaFormasPagamento ;

  ProxIndice := StrToIntDef(Posicao,-1) ;
  if (ProxIndice < 0) or (ProxIndice > 19) then { Indice passado é válido ? }
  begin
     For ProxIndice := 0 to 20 do  { Procurando Lacuna }
     begin
        if AchaFPGIndice(IntToStrZero(ProxIndice,2)) = nil then
           break ;
     end ;
  end ;

  if ProxIndice > 19 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Formas de '+
                            'Pagamento'));

  BytesResp := 1 ;
  EnviaComando( #36 + IntToStrZero(ProxIndice,2) + Descricao ) ;

  { Adcionanodo nova FPG no ObjectList }
  FPagto := TACBrECFFormaPagamento.create ;
  FPagto.Indice    := IntToStrZero(ProxIndice,2) ;
  FPagto.Descricao := Descricao ;
  fpFormasPagamentos.Add( FPagto ) ;

  if PermiteVinculado then
     ProgramaComprovanteNaoFiscal(Descricao,'V'+IntToStrZero(ProxIndice,2));

  FPagto.PermiteVinculado := PermiteVinculado ;
end;

procedure TACBrECFSchalter.CarregaComprovantesNaoFiscais;
Var RetCmd, Descricao : AnsiString ;
    Cont : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  inherited CarregaComprovantesNaoFiscais ;{Inicializa fpComprovantesNaoFiscais}

  try
     for Cont := 0 to 19 do
     begin
        BytesResp := 124 ;
        RetCmd    := EnviaComando( #227 + IntToStrZero(Cont,2)) ;
        Descricao := Trim(copy(RetCmd,5,20)) ;

        if (RetCmd[4] = 'S') and (Descricao <> '') then
        begin
           CNF := TACBrECFComprovanteNaoFiscal.create ;

           CNF.Indice           := IntToStrZero(Cont,2) ;
           CNF.Descricao        := Descricao ;
           CNF.PermiteVinculado := ( copy(RetCmd,30,1) = 'S' ) ;
           CNF.FormaPagamento   := copy(RetCmd,31,2) ;
           CNF.Contador         := StrToIntDef(copy(RetCmd,34,6),0) ;
           CNF.Total            := StringToFloatDef(
                                     RemoveString('.',copy(RetCmd,48,18)),0) ;

           fpComprovantesNaoFiscais.Add( CNF ) ;
        end ;
     end ;
  except
     fpComprovantesNaoFiscais.Free ;
     fpComprovantesNaoFiscais := nil ;

     raise ;
  end ;
end;

procedure TACBrECFSchalter.LerTotaisComprovanteNaoFiscal;
begin
  CarregaComprovantesNaoFiscais ;
end;

procedure TACBrECFSchalter.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String );
Var PV, ProxIndice, IndiceFPG : Integer ;
    FlagD, FlagA, FlagC, FlagP, FlagV : String ;
    FPG : TACBrECFFormaPagamento ;
    DescrFPG : String ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  Tipo      := UpperCase(Tipo) ;
  Descricao := PadRight(Descricao,20) ;
  IndiceFPG := -1 ;
  DescrFPG  := '' ;
  PV        := pos('V',Tipo) ;

  if Tipo = '' then
     Tipo := 'DAC'
  else
   begin
     IndiceFPG := StrToIntDef(Tipo,-1) ;
     if IndiceFPG = -1 then            { Passou apenas o Indice de uma FPG ? }
     begin
        if PV > 0 then
        begin
           IndiceFPG := StrToIntDef(copy(Tipo, PV + 1 ,2) , -1) ;
           if IndiceFPG = -1 then
              raise EACBrECFERRO.Create(ACBrStr('A impressora '+fpModeloStr+' necessita como '+sLineBreak+
                    'parâmetro o Indice de uma Forma de Pagamento para permitir '+sLineBreak+
                    'o Vinculado.'+sLineBreak+sLineBreak+
                    'Experimente passar para o parametro Tipo '+sLineBreak+
                    'o valor "V01" onde 01 é o indice da Forma de Pagamento a '+sLineBreak+
                    'ser usada para o Vinculo.')) ;
        end ;
     end ;
   end ;

  if IndiceFPG >= 0 then
  begin
     Tipo := 'V' ;

     if IndiceFPG <> 99 then  { 99 = Vinculo a Cupom }
     begin
        FPG := AchaFPGIndice(IntToStrZero(IndiceFPG,2)) ;
        if FPG = nil then
           raise EACBrECFERRO.Create(ACBrStr('Forma de Pagamento: '+
                                  IntToStrZero(IndiceFPG,2)+ ' não encontrada.')) ;
        DescrFPG := FPG.Descricao ;
     end ;
  end ;

  if pos('D',Tipo) > 0 then FlagD := 'S' else FlagD := 'N' ;
  if pos('A',Tipo) > 0 then FlagA := 'S' else FlagA := 'N' ;
  if pos('C',Tipo) > 0 then FlagC := 'S' else FlagC := 'N' ;
  if pos('P',Tipo) > 0 then FlagP := 'S' else FlagP := 'N' ;
  if pos('V',Tipo) > 0 then FlagV := 'S' else FlagV := 'N' ;

  CarregaComprovantesNaoFiscais ;
  
  ProxIndice := StrToIntDef(Posicao,-1) ;
  if (ProxIndice < 0) or (ProxIndice > 19) then { Indice passado é válido ? }
  begin
     For ProxIndice := 0 to 20 do  { Procurando Lacuna }
     begin
        if AchaCNFIndice(IntToStrZero(ProxIndice,2)) = nil then
           break ;
     end ;
  end ;

  if ProxIndice > 19 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novos Comprovantes'+
                            ' não Fiscais'));
  {$IFNDEF NOGUI}
    if (IndiceFPG > 0) and (IndiceFPG <> 99) then
    begin
       if MessageDlg( ACBrStr( 'Atenção. Você irá criar um vinculo entre:'+sLineBreak+
                     'Forma de Pagamento: '+DescrFPG+' e o '+sLineBreak+
                     'Comprovante não Fiscal: '+Descricao+sLineBreak+sLineBreak+
                     'Dessa maneira, toda vez que a Forma de Pagamento for '+
                     'utilizada, será OBRIGATÓRIA a emissao de um Comprovante '+
                     'Não Fiscal Vinculado. (Consulte o manual da '+fpModeloStr+
                     ')'+sLineBreak+sLineBreak+
                     'Continua com a operação ?' ) ,
                     {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtConfirmation,mbYesNoCancel,0) <> mrYes then
          raise EACBrECFERRO.create(ACBrStr('Programaçao de Comprovante não Fiscal cancelada'));
    end ;
  {$ENDIF}
  
  BytesResp := 1 ;
  EnviaComando( #35 + IntToStrZero(ProxIndice,2) + Descricao +
                StringOfChar(' ',20) + FlagD + FlagA + FlagC + FlagP + FlagV +
                IntToStrZero(IndiceFPG,2) ) ;

  { Adcionanodo novo CNF no ObjectList }
  CNF := TACBrECFComprovanteNaoFiscal.create ;
  CNF.Indice           := IntToStrZero(ProxIndice,2) ;
  CNF.Descricao        := Descricao ;
  CNF.PermiteVinculado := (FlagV = 'S') ;
  CNF.FormaPagamento   := IntToStrZero(IndiceFPG,2) ;
  fpComprovantesNaoFiscais.Add( CNF ) ;
end;


procedure TACBrECFSchalter.AbreRelatorioGerencial(Indice : Integer) ;
begin
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #16 + PadRight(Operador,8) ,35 ) ;
  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  ImprimirLinhaALinha( Linha, #110 );
end;

procedure TACBrECFSchalter.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
Var FPG : TACBrECFFormaPagamento ;
    CNF : TACBrECFComprovanteNaoFiscal ;
    StrValor : String ;
begin
  FPG := AchaFPGIndice( CodFormaPagto ) ;
  if FPG = nil then
     raise EACBrECFERRO.create( ACBrStr('Forma de Pagamento: '+CodFormaPagto+
                             ' não foi cadastrada.' )) ;

  if CodComprovanteNaoFiscal <> '' then
   begin
     CNF := AchaCNFIndice( CodComprovanteNaoFiscal ) ;
     if CNF = nil then
        raise EACBrECFERRO.create( ACBrStr('Comprovante NÃO Fiscal: '+
                         CodComprovanteNaoFiscal+' não cadastrado.' )) ;
   end
  else
   begin
     CNF := AchaCNFFormaPagamento( FPG.Indice ) ;
     if CNF = nil then
        raise EACBrECFERRO.create( ACBrStr('Não existe nenhum Comprovante NÃO Fiscal '+
                         ' associado a Forma de Pagamento: '+FPG.Indice )) ;
   end ;

  if not CNF.PermiteVinculado then
     raise EACBrECFERRO.create( ACBrStr('O Comprovante não Fiscal: '+CNF.Descricao+
                             ' não permite Cupom NÃO Fiscal Vinculado' )) ;

  COO      := Poem_Zeros( trim(COO) ,6) ;
  StrValor := IntToStrZero( Round(Valor * 100) ,9) ;

  BytesResp := 1 ;
  { Abrindo Cabeçalho }
  AguardaImpressao := True ;
  EnviaComando( #01 + ByteCabecalho,10) ;
  { Informando que será Cupom NAO Fiscal Vinculado }
  EnviaComando( #222 + COO + CNF.Indice + StrValor ,10) ;
  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha ) ; 
end;

procedure TACBrECFSchalter.FechaRelatorio;
Var RetCmd : AnsiString ;
    EstInt : Integer ;
begin
  BytesResp := 8 ;
  AguardaImpressao := True ;
  RetCmd    := EnviaComando( #223 + '  ' ) ;
  EstInt    := ord( RetCmd[6] ) ;

  if (EstInt = 90) or (EstInt = 122) or ( EstInt = 124) then
  begin
     BytesResp := 1 ;
     EnviaComando( #06 + '  ' + PadRight(Operador,8), 10 ) ;
     PulaLinhas ;
  end ;
  
  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.PulaLinhas(NumLinhas: Integer);
begin
  if NumLinhas = 0 then
     NumLinhas := LinhasEntreCupons ;
     
  BytesResp := 1 ;
  EnviaComando( #236 + '1'+IntToStrZero(NumLinhas,2)) ;
end;

procedure TACBrECFSchalter.CorrigeEstadoErro(Reducao: Boolean);
begin
  try
     ZeraVinculados ;
  except
  end ;

  inherited CorrigeEstadoErro(Reducao) ;
end;


procedure TACBrECFSchalter.ZeraVinculados;
Var Pos : Integer ;
    RetCmd, FPG, COO : AnsiString ;
    Valor : Double ;
    Texto : TStringList ; 
begin
  { Na Schalter, se houver mais de 30 Cupons vinculados pendentes a Impressora
    pode ficar travada. Essa rotina irá imprimir e cancelar esses Cupons
    Vinculados pendentes.
    - Esse problema pode ocorrer se você associar um CNF a uma Forma de Pagamento.
    Nesse caso SEMPRE que a Forma de pagamento for utilizada obrigatoriamente
    deverá ser listado um Cupom NAO Fiscal Vinculado. Cuidado: Nunca crie um
    vinculo na forma de Pagamento DINHEIRO }

  if StrToFloat( NumVersao ) <= 3 then exit ;

  Texto := TStringList.Create ;

  try
     Texto.Clear ;
     Texto.Text := '** CANCELANDO VINCULO PENDENTE **' ;

     For POS := 0 to 29 do
     begin
        BytesResp := 67 ;
        RetCmd    := EnviaComando( #231 + IntToStrZero(Pos,2) ) ;

        if RetCmd[4] = 'S' then { Pendencia existente ? ...cancelando }
        begin
           FPG   := copy(RetCmd, 5,2) ;
           COO   := copy(RetCmd,50,6) ;
           Valor := StringToFloatDef( RemoveString('.',copy(RetCmd,8,20)), 0) ;
           if Valor = 0 then
              Valor := StringToFloatDef( RemoveString('.',copy(RetCmd,29,20)), 0 ) ;

           CupomVinculado(COO, FPG ,'', Valor ,Texto);
           CancelaCupom ;
        end ;
     end ;
  finally
     Texto.Free ;
  end ;

  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada : Boolean);
Var Espera : Integer ;
begin
  // Schalter não possui Leitura Simplificada
  Espera := 40 + (ReducaoFinal - ReducaoInicial) ;
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #21 + #2 + IntToStrZero(0,12) +
                     IntToStrZero(ReducaoInicial,4)+
                     IntToStrZero(ReducaoFinal  ,4), Espera ) ;
end;

procedure TACBrECFSchalter.LeituraMemoriaFiscal(DataInicial,
   DataFinal: TDateTime; Simplificada : Boolean);
Var Espera : Integer ;
begin
  // Schalter não possui Leitura Simplificada
  Espera := 40 + DaysBetween(DataInicial,DataFinal) ;
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #21 + #1 + FormatDateTime('ddmmyy',DataInicial)+
                           FormatDateTime('ddmmyy',DataFinal)  +
                           IntToStrZero(0,8),   Espera ) ;
end;

procedure TACBrECFSchalter.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas: TStringList; Simplificada : Boolean);
 Var Espera : Integer ;
     RetCmd : AnsiString ;
begin
  // Schalter não possui Leitura Simplificada
  Espera    := 60 + (ReducaoFinal - ReducaoInicial)  ;
  BytesResp := -1 ;  { Reposta de Tamanho Variavel, Aguarda por #4  }
  Linhas.Clear ;
  RetCmd := EnviaComando( #21 + #4 + IntToStrZero(0,12) +
                     IntToStrZero(ReducaoInicial,4)+
                     IntToStrZero(ReducaoFinal  ,4), Espera ) ;
  RetCmd := StringReplace(RetCmd,#10,sLineBreak,[rfReplaceAll]) ;
  Linhas.Text := RetCmd
end;

procedure TACBrECFSchalter.LeituraMemoriaFiscalSerial(DataInicial,
   DataFinal: TDateTime; Linhas: TStringList; Simplificada : Boolean);
 Var Espera : Integer ;
     RetCmd : AnsiString ;
begin
  // Schalter não possui Leitura Simplificada
  Espera    := 60 + DaysBetween(DataInicial,DataFinal) ;
  BytesResp := -1 ;  { Reposta de Tamanho Variavel, Aguarda por #4  }
  Linhas.Clear ;
  RetCmd := EnviaComando( #21 + #3 +
                           FormatDateTime('ddmmyy',DataInicial)+
                           FormatDateTime('ddmmyy',DataFinal)  +
                           IntToStrZero(0,8),   Espera ) ;
  RetCmd := StringReplace(RetCmd,#10,sLineBreak,[rfReplaceAll]) ;
  Linhas.Text := RetCmd
end;

function TACBrECFSchalter.GetNumUltimoItem: Integer;
Var RetCmd : AnsiString;
begin
  Result := 0;
  if GetEstado = estVenda then
  begin
    RetCmd := EnviaComando( #232 + '  ' );
    Result := StrToInt( copy( RetCmd,6,3 ) );
  end;
end;

function TACBrECFSchalter.GetGrandeTotal: Double;
Var RetCmd : AnsiString;
begin
  BytesResp := 73;
  RetCmd    := copy( EnviaComando( #224 + '  ' ),55,18);
  RetCmd    := RemoveString('.',RetCmd);

  Result    := StringToFloatDef( RetCmd, 0 );
end;

procedure TACBrECFSchalter.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
  Var TamDoc : Integer ;
begin
  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }

  BytesResp := 1 ;
  { Abrindo Cabeçalho }
  AguardaImpressao := True ;
  EnviaComando( #01 + ByteCabecalho,10) ;
  { Informando que será Cupom NAO Fiscal NAO Vinculado }
  AguardaImpressao := True ;
  EnviaComando( #221 + ' ',5) ;

  CPF_CNPJ := Trim(CPF_CNPJ) ;
  TamDoc := Length(CPF_CNPJ) ;

  if TamDoc > 0 then
  begin
     fsTotalPago      := 0 ;
     fsEmPagamento    := false ;
     AguardaImpressao := True ;

      BytesResp := 1 ;
      if TamDoc > 11 then    { É CNPJ ? }
         EnviaComando( #05 + '  ' + PadRight(CPF_CNPJ,18) + StringOfChar(' ',11), 5)
      else if TamDoc > 0 then  { Enviou algum Documento ? }
         EnviaComando( #05 + '  ' + StringOfChar(' ',18) + PadRight(CPF_CNPJ,11), 5);
  end ;
  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString);
begin
  BytesResp := 1 ;
  AguardaImpressao := True ;
  EnviaComando( #214 + PadRight(Obs,78) +
                       IntToStrZero(Round( Valor * 100), 9) + CodCNF ,5) ;
  fsDadosLeituraX := '' ;
end;

procedure TACBrECFSchalter.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
   MensagemRodape: AnsiString);
begin
 { Comando não existente na Schalter }
end;

function TACBrECFSchalter.GetCNPJ: String;
begin
  Result := copy(DadosUsuario,161,18) ;
end;

function TACBrECFSchalter.GetIE: String;
begin
  Result := copy(DadosUsuario,180,15) ;
end;

function TACBrECFSchalter.GetNumLoja: String;
begin
  Result := copy(DadosUsuario,151,4) ;
end;

function TACBrECFSchalter.GetDadosUsuario: AnsiString;
  Var RetCmd : AnsiString ;
      I : Integer ;
begin
  if fsDadosUsuario = '' then
  begin
     BytesResp := 8 ;
     RetCmd := EnviaComando( #223 + '  ' ) ;
     I := ord( RetCmd[4] ) ;  // Numero do usuário atual

     { Infelizmente é EXTREMAMENTE LENTO ler os dados do usuário atual do ECF }

     BytesResp := 268 ;
     RetCmd := EnviaComando( #229 + IntToStrZero(I,2), 60 ) ;
     fsDadosUsuario := RetCmd  ;
  end ;

  Result := fsDadosUsuario ;
end;

function TACBrECFSchalter.GetDadosLeituraX: AnsiString;
  Var RetCmd : AnsiString ;
begin
  if fsDadosLeituraX = '' then
  begin
     BytesResp := 748 ;
     RetCmd := EnviaComando( #18 + ' ', 5 ) ;

     fsDadosLeituraX := RetCmd  ;
  end ;
{  CRZ-DD/MM/AACRO-COOINI            |---CANCELAMENTOS---||-----DESCONTOS-----||----ACRESCIMOS-----||----VENDA BRUTA----||--TOT.SUBSTIT.--||---TOT.ISENTO---||--TOT.N.ICIDEN--|
efc051717/08/070012000022000031000002000000000000000000,00000000000000000000,00000000000000000000,02000000000000000000,23000000000000000,03000000000000000,01000000000000000,0212,00000000000000000,05000000000000000,0118,00000000000000000,05000000000000000,0102,16000000000000000,07000000000000000,00  ,  000000000000000,00000000000000000,00  ,  000000000000000,00000000000000000,00  ,  000000000000000,00000000000000000,00  ,  000000000000000,00000000000000000,00  ,  000000000000000,00000000000000000,00  ,  000000000000000,00000000000000000,00  ,  000000000000000,00000000000000000,00000001000001000000000000000000000,00000000000000000,00000000000000000,00000000000000000,00000000000000000,00000000000000000,00000000000000000,00  0: 4: 0  6:52:41'#$19
}
  Result := fsDadosLeituraX ;
end;

function TACBrECFSchalter.GetNumCRO: String;
begin
  Result := copy( DadosLeituraX,16,4) ;
end;

function TACBrECFSchalter.GetNumCRZ: String;
begin
  try
     Result := IntToStrZero( StrToInt(copy(DadosLeituraX,4,4))-1,4) ;
  except
     Result := '0000' ;
  end ;
end;

function TACBrECFSchalter.GetDataMovimento: TDateTime;
 Var RetCmd : AnsiString ;
     OldShortDateFormat : String ;
begin
  RetCmd := copy( DadosLeituraX,8,8 ) ;

  try
     OldShortDateFormat := ShortDateFormat ;
     try
        ShortDateFormat := 'dd/mm/yy' ;
        Result := StringToDateTime( RetCmd ) ;
     finally
        ShortDateFormat := OldShortDateFormat ;
     end ;
  except
     Result := DateOf( DataHora ) ;
  end ;
end;

function TACBrECFSchalter.GetVendaBruta: Double;
  Var S : AnsiString ;
begin
   S := copy(DadosLeituraX,101,21) ;
   S := RemoveString('.',S) ;
   Result := StringToFloatDef(S,0) ;
end;

function TACBrECFSchalter.GetTotalAcrescimos: Double;
  Var S : AnsiString ;
begin
   S := copy(DadosLeituraX,80,21) ;
   S := RemoveString('.',S) ;
   Result := StringToFloatDef(S,0) ;
end;

function TACBrECFSchalter.GetTotalCancelamentos: Double;
  Var S : AnsiString ;
begin
   S := copy(DadosLeituraX,38,21) ;
   S := RemoveString('.',S) ;
   Result := StringToFloatDef(S,0) ;
end;

function TACBrECFSchalter.GetTotalDescontos: Double;
  Var S : AnsiString ;
begin
   S := copy(DadosLeituraX,59,21) ;
   S := RemoveString('.',S) ;
   Result := StringToFloatDef(S,0) ;
end;

function TACBrECFSchalter.GetNumCOOInicial: String;
begin
  Result := copy(DadosLeituraX,20,6) ;
end;

function TACBrECFSchalter.GetTotalIsencao: Double;
  Var S : AnsiString ;
begin
   S := copy(DadosLeituraX,140,18) ;
   S := RemoveString('.',S) ;
   Result := StringToFloatDef(S,0) ;
end;

function TACBrECFSchalter.GetTotalNaoTributado: Double;
  Var S : AnsiString ;
begin
   S := copy(DadosLeituraX,158,18) ;
   S := RemoveString('.',S) ;
   Result := StringToFloatDef(S,0) ;
end;

function TACBrECFSchalter.GetTotalSubstituicaoTributaria: Double;
  Var S : AnsiString ;
begin
   S := copy(DadosLeituraX,122,18) ;
   S := RemoveString('.',S) ;
   Result := StringToFloatDef(S,0) ;
end;

end.

