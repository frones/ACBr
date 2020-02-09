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
|*             O B S E R V A C O E S   S O B R E   A   M E C A F
|* - A Mecaf produz ECF's OEM para varios outros fabricantes, como por exemplo:
|*   Elgin, Digiarte, Zanthus, Acr, Aoki, Chronos, Promcomp, TrendsSTS, Unigraph.
|*   ( Geralmente existe o Logotipo da MECAF no corpo do equipamento )
|*   Entretanto não há garantia de plena compatibilidade entre os diferentes
|*   equipamentos, se eles usarem protocolos diferentes. (Favor reportar BUGS)
|* - O Procotolo utilizado para esse Modulo é o padrão de fábrica da MECAF
|*   STX/ETX. Se o protocolo estiver diferente não funcionará, é possivel mudar
|*   o protocolo usando o "MODO MENU" da impressora.
******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 13/09/2004:  Daniel Simoes de Almeida
|*   Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
|* 23/05/2005:  Daniel Simoes de Almeida
|*   Corrigido BUG em FechaCupom. Linhas de Observação eram impressas com
|*   alinhamento errado.
|*   Corrigido BUG em Estado. No incio do dia Estado sempre constava como
|*   estBloqueda
|*   -  Bugs reportados por Licerio Jose Rodrigues Neto
|* 16/06/2005:  Daniel Simoes de Almeida e Licerio Jose Rodrigues Neto
|*   - Adaptado para suporte da Versao FCP201 da MECAF (antiga)
|* 23/05/2005:  Daniel Simoes de Almeida
|*   - Corrigido BUG em VendeItem... para a MECAF imprimir Item em 2 linhas (e
|*     e nao 3 linhas) ela deve restringir a descriçao em 20 caracteres
|*     - Bug Reportado por Rildecy de Queiroz Borba
|* 01/11/2005: Gabriel Fernandes - Duel Informática
|*   - Compatibilizado com a vesão 301
|*   - Corrigido BUG. Quando a redução z ficava pendente de um dia para o outro
|*     o ECF ficava em estDesconhecido.
|* 08/12/2005:  Daniel Simoes de Almeida
|*  - Diminuido tempo de alguns Sleeps de 100 para 10 a fim de agilizar a
|*    comunicaçao com o ECF (experimental)
|* 27/04/2006:  Daniel Simoes de Almeida e Valmor Florez
|*   - Melhorado o suporte a Versao FCP201/301 da MECAF (antiga)
|* 29/06/2006:  Daniel Simoes de Almeida
|*   - Métodos que usam campo de Texto livre modificados para filtrar os
|*     caracteres acentuados
|* 04/04/2007:  Daniel Simoes de Almeida
|*  - Implementados métodos de Cupom Não Fiscal
|* 20/08/2007:  Daniel Simoes de Almeida
|*  - Adicionada as propriedades: CNPJ, IE, DataMovimento, NumCOOInicial,
|*    NumCRZ, VendaBruta, GrandeTotal, TotalDescontos, TotalAcrescimos,
|*    TotalCancelamentos, TotalIsencao, TotalNaoTributado,
|*    TotalSubstituicaoTributaria,
|*  - Adicionado os métodos: LerTotaisAliquota, LerTotaisFormaPagamento,
|*    LerTotaisComprovanteNaoFiscal
|* 06/11/2008:  Anderson Rogerio Bejatto
|*  - Alteração Método VendeItem
|* 24/12/2008:  Daniel Simoes de Almeida
|*  - Cancelamento de cupons sem itens retornava Erro 51. Corrigido vendendo um
|*    Item de 0.01 centavos antes de cancelar   (Reportado por Licério Neto)
|*  - Metodo CarregaAliquotas alterado para incluir prefixo 'T' no indice
|* 25/03/2010:  José Nilton Pace
|*  - Implementado método GETNUMCCF
******************************************************************************}
{$I ACBr.inc}

unit ACBrECFMecaf ;

interface
uses
  Classes,
  {$IFDEF NEXTGEN}
   ACBrBase,
  {$ENDIF}
  ACBrECFClass, ACBrDevice, ACBrDeviceSerial;

type
{ Classe filha de TACBrECFClass com implementaçao para Mecaf }

{ TACBrECFMecaf }

TACBrECFMecaf = class( TACBrECFClass )
 private
    fsNumVersao : String ;
    fsNumCRO    : String ;
    fsCNPJ      : String ;
    fsIE        : String ;
    fsNumECF    : String ;
    fsArredonda : Char ;
    fsVinculado : Boolean ;

    { Substitui caracteres nulos do retorno por espaços. #0 -> ' ' }
    function LimpaStr( const AString : AnsiString ) : AnsiString ;
    { Retorna Verdadeiro se o ECF é antigo Versão: 201 ou 301 }
    function IsOldMecaf : Boolean ;

 protected
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumCCF: String; override ;
    function GetNumECF: String; override ;
    function GetNumCRO: String; override ;
    function GetNumCRZ: String; override ;
    function GetNumGNF: String; override ;
    function GetNumSerie: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;
    function GetVendaBruta: Double; override ;
    function GetGrandeTotal: Double; override ;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;

    function GetTotalAcrescimosISSQN: Double; override;
    function GetTotalCancelamentosISSQN: Double; override;
    function GetTotalDescontosISSQN: Double; override;


    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetNumCOOInicial: String; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;
    function GetArredonda: Boolean; override ;
    function GetChequePronto: Boolean; override ;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;

 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

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
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;

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

    { Procedimentos de Cupom Não Fiscal }
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ; override ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaNaoFiscal ; override ;

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
       PermiteVinculado : Boolean = true; Posicao : String = '') ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;
 end ;

implementation
Uses SysUtils, Math,
    {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
    ACBrConsts, ACBrUtil;

{ ----------------------------- TACBrECFMecaf ------------------------------ }

constructor TACBrECFMecaf.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.HandShake := hsRTS_CTS ;
  { Variaveis internas dessa classe }
  fsVinculado := false ;
  fsNumVersao := '' ;
  fsNumECF    := '' ;
  fsNumCRO    := '' ;
  fsCNPJ      := '' ;
  fsIE        := '' ;
  fsArredonda := ' ';

  fpModeloStr := 'Mecaf' ;

  // O ecf em teste recebia apenas 40 colunas. Mecaf FPE 301
  fpColunas := 40;
end;

destructor TACBrECFMecaf.Destroy;
begin

  inherited Destroy ;
end;

procedure TACBrECFMecaf.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+#10+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

  fpDevice.HandShake := hsRTS_CTS ;
  inherited Ativar ; { Abre porta serial }

  fsNumVersao := '' ;
  fsNumECF    := '' ;
  fsNumCRO    := '' ;
  fsCNPJ      := '' ;
  fsIE        := '' ;
  fsArredonda := ' ';

  try
     { Testando a comunicaçao com a porta }
     if NumVersao = '' then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));

     if IsOldMecaf then
        fpColunas := 48;   // Mecaf antiga aceita 48 colunas
     
     if pos('302',NumVersao) > 0 then
        fpColunas := 40;
  except
     Desativar ;
     raise ;
  end ;
end;


function TACBrECFMecaf.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
Var ErroMsg : String ;
    Erro : Integer ;
begin
  result  := '' ;
  ErroMsg := '' ;
  fpComandoEnviado   := '' ;
  fpRespostaComando  := '' ;

  { Codificando CMD de acordo com o protocolo da Mecaf }
  cmd := STX + ESC + '$' + cmd + ETX ;

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
  { Retirando STX e ETX }
  Result := copy(fpRespostaComando, 2, Length(fpRespostaComando)-2 ) ;

  { Verificando por erros }
  ErroMsg := '' ;
  Erro    := StrToIntDef( copy(Result,6,2), 0 ) ;
  if LeftStr(Result,1) = '-' then
     case Erro of
        1 : ErroMsg := 'Cabeçalho contém caracteres inválidos' ;
        2 : ErroMsg := 'Comando inexistente' ;
        3 : ErroMsg := 'Valor não numérico em campo numérico' ;
        4 : ErroMsg := 'Caracteres inválidos no Comando' ;
        5 : ErroMsg := 'Campo deve iniciar com `@´, `&´ ou `%´' ;
        6 : ErroMsg := 'Campo deve iniciar com `$´, `#´ ou `?´' ;
        7 : ErroMsg := 'O intervalo é inconsistente' ;
        9 : ErroMsg := 'A palavra TOTAL não é aceita' ;
       10 : ErroMsg := 'A sintaxe do comando está errada' ;
       11 : ErroMsg := 'Excedeu o nº máximo de linhas permitidas pelo comando' ;
       12 : ErroMsg := 'O terminador enviado não está obedecendo o protocolo de comunicação' ;
       13 : ErroMsg := 'O checksum enviado está incorreto' ;
       14 : ErroMsg := 'A situação tributária deve iniciar com `T´, `F´,`I´ ou ´N´' ;
       16 : ErroMsg := 'Data Inválida' ;
       17 : ErroMsg := 'Hora Inválida' ;
       18 : ErroMsg := 'Aliquota não programada ou fora do Intervalo' ;
       20 : ErroMsg := 'Comando só aceito em Intervenção Fiscal' ;
       21 : ErroMsg := 'Comando só aceito em Modo Normal' ;
       22 : ErroMsg := 'Necessário abrir cupom' ;
       23 : ErroMsg := 'Comando não aceito durante Cupom Fiscal' ;
       26 : ErroMsg := 'O relógio já está em horário de verão' ;
       27 : ErroMsg := 'O relógio não está em horário de verão' ;
       28 : ErroMsg := 'Necessário realizar Redução Z' ;
       29 : ErroMsg := 'Fechamento diário (Redução Z) já realizada' ;
       31 : ErroMsg := 'Item inexistente ou já cancelado' ;
       32 : ErroMsg := 'Cupom anterior não pode ser cancelado' ;
       33 : ErroMsg := 'Detectado falta de papel' ;
       37 : ErroMsg := 'Necessário realizar intervenção fiscal' ;
       38 : ErroMsg := 'Memória fiscal não permite mais realizar vendas. '+
                       'Só é possível efetuar Leitura X ou da Mem.Fiscal.' ;
       39 : ErroMsg := 'OCORREU ALGUM PROBLEMA NA MEMORIA FISCAL. '+
                       'Será necessário realizar uma Intervenção Técnica '+
                       'Só é possível efetuar Leitura X ou da Mem.Fiscal.' ;
       40 : ErroMsg := 'Necessário programar a Data do Relógio' ;
       41 : ErroMsg := 'Número máximo de itens por cupom ultrapassado' ;
       44 : ErroMsg := 'Impressora em Estado de impressão de cheque' ;
       46 : ErroMsg := 'Necessário inserir o cheque' ;
       47 : ErroMsg := 'Necessário inserir nova bobina' ;
       48 : ErroMsg := 'Necessário executar uma Leitura X' ;
       49 : ErroMsg := 'Detectado algum problema na Impressora. '+
                       'Papel encravado, sobretensão, etc...' ;
       50 : ErroMsg := 'Cupom já foi totalizado' ;
       53 : ErroMsg := 'Ocorreu erro de gravação na memória fiscal' ;
       58 : ErroMsg := 'Falta completar valor do pagamento' ;
       61 : ErroMsg := 'Troco não realizado' ;
       63 : ErroMsg := 'Impressora não está respondendo' ;
     else
        ErroMsg := 'Erro retornado pelo ECF: '+IntToStr(Erro) ;
     end
  else
     { Verifica se possui erro "Pouco Papel" }
     if (LeftStr(Result,1) = '+') and (Erro = 1) then
        DoOnMsgPoucoPapel ;

  if ErroMsg <> '' then
   begin
     ErroMsg := ACBrStr('Erro retornado pela Impressora: '+fpModeloStr+#10+#10+
                ErroMsg );

     if (Erro = 33) then
        DoOnErrorSemPapel
     else
        raise EACBrECFSemResposta.create(ErroMsg) ;
   end
  else
     Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }

end;

function TACBrECFMecaf.VerificaFimLeitura(var Retorno : AnsiString ;
   var TempoLimite : TDateTime) : Boolean ;
begin
//  result := (LeftStr(  Retorno,1 ) = STX) and
//            (RightStr( Retorno,1 ) = ETX) ;
  { Modificado pois leitura da Memoria Fiscal pela serial nao inicia com STX }
  result := (RightStr( Retorno,1 ) = ETX) ;
end;

function TACBrECFMecaf.GetDataHora: TDateTime;
Var RetCmd : AnsiString ;
    OldShortDateFormat : String ;
begin
  RetCmd := EnviaComando( '33' ) ;
  OldShortDateFormat := ShortDateFormat ;
  try
     ShortDateFormat := 'dd/mm/yy' ;
     Result := StringToDateTime( Copy(RetCmd,6,8) ) ;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;
  Result := RecodeHour(  result,StrToInt(copy(RetCmd,15,2))) ;
  Result := RecodeMinute(result,StrToInt(copy(RetCmd,18,2))) ;
  Result := RecodeSecond(result,StrToInt(copy(RetCmd,21,2))) ;
end;

function TACBrECFMecaf.GetNumCupom: String;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M41' ) ;
  Result := '' ;
  if LeftStr(RetCmd, 1) = '+' then
     Result := IntToStrZero( StrToIntDef( copy(RetCmd,6,6), 0), 6) ;
end;

function TACBrECFMecaf.GetNumCCF: String;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M82' ) ;
  Result := '' ;
  if LeftStr(RetCmd,1) = '+' then
     Result := copy(RetCmd,6,6) ;
end;

function TACBrECFMecaf.GetNumCRO: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumCRO) = '' then
  begin
     RetCmd := EnviaComando( '34M43' ) ;
     if LeftStr(RetCmd,1) = '+' then
        fsNumCRO := IntToStrZero( StrToIntDef(copy(RetCmd,6,4),0),4) ;
  end ;

  Result := fsNumCRO ;
end;

function TACBrECFMecaf.GetNumECF: String;
Var RetCmd : AnsiString ;
begin
  if Trim(fsNumECF) = '' then
  begin
     RetCmd := EnviaComando( '34M48' ) ;
     if LeftStr(RetCmd,1) = '+' then
        fsNumECF := IntToStrZero( StrToIntDef(copy(RetCmd,6,6),0),4) ;
  end ;

  Result := fsNumECF ;
end;

function TACBrECFMecaf.GetNumSerie: String;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M49' ) ;
  Result := '' ;
  if LeftStr(RetCmd,1) = '+' then
     Result := Copy(RetCmd,6,10) ;
end;

function TACBrECFMecaf.GetNumVersao: String ;
Var RetCmd : AnsiString ;
begin
  if fsNumVersao = '' then
  begin
     RetCmd := EnviaComando( '34M47' ) ;
     if LeftStr(RetCmd,1) = '+' then
        fsNumVersao := Copy(RetCmd,6,7) ;
  end ;

  Result := fsNumVersao ;
end;

function TACBrECFMecaf.GetTotalPago: Double;
Var RetCmd : AnsiString ;
    Valor : Double ;
begin
  RetCmd := EnviaComando( '34M97' ) ;
  Result := 0 ;

  if LeftStr(RetCmd,1) = '+' then
  begin
     Valor := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;

     if RightStr(RetCmd,1) = '-' then
        Result := Subtotal - Valor 
     else
        Result := Subtotal + Valor ;

     Result := RoundTo(Result,-2) ;
  end ;
end;

function TACBrECFMecaf.GetSubTotal: Double;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M96' ) ;
  Result :=  0 ;
  if LeftStr(RetCmd, 1) = '+' then
     Result := RoundTo( StrToFloatDef( copy(RetCmd,6,15), 0 ) / 100, -2) ;
end;


function TACBrECFMecaf.GetEstado: TACBrECFEstado;
Var RetCmd : AnsiString ;
    Stat1,Stat4,Stat5 : Byte ;
    DataRedZ : String ;
begin
  if (not fpAtivo) then
     fpEstado := estNaoInicializada
  else
   begin
     RetCmd := EnviaComando( '32' ) ;

     Stat1  := StrToInt( '$'+copy(RetCmd, 6,2) ) ;
     Stat4  := StrToInt( '$'+copy(RetCmd,12,2) ) ;
     Stat5  := StrToInt( '$'+copy(RetCmd,14,2) ) ;
     DataRedZ := '' ;

     // este if foi alterado pois estava jogando para um estado
     // desconhecido qdo a reducao ficava pendente e no dia seguinte era efetuada.
     if TestBit( Stat1 ,2) then 
     begin
        RetCmd := EnviaComando( '34M83' ) ;
        { Usei o StringReplace pois não tenho certeza se a resposta vem com ou
          sem as / (estou sem o ECF para testes). Segundo o manual não deveriam
          vir as /  }
        DataRedZ := StringReplace(copy(RetCmd,6,8),'/','',[rfReplaceAll]) ;
     end;

     if TestBit( Stat1 ,5) then
        fpEstado := estRequerZ
     else if TestBit( Stat4 ,7) then
        fpEstado := estRequerX
     else if  TestBit( Stat1 ,2) and (DataRedZ = FormatDateTime('ddmmyy',now)) then
        fpEstado := estBloqueada
     else if TestBit( Stat1,0) and
            (TestBit( Stat5,1) or TestBit( Stat5,4) or TestBit( Stat5,5)) then
        fpEstado := estPagamento
     else if TestBit( Stat1, 0) then
        fpEstado := estVenda
     else
      begin
        RetCmd := EnviaComando( '34M84' ) ;
        Stat1  := StrToIntDef( '$'+copy(RetCmd, 6,2) ,0 ) ;

        if TestBit( Stat1, 0) or TestBit( Stat1, 1) or TestBit( Stat1, 2) then
           fpEstado := estRelatorio
        else
           fpEstado := estLivre ;
      end ;
   end ;

  Result := fpEstado ;
end;

function TACBrECFMecaf.GetGavetaAberta: Boolean;
Var RetCmd : AnsiString ;
    Stat3 : Byte ;
begin
  RetCmd := EnviaComando( '32' ) ;
  Stat3  := StrToInt( '$'+copy(RetCmd,10,2) ) ;
  Result := TestBit( Stat3, 1) ;
end;

function TACBrECFMecaf.GetPoucoPapel: Boolean;
Var RetCmd : AnsiString ;
    Stat3  : Byte ;
begin
  RetCmd := EnviaComando( '32' ) ;
  Stat3  := StrToInt( '$'+copy(RetCmd,10,2) ) ;
  Result := TestBit( Stat3, 6) ;
end;

function TACBrECFMecaf.GetHorarioVerao: Boolean;
Var RetCmd : AnsiString ;
    Stat1  : Byte ;
begin
  RetCmd := EnviaComando( '32' ) ;
  Stat1  := StrToInt( '$'+copy(RetCmd,6,2) ) ;
  Result := TestBit( Stat1, 6) ;
end;

function TACBrECFMecaf.GetArredonda: Boolean;
Var RetCmd : AnsiString ;
    Stat1  : Byte ;
begin
  if fsArredonda = ' ' then
  begin
     RetCmd := EnviaComando( '34M84' ) ;
     Stat1  := StrToIntDef( '$'+copy(RetCmd, 6,2) ,0 ) ;

     if TestBit( Stat1, 6) then
        fsArredonda := 'S'
     else
        fsArredonda := 'N' ;
  end ;

  Result := (fsArredonda = 'S') ;
end;

procedure TACBrECFMecaf.LeituraX ;
begin
  EnviaComando('150' , 40 ) ;
end;

procedure TACBrECFMecaf.ReducaoZ(DataHora : TDateTime) ;
begin
  EnviaComando( '160', 40 ) ;  { Mecaf NAO usa DataHora }
end;

procedure TACBrECFMecaf.AbreGaveta ;
begin
  sleep(300) ;
  EnviaComando( '240' + #20 + #80 ) ;
end;

procedure TACBrECFMecaf.MudaHorarioVerao ;
begin
  MudaHorarioVerao( not HorarioVerao ) ;
end;

procedure TACBrECFMecaf.MudaHorarioVerao(EHorarioVerao: Boolean);
 Var FlagHorarioVerao : Char ;
begin
  If EHorarioVerao then FlagHorarioVerao := '+' else FlagHorarioVerao := '-' ;
  EnviaComando( '27' + FlagHorarioVerao, 3 ) ;
end;

procedure TACBrECFMecaf.MudaArredondamento(Arredondar: Boolean);
 Var FlagArredondar : Char ;
begin
  If Arredondar then FlagArredondar := '1' else FlagArredondar := '0' ;
  EnviaComando( '50' + FlagArredondar ) ;
end;

procedure TACBrECFMecaf.AbreCupom ;
Var CPF_CNPJ : String ;
begin
  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }

  CPF_CNPJ := '' ;
  if (not IsOldMecaf) and (Consumidor.Documento <> '') then
     CPF_CNPJ := PadRight(Consumidor.Documento,28) ;

  EnviaComando('10'+CPF_CNPJ, 8) ;
  Consumidor.Enviado := (CPF_CNPJ <> '') and
                        (Trim(Consumidor.Nome)+Trim(Consumidor.Endereco)='')
end;

procedure TACBrECFMecaf.CancelaCupom(NumCOOCancelar: Integer);
begin
  try
     EnviaComando( '14' ,18) ;
  except
     on E : Exception do
     begin
        if (pos('51',E.Message) <> 0) then  // Cupom Aberto e sem Itens ?
         begin
           { Deve Vender 1 Item para conseguir cancelar }
           VendeItem('00000','CUPOM SERA CANCELADO','NN',1,0.01,0,'') ;
           EnviaComando( '14' ,18) ;  // Agora sim... cancelando...
         end
        else
           raise ;
     end ;
  end ;

  FechaRelatorio ;   { Fecha relatorio se ficou algum aberto (só por garantia)}
end;

procedure TACBrECFMecaf.CancelaItemVendido(NumItem: Integer);
begin
  EnviaComando( '12' + IntToStrZero(NumItem,3) ,1) ;
end;

procedure TACBrECFMecaf.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
Var Linhas, CodTroco : String ;
    FPG : TACBrECFFormaPagamento ;
begin
  if ImprimeVinculado then
     if fsVinculado then
        raise EACBrECFERRO.Create(ACBrStr('Já existe Forma de Pagamento com '+#10+
                               'comprovante NAO fiscal vinculado pendente. '+#10+
                               'Impressora: '+fpModeloStr+' aceita apenas um '+#10+
                               'Comprovante não Fiscal Viculado por Cupom.'))
     else
        fsVinculado := true ;

  Linhas := '' ;

  if IsOldMecaf then     { Modelo antigo não aceita Observações }
     Observacao := ''
  else
     Observacao := TiraAcentos(Observacao) ;  // Mecaf não imprime se tiver acentos

  if Length(Observacao) > 41 then
   begin
     Linhas     := '4' ;
     Observacao := PadRight(Observacao,80) ;
   end
  else if Length(Observacao) > 1 then
   begin
     Linhas     := '2' ;
     Observacao := PadRight(Observacao,40) ;
   end ;

  EnviaComando( '07' + CodFormaPagto + IntToStrZero( Round(Valor*100) ,15) +
                Linhas + Observacao ,5) ;

  { Mecaf nao lança troco sozinha. Verificando se precisa lançar Troco }
  if TotalPago > SubTotal then
  begin
     CodTroco := '00' ;
     FPG      := AchaFPGDescricao('Dinheiro') ;

{     if FPG = nil then
        MessageDlg('Forma de pagamento DINHEIRO não encontrada. '+#10+
                    'Usando Forma de Pagamento "00" para lançar o Troco.',
                    mtWarning,[mbOk],0)
     else}
     if FPG <> nil then
        CodTroco := FPG.Indice ;

     EnviaComando( '07' + CodTroco + StringOfChar('0' ,15) ,5) ;
  end ;
end;

procedure TACBrECFMecaf.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
Var Linhas : Integer ;
    Obs    : String ;
begin
  { Ajustando a Observaçao em N Linhas do mesmo tamanho de COLUNAS}
  Obs    := TiraAcentos( Observacao ) ;  // Mecaf não imprime se tiver acentos
  Obs    := AjustaLinhas( Obs, Colunas, 8, True) ;
  Linhas := CountStr( Obs, #10) ;

  { Fecha cupom }
  EnviaComando( '08S' + IntToStrZero(Length(Obs),3) + Obs, 7 + Linhas) ;

  fsVinculado := false
end;

procedure TACBrECFMecaf.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString);
Var A_D : String ;
begin
  if DescontoAcrescimo > 0 then
     A_D := '@'
  else
     A_D := '' ;

  DescontoAcrescimo := abs(DescontoAcrescimo) ;

  { Inicia fechamento com formas de Pagamento }
  EnviaComando( '06' + A_D + '&' +
                IntToStrZero( Round(DescontoAcrescimo*100), 15) ,2) ;
  fsVinculado := false ;
end;

procedure TACBrECFMecaf.VendeItem(Codigo, Descricao : String ;
   AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
   ValorDescontoAcrescimo : Double ; Unidade : String ;
   TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
   CodDepartamento : Integer) ;
Var QtdStr, ValorStr, DescontoStr, Fmt: String ;
    FlagDesc : Integer ;
begin
  if Qtd > 9999 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'Quantidade deve ser inferior a 9999.'));

  { Mecaf não permite Acrescimo por Item }
  if (ValorDescontoAcrescimo > 0) and (DescontoAcrescimo = 'A') then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'ECF '+fpModeloStr+' não permite Acréscimo por Item'));

  Codigo      := PadRight(Codigo,13) ;    { Ajustando Tamanhos }
  Unidade     := PadRight(Unidade,2) ;

  if IsOldMecaf then
   begin
     QtdStr   := IntToStrZero( Round( Qtd*1000 ) ,6) ;
     if DescricaoGrande then  
        Fmt   := ''
     else
        Fmt   := '-' ;
     ValorStr := IntToStrZero( Round( ValorUnitario*100 ) ,11) ;
   end
  else
   begin
     QtdStr := IntToStrZero( Round( Qtd*1000 ) ,7) ;

     if RoundTo(ValorUnitario,-2) <> ValorUnitario then {Tem mais de 2 casas dec ?}
      begin
        Fmt := 'H' ;
        ValorStr := IntToStrZero( Round( ValorUnitario*1000 ) ,11) ;
      end
     else
      begin
        Fmt   := 'G' ;
        ValorStr := IntToStrZero( Round( ValorUnitario*100 ) ,11) ;
      end;
   end ;

  if TipoDescontoAcrescimo='%' then
     DescontoStr := '%' + IntToStrZero( Round(ValorDescontoAcrescimo * 100), 4)
  else
     DescontoStr := '&' + IntToStrZero( Round(ValorDescontoAcrescimo * 100), 15) ;

  { maximo 5 linhas de 38 caracteres... e sem acentos } 
  Descricao := TiraAcentos( LeftStr( TrimRight(Descricao), 190)) ;
  if (Length(Descricao) <= 20) or (not DescricaoGrande) then
   begin
     FlagDesc  := 0 ;
     Descricao := PadRight(Descricao,20) ;
   end

  else if (Length(Descricao) <= 38) then
   begin
     FlagDesc  := 1 ;
     Descricao := PadRight(Descricao,38) ;
   end
  else
   begin
     FlagDesc  := Trunc( Length(Descricao) / 38 ) ;
     if (Length(Descricao) mod 38) > 0 then
        FlagDesc := FlagDesc + 1 ;

     Descricao := PadRight(Descricao, (38 * FlagDesc) ) ;
   end ;

  EnviaComando( '11' + Fmt + QtdStr + ValorStr + AliquotaECF +
                DescontoStr + Unidade + Codigo + IntToStr( FlagDesc ) +
                Descricao ,2) ;
end;

procedure TACBrECFMecaf.CarregaAliquotas;
Var RetCmd : AnsiString ;
    Aliquota : TACBrECFAliquota ;
    ValAliq : Double ;
    A : Integer ;
    ByteISS1,ByteISS2 : Byte ;
begin
  RetCmd   := EnviaComando( '34M86' ) ;
  ByteISS2 := StrToIntDef( '$'+copy(RetCmd, 8,2) ,0 ) ;
  ByteISS1 := StrToIntDef( '$'+copy(RetCmd,10,2) ,0 ) ;

  RetCmd := EnviaComando( '30' ) ;

  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  For A := 0 to 15 do
  begin
     ValAliq := RoundTo( StrToIntDef( copy(RetCmd, 6+(A*4), 4),0 ) / 100, -2) ;

     if ValAliq > 0 then
     begin
        Aliquota := TACBrECFAliquota.create ;

        Aliquota.Indice   := 'T'+IntToStrZero(A,2);
        Aliquota.Aliquota := ValAliq ;

        if A < 8 then
         begin
           if TestBit( ByteISS1, A ) then
              Aliquota.Tipo := 'S' ;
         end
        else
           if TestBit( ByteISS2, A-8) then
              Aliquota.Tipo := 'S' ;

        fpAliquotas.Add( Aliquota ) ;
     end ;
  end ;
end;

procedure TACBrECFMecaf.LerTotaisAliquota;
 Var I, P   : Integer ;
     RetCmd : AnsiString ;
begin
  if not Assigned( fpAliquotas ) then
     CarregaAliquotas ;

  For P := 0 to fpAliquotas.Count-1 do
  begin
     I := StrToIntDef( copy(fpAliquotas[P].Indice,2,2), -1) ;
     if I >= 0 then
     begin
        RetCmd := EnviaComando( '34M'+IntToStrZero(I,2) ) ;
        if LeftStr(RetCmd,1) = '+' then
           fpAliquotas[P].Total := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
     end ;
  end ;
end;

procedure TACBrECFMecaf.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String);
Var ProxIndice : Integer ;
    sTipo, ValStr : String ;
    Aliq : TACBrECFAliquota ;
begin
  ValStr := IntToStrZero( Round(Aliquota * 100) ,4) ;
  Tipo := UpCase(Tipo) ;
  if Tipo = 'S' then
     sTipo := '001'
  else
     sTipo := '000' ;

  CarregaAliquotas ;

  ProxIndice := StrToIntDef(Posicao,0) ;
  if (ProxIndice < 1) or (ProxIndice > 15) then { Indice passado é válido ? }
  begin
     For ProxIndice := 1 to 16 do  { Procurando Lacuna }
     begin
        if AchaICMSIndice('T'+IntToStrZero(ProxIndice,2)) = nil then
           break ;
     end ;
  end ;

  if ProxIndice > 15 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Aliquotas'));

  EnviaComando( '46' + IntToStrZero(ProxIndice,2) + ValStr + sTipo ) ;

  { Adcionanodo nova Aliquota no ObjectList }
  Aliq := TACBrECFAliquota.create ;
  Aliq.Indice   := 'T'+IntToStrZero(ProxIndice,2) ;
  Aliq.Aliquota := Aliquota ;
  if Tipo = '1' then
     Aliq.Tipo := 'S' ;
  fpAliquotas.Add( Aliq ) ;
end;

function TACBrECFMecaf.AchaICMSAliquota( var AliquotaICMS: String):
   TACBrECFAliquota;
  Var AliquotaStr : String ;
begin
  AliquotaStr := '' ;
  Result      := nil ;
  
  {Por indice, permite T01, TT01 ou T1 => todas devem ser indice = T01 }

  if copy(AliquotaICMS,1,2) = 'TT' then { Corrige Duplo T  }
     AliquotaICMS := copy(AliquotaICMS,2,5) ;

  case AliquotaICMS[1] of
    'I' : AliquotaStr := 'I00' ;
    'N' : AliquotaStr := 'N00' ;
    'F' : AliquotaStr := 'F00' ;
    'T' : AliquotaICMS := 'TT'+PadLeft(copy(AliquotaICMS,2,2),2,'0') { Indice T01, T02, T03}
  end ;

  if AliquotaStr = '' then
     Result := inherited AchaICMSAliquota( AliquotaICMS )
  else
     AliquotaICMS := AliquotaStr ;
end;


procedure TACBrECFMecaf.CarregaFormasPagamento;  { funçao Lenta +- 3 sec. }
Var RetCmd, Descricao : AnsiString ;
    Cont : Integer ;
    FPagto : TACBrECFFormaPagamento ;
begin
  inherited CarregaFormasPagamento ;       {Inicializa fpFormasPagamentos}

  try
     for Cont := 0 to 15 do
     begin
        RetCmd := EnviaComando( '34M' + IntToStrZero(50 + Cont,2) ) ;

        Descricao := Trim(copy(RetCmd,6,16)) ;
        if Descricao <> '' then
        begin
           FPagto := TACBrECFFormaPagamento.create ;

           FPagto.Indice    := IntToStrZero(Cont,2) ;
           FPagto.Descricao := Descricao ;
           FPagto.Total     := RoundTo( StrToFloatDef(copy(RetCmd,22,15),0) / 100, -2 ) ;

           fpFormasPagamentos.Add( FPagto ) ;
        end ;
     end ;
  except
     fpFormasPagamentos.Free ;
     fpFormasPagamentos := nil ;

     raise ;
  end ;
end;

procedure TACBrECFMecaf.LerTotaisFormaPagamento;
begin
  CarregaFormasPagamento ;
end;

procedure TACBrECFMecaf.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String );
Var ProxIndice : Integer ;
    FPagto : TACBrECFFormaPagamento ;
begin
  { Mecaf nao usa PermiteVinculado }
  Descricao := PadRight(Descricao,16) ;

  CarregaFormasPagamento ;
  
  ProxIndice := StrToIntDef(Posicao,0) ;
  if (ProxIndice < 1) or (ProxIndice > 15) then { Indice passado é válido ? }
  begin
     For ProxIndice := 0 to 16 do  { Procurando Lacuna }
     begin
        if AchaFPGIndice(IntToStrZero(ProxIndice,2)) = nil then
           break ;
     end ;
  end ;

  if ProxIndice > 15 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novas Formas de '+
                            'Pagamento'));

  EnviaComando( '29' + IntToStrZero(ProxIndice,2) + Descricao ) ;

  { Adcionanodo nova FPG no ObjectList }
  FPagto := TACBrECFFormaPagamento.create ;
  FPagto.Indice    := IntToStrZero(ProxIndice,2) ;
  FPagto.Descricao := Descricao ;
  fpFormasPagamentos.Add( FPagto ) ;
end;

procedure TACBrECFMecaf.CarregaComprovantesNaoFiscais;
Var RetCmd, Descricao : AnsiString ;
    Cont : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  inherited CarregaComprovantesNaoFiscais ;{Inicializa fpComprovantesNaoFiscais}

  try
     for Cont := 16 to 31 do
     begin
        RetCmd    := EnviaComando( '34M' + IntToStrZero(50 + Cont,2) ) ;

        Descricao := Trim(copy(RetCmd,6,16)) ;
        if Descricao <> '' then
        begin
           CNF := TACBrECFComprovanteNaoFiscal.create ;

           CNF.Indice    := IntToStrZero(Cont,2) ;
           CNF.Descricao := Descricao ;
           CNF.Total     := RoundTo( StrToFloatDef(copy(RetCmd,22,15),0) / 100, -2 ) ;

           fpComprovantesNaoFiscais.Add( CNF ) ;
        end ;
     end ;
  except
     fpComprovantesNaoFiscais.Free ;
     fpComprovantesNaoFiscais := nil ;

     raise ;
  end ;
end;

procedure TACBrECFMecaf.LerTotaisComprovanteNaoFiscal;
begin
  CarregaComprovantesNaoFiscais ;
end;

procedure TACBrECFMecaf.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String );
Var ProxIndice : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  { Mecaf nao usa Tipo }
  Descricao := PadRight(Descricao,16) ;

  CarregaComprovantesNaoFiscais ;

  ProxIndice := StrToIntDef(Posicao,0) ;
  if (ProxIndice < 16) or (ProxIndice > 31) then { Indice passado é válido ? }
  begin
     For ProxIndice := 16 to 32 do  { Procurando Lacuna }
     begin
        if AchaCNFIndice(IntToStrZero(ProxIndice,2)) = nil then
           break ;
     end ;
  end ;

  if ProxIndice > 31 then
     raise EACBrECFERRO.create(ACBrStr('Não há espaço para programar novos Comprovantes'+
                            ' não Fiscais'));

  EnviaComando( '29' + IntToStrZero(ProxIndice,2) + Descricao ) ;

  { Adcionanodo novo CNF no ObjectList }
  CNF := TACBrECFComprovanteNaoFiscal.create ;
  CNF.Indice    := IntToStrZero(ProxIndice,2) ;
  CNF.Descricao := Descricao ;
  fpComprovantesNaoFiscais.Add( CNF ) ;
end;


procedure TACBrECFMecaf.AbreRelatorioGerencial(Indice : Integer) ;
begin
  EnviaComando( '151' ,40 ) ;
end;

procedure TACBrECFMecaf.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  Linha := TiraAcentos(Linha) ;  // Mecaf não imprime se tiver acentos
  ImprimirLinhaALinha( Linha, '260' );
end;

procedure TACBrECFMecaf.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
begin
  { Mecaf nao faz Vinculo a uma Forma de Pagamento específica do Cupom Anterior
    também não é necessário informar nenhum CNF, ou Valor do Cupom Anterior.
    A unica exigencia que a impressora faz é que o Documento Anterior seja
    Cupom Fiscal ou Comprovante nao vinculado
    Fica fácil criar o Vinculado, mas não há a possibilidade de criar Varios
    Cupons Vinculados a mais de uma forma de pagamento do Cupom Anterior. Ou
    Seja o vinculo é feito ao CUPOM Anterior e não a alguma das Formas de
    Pagamento. }

  EnviaComando( '20' ,8 ) ;
end;

procedure TACBrECFMecaf.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha ) ; 
end;

procedure TACBrECFMecaf.FechaRelatorio;
Var RetCmd : AnsiString ;
    Stat   : Byte ;
begin
  RetCmd := EnviaComando( '34M84' ) ;
  Stat   := StrToInt( '$'+copy(RetCmd, 6,2) ) ;

  if TestBit( Stat, 0) or TestBit( Stat, 1) or TestBit( Stat, 2) then
     EnviaComando( '21' ,12 ) ;
end;

procedure TACBrECFMecaf.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada : Boolean);
Var Espera : Integer ;
begin
  // Mecaf não possui Leitura Simplificada
  Espera := 400 + (ReducaoFinal - ReducaoInicial) ;
  EnviaComando( '18'+IntToStrZero(ReducaoInicial,4)+
                     IntToStrZero(ReducaoFinal  ,4), Espera ) ;
end;

procedure TACBrECFMecaf.LeituraMemoriaFiscal(DataInicial, DataFinal: TDateTime;
  Simplificada : Boolean);
  Var Espera : Integer ;
begin
  // Mecaf não possui Leitura Simplificada
  Espera := 400 + DaysBetween(DataInicial,DataFinal) ;
  EnviaComando( '17'+FormatDateTime('ddmmyy',DataInicial)+
                     FormatDateTime('ddmmyy',DataFinal),   Espera ) ;
end;

procedure TACBrECFMecaf.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas : TStringList; Simplificada : Boolean);
Var Espera : Integer ;
    RetCmd : AnsiString ;
begin
  // Mecaf não possui Leitura Simplificada
  Espera := Trunc(60 + (ReducaoFinal - ReducaoInicial) ) ;
  Linhas.Clear ;
  RetCmd := EnviaComando( '37'+IntToStrZero(ReducaoInicial,4)+
                              IntToStrZero(ReducaoFinal  ,4), Espera ) ;
  RetCmd := LimpaStr( RetCmd ) ;  { Troca #0 dentro da String por espaços }
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

procedure TACBrECFMecaf.LeituraMemoriaFiscalSerial(DataInicial,
  DataFinal: TDateTime; Linhas : TStringList; Simplificada : Boolean);
Var Espera : Integer ;
    RetCmd : AnsiString ;
begin
  // Mecaf não possui Leitura Simplificada
  Espera := Trunc(60 + DaysBetween(DataInicial,DataFinal) ) ;
  RetCmd := EnviaComando( '36'+FormatDateTime('ddmmyy',DataInicial)+
                               FormatDateTime('ddmmyy',DataFinal), Espera );
  RetCmd := LimpaStr( RetCmd ) ;  { Troca #0 dentro da String por espaços }
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

function TACBrECFMecaf.GetChequePronto: Boolean;
Var RetCmd : AnsiString ;
    Stat4 : Byte ;
begin
  RetCmd := EnviaComando( '32' ) ;
  Stat4  := StrToInt( '$'+copy(RetCmd,12,2) ) ;
  Result := TestBit( Stat4, 2) ;
end;

procedure TACBrECFMecaf.CancelaImpressaoCheque;
begin
  EnviaComando( '04' ) ;
end;

procedure TACBrECFMecaf.ImprimeCheque(Banco: String; Valor: Double;
  Favorecido, Cidade: String; Data: TDateTime; Observacao: String);
Var Dia,Mes,Ano, Lin : String ;
begin
  Favorecido := PadRight(Favorecido,80) ;
  Cidade     := PadRight(Cidade,20) ;
  Observacao := TiraAcentos( LeftStr(Observacao,160) ) ;
  Dia        := IntToStrZero(  DayOf(Data),2) ;
  Mes        := IntToStrZero(MonthOf(Data),2) ;
  Ano        := RightStr(IntToStrZero( YearOf(Data),4),2) ;

  Lin := #36 + #75 + #72 + #10 + #36 + #10 + #36 + #36 + #40 + #36 + #36 + #75 ;

  EnviaComando( '011' ) ;
  EnviaComando( '02' + Lin + IntToStrZero(Round(Valor*100),15) +
                Favorecido + Cidade + '1' + Dia + Mes + Ano + Observacao ,10) ;
end;

function TACBrECFMecaf.LimpaStr(const AString: AnsiString): AnsiString;
Var A,Len : Integer ;
begin
  Result := '' ;
  Len    := Length( AString ) ;

  For A := 1 to Len do
  begin
     if AString[A] = #0 then
        Result := Result + ' '
     else
        Result := Result + AString[A] ;
  end ;
end;

function TACBrECFMecaf.IsOldMecaf: Boolean;
begin
  Result := (pos('201', fsNumVersao) > 0) or (pos('301', fsNumVersao) > 0) or (pos('400', fsNumVersao) > 0) or (pos('302', fsNumVersao) > 0);
end;

procedure TACBrECFMecaf.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  EnviaComando( '22', 10 ) ;
end;

procedure TACBrECFMecaf.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString);
begin
  EnviaComando( '23' + CodCNF + IntToStrZero( Round(Valor*100) ,15) +
                    '&' + StringOfChar('0', 15), 5 ) ;

  if Trim(Obs) <> '' then
     LinhaRelatorioGerencial( LeftStr(Obs,Colunas*2) );
end;

procedure TACBrECFMecaf.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
   MensagemRodape: AnsiString);
begin
 {Não diponivel na Mecaf}
end;

procedure TACBrECFMecaf.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
begin
  EnviaComando( '21' ,12 ) ;
end;

procedure TACBrECFMecaf.CancelaNaoFiscal;
begin
  EnviaComando( '25', 10 ) ;
end;

function TACBrECFMecaf.GetCNPJ: String;
 Var RetCmd : AnsiString ;
begin
  if Trim(fsCNPJ) = '' then
  begin
     RetCmd := EnviaComando( '34M88' ) ;
     if LeftStr(RetCmd,1) = '+' then
     begin
        fsCNPJ := copy(RetCmd,6,18) ;
        fsIE   := copy(RetCmd,24,15) ;
     end ;
  end ;

  Result := fsCNPJ ;
end;

function TACBrECFMecaf.GetIE: String;
begin
  GetCNPJ ;
  Result := fsIE ;
end;

function TACBrECFMecaf.GetDataMovimento: TDateTime;
 Var RetCmd : AnsiString ;
     OldShortDateFormat : String ;
begin
  RetCmd := EnviaComando( '34M83' ) ;
  OldShortDateFormat := ShortDateFormat ;
  try
     ShortDateFormat := 'dd/mm/yy' ;
     Result := StrToDate( Copy(RetCmd,6 ,2)+DateSeparator+
                          Copy(RetCmd,8 ,2)+DateSeparator+
                          Copy(RetCmd,10,2) ) ;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;
end;

function TACBrECFMecaf.GetNumCOOInicial: String;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M82' ) ;
  Result := '' ;
  if LeftStr(RetCmd,1) = '+' then
     Result := copy(RetCmd,12,6) ;
end;

function TACBrECFMecaf.GetNumCRZ: String;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M42' ) ;
  Result := '' ;
  if LeftStr(RetCmd,1) = '+' then
     Result := copy(RetCmd,6,4) ;
end;

function TACBrECFMecaf.GetVendaBruta: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M40' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,19),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetGrandeTotal: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M39' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,19),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetTotalAcrescimos: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M37' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetTotalCancelamentos: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M38' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetTotalDescontos: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M36' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetTotalIsencao: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M32' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetTotalAcrescimosISSQN: Double;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M87' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,21,15),0) / 100, -2 ) ;

end;


function TACBrECFMecaf.GetTotalCancelamentosISSQN: Double;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M87' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,36,15),0) / 100, -2 ) ;

end;

function TACBrECFMecaf.GetTotalDescontosISSQN: Double;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M87' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetTotalNaoTributado: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M33' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetTotalSubstituicaoTributaria: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '34M34' ) ;
  Result := 0 ;
  if LeftStr(RetCmd,1) = '+' then
     Result := RoundTo( StrToFloatDef(copy(RetCmd,6,15),0) / 100, -2 ) ;
end;

function TACBrECFMecaf.GetNumGNF: String;
 Var RetCmd : AnsiString;
begin
  RetCmd := EnviaComando( '31' );
  Result := '';
  if LeftStr(RetCmd, 1) = '+' then
     Result := IntToStrZero( StrToIntDef( copy(RetCmd,352,4), 0), 4);
end;

end.

