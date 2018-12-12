{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Rodrigo Frühwirth                      }
{                                       Daniel Simoes de Almeida               }
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
|* 25/11/2005: Rodrigo Frühwirth
|*   Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
|* 06/12/2005:  Rodrigo Frühwirth
|*  - Corrigido bug na leitura do Estado do ECF
|* 08/12/2005:  Daniel Simoes de Almeida
|*  - Diminuido tempo de alguns Sleeps de 100 para 10 a fim de agilizar a
|*    comunicaçao com o ECF (experimental)
******************************************************************************}
{$I ACBr.inc}

unit ACBrECFICash ;

interface
uses Classes,
    ACBrECFClass, ACBrDevice;

const  cACK = 06  ;
       NACK= 21  ;

type
{ Classe filha de TACBrECFClass com implementaçao para ICash }

{ TACBrECFICash }

TACBrECFICash = class( TACBrECFClass )
 private
    fsACK, fsST1, fsST2 : Integer ; { Status da ICash }
    fsNumVersao : String ;
    fsNumCRO    : String ;
    fsNumECF    : String ;
    fsNumLoja   : String ;
    fsLeDadosSerial : Boolean ;
    fsVinculado : Boolean ;

    function PreparaCMD( cmd : AnsiString ) : AnsiString ;
    procedure EnviaConsumidor;
    function LimpaStr(const AString: AnsiString): AnsiString;

 protected
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumECF: String; override ;
    function GetNumLoja: String; override ;
    function GetNumSerie: String; override ;
    function GetNumVersao: String ; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;

    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetGrandeTotal: Double; override ;
    function GetNumCRO: String; override ;
    function GetNumCCF: String; override ;
    function GetNumGNF: String; override ;
    function GetNumGRG: String; override ;
    function GetNumCDC: String; override ;
    function GetNumCRZ: String; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;
    function GetNumCOOInicial: String; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;
    function GetArredonda: Boolean; override ;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;

 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    Property ACK   : Integer read fsACK ;
    Property ST1   : Integer read fsST1 ;
    Property ST2   : Integer read fsST2 ;
    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString = '' ) ; override ;
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

    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False); override ;
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
    procedure LerTotaisFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '') ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;

    Procedure IdentificaOperador ( Nome: String); override;

    { Procedimentos de Cupom Não Fiscal }
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ; override ;
    Procedure EfetuaPagamentoNaoFiscal( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false) ; override ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaNaoFiscal ; override ;

    procedure Sangria(const Valor: Double; Obs: AnsiString; DescricaoCNF,
      DescricaoFPG: String; IndiceBMP: Integer); override ;
    procedure Suprimento( const Valor: Double; Obs : AnsiString;
       DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer) ; override ;
 end ;

implementation
Uses SysUtils, Math,
    {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
    ACBrConsts, ACBrUtil;

{ ----------------------------- TACBrECFICash ------------------------------ }

constructor TACBrECFICash.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.HandShake := hsNenhum ;
  { Variaveis internas dessa classe }
  fsVinculado := false ;
  fsLeDadosSerial := False ;
  fsNumVersao := '';
  fsNumECF    := '';
  fsNumLoja   := '' ;
  fsNumCRO    := '';
  fsST1       := 0 ;
  fsST2       := 0 ;
  fsACK       := 0 ;
  fpIdentificaConsumidorRodape := True ;

  fpModeloStr := 'ICash' ;
  fpRFDID     := 'IC' ;
end;

destructor TACBrECFICash.Destroy;
begin

  inherited Destroy ;
end;

procedure TACBrECFICash.Ativar;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+#10+
                            'Porta Serial:  (COM1, COM2, COM3, ...)'));

  inherited Ativar ; { Abre porta serial }

  try
     { Testando a comunicaçao com a porta }
     if NumVersao = '' then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));
  except
     Desativar ;
     raise ;
  end ;
end;

function TACBrECFICash.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
 Var ErroMsg, StatusMsg : String ;
     FalhasACK : Integer;
begin
  fsACK   := 0  ;
  fsST1   := 0  ;
  fsST2   := 0  ;
  Result  := '' ;
  ErroMsg := '' ;
  fpComandoEnviado   := '' ;
  fpRespostaComando  := '' ;

  try
     { Codificando CMD de acordo com o protocolo da ICash }
     cmd := PreparaCMD( cmd );

     fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }
     FalhasACK := 0 ;

     while (fsACK <> cACK) do     { Se ACK = 6 Comando foi reconhecido }
     begin
        fsACK := 0 ;
        fpDevice.Serial.Purge ;                   { Limpa a Porta }

        if not TransmiteComando( cmd ) then
           continue ;

        try
           { espera ACK chegar na Porta por 1,5s }
           try
              fsACK := fpDevice.Serial.RecvByte( 1500 ) ;
           except
           end ;

           if fsACK = 0 then
              raise EACBrECFSemResposta.create( ACBrStr(
                       'Impressora '+fpModeloStr+' não responde (ACK = 0)') )
           else if fsACK = NACK then    { retorno em caracter 21d=15h=NAK }
              raise EACBrECFSemResposta.create( ACBrStr(
                    'Impressora '+fpModeloStr+' não reconheceu o Comando'+
                    sLineBreak+' (ACK = 21)') )
           else if fsACK <> cACK then
              raise EACBrECFSemResposta.create( ACBrStr(
                    'Erro. Resposta da Impressora '+fpModeloStr+' inválida'+
                    sLineBreak+' (ACK = '+IntToStr(fsACK)+')') ) ;
        except
           on E : EACBrECFSemResposta do
            begin
              fpDevice.Serial.Purge ;

              Inc( FalhasACK ) ;
              if FalhasACK < 3 then
                 Sleep(100)
              else
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

     { Formato da Resposta:
       ACK(1) + ST1(1)+ ST2(1) + STX(1) + DADOS(N) + ETX(1) + BCC(2)
       Nota, ACK já foi retirado acima... }

     { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
       falha na leitura LeResposta dispara Exceçao }
     LeResposta ;

     if fsLeDadosSerial then
      begin
        try fsST1 := ord( fpRespostaComando[ Length(fpRespostaComando)-1 ] ) ; except end ;
        try fsST2 := ord( fpRespostaComando[ Length(fpRespostaComando)   ] ) ; except end ;
      end
     else
      begin
        try fsST1 := ord( fpRespostaComando[ 1 ] ) ; except end ;
        try fsST2 := ord( fpRespostaComando[ 2 ] ) ; except end ;
      end ;

     Result := copy( fpRespostaComando,6, Length( fpRespostaComando ) - 8 ) ;

     { Verificando por erros }
     StatusMsg := '' ;
     case fsST1 of
        0 : StatusMsg := '' ;
       48 : StatusMsg := 'Em período de vendas';
       49 : StatusMsg := 'Cupom Fiscal Aberto (Cabeçalho Impresso)';
       50 : StatusMsg := 'Cupom Fiscal em Posição de Efetuar Venda';
       51 : StatusMsg := 'Cupom Fiscal Totalizando Venda';
       52 : StatusMsg := 'Cupom Fiscal Finalizando a Totalização (Formas de Pagamento)';
       53 : StatusMsg := 'Cupom Fiscal com Totalização Finalizada';
       54 : StatusMsg := 'Cupom Não Fiscal Aberto';
       55 : StatusMsg := 'Cupom Não Fiscal em Posição de Efetuar Venda';
       56 : StatusMsg := 'Cupom Não Fiscal Totalizando Venda';
       57 : StatusMsg := 'Cupom Não Fiscal Finalizando a Totalização';
       65 : StatusMsg := 'Cupom Não Fiscal com Totalização Finalizada';
       66 : StatusMsg := 'Emitindo Relatório Gerencial';
       67 : StatusMsg := 'Em Intervenção Técnica';
       68 : StatusMsg := 'Dia já Encerrado';
       69 : StatusMsg := 'Obrigatório Fechar Dia';
       70 : StatusMsg := 'Comprovante de Crédito/Débito Aberto';
       71 : StatusMsg := 'Comprovante de Estorno Crédito/Débito Aberto';
       72 : StatusMsg := 'Fora de Período de Vendas';
     else
        StatusMsg := 'Status retornado pelo ECF '+fpModeloStr+': '+IntToStr(fsST1) ;
     end;

     case fsST2 of
       32 : ErroMsg := '' ;
       35 : ErroMsg := 'Tamanho do Comando Inválido';
       36 : ErroMsg := 'Comando Inválido';
       37 : ErroMsg := 'O Comando deve ser executado após zerar toda a memória';
       38 : ErroMsg := 'Não é após Redução Z';
       39 : ErroMsg := 'Erro de Cupom Aberto';
       40 : ErroMsg := 'Dados Numéricos Inválidos';
       41 : ErroMsg := 'Data e Hora Inválida';
       42 : ErroMsg := 'Comando já executado ou proibido';
       43 : ErroMsg := 'Erro de Texto Inválido';
       44 : ErroMsg := 'Erro de Sequencia de Operação';
       45 : ErroMsg := 'Erro de Parametro Inválido';
       47 : ErroMsg := 'Necessário Programação Prévia';
       64 : ErroMsg := 'Jumper de Intervenção na Posição Errada';
       67 : ErroMsg := 'Redução Z do Dia já Realizada';
       68 : ErroMsg := 'Efetuar Redução Z';
       79 : ErroMsg := 'Erro não Definido!';
     else
        ErroMsg := 'Erro retornado pelo ECF '+fpModeloStr+': '+IntToStr(fsST2) ;
     end;

     { ICash não tem mecanismo de alerta de de Pouco Papel }
  
     if ErroMsg <> '' then
      begin
        ErroMsg := ACBrStr('Erro retornado pela Impressora: '+fpModeloStr+
                   sLineBreak+sLineBreak+
                   'Erro..: '+ErroMsg  + sLineBreak+
                   'Status: '+StatusMsg );
        raise EACBrECFSemResposta.create(ErroMsg) ;
      end
     else
        Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }
  finally
     fsLeDadosSerial := False ;
  end ;
end;

function TACBrECFICash.VerificaFimLeitura(var Retorno : AnsiString ;
   var TempoLimite : TDateTime) : Boolean ;
 var LenRet : Integer ;
     Dados  : AnsiString ;
begin
  LenRet := Length(Retorno) ;

  Result := (LenRet = 2) ;  { Apenas ST1 e ST2 (sem resposta adicional ) }

  if not Result then
  begin
     if fsLeDadosSerial then
      begin
        Result := (LenRet > 3) and (Retorno[LenRet-2] = FF)
      end
     else
      begin
        if (LenRet > 8) and (Retorno[LenRet-2] = ETX) then
        begin
           { Verificando o BCC }
           Dados  := copy( Retorno,4, LenRet - 6 ) ;
           Result := (copy(Retorno,3,LenRet) = PreparaCMD(Dados)) ;

           if not Result then   // Cheksum não bateu, manda NACK
            begin
              Retorno := '' ;
              fpDevice.Serial.Purge ;
              fpDevice.Serial.SendByte(NACK);
            end
           else
              fpDevice.Serial.SendByte(cACK);
        end ;
      end ;
  end ;
end;

function TACBrECFICash.PreparaCMD(cmd : AnsiString) : AnsiString ;
  Var A, iSoma : Integer ;
      CS1, CS2 : AnsiChar ;
Begin
  Result := cmd;
  if cmd = '' then exit;

  cmd := STX + cmd + ETX ;   { STX(1) + CMD(2) + DADOS(N) + ETX + BCC(2) }

  { Calculando a Soma dos caracteres ASC }
  iSoma := 0 ;
  For A := 1 to Length(cmd) do
     iSoma := iSoma + ord( cmd[A] ) ;

  { Calculando os dígitos }
  CS1 := AnsiChar( chr( Trunc(iSoma / 256 ) ) );
  CS2 := AnsiChar( chr( iSoma mod 256 ) );

  Result := cmd + CS1 + CS2 ;
End;

function TACBrECFICash.GetDataHora: TDateTime;
Var RetCmd : AnsiString ;
    DataHora: TDateTime;
begin
   RetCmd := EnviaComando( '67081' ) ;
   DataHora := EncodeDateTime( StrToInt( Copy( RetCMD, 5, 4 ) ),
                               StrToInt( Copy( RetCMD, 3, 2 ) ),
                               StrToInt( Copy( RetCMD, 1, 2 ) ),
                               StrToInt( Copy( RetCMD, 9, 2 ) ),
                               StrToInt( Copy( RetCMD, 11, 2 ) ),
                               StrToInt( Copy( RetCMD, 13, 2 ) ),
                               0 );
   Result := DataHora;
end;

function TACBrECFICash.GetNumCupom: String;
Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67002' ) ;
  Result := Copy( RetCmd, 7, 6 );
end;

function TACBrECFICash.GetNumCRO: String;
begin
  if Trim(fsNumCRO) = '' then
     fsNumCRO := EnviaComando( '67000' );
  Result := fsNumCRO ;
end;

function TACBrECFICash.GetNumECF: String;
begin
  If Trim( fsNumECF ) = '' then
     fsNumECF := EnviaComando( '67086' );
   Result := fsNumECF;
end;

function TACBrECFICash.GetNumLoja: String;
begin
  If Trim( fsNumLoja ) = '' then
     fsNumLoja := EnviaComando( '67085' );
   Result := fsNumLoja;
end;

function TACBrECFICash.GetNumSerie: String;
begin
   Result := EnviaComando( '67091' );
end;

function TACBrECFICash.GetNumVersao: String ;
begin
  if fsNumVersao = '' then
     fsNumVersao := EnviaComando( '67089' );
  Result := fsNumVersao ;
end;

function TACBrECFICash.GetEstado: TACBrECFEstado;
begin
   if (not fpAtivo) then
      fpEstado := estNaoInicializada
   else
    begin
      EnviaComando( '10' ) ;

      case ST1 of
         48, 72     : fpEstado := estLivre ;
         49, 50     : fpEstado := estVenda ;
         51, 52, 53 : fpEstado := estPagamento ;
// TODO: Verificar estados Nao fiscais
         54, 55, 56, 57, 65 : fpEstado := estNaoFiscal ;
         66, 70     : fpEstado := estRelatorio ;
         68         : fpEstado := estBloqueada ;
         69         : fpEstado := estRequerZ ;
      else
         fpEstado := estDesconhecido;
      end;
    end ;

   Result := fpEstado ;
end;

function TACBrECFICash.GetTotalPago: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '2E' ) ;
  RetCmd := Copy( RetCMD, 29, 14 );
  Result := StrToFloatDef( RetCmd, 0 ) / 100;
end;

function TACBrECFICash.GetSubTotal: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '2E' ) ;
  RetCmd := Copy( RetCmd, 1, 14 );
  Result := StrToFloatDef( RetCmd, 0 ) / 100;
end;

function TACBrECFICash.GetGavetaAberta: Boolean;
 Var RetCmd : AnsiString ;
begin
   RetCMD := EnviaComando( '67082' );
   Result := (RetCmd[1] = '1') ;
end;

function TACBrECFICash.GetPoucoPapel: Boolean;
 Var RetCmd: AnsiString;
begin
   RetCmd := EnviaComando( '67082' );
   Result := (RetCmd[2] = '0') ;
end;

function TACBrECFICash.GetHorarioVerao: Boolean;
 Var RetCmd: AnsiString;
begin
   RetCmd := EnviaComando( '67082' );
   Result := (RetCmd[3] = '1') ;
end;

function TACBrECFICash.GetArredonda: Boolean;
begin
  Result := True ;
end;

procedure TACBrECFICash.LeituraX ;
begin
  EnviaComando( '510', 50 ) ;
end;

procedure TACBrECFICash.LeituraXSerial(Linhas: TStringList);
 Var RetCmd : AnsiString ;
begin
  fsLeDadosSerial := True ;
  RetCmd := EnviaComando('650', 20) ;

  RetCmd := LimpaStr( RetCmd ) ;  { Troca #0 dentro da String por espaços }
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

procedure TACBrECFICash.ReducaoZ(DataHora : TDateTime) ;
 Var Cmd     : AnsiString ;
     Ajuste  : Integer ;
     DtHrECF : TDateTime ;
begin
  Cmd := '72' ;
  if DataHora <> 0 then
   begin
     DtHrECF := GetDataHora ;
     Ajuste  := min(SecondsBetween(DataHora,DtHrECF),5) ;

     if Ajuste = 0 then
        Cmd := Cmd + '00'
     else
      begin
        if DtHrECF < DataHora then
           Cmd := Cmd + '0'
        else
           Cmd := Cmd + '1' ;

        Cmd := Cmd + IntToStr(Ajuste) ;
      end ;
   end
  else
     Cmd := Cmd + '00' ;

  EnviaComando( Cmd, 50 );
end;

procedure TACBrECFICash.AbreGaveta ;
begin
  EnviaComando( '45' );
end;

procedure TACBrECFICash.MudaHorarioVerao ;
begin
  EnviaComando( '71', 3 ) ;
end;

procedure TACBrECFICash.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  if EHorarioVerao <> HorarioVerao then
     MudaHorarioVerao ;
end;

procedure TACBrECFICash.AbreCupom ;
begin
  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }
//  TODO: if not Assigned( fpAliquotas ) then
//     CarregaAliquotas ;
  EnviaComando('200', 10 ) ;
  EnviaConsumidor ;
end;

procedure TACBrECFICash.EnviaConsumidor ;
 Var Doc : String ;
begin
  if (not Consumidor.Enviado) and (Consumidor.Documento <> '') then
  begin
     Doc := Trim(OnlyNumber( Consumidor.Documento )) ;
     if Length(Doc) > 11 then
        Doc := copy(Doc,1,2) + '.' + copy(Doc,3,3) + '.' +
               copy(Doc,6,3) + '/' + copy(Doc,9,4) + '-' + copy(Doc,13,2) +
               StringOfChar( ' ', 14 )
     else
        Doc := StringOfChar( ' ', 18 ) +
               copy(Doc,1,3) + '.' + copy(Doc,4 ,3) + '.' +
               copy(Doc,7,3) + '-' + copy(Doc,10,2) ;

     EnviaComando('2C' + PadRight(Consumidor.Nome,30) +
                         PadRight(Consumidor.Endereco,79) +  Doc, 5 ) ;
     Consumidor.Enviado := True ;
  end ;
end ;

procedure TACBrECFICash.CancelaCupom(NumCOOCancelar: Integer);
begin
// TODO: Cancelar CDC

   If Estado = estLivre Then
      EnviaComando( '2A', 20 )  // Cancela Cupom Anterior
   Else
      EnviaComando( '29', 8 ); // Cancela Cupom Atual
end;

procedure TACBrECFICash.VendeItem(Codigo, Descricao : String ;
   AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
   ValorDescontoAcrescimo : Double ; Unidade : String ;
   TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
   CodDepartamento : Integer) ;
Var QtdStr, ValorStr, DescontoStr : String ;
    RetCMD : AnsiString;
    NumItem, NumDecimais : Integer;
begin
   if Qtd > 9999 then
      raise EACBrECFCMDInvalido.Create( ACBrStr(
           'Quantidade deve ser inferior a 9999.'));

   Codigo    := PadRight(Codigo,14) ;    { Ajustando Tamanhos }
   Descricao := PadRight(Descricao,28);
   Unidade   := PadRight(Unidade,3) ;
   QtdStr    := IntToStrZero( Round( Qtd*1000 ), 7 );

   if RoundTo( ValorUnitario, -2 ) = ValorUnitario then
      NumDecimais := 2
   else
      NumDecimais := 3;
   ValorStr := IntToStrZero( Round( ValorUnitario * Power(10,NumDecimais)), 8 ) ;

  RetCMD := EnviaComando( '21' + Codigo + Descricao + QtdStr + Unidade +
                          ValorStr + IntToStr(NumDecimais) + AliquotaECF, 7 );

  If ValorDescontoAcrescimo > 0 then
  begin
     NumItem := StrToInt( copy(RetCMD, 1, 3) ) ;
     if TipoDescontoAcrescimo = '%' then
      begin
        if DescontoAcrescimo = 'D' then
           DescontoStr := '1'
        else
           DescontoStr := '3' ;
        DescontoStr := DescontoStr +
                       IntToStrZero( Round(ValorDescontoAcrescimo * 100), 4) +
                       StringOfChar('0',12) ;
      end
     else
      begin
        if DescontoAcrescimo = 'D' then
           DescontoStr := '2'
        else
           DescontoStr := '4' ;
        DescontoStr := DescontoStr + '0000' +
                       IntToStrZero( Round(ValorDescontoAcrescimo*100), 12) ;
      end ;

     EnviaComando( '26' + DescontoStr + IntToStrZero( NumItem, 3 ), 4  );
  end ;
end;

procedure TACBrECFICash.CancelaItemVendido(NumItem: Integer);
begin
  EnviaComando( '25' + IntToStrZero(NumItem,3), 10 ) ;
end;

procedure TACBrECFICash.SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString = '' ) ;
 Var DescontoStr, cTipoDesc : String;
begin
   DescontoStr := '';
   cTipoDesc   := '';
   if DescontoAcrescimo <> 0 then
   begin
      if DescontoAcrescimo < 0 Then
         cTipoDesc := '2'
      else
         cTipoDesc := '4';
      DescontoStr := IntToStrZero( Round( Abs( DescontoAcrescimo ) * 100), 12 );

      EnviaComando( '27' + cTipoDesc + '0000' + DescontoStr );
   end;
end;

procedure TACBrECFICash.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
begin
{  TODO: Mecanismo de vinculados
  if ImprimeVinculado then
     if fsVinculado then
        raise EACBrECFERRO.Create(ACBrStr('Já existe Forma de Pagamento com '+#10+
                               'comprovante NAO fiscal vinculado pendente. '+#10+
                               'Impressora: '+fpModeloStr+' aceita apenas um '+#10+
                               'Comprovante não Fiscal Viculado por Cupom.'))
     else
        fsVinculado := true ;
}

  EnviaComando( '22' + CodFormaPagto + IntToStrZero( Round(Valor*100) ,13) );
end;

procedure TACBrECFICash.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
Var Linhas    : TStringList ;
    I         : Integer ;
    Obs1, Obs2: String ;
begin
  Observacao := AjustaLinhas( Observacao, Colunas, 8 );
  Obs1       := '' ;
  Obs2       := '' ;

  Linhas := TStringList.Create ;
  try
     Linhas.Text := Observacao ;

     if Linhas.Count > 0 then
        for I := 0 to 3 do
           if  I < Linhas.Count then
              Obs1 := Obs1 + '0' + PadRight( Linhas[I] , 48)
           else
              Obs1 := Obs1 + '0' + PadRight( ' ' , 48) ;

     if Linhas.Count > 4 then
        for I := 4 to 7 do
           if  I < Linhas.Count then
              Obs2 := Obs2 + '0' + PadRight( Linhas[I] , 48)
           else
              Obs2 := Obs2 + '0' + PadRight( ' ' , 48) ;
  finally
     Linhas.Free ;
  end ;

  if Obs1 <> '' then
     EnviaComando( '23' + Obs1, 6 ); // Observacoes 4 Linhas
  if Obs2 <> '' then
     EnviaComando( '23' + Obs2, 6 ); // Observacoes 4 Linhas

  EnviaConsumidor ;

  EnviaComando( '17', 5 ) ; // Fechando Cupom
  fsVinculado := false
end;

procedure TACBrECFICash.CarregaAliquotas;
Var RetCmd   : AnsiString ;
    Aliquota : TACBrECFAliquota ;
    ValAliq  : Double ;
    A        : Integer ;
begin
  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  try
     For A := 0 to 16 do
     begin
        RetCmd  := EnviaComando( '67' + IntToStrZero(A+19,3) ) ;
//      E #2e7T02,15%0000000000000#3#4
//      E #2e7 **,**%0000000000000#3#4*
        ValAliq := StringToFloatDef( Copy( RetCmd, 2, 5) ,0 ) ;

        if pos( Copy( RetCmd, 1, 1), 'TS') > 0 then
        begin
           Aliquota := TACBrECFAliquota.create ;
           Aliquota.Indice   := RetCMD[1] + IntToStrZero(A+1,2) ; // T01, T02, S03, ...
           Aliquota.Aliquota := ValAliq ;
           Aliquota.Tipo     := Char( RetCMD[1] );
           Aliquota.Total    := RoundTo( StrToFloatDef( copy( RetCmd,8,13), 0 )/100,-2) ;
           fpAliquotas.Add( Aliquota ) ;
        end;
     end ;
  except
     fpAliquotas.Free ;
     fpAliquotas := nil ;
     raise ;
  end ;
end;

procedure TACBrECFICash.LerTotaisAliquota;
begin
  CarregaAliquotas ;
end;

procedure TACBrECFICash.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String);
begin
   raise EACBrECFERRO.Create( ACBrStr('ECF '+fpModeloStr+' só permite programação de Aliquotas'+
                           ' em Estado de Intervenção técnica.')) ;
end;

function TACBrECFICash.AchaICMSAliquota( var AliquotaICMS: String):
   TACBrECFAliquota;
  Var AliquotaStr : String ;
begin
  AliquotaStr := '' ;
  Result      := nil ;

   if copy(AliquotaICMS,1,2) = 'TT' then { Corrige Duplo T  }
     AliquotaICMS := copy(AliquotaICMS,2,7) ;

   case AliquotaICMS[1] of
      'I' : AliquotaStr := 'I1' ;
      'N' : AliquotaStr := 'N1' ;
      'F' : AliquotaStr := 'F1' ;
      'T' : AliquotaICMS := 'TT'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}
      'S' : AliquotaICMS := 'TS'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}
  end ;

  if AliquotaStr = '' then
   begin
     Result := inherited AchaICMSAliquota( AliquotaICMS ) ;
     if Result <> nil then
        AliquotaICMS := Result.Tipo +
                        StringReplace( FormatFloat('00.00', Result.Aliquota),
                                            DecimalSeparator,',',[]) + '%' ;
   end
  else
     AliquotaICMS := PadRight( AliquotaStr, 7) ;
end;


procedure TACBrECFICash.CarregaFormasPagamento;  
Var RetCmd, Descricao : AnsiString ;
    Cont : Integer ;
    FPagto : TACBrECFFormaPagamento ;
begin
  inherited CarregaFormasPagamento ;       {Inicializa fpFormasPagamentos}

  try
     for Cont := 0 to 9 do
     begin
        RetCmd := EnviaComando( '67' + IntToStrZero(65 + Cont,3) ) ;

        Descricao := Trim( copy(RetCmd, 16,15) ) ;
        if Descricao <> '' then
        begin
           FPagto := TACBrECFFormaPagamento.create ;

           FPagto.Indice    := IntToStrZero(Cont+1,2) ;
           FPagto.Descricao := Descricao ;
           FPagto.Total     := RoundTo( StrToFloatDef( copy(RetCmd,1,14), 0 ) / 100, -2) ;
           FPagto.PermiteVinculado := (copy(RetCmd,31,1)='1') ;

           fpFormasPagamentos.Add( FPagto ) ;
        end ;
     end ;
  except
     fpFormasPagamentos.Free ;
     fpFormasPagamentos := nil ;

     raise ;
  end ;
end;

procedure TACBrECFICash.LerTotaisFormaPagamento;
begin
  CarregaFormasPagamento ;
end;

procedure TACBrECFICash.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String );
 Var Vinc : AnsiChar ;
begin
  if Posicao = '' then
  begin
     CarregaFormasPagamento ;
     Posicao := IntToStrZero( fpFormasPagamentos.Count+1, 2 ) ;
  end ;

  if PermiteVinculado then
     Vinc := '1'
  else
     Vinc := '0' ;

  EnviaComando('0A' + Posicao + PadRight(Descricao,15) + Vinc ) ;
  CarregaFormasPagamento ;
end;

procedure TACBrECFICash.CarregaComprovantesNaoFiscais;
Var RetCmd, Descricao : AnsiString ;
    Cont : Integer ;
    CNF : TACBrECFComprovanteNaoFiscal ;
begin
  inherited CarregaComprovantesNaoFiscais ;{Inicializa fpComprovantesNaoFiscais}

  try
     for Cont := 0 to 19 do
     begin
        RetCmd := EnviaComando( '67' + IntToStrZero(45 + Cont,3) ) ;

        Descricao := Trim(copy(RetCmd,19,15)) ;
        if Descricao <> '' then
        begin
           CNF := TACBrECFComprovanteNaoFiscal.create ;
           CNF.Indice    := IntToStrZero(Cont+1,2) ;
           CNF.Descricao := Descricao ;
           CNF.Total     := RoundTo( StrToFloatDef(copy(RetCmd,1,13),0) /100, -2 ) ;
           CNF.Contador  := StrToIntDef( copy(RetCmd,15,4), 0 ) ;

           fpComprovantesNaoFiscais.Add( CNF ) ;
        end ;
      end ;
  except
     fpComprovantesNaoFiscais.Free ;
     fpComprovantesNaoFiscais := nil ;

     raise ;
  end ;
end;

procedure TACBrECFICash.LerTotaisComprovanteNaoFiscal;
begin
  CarregaComprovantesNaoFiscais ;
end;

procedure TACBrECFICash.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String );
begin
  if Posicao = '' then
  begin
     CarregaComprovantesNaoFiscais ;
     Posicao := IntToStrZero( fpComprovantesNaoFiscais.Count+1, 2 ) ;
  end ;

  if Tipo = '' then
     Tipo := '+' ;

  if Tipo = '+' then
     Tipo := '1'
  else
     Tipo := '2' ;

  EnviaComando( '09' + Posicao + Tipo + PadRight( Descricao, 15) ) ;
end;


procedure TACBrECFICash.AbreRelatorioGerencial(Indice : Integer) ;
begin
   EnviaComando( '5601', 50 ) ;
end;

procedure TACBrECFICash.LinhaRelatorioGerencial(Linha: AnsiString;IndiceBMP: Integer);
begin
  ImprimirLinhaALinha(Linha, '570' );
end;

procedure TACBrECFICash.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
begin
  EnviaComando('47' + CodFormaPagto + '01' + IntToStrZero(Round(Valor*100),13)+COO)
end;

procedure TACBrECFICash.LinhaCupomVinculado(Linha: AnsiString);
begin
  ImprimirLinhaALinha(Linha, '420' );
end;

procedure TACBrECFICash.FechaRelatorio;
begin
  if Estado = estRelatorio then
  begin
     if ST1 = 66 then
        EnviaComando( '58' )
     else
        EnviaComando( '43' ) ;
  end ;
end;

procedure TACBrECFICash.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : Char ;
begin
  Espera := 60 + (ReducaoFinal - ReducaoInicial) ;
  Flag   := '0' ;
  if Simplificada then
     Flag := '1' ;

  EnviaComando( '54'+IntToStrZero(ReducaoInicial,4) +
                     IntToStrZero(ReducaoFinal  ,4) + Flag, Espera ) ;
end;

procedure TACBrECFICash.LeituraMemoriaFiscal(DataInicial, DataFinal: TDateTime;
   Simplificada : Boolean);
  Var Espera : Integer ;
      Flag   : Char ;
begin
  Flag   := '0' ;
  if Simplificada then
     Flag := '1' ;

  Espera := 60 + DaysBetween(DataInicial,DataFinal) ;
  EnviaComando( '55'+FormatDateTime('ddmmyyyy',DataInicial) +
                     FormatDateTime('ddmmyyyy',DataFinal)   +  Flag, Espera ) ;
end;

procedure TACBrECFICash.LeituraMemoriaFiscalSerial(DataInicial,
  DataFinal: TDateTime; Linhas: TStringList; Simplificada : Boolean);
 Var RetCmd : AnsiString ;
begin
  { ICash não permite informar Inicio e Fim na leitura pela Serial, nem
    Tipo de Relatorio (Normal, Simplificado) }
  fsLeDadosSerial := True ;
  RetCmd := EnviaComando('660', 20) ;

  RetCmd := LimpaStr( RetCmd ) ;  { Troca #0 dentro da String por espaços }
  Linhas.Clear ;
  Linhas.Text := RetCmd ;
end;

procedure TACBrECFICash.LeituraMemoriaFiscalSerial(ReducaoInicial,
  ReducaoFinal: Integer; Linhas: TStringList; Simplificada : Boolean);
begin
  LeituraMemoriaFiscalSerial(Now, Now, Linhas);
end;

procedure TACBrECFICash.IdentificaOperador(Nome: String);
 Var RetCmd : AnsiString ;
begin
   { Lendo operador atual }
   RetCmd := UpperCase(Trim(EnviaComando('67090'))) ;

   if UpperCase(Trim(Nome)) <> RetCmd then
      EnviaComando( '70'+PadRight(Nome,10) ) ;
end;

procedure TACBrECFICash.PulaLinhas(NumLinhas: Integer);
begin
  EnviaComando('44'+IntToStrZero(min(NumLinhas,99),2))
end;

function TACBrECFICash.GetCNPJ: String;
begin
  Result := EnviaComando('67094') ;
end;

function TACBrECFICash.GetIE: String;
begin
  Result := EnviaComando('67095') ;
end;

function TACBrECFICash.GetDataMovimento: TDateTime;
 Var RetCmd : AnsiString ;
     OldShortDateFormat : String ;
begin
  RetCmd := EnviaComando( '67083' );

  OldShortDateFormat := ShortDateFormat ;
  try
     ShortDateFormat := 'dd/mm/yyyy' ;
     Result := StrToDate(copy(RetCmd, 1,2) + DateSeparator +
                         copy(RetCmd, 3,2) + DateSeparator +
                         copy(RetCmd, 5,4)) ;
  finally
     ShortDateFormat := OldShortDateFormat ;
  end ;
end;

function TACBrECFICash.GetGrandeTotal: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67009' );
  Result := StrToFloatDef( RetCmd, 0 ) / 100;
end;

function TACBrECFICash.GetNumCRZ: String;
begin
  Result := EnviaComando( '67001' );
end;

function TACBrECFICash.GetNumCOOInicial: String;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67002' );
  Result := copy(RetCmd,1,6) ;
end;

function TACBrECFICash.GetNumGNF: String;
begin
  Result := EnviaComando( '67003' );
end;

function TACBrECFICash.GetNumCCF: String;
begin
  Result := EnviaComando( '67004' );
end;

function TACBrECFICash.GetNumGRG: String;
begin
  Result := EnviaComando( '67005' );
end;

function TACBrECFICash.GetNumCDC: String;
begin
  Result := EnviaComando( '67008' );
end;

function TACBrECFICash.GetVendaBruta: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67010' );
  Result := StrToFloatDef( RetCmd, 0 ) / 100 ;
end;

function TACBrECFICash.GetTotalCancelamentos: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67011' );
  Result := StrToFloatDef( RetCmd, 0 ) / 100;
end;

function TACBrECFICash.GetTotalDescontos: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67012' );
  Result := StrToFloatDef( RetCmd, 0 ) / 100;
end;

function TACBrECFICash.GetTotalAcrescimos: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67013' );
  Result := StrToFloatDef( RetCmd, 0 ) / 100;
end;

function TACBrECFICash.GetTotalSubstituicaoTributaria: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67036' );
  Result := StrToFloatDef( copy(RetCmd,8,13), 0 ) / 100 ;

  RetCmd := EnviaComando( '67037' );
  Result := Result + ( StrToFloatDef( copy(RetCmd,8,13), 0 ) / 100 ) ;
end;

function TACBrECFICash.GetTotalIsencao: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67038' );
  Result := StrToFloatDef( copy(RetCmd,8,13), 0 ) / 100 ;

  RetCmd := EnviaComando( '67039' );
  Result := Result + StrToFloatDef( copy(RetCmd,8,13), 0 ) / 100 ;
end;

function TACBrECFICash.GetTotalNaoTributado: Double;
 Var RetCmd : AnsiString ;
begin
  RetCmd := EnviaComando( '67040' );
  Result := StrToFloatDef( copy(RetCmd,8,13), 0 ) / 100 ;

  RetCmd := EnviaComando( '67041' );
  Result := Result + StrToFloatDef( copy(RetCmd,8,13), 0 ) / 100 ;
end;

procedure TACBrECFICash.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  CPF_CNPJ := Trim(CPF_CNPJ) ;
  if CPF_CNPJ <> '' then
     Consumidor.AtribuiConsumidor(CPF_CNPJ, Consumidor.Nome, Consumidor.Endereco );

  EnviaComando('30', 10) ;
  EnviaConsumidor ;
end;

procedure TACBrECFICash.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString);
begin
  EnviaComando('31' + IntToStrZero( Round(Valor*100) ,12) + CodCNF ) ;
end;

procedure TACBrECFICash.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
  MensagemRodape: AnsiString);
 Var DescontoStr, cTipoDesc : String;
begin
   DescontoStr := '';
   cTipoDesc   := '';
   if DescontoAcrescimo <> 0 then
   begin
      if DescontoAcrescimo < 0 Then
         cTipoDesc := '2'
      else
         cTipoDesc := '4';
      DescontoStr := IntToStrZero( Round( Abs( DescontoAcrescimo ) * 100), 12 );

      EnviaComando( '37' + cTipoDesc + '0000' + DescontoStr );
   end;
end;

procedure TACBrECFICash.EfetuaPagamentoNaoFiscal(CodFormaPagto: String;
  Valor: Double; Observacao: AnsiString; ImprimeVinculado: Boolean);
begin
  EnviaComando('32' + CodFormaPagto + IntToStrZero( Round(Valor*100) ,13) ) ;
end;

procedure TACBrECFICash.CancelaNaoFiscal;
begin
   if Estado = estNaoFiscal then
      EnviaComando( '38', 16 )   // Cancela N.Fiscal Atual
   else
      EnviaComando( '39', 16 );  // Cancela N.Fiscal Anterior
end;

procedure TACBrECFICash.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
Var Linhas    : TStringList ;
    I         : Integer ;
    Obs1, Obs2: String ;
begin
  Observacao := AjustaLinhas( Observacao, Colunas, 8 );
  Obs1       := '' ;
  Obs2       := '' ;

  Linhas := TStringList.Create ;
  try
     Linhas.Text := Observacao ;

     if Linhas.Count > 0 then
        for I := 0 to 3 do
           if  I < Linhas.Count then
              Obs1 := Obs1 + '0' + PadRight( Linhas[I] , 48)
           else
              Obs1 := Obs1 + '0' + PadRight( ' ' , 48) ;

     if Linhas.Count > 4 then
        for I := 4 to 7 do
           if  I < Linhas.Count then
              Obs2 := Obs2 + '0' + PadRight( Linhas[I] , 48)
           else
              Obs2 := Obs2 + '0' + PadRight( ' ' , 48) ;
  finally
     Linhas.Free ;
  end ;

  if Obs1 <> '' then
     EnviaComando( '33' + Obs1, 6 ); // Observacoes 4 Linhas
  if Obs2 <> '' then
     EnviaComando( '33' + Obs2, 6 ); // Observacoes 4 Linhas

  EnviaConsumidor ;

  EnviaComando( '18', 5 ) ; // Fechando N.Fiscal
  fsVinculado := false
end;

procedure TACBrECFICash.Sangria( const Valor: Double; Obs: AnsiString;
   DescricaoCNF, DescricaoFPG: String; IndiceBMP: Integer ) ;
 Var CNF : TACBrECFComprovanteNaoFiscal ;
     FPG : TACBrECFFormaPagamento ;
     Linhas    : TStringList ;
     I         : Integer ;
     ObsStr    : String ;
begin
  CNF := AchaCNFDescricao(DescricaoCNF, True) ;
  if CNF = nil then
     raise EACBrECFERRO.Create(ACBrStr('Não existe nenhum Comprovante Não Fiscal '+
                            'cadastrado como: "'+DescricaoCNF+'"')) ;

  FPG := AchaFPGDescricao(DescricaoFPG, True) ;
  if FPG = nil then
     raise EACBrECFERRO.Create(ACBrStr('Não existe nenhuma Forma de Pagamento '+
                            'cadastrada como: "'+DescricaoFPG+'"')) ;

  Obs    := AjustaLinhas( Obs, Colunas, 8 );
  ObsStr := '' ;

  Linhas := TStringList.Create ;
  try
     Linhas.Text := Obs ;

     if Linhas.Count > 0 then
        for I := 0 to 3 do
           if  I < Linhas.Count then
              ObsStr := ObsStr + '0' + PadRight( Linhas[I] , 48)
           else
              ObsStr := ObsStr + '0' + PadRight( ' ' , 48) ;
  finally
     Linhas.Free ;
  end ;

  EnviaComando('41' + CNF.Indice + FPG.Indice +
                      IntToStrZero(Round(Valor*100),13) + ObsStr, 15 ) ;
end;
procedure TACBrECFICash.Suprimento(const Valor : Double ; Obs : AnsiString ;
   DescricaoCNF : String ; DescricaoFPG : String ; IndiceBMP : Integer) ;
begin
  if UpperCase(Trim(DescricaoCNF)) = 'SUPRIMENTO' then
     DescricaoCNF := 'FUNDO DE CAIXA' ;

  Sangria(Valor, Obs, DescricaoCNF, DescricaoFPG, IndiceBMP);
end;


function TACBrECFICash.LimpaStr(const AString: AnsiString): AnsiString;
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

end.


