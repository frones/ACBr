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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/08/2011: Márcio D. Carvalho
|*  - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrTEFDCliDTEF;

interface

uses
  Classes, SysUtils, ACBrTEFDClass
  {$IFNDEF NOGUI}
  {$IFDEF VisualCLX}
     ,QForms, QControls
  {$ELSE}
    {$IFDEF FMX} ,System.UITypes {$ENDIF}  ,Forms, Controls
  {$ENDIF}
  {$ENDIF};


Const
   CACBrTEFD_CliDTEF_Backup = 'ACBr_CliDTEF_Backup.tef' ;

{$IFDEF LINUX}
  CACBrTEFD_CliDTEF_Lib = '' ;
{$ELSE}
  CACBrTEFD_CliDTEF_Lib = 'DPOSDRV.DLL' ;
{$ENDIF}

type
  { TACBrTEFDRespCliDTEF }

  TACBrTEFDRespCliDTEF = class( TACBrTEFDResp )
  protected
    function GetTransacaoAprovada : Boolean; override;
  public
    procedure ConteudoToProperty; override;
    procedure GravaInformacao( const Identificacao : Integer;
      const Informacao : AnsiString );
  end;

  TACBrTEFDCliDTEFExibeMenu = procedure( Titulo : String; Opcoes : TStringList;
    var ItemSelecionado : Integer; var VoltarMenu : Boolean ) of object ;

  TACBrTEFDCliDTEFObtemInformacao = procedure( var ItemSelecionado : Integer ) of object ;

  { TACBrTEFDCliDTEF }

   TACBrTEFDCliDTEF = class( TACBrTEFDClass )
   private
      fArqResp : string;
      fNumeroTerminal : AnsiString;
      fParametrosAdicionais : TStringList;
      fRespostas: TStringList;
      fDocumentosProcessados : AnsiString ;
      fOnExibeMenu : TACBrTEFDCliDTEFExibeMenu;
      fOnObtemInformacao : TACBrTEFDCliDTEFObtemInformacao;

      xIdentificacaoAutomacaoComercial : function (
              pFabricanteAutomacao,
              pVersaoAutomacao,
              pReservado: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

     xTransacaoResgatePremio : function (
              pNumeroCupom,
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoCheque : function (
              pValorTransacao,
              pNumeroCupomVenda,
              pNumeroControle,
              pQuantidadeCheques,
              pPeriodicidadeCheques,
              pDataPrimeiroCheque,
              pCarenciaPrimeiroCheque: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoCartaoCredito : function (
              pValorTransacao,
              pNumeroCupomVenda,
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xConfirmaCartaoCredito : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoCartaoDebito : function (
              pValorTransacao,
              pNumeroCupomVenda,
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xConfirmaCartaoDebito : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoCartaoVoucher : function (
              pValorTransacao,
              pNumeroCupomVenda,
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xConfirmaCartaoVoucher : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoCartaoFrota : function (
              pValorTransacao,
              pNumeroCupomVenda,
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xConfirmaCartaoFrota : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoCancelamentoPagamento : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoPreAutorizacaoCartaoCredito : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoConsultaParcelas : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoResumoVendas : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xTransacaoReimpressaoCupom : function : Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xConfirmaCartao : function (
              pNumeroControle: PAnsiChar): Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xFinalizaTransacao : function : Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xObtemLogUltimaTransacao : procedure (
              oLogUltimaTransacao: PAnsiChar);
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xInicializaDPOS : function : Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

      xFinalizaDPOS : function : Integer;
              {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;


     procedure FinalizarTransacao(Operacao : AnsiString; Confirma : Boolean;
        NSU : AnsiString; DocumentoVinculado : AnsiString);
     procedure LoadDLLFunctions;
     procedure MontaArquivoResposta(aNSU: String; aRetorno:TStringList);
     procedure ImprimirComprovantes(SL : TStringList);
   protected
     procedure SetArqResp(const AValue : String);
     Function FazerRequisicao( Funcao : Integer;  AHeader : AnsiString = '';
       Valor : Double = 0; Documento : AnsiString = ''; QuantidadeCheques : AnsiString = '';
       PeriodicidadeCheques : AnsiString = ''; DataPrimeiroCheque : AnsiString = '';
       CarenciaPrimeiroCheque : AnsiString = '') : Integer ;
     Function ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean; override;

   public
     property Respostas : TStringList read fRespostas ;

     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     procedure Inicializar ; override;
     procedure DesInicializar ; override;

     procedure AtivarGP ; override;
     procedure VerificaAtivo ; override;

     procedure ConfirmarEReimprimirTransacoesPendentes ;

     Function ADM : Boolean ; override;
     Function CRT( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; override;
     Function CHQ( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; CMC7 : String = '';
        TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
        DataCheque : TDateTime = 0; Banco   : String = '';
        Agencia    : String = ''; AgenciaDC : String = '';
        Conta      : String = ''; ContaDC   : String = '';
        Cheque     : String = ''; ChequeDC  : String = '';
        Compensacao: String = '' ) : Boolean ; override;
     Procedure NCN(Rede, NSU, Finalizacao : String;
        Valor : Double = 0; DocumentoVinculado : String = ''); override;
     Procedure CNF(Rede, NSU, Finalizacao : String;
        DocumentoVinculado : String = ''); override;
     Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
        Valor : Double) : Boolean; overload; override;
   published
     property NumVias ;
     property ArqResp  : String read fArqResp   write SetArqResp ;
     property NumeroTerminal : AnsiString read fNumeroTerminal write fNumeroTerminal ;
     property OnExibeMenu : TACBrTEFDCliDTEFExibeMenu read fOnExibeMenu write fOnExibeMenu ;
     property OnObtemInformacao : TACBrTEFDCliDTEFObtemInformacao read fOnObtemInformacao write fOnObtemInformacao ;
   end;

implementation

Uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  DateUtils, Math, StrUtils,
  ACBrTEFD, ACBrUtil;

{ TACBrTEFDRespCliDTEF }

function TACBrTEFDRespCliDTEF.GetTransacaoAprovada : Boolean;
begin
   Result := True ;
end;

procedure TACBrTEFDRespCliDTEF.ConteudoToProperty;
var
   Linha : TACBrTEFDLinha ;
   I     : Integer;
   Parc  : TACBrTEFDRespParcela;
   LinStr: AnsiString ;
begin
   fpValorTotal := 0 ;
   fpImagemComprovante1aVia.Clear;
   fpImagemComprovante2aVia.Clear;

   for I := 0 to Conteudo.Count - 1 do
   begin
     Linha  := Conteudo.Linha[I];
     LinStr := StringToBinaryString( Linha.Informacao.AsString );

     case Linha.Identificacao of
       // TODO: Mapear mais propriedades do CliSiTef //
       100 :fpModalidadePagto              := LinStr;
       101 :fpModalidadePagtoExtenso       := LinStr;
       102 :fpModalidadePagtoDescrita      := LinStr;
       105 :
         begin
           fpDataHoraTransacaoComprovante  := Linha.Informacao.AsTimeStampSQL;
           fpDataHoraTransacaoHost         := fpDataHoraTransacaoComprovante ;
         end;
       121 : fpImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString );
       122 : fpImagemComprovante2aVia.Text := StringToBinaryString( Linha.Informacao.AsString );
       130 :
         begin
           fpSaque      := Linha.Informacao.AsFloat ;
           fpValorTotal := fpValorTotal + fpSaque ;
         end;
       131 : fpInstituicao                 := LinStr;
       133 : fpCodigoAutorizacaoTransacao  := Linha.Informacao.AsString;
       134 : fpNSU                         := Linha.Informacao.AsString;

       135 :
        begin
         fpDesconto   := Linha.Informacao.AsFloat;
         fpValorTotal := fpValorTotal - fpDesconto ;
        end;

       //156 : fpRede                      := LinStr;
       501 : fpTipoPessoa                  := AnsiChar(IfThen(Linha.Informacao.AsInteger = 0,'J','F')[1]);
       502 : fpDocumentoPessoa             := LinStr ;
       505 : fpQtdParcelas                 := Linha.Informacao.AsInteger ;
       506 : fpDataPreDatado               := Linha.Informacao.AsDate;
       627 : fpAgencia                     := LinStr;
       628 : fpAgenciaDC                   := LinStr;
       120 : fpAutenticacao                := LinStr;
       626 : fpBanco                       := LinStr;
       613 :
        begin
          fpCheque                         := copy(LinStr, 21, 6);
          fpCMC7                           := LinStr;
        end;
       629 : fpConta                       := LinStr;
       630 : fpContaDC                     := LinStr;
       527 : fpDataVencimento              := Linha.Informacao.AsDate ; {Data Vencimento}

       //

       899 :  // Tipos de Uso Interno do ACBrTEFD
        begin
          case Linha.Sequencia of
              1 : fpCNFEnviado         := (UpperCase( Linha.Informacao.AsString ) = 'S' );
              2 : fpIndiceFPG_ECF      := Linha.Informacao.AsString ;
              3 : fpOrdemPagamento     := Linha.Informacao.AsInteger ;
            100 : fpHeader             := LinStr;
            101 : fpID                 := Linha.Informacao.AsInteger;
            102 : fpDocumentoVinculado := LinStr;
            103 : fpValorTotal         := fpValorTotal + Linha.Informacao.AsFloat;
            104 : fpRede               := Linha.Informacao.AsString ;
            130 : fpTextoEspecialOperador := Linha.Informacao.AsString;            
          end;
        end;
     end;
   end ;

   fpParcelas.Clear;
   for I := 1 to fpQtdParcelas do
   begin
      Parc := TACBrTEFDRespParcela.create;
      Parc.Vencimento := LeInformacao( 141, I).AsDate ;
      Parc.Valor      := LeInformacao( 142, I).AsFloat ;

      fpParcelas.Add(Parc);
   end;
end;

procedure TACBrTEFDRespCliDTEF.GravaInformacao(const Identificacao : Integer;
   const Informacao : AnsiString);
begin
  fpConteudo.GravaInformacao( Identificacao, 0,
                              BinaryStringToString(Informacao) ); // Converte #10 para "\x0A"
end;

{ TACBrTEFDClass }

constructor TACBrTEFDCliDTEF.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ArqReq    := '' ;
  ArqResp   := '' ;
  ArqSTS    := '' ;
  ArqTemp   := '' ;
  GPExeName := '' ;
  fpTipo    := gpCliDTEF;
  Name      := 'CliDTEF' ;

  fDocumentosProcessados := '' ;

  fParametrosAdicionais := TStringList.Create;
  fRespostas            := TStringList.Create;

  xIdentificacaoAutomacaoComercial := nil;
  xTransacaoCheque := nil;
  xTransacaoCartaoCredito := nil;
  xConfirmaCartaoCredito := nil;
  xTransacaoCartaoDebito := nil;
  xConfirmaCartaoDebito := nil;
  xTransacaoCartaoVoucher := nil;
  xConfirmaCartaoVoucher := nil;
  xTransacaoCartaoFrota := nil;
  xConfirmaCartaoFrota := nil;
  xTransacaoCancelamentoPagamento := nil;
  xTransacaoPreAutorizacaoCartaoCredito := nil;
  xTransacaoConsultaParcelas := nil;
  xTransacaoResumoVendas := nil;
  xTransacaoReimpressaoCupom := nil;
  xConfirmaCartao := nil;
  xFinalizaTransacao := nil;
  xObtemLogUltimaTransacao := nil;
  xInicializaDPOS := nil;
  xFinalizaDPOS := nil;
  xTransacaoResgatePremio := nil;

  if Assigned( fpResp ) then
     fpResp.Free ;

  fpResp := TACBrTEFDRespCliDTEF.Create;
  fpResp.TipoGP := Tipo;
end;

destructor TACBrTEFDCliDTEF.Destroy;
begin
   fParametrosAdicionais.Free ;
   fRespostas.Free ;

   inherited Destroy;
end;

procedure TACBrTEFDCliDTEF.LoadDLLFunctions ;
 procedure CliDTEFFunctionDetect( FuncName: AnsiString; var LibPointer: Pointer ) ;
 begin
   if not Assigned( LibPointer )  then
   begin
     if not FunctionDetect( CACBrTEFD_CliDTEF_Lib, FuncName, LibPointer) then
     begin
        LibPointer := NIL ;
        raise EACBrTEFDErro.Create( ACBrStr( 'Erro ao carregar a função:'+FuncName+
                                         ' de: '+CACBrTEFD_CliDTEF_Lib ) ) ;
     end ;
   end ;
 end ;
begin
   CliDTEFFunctionDetect('IdentificacaoAutomacaoComercial', @xIdentificacaoAutomacaoComercial);
   CliDTEFFunctionDetect('TransacaoResgatePremio', @xTransacaoResgatePremio);
   CliDTEFFunctionDetect('TransacaoCheque', @xTransacaoCheque);
   CliDTEFFunctionDetect('TransacaoCartaoCredito', @xTransacaoCartaoCredito);
   CliDTEFFunctionDetect('ConfirmaCartaoCredito', @xConfirmaCartaoCredito);
   CliDTEFFunctionDetect('TransacaoCartaoDebito',@xTransacaoCartaoDebito);
   CliDTEFFunctionDetect('ConfirmaCartaoDebito',@xConfirmaCartaoDebito);
   CliDTEFFunctionDetect('TransacaoCartaoVoucher',@xTransacaoCartaoVoucher);
   CliDTEFFunctionDetect('ConfirmaCartaoVoucher',@xConfirmaCartaoVoucher);
   CliDTEFFunctionDetect('TransacaoCartaoFrota',@xTransacaoCartaoFrota);
   CliDTEFFunctionDetect('ConfirmaCartaoFrota',@xConfirmaCartaoFrota);
   CliDTEFFunctionDetect('TransacaoCancelamentoPagamento',@xTransacaoCancelamentoPagamento);
   CliDTEFFunctionDetect('TransacaoPreAutorizacaoCartaoCredito',@xTransacaoPreAutorizacaoCartaoCredito);
   CliDTEFFunctionDetect('TransacaoConsultaParcelas',@xTransacaoConsultaParcelas);
   CliDTEFFunctionDetect('TransacaoResumoVendas',@xTransacaoResumoVendas);
   CliDTEFFunctionDetect('TransacaoReimpressaoCupom',@xTransacaoReimpressaoCupom);
   CliDTEFFunctionDetect('ConfirmaCartao',@xConfirmaCartao);
   CliDTEFFunctionDetect('FinalizaTransacao',@xFinalizaTransacao);
   CliDTEFFunctionDetect('ObtemLogUltimaTransacao',@xObtemLogUltimaTransacao);
   CliDTEFFunctionDetect('InicializaDPOS',@xInicializaDPOS);
   CliDTEFFunctionDetect('FinalizaDPOS',@xFinalizaDPOS);
end ;

procedure TACBrTEFDCliDTEF.SetArqResp(const AValue : String);
begin
  fArqResp := Trim( AValue ) ;
end;

procedure TACBrTEFDCliDTEF.Inicializar;
var
  pFabricanteAutomacao, pVersaoAutomacao, pReservado: AnsiString;
begin
  if Inicializado then exit ;

  if not Assigned( OnExibeMenu ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnExibeMenu" não programado' ) ) ;

  if not Assigned( OnObtemInformacao ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnObtemInformacao" não programado' ) ) ;

  LoadDLLFunctions;

  xInicializaDPOS;

  pFabricanteAutomacao := TACBrTEFD(Owner).Identificacao.NomeAplicacao;
  pVersaoAutomacao := TACBrTEFD(Owner).Identificacao.VersaoAplicacao;
  pReservado := '10';

  xIdentificacaoAutomacaoComercial( PAnsiChar( pFabricanteAutomacao ),
                           PAnsiChar( pVersaoAutomacao ),
                           PAnsiChar( pReservado ));

  fpInicializado := True ;
  GravaLog( Name +' Inicializado CliDTEF' );

  VerificarTransacoesPendentesClass(True);
end;

procedure TACBrTEFDCliDTEF.DesInicializar;
begin
  xFinalizaDPOS;
  fpInicializado := False ;
  GravaLog( Name +' DesInicializado' );
end;

procedure TACBrTEFDCliDTEF.AtivarGP;
begin
   raise EACBrTEFDErro.Create( ACBrStr( 'CliDTEF não pode ser ativado localmente' )) ;
end;

procedure TACBrTEFDCliDTEF.VerificaAtivo;
begin
   {Nada a Fazer}
end;

procedure TACBrTEFDCliDTEF.ConfirmarEReimprimirTransacoesPendentes;
Var
  ArquivosVerficar : TStringList ;
  ArqMask, NSUs    : AnsiString;
  ExibeMsg         : Boolean ;
begin
  ArquivosVerficar := TStringList.Create;

  try
     ArquivosVerficar.Clear;

     { Achando Arquivos de Backup deste GP }
     ArqMask := TACBrTEFD(Owner).PathBackup + PathDelim + 'ACBr_' + Self.Name + '_*.tef' ;
     FindFiles( ArqMask, ArquivosVerficar, True );
     NSUs := '' ;
     ExibeMsg := (ArquivosVerficar.Count > 0) ;

     { Enviando NCN ou CNC para todos os arquivos encontrados }
     while ArquivosVerficar.Count > 0 do
     begin
        if not FileExists( ArquivosVerficar[ 0 ] ) then
        begin
           ArquivosVerficar.Delete( 0 );
           Continue;
        end;

        Resp.LeArquivo( ArquivosVerficar[ 0 ] );

        try
           if pos(Resp.DocumentoVinculado, fDocumentosProcessados) = 0 then
              //Caso não seja a ultima transação, não deve finalizar a transação(DLL) enviando I = Intermediário.
              CNF(Resp.Rede, Resp.NSU, IfThen(ArquivosVerficar.Count <= 1, '', 'I'), '');   {Confirma}
           if Resp.NSU <> '' then
              NSUs := NSUs + sLineBreak + 'NSU: '+Resp.NSU ;

           SysUtils.DeleteFile( ArquivosVerficar[ 0 ] );
           ArquivosVerficar.Delete( 0 );
        except
        end;
     end;

     if ExibeMsg then
        TACBrTEFD(Owner).DoExibeMsg( opmOK,
                               'Transação TEF efetuada.'+sLineBreak+
                               'Favor Re-Imprimir Ultimo Cupom ' + NSUs ) ;

  finally
     ArquivosVerficar.Free;
  end;
end;

Function TACBrTEFDCliDTEF.ADM : Boolean;
var
  Sts : Integer;
begin
  Result := False ;
  Sts := FazerRequisicao(0, 'ADM', 0, '0' ) ;

  if Sts = 0 then
  begin
     FinalizarTransacao('0', True, '', '');
     Result := True;
  end;
end;

Function TACBrTEFDCliDTEF.CRT( Valor : Double; IndiceFPG_ECF : String;
   DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean;
var
  Sts : Integer;
begin
  VerificarTransacaoPagamento( Valor );

  Sts := FazerRequisicao(0, 'CRT', Valor, DocumentoVinculado ) ;

  ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );

  Result := ( Sts = 0 ) ;
end;

Function TACBrTEFDCliDTEF.CHQ(Valor : Double; IndiceFPG_ECF : String;
   DocumentoVinculado : String; CMC7 : String; TipoPessoa : AnsiChar;
   DocumentoPessoa : String; DataCheque : TDateTime; Banco : String;
   Agencia : String; AgenciaDC : String; Conta : String; ContaDC : String;
   Cheque : String; ChequeDC : String; Compensacao: String) : Boolean ;
var
  Sts : Integer;
begin
  VerificarTransacaoPagamento( Valor );

  Sts := FazerRequisicao(4, 'CHQ', Valor, DocumentoVinculado ) ;

  ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );

  Result := ( Sts = 0 ) ;
end;

Procedure TACBrTEFDCliDTEF.CNF(Rede, NSU, Finalizacao : String;
   DocumentoVinculado : String) ;
var
  Confirma : Boolean ;
  I : Integer;
  RespostasPendentesNaoConfirmadas : Integer;
begin
  Confirma := True;
  RespostasPendentesNaoConfirmadas := 0;

  for I := 0 to TACBrTEFD(Owner).RespostasPendentes.Count - 1 do
  begin
    if not TACBrTEFD(Owner).RespostasPendentes.Objects[I].CNFEnviado then
      Inc( RespostasPendentesNaoConfirmadas );
  end;

  //Caso não seja a ultima transação, não deve finalizar a transação(DLL) enviando I = Intermediário.
  if RespostasPendentesNaoConfirmadas > 1 then
    Finalizacao := 'I' //Transação Intermediaria
  else
    Finalizacao := ''; //Transação Final

  if Finalizacao = 'I' then
    Confirma := False;

  FinalizarTransacao(Rede, Confirma, NSU, DocumentoVinculado);
end;

Function TACBrTEFDCliDTEF.CNC(Rede, NSU : String;
   DataHoraTransacao : TDateTime; Valor : Double) : Boolean;
begin
  FinalizarTransacao(Rede, True, NSU, '');
  Result := True ;
end;

Procedure TACBrTEFDCliDTEF.NCN(Rede, NSU, Finalizacao : String;
   Valor : Double; DocumentoVinculado : String) ;
begin
  xFinalizaTransacao;
end;

Function TACBrTEFDCliDTEF.FazerRequisicao( Funcao : Integer;  AHeader : AnsiString = '';
  Valor : Double = 0; Documento : AnsiString = ''; QuantidadeCheques : AnsiString = '';
  PeriodicidadeCheques : AnsiString = ''; DataPrimeiroCheque : AnsiString = '';
  CarenciaPrimeiroCheque : AnsiString = '') : Integer ;
Var
  ValorStr, DataStr, HoraStr : AnsiString;
  ANow : TDateTime ;
  pValorTransacao, pNumeroCupomVenda, pNumeroControle,
    pQuantidadeCheques, pPeriodicidadeCheques, pDataPrimeiroCheque,
    pCarenciaPrimeiroCheque : AnsiString;
  cNumeroControle : array [0..5] of AnsiChar;
  SL, ArquivoResposta : TStringList;
  Voltar, Parar : Boolean;
  ItemSelecionado : integer;
begin
  if fpAguardandoResposta then
    raise EACBrTEFDErro.Create( ACBrStr( 'Requisição anterior não concluida' ) ) ;

  if AHeader = 'CRT' then
  begin
     ItemSelecionado := -1 ;
     fOnObtemInformacao( ItemSelecionado ) ;
     if ItemSelecionado > 0 then
        Funcao := ItemSelecionado
     else
        raise EACBrTEFDErro.Create( ACBrStr( 'Tipo de cartão não informado' ) );
  end;

  if AHeader = 'ADM' then
  begin
     SL := TStringList.Create;
     try
        SL.Add('Cancelamento');
        SL.Add('Consulta de Parcelas');
        SL.Add('Reimpressão');
        SL.Add('Resumo das Vendas');
        SL.Add('Resgate de Prêmios');

        Parar := False;
        while not Parar do
        begin
           if ItemSelecionado = 30 then
           begin
             Result   := -1; //verificar isso aqui.
             Exit;
           end;

           Voltar := False;
           ItemSelecionado := -1;
           OnExibeMenu( 'Selecione a operação desejada', SL, ItemSelecionado, Voltar ) ;

           if (not Voltar) then
           begin
              if (ItemSelecionado >= 0) and (ItemSelecionado < SL.Count) then
              begin
                 case ItemSelecionado of
                   0: Funcao := 6;
                   1: Funcao := 9;
                   2: Funcao := 7;
                   3: Funcao := 8;
                   4: Funcao := 11;
                 end;

                 Parar := True;
              end 
              else
                Parar := True;
           end;
        end;
     finally
        SL.Free ;
     end ;
  end;

   Result   := -1 ;
   ANow     := Now ;
   DataStr  := FormatDateTime('YYYYMMDD', ANow );
   HoraStr  := FormatDateTime('HHNNSS', ANow );
   ValorStr := FormatFloat( '0.00', Valor );
   ValorStr := StringReplace( ValorStr, ',', '', [rfReplaceAll]) ;
   ValorStr := StringReplace( ValorStr, '.', '', [rfReplaceAll]) ;

   fDocumentosProcessados := '' ;

   GravaLog( '*** IniciaFuncaoDPOS. Modalidade: '+AHeader+
                                  ' Valor: '     +ValorStr+
                                  ' Documento: ' +Documento+
                                  ' Data: '      +DataStr+
                                  ' Hora: '      +HoraStr) ;

   pValorTransacao   := PadLeft(ValorStr, 12, '0') ;
   pNumeroCupomVenda := PadLeft(Documento, 6, '0') ;
   pNumeroControle   := HoraStr ;

   if Funcao = 1 then // Cartão de Crédito
      Result := xTransacaoCartaoCredito( PAnsiChar( pValorTransacao ),
                                         PAnsiChar( pNumeroCupomVenda ),
                                         PAnsiChar( pNumeroControle ) );
   if Funcao = 2 then // Cartão de Débito
      Result := xTransacaoCartaoDebito( PAnsiChar( pValorTransacao ),
                                        PAnsiChar( pNumeroCupomVenda ),
                                        PAnsiChar( pNumeroControle ) );
   if Funcao = 3 then // Cartão Voucher
      Result := xTransacaoCartaoVoucher( PAnsiChar( pValorTransacao ),
                                         PAnsiChar( pNumeroCupomVenda ),
                                         PAnsiChar( pNumeroControle ) );
   if Funcao = 5 then // Cheque
   begin
      pQuantidadeCheques     := '00' ;
      pPeriodicidadeCheques  := '000' ;
      pDataPrimeiroCheque    := '00000000' ;
      pCarenciaPrimeiroCheque:= '000' ;

      Result := xTransacaoCheque( PAnsiChar( pValorTransacao ),
                                  PAnsiChar( pNumeroCupomVenda ),
                                  PAnsiChar( pNumeroControle ),
                                  PAnsiChar( pQuantidadeCheques ),
                                  PAnsiChar( pPeriodicidadeCheques ),
                                  PAnsiChar( pDataPrimeiroCheque ),
                                  PAnsiChar( pCarenciaPrimeiroCheque ) );
   end;

   if Funcao = 6 then
   begin
      StrCopy( cNumeroControle, #0#0#0#0#0#0 );
      Result := xTransacaoCancelamentoPagamento( cNumeroControle );
      pNumeroControle := TrimRight( cNumeroControle );
   end;

   if Funcao = 7 then
      Result := xTransacaoReimpressaoCupom;

   if Funcao = 8 then
      Result := xTransacaoResumoVendas( PAnsiChar( pNumeroControle ) );

   if Funcao = 9 then
      Result := xTransacaoConsultaParcelas( PAnsiChar( pNumeroControle ) );

   if Funcao = 10 then
      Result := xTransacaoCartaoFrota( PAnsiChar( pValorTransacao ),
                                       PAnsiChar( pNumeroCupomVenda ),
                                       PAnsiChar( pNumeroControle ) );
   if Funcao = 11 then
      Result := xTransacaoResgatePremio( PAnsiChar( pNumeroCupomVenda),
                                       PAnsiChar( pNumeroControle ) );

   if Result <> 0 then
      raise EACBrTEFDErro.Create( ACBrStr( 'Retorno DTEF -> ' + IntToStr(Result) ) )
   else
   if Funcao <> 11 then
    begin
      Resp.Clear;

      with TACBrTEFDRespCliDTEF( Resp ) do
      begin
         fpIDSeq := fpIDSeq + 1 ;
         if Documento = '' then
            Documento := IntToStr(fpIDSeq) ;

         ArquivoResposta := TStringList.Create;
         try
           if ((Funcao = 7) or (Funcao = 9)) then
            begin
              if FileExists( ArqResp + 'ULTIMO.PRN' ) then
              begin
                ArquivoResposta.LoadFromFile(ArqResp + 'ULTIMO.PRN');
                SysUtils.DeleteFile(ArqResp + 'ULTIMO.PRN');
                ImprimirComprovantes(ArquivoResposta);
                ApagaEVerifica( ArqBackup );
              end;
            end
           else
            begin
              ArquivoResposta.LoadFromFile(ArqResp + pNumeroControle + '.' + NumeroTerminal);

              if ((Funcao = 6) or (Funcao = 8)) then
              begin
                 ImprimirComprovantes(ArquivoResposta);
                 ApagaEVerifica( ArqResp + pNumeroControle + '.' + NumeroTerminal );
                 ApagaEVerifica( ArqBackup );
              end
              else
               begin
                 MontaArquivoResposta(pNumeroControle, ArquivoResposta);

                 Conteudo.Conteudo.Text := ArquivoResposta.Text;
                 Conteudo.GravaInformacao(899,100, AHeader ) ;
                 Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
                 Conteudo.GravaInformacao(899,102, Documento ) ;
                 Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );
                 Conteudo.GravaInformacao(899,104, IntToStr(Funcao) );
                 Conteudo.GravaInformacao(899,130, 'IMPRIMINDO...' ) ;

                 Resp.TipoGP := fpTipo;
               end;
            end;
         finally
           if ((Funcao = 6) or (Funcao = 7) or (Funcao = 8) or (Funcao = 9)) then
           begin
             xConfirmaCartaoCredito( PAnsiChar( pNumeroControle ) );
             xFinalizaTransacao;
           end;

           if Assigned( ArquivoResposta ) then
             ArquivoResposta.Free;
         end;
      end;
    end;
end;

procedure TACBrTEFDCliDTEF.FinalizarTransacao(Operacao : AnsiString; Confirma : Boolean;
   NSU : AnsiString; DocumentoVinculado : AnsiString);
Var
   DataStr, HoraStr : AnsiString;
   nStatus, TipoTransacao, ItemSelecionado 	: Integer;
begin
  fRespostas.Clear;

  ItemSelecionado := 0 ;
  fOnObtemInformacao( ItemSelecionado ) ;

  if ItemSelecionado >= 0 then
     TipoTransacao := ItemSelecionado
  else
     TipoTransacao := -1;

 { if pos(DocumentoVinculado, fDocumentosProcessados) > 0 then
     exit ;   }

  fDocumentosProcessados := fDocumentosProcessados + DocumentoVinculado + '|' ;

  DataStr := FormatDateTime('YYYYMMDD',Now);
  HoraStr := FormatDateTime('HHNNSS',Now);

  GravaLog( '*** FinalizaTransacaoDPOS. Confirma: '+IfThen(Confirma,'SIM','NAO')+
            ' Documento: ' +DocumentoVinculado+
            ' Data: '      +DataStr+
            ' Hora: '      +HoraStr ) ;

  if Operacao = '0' then
   exit;

  case TipoTransacao of
    -1 : nStatus := xFinalizaTransacao;
    0  : nStatus := xConfirmaCartao( PAnsiChar( NSU ) );
    1  : nStatus := xConfirmaCartaoCredito( PAnsiChar( NSU ) );
    2  : nStatus := xConfirmaCartaoDebito( PAnsiChar( NSU ) );
    3  : nStatus := xConfirmaCartaoVoucher( PAnsiChar( NSU ) );
  //4  : nStatus Private Label
  //5  : nStatus := xConfirmaCartao
    10 : nStatus := xConfirmaCartaoFrota( PAnsiChar( NSU ) );
  else
    nStatus := -1 ;
  end;

  if ((nStatus = 0) and Confirma) then
  begin
     xFinalizaTransacao;
  end;

  if (nStatus = 11) then
  begin
     TACBrTEFD(Owner).DoExibeMsg( opmOK, 'Transação não efetuada.' );
     NCN(Operacao, NSU, '');
  end;

{  if not Confirma then
     TACBrTEFD(Owner).DoExibeMsg( opmOK, 'Transação TEF não efetuada.'+sLineBreak+
                                         'Favor reter o Cupom' );       }
end;

Function TACBrTEFDCliDTEF.ProcessarRespostaPagamento(
   const IndiceFPG_ECF : String; const Valor : Double) : Boolean;
var
  ImpressaoOk : Boolean;
  RespostaPendente : TACBrTEFDResp ;
begin
  Result := True ;

  with TACBrTEFD(Owner) do
  begin
     Self.Resp.IndiceFPG_ECF := IndiceFPG_ECF;

     { Cria Arquivo de Backup, contendo Todas as Respostas }
     CopiarResposta ;

     { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
     RespostaPendente := TACBrTEFDRespCliDTEF.Create ;
     RespostaPendente.Assign( Resp );
     RespostasPendentes.Add( RespostaPendente );

     if AutoEfetuarPagamento then
     begin
        ImpressaoOk := False ;

        try
           while not ImpressaoOk do
           begin
              try
                 ECFPagamento( IndiceFPG_ECF, Valor );
                 RespostasPendentes.SaldoAPagar  := RoundTo( RespostasPendentes.SaldoAPagar - Valor, -2 ) ;
                 RespostaPendente.OrdemPagamento := RespostasPendentes.Count + 1 ;
                 ImpressaoOk := True ;
              except
                 on EACBrTEFDECF do ImpressaoOk := False ;
                 else
                    raise ;
              end;

              if not ImpressaoOk then
              begin
                 if DoExibeMsg( opmYesNo, 'Impressora não responde'+sLineBreak+
                                          'Deseja imprimir novamente ?') <> mrYes then
                 begin
                    try ComandarECF(opeCancelaCupom); except {Exceção Muda} end ;
                    break ;
                 end;
              end;
           end;
        finally
           if not ImpressaoOk then
              CancelarTransacoesPendentes;
        end;
     end;

     if RespostasPendentes.SaldoRestante <= 0 then
     begin
        if AutoFinalizarCupom then
        begin
           FinalizarCupom( False );  { False não desbloqueia o MouseTeclado }
           ImprimirTransacoesPendentes;
        end;
     end ;
  end;
end;

procedure TACBrTEFDCliDTEF.MontaArquivoResposta(aNSU: String; aRetorno:TStringList);
var pDados : array [0..257] of AnsiChar;
    cDadosEstendido : array [0..257] of AnsiChar;
    sDados, sDadosEstendido, TipoTransacao, TipoOperacao, txt : String;
    aResposta, imgCupom : TStringList;
    i : Integer;
    ValSaque, ValorTemp: Double;
begin
  aResposta := TStringList.Create;
  imgCupom := TStringList.Create;
  try
    for i := 0 to aRetorno.Count - 1 do
    begin
       imgCupom.Add(aRetorno[i]);
    end;

    xObtemLogUltimaTransacao(pDados);
    sDados := TrimRight( pDados );

    strcopy(cDadosEstendido, 'LOGESTENDIDO');
    xObtemLogUltimaTransacao(cDadosEstendido);
    sDadosEstendido := TrimRight( cDadosEstendido );

    TipoTransacao := '99';
    txt := 'Outros Cartões';

    if copy(sDados,11,3) = 'CHQ' then
    begin
       TipoTransacao := '00';
       txt := 'Cheque';
    end;

    if copy(sDados,11,3) = 'CDB' then
    begin
       TipoTransacao := '01';
       txt := 'Cartão de Débito';
    end;

    if copy(sDados,11,3) = 'CCR' then
    begin
       TipoTransacao := '02';
       txt := 'Cartão de Crédito';
    end;

    if copy(sDados,11,3) = 'CCV' then
    begin
       TipoTransacao := '03';
       txt := 'Cartão tipo Voucher';
    end;

    if copy(sDados,112,2) = 'AV' then
       TipoOperacao := '00';
    if copy(sDados,112,2) = 'PD' then
       TipoOperacao := '01';
    if copy(sDados,112,2) = 'PS' then
       TipoOperacao := '02';
    if copy(sDados,112,2) = 'PC' then
       TipoOperacao := '03';

    aResposta.Add('134-000 = ' + aNSU);
    aResposta.Add('100-000 = ' + TipoTransacao + TipoOperacao);
    aResposta.Add('101-000 = ' + txt);
    aResposta.Add('102-000 = T.E.F.');
    aResposta.Add('105-000 = ' + copy(sDados,18,4) + copy(sDados,16,2) + copy(sDados,14,2) + copy(sDados,22,6));
    aResposta.Add('121-000 = ' +  BinaryStringToString(imgCupom.Text));
    //aResposta.Add('122-000 = ' +  BinaryStringToString(imgCupom.Text));

    //Debito - Saque
    if TipoTransacao = '01' then
    begin
       ValSaque := StrToFloat(copy(sDados,215,12));
       aResposta.Add('130-000 = ' + FloatToStr(ValSaque));
    end;

    //Cielo Premia - Desconto
    ValorTemp := StrToFloat(copy(sDadosEstendido,51,12));
    if ValorTemp > 0 then
    aResposta.Add('135-000 = ' + FloatToStr(ValorTemp));

    aRetorno.Text := aResposta.Text;
  finally
    aResposta.Free;
    imgCupom.Free;
  end ;
end;

procedure TACBrTEFDCliDTEF.ImprimirComprovantes(SL : TStringList);
var
  ImpressaoOk, FechaGerencialAberto, GerencialAberto : Boolean;
  Est : AnsiChar;
  TempoInicio : TDateTime;
begin
  CopiarResposta;

  GerencialAberto      := False;
  ImpressaoOk          := False ;
  FechaGerencialAberto := False ;
  TempoInicio          := now ;

  with TACBrTEFD(Owner) do
  begin
    try
      BloquearMouseTeclado( True );

      while not ImpressaoOk do
      begin
        try
          try
            if FechaGerencialAberto then
            begin
              Est := EstadoECF;

              { Fecha Vinculado ou Gerencial ou Cupom, se ficou algum aberto por Desligamento }
              case Est of
                'C'      : ComandarECF( opeFechaVinculado );
                'G', 'R' : ComandarECF( opeFechaGerencial );
                'V', 'P' : ComandarECF( opeCancelaCupom );
              end;

              GerencialAberto      := False ;
              FechaGerencialAberto := False ;

              if EstadoECF <> 'L' then
                raise EACBrTEFDECF.Create( ACBrStr('ECF não está LIVRE') ) ;
            end;

            TempoInicio := now ;
            DoExibeMsg( opmExibirMsgOperador, 'IMPRIMINDO...' ) ;

            if SL.Text <> '' then
            begin
              if not GerencialAberto then
               begin
                 ComandarECF( opeAbreGerencial ) ;
                 GerencialAberto := True ;
               end;

              ECFImprimeVia( trGerencial, 1, SL );

              ImpressaoOk := True ;
            end;

            if GerencialAberto then
            begin
               ComandarECF( opeFechaGerencial );
               GerencialAberto := False;
            end;

          finally
            { Verifica se Mensagem Ficou pelo menos por 5 segundos }
            if ImpressaoOk then
            begin
              while SecondsBetween(now,TempoInicio) < 5 do
              begin
                Sleep(EsperaSleep) ;
                {$IFNDEF NOGUI}
                Application.ProcessMessages;
                {$ENDIF}
              end;
            end;

            DoExibeMsg( opmRemoverMsgOperador, '' ) ;
          end;
        except
          on EACBrTEFDECF do ImpressaoOk := False ;
          else
             raise ;
        end;

        if not ImpressaoOk then
        begin
          if DoExibeMsg( opmYesNo, 'Impressora não responde'+sLineBreak+
                                   'Deseja imprimir novamente ?') <> mrYes then
            break ;

          FechaGerencialAberto := True ;
        end;
      end ;
    finally
      //SL.Free; //Não pode destruir está lista, pois a origem da chamada desta função ainda irá utliza-la.
    end;
  end;
end;

end.

