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
|* 21/11/2009: Daniel Simoes de Almeida
|*  - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrTEFDCliSiTef;

interface

uses
  Classes, SysUtils, ACBrTEFDClass
  {$IFNDEF NOGUI}
  {$IFDEF VisualCLX} ,QControls {$ELSE} {$IFDEF FMX} ,System.UITypes {$ENDIF} ,Controls {$ENDIF}
  {$ENDIF};

Const
   CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante = False ;
   CACBrTEFD_CliSiTef_PressioneEnter = 'PRESSIONE <ENTER>' ;
   CACBrTEFD_CliSiTef_TransacaoNaoEfetuada = 'Transação não efetuada.' ;
   CACBrTEFD_CliSiTef_TransacaoNaoEfetuadaReterCupom =
      'Transação não efetuada.'+sLineBreak+'Favor reter o Cupom' ;
   CACBrTEFD_CliSiTef_TransacaoEfetuadaReImprimir =
      'Transação TEF efetuada.'        + sLineBreak+
      'Favor reimprimir último Cupom.' + sLineBreak +
      '%s'                             + sLineBreak +
      '(Para Cielo utilizar os 6 últimos dígitos.)';
   CACBrTEFD_CliSiTef_NaoInicializado = 'CliSiTEF não inicializado' ;
   CACBrTEFD_CliSiTef_NaoConcluido = 'Requisição anterior não concluida' ;
   CACBrTEFD_CliSiTef_Erro1  = 'Endereço IP inválido ou não resolvido' ;
   CACBrTEFD_CliSiTef_Erro2  = 'Código da loja inválido' ;
   CACBrTEFD_CliSiTef_Erro3  = 'Código de terminal inválido' ;
   CACBrTEFD_CliSiTef_Erro6  = 'Erro na inicialização do TCP/IP' ;
   CACBrTEFD_CliSiTef_Erro7  = 'Falta de memória' ;
   CACBrTEFD_CliSiTef_Erro8  = 'Não encontrou a CliSiTef ou ela está com problemas' ;
   CACBrTEFD_CliSiTef_Erro10 = 'Erro de acesso na pasta CliSiTef (possível falta de permissão para escrita) ' + sLineBreak+
                               'ou o PinPad não está devidamente configurado no arquivo CliSiTef.ini ' + sLineBreak+
                               'ou parâmetros IdLoja e IdTerminal inválidos';
   CACBrTEFD_CliSiTef_Erro11 = 'Dados inválidos passados pela automação.';
   CACBrTEFD_CliSiTef_Erro12 = 'Modo seguro não ativo (possível falta de configuração no servidor SiTef do arquivo .cha).';
   CACBrTEFD_CliSiTef_Erro13 = 'Caminho da DLL inválido (o caminho completo das bibliotecas está muito grande).';

{$IFDEF LINUX}
  CACBrTEFD_CliSiTef_Lib = 'libclisitef.so' ;
{$ELSE}
  CACBrTEFD_CliSiTef_Lib = 'CliSiTef32I.dll' ;
{$ENDIF}

type
  { TACBrTEFDRespCliSiTef }

  TACBrTEFDRespCliSiTef = class( TACBrTEFDResp )
  protected
    function GetTransacaoAprovada : Boolean; override;
  public
    procedure ConteudoToProperty; override;
    procedure GravaInformacao( const Identificacao : Integer;
      const Informacao : AnsiString );
  end;

  TACBrTEFDCliSiTefExibeMenu = procedure( Titulo : String; Opcoes : TStringList;
    var ItemSelecionado : Integer; var VoltarMenu : Boolean ) of object ;

  TACBrTEFDCliSiTefOperacaoCampo = (tcString, tcDouble, tcCMC7, tcBarCode) ;

  TACBrTEFDCliSiTefObtemCampo = procedure( Titulo : String;
    TamanhoMinimo, TamanhoMaximo : Integer ;
    TipoCampo : Integer; Operacao : TACBrTEFDCliSiTefOperacaoCampo;
    var Resposta : AnsiString; var Digitado : Boolean; var VoltarMenu : Boolean )
    of object ;

  { TACBrTEFDCliSiTef }

   TACBrTEFDCliSiTef = class( TACBrTEFDClass )
   private
      fExibirErroRetorno: Boolean;
      fIniciouRequisicao: Boolean;
      fReimpressao: Boolean; {Indica se foi selecionado uma reimpressão no ADM}
      fCancelamento: Boolean; {Indica se foi selecionado Cancelamento no ADM}
      fCodigoLoja : AnsiString;
      fEnderecoIP : AnsiString;
      fNumeroTerminal : AnsiString;
      fOnExibeMenu : TACBrTEFDCliSiTefExibeMenu;
      fOnObtemCampo : TACBrTEFDCliSiTefObtemCampo;
      fOperacaoADM : Integer;
      fOperacaoATV : Integer;
      fOperacaoCHQ : Integer;
      fOperacaoCNC : Integer;
      fOperacaoCRT : Integer;
      fOperacaoPRE : Integer;
      fOperacaoReImpressao: Integer;
      fOperador : AnsiString;
      fParametrosAdicionais : TStringList;
      fRespostas: TStringList;
      fRestricoes : AnsiString;
      fDocumentoFiscal: AnsiString;
      fDataHoraFiscal: TDateTime;
      fDocumentosProcessados : AnsiString ;
      fPathDLL: string;
      fPortaPinPad: Integer;
      fUsaUTF8: Boolean;
      fArqBackUp: String;

     xConfiguraIntSiTefInterativoEx : function (
                pEnderecoIP: PAnsiChar;
                pCodigoLoja: PAnsiChar;
                pNumeroTerminal: PAnsiChar;
                ConfiguraResultado: smallint;
                pParametrosAdicionais: PAnsiChar): integer;
               {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

     xIniciaFuncaoSiTefInterativo : function (
                Modalidade: integer;
                pValor: PAnsiChar;
                pNumeroCuponFiscal: PAnsiChar;
                pDataFiscal: PAnsiChar;
                pHorario: PAnsiChar;
                pOperador: PAnsiChar;
                pRestricoes: PAnsiChar ): integer;
                {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;


     xFinalizaTransacaoSiTefInterativo : procedure (
                 smallint: Word;
                 pNumeroCuponFiscal: PAnsiChar;
                 pDataFiscal: PAnsiChar;
                 pHorario: PAnsiChar );
                 {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;


     xContinuaFuncaoSiTefInterativo : function (
                var ProximoComando: SmallInt;
                var TipoCampo: LongInt;
                var TamanhoMinimo: SmallInt;
                var TamanhoMaximo: SmallInt;
                pBuffer: PAnsiChar;
                TamMaxBuffer: Integer;
                ContinuaNavegacao: Integer ): integer;
                {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

     xEscreveMensagemPermanentePinPad: function(Mensagem:PAnsiChar):Integer;
        {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

     xObtemQuantidadeTransacoesPendentes: function(
        DataFiscal:AnsiString;
        NumeroCupon:AnsiString):Integer;
        {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

     xEnviaRecebeSiTefDireto : function (
        RedeDestino: SmallInt;
        FuncaoSiTef: SmallInt;
        OffsetCartao: SmallInt;
        DadosTx: AnsiString;
        TamDadosTx: SmallInt;
        DadosRx: PAnsiChar;
        TamMaxDadosRx: SmallInt;
        var CodigoResposta: SmallInt;
        TempoEsperaRx: SmallInt;
        CuponFiscal: AnsiString;
        DataFiscal: AnsiString;
        Horario: AnsiString;
        Operador: AnsiString;
        TipoTransacao: SmallInt): Integer;
        {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF} ;

     xValidaCampoCodigoEmBarras: function(
        Dados: AnsiString;
        var Tipo: SmallInt): Integer;
        {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

     xVerificaPresencaPinPad: function(): Integer
     {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};

     procedure AvaliaErro(Sts : Integer);
     function GetDataHoraFiscal: TDateTime;
     function GetDocumentoFiscal: AnsiString;
     procedure LoadDLLFunctions;
     procedure UnLoadDLLFunctions;
     procedure SetParametrosAdicionais(const AValue : TStringList) ;
   protected
     procedure SetNumVias(const AValue : Integer); override;

     Function FazerRequisicao( Funcao : Integer; AHeader : AnsiString = '';
        Valor : Double = 0; Documento : AnsiString = '';
        ListaRestricoes : AnsiString = '') : Integer ;
     Function ContinuarRequisicao( ImprimirComprovantes : Boolean ) : Integer ;

     procedure ProcessarResposta ; override;
     Function ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean; override;

     procedure VerificarIniciouRequisicao; override;

     Function SuportaDesconto : Boolean ;
     Function HMS : String ;

     function CopiarResposta: string; override;
   public
     property Respostas : TStringList read fRespostas ;
     property PathDLL: string read fPathDLL write fPathDLL;

     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     procedure Inicializar ; override;
     procedure DesInicializar ; override;

     procedure AtivarGP ; override;
     procedure VerificaAtivo ; override;

     Procedure ATV ; override;
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
     Function PRE(Valor : Double; DocumentoVinculado : String = '';
        Moeda : Integer = 0) : Boolean; override;

     Function DefineMensagemPermanentePinPad(Mensagem:AnsiString):Integer;
     Function ObtemQuantidadeTransacoesPendentes(Data:TDateTime;
        CupomFiscal:AnsiString):Integer;
     Function EnviaRecebeSiTefDireto( RedeDestino: SmallInt;
        FuncaoSiTef: SmallInt; OffsetCartao: SmallInt; DadosTx: AnsiString;
        var DadosRx: AnsiString; var CodigoResposta: SmallInt;
        TempoEsperaRx: SmallInt; CuponFiscal: AnsiString;
        Confirmar: Boolean): Integer;
     procedure FinalizarTransacao( Confirma : Boolean;
        DocumentoVinculado : AnsiString);
     function ValidaCampoCodigoEmBarras(Dados: AnsiString;
        var Tipo: SmallInt): Integer;
     function VerificaPresencaPinPad: Boolean;

   published
     property EnderecoIP: AnsiString                    read fEnderecoIP           write fEnderecoIP;
     property CodigoLoja: AnsiString                    read fCodigoLoja           write fCodigoLoja;
     property NumeroTerminal: AnsiString                read fNumeroTerminal       write fNumeroTerminal;
     property Operador: AnsiString                      read fOperador             write fOperador;
     property PortaPinPad: Integer                      read fPortaPinPad          write fPortaPinPad default 0;
     property ParametrosAdicionais: TStringList         read fParametrosAdicionais write SetParametrosAdicionais;
     property Restricoes: AnsiString                    read fRestricoes           write fRestricoes;
     property DocumentoFiscal: AnsiString               read GetDocumentoFiscal    write fDocumentoFiscal;
     property DataHoraFiscal: TDateTime                 read GetDataHoraFiscal     write fDataHoraFiscal;
     property OperacaoATV: Integer                      read fOperacaoATV          write fOperacaoATV default 111;
     property OperacaoADM: Integer                      read fOperacaoADM          write fOperacaoADM default 110;
     property OperacaoCRT: Integer                      read fOperacaoCRT          write fOperacaoCRT default 0;
     property OperacaoCHQ: Integer                      read fOperacaoCHQ          write fOperacaoCHQ default 1;
     property OperacaoCNC: Integer                      read fOperacaoCNC          write fOperacaoCNC default 200;
     property OperacaoPRE: Integer                      read fOperacaoPRE          write fOperacaoPRE default 115;
     property OperacaoReImpressao: Integer              read fOperacaoReImpressao  write fOperacaoReImpressao default 112;
     property OnExibeMenu: TACBrTEFDCliSiTefExibeMenu   read fOnExibeMenu          write fOnExibeMenu;
     property OnObtemCampo: TACBrTEFDCliSiTefObtemCampo read fOnObtemCampo         write fOnObtemCampo;
     property ExibirErroRetorno: Boolean                read fExibirErroRetorno    write fExibirErroRetorno default False;
     property UsaUTF8: Boolean                          read fUsaUTF8              write fUsaUTF8 default False;
   end;

implementation

Uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  dateutils, Math, StrUtils,
  ACBrTEFD, ACBrUtil ;

{ TACBrTEFDRespCliSiTef }

function TACBrTEFDRespCliSiTef.GetTransacaoAprovada : Boolean;
begin
   Result := True ;
end;

procedure TACBrTEFDRespCliSiTef.ConteudoToProperty;
var
   I, wTipoOperacao, wNumCB: Integer;
   wTotalParc, wValParc: Double;
   Parc  : TACBrTEFDRespParcela;
   Linha : TACBrTEFDLinha;
   CB    : TACBrTEFDRespCB;
   LinStr: AnsiString;
begin
   fpValorTotal := 0;
   fpImagemComprovante1aVia.Clear;
   fpImagemComprovante2aVia.Clear;
   fpDebito    := False;
   fpCredito   := False;
   fpDigitado  := False;

   for I := 0 to Conteudo.Count - 1 do
   begin
     Linha  := Conteudo.Linha[I];
     LinStr := StringToBinaryString( Linha.Informacao.AsString );

     case Linha.Identificacao of
        29 : fpDigitado := (LinStr = 'True');
       // TODO: Mapear mais propriedades do CliSiTef //
       100 :
         begin
            fpModalidadePagto := LinStr;

            case StrToIntDef(Copy(fpModalidadePagto,1,2),0) of
              01 : fpDebito  := True;
              02 : fpCredito := True;
            end;

            wTipoOperacao:= StrToIntDef(Copy(fpModalidadePagto,3,2),0);
            {Tipo de Parcelamento}
            case wTipoOperacao of
               02 : fpParceladoPor:= parcLoja;
               03 : fpParceladoPor:= parcADM;
            end;

            case wTipoOperacao of
               00    : fpTipoOperacao:= opAvista;
               01    : fpTipoOperacao:= opPreDatado;
               02,03 : fpTipoOperacao:= opParcelado;
               else
                 fpTipoOperacao:= opOutras;
            end;
         end;
       101 : fpModalidadePagtoExtenso  := LinStr;
       102 : fpModalidadePagtoDescrita := LinStr;
       105 :
         begin
           fpDataHoraTransacaoComprovante  := Linha.Informacao.AsTimeStampSQL;
           fpDataHoraTransacaoHost         := fpDataHoraTransacaoComprovante ;
           fpDataHoraTransacaoLocal        := fpDataHoraTransacaoComprovante ;
         end;
       120 : fpAutenticacao                := LinStr;
       121 : fpImagemComprovante1aVia.Text := StringReplace( StringToBinaryString( Linha.Informacao.AsString ), #10, sLineBreak, [rfReplaceAll] );
       122 : fpImagemComprovante2aVia.Text := StringReplace( StringToBinaryString( Linha.Informacao.AsString ), #10, sLineBreak, [rfReplaceAll] );
       123 : fpTipoTransacao               := Linha.Informacao.AsInteger;
       130 :
         begin
           fpSaque      := Linha.Informacao.AsFloat ;
           fpValorTotal := fpValorTotal + fpSaque ;
         end;
       131 : fpInstituicao                 := LinStr;
       132 : fpCodigoBandeiraPadrao        := LinStr;
       133 : fpCodigoAutorizacaoTransacao  := Linha.Informacao.AsString;
       134 : fpNSU                         := LinStr;
       136 : fpBin                         := Linha.Informacao.AsString;
       139 : fpValorEntradaCDC             := Linha.Informacao.AsFloat;
       140 : fpDataEntradaCDC              := Linha.Informacao.AsDate;
       156 : fpRede                        := LinStr;
       501 : fpTipoPessoa                  := AnsiChar(IfThen(Linha.Informacao.AsInteger = 0,'J','F')[1]);
       502 : fpDocumentoPessoa             := LinStr ;
       504 : fpTaxaServico                 := Linha.Informacao.AsFloat ;
       505 : fpQtdParcelas                 := Linha.Informacao.AsInteger ;
       506 : fpDataPreDatado               := Linha.Informacao.AsDate;
       511 : fpQtdParcelas                 := Linha.Informacao.AsInteger;  {Parcelas CDC - Neste caso o campo 505 não é retornado}
       515 : fpDataHoraTransacaoCancelada  := Linha.Informacao.AsDate ;
       516 : fpNSUTransacaoCancelada       := LinStr;
       527 : fpDataVencimento              := Linha.Informacao.AsDate;    { Data Vencimento do Cheque }
       589 : fpCodigoOperadoraCelular      := LinStr;                     { Código da Operadora de Celular }
       590 : fpNomeOperadoraCelular        := LinStr;                     { Nome da Operadora de Celular }
       591 : fpValorRecargaCelular         := Linha.Informacao.AsFloat;   { Valor selecionado para a Recarga }
       592 : fpNumeroRecargaCelular        := LinStr;                     { Numero de Celular informado para Recarda }

       607 :  // Indice do Correspondente Bancário
         begin
           wNumCB := Linha.Informacao.AsInteger;

           if (wNumCB = 1) then
             fpCorrespBancarios.Clear;

           CB := TACBrTEFDRespCB.Create;
           CB.DataVencimento  := LeInformacao(600, wNumCB).AsDate;    { Data Vencimento do título - CB }
           CB.ValorPago       := LeInformacao(601, wNumCB).AsFloat;   { Valor Pago do título - CB }
           CB.ValorOriginal   := LeInformacao(602, wNumCB).AsFloat;   { Valor Original do título - CB }
           CB.Acrescimo       := LeInformacao(603, wNumCB).AsFloat;   { Valor do Acréscimo - CB }
           CB.Desconto        := LeInformacao(604, wNumCB).AsFloat;   { Valor do Desconto - CB }
           CB.DataPagamento   := LeInformacao(605, wNumCB).AsDate;    { Data contábil do Pagamento - CB }
           CB.NSUTransacaoCB  := LeInformacao(611, wNumCB).AsString;  { NSU da Transação CB }
           CB.TipoDocumento   := LeInformacao(612, wNumCB).AsInteger; { Tipo Docto CB - 0:Arrecadação/ 1:Título/ 2:Tributo }
           CB.NSUCancelamento := LeInformacao(623, wNumCB).AsString;  { NSU para cancelamento - CB }
           CB.Documento       := LeInformacao(624, wNumCB).AsString;  { Linha Digitável/Código de Barras do documento pago}

           fpCorrespBancarios.Add(CB);
         end;
       609 : fpCorrespBancarios.TotalTitulos        := Linha.Informacao.AsFloat;   { Valor total dos títulos efetivamente pagos no caso de pagamento em lote }
       610 : fpCorrespBancarios.TotalTitulosNaoPago := Linha.Informacao.AsFloat;   { Valor total dos títulos NÃO pagos no caso de pagamento em lote }
       613 :
        begin
          fpCheque                         := copy(LinStr, 21, 6);
          fpCMC7                           := LinStr;
        end;
       626 : fpBanco                       := LinStr;
       627 : fpAgencia                     := LinStr;
       628 : fpAgenciaDC                   := LinStr;
       629 : fpConta                       := LinStr;
       630 : fpContaDC                     := LinStr;

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
            500 : fpIdPagamento      := Linha.Informacao.AsInteger ;
            501 : fpIdRespostaFiscal := Linha.Informacao.AsInteger ;
          end;
        end;

       // dados de retorno da NFC-e/SAT
       950: fpNFCeSAT.CNPJCredenciadora := Linha.Informacao.AsString;
       951: fpNFCeSAT.Bandeira          := Linha.Informacao.AsString;
       952: fpNFCeSAT.Autorizacao       := Linha.Informacao.AsString;
       953: fpNFCeSAT.CodCredenciadora  := Linha.Informacao.AsString;
      1002: fpNFCeSAT.DataExpiracao     := Linha.Informacao.AsString;
      1003: fpNFCeSAT.DonoCartao        := Linha.Informacao.AsString;
      1190: fpNFCeSAT.UltimosQuatroDigitos := Linha.Informacao.AsString;
       4029 :
        begin
          fpDesconto   := Linha.Informacao.AsFloat;
          fpValorTotal := fpValorTotal - fpDesconto;
        end;

     end;
   end ;

   fpQtdLinhasComprovante := max( fpImagemComprovante1aVia.Count,
                                  fpImagemComprovante2aVia.Count ) ;

  // leitura de parcelas conforme nova documentação
  // 141 e 142 foram removidos em Setembro de 2014
  fpParcelas.Clear;
  if (fpQtdParcelas > 0) then
  begin
    wValParc   := RoundABNT((fpValorTotal / fpQtdParcelas), -2);
    wTotalParc := 0;

    for I := 1 to fpQtdParcelas do
    begin
      Parc := TACBrTEFDRespParcela.Create;
      if I = 1 then
      begin
        Parc.Vencimento := LeInformacao(140, I).AsDate;
        Parc.Valor      := LeInformacao(524, I).AsFloat;
      end
      else
      begin
        Parc.Vencimento := IncDay(LeInformacao(140, I).AsDate, LeInformacao(508, I).AsInteger);
        Parc.Valor      := LeInformacao(525, I).AsFloat;
      end;

      // caso não retorne os dados acima prencher com os defaults
      if Trim(Parc.NSUParcela) = '' then
        Parc.NSUParcela := NSU;

      if Parc.Vencimento <= 0 then
        Parc.Vencimento := IncDay(fpDataHoraTransacaoHost, I * 30);

      if Parc.Valor <= 0 then
      begin
        if (I = fpQtdParcelas) then
          wValParc := fpValorTotal - wTotalParc
        else
          wTotalParc := wTotalParc + wValParc;

        Parc.Valor := wValParc;
      end;

      fpParcelas.Add(Parc);
    end;
  end;
end;

procedure TACBrTEFDRespCliSiTef.GravaInformacao(const Identificacao: Integer;
  const Informacao: AnsiString);
var
  Sequencia: Integer;
begin
  Sequencia := 0;

  case Identificacao of
    141, 142,            // 141 - Data Parcela, 142 - Valor Parcela
    600..607, 611..624:  // Dados do Corresp. Bancário
    begin
      Sequencia := 1;

      while (Trim(LeInformacao(Identificacao, Sequencia).AsString) <> '') do
        Inc(Sequencia);
    end;
  end;

  fpConteudo.GravaInformacao( Identificacao, Sequencia,
                              BinaryStringToString(Informacao) ); // Converte #10 para "\x0A"
end;


{ TACBrTEFDClass }

constructor TACBrTEFDCliSiTef.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  fIniciouRequisicao := False;
  fReimpressao       := False;
  fCancelamento      := False;
  ArqReq    := '' ;
  ArqResp   := '' ;
  ArqSTS    := '' ;
  ArqTemp   := '' ;
  GPExeName := '' ;
  fpTipo    := gpCliSiTef;
  Name      := 'CliSiTef' ;

  fEnderecoIP     := '' ;
  fCodigoLoja     := '' ;
  fNumeroTerminal := '' ;
  fOperador       := '' ;
  fRestricoes     := '' ;

  fDocumentoFiscal := '';
  fDataHoraFiscal  := 0;

  fOperacaoATV         := 111 ; // 111 - Teste de comunicação com o SiTef
  fOperacaoReImpressao := 112 ; // 112 - Menu Re-impressão
  fOperacaoPRE         := 115 ; // 115 - Pré-autorização
  fOperacaoADM         := 110 ; // 110 - Abre o leque das transações Gerenciais
  fOperacaoCRT         := 0 ;   // A CliSiTef permite que o operador escolha a forma
                                // de pagamento através de menus
  fOperacaoCHQ         := 1 ;   // Cheque
  fOperacaoCNC         := 200 ; // 200 Cancelamento Normal: Inicia a coleta dos dados
                                // no ponto necessário para fazer o cancelamento de uma
                                // transação de débito ou crédito, sem ser necessário
                                // passar antes pelo menu de transações administrativas
  fDocumentosProcessados := '' ;
  fExibirErroRetorno     := False;
  fUsaUTF8               := False;

  fParametrosAdicionais := TStringList.Create;
  fRespostas            := TStringList.Create;

  xConfiguraIntSiTefInterativoEx      := nil;
  xIniciaFuncaoSiTefInterativo        := nil;
  xContinuaFuncaoSiTefInterativo      := nil;
  xFinalizaTransacaoSiTefInterativo   := nil;
  xEscreveMensagemPermanentePinPad    := nil;
  xObtemQuantidadeTransacoesPendentes := nil;
  xValidaCampoCodigoEmBarras          := nil;
  xEnviaRecebeSiTefDireto             := nil;
  xVerificaPresencaPinPad             := nil;

  fOnExibeMenu  := nil ;
  fOnObtemCampo := nil ;

  if Assigned( fpResp ) then
     fpResp.Free ;

  fpResp := TACBrTEFDRespCliSiTef.Create;
  fpResp.TipoGP := Tipo;
end;

function TACBrTEFDCliSiTef.DefineMensagemPermanentePinPad(Mensagem:AnsiString):Integer;
begin
   if Assigned(xEscreveMensagemPermanentePinPad) then
      Result := xEscreveMensagemPermanentePinPad(PAnsiChar(Mensagem))
   else
      raise EACBrTEFDErro.Create( ACBrStr( CACBrTEFD_CliSiTef_NaoInicializado ) ) ;
end;

destructor TACBrTEFDCliSiTef.Destroy;
begin
   fParametrosAdicionais.Free ;
   fRespostas.Free ;

   inherited Destroy;
end;

procedure TACBrTEFDCliSiTef.LoadDLLFunctions ;
 procedure CliSiTefFunctionDetect( FuncName: AnsiString; var LibPointer: Pointer ) ;
 var
 sLibName: string;
 begin
   if not Assigned( LibPointer )  then
   begin
     // Verifica se exite o caminho das DLLs
     sLibName := '';
     if Length(PathDLL) > 0 then
        sLibName := PathWithDelim(PathDLL);

     // Concatena o caminho se exitir mais o nome da DLL.
     sLibName := sLibName + CACBrTEFD_CliSiTef_Lib;

     if not FunctionDetect( sLibName, FuncName, LibPointer) then
     begin
        LibPointer := NIL ;
        raise EACBrTEFDErro.Create( ACBrStr( 'Erro ao carregar a função:'+FuncName+
                                         ' de: '+CACBrTEFD_CliSiTef_Lib ) ) ;
     end ;
   end ;
 end ;
begin
   CliSiTefFunctionDetect('ConfiguraIntSiTefInterativoEx', @xConfiguraIntSiTefInterativoEx);
   CliSiTefFunctionDetect('IniciaFuncaoSiTefInterativo', @xIniciaFuncaoSiTefInterativo);
   CliSiTefFunctionDetect('ContinuaFuncaoSiTefInterativo', @xContinuaFuncaoSiTefInterativo);
   CliSiTefFunctionDetect('FinalizaTransacaoSiTefInterativo', @xFinalizaTransacaoSiTefInterativo);
   CliSiTefFunctionDetect('EscreveMensagemPermanentePinPad',@xEscreveMensagemPermanentePinPad);
   CliSiTefFunctionDetect('ObtemQuantidadeTransacoesPendentes',@xObtemQuantidadeTransacoesPendentes);
   CliSiTefFunctionDetect('ValidaCampoCodigoEmBarras',@xValidaCampoCodigoEmBarras);
   CliSiTefFunctionDetect('EnviaRecebeSiTefDireto',@xEnviaRecebeSiTefDireto);
   CliSiTefFunctionDetect('VerificaPresencaPinPad',@xVerificaPresencaPinPad);
end ;

procedure TACBrTEFDCliSiTef.UnLoadDLLFunctions;
var
   sLibName: String;
begin
  sLibName := '';
  if Length(PathDLL) > 0 then
     sLibName := PathWithDelim(PathDLL);

  UnLoadLibrary( sLibName + CACBrTEFD_CliSiTef_Lib );

  xConfiguraIntSiTefInterativoEx      := Nil;
  xIniciaFuncaoSiTefInterativo        := Nil;
  xContinuaFuncaoSiTefInterativo      := Nil;
  xFinalizaTransacaoSiTefInterativo   := Nil;
  xEscreveMensagemPermanentePinPad    := Nil;
  xObtemQuantidadeTransacoesPendentes := Nil;
  xValidaCampoCodigoEmBarras          := Nil;
  xEnviaRecebeSiTefDireto             := Nil;
  xVerificaPresencaPinPad             := Nil;
end;

procedure TACBrTEFDCliSiTef.SetParametrosAdicionais(const AValue : TStringList
   ) ;
begin
   fParametrosAdicionais.Assign( AValue ) ;
end ;

procedure TACBrTEFDCliSiTef.SetNumVias(const AValue : Integer);
begin
   fpNumVias := 2;
end;

procedure TACBrTEFDCliSiTef.Inicializar;
Var
  Sts : Integer ;
  ParamAdic : AnsiString ;
  Erro : String;
begin
  if Inicializado then exit ;

  if not Assigned( OnExibeMenu ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnExibeMenu" não programado' ) ) ;

  if not Assigned( OnObtemCampo ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnObtemCampo" não programado' ) ) ;

  LoadDLLFunctions;

  // configuração da porta do pin-pad
  if PortaPinPad > 0 then
  begin
    if ParametrosAdicionais.Values['PortaPinPad'] = '' then
      ParametrosAdicionais.Add('PortaPinPad=' + IntToStr(PortaPinPad))
    else
      ParametrosAdicionais.Values['PortaPinPad'] := IntToStr(PortaPinPad);
  end;

  // cielo premia
  if ParametrosAdicionais.Values['VersaoAutomacaoCielo'] = '' then
  begin
    if SuportaDesconto then
      ParametrosAdicionais.Add('VersaoAutomacaoCielo=' + PadRight( TACBrTEFD(Owner).Identificacao.SoftwareHouse, 8 ) + '10');
  end;

  // acertar quebras de linhas e abertura e fechamento da lista de parametros
  ParamAdic := StringReplace(Trim(ParametrosAdicionais.Text), sLineBreak, ';', [rfReplaceAll]);
  ParamAdic := '['+ ParamAdic + ']';

  GravaLog( '*** ConfiguraIntSiTefInterativoEx. EnderecoIP: '   +fEnderecoIP+
                                            ' CodigoLoja: '     +fCodigoLoja+
                                            ' NumeroTerminal: ' +fNumeroTerminal+
                                            ' Resultado: 0'     +
                                            ' ParametrosAdicionais: '+ParamAdic ) ;

  Sts := xConfiguraIntSiTefInterativoEx( PAnsiChar(fEnderecoIP),
                                         PAnsiChar(fCodigoLoja),
                                         PAnsiChar(fNumeroTerminal),
                                         0,
                                         PAnsiChar(ParamAdic) );
  Erro := '' ;
  Case Sts of
    1 :	Erro := CACBrTEFD_CliSiTef_Erro1;
    2 : Erro := CACBrTEFD_CliSiTef_Erro2;
    3 : Erro := CACBrTEFD_CliSiTef_Erro3;
    6 : Erro := CACBrTEFD_CliSiTef_Erro6;
    7 : Erro := CACBrTEFD_CliSiTef_Erro7;
    8 : Erro := CACBrTEFD_CliSiTef_Erro8;
   10 : Erro := CACBrTEFD_CliSiTef_Erro10;
   11 : Erro := CACBrTEFD_CliSiTef_Erro11;
   12 : Erro := CACBrTEFD_CliSiTef_Erro12;
   13 : Erro := CACBrTEFD_CliSiTef_Erro13;
  end;

  if Erro <> '' then
     raise EACBrTEFDErro.Create( ACBrStr( Erro ) ) ;

  GravaLog( Name +' Inicializado CliSiTEF' );

  VerificarTransacoesPendentesClass(True);
  fpInicializado := True;
end;

procedure TACBrTEFDCliSiTef.DesInicializar;
begin
   UnLoadDLLFunctions ;

   inherited DesInicializar;
end;

procedure TACBrTEFDCliSiTef.AtivarGP;
begin
   raise EACBrTEFDErro.Create( ACBrStr( 'CliSiTef não pode ser ativado localmente' )) ;
end;

procedure TACBrTEFDCliSiTef.VerificaAtivo;
begin
   {Nada a Fazer}
end;

procedure TACBrTEFDCliSiTef.ATV;
var
   Sts : Integer;
begin
  Sts := FazerRequisicao( fOperacaoATV, 'ATV' ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante ) ;

  if ( Sts <> 0 ) then
     AvaliaErro( Sts )
  else
     if not CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

function TACBrTEFDCliSiTef.ADM: Boolean;
var
   Sts : Integer;
begin
  Sts := FazerRequisicao( fOperacaoADM, 'ADM', 0, '', fRestricoes ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante ) ;

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     if not CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

function TACBrTEFDCliSiTef.CRT(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
var
  Sts : Integer;
  Restr : AnsiString ;
begin
  if (Valor <> 0) then
    VerificarTransacaoPagamento( Valor );

  Restr := fRestricoes;
  if Restr = '' then
     Restr := '[10]' ;     // 10 - Cheques

  if DocumentoVinculado = '' then
     DocumentoVinculado := fDocumentoFiscal;

  Sts := FazerRequisicao( fOperacaoCRT, 'CRT', Valor, DocumentoVinculado, Restr ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( False ) ;  { False = NAO Imprimir Comprovantes agora }

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );
end;

function TACBrTEFDCliSiTef.CHQ(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; CMC7: String; TipoPessoa: AnsiChar;
  DocumentoPessoa: String; DataCheque: TDateTime; Banco: String;
  Agencia: String; AgenciaDC: String; Conta: String; ContaDC: String;
  Cheque: String; ChequeDC: String; Compensacao: String): Boolean;
var
  Sts : Integer;
  Restr : AnsiString ;

  Function FormataCampo( Campo : AnsiString; Tamanho : Integer ) : AnsiString ;
  begin
    Result := PadLeft( OnlyNumber( Trim( Campo ) ), Tamanho, '0') ;
  end ;

begin
  if (DocumentoVinculado <> '') and (Valor <> 0) then
     VerificarTransacaoPagamento( Valor );

  Respostas.Values['501'] := ifthen(TipoPessoa = 'J','1','0');

  if DocumentoPessoa <> '' then
     Respostas.Values['502'] := OnlyNumber(Trim(DocumentoPessoa));

  if DataCheque <> 0  then
     Respostas.Values['506'] := FormatDateTime('DDMMYYYY',DataCheque) ;

  if CMC7 <> '' then
     Respostas.Values['517'] := '1:'+CMC7
  else
     Respostas.Values['517'] := '0:'+FormataCampo(Compensacao,3)+
                                     FormataCampo(Banco,3)+
                                     FormataCampo(Agencia,4)+
                                     FormataCampo(AgenciaDC,1)+
                                     FormataCampo(Conta,10)+
                                     FormataCampo(ContaDC,1)+
                                     FormataCampo(Cheque,6)+
                                     FormataCampo(ChequeDC,1) ;

  Restr := fRestricoes;
  if Restr = '' then
     Restr := '[15;25]' ;  // 15 - Cartão Credito; 25 - Cartao Debito

  if DocumentoVinculado = '' then
     DocumentoVinculado := fDocumentoFiscal;

  Sts := FazerRequisicao( fOperacaoCHQ, 'CHQ', Valor, DocumentoVinculado, Restr ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( False ) ;  { False = NAO Imprimir Comprovantes agora }

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );
end;

procedure TACBrTEFDCliSiTef.CNF(Rede, NSU, Finalizacao: String;
  DocumentoVinculado: String);
begin
  // CliSiTEF não usa Rede, NSU e Finalizacao

  FinalizarTransacao( True, DocumentoVinculado );
end;

function TACBrTEFDCliSiTef.CNC(Rede, NSU: String; DataHoraTransacao: TDateTime;
  Valor: Double): Boolean;
var
   Sts : Integer;
begin
  Respostas.Values['146'] := FormatFloat('0.00',Valor);
  Respostas.Values['147'] := FormatFloat('0.00',Valor);
  Respostas.Values['515'] := FormatDateTime('DDMMYYYY',DataHoraTransacao) ;
  Respostas.Values['516'] := NSU ;

  Sts := FazerRequisicao( fOperacaoCNC, 'CNC' ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante ) ;

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     if not CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

function TACBrTEFDCliSiTef.PRE(Valor: Double; DocumentoVinculado: String;
  Moeda: Integer): Boolean;
var
   Sts : Integer;
begin
  Sts := FazerRequisicao( fOperacaoPRE, 'PRE', Valor, '', fRestricoes ) ;

  if Sts = 10000 then
     Sts := ContinuarRequisicao( CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante ) ;

  Result := ( Sts = 0 ) ;

  if not Result then
     AvaliaErro( Sts )
  else
     if not CACBrTEFD_CliSiTef_ImprimeGerencialConcomitante then
        ProcessarResposta;
end;

procedure TACBrTEFDCliSiTef.NCN(Rede, NSU, Finalizacao: String; Valor: Double;
  DocumentoVinculado: String);
begin
  // CliSiTEF não usa Rede, NSU, Finalizacao e Valor
  FinalizarTransacao( False, DocumentoVinculado );
end;

function TACBrTEFDCliSiTef.ObtemQuantidadeTransacoesPendentes(Data:TDateTime;
  CupomFiscal: AnsiString): Integer;
var
  sDate:AnsiString;
begin
   sDate:= FormatDateTime('yyyymmdd',Data);
   if Assigned(xObtemQuantidadeTransacoesPendentes) then
      Result := xObtemQuantidadeTransacoesPendentes(sDate,CupomFiscal)
   else
      raise EACBrTEFDErro.Create( ACBrStr( CACBrTEFD_CliSiTef_NaoInicializado ) ) ;
end;

function TACBrTEFDCliSiTef.EnviaRecebeSiTefDireto(RedeDestino: SmallInt;
  FuncaoSiTef: SmallInt; OffsetCartao: SmallInt; DadosTx: AnsiString;
  var DadosRx: AnsiString; var CodigoResposta: SmallInt;
  TempoEsperaRx: SmallInt; CuponFiscal: AnsiString; Confirmar: Boolean
  ): Integer;
var
  Buffer: array [0..20000] of AnsiChar;
  ANow: TDateTime;
  DataStr, HoraStr: String;
begin
  ANow    := Now ;
  DataStr := FormatDateTime('YYYYMMDD', ANow );
  HoraStr := FormatDateTime('HHNNSS', ANow );
  Buffer := #0;
  FillChar(Buffer, SizeOf(Buffer), 0);

  GravaLog( 'EnviaRecebeSiTefDireto -> Rede:' +IntToStr(RedeDestino) +', Funcao:'+
            IntToStr(FuncaoSiTef)+', OffSetCartao:'+IntToStr(OffsetCartao)+', DadosTX:'+
            DadosTx+', TimeOut'+IntToStr(TempoEsperaRx)+' Cupom:'+CuponFiscal+', '+
            ifthen(Confirmar,'Confirmar','NAO Confirmar'), True );

  Result := xEnviaRecebeSiTefDireto( RedeDestino,
                           FuncaoSiTef,
                           OffsetCartao,
                           DadosTx,
                           Length(DadosTx)+1,
                           Buffer,
                           SizeOf(Buffer),
                           CodigoResposta,
                           TempoEsperaRx,
                           CuponFiscal, DataStr, HoraStr, fOperador,
                           IfThen(Confirmar,1,0) );

  DadosRx := TrimRight( LeftStr(Buffer,max(Result,0)) ) ;
end;

function TACBrTEFDCliSiTef.FazerRequisicao(Funcao: Integer;
  AHeader: AnsiString; Valor: Double; Documento: AnsiString;
  ListaRestricoes: AnsiString): Integer;
Var
  ValorStr, DataStr, HoraStr : AnsiString;
  DataHora : TDateTime ;
begin
   if not Assigned(xIniciaFuncaoSiTefInterativo) then
      raise EACBrTEFDErro.Create(ACBrStr(CACBrTEFD_CliSiTef_NaoInicializado));

   if Documento = '' then
      Documento := DocumentoFiscal;

   Req.DocumentoVinculado  := Documento;
   Req.ValorTotal          := Valor;

   if fpAguardandoResposta then
      raise EACBrTEFDErro.Create( ACBrStr( CACBrTEFD_CliSiTef_NaoConcluido ) ) ;

   if (pos('{TipoTratamento=4}',ListaRestricoes) = 0) and
      (pos(AHeader,'CRT,CHQ') > 0 ) and
      SuportaDesconto then
   begin
      ListaRestricoes := ListaRestricoes + '{TipoTratamento=4}';
   end;

   fIniciouRequisicao := True;

   DataHora := DataHoraFiscal ;
   DataStr  := FormatDateTime('YYYYMMDD', DataHora );
   HoraStr  := FormatDateTime('HHNNSS', DataHora );
   ValorStr := FormatFloatBr( Valor );
   fDocumentosProcessados := '' ;

   GravaLog( '*** IniciaFuncaoSiTefInterativo. Modalidade: '+IntToStr(Funcao)+
                                             ' Valor: '     +ValorStr+
                                             ' Documento: ' +Documento+
                                             ' Data: '      +DataStr+
                                             ' Hora: '      +HoraStr+
                                             ' Operador: '  +fOperador+
                                             ' Restricoes: '+ListaRestricoes ) ;

   Result := xIniciaFuncaoSiTefInterativo( Funcao,
                                           PAnsiChar( ValorStr ),
                                           PAnsiChar( Documento ),
                                           PAnsiChar( DataStr ),
                                           PAnsiChar( HoraStr ),
                                           PAnsiChar( fOperador ),
                                           PAnsiChar( ListaRestricoes ) ) ;

   { Adiciona Campos já conhecidos em Resp, para processa-los em
     métodos que manipulam "RespostasPendentes" (usa códigos do G.P.)  }
   Resp.Clear;

   with TACBrTEFDRespCliSiTef( Resp ) do
   begin
     fpIDSeq := fpIDSeq + 1 ;
     if Documento = '' then
        Documento := IntToStr(fpIDSeq) ;

     Conteudo.GravaInformacao(899,100, AHeader ) ;
     Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
     Conteudo.GravaInformacao(899,102, Documento ) ;
     Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );

     Resp.TipoGP := fpTipo;
   end;
end;

function TACBrTEFDCliSiTef.ContinuarRequisicao(ImprimirComprovantes: Boolean
  ): Integer;
var
  Continua, ItemSelecionado, I: Integer;
  ProximoComando,TamanhoMinimo, TamanhoMaximo : SmallInt;
  TipoCampo: LongInt;
  Buffer: array [0..20000] of AnsiChar;
  Mensagem, MensagemOperador, MensagemCliente, CaptionMenu : String ;
  Resposta : AnsiString;
  SL : TStringList ;
  Interromper, Digitado, GerencialAberto, FechaGerencialAberto, ImpressaoOk,
     HouveImpressao, Voltar : Boolean ;
  Est : AnsiChar;

  function ProcessaMensagemTela( AMensagem : String ) : String ;
  begin
    Result := StringReplace( AMensagem, '@', sLineBreak, [rfReplaceAll] );
    Result := StringReplace( Result, '/n', sLineBreak, [rfReplaceAll] );
  end;

begin
   ProximoComando    := 0;
   TipoCampo         := 0;
   TamanhoMinimo     := 0;
   TamanhoMaximo     := 0;
   Continua          := 0;
   Mensagem          := '' ;
   MensagemOperador  := '' ;
   MensagemCliente   := '' ;
   CaptionMenu       := '' ;
   GerencialAberto   := False ;
   ImpressaoOk       := True ;
   HouveImpressao    := False ;
   fIniciouRequisicao:= True ;
   fCancelamento     := False ;
   fReimpressao      := False;
   Interromper       := False;
   fArqBackUp        := '' ;
   Resposta          := '' ;

   fpAguardandoResposta := True ;
   FechaGerencialAberto := True ;

   with TACBrTEFD(Owner) do
   begin
      try
         BloquearMouseTeclado( True );

         repeat
            GravaLog( 'ContinuaFuncaoSiTefInterativo, Chamando: Continua = '+
                      IntToStr(Continua)+' Buffer = '+Resposta ) ;

            Result := xContinuaFuncaoSiTefInterativo( ProximoComando,
                                                      TipoCampo,
                                                      TamanhoMinimo, TamanhoMaximo,
                                                      Buffer, sizeof(Buffer),
                                                      Continua );
            Continua := 0;
            Mensagem := TrimRight( Buffer ) ;
            Resposta := '' ;
            Voltar   := False;
            Digitado := True ;

            if fUsaUTF8 then
              Mensagem := ACBrStrToAnsi(Mensagem);  // Será convertido para UTF8 em TACBrTEFD.DoExibeMsg

            GravaLog( 'ContinuaFuncaoSiTefInterativo, Retornos: STS = '+IntToStr(Result)+
                      ' ProximoComando = '+IntToStr(ProximoComando)+
                      ' TipoCampo = '+IntToStr(TipoCampo)+
                      ' Buffer = '+Mensagem +
                      ' Tam.Min = '+IntToStr(TamanhoMinimo) +
                      ' Tam.Max = '+IntToStr(TamanhoMaximo)) ;

            if Result = 10000 then
            begin
              if TipoCampo > 0 then
                 Resposta := fRespostas.Values[IntToStr(TipoCampo)];

              case ProximoComando of
                 0 :
                   begin
                     TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, Mensagem ) ;

                     case TipoCampo of
                        15 : TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, 'True') ;//Selecionou Debito;
                        25 : TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, 'True') ;//Selecionou Credito;
                        29 : TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, 'True') ;//Cartão Digitado;
                        {Indica que foi escolhido menu de reimpressão}
                        56,57,58 : fReimpressao := True;
                        110      : fCancelamento:= True;
                        121, 122 :
                          if ImprimirComprovantes then
                          begin
                             { Impressão de Gerencial, deve ser Sob demanda }
                             if not HouveImpressao then
                             begin
                                HouveImpressao := True ;
                                fArqBackUp := CopiarResposta;
                             end;

                             SL := TStringList.Create;
                             ImpressaoOk := False ;
                             I := TipoCampo ;
                             try
                                while not ImpressaoOk do
                                begin
                                  try
                                    while I <= TipoCampo do
                                    begin
                                       if FechaGerencialAberto then
                                       begin
                                         Est := EstadoECF;

                                         { Fecha Vinculado ou Gerencial ou Cupom, se ficou algum aberto por Desligamento }
                                         case Est of
                                           'C'         : ComandarECF( opeFechaVinculado );
                                           'G','R'     : ComandarECF( opeFechaGerencial );
                                           'V','P','N' : ComandarECF( opeCancelaCupom );
                                         end;

                                         GerencialAberto      := False ;
                                         FechaGerencialAberto := False ;

                                         if EstadoECF <> 'L' then
                                           raise EACBrTEFDECF.Create( ACBrStr(CACBrTEFD_Erro_ECFNaoLivre) ) ;
                                       end;

                                       Mensagem := Self.Resp.LeInformacao(I).AsString ;
                                       Mensagem := StringToBinaryString( Mensagem ) ;
                                       if Mensagem <> '' then
                                       begin
                                          SL.Text  := StringReplace( Mensagem, #10, sLineBreak, [rfReplaceAll] ) ;

                                          if not GerencialAberto then
                                           begin
                                             ComandarECF( opeAbreGerencial ) ;
                                             GerencialAberto := True ;
                                           end
                                          else
                                           begin
                                             ComandarECF( opePulaLinhas ) ;
                                             DoExibeMsg( opmDestaqueVia,
                                                Format(CACBrTEFD_DestaqueVia, [1]) ) ;
                                           end;

                                          ECFImprimeVia( trGerencial, I-120, SL );

                                          ImpressaoOk := True ;
                                       end;

                                       Inc( I ) ;
                                    end;

                                    if (TipoCampo = 122) and GerencialAberto then
                                    begin
                                       ComandarECF( opeFechaGerencial );
                                       GerencialAberto := False;
                                    end;
                                  except
                                    on EACBrTEFDECF do ImpressaoOk := False ;
                                    else
                                       raise ;
                                  end;

                                  if not ImpressaoOk then
                                  begin
                                    if DoExibeMsg( opmYesNo, CACBrTEFD_Erro_ECFNaoResponde ) <> mrYes then
                                      break ;

                                    I := 121 ;
                                    FechaGerencialAberto := True ;
                                  end;
                                end ;
                             finally
                                if not ImpressaoOk then
                                   Continua := -1 ;

                                SL.Free;
                             end;
                          end ;
                        133, 952:
                        begin
                          fArqBackUp := CopiarResposta;
                        end;
                     end;
                   end;

                 1 :
                   begin
                     MensagemOperador := ProcessaMensagemTela( Mensagem );
                     DoExibeMsg( opmExibirMsgOperador, MensagemOperador, (TipoCampo=5005) ) ;
                   end ;

                 2 :
                   begin
                     MensagemCliente := ProcessaMensagemTela( Mensagem );
                     DoExibeMsg( opmExibirMsgCliente, MensagemCliente, (TipoCampo=5005) ) ;
                   end;

                 3 :
                   begin
                     MensagemOperador := ProcessaMensagemTela( Mensagem );
                     MensagemCliente  := MensagemOperador;
                     DoExibeMsg( opmExibirMsgOperador, MensagemOperador, (TipoCampo=5005) ) ;
                     DoExibeMsg( opmExibirMsgCliente, MensagemCliente, (TipoCampo=5005) ) ;
                   end ;

                 4 : CaptionMenu := ACBrStr( Mensagem ) ;

                 11 :
                   begin
                     MensagemOperador := '' ;
                     DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                   end;

                 12 :
                   begin
                     MensagemCliente := '' ;
                     DoExibeMsg( opmRemoverMsgCliente, '' ) ;
                   end;

                 13 :
                   begin
                     DoExibeMsg( opmRemoverMsgOperador, '' ) ;
                     DoExibeMsg( opmRemoverMsgCliente, '' ) ;
                   end ;

                 14 : CaptionMenu := '' ;
                   {Deve limpar o texto utilizado como cabeçalho na apresentação do menu}

                 20 :
                   begin
                     if Mensagem = '' then
                        Mensagem := 'CONFIRMA ?';

                     Resposta := ifThen( (DoExibeMsg( opmYesNo, Mensagem ) = mrYes), '0', '1' ) ;

                     // se a resposta a mensagem for não, não deixar interromper
                     // voltar ao loop
                     if (TipoCampo = 5013) and (Resposta = '1') then
                       Interromper := False;
                   end ;

                 21 :
                   begin
                     SL := TStringList.Create;
                     try
                        ItemSelecionado := -1 ;
                        SL.Text := StringReplace( Mensagem, ';',
                                                         sLineBreak, [rfReplaceAll] ) ;
                        BloquearMouseTeclado(False);
                        OnExibeMenu( CaptionMenu, SL, ItemSelecionado, Voltar ) ;
                        BloquearMouseTeclado(True);

                        if (not Voltar) then
                        begin
                           if (ItemSelecionado >= 0) and
                              (ItemSelecionado < SL.Count) then
                              Resposta := copy( SL[ItemSelecionado], 1,
                                             pos(':',SL[ItemSelecionado])-1 )
                           else
                              Digitado := False ;
                        end;
                     finally
                        SL.Free ;
                     end ;
                   end;

                 22 :
                   begin
                     if Mensagem = '' then
                        Mensagem := CACBrTEFD_CliSiTef_PressioneEnter;

                     DoExibeMsg( opmOK, Mensagem );
                   end ;

                 23 :
                   begin
                     Interromper := False ;
                     OnAguardaResp( '23', 0, Interromper ) ;
                     if Interromper then
                        Continua := -1 ;
                   end;

                 30 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcString, Resposta, Digitado, Voltar) ;
                     {Se o tipo campo for 505 é a quantidade de parcelas}
                     TACBrTEFDRespCliSiTef( Self.Resp ).GravaInformacao( TipoCampo, Resposta ) ;
                     BloquearMouseTeclado(True);
                   end;

                 31 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcCMC7, Resposta, Digitado, Voltar ) ;
                     BloquearMouseTeclado(True);
                   end;

                 34 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcDouble, Resposta, Digitado, Voltar ) ;
                     BloquearMouseTeclado(True);

                     // Garantindo que a Resposta é Float //
                     Resposta := FormatFloatBr( StringToFloatDef(Resposta, 0));
                   end;

                 35 :
                   begin
                     BloquearMouseTeclado(False);
                     OnObtemCampo( ACBrStr(Mensagem), TamanhoMinimo, TamanhoMaximo,
                                   TipoCampo, tcBarCode, Resposta, Digitado, Voltar ) ;
                     BloquearMouseTeclado(True);
                   end;

              end;
            end
            else
               GravaLog( '*** ContinuaFuncaoSiTefInterativo, Finalizando: STS = '+IntToStr(Result) ) ;

            if Voltar then
               Continua := 1     { Volta para o menu anterior }
            else if (not Digitado) or Interromper then
               Continua := -1 ;  { Cancela operacao }

            if (Voltar and (Result = 10000)) or (not Digitado) then
            begin
              DoExibeMsg( opmRemoverMsgOperador, '' ) ;
              DoExibeMsg( opmRemoverMsgCliente, '' ) ;
            end ;

            StrPCopy(Buffer, Resposta);

         until Result <> 10000;
      finally
         if GerencialAberto then
         try
            ComandarECF( opeFechaGerencial );
         except
            ImpressaoOk := False ;
         end;

         if (fArqBackUp <> '') and FileExists( fArqBackUp ) then
            SysUtils.DeleteFile( fArqBackUp );

         if HouveImpressao or ( ImprimirComprovantes and fCancelamento) then
            FinalizarTransacao( ImpressaoOk, Resp.DocumentoVinculado );

         BloquearMouseTeclado( False );

         { Transfere valore de "Conteudo" para as propriedades }
         if LogDebug then
            GravaLog( Self.Resp.Conteudo.Conteudo.Text );

         TACBrTEFDRespCliSiTef( Self.Resp ).ConteudoToProperty ;

         if (HouveImpressao and fCancelamento) then
            DoExibeMsg( opmOK,
                        Format( CACBrTEFD_CliSiTef_TransacaoEfetuadaReImprimir,
                                [Resp.NSU]) ) ;

         fpAguardandoResposta := False ;
      end;
   end ;
end;

function TACBrTEFDCliSiTef.CopiarResposta: string;
begin
  if Trim(fArqBackUp) <> '' then
  begin
    if FileExists(fArqBackUp) then
    begin
      if not SysUtils.DeleteFile(fArqBackUp) then
        raise EFilerError.CreateFmt('Não foi possivel apagar o arquivo "%s" de backup!', [fArqBackUp]);
    end;

    fArqBackUp := '';
  end;

  Result := inherited CopiarResposta;
end;

procedure TACBrTEFDCliSiTef.ProcessarResposta;
var
   RespostaPendente: TACBrTEFDRespCliSiTef;
begin
  GravaLog( Name +' ProcessarResposta: '+Req.Header );

  TACBrTEFD(Owner).EstadoResp := respProcessando;

  if Resp.QtdLinhasComprovante > 0 then
  begin
      { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
      RespostaPendente := TACBrTEFDRespCliSiTef.Create ;
      try
         RespostaPendente.Assign( Resp );
         TACBrTEFD(Owner).RespostasPendentes.Add( RespostaPendente );

         ImprimirRelatorio ;

         with TACBrTEFD(Owner) do
         begin
            if Assigned( OnDepoisConfirmarTransacoes ) then
               OnDepoisConfirmarTransacoes( RespostasPendentes );
         end ;
      finally
         TACBrTEFD(Owner).RespostasPendentes.Clear;
      end;
  end ;
end;

procedure TACBrTEFDCliSiTef.FinalizarTransacao( Confirma : Boolean;
   DocumentoVinculado : AnsiString);
Var
   DataStr, HoraStr : AnsiString;
   Finalizacao : Integer;
   AMsg: String;
   Est: AnsiChar;
begin
   fRespostas.Clear;
   fIniciouRequisicao := False;

   { Re-Impressão não precisa de Finalização }
   if fReimpressao then
      exit ;

   { Já Finalizou este Documento por outra Transação ? }
   if (pos(DocumentoVinculado, fDocumentosProcessados) > 0) then
      exit;

  fDocumentosProcessados := fDocumentosProcessados + DocumentoVinculado + '|' ;

  DataStr     := FormatDateTime('YYYYMMDD',Now);
  HoraStr     := FormatDateTime('HHNNSS',Now);
  Finalizacao := ifthen(Confirma or fCancelamento,1,0);

  GravaLog( '*** FinalizaTransacaoSiTefInterativo. Confirma: '+
                                          IfThen(Finalizacao = 1,'SIM','NAO')+
                                          ' Documento: ' +DocumentoVinculado+
                                          ' Data: '      +DataStr+
                                          ' Hora: '      +HoraStr ) ;

  xFinalizaTransacaoSiTefInterativo( Finalizacao,
                                     PAnsiChar( DocumentoVinculado ),
                                     PAnsiChar( DataStr ),
                                     PAnsiChar( HoraStr ) ) ;

  if not Confirma then
  begin
     if fCancelamento then
        AMsg := Format( CACBrTEFD_CliSiTef_TransacaoEfetuadaReImprimir, [Resp.NSU])
     else
     begin
        try
           Est := TACBrTEFD(Owner).EstadoECF;
        except
           Est := 'O' ;
        end;

        if (Est = 'O') then
           AMsg := CACBrTEFD_CliSiTef_TransacaoNaoEfetuada
        else
           AMsg := CACBrTEFD_CliSiTef_TransacaoNaoEfetuadaReterCupom;
     end;

     TACBrTEFD(Owner).DoExibeMsg( opmOK, AMsg );
  end;
end;

{ Valores de Retorno:
  0 - se o código estiver correto;
  1 a 4 - Indicando qual o bloco que está com erro
  5 - Um ou mais blocos com erro

  Tipo: tipo de documento sendo coletado:
        -1: Ainda não identificado
        0: Arrecadação
        1: Titulo
}
function TACBrTEFDCliSiTef.ValidaCampoCodigoEmBarras(Dados: AnsiString;
  var Tipo: SmallInt): Integer;
begin
  GravaLog('ValidaCodigoEmBarras -> Dados:' + Dados, True);

  if Assigned(xValidaCampoCodigoEmBarras) then
    Result := xValidaCampoCodigoEmBarras(Dados,Tipo)
  else
    raise EACBrTEFDErro.Create(ACBrStr(CACBrTEFD_CliSiTef_NaoInicializado));
end;

procedure TACBrTEFDCliSiTef.AvaliaErro( Sts : Integer );
var
   Erro : String;
begin
   if not fExibirErroRetorno then
     Exit;

   Erro := '' ;
   Case Sts of
        -1 : Erro := 'Módulo não inicializado' ;
        -2 : Erro := 'Operação cancelada pelo operador' ;
        -3 : Erro := 'Fornecido um código de função inválido' ;
        -4 : Erro := 'Falta de memória para rodar a função' ;
        -5 : Erro := '' ; // 'Sem comunicação com o SiTef' ; // Comentado pois SiTEF já envia a msg de Erro
        -6 : Erro := 'Operação cancelada pelo usuário' ;
        -40: Erro := 'Transação negada pelo SiTef';
        -43: Erro := 'Falha no pinpad';
        -50: Erro := 'Transação não segura';
        -100: Erro := 'Erro interno do módulo';
   else
      if Sts < 0 then
         Erro := 'Erros detectados internamente pela rotina ('+IntToStr(Sts)+')'
      else
         Erro := 'Negada pelo autorizador ('+IntToStr(Sts)+')' ;
   end;

   if Erro <> '' then
      TACBrTEFD(Owner).DoExibeMsg( opmOK, Erro );
end ;

function TACBrTEFDCliSiTef.GetDataHoraFiscal: TDateTime;
begin
  if (csDesigning in Owner.ComponentState) then
     Result := fDataHoraFiscal
  else
     if fDataHoraFiscal = 0 then
        Result := Now
     else
        Result := fDataHoraFiscal;
end;

function TACBrTEFDCliSiTef.GetDocumentoFiscal: AnsiString;
begin
  if (csDesigning in Owner.ComponentState) then
     Result := fDocumentoFiscal
  else
     if fDocumentoFiscal = '' then
        Result := HMS
     else
        Result := fDocumentoFiscal;
end;


function TACBrTEFDCliSiTef.ProcessarRespostaPagamento(
  const IndiceFPG_ECF: String; const Valor: Double): Boolean;
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
     RespostaPendente := TACBrTEFDRespCliSiTef.Create ;
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
                 if DoExibeMsg( opmYesNo, CACBrTEFD_Erro_ECFNaoResponde ) <> mrYes then
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

procedure TACBrTEFDCliSiTef.VerificarIniciouRequisicao;
begin
  if not fIniciouRequisicao then
     raise EACBrTEFDErro.Create( ACBrStr( CACBrTEFD_Erro_SemRequisicao ) ) ;
end;

function TACBrTEFDCliSiTef.SuportaDesconto: Boolean;
begin
  with TACBrTEFD(Owner) do
  begin
     Result := (Identificacao.SoftwareHouse <> '') and
               Assigned( OnComandaECFSubtotaliza ) and
               (not AutoEfetuarPagamento) ;
  end;
end;

function TACBrTEFDCliSiTef.HMS: String;
begin
   Result := FormatDateTime('hhmmss',Now);
end;

function TACBrTEFDCliSiTef.VerificaPresencaPinPad: Boolean;
begin
  GravaLog('VerificaPresencaPinpad', True);

  {
   Retornos:
      1: Existe um PinPad operacional conectado ao micro;
      0: Nao Existe um PinPad conectado ao micro;
     -1: Biblioteca de acesso ao PinPad não encontrada }

  if Assigned(xVerificaPresencaPinPad) then
    Result := ( xVerificaPresencaPinPad() = 1 )
  else
    raise EACBrTEFDErro.Create(ACBrStr(CACBrTEFD_CliSiTef_NaoInicializado));
end;


end.
