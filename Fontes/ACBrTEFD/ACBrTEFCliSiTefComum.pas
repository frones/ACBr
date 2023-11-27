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

unit ACBrTEFCliSiTefComum;

interface

uses
  Classes, SysUtils,
  ACBrTEFComum;

resourcestring
  CACBrTEFCliSiTef_PressioneEnter = 'PRESSIONE <ENTER>';
  CACBrTEFCliSiTef_TransacaoNaoEfetuada = 'Transação não efetuada.';
  CACBrTEFCliSiTef_TransacaoNaoEfetuadaReterCupom =
    'Transação não efetuada.' + sLineBreak + 'Favor reter o Cupom';
  CACBrTEFCliSiTef_TransacaoEfetuadaReImprimir =
    'Transação TEF efetuada.' + sLineBreak +
    'Favor reimprimir último Cupom.' + sLineBreak +
    '%s' + sLineBreak +
    '(Para Cielo utilizar os 6 últimos dígitos.)';
  CACBrTEFCliSiTef_NaoInicializado = 'CliSiTEF não inicializado';
  CACBrTEFCliSiTef_NaoConcluido = 'Requisição anterior não concluida';
  CACBrTEFCliSiTef_Erro1 = 'Endereço IP inválido ou não resolvido';
  CACBrTEFCliSiTef_Erro2 = 'Código da loja inválido';
  CACBrTEFCliSiTef_Erro3 = 'Código de terminal inválido';
  CACBrTEFCliSiTef_Erro6 = 'Erro na inicialização do TCP/IP';
  CACBrTEFCliSiTef_Erro7 = 'Falta de memória';
  CACBrTEFCliSiTef_Erro8 = 'Não encontrou a CliSiTef ou ela está com problemas';
  CACBrTEFCliSiTef_Erro10 = 'Erro de acesso na pasta CliSiTef (possível falta de permissão para escrita) ' + sLineBreak +
    'ou o PinPad não está devidamente configurado no arquivo CliSiTef.ini ' + sLineBreak +
    'ou parâmetros IdLoja e IdTerminal inválidos';
  CACBrTEFCliSiTef_Erro11 = 'Dados inválidos passados pela automação.';
  CACBrTEFCliSiTef_Erro12 = 'Modo seguro não ativo (possível falta de configuração no servidor SiTef do arquivo .cha).';
  CACBrTEFCliSiTef_Erro13 = 'Caminho da DLL inválido (o caminho completo das bibliotecas está muito grande).';

const
  CACBrTEFCliSiTef_ImprimeGerencialConcomitante = False;
{$IFDEF LINUX}
  CACBrTEFCliSiTef_Lib = 'libclisitef.so';
{$ELSE}
  {$IFDEF WIN64}
   CACBrTEFCliSiTef_Lib = 'CliSiTef64I.dll';
  {$ELSE}
   CACBrTEFCliSiTef_Lib = 'CliSiTef32I.dll';
  {$ENDIF}
{$ENDIF}


type

  { TACBrTEFRespCliSiTef }

  TACBrTEFRespCliSiTef = class(TACBrTEFResp)
  public
    procedure ConteudoToProperty; override;
    procedure GravaInformacao(const Identificacao: Integer; const Informacao: AnsiString);
  end;

  { TACBrTEFCliSiTefAPI }

  TACBrTEFCliSiTefAPI = class
  private
    fInicializada: Boolean;
    fPathDLL: string;

    xConfiguraIntSiTefInterativoEx : function (
               pEnderecoIP: PAnsiChar;
               pCodigoLoja: PAnsiChar;
               pNumeroTerminal: PAnsiChar;
               ConfiguraResultado: smallint;
               pParametrosAdicionais: PAnsiChar): integer;
               {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xIniciaFuncaoSiTefInterativo : function (
               Modalidade: integer;
               pValor: PAnsiChar;
               pNumeroCuponFiscal: PAnsiChar;
               pDataFiscal: PAnsiChar;
               pHorario: PAnsiChar;
               pOperador: PAnsiChar;
               pRestricoes: PAnsiChar ): integer;
               {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xFinalizaFuncaoSiTefInterativo : procedure (
                pConfirma: SmallInt;
                pCupomFiscal: PAnsiChar;
                pDataFiscal: PAnsiChar;
                pHoraFiscal: PAnsiChar;
                pParamAdic: PAnsiChar);
                {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xContinuaFuncaoSiTefInterativo : function (
               var ProximoComando: SmallInt;
               var TipoCampo: LongInt;
               var TamanhoMinimo: SmallInt;
               var TamanhoMaximo: SmallInt;
               pBuffer: PAnsiChar;
               TamMaxBuffer: Integer;
               ContinuaNavegacao: Integer ): integer;
               {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xEscreveMensagemPermanentePinPad: function(Mensagem:PAnsiChar):Integer;
               {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xObtemQuantidadeTransacoesPendentes: function(
               DataFiscal:AnsiString;
               NumeroCupon:AnsiString):Integer;
               {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

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
               {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xValidaCampoCodigoEmBarras: function(
               Dados: AnsiString;
               var Tipo: SmallInt): Integer;
               {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xVerificaPresencaPinPad: function(): Integer
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xObtemDadoPinPadDiretoEx: function(ChaveAcesso: PAnsiChar;
              Identificador: PAnsiChar; Entrada: PAnsiChar; Saida: PAnsiChar): Integer;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    xLeDigitoPinPad: function(MensagemDisplay: PAnsiChar;
              NumeroDigitado: PAnsiChar): Integer;
              {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    procedure SetInicializada(AValue: Boolean);

    procedure LoadDLLFunctions;
    procedure UnLoadDLLFunctions;
  public
    constructor Create;
    destructor Destroy; override;

    function TraduzirErroInicializacao(Sts : Integer): String;
    function TraduzirErroTransacao(Sts : Integer): String;

    function ConfiguraIntSiTefInterativo(
               pEnderecoIP: PAnsiChar;
               pCodigoLoja: PAnsiChar;
               pNumeroTerminal: PAnsiChar;
               ConfiguraResultado: smallint;
               pParametrosAdicionais: PAnsiChar): integer;

    function IniciaFuncaoSiTefInterativo(
               Modalidade: integer;
               pValor: PAnsiChar;
               pNumeroCuponFiscal: PAnsiChar;
               pDataFiscal: PAnsiChar;
               pHorario: PAnsiChar;
               pOperador: PAnsiChar;
               pParamAdic: PAnsiChar ): integer;

    function ContinuaFuncaoSiTefInterativo(
               var ProximoComando: SmallInt;
               var TipoCampo: LongInt;
               var TamanhoMinimo: SmallInt;
               var TamanhoMaximo: SmallInt;
               pBuffer: PAnsiChar;
               TamMaxBuffer: Integer;
               ContinuaNavegacao: Integer ): integer;

    procedure FinalizaFuncaoSiTefInterativo(
                pConfirma: SmallInt;
                pCupomFiscal: PAnsiChar;
                pDataFiscal: PAnsiChar;
                pHoraFiscal: PAnsiChar;
                pParamAdic: PAnsiChar);

    Function DefineMensagemPermanentePinPad(Mensagem:AnsiString):Integer;
    Function ObtemQuantidadeTransacoesPendentes(Data:TDateTime;
       CupomFiscal:AnsiString):Integer;
    function EnviaRecebeSiTefDireto(RedeDestino: SmallInt; FuncaoSiTef: SmallInt;
      OffsetCartao: SmallInt; DadosTx: AnsiString; var DadosRx: AnsiString;
      var CodigoResposta: SmallInt; TempoEsperaRx: SmallInt;
      CupomFiscal: AnsiString; Confirmar: Boolean; Operador: AnsiString): Integer;
    function ValidaCampoCodigoEmBarras(Dados: AnsiString;
       var Tipo: SmallInt): Integer;
    function VerificaPresencaPinPad: Boolean;
    function ObtemDadoPinPadDiretoEx(TipoDocumento: Integer; ChaveAcesso,
      Identificador: AnsiString): AnsiString;

    function LeDigitoPinPad(MensagemDisplay: AnsiString): AnsiString;

    property PathDLL: string read fPathDLL write fPathDLL;
    property Inicializada: Boolean read fInicializada write SetInicializada;
  end;

procedure ConteudoToPropertyCliSiTef(AACBrTEFResp: TACBrTEFResp);

implementation

uses
  strutils, Math, dateutils,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.Math,
  ACBrUtil.FilesIO;

procedure ConteudoToPropertyCliSiTef(AACBrTEFResp: TACBrTEFResp);
var
  I, wTipoOperacao, wNumCB: Integer;
  wTotalParc, wValParc: Double;
  Parc: TACBrTEFRespParcela;
  Linha: TACBrTEFLinha;
  CB: TACBrTEFRespCB;
  LinStr: AnsiString;
begin
  with AACBrTEFResp do
  begin
    ValorTotal := 0;
    ImagemComprovante1aVia.Clear;
    ImagemComprovante2aVia.Clear;
    Debito := False;
    Credito := False;
    Voucher := False;
    Digitado := False;

    for I := 0 to Conteudo.Count - 1 do
    begin
      Linha := Conteudo.Linha[I];
      LinStr := StringToBinaryString(Linha.Informacao.AsString);

      case Linha.Identificacao of
        29: Digitado := (LinStr = 'True');
        // TODO: Mapear mais propriedades do CliSiTef //
        100:
        begin
          ModalidadePagto := LinStr;

          case StrToIntDef(Copy(ModalidadePagto, 1, 2), 0) of
            01: Debito := True;
            02: Credito := True;
            03: Voucher := True;
          end;

          wTipoOperacao := StrToIntDef(Copy(ModalidadePagto, 3, 2), 0);
          {Tipo de Parcelamento}
          case wTipoOperacao of
            02: ParceladoPor := parcLoja;
            03: ParceladoPor := parcADM;
          end;

          case wTipoOperacao of
            00: TipoOperacao := opAvista;
            01: TipoOperacao := opPreDatado;
            02, 03: TipoOperacao := opParcelado;
          else
            TipoOperacao := opOutras;
          end;
        end;

        101: ModalidadePagtoExtenso := LinStr;
        102: ModalidadePagtoDescrita := LinStr;
        105:
        begin
          DataHoraTransacaoComprovante := Linha.Informacao.AsTimeStampSQL;
          DataHoraTransacaoHost := DataHoraTransacaoComprovante;
          DataHoraTransacaoLocal := DataHoraTransacaoComprovante;
        end;

        106: IdCarteiraDigital := LinStr;
        107: NomeCarteiraDigital := LinStr;

        120: Autenticacao := LinStr;
        121: ImagemComprovante1aVia.Text := ChangeLineBreak(LinStr, sLineBreak);
        122: ImagemComprovante2aVia.Text := ChangeLineBreak(LinStr, sLineBreak);
        123: TipoTransacao := Linha.Informacao.AsInteger;
        130:
        begin
          Saque := Linha.Informacao.AsFloat;
          ValorTotal := ValorTotal + Saque;
        end;

        131: Instituicao := LinStr;
        132: CodigoBandeiraPadrao := LinStr;
        133: NSU_TEF := LinStr;
        134: NSU := LinStr;
        135: CodigoAutorizacaoTransacao := LinStr;
        136: Bin := LinStr;
        139: ValorEntradaCDC := Linha.Informacao.AsFloat;
        140: DataEntradaCDC := Linha.Informacao.AsDate;
        156: Rede := LinStr;
        157: Estabelecimento := LinStr;
        158: CodigoRedeAutorizada := LinStr; 
        161: IdPagamento := StrToInt(LinStr); { indice de pagamento naquela operação }
        501: TipoPessoa := AnsiChar(IfThen(Linha.Informacao.AsInteger = 0, 'J', 'F')[1]);
        502: DocumentoPessoa := LinStr;
        504: TaxaServico := Linha.Informacao.AsFloat;
        505: QtdParcelas := Linha.Informacao.AsInteger;
        506: DataPreDatado := Linha.Informacao.AsDate;
        511: QtdParcelas := Linha.Informacao.AsInteger;  {Parcelas CDC - Neste caso o campo 505 não é retornado}
        515: DataHoraTransacaoCancelada := Linha.Informacao.AsDate;
        516: NSUTransacaoCancelada := LinStr;
        527: DataVencimento := Linha.Informacao.AsDate;         { Data Vencimento do Cheque }
        584: QRCode := LinStr;
        589: CodigoOperadoraCelular := LinStr;                  { Código da Operadora de Celular }
        590: NomeOperadoraCelular := LinStr;                    { Nome da Operadora de Celular }
        591: ValorRecargaCelular := Linha.Informacao.AsFloat;   { Valor selecionado para a Recarga }
        592: NumeroRecargaCelular := LinStr;                    { Numero de Celular informado para Recarda }

        607:  // Indice do Correspondente Bancário
        begin
          wNumCB := Linha.Informacao.AsInteger;

          if (wNumCB = 1) then
            CorrespBancarios.Clear;

          CB := TACBrTEFRespCB.Create;
          CB.DataVencimento := LeInformacao(600, wNumCB).AsDate;   { Data Vencimento do título - CB }
          CB.ValorPago := LeInformacao(601, wNumCB).AsFloat;       { Valor Pago do título - CB }
          CB.ValorOriginal := LeInformacao(602, wNumCB).AsFloat;   { Valor Original do título - CB }
          CB.Acrescimo := LeInformacao(603, wNumCB).AsFloat;       { Valor do Acréscimo - CB }
          CB.Desconto := LeInformacao(604, wNumCB).AsFloat;        { Valor do Desconto - CB }
          CB.DataPagamento := LeInformacao(605, wNumCB).AsDate;    { Data contábil do Pagamento - CB }
          CB.NSUTransacaoCB := LeInformacao(611, wNumCB).AsString; { NSU da Transação CB }
          CB.TipoDocumento := LeInformacao(612, wNumCB).AsInteger; { Tipo Docto CB - 0:Arrecadação/ 1:Título/ 2:Tributo }
          CB.NSUCancelamento := LeInformacao(623, wNumCB).AsString;{ NSU para cancelamento - CB }
          CB.Documento := LeInformacao(624, wNumCB).AsString;      { Linha Digitável/Código de Barras do documento pago}

          CorrespBancarios.Add(CB);
        end;

        609: CorrespBancarios.TotalTitulos := Linha.Informacao.AsFloat;       { Valor total dos títulos efetivamente pagos no caso de pagamento em lote }
        610: CorrespBancarios.TotalTitulosNaoPago := Linha.Informacao.AsFloat;{ Valor total dos títulos NÃO pagos no caso de pagamento em lote }
        613:
        begin
          Cheque := copy(LinStr, 21, 6);
          CMC7 := LinStr;
        end;

        626: Banco := LinStr;
        627: Agencia := LinStr;
        628: AgenciaDC := LinStr;
        629: Conta := LinStr;
        630: ContaDC := LinStr;

        // dados de retorno da NFC-e/SAT
        950: NFCeSAT.CNPJCredenciadora := LinStr;
        951: NFCeSAT.Bandeira := LinStr;
        952: NFCeSAT.Autorizacao := LinStr;
        953: NFCeSAT.CodCredenciadora := LinStr;
        1002: NFCeSAT.DataExpiracao := LinStr;
        1003: NFCeSAT.DonoCartao := LinStr;
        1190: NFCeSAT.UltimosQuatroDigitos := LinStr;
        4029:
        begin
          Desconto := Linha.Informacao.AsFloat;
          ValorTotal := ValorTotal - Desconto;
        end;
        4153: CodigoPSP := LinStr;
      else
        ProcessarTipoInterno(Linha);
      end;
    end;

    QtdLinhasComprovante := max(ImagemComprovante1aVia.Count, ImagemComprovante2aVia.Count);
    Confirmar := (QtdLinhasComprovante > 0);
    Sucesso := (NSU_TEF <> '') or Confirmar;

    // leitura de parcelas conforme nova documentação
    // 141 e 142 foram removidos em Setembro de 2014
    Parcelas.Clear;
    if (QtdParcelas > 0) then
    begin
      wValParc := RoundABNT((ValorTotal / QtdParcelas), -2);
      wTotalParc := 0;

      for I := 1 to QtdParcelas do
      begin
        Parc := TACBrTEFRespParcela.Create;
        if I = 1 then
        begin
          Parc.Vencimento := LeInformacao(140, I).AsDate;
          Parc.Valor := LeInformacao(524, I).AsFloat;
        end
        else
        begin
          Parc.Vencimento := IncDay(LeInformacao(140, I).AsDate, LeInformacao(508, I).AsInteger);
          Parc.Valor := LeInformacao(525, I).AsFloat;
        end;

        // caso não retorne os dados acima prencher com os defaults
        if Trim(Parc.NSUParcela) = '' then
          Parc.NSUParcela := NSU;

        if Parc.Vencimento <= 0 then
          Parc.Vencimento := IncDay(DataHoraTransacaoHost, I * 30);

        if Parc.Valor <= 0 then
        begin
          if (I = QtdParcelas) then
            wValParc := ValorTotal - wTotalParc
          else
            wTotalParc := wTotalParc + wValParc;

          Parc.Valor := wValParc;
        end;

        Parcelas.Add(Parc);
      end;
    end;
  end;
end;

{ TACBrTEFRespCliSiTef }

procedure TACBrTEFRespCliSiTef.ConteudoToProperty;
begin
  ConteudoToPropertyCliSiTef(Self);
end;

procedure TACBrTEFRespCliSiTef.GravaInformacao(const Identificacao: Integer; const Informacao: AnsiString);
var
  Sequencia: Integer;
  AsString: String;
begin
  Sequencia := 0;

  { Os Tipos abaixo, devem ter a Sequencia incrementada, se já foram salvos antes,
    pois o SiTef retorna o mesmo Tipo, para várias ocorrências do campo }
  case Identificacao of
    141, 142,            // 141 - Data Parcela, 142 - Valor Parcela
    600..607, 611..624:  // Dados do Corresp. Bancário
    begin
      Sequencia := 1;
      while (Trim(LeInformacao(Identificacao, Sequencia).AsString) <> '') do
        Inc(Sequencia);
    end;
  end;

  AsString := BinaryStringToString(Informacao);  // Converte #10 para "\x0A"
  fpConteudo.GravaInformacao(Identificacao, Sequencia, AsString);
end;


{ TACBrTEFCliSiTefAPI }

constructor TACBrTEFCliSiTefAPI.Create;
begin
  inherited;

  xConfiguraIntSiTefInterativoEx      := nil;
  xIniciaFuncaoSiTefInterativo        := nil;
  xContinuaFuncaoSiTefInterativo      := nil;
  xFinalizaFuncaoSiTefInterativo      := nil;
  xEscreveMensagemPermanentePinPad    := nil;
  xObtemQuantidadeTransacoesPendentes := nil;
  xValidaCampoCodigoEmBarras          := nil;
  xEnviaRecebeSiTefDireto             := nil;
  xVerificaPresencaPinPad             := nil;
  xObtemDadoPinPadDiretoEx            := nil;
  xLeDigitoPinPad                     := nil;
end;

destructor TACBrTEFCliSiTefAPI.Destroy;
begin
  UnLoadDLLFunctions;
  inherited Destroy;
end;

procedure TACBrTEFCliSiTefAPI.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  if AValue then
    LoadDLLFunctions
  else
    UnLoadDLLFunctions;
end;

procedure TACBrTEFCliSiTefAPI.LoadDLLFunctions ;
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
     sLibName := sLibName + CACBrTEFCliSiTef_Lib;

     if not FunctionDetect( sLibName, FuncName, LibPointer) then
     begin
       LibPointer := NIL ;
       raise EACBrTEFErro.Create( ACBrStr( 'Erro ao carregar a função:'+FuncName+
                                           ' de: '+CACBrTEFCliSiTef_Lib ) ) ;
     end ;
   end ;
 end ;
begin
  if fInicializada then
    Exit;

  CliSiTefFunctionDetect('ConfiguraIntSiTefInterativoEx', @xConfiguraIntSiTefInterativoEx);
  CliSiTefFunctionDetect('IniciaFuncaoSiTefInterativo', @xIniciaFuncaoSiTefInterativo);
  CliSiTefFunctionDetect('ContinuaFuncaoSiTefInterativo', @xContinuaFuncaoSiTefInterativo);
  CliSiTefFunctionDetect('FinalizaFuncaoSiTefInterativo', @xFinalizaFuncaoSiTefInterativo);
  CliSiTefFunctionDetect('EscreveMensagemPermanentePinPad',@xEscreveMensagemPermanentePinPad);
  CliSiTefFunctionDetect('ObtemQuantidadeTransacoesPendentes',@xObtemQuantidadeTransacoesPendentes);
  CliSiTefFunctionDetect('ValidaCampoCodigoEmBarras',@xValidaCampoCodigoEmBarras);
  CliSiTefFunctionDetect('EnviaRecebeSiTefDireto',@xEnviaRecebeSiTefDireto);
  CliSiTefFunctionDetect('VerificaPresencaPinPad',@xVerificaPresencaPinPad);
  CliSiTefFunctionDetect('ObtemDadoPinPadDiretoEx', @xObtemDadoPinPadDiretoEx);
  CliSiTefFunctionDetect('LeDigitoPinPad', @xLeDigitoPinPad);

  fInicializada := True;
end ;

procedure TACBrTEFCliSiTefAPI.UnLoadDLLFunctions;
var
   sLibName: String;
begin
  if not fInicializada then
    Exit;

  sLibName := '';
  if Length(PathDLL) > 0 then
     sLibName := PathWithDelim(PathDLL);

  UnLoadLibrary( sLibName + CACBrTEFCliSiTef_Lib );

  xConfiguraIntSiTefInterativoEx      := Nil;
  xIniciaFuncaoSiTefInterativo        := Nil;
  xContinuaFuncaoSiTefInterativo      := Nil;
  xFinalizaFuncaoSiTefInterativo      := Nil;
  xEscreveMensagemPermanentePinPad    := Nil;
  xObtemQuantidadeTransacoesPendentes := Nil;
  xValidaCampoCodigoEmBarras          := Nil;
  xEnviaRecebeSiTefDireto             := Nil;
  xVerificaPresencaPinPad             := Nil;
  xObtemDadoPinPadDiretoEx            := Nil;
  xLeDigitoPinPad                     := Nil;

  fInicializada := False;
end;

function TACBrTEFCliSiTefAPI.TraduzirErroInicializacao(Sts: Integer): String;
begin
  Case Sts of
     1 : Result := CACBrTEFCliSiTef_Erro1;
     2 : Result := CACBrTEFCliSiTef_Erro2;
     3 : Result := CACBrTEFCliSiTef_Erro3;
     6 : Result := CACBrTEFCliSiTef_Erro6;
     7 : Result := CACBrTEFCliSiTef_Erro7;
     8 : Result := CACBrTEFCliSiTef_Erro8;
    10 : Result := CACBrTEFCliSiTef_Erro10;
    11 : Result := CACBrTEFCliSiTef_Erro11;
    12 : Result := CACBrTEFCliSiTef_Erro12;
    13 : Result := CACBrTEFCliSiTef_Erro13;
  else
    Result := '';
  end;
end;

function TACBrTEFCliSiTefAPI.TraduzirErroTransacao(Sts: Integer): String;
begin
   Result := '' ;
   Case Sts of
      0 : Result := '';
     -1 : Result := 'Módulo não inicializado' ;
     -2 : Result := 'Operação cancelada pelo operador' ;
     -3 : Result := 'Fornecido um código de função inválido' ;
     -4 : Result := 'Falta de memória para rodar a função' ;
     -5 : Result := 'Sem comunicação com o SiTef' ;
     -6 : Result := 'Operação cancelada pelo usuário no PinPad' ;
     -8 : Result := 'A CliSiTef não possui a implementação da função necessária, provavelmente está desatualizada';
     -9 : Result := 'A automação chamou a rotina ContinuaFuncaoSiTefInterativo sem antes iniciar uma função iterativa';
     -10: Result := 'Algum parâmetro obrigatório não foi passado pela automação comercial';
     -12: Result := 'Erro na execução da rotina iterativa. Provavelmente o processo iterativo anterior não foi executado até o final';
     -13: Result := 'Documento fiscal não encontrado nos registros da CliSiTef.';
     -15: Result := 'Operação cancelada pela automação comercial';
     -20: Result := 'Parâmetro inválido passado para a função';
     -21: Result := 'Utilizada uma palavra proibida, por exemplo SENHA, para coletar dados em aberto no pinpad.';
     -25: Result := 'Erro no Correspondente Bancário: Deve realizar sangria.';
     -30: Result := 'Erro de acesso a arquivo';
     -40: Result := 'Transação negada pelo SiTef';
     -41: Result := 'Dados inválidos';
     -43: Result := 'Problema na execução de alguma das rotinas no pinpad';
     -50: Result := 'Transação não segura';
     -100:Result := 'Result interno do módulo';
   else
     if Sts < 0 then
       Result := 'Erros detectados internamente pela rotina ('+IntToStr(Sts)+')'
     else
       Result := 'Negada pelo autorizador ('+IntToStr(Sts)+')' ;
   end;
end ;

function TACBrTEFCliSiTefAPI.ConfiguraIntSiTefInterativo(
  pEnderecoIP: PAnsiChar; pCodigoLoja: PAnsiChar; pNumeroTerminal: PAnsiChar;
  ConfiguraResultado: smallint; pParametrosAdicionais: PAnsiChar): integer;
begin
  LoadDLLFunctions;
  if Assigned(xConfiguraIntSiTefInterativoEx) then
    Result := xConfiguraIntSiTefInterativoEx( pEnderecoIP, pCodigoLoja, pNumeroTerminal,
                               ConfiguraResultado, pParametrosAdicionais)
  else
    Result := -1;
end;

function TACBrTEFCliSiTefAPI.IniciaFuncaoSiTefInterativo(Modalidade: integer;
  pValor: PAnsiChar; pNumeroCuponFiscal: PAnsiChar; pDataFiscal: PAnsiChar;
  pHorario: PAnsiChar; pOperador: PAnsiChar; pParamAdic: PAnsiChar): integer;
begin
  LoadDLLFunctions;
  if Assigned(xIniciaFuncaoSiTefInterativo) then
    Result := xIniciaFuncaoSiTefInterativo( Modalidade, pValor, pNumeroCuponFiscal,
                               pDataFiscal, pHorario, pOperador, pParamAdic)
  else
    Result := -1;
end;

function TACBrTEFCliSiTefAPI.ContinuaFuncaoSiTefInterativo(
  var ProximoComando: SmallInt; var TipoCampo: LongInt;
  var TamanhoMinimo: SmallInt; var TamanhoMaximo: SmallInt; pBuffer: PAnsiChar;
  TamMaxBuffer: Integer; ContinuaNavegacao: Integer): integer;
begin
  LoadDLLFunctions;
  if Assigned(xContinuaFuncaoSiTefInterativo) then
    Result := xContinuaFuncaoSiTefInterativo( ProximoComando, TipoCampo, TamanhoMinimo,
                               TamanhoMaximo, pBuffer, TamMaxBuffer, ContinuaNavegacao)
  else
    Result := -1;
end;

procedure TACBrTEFCliSiTefAPI.FinalizaFuncaoSiTefInterativo(
  pConfirma: SmallInt; pCupomFiscal: PAnsiChar; pDataFiscal: PAnsiChar;
  pHoraFiscal: PAnsiChar; pParamAdic: PAnsiChar);
begin
 LoadDLLFunctions;
 if Assigned(xFinalizaFuncaoSiTefInterativo) then
   xFinalizaFuncaoSiTefInterativo( pConfirma, pCupomFiscal, pDataFiscal,
                                   pHoraFiscal, pParamAdic);
end;

function TACBrTEFCliSiTefAPI.DefineMensagemPermanentePinPad(Mensagem:AnsiString):Integer;
begin
  LoadDLLFunctions;
  if Assigned(xEscreveMensagemPermanentePinPad) then
    Result := xEscreveMensagemPermanentePinPad(PAnsiChar(Mensagem))
  else
    Result := -1;
end;


function TACBrTEFCliSiTefAPI.ObtemQuantidadeTransacoesPendentes(Data:TDateTime;
  CupomFiscal: AnsiString): Integer;
var
  sDate:AnsiString;
begin
  LoadDLLFunctions;
  sDate:= FormatDateTime('yyyymmdd',Data);
  if Assigned(xObtemQuantidadeTransacoesPendentes) then
    Result := xObtemQuantidadeTransacoesPendentes(sDate,CupomFiscal)
  else
    Result := -1;
end;

function TACBrTEFCliSiTefAPI.EnviaRecebeSiTefDireto(RedeDestino: SmallInt;
  FuncaoSiTef: SmallInt; OffsetCartao: SmallInt; DadosTx: AnsiString;
  var DadosRx: AnsiString; var CodigoResposta: SmallInt;
  TempoEsperaRx: SmallInt; CupomFiscal: AnsiString; Confirmar: Boolean;
  Operador: AnsiString): Integer;
var
  Buffer: array [0..20000] of AnsiChar;
  ANow: TDateTime;
  DataStr, HoraStr: String;
begin
  LoadDLLFunctions;
  ANow    := Now ;
  DataStr := FormatDateTime('YYYYMMDD', ANow );
  HoraStr := FormatDateTime('HHNNSS', ANow );
  Buffer := #0;
  FillChar(Buffer, SizeOf(Buffer), 0);

  if Assigned(xEnviaRecebeSiTefDireto) then
    Result := xEnviaRecebeSiTefDireto( RedeDestino,
                             FuncaoSiTef,
                             OffsetCartao,
                             DadosTx,
                             Length(DadosTx)+1,
                             Buffer,
                             SizeOf(Buffer),
                             CodigoResposta,
                             TempoEsperaRx,
                             CupomFiscal, DataStr, HoraStr, Operador,
                             IfThen(Confirmar,1,0) )
    else
      Result := -1;

  DadosRx := TrimRight( LeftStr(Buffer,max(Result,0)) ) ;
end;

function TACBrTEFCliSiTefAPI.ValidaCampoCodigoEmBarras(Dados: AnsiString;
  var Tipo: SmallInt): Integer;
begin
  { Valores de Retorno:
    0 - se o código estiver correto;
    1 a 4 - Indicando qual o bloco que está com erro
    5 - Um ou mais blocos com erro

    Tipo: tipo de documento sendo coletado:
          -1: Ainda não identificado
          0: Arrecadação
          1: Titulo
  }
  LoadDLLFunctions;
  if Assigned(xValidaCampoCodigoEmBarras) then
    Result := xValidaCampoCodigoEmBarras(Dados,Tipo)
  else
    Result := -1;
end;

function TACBrTEFCliSiTefAPI.VerificaPresencaPinPad: Boolean;
begin
  {
   Retornos:
      1: Existe um PinPad operacional conectado ao micro;
      0: Nao Existe um PinPad conectado ao micro;
     -1: Biblioteca de acesso ao PinPad não encontrada }

  LoadDLLFunctions;
  if Assigned(xVerificaPresencaPinPad) then
    Result := ( xVerificaPresencaPinPad() = 1 )
  else
    Result := False;
end;

function TACBrTEFCliSiTefAPI.ObtemDadoPinPadDiretoEx(TipoDocumento: Integer; ChaveAcesso,
  Identificador: AnsiString): AnsiString;
var
  Saida: array [0..22] of AnsiChar;
  Retorno: Integer;
  DocLen: Integer;
const
  EntradaCelular = '011111NUMERO CELULAR                  CONFIRME NUMERO |(xx) xxxxx-xxxx  ';
  EntradaCPF     = '011111DIGITE O CPF                    CONFIRME O CPF  |xxx.xxx.xxx-xx  ';
  EntradaCNPJ    = '020808CNPJ Entre os 8 digitos iniciais Confirma os 8 ?|xx.xxx.xxx/     ' +
                     '0606CNPJ Entre os 6 digitos finais  Confirma os 6 ? |xxxx-xx         ';
begin
  Result := '';

  if Assigned(xObtemDadoPinPadDiretoEx) then
  begin
    case TipoDocumento of
      1: Retorno := xObtemDadoPinPadDiretoEx(PAnsiChar(ChaveAcesso), PAnsiChar(Identificador), PAnsiChar(EntradaCPF), Saida);
      2: Retorno := xObtemDadoPinPadDiretoEx(PAnsiChar(ChaveAcesso), PAnsiChar(Identificador), PAnsiChar(EntradaCNPJ), Saida);
      3: Retorno := xObtemDadoPinPadDiretoEx(PAnsiChar(ChaveAcesso), PAnsiChar(Identificador), PAnsiChar(EntradaCelular), Saida);
    else
      Retorno := -1;
    end;

    if Retorno = 0 then
    begin
      if (TipoDocumento = 2) then
        DocLen := 19
      else
        DocLen := 14;

      Result := copy(TrimRight(Saida), 5, DocLen);
      if (TipoDocumento = 2) then
        Delete(Result, 9, 2);
    end;
  end;
end;

function TACBrTEFCliSiTefAPI.LeDigitoPinPad(MensagemDisplay: AnsiString): AnsiString;
var
  Saida: array [0..5] of AnsiChar;
  Retorno: Integer;
begin
  Result := '';

  if Assigned(xLeDigitoPinPad) then
  begin
    Retorno := xLeDigitoPinPad(PAnsiChar(MensagemDisplay),
                    Saida);

    if Retorno = 0 then
    begin
      Result := TrimRight(Saida);
    end;
  end;
end;

end.



