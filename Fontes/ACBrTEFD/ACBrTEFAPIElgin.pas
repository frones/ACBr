{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Desenvolvido por:  MuriloS.A e Fernando Pasqueto                             }
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

unit ACBrTEFAPIElgin;

interface

uses
  Classes, SysUtils,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFAPIElginComum,
  ACBrJSON;

type

  { TACBrTEFAPIClassElgin }

  TACBrTEFAPIClassElgin = class(TACBrTEFAPIClass)
  private
    fTEFElginAPI: TACBrTEFElginAPI;
    fInFluxoAPI: Boolean;
    fConfirmaColeta: Boolean;
    fSequencial: String;
    fCancelarColeta: String;
    fModPagamento: TACBrTEFModalidadePagamento;

    function Vender(cartao: integer; const sequencial, pValorTotal: String;
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: byte = 0;
      DataPreDatado: TDateTime = 0): String;
    function Administrativo(opcao: Integer; const sequencial: String): String;

    function ConfirmarOperacao: String;
    function CancelarOperacao: String;
    function Coletar(root: TACBrJSONObject): String;
    function TratarRetornoOperacao(var Resposta: String): boolean;
  protected
    procedure InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo); override;
    procedure FinalizarChamadaAPI; override;
    procedure InterpretarRespostaAPI; override;

    function Iniciar(): String;
    function Finalizar(): String;
  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function EfetuarPagamento(ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0; DataPreDatado: TDateTime = 0): Boolean; override;
    function EfetuarAdministrativa(OperacaoAdm: TACBrTEFOperacao =
      tefopAdministrativo): Boolean; overload; override;
    function EfetuarAdministrativa(const CodOperacaoAdm: String = ''): boolean;
      overload; override;

    function CancelarTransacao(const NSU, CodigoAutorizacaoTransacao: String;
      DataHoraTransacao: TDateTime; Valor: Double;
      const CodigoFinalizacao: String = ''; const Rede: String = ''): Boolean;
      override;

    procedure FinalizarTransacao(const Rede, NSU, CodigoFinalizacao: String;
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); override;
    procedure AbortarTransacaoEmAndamento; override;
    function ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: SmallInt = 30000;
      MinLen: SmallInt = 0; MaxLen: SmallInt = 0): String; override;

//  procedure ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao = tefstsSucessoManual); override;
//  procedure ExibirMensagemPinPad(const MsgPinPad: String); override;
    property ConfirmaColeta: boolean read fConfirmaColeta;
  end;

implementation

uses
  TypInfo, StrUtils, DateUtils,
  ACBrUtil.Strings;

  { TACBrTEFAPIClassElgin }

constructor TACBrTEFAPIClassElgin.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;
  fpTEFRespClass := TACBrTEFRespElgin;
  fTEFElginAPI := TACBrTEFElginAPI.Create(fpACBrTEFAPI);
end;

destructor TACBrTEFAPIClassElgin.Destroy;
begin
  if Assigned(fTEFElginAPI) then
    FreeAndNil(fTEFElginAPI);

  inherited;
end;


procedure TACBrTEFAPIClassElgin.Inicializar;
var
  EnderecoIP, PortaPinPad, textoPinpad, versaoAC, nomeEstabelecimento,
    CodLoja, codTerminal: AnsiString;

 { procedure ApagarChaveSeExistir(Chave: String);
  var
    p: Integer;
  begin
    p := ParamAdicConfig.IndexOf(Chave);
    if (p >= 0) then
      ParamAdicConfig.Delete(p);
  end;}
begin
  if Inicializado then
    Exit;

  fTEFElginAPI.PathDLL := PathDLL;
  PortaPinPad := fpACBrTEFAPI.DadosTerminal.PortaPinPad;

  EnderecoIP := fpACBrTEFAPI.DadosTerminal.EnderecoServidor;
  textoPinpad := fpACBrTEFAPI.DadosAutomacao.NomeAplicacao + ' ' + fpACBrTEFapi.DadosAutomacao.VersaoAplicacao;
  nomeEstabelecimento := fpACBrTEFAPI.DadosEstabelecimento.RazaoSocial;
  versaoAC := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
  CodLoja := fpACBrTEFAPI.DadosTerminal.CodFilial;
  codTerminal := fpACBrTEFAPI.DadosTerminal.CodTerminal;

  fpACBrTEFAPI.GravarLog('*** ConfigurarDadosPDV ' +
    ' - EnderecoIP : ' + String(EnderecoIP) +
    ' - textoPinpad : ' + String(textoPinpad) +
    ' - versaoAC : ' + String(versaoAC) +
    ' - nomeEstabelecimento : ' + String(nomeEstabelecimento) +
    ' - CodLoja: ' + String(CodLoja) +
    ' - codTerminal: ' + String(codTerminal) );

  fTEFElginAPI.SetClientTCP(PAnsiChar(EnderecoIP), StrToIntDef(PortaPinPad, 60906));
  fTEFElginAPI.ConfigurarDadosPDV(PAnsiChar(textoPinpad),
    PAnsiChar(versaoAC), PAnsiChar(nomeEstabelecimento), PAnsiChar(CodLoja),
    PAnsiChar(codTerminal));
  Iniciar();

  fTEFElginAPI.Inicializada := True;
  fpACBrTEFAPI.GravarLog('   Inicializado Elgin');
  inherited;
end;

procedure TACBrTEFAPIClassElgin.InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo);
begin
  inherited;
  Iniciar();
end;

function TACBrTEFAPIClassElgin.Iniciar: String;
var
  payload: TACBRJsonObject;
  sJson: AnsiString;
  ddTerm: TACBrTEFAPIDadosTerminal;
  ddAuto: TACBrTEFAPIDadosAutomacao;
  ddEstab: TACBrTEFAPIDadosEstabelecimento;
begin
  Result := '';
  if fInFluxoAPI then
    exit;

  payload := TACBRJsonObject.Create;
  try
    ddTerm := fpACBrTEFAPI.DadosTerminal;
    ddAuto := fpACBrTEFAPI.DadosAutomacao;
    ddEstab := fpACBrTEFAPI.DadosEstabelecimento;

    payload.AddPair('aplicacao', ddAuto.NomeAplicacao);
    payload.AddPair('aplicacao_tela', ddAuto.NomeAplicacao);
    payload.AddPair('versao', ddAuto.VersaoAplicacao);
    payload.AddPair('estabelecimento', ddEstab.RazaoSocial);
    payload.AddPair('loja', ddTerm.codFilial);
    payload.AddPair('terminal', ddTerm.CodTerminal);
    payload.AddPair('nomeAC', ddAuto.NomeSoftwareHouse);
    payload.AddPair('textoPinpad', ddAuto.NomeAplicacao);
    payload.AddPair('versaoAC', ddAuto.VersaoAplicacao);
    payload.AddPair('nomeEstabelecimento', ddEstab.RazaoSocial);
    payload.AddPair('loja', ddTerm.codFilial);
    payload.AddPair('identificadorPontoCaptura', ddTerm.CodTerminal);

    sJson := payload.ToJSON;
    Result := fTEFElginAPI.IniciarOperacaoTEF(PAnsiChar(sJson));
    fSequencial := TACBrTEFElginUtils.getSequencial(Result);
  finally
    FreeAndNil(payload);
  end;

  fInFluxoAPI := (StrToIntDef(TACBrTEFElginUtils.getRetorno(Result), -1) in [0, 1]);
end;

procedure TACBrTEFAPIClassElgin.DesInicializar;
begin
  if not Inicializado then
    Exit;

  Finalizar;
  fpACBrTEFAPI.GravarLog('TACBrTEFAPIClassElgin.DesInicializar');
  fTEFElginAPI.Inicializada := False;
  inherited;
end;

function TACBrTEFAPIClassElgin.Finalizar: String;
begin
  Result := fTEFElginAPI.FinalizarOperacaoTEF(1);
  fInFluxoAPI := False;
end;

procedure TACBrTEFAPIClassElgin.FinalizarChamadaAPI;
begin
  inherited;
  Finalizar;
end;

procedure TACBrTEFAPIClassElgin.AbortarTransacaoEmAndamento;
begin
  fCancelarColeta := '9';
  CancelarOperacao;
end;

function TACBrTEFAPIClassElgin.CancelarOperacao: String;
begin
  fpACBrTEFAPI.GravarLog('CancelarOperacao: ' + fSequencial);
  Result := fTEFElginAPI.ConfirmarOperacaoTEF(StrToInt(fSequencial), 0);
end;

function TACBrTEFAPIClassElgin.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: String; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: String; const Rede: String): Boolean;
var
  payload: TACBrJSONObject;
  Resultado, sValor: String;
  iOp: integer;
begin
  fpACBrTEFAPI.GravarLog('CancelarTransacao( '+NSU+', '+CodigoAutorizacaoTransacao+', '+
                         DateTimeToStr(DataHoraTransacao)+', '+FloatToStr(Valor)+', '+
                         CodigoFinalizacao+', '+Rede+ ' )');
  payload := TACBrJSONObject.Create();
  try
    fSequencial := TACBrTEFElginUtils.incrementarSequencial(fSequencial);
    payload.AddPair('sequencial', fSequencial);
    payload.Addpair('transacao_data', FormatDateTime('dd/MM/yy', DataHoraTransacao));
    payload.Addpair('transacao_nsu', NSU);

    sVAlor := TACBrTEFElginUtils.FloatToJsonString(Valor);
    payload.Addpair('transacao_valor', SValor);
    fpACBrTEFAPI.GravarLog('CancelarTransacao Payload:' + payload.ToJson);

    iOP := TACBrTEFElginUtils.OperacaoAdminToElginint(tefopCancelamento);
    Resultado := String(fTEFElginAPI.RealizarAdmTEF( iOP, TACBrTEFElginUtils.stringify(payload), True));
    Result := TratarRetornoOperacao(Resultado);
    fpACBrTEFAPI.GravarLog('  Resultado: ' + TACBrTEFElginUtils.jsonify(Resultado).ToJson);
  finally
    FreeAndNil(payload);
  end;
end;

function TACBrTEFAPIClassElgin.Coletar(root: TACBrJSONObject): String;
var
  // chaves utilizadas na coleta
  codigo: integer;
  mensagem, coletaRetorno,      // In/Out; out: 0 = continuar coleta, 9 = cancelar coleta
  coletaSequencial,   // In/Out
  coletaMensagem,     // In/[Out]
  coletaTipo,         // In
  coletaOpcao,        // In
  coletaMascara, coletaInformacao: String;   // Out
  payload: TACBrJSONObject;
  resp, retorno: String;
  pPayload: PAnsiChar;
  opcoes, elements: TStringList;
  i, iSelecionado: integer;
  TEFAPI: TACBrTEFAPI;
  Validado, Cancelado: boolean;
  DefinicaoCampo: TACBrTEFAPIDefinicaoCampo;
  sRoot: String;

  procedure _DefinirCampos;
  begin
    FillChar(DefinicaoCampo, SizeOf(DefinicaoCampo), 0);
    with DefinicaoCampo do
    begin
      TituloPergunta := coletaMensagem;
      MascaraDeCaptura := coletaMascara;
      OcultarDadosDigitados := MatchText(UpperCase(TituloPergunta), ['SENHA']);

      case AnsiindexStr(coletaTipo, ELGIN_COLETA_TIPO) of
        0:
        begin
          TipoDeEntrada := tedTodos;
          OcultarDadosDigitados := True;
          //ValidacaoDado :=     TACBrTEFAPIValidacaoDado.valdSenhaLojista;   {Não exibivel podendo ser alfanumérico ou caracter especial}
        end;
        1: TipoDeEntrada := tedAlfabetico;   {Alfabético}
        2: TipoDeEntrada := tedTodos;        {Data/hora}
        3: TipoDeEntrada := tedNumerico;     {Numérico/Decimais}
        4: TipoDeEntrada := tedAlfaNum;      {Alfanumérico}
        else
          TipoDeEntrada := tedApenasLeitura;
      end;
    end;
  end;

begin
  TefAPI := TACBrTEFAPI(fpACBrTEFAPI);
  Validado := False;
  Cancelado := False;

  // extrai os dados da resposta / coleta
  sRoot := root.ToJson;
  codigo := TACBrTEFElginUtils.getIntegerValue(root, 'codigo');
  mensagem := TACBrTEFElginUtils.getStringValue(root, 'mensagem');
  coletaRetorno := TACBrTEFElginUtils.getStringValue(root, 'tef.automacao_coleta_retorno');
  coletaSequencial := TACBrTEFElginUtils.getStringValue(root, 'tef.automacao_coleta_sequencial');
  coletaMensagem := TACBrTEFElginUtils.getStringValue(root, 'tef.mensagemResultado');
  coletaTipo := TACBrTEFElginUtils.getStringValue(root, 'tef.automacao_coleta_tipo');
  coletaOpcao := TACBrTEFElginUtils.getStringValue(root, 'tef.automacao_coleta_opcao');
  coletaMascara := TACBrTEFElginUtils.getStringValue(root, 'tef.automacao_coleta_mascara');
  //coletaPalavraChave := TACBrTEFElginUtils.getStringValue(root, 'tef.automacao_coleta_palavra_chave');

  TefAPI.Gravarlog(UpperCase(coletaMensagem));
  // em caso de erro, encerra coleta
  if (coletaRetorno <> '0') and (UpperCase(copy(coletaMensagem, 1, 6)) <> 'QRCODE') then
  begin
    if (not (codigo in [0, 1])) and (Mensagem <> '') then
      TefAPI.QuandoExibirMensagem(Mensagem, telaoperador, 30000);

    Result := TACBrTEFElginUtils.stringify(root);
    Exit;
  end;

  // em caso de sucesso, monta o (novo) payload e continua a coleta
  payload := TACBRJsonObject.Create;
  try
    payload.AddPair('automacao_coleta_retorno', coletaRetorno);
    payload.AddPair('automacao_coleta_sequencial', coletaSequencial);

    if (coletaTipo <> '') and (coletaOpcao = '') then  // coleta dados do usuário
    begin // valor inserido (texto)
      _DefinirCampos;
      TefAPI.Gravarlog('  tef.mensagemResultado: ' + DefinicaoCampo.TituloPergunta);
      coletaInformacao := '';
      TefAPI.QuandoPerguntarCampo(DefinicaoCampo, coletaInformacao, Validado, Cancelado);

      if Cancelado then
        fCancelarColeta := '9';

      // se houve cancelamento, adiciona a chave com cancelamento para avisar a dll
      if (fCancelarColeta <> '') then
      begin
        //payload.RemovePair('automacao_coleta_retorno');
        payload.AddPair('automacao_coleta_retorno', fCancelarColeta);
        fCancelarColeta := '';
      end;

      payload.AddPair('automacao_coleta_informacao', coletaInformacao);
    end

    else if (coletaTipo <> '') and (coletaOpcao <> '') then
    begin // valor selecionado (lista)
      opcoes := TStringList.Create;
      elements := TStringList.Create;
      try
        TACBrTEFElginUtils.Split(';', coletaOpcao, opcoes);
        for i := 0 to opcoes.Count - 1 do
        begin
          elements.Add('[' + IntToStr(i) + ']' + UpperCase(opcoes[i]) + #13#10);
          fpACBrTEFAPI.GravarLog('[' + IntToStr(i) + '] ' + UpperCase(opcoes[i]) + #13#10);
        end;

        iSelecionado := -1;
        TefAPI.QuandoPerguntarMenu(coletaMensagem, elements, iSelecionado);
        TefAPI.GravarLog('  '+coletaMensagem +sLineBreak+ coletaOpcao);
        if iSelecionado >= 0 then
          coletaInformacao := opcoes[iSelecionado]
        else
          fCancelarColeta := '9';

        // se houve cancelamento, adiciona a chave com cancelamento para avisar a dll
        if (fCancelarColeta <> '') then
        begin
          //payload.RemovePair('automacao_coleta_retorno');
          payload.AddPair('automacao_coleta_retorno', fCancelarColeta);
          fCancelarColeta := '';
        end;
        payload.AddPair('automacao_coleta_informacao', coletaInformacao);
      finally
        FreeAndNil(elements);
        FreeAndNil(opcoes);
      end;
    end
    else if Trim(coletaMensagem) <> '' then
    begin
      if (UpperCase(copy(coletaMensagem, 1, 6)) = 'QRCODE') then
      begin
        TefAPI.GravarLog('Mensagem Operador: QRcode');
        TefAPI.QuandoExibirQRCode(coletaMensagem);
      end
      else
      begin
        TefAPI.GravarLog('Mensagem Operador:' + coletaMensagem);
        TefAPI.QuandoExibirMensagem(NativeStringToUTF8(coletaMensagem), telaoperador, -30000);
      end;
    end;

    // informa os dados coletados
    pPayload := TACBrTEFElginUtils.stringify(payload);
    case fpMetodoOperacao of
      tefmtdAdministrativa, tefmtdCancelamento:
        resp := fTEFElginAPI.RealizarAdmTEF(0, pPayload, False);

      tefmtdPagamento:
      begin
        if fModPagamento = tefmpCarteiraVirtual then
          resp := fTEFElginAPI.RealizarPixTEF(pPayload, False)
        else
          resp := fTEFElginAPI.RealizarPagamentoTEF(0, pPayload, False);
      end;
    end;
  finally
    fpACBrTEFAPI.GravarLog('verifica fim da coleta');
    fpACBrTEFAPI.GravarLog('sroot:' + sroot);
    fpACBrTEFAPI.GravarLog('Payload:' + String(pPayload));

    codigo := TACBrTEFElginUtils.getIntegerValue(root, 'codigo');
    mensagem := TACBrTEFElginUtils.getStringValue(root, 'mensagem');
    coletaRetorno := TACBrTEFElginUtils.getStringValue(root, 'tef.automacao_coleta_retorno');
    coletaMensagem := TACBrTEFElginUtils.getStringValue(root, 'tef.mensagemResultado');

    if not (codigo in [0, 1])then
      TefAPI.QuandoExibirMensagem(Mensagem, telaoperador, 5000);

    if not (StrToIntDef(coletaRetorno, 0) in [0, 1]) then
      TefAPI.QuandoExibirMensagem(coletaMensagem, telaoperador, 5000);

    FreeAndNil(payload);
  end;

  // verifica fim da coleta
  retorno := TACBrTEFElginUtils.getRetorno(resp);
  if (retorno <> '') then
    Result := TACBrTEFElginUtils.jsonify(resp).ToJSON
  else
    Result := Coletar(TACBrTEFElginUtils.jsonify(resp));
end;

function TACBrTEFAPIClassElgin.ConfirmarOperacao: String;
begin
  fpACBrTEFAPI.GravarLog('ConfirmarOperacao: ' + fSequencial);
  Result := FTEFElginAPI.ConfirmarOperacaoTEF(StrToInt(fSequencial), 1);
end;

function TACBrTEFAPIClassElgin.EfetuarAdministrativa(OperacaoAdm: TACBrTEFOperacao): Boolean;
var
  sOperacaoAdm: String;
begin
  sOperacaoAdm := TACBrTEFElginUtils.OperacaoAdminToElgin(OperacaoAdm);
  Result := EfetuarAdministrativa(sOperacaoAdm);
end;

function TACBrTEFAPIClassElgin.EfetuarAdministrativa(const CodOperacaoAdm: String): boolean;
var
  resp: String;
  CodOperacaoAdmInt: integer;
begin
  CodOperacaoAdmInt := StrToIntDef(CodOperacaoAdm, 0);
  //Inicializar;
  try
    // 2) REALIZAR OPERACAO
    fSequencial := TACBrTEFElginUtils.incrementarSequencial(fSequencial);
    // POR SER A JANELA DE Administrativo, USAR A FUNÇÃO Administrativo
    resp := Administrativo(CodOperacaoAdmInt, fSequencial);
    Result := TratarRetornoOperacao(resp);
  finally
    // DesInicializar;
  end;
end;

function TACBrTEFAPIClassElgin.Administrativo(opcao: Integer; const sequencial: String): String;
var
  payload: TACBrJSONObject;
begin
  Result := '';
  payload := TACBrJSONObject.Create;
  try
    fpACBrTEFAPI.GravarLog('Administrativo: ' +IntToStr(opcao)+', seq:'+ fSequencial);
    payload.AddPair('sequencial', fSequencial);
    Result := String(fTEFElginAPI.RealizarAdmTEF(opcao, TACBrTEFElginUtils.stringify(payload), True));
    fpACBrTEFAPI.GravarLog('  Resultado: ' + TACBrTEFElginUtils.jsonify(Result).ToJSON);
  finally
    FreeAndNil(payload);
  end;
end;

function TACBrTEFAPIClassElgin.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime): Boolean;
var
  CartaoInt, vCount, i: integer;
  CACount, CartaoAceito: TACBrTEFTipoCartao;
  resp, sValor: String;
begin
  CartaoAceito := teftcNAoDefinido;
    if not Inicializado then
    Self.Inicializar;

  fSequencial := TACBrTEFElginUtils.incrementarSequencial(fSequencial);
  vCount := 0;
  for CACount := Low(TACBrTEFTipoCartao) to High(TACBrTEFTipoCartao) do
  begin
    if (CACount in CartoesAceitos) then
    begin
      Inc(vCount);
      if (1 = vCount) then
        CartaoAceito := CACount;
    end;
  end;

  if (vCount <> 1) then
    CartaoAceito := teftcNAoDefinido;

  case CartaoAceito of
    teftcNaoDefinido, teftcOutros:
      CartaoInt := 0; // Pgto --> Perguntar tipo do cartao
    teftcCredito:
      CartaoInt := 1; // Pgto --> Cartao de credito
    teftcDebito:
      CartaoInt := 2; // Pgto --> Cartao de debito
    teftcVoucher:
      CartaoInt := 3; // Pgto --> Voucher (debito)
    teftcFrota:
      CartaoInt := 4; // Pgto --> Frota (debito)
    teftcPrivateLabel:
      CartaoInt := 5; // Pgto --> Private label (credito)
  end;

  try
    fModPagamento := Modalidade;
    sValor := CurrToStrF(ValorPagto, ffCurrency, 2);

    resp := Vender(CartaoInt, fSequencial, sValor, Financiamento, Parcelas, DataPreDatado);
    fpACBrTEFAPI.GravarLog('vender:' + resp);
    Result := TratarRetornoOperacao(resp);
  finally
    fModPagamento := tefmpNaoDefinido;
    Finalizar;
  end;
end;

procedure TACBrTEFAPIClassElgin.FinalizarTransacao(
  const Rede, NSU, CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  lId: longint;
begin
  lId := StrToIntDef(CodigoFinalizacao, -1);
  if (lId < 0) then
    Exit;

  case AStatus of
    tefstsSucessoAutomatico, tefstsSucessoManual:
      fTEFElginAPI.ConfirmarOperacaoTEF(lId, 1)
    else
      fTEFElginAPI.ConfirmarOperacaoTEF(lId, 0);
  end;
end;

procedure TACBrTEFAPIClassElgin.InterpretarRespostaAPI;
begin
  //inherited;
  fpACBrTEFAPI.GravarLog(fpACBrTEFAPI.UltimaRespostaTEF.Conteudo.Conteudo.Text);
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  fpACBrTEFAPI.UltimaRespostaTEF.ConteudoToProperty;
end;

function TACBrTEFAPIClassElgin.ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad;
  TimeOut: SmallInt; MinLen: SmallInt; MaxLen: SmallInt): String;
var
  TipoDocumento, RetornoInt: integer;
  RetornoDLL, resultadoCapturaPinPad: String;
begin
  case TipoDado of
    dpRG, dpRedRG:
      TipoDocumento := 1;
    dpCPF, dpRedCPF:
      TipoDocumento := 2;
    dpCNPJ, dpRedCNPJ:
      TipoDocumento := 3;
    dpFone, dpRedFone, dpDDDeFone, dpRedDDDeFone:
      TipoDocumento := 4;
    else
      fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada),
        [GetEnumName(TypeInfo(TACBrTEFAPIDadoPinPad), integer(TipoDado)), ClassName]));
  end;

  //sRet := fTEFElginAPI.IniciarOperacaoTEF(PAnsiChar('{}'));
  Inicializar;
  try
    RetornoDLL := String(fTEFElginAPI.RealizarColetaPinPad(TipoDocumento, fConfirmaColeta));
    // checkar se a operação foi bem sucedida ou não
    with TACBrTEFELginUtils do
      RetornoInt := StrToIntDef(getRetorno(RetornoDLL), -1);

    if (RetornoInt = 1) then
    begin
      // pega o valor digitado pelo usuário no pinpad
      with TACBrTEFELginUtils do
        resultadoCapturaPinPad := getStringValue(jsonify(RetornoDLL), 'tef.resultadoCapturaPinPad');

      fpACBrTEFAPI.GravarLog('  tef.resultadoCapturaPinPad: ' + resultadoCapturaPinPad);
    end
    else if (RetornoInt > 0) then
    begin
      with TACBrTEFELginUtils do
        fpACBrTEFAPI.GravarLog(getStringValue(jsonify(RetornoDLL), 'mensagem'));
      Exit;
    end
    else
    begin
      with TACBrTEFELginUtils do
        fpACBrTEFAPI.GravarLog(getStringValue(jsonify(RetornoDLL), 'tef.mensagemResultado'));
      Exit;
    end;

    if fConfirmaColeta then
    begin
      if (RetornoInt = 1) then
        Result := resultadoCapturaPinPad;
      Exit;
    end;

    // faz algo com o valor coletado
    // nesse exemplo são adicionadas as máscaras dos valores
    case TipoDocumento of
      1: resultadoCapturaPinPad := TACBrTEFELginUtils.FormatRG(resultadoCapturaPinPad);
      2: resultadoCapturaPinPad := TACBrTEFELginUtils.FormatCPF(resultadoCapturaPinPad);
      3: resultadoCapturaPinPad := TACBrTEFELginUtils.FormatCNPJ(resultadoCapturaPinPad);
      4: resultadoCapturaPinPad := TACBrTEFELginUtils.FormatPhone(resultadoCapturaPinPad);
    end;

    RetornoDLL := String(fTEFElginAPI.ConfirmarCapturaPinPad(TipoDocumento,
      PAnsiChar(AnsiString(resultadoCapturaPinPad))));
    with TACBrTEFELginUtils do
      RetornoInt := StrToIntDef(getRetorno(RetornoDLL), -1);

    if (RetornoInt = 1) then
    begin
      with TACBrTEFELginUtils do
        resultadoCapturaPinPad := getStringValue(jsonify(retornoDll), 'tef.resultadoCapturaPinPad');

      Result := resultadoCapturaPinPad;
      fpACBrTEFAPI.GravarLog('  tef.resultadoCapturaPinPad: ' + resultadoCapturaPinPad);
      fpACBrTEFAPI.GravarLog('  ConfirmarCapturaPinPad: '+RetornoDLL);
    end
    else if (RetornoInt > 0) then
    begin
      with TACBrTEFELginUtils do
        fpACBrTEFAPI.GravarLog(getStringValue(jsonify(RetornoDLL), 'mensagem'));
      exit;
    end
    else
    begin
      with TACBrTEFELginUtils do
        fpACBrTEFAPI.GravarLog(getStringValue(jsonify(RetornoDLL), 'tef.mensagemResultado'));
    end;
  finally
    DesInicializar;
  end;
end;

function TACBrTEFAPIClassElgin.TratarRetornoOperacao(var Resposta: String): boolean;
var
  retorno: String;
  RespElginTef: TACBrTEFRespElgin;
begin
  Result := False;
  RespElginTef := TACBrTEFRespElgin(fpACBrTEFAPI.UltimaRespostaTEF);
  RespElginTef.Clear;

  retorno := TACBrTEFElginUtils.getRetorno(Resposta);
  if (retorno = '') then // Continuar operacao/Iniciar o processo de coleta
  begin                  // 0 para Coletar vendas, 1 para Coletar Administrativo
    Resposta := Coletar(TACBrTEFElginUtils.jsonify(Resposta));
    fpACBrTEFAPI.GravarLog('tratarRetornoOperacao:' + Resposta);
    retorno := TACBrTEFElginUtils.getRetorno(Resposta);
    fpACBrTEFAPI.GravarLog('Conclusão da coleta...');
  end;

  RespElginTef.Conteudo.Conteudo.Clear;
  RespElginTef.Conteudo.Conteudo.Append(Resposta);
  fpACBrTEFAPI.GravarLog('Resposta Coleta:' + char(13) + Resposta);
  //VERIFICAR RESULTADO / CONFIRMAR
  case AnsiIndexStr(retorno, ['0', '1', '']) of
    0:
    begin
      fpACBrTEFAPI.GravarLog('TRANSAÇÃO OK, INICIANDO CONFIRMAÇÃO...');
      fSequencial := TACBrTEFElginUtils.getSequencial(Resposta);
      Resposta := ConfirmarOperacao;
      //retorno     := TACBrTEFElginUtils.getRetorno(Resposta);
      Result := True;
    end;

    1:
    begin
      fpACBrTEFAPI.GravarLog('TRANSAÇÃO OK');
      Result := True;
    end;

    2:
      fpACBrTEFAPI.GravarLog('ERRO AO COLETAR DADOS');
    else
      fpACBrTEFAPI.GravarLog('ERRO NA TRANSAÇÃO');
  end;
end;

function TACBrTEFAPIClassElgin.Vender(cartao: integer; const sequencial,
  pValorTotal: String; Financiamento: TACBrTEFModalidadeFinanciamento;
  Parcelas: byte; DataPreDatado: TDateTime): String;
var
  iPos, iQtdParc: integer;
  sQtdParc, sValorTotal: String;
  payload: TACBRJsonObject;
begin
  payload := TACbrJsonObject.Create;
  try
    iQtdParc := Parcelas;
    sQtdParc := IntToStr(iQtdParc);
    sValorTotal := pValorTotal;

    fpACBrTEFAPI.GravarLog('Vender: ' + fSequencial);
    payload.AddPair('sequencial', fSequencial);
    if (sValorTotal <> '') then
    begin
      iPos := pos(sValorTotal, ',');
      if (iPos > 0) then
        sValorTotal := ACBrUtil.Strings.PadRight(pValorTotal, (length(sValorTotal) - iPos), '0');

      sValorTotal := TACBrTEFElginUtils.RemoveNonNumericChars(sValorTotal);
      payload.AddPair('valorTotal', sValorTotal);
    end;

    case cartao of
      1, 5:
        payload.AddPair('tipoCartao', 'Credito');
      2, 3, 4:
        payload.AddPair('tipoCartao', 'Debito');
    end;

    case Financiamento of
      tefmfAVista:
        payload.AddPair('formaPagamento', 'A vista');

      tefmfParceladoEmissor:
      begin
        payload.AddPair('formaPagamento', 'Parcelado');
        payload.AddPair('tipoFinanciamento', 'Administradora');
        if iQtdParc > 1 then
          payload.AddPair('numeroParcelas', sQtdParc);
      end;

      tefmfParceladoEstabelecimento:
      begin
        payload.AddPair('formaPagamento', 'Parcelado');
        payload.AddPair('tipoFinanciamento', 'Estabelecimento');
        if iQtdParc > 1 then
          payload.AddPair('numeroParcelas', sQtdParc);
      end;

      tefmfPredatado:
        payload.AddPair('formaPagamento', 'Pre-datado');
    end;

    if (fModPagamento = tefmpCarteiraVirtual) then
      Result := String(fTEFElginAPI.RealizarPixTEF(TACBrTEFElginUtils.stringify(payload), True))
    else
      Result := String(fTEFElginAPI.RealizarPagamentoTEF(Cartao, TACBrTEFElginUtils.stringify(payload), True));

    fpACBrTEFAPI.GravarLog('  Vender: ' + TACBrTEFElginUtils.jsonify((Result)).ToJSON);
  finally
    FreeAndNil(payload);
  end;
end;

end.
