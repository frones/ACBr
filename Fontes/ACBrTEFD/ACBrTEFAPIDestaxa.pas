{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

unit ACBrTEFAPIDestaxa;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrTEFComum,
  ACBrTEFAPI,
  ACBrTEFAPIComum,
  ACBrUtil.Strings,
  ACBrTEFDestaxaComum;

type 

  { TACBrTEFRespDestaxa }

  TACBrTEFRespDestaxa = class(TACBrTEFResp)
  private
    fDestaxaResposta: TACBrTEFDestaxaTransacaoResposta;
  public
    destructor Destroy; override;

    procedure ConteudoToProperty; override;
    function DestaxaResposta: TACBrTEFDestaxaTransacaoResposta;
  end;

  { TACBrTEFAPIClassDestaxa }

  TACBrTEFAPIClassDestaxa = class(TACBrTEFAPIClass)
  private
    fDestaxaClient: TACBrTEFDestaxaClient;

    function DestaxaClient: TACBrTEFDestaxaClient;

    function AplicarMascaraData(const aMascara: String; var aResposta: String): String;

    procedure QuandoExibirQRCodeAPI(const aDados: String);
    procedure QuandoGravarLogAPI(const aLogLine: String; var Tratado: Boolean);
    procedure QuandoExibirMensagemAPI(aMensagem: String; MilissegundosExibicao: Integer; var Cancelar: Boolean);
    procedure QuandoPerguntarMenuAPI(aMensagem: String; aOpcoes: TSplitResult; var aOpcao: Integer; var Cancelado: Boolean);
    procedure QuandoPerguntarCampoAPI(aMensagem, aMascara: String; aTipo: TACBrTEFDestaxaColetaTipo; var Resposta: String; var Cancelar: Boolean);

    function ExibirMenuAdministrativo: Boolean;
  protected
    procedure InterpretarRespostaAPI; override;
    procedure InicializarChamadaAPI(aMetodoOperacao: TACBrTEFAPIMetodo); override;
    procedure FinalizarChamadaAPI; override;

    //procedure CarregarRespostasPendentes(const AListaRespostasTEF: TACBrTEFAPIRespostas); override;
  public
    constructor Create(aACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    procedure FinalizarTransacao(const Rede, NSU, CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); override;

    procedure ResolverTransacaoPendente(aStatus: TACBrTEFStatusTransacao = tefstsSucessoManual); override;
    procedure AbortarTransacaoEmAndamento; override;

    procedure ExibirMensagemPinPad(const MsgPinPad: String); override;
              
    function VerificarPresencaPinPad: Byte; override;
    function EfetuarPagamento(ValorPagto: Currency; Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = []; Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0; DataPreDatado: TDateTime = 0; DadosAdicionais: String = ''): Boolean; override;

    function TesteComunicacao: Boolean;
    function EfetuarAdministrativa(OperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; override;
    function EfetuarAdministrativa(const CodOperacaoAdm: String = ''): Boolean; overload; override;

    function CancelarTransacao(const NSU, CodigoAutorizacaoTransacao: String;DataHoraTransacao: TDateTime;Valor: Double;
      const CodigoFinalizacao: String = ''; const Rede: String = ''): Boolean; override;

    function ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: Integer = 30000; MinLen: SmallInt = 0; MaxLen: SmallInt = 0): String; override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Math, DateUtils, StrUtils;

{ TACBrTEFRespDestaxa }

destructor TACBrTEFRespDestaxa.Destroy;
begin
  if Assigned(fDestaxaResposta) then
    fDestaxaResposta.Free;
  inherited Destroy;
end;

procedure TACBrTEFRespDestaxa.ConteudoToProperty;

  procedure ConteudoToParcelas(resp: TACBrTEFDestaxaTransacaoResposta);
  var
    i: Integer;
    venc: TDateTime;
    parc_val, saldo: Double;
    parc: TACBrTEFRespParcela;
    vencs, valores: TSplitResult;
  begin
    Parcelas.Clear;
    if (resp.transacao_parcela > 0) then
      QtdParcelas := resp.transacao_parcela;

    if NaoEstaVazio(resp.transacao_administradora) then
      valores := Split(';', resp.transacao_parcela_valor);

    if NaoEstaVazio(resp.transacao_parcela_vencimento) then
      vencs := Split(';', resp.transacao_parcela_vencimento);
    
    for i := 0 to Length(valores) - 1 do
    begin
      parc := TACBrTEFRespParcela.Create;
      parc.Valor := StrToFloatDef(valores[i], 0);

      if (Length(vencs) >= (i+1)) then
        parc.Vencimento := StrToDateDef(vencs[i], 0);
      Parcelas.Add(parc);
    end;

    if EstaZerado(Parcelas.Count) and (QtdParcelas > 0) then
    begin
      saldo := resp.transacao_valor;
      parc_val := RoundABNT((saldo / QtdParcelas), -2);
      venc := IncDay(DateOf(resp.transacao_data), 30);
      for i := 1 to QtdParcelas do
      begin
        parc := TACBrTEFRespParcela.Create;
        parc.Vencimento := venc;

        if (i = QtdParcelas) then
          parc.Valor := saldo
        else
          parc.Valor := parc_val;

        Parcelas.Add(parc);
        venc := IncDay(venc, 30);
        saldo := saldo - parc_val;
      end;
    end;
  end;

var
  wResp: String;
  i: Integer;
begin
  DestaxaResposta.Clear;
  wResp := Conteudo.LeInformacao(899, 201).AsString;
  if EstaVazio(wResp) then
    Exit;

  DestaxaResposta.AsString := StringToBinaryString(wResp);
  if (DestaxaResposta.servico = dxsIniciar) then
  begin
    Confirmar := False;
    Sucesso := DestaxaResposta.estado.Conectado;
    Exit;
  end;

  for i := 0 to Conteudo.Count-1 do
    if (Conteudo.Linha[i].Identificacao <> 201) then
      ProcessarTipoInterno(Conteudo.Linha[i]);

  Sucesso := (DestaxaResposta.transacao_resposta = 0);
  Confirmar := (DestaxaResposta.retorno = drsSucessoComConfirmacao) or
               ((DestaxaResposta.transacao = CDESTAXA_ADM_PENDENTE) and NaoEstaVazio(DestaxaResposta.transacao_nsu));
  Rede := DestaxaResposta.transacao_rede;
  NSU := DestaxaResposta.transacao_nsu;
  TextoEspecialOperador := DestaxaResposta.mensagem;
  BIN := DestaxaResposta.transacao_cartao_numero;
  ValorTotal := DestaxaResposta.transacao_valor;
  NSU_TEF := DestaxaResposta.transacao_nsu_rede;
  DataHoraTransacaoLocal := DestaxaResposta.transacao_data;
  DataHoraTransacaoHost := DestaxaResposta.transacao_data;
  DataVencimento := DestaxaResposta.transacao_vencimento;
  TaxaServico := StrToFloatDef(DestaxaResposta.transacao_valor_taxa_servico, 0);

  NFCeSAT.Bandeira := DestaxaResposta.transacao_administradora;
  NFCeSAT.Autorizacao := DestaxaResposta.transacao_autorizacao;
  NFCeSAT.CNPJCredenciadora := DestaxaResposta.transacao_rede_cnpj;
  if NaoEstaVazio(DestaxaResposta.transacao_cartao_numero) and (Length(DestaxaResposta.transacao_cartao_numero) >= 4) then
    NFCeSAT.UltimosQuatroDigitos := RightStr(DestaxaResposta.transacao_cartao_numero, 4);

  CodigoBandeiraPadrao := DestaxaResposta.codigo_bandeira;
  NomeAdministradora := DestaxaResposta.transacao_administradora;
  CodigoAutorizacaoTransacao := DestaxaResposta.transacao_autorizacao;

  Credito := (DestaxaResposta.transacao_tipo_cartao = dtcCredito);
  Debito := DestaxaResposta.transacao_tipo_cartao = dtcDebito;

  ImagemComprovante1aVia.Text := DestaxaResposta.transacao_comprovante_1via.Text;
  ImagemComprovante2aVia.Text := DestaxaResposta.transacao_comprovante_2via.Text;

  ConteudoToParcelas(DestaxaResposta);
  case DestaxaResposta.transacao_financiado of
    dxfAdministradora: ParceladoPor := parcADM;
    dxfEstabelecimento: ParceladoPor := parcLoja;
  end;

  case DestaxaResposta.transacao_pagamento of
    dpgAVista: TipoOperacao := opAvista;
    dpgParcelado: TipoOperacao := opParcelado;
    dpgPreDatado: TipoOperacao := opPreDatado;
  end;
end;

function TACBrTEFRespDestaxa.DestaxaResposta: TACBrTEFDestaxaTransacaoResposta;
begin
  if (not Assigned(fDestaxaResposta)) then
    fDestaxaResposta := TACBrTEFDestaxaTransacaoResposta.Create;
  Result := fDestaxaResposta;
end;

{ TACBrTEFAPIClassDestaxa }

function TACBrTEFAPIClassDestaxa.DestaxaClient: TACBrTEFDestaxaClient;
var
  p: Integer;
begin
  if (not Assigned(fDestaxaClient)) then
  begin
    fDestaxaClient := TACBrTEFDestaxaClient.Create;
    fDestaxaClient.Loja := fpACBrTEFAPI.DadosTerminal.CodEmpresa;
    fDestaxaClient.Terminal := fpACBrTEFAPI.DadosTerminal.CodTerminal;
    fDestaxaClient.Aplicacao := fpACBrTEFAPI.DadosAutomacao.NomeAplicacao;
    //fDestaxaClient.AplicacaoTela := fpACBrTEFAPI.DadosTerminal.CodEmpresa;
    fDestaxaClient.AplicacaoVersao := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
    fDestaxaClient.Estabelecimento := fpACBrTEFAPI.DadosEstabelecimento.CNPJ;
    fDestaxaClient.OnGravarLog := QuandoGravarLogAPI;
    fDestaxaClient.OnColetarOpcao := QuandoPerguntarMenuAPI;
    fDestaxaClient.OnColetarInformacao := QuandoPerguntarCampoAPI;
    fDestaxaClient.OnExibirMensagem := QuandoExibirMensagemAPI;
    fDestaxaClient.OnExibirQRCode := QuandoExibirQRCodeAPI;
    fDestaxaClient.OnAguardarResposta := TACBrTEFAPI(fpACBrTEFAPI).QuandoEsperarOperacao;

    if NaoEstaVazio(fpACBrTEFAPI.DadosTerminal.EnderecoServidor) then
    begin
      p := Pos(':', fpACBrTEFAPI.DadosTerminal.EnderecoServidor);
      fDestaxaClient.EnderecoIP := Copy(fpACBrTEFAPI.DadosTerminal.EnderecoServidor, 1, p-1);
      fDestaxaClient.Porta := Copy(fpACBrTEFAPI.DadosTerminal.EnderecoServidor, p+1, Length(fpACBrTEFAPI.DadosTerminal.EnderecoServidor));
    end;
  end;
  Result := fDestaxaClient;
end;

function TACBrTEFAPIClassDestaxa.AplicarMascaraData(const aMascara: String; var aResposta: String): String;
var
  tam, tamM, i, j: Integer;
begin
  Result := aResposta;
  tam := Length(aResposta);
  tamM := Length(OnlyAlphaNum(aMascara));

  if (tam <> tamM) then
    Exit;

  j := 1;
  Result := EmptyStr;
  tamM := Length(aMascara);
  for i := 1 to tamM do
  begin
    if (aMascara[i] = '/') then
      Result := Result + '/'
    else if (aMascara[i] = 'd') or (aMascara[i] = 'M') or (aMascara[i] = 'y') then
    begin
      Result := Result + aResposta[j];
      Inc(j);
    end;
  end;
end;

procedure TACBrTEFAPIClassDestaxa.QuandoExibirQRCodeAPI(const aDados: String);
begin
  if (not Assigned(TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirQRCode)) then
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPIEventoInvalidoException), ['QuandoExibirQRCode']));
  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirQRCode(aDados);
end;

procedure TACBrTEFAPIClassDestaxa.QuandoGravarLogAPI(const aLogLine: String; var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAPIClassDestaxa.QuandoExibirMensagemAPI(aMensagem: String; MilissegundosExibicao: Integer; var Cancelar: Boolean);
begin
  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(aMensagem, telaTodas, MilissegundosExibicao);
end;

procedure TACBrTEFAPIClassDestaxa.QuandoPerguntarMenuAPI(aMensagem: String;aOpcoes: TSplitResult; var aOpcao: Integer; var Cancelado: Boolean);
var
  i: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    for i := 0 to Length(aOpcoes) - 1 do
      sl.Add(aOpcoes[i]);

    TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu(aMensagem, sl, aOpcao);
    Cancelado := (aOpcao < 0);
  finally
    sl.Free;
  end;
end;

procedure TACBrTEFAPIClassDestaxa.QuandoPerguntarCampoAPI(aMensagem,
  aMascara: String; aTipo: TACBrTEFDestaxaColetaTipo; var Resposta: String; var Cancelar: Boolean);
var
  Validado: Boolean;
  Def: TACBrTEFAPIDefinicaoCampo;
begin
  Validado := False;
  Def.TamanhoMinimo := 0;
  Def.TamanhoMaximo := 0;
  Def.TipoDeEntrada := tedTodos;
  Def.TituloPergunta := aMensagem;
  Def.ValidacaoDado := valdNenhuma;
  Def.OcultarDadosDigitados := False;

  if (aMascara = CDESTAXA_MASCARA_DATA1) then
    Def.MascaraDeCaptura := '**/**/**'
  else if (aMascara = CDESTAXA_MASCARA_DATA2) then
    Def.MascaraDeCaptura := '**/**/****'
  else if (aMascara = CDESTAXA_MASCARA_VALIDADE) then
    Def.MascaraDeCaptura := '**/**'
  else if (aMascara = CDESTAXA_MASCARA_DECIMAL) then
    Def.MascaraDeCaptura := '@,@@'
  else if (Pos('#', aMascara) > 0) then
  begin
    Def.TamanhoMaximo := Length(aMascara);
    Def.MascaraDeCaptura := StringReplace(aMascara, '#', '*', [rfReplaceAll]);
  end;

  case aTipo of
    dctNaoExibivel: Def.TipoDeEntrada := tedApenasLeitura;
    dctAlfabetico: Def.TipoDeEntrada := tedAlfabetico;
    dctDataHora: Def.TipoDeEntrada := tedAlfaNum;
    dctNumerico: Def.TipoDeEntrada := tedNumerico;
  end;

  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarCampo(Def, Resposta, Validado, Cancelar);

  if (Pos('/', aMascara) > 0) then
    Resposta := AplicarMascaraData(aMascara, Resposta);
end;

function TACBrTEFAPIClassDestaxa.ExibirMenuAdministrativo: Boolean;
var
  wOpcao: Integer;
  sr: TSplitResult;
  Cancelar: Boolean;
begin
  wOpcao := -1;
  Result := False;
  Cancelar := False;
  DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
  DestaxaClient.Socket.Consultar;

  if (DestaxaClient.Resposta.servico <> dxsConsultar) or
     (DestaxaClient.Resposta.retorno = drsErroDesconhecido) or
     EstaVazio(DestaxaClient.Resposta.transacao) then
    Exit;

  sr := Split(';', DestaxaClient.Resposta.transacao);

  QuandoPerguntarMenuAPI(CDESTAXA_MENU_ADMIN, sr, wOpcao, Cancelar);

  if (wOpcao >= 0) then
  begin
    DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
    Result := DestaxaClient.ExecutarTransacao(sr[wOpcao]);
  end;
end;

procedure TACBrTEFAPIClassDestaxa.InterpretarRespostaAPI;
var
  resp: String;
begin
  resp := BinaryStringToString(DestaxaClient.Resposta.AsString);

  AtualizarHeader;
  fpACBrTEFAPI.UltimaRespostaTEF.Clear;
  fpACBrTEFAPI.UltimaRespostaTEF.Conteudo.GravaInformacao(899, 201, resp);
  fpACBrTEFAPI.UltimaRespostaTEF.DocumentoVinculado := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  fpACBrTEFAPI.UltimaRespostaTEF.ConteudoToProperty;
end;

procedure TACBrTEFAPIClassDestaxa.InicializarChamadaAPI(aMetodoOperacao: TACBrTEFAPIMetodo);
begin
  inherited InicializarChamadaAPI(aMetodoOperacao);
  DestaxaClient.IniciarRequisicao;
end;

procedure TACBrTEFAPIClassDestaxa.FinalizarChamadaAPI;
begin
  inherited FinalizarChamadaAPI;
  if DestaxaClient.Socket.EmTransacao then
  begin
    DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
    DestaxaClient.FinalizarRequisicao;
  end;
end;

constructor TACBrTEFAPIClassDestaxa.Create(aACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited Create(aACBrTEFAPI);
  fpTEFRespClass := TACBrTEFRespDestaxa;
end;

destructor TACBrTEFAPIClassDestaxa.Destroy;
begin
  if Assigned(fDestaxaClient) then
    fDestaxaClient.Free;
  inherited Destroy;
end;

procedure TACBrTEFAPIClassDestaxa.Inicializar;
var
  wErro: Integer;
begin
  try
    wErro := DestaxaClient.Socket.Conectar;
    if NaoEstaZerado(wErro) then
      raise EACBrTEFDestaxaErro.Create(
        ACBrStr(
          'Erro ao conectar Destaxa Client' + sLineBreak +
          'Endereço: ' + DestaxaClient.EnderecoIP + sLineBreak +
          'Porta: ' + DestaxaClient.Porta + sLineBreak +
          'Erro: ' + IntToStr(wErro) + '-' + DestaxaClient.Socket.LastErrorDesc));
    fpInicializado := True;
  finally
    DestaxaClient.Socket.Desconectar;
  end;
end;

procedure TACBrTEFAPIClassDestaxa.DesInicializar;
begin
  DestaxaClient.Socket.Desconectar;
  fpInicializado := False;
end;

function TACBrTEFAPIClassDestaxa.EfetuarPagamento(ValorPagto: Currency; Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte; DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
begin
  if NaoEstaZerado(ValorPagto) then
    DestaxaClient.Requisicao.transacao_valor := ValorPagto;

  if (Modalidade = tefmpCarteiraVirtual) then
  begin
    Result := DestaxaClient.DigitalPagar;
    Exit;
  end;

  case Financiamento of
    tefmfAVista: DestaxaClient.Requisicao.transacao_pagamento := dpgAVista;
    tefmfPredatado: DestaxaClient.Requisicao.transacao_pagamento := dpgPreDatado;
    tefmfParceladoEmissor:
    begin
      DestaxaClient.Requisicao.transacao_pagamento := dpgParcelado;
      DestaxaClient.Requisicao.transacao_financiado := dxfAdministradora;
    end;
    tefmfParceladoEstabelecimento:
    begin
      DestaxaClient.Requisicao.transacao_pagamento := dpgParcelado;
      DestaxaClient.Requisicao.transacao_financiado := dxfEstabelecimento;
    end;
  end;

  if (teftcCredito in CartoesAceitos) and (not (teftcDebito in CartoesAceitos)) then
    DestaxaClient.Requisicao.transacao_tipo_cartao := dtcCredito;
  if (teftcDebito in CartoesAceitos) and (not (teftcCredito in CartoesAceitos)) then
    DestaxaClient.Requisicao.transacao_tipo_cartao := dtcDebito;

  if (DestaxaClient.Requisicao.transacao_pagamento <> dpgAVista) and NaoEstaZerado(Parcelas) then
    DestaxaClient.Requisicao.transacao_parcela := Parcelas;

  if NaoEstaZerado(DataPreDatado) then
    DestaxaClient.Requisicao.transacao_vencimento := DataPreDatado;

  Result := DestaxaClient.CartaoVender;
end;

function TACBrTEFAPIClassDestaxa.TesteComunicacao: Boolean;
var
  wIniciarTransacao: Boolean;
begin
  wIniciarTransacao := (not DestaxaClient.Socket.EmTransacao);
  if wIniciarTransacao then
    DestaxaClient.IniciarRequisicao;
  try
    Result := DestaxaClient.Resposta.estado.Conectado;
  finally
    if wIniciarTransacao then
      DestaxaClient.FinalizarRequisicao;
  end;
end;

function TACBrTEFAPIClassDestaxa.EfetuarAdministrativa(OperacaoAdm: TACBrTEFOperacao): Boolean;
var
  transacao: String;
begin
  transacao := EmptyStr;

  case OperacaoAdm of
    tefopCancelamento: transacao := CDESTAXA_ADM_CANCELAR;
    tefopReimpressao: transacao := CDESTAXA_ADM_REIMPRIMIR;
    tefopTesteComunicacao:
    begin
      Result := TesteComunicacao;
      Exit;
    end;
  end;

  Result := EfetuarAdministrativa(transacao);
end;

function TACBrTEFAPIClassDestaxa.EfetuarAdministrativa(const CodOperacaoAdm: String): Boolean;
begin
  if EstaVazio(CodOperacaoAdm) then
  begin
    Result := ExibirMenuAdministrativo;
    Exit;
  end;
      
  DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
  DestaxaClient.Requisicao.retorno := drqExecutarServico;
  DestaxaClient.Socket.Executar(CodOperacaoAdm);
end;

function TACBrTEFAPIClassDestaxa.CancelarTransacao(const NSU, CodigoAutorizacaoTransacao: String; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: String; const Rede: String): Boolean;
begin
  Result := False;
  if NaoEstaVazio(NSU) then
    DestaxaClient.Requisicao.transacao_nsu := NSU;
  if NaoEstaZerado(DataHoraTransacao) then
    DestaxaClient.Requisicao.transacao_data := DataHoraTransacao;
  if NaoEstaZerado(Valor) then
    DestaxaClient.Requisicao.transacao_valor := Valor;

  DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
  DestaxaClient.Requisicao.retorno := drqExecutarServico;
  Result := DestaxaClient.ExecutarTransacao(CDESTAXA_ADM_CANCELAR);
end;

procedure TACBrTEFAPIClassDestaxa.ExibirMensagemPinPad(const MsgPinPad: String);
var
  Msg: TACBrTEFDestaxaMensagem;
begin
  Msg := StringToDestaxaMensagem(MsgPinPad);
  DestaxaClient.IniciarRequisicao;
  try
    DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
    DestaxaClient.Requisicao.retorno := drqExecutarServico;
    DestaxaClient.Socket.Mostrar(Msg);
  finally
    DestaxaClient.FinalizarRequisicao;
  end;
end;

function TACBrTEFAPIClassDestaxa.ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: Integer; MinLen: SmallInt; MaxLen: SmallInt): String;
begin
  DestaxaClient.IniciarRequisicao;
  try
    DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
    DestaxaClient.Requisicao.retorno := drqExecutarServico;

    Result := DestaxaClient.Socket.Coletar(TipoDado, MinLen, MaxLen, TimeOut);
  finally
    DestaxaClient.FinalizarRequisicao;
  end;
end;

procedure TACBrTEFAPIClassDestaxa.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  retorno: TACBrTEFDestaxaRetornoRequisicao;
  i: Integer;
begin
  DestaxaClient.Requisicao.Clear;
  if aStatus in [tefstsSucessoManual, tefstsSucessoAutomatico] then
    retorno := drqConfirmarTransacao
  else
    retorno := drqCancelarTransacao;

  if (DestaxaClient.Resposta.retorno = drsSucessoComConfirmacao) and DestaxaClient.Socket.EmTransacao then
  begin
    DestaxaClient.Requisicao.retorno := retorno;
    DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial;
    DestaxaClient.ExecutarTransacao(DestaxaClient.Resposta.transacao);
  end
  else if (not fpACBrTEFAPI.ConfirmarTransacaoAutomaticamente) then
  begin
    if (not DestaxaClient.Socket.EmTransacao) then
      DestaxaClient.IniciarRequisicao;

    for i := 0 to Pred(fpACBrTEFAPI.RespostasTEF.Count) do
    if (fpACBrTEFAPI.RespostasTEF[i] is TACBrTEFRespDestaxa) and (fpACBrTEFAPI.RespostasTEF[i].NSU = NSU) then
    begin
      DestaxaClient.Requisicao.retorno := retorno;
      DestaxaClient.Requisicao.sequencial := TACBrTEFRespDestaxa(fpACBrTEFAPI.RespostasTEF[i]).DestaxaResposta.sequencial;
      if (TACBrTEFRespDestaxa(fpACBrTEFAPI.RespostasTEF[i]).DestaxaResposta.transacao = CDESTAXA_DIGITAL_PAGAR) then
        DestaxaClient.ExecutarTransacaoAnterior(CDESTAXA_DIGITAL_PAGAR)
      else
        DestaxaClient.ExecutarTransacaoAnterior(CDESTAXA_CARTAO_VENDER);
    end;

    if (fpACBrTEFAPI.UltimaRespostaTEF.NSU = NSU) then
      DestaxaClient.FinalizarRequisicao;
  end;
end;

{procedure TACBrTEFAPIClassDestaxa.CarregarRespostasPendentes(const AListaRespostasTEF: TACBrTEFAPIRespostas);
var
  ultNSU: String;
  wResp: TACBrTEFResp;
begin
  ultNSU := EmptyStr;
  DestaxaClient.Socket.ColetaAutomatica := False;
  DestaxaClient.IniciarRequisicao;
  try
    DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
    DestaxaClient.Requisicao.retorno := drqExecutarServico;
    DestaxaClient.ExecutarTransacaoSilenciosa(CDESTAXA_ADM_PENDENTE);
    if NaoEstaZerado(DestaxaClient.Resposta.automacao_coleta_sequencial) and
       NaoEstaVazio(DestaxaClient.Resposta.transacao_nsu) then
    begin
      while (DestaxaClient.ColetaResposta.automacao_coleta_retorno = dcrExecutarProcedimento) and
            (DestaxaClient.ColetaResposta.transacao_nsu <> ultNSU) do
      begin
        ultNSU := DestaxaClient.ColetaResposta.transacao_nsu;
        wResp := TACBrTEFRespDestaxa.Create;
        try
          wResp.Conteudo.GravaInformacao(899, 201, DestaxaClient.ColetaResposta.AsString);
          wResp.ConteudoToProperty;

          AListaRespostasTEF.AdicionarRespostaTEF(wResp);
        finally
          wResp.Free;
        end;

        DestaxaClient.ColetaRequisicao.automacao_coleta_retorno := dcrExecutarProcedimento;
        DestaxaClient.ColetaRequisicao.automacao_coleta_sequencial := DestaxaClient.ColetaResposta.automacao_coleta_sequencial;
        DestaxaClient.ColetaRequisicao.automacao_coleta_informacao := CDESTAXA_ADM_ANTERIOR;
        DestaxaClient.Socket.ExecutarColeta;
      end;

      DestaxaClient.ColetaRequisicao.automacao_coleta_retorno := dcrExecutarProcedimento;
      DestaxaClient.ColetaRequisicao.automacao_coleta_sequencial := DestaxaClient.ColetaResposta.automacao_coleta_sequencial;
      DestaxaClient.ColetaRequisicao.automacao_coleta_informacao := CDESTAXA_ADM_FECHAR;
      DestaxaClient.Socket.ExecutarColeta;
    end;
  finally
    DestaxaClient.Socket.ColetaAutomatica := True;
    DestaxaClient.FinalizarRequisicao;
  end;
end;}

procedure TACBrTEFAPIClassDestaxa.ResolverTransacaoPendente(aStatus: TACBrTEFStatusTransacao);
var
  achou: Boolean;
  status, nsu, ultNSU: String;
begin
  status := EmptyStr;
  ultNSU := EmptyStr;
  achou := False;
  nsu := fpACBrTEFAPI.UltimaRespostaTEF.NSU;

  case aStatus of
    tefstsSucessoAutomatico, tefstsSucessoManual: status := CDESTAXA_ADM_CONFIRMAR;
    tefstsErroImpressao, tefstsErroDispesador, tefstsErroEnergia, tefstsErroDiverso: status := CDESTAXA_ADM_DESFAZER;
  end;

  DestaxaClient.IniciarRequisicao;
  DestaxaClient.Socket.ColetaAutomatica := False;
  try
    DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
    DestaxaClient.Requisicao.retorno := drqExecutarServico;
    DestaxaClient.ExecutarTransacaoSilenciosa(CDESTAXA_ADM_PENDENTE);
    if NaoEstaZerado(DestaxaClient.Resposta.automacao_coleta_sequencial) and
       NaoEstaVazio(DestaxaClient.Resposta.transacao_nsu) then
    begin
      while (DestaxaClient.ColetaResposta.automacao_coleta_retorno = dcrExecutarProcedimento) and
            (DestaxaClient.ColetaResposta.transacao_nsu <> ultNSU) do
      begin
        ultNSU := DestaxaClient.ColetaResposta.transacao_nsu;

        achou := (DestaxaClient.ColetaResposta.transacao_nsu = nsu);
        if achou then
          DestaxaClient.ColetaRequisicao.automacao_coleta_informacao := status
        else
          DestaxaClient.ColetaRequisicao.automacao_coleta_informacao := CDESTAXA_ADM_ANTERIOR;

        DestaxaClient.ColetaRequisicao.automacao_coleta_retorno := dcrExecutarProcedimento;
        DestaxaClient.ColetaRequisicao.automacao_coleta_sequencial := DestaxaClient.ColetaResposta.automacao_coleta_sequencial;
        DestaxaClient.Socket.ExecutarColeta;
      end;

      if (not achou) then
      begin
        DestaxaClient.ColetaRequisicao.automacao_coleta_retorno := dcrExecutarProcedimento;
        DestaxaClient.ColetaRequisicao.automacao_coleta_sequencial := DestaxaClient.ColetaResposta.automacao_coleta_sequencial;
        DestaxaClient.ColetaRequisicao.automacao_coleta_informacao := CDESTAXA_ADM_FECHAR;
        DestaxaClient.Socket.ExecutarColeta;
      end;
    end;
  finally
    DestaxaClient.Socket.ColetaAutomatica := True;
    DestaxaClient.FinalizarRequisicao;
  end;
end;

procedure TACBrTEFAPIClassDestaxa.AbortarTransacaoEmAndamento;
begin
  DestaxaClient.Requisicao.sequencial := DestaxaClient.UltimoSequencial+1;
  DestaxaClient.Requisicao.retorno := drqCancelarTransacao;
  DestaxaClient.Socket.Executar(DestaxaClient.Resposta.transacao);
end;

function TACBrTEFAPIClassDestaxa.VerificarPresencaPinPad: Byte;
begin
  Result := 0;
  DestaxaClient.IniciarRequisicao;
  try
    if DestaxaClient.Resposta.estado.ConfiguradoComPinpad then
      Result := 99;
  finally
    DestaxaClient.FinalizarRequisicao;
  end;
end;

end.
