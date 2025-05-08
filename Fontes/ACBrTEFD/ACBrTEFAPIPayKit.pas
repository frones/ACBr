{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrTEFAPIPayKit;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFPayKitAPI;

type

  { TACBrTEFRespPayKit }

  TACBrTEFRespPayKit = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
  end;


  { TACBrTEFAPIClassPayKit }

  TACBrTEFAPIClassPayKit = class(TACBrTEFAPIClass)
  private
    function GetTEFPayKitAPI: TACBrTEFPayKitAPI;

    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandoExibirMensagemAPI(const Mensagem: String;
      TipoMensagem: TACBrTEFPayKitTipoMensagem; MilissegundosExibicao: Integer);
    procedure QuandoPerguntarMenuAPI(const Titulo: String; Opcoes: TStringList;
      var ItemSelecionado: Integer);
    procedure QuandoPerguntarCampoAPI(DefinicaoCampo: TACBrTEFPayKitDefinicaoCampo;
      var Resposta: String; var Acao: Integer);

    procedure QuandoExibirQRCodeAPI(const DadosQRCode: String);

  protected
    procedure InterpretarRespostaAPI; override;

    function ExibirVersaoPayKit: Boolean;
  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function EfetuarPagamento(
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0;
      DadosAdicionais: String = ''): Boolean; override;

    function EfetuarAdministrativa(
      CodOperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; override;
    function EfetuarAdministrativa(
      const CodOperacaoAdm: string = ''): Boolean; overload; override;

    function CancelarTransacao(
      const NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      const CodigoFinalizacao: string = '';
      const Rede: string = ''): Boolean; override;

    procedure FinalizarTransacao(
      const Rede, NSU, CodigoFinalizacao: String;
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); override;

    procedure ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao = tefstsSucessoManual); override;
    procedure AbortarTransacaoEmAndamento; override;

    procedure ExibirMensagemPinPad(const MsgPinPad: String); override;
    procedure ObterListaImagensPinPad(ALista: TStrings); override;

    property TEFPayKitAPI: TACBrTEFPayKitAPI read GetTEFPayKitAPI;
  end;

implementation

uses
  math, StrUtils,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

{ TACBrTEFRespPayKit }

procedure TACBrTEFRespPayKit.ConteudoToProperty;
begin
  inherited;
end;


{ TACBrTEFAPIClassPayKit }

constructor TACBrTEFAPIClassPayKit.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespPayKit;

  with GetTEFPayKitAPI do
  begin
    OnGravarLog := QuandoGravarLogAPI;
    OnExibeMensagem := QuandoExibirMensagemAPI;
    QuandoPerguntarMenu := QuandoPerguntarMenuAPI;
    QuandoPerguntarCampo := QuandoPerguntarCampoAPI;
    QuandoExibirQRCode := QuandoExibirQRCodeAPI;
  end;
end;

destructor TACBrTEFAPIClassPayKit.Destroy;
begin
  //fTEFPayKitAPI.Free;  // Libera em ACBrTEFPayKitAPI.finalization;
  inherited;
end;

procedure TACBrTEFAPIClassPayKit.Inicializar;
var
  s: String;
begin
  if Inicializado then
    Exit;

  with GetTEFPayKitAPI do
  begin
    PathPayKit := PathDLL;
    NomeAutomacao := fpACBrTEFAPI.DadosAutomacao.NomeAplicacao;
    VersaoAutomacao := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
    CNPJEstabelecimento := fpACBrTEFAPI.DadosEstabelecimento.CNPJ;

    NumeroEmpresa := StrToIntDef(fpACBrTEFAPI.DadosTerminal.CodEmpresa, 0);
    NumeroLoja := StrToIntDef(fpACBrTEFAPI.DadosTerminal.CodFilial, 0);
    NumeroPDV := StrToIntDef(fpACBrTEFAPI.DadosTerminal.CodTerminal, 0);
    PortaPinPad := fpACBrTEFAPI.DadosTerminal.PortaPinPad;

    s := fpACBrTEFAPI.DadosTerminal.EnderecoServidor;
    if (s <> '') then
      if (copy(s, Length(s)-1, 1) <> ':') then  // Informou parâmetro TLS ?
        s := s + ':1';                    // Se não informou, assume como ligado
    ConfiguracaoIpPortaSsl := s;

    Inicializar;
  end;

  inherited;
end;

procedure TACBrTEFAPIClassPayKit.DesInicializar;
begin
  GetTEFPayKitAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAPIClassPayKit.InterpretarRespostaAPI;
//var
//  i: Integer;
//  AChave, AValue: String;
begin
  fpACBrTEFAPI.UltimaRespostaTEF.Clear;
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;

  //for i := 0 to fTEFPayKitAPI.DadosDaTransacao.Count-1 do
  //begin
  //  AChave := fTEFPayKitAPI.DadosDaTransacao.Names[i];
  //  AValue := fTEFPayKitAPI.DadosDaTransacao.ValueFromIndex[i];
  //
  //  fpACBrTEFAPI.UltimaRespostaTEF.Conteudo.GravaInformacao(AChave, AValue);
  //end;

  fpACBrTEFAPI.UltimaRespostaTEF.ConteudoToProperty;
end;

function TACBrTEFAPIClassPayKit.ExibirVersaoPayKit: Boolean;
var
  s: String;
begin
  s := Trim(GetTEFPayKitAPI.VersaoDPOS);
  Result := (s <> '');
  if Result then
    TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(s, telaOperador, 0);
end;

function TACBrTEFAPIClassPayKit.GetTEFPayKitAPI: TACBrTEFPayKitAPI;
begin
  Result := ACBrTEFPayKitAPI.GetTEFPayKitAPI;
end;

procedure TACBrTEFAPIClassPayKit.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAPIClassPayKit.QuandoExibirMensagemAPI(
  const Mensagem: String; TipoMensagem: TACBrTEFPayKitTipoMensagem;
  MilissegundosExibicao: Integer);
var
  tela: TACBrTEFAPITela;
begin
  case TipoMensagem of
    msgInfo: tela := telaCliente;
    msgAlerta: tela := telaOperador;
    msgErro: tela := telaOperador;
    msgAdicional: tela := telaCliente;
    msgTerminal: tela := telaTodas;
  else
    tela := telaTodas;
  end;

  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem( Mensagem, tela, MilissegundosExibicao);
end;

procedure TACBrTEFAPIClassPayKit.QuandoPerguntarMenuAPI(const Titulo: String;
  Opcoes: TStringList; var ItemSelecionado: Integer);
begin
  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu( Titulo, Opcoes, ItemSelecionado);
end;

procedure TACBrTEFAPIClassPayKit.QuandoPerguntarCampoAPI(
  DefinicaoCampo: TACBrTEFPayKitDefinicaoCampo; var Resposta: String;
  var Acao: Integer);
var
  def: TACBrTEFAPIDefinicaoCampo;
  Validado, Cancelado: Boolean;
begin
  def.TituloPergunta := DefinicaoCampo.TituloPergunta;
  def.ValorInicial := DefinicaoCampo.ValorInicial;
  def.MascaraDeCaptura := DefinicaoCampo.MascaraDeCaptura;
  def.TamanhoMaximo := DefinicaoCampo.TamanhoMaximo;
  def.ValorMaximo := Trunc(DefinicaoCampo.ValorMaximo);
  def.TamanhoMinimo := DefinicaoCampo.TamanhoMinimo;
  def.ValorMinimo := Trunc(DefinicaoCampo.ValorMinimo);
  def.TipoDeEntrada := tedTodos;

  case DefinicaoCampo.TipoDeEntrada of
    teBarrasDigitado:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.TipoEntradaCodigoBarras := tbDigitado;
      end;

    teBarrasLido:
      def.TipoEntradaCodigoBarras := tbLeitor;

    teValidade:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.MascaraDeCaptura := '@@/@@';
        def.ValidacaoDado := valdMesAno;
      end;

    teData:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.MascaraDeCaptura := '@@/@@/@@';
        def.ValidacaoDado := valdDiaMesAno;
      end;

    teCodSeguranca:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.TamanhoMinimo := 3;
        def.OcultarDadosDigitados := True;
      end;

    teCartao, teValor, teNumero, teValorEspecial:
      begin
        def.TipoDeEntrada := tedNumerico;
      end;
  end;

  Validado := True;
  Cancelado := False;
  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarCampo(def, Resposta, Validado, Cancelado);

  if Cancelado then
    Acao := -1
  else
    Acao := 0;
end;

procedure TACBrTEFAPIClassPayKit.QuandoExibirQRCodeAPI(const DadosQRCode: String);
begin
  with TACBrTEFAPI(fpACBrTEFAPI) do
  begin
    if Assigned( QuandoExibirQRCode ) then
      QuandoExibirQRCode( DadosQRCode );
  end;
end;

function TACBrTEFAPIClassPayKit.EfetuarAdministrativa(CodOperacaoAdm: TACBrTEFOperacao): Boolean;
//var
//  Param1: String;
begin
  //fTEFPayKitAPI.DadosDaTransacao.Clear;
  Result := False;
  //if CodOperacaoAdm = tefopAdministrativo then
  //  CodOperacaoAdm := PerguntarMenuAdmScope;

  Result := True;
  //Param1 := '';

  case CodOperacaoAdm of
    tefopVersao:
      Result := ExibirVersaoPayKit();
  end;
end;

function TACBrTEFAPIClassPayKit.EfetuarAdministrativa(const CodOperacaoAdm: string): Boolean;
begin
  Result := EfetuarAdministrativa( TACBrTEFOperacao(StrToIntDef(CodOperacaoAdm, 0)) );
end;

function TACBrTEFAPIClassPayKit.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
begin
end;

function TACBrTEFAPIClassPayKit.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
begin
end;

procedure TACBrTEFAPIClassPayKit.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
end;

procedure TACBrTEFAPIClassPayKit.ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao);
begin
  FinalizarTransacao( fpACBrTEFAPI.UltimaRespostaTEF.Rede,
                      fpACBrTEFAPI.UltimaRespostaTEF.NSU,
                      fpACBrTEFAPI.UltimaRespostaTEF.Finalizacao,
                      AStatus );
end;

procedure TACBrTEFAPIClassPayKit.AbortarTransacaoEmAndamento;
begin
  //GetTEFPayKitAPI.AbortarTransacao;
end;

procedure TACBrTEFAPIClassPayKit.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  GetTEFPayKitAPI.ExibirMensagemPinPad(MsgPinPad, 5000);
end;

procedure TACBrTEFAPIClassPayKit.ObterListaImagensPinPad(ALista: TStrings);
var
  p, l: Integer;
  s, n: String;
begin
  ALista.Clear;
  s := Trim(GetTEFPayKitAPI.ListaArquivosMultimidia);
  p := 1;
  l := Length(s);
  while p < l do
  begin
    n := copy(s, p, 8);
    ALista.Add(n);
    Inc(p, 8);
  end;
end;

end.
