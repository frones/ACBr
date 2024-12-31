{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrTEFAPIScope;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFScopeAPI;

const
  CSUBDIRETORIO_SCOPE = 'scope';

resourcestring
  sErro_RespostaInvalida = 'Resposta inválida: %s';

type

  { TACBrTEFRespScope }

  TACBrTEFRespScope = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
  end;


  { TACBrTEFAPIClassScope }

  TACBrTEFAPIClassScope = class(TACBrTEFAPIClass)
  private
    fDiretorioTrabalho: String;
    fTEFScopeAPI: TACBrTEFScopeAPI;

    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandocopeTransacaoEmAndamentoAPI( EstadoOperacao: TACBrTEFScopeEstadoOperacao;
      out Cancelar: Boolean);

    procedure QuandoExibirMensagemAPI(const Mensagem: String;
       Terminal: TACBrTEFScopeTerminalMensagem; MilissegundosExibicao: Integer);
    procedure QuandoPerguntarMenuAPI(const Titulo: String; Opcoes: TStringList;
       var ItemSelecionado: Integer);
    procedure QuandoPerguntarCampoAPI( const TituloCampo: String;
       const Param_Coleta_Ext: TParam_Coleta_Ext;
       var Resposta: String; var AcaoResposta: Byte);

    procedure SetDiretorioTrabalho(const AValue: String);
    function DadoPinPadToMsg(ADadoPinPad: TACBrTEFAPIDadoPinPad): Word;

  protected
    function ExecutarTransacaoScopeSessaoUnica(OperacaoScope: TACBrTEFScopeOperacao;
       const Param1: String = ''; const Param2: String = ''; const Param3: String = ''): Boolean;

    procedure InterpretarRespostaAPI; override;

    function PerguntarMenuAdmScope: TACBrTEFOperacao;
    function ExibirVersaoScope: Boolean;
    function TestarComunicacaoScope: Boolean;

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
    function ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: integer = 30000;
      MinLen: SmallInt = 0; MaxLen: SmallInt = 0): String; override;
    function MenuPinPad(const Titulo: String; Opcoes: TStrings; TimeOut: Integer = 30000): Integer; override;
    function VerificarPresencaPinPad: Byte; override;

    property TEFScopeAPI: TACBrTEFScopeAPI read fTEFScopeAPI;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
  end;

implementation

uses
  math, TypInfo, DateUtils,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.Math,
  ACBrUtil.FilesIO;

{ TACBrTEFRespScope }

procedure TACBrTEFRespScope.ConteudoToProperty;
  procedure TrataCamposMask1(const IDCampo: Int64; Linha: TACBrTEFLinha);
  begin
    case IDCampo of
      MASK1_Numero_Conta_PAN:
        ;
      MASK1_Valor_transacao:
        ValorTotal := Linha.Informacao.AsFloat;
      MASK1_NSU_transacao:
        NSU_TEF := Linha.Informacao.AsString;
      MASK1_Hora_local_transacao:
        DataHoraTransacaoLocal := DateOf(DataHoraTransacaoLocal) + Linha.Informacao.AsTime;
      MASK1_Data_local_transacao:
        DataHoraTransacaoLocal := Linha.Informacao.AsDate + TimeOf(DataHoraTransacaoLocal);
      MASK1_Data_vencimento_cartao:
        NFCeSAT.DataExpiracao := Linha.Informacao.AsString;
      //MASK1_Data_referencia:
      //  ;
      MASK1_Numero_cheque:
        Cheque := Linha.Informacao.AsString;
      MASK1_Codigo_autorizacao:
        CodigoAutorizacaoTransacao := Linha.Informacao.AsString;
      MASK1_Codigo_resposta:
        CodigoRedeAutorizada := Linha.Informacao.AsString;
      MASK1_Identificacao_terminal:
        Estabelecimento := Linha.Informacao.AsString; //???
      //MASK1_Codigo_Origem_Mensagem:
      //  ;
      MASK1_Plano_Pagamento:
        QtdParcelas := Linha.Informacao.AsInteger;
      MASK1_Valor_Taxa_Servico:
        TaxaServico := Linha.Informacao.AsFloat;
      MASK1_NSU_Host:
        NSU := Linha.Informacao.AsString;
      MASK1_Cod_Banco:
        Banco := Linha.Informacao.AsString;
      MASK1_Cod_Agencia:
        Agencia := Linha.Informacao.AsString;
      MASK1_Data_Vencimento:
        DataVencimento := Linha.Informacao.AsTimeStamp;
      MASK1_Cod_Bandeira:
        CodigoBandeiraPadrao := Linha.Informacao.AsString;
      MASK1_Cod_Servico:
        ;//Ver tabela Código serviços página 40
      MASK1_Texto_BIT_62:
        ;// Parece muito o espelho do comprovante????
      MASK1_Controle_Dac:
        Finalizacao := Linha.Informacao.AsString;
      MASK1_Cod_Rede:
        CodigoRedeAutorizada := Linha.Informacao.AsString;
      MASK1_Nome_Bandeira:
        NFCeSAT.Bandeira := Linha.Informacao.AsString;
      MASK1_Nome_Rede:
        Rede := Linha.Informacao.AsString;
      MASK1_Cartao_Trilha02:
        ; //????
      //MASK1_Numero_Promissorias:
      //  ;
      MASK1_Cod_Estab_Impresso:
        ;
      MASK1_Numero_CMC7:
        ;
      MASK1_CGC_Convenio:
        ;
      MASK1_Msg_Autentic_Cheque:
        ;
    else
      // Case Delphi não permite valores fora do range 32 bits
      if IDCampo =  MASK1_Saldo_Disponivel then
      begin

      end
      else
      begin

      end;

    end;
  end;

  procedure TrataCamposMask2(const IDCampo: Int64; Linha: TACBrTEFLinha);
  begin
    case IDCampo of
      MASK2_NSU_transacao_Original:
        NSUTransacaoCancelada := Linha.Informacao.AsBinary;
      MASK2_Cliente_Com_Seguro:
        ;
      MASK2_Dados_Parcelado_Cetelem:
        ;
      MASK2_Data_Movimento:
        ;
      MASK2_Nome_Convenio:
        ;
      MASK2_Lista_TEF_Permitidas:
        ;
      MASK2_Linha_Autenticacao:
        ;
      MASK2_Dados_Consulta_Fatura:
        ;
      MASK2_Forma_Financiamento:
        ;
      MASK2_Codigo_Resposta_AVS:
        ;
      MASK2_Pontos_AdquiridosOuResgatados:
        ;
      MASK2_Fator_Compra:
        ;
      MASK2_NSU_Host_Transacao_Original:
        ;
      MASK2_Identificacao_Cliente_PBM:
        ;
      MASK2_Cod_Operadora:
        ;
      MASK2_Cod_Local_Telefone:
        ;
      MASK2_Num_Telefone:
        ;
      MASK2_Dados_ValeGas:
        ;
      MASK2_Codigo_IF:
        ;
      MASK2_Num_Item_Finivest_ou_Contrato:
        ;
      MASK2_Valor_Taxa_Embarque:
        ;
      MASK2_Digitos_TR2SON:
        ;
      MASK2_Taxa_Cliente_Lojista:
        ;
      MASK2_Cod_Servico_Original:
        ;
      MASK2_Cod_Barras:
        ;
      MASK2_Permite_Desfazimento:
        ;
      MASK2_Logo_PAN:
        ;
      MASK2_Cod_Empresa:
        ;
      MASK2_Cod_Autenticacao:
        ;
      MASK2_Dados_Pagto_ISOGCB:
        ;
      MASK2_UsoRes_63:
        ;
    else
      // Case Delphi não permite valores fora do range 32 bits
      if idcampo = MASK2_Numero_PDV then
      begin

      end
      else;



    end;
  end;

  procedure TrataCamposMask3(const IDCampo: Int64; Linha: TACBrTEFLinha);
  begin
    case IDCampo of
      MASK3_DadosQtdeECupons:
        ;
      MASK3_DescResgateMonetario:
        ;
      MASK3_Dados_Pagto_Bit48_BRADESCO:
        ;
      MASK3_Modo_Entrada:
        ;
      MASK3_Valor_Saque:
        ;
      MASK3_Resposta_Consulta_Infocards:
        ;
      MASK3_Dados_Resposta_Consulta_EPAY_INCOMM:
        ;
      //MASK3_Dados_Resposta_Consulta_INCOMM:
      //  ;
      MASK3_Max_Mercadorias_TicketCar:
        ;
      MASK3_Codigo_SAT:
        ; //Ver tabela Código das redes página 322
      MASK3_Versao_Carga_Tabelas_Host:
        ;
      MASK3_CNPJ_Rede_Credenciadora_SAT:
        NFCeSAT.CNPJCredenciadora := Linha.Informacao.AsString;
      MASK3_Dados_Correspondente_Bancario:
        ;
      MASK3_Dados_Adicionais_Gift_Card:
        ;
      MASK3_Dados_Operacao_Fidelidade_SGF:
        ;
      MASK3_Valor_Total_Pagamento:
        ;
      MASK3_Valor_Descontos_Pagamento:
        Desconto := Linha.Informacao.AsFloat ;
      MASK3_Valor_Entrada_IATA:
        ;
      MASK3_Valor_Acrescimos_Pagamento:
        ;
      MASK3_Dados_Perfil_Pagamento_Recorrente:
        ;
      MASK3_Dados_Assinatura_Pagamento:
        ;
      MASK3_Dados_Consulta_BACEN:
        ;
      MASK3_Valor_Documento:
        ;
      MASK3_Resposta_Consulta_BACEN_Comprovante:
        ;
      MASK3_Modo_Pagamento:
        ;
      MASK3_Consulta_Cedente_BACEN_BRADESCO:
        ;

    else
      // Case Delphi não permite valores fora do range 32 bits
      if idcampo = MASK3_Data_Vencimento_CORBAN then
      begin

      end
      else;

    end;
  end;

  procedure TrataCamposMask4(const IDCampo: Int64; Linha: TACBrTEFLinha);
  begin
    case IDCampo of
      MASK4_Nome_Portador_Cartao:
        ;
      MASK4_Data_Validade_Cartao:
        ;
      MASK4_Merchant_ID:
        ;
      MASK4_Codigo_Estab_Externo:
        ;
      MASK4_String_QRCode:
        QRCode := Linha.Informacao.AsBinary;
      MASK4_Relacao_Descontos_Item:
        ;
      MASK4_Indicador_Saldo_Disponivel:
        ;
      MASK4_Numero_CPF:
        ;
      MASK4_ARQC_Chip:
        ;
      MASK4_AID_Chip:
        ;
      MASK4_Transacao_Autorizada_Por_Senha:
        ;
      //MASK4_????????
      MASK4_Campo_TID_Pix:
        ;
      MASK4_Campo_Referencia_Pix:
        ;
      MASK4_Tamanho_BIN:
        ;
      MASK4_Dados_DCC:
        ;
      MASK4_Status_DCC:
        ;
    //else
    //
    end;
  end;


var
  I,P: Integer;
  LinChave: String;
  Linha: TACBrTEFLinha;
  mask, sIDCampo: string;
  IDCampo: Int64;
begin
  ImagemComprovante1aVia.Clear;
  ImagemComprovante2aVia.Clear;
  Debito := False;
  Credito := False;
  Digitado := False;
  TaxaServico := 0;
  DataHoraTransacaoCancelada := 0;
  DataHoraTransacaoLocal := 0;

  for I := 0 to Conteudo.Count - 1 do
  begin
    Linha := Conteudo.Linha[I];
    LinChave := Linha.Chave;

    if (LinChave = CCUPOM_LOJA) then
      ImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = CCUPOM_CLIENTE) then
      ImagemComprovante2aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = CCUPOM_REDUZIDO) and ViaClienteReduzida then
      ImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = CCHEQUE_BANCO) then
      Banco := Linha.Informacao.AsString
    else if (LinChave = CCHEQUE_AGENCIA) then
      Banco := Linha.Informacao.AsString
    else if (LinChave = CCHEQUE_NUMERO) then
      Cheque := Linha.Informacao.AsString
    else if (LinChave = CCHEQUE_DATA) then
      DataCheque := Linha.Informacao.AsDate;

    //Ex.: mask1-$00000001:
    P := pos('-', LinChave);
    mask := copy(LinChave, 1, P - 1);
    sIDCampo := copy(LinChave, P + 1, Length(LinChave));
    IDCampo := StrToInt64Def(sIDCampo, 0);
    if (mask = 'mask1') then
      TrataCamposMask1(IDCampo, Linha)
    else if (mask = 'mask2') then
      TrataCamposMask2(IDCampo, Linha)
    else if (mask = 'mask3') then
      TrataCamposMask3(IDCampo, Linha)
    else if (mask = 'mask4') then
      TrataCamposMask4(IDCampo, Linha)
    else
      ProcessarTipoInterno(Linha);
  end;

  //ConteudoToComprovantes;
  //ConteudoToParcelas;
  //TODO: verificar???
  QtdLinhasComprovante := max(ImagemComprovante1aVia.Count, ImagemComprovante2aVia.Count);
  Sucesso := Trim(ImagemComprovante1aVia.Text) <> '';
  Confirmar := True;
end;


{ TACBrTEFAPIClassScope }

constructor TACBrTEFAPIClassScope.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespScope;

  fTEFScopeAPI := TACBrTEFScopeAPI.Create;
  fTEFScopeAPI.OnTransacaoEmAndamento := QuandocopeTransacaoEmAndamentoAPI;
  fTEFScopeAPI.OnGravarLog := QuandoGravarLogAPI;
  fTEFScopeAPI.OnExibeMensagem := QuandoExibirMensagemAPI;
  fTEFScopeAPI.OnExibeMenu := QuandoPerguntarMenuAPI;
  fTEFScopeAPI.OnPerguntaCampo := QuandoPerguntarCampoAPI;
end;

destructor TACBrTEFAPIClassScope.Destroy;
begin
  fTEFScopeAPI.Free;
  inherited;
end;

procedure TACBrTEFAPIClassScope.Inicializar;
var
  i, P: Integer;
  ADir, IpStr, PortaStr: String;
begin
  if Inicializado then
    Exit;

  if (fDiretorioTrabalho = '') then
    ADir := PathWithDelim(fpACBrTEFAPI.DiretorioTrabalho) + CSUBDIRETORIO_SCOPE
  else
    ADir := fDiretorioTrabalho;

  IpStr := fpACBrTEFAPI.DadosTerminal.EnderecoServidor;
  PortaStr := '';
  p := pos(':', IpStr);
  if (p > 0) then
  begin
    PortaStr := copy(IpStr, p+1, Length(IpStr));
    IpStr := copy(IpStr, 1, p-1);
  end;

  fTEFScopeAPI.DiretorioTrabalho := ADir;
  fTEFScopeAPI.EnderecoIP := IpStr;
  fTEFScopeAPI.PortaTCP := PortaStr;
  fTEFScopeAPI.VersaoAutomacao := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
  fTEFScopeAPI.Empresa := fpACBrTEFAPI.DadosTerminal.CodEmpresa;
  fTEFScopeAPI.Filial := fpACBrTEFAPI.DadosTerminal.CodFilial;
  fTEFScopeAPI.PDV := fpACBrTEFAPI.DadosTerminal.CodTerminal;
  fTEFScopeAPI.MsgPinPad := fpACBrTEFAPI.DadosAutomacao.NomeSoftwareHouse + '|' +
                            fpACBrTEFAPI.DadosAutomacao.NomeAplicacao + ' ' +
                            fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
  fTEFScopeAPI.PortaPinPad := fpACBrTEFAPI.DadosTerminal.PortaPinPad;

  fTEFScopeAPI.Inicializar;

  inherited;
end;

procedure TACBrTEFAPIClassScope.DesInicializar;
begin
  fTEFScopeAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAPIClassScope.InterpretarRespostaAPI;
var
  i: Integer;
  AChave, AValue: String;
begin
  //inherited;
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;

  for i := 0 to fTEFScopeAPI.DadosDaTransacao.Count-1 do
  begin
    AChave := fTEFScopeAPI.DadosDaTransacao.Names[i];
    AValue := fTEFScopeAPI.DadosDaTransacao.ValueFromIndex[i];

    fpACBrTEFAPI.UltimaRespostaTEF.Conteudo.GravaInformacao(AChave, AValue);
  end;

  fpACBrTEFAPI.UltimaRespostaTEF.ConteudoToProperty;
end;

function TACBrTEFAPIClassScope.PerguntarMenuAdmScope: TACBrTEFOperacao;
var
  slMenu: TStringList;
  ItemSel: Integer;
begin
  Result := tefopNenhuma;

  slMenu := TStringList.Create;
  try
    slMenu.Add(ACBrStr('Versão'));
    slMenu.Add(ACBrStr('Teste Comunicação'));
    slMenu.Add(ACBrStr('Reimpressão'));
    slMenu.Add(ACBrStr('Cancelamento'));
    slMenu.Add(ACBrStr('Pré-autorização Crédito'));
    slMenu.Add(ACBrStr('Resumo de Vendas'));
    slMenu.Add(ACBrStr('Pagamento de Conta'));
    slMenu.Add(ACBrStr('Recarga de Celular'));
    slMenu.Add(ACBrStr('Consulta CDC'));
    slMenu.Add(ACBrStr('Consulta Cheque'));
    slMenu.Add(ACBrStr('Administrativo'));
    ItemSel := -1;
    TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu( 'Menu Administrativo', slMenu, ItemSel );
    case ItemSel of
      0: Result := tefopVersao;
      1: Result := tefopTesteComunicacao;
      2: Result := tefopReimpressao;
      3: Result := tefopCancelamento;
      4: Result := tefopPreAutorizacao;
      5: Result := tefopRelatSintetico;
      6: Result := tefopPagamentoConta;
      7: Result := tefopPrePago;
      8: Result := tefopConsultaSaldo;
      9: Result := tefopConsultaCheque;
     10: Result := tefopAdministrativo;
    end;
  finally
    slMenu.Free;
  end;
end;

function TACBrTEFAPIClassScope.ExibirVersaoScope: Boolean;
var
  s: String;
begin
  s := Trim(fTEFScopeAPI.ObterVersaoScope);
  Result := (s <> '');
  if Result then
    TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(s, telaOperador, 0);
end;

function TACBrTEFAPIClassScope.TestarComunicacaoScope: Boolean;
var
  s: String;
begin
  fTEFScopeAPI.AbrirSessaoTEF;
  fTEFScopeAPI.FecharSessaoTEF;
  Result := True;
  s := Format(ACBrStr('Comunicação OK.'+sLineBreak+sLineBreak+
                      'Empresa: %s, Filial %s, PDV: %S'+sLineBreak+
                      'Servidor: %s:%s'),
         [fTEFScopeAPI.Empresa, fTEFScopeAPI.Filial, fTEFScopeAPI.PDV,
          fTEFScopeAPI.EnderecoIP, fTEFScopeAPI.PortaTCP]);
  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(s, telaOperador, 0);
end;

procedure TACBrTEFAPIClassScope.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAPIClassScope.QuandocopeTransacaoEmAndamentoAPI(
  EstadoOperacao: TACBrTEFScopeEstadoOperacao; out Cancelar: Boolean);
var
  i: Integer;
begin
  i := Integer(EstadoOperacao);
  Cancelar := False;
  TACBrTEFAPI(fpACBrTEFAPI).QuandoEsperarOperacao(TACBrTEFAPIOperacaoAPI(i), Cancelar);
end;

procedure TACBrTEFAPIClassScope.QuandoExibirMensagemAPI(const Mensagem: String;
  Terminal: TACBrTEFScopeTerminalMensagem; MilissegundosExibicao: Integer);
var
  TelaMsg: TACBrTEFAPITela;
begin
  case Terminal of
    tmOperador: TelaMsg := telaOperador;
    tmCliente: TelaMsg := telaCliente;
  else
    TelaMsg := telaTodas;
  end;

  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(
    Mensagem,
    TelaMsg,
    MilissegundosExibicao );
end;

procedure TACBrTEFAPIClassScope.QuandoPerguntarMenuAPI(const Titulo: String;
  Opcoes: TStringList; var ItemSelecionado: Integer);
begin
  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu( Titulo, Opcoes, ItemSelecionado);
end;

procedure TACBrTEFAPIClassScope.QuandoPerguntarCampoAPI(
  const TituloCampo: String; const Param_Coleta_Ext: TParam_Coleta_Ext;
  var Resposta: String; var AcaoResposta: Byte);
var
  Validado, Cancelado: Boolean;
  DefCampo: TACBrTEFAPIDefinicaoCampo;
begin
  DefCampo.TituloPergunta := TituloCampo;
  DefCampo.MascaraDeCaptura := '';
  DefCampo.TipoDeEntrada := tedTodos;
  DefCampo.TamanhoMinimo := 0;
  DefCampo.TamanhoMaximo := 100;
  DefCampo.ValorMinimo := 0;
  DefCampo.ValorMaximo := 0;
  DefCampo.OcultarDadosDigitados := False;
  DefCampo.ValidacaoDado := valdNenhuma;
  DefCampo.ValorInicial := Resposta;
  DefCampo.MsgErroDeValidacao := '';
  DefCampo.MsgErroDadoMaior := '';
  DefCampo.MsgErroDadoMenor := '';
  DefCampo.MsgConfirmacaoDuplaDigitacao := '';
  DefCampo.TipoEntradaCodigoBarras := tbQualquer;
  DefCampo.TipoCampo := Param_Coleta_Ext.FormatoDado;

  case Param_Coleta_Ext.FormatoDado of
    TM_DDMMAA:     // String representando uma data no formato “DDMMAA”
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 6;
        DefCampo.TamanhoMaximo := 6;
        DefCampo.ValidacaoDado := valdDiaMesAno;
      end;
    TM_DDMM,            // 1 - String representando uma data no formato “DDMM”
    TM_ULTIMOS_DIGITOS: // 6 - String representando um número com 4 dígitos
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 4;
        DefCampo.TamanhoMaximo := 4;
      end;
    TM_MMAA:     // String representando uma data no formato “MMAA”
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 4;
        DefCampo.TamanhoMaximo := 4;
        DefCampo.ValidacaoDado := valdMesAno;
      end;
    TM_HHMMSS:     // String representando uma hora no formato “HHMMSS”
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 6;
        DefCampo.TamanhoMaximo := 6;
      end;
    TM_NUM:     // String representando um número
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
      end;
    TM_SENHA:     // String representando uma senha que é numérica
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.OcultarDadosDigitados := True;
        DefCampo.ValidacaoDado := valdSenhaGerente;
      end;
    TM_ALFANUMERICO:     // String representando um dado alfanumérico
      begin
        DefCampo.TipoDeEntrada := tedAlfaNum;
      end;
    TM_DDMMAAAA:     // String representando uma data no formato “DDMMAAAA”
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 8;
        DefCampo.TamanhoMaximo := 8;
        DefCampo.ValidacaoDado := valdDiaMesAno;
      end;
    TM_CONFIRMACAO:     // String de display para exibição (não há coleta)
      begin
        DefCampo.TipoDeEntrada := tedApenasLeitura;
      end;
    TM_MMAAAA:     // String representando uma data no formato “MMAAAA”
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 6;
        DefCampo.TamanhoMaximo := 6;
        DefCampo.ValidacaoDado := valdMesAno;
      end;
    TM_MASCARADO:     // Exibir ‘*’ na tela, mas enviar em claro
      begin
        DefCampo.OcultarDadosDigitados := True;
        DefCampo.ValidacaoDado := valdSenhaLojista;
      end;
    TM_HHMM:     // String representando uma hora no formato “HHMM
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 4;
        DefCampo.TamanhoMaximo := 4;
      end;
    TM_BOOL:     // Booleano, a resposta deve ser 0=Não ou 1=Sim
      begin
        //TODO: mudar para Menu
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 1;
        DefCampo.TamanhoMaximo := 1;
      end;
    TM_VALOR_MONETARIO:     // String representando “valor monetário” com tamanho total de 12, sendo os dois últimos dígitos os centavos
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 1;
        DefCampo.TamanhoMaximo := 12;
        DefCampo.MascaraDeCaptura := '@@@.@@@.@@@.@@@,@@';
      end;
    TM_NUM_DECIMAL:     // String representando número não inteiro, com casas decimais
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 1;
        DefCampo.TamanhoMaximo := 12;
        DefCampo.MascaraDeCaptura := '@,@@';
      end;
    TM_PAN:     // String representando o PAN do cartão
      begin
        DefCampo.TipoDeEntrada := tedNumerico;
        DefCampo.TamanhoMinimo := 16;
        DefCampo.TamanhoMaximo := 16;
        DefCampo.MascaraDeCaptura := '@@@@.@@@@.@@@@.@@@@';
      end;
  end;

  Validado := False;
  Cancelado := False;
  while not Validado do
  begin
    TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarCampo(DefCampo, Resposta, Validado, Cancelado);
    if Cancelado then
      Break;

    if not Validado then
    begin
      case DefCampo.TipoDeEntrada of
        tedNumerico: Validado := StrIsNumber(Resposta);
        tedAlfabetico: Validado := StrIsAlpha(Resposta);
        tedAlfaNum: Validado := StrIsAlphaNum(Resposta);
      else
        Validado := True;
      end;
    end;

    if Validado then
    begin
      case DefCampo.TipoCampo of
        0: Validado := ValidarDDMMAA(Resposta);
        1: Validado := ValidarDDMM(Resposta);
        2: Validado := ValidarMMAA(Resposta);
        3: Validado := ValidarHHMMSS(Resposta);
        8: Validado := ValidarDDMMAAAA(Resposta);
       10: Validado := ValidarMMAAAA(Resposta);
       12: Validado := ValidarHHMM(Resposta);
       13: Validado := (Resposta = '0') or (Resposta = '1');
      else
        Validado := True;
      end;
    end;

    if not Validado then
      QuandoExibirMensagemAPI(Format(ACBrStr(sErro_RespostaInvalida), [Resposta]), tmTodas, 0);
  end;

  if Cancelado or (Resposta = ':-1') then
    AcaoResposta := ACAO_CANCELAR
  else if (Resposta = ':-2') then
    AcaoResposta := ACAO_ESTADO_ANTERIOR
  else
    AcaoResposta := ACAO_PROXIMO_ESTADO;
end;


function TACBrTEFAPIClassScope.EfetuarAdministrativa(CodOperacaoAdm: TACBrTEFOperacao): Boolean;
var
  OpScope: TACBrTEFScopeOperacao;
  Param1: String;
begin
  Result := False;
  if CodOperacaoAdm = tefopAdministrativo then
    CodOperacaoAdm := PerguntarMenuAdmScope;

  OpScope := scoNone;
  Result := True;
  Param1 := '';

  case CodOperacaoAdm of
    tefopVersao:
      Result := ExibirVersaoScope();

    tefopTesteComunicacao:
      Result := TestarComunicacaoScope();

    tefopReimpressao:
      OpScope := scoReimpComp;

    tefopCancelamento:
      OpScope := scoCanc;              // Valor, Taxa Serviço

    tefopPreAutorizacao:
      OpScope := scoPreAutCredito;     // Valor, Taxa Serviço

    tefopRelatSintetico:
      OpScope := scoResVenda;

    tefopPagamentoConta:               // Serviço, Bandeira
      begin
        OpScope := scoPagto;
        Param1 := '87';
      end;

    tefopPrePago:
      OpScope := scoPreAutCredito;     // Valor, Taxa Serviço

    tefopConsultaSaldo:
      OpScope := scoConsCDC;           // Valor, Taxa Serviço

    tefopConsultaCheque:
      OpScope := scoCheque;            // Valor

  else
    OpScope := scoMenu;
  end;

  if (OpScope <> scoNone) then
    Result := ExecutarTransacaoScopeSessaoUnica(OpScope, Param1);
end;

function TACBrTEFAPIClassScope.EfetuarAdministrativa(const CodOperacaoAdm: string): Boolean;
begin
  Result := EfetuarAdministrativa( TACBrTEFOperacao(StrToIntDef(CodOperacaoAdm, 0)) );
end;

function TACBrTEFAPIClassScope.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
var
  Param1, Param2: string;
begin
  Param1 := IntToStr(Trunc(RoundTo(Valor * 100,-2)));
  Param2 := '000';

  if (CodigoFinalizacao <> '') then
    fTEFScopeAPI.RespostasPorEstados.Values[IntToStr(TC_CONTROLE)] := CodigoFinalizacao;

  ExecutarTransacaoScopeSessaoUnica(scoCanc, Param1, Param2);
  Result := fpACBrTEFAPI.UltimaRespostaTEF.Sucesso;
end;

function TACBrTEFAPIClassScope.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  Param1, Param2: string;
  op: TACBrTEFScopeOperacao;
begin
  VerificarIdentificadorVendaInformado;
  if (ValorPagto <= 0) then
    fpACBrTEFAPI.DoException(sACBrTEFAPIValorPagamentoInvalidoException);

  Param1 := IntToStr(Trunc(RoundTo(ValorPagto * 100,-2)));
  Param2 := '000';

  if (Financiamento = tefmfAVista) then
    fTEFScopeAPI.RespostasPorEstados.Values[IntToStr(TC_DECIDE_AVISTA)] := '1'
  else if (Financiamento > tefmfAVista) then   // Parcelado
    fTEFScopeAPI.RespostasPorEstados.Values[IntToStr(TC_DECIDE_AVISTA)] := '0';

  if (Parcelas > 0) then
    fTEFScopeAPI.RespostasPorEstados.Values[IntToStr(TC_QTDE_PARCELAS)] := IntToStr(Parcelas);

  if (Modalidade = tefmpCheque) then
    op := scoCheque
  else if (Modalidade = tefmpCarteiraVirtual) then
  begin
    op := scoPagto;
    Param1 := '432';  // 432=PIX
  end
  else if (Financiamento = tefmfPredatado) then
    op := scoPreAutCredito
  else if (teftcCredito in CartoesAceitos) or (teftcPrivateLabel in CartoesAceitos) then
    op := scoCredito
  else  // Debito, Voucher, Frota
    op := scoDebito;

  fTEFScopeAPI.IniciarTransacao(op, Param1, Param2);
  fTEFScopeAPI.ExecutarTransacao;

  Result := fpACBrTEFAPI.UltimaRespostaTEF.Sucesso;
end;

procedure TACBrTEFAPIClassScope.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  Confirmar: Boolean;
  TransacaoFoiDesfeita: Boolean;
begin
  // TEF Scope, não tem o conceito de tratamento de transações anteriores, a não ser da Última transação
  //i := fpACBrTEFAPI.RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);

  Confirmar := (AStatus in [tefstsSucessoAutomatico, tefstsSucessoManual]);
  fTEFScopeAPI.FecharSessaoTEF(Confirmar, TransacaoFoiDesfeita);
  if TransacaoFoiDesfeita then
    TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(ACBrStr(sErrUltTransDesfeita), telaOperador, 0);
end;

procedure TACBrTEFAPIClassScope.ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao);
begin
  FinalizarTransacao( fpACBrTEFAPI.UltimaRespostaTEF.Rede,
                      fpACBrTEFAPI.UltimaRespostaTEF.NSU,
                      fpACBrTEFAPI.UltimaRespostaTEF.Finalizacao,
                      AStatus );
end;

procedure TACBrTEFAPIClassScope.AbortarTransacaoEmAndamento;
begin
  fTEFScopeAPI.AbortarTransacao;
end;

procedure TACBrTEFAPIClassScope.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  fTEFScopeAPI.ExibirMensagemPinPad(MsgPinPad);
end;

function TACBrTEFAPIClassScope.ObterDadoPinPad(
  TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: integer; MinLen: SmallInt;
  MaxLen: SmallInt): String;
var
  Dado: Word;
begin
  Dado := DadoPinPadToMsg(TipoDado);
  if (Dado < 1) then
  begin
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada),
      [GetEnumName(TypeInfo(TACBrTEFAPIDadoPinPad), integer(TipoDado) ), ClassName] ));
  end;

  if (MinLen = 0) and (MaxLen = 0) then
    CalcularTamanhosCampoDadoPinPad(TipoDado, MinLen, MaxLen);

  Result := fTEFScopeAPI.ObterDadoPinPad(Dado, MinLen, MaxLen, TimeOut);
end;

function TACBrTEFAPIClassScope.MenuPinPad(const Titulo: String;
  Opcoes: TStrings; TimeOut: Integer): Integer;
begin
  Result := fTEFScopeAPI.MenuPinPad(Titulo, Opcoes, TimeOut);
end;

function TACBrTEFAPIClassScope.DadoPinPadToMsg(
  ADadoPinPad: TACBrTEFAPIDadoPinPad): Word;
begin
  case ADadoPinPad of
    dpDDD: Result := PP_DIGITE_O_DDD;
    dpRedDDD: Result := PP_REDIGITE_O_DDD;
    dpFone: Result := PP_DIGITE_O_TELEFONE;
    dpRedFone: Result := PP_REDIGITE_O_TELEFONE;
    dpDDDeFone: Result := PP_DIGITE_DDD_TELEFONE;
    dpRedDDDeFone: Result := PP_REDIGITE_DDD_TELEFONE;
    dpCPF: Result := PP_DIGITE_O_CPF;
    dpRedCPF: Result := PP_REDIGITE_O_CPF;
    dpRG: Result := PP_DIGITE_O_RG;
    dpRedRG: Result := PP_REDIGITE_O_RG;
    dp4UltDigitos: Result := PP_DIGITE_OS_4_ULTIMOS_DIGITOS;
    dpCodSeguranca: Result := PP_DIGITE_CODIGO_DE_SEGURANCA;
    dpCNPJ: Result := PP_DIGITE_O_CNPJ;
    dpRedCNPJ: Result := PP_REDIGITE_O_CNPJ;
  else
    Result := 0;
  end;
end;

function TACBrTEFAPIClassScope.VerificarPresencaPinPad: Byte;
var
  s: String;
begin
  s := fTEFScopeAPI.AcharPortaPinPad;
  Result := StrToIntDef(s, 0);
end;

procedure TACBrTEFAPIClassScope.SetDiretorioTrabalho(const AValue: String);
begin
  if fDiretorioTrabalho = AValue then Exit;

  if Inicializado then
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                                     ['TACBrTEFAPIClassScope.DiretorioTrabalho']));

  fDiretorioTrabalho := AValue;
end;

function TACBrTEFAPIClassScope.ExecutarTransacaoScopeSessaoUnica(
  OperacaoScope: TACBrTEFScopeOperacao; const Param1: String;
  const Param2: String; const Param3: String): Boolean;
begin
  Result := False;
  fTEFScopeAPI.IniciarTransacao(OperacaoScope, Param1, Param2,Param3);
  try
    fTEFScopeAPI.ExecutarTransacao;
  finally
    fTEFScopeAPI.FecharSessaoTEF;
    Result := True;
  end;
end;

end.
