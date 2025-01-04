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
    procedure QuandoExibirQRCodeAPI(const Dados: String);

    procedure SetDiretorioTrabalho(const AValue: String);
    function DadoPinPadToMsg(ADadoPinPad: TACBrTEFAPIDadoPinPad): Word;

  protected
    function IniciarTransacao(OperacaoScope: TACBrTEFScopeOperacao;
       const Param1: String = ''; const Param2: String = ''; const Param3: String = ''): LongInt;
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
  math, TypInfo, DateUtils, StrUtils,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.Math,
  ACBrUtil.FilesIO;

{ TACBrTEFRespScope }

procedure TACBrTEFRespScope.ConteudoToProperty;
  procedure TrataCamposMask1(const IDCampo: Int64; Linha: TACBrTEFLinha);
  begin
    //MASK1_Numero_Conta_PAN
    //MASK1_Data_referencia
    //MASK1_Codigo_Origem_Mensagem
    //MASK1_Cod_Servico    // Ver tabela Código serviços página 340
    //MASK1_Texto_BIT_62   // Parece muito o espelho do comprovante????
    //MASK1_Cartao_Trilha02
    //MASK1_Numero_Promissorias
    //MASK1_Cod_Estab_Impresso //<-- CNPJ do Estabelecimento
    //MASK1_CGC_Convenio
    //MASK1_Msg_Autentic_Cheque
    //MASK1_Saldo_Disponivel

    if IDCampo = MASK1_Numero_Conta_PAN then
      NFCeSAT.UltimosQuatroDigitos := RightStr(Linha.Informacao.AsString, 4)
    else if IDCampo = MASK1_Valor_transacao then
      ValorTotal := Linha.Informacao.AsFloat
    else if IDCampo = MASK1_NSU_transacao then
      NSU_TEF := Linha.Informacao.AsString
    else if IDCampo = MASK1_Hora_local_transacao then
      DataHoraTransacaoLocal := DateOf(DataHoraTransacaoLocal) + Linha.Informacao.AsTime
    else if IDCampo = MASK1_Data_local_transacao then
    begin
      if Trim(Linha.Informacao.AsString) <> '' then
        DataHoraTransacaoLocal := Linha.Informacao.AsDate + TimeOf(DataHoraTransacaoLocal);
    end
    else if IDCampo = MASK1_Data_vencimento_cartao then
      NFCeSAT.DataExpiracao := Linha.Informacao.AsString
    else if IDCampo = MASK1_Numero_cheque then
      Cheque := Linha.Informacao.AsString
    else if IDCampo = MASK1_Codigo_autorizacao then
    begin
      CodigoAutorizacaoTransacao := Linha.Informacao.AsString;
      NFCeSAT.Autorizacao := CodigoAutorizacaoTransacao;
    end
    else if IDCampo = MASK1_Codigo_resposta then
      CodigoRedeAutorizada := Linha.Informacao.AsString
    else if IDCampo = MASK1_Identificacao_terminal then
      SerialPOS := Linha.Informacao.AsString
    else if IDCampo = MASK1_Num_Parcelas then
      QtdParcelas := Linha.Informacao.AsInteger
    else if IDCampo = MASK1_Valor_Taxa_Servico then
      TaxaServico := Linha.Informacao.AsFloat
    else if IDCampo = MASK1_NSU_Host then
      NSU := Linha.Informacao.AsString
    else if IDCampo = MASK1_Cod_Banco then
      Banco := Linha.Informacao.AsString
    else if IDCampo = MASK1_Cod_Agencia then
      Agencia := Linha.Informacao.AsString
    else if IDCampo = MASK1_Data_Vencimento then
    begin
      if Trim(Linha.Informacao.AsString) <> '' then
        DataVencimento := Linha.Informacao.AsTimeStamp;
    end
    else if IDCampo = MASK1_Cod_Bandeira then
      CodigoBandeiraPadrao := Linha.Informacao.AsString
    else if IDCampo = MASK1_Controle_Dac then
      Finalizacao := Linha.Informacao.AsString
    else if IDCampo = MASK1_Cod_Rede then
      Rede := Linha.Informacao.AsString
    else if IDCampo = MASK1_Nome_Bandeira then
      NFCeSAT.Bandeira := Linha.Informacao.AsString
    else if IDCampo = MASK1_Nome_Rede then
      NomeAdministradora := Linha.Informacao.AsString
    else if IDCampo = MASK1_Numero_CMC7 then
      CMC7 := Linha.Informacao.AsString

  end;

  procedure TrataCamposMask2(const IDCampo: Int64; Linha: TACBrTEFLinha);
  var
    s: String;
  begin
    //MASK2_Cliente_Com_Seguro:
    //MASK2_Dados_Parcelado_Cetelem:
    //MASK2_Data_Movimento:
    //MASK2_Nome_Convenio:
    //MASK2_Lista_TEF_Permitidas:
    //MASK2_Dados_Consulta_Fatura:
    //MASK2_Codigo_Resposta_AVS:
    //MASK2_Pontos_AdquiridosOuResgatados:
    //MASK2_Fator_Compra:
    //MASK2_NSU_Host_Transacao_Original:
    //MASK2_Identificacao_Cliente_PBM:
    //MASK2_Cod_Local_Telefone:
    //MASK2_Num_Telefone:
    //MASK2_Dados_ValeGas:
    //MASK2_Num_Item_Finivest_ou_Contrato:
    //MASK2_Valor_Taxa_Embarque:
    //MASK2_Digitos_TR2SON:
    //MASK2_Taxa_Cliente_Lojista:
    //MASK2_Cod_Servico_Original:
    //MASK2_Cod_Barras:
    //MASK2_Logo_PAN:
    //MASK2_Cod_Empresa:
    //MASK2_Cod_Autenticacao:
    //MASK2_Dados_Pagto_ISOGCB:
    //MASK2_UsoRes_63:
    //MASK2_Numero_PDV

    if IDCampo = MASK2_Linha_Autenticacao then
      Autenticacao := Linha.Informacao.AsString
    else if IDCampo = MASK2_NSU_transacao_Original then
      NSUTransacaoCancelada := Linha.Informacao.AsString
    else if IDCampo = MASK2_Cod_Operadora then
      CodigoOperadoraCelular := Linha.Informacao.AsString
    else if IDCampo = MASK2_Codigo_IF then
      CodigoPSP := Linha.Informacao.AsString
    else if IDCampo = MASK2_Permite_Desfazimento then
      Confirmar := (Linha.Informacao.AsString = '1')
    else if IDCampo = MASK2_Cod_Empresa then
      Estabelecimento := Linha.Informacao.AsString
    else if IDCampo = MASK2_Forma_Financiamento then
    begin
      s := UpperCase(Trim(Linha.Informacao.AsString));
      if s = 'A' then
        ParceladoPor := parcADM
      else if s = 'E' then
        ParceladoPor := parcLoja
      else
        ParceladoPor := parcNenhum;
    end;
  end;

  procedure TrataCamposMask3(const IDCampo: Int64; Linha: TACBrTEFLinha);
  begin
    //MASK3_DadosQtdeECupons:
    //MASK3_DescResgateMonetario:
    //MASK3_Dados_Pagto_Bit48_BRADESCO:
    //MASK3_Modo_Entrada:
    //MASK3_Resposta_Consulta_Infocards:
    //MASK3_Dados_Resposta_Consulta_EPAY_INCOMM:
    //MASK3_Dados_Resposta_Consulta_INCOMM:
    //MASK3_Max_Mercadorias_TicketCar:
    //MASK3_Versao_Carga_Tabelas_Host:
    //MASK3_Dados_Correspondente_Bancario:
    //MASK3_Dados_Adicionais_Gift_Card:
    //MASK3_Dados_Operacao_Fidelidade_SGF:
    //MASK3_Valor_Total_Pagamento:
    //MASK3_Valor_Entrada_IATA:
    //MASK3_Valor_Acrescimos_Pagamento:
    //MASK3_Dados_Perfil_Pagamento_Recorrente:
    //MASK3_Dados_Assinatura_Pagamento:
    //MASK3_Dados_Consulta_BACEN:
    //MASK3_Valor_Documento:
    //MASK3_Resposta_Consulta_BACEN_Comprovante:
    //MASK3_Modo_Pagamento:
    //MASK3_Consulta_Cedente_BACEN_BRADESCO:
    //MASK3_Data_Vencimento_CORBAN

    if IDCampo = MASK3_Codigo_SAT then
      NFCeSAT.CodCredenciadora := Linha.Informacao.AsString
    else if IDCampo = MASK3_CNPJ_Rede_Credenciadora_SAT then
        NFCeSAT.CNPJCredenciadora := Linha.Informacao.AsString
    else if IDCampo = MASK3_Valor_Descontos_Pagamento then
      Desconto := Linha.Informacao.AsFloat
    else if IDCampo = MASK3_Valor_Saque then
      Saque := Linha.Informacao.AsFloat;
  end;

  procedure TrataCamposMask4(const IDCampo: Int64; Linha: TACBrTEFLinha);
  begin
    //MASK4_Merchant_ID:
    //MASK4_Codigo_Estab_Externo:
    //MASK4_Relacao_Descontos_Item:
    //MASK4_Indicador_Saldo_Disponivel:
    //MASK4_Numero_CPF:
    //MASK4_ARQC_Chip:
    //MASK4_AID_Chip:
    //MASK4_Transacao_Autorizada_Por_Senha:
    //MASK4_Campo_TID_Pix:
    //MASK4_Campo_Referencia_Pix:
    //MASK4_Tamanho_BIN:
    //MASK4_Dados_DCC:
    //MASK4_Status_DCC:

    if (IDCampo = MASK4_Nome_Portador_Cartao) then
      NFCeSAT.DonoCartao := Linha.Informacao.AsString
    else if (IDCampo = MASK4_Data_Validade_Cartao) and (NFCeSAT.DataExpiracao = '') then
      NFCeSAT.DataExpiracao := Linha.Informacao.AsString
    else if (IDCampo = MASK4_String_QRCode) and (QRCode = '') then
      QRCode := Linha.Informacao.AsString;
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

    if (LinChave = RET_STATUS) then
      Sucesso := (StrToIntDef(Trim(Linha.Informacao.AsString), -1) = RCS_SUCESSO)
    else if (LinChave = RET_CUPOM_LOJA) then
      ImagemComprovante2aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = RET_CUPOM_CLIENTE) then
      ImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = RET_CUPOM_REDUZIDO) and ViaClienteReduzida then
      ImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = RET_CHEQUE_BANCO) then
      Banco := Linha.Informacao.AsString
    else if (LinChave = RET_CHEQUE_AGENCIA) then
      Agencia := Linha.Informacao.AsString
    else if (LinChave = RET_CHEQUE_NUMERO) then
      Cheque := Linha.Informacao.AsString
    else if (LinChave = RET_CHEQUE_DATA) then
      DataCheque := Linha.Informacao.AsDate
    else if (LinChave = RET_MSG_OPERADOR) then
      TextoEspecialOperador := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = RET_MSG_CLIENTE) then
      TextoEspecialCliente := StringToBinaryString( Linha.Informacao.AsString )
    else if (LinChave = RET_QRCODE) then
      QRCode := Linha.Informacao.AsString;

    // Ex.: mask1-$00000001:
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

  QtdLinhasComprovante := max(ImagemComprovante1aVia.Count, ImagemComprovante2aVia.Count);
  Confirmar := Confirmar or (QtdLinhasComprovante > 0);
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
  fTEFScopeAPI.OnExibeQRCode := QuandoExibirQRCodeAPI;
end;

destructor TACBrTEFAPIClassScope.Destroy;
begin
  fTEFScopeAPI.Free;
  inherited;
end;

procedure TACBrTEFAPIClassScope.Inicializar;
var
  P: Integer;
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
  Inferior, Superior, ValResp: Double;
  s: String;
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

    Validado := True;
    if (DefCampo.MascaraDeCaptura = '') then
    begin
      case DefCampo.TipoDeEntrada of
        tedNumerico: Validado := StrIsNumber(Resposta);
        tedAlfabetico: Validado := StrIsAlpha(Resposta);
        tedAlfaNum: Validado := StrIsAlphaNum(Resposta);
      end;
    end;

    if Validado and (Param_Coleta_Ext.UsaLimites = 1)then
    begin
      Inferior := StrToIntDef(Param_Coleta_Ext.Limite.Inferior, 0)/100;
      Superior := StrToIntDef(Param_Coleta_Ext.Limite.Superior, 0)/100;
      ValResp  := StrToIntDef(OnlyNumber(Resposta), 0)/100;
      if (Inferior <> 0) then
        Validado := (ValResp >= Inferior);
      if Validado and (Superior <> 0) then
        Validado := (ValResp <= Superior);
    end;

    if Validado then
    begin
      case DefCampo.TipoCampo of
        TM_DDMMAA: Validado := ValidarDDMMAA(Resposta);
        TM_DDMM: Validado := ValidarDDMM(Resposta);
        TM_MMAA: Validado := ValidarMMAA(Resposta);
        TM_HHMMSS: Validado := ValidarHHMMSS(Resposta);
        TM_DDMMAAAA: Validado := ValidarDDMMAAAA(Resposta);
        TM_MMAAAA: Validado := ValidarMMAAAA(Resposta);
        TM_HHMM: Validado := ValidarHHMM(Resposta);
        TM_BOOL: Validado := (Resposta = '0') or (Resposta = '1');
        TM_NUM_DECIMAL, TM_VALOR_MONETARIO:
          begin
            s := Trim(Resposta);
            s := StringReplace(s, ',', '', [rfReplaceAll]);
            s := StringReplace(s, '.', '', [rfReplaceAll]);
            s := StringReplace(s, 'R$', '', [rfReplaceAll]);
            Validado := StrIsNumber(s);
          end
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

procedure TACBrTEFAPIClassScope.QuandoExibirQRCodeAPI(const Dados: String);
begin
  if not Assigned(TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirQRCode) then
    fpACBrTEFAPI.DoException( Format(ACBrStr(sACBrTEFAPIEventoInvalidoException),
                                     ['QuandoExibirQRCode']));

  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirQRCode(Dados);
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
        Param1 := IntToStr(SRV_PAGTO_CONTA_CARTAO);
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

  Result := ExecutarTransacaoScopeSessaoUnica(scoCanc, Param1, Param2);
end;

function TACBrTEFAPIClassScope.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  Param1, Param2: string;
  op: TACBrTEFScopeOperacao;
  ret: LongInt;
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
    op := scoCarteiraVirtual;
    Param2 := '432';  // 432=PIX
  end
  else if (Financiamento = tefmfPredatado) then
    op := scoPreAutCredito
  else if (teftcCredito in CartoesAceitos) or (teftcPrivateLabel in CartoesAceitos) then
    op := scoCredito
  else  // Debito, Voucher, Frota
    op := scoDebito;

  ret := IniciarTransacao(op, Param1, Param2);
  if (ret = RCS_SUCESSO) then
    ret := fTEFScopeAPI.ExecutarTransacao;

  Result := (ret = RCS_SUCESSO);
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

function TACBrTEFAPIClassScope.IniciarTransacao(
  OperacaoScope: TACBrTEFScopeOperacao; const Param1: String;
  const Param2: String; const Param3: String): LongInt;
begin
  fpACBrTEFAPI.UltimaRespostaTEF.Clear;
  fTEFScopeAPI.DadosDaTransacao.Clear;
  Result := fTEFScopeAPI.IniciarTransacao(OperacaoScope, Param1, Param2, Param3);
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
var
  ret: LongInt;
begin
  Result := False;
  if fTEFScopeAPI.SessaoAberta then
    fTEFScopeAPI.FecharSessaoTEF;

  ret := IniciarTransacao(OperacaoScope, Param1, Param2, Param3);
  if (ret = RCS_SUCESSO) then
  begin
    try
      ret := fTEFScopeAPI.ExecutarTransacao;
    finally
      fTEFScopeAPI.FecharSessaoTEF;
    end;
  end;

  Result := (ret = RCS_SUCESSO);
end;

end.
