  {******************************************************************************}
  { Projeto: Componentes ACBr                                                    }
  {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
  { mentos de Automação Comercial utilizados no Brasil                           }
  {                                                                              }
  { Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
  {                                                                              }
  { Colaboradores nesse arquivo:   Juliana Rodrigues Prado                       }
  {                            :   Agnaldo Pedroni                               }
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
unit ACBrBancoMercantil;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoConversao;

type

    { TACBrBancoMercantil }

  TACBrBancoMercantil = class(TACBrBancoClass)
  private
    function FormataNossoNumero(const ACBrTitulo: TACBrTitulo): String;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
  public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    procedure GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(aRemessa: TStringList); override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses
{$IFDEF COMPILER6_UP} dateutils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

  { TACBrBancoMercantil }

function TACBrBancoMercantil.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02:
      Result := toRemessaBaixar; {Pedido de Baixa}
    04:
      Result := toRemessaConcederAbatimento; {Concessão de Abatimento}
    05:
      Result := toRemessaCancelarAbatimento; {Cancelamento de Abatimento concedido}
    06:
      Result := toRemessaAlterarVencimento; {Prorrogação}
    07:
      Result := toRemessaAlterarUsoEmpresa; {Troca Uso da Empresa}
    09:
      Result := toRemessaProtestar; {Protestar}
    10:
      Result := toRemessaNaoProtestar; {Não Protestar}
    18:
      Result := toRemessaCancelarInstrucaoProtestoBaixa; {Sustar Protesto e Baixar Título}
    19:
      Result := toRemessaCancelarInstrucaoProtesto; {Sustar o Protesto e Manter em Carteira}
    31:
      Result := toRemessaOutrasOcorrencias; {Alteração de Outros Dados}
    34:
      Result := toRemessaBaixaporPagtoDiretoCedente; {Baixa por ter sido pago Diretamente ao Cedente}
    90:
      Result := toRemessaOutrasAlteracoes; {Troca de emitente}
    else
      Result := toRemessaRegistrar; {Remessa}
  end;
end;

constructor TACBrBancoMercantil.create(AOwner: TACBrBanco);
begin
  inherited create(AOwner);
  fpDigito                := 1;
  fpNome                  := 'Banco Mercantil';
  fpNumero                := 389;
  fpTamanhoMaximoNossoNum := 6;
  fpTamanhoCarteira       := 2;
end;

function TACBrBancoMercantil.FormataNossoNumero(const ACBrTitulo: TACBrTitulo): String;
var
  ANossoNumero: string;
  aModalidade : String;
begin
  aModalidade := IfThen(Length(trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade)) = 2, ACBrTitulo.ACBrBoleto.Cedente.Modalidade, '22');

  ANossoNumero := PadRight(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4, '0') + //4
    aModalidade + //6
    ACBrTitulo.Carteira + //8
    PadRight(ACBrTitulo.NossoNumero, 6, '0') + //14
    CalcularDigitoVerificador(ACBrTitulo); //15

  Result := ANossoNumero;
end;

function TACBrBancoMercantil.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String;
var
  aModalidade: String;
begin
  Modulo.CalculoPadrao;
  Modulo.MultiplicadorAtual := 2;

  aModalidade := IfThen(Length(trim(ACBrTitulo.ACBrBoleto.Cedente.Modalidade)) = 2, ACBrTitulo.ACBrBoleto.Cedente.Modalidade, '22');

  Modulo.Documento := ACBrTitulo.ACBrBoleto.Cedente.Agencia + aModalidade + ACBrTitulo.Carteira + PadRight(ACBrTitulo.NossoNumero, 6, '0');

  Modulo.Calcular;
  Result := IntToStr(Modulo.DigitoFinal);

end;

function TACBrBancoMercantil.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var
  FatorVencimento, DigitoCodBarras, CpObrigatorio, CpLivre: String;
begin

  with ACBrTitulo.ACBrBoleto do
  begin
    FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

      // comum a todos bancos
    CpObrigatorio := IntToStr(Numero) + //4
      '9' + //5
      FatorVencimento + //9
      IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10); //19

      // AG+22+01+123456
    CpLivre := FormataNossoNumero(ACBrTitulo) + //34
      PadRight(Cedente.CodigoCedente, 9, '0') + //43
      IfThen(ACBrTitulo.ValorDesconto > 0, '0', '2'); // ?? indicador Desconto 2-Sem 0-Com  // 44

    DigitoCodBarras := CalcularDigitoCodigoBarras(CpObrigatorio + CpLivre);

  end;

  Result := IntToStr(Numero) + '9' + DigitoCodBarras + Copy((CpObrigatorio + CpLivre), 5, 39);

end;

function TACBrBancoMercantil.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := Copy(FormataNossoNumero(ACBrTitulo), 5, 11);
end;

  // usado no carnê
function TACBrBancoMercantil.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '-' + ACBrTitulo.ACBrBoleto.Cedente.AgenciaDigito + '/' + ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente;
end;

procedure TACBrBancoMercantil.GerarRegistroHeader400(NumeroRemessa: Integer; aRemessa: TStringList);
var
  wLinha: String;
begin
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    wLinha := '0' + // ID do Registro
      '1' + // ID do Arquivo( 1 - Remessa)
      'REMESSA' + // Literal de Remessa
      '01' + // Código do Tipo de Serviço
      PadRight('COBRANCA', 15) + // Descrição do tipo de serviço
      PadRight(OnlyNumber(Agencia), 4) + // agencia origem
      PadLeft(OnlyNumber(CNPJCPF), 15, '0') + // CNPJ/CPF CEDENTE
      ' ' + // BRANCO
      PadRight(Nome, 30) + // Nome da Empresa
      '389' + // ID BANCO
      'BANCO MERCANTIL' + // nome banco
      FormatDateTime('ddmmyy', Now) + // data geração
      Space(281) + // espaços branco
      '01600   ' + // densidade da gravação
      IntToStrZero(NumeroRemessa, 5) + // nr. sequencial remessa
      IntToStrZero(1, 6); // Nr. Sequencial de Remessa

    aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
  end;
end;

procedure TACBrBancoMercantil.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  Ocorrencia, wLinha     : String;
  TipoSacado, ATipoAceite: String;
begin

  with ACBrTitulo do
  begin
      {Pegando Código da Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar:
        Ocorrencia := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento:
        Ocorrencia := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento:
        Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento:
        Ocorrencia := '06'; {Alteração de vencimento}
      toRemessaAlterarNumeroControle:
        Ocorrencia := '08'; {Alteração de seu número}
      toRemessaProtestar:
        Ocorrencia := '09'; {Pedido de protesto}
      toRemessaCancelarInstrucaoProtestoBaixa:
        Ocorrencia := '18'; {Sustar protesto e baixar}
      toRemessaCancelarInstrucaoProtesto:
        Ocorrencia := '19'; {Sustar protesto e manter na carteira}
      toRemessaOutrasOcorrencias:
        Ocorrencia := '31'; {Alteração de Outros Dados}
      else
        Ocorrencia := '01'; {Remessa}
    end;

      {Pegando Tipo de Sacado}
    case Sacado.Pessoa of
      pFisica:
        TipoSacado := '01';
      pJuridica:
        TipoSacado := '02';
      else
        TipoSacado := '99';
    end;

      { Pegando o Aceite do Titulo }
    case Aceite of
      atSim:
        ATipoAceite := 'S';
      atNao:
        ATipoAceite := 'N';
    end;

    with ACBrBoleto do
    begin
      wLinha := '1' + // ID Registro
        IfThen(PercentualMulta > 0, '092', '000') + // Indica se exite Multa ou não
        IntToStrZero(Round(PercentualMulta * 100), 13) + // Percentual de Multa formatado com 2 casas decimais
        FormatDateTime('ddmmyy', Vencimento + 1) + // data Multa
        Space(5) + PadLeft(Cedente.CodigoCedente, 9, '0') + // numero do contrato ???
        PadLeft(SeuNumero, 25, '0') + // Numero de Controle do Participante
        FormataNossoNumero(ACBrTitulo) + Space(5) + PadLeft(OnlyNumber(Cedente.CNPJCPF), 15, '0') + IntToStrZero(Round(ValorDocumento * 100), 10) + // qtde de moeda
        '1' + // Codigo Operação 1- Cobrança Simples
        Ocorrencia + PadRight(NumeroDocumento, 10) + // numero titulo atribuido pelo cliente
        FormatDateTime('ddmmyy', Vencimento) + IntToStrZero(Round(ValorDocumento * 100), 13) + // valor nominal do titulo
        '389' + // banco conbrador
        '00000' + // agencia cobradora
        '01' + // codigo da especie, duplicata mercantil
        ATipoAceite + // N
        FormatDateTime('ddmmyy', DataDocumento) + // Data de Emissão
        PadRight(Instrucao1, 2, '0') + // instruçoes de cobrança
        PadRight(Instrucao2, 2, '0') + // instruçoes de cobrança
        IntToStrZero(Round(ValorMoraJuros * 100), 13) + // juros mora 11.2
        IfThen(DataDesconto > 0, FormatDateTime('ddmmyy', DataDesconto), PadRight('', 6, '0')) + // data limite desconto
        IntToStrZero(Round(ValorDesconto * 100), 13) + // valor desconto
        StringOfChar('0', 13) + // iof - caso seguro
        StringOfChar('0', 13) + // valor abatimento ?????
        TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0') + PadRight(Sacado.NomeSacado, 40, ' ') + PadRight(Sacado.Logradouro + Sacado.Numero, 40) +
        PadRight(Sacado.Bairro, 12) + PadRight(Sacado.CEP, 8, '0') + PadRight(Sacado.Cidade, 15) + PadRight(Sacado.UF, 2) + PadRight(Sacado.Avalista, 30) + // Avalista
        Space(12) + '1' + // codigo moeda
        IntToStrZero(aRemessa.Count + 1, 6);

      aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
    end;
  end;
end;

procedure TACBrBancoMercantil.GerarRegistroTrailler400(aRemessa: TStringList);
var
  wLinha: String;
begin
  wLinha := '9' + Space(393) + // ID Registro
    IntToStrZero(aRemessa.Count + 1, 6); // Contador de Registros

  aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
end;

function TACBrBancoMercantil.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
  Result        := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0);

  case CodOcorrencia of
    04:
      Result := '04-Alteração de Dados (Entrada)';
    05:
      Result := '05-Alteração de Dados (Baixa)';
    07:
      Result := '07-Liquidação após Baixa';
    08:
      Result := '08-Liquidação em Cartório';
    10:
      Result := '10-Baixa comandada do cliente arquivo';
    15:
      Result := '15-Baixa rejeitada';
    16:
      Result := '16-Instrução rejeitada';
    21:
      Result := '21-Confirma instrução de não protestar';
    22:
      Result := '22-Alteração Seu Numero';
    32:
      Result := '32-Baixa por ter sido protestado';
    36:
      Result := '36-Custas de Edital';
    37:
      Result := '37-Custas de sustação judicial';
    38:
      Result := '38-Título sustado judicialmente';
    55:
      Result := '55-Instrução Codificada';
    56:
      Result := '56-Sustar protesto e manter em carteira';
    65:
      Result := '65-Emissão de Segunda via de aviso';
    67:
      Result := '67-Não conceder juros fora do Prazo';
    69:
      Result := '69-Cancelamento de Liquidação por Cheque Devolvido';
    71:
      Result := '71-Protesto cancelado pelo Cartório';
    75:
      Result := '75-Pagamento Parcial';
    83:
      Result := '83-Cobrança automática de tarifas';
    84:
      Result := '84-Protestar sem mais consultas';
    85:
      Result := '85-Baixaa de Título protestado';
    90:
      Result := '90-Instrução de Protesto Rejeitada';
    95:
      Result := '95-Troca Uso Empresa';
    96:
      Result := '96-Emissão Extrato Mov. Carteira';
    97:
      Result := '97-Tarifa de sustação de protesto';
    98:
      Result := '98-Tarifa de protesto';
    99:
      Result := '99-Custas de protesto';
  end;

  if (Result <> '') then
    Exit;

  case CodOcorrencia of
    02:
      Result := '02-Entrada Confirmada';
    03:
      Result := '03-Entrada Rejeitada';
    06:
      Result := '06-Liquidação Normal';
    09:
      Result := '09-Baixa Simples';
    12:
      Result := '12-Abatimento Concedido';
    13:
      Result := '13-Abatimento Cancelado';
    14:
      Result := '14-Vencimento Alterado';
    17:
      Result := '17-Alterações de dados rejeitados';
    19:
      Result := '19-Confirma instrução de protesto';
    20:
      Result := '20-Confirma instruão de sustação de protesto';
    23:
      Result := '23-Protesto enviado a cartório';
    35:
      Result := '35-Alegações do sacado';
  end;
end;

function TACBrBancoMercantil.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;

  case CodOcorrencia of
    04:
      Result := toRetornoAlteracaoDadosNovaEntrada;
    05:
      Result := toRetornoAlteracaoDadosBaixa;
    07:
      Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    08:
      Result := toRetornoLiquidadoEmCartorio;
    10:
      Result := toRetornoBaixadoViaArquivo; //toRetornoBaixadoInstAgencia;
    15:
      Result := toRetornoBaixaRejeitada; //toRetornoLiquidadoEmCartorio;
    16:
      Result := toRetornoInstrucaoRejeitada; //toRetornoTituloPagoEmCheque;
    21:
      Result := toRetornoRecebimentoInstrucaoNaoProtestar; //toRetornoAcertoControleParticipante;
    32:
      Result := toRetornoBaixaPorProtesto; //toRetornoComandoRecusado;
    36:
      Result := toRetornoCustasEdital;
    37:
      Result := toRetornoCustasSustacaoJudicial;
    38:
      Result := toRetornoTituloSustadoJudicialmente;
    65:
      Result := toRetornoChequePendenteCompensacao;
    69:
      Result := toRetornoChequeDevolvido; //toRetornoCancelamentoDadosRateio;
    71:
      Result := toRetornoDevolvidoPeloCartorio;
    75:
      Result := toRetornoLiquidadoParcialmente;
    90:
      Result := toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    95:
      Result := toRetornoAlteracaoUsoCedente;
    96:
      Result := toRetornoTarifaExtratoPosicao;
    97:
      Result := toRetornoDespesasSustacaoProtesto;
    98:
      Result := toRetornoDespesasProtesto;
    99:
      Result := toRetornoCustasProtesto;
  end;

  if (Result <> toTipoOcorrenciaNenhum) then
    Exit;

  case CodOcorrencia of
    02:
      Result := toRetornoRegistroConfirmado;
    03:
      Result := toRetornoRegistroRecusado;
    06:
      Result := toRetornoLiquidado;
    09:
      Result := toRetornoBaixaSimples; //toRetornoBaixadoViaArquivo;
    12:
      Result := toRetornoAbatimentoConcedido;
    13:
      Result := toRetornoAbatimentoCancelado;
    14:
      Result := toRetornoVencimentoAlterado;
    17:
      Result := toRetornoAlteracaoDadosRejeitados; //toRetornoLiquidadoAposBaixaouNaoRegistro;
    19:
      Result := toRetornoRecebimentoInstrucaoProtestar;
    20:
      Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    23:
      Result := toRetornoEncaminhadoACartorio;
    35:
      Result := toRetornoAlegacaoDoSacado; //toRetornoDesagendamentoDebitoAutomatico;
    36: Result:= toRetornoCustasEdital;
    37: Result:= toRetornoCustasSustacaoJudicial;
    38: Result:= toRetornoTituloSustadoJudicialmente;
    55: Result:= toRetornoAlteracaoInstrucao;
    56: Result:= toRetornoSustacaoSolicitada;
    65: Result:= toRetornoSegundaViaInstrumentoProtesto;
    67: Result:= toRetornoJurosDispensados;
    69: Result:= toRetornoChequeDevolvido;
    71: Result:= toRetornoDevolvidoPeloCartorio;
    75: Result:= toRetornoLiquidadoParcialmente;
    83: Result:= toRetornoCobrancaContratual;
    84: Result:= toRetornoProtestado;
    85: Result:= toRetornoBaixaPorProtesto;
    90: Result:= toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente;
    95: Result:= toRetornoAlteracaoUsoCedente;
    96: Result:= toRetornoTarifaExtratoPosicao;
    97: Result:= toRetornoDespesasSustacaoProtesto;
    98: Result:= toRetornoDespesasProtesto;
    99: Result:= toRetornoCustasProtesto;
    else
      Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoMercantil.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  Result := '';

  case TipoOcorrencia of
    toRetornoAlteracaoDadosNovaEntrada:
      Result := '04';
    toRetornoAlteracaoDadosBaixa:
      Result := '05';
    toRetornoLiquidadoAposBaixaOuNaoRegistro:
      Result := '07';
    toRetornoLiquidadoEmCartorio:
      Result := '08';
    toRetornoBaixadoViaArquivo:
      Result := '10';
    toRetornoBaixaRejeitada:
      Result := '15';
    toRetornoInstrucaoRejeitada:
      Result := '16';
    toRetornoRecebimentoInstrucaoNaoProtestar:
      Result := '21';
    toRetornoAlteracaoSeuNumero:
      Result := '22';
    toRetornoLiquidado:
      Result := '24';
    toRetornoBaixadoFrancoPagamento:
      Result := '31';
    toRetornoCustasEdital:
      Result := '36';
    toRetornoCustasSustacaoJudicial:
      Result := '37';
    toRetornoTituloSustadoJudicialmente:
      Result := '38';
    toRetornoAlteracaoInstrucao:
      Result := '55';
    toRetornoSustacaoSolicitada:
      Result := '56';
    toRetornoSegundaViaInstrumentoProtesto:
      Result := '65';
    toRetornoJurosDispensados:
      Result := '67';
    toRetornoChequeDevolvido:
      Result := '69';
    toRetornoDevolvidoPeloCartorio:
      Result := '71';
    toRetornoLiquidadoParcialmente:
      Result := '75';
    toRetornoCobrancaContratual:
      Result := '83';
    toRetornoProtestado:
      Result := '84';
    toRetornoBaixaPorProtesto:
      Result := '85';
    toRetornoInstrucaoProtestoRejeitadaSustadaOuPendente:
      Result := '90';
    toRetornoAlteracaoUsoCedente:
      Result := '95';
    toRetornoTarifaExtratoPosicao:
      Result := '96';
    toRetornoDespesasSustacaoProtesto:
      Result := '97';
    toRetornoDespesasProtesto:
      Result := '98';
    toRetornoCustasProtesto:
      Result := '99';
  end;

  if (Result <> '') then
    Exit;

  case TipoOcorrencia of
    toRetornoRegistroConfirmado:
      Result := '02';
    toRetornoRegistroRecusado:
      Result := '03';
    toRetornoLiquidado:
      Result := '06';
    toRetornoBaixaAutomatica:
      Result := '09';
    toRetornoAbatimentoConcedido:
      Result := '12';
    toRetornoAbatimentoCancelado:
      Result := '13';
    toRetornoVencimentoAlterado:
      Result := '14';
    toRetornoAlteracaoDadosRejeitados:
      Result := '17';
    toRetornoRecebimentoInstrucaoProtestar:
      Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto:
      Result := '20';
    toRetornoEncaminhadoACartorio:
      Result := '23';
    toRetornoAlegacaoDoSacado:
      Result := '35';
    else
      Result := '02';
  end;
end;

end.
