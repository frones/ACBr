unit ACBrBancoVotorantim;

interface

uses Classes, SysUtils, StrUtils, ACBrBoleto, DateUtils, Math, ACBrBoletoConversao;

type

  { TACBrBancoVotorantim }
  TACBrBancoVotorantim = class(TACBrBancoClass)
  private
  protected
    FNumeroRemessa: Integer;
    FSequencia : Integer;
    FQuantidadeCobrancaSimples,
    FQuantidadeCobrancaVinculada,
    FQuantidadeCobrancaCaucionada,
    FQuantidadeCobrancaDescontada : Integer;
    FValorCobrancaSimples,
    FValorCobrancaVinculada,
    FValorCobrancaCaucionada,
    FValorCobrancaDescontada : Currency;
  public
    constructor Create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string; override;

    procedure GerarRegistroHeader400(NumeroRemessa: integer; ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa: TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;

    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa: TStringList): String; override;
    Procedure LerRetorno240(ARetorno: TStringList); override;


    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): string; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): string; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: integer): string; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;

  end;

implementation

uses ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;

var
  aTotal: Extended;
  aCount: Integer;

{ TACBrBancoVotorantim }

constructor TACBrBancoVotorantim.Create(AOwner: TACBrBanco);
begin
  inherited Create(AOwner);
  fpDigito                := 6;
  fpNome                  := 'Banco Votorantim';
  fpNumero                := 655;
  fpTamanhoAgencia        := 5;
  fpTamanhoConta          := 10;
  fpTamanhoCarteira       := 1;
  fpTamanhoMaximoNossoNum := 10;
  fpLayoutVersaoArquivo   := 87;
  fpLayoutVersaoLote      := 45;
end;

function TACBrBancoVotorantim.CalcularDigitoVerificador(
  const ACBrTitulo: TACBrTitulo): string;
begin
  Modulo.CalculoPadrao;
  Modulo.Documento := ACBrTitulo.NossoNumero;
  Modulo.Calcular;
  if Modulo.ModuloFinal = 0 then
    Result := '1'
  else
    Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoVotorantim.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: integer): string;
begin
  case TipoOcorrencia of
    toRetornoRegistroRecusado:
      case CodMotivo of
        001: Result := '001-VALOR DO ABATIMENTO INVÁLIDO';
        002: Result := '002-AGENCIA ENCARREGADA DA COBRANCA EM BRANCO OU INVÁLIDO';
        003: Result := '003-BAIRRO DO SACADO EM BRANCO OU INVÁLIDO';
        004: Result := '004-CARTEIRA INVÁLIDA';
        005: Result := '005-CEP NÃO NUMÉRICO OU INVALIDO';
        006: Result := '006-CIDADE DO SACADO EM BRANCO OU INVÁLIDO';
        007: Result := '007-INAPTO CNPJ DO CEDENTE INAPTO';
        008: Result := '008-CNPJ/CPF DO SACADO NÃO NUMÉRICO OU IGUAL A ZEROS';
        009: Result := '009-MENSAGEM DE COBRANÇA EM BRANCO OU INVÁLIDA PARA REGISTRO DO TIPO 7';
        010: Result := '010-DATA DE EMISSÃO DE TITULO EM BRANCO OU INVÁLIDO';
        011: Result := '011-DATA DE MORA EM BRANCO OU INVÁLIDO';
        012: Result := '012-DATA DE MULTA INVALIDA/INFERIOR AO VENCIMENTO DO TÍTULO';
        013: Result := '013-INVALIDA/FORA DE PRAZO DE OPERAÇÃO (MÍNIMO OU MÁXIMO)';
        014: Result := '014-DATA LIMITE PARA CONCESSÃO DE DESCONTO INVÁLIDO';
        015: Result := '015-SIGLA DO ESTADO INVÁLIDA';
        016: Result := '016-CEP INCOMPATÍVEL COM A SIGLA DO ESTADO';
        017: Result := '017-IDENTIFICA A ESPECIE DO TITULO EM BRANCO OU INVÁLIDO';
        018: Result := '018-IDENTIFICAÇÃO DO TIPO INSCRIÇÃO DO SACADO EM BRANCO OU INVÁLIDO';
        019: Result := '019-INSTRUÇÃO DE COBRANCA INVÁLIDA';
        020: Result := '020-JUROS DE MORA MAIOR QUE O PERMITIDO';
        021: Result := '021-LOGRADOURO NÃO INFORMADO OU DESLOCADO';
        022: Result := '022-NOME DO SACADO NÃO INFORMADO OU DESLOCADO';
        023: Result := '023-NOSSO NÚMERO JÁ REGISTRADO OU INVÁLIDO';
        024: Result := '024-NUMERO DA INSCRIÇÃO DO SACADO EM BRANCO OU INVÁLIDO';
        025: Result := '025-NUMERO DE INSCRIÇÃO DA EMPRESA EM BRANCO OU INVÁLIDO';
        026: Result := '026-OCORRÊNCIA INVÁLIDA';
        027: Result := '027-PRAZO PARA PROTESTO EM BRANCO OU INVÁLIDO';
        028: Result := '028-NOME NÃO INFORMADO OU DESLOCADO (BANCOS CORRESPONDENTES)';
        029: Result := '029-SEU NUMERO EM BRANCO';
        030: Result := '030-VALOR DE MULTA INVÁLIDO OU MAIOR QUE O PERMITIDO';
        031: Result := '031-VALOR DE MORA POR DIA DE ATRASO EM BRANCO OU INVÁLIDO';
        032: Result := '032-VALOR DO ABATIMENTO A SER CONCEDIDO INVÁLIDO';
        033: Result := '033-VALOR DE DESCONTO A SER CONCEDIDO INVÁLIDO';
        034: Result := '034-VALOR DO ABATIMENTO A SER CONCEDIDO INVÁLIDO';
        035: Result := '035-VALOR NOMINAL DO TITULO EM BRANCO OU INVÁLIDO';
        036: Result := '036-VALOR DE ABATIMENTO INVÁLIDO';
        037: Result := '037-VALOR NOMINAL INVÁLIDO';
        038: Result := '038-DATA DE PRORROGAÇÃO INVÁLIDA';
        039: Result := '039-DATA DE VENCIMENTO MENOR QUE A DATA ATUAL OU INVÁLIDA';
        040: Result := '040-VALOR MINIMO INVÁLIDO';
        041: Result := '041-VALOR MAXIMO INVÁLIDO';
        042: Result := '042-VALOR MINIMO E/O MAXIMO INVÁLIDO(S)';
        043: Result := '043-TITULO JÁ BAIXADO, LIQUIDADO, COM INSTRUÇÃO DE PROTESTO OU RECUSADA PELO BANCO';
        044: Result := '044-NUMERO DE DIAS PARA BAIXA AUTOMÁTICA INVÁLIDO';
        045: Result := '045-TITULO SEM INSTRUÇÃO DE PROTESTO OU BAIXADO/LIQUIDADO';
        046: Result := '046-SUSTAÇÃO DE PROTESTO NÃO PERMITIDA PARA O TITULO';
        047: Result := '047-TITULO SEM INSTRUÇÃO E BAIXA AUTOMÁTICA OU JÁ BAIXADO/LIQUIDADO';
        048: Result := '048-RECUSADO CARTÓRIO SEM CUSTAS';
        049: Result := '049-RECUSADO CARTÓRIO COM CUSTAS';
        050: Result := '050-CONVÊNIO SEM CÓDIGO DO BOLETO PERSONALIZADO CADASTRADO OU CÓDIGO INFORMADO DIFERENTE DO CADASTRADO';
        051: Result := '051-PROTESTO RECUSADO PELO CARTÓRIO';
        052: Result := '052-QUANTIDADE DE PARCELAS INVÁLIDA';
        053: Result := '053-ARQUIVO EXCEDEU A QUANTIDADE DE LINHAS PERMITIDAS';
        054: Result := '054-CONVENIO NÃO PERMITE MENSAGEM PERSONALIZADA';
        else
          Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
      end;
    toRetornoInstrucaoRejeitada:
      case CodMotivo of
        036: Result := '036-ABATIMENTO: VALOR DE ABATIMENTO INVÁLIDO';
        037: Result := '037-ALTERAÇÃO DE VALOR: VALOR NOMINAL INVÁLIDO';
        038: Result := '038-PRORROGAÇÃO: DATA DE PRORROGAÇÃO INVÁLIDA';
        039: Result := '039-ALTERAÇÃO DE VENCIMENTO: DATA DE VENCIMENTO MENOR QUE A DATA ATUAL OU INVÁLIDA';
        040: Result := '040-ALTERAÇÃO DE VALOR MINIMO: VALOR MINIMO INVÁLIDO';
        041: Result := '041-ALTERAÇÃO DE VALOR MAXIMO: VALOR MAXIMO INVÁLIDO';
        042: Result := '042-ALTERAÇÃO DE VALOR MINIMO E MAXIMO: VALOR MINIMO E/O MAXIMO INVÁLIDO(S)';
        043: Result := '043-BAIXA: TITULO JÁ BAIXADO, LIQUIDADO, COM INSTRUÇAÕ DE PROTESTO OU RECUSADA PELO BANCO';
        044: Result := '044-ALTERAÇÃO DE BAIXA AUTOMATICA: NUMERO DE DIAS PARA BAIXA AUTOMÁTICA INVÁLIDO';
        045: Result := '045-NÃO PROTESTAR: TITULO SEM INSTRUÇÃO DE PROTESTO OU BAIXADO/LIQUIDADO';
        046: Result := '046-SUSTAR PROTESTO: SUSTAÇÃO DE PROTESTO NÃO PERMITIDA PARA O TITULO';
        047: Result := '047-NÃO BAIXAR AUTOMATICAMENTE: TITULO SEM INSTRUÇÃO E BAIXA AUTOMÁTICA OU JÁ BAIXADO/LIQUIDADO';
        048: Result := '048-RECUSADO CARTÓRIO: RECUSADO CARTÓRIO SEM CUSTAS';
        049: Result := '049-RECUSADO CARTÓRIO: RECUSADO CARTÓRIO COM CUSTAS';
        051: Result := '051-PROTESTO RECUSADO: PROTESTO RECUSADO PELO CARTÓRIO';
        052: Result := '052-QUANTIDADE DE PARCELAS INVÁLIDA: QUANTIDADE DE PARCELAS INVÁLIDA';
        else
          Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
      end;

  end;
end;

function TACBrBancoVotorantim.CodOcorrenciaToTipo(const CodOcorrencia: integer):
TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoLiquidadoParcialmente;
    08: Result := toRetornoLiquidadoEmCartorio;
    09: Result := toRetornoBaixaAutomatica;
    12: Result := toRetornoAbatimentoConcedido;
    14: Result := toRetornoVencimentoAlterado;
    16: Result := toRetornoInstrucaoRejeitada;
    18: Result := toRetornoConfAlteracaoDiasBaixaAutomatica;
    19: Result := toRetornoConfInstrucaoProtesto;
    20: Result := toRetornoConfInstrucaoSustacaoProtesto;
    21: Result := toRetornoConfInstrucaoNaoProtestar;
    22: Result := toRetornoConfInstrucaoNaoBaixarAutomaticamente;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoConfAlteracaoDiasBaixaAutomatica;
    25: Result := toRetornoConfirmacaoCancelamentoBaixaAutomatica;
    26: Result := toRetornoConfirmacaoAlteracaoValorNominal;
    27: Result := toRetornoConfirmacaoAlteracaoValorMinimoOuPercentual;
    28: Result := toRetornoConfirmacaoAlteracaoValorMaximoOuPercentual;
    29: Result := toRetornoConfirmacaoAlteracaoValorpercentualMinimoMaximo;
    32: Result := toRetornoBaixaPorProtesto;
    33: Result := toRetornoConfirmacaoProtesto;
    34: Result := toRetornoConfirmacaoSustacao;
    35: Result := toRetornoProtestoSustadoJudicialmente;
    47: Result := toRetornoTransferenciaCarteira;
    48: Result := toRetornoAlteracaoPercentualMinimoMaximo;
    49: Result := toRetornoAlteracaoPercentualMinimo;
    50: Result := toRetornoAlteracaoPercentualMaximo;
    51: Result := toRetornoAlteracaoQuantidadeParcela;
    else
      Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoVotorantim.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    06 : Result:= toRemessaAlterarVencimento;               {Prorrogação de vencimento}
    07 : Result:= toRemessaAlterarExclusivoCliente;         {Alteração do valor nominal}
    08 : Result:= toRemessaAlterarVencimento;               {Alteração do vencimento}
    09 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Não baixar automaticamente}
    10 : Result:= toRemessaNaoProtestar;                    {Não Protestar}
    11 : Result:= toRemessaAlterarNumeroDiasBaixa;          {Alteração de dias para baixa automática}
    12 : Result:= toRemessaAlterarValorMinimo;              {Alteração do percentual para mínimo}
    13 : Result:= toRemessaAlterarValorMaximo;              {Alteração do percentual para máximo}
    14 : Result:= toRemessaAlterarValorMinimoMaximo;        {Alteração do percentual para mínimo e máximo}
    15 : Result:= toRemessaAlteracaoQuantidadeParcela;      {Alteração da quantidade de parcelas}
    18 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto}
    36 : Result:= toRemessaProtestarUrgente;                {Protesto urgente}
    48 : Result:= toRemessaRegistrarDireta                  {Remessa cobrança direta (pré-impressa)}
  else
    Result:= toRemessaRegistrar;                            {Remessa Cobrança Escritural}
  end;
end;

function TACBrBancoVotorantim.GerarRegistroHeader240(
  NumeroRemessa: Integer): String;
begin
  raise Exception.create(ACBrStr('CNAB 240 não implementado.'));
end;

procedure TACBrBancoVotorantim.GerarRegistroHeader400(NumeroRemessa: integer;
  ARemessa: TStringList);
var wLinha: String;
  aAgencia,
  aConta: String;
begin
  aTotal := 0;
  aCount := 0;
  aAgencia := PadLeft(RightStr( ACBrBanco.ACBrBoleto.Cedente.Agencia, 5), 5, '0');

  aConta := PadLeft(ACBrBanco.ACBrBoleto.Cedente.Conta, 8, '0') +
              PadLeft(ACBrBanco.ACBrBoleto.Cedente.ContaDigito, 1, '0');
  FNumeroRemessa := NumeroRemessa;
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    wLinha := '0'                                   + // 001-001 ID do Registro Header
              '1'                                   + // 002-002 ID do Arquivo de Remessa
              'REMESSA'                             + // 003-009 Literal de Remessa
              '01'                                  + // 010-011 Código do Tipo de Serviço
              PadRight('COBRANCA', 15)              + // 012-026 Descrição do tipo de serviço + "brancos"
              Space(20)                             + // 027-046 "brancos"
              PadRight(Nome, 30)                    + // 047-076 Nome da Empresa
              IntToStr(Numero)                      + // 077-079 Código do Banco - 655
              PadRight('BANCO VOTORANTIM S/A', 20)  + // 080-099 Nome do Banco - BANCO VOTORANTIM + "brancos"
              FormatDateTime('ddmmyy', Now)         + // 100-105 Data de geração do arquivo
              Space(284)                            + // 106-389 "brancos"
              'CL' + IntToStrZero(NumeroRemessa, 3) + // 390-394 Nr. Sequencial de Geração do Arquivo
              IntToStrZero(1, 6);                     // 395-400 Nr. Sequencial do Registro no Arquivo

    ARemessa.Text := ARemessa.Text + UpperCase(wLinha);
  end;

end;

function TACBrBancoVotorantim.GerarRegistroTrailler240(
  ARemessa: TStringList): String;
begin
  raise Exception.create(ACBrStr('CNAB 240 não implementado.'));

end;

procedure TACBrBancoVotorantim.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
  wLinha := '9'                               + // Identificação do Registro do Trailler
  Space(393)                                  + // "Branco"
  IntToStrZero(ARemessa.Count + 1, 6);          // Número Sequencial do Registro do Arquivo

  ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

function TACBrBancoVotorantim.GerarRegistroTransacao240(
  ACBrTitulo: TACBrTitulo): String;
begin
  raise Exception.create(ACBrStr('CNAB 240 não implementado.'));
end;

procedure TACBrBancoVotorantim.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo;
  aRemessa: TStringList);
var
  wLinha, tipoInscricao,  aConta: String;
  Ocorrencia, aEspecie, aAceite, aInstrucao2: String;
  aTipoSacado, sDataDesconto:String;
begin
  with ACBrTitulo do
  begin
    if ACBrBoleto.Cedente.TipoInscricao = pFisica then
      tipoInscricao := '01'
    else
      tipoInscricao := '02';

    aConta := PadLeft(ACBrBoleto.Cedente.Conta, 8, '0') +
              PadLeft(ACBrBoleto.Cedente.ContaDigito, 1, '0');

    {Pegando Código da Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar                         : Ocorrencia := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento             : Ocorrencia := '04'; {Concessão de Abatimento}
      toRemessaProrrogarVencimento            : Ocorrencia := '06'; {Prorrogação de vencimento}
      toRemessaAlteracaoValorNominal          : Ocorrencia := '07'; {Alteração do Valor Nominal}
      toRemessaAlterarVencimento              : Ocorrencia := '08'; {Alteração de seu número}
      toRemessaNaoBaixarAutomaticamente       : Ocorrencia := '09'; {Pedido de protesto}
      toRemessaNaoProtestar                   : Ocorrencia := '10'; {Não Protestar}
      toRemessaAlterarNumeroDiasProtesto      : Ocorrencia := '11'; {Alteração de dias para baixa automática}
      toRemessaAlteracaoPercentualParaMinimo  : Ocorrencia := '12'; {Alteração do percentual para mínimo}
      toRemessaAlteracaoPercentualParaMaximo  : Ocorrencia := '13'; {Alteração do percentual para máximo}
      toRemessaAlteracaoPercentualParaMinimoMaximo : Ocorrencia := '14'; {Alteração do percentual para mínimo e máximo}
      toRemessaAlteracaoQuantidadeParcela     : Ocorrencia := '15'; {Alteração da quantidade de parcelas}
      toRemessaSustarProtesto                 : Ocorrencia := '18'; {Sustar protesto}
      toRemessaProtestarUrgente               : Ocorrencia := '36'; {Protestar urgente}
      toRemessaRegistrarDireta                : Ocorrencia := '48'; {Remessa cobrança direta (pré-impressa)}
      else
        Ocorrencia := '01'; {Remessa cobrança escritural}
    end;

    {Pegando Especie}
    if trim(EspecieDoc) = 'DM' then
      aEspecie := '01'
    else if trim(EspecieDoc) = 'DS' then
      aEspecie := '08'
    else if trim(EspecieDoc) = 'CC' then
      aEspecie := '31'
    else
      aEspecie := '01';


    if Aceite = atSim then
      aAceite := 'A'
    else
      aAceite := 'N';

    if Sacado.Pessoa = pFisica then
      aTipoSacado := '01'
    else if Sacado.Pessoa = pJuridica then
      aTipoSacado := '02'
    else
      aTipoSacado := '03';

    if DataDesconto = 0 then
      sDataDesconto := '000000'
    else
      sDataDesconto := FormatDateTime('ddmmyy', DataDesconto);

    wLinha := '1'                                                                            + // 001 a  001 - Identificação do Registro de Transação
              tipoInscricao                                                                  + // 002 a  003 - Tipo de Inscrição da Empresa
              PadLeft(OnlyNumber(ACBrBoleto.Cedente.CNPJCPF), 14, '0')                       + // 004 a  017 - Número da Inscrição da Empresa
              '00'                                                                           + // 018 a  019 - "Código Instrução/ Alegação a ser cancelada/ocorrencia 35"
              PadLeft(ACBrBoleto.Cedente.Convenio, 10, '0')                                  + // 020 a  029 - "Código do Convênio"
              Space(8)                                                                       + // 030 a  037 - "Brancos"
              PadLeft(NumeroDocumento,25)                                                    + // 038 a  062 - Uso exclusivo da Empresa
              PadLeft(NossoNumero,10,'0')                                                    + // 063 a 072 - Número do título no banco
              PadLeft(Carteira, 3, '0')                                                      + // 073 a 075 - Identificação do Número da Carteira
              Ocorrencia                                                                     + // 076 a 077 - Identificação do Tipo de Ocorrência
              PadLeft(NumeroDocumento, 10, '0')                                              + // 078 a 087 - Identificação do Título da Empresa
              FormatDateTime('ddmmyy', Vencimento)                                           + // 088 a 093 - Data de Vencimento do Título
              IntToStrZero(Round(ValorDocumento * 100), 13)                                  + // 094 a 106 - Valor Nominal do Título
              IntToStrZero(ACBrBoleto.Banco.Numero,3)                                        + // 107 a 109 - Código do Banco encarregado da cobraça
              PadLeft(RightStr( ACBrBoleto.Cedente.Agencia, 5), 5, '0')                      + // 110 a 114 - Agência Encarregada da Cobrança
              aEspecie                                                                       + // 115 a 116 - Espécie do Título
              aAceite                                                                        + // 117 a 117 - Identificação do Aceite do Título
              FormatDateTime('ddmmyy', DataDocumento)                                        + // 118 a 123 - Data de Emissão do Título
              PadLeft(trim(Instrucao1), 2, '0')                                              + // 124 a 125 - Primeira Instrução de Cobrança
              PadLeft(trim(Instrucao2), 2, '0')                                              + // 126 a 127 - Segunda Instrução de Cobrança
              Space(10)                                                                      + // 128 a 137 - "Brancos"
              IntToStrZero(round(ValorMoraJuros * 100), 13)                                  + // 138 a 150 - Juros de Mora Por Dia de Atraso
              sDataDesconto                                                                  + // 151 a 156 - Data Limite para Desconto
              IntToStrZero(round(ValorDesconto * 100), 13)                                   + // 157 a 169 - Valor Do Desconto Concedido
              IntToStrZero(round(ValorIOF * 100), 13)                                        + // 170 a 182 - Valor De Iof Operações Deseguro
              IntToStrZero(round(ValorAbatimento * 100), 13)                                 + // 183 a 195 -Valor Do Abatimento Concedido Ou Cancelado / Multa
              aTipoSacado                                                                    + // 196 a 197 - Código De Inscrição Do Sacado
              PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0')                                   + // 198 a 211 - Número de Inscrição do Sacado
              PadRight(Sacado.NomeSacado, 40, ' ')                                           + // 212 a 251 - Nome Do Sacado
              PadRight(trim(Sacado.Logradouro) + ' ' + trim(Sacado.Numero), 37, ' ')         + // 252 a 288 - Endereço Do Sacado
              Space(3)                                                                       + // 289 a 291 - Brancos
              PadRight(Sacado.Bairro, 12, ' ')                                               + // 292 a 303 - Bairro Do Sacado
              PadRight(Sacado.CEP, 8)                                                        + // 304 a 311 - CEP do Sacado
              PadRight(Sacado.Cidade, 15)                                                    + // 312 a 326 - Cidade do Sacado
              PadRight(Sacado.UF, 2)                                                         + // 327 a 328 - UF do Sacado
              PadRight(Sacado.SacadoAvalista.NomeAvalista, 40)                               + // 329 a 368 - Nome do Sacador Avalista / Mensagem específica vide nota 6.1.9 conforme manual do banco
              FormatDateTime('ddmmyy', Vencimento)                                           + // 369 a 374 - Data de Vencimento do Título
              IntToStrZero(DiasDeProtesto,2)                                                 + // 375 a 376 - Data a partir da qual a multa deve ser cobrada
              '0'                                                                            + // 377 a 377 - Identificação do Tipo de Moeda, 0=Real
              Space(17)                                                                      + // 378 a 394 - "Branco"
              IntToStrZero(ARemessa.Count + 1, 6);                                             // 395 a 400 - Número Sequencial De Registro De Arquivo

    aTotal := aTotal + ValorDocumento;
    Inc(aCount);
    aRemessa.Text := aRemessa.Text + wLinha;
  end;
end;

procedure TACBrBancoVotorantim.LerRetorno240(ARetorno: TStringList);
begin
  raise Exception.create(ACBrStr('CNAB 240 não implementado.'));
end;

procedure TACBrBancoVotorantim.LerRetorno400(ARetorno: TStringList);
var
  rCodEmpresa, rCedente, rAgencia, rDigitoAgencia: String;
  rConta, rDigitoConta, rCNPJCPF, Linha: String;
  ContLinha, CodOcorrencia, nColunaMotivoRejeicao, nQtdeMotivosRejeicao: Integer;
  Titulo: TACBrTitulo;
begin
  if StrToIntDef(copy(ARetorno.Strings[0], 77, 3), -1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                           'não é um arquivo de retorno do ' + Nome));

  rCodEmpresa    := trim(Copy(ARetorno[1], 04, 14));
  rCedente       := trim(Copy(ARetorno[0], 27, 10));

  ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 100, 2) + '/' +
                                                          Copy(ARetorno[0], 102, 2) + '/' +
                                                          Copy(ARetorno[0], 104, 2), 0,
                                                          'DD/MM/YY');

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 395, 6), 0);

  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCedente <>
        PadLeft(Cedente.CodigoCedente, 10, '0')) then
      raise Exception.Create(ACBrStr('Código do Cedente do arquivo inválido '+rCedente));

    case StrToIntDef(Copy(ARetorno[1], 2, 2), 0) of
      01: Cedente.TipoInscricao := pFisica;
      02: Cedente.TipoInscricao := pJuridica;
      else
        Cedente.TipoInscricao := pJuridica;
    end;

    rCNPJCPF := Copy(ARetorno[1], 4, 14);

    if LeCedenteRetorno then
    begin
      try
        Cedente.CNPJCPF := rCNPJCPF;
      except
      end;

      Cedente.CodigoCedente := rCodEmpresa;
      Cedente.Nome          := rCedente;
      Cedente.Agencia       := rAgencia;
      Cedente.AgenciaDigito := rDigitoAgencia;
      Cedente.Conta         := rConta;
      Cedente.ContaDigito   := rDigitoConta;
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha];

    if Copy(Linha, 1, 1) <> '1' then
      Continue;

    Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      SeuNumero      := Copy(Linha,38,25);
      NossoNumero    := Copy(Linha, 63, 10);
      CodOcorrencia  := StrToIntDef(copy(Linha, 101, 2),0);
      OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(CodOcorrencia);

      Carteira := copy(Linha, 099, 2);

      DataOcorrencia := StringToDateTimeDef(copy(Linha, 111, 2) + '/' +
                                            copy(Linha, 113, 2) + '/' +
                                            copy(Linha, 115, 2), 0, 'DD/MM/YY');

      if (CodOcorrencia = 03) or (CodOcorrencia = 16) then // entrada rejeitada
      begin

        nColunaMotivoRejeicao := 367; // posição da primeira rejeicao de 4 (367-374)

        for nQtdeMotivosRejeicao := 1 to 4 do
        begin
          if Copy(Linha, nColunaMotivoRejeicao, 2)<>'00' then
          begin
            Titulo.MotivoRejeicaoComando.Add(Copy(Linha, nColunaMotivoRejeicao, 2));
            Titulo.DescricaoMotivoRejeicaoComando.add(CodMotivoRejeicaoToDescricao(Titulo.OcorrenciaOriginal.Tipo,copy(Linha, nColunaMotivoRejeicao, 2)));
          end;
          nColunaMotivoRejeicao := nColunaMotivoRejeicao + 2; // incrementa 2 posicoes para próxima rejeicao
        end;

      end;
      NumeroDocumento := copy(Linha, 117, 10);

      if Copy(Linha, 147, 2) <> '00' then
        Vencimento := StringToDateTimeDef(copy(Linha, 147, 2) + '/' +
                                          copy(Linha, 149, 2) + '/' +
                                          copy(Linha, 151, 2), 0, 'DD/MM/YY');

      ValorDocumento       := StrToFloatDef(copy(Linha, 153, 13), 0) / 100;
      case StrToIntDef(copy(Linha, 174, 2),00) of
        01: EspecieDoc := 'DM';
        08: EspecieDoc := 'DS';
        31: EspecieDoc := 'CC';
      else
        EspecieDoc := copy(Linha, 174, 2);
      end;
      ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 176, 13), 0) / 100;
      ValorOutrasDespesas  := StrToFloatDef(Copy(Linha, 189, 13), 0) / 100;
      ValorIOF             := StrToFloatDef(Copy(Linha, 215, 13), 0) / 100;
      ValorAbatimento      := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
      ValorDesconto        := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
      ValorPago            := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;
      ValorMoraJuros       := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
      ValorOutrosCreditos  := StrToFloatDef(Copy(Linha, 280, 13), 0) / 100;

      if Copy(Linha, 296, 2) <> '00' then
        DataCredito := StringToDateTimeDef(copy(Linha, 296, 2) + '/' +
                                           copy(Linha, 298, 2) + '/' +
                                           copy(Linha, 300, 2), 0, 'DD/MM/YY');
    end;
  end;
end;

function TACBrBancoVotorantim.MontarCampoCodigoCedente(
  const ACBrTitulo: TACBrTitulo): string;
begin
  with ACBrTitulo.ACBrBoleto.Cedente do
  begin
    Result := PadLeft(RightStr(Agencia,5), 5, '0') + ' / ' + PadLeft(ACBrBoleto.Cedente.Conta, 8, '0') + PadLeft(ACBrBoleto.Cedente.ContaDigito, 1, '0');
  end;
end;

function TACBrBancoVotorantim.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string;
begin
  with ACBrTitulo do
  begin
    Result := PadLeft(RightStr(NossoNumero,9),9,'0');
  end;
end;

function TACBrBancoVotorantim.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras ,
  valorDocumento, agencia, convenio,
   NossoNumero: string;
begin
  with ACBrTitulo.ACBrBoleto do
  begin
    FatorVencimento  := CalcularFatorVencimento(ACBrTitulo.Vencimento);
    valorDocumento   := IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10);
    convenio            := PadLeft(Cedente.Convenio, 10, '0');
    NossoNumero      := PadLeft(RightStr(ACBrTitulo.NossoNumero,9),10,'0');

    CodigoBarras := IntToStr(Banco.Numero) + '9' + FatorVencimento +
                    valorDocumento +
                    trim(convenio) + '500' +
                    NossoNumero +  '00';


    DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
  end;
    Result := IntToStr(Numero) + '9' + DigitoCodBarras + FatorVencimento + valorDocumento + PadLeft(convenio,10,'0') + '500' + NossoNumero + '00';

end;

function TACBrBancoVotorantim.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): string;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                    : Result := '02';
    toRetornoRegistroRecusado                      : Result := '03';
    toRetornoLiquidado                             : Result := '06';
    toRetornoLiquidadoParcialmente                 : Result := '07';
    toRetornoLiquidadoEmCartorio                   : Result := '08';
    toRetornoBaixaAutomatica                       : Result := '09';
    toRetornoAbatimentoConcedido                   : Result := '12';
    toRetornoVencimentoAlterado                    : Result := '14';
    toRetornoInstrucaoRejeitada                    : Result := '16';
    toRetornoConfInstrucaoAlteracaoDiasBaixaAutomatica : Result := '18';
    toRetornoConfInstrucaoProtesto                 : Result := '19';
    toRetornoConfInstrucaoSustacaoProtesto         : Result := '20';
    toRetornoConfInstrucaoNaoProtestar             : Result := '21';
    toRetornoConfInstrucaoNaoBaixarAutomaticamente : Result := '22';
    toRetornoEncaminhadoACartorio                  : Result := '23';
    toRetornoConfAlteracaoDiasBaixaAutomatica      : Result := '24';
    toRetornoConfirmacaoCancelamentoBaixaAutomatica: Result := '25';
    toRetornoConfirmacaoAlteracaoValorNominal      : Result := '26';
    toRetornoConfirmacaoAlteracaoValorMinimoOuPercentual : Result := '27';
    toRetornoConfirmacaoAlteracaoValorMaximoOuPercentual : Result := '28';
    toRetornoConfirmacaoAlteracaoValorpercentualMinimoMaximo : Result := '29';
    toRetornoBaixaPorProtesto                      : Result := '32';
    toRetornoConfirmacaoProtesto                   : Result := '33';
    toRetornoConfirmacaoSustacao                   : Result := '34';
    toRetornoProtestoSustadoJudicialmente          : Result := '35';
    toRetornoTransferenciaCarteira                 : Result := '47';
    toRetornoAlteracaoPercentualMinimoMaximo       : Result := '48';
    toRetornoAlteracaoPercentualMinimo             : Result := '49';
    toRetornoAlteracaoPercentualMaximo             : Result := '50';
    toRetornoAlteracaoQuantidadeParcela            : Result := '51';
    else
      Result := '02';
  end;
end;

function TACBrBancoVotorantim.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): string;
var
  CodOcorrencia: integer;
begin
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0);

  case CodOcorrencia of
    02: Result := '02-ENTRADA CONFIRMADA';
    03: Result := '03-ENTRADA REJEITADA';
    06: Result := '06-LIQUIDAÇÃO NORMAL';
    07: Result := '07-LIQUIDAÇÃO PARCIAL';
    08: Result := '08-LIQUIDAÇÃO EM CARTÓRIO';
    09: Result := '09-BAIXA SIMPLES';
    12: Result := '12-ABATIMENTO CONCEDIDO';
    14: Result := '14-VENCIMENTO ALTERADO';
    16: Result := '16-INSTRUÇÕES REJEITADAS';
    18: Result := '18-CONFIRMAÇÃO DA INSTRUÇÃO DE ALTERAÇÃO DE DIAS PARA BAIXA AUTOMATICA';
    19: Result := '19-CONFIRMAÇÃO DA INSTRUÇÃO DE PROTESTO';
    20: Result := '20-CONFIRMAÇÃO DA INSTRUÇÃO DE SUSTAÇÃO DE PROTESTO';
    21: Result := '21-CONFIRMAÇÃO DA INSTRUÇÃO DE NÃO PROTESTAR';
    22: Result := '22-CONFIRMAÇÃO DA INSTRUÇÃO DE NÃO BAIXAR AUTOMATICAMENTE';
    23: Result := '23-PROTESTO ENVIADO A CARTÓRIO';
    24: Result := '24-CONFIRMAÇÃO DE ALTERAÇÃO DE DIAS PARA BAIXA AUTOMATICA';
    25: Result := '25-CONFIRMAÇÃO DE CANCELAMENTO DE BAIXA AUTOMATICA';
    26: Result := '26-CONFIRMAÇÃO DE ALTERAÇÃO DO VALOR NOMINAL';
    27: Result := '27-CONFIRMAÇÃO DE ALTERAÇÃO DE VALOR/PERCENTUAL MINIMO';
    28: Result := '28-CONFIRMAÇÃO DE ALTERAÇÃO DE VALOR/PERCENTUAL MAXIMO';
    29: Result := '29-CONFIRMAÇÃO DE ALTERAÇÃO DE VALOR/PERCENTUAL MÍNIMO E MAXIMO';
    32: Result := '32-BAIXA POR TER SIDO PROTESTADO';
    33: Result := '33-CONFIRMAÇÃO DE PROTESTO';
    34: Result := '34-CONFIRMAÇÃO DE SUSTAÇÃO';
    35: Result := '35-PROTESTO SUSTADO JUDICIALMENTE';
    47: Result := '47-TRANSFERÊNCIA DE CARTEIRA';
    48: Result := '48-ALTERAÇÃO DE PERCENTUAL MÍNIMO/ MÁXIMO';
    49: Result := '49-ALTERAÇÃO DE PERCENTUAL MÍNIMO';
    50: Result := '50-ALTERAÇÃO DE PERCENTUAL MÁXIMO';
    51: Result := '51-ALTERAÇÃO DA QUANTIDADE DE PARCELAS';
  end;

  Result := ACBrSTr(Result);
end;

end.
